# zerocrossing.R
#' Zero-Elevation Point Identification for Beach Profiles
#'
#' Accuracy-aware zero-elevation point identification for beach profile transects.
#' This module provides unified functionality for identifying all points where
#' elevation crosses or meets sea level (elevation = 0), incorporating tier-specific
#' measurement accuracy and calculating position uncertainty for each zero-crossing.
#'
#' @section Methods:
#' \describe{
#'   \item{Measured zeros}{Existing points within tier-specific tolerance of zero}
#'   \item{Interpolated zeros}{Linear interpolation between sign-change points}
#'   \item{Extrapolated intercepts}{2-point linear regression beyond measured data}
#' }
#'
#' @importFrom dplyr mutate filter select arrange group_by group_split pull
#' @importFrom dplyr left_join bind_rows summarize first last lead n
#' @importFrom stats median
#' @name zerocrossing
#' @keywords internal
"_PACKAGE"

# ============================================================================
# CONSTANTS (ERROR-AWARE)
# ============================================================================

#' Minimum slope for valid extrapolation (numerical stability)
#' @keywords internal
#' @noRd
SLOPE_TOLERANCE <- 1e-6

#' Maximum reasonable extrapolation distance (meters)
#' @keywords internal
#' @noRd
MAX_EXTRAP_DISTANCE <- 15

#' Slope uncertainty rate (10% per 10m extrapolated)
#' @keywords internal
#' @noRd
SLOPE_UNCERTAINTY_RATE <- 0.1

#' Model error rate (5 cm per meter extrapolated)
#' @keywords internal
#' @noRd
MODEL_ERROR_RATE <- 0.05

# ============================================================================
# UNCERTAINTY CALCULATION FUNCTIONS
# ============================================================================

#' Calculate Interpolated Zero-Crossing Position Uncertainty
#'
#' Propagates vertical and horizontal measurement errors through linear
#' interpolation to estimate position uncertainty of zero-crossing.
#'
#' Error sources:
#' \enumerate{
#'   \item Vertical measurement error (elevation uncertainty)
#'   \item Horizontal measurement error (distance uncertainty)
#' }
#'
#' @param sigma_v1 Vertical standard error of first point (meters)
#' @param sigma_v2 Vertical standard error of second point (meters)
#' @param sigma_h1 Horizontal standard error of first point (meters)
#' @param sigma_h2 Horizontal standard error of second point (meters)
#' @param slope Absolute value of slope between points (m/m)
#' @return List with \code{uncertainty} (m) and \code{slope} (m/m)
#' @keywords internal
calculate_interpolation_uncertainty <- function(sigma_v1, sigma_v2,
                                                sigma_h1, sigma_h2,
                                                slope) {

  # Position error from vertical uncertainty
  # As slope flattens, vertical error has larger horizontal impact
  sigma_x_vertical <- sqrt(sigma_v1^2 + sigma_v2^2) / slope

  # Position error from horizontal uncertainty
  sigma_x_horizontal <- sqrt(sigma_h1^2 + sigma_h2^2)

  # Total position uncertainty (combined in quadrature)
  total_uncertainty <- sqrt(sigma_x_vertical^2 + sigma_x_horizontal^2)

  return(list(
    uncertainty = total_uncertainty,
    slope = slope
  ))
}


#' Calculate Extrapolated Zero-Crossing Position Uncertainty
#'
#' Estimates position uncertainty for extrapolated zero-crossings, accounting for:
#' \enumerate{
#'   \item Base measurement error (same as interpolation)
#'   \item Slope uncertainty (increases with extrapolation distance)
#'   \item Model error (linear assumption may not hold)
#' }
#'
#' @param sigma_v1 Vertical standard error of first point (meters)
#' @param sigma_v2 Vertical standard error of second point (meters)
#' @param sigma_h1 Horizontal standard error of first point (meters)
#' @param sigma_h2 Horizontal standard error of second point (meters)
#' @param slope Absolute value of slope between points (m/m)
#' @param extrap_distance Distance extrapolated beyond last point (meters)
#' @return List with \code{uncertainty} (m), \code{slope} (m/m), and \code{extrap_distance} (m)
#' @keywords internal
calculate_extrapolation_uncertainty <- function(sigma_v1, sigma_v2,
                                                sigma_h1, sigma_h2,
                                                slope, extrap_distance) {

  # Base uncertainty (same as interpolation)
  base_calc <- calculate_interpolation_uncertainty(sigma_v1, sigma_v2,
                                                   sigma_h1, sigma_h2,
                                                   slope)
  sigma_base <- base_calc$uncertainty

  # Slope uncertainty penalty (10% per 10m extrapolated)
  slope_factor <- 1 + (SLOPE_UNCERTAINTY_RATE * extrap_distance / 10)
  sigma_with_slope <- sigma_base * slope_factor

  # Model error (linear assumption breakdown)
  model_error <- MODEL_ERROR_RATE * extrap_distance

  # Total uncertainty
  total_uncertainty <- sqrt(sigma_with_slope^2 + model_error^2)

  return(list(
    uncertainty = total_uncertainty,
    slope = slope,
    extrap_distance = extrap_distance
  ))
}


#' Assign Confidence Level Based on Slope and Method
#'
#' Vectorized function that assigns confidence ratings based on the detection
#' method and geometric characteristics of the zero-crossing.
#'
#' Confidence levels:
#' \describe{
#'   \item{high}{Measured points, or interpolated with slope > 0.05}
#'   \item{moderate}{Interpolated with slope 0.02-0.05, or short extrapolations <= 5m}
#'   \item{low}{Interpolated with slope < 0.02, or extrapolations > 5m}
#' }
#'
#' @param slope Slope magnitude (m/m) - can be vector
#' @param method "measured", "interpolated", or "extrapolated" - single value or vector
#' @param extrap_distance Extrapolation distance (meters), NA if not extrapolated - can be vector
#' @return Character vector: "high", "moderate", or "low"
#' @keywords internal
assign_confidence <- function(slope, method, extrap_distance = NA) {

  # Handle single values or vectors
  n <- max(length(slope), length(method), length(extrap_distance))

  # Recycle single values to match vector length
  if (length(slope) == 1) slope <- rep(slope, n)
  if (length(method) == 1) method <- rep(method, n)
  if (length(extrap_distance) == 1) extrap_distance <- rep(extrap_distance, n)

  # Initialize result vector
  confidence <- rep("unknown", n)

  # Measured points: always high
  confidence[method == "measured"] <- "high"

  # Extrapolated points: based on distance
  is_extrap <- method == "extrapolated"
  if (any(is_extrap)) {
    # All extrapolations currently get "low" confidence
    # (distance > 5m gets low, per original logic)
    confidence[is_extrap] <- ifelse(
      is.na(extrap_distance[is_extrap]) | extrap_distance[is_extrap] > 5,
      "low",
      "moderate"
    )
  }

  # Interpolated points: based on slope
  is_interp <- method == "interpolated"
  if (any(is_interp)) {
    confidence[is_interp] <- ifelse(
      slope[is_interp] > 0.05,
      "high",
      ifelse(
        slope[is_interp] > 0.02,
        "moderate",
        "low"
      )
    )
  }

  return(confidence)
}


# ============================================================================
# EXTRAPOLATION FUNCTIONS
# ============================================================================

#' 2-Point Linear Extrapolation to Sea Level with Uncertainty
#'
#' Derives sea-level intercept for beach profile transect using 2-point
#' linear regression. Extrapolates from the slope between the two outermost
#' measurement points to find where the profile would cross elevation = 0.
#'
#' Returns list with NA values if:
#' \itemize{
#'   \item Extrapolation is not needed (endpoint already at/near zero)
#'   \item Slope is too flat (< SLOPE_TOLERANCE)
#'   \item Extrapolation would go in wrong direction
#'   \item Extrapolation distance exceeds MAX_EXTRAP_DISTANCE
#' }
#'
#' @param data Data frame with columns: \code{distance}, \code{elevation},
#'   \code{sigma_h}, \code{sigma_v}
#' @param type Character. "max" for seaward extrapolation, "min" for landward
#' @return List with: \code{distance}, \code{uncertainty}, \code{slope},
#'   \code{extrap_distance}, or NA values if extrapolation invalid
#' @keywords internal
twoptexpl_with_uncertainty <- function(data, type = "max") {

  distance <- data$distance
  elevation <- data$elevation

  if (type == "max") {
    # Get indices of two highest distance values (furthest from origin)
    top_indices <- order(distance, decreasing = TRUE)[1:2]
    endpoint_elev <- elevation[top_indices[1]]
  } else if (type == "min") {
    # Get indices of two lowest distance values (closest to origin)
    top_indices <- order(distance, decreasing = FALSE)[1:2]
    endpoint_elev <- elevation[top_indices[1]]
  } else {
    stop("type must be 'max' or 'min'")
  }

  # Check if extrapolation is needed
  if (endpoint_elev < 0) {
    return(list(distance = NA, uncertainty = NA, slope = NA,
                extrap_distance = NA))
  }

  # Extract coordinates and accuracy for the two outermost points
  x_vals <- distance[top_indices]
  y_vals <- elevation[top_indices]
  sigma_h_vals <- data$sigma_h[top_indices]
  sigma_v_vals <- data$sigma_v[top_indices]

  # Calculate slope: m = (y2-y1)/(x2-x1)
  slope <- diff(y_vals) / diff(x_vals)
  slope_abs <- abs(slope)

  # Check if slope is too flat
  if (slope_abs < SLOPE_TOLERANCE) {
    return(list(distance = NA, uncertainty = NA, slope = NA,
                extrap_distance = NA))
  }

  # Calculate y-intercept: b = y2 - m*x2
  y_intercept <- y_vals[2] - slope * x_vals[2]

  # Calculate x-intercept where y=0: x = -b/m
  x_intercept <- -y_intercept / slope

  # Calculate extrapolation distance
  if (type == "max") {
    extrap_distance <- x_intercept - max(x_vals)
  } else {
    extrap_distance <- min(x_vals) - x_intercept
  }

  # Validation checks
  valid <- TRUE

  # Check extrapolation distance
  if (extrap_distance < 0 || extrap_distance > MAX_EXTRAP_DISTANCE) {
    valid <- FALSE
  }

  # Check direction
  if (type == "min" && (x_intercept < 0 || x_intercept >= min(x_vals))) {
    valid <- FALSE
  } else if (type == "max" && x_intercept <= max(x_vals)) {
    valid <- FALSE
  }

  if (!valid) {
    return(list(distance = NA, uncertainty = NA, slope = NA,
                extrap_distance = NA))
  }

  # Calculate uncertainty
  uncertainty_calc <- calculate_extrapolation_uncertainty(
    sigma_v1 = sigma_v_vals[1],
    sigma_v2 = sigma_v_vals[2],
    sigma_h1 = sigma_h_vals[1],
    sigma_h2 = sigma_h_vals[2],
    slope = slope_abs,
    extrap_distance = extrap_distance
  )

  return(list(
    distance = x_intercept,
    uncertainty = uncertainty_calc$uncertainty,
    slope = slope_abs,
    extrap_distance = extrap_distance
  ))
}


# ============================================================================
# INTERPOLATION FUNCTIONS
# ============================================================================

#' Find Zero Crossings with Uncertainty in Profile Data
#'
#' Identifies all locations where the profile crosses elevation = 0 by
#' detecting sign changes between consecutive non-zero points. Uses linear
#' interpolation to find exact x-coordinates where y=0, and calculates
#' position uncertainty based on measurement errors and slope.
#'
#' @param data Data frame with columns: \code{distance}, \code{elevation},
#'   \code{sigma_h}, \code{sigma_v}
#' @return Data frame with columns: \code{distance}, \code{uncertainty}, \code{slope}
#' @keywords internal
find_zero_crossings_with_uncertainty <- function(data) {

  x <- data$distance
  y <- data$elevation

  if (length(x) < 2) {
    return(data.frame(
      distance = numeric(0),
      uncertainty = numeric(0),
      slope = numeric(0)
    ))
  }

  # Prepare results list
  crossings_list <- list()

  # Check each consecutive pair of points
  for (i in 1:(length(y) - 1)) {

    # Sign change between consecutive non-zero points
    if ((y[i] > 0 && y[i+1] < 0) || (y[i] < 0 && y[i+1] > 0)) {

      # Calculate slope
      slope <- abs((y[i+1] - y[i]) / (x[i+1] - x[i]))

      # Linear interpolation to find exact x where y=0
      x_crossing <- x[i] - y[i] / ((y[i+1] - y[i]) / (x[i+1] - x[i]))

      # Calculate uncertainty
      uncertainty_calc <- calculate_interpolation_uncertainty(
        sigma_v1 = data$sigma_v[i],
        sigma_v2 = data$sigma_v[i+1],
        sigma_h1 = data$sigma_h[i],
        sigma_h2 = data$sigma_h[i+1],
        slope = slope
      )

      crossings_list[[length(crossings_list) + 1]] <- data.frame(
        distance = x_crossing,
        uncertainty = uncertainty_calc$uncertainty,
        slope = slope
      )
    }
  }

  # Combine results
  if (length(crossings_list) == 0) {
    return(data.frame(
      distance = numeric(0),
      uncertainty = numeric(0),
      slope = numeric(0)
    ))
  }

  crossings <- bind_rows(crossings_list)
  return(crossings)
}


# ============================================================================
# UNIFIED ZERO-ELEVATION IDENTIFICATION
# ============================================================================

#' Identify All Zero-Elevation Points with Accuracy Awareness
#'
#' Unified function to find ALL points where elevation crosses or meets zero,
#' using tier-specific accuracy values to determine tolerances and calculate
#' position uncertainties.
#'
#' @section Methods:
#' Three detection methods are used:
#' \enumerate{
#'   \item Measured zeros - existing points within 1.96*sigma_v of zero
#'   \item Interpolated zeros - sign changes with uncertainty propagation
#'   \item Extrapolated intercepts - 2-point regression with distance penalties
#' }
#'
#' @param data Data frame with columns: \code{distance}, \code{elevation},
#'   \code{sigma_h}, \code{sigma_v}, \code{accuracy_tier}, \code{cross_island}
#' @return Data frame with columns:
#'   \describe{
#'     \item{distance}{x-coordinate where elevation = 0}
#'     \item{elevation}{always 0}
#'     \item{point_type}{"measured_zero", "interpolated", or "extrapolated"}
#'     \item{uncertainty}{position uncertainty (meters, 1-sigma)}
#'     \item{uncertainty_95ci}{95\% confidence interval (±1.96*uncertainty)}
#'     \item{confidence}{"high", "moderate", or "low"}
#'     \item{slope}{slope at crossing (NA for measured points)}
#'     \item{extrap_distance}{extrapolation distance (NA except for extrapolated)}
#'     \item{sigma_v}{vertical accuracy at point}
#'     \item{sigma_h}{horizontal accuracy at point}
#'     \item{accuracy_tier}{quality tier}
#'   }
#' @export
identify_all_zero_elevation_points <- function(data) {

  distance <- data$distance
  elevation <- data$elevation
  cross_island <- first(data$cross_island)

  # Initialize empty result dataframe
  zero_points <- data.frame(
    distance = numeric(0),
    elevation = numeric(0),
    point_type = character(0),
    uncertainty = numeric(0),
    uncertainty_95ci = numeric(0),
    confidence = character(0),
    slope = numeric(0),
    extrap_distance = numeric(0),
    sigma_v = numeric(0),
    sigma_h = numeric(0),
    accuracy_tier = character(0),
    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------------------------
  # 1. MEASURED ZEROS - points within tier-specific tolerance
  # -------------------------------------------------------------------------

  # Calculate tier-specific tolerance (1.96 * sigma_v for 95% CI)
  data <- data %>%
    mutate(zero_tolerance = 1.96 * sigma_v)

  measured_zero_idx <- which(abs(elevation) < data$zero_tolerance)

  if (length(measured_zero_idx) > 0) {
    measured_data <- data[measured_zero_idx, ]
    zero_points <- bind_rows(zero_points, data.frame(
      distance = measured_data$distance,
      elevation = measured_data$elevation,
      point_type = "measured_zero",
      uncertainty = measured_data$sigma_h,  # Only horizontal uncertainty
      uncertainty_95ci = 1.96 * measured_data$sigma_h,
      confidence = "high",
      slope = NA,
      extrap_distance = NA,
      sigma_v = measured_data$sigma_v,
      sigma_h = measured_data$sigma_h,
      accuracy_tier = measured_data$accuracy_tier,
      stringsAsFactors = FALSE
    ))
  }

  # -------------------------------------------------------------------------
  # 2. INTERPOLATED ZEROS - sign changes with uncertainty
  # -------------------------------------------------------------------------

  interpolated <- find_zero_crossings_with_uncertainty(data)

  if (nrow(interpolated) > 0) {
    # Get accuracy values from nearest measured point for metadata
    # (actual uncertainty already calculated in interpolation function)
    for (i in 1:nrow(interpolated)) {
      nearest_idx <- which.min(abs(data$distance - interpolated$distance[i]))
      interpolated$sigma_v[i] <- data$sigma_v[nearest_idx]
      interpolated$sigma_h[i] <- data$sigma_h[nearest_idx]
      interpolated$accuracy_tier[i] <- data$accuracy_tier[nearest_idx]
    }

    zero_points <- bind_rows(zero_points, data.frame(
      distance = interpolated$distance,
      elevation = 0,
      point_type = "interpolated",
      uncertainty = interpolated$uncertainty,
      uncertainty_95ci = 1.96 * interpolated$uncertainty,
      confidence = assign_confidence(interpolated$slope, "interpolated"),
      slope = interpolated$slope,
      extrap_distance = NA,
      sigma_v = interpolated$sigma_v,
      sigma_h = interpolated$sigma_h,
      accuracy_tier = interpolated$accuracy_tier,
      stringsAsFactors = FALSE
    ))
  }

  # -------------------------------------------------------------------------
  # 3. EXTRAPOLATED SEAWARD (max distance) intercept
  # -------------------------------------------------------------------------

  max_extrap <- twoptexpl_with_uncertainty(data, type = "max")

  if (!is.na(max_extrap$distance)) {
    # Get accuracy from furthest point
    max_idx <- which.max(data$distance)

    zero_points <- bind_rows(zero_points, data.frame(
      distance = max_extrap$distance,
      elevation = 0,
      point_type = "extrapolated",
      uncertainty = max_extrap$uncertainty,
      uncertainty_95ci = 1.96 * max_extrap$uncertainty,
      confidence = assign_confidence(max_extrap$slope, "extrapolated",
                                     max_extrap$extrap_distance),
      slope = max_extrap$slope,
      extrap_distance = max_extrap$extrap_distance,
      sigma_v = data$sigma_v[max_idx],
      sigma_h = data$sigma_h[max_idx],
      accuracy_tier = data$accuracy_tier[max_idx],
      stringsAsFactors = FALSE
    ))
  }

  # -------------------------------------------------------------------------
  # 4. EXTRAPOLATED LANDWARD (min distance) intercept (cross-island only)
  # -------------------------------------------------------------------------

  if (cross_island) {
    min_extrap <- twoptexpl_with_uncertainty(data, type = "min")

    if (!is.na(min_extrap$distance)) {
      # Get accuracy from closest point
      min_idx <- which.min(data$distance)

      zero_points <- bind_rows(zero_points, data.frame(
        distance = min_extrap$distance,
        elevation = 0,
        point_type = "extrapolated",
        uncertainty = min_extrap$uncertainty,
        uncertainty_95ci = 1.96 * min_extrap$uncertainty,
        confidence = assign_confidence(min_extrap$slope, "extrapolated",
                                       min_extrap$extrap_distance),
        slope = min_extrap$slope,
        extrap_distance = min_extrap$extrap_distance,
        sigma_v = data$sigma_v[min_idx],
        sigma_h = data$sigma_h[min_idx],
        accuracy_tier = data$accuracy_tier[min_idx],
        stringsAsFactors = FALSE
      ))
    }
  }

  # -------------------------------------------------------------------------
  # Clean up and sort results
  # -------------------------------------------------------------------------

  if (nrow(zero_points) > 0) {
    zero_points <- zero_points %>%
      arrange(distance) %>%
      # Remove duplicates using error-aware tolerance (3 * sigma_h)
      # For consecutive points, use average of their uncertainties
      mutate(
        sigma_h_next = lead(sigma_h, default = last(sigma_h)),
        tolerance = 3 * (sigma_h + sigma_h_next) / 2,
        is_duplicate = c(FALSE, diff(distance) <= tolerance[-n()])
      ) %>%
      filter(!is_duplicate) %>%
      select(-sigma_h_next, -tolerance, -is_duplicate)
  }

  return(zero_points)
}


# ============================================================================
# BOUNDARY EXTRACTION HELPERS
# ============================================================================

#' Extract Internal Zero Crossings
#'
#' Helper function to extract zero crossings that occur between the integration
#' boundaries (excluding the boundary intercepts themselves).
#'
#' Uses error-aware tolerance: a point is considered "internal" if it's at least
#' 3*sigma_h away from both boundaries.
#'
#' @param zero_points Data frame from \code{\link{identify_all_zero_elevation_points}}.
#'   Must include \code{sigma_h} column
#' @param left_boundary Left integration boundary (numeric)
#' @param right_boundary Right integration boundary (numeric)
#' @return Numeric vector of distances where internal zero crossings occur
#' @export
extract_internal_crossings <- function(zero_points, left_boundary, right_boundary) {

  # Use error-aware tolerance: point must be 3*sigma_h away from boundaries
  internal <- zero_points %>%
    filter(
      point_type == "interpolated",
      distance > left_boundary + 3 * sigma_h,
      distance < right_boundary - 3 * sigma_h
    ) %>%
    pull(distance)

  return(internal)
}


# ============================================================================
# WORKFLOW INTEGRATION FUNCTION
# ============================================================================

#' Identify Zero Points for All Transect-Years with Accuracy
#'
#' Group-aware wrapper for \code{\link{identify_all_zero_elevation_points}} that works
#' with grouped/nested dataframes in the workflow pipeline. Requires data
#' with accuracy columns already assigned.
#'
#' @param data Data frame with columns: \code{transect}, \code{year}, \code{park},
#'   \code{cross_island}, \code{distance}, \code{elevation}, \code{sigma_h},
#'   \code{sigma_v}, \code{accuracy_tier}
#' @return Data frame with zero-elevation points and uncertainties for all transect-years
#' @export
identify_zero_points_all_transects <- function(data) {

  # Extract metadata before processing
  metadata <- data %>%
    select(transect, year, park, cross_island) %>%
    distinct()

  # Calculate zero points with uncertainty
  # Use group_split() + lapply() instead of group_modify() to avoid memory accumulation
  zero_points <- data %>%
    group_by(transect, year) %>%
    group_split() %>%
    lapply(function(df) {
      # Get group identifiers
      trans <- first(df$transect)
      yr <- first(df$year)

      # Process this group
      result <- identify_all_zero_elevation_points(df)

      # Add transect and year back to results
      if (nrow(result) > 0) {
        result$transect <- trans
        result$year <- yr
      }

      return(result)
    }) %>%
    bind_rows()

  # Add park and cross_island metadata back
  zero_points %>%
    left_join(metadata, by = c("transect", "year"))
}


# ============================================================================
# SUMMARY & QUALITY CONTROL FUNCTIONS
# ============================================================================

#' Summarize Zero-Crossing Detection Quality
#'
#' Generate summary statistics about zero-crossing detection success and
#' uncertainty across all transects.
#'
#' @param zero_points Data frame from \code{\link{identify_zero_points_all_transects}}
#' @return Summary data frame with quality metrics by park/year/method. Columns include:
#'   \describe{
#'     \item{park}{Park identifier}
#'     \item{year}{Survey year}
#'     \item{point_type}{Detection method}
#'     \item{confidence}{Confidence level}
#'     \item{n_crossings}{Number of crossings detected}
#'     \item{mean_uncertainty}{Mean position uncertainty}
#'     \item{median_uncertainty}{Median position uncertainty}
#'     \item{max_uncertainty}{Maximum position uncertainty}
#'     \item{mean_uncertainty_95ci}{Mean 95\% confidence interval width}
#'   }
#' @export
summarize_zero_crossing_quality <- function(zero_points) {

  summary <- zero_points %>%
    group_by(park, year, point_type, confidence) %>%
    summarize(
      n_crossings = n(),
      mean_uncertainty = mean(uncertainty, na.rm = TRUE),
      median_uncertainty = median(uncertainty, na.rm = TRUE),
      max_uncertainty = max(uncertainty, na.rm = TRUE),
      mean_uncertainty_95ci = mean(uncertainty_95ci, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(park, year, point_type)

  return(summary)
}


#' Flag High-Uncertainty Zero Crossings
#'
#' Identify zero crossings that exceed a specified uncertainty threshold.
#' Useful for quality control and deciding which crossings to use in analysis.
#'
#' @param zero_points Data frame from \code{\link{identify_zero_points_all_transects}}
#' @param uncertainty_threshold Maximum acceptable uncertainty in meters (default 2.0m)
#' @return Data frame with only high-uncertainty crossings, sorted by descending uncertainty
#' @export
flag_high_uncertainty_crossings <- function(zero_points,
                                            uncertainty_threshold = 2.0) {

  high_uncertainty <- zero_points %>%
    filter(uncertainty > uncertainty_threshold) %>%
    select(transect, year, park, distance, point_type,
           uncertainty, uncertainty_95ci, confidence,
           slope, extrap_distance, accuracy_tier) %>%
    arrange(desc(uncertainty))

  return(high_uncertainty)
}
