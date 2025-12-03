#' common_min.R
#' Common Minimum Distance Calculations for Beach Transects
#'
#' Functions for calculating common minimum distance points across transects.
#' This hybrid implementation combines row-appending approach with accuracy-aware
#' calculations for uncertainty, confidence, and slope.
#'
#' For land-to-water (non-cross-island) transects, the common minimum distance
#' ensures all transects share a consistent landward integration boundary across
#' years, enabling valid temporal comparisons.
#'
#' @section Distance Tolerance:
#' Distance tolerance is point-specific (3 * sigma_h per point):
#' \itemize{
#'   \item Worst case: 3 * 0.027m = 0.081m (low accuracy years)
#'   \item Best case: 3 * 0.009m = 0.027m (high accuracy years)
#' }
#'
#' Points within 3σ_h are statistically indistinguishable from the target
#' distance (99.7\% confidence interval). This error-aware approach adapts
#' to varying measurement accuracy across years.
#'
#' @importFrom dplyr filter group_by summarise mutate inner_join left_join
#' @importFrom dplyr bind_rows group_split select first if_else tibble n
#' @importFrom stats approx
#' @name common_min
#' @keywords internal
"_PACKAGE"

# ============================================================================
# COMMON MINIMUM CALCULATION WITH ACCURACY
# ============================================================================

#' Calculate Common Minimum and Generate Common Min Points with Accuracy
#'
#' For land-to-water (non-cross-island) transects, this function:
#' \enumerate{
#'   \item Calculates the common minimum distance (max of yearly minimums)
#'   \item For each transect-year, returns exactly one point at the common_min distance
#'   \item Includes uncertainty and confidence calculations based on accuracy values
#' }
#'
#' The returned points should be used to add/replace points in the dataset. Each
#' transect-year will have one common_min point with either:
#' \itemize{
#'   \item The existing elevation if a point already exists at that distance
#'   \item An interpolated elevation if no point exists at that distance
#' }
#'
#' @section Point Skipping:
#' Points are skipped if:
#' \itemize{
#'   \item common_min distance = 0 (to avoid conflicts with zero-crossing points)
#'   \item common_min is outside the measured range for that year
#'   \item Interpolation fails
#'   \item Slope is too flat (< 1e-6) causing numerical instability
#'   \item Accuracy values are missing or invalid
#' }
#'
#' @section Uncertainty Calculation:
#' For interpolated points, uncertainty is calculated using error propagation
#' through linear interpolation, combining vertical and horizontal measurement
#' errors. Confidence levels are assigned based on slope magnitude using the
#' same thresholds as interpolated zero crossings.
#'
#' @param data Cleaned transect data with columns: \code{transect}, \code{year},
#'   \code{distance}, \code{elevation}, \code{cross_island}, \code{park},
#'   \code{sigma_h}, \code{sigma_v}, \code{accuracy_tier}
#' @param verbose Logical. If TRUE, print warnings for edge cases (default: FALSE)
#' @return Tibble of common_min points to add/replace in dataset, with columns:
#'   \describe{
#'     \item{transect}{Transect identifier}
#'     \item{year}{Survey year}
#'     \item{distance}{The common_min distance for this transect}
#'     \item{elevation}{Either existing or interpolated elevation at common_min}
#'     \item{point_type}{"common_min" for all rows}
#'     \item{park}{Park identifier}
#'     \item{cross_island}{Always FALSE for common_min points}
#'     \item{sigma_h}{Horizontal standard error (from existing point or averaged)}
#'     \item{sigma_v}{Vertical standard error (from existing point or averaged)}
#'     \item{accuracy_tier}{Quality tier}
#'     \item{uncertainty}{Position uncertainty (meters, 1-sigma)}
#'     \item{confidence}{"high", "moderate", or "low"}
#'     \item{uncertainty_95ci}{95\% confidence interval (±1.96*uncertainty)}
#'     \item{slope}{Slope at common_min (NA if existing point, calculated if interpolated)}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Calculate common minimum points for all transects
#' common_min_points <- calculate_and_interpolate_common_min(
#'   cleaned_data,
#'   verbose = TRUE
#' )
#'
#' # Add common_min points to existing zero points
#' all_boundary_points <- bind_rows(zero_points, common_min_points)
#' }
calculate_and_interpolate_common_min <- function(data, verbose = FALSE) {

  # Validate input: check for cross_island column
  if (!"cross_island" %in% names(data)) {
    stop("ERROR: cross_island column missing from input data. ",
         "This is a critical bug - check identify_zero_points_all_transects().",
         call. = FALSE)
  }

  # Check for NA values in cross_island and handle them
  if (any(is.na(data$cross_island))) {
    n_na <- sum(is.na(data$cross_island))
    warning(sprintf("Found %d rows with NA in cross_island column. These will be excluded.", n_na),
            call. = FALSE)
    data <- data |> filter(!is.na(cross_island))
  }

  # Step 1: Calculate common minimum distance for each transect
  common_mins <- data |>
    filter(cross_island == FALSE) |>
    group_by(transect, year) |>
    summarise(year_min = min(distance), .groups = 'drop') |>
    group_by(transect) |>
    summarise(common_min_distance = max(year_min), .groups = 'drop')

  # Check if we have any transects to process
  if (nrow(common_mins) == 0) {
    if (verbose) cat("No land-to-water transects found. Skipping common_min calculation.\n")
    return(tibble())
  }

  # Step 2: For each transect-year, generate the common_min point with accuracy
  # Use group_split() + lapply() instead of group_modify() to avoid memory accumulation
  result <- data |>
    filter(cross_island == FALSE) |>
    inner_join(common_mins, by = "transect") |>
    group_by(transect, year) |>
    group_split() |>
    lapply(function(df) {

      # Safety check: ensure df is not empty
      if (nrow(df) == 0) {
        return(tibble())
      }

      # Get common_min distance with validation
      cm <- first(df$common_min_distance)

      # Validate cm value
      if (is.na(cm) || !is.finite(cm)) {
        return(tibble())
      }

      # Skip if common_min is at distance = 0 (avoid conflicts with zero crossings)
      # Use a small tolerance (1mm) for this check since it's a physical constraint, not a measurement tolerance
      if (abs(cm) < 0.001) {
        return(tibble())
      }

      # Calculate distance range with validation
      dist_min <- min(df$distance, na.rm = TRUE)
      dist_max <- max(df$distance, na.rm = TRUE)

      # Check for valid distance range
      if (!is.finite(dist_min) || !is.finite(dist_max)) {
        return(tibble())
      }

      # Skip if common_min is outside the measured range for this year
      if (cm < dist_min || cm > dist_max) {
        return(tibble())
      }

      # Check if we already have a point AT common_min (within error-aware tolerance)
      # Use 3-sigma rule: points within 3*sigma_h are statistically indistinguishable
      # If multiple candidates exist, use the one closest to common_min

      # Check for NA values in sigma_h and use fallback if needed
      if (any(is.na(df$sigma_h))) {
        point_tolerances <- rep(0.081, nrow(df))  # Use worst-case tolerance (3 * 0.027m)
      } else {
        point_tolerances <- 3 * df$sigma_h
      }

      existing_idx <- which(abs(df$distance - cm) < point_tolerances)

      # If multiple matches, choose the closest one
      if (length(existing_idx) > 1) {
        distances_to_cm <- abs(df$distance[existing_idx] - cm)
        existing_idx <- existing_idx[which.min(distances_to_cm)]
      }

      if (length(existing_idx) > 0) {
        # Use existing point's values
        existing_point <- df[existing_idx[1], ]

        # EDGE CASE: Check if this is a measured_zero point
        if (!is.na(existing_point$point_type) && existing_point$point_type == "measured_zero") {
          if (verbose) {
            warning(sprintf(
              "Common minimum coincides with measured zero crossing\n  Transect: %s, Year: %s, Distance: %.2fm\n  Point relabeled from 'measured_zero' to 'common_min'",
              first(df$transect),
              first(df$year),
              cm
            ), call. = FALSE)
          }
        }

        # Return point with point_type overridden to "common_min"
        tibble(
          transect = first(df$transect),
          year = first(df$year),
          distance = cm,
          elevation = existing_point$elevation,
          point_type = 'common_min',
          park = first(df$park),
          cross_island = FALSE,
          sigma_h = existing_point$sigma_h,
          sigma_v = existing_point$sigma_v,
          accuracy_tier = existing_point$accuracy_tier,
          uncertainty = existing_point$uncertainty,
          confidence = existing_point$confidence,
          uncertainty_95ci = existing_point$uncertainty_95ci,
          slope = if_else(is.na(existing_point$slope), NA_real_, existing_point$slope)
        )
      } else {
        # No existing point - interpolate elevation at common_min

        # Find bracketing points
        lower_candidates <- which(df$distance < cm)
        upper_candidates <- which(df$distance > cm)

        # Check if bracketing points exist
        if (length(lower_candidates) == 0 || length(upper_candidates) == 0) {
          return(tibble())  # Cannot interpolate without bracketing points
        }

        lower_idx <- max(lower_candidates)
        upper_idx <- min(upper_candidates)

        # Interpolate elevation
        interp_elev <- tryCatch({
          approx(df$distance, df$elevation, xout = cm)$y
        }, error = function(e) {
          NA_real_
        })

        # Return empty if interpolation failed
        if (is.na(interp_elev)) {
          return(tibble())
        }

        # Average accuracy values from bracketing points
        sigma_h_avg <- mean(c(df$sigma_h[lower_idx], df$sigma_h[upper_idx]), na.rm = TRUE)
        sigma_v_avg <- mean(c(df$sigma_v[lower_idx], df$sigma_v[upper_idx]), na.rm = TRUE)

        # Check for NA in accuracy values (both averages AND individual values)
        # Individual values must be checked because they're used directly in uncertainty calculations
        if (is.na(sigma_h_avg) || is.na(sigma_v_avg) ||
            is.na(df$sigma_h[lower_idx]) || is.na(df$sigma_h[upper_idx]) ||
            is.na(df$sigma_v[lower_idx]) || is.na(df$sigma_v[upper_idx])) {
          return(tibble())  # Cannot calculate uncertainty without complete accuracy values
        }

        # Calculate slope between bracketing points
        slope <- abs((df$elevation[upper_idx] - df$elevation[lower_idx]) /
                    (df$distance[upper_idx] - df$distance[lower_idx]))

        # Check for near-zero or zero slope (would cause division issues)
        if (slope < 1e-6) {
          return(tibble())  # Skip interpolation for flat profiles (unreliable)
        }

        # Calculate uncertainty using same formula as interpolated zero crossings
        # Position error from vertical uncertainty
        sigma_x_vertical <- sqrt(df$sigma_v[lower_idx]^2 + df$sigma_v[upper_idx]^2) / slope

        # Position error from horizontal uncertainty
        sigma_x_horizontal <- sqrt(df$sigma_h[lower_idx]^2 + df$sigma_h[upper_idx]^2)

        # Check for numerical issues (overflow, NaN, Inf)
        if (!is.finite(sigma_x_vertical) || !is.finite(sigma_x_horizontal)) {
          return(tibble())  # Skip if uncertainty calculation produces invalid values
        }

        # Total position uncertainty (combined in quadrature)
        uncertainty <- sqrt(sigma_x_vertical^2 + sigma_x_horizontal^2)

        # Final check for valid uncertainty
        if (!is.finite(uncertainty)) {
          return(tibble())
        }

        # Assign confidence based on slope (same thresholds as interpolated zeros)
        confidence <- ifelse(slope > 0.05, "high",
                           ifelse(slope > 0.02, "moderate", "low"))

        # Get most common accuracy tier from bracketing points
        tier <- df$accuracy_tier[lower_idx]  # Use lower point's tier

        tibble(
          transect = first(df$transect),
          year = first(df$year),
          distance = cm,
          elevation = interp_elev,
          point_type = 'common_min',
          park = first(df$park),
          cross_island = FALSE,
          sigma_h = sigma_h_avg,
          sigma_v = sigma_v_avg,
          accuracy_tier = tier,
          uncertainty = uncertainty,
          confidence = confidence,
          uncertainty_95ci = 1.96 * uncertainty,
          slope = slope
        )
      }
    }) |>
    bind_rows()

  return(result)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Calculate Transect Geometry Metrics
#'
#' Calculates transect length and overlap metrics for each transect-year.
#' These metrics are independent of the AUC calculation method and provide
#' information about data coverage and consistency across years.
#'
#' @section Overlap Calculation:
#' Overlap is calculated as the shared distance interval across all years for
#' a given transect. A transect with only one year has zero overlap by definition.
#' The overlap percentage is relative to each year's transect length.
#'
#' @param data Data frame with columns: \code{transect}, \code{year}, \code{distance}
#' @return Data frame with columns:
#'   \describe{
#'     \item{transect}{Transect identifier}
#'     \item{year}{Year of measurement}
#'     \item{transect_length}{Total transect length for this year (meters)}
#'     \item{overlap_m}{Shared distance interval across all years (meters)}
#'     \item{overlap_pct}{Overlap as percentage of transect length}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Calculate geometry metrics for all transects
#' metrics <- calculate_transect_metrics(cleaned_data)
#'
#' # Identify transects with low overlap
#' low_overlap <- metrics |>
#'   filter(overlap_pct < 50)
#' }
calculate_transect_metrics <- function(data) {

  # Step 1: Calculate transect length for each transect-year
  transect_lengths <- data |>
    group_by(transect, year) |>
    summarise(
      min_dist = min(distance, na.rm = TRUE),
      max_dist = max(distance, na.rm = TRUE),
      transect_length = max_dist - min_dist,
      .groups = 'drop'
    )

  # Step 2: Calculate overlap metrics
  overlap_metrics <- transect_lengths |>
    group_by(transect) |>
    summarise(
      n_years = n(),
      overlap_start = max(min_dist, na.rm = TRUE),  # Rightmost starting point
      overlap_end = min(max_dist, na.rm = TRUE),    # Leftmost ending point
      .groups = 'drop'
    ) |>
    mutate(
      # Calculate overlap length (0 if negative or only one year)
      overlap_m = if_else(
        n_years <= 1,
        0,
        pmax(overlap_end - overlap_start, 0)
      )
    ) |>
    select(transect, overlap_m)

  # Step 3: Join transect lengths with overlap metrics
  transect_lengths |>
    left_join(overlap_metrics, by = "transect") |>
    mutate(
      # Calculate overlap percentage
      overlap_pct = if_else(
        transect_length > 0,
        (overlap_m / transect_length) * 100,
        0
      )
    ) |>
    select(transect, year, overlap_m, overlap_pct, transect_length)
}
