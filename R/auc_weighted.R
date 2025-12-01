# Probability-weighted AUC calculation with uncertainty propagation
#
# This module implements probability-weighted Area Under Curve (AUC) calculations
# that account for measurement uncertainty near sea level. Points near zero elevation
# are probability-weighted to avoid artificially inflating/deflating AUC when
# measurements are uncertain about the true shoreline location.
#
# Key Innovation: For a measured point with elevation ε and vertical uncertainty σ_v:
#   - True elevation ~ N(ε, σ_v) [normally distributed]
#   - P(true elevation > 0) = Φ(ε / σ_v) [where Φ = standard normal CDF]
#   - Weighted elevation = ε × Φ(ε / σ_v)
#
# This prevents overconfident AUC estimates when shoreline location is uncertain.

# ============================================================================
# STEP 1: CALCULATE WEIGHTED ELEVATIONS
# ============================================================================

#' Calculate Probability-Weighted Elevations for All Points
#'
#' Applies probability weighting to points near zero elevation based on their
#' vertical uncertainty. Points with high confidence of being above zero receive
#' minimal adjustment, while points with significant uncertainty have their
#' contribution reduced proportionally.
#'
#' @param data Dataframe with columns: distance, elevation, sigma_v, point_type
#' @return Dataframe with added columns:
#'   - elev_weighted: Nominal (probability-weighted) elevation
#'   - elev_upper: Upper bound (elevation + 1σ, probability-weighted)
#'   - elev_lower: Lower bound (elevation - 1σ, probability-weighted, >= 0)
#'
#' @details
#' For measured_zero points (points within 1.96σ_v of zero):
#'   - Apply probability weighting: ε * Φ(ε / σ_v)
#'   - Φ is the standard normal cumulative distribution function (pnorm)
#'   - Upper/lower bounds are also weighted for consistency
#'   - Lower bound is clamped to >= 0 (cannot have negative area)
#'
#' For all other points:
#'   - No weighting applied (elevation is far from zero)
#'   - Simple bounds: elevation ± sigma_v
#'
#' Physical Interpretation Examples:
#'   - ε = +0.10m, σ_v = 0.05m → z = 2.0 → Φ(2.0) = 0.977
#'     97.7% confidence point is above zero, weighted = 0.098m (2% reduction)
#'   - ε = +0.05m, σ_v = 0.10m → z = 0.5 → Φ(0.5) = 0.691
#'     69.1% confidence point is above zero, weighted = 0.035m (30% reduction)
#'   - ε = -0.05m, σ_v = 0.10m → z = -0.5 → Φ(-0.5) = 0.309
#'     30.9% chance point is above zero, weighted = -0.015m (contribution reduced)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stats pnorm
#' @export
calculate_weighted_elevations <- function(data) {

  # Validate input
  required_cols <- c("elevation", "sigma_v", "point_type")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  data %>%
    mutate(
      # Nominal (weighted) elevation
      elev_weighted = case_when(
        point_type == "measured_zero" ~ elevation * pnorm(elevation / sigma_v),
        TRUE ~ elevation
      ),

      # Upper bound (elevation + 1σ, with weighting for measured_zero)
      elev_upper = case_when(
        point_type == "measured_zero" ~ {
          elev_plus_sigma <- elevation + sigma_v
          elev_plus_sigma * pnorm(elev_plus_sigma / sigma_v)
        },
        TRUE ~ elevation + sigma_v
      ),

      # Lower bound (elevation - 1σ, clamped >= 0, with weighting for measured_zero)
      elev_lower = case_when(
        point_type == "measured_zero" ~ {
          elev_minus_sigma <- pmax(elevation - sigma_v, 0)
          elev_minus_sigma * pnorm(elev_minus_sigma / sigma_v)
        },
        TRUE ~ pmax(elevation - sigma_v, 0)
      )
    )
}


# ============================================================================
# STEP 2: IDENTIFY POSITIVE SEGMENTS
# ============================================================================

#' Identify Continuous Segments Where Weighted Elevation is Positive
#'
#' Segments a profile into positive (above sea level) and negative (below sea level)
#' regions based on probability-weighted elevations. Only positive regions receive
#' segment IDs; negative regions are marked with NA.
#'
#' @param data Dataframe with distance and elev_weighted columns
#' @return List with two components:
#'   - data: Input dataframe with added columns:
#'       * above_zero: Logical, TRUE if elev_weighted > 0
#'       * segment_change: Logical, marks transitions between positive/negative
#'       * segment: Integer segment ID (NA for negative regions)
#'   - segments: Summary dataframe with one row per positive segment:
#'       * segment: Segment ID
#'       * distance_min: Left boundary of segment
#'       * distance_max: Right boundary of segment
#'       * n_points: Number of points in segment
#'
#' @details
#' Segmentation Logic:
#'   1. Mark all points as above_zero (TRUE) or below (FALSE)
#'   2. Detect transitions with segment_change flag
#'   3. Increment segment_id at each transition
#'   4. Only assign segment numbers when above_zero == TRUE
#'   5. Use ceiling(segment_id / 2) to convert alternating IDs to 1,1,2,2,3,3...
#'
#' Example:
#'   Distance | elev_weighted | above_zero | segment_id | segment
#'   0        | +0.5          | TRUE       | 1          | 1
#'   5        | +0.2          | TRUE       | 1          | 1
#'   10       | -0.02         | FALSE      | 2          | NA
#'   15       | +0.08         | TRUE       | 3          | 2
#'
#' @importFrom dplyr arrange mutate lag filter group_by summarize
#' @export
identify_positive_segments_weighted <- function(data) {

  # Validate input
  required_cols <- c("distance", "elev_weighted")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure sorted by distance
  data <- data %>% arrange(distance)

  # Mark transitions between positive and negative regions
  data <- data %>%
    mutate(
      above_zero = elev_weighted >= 0,
      segment_change = above_zero != lag(above_zero, default = !first(above_zero))
    )

  # Assign segment IDs (only to positive regions)
  segment_id <- 0
  data$segment <- NA_integer_

  for (i in seq_len(nrow(data))) {
    if (data$segment_change[i]) {
      segment_id <- segment_id + 1
    }
    # Only assign segment number if we're in a positive region
    if (data$above_zero[i]) {
      data$segment[i] <- ceiling(segment_id / 2)
    }
  }

  # Summarize segment boundaries
  segments <- data %>%
    filter(!is.na(segment)) %>%
    group_by(segment) %>%
    summarize(
      distance_min = min(distance),
      distance_max = max(distance),
      n_points = n(),
      .groups = "drop"
    )

  return(list(data = data, segments = segments))
}


# ============================================================================
# STEP 3: TRAPEZOIDAL INTEGRATION (WITH NEGATIVE CONTRIBUTIONS)
# ============================================================================

#' Calculate AUC Using Trapezoidal Rule for One Segment
#'
#' Applies trapezoidal integration to calculate area under curve for a single
#' segment. CRITICALLY: This function does NOT filter out negative elevations.
#' Negative weighted elevations within a segment contribute negative area, which
#' correctly accounts for probabilistic uncertainty.
#'
#' @param data Dataframe subset for one segment (already filtered to one segment)
#' @param elev_col Name of elevation column to integrate
#'   (default: "elev_weighted", alternatives: "elev_upper", "elev_lower")
#' @return Scalar AUC value (can be negative if segment dips below zero)
#'
#' @details
#' Why keep negative elevations?
#'   Consider a segment from 0-20m where most points are positive but one point
#'   at 10m has elev_weighted = -0.02m. If we remove it, the trapezoidal rule
#'   would draw a straight line across the gap, ignoring the probabilistic "dip"
#'   below zero. By keeping all points, negative weighted elevations contribute
#'   negative area, which is physically correct (there's a ~30% chance that
#'   point is below zero, so it reduces the total AUC appropriately).
#'
#' Trapezoidal Formula:
#'   AUC = Σ[Δx × (y[i] + y[i+1]) / 2] for all consecutive point pairs
#'
#' @importFrom dplyr arrange
#' @export
calculate_trapezoidal_auc <- function(data, elev_col = "elev_weighted") {

  # Ensure sorted by distance
  data <- data %>% arrange(distance)

  # Extract vectors
  x <- data$distance
  y <- data[[elev_col]]

  # Need at least 2 points for integration
  if (length(x) < 2) {
    return(0)
  }

  # Trapezoidal rule: Σ (Δx × (y[i] + y[i+1]) / 2)
  dx <- diff(x)
  y_avg <- (y[-1] + y[-length(y)]) / 2

  auc <- sum(dx * y_avg)

  return(auc)
}


# ============================================================================
# STEP 4: FULL AUC CALCULATION WITH UNCERTAINTY
# ============================================================================

#' Calculate AUC with Uncertainty for One Transect-Year
#'
#' Main function that performs complete probability-weighted AUC calculation
#' with uncertainty propagation for a single transect-year.
#'
#' @param data Dataframe for one transect-year with all Step 5 columns:
#'   - distance, elevation, sigma_v, sigma_h, point_type
#'   - Must include all measured, interpolated, extrapolated, and common_min points
#' @return One-row dataframe with AUC results:
#'   - auc_nominal: Nominal (best-estimate) AUC using weighted elevations
#'   - auc_upper: Upper bound AUC (elevation + 1σ)
#'   - auc_lower: Lower bound AUC (elevation - 1σ)
#'   - auc_uncertainty: Uncertainty in AUC (half the range: (upper - lower) / 2)
#'   - auc_uncertainty_pct: Relative uncertainty as percentage
#'   - n_segments: Number of positive segments
#'   - segment_info: Nested list-column with per-segment details
#'   - segments: List of plotting segments (is_positive, x, y)
#'
#' @details
#' Process:
#'   1. Calculate weighted elevations (nominal, upper, lower bounds)
#'   2. Identify positive segments based on nominal weighted elevations
#'   3. For each positive segment, integrate nominal/upper/lower elevations
#'   4. Sum segment contributions to get total AUC and bounds
#'   5. Calculate uncertainty metrics
#'
#' If no positive segments exist, returns zeros for all metrics.
#'
#' @importFrom dplyr filter mutate if_else
#' @importFrom tibble tibble
#' @export
calculate_auc_with_uncertainty <- function(data) {

  # Ensure we have required columns
  required_cols <- c("distance", "elevation", "sigma_v", "point_type")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  common_min_indices <- which(data$point_type == "common_min")
  if (length(common_min_indices) > 0) {
    left_boundary_idx <- max(common_min_indices)
    data <- data[left_boundary_idx:nrow(data), ]
  }


  # Generate plotting segments (from original data, before modifications)
  # This preserves the original data for plotting while using weighted segmentation
  plotting_segments <- generate_plotting_segments(data)

  # Step 1: Calculate weighted elevations
  data <- calculate_weighted_elevations(data)

  # Step 2: Identify positive segments
  segmented <- identify_positive_segments_weighted(data)
  data <- segmented$data
  segment_boundaries <- segmented$segments

  # Handle case with no positive segments
  if (nrow(segment_boundaries) == 0) {
    return(tibble(
      auc_nominal = 0,
      auc_upper = 0,
      auc_lower = 0,
      auc_uncertainty = 0,
      auc_uncertainty_pct = 0,
      n_segments = 0,
      segment_info = list(NULL),
      segments = list(plotting_segments)
    ))
  }

  # Step 3: Calculate AUC for each segment (nominal, upper, lower)
  segment_results <- lapply(seq_len(nrow(segment_boundaries)), function(i) {
    seg_id <- segment_boundaries$segment[i]
    seg_data <- data %>% filter(segment == seg_id)

    tibble(
      segment = seg_id,
      distance_min = segment_boundaries$distance_min[i],
      distance_max = segment_boundaries$distance_max[i],
      n_points = segment_boundaries$n_points[i],
      auc_nominal = calculate_trapezoidal_auc(seg_data, "elev_weighted"),
      auc_upper = calculate_trapezoidal_auc(seg_data, "elev_upper"),
      auc_lower = calculate_trapezoidal_auc(seg_data, "elev_lower")
    )
  }) %>% bind_rows()

  # Step 4: Sum across segments
  total_auc <- segment_results %>%
    summarize(
      auc_nominal = sum(auc_nominal, na.rm = TRUE),
      auc_upper = sum(auc_upper, na.rm = TRUE),
      auc_lower = sum(auc_lower, na.rm = TRUE),
      n_segments = n(),
      segment_info = list(segment_results)
    ) %>%
    mutate(
      # Calculate uncertainty metrics
      auc_uncertainty = (auc_upper - auc_lower) / 2,
      auc_uncertainty_pct = if_else(
        auc_nominal > 0,
        100 * auc_uncertainty / auc_nominal,
        0
      ),
      # Add plotting segments
      segments = list(plotting_segments)
    )

  return(total_auc)
}


# ============================================================================
# STEP 5: GENERATE SEGMENTS FOR PLOTTING
# ============================================================================

#' Generate Segments for Plotting from Weighted Segmentation
#'
#' Creates segment data structure expected by plotting functions. Segments are
#' identified based on probability-weighted elevations, but the actual elevation
#' values used for plotting are the ORIGINAL (unweighted) elevations.
#'
#' @param data Dataframe for one transect-year with columns:
#'   - distance, elevation (original), sigma_v, point_type
#' @return List of lists, where each element represents a segment with:
#'   - is_positive: Logical, TRUE if segment is above sea level
#'   - x: Numeric vector of distances for this segment
#'   - y: Numeric vector of ORIGINAL elevations for this segment
#'
#' @details
#' Process:
#'   1. Calculate weighted elevations to identify segments
#'   2. Mark positive vs negative regions based on weighted elevations
#'   3. Group consecutive points with same sign into segments
#'   4. For each segment, return ORIGINAL elevations (not weighted)
#'      so plotting shows actual measured/interpolated values
#'
#' This ensures plots display the original data while using probability-weighted
#' segmentation logic to determine what gets shaded as "above sea level".
#'
#' @importFrom dplyr arrange mutate group_by summarise
#' @export
generate_plotting_segments <- function(data) {

  # Ensure we have required columns
  required_cols <- c("distance", "elevation", "sigma_v", "point_type")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Step 1: Calculate weighted elevations for segmentation
  data <- calculate_weighted_elevations(data)

  # Step 2: Identify positive segments
  segmented <- identify_positive_segments_weighted(data)
  data <- segmented$data

  # Step 3: Create segment groups (both positive and negative)
  # Need to group ALL points into segments, not just positive ones
  data <- data %>%
    arrange(distance) %>%
    mutate(
      # Create segment ID for ALL regions (positive and negative)
      segment_group = cumsum(segment_change)
    )

  # Step 4: Convert to plotting format
  # For each segment_group, create list(is_positive, x, y)
  # Use ORIGINAL elevation, not weighted
  segments_summary <- data %>%
    group_by(segment_group) %>%
    summarise(
      is_positive = first(above_zero),
      distance = list(distance),
      elevation = list(elevation),  # ORIGINAL elevation, not weighted
      .groups = 'drop'
    )

  # Convert to list of lists using lapply
  segments_list <- lapply(seq_len(nrow(segments_summary)), function(i) {
    list(
      is_positive = segments_summary$is_positive[i],
      x = segments_summary$distance[[i]],
      y = segments_summary$elevation[[i]]
    )
  })

  return(segments_list)
}


# ============================================================================
# WRAPPER FOR MULTIPLE TRANSECT-YEARS
# ============================================================================

#' Calculate Probability-Weighted AUC for All Transect-Years
#'
#' Convenience wrapper that applies calculate_auc_with_uncertainty() to all
#' transect-year combinations in a dataset.
#'
#' @param data Dataframe with multiple transects and years. Must contain columns:
#'   - transect: Transect identifier
#'   - year: Year identifier
#'   - distance, elevation, sigma_v, point_type: Required for AUC calculation
#' @return Dataframe with one row per transect-year, containing:
#'   - transect, year: Identifiers
#'   - auc_nominal, auc_upper, auc_lower: AUC and bounds
#'   - auc_uncertainty, auc_uncertainty_pct: Uncertainty metrics
#'   - n_segments: Number of positive segments
#'   - segment_info: Nested segment details
#'   - segments: List of plotting segments
#'
#' @importFrom dplyr group_by group_split bind_rows
#' @export
calculate_auc_weighted_all <- function(data) {
  # Use group_split() + lapply() instead of group_modify() to avoid memory accumulation
  data %>%
    group_by(transect, year) %>%
    group_split() %>%
    lapply(function(df) {
      result <- calculate_auc_with_uncertainty(df)
      result$transect <- first(df$transect)
      result$year <- first(df$year)
      return(result)
    }) %>%
    bind_rows()
}


# ============================================================================
# HELPER FUNCTIONS FOR DIAGNOSTICS
# ============================================================================

#' Get Detailed Segment Information for One Transect-Year
#'
#' Returns detailed per-segment breakdown for diagnostic purposes.
#'
#' @param data Dataframe for one transect-year with columns:
#'   - distance, elevation, sigma_v, point_type
#' @return Dataframe with one row per positive segment showing:
#'   - segment: Segment ID
#'   - distance_min, distance_max: Segment boundaries
#'   - n_points: Number of points in segment
#'   - auc_nominal, auc_upper, auc_lower: AUC values for this segment
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom tibble tibble
#' @export
get_segment_details <- function(data) {

  # Calculate weighted elevations
  data <- calculate_weighted_elevations(data)

  # Identify segments
  segmented <- identify_positive_segments_weighted(data)
  data <- segmented$data
  segment_boundaries <- segmented$segments

  # Return empty if no positive segments
  if (nrow(segment_boundaries) == 0) {
    return(tibble(
      segment = integer(0),
      distance_min = numeric(0),
      distance_max = numeric(0),
      n_points = integer(0),
      auc_nominal = numeric(0),
      auc_upper = numeric(0),
      auc_lower = numeric(0)
    ))
  }

  # Calculate per-segment AUC
  # Use explicit lapply() instead of rowwise() to avoid memory accumulation
  lapply(seq_len(nrow(segment_boundaries)), function(i) {
    seg_id <- segment_boundaries$segment[i]
    seg_data <- data %>% filter(segment == seg_id)

    tibble(
      segment = seg_id,
      distance_min = segment_boundaries$distance_min[i],
      distance_max = segment_boundaries$distance_max[i],
      n_points = segment_boundaries$n_points[i],
      auc_nominal = calculate_trapezoidal_auc(seg_data, "elev_weighted"),
      auc_upper = calculate_trapezoidal_auc(seg_data, "elev_upper"),
      auc_lower = calculate_trapezoidal_auc(seg_data, "elev_lower")
    )
  }) %>% bind_rows()
}
