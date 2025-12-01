# R/plot_positioning.R
# Functions for positioning labels and insets on plots

# ============================================================================
# INTERNAL CONSTANTS
# ============================================================================

# Internal constants - Label positioning
.LABEL_Y_DEFAULT_POS <- 0.85          # Default vertical position for labels (85%)
.LABEL_Y_DEFAULT_MIN <- 0.75          # Default label zone minimum (75%)
.LABEL_Y_DEFAULT_MAX <- 0.95          # Default label zone maximum (95%)
.LABEL_Y_TOP_POS <- 0.95              # Top position for labels (95%)
.LABEL_Y_TOP_MIN <- 0.88              # Top label zone minimum (88%)
.LABEL_Y_TOP_MAX <- 1.0               # Top label zone maximum (100%)
.LABEL_Y_BOTTOM_POS <- 0.15           # Bottom position for labels (15%)
.LABEL_Y_BOTTOM_MIN <- 0.05           # Bottom label zone minimum (5%)
.LABEL_Y_BOTTOM_MAX <- 0.25           # Bottom label zone maximum (25%)
.TEXT_BUFFER_FRACTION <- 0.08         # Horizontal clearance for rotated text (8%)

# Internal constants - Inset positioning
.MAX_INSET_WIDTH <- 0.25              # Maximum inset width (25% of plot)
.FIXED_INSET_HEIGHT <- 0.25           # Fixed inset height (25% of plot)
.INSET_BARS_MAX <- 5                  # Maximum bars for full-width inset
.INSET_UPPER_RIGHT_LEFT <- 0.8        # Upper-right inset left edge
.INSET_UPPER_RIGHT_BOTTOM <- 0.7      # Upper-right inset bottom edge
.INSET_UPPER_RIGHT_RIGHT <- 0.98      # Upper-right inset right edge
.INSET_UPPER_RIGHT_TOP <- 0.95        # Upper-right inset top edge
.INSET_UPPER_LEFT_LEFT <- 0.12        # Upper-left inset left edge (gridline)
.INSET_UPPER_LEFT_BOTTOM <- 0.65      # Upper-left inset bottom edge
.INSET_UPPER_LEFT_RIGHT <- 0.34       # Upper-left inset right edge
.INSET_UPPER_LEFT_TOP <- 0.95         # Upper-left inset top edge
.INSET_LOWER_RIGHT_LEFT <- 0.76       # Lower-right inset left edge
.INSET_LOWER_RIGHT_BOTTOM <- 0.05     # Lower-right inset bottom edge
.INSET_LOWER_RIGHT_RIGHT <- 0.98      # Lower-right inset right edge
.INSET_LOWER_RIGHT_TOP <- 0.35        # Lower-right inset top edge
.INSET_LOWER_LEFT_LEFT <- 0.12        # Lower-left inset left edge
.INSET_LOWER_LEFT_BOTTOM <- 0.05      # Lower-left inset bottom edge
.INSET_LOWER_LEFT_RIGHT <- 0.34       # Lower-left inset right edge
.INSET_LOWER_LEFT_TOP <- 0.35         # Lower-left inset top edge

# Internal constants - Overlap penalties
.PROXIMITY_BUFFER_FRACTION <- 0.10    # Proximity buffer around inset (10%)
.AXIS_MARGIN_FRACTION <- 0.15         # Margin from axes (15%)
.DATA_OVERLAP_PENALTY <- 10           # Penalty multiplier for direct data overlap
.PROXIMITY_PENALTY <- 2               # Penalty multiplier for nearby data
.AXIS_OVERLAP_PENALTY <- 50           # Penalty for overlapping axes
.VLINE_OVERLAP_PENALTY <- 100         # Penalty for passing through vline
.VLINE_PROXIMITY_PENALTY_MAX <- 20    # Max penalty for being near vline

# ============================================================================
# LABEL POSITIONING
# ============================================================================

#' Find optimal vertical position for common minimum label
#'
#' Tests candidate vertical positions to avoid overlap with data curve.
#' Positions are specified in data coordinates (y-axis units).
#' Priority: default (85% height) → top → bottom
#'
#' @param data Dataframe containing distance and elevation data
#' @param vline_x Double. X-coordinate of the vertical line
#' @param xlim Numeric vector of x-axis limits c(min, max)
#' @param ylim Numeric vector of y-axis limits c(min, max)
#' @returns Double. Y-coordinate for label placement in data coordinate space
#' @export
find_label_position <- function(data, vline_x, xlim, ylim) {
  x_range <- xlim[2] - xlim[1]
  y_range <- ylim[2] - ylim[1]

  fraction_to_y <- function(frac) {
    ylim[1] + frac * y_range
  }

  # Define candidate positions with priority order
  candidates <- list(
    list(name = "default",
         y_pos = fraction_to_y(.LABEL_Y_DEFAULT_POS),
         y_min = fraction_to_y(.LABEL_Y_DEFAULT_MIN),
         y_max = fraction_to_y(.LABEL_Y_DEFAULT_MAX)),
    list(name = "top",
         y_pos = fraction_to_y(.LABEL_Y_TOP_POS),
         y_min = fraction_to_y(.LABEL_Y_TOP_MIN),
         y_max = fraction_to_y(.LABEL_Y_TOP_MAX)),
    list(name = "bottom",
         y_pos = fraction_to_y(.LABEL_Y_BOTTOM_POS),
         y_min = fraction_to_y(.LABEL_Y_BOTTOM_MIN),
         y_max = fraction_to_y(.LABEL_Y_BOTTOM_MAX))
  )

  # Check each candidate position for overlap
  for (candidate in candidates) {
    x_buffer <- x_range * .TEXT_BUFFER_FRACTION

    nearby_points <- sum(
      data$distance >= (vline_x - x_buffer) &
      data$distance <= (vline_x + x_buffer) &
      data$elevation >= candidate$y_min &
      data$elevation <= candidate$y_max,
      na.rm = TRUE
    )

    if (nearby_points == 0) {
      return(candidate$y_pos)
    }
  }

  return(candidates[[1]]$y_pos)
}

# ============================================================================
# INSET POSITIONING
# ============================================================================

#' Adjust inset width based on number of bars
#'
#' Scales inset width proportionally to maintain consistent bar width.
#' Coordinates are in normalized plot space (0-1).
#'
#' @param position Named list with left, bottom, right, top coordinates
#' @param n_years Integer. Number of years (bars) in the chart
#' @param corner Character. Corner location: "left" or "right"
#' @returns Named list with adjusted coordinates in normalized space (0-1)
#' @export
adjust_inset_width <- function(position, n_years, corner = "right") {
  width_per_bar <- .MAX_INSET_WIDTH / .INSET_BARS_MAX
  actual_width <- min(width_per_bar * n_years, .MAX_INSET_WIDTH)

  if (corner == "right") {
    position$left <- position$right - actual_width
  } else {
    position$right <- position$left + actual_width
  }

  position$top <- position$bottom + .FIXED_INSET_HEIGHT

  return(position)
}

#' Calculate overlap penalty for inset position
#'
#' Internal helper function to calculate penalty score for a candidate inset position.
#' Lower penalties indicate better positions.
#'
#' @param pos List. Position with left, bottom, right, top, corner
#' @param data Dataframe with distance and elevation
#' @param xlim,ylim Numeric vectors of axis limits
#' @param common_min Double or NA. Position of common minimum vline
#' @returns Double. Penalty score (lower is better)
#' @keywords internal
#' @noRd
calculate_inset_penalty <- function(pos, data, xlim, ylim, common_min) {
  x_range <- xlim[2] - xlim[1]
  y_range <- ylim[2] - ylim[1]

  norm_to_data <- function(norm_coords, range_min, range_max) {
    range_min + norm_coords * (range_max - range_min)
  }

  x_left <- norm_to_data(pos$left, xlim[1], xlim[2])
  x_right <- norm_to_data(pos$right, xlim[1], xlim[2])
  y_bottom <- norm_to_data(pos$bottom, ylim[1], ylim[2])
  y_top <- norm_to_data(pos$top, ylim[1], ylim[2])

  penalty <- 0

  # 1. Direct data overlap
  data_overlap <- sum(
    data$distance >= x_left &
    data$distance <= x_right &
    data$elevation >= y_bottom &
    data$elevation <= y_top,
    na.rm = TRUE
  )
  penalty <- penalty + (data_overlap * .DATA_OVERLAP_PENALTY)

  # 2. Proximity to data
  proximity_buffer_x <- x_range * .PROXIMITY_BUFFER_FRACTION
  proximity_buffer_y <- y_range * .PROXIMITY_BUFFER_FRACTION

  x_left_buffer <- x_left - proximity_buffer_x
  x_right_buffer <- x_right + proximity_buffer_x
  y_bottom_buffer <- y_bottom - proximity_buffer_y
  y_top_buffer <- y_top + proximity_buffer_y

  proximity_overlap <- sum(
    data$distance >= x_left_buffer &
    data$distance <= x_right_buffer &
    data$elevation >= y_bottom_buffer &
    data$elevation <= y_top_buffer &
    !(data$distance >= x_left &
      data$distance <= x_right &
      data$elevation >= y_bottom &
      data$elevation <= y_top),
    na.rm = TRUE
  )
  penalty <- penalty + (proximity_overlap * .PROXIMITY_PENALTY)

  # 3. Proximity to axes
  if (pos$bottom < .AXIS_MARGIN_FRACTION) {
    axis_overlap_fraction <- (.AXIS_MARGIN_FRACTION - pos$bottom) / .AXIS_MARGIN_FRACTION
    penalty <- penalty + (axis_overlap_fraction * .AXIS_OVERLAP_PENALTY)
  }

  if (pos$left < .INSET_UPPER_LEFT_LEFT) {
    axis_overlap_fraction <- (.INSET_UPPER_LEFT_LEFT - pos$left) / .INSET_UPPER_LEFT_LEFT
    penalty <- penalty + (axis_overlap_fraction * .AXIS_OVERLAP_PENALTY)
  }

  # 4. Overlap with common minimum vline
  if (!is.na(common_min)) {
    if (common_min >= x_left && common_min <= x_right) {
      penalty <- penalty + .VLINE_OVERLAP_PENALTY
    } else {
      proximity_threshold <- x_range * .PROXIMITY_BUFFER_FRACTION
      distance_to_vline <- min(abs(common_min - x_left), abs(common_min - x_right))
      if (distance_to_vline < proximity_threshold) {
        proximity_penalty <- (1 - distance_to_vline / proximity_threshold) * .VLINE_PROXIMITY_PENALTY_MAX
        penalty <- penalty + proximity_penalty
      }
    }
  }

  return(penalty)
}

#' Find optimal inset position to minimize data overlap
#'
#' Tests multiple corner positions and selects the one with fewest overlapping data points.
#' Returns coordinates in normalized plot space (0-1), where (0,0) is bottom-left
#' and (1,1) is top-right.
#'
#' @param data Dataframe containing distance and elevation data
#' @param xlim Numeric vector of x-axis limits c(min, max)
#' @param ylim Numeric vector of y-axis limits c(min, max)
#' @param common_min Double or NA. Position of common minimum vline, if present
#' @returns Named list with left, bottom, right, top coordinates (all in 0-1 space)
#'   and corner location ("left" or "right")
#' @export
find_best_inset_position <- function(data, xlim, ylim, common_min = NA) {
  # Define candidate positions using named constants
  candidates <- list(
    list(name = "upper-right",
         left = .INSET_UPPER_RIGHT_LEFT,
         bottom = .INSET_UPPER_RIGHT_BOTTOM,
         right = .INSET_UPPER_RIGHT_RIGHT,
         top = .INSET_UPPER_RIGHT_TOP,
         corner = "right"),
    list(name = "upper-left",
         left = .INSET_UPPER_LEFT_LEFT,
         bottom = .INSET_UPPER_LEFT_BOTTOM,
         right = .INSET_UPPER_LEFT_RIGHT,
         top = .INSET_UPPER_LEFT_TOP,
         corner = "left"),
    list(name = "lower-right",
         left = .INSET_LOWER_RIGHT_LEFT,
         bottom = .INSET_LOWER_RIGHT_BOTTOM,
         right = .INSET_LOWER_RIGHT_RIGHT,
         top = .INSET_LOWER_RIGHT_TOP,
         corner = "right"),
    list(name = "lower-left",
         left = .INSET_LOWER_LEFT_LEFT,
         bottom = .INSET_LOWER_LEFT_BOTTOM,
         right = .INSET_LOWER_LEFT_RIGHT,
         top = .INSET_LOWER_LEFT_TOP,
         corner = "left")
  )

  # Evaluate each candidate position
  overlap_scores <- sapply(candidates, function(pos) {
    calculate_inset_penalty(pos, data, xlim, ylim, common_min)
  })

  # Select position with minimum penalty
  best_idx <- which.min(overlap_scores)
  best_position <- candidates[[best_idx]]

  return(list(
    left = best_position$left,
    bottom = best_position$bottom,
    right = best_position$right,
    top = best_position$top,
    corner = best_position$corner
  ))
}
