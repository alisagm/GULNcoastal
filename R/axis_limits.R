# axis_limits.R
# Functions for calculating plot axis limits

# ============================================================================
# CONSTANTS
# ============================================================================

## Axis and padding
#' Default axis padding fraction
#'
#' The fraction of the data range to add as padding around axis limits.
#' Default is 0.025 (2.5% padding on each side).
#'
#' @keywords internal
AXIS_PADDING_FRACTION <- 0.025

# ============================================================================
# AXIS LIMITS
# ============================================================================

#' Calculate axis limits from topographic profile data
#'
#' Calculates appropriate x and y axis limits for topographic profile plots
#' based on the data range with padding. Includes special handling for
#' specific parks to ensure consistent scaling and exclude known outliers.
#'
#' @param data A data frame containing topographic profile data with columns:
#'   \describe{
#'     \item{distance}{Numeric. Distance along transect}
#'     \item{elevation}{Numeric. Elevation values}
#'     \item{park}{Character. Park identifier (optional, used when park != "all")}
#'     \item{transect}{Character. Transect identifier (optional, used for outlier filtering)}
#'     \item{year}{Numeric. Year of measurement (optional, used for outlier filtering)}
#'   }
#' @param park Character. Park filter for calculating limits. Options:
#'   \describe{
#'     \item{"all"}{Use all data (default)}
#'     \item{"PAIS"}{Filter to Padre Island National Seashore}
#'     \item{"GUIS"}{Filter to Gulf Islands National Seashore with special handling}
#'   }
#'   For GUIS, excludes the fp09 2018 outlier and uses PAIS-based y-padding
#'   for consistency across parks.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{xlim}{Numeric vector of length 2 with x-axis limits (min, max)}
#'     \item{ylim}{Numeric vector of length 2 with y-axis limits (min, max)}
#'   }
#'   Both ranges include padding defined by \code{AXIS_PADDING_FRACTION}.
#'
#' @details
#' The function applies \code{AXIS_PADDING_FRACTION} (default 2.5\%) to the
#' data range to create aesthetically pleasing axis limits. Special handling
#' is implemented for GUIS to exclude known outliers and maintain consistent
#' y-axis scaling with PAIS data.
#'
#' If the filtered dataset is empty (no data matching the park filter), the
#' function falls back to using all available data.
#'
#' @examples
#' \dontrun{
#' # Calculate limits for all parks
#' limits <- calculate_axis_limits(profile_data, park = "all")
#' plot(distance ~ elevation, xlim = limits$xlim, ylim = limits$ylim)
#'
#' # Calculate limits for PAIS only
#' limits_pais <- calculate_axis_limits(profile_data, park = "PAIS")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
calculate_axis_limits <- function(data, park = "all") {
  if(park != "all") {
    filtered_data <- data %>% filter(park == !!park)
    if(nrow(filtered_data) == 0) {
      filtered_data <- data
    }
  } else {
    filtered_data <- data
  }

  # Special handling for GUIS: exclude fp09 2018 outlier
  if(park == "GUIS") {
    filtered_data <- filtered_data %>%
      filter(!(transect == "fp09" & year == 2018))

    if(nrow(filtered_data) == 0) {
      filtered_data <- data %>% filter(park == "GUIS")
    }
  }

  # Calculate x-axis limits
  x_min <- min(filtered_data$distance, na.rm = TRUE)
  x_max <- max(filtered_data$distance, na.rm = TRUE)
  x_range <- x_max - x_min
  x_padding <- x_range * AXIS_PADDING_FRACTION
  xlim <- c(x_min - x_padding, x_max + x_padding)

  # Calculate y-axis limits
  y_min <- min(filtered_data$elevation, na.rm = TRUE)
  y_max <- max(filtered_data$elevation, na.rm = TRUE)
  y_range <- y_max - y_min

  # Use PAIS-based buffer for GUIS for consistency
  if(park == "GUIS") {
    pais_data <- data %>% filter(park == "PAIS")
    if(nrow(pais_data) > 0) {
      pais_range <- max(pais_data$elevation, na.rm = TRUE) - min(pais_data$elevation, na.rm = TRUE)
      y_padding <- pais_range * AXIS_PADDING_FRACTION
    } else {
      y_padding <- y_range * AXIS_PADDING_FRACTION
    }
  } else {
    y_padding <- y_range * AXIS_PADDING_FRACTION
  }

  ylim <- c(y_min - y_padding, y_max + y_padding)

  return(list(xlim = xlim, ylim = ylim))
}
