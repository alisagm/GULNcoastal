# plot_shading.R
# Functions for creating shaded area visualization data

# ============================================================================
# SHADE DATA CREATION
# ============================================================================

#' Create shade data for area under curve visualization
#'
#' Converts pre-computed positive segments into dataframe format for ggplot shading.
#' Returns ALL segments instead of just the largest one.
#'
#' @param segments List of segment objects from segment_profile() (segmentation.R).
#'   Each segment should be a list with:
#'   \itemize{
#'     \item \code{is_positive} - Logical indicating if segment is above baseline
#'     \item \code{x} - Numeric vector of distances along transect
#'     \item \code{y} - Numeric vector of elevations
#'   }
#' @param year Optional. Year value to include in output (for multi-year plots)
#' @returns List of dataframes (one per segment) with shade data, or NULL if no segments.
#'   Each dataframe contains:
#'   \itemize{
#'     \item \code{distance} - Numeric vector of x-coordinates
#'     \item \code{elevation} - Numeric vector of y-coordinates
#'     \item \code{year} - Year label (if provided)
#'   }
#' @note Shades ALL regions where elevation >= 0 (supports multi-peak profiles)
#' @export
create_shade_data <- function(segments, year = NULL) {
  if (is.null(segments) || length(segments) == 0) {
    return(NULL)
  }

  # Validate segments structure
  if (!is.list(segments)) {
    warning("Segments is not a list: ", class(segments))
    return(NULL)
  }

  # Filter to positive segments only
  positive_segments <- tryCatch({
    segments[sapply(segments, function(s) {
      if (!is.list(s)) return(FALSE)
      if (!"is_positive" %in% names(s)) return(FALSE)
      return(isTRUE(s$is_positive))
    })]
  }, error = function(e) {
    warning("Error filtering positive segments: ", e$message)
    return(list())
  })

  if (length(positive_segments) == 0) {
    return(NULL)
  }

  shade_data_list <- lapply(positive_segments, function(seg) {
    # Validate segment structure
    if (!is.list(seg) || !"x" %in% names(seg) || !"y" %in% names(seg)) {
      warning("Invalid segment structure - missing x or y")
      return(NULL)
    }

    # Filter to positive elevations only (matching AUC calculation)
    positive_idx <- seg$y >= 0

    if (sum(positive_idx) < 2) {
      # Need at least 2 points for geom_area
      return(NULL)
    }

    shade_data <- data.frame(
      distance = seg$x[positive_idx],
      elevation = seg$y[positive_idx]
    )

    if (!is.null(year)) {
      shade_data$year <- year
    }

    return(shade_data)
  })

  # Remove NULL entries
  shade_data_list <- Filter(Negate(is.null), shade_data_list)

  if (length(shade_data_list) == 0) {
    return(NULL)
  }

  return(shade_data_list)
}
