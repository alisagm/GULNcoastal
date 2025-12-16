# plot_colors.R
# Color palette and formatting functions for beach profile plots

# ============================================================================
# CONSTANTS
# ============================================================================

#' Expected cadence between survey years
#'
#' The typical interval (in years) between successive surveys. Used by
#' \code{format_year_sequence()} to identify consecutive runs of years.
#' Default is 2 years.
#'
#' @keywords internal
YEAR_CADENCE <- 2

# ============================================================================
# COLOR PALETTE
# ============================================================================

#' Get park-specific color palette for years
#'
#' Creates viridis-based color scales for visualizing temporal data.
#' When park information is available, generates separate color sequences
#' for each park's year range. Dynamically detects parks from data, so
#' new parks are automatically supported without code changes.
#'
#' @param data A data frame containing year information and optionally park
#'   identifiers. Must have a \code{year} column. If a \code{park} column
#'   is present, colors will be assigned separately for each park.
#' @param palette Character. Color palette to use from the viridis family.
#'   Options are:
#'   \describe{
#'     \item{"viridis"}{The original viridis palette (default)}
#'     \item{"magma"}{The magma palette (purple to yellow)}
#'     \item{"plasma"}{The plasma palette (purple to pink)}
#'     \item{"inferno"}{The inferno palette (black to yellow)}
#'     \item{"cividis"}{The cividis palette (colorblind-friendly)}
#'     \item{"turbo"}{The turbo palette (rainbow-like)}
#'   }
#'   If an invalid palette is specified, defaults to "viridis" with a warning.
#'
#' @return A named character vector mapping years to hex color codes.
#'   Names are year values as character strings, values are hex colors.
#'   When multiple parks are present, all years across all parks are included.
#'
#' @details
#' For single-year datasets, returns a default dark purple color (#440154FF).
#' The function uses the full viridis color range to maximize color
#' differentiation across years. When parks are detected, each park's years
#' receive their own color sequence, which may result in the same year
#' having different colors in different parks.
#'
#' @examples
#' \dontrun{
#' # Simple case with just years
#' data <- data.frame(year = c(2010, 2012, 2014))
#' palette <- get_color_palette(data)
#'
#' # Multiple parks with different color scheme
#' data <- data.frame(
#'   year = c(2010, 2012, 2014, 2011, 2013),
#'   park = c("PAIS", "PAIS", "PAIS", "GUIS", "GUIS")
#' )
#' palette <- get_color_palette(data, palette = "plasma")
#' }
#'
#' @importFrom dplyr filter pull
#' @export
get_color_palette <- function(data, palette = "viridis") {
  # Validate palette choice
  valid_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis", "turbo")
  if (!palette %in% valid_palettes) {
    warning("Unknown palette '", palette, "'. Using 'viridis'. ",
            "Valid options: ", paste(valid_palettes, collapse = ", "),
            call. = FALSE)
    palette <- "viridis"
  }

  # Helper function to get colors from chosen palette
  get_palette_colors <- function(n) {
    if (n == 1) {
      return("#440154FF")  # Viridis dark purple as single-color default
    }
    switch(palette,
           "viridis" = viridis::viridis(n),
           "magma" = viridis::magma(n),
           "plasma" = viridis::plasma(n),
           "inferno" = viridis::inferno(n),
           "cividis" = viridis::cividis(n),
           "turbo" = viridis::turbo(n),
           viridis::viridis(n)  # fallback
    )
  }

  # If no park column, create palette for all years
  if (!"park" %in% names(data)) {
    all_years <- data |>
      pull(year) |>
      unique() |>
      sort()

    n_years <- length(all_years)
    colors <- get_palette_colors(n_years)
    names(colors) <- as.character(all_years)
    return(colors)
  }

  # Dynamically detect parks from data (not hardcoded!)
  parks_in_data <- unique(data$park)

  all_colors <- character()

  for (park_name in parks_in_data) {
    park_years <- data |>
      filter(park == park_name) |>
      pull(year) |>
      unique() |>
      sort()

    if (length(park_years) == 0) next

    park_colors <- get_palette_colors(length(park_years))
    names(park_colors) <- as.character(park_years)
    all_colors <- c(all_colors, park_colors)
  }

  return(all_colors)
}

# ============================================================================
# FORMATTING UTILITIES
# ============================================================================

#' Format year sequence for plot titles
#'
#' Intelligently formats a sequence of years into a compact, readable string.
#' Consecutive runs of more than 2 years (based on expected survey cadence)
#' are formatted as ranges (e.g., "2010-2016"), while non-consecutive years
#' remain comma-separated (e.g., "2010, 2014").
#'
#' @param years Numeric vector of years to format. Will be sorted automatically.
#' @param park Character. Park identifier used for documentation purposes.
#'   The park parameter is currently not used in the logic but maintained for
#'   API consistency and potential future park-specific formatting rules.
#'
#' @return A character string with the formatted year sequence.
#'   Examples:
#'   \describe{
#'     \item{Single year:}{"2010"}
#'     \item{Two years:}{"2010, 2012"}
#'     \item{Consecutive run:}{"2010-2016" (for 2010, 2012, 2014, 2016)}
#'     \item{Mixed:}{"2010-2014, 2018, 2020" (for 2010, 2012, 2014, 2018, 2020)}
#'   }
#'
#' @details
#' The function uses \code{YEAR_CADENCE} (default: 2 years) to determine
#' whether years are consecutive. This respects typical survey intervals where
#' sampling occurs every 2 years rather than annually. Runs of more than 2
#' consecutive years are collapsed into ranges, while shorter runs or isolated
#' years remain comma-separated.
#'
#' @examples
#' \dontrun{
#' # Consecutive years (2-year cadence)
#' format_year_sequence(c(2010, 2012, 2014, 2016), park = "PAIS")
#' # Returns: "2010-2016"
#'
#' # Non-consecutive years
#' format_year_sequence(c(2010, 2014, 2018), park = "GUIS")
#' # Returns: "2010, 2014, 2018"
#'
#' # Mixed consecutive and non-consecutive
#' format_year_sequence(c(2010, 2012, 2014, 2020, 2022), park = "PAIS")
#' # Returns: "2010-2014, 2020, 2022"
#' }
#' @keywords internal
#' @noRd
format_year_sequence <- function(years, park) {
  years <- sort(years)

  if (length(years) == 1) {
    return(as.character(years))
  }

  if (length(years) == 2) {
    return(paste(years, collapse = ", "))
  }

  # Identify consecutive runs using expected cadence
  runs <- list()
  run_start <- 1

  for (i in 2:length(years)) {
    is_consecutive <- (years[i] - years[i-1]) == YEAR_CADENCE

    if (!is_consecutive) {
      runs[[length(runs) + 1]] <- years[run_start:(i-1)]
      run_start <- i
    }
  }
  runs[[length(runs) + 1]] <- years[run_start:length(years)]

  # Format each run
  formatted_segments <- sapply(runs, function(run) {
    if (length(run) > 2) {
      paste(min(run), "-", max(run))
    } else {
      paste(run, collapse = ", ")
    }
  })

  return(paste(formatted_segments, collapse = ", "))
}
