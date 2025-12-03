# plot_config_presets.R
# Preset configurations for common plotting scenarios

# ============================================================================
# PRESET CONFIGURATIONS
# ============================================================================

#' Quick configuration for rapid exploration
#'
#' Minimal configuration for quick visual inspection of transect data.
#' Uses sensible defaults optimized for fast iteration during data exploration.
#' Perfect for interactive sessions when you want to quickly see what your data looks like.
#'
#' Use case: "Just show me the most recent data quickly"
#'
#' @param n_years Number of most recent years to show (default: 1)
#' @param format Output format: "png" (default, fast) or "pdf" (publication)
#' @param show_uncertainty Whether to show uncertainty bands (default: FALSE for speed)
#' @returns plot_config object
#' @export
#'
#' @examples
#' # Quick look at most recent year
#' config <- config_quick()
#'
#' # Quick look at last 2 years with uncertainty
#' config <- config_quick(n_years = 2, show_uncertainty = TRUE)
config_quick <- function(n_years = 1, format = "png", show_uncertainty = FALSE) {
  create_plot_config(
    name = "quick",
    data_filter = create_data_filter(year_selector = years_recent(n_years)),
    filename_suffix = "quick",
    axis_limits = "park",
    show_auc = TRUE,
    shade_area = TRUE,
    show_inset = n_years > 1,
    show_auc_text = n_years == 1,
    show_uncertainty_bands = show_uncertainty,
    show_auc_uncertainty = show_uncertainty,
    show_point_uncertainty = FALSE,
    show_integration_boundaries = FALSE,
    file_format = format,
    width = 10,
    height = 6,
    dpi = 150,  # Lower DPI for faster rendering
    verbose = FALSE
  )
}

#' Default configuration for automated full analysis
#'
#' Generates all plots with all features enabled, including uncertainty visualization.
#' Selects all available data (all parks, years, and transects).
#'
#' Use case: "Generate complete report with all transects and years"
#'
#' @returns plot_config object
#' @export
#'
#' @examples
#' # Create config for full analysis
#' config <- config_automated_full()
config_automated_full <- function() {
  create_plot_config(
    name = "automated_full",
    data_filter = create_data_filter(),  # Select all data
    axis_limits = "park",
    show_auc = TRUE,
    shade_area = TRUE,
    show_inset = TRUE,
    show_auc_text = TRUE,
    show_uncertainty_bands = FALSE,
    show_auc_uncertainty = TRUE,
    show_point_uncertainty = FALSE,
    show_integration_boundaries = FALSE,
    uncertainty_alpha = 0.3,
    uncertainty_color = "gray70",
    file_format = "jpg",
    dpi = 300,
    verbose = FALSE
  )
}

#' Configuration for temporal comparison plots
#'
#' Generates plots comparing first survey year with recent years.
#' Optimized for temporal change analysis with clean visualization.
#'
#' Use case: "Compare baseline with recent conditions"
#'
#' @param n_recent Number of recent years to include (default: 2)
#' @returns plot_config object with year filtering
#' @export
#'
#' @examples
#' # Compare baseline with 2 most recent years
#' config <- config_temporal()
#'
#' # Compare baseline with 3 most recent years
#' config <- config_temporal(n_recent = 3)
config_temporal <- function(n_recent = 2) {
  create_plot_config(
    name = "temporal",
    data_filter = create_data_filter(year_selector = years_baseline_recent(n_recent)),
    filename_suffix = "recent",
    axis_limits = "park",
    show_auc = TRUE,
    show_uncertainty_bands = FALSE,
    show_inset = TRUE,
    file_format = "png",
    dpi = 300
  )
}

#' Configuration for annual progression plots
#'
#' Generates a separate set of plots for each survey year, showing temporal
#' progression. For each year as a "target year", plots include:
#'   - First survey year (baseline)
#'   - Year preceding target (if different from first)
#'   - Target year
#'
#' Special cases:
#'   - If target is first year: plots only that year
#'   - If year before target is first: plots first + target
#'
#' Each target year gets its own subdirectory: plots/annual/<year>/
#'
#' @param data Dataframe with transect data (must have 'year' and 'park' columns)
#' @param base_config Optional base configuration to modify
#' @param name Configuration name (default: "annual")
#' @param ... Additional parameters to pass to configuration
#' @returns plot_config object with annual_mode = TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data
#' results <- load_transect_results()
#' data <- results$data
#'
#' # Create annual progression config
#' config <- config_annual(data)
#'
#' # Use with visualization (will generate plots for each year)
#' run_visualization(config = config, verbose = TRUE)
#' }
config_annual <- function(data,
                          base_config = NULL,
                          name = "annual",
                          ...) {

  # Validate input
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }

  if (!all(c("year", "park") %in% names(data))) {
    stop("data must contain 'year' and 'park' columns", call. = FALSE)
  }

  # Create or modify configuration
  if (is.null(base_config)) {
    config <- create_plot_config(
      name = name,
      data_filter = create_data_filter(),  # Select all data
      filename_suffix = "annual",
      annual_mode = TRUE,
      axis_limits = "park",
      show_auc = TRUE,
      show_uncertainty_bands = FALSE,
      show_inset = TRUE,
      file_format = "png",
      dpi = 300,
      ...
    )
  } else {
    config <- modify_config(
      base_config,
      name = name,
      filename_suffix = "annual",
      annual_mode = TRUE,
      ...
    )
  }

  return(config)
}

# ============================================================================
# DATA FILTER-BASED PRESETS
# ============================================================================

#' Create configuration for first and last survey years
#'
#' Uses the data_filter system for park-aware year selection.
#' Year selection happens at resolution time, automatically handling
#' different survey schedules per park.
#'
#' @param n_recent Number of recent years to include (default: 2)
#' @param ... Additional parameters to pass to \code{\link{create_plot_config}}
#' @returns plot_config object
#' @export
#'
#' @examples
#' # First year plus 2 most recent years per park
#' config <- config_first_and_recent()
#'
#' # First year plus 3 most recent years
#' config <- config_first_and_recent(n_recent = 3)
#'
#' # Custom settings
#' config <- config_first_and_recent(n_recent = 2, file_format = "pdf", dpi = 600)
config_first_and_recent <- function(n_recent = 2, ...) {
  create_plot_config(
    name = paste0("first_and_last_", n_recent, "_years"),
    data_filter = create_data_filter(year_selector = years_baseline_recent(n_recent)),
    filename_suffix = "recent",
    ...
  )
}

#' Create configuration for park-specific first and recent years
#'
#' Filters to a single park and selects first + recent years.
#'
#' @param park Park code to filter (e.g., "GUIS", "PAIS")
#' @param n_recent Number of recent years to include (default: 2)
#' @param ... Additional parameters to pass to \code{\link{create_plot_config}}
#' @returns plot_config object
#' @export
#'
#' @examples
#' # PAIS first year plus 2 most recent
#' config <- config_park_first_and_recent("PAIS")
#'
#' # GUIS first year plus 3 most recent with PDF output
#' config <- config_park_first_and_recent("GUIS", n_recent = 3, file_format = "pdf")
config_park_first_and_recent <- function(park,
                                         n_recent = 2,
                                         ...) {
  create_plot_config(
    name = paste0(park, "_first_and_last_", n_recent, "_years"),
    data_filter = create_data_filter(
      year_selector = years_baseline_recent(n_recent),
      parks = park,
      name = paste0(park, "_baseline_recent_", n_recent)
    ),
    filename_suffix = "recent",
    ...
  )
}
