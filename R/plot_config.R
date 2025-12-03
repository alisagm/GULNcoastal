# plot_config.R
# Plot configuration management for flexible and reusable plotting settings

# ============================================================================
# INTERNAL DEFAULTS
# ============================================================================

.DEFAULT_FILE_FORMAT <- "jpg"
.DEFAULT_WIDTH <- 12
.DEFAULT_HEIGHT <- 8
.DEFAULT_DPI <- 300

# ============================================================================
# CONFIGURATION CONSTRUCTOR
# ============================================================================

#' Create plot configuration object
#'
#' Centralizes all plotting settings into a single configuration object.
#' Makes it easy to create, save, and reuse plotting preferences for both
#' automated and interactive workflows.
#'
#' Data selection is specified using a data_filter object, which provides
#' semantic selection with park-aware year filtering. See \code{\link{create_data_filter}}
#' and \code{years_*} selector functions.
#'
#' @param data_filter data_filter object. Semantic data selection.
#'   Use \code{create_data_filter()} with \code{years_*()} selectors
#'   (e.g., \code{years_baseline_recent(2)}).
#' @param axis_limits Character. "park", "dataset", "none", or "custom"
#' @param custom_ylim Numeric vector. Custom y-axis limits c(min, max)
#' @param show_auc Boolean. Include AUC visualization
#' @param shade_area Boolean. Shade AUC areas
#' @param show_inset Boolean. Show inset plots for multi-year plots
#' @param show_auc_text Boolean. Show AUC text annotation for single-year plots
#' @param show_uncertainty_bands Boolean. Show elevation uncertainty ribbons (elev_upper/elev_lower)
#' @param show_auc_uncertainty Boolean. Show AUC confidence intervals in insets/text
#' @param show_point_uncertainty Boolean. Show error bars on individual points
#' @param show_integration_boundaries Boolean. Show vertical lines at AUC integration boundaries (nominal, upper, lower)
#' @param uncertainty_alpha Numeric. Transparency for uncertainty bands (0-1)
#' @param uncertainty_color Character. Color for uncertainty visualization
#' @param theme Character. Plot theme preset: "default", "publication", "presentation", "minimal", "dark"
#' @param color_palette Character. Color palette: "viridis", "magma", "plasma", "inferno", "cividis", "turbo"
#' @param file_format Character. "jpg", "png", or "pdf"
#' @param width Numeric. Plot width in inches
#' @param height Numeric. Plot height in inches
#' @param dpi Numeric. Resolution (dots per inch)
#' @param output_dir Character. Output directory path. NULL = use project-specific default
#' @param filename_suffix Character. Suffix for output filenames (e.g., "full", "recent").
#'   Files saved as transectID_suffix.format
#' @param verbose Boolean. Print progress messages
#' @param annual_mode Boolean. If TRUE, generates annual progression plots (one set per year). Default FALSE.
#' @param name Character. Configuration name (for reference)
#' @returns List with class "plot_config"
#' @export
#'
#' @examples
#' # Using data_filter with year selector
#' config <- create_plot_config(
#'   data_filter = create_data_filter(year_selector = years_baseline_recent(2)),
#'   show_inset = TRUE
#' )
create_plot_config <- function(
    # Data filter
    data_filter = NULL,

    # Axis settings
    axis_limits = "park",
    custom_ylim = NULL,

    # AUC visualization options
    show_auc = TRUE,
    shade_area = TRUE,
    show_inset = TRUE,
    show_auc_text = TRUE,

    # Uncertainty visualization options
    show_uncertainty_bands = TRUE,
    show_auc_uncertainty = TRUE,
    show_point_uncertainty = FALSE,
    show_integration_boundaries = FALSE,
    uncertainty_alpha = 0.3,
    uncertainty_color = "gray70",

    # Theme and color options
    theme = "default",
    color_palette = "viridis",

    # Output format
    file_format = .DEFAULT_FILE_FORMAT,
    width = .DEFAULT_WIDTH,
    height = .DEFAULT_HEIGHT,
    dpi = .DEFAULT_DPI,
    output_dir = NULL,
    filename_suffix = "full",

    # Runtime options
    verbose = FALSE,
    annual_mode = FALSE,

    # Metadata
    name = "custom"
) {

  # Validate data_filter if provided
  if (!is.null(data_filter)) {
    if (!inherits(data_filter, "data_filter")) {
      stop("data_filter must be a data_filter object. Use create_data_filter() or filter_*() functions.",
           call. = FALSE)
    }
  }

  # Validate axis_limits
  valid_axis_limits <- c("park", "dataset", "none", "custom")
  if (!axis_limits %in% valid_axis_limits) {
    stop("axis_limits must be one of: ", paste(valid_axis_limits, collapse = ", "))
  }

  # Validate file_format
  valid_formats <- c("jpg", "png", "pdf")
  if (!file_format %in% valid_formats) {
    stop("file_format must be one of: ", paste(valid_formats, collapse = ", "))
  }

  # Validate theme
  valid_themes <- c("default", "publication", "presentation", "minimal", "dark")
  if (!theme %in% valid_themes) {
    warning("Unknown theme '", theme, "'. Using 'default'. ",
            "Valid options: ", paste(valid_themes, collapse = ", "),
            call. = FALSE)
    theme <- "default"
  }

  # Validate color_palette
  valid_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis", "turbo")
  if (!color_palette %in% valid_palettes) {
    warning("Unknown color_palette '", color_palette, "'. Using 'viridis'. ",
            "Valid options: ", paste(valid_palettes, collapse = ", "),
            call. = FALSE)
    color_palette <- "viridis"
  }

  # Validate custom_ylim if axis_limits is "custom"
  if (axis_limits == "custom" && is.null(custom_ylim)) {
    warning("axis_limits is 'custom' but no custom_ylim provided. Using park limits instead.")
    axis_limits <- "park"
  }

  # Create config object
  config <- structure(
    list(
      # Data filter
      data_filter = data_filter,

      # Axis settings
      axis_limits = axis_limits,
      custom_ylim = custom_ylim,

      # AUC visualization
      show_auc = show_auc,
      shade_area = shade_area,
      show_inset = show_inset,
      show_auc_text = show_auc_text,

      # Uncertainty visualization
      show_uncertainty_bands = show_uncertainty_bands,
      show_auc_uncertainty = show_auc_uncertainty,
      show_point_uncertainty = show_point_uncertainty,
      show_integration_boundaries = show_integration_boundaries,
      uncertainty_alpha = uncertainty_alpha,
      uncertainty_color = uncertainty_color,

      # Theme and color options
      theme = theme,
      color_palette = color_palette,

      # Output format
      file_format = file_format,
      width = width,
      height = height,
      dpi = dpi,
      output_dir = output_dir,
      filename_suffix = filename_suffix,

      # Runtime
      verbose = verbose,
      annual_mode = annual_mode,

      # Metadata
      name = name,
      created = Sys.time()
    ),
    class = "plot_config"
  )

  return(config)
}

# ============================================================================
# CONFIGURATION UTILITIES
# ============================================================================

#' Get plot configuration for a specific park
#'
#' Returns the default configuration for automated plotting.
#' Can be extended to support park-specific settings if needed.
#'
#' @param park Character. Park code (e.g., "ACAD", "BOHA")
#' @returns plot_config object
#' @export
get_park_config <- function(park) {
  config_automated_full()
}

#' Merge configuration with override parameters
#'
#' Takes a base configuration and updates specific parameters.
#' Useful for applying minor modifications to preset configs.
#'
#' @param config plot_config object. Base configuration
#' @param ... Named parameters to override
#' @returns Modified plot_config object
#' @export
modify_config <- function(config, ...) {
  if (!inherits(config, "plot_config")) {
    stop("config must be a plot_config object")
  }

  overrides <- list(...)

  # Update config with overrides (including data_filter)
  for (param_name in names(overrides)) {
    if (param_name %in% names(config) || param_name == "data_filter") {
      config[[param_name]] <- overrides[[param_name]]
    } else {
      warning("Unknown parameter '", param_name, "' ignored")
    }
  }

  config$created <- Sys.time()
  return(config)
}

#' Print configuration summary
#'
#' @param x plot_config object
#' @param ... Additional arguments (unused)
#' @export
print.plot_config <- function(x, ...) {
  cat("\n")
  cat("Plot Configuration: ", x$name, "\n")
  cat(strrep("=", 70), "\n\n")

  cat("Data Selection:\n")
  if (!is.null(x$data_filter)) {
    cat("  Using data_filter: ", x$data_filter$name, "\n")
    cat("  Year selector:     ", x$data_filter$year_selector$type, "\n")
    if (!is.null(x$data_filter$parks)) {
      cat("  Parks:             ", paste(x$data_filter$parks, collapse = ", "), "\n")
    } else {
      cat("  Parks:             All\n")
    }
  } else {
    cat("  No data_filter specified (will select all data)\n")
  }
  cat("\n")

  cat("Visualization:\n")
  cat("  Axis limits:   ", x$axis_limits, "\n")
  if (!is.null(x$custom_ylim)) {
    cat("  Custom Y-lim:  ", paste(x$custom_ylim, collapse = " to "), "\n")
  }
  cat("  Theme:         ", if (is.null(x$theme)) "default" else x$theme, "\n")
  cat("  Color palette: ", if (is.null(x$color_palette)) "viridis" else x$color_palette, "\n")
  cat("  Show AUC:      ", x$show_auc, "\n")
  cat("  Shade area:    ", x$shade_area, "\n")
  cat("  Show inset:    ", x$show_inset, "\n")
  cat("  Show AUC text: ", x$show_auc_text, "\n")
  cat("\n")

  cat("Uncertainty Visualization:\n")
  cat("  Elevation uncertainty bands:  ", x$show_uncertainty_bands, "\n")
  cat("  AUC uncertainty:              ", x$show_auc_uncertainty, "\n")
  cat("  Point uncertainty:            ", x$show_point_uncertainty, "\n")
  cat("  Integration boundaries:       ", x$show_integration_boundaries, "\n")
  cat("  Uncertainty alpha:            ", x$uncertainty_alpha, "\n")
  cat("  Uncertainty color:            ", x$uncertainty_color, "\n")
  cat("\n")

  cat("Output:\n")
  cat("  Format:        ", toupper(x$file_format), "\n")
  cat("  Dimensions:    ", x$width, "x", x$height, " inches\n")
  cat("  Resolution:    ", x$dpi, " dpi\n")
  cat("  Directory:     ", if (is.null(x$output_dir)) "project-specific" else x$output_dir, "\n")
  cat("  Filename:      <transectID>_", x$filename_suffix, ".", x$file_format, "\n", sep = "")
  cat("\n")

  cat("Runtime:\n")
  cat("  Verbose:      ", x$verbose, "\n")
  cat("  Annual mode:  ", x$annual_mode, "\n")
  if (x$annual_mode) {
    cat("    (Generates plots for each survey year with first + [target-1] + target)\n")
  }
  cat("  Created:      ", format(x$created, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("\n")

  invisible(x)
}

#' Save configuration to file
#'
#' @param config plot_config object
#' @param filepath Character. Path to save configuration (RDS format)
#' @export
save_config <- function(config, filepath) {
  if (!inherits(config, "plot_config")) {
    stop("config must be a plot_config object")
  }

  saveRDS(config, filepath)
  message("Configuration saved to: ", filepath)
  invisible(filepath)
}

#' Load configuration from file
#'
#' @param filepath Character. Path to configuration file (RDS format)
#' @returns plot_config object
#' @export
load_config <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("Configuration file not found: ", filepath)
  }

  config <- readRDS(filepath)

  if (!inherits(config, "plot_config")) {
    stop("File does not contain a valid plot_config object")
  }

  message("Configuration loaded: ", config$name)
  return(config)
}

# ============================================================================
# CONFIGURATION VALIDATION
# ============================================================================

#' Validate configuration against available data
#'
#' Checks that the data_filter can be resolved against the data and returns
#' valid selections. Returns a detailed validation report with issues and suggestions.
#'
#' @param config plot_config object
#' @param data Dataframe with transect data
#' @param verbose If TRUE (default), print formatted report. If FALSE, return silently.
#' @returns validation_report object with is_valid, issues, and details
#' @export
#'
#' @examples
#' \dontrun{
#' # Check config before running visualization
#' report <- validate_config(config, data)
#' if (!report$is_valid) {
#'   # Handle issues
#' }
#' }
validate_config <- function(config, data, verbose = TRUE) {
  if (!inherits(config, "plot_config")) {
    stop("config must be a plot_config object")
  }

  # Initialize report
  report <- list(
    is_valid = TRUE,
    issues = character(),
    warnings = character(),
    details = list(),
    summary = list()
  )

  # Get available data
  available_parks <- unique(data$park)
  available_transects <- unique(data$transect)
  available_years <- sort(unique(data$year))

  report$details$available <- list(
    parks = available_parks,
    transects = available_transects,
    years = available_years,
    n_parks = length(available_parks),
    n_transects = length(available_transects),
    n_years = length(available_years)
  )

  # Validate data_filter
  if (is.null(config$data_filter)) {
    # No filter means all data will be selected
    report$summary$selected_parks <- available_parks
    report$summary$selected_years <- available_years
    report$summary$selected_transects <- length(available_transects)
  } else {
    # Resolve data_filter and check
    resolved <- tryCatch({
      resolve_filter(config$data_filter, data)
    }, error = function(e) {
      report$issues <<- c(report$issues, paste("Error resolving data_filter:", e$message))
      report$is_valid <<- FALSE
      return(NULL)
    })

    if (!is.null(resolved)) {
      report$details$resolved <- resolved

      if (length(resolved$years) == 0) {
        report$issues <- c(report$issues, "No years selected by data_filter")
        report$is_valid <- FALSE
      }
      if (length(resolved$transects) == 0) {
        report$issues <- c(report$issues, "No transects selected by data_filter")
        report$is_valid <- FALSE
      }
      if (length(resolved$parks) == 0) {
        report$issues <- c(report$issues, "No parks selected by data_filter")
        report$is_valid <- FALSE
      }

      report$summary$selected_parks <- resolved$parks
      report$summary$selected_years <- resolved$years
      report$summary$selected_transects <- length(resolved$transects)
    }
  }

  # Check for potential issues (warnings, not errors)
  if (!is.null(config$theme) && config$theme == "dark" && config$file_format == "pdf") {
    report$warnings <- c(report$warnings,
                         "Dark theme with PDF format may not print well")
  }

  if (config$dpi < 150 && config$file_format == "pdf") {
    report$warnings <- c(report$warnings,
                         "Low DPI (< 150) for PDF may produce poor quality output")
  }

  # Add class for pretty printing
  class(report) <- c("validation_report", "list")

  # Print report if verbose
  if (verbose) {
    print(report)
  }

  return(invisible(report))
}

#' Print validation report
#'
#' @param x validation_report object
#' @param ... Additional arguments (unused)
#' @export
print.validation_report <- function(x, ...) {
  cat("\n")
  cat("Configuration Validation Report\n")
  cat(strrep("=", 60), "\n\n")

  # Status
  if (x$is_valid) {
    cat("Status: VALID\n\n")
  } else {
    cat("Status: INVALID\n\n")
  }

  # Issues
  if (length(x$issues) > 0) {
    cat("Issues:\n")
    for (issue in x$issues) {
      cat("  [X] ", issue, "\n")
    }
    cat("\n")
  }

  # Warnings
  if (length(x$warnings) > 0) {
    cat("Warnings:\n")
    for (warning in x$warnings) {
      cat("  [!] ", warning, "\n")
    }
    cat("\n")
  }

  # Summary
  cat("Selection Summary:\n")
  cat("  Parks:     ", paste(x$summary$selected_parks, collapse = ", "), "\n")
  cat("  Years:     ", paste(x$summary$selected_years, collapse = ", "), "\n")
  cat("  Transects: ", x$summary$selected_transects, "\n\n")

  # Available data
  cat("Available Data:\n")
  cat("  Parks:     ", x$details$available$n_parks, " (",
      paste(x$details$available$parks, collapse = ", "), ")\n", sep = "")
  cat("  Years:     ", x$details$available$n_years, " (",
      paste(range(x$details$available$years), collapse = "-"), ")\n", sep = "")
  cat("  Transects: ", x$details$available$n_transects, "\n\n")

  invisible(x)
}

# ============================================================================
# RESOLVE CONFIG DATA FILTER
# ============================================================================

#' Resolve configuration data selection against actual data
#'
#' Resolves the config's data_filter to get actual years/parks/transects.
#' If no data_filter is present, returns NULL for all selections (indicating all data).
#'
#' @param config plot_config object
#' @param data Dataframe with transect data
#' @returns List with resolved parks, years, transects
#' @export
resolve_config_filter <- function(config, data) {
  if (!inherits(config, "plot_config")) {
    stop("config must be a plot_config object", call. = FALSE)
  }

  # If data_filter is present, use it
  if (!is.null(config$data_filter)) {
    resolved <- resolve_filter(config$data_filter, data)
    return(list(
      parks = resolved$parks,
      years = resolved$years,
      transects = resolved$transects,
      park_year_details = resolved$park_year_details
    ))
  }

  # No data_filter means select all data
  return(list(
    parks = NULL,
    years = NULL,
    transects = NULL,
    park_year_details = NULL
  ))
}
