# plot_transect.R
# Core transect plotting engine
#
# This module provides the primary plotting interface for beach profile transects.
# Users interact with plot_transect_profiles() using config objects from plot_config.R.
# Internal helpers handle uncertainty visualization, AUC insets, and plot assembly.

#' @importFrom ggplot2 ggplot aes geom_path geom_point geom_area geom_ribbon
#'   geom_vline geom_errorbar geom_bar geom_label annotate
#'   labs xlab ylab ylim xlim theme theme_minimal element_blank element_text
#'   element_rect scale_fill_manual scale_y_continuous expansion
#' @importFrom patchwork inset_element
#' @importFrom dplyr filter pull mutate arrange first
#' @importFrom grid unit
#' @importFrom grDevices rainbow

# ============================================================================
# CONSTANTS
# ============================================================================

# Internal constants for plot defaults
# NOTE: These values also appear in add_common_min_line() in plot_legend.R
# as magic numbers. Keep them in sync if modified.
.GGPLOT_DEFAULT_EXPANSION <- 0.05     # ggplot default axis expansion (5%)
.DEFAULT_Y_MIN <- -1                  # Default y-axis minimum
.DEFAULT_Y_MAX <- 11                  # Default y-axis maximum

# ============================================================================
# INTERNAL HELPERS
# ============================================================================

#' Add elevation uncertainty bands to plot
#'
#' Adds geom_ribbon layers showing uncertainty in elevation measurements
#' using elev_upper and elev_lower bounds. Only shown if columns are present.
#'
#' @param p ggplot object
#' @param filtered_data Dataframe with transect data (must have elev_upper, elev_lower)
#' @param years Numeric vector of years
#' @param colors Named vector of colors (for multi-year)
#' @param config plot_config object with uncertainty settings
#' @return Modified ggplot object with uncertainty bands added
#'
#' @keywords internal
#' @noRd
add_uncertainty_bands <- function(p, filtered_data, years, colors = NULL, config = NULL) {

  # Check if uncertainty columns are present
  if (!all(c("elev_upper", "elev_lower") %in% names(filtered_data))) {
    return(p)  # Skip if uncertainty data not available
  }

  # Check if uncertainty bands should be shown
  if (!is.null(config) && !config$show_uncertainty_bands) {
    return(p)
  }

  # Get uncertainty visualization settings
  uncertainty_alpha <- if (!is.null(config)) config$uncertainty_alpha else 0.3
  uncertainty_color <- if (!is.null(config)) config$uncertainty_color else "gray70"

  if (length(years) == 1) {
    # Single year: one uncertainty band
    p <- p +
      geom_ribbon(
        aes(ymin = elev_lower, ymax = elev_upper),
        alpha = uncertainty_alpha,
        fill = uncertainty_color,
        color = NA
      )
  } else {
    # Multiple years: color-coded uncertainty bands
    for (i in seq_along(years)) {
      yr <- years[i]
      year_data <- filtered_data |> filter(year == yr)

      # Use lighter version of year color for uncertainty
      year_color <- if (!is.null(colors)) colors[as.character(yr)] else uncertainty_color

      p <- p +
        geom_ribbon(
          data = year_data,
          aes(ymin = elev_lower, ymax = elev_upper, group = year),
          alpha = uncertainty_alpha * 0.5,  # More transparent for multi-year
          fill = year_color,
          color = NA
        )
    }
  }

  return(p)
}

#' Add shading layers to plot for all segments
#'
#' @param p ggplot object
#' @param filtered_data Dataframe with transect data
#' @param transect_num Character. Transect ID
#' @param years Numeric vector of years
#' @param auc_results Dataframe with AUC results
#' @param colors Named vector of colors (for multi-year)
#' @return Modified ggplot object with shading layers added
#'
#' @keywords internal
#' @noRd
add_shading_layers <- function(p, filtered_data, transect_num, years, auc_results, colors = NULL) {
  if(length(years) == 1) {
    # Single year: shade all segments
    auc_filtered <- auc_results |>
      filter(transect == transect_num, year == years)

    if (nrow(auc_filtered) == 0) {
      warning("No AUC results found for transect ", transect_num, " year ", years)
      return(p)
    }

    year_segments <- auc_filtered |>
      pull(segments) |>
      first()

    if (is.null(year_segments)) {
      warning("Segments column is NULL for transect ", transect_num, " year ", years)
      return(p)
    }

    shade_data_list <- create_shade_data(year_segments)

    if(!is.null(shade_data_list)) {
      for(shade_data in shade_data_list) {
        if (nrow(shade_data) >= 2) {  # Need at least 2 points for geom_area
          p <- p +
            geom_area(data = shade_data,
                      alpha = 0.3, fill = "lightblue")
        }
      }
    }
  } else {
    # Multiple years: shade with different colors
    for(i in seq_along(years)) {
      yr <- years[i]
      auc_filtered <- auc_results |>
        filter(transect == transect_num, year == yr)

      if (nrow(auc_filtered) == 0) {
        warning("No AUC results found for transect ", transect_num, " year ", yr)
        next
      }

      year_segments <- auc_filtered |>
        pull(segments) |>
        first()

      if (is.null(year_segments)) {
        warning("Segments column is NULL for transect ", transect_num, " year ", yr)
        next
      }

      shade_data_list <- create_shade_data(year_segments, year = yr)

      if(!is.null(shade_data_list)) {
        for(shade_data in shade_data_list) {
          if (nrow(shade_data) >= 2) {  # Need at least 2 points for geom_area
            p <- p +
              geom_area(data = shade_data,
                        alpha = 0.2,
                        fill = colors[as.character(yr)])
          }
        }
      }
    }
  }

  return(p)
}

#' Add integration boundary vertical lines to plot
#'
#' Shows vertical lines at AUC integration boundaries (segment min/max distances).
#' This visualizes where each positive segment begins and ends for AUC calculation.
#'
#' @param p ggplot object
#' @param transect_num Character. Transect ID
#' @param years Numeric vector of years
#' @param auc_results Dataframe with AUC results (must include segment_info)
#' @return Modified ggplot object with integration boundary vlines added
#'
#' @keywords internal
#' @noRd
add_integration_boundaries <- function(p, transect_num, years, auc_results) {

  # Filter AUC results for this transect and years
  auc_filtered <- auc_results |>
    filter(transect == transect_num, year %in% years)

  if (nrow(auc_filtered) == 0 || !"segment_info" %in% names(auc_filtered)) {
    return(p)
  }

  # Extract all unique boundary distances from all years' segment_info
  all_boundaries <- unique(unlist(lapply(seq_len(nrow(auc_filtered)), function(i) {
    seg_info <- auc_filtered$segment_info[[i]]
    if (is.null(seg_info) || !is.data.frame(seg_info)) {
      return(NULL)
    }
    # Get distance_min and distance_max from each segment
    c(seg_info$distance_min, seg_info$distance_max)
  })))

  # Remove NAs and sort
  all_boundaries <- sort(unique(all_boundaries[!is.na(all_boundaries)]))

  if (length(all_boundaries) == 0) {
    return(p)
  }

  # Add vlines for all integration boundaries
  p <- p +
    geom_vline(xintercept = all_boundaries,
               linetype = "dotted",
               color = "blue",
               alpha = 0.5,
               linewidth = 0.6)

  return(p)
}

#' Add AUC inset bar chart to plot
#'
#' @param p ggplot object
#' @param filtered_data Dataframe with transect data
#' @param transect_num Character. Transect ID
#' @param years Numeric vector of years
#' @param auc_results Dataframe with AUC results
#' @param colors Named vector of colors
#' @param xlim_range Numeric vector of x-axis limits
#' @param ylim_range Numeric vector of y-axis limits
#' @param common_min_dist Double. Common minimum distance
#' @param show_uncertainty Boolean. Show AUC uncertainty as error bars. Default TRUE.
#' @return Modified ggplot object with inset added
#'
#' @keywords internal
#' @noRd
add_auc_inset <- function(p, filtered_data, transect_num, years, auc_results, colors,
                          xlim_range, ylim_range, common_min_dist, show_uncertainty = TRUE) {
  auc_info <- auc_results |>
    filter(transect == transect_num, year %in% years) |>
    arrange(year)

  if(nrow(auc_info) == 0) {
    return(p)
  }

  # Create bar chart data
  bar_data <- auc_info |>
    mutate(
      year_factor = as.factor(year),
      auc_clean = ifelse(is.na(auc), 0, auc)
    )

  # Check if uncertainty columns are available
  has_uncertainty <- all(c("auc_upper", "auc_lower") %in% names(auc_info))

  # Create inset bar chart
  inset_plot <- ggplot(bar_data, aes(x = year_factor, y = auc_clean, fill = year_factor)) +
    geom_bar(stat = "identity", alpha = 0.8, width = 0.9)

  # Add error bars if uncertainty is available and requested
  if (show_uncertainty && has_uncertainty) {
    inset_plot <- inset_plot +
      geom_errorbar(
        aes(ymin = ifelse(is.na(auc_lower), auc_clean, auc_lower),
            ymax = ifelse(is.na(auc_upper), auc_clean, auc_upper)),
        width = 0.3,
        linewidth = 0.6,
        color = "black",
        alpha = 0.7
      )
  }

  inset_plot <- inset_plot +
    scale_fill_manual(values = colors) +
    labs(x = "Year", y = expression("Area [m"^2*"]")) +
    ggplot2::theme_minimal() +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      panel.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))  # Extra space for error bars

  # Determine actual axis limits
  actual_xlim <- if(!is.null(xlim_range)) {
    xlim_range
  } else {
    x_range <- range(filtered_data$distance, na.rm = TRUE)
    x_padding <- diff(x_range) * .GGPLOT_DEFAULT_EXPANSION
    c(x_range[1] - x_padding, x_range[2] + x_padding)
  }

  actual_ylim <- if(!is.null(ylim_range)) {
    ylim_range
  } else {
    c(.DEFAULT_Y_MIN, .DEFAULT_Y_MAX)
  }

  # Find optimal position and adjust width
  inset_position <- find_best_inset_position(
    data = filtered_data,
    xlim = actual_xlim,
    ylim = actual_ylim,
    common_min = common_min_dist
  )

  inset_position <- adjust_inset_width(
    position = inset_position,
    n_years = length(years),
    corner = inset_position$corner
  )

  # Add inset to main plot
  p <- p +
    inset_element(inset_plot,
                  left = inset_position$left,
                  bottom = inset_position$bottom,
                  right = inset_position$right,
                  top = inset_position$top,
                  align_to = "plot")

  return(p)
}

#' Add AUC text annotation for single year plots
#'
#' @param p ggplot object
#' @param filtered_data Dataframe with transect data
#' @param transect_num Character. Transect ID
#' @param years Numeric vector of years (should be length 1)
#' @param auc_results Dataframe with AUC results
#' @param xlim_range Numeric vector of x-axis limits
#' @param ylim_range Numeric vector of y-axis limits
#' @param show_uncertainty Boolean. Show AUC uncertainty. Default TRUE.
#' @return Modified ggplot object with text annotation added
#'
#' @keywords internal
#' @noRd
add_auc_text_annotation <- function(p, filtered_data, transect_num, years, auc_results,
                                    xlim_range, ylim_range, show_uncertainty = TRUE) {
  auc_info <- auc_results |>
    filter(transect == transect_num, year %in% years)

  if(nrow(auc_info) == 0) {
    return(p)
  }

  auc_value <- auc_info$auc

  # Build AUC text with or without uncertainty
  if (is.na(auc_value)) {
    auc_text <- "AUC: N/A"
  } else if (show_uncertainty && "auc_sigma" %in% names(auc_info) && !is.na(auc_info$auc_sigma)) {
    # Show uncertainty if available and requested
    auc_sigma <- auc_info$auc_sigma
    auc_text <- paste0("AUC: ", round(auc_value, 2), " \u00B1 ", round(auc_sigma, 2), " m\u00B2")
  } else if (show_uncertainty && all(c("auc_upper", "auc_lower") %in% names(auc_info)) &&
             !is.na(auc_info$auc_upper) && !is.na(auc_info$auc_lower)) {
    # Fallback: show confidence interval
    auc_upper <- auc_info$auc_upper
    auc_lower <- auc_info$auc_lower
    auc_text <- paste0("AUC: ", round(auc_value, 2), " m\u00B2\n",
                       "[", round(auc_lower, 2), ", ", round(auc_upper, 2), "]")
  } else {
    # No uncertainty available or not requested
    auc_text <- paste0("AUC: ", round(auc_value, 2), " m\u00B2")
  }

  # Calculate actual x-axis limits
  if(!is.null(xlim_range)) {
    actual_xlim <- xlim_range
  } else {
    x_range <- range(filtered_data$distance, na.rm = TRUE)
    x_padding <- diff(x_range) * .GGPLOT_DEFAULT_EXPANSION
    actual_xlim <- c(x_range[1] - x_padding, x_range[2] + x_padding)
  }

  # Calculate actual y-axis limits
  if(!is.null(ylim_range)) {
    actual_ylim <- ylim_range
  } else {
    actual_ylim <- c(.DEFAULT_Y_MIN, .DEFAULT_Y_MAX)
  }

  # Position label in upper right corner, right-aligned within plot bounds
  # Small padding from edge (2% of range)
  x_padding_pct <- 0.02
  y_padding_pct <- 0.05
  x_pos <- actual_xlim[2] - (diff(actual_xlim) * x_padding_pct)
  y_pos <- actual_ylim[2] - (diff(actual_ylim) * y_padding_pct)

  # Create data frame for label positioning
  label_df <- data.frame(
    x = x_pos,
    y = y_pos,
    label = auc_text
  )

  p <- p +
    geom_label(
      data = label_df,
      aes(x = x, y = y, label = label),
      hjust = 1,           # Right-align: text ends at x_pos
      vjust = 1,           # Top-align: text top at y_pos
      size = 3.5,
      fontface = "bold",
      color = "darkblue",
      fill = "white",
      linewidth = 0.5,     # Border thickness (ggplot2 >= 3.5.0)
      label.r = unit(0.15, "lines"),  # Rounded corners
      label.padding = unit(0.25, "lines"),
      inherit.aes = FALSE  # Don't inherit aesthetics from main plot
    )

  return(p)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Validate plot inputs
#'
#' Validates that all required inputs are present and correctly formatted
#' for transect plotting functions.
#'
#' @param data Dataframe with transect data
#' @param auc_results Dataframe with AUC results
#' @param config plot_config object
#' @keywords internal
#' @noRd
validate_plot_inputs <- function(data, auc_results, config) {
  # Validate data
  if (missing(data) || is.null(data)) {
    stop("data is required and cannot be NULL", call. = FALSE)
  }

  if (nrow(data) == 0) {
    stop("data cannot be empty", call. = FALSE)
  }

  required_data_cols <- c("transect", "year", "park", "distance", "elevation")
  missing_data_cols <- setdiff(required_data_cols, names(data))
  if (length(missing_data_cols) > 0) {
    stop("data is missing required columns: ", paste(missing_data_cols, collapse = ", "),
         call. = FALSE)
  }

  # Validate auc_results
  if (missing(auc_results) || is.null(auc_results)) {
    stop("auc_results is required and cannot be NULL", call. = FALSE)
  }

  if (nrow(auc_results) == 0) {
    stop("auc_results cannot be empty", call. = FALSE)
  }

  required_auc_cols <- c("transect", "year", "auc", "segments")
  missing_auc_cols <- setdiff(required_auc_cols, names(auc_results))
  if (length(missing_auc_cols) > 0) {
    stop("auc_results is missing required columns: ", paste(missing_auc_cols, collapse = ", "),
         call. = FALSE)
  }

  # Validate config
  if (missing(config) || is.null(config)) {
    stop("config is required and cannot be NULL", call. = FALSE)
  }

  if (!inherits(config, "plot_config")) {
    stop("config must be a plot_config object. Use create_plot_config() or a preset function.",
         call. = FALSE)
  }

  # Validate data contains exactly one transect
  transect_id <- unique(data$transect)
  if (length(transect_id) != 1) {
    stop("data must contain exactly one transect. Found: ",
         paste(transect_id, collapse = ", "),
         call. = FALSE)
  }

  # Validate years exist
  years <- unique(data$year)
  if (length(years) == 0) {
    stop("No years found in data for transect ", transect_id, call. = FALSE)
  }

  invisible(TRUE)
}

#' Resolve y-axis limits from config
#'
#' Determines y-axis limits based on config settings and data.
#'
#' @param config plot_config object
#' @param data Dataframe with transect data
#' @param park Character. Park code
#' @return Numeric vector of y-limits c(min, max), or NULL for auto-scale
#' @keywords internal
#' @noRd
resolve_ylim_from_config <- function(config, data, park) {
  ylim_to_use <- NULL

  if (config$axis_limits == "park") {
    # Calculate park-specific limits
    park_limits <- tryCatch({
      calculate_axis_limits(data, park)
    }, error = function(e) {
      warning("Failed to calculate park-specific limits: ", e$message,
              ". Using auto-scale instead.", call. = FALSE)
      list(ylim = NULL)
    })
    ylim_to_use <- park_limits$ylim

  } else if (config$axis_limits == "dataset") {
    # Use dataset-wide limits (requires all data, but we only have filtered)
    # Fall back to park limits in this case
    park_limits <- tryCatch({
      calculate_axis_limits(data, park)
    }, error = function(e) {
      warning("Failed to calculate dataset limits: ", e$message,
              ". Using auto-scale instead.", call. = FALSE)
      list(ylim = NULL)
    })
    ylim_to_use <- park_limits$ylim

  } else if (config$axis_limits == "custom" && !is.null(config$custom_ylim)) {
    ylim_to_use <- config$custom_ylim
  }
  # If axis_limits == "none", ylim_to_use remains NULL (auto-scale)

  return(ylim_to_use)
}

#' Extract common minimum distance from data
#'
#' Gets the common minimum distance value from point_type column.
#'
#' @param data Dataframe with transect data (must have point_type and distance columns)
#' @return Numeric common minimum distance, or NA if not found
#' @keywords internal
#' @noRd
extract_common_min_distance <- function(data) {
  if (!"point_type" %in% names(data)) {
    return(NA_real_)
  }

  common_min_dist <- data |>
    filter(point_type == "common_min") |>
    pull(distance) |>
    first()

  if (length(common_min_dist) == 0 || is.na(common_min_dist)) {
    return(NA_real_)
  }

  return(common_min_dist)
}

# ============================================================================
# CORE PLOTTING ENGINE
# ============================================================================

#' Plot beach profile transect
#'
#' Creates a ggplot visualization of a transect elevation profile with optional
#' AUC visualization, uncertainty bands, and multi-year comparisons. This is
#' the main plotting function in GULNcoastal.
#'
#' Uses a config-based approach where all visualization settings are specified
#' through a \code{plot_config} object. Data must be pre-filtered to contain
#' exactly one transect.
#'
#' @param data Dataframe with transect profile data for a **single transect**.
#'   Required columns: \code{transect}, \code{year}, \code{park}, \code{distance},
#'   \code{elevation}. Optional columns: \code{point_type}, \code{elev_upper},
#'   \code{elev_lower}.
#' @param auc_results Dataframe with AUC results for the transect.
#'   Required columns: \code{transect}, \code{year}, \code{auc}, \code{segments}.
#'   Optional columns: \code{auc_upper}, \code{auc_lower}, \code{auc_sigma},
#'   \code{segment_info}.
#' @param config A \code{plot_config} object controlling all visualization options.
#'   Create using \code{\link{create_plot_config}} or use a preset like
#'   \code{\link{config_automated_full}}.
#'
#' @return A ggplot object that can be further modified, saved with
#'   \code{\link[ggplot2]{ggsave}}, or printed directly.
#'
#' @section Configuration Presets:
#' GULNcoastal provides several presets for common use cases:
#' \describe{
#'   \item{\code{\link{config_quick}}}{Fast exploration with minimal options}
#'   \item{\code{\link{config_temporal}}}{Baseline vs. recent comparison}
#'   \item{\code{\link{config_annual}}}{Annual progression plots}
#'   \item{\code{\link{config_automated_full}}}{Full analysis with all features}
#' }
#'
#' @seealso
#' Configuration: \code{\link{create_plot_config}}, \code{\link{modify_config}}
#'
#' Presets: \code{\link{config_quick}}, \code{\link{config_temporal}},
#' \code{\link{config_annual}}, \code{\link{config_automated_full}}
#'
#' Data filtering: \code{\link{create_data_filter}}, \code{\link{years_baseline_recent}},
#' \code{\link{years_recent}}
#'
#' @examples
#' \dontrun{
#' # Basic usage with preset config
#' p <- plot_transect(
#'   data = transect_data,
#'   auc_results = auc_data,
#'   config = config_automated_full()
#' )
#'
#' # Custom configuration
#' config <- create_plot_config(
#'   data_filter = create_data_filter(year_selector = years_recent(3)),
#'   show_uncertainty_bands = TRUE,
#'   axis_limits = "park"
#' )
#' p <- plot_transect(transect_data, auc_data, config)
#'
#' # Save the plot
#' ggplot2::ggsave("transect_plot.png", p, width = 12, height = 8, dpi = 300)
#' }
#'
#' @export
plot_transect <- function(data, auc_results, config) {

  # Validate inputs
  validate_plot_inputs(data, auc_results, config)

  # Extract metadata from data (data is already filtered to single transect)
  transect_id <- unique(data$transect)
  years <- sort(unique(data$year))
  park <- unique(data$park) |> first()

  # Get color palette using config setting
  color_palette <- tryCatch({
    get_color_palette(data, palette = config$color_palette)
  }, error = function(e) {
    stop("Failed to generate color palette: ", e$message, call. = FALSE)
  })

  # Resolve y-axis limits from config
  ylim_range <- resolve_ylim_from_config(config, data, park)

  # Extract common minimum distance
  common_min_dist <- extract_common_min_distance(data)

  # Track legend state for unified legend configuration
  has_common_min <- !is.na(common_min_dist)
  has_interp <- nrow(data |> filter(point_type == "interpolated")) > 0
  has_extrap <- nrow(data |> filter(point_type == "extrapolated")) > 0
  is_multiyear <- length(years) > 1

  # Create title
  title_text <- paste(park, "Transect", transect_id)
  if (length(years) == 1) {
    subtitle_text <- as.character(years)
  } else {
    subtitle_text <- format_year_sequence(years, park)
  }

  # Create base plot
  p <- ggplot(data) +
    aes(x = distance, y = elevation) +
    ylab("Elevation [m]") +
    xlab("Distance along transect [m]") +
    labs(title = title_text, subtitle = subtitle_text) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )

  # Apply y-axis limits
  if (!is.null(ylim_range)) {
    p <- p + ylim(ylim_range[1], ylim_range[2])
  } else {
    p <- p + ylim(.DEFAULT_Y_MIN, .DEFAULT_Y_MAX)
  }

  # x-axis: always auto-scale per transect
  # (xlim_range parameter removed - not used in config system)

  # Get colors for this transect's years
  colors <- color_palette[as.character(years)]

  # Add shading if requested
  if (config$shade_area && !is.null(auc_results)) {
    p <- tryCatch({
      add_shading_layers(p, data, transect_id, years, auc_results, colors)
    }, error = function(e) {
      warning("Failed to add shading layers for transect ", transect_id,
              ": ", e$message, ". Continuing without shading.", call. = FALSE)
      p  # Return plot without shading
    })
  }

  # Add elevation uncertainty bands
  if (config$show_uncertainty_bands) {
    p <- tryCatch({
      add_uncertainty_bands(p, data, years, colors, config)
    }, error = function(e) {
      if (config$verbose) {
        warning("Failed to add uncertainty bands for transect ", transect_id,
                ": ", e$message, ". Continuing without uncertainty visualization.",
                call. = FALSE)
      }
      p  # Return plot without uncertainty bands
    })
  }

  # Add line and points
  p <- p + geom_path(linewidth = 1)

  if (is_multiyear) {
    # Multi-year: add color aesthetic for year grouping
    p <- p + aes(color = as.factor(year), group = year)
  }

  # Add measured/common_min/measured_zero points (no legend)
  if (is_multiyear) {
    # Multi-year: inherit color from aes(color = year)
    p <- p +
      geom_point(
        data = data |> filter(point_type %in% c("measured", "common_min", "measured_zero")),
        alpha = 0.7,
        size = 1.2,
        shape = 16,
        show.legend = FALSE
      )
  } else {
    # Single-year: use explicit year color
    year_color <- colors[as.character(years)]
    p <- p +
      geom_point(
        data = data |> filter(point_type %in% c("measured", "common_min", "measured_zero")),
        alpha = 0.6,
        size = 1.5,
        shape = 16,
        color = year_color,
        show.legend = FALSE
      )
  }

  # Add interpolated points (green, with shape aesthetic for legend)
  if (has_interp) {
    p <- p +
      geom_point(
        data = data |> filter(point_type == "interpolated"),
        aes(shape = "Interpolated"),
        alpha = if (is_multiyear) 0.7 else 0.6,
        size = if (is_multiyear) 1.2 else 1.5,
        color = "green",
        show.legend = TRUE
      )
  }

  # Add extrapolated points (red, with shape aesthetic for legend)
  if (has_extrap) {
    p <- p +
      geom_point(
        data = data |> filter(point_type == "extrapolated"),
        aes(shape = "Extrapolated"),
        alpha = if (is_multiyear) 0.7 else 0.6,
        size = if (is_multiyear) 1.2 else 1.5,
        color = "red",
        show.legend = TRUE
      )
  }

  # Add common minimum vline
  p <- add_common_min_line(p, common_min_dist, data,
                                    xlim_range = NULL, ylim_range)

  # Configure all legends in unified system
  p <- configure_plot_legends(
    p = p,
    has_common_min = has_common_min,
    has_interp = has_interp,
    has_extrap = has_extrap,
    is_multiyear = is_multiyear,
    colors = colors,
    years = years
  )

  # Add integration boundaries if requested
  if (config$show_integration_boundaries && !is.null(auc_results)) {
    p <- tryCatch({
      add_integration_boundaries(p, transect_id, years, auc_results)
    }, error = function(e) {
      if (config$verbose) {
        warning("Failed to add integration boundaries for transect ", transect_id,
                ": ", e$message, ". Continuing without integration boundaries.",
                call. = FALSE)
      }
      p  # Return plot without integration boundaries
    })
  }

  # Add AUC visualization
  if (config$show_auc && !is.null(auc_results)) {
    if (length(years) > 1) {
      p <- add_auc_inset(p, data, transect_id, years, auc_results, colors,
                         xlim_range = NULL, ylim_range, common_min_dist,
                         show_uncertainty = config$show_auc_uncertainty)
    } else {
      p <- add_auc_text_annotation(p, data, transect_id, years, auc_results,
                                   xlim_range = NULL, ylim_range,
                                   show_uncertainty = config$show_auc_uncertainty)
    }
  }

  return(p)
}

