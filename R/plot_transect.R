# R/plot_transect.R
# Core transect plotting engine
#
# This module provides the primary plotting interface for beach profile transects.
# Users interact with plot_transect_profiles() using config objects from plot_config.R.
# Internal helpers handle uncertainty visualization, AUC insets, and plot assembly.

#' @importFrom ggplot2 ggplot aes geom_path geom_point geom_area geom_ribbon
#'   geom_vline geom_errorbar geom_bar geom_label annotate
#'   labs xlab ylab ylim xlim theme element_text element_rect scale_fill_manual
#'   scale_y_continuous expansion
#' @importFrom patchwork inset_element
#' @importFrom dplyr filter pull mutate arrange first

# ============================================================================
# CONSTANTS
# ============================================================================

# Internal constants for plot defaults
# NOTE: These values also appear in add_common_min_vline_unified() in plot_legend.R
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
      year_data <- filtered_data %>% filter(year == yr)

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
    auc_filtered <- auc_results %>%
      filter(transect == transect_num, year == years)

    if (nrow(auc_filtered) == 0) {
      warning("No AUC results found for transect ", transect_num, " year ", years)
      return(p)
    }

    year_segments <- auc_filtered %>%
      pull(segments) %>%
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
      auc_filtered <- auc_results %>%
        filter(transect == transect_num, year == yr)

      if (nrow(auc_filtered) == 0) {
        warning("No AUC results found for transect ", transect_num, " year ", yr)
        next
      }

      year_segments <- auc_filtered %>%
        pull(segments) %>%
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
  auc_filtered <- auc_results %>%
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
  auc_info <- auc_results %>%
    filter(transect == transect_num, year %in% years) %>%
    arrange(year)

  if(nrow(auc_info) == 0) {
    return(p)
  }

  # Create bar chart data
  bar_data <- auc_info %>%
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
    theme_minimal() +
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
  auc_info <- auc_results %>%
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
# CORE PLOTTING ENGINE (INTERNAL)
# ============================================================================

#' Plot beach profile transect for one or more years
#'
#' Internal core plotting function. Users should call [plot_transect_profiles()]
#' with a config object instead.
#'
#' @param data Dataframe with transect data.
#' @param transect_num Character. Transect ID to plot.
#' @param years Numeric vector. Years to include (NULL = all available).
#' @param auc_results Dataframe with AUC results.
#' @param show_auc Logical. Include AUC visualization.
#' @param shade_area Logical. Shade AUC areas.
#' @param color_palette Named character vector. Colors for each year.
#' @param xlim_range Numeric vector. X-axis limits.
#' @param ylim_range Numeric vector. Y-axis limits.
#' @param show_uncertainty_bands Logical. Show elevation uncertainty ribbons.
#' @param show_auc_uncertainty Logical. Show AUC confidence intervals.
#' @param show_integration_boundaries Logical. Show integration boundary lines.
#' @param uncertainty_alpha Numeric. Transparency for uncertainty bands.
#' @param uncertainty_color Character. Color for uncertainty visualization.
#' @param verbose Logical. Print progress messages.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_transect_year <- function(data, transect_num = NULL, years = NULL, auc_results = NULL,
                               show_auc = TRUE, shade_area = TRUE, color_palette = NULL,
                               xlim_range = NULL, ylim_range = NULL,
                               show_uncertainty_bands = TRUE,
                               show_auc_uncertainty = TRUE,
                               show_integration_boundaries = FALSE,
                               uncertainty_alpha = 0.3,
                               uncertainty_color = "gray70",
                               verbose = FALSE) {

  # Validate inputs
  available_transects <- data %>% pull(transect) %>% unique() %>% sort()

  if(is.null(transect_num)) {
    stop(paste("No transect number provided. Available transects:", paste(available_transects, collapse = ", ")))
  }

  if(!transect_num %in% available_transects) {
    stop(paste("No transect with number", transect_num, "found. Available transects:", paste(available_transects, collapse = ", ")))
  }

  # Get all years if not specified
  if(is.null(years)) {
    available_years <- data %>%
      filter(transect == transect_num) %>%
      pull(year) %>%
      unique() %>%
      sort()
    years <- available_years
  }

  # Filter data
  filtered_data <- data %>%
    filter(transect == transect_num, year %in% years)

  if(nrow(filtered_data) == 0) {
    stop(paste("No data found for transect", transect_num, "in years:", paste(years, collapse = ", ")))
  }

  # Get metadata
  park_name <- filtered_data %>% pull(park) %>% first()

  # Extract common_min distance from point_type column
  common_min_dist <- filtered_data %>%
    filter(transect == transect_num, point_type == "common_min") %>%
    pull(distance) %>%
    first()
  if(length(common_min_dist) == 0 || is.na(common_min_dist)) {
    common_min_dist <- NA
  }

  # Track legend state for unified legend configuration
  has_common_min <- !is.na(common_min_dist)
  has_interp <- nrow(filtered_data %>% filter(point_type == "interpolated")) > 0
  has_extrap <- nrow(filtered_data %>% filter(point_type == "extrapolated")) > 0
  is_multiyear <- length(years) > 1

  # Create title
  title_text <- paste(park_name, "Transect", transect_num)
  if(length(years) == 1) {
    subtitle_text <- as.character(years)
  } else {
    subtitle_text <- format_year_sequence(years, park_name)
  }

  # Create base plot
  p <- ggplot(filtered_data) +
    aes(x = distance, y = elevation) +
    ylab("Elevation [m]") +
    xlab("Distance along transect [m]") +
    labs(title = title_text, subtitle = subtitle_text) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )

  # Apply axis limits
  if(!is.null(ylim_range)) {
    p <- p + ylim(ylim_range[1], ylim_range[2])
  } else {
    p <- p + ylim(.DEFAULT_Y_MIN, .DEFAULT_Y_MAX)
  }

  if(!is.null(xlim_range)) {
    p <- p + xlim(xlim_range[1], xlim_range[2])
  }

  # Get or generate color palette
  colors <- NULL
  if(length(years) >= 1) {
    if(!is.null(color_palette)) {
      colors <- color_palette[as.character(years)]
    } else {
      if(length(years) == 1) {
        colors <- "#440154FF"  # Viridis dark purple (default single color)
        names(colors) <- as.character(years)
      } else {
        colors <- rainbow(length(years))
        names(colors) <- as.character(years)
      }
    }
  }

  # Add shading if requested
  if(shade_area && !is.null(auc_results)) {
    p <- tryCatch({
      add_shading_layers(p, filtered_data, transect_num, years, auc_results, colors)
    }, error = function(e) {
      warning("Failed to add shading layers for transect ", transect_num,
              ": ", e$message, ". Continuing without shading.", call. = FALSE)
      p  # Return plot without shading
    })
  }

  # Add elevation uncertainty bands
  if(show_uncertainty_bands) {
    # Create minimal config object for uncertainty settings
    uncertainty_config <- list(
      show_uncertainty_bands = show_uncertainty_bands,
      uncertainty_alpha = uncertainty_alpha,
      uncertainty_color = uncertainty_color
    )

    p <- tryCatch({
      add_uncertainty_bands(p, filtered_data, years, colors, uncertainty_config)
    }, error = function(e) {
      if(verbose) {
        warning("Failed to add uncertainty bands for transect ", transect_num,
                ": ", e$message, ". Continuing without uncertainty visualization.", call. = FALSE)
      }
      p  # Return plot without uncertainty bands
    })
  }

  # Add line and points (simplified - no legend configuration here)
  p <- p + geom_path(linewidth = 1)

  if(is_multiyear) {
    # Multi-year: add color aesthetic for year grouping
    p <- p + aes(color = as.factor(year), group = year)
  }

  # Add measured/common_min/measured_zero points (no legend)
  if(is_multiyear) {
    # Multi-year: inherit color from aes(color = year)
    p <- p +
      geom_point(
        data = filtered_data %>% filter(point_type %in% c("measured", "common_min", "measured_zero")),
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
        data = filtered_data %>% filter(point_type %in% c("measured", "common_min", "measured_zero")),
        alpha = 0.6,
        size = 1.5,
        shape = 16,
        color = year_color,
        show.legend = FALSE
      )
  }

  # Add interpolated points (green, with shape aesthetic for legend)
  if(has_interp) {
    p <- p +
      geom_point(
        data = filtered_data %>% filter(point_type == "interpolated"),
        aes(shape = "Interpolated"),
        alpha = if(is_multiyear) 0.7 else 0.6,
        size = if(is_multiyear) 1.2 else 1.5,
        color = "green",
        show.legend = TRUE
      )
  }

  # Add extrapolated points (red, with shape aesthetic for legend)
  if(has_extrap) {
    p <- p +
      geom_point(
        data = filtered_data %>% filter(point_type == "extrapolated"),
        aes(shape = "Extrapolated"),
        alpha = if(is_multiyear) 0.7 else 0.6,
        size = if(is_multiyear) 1.2 else 1.5,
        color = "red",
        show.legend = TRUE
      )
  }

  # Add common minimum vline with proper aesthetic mapping
  # Use the unified function directly (deprecated wrapper removed)
  p <- add_common_min_vline_unified(p, common_min_dist, filtered_data, xlim_range, ylim_range)

  # Configure all legends in unified system (from legend.R)
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
  if(show_integration_boundaries && !is.null(auc_results)) {
    p <- tryCatch({
      add_integration_boundaries(p, transect_num, years, auc_results)
    }, error = function(e) {
      if(verbose) {
        warning("Failed to add integration boundaries for transect ", transect_num,
                ": ", e$message, ". Continuing without integration boundaries.", call. = FALSE)
      }
      p  # Return plot without integration boundaries
    })
  }

  # Add AUC visualization
  if(show_auc && !is.null(auc_results)) {
    if(length(years) > 1) {
      p <- add_auc_inset(p, filtered_data, transect_num, years, auc_results, colors,
                         xlim_range, ylim_range, common_min_dist,
                         show_uncertainty = show_auc_uncertainty)
    } else {
      p <- add_auc_text_annotation(p, filtered_data, transect_num, years, auc_results,
                                   xlim_range, ylim_range,
                                   show_uncertainty = show_auc_uncertainty)
    }
  }

  return(p)
}

# ============================================================================
# PUBLIC API
# ============================================================================

#' Plot transect profiles using configuration presets
#'
#' Creates a ggplot visualization of transect elevation profiles with optional
#' AUC visualization, uncertainty bands, and multi-year comparisons. This is
#' the primary interface for generating transect plots in GULNcoastal.
#'
#' @param data Dataframe with transect profile data for a **single transect**.
#'   Required columns: `transect`, `year`, `park`, `distance`, `elevation`.
#'   Optional columns for enhanced visualization: `point_type`, `elev_upper`,
#'   `elev_lower`.
#' @param auc_results Dataframe with AUC results for the transect.
#'   Required columns: `transect`, `year`, `auc`, `segments`.
#'   Optional columns for uncertainty: `auc_upper`, `auc_lower`, `auc_sigma`,
#'   `segment_info`.
#' @param config A `plot_config` object controlling all visualization options.
#'   Create using [create_plot_config()] for full customization, or use a
#'   preset for common workflows.
#'
#' @section Configuration Presets:
#' GULNcoastal provides several presets for common use cases:
#'
#' \describe{
#'   \item{[config_quick()]}{Fast exploration with minimal options. Shows most
#'     recent year(s) at lower resolution. Ideal for interactive data review.
#'     Example: `config_quick(n_years = 2)`}
#'   \item{[config_temporal()]}{Baseline vs. recent comparison. Shows first
#'     survey year alongside recent years for temporal change analysis.
#'     Example: `config_temporal(n_recent = 2)`}
#'   \item{[config_annual()]}{Annual progression plots. Generates separate
#'     output for each survey year showing cumulative change from baseline.
#'     Example: `config_annual()`}
#'   \item{[config_automated_full()]}{Full analysis with all features enabled.
#'     Best for comprehensive reporting. Example: `config_automated_full()`}
#' }
#'
#' @section Custom Configuration:
#' For fine-grained control, use [create_plot_config()] with a data filter:
#'
#' ```
#' config <- create_plot_config(
#'   data_filter = create_data_filter(
#'     year_selector = years_baseline_recent(3),
#'     parks = "PAIS"
#'   ),
#'   show_auc = TRUE,
#'   show_uncertainty_bands = TRUE,
#'   show_inset = TRUE,
#'   axis_limits = "park",
#'   theme = "default",
#'   color_palette = "viridis",
#'   file_format = "png",
#'   dpi = 300
#' )
#' ```
#'
#' @section Key Configuration Options:
#' \describe{
#'   \item{axis_limits}{Controls y-axis scaling: `"park"` (consistent within
#'     park), `"dataset"` (consistent across all data), `"custom"` (user-specified
#'     via `custom_ylim`), or `"none"` (auto-scale per plot).}
#'   \item{show_auc}{If TRUE, displays AUC as inset bar chart (multi-year) or
#'     text annotation (single-year).}
#'   \item{show_uncertainty_bands}{If TRUE and data contains `elev_upper`/`elev_lower`,
#'     displays elevation uncertainty as ribbons.}
#'   \item{show_auc_uncertainty}{If TRUE and AUC results contain uncertainty
#'     columns, displays error bars on AUC visualization.}
#'   \item{show_integration_boundaries}{If TRUE, shows vertical lines at AUC
#'     integration boundaries.}
#' }
#'
#' @return A ggplot object that can be further modified, saved with [ggplot2::ggsave()],
#'   or printed directly.
#'
#' @seealso
#' Configuration: [create_plot_config()], [modify_config()], [validate_config()]
#'
#' Presets: [config_quick()], [config_temporal()], [config_annual()], [config_automated_full()]
#'
#' Data filtering: [create_data_filter()], [years_baseline_recent()], [years_recent()]
#'
#' @examples
#' \dontrun{
#' # Quick exploration of recent data
#' p <- plot_transect_profiles(
#'   data = transect_data,
#'   auc_results = auc_data,
#'   config = config_quick(n_years = 2)
#' )
#'
#' # Temporal comparison (baseline vs recent)
#' p <- plot_transect_profiles(
#'   data = transect_data,
#'   auc_results = auc_data,
#'   config = config_temporal(n_recent = 2)
#' )
#'
#' # Custom configuration with uncertainty visualization
#' config <- create_plot_config(
#'   data_filter = create_data_filter(year_selector = years_recent(3)),
#'   show_uncertainty_bands = TRUE,
#'   show_auc_uncertainty = TRUE,
#'   axis_limits = "park"
#' )
#' p <- plot_transect_profiles(transect_data, auc_data, config)
#'
#' # Save the plot
#' ggplot2::ggsave("transect_plot.png", p, width = 12, height = 8, dpi = 300)
#' }
#'
#' @export
plot_transect_profiles <- function(data, auc_results, config) {

  # Validate inputs
  if (missing(data) || is.null(data)) {
    stop("data is required and cannot be NULL", call. = FALSE)
  }

  if (missing(auc_results) || is.null(auc_results)) {
    stop("auc_results is required and cannot be NULL", call. = FALSE)
  }

  if (missing(config) || is.null(config)) {
    stop("config is required and cannot be NULL", call. = FALSE)
  }

  # Validate config
  if (!inherits(config, "plot_config")) {
    stop("config must be a plot_config object. Use create_plot_config() or config_automated_full().",
         call. = FALSE)
  }

  # Validate data is not empty
  if (nrow(data) == 0) {
    stop("data cannot be empty", call. = FALSE)
  }

  if (nrow(auc_results) == 0) {
    stop("auc_results cannot be empty", call. = FALSE)
  }

  # Validate required columns in data
  required_data_cols <- c("transect", "year", "park", "distance", "elevation")
  missing_data_cols <- setdiff(required_data_cols, names(data))
  if (length(missing_data_cols) > 0) {
    stop("data is missing required columns: ", paste(missing_data_cols, collapse = ", "),
         call. = FALSE)
  }

  # Validate required columns in auc_results
  required_auc_cols <- c("transect", "year", "auc", "segments")
  missing_auc_cols <- setdiff(required_auc_cols, names(auc_results))
  if (length(missing_auc_cols) > 0) {
    stop("auc_results is missing required columns: ", paste(missing_auc_cols, collapse = ", "),
         call. = FALSE)
  }

  # Extract transect and years from data
  transect_id <- unique(data$transect)
  if (length(transect_id) != 1) {
    stop("data must contain exactly one transect. Found: ",
         paste(transect_id, collapse = ", "),
         call. = FALSE)
  }

  years <- unique(data$year) %>% sort()
  if (length(years) == 0) {
    stop("No years found in data for transect ", transect_id, call. = FALSE)
  }

  park <- unique(data$park) %>% first()
  if (is.na(park) || is.null(park)) {
    stop("No valid park found in data for transect ", transect_id, call. = FALSE)
  }

  # Get color palette with error handling
  color_palette <- tryCatch({
    get_color_palette(data)
  }, error = function(e) {
    stop("Failed to generate color palette: ", e$message,
         call. = FALSE)
  })

  # Determine y-axis limits based on config
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

  # Call the underlying plotting function with error handling
  plot <- tryCatch({
    plot_transect_year(
      data = data,
      transect_num = transect_id,
      years = years,
      auc_results = auc_results,
      show_auc = config$show_auc,
      shade_area = config$shade_area,
      color_palette = color_palette,
      xlim_range = NULL,  # Always auto-scale x-axis per transect
      ylim_range = ylim_to_use,
      show_uncertainty_bands = config$show_uncertainty_bands,
      show_auc_uncertainty = config$show_auc_uncertainty,
      show_integration_boundaries = config$show_integration_boundaries,
      uncertainty_alpha = config$uncertainty_alpha,
      uncertainty_color = config$uncertainty_color,
      verbose = config$verbose
    )
  }, error = function(e) {
    stop("Failed to generate plot for transect ", transect_id, ": ", e$message,
         call. = FALSE)
  })

  return(plot)
}
