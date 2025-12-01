# R/plot_legend.R
# Consolidated legend configuration for transect profile plots
#
# This module provides unified legend management to ensure all legend
# components (linetype, color, shape) are properly integrated into a
# single cohesive display.

# ============================================================================
# LEGEND CONFIGURATION
# ============================================================================

#' Configure all plot legends in a unified system
#'
#' This function consolidates all legend components (common minimum boundary,
#' year colors, and point types) into a single integrated legend display.
#' It replaces the fragmented legend generation that previously caused
#' misalignment and separation issues.
#'
#' @param p ggplot object with data layers already added
#' @param has_common_min Logical. TRUE if common minimum vline is present
#' @param has_interp Logical. TRUE if interpolated points exist in data
#' @param has_extrap Logical. TRUE if extrapolated points exist in data
#' @param is_multiyear Logical. TRUE if plotting multiple years
#' @param colors Named vector mapping years to colors (e.g., c("2020" = "#FF0000", "2021" = "#00FF00"))
#' @param years Numeric vector of years being plotted
#' @returns Modified ggplot object with unified legend configuration
#' @importFrom ggplot2 scale_linetype_manual scale_color_manual scale_shape_manual guide_legend guides theme
#' @export
configure_plot_legends <- function(p, has_common_min, has_interp, has_extrap,
                                   is_multiyear, colors, years) {

  # Build guide list dynamically based on what's present
  guide_list <- list()
  guide_order <- 1

  # ============================================================================
  # 1. LINETYPE LEGEND (Common Minimum Boundary)
  # ============================================================================

  if (has_common_min) {
    # Add scale for linetype
    p <- p +
      scale_linetype_manual(
        name = "Boundary",
        values = c("Integration boundary" = "dashed"),
        labels = c("Integration boundary" = "Lower bound for area calculation")
      )

    # Configure linetype guide
    guide_list$linetype <- guide_legend(
      order = guide_order,
      nrow = 1,
      override.aes = list(
        color = "red",
        alpha = 0.7,
        linetype = "dashed",
        linewidth = 0.8
      )
    )
    guide_order <- guide_order + 1
  }

  # ============================================================================
  # 2. COLOR LEGEND (Year)
  # ============================================================================

  if (is_multiyear) {
    # Add scale for year colors
    p <- p +
      scale_color_manual(
        name = "Year",
        values = colors
      )

    # Configure color guide
    guide_list$color <- guide_legend(
      order = guide_order,
      nrow = 1
    )
    guide_order <- guide_order + 1
  }

  # ============================================================================
  # 3. SHAPE LEGEND (Point Type)
  # ============================================================================

  if (has_interp || has_extrap) {
    # Build legend values and colors dynamically
    legend_values <- c()
    legend_colors <- c()
    legend_labels <- c()

    if (has_interp) {
      legend_values <- c(legend_values, "Interpolated" = 16)
      legend_colors <- c(legend_colors, "Interpolated" = "green")
      legend_labels <- c(legend_labels, "Interpolated" = "Interpolated")
    }

    if (has_extrap) {
      legend_values <- c(legend_values, "Extrapolated" = 16)
      legend_colors <- c(legend_colors, "Extrapolated" = "red")
      legend_labels <- c(legend_labels, "Extrapolated" = "Extrapolated")
    }

    # Add scale for point types
    # Use limits parameter to explicitly control factor level order
    p <- p +
      scale_shape_manual(
        name = "Point Type",
        values = legend_values,
        labels = legend_labels,
        limits = names(legend_values)  # Force consistent factor level order
      )

    # Configure shape guide
    # Reorder colors to match the explicit factor level order
    # (override.aes ignores names, so we must use unnamed vector in correct order)
    legend_colors_ordered <- legend_colors[names(legend_values)]

    guide_list$shape <- guide_legend(
      order = guide_order,
      nrow = 1,
      override.aes = list(
        color = unname(legend_colors_ordered),  # Unnamed vector in correct order
        shape = 16,
        size = if (is_multiyear) 1.2 else 1.5,
        alpha = if (is_multiyear) 0.7 else 0.6
      )
    )
    guide_order <- guide_order + 1
  }

  # ============================================================================
  # 4. APPLY UNIFIED GUIDES
  # ============================================================================

  # Set legend position
  if (is_multiyear || has_interp || has_extrap || has_common_min) {
    p <- p + theme(legend.position = "bottom")
  }

  # Apply all guides together
  if (length(guide_list) > 0) {
    p <- p + do.call(guides, guide_list)
  }

  return(p)
}

#' Add common minimum vline with proper aesthetic mapping
#'
#' Adds a vertical line at the common minimum distance with proper
#' aesthetic mapping that integrates with the legend system.
#' Replaces the old invisible geom trick.
#'
#' Internally calls \code{\link{find_label_position}} from the plot_positioning
#' module to determine optimal label placement.
#'
#' @param p ggplot object
#' @param common_min_dist Numeric. Common minimum distance value, or NA
#' @param filtered_data Dataframe with transect data (must have distance and elevation columns)
#' @param xlim_range Numeric vector of x-axis limits c(min, max), or NULL for auto
#' @param ylim_range Numeric vector of y-axis limits c(min, max), or NULL for auto
#' @returns Modified ggplot object with vline and label added
#' @importFrom ggplot2 geom_vline annotate
#' @export
add_common_min_vline_unified <- function(p, common_min_dist, filtered_data,
                                         xlim_range, ylim_range) {

  if (is.na(common_min_dist)) {
    return(p)
  }

  # Determine actual axis limits for label positioning
  actual_xlim <- if (!is.null(xlim_range)) {
    xlim_range
  } else {
    x_range <- range(filtered_data$distance, na.rm = TRUE)
    x_padding <- diff(x_range) * 0.05  # GGPLOT_DEFAULT_EXPANSION
    c(x_range[1] - x_padding, x_range[2] + x_padding)
  }

  actual_ylim <- if (!is.null(ylim_range)) {
    ylim_range
  } else {
    c(-1, 11)  # DEFAULT_Y_MIN, DEFAULT_Y_MAX
  }

  # Find optimal label position using positioning module
  label_y_pos <- find_label_position(
    data = filtered_data,
    vline_x = common_min_dist,
    xlim = actual_xlim,
    ylim = actual_ylim
  )

  # Create a data frame for the vline with aesthetic mapping
  vline_data <- data.frame(
    x = common_min_dist,
    linetype_category = "Integration boundary"
  )

  # Add vline with proper aesthetic mapping for legend integration
  p <- p +
    geom_vline(
      data = vline_data,
      aes(xintercept = x, linetype = linetype_category),
      color = "red",
      alpha = 0.7,
      linewidth = 0.8
    ) +
    annotate(
      "text",
      x = common_min_dist,
      y = label_y_pos,
      label = paste0("Min dist: ", round(common_min_dist, 1), " m"),
      angle = 90,
      hjust = 1,
      vjust = -0.5,
      size = 3,
      color = "red",
      fontface = "bold"
    )

  return(p)
}
