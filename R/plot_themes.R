# plot_themes.R
#' Plot Theme Functions
#'
#' @description
#' Visual theme presets for transect profile plots. Provides customizable
#' ggplot2 themes and color schemes for different use cases including
#' publication, presentation, colorblind-accessible, and grayscale output.
#'
#' @name themes
#' @keywords internal
NULL

# ============================================================================
# THEME PRESETS
# ============================================================================

#' Get a predefined plot theme
#'
#' Returns a ggplot2 theme object based on the specified preset.
#' Themes are designed for different output contexts including publication,
#' presentation, minimal, and dark mode displays.
#'
#' @param preset Character. Theme preset name:
#'   \describe{
#'     \item{default}{Standard theme with clean styling}
#'     \item{publication}{Minimal design for journal figures}
#'     \item{presentation}{Larger elements for slides}
#'     \item{minimal}{Very clean, reduced chrome}
#'     \item{dark}{Dark background for screen viewing}
#'   }
#' @param base_size Numeric. Base font size in points (default: 11)
#'
#' @return A ggplot2 theme object that can be added to plots with \code{+}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Apply different themes
#' p + get_plot_theme("publication")
#' p + get_plot_theme("presentation", base_size = 16)
#' p + get_plot_theme("dark")
get_plot_theme <- function(preset = "default", base_size = 11) {
  # Validate preset
  valid_presets <- c("default", "publication", "presentation", "minimal", "dark")
  if (!preset %in% valid_presets) {
    warning("Unknown theme preset '", preset, "'. Using 'default'. ",
            "Valid options: ", paste(valid_presets, collapse = ", "),
            call. = FALSE)
    preset <- "default"
  }

  base_theme <- switch(preset,
    "default" = theme_default(base_size),
    "publication" = theme_publication(base_size),
    "presentation" = theme_presentation(base_size),
    "minimal" = theme_minimal_clean(base_size),
    "dark" = theme_dark_plot(base_size),
    theme_default(base_size)
  )

  return(base_theme)
}

#' Default transect plot theme
#'
#' Standard theme with clean styling suitable for most purposes. Based on
#' \code{ggplot2::theme_bw()} with customizations for readability and clarity.
#'
#' @param base_size Numeric. Base font size in points (default: 11)
#'
#' @return A ggplot2 theme object
#'
#' @keywords internal
#' @noRd
theme_default <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      # Title styling
      plot.title = ggplot2::element_text(size = base_size + 3, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size + 1, hjust = 0, color = "gray40"),

      # Axis styling
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),

      # Panel styling
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),

      # Legend styling
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 2),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),

      # Plot margins
      plot.margin = ggplot2::margin(10, 15, 10, 10)
    )
}

#' Publication-ready theme
#'
#' Minimal, clean theme suitable for journal figures. Designed for clarity
#' at small sizes and grayscale reproduction. Based on \code{ggplot2::theme_classic()}
#' with no grid lines (publication standard).
#'
#' @param base_size Numeric. Base font size in points (default: 10)
#'
#' @return A ggplot2 theme object
#'
#' @keywords internal
#' @noRd
theme_publication <- function(base_size = 10) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      # Title styling - minimal
      plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size, hjust = 0, color = "gray30"),

      # Axis styling - clear and readable
      axis.title = ggplot2::element_text(size = base_size, face = "bold"),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "black"),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.6),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.4),

      # No grid lines (publication standard)
      panel.grid = ggplot2::element_blank(),

      # Legend styling
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size - 1, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size - 2),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.8, "lines"),

      # Tight margins
      plot.margin = ggplot2::margin(5, 10, 5, 5)
    )
}

#' Presentation theme
#'
#' Theme with larger text and bold styling for slides and presentations.
#' Based on \code{ggplot2::theme_minimal()} with increased font sizes and
#' generous margins for visibility on large displays.
#'
#' @param base_size Numeric. Base font size in points (default: 14, recommended: 14-18)
#'
#' @return A ggplot2 theme object
#'
#' @keywords internal
#' @noRd
theme_presentation <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Title styling - large and prominent
      plot.title = ggplot2::element_text(size = base_size + 6, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size + 2, hjust = 0, color = "gray30"),

      # Axis styling - bold and readable
      axis.title = ggplot2::element_text(size = base_size + 2, face = "bold"),
      axis.text = ggplot2::element_text(size = base_size, color = "black"),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.8),

      # Light grid
      panel.grid.major = ggplot2::element_line(color = "gray85", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),

      # Legend styling - larger
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.size = ggplot2::unit(1.2, "lines"),

      # Generous margins
      plot.margin = ggplot2::margin(15, 20, 15, 15)
    )
}

#' Minimal clean theme
#'
#' Very minimal theme with reduced visual chrome. Based on
#' \code{ggplot2::theme_minimal()} with subtle styling and reduced emphasis
#' on plot elements for a modern, clean appearance.
#'
#' @param base_size Numeric. Base font size in points (default: 11)
#'
#' @return A ggplot2 theme object
#'
#' @keywords internal
#' @noRd
theme_minimal_clean <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Minimal title styling
      plot.title = ggplot2::element_text(size = base_size + 2, face = "plain", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size, hjust = 0, color = "gray50"),

      # Clean axis
      axis.title = ggplot2::element_text(size = base_size - 1, color = "gray30"),
      axis.text = ggplot2::element_text(size = base_size - 2, color = "gray40"),
      axis.line = ggplot2::element_blank(),

      # Subtle grid
      panel.grid.major = ggplot2::element_line(color = "gray92", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),

      # Minimal legend
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 2),

      # Minimal margins
      plot.margin = ggplot2::margin(5, 10, 5, 5)
    )
}

#' Dark theme for screen viewing
#'
#' Dark background theme optimized for screen viewing and presentations.
#' Based on \code{ggplot2::theme_dark()} with customizations for improved
#' readability with light text on dark backgrounds.
#'
#' @param base_size Numeric. Base font size in points (default: 11)
#'
#' @return A ggplot2 theme object
#'
#' @keywords internal
#' @noRd
theme_dark_plot <- function(base_size = 11) {
  ggplot2::theme_dark(base_size = base_size) +
    ggplot2::theme(
      # Light text on dark background
      plot.title = ggplot2::element_text(size = base_size + 3, face = "bold", hjust = 0, color = "white"),
      plot.subtitle = ggplot2::element_text(size = base_size + 1, hjust = 0, color = "gray70"),
      plot.background = ggplot2::element_rect(fill = "gray20", color = NA),

      # Axis styling
      axis.title = ggplot2::element_text(size = base_size, color = "gray90"),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "gray80"),
      axis.line = ggplot2::element_line(color = "gray60", linewidth = 0.5),

      # Panel styling
      panel.background = ggplot2::element_rect(fill = "gray25", color = NA),
      panel.grid.major = ggplot2::element_line(color = "gray40", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),

      # Legend styling
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "gray20", color = NA),
      legend.title = ggplot2::element_text(size = base_size - 1, color = "gray90"),
      legend.text = ggplot2::element_text(size = base_size - 2, color = "gray80"),
      legend.key = ggplot2::element_rect(fill = "gray25", color = NA)
    )
}

# ============================================================================
# THEME UTILITIES
# ============================================================================

#' List available theme presets
#'
#' Returns information about available theme presets with descriptions and
#' recommended use cases. Useful for discovering which themes are available
#' and selecting the appropriate theme for your output context.
#'
#' @param as_df Logical. If \code{TRUE}, return a data frame. If \code{FALSE}
#'   (default), print formatted list.
#'
#' @return If \code{as_df=TRUE}, returns a data frame with columns Theme,
#'   Description, and Recommended For. Otherwise prints formatted output and
#'   returns the data frame invisibly.
#'
#' @export
#'
#' @examples
#' # Print formatted list
#' list_themes()
#'
#' # Get as data frame
#' themes <- list_themes(as_df = TRUE)
list_themes <- function(as_df = FALSE) {
  themes <- data.frame(
    Theme = c("default", "publication", "presentation", "minimal", "dark"),
    Description = c(
      "Standard clean styling for general use",
      "Minimal design for journal figures (no grid lines)",
      "Large text and bold styling for slides",
      "Very clean with reduced visual elements",
      "Dark background for screen viewing"
    ),
    `Recommended For` = c(
      "Reports, exploratory analysis",
      "Journal articles, printed figures",
      "Slides, posters, large displays",
      "Modern reports, web display",
      "Screen presentations, dashboards"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (as_df) {
    return(themes)
  }

  # Print formatted output
  cat("\n")
  cat("Available Plot Themes\n")
  cat(strrep("=", 60), "\n\n")

  for (i in seq_len(nrow(themes))) {
    cat(themes$Theme[i], "\n")
    cat("  ", themes$Description[i], "\n")
    cat("  Best for: ", themes$`Recommended For`[i], "\n\n")
  }

  cat("Usage:\n")
  cat("  library(ggplot2)\n")
  cat("  p <- ggplot(data, aes(x, y)) + geom_point()\n")
  cat("  p + get_plot_theme(\"publication\")\n\n")

  invisible(themes)
}

#' List available color palettes
#'
#' Returns information about available color palettes from the viridis family.
#' All listed palettes are perceptually uniform and designed to be
#' colorblind-accessible.
#'
#' @param as_df Logical. If \code{TRUE}, return a data frame. If \code{FALSE}
#'   (default), print formatted list.
#'
#' @return If \code{as_df=TRUE}, returns a data frame with columns Palette,
#'   Description, and Colorblind Safe. Otherwise prints formatted output and
#'   returns the data frame invisibly.
#'
#' @export
#'
#' @examples
#' # Print formatted list
#' list_palettes()
#'
#' # Get as data frame
#' palettes <- list_palettes(as_df = TRUE)
list_palettes <- function(as_df = FALSE) {
  palettes <- data.frame(
    Palette = c("viridis", "magma", "plasma", "inferno", "cividis", "turbo"),
    Description = c(
      "Default perceptually uniform palette (purple to yellow)",
      "Dark purple to light yellow (high contrast)",
      "Blue to yellow through pink (vibrant)",
      "Black to yellow through red (dramatic)",
      "Blue to yellow (colorblind-friendly)",
      "Rainbow-like but perceptually uniform"
    ),
    `Colorblind Safe` = c("Yes", "Yes", "Yes", "Yes", "Yes (designed for)", "Moderate"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (as_df) {
    return(palettes)
  }

  # Print formatted output
  cat("\n")
  cat("Available Color Palettes\n")
  cat(strrep("=", 60), "\n\n")

  for (i in seq_len(nrow(palettes))) {
    cat(palettes$Palette[i], "\n")
    cat("  ", palettes$Description[i], "\n")
    cat("  Colorblind safe: ", palettes$`Colorblind Safe`[i], "\n\n")
  }

  cat("Note: These palettes require the viridis or viridisLite package.\n\n")

  invisible(palettes)
}
