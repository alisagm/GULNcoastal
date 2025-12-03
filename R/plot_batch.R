# plot_batch.R
# Batch plotting utilities for saving and managing plot outputs
#
# This module provides helper functions for batch plot operations.
# Currently contains save_plot_file() for standardized plot export.

#' Save a ggplot to file
#'
#' Wrapper around [ggplot2::ggsave()] with standardized defaults and
#' error handling for batch plot generation workflows.
#'
#' @param plot A ggplot2 plot object to save.
#' @param filename Character. Output filename. Extension is added automatically
#'   if not present (based on `file_format`).
#' @param output_dir Character. Directory path for output. Must exist.
#' @param file_format Character. File format/extension. Default `"jpg"`.
#'   Common options: `"jpg"`, `"png"`, `"pdf"`.
#' @param width Numeric. Plot width in inches. Default `12`.
#' @param height Numeric. Plot height in inches. Default `8`.
#' @param dpi Numeric. Resolution in dots per inch. Default `300`.
#' @param verbose Logical. If `TRUE`, prints confirmation message. Default `FALSE`.
#'
#' @return Invisibly returns the full file path on success, or `NULL` on failure
#'
#' @examples
#' \dontrun{
#' # Save a plot with defaults
#' save_plot_file(my_plot, "transect_001", "/path/to/output")
#'
#' # Save as high-resolution PNG
#' save_plot_file(
#'   plot = my_plot,
#'   filename = "transect_001_hires",
#'   output_dir = "/path/to/output",
#'   file_format = "png",
#'   dpi = 600
#' )
#' }
#'
#' @seealso [ggplot2::ggsave()] for underlying save mechanism,
#'   [plot_transect()] for generating transect plots
#'
#' @export
save_plot_file <- function(plot,
                           filename,
                           output_dir,
                           file_format = "jpg",
                           width = 12,
                           height = 8,
                           dpi = 300,
                           verbose = FALSE) {


  # Validate required inputs
  if (missing(plot) || is.null(plot)) {
    stop("plot is required and cannot be NULL", call. = FALSE)
  }

  if (!inherits(plot, "gg") && !inherits(plot, "ggplot")) {
    stop("plot must be a ggplot object", call. = FALSE)
  }

  if (missing(filename) || is.null(filename) || filename == "") {
    stop("filename is required and cannot be empty", call. = FALSE)
  }

  if (missing(output_dir) || is.null(output_dir)) {
    stop("output_dir is required and cannot be NULL", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    stop("output_dir does not exist: ", output_dir, call. = FALSE)
  }

  # Ensure correct extension
  if (!grepl(paste0("\\.", file_format, "$"), filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".", file_format)
  }

  filepath <- file.path(output_dir, filename)

  tryCatch({
    suppressMessages(
      ggsave(
        filename = filepath,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        units = "in"
      )
    )

    if (verbose) {
      message("  Saved: ", filepath)
    }

    return(invisible(filepath))

  }, error = function(e) {
    warning("Failed to save plot ", filename, ": ", e$message, call. = FALSE)
    return(NULL)
  })
}
