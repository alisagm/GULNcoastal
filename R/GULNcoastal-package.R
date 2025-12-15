#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom ggplot2 ggplot ggsave
#' @importFrom readr read_csv
#' @importFrom stringr str_detect
#' @importFrom here here
#' @importFrom withr with_tempdir
#' @importFrom utils head
## usethis namespace: end
NULL

#' Gulf Coast Network Coastal Monitoring Analysis Tools
#'
#' @description
#' Tools for analyzing coastal monitoring data from Gulf Coast Network park units.
#' This package provides data processing, analysis, and visualization capabilities
#' for coastal ecosystem monitoring with advanced uncertainty quantification.
#'
#' @section Package Workflows:
#'
#' The package supports two main workflows:
#'
#' **Standard Workflow** (automated, convenient):
#' \enumerate{
#'   \item \code{\link{run_transect_analysis}()} - Import, analyze, export
#'   \item \code{\link{load_transect_results}()} - Load saved results
#'   \item \code{\link{plot_transect}()} - Visualize transects
#' }
#'
#' **Custom Workflow** (flexible, for complex cases):
#' \enumerate{
#'   \item \code{\link{import_transects_park}()} - Import and clean data YOUR way
#'   \item \code{\link{run_transect_analysis_data}()} - Analyze pre-loaded data
#'   \item Individual pipeline steps for maximum control
#' }
#'
#' @section Main Function Categories:
#'
#' \strong{Analysis Pipeline:}
#' \itemize{
#'   \item \code{\link{run_transect_analysis}()} - Complete automated pipeline
#'   \item \code{\link{run_transect_analysis_data}()} - Flexible pipeline for pre-loaded data
#'   \item \code{\link{run_pairwise_analysis}()} - Pairwise transect comparison
#' }
#'
#' \strong{Data Import/Export:}
#' \itemize{
#'   \item \code{\link{import_transects_park}()} - Import beach transect CSV data
#'   \item \code{\link{load_transect_results}()} - Load previously saved results
#'   \item \code{\link{export_all_results}()} - Export RDS and publication-ready CSVs
#' }
#'
#' \strong{Visualization:}
#' \itemize{
#'   \item \code{\link{plot_transect}()} - Create transect elevation plot
#'   \item \code{\link{apply_config}()} - Apply configuration to data
#'   \item \code{\link{create_plot_config}()} - Create plot configuration
#'   \item \code{\link{config_quick}()}, \code{\link{config_temporal}()},
#'         \code{\link{config_annual}()} - Configuration presets
#' }
#'
#' \strong{Data Filtering:}
#' \itemize{
#'   \item \code{\link{create_data_filter}()} - Create semantic data filter
#'   \item \code{\link{years_all}()}, \code{\link{years_first}()},
#'         \code{\link{years_recent}()} - Year selectors
#'   \item \code{\link{years_baseline_recent}()} - Baseline + recent years
#'   \item \code{\link{list_year_selectors}()} - See all available selectors
#' }
#'
#' \strong{Pipeline Steps (Advanced):}
#' \itemize{
#'   \item \code{\link{remove_negatives}()} - Clean extraneous negative elevations
#'   \item \code{\link{assign_accuracy}()} - Assign measurement uncertainty
#'   \item \code{\link{find_zero_crossings}()} - Identify zero-elevation points
#'   \item \code{\link{calculate_common_minimum}()} - Common minimum distance
#'   \item \code{\link{calculate_auc_with_uncertainty}()} - Probability-weighted AUC
#' }
#'
#' @section Quick Start:
#'
#' For standard analysis:
#' \preformatted{
#' library(GULNcoastal)
#'
#' # Run complete analysis
#' results <- run_transect_analysis(
#'   data_dir = "path/to/data",
#'   output_dir = "path/to/output"
#' )
#'
#' # Create plots
#' config <- config_temporal(n_recent = 2)
#' plots <- apply_config(config, results$data, results$auc_results)
#' }
#'
#' For custom workflows:
#' \preformatted{
#' # Import and clean your way
#' data1 <- import_transects_park("file1.csv")
#' data2 <- import_transects_park("file2.csv")
#' data <- bind_rows(data1, data2) # combine as needed
#'
#' # Run analysis on cleaned data
#' results <- run_transect_analysis_data(
#'   data = data,
#'   accuracy_table = "accuracy_values.csv"
#' )
#' }
#'
#' @section Learn More:
#'
#' See vignettes for detailed tutorials:
#' \itemize{
#'   \item \code{vignette("introduction", package = "GULNcoastal")}
#'   \item \code{vignette("custom-workflows", package = "GULNcoastal")}
#'   \item \code{vignette("filtering", package = "GULNcoastal")}
#' }
#'
#' @docType _PACKAGE
#' @name GULNcoastal-package
NULL

