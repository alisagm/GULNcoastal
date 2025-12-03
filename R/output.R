# output.R
# Functions for saving and loading topographic analysis results
#
# This module provides functions for exporting and loading topographic transect
# analysis results, including RDS files with full precision and CSV files with
# error-aware rounding for publication quality.

# ============================================================================
# ERROR-AWARE ROUNDING FUNCTIONS (for CSV exports)
# ============================================================================

#' Round Value to Match Uncertainty Precision
#'
#' Rounds a value to the appropriate number of decimal places based on its
#' uncertainty, following scientific convention: uncertainty is rounded to
#' 1-2 significant figures, and the value is rounded to match.
#'
#' Fully vectorized for memory-efficient use with data frame columns.
#'
#' @param value Numeric value or vector to round
#' @param uncertainty Uncertainty in the value (1-sigma)
#' @param sig_figs Number of significant figures for uncertainty (default: 2)
#' @return Rounded value or vector
#' @examples
#' round_to_uncertainty(1.23456, 0.034)  # returns 1.23
#' round_to_uncertainty(123.456, 3.4)    # returns 123
#' @export
round_to_uncertainty <- function(value, uncertainty, sig_figs = 2) {
  # Handle NA and zero uncertainty cases
  result <- value
  valid <- !is.na(value) & !is.na(uncertainty) & uncertainty != 0

  # Only process valid entries
  if (any(valid)) {
    # Vectorized calculation of decimal places for all values
    uncertainty_order <- floor(log10(abs(uncertainty[valid])))
    decimal_places <- sig_figs - 1 - uncertainty_order

    # Apply rounding using Map (more efficient than loop for large vectors)
    result[valid] <- unlist(Map(round, value[valid], decimal_places),
                           use.names = FALSE)
  }

  return(result)
}

#' Round Uncertainty to Significant Figures
#'
#' Rounds uncertainty values to 1-2 significant figures for reporting.
#'
#' Fully vectorized for memory-efficient use with data frame columns.
#'
#' @param uncertainty Numeric uncertainty value or vector
#' @param sig_figs Number of significant figures (default: 2)
#' @return Rounded uncertainty value or vector
#' @examples
#' round_uncertainty(0.0342)  # returns 0.034
#' round_uncertainty(3.456)   # returns 3.5
#' @export
round_uncertainty <- function(uncertainty, sig_figs = 2) {
  # Handle NA and zero cases
  result <- uncertainty
  valid <- !is.na(uncertainty) & uncertainty != 0

  # Only process valid entries
  if (any(valid)) {
    # Vectorized calculation of decimal places for all values
    uncertainty_order <- floor(log10(abs(uncertainty[valid])))
    decimal_places <- sig_figs - 1 - uncertainty_order

    # Apply rounding using Map (more efficient than loop for large vectors)
    result[valid] <- unlist(Map(round, uncertainty[valid], decimal_places),
                           use.names = FALSE)
  }

  return(result)
}

# ============================================================================
# MASTER RDS SAVE FUNCTION
# ============================================================================

#' Save Complete Topographic Transect Analysis Results
#'
#' Saves all analysis results to a dated master RDS file containing complete
#' dataset, AUC results with metrics, and processing metadata. Includes
#' probability-weighted AUC with uncertainty propagation.
#'
#' NOTE: RDS files retain FULL NUMERICAL PRECISION. Error-aware rounding
#' is only applied to CSV exports for publication quality.
#'
#' @param data Data frame. Final processed transect data with all point types,
#'   measurement accuracy (sigma_h, sigma_v), uncertainty quantification,
#'   and confidence values
#' @param auc_results Data frame. AUC results with columns: transect, year,
#'   auc, auc_upper, auc_lower, auc_sigma, auc_uncertainty_pct,
#'   overlap_m, overlap_pct, transect_length, n_segments, segment_info, segments
#' @param output_dir Character. Directory to save RDS file. Default: current
#'   working directory
#' @param date Character. Date string (YYYYMMDD). Default: today's date
#' @param verbose Logical. Print informational messages. Default: TRUE
#' @return Character. Full path to saved RDS file (invisibly)
#' @examples
#' \dontrun{
#' # Save results to current directory
#' save_topo_transects_rds(data, auc_results)
#'
#' # Save to specific directory
#' save_topo_transects_rds(data, auc_results, output_dir = "path/to/output")
#' }
#' @export
save_topo_transects_rds <- function(data,
                                    auc_results,
                                    output_dir = output_dir,
                                    date = format(Sys.Date(), "%Y%m%d"),
                                    verbose = TRUE) {

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) cat("Created directory:", output_dir, "\n")
  }

  # Generate filename
  filename <- paste0("topo_transects_", date, ".rds")
  filepath <- file.path(output_dir, filename)

  # Package results with metadata
  results <- list(
    data = data,
    auc_results = auc_results,
    metadata = list(
      timestamp = Sys.time(),
      date = date,
      n_transects = length(unique(data$transect)),
      n_years = length(unique(data$year)),
      n_observations = nrow(data),
      parks = sort(unique(data$park)),
      years = sort(unique(data$year))
    )
  )

  # Save to RDS
  tryCatch({
    saveRDS(results, file = filepath)

    if (verbose) {
      cat("Saved master RDS file to:\n")
      cat("  ", filepath, "\n")
      cat("  Contains:\n")
      cat("    - Complete dataset (", nrow(data), " points)\n", sep = "")
      cat("    - AUC results (", nrow(auc_results), " records)\n", sep = "")
      cat("    - Processing metadata\n")
    }

    return(invisible(filepath))

  }, error = function(e) {
    stop("Failed to save RDS file: ", e$message)
  })
}


# ============================================================================
# PARK-SPECIFIC CSV EXPORT FUNCTIONS
# ============================================================================

#' Export Transect Profiles CSV for Single Park
#'
#' Exports profile data (measurements, common_mins, zero crossings) to CSV
#' for a single park. Includes all error propagation and uncertainty columns
#' with error-aware rounding applied for publication quality.
#'
#' Exported columns:
#'   - Identifiers: transect, year, distance, elevation, point_type, cross_island
#'   - Measurement accuracy: sigma_h, sigma_v, accuracy_tier
#'   - Uncertainty quantification: uncertainty, confidence, uncertainty_95ci
#'   - Slope/extrapolation: slope, extrap_distance (NA for measured points)
#'
#' Rounding applied (CSV only; RDS retains full precision):
#'   - distance, elevation: Rounded to match measurement uncertainty
#'   - uncertainties (sigma_h, sigma_v, etc.): Rounded to 2 significant figures
#'   - slope: Rounded to 3 significant figures
#'
#' @param data Data frame. Complete dataset with all point types
#' @param park Character. Park code (e.g., "GUIS" or "PAIS")
#' @param output_dir Character. Base output directory. Default: current
#'   working directory
#' @param date Character. Date string (YYYYMMDD). Default: today's date
#' @param verbose Logical. Print informational messages. Default: TRUE
#' @return Character. Full path to saved CSV file (invisibly)
#' @examples
#' \dontrun{
#' export_transect_profiles_csv(data, park = "GUIS")
#' }
#' @importFrom dplyr filter select arrange mutate if_else
#' @importFrom readr write_csv
#' @export
export_transect_profiles_csv <- function(data, park,
                                         output_dir = output_dir,
                                         date = format(Sys.Date(), "%Y%m%d"),
                                         verbose = TRUE) {

  # Create park-specific directory
  park_dir <- file.path(output_dir, park, "topo")
  if (!dir.exists(park_dir)) {
    dir.create(park_dir, recursive = TRUE)
    if (verbose) cat("Created directory:", park_dir, "\n")
  }

  # Generate filename
  filename <- paste0("transect_profiles_", date, ".csv")
  filepath <- file.path(park_dir, filename)

  # Filter and select columns for this park
  # Include all error propagation and uncertainty columns
  park_data <- data |>
    dplyr::filter(park == !!park) |>
    dplyr::select(
      transect, year, distance, elevation, point_type, cross_island,
      # Measurement accuracy columns
      sigma_h, sigma_v, accuracy_tier,
      # Uncertainty quantification columns
      uncertainty, confidence, uncertainty_95ci,
      # Slope/extrapolation columns (NA for measured points)
      slope, extrap_distance
    ) |>
    dplyr::arrange(transect, year, distance) |>
    # Apply error-aware rounding for CSV export
    dplyr::mutate(
      # Round distance to horizontal uncertainty precision
      distance = round_to_uncertainty(distance, sigma_h),

      # Round elevation to vertical uncertainty precision
      elevation = round_to_uncertainty(elevation, sigma_v),

      # Round uncertainties to 2 significant figures
      sigma_h = round_uncertainty(sigma_h, sig_figs = 2),
      sigma_v = round_uncertainty(sigma_v, sig_figs = 2),
      uncertainty = round_uncertainty(uncertainty, sig_figs = 2),
      uncertainty_95ci = round_uncertainty(uncertainty_95ci, sig_figs = 2),

      # Round slope to 3 significant figures (if present)
      slope = dplyr::if_else(
        !is.na(slope),
        signif(slope, 3),
        slope
      ),

      # Round extrapolation distance to horizontal uncertainty precision
      extrap_distance = dplyr::if_else(
        !is.na(extrap_distance) & !is.na(sigma_h),
        round_to_uncertainty(extrap_distance, sigma_h),
        extrap_distance
      )
    )

  # Write CSV
  tryCatch({
    readr::write_csv(park_data, filepath)

    if (verbose) {
      cat("  Profiles:", filepath, "\n")
      cat("            (", nrow(park_data), " points)\n", sep = "")
    }

    return(invisible(filepath))

  }, error = function(e) {
    stop("Failed to export profiles CSV for ", park, ": ", e$message)
  })
}


#' Export AUC Results CSV for Single Park
#'
#' Exports AUC metrics (transect_length, overlap, AUC) to CSV for a single park.
#' Includes probability-weighted AUC with uncertainty bounds and error-aware
#' rounding applied for publication quality.
#'
#' Exported columns:
#'   - Identifiers: transect, year
#'   - Transect metrics: transect_length, overlap_m, overlap_pct
#'   - AUC with uncertainty: auc, auc_upper, auc_lower, auc_sigma, auc_uncertainty_pct
#'   - Segmentation: n_segments
#'
#' Rounding applied (CSV only; RDS retains full precision):
#'   - transect_length, overlap_m: Rounded to cm precision (0.01m)
#'   - overlap_pct: Rounded to 1 decimal place
#'   - auc_sigma: Rounded to 2 significant figures
#'   - auc, auc_upper, auc_lower: Rounded to match auc_sigma precision
#'   - auc_uncertainty_pct: Rounded to 1 decimal place
#'
#' @param auc_results Data frame. AUC results with metrics
#' @param data Data frame. Complete dataset (used to get park assignments)
#' @param park Character. Park code (e.g., "GUIS" or "PAIS")
#' @param output_dir Character. Base output directory. Default: current
#'   working directory
#' @param date Character. Date string (YYYYMMDD). Default: today's date
#' @param verbose Logical. Print informational messages. Default: TRUE
#' @return Character. Full path to saved CSV file (invisibly)
#' @examples
#' \dontrun{
#' export_auc_csv(auc_results, data, park = "GUIS")
#' }
#' @importFrom dplyr filter select arrange mutate left_join distinct
#' @importFrom readr write_csv
#' @export
export_auc_csv <- function(auc_results, data, park,
                           output_dir = output_dir,
                           date = format(Sys.Date(), "%Y%m%d"),
                           verbose = TRUE) {

  # Create park-specific directory
  park_dir <- file.path(output_dir, park, "topo")
  if (!dir.exists(park_dir)) {
    dir.create(park_dir, recursive = TRUE)
    if (verbose) cat("Created directory:", park_dir, "\n")
  }

  # Generate filename
  filename <- paste0("auc_", date, ".csv")
  filepath <- file.path(park_dir, filename)

  # Get park assignment for each transect
  transect_park <- data |>
    dplyr::select(transect, park) |>
    dplyr::distinct()

  # Filter and select columns for this park
  # Include all AUC metrics with uncertainty bounds
  park_auc <- auc_results |>
    dplyr::left_join(transect_park, by = "transect") |>
    dplyr::filter(park == !!park) |>
    dplyr::select(
      transect, year,
      transect_length, overlap_m, overlap_pct,
      # AUC with uncertainty bounds
      auc, auc_upper, auc_lower, auc_sigma, auc_uncertainty_pct,
      # Segment information
      n_segments
    ) |>
    dplyr::arrange(transect, year) |>
    # Apply error-aware rounding for CSV export
    dplyr::mutate(
      # Round transect metrics to cm precision (0.01m)
      transect_length = round(transect_length, 2),
      overlap_m = round(overlap_m, 2),
      overlap_pct = round(overlap_pct, 1),

      # Round AUC uncertainty to 2 significant figures
      auc_sigma = round_uncertainty(auc_sigma, sig_figs = 2),

      # Round AUC values to match uncertainty precision
      auc = round_to_uncertainty(auc, auc_sigma),
      auc_upper = round_to_uncertainty(auc_upper, auc_sigma),
      auc_lower = round_to_uncertainty(auc_lower, auc_sigma),

      # Round uncertainty percentage to 1 decimal place
      auc_uncertainty_pct = round(auc_uncertainty_pct, 1)
    )

  # Write CSV
  tryCatch({
    readr::write_csv(park_auc, filepath)

    if (verbose) {
      cat("  AUC:     ", filepath, "\n", sep = "")
      cat("            (", nrow(park_auc), " records)\n", sep = "")
    }

    return(invisible(filepath))

  }, error = function(e) {
    stop("Failed to export AUC CSV for ", park, ": ", e$message)
  })
}


#' Export All Park-Specific CSVs
#'
#' Convenience wrapper to export both profile and AUC CSVs for a single park.
#'
#' @param data Data frame. Complete dataset with all point types
#' @param auc_results Data frame. AUC results with metrics
#' @param park Character. Park code (e.g., "GUIS" or "PAIS")
#' @param output_dir Character. Base output directory. Default: current
#'   working directory
#' @param date Character. Date string (YYYYMMDD). Default: today's date
#' @param verbose Logical. Print informational messages. Default: TRUE
#' @return Named list with paths to exported files (profiles_file, auc_file, park)
#' @examples
#' \dontrun{
#' export_park_files(data, auc_results, park = "GUIS")
#' }
#' @export
export_park_files <- function(data, auc_results, park,
                              output_dir = output_dir,
                              date = format(Sys.Date(), "%Y%m%d"),
                              verbose = TRUE) {

  if (verbose) cat("Exporting", park, "files...\n")

  # Export profiles
  profiles_path <- export_transect_profiles_csv(
    data = data,
    park = park,
    output_dir = output_dir,
    date = date,
    verbose = verbose
  )

  # Export AUC
  auc_path <- export_auc_csv(
    auc_results = auc_results,
    data = data,
    park = park,
    output_dir = output_dir,
    date = date,
    verbose = verbose
  )

  list(
    park = park,
    profiles_file = profiles_path,
    auc_file = auc_path
  )
}


#' Export All Results (RDS and CSVs for All Parks)
#'
#' Master export function that saves the complete RDS file and exports
#' CSV files for all parks present in the data.
#'
#' @param data Data frame. Complete dataset with all point types
#' @param auc_results Data frame. AUC results with metrics
#' @param processed_dir Character. Directory for RDS file. Default: current
#'   working directory
#' @param output_dir Character. Base directory for CSVs. Default: current
#'   working directory
#' @param date Character. Date string (YYYYMMDD). Default: today's date
#' @param verbose Logical. Print informational messages. Default: TRUE
#' @return List with paths to all exported files (rds_file, park_files)
#' @examples
#' \dontrun{
#' export_all_results(data, auc_results)
#' }
#' @export
export_all_results <- function(data, auc_results,
                               processed_dir = processed_dir,
                               output_dir = output_dir,
                               date = format(Sys.Date(), "%Y%m%d"),
                               verbose = TRUE) {

  # Save master RDS
  rds_path <- save_topo_transects_rds(
    data = data,
    auc_results = auc_results,
    output_dir = processed_dir,
    date = date,
    verbose = verbose
  )

  cat("\n")

  # Export park CSVs
  parks <- unique(data$park)
  park_exports <- lapply(parks, function(park) {
    export_park_files(
      data = data,
      auc_results = auc_results,
      park = park,
      output_dir = output_dir,
      date = date,
      verbose = verbose
    )
  })
  names(park_exports) <- parks

  list(
    rds_file = rds_path,
    park_files = park_exports
  )
}


# ============================================================================
# LOAD FUNCTIONS
# ============================================================================

#' Load Topographic Transect Analysis Results from RDS
#'
#' Loads previously saved analysis results from master RDS file.
#' If no filepath is specified, finds the most recent topo_transects RDS file
#' in the input directory.
#'
#' @param filepath Character. Path to RDS file. If NULL, finds most recent file
#'   in input_dir.
#' @param input_dir Character. Directory to search if filepath is NULL.
#'   Default: current working directory
#' @param verbose Logical. Print informational messages. Default: TRUE
#' @return List with components: data, auc_results, metadata
#' @examples
#' \dontrun{
#' # Load most recent file from current directory
#' results <- load_topo_transects_rds()
#'
#' # Load specific file
#' results <- load_topo_transects_rds("path/to/topo_transects_20240101.rds")
#' }
#' @export
load_topo_transects_rds <- function(filepath = NULL,
                                    input_dir = input_dir,
                                    verbose = TRUE) {

  # If no filepath provided, find most recent file
  if (is.null(filepath)) {
    if (!dir.exists(input_dir)) {
      stop("Input directory does not exist: ", input_dir)
    }

    # List topo_transects RDS files
    rds_files <- list.files(input_dir, pattern = "^topo_transects_.*\\.rds$",
                            full.names = TRUE)

    if (length(rds_files) == 0) {
      stop("No topo_transects RDS files found in: ", input_dir)
    }

    # Get most recent file by modification time
    file_info <- file.info(rds_files)
    filepath <- rownames(file_info)[which.max(file_info$mtime)]
  }

  # Check file exists
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }

  # Load RDS file
  tryCatch({
    results <- readRDS(filepath)

    # Validate structure
    required_fields <- c("data", "auc_results", "metadata")
    missing_fields <- setdiff(required_fields, names(results))

    if (length(missing_fields) > 0) {
      stop("Invalid file structure. Missing fields: ",
           paste(missing_fields, collapse = ", "))
    }

    if (verbose) {
      cat("Loaded analysis results from:", filepath, "\n")
      cat("  - Data points:", nrow(results$data), "\n")
      cat("  - AUC records:", nrow(results$auc_results), "\n")
      cat("  - Parks:", paste(results$metadata$parks, collapse = ", "), "\n")
    }

    return(results)

  }, error = function(e) {
    stop("Failed to load RDS file: ", e$message)
  })
}


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' List Available Topo Transect Result Files
#'
#' Lists all topo_transects RDS files in the specified directory with
#' file information (size, modification time).
#'
#' @param input_dir Character. Directory to search.
#' @return Data frame with file information (filename, path, size_mb, modified),
#'   or NULL if no files found
#' @examples
#' \dontrun{
#' list_topo_transect_files()
#' }
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc
#' @export
list_topo_transect_files <- function(input_dir = input_dir) {

  if (!dir.exists(input_dir)) {
    return(invisible(NULL))
  }

  # List topo_transects RDS files
  rds_files <- list.files(input_dir, pattern = "^topo_transects_.*\\.rds$",
                          full.names = TRUE)

  if (length(rds_files) == 0) {
    return(invisible(NULL))
  }

  file_info <- file.info(rds_files)

  results <- tibble::tibble(
    filename = basename(rds_files),
    path = rds_files,
    size_mb = round(file_info$size / 1024^2, 2),
    modified = file_info$mtime
  ) |>
    dplyr::arrange(dplyr::desc(modified))

  return(results)
}
