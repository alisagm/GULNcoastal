# transect_analysis.R
# ============================================================================
# TRANSECT ANALYSIS WORKFLOW WRAPPER
# ============================================================================

#' Run Complete Transect Analysis Pipeline
#'
#' Executes the full transect analysis workflow from raw CSV import through
#' probability-weighted AUC calculation with uncertainty propagation.
#'
#' @section Pipeline Steps:
#' \enumerate{
#'   \item Import raw topography data from CSV files
#'   \item Clean and deduplicate observations
#'   \item Load accuracy tables (measurement uncertainty by year/park)
#'   \item Assign year-specific accuracy values (sigma_h, sigma_v)
#'   \item Identify zero-elevation crossings with uncertainty quantification
#'   \item Calculate common minimum distances for cross-island transects
#'   \item Calculate probability-weighted AUC with error propagation
#' }
#'
#' @param data_dir Character or NULL. Path to directory containing raw transect CSV
#'   files. Each CSV must have columns: transect, year, distance, elevation.
#'   Park is inferred from filename (must contain "GUIS" or "PAIS").
#' @param data Dataframe or NULL. Pre-loaded raw transect data.
#' @param processed_dir Character. Path to directory to save intermediate
#'   result data files. Created if it doesn't exist.
#' @param output_dir Character. Path to save results. Default is current
#'   working directory. Created if it doesn't exist.
#' @param accuracy_table_path Character or NULL. Path to accuracy_values.csv
#'   file containing measurement uncertainty by park/year. If NULL (default),
#'   looks for "accuracy_values.csv" in \code{data_dir}.
#' @param special_cases_path Character or NULL. Path to special_cases.csv
#'   file containing transect-specific accuracy overrides. If NULL (default),
#'   looks for "special_cases.csv" in \code{data_dir}. This file is optional.
#' @param save_results Logical. If TRUE (default), saves results using
#'  \code{export_all_results()}.
#' @param verbose Logical. If TRUE, prints progress messages. Default FALSE.
#'
#' @return A list (returned invisibly) containing:
#'   \describe{
#'     \item{data}{Data frame. Final processed transect data with all point
#'       types (measured, interpolated, extrapolated, common_min), measurement
#'       accuracy, and uncertainty values.}
#'     \item{auc_results}{Data frame. AUC results by transect-year with columns:
#'       transect, year, auc, auc_upper, auc_lower, auc_sigma, auc_uncertainty_pct,
#'       overlap_m, overlap_pct, transect_length, n_segments, segment_info, segments.}
#'     \item{metadata}{List. Processing metadata including timestamp, counts,
#'       parks, years, and workflow version.}
#'   }
#'
#' @section Accuracy Table Format:
#' The accuracy_values.csv file must contain columns:
#' \itemize{
#'   \item park: Park code (e.g., "GUIS", "PAIS")
#'   \item year: Survey year
#'   \item sigma_h: Horizontal measurement uncertainty (meters)
#'   \item sigma_v: Vertical measurement uncertainty (meters)
#' }
#'
#' @section Special Cases Format:
#' The optional special_cases.csv file allows transect-specific overrides:
#' \itemize{
#'   \item park: Park code
#'   \item transect: Transect identifier
#'   \item year: Survey year (or "all" for all years)
#'   \item sigma_h: Override horizontal uncertainty
#'   \item sigma_v: Override vertical uncertainty
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with accuracy table in data directory
#' results <- run_transect_analysis(
#'   data_dir = "path/to/raw/data",
#'   output_dir = "path/to/output"
#' )
#'
#' # With accuracy tables in separate location
#' results <- run_transect_analysis(
#'   data_dir = "path/to/raw/data",
#'   output_dir = "path/to/output",
#'   accuracy_table_path = "config/accuracy_values.csv",
#'   special_cases_path = "config/special_cases.csv"
#' )
#'
#' # Access results
#' head(results$auc_results)
#' summary(results$data)
#'
#' # Standard usage: directory-based
#' results <- run_transect_analysis(
#'   data_dir = "data/raw",
#'   output_dir = "data/output"
#' )
#'
#' # Flexible usage: with pre-loaded data (new in v0.2.0)
#' data <- import_transects_park("custom_file.csv")
#' # ... clean/fix data as needed ...
#' results <- run_transect_analysis(
#'   data = data,
#'   accuracy_table_path = "accuracy_values.csv"
#' )
#' }
#'
#' @seealso
#' \code{\link{run_transect_analysis_data}} for flexible version with pre-loaded data
#' \code{\link{import_transects_park}} for data import details
#' \code{\link{assign_accuracy}} for accuracy assignment
#' \code{\link{load_accuracy_table}} for accuracy table loading
#' \code{\link{calculate_auc_with_uncertainty}} for AUC calculation
#' \code{\link{export_all_results}} for output format
#'
#' @importFrom dplyr mutate filter select arrange group_by ungroup slice
#'   bind_rows anti_join left_join rename first
#' @importFrom stats setNames
#' @export
run_transect_analysis <- function(data_dir = NULL,
                                  data = NULL,
                                  processed_dir = ".",
                                  output_dir = ".",
                                  accuracy_table_path = NULL,
                                  special_cases_path = NULL,
                                  save_results = TRUE,
                                  verbose = FALSE) {

  # ──────────────────────────────────────────────────────────────────────────

  # Allow either data_dir OR data (not both)
  if (is.null(data_dir) && is.null(data)) {
    stop("Either data_dir or data must be provided", call. = FALSE)
  }

  if (!is.null(data_dir) && !is.null(data)) {
    stop("Provide either data_dir OR data, not both", call. = FALSE)
  }

  # If data provided, use flexible version
  if (!is.null(data)) {
    return(run_transect_analysis_data(
      data = data,
      accuracy_table = accuracy_table_path,
      special_cases = special_cases_path,
      save_results = save_results,
      processed_dir = processed_dir,
      output_dir = output_dir,
      verbose = verbose
    ))
  }

  # Validate data directory
  if (!dir.exists(data_dir) && is.null(data)) {
    stop("Data directory does not exist: ", data_dir, call. = FALSE)
  }

  data_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

  # Exclude accuracy table files from data files
  accuracy_patterns <- c("accuracy_values", "special_cases")
  data_files <- data_files[!grepl(paste(accuracy_patterns, collapse = "|"),
                                  basename(data_files), ignore.case = TRUE)]

  if (length(data_files) == 0) {
    stop("No CSV data files found in: ", data_dir,
         "\n  (accuracy_values.csv and special_cases.csv are excluded)",
         call. = FALSE)
  }

  # Resolve accuracy table paths
  if (is.null(accuracy_table_path)) {
    accuracy_table_path <- file.path(data_dir, "accuracy_values.csv")
  }

  if (is.null(special_cases_path)) {
    special_cases_path <- file.path(data_dir, "special_cases.csv")
  }

  # Validate accuracy table exists (required)
  if (!file.exists(accuracy_table_path)) {
    stop("Accuracy table not found: ", accuracy_table_path,
         "\n  Provide path via accuracy_table_path parameter, or",
         "\n  place accuracy_values.csv in data_dir.",
         call. = FALSE)
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 1: Import data
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Loading data files...\n")
  if (verbose) cat("  Found ", length(data_files), " data file(s)\n")

  # Track import failures
  failed_imports <- character()
  import_errors <- list()

  data_raw <- lapply(data_files, function(f) {
    tryCatch({
      import_transects_park(f)
    }, error = function(e) {
      failed_imports <<- c(failed_imports, basename(f))
      import_errors[[basename(f)]] <<- e$message
      return(NULL)
    })
  })

  # Filter out NULLs and combine
  data_raw <- Filter(Negate(is.null), data_raw)
  data_raw <- dplyr::bind_rows(data_raw)

  # Report import failures
  if (length(failed_imports) > 0) {
    warning("Failed to import ", length(failed_imports), " file(s): ",
            paste(failed_imports, collapse = ", "),
            "\n  First error: ", import_errors[[1]],
            call. = FALSE)
    if (verbose) {
      cat("  Import errors:\n")
      for (file in failed_imports) {
        cat("    ", file, ": ", import_errors[[file]], "\n", sep = "")
      }
    }
  }

  # Validate imported data
  if (nrow(data_raw) == 0) {
    stop("No data successfully imported from ", data_dir,
         "\n  All ", length(data_files), " file(s) failed to import.",
         call. = FALSE)
  }

  # Validate required columns
  required_cols <- c("transect", "year", "distance", "elevation", "park")
  missing_cols <- setdiff(required_cols, names(data_raw))
  if (length(missing_cols) > 0) {
    stop("Imported data is missing required columns: ",
         paste(missing_cols, collapse = ", "),
         "\n  Available columns: ", paste(names(data_raw), collapse = ", "),
         call. = FALSE)
  }

  if (verbose) {
    cat("  Successfully imported ", nrow(data_raw), " observations\n")
    cat("  Transects: ", length(unique(data_raw$transect)), "\n")
    cat("  Years: ", paste(sort(unique(data_raw$year)), collapse = ", "), "\n")
    cat("  Parks: ", paste(unique(data_raw$park), collapse = ", "), "\n")
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 2: Clean and deduplicate
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Cleaning and deduplicating data...\n")

  data_typed <- dplyr::mutate(data_raw, point_type = "measured")

  # Use group_split() + lapply() to avoid memory accumulation
  data_clean <- data_typed |>
    dplyr::group_by(transect, year, cross_island, park) |>
    dplyr::group_split() |>
    lapply(remove_negatives) |>
    dplyr::bind_rows()

  # Validate cleaning didn't remove all data
  if (nrow(data_clean) == 0) {
    stop("All observations were removed during cleaning.",
         "\n  Original observations: ", nrow(data_typed),
         call. = FALSE)
  }

  data_deduplicated <- data_clean |>
    dplyr::group_by(transect, year, distance) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  # Report deduplication statistics
  n_duplicates <- nrow(data_clean) - nrow(data_deduplicated)
  if (n_duplicates > 0 && verbose) {
    cat("  Removed ", n_duplicates, " duplicate observation(s) (",
        round(100 * n_duplicates / nrow(data_clean), 1), "%)\n", sep = "")
  }

  if (verbose) cat("  Cleaned data: ", nrow(data_deduplicated), " observations\n")

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 3: Load accuracy tables
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Loading accuracy tables...\n")

  accuracy_table <- load_accuracy_table(accuracy_table_path)

  if (verbose) {
    cat("  Loaded accuracy table: ", nrow(accuracy_table),
        " park/year combinations\n", sep = "")
  }

  # Load special cases table (optional)
  special_cases <- NULL
  if (file.exists(special_cases_path)) {
    special_cases <- load_special_cases_table(special_cases_path)
    if (verbose) {
      cat("  Loaded special cases: ", nrow(special_cases),
          " transect-specific overrides\n", sep = "")
    }
  } else {
    if (verbose) {
      cat("  No special cases file found (optional)\n")
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 4: Assign accuracy values
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Assigning year-specific accuracy values...\n")

  data_with_accuracy <- assign_accuracy(
    data_deduplicated,
    accuracy_table = accuracy_table,
    special_cases = special_cases,
    verbose = verbose
  )

  # Validate accuracy assignment
  if (!"sigma_h" %in% names(data_with_accuracy) ||
      !"sigma_v" %in% names(data_with_accuracy)) {
    stop("assign_accuracy() did not add required columns: sigma_h, sigma_v",
         "\n  Available columns: ",
         paste(names(data_with_accuracy), collapse = ", "),
         call. = FALSE)
  }

  # Check for missing accuracy values
  n_missing_accuracy <- sum(is.na(data_with_accuracy$sigma_h) |
                              is.na(data_with_accuracy$sigma_v))
  if (n_missing_accuracy > 0) {
    warning("Missing accuracy values for ", n_missing_accuracy,
            " observation(s) (",
            round(100 * n_missing_accuracy / nrow(data_with_accuracy), 1), "%)",
            call. = FALSE)
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 5: Identify zero-elevation points
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Finding zero crossings with uncertainty...\n")

  zero_points_all <- identify_zero_points_all_transects(data_with_accuracy)

  # Validate zero points
  if (!is.data.frame(zero_points_all)) {
    stop("identify_zero_points_all_transects() did not return a data frame",
         call. = FALSE)
  }

  if (verbose && nrow(zero_points_all) > 0) {
    zero_type_counts <- table(zero_points_all$point_type)
    cat("  Found ", nrow(zero_points_all), " zero-crossing point(s):\n", sep = "")
    for (type in names(zero_type_counts)) {
      cat("    ", type, ": ", zero_type_counts[type], "\n", sep = "")
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 6: Prepare data for AUC calculation
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Preparing AUC data...\n")

  # 6a. Reclassify measured points and add uncertainty/confidence
  data_classified <- add_measured_uncertainty(data_with_accuracy)

  # 6b. Add interpolated and extrapolated zero-crossings
  # (do NOT add measured_zero - they're already in data_classified)
  data_with_zeros <- dplyr::bind_rows(
    data_classified,
    dplyr::filter(zero_points_all,
                  point_type %in% c("interpolated", "extrapolated"))
  ) |>
    dplyr::arrange(transect, year, distance)

  # 6c. Calculate common minimum distances
  if (verbose) cat("Calculating common minima...\n")

  common_mins <- calculate_and_interpolate_common_min(
    data_with_zeros,
    verbose = verbose
  )

  # Validate common_mins result
  if (!is.data.frame(common_mins)) {
    stop("calculate_and_interpolate_common_min() did not return a data frame",
         call. = FALSE)
  }

  if (verbose && nrow(common_mins) > 0) {
    cat("  Added ", nrow(common_mins), " common minimum point(s)\n", sep = "")
  }

  # 6d. Add common_min points (remove duplicates first)
  if (nrow(common_mins) > 0) {
    # Validate common_mins has required columns
    required_cm_cols <- c("transect", "year", "distance")
    missing_cm_cols <- setdiff(required_cm_cols, names(common_mins))
    if (length(missing_cm_cols) > 0) {
      stop("common_mins is missing required columns: ",
           paste(missing_cm_cols, collapse = ", "),
           call. = FALSE)
    }

    data_auc_ready <- data_with_zeros |>
      dplyr::anti_join(
        dplyr::select(common_mins, transect, year, distance),
        by = c("transect", "year", "distance")
      ) |>
      dplyr::bind_rows(common_mins) |>
      dplyr::arrange(transect, year, distance)
  } else {
    data_auc_ready <- data_with_zeros
  }

  if (verbose) cat("  Total points for AUC: ", nrow(data_auc_ready), "\n")

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 7: Calculate probability-weighted AUC with uncertainty
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Calculating probability-weighted AUC with uncertainty...\n")

  # Use group_split() + lapply() for memory efficiency
  auc_results_weighted <- data_auc_ready |>
    dplyr::group_by(transect, year) |>
    dplyr::group_split() |>
    lapply(function(df) {
      result <- calculate_auc_with_uncertainty(df)
      result$transect <- dplyr::first(df$transect)
      result$year <- dplyr::first(df$year)
      return(result)
    }) |>
    dplyr::bind_rows()

  # Validate AUC results
  if (nrow(auc_results_weighted) == 0) {
    stop("AUC calculation returned no results",
         "\n  Input data had ", nrow(data_auc_ready), " observations across ",
         length(unique(data_auc_ready$transect)), " transects",
         call. = FALSE)
  }

  required_auc_cols <- c("transect", "year", "auc_nominal", "auc_uncertainty")
  missing_auc_cols <- setdiff(required_auc_cols, names(auc_results_weighted))
  if (length(missing_auc_cols) > 0) {
    stop("AUC results missing required columns: ",
         paste(missing_auc_cols, collapse = ", "),
         "\n  Available columns: ",
         paste(names(auc_results_weighted), collapse = ", "),
         call. = FALSE)
  }

  if (verbose) {
    cat("  Calculated AUC for ", nrow(auc_results_weighted),
        " transect-year combination(s)\n", sep = "")
  }

  # Calculate transect geometry metrics
  transect_metrics <- calculate_transect_metrics(data_auc_ready)

  if (nrow(transect_metrics) == 0) {
    warning("Transect metrics calculation returned no results", call. = FALSE)
  }

  # Combine weighted AUC with transect metrics
  auc_results <- auc_results_weighted |>
    dplyr::left_join(transect_metrics, by = c("transect", "year")) |>
    dplyr::rename(
      auc = auc_nominal,
      auc_sigma = auc_uncertainty
    ) |>
    dplyr::select(
      transect, year,
      auc, auc_upper, auc_lower, auc_sigma, auc_uncertainty_pct,
      overlap_m, overlap_pct, transect_length,
      n_segments, segment_info, segments
    )

  # ──────────────────────────────────────────────────────────────────────────
  # Package and return results
  # ──────────────────────────────────────────────────────────────────────────

  results <- list(
    data = data_auc_ready,
    auc_results = auc_results,
    metadata = list(
      timestamp = Sys.time(),
      date = Sys.Date(),
      n_transects = length(unique(data_auc_ready$transect)),
      n_years = length(unique(data_auc_ready$year)),
      n_observations = nrow(data_auc_ready),
      parks = sort(unique(data_auc_ready$park)),
      years = sort(unique(data_auc_ready$year)),
      workflow_version = "v3_weighted_auc",
      accuracy_table = accuracy_table_path,
      special_cases = if (file.exists(special_cases_path)) special_cases_path else NULL,
      notes = paste("Probability-weighted AUC with uncertainty propagation;",
                    "error-aware tolerances throughout")
    )
  )

  # Save if requested
  if (save_results) {
    export_all_results(
      data = results$data,
      auc_results = results$auc_results,
      processed_dir = processed_dir,
      output_dir = output_dir,
      date = format(Sys.Date(), "%Y%m%d"),
      verbose = verbose
    )
  }

  return(invisible(results))
}

#' Run Transect Analysis on Pre-Loaded Data
#'
#' Flexible version of run_transect_analysis() that accepts data frames
#' instead of file paths. Use this when you need to:
#' - Combine data from multiple sources
#' - Clean/fix data before analysis
#' - Use custom import logic
#' - Debug individual pipeline steps
#'
#' @param data Data frame with columns: transect, year, distance, elevation, park
#' @param accuracy_table Data frame with accuracy values (or path to CSV)
#' @param special_cases Data frame with special cases (or path to CSV, or NULL)
#' @param save_results Logical. Save outputs to processed_dir/output_dir
#' @param processed_dir Directory for RDS output
#' @param output_dir Directory for CSV outputs
#' @param verbose Logical. Print progress messages
#'
#' @return List with data, auc_results, metadata
#'
#' @examples
#' \dontrun{
#' # Import and clean data YOUR way
#' data1 <- import_transects_park("file1.csv")
#' data2 <- import_transects_park("file2.csv")
#'
#' # Fix issues
#' data1$year[data1$year == 2017] <- 2016
#' data1$transect <- str_remove(data1$transect, "^t0*")
#'
#' # Combine
#' data <- bind_rows(data1, data2)
#'
#' # Run analysis on cleaned data
#' results <- run_transect_analysis_data(
#'   data = data,
#'   accuracy_table = "accuracy_values.csv",
#'   save_results = TRUE
#' )
#' }
#'
#'@seealso
#' \code{\link{run_transect_analysis}} for full pipeline including data import
#' \code{\link{import_transects_park}} for data import details
#' \code{\link{assign_accuracy}} for accuracy assignment
#' \code{\link{calculate_auc_with_uncertainty}} for AUC calculation
#' \code{\link{export_all_results}} for output format
#'
#' @export
run_transect_analysis_data <- function(
    data,
    accuracy_table,
    special_cases = NULL,
    save_results = TRUE,
    processed_dir = ".",
    output_dir = ".",
    verbose = FALSE) {

  # Validate input data
  required_cols <- c("transect", "year", "distance", "elevation", "park")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Load accuracy table if path provided
  if (is.character(accuracy_table)) {
    accuracy_table <- load_accuracy_table(accuracy_table)
  }

  # Load special cases if path provided
  if (is.character(special_cases)) {
    special_cases <- load_special_cases_table(special_cases)
  }

  # Mark as measured points
  data_typed <- dplyr::mutate(data, point_type = "measured")

  # Clean and deduplicate
  if (verbose) cat("Cleaning and deduplicating data...\n")

  data_clean <- data_typed |>
    dplyr::group_by(transect, year, cross_island, park) |>
    dplyr::group_split() |>
    lapply(remove_negatives) |>
    dplyr::bind_rows()

  data_deduplicated <- data_clean |>
    dplyr::group_by(transect, year, distance) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  # Assign accuracy
  if (verbose) cat("Assigning accuracy values...\n")

  data_with_accuracy <- assign_accuracy(
    data_deduplicated,
    accuracy_table = accuracy_table,
    special_cases = special_cases,
    verbose = verbose
  )

  # Identify zero points
  if (verbose) cat("Finding zero crossings...\n")

  zero_points_all <- identify_zero_points_all_transects(data_with_accuracy)

  # Prepare for AUC
  if (verbose) cat("Preparing AUC data...\n")

  data_classified <- add_measured_uncertainty(data_with_accuracy)

  data_with_zeros <- dplyr::bind_rows(
    data_classified,
    dplyr::filter(zero_points_all,
                  point_type %in% c("interpolated", "extrapolated"))
  ) |>
    dplyr::arrange(transect, year, distance)

  # Calculate common minima
  if (verbose) cat("Calculating common minima...\n")

  common_mins <- calculate_and_interpolate_common_min(
    data_with_zeros,
    verbose = verbose
  )

  # Add common min points
  if (nrow(common_mins) > 0) {
    data_auc_ready <- data_with_zeros |>
      dplyr::anti_join(
        dplyr::select(common_mins, transect, year, distance),
        by = c("transect", "year", "distance")
      ) |>
      dplyr::bind_rows(common_mins) |>
      dplyr::arrange(transect, year, distance)
  } else {
    data_auc_ready <- data_with_zeros
  }

  # Calculate AUC with uncertainty
  if (verbose) cat("Calculating probability-weighted AUC...\n")

  auc_results_weighted <- data_auc_ready |>
    dplyr::group_by(transect, year) |>
    dplyr::group_split() |>
    lapply(function(df) {
      result <- calculate_auc_with_uncertainty(df)
      result$transect <- dplyr::first(df$transect)
      result$year <- dplyr::first(df$year)
      return(result)
    }) |>
    dplyr::bind_rows()

  # Calculate transect metrics
  transect_metrics <- calculate_transect_metrics(data_auc_ready)

  # Combine results
  auc_results <- auc_results_weighted |>
    dplyr::left_join(transect_metrics, by = c("transect", "year")) |>
    dplyr::rename(
      auc = auc_nominal,
      auc_sigma = auc_uncertainty
    ) |>
    dplyr::select(
      transect, year,
      auc, auc_upper, auc_lower, auc_sigma, auc_uncertainty_pct,
      overlap_m, overlap_pct, transect_length,
      n_segments, segment_info, segments
    )

  # Package results
  results <- list(
    data = data_auc_ready,
    auc_results = auc_results,
    metadata = list(
      timestamp = Sys.time(),
      date = Sys.Date(),
      n_transects = length(unique(data_auc_ready$transect)),
      n_years = length(unique(data_auc_ready$year)),
      n_observations = nrow(data_auc_ready),
      parks = sort(unique(data_auc_ready$park)),
      years = sort(unique(data_auc_ready$year)),
      workflow_version = "v3_weighted_auc",
      notes = "Probability-weighted AUC with uncertainty propagation"
    )
  )

  # Save if requested
  if (save_results) {
    if (verbose) cat("Saving results...\n")

    export_all_results(
      data = results$data,
      auc_results = results$auc_results,
      processed_dir = processed_dir,
      output_dir = output_dir,
      date = format(Sys.Date(), "%Y%m%d"),
      verbose = verbose
    )
  }

  return(invisible(results))
}
