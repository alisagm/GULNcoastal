# transects_pairwise.R
# ============================================================================
# PAIRWISE TRANSECT ANALYSIS WORKFLOW
# ============================================================================

#' Run Pairwise Transect Analysis for Non-Cross-Island Transects
#'
#' Executes pairwise transect analysis with year-specific common minimum
#' distances for non-cross-island transects. For each transect, excludes the
#' year with the highest minimum distance and calculates AUC for all unique
#' pairs of the remaining years.
#'
#' @section Pairwise Analysis Logic:
#' \enumerate{
#'   \item Filter to only non-cross-island transects (cross_island = FALSE)
#'   \item For each transect, identify the year with the highest minimum distance
#'   \item Exclude that year from pairwise analysis
#'   \item Generate all unique pairs from remaining years (e.g., Y1-Y2, Y1-Y3, Y2-Y3)
#'   \item For each pair, calculate common minimum distance (max of the two yearly minimums)
#'   \item Calculate probability-weighted AUC for each year in each pair
#' }
#'
#' @section Key Differences from Standard Analysis:
#' Unlike \code{\link{run_transect_analysis}}, which calculates a single common
#' minimum distance across all years, this function:
#' \itemize{
#'   \item Only processes non-cross-island transects
#'   \item Calculates unique common minimum for each year pair
#'   \item Excludes the year with highest minimum distance
#'   \item Returns results organized by pair_id
#' }
#'
#' @param data_dir Character. Path to directory containing raw transect CSV
#'   files. Each CSV must have columns: transect, year, distance, elevation.
#'   Park is inferred from filename (must contain "GUIS" or "PAIS").
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
#' @param save_results Logical. If TRUE (default), saves results as RDS files
#'   per park: {park_code}_pairwise_results.rds
#' @param verbose Logical. If TRUE, prints progress messages. Default FALSE.
#'
#' @return A list (returned invisibly) containing:
#'   \describe{
#'     \item{data}{Data frame. Final processed transect data for all pairs with
#'       pair_id column identifying which pair each observation belongs to.}
#'     \item{auc_results}{Data frame. AUC results by transect-year-pair with columns:
#'       transect, year, pair_id, year1, year2, auc, auc_upper, auc_lower, auc_sigma,
#'       auc_uncertainty_pct, overlap_m, overlap_pct, transect_length, n_segments,
#'       segment_info, segments.}
#'     \item{metadata}{List. Processing metadata including:
#'       \itemize{
#'         \item timestamp: When analysis was run
#'         \item excluded_years: Data frame showing which year was excluded per transect
#'         \item pair_summary: Data frame with pair counts per transect
#'         \item n_transects: Total number of transects analyzed
#'         \item n_pairs: Total number of pairs analyzed
#'         \item parks: Parks included
#'         \item workflow_version: "pairwise_v1"
#'       }}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- run_pairwise_analysis(
#'   data_dir = "path/to/raw/data",
#'   output_dir = "path/to/output"
#' )
#'
#' # Access results
#' head(results$auc_results)
#' results$metadata$excluded_years
#' results$metadata$pair_summary
#' }
#'
#' @seealso
#' \code{\link{run_transect_analysis}} for standard (all-years) analysis
#' \code{\link{calculate_pairwise_common_min}} for pair-specific common minimum
#'
#' @importFrom dplyr mutate filter select arrange group_by ungroup slice
#'   bind_rows anti_join left_join rename first summarise
#' @importFrom stats setNames
#' @importFrom utils combn
#' @export
run_pairwise_analysis <- function(data_dir,
                                  processed_dir = ".",
                                  output_dir = ".",
                                  accuracy_table_path = NULL,
                                  special_cases_path = NULL,
                                  save_results = TRUE,
                                  verbose = FALSE) {


  # ──────────────────────────────────────────────────────────────────────────
  # VALIDATE DIRECTORIES AND INPUTS
  # ──────────────────────────────────────────────────────────────────────────

  if (!dir.exists(data_dir)) {
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
  required_cols <- c("transect", "year", "distance", "elevation", "park", "cross_island")
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
  # STEP 1.5: Filter to non-cross-island transects only
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Filtering to non-cross-island transects (cross_island = FALSE)...\n")

  # Handle NA values in cross_island
  if (any(is.na(data_raw$cross_island))) {
    n_na <- sum(is.na(data_raw$cross_island))
    warning(sprintf("Found %d rows with NA in cross_island column. These will be excluded.", n_na),
            call. = FALSE)
    data_raw <- data_raw |> dplyr::filter(!is.na(cross_island))
  }

  data_filtered <- data_raw |> dplyr::filter(cross_island == FALSE)

  if (nrow(data_filtered) == 0) {
    stop("No non-cross-island transects found in data.",
         "\n  All transects have cross_island = TRUE or NA.",
         call. = FALSE)
  }

  if (verbose) {
    n_removed <- nrow(data_raw) - nrow(data_filtered)
    cat("  Filtered to ", nrow(data_filtered), " observations from ",
        length(unique(data_filtered$transect)), " non-cross-island transects\n",
        sep = "")
    if (n_removed > 0) {
      cat("  Removed ", n_removed, " observations from cross-island transects\n", sep = "")
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 2: Clean and deduplicate
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Cleaning and deduplicating data...\n")

  data_typed <- dplyr::mutate(data_filtered, point_type = "measured")

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

  zero_points_all <- find_zero_crossings(data_with_accuracy)

  # Validate zero points
  if (!is.data.frame(zero_points_all)) {
    stop("find_zero_crossings() did not return a data frame",
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
  # STEP 6: Determine which year to exclude per transect
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Determining years to exclude (highest minimum distance per transect)...\n")

  # Calculate minimum distance per transect-year
  year_min_distances <- data_with_accuracy |>
    dplyr::group_by(transect, year) |>
    dplyr::summarise(
      min_distance = min(distance, na.rm = TRUE),
      .groups = 'drop'
    )

  # Identify year with highest minimum distance per transect
  excluded_years <- year_min_distances |>
    dplyr::group_by(transect) |>
    dplyr::slice_max(min_distance, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::rename(excluded_year = year)

  if (verbose) {
    cat("  Excluded years summary:\n")
    for (i in 1:nrow(excluded_years)) {
      cat("    Transect ", excluded_years$transect[i],
          ": Year ", excluded_years$excluded_year[i],
          " (min_distance = ", round(excluded_years$min_distance[i], 2), "m)\n",
          sep = "")
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 7: Generate pairwise combinations for each transect
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Generating pairwise year combinations...\n")

  # Get available years per transect after exclusion
  available_years <- year_min_distances |>
    dplyr::left_join(excluded_years, by = "transect") |>
    dplyr::filter(year != excluded_year) |>
    dplyr::select(transect, year, min_distance)

  # Generate all unique pairs per transect
  pair_list <- available_years |>
    dplyr::group_by(transect) |>
    dplyr::group_split() |>
    lapply(function(df) {
      transect_id <- unique(df$transect)
      years <- sort(df$year)

      # Need at least 2 years for a pair
      if (length(years) < 2) {
        if (verbose) {
          cat("  Skipping transect ", transect_id,
              ": only ", length(years), " year(s) after exclusion\n", sep = "")
        }
        return(NULL)
      }

      # Generate all unique combinations
      pairs <- combn(years, 2, simplify = FALSE)

      # Create data frame with pair information
      pairs_df <- do.call(rbind, lapply(pairs, function(pair) {
        data.frame(
          transect = transect_id,
          year1 = pair[1],
          year2 = pair[2],
          pair_id = paste0("Y", pair[1], "_Y", pair[2]),
          stringsAsFactors = FALSE
        )
      }))

      return(pairs_df)
    })

  # Combine all pairs, removing NULL entries (transects with < 2 years)
  pair_list <- Filter(Negate(is.null), pair_list)

  if (length(pair_list) == 0) {
    stop("No valid pairs found. All transects have fewer than 2 years after exclusion.",
         call. = FALSE)
  }

  pairs_df <- dplyr::bind_rows(pair_list)

  if (verbose) {
    cat("  Generated ", nrow(pairs_df), " unique pairs across ",
        length(unique(pairs_df$transect)), " transects\n", sep = "")
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 8: Calculate AUC for each pair
  # ──────────────────────────────────────────────────────────────────────────

  if (verbose) cat("Calculating pairwise common minima and AUC...\n")

  # Prepare data for AUC (add uncertainty to measured points)
  data_classified <- add_measured_uncertainty(data_with_accuracy)

  # Add interpolated and extrapolated zero-crossings
  data_with_zeros <- dplyr::bind_rows(
    data_classified,
    dplyr::filter(zero_points_all,
                  point_type %in% c("interpolated", "extrapolated"))
  ) |>
    dplyr::arrange(transect, year, distance)

  # Process each pair
  all_pair_results <- lapply(1:nrow(pairs_df), function(i) {
    pair_info <- pairs_df[i, ]
    transect_id <- pair_info$transect
    year1 <- pair_info$year1
    year2 <- pair_info$year2
    pair_id <- pair_info$pair_id

    if (verbose) {
      cat("  Processing pair ", i, "/", nrow(pairs_df), ": ",
          transect_id, " - ", pair_id, "\n", sep = "")
    }

    # Filter data to just this transect and these two years
    pair_data <- data_with_zeros |>
      dplyr::filter(transect == transect_id, year %in% c(year1, year2))

    if (nrow(pair_data) == 0) {
      warning("No data found for transect ", transect_id,
              " pair ", pair_id, call. = FALSE)
      return(NULL)
    }

    # Calculate pairwise common minimum for this pair
    common_mins <- calculate_pairwise_common_min(
      pair_data,
      year1 = year1,
      year2 = year2,
      verbose = verbose
    )

    # Add common_min points to data
    if (nrow(common_mins) > 0) {
      pair_data_auc_ready <- pair_data |>
        dplyr::anti_join(
          dplyr::select(common_mins, transect, year, distance),
          by = c("transect", "year", "distance")
        ) |>
        dplyr::bind_rows(common_mins) |>
        dplyr::arrange(transect, year, distance)
    } else {
      pair_data_auc_ready <- pair_data
    }

    # Calculate AUC for each year in the pair
    auc_results <- pair_data_auc_ready |>
      dplyr::group_by(transect, year) |>
      dplyr::group_split() |>
      lapply(function(df) {
        result <- calculate_auc_with_uncertainty(df)
        result$transect <- dplyr::first(df$transect)
        result$year <- dplyr::first(df$year)
        return(result)
      }) |>
      dplyr::bind_rows()

    # Add pair information
    auc_results <- auc_results |>
      dplyr::mutate(
        pair_id = pair_id,
        year1 = year1,
        year2 = year2
      )

    # Calculate transect metrics
    transect_metrics <- calculate_transect_metrics(pair_data_auc_ready)

    # Combine AUC with metrics
    auc_with_metrics <- auc_results |>
      dplyr::left_join(transect_metrics, by = c("transect", "year")) |>
      dplyr::rename(
        auc = auc_nominal,
        auc_sigma = auc_uncertainty
      ) |>
      dplyr::select(
        transect, year, pair_id, year1, year2,
        auc, auc_upper, auc_lower, auc_sigma, auc_uncertainty_pct,
        overlap_m, overlap_pct, transect_length,
        n_segments, segment_info, segments
      )

    # Add pair_id to data as well
    pair_data_auc_ready <- pair_data_auc_ready |>
      dplyr::mutate(
        pair_id = pair_id,
        year1 = year1,
        year2 = year2
      )

    return(list(
      data = pair_data_auc_ready,
      auc_results = auc_with_metrics
    ))
  })

  # Remove NULL results
  all_pair_results <- Filter(Negate(is.null), all_pair_results)

  if (length(all_pair_results) == 0) {
    stop("No pairs successfully processed", call. = FALSE)
  }

  # Combine all data and results
  combined_data <- dplyr::bind_rows(lapply(all_pair_results, function(x) x$data))
  combined_auc_results <- dplyr::bind_rows(lapply(all_pair_results, function(x) x$auc_results))

  if (verbose) {
    cat("  Calculated AUC for ", nrow(combined_auc_results),
        " transect-year-pair combination(s)\n", sep = "")
  }

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 9: Generate metadata and summary statistics
  # ──────────────────────────────────────────────────────────────────────────

  # Pair summary per transect
  pair_summary <- available_years |>
    dplyr::group_by(transect) |>
    dplyr::summarise(
      n_years_total = n() + 1,  # +1 for excluded year
      n_years_after_exclusion = n(),
      n_pairs = choose(n(), 2),
      .groups = 'drop'
    )

  # Overall metadata
  metadata <- list(
    timestamp = Sys.time(),
    date = Sys.Date(),
    n_transects = length(unique(combined_data$transect)),
    n_pairs = nrow(pairs_df),
    n_observations = nrow(combined_data),
    parks = sort(unique(combined_data$park)),
    excluded_years = excluded_years,
    pair_summary = pair_summary,
    workflow_version = "pairwise_v1",
    accuracy_table = accuracy_table_path,
    special_cases = if (file.exists(special_cases_path)) special_cases_path else NULL,
    notes = paste("Pairwise analysis with year-specific common minima;",
                  "excludes year with highest minimum distance per transect")
  )

  # ──────────────────────────────────────────────────────────────────────────
  # Package results
  # ──────────────────────────────────────────────────────────────────────────

  results <- list(
    data = combined_data,
    auc_results = combined_auc_results,
    metadata = metadata
  )

  # ──────────────────────────────────────────────────────────────────────────
  # STEP 10: Save results if requested
  # ──────────────────────────────────────────────────────────────────────────

  if (save_results) {
    if (verbose) cat("Saving results...\n")

    # Create output directories if needed
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Save per park
    parks <- unique(combined_data$park)

    for (park in parks) {
      # Filter results for this park
      park_data <- combined_data |> dplyr::filter(park == !!park)
      park_auc <- combined_auc_results |>
        dplyr::filter(transect %in% unique(park_data$transect))

      # Create park-specific metadata
      park_excluded <- excluded_years |>
        dplyr::filter(transect %in% unique(park_data$transect))
      park_pair_summary <- pair_summary |>
        dplyr::filter(transect %in% unique(park_data$transect))

      park_metadata <- metadata
      park_metadata$excluded_years <- park_excluded
      park_metadata$pair_summary <- park_pair_summary
      park_metadata$n_transects <- length(unique(park_data$transect))
      park_metadata$n_pairs <- nrow(park_auc) / 2  # Each pair has 2 years
      park_metadata$parks <- park

      park_results <- list(
        data = park_data,
        auc_results = park_auc,
        metadata = park_metadata
      )

      # Save as RDS
      output_file <- file.path(output_dir, paste0(park, "_pairwise_results.rds"))
      saveRDS(park_results, output_file)

      if (verbose) {
        cat("  Saved ", park, " results to: ", output_file, "\n", sep = "")
      }
    }
  }

  return(invisible(results))
}


# ============================================================================
# HELPER FUNCTION: PAIRWISE COMMON MINIMUM CALCULATION
# ============================================================================

#' Calculate Common Minimum for a Specific Pair of Years
#'
#' Similar to \code{calculate_and_interpolate_common_min}, but calculates
#' the common minimum distance for only two specific years rather than all
#' years for a transect.
#'
#' @param data Cleaned transect data for a single transect and two years
#' @param year1 First year in the pair
#' @param year2 Second year in the pair
#' @param verbose Logical. If TRUE, print warnings for edge cases
#' @return Tibble of common_min points with same structure as
#'   \code{calculate_and_interpolate_common_min}
#'
#' @importFrom dplyr filter group_by summarise mutate inner_join group_split
#'   first if_else tibble bind_rows
#' @importFrom stats approx
#' @keywords internal
calculate_pairwise_common_min <- function(data, year1, year2, verbose = FALSE) {

  # Validate input
  if (!"cross_island" %in% names(data)) {
    stop("ERROR: cross_island column missing from input data.",
         call. = FALSE)
  }

  # Check for NA values in cross_island
  if (any(is.na(data$cross_island))) {
    data <- data |> dplyr::filter(!is.na(cross_island))
  }

  # Calculate minimum distance for each of the two years
  year_mins <- data |>
    dplyr::filter(year %in% c(year1, year2)) |>
    dplyr::group_by(transect, year) |>
    dplyr::summarise(year_min = min(distance, na.rm = TRUE), .groups = 'drop')

  # Calculate common minimum (max of the two minimums)
  common_mins <- year_mins |>
    dplyr::group_by(transect) |>
    dplyr::summarise(common_min_distance = max(year_min, na.rm = TRUE), .groups = 'drop')

  # Check if we have any transects to process
  if (nrow(common_mins) == 0) {
    return(dplyr::tibble())
  }

  # For each year, generate the common_min point
  result <- data |>
    dplyr::inner_join(common_mins, by = "transect") |>
    dplyr::group_by(transect, year) |>
    dplyr::group_split() |>
    lapply(function(df) {

      # Safety check: ensure df is not empty
      if (nrow(df) == 0) {
        return(dplyr::tibble())
      }

      # Get common_min distance with validation
      cm <- dplyr::first(df$common_min_distance)

      # Validate cm value
      if (is.na(cm) || !is.finite(cm)) {
        return(dplyr::tibble())
      }

      # Skip if common_min is at distance = 0
      if (abs(cm) < 0.001) {
        return(dplyr::tibble())
      }

      # Calculate distance range with validation
      dist_min <- min(df$distance, na.rm = TRUE)
      dist_max <- max(df$distance, na.rm = TRUE)

      # Check for valid distance range
      if (!is.finite(dist_min) || !is.finite(dist_max)) {
        return(dplyr::tibble())
      }

      # Skip if common_min is outside the measured range for this year
      if (cm < dist_min || cm > dist_max) {
        return(dplyr::tibble())
      }

      # Check if we already have a point AT common_min (within error-aware tolerance)
      if (any(is.na(df$sigma_h))) {
        point_tolerances <- rep(0.081, nrow(df))
      } else {
        point_tolerances <- 3 * df$sigma_h
      }

      existing_idx <- which(abs(df$distance - cm) < point_tolerances)

      # If multiple matches, choose the closest one
      if (length(existing_idx) > 1) {
        distances_to_cm <- abs(df$distance[existing_idx] - cm)
        existing_idx <- existing_idx[which.min(distances_to_cm)]
      }

      if (length(existing_idx) > 0) {
        # Use existing point's values
        existing_point <- df[existing_idx[1], ]

        # Return point with point_type overridden to "common_min"
        dplyr::tibble(
          transect = dplyr::first(df$transect),
          year = dplyr::first(df$year),
          distance = cm,
          elevation = existing_point$elevation,
          point_type = 'common_min',
          park = dplyr::first(df$park),
          cross_island = FALSE,
          sigma_h = existing_point$sigma_h,
          sigma_v = existing_point$sigma_v,
          accuracy_tier = existing_point$accuracy_tier,
          uncertainty = existing_point$uncertainty,
          confidence = existing_point$confidence,
          uncertainty_95ci = existing_point$uncertainty_95ci,
          slope = dplyr::if_else(is.na(existing_point$slope), NA_real_, existing_point$slope)
        )
      } else {
        # No existing point - interpolate elevation at common_min

        # Find bracketing points
        lower_candidates <- which(df$distance < cm)
        upper_candidates <- which(df$distance > cm)

        # Check if bracketing points exist
        if (length(lower_candidates) == 0 || length(upper_candidates) == 0) {
          return(dplyr::tibble())
        }

        lower_idx <- max(lower_candidates)
        upper_idx <- min(upper_candidates)

        # Interpolate elevation
        interp_elev <- tryCatch({
          approx(df$distance, df$elevation, xout = cm)$y
        }, error = function(e) {
          NA_real_
        })

        # Return empty if interpolation failed
        if (is.na(interp_elev)) {
          return(dplyr::tibble())
        }

        # Average accuracy values from bracketing points
        sigma_h_avg <- mean(c(df$sigma_h[lower_idx], df$sigma_h[upper_idx]), na.rm = TRUE)
        sigma_v_avg <- mean(c(df$sigma_v[lower_idx], df$sigma_v[upper_idx]), na.rm = TRUE)

        # Check for NA in accuracy values
        if (is.na(sigma_h_avg) || is.na(sigma_v_avg) ||
            is.na(df$sigma_h[lower_idx]) || is.na(df$sigma_h[upper_idx]) ||
            is.na(df$sigma_v[lower_idx]) || is.na(df$sigma_v[upper_idx])) {
          return(dplyr::tibble())
        }

        # Calculate slope between bracketing points
        slope <- abs((df$elevation[upper_idx] - df$elevation[lower_idx]) /
                    (df$distance[upper_idx] - df$distance[lower_idx]))

        # Check for near-zero slope
        if (slope < 1e-6) {
          return(dplyr::tibble())
        }

        # Calculate uncertainty using same formula as interpolated zero crossings
        sigma_x_vertical <- sqrt(df$sigma_v[lower_idx]^2 + df$sigma_v[upper_idx]^2) / slope
        sigma_x_horizontal <- sqrt(df$sigma_h[lower_idx]^2 + df$sigma_h[upper_idx]^2)

        # Check for numerical issues
        if (!is.finite(sigma_x_vertical) || !is.finite(sigma_x_horizontal)) {
          return(dplyr::tibble())
        }

        # Total position uncertainty
        uncertainty <- sqrt(sigma_x_vertical^2 + sigma_x_horizontal^2)

        # Final check for valid uncertainty
        if (!is.finite(uncertainty)) {
          return(dplyr::tibble())
        }

        # Assign confidence based on slope
        confidence <- ifelse(slope > 0.05, "high",
                           ifelse(slope > 0.02, "moderate", "low"))

        # Get accuracy tier from bracketing points
        tier <- df$accuracy_tier[lower_idx]

        dplyr::tibble(
          transect = dplyr::first(df$transect),
          year = dplyr::first(df$year),
          distance = cm,
          elevation = interp_elev,
          point_type = 'common_min',
          park = dplyr::first(df$park),
          cross_island = FALSE,
          sigma_h = sigma_h_avg,
          sigma_v = sigma_v_avg,
          accuracy_tier = tier,
          uncertainty = uncertainty,
          confidence = confidence,
          uncertainty_95ci = 1.96 * uncertainty,
          slope = slope
        )
      }
    }) |>
    dplyr::bind_rows()

  return(result)
}
