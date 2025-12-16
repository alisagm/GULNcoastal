# tests/testthat/test-transect_analysis.R
# ============================================================================
# INTEGRATION TESTS FOR run_transect_analysis()
# ============================================================================
#
# These tests verify the complete workflow wrapper functions correctly,
# including input validation, pipeline execution, and output structure.
#
# Test fixtures are in tests/testthat/testdata/
# ============================================================================


# ============================================================================
# SETUP: Helper functions and paths
# ============================================================================
#
# CRITICAL: test_path() must be called OUTSIDE of withr::with_tempdir()
# because it resolves paths relative to the test file, but we need to
# capture the absolute path before changing working directories.
#

# Get absolute paths to fixture files at test load time
# This ensures paths work even when working directory changes
# CRITICAL: Use normalizePath() to convert to absolute path
FIXTURE_DIR <- tryCatch(
  normalizePath(test_path("testdata"), winslash = "/", mustWork = TRUE),
  error = function(e) {
    stop(
      "Fixture directory not found. Expected: tests/testthat/testdata/\n",
      "  Create this directory and add fixture files.\n",
      "  Original error: ", e$message
    )
  }
)

# Verify fixture directory exists
if (!dir.exists(FIXTURE_DIR)) {
  stop(
    "Fixture directory not found: ", FIXTURE_DIR, "\n",
    "  Create tests/testthat/testdata/ and add fixture files."
  )
}

# Define fixture file paths (update these to match your actual fixture names)
# Transect data files - one per park (filenames must contain park code)
FIXTURE_GUIS_CSV <- file.path(FIXTURE_DIR, "00_importGUIS.csv")
FIXTURE_PAIS_CSV <- file.path(FIXTURE_DIR, "00_importPAIS.csv")

# Config files
FIXTURE_ACCURACY_CSV <- file.path(FIXTURE_DIR, "accuracy_values.csv")
FIXTURE_SPECIAL_CSV <- file.path(FIXTURE_DIR, "special_cases.csv")

# Helper to check if at least one transect file exists
has_transect_fixtures <- function() {

  file.exists(FIXTURE_GUIS_CSV) || file.exists(FIXTURE_PAIS_CSV)
}

# Helper to get list of available transect fixtures
get_transect_fixtures <- function() {
  fixtures <- c(FIXTURE_GUIS_CSV, FIXTURE_PAIS_CSV)
  fixtures[file.exists(fixtures)]
}


#' Copy fixture file with validation
#'
#' Copies a file and errors if the source doesn't exist or copy fails.
#' This prevents silent failures that make tests hard to debug.
#'
#' @param from Source file path (should be absolute)
#' @param to Destination file path
#' @return TRUE (invisibly) on success, errors on failure
copy_fixture <- function(from, to) {
  if (!file.exists(from)) {
    stop(
      "Fixture file not found: ", from, "\n",
      "  Available files in fixture dir:\n    ",
      paste(list.files(dirname(from)), collapse = "\n    ")
    )
  }

  success <- file.copy(from, to, overwrite = TRUE)

  if (!success) {
    stop("Failed to copy fixture file: ", from, " -> ", to)
  }

  invisible(TRUE)
}


#' Setup minimal test environment in a directory
#'
#' Copies the standard fixture files needed for most tests.
#' Copies all available transect files (GUIS and/or PAIS).
#'
#' @param dest_dir Destination directory (must already exist)
setup_test_environment <- function(dest_dir) {
  # Copy all available transect files (preserving original filenames for park detection)
  if (file.exists(FIXTURE_GUIS_CSV)) {
    copy_fixture(FIXTURE_GUIS_CSV, file.path(dest_dir, basename(FIXTURE_GUIS_CSV)))
  }
  if (file.exists(FIXTURE_PAIS_CSV)) {
    copy_fixture(FIXTURE_PAIS_CSV, file.path(dest_dir, basename(FIXTURE_PAIS_CSV)))
  }

  # Copy accuracy table
  copy_fixture(FIXTURE_ACCURACY_CSV, file.path(dest_dir, "accuracy_values.csv"))
}


# ============================================================================
# TEST GROUP 0: Fixture Validation (runs first to catch setup issues)
# ============================================================================

test_that("fixture files exist", {
  # At least one transect file should exist
  expect_true(
    has_transect_fixtures(),
    info = paste(
      "No transect fixtures found. Expected at least one of:\n",
      "  -", FIXTURE_GUIS_CSV, "\n",
      "  -", FIXTURE_PAIS_CSV
    )
  )

  # Report which transect files are available
  if (file.exists(FIXTURE_GUIS_CSV)) {
    succeed(paste("Found GUIS fixture:", FIXTURE_GUIS_CSV))
  }
  if (file.exists(FIXTURE_PAIS_CSV)) {
    succeed(paste("Found PAIS fixture:", FIXTURE_PAIS_CSV))
  }

  # Accuracy table is required
  expect_true(
    file.exists(FIXTURE_ACCURACY_CSV),
    info = paste("Missing:", FIXTURE_ACCURACY_CSV)
  )

  # Special cases is optional, just note if missing
  if (!file.exists(FIXTURE_SPECIAL_CSV)) {
    skip("Optional fixture missing: special_cases.csv")
  }
})


# ============================================================================
# TEST GROUP 1: Input Validation and Error Handling
# ============================================================================

test_that("run_transect_analysis errors on non-existent data directory", {
  expect_error(
    run_transect_analysis(data_dir = "this/path/does/not/exist"),
    "Data directory does not exist"
  )
})


test_that("run_transect_analysis errors on empty data directory", {
  withr::with_tempdir({
    # Create empty directory
    dir.create("empty_data")

    expect_error(
      run_transect_analysis(data_dir = "empty_data"),
      "No CSV data files found"
    )
  })
})


test_that("run_transect_analysis errors when accuracy table is missing", {
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")

  withr::with_tempdir({
    # Create directory with data but no accuracy table
    dir.create("no_accuracy")

    # Copy transect data file(s) - need at least one
    transect_files <- get_transect_fixtures()
    for (f in transect_files) {
      copy_fixture(f, file.path("no_accuracy", basename(f)))
    }

    # Verify at least one file was copied
    csv_files <- list.files("no_accuracy", pattern = "\\.csv$")
    expect_true(
      length(csv_files) > 0,
      info = "No CSV files were copied to test directory"
    )

    expect_error(
      run_transect_analysis(data_dir = "no_accuracy"),
      "Accuracy table not found"
    )
  })
})


# ============================================================================
# TEST GROUP 2: Successful Execution
# ============================================================================

test_that("run_transect_analysis completes successfully with valid input", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    # Setup test environment
    dir.create("data")
    dir.create("output")
    setup_test_environment("data")

    # Verify files were copied
    csv_files <- list.files("data", pattern = "\\.csv$")
    expect_true(
      length(csv_files) > 1,
      info = "Expected transect + accuracy files"
    )

    # Run analysis
    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      save_results = FALSE,
      verbose = FALSE
    )

    # Basic success check
    expect_type(results, "list")
    expect_named(results, c("data", "auc_results", "metadata"))
  })
})


test_that("run_transect_analysis returns correct output structure", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    dir.create("data")
    dir.create("output")
    setup_test_environment("data")

    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      save_results = FALSE,
      verbose = FALSE
    )

    # Validate data component
    expect_s3_class(results$data, "data.frame")
    expected_data_cols <- c(
      "transect", "year", "distance", "elevation", "point_type",
      "park", "sigma_h", "sigma_v"
    )
    expect_true(
      all(expected_data_cols %in% names(results$data)),
      info = paste(
        "Missing columns in data:",
        paste(setdiff(expected_data_cols, names(results$data)), collapse = ", ")
      )
    )

    # Validate auc_results component
    expect_s3_class(results$auc_results, "data.frame")
    expected_auc_cols <- c(
      "transect", "year", "auc", "auc_sigma"
    )
    expect_true(
      all(expected_auc_cols %in% names(results$auc_results)),
      info = paste(
        "Missing columns in auc_results:",
        paste(setdiff(expected_auc_cols, names(results$auc_results)), collapse = ", ")
      )
    )

    # Validate metadata component
    expect_type(results$metadata, "list")
    expect_true("timestamp" %in% names(results$metadata))
    expect_true("n_transects" %in% names(results$metadata))
    expect_true("parks" %in% names(results$metadata))
    expect_true("years" %in% names(results$metadata))
  })
})


# ============================================================================
# TEST GROUP 3: Parameter Flexibility
# ============================================================================

test_that("run_transect_analysis accepts accuracy table from separate location", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    # Create separate directories for data and config
    dir.create("data")
    dir.create("config")
    dir.create("output")

    # Copy transect data to data dir (preserving filenames for park detection)
    for (f in get_transect_fixtures()) {
      copy_fixture(f, file.path("data", basename(f)))
    }

    # Copy accuracy table to config dir (separate location)
    copy_fixture(FIXTURE_ACCURACY_CSV, "config/accuracy_values.csv")

    # Should work with explicit accuracy_table_path
    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      accuracy_table_path = "config/accuracy_values.csv",
      save_results = FALSE,
      verbose = FALSE
    )

    expect_type(results, "list")
    expect_equal(results$metadata$accuracy_table, "config/accuracy_values.csv")
  })
})


test_that("run_transect_analysis accepts special cases from separate location", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")
  skip_if_not(file.exists(FIXTURE_SPECIAL_CSV), "Special cases fixture missing")

  withr::with_tempdir({
    dir.create("data")
    dir.create("config")
    dir.create("output")

    # Copy transect files to data dir
    for (f in get_transect_fixtures()) {
      copy_fixture(f, file.path("data", basename(f)))
    }

    # Copy config files to config dir
    copy_fixture(FIXTURE_ACCURACY_CSV, "config/accuracy_values.csv")
    copy_fixture(FIXTURE_SPECIAL_CSV, "config/special_cases.csv")

    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      accuracy_table_path = "config/accuracy_values.csv",
      special_cases_path = "config/special_cases.csv",
      save_results = FALSE,
      verbose = FALSE
    )

    expect_type(results, "list")
    expect_equal(results$metadata$special_cases, "config/special_cases.csv")
  })
})


test_that("run_transect_analysis works without special cases file", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    dir.create("data")
    dir.create("output")

    # Copy transect data and accuracy table (no special cases)
    for (f in get_transect_fixtures()) {
      copy_fixture(f, file.path("data", basename(f)))
    }
    copy_fixture(FIXTURE_ACCURACY_CSV, "data/accuracy_values.csv")

    # Should work without special cases
    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      save_results = FALSE,
      verbose = FALSE
    )

    expect_type(results, "list")
    expect_null(results$metadata$special_cases)
  })
})


# ============================================================================
# TEST GROUP 4: Output File Generation
# ============================================================================

test_that("run_transect_analysis creates output files when save_results = TRUE", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    dir.create("data")
    dir.create("output")
    setup_test_environment("data")

    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      save_results = TRUE,
      verbose = FALSE
    )

    # Check that RDS file was created (in current dir, default processed_dir)
    rds_files <- list.files(".", pattern = "\\.rds$", full.names = TRUE)
    expect_true(
      length(rds_files) > 0,
      info = "Expected at least one RDS file"
    )

    # Verify RDS file can be loaded and has correct structure
    if (length(rds_files) > 0) {
      loaded <- readRDS(rds_files[1])
      expect_named(loaded, c("data", "auc_results", "metadata"))
    }
  })
})


test_that("run_transect_analysis creates output directory if it doesn't exist", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    dir.create("data")
    setup_test_environment("data")

    # output directory doesn't exist yet
    expect_false(dir.exists("new_output"))

    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "new_output",
      save_results = FALSE,
      verbose = FALSE
    )

    # Should have created the directory
    expect_true(dir.exists("new_output"))
  })
})


# ============================================================================
# TEST GROUP 5: Data Integrity
# ============================================================================

test_that("run_transect_analysis preserves all transect-year combinations", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    dir.create("data")
    dir.create("output")
    setup_test_environment("data")

    # Read input data to get expected combinations (from all transect files)
    # Pattern matches files containing GUIS or PAIS (case-insensitive)
    transect_files <- list.files(
      "data",
      pattern = "(GUIS|PAIS).*\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    # Read with transect explicitly as character to handle mixed numeric/character IDs
    input_data <- lapply(transect_files, function(f) {
      readr::read_csv(
        f,
        col_types = readr::cols(transect = readr::col_character()),
        show_col_types = FALSE
      )
    })
    input_data <- dplyr::bind_rows(input_data)

    # Get unique transect-year combinations from input
    input_combos <- unique(paste(input_data$transect, input_data$year, sep = "-"))

    # Run analysis
    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      save_results = FALSE,
      verbose = FALSE
    )

    # Get unique transect-year combinations from output
    # Convert to character to handle factor vs character comparison
    output_combos <- unique(paste(
      as.character(results$auc_results$transect),
      results$auc_results$year,
      sep = "-"
    ))

    expect_setequal(input_combos, output_combos)
  })
})


test_that("run_transect_analysis produces positive AUC values", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    dir.create("data")
    dir.create("output")
    setup_test_environment("data")

    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      save_results = FALSE,
      verbose = FALSE
    )

    # AUC should be positive for beach profiles above sea level
    expect_true(all(results$auc_results$auc > 0))
  })
})


test_that("run_transect_analysis adds zero-crossing points", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    dir.create("data")
    dir.create("output")
    setup_test_environment("data")

    results <- run_transect_analysis(
      data_dir = "data",
      output_dir = "output",
      save_results = FALSE,
      verbose = FALSE
    )

    # Check that point_type column includes more than just "measured"
    point_types <- unique(results$data$point_type)

    # Should have at least measured points
    expect_true("measured" %in% point_types)

    # Test data should produce interpolated zero crossings
    # (profiles go from negative to positive and back)
    expect_true(
      any(c("interpolated", "extrapolated", "measured_zero") %in% point_types),
      info = paste("Point types found:", paste(point_types, collapse = ", "))
    )
  })
})


# ============================================================================
# TEST GROUP 6: Verbose Output
# ============================================================================

test_that("run_transect_analysis verbose mode produces output", {
  skip_on_cran()
  skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
  skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")

  withr::with_tempdir({
    dir.create("data")
    dir.create("output")
    setup_test_environment("data")

    # Capture output from verbose mode
    output <- capture.output({
      results <- run_transect_analysis(
        data_dir = "data",
        output_dir = "output",
        save_results = FALSE,
        verbose = TRUE
      )
    })

    # Should produce some output
    expect_gt(length(output), 0)

    # Should mention key steps
    output_text <- paste(output, collapse = "\n")
    expect_match(output_text, "Loading data", ignore.case = TRUE)
  })
})


# ============================================================================
# TEST GROUP 7: Regression Tests (Optional - for numerical consistency)
# ============================================================================

# Uncomment and customize these tests once you have validated baseline results

# test_that("run_transect_analysis produces consistent AUC values", {
#   skip_on_cran()
#   skip_if_not(has_transect_fixtures(), "Transect fixture files missing")
#   skip_if_not(file.exists(FIXTURE_ACCURACY_CSV), "Accuracy fixture missing")
#
#   withr::with_tempdir({
#     dir.create("data")
#     dir.create("output")
#     setup_test_environment("data")
#
#     results <- run_transect_analysis(
#       data_dir = "data",
#       output_dir = "output",
#       save_results = FALSE,
#       verbose = FALSE
#     )
#
#     # Snapshot test for AUC values
#     # This will fail if values change, alerting you to investigate
#     expect_snapshot_value(
#       round(results$auc_results$auc, 4),
#       style = "json2"
#     )
#   })
# })
