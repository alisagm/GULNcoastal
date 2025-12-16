# DATA IMPORT
test_that("import_profile_transects reads valid CSV correctly", {
  # Create minimal fixture in tests/testthat/testdata/
  result <- import_profile_transects(test_path("testdata", "00_import.csv"))

  expect_s3_class(result, "data.frame")
  expect_named(result, c("transect", "year", "distance", "elevation", "cross_island"))
  expect_s3_class(result$transect, "factor")
})

# ZEROCROSSING
test_that("find_zero_crossings matches validated output", {


  input <- readRDS(test_path("testdata", "05_accuracy.rds"))
  expected <- readRDS(test_path("testdata", "06_zeropts.rds"))

  result <- find_zero_crossings(input)

  expect_equal(result, expected)
})

# COMMON MIN
test_that("calculate_common_min matches validated output", {

  input <- readRDS(test_path("testdata", "08_with0s.rds"))
  expected <- readRDS(test_path("testdata", "09_commonmins.rds"))

  result <- calculate_common_min(input)

  expect_equal(result, expected)
})

# ACCURACY
test_that("assign_accuracy matches validated output", {

  input <- readRDS(test_path("testdata", "04_dedup.rds"))
  expected <- readRDS(test_path("testdata", "05_accuracy.rds"))

  accuracy_values <- read.csv(system.file("extdata", "accuracy_values.csv", package = "GULNcoastal"))
  special_cases <- read.csv(system.file("extdata", "special_cases.csv", package = "GULNcoastal"))
  result <- assign_accuracy(input,accuracy_table=accuracy_values,special_cases = special_cases)

  expect_equal(result, expected)
})

# AUC
test_that("calculate_auc_weighted_all matches validated output", {

  input <- readRDS(test_path("testdata", "11_withcms.rds"))
  expected <- readRDS(test_path("testdata", "12_auc.rds"))

  result <- calculate_auc_weighted_all(input)

  expect_equal(result, expected)
})

# NEGATIVES CLEANING
test_that("remove_negatives matches validated output", {

  input <- readRDS(test_path("testdata", "02_typed.rds"))
  expected <- readRDS(test_path("testdata", "03_clean.rds"))

  # apply per group
  result <- input %>%
    dplyr::group_by(transect, year, cross_island, park) %>%
    dplyr::group_split() %>%
    lapply(remove_negatives) %>%
    dplyr::bind_rows()

  expect_equal(result, expected)
})

# DATA SAVING
test_that("save_topo_transects_rds creates file with correct structure", {

  data <- readRDS(test_path("testdata", "11_withcms.rds"))
  auc_results <- readRDS(test_path("testdata", "14_aucwithmetrics.rds"))

  withr::with_tempdir({
    # Test export
    path <- save_topo_transects_rds(data, auc_results, output_dir = ".", verbose = FALSE)

    expect_true(file.exists(path))

    # Verify structure
    loaded <- readRDS(path)
    expect_named(loaded, c("data", "auc_results", "metadata"))
  })
})
