# test-plot_visual.R
# Visual regression tests for plotting functions
#
# These tests compare current plot output against baseline SVG snapshots.
# Baselines are stored in _snaps/test-plot_visual/
#
# To regenerate baselines: run generate_snapshots_simple.R
# To review changes: vdiffr::manage_cases()

library(vdiffr)
library(dplyr)
library(ggplot2)
library(patchwork)

# ============================================================================
# TEST DATA LOADING
# ============================================================================

# Load test data once for all tests
# Using test_path() which resolves relative to the test file location
load_visual_testdata <- function() {
  list(
    data = readRDS(test_path("testdata", "11_withcms.rds")),
    auc = readRDS(test_path("testdata", "14_aucwithmetrics.rds"))
  )
}

# ============================================================================
# SINGLE YEAR PLOTS
# ============================================================================

test_that("single year plot with AUC shading", {
  skip_on_cran()
  skip_on_ci()  # Visual tests can have platform differences

  td <- load_visual_testdata()

  first_transect <- unique(td$data$transect)[1]
  t_data <- td$data[td$data$transect == first_transect, ]
  latest_year <- max(t_data$year)
  t_data <- t_data[t_data$year == latest_year, ]

  # Create config for single year with AUC and shading
  config <- create_plot_config(
    show_auc = TRUE,
    shade_area = TRUE,
    axis_limits = "none"
  )

  p <- plot_transect(
    data = t_data,
    auc_results = td$auc,
    config = config
  )

  expect_doppelganger("single-year-auc-shading", p)
})

test_that("single year plot without AUC", {
  skip_on_cran()
  skip_on_ci()

  td <- load_visual_testdata()

  first_transect <- unique(td$data$transect)[1]
  t_data <- td$data[td$data$transect == first_transect, ]
  latest_year <- max(t_data$year)
  t_data <- t_data[t_data$year == latest_year, ]

  # Create config for single year without AUC
  config <- create_plot_config(
    show_auc = FALSE,
    shade_area = FALSE,
    axis_limits = "none"
  )

  p <- plot_transect(
    data = t_data,
    auc_results = td$auc,
    config = config
  )

  expect_doppelganger("single-year-no-auc", p)
})

# ============================================================================
# MULTI-YEAR PLOTS
# ============================================================================

test_that("multi-year baseline vs recent comparison", {
  skip_on_cran()
  skip_on_ci()

  td <- load_visual_testdata()

  first_transect <- unique(td$data$transect)[1]
  t_data <- td$data[td$data$transect == first_transect, ]

  # Baseline + most recent
  years <- c(min(t_data$year), max(t_data$year))
  t_data <- t_data[t_data$year %in% years, ]

  # Create config for multi-year comparison
  config <- create_plot_config(
    show_auc = TRUE,
    shade_area = TRUE,
    axis_limits = "none"
  )

  p <- plot_transect(
    data = t_data,
    auc_results = td$auc,
    config = config
  )

  expect_doppelganger("multi-year-baseline-recent", p)
})

test_that("multi-year all years", {
  skip_on_cran()
  skip_on_ci()

  td <- load_visual_testdata()

  first_transect <- unique(td$data$transect)[1]
  t_data <- td$data[td$data$transect == first_transect, ]

  # Create config for all years
  config <- create_plot_config(
    show_auc = TRUE,
    shade_area = TRUE,
    axis_limits = "none"
  )

  p <- plot_transect(
    data = t_data,
    auc_results = td$auc,
    config = config
  )

  expect_doppelganger("multi-year-all", p)
})
