# error.R
# ============================================================================
# ERROR/ACCURACY ASSIGNMENT FUNCTIONS
# ============================================================================
#
# This module assigns year-specific measurement accuracy values to topographic
# transect data. Accuracy varies significantly by survey year due to differences
# in GPS equipment, field procedures, and environmental conditions.
#
# Key findings from accuracy analysis:
# - Measurement precision varies by YEAR, not by location
# - Using single error values misses 60-80% of real coastal changes
# - Year-stratified accuracy is essential for proper uncertainty quantification
#
# Lookup hierarchy:
#   1. special_cases (park + year + transect) - most specific
#   2. accuracy_table (park + year) - general
#   3. default_accuracy - conservative fallback

# ============================================================================
# ACCURACY TABLE LOADING AND VALIDATION
# ============================================================================

#' Load Accuracy Table from CSV
#'
#' Reads a CSV file containing survey-specific accuracy values and validates
#' its structure.
#'
#' @param path Path to CSV file containing accuracy table
#'
#' @return A validated tibble with columns: park, year, sigma_h, sigma_v, accuracy_tier
#'
#' @details
#' Expected CSV format (with header row):
#' ```
#' park,year,sigma_h,sigma_v,accuracy_tier
#' PAIS,2019,0.009,0.004,A_Excellent
#' PAIS,2021,0.012,0.003,A_Excellent
#' ...
#' ```
#'
#' @examples
#' \dontrun{
#' accuracy_table <- load_accuracy_table("data/accuracy_values.csv")
#' }
#'
#' @importFrom readr read_csv cols col_character col_integer col_double
#' @export
load_accuracy_table <- function(path) {

  if (!file.exists(path)) {
    stop("Accuracy table file not found: ", path)
  }

  # Read with explicit column types
  accuracy_table <- readr::read_csv(
    path,
    col_types = readr::cols(
      park = readr::col_character(),
      year = readr::col_integer(),
      sigma_h = readr::col_double(),
      sigma_v = readr::col_double(),
      accuracy_tier = readr::col_character()
    ),
    show_col_types = FALSE
  )

  # Validate structure
  validate_accuracy_table(accuracy_table)

  return(accuracy_table)
}


#' Validate Accuracy Table Structure
#'
#' Checks that an accuracy table has the required columns and valid values.
#' Called automatically by load_accuracy_table(), but can be used directly
#' if constructing a table programmatically.
#'
#' @param accuracy_table A data frame to validate
#'
#' @return TRUE invisibly if valid; stops with error if invalid
#'
#' @examples
#' \dontrun{
#' my_table <- tribble(
#'   ~park, ~year, ~sigma_h, ~sigma_v, ~accuracy_tier,
#'   "PAIS", 2019, 0.009, 0.004, "A_Excellent"
#' )
#' validate_accuracy_table(my_table)
#' }
#'
#' @export
validate_accuracy_table <- function(accuracy_table) {

  # Check required columns
  required_cols <- c("park", "year", "sigma_h", "sigma_v", "accuracy_tier")
  missing_cols <- setdiff(required_cols, names(accuracy_table))

  if (length(missing_cols) > 0) {
    stop("Accuracy table missing required columns: ",
         paste(missing_cols, collapse = ", "),
         "\nRequired: ", paste(required_cols, collapse = ", "))
  }

  # Check for NA values in key columns
  if (any(is.na(accuracy_table$park))) {
    stop("Accuracy table contains NA values in 'park' column")
  }
  if (any(is.na(accuracy_table$year))) {
    stop("Accuracy table contains NA values in 'year' column")
  }

  # Check for non-positive sigma values
  if (any(accuracy_table$sigma_h <= 0, na.rm = TRUE)) {
    stop("Accuracy table contains non-positive sigma_h values")
  }
  if (any(accuracy_table$sigma_v <= 0, na.rm = TRUE)) {
    stop("Accuracy table contains non-positive sigma_v values")
  }

  # Check for duplicate park/year combinations
  duplicates <- accuracy_table |>
    dplyr::group_by(park, year) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(duplicates) > 0) {
    dup_summary <- duplicates |>
      dplyr::distinct(park, year) |>
      dplyr::mutate(label = paste0(park, "/", year)) |>
      dplyr::pull(label)
    stop("Accuracy table contains duplicate park/year combinations: ",
         paste(dup_summary, collapse = ", "))
  }

  # Warn about non-standard tier values (but don't fail)
  valid_tiers <- c("A_Excellent", "B_Good", "C_Moderate")
  found_tiers <- unique(accuracy_table$accuracy_tier)
  unknown_tiers <- setdiff(found_tiers, valid_tiers)

  if (length(unknown_tiers) > 0) {
    warning("Accuracy table contains non-standard tier values: ",
            paste(unknown_tiers, collapse = ", "),
            "\nStandard tiers: ", paste(valid_tiers, collapse = ", "))
  }

  invisible(TRUE)
}


# ============================================================================
# SPECIAL CASES TABLE LOADING AND VALIDATION
# ============================================================================

#' Load Special Cases Table from CSV
#'
#' Reads a CSV file containing transect-specific accuracy overrides.
#' These values take precedence over the general accuracy_table.
#'
#' @param path Path to CSV file containing special cases table
#'
#' @return A validated tibble with columns: park, year, transect, sigma_h, sigma_v, accuracy_tier
#'
#' @details
#' Expected CSV format (with header row):
#' ```
#' park,year,transect,sigma_h,sigma_v,accuracy_tier
#' GUIS,2018,fp07,0.015,0.120,C_Moderate
#' GUIS,2018,fp08,0.015,0.120,C_Moderate
#' ...
#' ```
#'
#' Use this for transects that have different accuracy values than the
#' park/year default (e.g., different survey methods within the same year).
#'
#' @examples
#' \dontrun{
#' special_cases <- load_special_cases_table("data/special_cases.csv")
#' }
#'
#' @importFrom readr read_csv cols col_character col_integer col_double
#' @export
load_special_cases_table <- function(path) {

  if (!file.exists(path)) {
    stop("Special cases table file not found: ", path)
  }

  # Read with explicit column types
  special_cases <- readr::read_csv(
    path,
    col_types = readr::cols(
      park = readr::col_character(),
      year = readr::col_integer(),
      transect = readr::col_character(),
      sigma_h = readr::col_double(),
      sigma_v = readr::col_double(),
      accuracy_tier = readr::col_character()
    ),
    show_col_types = FALSE
  )

  # Validate structure
  validate_special_cases_table(special_cases)

  return(special_cases)
}


#' Validate Special Cases Table Structure
#'
#' Checks that a special cases table has the required columns and valid values.
#' Called automatically by load_special_cases_table().
#'
#' @param special_cases A data frame to validate
#'
#' @return TRUE invisibly if valid; stops with error if invalid
#'
#' @export
validate_special_cases_table <- function(special_cases) {

  # Check required columns
  required_cols <- c("park", "year", "transect", "sigma_h", "sigma_v", "accuracy_tier")
  missing_cols <- setdiff(required_cols, names(special_cases))

  if (length(missing_cols) > 0) {
    stop("Special cases table missing required columns: ",
         paste(missing_cols, collapse = ", "),
         "\nRequired: ", paste(required_cols, collapse = ", "))
  }

  # Check for NA values in key columns
  if (any(is.na(special_cases$park))) {
    stop("Special cases table contains NA values in 'park' column")
  }
  if (any(is.na(special_cases$year))) {
    stop("Special cases table contains NA values in 'year' column")
  }
  if (any(is.na(special_cases$transect))) {
    stop("Special cases table contains NA values in 'transect' column")
  }

  # Check for non-positive sigma values
  if (any(special_cases$sigma_h <= 0, na.rm = TRUE)) {
    stop("Special cases table contains non-positive sigma_h values")
  }
  if (any(special_cases$sigma_v <= 0, na.rm = TRUE)) {
    stop("Special cases table contains non-positive sigma_v values")
  }

  # Check for duplicate park/year/transect combinations
  duplicates <- special_cases |>
    dplyr::group_by(park, year, transect) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(duplicates) > 0) {
    dup_summary <- duplicates |>
      dplyr::distinct(park, year, transect) |>
      dplyr::mutate(label = paste0(park, "/", year, "/", transect)) |>
      dplyr::pull(label)
    stop("Special cases table contains duplicate park/year/transect combinations: ",
         paste(dup_summary, collapse = ", "))
  }

  invisible(TRUE)
}


# ============================================================================
# ACCURACY ASSIGNMENT
# ============================================================================

#' Assign Year-Specific Accuracy Values to Topographic Data
#'
#' Adds three columns to the input dataframe containing survey-specific
#' vertical accuracy, horizontal accuracy, and quality tier classifications.
#'
#' @param df Dataframe containing topographic data with required columns:
#'   - park: Park identifier (e.g., "PAIS" or "GUIS")
#'   - year: Survey year (numeric)
#'   - transect: Transect identifier
#'
#' @param accuracy_table Optional. A data frame containing accuracy lookup values
#'   with columns: park, year, sigma_h, sigma_v, accuracy_tier.
#'   If NULL (default), conservative default values are applied to all observations.
#'   Use load_accuracy_table() to read from CSV.
#'
#' @param special_cases Optional. A data frame containing transect-specific
#'   accuracy overrides with columns: park, year, transect, sigma_h, sigma_v,
#'   accuracy_tier. These values take precedence over accuracy_table.
#'   Use load_special_cases_table() to read from CSV.
#'
#' @param default_accuracy Named list with default sigma_h, sigma_v, and
#'   accuracy_tier for unknown park/year combinations.
#'   Default: list(sigma_h = 0.030, sigma_v = 0.070, accuracy_tier = "C_Moderate")
#'
#' @param verbose Logical. If TRUE, print summary of accuracy assignments.
#'   Default = FALSE.
#'
#' @return Dataframe with three additional columns:
#'   - sigma_v: Vertical accuracy (meters) - point-level uncertainty
#'   - sigma_h: Horizontal accuracy (meters) - point-level uncertainty
#'   - accuracy_tier: Quality classification (e.g., A_Excellent, B_Good, C_Moderate)
#'
#' @details
#' Accuracy values represent 1-sigma uncertainties based on survey-specific
#' error analysis using GPS check point validation.
#'
#' ## Lookup Hierarchy
#' Values are assigned in the following order of precedence:
#'
#' 1. **special_cases** (park + year + transect) - Most specific. Use for
#'    transects with different survey methods or equipment than the park/year
#'    default.
#'
#' 2. **accuracy_table** (park + year) - General values for each survey.
#'
#' 3. **default_accuracy** - Conservative fallback for unknown combinations.
#'
#' ## Default Behavior
#' If no accuracy_table is provided, all observations receive conservative
#' default values (equivalent to moderate-quality handheld GPS). A message
#' is logged to inform the user.
#'
#' @examples
#' \dontrun{
#' # Load both tables
#' accuracy_table <- load_accuracy_table("data/accuracy_values.csv")
#' special_cases <- load_special_cases_table("data/special_cases.csv")
#'
#' # Apply with full hierarchy
#' data_with_accuracy <- assign_accuracy(
#'   transect_data,
#'   accuracy_table,
#'   special_cases,
#'   verbose = TRUE
#' )
#'
#' # Without special cases
#' data_with_accuracy <- assign_accuracy(transect_data, accuracy_table)
#'
#' # Without any tables (uses defaults)
#' data_with_accuracy <- assign_accuracy(transect_data)
#' }
#'
#' @importFrom dplyr left_join mutate case_when if_else coalesce
#' @importFrom dplyr group_by summarize arrange filter n select
#' @export
assign_accuracy <- function(
    df,
    accuracy_table = NULL,
    special_cases = NULL,
    default_accuracy = list(sigma_h = 0.030, sigma_v = 0.070, accuracy_tier = "C_Moderate"),
    verbose = FALSE
) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------

  required_cols <- c("park", "year", "transect")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Input dataframe missing required columns: ",
         paste(missing_cols, collapse = ", "),
         "\nRequired: park, year, transect")
  }

  # ---------------------------------------------------------------------------
  # Handle missing accuracy table
  # ---------------------------------------------------------------------------

  if (is.null(accuracy_table)) {
    message("No accuracy table provided. Using conservative default values for all observations.")
    message("  Default sigma_h: ", default_accuracy$sigma_h, " m")
    message("  Default sigma_v: ", default_accuracy$sigma_v, " m")
    message("  Default tier: ", default_accuracy$accuracy_tier)
    message("To use project-specific values, provide an accuracy_table argument.")

    df_with_accuracy <- df |>
      dplyr::mutate(
        sigma_h = default_accuracy$sigma_h,
        sigma_v = default_accuracy$sigma_v,
        accuracy_tier = default_accuracy$accuracy_tier
      )

    # Still apply special cases if provided
    if (!is.null(special_cases)) {
      df_with_accuracy <- apply_special_cases(df_with_accuracy, special_cases)
    }

    if (verbose) {
      print_accuracy_summary(df_with_accuracy, default_accuracy)
    }

    return(df_with_accuracy)
  }

  # ---------------------------------------------------------------------------
  # Validate provided tables
  # ---------------------------------------------------------------------------

  validate_accuracy_table(accuracy_table)

  if (!is.null(special_cases)) {
    validate_special_cases_table(special_cases)
  }

  # ---------------------------------------------------------------------------
  # Check for missing combinations and warn
  # ---------------------------------------------------------------------------

  # Check for park codes in data not present in accuracy table
  data_parks <- unique(df$park[!is.na(df$park)])
  table_parks <- unique(accuracy_table$park)
  unknown_parks <- setdiff(data_parks, table_parks)

  if (length(unknown_parks) > 0) {
    warning("Park codes in data not found in accuracy table: ",
            paste(unknown_parks, collapse = ", "),
            "\nThese will receive default accuracy values.")
  }

  # Check for park/year combinations in data not in accuracy table
  data_combinations <- df |>
    dplyr::distinct(park, year) |>
    dplyr::filter(!is.na(park), !is.na(year))

  table_combinations <- accuracy_table |>
    dplyr::distinct(park, year)

  missing_combinations <- dplyr::anti_join(
    data_combinations,
    table_combinations,
    by = c("park", "year")
  )

  if (nrow(missing_combinations) > 0) {
    combo_labels <- missing_combinations |>
      dplyr::mutate(label = paste0(park, "/", year)) |>
      dplyr::pull(label)
    warning("Park/year combinations in data not found in accuracy table: ",
            paste(combo_labels, collapse = ", "),
            "\nThese will receive default accuracy values (unless in special_cases).")
  }

  # ---------------------------------------------------------------------------
  # Join accuracy values from main table
  # ---------------------------------------------------------------------------

  df_with_accuracy <- df |>
    dplyr::left_join(
      accuracy_table,
      by = c("park", "year")
    )

  # ---------------------------------------------------------------------------
  # Apply special cases (overrides main table)
  # ---------------------------------------------------------------------------

  if (!is.null(special_cases)) {
    # Check for special_cases that override accuracy_table values
    check_special_case_overrides(df, accuracy_table, special_cases, verbose)

    df_with_accuracy <- apply_special_cases(df_with_accuracy, special_cases)
  }

  # ---------------------------------------------------------------------------
  # Apply defaults for missing values
  # ---------------------------------------------------------------------------

  df_with_accuracy <- df_with_accuracy |>
    dplyr::mutate(
      sigma_h = dplyr::if_else(is.na(sigma_h), default_accuracy$sigma_h, sigma_h),
      sigma_v = dplyr::if_else(is.na(sigma_v), default_accuracy$sigma_v, sigma_v),
      accuracy_tier = dplyr::if_else(is.na(accuracy_tier), default_accuracy$accuracy_tier, accuracy_tier)
    )

  # ---------------------------------------------------------------------------
  # Verbose output
  # ---------------------------------------------------------------------------

  if (verbose) {
    print_accuracy_summary(df_with_accuracy, default_accuracy)
  }

  return(df_with_accuracy)
}


#' Check for Special Cases Overriding Accuracy Table
#'
#' Internal helper function that warns when special_cases entries will
#' override values from the main accuracy_table. This is expected behavior,
#' but the warning helps catch accidental overlaps.
#'
#' @param df Data frame being processed (to check which transects are actually present)
#' @param accuracy_table Main accuracy lookup table
#' @param special_cases Special cases override table
#' @param verbose If TRUE, provide detailed output
#'
#' @keywords internal
check_special_case_overrides <- function(df, accuracy_table, special_cases, verbose) {


  # Find park/year combinations that exist in BOTH tables
  accuracy_park_years <- accuracy_table |>
    dplyr::distinct(park, year)

  special_park_years <- special_cases |>
    dplyr::distinct(park, year)

  overlapping <- dplyr::inner_join(
    special_park_years,
    accuracy_park_years,
    by = c("park", "year")
  )


  if (nrow(overlapping) > 0) {
    # Get the specific transects being overridden
    overridden_transects <- special_cases |>
      dplyr::semi_join(overlapping, by = c("park", "year")) |>
      dplyr::distinct(park, year, transect)

    # Check how many of these transects are actually in the data
    transects_in_data <- df |>
      dplyr::semi_join(overridden_transects, by = c("park", "year", "transect")) |>
      dplyr::distinct(park, year, transect)

    if (nrow(transects_in_data) > 0) {
      # Build informative message
      override_summary <- transects_in_data |>
        dplyr::group_by(park, year) |>
        dplyr::summarize(
          n_transects = dplyr::n(),
          transects = paste(transect, collapse = ", "),
          .groups = "drop"
        )

      warning_lines <- override_summary |>
        dplyr::mutate(
          line = paste0("  ", park, "/", year, ": ", n_transects,
                        " transect(s) [", transects, "]")
        ) |>
        dplyr::pull(line)

      message("Note: special_cases overriding accuracy_table for:\n",
              paste(warning_lines, collapse = "\n"),
              "\n(This is expected if these transects used different survey methods.)")
    }
  }
}


#' Apply Special Cases Overrides
#'
#' Internal helper function that applies transect-specific accuracy values
#' from the special_cases table, overriding any existing values.
#'
#' @param df Data frame with sigma_h, sigma_v, accuracy_tier columns
#' @param special_cases Data frame with park, year, transect, sigma_h, sigma_v, accuracy_tier
#'
#' @return Data frame with special case values applied
#'
#' @keywords internal
apply_special_cases <- function(df, special_cases) {

  # Preserve transect factor structure (left_join coerces factor+character to character)
  transect_is_factor <- is.factor(df$transect)
  if (transect_is_factor) {
    transect_levels <- levels(df$transect)
  }

  # Rename special_cases columns to avoid conflicts during join
  special_cases_renamed <- special_cases |>
    dplyr::rename(
      sigma_h_special = sigma_h,
      sigma_v_special = sigma_v,
      accuracy_tier_special = accuracy_tier
    )

  # Convert special_cases transect to factor to match df (prevents type coercion)
  if (transect_is_factor) {
    # Identify transect IDs in special_cases that don't exist in main data
    unknown_transects <- setdiff(
      unique(special_cases_renamed$transect),
      transect_levels
    )

    if (length(unknown_transects) > 0) {
      # Get details about which park/year/transect combos won't match
      unmatched_entries <- special_cases_renamed |>
        dplyr::filter(transect %in% unknown_transects) |>
        dplyr::mutate(label = paste0(park, "/", year, "/", transect)) |>
        dplyr::pull(label)

      warning("Special cases contain transect IDs not found in main data:\n  ",
              paste(unmatched_entries, collapse = "\n  "),
              "\n\nThese entries will NOT be applied. ",
              "Check for typos in special_cases.csv or ensure these transects are in the input data.",
              call. = FALSE)
    }

    special_cases_renamed <- special_cases_renamed |>
      dplyr::mutate(transect = factor(transect, levels = transect_levels))
  }

  # Join and coalesce (special case values take precedence)
  result <- df |>
    dplyr::left_join(
      special_cases_renamed,
      by = c("park", "year", "transect")
    ) |>
    dplyr::mutate(
      sigma_h = dplyr::coalesce(sigma_h_special, sigma_h),
      sigma_v = dplyr::coalesce(sigma_v_special, sigma_v),
      accuracy_tier = dplyr::coalesce(accuracy_tier_special, accuracy_tier)
    ) |>
    dplyr::select(-sigma_h_special, -sigma_v_special, -accuracy_tier_special)

  # Defensive: restore factor if join still coerced to character
  if (transect_is_factor && !is.factor(result$transect)) {
    result <- result |>
      dplyr::mutate(transect = factor(transect, levels = transect_levels))
  }

  return(result)
}


#' Print Accuracy Assignment Summary
#'
#' Internal helper function for verbose output.
#'
#' @param df Data frame with accuracy columns
#' @param default_accuracy Default accuracy values for comparison
#'
#' @keywords internal
print_accuracy_summary <- function(df, default_accuracy) {
  cat("\n=== Accuracy Assignment Summary ===\n")

  summary_table <- df |>
    dplyr::group_by(park, year, accuracy_tier) |>
    dplyr::summarize(
      n_points = dplyr::n(),
      sigma_h_mean = mean(sigma_h, na.rm = TRUE),
      sigma_v_mean = mean(sigma_v, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(park, year)

  print(summary_table, n = Inf)

  # Check for defaults applied
  n_default <- df |>
    dplyr::filter(
      sigma_h == default_accuracy$sigma_h &
        sigma_v == default_accuracy$sigma_v
    ) |>
    nrow()

  if (n_default > 0) {
    cat("\nNote:", n_default, "observations received conservative default accuracy values\n")
    cat("(typically due to unknown park/year combinations)\n")
  }

  cat("\n")
}


# ============================================================================
# UNCERTAINTY CALCULATION
# ============================================================================

#' Calculate Slope-Dependent Uncertainty for Measured Points
#'
#' Computes total elevation uncertainty by propagating both horizontal (sigma_h)
#' and vertical (sigma_v) measurement errors through the profile slope.
#'
#' For each measured point, calculates:
#'   uncertainty = sqrt(sigma_v^2 + (sigma_h * |slope|)^2)
#'
#' Where slope is estimated using centered finite differences (forward/backward
#' differences at endpoints).
#'
#' @param data Data frame containing transect profile measurements with columns:
#'   - distance: horizontal distance along transect (m)
#'   - elevation: vertical elevation (m, NAVD88)
#'   - sigma_h: horizontal position uncertainty (m)
#'   - sigma_v: vertical elevation uncertainty (m)
#'   - transect: transect identifier
#'   - year: survey year
#'
#' @return Data frame with added columns:
#'   - slope: Local slope (dz/dd)
#'   - uncertainty: Total propagated uncertainty (1-sigma)
#'   - uncertainty_95ci: 95% confidence interval (1.96 * uncertainty)
#'
#' @details
#' Slope calculation methods:
#'   - Interior points: centered difference (z[i+1] - z[i-1]) / (d[i+1] - d[i-1])
#'   - First point: forward difference (z[2] - z[1]) / (d[2] - d[1])
#'   - Last point: backward difference (z[n] - z[n-1]) / (d[n] - d[n-1])
#'   - Single point: slope = 0 (no neighbors)
#'
#' The function operates on data grouped by transect and year, so slopes are
#' calculated within each individual profile.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # Apply to grouped data
#' data_with_uncertainty <- profile_data |>
#'   group_by(transect, year) |>
#'   group_modify(~calculate_measured_uncertainty(.x)) |>
#'   ungroup()
#' }
#'
#' @importFrom dplyr arrange mutate
#' @export
calculate_measured_uncertainty <- function(data) {

  # Ensure data is sorted by distance
  data <- data |> dplyr::arrange(distance)

  n <- nrow(data)

  # Initialize slope vector
  slopes <- rep(NA_real_, n)

  # Calculate slopes based on position
  if (n == 1) {
    # Single point: no slope information
    slopes[1] <- 0

  } else if (n == 2) {
    # Two points: use same slope for both
    slope_val <- (data$elevation[2] - data$elevation[1]) /
      (data$distance[2] - data$distance[1])
    slopes <- rep(slope_val, 2)

  } else {
    # Multiple points: use centered, forward, and backward differences

    # First point: forward difference
    slopes[1] <- (data$elevation[2] - data$elevation[1]) /
      (data$distance[2] - data$distance[1])

    # Interior points: centered difference
    for (i in 2:(n-1)) {
      slopes[i] <- (data$elevation[i+1] - data$elevation[i-1]) /
        (data$distance[i+1] - data$distance[i-1])
    }

    # Last point: backward difference
    slopes[n] <- (data$elevation[n] - data$elevation[n-1]) /
      (data$distance[n] - data$distance[n-1])
  }

  # Handle edge case: zero distance interval (duplicate distances)
  slopes[is.infinite(slopes)] <- 0
  slopes[is.nan(slopes)] <- 0

  # Calculate total uncertainty using error propagation
  # σ_total = sqrt(σ_v^2 + (σ_h * |slope|)^2)
  uncertainty_total <- sqrt(
    data$sigma_v^2 + (data$sigma_h * abs(slopes))^2
  )

  # Add calculated values to data
  data <- data |>
    dplyr::mutate(
      slope = slopes,
      uncertainty = uncertainty_total,
      uncertainty_95ci = 1.96 * uncertainty_total
    )

  return(data)
}


#' Add Uncertainty and Classification to Measured Points
#'
#' Wrapper function that applies slope-dependent uncertainty calculation and
#' classifies measured zero-crossings in a single step.
#'
#' @param data_accuracy Data frame with sigma_h and sigma_v columns, plus
#'   transect grouping variables (transect, year, park, cross_island)
#'
#' @return Data frame with uncertainty, confidence, and point_type columns
#'
#' @details
#' This function:
#'   1. Calculates slope-dependent uncertainty for measured points
#'   2. Identifies "measured zero" points (within 1.96*sigma_v of datum)
#'   3. Assigns confidence levels (all measured data = "high")
#'   4. Initializes columns for derived points (extrap_distance, etc.)
#'
#' @examples
#' \dontrun{
#' # Apply after assign_accuracy()
#' data_with_accuracy <- assign_accuracy(transect_data, accuracy_table, special_cases)
#' data_classified <- add_measured_uncertainty(data_with_accuracy)
#' }
#'
#' @importFrom dplyr group_by group_modify ungroup mutate if_else select
#' @export
add_measured_uncertainty <- function(data_accuracy) {

  # Calculate uncertainty with proper slope propagation
  data_with_uncertainty <- data_accuracy |>
    dplyr::group_by(transect, year, park, cross_island) |>
    dplyr::group_modify(~calculate_measured_uncertainty(.x)) |>
    dplyr::ungroup()

  # Classify measured zero-crossings and add metadata
  data_classified <- data_with_uncertainty |>
    dplyr::mutate(
      # Determine if point is within zero tolerance (using sigma_v, NOT uncertainty)
      # This is correct: we only care about vertical uncertainty for zero-crossing
      is_measured_zero = abs(elevation) < 1.96 * sigma_v,
      point_type = dplyr::if_else(is_measured_zero, "measured_zero", "measured"),

      # All measured data has high confidence
      confidence = "high",

      # Initialize columns for derived points (will be populated later)
      extrap_distance = NA_real_
    ) |>
    dplyr::select(-is_measured_zero)

  return(data_classified)
}


# ============================================================================
# VALIDATION AND SUMMARY FUNCTIONS
# ============================================================================

#' Validate Accuracy Assignment
#'
#' Checks that all observations have received valid accuracy values.
#' Useful for quality control after running assign_accuracy().
#'
#' @param df Dataframe with assigned accuracy columns (sigma_h, sigma_v, accuracy_tier)
#' @return Logical. TRUE if all checks pass, FALSE with warnings if issues found.
#'
#' @examples
#' \dontrun{
#' transect_data <- assign_accuracy(transect_data, accuracy_table, special_cases)
#' validate_accuracy_assignment(transect_data)
#' }
#'
#' @export
validate_accuracy_assignment <- function(df) {

  # Check required columns exist
  required_cols <- c("sigma_h", "sigma_v", "accuracy_tier")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    warning("Missing required accuracy columns: ",
            paste(missing_cols, collapse = ", "))
    return(FALSE)
  }

  # Check for NA values
  n_missing_h <- sum(is.na(df$sigma_h))
  n_missing_v <- sum(is.na(df$sigma_v))
  n_missing_tier <- sum(is.na(df$accuracy_tier))

  if (n_missing_h > 0 | n_missing_v > 0 | n_missing_tier > 0) {
    warning("Found missing accuracy values:\n",
            "  sigma_h: ", n_missing_h, " missing\n",
            "  sigma_v: ", n_missing_v, " missing\n",
            "  accuracy_tier: ", n_missing_tier, " missing")
    return(FALSE)
  }

  # Check for invalid/negative values
  n_invalid_h <- sum(df$sigma_h <= 0, na.rm = TRUE)
  n_invalid_v <- sum(df$sigma_v <= 0, na.rm = TRUE)

  if (n_invalid_h > 0 | n_invalid_v > 0) {
    warning("Found invalid (non-positive) accuracy values:\n",
            "  sigma_h: ", n_invalid_h, " invalid\n",
            "  sigma_v: ", n_invalid_v, " invalid")
    return(FALSE)
  }

  # Check tier values are valid
  valid_tiers <- c("A_Excellent", "B_Good", "C_Moderate")
  invalid_tiers <- setdiff(unique(df$accuracy_tier), valid_tiers)

  if (length(invalid_tiers) > 0) {
    warning("Found invalid accuracy tier values: ",
            paste(invalid_tiers, collapse = ", "),
            "\nValid tiers: ", paste(valid_tiers, collapse = ", "))
    return(FALSE)
  }

  # All checks passed
  message("\u2713 All accuracy values validated successfully")
  message("  Total observations: ", nrow(df))
  message("  Vertical accuracy range: ",
          round(min(df$sigma_v), 4), " - ",
          round(max(df$sigma_v), 4), " m")
  message("  Horizontal accuracy range: ",
          round(min(df$sigma_h), 4), " - ",
          round(max(df$sigma_h), 4), " m")

  return(TRUE)
}


#' Get Accuracy Summary Statistics by Year
#'
#' Convenient function to view accuracy distribution across years and parks.
#'
#' @param df Dataframe with assigned accuracy columns (park, year, accuracy_tier,
#'   sigma_v, sigma_h)
#' @return Dataframe with summary statistics grouped by park, year, and accuracy tier
#'
#' @examples
#' \dontrun{
#' data_with_accuracy <- assign_accuracy(transect_data, accuracy_table, special_cases)
#' summary_stats <- summarize_accuracy_by_year(data_with_accuracy)
#' print(summary_stats)
#' }
#'
#' @importFrom dplyr group_by summarize arrange n
#' @export
summarize_accuracy_by_year <- function(df) {
  df |>
    dplyr::group_by(park, year, accuracy_tier) |>
    dplyr::summarize(
      n_observations = dplyr::n(),
      sigma_v_mean = mean(sigma_v, na.rm = TRUE),
      sigma_v_min = min(sigma_v, na.rm = TRUE),
      sigma_v_max = max(sigma_v, na.rm = TRUE),
      sigma_h_mean = mean(sigma_h, na.rm = TRUE),
      sigma_h_min = min(sigma_h, na.rm = TRUE),
      sigma_h_max = max(sigma_h, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(park, year)
}
