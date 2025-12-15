# data_filter.R
# Data filter class for intuitive, semantic data selection
#
# Separates data selection concerns from visualization settings.
# Provides chainable API for building custom filters.
#
# Dependencies: year_selectors.R (for year_selector class)

# ============================================================================
# DATA FILTER CONSTRUCTOR
# ============================================================================

#' Create a data filter object
#'
#' Creates a filter specification for selecting subsets of transect data.
#' Filters are resolved at runtime against actual data, allowing semantic
#' selections like "first year per park" to work correctly across datasets
#' with different survey schedules.
#'
#' Resolution order: Parks -> Years (per-park) -> Transects
#'
#' @param year_selector year_selector object. Semantic year selection (default: years_all())
#' @param parks Character vector. Park codes to include. NULL = all parks
#' @param transects Character vector. Transect IDs to include. NULL = all transects
#' @param exclude_transects Character vector. Transect IDs to exclude
#' @param name Character. Filter name for reference
#' @returns data_filter object
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic filter: baseline + recent years
#' filter <- create_data_filter(
#'   year_selector = years_baseline_recent(2)
#' )
#'
#' # Park-specific filter
#' filter <- create_data_filter(
#'   year_selector = years_recent(3),
#'   parks = "PAIS"
#' )
#'
#' # Exclude specific transects
#' filter <- create_data_filter(
#'   year_selector = years_all(),
#'   exclude_transects = c("PAIS_T001", "GUIS_T015")
#' )
#' }
#'
#'  @seealso
#' Year selectors: \code{\link{years_all}}, \code{\link{years_first}},
#' \code{\link{years_recent}}, \code{\link{years_baseline_recent}},
#' \code{\link{years_range}}, \code{\link{years_explicit}}
#'
#' See all: \code{\link{list_year_selectors}}
#'
#' Related: \code{\link{apply_filter}}, \code{\link{resolve_filter}},
#' \code{\link{preview_year_selection}}
create_data_filter <- function(
    year_selector = NULL,
    parks = NULL,
    transects = NULL,
    exclude_transects = NULL,
    name = "custom") {
  # Default to all years if no selector provided
  if (is.null(year_selector)) {
    year_selector <- years_all()
  }

  # Validate year_selector

  if (!inherits(year_selector, "year_selector")) {
    stop("year_selector must be a year_selector object. Use years_*() functions.",
      call. = FALSE
    )
  }

  structure(
    list(
      year_selector = year_selector,
      parks = parks,
      transects = transects,
      exclude_transects = exclude_transects,
      name = name,
      created = Sys.time()
    ),
    class = "data_filter"
  )
}

# ============================================================================
# CHAINABLE MODIFIERS (Pipe-Friendly API)
# ============================================================================

#' Set year selector for filter
#'
#' @param filter data_filter object
#' @param selector year_selector object
#' @returns Modified data_filter object
#' @export
#'
#' @examples
#' filter <- create_data_filter() |>
#'   set_years(years_recent(3))
set_years <- function(filter, selector) {
  if (!inherits(filter, "data_filter")) {
    stop("filter must be a data_filter object", call. = FALSE)
  }
  if (!inherits(selector, "year_selector")) {
    stop("selector must be a year_selector object", call. = FALSE)
  }

  filter$year_selector <- selector
  filter
}

#' Set park filter
#'
#' @param filter data_filter object
#' @param parks Character vector. Park codes to include
#' @returns Modified data_filter object
#' @export
#'
#' @examples
#' filter <- create_data_filter() |>
#'   set_parks(c("PAIS", "GUIS"))
set_parks <- function(filter, parks) {
  if (!inherits(filter, "data_filter")) {
    stop("filter must be a data_filter object", call. = FALSE)
  }

  filter$parks <- parks
  filter
}

#' Set transect filter
#'
#' @param filter data_filter object
#' @param transects Character vector. Transect IDs to include
#' @returns Modified data_filter object
#' @export
#'
#' @examples
#' filter <- create_data_filter() |>
#'   set_transects(c("PAIS_T001", "PAIS_T002"))
set_transects <- function(filter, transects) {
  if (!inherits(filter, "data_filter")) {
    stop("filter must be a data_filter object", call. = FALSE)
  }

  filter$transects <- transects
  filter
}

#' Exclude specific transects
#'
#' @param filter data_filter object
#' @param transects Character vector. Transect IDs to exclude
#' @returns Modified data_filter object
#' @export
#'
#' @examples
#' filter <- create_data_filter() |>
#'   exclude_transects(c("PAIS_T001", "GUIS_T015"))
exclude_transects <- function(filter, transects) {
  if (!inherits(filter, "data_filter")) {
    stop("filter must be a data_filter object", call. = FALSE)
  }

  filter$exclude_transects <- c(filter$exclude_transects, transects)
  filter
}

#' Set filter name
#'
#' @param filter data_filter object
#' @param name Character. Filter name
#' @returns Modified data_filter object
#' @export
#'
#' @examples
#' filter <- create_data_filter() |>
#'   set_name("baseline_comparison")
set_name <- function(filter, name) {
  if (!inherits(filter, "data_filter")) {
    stop("filter must be a data_filter object", call. = FALSE)
  }

  filter$name <- name
  filter
}

# ============================================================================
# FILTER RESOLUTION
# ============================================================================

#' Resolve filter against data
#'
#' Applies the semantic filter to actual data, resolving year selections
#' per-park and computing final year/park/transect lists.
#'
#' Resolution order:
#' 1. Parks: Determine which parks are in scope
#' 2. Years: Apply year_selector to each park's years, union results
#' 3. Transects: Filter and apply exclusions
#'
#' @param filter data_filter object
#' @param data Dataframe with transect data (must have park, year, transect columns)
#' @returns List with resolved values:
#'   - parks: Character vector of resolved park codes
#'   - years: Numeric vector of resolved years (union across parks)
#'   - transects: Character vector of resolved transect IDs
#'   - park_year_details: List with per-park year selection details
#' @export
#'
#' @examples
#' \dontrun{
#' filter <- create_data_filter(year_selector = years_baseline_recent(2))
#' resolved <- resolve_filter(filter, data)
#' print(resolved)
#' }
resolve_filter <- function(filter, data) {
  if (!inherits(filter, "data_filter")) {
    stop("filter must be a data_filter object", call. = FALSE)
  }

  # Validate data has required columns

  required_cols <- c("park", "year", "transect")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data missing required columns: ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ========================================================================
  # Step 1: Resolve parks
  # ========================================================================
  available_parks <- unique(data$park)

  if (!is.null(filter$parks)) {
    active_parks <- intersect(filter$parks, available_parks)
    if (length(active_parks) == 0) {
      warning("None of the specified parks found in data: ",
        paste(filter$parks, collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    active_parks <- available_parks
  }

  # ========================================================================
  # Step 2: Resolve years (per-park, then union)
  # ========================================================================
  all_selected_years <- c()
  park_year_details <- list()

  for (park in active_parks) {
    # Get this park's available years
    park_years <- data[data$park == park, "year", drop = TRUE]
    park_years <- sort(unique(park_years))

    if (length(park_years) == 0) {
      next
    }

    # Apply year selector to this park's years
    selected <- filter$year_selector$select(park_years)

    # Store details for this park
    park_year_details[[park]] <- list(
      available = park_years,
      selected = selected
    )

    # Add to combined selection
    all_selected_years <- c(all_selected_years, selected)
  }

  resolved_years <- sort(unique(all_selected_years))

  # ========================================================================
  # Step 3: Resolve transects
  # ========================================================================
  # Get transects in scope (matching parks and years)
  in_scope_data <- data[
    data$park %in% active_parks &
      (length(resolved_years) == 0 | data$year %in% resolved_years),
  ]
  available_transects <- unique(in_scope_data$transect)

  if (!is.null(filter$transects)) {
    resolved_transects <- intersect(filter$transects, available_transects)
  } else {
    resolved_transects <- available_transects
  }

  # Apply exclusions

  if (!is.null(filter$exclude_transects)) {
    resolved_transects <- setdiff(resolved_transects, filter$exclude_transects)
  }

  resolved_transects <- sort(resolved_transects)

  # ========================================================================
  # Build result
  # ========================================================================
  result <- structure(
    list(
      parks = active_parks,
      years = resolved_years,
      transects = resolved_transects,
      park_year_details = park_year_details,
      filter_name = filter$name,
      selector_type = filter$year_selector$type
    ),
    class = "resolved_filter"
  )

  return(result)
}

#' Apply filter to data
#'
#' Resolves the filter and returns filtered dataframes. This is the main
#' function for actually filtering data using a data_filter object.
#'
#' @param filter data_filter object
#' @param data Dataframe with transect data (must have park, year, transect columns)
#' @param auc_results Optional dataframe with AUC results (filtered to match)
#' @param verbose Boolean. Print filtering summary. Default FALSE
#' @returns List with:
#'   - data: Filtered transect dataframe
#'   - auc_results: Filtered AUC dataframe (if provided)
#'   - resolved: The resolved_filter object with selection details
#' @export
#'
#' @examples
#' \dontrun{
#' filter <- create_data_filter(year_selector = years_baseline_recent(2))
#' result <- apply_filter(filter, data, auc_results)
#' filtered_data <- result$data
#' filtered_auc <- result$auc_results
#' }
apply_filter <- function(filter, data, auc_results = NULL, verbose = FALSE) {
  # Resolve the filter first
  resolved <- resolve_filter(filter, data)

  original_rows <- nrow(data)

  # Filter the main data
  filtered_data <- data[
    data$park %in% resolved$parks &
      data$year %in% resolved$years &
      data$transect %in% resolved$transects,
  ]

  # Filter AUC results to match
  filtered_auc <- NULL
  if (!is.null(auc_results)) {
    # Get unique transect-year combinations from filtered data
    valid_combinations <- unique(filtered_data[, c("transect", "year")])

    filtered_auc <- auc_results[
      paste(auc_results$transect, auc_results$year) %in%
        paste(valid_combinations$transect, valid_combinations$year),
    ]
  }

  if (verbose) {
    cat("Filter applied:", filter$name, "\n")
    cat("  Rows:", original_rows, "->", nrow(filtered_data), "\n")
    cat("  Parks:", paste(resolved$parks, collapse = ", "), "\n")
    cat("  Years:", paste(resolved$years, collapse = ", "), "\n")
    cat("  Transects:", length(resolved$transects), "\n")
  }

  return(list(
    data = filtered_data,
    auc_results = filtered_auc,
    resolved = resolved
  ))
}

# ============================================================================
# PRINT METHODS
# ============================================================================

#' Print data filter summary
#'
#' @param x data_filter object
#' @param ... Additional arguments (unused)
#' @export
print.data_filter <- function(x, ...) {
  cat("\nData Filter:", x$name, "\n")
  cat(strrep("-", 50), "\n")

  cat("Year Selection: ")
  print(x$year_selector)

  cat("Parks:          ",
    if (is.null(x$parks)) "All" else paste(x$parks, collapse = ", "), "\n"
  )
  cat("Transects:      ",
    if (is.null(x$transects)) "All" else paste(x$transects, collapse = ", "), "\n"
  )
  if (!is.null(x$exclude_transects)) {
    cat("Excluded:       ", paste(x$exclude_transects, collapse = ", "), "\n")
  }
  cat("\n")

  invisible(x)
}

#' Print resolved filter summary
#'
#' @param x resolved_filter object
#' @param ... Additional arguments (unused)
#' @export
print.resolved_filter <- function(x, ...) {
  cat("\nResolved Data Filter:", x$filter_name, "\n")
  cat(strrep("=", 60), "\n\n")

  cat("Parks:     ", paste(x$parks, collapse = ", "), "\n")
  cat("Years:     ", paste(x$years, collapse = ", "), "\n")
  cat("Transects: ", length(x$transects), " total\n\n")

  if (length(x$park_year_details) > 0) {
    cat("Year Selection by Park:\n")
    for (park in names(x$park_year_details)) {
      info <- x$park_year_details[[park]]
      cat("  ", park, ":\n")
      cat("    Available: ", paste(info$available, collapse = ", "), "\n")
      cat("    Selected:  ", paste(info$selected, collapse = ", "), "\n")
    }
    cat("\n")
  }

  invisible(x)
}

# ============================================================================
# UTILITY FUNCTIONS (devtools for troubleshooting)
# ============================================================================

#' Check if object is a data_filter
#'
#' @param x Object to check
#' @returns Logical
#' @keywords internal
#' @noRd
is_data_filter <- function(x) {
  inherits(x, "data_filter")
}

#' Check if object is a resolved_filter
#'
#' @param x Object to check
#' @returns Logical
#' @keywords internal
#' @noRd
is_resolved_filter <- function(x) {
  inherits(x, "resolved_filter")
}

# ============================================================================
# QUERY HELPERS
# ============================================================================

#' Get available values for filtering
#'
#' Returns all unique values for parks, transects, and years in dataset.
#'
#' @param data Dataframe with transect data
#' @returns List with available parks, transects, years
#' @export
#'
#' @examples
#' \dontrun{
#' available <- get_available_values(data)
#' print(available$parks)
#' print(available$years)
#' }
get_available_values <- function(data) {
  list(
    parks = sort(unique(data$park)),
    transects = sort(unique(data$transect)),
    years = sort(unique(data$year))
  )
}

#' Get transects for a specific park
#'
#' @param data Dataframe with transect data
#' @param park Character. Park code
#' @returns Character vector of transect IDs
#' @export
#'
#' @examples
#' \dontrun{
#' pais_transects <- get_transects_by_park(data, "PAIS")
#' }
get_transects_by_park <- function(data, park) {
  sort(unique(data$transect[data$park == park]))
}

#' Get years for a specific transect
#'
#' @param data Dataframe with transect data
#' @param transect Character. Transect ID
#' @returns Numeric vector of years
#' @export
#'
#' @examples
#' \dontrun{
#' years <- get_years_by_transect(data, "PAIS_T001")
#' }
get_years_by_transect <- function(data, transect) {
  sort(unique(data$year[data$transect == transect]))
}

#' Get transects measured in a specific year
#'
#' @param data Dataframe with transect data
#' @param year Numeric. Year
#' @returns Character vector of transect IDs
#' @export
#'
#' @examples
#' \dontrun{
#' transects_2020 <- get_transects_by_year(data, 2020)
#' }
get_transects_by_year <- function(data, year) {
  sort(unique(data$transect[data$year == year]))
}

#' Check data availability for specific criteria
#'
#' @param data Dataframe with transect data
#' @param parks Character vector. Park codes (NULL = all)
#' @param transects Character vector. Transect IDs (NULL = all)
#' @param years Numeric vector. Years (NULL = all)
#' @returns Dataframe with availability summary
#' @export
#'
#' @examples
#' \dontrun{
#' # Check which combinations exist
#' availability <- check_data_availability(
#'   data,
#'   parks = c("PAIS", "GUIS"),
#'   years = c(2015, 2020)
#' )
#' }
check_data_availability <- function(data, parks = NULL, transects = NULL, years = NULL) {
  # Use all values if not specified
  if (is.null(parks)) parks <- unique(data$park)
  if (is.null(transects)) transects <- unique(data$transect)
  if (is.null(years)) years <- unique(data$year)

  # Create all combinations
  combinations <- expand.grid(
    park = parks,
    transect = transects,
    year = years,
    stringsAsFactors = FALSE
  )

  # Check which combinations exist in data
  data_key <- paste(data$park, data$transect, data$year, sep = "|")
  combo_key <- paste(combinations$park, combinations$transect, combinations$year, sep = "|")

  combinations$available <- combo_key %in% data_key
  combinations <- combinations[order(combinations$park, combinations$transect, combinations$year), ]

  return(combinations)
}
