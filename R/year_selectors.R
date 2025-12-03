# year_selectors.R
#' Year Selector Functions for Data Filtering
#'
#' @description
#' Semantic year selection functions for intuitive data filtering in coastal
#' monitoring analysis. Year selectors are functions that operate on per-park
#' year vectors. When resolved, they are applied to each park's available years
#' independently, then results are unioned across parks.
#'
#' @name year_selectors
#' @keywords internal
NULL

# ============================================================================
# YEAR SELECTOR CONSTRUCTOR
# ============================================================================

#' Create a year selector object
#'
#' Internal constructor for year selector objects. This function is used
#' internally by all year selector functions to create consistent selector
#' objects with a standard structure.
#'
#' @param type Character. Selector type name for identification and printing
#' @param select Function. Takes a sorted numeric vector of years, returns
#'   selected years based on the selector's logic
#' @param ... Additional metadata to store in the selector object (e.g., n, from, to)
#'
#' @return A \code{year_selector} object, which is a list with class
#'   \code{"year_selector"} containing the type, selection function, and any
#'   additional metadata
#'
#' @keywords internal
#' @noRd
create_year_selector <- function(type, select, ...) {
  structure(
    list(
      type = type,
      select = select,
      ...
    ),
    class = "year_selector"
  )
}

# ============================================================================
# BASIC SELECTORS
# ============================================================================

#' Select all available years
#'
#' Returns all years available for each park. This is the default behavior
#' when no year filter is specified.
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # Explicit "all years" selection
#' selector <- years_all()
#' print(selector)
years_all <- function() {
  create_year_selector(
    type = "all",
    select = function(park_years) park_years
  )
}

#' Select first N survey years
#'
#' Returns the first N survey years for each park. Useful for baseline
#' comparisons where parks may have started monitoring in different years.
#' Applied independently to each park's available years.
#'
#' @param n Integer. Number of first years to select (default: 1)
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # Get baseline year for each park
#' selector <- years_first(1)
#'
#' # Get first 2 survey years per park
#' selector <- years_first(2)
years_first <- function(n = 1) {
  if (!is.numeric(n) || n < 1) {
    stop("n must be a positive integer", call. = FALSE)
  }
  n <- as.integer(n)

  create_year_selector(
    type = "first",
    n = n,
    select = function(park_years) {
      sorted <- sort(park_years)
      head(sorted, min(n, length(sorted)))
    }
  )
}

#' Select most recent N survey years
#'
#' Returns the last N survey years for each park. Parks with different
#' survey schedules will contribute their own recent years to the union.
#' Applied independently to each park's available years.
#'
#' @param n Integer. Number of recent years to select (default: 2)
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # Get last 2 survey years per park
#' selector <- years_recent(2)
#'
#' # Get most recent year only
#' selector <- years_recent(1)
years_recent <- function(n = 2) {
  if (!is.numeric(n) || n < 1) {
    stop("n must be a positive integer", call. = FALSE)
  }
  n <- as.integer(n)

  create_year_selector(
    type = "recent",
    n = n,
    select = function(park_years) {
      sorted <- sort(park_years)
      tail(sorted, min(n, length(sorted)))
    }
  )
}

#' Select first year plus most recent N years
#'
#' Combines baseline and recent year selection. For each park, selects
#' the first survey year and the last N survey years. Useful for
#' temporal comparison showing change from baseline to current conditions.
#'
#' @param n_recent Integer. Number of recent years to include (default: 2)
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # Baseline + last 2 years per park
#' selector <- years_baseline_recent(2)
#'
#' # Baseline + last year
#' selector <- years_baseline_recent(1)
years_baseline_recent <- function(n_recent = 2) {
  if (!is.numeric(n_recent) || n_recent < 1) {
    stop("n_recent must be a positive integer", call. = FALSE)
  }
  n_recent <- as.integer(n_recent)

  create_year_selector(
    type = "baseline_recent",
    n_recent = n_recent,
    select = function(park_years) {
      sorted <- sort(park_years)
      first <- head(sorted, 1)
      recent <- tail(sorted, min(n_recent, length(sorted)))
      sort(unique(c(first, recent)))
    }
  )
}

# ============================================================================
# RANGE-BASED SELECTORS
# ============================================================================

#' Select years within a range
#'
#' Returns years that fall within the specified range (inclusive).
#' Applied per-park, so only years actually surveyed are returned.
#'
#' @param from Numeric. Start year (inclusive)
#' @param to Numeric. End year (inclusive)
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # Years between 2015 and 2020
#' selector <- years_range(2015, 2020)
years_range <- function(from, to) {
  if (!is.numeric(from) || !is.numeric(to)) {
    stop("from and to must be numeric years", call. = FALSE)
  }
  if (from > to) {
    stop("from must be <= to", call. = FALSE)
  }

  create_year_selector(
    type = "range",
    from = from,
    to = to,
    select = function(park_years) {
      park_years[park_years >= from & park_years <= to]
    }
  )
}

#' Select years since a given year
#'
#' Returns all years greater than or equal to the specified year.
#' Useful for filtering to recent time periods.
#'
#' @param year Numeric. Start year (inclusive)
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # All surveys from 2018 onward
#' selector <- years_since(2018)
years_since <- function(year) {
  if (!is.numeric(year)) {
    stop("year must be numeric", call. = FALSE)
  }

  create_year_selector(
    type = "since",
    year = year,
    select = function(park_years) {
      park_years[park_years >= year]
    }
  )
}

#' Select years up to a given year
#'
#' Returns all years less than or equal to the specified year.
#' Useful for historical analysis.
#'
#' @param year Numeric. End year (inclusive)
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # All surveys up to 2015
#' selector <- years_until(2015)
years_until <- function(year) {
  if (!is.numeric(year)) {
    stop("year must be numeric", call. = FALSE)
  }

  create_year_selector(
    type = "until",
    year = year,
    select = function(park_years) {
      park_years[park_years <= year]
    }
  )
}

# ============================================================================
# EXPLICIT SELECTION
# ============================================================================

#' Select specific years explicitly
#'
#' Returns only the specified years. Unlike other selectors, this is
#' park-independent - it returns the intersection of requested years
#' and available years for each park.
#'
#' @param ... Numeric. Years to select
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # Specific years only
#' selector <- years_explicit(2010, 2015, 2020)
years_explicit <- function(...) {
  years <- c(...)
  if (length(years) == 0) {
    stop("At least one year must be specified", call. = FALSE)
  }
  if (!is.numeric(years)) {
    stop("years must be numeric", call. = FALSE)
  }

  create_year_selector(
    type = "explicit",
    years = sort(unique(years)),
    select = function(park_years) {
      intersect(years, park_years)
    }
  )
}

# ============================================================================
# COMPARISON SELECTORS
# ============================================================================

#' Select years for baseline vs current comparison
#'
#' Specifically designed for two-point comparisons. Selects the first
#' survey year (baseline) and the most recent survey year (current)
#' for each park.
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # Compare first vs last year per park
#' selector <- years_compare_endpoints()
years_compare_endpoints <- function() {
  create_year_selector(
    type = "compare_endpoints",
    select = function(park_years) {
      sorted <- sort(park_years)
      unique(c(head(sorted, 1), tail(sorted, 1)))
    }
  )
}

# ============================================================================
# COMBINING SELECTORS
# ============================================================================

#' Combine multiple year selectors
#'
#' Creates a selector that unions the results of multiple selectors.
#' Each selector is applied to the park's years, and results are combined.
#'
#' @param ... \code{year_selector} objects to combine
#'
#' @return A \code{year_selector} object
#'
#' @export
#'
#' @examples
#' # First year + years since 2018
#' combined <- years_combine(
#'   years_first(1),
#'   years_since(2018)
#' )
years_combine <- function(...) {
  selectors <- list(...)

  # Validate all are year_selectors
  for (i in seq_along(selectors)) {
    if (!inherits(selectors[[i]], "year_selector")) {
      stop("All arguments must be year_selector objects", call. = FALSE)
    }
  }

  create_year_selector(
    type = "combined",
    selectors = selectors,
    select = function(park_years) {
      all_selected <- c()
      for (selector in selectors) {
        selected <- selector$select(park_years)
        all_selected <- c(all_selected, selected)
      }
      sort(unique(all_selected))
    }
  )
}

# ============================================================================
# ANNUAL PROGRESSION SELECTOR
# ============================================================================

#' Create selector for annual progression mode
#'
#' Special selector used by annual visualization mode. For a given target year,
#' returns: first year, year before target (if different), and target year.
#'
#' This is typically not called directly by users but used internally
#' by the annual visualization system.
#'
#' @param target_year Numeric. The target year for progression
#'
#' @return A \code{year_selector} object
#'
#' @keywords internal
#'
#' @export
years_annual_progression <- function(target_year) {
  if (!is.numeric(target_year)) {
    stop("target_year must be numeric", call. = FALSE)
  }

  create_year_selector(
    type = "annual_progression",
    target_year = target_year,
    select = function(park_years) {
      sorted <- sort(park_years)
      first_year <- sorted[1]

      # Find target in this park's years
      if (!target_year %in% sorted) {
        # Target year not available for this park
        return(integer(0))
      }

      target_idx <- which(sorted == target_year)

      if (target_year == first_year) {
        # Case 1: Target is first year - only that year
        return(target_year)
      }

      year_before <- sorted[target_idx - 1]

      if (year_before == first_year) {
        # Case 2: Year before is first - return first + target
        return(c(first_year, target_year))
      }

      # Case 3: Standard - first + year_before + target
      c(first_year, year_before, target_year)
    }
  )
}

# ============================================================================
# DISCOVERY & HELP FUNCTIONS
# ============================================================================

#' List all available year selectors
#'
#' Returns a summary of all year selector functions with descriptions.
#' Useful for discovering which selectors are available and their purpose.
#'
#' @param as_df Logical. If \code{TRUE}, return a data frame. If \code{FALSE}
#'   (default), print formatted list.
#'
#' @return If \code{as_df=TRUE}, returns a data frame with columns Function,
#'   Description, and Example. Otherwise prints formatted output and returns
#'   the data frame invisibly.
#'
#' @export
#'
#' @examples
#' # Print formatted list
#' list_year_selectors()
#'
#' # Get as data frame for programmatic use
#' selectors <- list_year_selectors(as_df = TRUE)
list_year_selectors <- function(as_df = FALSE) {
  selectors <- data.frame(
    Function = c(
      "years_all()",
      "years_first(n)",
      "years_recent(n)",
      "years_baseline_recent(n)",
      "years_range(from, to)",
      "years_since(year)",
      "years_until(year)",
      "years_explicit(...)",
      "years_compare_endpoints()",
      "years_combine(...)"
    ),
    Description = c(
      "Select all available years",
      "Select first n survey years per park",
      "Select most recent n survey years per park",
      "Select first year + last n years per park (temporal comparison)",
      "Select years within a range (inclusive)",
      "Select all years >= specified year",
      "Select all years <= specified year",
      "Select specific years by number",
      "Select first and last year only (two-point comparison)",
      "Combine multiple selectors (union)"
    ),
    Example = c(
      "years_all()",
      "years_first(2)",
      "years_recent(3)",
      "years_baseline_recent(2)",
      "years_range(2015, 2020)",
      "years_since(2018)",
      "years_until(2015)",
      "years_explicit(2010, 2015, 2020)",
      "years_compare_endpoints()",
      "years_combine(years_first(1), years_recent(2))"
    ),
    stringsAsFactors = FALSE
  )

  if (as_df) {
    return(selectors)
  }

  # Print formatted output
  cat("\n")
  cat("Available Year Selectors\n")
  cat(strrep("=", 70), "\n\n")

  for (i in seq_len(nrow(selectors))) {
    cat(selectors$Function[i], "\n")
    cat("  ", selectors$Description[i], "\n")
    cat("  Example: ", selectors$Example[i], "\n\n")
  }

  cat("Usage:\n")
  cat("  These selectors can be used with data filtering functions\n")
  cat("  to control which years are included in analysis or visualization.\n\n")

  invisible(selectors)
}

#' Preview year selection before running visualization
#'
#' Shows which years would be selected by a year selector when applied to
#' actual data. Useful for verifying your selection logic before generating
#' plots or running analysis.
#'
#' @param selector A \code{year_selector} object to preview
#' @param data Data frame with monitoring data. Must have a \code{year} column.
#'   If a \code{park} column is present, selection is applied per-park.
#' @param verbose Logical. If \code{TRUE} (default), print formatted output.
#'   If \code{FALSE}, return results silently.
#'
#' @return A list with selection details:
#'   \itemize{
#'     \item If data has park column: list with \code{by_park} (list of
#'       selection info per park) and \code{combined} (vector of all selected years)
#'     \item If no park column: list with \code{available}, \code{selected},
#'       \code{n_available}, and \code{n_selected}
#'   }
#'   Returns invisibly if \code{verbose=TRUE}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Preview what years_baseline_recent(2) would select
#' preview_year_selection(years_baseline_recent(2), data)
#'
#' # Compare different selectors
#' preview_year_selection(years_recent(3), data)
#' preview_year_selection(years_compare_endpoints(), data)
#' }
preview_year_selection <- function(selector, data, verbose = TRUE) {
  # Validate inputs
  if (!inherits(selector, "year_selector")) {
    stop("selector must be a year_selector object. Use years_*() functions.",
         call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }

  if (!"year" %in% names(data)) {
    stop("data must have a 'year' column", call. = FALSE)
  }

  # Check if data has park column
  has_parks <- "park" %in% names(data)

  if (has_parks) {
    # Apply selector per-park
    parks <- unique(data$park)
    result <- list()

    for (park in parks) {
      park_years <- sort(unique(data$year[data$park == park]))
      selected <- selector$select(park_years)

      result[[park]] <- list(
        available = park_years,
        selected = selected,
        n_available = length(park_years),
        n_selected = length(selected)
      )
    }

    # Compute combined selection
    all_selected <- sort(unique(unlist(lapply(result, function(x) x$selected))))

    if (verbose) {
      cat("\n")
      cat("Year Selection Preview\n")
      cat(strrep("=", 60), "\n\n")
      cat("Selector: ", selector$type, "\n\n")

      for (park in names(result)) {
        info <- result[[park]]
        cat(park, ":\n")
        cat("  Available (", info$n_available, "): ",
            paste(info$available, collapse = ", "), "\n", sep = "")
        cat("  Selected  (", info$n_selected, "): ",
            if (length(info$selected) == 0) "(none)" else paste(info$selected, collapse = ", "),
            "\n\n", sep = "")
      }

      cat("Combined selection: ", paste(all_selected, collapse = ", "), "\n")
      cat("Total years: ", length(all_selected), "\n\n")

      invisible(list(by_park = result, combined = all_selected))
    } else {
      return(list(by_park = result, combined = all_selected))
    }

  } else {
    # No park column - apply to all years
    all_years <- sort(unique(data$year))
    selected <- selector$select(all_years)

    result <- list(
      available = all_years,
      selected = selected,
      n_available = length(all_years),
      n_selected = length(selected)
    )

    if (verbose) {
      cat("\n")
      cat("Year Selection Preview\n")
      cat(strrep("=", 60), "\n\n")
      cat("Selector: ", selector$type, "\n\n")
      cat("Available (", result$n_available, "): ",
          paste(result$available, collapse = ", "), "\n", sep = "")
      cat("Selected  (", result$n_selected, "): ",
          if (length(result$selected) == 0) "(none)" else paste(result$selected, collapse = ", "),
          "\n\n", sep = "")

      invisible(result)
    } else {
      return(result)
    }
  }
}

# ============================================================================
# PRINT METHOD
# ============================================================================

#' Print method for year selector objects
#'
#' Displays a human-readable summary of a year selector object, including
#' its type and key parameters.
#'
#' @param x A \code{year_selector} object
#' @param ... Additional arguments (unused, for S3 method consistency)
#'
#' @return The input object \code{x}, invisibly
#'
#' @export
#' @method print year_selector
print.year_selector <- function(x, ...) {
  cat("Year Selector: ", x$type, "\n")

  switch(x$type,
    "all" = cat("  Selects all available years\n"),
    "first" = cat("  Selects first", x$n, "year(s) per park\n"),
    "recent" = cat("  Selects last", x$n, "year(s) per park\n"),
    "baseline_recent" = cat("  Selects first year +", x$n_recent, "recent year(s) per park\n"),
    "range" = cat("  Selects years from", x$from, "to", x$to, "\n"),
    "since" = cat("  Selects years >=", x$year, "\n"),
    "until" = cat("  Selects years <=", x$year, "\n"),
    "explicit" = cat("  Selects specific years:", paste(x$years, collapse = ", "), "\n"),
    "compare_endpoints" = cat("  Selects first and last year per park\n"),
    "combined" = cat("  Combines", length(x$selectors), "selectors\n"),
    "annual_progression" = cat("  Annual progression for target year", x$target_year, "\n"),
    cat("  Custom selector\n")
  )

  invisible(x)
}
