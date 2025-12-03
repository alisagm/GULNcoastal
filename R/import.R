# import.R
# ============================================================================
# DATA IMPORT FUNCTIONS FOR BEACH TRANSECT PROFILES
# ============================================================================

#' Import Beach Transect Profile Data
#'
#' Reads and validates beach transect profile data from a CSV file. The function
#' ensures required columns are present, converts data types appropriately, and
#' creates proper factor levels for transect ordering.
#'
#' @param file_path Character string. Path to CSV file containing transect data.
#'
#' @return A data frame with columns:
#'   \item{transect}{Factor. Transect identifier with natural sorting}
#'   \item{year}{Numeric. Survey year}
#'   \item{distance}{Numeric. Distance along transect (meters)}
#'   \item{elevation}{Numeric. Elevation measurement (meters)}
#'   \item{cross_island}{Logical. TRUE if transect crosses island, FALSE otherwise}
#'
#' @details The function performs the following operations:
#'   \itemize{
#'     \item Validates that the file exists
#'     \item Reads CSV with flexible column type detection
#'     \item Checks for required columns: transect, year, distance, elevation
#'     \item Converts transect to factor with natural numeric sorting
#'     \item Ensures year, distance, and elevation are numeric
#'     \item Handles optional cross_island column (converts "yes" to TRUE)
#'   }
#'
#' @examples
#' \dontrun{
#' # Import transect data from CSV
#' transects <- import_profile_transects("path/to/transects.csv")
#' }
#'
#' @importFrom readr read_csv cols col_guess
#' @importFrom dplyr select mutate any_of
#' @importFrom stringr str_sort
#' @export
import_profile_transects <- function(file_path) {
  # Validate file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  # Read file with flexible column types - let read_csv guess types
  # Only keep the columns we need
  df <- readr::read_csv(file_path,
                 col_types = readr::cols(.default = readr::col_guess()),
                 name_repair = "minimal",
                 show_col_types = FALSE)

  # Validate required columns are present
  required_cols <- c("transect", "year", "distance", "elevation")
  missing_required <- setdiff(required_cols, names(df))
  if (length(missing_required) > 0) {
    stop(paste("Missing required columns:", paste(missing_required, collapse = ", ")))
  }

  # create factor levels for proper ordering
  transect_levels <- stringr::str_sort(unique(df$transect), numeric=TRUE)

  # Select only the columns we need and ensure correct types
  df <- df |>
    dplyr::select(transect, year, distance, elevation,
           dplyr::any_of("cross_island")) |>  # Include cross_island if it exists
    dplyr::mutate(
      transect = factor(transect, levels = transect_levels),
      year = as.numeric(year),
      distance = as.numeric(distance),
      elevation = as.numeric(elevation)
    )

  # Handle cross_island column: convert "yes" to TRUE, or add FALSE if missing
  if ("cross_island" %in% names(df)) {
    df <- df |> dplyr::mutate(cross_island = cross_island == "yes")
  } else {
    df <- df |> dplyr::mutate(cross_island = FALSE)
  }

  return(df)
}


#' Import Transects with Park Identifier
#'
#' Imports beach transect profile data and adds a park identifier based on the
#' filename. This is a wrapper around \code{import_profile_transects} that
#' automatically detects park codes from filenames.
#'
#' @param file_path Character string. Path to input CSV file.
#'
#' @return A data frame with all columns from \code{import_profile_transects}
#'   plus an additional column:
#'   \item{park}{Character. Park identifier extracted from filename (e.g., "GUIS", "PAIS")}
#'
#' @details The function extracts park identifiers from filenames using
#'   case-insensitive pattern matching:
#'   \itemize{
#'     \item Files containing "guis" are assigned park = "GUIS"
#'     \item Files containing "pais" are assigned park = "PAIS"
#'     \item Files not matching known patterns get park = NA
#'   }
#'
#' @seealso \code{\link{import_profile_transects}}
#'
#' @examples
#' \dontrun{
#' # Import GUIS transect data
#' guis_data <- import_transects_park("guis_transects_2023.csv")
#'
#' # Import PAIS transect data
#' pais_data <- import_transects_park("PAIS_profiles.csv")
#' }
#'
#' @importFrom dplyr mutate case_when
#' @export
import_transects_park <- function(file_path) {
  df <- import_profile_transects(file_path)

  # get park from filename
  filename <- basename(file_path)
  park_value <- dplyr::case_when(
    grepl("guis", filename, ignore.case=TRUE) ~ "GUIS",
    grepl("PAIS", filename, ignore.case=TRUE) ~ "PAIS",
    TRUE ~ NA_character_
  )

  df |> dplyr::mutate(park=park_value)
}

# ============================================================================
# DATA LOADING FUNCTIONS
# ============================================================================

#' Load Transect Analysis Results
#'
#' Loads previously saved transect analysis results from an RDS file.
#' Automatically finds the most recent topo_transects_<YYYYMMDD>.rds file
#' in the specified directory if no specific filepath is provided.
#'
#' @param filepath Character string or NULL. Full path to RDS file. If NULL,
#'   searches for most recent topo_transects_*.rds file in \code{input_dir}.
#' @param input_dir Character string. Directory to search if filepath is NULL.
#'   Default is "data/processed" relative to current working directory.
#'
#' @return A list containing transect analysis results with components:
#'   \item{auc_results}{Data frame with columns: transect, year, auc, segments}
#'   \item{metadata}{List of metadata about the analysis}
#'   \item{...}{Additional components that may vary by analysis version}
#'
#' @details The function performs the following validations:
#'   \itemize{
#'     \item Searches for files matching pattern topo_transects_<YYYYMMDD>.rds
#'     \item Selects most recent file by date if filepath not specified
#'     \item Validates file structure and required components
#'     \item Checks that auc_results is a data frame with required columns
#'   }
#'
#'   If no valid results file is found, provides helpful error messages about
#'   how to generate the required analysis results.
#'
#' @examples
#' \dontrun{
#' # Load most recent transect analysis results from default directory
#' results <- load_transect_results()
#'
#' # Load from specific file
#' results <- load_transect_results(filepath = "data/topo_transects_20231201.rds")
#'
#' # Load from custom directory
#' results <- load_transect_results(input_dir = "output/analysis")
#'
#' # Access AUC results
#' auc_data <- results$auc_results
#' }
#'
#' @export
load_transect_results <- function(filepath = NULL,
                                  input_dir = "data/processed") {

  # Determine filepath
  if (is.null(filepath)) {
    # Search for topo_transects_*.rds files
    pattern <- "^topo_transects_[0-9]{8}\\.rds$"
    available_files <- list.files(input_dir, pattern = pattern, full.names = FALSE)

    if (length(available_files) == 0) {
      stop(
        "No transect analysis results found in: ", input_dir, "\n",
        "Expected file pattern: topo_transects_<YYYYMMDD>.rds\n",
        "Run transect analysis first using run_transect_analysis() function"
      )
    }

    # Sort files by date (filename) and select the most recent
    available_files <- sort(available_files, decreasing = TRUE)
    most_recent <- available_files[1]
    filepath <- file.path(input_dir, most_recent)

    message("Loading most recent transect results: ", most_recent)
  }

  # Check file exists
  if (!file.exists(filepath)) {
    stop(
      "Transect analysis results not found: ", filepath, "\n",
      "Run transect analysis first using run_transect_analysis() function"
    )
  }

  # Load and validate
  tryCatch({
    results <- readRDS(filepath)

    # Validate structure
    required_fields <- c("auc_results", "metadata")
    missing_fields <- setdiff(required_fields, names(results))

    if (length(missing_fields) > 0) {
      stop("Invalid file structure. Missing fields: ",
           paste(missing_fields, collapse = ", "))
    }

    # Validate auc_results is a dataframe
    if (!is.data.frame(results$auc_results)) {
      stop("Invalid auc_results component - expected data frame")
    }

    # Check required columns in auc_results
    required_cols <- c("transect", "year", "auc", "segments")
    missing_cols <- setdiff(required_cols, names(results$auc_results))
    if (length(missing_cols) > 0) {
      warning("AUC results missing expected columns: ",
              paste(missing_cols, collapse = ", "))
    }

    return(results)

  }, error = function(e) {
    stop("Failed to load transect analysis results: ", e$message)
  })
}


# ============================================================================
# DATA CLEANING FUNCTIONS
# ============================================================================

#' Remove Extraneous Negative Elevations
#'
#' Removes negative elevation values from the beginning and end of a transect
#' profile, while preserving one leading and one trailing negative value for
#' proper boundary detection. This is useful for cleaning beach profile data
#' where negative elevations (below sea level) at the ends are artifacts.
#'
#' @param df Data frame. Must contain an \code{elevation} column with numeric values.
#'
#' @return Data frame with leading and trailing negative elevations removed,
#'   except for one value at each end (if present).
#'
#' @details The function operates in two steps:
#'   \enumerate{
#'     \item Leading negatives: If the first elevation is negative, keeps only
#'       the value immediately before the first positive elevation.
#'     \item Trailing negatives: If the last elevation is negative, keeps only
#'       the value immediately after the last positive elevation.
#'   }
#'
#'   This preserves boundary information while removing spurious underwater
#'   measurements that extend beyond the area of interest.
#'
#' @examples
#' \dontrun{
#' # Create sample data with leading/trailing negatives
#' sample_df <- data.frame(
#'   distance = 1:10,
#'   elevation = c(-2, -1, 0.5, 1, 2, 1.5, 0.5, -0.5, -1, -2)
#' )
#'
#' # Remove extraneous negatives
#' cleaned <- remove_negatives(sample_df)
#' # Result keeps one negative at start and end: -1, 0.5, ..., 0.5, -0.5
#' }
#'
#' @importFrom utils tail
#' @export
remove_negatives <- function(df) {
  if (nrow(df) == 0) return(df)

  # Remove leading negatives (keep one)
  if (df$elevation[1] < 0) {
    first_positive_index <- which(df$elevation >= 0)[1]
    if (!is.na(first_positive_index) && first_positive_index > 1) {
      df <- df[(first_positive_index - 1):nrow(df), ]
    }
  }

  # Remove trailing negatives (keep one)
  if (df$elevation[nrow(df)] < 0) {
    last_positive_index <- tail(which(df$elevation >= 0), 1)
    if (length(last_positive_index) > 0 && last_positive_index < nrow(df)) {
      df <- df[1:(last_positive_index + 1), ]
    }
  }

  return(df)
}
