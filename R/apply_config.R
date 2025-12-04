#' Apply Plot Configuration to Data
#'
#' Applies all configuration settings including data filtering and park-wide
#' axis limit calculation. Returns prepared data ready for plotting.
#'
#' This function serves as a wrapper that:
#' 1. Applies data filters from the config (year/park/transect selection)
#' 2. Calculates park-wide axis limits for consistent scaling
#' 3. Creates park-specific configs with pre-calculated limits
#' 4. Returns prepared data ready for plotting
#'
#' @param config plot_config object from create_plot_config() or presets
#' @param data Dataframe with transect profile data
#' @param auc_results Dataframe with AUC results
#' @param verbose Logical. Print progress messages. Default FALSE.
#'
#' @return List with class "applied_config" containing:
#'   \item{data}{Filtered transect dataframe}
#'   \item{auc_results}{Filtered AUC dataframe}
#'   \item{park_configs}{Named list of park-specific configs with pre-calculated axis limits}
#'   \item{parks}{Vector of park names in the prepared data}
#'   \item{metadata}{Details about applied filters and selections}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create configuration
#' config <- config_temporal(n_recent = 2)
#'
#' # Apply configuration to data
#' prepared <- apply_config(config, data, auc_results, verbose = TRUE)
#'
#' # Now plot with prepared data:
#' for (park in prepared$parks) {
#'   park_transects <- unique(prepared$data$transect[prepared$data$park == park])
#'   for (transect in park_transects) {
#'     transect_data <- prepared$data[prepared$data$transect == transect, ]
#'     p <- plot_transect(transect_data, prepared$auc_results,
#'                        prepared$park_configs[[park]])
#'     save_plot_file(p, ...)
#'   }
#' }
#' }
apply_config <- function(config, data, auc_results = NULL, verbose = FALSE) {

  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================

  if (!inherits(config, "plot_config")) {
    stop("config must be a plot_config object from create_plot_config()",
         call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("data must be a dataframe", call. = FALSE)
  }

  if (nrow(data) == 0) {
    stop("data is empty", call. = FALSE)
  }

  # Validate required columns
  required_cols <- c("transect", "year", "park", "distance", "elevation")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("data is missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # ==========================================================================
  # STEP 1: Apply data filters from config
  # ==========================================================================

  filtered_data <- data
  filtered_auc <- auc_results
  filter_metadata <- NULL

  if (!is.null(config$data_filter)) {
    if (verbose) {
      message("Applying data filter: ", config$data_filter$name)
    }

    filter_result <- apply_filter(
      filter = config$data_filter,
      data = data,
      auc_results = auc_results,
      verbose = verbose
    )

    filtered_data <- filter_result$data
    filtered_auc <- filter_result$auc_results
    filter_metadata <- filter_result$resolved

    if (verbose && !is.null(filter_result$resolved)) {
      message("  Parks: ", paste(filter_result$resolved$parks, collapse = ", "))
      message("  Years: ", paste(filter_result$resolved$years, collapse = ", "))
      message("  Transects: ", length(filter_result$resolved$transects))
    }
  }

  # Check that filtering didn't remove all data
  if (nrow(filtered_data) == 0) {
    stop("Data filter removed all data. Check filter settings.", call. = FALSE)
  }

  # ==========================================================================
  # STEP 2: Calculate park-wide axis limits
  # ==========================================================================

  parks <- unique(filtered_data$park)
  park_configs <- list()

  for (park in parks) {
    park_data <- filtered_data[filtered_data$park == park, ]
    park_ylim <- NULL

    # Calculate park-wide limits if config specifies "park" mode
    if (!is.null(config$axis_limits) && config$axis_limits == "park") {
      if (verbose) {
        message("Calculating park-wide axis limits for ", park, "...")
      }

      park_limits <- tryCatch({
        calculate_axis_limits(park_data, park)
      }, error = function(e) {
        warning("Failed to calculate park limits for ", park, ": ", e$message,
                ". Using auto-scale instead.", call. = FALSE)
        NULL
      })

      if (!is.null(park_limits)) {
        park_ylim <- park_limits$ylim
        if (verbose) {
          message("  Y-axis range: ", sprintf("[%.2f, %.2f]",
                                              park_ylim[1], park_ylim[2]))
        }
      }
    }

    # Create park-specific config with pre-calculated limits
    park_configs[[park]] <- if (!is.null(park_ylim)) {
      modify_config(config,
                    axis_limits = "custom",
                    custom_ylim = park_ylim)
    } else {
      config  # Use original config if no park limits calculated
    }
  }

  # ==========================================================================
  # STEP 3: Return prepared data and configs
  # ==========================================================================

  result <- list(
    data = filtered_data,
    auc_results = filtered_auc,
    park_configs = park_configs,
    parks = parks,
    metadata = list(
      config_name = if (!is.null(config$name)) config$name else "custom",
      filter_applied = !is.null(config$data_filter),
      filter_details = filter_metadata,
      parks = parks,
      n_transects = length(unique(filtered_data$transect)),
      n_years = length(unique(filtered_data$year)),
      years = sort(unique(filtered_data$year)),
      axis_mode = config$axis_limits
    )
  )

  class(result) <- c("applied_config", "list")

  return(result)
}
