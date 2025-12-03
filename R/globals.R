# globals.R
# Column name declarations for R CMD check
# These are used in dplyr/tidyr NSE expressions throughout the package

utils::globalVariables(c(
  # Core transect identifiers
  "transect", "year", "park", "cross_island",

  # Measurement columns
  "distance", "elevation", "point_type",
  "elev_upper", "elev_lower",  # Elevation uncertainty bounds

  # Accuracy/uncertainty columns
  "sigma_h", "sigma_v", "sigma_h_next", "sigma_h_special", "sigma_v_special",
  "accuracy_tier", "accuracy_tier_special",
  "uncertainty", "uncertainty_95ci", "confidence",
  "tolerance", "is_duplicate", "is_measured_zero",

  # Calculated columns
  "slope", "extrap_distance", "label",

  # AUC/segment columns
  "auc", "auc_nominal", "auc_upper", "auc_lower",
  "auc_sigma", "auc_uncertainty", "auc_uncertainty_pct",
  "auc_clean",  # Cleaned AUC value (NA -> 0)
  "segment", "segment_info", "segments", "segment_change", "segment_group", "n_segments",
  "above_zero", "elev_weighted",
  "year_factor",  # Year as factor for plotting

  # Metrics columns
  "transect_length", "overlap_m", "overlap_pct",
  "overlap_start", "overlap_end",
  "max_dist", "min_dist", "n_years", "year_min",

  # Misc
  "modified", "n_transects", "transects", "line",

  # Plotting variables (used in aes() calls)
  "x", "y", "linetype_category"
))
