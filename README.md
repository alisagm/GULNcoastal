# GULNcoastal

Gulf Coast Network Monitoring Analysis Tools

## Description

**Work in Progress**: This package is under active development.
Function signatures and behavior may change without notice.

This package provides a comprehensive suite of tools for analyzing coastal monitoring data from Gulf Coast Network National Park units. It specializes in beach profile analysis with advanced uncertainty quantification, featuring probability-weighted area-under-curve (AUC) calculations that account for measurement accuracy variations across survey years.

## Key Features

- **Uncertainty-Aware Analysis**: Year-stratified measurement accuracy assignment with full error propagation
- **Probability-Weighted AUC**: Novel approach to handle measurement uncertainty near sea level
- **Semantic Data Selection**: Flexible year selection system that works per-park (e.g., "first and recent 2 years")
- **Configuration-Based Plotting**: Centralized plot settings with presets for common use cases
- **Automated Workflows**: Complete analysis pipelines from raw CSV to publication-ready outputs
- **Smart Visualization**: Automatic legend configuration, optimal inset positioning, and park-specific axis scaling

## Installation

You can install the development version of GULNcoastal from this repository:

```r
# install.packages("devtools")
devtools::install_local("path/to/GULNcoastal")
```

## Dependencies

Required packages:
- tidyverse (ggplot2, dplyr, readr, stringr)
- here
- viridis
- patchwork
- grid

## Package Structure

```
GULNcoastal/
├── R/                        # R source code (22 files, 100+ exported functions)
│   ├── import.R              # Data import and cleaning
│   ├── output.R              # Data export (RDS, CSV)
│   ├── error.R               # Accuracy assignment and uncertainty quantification
│   ├── data_filter.R         # Semantic data filtering
│   ├── year_selectors.R      # Per-park year selection
│   ├── zerocrossing.R        # Zero-elevation point detection
│   ├── common_min.R          # Common landward integration boundary
│   ├── auc_weighted.R        # Probability-weighted AUC calculation
│   ├── transect_analysis.R   # Complete analysis pipeline
│   ├── plot_transect.R       # Main plotting engine
│   ├── plot_config.R         # Plot configuration management
│   ├── plot_config_presets.R # Pre-configured plotting scenarios
│   ├── apply_config.R        # Configuration application
│   ├── plot_colors.R         # Color palette management
│   ├── plot_themes.R         # ggplot2 theme presets
│   ├── plot_legend.R         # Unified legend system
│   ├── plot_positioning.R    # Smart inset positioning
│   ├── plot_shading.R        # AUC shading polygons
│   ├── plot_batch.R          # Batch plot export
│   └── axis_limits.R         # Park-specific axis calculation
├── inst/extdata/             # External data files
│   ├── accuracy_values.csv   # Survey-specific accuracy values
│   └── special_cases.csv     # Transect-specific accuracy overrides
├── man/                      # Documentation (113 .Rd files)
└── tests/                    # Test suite

```

## Workflow Logic

### Standard Analysis Workflow

#### Phase 1: Data Import & Preparation

```r
library(GULNcoastal)

# Import raw transect data
data <- import_transects_park("path/to/transect_data.csv")

# Clean extraneous negative elevations
data <- remove_negatives(data)

# Load accuracy tables
accuracy_table <- load_accuracy_table()
special_cases <- load_special_cases_table()

# Assign year-specific measurement accuracy
data <- assign_accuracy(
  data,
  accuracy_table = accuracy_table,
  special_cases = special_cases
)
```

#### Phase 2: Analysis

```r
# Run complete analysis pipeline
results <- run_transect_analysis(
  csv_path = "path/to/transect_data.csv",
  output_dir = "results/",
  park_code = "PAIS"
)

# This orchestrates:
# 1. Data import and cleaning
# 2. Accuracy assignment
# 3. Zero-crossing detection with uncertainty
# 4. Common minimum calculation
# 5. Probability-weighted AUC calculation
# 6. Export results to RDS and CSVs
```

#### Phase 3: Visualization

```r
# Load analysis results
data <- load_transect_results("results/PAIS_transect_results.rds")

# Create semantic data filter
filter <- create_data_filter(
  year_selector = years_baseline_recent(2),  # First + most recent 2 years
  parks = "PAIS",
  transects = NULL  # All transects
)

# Create plot configuration
config <- create_plot_config(
  data_filter = filter,
  show_uncertainty_bands = TRUE,
  show_common_min_vline = TRUE,
  show_auc_inset = TRUE,
  axis_limits = "park",  # Consistent limits across all transects
  theme = "default"
)

# Apply configuration to data
applied <- apply_config(config, data)

# Generate plots for each transect
for (transect_id in unique(applied$filtered_data$transect)) {
  p <- plot_transect(
    data = applied$filtered_data,
    transect = transect_id,
    config = config,
    applied_config = applied
  )

  # Save plot
  save_plot_file(
    p,
    filename = paste0("transect_", transect_id, ".png"),
    output_dir = "plots/",
    width = 10,
    height = 6
  )
}
```

### Quick Exploration Workflows

#### Quick Recent Data Visualization

```r
# Load data
data <- load_transect_results("results/park_results.rds")

# Use preset configuration
config <- config_quick(n_years = 2)

# Apply and plot
applied <- apply_config(config, data)
plot_transect(data$data, transect = "T001", config = config, applied_config = applied)
```

#### Temporal Comparison

```r
# Compare first and last survey years
config <- config_temporal(
  data = data,
  comparison_type = "endpoints"
)

applied <- apply_config(config, data)
plot_transect(data$data, transect = "T001", config = config, applied_config = applied)
```

#### Annual Progression

```r
# Show annual progression to target year
config <- config_annual(
  data = data,
  target_year = 2023,
  parks = "PAIS"
)

applied <- apply_config(config, data)
# Generates plots showing progression: 2019, 2019-2020, 2019-2021, ..., 2019-2023
```

## Custom Workflows

### When Standard Workflow Isn't Enough

Use `run_transect_analysis()` for standard cases where:
- Data is in a single directory
- All files follow the same format
- No data cleaning or fixing needed
- Standard accuracy tables apply

Use **custom workflows** when you need:
- Multiple data sources with different formats
- Data cleaning (fix typos, standardize naming, handle NAs)
- Combining datasets from different time periods
- Custom import logic
- Step-by-step processing for debugging

### Pattern 1: Using `run_transect_analysis_data()`

The flexible version accepts pre-loaded data:

```r
library(GULNcoastal)
library(dplyr)

# Import from multiple sources
data1 <- import_transects_park("older_format.csv")
data2 <- import_transects_park("newer_format.csv")

# Fix issues
data1$year[data1$year == 2017] <- 2016              # Fix typo
data1$transect <- str_remove(data1$transect, "^t0*") # Standardize naming

# Combine datasets
data_combined <- bind_rows(
  data1,
  data2 |> filter(year %in% c(2017, 2025))
) |>
  drop_na() |>
  arrange(transect, year)

# Run analysis on cleaned data
results <- run_transect_analysis_data(
  data = data_combined,
  accuracy_table = "path/to/accuracy_values.csv",
  special_cases = NULL,
  save_results = TRUE,
  verbose = TRUE
)
```

### Pattern 2: Individual Pipeline Steps

For maximum control, call pipeline steps individually:

```r
# Step 1: Clean data
data_clean <- data |>
  group_by(transect, year, cross_island, park) |>
  group_split() |>
  lapply(remove_negatives) |>
  bind_rows()

# Step 2: Deduplicate
data_deduplicated <- data_clean |>
  group_by(transect, year, distance) |>
  slice(1) |>
  ungroup()

# Step 3: Assign accuracy
accuracy_table <- load_accuracy_table("accuracy_values.csv")
data_with_accuracy <- assign_accuracy(
  data_deduplicated,
  accuracy_table = accuracy_table,
  verbose = TRUE
)

# Step 4: Find zero crossings
zero_points <- identify_zero_points_all_transects(data_with_accuracy)

# Step 5: Add uncertainty
data_classified <- add_measured_uncertainty(data_with_accuracy)

# Step 6: Combine with zero points
data_with_zeros <- bind_rows(
  data_classified,
  filter(zero_points, point_type %in% c("interpolated", "extrapolated"))
) |>
  arrange(transect, year, distance)

# Step 7: Calculate common minimum
common_mins <- calculate_and_interpolate_common_min(data_with_zeros)

# Step 8: Prepare AUC-ready data
data_auc_ready <- data_with_zeros |>
  anti_join(
    select(common_mins, transect, year, distance),
    by = c("transect", "year", "distance")
  ) |>
  bind_rows(common_mins) |>
  arrange(transect, year, distance)

# Step 9: Calculate AUC
auc_results <- data_auc_ready |>
  group_by(transect, year) |>
  group_split() |>
  lapply(function(df) {
    result <- calculate_auc_with_uncertainty(df)
    result$transect <- first(df$transect)
    result$year <- first(df$year)
    return(result)
  }) |>
  bind_rows()
```

### Comparison: When to Use Each Pattern

| Scenario | Use | Rationale |
|----------|-----|-----------|
| Standard analysis, single data source | `run_transect_analysis()` | Simplest, most convenient |
| Multiple data sources, data cleaning needed | `run_transect_analysis_data()` | Flexible import, still automated |
| Debugging pipeline, custom processing | Individual steps | Full control, inspect intermediate results |
| Production batch processing | `run_transect_analysis()` | Reliable, tested, consistent |
| Research/exploratory analysis | Individual steps or `_data()` | Iterate quickly, customize easily |

## Core Function Categories

### Data Import/Export (2 files, 13 functions)
- `import_profile_transects()` - Import beach transect CSV data
- `import_transects_park()` - Import with park identifier extraction
- `load_transect_results()` - Load previously saved results
- `save_topo_transects_rds()` - Save complete analysis results
- `export_all_results()` - Export RDS and publication-ready CSVs
- `round_to_uncertainty()` - Error-aware rounding for CSV exports

### Data Filtering (2 files, 20+ functions)
- `create_data_filter()` - Create filter specification
- `set_years()`, `set_parks()`, `set_transects()` - Chainable modifiers
- `apply_filter()` - Apply filter to data
- `years_all()`, `years_first()`, `years_recent()` - Year selectors
- `years_baseline_recent()` - First + recent N years
- `years_compare_endpoints()` - First vs. last comparison
- `years_annual_progression()` - Progressive year selection

### Measurement Accuracy & Uncertainty (1 file, 10+ functions)
- `load_accuracy_table()` - Load survey-specific accuracy values
- `load_special_cases_table()` - Load transect-specific overrides
- `assign_accuracy()` - Assign year-specific accuracy to data
- `summarize_accuracy_by_year()` - Generate accuracy summary

### Core Analysis (4 files, 30+ functions)
- `run_transect_analysis()` - Complete analysis pipeline orchestrator
- `identify_zero_points_all_transects()` - Zero-crossing detection
- `calculate_and_interpolate_common_min()` - Common minimum boundary
- `calculate_auc_with_uncertainty()` - Probability-weighted AUC

### Plotting System (10 files, 45+ functions)
- `plot_transect()` - Main plotting engine
- `create_plot_config()` - Configuration management
- `config_quick()`, `config_temporal()`, `config_annual()` - Preset configurations
- `apply_config()` - Apply configuration to data
- `get_color_palette()` - Year color generation
- `get_plot_theme()` - ggplot2 theme presets
- `configure_plot_legends()` - Unified legend system
- `find_best_inset_position()` - Smart inset positioning
- `save_plot_file()` - Batch plot export

## Key Concepts

### Year Selectors

Year selectors enable semantic data selection that works independently for each park:

```r
# Select all available years
years_all()

# Select first N survey years per park
years_first(3)

# Select most recent N years per park
years_recent(2)

# Select first year + most recent N years (baseline + recent)
years_baseline_recent(2)

# Select years within a range
years_range(2019, 2023)

# Compare first vs. last year only
years_compare_endpoints()

# Annual progression to target year
years_annual_progression(2023)

# Combine multiple selectors
years_combine(years_first(1), years_recent(2))
```

### Plot Configuration System

All visualization settings are managed through `plot_config` objects:

```r
config <- create_plot_config(
  data_filter = my_filter,
  show_uncertainty_bands = TRUE,      # Show measurement uncertainty
  show_common_min_vline = TRUE,       # Show integration boundary
  show_auc_inset = TRUE,              # Show AUC values inset
  show_positive_shading = TRUE,       # Shade positive segments
  axis_limits = "park",               # Park-wide consistent limits
  theme = "publication",              # Visual theme
  color_palette = "viridis",          # Color scheme
  subtitle_format = "detailed"        # Subtitle verbosity
)

# Modify existing configuration
config <- modify_config(config, show_uncertainty_bands = FALSE)

# Save/load configurations
save_config(config, "my_config.rds")
config <- load_config("my_config.rds")
```

### Accuracy Tables

Measurement accuracy varies by survey year and is stored in external CSV files:

**accuracy_values.csv** - Survey-specific accuracy:
```csv
park,year,sigma_h,sigma_v,accuracy_tier
PAIS,2019,0.050,0.050,A_Excellent
PAIS,2020,0.070,0.060,B_Good
```

**special_cases.csv** - Transect-specific overrides:
```csv
park,year,transect,sigma_h,sigma_v,accuracy_tier
GUIS,2018,T001,0.050,0.120,C_Moderate
```

Accuracy tiers:
- **A_Excellent**: High-precision surveys (sigma_v < 0.06m)
- **B_Good**: Standard precision (0.06m <= sigma_v < 0.10m)
- **C_Moderate**: Lower precision or special cases (sigma_v >= 0.10m)

### Probability-Weighted AUC

Traditional AUC integration can be misleading when measurement uncertainty is comparable to elevation near sea level. This package implements a probability-weighted approach:

1. For each measured point, calculate the probability that the true elevation is positive given measurement uncertainty
2. Weight the point's contribution to AUC by this probability
3. Propagate uncertainty through interpolation and integration
4. Report AUC with confidence bounds

This provides more realistic estimates when working with uncertain measurements near critical thresholds.

## Data Structure

### Input CSV Format

Beach profile transect data should be in CSV format with columns:
- `park` - Park code (e.g., "PAIS", "GUIS")
- `year` - Survey year (numeric)
- `transect` - Transect identifier (e.g., "T001")
- `distance` - Horizontal distance from origin (m)
- `elevation` - Vertical elevation (m, relative to datum)

### Analysis Results Structure

The analysis pipeline produces:

1. **RDS files** - Full precision results for further analysis
   - Complete transect data with accuracy assignments
   - Zero-crossing locations with uncertainty
   - Common minimum distances
   - Weighted AUC values with confidence bounds

2. **CSV files** - Publication-ready exports with error-aware rounding
   - Transect profiles with accuracy values
   - AUC summary tables
   - Values rounded to match measurement uncertainty

## Examples

### Complete Analysis from Raw Data

```r
library(GULNcoastal)

# Run complete analysis
results <- run_transect_analysis(
  csv_path = "data/PAIS_beach_profiles.csv",
  output_dir = "analysis_results/",
  park_code = "PAIS",
  export_csv = TRUE
)

# Results saved to:
# - analysis_results/PAIS_transect_results.rds (full precision)
# - analysis_results/PAIS_profiles.csv (rounded for publication)
# - analysis_results/PAIS_auc.csv (rounded AUC values)
```

### Custom Temporal Analysis

```r
# Load existing results
data <- load_transect_results("analysis_results/PAIS_transect_results.rds")

# Create custom filter for specific transects and years
filter <- create_data_filter(
  year_selector = years_combine(
    years_first(1),     # First survey year
    years_recent(3)     # Three most recent years
  ),
  parks = "PAIS",
  transects = c("T001", "T002", "T003"),
  name = "Selected Transects - Baseline + Recent"
)

# Preview what will be selected
preview_year_selection(filter, data)

# Create configuration with publication settings
config <- create_plot_config(
  data_filter = filter,
  show_uncertainty_bands = TRUE,
  show_common_min_vline = TRUE,
  show_auc_inset = TRUE,
  axis_limits = "park",
  theme = "publication",
  color_palette = "viridis",
  subtitle_format = "detailed"
)

# Apply and generate plots
applied <- apply_config(config, data)

for (t_id in c("T001", "T002", "T003")) {
  p <- plot_transect(
    data = applied$filtered_data,
    transect = t_id,
    config = config,
    applied_config = applied
  )

  save_plot_file(
    p,
    filename = paste0("transect_", t_id, "_temporal.png"),
    output_dir = "figures/",
    width = 12,
    height = 7,
    dpi = 300
  )
}
```

### Batch Processing Multiple Parks

```r
parks <- c("PAIS", "GUIS")
data_paths <- c(
  "data/PAIS_profiles.csv",
  "data/GUIS_profiles.csv"
)

# Analyze all parks
for (i in seq_along(parks)) {
  results <- run_transect_analysis(
    csv_path = data_paths[i],
    output_dir = paste0("results/", parks[i], "/"),
    park_code = parks[i],
    export_csv = TRUE
  )

  cat("Completed analysis for", parks[i], "\n")
}

# Create standardized plots for all parks
for (park in parks) {
  data <- load_transect_results(
    paste0("results/", park, "/", park, "_transect_results.rds")
  )

  config <- config_temporal(data, comparison_type = "baseline_recent", n_recent = 2)
  applied <- apply_config(config, data)

  transects <- unique(applied$filtered_data$transect)

  for (t_id in transects) {
    p <- plot_transect(
      data = applied$filtered_data,
      transect = t_id,
      config = config,
      applied_config = applied
    )

    save_plot_file(
      p,
      filename = paste0(park, "_", t_id, ".png"),
      output_dir = paste0("figures/", park, "/"),
      width = 10,
      height = 6
    )
  }
}
```

## Documentation

All functions are fully documented with roxygen2. Access help for any function:

```r
?run_transect_analysis
?plot_transect
?create_data_filter
?years_baseline_recent
?create_plot_config
```

View all available functions:

```r
help(package = "GULNcoastal")
```

## Design Philosophy

### Configuration Over Code

Rather than modifying function parameters for every plot, create reusable configurations:

```r
# Define once
pub_config <- create_plot_config(
  data_filter = my_filter,
  theme = "publication",
  show_uncertainty_bands = TRUE,
  axis_limits = "park"
)

# Reuse many times
plot_transect(data, "T001", config = pub_config, applied_config = applied)
plot_transect(data, "T002", config = pub_config, applied_config = applied)
plot_transect(data, "T003", config = pub_config, applied_config = applied)
```

### Semantic Selection

Express intent clearly rather than manipulating data manually:

```r
# Clear intent
years_baseline_recent(2)  # First year + 2 most recent years

# Rather than
# years_explicit(2019, 2022, 2023)  # What if you add new data?
```

### Uncertainty Throughout

Measurement accuracy is assigned at import and propagated through all calculations:

```r
# Accuracy assigned
data <- assign_accuracy(data, accuracy_table)

# Propagated through zero-crossing detection
zeros <- identify_zero_points_all_transects(data)

# Propagated through AUC calculation
auc_results <- calculate_auc_with_uncertainty(data, zeros)

# Reflected in exports with appropriate rounding
export_auc_csv(auc_results, output_dir)
```

## Advanced Features

### Park-Specific Axis Limits

When comparing transects within a park, use consistent axis limits:

```r
config <- create_plot_config(
  axis_limits = "park"  # All transects use same x/y limits
)
# vs.
config <- create_plot_config(
  axis_limits = "transect"  # Each transect optimized independently
)
```

### Theme Presets

Multiple visualization themes are available:

```r
list_themes()
# [1] "default"      "publication"  "presentation" "minimal"      "dark"

config <- create_plot_config(theme = "publication")
```

### Color Palettes

Year colors can be customized:

```r
list_palettes()
# [1] "viridis"  "plasma"   "inferno"  "magma"    "cividis"  "turbo"

config <- create_plot_config(color_palette = "plasma")
```

### Validation and Diagnostics

```r
# Check data availability
check_data_availability(data, parks = "PAIS", years = 2019:2023)

# Validate accuracy assignment
validate_accuracy_assignment(data)

# Summarize accuracy by year
accuracy_summary <- summarize_accuracy_by_year(data)
print_accuracy_summary(accuracy_summary)

# Validate configuration
validation <- validate_config(config, data)
```

## Citation

If you use this package in your research, please cite:

```
GULNcoastal: Gulf Coast Network Monitoring Analysis Tools
National Park Service, Gulf Coast Network
https://github.com/[repository]
```

## License

CC0 - Public Domain

## Contact

For questions, issues, or contributions, please visit the repository or contact the Gulf Coast Network.

## Version History

- **0.1.0** - Initial development release
  - Core analysis pipeline
  - Probability-weighted AUC calculation
  - Configuration-based plotting system
  - Year selector framework
  - Uncertainty propagation throughout
