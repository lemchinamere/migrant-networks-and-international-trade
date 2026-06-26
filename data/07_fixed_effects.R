# ============================================================
# Fixed-effect identifiers for empirical specifications
# ============================================================
#
# Purpose:
#   Construct and update fixed-effect identifiers used in the main
#   empirical specifications and robustness checks.
#
# Script type:
#   Data-construction / panel-update script
#
# Main specification fixed effects:
#   1. federal_state × origin_country
#   2. federal_state × year
#   3. origin_country × year
#
# Variables created:
#   fe_state_origin
#   fe_state_year
#   fe_origin_year
#
# Active panels updated:
#   analysis_panel
#   analysis_panel_no_eritrea
#   analysis_panel_controls
#   analysis_panel_controls_no_eritrea
#
# Archived panels updated:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Workflow logic:
#   This script loads already constructed .rds panels, adds or overwrites
#   fixed-effect identifiers, and saves the updated panels again.
#
#   It does not reconstruct the underlying analysis panels from raw data.
#
# Notes:
#   The fixed-effect identifiers are used in the preferred PPML reduced-form
#   specification and in most robustness checks.
#
#   The active main specification uses:
#
#     export_value =
#       beta * iv_stock_2016_post_1000
#       + federal_state × origin_country fixed effects
#       + federal_state × year fixed effects
#       + origin_country × year fixed effects
#       + error
#
#   The federal_state × origin_country fixed effects absorb time-invariant
#   differences between German Länder and origin countries.
#
#   The federal_state × year fixed effects absorb federal-state-specific
#   shocks in each year, including regional macroeconomic conditions.
#
#   The origin_country × year fixed effects absorb origin-specific shocks in
#   each year, including common trade or political shocks affecting exports
#   to a given origin country.
#
#   analysis_panel_cepii and analysis_panel_cepii_no_eritrea are archived
#   and not used in the active final robustness package. They are updated
#   here only for transparency and reproducibility.
# ============================================================


# ============================================================
# Setup
# ============================================================

### Path

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")


### Packages

library(dplyr)
library(tibble)


# ============================================================
# Required input files
# ============================================================
#
# Purpose:
#   Define all already constructed panel files required by this script.
#
# Required active panels:
#   analysis_panel.rds
#   analysis_panel_no_eritrea.rds
#   analysis_panel_controls.rds
#   analysis_panel_controls_no_eritrea.rds
#
# Required archived panels:
#   analysis_panel_cepii.rds
#   analysis_panel_cepii_no_eritrea.rds
#
# Notes:
#   This script is self-contained in the sense that it explicitly loads all
#   required panels from disk.
#
#   However, because this is a panel-update script rather than a raw
#   data-cleaning script, it does not rebuild these panels from raw data.
#
#   If one of these files is missing, the corresponding upstream
#   panel-construction script should be rerun first.
# ============================================================

required_input_files <- c(
  "analysis_panel.rds",
  "analysis_panel_no_eritrea.rds",
  "analysis_panel_controls.rds",
  "analysis_panel_controls_no_eritrea.rds",
  "analysis_panel_cepii.rds",
  "analysis_panel_cepii_no_eritrea.rds"
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

missing_input_files

if (length(missing_input_files) > 0) {
  stop(
    paste(
      "The following required input files are missing:",
      paste(missing_input_files, collapse = ", "),
      "Please rerun the corresponding panel-construction scripts before running this fixed-effect script."
    )
  )
}


# ============================================================
# Load required panels
# ============================================================
#
# Purpose:
#   Load all active and archived panels that receive fixed-effect
#   identifiers in this script.
#
# Active panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#   analysis_panel_controls
#   analysis_panel_controls_no_eritrea
#
# Archived panels:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Notes:
#   All panels are loaded directly from existing .rds files.
#   This avoids accidental dependence on outdated objects already present in
#   the R environment.
# ============================================================

analysis_panel <- readRDS(
  "analysis_panel.rds"
)

analysis_panel_no_eritrea <- readRDS(
  "analysis_panel_no_eritrea.rds"
)

analysis_panel_controls <- readRDS(
  "analysis_panel_controls.rds"
)

analysis_panel_controls_no_eritrea <- readRDS(
  "analysis_panel_controls_no_eritrea.rds"
)

analysis_panel_cepii <- readRDS(
  "analysis_panel_cepii.rds"
)

analysis_panel_cepii_no_eritrea <- readRDS(
  "analysis_panel_cepii_no_eritrea.rds"
)


# ============================================================
# Required-variable check before fixed-effect construction
# ============================================================
#
# Purpose:
#   Check whether all loaded panels contain the variables required to
#   construct fixed-effect identifiers.
#
# Required variables:
#   federal_state
#   origin_country
#   year
#
# Interpretation:
#   Missing variables indicate that one of the upstream panel-construction
#   scripts must be corrected or rerun.
# ============================================================

required_fixed_effect_source_variables <- c(
  "federal_state",
  "origin_country",
  "year"
)

missing_fixed_effect_source_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_fixed_effect_source_variables,
    present = required_fixed_effect_source_variables %in%
      names(analysis_panel)
  ),
  
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_fixed_effect_source_variables,
    present = required_fixed_effect_source_variables %in%
      names(analysis_panel_no_eritrea)
  ),
  
  tibble(
    panel = "analysis_panel_controls",
    variable = required_fixed_effect_source_variables,
    present = required_fixed_effect_source_variables %in%
      names(analysis_panel_controls)
  ),
  
  tibble(
    panel = "analysis_panel_controls_no_eritrea",
    variable = required_fixed_effect_source_variables,
    present = required_fixed_effect_source_variables %in%
      names(analysis_panel_controls_no_eritrea)
  ),
  
  tibble(
    panel = "analysis_panel_cepii",
    variable = required_fixed_effect_source_variables,
    present = required_fixed_effect_source_variables %in%
      names(analysis_panel_cepii)
  ),
  
  tibble(
    panel = "analysis_panel_cepii_no_eritrea",
    variable = required_fixed_effect_source_variables,
    present = required_fixed_effect_source_variables %in%
      names(analysis_panel_cepii_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_fixed_effect_source_variables

if (nrow(missing_fixed_effect_source_variables) > 0) {
  stop(
    "At least one required source variable for fixed-effect construction is missing. Inspect missing_fixed_effect_source_variables."
  )
}


# ============================================================
# Helper function: add fixed-effect identifiers
# ============================================================
#
# Purpose:
#   Add the fixed-effect identifiers required by the preferred empirical
#   specifications.
#
# Constructed variables:
#   fe_state_origin
#   = federal_state × origin_country
#
#   fe_state_year
#   = federal_state × year
#
#   fe_origin_year
#   = origin_country × year
#
# Logic:
#   The identifiers are constructed from the underlying panel variables using
#   interaction(..., drop = TRUE). The drop = TRUE option removes unused
#   factor levels and keeps the fixed-effect dimensions clean.
#
# Interpretation:
#   fe_state_origin controls for time-invariant differences across
#   federal_state × origin_country pairs.
#
#   fe_state_year controls for year-specific shocks at the federal-state
#   level.
#
#   fe_origin_year controls for year-specific shocks at the origin-country
#   level.
#
# Notes:
#   The function overwrites existing fixed-effect identifiers to ensure
#   consistency across panels.
# ============================================================

add_fixed_effect_identifiers <- function(data) {
  data %>%
    mutate(
      fe_state_origin =
        interaction(
          federal_state,
          origin_country,
          drop = TRUE
        ),
      
      fe_state_year =
        interaction(
          federal_state,
          year,
          drop = TRUE
        ),
      
      fe_origin_year =
        interaction(
          origin_country,
          year,
          drop = TRUE
        )
    )
}


# ============================================================
# Add fixed-effect identifiers to active panels
# ============================================================
#
# Purpose:
#   Update the active main and robustness panels with consistent fixed-effect
#   identifiers.
#
# Active panels updated:
#   analysis_panel
#   analysis_panel_no_eritrea
#   analysis_panel_controls
#   analysis_panel_controls_no_eritrea
#
# Notes:
#   analysis_panel is the main panel for the preferred specification.
#
#   analysis_panel_no_eritrea is used for the no-Eritrea robustness check.
#
#   analysis_panel_controls and analysis_panel_controls_no_eritrea are used
#   for regional-control robustness checks.
# ============================================================

analysis_panel <- add_fixed_effect_identifiers(
  analysis_panel
)

analysis_panel_no_eritrea <- add_fixed_effect_identifiers(
  analysis_panel_no_eritrea
)

analysis_panel_controls <- add_fixed_effect_identifiers(
  analysis_panel_controls
)

analysis_panel_controls_no_eritrea <- add_fixed_effect_identifiers(
  analysis_panel_controls_no_eritrea
)


# ============================================================
# Add fixed-effect identifiers to archived CEPII panels
# ============================================================
#
# Purpose:
#   Update archived CEPII panels with consistent fixed-effect identifiers for
#   transparency and reproducibility.
#
# Archived panels updated:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Notes:
#   These panels are not part of the active final robustness package.
#
#   They are retained because the CEPII / gravity-control robustness check
#   was considered but not retained after the gravity controls were absorbed
#   by the remaining fixed effects and dropped due to collinearity.
# ============================================================

analysis_panel_cepii <- add_fixed_effect_identifiers(
  analysis_panel_cepii
)

analysis_panel_cepii_no_eritrea <- add_fixed_effect_identifiers(
  analysis_panel_cepii_no_eritrea
)


# ============================================================
# Helper function: summarise fixed-effect identifiers
# ============================================================
#
# Purpose:
#   Summarise the fixed-effect dimensions and missingness for each panel.
#
# Checks:
#   Number of observations
#   Number of federal_state × origin_country fixed effects
#   Number of federal_state × year fixed effects
#   Number of origin_country × year fixed effects
#   Missing values in all fixed-effect identifiers
#
# Interpretation:
#   The fixed-effect identifiers should not contain missing values.
#   The number of fixed-effect groups documents the effective dimensionality
#   of each panel.
# ============================================================

summarise_fixed_effects <- function(data, panel_name, active_status) {
  data %>%
    summarise(
      panel = panel_name,
      active_status = active_status,
      n_obs = n(),
      n_fe_state_origin = n_distinct(fe_state_origin),
      n_fe_state_year = n_distinct(fe_state_year),
      n_fe_origin_year = n_distinct(fe_origin_year),
      missing_fe_state_origin = sum(is.na(fe_state_origin)),
      missing_fe_state_year = sum(is.na(fe_state_year)),
      missing_fe_origin_year = sum(is.na(fe_origin_year))
    )
}


# ============================================================
# Fixed-effect checks
# ============================================================
#
# Purpose:
#   Check whether fixed-effect identifiers were successfully constructed for
#   all active and archived panels.
#
# Interpretation:
#   All panels should have zero missing fixed-effect identifiers.
#
# Notes:
#   This summary is the main diagnostic for the fixed-effect construction
#   step.
# ============================================================

fixed_effect_summary <- bind_rows(
  summarise_fixed_effects(
    data = analysis_panel,
    panel_name = "analysis_panel",
    active_status = "active main panel"
  ),
  
  summarise_fixed_effects(
    data = analysis_panel_no_eritrea,
    panel_name = "analysis_panel_no_eritrea",
    active_status = "active no-Eritrea robustness panel"
  ),
  
  summarise_fixed_effects(
    data = analysis_panel_controls,
    panel_name = "analysis_panel_controls",
    active_status = "active regional-control robustness panel"
  ),
  
  summarise_fixed_effects(
    data = analysis_panel_controls_no_eritrea,
    panel_name = "analysis_panel_controls_no_eritrea",
    active_status = "active regional-control no-Eritrea robustness panel"
  ),
  
  summarise_fixed_effects(
    data = analysis_panel_cepii,
    panel_name = "analysis_panel_cepii",
    active_status = "archived / not used actively"
  ),
  
  summarise_fixed_effects(
    data = analysis_panel_cepii_no_eritrea,
    panel_name = "analysis_panel_cepii_no_eritrea",
    active_status = "archived / not used actively"
  )
)

fixed_effect_summary


# ============================================================
# Helper function: fixed-effect dimension checks
# ============================================================
#
# Purpose:
#   Calculate minimum and maximum observations per fixed-effect group for a
#   given panel.
#
# Fixed effects checked:
#   fe_state_origin
#   fe_state_year
#   fe_origin_year
#
# Interpretation:
#   These diagnostics document the structure of the fixed effects. For
#   example, fe_state_origin groups should usually contain multiple yearly
#   observations, while fe_state_year groups contain observations across
#   origin countries in the same Land and year.
# ============================================================

summarise_fixed_effect_dimensions <- function(data, panel_name) {
  tibble(
    panel = panel_name,
    fixed_effect = c(
      "fe_state_origin",
      "fe_state_year",
      "fe_origin_year"
    ),
    min_obs_per_fe = c(
      data %>%
        count(fe_state_origin) %>%
        summarise(min_n = min(n)) %>%
        pull(min_n),
      
      data %>%
        count(fe_state_year) %>%
        summarise(min_n = min(n)) %>%
        pull(min_n),
      
      data %>%
        count(fe_origin_year) %>%
        summarise(min_n = min(n)) %>%
        pull(min_n)
    ),
    max_obs_per_fe = c(
      data %>%
        count(fe_state_origin) %>%
        summarise(max_n = max(n)) %>%
        pull(max_n),
      
      data %>%
        count(fe_state_year) %>%
        summarise(max_n = max(n)) %>%
        pull(max_n),
      
      data %>%
        count(fe_origin_year) %>%
        summarise(max_n = max(n)) %>%
        pull(max_n)
    )
  )
}


# ============================================================
# Fixed-effect dimension checks
# ============================================================
#
# Purpose:
#   Document the size distribution of the fixed-effect groups in the active
#   and archived panels.
#
# Notes:
#   These checks are diagnostic only. They help verify that the fixed-effect
#   variables are constructed as intended before running regressions.
# ============================================================

fixed_effect_dimension_summary <- bind_rows(
  summarise_fixed_effect_dimensions(
    data = analysis_panel,
    panel_name = "analysis_panel"
  ),
  
  summarise_fixed_effect_dimensions(
    data = analysis_panel_no_eritrea,
    panel_name = "analysis_panel_no_eritrea"
  ),
  
  summarise_fixed_effect_dimensions(
    data = analysis_panel_controls,
    panel_name = "analysis_panel_controls"
  ),
  
  summarise_fixed_effect_dimensions(
    data = analysis_panel_controls_no_eritrea,
    panel_name = "analysis_panel_controls_no_eritrea"
  ),
  
  summarise_fixed_effect_dimensions(
    data = analysis_panel_cepii,
    panel_name = "analysis_panel_cepii"
  ),
  
  summarise_fixed_effect_dimensions(
    data = analysis_panel_cepii_no_eritrea,
    panel_name = "analysis_panel_cepii_no_eritrea"
  )
)

fixed_effect_dimension_summary


# ============================================================
# Save updated panels with fixed-effect identifiers
# ============================================================
#
# Purpose:
#   Save all panels after fixed-effect identifiers have been updated.
#
# Notes:
#   Since all panels are required inputs in this self-contained script, all
#   updated panels are saved explicitly.
# ============================================================

saveRDS(
  analysis_panel,
  "analysis_panel.rds"
)

saveRDS(
  analysis_panel_no_eritrea,
  "analysis_panel_no_eritrea.rds"
)

saveRDS(
  analysis_panel_controls,
  "analysis_panel_controls.rds"
)

saveRDS(
  analysis_panel_controls_no_eritrea,
  "analysis_panel_controls_no_eritrea.rds"
)

saveRDS(
  analysis_panel_cepii,
  "analysis_panel_cepii.rds"
)

saveRDS(
  analysis_panel_cepii_no_eritrea,
  "analysis_panel_cepii_no_eritrea.rds"
)

saveRDS(
  fixed_effect_summary,
  "fixed_effect_summary.rds"
)

saveRDS(
  fixed_effect_dimension_summary,
  "fixed_effect_dimension_summary.rds"
)

saveRDS(
  missing_fixed_effect_source_variables,
  "missing_fixed_effect_source_variables.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_fixed_effect_source_variables,
  add_fixed_effect_identifiers,
  summarise_fixed_effects,
  summarise_fixed_effect_dimensions
)


# ============================================================
# Final objects kept
# ============================================================
#
# Updated active panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#   analysis_panel_controls
#   analysis_panel_controls_no_eritrea
#
# Updated archived panels:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Fixed-effect diagnostic objects:
#   fixed_effect_summary
#   fixed_effect_dimension_summary
#   missing_fixed_effect_source_variables
#
# Notes:
#   analysis_panel is the active main panel for the preferred specification.
#
#   analysis_panel_no_eritrea is the active no-Eritrea robustness panel.
#
#   analysis_panel_controls is the active regional-control robustness panel.
#
#   analysis_panel_controls_no_eritrea is the active regional-control
#   robustness panel excluding Eritrea.
#
#   analysis_panel_cepii and analysis_panel_cepii_no_eritrea are archived and
#   not used in the active final robustness package. They are retained only
#   for transparency because the CEPII / gravity-control robustness check was
#   considered but not retained.
#
#   Main fixed effects:
#     fe_state_origin = federal_state × origin_country
#     fe_state_year   = federal_state × year
#     fe_origin_year  = origin_country × year
#
#   These fixed effects are used in the preferred PPML reduced-form
#   specification and in most active robustness checks.
#
#   The fixed-effect identifiers are overwritten consistently across panels
#   to avoid inconsistencies from earlier construction steps.
#
#   This script is self-contained: all required panels are loaded from disk at
#   the beginning of the script.
# ============================================================