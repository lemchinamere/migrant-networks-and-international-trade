# ============================================================
# Rescale treatment and IV variables
# ============================================================
#
# Purpose:
#   Rescale main treatment variables, main IV variables, and alternative
#   Königstein-key IV variables from persons to thousand persons to improve
#   interpretability of regression coefficients.
#
# Original main variables:
#   treatment_stock_2016_post
#   iv_stock_2016_post
#   treatment_delta_post
#   iv_delta_post
#
# Original robustness IV variables:
#   iv_stock_2016_post_k14
#   iv_delta_post_k14
#   iv_stock_2016_post_k141516
#   iv_delta_post_k141516
#
# New main variables:
#   treatment_stock_2016_post_1000
#   iv_stock_2016_post_1000
#   treatment_delta_post_1000
#   iv_delta_post_1000
#
# New robustness IV variables:
#   iv_stock_2016_post_k14_1000
#   iv_delta_post_k14_1000
#   iv_stock_2016_post_k141516_1000
#   iv_delta_post_k141516_1000
#
# Interpretation:
#   Coefficients using treatment variables are interpreted per 1,000
#   additional actual protection seekers.
#
#   Coefficients using IV variables are interpreted per 1,000 additional
#   predicted protection seekers.
#
# Notes:
#   Rescaling changes only the unit of measurement. It does not change the
#   underlying identifying variation, fitted values, standard errors, or
#   statistical significance apart from the corresponding change in scale.
#
#   CEPII panels are rescaled only for archival consistency. They are not
#   part of the active final robustness package.
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
# Load required panels
# ============================================================
#
# Purpose:
#   Load all active and archived analysis panels that contain treatment and
#   IV variables measured in persons.
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
#   The same rescaling is applied to all panels to keep variable names and
#   regression inputs consistent across scripts.
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
# Helper function: rescale treatment and IV variables
# ============================================================
#
# Purpose:
#   Add regression-ready treatment and IV variables measured in thousand
#   persons to a given panel.
#
# Logic:
#   Each original treatment or IV variable measured in persons is divided
#   by 1,000.
#
# Notes:
#   The function overwrites existing _1000 variables if they already exist.
#   This is intentional and ensures that the scaled variables always reflect
#   the current unscaled variables.
# ============================================================

add_rescaled_treatment_iv_variables <- function(data) {
  data %>%
    mutate(
      treatment_stock_2016_post_1000 =
        treatment_stock_2016_post / 1000,
      
      iv_stock_2016_post_1000 =
        iv_stock_2016_post / 1000,
      
      treatment_delta_post_1000 =
        treatment_delta_post / 1000,
      
      iv_delta_post_1000 =
        iv_delta_post / 1000,
      
      iv_stock_2016_post_k14_1000 =
        iv_stock_2016_post_k14 / 1000,
      
      iv_delta_post_k14_1000 =
        iv_delta_post_k14 / 1000,
      
      iv_stock_2016_post_k141516_1000 =
        iv_stock_2016_post_k141516 / 1000,
      
      iv_delta_post_k141516_1000 =
        iv_delta_post_k141516 / 1000
    )
}


# ============================================================
# Rescale variables in all relevant panels
# ============================================================
#
# Purpose:
#   Apply the rescaling function to all active and archived panels.
#
# Notes:
#   The main regressions use the active panels. The CEPII panels are updated
#   only to keep archived objects internally consistent.
# ============================================================

analysis_panel <- add_rescaled_treatment_iv_variables(
  analysis_panel
)

analysis_panel_no_eritrea <- add_rescaled_treatment_iv_variables(
  analysis_panel_no_eritrea
)

analysis_panel_controls <- add_rescaled_treatment_iv_variables(
  analysis_panel_controls
)

analysis_panel_controls_no_eritrea <- add_rescaled_treatment_iv_variables(
  analysis_panel_controls_no_eritrea
)

analysis_panel_cepii <- add_rescaled_treatment_iv_variables(
  analysis_panel_cepii
)

analysis_panel_cepii_no_eritrea <- add_rescaled_treatment_iv_variables(
  analysis_panel_cepii_no_eritrea
)


# ============================================================
# Rescaling checks
# ============================================================
#
# Purpose:
#   Verify that the rescaled variables were created successfully in each
#   panel and contain plausible variation.
#
# Checks:
#   For each panel, the code reports the minimum and maximum of the main
#   scaled treatment variables, main scaled IV variables, and alternative
#   scaled Königstein-key IV variables.
#
# Notes:
#   These checks are descriptive. They are intended to catch missing values,
#   failed variable creation, or implausible scaling before running the
#   regression scripts.
# ============================================================

rescaled_treatment_iv_summary <- bind_rows(
  analysis_panel %>%
    summarise(
      panel = "analysis_panel",
      n_obs = n(),
      
      min_treatment_stock_1000 =
        min(treatment_stock_2016_post_1000, na.rm = TRUE),
      max_treatment_stock_1000 =
        max(treatment_stock_2016_post_1000, na.rm = TRUE),
      
      min_iv_stock_1000 =
        min(iv_stock_2016_post_1000, na.rm = TRUE),
      max_iv_stock_1000 =
        max(iv_stock_2016_post_1000, na.rm = TRUE),
      
      min_treatment_delta_1000 =
        min(treatment_delta_post_1000, na.rm = TRUE),
      max_treatment_delta_1000 =
        max(treatment_delta_post_1000, na.rm = TRUE),
      
      min_iv_delta_1000 =
        min(iv_delta_post_1000, na.rm = TRUE),
      max_iv_delta_1000 =
        max(iv_delta_post_1000, na.rm = TRUE),
      
      min_iv_stock_k14_1000 =
        min(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      max_iv_stock_k14_1000 =
        max(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      
      min_iv_delta_k14_1000 =
        min(iv_delta_post_k14_1000, na.rm = TRUE),
      max_iv_delta_k14_1000 =
        max(iv_delta_post_k14_1000, na.rm = TRUE),
      
      min_iv_stock_k141516_1000 =
        min(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      max_iv_stock_k141516_1000 =
        max(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      
      min_iv_delta_k141516_1000 =
        min(iv_delta_post_k141516_1000, na.rm = TRUE),
      max_iv_delta_k141516_1000 =
        max(iv_delta_post_k141516_1000, na.rm = TRUE)
    ),
  
  analysis_panel_no_eritrea %>%
    summarise(
      panel = "analysis_panel_no_eritrea",
      n_obs = n(),
      
      min_treatment_stock_1000 =
        min(treatment_stock_2016_post_1000, na.rm = TRUE),
      max_treatment_stock_1000 =
        max(treatment_stock_2016_post_1000, na.rm = TRUE),
      
      min_iv_stock_1000 =
        min(iv_stock_2016_post_1000, na.rm = TRUE),
      max_iv_stock_1000 =
        max(iv_stock_2016_post_1000, na.rm = TRUE),
      
      min_treatment_delta_1000 =
        min(treatment_delta_post_1000, na.rm = TRUE),
      max_treatment_delta_1000 =
        max(treatment_delta_post_1000, na.rm = TRUE),
      
      min_iv_delta_1000 =
        min(iv_delta_post_1000, na.rm = TRUE),
      max_iv_delta_1000 =
        max(iv_delta_post_1000, na.rm = TRUE),
      
      min_iv_stock_k14_1000 =
        min(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      max_iv_stock_k14_1000 =
        max(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      
      min_iv_delta_k14_1000 =
        min(iv_delta_post_k14_1000, na.rm = TRUE),
      max_iv_delta_k14_1000 =
        max(iv_delta_post_k14_1000, na.rm = TRUE),
      
      min_iv_stock_k141516_1000 =
        min(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      max_iv_stock_k141516_1000 =
        max(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      
      min_iv_delta_k141516_1000 =
        min(iv_delta_post_k141516_1000, na.rm = TRUE),
      max_iv_delta_k141516_1000 =
        max(iv_delta_post_k141516_1000, na.rm = TRUE)
    ),
  
  analysis_panel_controls %>%
    summarise(
      panel = "analysis_panel_controls",
      n_obs = n(),
      
      min_treatment_stock_1000 =
        min(treatment_stock_2016_post_1000, na.rm = TRUE),
      max_treatment_stock_1000 =
        max(treatment_stock_2016_post_1000, na.rm = TRUE),
      
      min_iv_stock_1000 =
        min(iv_stock_2016_post_1000, na.rm = TRUE),
      max_iv_stock_1000 =
        max(iv_stock_2016_post_1000, na.rm = TRUE),
      
      min_treatment_delta_1000 =
        min(treatment_delta_post_1000, na.rm = TRUE),
      max_treatment_delta_1000 =
        max(treatment_delta_post_1000, na.rm = TRUE),
      
      min_iv_delta_1000 =
        min(iv_delta_post_1000, na.rm = TRUE),
      max_iv_delta_1000 =
        max(iv_delta_post_1000, na.rm = TRUE),
      
      min_iv_stock_k14_1000 =
        min(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      max_iv_stock_k14_1000 =
        max(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      
      min_iv_delta_k14_1000 =
        min(iv_delta_post_k14_1000, na.rm = TRUE),
      max_iv_delta_k14_1000 =
        max(iv_delta_post_k14_1000, na.rm = TRUE),
      
      min_iv_stock_k141516_1000 =
        min(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      max_iv_stock_k141516_1000 =
        max(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      
      min_iv_delta_k141516_1000 =
        min(iv_delta_post_k141516_1000, na.rm = TRUE),
      max_iv_delta_k141516_1000 =
        max(iv_delta_post_k141516_1000, na.rm = TRUE)
    ),
  
  analysis_panel_controls_no_eritrea %>%
    summarise(
      panel = "analysis_panel_controls_no_eritrea",
      n_obs = n(),
      
      min_treatment_stock_1000 =
        min(treatment_stock_2016_post_1000, na.rm = TRUE),
      max_treatment_stock_1000 =
        max(treatment_stock_2016_post_1000, na.rm = TRUE),
      
      min_iv_stock_1000 =
        min(iv_stock_2016_post_1000, na.rm = TRUE),
      max_iv_stock_1000 =
        max(iv_stock_2016_post_1000, na.rm = TRUE),
      
      min_treatment_delta_1000 =
        min(treatment_delta_post_1000, na.rm = TRUE),
      max_treatment_delta_1000 =
        max(treatment_delta_post_1000, na.rm = TRUE),
      
      min_iv_delta_1000 =
        min(iv_delta_post_1000, na.rm = TRUE),
      max_iv_delta_1000 =
        max(iv_delta_post_1000, na.rm = TRUE),
      
      min_iv_stock_k14_1000 =
        min(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      max_iv_stock_k14_1000 =
        max(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      
      min_iv_delta_k14_1000 =
        min(iv_delta_post_k14_1000, na.rm = TRUE),
      max_iv_delta_k14_1000 =
        max(iv_delta_post_k14_1000, na.rm = TRUE),
      
      min_iv_stock_k141516_1000 =
        min(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      max_iv_stock_k141516_1000 =
        max(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      
      min_iv_delta_k141516_1000 =
        min(iv_delta_post_k141516_1000, na.rm = TRUE),
      max_iv_delta_k141516_1000 =
        max(iv_delta_post_k141516_1000, na.rm = TRUE)
    ),
  
  analysis_panel_cepii %>%
    summarise(
      panel = "analysis_panel_cepii",
      n_obs = n(),
      
      min_treatment_stock_1000 =
        min(treatment_stock_2016_post_1000, na.rm = TRUE),
      max_treatment_stock_1000 =
        max(treatment_stock_2016_post_1000, na.rm = TRUE),
      
      min_iv_stock_1000 =
        min(iv_stock_2016_post_1000, na.rm = TRUE),
      max_iv_stock_1000 =
        max(iv_stock_2016_post_1000, na.rm = TRUE),
      
      min_treatment_delta_1000 =
        min(treatment_delta_post_1000, na.rm = TRUE),
      max_treatment_delta_1000 =
        max(treatment_delta_post_1000, na.rm = TRUE),
      
      min_iv_delta_1000 =
        min(iv_delta_post_1000, na.rm = TRUE),
      max_iv_delta_1000 =
        max(iv_delta_post_1000, na.rm = TRUE),
      
      min_iv_stock_k14_1000 =
        min(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      max_iv_stock_k14_1000 =
        max(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      
      min_iv_delta_k14_1000 =
        min(iv_delta_post_k14_1000, na.rm = TRUE),
      max_iv_delta_k14_1000 =
        max(iv_delta_post_k14_1000, na.rm = TRUE),
      
      min_iv_stock_k141516_1000 =
        min(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      max_iv_stock_k141516_1000 =
        max(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      
      min_iv_delta_k141516_1000 =
        min(iv_delta_post_k141516_1000, na.rm = TRUE),
      max_iv_delta_k141516_1000 =
        max(iv_delta_post_k141516_1000, na.rm = TRUE)
    ),
  
  analysis_panel_cepii_no_eritrea %>%
    summarise(
      panel = "analysis_panel_cepii_no_eritrea",
      n_obs = n(),
      
      min_treatment_stock_1000 =
        min(treatment_stock_2016_post_1000, na.rm = TRUE),
      max_treatment_stock_1000 =
        max(treatment_stock_2016_post_1000, na.rm = TRUE),
      
      min_iv_stock_1000 =
        min(iv_stock_2016_post_1000, na.rm = TRUE),
      max_iv_stock_1000 =
        max(iv_stock_2016_post_1000, na.rm = TRUE),
      
      min_treatment_delta_1000 =
        min(treatment_delta_post_1000, na.rm = TRUE),
      max_treatment_delta_1000 =
        max(treatment_delta_post_1000, na.rm = TRUE),
      
      min_iv_delta_1000 =
        min(iv_delta_post_1000, na.rm = TRUE),
      max_iv_delta_1000 =
        max(iv_delta_post_1000, na.rm = TRUE),
      
      min_iv_stock_k14_1000 =
        min(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      max_iv_stock_k14_1000 =
        max(iv_stock_2016_post_k14_1000, na.rm = TRUE),
      
      min_iv_delta_k14_1000 =
        min(iv_delta_post_k14_1000, na.rm = TRUE),
      max_iv_delta_k14_1000 =
        max(iv_delta_post_k14_1000, na.rm = TRUE),
      
      min_iv_stock_k141516_1000 =
        min(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      max_iv_stock_k141516_1000 =
        max(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
      
      min_iv_delta_k141516_1000 =
        min(iv_delta_post_k141516_1000, na.rm = TRUE),
      max_iv_delta_k141516_1000 =
        max(iv_delta_post_k141516_1000, na.rm = TRUE)
    )
)

rescaled_treatment_iv_summary


# ============================================================
# Save updated panels
# ============================================================
#
# Purpose:
#   Save all panels after adding the scaled treatment and IV variables.
#
# Notes:
#   Existing panel files are overwritten so that all later regression scripts
#   load panels containing the _1000 variables.
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
  rescaled_treatment_iv_summary,
  "rescaled_treatment_iv_summary.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  add_rescaled_treatment_iv_variables
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
# Diagnostic object:
#   rescaled_treatment_iv_summary
#
# Notes:
#   This script rescales treatment and IV variables from persons to thousand
#   persons.
#
#   The main rescaled treatment variable is:
#     treatment_stock_2016_post_1000
#
#   The main rescaled instrument is:
#     iv_stock_2016_post_1000
#
#   The alternative rescaled treatment variable is:
#     treatment_delta_post_1000
#
#   The alternative rescaled instrument is:
#     iv_delta_post_1000
#
#   The alternative Königstein-key instruments are:
#     iv_stock_2016_post_k14_1000
#     iv_delta_post_k14_1000
#     iv_stock_2016_post_k141516_1000
#     iv_delta_post_k141516_1000
#
#   Coefficients using these variables are interpreted per additional
#   1,000 actual or predicted protection seekers.
#
#   The CEPII panels are updated only for reproducibility and archival
#   consistency. They are not part of the active final robustness package.
# ============================================================