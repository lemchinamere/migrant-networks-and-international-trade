# ============================================================
# Final data structure and panel checks
# ============================================================
#
# Purpose:
#   Load the final analysis panels, check their structure, document
#   missing observations, construct no-Eritrea robustness panels, load and
#   document delta-endpoint robustness panels, and save final structure
#   summaries.
#
# Script type:
#   Final data-structure / panel-check script
#
# Workflow logic:
#   This script is the final data-structure script before running the
#   empirical regressions.
#
#   It does not clean raw data and does not reconstruct outcome, treatment,
#   control, or IV variables from scratch.
#
#   Instead, it loads existing .rds panels, verifies and documents the final
#   datasets created in previous cleaning scripts, reconstructs fixed-effect
#   identifiers only if missing, loads delta-endpoint robustness panels,
#   constructs no-Eritrea robustness panels, and saves final structure
#   summaries.
#
# Technical data-construction order:
#   01_outcome.R
#     Constructs the export outcome data.
#
#   02_treatment.R
#     Constructs the main 2014–2016 protection-seeker treatment exposure.
#
#   03_instrument.R
#     Constructs Königstein-based instrument variables.
#
#   04_analysis.R
#     Constructs the main analysis panel.
#
#   05_controls.R
#     Constructs regional-control robustness panels.
#
#   06_rescaling.R
#     Constructs regression-ready variables scaled by 1,000 persons.
#
#   07_fixed_effects.R
#     Constructs fixed-effect identifiers.
#
#   08_delta_endpoint_variables.R
#     Constructs the alternative 2014–2017 delta-endpoint exposure variables
#     and the corresponding delta-endpoint robustness panels.
#
#   09_data_structure.R
#     Checks final panel structure and saves final panel diagnostics.
#
#   10_sources.R
#     Documents data sources and saved data objects.
#
# Main active panels:
#
#   1. analysis_panel
#      Main panel for the preferred specification.
#
#      Unit of observation:
#        federal_state × origin_country × year
#
#      Period:
#        2010–2025
#
#      Empirical use:
#        Preferred main specification with PPML and three-way fixed effects.
#
#   2. analysis_panel_controls
#      Robustness panel for specifications with explicit regional controls.
#
#      Unit of observation:
#        federal_state × origin_country × year
#
#      Period:
#        2010–2024
#
#      Empirical use:
#        Robustness specifications where federal_state × year fixed effects
#        are replaced by observed federal_state × year controls.
#
#   3. analysis_panel_delta_endpoint
#      Delta-endpoints robustness panel using 2014–2017 exposure change.
#
#      Unit of observation:
#        federal_state × origin_country × year
#
#      Period:
#        2010–2025
#
#      Empirical use:
#        Robustness specification using the alternative 2014–2017
#        protection-seeker exposure window constructed in
#        08_delta_endpoint_variables.R.
#
# Additional active robustness panels:
#
#   4. analysis_panel_no_eritrea
#      Main panel excluding Eritrea.
#
#   5. analysis_panel_controls_no_eritrea
#      Regional-control robustness panel excluding Eritrea.
#
#   6. analysis_panel_no_eritrea_delta_endpoint
#      Delta-endpoints robustness panel excluding Eritrea.
#
# Archived panels:
#
#   A1. analysis_panel_cepii
#       CEPII / gravity-control panel.
#
#   A2. analysis_panel_cepii_no_eritrea
#       CEPII panel excluding Eritrea.
#
#       Status:
#         Archived / not used in the active final robustness package.
#
# Preferred fixed effects:
#
#   fe_state_origin
#     = federal_state × origin_country fixed effect
#
#   fe_state_year
#     = federal_state × year fixed effect
#
#   fe_origin_year
#     = origin_country × year fixed effect
#
# Preferred estimator:
#   PPML with three-way fixed effects.
#
# Notes:
#   The active final robustness package consists of:
#     - regional-control robustness
#     - COVID-year exclusion robustness
#     - leave-one-origin-out robustness
#     - delta-endpoints robustness
#     - 2014 Königstein key
#     - no-Eritrea sample
#     - export weight as an alternative outcome
# ============================================================


# ============================================================
# Setup
# ============================================================

### Path

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")

### Locale: read the UTF-8 data files correctly regardless of the ambient
### locale. A bare Rscript in a C/POSIX locale otherwise mis-reads the UTF-8
### CSVs (only the interactive RStudio/R.app UTF-8 locale would work).
for (.utf8_locale in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
  if (suppressWarnings(Sys.setlocale("LC_CTYPE", .utf8_locale)) != "") break
}
rm(.utf8_locale)


### Packages

library(dplyr)
library(tidyr)
library(tibble)
library(stringr)


# ============================================================
# Required input files
# ============================================================
#
# Purpose:
#   Check whether all final panels required by this data-structure script
#   exist in the working directory.
#
# Required active panels:
#   analysis_panel.rds
#   analysis_panel_controls.rds
#   analysis_panel_delta_endpoint.rds
#   analysis_panel_no_eritrea_delta_endpoint.rds
#
# Required archived panel:
#   analysis_panel_cepii.rds
#
# Notes:
#   This script loads existing .rds panels. It does not rebuild them from raw
#   data.
# ============================================================

required_input_files <- c(
  "analysis_panel.rds",
  "analysis_panel_controls.rds",
  "analysis_panel_cepii.rds",
  "analysis_panel_delta_endpoint.rds",
  "analysis_panel_no_eritrea_delta_endpoint.rds"
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

missing_input_files

if (length(missing_input_files) > 0) {
  stop(
    paste(
      "The following required panel files are missing:",
      paste(missing_input_files, collapse = ", "),
      "Please rerun the relevant data-cleaning / panel-construction scripts before running this final panel-check script."
    )
  )
}


# ============================================================
# Load final panels
# ============================================================
#
# Purpose:
#   Load final full-sample active panels, delta-endpoint robustness panels,
#   and the archived CEPII panel.
#
# Notes:
#   CEPII panels are loaded only for archived documentation. They are not
#   part of the active final robustness package.
# ============================================================

analysis_panel <- readRDS(
  "analysis_panel.rds"
)

analysis_panel_controls <- readRDS(
  "analysis_panel_controls.rds"
)

analysis_panel_cepii <- readRDS(
  "analysis_panel_cepii.rds"
)

analysis_panel_delta_endpoint <- readRDS(
  "analysis_panel_delta_endpoint.rds"
)

analysis_panel_no_eritrea_delta_endpoint <- readRDS(
  "analysis_panel_no_eritrea_delta_endpoint.rds"
)


# ============================================================
# Define expected sample structure
# ============================================================
#
# Purpose:
#   Define the expected state-origin-year grids used to document missing
#   observations and panel dimensions.
# ============================================================

federal_states <- c(
  "Baden-Württemberg",
  "Bayern",  # Bavaria
  "Berlin",
  "Brandenburg",
  "Bremen",
  "Hamburg",
  "Hessen",  # Hesse
  "Mecklenburg-Vorpommern",  # Mecklenburg-Western Pomerania
  "Niedersachsen",  # Lower Saxony
  "Nordrhein-Westfalen",  # North Rhine-Westphalia
  "Rheinland-Pfalz",  # Rhineland-Palatinate
  "Saarland",
  "Sachsen",  # Saxony
  "Sachsen-Anhalt",  # Saxony-Anhalt
  "Schleswig-Holstein",
  "Thüringen"  # Thuringia
)

origin_countries <- c(
  "Afghanistan",
  "Eritrea",
  "Irak",
  "Iran, Islamische Republik",
  "Syrien"
)

origin_countries_no_eritrea <- origin_countries[
  origin_countries != "Eritrea"
]


# ============================================================
# Helper function: check required variables
# ============================================================

check_required_variables <- function(data, required_variables, panel_name) {
  tibble(
    panel = panel_name,
    required_variable = required_variables,
    present = required_variables %in% names(data)
  ) %>%
    filter(
      !present
    )
}


# ============================================================
# Helper function: reconstruct fixed-effect identifiers if missing
# ============================================================
#
# Purpose:
#   Fixed-effect identifiers should already be present in the final panels.
#   This helper reconstructs them only if they are missing.
#
# Notes:
#   This does not rebuild the panel or any treatment / IV / control
#   variables. It only ensures that the final panels contain consistent
#   fixed-effect identifiers.
# ============================================================

add_fixed_effects_if_missing <- function(data) {
  data %>%
    mutate(
      fe_state_origin = if (
        "fe_state_origin" %in% names(.)
      ) {
        fe_state_origin
      } else {
        interaction(
          federal_state,
          origin_country,
          drop = TRUE
        )
      },
      
      fe_state_year = if (
        "fe_state_year" %in% names(.)
      ) {
        fe_state_year
      } else {
        interaction(
          federal_state,
          year,
          drop = TRUE
        )
      },
      
      fe_origin_year = if (
        "fe_origin_year" %in% names(.)
      ) {
        fe_origin_year
      } else {
        interaction(
          origin_country,
          year,
          drop = TRUE
        )
      }
    )
}


# ============================================================
# Reconstruct fixed-effect identifiers only if missing
# ============================================================
#
# Purpose:
#   Run fixed-effect reconstruction before the required-variable check, so
#   that fixed effects that can be reconstructed from existing identifiers
#   are not falsely reported as missing.
# ============================================================

analysis_panel <- add_fixed_effects_if_missing(
  analysis_panel
)

analysis_panel_controls <- add_fixed_effects_if_missing(
  analysis_panel_controls
)

analysis_panel_cepii <- add_fixed_effects_if_missing(
  analysis_panel_cepii
)

analysis_panel_delta_endpoint <- add_fixed_effects_if_missing(
  analysis_panel_delta_endpoint
)

analysis_panel_no_eritrea_delta_endpoint <- add_fixed_effects_if_missing(
  analysis_panel_no_eritrea_delta_endpoint
)


# ============================================================
# Required variables
# ============================================================
#
# Purpose:
#   Check whether all variables required for the main specification and
#   active robustness checks are present in the final active panels.
#
# Notes:
#   analysis_panel is not required to contain regional controls. Regional
#   controls are required only in analysis_panel_controls.
# ============================================================

required_main_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "pair_id",
  
  "export_value",
  "log_export_value",
  "export_weight",
  
  "pre_period",
  "shock_period",
  "post_period",
  
  "protection_seekers_stock_2014",
  "protection_seekers_stock_2016",
  "delta_protection_seekers_2014_2016",
  
  "treatment_stock_2016_post",
  "treatment_delta_post",
  "treatment_stock_2016_post_1000",
  "treatment_delta_post_1000",
  
  "koenigstein_share_2014",
  "koenigstein_share_2015",
  "koenigstein_share_2016",
  "koenigstein_share_2015_2016_avg",
  "koenigstein_share_2014_2015_2016_avg",
  
  "national_protection_seekers_stock_2016",
  "national_delta_protection_seekers_2014_2016",
  
  "predicted_protection_seekers_stock_2016",
  "predicted_delta_protection_seekers_2014_2016",
  "iv_stock_2016_post",
  "iv_delta_post",
  "iv_stock_2016_post_1000",
  "iv_delta_post_1000",
  
  "predicted_protection_seekers_stock_2016_k14",
  "predicted_delta_protection_seekers_2014_2016_k14",
  "iv_stock_2016_post_k14",
  "iv_delta_post_k14",
  "iv_stock_2016_post_k14_1000",
  "iv_delta_post_k14_1000",
  
  "predicted_protection_seekers_stock_2016_k141516",
  "predicted_delta_protection_seekers_2014_2016_k141516",
  "iv_stock_2016_post_k141516",
  "iv_delta_post_k141516",
  "iv_stock_2016_post_k141516_1000",
  "iv_delta_post_k141516_1000",
  
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)

required_regional_control_variables <- c(
  "gdp_million_eur",
  "population",
  "unemployment_rate",
  "employment_thousand_persons",
  "gva_total",
  "gva_manufacturing",
  "manufacturing_share",
  "total_exports_world"
)

required_delta_endpoint_variables <- c(
  required_main_variables,
  "protection_seekers_stock_2017",
  "delta_protection_seekers_2014_2017",
  "national_protection_seekers_stock_2017",
  "national_delta_protection_seekers_2014_2017",
  "predicted_protection_seekers_stock_2017",
  "predicted_delta_protection_seekers_2014_2017",
  "treatment_delta_2014_2017_post",
  "iv_delta_2014_2017_post",
  "treatment_delta_2014_2017_post_1000",
  "iv_delta_2014_2017_post_1000"
)

archived_cepii_variables <- c(
  "iso3_d",
  "iso3_o",
  "dist",
  "contig",
  "comlang_off",
  "comlang_ethno",
  "comcol",
  "col45",
  "fta_wto"
)


missing_required_variables <- bind_rows(
  check_required_variables(
    analysis_panel,
    required_main_variables,
    "analysis_panel"
  ),
  
  check_required_variables(
    analysis_panel_controls,
    c(
      required_main_variables,
      required_regional_control_variables
    ),
    "analysis_panel_controls"
  ),
  
  check_required_variables(
    analysis_panel_delta_endpoint,
    required_delta_endpoint_variables,
    "analysis_panel_delta_endpoint"
  ),
  
  check_required_variables(
    analysis_panel_no_eritrea_delta_endpoint,
    required_delta_endpoint_variables,
    "analysis_panel_no_eritrea_delta_endpoint"
  )
)

missing_required_variables

if (nrow(missing_required_variables) > 0) {
  stop(
    "At least one required variable is missing from the active final panels. Inspect missing_required_variables."
  )
}


# ============================================================
# Archived CEPII variable check
# ============================================================
#
# Purpose:
#   Document whether archived CEPII variables are present.
#
# Notes:
#   These variables are not required for the active final regression package.
#   Therefore, missing archived CEPII variables are documented but do not stop
#   the script.
# ============================================================

missing_archived_cepii_variables <- check_required_variables(
  analysis_panel_cepii,
  c(
    required_main_variables,
    required_regional_control_variables,
    archived_cepii_variables
  ),
  "analysis_panel_cepii_archived"
)

missing_archived_cepii_variables


# ============================================================
# Final active full-sample panel summaries
# ============================================================

panel_summary <- tibble(
  panel = c(
    "analysis_panel",
    "analysis_panel_controls"
  ),
  active_status = c(
    "active main panel",
    "active regional-control robustness panel"
  ),
  period = c(
    "2010–2025",
    "2010–2024"
  ),
  theoretical_n = c(
    length(federal_states) * length(origin_countries) * length(2010:2025),
    length(federal_states) * length(origin_countries) * length(2010:2024)
  ),
  actual_n = c(
    nrow(analysis_panel),
    nrow(analysis_panel_controls)
  ),
  missing_from_balanced_panel = theoretical_n - actual_n,
  n_variables = c(
    ncol(analysis_panel),
    ncol(analysis_panel_controls)
  ),
  n_states = c(
    n_distinct(analysis_panel$federal_state),
    n_distinct(analysis_panel_controls$federal_state)
  ),
  n_origins = c(
    n_distinct(analysis_panel$origin_country),
    n_distinct(analysis_panel_controls$origin_country)
  ),
  min_year = c(
    min(analysis_panel$year, na.rm = TRUE),
    min(analysis_panel_controls$year, na.rm = TRUE)
  ),
  max_year = c(
    max(analysis_panel$year, na.rm = TRUE),
    max(analysis_panel_controls$year, na.rm = TRUE)
  )
)

panel_summary


# ============================================================
# Delta-endpoint panel summary
# ============================================================
#
# Purpose:
#   Summarise the active delta-endpoints robustness panels constructed in
#   08_delta_endpoint_variables.R.
#
# Notes:
#   This is treated as the eighth data-construction component in the
#   technical data pipeline.
# ============================================================

delta_endpoint_panel_summary_final <- tibble(
  panel = c(
    "analysis_panel_delta_endpoint",
    "analysis_panel_no_eritrea_delta_endpoint"
  ),
  active_status = c(
    "active delta-endpoints robustness panel",
    "active delta-endpoints robustness panel excluding Eritrea"
  ),
  period = c(
    "2010–2025",
    "2010–2025"
  ),
  theoretical_n = c(
    length(federal_states) *
      length(origin_countries) *
      length(2010:2025),
    length(federal_states) *
      length(origin_countries_no_eritrea) *
      length(2010:2025)
  ),
  actual_n = c(
    nrow(analysis_panel_delta_endpoint),
    nrow(analysis_panel_no_eritrea_delta_endpoint)
  ),
  missing_from_balanced_panel = theoretical_n - actual_n,
  n_variables = c(
    ncol(analysis_panel_delta_endpoint),
    ncol(analysis_panel_no_eritrea_delta_endpoint)
  ),
  n_states = c(
    n_distinct(analysis_panel_delta_endpoint$federal_state),
    n_distinct(analysis_panel_no_eritrea_delta_endpoint$federal_state)
  ),
  n_origins = c(
    n_distinct(analysis_panel_delta_endpoint$origin_country),
    n_distinct(analysis_panel_no_eritrea_delta_endpoint$origin_country)
  ),
  min_year = c(
    min(analysis_panel_delta_endpoint$year, na.rm = TRUE),
    min(analysis_panel_no_eritrea_delta_endpoint$year, na.rm = TRUE)
  ),
  max_year = c(
    max(analysis_panel_delta_endpoint$year, na.rm = TRUE),
    max(analysis_panel_no_eritrea_delta_endpoint$year, na.rm = TRUE)
  )
)

delta_endpoint_panel_summary_final


# ============================================================
# Archived CEPII panel summary
# ============================================================

archived_cepii_panel_summary <- tibble(
  panel = "analysis_panel_cepii",
  active_status = "archived / considered but not retained",
  reason_not_retained = paste(
    "CEPII gravity variables were absorbed by the remaining fixed effects",
    "and dropped due to collinearity in the attempted specification."
  ),
  period = "2010–2021",
  theoretical_n =
    length(federal_states) * length(origin_countries) * length(2010:2021),
  actual_n = nrow(analysis_panel_cepii),
  missing_from_balanced_panel = theoretical_n - actual_n,
  n_variables = ncol(analysis_panel_cepii),
  n_states = n_distinct(analysis_panel_cepii$federal_state),
  n_origins = n_distinct(analysis_panel_cepii$origin_country),
  min_year = min(analysis_panel_cepii$year, na.rm = TRUE),
  max_year = max(analysis_panel_cepii$year, na.rm = TRUE)
)

archived_cepii_panel_summary


# ============================================================
# Fixed-effect summaries
# ============================================================

fixed_effect_summary <- tibble(
  panel = c(
    "analysis_panel",
    "analysis_panel_controls",
    "analysis_panel_delta_endpoint",
    "analysis_panel_no_eritrea_delta_endpoint",
    "analysis_panel_cepii_archived"
  ),
  active_status = c(
    "active",
    "active",
    "active delta-endpoints robustness",
    "active delta-endpoints robustness excluding Eritrea",
    "archived / not used actively"
  ),
  n_obs = c(
    nrow(analysis_panel),
    nrow(analysis_panel_controls),
    nrow(analysis_panel_delta_endpoint),
    nrow(analysis_panel_no_eritrea_delta_endpoint),
    nrow(analysis_panel_cepii)
  ),
  n_fe_state_origin = c(
    n_distinct(analysis_panel$fe_state_origin),
    n_distinct(analysis_panel_controls$fe_state_origin),
    n_distinct(analysis_panel_delta_endpoint$fe_state_origin),
    n_distinct(analysis_panel_no_eritrea_delta_endpoint$fe_state_origin),
    n_distinct(analysis_panel_cepii$fe_state_origin)
  ),
  n_fe_state_year = c(
    n_distinct(analysis_panel$fe_state_year),
    n_distinct(analysis_panel_controls$fe_state_year),
    n_distinct(analysis_panel_delta_endpoint$fe_state_year),
    n_distinct(analysis_panel_no_eritrea_delta_endpoint$fe_state_year),
    n_distinct(analysis_panel_cepii$fe_state_year)
  ),
  n_fe_origin_year = c(
    n_distinct(analysis_panel$fe_origin_year),
    n_distinct(analysis_panel_controls$fe_origin_year),
    n_distinct(analysis_panel_delta_endpoint$fe_origin_year),
    n_distinct(analysis_panel_no_eritrea_delta_endpoint$fe_origin_year),
    n_distinct(analysis_panel_cepii$fe_origin_year)
  ),
  missing_fe_state_origin = c(
    sum(is.na(analysis_panel$fe_state_origin)),
    sum(is.na(analysis_panel_controls$fe_state_origin)),
    sum(is.na(analysis_panel_delta_endpoint$fe_state_origin)),
    sum(is.na(analysis_panel_no_eritrea_delta_endpoint$fe_state_origin)),
    sum(is.na(analysis_panel_cepii$fe_state_origin))
  ),
  missing_fe_state_year = c(
    sum(is.na(analysis_panel$fe_state_year)),
    sum(is.na(analysis_panel_controls$fe_state_year)),
    sum(is.na(analysis_panel_delta_endpoint$fe_state_year)),
    sum(is.na(analysis_panel_no_eritrea_delta_endpoint$fe_state_year)),
    sum(is.na(analysis_panel_cepii$fe_state_year))
  ),
  missing_fe_origin_year = c(
    sum(is.na(analysis_panel$fe_origin_year)),
    sum(is.na(analysis_panel_controls$fe_origin_year)),
    sum(is.na(analysis_panel_delta_endpoint$fe_origin_year)),
    sum(is.na(analysis_panel_no_eritrea_delta_endpoint$fe_origin_year)),
    sum(is.na(analysis_panel_cepii$fe_origin_year))
  )
)

fixed_effect_summary


# ============================================================
# Missing observations by panel
# ============================================================

construct_missing_observations <- function(data, years, origins = origin_countries) {
  full_grid <- expand.grid(
    federal_state = federal_states,
    origin_country = origins,
    year = years,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble()
  
  full_grid %>%
    anti_join(
      data %>%
        select(
          federal_state,
          origin_country,
          year
        ),
      by = c(
        "federal_state",
        "origin_country",
        "year"
      )
    ) %>%
    arrange(
      origin_country,
      federal_state,
      year
    )
}

missing_main_observations <- construct_missing_observations(
  analysis_panel,
  2010:2025
)

missing_controls_observations <- construct_missing_observations(
  analysis_panel_controls,
  2010:2024
)

missing_delta_endpoint_observations <- construct_missing_observations(
  analysis_panel_delta_endpoint,
  2010:2025
)

missing_delta_endpoint_no_eritrea_observations <- construct_missing_observations(
  analysis_panel_no_eritrea_delta_endpoint,
  2010:2025,
  origin_countries_no_eritrea
)

missing_cepii_observations_archived <- construct_missing_observations(
  analysis_panel_cepii,
  2010:2021
)


missing_observations_summary <- bind_rows(
  missing_main_observations %>%
    count(
      origin_country,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel",
      active_status = "active"
    ),
  
  missing_controls_observations %>%
    count(
      origin_country,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_controls",
      active_status = "active"
    ),
  
  missing_delta_endpoint_observations %>%
    count(
      origin_country,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_delta_endpoint",
      active_status = "active delta-endpoints robustness"
    ),
  
  missing_delta_endpoint_no_eritrea_observations %>%
    count(
      origin_country,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_no_eritrea_delta_endpoint",
      active_status = "active delta-endpoints robustness excluding Eritrea"
    ),
  
  missing_cepii_observations_archived %>%
    count(
      origin_country,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_cepii_archived",
      active_status = "archived / not used actively"
    )
) %>%
  select(
    active_status,
    panel,
    origin_country,
    missing_n
  ) %>%
  arrange(
    active_status,
    panel,
    origin_country
  )

missing_observations_summary


missing_observations_by_year <- bind_rows(
  missing_main_observations %>%
    count(
      year,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel",
      active_status = "active"
    ),
  
  missing_controls_observations %>%
    count(
      year,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_controls",
      active_status = "active"
    ),
  
  missing_delta_endpoint_observations %>%
    count(
      year,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_delta_endpoint",
      active_status = "active delta-endpoints robustness"
    ),
  
  missing_delta_endpoint_no_eritrea_observations %>%
    count(
      year,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_no_eritrea_delta_endpoint",
      active_status = "active delta-endpoints robustness excluding Eritrea"
    ),
  
  missing_cepii_observations_archived %>%
    count(
      year,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_cepii_archived",
      active_status = "archived / not used actively"
    )
) %>%
  select(
    active_status,
    panel,
    year,
    missing_n
  ) %>%
  arrange(
    active_status,
    panel,
    year
  )

missing_observations_by_year


missing_observations_by_state <- bind_rows(
  missing_main_observations %>%
    count(
      federal_state,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel",
      active_status = "active"
    ),
  
  missing_controls_observations %>%
    count(
      federal_state,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_controls",
      active_status = "active"
    ),
  
  missing_delta_endpoint_observations %>%
    count(
      federal_state,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_delta_endpoint",
      active_status = "active delta-endpoints robustness"
    ),
  
  missing_delta_endpoint_no_eritrea_observations %>%
    count(
      federal_state,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_no_eritrea_delta_endpoint",
      active_status = "active delta-endpoints robustness excluding Eritrea"
    ),
  
  missing_cepii_observations_archived %>%
    count(
      federal_state,
      name = "missing_n"
    ) %>%
    mutate(
      panel = "analysis_panel_cepii_archived",
      active_status = "archived / not used actively"
    )
) %>%
  select(
    active_status,
    panel,
    federal_state,
    missing_n
  ) %>%
  arrange(
    active_status,
    panel,
    federal_state
  )

missing_observations_by_state


# ============================================================
# Duplicate checks
# ============================================================

duplicate_main_rows <- analysis_panel %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )

duplicate_controls_rows <- analysis_panel_controls %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )

duplicate_delta_endpoint_rows <- analysis_panel_delta_endpoint %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )

duplicate_delta_endpoint_no_eritrea_rows <- analysis_panel_no_eritrea_delta_endpoint %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )

duplicate_cepii_rows_archived <- analysis_panel_cepii %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )

duplicate_summary <- tibble(
  panel = c(
    "analysis_panel",
    "analysis_panel_controls",
    "analysis_panel_delta_endpoint",
    "analysis_panel_no_eritrea_delta_endpoint",
    "analysis_panel_cepii_archived"
  ),
  active_status = c(
    "active",
    "active",
    "active delta-endpoints robustness",
    "active delta-endpoints robustness excluding Eritrea",
    "archived / not used actively"
  ),
  duplicate_rows = c(
    nrow(duplicate_main_rows),
    nrow(duplicate_controls_rows),
    nrow(duplicate_delta_endpoint_rows),
    nrow(duplicate_delta_endpoint_no_eritrea_rows),
    nrow(duplicate_cepii_rows_archived)
  )
)

duplicate_summary


# ============================================================
# Treatment and IV variation checks
# ============================================================

treatment_iv_variation_summary <- analysis_panel %>%
  summarise(
    min_treatment_stock =
      min(protection_seekers_stock_2016, na.rm = TRUE),
    max_treatment_stock =
      max(protection_seekers_stock_2016, na.rm = TRUE),
    
    min_predicted_stock =
      min(predicted_protection_seekers_stock_2016, na.rm = TRUE),
    max_predicted_stock =
      max(predicted_protection_seekers_stock_2016, na.rm = TRUE),
    
    min_treatment_stock_post =
      min(treatment_stock_2016_post, na.rm = TRUE),
    max_treatment_stock_post =
      max(treatment_stock_2016_post, na.rm = TRUE),
    
    min_iv_stock_post =
      min(iv_stock_2016_post, na.rm = TRUE),
    max_iv_stock_post =
      max(iv_stock_2016_post, na.rm = TRUE),
    
    min_treatment_stock_post_1000 =
      min(treatment_stock_2016_post_1000, na.rm = TRUE),
    max_treatment_stock_post_1000 =
      max(treatment_stock_2016_post_1000, na.rm = TRUE),
    
    min_iv_stock_post_1000 =
      min(iv_stock_2016_post_1000, na.rm = TRUE),
    max_iv_stock_post_1000 =
      max(iv_stock_2016_post_1000, na.rm = TRUE)
  )

treatment_iv_variation_summary


treatment_iv_correlation_summary <- analysis_panel %>%
  filter(
    post_period == 1
  ) %>%
  distinct(
    federal_state,
    origin_country,
    protection_seekers_stock_2016,
    predicted_protection_seekers_stock_2016,
    delta_protection_seekers_2014_2016,
    predicted_delta_protection_seekers_2014_2016
  ) %>%
  summarise(
    correlation_stock =
      cor(
        protection_seekers_stock_2016,
        predicted_protection_seekers_stock_2016,
        use = "complete.obs"
      ),
    correlation_delta =
      cor(
        delta_protection_seekers_2014_2016,
        predicted_delta_protection_seekers_2014_2016,
        use = "complete.obs"
      )
  )

treatment_iv_correlation_summary


robustness_iv_correlation_summary <- analysis_panel %>%
  filter(
    post_period == 1
  ) %>%
  distinct(
    federal_state,
    origin_country,
    iv_stock_2016_post,
    iv_stock_2016_post_k14,
    iv_stock_2016_post_k141516,
    iv_delta_post,
    iv_delta_post_k14,
    iv_delta_post_k141516
  ) %>%
  summarise(
    corr_stock_main_k14 =
      cor(
        iv_stock_2016_post,
        iv_stock_2016_post_k14,
        use = "complete.obs"
      ),
    corr_stock_main_k141516 =
      cor(
        iv_stock_2016_post,
        iv_stock_2016_post_k141516,
        use = "complete.obs"
      ),
    corr_delta_main_k14 =
      cor(
        iv_delta_post,
        iv_delta_post_k14,
        use = "complete.obs"
      ),
    corr_delta_main_k141516 =
      cor(
        iv_delta_post,
        iv_delta_post_k141516,
        use = "complete.obs"
      )
  )

robustness_iv_correlation_summary


delta_endpoint_treatment_iv_variation_summary <- analysis_panel_delta_endpoint %>%
  summarise(
    min_treatment_delta_2014_2017_post_1000 =
      min(treatment_delta_2014_2017_post_1000, na.rm = TRUE),
    max_treatment_delta_2014_2017_post_1000 =
      max(treatment_delta_2014_2017_post_1000, na.rm = TRUE),
    
    min_iv_delta_2014_2017_post_1000 =
      min(iv_delta_2014_2017_post_1000, na.rm = TRUE),
    max_iv_delta_2014_2017_post_1000 =
      max(iv_delta_2014_2017_post_1000, na.rm = TRUE)
  )

delta_endpoint_treatment_iv_variation_summary


delta_endpoint_treatment_iv_correlation_summary <- analysis_panel_delta_endpoint %>%
  filter(
    post_period == 1
  ) %>%
  distinct(
    federal_state,
    origin_country,
    delta_protection_seekers_2014_2017,
    predicted_delta_protection_seekers_2014_2017
  ) %>%
  summarise(
    correlation_delta_2014_2017 =
      cor(
        delta_protection_seekers_2014_2017,
        predicted_delta_protection_seekers_2014_2017,
        use = "complete.obs"
      )
  )

delta_endpoint_treatment_iv_correlation_summary


# ============================================================
# Period and interaction checks
# ============================================================

post_period_distribution <- analysis_panel %>%
  count(
    post_period
  )

post_period_distribution


period_distribution <- analysis_panel %>%
  count(
    pre_period,
    shock_period,
    post_period
  )

period_distribution


interaction_period_check <- analysis_panel %>%
  group_by(
    post_period
  ) %>%
  summarise(
    min_treatment_stock_post =
      min(treatment_stock_2016_post, na.rm = TRUE),
    max_treatment_stock_post =
      max(treatment_stock_2016_post, na.rm = TRUE),
    min_iv_stock_post =
      min(iv_stock_2016_post, na.rm = TRUE),
    max_iv_stock_post =
      max(iv_stock_2016_post, na.rm = TRUE),
    min_treatment_delta_post =
      min(treatment_delta_post, na.rm = TRUE),
    max_treatment_delta_post =
      max(treatment_delta_post, na.rm = TRUE),
    min_iv_delta_post =
      min(iv_delta_post, na.rm = TRUE),
    max_iv_delta_post =
      max(iv_delta_post, na.rm = TRUE),
    .groups = "drop"
  )

interaction_period_check


delta_endpoint_interaction_period_check <- analysis_panel_delta_endpoint %>%
  group_by(
    post_period
  ) %>%
  summarise(
    min_treatment_delta_2014_2017_post =
      min(treatment_delta_2014_2017_post, na.rm = TRUE),
    max_treatment_delta_2014_2017_post =
      max(treatment_delta_2014_2017_post, na.rm = TRUE),
    min_iv_delta_2014_2017_post =
      min(iv_delta_2014_2017_post, na.rm = TRUE),
    max_iv_delta_2014_2017_post =
      max(iv_delta_2014_2017_post, na.rm = TRUE),
    .groups = "drop"
  )

delta_endpoint_interaction_period_check


# ============================================================
# Regional control missingness
# ============================================================

regional_control_missingness_summary <- tibble(
  panel = c(
    "analysis_panel_controls"
  ),
  n_obs = c(
    nrow(analysis_panel_controls)
  ),
  missing_gdp = c(
    sum(is.na(analysis_panel_controls$gdp_million_eur))
  ),
  missing_population = c(
    sum(is.na(analysis_panel_controls$population))
  ),
  missing_unemployment_rate = c(
    sum(is.na(analysis_panel_controls$unemployment_rate))
  ),
  missing_employment = c(
    sum(is.na(analysis_panel_controls$employment_thousand_persons))
  ),
  missing_manufacturing_share = c(
    sum(is.na(analysis_panel_controls$manufacturing_share))
  ),
  missing_total_exports_world = c(
    sum(is.na(analysis_panel_controls$total_exports_world))
  )
)

regional_control_missingness_summary


# ============================================================
# Archived CEPII identification logic and variation checks
# ============================================================
#
# Purpose:
#   These checks document why the CEPII / gravity-control robustness is not
#   retained as an active robustness specification.
# ============================================================

cepii_origin_variation_summary <- analysis_panel_cepii %>%
  group_by(
    origin_country
  ) %>%
  summarise(
    iso3_d = first(iso3_d),
    dist = first(dist),
    contig = first(contig),
    comlang_off = first(comlang_off),
    comlang_ethno = first(comlang_ethno),
    comcol = first(comcol),
    col45 = first(col45),
    fta_wto_min = min(fta_wto, na.rm = TRUE),
    fta_wto_max = max(fta_wto, na.rm = TRUE),
    .groups = "drop"
  )

cepii_origin_variation_summary


cepii_within_pair_variation_summary <- analysis_panel_cepii %>%
  group_by(
    federal_state,
    origin_country
  ) %>%
  summarise(
    sd_dist = sd(dist, na.rm = TRUE),
    sd_contig = sd(contig, na.rm = TRUE),
    sd_comlang_off = sd(comlang_off, na.rm = TRUE),
    sd_comlang_ethno = sd(comlang_ethno, na.rm = TRUE),
    sd_comcol = sd(comcol, na.rm = TRUE),
    sd_col45 = sd(col45, na.rm = TRUE),
    sd_fta_wto = sd(fta_wto, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    varying_dist = sum(sd_dist > 0, na.rm = TRUE),
    varying_contig = sum(sd_contig > 0, na.rm = TRUE),
    varying_comlang_off = sum(sd_comlang_off > 0, na.rm = TRUE),
    varying_comlang_ethno = sum(sd_comlang_ethno > 0, na.rm = TRUE),
    varying_comcol = sum(sd_comcol > 0, na.rm = TRUE),
    varying_col45 = sum(sd_col45 > 0, na.rm = TRUE),
    varying_fta_wto = sum(sd_fta_wto > 0, na.rm = TRUE)
  )

cepii_within_pair_variation_summary


cepii_archive_note <- tibble(
  archived_check = "CEPII / gravity-control robustness",
  status = "considered but not retained",
  reason = paste(
    "In the attempted CEPII specification, federal_state × origin_country",
    "fixed effects were dropped and CEPII gravity controls were added.",
    "However, the CEPII variables were absorbed by the remaining fixed",
    "effects and dropped due to collinearity. Therefore, the CEPII check is",
    "archived and not included in the final active robustness package."
  )
)

cepii_archive_note


# ============================================================
# Robustness panels: excluding Eritrea
# ============================================================
#
# Purpose:
#   Construct active no-Eritrea robustness panels.
#
# Notes:
#   Missing export observations are concentrated in Eritrea. Excluding
#   Eritrea checks whether results are driven by this relatively sparse
#   origin-country panel.
# ============================================================

analysis_panel_no_eritrea <- analysis_panel %>%
  filter(
    origin_country != "Eritrea"
  ) %>%
  add_fixed_effects_if_missing()

analysis_panel_controls_no_eritrea <- analysis_panel_controls %>%
  filter(
    origin_country != "Eritrea"
  ) %>%
  add_fixed_effects_if_missing()

analysis_panel_cepii_no_eritrea <- analysis_panel_cepii %>%
  filter(
    origin_country != "Eritrea"
  ) %>%
  add_fixed_effects_if_missing()


no_eritrea_panel_summary <- tibble(
  panel = c(
    "analysis_panel_no_eritrea",
    "analysis_panel_controls_no_eritrea"
  ),
  active_status = c(
    "active robustness panel",
    "active regional-control robustness panel"
  ),
  period = c(
    "2010–2025",
    "2010–2024"
  ),
  theoretical_n = c(
    length(federal_states) *
      length(origin_countries_no_eritrea) *
      length(2010:2025),
    length(federal_states) *
      length(origin_countries_no_eritrea) *
      length(2010:2024)
  ),
  actual_n = c(
    nrow(analysis_panel_no_eritrea),
    nrow(analysis_panel_controls_no_eritrea)
  ),
  missing_from_balanced_panel = theoretical_n - actual_n,
  n_variables = c(
    ncol(analysis_panel_no_eritrea),
    ncol(analysis_panel_controls_no_eritrea)
  ),
  n_states = c(
    n_distinct(analysis_panel_no_eritrea$federal_state),
    n_distinct(analysis_panel_controls_no_eritrea$federal_state)
  ),
  n_origins = c(
    n_distinct(analysis_panel_no_eritrea$origin_country),
    n_distinct(analysis_panel_controls_no_eritrea$origin_country)
  ),
  min_year = c(
    min(analysis_panel_no_eritrea$year, na.rm = TRUE),
    min(analysis_panel_controls_no_eritrea$year, na.rm = TRUE)
  ),
  max_year = c(
    max(analysis_panel_no_eritrea$year, na.rm = TRUE),
    max(analysis_panel_controls_no_eritrea$year, na.rm = TRUE)
  )
)

no_eritrea_panel_summary


archived_cepii_no_eritrea_panel_summary <- tibble(
  panel = "analysis_panel_cepii_no_eritrea",
  active_status = "archived / not used actively",
  period = "2010–2021",
  theoretical_n =
    length(federal_states) *
    length(origin_countries_no_eritrea) *
    length(2010:2021),
  actual_n = nrow(analysis_panel_cepii_no_eritrea),
  missing_from_balanced_panel = theoretical_n - actual_n,
  n_variables = ncol(analysis_panel_cepii_no_eritrea),
  n_states = n_distinct(analysis_panel_cepii_no_eritrea$federal_state),
  n_origins = n_distinct(analysis_panel_cepii_no_eritrea$origin_country),
  min_year = min(analysis_panel_cepii_no_eritrea$year, na.rm = TRUE),
  max_year = max(analysis_panel_cepii_no_eritrea$year, na.rm = TRUE)
)

archived_cepii_no_eritrea_panel_summary


# ============================================================
# Combined final panel summary
# ============================================================
#
# Purpose:
#   Combine active full-sample, delta-endpoint, no-Eritrea, and archived
#   CEPII panel summaries.
#
# Notes:
#   The ordering follows the technical data-structure logic:
#     1. active main panels
#     2. delta-endpoint panels constructed in 08_delta_endpoint_variables.R
#     3. derived no-Eritrea panels
#     4. archived CEPII panels
# ============================================================

final_panel_summary <- bind_rows(
  panel_summary %>%
    mutate(
      sample = "full sample"
    ),
  
  delta_endpoint_panel_summary_final %>%
    mutate(
      sample = ifelse(
        panel == "analysis_panel_delta_endpoint",
        "full sample",
        "excluding Eritrea"
      )
    ),
  
  no_eritrea_panel_summary %>%
    mutate(
      sample = "excluding Eritrea"
    ),
  
  archived_cepii_panel_summary %>%
    mutate(
      sample = "full sample"
    ),
  
  archived_cepii_no_eritrea_panel_summary %>%
    mutate(
      sample = "excluding Eritrea"
    )
) %>%
  select(
    sample,
    active_status,
    panel,
    period,
    theoretical_n,
    actual_n,
    missing_from_balanced_panel,
    n_variables,
    n_states,
    n_origins,
    min_year,
    max_year,
    everything()
  )

final_panel_summary


# ============================================================
# Save final panels and structure summaries
# ============================================================

### Updated active full-sample panels

saveRDS(
  analysis_panel,
  "analysis_panel.rds"
)

saveRDS(
  analysis_panel_controls,
  "analysis_panel_controls.rds"
)


### Active delta-endpoints robustness panels

saveRDS(
  analysis_panel_delta_endpoint,
  "analysis_panel_delta_endpoint.rds"
)

saveRDS(
  analysis_panel_no_eritrea_delta_endpoint,
  "analysis_panel_no_eritrea_delta_endpoint.rds"
)


### Active no-Eritrea robustness panels

saveRDS(
  analysis_panel_no_eritrea,
  "analysis_panel_no_eritrea.rds"
)

saveRDS(
  analysis_panel_controls_no_eritrea,
  "analysis_panel_controls_no_eritrea.rds"
)


### Archived CEPII panels

saveRDS(
  analysis_panel_cepii,
  "analysis_panel_cepii.rds"
)

saveRDS(
  analysis_panel_cepii_no_eritrea,
  "analysis_panel_cepii_no_eritrea.rds"
)


### Structure summaries

saveRDS(
  required_input_files,
  "final_panel_required_input_files.rds"
)

saveRDS(
  missing_input_files,
  "final_panel_missing_input_files.rds"
)

saveRDS(
  panel_summary,
  "panel_summary.rds"
)

saveRDS(
  delta_endpoint_panel_summary_final,
  "delta_endpoint_panel_summary_final.rds"
)

saveRDS(
  no_eritrea_panel_summary,
  "no_eritrea_panel_summary.rds"
)

saveRDS(
  archived_cepii_panel_summary,
  "archived_cepii_panel_summary.rds"
)

saveRDS(
  archived_cepii_no_eritrea_panel_summary,
  "archived_cepii_no_eritrea_panel_summary.rds"
)

saveRDS(
  final_panel_summary,
  "final_panel_summary.rds"
)

saveRDS(
  fixed_effect_summary,
  "fixed_effect_summary.rds"
)

saveRDS(
  missing_required_variables,
  "missing_required_variables.rds"
)

saveRDS(
  missing_archived_cepii_variables,
  "missing_archived_cepii_variables.rds"
)

saveRDS(
  missing_main_observations,
  "missing_main_observations.rds"
)

saveRDS(
  missing_controls_observations,
  "missing_controls_observations.rds"
)

saveRDS(
  missing_delta_endpoint_observations,
  "missing_delta_endpoint_observations.rds"
)

saveRDS(
  missing_delta_endpoint_no_eritrea_observations,
  "missing_delta_endpoint_no_eritrea_observations.rds"
)

saveRDS(
  missing_cepii_observations_archived,
  "missing_cepii_observations_archived.rds"
)

saveRDS(
  missing_observations_summary,
  "missing_observations_summary.rds"
)

saveRDS(
  missing_observations_by_year,
  "missing_observations_by_year.rds"
)

saveRDS(
  missing_observations_by_state,
  "missing_observations_by_state.rds"
)

saveRDS(
  duplicate_summary,
  "duplicate_summary.rds"
)

saveRDS(
  duplicate_main_rows,
  "duplicate_main_rows.rds"
)

saveRDS(
  duplicate_controls_rows,
  "duplicate_controls_rows.rds"
)

saveRDS(
  duplicate_delta_endpoint_rows,
  "duplicate_delta_endpoint_rows.rds"
)

saveRDS(
  duplicate_delta_endpoint_no_eritrea_rows,
  "duplicate_delta_endpoint_no_eritrea_rows.rds"
)

saveRDS(
  duplicate_cepii_rows_archived,
  "duplicate_cepii_rows_archived.rds"
)

saveRDS(
  treatment_iv_variation_summary,
  "treatment_iv_variation_summary.rds"
)

saveRDS(
  treatment_iv_correlation_summary,
  "treatment_iv_correlation_summary.rds"
)

saveRDS(
  robustness_iv_correlation_summary,
  "robustness_iv_correlation_summary.rds"
)

saveRDS(
  delta_endpoint_treatment_iv_variation_summary,
  "delta_endpoint_treatment_iv_variation_summary.rds"
)

saveRDS(
  delta_endpoint_treatment_iv_correlation_summary,
  "delta_endpoint_treatment_iv_correlation_summary.rds"
)

saveRDS(
  post_period_distribution,
  "post_period_distribution.rds"
)

saveRDS(
  period_distribution,
  "period_distribution.rds"
)

saveRDS(
  interaction_period_check,
  "interaction_period_check.rds"
)

saveRDS(
  delta_endpoint_interaction_period_check,
  "delta_endpoint_interaction_period_check.rds"
)

saveRDS(
  regional_control_missingness_summary,
  "regional_control_missingness_summary.rds"
)

saveRDS(
  cepii_origin_variation_summary,
  "cepii_origin_variation_summary.rds"
)

saveRDS(
  cepii_within_pair_variation_summary,
  "cepii_within_pair_variation_summary.rds"
)

saveRDS(
  cepii_archive_note,
  "cepii_archive_note.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_main_variables,
  required_regional_control_variables,
  required_delta_endpoint_variables,
  archived_cepii_variables,
  check_required_variables,
  add_fixed_effects_if_missing,
  construct_missing_observations,
  federal_states,
  origin_countries,
  origin_countries_no_eritrea
)


# ============================================================
# Final objects kept
# ============================================================
#
# Active full-sample panels:
#   analysis_panel
#   analysis_panel_controls
#   analysis_panel_delta_endpoint
#
# Active no-Eritrea robustness panels:
#   analysis_panel_no_eritrea
#   analysis_panel_controls_no_eritrea
#   analysis_panel_no_eritrea_delta_endpoint
#
# Archived CEPII panels:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Main panel summaries:
#   final_panel_summary
#   panel_summary
#   delta_endpoint_panel_summary_final
#   no_eritrea_panel_summary
#   archived_cepii_panel_summary
#   archived_cepii_no_eritrea_panel_summary
#   fixed_effect_summary
#
# Required-variable checks:
#   missing_required_variables
#   missing_archived_cepii_variables
#
# Missing-observation diagnostics:
#   missing_main_observations
#   missing_controls_observations
#   missing_delta_endpoint_observations
#   missing_delta_endpoint_no_eritrea_observations
#   missing_cepii_observations_archived
#   missing_observations_summary
#   missing_observations_by_year
#   missing_observations_by_state
#
# Duplicate checks:
#   duplicate_main_rows
#   duplicate_controls_rows
#   duplicate_delta_endpoint_rows
#   duplicate_delta_endpoint_no_eritrea_rows
#   duplicate_cepii_rows_archived
#   duplicate_summary
#
# Treatment and IV diagnostics:
#   treatment_iv_variation_summary
#   treatment_iv_correlation_summary
#   robustness_iv_correlation_summary
#   delta_endpoint_treatment_iv_variation_summary
#   delta_endpoint_treatment_iv_correlation_summary
#
# Period and interaction diagnostics:
#   post_period_distribution
#   period_distribution
#   interaction_period_check
#   delta_endpoint_interaction_period_check
#
# Regional-control diagnostics:
#   regional_control_missingness_summary
#
# Archived CEPII diagnostics:
#   cepii_origin_variation_summary
#   cepii_within_pair_variation_summary
#   cepii_archive_note
#
# Notes:
#   analysis_panel is the active main panel for the preferred specification.
#
#   analysis_panel_controls is the active regional-control robustness panel.
#   It is restricted to 2010–2024 and contains complete regional controls.
#
#   analysis_panel_delta_endpoint is the active full-sample robustness panel
#   for the 2014–2017 delta-endpoints exposure-window robustness. It is
#   constructed in 08_delta_endpoint_variables.R and is treated as the eighth
#   data-construction component in the technical project pipeline.
#
#   analysis_panel_no_eritrea, analysis_panel_controls_no_eritrea, and
#   analysis_panel_no_eritrea_delta_endpoint are active robustness panels
#   excluding Eritrea.
#
#   analysis_panel_cepii and analysis_panel_cepii_no_eritrea are archived
#   only. They are retained for transparency because the CEPII /
#   gravity-control robustness check was considered but not retained:
#   the CEPII gravity controls were absorbed by the remaining fixed effects
#   and dropped due to collinearity.
#
#   This script is a final data-structure / panel-check script. It loads
#   existing .rds panels, verifies their structure, loads and documents
#   delta-endpoint panels, constructs no-Eritrea robustness panels, saves
#   diagnostics, and does not rebuild raw outcome, treatment, control, or IV
#   variables.
#
#   The active final robustness package therefore consists of:
#     - regional-control robustness
#     - COVID-year exclusion robustness
#     - leave-one-origin-out robustness
#     - delta-endpoints robustness
#     - 2014 Königstein key
#     - no-Eritrea sample
#     - export weight as an alternative outcome
# ============================================================