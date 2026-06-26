# ============================================================
# Archived empirical check: CEPII / gravity-control robustness
# ============================================================
#
# Purpose:
#   Document the attempted CEPII / gravity-control robustness check.
#
# Status:
#   Considered but not retained.
#
# Reason:
#   The intended robustness check was to drop federal_state × origin_country
#   fixed effects and include explicit CEPII gravity controls instead.
#   However, in the attempted specification with fe_state_year and
#   fe_origin_year fixed effects, the CEPII gravity controls were absorbed by
#   the remaining fixed effects and dropped due to collinearity.
#
# Interpretation:
#   Because the CEPII controls are not separately identified in the attempted
#   specification, this check is not used as an active robustness
#   specification in the final empirical analysis.
#
# Archived objects:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#   robustness_cepii_diagnostics
#   robustness_cepii_missingness
#   missing_robustness_cepii_variables
#   cepii_origin_variation_summary
#   cepii_within_pair_variation_summary
#   cepii_archive_note
#
# Note:
#   No CEPII regression estimates are used in the final empirical story.
#   The active final robustness package instead consists of:
#     - regional-control robustness
#     - COVID-year exclusion robustness
#     - leave-one-origin-out robustness
#     - delta-exposure robustness
#     - delta-endpoint robustness, 2014–2017
#     - alternative Königstein-key robustness
#     - no-Eritrea sample
#     - export weight as an alternative outcome
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
# Load archived CEPII panels
# ============================================================
#
# Purpose:
#   Load the archived CEPII panels that were constructed for the attempted
#   gravity-control robustness check.
#
# Archived panels:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Notes:
#   These panels are retained for documentation and reproducibility only.
#   They are not part of the active final empirical strategy.
# ============================================================

analysis_panel_cepii <- readRDS(
  "analysis_panel_cepii.rds"
)

analysis_panel_cepii_no_eritrea <- readRDS(
  "analysis_panel_cepii_no_eritrea.rds"
)


# ============================================================
# Defensive fixed-effect reconstruction
# ============================================================
#
# Purpose:
#   Ensure that the archived CEPII panels contain the fixed-effect
#   identifiers required by the attempted regression specification.
#
# Fixed effects:
#   fe_state_origin
#   = federal_state × origin_country
#
#   fe_state_year
#   = federal_state × year
#
#   fe_origin_year
#   = origin_country × year
#
# Notes:
#   If the fixed-effect identifiers already exist, they are kept unchanged.
#   If they are missing, they are reconstructed from the underlying panel
#   identifiers.
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

analysis_panel_cepii <- add_fixed_effects_if_missing(
  analysis_panel_cepii
)

analysis_panel_cepii_no_eritrea <- add_fixed_effects_if_missing(
  analysis_panel_cepii_no_eritrea
)


# ============================================================
# Construct logged distance for documentation
# ============================================================
#
# Purpose:
#   Construct logged bilateral distance for the archived CEPII
#   gravity-control diagnostics.
#
# Constructed variable:
#   log_dist
#   = log(dist)
#
# Logic:
#   The CEPII distance variable is converted to numeric before taking logs
#   to avoid problems if the archived panel stores distance as a character
#   variable.
#
# Notes:
#   This variable is created for documentation only. It is not used in an
#   active final robustness regression.
# ============================================================

analysis_panel_cepii <- analysis_panel_cepii %>%
  mutate(
    dist = as.numeric(dist),
    log_dist = log(dist)
  )

analysis_panel_cepii_no_eritrea <- analysis_panel_cepii_no_eritrea %>%
  mutate(
    dist = as.numeric(dist),
    log_dist = log(dist)
  )


# ============================================================
# Required-variable check for archived CEPII panels
# ============================================================
#
# Purpose:
#   Check whether the archived CEPII panels contain the variables that would
#   have been required for the attempted gravity-control robustness check.
#
# Variables checked:
#   Outcome variable, treatment and IV variables, fixed effects, and CEPII
#   gravity controls.
#
# Interpretation:
#   This check is diagnostic only. Missing variables are documented, but no
#   CEPII regression is retained in the final empirical analysis.
#
# Notes:
#   The check is run after reconstructing fixed effects and logged distance
#   so that it reflects the archived panels in their final documented form.
# ============================================================

required_robustness_cepii_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "export_value",
  "treatment_stock_2016_post_1000",
  "treatment_delta_post_1000",
  "iv_stock_2016_post_1000",
  "iv_delta_post_1000",
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year",
  "dist",
  "log_dist",
  "contig",
  "comlang_off",
  "comlang_ethno",
  "comcol",
  "col45",
  "fta_wto"
)

missing_robustness_cepii_variables <- bind_rows(
  tibble(
    panel = "analysis_panel_cepii",
    variable = required_robustness_cepii_variables,
    present = required_robustness_cepii_variables %in%
      names(analysis_panel_cepii)
  ),
  tibble(
    panel = "analysis_panel_cepii_no_eritrea",
    variable = required_robustness_cepii_variables,
    present = required_robustness_cepii_variables %in%
      names(analysis_panel_cepii_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_robustness_cepii_variables


# ============================================================
# Archived CEPII panel diagnostics
# ============================================================
#
# Purpose:
#   Summarise the basic structure and coverage of the archived CEPII panels.
#
# Checks:
#   Number of observations, number of federal_state × origin_country pairs,
#   number of Länder, number of origin countries, and year coverage.
#
# Interpretation:
#   These diagnostics document the archived CEPII sample. They are not used
#   to support an active robustness estimate.
# ============================================================

robustness_cepii_diagnostics <- bind_rows(
  analysis_panel_cepii %>%
    summarise(
      panel = "analysis_panel_cepii",
      active_status = "archived / not used actively",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      n_federal_states = n_distinct(federal_state),
      n_origin_countries = n_distinct(origin_country),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE)
    ),
  
  analysis_panel_cepii_no_eritrea %>%
    summarise(
      panel = "analysis_panel_cepii_no_eritrea",
      active_status = "archived / not used actively",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      n_federal_states = n_distinct(federal_state),
      n_origin_countries = n_distinct(origin_country),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE)
    )
)

robustness_cepii_diagnostics


# ============================================================
# Archived CEPII gravity-control missingness diagnostics
# ============================================================
#
# Purpose:
#   Document missing values in the CEPII gravity controls contained in the
#   archived CEPII panels.
#
# Gravity controls:
#   log_dist
#   contig
#   comlang_off
#   comlang_ethno
#   comcol
#   col45
#   fta_wto
#
# Interpretation:
#   These checks describe the archived CEPII data quality. They do not
#   imply that the controls are separately identified in the attempted
#   fixed-effect specification.
# ============================================================

gravity_control_variables <- c(
  "log_dist",
  "contig",
  "comlang_off",
  "comlang_ethno",
  "comcol",
  "col45",
  "fta_wto"
)

robustness_cepii_missingness <- bind_rows(
  analysis_panel_cepii %>%
    summarise(
      across(
        all_of(gravity_control_variables),
        ~ sum(is.na(.x)),
        .names = "missing_{.col}"
      )
    ) %>%
    mutate(
      panel = "analysis_panel_cepii",
      active_status = "archived / not used actively"
    ),
  
  analysis_panel_cepii_no_eritrea %>%
    summarise(
      across(
        all_of(gravity_control_variables),
        ~ sum(is.na(.x)),
        .names = "missing_{.col}"
      )
    ) %>%
    mutate(
      panel = "analysis_panel_cepii_no_eritrea",
      active_status = "archived / not used actively"
    )
) %>%
  select(
    active_status,
    panel,
    everything()
  )

robustness_cepii_missingness


# ============================================================
# Archived CEPII origin-level variation diagnostics
# ============================================================
#
# Purpose:
#   Document how the CEPII gravity controls vary across origin countries in
#   the archived CEPII panel.
#
# Logic:
#   Most CEPII gravity controls are bilateral Germany-origin-country
#   variables and therefore vary across origin countries, not across German
#   Länder.
#
# Interpretation:
#   This diagnostic helps explain why the intended gravity-control
#   robustness check is not separately informative under the preferred
#   fixed-effect structure.
# ============================================================

cepii_origin_variation_summary <- analysis_panel_cepii %>%
  group_by(
    origin_country
  ) %>%
  summarise(
    iso3_d = first(iso3_d),
    dist = first(dist),
    log_dist = first(log_dist),
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


# ============================================================
# Archived CEPII within-pair variation diagnostics
# ============================================================
#
# Purpose:
#   Check whether CEPII gravity controls vary within
#   federal_state × origin_country pairs over time.
#
# Logic:
#   The preferred empirical strategy uses federal_state × origin_country
#   fixed effects. Time-invariant bilateral controls are absorbed by these
#   fixed effects. Controls that only vary at the origin-year level may also
#   be absorbed by origin_country × year fixed effects.
#
# Interpretation:
#   Low or zero within-pair variation supports the decision to archive the
#   CEPII gravity-control robustness check rather than use it as an active
#   final specification.
# ============================================================

cepii_within_pair_variation_summary <- analysis_panel_cepii %>%
  group_by(
    federal_state,
    origin_country
  ) %>%
  summarise(
    sd_log_dist = sd(log_dist, na.rm = TRUE),
    sd_contig = sd(contig, na.rm = TRUE),
    sd_comlang_off = sd(comlang_off, na.rm = TRUE),
    sd_comlang_ethno = sd(comlang_ethno, na.rm = TRUE),
    sd_comcol = sd(comcol, na.rm = TRUE),
    sd_col45 = sd(col45, na.rm = TRUE),
    sd_fta_wto = sd(fta_wto, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    varying_log_dist = sum(sd_log_dist > 0, na.rm = TRUE),
    varying_contig = sum(sd_contig > 0, na.rm = TRUE),
    varying_comlang_off = sum(sd_comlang_off > 0, na.rm = TRUE),
    varying_comlang_ethno = sum(sd_comlang_ethno > 0, na.rm = TRUE),
    varying_comcol = sum(sd_comcol > 0, na.rm = TRUE),
    varying_col45 = sum(sd_col45 > 0, na.rm = TRUE),
    varying_fta_wto = sum(sd_fta_wto > 0, na.rm = TRUE)
  )

cepii_within_pair_variation_summary


# ============================================================
# Archive note
# ============================================================
#
# Purpose:
#   Store a concise written note documenting why the CEPII / gravity-control
#   robustness check is archived and not used in the final empirical
#   analysis.
#
# Interpretation:
#   This object provides a reproducible paper-trail for the specification
#   decision and helps avoid accidentally treating CEPII estimates as active
#   final results.
# ============================================================

cepii_archive_note <- tibble(
  archived_check = "CEPII / gravity-control robustness",
  status = "considered but not retained",
  attempted_specification = paste(
    "export_value on exposure variable plus CEPII gravity controls,",
    "with fe_state_year and fe_origin_year fixed effects,",
    "clustered at fe_state_origin level."
  ),
  reason_not_retained = paste(
    "In the attempted specification, the CEPII gravity controls were absorbed",
    "by the remaining fixed effects and dropped due to collinearity.",
    "Therefore, this check is archived and not used in the final active",
    "robustness package."
  ),
  final_empirical_use = "archived documentation only"
)

cepii_archive_note


# ============================================================
# Save archived CEPII documentation outputs
# ============================================================
#
# Purpose:
#   Save the archived CEPII panels and diagnostic objects for
#   reproducibility.
#
# Notes:
#   These outputs document the attempted robustness check but do not enter
#   the active final empirical story.
# ============================================================

saveRDS(
  analysis_panel_cepii,
  "analysis_panel_cepii.rds"
)

saveRDS(
  analysis_panel_cepii_no_eritrea,
  "analysis_panel_cepii_no_eritrea.rds"
)

saveRDS(
  robustness_cepii_diagnostics,
  "robustness_cepii_diagnostics.rds"
)

saveRDS(
  robustness_cepii_missingness,
  "robustness_cepii_missingness.rds"
)

saveRDS(
  missing_robustness_cepii_variables,
  "missing_robustness_cepii_variables.rds"
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
  required_robustness_cepii_variables,
  gravity_control_variables,
  add_fixed_effects_if_missing
)


# ============================================================
# Final objects kept
# ============================================================
#
# Archived CEPII panels:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Archived CEPII diagnostics:
#   robustness_cepii_diagnostics
#   robustness_cepii_missingness
#   missing_robustness_cepii_variables
#   cepii_origin_variation_summary
#   cepii_within_pair_variation_summary
#   cepii_archive_note
#
# Notes:
#   No CEPII regression models are kept because the CEPII / gravity-control
#   robustness check is not part of the active final empirical strategy.
#
#   The CEPII check was considered but not retained because the CEPII gravity
#   controls were absorbed by the remaining fixed effects and dropped due to
#   collinearity in the attempted specification.
#
#   The active final robustness package instead uses:
#     - regional-control robustness
#     - COVID-year exclusion robustness
#     - leave-one-origin-out robustness
#     - delta-exposure robustness
#     - delta-endpoint robustness, 2014–2017
#     - alternative Königstein-key robustness
#     - no-Eritrea sample
#     - export weight as an alternative outcome
# ============================================================