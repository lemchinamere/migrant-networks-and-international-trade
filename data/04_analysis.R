# ============================================================
# Construct main analysis panel: outcome, treatment and instrument data
# ============================================================
#
# Purpose:
#   Construct the main analysis panel by merging:
#     1. Outcome panel: export_value_thousand_eur
#     2. Treatment data: protection_seekers_stock
#     3. Instrument data: koenigstein_key
#
# Unit of observation:
#   federal_state × origin_country × year
#
# Main treatment:
#   treatment_stock_2016_post
#   = protection_seekers_stock_2016 × post_period
#
# Main instrument:
#   iv_stock_2016_post
#   = predicted_protection_seekers_stock_2016 × post_period
#
# Main IV basis:
#   koenigstein_share_2015_2016_avg
#
# Robustness treatment:
#   treatment_delta_post
#   = delta_protection_seekers_2014_2016 × post_period
#
# Robustness instrument:
#   iv_delta_post
#   = predicted_delta_protection_seekers_2014_2016 × post_period
#
# Additional robustness IV bases:
#   1. koenigstein_share_2014
#      Strictly pre-shock allocation key.
#
#   2. koenigstein_share_2014_2015_2016_avg
#      Three-year average over 2014, 2015 and 2016.
#
# Notes:
#   Treatment and IV variables are measured in persons in the raw panel.
#   Regression-ready versions scaled by 1,000 persons are created in the
#   separate rescaling script.
#
#   This script constructs the main analysis panel only.
#   Regional controls are merged in a separate controls script.
#   CEPII gravity controls were considered separately but are not part of the
#   active final empirical strategy.
#
# Final output objects:
#   analysis_panel
#   analysis_panel_summary
#   duplicate_analysis_panel_rows
#   treatment_iv_variation_summary
#   treatment_iv_correlation_summary
#   robustness_iv_correlation_summary
#   post_period_distribution
#   period_distribution
#   interaction_period_check
#   national_treatment_totals
# ============================================================


# ============================================================
# Setup
# ============================================================

### Path

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")


### Packages

library(dplyr)
library(stringr)
library(tibble)


### Define German Länder

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


### Define final sample of origin countries

origin_countries <- c(
  "Afghanistan",
  "Eritrea",
  "Irak",
  "Iran, Islamische Republik",
  "Syrien"
)


# ============================================================
# Load cleaned input datasets
# ============================================================
#
# Purpose:
#   Load the cleaned outcome, treatment, and Königstein-key datasets used
#   to construct the main analysis panel.
#
# Input datasets:
#   export_value_thousand_eur
#   protection_seekers_stock
#   koenigstein_key
#
# Notes:
#   These objects are expected to have been created in earlier data-cleaning
#   scripts. This script does not clean the raw Destatis files again.
# ============================================================

export_value_thousand_eur <- readRDS(
  "export_value_thousand_eur.rds"
)

protection_seekers_stock <- readRDS(
  "protection_seekers_stock.rds"
)

koenigstein_key <- readRDS(
  "koenigstein_key.rds"
)


# ============================================================
# Harmonise and restrict input datasets
# ============================================================
#
# Purpose:
#   Harmonise country and federal-state names across the outcome,
#   treatment, and Königstein-key datasets, and restrict all inputs to the
#   selected origin countries.
#
# Input datasets:
#   export_value_thousand_eur
#   protection_seekers_stock
#   koenigstein_key
#
# Countries retained:
#   Afghanistan
#   Eritrea
#   Irak (Iraq)
#   Iran, Islamische Republik (Iran, Islamic Republic)
#   Syrien (Syria)
#
# Harmonisation:
#   Country names are standardised so that the outcome and treatment data
#   use the same origin-country labels.
#
# Königstein shares:
#   Königstein allocation shares are converted to numeric values.
#   The 2014–2016 average allocation share is created if it is not already
#   included in the input data.
#
# Notes:
#   This step ensures that the subsequent merge does not fail because of
#   inconsistent spelling, whitespace, or country-name conventions across
#   datasets.
# ============================================================

export_value_thousand_eur <- export_value_thousand_eur %>%
  mutate(
    federal_state = str_squish(federal_state),
    origin_country = case_when(
      origin_country == "Eritrea (ab 1994)" ~ "Eritrea",
      origin_country == "Islamische Republik Iran" ~
        "Iran, Islamische Republik",
      origin_country == "Arabische Republik Syrien" ~ "Syrien",
      TRUE ~ origin_country
    )
  ) %>%
  filter(
    federal_state %in% federal_states,
    origin_country %in% origin_countries
  )


protection_seekers_stock <- protection_seekers_stock %>%
  mutate(
    federal_state = str_squish(federal_state),
    origin_country = case_when(
      origin_country == "Eritrea (ab 1994)" ~ "Eritrea",
      origin_country == "Islamische Republik Iran" ~
        "Iran, Islamische Republik",
      origin_country == "Arabische Republik Syrien" ~ "Syrien",
      TRUE ~ origin_country
    )
  ) %>%
  filter(
    federal_state %in% federal_states,
    origin_country %in% origin_countries
  )


koenigstein_key <- koenigstein_key %>%
  mutate(
    federal_state = str_squish(federal_state),
    across(
      starts_with("koenigstein_share_"),
      as.numeric
    ),
    koenigstein_share_2014_2015_2016_avg =
      ifelse(
        "koenigstein_share_2014_2015_2016_avg" %in% names(.),
        koenigstein_share_2014_2015_2016_avg,
        (
          koenigstein_share_2014 +
            koenigstein_share_2015 +
            koenigstein_share_2016
        ) / 3
      )
  ) %>%
  filter(
    federal_state %in% federal_states
  )


# ============================================================
# Merge outcome, treatment and Königstein key
# ============================================================
#
# Purpose:
#   Merge the cleaned outcome panel, protection-seeker treatment data, and
#   Königstein allocation shares into the main analysis panel.
#
# Input datasets:
#   export_value_thousand_eur
#   protection_seekers_stock
#   koenigstein_key
#
# Merge keys:
#   Outcome and treatment data:
#     federal_state × origin_country
#
#   Königstein allocation shares:
#     federal_state
#
# Logic:
#   The treatment variables are measured at the federal_state × origin_country
#   level and are merged into the yearly outcome panel.
#
#   The Königstein allocation shares vary by Land and are merged
#   by federal_state.
#
# Unit of observation after merge:
#   federal_state × origin_country × year
#
# Notes:
#   Treatment and Königstein variables are repeated across years after the
#   merge because they define time-invariant exposure measures that are
#   interacted with the post-period indicator below.
# ============================================================

analysis_panel <- export_value_thousand_eur %>%
  left_join(
    protection_seekers_stock,
    by = c(
      "federal_state",
      "origin_country"
    )
  ) %>%
  left_join(
    koenigstein_key,
    by = "federal_state"
  )


# ============================================================
# Construct national treatment totals
# ============================================================
#
# Purpose:
#   Construct origin-specific national protection-seeker totals used to
#   build predicted regional exposure.
#
# Constructed variables:
#   national_protection_seekers_stock_2016
#   national_delta_protection_seekers_2014_2016
#
# Logic:
#   National totals are computed by summing protection-seeker stocks across
#   all Länder within each origin country.
#
# Important:
#   After merging treatment data into the outcome panel, treatment variables
#   are constant within federal_state × origin_country and repeated across
#   years. Therefore, national totals must be computed from distinct
#   federal_state × origin_country observations, not from the full
#   federal_state × origin_country × year panel.
#
# Interpretation:
#   These origin-specific national totals provide the national exposure
#   component that is allocated across Länder using Königstein
#   allocation shares.
# ============================================================

national_treatment_totals <- protection_seekers_stock %>%
  distinct(
    federal_state,
    origin_country,
    protection_seekers_stock_2016,
    delta_protection_seekers_2014_2016
  ) %>%
  group_by(
    origin_country
  ) %>%
  summarise(
    national_protection_seekers_stock_2016 =
      sum(
        protection_seekers_stock_2016,
        na.rm = TRUE
      ),
    national_delta_protection_seekers_2014_2016 =
      sum(
        delta_protection_seekers_2014_2016,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


analysis_panel <- analysis_panel %>%
  left_join(
    national_treatment_totals,
    by = "origin_country"
  )


# ============================================================
# Construct main predicted exposure
# ============================================================
#
# Purpose:
#   Construct the main predicted regional protection-seeker exposure used
#   as the basis for the instrument.
#
# Main IV basis:
#   koenigstein_share_2015_2016_avg
#
# Constructed variables:
#   predicted_protection_seekers_stock_2016
#   = national_protection_seekers_stock_2016
#     × koenigstein_share_2015_2016_avg
#
#   predicted_delta_protection_seekers_2014_2016
#   = national_delta_protection_seekers_2014_2016
#     × koenigstein_share_2015_2016_avg
#
# Interpretation:
#   The predicted exposure allocates national origin-specific protection-
#   seeker stocks or national origin-specific changes across Länder
#   according to the cohort-relevant Königstein allocation shares in 2015
#   and 2016.
#
# Notes:
#   The actual instruments are constructed below by interacting predicted
#   exposure with the post-period indicator:
#
#     iv_stock_2016_post
#     = predicted_protection_seekers_stock_2016 × post_period
#
#     iv_delta_post
#     = predicted_delta_protection_seekers_2014_2016 × post_period
# ============================================================

analysis_panel <- analysis_panel %>%
  mutate(
    predicted_protection_seekers_stock_2016 =
      koenigstein_share_2015_2016_avg *
      national_protection_seekers_stock_2016,
    
    predicted_delta_protection_seekers_2014_2016 =
      koenigstein_share_2015_2016_avg *
      national_delta_protection_seekers_2014_2016
  )


# ============================================================
# Construct main treatment and IV post-period interactions
# ============================================================
#
# Purpose:
#   Construct the main treatment and instrument variables used in the
#   baseline empirical specifications.
#
# Main treatment:
#   treatment_stock_2016_post
#   = protection_seekers_stock_2016 × post_period
#
# Main instrument:
#   iv_stock_2016_post
#   = predicted_protection_seekers_stock_2016 × post_period
#
# Alternative treatment:
#   treatment_delta_post
#   = delta_protection_seekers_2014_2016 × post_period
#
# Alternative instrument:
#   iv_delta_post
#   = predicted_delta_protection_seekers_2014_2016 × post_period
#
# Logic:
#   The exposure variables measure regional exposure to protection seekers
#   after the 2015/16 refugee inflow. Interacting them with the post-period
#   indicator assigns this exposure to the post-treatment years.
#
# Interpretation:
#   treatment_stock_2016_post captures actual regional stock exposure in
#   the post period.
#
#   iv_stock_2016_post captures predicted regional stock exposure in the
#   post period based on national origin-specific protection-seeker stocks
#   and Königstein allocation shares.
#
#   treatment_delta_post and iv_delta_post provide analogous variables based
#   on the 2014–2016 change in protection-seeker exposure.
#
# Notes:
#   These variables are measured in persons. Regression-ready versions
#   scaled by 1,000 persons are created in the separate rescaling script.
# ============================================================

analysis_panel <- analysis_panel %>%
  mutate(
    treatment_stock_2016_post =
      protection_seekers_stock_2016 * post_period,
    
    iv_stock_2016_post =
      predicted_protection_seekers_stock_2016 * post_period,
    
    treatment_delta_post =
      delta_protection_seekers_2014_2016 * post_period,
    
    iv_delta_post =
      predicted_delta_protection_seekers_2014_2016 * post_period
  )


# ============================================================
# Robustness IVs: alternative Königstein allocation keys
# ============================================================
#
# Purpose:
#   Construct alternative predicted exposure measures using different
#   Königstein allocation rules.
#
# Main IV basis:
#   koenigstein_share_2015_2016_avg
#
# Robustness IV bases:
#   1. koenigstein_share_2014
#      Strictly pre-shock allocation rule.
#
#   2. koenigstein_share_2014_2015_2016_avg
#      Three-year average over pre-shock and shock years.
#
# Constructed variables:
#   predicted_protection_seekers_stock_2016_k14
#   predicted_delta_protection_seekers_2014_2016_k14
#   iv_stock_2016_post_k14
#   iv_delta_post_k14
#
#   predicted_protection_seekers_stock_2016_k141516
#   predicted_delta_protection_seekers_2014_2016_k141516
#   iv_stock_2016_post_k141516
#   iv_delta_post_k141516
#
# Interpretation:
#   The alternative Königstein keys test whether the predicted-exposure
#   results depend on using the 2015–2016 average allocation shares as the
#   main IV basis.
#
# Note:
#   The 2014-key IV is retained as an active robustness check.
#   The 2014–2016 average IV is constructed for transparency but is not used
#   as a central robustness specification in the final empirical analysis.
# ============================================================

analysis_panel <- analysis_panel %>%
  mutate(
    # Robustness 1: 2014 key only
    predicted_protection_seekers_stock_2016_k14 =
      koenigstein_share_2014 *
      national_protection_seekers_stock_2016,
    
    predicted_delta_protection_seekers_2014_2016_k14 =
      koenigstein_share_2014 *
      national_delta_protection_seekers_2014_2016,
    
    iv_stock_2016_post_k14 =
      predicted_protection_seekers_stock_2016_k14 *
      post_period,
    
    iv_delta_post_k14 =
      predicted_delta_protection_seekers_2014_2016_k14 *
      post_period,
    
    
    # Robustness 2: 2014–2016 three-year average
    predicted_protection_seekers_stock_2016_k141516 =
      koenigstein_share_2014_2015_2016_avg *
      national_protection_seekers_stock_2016,
    
    predicted_delta_protection_seekers_2014_2016_k141516 =
      koenigstein_share_2014_2015_2016_avg *
      national_delta_protection_seekers_2014_2016,
    
    iv_stock_2016_post_k141516 =
      predicted_protection_seekers_stock_2016_k141516 *
      post_period,
    
    iv_delta_post_k141516 =
      predicted_delta_protection_seekers_2014_2016_k141516 *
      post_period
  )


# ============================================================
# Construct identifiers and fixed effects
# ============================================================
#
# Purpose:
#   Construct panel identifiers and fixed-effect variables used in the
#   empirical specifications.
#
# Identifiers:
#   pair_id
#   = federal_state × origin_country identifier
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
# Interpretation:
#   These fixed effects absorb time-invariant federal_state × origin_country
#   differences, federal-state-specific year shocks, and origin-country-
#   specific year shocks.
#
# Notes:
#   Treatment and IV variables scaled by 1,000 persons are constructed in
#   the separate rescaling script.
# ============================================================

analysis_panel <- analysis_panel %>%
  mutate(
    pair_id =
      paste(
        federal_state,
        origin_country,
        sep = "_"
      ),
    
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


# ============================================================
# Remove missing export observations and ensure unique panel
# ============================================================
#
# Purpose:
#   Keep only observations with non-missing export outcomes and ensure that
#   the analysis panel contains one observation per
#   federal_state × origin_country × year.
#
# Logic:
#   Export observations with missing export values cannot be used in the
#   main outcome regressions. Duplicate rows would violate the intended panel
#   structure and could overweight individual state-origin-year cells.
#
# Unit of observation:
#   federal_state × origin_country × year
#
# Main outcome:
#   export_value
#
# Notes:
#   This step defines the final estimation sample of the main analysis panel.
#   Missing observations relative to a fully balanced panel are documented in
#   the outcome-data cleaning script.
# ============================================================

analysis_panel <- analysis_panel %>%
  filter(
    !is.na(export_value)
  ) %>%
  distinct(
    federal_state,
    origin_country,
    year,
    .keep_all = TRUE
  ) %>%
  arrange(
    origin_country,
    federal_state,
    year
  )


# ============================================================
# Keep final variable order
# ============================================================
#
# Purpose:
#   Arrange the main analysis panel variables in a transparent and
#   reproducible order.
#
# Logic:
#   The panel is ordered by identifiers, outcome variables, period
#   indicators, treatment variables, Königstein allocation shares,
#   predicted exposure variables, instruments, robustness IVs, and fixed
#   effects.
#
# Notes:
#   Variables scaled by 1,000 persons are not selected here because they are
#   constructed in the separate rescaling script.
# ============================================================

analysis_panel <- analysis_panel %>%
  select(
    federal_state,
    origin_country,
    year,
    pair_id,
    
    export_value,
    log_export_value,
    export_weight,
    
    pre_period,
    shock_period,
    post_period,
    
    protection_seekers_stock_2014,
    protection_seekers_stock_2016,
    delta_protection_seekers_2014_2016,
    
    treatment_stock_2016_post,
    treatment_delta_post,
    
    koenigstein_share_2014,
    koenigstein_share_2015,
    koenigstein_share_2016,
    koenigstein_share_2015_2016_avg,
    koenigstein_share_2014_2015_2016_avg,
    
    national_protection_seekers_stock_2016,
    national_delta_protection_seekers_2014_2016,
    
    predicted_protection_seekers_stock_2016,
    predicted_delta_protection_seekers_2014_2016,
    iv_stock_2016_post,
    iv_delta_post,
    
    predicted_protection_seekers_stock_2016_k14,
    predicted_delta_protection_seekers_2014_2016_k14,
    iv_stock_2016_post_k14,
    iv_delta_post_k14,
    
    predicted_protection_seekers_stock_2016_k141516,
    predicted_delta_protection_seekers_2014_2016_k141516,
    iv_stock_2016_post_k141516,
    iv_delta_post_k141516,
    
    fe_state_origin,
    fe_state_year,
    fe_origin_year
  )


# ============================================================
# Analysis panel checks
# ============================================================
#
# Purpose:
#   Check the structure, coverage, missing values and uniqueness of the
#   main analysis panel.
#
# Unit of observation:
#   federal_state × origin_country × year
#
# Checks:
#   The summary verifies sample size, year coverage, country coverage,
#   missing values in outcome, treatment, instrument and fixed-effect
#   variables, and duplicate panel observations.
#
# Notes:
#   Variables scaled by 1,000 persons are checked in the separate rescaling
#   script after they are constructed.
# ============================================================

analysis_panel_summary <- analysis_panel %>%
  summarise(
    n_obs = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    
    missing_export_value =
      sum(is.na(export_value)),
    
    missing_log_export_value =
      sum(is.na(log_export_value)),
    
    missing_export_weight =
      sum(is.na(export_weight)),
    
    missing_treatment_stock =
      sum(is.na(protection_seekers_stock_2016)),
    
    missing_treatment_delta =
      sum(is.na(delta_protection_seekers_2014_2016)),
    
    missing_koenigstein_share_main =
      sum(is.na(koenigstein_share_2015_2016_avg)),
    
    missing_koenigstein_share_2014 =
      sum(is.na(koenigstein_share_2014)),
    
    missing_koenigstein_share_2014_2015_2016_avg =
      sum(is.na(koenigstein_share_2014_2015_2016_avg)),
    
    missing_predicted_stock =
      sum(is.na(predicted_protection_seekers_stock_2016)),
    
    missing_predicted_delta =
      sum(is.na(predicted_delta_protection_seekers_2014_2016)),
    
    missing_iv_stock_post =
      sum(is.na(iv_stock_2016_post)),
    
    missing_iv_delta_post =
      sum(is.na(iv_delta_post)),
    
    missing_iv_stock_post_k14 =
      sum(is.na(iv_stock_2016_post_k14)),
    
    missing_iv_delta_post_k14 =
      sum(is.na(iv_delta_post_k14)),
    
    missing_iv_stock_post_k141516 =
      sum(is.na(iv_stock_2016_post_k141516)),
    
    missing_iv_delta_post_k141516 =
      sum(is.na(iv_delta_post_k141516)),
    
    missing_fe_state_origin =
      sum(is.na(fe_state_origin)),
    
    missing_fe_state_year =
      sum(is.na(fe_state_year)),
    
    missing_fe_origin_year =
      sum(is.na(fe_origin_year))
  )

analysis_panel_summary


# ============================================================
# Check duplicate panel observations
# ============================================================
#
# Purpose:
#   Verify that the final main analysis panel contains at most one
#   observation per federal_state × origin_country × year.
#
# Interpretation:
#   The resulting object should be empty. Any positive count would indicate
#   duplicate panel observations that require inspection.
# ============================================================

duplicate_analysis_panel_rows <- analysis_panel %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )

duplicate_analysis_panel_rows


# ============================================================
# Check observations by origin country
# ============================================================
#
# Purpose:
#   Document the number of observations by origin country in the final main
#   analysis panel.
#
# Interpretation:
#   This check helps identify whether one origin country has fewer
#   observations due to missing export data.
# ============================================================

analysis_panel_by_origin <- analysis_panel %>%
  count(
    origin_country,
    name = "n_obs"
  ) %>%
  arrange(
    origin_country
  )

analysis_panel_by_origin


# ============================================================
# Check observations by Land
# ============================================================
#
# Purpose:
#   Document the number of observations by German Land in the final
#   main analysis panel.
#
# Interpretation:
#   This check helps identify whether one Land has fewer
#   observations due to missing export data.
# ============================================================

analysis_panel_by_state <- analysis_panel %>%
  count(
    federal_state,
    name = "n_obs"
  ) %>%
  arrange(
    federal_state
  )

analysis_panel_by_state


# ============================================================
# Check observations by year
# ============================================================
#
# Purpose:
#   Document the number of observations by year in the final main analysis
#   panel.
#
# Interpretation:
#   This check helps identify years with fewer observations and verifies the
#   intended 2010–2025 panel coverage.
# ============================================================

analysis_panel_by_year <- analysis_panel %>%
  count(
    year,
    name = "n_obs"
  ) %>%
  arrange(
    year
  )

analysis_panel_by_year


# ============================================================
# Treatment and IV variation checks
# ============================================================
#
# Purpose:
#   Check whether the actual and predicted exposure variables contain
#   variation in the main analysis panel.
#
# Variables checked:
#   protection_seekers_stock_2016
#   predicted_protection_seekers_stock_2016
#   treatment_stock_2016_post
#   iv_stock_2016_post
#   treatment_delta_post
#   iv_delta_post
#
# Interpretation:
#   These checks confirm that the treatment and instrument variables contain
#   non-zero variation before running the empirical specifications.
#
# Notes:
#   Variables scaled by 1,000 persons are checked in the separate rescaling
#   script after they are constructed.
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
    
    min_treatment_delta_post =
      min(treatment_delta_post, na.rm = TRUE),
    max_treatment_delta_post =
      max(treatment_delta_post, na.rm = TRUE),
    
    min_iv_delta_post =
      min(iv_delta_post, na.rm = TRUE),
    max_iv_delta_post =
      max(iv_delta_post, na.rm = TRUE)
  )

treatment_iv_variation_summary


# ============================================================
# Correlation between actual and predicted treatment exposure
# ============================================================
#
# Purpose:
#   Check how strongly actual protection-seeker exposure is correlated with
#   predicted protection-seeker exposure.
#
# Variables checked:
#   protection_seekers_stock_2016
#   predicted_protection_seekers_stock_2016
#   delta_protection_seekers_2014_2016
#   predicted_delta_protection_seekers_2014_2016
#
# Logic:
#   The correlation is computed in the post-period sample and based on
#   distinct federal_state × origin_country observations.
#
#   This avoids mechanically repeating the same treatment and predicted
#   exposure values across multiple post-period years.
#
# Interpretation:
#   A positive correlation indicates that the predicted exposure measure is
#   related to actual regional protection-seeker exposure.
#
# Notes:
#   This is a descriptive diagnostic. It is not a first-stage regression and
#   does not replace the fixed-effects first-stage specification estimated
#   separately.
# ============================================================

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


# ============================================================
# Correlation across alternative IV definitions
# ============================================================
#
# Purpose:
#   Check how strongly the main predicted-exposure instruments are
#   correlated with alternative IV definitions based on different
#   Königstein allocation keys.
#
# Main IV basis:
#   koenigstein_share_2015_2016_avg
#
# Alternative IV bases:
#   koenigstein_share_2014
#   koenigstein_share_2014_2015_2016_avg
#
# Variables checked:
#   iv_stock_2016_post
#   iv_stock_2016_post_k14
#   iv_stock_2016_post_k141516
#   iv_delta_post
#   iv_delta_post_k14
#   iv_delta_post_k141516
#
# Logic:
#   Correlations are computed in the post-period sample and based on
#   distinct federal_state × origin_country observations.
#
#   This avoids mechanically repeating the same IV values across multiple
#   post-period years.
#
# Interpretation:
#   High correlations indicate that alternative Königstein-key definitions
#   generate similar predicted-exposure variation.
#
# Notes:
#   This is a descriptive robustness diagnostic. It does not replace the
#   corresponding robustness regressions using alternative IV definitions.
# ============================================================

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


# ============================================================
# Period and interaction checks
# ============================================================
#
# Purpose:
#   Check whether the period indicators and post-period interaction
#   variables are constructed correctly.
#
# Period variables:
#   pre_period
#   shock_period
#   post_period
#
# Interaction variables:
#   treatment_stock_2016_post
#   iv_stock_2016_post
#   treatment_delta_post
#   iv_delta_post
#
# Logic:
#   The treatment and IV post-period interactions should be zero in the
#   pre-period and shock period, and positive only in the post-period for
#   exposed federal_state × origin_country cells.
#
# Interpretation:
#   The distribution checks verify the timing structure of the panel.
#   The interaction check verifies that treatment and IV variation enters
#   only through the post-period interaction terms.
#
# Notes:
#   Variables scaled by 1,000 persons are checked in the separate rescaling
#   script after they are constructed.
# ============================================================

post_period_distribution <- analysis_panel %>%
  count(
    post_period,
    name = "n_obs"
  )

post_period_distribution


period_distribution <- analysis_panel %>%
  count(
    pre_period,
    shock_period,
    post_period,
    name = "n_obs"
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


# ============================================================
# Save cleaned panel data
# ============================================================
#
# Purpose:
#   Save the constructed main analysis panel and all accompanying summary
#   and diagnostic objects.
#
# Notes:
#   The saved analysis_panel is the final main panel before adding regional
#   controls and before constructing the _1000 scaled variables.
# ============================================================

saveRDS(
  analysis_panel,
  "analysis_panel.rds"
)

saveRDS(
  analysis_panel_summary,
  "analysis_panel_summary.rds"
)

saveRDS(
  duplicate_analysis_panel_rows,
  "duplicate_analysis_panel_rows.rds"
)

saveRDS(
  analysis_panel_by_origin,
  "analysis_panel_by_origin.rds"
)

saveRDS(
  analysis_panel_by_state,
  "analysis_panel_by_state.rds"
)

saveRDS(
  analysis_panel_by_year,
  "analysis_panel_by_year.rds"
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
  national_treatment_totals,
  "national_treatment_totals.rds"
)


# ============================================================
# Clean temporary objects
# ============================================================

rm(
  export_value_thousand_eur,
  protection_seekers_stock,
  koenigstein_key,
  federal_states,
  origin_countries
)


# ============================================================
# Final objects kept
# ============================================================
#
# Main analysis panel:
#   analysis_panel
#
# Main panel summary and structure checks:
#   analysis_panel_summary
#   duplicate_analysis_panel_rows
#   analysis_panel_by_origin
#   analysis_panel_by_state
#   analysis_panel_by_year
#
# Treatment and IV diagnostic objects:
#   treatment_iv_variation_summary
#   treatment_iv_correlation_summary
#   robustness_iv_correlation_summary
#
# Period and interaction checks:
#   post_period_distribution
#   period_distribution
#   interaction_period_check
#
# National exposure totals:
#   national_treatment_totals
#
# Notes:
#   analysis_panel is the final main panel before regional controls are added
#   and before treatment and IV variables are scaled by 1,000 persons.
#
#   Unit of observation:
#     federal_state × origin_country × year
#
#   Main outcome:
#     export_value
#
#   Main treatment variable:
#     treatment_stock_2016_post
#
#   Main instrument:
#     iv_stock_2016_post
#
#   Main IV basis:
#     koenigstein_share_2015_2016_avg
#
#   Active robustness variables constructed here:
#     treatment_delta_post
#     iv_delta_post
#     iv_stock_2016_post_k14
#     iv_delta_post_k14
#
#   Additional IV variables constructed for transparency:
#     iv_stock_2016_post_k141516
#     iv_delta_post_k141516
#
#   Regression-ready versions scaled by 1,000 persons are created in the
#   separate rescaling script.
#
#   Regional controls are merged later in the separate controls script.
#   CEPII gravity controls were considered separately but are not part of the
#   active final empirical strategy.
# ============================================================