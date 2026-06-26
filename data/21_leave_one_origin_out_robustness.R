# ============================================================
# Leave-one-origin-out robustness
# ============================================================
#
# Purpose:
#   Estimate the preferred reduced-form PPML specification repeatedly while
#   excluding one origin country at a time.
#
# Script type:
#   Regression / robustness script
#
# Pipeline position:
#   This script should be run after:
#     01_outcome.R
#     02_treatment.R
#     03_instrument.R
#     04_analysis.R
#     05_controls.R
#     06_rescaling.R
#     07_fixed_effects.R
#     08_delta_endpoint_variables.R
#     09_data_structure.R
#     10_sources.R
#     11_first_stage_relevance.R
#     12_ppml_reduced_form.R
#     13_ppml_benchmark.R
#     14_linear_reduced_form_iv.R
#     15_control_function_iv_ppml.R
#     16_pretrend_bhj_check.R
#     17_event_study.R
#     18_regional_control_robustness.R
#     19_covid_exclusion_robustness.R
#     20_delta_endpoints_robustness.R
#
# Workflow logic:
#   This is a regression / robustness script.
#
#   It does not reconstruct raw data or rebuild the analysis panel.
#
#   It loads the already constructed final analysis panel:
#     analysis_panel.rds
#
#   and estimates the preferred PPML reduced-form specification repeatedly,
#   each time excluding one origin country.
#
# Research motivation:
#   The main sample contains five origin countries:
#     Afghanistan
#     Eritrea
#     Irak (Iraq)
#     Iran, Islamische Republik (Iran, Islamic Republic)
#     Syrien (Syria)
#
#   Since the sample is small at the origin-country dimension, it is useful
#   to check whether the preferred reduced-form estimate is driven by any
#   single origin country.
#
# Empirical interpretation:
#   The leave-one-origin-out robustness check asks whether the main result is
#   stable when one origin-country panel is removed from the estimation
#   sample.
#
# Preferred specification:
#   PPML reduced form:
#
#     export_value_it
#       = exp(beta * iv_stock_2016_post_1000
#             + FE_state_origin
#             + FE_state_year
#             + FE_origin_year)
#
#   estimated by fixest::fepois.
#
# Fixed effects:
#   fe_state_origin
#     = federal_state × origin_country
#
#   fe_state_year
#     = federal_state × year
#
#   fe_origin_year
#     = origin_country × year
#
# Standard errors:
#   Clustered at the federal_state × origin_country level.
#
# Main coefficient:
#   iv_stock_2016_post_1000
#
# Interpretation:
#   Reduced-form effect of one additional 1,000 Königstein-predicted
#   protection seekers from an origin country in a Land after the
#   refugee shock on exports from that Land to that origin country.
#
# Notes:
#   This is a robustness check, not the preferred main specification.
#
#   The preferred main specification remains the full-sample PPML
#   reduced-form model estimated in:
#     12_ppml_reduced_form.R
# ============================================================


# ============================================================
# Setup
# ============================================================

### Path

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")


### Packages

library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)
library(fixest)
library(broom)


# ============================================================
# Required input files
# ============================================================
#
# Purpose:
#   Check whether the final main analysis panel exists.
#
# Required input:
#   analysis_panel.rds
#
# Notes:
#   This script loads the existing final panel and does not rebuild it from
#   raw data.
# ============================================================

required_input_files <- c(
  "analysis_panel.rds"
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
      "Please rerun the relevant data-cleaning / panel-construction scripts before running this robustness script."
    )
  )
}


# ============================================================
# Load final analysis panel
# ============================================================
#
# Purpose:
#   Load the main final panel used for the preferred specification.
#
# Panel:
#   analysis_panel
#
# Unit of observation:
#   federal_state × origin_country × year
#
# Period:
#   2010–2025
# ============================================================

analysis_panel <- readRDS(
  "analysis_panel.rds"
)


# ============================================================
# Defensive fixed-effect reconstruction
# ============================================================
#
# Purpose:
#   Ensure that the analysis panel contains the fixed-effect identifiers
#   needed for the PPML specifications.
#
# Notes:
#   The fixed-effect identifiers should already exist after the final panel
#   construction scripts. This block reconstructs them only if missing.
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

analysis_panel <- add_fixed_effects_if_missing(
  analysis_panel
)


# ============================================================
# Required-variable check
# ============================================================
#
# Purpose:
#   Check whether all variables needed for the leave-one-origin-out
#   regressions are present in the final analysis panel.
#
# Required variables:
#   export_value
#     PPML outcome variable.
#
#   iv_stock_2016_post_1000
#     Preferred reduced-form instrument variable.
#
#   iv_delta_post_1000
#     Alternative reduced-form instrument based on the 2014–2016 delta.
#
#   treatment_stock_2016_post_1000
#     Non-instrumented PPML benchmark treatment variable.
#
#   treatment_delta_post_1000
#     Non-instrumented PPML benchmark treatment based on the 2014–2016 delta.
#
#   fe_state_origin, fe_state_year, fe_origin_year
#     Fixed-effect identifiers.
#
#   federal_state, origin_country, year
#     Panel identifiers.
# ============================================================

required_leave_one_origin_out_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "export_value",
  "iv_stock_2016_post_1000",
  "iv_delta_post_1000",
  "treatment_stock_2016_post_1000",
  "treatment_delta_post_1000",
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)

missing_leave_one_origin_out_variables <- tibble(
  variable = required_leave_one_origin_out_variables,
  present = required_leave_one_origin_out_variables %in%
    names(analysis_panel)
) %>%
  filter(
    !present
  )

missing_leave_one_origin_out_variables

if (nrow(missing_leave_one_origin_out_variables) > 0) {
  stop(
    "At least one required variable for the leave-one-origin-out robustness check is missing. Inspect missing_leave_one_origin_out_variables."
  )
}


# ============================================================
# Define origin countries for leave-one-out procedure
# ============================================================
#
# Purpose:
#   Define the origin countries that are excluded one at a time.
#
# Notes:
#   The origin list is taken directly from analysis_panel to avoid hard-coding
#   a country that is not present in the loaded panel.
# ============================================================

leave_one_origin_out_origin_list <- sort(
  unique(analysis_panel$origin_country)
)

leave_one_origin_out_origin_list


# ============================================================
# Estimation helper: safe PPML estimation
# ============================================================
#
# Purpose:
#   Estimate a PPML model and return NULL instead of stopping the entire
#   script if a specific leave-one-origin-out sample fails.
#
# Why this is useful:
#   With high-dimensional fixed effects and a small number of origin
#   countries, some leave-one-out samples can in principle lead to collinearity
#   or separation issues.
#
#   This helper allows the script to complete and documents failed models in
#   the diagnostics instead of stopping immediately.
# ============================================================

estimate_ppml_safely <- function(formula, data, cluster_formula) {
  tryCatch(
    fepois(
      formula,
      data = data,
      cluster = cluster_formula
    ),
    error = function(e) {
      NULL
    }
  )
}


# ============================================================
# Estimation helper: safe OLS first-stage estimation
# ============================================================
#
# Purpose:
#   Estimate a first-stage relevance regression for each leave-one-out
#   sample and return NULL if a model cannot be estimated.
#
# Notes:
#   These first-stage regressions are diagnostic only.
#
#   The main leave-one-origin-out robustness check is the PPML reduced-form
#   stability check.
# ============================================================

estimate_feols_safely <- function(formula, data, cluster_formula) {
  tryCatch(
    feols(
      formula,
      data = data,
      cluster = cluster_formula
    ),
    error = function(e) {
      NULL
    }
  )
}


# ============================================================
# Helper: extract coefficient from fixest model
# ============================================================
#
# Purpose:
#   Convert a fixest model into a compact coefficient summary for the
#   coefficient of interest.
#
# Output:
#   A one-row tibble containing estimate, standard error, test statistic,
#   p-value, confidence interval, and sample size.
#
# Notes:
#   If the model is NULL or the coefficient is not available, the function
#   returns a one-row tibble with missing values. This keeps result tables
#   rectangular across all leave-one-out samples.
# ============================================================

extract_fixest_coefficient <- function(model, coefficient_name) {
  if (is.null(model)) {
    return(
      tibble(
        term = coefficient_name,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        n_obs = NA_integer_,
        model_status = "not estimated"
      )
    )
  }
  
  tidy_model <- broom::tidy(
    model,
    conf.int = TRUE
  )
  
  coefficient_row <- tidy_model %>%
    filter(
      term == coefficient_name
    )
  
  if (nrow(coefficient_row) == 0) {
    return(
      tibble(
        term = coefficient_name,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        n_obs = nobs(model),
        model_status = "coefficient not available"
      )
    )
  }
  
  coefficient_row %>%
    transmute(
      term,
      estimate,
      std_error = std.error,
      statistic,
      p_value = p.value,
      conf_low = conf.low,
      conf_high = conf.high,
      n_obs = nobs(model),
      model_status = "estimated"
    )
}


# ============================================================
# Helper: extract first-stage F statistic
# ============================================================
#
# Purpose:
#   Construct an approximate first-stage relevance diagnostic for each
#   leave-one-origin-out sample.
#
# Logic:
#   For a single excluded instrument, the squared t-statistic on the
#   instrument coefficient equals the corresponding F statistic.
#
# Notes:
#   This is a diagnostic relevance check, not a formal test of instrument
#   validity.
# ============================================================

extract_first_stage_summary <- function(model, coefficient_name) {
  coefficient_summary <- extract_fixest_coefficient(
    model,
    coefficient_name
  )
  
  coefficient_summary %>%
    mutate(
      first_stage_f_statistic = statistic^2
    )
}


# ============================================================
# Estimate leave-one-origin-out models
# ============================================================
#
# Purpose:
#   Estimate the robustness models for each sample excluding one origin
#   country.
#
# Estimated specifications:
#
#   1. Preferred PPML reduced form, stock exposure:
#        export_value ~ iv_stock_2016_post_1000
#
#   2. PPML reduced form, delta exposure:
#        export_value ~ iv_delta_post_1000
#
#   3. Non-instrumented PPML benchmark, stock exposure:
#        export_value ~ treatment_stock_2016_post_1000
#
#   4. Non-instrumented PPML benchmark, delta exposure:
#        export_value ~ treatment_delta_post_1000
#
#   5. First-stage diagnostic, stock exposure:
#        treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000
#
#   6. First-stage diagnostic, delta exposure:
#        treatment_delta_post_1000 ~ iv_delta_post_1000
#
# Fixed effects in all specifications:
#   fe_state_origin + fe_state_year + fe_origin_year
#
# Clustering:
#   federal_state × origin_country
# ============================================================

leave_one_origin_out_models <- map(
  leave_one_origin_out_origin_list,
  function(excluded_origin_country) {
    
    estimation_sample <- analysis_panel %>%
      filter(
        origin_country != excluded_origin_country
      )
    
    ppml_reduced_form_stock_1000 <- estimate_ppml_safely(
      export_value ~ iv_stock_2016_post_1000 |
        fe_state_origin + fe_state_year + fe_origin_year,
      estimation_sample,
      ~fe_state_origin
    )
    
    ppml_reduced_form_delta_1000 <- estimate_ppml_safely(
      export_value ~ iv_delta_post_1000 |
        fe_state_origin + fe_state_year + fe_origin_year,
      estimation_sample,
      ~fe_state_origin
    )
    
    ppml_benchmark_stock_1000 <- estimate_ppml_safely(
      export_value ~ treatment_stock_2016_post_1000 |
        fe_state_origin + fe_state_year + fe_origin_year,
      estimation_sample,
      ~fe_state_origin
    )
    
    ppml_benchmark_delta_1000 <- estimate_ppml_safely(
      export_value ~ treatment_delta_post_1000 |
        fe_state_origin + fe_state_year + fe_origin_year,
      estimation_sample,
      ~fe_state_origin
    )
    
    first_stage_stock_1000 <- estimate_feols_safely(
      treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000 |
        fe_state_origin + fe_state_year + fe_origin_year,
      estimation_sample,
      ~fe_state_origin
    )
    
    first_stage_delta_1000 <- estimate_feols_safely(
      treatment_delta_post_1000 ~ iv_delta_post_1000 |
        fe_state_origin + fe_state_year + fe_origin_year,
      estimation_sample,
      ~fe_state_origin
    )
    
    list(
      excluded_origin_country = excluded_origin_country,
      estimation_sample = estimation_sample,
      ppml_reduced_form_stock_1000 = ppml_reduced_form_stock_1000,
      ppml_reduced_form_delta_1000 = ppml_reduced_form_delta_1000,
      ppml_benchmark_stock_1000 = ppml_benchmark_stock_1000,
      ppml_benchmark_delta_1000 = ppml_benchmark_delta_1000,
      first_stage_stock_1000 = first_stage_stock_1000,
      first_stage_delta_1000 = first_stage_delta_1000
    )
  }
)

names(leave_one_origin_out_models) <- leave_one_origin_out_origin_list


# ============================================================
# Extract leave-one-origin-out coefficient summaries
# ============================================================
#
# Purpose:
#   Convert all estimated models into compact result tables.
#
# Notes:
#   Each table has one row per excluded origin country.
# ============================================================

leave_one_origin_out_ppml_reduced_form_stock_1000 <- map_dfr(
  leave_one_origin_out_models,
  function(model_list) {
    extract_fixest_coefficient(
      model_list$ppml_reduced_form_stock_1000,
      "iv_stock_2016_post_1000"
    ) %>%
      mutate(
        excluded_origin_country = model_list$excluded_origin_country,
        model = "PPML reduced form",
        exposure_definition = "Stock exposure",
        regressor = "iv_stock_2016_post_1000"
      )
  }
) %>%
  select(
    excluded_origin_country,
    model,
    exposure_definition,
    regressor,
    everything()
  )

leave_one_origin_out_ppml_reduced_form_stock_1000


leave_one_origin_out_ppml_reduced_form_delta_1000 <- map_dfr(
  leave_one_origin_out_models,
  function(model_list) {
    extract_fixest_coefficient(
      model_list$ppml_reduced_form_delta_1000,
      "iv_delta_post_1000"
    ) %>%
      mutate(
        excluded_origin_country = model_list$excluded_origin_country,
        model = "PPML reduced form",
        exposure_definition = "Delta exposure",
        regressor = "iv_delta_post_1000"
      )
  }
) %>%
  select(
    excluded_origin_country,
    model,
    exposure_definition,
    regressor,
    everything()
  )

leave_one_origin_out_ppml_reduced_form_delta_1000


leave_one_origin_out_ppml_benchmark_stock_1000 <- map_dfr(
  leave_one_origin_out_models,
  function(model_list) {
    extract_fixest_coefficient(
      model_list$ppml_benchmark_stock_1000,
      "treatment_stock_2016_post_1000"
    ) %>%
      mutate(
        excluded_origin_country = model_list$excluded_origin_country,
        model = "PPML benchmark",
        exposure_definition = "Stock exposure",
        regressor = "treatment_stock_2016_post_1000"
      )
  }
) %>%
  select(
    excluded_origin_country,
    model,
    exposure_definition,
    regressor,
    everything()
  )

leave_one_origin_out_ppml_benchmark_stock_1000


leave_one_origin_out_ppml_benchmark_delta_1000 <- map_dfr(
  leave_one_origin_out_models,
  function(model_list) {
    extract_fixest_coefficient(
      model_list$ppml_benchmark_delta_1000,
      "treatment_delta_post_1000"
    ) %>%
      mutate(
        excluded_origin_country = model_list$excluded_origin_country,
        model = "PPML benchmark",
        exposure_definition = "Delta exposure",
        regressor = "treatment_delta_post_1000"
      )
  }
) %>%
  select(
    excluded_origin_country,
    model,
    exposure_definition,
    regressor,
    everything()
  )

leave_one_origin_out_ppml_benchmark_delta_1000


leave_one_origin_out_first_stage_stock_1000 <- map_dfr(
  leave_one_origin_out_models,
  function(model_list) {
    extract_first_stage_summary(
      model_list$first_stage_stock_1000,
      "iv_stock_2016_post_1000"
    ) %>%
      mutate(
        excluded_origin_country = model_list$excluded_origin_country,
        model = "First stage",
        exposure_definition = "Stock exposure",
        regressor = "iv_stock_2016_post_1000"
      )
  }
) %>%
  select(
    excluded_origin_country,
    model,
    exposure_definition,
    regressor,
    everything()
  )

leave_one_origin_out_first_stage_stock_1000


leave_one_origin_out_first_stage_delta_1000 <- map_dfr(
  leave_one_origin_out_models,
  function(model_list) {
    extract_first_stage_summary(
      model_list$first_stage_delta_1000,
      "iv_delta_post_1000"
    ) %>%
      mutate(
        excluded_origin_country = model_list$excluded_origin_country,
        model = "First stage",
        exposure_definition = "Delta exposure",
        regressor = "iv_delta_post_1000"
      )
  }
) %>%
  select(
    excluded_origin_country,
    model,
    exposure_definition,
    regressor,
    everything()
  )

leave_one_origin_out_first_stage_delta_1000


# ============================================================
# Combined leave-one-origin-out results
# ============================================================
#
# Purpose:
#   Combine all leave-one-origin-out result tables into one long overview.
#
# Interpretation:
#   The most important rows for the final paper are the rows where:
#
#     model == "PPML reduced form"
#     exposure_definition == "Stock exposure"
#
#   These correspond to the preferred specification re-estimated while
#   excluding one origin country at a time.
# ============================================================

leave_one_origin_out_results_overview <- bind_rows(
  leave_one_origin_out_ppml_reduced_form_stock_1000,
  leave_one_origin_out_ppml_reduced_form_delta_1000,
  leave_one_origin_out_ppml_benchmark_stock_1000,
  leave_one_origin_out_ppml_benchmark_delta_1000,
  leave_one_origin_out_first_stage_stock_1000,
  leave_one_origin_out_first_stage_delta_1000
) %>%
  arrange(
    model,
    exposure_definition,
    excluded_origin_country
  )

leave_one_origin_out_results_overview


leave_one_origin_out_results_paper <- leave_one_origin_out_ppml_reduced_form_stock_1000 %>%
  select(
    excluded_origin_country,
    estimate,
    std_error,
    p_value,
    conf_low,
    conf_high,
    n_obs,
    model_status
  ) %>%
  arrange(
    excluded_origin_country
  )

leave_one_origin_out_results_paper


# ============================================================
# Leave-one-origin-out sample diagnostics
# ============================================================
#
# Purpose:
#   Document the estimation sample used for each excluded origin country.
# ============================================================

leave_one_origin_out_sample_diagnostics <- map_dfr(
  leave_one_origin_out_models,
  function(model_list) {
    model_list$estimation_sample %>%
      summarise(
        excluded_origin_country = model_list$excluded_origin_country,
        n_obs = n(),
        n_states = n_distinct(federal_state),
        n_origins = n_distinct(origin_country),
        n_state_origin_pairs = n_distinct(fe_state_origin),
        min_year = min(year, na.rm = TRUE),
        max_year = max(year, na.rm = TRUE),
        zero_export_observations = sum(export_value == 0, na.rm = TRUE),
        positive_export_observations = sum(export_value > 0, na.rm = TRUE),
        missing_export_value = sum(is.na(export_value)),
        missing_iv_stock_2016_post_1000 = sum(is.na(iv_stock_2016_post_1000)),
        missing_iv_delta_post_1000 = sum(is.na(iv_delta_post_1000)),
        missing_treatment_stock_2016_post_1000 = sum(is.na(treatment_stock_2016_post_1000)),
        missing_treatment_delta_post_1000 = sum(is.na(treatment_delta_post_1000))
      )
  }
) %>%
  arrange(
    excluded_origin_country
  )

leave_one_origin_out_sample_diagnostics


# ============================================================
# Model status diagnostics
# ============================================================
#
# Purpose:
#   Document whether each model was successfully estimated for each
#   leave-one-origin-out sample.
# ============================================================

leave_one_origin_out_model_status <- tibble(
  excluded_origin_country = rep(
    leave_one_origin_out_origin_list,
    each = 6
  ),
  model = rep(
    c(
      "ppml_reduced_form_stock_1000",
      "ppml_reduced_form_delta_1000",
      "ppml_benchmark_stock_1000",
      "ppml_benchmark_delta_1000",
      "first_stage_stock_1000",
      "first_stage_delta_1000"
    ),
    times = length(leave_one_origin_out_origin_list)
  ),
  estimated = map_lgl(
    flatten(
      map(
        leave_one_origin_out_models,
        function(model_list) {
          list(
            model_list$ppml_reduced_form_stock_1000,
            model_list$ppml_reduced_form_delta_1000,
            model_list$ppml_benchmark_stock_1000,
            model_list$ppml_benchmark_delta_1000,
            model_list$first_stage_stock_1000,
            model_list$first_stage_delta_1000
          )
        }
      )
    ),
    ~ !is.null(.x)
  )
)

leave_one_origin_out_model_status


# ============================================================
# Main robustness interpretation summary
# ============================================================
#
# Purpose:
#   Create a compact table that summarizes whether the preferred
#   leave-one-origin-out estimates are stable in sign and magnitude.
#
# Notes:
#   This table is descriptive. The substantive interpretation should still
#   rely on the coefficient table and confidence intervals.
# ============================================================

leave_one_origin_out_main_interpretation_summary <- leave_one_origin_out_ppml_reduced_form_stock_1000 %>%
  summarise(
    n_estimated_models = sum(model_status == "estimated"),
    n_failed_models = sum(model_status != "estimated"),
    min_estimate = min(estimate, na.rm = TRUE),
    max_estimate = max(estimate, na.rm = TRUE),
    mean_estimate = mean(estimate, na.rm = TRUE),
    median_estimate = median(estimate, na.rm = TRUE),
    n_positive_estimates = sum(estimate > 0, na.rm = TRUE),
    n_negative_estimates = sum(estimate < 0, na.rm = TRUE),
    n_significant_at_5_percent = sum(p_value < 0.05, na.rm = TRUE),
    n_significant_at_10_percent = sum(p_value < 0.10, na.rm = TRUE)
  )

leave_one_origin_out_main_interpretation_summary


# ============================================================
# Save leave-one-origin-out outputs
# ============================================================

### Model objects

saveRDS(
  leave_one_origin_out_models,
  "leave_one_origin_out_models.rds"
)


### Main result tables

saveRDS(
  leave_one_origin_out_ppml_reduced_form_stock_1000,
  "leave_one_origin_out_ppml_reduced_form_stock_1000.rds"
)

saveRDS(
  leave_one_origin_out_ppml_reduced_form_delta_1000,
  "leave_one_origin_out_ppml_reduced_form_delta_1000.rds"
)

saveRDS(
  leave_one_origin_out_ppml_benchmark_stock_1000,
  "leave_one_origin_out_ppml_benchmark_stock_1000.rds"
)

saveRDS(
  leave_one_origin_out_ppml_benchmark_delta_1000,
  "leave_one_origin_out_ppml_benchmark_delta_1000.rds"
)

saveRDS(
  leave_one_origin_out_first_stage_stock_1000,
  "leave_one_origin_out_first_stage_stock_1000.rds"
)

saveRDS(
  leave_one_origin_out_first_stage_delta_1000,
  "leave_one_origin_out_first_stage_delta_1000.rds"
)


### Combined result tables

saveRDS(
  leave_one_origin_out_results_overview,
  "leave_one_origin_out_results_overview.rds"
)

saveRDS(
  leave_one_origin_out_results_paper,
  "leave_one_origin_out_results_paper.rds"
)


### Diagnostics

saveRDS(
  required_input_files,
  "leave_one_origin_out_required_input_files.rds"
)

saveRDS(
  missing_input_files,
  "leave_one_origin_out_missing_input_files.rds"
)

saveRDS(
  missing_leave_one_origin_out_variables,
  "missing_leave_one_origin_out_variables.rds"
)

saveRDS(
  leave_one_origin_out_origin_list,
  "leave_one_origin_out_origin_list.rds"
)

saveRDS(
  leave_one_origin_out_sample_diagnostics,
  "leave_one_origin_out_sample_diagnostics.rds"
)

saveRDS(
  leave_one_origin_out_model_status,
  "leave_one_origin_out_model_status.rds"
)

saveRDS(
  leave_one_origin_out_main_interpretation_summary,
  "leave_one_origin_out_main_interpretation_summary.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_leave_one_origin_out_variables,
  add_fixed_effects_if_missing,
  estimate_ppml_safely,
  estimate_feols_safely,
  extract_fixest_coefficient,
  extract_first_stage_summary
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base panel:
#   analysis_panel
#
# Origin list:
#   leave_one_origin_out_origin_list
#
# Model objects:
#   leave_one_origin_out_models
#
# Main leave-one-origin-out result tables:
#   leave_one_origin_out_ppml_reduced_form_stock_1000
#   leave_one_origin_out_ppml_reduced_form_delta_1000
#   leave_one_origin_out_ppml_benchmark_stock_1000
#   leave_one_origin_out_ppml_benchmark_delta_1000
#   leave_one_origin_out_first_stage_stock_1000
#   leave_one_origin_out_first_stage_delta_1000
#
# Combined result tables:
#   leave_one_origin_out_results_overview
#   leave_one_origin_out_results_paper
#
# Diagnostics:
#   missing_leave_one_origin_out_variables
#   leave_one_origin_out_sample_diagnostics
#   leave_one_origin_out_model_status
#   leave_one_origin_out_main_interpretation_summary
#
# Notes:
#   This script estimates the leave-one-origin-out robustness checks.
#
#   The preferred robustness table for the final paper is:
#     leave_one_origin_out_results_paper
#
#   The preferred coefficient is:
#     iv_stock_2016_post_1000
#
#   The preferred model is:
#     PPML reduced form with three-way fixed effects.
#
#   Fixed effects:
#     fe_state_origin
#     fe_state_year
#     fe_origin_year
#
#   Standard errors are clustered by:
#     fe_state_origin
#
#   This robustness check tests whether the main reduced-form result is
#   driven by any single origin country.
#
#   This is a regression / robustness script. It does not clean raw data or
#   reconstruct the analysis panel.
# ============================================================