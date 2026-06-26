# ============================================================
# Empirical results: Linear reduced form and linear IV / 2SLS
# ============================================================
#
# Purpose:
#   Estimate linear robustness specifications to check whether the null
#   result is specific to the preferred PPML estimator.
#
# Script type:
#   Regression / analysis script
#
# Workflow logic:
#   This script loads already constructed .rds panels and estimates linear
#   reduced-form and linear IV / 2SLS robustness models.
#
#   It does not reconstruct treatment variables, instruments, fixed effects,
#   or control panels from raw data.
#
#   If required fixed-effect identifiers are missing, they are reconstructed
#   defensively from the already loaded panel identifiers. This is only a
#   safeguard and does not rebuild the panel from raw data.
#
# Main logic:
#
#   1. Linear reduced form:
#
#      log_export_value =
#        beta * iv_stock_2016_post_1000
#        + federal_state × origin_country fixed effects
#        + federal_state × year fixed effects
#        + origin_country × year fixed effects
#        + error
#
#   2. Linear IV / 2SLS:
#
#      log_export_value =
#        beta * treatment_stock_2016_post_1000
#        + federal_state × origin_country fixed effects
#        + federal_state × year fixed effects
#        + origin_country × year fixed effects
#        + error
#
#      where treatment_stock_2016_post_1000 is instrumented by
#      iv_stock_2016_post_1000.
#
# Main outcome:
#   log_export_value
#
# Main endogenous treatment:
#   treatment_stock_2016_post_1000
#
# Main instrument:
#   iv_stock_2016_post_1000
#
# Estimator:
#   Linear fixed-effects reduced form using feols.
#   Linear IV / 2SLS using feols IV syntax.
#
# Interpretation:
#   These models are robustness checks. They are not the preferred trade-flow
#   specification, because the main export outcome is non-negative and better
#   handled by PPML. Their purpose is to show whether the null result also
#   appears under familiar linear reduced-form and IV specifications.
#
# Unit interpretation:
#   Treatment and instrument variables are measured in thousand persons.
#   Coefficients therefore correspond to the association/effect of an
#   additional 1,000 actual or predicted protection seekers.
#
# Standard errors:
#   Clustered at the federal_state × origin_country level.
#
# Output objects:
#   linear_reduced_form_stock_1000
#   linear_reduced_form_delta_1000
#   linear_iv_stock_1000
#   linear_iv_delta_1000
#   linear_iv_results_overview
#   linear_iv_results_paper
# ============================================================


# ============================================================
# Setup
# ============================================================

### Path

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")


### Packages

library(dplyr)
library(fixest)
library(tibble)


# ============================================================
# Required input files
# ============================================================
#
# Purpose:
#   Define all saved .rds panels required by this regression script.
#
# Required panels:
#   analysis_panel.rds
#   analysis_panel_no_eritrea.rds
#
# Notes:
#   This is a regression / analysis script. Therefore, it loads existing .rds
#   panels directly and does not rebuild them from raw data.
# ============================================================

required_input_files <- c(
  "analysis_panel.rds",
  "analysis_panel_no_eritrea.rds"
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
      "Please rerun the corresponding data-cleaning / panel-construction scripts before running this regression script."
    )
  )
}


# ============================================================
# Load required panels
# ============================================================
#
# Purpose:
#   Load the already constructed analysis panels from disk.
#
# Panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Notes:
#   These panels should already contain the outcome variables, treatment
#   variables, instrument variables, and scaled _1000 variables.
#
#   This script does not recreate any of these objects.
# ============================================================

analysis_panel <- readRDS(
  "analysis_panel.rds"
)

analysis_panel_no_eritrea <- readRDS(
  "analysis_panel_no_eritrea.rds"
)


# ============================================================
# Defensive fixed-effect reconstruction
# ============================================================
#
# Purpose:
#   Ensure that fixed-effect identifiers exist before running the
#   required-variable check and regression models.
#
# Fixed effects:
#   fe_state_origin = federal_state × origin_country
#   fe_state_year   = federal_state × year
#   fe_origin_year  = origin_country × year
#
# Notes:
#   Fixed effects should already exist in the saved panels.
#
#   This block only reconstructs them if missing. It is a defensive safeguard
#   and does not rebuild the underlying analysis panel from raw data.
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

analysis_panel_no_eritrea <- add_fixed_effects_if_missing(
  analysis_panel_no_eritrea
)


# ============================================================
# Required-variable check
# ============================================================
#
# Purpose:
#   Check whether the loaded panels contain all variables required for the
#   linear reduced-form and linear IV / 2SLS robustness specifications.
#
# Notes:
#   This check is run after defensive fixed-effect reconstruction so that
#   reconstructable fixed-effect identifiers are not falsely reported as
#   missing.
#
#   If key outcome, treatment, or IV variables are missing, rerun the
#   relevant data-construction and rescaling scripts.
# ============================================================

required_linear_iv_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "post_period",
  
  "export_value",
  "log_export_value",
  
  "treatment_stock_2016_post_1000",
  "treatment_delta_post_1000",
  
  "iv_stock_2016_post_1000",
  "iv_delta_post_1000",
  
  "iv_stock_2016_post_k14_1000",
  "iv_delta_post_k14_1000",
  
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)

missing_linear_iv_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_linear_iv_variables,
    present = required_linear_iv_variables %in% names(analysis_panel)
  ),
  
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_linear_iv_variables,
    present = required_linear_iv_variables %in%
      names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_linear_iv_variables

if (nrow(missing_linear_iv_variables) > 0) {
  stop(
    "At least one required variable for the linear IV regressions is missing. Inspect missing_linear_iv_variables."
  )
}


# ============================================================
# Helper function: run feols safely
# ============================================================
#
# Purpose:
#   Estimate a fixest model while preventing the full script from stopping
#   if one robustness specification cannot be estimated.
#
# Notes:
#   If a model cannot be estimated, the function returns NULL. The summary
#   extraction function then records the model as not estimable.
# ============================================================

run_feols_safely <- function(
    formula,
    data,
    cluster_formula
) {
  tryCatch(
    {
      feols(
        formula,
        data = data,
        cluster = cluster_formula
      )
    },
    error = function(e) {
      message("Model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: extract coefficient from feols model safely
# ============================================================
#
# Purpose:
#   Extract the coefficient of interest and key model statistics from each
#   linear reduced-form or linear IV / 2SLS model.
#
# Extracted values:
#   estimate
#   standard error
#   t-statistic or z-statistic
#   p-value
#   within R2
#   number of observations
#   estimation status
#
# Notes:
#   In fixest IV models, the estimated endogenous regressor usually appears
#   as:
#
#     fit_treatment_stock_2016_post_1000
#
#   or:
#
#     fit_treatment_delta_post_1000
# ============================================================

extract_linear_results_safely <- function(
    model,
    term,
    specification,
    sample,
    model_type,
    outcome_variable,
    variable_of_interest,
    instrument_variable = NA_character_
) {
  if (is.null(model)) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        model_type = model_type,
        outcome_variable = outcome_variable,
        variable_of_interest = variable_of_interest,
        instrument_variable = instrument_variable,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        within_r2 = NA_real_,
        n_obs = NA_integer_,
        status = "not estimable"
      )
    )
  }
  
  coefficient_table <- coeftable(model)
  
  statistic_column <- if ("t value" %in% colnames(coefficient_table)) {
    "t value"
  } else if ("z value" %in% colnames(coefficient_table)) {
    "z value"
  } else {
    NA_character_
  }
  
  p_value_column <- if ("Pr(>|t|)" %in% colnames(coefficient_table)) {
    "Pr(>|t|)"
  } else if ("Pr(>|z|)" %in% colnames(coefficient_table)) {
    "Pr(>|z|)"
  } else {
    NA_character_
  }
  
  within_r2_value <- suppressWarnings(
    tryCatch(
      fitstat(model, "wr2")$wr2,
      error = function(e) NA_real_
    )
  )
  
  if (!(term %in% rownames(coefficient_table))) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        model_type = model_type,
        outcome_variable = outcome_variable,
        variable_of_interest = variable_of_interest,
        instrument_variable = instrument_variable,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        within_r2 = within_r2_value,
        n_obs = nobs(model),
        status = "term dropped"
      )
    )
  }
  
  tibble(
    sample = sample,
    specification = specification,
    model_type = model_type,
    outcome_variable = outcome_variable,
    variable_of_interest = variable_of_interest,
    instrument_variable = instrument_variable,
    term = term,
    estimate = coefficient_table[term, "Estimate"],
    std_error = coefficient_table[term, "Std. Error"],
    t_statistic = if (!is.na(statistic_column)) {
      coefficient_table[term, statistic_column]
    } else {
      NA_real_
    },
    p_value = if (!is.na(p_value_column)) {
      coefficient_table[term, p_value_column]
    } else {
      NA_real_
    },
    within_r2 = within_r2_value,
    n_obs = nobs(model),
    status = "estimated"
  )
}


# ============================================================
# 1. Main linear reduced form: stock exposure
# ============================================================
#
# Purpose:
#   Estimate the main linear reduced-form robustness specification.
#
# Interpretation:
#   The coefficient on iv_stock_2016_post_1000 captures whether predicted
#   exposure is associated with log exports after absorbing the three-way
#   fixed-effect structure.
# ============================================================

linear_reduced_form_stock_1000 <- run_feols_safely(
  formula =
    log_export_value ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_reduced_form_stock_1000)) {
  summary(linear_reduced_form_stock_1000)
}

linear_reduced_form_stock_summary <- extract_linear_results_safely(
  model = linear_reduced_form_stock_1000,
  term = "iv_stock_2016_post_1000",
  specification = "Linear reduced form: main stock exposure",
  sample = "Full sample",
  model_type = "Linear reduced form",
  outcome_variable = "log_export_value",
  variable_of_interest = "iv_stock_2016_post_1000",
  instrument_variable = NA_character_
)

linear_reduced_form_stock_summary


# ============================================================
# 2. Main linear IV / 2SLS: stock exposure
# ============================================================
#
# Purpose:
#   Estimate the main linear IV / 2SLS robustness specification.
#
# Endogenous variable:
#   treatment_stock_2016_post_1000
#
# Instrument:
#   iv_stock_2016_post_1000
#
# Interpretation:
#   This specification estimates the effect of actual protection-seeker stock
#   exposure on log exports, using Königstein-predicted exposure as the
#   instrument.
# ============================================================

linear_iv_stock_1000 <- run_feols_safely(
  formula =
    log_export_value ~ 1 |
    fe_state_origin + fe_state_year + fe_origin_year |
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_iv_stock_1000)) {
  summary(linear_iv_stock_1000)
}

linear_iv_stock_summary <- extract_linear_results_safely(
  model = linear_iv_stock_1000,
  term = "fit_treatment_stock_2016_post_1000",
  specification = "Linear IV / 2SLS: main stock exposure",
  sample = "Full sample",
  model_type = "Linear IV / 2SLS",
  outcome_variable = "log_export_value",
  variable_of_interest = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_1000"
)

linear_iv_stock_summary


# ============================================================
# 3. Alternative linear reduced form: delta exposure
# ============================================================
#
# Purpose:
#   Estimate an alternative linear reduced-form specification based on
#   predicted delta exposure.
# ============================================================

linear_reduced_form_delta_1000 <- run_feols_safely(
  formula =
    log_export_value ~ iv_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_reduced_form_delta_1000)) {
  summary(linear_reduced_form_delta_1000)
}

linear_reduced_form_delta_summary <- extract_linear_results_safely(
  model = linear_reduced_form_delta_1000,
  term = "iv_delta_post_1000",
  specification = "Linear reduced form: alternative delta exposure",
  sample = "Full sample",
  model_type = "Linear reduced form",
  outcome_variable = "log_export_value",
  variable_of_interest = "iv_delta_post_1000",
  instrument_variable = NA_character_
)

linear_reduced_form_delta_summary


# ============================================================
# 4. Alternative linear IV / 2SLS: delta exposure
# ============================================================
#
# Purpose:
#   Estimate an alternative linear IV / 2SLS specification based on actual
#   and predicted delta exposure.
# ============================================================

linear_iv_delta_1000 <- run_feols_safely(
  formula =
    log_export_value ~ 1 |
    fe_state_origin + fe_state_year + fe_origin_year |
    treatment_delta_post_1000 ~ iv_delta_post_1000,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_iv_delta_1000)) {
  summary(linear_iv_delta_1000)
}

linear_iv_delta_summary <- extract_linear_results_safely(
  model = linear_iv_delta_1000,
  term = "fit_treatment_delta_post_1000",
  specification = "Linear IV / 2SLS: alternative delta exposure",
  sample = "Full sample",
  model_type = "Linear IV / 2SLS",
  outcome_variable = "log_export_value",
  variable_of_interest = "treatment_delta_post_1000",
  instrument_variable = "iv_delta_post_1000"
)

linear_iv_delta_summary


# ============================================================
# 5. Robustness: 2014 Königstein key
# ============================================================
#
# Purpose:
#   Check whether the linear reduced-form and IV results are robust to using
#   the strictly pre-shock 2014 Königstein key.
# ============================================================

linear_reduced_form_stock_k14_1000 <- run_feols_safely(
  formula =
    log_export_value ~ iv_stock_2016_post_k14_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_reduced_form_stock_k14_1000)) {
  summary(linear_reduced_form_stock_k14_1000)
}

linear_reduced_form_stock_k14_summary <- extract_linear_results_safely(
  model = linear_reduced_form_stock_k14_1000,
  term = "iv_stock_2016_post_k14_1000",
  specification = "Linear reduced form: stock exposure, 2014 key",
  sample = "Full sample",
  model_type = "Linear reduced form",
  outcome_variable = "log_export_value",
  variable_of_interest = "iv_stock_2016_post_k14_1000",
  instrument_variable = NA_character_
)

linear_reduced_form_stock_k14_summary


linear_iv_stock_k14_1000 <- run_feols_safely(
  formula =
    log_export_value ~ 1 |
    fe_state_origin + fe_state_year + fe_origin_year |
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_k14_1000,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_iv_stock_k14_1000)) {
  summary(linear_iv_stock_k14_1000)
}

linear_iv_stock_k14_summary <- extract_linear_results_safely(
  model = linear_iv_stock_k14_1000,
  term = "fit_treatment_stock_2016_post_1000",
  specification = "Linear IV / 2SLS: stock exposure, 2014 key",
  sample = "Full sample",
  model_type = "Linear IV / 2SLS",
  outcome_variable = "log_export_value",
  variable_of_interest = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_k14_1000"
)

linear_iv_stock_k14_summary


# ============================================================
# 6. No-Eritrea robustness
# ============================================================
#
# Purpose:
#   Check whether the main linear reduced-form and IV results are robust to
#   excluding Eritrea.
# ============================================================

linear_reduced_form_stock_no_eritrea_1000 <- run_feols_safely(
  formula =
    log_export_value ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_reduced_form_stock_no_eritrea_1000)) {
  summary(linear_reduced_form_stock_no_eritrea_1000)
}

linear_reduced_form_stock_no_eritrea_summary <- extract_linear_results_safely(
  model = linear_reduced_form_stock_no_eritrea_1000,
  term = "iv_stock_2016_post_1000",
  specification = "Linear reduced form: main stock exposure",
  sample = "Excluding Eritrea",
  model_type = "Linear reduced form",
  outcome_variable = "log_export_value",
  variable_of_interest = "iv_stock_2016_post_1000",
  instrument_variable = NA_character_
)

linear_reduced_form_stock_no_eritrea_summary


linear_iv_stock_no_eritrea_1000 <- run_feols_safely(
  formula =
    log_export_value ~ 1 |
    fe_state_origin + fe_state_year + fe_origin_year |
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_iv_stock_no_eritrea_1000)) {
  summary(linear_iv_stock_no_eritrea_1000)
}

linear_iv_stock_no_eritrea_summary <- extract_linear_results_safely(
  model = linear_iv_stock_no_eritrea_1000,
  term = "fit_treatment_stock_2016_post_1000",
  specification = "Linear IV / 2SLS: main stock exposure",
  sample = "Excluding Eritrea",
  model_type = "Linear IV / 2SLS",
  outcome_variable = "log_export_value",
  variable_of_interest = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_1000"
)

linear_iv_stock_no_eritrea_summary


# ============================================================
# 7. Alternative linear outcome: export value in levels
# ============================================================
#
# Purpose:
#   Estimate an additional linear IV specification using export_value in
#   levels instead of log_export_value.
#
# Interpretation:
#   This is only a supplementary robustness check. It should not be
#   emphasized over the log-outcome specifications or the preferred PPML
#   reduced-form model.
# ============================================================

linear_iv_export_value_stock_1000 <- run_feols_safely(
  formula =
    export_value ~ 1 |
    fe_state_origin + fe_state_year + fe_origin_year |
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_iv_export_value_stock_1000)) {
  summary(linear_iv_export_value_stock_1000)
}

linear_iv_export_value_stock_summary <- extract_linear_results_safely(
  model = linear_iv_export_value_stock_1000,
  term = "fit_treatment_stock_2016_post_1000",
  specification = "Linear IV / 2SLS: export value in levels",
  sample = "Full sample",
  model_type = "Linear IV / 2SLS",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_1000"
)

linear_iv_export_value_stock_summary


# ============================================================
# 8. Combined linear IV robustness results overview
# ============================================================
#
# Purpose:
#   Combine all linear reduced-form and linear IV / 2SLS robustness results
#   into one overview table.
# ============================================================

linear_iv_results_overview <- bind_rows(
  linear_reduced_form_stock_summary,
  linear_iv_stock_summary,
  linear_reduced_form_delta_summary,
  linear_iv_delta_summary,
  linear_reduced_form_stock_k14_summary,
  linear_iv_stock_k14_summary,
  linear_reduced_form_stock_no_eritrea_summary,
  linear_iv_stock_no_eritrea_summary,
  linear_iv_export_value_stock_summary
) %>%
  select(
    sample,
    specification,
    model_type,
    outcome_variable,
    variable_of_interest,
    instrument_variable,
    term,
    estimate,
    std_error,
    t_statistic,
    p_value,
    within_r2,
    n_obs,
    status
  )

linear_iv_results_overview


# ============================================================
# 9. Paper-ready rounded linear IV robustness table
# ============================================================
#
# Purpose:
#   Create a rounded version of the combined results table for easier
#   reporting in the paper.
# ============================================================

linear_iv_results_paper <- linear_iv_results_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    t_statistic = round(t_statistic, 2),
    p_value = signif(p_value, 3),
    within_r2 = round(within_r2, 3),
    n_obs = as.integer(n_obs)
  )

linear_iv_results_paper


# ============================================================
# 10. Paper-ready text values
# ============================================================
#
# Purpose:
#   Store the main linear reduced-form and main linear IV estimates in
#   separate objects for easy use in the written results section.
# ============================================================

main_linear_reduced_form_coef <-
  linear_reduced_form_stock_summary$estimate

main_linear_reduced_form_se <-
  linear_reduced_form_stock_summary$std_error

main_linear_reduced_form_t <-
  linear_reduced_form_stock_summary$t_statistic

main_linear_reduced_form_p <-
  linear_reduced_form_stock_summary$p_value

main_linear_iv_coef <-
  linear_iv_stock_summary$estimate

main_linear_iv_se <-
  linear_iv_stock_summary$std_error

main_linear_iv_t <-
  linear_iv_stock_summary$t_statistic

main_linear_iv_p <-
  linear_iv_stock_summary$p_value

main_linear_reduced_form_coef
main_linear_reduced_form_se
main_linear_reduced_form_t
main_linear_reduced_form_p

main_linear_iv_coef
main_linear_iv_se
main_linear_iv_t
main_linear_iv_p


# ============================================================
# 11. Save linear reduced-form and 2SLS outputs
# ============================================================
#
# Purpose:
#   Save all model objects, individual summary objects, combined result
#   tables, diagnostics, and paper-ready text values.
# ============================================================

### Model objects

if (!is.null(linear_reduced_form_stock_1000)) {
  saveRDS(
    linear_reduced_form_stock_1000,
    "linear_reduced_form_stock_1000.rds"
  )
}

if (!is.null(linear_reduced_form_delta_1000)) {
  saveRDS(
    linear_reduced_form_delta_1000,
    "linear_reduced_form_delta_1000.rds"
  )
}

if (!is.null(linear_reduced_form_stock_k14_1000)) {
  saveRDS(
    linear_reduced_form_stock_k14_1000,
    "linear_reduced_form_stock_k14_1000.rds"
  )
}

if (!is.null(linear_reduced_form_stock_no_eritrea_1000)) {
  saveRDS(
    linear_reduced_form_stock_no_eritrea_1000,
    "linear_reduced_form_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(linear_iv_stock_1000)) {
  saveRDS(
    linear_iv_stock_1000,
    "linear_iv_stock_1000.rds"
  )
}

if (!is.null(linear_iv_delta_1000)) {
  saveRDS(
    linear_iv_delta_1000,
    "linear_iv_delta_1000.rds"
  )
}

if (!is.null(linear_iv_stock_k14_1000)) {
  saveRDS(
    linear_iv_stock_k14_1000,
    "linear_iv_stock_k14_1000.rds"
  )
}

if (!is.null(linear_iv_stock_no_eritrea_1000)) {
  saveRDS(
    linear_iv_stock_no_eritrea_1000,
    "linear_iv_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(linear_iv_export_value_stock_1000)) {
  saveRDS(
    linear_iv_export_value_stock_1000,
    "linear_iv_export_value_stock_1000.rds"
  )
}


### Individual summary objects

saveRDS(
  linear_reduced_form_stock_summary,
  "linear_reduced_form_stock_summary.rds"
)

saveRDS(
  linear_iv_stock_summary,
  "linear_iv_stock_summary.rds"
)

saveRDS(
  linear_reduced_form_delta_summary,
  "linear_reduced_form_delta_summary.rds"
)

saveRDS(
  linear_iv_delta_summary,
  "linear_iv_delta_summary.rds"
)

saveRDS(
  linear_reduced_form_stock_k14_summary,
  "linear_reduced_form_stock_k14_summary.rds"
)

saveRDS(
  linear_iv_stock_k14_summary,
  "linear_iv_stock_k14_summary.rds"
)

saveRDS(
  linear_reduced_form_stock_no_eritrea_summary,
  "linear_reduced_form_stock_no_eritrea_summary.rds"
)

saveRDS(
  linear_iv_stock_no_eritrea_summary,
  "linear_iv_stock_no_eritrea_summary.rds"
)

saveRDS(
  linear_iv_export_value_stock_summary,
  "linear_iv_export_value_stock_summary.rds"
)


### Combined outputs

saveRDS(
  linear_iv_results_overview,
  "linear_iv_results_overview.rds"
)

saveRDS(
  linear_iv_results_paper,
  "linear_iv_results_paper.rds"
)

saveRDS(
  missing_input_files,
  "linear_iv_missing_input_files.rds"
)

saveRDS(
  missing_linear_iv_variables,
  "missing_linear_iv_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_linear_reduced_form_coef,
  "main_linear_reduced_form_coef.rds"
)

saveRDS(
  main_linear_reduced_form_se,
  "main_linear_reduced_form_se.rds"
)

saveRDS(
  main_linear_reduced_form_t,
  "main_linear_reduced_form_t.rds"
)

saveRDS(
  main_linear_reduced_form_p,
  "main_linear_reduced_form_p.rds"
)

saveRDS(
  main_linear_iv_coef,
  "main_linear_iv_coef.rds"
)

saveRDS(
  main_linear_iv_se,
  "main_linear_iv_se.rds"
)

saveRDS(
  main_linear_iv_t,
  "main_linear_iv_t.rds"
)

saveRDS(
  main_linear_iv_p,
  "main_linear_iv_p.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_linear_iv_variables,
  add_fixed_effects_if_missing,
  run_feols_safely,
  extract_linear_results_safely
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Main linear reduced-form model objects:
#   linear_reduced_form_stock_1000
#   linear_reduced_form_delta_1000
#
# Main linear IV / 2SLS model objects:
#   linear_iv_stock_1000
#   linear_iv_delta_1000
#
# 2014 Königstein-key robustness model objects:
#   linear_reduced_form_stock_k14_1000
#   linear_iv_stock_k14_1000
#
# No-Eritrea robustness model objects:
#   linear_reduced_form_stock_no_eritrea_1000
#   linear_iv_stock_no_eritrea_1000
#
# Alternative linear outcome model object:
#   linear_iv_export_value_stock_1000
#
# Individual summary objects:
#   linear_reduced_form_stock_summary
#   linear_iv_stock_summary
#   linear_reduced_form_delta_summary
#   linear_iv_delta_summary
#   linear_reduced_form_stock_k14_summary
#   linear_iv_stock_k14_summary
#   linear_reduced_form_stock_no_eritrea_summary
#   linear_iv_stock_no_eritrea_summary
#   linear_iv_export_value_stock_summary
#
# Combined result tables:
#   linear_iv_results_overview
#   linear_iv_results_paper
#
# Required-variable check:
#   missing_linear_iv_variables
#
# Paper-ready text values:
#   main_linear_reduced_form_coef
#   main_linear_reduced_form_se
#   main_linear_reduced_form_t
#   main_linear_reduced_form_p
#   main_linear_iv_coef
#   main_linear_iv_se
#   main_linear_iv_t
#   main_linear_iv_p
#
# Notes:
#   This script estimates linear reduced-form and linear IV / 2SLS
#   robustness specifications.
#
#   The preferred empirical model remains the PPML reduced form, because
#   export flows are non-negative, can include zeros, and are standardly
#   handled with PPML in gravity-style trade applications.
#
#   The linear reduced-form and 2SLS models are included to show that the
#   null result is not specific to the nonlinear PPML estimator.
#
#   Main linear reduced-form model:
#     linear_reduced_form_stock_1000
#
#   Main linear IV / 2SLS model:
#     linear_iv_stock_1000
#
#   Main outcome in the linear specifications:
#     log_export_value
#
#   Main endogenous treatment:
#     treatment_stock_2016_post_1000
#
#   Main excluded instrument:
#     iv_stock_2016_post_1000
#
#   The export-value-in-levels IV model is only an additional robustness check
#   and should not be emphasized over the log-outcome specifications.
#
#   In the final write-up, describe this section as:
#     linear reduced-form and 2SLS robustness checks.
#
#   Do not describe the linear 2SLS estimates as the preferred trade-flow
#   specification.
#
#   This is a regression / analysis script. It loads existing .rds panels
#   and estimates models. It does not rebuild panels, controls, treatments,
#   instruments, or rescaled variables from raw data.
# ============================================================