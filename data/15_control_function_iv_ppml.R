# ============================================================
# Empirical results: Control-function IV-style PPML
# ============================================================
#
# Purpose:
#   Estimate an IV-style nonlinear PPML robustness specification using a
#   control-function / two-stage residual-inclusion approach.
#
# Important terminology:
#   This is not standard linear 2SLS and not a built-in IV-PPML estimator.
#   It is a control-function IV-style PPML robustness check.
#
# Main specification:
#
#   1. First stage:
#
#      treatment_stock_2016_post_1000 =
#        pi * iv_stock_2016_post_1000
#        + federal_state × origin_country fixed effects
#        + federal_state × year fixed effects
#        + origin_country × year fixed effects
#        + error
#
#   2. Store first-stage residual:
#
#      first_stage_residual_stock
#
#   3. PPML outcome equation:
#
#      export_value =
#        beta * treatment_stock_2016_post_1000
#        + gamma * first_stage_residual_stock
#        + federal_state × origin_country fixed effects
#        + federal_state × year fixed effects
#        + origin_country × year fixed effects
#        + error
#
# Main outcome:
#   export_value
#
# Main treatment:
#   treatment_stock_2016_post_1000
#
# Main instrument:
#   iv_stock_2016_post_1000
#
# Estimator:
#   First stage: feols
#   Outcome equation: fepois
#
# Fixed effects:
#   federal_state × origin_country
#   federal_state × year
#   origin_country × year
#
# Standard errors:
#   Clustered at federal_state × origin_country level.
#
# Interpretation:
#   beta captures the association between actual exposure and exports after
#   controlling for the first-stage residual. The residual controls for the
#   part of actual exposure that is not explained by the instrument.
#
# Caveat:
#   This is an IV-style nonlinear robustness check. It should be interpreted
#   cautiously and presented alongside the PPML reduced form, PPML benchmark,
#   and linear IV robustness.
#
# Output objects:
#   iv_ppml_stock_1000
#   iv_ppml_delta_1000
#   iv_ppml_stock_k14_1000
#   iv_ppml_stock_no_eritrea_1000
#   iv_ppml_weight_stock_1000
#   iv_ppml_results_overview
#   iv_ppml_results_paper
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
# Load required panels
# ============================================================
#
# Purpose:
#   Load the active analysis panels required for the control-function
#   IV-style PPML robustness checks.
#
# Panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Notes:
#   The full sample is used for the main stock, delta, 2014-key, and export
#   weight specifications. The no-Eritrea panel is used for the no-Eritrea
#   robustness specification.
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
#   Ensure that both analysis panels contain the fixed-effect identifiers
#   required by the empirical specifications.
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
#   Check whether the loaded analysis panels contain all variables required
#   for the control-function IV-style PPML specifications.
#
# Variables checked:
#   Panel identifiers, period indicators, outcome variables, treatment
#   variables, IV variables, alternative Königstein-key IVs, and fixed
#   effects.
#
# Interpretation:
#   Missing variables indicate that an earlier data-construction or
#   rescaling script must be rerun before estimating this robustness check.
# ============================================================

required_iv_ppml_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "post_period",
  
  "export_value",
  "export_weight",
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

missing_iv_ppml_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_iv_ppml_variables,
    present = required_iv_ppml_variables %in% names(analysis_panel)
  ),
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_iv_ppml_variables,
    present = required_iv_ppml_variables %in% names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_iv_ppml_variables


# ============================================================
# Helper function: run feols safely
# ============================================================
#
# Purpose:
#   Estimate first-stage models while preventing the full script from
#   stopping if a specification is not estimable.
#
# Notes:
#   If estimation fails, the function returns NULL and the corresponding
#   output table records the model as not estimable.
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
      message("First-stage model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: run PPML safely
# ============================================================
#
# Purpose:
#   Estimate PPML outcome equations while preventing the full script from
#   stopping if a specification is not estimable.
#
# Notes:
#   If estimation fails, the function returns NULL and the corresponding
#   output table records the model as not estimable.
# ============================================================

run_ppml_safely <- function(
    formula,
    data,
    cluster_formula
) {
  tryCatch(
    {
      fepois(
        formula,
        data = data,
        cluster = cluster_formula
      )
    },
    error = function(e) {
      message("PPML model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: extract control-function PPML results safely
# ============================================================
#
# Purpose:
#   Extract coefficient estimates, standard errors, test statistics,
#   p-values, pseudo-R2, sample size, and estimation status from
#   control-function IV-style PPML models.
#
# Logic:
#   The function separately extracts the coefficient on the actual treatment
#   variable and the coefficient on the first-stage residual.
#
# Notes:
#   If either term is dropped, the function records missing coefficient
#   values and marks the model status as "term dropped".
# ============================================================

extract_iv_ppml_results_safely <- function(
    model,
    treatment_term,
    residual_term,
    specification,
    sample,
    outcome_variable,
    treatment_variable,
    instrument_variable
) {
  if (is.null(model)) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        outcome_variable = outcome_variable,
        treatment_variable = treatment_variable,
        instrument_variable = instrument_variable,
        treatment_term = treatment_term,
        residual_term = residual_term,
        treatment_estimate = NA_real_,
        treatment_std_error = NA_real_,
        treatment_z_statistic = NA_real_,
        treatment_p_value = NA_real_,
        residual_estimate = NA_real_,
        residual_std_error = NA_real_,
        residual_z_statistic = NA_real_,
        residual_p_value = NA_real_,
        pseudo_r2 = NA_real_,
        n_obs = NA_integer_,
        status = "not estimable"
      )
    )
  }
  
  coefficient_table <- coeftable(model)
  
  statistic_column <- if ("z value" %in% colnames(coefficient_table)) {
    "z value"
  } else if ("t value" %in% colnames(coefficient_table)) {
    "t value"
  } else {
    NA_character_
  }
  
  p_value_column <- if ("Pr(>|z|)" %in% colnames(coefficient_table)) {
    "Pr(>|z|)"
  } else if ("Pr(>|t|)" %in% colnames(coefficient_table)) {
    "Pr(>|t|)"
  } else {
    NA_character_
  }
  
  treatment_available <- treatment_term %in% rownames(coefficient_table)
  residual_available <- residual_term %in% rownames(coefficient_table)
  
  tibble(
    sample = sample,
    specification = specification,
    outcome_variable = outcome_variable,
    treatment_variable = treatment_variable,
    instrument_variable = instrument_variable,
    treatment_term = treatment_term,
    residual_term = residual_term,
    
    treatment_estimate = if (treatment_available) {
      coefficient_table[treatment_term, "Estimate"]
    } else {
      NA_real_
    },
    
    treatment_std_error = if (treatment_available) {
      coefficient_table[treatment_term, "Std. Error"]
    } else {
      NA_real_
    },
    
    treatment_z_statistic = if (treatment_available && !is.na(statistic_column)) {
      coefficient_table[treatment_term, statistic_column]
    } else {
      NA_real_
    },
    
    treatment_p_value = if (treatment_available && !is.na(p_value_column)) {
      coefficient_table[treatment_term, p_value_column]
    } else {
      NA_real_
    },
    
    residual_estimate = if (residual_available) {
      coefficient_table[residual_term, "Estimate"]
    } else {
      NA_real_
    },
    
    residual_std_error = if (residual_available) {
      coefficient_table[residual_term, "Std. Error"]
    } else {
      NA_real_
    },
    
    residual_z_statistic = if (residual_available && !is.na(statistic_column)) {
      coefficient_table[residual_term, statistic_column]
    } else {
      NA_real_
    },
    
    residual_p_value = if (residual_available && !is.na(p_value_column)) {
      coefficient_table[residual_term, p_value_column]
    } else {
      NA_real_
    },
    
    pseudo_r2 = suppressWarnings(
      tryCatch(
        fitstat(model, "pr2")$pr2,
        error = function(e) NA_real_
      )
    ),
    
    n_obs = nobs(model),
    
    status = if (treatment_available & residual_available) {
      "estimated"
    } else {
      "term dropped"
    }
  )
}


# ============================================================
# Helper function: add first-stage residuals safely
# ============================================================
#
# Purpose:
#   Add residuals from a first-stage model to a panel for use in the
#   second-stage PPML outcome equation.
#
# Logic:
#   If the first-stage model is estimable, the residuals are stored under
#   the requested residual variable name.
#
#   If the first-stage model is not estimable, the residual column is filled
#   with missing values so that the script can continue and report the
#   corresponding model status.
# ============================================================

add_first_stage_residual <- function(
    data,
    first_stage_model,
    residual_name
) {
  data_out <- data
  
  if (is.null(first_stage_model)) {
    data_out[[residual_name]] <- NA_real_
  } else {
    data_out[[residual_name]] <- resid(first_stage_model)
  }
  
  data_out
}


# ============================================================
# 1. Main control-function IV-style PPML: stock exposure
# ============================================================
#
# Purpose:
#   Estimate the main control-function IV-style PPML robustness check using
#   actual stock exposure instrumented by predicted stock exposure.
#
# First stage:
#   treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000
#
# Outcome equation:
#   export_value ~ treatment_stock_2016_post_1000
#                  + first_stage_residual_stock
#
# Interpretation:
#   The coefficient on treatment_stock_2016_post_1000 is interpreted as the
#   association between actual stock exposure and exports after controlling
#   for the residual component of actual exposure not explained by the
#   instrument.
# ============================================================

iv_ppml_first_stage_stock_1000 <- run_feols_safely(
  formula =
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_first_stage_stock_1000)) {
  summary(iv_ppml_first_stage_stock_1000)
}

analysis_panel_iv_ppml_stock <- add_first_stage_residual(
  data = analysis_panel,
  first_stage_model = iv_ppml_first_stage_stock_1000,
  residual_name = "first_stage_residual_stock"
)

iv_ppml_stock_1000 <- run_ppml_safely(
  formula =
    export_value ~
    treatment_stock_2016_post_1000 + first_stage_residual_stock |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_iv_ppml_stock,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_stock_1000)) {
  summary(iv_ppml_stock_1000)
}

iv_ppml_stock_summary <- extract_iv_ppml_results_safely(
  model = iv_ppml_stock_1000,
  treatment_term = "treatment_stock_2016_post_1000",
  residual_term = "first_stage_residual_stock",
  specification = "Control-function IV-style PPML: main stock exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  treatment_variable = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_1000"
)

iv_ppml_stock_summary


# ============================================================
# 2. Alternative control-function IV-style PPML: delta exposure
# ============================================================
#
# Purpose:
#   Estimate the control-function IV-style PPML robustness check using the
#   alternative delta exposure measure instead of stock exposure.
#
# First stage:
#   treatment_delta_post_1000 ~ iv_delta_post_1000
#
# Outcome equation:
#   export_value ~ treatment_delta_post_1000
#                  + first_stage_residual_delta
#
# Interpretation:
#   This specification checks whether the IV-style PPML robustness result is
#   sensitive to using the 2014–2016 change in protection-seeker stocks
#   rather than the 2016 stock exposure.
# ============================================================

iv_ppml_first_stage_delta_1000 <- run_feols_safely(
  formula =
    treatment_delta_post_1000 ~ iv_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_first_stage_delta_1000)) {
  summary(iv_ppml_first_stage_delta_1000)
}

analysis_panel_iv_ppml_delta <- add_first_stage_residual(
  data = analysis_panel,
  first_stage_model = iv_ppml_first_stage_delta_1000,
  residual_name = "first_stage_residual_delta"
)

iv_ppml_delta_1000 <- run_ppml_safely(
  formula =
    export_value ~
    treatment_delta_post_1000 + first_stage_residual_delta |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_iv_ppml_delta,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_delta_1000)) {
  summary(iv_ppml_delta_1000)
}

iv_ppml_delta_summary <- extract_iv_ppml_results_safely(
  model = iv_ppml_delta_1000,
  treatment_term = "treatment_delta_post_1000",
  residual_term = "first_stage_residual_delta",
  specification = "Control-function IV-style PPML: alternative delta exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  treatment_variable = "treatment_delta_post_1000",
  instrument_variable = "iv_delta_post_1000"
)

iv_ppml_delta_summary


# ============================================================
# 3. Robustness: 2014 Königstein key
# ============================================================
#
# Purpose:
#   Re-estimate the control-function IV-style PPML robustness check using
#   the strictly pre-shock 2014 Königstein allocation key as the IV basis.
#
# First stage:
#   treatment_stock_2016_post_1000 ~ iv_stock_2016_post_k14_1000
#
# Outcome equation:
#   export_value ~ treatment_stock_2016_post_1000
#                  + first_stage_residual_stock_k14
#
# Interpretation:
#   This specification checks whether the IV-style PPML result depends on
#   using the cohort-relevant 2015–2016 Königstein average instead of the
#   strictly pre-shock 2014 key.
# ============================================================

iv_ppml_first_stage_stock_k14_1000 <- run_feols_safely(
  formula =
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_k14_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_first_stage_stock_k14_1000)) {
  summary(iv_ppml_first_stage_stock_k14_1000)
}

analysis_panel_iv_ppml_stock_k14 <- add_first_stage_residual(
  data = analysis_panel,
  first_stage_model = iv_ppml_first_stage_stock_k14_1000,
  residual_name = "first_stage_residual_stock_k14"
)

iv_ppml_stock_k14_1000 <- run_ppml_safely(
  formula =
    export_value ~
    treatment_stock_2016_post_1000 + first_stage_residual_stock_k14 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_iv_ppml_stock_k14,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_stock_k14_1000)) {
  summary(iv_ppml_stock_k14_1000)
}

iv_ppml_stock_k14_summary <- extract_iv_ppml_results_safely(
  model = iv_ppml_stock_k14_1000,
  treatment_term = "treatment_stock_2016_post_1000",
  residual_term = "first_stage_residual_stock_k14",
  specification = "Control-function IV-style PPML: stock exposure, 2014 key",
  sample = "Full sample",
  outcome_variable = "export_value",
  treatment_variable = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_k14_1000"
)

iv_ppml_stock_k14_summary


# ============================================================
# 4. No-Eritrea robustness
# ============================================================
#
# Purpose:
#   Re-estimate the main stock-exposure control-function IV-style PPML
#   robustness check after excluding Eritrea.
#
# First stage:
#   treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000
#
# Outcome equation:
#   export_value ~ treatment_stock_2016_post_1000
#                  + first_stage_residual_stock
#
# Interpretation:
#   This specification checks whether the IV-style PPML result is driven by
#   Eritrea, which may differ from the other origin countries in export
#   levels, reporting patterns, or migration dynamics.
# ============================================================

iv_ppml_first_stage_stock_no_eritrea_1000 <- run_feols_safely(
  formula =
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_first_stage_stock_no_eritrea_1000)) {
  summary(iv_ppml_first_stage_stock_no_eritrea_1000)
}

analysis_panel_no_eritrea_iv_ppml_stock <- add_first_stage_residual(
  data = analysis_panel_no_eritrea,
  first_stage_model = iv_ppml_first_stage_stock_no_eritrea_1000,
  residual_name = "first_stage_residual_stock"
)

iv_ppml_stock_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~
    treatment_stock_2016_post_1000 + first_stage_residual_stock |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea_iv_ppml_stock,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_stock_no_eritrea_1000)) {
  summary(iv_ppml_stock_no_eritrea_1000)
}

iv_ppml_stock_no_eritrea_summary <- extract_iv_ppml_results_safely(
  model = iv_ppml_stock_no_eritrea_1000,
  treatment_term = "treatment_stock_2016_post_1000",
  residual_term = "first_stage_residual_stock",
  specification = "Control-function IV-style PPML: main stock exposure",
  sample = "Excluding Eritrea",
  outcome_variable = "export_value",
  treatment_variable = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_1000"
)

iv_ppml_stock_no_eritrea_summary


# ============================================================
# 5. Alternative outcome: export weight
# ============================================================
#
# Purpose:
#   Re-estimate the main stock-exposure control-function IV-style PPML
#   robustness check using export weight instead of export value.
#
# Outcome equation:
#   export_weight ~ treatment_stock_2016_post_1000
#                   + first_stage_residual_stock
#
# Interpretation:
#   This specification checks whether the IV-style PPML result depends on
#   measuring exports by value rather than by physical weight.
# ============================================================

iv_ppml_weight_stock_1000 <- run_ppml_safely(
  formula =
    export_weight ~
    treatment_stock_2016_post_1000 + first_stage_residual_stock |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_iv_ppml_stock,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(iv_ppml_weight_stock_1000)) {
  summary(iv_ppml_weight_stock_1000)
}

iv_ppml_weight_stock_summary <- extract_iv_ppml_results_safely(
  model = iv_ppml_weight_stock_1000,
  treatment_term = "treatment_stock_2016_post_1000",
  residual_term = "first_stage_residual_stock",
  specification = "Control-function IV-style PPML: export weight, stock exposure",
  sample = "Full sample",
  outcome_variable = "export_weight",
  treatment_variable = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_1000"
)

iv_ppml_weight_stock_summary


# ============================================================
# 6. Combined IV-style PPML results overview
# ============================================================
#
# Purpose:
#   Combine all control-function IV-style PPML robustness results into one
#   overview table.
#
# Included specifications:
#   Main stock exposure
#   Alternative delta exposure
#   2014 Königstein-key robustness
#   No-Eritrea robustness
#   Export-weight alternative outcome
#
# Notes:
#   This table is intended for internal comparison and documentation.
# ============================================================

iv_ppml_results_overview <- bind_rows(
  iv_ppml_stock_summary,
  iv_ppml_delta_summary,
  iv_ppml_stock_k14_summary,
  iv_ppml_stock_no_eritrea_summary,
  iv_ppml_weight_stock_summary
) %>%
  select(
    sample,
    specification,
    outcome_variable,
    treatment_variable,
    instrument_variable,
    treatment_term,
    residual_term,
    treatment_estimate,
    treatment_std_error,
    treatment_z_statistic,
    treatment_p_value,
    residual_estimate,
    residual_std_error,
    residual_z_statistic,
    residual_p_value,
    pseudo_r2,
    n_obs,
    status
  )

iv_ppml_results_overview


# ============================================================
# 7. Paper-ready rounded IV-style PPML table
# ============================================================
#
# Purpose:
#   Create a rounded version of the combined IV-style PPML results table for
#   easier reporting and interpretation.
#
# Notes:
#   This table is not automatically formatted for publication but provides
#   paper-ready rounded values.
# ============================================================

iv_ppml_results_paper <- iv_ppml_results_overview %>%
  mutate(
    treatment_estimate = round(treatment_estimate, 4),
    treatment_std_error = round(treatment_std_error, 4),
    treatment_z_statistic = round(treatment_z_statistic, 2),
    treatment_p_value = signif(treatment_p_value, 3),
    
    residual_estimate = round(residual_estimate, 4),
    residual_std_error = round(residual_std_error, 4),
    residual_z_statistic = round(residual_z_statistic, 2),
    residual_p_value = signif(residual_p_value, 3),
    
    pseudo_r2 = round(pseudo_r2, 3),
    n_obs = as.integer(n_obs)
  )

iv_ppml_results_paper


# ============================================================
# 8. Paper-ready text values
# ============================================================
#
# Purpose:
#   Store the main control-function IV-style PPML coefficient values in
#   separate objects for easy use in the written results section.
#
# Main specification:
#   Control-function IV-style PPML using stock exposure and export value.
# ============================================================

main_iv_ppml_treatment_coef <-
  iv_ppml_stock_summary$treatment_estimate

main_iv_ppml_treatment_se <-
  iv_ppml_stock_summary$treatment_std_error

main_iv_ppml_treatment_z <-
  iv_ppml_stock_summary$treatment_z_statistic

main_iv_ppml_treatment_p <-
  iv_ppml_stock_summary$treatment_p_value

main_iv_ppml_residual_coef <-
  iv_ppml_stock_summary$residual_estimate

main_iv_ppml_residual_se <-
  iv_ppml_stock_summary$residual_std_error

main_iv_ppml_residual_z <-
  iv_ppml_stock_summary$residual_z_statistic

main_iv_ppml_residual_p <-
  iv_ppml_stock_summary$residual_p_value


main_iv_ppml_treatment_coef
main_iv_ppml_treatment_se
main_iv_ppml_treatment_z
main_iv_ppml_treatment_p

main_iv_ppml_residual_coef
main_iv_ppml_residual_se
main_iv_ppml_residual_z
main_iv_ppml_residual_p


# ============================================================
# 9. Save IV-style PPML outputs
# ============================================================
#
# Purpose:
#   Save all model objects, panels with first-stage residuals, summary
#   tables, diagnostics, and paper-ready text values created in this script.
#
# Notes:
#   These outputs document the control-function IV-style PPML robustness
#   check. They should be interpreted as robustness evidence, not as the main
#   causal IV estimate.
# ============================================================

### First-stage model objects

if (!is.null(iv_ppml_first_stage_stock_1000)) {
  saveRDS(
    iv_ppml_first_stage_stock_1000,
    "iv_ppml_first_stage_stock_1000.rds"
  )
}

if (!is.null(iv_ppml_first_stage_delta_1000)) {
  saveRDS(
    iv_ppml_first_stage_delta_1000,
    "iv_ppml_first_stage_delta_1000.rds"
  )
}

if (!is.null(iv_ppml_first_stage_stock_k14_1000)) {
  saveRDS(
    iv_ppml_first_stage_stock_k14_1000,
    "iv_ppml_first_stage_stock_k14_1000.rds"
  )
}

if (!is.null(iv_ppml_first_stage_stock_no_eritrea_1000)) {
  saveRDS(
    iv_ppml_first_stage_stock_no_eritrea_1000,
    "iv_ppml_first_stage_stock_no_eritrea_1000.rds"
  )
}


### PPML model objects

if (!is.null(iv_ppml_stock_1000)) {
  saveRDS(
    iv_ppml_stock_1000,
    "iv_ppml_stock_1000.rds"
  )
}

if (!is.null(iv_ppml_delta_1000)) {
  saveRDS(
    iv_ppml_delta_1000,
    "iv_ppml_delta_1000.rds"
  )
}

if (!is.null(iv_ppml_stock_k14_1000)) {
  saveRDS(
    iv_ppml_stock_k14_1000,
    "iv_ppml_stock_k14_1000.rds"
  )
}

if (!is.null(iv_ppml_stock_no_eritrea_1000)) {
  saveRDS(
    iv_ppml_stock_no_eritrea_1000,
    "iv_ppml_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(iv_ppml_weight_stock_1000)) {
  saveRDS(
    iv_ppml_weight_stock_1000,
    "iv_ppml_weight_stock_1000.rds"
  )
}


### Panels with first-stage residuals

saveRDS(
  analysis_panel_iv_ppml_stock,
  "analysis_panel_iv_ppml_stock.rds"
)

saveRDS(
  analysis_panel_iv_ppml_delta,
  "analysis_panel_iv_ppml_delta.rds"
)

saveRDS(
  analysis_panel_iv_ppml_stock_k14,
  "analysis_panel_iv_ppml_stock_k14.rds"
)

saveRDS(
  analysis_panel_no_eritrea_iv_ppml_stock,
  "analysis_panel_no_eritrea_iv_ppml_stock.rds"
)


### Individual summary objects

saveRDS(
  iv_ppml_stock_summary,
  "iv_ppml_stock_summary.rds"
)

saveRDS(
  iv_ppml_delta_summary,
  "iv_ppml_delta_summary.rds"
)

saveRDS(
  iv_ppml_stock_k14_summary,
  "iv_ppml_stock_k14_summary.rds"
)

saveRDS(
  iv_ppml_stock_no_eritrea_summary,
  "iv_ppml_stock_no_eritrea_summary.rds"
)

saveRDS(
  iv_ppml_weight_stock_summary,
  "iv_ppml_weight_stock_summary.rds"
)


### Combined outputs

saveRDS(
  iv_ppml_results_overview,
  "iv_ppml_results_overview.rds"
)

saveRDS(
  iv_ppml_results_paper,
  "iv_ppml_results_paper.rds"
)

saveRDS(
  missing_iv_ppml_variables,
  "missing_iv_ppml_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_iv_ppml_treatment_coef,
  "main_iv_ppml_treatment_coef.rds"
)

saveRDS(
  main_iv_ppml_treatment_se,
  "main_iv_ppml_treatment_se.rds"
)

saveRDS(
  main_iv_ppml_treatment_z,
  "main_iv_ppml_treatment_z.rds"
)

saveRDS(
  main_iv_ppml_treatment_p,
  "main_iv_ppml_treatment_p.rds"
)

saveRDS(
  main_iv_ppml_residual_coef,
  "main_iv_ppml_residual_coef.rds"
)

saveRDS(
  main_iv_ppml_residual_se,
  "main_iv_ppml_residual_se.rds"
)

saveRDS(
  main_iv_ppml_residual_z,
  "main_iv_ppml_residual_z.rds"
)

saveRDS(
  main_iv_ppml_residual_p,
  "main_iv_ppml_residual_p.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_iv_ppml_variables,
  add_fixed_effects_if_missing,
  run_feols_safely,
  run_ppml_safely,
  extract_iv_ppml_results_safely,
  add_first_stage_residual
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Panels with first-stage residuals:
#   analysis_panel_iv_ppml_stock
#   analysis_panel_iv_ppml_delta
#   analysis_panel_iv_ppml_stock_k14
#   analysis_panel_no_eritrea_iv_ppml_stock
#
# First-stage model objects:
#   iv_ppml_first_stage_stock_1000
#   iv_ppml_first_stage_delta_1000
#   iv_ppml_first_stage_stock_k14_1000
#   iv_ppml_first_stage_stock_no_eritrea_1000
#
# Control-function IV-style PPML model objects:
#   iv_ppml_stock_1000
#   iv_ppml_delta_1000
#   iv_ppml_stock_k14_1000
#   iv_ppml_stock_no_eritrea_1000
#   iv_ppml_weight_stock_1000
#
# Individual summary objects:
#   iv_ppml_stock_summary
#   iv_ppml_delta_summary
#   iv_ppml_stock_k14_summary
#   iv_ppml_stock_no_eritrea_summary
#   iv_ppml_weight_stock_summary
#
# Combined result tables:
#   iv_ppml_results_overview
#   iv_ppml_results_paper
#
# Required-variable check:
#   missing_iv_ppml_variables
#
# Paper-ready text values:
#   main_iv_ppml_treatment_coef
#   main_iv_ppml_treatment_se
#   main_iv_ppml_treatment_z
#   main_iv_ppml_treatment_p
#   main_iv_ppml_residual_coef
#   main_iv_ppml_residual_se
#   main_iv_ppml_residual_z
#   main_iv_ppml_residual_p
#
# Notes:
#   This script estimates a control-function IV-style PPML robustness check.
#
#   It is not standard linear 2SLS and not a built-in IV-PPML estimator.
#   Therefore, the estimates should be interpreted cautiously and presented
#   as a nonlinear robustness check rather than as the main causal IV result.
#
#   The main causal empirical evidence should rely primarily on:
#     - the first stage,
#     - the PPML reduced form,
#     - the non-instrumented PPML benchmark,
#     - the linear reduced-form and 2SLS robustness checks,
#     - and the pre-trend diagnostics.
#
#   In the final write-up, refer to these models as:
#     Control-function IV-style PPML robustness.
#
#   Do not call them:
#     standard IV-PPML
#     nonlinear 2SLS
#     main causal IV estimate
#
#   The first-stage residuals are included to control for the component of
#   actual exposure that is not explained by the instrument. However, standard
#   errors from the second-stage PPML equation do not automatically account
#   for first-stage uncertainty.
# ============================================================