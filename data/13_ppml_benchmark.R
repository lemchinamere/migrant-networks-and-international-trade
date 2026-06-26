# ============================================================
# Empirical results: Non-instrumented PPML benchmark
# ============================================================
#
# Purpose:
#   Estimate the conditional association between actual protection-seeker
#   exposure and exports.
#
# Script type:
#   Regression / analysis script
#
# Workflow logic:
#   This script loads already constructed .rds panels and estimates
#   non-instrumented PPML benchmark models.
#
#   It does not reconstruct outcome variables, treatment variables,
#   instruments, fixed effects, control panels, or _1000 variables from raw
#   data.
#
#   If required fixed-effect identifiers are missing, they are reconstructed
#   defensively from the already loaded panel identifiers. This is only a
#   safeguard and does not rebuild the panel from raw data.
#
# Benchmark logic:
#   This model regresses exports on actual exposure without instrumenting
#   treatment exposure.
#
# Main benchmark equation:
#
#   export_value =
#     beta * treatment_stock_2016_post_1000
#     + federal_state × origin_country fixed effects
#     + federal_state × year fixed effects
#     + origin_country × year fixed effects
#     + error
#
# Main outcome:
#   export_value
#
# Main treatment:
#   treatment_stock_2016_post_1000
#
# Preferred estimator:
#   PPML with high-dimensional fixed effects.
#
# Interpretation:
#   beta captures the conditional association between actual post-period
#   protection-seeker exposure and exports.
#
# Caveat:
#   This benchmark is not interpreted as causal because actual settlement
#   may be endogenous. It is used as a comparison to the IV-based reduced
#   form and possible control-function PPML robustness.
#
# Unit interpretation:
#   The treatment variable is measured in thousand protection seekers.
#   Therefore, the coefficient is interpreted as the association of an
#   additional 1,000 actual protection seekers in the post period.
#
# Standard errors:
#   Clustered at the federal_state × origin_country level.
#
# Output objects:
#   ppml_benchmark_stock_1000
#   ppml_benchmark_delta_1000
#   ppml_benchmark_results_overview
#   ppml_benchmark_results_paper
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
#
#   If one of these files is missing, rerun the corresponding data-cleaning,
#   panel-construction, fixed-effect, and rescaling scripts first.
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
#   variables, scaled _1000 variables, and fixed-effect identifiers.
#
#   This script does not recreate those objects from raw data.
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
#   required-variable check and PPML benchmark regressions.
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
#   non-instrumented PPML benchmark specifications.
#
# Required variable groups:
#   Panel identifiers
#   Outcome variables
#   Treatment variables
#   Fixed-effect identifiers
#
# Notes:
#   This check is run after defensive fixed-effect reconstruction so that
#   reconstructable fixed-effect identifiers are not falsely reported as
#   missing.
#
#   If key outcome or treatment variables are missing, rerun the relevant
#   data-construction and rescaling scripts.
# ============================================================

required_ppml_benchmark_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "post_period",
  
  "export_value",
  "export_weight",
  "log_export_value",
  
  "treatment_stock_2016_post_1000",
  "treatment_delta_post_1000",
  
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)


missing_ppml_benchmark_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_ppml_benchmark_variables,
    present = required_ppml_benchmark_variables %in% names(analysis_panel)
  ),
  
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_ppml_benchmark_variables,
    present = required_ppml_benchmark_variables %in%
      names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_ppml_benchmark_variables

if (nrow(missing_ppml_benchmark_variables) > 0) {
  stop(
    "At least one required variable for the PPML benchmark regressions is missing. Inspect missing_ppml_benchmark_variables."
  )
}


# ============================================================
# Helper function: run PPML model safely
# ============================================================
#
# Purpose:
#   Estimate a PPML fixed-effects model while preventing the full script from
#   stopping if one benchmark or robustness specification cannot be
#   estimated.
#
# Estimator:
#   fepois from fixest
#
# Notes:
#   If a model cannot be estimated, the function returns NULL. The summary
#   extraction function then records the model as not estimable.
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
      message("Model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: extract PPML benchmark results safely
# ============================================================
#
# Purpose:
#   Extract the coefficient of interest and key model statistics from each
#   non-instrumented PPML benchmark model.
#
# Extracted values:
#   estimate
#   standard error
#   z-statistic or t-statistic
#   p-value
#   pseudo R2
#   number of observations
#   estimation status
#
# Notes:
#   For PPML models, fixest usually reports a z-statistic and "Pr(>|z|)".
#   The function is written defensively and can also handle "t value" if
#   fixest reports that column in a specific setting.
# ============================================================

extract_ppml_results_safely <- function(
    model,
    term,
    specification,
    sample,
    outcome_variable,
    variable_of_interest
) {
  if (is.null(model)) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        outcome_variable = outcome_variable,
        variable_of_interest = variable_of_interest,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        z_statistic = NA_real_,
        p_value = NA_real_,
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
  
  pseudo_r2_value <- suppressWarnings(
    tryCatch(
      fitstat(model, "pr2")$pr2,
      error = function(e) NA_real_
    )
  )
  
  if (!(term %in% rownames(coefficient_table))) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        outcome_variable = outcome_variable,
        variable_of_interest = variable_of_interest,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        z_statistic = NA_real_,
        p_value = NA_real_,
        pseudo_r2 = pseudo_r2_value,
        n_obs = nobs(model),
        status = "term dropped"
      )
    )
  }
  
  statistic_value <- if (!is.na(statistic_column)) {
    coefficient_table[term, statistic_column]
  } else {
    NA_real_
  }
  
  p_value <- if (!is.na(p_value_column)) {
    coefficient_table[term, p_value_column]
  } else {
    NA_real_
  }
  
  tibble(
    sample = sample,
    specification = specification,
    outcome_variable = outcome_variable,
    variable_of_interest = variable_of_interest,
    term = term,
    estimate = coefficient_table[term, "Estimate"],
    std_error = coefficient_table[term, "Std. Error"],
    z_statistic = statistic_value,
    p_value = p_value,
    pseudo_r2 = pseudo_r2_value,
    n_obs = nobs(model),
    status = "estimated"
  )
}


# ============================================================
# 1. Main PPML benchmark: stock exposure
# ============================================================
#
# Outcome:
#   export_value
#
# Treatment:
#   treatment_stock_2016_post_1000
#
# Estimator:
#   PPML
#
# Fixed effects:
#   fe_state_origin + fe_state_year + fe_origin_year
#
# Interpretation:
#   This is the main non-instrumented benchmark. The coefficient captures the
#   conditional association between actual post-period stock exposure and
#   exports.
#
# Caveat:
#   This coefficient should not be interpreted causally because actual
#   settlement can be endogenous.
# ============================================================

ppml_benchmark_stock_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_benchmark_stock_1000)) {
  summary(ppml_benchmark_stock_1000)
}


ppml_benchmark_stock_summary <- extract_ppml_results_safely(
  model = ppml_benchmark_stock_1000,
  term = "treatment_stock_2016_post_1000",
  specification = "Main stock exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_stock_2016_post_1000"
)

ppml_benchmark_stock_summary


# ============================================================
# 2. Alternative PPML benchmark: delta exposure
# ============================================================
#
# Purpose:
#   Estimate an alternative non-instrumented benchmark using the actual
#   delta-exposure treatment variable.
#
# Treatment:
#   treatment_delta_post_1000
#
# Interpretation:
#   This checks whether the conditional association differs when exposure is
#   measured as the change in protection-seeker exposure rather than the 2016
#   stock.
# ============================================================

ppml_benchmark_delta_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_benchmark_delta_1000)) {
  summary(ppml_benchmark_delta_1000)
}


ppml_benchmark_delta_summary <- extract_ppml_results_safely(
  model = ppml_benchmark_delta_1000,
  term = "treatment_delta_post_1000",
  specification = "Alternative delta exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_delta_post_1000"
)

ppml_benchmark_delta_summary


# ============================================================
# 3. No-Eritrea PPML benchmark
# ============================================================
#
# Purpose:
#   Check whether the benchmark association is robust to excluding Eritrea,
#   where missing export observations are concentrated.
#
# Sample:
#   analysis_panel_no_eritrea
#
# Specifications:
#   Stock exposure benchmark
#   Delta exposure benchmark
# ============================================================

ppml_benchmark_stock_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_benchmark_stock_no_eritrea_1000)) {
  summary(ppml_benchmark_stock_no_eritrea_1000)
}


ppml_benchmark_stock_no_eritrea_summary <- extract_ppml_results_safely(
  model = ppml_benchmark_stock_no_eritrea_1000,
  term = "treatment_stock_2016_post_1000",
  specification = "Main stock exposure",
  sample = "Excluding Eritrea",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_stock_2016_post_1000"
)

ppml_benchmark_stock_no_eritrea_summary


ppml_benchmark_delta_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_benchmark_delta_no_eritrea_1000)) {
  summary(ppml_benchmark_delta_no_eritrea_1000)
}


ppml_benchmark_delta_no_eritrea_summary <- extract_ppml_results_safely(
  model = ppml_benchmark_delta_no_eritrea_1000,
  term = "treatment_delta_post_1000",
  specification = "Alternative delta exposure",
  sample = "Excluding Eritrea",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_delta_post_1000"
)

ppml_benchmark_delta_no_eritrea_summary


# ============================================================
# 4. Alternative outcome: export weight
# ============================================================
#
# Purpose:
#   Estimate the non-instrumented PPML benchmark using export_weight as an
#   alternative trade outcome.
#
# Interpretation:
#   This is an alternative-outcome robustness check and should not be
#   emphasized over the preferred export_value specification.
# ============================================================

ppml_benchmark_weight_stock_1000 <- run_ppml_safely(
  formula =
    export_weight ~ treatment_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_benchmark_weight_stock_1000)) {
  summary(ppml_benchmark_weight_stock_1000)
}


ppml_benchmark_weight_stock_summary <- extract_ppml_results_safely(
  model = ppml_benchmark_weight_stock_1000,
  term = "treatment_stock_2016_post_1000",
  specification = "Export weight, stock exposure",
  sample = "Full sample",
  outcome_variable = "export_weight",
  variable_of_interest = "treatment_stock_2016_post_1000"
)

ppml_benchmark_weight_stock_summary


# ============================================================
# 5. Combined PPML benchmark results overview
# ============================================================
#
# Purpose:
#   Combine all non-instrumented PPML benchmark estimates into one overview
#   table.
#
# Included specifications:
#   Main stock exposure benchmark
#   Delta exposure benchmark
#   No-Eritrea stock exposure benchmark
#   No-Eritrea delta exposure benchmark
#   Export-weight alternative-outcome benchmark
# ============================================================

ppml_benchmark_results_overview <- bind_rows(
  ppml_benchmark_stock_summary,
  ppml_benchmark_delta_summary,
  ppml_benchmark_stock_no_eritrea_summary,
  ppml_benchmark_delta_no_eritrea_summary,
  ppml_benchmark_weight_stock_summary
) %>%
  select(
    sample,
    specification,
    outcome_variable,
    variable_of_interest,
    term,
    estimate,
    std_error,
    z_statistic,
    p_value,
    pseudo_r2,
    n_obs,
    status
  )

ppml_benchmark_results_overview


# ============================================================
# 6. Paper-ready rounded PPML benchmark table
# ============================================================
#
# Purpose:
#   Create a rounded version of the PPML benchmark result table for easier
#   reporting and interpretation.
# ============================================================

ppml_benchmark_results_paper <- ppml_benchmark_results_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    z_statistic = round(z_statistic, 2),
    p_value = signif(p_value, 3),
    pseudo_r2 = round(pseudo_r2, 3),
    n_obs = as.integer(n_obs)
  )

ppml_benchmark_results_paper


# ============================================================
# 7. Paper-ready text values
# ============================================================
#
# Purpose:
#   Store the main stock-exposure benchmark and delta-exposure benchmark
#   estimates in separate objects for easy use in the written results
#   section.
# ============================================================

main_ppml_benchmark_coef <- ppml_benchmark_stock_summary$estimate
main_ppml_benchmark_se <- ppml_benchmark_stock_summary$std_error
main_ppml_benchmark_z <- ppml_benchmark_stock_summary$z_statistic
main_ppml_benchmark_p <- ppml_benchmark_stock_summary$p_value

delta_ppml_benchmark_coef <- ppml_benchmark_delta_summary$estimate
delta_ppml_benchmark_se <- ppml_benchmark_delta_summary$std_error
delta_ppml_benchmark_z <- ppml_benchmark_delta_summary$z_statistic
delta_ppml_benchmark_p <- ppml_benchmark_delta_summary$p_value

main_ppml_benchmark_coef
main_ppml_benchmark_se
main_ppml_benchmark_z
main_ppml_benchmark_p

delta_ppml_benchmark_coef
delta_ppml_benchmark_se
delta_ppml_benchmark_z
delta_ppml_benchmark_p


# ============================================================
# 8. Save PPML benchmark outputs
# ============================================================
#
# Purpose:
#   Save all PPML benchmark model objects, individual summary objects,
#   combined result tables, diagnostics, and paper-ready text values.
# ============================================================

### Model objects

if (!is.null(ppml_benchmark_stock_1000)) {
  saveRDS(
    ppml_benchmark_stock_1000,
    "ppml_benchmark_stock_1000.rds"
  )
}

if (!is.null(ppml_benchmark_delta_1000)) {
  saveRDS(
    ppml_benchmark_delta_1000,
    "ppml_benchmark_delta_1000.rds"
  )
}

if (!is.null(ppml_benchmark_stock_no_eritrea_1000)) {
  saveRDS(
    ppml_benchmark_stock_no_eritrea_1000,
    "ppml_benchmark_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(ppml_benchmark_delta_no_eritrea_1000)) {
  saveRDS(
    ppml_benchmark_delta_no_eritrea_1000,
    "ppml_benchmark_delta_no_eritrea_1000.rds"
  )
}

if (!is.null(ppml_benchmark_weight_stock_1000)) {
  saveRDS(
    ppml_benchmark_weight_stock_1000,
    "ppml_benchmark_weight_stock_1000.rds"
  )
}


### Individual summary objects

saveRDS(
  ppml_benchmark_stock_summary,
  "ppml_benchmark_stock_summary.rds"
)

saveRDS(
  ppml_benchmark_delta_summary,
  "ppml_benchmark_delta_summary.rds"
)

saveRDS(
  ppml_benchmark_stock_no_eritrea_summary,
  "ppml_benchmark_stock_no_eritrea_summary.rds"
)

saveRDS(
  ppml_benchmark_delta_no_eritrea_summary,
  "ppml_benchmark_delta_no_eritrea_summary.rds"
)

saveRDS(
  ppml_benchmark_weight_stock_summary,
  "ppml_benchmark_weight_stock_summary.rds"
)


### Combined outputs

saveRDS(
  ppml_benchmark_results_overview,
  "ppml_benchmark_results_overview.rds"
)

saveRDS(
  ppml_benchmark_results_paper,
  "ppml_benchmark_results_paper.rds"
)

saveRDS(
  missing_input_files,
  "ppml_benchmark_missing_input_files.rds"
)

saveRDS(
  missing_ppml_benchmark_variables,
  "missing_ppml_benchmark_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_ppml_benchmark_coef,
  "main_ppml_benchmark_coef.rds"
)

saveRDS(
  main_ppml_benchmark_se,
  "main_ppml_benchmark_se.rds"
)

saveRDS(
  main_ppml_benchmark_z,
  "main_ppml_benchmark_z.rds"
)

saveRDS(
  main_ppml_benchmark_p,
  "main_ppml_benchmark_p.rds"
)

saveRDS(
  delta_ppml_benchmark_coef,
  "delta_ppml_benchmark_coef.rds"
)

saveRDS(
  delta_ppml_benchmark_se,
  "delta_ppml_benchmark_se.rds"
)

saveRDS(
  delta_ppml_benchmark_z,
  "delta_ppml_benchmark_z.rds"
)

saveRDS(
  delta_ppml_benchmark_p,
  "delta_ppml_benchmark_p.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_ppml_benchmark_variables,
  add_fixed_effects_if_missing,
  run_ppml_safely,
  extract_ppml_results_safely
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Main PPML benchmark model objects:
#   ppml_benchmark_stock_1000
#   ppml_benchmark_delta_1000
#
# No-Eritrea PPML benchmark model objects:
#   ppml_benchmark_stock_no_eritrea_1000
#   ppml_benchmark_delta_no_eritrea_1000
#
# Alternative outcome model object:
#   ppml_benchmark_weight_stock_1000
#
# Individual summary objects:
#   ppml_benchmark_stock_summary
#   ppml_benchmark_delta_summary
#   ppml_benchmark_stock_no_eritrea_summary
#   ppml_benchmark_delta_no_eritrea_summary
#   ppml_benchmark_weight_stock_summary
#
# Combined result tables:
#   ppml_benchmark_results_overview
#   ppml_benchmark_results_paper
#
# Required-variable check:
#   missing_ppml_benchmark_variables
#
# Paper-ready text values:
#   main_ppml_benchmark_coef
#   main_ppml_benchmark_se
#   main_ppml_benchmark_z
#   main_ppml_benchmark_p
#   delta_ppml_benchmark_coef
#   delta_ppml_benchmark_se
#   delta_ppml_benchmark_z
#   delta_ppml_benchmark_p
#
# Notes:
#   This script estimates the non-instrumented PPML benchmark.
#
#   The main benchmark object is:
#     ppml_benchmark_stock_1000
#
#   The main outcome is:
#     export_value
#
#   The main treatment variable is:
#     treatment_stock_2016_post_1000
#
#   The benchmark estimates the conditional association between actual
#   post-period protection-seeker exposure and exports.
#
#   This benchmark is not interpreted as causal because actual settlement
#   may be endogenous. It is included as a comparison to the PPML reduced
#   form and the control-function IV-style PPML robustness check.
#
#   In the final write-up, refer to this section as:
#     non-instrumented PPML benchmark.
#
#   Do not describe it as:
#     OLS
#     causal estimate
#     main IV estimate
#
#   The preferred causal evidence should be based on the Königstein-predicted
#   reduced form, supported by the first stage, pre-trend diagnostics, and
#   linear IV robustness checks.
#
#   This is a regression / analysis script. It loads existing .rds panels
#   and estimates models. It does not rebuild panels, controls, treatments,
#   instruments, or rescaled variables from raw data.
# ============================================================