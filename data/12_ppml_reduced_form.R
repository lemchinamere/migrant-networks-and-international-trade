# ============================================================
# Empirical results: PPML reduced-form regressions
# ============================================================
#
# Purpose:
#   Estimate whether Königstein-predicted protection-seeker exposure
#   predicts post-period exports from German Länder to selected
#   origin countries.
#
# Script type:
#   Regression / analysis script
#
# Workflow logic:
#   This script loads already constructed .rds panels and estimates PPML
#   reduced-form regressions.
#
#   It does not reconstruct the analysis panel, treatment variables,
#   instruments, fixed effects, regional controls, or _1000 variables from
#   raw data.
#
# Reduced-form logic:
#   The reduced form regresses the outcome directly on the instrument.
#   It therefore estimates whether predicted exposure, based on the
#   Königstein allocation key, is associated with export outcomes.
#
# Important terminology:
#   This script estimates PPML reduced-form regressions.
#
#   Reduced form:
#     Y on Z
#
#   Here:
#     Y = export_value or export_weight
#     Z = Königstein-predicted protection-seeker exposure
#
#   The estimator is PPML because the outcome is a non-negative trade-flow
#   variable. This should be distinguished from a linear reduced form, where
#   log_export_value would be regressed on the instrument using feols.
#
# Main PPML reduced-form equation:
#
#   export_value =
#     beta * iv_stock_2016_post_1000
#     + federal_state × origin_country fixed effects
#     + federal_state × year fixed effects
#     + origin_country × year fixed effects
#     + error
#
# Main outcome:
#   export_value
#
# Main reduced-form variable:
#   iv_stock_2016_post_1000
#
# Preferred estimator:
#   PPML with high-dimensional fixed effects.
#
# Interpretation:
#   A positive coefficient suggests that Königstein-predicted
#   protection-seeker exposure is associated with higher exports to the
#   corresponding origin country after the refugee shock.
#
# Unit interpretation:
#   The variable of interest is measured in thousand predicted protection
#   seekers. Therefore, the coefficient is interpreted as the reduced-form
#   association of an additional 1,000 predicted protection seekers in the
#   post period.
#
# Standard errors:
#   Clustered at the federal_state × origin_country level.
#
# Output objects:
#   ppml_reduced_form_stock_1000
#   ppml_reduced_form_delta_1000
#   ppml_reduced_form_stock_summary
#   ppml_reduced_form_delta_summary
#   ppml_reduced_form_results_overview
#   ppml_reduced_form_results_paper
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
      "Please rerun the corresponding data-cleaning / panel-construction scripts before running this PPML reduced-form script."
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
#   These panels should already contain the outcome variables, instrument
#   variables, scaled _1000 variables, and fixed-effect identifiers.
#
#   This script does not recreate any of these objects from raw data.
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
#   required-variable check and PPML reduced-form regressions.
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
#   PPML reduced-form specifications.
#
# Notes:
#   This check is run after defensive fixed-effect reconstruction so that
#   reconstructable fixed-effect identifiers are not falsely reported as
#   missing.
#
#   If key outcome or IV variables are missing, rerun the relevant
#   data-construction and rescaling scripts.
# ============================================================

required_ppml_reduced_form_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "post_period",
  
  "export_value",
  "export_weight",
  "log_export_value",
  
  "iv_stock_2016_post_1000",
  "iv_delta_post_1000",
  
  "iv_stock_2016_post_k14_1000",
  "iv_delta_post_k14_1000",
  
  "iv_stock_2016_post_k141516_1000",
  "iv_delta_post_k141516_1000",
  
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)


missing_ppml_reduced_form_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_ppml_reduced_form_variables,
    present = required_ppml_reduced_form_variables %in% names(analysis_panel)
  ),
  
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_ppml_reduced_form_variables,
    present = required_ppml_reduced_form_variables %in%
      names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_ppml_reduced_form_variables

if (nrow(missing_ppml_reduced_form_variables) > 0) {
  stop(
    "At least one required variable for the PPML reduced-form regressions is missing. Inspect missing_ppml_reduced_form_variables."
  )
}


# ============================================================
# Helper function: run PPML model safely
# ============================================================
#
# Purpose:
#   Estimate a PPML fixed-effects model while preventing the full script from
#   stopping if one robustness specification cannot be estimated.
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
# Helper function: extract PPML results safely
# ============================================================
#
# Purpose:
#   Extract the coefficient of interest and key model statistics from each
#   PPML reduced-form model.
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
  
  tibble(
    sample = sample,
    specification = specification,
    outcome_variable = outcome_variable,
    variable_of_interest = variable_of_interest,
    term = term,
    estimate = coefficient_table[term, "Estimate"],
    std_error = coefficient_table[term, "Std. Error"],
    
    z_statistic = if (!is.na(statistic_column)) {
      coefficient_table[term, statistic_column]
    } else {
      NA_real_
    },
    
    p_value = if (!is.na(p_value_column)) {
      coefficient_table[term, p_value_column]
    } else {
      NA_real_
    },
    
    pseudo_r2 = pseudo_r2_value,
    n_obs = nobs(model),
    status = "estimated"
  )
}


# ============================================================
# 1. Main PPML reduced form: stock exposure
# ============================================================
#
# Purpose:
#   Estimate the preferred PPML reduced-form specification.
#
# Outcome:
#   export_value
#
# Reduced-form variable:
#   iv_stock_2016_post_1000
#
# Fixed effects:
#   fe_state_origin + fe_state_year + fe_origin_year
# ============================================================

ppml_reduced_form_stock_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_stock_1000)) {
  summary(ppml_reduced_form_stock_1000)
}


ppml_reduced_form_stock_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_stock_1000,
  term = "iv_stock_2016_post_1000",
  specification = "PPML reduced form: main stock exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "iv_stock_2016_post_1000"
)

ppml_reduced_form_stock_summary


# ============================================================
# 2. Alternative PPML reduced form: delta exposure
# ============================================================
#
# Purpose:
#   Estimate the PPML reduced form using the alternative predicted delta
#   exposure measure.
# ============================================================

ppml_reduced_form_delta_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_delta_1000)) {
  summary(ppml_reduced_form_delta_1000)
}


ppml_reduced_form_delta_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_delta_1000,
  term = "iv_delta_post_1000",
  specification = "PPML reduced form: alternative delta exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "iv_delta_post_1000"
)

ppml_reduced_form_delta_summary


# ============================================================
# 3. PPML reduced-form robustness: 2014 Königstein key
# ============================================================
#
# Purpose:
#   Check whether the PPML reduced-form results are robust to using the
#   strictly pre-shock 2014 Königstein key.
# ============================================================

ppml_reduced_form_stock_k14_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_stock_2016_post_k14_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_stock_k14_1000)) {
  summary(ppml_reduced_form_stock_k14_1000)
}


ppml_reduced_form_stock_k14_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_stock_k14_1000,
  term = "iv_stock_2016_post_k14_1000",
  specification = "PPML reduced form: stock exposure, 2014 key",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "iv_stock_2016_post_k14_1000"
)

ppml_reduced_form_stock_k14_summary


ppml_reduced_form_delta_k14_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_delta_post_k14_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_delta_k14_1000)) {
  summary(ppml_reduced_form_delta_k14_1000)
}


ppml_reduced_form_delta_k14_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_delta_k14_1000,
  term = "iv_delta_post_k14_1000",
  specification = "PPML reduced form: delta exposure, 2014 key",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "iv_delta_post_k14_1000"
)

ppml_reduced_form_delta_k14_summary


# ============================================================
# 4. PPML reduced-form robustness: 2014–2016 three-year average
# ============================================================
#
# Purpose:
#   Check whether the PPML reduced-form results are robust to using the
#   2014–2016 average Königstein-key variant.
#
# Note:
#   As in the first-stage regressions, this alternative IV may be collinear
#   with the preferred three-way fixed-effect structure. If so, it is
#   documented as not estimable or as a dropped term.
# ============================================================

ppml_reduced_form_stock_k141516_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_stock_2016_post_k141516_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_stock_k141516_1000)) {
  summary(ppml_reduced_form_stock_k141516_1000)
}


ppml_reduced_form_stock_k141516_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_stock_k141516_1000,
  term = "iv_stock_2016_post_k141516_1000",
  specification = "PPML reduced form: stock exposure, 2014–2016 average",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "iv_stock_2016_post_k141516_1000"
)

ppml_reduced_form_stock_k141516_summary


ppml_reduced_form_delta_k141516_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_delta_post_k141516_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_delta_k141516_1000)) {
  summary(ppml_reduced_form_delta_k141516_1000)
}


ppml_reduced_form_delta_k141516_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_delta_k141516_1000,
  term = "iv_delta_post_k141516_1000",
  specification = "PPML reduced form: delta exposure, 2014–2016 average",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "iv_delta_post_k141516_1000"
)

ppml_reduced_form_delta_k141516_summary


# ============================================================
# 5. No-Eritrea PPML reduced-form robustness
# ============================================================
#
# Purpose:
#   Check whether the PPML reduced-form results are robust to excluding
#   Eritrea.
# ============================================================

ppml_reduced_form_stock_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_stock_no_eritrea_1000)) {
  summary(ppml_reduced_form_stock_no_eritrea_1000)
}


ppml_reduced_form_stock_no_eritrea_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_stock_no_eritrea_1000,
  term = "iv_stock_2016_post_1000",
  specification = "PPML reduced form: main stock exposure",
  sample = "Excluding Eritrea",
  outcome_variable = "export_value",
  variable_of_interest = "iv_stock_2016_post_1000"
)

ppml_reduced_form_stock_no_eritrea_summary


ppml_reduced_form_delta_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_delta_no_eritrea_1000)) {
  summary(ppml_reduced_form_delta_no_eritrea_1000)
}


ppml_reduced_form_delta_no_eritrea_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_delta_no_eritrea_1000,
  term = "iv_delta_post_1000",
  specification = "PPML reduced form: alternative delta exposure",
  sample = "Excluding Eritrea",
  outcome_variable = "export_value",
  variable_of_interest = "iv_delta_post_1000"
)

ppml_reduced_form_delta_no_eritrea_summary


# ============================================================
# 6. Alternative outcome: export weight
# ============================================================
#
# Purpose:
#   Estimate the PPML reduced form using export_weight as an alternative
#   trade outcome.
#
# Interpretation:
#   This is an alternative-outcome robustness check and should not be
#   emphasized over the preferred export_value specification.
# ============================================================

ppml_reduced_form_weight_stock_1000 <- run_ppml_safely(
  formula =
    export_weight ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_reduced_form_weight_stock_1000)) {
  summary(ppml_reduced_form_weight_stock_1000)
}


ppml_reduced_form_weight_stock_summary <- extract_ppml_results_safely(
  model = ppml_reduced_form_weight_stock_1000,
  term = "iv_stock_2016_post_1000",
  specification = "PPML reduced form: export weight, stock exposure",
  sample = "Full sample",
  outcome_variable = "export_weight",
  variable_of_interest = "iv_stock_2016_post_1000"
)

ppml_reduced_form_weight_stock_summary


# ============================================================
# 7. PPML reduced-form results overview
# ============================================================
#
# Purpose:
#   Combine all PPML reduced-form estimates into one overview table.
# ============================================================

ppml_reduced_form_results_overview <- bind_rows(
  ppml_reduced_form_stock_summary,
  ppml_reduced_form_delta_summary,
  ppml_reduced_form_stock_k14_summary,
  ppml_reduced_form_delta_k14_summary,
  ppml_reduced_form_stock_k141516_summary,
  ppml_reduced_form_delta_k141516_summary,
  ppml_reduced_form_stock_no_eritrea_summary,
  ppml_reduced_form_delta_no_eritrea_summary,
  ppml_reduced_form_weight_stock_summary
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

ppml_reduced_form_results_overview


# ============================================================
# 8. Paper-ready rounded PPML reduced-form table
# ============================================================
#
# Purpose:
#   Create a rounded version of the PPML reduced-form result table for easier
#   reporting and interpretation.
# ============================================================

ppml_reduced_form_results_paper <- ppml_reduced_form_results_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    z_statistic = round(z_statistic, 2),
    p_value = signif(p_value, 3),
    pseudo_r2 = round(pseudo_r2, 3),
    n_obs = as.integer(n_obs)
  )

ppml_reduced_form_results_paper


# ============================================================
# 9. Paper-ready text values
# ============================================================
#
# Purpose:
#   Store the main stock-exposure and delta-exposure PPML reduced-form
#   estimates in separate objects for easy use in the written results
#   section.
# ============================================================

main_ppml_reduced_form_coef <- ppml_reduced_form_stock_summary$estimate
main_ppml_reduced_form_se <- ppml_reduced_form_stock_summary$std_error
main_ppml_reduced_form_z <- ppml_reduced_form_stock_summary$z_statistic
main_ppml_reduced_form_p <- ppml_reduced_form_stock_summary$p_value

delta_ppml_reduced_form_coef <- ppml_reduced_form_delta_summary$estimate
delta_ppml_reduced_form_se <- ppml_reduced_form_delta_summary$std_error
delta_ppml_reduced_form_z <- ppml_reduced_form_delta_summary$z_statistic
delta_ppml_reduced_form_p <- ppml_reduced_form_delta_summary$p_value

main_ppml_reduced_form_coef
main_ppml_reduced_form_se
main_ppml_reduced_form_z
main_ppml_reduced_form_p

delta_ppml_reduced_form_coef
delta_ppml_reduced_form_se
delta_ppml_reduced_form_z
delta_ppml_reduced_form_p


# ============================================================
# 10. Save PPML reduced-form outputs
# ============================================================
#
# Purpose:
#   Save all PPML reduced-form model objects, individual summary objects,
#   combined result tables, diagnostics, and paper-ready text values.
# ============================================================

### Model objects

if (!is.null(ppml_reduced_form_stock_1000)) {
  saveRDS(
    ppml_reduced_form_stock_1000,
    "ppml_reduced_form_stock_1000.rds"
  )
}

if (!is.null(ppml_reduced_form_delta_1000)) {
  saveRDS(
    ppml_reduced_form_delta_1000,
    "ppml_reduced_form_delta_1000.rds"
  )
}

if (!is.null(ppml_reduced_form_stock_k14_1000)) {
  saveRDS(
    ppml_reduced_form_stock_k14_1000,
    "ppml_reduced_form_stock_k14_1000.rds"
  )
}

if (!is.null(ppml_reduced_form_delta_k14_1000)) {
  saveRDS(
    ppml_reduced_form_delta_k14_1000,
    "ppml_reduced_form_delta_k14_1000.rds"
  )
}

if (!is.null(ppml_reduced_form_stock_k141516_1000)) {
  saveRDS(
    ppml_reduced_form_stock_k141516_1000,
    "ppml_reduced_form_stock_k141516_1000.rds"
  )
}

if (!is.null(ppml_reduced_form_delta_k141516_1000)) {
  saveRDS(
    ppml_reduced_form_delta_k141516_1000,
    "ppml_reduced_form_delta_k141516_1000.rds"
  )
}

if (!is.null(ppml_reduced_form_stock_no_eritrea_1000)) {
  saveRDS(
    ppml_reduced_form_stock_no_eritrea_1000,
    "ppml_reduced_form_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(ppml_reduced_form_delta_no_eritrea_1000)) {
  saveRDS(
    ppml_reduced_form_delta_no_eritrea_1000,
    "ppml_reduced_form_delta_no_eritrea_1000.rds"
  )
}

if (!is.null(ppml_reduced_form_weight_stock_1000)) {
  saveRDS(
    ppml_reduced_form_weight_stock_1000,
    "ppml_reduced_form_weight_stock_1000.rds"
  )
}


### Individual summary objects

saveRDS(
  ppml_reduced_form_stock_summary,
  "ppml_reduced_form_stock_summary.rds"
)

saveRDS(
  ppml_reduced_form_delta_summary,
  "ppml_reduced_form_delta_summary.rds"
)

saveRDS(
  ppml_reduced_form_stock_k14_summary,
  "ppml_reduced_form_stock_k14_summary.rds"
)

saveRDS(
  ppml_reduced_form_delta_k14_summary,
  "ppml_reduced_form_delta_k14_summary.rds"
)

saveRDS(
  ppml_reduced_form_stock_k141516_summary,
  "ppml_reduced_form_stock_k141516_summary.rds"
)

saveRDS(
  ppml_reduced_form_delta_k141516_summary,
  "ppml_reduced_form_delta_k141516_summary.rds"
)

saveRDS(
  ppml_reduced_form_stock_no_eritrea_summary,
  "ppml_reduced_form_stock_no_eritrea_summary.rds"
)

saveRDS(
  ppml_reduced_form_delta_no_eritrea_summary,
  "ppml_reduced_form_delta_no_eritrea_summary.rds"
)

saveRDS(
  ppml_reduced_form_weight_stock_summary,
  "ppml_reduced_form_weight_stock_summary.rds"
)


### Combined outputs

saveRDS(
  ppml_reduced_form_results_overview,
  "ppml_reduced_form_results_overview.rds"
)

saveRDS(
  ppml_reduced_form_results_paper,
  "ppml_reduced_form_results_paper.rds"
)

saveRDS(
  missing_input_files,
  "ppml_reduced_form_missing_input_files.rds"
)

saveRDS(
  missing_ppml_reduced_form_variables,
  "missing_ppml_reduced_form_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_ppml_reduced_form_coef,
  "main_ppml_reduced_form_coef.rds"
)

saveRDS(
  main_ppml_reduced_form_se,
  "main_ppml_reduced_form_se.rds"
)

saveRDS(
  main_ppml_reduced_form_z,
  "main_ppml_reduced_form_z.rds"
)

saveRDS(
  main_ppml_reduced_form_p,
  "main_ppml_reduced_form_p.rds"
)

saveRDS(
  delta_ppml_reduced_form_coef,
  "delta_ppml_reduced_form_coef.rds"
)

saveRDS(
  delta_ppml_reduced_form_se,
  "delta_ppml_reduced_form_se.rds"
)

saveRDS(
  delta_ppml_reduced_form_z,
  "delta_ppml_reduced_form_z.rds"
)

saveRDS(
  delta_ppml_reduced_form_p,
  "delta_ppml_reduced_form_p.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_ppml_reduced_form_variables,
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
# Main PPML reduced-form model objects:
#   ppml_reduced_form_stock_1000
#   ppml_reduced_form_delta_1000
#
# 2014 Königstein-key robustness model objects:
#   ppml_reduced_form_stock_k14_1000
#   ppml_reduced_form_delta_k14_1000
#
# 2014–2016 average Königstein-key model objects:
#   ppml_reduced_form_stock_k141516_1000
#   ppml_reduced_form_delta_k141516_1000
#
# No-Eritrea PPML reduced-form model objects:
#   ppml_reduced_form_stock_no_eritrea_1000
#   ppml_reduced_form_delta_no_eritrea_1000
#
# Alternative outcome model object:
#   ppml_reduced_form_weight_stock_1000
#
# Individual summary objects:
#   ppml_reduced_form_stock_summary
#   ppml_reduced_form_delta_summary
#   ppml_reduced_form_stock_k14_summary
#   ppml_reduced_form_delta_k14_summary
#   ppml_reduced_form_stock_k141516_summary
#   ppml_reduced_form_delta_k141516_summary
#   ppml_reduced_form_stock_no_eritrea_summary
#   ppml_reduced_form_delta_no_eritrea_summary
#   ppml_reduced_form_weight_stock_summary
#
# Combined result tables:
#   ppml_reduced_form_results_overview
#   ppml_reduced_form_results_paper
#
# Required-variable check:
#   missing_ppml_reduced_form_variables
#
# Paper-ready text values:
#   main_ppml_reduced_form_coef
#   main_ppml_reduced_form_se
#   main_ppml_reduced_form_z
#   main_ppml_reduced_form_p
#   delta_ppml_reduced_form_coef
#   delta_ppml_reduced_form_se
#   delta_ppml_reduced_form_z
#   delta_ppml_reduced_form_p
#
# Notes:
#   This script estimates the preferred PPML reduced-form specifications.
#
#   The main reduced-form object is:
#     ppml_reduced_form_stock_1000
#
#   The main outcome is:
#     export_value
#
#   The main reduced-form variable is:
#     iv_stock_2016_post_1000
#
#   The reduced form regresses the export outcome directly on the
#   Königstein-predicted exposure measure.
#
#   In the notation Y on Z:
#     Y = export_value
#     Z = iv_stock_2016_post_1000
#
#   This is different from the non-instrumented PPML benchmark, which uses
#   actual exposure:
#     treatment_stock_2016_post_1000
#
#   The PPML reduced form is the preferred main outcome specification because
#   export flows are non-negative, may include zeros, and are standardly
#   handled with PPML in gravity-style trade applications.
#
#   The coefficient should be interpreted as the reduced-form association of
#   an additional 1,000 Königstein-predicted protection seekers in the post
#   period with exports to the corresponding origin country.
#
#   A statistically insignificant coefficient indicates that there is no
#   evidence that predicted protection-seeker exposure increased exports.
#
#   In the final write-up, refer to this section as:
#     PPML reduced-form regressions
#     or
#     preferred PPML reduced form.
#
#   Do not describe this model as:
#     OLS
#     linear IV
#     non-instrumented benchmark
#     standard 2SLS
#
#   The PPML reduced form should be interpreted together with:
#     - the first stage,
#     - the BHJ-style pre-trend diagnostic,
#     - the non-instrumented PPML benchmark,
#     - the control-function IV-style PPML robustness,
#     - and the linear reduced-form / 2SLS robustness checks.
#
#   This is a regression / analysis script. It loads existing .rds panels
#   and estimates models. It does not rebuild the analysis panel, treatment
#   variables, instruments, controls, or rescaled variables from raw data.
# ============================================================