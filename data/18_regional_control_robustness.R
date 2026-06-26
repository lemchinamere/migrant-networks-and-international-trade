# ============================================================
# Empirical results: Regional-control robustness
# ============================================================
#
# Purpose:
#   Check whether the null result depends on the saturated
#   federal_state × year fixed effects.
#
# Script type:
#   Regression / analysis script
#
# Workflow logic:
#   This script loads already constructed .rds panels and estimates
#   regional-control robustness regressions.
#
#   It does not reconstruct the analysis panel, regional-control panels,
#   treatment variables, instruments, fixed effects, or _1000 variables from
#   raw data.
#
# Main idea:
#   The preferred specification controls for federal_state × year fixed
#   effects. These absorb all state-year variation, including regional GDP,
#   population, unemployment, employment, manufacturing structure, and total
#   export capacity.
#
#   This robustness check drops fe_state_year and instead includes explicit
#   regional controls.
#
# Main specification:
#
#   export_value =
#     beta * iv_stock_2016_post_1000
#     + regional controls
#     + fe_state_origin
#     + fe_origin_year
#     + error
#
# Regional controls:
#   log_gdp_million_eur
#   log_population
#   unemployment_rate
#   log_employment_thousand_persons
#   manufacturing_share
#   log_total_exports_world
#
# Main estimator:
#   PPML / fepois
#
# Standard errors:
#   Clustered at federal_state × origin_country level.
#
# Output objects:
#   robustness_controls_ppml_reduced_form_stock_1000
#   robustness_controls_ppml_benchmark_stock_1000
#   robustness_controls_ppml_reduced_form_delta_1000
#   robustness_controls_ppml_benchmark_delta_1000
#   robustness_controls_ppml_reduced_form_stock_no_eritrea_1000
#   robustness_controls_ppml_benchmark_stock_no_eritrea_1000
#   robustness_controls_results_overview
#   robustness_controls_results_paper
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
#   analysis_panel_controls.rds
#   analysis_panel_controls_no_eritrea.rds
#
# Notes:
#   This is a regression / analysis script. Therefore, it loads existing .rds
#   panels directly and does not rebuild them from raw data.
# ============================================================

required_input_files <- c(
  "analysis_panel_controls.rds",
  "analysis_panel_controls_no_eritrea.rds"
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
      "Please rerun the regional-control data-cleaning / panel-construction script before running this robustness script."
    )
  )
}


# ============================================================
# Load required panels
# ============================================================
#
# Purpose:
#   Load the already constructed regional-control analysis panels from disk.
#
# Panels:
#   analysis_panel_controls
#   analysis_panel_controls_no_eritrea
#
# Notes:
#   These panels should already contain regional controls, outcome variables,
#   treatment variables, instrument variables, scaled _1000 variables, and
#   fixed-effect identifiers.
# ============================================================

analysis_panel_controls <- readRDS(
  "analysis_panel_controls.rds"
)

analysis_panel_controls_no_eritrea <- readRDS(
  "analysis_panel_controls_no_eritrea.rds"
)


# ============================================================
# Defensive fixed-effect reconstruction
# ============================================================
#
# Purpose:
#   Ensure that the fixed-effect identifiers needed in the regional-control
#   robustness specifications exist before running the required-variable
#   check and regressions.
#
# Fixed effects used in this script:
#   fe_state_origin = federal_state × origin_country
#   fe_origin_year  = origin_country × year
#
# Important:
#   fe_state_year is intentionally not used here.
#
#   The purpose of this robustness check is to replace federal_state × year
#   fixed effects with explicit regional controls.
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

analysis_panel_controls <- add_fixed_effects_if_missing(
  analysis_panel_controls
)

analysis_panel_controls_no_eritrea <- add_fixed_effects_if_missing(
  analysis_panel_controls_no_eritrea
)


# ============================================================
# Required-variable check
# ============================================================
#
# Purpose:
#   Check whether the loaded regional-control panels contain all variables
#   required for the robustness specifications.
#
# Notes:
#   This check is run after defensive fixed-effect reconstruction so that
#   reconstructable fixed-effect identifiers are not falsely reported as
#   missing.
#
#   If key regional controls, outcome variables, treatment variables, or IV
#   variables are missing, rerun the relevant data-construction and rescaling
#   scripts.
# ============================================================

required_robustness_controls_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "export_value",
  "treatment_stock_2016_post_1000",
  "treatment_delta_post_1000",
  "iv_stock_2016_post_1000",
  "iv_delta_post_1000",
  "fe_state_origin",
  "fe_origin_year",
  "gdp_million_eur",
  "population",
  "unemployment_rate",
  "employment_thousand_persons",
  "manufacturing_share",
  "total_exports_world"
)

missing_robustness_controls_variables <- bind_rows(
  tibble(
    panel = "analysis_panel_controls",
    variable = required_robustness_controls_variables,
    present = required_robustness_controls_variables %in%
      names(analysis_panel_controls)
  ),
  
  tibble(
    panel = "analysis_panel_controls_no_eritrea",
    variable = required_robustness_controls_variables,
    present = required_robustness_controls_variables %in%
      names(analysis_panel_controls_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_robustness_controls_variables

if (nrow(missing_robustness_controls_variables) > 0) {
  stop(
    "At least one required variable for the regional-control robustness checks is missing. Inspect missing_robustness_controls_variables."
  )
}


# ============================================================
# Construct logged regional controls
# ============================================================
#
# Purpose:
#   Construct logged scale controls used in the robustness regressions.
#
# Notes:
#   Logs are used for scale variables.
#
#   Rates and shares remain in levels:
#     unemployment_rate
#     manufacturing_share
#
# Important:
#   These logged controls are constructed from existing regional-control
#   variables in the loaded .rds panels. No raw data are rebuilt here.
# ============================================================

add_logged_regional_controls <- function(data) {
  data %>%
    mutate(
      log_gdp_million_eur = if_else(
        gdp_million_eur > 0,
        log(gdp_million_eur),
        NA_real_
      ),
      
      log_population = if_else(
        population > 0,
        log(population),
        NA_real_
      ),
      
      log_employment_thousand_persons = if_else(
        employment_thousand_persons > 0,
        log(employment_thousand_persons),
        NA_real_
      ),
      
      log_total_exports_world = if_else(
        total_exports_world >= 0,
        log(total_exports_world + 1),
        NA_real_
      )
    )
}

analysis_panel_controls <- add_logged_regional_controls(
  analysis_panel_controls
)

analysis_panel_controls_no_eritrea <- add_logged_regional_controls(
  analysis_panel_controls_no_eritrea
)


# ============================================================
# Regional-control diagnostics
# ============================================================
#
# Purpose:
#   Summarise the regional-control panels used in this robustness check.
# ============================================================

robustness_controls_diagnostics <- bind_rows(
  analysis_panel_controls %>%
    summarise(
      panel = "analysis_panel_controls",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      n_federal_states = n_distinct(federal_state),
      n_origin_countries = n_distinct(origin_country),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE)
    ),
  
  analysis_panel_controls_no_eritrea %>%
    summarise(
      panel = "analysis_panel_controls_no_eritrea",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      n_federal_states = n_distinct(federal_state),
      n_origin_countries = n_distinct(origin_country),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE)
    )
)

robustness_controls_diagnostics


# ============================================================
# Control missingness diagnostics
# ============================================================
#
# Purpose:
#   Check missingness in the logged regional controls and the level controls
#   used in the regional-control robustness specifications.
# ============================================================

regional_control_variables <- c(
  "log_gdp_million_eur",
  "log_population",
  "unemployment_rate",
  "log_employment_thousand_persons",
  "manufacturing_share",
  "log_total_exports_world"
)

robustness_controls_missingness <- bind_rows(
  analysis_panel_controls %>%
    summarise(
      across(
        all_of(regional_control_variables),
        ~ sum(is.na(.x)),
        .names = "missing_{.col}"
      )
    ) %>%
    mutate(
      panel = "analysis_panel_controls"
    ),
  
  analysis_panel_controls_no_eritrea %>%
    summarise(
      across(
        all_of(regional_control_variables),
        ~ sum(is.na(.x)),
        .names = "missing_{.col}"
      )
    ) %>%
    mutate(
      panel = "analysis_panel_controls_no_eritrea"
    )
) %>%
  select(
    panel,
    everything()
  )

robustness_controls_missingness


# ============================================================
# Helper function: run PPML safely
# ============================================================
#
# Purpose:
#   Estimate a PPML fixed-effects model while preventing the full script from
#   stopping if one robustness specification cannot be estimated.
#
# Estimator:
#   fepois from fixest
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
      message("PPML regional-control model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: extract key coefficient safely
# ============================================================
#
# Purpose:
#   Extract the coefficient of interest and key model information from each
#   regional-control robustness model.
#
# Extracted values:
#   estimate
#   standard error
#   z-statistic or t-statistic
#   p-value
#   number of observations
#   estimation status
# ============================================================

extract_key_coefficient_safely <- function(
    model,
    variable,
    model_name,
    sample,
    estimator,
    outcome_variable,
    treatment_type,
    fixed_effect_structure,
    controls_included
) {
  if (is.null(model)) {
    return(
      tibble(
        model_name = model_name,
        sample = sample,
        estimator = estimator,
        outcome_variable = outcome_variable,
        treatment_type = treatment_type,
        variable = variable,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        n_obs = NA_integer_,
        fixed_effect_structure = fixed_effect_structure,
        controls_included = controls_included,
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
  
  if (!(variable %in% rownames(coefficient_table))) {
    return(
      tibble(
        model_name = model_name,
        sample = sample,
        estimator = estimator,
        outcome_variable = outcome_variable,
        treatment_type = treatment_type,
        variable = variable,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        n_obs = nobs(model),
        fixed_effect_structure = fixed_effect_structure,
        controls_included = controls_included,
        status = "term dropped"
      )
    )
  }
  
  tibble(
    model_name = model_name,
    sample = sample,
    estimator = estimator,
    outcome_variable = outcome_variable,
    treatment_type = treatment_type,
    variable = variable,
    estimate = coefficient_table[variable, "Estimate"],
    std_error = coefficient_table[variable, "Std. Error"],
    
    statistic = if (!is.na(statistic_column)) {
      coefficient_table[variable, statistic_column]
    } else {
      NA_real_
    },
    
    p_value = if (!is.na(p_value_column)) {
      coefficient_table[variable, p_value_column]
    } else {
      NA_real_
    },
    
    n_obs = nobs(model),
    fixed_effect_structure = fixed_effect_structure,
    controls_included = controls_included,
    status = "estimated"
  )
}


# ============================================================
# Regional controls formula component
# ============================================================
#
# Purpose:
#   Create the formula component containing the explicit regional controls.
#
# Notes:
#   fe_state_year is deliberately not included in the model formulas below.
#   It is replaced by the explicit regional controls in this robustness check.
# ============================================================

regional_controls_formula <- paste(
  regional_control_variables,
  collapse = " + "
)

regional_controls_formula


# ============================================================
# 1. PPML reduced form with regional controls: stock exposure
# ============================================================

robustness_controls_ppml_reduced_form_stock_1000 <- run_ppml_safely(
  formula = as.formula(
    paste0(
      "export_value ~ iv_stock_2016_post_1000 + ",
      regional_controls_formula,
      " | fe_state_origin + fe_origin_year"
    )
  ),
  data = analysis_panel_controls,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_controls_ppml_reduced_form_stock_1000)) {
  summary(robustness_controls_ppml_reduced_form_stock_1000)
}


# ============================================================
# 2. PPML benchmark with regional controls: stock exposure
# ============================================================

robustness_controls_ppml_benchmark_stock_1000 <- run_ppml_safely(
  formula = as.formula(
    paste0(
      "export_value ~ treatment_stock_2016_post_1000 + ",
      regional_controls_formula,
      " | fe_state_origin + fe_origin_year"
    )
  ),
  data = analysis_panel_controls,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_controls_ppml_benchmark_stock_1000)) {
  summary(robustness_controls_ppml_benchmark_stock_1000)
}


# ============================================================
# 3. PPML reduced form with regional controls: delta exposure
# ============================================================

robustness_controls_ppml_reduced_form_delta_1000 <- run_ppml_safely(
  formula = as.formula(
    paste0(
      "export_value ~ iv_delta_post_1000 + ",
      regional_controls_formula,
      " | fe_state_origin + fe_origin_year"
    )
  ),
  data = analysis_panel_controls,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_controls_ppml_reduced_form_delta_1000)) {
  summary(robustness_controls_ppml_reduced_form_delta_1000)
}


# ============================================================
# 4. PPML benchmark with regional controls: delta exposure
# ============================================================

robustness_controls_ppml_benchmark_delta_1000 <- run_ppml_safely(
  formula = as.formula(
    paste0(
      "export_value ~ treatment_delta_post_1000 + ",
      regional_controls_formula,
      " | fe_state_origin + fe_origin_year"
    )
  ),
  data = analysis_panel_controls,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_controls_ppml_benchmark_delta_1000)) {
  summary(robustness_controls_ppml_benchmark_delta_1000)
}


# ============================================================
# 5. PPML reduced form with regional controls: no Eritrea
# ============================================================

robustness_controls_ppml_reduced_form_stock_no_eritrea_1000 <- run_ppml_safely(
  formula = as.formula(
    paste0(
      "export_value ~ iv_stock_2016_post_1000 + ",
      regional_controls_formula,
      " | fe_state_origin + fe_origin_year"
    )
  ),
  data = analysis_panel_controls_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_controls_ppml_reduced_form_stock_no_eritrea_1000)) {
  summary(robustness_controls_ppml_reduced_form_stock_no_eritrea_1000)
}


# ============================================================
# 6. PPML benchmark with regional controls: no Eritrea
# ============================================================

robustness_controls_ppml_benchmark_stock_no_eritrea_1000 <- run_ppml_safely(
  formula = as.formula(
    paste0(
      "export_value ~ treatment_stock_2016_post_1000 + ",
      regional_controls_formula,
      " | fe_state_origin + fe_origin_year"
    )
  ),
  data = analysis_panel_controls_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_controls_ppml_benchmark_stock_no_eritrea_1000)) {
  summary(robustness_controls_ppml_benchmark_stock_no_eritrea_1000)
}


# ============================================================
# 7. Extract key regional-control coefficients
# ============================================================

robustness_controls_results_overview <- bind_rows(
  extract_key_coefficient_safely(
    model = robustness_controls_ppml_reduced_form_stock_1000,
    variable = "iv_stock_2016_post_1000",
    model_name = "Regional controls PPML reduced form: stock exposure",
    sample = "Full sample",
    estimator = "PPML / fepois",
    outcome_variable = "export_value",
    treatment_type = "Predicted stock exposure",
    fixed_effect_structure = "fe_state_origin + fe_origin_year",
    controls_included = paste(regional_control_variables, collapse = ", ")
  ),
  
  extract_key_coefficient_safely(
    model = robustness_controls_ppml_benchmark_stock_1000,
    variable = "treatment_stock_2016_post_1000",
    model_name = "Regional controls PPML benchmark: stock exposure",
    sample = "Full sample",
    estimator = "PPML / fepois",
    outcome_variable = "export_value",
    treatment_type = "Actual stock exposure",
    fixed_effect_structure = "fe_state_origin + fe_origin_year",
    controls_included = paste(regional_control_variables, collapse = ", ")
  ),
  
  extract_key_coefficient_safely(
    model = robustness_controls_ppml_reduced_form_delta_1000,
    variable = "iv_delta_post_1000",
    model_name = "Regional controls PPML reduced form: delta exposure",
    sample = "Full sample",
    estimator = "PPML / fepois",
    outcome_variable = "export_value",
    treatment_type = "Predicted delta exposure",
    fixed_effect_structure = "fe_state_origin + fe_origin_year",
    controls_included = paste(regional_control_variables, collapse = ", ")
  ),
  
  extract_key_coefficient_safely(
    model = robustness_controls_ppml_benchmark_delta_1000,
    variable = "treatment_delta_post_1000",
    model_name = "Regional controls PPML benchmark: delta exposure",
    sample = "Full sample",
    estimator = "PPML / fepois",
    outcome_variable = "export_value",
    treatment_type = "Actual delta exposure",
    fixed_effect_structure = "fe_state_origin + fe_origin_year",
    controls_included = paste(regional_control_variables, collapse = ", ")
  ),
  
  extract_key_coefficient_safely(
    model = robustness_controls_ppml_reduced_form_stock_no_eritrea_1000,
    variable = "iv_stock_2016_post_1000",
    model_name = "Regional controls PPML reduced form: stock exposure",
    sample = "Excluding Eritrea",
    estimator = "PPML / fepois",
    outcome_variable = "export_value",
    treatment_type = "Predicted stock exposure",
    fixed_effect_structure = "fe_state_origin + fe_origin_year",
    controls_included = paste(regional_control_variables, collapse = ", ")
  ),
  
  extract_key_coefficient_safely(
    model = robustness_controls_ppml_benchmark_stock_no_eritrea_1000,
    variable = "treatment_stock_2016_post_1000",
    model_name = "Regional controls PPML benchmark: stock exposure",
    sample = "Excluding Eritrea",
    estimator = "PPML / fepois",
    outcome_variable = "export_value",
    treatment_type = "Actual stock exposure",
    fixed_effect_structure = "fe_state_origin + fe_origin_year",
    controls_included = paste(regional_control_variables, collapse = ", ")
  )
)

robustness_controls_results_overview


# ============================================================
# 8. Paper-ready rounded table
# ============================================================

robustness_controls_results_paper <- robustness_controls_results_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    statistic = round(statistic, 2),
    p_value = signif(p_value, 3),
    n_obs = as.integer(n_obs)
  )

robustness_controls_results_paper


# ============================================================
# 9. Main paper-ready text values
# ============================================================

main_robustness_controls_reduced_form <- robustness_controls_results_paper %>%
  filter(
    model_name == "Regional controls PPML reduced form: stock exposure",
    sample == "Full sample"
  )

main_robustness_controls_benchmark <- robustness_controls_results_paper %>%
  filter(
    model_name == "Regional controls PPML benchmark: stock exposure",
    sample == "Full sample"
  )

main_robustness_controls_reduced_form
main_robustness_controls_benchmark


# ============================================================
# 10. Save regional-control robustness outputs
# ============================================================

### Model objects

if (!is.null(robustness_controls_ppml_reduced_form_stock_1000)) {
  saveRDS(
    robustness_controls_ppml_reduced_form_stock_1000,
    "robustness_controls_ppml_reduced_form_stock_1000.rds"
  )
}

if (!is.null(robustness_controls_ppml_benchmark_stock_1000)) {
  saveRDS(
    robustness_controls_ppml_benchmark_stock_1000,
    "robustness_controls_ppml_benchmark_stock_1000.rds"
  )
}

if (!is.null(robustness_controls_ppml_reduced_form_delta_1000)) {
  saveRDS(
    robustness_controls_ppml_reduced_form_delta_1000,
    "robustness_controls_ppml_reduced_form_delta_1000.rds"
  )
}

if (!is.null(robustness_controls_ppml_benchmark_delta_1000)) {
  saveRDS(
    robustness_controls_ppml_benchmark_delta_1000,
    "robustness_controls_ppml_benchmark_delta_1000.rds"
  )
}

if (!is.null(robustness_controls_ppml_reduced_form_stock_no_eritrea_1000)) {
  saveRDS(
    robustness_controls_ppml_reduced_form_stock_no_eritrea_1000,
    "robustness_controls_ppml_reduced_form_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(robustness_controls_ppml_benchmark_stock_no_eritrea_1000)) {
  saveRDS(
    robustness_controls_ppml_benchmark_stock_no_eritrea_1000,
    "robustness_controls_ppml_benchmark_stock_no_eritrea_1000.rds"
  )
}


### Summary tables

saveRDS(
  robustness_controls_results_overview,
  "robustness_controls_results_overview.rds"
)

saveRDS(
  robustness_controls_results_paper,
  "robustness_controls_results_paper.rds"
)


### Diagnostics

saveRDS(
  robustness_controls_diagnostics,
  "robustness_controls_diagnostics.rds"
)

saveRDS(
  robustness_controls_missingness,
  "robustness_controls_missingness.rds"
)

saveRDS(
  missing_input_files,
  "robustness_controls_missing_input_files.rds"
)

saveRDS(
  missing_robustness_controls_variables,
  "missing_robustness_controls_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_robustness_controls_reduced_form,
  "main_robustness_controls_reduced_form.rds"
)

saveRDS(
  main_robustness_controls_benchmark,
  "main_robustness_controls_benchmark.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_robustness_controls_variables,
  add_fixed_effects_if_missing,
  add_logged_regional_controls,
  regional_control_variables,
  regional_controls_formula,
  run_ppml_safely,
  extract_key_coefficient_safely
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base regional-control panels:
#   analysis_panel_controls
#   analysis_panel_controls_no_eritrea
#
# Regional-control PPML reduced-form model objects:
#   robustness_controls_ppml_reduced_form_stock_1000
#   robustness_controls_ppml_reduced_form_delta_1000
#   robustness_controls_ppml_reduced_form_stock_no_eritrea_1000
#
# Regional-control PPML benchmark model objects:
#   robustness_controls_ppml_benchmark_stock_1000
#   robustness_controls_ppml_benchmark_delta_1000
#   robustness_controls_ppml_benchmark_stock_no_eritrea_1000
#
# Combined result tables:
#   robustness_controls_results_overview
#   robustness_controls_results_paper
#
# Diagnostics:
#   robustness_controls_diagnostics
#   robustness_controls_missingness
#   missing_robustness_controls_variables
#
# Paper-ready text values:
#   main_robustness_controls_reduced_form
#   main_robustness_controls_benchmark
#
# Notes:
#   This script estimates the regional-control robustness checks.
#
#   The purpose is to check whether the main null result depends on the
#   saturated federal_state × year fixed effects.
#
#   In the preferred specification, federal_state × year fixed effects absorb
#   all state-year variation, including regional GDP, population,
#   unemployment, employment, manufacturing structure, and total export
#   capacity.
#
#   In this robustness check, fe_state_year is dropped and replaced by
#   explicit regional controls:
#     log_gdp_million_eur
#     log_population
#     unemployment_rate
#     log_employment_thousand_persons
#     manufacturing_share
#     log_total_exports_world
#
#   The retained fixed effects are:
#     fe_state_origin
#     fe_origin_year
#
#   The main regional-control reduced-form object is:
#     robustness_controls_ppml_reduced_form_stock_1000
#
#   The main regional-control benchmark object is:
#     robustness_controls_ppml_benchmark_stock_1000
#
#   The main outcome is:
#     export_value
#
#   The main reduced-form variable is:
#     iv_stock_2016_post_1000
#
#   The main benchmark treatment variable is:
#     treatment_stock_2016_post_1000
#
#   This robustness check should be interpreted as a sensitivity analysis,
#   not as the preferred specification. The preferred specification remains
#   the PPML reduced form with federal_state × origin_country,
#   federal_state × year, and origin_country × year fixed effects.
#
#   In the final write-up, refer to this section as:
#     regional-control robustness check.
#
#   Do not describe it as:
#     main specification
#     preferred PPML reduced form
#     CEPII robustness
#     IV / 2SLS estimate
#
#   If the coefficient remains statistically insignificant, this supports
#   the interpretation that the null result is not driven mechanically by the
#   inclusion of saturated federal_state × year fixed effects.
#
#   This is a regression / analysis script. It loads existing .rds panels and
#   estimates models. It does not rebuild the analysis panel, controls,
#   treatment variables, instruments, fixed effects, or rescaled variables
#   from raw data.
# ============================================================