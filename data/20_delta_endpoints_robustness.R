# ============================================================
# Empirical results: Delta-endpoint robustness
# 2014–2017 exposure change instead of 2014–2016
# ============================================================
#
# Purpose:
#   Check whether the null result depends on defining the migration shock
#   as the change in protection-seeker exposure from 2014 to 2016.
#
# Script type:
#   Regression / analysis script
#
# Workflow logic:
#   This script loads already constructed delta-endpoint .rds panels and
#   estimates robustness regressions.
#
#   It does not clean raw data and does not construct the 2014–2017
#   treatment or IV variables.
#
# Required input panels:
#   analysis_panel_delta_endpoint.rds
#   analysis_panel_no_eritrea_delta_endpoint.rds
#
# Main specification:
#   Re-estimate the preferred reduced-form and benchmark specifications
#   using an alternative migration-exposure measure based on the change in
#   protection-seeker stocks from 2014 to 2017.
#
# Main alternative treatment:
#   treatment_delta_2014_2017_post_1000
#
# Main alternative instrument:
#   iv_delta_2014_2017_post_1000
#
# Estimator:
#   First stage: feols
#   PPML reduced form: fepois
#   PPML benchmark: fepois
#   Linear reduced form and IV: feols
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
#   This robustness check tests whether the null result is sensitive to
#   choosing 2016 as the endpoint of the migration-shock measure.
#
#   The 2014–2017 endpoint allows for delayed adjustment in regional exposure
#   after the initial 2015/16 refugee inflow.
#
#   It should not replace the main 2014–2016 exposure definition because
#   the 2017 endpoint is further removed from the initial allocation shock
#   and may be more affected by secondary mobility or endogenous location
#   choices.
#
# Output objects:
#   robustness_delta_endpoint_first_stage_1000
#   robustness_delta_endpoint_ppml_reduced_form_1000
#   robustness_delta_endpoint_ppml_benchmark_1000
#   robustness_delta_endpoint_linear_reduced_form_1000
#   robustness_delta_endpoint_linear_iv_1000
#   robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000
#   robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000
#   robustness_delta_endpoint_results_overview
#   robustness_delta_endpoint_results_paper
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
library(fixest)
library(tibble)


# ============================================================
# Required input files
# ============================================================
#
# Purpose:
#   Check whether the constructed delta-endpoint panels exist before running
#   the robustness regressions.
#
# Required panels:
#   analysis_panel_delta_endpoint.rds
#   analysis_panel_no_eritrea_delta_endpoint.rds
# ============================================================

required_input_files <- c(
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
      "The following required delta-endpoint panel files are missing:",
      paste(missing_input_files, collapse = ", "),
      "Please run 08_delta_endpoint_variables.R before running this regression script."
    )
  )
}


# ============================================================
# Load constructed delta-endpoint panels
# ============================================================
#
# Purpose:
#   Load the full-sample and no-Eritrea delta-endpoint panels.
#
# Notes:
#   These panels are created in:
#     08_delta_endpoint_variables.R
# ============================================================

analysis_panel_delta_endpoint <- readRDS(
  "analysis_panel_delta_endpoint.rds"
)

analysis_panel_no_eritrea_delta_endpoint <- readRDS(
  "analysis_panel_no_eritrea_delta_endpoint.rds"
)


# ============================================================
# Required-variable check
# ============================================================
#
# Purpose:
#   Check whether the loaded delta-endpoint panels contain all variables
#   required for the robustness regressions.
# ============================================================

required_delta_endpoint_regression_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "post_period",
  "export_value",
  "log_export_value",
  "treatment_delta_2014_2017_post_1000",
  "iv_delta_2014_2017_post_1000",
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)

missing_delta_endpoint_regression_variables <- bind_rows(
  tibble(
    panel = "analysis_panel_delta_endpoint",
    variable = required_delta_endpoint_regression_variables,
    present = required_delta_endpoint_regression_variables %in%
      names(analysis_panel_delta_endpoint)
  ),
  
  tibble(
    panel = "analysis_panel_no_eritrea_delta_endpoint",
    variable = required_delta_endpoint_regression_variables,
    present = required_delta_endpoint_regression_variables %in%
      names(analysis_panel_no_eritrea_delta_endpoint)
  )
) %>%
  filter(
    !present
  )

missing_delta_endpoint_regression_variables

if (nrow(missing_delta_endpoint_regression_variables) > 0) {
  stop(
    "At least one required variable for the delta-endpoint robustness regressions is missing. Inspect missing_delta_endpoint_regression_variables."
  )
}


# ============================================================
# Delta-endpoint regression diagnostics
# ============================================================
#
# Purpose:
#   Document the sample structure and missingness of the loaded
#   delta-endpoint panels before estimating regressions.
# ============================================================

robustness_delta_endpoint_regression_diagnostics <- bind_rows(
  analysis_panel_delta_endpoint %>%
    summarise(
      panel = "analysis_panel_delta_endpoint",
      sample = "Full sample",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      missing_export_value = sum(is.na(export_value)),
      missing_log_export_value = sum(is.na(log_export_value)),
      missing_treatment_delta_2014_2017_post_1000 = sum(
        is.na(treatment_delta_2014_2017_post_1000)
      ),
      missing_iv_delta_2014_2017_post_1000 = sum(
        is.na(iv_delta_2014_2017_post_1000)
      )
    ),
  
  analysis_panel_no_eritrea_delta_endpoint %>%
    summarise(
      panel = "analysis_panel_no_eritrea_delta_endpoint",
      sample = "Excluding Eritrea",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      missing_export_value = sum(is.na(export_value)),
      missing_log_export_value = sum(is.na(log_export_value)),
      missing_treatment_delta_2014_2017_post_1000 = sum(
        is.na(treatment_delta_2014_2017_post_1000)
      ),
      missing_iv_delta_2014_2017_post_1000 = sum(
        is.na(iv_delta_2014_2017_post_1000)
      )
    )
)

robustness_delta_endpoint_regression_diagnostics


# ============================================================
# Helper function: run feols safely
# ============================================================
#
# Purpose:
#   Estimate linear models while preventing the full script from stopping if
#   a specification is not estimable.
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
      message("Linear delta-endpoint model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: run PPML safely
# ============================================================
#
# Purpose:
#   Estimate PPML models while preventing the full script from stopping if a
#   specification is not estimable.
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
      message("PPML delta-endpoint model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: extract model results safely
# ============================================================
#
# Purpose:
#   Extract coefficient estimates, standard errors, test statistics,
#   p-values, fit statistics, sample size and estimation status from model
#   objects.
#
# Logic:
#   For PPML models, the fit statistic is the pseudo-R2.
#   For linear fixed-effect models, the fit statistic is the within-R2.
# ============================================================

extract_delta_endpoint_results_safely <- function(
    model,
    term,
    specification,
    sample,
    estimator,
    outcome_variable,
    variable_of_interest
) {
  if (is.null(model)) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        estimator = estimator,
        outcome_variable = outcome_variable,
        variable_of_interest = variable_of_interest,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        fit_statistic = NA_real_,
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
  
  fit_value <- suppressWarnings(
    tryCatch(
      {
        if (estimator == "PPML / fepois") {
          fitstat(model, "pr2")$pr2
        } else {
          fitstat(model, "wr2")$wr2
        }
      },
      error = function(e) {
        NA_real_
      }
    )
  )
  
  if (!(term %in% rownames(coefficient_table))) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        estimator = estimator,
        outcome_variable = outcome_variable,
        variable_of_interest = variable_of_interest,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        fit_statistic = fit_value,
        n_obs = nobs(model),
        status = "term dropped"
      )
    )
  }
  
  tibble(
    sample = sample,
    specification = specification,
    estimator = estimator,
    outcome_variable = outcome_variable,
    variable_of_interest = variable_of_interest,
    term = term,
    estimate = coefficient_table[term, "Estimate"],
    std_error = coefficient_table[term, "Std. Error"],
    
    statistic = if (!is.na(statistic_column)) {
      coefficient_table[term, statistic_column]
    } else {
      NA_real_
    },
    
    p_value = if (!is.na(p_value_column)) {
      coefficient_table[term, p_value_column]
    } else {
      NA_real_
    },
    
    fit_statistic = fit_value,
    n_obs = nobs(model),
    status = "estimated"
  )
}


# ============================================================
# 1. First stage: 2014–2017 delta endpoint
# ============================================================

robustness_delta_endpoint_first_stage_1000 <- run_feols_safely(
  formula =
    treatment_delta_2014_2017_post_1000 ~
    iv_delta_2014_2017_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_delta_endpoint,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_delta_endpoint_first_stage_1000)) {
  summary(robustness_delta_endpoint_first_stage_1000)
}


robustness_delta_endpoint_first_stage_summary <- extract_delta_endpoint_results_safely(
  model = robustness_delta_endpoint_first_stage_1000,
  term = "iv_delta_2014_2017_post_1000",
  specification = "Delta-endpoint first stage: 2014–2017",
  sample = "Full sample",
  estimator = "feols",
  outcome_variable = "treatment_delta_2014_2017_post_1000",
  variable_of_interest = "iv_delta_2014_2017_post_1000"
)

robustness_delta_endpoint_first_stage_summary


# ============================================================
# 2. PPML reduced form: 2014–2017 delta endpoint
# ============================================================

robustness_delta_endpoint_ppml_reduced_form_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_delta_2014_2017_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_delta_endpoint,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_delta_endpoint_ppml_reduced_form_1000)) {
  summary(robustness_delta_endpoint_ppml_reduced_form_1000)
}


robustness_delta_endpoint_ppml_reduced_form_summary <- extract_delta_endpoint_results_safely(
  model = robustness_delta_endpoint_ppml_reduced_form_1000,
  term = "iv_delta_2014_2017_post_1000",
  specification = "Delta-endpoint PPML reduced form: 2014–2017",
  sample = "Full sample",
  estimator = "PPML / fepois",
  outcome_variable = "export_value",
  variable_of_interest = "iv_delta_2014_2017_post_1000"
)

robustness_delta_endpoint_ppml_reduced_form_summary


# ============================================================
# 3. PPML benchmark: 2014–2017 delta endpoint
# ============================================================

robustness_delta_endpoint_ppml_benchmark_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_delta_2014_2017_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_delta_endpoint,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_delta_endpoint_ppml_benchmark_1000)) {
  summary(robustness_delta_endpoint_ppml_benchmark_1000)
}


robustness_delta_endpoint_ppml_benchmark_summary <- extract_delta_endpoint_results_safely(
  model = robustness_delta_endpoint_ppml_benchmark_1000,
  term = "treatment_delta_2014_2017_post_1000",
  specification = "Delta-endpoint PPML benchmark: 2014–2017",
  sample = "Full sample",
  estimator = "PPML / fepois",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_delta_2014_2017_post_1000"
)

robustness_delta_endpoint_ppml_benchmark_summary


# ============================================================
# 4. Linear reduced form: 2014–2017 delta endpoint
# ============================================================

robustness_delta_endpoint_linear_reduced_form_1000 <- run_feols_safely(
  formula =
    log_export_value ~ iv_delta_2014_2017_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_delta_endpoint,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_delta_endpoint_linear_reduced_form_1000)) {
  summary(robustness_delta_endpoint_linear_reduced_form_1000)
}


robustness_delta_endpoint_linear_reduced_form_summary <- extract_delta_endpoint_results_safely(
  model = robustness_delta_endpoint_linear_reduced_form_1000,
  term = "iv_delta_2014_2017_post_1000",
  specification = "Delta-endpoint linear reduced form: 2014–2017",
  sample = "Full sample",
  estimator = "feols",
  outcome_variable = "log_export_value",
  variable_of_interest = "iv_delta_2014_2017_post_1000"
)

robustness_delta_endpoint_linear_reduced_form_summary


# ============================================================
# 5. Linear IV / 2SLS: 2014–2017 delta endpoint
# ============================================================

robustness_delta_endpoint_linear_iv_1000 <- run_feols_safely(
  formula =
    log_export_value ~ 1 |
    fe_state_origin + fe_state_year + fe_origin_year |
    treatment_delta_2014_2017_post_1000 ~
    iv_delta_2014_2017_post_1000,
  data = analysis_panel_delta_endpoint,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_delta_endpoint_linear_iv_1000)) {
  summary(robustness_delta_endpoint_linear_iv_1000)
}


robustness_delta_endpoint_linear_iv_summary <- extract_delta_endpoint_results_safely(
  model = robustness_delta_endpoint_linear_iv_1000,
  term = "fit_treatment_delta_2014_2017_post_1000",
  specification = "Delta-endpoint linear IV / 2SLS: 2014–2017",
  sample = "Full sample",
  estimator = "feols IV",
  outcome_variable = "log_export_value",
  variable_of_interest = "treatment_delta_2014_2017_post_1000 instrumented by iv_delta_2014_2017_post_1000"
)

robustness_delta_endpoint_linear_iv_summary


# ============================================================
# 6. No-Eritrea PPML reduced form: 2014–2017 delta endpoint
# ============================================================

robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_delta_2014_2017_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea_delta_endpoint,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000)) {
  summary(robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000)
}


robustness_delta_endpoint_ppml_reduced_form_no_eritrea_summary <- extract_delta_endpoint_results_safely(
  model = robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000,
  term = "iv_delta_2014_2017_post_1000",
  specification = "Delta-endpoint PPML reduced form: 2014–2017",
  sample = "Excluding Eritrea",
  estimator = "PPML / fepois",
  outcome_variable = "export_value",
  variable_of_interest = "iv_delta_2014_2017_post_1000"
)

robustness_delta_endpoint_ppml_reduced_form_no_eritrea_summary


# ============================================================
# 7. No-Eritrea PPML benchmark: 2014–2017 delta endpoint
# ============================================================

robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_delta_2014_2017_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea_delta_endpoint,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000)) {
  summary(robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000)
}


robustness_delta_endpoint_ppml_benchmark_no_eritrea_summary <- extract_delta_endpoint_results_safely(
  model = robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000,
  term = "treatment_delta_2014_2017_post_1000",
  specification = "Delta-endpoint PPML benchmark: 2014–2017",
  sample = "Excluding Eritrea",
  estimator = "PPML / fepois",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_delta_2014_2017_post_1000"
)

robustness_delta_endpoint_ppml_benchmark_no_eritrea_summary


# ============================================================
# 8. Combined delta-endpoint results overview
# ============================================================

robustness_delta_endpoint_results_overview <- bind_rows(
  robustness_delta_endpoint_first_stage_summary,
  robustness_delta_endpoint_ppml_reduced_form_summary,
  robustness_delta_endpoint_ppml_benchmark_summary,
  robustness_delta_endpoint_linear_reduced_form_summary,
  robustness_delta_endpoint_linear_iv_summary,
  robustness_delta_endpoint_ppml_reduced_form_no_eritrea_summary,
  robustness_delta_endpoint_ppml_benchmark_no_eritrea_summary
) %>%
  select(
    sample,
    specification,
    estimator,
    outcome_variable,
    variable_of_interest,
    term,
    estimate,
    std_error,
    statistic,
    p_value,
    fit_statistic,
    n_obs,
    status
  )

robustness_delta_endpoint_results_overview


# ============================================================
# 9. Paper-ready rounded delta-endpoint results
# ============================================================

robustness_delta_endpoint_results_paper <- robustness_delta_endpoint_results_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    statistic = round(statistic, 2),
    p_value = signif(p_value, 3),
    fit_statistic = round(fit_statistic, 3),
    n_obs = as.integer(n_obs)
  )

robustness_delta_endpoint_results_paper


# ============================================================
# 10. Paper-ready text values
# ============================================================

main_robustness_delta_endpoint_first_stage <-
  robustness_delta_endpoint_results_paper %>%
  filter(
    specification == "Delta-endpoint first stage: 2014–2017",
    sample == "Full sample"
  )

main_robustness_delta_endpoint_reduced_form <-
  robustness_delta_endpoint_results_paper %>%
  filter(
    specification == "Delta-endpoint PPML reduced form: 2014–2017",
    sample == "Full sample"
  )

main_robustness_delta_endpoint_benchmark <-
  robustness_delta_endpoint_results_paper %>%
  filter(
    specification == "Delta-endpoint PPML benchmark: 2014–2017",
    sample == "Full sample"
  )

main_robustness_delta_endpoint_linear_iv <-
  robustness_delta_endpoint_results_paper %>%
  filter(
    specification == "Delta-endpoint linear IV / 2SLS: 2014–2017",
    sample == "Full sample"
  )

main_robustness_delta_endpoint_first_stage
main_robustness_delta_endpoint_reduced_form
main_robustness_delta_endpoint_benchmark
main_robustness_delta_endpoint_linear_iv


# ============================================================
# 11. Save delta-endpoint regression outputs
# ============================================================

### Model objects

if (!is.null(robustness_delta_endpoint_first_stage_1000)) {
  saveRDS(
    robustness_delta_endpoint_first_stage_1000,
    "robustness_delta_endpoint_first_stage_1000.rds"
  )
}

if (!is.null(robustness_delta_endpoint_ppml_reduced_form_1000)) {
  saveRDS(
    robustness_delta_endpoint_ppml_reduced_form_1000,
    "robustness_delta_endpoint_ppml_reduced_form_1000.rds"
  )
}

if (!is.null(robustness_delta_endpoint_ppml_benchmark_1000)) {
  saveRDS(
    robustness_delta_endpoint_ppml_benchmark_1000,
    "robustness_delta_endpoint_ppml_benchmark_1000.rds"
  )
}

if (!is.null(robustness_delta_endpoint_linear_reduced_form_1000)) {
  saveRDS(
    robustness_delta_endpoint_linear_reduced_form_1000,
    "robustness_delta_endpoint_linear_reduced_form_1000.rds"
  )
}

if (!is.null(robustness_delta_endpoint_linear_iv_1000)) {
  saveRDS(
    robustness_delta_endpoint_linear_iv_1000,
    "robustness_delta_endpoint_linear_iv_1000.rds"
  )
}

if (!is.null(robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000)) {
  saveRDS(
    robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000,
    "robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000.rds"
  )
}

if (!is.null(robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000)) {
  saveRDS(
    robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000,
    "robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000.rds"
  )
}


### Individual summary objects

saveRDS(
  robustness_delta_endpoint_first_stage_summary,
  "robustness_delta_endpoint_first_stage_summary.rds"
)

saveRDS(
  robustness_delta_endpoint_ppml_reduced_form_summary,
  "robustness_delta_endpoint_ppml_reduced_form_summary.rds"
)

saveRDS(
  robustness_delta_endpoint_ppml_benchmark_summary,
  "robustness_delta_endpoint_ppml_benchmark_summary.rds"
)

saveRDS(
  robustness_delta_endpoint_linear_reduced_form_summary,
  "robustness_delta_endpoint_linear_reduced_form_summary.rds"
)

saveRDS(
  robustness_delta_endpoint_linear_iv_summary,
  "robustness_delta_endpoint_linear_iv_summary.rds"
)

saveRDS(
  robustness_delta_endpoint_ppml_reduced_form_no_eritrea_summary,
  "robustness_delta_endpoint_ppml_reduced_form_no_eritrea_summary.rds"
)

saveRDS(
  robustness_delta_endpoint_ppml_benchmark_no_eritrea_summary,
  "robustness_delta_endpoint_ppml_benchmark_no_eritrea_summary.rds"
)


### Combined outputs

saveRDS(
  robustness_delta_endpoint_results_overview,
  "robustness_delta_endpoint_results_overview.rds"
)

saveRDS(
  robustness_delta_endpoint_results_paper,
  "robustness_delta_endpoint_results_paper.rds"
)


### Diagnostics

saveRDS(
  required_input_files,
  "delta_endpoint_regression_required_input_files.rds"
)

saveRDS(
  missing_input_files,
  "delta_endpoint_regression_missing_input_files.rds"
)

saveRDS(
  missing_delta_endpoint_regression_variables,
  "missing_delta_endpoint_regression_variables.rds"
)

saveRDS(
  robustness_delta_endpoint_regression_diagnostics,
  "robustness_delta_endpoint_regression_diagnostics.rds"
)


### Paper-ready text values

saveRDS(
  main_robustness_delta_endpoint_first_stage,
  "main_robustness_delta_endpoint_first_stage.rds"
)

saveRDS(
  main_robustness_delta_endpoint_reduced_form,
  "main_robustness_delta_endpoint_reduced_form.rds"
)

saveRDS(
  main_robustness_delta_endpoint_benchmark,
  "main_robustness_delta_endpoint_benchmark.rds"
)

saveRDS(
  main_robustness_delta_endpoint_linear_iv,
  "main_robustness_delta_endpoint_linear_iv.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_delta_endpoint_regression_variables,
  run_feols_safely,
  run_ppml_safely,
  extract_delta_endpoint_results_safely
)


# ============================================================
# Final objects kept
# ============================================================
#
# Delta-endpoint panels:
#   analysis_panel_delta_endpoint
#   analysis_panel_no_eritrea_delta_endpoint
#
# Delta-endpoint first-stage model object:
#   robustness_delta_endpoint_first_stage_1000
#
# Delta-endpoint PPML reduced-form model objects:
#   robustness_delta_endpoint_ppml_reduced_form_1000
#   robustness_delta_endpoint_ppml_reduced_form_no_eritrea_1000
#
# Delta-endpoint PPML benchmark model objects:
#   robustness_delta_endpoint_ppml_benchmark_1000
#   robustness_delta_endpoint_ppml_benchmark_no_eritrea_1000
#
# Delta-endpoint linear robustness model objects:
#   robustness_delta_endpoint_linear_reduced_form_1000
#   robustness_delta_endpoint_linear_iv_1000
#
# Individual summary objects:
#   robustness_delta_endpoint_first_stage_summary
#   robustness_delta_endpoint_ppml_reduced_form_summary
#   robustness_delta_endpoint_ppml_benchmark_summary
#   robustness_delta_endpoint_linear_reduced_form_summary
#   robustness_delta_endpoint_linear_iv_summary
#   robustness_delta_endpoint_ppml_reduced_form_no_eritrea_summary
#   robustness_delta_endpoint_ppml_benchmark_no_eritrea_summary
#
# Combined result tables:
#   robustness_delta_endpoint_results_overview
#   robustness_delta_endpoint_results_paper
#
# Diagnostics:
#   missing_delta_endpoint_regression_variables
#   robustness_delta_endpoint_regression_diagnostics
#
# Paper-ready text values:
#   main_robustness_delta_endpoint_first_stage
#   main_robustness_delta_endpoint_reduced_form
#   main_robustness_delta_endpoint_benchmark
#   main_robustness_delta_endpoint_linear_iv
#
# Notes:
#   This script estimates the 2014–2017 delta-endpoint robustness check.
#
#   It loads existing delta-endpoint panels and does not construct treatment
#   or IV variables from raw data.
#
#   The alternative treatment variable is:
#     treatment_delta_2014_2017_post_1000
#
#   The alternative instrument is:
#     iv_delta_2014_2017_post_1000
#
#   These variables are measured in thousand persons.
#
#   The 2014–2017 endpoint allows for delayed adjustment in regional exposure
#   after the initial 2015/16 refugee inflow.
#
#   This robustness check should not replace the main 2014–2016 delta
#   exposure because the 2017 endpoint is further removed from the initial
#   allocation shock and may be more affected by secondary mobility or
#   endogenous location choices.
#
#   In the final write-up, refer to this section as:
#     delta-endpoint robustness check
#     or
#     2014–2017 exposure-window robustness
#
#   Do not describe it as:
#     preferred exposure definition
#     main causal estimate
#
#   If the PPML reduced-form coefficient remains statistically insignificant,
#   this supports the interpretation that the null result is not driven by
#   the choice of 2016 as the endpoint of the migration-shock measure.
# ============================================================