# ============================================================
# Empirical results: COVID-year exclusion robustness
# ============================================================
#
# Purpose:
#   Check whether the null result is driven by global trade disruptions
#   during the COVID-19 years.
#
# Main idea:
#   Re-estimate the preferred PPML reduced-form and non-instrumented PPML
#   benchmark specifications after excluding 2020 and 2021.
#
# Excluded years:
#   2020
#   2021
#
# Main reduced-form specification:
#
#   export_value =
#     beta * iv_stock_2016_post_1000
#     + federal_state × origin_country fixed effects
#     + federal_state × year fixed effects
#     + origin_country × year fixed effects
#     + error
#
# Main benchmark specification:
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
# Main reduced-form variable:
#   iv_stock_2016_post_1000
#
# Main benchmark treatment:
#   treatment_stock_2016_post_1000
#
# Estimator:
#   PPML / fepois
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
#   This robustness check tests whether the main null result is sensitive to
#   excluding the pandemic years 2020 and 2021, when global trade flows were
#   affected by exceptional disruptions.
#
# Output objects:
#   robustness_covid_ppml_reduced_form_stock_1000
#   robustness_covid_ppml_benchmark_stock_1000
#   robustness_covid_ppml_reduced_form_delta_1000
#   robustness_covid_ppml_benchmark_delta_1000
#   robustness_covid_ppml_reduced_form_stock_no_eritrea_1000
#   robustness_covid_ppml_benchmark_stock_no_eritrea_1000
#   robustness_covid_ppml_reduced_form_weight_stock_1000
#   robustness_covid_ppml_benchmark_weight_stock_1000
#   robustness_covid_results_overview
#   robustness_covid_results_paper
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
#   Load the active analysis panels required for the COVID-year exclusion
#   robustness checks.
#
# Panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Notes:
#   analysis_panel is used for the full-sample COVID-exclusion
#   specifications.
#
#   analysis_panel_no_eritrea is used for the no-Eritrea COVID-exclusion
#   robustness specifications.
#
#   Both panels are expected to contain the _1000 treatment and IV variables
#   created in the separate rescaling script.
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
#   required by the PPML specifications.
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
#   for the COVID-year exclusion robustness specifications.
#
# Variables checked:
#   Panel identifiers, year variable, period indicator, outcome variables,
#   treatment variables, IV variables, and fixed effects.
#
# Interpretation:
#   Missing variables indicate that an earlier data-construction or rescaling
#   script must be rerun before estimating this robustness check.
#
# Notes:
#   This check is run after defensive fixed-effect reconstruction so that it
#   reflects the panels in their final usable form.
# ============================================================

required_robustness_covid_variables <- c(
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
  
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)


missing_robustness_covid_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_robustness_covid_variables,
    present = required_robustness_covid_variables %in% names(analysis_panel)
  ),
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_robustness_covid_variables,
    present = required_robustness_covid_variables %in% names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_robustness_covid_variables


# ============================================================
# Construct COVID-exclusion panels
# ============================================================
#
# Purpose:
#   Construct the estimation samples for the COVID-year exclusion
#   robustness check by dropping 2020 and 2021.
#
# Excluded years:
#   2020
#   2021
#
# Constructed panels:
#   analysis_panel_no_covid
#   analysis_panel_no_eritrea_no_covid
#
# Interpretation:
#   These panels allow the main PPML reduced-form and benchmark results to
#   be re-estimated without the years most directly affected by pandemic-era
#   global trade disruptions.
# ============================================================

analysis_panel_no_covid <- analysis_panel %>%
  filter(
    !(year %in% c(2020, 2021))
  )

analysis_panel_no_eritrea_no_covid <- analysis_panel_no_eritrea %>%
  filter(
    !(year %in% c(2020, 2021))
  )


# ============================================================
# COVID-exclusion diagnostics
# ============================================================
#
# Purpose:
#   Document how the COVID-year exclusion changes the estimation samples.
#
# Checks:
#   Number of observations, number of federal_state × origin_country pairs,
#   year coverage, and number of observations in the excluded years.
#
# Interpretation:
#   The diagnostics verify that 2020 and 2021 are removed from the
#   robustness samples and show the resulting sample sizes.
# ============================================================

robustness_covid_diagnostics <- bind_rows(
  analysis_panel %>%
    summarise(
      panel = "analysis_panel",
      sample = "Full sample before COVID-year exclusion",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      n_excluded_2020_2021 = sum(year %in% c(2020, 2021))
    ),
  
  analysis_panel_no_covid %>%
    summarise(
      panel = "analysis_panel_no_covid",
      sample = "Full sample after excluding 2020 and 2021",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      n_excluded_2020_2021 = sum(year %in% c(2020, 2021))
    ),
  
  analysis_panel_no_eritrea %>%
    summarise(
      panel = "analysis_panel_no_eritrea",
      sample = "No-Eritrea sample before COVID-year exclusion",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      n_excluded_2020_2021 = sum(year %in% c(2020, 2021))
    ),
  
  analysis_panel_no_eritrea_no_covid %>%
    summarise(
      panel = "analysis_panel_no_eritrea_no_covid",
      sample = "No-Eritrea sample after excluding 2020 and 2021",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      n_excluded_2020_2021 = sum(year %in% c(2020, 2021))
    )
)

robustness_covid_diagnostics


# ============================================================
# Helper function: run PPML safely
# ============================================================
#
# Purpose:
#   Estimate PPML models while preventing the full script from stopping if a
#   specification is not estimable.
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
      message("PPML COVID-exclusion model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: extract PPML result safely
# ============================================================
#
# Purpose:
#   Extract coefficient estimates, standard errors, test statistics,
#   p-values, pseudo-R2, sample size, and estimation status from PPML
#   robustness models.
#
# Logic:
#   The function extracts the coefficient on the specified variable of
#   interest.
#
# Notes:
#   If the model is not estimable, or if the term is dropped because of
#   collinearity, the function returns missing coefficient values and records
#   the corresponding status.
# ============================================================

extract_covid_ppml_results_safely <- function(
    model,
    term,
    specification,
    sample,
    outcome_variable,
    variable_of_interest,
    excluded_years
) {
  if (is.null(model)) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        outcome_variable = outcome_variable,
        variable_of_interest = variable_of_interest,
        excluded_years = excluded_years,
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
  
  if (!(term %in% rownames(coefficient_table))) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        outcome_variable = outcome_variable,
        variable_of_interest = variable_of_interest,
        excluded_years = excluded_years,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        z_statistic = NA_real_,
        p_value = NA_real_,
        pseudo_r2 = suppressWarnings(
          tryCatch(
            fitstat(model, "pr2")$pr2,
            error = function(e) NA_real_
          )
        ),
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
    excluded_years = excluded_years,
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
    pseudo_r2 = suppressWarnings(
      tryCatch(
        fitstat(model, "pr2")$pr2,
        error = function(e) NA_real_
      )
    ),
    n_obs = nobs(model),
    status = "estimated"
  )
}


# ============================================================
# 1. PPML reduced form without COVID years: stock exposure
# ============================================================
#
# Purpose:
#   Re-estimate the preferred PPML reduced form after excluding 2020 and
#   2021.
#
# Specification:
#   export_value ~ iv_stock_2016_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This is the main COVID-exclusion reduced-form robustness check.
# ============================================================

robustness_covid_ppml_reduced_form_stock_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_covid,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_covid_ppml_reduced_form_stock_1000)) {
  summary(robustness_covid_ppml_reduced_form_stock_1000)
}


robustness_covid_ppml_reduced_form_stock_summary <- extract_covid_ppml_results_safely(
  model = robustness_covid_ppml_reduced_form_stock_1000,
  term = "iv_stock_2016_post_1000",
  specification = "COVID exclusion PPML reduced form: stock exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "iv_stock_2016_post_1000",
  excluded_years = "2020, 2021"
)

robustness_covid_ppml_reduced_form_stock_summary


# ============================================================
# 2. PPML benchmark without COVID years: stock exposure
# ============================================================
#
# Purpose:
#   Re-estimate the non-instrumented PPML benchmark after excluding 2020
#   and 2021.
#
# Specification:
#   export_value ~ treatment_stock_2016_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This benchmark uses actual exposure rather than predicted exposure. It
#   is descriptive and should not be interpreted as the main causal IV
#   estimate.
# ============================================================

robustness_covid_ppml_benchmark_stock_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_covid,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_covid_ppml_benchmark_stock_1000)) {
  summary(robustness_covid_ppml_benchmark_stock_1000)
}


robustness_covid_ppml_benchmark_stock_summary <- extract_covid_ppml_results_safely(
  model = robustness_covid_ppml_benchmark_stock_1000,
  term = "treatment_stock_2016_post_1000",
  specification = "COVID exclusion PPML benchmark: stock exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_stock_2016_post_1000",
  excluded_years = "2020, 2021"
)

robustness_covid_ppml_benchmark_stock_summary


# ============================================================
# 3. PPML reduced form without COVID years: delta exposure
# ============================================================
#
# Purpose:
#   Re-estimate the PPML reduced form using the alternative delta-exposure
#   instrument after excluding 2020 and 2021.
#
# Specification:
#   export_value ~ iv_delta_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This checks whether the COVID-exclusion result is robust to using the
#   2014–2016 change in protection-seeker exposure instead of the 2016 stock
#   exposure.
# ============================================================

robustness_covid_ppml_reduced_form_delta_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_covid,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_covid_ppml_reduced_form_delta_1000)) {
  summary(robustness_covid_ppml_reduced_form_delta_1000)
}


robustness_covid_ppml_reduced_form_delta_summary <- extract_covid_ppml_results_safely(
  model = robustness_covid_ppml_reduced_form_delta_1000,
  term = "iv_delta_post_1000",
  specification = "COVID exclusion PPML reduced form: delta exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "iv_delta_post_1000",
  excluded_years = "2020, 2021"
)

robustness_covid_ppml_reduced_form_delta_summary


# ============================================================
# 4. PPML benchmark without COVID years: delta exposure
# ============================================================
#
# Purpose:
#   Re-estimate the non-instrumented PPML benchmark using the alternative
#   delta-exposure treatment after excluding 2020 and 2021.
#
# Specification:
#   export_value ~ treatment_delta_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This benchmark checks whether the descriptive association between actual
#   migration exposure and exports changes when exposure is defined by the
#   2014–2016 stock change.
# ============================================================

robustness_covid_ppml_benchmark_delta_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_covid,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_covid_ppml_benchmark_delta_1000)) {
  summary(robustness_covid_ppml_benchmark_delta_1000)
}


robustness_covid_ppml_benchmark_delta_summary <- extract_covid_ppml_results_safely(
  model = robustness_covid_ppml_benchmark_delta_1000,
  term = "treatment_delta_post_1000",
  specification = "COVID exclusion PPML benchmark: delta exposure",
  sample = "Full sample",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_delta_post_1000",
  excluded_years = "2020, 2021"
)

robustness_covid_ppml_benchmark_delta_summary


# ============================================================
# 5. No-Eritrea PPML reduced form without COVID years
# ============================================================
#
# Purpose:
#   Re-estimate the stock-exposure PPML reduced form after excluding both
#   Eritrea and the COVID years.
#
# Specification:
#   export_value ~ iv_stock_2016_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This checks whether the COVID-exclusion reduced-form result is driven by
#   Eritrea.
# ============================================================

robustness_covid_ppml_reduced_form_stock_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea_no_covid,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_covid_ppml_reduced_form_stock_no_eritrea_1000)) {
  summary(robustness_covid_ppml_reduced_form_stock_no_eritrea_1000)
}


robustness_covid_ppml_reduced_form_stock_no_eritrea_summary <- extract_covid_ppml_results_safely(
  model = robustness_covid_ppml_reduced_form_stock_no_eritrea_1000,
  term = "iv_stock_2016_post_1000",
  specification = "COVID exclusion PPML reduced form: stock exposure",
  sample = "Excluding Eritrea",
  outcome_variable = "export_value",
  variable_of_interest = "iv_stock_2016_post_1000",
  excluded_years = "2020, 2021"
)

robustness_covid_ppml_reduced_form_stock_no_eritrea_summary


# ============================================================
# 6. No-Eritrea PPML benchmark without COVID years
# ============================================================
#
# Purpose:
#   Re-estimate the non-instrumented PPML benchmark after excluding both
#   Eritrea and the COVID years.
#
# Specification:
#   export_value ~ treatment_stock_2016_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This checks whether the COVID-exclusion benchmark result is driven by
#   Eritrea.
# ============================================================

robustness_covid_ppml_benchmark_stock_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~ treatment_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea_no_covid,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_covid_ppml_benchmark_stock_no_eritrea_1000)) {
  summary(robustness_covid_ppml_benchmark_stock_no_eritrea_1000)
}


robustness_covid_ppml_benchmark_stock_no_eritrea_summary <- extract_covid_ppml_results_safely(
  model = robustness_covid_ppml_benchmark_stock_no_eritrea_1000,
  term = "treatment_stock_2016_post_1000",
  specification = "COVID exclusion PPML benchmark: stock exposure",
  sample = "Excluding Eritrea",
  outcome_variable = "export_value",
  variable_of_interest = "treatment_stock_2016_post_1000",
  excluded_years = "2020, 2021"
)

robustness_covid_ppml_benchmark_stock_no_eritrea_summary


# ============================================================
# 7. Alternative outcome without COVID years: export weight RF
# ============================================================
#
# Purpose:
#   Re-estimate the COVID-exclusion PPML reduced form using export weight
#   instead of export value.
#
# Specification:
#   export_weight ~ iv_stock_2016_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This checks whether the COVID-exclusion result depends on measuring
#   exports by value rather than by physical weight.
# ============================================================

robustness_covid_ppml_reduced_form_weight_stock_1000 <- run_ppml_safely(
  formula =
    export_weight ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_covid,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_covid_ppml_reduced_form_weight_stock_1000)) {
  summary(robustness_covid_ppml_reduced_form_weight_stock_1000)
}


robustness_covid_ppml_reduced_form_weight_stock_summary <- extract_covid_ppml_results_safely(
  model = robustness_covid_ppml_reduced_form_weight_stock_1000,
  term = "iv_stock_2016_post_1000",
  specification = "COVID exclusion PPML reduced form: export weight, stock exposure",
  sample = "Full sample",
  outcome_variable = "export_weight",
  variable_of_interest = "iv_stock_2016_post_1000",
  excluded_years = "2020, 2021"
)

robustness_covid_ppml_reduced_form_weight_stock_summary


# ============================================================
# 8. Alternative outcome without COVID years: export weight benchmark
# ============================================================
#
# Purpose:
#   Re-estimate the COVID-exclusion PPML benchmark using export weight
#   instead of export value.
#
# Specification:
#   export_weight ~ treatment_stock_2016_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This checks whether the benchmark result depends on measuring exports by
#   value rather than by physical weight.
# ============================================================

robustness_covid_ppml_benchmark_weight_stock_1000 <- run_ppml_safely(
  formula =
    export_weight ~ treatment_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_covid,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(robustness_covid_ppml_benchmark_weight_stock_1000)) {
  summary(robustness_covid_ppml_benchmark_weight_stock_1000)
}


robustness_covid_ppml_benchmark_weight_stock_summary <- extract_covid_ppml_results_safely(
  model = robustness_covid_ppml_benchmark_weight_stock_1000,
  term = "treatment_stock_2016_post_1000",
  specification = "COVID exclusion PPML benchmark: export weight, stock exposure",
  sample = "Full sample",
  outcome_variable = "export_weight",
  variable_of_interest = "treatment_stock_2016_post_1000",
  excluded_years = "2020, 2021"
)

robustness_covid_ppml_benchmark_weight_stock_summary


# ============================================================
# 9. Combined COVID-exclusion results overview
# ============================================================
#
# Purpose:
#   Combine all COVID-exclusion PPML reduced-form and benchmark results into
#   one overview table.
#
# Included specifications:
#   Main stock reduced form
#   Main stock benchmark
#   Delta reduced form
#   Delta benchmark
#   No-Eritrea reduced form
#   No-Eritrea benchmark
#   Export-weight reduced form
#   Export-weight benchmark
#
# Notes:
#   This table is intended for internal comparison and documentation.
# ============================================================

robustness_covid_results_overview <- bind_rows(
  robustness_covid_ppml_reduced_form_stock_summary,
  robustness_covid_ppml_benchmark_stock_summary,
  robustness_covid_ppml_reduced_form_delta_summary,
  robustness_covid_ppml_benchmark_delta_summary,
  robustness_covid_ppml_reduced_form_stock_no_eritrea_summary,
  robustness_covid_ppml_benchmark_stock_no_eritrea_summary,
  robustness_covid_ppml_reduced_form_weight_stock_summary,
  robustness_covid_ppml_benchmark_weight_stock_summary
) %>%
  select(
    sample,
    specification,
    outcome_variable,
    variable_of_interest,
    excluded_years,
    term,
    estimate,
    std_error,
    z_statistic,
    p_value,
    pseudo_r2,
    n_obs,
    status
  )

robustness_covid_results_overview


# ============================================================
# 10. Paper-ready rounded COVID-exclusion results
# ============================================================
#
# Purpose:
#   Create a rounded version of the combined COVID-exclusion results table
#   for easier reporting and interpretation.
#
# Notes:
#   This table is not automatically formatted for publication but provides
#   paper-ready rounded values.
# ============================================================

robustness_covid_results_paper <- robustness_covid_results_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    z_statistic = round(z_statistic, 2),
    p_value = signif(p_value, 3),
    pseudo_r2 = round(pseudo_r2, 3),
    n_obs = as.integer(n_obs)
  )

robustness_covid_results_paper


# ============================================================
# 11. Paper-ready text values
# ============================================================
#
# Purpose:
#   Store the main COVID-exclusion reduced-form and benchmark results in
#   separate objects for easy use in the written results section.
#
# Main reported COVID-exclusion results:
#   COVID exclusion PPML reduced form: stock exposure
#   COVID exclusion PPML benchmark: stock exposure
#
# Sample:
#   Full sample
# ============================================================

main_robustness_covid_reduced_form <- robustness_covid_results_paper %>%
  filter(
    specification == "COVID exclusion PPML reduced form: stock exposure",
    sample == "Full sample"
  )

main_robustness_covid_benchmark <- robustness_covid_results_paper %>%
  filter(
    specification == "COVID exclusion PPML benchmark: stock exposure",
    sample == "Full sample"
  )

main_robustness_covid_reduced_form
main_robustness_covid_benchmark


# ============================================================
# 12. Save COVID-exclusion outputs
# ============================================================
#
# Purpose:
#   Save all COVID-exclusion model objects, robustness panels, summary
#   tables, diagnostics, and paper-ready text values.
#
# Notes:
#   These outputs document the COVID-year exclusion robustness check. They
#   do not replace the preferred full-sample PPML reduced-form specification.
# ============================================================

### Model objects

if (!is.null(robustness_covid_ppml_reduced_form_stock_1000)) {
  saveRDS(
    robustness_covid_ppml_reduced_form_stock_1000,
    "robustness_covid_ppml_reduced_form_stock_1000.rds"
  )
}

if (!is.null(robustness_covid_ppml_benchmark_stock_1000)) {
  saveRDS(
    robustness_covid_ppml_benchmark_stock_1000,
    "robustness_covid_ppml_benchmark_stock_1000.rds"
  )
}

if (!is.null(robustness_covid_ppml_reduced_form_delta_1000)) {
  saveRDS(
    robustness_covid_ppml_reduced_form_delta_1000,
    "robustness_covid_ppml_reduced_form_delta_1000.rds"
  )
}

if (!is.null(robustness_covid_ppml_benchmark_delta_1000)) {
  saveRDS(
    robustness_covid_ppml_benchmark_delta_1000,
    "robustness_covid_ppml_benchmark_delta_1000.rds"
  )
}

if (!is.null(robustness_covid_ppml_reduced_form_stock_no_eritrea_1000)) {
  saveRDS(
    robustness_covid_ppml_reduced_form_stock_no_eritrea_1000,
    "robustness_covid_ppml_reduced_form_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(robustness_covid_ppml_benchmark_stock_no_eritrea_1000)) {
  saveRDS(
    robustness_covid_ppml_benchmark_stock_no_eritrea_1000,
    "robustness_covid_ppml_benchmark_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(robustness_covid_ppml_reduced_form_weight_stock_1000)) {
  saveRDS(
    robustness_covid_ppml_reduced_form_weight_stock_1000,
    "robustness_covid_ppml_reduced_form_weight_stock_1000.rds"
  )
}

if (!is.null(robustness_covid_ppml_benchmark_weight_stock_1000)) {
  saveRDS(
    robustness_covid_ppml_benchmark_weight_stock_1000,
    "robustness_covid_ppml_benchmark_weight_stock_1000.rds"
  )
}


### COVID-exclusion panels

saveRDS(
  analysis_panel_no_covid,
  "analysis_panel_no_covid.rds"
)

saveRDS(
  analysis_panel_no_eritrea_no_covid,
  "analysis_panel_no_eritrea_no_covid.rds"
)


### Summary objects

saveRDS(
  robustness_covid_results_overview,
  "robustness_covid_results_overview.rds"
)

saveRDS(
  robustness_covid_results_paper,
  "robustness_covid_results_paper.rds"
)


### Diagnostics

saveRDS(
  robustness_covid_diagnostics,
  "robustness_covid_diagnostics.rds"
)

saveRDS(
  missing_robustness_covid_variables,
  "missing_robustness_covid_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_robustness_covid_reduced_form,
  "main_robustness_covid_reduced_form.rds"
)

saveRDS(
  main_robustness_covid_benchmark,
  "main_robustness_covid_benchmark.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_robustness_covid_variables,
  add_fixed_effects_if_missing,
  run_ppml_safely,
  extract_covid_ppml_results_safely
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# COVID-exclusion panels:
#   analysis_panel_no_covid
#   analysis_panel_no_eritrea_no_covid
#
# COVID-exclusion PPML reduced-form model objects:
#   robustness_covid_ppml_reduced_form_stock_1000
#   robustness_covid_ppml_reduced_form_delta_1000
#   robustness_covid_ppml_reduced_form_stock_no_eritrea_1000
#   robustness_covid_ppml_reduced_form_weight_stock_1000
#
# COVID-exclusion PPML benchmark model objects:
#   robustness_covid_ppml_benchmark_stock_1000
#   robustness_covid_ppml_benchmark_delta_1000
#   robustness_covid_ppml_benchmark_stock_no_eritrea_1000
#   robustness_covid_ppml_benchmark_weight_stock_1000
#
# Individual summary objects:
#   robustness_covid_ppml_reduced_form_stock_summary
#   robustness_covid_ppml_benchmark_stock_summary
#   robustness_covid_ppml_reduced_form_delta_summary
#   robustness_covid_ppml_benchmark_delta_summary
#   robustness_covid_ppml_reduced_form_stock_no_eritrea_summary
#   robustness_covid_ppml_benchmark_stock_no_eritrea_summary
#   robustness_covid_ppml_reduced_form_weight_stock_summary
#   robustness_covid_ppml_benchmark_weight_stock_summary
#
# Combined result tables:
#   robustness_covid_results_overview
#   robustness_covid_results_paper
#
# Diagnostics:
#   robustness_covid_diagnostics
#   missing_robustness_covid_variables
#
# Paper-ready text values:
#   main_robustness_covid_reduced_form
#   main_robustness_covid_benchmark
#
# Notes:
#   This script estimates the COVID-year exclusion robustness check.
#
#   The excluded years are:
#     2020
#     2021
#
#   The purpose is to check whether the main null result is driven by global
#   trade disruptions during the COVID-19 period.
#
#   The main COVID-exclusion reduced-form object is:
#     robustness_covid_ppml_reduced_form_stock_1000
#
#   The main COVID-exclusion benchmark object is:
#     robustness_covid_ppml_benchmark_stock_1000
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
#   The preferred main specification remains the PPML reduced form using the
#   full sample. This script is a robustness check only.
#
#   In the final write-up, refer to this section as:
#     COVID-year exclusion robustness check.
#
#   Do not describe it as:
#     main specification
#     preferred sample
#     linear IV estimate
#
#   If the coefficient remains statistically insignificant, this supports the
#   interpretation that the null result is not driven by COVID-period trade
#   disruptions in 2020 and 2021.
# ============================================================