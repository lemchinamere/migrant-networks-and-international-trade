# ============================================================
# Empirical results: Event study
# Dynamic reduced-form specification
# ============================================================
#
# Purpose:
#   Estimate dynamic reduced-form associations between later Königstein-
#   predicted exposure and exports before and after the 2015/16 refugee
#   shock.
#
# Main idea:
#   The main instrument used in the baseline specification is a post-period
#   interaction:
#
#     iv_stock_2016_post_1000
#
#   This variable is mechanically zero before the post period and is
#   therefore not suitable for plotting year-by-year dynamics directly.
#   For the event-study diagnostic, the code recovers a pair-level future
#   exposure intensity:
#
#     future_iv_stock_2016_1000 =
#       max(iv_stock_2016_post_1000)
#       within federal_state × origin_country
#
#   This future exposure intensity is then interacted with year dummies.
#
# Main event-study regression:
#
#   export_value =
#     sum_t beta_t * 1[year = t] × future_iv_stock_2016_1000
#     + federal_state × origin_country fixed effects
#     + federal_state × year fixed effects
#     + origin_country × year fixed effects
#     + error
#
# Reference year:
#   2014
#
# Interpretation:
#   The coefficients show whether federal_state × origin_country pairs with
#   higher later Königstein-predicted exposure experienced different export
#   outcomes in each year relative to 2014.
#
#   Pre-shock coefficients are useful as a descriptive check of whether
#   high-exposure and low-exposure pairs already followed different export
#   patterns before the refugee inflow.
#
#   Post-shock coefficients describe whether predicted exposure is followed
#   by differential export outcomes after the 2015/16 shock.
#
# Main outcome:
#   export_value
#
# Main exposure intensity:
#   future_iv_stock_2016_1000
#
# Estimator:
#   PPML / fepois for export_value
#
# Robustness:
#   Linear / feols for log_export_value
#   2014 Königstein key as alternative predicted exposure
#   Actual future exposure as descriptive robustness
#   No-Eritrea sample
#
# Fixed effects:
#   federal_state × origin_country
#   federal_state × year
#   origin_country × year
#
# Standard errors:
#   Clustered at the federal_state × origin_country level.
#
# Important caveat:
#   This event study is a dynamic reduced-form diagnostic. It should not be
#   interpreted as a separate causal IV event study or as a 2SLS event-study
#   estimate.
#
# Output objects:
#   ppml_event_study_iv_stock_1000
#   linear_event_study_iv_stock_1000
#   ppml_event_study_iv_stock_k14_1000
#   ppml_event_study_iv_stock_no_eritrea_1000
#   ppml_event_study_treatment_stock_1000
#   event_study_coefficients_overview
#   event_study_coefficients_paper
#   event_study_plot_data_main
#   event_study_plot_data_all
#   event_study_plot_main
#   event_study_results_overview
#   event_study_results_paper
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
library(stringr)
library(ggplot2)


# ============================================================
# Load required panels
# ============================================================
#
# Purpose:
#   Load the active analysis panels required for the event-study
#   specifications.
#
# Panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Notes:
#   analysis_panel is used for the main full-sample event study and for the
#   descriptive actual-exposure event study.
#
#   analysis_panel_no_eritrea is used for the no-Eritrea robustness event
#   study.
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
#   required by the event-study specifications.
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
#   for the event-study specifications.
#
# Variables checked:
#   Panel identifiers, outcome variables, treatment variables, IV variables,
#   alternative Königstein-key IVs, and fixed effects.
#
# Interpretation:
#   Missing variables indicate that an earlier data-construction or rescaling
#   script must be rerun before estimating the event-study specifications.
#
# Notes:
#   This check is run after defensive fixed-effect reconstruction so that it
#   reflects the panels in their final usable form.
# ============================================================

required_event_study_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "export_value",
  "log_export_value",
  "treatment_stock_2016_post_1000",
  "iv_stock_2016_post_1000",
  "iv_stock_2016_post_k14_1000",
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)


missing_event_study_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_event_study_variables,
    present = required_event_study_variables %in% names(analysis_panel)
  ),
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_event_study_variables,
    present = required_event_study_variables %in% names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_event_study_variables


# ============================================================
# Construct future exposure intensities
# ============================================================
#
# Purpose:
#   Construct pair-level future exposure intensities for dynamic event-study
#   specifications.
#
# Constructed variables:
#   future_iv_stock_2016_1000
#   future_iv_stock_2016_k14_1000
#   future_treatment_stock_2016_1000
#
# Logic:
#   The post-period interaction variables are equal to zero before the
#   post period. To estimate year-by-year event-study coefficients, the code
#   recovers the pair-level exposure intensity by taking the maximum value
#   within each federal_state × origin_country pair.
#
# Interpretation:
#   future_iv_stock_2016_1000 captures the later predicted exposure intensity
#   assigned to a federal_state × origin_country pair. It is time-invariant
#   within the pair and can therefore be interacted with year dummies.
#
# Notes:
#   The actual-exposure version is included only as a descriptive robustness
#   check because actual settlement may be endogenous.
# ============================================================

add_future_exposure_intensities <- function(data) {
  data %>%
    group_by(
      federal_state,
      origin_country
    ) %>%
    mutate(
      future_iv_stock_2016_1000 = max(
        iv_stock_2016_post_1000,
        na.rm = TRUE
      ),
      
      future_iv_stock_2016_k14_1000 = max(
        iv_stock_2016_post_k14_1000,
        na.rm = TRUE
      ),
      
      future_treatment_stock_2016_1000 = max(
        treatment_stock_2016_post_1000,
        na.rm = TRUE
      )
    ) %>%
    ungroup()
}

analysis_panel <- add_future_exposure_intensities(
  analysis_panel
)

analysis_panel_no_eritrea <- add_future_exposure_intensities(
  analysis_panel_no_eritrea
)


# ============================================================
# Event-study exposure diagnostics
# ============================================================
#
# Purpose:
#   Summarise the distribution of the constructed future exposure
#   intensities.
#
# Unit of observation:
#   federal_state × origin_country pair
#
# Checks:
#   Number of pairs, mean, standard deviation, minimum, and maximum of the
#   predicted and actual future exposure variables.
#
# Interpretation:
#   These diagnostics verify that the event-study exposure variables contain
#   cross-pair variation before running the dynamic specifications.
# ============================================================

event_study_exposure_diagnostics <- analysis_panel %>%
  distinct(
    federal_state,
    origin_country,
    future_iv_stock_2016_1000,
    future_iv_stock_2016_k14_1000,
    future_treatment_stock_2016_1000
  ) %>%
  summarise(
    n_pairs = n(),
    
    mean_future_iv_stock_2016_1000 = mean(
      future_iv_stock_2016_1000,
      na.rm = TRUE
    ),
    
    sd_future_iv_stock_2016_1000 = sd(
      future_iv_stock_2016_1000,
      na.rm = TRUE
    ),
    
    min_future_iv_stock_2016_1000 = min(
      future_iv_stock_2016_1000,
      na.rm = TRUE
    ),
    
    max_future_iv_stock_2016_1000 = max(
      future_iv_stock_2016_1000,
      na.rm = TRUE
    ),
    
    mean_future_iv_stock_2016_k14_1000 = mean(
      future_iv_stock_2016_k14_1000,
      na.rm = TRUE
    ),
    
    sd_future_iv_stock_2016_k14_1000 = sd(
      future_iv_stock_2016_k14_1000,
      na.rm = TRUE
    ),
    
    min_future_iv_stock_2016_k14_1000 = min(
      future_iv_stock_2016_k14_1000,
      na.rm = TRUE
    ),
    
    max_future_iv_stock_2016_k14_1000 = max(
      future_iv_stock_2016_k14_1000,
      na.rm = TRUE
    ),
    
    mean_future_treatment_stock_2016_1000 = mean(
      future_treatment_stock_2016_1000,
      na.rm = TRUE
    ),
    
    sd_future_treatment_stock_2016_1000 = sd(
      future_treatment_stock_2016_1000,
      na.rm = TRUE
    ),
    
    min_future_treatment_stock_2016_1000 = min(
      future_treatment_stock_2016_1000,
      na.rm = TRUE
    ),
    
    max_future_treatment_stock_2016_1000 = max(
      future_treatment_stock_2016_1000,
      na.rm = TRUE
    )
  )

event_study_exposure_diagnostics


# ============================================================
# Helper function: run PPML safely
# ============================================================
#
# Purpose:
#   Estimate PPML event-study models while preventing the full script from
#   stopping if a specification is not estimable.
#
# Notes:
#   If estimation fails, the function returns NULL and the corresponding
#   coefficient table records the model as not estimable.
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
      message("PPML event-study model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: run feols safely
# ============================================================
#
# Purpose:
#   Estimate linear event-study models while preventing the full script from
#   stopping if a specification is not estimable.
#
# Notes:
#   If estimation fails, the function returns NULL and the corresponding
#   coefficient table records the model as not estimable.
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
      message("Linear event-study model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: extract event-study coefficients safely
# ============================================================
#
# Purpose:
#   Extract year-by-year event-study coefficients from PPML or linear
#   event-study model objects.
#
# Logic:
#   The function searches the coefficient table for terms containing the
#   exposure variable, extracts the year associated with each interaction
#   term, and returns estimates, standard errors, test statistics, p-values,
#   sample size, and model status.
#
# Notes:
#   If the model is not estimable, or if all event-study terms are dropped,
#   the function returns a diagnostic row with missing coefficient values.
# ============================================================

extract_event_study_coefficients_safely <- function(
    model,
    specification,
    sample,
    estimator,
    outcome_variable,
    exposure_variable,
    reference_year = 2014
) {
  if (is.null(model)) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        estimator = estimator,
        outcome_variable = outcome_variable,
        exposure_variable = exposure_variable,
        reference_year = reference_year,
        term = NA_character_,
        year = NA_integer_,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
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
  
  coefficient_terms <- rownames(coefficient_table)
  
  event_study_terms <- coefficient_terms[
    str_detect(
      coefficient_terms,
      fixed(exposure_variable)
    )
  ]
  
  if (length(event_study_terms) == 0) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        estimator = estimator,
        outcome_variable = outcome_variable,
        exposure_variable = exposure_variable,
        reference_year = reference_year,
        term = NA_character_,
        year = NA_integer_,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
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
    exposure_variable = exposure_variable,
    reference_year = reference_year,
    term = event_study_terms,
    year = as.integer(
      str_extract(
        event_study_terms,
        "20[0-9]{2}"
      )
    ),
    estimate = coefficient_table[event_study_terms, "Estimate"],
    std_error = coefficient_table[event_study_terms, "Std. Error"],
    statistic = if (!is.na(statistic_column)) {
      coefficient_table[event_study_terms, statistic_column]
    } else {
      NA_real_
    },
    p_value = if (!is.na(p_value_column)) {
      coefficient_table[event_study_terms, p_value_column]
    } else {
      NA_real_
    },
    n_obs = nobs(model),
    status = "estimated"
  ) %>%
    arrange(
      year
    )
}


# ============================================================
# Helper function: event-study plot data
# ============================================================
#
# Purpose:
#   Construct plot-ready event-study data by adding confidence intervals and
#   inserting the reference year with coefficient equal to zero.
#
# Confidence intervals:
#   95 percent confidence interval
#   estimate ± 1.96 × standard error
#
# Reference year:
#   2014
#
# Notes:
#   The reference-year row is inserted manually because fixest omits the
#   reference category from the estimated coefficient table.
# ============================================================

make_event_study_plot_data <- function(event_study_coefficients) {
  event_study_coefficients %>%
    filter(
      status == "estimated"
    ) %>%
    mutate(
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error
    ) %>%
    bind_rows(
      event_study_coefficients %>%
        filter(
          status == "estimated"
        ) %>%
        distinct(
          sample,
          specification,
          estimator,
          outcome_variable,
          exposure_variable,
          reference_year,
          n_obs,
          status
        ) %>%
        mutate(
          term = NA_character_,
          year = reference_year,
          estimate = 0,
          std_error = NA_real_,
          statistic = NA_real_,
          p_value = NA_real_,
          conf_low = 0,
          conf_high = 0
        )
    ) %>%
    arrange(
      year
    )
}


# ============================================================
# 1. Main PPML event study: predicted future exposure
# ============================================================
#
# Purpose:
#   Estimate the main dynamic reduced-form event-study specification using
#   Königstein-predicted future stock exposure.
#
# Specification:
#   export_value ~ i(year, future_iv_stock_2016_1000, ref = 2014)
#   + three-way fixed effects
#
# Reference year:
#   2014
#
# Interpretation:
#   Coefficients show whether pairs with higher later predicted exposure had
#   different export values in each year relative to 2014.
# ============================================================

ppml_event_study_iv_stock_1000 <- run_ppml_safely(
  formula =
    export_value ~
    i(year, future_iv_stock_2016_1000, ref = 2014) |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_event_study_iv_stock_1000)) {
  summary(ppml_event_study_iv_stock_1000)
}


ppml_event_study_iv_stock_coefficients <- extract_event_study_coefficients_safely(
  model = ppml_event_study_iv_stock_1000,
  specification = "PPML event study: predicted future stock exposure",
  sample = "Full sample",
  estimator = "PPML / fepois",
  outcome_variable = "export_value",
  exposure_variable = "future_iv_stock_2016_1000",
  reference_year = 2014
)

ppml_event_study_iv_stock_coefficients


# ============================================================
# 2. Linear event study: predicted future exposure
# ============================================================
#
# Purpose:
#   Estimate a linear event-study robustness specification using log export
#   values and Königstein-predicted future stock exposure.
#
# Specification:
#   log_export_value ~ i(year, future_iv_stock_2016_1000, ref = 2014)
#   + three-way fixed effects
#
# Interpretation:
#   This provides a linear comparison to the main PPML event-study
#   diagnostic.
# ============================================================

linear_event_study_iv_stock_1000 <- run_feols_safely(
  formula =
    log_export_value ~
    i(year, future_iv_stock_2016_1000, ref = 2014) |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(linear_event_study_iv_stock_1000)) {
  summary(linear_event_study_iv_stock_1000)
}


linear_event_study_iv_stock_coefficients <- extract_event_study_coefficients_safely(
  model = linear_event_study_iv_stock_1000,
  specification = "Linear event study: predicted future stock exposure",
  sample = "Full sample",
  estimator = "feols",
  outcome_variable = "log_export_value",
  exposure_variable = "future_iv_stock_2016_1000",
  reference_year = 2014
)

linear_event_study_iv_stock_coefficients


# ============================================================
# 3. PPML event-study robustness: 2014 Königstein key
# ============================================================
#
# Purpose:
#   Re-estimate the PPML event-study diagnostic using the strictly pre-shock
#   2014 Königstein key as the basis for predicted future exposure.
#
# Specification:
#   export_value ~ i(year, future_iv_stock_2016_k14_1000, ref = 2014)
#   + three-way fixed effects
#
# Interpretation:
#   This checks whether the dynamic reduced-form pattern depends on using
#   the main 2015–2016 Königstein average rather than the 2014 key.
# ============================================================

ppml_event_study_iv_stock_k14_1000 <- run_ppml_safely(
  formula =
    export_value ~
    i(year, future_iv_stock_2016_k14_1000, ref = 2014) |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_event_study_iv_stock_k14_1000)) {
  summary(ppml_event_study_iv_stock_k14_1000)
}


ppml_event_study_iv_stock_k14_coefficients <- extract_event_study_coefficients_safely(
  model = ppml_event_study_iv_stock_k14_1000,
  specification = "PPML event study: predicted future stock exposure, 2014 key",
  sample = "Full sample",
  estimator = "PPML / fepois",
  outcome_variable = "export_value",
  exposure_variable = "future_iv_stock_2016_k14_1000",
  reference_year = 2014
)

ppml_event_study_iv_stock_k14_coefficients


# ============================================================
# 4. No-Eritrea PPML event study: predicted future exposure
# ============================================================
#
# Purpose:
#   Re-estimate the main PPML event-study diagnostic after excluding
#   Eritrea.
#
# Specification:
#   export_value ~ i(year, future_iv_stock_2016_1000, ref = 2014)
#   + three-way fixed effects
#
# Interpretation:
#   This checks whether the dynamic reduced-form pattern is driven by
#   Eritrea.
# ============================================================

ppml_event_study_iv_stock_no_eritrea_1000 <- run_ppml_safely(
  formula =
    export_value ~
    i(year, future_iv_stock_2016_1000, ref = 2014) |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_event_study_iv_stock_no_eritrea_1000)) {
  summary(ppml_event_study_iv_stock_no_eritrea_1000)
}


ppml_event_study_iv_stock_no_eritrea_coefficients <- extract_event_study_coefficients_safely(
  model = ppml_event_study_iv_stock_no_eritrea_1000,
  specification = "PPML event study: predicted future stock exposure",
  sample = "Excluding Eritrea",
  estimator = "PPML / fepois",
  outcome_variable = "export_value",
  exposure_variable = "future_iv_stock_2016_1000",
  reference_year = 2014
)

ppml_event_study_iv_stock_no_eritrea_coefficients


# ============================================================
# 5. Descriptive PPML event study: actual future exposure
# ============================================================
#
# Purpose:
#   Estimate a descriptive PPML event-study specification using actual
#   future stock exposure.
#
# Specification:
#   export_value ~ i(year, future_treatment_stock_2016_1000, ref = 2014)
#   + three-way fixed effects
#
# Interpretation:
#   This is not the main event study because actual settlement may be
#   endogenous. It is kept only as a descriptive robustness check.
# ============================================================

ppml_event_study_treatment_stock_1000 <- run_ppml_safely(
  formula =
    export_value ~
    i(year, future_treatment_stock_2016_1000, ref = 2014) |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(ppml_event_study_treatment_stock_1000)) {
  summary(ppml_event_study_treatment_stock_1000)
}


ppml_event_study_treatment_stock_coefficients <- extract_event_study_coefficients_safely(
  model = ppml_event_study_treatment_stock_1000,
  specification = "PPML event study: actual future stock exposure",
  sample = "Full sample",
  estimator = "PPML / fepois",
  outcome_variable = "export_value",
  exposure_variable = "future_treatment_stock_2016_1000",
  reference_year = 2014
)

ppml_event_study_treatment_stock_coefficients


# ============================================================
# 6. Combined event-study coefficient results
# ============================================================
#
# Purpose:
#   Combine all event-study coefficient tables into one overview table.
#
# Included specifications:
#   Main PPML predicted-exposure event study
#   Linear predicted-exposure event study
#   PPML 2014-key predicted-exposure event study
#   PPML no-Eritrea predicted-exposure event study
#   PPML descriptive actual-exposure event study
#
# Notes:
#   This table is intended for internal comparison and documentation.
# ============================================================

event_study_coefficients_overview <- bind_rows(
  ppml_event_study_iv_stock_coefficients,
  linear_event_study_iv_stock_coefficients,
  ppml_event_study_iv_stock_k14_coefficients,
  ppml_event_study_iv_stock_no_eritrea_coefficients,
  ppml_event_study_treatment_stock_coefficients
) %>%
  select(
    sample,
    specification,
    estimator,
    outcome_variable,
    exposure_variable,
    reference_year,
    year,
    term,
    estimate,
    std_error,
    statistic,
    p_value,
    n_obs,
    status
  )

event_study_coefficients_overview


# ============================================================
# 7. Paper-ready rounded event-study coefficients
# ============================================================
#
# Purpose:
#   Create a rounded version of the combined event-study coefficient table
#   for easier reporting and interpretation.
#
# Notes:
#   This table is not automatically formatted for publication but provides
#   paper-ready rounded values.
# ============================================================

event_study_coefficients_paper <- event_study_coefficients_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    statistic = round(statistic, 2),
    p_value = signif(p_value, 3),
    n_obs = as.integer(n_obs)
  )

event_study_coefficients_paper


# ============================================================
# 8. Event-study plot data
# ============================================================
#
# Purpose:
#   Construct plot-ready data for the main event-study figure and for all
#   event-study specifications.
#
# Main plot data:
#   event_study_plot_data_main
#
# Combined plot data:
#   event_study_plot_data_all
#
# Notes:
#   The reference year 2014 is added manually with coefficient equal to zero.
# ============================================================

event_study_plot_data_main <- make_event_study_plot_data(
  ppml_event_study_iv_stock_coefficients
)

event_study_plot_data_main


event_study_plot_data_all <- bind_rows(
  make_event_study_plot_data(ppml_event_study_iv_stock_coefficients),
  make_event_study_plot_data(linear_event_study_iv_stock_coefficients),
  make_event_study_plot_data(ppml_event_study_iv_stock_k14_coefficients),
  make_event_study_plot_data(ppml_event_study_iv_stock_no_eritrea_coefficients),
  make_event_study_plot_data(ppml_event_study_treatment_stock_coefficients)
)

event_study_plot_data_all


# ============================================================
# 9. Main event-study plot
# ============================================================
#
# Purpose:
#   Plot the main PPML dynamic reduced-form event-study coefficients.
#
# Figure:
#   Coefficient on predicted future exposure by year
#
# Reference year:
#   2014
#
# Additional vertical marker:
#   2016, the main exposure stock year used in the baseline instrument.
#
# Notes:
#   The plot is a descriptive dynamic diagnostic. It should be interpreted
#   together with the coefficient table and not as the main causal estimate.
# ============================================================

event_study_plot_main <- ggplot(
  event_study_plot_data_main,
  aes(
    x = year,
    y = estimate
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = 2014,
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = 2016,
    linetype = "dotted",
    linewidth = 0.3
  ) +
  geom_errorbar(
    aes(
      ymin = conf_low,
      ymax = conf_high
    ),
    width = 0.15
  ) +
  geom_point() +
  geom_line() +
  labs(
    title = "Event study: predicted refugee exposure and exports",
    subtitle = "PPML reduced-form event study; reference year = 2014",
    x = "Year",
    y = "Coefficient on predicted future exposure"
  ) +
  theme_minimal()

event_study_plot_main


ggsave(
  filename = "event_study_plot_main.png",
  plot = event_study_plot_main,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# 10. Combined event-study results object
# ============================================================
#
# Purpose:
#   Store coefficient results and exposure diagnostics together in combined
#   list objects.
#
# Objects:
#   event_study_results_overview
#   event_study_results_paper
#
# Notes:
#   The overview object contains the full coefficient table and exposure
#   diagnostics. The paper object contains the rounded coefficient table.
# ============================================================

event_study_results_overview <- list(
  coefficient_results = event_study_coefficients_overview,
  exposure_diagnostics = event_study_exposure_diagnostics
)

event_study_results_paper <- list(
  coefficient_results = event_study_coefficients_paper
)

event_study_results_overview
event_study_results_paper


# ============================================================
# 11. Paper-ready text values
# ============================================================
#
# Purpose:
#   Store the main PPML event-study pre-period and post-period coefficients
#   in separate objects for easy use in the written results section.
#
# Main specification:
#   PPML event study: predicted future stock exposure
#
# Sample:
#   Full sample
#
# Interpretation:
#   Pre-period coefficients describe differences before the refugee shock.
#   Post-period coefficients describe differences after the beginning of the
#   refugee-shock period.
# ============================================================

main_event_study_pre_coefficients <- event_study_coefficients_paper %>%
  filter(
    specification == "PPML event study: predicted future stock exposure",
    sample == "Full sample",
    year < 2015
  )

main_event_study_post_coefficients <- event_study_coefficients_paper %>%
  filter(
    specification == "PPML event study: predicted future stock exposure",
    sample == "Full sample",
    year >= 2015
  )

main_event_study_pre_coefficients
main_event_study_post_coefficients


# ============================================================
# 12. Save event-study outputs
# ============================================================
#
# Purpose:
#   Save all event-study model objects, coefficient tables, plot data, plot
#   objects, diagnostics, and paper-ready text values.
#
# Notes:
#   These outputs document the dynamic reduced-form event-study diagnostic.
#   They should not replace the preferred PPML reduced-form specification.
# ============================================================

### Model objects

if (!is.null(ppml_event_study_iv_stock_1000)) {
  saveRDS(
    ppml_event_study_iv_stock_1000,
    "ppml_event_study_iv_stock_1000.rds"
  )
}

if (!is.null(linear_event_study_iv_stock_1000)) {
  saveRDS(
    linear_event_study_iv_stock_1000,
    "linear_event_study_iv_stock_1000.rds"
  )
}

if (!is.null(ppml_event_study_iv_stock_k14_1000)) {
  saveRDS(
    ppml_event_study_iv_stock_k14_1000,
    "ppml_event_study_iv_stock_k14_1000.rds"
  )
}

if (!is.null(ppml_event_study_iv_stock_no_eritrea_1000)) {
  saveRDS(
    ppml_event_study_iv_stock_no_eritrea_1000,
    "ppml_event_study_iv_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(ppml_event_study_treatment_stock_1000)) {
  saveRDS(
    ppml_event_study_treatment_stock_1000,
    "ppml_event_study_treatment_stock_1000.rds"
  )
}


### Coefficient results

saveRDS(
  event_study_coefficients_overview,
  "event_study_coefficients_overview.rds"
)

saveRDS(
  event_study_coefficients_paper,
  "event_study_coefficients_paper.rds"
)


### Plot data and plot

saveRDS(
  event_study_plot_data_main,
  "event_study_plot_data_main.rds"
)

saveRDS(
  event_study_plot_data_all,
  "event_study_plot_data_all.rds"
)

saveRDS(
  event_study_plot_main,
  "event_study_plot_main.rds"
)


### Combined results

saveRDS(
  event_study_results_overview,
  "event_study_results_overview.rds"
)

saveRDS(
  event_study_results_paper,
  "event_study_results_paper.rds"
)


### Diagnostics

saveRDS(
  event_study_exposure_diagnostics,
  "event_study_exposure_diagnostics.rds"
)

saveRDS(
  missing_event_study_variables,
  "missing_event_study_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_event_study_pre_coefficients,
  "main_event_study_pre_coefficients.rds"
)

saveRDS(
  main_event_study_post_coefficients,
  "main_event_study_post_coefficients.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_event_study_variables,
  add_fixed_effects_if_missing,
  add_future_exposure_intensities,
  run_ppml_safely,
  run_feols_safely,
  extract_event_study_coefficients_safely,
  make_event_study_plot_data
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base panels with future exposure intensities:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Main event-study model objects:
#   ppml_event_study_iv_stock_1000
#   linear_event_study_iv_stock_1000
#
# Robustness and descriptive event-study model objects:
#   ppml_event_study_iv_stock_k14_1000
#   ppml_event_study_iv_stock_no_eritrea_1000
#   ppml_event_study_treatment_stock_1000
#
# Individual coefficient tables:
#   ppml_event_study_iv_stock_coefficients
#   linear_event_study_iv_stock_coefficients
#   ppml_event_study_iv_stock_k14_coefficients
#   ppml_event_study_iv_stock_no_eritrea_coefficients
#   ppml_event_study_treatment_stock_coefficients
#
# Combined coefficient tables:
#   event_study_coefficients_overview
#   event_study_coefficients_paper
#
# Plot data and plot objects:
#   event_study_plot_data_main
#   event_study_plot_data_all
#   event_study_plot_main
#
# Combined result objects:
#   event_study_results_overview
#   event_study_results_paper
#
# Diagnostics:
#   event_study_exposure_diagnostics
#   missing_event_study_variables
#
# Paper-ready text values:
#   main_event_study_pre_coefficients
#   main_event_study_post_coefficients
#
# Notes:
#   This script estimates dynamic reduced-form event-study specifications.
#
#   The main event-study object is:
#     ppml_event_study_iv_stock_1000
#
#   The main exposure variable is:
#     future_iv_stock_2016_1000
#
#   This exposure is constructed as the federal_state × origin_country
#   pair-level maximum of the post-period predicted exposure interaction.
#   It is then interacted with year dummies, using 2014 as the reference
#   year.
#
#   The event study should be interpreted as a dynamic reduced-form
#   diagnostic, not as a separate causal IV estimate.
#
#   The PPML event study does not by itself establish a causal post-shock
#   export response. In particular, if some pre-period coefficients differ
#   from zero, the figure should be presented descriptively and not as the
#   main identifying-assumption test.
#
#   The main pre-trend diagnostic remains the BHJ-style pre-shock change
#   regression.
#
#   In the final write-up, refer to this section as:
#     dynamic reduced-form event-study diagnostic
#
#   Do not describe it as:
#     main causal event-study estimate
#     2SLS event study
#     IV event study in the strict linear 2SLS sense
# ============================================================