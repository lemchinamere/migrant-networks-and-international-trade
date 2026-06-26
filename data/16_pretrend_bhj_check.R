# ============================================================
# Empirical results: Pre-trend test
# Borusyak-Hull-Jaravel-style pre-shock change test
# ============================================================
#
# Purpose:
#   Test whether federal-state-origin pairs with higher later predicted
#   exposure already experienced different export changes before the
#   2015/16 refugee shock.
#
# Script type:
#   Regression / analysis script
#
# Workflow logic:
#   This script loads already constructed .rds panels and estimates
#   cross-sectional pre-shock change regressions.
#
#   It does not reconstruct the analysis panel, treatment variables,
#   instruments, fixed effects, or _1000 variables from raw data.
#
# Supervisor recommendation:
#   Regress the pre-shock change in outcomes on the IV.
#
#   Since the IV is cross-sectional and constant within a
#   federal_state × origin_country pair, pair fixed effects cannot be used.
#   Pair fixed effects would absorb the IV mechanically.
#
# Main idea:
#
#   1. Construct future predicted exposure:
#
#      future_iv_stock_2016_1000 =
#        max(iv_stock_2016_post_1000)
#        within federal_state × origin_country
#
#   2. Construct pre-shock outcome change:
#
#      delta_log_export_value_2010_2014 =
#        log_export_value_2014 - log_export_value_2010
#
#   3. Regress the pre-shock outcome change on future predicted exposure:
#
#      delta_log_export_value_2010_2014 =
#        beta * future_iv_stock_2016_1000
#        + federal_state fixed effects
#        + origin_country fixed effects
#        + error
#
# Interpretation:
#   If beta is close to zero and statistically insignificant, there is no
#   evidence that later high-exposure state-origin pairs already had
#   differential pre-shock export growth.
#
# Main outcome:
#   delta_log_export_value
#
# Main exposure variable:
#   future_iv_stock_2016_1000
#
# Estimator:
#   Linear cross-sectional regression using feols.
#
# Fixed effects:
#   federal_state fixed effects
#   origin_country fixed effects
#
# Important:
#   Do not include federal_state × origin_country fixed effects here.
#   The exposure variable varies at the pair level and would be absorbed.
#
# Standard errors:
#   Clustered at the federal_state level in the cross-sectional baseline.
#
# Output objects:
#   pretrend_bhj_stock_2010_2014
#   pretrend_bhj_delta_2010_2014
#   pretrend_bhj_stock_2011_2014
#   pretrend_bhj_stock_2010_2013
#   pretrend_bhj_stock_no_eritrea_2010_2014
#   pretrend_bhj_results_overview
#   pretrend_bhj_results_paper
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
      "Please rerun the corresponding data-cleaning / panel-construction scripts before running this pre-trend script."
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
# ============================================================

analysis_panel <- readRDS(
  "analysis_panel.rds"
)

analysis_panel_no_eritrea <- readRDS(
  "analysis_panel_no_eritrea.rds"
)


# ============================================================
# Required-variable check
# ============================================================
#
# Purpose:
#   Check whether the loaded panels contain all variables required for the
#   BHJ-style pre-shock change regressions.
#
# Notes:
#   If variables are missing, rerun the relevant data-construction and
#   rescaling scripts.
# ============================================================

required_pretrend_bhj_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "export_value",
  "log_export_value",
  "treatment_stock_2016_post_1000",
  "treatment_delta_post_1000",
  "iv_stock_2016_post_1000",
  "iv_delta_post_1000",
  "iv_stock_2016_post_k14_1000"
)


missing_pretrend_bhj_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_pretrend_bhj_variables,
    present = required_pretrend_bhj_variables %in% names(analysis_panel)
  ),
  
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_pretrend_bhj_variables,
    present = required_pretrend_bhj_variables %in%
      names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_pretrend_bhj_variables

if (nrow(missing_pretrend_bhj_variables) > 0) {
  stop(
    "At least one required variable for the BHJ-style pre-trend test is missing. Inspect missing_pretrend_bhj_variables."
  )
}


# ============================================================
# Helper function: safe maximum
# ============================================================
#
# Purpose:
#   Compute a maximum within a group without producing misleading values if
#   all observations are missing.
#
# Reason:
#   max(x, na.rm = TRUE) returns -Inf if all values are NA.
#
#   For future exposure intensities, all-missing groups should instead remain
#   NA.
# ============================================================

safe_max <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(
      x,
      na.rm = TRUE
    )
  }
}


# ============================================================
# Construct future exposure intensities
# ============================================================
#
# Purpose:
#   Recover pair-level future exposure intensities from post-period
#   interaction variables.
#
# Logic:
#   The post-period interaction variables are zero before the post period.
#   For the pre-trend test, the future pair-level exposure intensity is
#   recovered by taking the maximum value within each
#   federal_state × origin_country pair.
#
# Notes:
#   This creates additional variables inside the loaded analysis panels.
#   It does not rebuild treatment or IV variables from raw data.
# ============================================================

add_future_exposure_intensities <- function(data) {
  data %>%
    group_by(
      federal_state,
      origin_country
    ) %>%
    mutate(
      future_iv_stock_2016_1000 = safe_max(
        iv_stock_2016_post_1000
      ),
      
      future_iv_delta_1000 = safe_max(
        iv_delta_post_1000
      ),
      
      future_iv_stock_2016_k14_1000 = safe_max(
        iv_stock_2016_post_k14_1000
      ),
      
      future_treatment_stock_2016_1000 = safe_max(
        treatment_stock_2016_post_1000
      ),
      
      future_treatment_delta_1000 = safe_max(
        treatment_delta_post_1000
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
# Helper function: construct pre-shock change panel
# ============================================================
#
# Purpose:
#   Collapse the panel to one pre-shock change observation per
#   federal_state × origin_country pair.
#
# Inputs:
#   data
#   start_year
#   end_year
#
# Constructed variables:
#   log_export_value_start
#   log_export_value_end
#   export_value_start
#   export_value_end
#   delta_log_export_value
#   delta_export_value
#   export_growth_rate
#
# Notes:
#   The function uses an inner join between start-year and end-year
#   observations. Pairs missing either endpoint are excluded from the
#   corresponding pre-trend change panel.
# ============================================================

make_pretrend_change_panel <- function(
    data,
    start_year,
    end_year
) {
  start_data <- data %>%
    filter(
      year == start_year
    ) %>%
    select(
      federal_state,
      origin_country,
      log_export_value_start = log_export_value,
      export_value_start = export_value,
      future_iv_stock_2016_1000,
      future_iv_delta_1000,
      future_iv_stock_2016_k14_1000,
      future_treatment_stock_2016_1000,
      future_treatment_delta_1000
    )
  
  end_data <- data %>%
    filter(
      year == end_year
    ) %>%
    select(
      federal_state,
      origin_country,
      log_export_value_end = log_export_value,
      export_value_end = export_value
    )
  
  start_data %>%
    inner_join(
      end_data,
      by = c(
        "federal_state",
        "origin_country"
      )
    ) %>%
    mutate(
      start_year = start_year,
      end_year = end_year,
      
      delta_log_export_value =
        log_export_value_end - log_export_value_start,
      
      delta_export_value =
        export_value_end - export_value_start,
      
      export_growth_rate = if_else(
        export_value_start > 0,
        (export_value_end - export_value_start) / export_value_start,
        NA_real_
      )
    )
}


# ============================================================
# Construct pre-trend change panels
# ============================================================
#
# Main window:
#   2010–2014
#
# Robustness windows:
#   2011–2014
#   2010–2013
#
# These windows use only pre-shock years.
# ============================================================

pretrend_bhj_panel_2010_2014 <- make_pretrend_change_panel(
  data = analysis_panel,
  start_year = 2010,
  end_year = 2014
)

pretrend_bhj_panel_2011_2014 <- make_pretrend_change_panel(
  data = analysis_panel,
  start_year = 2011,
  end_year = 2014
)

pretrend_bhj_panel_2010_2013 <- make_pretrend_change_panel(
  data = analysis_panel,
  start_year = 2010,
  end_year = 2013
)

pretrend_bhj_panel_no_eritrea_2010_2014 <- make_pretrend_change_panel(
  data = analysis_panel_no_eritrea,
  start_year = 2010,
  end_year = 2014
)


# ============================================================
# Basic diagnostics
# ============================================================
#
# Purpose:
#   Summarise the pre-trend change panels before running regressions.
#
# Checks:
#   Number of state-origin pairs
#   Mean and standard deviation of pre-shock export changes
#   Mean, standard deviation, minimum, and maximum of future predicted
#   exposure
# ============================================================

pretrend_bhj_panel_diagnostics <- bind_rows(
  pretrend_bhj_panel_2010_2014 %>%
    mutate(
      sample = "Full sample",
      window = "2010–2014"
    ),
  
  pretrend_bhj_panel_2011_2014 %>%
    mutate(
      sample = "Full sample",
      window = "2011–2014"
    ),
  
  pretrend_bhj_panel_2010_2013 %>%
    mutate(
      sample = "Full sample",
      window = "2010–2013"
    ),
  
  pretrend_bhj_panel_no_eritrea_2010_2014 %>%
    mutate(
      sample = "Excluding Eritrea",
      window = "2010–2014"
    )
) %>%
  group_by(
    sample,
    window
  ) %>%
  summarise(
    n_pairs = n(),
    
    mean_delta_log_export_value = mean(
      delta_log_export_value,
      na.rm = TRUE
    ),
    
    sd_delta_log_export_value = sd(
      delta_log_export_value,
      na.rm = TRUE
    ),
    
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
    
    .groups = "drop"
  )

pretrend_bhj_panel_diagnostics


# ============================================================
# Helper function: run feols safely
# ============================================================
#
# Purpose:
#   Estimate a fixed-effects linear model while preventing the full script
#   from stopping if one robustness specification cannot be estimated.
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
      message("Pre-trend model could not be estimated: ", e$message)
      return(NULL)
    }
  )
}


# ============================================================
# Helper function: extract results safely
# ============================================================
#
# Purpose:
#   Extract the coefficient of interest and key model statistics from each
#   BHJ-style pre-trend regression.
#
# Extracted values:
#   estimate
#   standard error
#   t-statistic or z-statistic
#   p-value
#   within R2
#   number of observations
#   estimation status
# ============================================================

extract_pretrend_bhj_results_safely <- function(
    model,
    term,
    specification,
    sample,
    window,
    outcome_variable,
    exposure_variable
) {
  if (is.null(model)) {
    return(
      tibble(
        sample = sample,
        window = window,
        specification = specification,
        outcome_variable = outcome_variable,
        exposure_variable = exposure_variable,
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
        window = window,
        specification = specification,
        outcome_variable = outcome_variable,
        exposure_variable = exposure_variable,
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
    window = window,
    specification = specification,
    outcome_variable = outcome_variable,
    exposure_variable = exposure_variable,
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
# 1. Main BHJ-style pre-trend test: 2010–2014
# ============================================================
#
# Outcome:
#   delta_log_export_value
#
# Exposure:
#   future_iv_stock_2016_1000
#
# Fixed effects:
#   federal_state + origin_country
#
# Important:
#   No federal_state × origin_country fixed effects.
# ============================================================

pretrend_bhj_stock_2010_2014 <- run_feols_safely(
  formula =
    delta_log_export_value ~ future_iv_stock_2016_1000 |
    federal_state + origin_country,
  data = pretrend_bhj_panel_2010_2014,
  cluster_formula = ~ federal_state
)

if (!is.null(pretrend_bhj_stock_2010_2014)) {
  summary(pretrend_bhj_stock_2010_2014)
}


pretrend_bhj_stock_2010_2014_summary <- extract_pretrend_bhj_results_safely(
  model = pretrend_bhj_stock_2010_2014,
  term = "future_iv_stock_2016_1000",
  specification = "BHJ-style pre-trend: predicted future stock exposure",
  sample = "Full sample",
  window = "2010–2014",
  outcome_variable = "delta_log_export_value",
  exposure_variable = "future_iv_stock_2016_1000"
)

pretrend_bhj_stock_2010_2014_summary


# ============================================================
# 2. Delta-IV pre-trend robustness: 2010–2014
# ============================================================

pretrend_bhj_delta_2010_2014 <- run_feols_safely(
  formula =
    delta_log_export_value ~ future_iv_delta_1000 |
    federal_state + origin_country,
  data = pretrend_bhj_panel_2010_2014,
  cluster_formula = ~ federal_state
)

if (!is.null(pretrend_bhj_delta_2010_2014)) {
  summary(pretrend_bhj_delta_2010_2014)
}


pretrend_bhj_delta_2010_2014_summary <- extract_pretrend_bhj_results_safely(
  model = pretrend_bhj_delta_2010_2014,
  term = "future_iv_delta_1000",
  specification = "BHJ-style pre-trend: predicted future delta exposure",
  sample = "Full sample",
  window = "2010–2014",
  outcome_variable = "delta_log_export_value",
  exposure_variable = "future_iv_delta_1000"
)

pretrend_bhj_delta_2010_2014_summary


# ============================================================
# 3. 2014-key IV pre-trend robustness: 2010–2014
# ============================================================

pretrend_bhj_stock_k14_2010_2014 <- run_feols_safely(
  formula =
    delta_log_export_value ~ future_iv_stock_2016_k14_1000 |
    federal_state + origin_country,
  data = pretrend_bhj_panel_2010_2014,
  cluster_formula = ~ federal_state
)

if (!is.null(pretrend_bhj_stock_k14_2010_2014)) {
  summary(pretrend_bhj_stock_k14_2010_2014)
}


pretrend_bhj_stock_k14_2010_2014_summary <- extract_pretrend_bhj_results_safely(
  model = pretrend_bhj_stock_k14_2010_2014,
  term = "future_iv_stock_2016_k14_1000",
  specification = "BHJ-style pre-trend: predicted future stock exposure, 2014 key",
  sample = "Full sample",
  window = "2010–2014",
  outcome_variable = "delta_log_export_value",
  exposure_variable = "future_iv_stock_2016_k14_1000"
)

pretrend_bhj_stock_k14_2010_2014_summary


# ============================================================
# 4. Alternative pre-period windows
# ============================================================

pretrend_bhj_stock_2011_2014 <- run_feols_safely(
  formula =
    delta_log_export_value ~ future_iv_stock_2016_1000 |
    federal_state + origin_country,
  data = pretrend_bhj_panel_2011_2014,
  cluster_formula = ~ federal_state
)

if (!is.null(pretrend_bhj_stock_2011_2014)) {
  summary(pretrend_bhj_stock_2011_2014)
}


pretrend_bhj_stock_2011_2014_summary <- extract_pretrend_bhj_results_safely(
  model = pretrend_bhj_stock_2011_2014,
  term = "future_iv_stock_2016_1000",
  specification = "BHJ-style pre-trend: predicted future stock exposure",
  sample = "Full sample",
  window = "2011–2014",
  outcome_variable = "delta_log_export_value",
  exposure_variable = "future_iv_stock_2016_1000"
)

pretrend_bhj_stock_2011_2014_summary


pretrend_bhj_stock_2010_2013 <- run_feols_safely(
  formula =
    delta_log_export_value ~ future_iv_stock_2016_1000 |
    federal_state + origin_country,
  data = pretrend_bhj_panel_2010_2013,
  cluster_formula = ~ federal_state
)

if (!is.null(pretrend_bhj_stock_2010_2013)) {
  summary(pretrend_bhj_stock_2010_2013)
}


pretrend_bhj_stock_2010_2013_summary <- extract_pretrend_bhj_results_safely(
  model = pretrend_bhj_stock_2010_2013,
  term = "future_iv_stock_2016_1000",
  specification = "BHJ-style pre-trend: predicted future stock exposure",
  sample = "Full sample",
  window = "2010–2013",
  outcome_variable = "delta_log_export_value",
  exposure_variable = "future_iv_stock_2016_1000"
)

pretrend_bhj_stock_2010_2013_summary


# ============================================================
# 5. No-Eritrea pre-trend robustness
# ============================================================

pretrend_bhj_stock_no_eritrea_2010_2014 <- run_feols_safely(
  formula =
    delta_log_export_value ~ future_iv_stock_2016_1000 |
    federal_state + origin_country,
  data = pretrend_bhj_panel_no_eritrea_2010_2014,
  cluster_formula = ~ federal_state
)

if (!is.null(pretrend_bhj_stock_no_eritrea_2010_2014)) {
  summary(pretrend_bhj_stock_no_eritrea_2010_2014)
}


pretrend_bhj_stock_no_eritrea_2010_2014_summary <- extract_pretrend_bhj_results_safely(
  model = pretrend_bhj_stock_no_eritrea_2010_2014,
  term = "future_iv_stock_2016_1000",
  specification = "BHJ-style pre-trend: predicted future stock exposure",
  sample = "Excluding Eritrea",
  window = "2010–2014",
  outcome_variable = "delta_log_export_value",
  exposure_variable = "future_iv_stock_2016_1000"
)

pretrend_bhj_stock_no_eritrea_2010_2014_summary


# ============================================================
# 6. Descriptive actual-exposure pre-trend robustness
# ============================================================
#
# This is not the main identifying pre-trend check because actual settlement
# may be endogenous. It is kept as a descriptive robustness check.
# ============================================================

pretrend_bhj_treatment_stock_2010_2014 <- run_feols_safely(
  formula =
    delta_log_export_value ~ future_treatment_stock_2016_1000 |
    federal_state + origin_country,
  data = pretrend_bhj_panel_2010_2014,
  cluster_formula = ~ federal_state
)

if (!is.null(pretrend_bhj_treatment_stock_2010_2014)) {
  summary(pretrend_bhj_treatment_stock_2010_2014)
}


pretrend_bhj_treatment_stock_2010_2014_summary <- extract_pretrend_bhj_results_safely(
  model = pretrend_bhj_treatment_stock_2010_2014,
  term = "future_treatment_stock_2016_1000",
  specification = "BHJ-style pre-trend: actual future stock exposure",
  sample = "Full sample",
  window = "2010–2014",
  outcome_variable = "delta_log_export_value",
  exposure_variable = "future_treatment_stock_2016_1000"
)

pretrend_bhj_treatment_stock_2010_2014_summary


# ============================================================
# 7. Combined pre-trend results overview
# ============================================================

pretrend_bhj_results_overview <- bind_rows(
  pretrend_bhj_stock_2010_2014_summary,
  pretrend_bhj_delta_2010_2014_summary,
  pretrend_bhj_stock_k14_2010_2014_summary,
  pretrend_bhj_stock_2011_2014_summary,
  pretrend_bhj_stock_2010_2013_summary,
  pretrend_bhj_stock_no_eritrea_2010_2014_summary,
  pretrend_bhj_treatment_stock_2010_2014_summary
) %>%
  select(
    sample,
    window,
    specification,
    outcome_variable,
    exposure_variable,
    term,
    estimate,
    std_error,
    t_statistic,
    p_value,
    within_r2,
    n_obs,
    status
  )

pretrend_bhj_results_overview


# ============================================================
# 8. Paper-ready rounded pre-trend results
# ============================================================

pretrend_bhj_results_paper <- pretrend_bhj_results_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    t_statistic = round(t_statistic, 2),
    p_value = signif(p_value, 3),
    within_r2 = round(within_r2, 3),
    n_obs = as.integer(n_obs)
  )

pretrend_bhj_results_paper


# ============================================================
# 9. Paper-ready text values for main pre-trend test
# ============================================================

main_pretrend_bhj_coef <-
  pretrend_bhj_stock_2010_2014_summary$estimate

main_pretrend_bhj_se <-
  pretrend_bhj_stock_2010_2014_summary$std_error

main_pretrend_bhj_t <-
  pretrend_bhj_stock_2010_2014_summary$t_statistic

main_pretrend_bhj_p <-
  pretrend_bhj_stock_2010_2014_summary$p_value

main_pretrend_bhj_coef
main_pretrend_bhj_se
main_pretrend_bhj_t
main_pretrend_bhj_p


# ============================================================
# 10. Save pre-trend outputs
# ============================================================

### Model objects

if (!is.null(pretrend_bhj_stock_2010_2014)) {
  saveRDS(
    pretrend_bhj_stock_2010_2014,
    "pretrend_bhj_stock_2010_2014.rds"
  )
}

if (!is.null(pretrend_bhj_delta_2010_2014)) {
  saveRDS(
    pretrend_bhj_delta_2010_2014,
    "pretrend_bhj_delta_2010_2014.rds"
  )
}

if (!is.null(pretrend_bhj_stock_k14_2010_2014)) {
  saveRDS(
    pretrend_bhj_stock_k14_2010_2014,
    "pretrend_bhj_stock_k14_2010_2014.rds"
  )
}

if (!is.null(pretrend_bhj_stock_2011_2014)) {
  saveRDS(
    pretrend_bhj_stock_2011_2014,
    "pretrend_bhj_stock_2011_2014.rds"
  )
}

if (!is.null(pretrend_bhj_stock_2010_2013)) {
  saveRDS(
    pretrend_bhj_stock_2010_2013,
    "pretrend_bhj_stock_2010_2013.rds"
  )
}

if (!is.null(pretrend_bhj_stock_no_eritrea_2010_2014)) {
  saveRDS(
    pretrend_bhj_stock_no_eritrea_2010_2014,
    "pretrend_bhj_stock_no_eritrea_2010_2014.rds"
  )
}

if (!is.null(pretrend_bhj_treatment_stock_2010_2014)) {
  saveRDS(
    pretrend_bhj_treatment_stock_2010_2014,
    "pretrend_bhj_treatment_stock_2010_2014.rds"
  )
}


### Pre-trend change panels

saveRDS(
  pretrend_bhj_panel_2010_2014,
  "pretrend_bhj_panel_2010_2014.rds"
)

saveRDS(
  pretrend_bhj_panel_2011_2014,
  "pretrend_bhj_panel_2011_2014.rds"
)

saveRDS(
  pretrend_bhj_panel_2010_2013,
  "pretrend_bhj_panel_2010_2013.rds"
)

saveRDS(
  pretrend_bhj_panel_no_eritrea_2010_2014,
  "pretrend_bhj_panel_no_eritrea_2010_2014.rds"
)


### Summary objects

saveRDS(
  pretrend_bhj_stock_2010_2014_summary,
  "pretrend_bhj_stock_2010_2014_summary.rds"
)

saveRDS(
  pretrend_bhj_delta_2010_2014_summary,
  "pretrend_bhj_delta_2010_2014_summary.rds"
)

saveRDS(
  pretrend_bhj_stock_k14_2010_2014_summary,
  "pretrend_bhj_stock_k14_2010_2014_summary.rds"
)

saveRDS(
  pretrend_bhj_stock_2011_2014_summary,
  "pretrend_bhj_stock_2011_2014_summary.rds"
)

saveRDS(
  pretrend_bhj_stock_2010_2013_summary,
  "pretrend_bhj_stock_2010_2013_summary.rds"
)

saveRDS(
  pretrend_bhj_stock_no_eritrea_2010_2014_summary,
  "pretrend_bhj_stock_no_eritrea_2010_2014_summary.rds"
)

saveRDS(
  pretrend_bhj_treatment_stock_2010_2014_summary,
  "pretrend_bhj_treatment_stock_2010_2014_summary.rds"
)


### Combined results

saveRDS(
  pretrend_bhj_results_overview,
  "pretrend_bhj_results_overview.rds"
)

saveRDS(
  pretrend_bhj_results_paper,
  "pretrend_bhj_results_paper.rds"
)


### Diagnostics

saveRDS(
  pretrend_bhj_panel_diagnostics,
  "pretrend_bhj_panel_diagnostics.rds"
)

saveRDS(
  missing_input_files,
  "pretrend_bhj_missing_input_files.rds"
)

saveRDS(
  missing_pretrend_bhj_variables,
  "missing_pretrend_bhj_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_pretrend_bhj_coef,
  "main_pretrend_bhj_coef.rds"
)

saveRDS(
  main_pretrend_bhj_se,
  "main_pretrend_bhj_se.rds"
)

saveRDS(
  main_pretrend_bhj_t,
  "main_pretrend_bhj_t.rds"
)

saveRDS(
  main_pretrend_bhj_p,
  "main_pretrend_bhj_p.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_pretrend_bhj_variables,
  safe_max,
  add_future_exposure_intensities,
  make_pretrend_change_panel,
  run_feols_safely,
  extract_pretrend_bhj_results_safely
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base panels with future exposure intensities:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Pre-trend change panels:
#   pretrend_bhj_panel_2010_2014
#   pretrend_bhj_panel_2011_2014
#   pretrend_bhj_panel_2010_2013
#   pretrend_bhj_panel_no_eritrea_2010_2014
#
# Main pre-trend model object:
#   pretrend_bhj_stock_2010_2014
#
# Robustness pre-trend model objects:
#   pretrend_bhj_delta_2010_2014
#   pretrend_bhj_stock_k14_2010_2014
#   pretrend_bhj_stock_2011_2014
#   pretrend_bhj_stock_2010_2013
#   pretrend_bhj_stock_no_eritrea_2010_2014
#
# Descriptive actual-exposure pre-trend model object:
#   pretrend_bhj_treatment_stock_2010_2014
#
# Individual summary objects:
#   pretrend_bhj_stock_2010_2014_summary
#   pretrend_bhj_delta_2010_2014_summary
#   pretrend_bhj_stock_k14_2010_2014_summary
#   pretrend_bhj_stock_2011_2014_summary
#   pretrend_bhj_stock_2010_2013_summary
#   pretrend_bhj_stock_no_eritrea_2010_2014_summary
#   pretrend_bhj_treatment_stock_2010_2014_summary
#
# Combined result tables:
#   pretrend_bhj_results_overview
#   pretrend_bhj_results_paper
#
# Diagnostics:
#   pretrend_bhj_panel_diagnostics
#   missing_pretrend_bhj_variables
#
# Paper-ready text values:
#   main_pretrend_bhj_coef
#   main_pretrend_bhj_se
#   main_pretrend_bhj_t
#   main_pretrend_bhj_p
#
# Notes:
#   This script implements the main pre-trend diagnostic.
#
#   The main pre-trend object is:
#     pretrend_bhj_stock_2010_2014
#
#   The main outcome is:
#     delta_log_export_value
#
#   The main exposure variable is:
#     future_iv_stock_2016_1000
#
#   The main window is:
#     2010–2014
#
#   The regression is cross-sectional at the federal_state × origin_country
#   level and controls for federal_state and origin_country fixed effects.
#
#   Do not include federal_state × origin_country fixed effects in this
#   pre-trend regression because the exposure variable varies at the pair
#   level and would be mechanically absorbed.
#
#   The main interpretation is:
#     A coefficient close to zero and statistically insignificant provides
#     no evidence that later high-exposure pairs already had differential
#     pre-shock export growth.
#
#   This is the main identifying-assumption diagnostic in the final write-up.
#
#   The descriptive actual-exposure pre-trend check is not the main validity
#   test because actual settlement may be endogenous.
#
#   In the final write-up, refer to this section as:
#     BHJ-style pre-shock change regression
#     or
#     pre-trend diagnostic.
#
#   Do not describe it as:
#     event study
#     first stage
#     causal estimate
#     pair-fixed-effects pre-trend test
#
#   This is a regression / analysis script. It loads existing .rds panels
#   and estimates models. It does not rebuild the analysis panel, treatment
#   variables, instruments, or rescaled variables from raw data.
# ============================================================