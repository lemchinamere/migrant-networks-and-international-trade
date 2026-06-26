# ============================================================
# IV strength check: First-stage relevance
# ============================================================
#
# Purpose:
#   Check whether the Königstein-based predicted exposure variables are
#   strong predictors of actual regional protection-seeker exposure.
#
# Main endogenous treatment interaction:
#   treatment_stock_2016_post_1000
#
# Main instrument:
#   iv_stock_2016_post_1000
#
# Main first-stage equation:
#   treatment_stock_2016_post_1000 =
#     beta * iv_stock_2016_post_1000
#     + federal_state × origin_country fixed effects
#     + federal_state × year fixed effects
#     + origin_country × year fixed effects
#     + error
#
# Main IV basis:
#   koenigstein_share_2015_2016_avg
#
# Alternative treatment:
#   treatment_delta_post_1000
#
# Alternative instrument:
#   iv_delta_post_1000
#
# Robustness IV bases:
#   iv_stock_2016_post_k14_1000
#   iv_delta_post_k14_1000
#   iv_stock_2016_post_k141516_1000
#   iv_delta_post_k141516_1000
#
# Additional robustness sample:
#   analysis_panel_no_eritrea
#
# Interpretation:
#   Treatment and IV variables are measured in thousand persons.
#
#   A strong and statistically significant first-stage relationship supports
#   the relevance condition of the instrument: predicted exposure should be
#   strongly related to actual protection-seeker exposure.
#
#   For a single excluded instrument, the first-stage F-statistic is the
#   squared t-statistic on the excluded instrument.
#
# Important caveat:
#   A strong first stage supports instrument relevance. It does not establish
#   the exclusion restriction or instrument exogeneity. Exogeneity must be
#   motivated by the institutional setting and assessed through pre-trend
#   diagnostics.
#
# Note:
#   Some alternative IV specifications may be collinear with the preferred
#   three-way fixed-effect structure. These models are reported as not
#   estimable instead of stopping the script.
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
# Load required panels
# ============================================================
#
# Purpose:
#   Load the active analysis panels required for the first-stage relevance
#   checks.
#
# Panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Notes:
#   analysis_panel is used for the main full-sample first-stage
#   specifications.
#
#   analysis_panel_no_eritrea is used to check whether first-stage relevance
#   is robust to excluding Eritrea.
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
#   required by the first-stage specifications.
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
#   Fixed-effect variables should already be present in the final panels.
#   This block reconstructs them only if they are missing.
#
#   The required-variable check is run after this reconstruction so that
#   missing fixed-effect variables are not incorrectly flagged if they can be
#   reconstructed from the base identifiers.
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
#   for the first-stage relevance checks.
#
# Variables checked:
#   Panel identifiers, period indicator, treatment variables, main IV
#   variables, alternative Königstein-key IV variables, and fixed effects.
#
# Interpretation:
#   Missing variables indicate that an earlier data-construction or rescaling
#   script must be rerun before estimating the first-stage specifications.
#
# Notes:
#   This check is diagnostic. The script still prints the missing-variable
#   table so that problems can be inspected directly.
# ============================================================

required_first_stage_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "post_period",
  
  "treatment_stock_2016_post_1000",
  "iv_stock_2016_post_1000",
  
  "treatment_delta_post_1000",
  "iv_delta_post_1000",
  
  "iv_stock_2016_post_k14_1000",
  "iv_delta_post_k14_1000",
  
  "iv_stock_2016_post_k141516_1000",
  "iv_delta_post_k141516_1000",
  
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)


missing_first_stage_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_first_stage_variables,
    present = required_first_stage_variables %in% names(analysis_panel)
  ),
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_first_stage_variables,
    present = required_first_stage_variables %in%
      names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_first_stage_variables


# ============================================================
# Helper function: run first-stage model safely
# ============================================================
#
# Purpose:
#   Estimate a first-stage model while preventing the full script from
#   stopping if a specification is not estimable.
#
# Logic:
#   If the excluded instrument is collinear with the fixed effects, or if
#   the model otherwise cannot be estimated, the function returns NULL.
#
# Notes:
#   The corresponding summary table then records the model as not estimable.
# ============================================================

run_first_stage_safely <- function(
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
# Helper function: extract first-stage results safely
# ============================================================
#
# Purpose:
#   Extract coefficient estimates, standard errors, test statistics,
#   p-values, first-stage F-statistics, sample size, and estimation status
#   from first-stage model objects.
#
# Logic:
#   For a single excluded instrument, the first-stage F-statistic is computed
#   as:
#
#     first_stage_f_statistic = t_statistic^2
#
# Notes:
#   If the model is not estimable or the excluded instrument is dropped, the
#   function returns missing coefficient values and records the corresponding
#   status.
# ============================================================

extract_first_stage_results_safely <- function(
    model,
    term,
    specification,
    sample,
    endogenous_variable,
    instrument_variable
) {
  if (is.null(model)) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        endogenous_variable = endogenous_variable,
        instrument_variable = instrument_variable,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        first_stage_f_statistic = NA_real_,
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
  
  if (!(term %in% rownames(coefficient_table))) {
    return(
      tibble(
        sample = sample,
        specification = specification,
        endogenous_variable = endogenous_variable,
        instrument_variable = instrument_variable,
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        first_stage_f_statistic = NA_real_,
        n_obs = nobs(model),
        status = "term dropped"
      )
    )
  }
  
  t_value <- if (!is.na(statistic_column)) {
    coefficient_table[term, statistic_column]
  } else {
    NA_real_
  }
  
  tibble(
    sample = sample,
    specification = specification,
    endogenous_variable = endogenous_variable,
    instrument_variable = instrument_variable,
    term = term,
    estimate = coefficient_table[term, "Estimate"],
    std_error = coefficient_table[term, "Std. Error"],
    t_statistic = t_value,
    p_value = if (!is.na(p_value_column)) {
      coefficient_table[term, p_value_column]
    } else {
      NA_real_
    },
    first_stage_f_statistic = t_value^2,
    n_obs = nobs(model),
    status = "estimated"
  )
}


# ============================================================
# 1. Main first stage: stock exposure
# ============================================================
#
# Purpose:
#   Estimate the main first-stage relevance specification using the 2016
#   stock exposure treatment and the Königstein-predicted stock exposure
#   instrument.
#
# Specification:
#   treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   A positive and strong coefficient indicates that the main predicted
#   exposure measure is relevant for actual regional stock exposure.
# ============================================================

first_stage_stock_1000 <- run_first_stage_safely(
  formula =
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(first_stage_stock_1000)) {
  summary(first_stage_stock_1000)
}


first_stage_stock_1000_summary <- extract_first_stage_results_safely(
  model = first_stage_stock_1000,
  term = "iv_stock_2016_post_1000",
  specification = "Main stock exposure",
  sample = "Full sample",
  endogenous_variable = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_1000"
)

first_stage_stock_1000_summary


# ============================================================
# 2. Alternative first stage: delta exposure
# ============================================================
#
# Purpose:
#   Estimate an alternative first-stage specification using the 2014–2016
#   change in protection-seeker exposure.
#
# Specification:
#   treatment_delta_post_1000 ~ iv_delta_post_1000
#   + three-way fixed effects
#
# Interpretation:
#   This checks whether predicted delta exposure is relevant for actual
#   regional changes in protection-seeker exposure.
# ============================================================

first_stage_delta_1000 <- run_first_stage_safely(
  formula =
    treatment_delta_post_1000 ~ iv_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(first_stage_delta_1000)) {
  summary(first_stage_delta_1000)
}


first_stage_delta_1000_summary <- extract_first_stage_results_safely(
  model = first_stage_delta_1000,
  term = "iv_delta_post_1000",
  specification = "Alternative delta exposure",
  sample = "Full sample",
  endogenous_variable = "treatment_delta_post_1000",
  instrument_variable = "iv_delta_post_1000"
)

first_stage_delta_1000_summary


# ============================================================
# 3. Robustness first stage: 2014 Königstein key
# ============================================================
#
# Purpose:
#   Check whether first-stage relevance also holds when predicted exposure
#   is constructed using the strictly pre-shock 2014 Königstein key.
#
# IV basis:
#   koenigstein_share_2014
#
# Specifications:
#   treatment_stock_2016_post_1000 ~ iv_stock_2016_post_k14_1000
#   treatment_delta_post_1000 ~ iv_delta_post_k14_1000
#
# Interpretation:
#   These specifications test whether first-stage relevance depends on using
#   the main 2015–2016 Königstein average rather than the strictly pre-shock
#   2014 allocation key.
# ============================================================

first_stage_stock_k14_1000 <- run_first_stage_safely(
  formula =
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_k14_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(first_stage_stock_k14_1000)) {
  summary(first_stage_stock_k14_1000)
}


first_stage_stock_k14_1000_summary <- extract_first_stage_results_safely(
  model = first_stage_stock_k14_1000,
  term = "iv_stock_2016_post_k14_1000",
  specification = "Stock exposure, 2014 key",
  sample = "Full sample",
  endogenous_variable = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_k14_1000"
)

first_stage_stock_k14_1000_summary


first_stage_delta_k14_1000 <- run_first_stage_safely(
  formula =
    treatment_delta_post_1000 ~ iv_delta_post_k14_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(first_stage_delta_k14_1000)) {
  summary(first_stage_delta_k14_1000)
}


first_stage_delta_k14_1000_summary <- extract_first_stage_results_safely(
  model = first_stage_delta_k14_1000,
  term = "iv_delta_post_k14_1000",
  specification = "Delta exposure, 2014 key",
  sample = "Full sample",
  endogenous_variable = "treatment_delta_post_1000",
  instrument_variable = "iv_delta_post_k14_1000"
)

first_stage_delta_k14_1000_summary


# ============================================================
# 4. Robustness first stage: 2014–2016 three-year average
# ============================================================
#
# Purpose:
#   Check whether first-stage relevance also holds when predicted exposure
#   is constructed using the average Königstein allocation share over 2014,
#   2015 and 2016.
#
# IV basis:
#   koenigstein_share_2014_2015_2016_avg
#
# Specifications:
#   treatment_stock_2016_post_1000 ~ iv_stock_2016_post_k141516_1000
#   treatment_delta_post_1000 ~ iv_delta_post_k141516_1000
#
# Interpretation:
#   This is mainly a transparency check. If the 2014–2016 average IV is
#   collinear with the preferred three-way fixed-effect structure, the model
#   is reported as not estimable rather than interpreted as evidence against
#   instrument relevance.
# ============================================================

first_stage_stock_k141516_1000 <- run_first_stage_safely(
  formula =
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_k141516_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(first_stage_stock_k141516_1000)) {
  summary(first_stage_stock_k141516_1000)
}


first_stage_stock_k141516_1000_summary <- extract_first_stage_results_safely(
  model = first_stage_stock_k141516_1000,
  term = "iv_stock_2016_post_k141516_1000",
  specification = "Stock exposure, 2014–2016 average",
  sample = "Full sample",
  endogenous_variable = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_k141516_1000"
)

first_stage_stock_k141516_1000_summary


first_stage_delta_k141516_1000 <- run_first_stage_safely(
  formula =
    treatment_delta_post_1000 ~ iv_delta_post_k141516_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(first_stage_delta_k141516_1000)) {
  summary(first_stage_delta_k141516_1000)
}


first_stage_delta_k141516_1000_summary <- extract_first_stage_results_safely(
  model = first_stage_delta_k141516_1000,
  term = "iv_delta_post_k141516_1000",
  specification = "Delta exposure, 2014–2016 average",
  sample = "Full sample",
  endogenous_variable = "treatment_delta_post_1000",
  instrument_variable = "iv_delta_post_k141516_1000"
)

first_stage_delta_k141516_1000_summary


# ============================================================
# 5. No-Eritrea robustness first stage
# ============================================================
#
# Purpose:
#   Check whether first-stage relevance is robust to excluding Eritrea.
#
# Motivation:
#   Eritrea may differ from the other origin countries in export reporting,
#   trade levels, or migration dynamics. Excluding it tests whether the first
#   stage is driven by this origin country.
#
# Specifications:
#   treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000
#   treatment_delta_post_1000 ~ iv_delta_post_1000
#
# Sample:
#   analysis_panel_no_eritrea
# ============================================================

first_stage_stock_no_eritrea_1000 <- run_first_stage_safely(
  formula =
    treatment_stock_2016_post_1000 ~ iv_stock_2016_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(first_stage_stock_no_eritrea_1000)) {
  summary(first_stage_stock_no_eritrea_1000)
}


first_stage_stock_no_eritrea_1000_summary <- extract_first_stage_results_safely(
  model = first_stage_stock_no_eritrea_1000,
  term = "iv_stock_2016_post_1000",
  specification = "Main stock exposure",
  sample = "Excluding Eritrea",
  endogenous_variable = "treatment_stock_2016_post_1000",
  instrument_variable = "iv_stock_2016_post_1000"
)

first_stage_stock_no_eritrea_1000_summary


first_stage_delta_no_eritrea_1000 <- run_first_stage_safely(
  formula =
    treatment_delta_post_1000 ~ iv_delta_post_1000 |
    fe_state_origin + fe_state_year + fe_origin_year,
  data = analysis_panel_no_eritrea,
  cluster_formula = ~ fe_state_origin
)

if (!is.null(first_stage_delta_no_eritrea_1000)) {
  summary(first_stage_delta_no_eritrea_1000)
}


first_stage_delta_no_eritrea_1000_summary <- extract_first_stage_results_safely(
  model = first_stage_delta_no_eritrea_1000,
  term = "iv_delta_post_1000",
  specification = "Alternative delta exposure",
  sample = "Excluding Eritrea",
  endogenous_variable = "treatment_delta_post_1000",
  instrument_variable = "iv_delta_post_1000"
)

first_stage_delta_no_eritrea_1000_summary


# ============================================================
# 6. Descriptive treatment-IV correlations
# ============================================================
#
# Purpose:
#   Compute simple descriptive correlations between actual treatment
#   exposure and predicted exposure.
#
# Variables checked:
#   treatment_stock_2016_post_1000
#   iv_stock_2016_post_1000
#   treatment_delta_post_1000
#   iv_delta_post_1000
#
# Logic:
#   Correlations are computed in the post-period sample and based on
#   distinct federal_state × origin_country observations.
#
# Interpretation:
#   These are descriptive relevance diagnostics only. The fixed-effects
#   first-stage regressions above are the relevant specifications for
#   assessing first-stage strength.
# ============================================================

treatment_iv_correlation_first_stage <- analysis_panel %>%
  filter(
    post_period == 1
  ) %>%
  distinct(
    federal_state,
    origin_country,
    treatment_stock_2016_post_1000,
    iv_stock_2016_post_1000,
    treatment_delta_post_1000,
    iv_delta_post_1000
  ) %>%
  summarise(
    correlation_stock =
      cor(
        treatment_stock_2016_post_1000,
        iv_stock_2016_post_1000,
        use = "complete.obs"
      ),
    correlation_delta =
      cor(
        treatment_delta_post_1000,
        iv_delta_post_1000,
        use = "complete.obs"
      )
  )

treatment_iv_correlation_first_stage


robustness_iv_correlation_first_stage <- analysis_panel %>%
  filter(
    post_period == 1
  ) %>%
  distinct(
    federal_state,
    origin_country,
    iv_stock_2016_post_1000,
    iv_stock_2016_post_k14_1000,
    iv_stock_2016_post_k141516_1000,
    iv_delta_post_1000,
    iv_delta_post_k14_1000,
    iv_delta_post_k141516_1000
  ) %>%
  summarise(
    corr_stock_main_k14 =
      cor(
        iv_stock_2016_post_1000,
        iv_stock_2016_post_k14_1000,
        use = "complete.obs"
      ),
    corr_stock_main_k141516 =
      cor(
        iv_stock_2016_post_1000,
        iv_stock_2016_post_k141516_1000,
        use = "complete.obs"
      ),
    corr_delta_main_k14 =
      cor(
        iv_delta_post_1000,
        iv_delta_post_k14_1000,
        use = "complete.obs"
      ),
    corr_delta_main_k141516 =
      cor(
        iv_delta_post_1000,
        iv_delta_post_k141516_1000,
        use = "complete.obs"
      )
  )

robustness_iv_correlation_first_stage


# ============================================================
# 7. Additional variation diagnostic for 2014–2016 average IV
# ============================================================
#
# Purpose:
#   Check whether the 2014–2016 average IV has overall variation, even if it
#   is not separately estimable after absorbing the preferred fixed effects.
#
# Variables checked:
#   iv_stock_2016_post_k141516_1000
#   iv_delta_post_k141516_1000
#
# Interpretation:
#   Overall variation in the raw variable does not guarantee that the
#   coefficient is separately identified after fixed effects. This diagnostic
#   therefore complements, but does not replace, the first-stage regression.
# ============================================================

k141516_variation_check <- analysis_panel %>%
  summarise(
    min_stock_k141516 =
      min(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
    max_stock_k141516 =
      max(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
    sd_stock_k141516 =
      sd(iv_stock_2016_post_k141516_1000, na.rm = TRUE),
    n_unique_stock_k141516 =
      n_distinct(iv_stock_2016_post_k141516_1000),
    
    min_delta_k141516 =
      min(iv_delta_post_k141516_1000, na.rm = TRUE),
    max_delta_k141516 =
      max(iv_delta_post_k141516_1000, na.rm = TRUE),
    sd_delta_k141516 =
      sd(iv_delta_post_k141516_1000, na.rm = TRUE),
    n_unique_delta_k141516 =
      n_distinct(iv_delta_post_k141516_1000)
  )

k141516_variation_check


# ============================================================
# 8. Combined first-stage results overview
# ============================================================
#
# Purpose:
#   Combine all first-stage relevance results into one overview table.
#
# Included specifications:
#   Main stock first stage
#   Delta-exposure first stage
#   2014-key stock first stage
#   2014-key delta first stage
#   2014–2016-average stock first stage
#   2014–2016-average delta first stage
#   No-Eritrea stock first stage
#   No-Eritrea delta first stage
#
# Notes:
#   This table is intended for internal comparison and documentation.
# ============================================================

first_stage_results_overview <- bind_rows(
  first_stage_stock_1000_summary,
  first_stage_delta_1000_summary,
  first_stage_stock_k14_1000_summary,
  first_stage_delta_k14_1000_summary,
  first_stage_stock_k141516_1000_summary,
  first_stage_delta_k141516_1000_summary,
  first_stage_stock_no_eritrea_1000_summary,
  first_stage_delta_no_eritrea_1000_summary
)

first_stage_results_overview


# ============================================================
# 9. Paper-ready rounded first-stage results
# ============================================================
#
# Purpose:
#   Create a rounded version of the first-stage results table for easier
#   reporting and interpretation.
#
# Notes:
#   This table is not automatically formatted for publication but provides
#   paper-ready rounded values.
# ============================================================

first_stage_results_paper <- first_stage_results_overview %>%
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    t_statistic = round(t_statistic, 2),
    p_value = signif(p_value, 3),
    first_stage_f_statistic = round(first_stage_f_statistic, 1),
    n_obs = as.integer(n_obs)
  )

first_stage_results_paper


# ============================================================
# 10. Paper-ready text values
# ============================================================
#
# Purpose:
#   Store the main first-stage result in a separate object for easy use in
#   the written results section.
#
# Main reported first-stage result:
#   Main stock exposure, full sample
#
# Notes:
#   Additional first-stage specifications are retained in the combined
#   results table and can be discussed as robustness checks.
# ============================================================

main_first_stage_stock_result <- first_stage_results_paper %>%
  filter(
    specification == "Main stock exposure",
    sample == "Full sample"
  )

main_first_stage_stock_result


# ============================================================
# 11. Save first-stage outputs
# ============================================================
#
# Purpose:
#   Save all first-stage model objects, summary tables, diagnostic objects,
#   and paper-ready text values.
#
# Notes:
#   These outputs document first-stage relevance. They should be used to
#   support the relevance condition of the instrument, not the exclusion
#   restriction.
# ============================================================

### Model objects

if (!is.null(first_stage_stock_1000)) {
  saveRDS(
    first_stage_stock_1000,
    "first_stage_stock_1000.rds"
  )
}

if (!is.null(first_stage_delta_1000)) {
  saveRDS(
    first_stage_delta_1000,
    "first_stage_delta_1000.rds"
  )
}

if (!is.null(first_stage_stock_k14_1000)) {
  saveRDS(
    first_stage_stock_k14_1000,
    "first_stage_stock_k14_1000.rds"
  )
}

if (!is.null(first_stage_delta_k14_1000)) {
  saveRDS(
    first_stage_delta_k14_1000,
    "first_stage_delta_k14_1000.rds"
  )
}

if (!is.null(first_stage_stock_k141516_1000)) {
  saveRDS(
    first_stage_stock_k141516_1000,
    "first_stage_stock_k141516_1000.rds"
  )
}

if (!is.null(first_stage_delta_k141516_1000)) {
  saveRDS(
    first_stage_delta_k141516_1000,
    "first_stage_delta_k141516_1000.rds"
  )
}

if (!is.null(first_stage_stock_no_eritrea_1000)) {
  saveRDS(
    first_stage_stock_no_eritrea_1000,
    "first_stage_stock_no_eritrea_1000.rds"
  )
}

if (!is.null(first_stage_delta_no_eritrea_1000)) {
  saveRDS(
    first_stage_delta_no_eritrea_1000,
    "first_stage_delta_no_eritrea_1000.rds"
  )
}


### Combined first-stage result tables

saveRDS(
  first_stage_results_overview,
  "first_stage_results_overview.rds"
)

saveRDS(
  first_stage_results_paper,
  "first_stage_results_paper.rds"
)


### Descriptive diagnostics

saveRDS(
  treatment_iv_correlation_first_stage,
  "treatment_iv_correlation_first_stage.rds"
)

saveRDS(
  robustness_iv_correlation_first_stage,
  "robustness_iv_correlation_first_stage.rds"
)

saveRDS(
  k141516_variation_check,
  "k141516_variation_check.rds"
)


### Required-variable check

saveRDS(
  missing_first_stage_variables,
  "missing_first_stage_variables.rds"
)


### Paper-ready text values

saveRDS(
  main_first_stage_stock_result,
  "main_first_stage_stock_result.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_first_stage_variables,
  add_fixed_effects_if_missing,
  run_first_stage_safely,
  extract_first_stage_results_safely
)


# ============================================================
# Final objects kept
# ============================================================
#
# Base panels:
#   analysis_panel
#   analysis_panel_no_eritrea
#
# Main first-stage model objects:
#   first_stage_stock_1000
#   first_stage_delta_1000
#
# 2014 Königstein-key first-stage model objects:
#   first_stage_stock_k14_1000
#   first_stage_delta_k14_1000
#
# 2014–2016 average Königstein-key first-stage model objects:
#   first_stage_stock_k141516_1000
#   first_stage_delta_k141516_1000
#
# No-Eritrea first-stage model objects:
#   first_stage_stock_no_eritrea_1000
#   first_stage_delta_no_eritrea_1000
#
# Individual first-stage summary objects:
#   first_stage_stock_1000_summary
#   first_stage_delta_1000_summary
#   first_stage_stock_k14_1000_summary
#   first_stage_delta_k14_1000_summary
#   first_stage_stock_k141516_1000_summary
#   first_stage_delta_k141516_1000_summary
#   first_stage_stock_no_eritrea_1000_summary
#   first_stage_delta_no_eritrea_1000_summary
#
# Combined first-stage result tables:
#   first_stage_results_overview
#   first_stage_results_paper
#
# Descriptive correlation diagnostics:
#   treatment_iv_correlation_first_stage
#   robustness_iv_correlation_first_stage
#
# Additional variation diagnostic:
#   k141516_variation_check
#
# Required-variable check:
#   missing_first_stage_variables
#
# Paper-ready text values:
#   main_first_stage_stock_result
#
# Notes:
#   This script checks first-stage relevance of the Königstein-based
#   predicted exposure measures.
#
#   The main first-stage object is:
#     first_stage_stock_1000
#
#   The main endogenous variable is:
#     treatment_stock_2016_post_1000
#
#   The main excluded instrument is:
#     iv_stock_2016_post_1000
#
#   The first-stage F-statistic is computed as the squared t-statistic on the
#   excluded instrument because each first-stage specification contains only
#   one excluded instrument.
#
#   A strong first stage supports the instrument relevance condition. It does
#   not by itself prove instrument exogeneity or the exclusion restriction.
#   Exogeneity is instead assessed through the institutional setting and the
#   pre-trend diagnostics.
#
#   The 2014 Königstein-key specification is an active robustness check.
#
#   The 2014–2016 average Königstein-key variables are constructed and
#   diagnosed for transparency. If they are collinear with the preferred
#   three-way fixed-effect structure, they should be reported as not
#   separately estimable rather than interpreted as failed evidence.
#
#   In the final write-up, use this section to support the relevance
#   condition of the instrument, not the exclusion restriction.
# ============================================================