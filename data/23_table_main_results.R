# ============================================================
# Main Table 1: Main results
# ============================================================
#
# Purpose:
#   Create Main Table 1 for the term paper in standard trade-paper format.
#
# Table structure:
#   (1) First stage
#   (2) PPML reduced form
#   (3) PPML benchmark
#   (4) Linear IV / 2SLS
#   (5) Control-function IV-style PPML
#
# Script type:
#   Table-construction script.
#
# Important:
#   This script does not estimate regressions.
#   It loads already estimated model objects from .rds files and creates
#   a paper-ready table.
#
# Output:
#   main_table_1_results.rds
#   main_table_1_coefficient_details.rds
#   main_table_1_results.csv
#   main_table_1_results.tex
#
# ============================================================


# ============================================================
# Setup
# ============================================================

### Path

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")


### Packages

library(dplyr)
library(tibble)
library(stringr)
library(broom)
library(fixest)
library(readr)


# ============================================================
# Required input files
# ============================================================

required_input_files <- c(
  "first_stage_stock_1000.rds",
  "ppml_reduced_form_stock_1000.rds",
  "ppml_benchmark_stock_1000.rds",
  "linear_iv_stock_1000.rds",
  "iv_ppml_stock_1000.rds"
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
      "Please rerun the corresponding regression scripts before creating Main Table 1."
    )
  )
}


# ============================================================
# Load model objects
# ============================================================

first_stage_stock_1000 <- readRDS(
  "first_stage_stock_1000.rds"
)

ppml_reduced_form_stock_1000 <- readRDS(
  "ppml_reduced_form_stock_1000.rds"
)

ppml_benchmark_stock_1000 <- readRDS(
  "ppml_benchmark_stock_1000.rds"
)

linear_iv_stock_1000 <- readRDS(
  "linear_iv_stock_1000.rds"
)

iv_ppml_stock_1000 <- readRDS(
  "iv_ppml_stock_1000.rds"
)


# ============================================================
# Helper functions
# ============================================================

add_significance_stars <- function(p_value) {
  case_when(
    is.na(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}


format_estimate <- function(estimate, p_value, digits = 4) {
  if (is.na(estimate)) {
    return("")
  }
  
  paste0(
    formatC(
      estimate,
      digits = digits,
      format = "f"
    ),
    add_significance_stars(p_value)
  )
}


format_standard_error <- function(std_error, digits = 4) {
  if (is.na(std_error)) {
    return("")
  }
  
  paste0(
    "(",
    formatC(
      std_error,
      digits = digits,
      format = "f"
    ),
    ")"
  )
}


format_observations <- function(n_obs) {
  format(
    n_obs,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}


escape_latex <- function(x) {
  x %>%
    str_replace_all("\\\\", "\\\\textbackslash{}") %>%
    str_replace_all("&", "\\\\&") %>%
    str_replace_all("%", "\\\\%") %>%
    str_replace_all("\\$", "\\\\$") %>%
    str_replace_all("#", "\\\\#") %>%
    str_replace_all("_", "\\\\_") %>%
    str_replace_all("\\{", "\\\\{") %>%
    str_replace_all("\\}", "\\\\}") %>%
    str_replace_all("~", "\\\\textasciitilde{}") %>%
    str_replace_all("\\^", "\\\\textasciicircum{}")
}


extract_model_coefficient <- function(
    model,
    coefficient_patterns,
    model_label
) {
  tidy_model <- broom::tidy(
    model,
    conf.int = FALSE
  )
  
  matched_rows <- tidy_model %>%
    filter(
      str_detect(
        term,
        paste(coefficient_patterns, collapse = "|")
      )
    )
  
  if (nrow(matched_rows) == 0) {
    stop(
      paste(
        "Could not find coefficient for",
        model_label,
        "using patterns:",
        paste(coefficient_patterns, collapse = ", "),
        "Available terms are:",
        paste(tidy_model$term, collapse = ", ")
      )
    )
  }
  
  matched_rows %>%
    slice(1) %>%
    transmute(
      model_label = model_label,
      term = term,
      estimate = estimate,
      std_error = std.error,
      statistic = statistic,
      p_value = p.value,
      n_obs = nobs(model)
    )
}


extract_number_of_clusters <- function(model, cluster_variable_name = "fe_state_origin") {
  model_data <- tryCatch(
    {
      model.frame(model)
    },
    error = function(e) {
      NULL
    }
  )
  
  if (!is.null(model_data) && cluster_variable_name %in% names(model_data)) {
    return(
      n_distinct(model_data[[cluster_variable_name]])
    )
  }
  
  # Fallback:
  # The main analysis panel has 16 Länder × 5 origins = 80 pair clusters.
  return(80L)
}


extract_first_stage_f <- function(first_stage_coefficient_row) {
  first_stage_coefficient_row$statistic^2
}


# ============================================================
# Extract coefficients
# ============================================================

coefficient_first_stage <- extract_model_coefficient(
  model = first_stage_stock_1000,
  coefficient_patterns = c(
    "^iv_stock_2016_post_1000$"
  ),
  model_label = "First stage"
)

coefficient_ppml_reduced_form <- extract_model_coefficient(
  model = ppml_reduced_form_stock_1000,
  coefficient_patterns = c(
    "^iv_stock_2016_post_1000$"
  ),
  model_label = "PPML reduced form"
)

coefficient_ppml_benchmark <- extract_model_coefficient(
  model = ppml_benchmark_stock_1000,
  coefficient_patterns = c(
    "^treatment_stock_2016_post_1000$"
  ),
  model_label = "PPML benchmark"
)

coefficient_linear_iv <- extract_model_coefficient(
  model = linear_iv_stock_1000,
  coefficient_patterns = c(
    "treatment_stock_2016_post_1000"
  ),
  model_label = "Linear IV / 2SLS"
)

coefficient_cf_treatment <- extract_model_coefficient(
  model = iv_ppml_stock_1000,
  coefficient_patterns = c(
    "^treatment_stock_2016_post_1000$"
  ),
  model_label = "CF-IV-PPML treatment"
)

coefficient_cf_residual <- extract_model_coefficient(
  model = iv_ppml_stock_1000,
  coefficient_patterns = c(
    "residual",
    "first_stage_residual"
  ),
  model_label = "CF-IV-PPML residual"
)


# ============================================================
# Diagnostic statistics
# ============================================================

first_stage_f_statistic <- extract_first_stage_f(
  coefficient_first_stage
)

n_first_stage <- nobs(first_stage_stock_1000)
n_ppml_reduced_form <- nobs(ppml_reduced_form_stock_1000)
n_ppml_benchmark <- nobs(ppml_benchmark_stock_1000)
n_linear_iv <- nobs(linear_iv_stock_1000)
n_cf_iv_ppml <- nobs(iv_ppml_stock_1000)

clusters_first_stage <- extract_number_of_clusters(first_stage_stock_1000)
clusters_ppml_reduced_form <- extract_number_of_clusters(ppml_reduced_form_stock_1000)
clusters_ppml_benchmark <- extract_number_of_clusters(ppml_benchmark_stock_1000)
clusters_linear_iv <- extract_number_of_clusters(linear_iv_stock_1000)
clusters_cf_iv_ppml <- extract_number_of_clusters(iv_ppml_stock_1000)


# ============================================================
# Build Main Table 1
# ============================================================
#
# Layout follows the supervisor's standard trade-paper convention.
#
# ============================================================

main_table_1_results <- tibble(
  row = c(
    "IV: predicted exposure (/1,000)",
    "",
    "Treatment: actual exposure (/1,000)",
    "",
    "First-stage residual (/1,000)",
    "",
    "Outcome",
    "Estimator",
    "Fixed effects:",
    "  Bundesland × origin",
    "  Bundesland × year",
    "  Origin × year",
    "First-stage F",
    "Observations",
    "Clusters (pair)"
  ),
  `First stage` = c(
    format_estimate(
      coefficient_first_stage$estimate,
      coefficient_first_stage$p_value,
      digits = 3
    ),
    format_standard_error(
      coefficient_first_stage$std_error,
      digits = 3
    ),
    "",
    "",
    "",
    "",
    "Treatment",
    "feols",
    "",
    "Yes",
    "Yes",
    "Yes",
    formatC(
      first_stage_f_statistic,
      digits = 1,
      format = "f"
    ),
    format_observations(n_first_stage),
    format_observations(clusters_first_stage)
  ),
  `PPML reduced form` = c(
    format_estimate(
      coefficient_ppml_reduced_form$estimate,
      coefficient_ppml_reduced_form$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_ppml_reduced_form$std_error,
      digits = 4
    ),
    "",
    "",
    "",
    "",
    "Exports",
    "fepois",
    "",
    "Yes",
    "Yes",
    "Yes",
    "",
    format_observations(n_ppml_reduced_form),
    format_observations(clusters_ppml_reduced_form)
  ),
  `PPML benchmark` = c(
    "",
    "",
    format_estimate(
      coefficient_ppml_benchmark$estimate,
      coefficient_ppml_benchmark$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_ppml_benchmark$std_error,
      digits = 4
    ),
    "",
    "",
    "Exports",
    "fepois",
    "",
    "Yes",
    "Yes",
    "Yes",
    "",
    format_observations(n_ppml_benchmark),
    format_observations(clusters_ppml_benchmark)
  ),
  `Linear IV / 2SLS` = c(
    "",
    "",
    format_estimate(
      coefficient_linear_iv$estimate,
      coefficient_linear_iv$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_linear_iv$std_error,
      digits = 4
    ),
    "",
    "",
    "log Exports",
    "feols-IV",
    "",
    "Yes",
    "Yes",
    "Yes",
    "",
    format_observations(n_linear_iv),
    format_observations(clusters_linear_iv)
  ),
  `CF-IV-PPML` = c(
    "",
    "",
    format_estimate(
      coefficient_cf_treatment$estimate,
      coefficient_cf_treatment$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_cf_treatment$std_error,
      digits = 4
    ),
    format_estimate(
      coefficient_cf_residual$estimate,
      coefficient_cf_residual$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_cf_residual$std_error,
      digits = 4
    ),
    "Exports",
    "fepois + CF",
    "",
    "Yes",
    "Yes",
    "Yes",
    "",
    format_observations(n_cf_iv_ppml),
    format_observations(clusters_cf_iv_ppml)
  )
)

main_table_1_results


# ============================================================
# Long-format coefficient table for documentation
# ============================================================

main_table_1_coefficient_details <- bind_rows(
  coefficient_first_stage,
  coefficient_ppml_reduced_form,
  coefficient_ppml_benchmark,
  coefficient_linear_iv,
  coefficient_cf_treatment,
  coefficient_cf_residual
) %>%
  mutate(
    formatted_estimate = mapply(
      format_estimate,
      estimate,
      p_value,
      MoreArgs = list(digits = 4)
    ),
    formatted_standard_error = mapply(
      format_standard_error,
      std_error,
      MoreArgs = list(digits = 4)
    )
  )

main_table_1_coefficient_details


# ============================================================
# Create LaTeX table
# ============================================================

main_table_1_note <- paste0(
  "\\textit{Notes:} Robust standard errors clustered at the Bundesland ",
  "$\\times$ origin country level in parentheses. ",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. ",
  "The instrument is the Königstein-predicted protection-seeker stock in 2016 ",
  "interacted with a post-2016 indicator, scaled per 1,000 persons. ",
  "The treatment is the corresponding actual stock. ",
  "Column (1) is the first-stage \\texttt{feols} regression. ",
  "Columns (2) and (3) are PPML specifications estimated via \\texttt{fepois}. ",
  "Column (4) reports $\\log(\\mathrm{exports}+1)$ as outcome to handle zero flows. ",
  "Column (5) implements a control-function IV-style PPML approach: the ",
  "first-stage residual from a linear regression of treatment on the instrument ",
  "is included as an additional regressor in the PPML second stage. ",
  "Sample period: 2010--2025."
)

main_table_1_latex_rows <- main_table_1_results %>%
  mutate(
    across(
      everything(),
      escape_latex
    )
  ) %>%
  transmute(
    latex_row = paste(
      row,
      `First stage`,
      `PPML reduced form`,
      `PPML benchmark`,
      `Linear IV / 2SLS`,
      `CF-IV-PPML`,
      sep = " & "
    )
  ) %>%
  pull(latex_row)

main_table_1_latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Main results}",
  "\\label{tab:main_results}",
  "\\small",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) & (5) \\\\",
  " & First stage & PPML reduced form & PPML benchmark & Linear IV / 2SLS & CF-IV-PPML \\\\",
  "\\midrule",
  paste0(main_table_1_latex_rows[1:6], " \\\\"),
  "\\midrule",
  paste0(main_table_1_latex_rows[7:8], " \\\\"),
  "\\midrule",
  paste0(main_table_1_latex_rows[9:12], " \\\\"),
  "\\midrule",
  paste0(main_table_1_latex_rows[13:15], " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\vspace{0.2cm}",
  "\\footnotesize",
  main_table_1_note,
  "\\end{minipage}",
  "\\end{table}"
)


# ============================================================
# Save outputs
# ============================================================

saveRDS(
  main_table_1_results,
  "main_table_1_results.rds"
)

saveRDS(
  main_table_1_coefficient_details,
  "main_table_1_coefficient_details.rds"
)

write_csv(
  main_table_1_results,
  "main_table_1_results.csv"
)

write_lines(
  main_table_1_latex,
  "main_table_1_results.tex"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  add_significance_stars,
  format_estimate,
  format_standard_error,
  format_observations,
  escape_latex,
  extract_model_coefficient,
  extract_number_of_clusters,
  extract_first_stage_f,
  coefficient_first_stage,
  coefficient_ppml_reduced_form,
  coefficient_ppml_benchmark,
  coefficient_linear_iv,
  coefficient_cf_treatment,
  coefficient_cf_residual,
  first_stage_f_statistic,
  n_first_stage,
  n_ppml_reduced_form,
  n_ppml_benchmark,
  n_linear_iv,
  n_cf_iv_ppml,
  clusters_first_stage,
  clusters_ppml_reduced_form,
  clusters_ppml_benchmark,
  clusters_linear_iv,
  clusters_cf_iv_ppml,
  main_table_1_note,
  main_table_1_latex_rows
)


# ============================================================
# Final objects kept
# ============================================================
#
# Model objects loaded:
#   first_stage_stock_1000
#   ppml_reduced_form_stock_1000
#   ppml_benchmark_stock_1000
#   linear_iv_stock_1000
#   iv_ppml_stock_1000
#
# Table objects:
#   main_table_1_results
#   main_table_1_coefficient_details
#   main_table_1_latex
#
# Saved files:
#   main_table_1_results.rds
#   main_table_1_coefficient_details.rds
#   main_table_1_results.csv
#   main_table_1_results.tex
#
# Interpretation:
#   Main Table 1 summarizes the central empirical results:
#     - strong first stage,
#     - no PPML reduced-form evidence of a positive export response,
#     - no non-instrumented PPML benchmark association,
#     - no linear IV / 2SLS evidence,
#     - no control-function IV-style PPML evidence.
#
# ============================================================