# ============================================================
# Main Table 2: Pre-trend test and additional main specifications
# ============================================================
#
# Purpose:
#   Create Main Table 2 for the term paper in standard trade-paper format.
#
# Table structure:
#   (1) BHJ pre-trend
#   (2) Delta PPML reduced form
#   (3) Regional-control PPML reduced form
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
#   main_table_2_diagnostics_robustness.rds
#   main_table_2_coefficient_details.rds
#   main_table_2_diagnostics_robustness.csv
#   main_table_2_diagnostics_robustness.tex
#
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
library(tibble)
library(stringr)
library(broom)
library(fixest)
library(readr)


# ============================================================
# Required input files
# ============================================================

required_input_files <- c(
  "pretrend_bhj_stock_2010_2014.rds",
  "ppml_reduced_form_delta_1000.rds",
  "robustness_controls_ppml_reduced_form_stock_1000.rds"
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
      "Please rerun the corresponding regression scripts before creating Main Table 2."
    )
  )
}


# ============================================================
# Load model objects
# ============================================================

pretrend_bhj_stock_2010_2014 <- readRDS(
  "pretrend_bhj_stock_2010_2014.rds"
)

ppml_reduced_form_delta_1000 <- readRDS(
  "ppml_reduced_form_delta_1000.rds"
)

robustness_controls_ppml_reduced_form_stock_1000 <- readRDS(
  "robustness_controls_ppml_reduced_form_stock_1000.rds"
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


extract_number_of_clusters <- function(
    model,
    cluster_variable_name = "fe_state_origin",
    fallback_clusters = 80L
) {
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
  
  return(fallback_clusters)
}


# ============================================================
# Extract coefficients
# ============================================================
#
# Main Table 2 coefficients:
#
#   Column 1:
#     BHJ pre-trend coefficient on future_iv_stock_2016_1000.
#
#   Column 2:
#     Delta PPML reduced-form coefficient on iv_delta_post_1000.
#
#   Column 3:
#     Regional-control PPML reduced-form coefficient on
#     iv_stock_2016_post_1000.
#
# ============================================================

coefficient_pretrend_bhj <- extract_model_coefficient(
  model = pretrend_bhj_stock_2010_2014,
  coefficient_patterns = c(
    "^future_iv_stock_2016_1000$"
  ),
  model_label = "BHJ pre-trend"
)

coefficient_delta_ppml <- extract_model_coefficient(
  model = ppml_reduced_form_delta_1000,
  coefficient_patterns = c(
    "^iv_delta_post_1000$"
  ),
  model_label = "Delta PPML reduced form"
)

coefficient_regional_controls <- extract_model_coefficient(
  model = robustness_controls_ppml_reduced_form_stock_1000,
  coefficient_patterns = c(
    "^iv_stock_2016_post_1000$"
  ),
  model_label = "Regional controls"
)


# ============================================================
# Diagnostic statistics
# ============================================================

n_pretrend_bhj <- nobs(pretrend_bhj_stock_2010_2014)
n_delta_ppml <- nobs(ppml_reduced_form_delta_1000)
n_regional_controls <- nobs(robustness_controls_ppml_reduced_form_stock_1000)

clusters_pretrend_bhj <- extract_number_of_clusters(
  pretrend_bhj_stock_2010_2014,
  cluster_variable_name = "federal_state",
  fallback_clusters = 16L
)

clusters_delta_ppml <- extract_number_of_clusters(
  ppml_reduced_form_delta_1000,
  cluster_variable_name = "fe_state_origin",
  fallback_clusters = 80L
)

clusters_regional_controls <- extract_number_of_clusters(
  robustness_controls_ppml_reduced_form_stock_1000,
  cluster_variable_name = "fe_state_origin",
  fallback_clusters = 80L
)


# ============================================================
# Build Main Table 2
# ============================================================

main_table_2_diagnostics_robustness <- tibble(
  row = c(
    "IV: predicted exposure (/1,000)",
    "",
    "IV: predicted delta exposure (/1,000)",
    "",
    "Outcome",
    "Estimator",
    "Sample period",
    "Fixed effects:",
    "  Bundesland × origin",
    "  Bundesland × year",
    "  Origin × year",
    "  Bundesland (separate)",
    "  Origin country (separate)",
    "Regional controls",
    "Observations",
    "Clusters"
  ),
  `BHJ pre-trend` = c(
    format_estimate(
      coefficient_pretrend_bhj$estimate,
      coefficient_pretrend_bhj$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_pretrend_bhj$std_error,
      digits = 4
    ),
    "",
    "",
    "Δ log exports",
    "feols",
    "2010–2014",
    "",
    "No",
    "No",
    "No",
    "Yes",
    "Yes",
    "No",
    format_observations(n_pretrend_bhj),
    format_observations(clusters_pretrend_bhj)
  ),
  `Delta PPML RF` = c(
    "",
    "",
    format_estimate(
      coefficient_delta_ppml$estimate,
      coefficient_delta_ppml$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_delta_ppml$std_error,
      digits = 4
    ),
    "Exports",
    "fepois",
    "2010–2025",
    "",
    "Yes",
    "Yes",
    "Yes",
    "-",
    "-",
    "No",
    format_observations(n_delta_ppml),
    format_observations(clusters_delta_ppml)
  ),
  `Regional controls` = c(
    format_estimate(
      coefficient_regional_controls$estimate,
      coefficient_regional_controls$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_regional_controls$std_error,
      digits = 4
    ),
    "",
    "",
    "Exports",
    "fepois",
    "2010–2024",
    "",
    "Yes",
    "No",
    "Yes",
    "-",
    "-",
    "Yes",
    format_observations(n_regional_controls),
    format_observations(clusters_regional_controls)
  )
)

main_table_2_diagnostics_robustness


# ============================================================
# Long-format coefficient table for documentation
# ============================================================

main_table_2_coefficient_details <- bind_rows(
  coefficient_pretrend_bhj,
  coefficient_delta_ppml,
  coefficient_regional_controls
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

main_table_2_coefficient_details


# ============================================================
# Create LaTeX table
# ============================================================

main_table_2_note <- paste0(
  "\\textit{Notes:} Standard errors are reported in parentheses. ",
  "Standard errors are clustered at the Bundesland $\\times$ origin country ",
  "level in columns (2) and (3), and at the Bundesland level in column (1). ",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. ",
  "Column (1) is a Borusyak-Hull-Jaravel-style cross-sectional first-differences ",
  "pre-trend test, regressing $\\Delta \\log$ exports between 2010 and 2014 ",
  "on the IV with separate Bundesland and origin fixed effects. Pair fixed ",
  "effects would absorb the cross-sectional IV. ",
  "Column (2) is the delta-version PPML reduced form, replacing the 2016-stock ",
  "exposure with the 2014--2016 predicted delta exposure. ",
  "Column (3) replaces Bundesland $\\times$ year fixed effects with explicit ",
  "controls for log GDP, log population, unemployment rate, log employment, ",
  "manufacturing share, and log total Bundesland exports to the world."
)

main_table_2_latex_rows <- main_table_2_diagnostics_robustness %>%
  mutate(
    across(
      everything(),
      escape_latex
    )
  ) %>%
  transmute(
    latex_row = paste(
      row,
      `BHJ pre-trend`,
      `Delta PPML RF`,
      `Regional controls`,
      sep = " & "
    )
  ) %>%
  pull(latex_row)

main_table_2_latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Pre-trend test and additional main specifications}",
  "\\label{tab:main_diagnostics_robustness}",
  "\\small",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & (1) & (2) & (3) \\\\",
  " & BHJ pre-trend & Delta PPML RF & Regional controls \\\\",
  "\\midrule",
  paste0(main_table_2_latex_rows[1:4], " \\\\"),
  "\\midrule",
  paste0(main_table_2_latex_rows[5:7], " \\\\"),
  "\\midrule",
  paste0(main_table_2_latex_rows[8:14], " \\\\"),
  "\\midrule",
  paste0(main_table_2_latex_rows[15:16], " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\vspace{0.2cm}",
  "\\footnotesize",
  main_table_2_note,
  "\\end{minipage}",
  "\\end{table}"
)


# ============================================================
# Save outputs
# ============================================================

saveRDS(
  main_table_2_diagnostics_robustness,
  "main_table_2_diagnostics_robustness.rds"
)

saveRDS(
  main_table_2_coefficient_details,
  "main_table_2_coefficient_details.rds"
)

write_csv(
  main_table_2_diagnostics_robustness,
  "main_table_2_diagnostics_robustness.csv"
)

write_lines(
  main_table_2_latex,
  "main_table_2_diagnostics_robustness.tex"
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
  coefficient_pretrend_bhj,
  coefficient_delta_ppml,
  coefficient_regional_controls,
  n_pretrend_bhj,
  n_delta_ppml,
  n_regional_controls,
  clusters_pretrend_bhj,
  clusters_delta_ppml,
  clusters_regional_controls,
  main_table_2_note,
  main_table_2_latex_rows
)


# ============================================================
# Final objects kept
# ============================================================
#
# Model objects loaded:
#   pretrend_bhj_stock_2010_2014
#   ppml_reduced_form_delta_1000
#   robustness_controls_ppml_reduced_form_stock_1000
#
# Table objects:
#   main_table_2_diagnostics_robustness
#   main_table_2_coefficient_details
#   main_table_2_latex
#
# Saved files:
#   main_table_2_diagnostics_robustness.rds
#   main_table_2_coefficient_details.rds
#   main_table_2_diagnostics_robustness.csv
#   main_table_2_diagnostics_robustness.tex
#
# Interpretation:
#   Main Table 2 summarizes:
#     - no evidence of differential pre-shock export changes,
#     - no positive delta-version PPML reduced-form evidence,
#     - no positive regional-control PPML evidence.
#
# ============================================================