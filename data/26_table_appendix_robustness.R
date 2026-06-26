# ============================================================
# Appendix Table A1: Consolidated robustness package
# ============================================================
#
# Purpose:
#   Create Appendix Table A1 for the term paper.
#
# Table structure:
#   (1) Drop COVID
#   (2) Drop Eritrea
#   (3) Export weight
#   (4) Delta endpoint 2014–2017
#   (5) Leave-one-origin-out range
#
# Script type:
#   Table-construction script.
#
# Important:
#   This script does not estimate regressions.
#   It loads already estimated robustness objects from .rds files and creates
#   a paper-ready table.
#
# Output:
#   appendix_table_a1_robustness.rds
#   appendix_table_a1_coefficient_details.rds
#   appendix_table_a1_robustness.csv
#   appendix_table_a1_robustness.tex
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
# Helper function: load first existing file from ordered list
# ============================================================

load_first_existing_rds <- function(candidate_files, object_label) {
  
  existing_files <- candidate_files[
    file.exists(candidate_files)
  ]
  
  if (length(existing_files) == 0) {
    stop(
      paste(
        "No .rds file found for:",
        object_label,
        "Checked candidates:",
        paste(candidate_files, collapse = ", "),
        "Available .rds files:",
        paste(
          list.files(
            pattern = "\\.rds$",
            ignore.case = TRUE
          ),
          collapse = ", "
        )
      )
    )
  }
  
  selected_file <- existing_files[1]
  
  message(
    paste(
      "Loading",
      object_label,
      "from",
      selected_file
    )
  )
  
  readRDS(selected_file)
}


# ============================================================
# Load robustness objects
# ============================================================
#
# Column (1): Drop COVID
# Column (2): Drop Eritrea
# Column (3): Export weight
# Column (4): Delta endpoint 2014–2017
# Column (5): Leave-one-origin-out range
#
# ============================================================

robustness_covid_ppml_reduced_form_stock_1000 <- load_first_existing_rds(
  candidate_files = c(
    "robustness_covid_ppml_reduced_form_stock_1000.rds",
    "covid_exclusion_ppml_reduced_form_stock_1000.rds",
    "ppml_reduced_form_stock_1000_no_covid.rds",
    "ppml_reduced_form_stock_excluding_covid_1000.rds"
  ),
  object_label = "Drop COVID PPML reduced form"
)


# Correct simple no-Eritrea PPML reduced-form stock specification.
# Do not load summary, CEPII, regional-control, or COVID no-Eritrea files here.

robustness_no_eritrea_ppml_reduced_form_stock_1000 <- readRDS(
  "ppml_reduced_form_stock_no_eritrea_1000.rds"
)


robustness_export_weight_ppml_reduced_form_stock_1000 <- load_first_existing_rds(
  candidate_files = c(
    "ppml_reduced_form_weight_stock_1000.rds",
    "ppml_reduced_form_export_weight_stock_1000.rds",
    "robustness_export_weight_ppml_reduced_form_stock_1000.rds",
    "export_weight_ppml_reduced_form_stock_1000.rds"
  ),
  object_label = "Export weight PPML reduced form"
)


# Correct endpoint PPML reduced-form robustness specification.

robustness_delta_endpoint_ppml_reduced_form <- readRDS(
  "robustness_delta_endpoint_ppml_reduced_form_1000.rds"
)


leave_one_origin_out_results_paper <- load_first_existing_rds(
  candidate_files = c(
    "leave_one_origin_out_results_paper.rds",
    "leave_one_origin_out_results.rds",
    "robustness_leave_one_origin_out_results.rds",
    "leave_one_origin_out_summary.rds"
  ),
  object_label = "Leave-one-origin-out results"
)


# ============================================================
# Helper functions: extraction and formatting
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


extract_leave_one_origin_out_range <- function(leave_one_origin_out_results) {
  
  leave_one_origin_out_df <- as_tibble(
    leave_one_origin_out_results
  )
  
  estimate_column <- names(leave_one_origin_out_df)[
    str_detect(
      names(leave_one_origin_out_df),
      regex("estimate|coefficient|coef", ignore_case = TRUE)
    )
  ][1]
  
  n_column <- names(leave_one_origin_out_df)[
    str_detect(
      names(leave_one_origin_out_df),
      regex("^n$|n_obs|observations", ignore_case = TRUE)
    )
  ][1]
  
  cluster_column <- names(leave_one_origin_out_df)[
    str_detect(
      names(leave_one_origin_out_df),
      regex("cluster", ignore_case = TRUE)
    )
  ][1]
  
  if (is.na(estimate_column)) {
    stop(
      paste(
        "Could not identify the coefficient column in leave_one_origin_out_results_paper.",
        "Available columns:",
        paste(names(leave_one_origin_out_df), collapse = ", ")
      )
    )
  }
  
  estimate_values <- leave_one_origin_out_df[[estimate_column]]
  
  estimate_min <- min(
    estimate_values,
    na.rm = TRUE
  )
  
  estimate_max <- max(
    estimate_values,
    na.rm = TRUE
  )
  
  if (!is.na(n_column)) {
    n_min <- min(
      leave_one_origin_out_df[[n_column]],
      na.rm = TRUE
    )
    
    n_max <- max(
      leave_one_origin_out_df[[n_column]],
      na.rm = TRUE
    )
  } else {
    n_min <- 991
    n_max <- 1020
  }
  
  if (!is.na(cluster_column)) {
    cluster_min <- min(
      leave_one_origin_out_df[[cluster_column]],
      na.rm = TRUE
    )
    
    cluster_max <- max(
      leave_one_origin_out_df[[cluster_column]],
      na.rm = TRUE
    )
  } else {
    cluster_min <- 64
    cluster_max <- 80
  }
  
  tibble(
    estimate_min = estimate_min,
    estimate_max = estimate_max,
    n_min = n_min,
    n_max = n_max,
    cluster_min = cluster_min,
    cluster_max = cluster_max
  )
}


# ============================================================
# Extract coefficients
# ============================================================

coefficient_drop_covid <- extract_model_coefficient(
  model = robustness_covid_ppml_reduced_form_stock_1000,
  coefficient_patterns = c(
    "^iv_stock_2016_post_1000$"
  ),
  model_label = "Drop COVID"
)

coefficient_drop_eritrea <- extract_model_coefficient(
  model = robustness_no_eritrea_ppml_reduced_form_stock_1000,
  coefficient_patterns = c(
    "^iv_stock_2016_post_1000$"
  ),
  model_label = "Drop Eritrea"
)

coefficient_export_weight <- extract_model_coefficient(
  model = robustness_export_weight_ppml_reduced_form_stock_1000,
  coefficient_patterns = c(
    "^iv_stock_2016_post_1000$"
  ),
  model_label = "Export weight"
)

coefficient_delta_endpoint <- extract_model_coefficient(
  model = robustness_delta_endpoint_ppml_reduced_form,
  coefficient_patterns = c(
    "^iv_delta_2014_2017_post_1000$",
    "^iv_delta_endpoint_post_1000$",
    "^iv_delta_post_1000$"
  ),
  model_label = "Delta endpoint 2014–2017"
)

leave_one_origin_out_range <- extract_leave_one_origin_out_range(
  leave_one_origin_out_results_paper
)


# ============================================================
# Diagnostic statistics
# ============================================================

n_drop_covid <- nobs(
  robustness_covid_ppml_reduced_form_stock_1000
)

n_drop_eritrea <- nobs(
  robustness_no_eritrea_ppml_reduced_form_stock_1000
)

n_export_weight <- nobs(
  robustness_export_weight_ppml_reduced_form_stock_1000
)

n_delta_endpoint <- nobs(
  robustness_delta_endpoint_ppml_reduced_form
)

clusters_drop_covid <- extract_number_of_clusters(
  robustness_covid_ppml_reduced_form_stock_1000,
  fallback_clusters = 80L
)

clusters_drop_eritrea <- extract_number_of_clusters(
  robustness_no_eritrea_ppml_reduced_form_stock_1000,
  fallback_clusters = 64L
)

clusters_export_weight <- extract_number_of_clusters(
  robustness_export_weight_ppml_reduced_form_stock_1000,
  fallback_clusters = 80L
)

clusters_delta_endpoint <- extract_number_of_clusters(
  robustness_delta_endpoint_ppml_reduced_form,
  fallback_clusters = 80L
)


# ============================================================
# Build Appendix Table A1
# ============================================================

leave_one_origin_out_range_cell <- paste0(
  "[",
  formatC(
    leave_one_origin_out_range$estimate_min,
    digits = 4,
    format = "f"
  ),
  ", ",
  formatC(
    leave_one_origin_out_range$estimate_max,
    digits = 4,
    format = "f"
  ),
  "]"
)

leave_one_origin_out_n_cell <- paste0(
  format_observations(
    leave_one_origin_out_range$n_min
  ),
  "–",
  format_observations(
    leave_one_origin_out_range$n_max
  )
)

leave_one_origin_out_cluster_cell <- paste0(
  format_observations(
    leave_one_origin_out_range$cluster_min
  ),
  "–",
  format_observations(
    leave_one_origin_out_range$cluster_max
  )
)


appendix_table_a1_robustness <- tibble(
  row = c(
    "IV: predicted exposure (/1,000)",
    "",
    "Outcome",
    "Estimator",
    "Sample period",
    "Fixed effects:",
    "  Bundesland × origin",
    "  Bundesland × year",
    "  Origin × year",
    "Observations",
    "Clusters (pair)"
  ),
  `Drop COVID` = c(
    format_estimate(
      coefficient_drop_covid$estimate,
      coefficient_drop_covid$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_drop_covid$std_error,
      digits = 4
    ),
    "Exports",
    "fepois",
    "excl. 2020 & 2021",
    "",
    "Yes",
    "Yes",
    "Yes",
    format_observations(
      n_drop_covid
    ),
    format_observations(
      clusters_drop_covid
    )
  ),
  `Drop Eritrea` = c(
    format_estimate(
      coefficient_drop_eritrea$estimate,
      coefficient_drop_eritrea$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_drop_eritrea$std_error,
      digits = 4
    ),
    "Exports",
    "fepois",
    "2010–2025",
    "",
    "Yes",
    "Yes",
    "Yes",
    format_observations(
      n_drop_eritrea
    ),
    format_observations(
      clusters_drop_eritrea
    )
  ),
  `Export weight` = c(
    format_estimate(
      coefficient_export_weight$estimate,
      coefficient_export_weight$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_export_weight$std_error,
      digits = 4
    ),
    "Weight",
    "fepois",
    "2010–2025",
    "",
    "Yes",
    "Yes",
    "Yes",
    format_observations(
      n_export_weight
    ),
    format_observations(
      clusters_export_weight
    )
  ),
  `Delta endpoint 2014–2017` = c(
    format_estimate(
      coefficient_delta_endpoint$estimate,
      coefficient_delta_endpoint$p_value,
      digits = 4
    ),
    format_standard_error(
      coefficient_delta_endpoint$std_error,
      digits = 4
    ),
    "Exports",
    "fepois",
    "2010–2025",
    "",
    "Yes",
    "Yes",
    "Yes",
    format_observations(
      n_delta_endpoint
    ),
    format_observations(
      clusters_delta_endpoint
    )
  ),
  `Leave-one-origin-out range` = c(
    leave_one_origin_out_range_cell,
    "",
    "Exports",
    "fepois",
    "varies",
    "",
    "Yes",
    "Yes",
    "Yes",
    leave_one_origin_out_n_cell,
    leave_one_origin_out_cluster_cell
  )
)

appendix_table_a1_robustness


# ============================================================
# Long-format coefficient table for documentation
# ============================================================

appendix_table_a1_coefficient_details <- bind_rows(
  coefficient_drop_covid,
  coefficient_drop_eritrea,
  coefficient_export_weight,
  coefficient_delta_endpoint
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

appendix_table_a1_coefficient_details

leave_one_origin_out_range


# ============================================================
# Create LaTeX table
# ============================================================

appendix_table_a1_note <- paste0(
  "\\textit{Notes:} Robust standard errors clustered at the Bundesland ",
  "$\\times$ origin country level in parentheses. ",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. ",
  "Each column reproduces the preferred PPML reduced-form specification ",
  "from Main Table 1, column (2), under the indicated robustness condition. ",
  "Column (5) reports the range of point estimates across five regressions, ",
  "each excluding one of the five origin countries; the full set of ",
  "leave-one-origin-out estimates appears in Appendix Table A2. ",
  "The instrument in column (4) is the 2014--2017 predicted delta exposure. ",
  "None of the coefficients is statistically significant at conventional levels."
)

appendix_table_a1_latex_rows <- appendix_table_a1_robustness %>%
  mutate(
    across(
      everything(),
      escape_latex
    )
  ) %>%
  transmute(
    latex_row = paste(
      row,
      `Drop COVID`,
      `Drop Eritrea`,
      `Export weight`,
      `Delta endpoint 2014–2017`,
      `Leave-one-origin-out range`,
      sep = " & "
    )
  ) %>%
  pull(
    latex_row
  )

appendix_table_a1_latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Consolidated robustness package}",
  "\\label{tab:appendix_robustness}",
  "\\small",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) & (5) \\\\",
  " & Drop COVID & Drop Eritrea & Export weight & Delta endpoint & Leave-one-origin-out \\\\",
  " &  &  &  & 2014--2017 & range \\\\",
  "\\midrule",
  paste0(appendix_table_a1_latex_rows[1:2], " \\\\"),
  "\\midrule",
  paste0(appendix_table_a1_latex_rows[3:5], " \\\\"),
  "\\midrule",
  paste0(appendix_table_a1_latex_rows[6:9], " \\\\"),
  "\\midrule",
  paste0(appendix_table_a1_latex_rows[10:11], " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\vspace{0.2cm}",
  "\\footnotesize",
  appendix_table_a1_note,
  "\\end{minipage}",
  "\\end{table}"
)


# ============================================================
# Save outputs
# ============================================================

saveRDS(
  appendix_table_a1_robustness,
  "appendix_table_a1_robustness.rds"
)

saveRDS(
  appendix_table_a1_coefficient_details,
  "appendix_table_a1_coefficient_details.rds"
)

write_csv(
  appendix_table_a1_robustness,
  "appendix_table_a1_robustness.csv"
)

write_lines(
  appendix_table_a1_latex,
  "appendix_table_a1_robustness.tex"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  load_first_existing_rds,
  add_significance_stars,
  format_estimate,
  format_standard_error,
  format_observations,
  escape_latex,
  extract_model_coefficient,
  extract_number_of_clusters,
  extract_leave_one_origin_out_range,
  coefficient_drop_covid,
  coefficient_drop_eritrea,
  coefficient_export_weight,
  coefficient_delta_endpoint,
  n_drop_covid,
  n_drop_eritrea,
  n_export_weight,
  n_delta_endpoint,
  clusters_drop_covid,
  clusters_drop_eritrea,
  clusters_export_weight,
  clusters_delta_endpoint,
  leave_one_origin_out_range_cell,
  leave_one_origin_out_n_cell,
  leave_one_origin_out_cluster_cell,
  appendix_table_a1_note,
  appendix_table_a1_latex_rows
)


# ============================================================
# Final objects kept
# ============================================================
#
# Loaded model/result objects:
#   robustness_covid_ppml_reduced_form_stock_1000
#   robustness_no_eritrea_ppml_reduced_form_stock_1000
#   robustness_export_weight_ppml_reduced_form_stock_1000
#   robustness_delta_endpoint_ppml_reduced_form
#   leave_one_origin_out_results_paper
#
# Table objects:
#   appendix_table_a1_robustness
#   appendix_table_a1_coefficient_details
#   leave_one_origin_out_range
#   appendix_table_a1_latex
#
# Saved files:
#   appendix_table_a1_robustness.rds
#   appendix_table_a1_coefficient_details.rds
#   appendix_table_a1_robustness.csv
#   appendix_table_a1_robustness.tex
#
# Interpretation:
#   Appendix Table A1 summarizes robustness checks around the preferred
#   PPML reduced-form specification. The results provide no evidence of a
#   positive export response across the alternative samples and specifications.
#
# ============================================================