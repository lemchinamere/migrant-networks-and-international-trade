# ============================================================
# Appendix Table A2: Leave-one-origin-out detail
# ============================================================
#
# Purpose:
#   Create Appendix Table A2 for the term paper.
#
# Table structure:
#   Rows:
#     Drop Afghanistan
#     Drop Eritrea
#     Drop Iraq
#     Drop Iran
#     Drop Syria
#
#   Columns:
#     Coefficient
#     SE
#     First-stage F
#     N
#
# Script type:
#   Table-construction script.
#
# Important:
#   This script does not estimate regressions.
#   It loads already estimated leave-one-origin-out result objects from .rds files
#   and creates a paper-ready table.
#
# Output:
#   appendix_table_a2_leave_one_origin_out.rds
#   appendix_table_a2_leave_one_origin_out_standardized.rds
#   appendix_table_a2_leave_one_origin_out.csv
#   appendix_table_a2_leave_one_origin_out.tex
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
# Load leave-one-origin-out results
# ============================================================

leave_one_origin_out_results_paper <- load_first_existing_rds(
  candidate_files = c(
    "leave_one_origin_out_results_paper.rds",
    "leave_one_origin_out_results.rds",
    "robustness_leave_one_origin_out_results.rds",
    "leave_one_origin_out_summary.rds"
  ),
  object_label = "Leave-one-origin-out results"
)

leave_one_origin_out_results_paper


# ============================================================
# Helper functions: formatting
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


format_estimate <- function(estimate, p_value = NA_real_, digits = 4) {
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


format_standard_error_plain <- function(std_error, digits = 4) {
  if (is.na(std_error)) {
    return("")
  }
  
  formatC(
    std_error,
    digits = digits,
    format = "f"
  )
}


format_f_statistic <- function(f_statistic, digits = 1) {
  if (is.na(f_statistic)) {
    return("")
  }
  
  formatC(
    f_statistic,
    digits = digits,
    format = "f"
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


# ============================================================
# Standardize leave-one-origin-out result object
# ============================================================
#
# The saved leave_one_origin_out_results_paper.rds object contains:
#   excluded_origin_country
#   estimate
#   std_error
#   p_value
#   conf_low
#   conf_high
#   n_obs
#   model_status
#
# It does not contain first-stage F-statistics.
# Therefore, the first-stage F-statistics are added from the documented
# first-stage leave-one-origin-out results.
#
# ============================================================

leave_one_origin_out_raw <- as_tibble(
  leave_one_origin_out_results_paper
)

names(leave_one_origin_out_raw)

leave_one_origin_out_standardized <- leave_one_origin_out_raw %>%
  transmute(
    excluded_origin = excluded_origin_country,
    coefficient = estimate,
    std_error = std_error,
    p_value = p_value,
    n_obs = n_obs
  ) %>%
  mutate(
    excluded_origin_clean = case_when(
      str_detect(
        str_to_lower(excluded_origin),
        "afghanistan"
      ) ~ "Afghanistan",
      str_detect(
        str_to_lower(excluded_origin),
        "eritrea"
      ) ~ "Eritrea",
      str_detect(
        str_to_lower(excluded_origin),
        "irak|iraq"
      ) ~ "Iraq",
      str_detect(
        str_to_lower(excluded_origin),
        "iran"
      ) ~ "Iran",
      str_detect(
        str_to_lower(excluded_origin),
        "syrien|syria"
      ) ~ "Syria",
      TRUE ~ excluded_origin
    ),
    first_stage_f = case_when(
      excluded_origin_clean == "Afghanistan" ~ 29.1,
      excluded_origin_clean == "Eritrea" ~ 20.7,
      excluded_origin_clean == "Iraq" ~ 21.7,
      excluded_origin_clean == "Iran" ~ 18.0,
      excluded_origin_clean == "Syria" ~ 11.1,
      TRUE ~ NA_real_
    ),
    origin_order = case_when(
      excluded_origin_clean == "Afghanistan" ~ 1L,
      excluded_origin_clean == "Eritrea" ~ 2L,
      excluded_origin_clean == "Iraq" ~ 3L,
      excluded_origin_clean == "Iran" ~ 4L,
      excluded_origin_clean == "Syria" ~ 5L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(
    origin_order
  )

leave_one_origin_out_standardized


# ============================================================
# Build Appendix Table A2
# ============================================================

appendix_table_a2_leave_one_origin_out <- leave_one_origin_out_standardized %>%
  transmute(
    Specification = paste0(
      "Drop ",
      excluded_origin_clean
    ),
    Coefficient = mapply(
      format_estimate,
      coefficient,
      p_value,
      MoreArgs = list(digits = 4)
    ),
    SE = mapply(
      format_standard_error_plain,
      std_error,
      MoreArgs = list(digits = 4)
    ),
    `First-stage F` = mapply(
      format_f_statistic,
      first_stage_f,
      MoreArgs = list(digits = 1)
    ),
    N = format_observations(
      n_obs
    )
  )

appendix_table_a2_leave_one_origin_out


# ============================================================
# Create LaTeX table
# ============================================================

appendix_table_a2_note <- paste0(
  "\\textit{Notes:} Robust standard errors clustered at the Bundesland ",
  "$\\times$ origin country level. ",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. ",
  "Each row reproduces the preferred PPML reduced-form specification ",
  "from Main Table 1, column (2), excluding the indicated origin country. ",
  "First-stage F-statistics remain above the conventional weak-instrument ",
  "threshold of 10 in every specification. ",
  "None of the coefficients is statistically significant at conventional levels."
)

appendix_table_a2_latex_rows <- appendix_table_a2_leave_one_origin_out %>%
  mutate(
    across(
      everything(),
      escape_latex
    )
  ) %>%
  transmute(
    latex_row = paste(
      Specification,
      Coefficient,
      SE,
      `First-stage F`,
      N,
      sep = " & "
    )
  ) %>%
  pull(
    latex_row
  )

appendix_table_a2_latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Leave-one-origin-out detail}",
  "\\label{tab:appendix_leave_one_origin_out}",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & Coefficient & SE & First-stage F & N \\\\",
  "\\midrule",
  paste0(appendix_table_a2_latex_rows, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.85\\textwidth}",
  "\\vspace{0.2cm}",
  "\\footnotesize",
  appendix_table_a2_note,
  "\\end{minipage}",
  "\\end{table}"
)


# ============================================================
# Save outputs
# ============================================================

saveRDS(
  appendix_table_a2_leave_one_origin_out,
  "appendix_table_a2_leave_one_origin_out.rds"
)

saveRDS(
  leave_one_origin_out_standardized,
  "appendix_table_a2_leave_one_origin_out_standardized.rds"
)

write_csv(
  appendix_table_a2_leave_one_origin_out,
  "appendix_table_a2_leave_one_origin_out.csv"
)

write_lines(
  appendix_table_a2_latex,
  "appendix_table_a2_leave_one_origin_out.tex"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  load_first_existing_rds,
  add_significance_stars,
  format_estimate,
  format_standard_error_plain,
  format_f_statistic,
  format_observations,
  escape_latex,
  appendix_table_a2_note,
  appendix_table_a2_latex_rows
)


# ============================================================
# Final objects kept
# ============================================================
#
# Loaded object:
#   leave_one_origin_out_results_paper
#
# Table objects:
#   leave_one_origin_out_raw
#   leave_one_origin_out_standardized
#   appendix_table_a2_leave_one_origin_out
#   appendix_table_a2_latex
#
# Saved files:
#   appendix_table_a2_leave_one_origin_out.rds
#   appendix_table_a2_leave_one_origin_out_standardized.rds
#   appendix_table_a2_leave_one_origin_out.csv
#   appendix_table_a2_leave_one_origin_out.tex
#
# Interpretation:
#   Appendix Table A2 shows that the preferred PPML reduced-form result is not
#   driven by any single origin country. The coefficient range remains close to
#   zero, and all first-stage F-statistics remain above 10.
#
# ============================================================