# ============================================================
# 29_results_documentation.R
# Results documentation for empirical analysis
# ============================================================
#
# Purpose:
#   Document the final empirical results used in the research proposal.
#
# Important:
#   This script does not estimate regressions.
#   It loads and documents already created tables, figures, and result objects.
#
# Main outputs:
#   results_documentation_overview.rds
#   results_documentation_overview.csv
#   results_table_figure_inventory.rds
#   results_table_figure_inventory.csv
#   results_interpretation_guide.rds
#   results_interpretation_guide.csv
#   results_caption_guide.rds
#   results_caption_guide.csv
#   results_final_file_check.rds
#   results_documentation_summary.md
#
# ============================================================


# ============================================================
# Setup
# ============================================================

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")

library(dplyr)
library(tibble)
library(readr)
library(stringr)


# ============================================================
# Helper functions
# ============================================================

load_optional_rds <- function(file) {
  if (file.exists(file)) {
    readRDS(file)
  } else {
    NULL
  }
}


format_file_status <- function(file) {
  ifelse(
    file.exists(file),
    "created",
    "missing"
  )
}


safe_nrow <- function(object) {
  if (is.null(object)) {
    return(NA_integer_)
  }
  
  if (is.data.frame(object)) {
    return(nrow(object))
  }
  
  return(NA_integer_)
}


# ============================================================
# Load final table objects if available
# ============================================================

main_table_1_results <- load_optional_rds(
  "main_table_1_results.rds"
)

main_table_2_diagnostics_robustness <- load_optional_rds(
  "main_table_2_diagnostics_robustness.rds"
)

appendix_table_a1_robustness <- load_optional_rds(
  "appendix_table_a1_robustness.rds"
)

appendix_table_a2_leave_one_origin_out <- load_optional_rds(
  "appendix_table_a2_leave_one_origin_out.rds"
)

event_study_main_linear_data <- load_optional_rds(
  "event_study_main_linear_data.rds"
)

appendix_figure_a1_ppml_event_study_data <- load_optional_rds(
  "appendix_figure_a1_ppml_event_study_data.rds"
)


# ============================================================
# Final results documentation overview
# ============================================================

results_documentation_overview <- tibble(
  result_block = c(
    "First stage",
    "PPML reduced form",
    "PPML benchmark",
    "Linear IV / 2SLS",
    "Control-function IV-style PPML",
    "BHJ pre-trend test",
    "Linear event study",
    "Delta-version PPML reduced form",
    "Regional-control robustness",
    "COVID-year exclusion robustness",
    "Drop Eritrea robustness",
    "Export-weight robustness",
    "Delta-endpoint robustness",
    "Leave-one-origin-out robustness",
    "PPML event study"
  ),
  location_in_paper = c(
    "Main Table 1, column 1",
    "Main Table 1, column 2",
    "Main Table 1, column 3",
    "Main Table 1, column 4",
    "Main Table 1, column 5",
    "Main Table 2, column 1",
    "Main Figure 1",
    "Main Table 2, column 2",
    "Main Table 2, column 3",
    "Appendix Table A1, column 1",
    "Appendix Table A1, column 2",
    "Appendix Table A1, column 3",
    "Appendix Table A1, column 4",
    "Appendix Table A1 column 5 and Appendix Table A2",
    "Appendix Figure A1"
  ),
  key_estimate_or_pattern = c(
    "0.998*** (0.206), first-stage F = 23.4",
    "-0.0001 (0.0044)",
    "0.0001 (0.0032)",
    "0.0064 (0.0072)",
    "-0.0003 (0.0047), residual 0.0009 (0.0103)",
    "0.0026 (0.0175), p = 0.885",
    "Year-specific coefficients statistically insignificant",
    "-0.0001 (0.0049)",
    "-0.0109 (0.0069)",
    "-0.0015 (0.0043)",
    "-0.0001 (0.0045)",
    "0.0055 (0.0153)",
    "-0.0001 (0.0076)",
    "Range [-0.0020, 0.0018], none significant",
    "No positive post-shock response; several coefficients negative; interpreted cautiously"
  ),
  interpretation = c(
    "The instrument is strongly relevant.",
    "The preferred PPML reduced form provides no evidence of a positive export response.",
    "The non-instrumented PPML benchmark also shows no positive association.",
    "The null result is robust to a familiar linear IV / 2SLS specification.",
    "The null result is robust to the control-function IV-style PPML specification.",
    "There is no evidence of differential pre-shock export growth.",
    "The main visual diagnostic supports the null result.",
    "The null is robust to measuring exposure as the 2014–2016 change.",
    "The null is not mechanically driven by saturated federal-state-year fixed effects.",
    "The null is not driven by COVID-period trade disruptions.",
    "The null is not driven by Eritrea.",
    "The null is not driven by using export value instead of export weight.",
    "The null is not driven by using 2017 rather than 2016 as the endpoint.",
    "The null is not driven by any single origin country.",
    "The PPML event study is consistent with the null but visually less clean because 2014 is a single high reference year."
  ),
  final_use = c(
    "Main evidence for relevance",
    "Preferred outcome specification",
    "Benchmark comparison",
    "Robustness check",
    "Robustness check",
    "Formal pre-trend validity check",
    "Main visual diagnostic",
    "Additional main specification",
    "Additional main specification",
    "Appendix robustness",
    "Appendix robustness",
    "Appendix robustness",
    "Appendix robustness",
    "Appendix robustness",
    "Appendix robustness figure"
  )
)

results_documentation_overview


# ============================================================
# Table and figure inventory
# ============================================================

results_table_figure_inventory <- tibble(
  item = c(
    "Main Table 1",
    "Main Table 2",
    "Main Figure 1",
    "Appendix Table A1",
    "Appendix Table A2",
    "Appendix Figure A1"
  ),
  title = c(
    "Main Results",
    "Pre-trend Test and Additional Main Specifications",
    "Linear Event-Study Estimates",
    "Consolidated Robustness Package",
    "Leave-One-Origin-Out Detail",
    "PPML Event-Study Estimates"
  ),
  tex_or_pdf_file = c(
    "main_table_1_results.tex",
    "main_table_2_diagnostics_robustness.tex",
    "event_study_main_linear.pdf",
    "appendix_table_a1_robustness.tex",
    "appendix_table_a2_leave_one_origin_out.tex",
    "appendix_figure_a1_ppml_event_study.pdf"
  ),
  rds_file = c(
    "main_table_1_results.rds",
    "main_table_2_diagnostics_robustness.rds",
    "event_study_main_linear_plot.rds",
    "appendix_table_a1_robustness.rds",
    "appendix_table_a2_leave_one_origin_out.rds",
    "appendix_figure_a1_ppml_event_study_plot.rds"
  ),
  csv_or_data_file = c(
    "main_table_1_results.csv",
    "main_table_2_diagnostics_robustness.csv",
    "event_study_main_linear_data.csv",
    "appendix_table_a1_robustness.csv",
    "appendix_table_a2_leave_one_origin_out.csv",
    "appendix_figure_a1_ppml_event_study_data.csv"
  ),
  created_by_script = c(
    "23_table_main_results.R",
    "24_table_main_diagnostics_and_robustness.R",
    "25_figure_event_study_main.R",
    "26_table_appendix_robustness.R",
    "27_table_leave_one_origin_out.R",
    "28_figure_appendix_event_study_main.R"
  ),
  status_main_file = format_file_status(
    c(
      "main_table_1_results.tex",
      "main_table_2_diagnostics_robustness.tex",
      "event_study_main_linear.pdf",
      "appendix_table_a1_robustness.tex",
      "appendix_table_a2_leave_one_origin_out.tex",
      "appendix_figure_a1_ppml_event_study.pdf"
    )
  )
)

results_table_figure_inventory


# ============================================================
# Interpretation guide
# ============================================================

results_interpretation_guide <- tibble(
  writing_context = c(
    "Overall result",
    "First stage",
    "Main PPML reduced form",
    "Linear IV / 2SLS",
    "Control-function IV-style PPML",
    "Pre-trend evidence",
    "Main event study",
    "PPML appendix event study",
    "Robustness checks",
    "Scope condition",
    "Avoided claim"
  ),
  recommended_wording = c(
    "The estimates provide no evidence of a positive export response in this setting and over this horizon.",
    "The Königstein-based instrument strongly predicts actual protection-seeker exposure.",
    "The preferred PPML reduced form is close to zero and statistically insignificant.",
    "The linear IV estimate is statistically insignificant and supports the same null pattern.",
    "The control-function IV-style PPML specification also yields no evidence of a positive export response.",
    "The BHJ-style pre-trend test provides no evidence that later high-exposure pairs were already on differential pre-shock export trends.",
    "The linear event-study figure provides the main visual diagnostic and shows no clear differential post-shock response.",
    "The PPML event study is reported in the appendix because its 2014 reference year makes the dynamic pattern visually less clean, although it is consistent with the null result.",
    "The null pattern is robust to alternative samples, outcomes, exposure definitions, and origin-country exclusions.",
    "The findings apply to the selected refugee-origin countries, German Länder, and the observed time horizon.",
    "Do not write that migration has no effect on trade."
  ),
  reason = c(
    "This is precise and avoids overclaiming.",
    "This supports instrument relevance but not exclusion or exogeneity by itself.",
    "This is the preferred trade-flow outcome specification.",
    "This shows the null is not specific to PPML.",
    "This shows the null is not specific to the reduced-form PPML approach.",
    "This is the formal pre-trend validity check.",
    "This reflects the supervisor's recommendation.",
    "This prevents readers from misinterpreting the PPML event study as the main validity test.",
    "This supports the credibility of the informative null result.",
    "This keeps the conclusion appropriately bounded.",
    "The empirical design cannot prove a universal zero effect of migration on trade."
  )
)

results_interpretation_guide


# ============================================================
# Caption guide
# ============================================================

results_caption_guide <- tibble(
  item = c(
    "Main Table 1",
    "Main Table 2",
    "Main Figure 1",
    "Appendix Table A1",
    "Appendix Table A2",
    "Appendix Figure A1"
  ),
  suggested_caption = c(
    "Main results.",
    "Pre-trend test and additional main specifications.",
    "Linear event-study estimates.",
    "Consolidated robustness package.",
    "Leave-one-origin-out detail.",
    "PPML event-study estimates."
  ),
  suggested_note_or_caption_text = c(
    paste0(
      "Robust standard errors clustered at the Bundesland × origin-country level ",
      "in parentheses. The instrument is the Königstein-predicted protection-seeker ",
      "stock in 2016 interacted with a post-2016 indicator, scaled per 1,000 persons. ",
      "The treatment is the corresponding actual stock. Column (5) reports the ",
      "control-function IV-style PPML specification."
    ),
    paste0(
      "Column (1) reports the BHJ-style pre-trend test. Column (2) reports the ",
      "delta-version PPML reduced form based on the 2014–2016 exposure change. ",
      "Column (3) replaces Bundesland × year fixed effects with explicit regional controls."
    ),
    paste0(
      "The figure plots year-specific coefficients from the linear event-study specification, ",
      "with 2014 as the reference year. The shaded band indicates the 2015–2016 refugee ",
      "shock period. The pre-period coefficients serve as a visual pre-trend diagnostic; ",
      "the formal BHJ pre-trend test is reported in Main Table 2, column 1."
    ),
    paste0(
      "Each column reproduces the preferred PPML reduced-form specification under the ",
      "indicated robustness condition. Column (5) reports the range of point estimates ",
      "across five leave-one-origin-out regressions."
    ),
    paste0(
      "Each row reproduces the preferred PPML reduced-form specification excluding the ",
      "indicated origin country. First-stage F-statistics remain above the conventional ",
      "weak-instrument threshold of 10 in every specification."
    ),
    paste0(
      "The figure plots year-specific PPML coefficients with 2014 as the reference year. ",
      "Most coefficients lie below zero, including both pre- and post-shock years. ",
      "This pattern likely reflects the choice of 2014 as a single high reference year ",
      "rather than a differential post-shock effect. The figure is therefore interpreted ",
      "as a descriptive robustness diagnostic."
    )
  )
)

results_caption_guide


# ============================================================
# Final file check
# ============================================================

results_final_file_check <- tibble(
  file = c(
    "main_table_1_results.tex",
    "main_table_1_results.rds",
    "main_table_1_results.csv",
    "main_table_2_diagnostics_robustness.tex",
    "main_table_2_diagnostics_robustness.rds",
    "main_table_2_diagnostics_robustness.csv",
    "event_study_main_linear.pdf",
    "event_study_main_linear.png",
    "event_study_main.pdf",
    "event_study_main.png",
    "appendix_table_a1_robustness.tex",
    "appendix_table_a1_robustness.rds",
    "appendix_table_a1_robustness.csv",
    "appendix_table_a2_leave_one_origin_out.tex",
    "appendix_table_a2_leave_one_origin_out.rds",
    "appendix_table_a2_leave_one_origin_out.csv",
    "appendix_figure_a1_ppml_event_study.pdf",
    "appendix_figure_a1_ppml_event_study.png",
    "appendix_figure_a1_ppml_event_study_data.rds"
  ),
  exists = file.exists(file)
)

results_final_file_check


# ============================================================
# Loaded object check
# ============================================================

results_loaded_object_check <- tibble(
  object_name = c(
    "main_table_1_results",
    "main_table_2_diagnostics_robustness",
    "appendix_table_a1_robustness",
    "appendix_table_a2_leave_one_origin_out",
    "event_study_main_linear_data",
    "appendix_figure_a1_ppml_event_study_data"
  ),
  loaded = c(
    !is.null(main_table_1_results),
    !is.null(main_table_2_diagnostics_robustness),
    !is.null(appendix_table_a1_robustness),
    !is.null(appendix_table_a2_leave_one_origin_out),
    !is.null(event_study_main_linear_data),
    !is.null(appendix_figure_a1_ppml_event_study_data)
  ),
  n_rows = c(
    safe_nrow(main_table_1_results),
    safe_nrow(main_table_2_diagnostics_robustness),
    safe_nrow(appendix_table_a1_robustness),
    safe_nrow(appendix_table_a2_leave_one_origin_out),
    safe_nrow(event_study_main_linear_data),
    safe_nrow(appendix_figure_a1_ppml_event_study_data)
  )
)

results_loaded_object_check


# ============================================================
# Compact markdown summary
# ============================================================

results_documentation_summary <- c(
  "# Results Documentation Summary",
  "",
  "## Main empirical conclusion",
  "",
  "The empirical results provide no evidence of a positive export response to refugee-induced regional exposure in this setting and over this horizon.",
  "",
  "This should be interpreted as an informative null result, not as proof that migrant networks never affect trade.",
  "",
  "## Main outputs",
  "",
  "- Main Table 1: Main Results",
  "- Main Table 2: Pre-trend Test and Additional Main Specifications",
  "- Main Figure 1: Linear Event-Study Estimates",
  "- Appendix Table A1: Consolidated Robustness Package",
  "- Appendix Table A2: Leave-One-Origin-Out Detail",
  "- Appendix Figure A1: PPML Event-Study Estimates",
  "",
  "## Core result pattern",
  "",
  "- The first stage is strong: 0.998*** with first-stage F = 23.4.",
  "- The preferred PPML reduced form is essentially zero: -0.0001 (0.0044).",
  "- The non-instrumented PPML benchmark is also essentially zero.",
  "- The linear IV / 2SLS estimate is statistically insignificant.",
  "- The control-function IV-style PPML estimate is statistically insignificant.",
  "- The BHJ pre-trend test provides no evidence of differential pre-shock export growth.",
  "- The linear event study is the main visual diagnostic.",
  "- The PPML event study is kept as an appendix robustness diagnostic.",
  "",
  "## Recommended wording",
  "",
  "Preferred wording:",
  "",
  "The estimates provide no evidence of a positive export response in this setting and over this horizon.",
  "",
  "Avoided wording:",
  "",
  "Migration has no effect on trade.",
  "",
  "## Robustness interpretation",
  "",
  "The null pattern is robust to excluding COVID years, excluding Eritrea, using export weight, using the 2014–2017 delta endpoint, and excluding each origin country one at a time.",
  "",
  "## Event-study interpretation",
  "",
  "The main figure should use the linear event study. The PPML event study should be reported in the appendix because the 2014 reference year makes the pattern visually less clean, although it remains consistent with the null result."
)

write_lines(
  results_documentation_summary,
  "results_documentation_summary.md"
)


# ============================================================
# Save documentation objects
# ============================================================

saveRDS(
  results_documentation_overview,
  "results_documentation_overview.rds"
)

write_csv(
  results_documentation_overview,
  "results_documentation_overview.csv"
)

saveRDS(
  results_table_figure_inventory,
  "results_table_figure_inventory.rds"
)

write_csv(
  results_table_figure_inventory,
  "results_table_figure_inventory.csv"
)

saveRDS(
  results_interpretation_guide,
  "results_interpretation_guide.rds"
)

write_csv(
  results_interpretation_guide,
  "results_interpretation_guide.csv"
)

saveRDS(
  results_caption_guide,
  "results_caption_guide.rds"
)

write_csv(
  results_caption_guide,
  "results_caption_guide.csv"
)

saveRDS(
  results_final_file_check,
  "results_final_file_check.rds"
)

write_csv(
  results_final_file_check,
  "results_final_file_check.csv"
)

saveRDS(
  results_loaded_object_check,
  "results_loaded_object_check.rds"
)

write_csv(
  results_loaded_object_check,
  "results_loaded_object_check.csv"
)


# ============================================================
# Combined documentation object
# ============================================================

results_documentation_complete <- list(
  results_documentation_overview = results_documentation_overview,
  results_table_figure_inventory = results_table_figure_inventory,
  results_interpretation_guide = results_interpretation_guide,
  results_caption_guide = results_caption_guide,
  results_final_file_check = results_final_file_check,
  results_loaded_object_check = results_loaded_object_check,
  results_documentation_summary = results_documentation_summary
)

saveRDS(
  results_documentation_complete,
  "results_documentation_complete.rds"
)


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  load_optional_rds,
  format_file_status,
  safe_nrow
)


# ============================================================
# Final objects kept
# ============================================================
#
# Loaded final result objects:
#   main_table_1_results
#   main_table_2_diagnostics_robustness
#   appendix_table_a1_robustness
#   appendix_table_a2_leave_one_origin_out
#   event_study_main_linear_data
#   appendix_figure_a1_ppml_event_study_data
#
# Documentation objects:
#   results_documentation_overview
#   results_table_figure_inventory
#   results_interpretation_guide
#   results_caption_guide
#   results_final_file_check
#   results_loaded_object_check
#   results_documentation_summary
#   results_documentation_complete
#
# Saved files:
#   results_documentation_overview.rds
#   results_documentation_overview.csv
#   results_table_figure_inventory.rds
#   results_table_figure_inventory.csv
#   results_interpretation_guide.rds
#   results_interpretation_guide.csv
#   results_caption_guide.rds
#   results_caption_guide.csv
#   results_final_file_check.rds
#   results_final_file_check.csv
#   results_loaded_object_check.rds
#   results_loaded_object_check.csv
#   results_documentation_summary.md
#   results_documentation_complete.rds
#
# ============================================================