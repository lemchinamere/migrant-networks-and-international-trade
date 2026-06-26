# ============================================================
# Regression structure for empirical analysis
# ============================================================
#
# Purpose:
#   Document the final empirical regression structure used in the research
#   proposal.
#
# Important:
#   This script does not estimate regressions.
#   It creates documentation objects that summarize:
#     - the final technical script order,
#     - the paper structure,
#     - the main panels,
#     - the main variables,
#     - the fixed-effect structures,
#     - the active empirical outputs,
#     - the final tables and figures,
#     - archived or non-emphasized checks.
#
# Research topic:
#   Migrant Networks and International Trade.
#
# Empirical setting:
#   Protection seekers in German Länder from the 2015/16 asylum-
#   seeker wave and exports from German Länder to selected origin
#   countries.
#
# Unit of observation:
#   federal_state × origin_country × year
#
# Selected origin countries:
#   Afghanistan
#   Eritrea
#   Irak (Iraq)
#   Iran, Islamische Republik (Iran, Islamic Republic)
#   Syrien (Syria)
#
# Main period:
#   2010–2025
#
# Period structure:
#   2010–2014 = pre-period
#   2015–2016 = refugee shock period
#   2017–2025 = post-period
#
# Final interpretation:
#   The empirical package provides an informative null result:
#   the data do not provide evidence of a positive export response in this
#   setting and over this horizon.
#
#   Do not write:
#     migration has no effect on trade.
#
#   Prefer:
#     The estimates provide no evidence of a positive export response in this
#     setting and over this horizon.
#
# ============================================================


# ============================================================
# Setup
# ============================================================

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")

### Locale: read the UTF-8 data files correctly regardless of the ambient
### locale. A bare Rscript in a C/POSIX locale otherwise mis-reads the UTF-8
### CSVs (only the interactive RStudio/R.app UTF-8 locale would work).
for (.utf8_locale in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
  if (suppressWarnings(Sys.setlocale("LC_CTYPE", .utf8_locale)) != "") break
}
rm(.utf8_locale)

library(dplyr)
library(tibble)


# ============================================================
# 1. Final technical script order
# ============================================================

final_script_order <- tibble(
  script_order = c(
    1,
    2,
    3,
    4,
    4.5,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    99
  ),
  script_file = c(
    "01_outcome.R",
    "02_treatment.R",
    "03_instrument.R",
    "04_analysis.R",
    "04b_create_no_eritrea_objects.R",
    "05_controls.R",
    "06_rescaling.R",
    "07_fixed_effects.R",
    "08_delta_endpoint_variables.R",
    "09_data_structure.R",
    "10_sources.R",
    "11_first_stage_relevance.R",
    "12_ppml_reduced_form.R",
    "13_ppml_benchmark.R",
    "14_linear_reduced_form_iv.R",
    "15_control_function_iv_ppml.R",
    "16_pretrend_bhj_check.R",
    "17_event_study.R",
    "18_regional_control_robustness.R",
    "19_covid_exclusion_robustness.R",
    "20_delta_endpoints_robustness.R",
    "21_leave_one_origin_out_robustness.R",
    "22_regression_structure.R",
    "23_table_main_results.R",
    "24_table_main_diagnostics_and_robustness.R",
    "25_figure_event_study_main.R",
    "26_table_appendix_robustness.R",
    "27_table_leave_one_origin_out.R",
    "28_figure_appendix_event_study_main.R",
    "99_archived_cepii_gravity_robustness.R"
  ),
  script_role = c(
    "Construct outcome data from German federal-state export data",
    "Construct actual protection-seeker treatment variables",
    "Construct Königstein-predicted instrument variables",
    "Construct main analysis panel",
    "Create no-Eritrea panel objects needed by later scripts",
    "Construct and merge regional control variables",
    "Create scaled treatment and instrument variables",
    "Create fixed-effect identifiers",
    "Construct 2014–2017 delta-endpoint variables",
    "Document data structure and panel diagnostics",
    "Document sources used in the empirical analysis",
    "Estimate first-stage relevance regressions",
    "Estimate PPML reduced-form regressions",
    "Estimate non-instrumented PPML benchmark regressions",
    "Estimate linear reduced-form and linear IV / 2SLS robustness checks",
    "Estimate control-function IV-style PPML robustness checks",
    "Estimate BHJ-style pre-trend tests",
    "Estimate event-study diagnostics",
    "Estimate regional-control robustness checks",
    "Estimate COVID-year exclusion robustness checks",
    "Estimate 2014–2017 delta-endpoint robustness checks",
    "Estimate leave-one-origin-out robustness checks",
    "Document final regression structure and output objects",
    "Create Main Table 1",
    "Create Main Table 2",
    "Create Main Figure 1: linear event study",
    "Create Appendix Table A1",
    "Create Appendix Table A2",
    "Create Appendix Figure A1: PPML event study",
    "Archived CEPII / gravity-control robustness documentation"
  ),
  script_type = c(
    rep("data construction", 11),
    rep("regression", 11),
    "documentation",
    "table construction",
    "table construction",
    "figure construction",
    "table construction",
    "table construction",
    "figure construction",
    "archived robustness"
  ),
  active_in_final_paper = c(
    rep(TRUE, 29),
    FALSE
  )
)

final_script_order


# ============================================================
# 2. Paper structure
# ============================================================

paper_empirical_structure <- tibble(
  paper_section = c(
    "Main paper",
    "Main paper",
    "Main paper",
    "Main paper",
    "Main paper",
    "Main paper",
    "Appendix",
    "Appendix",
    "Appendix",
    "Not emphasized / archived",
    "Not emphasized / archived",
    "Not implemented"
  ),
  paper_order = c(
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    NA_integer_,
    NA_integer_,
    NA_integer_
  ),
  item = c(
    "Main Table 1: Main results",
    "Main Table 2: Pre-trend test and additional main specifications",
    "Main Figure 1: Linear event study",
    "Results discussion: informative null",
    "Identification discussion: first stage and BHJ pre-trend test",
    "Robustness discussion: consistency across specifications",
    "Appendix Table A1: Consolidated robustness package",
    "Appendix Table A2: Leave-one-origin-out detail",
    "Appendix Figure A1: PPML event study",
    "CEPII / gravity-control robustness",
    "Königstein-key construction robustness",
    "Main IV-PPML via fixest IV syntax"
  ),
  content = c(
    "First stage, PPML reduced form, PPML benchmark, linear IV / 2SLS, control-function IV-style PPML",
    "BHJ pre-trend test, delta PPML reduced form, regional-control PPML reduced form",
    "Linear event-study diagnostic with 2014 as reference year and 2015–2016 shock band",
    "No evidence of a positive export response in this setting and horizon",
    "Strong first stage; no evidence of differential pre-shock export growth in BHJ test",
    "Null pattern is robust across estimator, sample, exposure definition, outcome, and origin exclusions",
    "Drop COVID, drop Eritrea, export weight, delta endpoint 2014–2017, leave-one-origin-out range",
    "Detailed leave-one-origin-out coefficients, standard errors, first-stage F-statistics, and observations",
    "PPML event-study diagnostic moved to appendix because 2014 reference year produces visually less clean pattern",
    "Archived because gravity controls were absorbed / collinear under attempted fixed-effect structure",
    "Documented but not emphasized because key variants are highly correlated and some variants are collinear with fixed effects",
    "Not implemented; replaced by linear IV / 2SLS and control-function IV-style PPML"
  ),
  final_status = c(
    "active",
    "active",
    "active",
    "active",
    "active",
    "active",
    "active",
    "active",
    "active appendix figure",
    "archived only",
    "documented only",
    "not implemented"
  )
)

paper_empirical_structure


# ============================================================
# 3. Main panels
# ============================================================

analysis_panels_overview <- tibble(
  panel_object = c(
    "analysis_panel.rds",
    "analysis_panel_no_eritrea.rds",
    "analysis_panel_no_covid.rds",
    "analysis_panel_no_eritrea_no_covid.rds",
    "analysis_panel_controls.rds",
    "analysis_panel_controls_no_eritrea.rds",
    "pretrend_bhj_panel_2010_2014.rds",
    "analysis_panel_delta_endpoint.rds",
    "analysis_panel_no_eritrea_delta_endpoint.rds",
    "analysis_panel_cepii.rds",
    "analysis_panel_cepii_no_eritrea.rds"
  ),
  period = c(
    "2010–2025",
    "2010–2025",
    "2010–2025 excluding 2020 and 2021",
    "2010–2025 excluding 2020 and 2021",
    "2010–2024",
    "2010–2024",
    "2010–2014 first-difference cross section",
    "2010–2025",
    "2010–2025",
    "2010–2021",
    "2010–2021"
  ),
  unit = c(
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year"
  ),
  observations = c(
    1247,
    1020,
    1095,
    892,
    1169,
    956,
    77,
    1247,
    1020,
    940,
    768
  ),
  use = c(
    "Main first stage, PPML reduced form, benchmark, linear IV, CF-IV-PPML, event study",
    "No-Eritrea robustness and leave-one-origin-out component",
    "COVID-year exclusion robustness",
    "No-Eritrea plus COVID-year exclusion robustness",
    "Regional-control robustness",
    "Regional-control robustness excluding Eritrea",
    "BHJ-style pre-trend test",
    "2014–2017 delta-endpoint robustness",
    "2014–2017 delta-endpoint robustness excluding Eritrea",
    "Archived CEPII / gravity-control robustness",
    "Archived CEPII / gravity-control robustness excluding Eritrea"
  ),
  final_status = c(
    rep("active", 9),
    "archived only",
    "archived only"
  )
)

analysis_panels_overview


# ============================================================
# 4. Main variables
# ============================================================

main_variables_overview <- tibble(
  variable_group = c(
    "Outcome",
    "Outcome",
    "Outcome",
    "Outcome",
    "Treatment",
    "Treatment",
    "Treatment",
    "Treatment",
    "Instrument",
    "Instrument",
    "Instrument",
    "Instrument",
    "Fixed effect",
    "Fixed effect",
    "Fixed effect",
    "Regional controls"
  ),
  variable_name = c(
    "export_value",
    "export_weight",
    "log_export_value",
    "delta_log_export_value",
    "treatment_stock_2016_post_1000",
    "treatment_delta_post_1000",
    "treatment_delta_2014_2017_post_1000",
    "future_treatment_stock_2016_1000",
    "iv_stock_2016_post_1000",
    "iv_delta_post_1000",
    "iv_delta_2014_2017_post_1000",
    "future_iv_stock_2016_1000",
    "fe_state_origin",
    "fe_state_year",
    "fe_origin_year",
    "log_gdp_million_eur; log_population; unemployment_rate; log_employment_thousand_persons; manufacturing_share; log_total_exports_world"
  ),
  definition = c(
    "Export value from German Länder to selected origin countries, in thousand EUR",
    "Export weight from German Länder to selected origin countries",
    "log(export_value + 1)",
    "Pre-shock change in log exports, mainly log exports in 2014 minus log exports in 2010",
    "Actual 2016 protection-seeker stock interacted with post period, scaled per 1,000 persons",
    "Actual 2014–2016 change in protection-seeker exposure interacted with post period, scaled per 1,000 persons",
    "Actual 2014–2017 change in protection-seeker exposure interacted with post period, scaled per 1,000 persons",
    "Pair-level future actual exposure intensity for pre-trend and event-study diagnostics",
    "Königstein-predicted 2016 protection-seeker stock interacted with post period, scaled per 1,000 persons",
    "Königstein-predicted 2014–2016 change interacted with post period, scaled per 1,000 persons",
    "Königstein-predicted 2014–2017 change interacted with post period, scaled per 1,000 persons",
    "Pair-level future predicted exposure intensity for pre-trend and event-study diagnostics",
    "federal_state × origin_country fixed effects",
    "federal_state × year fixed effects",
    "origin_country × year fixed effects",
    "Explicit regional controls used when fe_state_year is replaced"
  ),
  main_use = c(
    "Preferred PPML outcome",
    "Alternative outcome robustness",
    "Linear reduced form, linear IV / 2SLS, linear event study",
    "BHJ-style pre-trend test",
    "Main actual treatment",
    "Main delta-version actual treatment",
    "Appendix delta-endpoint actual treatment",
    "Descriptive treatment-based pre-trend and event-study diagnostics",
    "Main instrument",
    "Main delta-version instrument",
    "Appendix delta-endpoint instrument",
    "BHJ pre-trend test and event-study diagnostics",
    "Main pair fixed effect",
    "Main state-year fixed effect",
    "Main origin-year fixed effect",
    "Regional-control robustness"
  )
)

main_variables_overview


# ============================================================
# 5. Fixed-effect and estimator overview
# ============================================================

estimator_fe_overview <- tibble(
  specification = c(
    "First stage",
    "PPML reduced form",
    "PPML benchmark",
    "Linear reduced form",
    "Linear IV / 2SLS",
    "Control-function IV-style PPML",
    "BHJ pre-trend test",
    "Linear event study",
    "PPML event study",
    "Regional-control robustness",
    "COVID exclusion robustness",
    "Delta-endpoint robustness",
    "Leave-one-origin-out robustness"
  ),
  estimator = c(
    "feols",
    "fepois",
    "fepois",
    "feols",
    "feols IV syntax",
    "feols first stage + fepois with first-stage residual",
    "feols",
    "feols",
    "fepois",
    "fepois",
    "fepois",
    "feols and fepois",
    "feols first stage and fepois reduced form"
  ),
  outcome = c(
    "treatment exposure",
    "export_value",
    "export_value",
    "log_export_value",
    "log_export_value",
    "export_value",
    "delta_log_export_value",
    "log_export_value",
    "export_value",
    "export_value",
    "export_value",
    "export_value and log_export_value",
    "export_value"
  ),
  fixed_effects = c(
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "federal_state + origin_country",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_origin_year plus explicit regional controls",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_state_year + fe_origin_year",
    "fe_state_origin + fe_state_year + fe_origin_year"
  ),
  cluster = c(
    "fe_state_origin",
    "fe_state_origin",
    "fe_state_origin",
    "fe_state_origin",
    "fe_state_origin",
    "fe_state_origin",
    "federal_state",
    "fe_state_origin",
    "fe_state_origin",
    "fe_state_origin",
    "fe_state_origin",
    "fe_state_origin",
    "fe_state_origin"
  ),
  interpretation_note = c(
    "Tests relevance of the Königstein-based instrument",
    "Preferred trade-flow reduced form",
    "Non-causal conditional benchmark using actual treatment",
    "Linear robustness check",
    "Familiar linear IV / 2SLS robustness check",
    "Nonlinear IV-style robustness check; not standard IV-PPML",
    "Formal pre-trend validity diagnostic",
    "Main visual event-study diagnostic after supervisor feedback",
    "Appendix robustness diagnostic because 2014 reference year is visually less clean",
    "Checks whether saturated state-year fixed effects mechanically drive the null",
    "Checks whether COVID-period trade disruptions drive the null",
    "Checks alternative 2014–2017 endpoint for exposure change",
    "Checks that no single origin country drives the null"
  )
)

estimator_fe_overview


# ============================================================
# 6. Final empirical results overview
# ============================================================

final_results_overview <- tibble(
  result_block = c(
    "First stage",
    "PPML reduced form",
    "PPML benchmark",
    "Linear IV / 2SLS",
    "Control-function IV-style PPML",
    "BHJ pre-trend test",
    "Linear event study",
    "PPML event study",
    "Delta PPML reduced form",
    "Regional controls",
    "COVID exclusion",
    "Drop Eritrea",
    "Export weight",
    "Delta endpoint 2014–2017",
    "Leave-one-origin-out"
  ),
  key_result = c(
    "0.998*** (0.206), F = 23.4",
    "-0.0001 (0.0044)",
    "0.0001 (0.0032)",
    "0.0064 (0.0072)",
    "-0.0003 (0.0047), residual 0.0009 (0.0103)",
    "0.0026 (0.0175), p = 0.885",
    "Year-specific coefficients statistically insignificant",
    "No positive post-shock response; several coefficients negative; moved to appendix",
    "-0.0001 (0.0049)",
    "-0.0109 (0.0069)",
    "-0.0015 (0.0043)",
    "-0.0001 (0.0045)",
    "0.0055 (0.0153)",
    "-0.0001 (0.0076)",
    "Range [-0.0020, 0.0018], none significant"
  ),
  table_or_figure = c(
    "Main Table 1, column 1",
    "Main Table 1, column 2",
    "Main Table 1, column 3",
    "Main Table 1, column 4",
    "Main Table 1, column 5",
    "Main Table 2, column 1",
    "Main Figure 1",
    "Appendix Figure A1",
    "Main Table 2, column 2",
    "Main Table 2, column 3",
    "Appendix Table A1, column 1",
    "Appendix Table A1, column 2",
    "Appendix Table A1, column 3",
    "Appendix Table A1, column 4",
    "Appendix Table A1 column 5 and Appendix Table A2"
  ),
  interpretation = c(
    "Instrument is strongly relevant",
    "No PPML reduced-form evidence of positive export response",
    "No conditional benchmark association",
    "No linear IV / 2SLS evidence of positive export response",
    "No control-function IV-style PPML evidence of positive export response",
    "No evidence of differential pre-shock export growth",
    "Supports null as main visual diagnostic",
    "Consistent with null but visually less clean because 2014 is a single high reference year",
    "Null is robust to measuring exposure as 2014–2016 change",
    "Null is not mechanically driven by saturated state-year fixed effects",
    "Null is not driven by 2020–2021 trade disruptions",
    "Null is not driven by Eritrea",
    "Null is not driven by using export value rather than export weight",
    "Null is not driven by using 2016 rather than 2017 as endpoint",
    "Null is not driven by any single origin country"
  )
)

final_results_overview


# ============================================================
# 7. Final table and figure files
# ============================================================

final_table_figure_files <- tibble(
  output_type = c(
    "Main table",
    "Main table",
    "Main figure",
    "Appendix table",
    "Appendix table",
    "Appendix figure"
  ),
  output_name = c(
    "Main Table 1",
    "Main Table 2",
    "Main Figure 1",
    "Appendix Table A1",
    "Appendix Table A2",
    "Appendix Figure A1"
  ),
  description = c(
    "Main results",
    "Pre-trend test and additional main specifications",
    "Linear event-study estimates",
    "Consolidated robustness package",
    "Leave-one-origin-out detail",
    "PPML event-study estimates"
  ),
  main_file = c(
    "main_table_1_results.tex",
    "main_table_2_diagnostics_robustness.tex",
    "event_study_main_linear.pdf",
    "appendix_table_a1_robustness.tex",
    "appendix_table_a2_leave_one_origin_out.tex",
    "appendix_figure_a1_ppml_event_study.pdf"
  ),
  additional_files = c(
    "main_table_1_results.rds; main_table_1_results.csv",
    "main_table_2_diagnostics_robustness.rds; main_table_2_diagnostics_robustness.csv",
    "event_study_main_linear.png; event_study_main.pdf; event_study_main.png",
    "appendix_table_a1_robustness.rds; appendix_table_a1_robustness.csv",
    "appendix_table_a2_leave_one_origin_out.rds; appendix_table_a2_leave_one_origin_out.csv",
    "appendix_figure_a1_ppml_event_study.png; appendix_figure_a1_ppml_event_study_data.rds"
  ),
  created_by_script = c(
    "23_table_main_results.R",
    "24_table_main_diagnostics_and_robustness.R",
    "25_figure_event_study_main.R",
    "26_table_appendix_robustness.R",
    "27_table_leave_one_origin_out.R",
    "28_figure_appendix_event_study_main.R"
  )
)

final_table_figure_files


# ============================================================
# 8. Regression output object overview
# ============================================================

regression_output_overview <- tibble(
  output_group = c(
    "First stage",
    "First stage",
    "First stage",
    "First stage",
    "PPML reduced form",
    "PPML reduced form",
    "PPML reduced form",
    "PPML reduced form",
    "PPML benchmark",
    "PPML benchmark",
    "PPML benchmark",
    "Linear IV / 2SLS",
    "Linear IV / 2SLS",
    "Linear IV / 2SLS",
    "Control-function IV-style PPML",
    "Control-function IV-style PPML",
    "Control-function IV-style PPML",
    "BHJ pre-trend",
    "BHJ pre-trend",
    "Event study",
    "Event study",
    "Event study",
    "Regional controls",
    "Regional controls",
    "COVID exclusion",
    "COVID exclusion",
    "Delta endpoint",
    "Delta endpoint",
    "Leave-one-origin-out",
    "Leave-one-origin-out",
    "Archived CEPII",
    "Archived CEPII",
    "Final output",
    "Final output",
    "Final output",
    "Final output",
    "Final output",
    "Final output"
  ),
  object_or_file = c(
    "first_stage_stock_1000.rds",
    "first_stage_delta_1000.rds",
    "first_stage_stock_no_eritrea_1000.rds",
    "first_stage_results_paper.rds",
    "ppml_reduced_form_stock_1000.rds",
    "ppml_reduced_form_delta_1000.rds",
    "ppml_reduced_form_stock_no_eritrea_1000.rds",
    "ppml_reduced_form_results_paper.rds",
    "ppml_benchmark_stock_1000.rds",
    "ppml_benchmark_delta_1000.rds",
    "ppml_benchmark_results_paper.rds",
    "linear_reduced_form_stock_1000.rds",
    "linear_iv_stock_1000.rds",
    "linear_iv_results_paper.rds",
    "iv_ppml_stock_1000.rds",
    "iv_ppml_delta_1000.rds",
    "iv_ppml_results_paper.rds",
    "pretrend_bhj_stock_2010_2014.rds",
    "pretrend_bhj_results_paper.rds",
    "linear_event_study_iv_stock_1000.rds",
    "ppml_event_study_iv_stock_1000.rds",
    "event_study_results_paper.rds",
    "robustness_controls_ppml_reduced_form_stock_1000.rds",
    "robustness_controls_results_paper.rds",
    "robustness_covid_ppml_reduced_form_stock_1000.rds",
    "robustness_covid_results_paper.rds",
    "robustness_delta_endpoint_ppml_reduced_form_1000.rds",
    "robustness_delta_endpoint_results_paper.rds",
    "leave_one_origin_out_results_paper.rds",
    "leave_one_origin_out_main_interpretation_summary.rds",
    "analysis_panel_cepii.rds",
    "cepii_archive_note.rds",
    "main_table_1_results.tex",
    "main_table_2_diagnostics_robustness.tex",
    "event_study_main_linear.pdf",
    "appendix_table_a1_robustness.tex",
    "appendix_table_a2_leave_one_origin_out.tex",
    "appendix_figure_a1_ppml_event_study.pdf"
  ),
  content = c(
    "Main first-stage model object",
    "Delta first-stage model object",
    "No-Eritrea first-stage model object",
    "Paper-ready first-stage results",
    "Main PPML reduced-form model object",
    "Delta PPML reduced-form model object",
    "No-Eritrea PPML reduced-form model object",
    "Paper-ready PPML reduced-form results",
    "Main PPML benchmark model object",
    "Delta PPML benchmark model object",
    "Paper-ready PPML benchmark results",
    "Main linear reduced-form model object",
    "Main linear IV / 2SLS model object",
    "Paper-ready linear robustness results",
    "Main control-function IV-style PPML model object",
    "Delta control-function IV-style PPML model object",
    "Paper-ready control-function IV-style PPML results",
    "Main BHJ pre-trend model object",
    "Paper-ready BHJ pre-trend results",
    "Linear event-study model object used for Main Figure 1",
    "PPML event-study model object used for Appendix Figure A1",
    "Paper-ready event-study coefficient results",
    "Main regional-control PPML reduced-form model object",
    "Paper-ready regional-control robustness results",
    "Main COVID-exclusion PPML reduced-form model object",
    "Paper-ready COVID-exclusion results",
    "2014–2017 delta-endpoint PPML reduced-form model object",
    "Paper-ready delta-endpoint robustness results",
    "Paper-ready leave-one-origin-out results",
    "Compact leave-one-origin-out interpretation summary",
    "Archived CEPII panel",
    "Archived note explaining why CEPII robustness is not retained",
    "Main Table 1 LaTeX output",
    "Main Table 2 LaTeX output",
    "Main Figure 1 PDF output",
    "Appendix Table A1 LaTeX output",
    "Appendix Table A2 LaTeX output",
    "Appendix Figure A1 PDF output"
  ),
  final_status = c(
    rep("created", 30),
    "archived only",
    "archived only",
    rep("created", 6)
  )
)

regression_output_overview


# ============================================================
# 9. Archived / not emphasized / not implemented checks
# ============================================================

archived_or_not_emphasized_checks <- tibble(
  check = c(
    "Main IV-PPML via fixest IV syntax",
    "CEPII / gravity-control robustness",
    "Königstein-key construction robustness",
    "PPML event study as main figure",
    "2014–2016 average Königstein-key variant"
  ),
  status = c(
    "not implemented",
    "archived",
    "documented but not emphasized",
    "moved to appendix",
    "not emphasized / partly collinear"
  ),
  reason = c(
    "fixest IV syntax is used for linear IV / 2SLS with feols; preferred trade-flow estimator is fepois",
    "Attempted gravity controls were absorbed by fixed effects or dropped due to collinearity",
    "2014, 2015, and 2016 Königstein keys are highly correlated under the fixed-effect structure",
    "Supervisor feedback: 2014 is visually a high reference year, making PPML event study less clean as main visual",
    "Some k141516 variables are collinear with the preferred three-way fixed-effect structure"
  ),
  paper_treatment = c(
    "Do not claim to estimate standard IV-PPML; report linear IV and control-function IV-style PPML instead",
    "Mention only briefly if needed; do not present as active robustness result",
    "Mention only as design/documentation check if needed",
    "Use linear event study as Main Figure 1; keep PPML event study as Appendix Figure A1",
    "Document for transparency; do not use as central robustness result"
  )
)

archived_or_not_emphasized_checks


# ============================================================
# 10. Final interpretation object
# ============================================================

final_interpretation_note <- tibble(
  theme = c(
    "Main empirical conclusion",
    "What the results support",
    "What the results do not prove",
    "Preferred wording",
    "Avoided wording",
    "Identification statement",
    "Event-study statement",
    "Robustness statement"
  ),
  note = c(
    "The estimates provide no evidence of a positive export response to refugee-induced regional exposure in this setting and over this horizon.",
    "The result is an informative null supported by strong first stage, null PPML reduced form, null benchmark, null linear IV, null CF-IV-PPML, null pre-trend test, and robustness checks.",
    "The results do not prove that migrant networks never affect trade.",
    "The data provide no evidence of a positive export response in this setting and over this horizon.",
    "Migration has no effect on trade.",
    "The Königstein instrument is strongly relevant; the BHJ pre-trend test provides no evidence of differential pre-shock export growth, but exclusion and exogeneity cannot be proven mechanically.",
    "The main visual event-study diagnostic is the linear event study; the PPML event study is reported in the appendix and interpreted cautiously.",
    "The null is robust to excluding COVID years, excluding Eritrea, using export weight, using the 2014–2017 delta endpoint, and excluding each origin country one at a time."
  )
)

final_interpretation_note


# ============================================================
# Save documentation objects
# ============================================================

saveRDS(
  final_script_order,
  "final_script_order.rds"
)

saveRDS(
  paper_empirical_structure,
  "paper_empirical_structure.rds"
)

saveRDS(
  analysis_panels_overview,
  "analysis_panels_overview.rds"
)

saveRDS(
  main_variables_overview,
  "main_variables_overview.rds"
)

saveRDS(
  estimator_fe_overview,
  "estimator_fe_overview.rds"
)

saveRDS(
  final_results_overview,
  "final_results_overview.rds"
)

saveRDS(
  final_table_figure_files,
  "final_table_figure_files.rds"
)

saveRDS(
  regression_output_overview,
  "regression_output_overview.rds"
)

saveRDS(
  archived_or_not_emphasized_checks,
  "archived_or_not_emphasized_checks.rds"
)

saveRDS(
  final_interpretation_note,
  "final_interpretation_note.rds"
)


# ============================================================
# Optional: save compact combined documentation object
# ============================================================

regression_structure_documentation <- list(
  final_script_order = final_script_order,
  paper_empirical_structure = paper_empirical_structure,
  analysis_panels_overview = analysis_panels_overview,
  main_variables_overview = main_variables_overview,
  estimator_fe_overview = estimator_fe_overview,
  final_results_overview = final_results_overview,
  final_table_figure_files = final_table_figure_files,
  regression_output_overview = regression_output_overview,
  archived_or_not_emphasized_checks = archived_or_not_emphasized_checks,
  final_interpretation_note = final_interpretation_note
)

saveRDS(
  regression_structure_documentation,
  "regression_structure_documentation.rds"
)


# ============================================================
# Check final table and figure files
# ============================================================

final_output_file_check <- tibble(
  file = c(
    "main_table_1_results.tex",
    "main_table_2_diagnostics_robustness.tex",
    "event_study_main_linear.pdf",
    "event_study_main.pdf",
    "appendix_table_a1_robustness.tex",
    "appendix_table_a2_leave_one_origin_out.tex",
    "appendix_figure_a1_ppml_event_study.pdf"
  ),
  exists = file.exists(file)
)

final_output_file_check

saveRDS(
  final_output_file_check,
  "final_output_file_check.rds"
)


# ============================================================
# Final objects kept
# ============================================================
#
# Documentation objects:
#   final_script_order
#   paper_empirical_structure
#   analysis_panels_overview
#   main_variables_overview
#   estimator_fe_overview
#   final_results_overview
#   final_table_figure_files
#   regression_output_overview
#   archived_or_not_emphasized_checks
#   final_interpretation_note
#   regression_structure_documentation
#   final_output_file_check
#
# Saved files:
#   final_script_order.rds
#   paper_empirical_structure.rds
#   analysis_panels_overview.rds
#   main_variables_overview.rds
#   estimator_fe_overview.rds
#   final_results_overview.rds
#   final_table_figure_files.rds
#   regression_output_overview.rds
#   archived_or_not_emphasized_checks.rds
#   final_interpretation_note.rds
#   regression_structure_documentation.rds
#   final_output_file_check.rds
#
# ============================================================