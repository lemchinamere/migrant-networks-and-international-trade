# ============================================================
# Create no-Eritrea panel objects
# ============================================================
#
# Purpose:
#   Create no-Eritrea versions of already constructed analysis panels.
#
# Script type:
#   Data-construction helper script.
#
# Important:
#   This script does not estimate regressions.
#   It only loads existing .rds panel objects, filters out Eritrea,
#   and saves no-Eritrea versions for later robustness scripts.
#
# ============================================================


# ============================================================
# Setup
# ============================================================

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")

library(dplyr)
library(stringr)


# ============================================================
# Helper functions
# ============================================================

detect_origin_column <- function(data) {
  possible_origin_columns <- c(
    "origin_country",
    "origin",
    "country_origin",
    "origin_country_name"
  )
  
  detected_column <- possible_origin_columns[
    possible_origin_columns %in% names(data)
  ][1]
  
  if (is.na(detected_column)) {
    stop(
      paste(
        "Could not detect origin-country column. Available columns are:",
        paste(names(data), collapse = ", ")
      )
    )
  }
  
  detected_column
}


remove_eritrea <- function(data) {
  origin_column <- detect_origin_column(data)
  
  data %>%
    filter(
      !str_detect(
        str_to_lower(.data[[origin_column]]),
        "eritrea"
      )
    )
}


create_no_eritrea_file <- function(input_file, output_file) {
  if (!file.exists(input_file)) {
    message(
      paste(
        "Skipping because input file does not exist:",
        input_file
      )
    )
    return(invisible(NULL))
  }
  
  data <- readRDS(input_file)
  
  data_no_eritrea <- remove_eritrea(data)
  
  saveRDS(
    data_no_eritrea,
    output_file
  )
  
  message(
    paste(
      "Saved:",
      output_file
    )
  )
  
  invisible(data_no_eritrea)
}


# ============================================================
# Create no-Eritrea versions of main panels
# ============================================================

create_no_eritrea_file(
  input_file = "analysis_panel.rds",
  output_file = "analysis_panel_no_eritrea.rds"
)

create_no_eritrea_file(
  input_file = "analysis_panel_iv_ppml_stock.rds",
  output_file = "analysis_panel_iv_ppml_stock_no_eritrea.rds"
)

create_no_eritrea_file(
  input_file = "analysis_panel_iv_ppml_delta.rds",
  output_file = "analysis_panel_iv_ppml_delta_no_eritrea.rds"
)

create_no_eritrea_file(
  input_file = "analysis_panel_delta_endpoint.rds",
  output_file = "analysis_panel_no_eritrea_delta_endpoint.rds"
)

create_no_eritrea_file(
  input_file = "analysis_panel_controls.rds",
  output_file = "analysis_panel_controls_no_eritrea.rds"
)

create_no_eritrea_file(
  input_file = "analysis_panel_cepii.rds",
  output_file = "analysis_panel_cepii_no_eritrea.rds"
)

create_no_eritrea_file(
  input_file = "analysis_panel_no_covid.rds",
  output_file = "analysis_panel_no_covid_no_eritrea.rds"
)


# ============================================================
# Diagnostics
# ============================================================

no_eritrea_files_created <- list.files(
  pattern = "no_eritrea.*\\.rds$",
  ignore.case = TRUE
)

no_eritrea_files_created


# ============================================================
# Final note
# ============================================================
#
# This script ensures that later scripts can load no-Eritrea panels
# without depending on robustness regressions being run first.
#
# ============================================================