# ============================================================
# Appendix Figure A1: PPML event-study figure
# ============================================================
#
# Purpose:
#   Create the appendix PPML event-study figure for the term paper.
#
# Figure structure:
#   - x-axis: year, 2010 to 2025
#   - y-axis: PPML event-study coefficient
#   - 95% confidence intervals
#   - vertical reference line at 2014
#   - shaded shock period for 2015–2016
#
# Script type:
#   Figure-construction script.
#
# Important:
#   This script does not estimate regressions.
#   It loads already saved PPML event-study plot data from .rds files.
#
# Output:
#   appendix_figure_a1_ppml_event_study_data.rds
#   appendix_figure_a1_ppml_event_study_data.csv
#   appendix_figure_a1_ppml_event_study_plot.rds
#   appendix_figure_a1_ppml_event_study.pdf
#   appendix_figure_a1_ppml_event_study.png
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
library(readr)
library(ggplot2)


# ============================================================
# Required input file
# ============================================================
#
# This is the already saved PPML event-study plot data from script 17.
#
# ============================================================

required_input_files <- c(
  "event_study_plot_data_main.rds"
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

missing_input_files

if (length(missing_input_files) > 0) {
  stop(
    paste(
      "The following required input file is missing:",
      paste(missing_input_files, collapse = ", "),
      "Please rerun script 17_event_study.R before creating Appendix Figure A1."
    )
  )
}


# ============================================================
# Load PPML event-study plot data
# ============================================================

event_study_plot_data_main <- readRDS(
  "event_study_plot_data_main.rds"
)

event_study_plot_data_main


# ============================================================
# Standardize PPML event-study data
# ============================================================
#
# The loaded object should contain year-level PPML event-study coefficients.
# This block standardizes column names if needed.
#
# Required final columns:
#   year
#   coefficient
#   std_error
#   confidence_low
#   confidence_high
#   p_value
#
# ============================================================

appendix_figure_a1_ppml_event_study_data <- event_study_plot_data_main

names(appendix_figure_a1_ppml_event_study_data)


# ------------------------------------------------------------
# Rename common possible column names to standardized names
# ------------------------------------------------------------

if ("estimate" %in% names(appendix_figure_a1_ppml_event_study_data) &&
    !"coefficient" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    rename(
      coefficient = estimate
    )
}

if ("std.error" %in% names(appendix_figure_a1_ppml_event_study_data) &&
    !"std_error" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    rename(
      std_error = std.error
    )
}

if ("se" %in% names(appendix_figure_a1_ppml_event_study_data) &&
    !"std_error" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    rename(
      std_error = se
    )
}

if ("conf.low" %in% names(appendix_figure_a1_ppml_event_study_data) &&
    !"confidence_low" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    rename(
      confidence_low = conf.low
    )
}

if ("conf.high" %in% names(appendix_figure_a1_ppml_event_study_data) &&
    !"confidence_high" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    rename(
      confidence_high = conf.high
    )
}

if ("ci_low" %in% names(appendix_figure_a1_ppml_event_study_data) &&
    !"confidence_low" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    rename(
      confidence_low = ci_low
    )
}

if ("ci_high" %in% names(appendix_figure_a1_ppml_event_study_data) &&
    !"confidence_high" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    rename(
      confidence_high = ci_high
    )
}

if ("p.value" %in% names(appendix_figure_a1_ppml_event_study_data) &&
    !"p_value" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    rename(
      p_value = p.value
    )
}


# ============================================================
# Check required columns
# ============================================================

required_columns <- c(
  "year",
  "coefficient"
)

missing_required_columns <- required_columns[
  !required_columns %in% names(appendix_figure_a1_ppml_event_study_data)
]

missing_required_columns

if (length(missing_required_columns) > 0) {
  stop(
    paste(
      "The PPML event-study plot data does not contain the required columns:",
      paste(missing_required_columns, collapse = ", "),
      "Please inspect names(event_study_plot_data_main)."
    )
  )
}


# ============================================================
# Add confidence intervals if not already included
# ============================================================

if (!"confidence_low" %in% names(appendix_figure_a1_ppml_event_study_data) ||
    !"confidence_high" %in% names(appendix_figure_a1_ppml_event_study_data)) {
  
  if (!"std_error" %in% names(appendix_figure_a1_ppml_event_study_data)) {
    stop(
      paste(
        "The PPML event-study plot data does not contain confidence intervals",
        "or standard errors. Please inspect names(event_study_plot_data_main)."
      )
    )
  }
  
  appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
    mutate(
      confidence_low = coefficient - 1.96 * std_error,
      confidence_high = coefficient + 1.96 * std_error
    )
}


# ============================================================
# Add reference and shock-period indicators
# ============================================================

reference_year <- 2014L

appendix_figure_a1_ppml_event_study_data <- appendix_figure_a1_ppml_event_study_data %>%
  mutate(
    year = as.integer(year),
    reference_year = year == reference_year,
    shock_period = year %in% c(2015L, 2016L)
  ) %>%
  filter(
    year >= 2010,
    year <= 2025
  ) %>%
  arrange(year)

appendix_figure_a1_ppml_event_study_data


# ============================================================
# Save coefficient data
# ============================================================

saveRDS(
  appendix_figure_a1_ppml_event_study_data,
  "appendix_figure_a1_ppml_event_study_data.rds"
)

write_csv(
  appendix_figure_a1_ppml_event_study_data,
  "appendix_figure_a1_ppml_event_study_data.csv"
)


# ============================================================
# Create PPML appendix event-study plot
# ============================================================

appendix_figure_a1_ppml_event_study_plot <- ggplot(
  appendix_figure_a1_ppml_event_study_data,
  aes(
    x = year,
    y = coefficient
  )
) +
  annotate(
    "rect",
    xmin = 2015,
    xmax = 2016,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = reference_year,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_errorbar(
    data = appendix_figure_a1_ppml_event_study_data %>%
      filter(!reference_year),
    aes(
      ymin = confidence_low,
      ymax = confidence_high
    ),
    width = 0.15,
    linetype = "dotted",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.5
  ) +
  geom_point(
    size = 1.8
  ) +
  scale_x_continuous(
    breaks = 2010:2025,
    limits = c(2010, 2025)
  ) +
  labs(
    x = "Year",
    y = "PPML event-study coefficient"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_blank()
  )

appendix_figure_a1_ppml_event_study_plot


# ============================================================
# Save figure outputs
# ============================================================

saveRDS(
  appendix_figure_a1_ppml_event_study_plot,
  "appendix_figure_a1_ppml_event_study_plot.rds"
)

ggsave(
  filename = "appendix_figure_a1_ppml_event_study.pdf",
  plot = appendix_figure_a1_ppml_event_study_plot,
  width = 7,
  height = 4.5,
  units = "in"
)

ggsave(
  filename = "appendix_figure_a1_ppml_event_study.png",
  plot = appendix_figure_a1_ppml_event_study_plot,
  width = 7,
  height = 4.5,
  units = "in",
  dpi = 300
)


# ============================================================
# Suggested appendix caption
# ============================================================
#
# Appendix Figure A1: PPML event-study estimates. The figure plots
# year-specific PPML coefficients on future_iv_stock_2016_1000, with 2014 as
# the reference year. The shaded band indicates the 2015–2016 refugee shock
# period. Most coefficients lie below zero, including both pre- and post-shock
# years. This pattern likely reflects the choice of 2014 as a single high
# reference year for high-IV pairs rather than a differential post-shock effect.
# The figure is therefore interpreted as a descriptive robustness diagnostic
# rather than as the main validity test. The formal BHJ pre-trend test is
# reported in Main Table 2, column 1.
#
# ============================================================


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_columns,
  missing_required_columns,
  reference_year
)


# ============================================================
# Final objects kept
# ============================================================
#
# Loaded object:
#   event_study_plot_data_main
#
# Figure objects:
#   appendix_figure_a1_ppml_event_study_data
#   appendix_figure_a1_ppml_event_study_plot
#
# Saved files:
#   appendix_figure_a1_ppml_event_study_data.rds
#   appendix_figure_a1_ppml_event_study_data.csv
#   appendix_figure_a1_ppml_event_study_plot.rds
#   appendix_figure_a1_ppml_event_study.pdf
#   appendix_figure_a1_ppml_event_study.png
#
# Interpretation:
#   The PPML event-study figure is kept as an appendix robustness diagnostic.
#   Because 2014 appears to be a high reference year for high-IV pairs, the
#   PPML dynamic pattern should not be used as the main visual validity test.
#   The main text instead uses the linear event-study figure, while the formal
#   pre-trend check is the BHJ test in Main Table 2, column 1.
#
# ============================================================