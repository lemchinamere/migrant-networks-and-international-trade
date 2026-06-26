# ============================================================
# Main Figure 1: Linear event-study figure
# ============================================================
#
# Purpose:
#   Create the main event-study figure for the term paper.
#
# Main change:
#   The main-text event-study figure is based on the linear event-study
#   specification, not the PPML event study.
#
# Figure structure:
#   - x-axis: year, 2010 to 2025
#   - y-axis: linear event-study coefficient
#   - 95% confidence intervals
#   - vertical reference line at 2014
#   - shaded shock period for 2015–2016
#
# Script type:
#   Figure-construction script.
#
# Important:
#   This script does not estimate regressions.
#   It loads the already estimated linear event-study model from .rds.
#
# Input:
#   linear_event_study_iv_stock_1000.rds
#
# Output:
#   event_study_main_linear_data.rds
#   event_study_main_linear_data.csv
#   event_study_main_linear_plot.rds
#   event_study_main_linear.pdf
#   event_study_main_linear.png
#
# Also saved for LaTeX convenience:
#   event_study_main.pdf
#   event_study_main.png
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
library(ggplot2)
library(broom)
library(fixest)


# ============================================================
# Required input file
# ============================================================

required_input_files <- c(
  "linear_event_study_iv_stock_1000.rds"
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
      "Please rerun script 17_event_study.R before creating Main Figure 1."
    )
  )
}


# ============================================================
# Load linear event-study model
# ============================================================

linear_event_study_iv_stock_1000 <- readRDS(
  "linear_event_study_iv_stock_1000.rds"
)

linear_event_study_iv_stock_1000


# ============================================================
# Extract coefficients
# ============================================================

linear_event_study_tidy <- broom::tidy(
  linear_event_study_iv_stock_1000,
  conf.int = FALSE
)

linear_event_study_tidy


# ============================================================
# Identify event-study coefficients
# ============================================================
#
# The exact term names from fixest::i() can vary.
# Therefore, the extraction keeps terms that:
#   - contain future_iv_stock_2016_1000
#   - contain a year between 2010 and 2025
#
# ============================================================

event_study_coefficients_raw <- linear_event_study_tidy %>%
  filter(
    str_detect(term, "future_iv_stock_2016_1000")
  ) %>%
  mutate(
    year = as.integer(
      str_extract(term, "20[0-2][0-9]")
    )
  ) %>%
  filter(
    !is.na(year),
    year >= 2010,
    year <= 2025
  ) %>%
  transmute(
    year = year,
    coefficient = estimate,
    std_error = std.error,
    statistic = statistic,
    p_value = p.value
  ) %>%
  arrange(year)

event_study_coefficients_raw

if (nrow(event_study_coefficients_raw) == 0) {
  stop(
    paste(
      "No linear event-study coefficients could be extracted.",
      "Please inspect linear_event_study_tidy$term."
    )
  )
}


# ============================================================
# Add reference year
# ============================================================
#
# Reference year:
#   2014
#
# Since 2014 is omitted in the event-study regression, it is added manually
# with coefficient zero and no confidence interval.
#
# ============================================================

reference_year <- 2014L

event_study_main_linear_data <- bind_rows(
  event_study_coefficients_raw,
  tibble(
    year = reference_year,
    coefficient = 0,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_
  )
) %>%
  mutate(
    confidence_low = coefficient - 1.96 * std_error,
    confidence_high = coefficient + 1.96 * std_error,
    reference_year = year == reference_year,
    shock_period = year %in% c(2015L, 2016L)
  ) %>%
  arrange(year)

event_study_main_linear_data


# ============================================================
# Save standardized coefficient data
# ============================================================

saveRDS(
  event_study_main_linear_data,
  "event_study_main_linear_data.rds"
)

write_csv(
  event_study_main_linear_data,
  "event_study_main_linear_data.csv"
)


# ============================================================
# Create linear event-study plot
# ============================================================

event_study_main_linear_plot <- ggplot(
  event_study_main_linear_data,
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
    data = event_study_main_linear_data %>%
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
    y = "Linear event-study coefficient"
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

event_study_main_linear_plot


# ============================================================
# Save figure outputs
# ============================================================

saveRDS(
  event_study_main_linear_plot,
  "event_study_main_linear_plot.rds"
)

ggsave(
  filename = "event_study_main_linear.pdf",
  plot = event_study_main_linear_plot,
  width = 7,
  height = 4.5,
  units = "in"
)

ggsave(
  filename = "event_study_main_linear.png",
  plot = event_study_main_linear_plot,
  width = 7,
  height = 4.5,
  units = "in",
  dpi = 300
)


# ------------------------------------------------------------
# Save also under generic main-figure names for LaTeX convenience
# ------------------------------------------------------------

ggsave(
  filename = "event_study_main.pdf",
  plot = event_study_main_linear_plot,
  width = 7,
  height = 4.5,
  units = "in"
)

ggsave(
  filename = "event_study_main.png",
  plot = event_study_main_linear_plot,
  width = 7,
  height = 4.5,
  units = "in",
  dpi = 300
)


# ============================================================
# Suggested caption
# ============================================================
#
# Figure 1: Linear event-study estimates. The figure plots year-specific
# coefficients on the interaction between year indicators and the 2016 predicted
# exposure measure, with 2014 as the reference year. The shaded band indicates
# the 2015–2016 refugee shock period. The estimates provide no evidence of a
# differential post-shock export response. The pre-period coefficients serve as
# a visual pre-trend diagnostic; the formal BHJ pre-trend test is reported in
# Main Table 2, column 1, and is statistically indistinguishable from zero.
#
# ============================================================


# ============================================================
# Keep only final objects in environment
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  linear_event_study_tidy,
  event_study_coefficients_raw,
  reference_year
)


# ============================================================
# Final objects kept
# ============================================================
#
# Loaded object:
#   linear_event_study_iv_stock_1000
#
# Figure objects:
#   event_study_main_linear_data
#   event_study_main_linear_plot
#
# Saved files:
#   event_study_main_linear_data.rds
#   event_study_main_linear_data.csv
#   event_study_main_linear_plot.rds
#   event_study_main_linear.pdf
#   event_study_main_linear.png
#   event_study_main.pdf
#   event_study_main.png
#
# Interpretation:
#   The linear event-study figure is the main visual diagnostic.
#   It supports the null result by showing no clear differential post-shock
#   export response relative to pre-shock coefficients.
#
# ============================================================