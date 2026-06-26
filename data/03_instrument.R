# ============================================================
# Instrument data: Königsteiner Schlüssel
# ============================================================
#
# Data source:
#   Königsteiner Schlüssel
#   Gemeinsame Wissenschaftskonferenz (GWK)
#
# Unit of observation:
#   federal_state
#
# Purpose:
#   Clean and check the Königstein allocation key used as the basis for the
#   predicted migration-exposure instrument.
#
# Script type:
#   Data-cleaning / construction script
#
# Main instrument basis:
#   koenigstein_share_2015_2016_avg
#
# Robustness instrument bases:
#   koenigstein_share_2014
#   koenigstein_share_2014_2015_2016_avg
#
# Workflow logic:
#   This is a data-cleaning script. Therefore, the required Königstein
#   objects are built from the raw CSV file and then saved as .rds files.
#
#   Later regression and robustness scripts should load the saved .rds
#   objects directly instead of rebuilding them.
#
# Notes:
#   The Königstein allocation key is used as the allocation-share component
#   of the instrument.
#
#   The main specification uses the average Königstein allocation share in
#   2015 and 2016. This matches the allocation rules relevant for the main
#   2015/16 refugee-shock cohort.
#
#   The 2014 key is retained as a stricter pre-shock robustness basis.
#
#   The 2014–2016 average is constructed as an additional robustness and
#   transparency basis.
#
#   The actual construction of predicted exposure and post-period IV
#   interactions is done later in the analysis-panel construction script.
# ============================================================


# ============================================================
# Setup
# ============================================================

### Path

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")


### Packages

library(dplyr)
library(stringr)
library(tibble)


# ============================================================
# Required input file and reference object
# ============================================================
#
# Purpose:
#   Define the raw input file and the expected federal-state reference list.
#
# Required input file:
#   koenigsteiner_schluessel_2014_2016.csv
#
# Required reference object:
#   federal_states
#
# Notes:
#   This script is self-contained. It loads the raw Königstein CSV from disk
#   and defines the expected list of German Länder internally.
#
#   Because this is a data-cleaning script, the cleaned objects are built
#   from the raw CSV rather than loaded from previously saved .rds files.
# ============================================================

raw_koenigstein_file <- "koenigsteiner_schluessel_2014_2016.csv"

if (!file.exists(raw_koenigstein_file)) {
  stop(
    paste(
      "The required raw Königstein input file is missing:",
      raw_koenigstein_file,
      "Please make sure the CSV is stored in the working directory."
    )
  )
}


### Define German Länder

federal_states <- c(
  "Baden-Württemberg",
  "Bayern",  # Bavaria
  "Berlin",
  "Brandenburg",
  "Bremen",
  "Hamburg",
  "Hessen",  # Hesse
  "Mecklenburg-Vorpommern",  # Mecklenburg-Western Pomerania
  "Niedersachsen",  # Lower Saxony
  "Nordrhein-Westfalen",  # North Rhine-Westphalia
  "Rheinland-Pfalz",  # Rhineland-Palatinate
  "Saarland",
  "Sachsen",  # Saxony
  "Sachsen-Anhalt",  # Saxony-Anhalt
  "Schleswig-Holstein",
  "Thüringen"  # Thuringia
)


# ============================================================
# Load raw Königstein allocation key
# ============================================================
#
# Purpose:
#   Load the raw Königstein allocation-key file from CSV.
#
# Input file:
#   koenigsteiner_schluessel_2014_2016.csv
#
# Expected unit of observation:
#   federal_state
#
# Expected core variables:
#   federal_state
#   koenigstein_share_2014
#   koenigstein_share_2015
#   koenigstein_share_2016
#
# Notes:
#   This is the raw input used to construct the cleaned instrument-basis
#   dataset koenigstein_key.
# ============================================================

koenigstein_key_raw <- read.csv(
  raw_koenigstein_file,
  stringsAsFactors = FALSE
)


# ============================================================
# Required-variable check for raw Königstein data
# ============================================================
#
# Purpose:
#   Check whether the raw Königstein file contains the variables required to
#   construct the main and robustness allocation-share measures.
#
# Required variables:
#   federal_state
#   koenigstein_share_2014
#   koenigstein_share_2015
#   koenigstein_share_2016
#
# Interpretation:
#   Missing variables indicate that the raw CSV must be corrected before the
#   instrument-basis dataset can be constructed.
# ============================================================

required_koenigstein_variables <- c(
  "federal_state",
  "koenigstein_share_2014",
  "koenigstein_share_2015",
  "koenigstein_share_2016"
)

missing_koenigstein_raw_variables <- tibble(
  variable = required_koenigstein_variables,
  present = required_koenigstein_variables %in% names(koenigstein_key_raw)
) %>%
  filter(
    !present
  )

missing_koenigstein_raw_variables

if (nrow(missing_koenigstein_raw_variables) > 0) {
  stop(
    "At least one required raw Königstein variable is missing. Inspect missing_koenigstein_raw_variables."
  )
}


# ============================================================
# Clean Königstein key
# ============================================================
#
# Purpose:
#   Clean federal-state names, convert allocation-share variables to numeric
#   format, and construct the main and robustness allocation-share measures.
#
# Constructed variables:
#   koenigstein_share_2015_2016_avg
#   koenigstein_share_2014_2015_2016_avg
#
# Main IV basis:
#   koenigstein_share_2015_2016_avg
#
# Robustness IV bases:
#   koenigstein_share_2014
#   koenigstein_share_2014_2015_2016_avg
#
# Logic:
#   The main allocation basis is the average of the 2015 and 2016
#   Königstein shares:
#
#     koenigstein_share_2015_2016_avg =
#       (koenigstein_share_2015 + koenigstein_share_2016) / 2
#
#   The additional robustness basis is the average of the 2014, 2015 and
#   2016 Königstein shares:
#
#     koenigstein_share_2014_2015_2016_avg =
#       (koenigstein_share_2014 + koenigstein_share_2015
#        + koenigstein_share_2016) / 3
#
# Notes:
#   The average variables are constructed directly in this script rather
#   than relying on pre-existing average columns in the raw CSV. This avoids
#   hidden dependence on the exact raw-file structure.
# ============================================================

koenigstein_key <- koenigstein_key_raw %>%
  mutate(
    federal_state = str_squish(federal_state),
    
    across(
      starts_with("koenigstein_share_"),
      as.numeric
    ),
    
    koenigstein_share_2015_2016_avg =
      (
        koenigstein_share_2015 +
          koenigstein_share_2016
      ) / 2,
    
    koenigstein_share_2014_2015_2016_avg =
      (
        koenigstein_share_2014 +
          koenigstein_share_2015 +
          koenigstein_share_2016
      ) / 3
  ) %>%
  arrange(
    federal_state
  )


# ============================================================
# Königstein key structure checks
# ============================================================
#
# Purpose:
#   Inspect the cleaned Königstein allocation-key dataset.
#
# Checks:
#   Object structure, summary statistics, and first rows.
#
# Notes:
#   These checks are printed for manual inspection and documentation.
# ============================================================

str(koenigstein_key)

summary(koenigstein_key)

head(
  koenigstein_key,
  10
)


# ============================================================
# Königstein key summary and missing-value checks
# ============================================================
#
# Purpose:
#   Summarise the cleaned Königstein key and document missing values.
#
# Checks:
#   Number of observations, number of Länder, and missing values in
#   each allocation-share variable.
#
# Interpretation:
#   The cleaned dataset should contain exactly one observation per German
#   Land and no missing allocation shares.
# ============================================================

koenigstein_key_summary <- koenigstein_key %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    missing_state = sum(is.na(federal_state)),
    missing_share_2014 = sum(is.na(koenigstein_share_2014)),
    missing_share_2015 = sum(is.na(koenigstein_share_2015)),
    missing_share_2016 = sum(is.na(koenigstein_share_2016)),
    missing_share_2015_2016_avg =
      sum(is.na(koenigstein_share_2015_2016_avg)),
    missing_share_2014_2015_2016_avg =
      sum(is.na(koenigstein_share_2014_2015_2016_avg))
  )

koenigstein_key_summary


# ============================================================
# Federal-state coverage checks
# ============================================================
#
# Purpose:
#   Check whether the Königstein data cover exactly the expected German
#   Länder.
#
# Checks:
#   missing_koenigstein_states
#   unexpected_koenigstein_states
#
# Interpretation:
#   missing_koenigstein_states should be empty.
#   unexpected_koenigstein_states should be empty.
# ============================================================

missing_koenigstein_states <- tibble(
  federal_state = federal_states
) %>%
  anti_join(
    koenigstein_key %>%
      select(
        federal_state
      ),
    by = "federal_state"
  )

missing_koenigstein_states


unexpected_koenigstein_states <- koenigstein_key %>%
  filter(
    !(federal_state %in% federal_states)
  ) %>%
  select(
    federal_state
  )

unexpected_koenigstein_states


# ============================================================
# Allocation-share sum checks
# ============================================================
#
# Purpose:
#   Check whether the Königstein allocation shares sum to one.
#
# Variables checked:
#   koenigstein_share_2014
#   koenigstein_share_2015
#   koenigstein_share_2016
#   koenigstein_share_2015_2016_avg
#   koenigstein_share_2014_2015_2016_avg
#
# Interpretation:
#   Each sum should be approximately one. Small deviations can arise from
#   rounding in the published allocation shares.
# ============================================================

koenigstein_share_sums <- koenigstein_key %>%
  summarise(
    sum_share_2014 =
      sum(koenigstein_share_2014, na.rm = TRUE),
    sum_share_2015 =
      sum(koenigstein_share_2015, na.rm = TRUE),
    sum_share_2016 =
      sum(koenigstein_share_2016, na.rm = TRUE),
    sum_share_2015_2016_avg =
      sum(koenigstein_share_2015_2016_avg, na.rm = TRUE),
    sum_share_2014_2015_2016_avg =
      sum(koenigstein_share_2014_2015_2016_avg, na.rm = TRUE)
  )

koenigstein_share_sums


# ============================================================
# Check 2015–2016 average construction
# ============================================================
#
# Purpose:
#   Verify that the main 2015–2016 average allocation share is constructed
#   correctly.
#
# Logic:
#   The script recalculates the 2015–2016 average and compares it with the
#   stored variable.
#
# Interpretation:
#   max_abs_avg_difference should be zero, apart from possible floating-point
#   precision differences.
# ============================================================

koenigstein_avg_2015_2016_check <- koenigstein_key %>%
  mutate(
    koenigstein_share_2015_2016_avg_check =
      (
        koenigstein_share_2015 +
          koenigstein_share_2016
      ) / 2,
    avg_difference =
      koenigstein_share_2015_2016_avg -
      koenigstein_share_2015_2016_avg_check
  ) %>%
  summarise(
    max_abs_avg_difference =
      max(abs(avg_difference), na.rm = TRUE)
  )

koenigstein_avg_2015_2016_check


# ============================================================
# Check 2014–2016 average construction
# ============================================================
#
# Purpose:
#   Verify that the 2014–2016 average allocation share is constructed
#   correctly.
#
# Logic:
#   The script recalculates the 2014–2016 average and compares it with the
#   stored variable.
#
# Interpretation:
#   max_abs_avg_difference should be zero, apart from possible floating-point
#   precision differences.
# ============================================================

koenigstein_avg_2014_2015_2016_check <- koenigstein_key %>%
  mutate(
    koenigstein_share_2014_2015_2016_avg_check =
      (
        koenigstein_share_2014 +
          koenigstein_share_2015 +
          koenigstein_share_2016
      ) / 3,
    avg_difference =
      koenigstein_share_2014_2015_2016_avg -
      koenigstein_share_2014_2015_2016_avg_check
  ) %>%
  summarise(
    max_abs_avg_difference =
      max(abs(avg_difference), na.rm = TRUE)
  )

koenigstein_avg_2014_2015_2016_check


# ============================================================
# Duplicate checks
# ============================================================
#
# Purpose:
#   Check whether any Land appears more than once in the cleaned
#   Königstein key.
#
# Interpretation:
#   duplicate_koenigstein_states should be empty. Duplicates would create
#   many-to-many merges in the analysis-panel construction script.
# ============================================================

duplicate_koenigstein_states <- koenigstein_key %>%
  count(
    federal_state
  ) %>%
  filter(
    n > 1
  )

duplicate_koenigstein_states


# ============================================================
# Final consistency check
# ============================================================
#
# Purpose:
#   Collect high-level consistency indicators in one diagnostic table.
#
# Checks:
#   Expected number of Länder, missing state names, unexpected state
#   names, duplicate state rows, and approximate share sums.
#
# Interpretation:
#   This object provides a compact overview of whether the cleaned
#   Königstein instrument-basis data are ready for the analysis-panel
#   construction script.
# ============================================================

koenigstein_consistency_check <- tibble(
  check = c(
    "expected_number_of_states",
    "missing_federal_states",
    "unexpected_federal_states",
    "duplicate_federal_states",
    "share_sum_2014_close_to_one",
    "share_sum_2015_close_to_one",
    "share_sum_2016_close_to_one",
    "share_sum_2015_2016_avg_close_to_one",
    "share_sum_2014_2015_2016_avg_close_to_one"
  ),
  value = c(
    nrow(koenigstein_key) == length(federal_states),
    nrow(missing_koenigstein_states) == 0,
    nrow(unexpected_koenigstein_states) == 0,
    nrow(duplicate_koenigstein_states) == 0,
    abs(koenigstein_share_sums$sum_share_2014 - 1) < 0.001,
    abs(koenigstein_share_sums$sum_share_2015 - 1) < 0.001,
    abs(koenigstein_share_sums$sum_share_2016 - 1) < 0.001,
    abs(koenigstein_share_sums$sum_share_2015_2016_avg - 1) < 0.001,
    abs(koenigstein_share_sums$sum_share_2014_2015_2016_avg - 1) < 0.001
  )
)

koenigstein_consistency_check


# ============================================================
# Save cleaned instrument data
# ============================================================
#
# Purpose:
#   Save the cleaned Königstein instrument-basis dataset and all diagnostic
#   objects.
#
# Main saved dataset:
#   koenigstein_key.rds
#
# Notes:
#   koenigstein_key.rds is loaded later in the main analysis-panel
#   construction script to construct predicted exposure and post-period IV
#   interactions.
#
#   Regression and robustness scripts should load this .rds file directly
#   rather than rebuilding the Königstein key from the raw CSV.
# ============================================================

saveRDS(
  koenigstein_key,
  "koenigstein_key.rds"
)

saveRDS(
  koenigstein_key_summary,
  "koenigstein_key_summary.rds"
)

saveRDS(
  koenigstein_share_sums,
  "koenigstein_share_sums.rds"
)

saveRDS(
  koenigstein_avg_2015_2016_check,
  "koenigstein_avg_2015_2016_check.rds"
)

saveRDS(
  koenigstein_avg_2014_2015_2016_check,
  "koenigstein_avg_2014_2015_2016_check.rds"
)

saveRDS(
  missing_koenigstein_states,
  "missing_koenigstein_states.rds"
)

saveRDS(
  unexpected_koenigstein_states,
  "unexpected_koenigstein_states.rds"
)

saveRDS(
  duplicate_koenigstein_states,
  "duplicate_koenigstein_states.rds"
)

saveRDS(
  missing_koenigstein_raw_variables,
  "missing_koenigstein_raw_variables.rds"
)

saveRDS(
  koenigstein_consistency_check,
  "koenigstein_consistency_check.rds"
)


# ============================================================
# Clean temporary objects
# ============================================================

rm(
  raw_koenigstein_file,
  required_koenigstein_variables,
  federal_states,
  koenigstein_key_raw
)


# ============================================================
# Final objects kept
# ============================================================
#
# Cleaned instrument data:
#   koenigstein_key
#
# Summary and diagnostic objects:
#   koenigstein_key_summary
#   koenigstein_share_sums
#   koenigstein_avg_2015_2016_check
#   koenigstein_avg_2014_2015_2016_check
#   missing_koenigstein_states
#   unexpected_koenigstein_states
#   duplicate_koenigstein_states
#   missing_koenigstein_raw_variables
#   koenigstein_consistency_check
#
# Notes:
#   koenigstein_key is the final instrument-basis dataset used in the main
#   analysis-panel construction script.
#
#   Unit of observation:
#     federal_state
#
#   Main instrument basis:
#     koenigstein_share_2015_2016_avg
#
#   Active robustness instrument basis:
#     koenigstein_share_2014
#
#   Additional instrument basis constructed for transparency:
#     koenigstein_share_2014_2015_2016_avg
#
#   The actual predicted exposure variables and post-period IV interactions
#   are constructed later in the main analysis-panel script.
#
#   This script is a data-cleaning / construction script. It builds the
#   cleaned Königstein instrument-basis dataset from the raw CSV and saves it
#   as .rds for use in later analysis and regression scripts.
# ============================================================