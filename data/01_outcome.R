# ============================================================
# Outcome data: German subnational exports by origin country
# ============================================================
#
# Data source:
#   GENESIS / Destatis foreign trade data
#   Table 51000-0032:
#   Aus- und Einfuhr (Außenhandel): Bundesländer, Jahre, Länder
#   (English: Exports and imports (foreign trade): federal states, years, countries)
#
# Unit of observation:
#   federal_state × origin_country × year
#
# Script type:
#   Data-cleaning / construction script
#
# Countries included:
#   Afghanistan
#   Eritrea
#   Irak (Iraq)
#   Iran, Islamische Republik (Iran, Islamic Republic)
#   Syrien (Syria)
#
# Period:
#   2010–2025
#
# Period structure:
#   2010–2014 = pre-period
#   2015–2016 = refugee shock period
#   2017–2025 = post-period
#
# Main outcome:
#   export_value
#   = Export value in thousand EUR.
#
# Additional outcomes:
#   log_export_value = log(export_value + 1)
#   export_weight
#
# Workflow logic:
#   This is a data-cleaning / construction script.
#
#   Therefore, the outcome data are constructed from the raw GENESIS export
#   CSV and then saved as .rds.
#
#   Later analysis-panel, robustness, and regression scripts should load the
#   saved .rds outcome object directly instead of rebuilding it from the raw
#   CSV.
#
# Notes:
#   The main specification uses the full 2010–2025 period.
#
#   Robustness checks may restrict the sample, for example by excluding
#   Covid years or using alternative outcome variables.
#
#   Missing export observations relative to the balanced
#   federal_state × origin_country × year grid are documented separately.
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
library(stringr)
library(tidyr)
library(tibble)


# ============================================================
# Required input file and reference objects
# ============================================================
#
# Purpose:
#   Define the raw export input file and the reference lists needed to build
#   the cleaned export outcome dataset.
#
# Required raw input file:
#   51000-0032_de.csv
#
# Reference objects:
#   federal_states
#   origin_countries
#
# Notes:
#   This script is self-contained. It defines the state and origin-country
#   lists internally and does not rely on objects already present in the R
#   environment.
# ============================================================

raw_exports_file <- "51000-0032_de.csv"

if (!file.exists(raw_exports_file)) {
  stop(
    paste(
      "The required raw export input file is missing:",
      raw_exports_file,
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


### Define origin countries

origin_countries <- c(
  "Afghanistan",
  "Eritrea",
  "Irak",
  "Iran, Islamische Republik",
  "Syrien"
)


# ============================================================
# Helper function: clean German-formatted numbers
# ============================================================
#
# Purpose:
#   Convert German-formatted numeric strings into numeric values.
#
# Examples:
#   "1.234,5" -> 1234.5
#   "-"       -> NA
#   ""        -> NA
#
# Notes:
#   GENESIS CSV files often use German number formatting with dots as
#   thousand separators and commas as decimal separators.
# ============================================================

clean_number_de <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    na_if("-") %>%
    na_if("–") %>%
    na_if(".") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", ".") %>%
    as.numeric()
}


# ============================================================
# Load raw export data
# ============================================================
#
# Purpose:
#   Load raw GENESIS / Destatis export data.
#
# Input file:
#   51000-0032_de.csv
#
# Expected structure:
#   The raw file is a semi-structured GENESIS table. Years and Länder
#   appear as header-like rows and are filled downward to identify the
#   federal_state × origin_country × year observations.
#
# Notes:
#   The file is read as character data to avoid premature type conversion.
# ============================================================

raw_exports <- read.csv2(
  raw_exports_file,
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character"
)


# ============================================================
# Construct cleaned export outcome dataset
# ============================================================
#
# Purpose:
#   Clean the raw foreign-trade table and construct the final export outcome
#   dataset.
#
# Constructed object:
#   export_value_thousand_eur
#
# Unit of observation:
#   federal_state × origin_country × year
#
# Main steps:
#   1. Keep relevant columns.
#   2. Identify and fill year rows downward.
#   3. Identify and fill federal-state rows downward.
#   4. Keep only actual country rows.
#   5. Harmonise origin-country names.
#   6. Restrict to the five origin countries.
#   7. Clean numeric export variables.
#   8. Construct log outcome and period indicators.
#   9. Enforce one observation per federal_state × origin_country × year.
# ============================================================

export_value_thousand_eur <- raw_exports %>%
  
  # Keep only relevant raw columns.
  transmute(
    name = str_trim(V1),
    export_weight = str_trim(V2),
    export_value = str_trim(V4)
  ) %>%
  
  # Identify year rows and fill year downward.
  mutate(
    year = ifelse(
      str_detect(name, "^20[0-9]{2}$"),
      name,
      NA_character_
    )
  ) %>%
  fill(
    year,
    .direction = "down"
  ) %>%
  
  # Identify federal-state rows and fill Land downward.
  mutate(
    federal_state = ifelse(
      name %in% federal_states,
      name,
      NA_character_
    )
  ) %>%
  fill(
    federal_state,
    .direction = "down"
  ) %>%
  
  # Keep only actual country rows.
  filter(
    !is.na(year),
    !is.na(federal_state),
    !(name %in% federal_states),
    !str_detect(name, "^20[0-9]{2}$"),
    !str_detect(name, "Tabelle"),
    !str_detect(name, "Aus- und Einfuhr"),
    !str_detect(name, "Länder"),
    !str_detect(name, "Außenhandel"),
    name != "",
    export_value != "",
    export_weight != ""
  ) %>%
  
  # Harmonise country names.
  mutate(
    origin_country = case_when(
      name == "Eritrea (ab 1994)" ~ "Eritrea",
      name == "Islamische Republik Iran" ~ "Iran, Islamische Republik",
      name == "Arabische Republik Syrien" ~ "Syrien",
      TRUE ~ name
    )
  ) %>%
  
  # Keep only selected origin countries.
  filter(
    origin_country %in% origin_countries
  ) %>%
  
  # Clean numeric variables.
  mutate(
    year = as.integer(year),
    export_weight = clean_number_de(export_weight),
    export_value = clean_number_de(export_value)
  ) %>%
  
  # Remove missing or non-reported export values.
  filter(
    !is.na(export_value)
  ) %>%
  
  # Create panel identifiers and period variables.
  mutate(
    pair_id = paste(
      federal_state,
      origin_country,
      sep = "_"
    ),
    
    log_export_value = log(export_value + 1),
    
    pre_period = ifelse(year <= 2014, 1, 0),
    shock_period = ifelse(year %in% c(2015, 2016), 1, 0),
    post_period = ifelse(year >= 2017, 1, 0)
  ) %>%
  
  # Ensure one observation per federal_state × origin_country × year.
  distinct(
    federal_state,
    origin_country,
    year,
    .keep_all = TRUE
  ) %>%
  
  # Keep final variable order.
  select(
    federal_state,
    origin_country,
    year,
    pair_id,
    export_value,
    log_export_value,
    export_weight,
    pre_period,
    shock_period,
    post_period
  ) %>%
  
  arrange(
    origin_country,
    federal_state,
    year
  )


# ============================================================
# Outcome data checks
# ============================================================
#
# Purpose:
#   Inspect the cleaned export outcome dataset and verify sample coverage.
# ============================================================

### Structure

str(export_value_thousand_eur)

summary(export_value_thousand_eur)

head(
  export_value_thousand_eur,
  10
)


### Sample size and coverage

outcome_panel_summary <- export_value_thousand_eur %>%
  summarise(
    n_obs = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    missing_export_value = sum(is.na(export_value)),
    missing_log_export_value = sum(is.na(log_export_value)),
    missing_export_weight = sum(is.na(export_weight))
  )

outcome_panel_summary


### Check observations by origin country

outcome_panel_by_origin <- export_value_thousand_eur %>%
  count(
    origin_country
  )

outcome_panel_by_origin


### Check observations by Land

outcome_panel_by_state <- export_value_thousand_eur %>%
  count(
    federal_state
  )

outcome_panel_by_state


### Check observations by year

outcome_panel_by_year <- export_value_thousand_eur %>%
  count(
    year
  )

outcome_panel_by_year


### Check duplicates

duplicate_outcome_panel_rows <- export_value_thousand_eur %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )

duplicate_outcome_panel_rows


# ============================================================
# Missing export observations
# ============================================================
#
# Purpose:
#   Document which observations are missing relative to a balanced
#   federal_state × origin_country × year panel.
#
# Balanced reference grid:
#   16 German Länder × 5 origin countries × 16 years
#   = 1,280 theoretical observations.
#
# Notes:
#   Missing observations are not filled here. They are documented so that the
#   final analysis panel can be interpreted correctly.
# ============================================================

full_outcome_grid <- expand.grid(
  federal_state = federal_states,
  origin_country = origin_countries,
  year = 2010:2025,
  stringsAsFactors = FALSE
) %>%
  as_tibble()

missing_outcome_observations <- full_outcome_grid %>%
  anti_join(
    export_value_thousand_eur %>%
      select(
        federal_state,
        origin_country,
        year
      ),
    by = c(
      "federal_state",
      "origin_country",
      "year"
    )
  ) %>%
  arrange(
    origin_country,
    federal_state,
    year
  )

missing_outcome_observations


missing_outcome_by_origin <- missing_outcome_observations %>%
  count(
    origin_country
  )

missing_outcome_by_origin


missing_outcome_by_state <- missing_outcome_observations %>%
  count(
    federal_state
  )

missing_outcome_by_state


missing_outcome_by_year <- missing_outcome_observations %>%
  count(
    year
  )

missing_outcome_by_year


### Expected vs actual sample size

outcome_sample_size_comparison <- tibble(
  panel = "export_value_thousand_eur",
  actual_n = nrow(export_value_thousand_eur),
  theoretical_n =
    length(federal_states) *
    length(origin_countries) *
    length(2010:2025),
  missing_from_balanced_panel = theoretical_n - actual_n
)

outcome_sample_size_comparison


# ============================================================
# Final consistency check
# ============================================================
#
# Purpose:
#   Collect key export-outcome diagnostics in one compact object.
#
# Interpretation:
#   This object quickly shows whether the cleaned export data have the
#   expected country coverage, state coverage, year range, and duplicate
#   structure.
# ============================================================

outcome_panel_consistency_check <- tibble(
  check = c(
    "contains_expected_number_of_states",
    "contains_expected_number_of_origins",
    "starts_in_2010",
    "ends_in_2025",
    "has_no_duplicate_state_origin_year_rows",
    "has_no_missing_export_value",
    "has_no_missing_log_export_value"
  ),
  value = c(
    n_distinct(export_value_thousand_eur$federal_state) ==
      length(federal_states),
    n_distinct(export_value_thousand_eur$origin_country) ==
      length(origin_countries),
    min(export_value_thousand_eur$year, na.rm = TRUE) == 2010,
    max(export_value_thousand_eur$year, na.rm = TRUE) == 2025,
    nrow(duplicate_outcome_panel_rows) == 0,
    sum(is.na(export_value_thousand_eur$export_value)) == 0,
    sum(is.na(export_value_thousand_eur$log_export_value)) == 0
  )
)

outcome_panel_consistency_check


# ============================================================
# Save cleaned outcome data
# ============================================================
#
# Purpose:
#   Save the cleaned export outcome dataset and diagnostic objects.
#
# Main saved outcome object:
#   export_value_thousand_eur.rds
#
# Notes:
#   Later data-construction and regression scripts should load
#   export_value_thousand_eur.rds directly instead of rebuilding the outcome
#   data from the raw GENESIS CSV.
# ============================================================

saveRDS(
  export_value_thousand_eur,
  "export_value_thousand_eur.rds"
)

saveRDS(
  missing_outcome_observations,
  "missing_outcome_observations.rds"
)

saveRDS(
  outcome_panel_summary,
  "outcome_panel_summary.rds"
)

saveRDS(
  outcome_panel_by_origin,
  "outcome_panel_by_origin.rds"
)

saveRDS(
  outcome_panel_by_state,
  "outcome_panel_by_state.rds"
)

saveRDS(
  outcome_panel_by_year,
  "outcome_panel_by_year.rds"
)

saveRDS(
  duplicate_outcome_panel_rows,
  "duplicate_outcome_panel_rows.rds"
)

saveRDS(
  missing_outcome_by_origin,
  "missing_outcome_by_origin.rds"
)

saveRDS(
  missing_outcome_by_state,
  "missing_outcome_by_state.rds"
)

saveRDS(
  missing_outcome_by_year,
  "missing_outcome_by_year.rds"
)

saveRDS(
  outcome_sample_size_comparison,
  "outcome_sample_size_comparison.rds"
)

saveRDS(
  outcome_panel_consistency_check,
  "outcome_panel_consistency_check.rds"
)


# ============================================================
# Clean temporary objects
# ============================================================

rm(
  raw_exports_file,
  raw_exports,
  full_outcome_grid,
  federal_states,
  origin_countries,
  clean_number_de
)


# ============================================================
# Final objects kept
# ============================================================
#
# Cleaned outcome data:
#   export_value_thousand_eur
#
# Summary and diagnostic objects:
#   outcome_panel_summary
#   outcome_panel_by_origin
#   outcome_panel_by_state
#   outcome_panel_by_year
#   duplicate_outcome_panel_rows
#   missing_outcome_observations
#   missing_outcome_by_origin
#   missing_outcome_by_state
#   missing_outcome_by_year
#   outcome_sample_size_comparison
#   outcome_panel_consistency_check
#
# Notes:
#   export_value_thousand_eur is the cleaned export outcome dataset.
#
#   Unit of observation:
#     federal_state × origin_country × year
#
#   Main outcome:
#     export_value
#     = export value in thousand EUR
#
#   Additional outcomes:
#     log_export_value = log(export_value + 1)
#     export_weight
#
#   Period:
#     2010–2025
#
#   Period indicators:
#     pre_period   = 1 for years up to and including 2014
#     shock_period = 1 for 2015 and 2016
#     post_period  = 1 for years from 2017 onward
#
#   This script is a data-cleaning / construction script. It builds the
#   cleaned export outcome dataset from the raw GENESIS CSV and saves it as
#   .rds for later analysis-panel construction and regression scripts.
#
#   Later scripts should load export_value_thousand_eur.rds directly instead
#   of reconstructing the outcome data from raw exports.
# ============================================================