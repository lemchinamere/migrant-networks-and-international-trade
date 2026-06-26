# ============================================================
# Treatment data: protection seekers by Land and origin country
# ============================================================
#
# Data source:
#   GENESIS / Destatis
#   Table 12531-0024:
#   Schutzsuchende: Bundesländer, Stichtag, Geschlecht,
#   Familienstand, Ländergruppierungen/Staatsangehörigkeit
#   (English: Protection seekers: federal states, reference date, sex, marital status, country groupings / nationality)
#
# Unit of observation after cleaning:
#   federal_state × origin_country
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
# Treatment variables:
#   protection_seekers_stock_2014
#   protection_seekers_stock_2016
#   delta_protection_seekers_2014_2016
#
# Main treatment:
#   protection_seekers_stock_2016
#
# Robustness treatment:
#   delta_protection_seekers_2014_2016
#
# Workflow logic:
#   This is a data-cleaning / construction script.
#
#   Therefore, the treatment dataset is constructed from the raw GENESIS CSV
#   and then saved as .rds.
#
#   Later analysis-panel, robustness, and regression scripts should load the
#   saved .rds treatment object directly instead of rebuilding it from the
#   raw CSV.
#
# Notes:
#   The treatment is measured as the stock of protection seekers by federal
#   state and origin country.
#
#   The 2016 stock captures post-shock exposure, while the 2014–2016 change
#   captures the increase during the refugee shock period.
#
#   Treatment variables are measured in persons. Regression variables scaled
#   by 1,000 persons are constructed later in the final panel or rescaling
#   script.
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
#   Define the raw treatment input file and the reference lists needed to
#   build the cleaned treatment dataset.
#
# Required raw input file:
#   12531-0024_de.csv
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

raw_treatment_file <- "12531-0024_de.csv"

if (!file.exists(raw_treatment_file)) {
  stop(
    paste(
      "The required raw treatment input file is missing:",
      raw_treatment_file,
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
#   "1.234" -> 1234
#   "-"     -> NA
#   ""      -> NA
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
# Load raw treatment data
# ============================================================
#
# Purpose:
#   Load raw GENESIS / Destatis protection-seeker data.
#
# Input file:
#   12531-0024_de.csv
#
# Expected structure:
#   The raw file is a semi-structured GENESIS table. Record dates and
#   Länder appear as header-like rows and are filled downward to
#   identify the federal_state × origin_country observations.
#
# Notes:
#   The file is read as character data to avoid premature type conversion.
# ============================================================

raw_treatment <- read.csv2(
  raw_treatment_file,
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character"
)


# ============================================================
# Diagnostic check: raw country rows
# ============================================================
#
# Purpose:
#   Inspect the raw structure of the country rows before aggregating numeric
#   cells.
#
# Reason:
#   This is important because summing across row entries is appropriate only
#   if the row contains subcategories that need to be aggregated, and not a
#   total column plus subcategories.
#
# Notes:
#   This object is saved as a diagnostic so that the raw aggregation logic can
#   be checked later.
# ============================================================

treatment_raw_country_rows <- raw_treatment %>%
  mutate(
    name = str_trim(V1)
  ) %>%
  filter(
    name %in% origin_countries
  ) %>%
  select(
    V1:V12
  )

treatment_raw_country_rows %>%
  head(20)


# ============================================================
# Construct long treatment dataset
# ============================================================
#
# Purpose:
#   Convert the semi-structured raw treatment table into a long dataset with
#   one row per federal_state × origin_country × record_date.
#
# Constructed object:
#   protection_seekers_long
#
# Main steps:
#   1. Identify record-date rows.
#   2. Identify federal-state rows.
#   3. Fill record date and Land downward.
#   4. Keep the five selected origin-country rows.
#   5. Sum numeric cells across the row to construct stock.
#
# Important:
#   The rowwise sum follows the logic in the original script. It assumes that
#   the relevant country row contains subcategory cells to be aggregated.
#   The raw country-row diagnostic above is kept to verify this assumption.
# ============================================================

protection_seekers_long <- raw_treatment %>%
  
  # Clean first column and identify record dates / Länder.
  mutate(
    name = str_trim(V1),
    
    record_date = ifelse(
      str_detect(name, "^\\d{2}\\.\\d{2}\\.\\d{4}$"),
      name,
      NA_character_
    ),
    
    federal_state = ifelse(
      name %in% federal_states,
      name,
      NA_character_
    )
  ) %>%
  
  # Fill record date and Land downward.
  fill(
    record_date,
    federal_state,
    .direction = "down"
  ) %>%
  
  # Keep only selected origin-country rows.
  filter(
    name %in% origin_countries,
    !is.na(record_date),
    !is.na(federal_state)
  ) %>%
  
  # Sum over all numeric cells in the row.
  # This aggregates over subcategories if the table does not provide a
  # single direct total column.
  rowwise() %>%
  mutate(
    stock = sum(
      clean_number_de(
        c_across(starts_with("V"))[-1]
      ),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  
  transmute(
    federal_state,
    record_date,
    origin_country = name,
    stock
  )


# ============================================================
# Convert treatment data from long to wide format
# ============================================================
#
# Purpose:
#   Construct the final treatment dataset with one row per
#   federal_state × origin_country.
#
# Constructed object:
#   protection_seekers_stock
#
# Constructed variables:
#   protection_seekers_stock_2014
#   protection_seekers_stock_2016
#   delta_protection_seekers_2014_2016
# ============================================================

protection_seekers_stock <- protection_seekers_long %>%
  mutate(
    year = as.integer(str_sub(record_date, 7, 10))
  ) %>%
  select(
    federal_state,
    origin_country,
    year,
    stock
  ) %>%
  filter(
    year %in% c(2014, 2016)
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = stock,
    names_prefix = "protection_seekers_stock_"
  ) %>%
  mutate(
    delta_protection_seekers_2014_2016 =
      protection_seekers_stock_2016 - protection_seekers_stock_2014
  ) %>%
  select(
    federal_state,
    origin_country,
    protection_seekers_stock_2014,
    protection_seekers_stock_2016,
    delta_protection_seekers_2014_2016
  ) %>%
  arrange(
    origin_country,
    federal_state
  )


# ============================================================
# Treatment data checks
# ============================================================
#
# Purpose:
#   Inspect the cleaned treatment dataset and verify sample coverage.
# ============================================================

### Structure

str(protection_seekers_stock)

summary(protection_seekers_stock)

head(
  protection_seekers_stock,
  10
)


### Sample size and coverage

treatment_panel_summary <- protection_seekers_stock %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    missing_stock_2014 = sum(is.na(protection_seekers_stock_2014)),
    missing_stock_2016 = sum(is.na(protection_seekers_stock_2016)),
    missing_delta = sum(is.na(delta_protection_seekers_2014_2016))
  )

treatment_panel_summary


### Check observations by origin country

treatment_by_origin <- protection_seekers_stock %>%
  count(
    origin_country
  )

treatment_by_origin


### Check observations by Land

treatment_by_state <- protection_seekers_stock %>%
  count(
    federal_state
  )

treatment_by_state


### Check duplicates

duplicate_treatment_pairs <- protection_seekers_stock %>%
  count(
    federal_state,
    origin_country
  ) %>%
  filter(
    n > 1
  )

duplicate_treatment_pairs


### Check expected full coverage

treatment_expected_grid <- expand.grid(
  federal_state = federal_states,
  origin_country = origin_countries,
  stringsAsFactors = FALSE
) %>%
  as_tibble()

missing_treatment_pairs <- treatment_expected_grid %>%
  anti_join(
    protection_seekers_stock %>%
      select(
        federal_state,
        origin_country
      ),
    by = c(
      "federal_state",
      "origin_country"
    )
  ) %>%
  arrange(
    origin_country,
    federal_state
  )

missing_treatment_pairs


### Check treatment variation

treatment_variation_summary <- protection_seekers_stock %>%
  summarise(
    min_stock_2014 = min(protection_seekers_stock_2014, na.rm = TRUE),
    max_stock_2014 = max(protection_seekers_stock_2014, na.rm = TRUE),
    min_stock_2016 = min(protection_seekers_stock_2016, na.rm = TRUE),
    max_stock_2016 = max(protection_seekers_stock_2016, na.rm = TRUE),
    min_delta = min(delta_protection_seekers_2014_2016, na.rm = TRUE),
    max_delta = max(delta_protection_seekers_2014_2016, na.rm = TRUE)
  )

treatment_variation_summary


### Check origin-level national totals

national_treatment_totals <- protection_seekers_stock %>%
  group_by(
    origin_country
  ) %>%
  summarise(
    national_protection_seekers_stock_2014 =
      sum(protection_seekers_stock_2014, na.rm = TRUE),
    
    national_protection_seekers_stock_2016 =
      sum(protection_seekers_stock_2016, na.rm = TRUE),
    
    national_delta_protection_seekers_2014_2016 =
      sum(delta_protection_seekers_2014_2016, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(
    origin_country
  )

national_treatment_totals


# ============================================================
# Final consistency check
# ============================================================
#
# Purpose:
#   Collect key treatment-data diagnostics in one compact object.
#
# Interpretation:
#   This object quickly shows whether the cleaned treatment dataset has the
#   expected state-origin coverage, missingness pattern, and duplicate
#   structure.
# ============================================================

treatment_consistency_check <- tibble(
  check = c(
    "contains_expected_number_of_states",
    "contains_expected_number_of_origins",
    "contains_expected_number_of_state_origin_pairs",
    "has_no_duplicate_state_origin_pairs",
    "has_no_missing_stock_2014",
    "has_no_missing_stock_2016",
    "has_no_missing_delta"
  ),
  value = c(
    n_distinct(protection_seekers_stock$federal_state) ==
      length(federal_states),
    
    n_distinct(protection_seekers_stock$origin_country) ==
      length(origin_countries),
    
    nrow(protection_seekers_stock) ==
      length(federal_states) * length(origin_countries),
    
    nrow(duplicate_treatment_pairs) == 0,
    
    sum(is.na(protection_seekers_stock$protection_seekers_stock_2014)) == 0,
    
    sum(is.na(protection_seekers_stock$protection_seekers_stock_2016)) == 0,
    
    sum(is.na(protection_seekers_stock$delta_protection_seekers_2014_2016)) == 0
  )
)

treatment_consistency_check


# ============================================================
# Save cleaned treatment data
# ============================================================
#
# Purpose:
#   Save the cleaned treatment dataset and diagnostic objects.
#
# Main saved treatment object:
#   protection_seekers_stock.rds
#
# Notes:
#   Later data-construction and regression scripts should load
#   protection_seekers_stock.rds directly instead of rebuilding treatment
#   data from the raw GENESIS CSV.
# ============================================================

saveRDS(
  protection_seekers_stock,
  "protection_seekers_stock.rds"
)

saveRDS(
  treatment_panel_summary,
  "treatment_panel_summary.rds"
)

saveRDS(
  treatment_variation_summary,
  "treatment_variation_summary.rds"
)

saveRDS(
  national_treatment_totals,
  "national_treatment_totals.rds"
)

saveRDS(
  missing_treatment_pairs,
  "missing_treatment_pairs.rds"
)

saveRDS(
  treatment_raw_country_rows,
  "treatment_raw_country_rows.rds"
)

saveRDS(
  protection_seekers_long,
  "protection_seekers_long.rds"
)

saveRDS(
  treatment_by_origin,
  "treatment_by_origin.rds"
)

saveRDS(
  treatment_by_state,
  "treatment_by_state.rds"
)

saveRDS(
  duplicate_treatment_pairs,
  "duplicate_treatment_pairs.rds"
)

saveRDS(
  treatment_consistency_check,
  "treatment_consistency_check.rds"
)


# ============================================================
# Clean temporary objects
# ============================================================

rm(
  raw_treatment_file,
  raw_treatment,
  treatment_expected_grid,
  federal_states,
  origin_countries,
  clean_number_de
)


# ============================================================
# Final objects kept
# ============================================================
#
# Cleaned treatment data:
#   protection_seekers_stock
#
# Intermediate cleaned treatment data:
#   protection_seekers_long
#
# Raw diagnostic object:
#   treatment_raw_country_rows
#
# Summary and diagnostic objects:
#   treatment_panel_summary
#   treatment_by_origin
#   treatment_by_state
#   duplicate_treatment_pairs
#   missing_treatment_pairs
#   treatment_variation_summary
#   national_treatment_totals
#   treatment_consistency_check
#
# Notes:
#   protection_seekers_stock is the final treatment dataset used in the main
#   analysis-panel construction script.
#
#   Unit of observation:
#     federal_state × origin_country
#
#   Main treatment variable:
#     protection_seekers_stock_2016
#
#   Robustness treatment variable:
#     delta_protection_seekers_2014_2016
#
#   Treatment variables are measured in persons.
#
#   Regression-ready post-period interactions and variables scaled by 1,000
#   persons are constructed later in the main analysis-panel or rescaling
#   script.
#
#   This is a data-cleaning / construction script. It builds the cleaned
#   treatment dataset from the raw GENESIS CSV and saves it as .rds for later
#   analysis-panel construction and regression scripts.
#
#   Later scripts should load protection_seekers_stock.rds directly instead
#   of reconstructing the treatment data from raw data.
# ============================================================