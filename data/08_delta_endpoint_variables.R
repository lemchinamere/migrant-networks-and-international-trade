# ============================================================
# Delta-endpoint data: protection seekers by Land and origin country
# 2014–2017 exposure change instead of 2014–2016
# ============================================================
#
# Data source:
#   GENESIS / Destatis
#   Table 12531-0024:
#   Schutzsuchende: Bundesländer, Stichtag, Geschlecht,
#   Familienstand, Ländergruppierungen/Staatsangehörigkeit
#   (English: Protection seekers: federal states, reference date, sex, marital status, country groupings / nationality)
#
# Required raw input file:
#   12531-0024_de_2014_2017.csv
#
# Required existing panels:
#   analysis_panel.rds
#   analysis_panel_no_eritrea.rds
#
# Unit of observation after cleaning:
#   federal_state × origin_country
#
# Unit of observation in final delta-endpoint panels:
#   federal_state × origin_country × year
#
# Script type:
#   Data-cleaning / construction script
#
# Pipeline position:
#   This script should be run after:
#     01_outcome.R
#     02_treatment.R
#     03_instrument.R
#     04_analysis.R
#     05_controls.R
#     06_rescaling.R
#     07_fixed_effects.R
#
#   This script should be run before:
#     09_data_structure.R
#     10_sources.R
#     20_delta_endpoints_robustness.R
#
# Countries included:
#   Afghanistan
#   Eritrea
#   Irak (Iraq)
#   Iran, Islamische Republik (Iran, Islamic Republic)
#   Syrien (Syria)
#
# Delta-endpoint variables:
#   protection_seekers_stock_2014
#   protection_seekers_stock_2017
#   delta_protection_seekers_2014_2017
#
# Main constructed treatment:
#   treatment_delta_2014_2017_post_1000
#
# Main constructed instrument:
#   iv_delta_2014_2017_post_1000
#
# Workflow logic:
#   This is a data-cleaning / construction script.
#
#   Therefore, the alternative 2014–2017 exposure dataset is constructed
#   from the raw GENESIS CSV and then merged into the existing analysis panels.
#
#   The resulting robustness panels are saved as .rds files.
#
#   Later regression scripts should load:
#     analysis_panel_delta_endpoint.rds
#     analysis_panel_no_eritrea_delta_endpoint.rds
#
#   instead of rebuilding the 2014–2017 exposure variables from the raw CSV.
#
# Notes:
#   This script is intentionally structured similarly to 02_treatment.R.
#
#   The main difference is that the raw 2014–2017 CSV stores the selected
#   origin countries in columns rather than as country rows.
#
#   Therefore, this script identifies the country value columns, reconstructs
#   record date and Land from header-like rows, keeps detailed
#   gender × family-status rows, reshapes the data to long format, and then
#   aggregates to federal_state × origin_country × year.
#
#   The 2014–2017 endpoint is used only as a robustness check. It should not
#   replace the main 2014–2016 exposure definition because 2017 is further
#   removed from the initial refugee allocation shock and may be more affected
#   by secondary mobility or endogenous location choices.
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
# Required input files and reference objects
# ============================================================
#
# Purpose:
#   Define the raw delta-endpoint input file, the required existing panel
#   files, and the reference lists needed to build the cleaned 2014–2017
#   exposure dataset.
#
# Required raw input file:
#   12531-0024_de_2014_2017.csv
#
# Required existing panel files:
#   analysis_panel.rds
#   analysis_panel_no_eritrea.rds
#
# Reference objects:
#   federal_states
#   origin_countries
#
# Notes:
#   This script defines the state and origin-country lists internally, just
#   like 02_treatment.R.
# ============================================================

raw_delta_endpoint_file <- "12531-0024_de_2014_2017.csv"

required_input_files <- c(
  "analysis_panel.rds",
  "analysis_panel_no_eritrea.rds",
  raw_delta_endpoint_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

missing_input_files

if (length(missing_input_files) > 0) {
  stop(
    paste(
      "The following required input files are missing:",
      paste(missing_input_files, collapse = ", "),
      "Please make sure all required files are stored in the working directory before running this script."
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
# Load required base panels
# ============================================================
#
# Purpose:
#   Load the already constructed analysis panels.
#
# Notes:
#   This script does not rebuild the main analysis panel.
#
#   It only constructs an additional 2014–2017 exposure measure and adds it
#   to the existing full-sample and no-Eritrea panels.
# ============================================================

analysis_panel <- readRDS(
  "analysis_panel.rds"
)

analysis_panel_no_eritrea <- readRDS(
  "analysis_panel_no_eritrea.rds"
)


# ============================================================
# Defensive fixed-effect reconstruction
# ============================================================
#
# Purpose:
#   Ensure that both base panels contain the fixed-effect identifiers needed
#   later in the delta-endpoints robustness regressions.
#
# Fixed effects:
#   fe_state_origin
#     = federal_state × origin_country
#
#   fe_state_year
#     = federal_state × year
#
#   fe_origin_year
#     = origin_country × year
#
# Logic:
#   If a fixed-effect variable already exists, it is kept unchanged.
#
#   If it is missing, it is reconstructed from the underlying panel
#   identifiers.
#
# Interpretation:
#   This block is defensive and keeps the script robust to whether fixed
#   effects were already saved by an earlier panel-construction script.
# ============================================================

add_fixed_effects_if_missing <- function(data) {
  data %>%
    mutate(
      fe_state_origin = if (
        "fe_state_origin" %in% names(.)
      ) {
        fe_state_origin
      } else {
        interaction(
          federal_state,
          origin_country,
          drop = TRUE
        )
      },
      
      fe_state_year = if (
        "fe_state_year" %in% names(.)
      ) {
        fe_state_year
      } else {
        interaction(
          federal_state,
          year,
          drop = TRUE
        )
      },
      
      fe_origin_year = if (
        "fe_origin_year" %in% names(.)
      ) {
        fe_origin_year
      } else {
        interaction(
          origin_country,
          year,
          drop = TRUE
        )
      }
    )
}

analysis_panel <- add_fixed_effects_if_missing(
  analysis_panel
)

analysis_panel_no_eritrea <- add_fixed_effects_if_missing(
  analysis_panel_no_eritrea
)


# ============================================================
# Required-variable check for base panels
# ============================================================
#
# Purpose:
#   Check whether the loaded base panels contain all variables needed to
#   construct the 2014–2017 delta-endpoint treatment and IV variables.
#
# Required variables:
#   federal_state
#   origin_country
#   year
#   post_period
#   export_value
#   log_export_value
#   koenigstein_share_2015_2016_avg
#   fe_state_origin
#   fe_state_year
#   fe_origin_year
#
# Interpretation:
#   Missing variables indicate that an earlier data-construction script must
#   be rerun before constructing the delta-endpoint robustness panels.
# ============================================================

required_delta_endpoint_base_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "post_period",
  "export_value",
  "log_export_value",
  "koenigstein_share_2015_2016_avg",
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)

missing_delta_endpoint_base_variables <- bind_rows(
  tibble(
    panel = "analysis_panel",
    variable = required_delta_endpoint_base_variables,
    present = required_delta_endpoint_base_variables %in%
      names(analysis_panel)
  ),
  
  tibble(
    panel = "analysis_panel_no_eritrea",
    variable = required_delta_endpoint_base_variables,
    present = required_delta_endpoint_base_variables %in%
      names(analysis_panel_no_eritrea)
  )
) %>%
  filter(
    !present
  )

missing_delta_endpoint_base_variables

if (nrow(missing_delta_endpoint_base_variables) > 0) {
  stop(
    "At least one required base-panel variable for delta-endpoint construction is missing. Inspect missing_delta_endpoint_base_variables."
  )
}


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
#   "e"     -> NA
#
# Notes:
#   GENESIS CSV files often use German number formatting with dots as
#   thousand separators and commas as decimal separators.
#
#   In this specific file, every value column is followed by a quality-marker
#   column containing entries such as "e". The script identifies only the
#   value columns, but treating "e" as missing makes the helper robust.
# ============================================================

clean_number_de <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    na_if("-") %>%
    na_if("–") %>%
    na_if(".") %>%
    na_if("x") %>%
    na_if("e") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", ".") %>%
    as.numeric()
}


# ============================================================
# Load raw delta-endpoint data
# ============================================================
#
# Purpose:
#   Load raw GENESIS / Destatis protection-seeker data for the alternative
#   2014–2017 endpoint.
#
# Input file:
#   12531-0024_de_2014_2017.csv
#
# Expected structure:
#   The raw file is a semi-structured GENESIS table.
#
#   Record dates and Länder appear as header-like rows.
#
#   Detailed observations appear below them as gender × family-status rows.
#
#   In contrast to 02_treatment.R, the selected origin countries appear in
#   columns:
#
#     column_3   Afghanistan
#     column_5   Eritrea
#     column_7   Irak (Iraq)
#     column_9   Iran, Islamische Republik (Iran, Islamic Republic)
#     column_11  Syrien (Syria)
#
#   The even-numbered columns after each value column contain quality markers
#   and are not used as values.
#
# Notes:
#   The file is read as character data to avoid premature type conversion.
# ============================================================

raw_delta_endpoint <- read.csv2(
  raw_delta_endpoint_file,
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character",
  na.strings = c("")
)

names(raw_delta_endpoint) <- paste0(
  "V",
  seq_len(ncol(raw_delta_endpoint))
)

raw_delta_endpoint <- as_tibble(
  raw_delta_endpoint
)


# ============================================================
# Diagnostic check: raw file structure
# ============================================================
#
# Purpose:
#   Inspect the raw structure before transforming the data.
#
# Reason:
#   This file is semi-structured. If the download format changes, this
#   diagnostic helps identify where the parsing logic needs adjustment.
#
# Expected:
#   The file should have multiple columns. In the current format, it has
#   twelve columns.
# ============================================================

raw_delta_endpoint %>%
  slice(1:25)

raw_delta_endpoint_file_structure <- tibble(
  n_rows = nrow(raw_delta_endpoint),
  n_columns = ncol(raw_delta_endpoint)
)

raw_delta_endpoint_file_structure

if (ncol(raw_delta_endpoint) == 1) {
  stop(
    "The raw 2014–2017 GENESIS file was read as one column. ",
    "Please check whether the file is semicolon-separated and saved as a CSV."
  )
}


# ============================================================
# Identify origin-country value columns
# ============================================================
#
# Purpose:
#   Identify the columns that contain numeric values for the five selected
#   origin countries.
#
# Logic:
#   The country header row is identified by searching all rows for the
#   selected country names.
#
#   The columns in that row containing one of the selected country names are
#   interpreted as value columns.
#
# Important:
#   Empty header cells are explicitly excluded before matching country names.
#   This prevents NA rows from entering the crosswalk.
#
# Constructed object:
#   origin_country_crosswalk
#
# Expected value columns:
#   V3   Afghanistan
#   V5   Eritrea
#   V7   Irak (Iraq)
#   V9   Iran, Islamische Republik (Iran, Islamic Republic)
#   V11  Syrien (Syria)
# ============================================================

origin_country_header_row <- raw_delta_endpoint %>%
  mutate(
    row_id = row_number(),
    row_text = apply(
      across(starts_with("V")),
      1,
      paste,
      collapse = " "
    )
  ) %>%
  filter(
    str_detect(
      row_text,
      regex(
        paste(origin_countries, collapse = "|"),
        ignore_case = TRUE
      )
    )
  ) %>%
  slice(1) %>%
  pull(row_id)

if (length(origin_country_header_row) == 0) {
  stop(
    "No country-header row containing the selected origin countries was found. ",
    "Please inspect the first rows of raw_delta_endpoint."
  )
}

origin_country_header_values <- raw_delta_endpoint %>%
  slice(origin_country_header_row) %>%
  unlist(
    use.names = FALSE
  ) %>%
  as.character() %>%
  str_squish()

origin_country_header_matches <- !is.na(origin_country_header_values) &
  str_detect(
    origin_country_header_values,
    regex(
      paste(origin_countries, collapse = "|"),
      ignore_case = TRUE
    )
  )

origin_country_value_columns <- names(raw_delta_endpoint)[
  origin_country_header_matches
]

origin_country_raw_names <- origin_country_header_values[
  origin_country_header_matches
]

origin_country_crosswalk <- tibble(
  raw_column = origin_country_value_columns,
  origin_country_raw = origin_country_raw_names
) %>%
  mutate(
    origin_country = case_when(
      str_detect(
        origin_country_raw,
        regex("^Afghanistan$", ignore_case = TRUE)
      ) ~ "Afghanistan",
      
      str_detect(
        origin_country_raw,
        regex("^Eritrea$", ignore_case = TRUE)
      ) ~ "Eritrea",
      
      str_detect(
        origin_country_raw,
        regex("^Irak$", ignore_case = TRUE)
      ) ~ "Irak",
      
      str_detect(
        origin_country_raw,
        regex("^Iran, Islamische Republik$", ignore_case = TRUE)
      ) ~ "Iran, Islamische Republik",
      
      str_detect(
        origin_country_raw,
        regex("^Syrien$", ignore_case = TRUE)
      ) ~ "Syrien",
      
      TRUE ~ origin_country_raw
    )
  )

origin_country_crosswalk

if (nrow(origin_country_crosswalk) != length(origin_countries)) {
  stop(
    "The country-column crosswalk does not contain exactly the expected number of origin countries. ",
    "Inspect origin_country_crosswalk and raw_delta_endpoint."
  )
}

if (any(is.na(origin_country_crosswalk$raw_column))) {
  stop(
    "The country-column crosswalk contains missing raw column names. ",
    "Inspect origin_country_crosswalk and the country-header row."
  )
}

if (any(is.na(origin_country_crosswalk$origin_country))) {
  stop(
    "The country-column crosswalk contains missing origin-country names. ",
    "Inspect origin_country_crosswalk and the country-header row."
  )
}


# ============================================================
# Diagnostic check: raw country value columns
# ============================================================
#
# Purpose:
#   Inspect the raw country value columns before aggregation.
#
# Reason:
#   This is comparable to treatment_raw_country_rows in 02_treatment.R.
#
#   It allows checking whether the script selected the numeric country-value
#   columns and not the quality-marker columns.
# ============================================================

delta_endpoint_raw_country_columns <- raw_delta_endpoint %>%
  select(
    all_of(origin_country_value_columns)
  )

delta_endpoint_raw_country_columns %>%
  head(20)


# ============================================================
# Construct long delta-endpoint dataset
# ============================================================
#
# Purpose:
#   Convert the semi-structured raw 2014–2017 GENESIS table into a long
#   dataset with one row per:
#
#     federal_state × origin_country × record_date
#
# Constructed object:
#   protection_seekers_2014_2017_long
#
# Main steps:
#   1. Identify record-date rows.
#   2. Identify federal-state rows.
#   3. Fill record date and Land downward.
#   4. Keep detailed gender × family-status rows.
#   5. Reshape selected origin-country value columns from wide to long.
#   6. Attach harmonised origin-country names.
#   7. Convert raw numeric values.
#   8. Exclude "Insgesamt" (total) rows to avoid double counting.
#   9. Aggregate to federal_state × origin_country × year.
#
# Important:
#   This mirrors the fill-down logic of 02_treatment.R, but uses pivot_longer
#   because the origin countries are stored in columns rather than rows.
# ============================================================

protection_seekers_2014_2017_long <- raw_delta_endpoint %>%
  
  # Clean first and second columns and identify record dates / Länder.
  mutate(
    name = str_trim(V1),
    
    family_status = str_trim(V2),
    
    record_date = ifelse(
      str_detect(
        name,
        "^\\d{2}\\.\\d{2}\\.\\d{4}$"
      ),
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
  
  # Keep only rows that belong to the two endpoint years and one Land.
  filter(
    !is.na(record_date),
    !is.na(federal_state)
  ) %>%
  
  # Keep detailed gender × family-status rows.
  # In this file, V1 is the gender category and V2 is the family-status
  # category on actual data rows.
  mutate(
    gender = str_trim(V1),
    family_status = str_trim(V2)
  ) %>%
  filter(
    str_detect(
      gender,
      regex(
        # German gender categories: male | female | diverse | not stated | total
        "männlich|weiblich|divers|ohne Angabe|Insgesamt",
        ignore_case = TRUE
      )
    ),
    str_detect(
      family_status,
      regex(
        paste(
          # German marital-status values: single, married, widowed, divorced,
          # registered civil partnership, civil partnership dissolved, unknown, total
          c(
            "ledig",
            "verheiratet",
            "verwitwet",
            "geschieden",
            "eingetragene Lebenspartnerschaft",
            "Lebenspartnerschaft aufgehoben",
            "unbekannt",
            "Insgesamt"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    )
  ) %>%
  
  # Reshape the selected country value columns from wide to long.
  pivot_longer(
    cols = all_of(origin_country_value_columns),
    names_to = "raw_column",
    values_to = "protection_seekers_raw"
  ) %>%
  
  # Attach harmonised origin-country names.
  left_join(
    origin_country_crosswalk,
    by = "raw_column"
  ) %>%
  
  # Convert record date and raw value.
  mutate(
    year = as.integer(str_sub(record_date, 7, 10)),
    
    protection_seekers = clean_number_de(
      protection_seekers_raw
    )
  ) %>%
  
  # Keep only endpoint years and selected origin countries.
  filter(
    year %in% c(2014, 2017),
    origin_country %in% origin_countries
  ) %>%
  
  # Exclude aggregate rows before aggregation to avoid double counting.
  filter(
    !str_detect(
      gender,
      regex("^Insgesamt$", ignore_case = TRUE)  # "^total$"
    ),
    !str_detect(
      family_status,
      regex("^Insgesamt$", ignore_case = TRUE)  # "^total$"
    )
  ) %>%
  
  # Aggregate over gender and family-status cells.
  group_by(
    federal_state,
    origin_country,
    year
  ) %>%
  summarise(
    protection_seekers_stock = sum(
      protection_seekers,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    origin_country,
    federal_state,
    year
  )

protection_seekers_2014_2017_long


# ============================================================
# Convert delta-endpoint data from long to wide format
# ============================================================
#
# Purpose:
#   Construct the alternative 2014–2017 exposure dataset with one row per:
#
#     federal_state × origin_country
#
# Constructed object:
#   protection_seekers_delta_2014_2017
#
# Constructed variables:
#   protection_seekers_stock_2014
#   protection_seekers_stock_2017
#   delta_protection_seekers_2014_2017
# ============================================================

protection_seekers_delta_2014_2017 <- protection_seekers_2014_2017_long %>%
  select(
    federal_state,
    origin_country,
    year,
    protection_seekers_stock
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = protection_seekers_stock,
    names_prefix = "protection_seekers_stock_"
  ) %>%
  mutate(
    delta_protection_seekers_2014_2017 =
      protection_seekers_stock_2017 - protection_seekers_stock_2014
  ) %>%
  select(
    federal_state,
    origin_country,
    protection_seekers_stock_2014,
    protection_seekers_stock_2017,
    delta_protection_seekers_2014_2017
  ) %>%
  arrange(
    origin_country,
    federal_state
  )

protection_seekers_delta_2014_2017


# ============================================================
# Delta-endpoint data checks
# ============================================================
#
# Purpose:
#   Inspect the cleaned 2014–2017 delta-endpoint dataset and verify sample
#   coverage.
# ============================================================

### Structure

str(protection_seekers_delta_2014_2017)

summary(protection_seekers_delta_2014_2017)

head(
  protection_seekers_delta_2014_2017,
  10
)


### Sample size and coverage

delta_endpoint_panel_summary <- protection_seekers_delta_2014_2017 %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    missing_stock_2014 = sum(is.na(protection_seekers_stock_2014)),
    missing_stock_2017 = sum(is.na(protection_seekers_stock_2017)),
    missing_delta = sum(is.na(delta_protection_seekers_2014_2017))
  )

delta_endpoint_panel_summary


### Check observations by origin country

delta_endpoint_by_origin <- protection_seekers_delta_2014_2017 %>%
  count(
    origin_country
  )

delta_endpoint_by_origin


### Check observations by Land

delta_endpoint_by_state <- protection_seekers_delta_2014_2017 %>%
  count(
    federal_state
  )

delta_endpoint_by_state


### Check duplicates

duplicate_delta_endpoint_pairs <- protection_seekers_delta_2014_2017 %>%
  count(
    federal_state,
    origin_country
  ) %>%
  filter(
    n > 1
  )

duplicate_delta_endpoint_pairs


### Check expected full coverage

delta_endpoint_expected_grid <- expand.grid(
  federal_state = federal_states,
  origin_country = origin_countries,
  stringsAsFactors = FALSE
) %>%
  as_tibble()

missing_delta_endpoint_pairs <- delta_endpoint_expected_grid %>%
  anti_join(
    protection_seekers_delta_2014_2017 %>%
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

missing_delta_endpoint_pairs


### Check exposure variation

delta_endpoint_variation_summary <- protection_seekers_delta_2014_2017 %>%
  summarise(
    min_stock_2014 = min(protection_seekers_stock_2014, na.rm = TRUE),
    max_stock_2014 = max(protection_seekers_stock_2014, na.rm = TRUE),
    min_stock_2017 = min(protection_seekers_stock_2017, na.rm = TRUE),
    max_stock_2017 = max(protection_seekers_stock_2017, na.rm = TRUE),
    min_delta = min(delta_protection_seekers_2014_2017, na.rm = TRUE),
    max_delta = max(delta_protection_seekers_2014_2017, na.rm = TRUE)
  )

delta_endpoint_variation_summary


# ============================================================
# Construct national 2014–2017 totals
# ============================================================
#
# Purpose:
#   Construct national origin-specific protection-seeker stocks and national
#   2014–2017 exposure changes.
#
# Constructed object:
#   national_protection_seekers_delta_2014_2017
#
# Constructed variables:
#   national_protection_seekers_stock_2014
#   national_protection_seekers_stock_2017
#   national_delta_protection_seekers_2014_2017
#
# Interpretation:
#   These national totals provide the national exposure component that is
#   allocated across Länder using the Königstein allocation shares.
# ============================================================

national_protection_seekers_delta_2014_2017 <- protection_seekers_delta_2014_2017 %>%
  group_by(
    origin_country
  ) %>%
  summarise(
    national_protection_seekers_stock_2014 =
      sum(protection_seekers_stock_2014, na.rm = TRUE),
    
    national_protection_seekers_stock_2017 =
      sum(protection_seekers_stock_2017, na.rm = TRUE),
    
    national_delta_protection_seekers_2014_2017 =
      sum(delta_protection_seekers_2014_2017, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(
    origin_country
  )

national_protection_seekers_delta_2014_2017


# ============================================================
# Final delta-endpoint consistency check
# ============================================================
#
# Purpose:
#   Collect key delta-endpoint data diagnostics in one compact object.
#
# Interpretation:
#   This object quickly shows whether the cleaned 2014–2017 exposure dataset
#   has the expected state-origin coverage, missingness pattern, and duplicate
#   structure.
# ============================================================

delta_endpoint_consistency_check <- tibble(
  check = c(
    "contains_expected_number_of_states",
    "contains_expected_number_of_origins",
    "contains_expected_number_of_state_origin_pairs",
    "has_no_duplicate_state_origin_pairs",
    "has_no_missing_stock_2014",
    "has_no_missing_stock_2017",
    "has_no_missing_delta"
  ),
  value = c(
    n_distinct(protection_seekers_delta_2014_2017$federal_state) ==
      length(federal_states),
    
    n_distinct(protection_seekers_delta_2014_2017$origin_country) ==
      length(origin_countries),
    
    nrow(protection_seekers_delta_2014_2017) ==
      length(federal_states) * length(origin_countries),
    
    nrow(duplicate_delta_endpoint_pairs) == 0,
    
    sum(is.na(protection_seekers_delta_2014_2017$protection_seekers_stock_2014)) == 0,
    
    sum(is.na(protection_seekers_delta_2014_2017$protection_seekers_stock_2017)) == 0,
    
    sum(is.na(protection_seekers_delta_2014_2017$delta_protection_seekers_2014_2017)) == 0
  )
)

delta_endpoint_consistency_check


# ============================================================
# Helper function: add 2014–2017 delta-endpoint variables
# ============================================================
#
# Purpose:
#   Add the 2014–2017 delta-endpoint treatment and IV variables to a given
#   analysis panel.
#
# Logic:
#   The actual 2014–2017 exposure change is:
#
#     delta_protection_seekers_2014_2017
#
#   The predicted 2014–2017 exposure change is:
#
#     national_delta_protection_seekers_2014_2017
#     × koenigstein_share_2015_2016_avg
#
#   Both are interacted with post_period because the exposure shock is only
#   used as a treatment intensity after the shock.
#
# Constructed variables:
#   protection_seekers_stock_2017
#   delta_protection_seekers_2014_2017
#   national_protection_seekers_stock_2017
#   national_delta_protection_seekers_2014_2017
#   predicted_protection_seekers_stock_2017
#   predicted_delta_protection_seekers_2014_2017
#   treatment_delta_2014_2017_post
#   iv_delta_2014_2017_post
#   treatment_delta_2014_2017_post_1000
#   iv_delta_2014_2017_post_1000
# ============================================================

add_delta_endpoint_2014_2017 <- function(data) {
  data %>%
    left_join(
      protection_seekers_delta_2014_2017 %>%
        select(
          federal_state,
          origin_country,
          protection_seekers_stock_2017,
          delta_protection_seekers_2014_2017
        ),
      by = c(
        "federal_state",
        "origin_country"
      )
    ) %>%
    left_join(
      national_protection_seekers_delta_2014_2017 %>%
        select(
          origin_country,
          national_protection_seekers_stock_2017,
          national_delta_protection_seekers_2014_2017
        ),
      by = "origin_country"
    ) %>%
    mutate(
      predicted_protection_seekers_stock_2017 =
        national_protection_seekers_stock_2017 *
        koenigstein_share_2015_2016_avg,
      
      predicted_delta_protection_seekers_2014_2017 =
        national_delta_protection_seekers_2014_2017 *
        koenigstein_share_2015_2016_avg,
      
      treatment_delta_2014_2017_post =
        delta_protection_seekers_2014_2017 * post_period,
      
      iv_delta_2014_2017_post =
        predicted_delta_protection_seekers_2014_2017 * post_period,
      
      treatment_delta_2014_2017_post_1000 =
        treatment_delta_2014_2017_post / 1000,
      
      iv_delta_2014_2017_post_1000 =
        iv_delta_2014_2017_post / 1000
    )
}


# ============================================================
# Construct delta-endpoint panels
# ============================================================
#
# Purpose:
#   Construct full-sample and no-Eritrea panels containing the 2014–2017
#   delta-endpoint treatment and IV variables.
#
# Constructed panels:
#   analysis_panel_delta_endpoint
#   analysis_panel_no_eritrea_delta_endpoint
# ============================================================

analysis_panel_delta_endpoint <- add_delta_endpoint_2014_2017(
  analysis_panel
)

analysis_panel_no_eritrea_delta_endpoint <- add_delta_endpoint_2014_2017(
  analysis_panel_no_eritrea
)


# ============================================================
# Required-variable check after construction
# ============================================================
#
# Purpose:
#   Check whether the constructed delta-endpoint panels contain all variables
#   required for the robustness specifications.
# ============================================================

required_delta_endpoint_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "post_period",
  "export_value",
  "log_export_value",
  "koenigstein_share_2015_2016_avg",
  "treatment_delta_2014_2017_post_1000",
  "iv_delta_2014_2017_post_1000",
  "fe_state_origin",
  "fe_state_year",
  "fe_origin_year"
)

missing_delta_endpoint_variables <- bind_rows(
  tibble(
    panel = "analysis_panel_delta_endpoint",
    variable = required_delta_endpoint_variables,
    present = required_delta_endpoint_variables %in%
      names(analysis_panel_delta_endpoint)
  ),
  
  tibble(
    panel = "analysis_panel_no_eritrea_delta_endpoint",
    variable = required_delta_endpoint_variables,
    present = required_delta_endpoint_variables %in%
      names(analysis_panel_no_eritrea_delta_endpoint)
  )
) %>%
  filter(
    !present
  )

missing_delta_endpoint_variables

if (nrow(missing_delta_endpoint_variables) > 0) {
  stop(
    "At least one required delta-endpoint variable is missing after construction. Inspect missing_delta_endpoint_variables."
  )
}


# ============================================================
# Delta-endpoint panel diagnostics
# ============================================================
#
# Purpose:
#   Document the structure and variation of the constructed robustness panels.
# ============================================================

robustness_delta_endpoint_diagnostics <- bind_rows(
  analysis_panel_delta_endpoint %>%
    summarise(
      panel = "analysis_panel_delta_endpoint",
      sample = "Full sample",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      missing_treatment_delta_2014_2017_post_1000 = sum(
        is.na(treatment_delta_2014_2017_post_1000)
      ),
      missing_iv_delta_2014_2017_post_1000 = sum(
        is.na(iv_delta_2014_2017_post_1000)
      ),
      mean_treatment_delta_2014_2017_1000 = mean(
        treatment_delta_2014_2017_post_1000,
        na.rm = TRUE
      ),
      sd_treatment_delta_2014_2017_1000 = sd(
        treatment_delta_2014_2017_post_1000,
        na.rm = TRUE
      ),
      mean_iv_delta_2014_2017_1000 = mean(
        iv_delta_2014_2017_post_1000,
        na.rm = TRUE
      ),
      sd_iv_delta_2014_2017_1000 = sd(
        iv_delta_2014_2017_post_1000,
        na.rm = TRUE
      )
    ),
  
  analysis_panel_no_eritrea_delta_endpoint %>%
    summarise(
      panel = "analysis_panel_no_eritrea_delta_endpoint",
      sample = "Excluding Eritrea",
      n_obs = n(),
      n_state_origin_pairs = n_distinct(fe_state_origin),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      missing_treatment_delta_2014_2017_post_1000 = sum(
        is.na(treatment_delta_2014_2017_post_1000)
      ),
      missing_iv_delta_2014_2017_post_1000 = sum(
        is.na(iv_delta_2014_2017_post_1000)
      ),
      mean_treatment_delta_2014_2017_1000 = mean(
        treatment_delta_2014_2017_post_1000,
        na.rm = TRUE
      ),
      sd_treatment_delta_2014_2017_1000 = sd(
        treatment_delta_2014_2017_post_1000,
        na.rm = TRUE
      ),
      mean_iv_delta_2014_2017_1000 = mean(
        iv_delta_2014_2017_post_1000,
        na.rm = TRUE
      ),
      sd_iv_delta_2014_2017_1000 = sd(
        iv_delta_2014_2017_post_1000,
        na.rm = TRUE
      )
    )
)

robustness_delta_endpoint_diagnostics


# ============================================================
# Delta-endpoint construction summary
# ============================================================
#
# Purpose:
#   Collect key diagnostics from the delta-endpoint construction in one
#   compact object.
# ============================================================

delta_endpoint_construction_summary <- tibble(
  check = c(
    "input_files_missing",
    "base_panel_variables_missing",
    "constructed_delta_endpoint_variables_missing",
    "missing_delta_endpoint_pairs",
    "full_sample_panel_observations",
    "no_eritrea_panel_observations",
    "raw_file_rows",
    "raw_file_columns"
  ),
  value = c(
    length(missing_input_files),
    nrow(missing_delta_endpoint_base_variables),
    nrow(missing_delta_endpoint_variables),
    nrow(missing_delta_endpoint_pairs),
    nrow(analysis_panel_delta_endpoint),
    nrow(analysis_panel_no_eritrea_delta_endpoint),
    nrow(raw_delta_endpoint),
    ncol(raw_delta_endpoint)
  )
)

delta_endpoint_construction_summary


# ============================================================
# Save cleaned delta-endpoint data and panels
# ============================================================
#
# Purpose:
#   Save the cleaned 2014–2017 exposure dataset, the constructed robustness
#   panels, and diagnostic objects.
# ============================================================

### Main cleaned delta-endpoint objects

saveRDS(
  protection_seekers_delta_2014_2017,
  "protection_seekers_delta_2014_2017.rds"
)

saveRDS(
  protection_seekers_2014_2017_long,
  "protection_seekers_2014_2017_long.rds"
)

saveRDS(
  national_protection_seekers_delta_2014_2017,
  "national_protection_seekers_delta_2014_2017.rds"
)


### Delta-endpoint analysis panels

saveRDS(
  analysis_panel_delta_endpoint,
  "analysis_panel_delta_endpoint.rds"
)

saveRDS(
  analysis_panel_no_eritrea_delta_endpoint,
  "analysis_panel_no_eritrea_delta_endpoint.rds"
)


### Raw and parsing diagnostics

saveRDS(
  raw_delta_endpoint,
  "raw_delta_endpoint.rds"
)

saveRDS(
  raw_delta_endpoint_file_structure,
  "raw_delta_endpoint_file_structure.rds"
)

saveRDS(
  origin_country_crosswalk,
  "origin_country_crosswalk_delta_endpoint.rds"
)

saveRDS(
  delta_endpoint_raw_country_columns,
  "delta_endpoint_raw_country_columns.rds"
)


### Data checks and summaries

saveRDS(
  delta_endpoint_panel_summary,
  "delta_endpoint_panel_summary.rds"
)

saveRDS(
  delta_endpoint_by_origin,
  "delta_endpoint_by_origin.rds"
)

saveRDS(
  delta_endpoint_by_state,
  "delta_endpoint_by_state.rds"
)

saveRDS(
  duplicate_delta_endpoint_pairs,
  "duplicate_delta_endpoint_pairs.rds"
)

saveRDS(
  missing_delta_endpoint_pairs,
  "missing_delta_endpoint_pairs.rds"
)

saveRDS(
  delta_endpoint_variation_summary,
  "delta_endpoint_variation_summary.rds"
)

saveRDS(
  delta_endpoint_consistency_check,
  "delta_endpoint_consistency_check.rds"
)

saveRDS(
  robustness_delta_endpoint_diagnostics,
  "robustness_delta_endpoint_diagnostics.rds"
)

saveRDS(
  delta_endpoint_construction_summary,
  "delta_endpoint_construction_summary.rds"
)

saveRDS(
  missing_delta_endpoint_base_variables,
  "missing_delta_endpoint_base_variables.rds"
)

saveRDS(
  missing_delta_endpoint_variables,
  "missing_delta_endpoint_variables.rds"
)


# ============================================================
# Clean temporary objects
# ============================================================

rm(
  raw_delta_endpoint_file,
  required_input_files,
  missing_input_files,
  required_delta_endpoint_base_variables,
  required_delta_endpoint_variables,
  delta_endpoint_expected_grid,
  origin_country_header_row,
  origin_country_header_values,
  origin_country_header_matches,
  origin_country_value_columns,
  origin_country_raw_names,
  add_fixed_effects_if_missing,
  add_delta_endpoint_2014_2017,
  clean_number_de,
  federal_states,
  origin_countries
)


# ============================================================
# Final objects kept
# ============================================================
#
# Cleaned delta-endpoint data:
#   protection_seekers_delta_2014_2017
#
# Intermediate cleaned delta-endpoint data:
#   protection_seekers_2014_2017_long
#
# National delta-endpoint totals:
#   national_protection_seekers_delta_2014_2017
#
# Constructed delta-endpoint panels:
#   analysis_panel_delta_endpoint
#   analysis_panel_no_eritrea_delta_endpoint
#
# Raw diagnostic objects:
#   raw_delta_endpoint
#   raw_delta_endpoint_file_structure
#   origin_country_crosswalk
#   delta_endpoint_raw_country_columns
#
# Summary and diagnostic objects:
#   delta_endpoint_panel_summary
#   delta_endpoint_by_origin
#   delta_endpoint_by_state
#   duplicate_delta_endpoint_pairs
#   missing_delta_endpoint_pairs
#   delta_endpoint_variation_summary
#   delta_endpoint_consistency_check
#   robustness_delta_endpoint_diagnostics
#   delta_endpoint_construction_summary
#   missing_delta_endpoint_base_variables
#   missing_delta_endpoint_variables
#
# Notes:
#   protection_seekers_delta_2014_2017 is the cleaned 2014–2017 alternative
#   exposure dataset.
#
#   Unit of observation:
#     federal_state × origin_country
#
#   Alternative treatment variable:
#     delta_protection_seekers_2014_2017
#
#   Regression-ready post-period interactions:
#     treatment_delta_2014_2017_post_1000
#     iv_delta_2014_2017_post_1000
#
#   These are added to:
#     analysis_panel_delta_endpoint
#     analysis_panel_no_eritrea_delta_endpoint
#
#   This is a data-cleaning / construction script. It builds the cleaned
#   2014–2017 exposure data from the raw GENESIS CSV and saves the resulting
#   panels as .rds files for later robustness regressions.
#
#   Later scripts should load analysis_panel_delta_endpoint.rds and
#   analysis_panel_no_eritrea_delta_endpoint.rds directly instead of
#   reconstructing the 2014–2017 exposure variables from raw data.
# ============================================================