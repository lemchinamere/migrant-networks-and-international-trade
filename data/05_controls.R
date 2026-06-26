# ============================================================
# Land × year controls and archived CEPII gravity controls
# ============================================================
#
# Purpose:
#   Construct regional federal_state × year controls, merge them into the
#   main analysis panel, and construct the active regional-control
#   robustness panel.
#
# Script type:
#   Data-cleaning / panel-construction script
#
# Active regional controls:
#   1. GDP
#   2. Population
#   3. Unemployment rate
#   4. Employment
#   5. Manufacturing share
#   6. Total exports to the world
#
# Archived CEPII / gravity controls:
#   7. CEPII Gravity controls for Germany to the five origin countries
#
# Unit of observation in main panel:
#   federal_state × origin_country × year
#
# Unit of observation for regional controls:
#   federal_state × year
#
# Active final output objects:
#   gdp_controls
#   population_controls
#   unemployment_controls
#   employment_controls
#   manufacturing_controls
#   total_exports_world_controls
#   analysis_panel
#   analysis_panel_controls
#   analysis_panel_controls_no_eritrea
#
# Archived output objects:
#   cepii_gravity_controls_clean
#   origin_mapping
#   cepii_gravity
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Workflow logic:
#   This is a data-cleaning / panel-construction script.
#
#   Therefore, the regional-control datasets are built from their raw CSV
#   files and then saved as .rds objects.
#
#   The script loads the already constructed main analysis_panel.rds from
#   disk, adds regional controls, constructs robustness panels, and saves the
#   updated objects.
#
#   Later regression scripts should load the saved .rds objects directly
#   instead of rebuilding the controls from raw data.
#
# Notes:
#   Regional controls are merged into analysis_panel for robustness checks.
#   They are not used in the preferred main specification because the
#   preferred model includes federal_state × year fixed effects, which absorb
#   federal_state-year-level controls.
#
#   analysis_panel_controls is restricted to 2010–2024 because some regional
#   controls, especially population, are not available for 2025.
#
#   Treatment and IV variables scaled by 1,000 persons are not constructed
#   here. They are created in the separate rescaling script after the
#   regional-control panels have been constructed.
#
#   analysis_panel_cepii is restricted to 2010–2021 because the available
#   CEPII Gravity data end in 2021. It is archived only.
#
#   The CEPII / gravity-control robustness check was considered but not
#   retained because the gravity controls were absorbed by the remaining fixed
#   effects and dropped due to collinearity.
# ============================================================


# ============================================================
# Setup
# ============================================================

### Path

# Run from the project's data/ folder (see README). Falls back gracefully if started from the repo root.
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")


### Packages

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)


# ============================================================
# Required input files
# ============================================================
#
# Purpose:
#   Define all raw and already constructed input files required by this
#   data-cleaning / panel-construction script.
#
# Already constructed input:
#   analysis_panel.rds
#
# Raw regional-control input files:
#   82111-0010_de.csv
#   12411-0010_de.csv
#   13211-0007_de.csv
#   13311-0002_de.csv
#   82111-0011_de.csv
#   51000-0032_all_countries_de.csv
#
# Raw CEPII input file:
#   Gravity_V202211.csv
#
# Notes:
#   This script should stop early if a required input is missing, because all
#   outputs below depend on these files.
# ============================================================

required_input_files <- c(
  "analysis_panel.rds",
  "82111-0010_de.csv",
  "12411-0010_de.csv",
  "13211-0007_de.csv",
  "13311-0002_de.csv",
  "82111-0011_de.csv",
  "51000-0032_all_countries_de.csv",
  "Gravity_V202211.csv"
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
      "Please place them in the working directory before running this script."
    )
  )
}


# ============================================================
# Load main analysis panel
# ============================================================
#
# Purpose:
#   Load the already constructed main analysis panel from disk before adding
#   regional controls.
#
# Important:
#   This script does not reconstruct analysis_panel from raw data.
#   It loads analysis_panel.rds, adds controls, and saves the updated panel.
#
# Notes:
#   The _1000 rescaled treatment and IV variables do not need to exist yet,
#   because rescaling is handled in a separate script after control panels
#   are created.
# ============================================================

analysis_panel <- readRDS(
  "analysis_panel.rds"
)


# ============================================================
# Reference objects
# ============================================================
#
# Purpose:
#   Define reference objects required for cleaning and merging controls.
#
# Objects:
#   federal_states
#   origin_mapping
#
# Notes:
#   These are defined inside the script so that the script does not rely on
#   objects already present in the R environment.
# ============================================================

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


origin_mapping <- tibble(
  origin_country = c(
    "Afghanistan",
    "Eritrea",
    "Irak",
    "Iran, Islamische Republik",
    "Syrien"
  ),
  iso3_d = c(
    "AFG",
    "ERI",
    "IRQ",
    "IRN",
    "SYR"
  )
)


# ============================================================
# Helper functions
# ============================================================
#
# Purpose:
#   Define helper functions used repeatedly across the regional-control
#   cleaning steps.
#
# Functions:
#   clean_number_de()
#     Converts German-formatted numeric strings into numeric values.
#
#   find_year_row()
#     Identifies the row containing year headers in wide GENESIS-style
#     tables.
#
#   merge_control()
#     Merges a federal_state × year control dataset into the analysis panel
#     while first removing previous versions of the same variables.
#
#   add_fixed_effect_identifiers()
#     Adds the fixed-effect identifiers used in the empirical specifications.
#
# Notes:
#   merge_control() prevents duplicate .x and .y columns if a control
#   variable already exists in the panel.
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


find_year_row <- function(data) {
  which(
    apply(
      data,
      1,
      function(x) {
        sum(
          str_detect(
            as.character(x),
            "^20[0-9]{2}$"
          ),
          na.rm = TRUE
        ) >= 2
      }
    )
  )[1]
}


merge_control <- function(panel, control_data, new_variables) {
  panel %>%
    select(
      -any_of(c(
        new_variables,
        paste0(new_variables, ".x"),
        paste0(new_variables, ".y")
      ))
    ) %>%
    left_join(
      control_data,
      by = c(
        "federal_state",
        "year"
      )
    )
}


add_fixed_effect_identifiers <- function(data) {
  data %>%
    mutate(
      fe_state_origin =
        interaction(
          federal_state,
          origin_country,
          drop = TRUE
        ),
      
      fe_state_year =
        interaction(
          federal_state,
          year,
          drop = TRUE
        ),
      
      fe_origin_year =
        interaction(
          origin_country,
          year,
          drop = TRUE
        )
    )
}


# ============================================================
# Required-variable check for main analysis panel
# ============================================================
#
# Purpose:
#   Check whether analysis_panel contains the identifiers needed for merging
#   regional controls and constructing fixed effects.
# ============================================================

required_analysis_panel_variables <- c(
  "federal_state",
  "origin_country",
  "year",
  "export_value"
)

missing_analysis_panel_variables <- tibble(
  variable = required_analysis_panel_variables,
  present = required_analysis_panel_variables %in% names(analysis_panel)
) %>%
  filter(
    !present
  )

missing_analysis_panel_variables

if (nrow(missing_analysis_panel_variables) > 0) {
  stop(
    "analysis_panel is missing at least one required variable. Inspect missing_analysis_panel_variables."
  )
}


# ============================================================
# Ensure fixed-effect identifiers in analysis panel
# ============================================================
#
# Purpose:
#   Ensure that the updated analysis panel contains the fixed-effect
#   identifiers used in downstream empirical specifications.
#
# Notes:
#   If they already exist, they are overwritten consistently.
# ============================================================

analysis_panel <- add_fixed_effect_identifiers(
  analysis_panel
)


# ============================================================
# Control 1: GDP by Land and year
# ============================================================
#
# Data source:
#   GENESIS / Destatis
#   Table 82111-0010
#
# Purpose:
#   Construct annual GDP controls at the federal_state × year level.
#
# Constructed variable:
#   gdp_million_eur
# ============================================================

gdp_raw <- read.csv2(
  "82111-0010_de.csv",
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character"
) %>%
  mutate(
    across(
      everything(),
      ~ str_squish(.x)
    )
  )

year_row <- find_year_row(
  gdp_raw
)

year_headers <- as.character(
  unlist(
    gdp_raw[year_row, ]
  )
)

year_cols <- which(
  str_detect(
    year_headers,
    "^20[0-9]{2}$"
  )
)

gdp_controls <- gdp_raw %>%
  slice((year_row + 1):n()) %>%
  select(
    federal_state = V1,
    all_of(names(gdp_raw)[year_cols])
  )

names(gdp_controls) <- c(
  "federal_state",
  year_headers[year_cols]
)

gdp_controls <- gdp_controls %>%
  mutate(
    federal_state = str_squish(federal_state),
    federal_state = str_replace_all(federal_state, "\u00A0", " ")
  ) %>%
  filter(
    federal_state %in% federal_states
  ) %>%
  pivot_longer(
    cols = -federal_state,
    names_to = "year",
    values_to = "gdp_million_eur"
  ) %>%
  mutate(
    year = as.integer(year),
    gdp_million_eur = clean_number_de(gdp_million_eur)
  ) %>%
  arrange(
    federal_state,
    year
  )


gdp_controls_summary <- gdp_controls %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    missing_gdp = sum(is.na(gdp_million_eur))
  )

gdp_controls_summary

gdp_controls %>%
  count(year)

gdp_controls %>%
  count(federal_state)


analysis_panel <- merge_control(
  panel = analysis_panel,
  control_data = gdp_controls,
  new_variables = "gdp_million_eur"
)


# ============================================================
# Control 2: Population by Land and year
# ============================================================
#
# Data source:
#   GENESIS / Destatis
#   Table 12411-0010
#
# Purpose:
#   Construct annual population controls at the federal_state × year level.
#
# Constructed variable:
#   population
# ============================================================

population_raw <- read.csv2(
  "12411-0010_de.csv",
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character"
) %>%
  mutate(
    across(
      everything(),
      ~ str_squish(.x)
    )
  )

state_row <- which(
  apply(
    population_raw,
    1,
    function(x) {
      any(x %in% federal_states)
    }
  )
)[1]

state_headers <- as.character(
  unlist(
    population_raw[state_row, ]
  )
)

state_cols <- which(
  state_headers %in% federal_states
)

population_controls <- population_raw %>%
  slice((state_row + 1):n()) %>%
  select(
    record_date = V1,
    all_of(names(population_raw)[state_cols])
  )

names(population_controls) <- c(
  "record_date",
  state_headers[state_cols]
)

population_controls <- population_controls %>%
  filter(
    str_detect(
      record_date,
      "^\\d{2}\\.\\d{2}\\.\\d{4}$"
    )
  ) %>%
  mutate(
    year = as.integer(
      str_extract(
        record_date,
        "\\d{4}"
      )
    )
  ) %>%
  select(
    -record_date
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "federal_state",
    values_to = "population"
  ) %>%
  mutate(
    federal_state = str_squish(federal_state),
    population = clean_number_de(population)
  ) %>%
  filter(
    federal_state %in% federal_states
  ) %>%
  arrange(
    federal_state,
    year
  )


population_controls_summary <- population_controls %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    missing_population = sum(is.na(population))
  )

population_controls_summary

population_controls %>%
  count(year)

population_controls %>%
  count(federal_state)


analysis_panel <- merge_control(
  panel = analysis_panel,
  control_data = population_controls,
  new_variables = "population"
)


# ============================================================
# Control 3: Unemployment rate by Land and year
# ============================================================
#
# Data source:
#   GENESIS / Destatis
#   Table 13211-0007
#
# Purpose:
#   Construct annual unemployment-rate controls at the
#   federal_state × year level.
#
# Constructed variable:
#   unemployment_rate
# ============================================================

unemployment_raw <- read.csv2(
  "13211-0007_de.csv",
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character"
) %>%
  mutate(
    across(
      everything(),
      ~ str_squish(.x)
    )
  )

year_row <- find_year_row(
  unemployment_raw
)

year_headers <- as.character(
  unlist(
    unemployment_raw[year_row, ]
  )
)

year_cols <- which(
  str_detect(
    year_headers,
    "^20[0-9]{2}$"
  )
)

unemployment_step <- unemployment_raw %>%
  slice((year_row + 1):n()) %>%
  select(
    name = V1,
    unit = V2,
    all_of(names(unemployment_raw)[year_cols])
  )

names(unemployment_step) <- c(
  "name",
  "unit",
  year_headers[year_cols]
)

unemployment_controls <- unemployment_step %>%
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
  filter(
    str_detect(
      name,
      # German data value: "Unemployment rate of all civilian labour force"
      "Arbeitslosenquote aller zivilen Erwerbspersonen"
    )
  ) %>%
  pivot_longer(
    cols = matches("^20[0-9]{2}$"),
    names_to = "year",
    values_to = "unemployment_rate"
  ) %>%
  mutate(
    year = as.integer(year),
    unemployment_rate = clean_number_de(unemployment_rate)
  ) %>%
  select(
    federal_state,
    year,
    unemployment_rate
  ) %>%
  filter(
    federal_state %in% federal_states
  ) %>%
  arrange(
    federal_state,
    year
  )


unemployment_controls_summary <- unemployment_controls %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    missing_unemployment_rate = sum(is.na(unemployment_rate))
  )

unemployment_controls_summary

unemployment_controls %>%
  count(year)

unemployment_controls %>%
  count(federal_state)


analysis_panel <- merge_control(
  panel = analysis_panel,
  control_data = unemployment_controls,
  new_variables = "unemployment_rate"
)


# ============================================================
# Control 4: Employment by Land and year
# ============================================================
#
# Data source:
#   GENESIS / Destatis
#   Table 13311-0002
#
# Purpose:
#   Construct annual employment controls at the federal_state × year level.
#
# Constructed variable:
#   employment_thousand_persons
# ============================================================

employment_raw <- read.csv2(
  "13311-0002_de.csv",
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character"
) %>%
  mutate(
    across(
      everything(),
      ~ str_squish(.x)
    )
  )

year_row <- find_year_row(
  employment_raw
)

year_headers <- as.character(
  unlist(
    employment_raw[year_row, ]
  )
)

year_cols <- which(
  str_detect(
    year_headers,
    "^20[0-9]{2}$"
  )
)

employment_step <- employment_raw %>%
  slice((year_row + 1):n()) %>%
  select(
    name = V1,
    sector = V2,
    variable = V3,
    unit = V4,
    all_of(names(employment_raw)[year_cols])
  )

names(employment_step) <- c(
  "name",
  "sector",
  "variable",
  "unit",
  year_headers[year_cols]
)

employment_controls <- employment_step %>%
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
  filter(
    str_detect(variable, "Erwerbstätige"),     # "Employed persons"
    str_detect(variable, "Inlandskonzept"),    # "domestic concept" (place-of-work basis)
    str_detect(sector, "Insgesamt|Alle Wirtschaftsbereiche|A-T|A-S|WZ08 Insgesamt")  # "Insgesamt"/"Alle Wirtschaftsbereiche" = total / all economic sectors
  ) %>%
  pivot_longer(
    cols = matches("^20[0-9]{2}$"),
    names_to = "year",
    values_to = "employment_thousand_persons"
  ) %>%
  mutate(
    year = as.integer(year),
    employment_thousand_persons =
      clean_number_de(employment_thousand_persons)
  ) %>%
  select(
    federal_state,
    year,
    employment_thousand_persons
  ) %>%
  filter(
    federal_state %in% federal_states
  ) %>%
  arrange(
    federal_state,
    year
  )


employment_controls_summary <- employment_controls %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    missing_employment = sum(is.na(employment_thousand_persons))
  )

employment_controls_summary

employment_controls %>%
  count(year)

employment_controls %>%
  count(federal_state)

summary(
  employment_controls$employment_thousand_persons
)


analysis_panel <- merge_control(
  panel = analysis_panel,
  control_data = employment_controls,
  new_variables = "employment_thousand_persons"
)


# ============================================================
# Control 5: Manufacturing share by Land and year
# ============================================================
#
# Data source:
#   GENESIS / Destatis
#   Table 82111-0011
#
# Purpose:
#   Construct annual manufacturing-share controls at the
#   federal_state × year level.
#
# Constructed variables:
#   gva_total
#   gva_manufacturing
#   manufacturing_share
# ============================================================

gva_raw <- read.csv2(
  "82111-0011_de.csv",
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character"
) %>%
  mutate(
    across(
      everything(),
      ~ str_squish(.x)
    )
  )

state_headers <- as.character(
  unlist(
    gva_raw[8, ]
  )
)

state_cols <- which(
  state_headers %in% federal_states
)

gva_long <- gva_raw %>%
  mutate(
    year = ifelse(
      V1 %in% as.character(2010:2025),
      V1,
      NA_character_
    )
  ) %>%
  fill(
    year,
    .direction = "down"
  ) %>%
  filter(
    V1 %in% c(
      "WZ08-A",
      "WZ08-B-F",
      "WZ08-G-T",
      "WZ08-C"
    )
  ) %>%
  select(
    year,
    sector_code = V1,
    sector_name = V2,
    all_of(names(gva_raw)[state_cols])
  )

names(gva_long) <- c(
  "year",
  "sector_code",
  "sector_name",
  state_headers[state_cols]
)

gva_long <- gva_long %>%
  pivot_longer(
    cols = all_of(federal_states),
    names_to = "federal_state",
    values_to = "gva_value"
  ) %>%
  mutate(
    year = as.integer(year),
    gva_value = clean_number_de(gva_value),
    sector_type = case_when(
      sector_code == "WZ08-C" ~ "gva_manufacturing",
      sector_code %in% c(
        "WZ08-A",
        "WZ08-B-F",
        "WZ08-G-T"
      ) ~ "gva_total_component",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    federal_state %in% federal_states,
    !is.na(sector_type),
    !is.na(gva_value)
  )

manufacturing_controls <- gva_long %>%
  group_by(
    federal_state,
    year
  ) %>%
  summarise(
    gva_total = sum(
      gva_value[sector_type == "gva_total_component"],
      na.rm = TRUE
    ),
    gva_manufacturing = sum(
      gva_value[sector_type == "gva_manufacturing"],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    manufacturing_share =
      gva_manufacturing / gva_total
  ) %>%
  arrange(
    federal_state,
    year
  )


manufacturing_controls_summary <- manufacturing_controls %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    missing_gva_total = sum(is.na(gva_total)),
    missing_gva_manufacturing = sum(is.na(gva_manufacturing)),
    missing_manufacturing_share = sum(is.na(manufacturing_share))
  )

manufacturing_controls_summary

manufacturing_controls %>%
  count(year)

manufacturing_controls %>%
  count(federal_state)

summary(
  manufacturing_controls$manufacturing_share
)


analysis_panel <- merge_control(
  panel = analysis_panel,
  control_data = manufacturing_controls,
  new_variables = c(
    "gva_total",
    "gva_manufacturing",
    "manufacturing_share"
  )
)


# ============================================================
# Control 6: Total federal-state exports to the world
# ============================================================
#
# Data source:
#   GENESIS / Destatis foreign trade data
#   Table 51000-0032:
#   Aus- und Einfuhr (Außenhandel): Bundesländer, Jahre, Länder
#   (English: Exports and imports (foreign trade): federal states, years, countries)
#
# Purpose:
#   Construct annual total exports to the world by Land.
#
# Constructed variable:
#   total_exports_world
#
# Important:
#   If the raw file contains aggregate rows such as "Insgesamt" (total), "Welt"
#   (world), "Alle Länder" (all countries), or similar, these rows are excluded
#   before aggregation.
#
#   Otherwise, total exports would be double counted because the aggregate
#   row would be added to the sum of individual destination countries.
# ============================================================

raw_exports_all_countries <- read.csv2(
  "51000-0032_all_countries_de.csv",
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  fill = TRUE,
  colClasses = "character"
)

exports_all_countries_long <- raw_exports_all_countries %>%
  transmute(
    name = str_trim(V1),
    export_value = str_trim(V4)
  ) %>%
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
    export_value != ""
  ) %>%
  mutate(
    year = as.integer(year),
    destination_country = name,
    export_value = clean_number_de(export_value)
  ) %>%
  filter(
    !is.na(export_value)
  ) %>%
  select(
    federal_state,
    year,
    destination_country,
    export_value
  )


possible_world_rows <- exports_all_countries_long %>%
  distinct(
    destination_country
  ) %>%
  filter(
    str_detect(
      destination_country,
      regex(
        # drop aggregate rows: Insgesamt/Gesamt = total, Welt = world, Alle Länder = all countries
        "^Insgesamt$|^Welt$|^World$|^Alle Länder$|^Gesamt$|^Total$",
        ignore_case = TRUE
      )
    )
  )

possible_world_rows


exports_all_countries_long_no_world_rows <- exports_all_countries_long %>%
  filter(
    !str_detect(
      destination_country,
      regex(
        # drop aggregate rows: Insgesamt/Gesamt = total, Welt = world, Alle Länder = all countries
        "^Insgesamt$|^Welt$|^World$|^Alle Länder$|^Gesamt$|^Total$",
        ignore_case = TRUE
      )
    )
  )


total_exports_world_controls <- exports_all_countries_long_no_world_rows %>%
  group_by(
    federal_state,
    year
  ) %>%
  summarise(
    total_exports_world = sum(
      export_value,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    federal_state,
    year
  )


total_exports_world_controls_summary <- total_exports_world_controls %>%
  summarise(
    n_obs = n(),
    n_states = n_distinct(federal_state),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    missing_total_exports_world = sum(is.na(total_exports_world))
  )

total_exports_world_controls_summary

total_exports_world_controls %>%
  count(year)

total_exports_world_controls %>%
  count(federal_state)

total_exports_world_controls %>%
  count(
    federal_state,
    year
  ) %>%
  filter(
    n > 1
  )

summary(
  total_exports_world_controls$total_exports_world
)


analysis_panel <- merge_control(
  panel = analysis_panel,
  control_data = total_exports_world_controls,
  new_variables = "total_exports_world"
)


# ============================================================
# Check updated main analysis panel after regional-control merges
# ============================================================
#
# Purpose:
#   Check whether all regional controls were merged successfully into the
#   main analysis panel.
#
# Notes:
#   Scaled _1000 treatment and IV variables are checked in the separate
#   rescaling script, not here.
# ============================================================

analysis_panel <- add_fixed_effect_identifiers(
  analysis_panel
)

analysis_panel_controls_merge_summary <- analysis_panel %>%
  summarise(
    n_obs = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    
    missing_gdp =
      sum(is.na(gdp_million_eur)),
    
    missing_population =
      sum(is.na(population)),
    
    missing_unemployment_rate =
      sum(is.na(unemployment_rate)),
    
    missing_employment =
      sum(is.na(employment_thousand_persons)),
    
    missing_manufacturing_share =
      sum(is.na(manufacturing_share)),
    
    missing_total_exports_world =
      sum(is.na(total_exports_world)),
    
    missing_fe_state_origin =
      sum(is.na(fe_state_origin)),
    
    missing_fe_state_year =
      sum(is.na(fe_state_year)),
    
    missing_fe_origin_year =
      sum(is.na(fe_origin_year))
  )

analysis_panel_controls_merge_summary


# ============================================================
# Construct active regional-control robustness panel
# ============================================================
#
# Purpose:
#   Construct the active regional-control robustness panel used to estimate
#   robustness specifications with explicit federal_state × year controls.
#
# Period:
#   2010–2024
#
# Reason:
#   Some regional controls, especially population, are not available for
#   2025. Restricting the panel to 2010–2024 ensures a complete set of
#   regional controls.
# ============================================================

analysis_panel_controls <- analysis_panel %>%
  filter(
    year <= 2024,
    !is.na(gdp_million_eur),
    !is.na(population),
    !is.na(unemployment_rate),
    !is.na(employment_thousand_persons),
    !is.na(manufacturing_share),
    !is.na(total_exports_world)
  ) %>%
  add_fixed_effect_identifiers()


analysis_panel_controls_no_eritrea <- analysis_panel_controls %>%
  filter(
    origin_country != "Eritrea"
  ) %>%
  add_fixed_effect_identifiers()


analysis_panel_controls_summary <- analysis_panel_controls %>%
  summarise(
    n_obs = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    
    missing_export_value =
      sum(is.na(export_value)),
    
    missing_gdp =
      sum(is.na(gdp_million_eur)),
    
    missing_population =
      sum(is.na(population)),
    
    missing_unemployment =
      sum(is.na(unemployment_rate)),
    
    missing_employment =
      sum(is.na(employment_thousand_persons)),
    
    missing_manufacturing_share =
      sum(is.na(manufacturing_share)),
    
    missing_total_exports_world =
      sum(is.na(total_exports_world)),
    
    missing_fe_state_origin =
      sum(is.na(fe_state_origin)),
    
    missing_fe_state_year =
      sum(is.na(fe_state_year)),
    
    missing_fe_origin_year =
      sum(is.na(fe_origin_year))
  )

analysis_panel_controls_summary


analysis_panel_controls_no_eritrea_summary <- analysis_panel_controls_no_eritrea %>%
  summarise(
    n_obs = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    
    missing_export_value =
      sum(is.na(export_value)),
    
    missing_gdp =
      sum(is.na(gdp_million_eur)),
    
    missing_population =
      sum(is.na(population)),
    
    missing_unemployment =
      sum(is.na(unemployment_rate)),
    
    missing_employment =
      sum(is.na(employment_thousand_persons)),
    
    missing_manufacturing_share =
      sum(is.na(manufacturing_share)),
    
    missing_total_exports_world =
      sum(is.na(total_exports_world)),
    
    missing_fe_state_origin =
      sum(is.na(fe_state_origin)),
    
    missing_fe_state_year =
      sum(is.na(fe_state_year)),
    
    missing_fe_origin_year =
      sum(is.na(fe_origin_year))
  )

analysis_panel_controls_no_eritrea_summary


analysis_panel_controls %>%
  count(year)

analysis_panel_controls %>%
  count(origin_country)

analysis_panel_controls %>%
  count(federal_state)

analysis_panel_controls %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )


analysis_panel_controls_no_eritrea %>%
  count(year)

analysis_panel_controls_no_eritrea %>%
  count(origin_country)

analysis_panel_controls_no_eritrea %>%
  count(federal_state)

analysis_panel_controls_no_eritrea %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )


# ============================================================
# Archived Control 7: CEPII Gravity controls
# ============================================================
#
# Data source:
#   CEPII Gravity Database
#   Gravity_V202211.csv
#
# Purpose:
#   Clean CEPII gravity controls for Germany and the five origin countries
#   and construct archived CEPII panels for transparency.
#
# Status:
#   Archived / considered but not retained.
#
# Period:
#   2010–2021
#
# Final archived gravity variables:
#   dist
#   contig
#   comlang_off
#   comlang_ethno
#   comcol
#   col45
#   fta_wto
#
# Note:
#   rta_coverage is excluded from the clean CEPII controls because it has
#   high missingness in this sample.
# ============================================================

cepii_gravity <- read.csv(
  "Gravity_V202211.csv",
  stringsAsFactors = FALSE
)

cepii_gravity_controls <- cepii_gravity %>%
  filter(
    iso3_o == "DEU",
    iso3_d %in% origin_mapping$iso3_d,
    year >= 2010,
    year <= 2021,
    country_exists_o == 1,
    country_exists_d == 1
  ) %>%
  select(
    iso3_o,
    iso3_d,
    year,
    dist,
    contig,
    comlang_off,
    comlang_ethno,
    comcol,
    col45,
    fta_wto,
    rta_coverage
  ) %>%
  distinct()


cepii_gravity_controls_summary <- cepii_gravity_controls %>%
  summarise(
    n_obs = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    missing_dist = sum(is.na(dist)),
    missing_contig = sum(is.na(contig)),
    missing_comlang_off = sum(is.na(comlang_off)),
    missing_comlang_ethno = sum(is.na(comlang_ethno)),
    missing_comcol = sum(is.na(comcol)),
    missing_col45 = sum(is.na(col45)),
    missing_fta_wto = sum(is.na(fta_wto)),
    missing_rta_coverage = sum(is.na(rta_coverage))
  )

cepii_gravity_controls_summary

cepii_gravity_controls %>%
  count(iso3_d)

cepii_gravity_controls %>%
  count(year)


cepii_gravity_controls_clean <- cepii_gravity_controls %>%
  select(
    iso3_o,
    iso3_d,
    year,
    dist,
    contig,
    comlang_off,
    comlang_ethno,
    comcol,
    col45,
    fta_wto
  )


# ============================================================
# Construct archived CEPII panels
# ============================================================
#
# Purpose:
#   Merge cleaned CEPII gravity controls into the analysis panel to document
#   the attempted CEPII / gravity-control robustness check.
#
# Period:
#   2010–2021
#
# Status:
#   Archived / not used in active final robustness package.
# ============================================================

analysis_panel_cepii <- analysis_panel %>%
  filter(
    year <= 2021
  ) %>%
  select(
    -any_of(c(
      "iso3_d",
      "iso3_o",
      "dist",
      "contig",
      "comlang_off",
      "comlang_ethno",
      "comcol",
      "col45",
      "fta_wto"
    ))
  ) %>%
  left_join(
    origin_mapping,
    by = "origin_country"
  ) %>%
  left_join(
    cepii_gravity_controls_clean,
    by = c(
      "iso3_d",
      "year"
    )
  ) %>%
  add_fixed_effect_identifiers()


analysis_panel_cepii_no_eritrea <- analysis_panel_cepii %>%
  filter(
    origin_country != "Eritrea"
  ) %>%
  add_fixed_effect_identifiers()


analysis_panel_cepii_summary <- analysis_panel_cepii %>%
  summarise(
    n_obs = n(),
    active_status = "archived / not used actively",
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    
    missing_export_value =
      sum(is.na(export_value)),
    
    missing_total_exports_world =
      sum(is.na(total_exports_world)),
    
    missing_iso3 =
      sum(is.na(iso3_d)),
    
    missing_dist =
      sum(is.na(dist)),
    
    missing_contig =
      sum(is.na(contig)),
    
    missing_comlang_off =
      sum(is.na(comlang_off)),
    
    missing_comlang_ethno =
      sum(is.na(comlang_ethno)),
    
    missing_comcol =
      sum(is.na(comcol)),
    
    missing_col45 =
      sum(is.na(col45)),
    
    missing_fta_wto =
      sum(is.na(fta_wto)),
    
    missing_fe_state_origin =
      sum(is.na(fe_state_origin)),
    
    missing_fe_state_year =
      sum(is.na(fe_state_year)),
    
    missing_fe_origin_year =
      sum(is.na(fe_origin_year))
  )

analysis_panel_cepii_summary


analysis_panel_cepii_no_eritrea_summary <- analysis_panel_cepii_no_eritrea %>%
  summarise(
    n_obs = n(),
    active_status = "archived / not used actively",
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_states = n_distinct(federal_state),
    n_origins = n_distinct(origin_country),
    
    missing_export_value =
      sum(is.na(export_value)),
    
    missing_total_exports_world =
      sum(is.na(total_exports_world)),
    
    missing_iso3 =
      sum(is.na(iso3_d)),
    
    missing_dist =
      sum(is.na(dist)),
    
    missing_contig =
      sum(is.na(contig)),
    
    missing_comlang_off =
      sum(is.na(comlang_off)),
    
    missing_comlang_ethno =
      sum(is.na(comlang_ethno)),
    
    missing_comcol =
      sum(is.na(comcol)),
    
    missing_col45 =
      sum(is.na(col45)),
    
    missing_fta_wto =
      sum(is.na(fta_wto)),
    
    missing_fe_state_origin =
      sum(is.na(fe_state_origin)),
    
    missing_fe_state_year =
      sum(is.na(fe_state_year)),
    
    missing_fe_origin_year =
      sum(is.na(fe_origin_year))
  )

analysis_panel_cepii_no_eritrea_summary


analysis_panel_cepii %>%
  count(year)

analysis_panel_cepii %>%
  count(origin_country)

analysis_panel_cepii %>%
  count(federal_state)

analysis_panel_cepii %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )


analysis_panel_cepii_no_eritrea %>%
  count(year)

analysis_panel_cepii_no_eritrea %>%
  count(origin_country)

analysis_panel_cepii_no_eritrea %>%
  count(federal_state)

analysis_panel_cepii_no_eritrea %>%
  count(
    federal_state,
    origin_country,
    year
  ) %>%
  filter(
    n > 1
  )


cepii_archive_note <- tibble(
  archived_check = "CEPII / gravity-control robustness",
  status = "considered but not retained",
  reason = paste(
    "CEPII gravity controls were cleaned and merged into analysis_panel_cepii.",
    "However, in the attempted robustness specification, the gravity variables",
    "were absorbed by the remaining fixed effects and dropped because of",
    "collinearity. Therefore, CEPII controls and analysis_panel_cepii are",
    "archived but not used in the active final robustness package."
  )
)

cepii_archive_note


# ============================================================
# Save cleaned controls and updated panels
# ============================================================
#
# Purpose:
#   Save all active regional-control datasets, updated panels, archived
#   CEPII objects, and summary diagnostics.
#
# Notes:
#   The saved analysis_panel includes regional controls.
#
#   analysis_panel_controls is the active regional-control robustness panel.
#
#   analysis_panel_controls_no_eritrea is the corresponding active
#   no-Eritrea regional-control robustness panel.
#
#   analysis_panel_cepii and analysis_panel_cepii_no_eritrea are archived
#   only.
# ============================================================

### Active regional-control objects

saveRDS(
  gdp_controls,
  "gdp_controls.rds"
)

saveRDS(
  population_controls,
  "population_controls.rds"
)

saveRDS(
  unemployment_controls,
  "unemployment_controls.rds"
)

saveRDS(
  employment_controls,
  "employment_controls.rds"
)

saveRDS(
  manufacturing_controls,
  "manufacturing_controls.rds"
)

saveRDS(
  total_exports_world_controls,
  "total_exports_world_controls.rds"
)


### Active updated panels

saveRDS(
  analysis_panel,
  "analysis_panel.rds"
)

saveRDS(
  analysis_panel_controls,
  "analysis_panel_controls.rds"
)

saveRDS(
  analysis_panel_controls_no_eritrea,
  "analysis_panel_controls_no_eritrea.rds"
)


### Archived CEPII objects

saveRDS(
  cepii_gravity_controls_clean,
  "cepii_gravity_controls_clean.rds"
)

saveRDS(
  origin_mapping,
  "origin_mapping.rds"
)

saveRDS(
  cepii_gravity,
  "cepii_gravity.rds"
)

saveRDS(
  analysis_panel_cepii,
  "analysis_panel_cepii.rds"
)

saveRDS(
  analysis_panel_cepii_no_eritrea,
  "analysis_panel_cepii_no_eritrea.rds"
)

saveRDS(
  cepii_archive_note,
  "cepii_archive_note.rds"
)


### Active summary objects

saveRDS(
  gdp_controls_summary,
  "gdp_controls_summary.rds"
)

saveRDS(
  population_controls_summary,
  "population_controls_summary.rds"
)

saveRDS(
  unemployment_controls_summary,
  "unemployment_controls_summary.rds"
)

saveRDS(
  employment_controls_summary,
  "employment_controls_summary.rds"
)

saveRDS(
  manufacturing_controls_summary,
  "manufacturing_controls_summary.rds"
)

saveRDS(
  total_exports_world_controls_summary,
  "total_exports_world_controls_summary.rds"
)

saveRDS(
  analysis_panel_controls_merge_summary,
  "analysis_panel_controls_merge_summary.rds"
)

saveRDS(
  analysis_panel_controls_summary,
  "analysis_panel_controls_summary.rds"
)

saveRDS(
  analysis_panel_controls_no_eritrea_summary,
  "analysis_panel_controls_no_eritrea_summary.rds"
)


### Archived CEPII summary objects

saveRDS(
  cepii_gravity_controls_summary,
  "cepii_gravity_controls_summary.rds"
)

saveRDS(
  analysis_panel_cepii_summary,
  "analysis_panel_cepii_summary.rds"
)

saveRDS(
  analysis_panel_cepii_no_eritrea_summary,
  "analysis_panel_cepii_no_eritrea_summary.rds"
)

saveRDS(
  missing_input_files,
  "regional_controls_missing_input_files.rds"
)

saveRDS(
  missing_analysis_panel_variables,
  "regional_controls_missing_analysis_panel_variables.rds"
)


# ============================================================
# Clean temporary objects
# ============================================================

rm(
  required_input_files,
  missing_input_files,
  required_analysis_panel_variables,
  missing_analysis_panel_variables,
  year_row,
  year_headers,
  year_cols,
  state_row,
  state_headers,
  state_cols,
  gdp_raw,
  population_raw,
  unemployment_raw,
  unemployment_step,
  employment_raw,
  employment_step,
  gva_raw,
  gva_long,
  raw_exports_all_countries,
  exports_all_countries_long,
  exports_all_countries_long_no_world_rows,
  possible_world_rows,
  cepii_gravity_controls,
  federal_states,
  clean_number_de,
  find_year_row,
  merge_control,
  add_fixed_effect_identifiers
)


# ============================================================
# Final objects kept
# ============================================================
#
# Active main and robustness panels:
#   analysis_panel
#   analysis_panel_controls
#   analysis_panel_controls_no_eritrea
#
# Active regional-control datasets:
#   gdp_controls
#   population_controls
#   unemployment_controls
#   employment_controls
#   manufacturing_controls
#   total_exports_world_controls
#
# Active regional-control summary objects:
#   gdp_controls_summary
#   population_controls_summary
#   unemployment_controls_summary
#   employment_controls_summary
#   manufacturing_controls_summary
#   total_exports_world_controls_summary
#   analysis_panel_controls_merge_summary
#   analysis_panel_controls_summary
#   analysis_panel_controls_no_eritrea_summary
#
# Archived CEPII / gravity-control objects:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#   cepii_gravity_controls_clean
#   cepii_gravity_controls_summary
#   analysis_panel_cepii_summary
#   analysis_panel_cepii_no_eritrea_summary
#   origin_mapping
#   cepii_gravity
#   cepii_archive_note
#
# Notes:
#   analysis_panel is the updated main panel with regional controls merged in.
#
#   analysis_panel_controls is the active regional-control robustness panel.
#   It is restricted to 2010–2024 and contains only observations with complete
#   regional controls.
#
#   analysis_panel_controls_no_eritrea is the corresponding active
#   regional-control robustness panel excluding Eritrea.
#
#   Treatment and IV variables scaled by 1,000 persons are created in the
#   separate rescaling script after this script has produced the active
#   regional-control panel.
#
#   The CEPII objects are retained for transparency only. They are archived
#   because the CEPII / gravity-control robustness check was considered but
#   not retained: the gravity controls were absorbed by the remaining fixed
#   effects and dropped due to collinearity.
#
#   Later regression scripts should load these saved .rds panels and control
#   objects directly rather than rebuilding them from raw CSV files.
# ============================================================