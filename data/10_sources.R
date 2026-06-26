# ============================================================
# Data sources for research proposal
# ============================================================
#
# Research topic:
#   Migrant Networks and International Trade.
#
# Script type:
#   Data-documentation script
#
# Workflow logic:
#   This script documents the data sources, cleaned objects, saved .rds
#   files, and final analysis panels used in the research proposal.
#
#   It does not clean raw data, construct panels, run regressions, or modify
#   existing .rds files.
#
# Proposed empirical setting:
#   Protection seekers in German Länder from the asylum-seeker wave
#   in 2015/16 and exports from German Länder to selected origin
#   countries.
#
# Unit of observation in final main panel:
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
# Active final panels:
#   analysis_panel
#   analysis_panel_controls
#   analysis_panel_no_eritrea
#   analysis_panel_controls_no_eritrea
#   analysis_panel_delta_endpoint
#   analysis_panel_no_eritrea_delta_endpoint
#
# Archived panels:
#   analysis_panel_cepii
#   analysis_panel_cepii_no_eritrea
#
# Note:
#   CEPII gravity controls and CEPII panels were constructed and archived for
#   transparency, but they are not part of the active final robustness package.
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
library(tibble)


# ============================================================
# Required files for documentation consistency check
# ============================================================
#
# Purpose:
#   Check whether the documented saved .rds objects exist in the working
#   directory.
#
# Notes:
#   This is only a documentation consistency check.
#
#   Missing files are documented in missing_documented_data_files, but the
#   script does not stop automatically. This allows the documentation table
#   to be produced even if some archived files are absent.
# ============================================================

documented_data_files <- c(
  "export_value_thousand_eur.rds",
  "protection_seekers_stock.rds",
  "protection_seekers_delta_2014_2017.rds",
  "protection_seekers_2014_2017_long.rds",
  "national_protection_seekers_delta_2014_2017.rds",
  "koenigstein_key.rds",
  "gdp_controls.rds",
  "population_controls.rds",
  "unemployment_controls.rds",
  "employment_controls.rds",
  "manufacturing_controls.rds",
  "total_exports_world_controls.rds",
  "cepii_gravity_controls_clean.rds",
  "origin_mapping.rds",
  "analysis_panel.rds",
  "analysis_panel_controls.rds",
  "analysis_panel_no_eritrea.rds",
  "analysis_panel_controls_no_eritrea.rds",
  "analysis_panel_delta_endpoint.rds",
  "analysis_panel_no_eritrea_delta_endpoint.rds",
  "analysis_panel_cepii.rds",
  "analysis_panel_cepii_no_eritrea.rds"
)

missing_documented_data_files <- tibble(
  saved_file = documented_data_files,
  file_exists = file.exists(documented_data_files)
) %>%
  filter(
    !file_exists
  )

missing_documented_data_files


# ============================================================
# 1. Outcome data: German federal-state exports
# ============================================================
#
# Dataset:
#   GENESIS / Destatis foreign trade data
#
# Table:
#   51000-0032
#
# German table title:
#   Aus- und Einfuhr (Außenhandel): Bundesländer, Jahre, Länder
#   (English: Exports and imports (foreign trade): federal states, years, countries)
#
# Raw file:
#   51000-0032_de.csv
#
# Cleaned object:
#   export_value_thousand_eur
#
# Saved file:
#   export_value_thousand_eur.rds
#
# Unit after cleaning:
#   federal_state × origin_country × year
#
# Main variables:
#   export_value
#   log_export_value
#   export_weight
#
# Use:
#   Outcome data for the main analysis panel.
#
# Notes:
#   export_value is measured in thousand EUR.
#   log_export_value is constructed as log(export_value + 1).
# ============================================================


# ============================================================
# 2. Treatment data: protection seekers
# ============================================================
#
# Dataset:
#   GENESIS / Destatis protection-seeker statistics
#
# Table:
#   12531-0024
#
# German table title:
#   Schutzsuchende: Bundesländer, Stichtag, Geschlecht,
#   Familienstand, Ländergruppierungen/Staatsangehörigkeit
#   (English: Protection seekers: federal states, reference date, sex, marital status, country groupings / nationality)
#
# Raw file:
#   12531-0024_de.csv
#
# Cleaned object:
#   protection_seekers_stock
#
# Saved file:
#   protection_seekers_stock.rds
#
# Unit after cleaning:
#   federal_state × origin_country
#
# Main variables:
#   protection_seekers_stock_2014
#   protection_seekers_stock_2016
#   delta_protection_seekers_2014_2016
#
# Use:
#   Treatment exposure by Land and origin country.
#
# Notes:
#   protection_seekers_stock_2016 is the main treatment variable.
#   delta_protection_seekers_2014_2016 is used as an alternative treatment
#   measure for robustness.
#
#   Treatment variables are measured in persons. Regression-ready variables
#   scaled by 1,000 persons are constructed in the analysis panel.
# ============================================================


# ============================================================
# 2b. Delta-endpoint treatment data: 2014–2017 protection-seeker change
# ============================================================
#
# Dataset:
#   GENESIS / Destatis protection-seeker statistics
#
# Table:
#   12531-0024
#
# German table title:
#   Schutzsuchende: Bundesländer, Stichtag, Geschlecht,
#   Familienstand, Ländergruppierungen/Staatsangehörigkeit
#   (English: Protection seekers: federal states, reference date, sex, marital status, country groupings / nationality)
#
# Raw file:
#   12531-0024_de_2014_2017.csv
#
# Cleaned objects:
#   protection_seekers_2014_2017_long
#   protection_seekers_delta_2014_2017
#   national_protection_seekers_delta_2014_2017
#
# Saved files:
#   protection_seekers_2014_2017_long.rds
#   protection_seekers_delta_2014_2017.rds
#   national_protection_seekers_delta_2014_2017.rds
#
# Unit after cleaning:
#   protection_seekers_2014_2017_long:
#     federal_state × origin_country × year
#
#   protection_seekers_delta_2014_2017:
#     federal_state × origin_country
#
# Main variables:
#   protection_seekers_stock_2014
#   protection_seekers_stock_2017
#   delta_protection_seekers_2014_2017
#
# Use:
#   Alternative treatment exposure for the delta-endpoints robustness check.
#
# Constructed panel variables:
#   treatment_delta_2014_2017_post
#   iv_delta_2014_2017_post
#   treatment_delta_2014_2017_post_1000
#   iv_delta_2014_2017_post_1000
#
# Notes:
#   The 2014–2017 exposure window allows for delayed adjustment after the
#   initial 2015/16 refugee inflow.
#
#   It is used only as a robustness check and should not replace the main
#   2014–2016 exposure definition because 2017 is further removed from the
#   initial allocation shock and may be more affected by secondary mobility
#   or endogenous location choices.
# ============================================================


# ============================================================
# 3. Instrument data: Königsteiner Schlüssel
# ============================================================
#
# Dataset:
#   Königsteiner Schlüssel
#
# Source institution:
#   Gemeinsame Wissenschaftskonferenz (GWK)
#
# Source page:
#   https://www.gwk-bonn.de/themen/finanzierung-von-wissenschaft-und-forschung/koenigsteiner-schluessel
#
# PDF source used:
#   Koenigsteiner_Schluessel_fuer_2010_-_2020.pdf
#
# Raw file:
#   koenigsteiner_schluessel_2014_2016.csv
#
# Cleaned object:
#   koenigstein_key
#
# Saved file:
#   koenigstein_key.rds
#
# Unit after cleaning:
#   federal_state
#
# Main variables:
#   koenigstein_share_2014
#   koenigstein_share_2015
#   koenigstein_share_2016
#   koenigstein_share_2015_2016_avg
#   koenigstein_share_2014_2015_2016_avg
#
# Use:
#   Instrument basis for predicted protection-seeker exposure.
#
# Main IV basis:
#   koenigstein_share_2015_2016_avg
#
# Robustness IV bases:
#   koenigstein_share_2014
#   koenigstein_share_2014_2015_2016_avg
#
# Constructed main instrument variables in analysis_panel:
#   predicted_protection_seekers_stock_2016
#   predicted_delta_protection_seekers_2014_2016
#   iv_stock_2016_post
#   iv_delta_post
#   iv_stock_2016_post_1000
#   iv_delta_post_1000
#
# Constructed robustness instrument variables in analysis_panel:
#   predicted_protection_seekers_stock_2016_k14
#   predicted_delta_protection_seekers_2014_2016_k14
#   iv_stock_2016_post_k14
#   iv_delta_post_k14
#   iv_stock_2016_post_k14_1000
#   iv_delta_post_k14_1000
#
#   predicted_protection_seekers_stock_2016_k141516
#   predicted_delta_protection_seekers_2014_2016_k141516
#   iv_stock_2016_post_k141516
#   iv_delta_post_k141516
#   iv_stock_2016_post_k141516_1000
#   iv_delta_post_k141516_1000
#
# Notes:
#   The preferred instrument uses the average allocation share over 2015
#   and 2016, matching the allocation rules relevant for the refugee-wave
#   cohort.
#
#   The 2014 key and the 2014–2016 three-year average are used only as
#   robustness checks.
# ============================================================


# ============================================================
# 4. Regional control: GDP
# ============================================================
#
# Dataset:
#   GENESIS / VGR der Länder
#
# Table:
#   82111-0010
#
# German table title:
#   Bruttoinlandsprodukt zu Marktpreisen, nominal:
#   (English: Gross domestic product at market prices, nominal: federal states, years)
#   Bundesländer, Jahre
#
# Raw file:
#   82111-0010_de.csv
#
# Cleaned object:
#   gdp_controls
#
# Saved file:
#   gdp_controls.rds
#
# Unit after cleaning:
#   federal_state × year
#
# Main variable:
#   gdp_million_eur
#
# Use:
#   Regional-control robustness specification.
#
# Notes:
#   Not included in the preferred main specification because it is absorbed
#   by federal_state × year fixed effects.
# ============================================================


# ============================================================
# 5. Regional control: population
# ============================================================
#
# Dataset:
#   GENESIS / Destatis population statistics
#
# Table:
#   12411-0010
#
# German table title:
#   Bevölkerung: Bundesländer, Stichtag
#   (English: Population: federal states, reference date)
#
# Raw file:
#   12411-0010_de.csv
#
# Cleaned object:
#   population_controls
#
# Saved file:
#   population_controls.rds
#
# Unit after cleaning:
#   federal_state × year
#
# Main variable:
#   population
#
# Use:
#   Regional-control robustness specification.
#
# Notes:
#   Population is one reason why the regional-control robustness panel is
#   restricted to the common control sample ending in 2024.
# ============================================================


# ============================================================
# 6. Regional control: unemployment rate
# ============================================================
#
# Dataset:
#   GENESIS / labour market statistics
#
# Table:
#   13211-0007
#
# Raw file:
#   13211-0007_de.csv
#
# Cleaned object:
#   unemployment_controls
#
# Saved file:
#   unemployment_controls.rds
#
# Unit after cleaning:
#   federal_state × year
#
# Main variable:
#   unemployment_rate
#
# Use:
#   Regional-control robustness specification.
#
# Notes:
#   Not included in the preferred main specification because it is absorbed
#   by federal_state × year fixed effects.
# ============================================================


# ============================================================
# 7. Regional control: employment
# ============================================================
#
# Dataset:
#   GENESIS / employment statistics
#
# Table:
#   13311-0002
#
# German table title:
#   Erwerbstätige, Arbeitnehmer, Selbständige
#   (English: Employed persons, employees, self-employed: federal states, years)
#
# Raw file:
#   13311-0002_de.csv
#
# Cleaned object:
#   employment_controls
#
# Saved file:
#   employment_controls.rds
#
# Unit after cleaning:
#   federal_state × year
#
# Main variable:
#   employment_thousand_persons
#
# Use:
#   Regional-control robustness specification.
#
# Notes:
#   Employment is measured in thousand persons.
# ============================================================


# ============================================================
# 8. Regional control: manufacturing share
# ============================================================
#
# Dataset:
#   GENESIS / VGR der Länder
#
# Table:
#   82111-0011
#
# German table title:
#   Bruttowertschöpfung nach Wirtschaftsbereichen:
#   Bundesländer, Jahre
#
# Raw file:
#   82111-0011_de.csv
#
# Cleaned object:
#   manufacturing_controls
#
# Saved file:
#   manufacturing_controls.rds
#
# Unit after cleaning:
#   federal_state × year
#
# Main variables:
#   gva_total
#   gva_manufacturing
#   manufacturing_share
#
# Construction:
#   gva_total = WZ08-A + WZ08-B-F + WZ08-G-T
#   manufacturing_share = gva_manufacturing / gva_total
#
# Use:
#   Regional-control robustness specification.
#
# Notes:
#   Not included in the preferred main specification because it is absorbed
#   by federal_state × year fixed effects.
# ============================================================


# ============================================================
# 9. Regional control: total exports to the world
# ============================================================
#
# Dataset:
#   GENESIS / Destatis foreign trade data
#
# Table:
#   51000-0032
#
# German table title:
#   Aus- und Einfuhr (Außenhandel): Bundesländer, Jahre, Länder
#   (English: Exports and imports (foreign trade): federal states, years, countries)
#
# Raw file:
#   51000-0032_all_countries_de.csv
#
# Cleaned object:
#   total_exports_world_controls
#
# Saved file:
#   total_exports_world_controls.rds
#
# Unit after cleaning:
#   federal_state × year
#
# Main variable:
#   total_exports_world
#
# Construction:
#   total_exports_world is constructed by summing export values across all
#   destination countries for each federal_state × year.
#
# Use:
#   Regional-control robustness specification.
#
# Notes:
#   Aggregate rows such as "Insgesamt" (total), "Welt" (world), "Alle Länder"
#   (all countries) or equivalent total rows should be excluded before summing
#   if present in the raw file,
#   to avoid double counting.
#
#   Not included in the preferred main specification because it is absorbed
#   by federal_state × year fixed effects.
# ============================================================


# ============================================================
# 10. Archived gravity controls: CEPII Gravity Database
# ============================================================
#
# Dataset:
#   CEPII Gravity Database
#
# Citation:
#   Conte, M., Cotterlaz, P., and Mayer, T. (2022).
#   The CEPII Gravity Database.
#   CEPII Working Paper No. 2022-05.
#
# Source page:
#   http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
#
# Raw file:
#   Gravity_V202211.csv
#
# Raw object:
#   cepii_gravity
#
# Cleaned object:
#   cepii_gravity_controls_clean
#
# Saved files:
#   cepii_gravity_controls_clean.rds
#   cepii_gravity.rds
#
# Unit after cleaning:
#   iso3_o × iso3_d × year
#
# Relevant exporter:
#   Germany / DEU
#
# Relevant destination countries:
#   Afghanistan / AFG
#   Eritrea / ERI
#   Irak (Iraq) / IRQ
#   Iran / IRN
#   Syrien (Syria) / SYR
#
# Period:
#   2010–2021
#
# Main variables:
#   dist
#   contig
#   comlang_off
#   comlang_ethno
#   comcol
#   col45
#   fta_wto
#
# Excluded variable:
#   rta_coverage
#
# Reason for exclusion:
#   High missingness in the selected sample.
#
# Status:
#   Archived / considered but not retained.
#
# Use:
#   Not used in the active final robustness package.
#
# Reason:
#   A CEPII / gravity-control robustness check was considered. However, in
#   the attempted fixed-effect specification, the CEPII gravity controls were
#   absorbed by the remaining fixed effects and dropped due to collinearity.
#   Therefore, CEPII controls are retained for transparency but not used as an
#   active robustness specification.
#
# Notes:
#   CEPII controls are not used in the preferred main specification because
#   time-invariant gravity variables are absorbed by federal_state ×
#   origin_country fixed effects, and origin-level time-varying variables are
#   absorbed by origin_country × year fixed effects.
# ============================================================


# ============================================================
# 11. Archived country-name mapping for CEPII merge
# ============================================================
#
# Object:
#   origin_mapping
#
# Saved file:
#   origin_mapping.rds
#
# Purpose:
#   Maps the origin_country names used in the GENESIS/Destatis data to
#   the ISO3 destination-country codes used in the CEPII Gravity Database.
#
# Mapping:
#   Afghanistan                  -> AFG
#   Eritrea                      -> ERI
#   Irak (Iraq)                         -> IRQ
#   Iran, Islamische Republik (Iran, Islamic Republic)    -> IRN
#   Syrien (Syria)                       -> SYR
#
# Status:
#   Archived / not used in active final robustness package.
#
# Use:
#   Required only for the archived CEPII merge into analysis_panel_cepii.
# ============================================================


# ============================================================
# Data sources overview
# ============================================================
#
# Purpose:
#   Summarise all active and archived data sources used in the project.
# ============================================================

data_sources_overview <- tibble(
  category = c(
    "Outcome",
    "Treatment",
    "Treatment robustness",
    "Instrument",
    "Regional control",
    "Regional control",
    "Regional control",
    "Regional control",
    "Regional control",
    "Regional control",
    "Archived gravity control",
    "Archived mapping"
  ),
  source = c(
    "GENESIS / Destatis foreign trade",
    "GENESIS / Destatis protection-seeker statistics",
    "GENESIS / Destatis protection-seeker statistics",
    "GWK Königsteiner Schlüssel",
    "GENESIS / VGR der Länder",
    "GENESIS / Destatis population statistics",
    "GENESIS / labour market statistics",
    "GENESIS / employment statistics",
    "GENESIS / VGR der Länder",
    "GENESIS / Destatis foreign trade",
    "CEPII Gravity Database",
    "Constructed manually"
  ),
  table_or_file = c(
    "51000-0032",
    "12531-0024",
    "12531-0024",
    "koenigsteiner_schluessel_2014_2016.csv",
    "82111-0010",
    "12411-0010",
    "13211-0007",
    "13311-0002",
    "82111-0011",
    "51000-0032_all_countries_de.csv",
    "Gravity_V202211.csv",
    "origin_mapping"
  ),
  raw_file = c(
    "51000-0032_de.csv",
    "12531-0024_de.csv",
    "12531-0024_de_2014_2017.csv",
    "koenigsteiner_schluessel_2014_2016.csv",
    "82111-0010_de.csv",
    "12411-0010_de.csv",
    "13211-0007_de.csv",
    "13311-0002_de.csv",
    "82111-0011_de.csv",
    "51000-0032_all_countries_de.csv",
    "Gravity_V202211.csv",
    NA_character_
  ),
  cleaned_object = c(
    "export_value_thousand_eur",
    "protection_seekers_stock",
    "protection_seekers_delta_2014_2017",
    "koenigstein_key",
    "gdp_controls",
    "population_controls",
    "unemployment_controls",
    "employment_controls",
    "manufacturing_controls",
    "total_exports_world_controls",
    "cepii_gravity_controls_clean",
    "origin_mapping"
  ),
  saved_file = c(
    "export_value_thousand_eur.rds",
    "protection_seekers_stock.rds",
    "protection_seekers_delta_2014_2017.rds",
    "koenigstein_key.rds",
    "gdp_controls.rds",
    "population_controls.rds",
    "unemployment_controls.rds",
    "employment_controls.rds",
    "manufacturing_controls.rds",
    "total_exports_world_controls.rds",
    "cepii_gravity_controls_clean.rds",
    "origin_mapping.rds"
  ),
  unit_after_cleaning = c(
    "federal_state × origin_country × year",
    "federal_state × origin_country",
    "federal_state × origin_country",
    "federal_state",
    "federal_state × year",
    "federal_state × year",
    "federal_state × year",
    "federal_state × year",
    "federal_state × year",
    "federal_state × year",
    "iso3_o × iso3_d × year",
    "origin_country"
  ),
  final_use = c(
    "Outcome variables",
    "Main treatment variables",
    "Delta-endpoints robustness treatment",
    "Instrument construction",
    "Regional-control robustness",
    "Regional-control robustness",
    "Regional-control robustness",
    "Regional-control robustness",
    "Regional-control robustness",
    "Regional-control robustness",
    "Archived only; considered but not retained",
    "Archived only; CEPII merge"
  ),
  active_status = c(
    "active",
    "active",
    "active robustness",
    "active",
    "active",
    "active",
    "active",
    "active",
    "active",
    "active",
    "archived / not used actively",
    "archived / not used actively"
  )
) %>%
  mutate(
    saved_file_exists = file.exists(saved_file)
  )

data_sources_overview


# ============================================================
# Final data objects overview
# ============================================================
#
# Purpose:
#   Summarise the final active and archived panel objects used in the project.
# ============================================================

final_data_objects_overview <- tibble(
  object = c(
    "analysis_panel",
    "analysis_panel_controls",
    "analysis_panel_no_eritrea",
    "analysis_panel_controls_no_eritrea",
    "analysis_panel_delta_endpoint",
    "analysis_panel_no_eritrea_delta_endpoint",
    "analysis_panel_cepii",
    "analysis_panel_cepii_no_eritrea"
  ),
  saved_file = c(
    "analysis_panel.rds",
    "analysis_panel_controls.rds",
    "analysis_panel_no_eritrea.rds",
    "analysis_panel_controls_no_eritrea.rds",
    "analysis_panel_delta_endpoint.rds",
    "analysis_panel_no_eritrea_delta_endpoint.rds",
    "analysis_panel_cepii.rds",
    "analysis_panel_cepii_no_eritrea.rds"
  ),
  period = c(
    "2010–2025",
    "2010–2024",
    "2010–2025",
    "2010–2024",
    "2010–2025",
    "2010–2025",
    "2010–2021",
    "2010–2021"
  ),
  unit_of_observation = c(
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year",
    "federal_state × origin_country × year"
  ),
  empirical_use = c(
    "Preferred main specification",
    "Regional-control robustness",
    "Main specification excluding Eritrea",
    "Regional-control robustness excluding Eritrea",
    "Delta-endpoints robustness",
    "Delta-endpoints robustness excluding Eritrea",
    "Archived CEPII panel; not used in active robustness package",
    "Archived CEPII panel excluding Eritrea; not used in active robustness package"
  ),
  active_status = c(
    "active",
    "active",
    "active",
    "active",
    "active robustness",
    "active robustness",
    "archived / not used actively",
    "archived / not used actively"
  )
) %>%
  mutate(
    saved_file_exists = file.exists(saved_file)
  )

final_data_objects_overview


# ============================================================
# Documentation consistency summary
# ============================================================
#
# Purpose:
#   Summarise whether documented cleaned objects and final panel objects are
#   present as saved .rds files.
#
# Notes:
#   This is useful before writing the final empirical section because it
#   confirms whether the documented files exist in the current working
#   directory.
# ============================================================

data_documentation_consistency_summary <- tibble(
  category = c(
    "documented_data_files",
    "data_sources_overview_saved_files",
    "final_data_objects_overview_saved_files"
  ),
  n_files = c(
    length(documented_data_files),
    nrow(data_sources_overview),
    nrow(final_data_objects_overview)
  ),
  n_existing_files = c(
    sum(file.exists(documented_data_files)),
    sum(data_sources_overview$saved_file_exists),
    sum(final_data_objects_overview$saved_file_exists)
  ),
  n_missing_files = c(
    sum(!file.exists(documented_data_files)),
    sum(!data_sources_overview$saved_file_exists),
    sum(!final_data_objects_overview$saved_file_exists)
  )
)

data_documentation_consistency_summary


# ============================================================
# Save data source and final-object overviews
# ============================================================

saveRDS(
  data_sources_overview,
  "data_sources_overview.rds"
)

saveRDS(
  final_data_objects_overview,
  "final_data_objects_overview.rds"
)

saveRDS(
  documented_data_files,
  "documented_data_files.rds"
)

saveRDS(
  missing_documented_data_files,
  "missing_documented_data_files.rds"
)

saveRDS(
  data_documentation_consistency_summary,
  "data_documentation_consistency_summary.rds"
)


# ============================================================
# Clean temporary objects
# ============================================================

rm(
  documented_data_files
)


# ============================================================
# Final objects kept
# ============================================================
#
# Data-source documentation:
#   data_sources_overview
#
# Final data-object documentation:
#   final_data_objects_overview
#
# Documentation checks:
#   missing_documented_data_files
#   data_documentation_consistency_summary
#
# Notes:
#   data_sources_overview documents all active data sources used in the final
#   empirical analysis, plus archived CEPII objects retained for transparency.
#
#   final_data_objects_overview distinguishes active panels from archived
#   CEPII panels.
#
#   It also documents the active delta-endpoints robustness panels using the
#   2014–2017 exposure window.
#
#   missing_documented_data_files documents which listed saved files are not
#   present in the current working directory.
#
#   data_documentation_consistency_summary gives a compact overview of how
#   many documented saved files exist.
#
#   Active final panels:
#     analysis_panel
#     analysis_panel_controls
#     analysis_panel_no_eritrea
#     analysis_panel_controls_no_eritrea
#     analysis_panel_delta_endpoint
#     analysis_panel_no_eritrea_delta_endpoint
#
#   Archived panels:
#     analysis_panel_cepii
#     analysis_panel_cepii_no_eritrea
#
#   CEPII is not part of the active final robustness package because the
#   CEPII gravity controls were absorbed by the remaining fixed effects and
#   dropped due to collinearity in the attempted specification.
#
#   This script is a documentation script. It does not clean raw data,
#   construct panels, run regressions, or modify existing panel objects.
# ============================================================