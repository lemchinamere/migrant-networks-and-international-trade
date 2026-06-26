# Migrant Networks and International Trade

Replication code for the term paper *Migrant Networks and International Trade*
(L. E. M. Chinamere, LMU Munich, seminar *The Causes and Consequences of Migration*,
Summer Term 2026). The empirical part studies whether the inflow of protection seekers
into the German **Länder** during the 2015/16 asylum-seeker wave raised subsequent exports
from those Länder to the migrants' origin countries.

---

## 1. Research question

> **Did the 2015/16 refugee inflow into Germany raise Länder exports to the main origin
> countries of protection seekers?**

When migrants settle in a region they can lower trade costs to their origin country
through information, preferences, and contract-enforcement channels — the standard
"migrant networks and trade" mechanism (Gould 1994; Rauch & Trindade 2002). The paper
critically assesses two studies of this mechanism (Parsons & Vézina 2018 on the Vietnamese
Boat People; Orefice, Rapoport & Santoni 2025 on the migration–export channel) and then
develops its own research proposal that tests the mechanism in a German subnational setting
using the large, plausibly exogenous 2015/16 refugee inflow.

- **Treatment:** the actual stock of protection seekers in a Land from a given origin
  country after the shock (`protection_seekers_stock_2016`).
- **Outcome:** export value from the Land to that origin country (thousand EUR).
- **Identification problem:** migrants do not settle randomly — they tend to sort toward
  regions already internationally integrated, so the raw exposure–export correlation is
  confounded.
- **Empirical answer:** a **shift-share instrument** built on a *spatial dispersal* rule.
  Germany allocates asylum seekers across the sixteen Länder by the *Königsteiner Schlüssel*,
  an administrative formula based on general state characteristics (tax revenue and
  population) rather than origin-specific economic ties. Interacting national inflows per
  origin country with each Land's Königstein share yields **predicted exposure** that is
  orthogonal to Land-specific trade trends (GWK 2020).

### Design summary

| Element | Variable |
|---|---|
| Unit of observation | `Land × origin_country × year` (coded `federal_state × origin_country × year`) |
| Outcome | `export_value` (export value, thousand EUR) |
| Treatment | `treatment_stock_2016_post` = stock₂₀₁₆ × post-period |
| Instrument | `iv_stock_2016_post` = predicted stock₂₀₁₆ × post-period |
| IV basis | `koenigstein_share_2015_2016_avg` (Königstein share averaged over 2015–2016) |
| Origin countries | Afghanistan, Eritrea, Iraq, Iran, Syria |
| Period | 2010–2025 (pre 2010–2014 · shock 2015–2016 · post 2017–2025) |
| Fixed effects | Land×origin, Land×year, origin×year |
| Preferred estimator | PPML (with linear 2SLS and control-function IV-style PPML as robustness) |

The headline result is an **informative null**: predicted refugee exposure does not
detectably raise exports over this period. The pipeline is built around documenting and
stress-testing that null (first-stage relevance, pre-trend / BHJ check, event study,
leave-one-origin-out, COVID exclusion, alternative IV bases, regional controls).

---

## 2. Data sources

All raw inputs live in [`data/`](data/) as `.csv` / `.pdf`. The main panel is built
entirely from **public German official statistics (GENESIS / Destatis)** plus one
administrative allocation key.

### Core variables

| Source | Raw file | Used for | Built by |
|---|---|---|---|
| GENESIS **51000-0032** — Foreign trade by Land, year, country | `51000-0032_de.csv` | Export outcome | `01_outcome.R` |
| GENESIS **12531-0024** — Protection seekers by Land and nationality | `12531-0024_de.csv`, `12531-0024_de_2014_2017.csv` | Treatment (exposure) | `02_treatment.R` |
| **Königsteiner Schlüssel** (GWK allocation key) | `koenigsteiner_schluessel_2014_2016.csv`, `Koenigsteiner_Schluessel_fuer_2010_-_2020.pdf` | Instrument (shares) | `03_instrument.R` |

### Regional controls (`federal_state × year`, robustness)

| Source | Raw file | Control |
|---|---|---|
| GENESIS 82111-0010 | `82111-0010_de.csv` | GDP |
| GENESIS 12411-0010 | `12411-0010_de.csv` | Population |
| GENESIS 13211-0007 | `13211-0007_de.csv` | Unemployment rate |
| GENESIS 13311-0002 | `13311-0002_de.csv` | Employment |
| GENESIS 82111-0011 | `82111-0011_de.csv` | Manufacturing share |
| GENESIS 51000-0032 | `51000-0032_all_countries_de.csv` | Total state exports to the world |

### Archived / not in active strategy

`Gravity_V202211.csv` (CEPII Gravity database, ~1.2 GB) was used to build CEPII
gravity-control panels. These are **archived for transparency only** — the gravity controls
are absorbed by the fixed effects and drop out for collinearity. See
`99_archived_cepii_gravity_robustness.R`.

> **Not included in this repository.** Because of its size and licensing, the CEPII file is
> not redistributed here. To run the archived gravity robustness, download
> `Gravity_V202211.csv` from the [CEPII Gravity database](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8)
> and place it in `data/`. The active results do **not** require it.

### Data licensing and attribution

- **Destatis / GENESIS-Online** tables (`51000-0032`, `12531-0024`, `82111-0010/0011`,
  `12411-0010`, `13211-0007`, `13311-0002`): © Statistisches Bundesamt (Destatis),
  reproduced under the [Data licence Germany – attribution – 2.0](https://www.govdata.de/dl-de/by-2-0).
- **Königsteiner Schlüssel**: published by the Gemeinsame Wissenschaftskonferenz (GWK).
- **CEPII Gravity database**: see CEPII's terms at the link above (not redistributed here).

Please cite the original providers when reusing the data. The R code is released under the
MIT License (see [`LICENSE`](LICENSE)).

---

## 3. How to reproduce

### Requirements

- **R** (the analysis was run on **R 4.5.1**)
- R packages (versions used in parentheses):

  ```r
  install.packages(c(
    "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble",
    "fixest",   # PPML / fixed-effects estimation
    "broom",    # tidy model output
    "ggplot2"   # figures
  ))
  ```

  | Package | Version | | Package | Version |
  |---|---|---|---|---|
  | dplyr | 1.1.4 | | purrr | 1.2.0 |
  | tidyr | 1.3.1 | | tibble | 3.3.0 |
  | readr | 2.2.0 | | fixest | 0.14.1 |
  | stringr | 1.5.2 | | broom | 1.0.10 |
  | ggplot2 | 4.0.0 | | | |

  Results — especially standard errors — can depend on the `fixest` version; pin it if
  exact reproduction matters.

### Paths

Each script sets its own working directory to the `data/` folder with a portable check:

```r
if (basename(getwd()) != "data" && dir.exists("data")) setwd("data")
```

So you can run the scripts either from the repository root or from inside `data/` — no
machine-specific paths to edit. All scripts read and write relative to `data/`.

### Pipeline

The scripts are **numbered in execution order**. Run them in sequence from the `data/`
directory. Each cleaning script reads raw CSVs and saves intermediate `.rds` objects;
later scripts load those `.rds` files rather than rebuilding from raw data.

```r
# from R, with working directory = .../sps/data
for (f in sort(list.files(pattern = "^[0-9].*\\.R$"))) source(f)
```

or one by one, in this order:

**A · Build the data (01–10)**

| Script | Does |
|---|---|
| `01_outcome.R` | Clean export outcome from GENESIS 51000-0032 |
| `02_treatment.R` | Clean protection-seeker treatment from 12531-0024 |
| `03_instrument.R` | Clean Königstein allocation shares |
| `04_analysis.R` | Merge outcome + treatment + instrument → **main analysis panel** |
| `04b_create_no_eritrea_objects.R` | No-Eritrea robustness objects |
| `05_controls.R` | Build & merge `state×year` regional controls |
| `06_rescaling.R` | Regression-ready variables scaled per 1,000 persons (`*_1000`) |
| `07_fixed_effects.R` | Construct fixed-effect identifiers |
| `08_delta_endpoint_variables.R` | Alternative 2014–2017 delta-endpoint exposure |
| `09_data_structure.R` | Final panel checks, missingness, structure summaries |
| `10_sources.R` | Document data sources & saved objects (no data changes) |

> The canonical run order is also documented in-code in `22_regression_structure.R`.

**B · Estimate models (11–22)**

| Script | Specification |
|---|---|
| `11_first_stage_relevance.R` | First-stage / IV strength |
| `12_ppml_reduced_form.R` | PPML reduced form (Y on Z) |
| `13_ppml_benchmark.R` | Non-instrumented PPML benchmark |
| `14_linear_reduced_form_iv.R` | Linear reduced form + linear IV / 2SLS |
| `15_control_function_iv_ppml.R` | Control-function IV-style PPML |
| `16_pretrend_bhj_check.R` | Borusyak–Hull–Jaravel pre-trend / shock-orthogonality check |
| `17_event_study.R` | Dynamic event-study reduced form |
| `18_regional_control_robustness.R` | Add regional controls |
| `19_covid_exclusion_robustness.R` | Drop COVID years |
| `20_delta_endpoints_robustness.R` | Delta-endpoint exposure |
| `21_leave_one_origin_out_robustness.R` | Drop each origin country in turn |
| `22_regression_structure.R` | Documents final regression structure (no estimation) |

**C · Tables, figures & documentation (23–29)**

| Script | Output |
|---|---|
| `23_table_main_results.R` | Main Table 1 (`.tex` / `.csv` / `.rds`) |
| `24_table_main_diagnostics_and_robustness.R` | Main diagnostics / robustness table |
| `25_figure_event_study_main.R` | Main event-study figure |
| `26_table_appendix_robustness.R` | Appendix Table A1 |
| `27_table_leave_one_origin_out.R` | Appendix Table A2 |
| `28_figure_appendix_event_study_main.R` | Appendix event-study (PPML) figure |
| `29_results_documentation.R` | Output inventory & consistency checks |

`99_archived_cepii_gravity_robustness.R` is optional and not part of the active results.

### Outputs

Results are written back into `data/`:

- **Tables** — `main_table_1_results.{tex,csv,rds}`,
  `main_table_2_diagnostics_robustness.csv`, `appendix_table_a1_robustness.{tex,csv,rds}`,
  `appendix_table_a2_leave_one_origin_out.{tex,csv,rds}`
- **Figures** — `appendix_figure_a1_ppml_event_study.{pdf,png}` and event-study `.csv`/`.rds` data
- **Documentation** — `results_*` and `data_*` overview `.csv`/`.rds` summaries

---

## 4. Repository layout

```
sps/
├── README.md                  ← this file
├── LICENSE                    ← MIT (code); data attribution notes
├── .gitignore                 ← excludes intermediate .rds, the CEPII file, personal/3rd-party files
├── data/                      ← R scripts + raw CSVs + final tables/figures
│   ├── 01..29_*.R             ← numbered pipeline (run in order)
│   ├── 99_archived_*.R        ← optional archived robustness
│   ├── *_de.csv               ← raw GENESIS / Destatis inputs (committed)
│   ├── main_table_*, appendix_*, *figure*  ← final outputs (.tex/.csv/.png, committed)
│   ├── *.rds                  ← intermediate objects (git-ignored; regenerated by the pipeline)
│   └── Gravity_V202211.csv    ← CEPII, ~1.2 GB (git-ignored; download separately)
└── Chinamere_*.pdf / *.pptx   ← the paper and slides
```

Intermediate `.rds` objects, the large CEPII CSV, personal documents, and third-party
copyrighted PDFs are excluded via [`.gitignore`](.gitignore) and are not part of the
published repository. Running the pipeline regenerates everything under `data/`.
