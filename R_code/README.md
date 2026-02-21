# R_code
 
 This folder contains R scripts and Quarto notebooks used for:
 
 - Generating curated iNaturalist-derived datasets (`final_results`, `norm_results`) by combining community IDs, manual labels/corrections, and ML predictions
 - Producing the figures/panels used in the project (maps, seasonal plots, confusion matrices, alluvial plots)
 - Generating population-genomic figure panels (PCA / admixture) via Quarto (`.qmd`)
 
 ## Working directory expectations
 
 Most scripts assume you run them from the **repository root** (i.e., `Physalia_spatiotemporal/`), because they reference paths like:
 
 - `source("R_code/read.data.R")`
 - `data/...`
 - `results/...`
 - `figures/...`
 
 Two scripts (`iNat_plot.r`, `map_problems.r`) currently hard-code `setwd("~/Downloads/Physalia_spatiotemporal/")`. If you run elsewhere, either:
 
 - Update the `setwd(...)` line for your machine, or
 - Start R in the repo root so relative paths resolve.
 
 ## Shared inputs produced/consumed by scripts
 
 - **Primary iNat download(s)**: `data/observations-*.csv`
 - **Manual labels**: `data/labels.tsv`
 - **Manual correction passes**:
   - `data/corrected.tsv`
   - `data/corrected_final.tsv`
 - **Model predictions**: `results/all_predictions.json`
 - **Outputs**: most figure scripts write PDFs into `figures/panels/`
 
 ## File-by-file guide
 
 ### `read.data.R`
 **Role**: Central data assembly script. Loads packages, reads raw iNat data + labeling/correction/prediction artifacts, and builds the core tables used everywhere.
 
 **Key objects created**:
 
 - `inat_results`: raw iNat export from `data/observations-604981.csv`
 - `norm_results`: “effort/baseline” iNat export created by binding `data/observations-607577.csv` and `data/observations-607598.csv` (tagged `species = "beach_sp"`)
 - `labeled_data`, `corrected_data_1`, `corrected_data_2`: manual review tables
 - `learned_data` / `predicted_data`: ML predictions read from `results/all_predictions.json` and filtered by `confidence_threshold`
 - `community_data`: non-generic community IDs (anything where `scientific_name != "Physalia"`)
 - `inr_0 ... inr_5`: staged versions of the classification table
 - `final_results`: final merged dataset (`id`, `species`, `status` plus iNat columns), with derived `yd`, `ymd`, `mon`, `year`
 
 **Notes**:
 
 - `confidence_threshold` is set to `0.65`.
 - `species_order` is defined here and used for consistent plotting order.
 
 ### `functions.R`
 **Role**: Helper functions used across plotting scripts.
 
 - `conditional_breaks(limit)`: y-axis breaks tuned for low-count facets
 - `circular_median_day(days)`: circular median for day-of-year (seasonality)
 - `build_world(LeftBound, latitude_range, longitude_range, species_of_interest)`: subsets `final_results` and returns projected map layers + `xlims`/`ylims`
 
 **Important**: `build_world()` expects a global `final_results` (created by sourcing `read.data.R`).
 
 ### `model_figures.R`
 **Role**: Produces several figure panels related to the semi-supervised labeling pipeline and model performance.
 
 **Major outputs (PDF)**:
 
 - `figures/panels/alluvial_plot.pdf`: flow of IDs through staging (`inr_0`→`inr_5`)
 - `figures/panels/confusion_matrix_raw.pdf`: confusion matrix proportions from `results/confusion_matrix.txt`
 - `figures/panels/confusion_matrix_norm.pdf`: row-normalized confusion matrix
 - `figures/panels/community_predicted_matrix.pdf`: agreement between community IDs and model predictions
 - `figures/panels/global_map.pdf`: global map of `final_results`
 - `figures/panels/year_species.pdf`: time histogram by species (post-2015)
 
 ### `seasonal_tiles.R`
 **Role**: Regional seasonal panels, iterated over predefined lat/lon “ranges” (requires setting `i`).
 
 **How to run**:
 
 - Set `i <- 1` (or 1–5)
 - `source("R_code/seasonal_tiles.R")`
 
 **Outputs (PDF)** (examples):
 
 - `figures/panels/seasonal_<species>_<range>.pdf`: points colored by day-of-year
 - `figures/panels/seasonal_tiled_count_<species>_<range>.pdf`: DGGS-tiled log-scaled counts
 - `figures/panels/seasonal_tiled_<species>_<range>.pdf`: DGGS-tiled median day-of-year
 - `figures/panels/polar_calendar_<range>.pdf`: reference polar color map
 
 ### `seasonal_histograms.R`
 **Role**: For a chosen region (`i`), generates seasonal histograms and year-by-year seasonal histograms.
 
 **How to run**:
 
 - Set `i <- 1` (or 1–5)
 - `source("R_code/seasonal_histograms.R")`
 
 **Outputs (PDF)**:
 
 - `figures/panels/months_<range>.pdf`: quarterly maps
 - `figures/panels/facet_hist_<range>.pdf`: stacked seasonal histograms by species
 - `figures/panels/yearly_hist_<species>_<range>.pdf`: seasonal histograms faceted by year
 
 ### `norm.R`
 **Role**: Explores “effort normalization” using the `norm_results` baseline dataset and a cyclic GAM (`mgcv::gam`) over day-of-year.
 
 **How to run**:
 
 - Set `i <- 1` (or 1–5)
 - `source("R_code/norm.R")`
 
 **Output (PDF)**:
 
 - `figures/panels/norm_hist_<species>_<range>.pdf`: baseline vs unweighted vs daily vs seasonal weighting
 
 ### `seasonal.r`
 **Role**: Older / exploratory seasonal plotting script.
 
 **Notes**:
 
 - Re-defines `circular_median_day()` and `build_world()` locally (similar to `functions.R`).
 - Includes both point-based seasonal maps and DGGS tiled maps.
 
 **Outputs (PDF)**:
 
 - `figures/panels/months_<name>.pdf`
 - `figures/panels/seasonal_<name>.pdf`
 - `figures/panels/seasonal_tiled_count_<name>.pdf`
 - `figures/panels/seasonal_tiled_season_<name>.pdf`
 
 ### `iNat_plot.r`
 **Role**: Exploratory map + alluvial plots comparing semisupervised labels vs community scientific names.
 
 **Outputs**:
 
 - `<title>_predictions.pdf` maps written to the current working directory
 
 **Important**: Contains a hard-coded `setwd(...)`.
 
 ### `map_problems.r`
 **Role**: Debugging/outlier triage for suspicious spatial clusters. Subsets records for specific species/regions and writes review TSVs.
 
 **Outputs**:
 
 - `<subset>_predictions.pdf` maps written to the current working directory
 - `<subset>.tsv` review tables (`id`, `image_url`, `place_guess`, `status`)
 
 **Important**: Contains a hard-coded `setwd(...)`.
 
 ### `PCA.qmd`
 **Role**: Quarto notebook for population-genomic PCA + related map panels.
 
 **Inputs** (relative to `R_code/` in the notebook):
 
 - `../data/metadata.tsv`
 - `../data/sample_ids.tsv`
 - `../data/subset.txt`
 - `../results/...` (e.g., covariance files)
 
 **Outputs**:
 
 - Writes panels to `../figures/panels/` (PDF/PNG)
 
 **Important**: Contains a hard-coded `setwd("/Users/samuelchurch/Downloads/Physalia_spatiotemporal/R_code")`.
 
 ### `admixture.qmd`
 **Role**: Quarto notebook for admixture plots, assignment maps, and pixy-based population summary panels.
 
 **Inputs**:
 
 - `../data/metadata.tsv`
 - `../data/sample_ids.tsv`
 - `../results/subset/` (admix Q files, bam list)
 - `../results/pixy/locations/` (pixy output tables)
 
 **Outputs**:
 
 - Writes panels to `../figures/panels/` (PDF/PNG)
 
 ## Reproducibility with `renv`
 
 The R scripts here rely on a consistent set of spatial + tidyverse packages (e.g., `sf`, `rnaturalearth`, `dggridR`, `ggplot2`, `dplyr`, `mgcv`, `ggalluvial`, etc.). `renv` is the easiest way to lock versions.
 
 ### Recommendation: choose the `renv` project root intentionally
 
 Because many scripts use paths like `data/...` and `results/...`, the cleanest options are:
 
 - **Option 1 (most compatible):** initialize `renv` at the **repository root** (`Physalia_spatiotemporal/`).
 - **Option 2 (R_code-only renv):** initialize inside `R_code/`, but still run scripts with working directory set to the repo root, and explicitly activate `R_code`’s `renv` when needed.
 
 If you initialize in `R_code/` and then set your working directory to the repo root, your scripts’ relative paths will work, but you need to ensure the `renv` environment is activated first.
 
 ### Create a new `renv` for `R_code/`
 
 From an R session:
 
 1. Set your working directory to `R_code/`.
 2. Run:
 
 ```r
 install.packages("renv")
 renv::init()
 ```
 
 3. Install any missing packages by running one of the scripts/notebooks.
 4. Lock versions:
 
 ```r
 renv::snapshot()
 ```
 
 This creates:
 
 - `R_code/renv.lock`
 - `R_code/renv/` (activation + infrastructure)
 
 ### Using the `renv` on a new machine
 
 1. Open R with working directory set to `R_code/`.
 2. Run:
 
 ```r
 install.packages("renv")
 renv::restore()
 ```
 
 ### Running scripts from the repo root while using `R_code`’s `renv`
 
 If you want to keep the lockfile inside `R_code/` but execute scripts from the repo root (recommended for the current relative paths), you can activate the environment explicitly:
 
 ```r
 source("R_code/renv/activate.R")
 source("R_code/read.data.R")
 ```
 
 After activation, you can run the other scripts normally.
