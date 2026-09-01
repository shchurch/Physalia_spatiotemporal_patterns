# analysis

R code for the iNaturalist analyses: it assembles the labelled observation
dataset from the raw exports, manual corrections and model predictions, then
draws the seasonality, distribution and classifier panels used in the manuscript.

Every script reads from `data/` and `results/` and writes panels into
`figures/panels/`. The multi-panel figures in `figures/` are assembled from those
panels in Illustrator; the `.ai` sources are not tracked.

## Running

Start R **in the repository root**, not in this directory — every path is
relative to the root:

```r
renv::restore(project = "analysis")   # once, to install pinned versions
```

```
Rscript analysis/read.data.R
Rscript analysis/model_figures.R
Rscript analysis/seasonal_panels.R
Rscript analysis/open_ocean_map.R
```

Do not `source("analysis/renv/activate.R")` from the root: `activate.R` treats the
current directory as the project, so it creates an empty library there and hides
everything installed. If that has happened, delete the stray `renv/` at the root.

`seasonal_tiles.R`, `seasonal_histograms.R` and `norm.R` run one region at a time
and expect `i` to be set to 1–5 first:

```r
for (i in 1:5) source("analysis/seasonal_tiles.R")
```

## Contents

| | |
|---|---|
| `read.data.R` | Builds `final_results`, the canonical per-observation table, by layering manual labels, corrections, model predictions and community IDs in order. Writes `results/final_labeled_dataset.tsv`. Every other script sources this one. |
| `functions.R` | Shared helpers: circular median day of year, map construction, axis breaks |
| `model_figures.R` | Classification figure panels — label-provenance alluvial, confusion matrices, model vs community agreement, global map, observations by year |
| `seasonal_panels.R` | The seasonality figures: radial species rings and three-month maps for four regions, in unweighted, effort-weighted and daily-weighted versions |
| `seasonal_tiles.R` | Per-region hexagon maps of circular median day of year, and tiled record counts |
| `seasonal_histograms.R` | Per-region seasonal histograms, overall and by year |
| `norm.R` | Observer-effort normalisation against the beach-species baseline, using a cyclic GAM over day of year |
| `open_ocean_map.R` | The South Pacific open-ocean sampling map |
| `seasonal.r`, `iNat_plot.r`, `map_problems.r` | Earlier exploratory scripts, kept for provenance. `map_problems.r` defines the geographic outlier regions used in the manual review described in the Methods. |

## Inputs

| | |
|---|---|
| `data/observations-604981.csv` | *Physalia* observations exported from iNaturalist |
| `data/observations-607577.csv`, `-607598.csv` | Hermit crab and echinoderm exports, the observer-effort baseline |
| `data/labels.tsv` | First-pass manual species labels |
| `data/corrected.tsv`, `corrected_final.tsv`, `corrected_DD.tsv` | Manual correction passes |
| `results/all_predictions.json` | Per-image classifier predictions |
| `data/sample_ids.tsv`, `data/subset.txt` | Sequenced specimen coordinates and genomic cluster assignments |

Analysis choices — bin widths, the effort-weighting scheme, the intensity cap,
the record thresholds below which a median or a ring is not drawn, and the
regional bounding boxes — are documented in comments at the point of use in each
script rather than repeated here.

## Reproducibility

Package versions are pinned in `renv.lock` and were last verified on 2026-08-29
against R 4.5.2, where `renv::restore(project = "analysis")` resolved all 139
packages and `model_figures.R`, `seasonal_panels.R` and `seasonal_tiles.R`
reproduced every existing panel byte-identically. Note that restoring this way
installs into the default R library rather than an isolated project library, so it
will change versions of packages already present on the machine.

## Authorship note

Code and documentation in this directory were written and edited in part with
Claude Code (Anthropic). The authors have reviewed all of it and take full
responsibility for its contents.
