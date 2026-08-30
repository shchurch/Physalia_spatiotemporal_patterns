# Bluebottles (*Physalia*) around the world's oceans

Code, data, and figures for **Seasonality, distribution, and morphology of bluebottles (*Physalia*) in the world's oceans**. Full author list and corresponding author are in `manuscript/`.

Bluebottles are neustonic siphonophores comprising at least five species with overlapping ranges. This study combines participatory-science photographs, whole mitochondrial genomes, and morphological description into a toolkit for identifying and monitoring those species, and uses it to describe where and when each one strands.

The population-genomic analyses of the same specimens belong to a separate manuscript: <https://github.com/shchurch/Physalia_population_genomics>

## What is here

| | |
|---|---|
| `manuscript/` | Manuscript and supplementary figures, as Quarto sources |
| `analysis/` | R code for the iNaturalist analyses: seasonality, observer-effort normalisation, maps, classifier figures |
| `phylogenetics/` | Gene and mitochondrial genome alignments, trees, tip metadata, and GenBank submission bundles |
| `machine_learning/` | Image classification of iNaturalist photographs |
| `pipeline/` | Snakemake workflows for read quality control and mitochondrial genome assembly |
| `data/` | iNaturalist exports, specimen tables, and manual labels |
| `results/` | Classification outputs, including the labelled observation dataset |
| `figures/` | Figures as they appear in the manuscript |

## Reproducing the analyses

`analysis/` and `phylogenetics/` regenerate their outputs from the data in this repository.

For the R code, install the pinned package versions once, then run scripts from the repository root:

```r
renv::restore(project = "analysis")
```

```
Rscript analysis/read.data.R          # builds results/final_labeled_dataset.tsv
Rscript analysis/model_figures.R      # classifier panels
Rscript analysis/seasonal_panels.R    # seasonality panels
Rscript analysis/open_ocean_map.R     # open-ocean sampling map
```

`analysis/README.md` documents each script and its outputs. Do not source `analysis/renv/activate.R` from the repository root; see that file for why.

The phylogenetic metadata regenerates with Python and Biopython:

```
python3 phylogenetics/metadata/build_tip_metadata.py
python3 phylogenetics/metadata/assign_ont_species.py
```

Alignments and trees themselves were built with MAFFT and IQ-TREE; `phylogenetics/build/` holds those commands, and `phylogenetics/README.md` describes the tree set.

Two directories cannot be re-run here. `machine_learning/` needs the iNaturalist image corpus, and `pipeline/` needs the raw sequencing reads and a Slurm cluster. Both were run once on HPC and their outputs are included as they were produced.

### Figures

Most figures are multi-panel compositions assembled in Illustrator from the panels in `figures/panels/`. The panels are rebuilt by the scripts above and are not tracked, so re-running the generating script before re-exporting a figure is what keeps the two in step. The Illustrator sources are not tracked either — they are large binaries that git would store whole on every save.

Figures are named for what they show rather than by number, since the numbering changed during review. The `.png` committed here is the version the manuscript includes.

## Data availability

| Resource | Where |
|---|---|
| Labelled iNaturalist dataset — 20,704 observations with photo URL, licence, attribution, species, and label provenance; 16,453 identified to species | `results/final_labeled_dataset.tsv` |
| Gene alignments and trees — 16S, 18S, *COI*, ITS | `phylogenetics/gene_trees/` |
| Mitochondrial genome alignments and trees, including the 199-sample tree used for species assignment | `phylogenetics/mitogenome_tree/` |
| Tip metadata for every tree | `phylogenetics/tip_metadata.tsv` |
| Mitochondrial genomes (n = 168) | GenBank PZ224317–PZ224484 |
| Nuclear 18S (n = 181) | GenBank PZ802573–PZ802753 |
| Nuclear ITS (n = 186) | GenBank PZ804143–PZ804328 |
| Raw reads | SRA, submission pending |
| Morphological characters, type material, and comparative imaging | the manuscript |

Two things are not distributed here. The iNaturalist photographs remain with their contributors: licences vary per observation and about a quarter carry no Creative Commons licence at all, so the dataset above lists every observation and its licence instead, allowing the corpus to be reassembled under each contributor's own terms. The fitted classification model was not retained; the training and inference code is in `machine_learning/`.

## License

Code is released under the MIT license; data, figures, and manuscript text under
CC BY 4.0. The iNaturalist photographs are not covered by either — they remain
under each contributor's own terms. See `LICENSE`.

## Citation

Please cite the manuscript. This repository is archived on Zenodo, and the DOI will be added here once minted.
