# Spatiotemporal distributions of bluebottles (*Physalia*) around the world's oceans

Code, data, and figures for the manuscript.

## Layout

| directory | contents | re-runnable? |
|---|---|---|
| `manuscript/` | the manuscript and supplementary figures, as Quarto sources | yes — `quarto render` |
| `analysis/` | R code for the iNaturalist analyses: seasonal abundance, effort normalisation, maps, classification figures | **yes** |
| `phylogenetics/` | gene and mitochondrial genome trees, tip metadata, GenBank submission bundles | **yes** |
| `machine_learning/` | image classification of iNaturalist photographs | no — needs the image corpus, which is not redistributed |
| `pipeline/` | Snakemake read QC and mitochondrial genome assembly, Illumina and Nanopore | no — needs the raw reads and a Slurm cluster |
| `data/` | iNaturalist exports, specimen tables, labels | — |
| `results/` | classification outputs | — |
| `figures/` | final figures, and the panels they are composed from | — |

### What can and cannot be re-run

`analysis/` and `phylogenetics/` regenerate their outputs from the data here.
Run R scripts from the repository root:

```r
source("analysis/norm.R")
```

`machine_learning/` and `pipeline/` were run on an HPC against inputs too large
for this repository: the iNaturalist image corpus and the raw sequencing reads.
The images are not redistributed, since their licences vary per observation, but
`results/final_labeled_dataset.tsv` lists every observation with its photo URL,
licence, and species label, so the corpus can be reassembled. The reads are in
the SRA. Neither directory has been modified since it was run.

### Figures

Several main figures are multi-panel compositions assembled in Illustrator from
the PDFs in `figures/panels/`. Re-running the R code regenerates the panels; the
composite `.png` files then need to be re-exported.

Figure files are named for what they show rather than by figure number, since
the numbering has changed more than once. The `.png` committed here is what the
manuscript includes.

## Data availability

Together these resources are intended as a toolkit for identifying and
monitoring the five described *Physalia* species. They are split across three
places, indexed here.

| Resource | Where |
|---|---|
| Labelled iNaturalist dataset — 20,704 observations with photo URL, licence, attribution, species, and label provenance | `results/final_labeled_dataset.tsv` |
| Gene alignments and trees — 16S, 18S, CO1, ITS | `phylogenetics/gene_trees/` |
| Mitochondrial genome alignments and trees, incl. the 199-sample tree used for species assignment | `phylogenetics/mitogenome_tree/` |
| Tip metadata for all trees | `phylogenetics/tip_metadata.tsv` |
| Seasonal abundance, effort normalisation, and mapping code | `analysis/` |
| Image classification code | `machine_learning/` |
| Read QC and mitogenome assembly workflows | `pipeline/` |
| Mitochondrial genomes (n=168) | GenBank PZ224317–PZ224484 |
| Nuclear 18S (n=181) | GenBank PZ802573–PZ802753 |
| Nuclear ITS (n=186) | GenBank PZ804143–PZ804328 |
| Raw reads | SRA — *submission pending* |
| Morphological characters, type material, and the comparative figures | the manuscript |

Two things are deliberately absent. The iNaturalist photographs are not
redistributed, since their licences vary per observation and roughly a quarter
carry no CC licence at all; the table above lists every observation so the
corpus can be reassembled under each contributor's own terms. The fitted
classification model was not retained, so it is not distributed either — the
training and inference code is here, and the seeds are fixed, so it can be
retrained.

This repository is archived on Zenodo; the DOI will be added here and to the
manuscript once minted.

The population-genomic analyses of these samples are a separate manuscript:
<https://github.com/shchurch/Physalia_population_genomics>
