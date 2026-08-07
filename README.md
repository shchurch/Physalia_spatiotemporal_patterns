# Spatiotemporal distributions of bluebottles (*Physalia*) around the world's oceans

Code, data, and figures for the manuscript.

The repository holds what is needed to follow and, where possible, re-run the
analyses. It is not a complete record of the project — working files, private
notes, and intermediate outputs live outside it.

## Layout

| directory | contents | re-runnable? |
|---|---|---|
| `manuscript/` | the manuscript and supplementary figures, as Quarto sources | yes — `quarto render` |
| `analysis/` | R code for the iNaturalist analyses: seasonal abundance, effort normalisation, maps, classification figures | **yes** |
| `phylogenetics/` | gene and mitochondrial genome trees, tip metadata, GenBank submission bundles | **yes** |
| `machine_learning/` | image classification of iNaturalist photographs | no — needs the image corpus and a GPU |
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

`machine_learning/` and `pipeline/` were run on an HPC against inputs not
distributed here — the image corpus and the raw sequencing reads respectively.
They are included so the procedure and parameters can be checked. Neither has
been modified since it was run.

### Figures

Several main figures are multi-panel compositions assembled in Illustrator from
the PDFs in `figures/panels/`. Re-running the R code regenerates the panels; the
composite `.png` files then need to be re-exported from their `.ai` sources.

## Data availability

Mitochondrial genomes, 18S, and ITS sequences are deposited in GenBank; see the
Data availability section of the manuscript for accession ranges. Raw reads are
in the SRA.

The population-genomic analyses of these samples are a separate manuscript:
<https://github.com/shchurch/Physalia_population_genomics>
