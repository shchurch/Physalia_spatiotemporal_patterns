# Bluebottles (*Physalia*) around the world's oceans

Code, data, and figures for **Seasonality, distribution, and morphology of bluebottles (*Physalia*) in the world's oceans**. Authors and corresponding author are listed in `manuscript/`.

Bluebottles are neustonic siphonophores comprising at least five species with overlapping ranges. This study combines participatory-science photographs, whole mitochondrial genomes, and morphological description into a toolkit for identifying and monitoring those species, and uses it to describe where and when each one strands.

A population-genomics sister study is at <https://github.com/shchurch/Physalia_population_genomics>.

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

`analysis/` and `phylogenetics/` regenerate their outputs from the data here; pinned package versions are available through renv, and each directory's README covers its scripts. `machine_learning/` and `pipeline/` were run on HPC against the image corpus and the raw reads, neither of which is redistributed, so their outputs are included as produced.

## Data availability

| Resource | Where |
|---|---|
| Labelled iNaturalist dataset — 20,704 observations with photo URL, licence, attribution, species, and label provenance; 16,453 identified to species | `results/final_labeled_dataset.tsv` |
| Gene alignments and trees — 16S, 18S, *COI*, ITS | `phylogenetics/gene_trees/` |
| Mitochondrial genome alignments and trees, including the identification tree covering all 199 sequenced specimens | `phylogenetics/mitogenome_tree/` |
| Tip metadata for every tree | `phylogenetics/tip_metadata.tsv` |
| Mitochondrial genomes (n = 168) | GenBank PZ224317–PZ224484 |
| Nuclear 18S (n = 181) | GenBank PZ802573–PZ802753 |
| Nuclear ITS (n = 186) | GenBank PZ804143–PZ804328 |
| Raw reads (48 libraries, 56 runs) | SRA BioProject PRJNA1509992, runs SRR40085724–SRR40085779 |
| Morphological characters, type material, and comparative imaging | the manuscript |

The iNaturalist photographs are not redistributed, since their licences vary per observation; the dataset above records each one, so the corpus can be reassembled under each contributor's own terms. The fitted classification model was not retained.

## License and citation

Code is MIT, data and text CC BY 4.0; see `LICENSE`. Please cite the manuscript. This repository is archived on Zenodo, and the DOI will be added once minted.
