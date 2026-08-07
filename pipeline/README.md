# pipeline

Read quality control and mitochondrial genome assembly, as described in the
Methods. Two Snakemake workflows: one for the Illumina whole-genome libraries
and one for the Oxford Nanopore libraries from the 2025 cruise.

Both were run on a Slurm cluster. `profile/config.yaml` is the Snakemake
profile used; `illumina/batch_snake.sh` and `ont/batch_snake.sh` are the
submission wrappers.

## `illumina/`

Applied to the Illumina WGS libraries. Rules, in order:

| rule | what it does |
|---|---|
| `download_SRA` | fetch previously published libraries |
| `reads_subset` | subset to 10 million reads |
| `trimmomatic_pe` | Trimmomatic 0.39, default settings |
| `fastqc` | read quality report |
| `kraken` | Kraken2 against the standard contaminant database |
| `map_human` | BWA-MEM against GRCh38, to check for human contamination |
| `get_organelle` | GetOrganelle, `-R 10 -k 21,45,65,85,105` |
| `mitos` | MITOS2 annotation (`runmitos.py`) |
| `trnascan` | tRNAscan-SE |
| `sharkmer` | sharkmer, `-k 31 -n 100` |
| `rego` | in-silico PCR for the marker loci |
| `report` | per-sample summary |

## `ont/`

Applied to the Oxford Nanopore libraries. It shares the QC front end with the
Illumina workflow, but **assembles the mitochondrial genome by a different
route**:

| rule | what it does |
|---|---|
| `reads_subset`, `fastqc`, `kraken`, `map_human` | as above |
| `map_to_mito` | minimap2 `-x map-ont` against a siphonophore mitochondrial reference set, keeping mapped reads only |
| `extract_mito_reads` | convert the mapped BAM back to FASTQ |
| `subsample_reads` | `seqtk sample -s100`, to 1,000,000 reads |
| `flye` | Flye, `--nano-raw --genome-size 20k --meta` |
| `get_organelle` | `get_organelle_from_assembly.py -F animal_mt`, run **on the Flye assembly graph** |
| `mitos`, `trnascan` | as above |
| `sharkmer` | sharkmer, **`-k 21`** rather than `-k 31` |
| `rego`, `report` | as above |

### How the two assemblies differ

This is the substantive difference between the workflows, and it is not simply
a change of assembler:

- **Illumina** runs `get_organelle_from_reads.py` directly on the trimmed
  reads, with `-R 10 -k 21,45,65,85,105`. GetOrganelle does its own seed-based
  read recruitment and graph assembly.
- **Nanopore** first recruits reads by mapping to a reference set of
  siphonophore mitochondrial genomes, subsamples them, assembles with Flye,
  and only then runs GetOrganelle — as
  `get_organelle_from_assembly.py`, on Flye's assembly graph rather than on
  reads.

So the Nanopore mitogenomes are reference-recruited and Flye-assembled, with
GetOrganelle used to resolve the organellar path through the graph. The
Illumina mitogenomes are assembled by GetOrganelle throughout.

The `flye` rule ends in `|| true`, so a failed assembly does not halt the
workflow; the `get_organelle` rule then checks for `assembly_graph.gfa` and
records "No flye assembly detected" if it is absent.

The two sharkmer k-mer sizes are the other difference: 31 for Illumina, 21 for
Nanopore, reflecting the different error profiles.

## `tool_scripts/`

Helper scripts used around the workflows rather than by them: assembling the
per-sample QC report, retrieving MITOS results, selecting the longest sequence
per marker, summarising individual mitogenomes, concatenating FASTA files,
trimming FASTA headers, and pulling reference sequences from NCBI.

The workflows do not call these. They are kept because the Methods describe
steps they carry out. Lab-general helpers for other taxa and other projects,
which came from the same HPC directory, are not included here.

## Notes

Paths in the config files point at the cluster's scratch filesystem and at
locally built copies of `sharkmer` and `rego`. They are recorded as they were
run, and would need adjusting to run elsewhere.
