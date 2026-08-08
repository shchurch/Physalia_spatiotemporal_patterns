# Phylogenetics

## `Physalia_sample_IDs.txt` — the study sample list

The **199** *Physalia* specimens sequenced for this study. This is the canonical
sample list for every phylogenetic analysis here, not just the mitogenome tree.

It comprises:

- the **168** mitogenomes submitted to GenBank (accessions PZ224317–PZ224484)
- **28** `SEA2025-*` samples from the 2025 cruise
- `YPM-IZ-104465`, `YPM-IZ-110972`, `YPM-IZ-111760`

All 199 have both an assembled mitochondrial genome and a `shark_results/`
directory, so every sample is available to both the mitochondrial and the
nuclear gene analyses.

### Deliberate exclusions

`data/sample_ids.tsv` lists twelve further specimens marked `excluded`. They
have no sequence data of any kind — no assembly, no sharkmer output — and are
intentionally absent from the sample list:

```
YPM-IZ-106944   YPM-IZ-110432   YPM-IZ-110474-2   YPM-IZ-110574
YPM-IZ-110632   YPM-IZ-110694-dry   YPM-IZ-110826   YPM-IZ-110827
YPM-IZ-110879   YPM-IZ-110973   YPM-IZ-111015     YPM-IZ-111019
```

The list is otherwise complete: no specimen appearing in any existing gene tree
or specimen table falls outside these 211 (199 + 12).

## Layout

Scripts are grouped by what they do; the rest of the directories are data and
output.

| Directory | Contents | Runs from a clean clone? |
|---|---|---|
| `build/` | alignment and tree inference: `build_gene_trees.sh`, `build_mitogenome_tree.sh`, `add_mitogenome_outgroups.sh`, `rebuild_mitogenome_tree.sh` | all but `build_gene_trees.sh` |
| `metadata/` | `build_tip_metadata.py`, `assign_ont_species.py`, `annotate_its.py` | yes |
| `submission/` | `build_gene_submission.py`, `add_specimen_vouchers.py` | `build_gene_submission.py` only |
| `plots/` | `plot_trees.R` | yes |
| `external_seqs/` | published sequences and `harvest_genbank.py` (queries GenBank) | needs network |
| `gene_trees/` | 16S, 18S, CO1, ITS alignments and trees | output |
| `mitogenome_tree/` | mitochondrial genome alignments and trees | output |
| `mitogenome_submission/` | metadata from the mitogenome GenBank submission | data |
| `figures/` | the seven tree figures | output |

Scripts locate their inputs relative to `phylogenetics/`, so they can be run
from anywhere:

```
python3 metadata/build_tip_metadata.py
Rscript plots/plot_trees.R
```

### The two that need the working folder

`build/build_gene_trees.sh` gathers per-sample marker sequences from
`mitos_results_fas` and `shark_results`, which are not in this repository —
`shark_results` alone is 930 MB. Its outputs, `gene_trees/*.all.fasta`, are
committed, so every downstream step reproduces without it.

`submission/add_specimen_vouchers.py` reads and writes the GenBank submission
bundles themselves rather than consuming metadata, so it points outside by
design. Both directories are overridable with `--mito-dir` and `--genes-dir`.

## Gene trees

Gene trees for 16S, 18S, CO1 and ITS are in `gene_trees/`, built by
`build/build_gene_trees.sh`. Tip marks distinguish sequences new to this study
from those also present in the previous manuscript's trees; the previous tip
sets are recorded in `previous_study_tips.tsv` so the figures do not depend on
files outside this repository.
