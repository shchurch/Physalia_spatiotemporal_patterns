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

## Contents

| Directory | Contents |
|---|---|
| `mitogenome_tree/` | Whole mitochondrial genome alignment and ML tree, 199 samples |

Gene trees for 16S, 18S, CO1 and ITS are in `gene_trees/`, built by
`build_gene_trees.sh`. Tip marks distinguish sequences new to this study from
those also present in the previous manuscript's trees; the previous tip sets are
recorded in `previous_study_tips.tsv` so the figures do not depend on files
outside this repository.
