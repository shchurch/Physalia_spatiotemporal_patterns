# Mitochondrial genome tree

Maximum-likelihood tree of 199 *Physalia* mitochondrial genomes. This is the tree
referred to in the methods ("mitochondrial genomes were aligned with MAFFT and
passed to IQ-TREE with 1000 bootstraps").

## Files

| File | Contents |
|---|---|
| `combined.aln.fa` | MAFFT alignment, 201 sequences / 25,753 sites |
| `iqtree_result.treefile` | ML tree |
| `iqtree_result.contree` | Consensus tree with UFBoot support values |
| `iqtree_result.iqtree` | IQ-TREE report — model selection, alignment stats |
| `Physalia_mt_IDs.txt` | The 199 sample IDs the tree was built from |

## Provenance

Generated on the Yale McCleary HPC, completed **2026-03-05 17:55:17**. IQ-TREE
2.2.2.3, best-fit model **GTR+F+I+R7** by BIC, 1000 ultrafast bootstrap replicates.

The authoritative pipeline is `new_mt_script.sh` driven by `batch_newmt.sh`:

```bash
bash tool_scripts/new_mt_script.sh 24 Physalia_annotated tmp.txt Physalia_mt_IDs.txt
```

`new_mt_script.sh` takes the first `*path*fasta` from each
`getorganelle/<ID>_mtgenome/` directory, writes a per-sample FASTA, merges the
MITOS `result.gff` with the tRNAscan-SE output into a per-sample annotation GFF,
then aligns with `mafft --adjustdirectionaccurately --auto` and builds the tree
with `iqtree -bb 1000`.

**`20250417_redo_iqtree_physalia/` is not the authoritative pipeline.** That
directory holds an earlier local attempt whose mitogenome alignment
(`mito.aln.fasta`) is empty — MAFFT was killed partway through. Its four
single-gene trees (16S, 18S, CO1, ITS) are complete and unaffected, but the
mitogenome tree there should not be used. Likewise `physalia_mt_tree/` on the
HPC is a superseded earlier run: it adds NCBI reference sequences but predates
the four Saint Helena samples.

## Taxon set

199 samples, matching `Physalia_mt_IDs.txt` exactly:

- all **168** mitogenomes submitted to GenBank (accessions PZ224316–PZ224483)
- **28** `SEA2025-*` samples from the 2025 cruise
- `YPM-IZ-104465`, `YPM-IZ-110972`, `YPM-IZ-111760`

No external or NCBI reference sequences are included — this tree is the study's
own sequences only.

## Note on the Saint Helena samples

`YPM-IZ-115977`, `-115978`, `-115980`, `-115990` (Saint Helena, 16.003892 S
5.714589 W, all *P. minuta*, accessions PZ224480–PZ224483) fall inside a 56-tip
clade at 100% bootstrap whose other members are Pacific: SW Pacific `SEA2025-*`,
Tasmania, New Zealand, and Hawai'i. They are nested within that clade rather
than sister to it. See issue #9.
