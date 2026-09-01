# Mitochondrial genome trees

Two trees are built from the same alignment, for two different jobs.

## `submitted.*` — the deposited mitogenomes (168 samples)

The primary mitogenome phylogeny. Contains exactly the 168 mitochondrial genomes
deposited in GenBank as **PZ224317–PZ224484**, so the tree and the accessions
correspond one to one.

These are the whole-genome-sequenced Illumina samples, for which mitochondrial
genomes were assembled with GetOrganelle and annotated with MITOS2 and
tRNAscan-SE. Out of scope here: the 28 ONT `SEA2025-*` cruise samples, and three
Illumina samples with assemblies but no accession (`YPM-IZ-104465`, `-110972`,
`-111760`).

Built by `../build_mitogenome_tree.sh`, which subsets the identification
alignment to the deposited set and drops columns left all-gap. Removing
sequences from an alignment cannot change the homology of the columns that
remain, so the alignment is not recomputed. Subsetting removed **10,333 of
25,753 columns** (40%) as all-gap — those columns existed only to accommodate
the excluded sequences.

## `identification.*` — all sequenced samples (199 specimens, 201 tips)

A supporting tree, **not** a published phylogeny of deposited data. Its only
purpose is to assign species to the ONT cruise samples by their placement among
the Illumina samples, whose identities are known from the GenBank submission.

**The alignment holds 201 sequences for 199 specimens.** `YPM-IZ-110972` and
`YPM-IZ-111760` each appear twice, as two GetOrganelle path variants of the same
assembly — the pairs differ by 2 and 4 bases respectively and each is recovered
as a cherry at 100% bootstrap. These are the two specimens withheld from the
GenBank submission for being the longest recovered, and the duplication is the
same ambiguity that made them long: the assembly graph admits more than one path,
and `new_mt_script.sh` took the first `*path*fasta` per sample, which for these
two was not unique. It does not affect any species assignment. `plot_trees.R`
drops the duplicate at plot time, so the published figure shows 199 tips, but the
alignment and the treefiles built from it carry both copies — anything counting
tips in those files should expect 201. See issue #57.

The ONT assemblies are good enough to place a sample in a well-supported clade
but not good enough to deposit as reference mitogenomes: their terminal branches
are 2.2× longer at the median and 5× at the mean than the Illumina samples
(max 0.438 against 0.036), reflecting residual ONT error.

`../assign_ont_species.py` reads this tree and writes
`../ont_species_assignments.tsv` — 25 *P. minuta*, 3 *P. megalista*, with clade
size and bootstrap support per sample. Figure 7 should be built from that file
rather than from assignments read off the tree by hand. Seven assignments fall
below 90% bootstrap and are flagged there.

**This tree is why 168 accessions accompany 199 sequenced specimens.** Both
numbers are correct; they answer different questions.

### Why the ONT samples cannot be identified from the gene trees instead

Only 20 of the 28 have any sharkmer marker at all, and all three *P. megalista*
samples — `SEA2025-Ph227`, `-Ph277`, `-Ph278` — have none. Identifying the
cruise samples from the gene trees would drop every *megalista* in the 2025
transect, which is the signal behind Figure 7's eastern distribution.

## Provenance

The source alignment was produced on the Yale McCleary HPC by
`new_mt_script.sh`, driven by `batch_newmt.sh`:

```bash
bash new_mt_script.sh   # HPC helper, not distributed here 24 Physalia_annotated tmp.txt Physalia_mt_IDs.txt
# Physalia_mt_IDs.txt is tracked here as ../Physalia_sample_IDs.txt
```

It takes the first `*path*fasta` from each `getorganelle/<ID>_mtgenome/`
directory, writes a per-sample FASTA, merges the MITOS `result.gff` with the
tRNAscan-SE output into a per-sample annotation GFF, then aligns with
`mafft --adjustdirectionaccurately --auto` (v7.525).

Both trees here were inferred locally with **IQ-TREE 3.0.1**, `-B 1000`
ultrafast bootstrap, model chosen by ModelFinder under BIC — the same version as
the gene trees.

The original inference was run on the HPC under IQ-TREE 2.2.2.3. Its output is
not kept in this repository, since the trees presented were all re-inferred
under 3.0.1. Worth recording, though: the two versions chose different models on
the identical alignment — 2.2.2.3 GTR+F+I+R7, 3.0.1 GTR+F+I+R5 — while
recovering the same Saint Helena result at 100% bootstrap.

**`20250417_redo_iqtree_physalia/` is not the authoritative pipeline.** That
directory holds an earlier local attempt whose mitogenome alignment
(`mito.aln.fasta`) is empty — MAFFT was killed partway through. Likewise
`physalia_mt_tree/` on the HPC is a superseded earlier run: it adds NCBI
reference sequences but predates the five Saint Helena samples.

## Note on the Saint Helena samples

Five samples were collected at Saint Helena (16.003892 S, 5.714589 W), all
retained in both trees:

| Sample | Accession | Species |
|---|---|---|
| `YPM-IZ-115977` | PZ224480 | *P. minuta* |
| `YPM-IZ-115978` | PZ224481 | *P. minuta* |
| `YPM-IZ-115980` | PZ224482 | *P. minuta* |
| `YPM-IZ-115990` | PZ224483 | *P. minuta* |
| `YPM-IZ-116019` | PZ224484 | *P. megalista* |

The four *P. minuta* samples fall inside a 30-tip, entirely *P. minuta* clade at
100% bootstrap in the deposited tree — nested within Pacific material rather
than sister to it. This is the South Atlantic range extension. `YPM-IZ-116019`
groups separately, with *P. megalista*. See issue #9.

Note these five are the only samples of the 199 with no row in
`data/sample_ids.tsv` — see issue #1.
