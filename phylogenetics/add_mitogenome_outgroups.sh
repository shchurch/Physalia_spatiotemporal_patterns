#!/usr/bin/env bash
# Add Rhizophysa mitogenome outgroups to a Physalia mitogenome alignment and
# re-infer, so the tree can be rooted rather than midpoint-rooted.
#
# Uses `mafft --add`, which aligns the new sequences into the existing alignment
# rather than recomputing it from scratch: the Physalia alignment is the one the
# HPC run produced and is not something to redo casually.
#
# Outgroups are the three GenBank Rhizophysa mitogenomes. NC_080941 / NC_080942
# are RefSeq mirrors of OQ957199 / OQ957206 and are deliberately excluded.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IN="${1:-$HERE/mitogenome_tree/submitted.aln.fa}"
TAG="${2:-submitted_rooted}"
OG="$HERE/mitogenome_tree/rhizophysa_outgroups.fasta"
OUT="$HERE/mitogenome_tree/${TAG}.aln.fa"

command -v mafft >/dev/null || { echo "ERROR: mafft not on PATH (conda activate tree)" >&2; exit 1; }
IQTREE="$(command -v iqtree || command -v iqtree2 || command -v iqtree3)"

echo "base alignment : $IN ($(grep -c '^>' "$IN") seqs)"
mafft --add "$OG" --adjustdirectionaccurately --thread -1 "$IN" > "$OUT" 2> "$HERE/mitogenome_tree/${TAG}.mafft.log"
sed -i '' -E 's/^>_R_/>/' "$OUT"
echo "with outgroups : $OUT ($(grep -c '^>' "$OUT") seqs)"

"$IQTREE" -s "$OUT" -B 1000 -nt AUTO -pre "$HERE/mitogenome_tree/${TAG}"
echo "done: ${TAG}.treefile"
