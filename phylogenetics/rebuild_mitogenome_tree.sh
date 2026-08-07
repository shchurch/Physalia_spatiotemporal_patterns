#!/usr/bin/env bash
# Rebuild the mitochondrial genome tree locally under the same IQ-TREE version
# as the gene trees.
#
# The tree currently in mitogenome_tree/ was inferred on the McCleary HPC with
# IQ-TREE 2.2.2.3. The gene trees are built here with IQ-TREE 3.0.1, so this
# re-infers the mitogenome tree from the same alignment under the same version,
# removing the version discrepancy noted in issue #20.
#
# The MAFFT alignment is reused unchanged -- it is the input that took the HPC
# run to produce, and re-aligning would change the alignment as well as the
# inference, which is not the point of this rebuild. Only the tree search is
# repeated.
#
# Requires iqtree from the `tree` conda environment:
#     conda activate tree

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ALN="${ALN:-$HERE/mitogenome_tree/identification.aln.fa}"
OUT_DIR="${OUT_DIR:-$HERE/mitogenome_tree}"
PREFIX="${PREFIX:-$OUT_DIR/iqtree_result_v3}"
THREADS="${THREADS:-AUTO}"

IQTREE="$(command -v iqtree || command -v iqtree2 || command -v iqtree3 || true)"
[ -n "$IQTREE" ] || { echo "ERROR: iqtree not on PATH (conda activate tree)" >&2; exit 1; }
[ -s "$ALN" ] || { echo "ERROR: alignment not found or empty: $ALN" >&2; exit 1; }

echo "alignment : $ALN"
echo "sequences : $(grep -c '^>' "$ALN")"
echo "iqtree    : $IQTREE"
"$IQTREE" --version | head -1

# Matches the original HPC invocation (-bb 1000 is the legacy spelling of -B).
"$IQTREE" -s "$ALN" -B 1000 -nt "$THREADS" -pre "$PREFIX"

echo "done: ${PREFIX}.treefile"
