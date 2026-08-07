#!/usr/bin/env bash
# Build the mitochondrial genome tree from the deposited mitogenomes only.
#
# Scope: the 168 mitogenomes submitted to GenBank (PZ224316-PZ224483). These are
# the WGS Illumina samples, for which mitochondrial genomes were assembled and
# annotated. The SEA2025 cruise samples are ONT and contribute to the individual
# gene trees via sharkmer instead; three further samples with assemblies but no
# accession are also out of scope here.
#
# The alignment is subset from the existing 199-sample MAFFT alignment rather
# than re-aligned: dropping sequences from an alignment cannot change the
# homology of the columns that remain. All-gap columns left behind are removed,
# since they carry no information and inflate the site count.
#
# Requires iqtree from the `tree` conda environment:
#     conda activate tree

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA="${DATA_ROOT:-$HERE/../..}"

SRC_ALN="${SRC_ALN:-$HERE/mitogenome_tree/identification.aln.fa}"
KEEP_IDS="${KEEP_IDS:-$HERE/mitogenome_submission/Physalia_mitogenomes_IDs.txt}"
OUT_DIR="${OUT_DIR:-$HERE/mitogenome_tree}"
ALN="$OUT_DIR/submitted.aln.fa"
PREFIX="${PREFIX:-$OUT_DIR/submitted}"
THREADS="${THREADS:-AUTO}"

IQTREE="$(command -v iqtree || command -v iqtree2 || command -v iqtree3 || true)"
[ -n "$IQTREE" ] || { echo "ERROR: iqtree not on PATH (conda activate tree)" >&2; exit 1; }
[ -s "$SRC_ALN" ] || { echo "ERROR: source alignment not found: $SRC_ALN" >&2; exit 1; }
[ -f "$KEEP_IDS" ] || { echo "ERROR: accession ID list not found: $KEEP_IDS" >&2; exit 1; }

echo "source alignment : $SRC_ALN  ($(grep -c '^>' "$SRC_ALN") sequences)"
echo "keep list        : $KEEP_IDS ($(grep -c . "$KEEP_IDS") ids)"

# Tip labels carry MAFFT's _R_ direction marker and a _mtgenome_<contig> suffix;
# match on the sample ID embedded in them.
awk 'NR==FNR { if ($1 != "") keep[$1]; next }
     /^>/ {
         h = substr($0, 2); sub(/^_R_/, "", h)
         id = h; sub(/_(mtgenome|mito)_.*$/, "", id)
         p = (id in keep)
     }
     p' "$KEEP_IDS" "$SRC_ALN" > "$OUT_DIR/.subset.tmp"

echo "subset           : $(grep -c '^>' "$OUT_DIR/.subset.tmp") sequences"

# Strip columns that are all gaps after subsetting.
python3 - "$OUT_DIR/.subset.tmp" "$ALN" <<'PY'
import sys
src, dst = sys.argv[1], sys.argv[2]
names, seqs, cur = [], [], None
for line in open(src):
    line = line.rstrip("\n")
    if line.startswith(">"):
        names.append(line[1:]); seqs.append([]); cur = seqs[-1]
    elif cur is not None:
        cur.append(line)
seqs = ["".join(s) for s in seqs]
n = len(seqs[0])
assert all(len(s) == n for s in seqs), "ragged alignment"
keep = [i for i in range(n) if any(s[i] not in "-?" for s in seqs)]
with open(dst, "w") as f:
    for nm, s in zip(names, seqs):
        f.write(">" + nm + "\n" + "".join(s[i] for i in keep) + "\n")
print(f"columns          : {n} -> {len(keep)} ({n - len(keep)} all-gap removed)")
PY
rm -f "$OUT_DIR/.subset.tmp"

"$IQTREE" -s "$ALN" -B 1000 -nt "$THREADS" -pre "$PREFIX"

echo "done: ${PREFIX}.treefile"
