#!/usr/bin/env bash
# Build the 16S / 18S / CO1 / ITS gene trees for the Physalia sample set.
#
# A local port of the gene-tree script used on the McCleary HPC. Same stages in
# the same order -- gather, trim headers, select longest per sample, restrict to
# the study samples, add external sequences, align, infer -- but reading from
# locally downloaded MITOS and sharkmer output instead of the HPC directories.
#
# The two mitochondrial loci draw on both MITOS (from the assembled mitogenomes)
# and sharkmer; where a sample has both, the longer sequence wins. The two
# nuclear loci are sharkmer-only.
#
# Requires mafft and iqtree. Both live in the `tree` conda environment:
#     conda activate tree
#
# Usage:  ./build_gene_trees.sh [locus ...]        (default: all four)

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"   # phylogenetics/
DATA="${DATA_ROOT:-$ROOT/..}"

MITOS_DIR="${MITOS_DIR:-$DATA/mccleary_20250114_Siph_Genomics_partialdownload/mitos_results_fas}"
SHARK_DIR="${SHARK_DIR:-$DATA/shark_results}"
EXT_DIR="${EXT_DIR:-$ROOT/external_seqs}"
ID_FILE="${ID_FILE:-$ROOT/Physalia_sample_IDs.txt}"
OUT_DIR="${OUT_DIR:-$ROOT/gene_trees}"
THREADS="${THREADS:-AUTO}"

# locus : mitos gene name (or NONE) : sharkmer gene name (or NONE)
LOCI_16S="rrnL:cnidaria_16S"
LOCI_18S="NONE:cnidaria_18S"
LOCI_CO1="cox1:cnidaria_CO1"
LOCI_ITS="NONE:cnidaria_ITS"

command -v mafft  >/dev/null || { echo "ERROR: mafft not on PATH (conda activate tree)" >&2; exit 1; }
IQTREE="$(command -v iqtree || command -v iqtree2 || command -v iqtree3 || true)"
[ -n "$IQTREE" ] || { echo "ERROR: iqtree not on PATH (conda activate tree)" >&2; exit 1; }

for d in "$MITOS_DIR" "$SHARK_DIR" "$EXT_DIR"; do
    [ -d "$d" ] || { echo "ERROR: missing input directory $d" >&2; exit 1; }
done
[ -f "$ID_FILE" ] || { echo "ERROR: missing sample list $ID_FILE" >&2; exit 1; }

mkdir -p "$OUT_DIR"

# Samples whose MITOS annotations are NOT used as a marker source. Mitochondrial
# genomes were assembled and annotated for the WGS Illumina samples only; the
# SEA2025 cruise samples are ONT, and their assemblies are not the basis of any
# deposited mitogenome. Gene markers for those samples come from sharkmer alone,
# as the methods describe. They still enter the gene trees -- just via sharkmer.
#
# This also removes SEA2025-Ph277 from the gene trees structurally rather than by
# name: its ONT assembly is locally damaged (nad4 in five fragments, rrnL called
# on the + strand while every other gene is on -), which produced terminal
# branches of 6.23 (CO1) and 2.52 (16S) substitutions/site when its MITOS
# markers were used. It has no sharkmer product at any locus, so it now drops
# out on its own.
MITOS_SOURCE_EXCLUDE="${MITOS_SOURCE_EXCLUDE:-SEA2025}"

# Pull every record whose header mentions $gene out of a MITOS result.fas,
# re-headering it with the sample ID. Mirrors the HPC retrieval script, which
# builds the header from the containing directory name.
gather_mitos () {
    local gene="$1" out="$2"
    : > "$out"
    while IFS= read -r ID; do
        [ -n "$ID" ] || continue
        case "$ID" in *${MITOS_SOURCE_EXCLUDE}*) continue ;; esac
        local f="$MITOS_DIR/$ID/result.fas"
        [ -f "$f" ] || continue
        awk -v id="$ID" -v gene="$gene" '
            /^>/ { keep = ($0 ~ gene); if (keep) print ">" id "_" substr($0,2); next }
            keep { print }
        ' "$f" >> "$out"
    done < "$ID_FILE"
}

gather_shark () {
    local gene="$1" out="$2"
    : > "$out"
    while IFS= read -r ID; do
        [ -n "$ID" ] || continue
        local f="$SHARK_DIR/$ID/${ID}_${gene}.fasta"
        [ -s "$f" ] || continue
        cat "$f" >> "$out"
    done < "$ID_FILE"
}

# Reduce each header to its first token, split on space, underscore or slash.
# MITOS "YPM-IZ-1_scaffold_1--3; 31-1593; -; cox1_0" and sharkmer
# "YPM-IZ-1 cnidaria_CO1 product 0 length 688" both collapse to the sample ID,
# which is what lets the two sources merge below. Same rule as
# the HPC header-trimming script.
trim_headers () {
    awk 'BEGIN{OFS=""} /^>/ {split($0,a,/[ _\/]/); print a[1]; next} {print}' "$1" > "$2"
}

# One sequence per header, longest wins. Same intent as the HPC script, but
# stream-based so it copes with the multi-line FASTA that sharkmer emits.
#
# Each record must be closed off before its length is compared. A sample
# routinely has SEVERAL records for one locus -- MITOS splits cox1 into cox1_0
# and cox1_1, and sharkmer emits one record per product -- and after
# trim_headers they all carry the same bare sample ID. Accumulating sequence
# across records instead of within one concatenates them into a chimera.
select_longest () {
    awk '
        function flush() {
            if (cur == "") return
            if (!(cur in best) || length(seq) > length(best[cur])) best[cur] = seq
            if (!(cur in seen)) { seen[cur] = 1; order[++n] = cur }
        }
        /^>/ { flush(); cur = substr($0,2); seq = ""; next }
        { seq = seq $0 }
        END {
            flush()
            for (i = 1; i <= n; i++) print ">" order[i] ORS best[order[i]]
        }
    ' "$1" > "$2"
}

# Guard against the failure above recurring silently: a chimera is invisible in
# record counts and only shows up as an implausible sequence length.
check_lengths () {
    local file="$1" locus="$2" lo hi
    case "$locus" in
        16S) lo=300;  hi=2000 ;;
        18S) lo=900;  hi=2500 ;;
        CO1) lo=400;  hi=2000 ;;
        ITS) lo=150;  hi=1500 ;;
        *)   return 0 ;;
    esac
    awk -v lo="$lo" -v hi="$hi" -v locus="$locus" '
        function flush() {
            if (cur == "") return
            n++
            if (length(seq) < lo || length(seq) > hi) { bad++; if (bad <= 5) print "    OUT OF RANGE: " cur " (" length(seq) " bp)" }
            if (length(seq) > max) { max = length(seq); maxid = cur }
            if (min == 0 || length(seq) < min) { min = length(seq); minid = cur }
        }
        /^>/ { flush(); cur = substr($0,2); seq = ""; next }
        { seq = seq $0 }
        END {
            flush()
            printf "  length check    : n=%d  min=%d (%s)  max=%d (%s)  expected %d-%d\n", n, min, minid, max, maxid, lo, hi
            if (bad > 0) printf "  *** WARNING: %d sequence(s) outside the expected range for %s ***\n", bad, locus
        }
    ' "$file"
}

# Samples excluded from the gene trees by name. Empty by default: the only
# sample that needed removing (SEA2025-Ph277) now drops out structurally via
# MITOS_SOURCE_EXCLUDE above. Retained as a hook.
EXCLUDE_GENE_TREES="${EXCLUDE_GENE_TREES:-}"

keep_study_samples () {
    awk -v excl="$EXCLUDE_GENE_TREES" '
        BEGIN { n = split(excl, e, /[ ,]+/); for (i = 1; i <= n; i++) if (e[i] != "") drop[e[i]] }
        NR == FNR { if (!($1 in drop)) keep[$1]; next }
        /^>/ { p = (substr($0,2) in keep) } p' "$ID_FILE" "$1" > "$2"
}

build_locus () {
    local locus="$1" spec mitos_gene shark_gene
    eval "spec=\${LOCI_${locus}}"
    mitos_gene="${spec%%:*}"
    shark_gene="${spec##*:}"

    echo "=============================================================="
    echo "  $locus   (MITOS: $mitos_gene   sharkmer: $shark_gene)"
    echo "=============================================================="

    local w="$OUT_DIR/work_$locus"
    mkdir -p "$w"

    : > "$w/raw.fasta"
    if [ "$mitos_gene" != "NONE" ]; then
        gather_mitos "$mitos_gene" "$w/mitos.fasta"
        echo "  MITOS records   : $(grep -c '^>' "$w/mitos.fasta" || true)"
        cat "$w/mitos.fasta" >> "$w/raw.fasta"
    fi
    gather_shark "$shark_gene" "$w/shark.fasta"
    echo "  sharkmer records: $(grep -c '^>' "$w/shark.fasta" || true)"
    cat "$w/shark.fasta" >> "$w/raw.fasta"

    trim_headers      "$w/raw.fasta"      "$w/head.fasta"
    select_longest    "$w/head.fasta"     "$w/longest.fasta"
    keep_study_samples "$w/longest.fasta" "$w/study.fasta"
    echo "  study samples   : $(grep -c '^>' "$w/study.fasta" || true)"
    check_lengths "$w/study.fasta" "$locus"

    local ext="$EXT_DIR/${locus}.external.fasta"
    if [ -s "$ext" ]; then
        cat "$w/study.fasta" "$ext" > "$OUT_DIR/${locus}.all.fasta"
        echo "  + external      : $(grep -c '^>' "$ext")"
    else
        cp "$w/study.fasta" "$OUT_DIR/${locus}.all.fasta"
        echo "  + external      : none"
    fi
    echo "  total tips      : $(grep -c '^>' "$OUT_DIR/${locus}.all.fasta")"

    echo "  --> mafft"
    mafft --thread -1 --adjustdirectionaccurately --auto \
          "$OUT_DIR/${locus}.all.fasta" > "$OUT_DIR/${locus}.aln.fasta" 2> "$OUT_DIR/${locus}.mafft.log"
    # MAFFT flags reverse-complemented records with _R_; drop the marker so tip
    # labels stay equal to sample IDs.
    sed -i '' -E 's/^>_R_/>/' "$OUT_DIR/${locus}.aln.fasta"

    echo "  --> iqtree"
    "$IQTREE" -s "$OUT_DIR/${locus}.aln.fasta" -B 1000 -nt "$THREADS" \
        > "$OUT_DIR/${locus}.iqtree.stdout" 2>&1

    echo "  done: $OUT_DIR/${locus}.aln.fasta.treefile"
}

loci=("$@")
[ ${#loci[@]} -eq 0 ] && loci=(16S 18S CO1 ITS)
for l in "${loci[@]}"; do build_locus "$l"; done

echo
echo "All requested loci complete. Results in $OUT_DIR/"
