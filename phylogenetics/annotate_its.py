#!/usr/bin/env python
"""Locate the rRNA/ITS boundaries in the study's ITS sequences.

The sharkmer ITS amplicon spans partial 18S -> ITS1 -> 5.8S -> ITS2 -> partial
28S. GenBank wants those internal boundaries annotated, so each region has to be
delimited per sequence.

ITSx and HMMER are not available here, so the conserved genes are located by
local (Smith-Waterman) alignment of three probes -- the 3' end of 18S, a full
5.8S, and the 5' end of 28S -- against each sequence in turn. The spacers are
then whatever falls between them, which is the same principle ITSx applies.

Local alignment rather than MAFFT: the probes are short regions of homology
inside a longer amplicon, and a global aligner smears them across the whole
alignment instead of pinning them to the homologous block.

Outputs:
  its_boundaries.tsv   per-sequence coordinates for each region
  ITS.tbl              GenBank feature table (5-column, for table2asn)

Requires biopython. Run from this directory.
"""
import argparse
import collections
import os
import re
import sys

from Bio import Align

HERE = os.path.dirname(os.path.abspath(__file__))

ap = argparse.ArgumentParser()
ap.add_argument("--its", default=os.path.join(HERE, "gene_trees", "ITS.all.fasta"))
ap.add_argument("--probe-18s", default="/tmp/18S_probe_trim.fasta")
ap.add_argument("--probe-58s", default="/tmp/58S_probe.fasta")
ap.add_argument("--probe-28s", default="/tmp/28S_probe_trim.fasta")
ap.add_argument("--out-tsv", default=os.path.join(HERE, "its_boundaries.tsv"))
ap.add_argument("--out-tbl", default=os.path.join(HERE, "ITS.tbl"))
ap.add_argument("--min-score", type=float, default=60.0,
                help="minimum local alignment score for a probe hit to be trusted")
ap.add_argument("--min-28s", type=int, default=50,
                help="minimum length of 28S that must be present to annotate it; "
                     "below this ITS2 is simply reported as running to the 3' end")
args = ap.parse_args()

ACC = re.compile(r"^[A-Z]{1,2}[0-9_]+\.\d")


def read_fasta(path):
    seqs, cur = {}, None
    for line in open(path):
        line = line.rstrip("\n")
        if line.startswith(">"):
            cur = line[1:].split()[0]
            seqs[cur] = []
        elif cur is not None:
            seqs[cur].append(line.strip())
    return {k: "".join(v).upper().replace("-", "") for k, v in seqs.items()}


its = read_fasta(args.its)
study = {k: v for k, v in its.items() if not ACC.match(k)}
print(f"{len(study)} study ITS sequences ({len(its) - len(study)} GenBank records skipped)")

probes = {}
for tag, path in (("18S", args.probe_18s), ("5.8S", args.probe_58s), ("28S", args.probe_28s)):
    if not os.path.exists(path):
        sys.exit(f"ERROR: probe missing: {path}")
    seq = next(iter(read_fasta(path).values()))
    probes[tag] = seq
    print(f"  probe {tag:5s} {len(seq)} bp")

aligner = Align.PairwiseAligner(scoring="blastn", mode="local")


def locate(probe, target):
    """Return (start, end) 1-based inclusive of the best local hit, and its score."""
    try:
        aln = aligner.align(target, probe)
        best = aln[0]
    except Exception:
        return None, None, 0.0
    blocks = best.aligned[0]
    if len(blocks) == 0:
        return None, None, 0.0
    return int(blocks[0][0]) + 1, int(blocks[-1][1]), float(best.score)


REGIONS = [
    ("18S ribosomal RNA", "rRNA"),
    ("internal transcribed spacer 1", "misc_RNA"),
    ("5.8S ribosomal RNA", "rRNA"),
    ("internal transcribed spacer 2", "misc_RNA"),
    ("28S ribosomal RNA", "rRNA"),
]

rows, tbl, warn = [], [], []
for sid in sorted(study):
    seq = study[sid]
    hits = {}
    for tag, probe in probes.items():
        s, e, sc = locate(probe, seq)
        hits[tag] = (s, e, sc) if (s and sc >= args.min_score) else (None, None, sc)

    h18, h58, h28 = hits["18S"], hits["5.8S"], hits["28S"]
    # The amplicon runs 18S -> 5.8S -> 28S; anything else means a bad hit.
    ok = h18[0] and h58[0] and h18[1] < h58[0]
    if not ok:
        warn.append((sid, {k: (v[0], v[1], round(v[2])) for k, v in hits.items()}))

    coords = {}
    if h18[0]:
        coords["18S ribosomal RNA"] = (1, h18[1])          # runs off the 5' end
    if h18[0] and h58[0] and h58[0] > h18[1] + 1:
        coords["internal transcribed spacer 1"] = (h18[1] + 1, h58[0] - 1)
    if h58[0]:
        coords["5.8S ribosomal RNA"] = (h58[0], h58[1])
    # The ITS amplicon and the sharkmer 28S amplicon share a 3' primer site, so
    # only a few bases of 28S are ever present -- too short to annotate as a
    # gene. Annotate 28S only if a real stretch of it is there; otherwise ITS2
    # simply runs to the 3' end and is reported partial.
    has28 = h28[0] and (len(seq) - h28[0] + 1) >= args.min_28s
    if h58[0]:
        if has28:
            coords["internal transcribed spacer 2"] = (h58[1] + 1, h28[0] - 1)
            coords["28S ribosomal RNA"] = (h28[0], len(seq))
        else:
            coords["internal transcribed spacer 2"] = (h58[1] + 1, len(seq))

    row = {"sequence": sid, "length": len(seq), "ok": "yes" if ok else "CHECK"}
    for label, _ in REGIONS:
        c = coords.get(label)
        row[label.replace(" ", "_")] = f"{c[0]}..{c[1]}" if c else ""
    rows.append(row)

    tbl.append(f">Feature {sid}")
    for label, ftype in REGIONS:
        c = coords.get(label)
        if not c:
            continue
        lo = f"<{c[0]}" if label.startswith("18S") else str(c[0])
        hi = (f">{c[1]}" if (label.startswith("28S") or
              (label.startswith("internal transcribed spacer 2") and c[1] == len(seq)))
              else str(c[1]))
        tbl.append(f"{lo}\t{hi}\t{ftype}")
        tbl.append(f"\t\t\tproduct\t{label}")

hdr = ["sequence", "length", "ok"] + [r[0].replace(" ", "_") for r in REGIONS]
with open(args.out_tsv, "w") as fh:
    fh.write("\t".join(hdr) + "\n")
    for r in rows:
        fh.write("\t".join(str(r.get(h, "")) for h in hdr) + "\n")
with open(args.out_tbl, "w") as fh:
    fh.write("\n".join(tbl) + "\n")

print(f"\n{len(rows)} sequences annotated -> {os.path.basename(args.out_tsv)}, "
      f"{os.path.basename(args.out_tbl)}")
for h in hdr[3:]:
    n = sum(1 for r in rows if r.get(h))
    print(f"   {h:32s} called in {n}/{len(rows)}")

for label in ("5.8S_ribosomal_RNA", "internal_transcribed_spacer_1", "internal_transcribed_spacer_2"):
    lens = []
    for r in rows:
        v = r.get(label, "")
        if v:
            a, b = v.split("..")
            lens.append(int(b) - int(a) + 1)
    if lens:
        print(f"   {label:32s} length min/median/max = "
              f"{min(lens)}/{sorted(lens)[len(lens)//2]}/{max(lens)}")

if warn:
    print(f"\n*** {len(warn)} sequence(s) with out-of-order or missing probe hits ***")
    for sid, h in warn[:10]:
        print(f"   {sid:18s} {h}")
