#!/usr/bin/env python
"""Add a Specimen_voucher column to both GenBank submission bundles.

The sample IDs are already carried as `isolate`, which is what the deposited
mitogenomes use and what keeps the gene records linkable to them. Most of those
IDs are also museum catalogue numbers, though, and NCBI prefers those in
`specimen_voucher` using the institution:collection:id form. This adds the
column alongside `Isolate` rather than replacing it.

Not every sample is vouchered: the SEA2025 cruise samples and the OA field
collections have no museum accession, and their rows are left blank rather than
given an invented voucher.

Precedent for the format: the Rhizophysa mitogenomes deposited by this lab use
`YPM:IZ:35347` (OQ957199.1).

Writes new files rather than editing the submitted ones -- the mitogenome
records are already accessioned, so adding this column there is an update
request to NCBI, not a local edit.
"""
import argparse
import csv
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))   # phylogenetics/
REPO = os.path.abspath(os.path.join(ROOT, ".."))        # repository root
WORK = os.path.abspath(os.path.join(REPO, ".."))        # working folder above it
MITO = os.path.join(WORK, "Physalia_mitogenomes_GenBank_submission")
GENES = os.path.join(WORK, "Physalia_genes_GenBank_submission")

ap = argparse.ArgumentParser()
ap.add_argument("--mito-dir", default=MITO)
ap.add_argument("--genes-dir", default=GENES)
args = ap.parse_args()

# institution:collection:id, per NCBI's preferred voucher form.
#   YPM-IZ-115977 -> YPM:IZ:115977      Yale Peabody Museum, Invertebrate Zoology
#   WAM-Z97900    -> WAM:Z97900         Western Australian Museum
#   TMAG-K5632    -> TMAG:K5632         Tasmanian Museum and Art Gallery
#   NIWA-173304   -> NIWA:173304        NIWA Invertebrate Collection
#   FM-16644      -> FMNH:16644         Field Museum of Natural History
#
# Deliberately NOT vouchered:
#   SEA2025-*  cruise samples, no museum accession
#   OA-*       field collections, no museum accession
RULES = [
    (re.compile(r"^YPM-IZ-(\d+)$"),   lambda m: f"YPM:IZ:{m.group(1)}"),
    (re.compile(r"^WAM-(Z?\d+)$"),    lambda m: f"WAM:{m.group(1)}"),
    (re.compile(r"^TMAG-(\S+)$"),     lambda m: f"TMAG:{m.group(1)}"),
    (re.compile(r"^NIWA-(\d+)$"),     lambda m: f"NIWA:{m.group(1)}"),
    (re.compile(r"^FM-(\d+)$"),       lambda m: f"FMNH:{m.group(1)}"),
]


def voucher(sample_id):
    for pat, fmt in RULES:
        m = pat.match(sample_id)
        if m:
            return fmt(m)
    return ""


def augment(path, out_path, id_col="Sequence_ID", strip_suffix="_annotation"):
    with open(path, errors="replace") as fh:
        rows = list(csv.DictReader(fh, delimiter="\t"))
    if not rows:
        return None
    cols = list(rows[0].keys())
    if "Specimen_voucher" not in cols:
        # place it next to Isolate, which it complements
        i = cols.index("Isolate") + 1 if "Isolate" in cols else len(cols)
        cols = cols[:i] + ["Specimen_voucher"] + cols[i:]
    n = 0
    for r in rows:
        sid = (r.get(id_col) or "").replace(strip_suffix, "").strip()
        v = voucher(sid)
        r["Specimen_voucher"] = v
        if v:
            n += 1
    with open(out_path, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=cols, delimiter="\t", extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)
    return len(rows), n


print("mitogenome submission (already accessioned -- these are update files):")
for name in ("Physalia_mitogenomes_sourcemodifiers.tsv",
             "Physalia_mitogenomes_sourcemodifiers_accessioned.tsv"):
    p = os.path.join(args.mito_dir, name)
    if not os.path.exists(p):
        print(f"   {name:58s} not found"); continue
    out = p.replace(".tsv", "_with_voucher.tsv")
    total, n = augment(p, out)
    print(f"   {os.path.basename(out):58s} {n}/{total} vouchered")

print("\ngene submission:")
for locus in ("16S", "18S", "CO1", "ITS"):
    p = os.path.join(args.genes_dir, f"Physalia_{locus}_sourcemodifiers.tsv")
    if not os.path.exists(p):
        continue
    total, n = augment(p, p)          # regenerable, so edit in place
    print(f"   Physalia_{locus}_sourcemodifiers.tsv{'':<24} {n}/{total} vouchered")

# what was left blank, and why
ids = set()
for locus in ("16S", "18S", "CO1", "ITS"):
    p = os.path.join(args.genes_dir, f"Physalia_{locus}_sourcemodifiers.tsv")
    if os.path.exists(p):
        with open(p, errors="replace") as fh:
            for r in csv.DictReader(fh, delimiter="\t"):
                if not r.get("Specimen_voucher"):
                    ids.add(r["Sequence_ID"])
if ids:
    import collections
    pre = collections.Counter(re.match(r"^([A-Za-z]+)", i).group(1) for i in ids)
    print(f"\nleft blank ({len(ids)} samples): {dict(pre)}")
