#!/usr/bin/env python
"""Assemble the GenBank submission bundle for the study's original gene sequences.

Scope is deliberately narrow: only sequences that are not already public. The
cox1 and rrnL of the 168 deposited mitogenomes are already in GenBank inside
those records, so resubmitting them would duplicate existing data. What remains:

  18S  181  nuclear, never submitted
  ITS  186  nuclear, never submitted
  CO1    9  samples with no deposited mitogenome
  16S    6  samples with no deposited mitogenome

Output mirrors the layout of Physalia_mitogenomes_GenBank_submission/, one set
of files per locus, so each locus can be submitted separately -- they have
different annotation shapes and NCBI's wizard branches on that.

Run from the phylogenetics/ directory.
"""
import argparse
import csv
import os
import re
import sys
import unicodedata

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))   # phylogenetics/
REPO = os.path.abspath(os.path.join(ROOT, ".."))        # repository root
WORK = os.path.abspath(os.path.join(REPO, ".."))        # working folder above it
# Metadata from the mitogenome GenBank submission. Vendored here so this script
# depends only on files in this repository; the full submission bundle, including
# the sequence and annotation files, is not distributed.
SUB = os.path.join(ROOT, "mitogenome_submission")

ap = argparse.ArgumentParser()
ap.add_argument("--out", default=os.path.join(WORK, "Physalia_genes_GenBank_submission"))
args = ap.parse_args()

ACC = re.compile(r"^[A-Z]{1,2}[0-9_]+\.\d")

# Excluded from the mitochondrial gene submissions (CO1, 16S). Their cox1 and
# rrnL are derived from mitogenome assemblies that are not being released, so
# releasing the genes from them would publish annotation we have withheld.
#
# YPM-IZ-104465  its mitogenome is already public as OQ957220.1 (2023). Our cox1
#                is 100% identical to that record's and rrnL 99.94%, so these
#                would be duplicates of existing GenBank data.
# YPM-IZ-111760  the two longest Illumina assemblies, ranks 1 and 2 of 171, each
# YPM-IZ-110972  ~420 bp over the 15037 bp modal length, with annotation tables
#                that do not behave like the rest. Deliberately withheld from the
#                mitogenome submission; see issue #17.
MITO_GENE_EXCLUDE = {"YPM-IZ-104465", "YPM-IZ-111760", "YPM-IZ-110972"}
# Only the nuclear loci are submitted.
#
# The mitochondrial loci were prepared and then withdrawn. After excluding the
# samples whose mitogenomes are already public or deliberately withheld, all
# that remained were the SEA2025 cruise samples -- and their mitochondrial data
# does not meet the bar in either form. Their ONT mitogenome assemblies were not
# good enough to deposit, and the sharkmer COI barcodes are no better: four of
# six needed codon_start 2 to translate at all, and two carried internal stop
# codons in every frame under translation table 4.
#
# Species identification for those samples therefore rests on the mitogenome
# identification tree, which is provided in this repository as alignment and
# treefiles rather than as GenBank records. The SEA2025 samples are represented
# in GenBank through ITS and 18S, which are clean.
LOCI = ("18S", "ITS")
MITO_LOCI = {"16S", "CO1"}   # retained for the exclusion logic below

# Free-text region names for the open-ocean cruise samples, which have no
# country. NCBI accepts ocean names in geo_loc_name.
OCEAN_GEO = {"SW Pacific": "Pacific Ocean", "Central Pacific": "Pacific Ocean",
             "SE Pacific": "Pacific Ocean", "NW Pacific": "Pacific Ocean",
             "NE Atlantic": "Atlantic Ocean", "NW Atlantic": "Atlantic Ocean",
             "SW Atlantic": "Atlantic Ocean", "SE Atlantic": "Atlantic Ocean",
             "E Indian": "Indian Ocean", "W Indian": "Indian Ocean"}


def read_fasta(path):
    seqs, cur = {}, None
    for line in open(path):
        line = line.rstrip("\n")
        if line.startswith(">"):
            cur = line[1:].split()[0]
            seqs[cur] = []
        elif cur is not None:
            seqs[cur].append(line.strip())
    return {k: "".join(v).replace("-", "") for k, v in seqs.items()}


def read_tsv(path):
    with open(path, errors="replace") as fh:
        return list(csv.DictReader(fh, delimiter="\t"))


def ascii_fold(v):
    """Strip diacritics; NCBI's submission portal rejects non-ASCII outright."""
    if not v:
        return v
    out = unicodedata.normalize("NFKD", v).encode("ascii", "ignore").decode("ascii")
    return out


def to_iso(d):
    """5/14/2023 -> 2023-05-14; pass through anything already ISO."""
    d = (d or "").strip()
    if not d:
        return ""
    if re.match(r"^\d{4}-\d{2}-\d{2}$", d):
        return d
    m = re.match(r"^(\d{1,2})/(\d{1,2})/(\d{4})$", d)
    if m:
        mo, da, yr = m.groups()
        return f"{yr}-{int(mo):02d}-{int(da):02d}"
    return d


def to_latlon(v):
    """'-44.379, -174.7119' -> '44.379000 S 174.711900 W'."""
    v = (v or "").strip()
    if not v or re.search(r"[NSEW]", v):
        return v
    m = re.match(r"^\s*(-?[\d.]+)\s*,\s*(-?[\d.]+)\s*$", v)
    if not m:
        return ""
    lat, lon = float(m.group(1)), float(m.group(2))
    return (f"{abs(lat):.6f} {'N' if lat >= 0 else 'S'} "
            f"{abs(lon):.6f} {'E' if lon >= 0 else 'W'}")


# ---------------------------------------------------------------- metadata ---
deposited = {l.strip() for l in open(os.path.join(SUB, "Physalia_mitogenomes_IDs.txt")) if l.strip()}

species = {}
for line in open(os.path.join(SUB, "Physalia_mitogenomes_species.tsv"), errors="replace"):
    p = line.rstrip("\n").split("\t")
    if len(p) > 1:
        species[p[0].replace("_annotation", "")] = p[1]
ont_path = os.path.join(ROOT, "ont_species_assignments.tsv")
if os.path.exists(ont_path):
    for r in read_tsv(ont_path):
        species.setdefault(r["sample"], r["species"])
# confirmed against both the COI and mitogenome trees
species.setdefault("YPM-IZ-104465", "Physalia utriculus")
species.setdefault("YPM-IZ-110972", "Physalia physalis")
species.setdefault("YPM-IZ-111760", "Physalia minuta")

# The five Saint Helena specimens used to be absent from the specimen tables, so
# their collector was written in here as "Adam Riggs, Casey Dunn" for all five.
# They are in data/sample_ids.tsv now, and the table disagrees: 115977 and 115978
# were collected by Adam Riggs on 2025-09-24, and 115980, 115990 and 116019 by
# Casey W. Dunn in January 2026. The lookup is correct, so the override is gone.
# See issue #1.

# collection metadata, in priority order
mito_src = {r["Sequence_ID"].replace("_annotation", ""): r
            for r in read_tsv(os.path.join(SUB, "Physalia_mitogenomes_sourcemodifiers_accessioned.tsv"))}
tables = {}
for path in (os.path.join(REPO, "data", "sample_ids.tsv"), os.path.join(REPO, "data", "SEA.tsv")):
    for r in read_tsv(path):
        sid = (r.get("ID") or "").strip()
        if sid:
            tables.setdefault(sid, r)


def source_row(sid):
    m = mito_src.get(sid, {})
    t = tables.get(sid, {})
    geo = (m.get("Country (geo_loc_name)") or m.get("geo_loc_name") or "").strip()
    if not geo:
        loc = (t.get("location") or "").strip()
        ocean = (t.get("ocean") or "").strip()
        geo = OCEAN_GEO.get(ocean, ocean or loc)
    return {
        "Sequence_ID": sid,
        "Organism": species.get(sid, ""),
        "Collection_date": to_iso(m.get("Collection_date") or t.get("date_collected")),
        "geo_loc_name": geo,
        "Isolate": sid,
        "Isolation_source": (m.get("Isolation_source") or "ocean").strip(),
        "Tissue_type": (m.get("Tissue_type") or "tentacle").strip(),
        # Prefer the signed decimals in the specimen tables and convert them
        # here. The Lat_Lon strings in the mitogenome submission are taken only
        # as a fallback: several are wrong (see README), and one is malformed.
        "Lat_Lon": to_latlon(t.get("lat_long")) or to_latlon(m.get("Lat_Lon")),
        "Collected_by": (t.get("collector") or "").strip(),
    }


# ------------------------------------------------------------------ build ----
os.makedirs(args.out, exist_ok=True)
its_tbl = {}
tbl_path = os.path.join(ROOT, "ITS.tbl")
if os.path.exists(tbl_path):
    cur = None
    for line in open(tbl_path):
        if line.startswith(">Feature "):
            cur = line.split()[1].strip()
            its_tbl[cur] = []
        elif cur:
            its_tbl[cur].append(line.rstrip("\n"))

summary = []
for locus in LOCI:
    fa = read_fasta(os.path.join(ROOT, "gene_trees", f"{locus}.all.fasta"))
    ids = [k for k in fa if not ACC.match(k)]
    if locus in MITO_LOCI:
        ids = [k for k in ids if k not in deposited and k not in MITO_GENE_EXCLUDE]
    ids.sort()
    if not ids:
        continue

    with open(os.path.join(args.out, f"Physalia_{locus}.fasta"), "w") as fh:
        for sid in ids:
            fh.write(f">{sid}\n{fa[sid]}\n")

    rows = [{k: ascii_fold(v) for k, v in source_row(sid).items()} for sid in ids]
    cols = ["Sequence_ID", "Organism", "Collection_date", "geo_loc_name", "Isolate",
            "Isolation_source", "Tissue_type", "Lat_Lon", "Collected_by"]
    with open(os.path.join(args.out, f"Physalia_{locus}_sourcemodifiers.tsv"), "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=cols, delimiter="\t", extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)
    with open(os.path.join(args.out, f"Physalia_{locus}_species.tsv"), "w", newline="") as fh:
        w = csv.writer(fh, delimiter="\t")
        w.writerow(["Sequence_ID", "Organism"])
        for r in rows:
            w.writerow([r["Sequence_ID"], r["Organism"]])

    # feature tables
    if locus == "ITS":
        with open(os.path.join(args.out, "Physalia_ITS.tbl"), "w") as fh:
            for sid in ids:
                if sid in its_tbl:
                    fh.write(f">Feature {sid}\n" + "\n".join(its_tbl[sid]) + "\n")
    else:
        product = {"16S": ("rRNA", "16S ribosomal RNA"),
                   "18S": ("rRNA", "18S ribosomal RNA"),
                   "CO1": ("CDS", "cytochrome c oxidase subunit I")}[locus]
        with open(os.path.join(args.out, f"Physalia_{locus}.tbl"), "w") as fh:
            for sid in ids:
                n = len(fa[sid])
                fh.write(f">Feature {sid}\n")
                fh.write(f"<1\t>{n}\t{product[0]}\n")
                fh.write(f"\t\t\tproduct\t{product[1]}\n")
                # transl_table is deliberately not declared. The mitogenome
                # submission omits it too -- translation table 4 is applied from
                # the organism's taxonomy rather than stated in the table.

    miss = {c: sum(1 for r in rows if not r[c]) for c in cols if c != "Sequence_ID"}
    miss = {k: v for k, v in miss.items() if v}
    summary.append((locus, len(ids), miss))

print(f"bundle written to {args.out}\n")
print(f"{'locus':6s} {'records':>8s}  gaps")
for locus, n, miss in summary:
    print(f"{locus:6s} {n:8d}  {miss or 'none'}")
print(f"\ntotal: {sum(n for _, n, _ in summary)} records")
