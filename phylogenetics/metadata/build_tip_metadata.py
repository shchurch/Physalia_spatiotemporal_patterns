#!/usr/bin/env python
"""Assemble per-tip metadata for the tree figures.

Produces tip_metadata.tsv with one row per tip label that can appear in any of
the trees: ocean of origin, a display label, species, and provenance. Ocean is
the variable the supplemental figures colour by, following the convention of
the gene-tree figures in the previous manuscript.

Sources, in priority order for study samples:
  data/sample_ids.tsv            most sequenced specimens
  data/SEA.tsv                   the 2025 cruise samples
  GenBank source modifiers       the five Saint Helena samples, which are absent
                                 from both tables (see issue #1)

For GenBank accessions the previous manuscript's data/NCBI_data.tsv supplies
most oceans; the remainder are assigned here from the lat_lon on the record.
Rhizophysa outgroups are deliberately left blank, as in the previous figures.
"""
import csv
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))   # phylogenetics/
REPO = os.path.abspath(os.path.join(ROOT, ".."))        # repository root
WORK = os.path.abspath(os.path.join(REPO, ".."))        # working folder above it
# The previous study's curated GenBank table, kept here so this script depends
# only on files in this repository. Source:
# https://github.com/shchurch/Physalia_population_genomics
PREV = os.path.join(ROOT, "previous_study_NCBI_data.tsv")
SUB = os.path.join(WORK, "Physalia_mitogenomes_GenBank_submission")

# GenBank locality -> ocean. NCBI renamed the `country` source qualifier to
# `geo_loc_name`; reading only `country` makes most records look unlocalised,
# which is why these are keyed on the current field. Values are the literal
# geo_loc_name strings returned by Entrez.
GEO_TO_OCEAN = {
    "New Zealand": "SW Pacific",
    "Australia": "SW Pacific",
    "Japan: Miyagi Prefecture, Sendai, Sendai Bay": "NW Pacific",
    "Japan: Okinawa Prefecture, Kunigami District, Onna": "NW Pacific",
    "USA: Florida, Gulf Stream, 6-7 miles east of Palm Beach": "NW Atlantic",
    "USA: Hawaii, Oahu, Kaneohe Bay, NW of Sand Island": "Central Pacific",
    "India: Alappuzha, Kerala": "W Indian",
    "Pakistan": "W Indian",
    # Basin-level only; left unassigned rather than guessed at a sub-basin.
    "Pacific Ocean": "",
    "Atlantic Ocean": "",
}

# Records with no geo qualifier at all. Ten are Rhizophysa outgroups, which are
# uncoloured by convention. The remaining ten are older Physalia submissions
# (2001-2015) whose localities exist only in their source publications and would
# have to be curated by hand -- see issue #20.
NO_LOCALITY_NOTE = "no geo_loc_name on the GenBank record"

# The Saint Helena specimens (16.003892 S, 5.714589 W) need an ocean category
# the previous manuscript did not use.
ST_HELENA_OCEAN = "S Atlantic"

rows = {}


def add(seq, ocean="", location="", species="", source=""):
    if seq in rows and rows[seq]["ocean"]:
        return
    rows[seq] = {"seq": seq, "ocean": ocean, "location": location,
                 "species": species, "source": source}


def read_tsv(path):
    with open(path, errors="replace") as fh:
        for r in csv.DictReader(fh, delimiter="\t"):
            yield r


# --- species calls -----------------------------------------------------------
species = {}
with open(os.path.join(SUB, "Physalia_mitogenomes_species.tsv"), errors="replace") as fh:
    for line in fh:
        p = line.rstrip("\n").split("\t")
        if len(p) > 1:
            species[p[0].replace("_annotation", "")] = p[1]

ont = os.path.join(ROOT, "ont_species_assignments.tsv")
if os.path.exists(ont):
    for r in read_tsv(ont):
        species.setdefault(r["sample"], r["species"])

# The two sources above are the 168 deposited records and the 28 Nanopore
# samples: 196 of the 199 sequenced. The remaining three are exactly the samples
# excluded from the GenBank submission, so they fall through both and would
# otherwise carry no species at all.
#
#   YPM-IZ-111760, YPM-IZ-110972  withheld from deposition, see issue #17
#   YPM-IZ-104465                 already public as OQ957220.1, which is itself
#                                 deposited as "Physalia sp." under the same
#                                 voucher and so supplies no species call
#
# Assigned by the rule the manuscript applies to the Nanopore samples: the
# majority species of the smallest clade in the mitochondrial genome tree
# containing the sample and at least one identified sample. All three are
# unanimous, and each agrees with the specimen's locality -- Ireland for
# physalis, Guam for utriculus, the SW Pacific for minuta.
NOT_DEPOSITED_SPECIES = {
    "YPM-IZ-111760": "Physalia minuta",
    "YPM-IZ-110972": "Physalia physalis",
    "YPM-IZ-104465": "Physalia utriculus",
}
for _sid, _sp in NOT_DEPOSITED_SPECIES.items():
    species.setdefault(_sid, _sp)

# --- study samples -----------------------------------------------------------
for path in (os.path.join(REPO, "data", "sample_ids.tsv"),
             os.path.join(REPO, "data", "SEA.tsv")):
    for r in read_tsv(path):
        sid = (r.get("ID") or "").strip()
        if sid:
            add(sid, (r.get("ocean") or "").strip(), (r.get("location") or "").strip(),
                species.get(sid, ""), "study")

# Saint Helena: present in the submission but in neither specimen table.
sm = os.path.join(SUB, "Physalia_mitogenomes_sourcemodifiers_accessioned.tsv")
for r in read_tsv(sm):
    sid = (r.get("Specimen_voucher") or "").strip()
    country = ""
    for k, v in r.items():
        if v and v.strip() == "Saint Helena":
            country = "Saint Helena"
    name = list(r.values())[0].replace("_annotation", "") if r else ""
    if country and name:
        add(name, ST_HELENA_OCEAN, "Saint Helena", species.get(name, ""), "study")

# --- GenBank accessions ------------------------------------------------------
if os.path.exists(PREV):
    for r in read_tsv(PREV):
        acc = (r.get("accession") or "").strip()
        if not acc:
            continue
        org = (r.get("organism") or "").strip()
        ocean = (r.get("ocean") or "").strip()
        if "Rhizo" in org:
            ocean = ""
        add(acc, ocean, (r.get("country") or "").strip(), org, "genbank")
        add(acc.split(".")[0], ocean, (r.get("country") or "").strip(), org, "genbank")

# Localities pulled from Entrez, cached alongside this script so the table can be
# rebuilt without network access.
GEO_CACHE = os.path.join(ROOT, "external_seqs", "genbank_localities.tsv")
geo = {}
if os.path.exists(GEO_CACHE):
    for r in read_tsv(GEO_CACHE):
        geo[r["accession"]] = r

# accessions in our external sets, including ones the previous table lacks
EXT = os.path.join(ROOT, "external_seqs")
for g in ("16S", "18S", "CO1", "ITS"):
    f = os.path.join(EXT, f"{g}.external.fasta")
    if not os.path.exists(f):
        continue
    for line in open(f):
        if not line.startswith(">"):
            continue
        parts = line[1:].split()
        acc = parts[0]
        org = " ".join(parts[1:3]) if len(parts) > 2 else ""
        base = acc.split(".")[0]
        gi = geo.get(acc, {})
        loc = gi.get("geo_loc_name", "")
        ocean = GEO_TO_OCEAN.get(loc, "")
        # fall back to the previous manuscript's curated table
        if not ocean and base in rows and rows[base]["ocean"]:
            ocean = rows[base]["ocean"]
        if "Rhizophysa" in org:
            ocean = ""
            loc = org
        add(acc, ocean, loc, org, "genbank")

out = os.path.join(ROOT, "tip_metadata.tsv")
with open(out, "w", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=["seq", "ocean", "location", "species", "source"],
                       delimiter="\t")
    w.writeheader()
    for k in sorted(rows):
        w.writerow(rows[k])

import collections
print(f"{len(rows)} tip records written to {out}")
print("\nocean distribution:")
for k, v in collections.Counter(r["ocean"] or "(none)" for r in rows.values()).most_common():
    print(f"   {k:16s} {v}")
