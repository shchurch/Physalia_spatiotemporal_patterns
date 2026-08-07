#!/usr/bin/env python
"""Assign species to the ONT (SEA2025) samples from the identification tree.

The SEA2025 cruise samples are ONT and their assemblies are not deposited, so
they are absent from the 168-sample deposited mitogenome tree. Their species
identity -- which Figure 7 depends on -- comes instead from their placement in
the 199-sample identification tree, among the Illumina samples whose species
are known from the GenBank submission.

For each ONT sample this walks up from the tip to the smallest ancestral clade
containing at least one identified sample, and reports the majority species of
that clade together with the clade size and its bootstrap support. Support is
reported so that weakly-placed samples can be shown differently, or dropped,
in the figure.

Usage:
    python assign_ont_species.py [--tree ...] [--species ...] [--out ...]
"""
import argparse
import collections
import csv
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))   # phylogenetics/
REPO = os.path.abspath(os.path.join(ROOT, ".."))        # repository root
WORK = os.path.abspath(os.path.join(REPO, ".."))        # working folder above it

ap = argparse.ArgumentParser()
ap.add_argument("--tree", default=os.path.join(ROOT, "mitogenome_tree", "identification.contree"))
ap.add_argument("--species", default=os.path.join(
    ROOT, "mitogenome_submission", "Physalia_mitogenomes_species.tsv"))
ap.add_argument("--pattern", default="SEA2025", help="substring identifying the ONT samples")
ap.add_argument("--out", default=os.path.join(ROOT, "ont_species_assignments.tsv"))
args = ap.parse_args()


def parse_newick(nw):
    nw = nw.strip().rstrip(";")
    pos = 0

    def node():
        nonlocal pos
        if nw[pos] == "(":
            pos += 1
            children = []
            while True:
                children.append(node())
                if nw[pos] == ",":
                    pos += 1
                else:
                    break
            pos += 1
            m = re.match(r"[^,()]*", nw[pos:])
            label = m.group(0)
            pos += len(label)
            return (label.split(":")[0], children)
        m = re.match(r"[^,()]*", nw[pos:])
        label = m.group(0)
        pos += len(label)
        return (label.split(":")[0], None)

    return node()


def leaves(n):
    return [n[0]] if n[1] is None else [l for c in n[1] for l in leaves(c)]


def sample_id(tip):
    """Strip MAFFT's _R_ direction marker and the _mtgenome_<contig> suffix."""
    return re.sub(r"_(mtgenome|mito)_.*$", "", re.sub(r"^_R_", "", tip))


species = {}
with open(args.species) as fh:
    for line in fh:
        parts = line.rstrip("\n").split("\t")
        if len(parts) > 1:
            species[parts[0].replace("_annotation", "")] = parts[1]

tree = parse_newick(open(args.tree).read())

rows = []


def walk(node, ancestors):
    if node[1] is None:
        sid = sample_id(node[0])
        if args.pattern not in sid:
            return
        for anc in reversed(ancestors):
            ids = [species[sample_id(t)] for t in leaves(anc) if sample_id(t) in species]
            if not ids:
                continue
            counts = collections.Counter(ids)
            top, n_top = counts.most_common(1)[0]
            support = anc[0] if anc[0] not in ("", None) else "NA"
            rows.append({
                "sample": sid,
                "species": top,
                "clade_size": len(leaves(anc)),
                "support": support,
                "identified_in_clade": len(ids),
                "unanimous": "yes" if len(counts) == 1 else "no",
            })
            return
        rows.append({"sample": sid, "species": "UNASSIGNED", "clade_size": "NA",
                     "support": "NA", "identified_in_clade": 0, "unanimous": "NA"})
        return
    for child in node[1]:
        walk(child, ancestors + [node])


walk(tree, [])
rows.sort(key=lambda r: r["sample"])

with open(args.out, "w", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()), delimiter="\t")
    w.writeheader()
    w.writerows(rows)

counts = collections.Counter(r["species"] for r in rows)
print(f"{len(rows)} {args.pattern} samples assigned from {os.path.basename(args.tree)}")
for k, v in counts.most_common():
    print(f"   {k:22s} {v}")
weak = [r for r in rows if r["support"] not in ("NA",) and str(r["support"]).isdigit() and int(r["support"]) < 90]
if weak:
    print(f"\n   {len(weak)} assignment(s) below 90% bootstrap:")
    for r in weak:
        print(f"     {r['sample']:16s} {r['species']:20s} support={r['support']} clade_size={r['clade_size']}")
print(f"\nwritten to {args.out}")
