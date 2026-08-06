#!/usr/bin/env python
"""Refresh the external sequence sets for the Physalia gene trees.

Harvests Physalia and Rhizophysa records from GenBank and sorts them into
16S / 18S / CO1 / ITS. Excludes this study's own submissions (PZ*) and
whole-genome / assembly records, which would otherwise duplicate the study
samples or swamp the alignments.
"""
import re, sys, collections
from Bio import Entrez, SeqIO

Entrez.email = "churchevolutionlab@gmail.com"
OUT = sys.argv[1] if len(sys.argv) > 1 else "external_seqs_refresh"

# Our own submission; already represented by the 199 study samples.
OWN = re.compile(r'^PZ2244?\d\d')
# WGS / TSA / chromosome-level assemblies - not gene records.
ASSEMBLY = re.compile(r'^(CM|JBQPFL|JAWZSJ|JAWZSK|GHBB)')

# gene -> (feature-qualifier patterns, description fallback pattern)
GENES = {
    '16S': (re.compile(r'(?i)\b(16S|rrnL|l-?rRNA|large subunit ribosomal)'),
            re.compile(r'(?i)16S ribosomal')),
    '18S': (re.compile(r'(?i)\b(18S|rrnS.*nuclear|small subunit ribosomal)'),
            re.compile(r'(?i)18S ribosomal|small subunit ribosomal')),
    'CO1': (re.compile(r'(?i)(CO ?I\b|COX ?1|COX ?I\b|cytochrome c? ?oxidase subunit (1|I)\b)'),
            re.compile(r'(?i)cytochrome c? ?oxidase subunit (1|I)\b|COI')),
    'ITS': (re.compile(r'(?i)internal transcribed spacer|\bITS[12]?\b'),
            re.compile(r'(?i)internal transcribed spacer|\bITS[12]?\b')),
}
# 18S is nuclear SSU; mitochondrial rrnS must not leak in.
MITO_SSU = re.compile(r'(?i)rrnS|12S|s-?rRNA')
# "16S" here means the MITOCHONDRIAL large subunit (rrnL). The NUCLEAR large
# subunit is 28S and is a different gene entirely, but GenBank describes it as
# "28S large subunit ribosomal RNA" - which matches a bare "large subunit
# ribosomal" pattern. EU448095.1 (Physalia physalis 28S, 3093 bp) slipped into
# the 16S set this way and produced a 2.24 substitutions/site branch.
NUC_LSU = re.compile(r'(?i)\b28S\b|\bLSU\b')
# ITS records are described as "18S ... partial sequence; internal transcribed
# spacer 1, ...". Their 18S flank is a stub, not an 18S sequence - reject them
# from the 18S set, and require near-full-length for a description-only match.
ITS_RECORD = re.compile(r'(?i)internal transcribed spacer')
MIN_LEN = {'16S': 300, '18S': 900, 'CO1': 200, 'ITS': 200}


def fetch(organism):
    h = Entrez.esearch(db="nucleotide", term=f'"{organism}"[Organism]', retmax=10000)
    ids = Entrez.read(h)["IdList"]
    print(f"  {organism}: {len(ids)} records found")
    out = []
    for i in range(0, len(ids), 100):
        h = Entrez.efetch(db="nucleotide", id=ids[i:i+100], rettype="gb", retmode="text")
        out.extend(SeqIO.parse(h, "gb"))
        print(f"    fetched {min(i+100,len(ids))}/{len(ids)}")
    return out


def qualifier_text(feat):
    bits = []
    for k in ('gene', 'product', 'note', 'standard_name'):
        bits += feat.qualifiers.get(k, [])
    return " ".join(bits)


def classify(records, tag):
    hits = collections.defaultdict(list)
    for rec in records:
        acc = rec.annotations.get('accessions', [rec.id])[0]
        ver = rec.id
        if OWN.match(acc) or ASSEMBLY.match(acc):
            continue
        org = rec.annotations.get('organism', '')
        desc = rec.description
        claimed = set()
        # 1. annotated features
        for feat in rec.features:
            if feat.type in ('source',):
                continue
            qt = qualifier_text(feat)
            if not qt:
                continue
            for g, (pat, _) in GENES.items():
                if pat.search(qt):
                    if g == '18S' and MITO_SSU.search(qt):
                        continue
                    if g == '16S' and (NUC_LSU.search(qt) or NUC_LSU.search(desc)):
                        continue
                    try:
                        seq = feat.extract(rec.seq)
                    except Exception:
                        continue
                    if len(seq) < MIN_LEN[g]:
                        continue
                    if g == '18S' and ITS_RECORD.search(desc) and len(seq) < 1200:
                        continue
                    hits[g].append((f"{ver} {org} {tag}", str(seq)))
                    claimed.add(g)
        # 2. description fallback for records with no useful features (common for ITS)
        for g, (_, dpat) in GENES.items():
            if g in claimed:
                continue
            if dpat.search(desc) and MIN_LEN[g] < len(rec.seq) < 20000:
                if g == '18S' and (MITO_SSU.search(desc) or ITS_RECORD.search(desc)):
                    continue
                if g == '16S' and NUC_LSU.search(desc):
                    continue
                hits[g].append((f"{ver} {org} {tag}", str(rec.seq)))
    return hits


print("Fetching GenBank records...")
phys = fetch("Physalia")
rhiz = fetch("Rhizophysa")

allhits = collections.defaultdict(list)
for g, v in classify(phys, "Physalia").items():
    allhits[g] += v
for g, v in classify(rhiz, "Rhizophysa").items():
    allhits[g] += v

import os
os.makedirs(OUT, exist_ok=True)
print(f"\n{'gene':5s} {'total':>6s} {'Physalia':>9s} {'Rhizophysa':>11s}")
for g in ('16S', '18S', 'CO1', 'ITS'):
    recs = allhits[g]
    # one sequence per accession: keep the longest
    best = {}
    for hdr, seq in recs:
        acc = hdr.split()[0]
        if acc not in best or len(seq) > len(best[acc][1]):
            best[acc] = (hdr, seq)
    rows = sorted(best.values())
    nph = sum(1 for h, _ in rows if 'Physalia' in h.split()[1])
    nrh = sum(1 for h, _ in rows if 'Rhizophysa' in h.split()[1])
    with open(f"{OUT}/{g}.external.fasta", "w") as f:
        for h, s in rows:
            f.write(f">{h}\n{s}\n")
    print(f"{g:5s} {len(rows):6d} {nph:9d} {nrh:11d}")
print(f"\nwritten to {OUT}/")
