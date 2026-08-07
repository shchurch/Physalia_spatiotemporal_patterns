import argparse
from Bio import Entrez, SeqIO
import csv
from collections import defaultdict

# === CLI ARGUMENTS ===
parser = argparse.ArgumentParser(description="Fetch all NCBI annotated features for an organism.")
parser.add_argument("--email", required=True, help="Your email address (required by NCBI)")
parser.add_argument("--organism", required=True, help="Organism name to search for (e.g., Siphonophorae)")
parser.add_argument("--outdir", required=True, help="Output directory")
args = parser.parse_args()

Entrez.email = args.email
outdir = args.outdir
search_term = f'"{args.organism}"[Organism]'

# === Filenames ===
safe_organism = args.organism.replace(" ", "_")
tsv_output_file = f"{outdir}/{safe_organism}_metadata.tsv"
retmax = 10000

# === SEARCH ===
print(f"🔍 Searching NCBI for: {search_term}")
handle = Entrez.esearch(db="nucleotide", term=search_term, retmax=retmax)
record = Entrez.read(handle)
id_list = record["IdList"]
n_found = len(id_list)
print(f"✅ Found {n_found} records.")

if n_found == 0:
    print("⚠️ No records found. Exiting.")
    exit()

# === FETCH GENBANK ===
print("📥 Fetching GenBank records...")
handle = Entrez.efetch(db="nucleotide", id=id_list, rettype="gb", retmode="text")
gb_records = list(SeqIO.parse(handle, "gb"))
print(f"📦 Retrieved {len(gb_records)} GenBank records.")

# === PARSE METADATA ===
print("📊 Parsing records and collecting metadata fields...")
all_keys = set(["accession", "organism"])
record_data = []

for i, rec in enumerate(gb_records):
    if i % 50 == 0 or i == len(gb_records) - 1:
        print(f"   - Processing record {i+1}/{len(gb_records)}")
    rec_dict = defaultdict(str)
    rec_dict["accession"] = rec.id
    rec_dict["organism"] = rec.annotations.get("organism", "")

    for feature in rec.features:
        for key, value in feature.qualifiers.items():
            val = "; ".join(value) if isinstance(value, list) else str(value)
            if key in rec_dict and rec_dict[key]:
                rec_dict[key] += " | " + val
            else:
                rec_dict[key] = val
            all_keys.add(key)

    record_data.append(rec_dict)

# === WRITE TSV ===
print(f"💾 Writing metadata to: {tsv_output_file}")
all_keys = sorted(all_keys)
with open(tsv_output_file, "w", newline='') as f:
    writer = csv.DictWriter(f, fieldnames=all_keys, delimiter="\t")
    writer.writeheader()
    for rec in record_data:
        writer.writerow(rec)

print(f"✅ Metadata TSV complete: {tsv_output_file} ({len(record_data)} rows)")

# === EXTRACT FEATURES INTO FASTA FILES ===
print("🧬 Extracting all annotated features into separate FASTA files...")
feature_groups = defaultdict(list)

for rec in gb_records:
    accession = rec.annotations.get("accessions", [rec.id])[0]
    seen_coords = set()

    for feature in rec.features:
        if not feature.location or not hasattr(feature, "type"):
            continue

        ftype = feature.type
        if ftype == "source":
            continue  # Skip 'source' features

        start = int(feature.location.start)
        end = int(feature.location.end)
        coords = (start, end)
        if coords in seen_coords:
            continue
        seen_coords.add(coords)

        try:
            subseq = feature.extract(rec.seq)
            if not subseq or len(subseq) == 0:
                continue  # skip empty

            qualifiers = feature.qualifiers
            name_parts = [
                qualifiers.get("gene", [""])[0],
                qualifiers.get("product", [""])[0],
                qualifiers.get("note", [""])[0],
                qualifiers.get("protein_id", [""])[0],
                qualifiers.get("locus_tag", [""])[0]
            ]
            name_parts = [p.replace(" ", "_") for p in name_parts if p]
            name_part = "_".join(name_parts) if name_parts else "unknown"

            feature_id = f"{accession}_{ftype}_{name_part}_{start}_{end}"

            fasta_record = SeqIO.SeqRecord(
                subseq,
                id=feature_id,
                description=""
            )
            feature_groups[ftype].append(fasta_record)
        except Exception as e:
            print(f"⚠️ Skipping {ftype} from {accession} ({start}-{end}): {e}")

# === WRITE FASTA FILES ===
for ftype, records in feature_groups.items():
    out_fasta = f"{outdir}/{safe_organism}_{ftype}.fasta"
    print(f"💾 Writing {len(records)} records to {out_fasta}")
    with open(out_fasta, "w") as f:
        SeqIO.write(records, f, "fasta")

print("✅ Feature FASTA extraction complete.")
