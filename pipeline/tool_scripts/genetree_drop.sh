#!/bin/bash

# Load necessary modules
ml MAFFT
ml miniconda
conda activate siphgen

# Default values
shark_gene_name="NONE"
mitos_gene_name="NONE"
output_dir=""
threads=1
ext_ids=""
ext_seqs=""
name=""

# Parse named arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --shark_gene_name)
            shark_gene_name="$2"; shift 2;;
        --mitos_gene_name)
            mitos_gene_name="$2"; shift 2;;
        --output_dir)
            output_dir="$2"; shift 2;;
        --threads)
            threads="$2"; shift 2;;
        --ext_ids)
            ext_ids="$2"; shift 2;;
        --ext_seqs)
            ext_seqs="$2"; shift 2;;
        --name)
            name="$2"; shift 2;;
        *)
            echo "Unknown parameter: $1"; exit 1;;
    esac
done

# Ensure required parameters are set
if [[ -z "$output_dir" ]]; then
    echo "Error: --output_dir is required" >&2
    exit 1
fi
if [[ -z "$name" ]]; then
    echo "Error: --name is required" >&2
    exit 1
fi

# Ensure output directory exists
mkdir -p "$output_dir"

# Retrieve MITOS results
./tool_scripts/retrieve_mitos_results.sh "$mitos_gene_name"

# Concatenate shark FASTA results
cat shark_results/*/*${shark_gene_name}*.fasta > "$output_dir/tmp.shark.${shark_gene_name}.fasta"

# Trim headers for MITOS and Shark FASTA files
./tool_scripts/trim_headers.sh "mitos_results/mitos_${mitos_gene_name}.fasta" "$output_dir/tmp.mitos.${mitos_gene_name}.head.fasta"
./tool_scripts/trim_headers.sh "$output_dir/tmp.shark.${shark_gene_name}.fasta" "$output_dir/tmp.shark.${shark_gene_name}.head.fasta"

# Combine MITOS and Shark FASTA files
cat "$output_dir/tmp.mitos.${mitos_gene_name}.head.fasta" "$output_dir/tmp.shark.${shark_gene_name}.head.fasta" > "$output_dir/tmp.${name}.fasta"

# Select the longest sequences
./tool_scripts/select_longest.sh "$output_dir/tmp.${name}.fasta" "$output_dir/${name}.longest.fasta"

# Process external ID filtering if provided
if [[ -n "$ext_ids" ]]; then
    ./tool_scripts/drop_sequences.sh "$ext_ids" "$output_dir/${name}.longest.fasta" "$output_dir/${name}.drop.fasta"
else
    cp "$output_dir/${name}.longest.fasta" "$output_dir/${name}.drop.fasta"
fi

# Add external sequences if provided
if [[ -n "$ext_seqs" ]]; then
    cat "$output_dir/${name}.drop.fasta" "$ext_seqs" > "$output_dir/${name}.all.fasta" 
else
    cat "$output_dir/${name}.drop.fasta" > "$output_dir/${name}.all.fasta" 
fi

# Align sequences using MAFFT
mafft --thread $threads --adjustdirectionaccurately --auto "$output_dir/${name}.all.fasta" > "$output_dir/${name}.aln.fasta"
sed -i -E "s/>_R_/>/g" "$output_dir/${name}.aln.fasta"

# Build phylogenetic tree using IQ-TREE
iqtree -B 1000 -nt AUTO -s "$output_dir/${name}.aln.fasta" --redo

