#!/bin/bash

input_file=$1
output_file=$2

# Process the FASTA file
awk 'BEGIN {OFS=""} 
    /^>/ {split($0, a, /[ _\/]/); print a[1]} 
    !/^>/ {print}' "$input_file" > "$output_file"

echo "Trimmed FASTA saved as $output_file"
