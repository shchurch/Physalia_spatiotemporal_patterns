#!/bin/bash

input_file=$1
output_file=$2

declare -A sequences
declare current_header=""
declare current_sequence=""

# Read the FASTA file line by line
while IFS= read -r line; do
    if [[ $line == '>'* ]]; then
        # If a previous header exists, store its longest sequence
        if [[ -n $current_header ]]; then
            if [[ -z ${sequences[$current_header]} || ${#current_sequence} -gt ${#sequences[$current_header]} ]]; then
                sequences[$current_header]=$current_sequence
            fi
        fi
        current_header=$line
        current_sequence=""
    else
        current_sequence+=$line
    fi
done < "$input_file"

# Store the last read sequence
if [[ -n $current_header ]]; then
    if [[ -z ${sequences[$current_header]} || ${#current_sequence} -gt ${#sequences[$current_header]} ]]; then
        sequences[$current_header]=$current_sequence
    fi
fi

# Write the longest sequences to the output file
> "$output_file"
for header in "${!sequences[@]}"; do
    echo "$header" >> "$output_file"
    echo "${sequences[$header]}" >> "$output_file"
done

echo "Longest sequences saved as $output_file"

