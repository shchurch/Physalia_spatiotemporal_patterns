#!/bin/bash

# Input files
id_file=$1  # File containing IDs, one per line
fasta_file=$2  # Input FASTA file
output_file=$3  # Output FASTA file

# Debug: Print input files
echo "Debug: ID file = $id_file"
echo "Debug: FASTA file = $fasta_file"
echo "Debug: Output file = $output_file"

# Debug: Print the first few lines of the FASTA file
echo "Debug: First few lines of FASTA file:"
head -n 10 "$fasta_file"

# Step 1: Read IDs from the ID file and create a regex pattern
# Use grep -F for fixed-string matching to avoid issues with special characters
keep_pattern=$(paste -sd '|' "$id_file")

# Debug: Print the keep pattern
echo "Debug: Keep pattern = $keep_pattern"

# Step 2: Process the FASTA file
awk -v pattern="$keep_pattern" '
    # When a header line (starts with ">") is found
    /^>/ {
        # If we have a previous header and sequence, check if it should be printed
        if (header != "") {
            if (header ~ ">" pattern) {
                print header
                print seq
            } 
        }
        # Start a new header and reset the sequence
        header = $0
        seq = ""
    }
    # When a sequence line (does not start with ">") is found
    !/^>/ {
        # Append the sequence line to the current sequence
        seq = seq $0 "\n"
    }
    # At the end of the file, handle the last header and sequence
    END {
        if (header != "") {
            if (header ~ ">" pattern) {
                print header
                print seq
            }
        }
    }
' "$fasta_file" > "$output_file"

# Debug: Print completion message
echo "Debug: Filtering complete. Output written to $output_file"
