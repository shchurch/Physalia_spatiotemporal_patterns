#!/bin/bash

# Assign input variables
ID_file="$1"
fasta_file="$2"
output_file="$3"

# Check if the correct number of arguments is provided
if [[ $# -ne 3 ]]; then
    echo "Usage: $0 <ID_file> <fasta_file> <output_file>"
    exit 1
fi

# Check if ID_file exists
if [[ ! -f "$ID_file" ]]; then
    echo "Error: ID file '$ID_file' does not exist."
    exit 1
fi

# Check if fasta_file exists
if [[ ! -f "$fasta_file" ]]; then
    echo "Error: FASTA file '$fasta_file' does not exist."
    exit 1
fi

# Run the awk command
awk -v id_file="$ID_file" '
BEGIN {
    # Read IDs into an array
    while (getline < id_file) {
        ids[$1] = 1
    }
}
# Reset print_seq at each header
/^>/ {
    print_seq = 0
}
# Check for any ID match in the header
/^>/ {
    for (id in ids) {
        if (index($0, id)) {
            print_seq = 1
            break
        }
    }
}
# Print if print_seq is active
print_seq
' "$fasta_file" > "$output_file"

# Confirm success
if [[ $? -eq 0 ]]; then
    echo "Output written to '$output_file'."
else
    echo "Error processing files."
    exit 1
fi

