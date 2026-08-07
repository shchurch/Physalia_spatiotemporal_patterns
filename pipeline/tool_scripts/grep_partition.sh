#!/bin/bash

# Input file containing the alignment data
input_file="$1"

# Output partition file
output_file="$2"

# Grep the relevant lines (adjust the pattern as needed)
grep -E '.* = [0-9]+-[0-9]+;' "$input_file" > tmp_lines.txt

# Check if grep found any lines
if [[ ! -s tmp_lines.txt ]]; then
    echo "No matching lines found in $input_file"
    exit 1
fi

# Start writing the partition file
echo "#nexus" > "$output_file"
echo "begin sets;" >> "$output_file"

# Initialize a counter for gene names
counter=1

# Process each line to format as IQ-TREE partitions
while read -r line; do
    # Extract filename and range
    name=$(echo "$line" | cut -d'=' -f1 | tr -d ' ')
    range=$(echo "$line" | cut -d'=' -f2 | tr -d ' ;')

    # Write charset line
    echo "   charset gene${counter} = ${range};" >> "$output_file"

    # Increment counter
    ((counter++))
done < tmp_lines.txt

# Start charpartition block
echo -e "   charpartition mypart =" >> "$output_file"

# Reset counter for charpartition entries
counter=1

# Process again for charpartition formatting
while read -r line; do
    # Extract filename
    name=$(echo "$line" | cut -d'=' -f1 | tr -d ' ')

    # Append to charpartition
    echo -e "      gene${counter}: gene${counter}," >> "$output_file"

    # Increment counter
    ((counter++))
done < tmp_lines.txt

# Remove the last comma and close the block
sed -i '$ s/,$/;/' "$output_file"
echo "end;" >> "$output_file"

# Cleanup tmporary file
rm tmp_lines.txt

# Confirmation message
echo "Partition file '$output_file' created successfully."

