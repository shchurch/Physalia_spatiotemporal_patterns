#!/bin/bash

search_term="$1"

echo "Search term: $search_term"
cd mitos_results || { echo "Directory 'mitos_results' not found. Exiting."; exit 1; }

output_file="mitos_$search_term.fasta"
echo "Output will be saved to $output_file"
> "$output_file"

total_matches=0

find . -type f -name "result.fas" | while read -r file; do
    dir_name=$(realpath --relative-to="$(pwd)" "$(dirname "$file")")
    echo "Processing file: $file (directory: $dir_name)"

    file_matches=0
    awk -v dir_name="$dir_name" -v search_term="$search_term" -v file_matches_ref=file_matches '
    BEGIN { found = 0; }
    $0 ~ search_term {
        found = 1
        file_matches_ref++
        print_flag = 1
        sub(/^>/, "")
        header = dir_name "_" $0
        if (length(header) > 80) {
            header = substr(header, 1, 60)
	    header = header "_headtrim_" search_term
        }
        print ">" header
	next
    }
    /^[>]/ && print_flag {
        print_flag = 0
    }
    print_flag
    END {
        if (found == 0) {
            print "No matches found for search term: " search_term > "/dev/stderr"
        } else {
            print file_matches_ref " matches found in file: " FILENAME > "/dev/stderr"
        }
    }
    ' "$file" >> "$output_file"

    total_matches=$((total_matches + file_matches))
done

if [[ $total_matches -gt 0 ]]; then
    echo "Finished processing. Total matches found: $total_matches"
    echo "Results saved to $output_file."
else
    echo "No matches found for the search term '$search_term'. Output file is empty."
fi

