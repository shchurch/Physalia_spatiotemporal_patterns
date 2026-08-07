#!/bin/bash
ml MAFFT
ml miniconda
conda activate mito

THREADS="$1" # arg 1 = threads
OUTPUT_DIR="$2" # arg 2 = output directory
EXT_SEQ_FILE="$3"
ID_FILE="$4"

if [[ ! -d "$OUTPUT_DIR" ]]; then
    echo "Error: Output directory $OUTPUT_DIR does not exist."
    exit 1
fi

COMBINED_FASTA="$OUTPUT_DIR/combined.fasta"
touch "$COMBINED_FASTA"

while IFS= read -r ID; do
    echo "Processing ID: $ID"
    for file in ./getorganelle/"${ID}"_mtgenome/*path*; do
        if [[ -f "$file" ]]; then
            echo "  Found file: $file"
    	    FIRST_SEQ=$(awk '/^>/ {if (seq) exit; header=$0; next} {seq=seq $0} END {print header "\n" seq}' "$file")
            PARENT_DIR=$(basename $(dirname "$file"))
            echo "$FIRST_SEQ" | sed "1s/^>/>${PARENT_DIR}_/" >> "$COMBINED_FASTA"
            echo "Processed $file and appended to $COMBINED_FASTA"
        else
            echo "  No files found for ID: $ID in ${ID}_mtgenome/*path*"
        fi
    done
done < "$ID_FILE"

cat "$EXT_SEQ_FILE" >> "$COMBINED_FASTA"


# Run MAFFT and IQ-TREE
MAFFT_OUTPUT="$OUTPUT_DIR/combined.aln.fa"
mafft --thread $THREADS --adjustdirectionaccurately --auto "$COMBINED_FASTA" > "$MAFFT_OUTPUT"

IQTREE_OUTPUT="$OUTPUT_DIR/iqtree_result"
iqtree -s "$MAFFT_OUTPUT" -bb 1000 --redo -pre "$IQTREE_OUTPUT"

echo "MAFFT and IQ-TREE analysis completed. Results saved to $IQTREE_OUTPUT"

