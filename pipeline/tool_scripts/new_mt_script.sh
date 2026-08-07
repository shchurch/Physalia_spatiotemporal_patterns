#!/bin/bash

# Load modules
ml MAFFT
ml miniconda
conda activate mito

# Read arguments
THREADS="$1" # arg 1 = threads
OUTPUT_DIR="$2" # arg 2 = output directory
EXT_SEQ_FILE="$3" # external sequences to add
ID_FILE="$4" # file with list of IDs

# Check output dir
if [[ ! -d "$OUTPUT_DIR" ]]; then
    echo "Error: Output directory $OUTPUT_DIR does not exist."
    exit 1
fi

# Initialize combined fasta cleanly
COMBINED_FASTA="$OUTPUT_DIR/combined.fasta"
> "$COMBINED_FASTA" # overwrite to empty

# Process each ID
while IFS= read -r ID; do
    echo "Processing ID: $ID"
    
    # Make subdirectory
    ID_SUBDIR="$OUTPUT_DIR/individual_genomes/$ID"
    mkdir -p "$ID_SUBDIR"
    
    FOUND=0

    for file in ./getorganelle/"${ID}"_mtgenome/*path*fasta; do
        if [[ -f "$file" ]]; then
            echo "  Found file: $file"
            FOUND=1

            # Extract first sequence
            FIRST_SEQ=$(awk '/^>/ {if (seq) exit; header=$0; next} {seq=seq $0} END {print header "\n" seq}' "$file")
            PARENT_DIR=$(basename $(dirname "$file"))

            # Modify header
            MOD_HEADER=$(echo "$FIRST_SEQ" | head -n1 | sed "s/^>/>${PARENT_DIR}_/")
            
            # Trim if header >80 chars
            if [[ ${#MOD_HEADER} -gt 80 ]]; then
                MOD_HEADER="${MOD_HEADER:0:75}_trimmed"
            fi

            SEQUENCE=$(echo "$FIRST_SEQ" | tail -n+2)

            # Write to per-ID fasta
            PER_ID_FASTA="$ID_SUBDIR/${ID}_mitochondrion.fasta"
            {
                echo "$MOD_HEADER"
                echo "$SEQUENCE"
            } > "$PER_ID_FASTA"

            # Also append to combined fasta
            {
                echo "$MOD_HEADER"
                echo "$SEQUENCE"
            } >> "$COMBINED_FASTA"

            echo "Processed $file and saved fasta to $PER_ID_FASTA"
        fi
    done

    if [[ $FOUND -eq 0 ]]; then
        echo "  No files found for ID: $ID in ./getorganelle/${ID}_mtgenome/*path*"
    fi

    ### Copy GFF annotation
    GFF_SOURCE="./mitos_results/${ID}/result.gff"
    GFF_DEST="$ID_SUBDIR/${ID}_annotation.gff"
    if [[ -f "$GFF_SOURCE" ]]; then
        cp "$GFF_SOURCE" "$GFF_DEST"
        echo "Copied GFF annotation to $GFF_DEST"

    	# Add tRNAscan annotations if available
    	TRNA_SOURCE="./tRNAscan_results/${ID}/${ID}_tRNAscan_result.txt"
    	if [[ -f "$TRNA_SOURCE" ]]; then
    	    echo "Adding tRNAscan results to $GFF_DEST"
    	    awk 'BEGIN{OFS="\t"}
    		/^[[:space:]]*$/ { next }
    		/^Sequence/ { next }
    		/^Name/ { next }
    		/^-+/ { next }
    		{
    		    scaffold=$1;
    		    start=$3;
    		    end=$4;
    		    aa=$5;
    		    anticodon=$6;
    		    if (start <= end) {
    			strand = "+"
    		    } else {
    			strand = "-"
    			tmp=start; start=end; end=tmp
    		    }
    		    name="trn" substr(aa,1,1) "-" anticodon
    		    printf "%s\ttRNAscan-SE\ttRNA\t%s\t%s\t.\t%s\t.\tID=%s;Name=%s\n", scaffold, start, end, strand, name, name
    		}' "$TRNA_SOURCE" >> "$GFF_DEST"
    	else
    	    echo "Warning: tRNAscan result not found for $ID at $TRNA_SOURCE"
    	fi
    
    else
        echo "Warning: GFF file not found for $ID at $GFF_SOURCE"
    fi
    
    # Fix scaffold names in GFF to match FASTA headers
    if [[ -f "$GFF_DEST" ]]; then
        TMP_GFF="${GFF_DEST}.tmp"
        awk -v id="$ID" 'BEGIN{OFS="\t"}
            {
                if($1 !~ /^#/) $1=id"_mtgenome_"$1; print
            }' "$GFF_DEST" > "$TMP_GFF"
        mv "$TMP_GFF" "$GFF_DEST"
        echo "Fixed scaffold names in $GFF_DEST"
    fi

done < "$ID_FILE"

# Add external sequences to combined fasta
cat "$EXT_SEQ_FILE" >> "$COMBINED_FASTA"

# Run MAFFT alignment
MAFFT_OUTPUT="$OUTPUT_DIR/combined.aln.fa"
mafft --thread $THREADS --adjustdirectionaccurately --auto "$COMBINED_FASTA" > "$MAFFT_OUTPUT"

# Run IQ-TREE
IQTREE_OUTPUT="$OUTPUT_DIR/iqtree_result"
iqtree -s "$MAFFT_OUTPUT" -bb 1000 --redo -pre "$IQTREE_OUTPUT"

echo "MAFFT and IQ-TREE analysis completed. Results saved to $IQTREE_OUTPUT"

