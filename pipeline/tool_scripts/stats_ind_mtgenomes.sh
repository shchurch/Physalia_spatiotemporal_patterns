#!/bin/bash
set -euo pipefail

INPUT_DIR="individual_genomes"
OUTPUT_TSV="genome_statistics.tsv"

# Write TSV header
echo -e "ID\tNum_Sequences\tLengths(bp)\tCircular\tGene_Count\tExon_Count\tATP8_Present\tncRNA_Count\trRNA_Count\ttRNA_Count\tcontents" > "$OUTPUT_TSV"

# Loop through each ID folder
for ID_DIR in "$INPUT_DIR"/*/; do
    ID=$(basename "$ID_DIR")
    FASTA_FILE="${ID_DIR}/${ID}_mitochondrion.fasta"
    GFF_FILE="${ID_DIR}/${ID}_annotation.gff"

    if [[ ! -f "$FASTA_FILE" ]]; then
        echo "Skipping $ID — missing FASTA file."
        continue
    fi

    echo "Processing $ID..."

    # FASTA stats
    NUM_SEQUENCES=$(grep -c '^>' "$FASTA_FILE")
    LENGTHS=$(awk '/^>/ {if (seq) print length(seq); seq=""} /^[^>]/ {seq=seq $0} END {print length(seq)}' "$FASTA_FILE" | paste -sd ";" -)
    CIRCULAR=$(grep -qi "circular" "$FASTA_FILE" && echo "yes" || echo "no")

    # Initialize GFF-related fields as NA
    GENE_COUNT="NA"
    EXON_COUNT="NA"
    ATP8_PRESENT="NA"
    ncRNA_COUNT="NA"
    rRNA_COUNT="NA"
    tRNA_COUNT="NA"
    Gene_tRNA_Order="NA"

    # If GFF exists, parse it
    if [[ -f "$GFF_FILE" ]]; then
        GENE_COUNT=$(awk '$3 == "gene"' "$GFF_FILE" | wc -l)
        EXON_COUNT=$(awk '$3 == "exon"' "$GFF_FILE" | wc -l)
        ncRNA_COUNT=$(awk '$3 == "ncRNA_gene"' "$GFF_FILE" | wc -l)
        rRNA_COUNT=$(awk '$3 == "rRNA"' "$GFF_FILE" | wc -l)
        tRNA_COUNT=$(awk '$3 == "tRNA"' "$GFF_FILE" | wc -l)
        ATP8_PRESENT=$(grep -i "atp8" "$GFF_FILE" >/dev/null && echo "yes" || echo "no")

        CONTENT_AtoZ=$(awk -F'\t' '
        ($3 == "gene" || $3 == "tRNA") {
            if (match($9, /Name=([^;]+)/, arr)) {
                print arr[1]
            }
        }' "$GFF_FILE" | sort | paste -sd ';' -)
    fi

    # Output line
    echo -e "${ID}\t${NUM_SEQUENCES}\t${LENGTHS}\t${CIRCULAR}\t${GENE_COUNT}\t${EXON_COUNT}\t${ATP8_PRESENT}\t${ncRNA_COUNT}\t${rRNA_COUNT}\t${tRNA_COUNT}\t${CONTENT_AtoZ}" >> "$OUTPUT_TSV"
done

echo "Done! Results saved to $OUTPUT_TSV"

