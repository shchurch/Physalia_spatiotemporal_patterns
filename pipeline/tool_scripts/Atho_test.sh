#!/bin/bash
#SBATCH --job-name=Atho_test
#SBATCH --output=Atho_test_%j.log
#SBATCH --requeue
#SBATCH --time=2-00:00:00
#SBATCH --partition=ycga
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=8G

# Load required modules
ml minimap2
ml BCFtools
ml SAMtools

# Threads to use
THREADS=8

# Parse arguments
READS="$1"
NUM_READS="$2"
shift 2
GENOMES=("$@")

if [[ -z "$READS" || -z "$NUM_READS" || ${#GENOMES[@]} -eq 0 ]]; then
    echo "Usage: sbatch $0 reads.fastq.gz num_reads genome1.fasta [genome2.fasta ...]"
    exit 1
fi

# Derive base name and setup output dir
BASE_READS=$(basename "$READS" .fastq.gz)
OUTDIR="Atho_test/$BASE_READS"
mkdir -p "$OUTDIR"

SUMMARY_FILE="$OUTDIR/summary.tsv"

echo "Starting job on $(date)"
echo "FASTQ: $READS"
echo "Read limit: $NUM_READS"
echo "Genomes: ${GENOMES[*]}"
echo "Output dir: $OUTDIR"

# Subset FASTQ
echo "Subsetting FASTQ to $NUM_READS reads..."
READS_SUBSET="$OUTDIR/${BASE_READS}_subset.fastq.gz"
zcat "$READS" | head -n $((NUM_READS * 4)) | gzip > "$READS_SUBSET"
echo "Subset written to: $READS_SUBSET"

# Header for summary file
echo -e "Reads_Basename\tGenome\tMapped_Reads\tTotal_Unique_Mapped\tFraction_of_Total(%)\tMapping_Percent\tSNP_Count\tSNP_Density\tAvg_Depth\tMultiallelic_Count" > "$SUMMARY_FILE"

# Process each genome
for GENOME in "${GENOMES[@]}"; do
    GENOME_TAG=${GENOME%.fasta}
    GENOME_TAG=${GENOME_TAG%.fa}
    PREFIX=${GENOME_TAG//\//__}

    echo -e "\n--- Processing $GENOME ---"

    BAM="$OUTDIR/$PREFIX.bam"
    VCF="$OUTDIR/$PREFIX.vcf"

    echo "Mapping reads..."
    minimap2 -t $THREADS -ax sr "$GENOME" "$READS_SUBSET" | samtools sort -@ $((THREADS / 2)) -o "$BAM"
    samtools index "$BAM"

    echo "Extracting mapped read names..."
    samtools view -F 4 "$BAM" | cut -f1 > "$OUTDIR/${PREFIX}_mapped_reads.txt"

    echo "Calling SNPs..."
    bcftools mpileup -Ou -f "$GENOME" "$BAM" --threads $THREADS | \
        bcftools call -mv -Ov --threads $THREADS -o "$VCF"

    echo "Calculating stats..."
    MAPPED=$(samtools view -c -F 4 "$BAM")
    TOTAL=$(samtools view -c "$BAM")
    PERCENT=$(awk "BEGIN {printf \"%.4f\", ($MAPPED/$TOTAL)*100}")
    SNPS=$(grep -vc '^#' "$VCF")
    SNP_DENSITY=$(awk "BEGIN {print $SNPS/$MAPPED}")
    AVG_DEPTH=$(samtools depth "$BAM" | awk '{sum+=$3} END {print sum/NR}')
    MULTIALLELIC_COUNT=$(bcftools view -H "$VCF" | awk 'BEGIN {c=0} {if ($5 ~ /,/) c++} END {print c}')
done

# Calculate union of mapped reads across all genomes
echo -e "\nMerging mapped reads across all genomes..."
cat "$OUTDIR"/*_mapped_reads.txt | sort | uniq > "$OUTDIR/union_mapped_reads.txt"
TOTAL_UNIQUE_MAPPED=$(wc -l < "$OUTDIR/union_mapped_reads.txt")
echo "Total unique mapped reads across all references: $TOTAL_UNIQUE_MAPPED"

# Append final stats per genome
for GENOME in "${GENOMES[@]}"; do
    GENOME_TAG=${GENOME%.fasta}
    GENOME_TAG=${GENOME_TAG%.fa}
    PREFIX=${GENOME_TAG//\//__}

    BAM="$OUTDIR/$PREFIX.bam"
    VCF="$OUTDIR/$PREFIX.vcf"

    MAPPED=$(samtools view -c -F 4 "$BAM")
    TOTAL=$(samtools view -c "$BAM")
    PERCENT=$(awk "BEGIN {printf \"%.4f\", ($MAPPED/$TOTAL)*100}")
    FRACTION_TOTAL=$(awk "BEGIN {printf \"%.4f\", ($MAPPED/$TOTAL_UNIQUE_MAPPED)*100}")
    SNPS=$(grep -vc '^#' "$VCF")
    SNP_DENSITY=$(awk "BEGIN {print $SNPS/$MAPPED}")
    AVG_DEPTH=$(samtools depth "$BAM" | awk '{sum+=$3} END {print sum/NR}')
    MULTIALLELIC_COUNT=$(bcftools view -H "$VCF" | awk 'BEGIN {c=0} {if ($5 ~ /,/) c++} END {print c}')

    echo -e "$BASE_READS\t$PREFIX\t$MAPPED\t$TOTAL_UNIQUE_MAPPED\t$FRACTION_TOTAL\t$PERCENT\t$SNPS\t$SNP_DENSITY\t$AVG_DEPTH\t$MULTIALLELIC_COUNT" >> "$SUMMARY_FILE"
done

# Cleanup
echo -e "\nCleaning up..."
rm -f "$READS_SUBSET"
rm -f "$OUTDIR"/*_mapped_reads.txt

echo -e "Done. Finished at $(date)"

