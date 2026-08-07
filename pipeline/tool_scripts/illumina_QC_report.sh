#!/bin/bash

ID=$1 
REPORT=$2
NUM_READS=$3

{
    echo "## ILLUMINA QC SUMMARY ##"
    echo "ID: ${ID}"
    echo "Number of reads used: ${NUM_READS}"
    echo
    echo "### FastQC Results ###"
    echo "**R1 Reads:**"
    unzip -p fastqc/$ID/$ID.R1.trimmed_fastqc.zip '*/summary.txt' | sed 's/^/    /'
    GCR1=$(unzip -p fastqc/$ID/$ID.R1.trimmed_fastqc.zip '*/fastqc_data.txt' | grep "%GC")
    echo
    echo "    $GCR1"    
    echo
    echo "**R2 Reads:**"
    unzip -p fastqc/$ID/$ID.R2.trimmed_fastqc.zip '*/summary.txt' | sed 's/^/    /'
    echo
    GCR2=$(unzip -p fastqc/$ID/$ID.R2.trimmed_fastqc.zip '*/fastqc_data.txt' | grep "%GC")
    echo "    $GCR2"    
    echo
    echo "## Kraken2 Results ###"
    echo "**Percent Unclassified (Probable Siphonophore):**"
    grep -E "\bU\b.*\bunclassified$" kraken/$ID.report.txt | awk '{print "    " $0}'
    echo
    echo "**Percent Classified as Bacterial:**"
    grep -E "\bD\b.*\bBacteria$" kraken/$ID.report.txt | awk '{print "    " $0}'
    echo
    echo "## Human Contamination Results ###"
    echo "**Percent mapped to human:**"
    human_mapped=$(cat human/${ID}.mapped_paired)
    percent_human=$(echo "scale=5; 100 *(($human_mapped) / $NUM_READS)" | bc)
    echo "    Reads mapped to human genome: $human_mapped"
    echo "    Percent reads human: $percent_human%"
    echo
    echo "### GetOrganelle Results ###"
    echo "**Path to Mitochondrial Genome Assembly:**"
    fasta_file=$(find getorganelle/${ID}_mtgenome -name '*1.path_sequence.fasta' -print)
    if [ -n "$fasta_file" ]; then
        echo "    $fasta_file"
        echo
        echo "**Scaffold Headers and Lengths:**"
        awk '/^>/ {header=substr($0,2,match($0," ") - 2); if (match($0," ") == 0) header=substr($0,2); getline seq; print "    " header ": " length(seq)}' "$fasta_file"
    else
        echo "    No assembly path"
        echo
        echo "**Scaffold Headers and Lengths:**"
        echo "    No scaffolds found"
    fi
    echo
    echo "**Errors from GetOrganelle:**"
    grep "ERROR" getorganelle/${ID}_mtgenome/get_org.log.txt | sed 's/^/    /'
    echo
    echo "**Mapping Statistics"
    fq_file="getorganelle/${ID}_mtgenome/extended_1_paired.fq"
    if [ -f "$fq_file" ]; then
        reads_used=$(grep "Reads used" getorganelle/${ID}_mtgenome/get_org.log.txt | awk -F'[=+]' '{print $2 + $3}')
        mt_lines=$(wc -l < "$fq_file")
        mt_reads=$((mt_lines / 4))
        mapped_reads=$(echo "scale=5; 100 *(($mt_reads) / $reads_used)" | bc)
        echo "    Reads used by GetOrganelle: $reads_used"
        echo "    Reads in extended_1_paired.fq: $mt_reads"
        echo "    Estimated percent mt of reads used: $mapped_reads%"
    else
        echo "    No paired fastq file found"
    fi
    echo
    echo "### Mitos2 Results ###"
    echo "**Gene Order Files:**"
    geneorder_files=($(find mitos_results/$ID -name 'result.geneorder'))
    IFS=$'\n' read -d '' -r -a missing_lines < <(grep 'missing:' mitos_results/$ID/${ID}_runinfo.txt && printf '\0')
    IFS=$'\n' read -d '' -r -a duplicated_lines < <(grep 'duplicated:' mitos_results/$ID/${ID}_runinfo.txt && printf '\0')
    max_lines=${#geneorder_files[@]}
    for ((i=0; i<$max_lines; i++)); do
        echo "    ${geneorder_files[i]:-None}"
        if [ -f "${geneorder_files[i]}" ]; then
            cat "${geneorder_files[i]}" | sed 's/^/        /'
        fi
        echo "    ${missing_lines[i]:-None missing}"
        echo "    ${duplicated_lines[i]:-None duplicated}"
        echo
    done
    echo
    echo "### tRNAscan-SE Results ###"
    awk '{print "    " $0}' tRNAscan_results/${ID}/${ID}_tRNAscan_result.txt
    echo
    echo "### sharkmer Results ###"
    echo "**PCR Products Assembled:**"
    shark_files=(shark_results/$ID/*.fasta)
    if [ -e "${shark_files[0]}" ]; then
        for file in "${shark_files[@]}"; do
            primer=$(basename "$file" | cut -d'_' -f2)
            echo "    $primer: $file"
            # Print the first 5 headers from the FASTA file
            head -n 10 "$file" | grep '^>' | head -n 5 | sed 's/^/        /'
        done
    else
        echo "    No PCR products found"
    fi
    echo
    echo "### rego Results ###"
    echo "**Hits Found Per Primer:**"
    for file in rego_results/$ID/*.txt; do
        primer=$(basename "$file" | cut -d'_' -f2)
        hits=$(grep -m1 '^Hits found:' "$file" | awk '{print $3}')
        echo "    $primer: $hits"
    done
} > $REPORT
