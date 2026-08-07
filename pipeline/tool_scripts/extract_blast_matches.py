import argparse
import logging
from Bio import SeqIO

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger()

def extract_matching_sequences(blast_file, fasta_file, output_file):
    logger.info(f"Starting to parse BLAST results from {blast_file}")
    
    matches = set()
    try:
        # Read the BLAST results
        with open(blast_file, 'r') as blast:
            for line in blast:
                cols = line.strip().split('\t')
                subject_id = cols[1]  # Subject (hit) ID is in the second column
                matches.add(subject_id)

        logger.info(f"Found {len(matches)} matching sequences in BLAST results.")

        logger.info(f"Extracting matching sequences from {fasta_file}")
        # Extract the matching sequences from the large FASTA file
        with open(fasta_file, 'r') as fasta, open(output_file, 'w') as output:
            count = 0
            for record in SeqIO.parse(fasta, 'fasta'):
                if record.id in matches:
                    SeqIO.write(record, output, 'fasta')
                    count += 1

        logger.info(f"Successfully extracted {count} matching sequences to {output_file}.")
    
    except Exception as e:
        logger.error(f"Error occurred: {e}")

def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description='Extract matching sequences from a FASTA file using BLAST results.')
    parser.add_argument('blast_file', help='BLAST result file in TSV format')
    parser.add_argument('fasta_file', help='Large FASTA file containing sequences to search')
    parser.add_argument('output_file', help='Output FASTA file for storing matched sequences')
    
    # Parse the arguments
    args = parser.parse_args()

    # Start the sequence extraction
    extract_matching_sequences(args.blast_file, args.fasta_file, args.output_file)

if __name__ == '__main__':
    main()

