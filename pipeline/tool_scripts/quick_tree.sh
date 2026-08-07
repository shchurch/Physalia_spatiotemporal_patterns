fasta_file="$1"
threads="$2"
name="$3"

mafft --adjustdirectionaccurately --thread $threads --auto $fasta_file > $name.aln.fa
iqtree -fast -s $name.aln.fa -nt AUTO -m GTR+I+G4 --redo
