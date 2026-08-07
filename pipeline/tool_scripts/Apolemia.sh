#!/bin/bash                                                                                                                                    
#SBATCH --job-name=apo                                                                                                                      
#SBATCH --output=apo_%j.log                                                                                                                 
#SBATCH --requeue                                                                                                                              
#SBATCH --time=2-00:00:00                                                                                                                      
#SBATCH --partition=ycga                                                                                                                       
#SBATCH --nodes=1                    # number of cores and nodes                                                                               
#SBATCH --cpus-per-task=16          # number of cores                                                                                          
#SBATCH --mem-per-cpu=8G             # shared memory, scaling with CPU request                                                                 
                                                                                                                                               
# Set up modules      
module purge
ml miniconda

conda activate siphgen

rm -r Apolemia_trees/
mkdir Apolemia_trees/

bash tool_scripts/genetree.sh --name Apolemia_16S --output_dir Apolemia_trees/ --shark_gene_name cnidaria_16S --mitos_gene_name rrnL --threads 16 --ext_ids Apolemia_IDs.txt --ext_seqs external_seqs/Apolemia_16S.fasta
bash tool_scripts/genetree.sh --name Apolemia_CO1 --output_dir Apolemia_trees/ --shark_gene_name cnidaria_CO1 --mitos_gene_name cox1 --threads 16 --ext_ids Apolemia_IDs.txt --ext_seqs external_seqs/Apolemia_COI.fasta 
bash tool_scripts/genetree.sh --name Apolemia_18S --output_dir Apolemia_trees/ --shark_gene_name cnidaria_18S --threads 16 --ext_ids Apolemia_IDs.txt  --ext_seqs external_seqs/Apolemia_18S.fasta
bash tool_scripts/genetree.sh --name Apolemia_CO2 --output_dir Apolemia_trees/ --shark_gene_name cnidaria_CO2 --mitos_gene_name cox2 --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_28S --output_dir Apolemia_trees/ --shark_gene_name cnidaria_28S --threads 16 --ext_ids Apolemia_IDs.txt 
bash tool_scripts/genetree.sh --name Apolemia_ITS --output_dir Apolemia_trees/ --shark_gene_name cnidaria_ITS --threads 16 --ext_ids Apolemia_IDs.txt 
bash tool_scripts/genetree.sh --name Apolemia_rrnS --output_dir Apolemia_trees/ --mitos_gene_name rrnS --threads 16 --ext_ids Apolemia_IDs.txt
bash tool_scripts/genetree.sh --name Apolemia_CO3 --output_dir Apolemia_trees/ --mitos_gene_name cox3 --threads 16 --ext_ids Apolemia_IDs.txt 
bash tool_scripts/genetree.sh --name Apolemia_cob --output_dir Apolemia_trees/ --mitos_gene_name cob --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_nad4 --output_dir Apolemia_trees/ --mitos_gene_name nad4 --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_nad1 --output_dir Apolemia_trees/ --mitos_gene_name nad1 --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_nad4l --output_dir Apolemia_trees/ --mitos_gene_name nad4l --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_nad3 --output_dir Apolemia_trees/ --mitos_gene_name nad3 --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_nad6 --output_dir Apolemia_trees/ --mitos_gene_name nad6 --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_nad5 --output_dir Apolemia_trees/ --mitos_gene_name nad5 --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_nad2 --output_dir Apolemia_trees/ --mitos_gene_name nad2 --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_atp6 --output_dir Apolemia_trees/ --mitos_gene_name atp6 --threads 16 --ext_ids Apolemia_IDs.txt  
bash tool_scripts/genetree.sh --name Apolemia_atp8 --output_dir Apolemia_trees/ --mitos_gene_name atp8 --threads 16 --ext_ids Apolemia_IDs.txt  

ml Python

python tool_scripts/ConcatFasta.py --files $(ls Apolemia_trees/*aln.fasta) --outfile Apolemia_trees/Apolemia_all.aln.fasta --part > Apolemia_trees/Apolemia_all.part
bash tool_scripts/grep_partition.sh Apolemia_trees/Apolemia_all.part Apolemia_trees/Apolemia_all.part.nex
iqtree -B 1000 -nt AUTO -p Apolemia_trees/Apolemia_all.part.nex -m MFP+MERGE --redo -s Apolemia_trees/Apolemia_all.aln.fasta

python tool_scripts/ConcatFasta.py --files \
    Apolemia_trees/Apolemia_18S.aln.fasta \
    Apolemia_trees/Apolemia_28S.aln.fasta \
    Apolemia_trees/Apolemia_ITS.aln.fasta \
    --outfile Apolemia_trees/Apolemia_nuc.aln.fasta --part > Apolemia_trees/Apolemia_nuc.part
bash tool_scripts/grep_partition.sh Apolemia_trees/Apolemia_nuc.part Apolemia_trees/Apolemia_nuc.part.nex
iqtree -B 1000 -nt AUTO -p Apolemia_trees/Apolemia_nuc.part.nex -m MFP+MERGE --redo -s Apolemia_trees/Apolemia_nuc.aln.fasta

python tool_scripts/ConcatFasta.py --files \
    Apolemia_trees/Apolemia_16S.aln.fasta \
    Apolemia_trees/Apolemia_CO1.aln.fasta \
    Apolemia_trees/Apolemia_CO2.aln.fasta \
    Apolemia_trees/Apolemia_rrnS.aln.fasta \
    Apolemia_trees/Apolemia_CO3.aln.fasta \
    Apolemia_trees/Apolemia_cob.aln.fasta \
    Apolemia_trees/Apolemia_nad4.aln.fasta \
    Apolemia_trees/Apolemia_nad1.aln.fasta \
    Apolemia_trees/Apolemia_nad4l.aln.fasta \
    Apolemia_trees/Apolemia_nad3.aln.fasta \
    Apolemia_trees/Apolemia_nad6.aln.fasta \
    Apolemia_trees/Apolemia_nad5.aln.fasta \
    Apolemia_trees/Apolemia_nad2.aln.fasta \
    Apolemia_trees/Apolemia_atp6.aln.fasta \
    Apolemia_trees/Apolemia_atp8.aln.fasta \
    --outfile Apolemia_trees/Apolemia_mit.aln.fasta --part > Apolemia_trees/Apolemia_mit.part
bash tool_scripts/grep_partition.sh Apolemia_trees/Apolemia_mit.part Apolemia_trees/Apolemia_mit.part.nex
iqtree -B 1000 -nt AUTO -p Apolemia_trees/Apolemia_mit.part.nex -m MFP+MERGE --redo -s Apolemia_trees/Apolemia_mit.aln.fasta

