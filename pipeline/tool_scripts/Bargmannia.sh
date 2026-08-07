#!/bin/bash                                                                                                                                    
#SBATCH --job-name=barg                                                                                                                      
#SBATCH --output=barg_%j.log                                                                                                                 
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

rm -r Bargmannia_trees/
mkdir Bargmannia_trees/

bash tool_scripts/genetree.sh --name Bargmannia_16S --output_dir Bargmannia_trees/ --shark_gene_name cnidaria_16S --mitos_gene_name rrnL --threads 16 --ext_ids Bargmannia_IDs.txt --ext_seqs external_seqs/Bargmannia_16S.fasta 
bash tool_scripts/genetree.sh --name Bargmannia_CO1 --output_dir Bargmannia_trees/ --shark_gene_name cnidaria_CO1 --mitos_gene_name cox1 --threads 16 --ext_ids Bargmannia_IDs.txt --ext_seqs external_seqs/Bargmannia_CO1.fasta
bash tool_scripts/genetree.sh --name Bargmannia_18S --output_dir Bargmannia_trees/ --shark_gene_name cnidaria_18S --threads 16 --ext_ids Bargmannia_IDs.txt --ext_seqs external_seqs/Bargmannia_18S.fasta 
bash tool_scripts/genetree.sh --name Bargmannia_CO2 --output_dir Bargmannia_trees/ --shark_gene_name cnidaria_CO2 --mitos_gene_name cox2 --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_28S --output_dir Bargmannia_trees/ --shark_gene_name cnidaria_28S --threads 16 --ext_ids Bargmannia_IDs.txt 
bash tool_scripts/genetree.sh --name Bargmannia_ITS --output_dir Bargmannia_trees/ --shark_gene_name cnidaria_ITS --threads 16 --ext_ids Bargmannia_IDs.txt 
bash tool_scripts/genetree.sh --name Bargmannia_rrnS --output_dir Bargmannia_trees/ --mitos_gene_name rrnS --threads 16 --ext_ids Bargmannia_IDs.txt
bash tool_scripts/genetree.sh --name Bargmannia_CO3 --output_dir Bargmannia_trees/ --mitos_gene_name cox3 --threads 16 --ext_ids Bargmannia_IDs.txt 
bash tool_scripts/genetree.sh --name Bargmannia_cob --output_dir Bargmannia_trees/ --mitos_gene_name cob --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_nad4 --output_dir Bargmannia_trees/ --mitos_gene_name nad4 --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_nad1 --output_dir Bargmannia_trees/ --mitos_gene_name nad1 --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_nad4l --output_dir Bargmannia_trees/ --mitos_gene_name nad4l --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_nad3 --output_dir Bargmannia_trees/ --mitos_gene_name nad3 --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_nad6 --output_dir Bargmannia_trees/ --mitos_gene_name nad6 --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_nad5 --output_dir Bargmannia_trees/ --mitos_gene_name nad5 --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_nad2 --output_dir Bargmannia_trees/ --mitos_gene_name nad2 --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_atp6 --output_dir Bargmannia_trees/ --mitos_gene_name atp6 --threads 16 --ext_ids Bargmannia_IDs.txt  
bash tool_scripts/genetree.sh --name Bargmannia_atp8 --output_dir Bargmannia_trees/ --mitos_gene_name atp8 --threads 16 --ext_ids Bargmannia_IDs.txt  

ml Python

python tool_scripts/ConcatFasta.py --files $(ls Bargmannia_trees/*aln.fasta) --outfile Bargmannia_trees/Bargmannia_all.aln.fasta --part > Bargmannia_trees/Bargmannia_all.part
bash tool_scripts/grep_partition.sh Bargmannia_trees/Bargmannia_all.part Bargmannia_trees/Bargmannia_all.part.nex
iqtree -B 1000 -nt AUTO -p Bargmannia_trees/Bargmannia_all.part.nex -m MFP+MERGE --redo -s Bargmannia_trees/Bargmannia_all.aln.fasta

python tool_scripts/ConcatFasta.py --files \
    Bargmannia_trees/Bargmannia_18S.aln.fasta \
    Bargmannia_trees/Bargmannia_28S.aln.fasta \
    Bargmannia_trees/Bargmannia_ITS.aln.fasta \
    --outfile Bargmannia_trees/Bargmannia_nuc.aln.fasta --part > Bargmannia_trees/Bargmannia_nuc.part
bash tool_scripts/grep_partition.sh Bargmannia_trees/Bargmannia_nuc.part Bargmannia_trees/Bargmannia_nuc.part.nex
iqtree -B 1000 -nt AUTO -p Bargmannia_trees/Bargmannia_nuc.part.nex -m MFP+MERGE --redo -s Bargmannia_trees/Bargmannia_nuc.aln.fasta

python tool_scripts/ConcatFasta.py --files \
    Bargmannia_trees/Bargmannia_16S.aln.fasta \
    Bargmannia_trees/Bargmannia_CO1.aln.fasta \
    Bargmannia_trees/Bargmannia_CO2.aln.fasta \
    Bargmannia_trees/Bargmannia_rrnS.aln.fasta \
    Bargmannia_trees/Bargmannia_CO3.aln.fasta \
    Bargmannia_trees/Bargmannia_cob.aln.fasta \
    Bargmannia_trees/Bargmannia_nad4.aln.fasta \
    Bargmannia_trees/Bargmannia_nad1.aln.fasta \
    Bargmannia_trees/Bargmannia_nad4l.aln.fasta \
    Bargmannia_trees/Bargmannia_nad3.aln.fasta \
    Bargmannia_trees/Bargmannia_nad6.aln.fasta \
    Bargmannia_trees/Bargmannia_nad5.aln.fasta \
    Bargmannia_trees/Bargmannia_nad2.aln.fasta \
    Bargmannia_trees/Bargmannia_atp6.aln.fasta \
    Bargmannia_trees/Bargmannia_atp8.aln.fasta \
    --outfile Bargmannia_trees/Bargmannia_mit.aln.fasta --part > Bargmannia_trees/Bargmannia_mit.part
bash tool_scripts/grep_partition.sh Bargmannia_trees/Bargmannia_mit.part Bargmannia_trees/Bargmannia_mit.part.nex
iqtree -B 1000 -nt AUTO -p Bargmannia_trees/Bargmannia_mit.part.nex -m MFP+MERGE --redo -s Bargmannia_trees/Bargmannia_mit.aln.fasta

