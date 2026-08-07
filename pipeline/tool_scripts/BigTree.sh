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

rm -r BigTree_trees/
mkdir BigTree_trees/

bash tool_scripts/genetree.sh --name BigTree_16S --output_dir BigTree_trees/ --shark_gene_name cnidaria_16S --mitos_gene_name rrnL --threads 16  
bash tool_scripts/genetree.sh --name BigTree_CO1 --output_dir BigTree_trees/ --shark_gene_name cnidaria_CO1 --mitos_gene_name cox1 --threads 16  
bash tool_scripts/genetree.sh --name BigTree_18S --output_dir BigTree_trees/ --shark_gene_name cnidaria_18S --threads 16  
bash tool_scripts/genetree.sh --name BigTree_CO2 --output_dir BigTree_trees/ --shark_gene_name cnidaria_CO2 --mitos_gene_name cox2 --threads 16   
bash tool_scripts/genetree.sh --name BigTree_28S --output_dir BigTree_trees/ --shark_gene_name cnidaria_28S --threads 16  
bash tool_scripts/genetree.sh --name BigTree_ITS --output_dir BigTree_trees/ --shark_gene_name cnidaria_ITS --threads 16  
bash tool_scripts/genetree.sh --name BigTree_rrnS --output_dir BigTree_trees/ --mitos_gene_name rrnS --threads 16 
bash tool_scripts/genetree.sh --name BigTree_CO3 --output_dir BigTree_trees/ --mitos_gene_name cox3 --threads 16  
bash tool_scripts/genetree.sh --name BigTree_cob --output_dir BigTree_trees/ --mitos_gene_name cob --threads 16   
bash tool_scripts/genetree.sh --name BigTree_nad4 --output_dir BigTree_trees/ --mitos_gene_name nad4 --threads 16   
bash tool_scripts/genetree.sh --name BigTree_nad1 --output_dir BigTree_trees/ --mitos_gene_name nad1 --threads 16   
bash tool_scripts/genetree.sh --name BigTree_nad4l --output_dir BigTree_trees/ --mitos_gene_name nad4l --threads 16   
bash tool_scripts/genetree.sh --name BigTree_nad3 --output_dir BigTree_trees/ --mitos_gene_name nad3 --threads 16   
bash tool_scripts/genetree.sh --name BigTree_nad6 --output_dir BigTree_trees/ --mitos_gene_name nad6 --threads 16   
bash tool_scripts/genetree.sh --name BigTree_nad5 --output_dir BigTree_trees/ --mitos_gene_name nad5 --threads 16   
bash tool_scripts/genetree.sh --name BigTree_nad2 --output_dir BigTree_trees/ --mitos_gene_name nad2 --threads 16   
bash tool_scripts/genetree.sh --name BigTree_atp6 --output_dir BigTree_trees/ --mitos_gene_name atp6 --threads 16   
bash tool_scripts/genetree.sh --name BigTree_atp8 --output_dir BigTree_trees/ --mitos_gene_name atp8 --threads 16   

ml Python

python tool_scripts/ConcatFasta.py --files $(ls BigTree_trees/*aln.fasta) --outfile BigTree_trees/BigTree_all.aln.fasta --part > BigTree_trees/BigTree_all.part
bash tool_scripts/grep_partition.sh BigTree_trees/BigTree_all.part BigTree_trees/BigTree_all.part.nex
iqtree -B 1000 -nt AUTO -p BigTree_trees/BigTree_all.part.nex -m MFP+MERGE --redo -s BigTree_trees/BigTree_all.aln.fasta

python tool_scripts/ConcatFasta.py --files \
    BigTree_trees/BigTree_18S.aln.fasta \
    BigTree_trees/BigTree_28S.aln.fasta \
    BigTree_trees/BigTree_ITS.aln.fasta \
    --outfile BigTree_trees/BigTree_nuc.aln.fasta --part > BigTree_trees/BigTree_nuc.part
bash tool_scripts/grep_partition.sh BigTree_trees/BigTree_nuc.part BigTree_trees/BigTree_nuc.part.nex
iqtree -B 1000 -nt AUTO -p BigTree_trees/BigTree_nuc.part.nex -m MFP+MERGE --redo -s BigTree_trees/BigTree_nuc.aln.fasta

python tool_scripts/ConcatFasta.py --files \
    BigTree_trees/BigTree_16S.aln.fasta \
    BigTree_trees/BigTree_CO1.aln.fasta \
    BigTree_trees/BigTree_CO2.aln.fasta \
    BigTree_trees/BigTree_rrnS.aln.fasta \
    BigTree_trees/BigTree_CO3.aln.fasta \
    BigTree_trees/BigTree_cob.aln.fasta \
    BigTree_trees/BigTree_nad4.aln.fasta \
    BigTree_trees/BigTree_nad1.aln.fasta \
    BigTree_trees/BigTree_nad4l.aln.fasta \
    BigTree_trees/BigTree_nad3.aln.fasta \
    BigTree_trees/BigTree_nad6.aln.fasta \
    BigTree_trees/BigTree_nad5.aln.fasta \
    BigTree_trees/BigTree_nad2.aln.fasta \
    BigTree_trees/BigTree_atp6.aln.fasta \
    BigTree_trees/BigTree_atp8.aln.fasta \
    --outfile BigTree_trees/BigTree_mit.aln.fasta --part > BigTree_trees/BigTree_mit.part
bash tool_scripts/grep_partition.sh BigTree_trees/BigTree_mit.part BigTree_trees/BigTree_mit.part.nex
iqtree -B 1000 -nt AUTO -p BigTree_trees/BigTree_mit.part.nex -m MFP+MERGE --redo -s BigTree_trees/BigTree_mit.aln.fasta

