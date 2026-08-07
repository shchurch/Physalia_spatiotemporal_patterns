#!/bin/bash                                                                                                                                    
#SBATCH --job-name=siphgen                                                                                                                      
#SBATCH --output=siphgen_%j.log                                                                                                                 
#SBATCH --requeue                                                                                                                              
#SBATCH --time=2-00:00:00                                                                                                                      
#SBATCH --partition=ycga_bigmem                                                                                                                       
#SBATCH --nodes=1                    # number of cores and nodes                                                                               
#SBATCH --cpus-per-task=24          # number of cores                                                                                          
#SBATCH --mem-per-cpu=12G             # shared memory, scaling with CPU request                                                                 
                                                                                                                                               
# Set up modules      
module purge # Unload any existing modules that might conflict
module load miniconda
module load MAFFT
ml FastQC
ml minimap2
ml seqtk
module load SAMtools
module load Trimmomatic
module list
conda activate siphgen_flye

snakemake --rerun-incomplete --cores 24
