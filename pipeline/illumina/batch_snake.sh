#!/bin/bash                                                                                                                                    
#SBATCH --job-name=siphgen                                                                                                                      
#SBATCH --output=siphgen_%j.log                                                                                                                 
#SBATCH --requeue                                                                                                                              
#SBATCH --time=2-00:00:00                                                                                                                      
#SBATCH --partition=ycga                                                                                                                       
#SBATCH --nodes=1                    # number of cores and nodes                                                                               
#SBATCH --cpus-per-task=8          # number of cores                                                                                          
#SBATCH --mem-per-cpu=8G             # shared memory, scaling with CPU request                                                                 
                                                                                                                                               
# Set up modules      
module purge # Unload any existing modules that might conflict
module load miniconda
module load MAFFT
module load Trimmomatic
module list

conda activate siphgen


snakemake --latency-wait 30 --rerun-incomplete --workflow-profile /vast/palmer/pi/dunn/sc2962/20250114_Siph_Genomics/workflow_profile \
       --cores 8 --use-envmodules \
