#!/bin/bash
#SBATCH --job-name=ece-install
#SBATCH --account=haozhang
#SBATCH --partition=standard
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=0:10:00
#SBATCH --mem=2G
#SBATCH --output=logs/install.out
#SBATCH --error=logs/install.err

module load R

Rscript -e 'devtools::install(quiet = TRUE)'
