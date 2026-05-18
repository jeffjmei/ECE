#!/bin/bash
#SBATCH --job-name=ece-sim
#SBATCH --account=haozhang
#SBATCH --partition=standard
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=2:00:00
#SBATCH --mem=2G
#SBATCH --mail-user=jmei@arizona.edu
#SBATCH --mail-type=ALL

module load R

Rscript scripts/simulate.R \
    --task_id "$SLURM_ARRAY_TASK_ID" \
    --config  scripts/param_grid.csv \
    --n_sims  1000 \
    --outdir  data/
