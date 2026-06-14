#!/bin/bash
#SBATCH --job-name=ece-sim
#SBATCH --account=haozhang
#SBATCH --partition=standard
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=3:00:00
#SBATCH --mem=3G
#SBATCH --output=logs/%A_%a.out
#SBATCH --error=logs/%A_%a.err
#SBATCH --open-mode=append
#SBATCH --mail-user=jmei@arizona.edu
#SBATCH --mail-type=ALL

module load R

Rscript scripts/simulate.R \
	--task_id "$SLURM_ARRAY_TASK_ID" \
	--config scripts/param_grid.csv \
	--outdir data/ \
	--cpus "$SLURM_CPUS_PER_TASK"
