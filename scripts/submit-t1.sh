#!/bin/bash
# Run from project root: bash scripts/submit-t1.sh
# Regenerate param_grid.csv first if needed: Rscript scripts/make_grid.R
set -e

N=$(($(wc -l < scripts/param_grid.csv) - 1))
echo "Submitting array of $N tasks"

mkdir -p data logs

sbatch --array="1-${N}%500" --chdir="$(pwd)" scripts/run_array_t1.slurm
