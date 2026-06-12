#!/bin/bash
# Run from project root: bash scripts/submit.sh [--time H:MM:SS] [--mem XG]
# Regenerate param_grid.csv first if needed: Rscript scripts/make_grid.R
set -e

TIME=3:00:00
MEM=3G

while [[ $# -gt 0 ]]; do
  case $1 in
    --time) TIME=$2; shift 2 ;;
    --mem)  MEM=$2;  shift 2 ;;
    *) echo "Unknown argument: $1"; exit 1 ;;
  esac
done

N=$(($(wc -l < scripts/param_grid.csv) - 1))
echo "Submitting array of $N tasks (time=$TIME, mem=$MEM)"

mkdir -p data logs

sbatch --time="$TIME" --mem="$MEM" --array="1-${N}%500" --chdir="$(pwd)" scripts/run_array.slurm
