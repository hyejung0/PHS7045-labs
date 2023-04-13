#!/bin/sh
#SBATCH --job-name=slurm_sapply
#SBATCH --time=00:10:00
#SBATH --account=notchpeak-shared-short
#SBATH --partition=notchpeak-sahred-short
module load R/4.2.2
Rscript --vanilla 04-slurm_sapply.R

