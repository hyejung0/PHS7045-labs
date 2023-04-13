#!/bin/sh
#SBATCH --job-name=parsapply
#SBATCH --time=00:10:00
#SBATCH --account=notchpeak-shared-short
#SBATCH --partition=notchpeak-shared-short

module load R/4.2.2
Rscript --vanilla 03-parsapply-slurmr.R

