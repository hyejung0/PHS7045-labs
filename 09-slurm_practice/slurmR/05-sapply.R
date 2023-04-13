#!/bin/sh
#SBATCH --job-name=sapply-sourceSlurm
#SBATCH --time=00:10:00
#SBATCH --account=notchpeak-shared-short
#SBATCH --partition=notchpeak-shared-short
/uufs/chpc.utah.edu/sys/installdir/r8/R/4.2.2/lib64/R/bin/Rscript --vanilla 05-sapply.R
