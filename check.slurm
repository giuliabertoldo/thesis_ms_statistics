#!/bin/bash

#SBATCH --time=00:20:00   # walltime
#SBATCH --ntasks=2 --cpus-per-task=3 --mem=2g
#SBATCH --account=intro_vsc35419
#SBATCH --mail-user=giulia.bertoldo@student.kuleuven.be   # email address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

#Submit this script with: sbatch thefilename

cd $SLURM_SUBMIT_DIR

module purge
module load R

export OMP_NUM_THREADS=2
Rscript check.R
