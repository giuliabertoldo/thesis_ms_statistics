#!/bin/bash

#SBATCH --time=00:05:00   # walltime
#SBATCH --ntasks=9 --cpus-per-task=36 --mem=2g
#SBATCH --account=intro_vsc35419
#SBATCH --mail-user=giulia.bertoldo@student.kuleuven.be   # email address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

#Submit this script with: sbatch thefilename

cd $SLURM_SUBMIT_DIR

module purge
module load R

Rscript sim_parallel_pb_no_orb_no_k_15.R
