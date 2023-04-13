#!/bin/bash

#SBATCH --cluster=wice
#SBATCH --time=00:25:00   # walltime
#SBATCH --ntasks=9   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=2G   # memory per CPU core
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
Rscript test_sim_parallel_pb_str_orb_no_k_30.R
