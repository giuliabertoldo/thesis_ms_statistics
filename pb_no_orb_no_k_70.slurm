#!/bin/bash -l

#SBATCH --time=01:30:00   # walltime
#SBATCH --ntasks=1 --cpus-per-task=4 --mem=2g
#SBATCH --account=intro_vsc35419
#SBATCH --mail-user=giulia.bertoldo@student.kuleuven.be   # email address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

#Submit this script with: sbatch thefilename

cd $SLURM_SUBMIT_DIR

module purge
module load R

Rscript sim_parallel_pb_no_orb_no_k_70.R ${SLURM_ARRAY_TASK_ID}
