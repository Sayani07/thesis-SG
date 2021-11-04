#! /bin/bash
#SBATCH --job-name=size-nullonly
#SBATCH --output=trace_%a.out
#SBATCH --error=error_%a.err
#SBATCH --mem-per-cpu=4096
#SBATCH --time=168:00:00
#SBATCH --array=1-1000
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/4.0.0-openblas
Rscript ../R-ind/size-nullonly.R $SLURM_ARRAY_TASK_ID 