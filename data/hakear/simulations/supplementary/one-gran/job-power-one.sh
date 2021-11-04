#! /bin/bash
#SBATCH --job-name=power-one
#SBATCH --output=trace_%a.out
#SBATCH --error=error_%a.err
#SBATCH --mem-per-cpu=4096
#SBATCH --time=24:00:00
#SBATCH --array=1-12000
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/4.0.5
Rscript ../R-ind/power-one.R $SLURM_ARRAY_TASK_ID 
