ls
cd ../
ls
cd vary_all
ls
cd raw
ls
cd data-ind
ls
rm  wpd_Gamma01/*
ls
rm  wpd_Gamma21/*
rm  wpd_N01/*
rm  wpd_N05/*
rm  wpd_N51/*
rm  wpd_N55/*
ls
rm wpd_Gamma21/*
rm wpd_Gamma21/
rm wpd_Gamma21/*
rm #This script calculates raw mmpd for each simulation scenario*
##Read Simulation Table
.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(drake)
library(tidyverse)
library(hakear)
set.seed(9999)
nsim = 200
# change path while running it on HPC
# simtable<-read_csv(here::here('simulations/null/sim_table.csv'))
simtable<-read_csv('../sim_table.csv')
### Extract flags from simulation scenario
#scen<- 2 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this
simj<-simtable[scen,] #Extract row of table
nfacetj<-simj$nfacet # Which nfacet level
nxj<-simj$nx #Which nx level
wj <- simj$w
#create data for each row for null normal
sim_varall = function(nx,
                      nfacet,
                      shape = 0.5,
                      rate = 1,
                      w)
{   dist_gamma((shape + seq(0, (nx*nfacet - 1), by  = 1)*w), rate)
}
sim_panel_data = 
  hakear::sim_panel(nx = nxj,
                    nfacet = nfacetj, 
                    ntimes = 500, 
                    sim_dist = sim_varall(nx = nxj, nfacet = nfacetj, w = wj)) %>% 
  unnest(c(data)) %>% ungroup() %>% 
  bind_cols(w = wj)
set.seed(1111)
raw_dist <- map(seq_len(nsim), function(i)
{   new_sim_data = sample(sim_panel_data$sim_data, 
                        size = nrow(sim_panel_data))
  new_data = sim_panel_data %>% 
    select(-sim_data) %>% 
    mutate(sim_data = new_sim_data)
  
  # for creating one raw mmpd
  raw_mmpd = compute_pairwise_max(new_data, 
                                  gran_x = "id_x",
                                  gran_facet = "id_facet",
                                  response = sim_data) %>% 
    as_tibble() %>% mutate(perm_id = i)
  
}) %>% bind_rows()
{   dir.create("simulations/vary_all/raw/data-ind/wpd_Gamma01")
}
saveRDS(raw_dist,
        paste0('../data-ind/wpd_Gamma01/',
               nxj,'_',
               nfacetj,"_",
               wj, '_wpd.rds'))
la
ls
cd ../
ls
cd job
ls
sbatch job-raw-Gamma01.sh
sbatch job-raw-Gamma21.sh
sbatch job-raw-N01.sh
sbatch job-raw-N05.sh
sbatch job-raw-N51.sh
sbatch job-raw-N55.sh
ls
cd zo93/
ls
squeue -u sgup0008
ls
squeue -u sgup0008
ls
cd zo93
squeue -u sgup0008
scancel 6647513
scancel 6647514
scancel 6647515
scancel 6647516
scancel 6647517
squeue -u sgup0008
ls
cd simulations/vary_x
ls
cd null_design_quantrans/
ls
cd job
ls
sbatch job-raw-Gamma01.sh
sbatch job-raw-Gamma21.sh
sbatch job-raw-N01.sh
sbatch job-raw-N05.sh
sbatch job-raw-N51.sh
sbatch job-raw-N55.sh
squeue -u sgup0008
scancel 6650283
scancel 6650282
scancel 6650280

squeue -u sgup0008
scancel 6650284
squeue -u sgup0008
ls
cd zo93
squeue -u sgup0008
ls
cd simulations
ls
cd vary_all
ls
cd raw
ls
cd job
ls
rm err*
ls
rm trace_*
ls
nano job-raw-N01.sh
nano ../R-ind/raw_pairwise_N01.R
sbatch job-raw-N01.sh
cd ../../
ls
cd ../
ls
cd vary_facet
ls
cd raw
cd job
ls
rm err*
rm trace_*
ls
sbatch job-raw-N01.sh 
squeue -u sgup0008
ls
cd zo93
ls
cd simulations
ls
cd vary_All
cd vary_all
ls
cd norm
ls
cd job
ls
nano job-raw-N01.sh
ls
cd ../
ls
cd ../
ls
squeue -u sgup0008
ls
cd norm
cd job
ls
nano job-norm-N01.sh
sbatch job-norm-N01.sh
cd ../
ls
cd vary_x
cd job
cd norm/job
ls
sbatch job-norm-N01.SH
sbatch job-norm-N01.sh
cd ../../
ls
cd ../
cd vary_facet
ls
cd norm/job
ls
nano job-norm-N01.sh
sbatch job-norm-N01.sh
ls
cd zo93
ls
cd simulations
ls
squeue -u sgup0008
ls
cd zo93
ls
cd zo93
ls
cd paper
ls
cd sim_table
ls
cd ../
ls
cd R
nano wpd_all_households.R 
cd ../
ls
cd simulations_old
ls
cd null
ls
module load R/4.0.0-openblas
ls
nano updatepkg.R 
Rscript updatepkg.R 
nano updatepkg.R 
Rscript updatepkg.R 
cd ../
cd../
ls
cd ../
cd
ls
cd zo93/
ls
cd paper
ls
cd R
nano wpd_all_households.R 
Rscript wpd_all_households.R 
nano  wpd_all_households.R 
Rscript wpd_all_households.R 
nano  wpd_all_households.R 
ls
nano updatepkg.R 
Rscript  updatepkg.R 
nano updatepkg.R 
Rscript  updatepkg.R 
ls
Rscript wpd_all_households.R 
nano wpd_all_households.R 
nano job_all_households.R 
nano job_all_households.sh 
sbatch job_all_households.sh 
squeue -u sgup0008
sbatch cancel 6651698
scancel 6651698
scancel 6651695_
squeue -u sgup0008
scancel 6651695
squeue -u sgup0008
scancel 6651693
squeue -u sgup000
squeue -u sgup0008
show_job 6671975
squeue -u sgup0008
cd ../cc/
cd ../../
ls
cd simulations/supplementary/
ls
cd R
cd R-ind
ls
nano wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
nano  wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
nano  wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
cd ../
cd data-ind
ls
cd wpd_N01/
ls
R
cd ../
ls
cd ../
ls
cd r
cd R
ls
cd R-ind
nano wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
nano wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
nano wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
nano wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
nano wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
nano wpd_pairwise_N01.R 
Rscript wpd_pairwise_N01.R 
nano  wpd_pairwise_N01.R 
Rscript  wpd_pairwise_N01.R 
nano  wpd_pairwise_N01.R 
Rscript  wpd_pairwise_N01.R 
nano  wpd_pairwise_N01.R 
R
nano  wpd_pairwise_N01.R 
Rscript  wpd_pairwise_N01.R 
nano  wpd_pairwise_N01.R 
Rscript  wpd_pairwise_N01.R 
nano  wpd_pairwise_N01.R 
Rscript  wpd_pairwise_N01.R 
nano  wpd_pairwise_N01.R 
R
nano  wpd_pairwise_N01.R 
cd ../
cd job
ls
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
squeu -u sgup0008

scancel 6672120
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
cd ../
ls
cd R
cd R-ind
ls
nano wpd_pairwise_N01.R wpd_pairwise_N01.R 
nano wpd_pairwise_N01.R
R
cd ../
cd job
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
squeue -u sgup0008
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
nano job-raw-N01.sh 
squeue -u sgup0008
cd 
cd ~/Documents/paper-hakear
ls
cd zo93/
ls
cd simulations
ls
squeue -u sgup0008
ls
squeue -u sgup0008
ls
ccd zo93
cd zo93
ls
ls simulations
cd supplementary
cd simulations/supplementary
ls
ls data-ind
cd wpd_N01
cd data-ind/wpd-N01
cd data-ind/wpd_N01
ls
cd zo93/
ls
cd simulations
cd supplementary/
ls
cd ../
ls
cd supplementary/
ls
cd R-ind
ls
cd ../
ls
cd ../
ls
cd ../
ls
cd simulations
ls
cd ../
ls
cd simulations_old
ls
cd simulations_old
ls
cd null
ls
module load R/4.0.0-openblas
nano updatepkg.R 
Rscript updatepkg.R 
nano updatepkg.R 
cd ../
ls
cd simulations
cd supplementary/
ls
cd job
ls
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
nano job-raw-N01.sh 
nano job1000-raw-N01.sh 
cd ../
ls
cd R-ind
ls
nano wpd_pairwise_N01.R
ls
nano wpd_pairwise_N01.R
Rscript wpd_pairwise_N01.R 
ls
nano wpd_pairwise_N01.R
Rscript wpd_pairwise_N01.R 
nano wpd_pairwise_N01.R
Rscript wpd_pairwise_N01.R 
ls
cd zo93
ls
cd simulations
ls
cd supplementary/
ls
cd R-ind
ls
module load R/4.0.0-openblas
ls
Rscript wpd_pairwise_N01.R 
nano wpd_pairwise_N01.R 
cd ../
 ls
cd job
nano job-raw-N01.sh 
sbatch job-raw-N01.sh 
nano job-raw-N01.sh 
sbatch job-1001-2000.sh.sh 
nano job-1001-2000.sh
sbatch job-1001-2000.sh 
nano job-2001-3000.sh
sbatch job-2001-3000.sh 
nano job_3001-4000.sh
sbatch job_3001-4000.sh
nano job_4001-5000.sh
sbatch job_4001-5000.sh
ls
nano job-1001-2000.sh
nano job_4001-5000.sh
R
cd ../
ls
cd sim_table/
ls
Rscript simtable.R
nano simtable.R
Rscript simtable.R
squeue -u sgup0008
scancel
scancel sgup0008
scancel -u sgup0008
squeue -u sgup0008
scancel -u sgup0008
squeue -u sgup0008
cd ../
ls
cd job
ls
rm err*
ls
rm out*
rm .out*
rm trace*
ls
sbatch job_4001-5000.sh 
cd ../
cd R-ind
ls
cd ../
cd sim_table/
ls
R
cd ../
cd sim_table/
nano simtable.R 
Rscript simtable.R 
R
cd ../
cd job
ls
job_4001-5000.sh
sbatch job_4001-5000.sh
nano job_4001-5000.sh
sbatch job_4001-5000.sh
nano job_4501-5000.sh
sbatch job_4501-5000.sh
sbatch job-1001-2000.sh
sbatch job-2001-3000.sh
sbatch job-raw-N01.sh
nano job_5001-6000.sh
sbatch job_5001-6000.sh
nano job_5001-5500.sh
sbatch  job_5001-5500.sh
squeue -u sgup0008
ls
cd zo93/
cd simulations
cd supplementary/
ls
ls data-ind
cd data-ind/
cd wpd_N01/
ls
ls - l
cd ../
ls -l
ls
cd wpd_N01/
ls
SQ -u sgup0008
SQ -u sgup0008
ls
cd zo93
ls
cd simulations/supplementary/
ls
cd data-ind/
ls
cd wpd_N01/
ls
cd zo93/
ls
cd simulations
ls
cd ../
ls
cd paper
ls
cd sim_table/
ls
cd ../
ls
cd R
ls
rm err*
ls
rm trace*
ls
cd zo93/
cd paper
cd sim_table/
ls
cd ../
ls
cd 
cd R
ls
cd../
cd zo93
ls
cd ../
ls
cd zo93/
cd paper/
ls
cd R
ls
nano scalar_trans_wpd_all_households.R 
nano wpd_all_households.R 
 
nano scalar_trans_wpd_all_households.R 
Rscript  scalar_trans_wpd_all_households.R 
module load R/4.0.0-openblas
ls
Rscript  scalar_trans_wpd_all_households.R 
ls
cd ../
ls
cd R
ls
Rscript updatepkg.R 
ls
cd ../
ls
cd sim_table/
ls
cd results_norm_scalar/
ls
cd ../
ls
cd ../
ls
cd R
ls
Rscript scalar_trans_wpd_all_households.R 
nano scalar_trans_wpd_all_households.R 
R
ls
nano updatepkg.R 
Rscri
Rscript updatepkg.R 
ls
Rscript scalar_trans_wpd_all_households.R 
nano scalar_trans_wpd_all_households.R 
Rscript scalar_trans_wpd_all_households.R 
ls
cd ../
ls
cd sim_table/
ls
cd results_norm_scalar
ls
R
nano scalar_trans_wpd_all_households.R 
cd ../
ls
cd ../
ls
cd R
nano scalar_trans_wpd_all_households.Rouseholds.R 
ls
nano job_scalar_trans_all_households.sh 
sbatch job_scalar_trans_all_households.sh 
squeue -u sgup0008
scontrol hold 6772727
scontrol hold 6772727_
scontrol hold comp job_4000
scontrol view
scontrol suspend 6772727
scontrol hold  6772727_*
scontrol hold  6772727*
scontrol hold 6772795_
scontrol hold 6772795
scontrol hold 6772796
scontrol hold 6772797
scontrol hold 6772794
scontrol release 6796377
squeue -u sgup0008
scontrol hold 6772794_*
scontrol hold 6772794_
scontrol hold 6772794
scontrol hold 6772727
squeue -u sgup0008
scancel 6772727
squeue -u sgup0008
scontrol hold 6772794
squeue -u sgup0008

squeue -u sgup0008
ls
cd zo93/
ls
squeue -u sgup0008
ls
cd zo93/
ls
cd simulations
ls
cdsu
cd supplementary/
ls
cd test-hpc
ls
cd R-ind
module load R/4.0.0-openblas
ls
nano size-test.R 
Rscript size-test.R 
nano size-test.R 
Rscript size-test.R 
nano size-test.R 
Rscript size-test.R 
cd ../
ls
cd ../
ls
cd ../
ls
cd ../
ls
cd simulations
ls
cd supplementary/
ls
cd ..
cd simulations_old
ls
cd null
ls
Rscript updatepkg.R
R
cd ../
ls
cd ../
ls
cd simulations
ls
cd supplementary/
ls
cd R-ind
ls
cd ../
ls
cd ../
ls
cd simulations_old
ls
cd null
ks
ls
nano updatepkg.R 
Rscript updatepkg.R 
R
ls
module load R/4.0.0-openblas
ls
R
ls
cd zo93/
ls
cd paper
ls
ls R
rm trace*
rm trace_*
rm err*
rm *err
nano updatepkg.R
Rscript updatepkg.R
R
ls
cd ..
ls
cd simulations
ls
cd supplementary
ls
cd test-hpc
ls
cd R-ind
ls
nano size-test.R 
Rscript size-test.R 
ls
cd ../
ls
cd data-ind
ls
cd wpd_N01/
ls
R
ls
cd..
cd../
cd ../
ls
cd ../
ls
cd R-ind
ls
nano size-test.R 
Rscript size-test.R 
nano size-test.R 
Rscript size-test.R 
nano size-test.R 
Rscript size-test.R 
nano size-test.R 
nano size-test.job
nano hob-size-test.sh
cd ..
ls
cd job
nano job-size-test.sh
squeue -u sgup0008
sbatch job-size-test.sh 
squeue -u sgup0008
ls
cd zo93/
ls
squeue -u sgup0008
cd zo93/
ls
cd sgup0008
ls
cd paper-graclust
ls
cd script
ls
module load R/4.0.0-openblas
ls
nano smart_meter.R 
Rscript smart_meter.R 
nano smart_meter.R 
Rscript smart_meter.R 
nano smart_meter.R 
Rscript smart_meter.R 
q() q()
nano smart_meter.R 
Rscript smart_meter.R 
nano smart_meter.R 
Rscript smart_meter.R 
cd ../
ls
cd data-raw
ls
R
cd ~/Documents/paper-hakear
ls
zo93
cd zo93
ls
squeue -u sgup0008
scancel 6772794
ls
squeue -u sgup0008
$ scontrol suspend 6772795
scontrol suspend 6772795
squeue -u sgup0008
scontrol suspend 6772795
squeue -u sgup0008
scontrol suspend 6772795_
scontrol suspend 6772795*
scontrol suspend *6772795
scontrol suspend 6772795
squeue -u sgup0008
ls
cd zo93/
ls
cd paper
ls
cd ../
ls
cd simulations
ls
cd norm
ls
cd null_design_quantrans_nperm/
ls
cd data-agg
ls
cd data-ind
cd .
ls
cd ../
cd data-ind
ls
ls wpd_N01
ls Gamma21
ls wpd_Gamma21
ls wpd_N55
cd ../
ls
cd R-ind
ls
nano norm_pairwise_N01.R
ls
R
module load R/4.0.0-openblas
ls
R
ls
nano norm_pairwise_N01.R
 cd..
 cd ../
ls
cd job
ls
rm .err
rm error_
rm * error
rm .err *
rm .err*
rm * .err
nano b-norm-N01.sh 
nano job-norm-N01.sh 
nano job-norm-Gamm01.sh 
ls
cd ../
ls
cd job
ls
ls 
nano job-norm-N01.sh
sbatch job-norm-N01.sh
squeue -u sgup0008
scancel job-norm-N01.sh
cancel job-norm-N01.sh
scancel 7233322
ls
squeue -u sgup0008
nano job-norm-N01.sh
sbatch job-norm-N01.sh
squeue -u sgup0008
cd ../
ls
cd ../
lsd
ls
cd ../
ls
cd raw
ls
cd null_design_quantrans/
ls
cd R-ind
ls
nano raw_pairwise_Gamma21.R
nano raw_pairwise_Gamma01.R
nano raw_pairwise_N01.R
cd ../
ls
cd job
ls
cd job-raw-Gamma21.sh
nano job-raw-Gamma21.sh
ls
nano job-raw-N01.sh
sbatch job-raw-N01.sh
sbatch job-raw-Gamma21.sh
squeue -u sgup0008
cd ..
cd ../
l
ls
cd norm
ls
cd null_design_quantrans
l
ls
cd R-ind
ls
nano norm_pairwise_N01.R
cd ../
ls
cd ..
cd 
ls
cd zo93/
ls
cd simulations
ls
cd norm
ls
cd null_design_quantrans_nperm
ls
cd R-ind
ls
nano norm_pairwise_N01.R
nano norm_pairwise_Gamma21.R
cd ../
ls
cd job
ls
nano job-norm-Gamma21.sh
submit job-norm-Gamma21.sh
sbatch job-norm-Gamma21.sh
squeue -u sgup0008
ls
squeue -u sgup0008
cd zo93
l
ls
cd simulations
ls
cd norm
ls
cd null_design_quantrans_nperm
ls
cd R-ind
ls
cd ../
ls
cd data-ind
ls
ls wpd_N01
ls wpd_Gamma21
squeue -u sgup0008
