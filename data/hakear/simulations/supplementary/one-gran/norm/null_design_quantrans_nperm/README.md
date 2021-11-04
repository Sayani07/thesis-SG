# All simulation scenarios under null design after quantile transforming the data to standard normal distribution and generating 100 permutations to scale by mean and standard deviation

**Objective:**  Behavior of raw wpd under different null designs N(0,1), N(5, 1), N(0,5) and N(5,5), Gamma(0,1), Gamma(0,5) for different nx and nfacets. Here, we have to fix a value of lambda = 0.67.

**Assumptions:** There is no difference in distribution between any facet or x-category
nsim = 200
lambda = 0.67

**Questions:** 
 - How raw value of wpd changes with different nx and nfacet for different location and scale of a Normal and non-normal distribution  

**Folder structure:**  
 - `R-ind`:  R files for running raw and norm wpd  
 - `job`:  slurm job .sh files calling `R-ind` scripts for running raw and norm wpd in MonARCH  
 - `data-ind`:  output files from `job`  
 - `R-agg`:  R files for aggregating   outputs from `data-ind` into `data-agg` and plotting  
 - `data-agg`:  output files from `R-agg`  
 - `figs`: plots generated from `data-agg` for each simulation scenario  



