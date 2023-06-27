## Required R packages 
1. dlmtree (version 0.8.0.0) 
	* Install from source `install.packages(path_to_file, repos = NULL, type="source")`
    ! Updated software found at github.com/danielmork/dlmtree
2. tidyverse (2.0.0)
3. ggplot2 (3.4.2)
4. viridis (0.6.3)
5. lubridate (1.9.2)
6. data.table (1.14.8)
7. units (0.8.2)
8. sf (1.0.13)
9. tigris (2.0.3)
10. raster (3.6.20)
11. gstat (2.1.1)
12. dplyr (1.1.2)
13. R.utils (2.12.2)


## Files

1. dlmtree_0.8.0.0.tar.gz : R package dlmtree

### Simulation
1. sim.R : replicate all simulation results
2. analyze.sim.dlmtree.R : function for simulation results
3. run.sim.R : produces results and tables
4. TDLM_vs_BART_sim.R : script for part of Figure 1
5. HDLM_vs_BART_sim.R : script for part of Figure 1
6. sim_res_waic.txt numerical output from simulation

### Data Analysis
1. data_analysis1.R : run data analysis models
2. data_analysis2.R : data analysis results
3. data_analysis3.R : data analysis cross validation

### Data Preparation
1. 1_getcmaq.R
2. 2_LimitCMAQToColorado.R
3. 3_GetEPAairdata.R
4. 4_IDW.R
5. 5_MergeCMAQandIDW.R
6. 6_prep_limit_birth_data.R
7. 7_make_analysis_file.R