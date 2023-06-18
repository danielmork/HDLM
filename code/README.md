## Required R packages 
1. dlmtree (version 0.8.0.0) 
	* Install from source `install.packages(path_to_file, repos = NULL, type="source")`
    ! Updated software found at github.com/danielmork/dlmtree
2. tidyverse (install from CRAN)
3. ggplot2 (install from CRAN)
4. viridis (install from CRAN)
5. lubridate (install from CRAN)
6. data.table (install from CRAN)
7. units (install from CRAN)
8. sf (install from CRAN)
9. tigris (install from CRAN)
10. raster (install from CRAN)
11. gstat (install from CRAN)
12. dplyr (install from CRAN)
13. R.utils ()


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