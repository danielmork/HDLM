Heterogeneous Distributed Lag Models to Estimate Personalized Effects of Maternal Exposures to Air Pollution
================

## Article Abstract

Children's health studies support an association between maternal environmental exposures and children's birth outcomes. A common goal is to identify critical windows of susceptibility--periods during gestation with increased association between maternal exposures and a future outcome.  The timing of the critical windows and magnitude of the associations are likely heterogeneous across different levels of individual, family, and neighborhood characteristics. Using an administrative Colorado birth cohort we estimate the individualized relationship between weekly exposures to fine particulate matter (PM2.5) during gestation and birth weight. To achieve this goal, we propose a statistical learning method combining distributed lag models and Bayesian additive regression trees to estimate critical windows at the individual level and identify characteristics that induce heterogeneity from a high-dimensional set of potential modifying factors. We find evidence of heterogeneity in the PM2.5–birth weight relationship, with some mother-child dyads showing a 3 times larger decrease in birth weight for an IQR increase in exposure compared to the population average. Specifically, we find increased vulnerability for non-Hispanic mothers who are either younger, have higher body mass index or lower educational attainment. Our case study is the first precision health study of critical windows.


## Data

We used data from three sources. First is birth records from the Colorado Birth Registry. There were provided by the Colorado Department of Public Health and Environment under a data use agreement. Second is fine particulate matter (PM2.5) air pollution exposure data from the US Environmental Protection Agency (EPA). Third is temperature data from the US EPA. The use of these data for the analyses in this paper was approved by the institutional review board of Colorado State University. Data sources are described in Section 2 of the main text. 

This data folder of this repository contains simulated data files that replicate the inputs and outputs of the data processing scripts.

- co_birth_simulated.csv: Simulated data that mimics the birth data provided by Colorado Department of Public Health and Environment.
- fenton_simulated.csv: Simulated birth weight for gestational age z scores. These can be linked to the birth data. Data processing script 6 can be altered to use this file (uncomment line of code reading in this file). That avoids the tedious process of linking the simulated birth weights to Fenton z scores, which serves no purpose here because the data are simulated.
- CO_Birth_data_simulated_50000.rda: The output file created from the data processing scripts. This can be used as input to the analysis scripts. This is the result of running the data processing scripts on the simulated data. It contains only simulated health data linked to real exposure data. Because of file size limitations on GitHub we have uploaded a random sample of 50000 births.

**All birth data included in this repository is simulated.**



## Code

### Required R Packages 

The following packages are required to use the included code.

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


### Files

The current version of the dlmtree package is available at https://github.com/danielmork/dlmtree. We have included in this repository an archived version used in this paper.

1. dlmtree_0.8.0.0.tar.gz : R package dlmtree

### Data Preparation Scripts

The following scripts will reproduce the data processing steps required to make the analysis file used. They are run in order. They will download large amounts of data. Processing is slow.

1. 1_getcmaq.R
2. 2_LimitCMAQToColorado.R
3. 3_GetEPAairdata.R
4. 4_IDW.R
5. 5_MergeCMAQandIDW.R
6. 6_prep_limit_birth_data.R (Note: There is a manual linking step in this script. We have included an alternative approach to link simulated data. Please see commented out part of the Fenton z-score linking section of the code.)
7. 7_make_analysis_file.R


### Simulation Scripts

The following scripts reproduce the simulation results.

1. sim.R : replicate all simulation results
2. analyze.sim.dlmtree.R : function for simulation results
3. run.sim.R : produces results and tables
4. TDLM_vs_BART_sim.R : script for part of Figure 1
5. HDLM_vs_BART_sim.R : script for part of Figure 1
6. sim_res_waic.txt numerical output from simulation

### Data Analysis Scripts

The following scripts reproduce the data analysis results.

1. data_analysis1.R : run data analysis models
2. data_analysis2.R : data analysis results
3. data_analysis3.R : data analysis cross validation


## References

Mork, Daniel, Kioumourtzoglou, Marianthi-Anna, Weisskopf, Marc, Coull, Brent A, and Wilson, Ander. “[Heterogeneous Distributed Lag Models to Estimate Personalized Effects of Maternal Exposures to Air Pollution](https://arxiv.org/abs/2109.13763).”
(2022).

Mork, Daniel. [dlmtree Github Page](https://github.com/danielmork/dlmtree).



## Acknowldgements 

This work was supported by National Institutes of Health grants ES029943, ES028811, and P30-ES000002. This research was also supported by USEPA grants RD-839278 and RD-83587201. Its contents are solely the responsibility of the grantee and do not necessarily represent the official views of the USEPA. Further, USEPA does not endorse the purchase of any commercial products or services mentioned in the publication.

This work utilized the RMACC Summit supercomputer, which is supported by the National Science Foundation (awards ACI-1532235 and ACI-1532236), the University of Colorado Boulder and Colorado State University. The RMACC Summit supercomputer is a joint effort of the University of Colorado Boulder and Colorado State University.

These data were supplied by the Center for Health and Environmental Data Vital Statistics Program of the Colorado Department of Public Health and Environment, which specifically disclaims responsibility for any analyses, interpretations, or conclusions it has not provided.



