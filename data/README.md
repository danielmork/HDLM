## Simulated data

This folder contains simulated data files that replicate the inputs and outputs of the data processing scripts.

- co_birth_simulated.csv: Simulated data that mimics the birth data provided by Colorado Department of Public Health and Environment
- fenton_simulated.csv: Simulated birth weight for gestational age z scores. These can be linked to the data. Data processing script 6 can be altered to use this file (uncomment line of code reading in this file). That avoids linking the data the tedious process of linking the simulated birth weights to fenton z scores, which serves no pupose because the data are simulated.
- CO_Birth_data.rda: Is the out file created from the data processing scripts that is used as input the the analysis scripts. This is the result of running the data processing scripts on the simulated data. It contains only simulated health data linked to real exposure data.