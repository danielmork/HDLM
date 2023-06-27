## Simulated data

This data folder of this repository contains simulated data files that replicate the inputs and outputs of the data processing scripts.

- co_birth_simulated.csv: Simulated data that mimics the birth data provided by Colorado Department of Public Health and Environment.
- fenton_simulated.csv: Simulated birth weight for gestational age z scores. These can be linked to the birth data. Data processing script 6 can be altered to use this file (uncomment line of code reading in this file). That avoids the tedious process of linking the simulated birth weights to Fenton z scores, which serves no purpose here because the data are simulated.
- CO_Birth_data_simulated_50000.rda: The output file created from the data processing scripts. This can be used as input to the analysis scripts. This is the result of running the data processing scripts on the simulated data. It contains only simulated health data linked to real exposure data. Because of file size limitations on GitHub we have uploaded a random sample of 50000 births.

All birth data included in this repository is simulated. 