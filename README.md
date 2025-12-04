# TEDS Selection Bias Study

This repository contains analysis code for studying selection bias in the Twins Early Development Study (TEDS).

Researchers can apply for access to TEDS data from: https://www.teds.ac.uk/researchers/teds-data-access-policy/

Analyses were run using the docker container: bignardig/tidyverse451:v5

# Overview

### General Scripts:

- `0_load_data.R` -- Data loading script, does the following:
  -- calls helper scripts `0_functions.R` and  `0_lists_of_variables.R`
  -- initial data cleaning
  -- creates pollution and SDQ externalising scores
  -- defines vectors with variable sets and labels
  -- creates vectors with participants to exclude from given analyses


### Helper Scripts:

- `0_functions.R` - Helper functions
- `0_lists_of_variables.R` - List of variables for imputation

### Research Question 1 Code: 
- `1_calculate_participation_ACE.R` - ACE analysis of participation
- `1_rq1_predictors_of_participation.qmd` - Analysis of which factors predict participation at each time point in TEDS

### Research Question 2 Code: 
- `2_rq2_1_run_bootstrapping.R` - Runs boostrapping, attritioning and analysis 
- `2_rq2_results.qmd`           - Visualise and present results

# code for running correlation analyses
-  - Generated Imputed Baseline Data

### Research Question 3 Code: 
- `3_rq3_1_weighting_bootstrap.R` - Code to run inverse probability weighting + bootstrap analysis
- `3_rq3_2_weighting_results.qmd` - Visualise and present results 

### Research Question 4 Code: 
- `5_rq5_1_run_imputation.R` - Generate imputed datasets
- `5_rq5_2_run_bootstrapping.R` - 

### Descriptive Statistics:
- `11_descriptives.qmd` - Calculate descriptive statistics


