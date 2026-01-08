# TEDS Selection Bias Study

This repository contains analysis code for studying selection bias in the Twins Early Development Study (TEDS).

Researchers can apply for access to TEDS data from: https://www.teds.ac.uk/researchers/teds-data-access-policy/

Analyses were run using the Docker container: `bignardig/tidyverse451`

## Overview of scripts

### General Scripts:

- [`0_load_data.R`](0_load_data.R) - Data loading script that performs the following:
    - Calls helper scripts [`0_functions.R`](0_functions.R) and [`0_lists_of_variables.R`](0_lists_of_variables.R)
    - Performs initial data cleaning
    - Creates pollution and SDQ externalising scores
    - Defines vectors with variable sets and labels
    - Creates vectors with participants to exclude from given analyses
- [`0_functions.R`](0_functions.R) - Helper and analysis functions
- [`0_lists_of_variables.R`](0_lists_of_variables.R) - List of variables for imputation

### Descriptive Statistics:

- [`1_calculate_participation_ACE.R`](1_calculate_participation_ACE.R) - ACE analysis of participation
- [`11_descriptives.qmd`](11_descriptives.qmd) - Calculate descriptive statistics
    - [View HTML Output](11_descriptives.html)

### Research Question 1 Code:
Analysis of which factors at Year 2 predict participation at later time points.

- [`1_rq1_predictors_of_participation.qmd`](1_rq1_predictors_of_participation.qmd) - This script runs the analysis and generates results
    - [View HTML Output](1_rq1_predictors_of_participation.html)

### Research Question 2 Code:
Analysis of the effect of selecting Year 2 data conditional on later participation ("attritioning").

- [`2_rq2_1_run_bootstrapping.R`](2_rq2_1_run_bootstrapping.R) - Runs bootstrapping, attritioning, and analysis
- [`2_rq2_results.qmd`](2_rq2_results.qmd) - Visualise and present results
    - [View HTML Output](2_rq2_results.html)

### Research Question 3 Code:
Effect of IP weighting data collected at later time points.

- [`3_rq3_1_weighting_bootstrap.R`](3_rq3_1_weighting_bootstrap.R) - Code to run inverse probability weighting + bootstrap analysis
- [`3_rq3_2_weighting_results.qmd`](3_rq3_2_weighting_results.qmd) - Visualise and present results 
    - [View HTML Output](3_rq3_2_weighting_results.html)

### Research Question 4 Code:
Effect of imputing data collected at later time points.

- [`5_rq5_1_run_imputation.R`](5_rq5_1_run_imputation.R) - Generate imputed datasets
- [`5_rq5_2_run_bootstrapping.R`](5_rq5_2_run_bootstrapping.R) - Run bootstrapping analysis on imputed datasets 

### Research Question 5 Code:
In this section, we use data from four variables with low missingness (*rq6y*: Y12: Depression (MFQ), Y12: Externalising, Y12: Cognitive ability, Y16: GCSE core subjects grade), and test the effect of attritioning based on the late time points. 

We also test if imputation or weighting can correct for the effect of attrition.

**Comparison of original vs attritioned (no correction applied)**

- [`6_rq6_1_run_bootstrap_attritioning.R`](6_rq6_1_run_bootstrap_attritioning.R) - Compares original versus attritioned
- [`6_rq6_2_results.qmd`] - Visualise and present results
    - [View HTML Output](6_rq6_2_results.html)

**Comparison of original vs attritioned + IP weighted**

- [`7_rq7_1_weighting.R`](7_rq7_1_weighting.R) - 

**Comparison of original vs attritioned + imputed**

- [`8_rq6_1_imputation.R`](8_rq6_1_imputation.R) - Generates K imputed datasets
- [`8_rq6_2_run_bootstrapping.R`](8_rq6_2_run_bootstrapping.R) - Runs bootstrap comparison analysis using imputed datasets
- [`8_rq6_3_imputation_results.qmd`](8_rq6_3_imputation_results.qmd) - Visualise and present results
    - [View HTML Output](8_rq6_3_imputation_results.html)

## Note on running parallelized scripts

Analysis scripts that use parallelization should be run in the terminal using the commands below.

This is due to an issue where individual workers will use more than 100% of a single CPU when run within RStudio.

```bash
  export OMP_NUM_THREADS=1
  export OPENBLAS_NUM_THREADS=1
  export MKL_NUM_THREADS=1
  export VECLIB_MAXIMUM_THREADS=1

  Rscript 6_rq6_1_run_bootstrap_attritioning.R && \
  Rscript 3_rq3_1_weighting_bootstrap.R && \
  Rscript 7_rq6_1_weighting.R
  
```

## Sesssion info

```{r}
R version 4.5.1 (2025-06-13)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.2 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: Etc/UTC
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_4.5.1    tools_4.5.1       rstudioapi_0.17.1
```