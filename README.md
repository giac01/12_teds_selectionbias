# TEDS Selection Bias Study

[![R](https://img.shields.io/badge/R-4.5.1-blue.svg)](https://www.r-project.org/)
[![Docker](https://img.shields.io/badge/Docker-bignardig%2Ftidyverse451-blue.svg)](https://hub.docker.com/)

Analysis code for studying selection bias in the Twins Early Development Study (TEDS).

> **Data Access:** Researchers can apply for access to TEDS data from: https://www.teds.ac.uk/researchers/teds-data-access-policy/

**[View Online Supplement](0_OnlineSupplement.html)** - Supplementary tables and figures with additional results

---

## Overview of Scripts

### General Scripts

| Script | Description |
|:-------|:------------|
| [`0_load_data.R`](0_load_data.R) | Data loading and initial cleaning. Calls helper scripts, creates composite scores (pollution, SDQ externalising), defines variable sets and exclusion criteria |
| [`0_functions.R`](0_functions.R) | Helper and analysis functions |
| [`0_lists_of_variables.R`](0_lists_of_variables.R) | Variable lists for imputation |

---

### Descriptive Statistics

| Script | Description | Output |
|:-------|:------------|:------:|
| [`1_calculate_participation_ACE.R`](1_calculate_participation_ACE.R) | ACE analysis of participation | - |
| [`11_descriptives.qmd`](11_descriptives.qmd) | Calculate descriptive statistics | [HTML](11_descriptives.html) |

---

### Research Question 1

**Objective:** Identify which factors at Year 2 predict participation at later time points.

| Script | Description | Output |
|:-------|:------------|:------:|
| [`1_rq1_predictors_of_participation.qmd`](1_rq1_predictors_of_participation.qmd) | Run analysis and generate results | [HTML](1_rq1_predictors_of_participation.html) |

---

### Research Question 2

**Objective:** Analyze the effect of selecting Year 2 data conditional on later participation ("attritioning").

| Script | Description | Output |
|:-------|:------------|:------:|
| [`2_rq2_1_run_bootstrapping.R`](2_rq2_1_run_bootstrapping.R) | Run bootstrapping, attritioning, and analysis | - |
| [`2_rq2_results.qmd`](2_rq2_results.qmd) | Visualise and present results | [HTML](2_rq2_results.html) |

---

### Research Question 3

**Objective:** Test the effect of inverse probability weighting on data collected at later time points.

| Script | Description | Output |
|:-------|:------------|:------:|
| [`3_rq3_1_weighting_bootstrap.R`](3_rq3_1_weighting_bootstrap.R) | Run IP weighting + bootstrap analysis | - |
| [`3_rq3_2_weighting_results.qmd`](3_rq3_2_weighting_results.qmd) | Visualise and present results | [HTML](3_rq3_2_weighting_results.html) |

---

### Research Question 4

**Objective:** Test the effect of imputing data collected at later time points.

| Script | Description | Output |
|:-------|:------------|:------:|
| [`5_rq5_1_run_imputation.R`](5_rq5_1_run_imputation.R) | Generate imputed datasets | - |
| [`5_rq5_2_run_bootstrapping.R`](5_rq5_2_run_bootstrapping.R) | Run bootstrapping on imputed datasets | - |

---

### Research Question 5

**Objective:** Using four variables with low missingness (Y12: Depression, Y12: Externalising, Y12: Cognitive ability, Y16: GCSE core subjects), test the effect of attritioning based on late time points and evaluate whether imputation or weighting can correct for attrition effects.

#### Original vs. Attritioned (no correction)

| Script | Description | Output |
|:-------|:------------|:------:|
| [`6_rq6_1_run_bootstrap_attritioning.R`](6_rq6_1_run_bootstrap_attritioning.R) | Compare original vs. attritioned | - |
| [`6_rq6_2_results.qmd`](6_rq6_2_results.qmd) | Visualise and present results | [HTML](6_rq6_2_results.html) |

#### Original vs. Attritioned + IP Weighted

| Script | Description | Output |
|:-------|:------------|:------:|
| [`7_rq7_1_weighting.R`](7_rq7_1_weighting.R) | Run IP weighting analysis | - |
| [`7_rq6_2_weighting_results.qmd`](7_rq6_2_weighting_results.qmd) | Visualise and present results | [HTML](7_rq6_2_weighting_results.html) |

#### Original vs. Attritioned + Imputed

| Script | Description | Output |
|:-------|:------------|:------:|
| [`8_rq6_1_imputation.R`](8_rq6_1_imputation.R) | Generate K imputed datasets | - |
| [`8_rq6_2_run_bootstrapping.R`](8_rq6_2_run_bootstrapping.R) | Run bootstrap comparison on imputed data | - |
| [`8_rq6_3_imputation_results.qmd`](8_rq6_3_imputation_results.qmd) | Visualise and present results | [HTML](8_rq6_3_imputation_results.html) |

---

## Running Parallelized Scripts

> **Note:** Run parallelized scripts must be run from the terminal (not RStudio) because the plan(multicore) command won't work in Rstudio. This approach also forces each worker to use a single CPU thread.  

```bash
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

Rscript 2_rq2_1_run_bootstrapping.R && \
Rscript 3_rq3_1_weighting_bootstrap.R && \
Rscript 5_rq5_1_run_imputation.R
Rscript 6_rq6_1_run_bootstrap_attritioning.R && \
Rscript 7_rq6_1_weighting.R && \
Rscript 8_rq6_2_run_bootstrapping.R
```

---

## Session Info

For computational reproducibility, all code has been run using docker containers (linked above). Information about the versions of R packages used can be found below. 

All analyses were run on a GMKtec NucBox K8 plus running Ubuntu 24.04.03 LTS, with a AMD Ryzen 7 8845HS CPU and 96GB of RAM.

<details>
<summary>Click to expand</summary>

```r
> sessionInfo()
R version 4.5.1 (2025-06-13)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.2 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: Etc/UTC
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] Ternary_2.3.4          distrEx_2.9.6          distr_2.9.7            sfsmisc_1.1-21         startupmsg_1.0.0      
 [6] lubridate_1.9.4        forcats_1.0.0          stringr_1.5.1          dplyr_1.1.4            purrr_1.0.4           
[11] readr_2.1.5            tidyr_1.3.1            tibble_3.3.0           tidyverse_2.0.0        future.apply_1.20.0   
[16] future_1.58.0          see_0.11.0             DHARMa_0.4.7           performance_0.15.0     pROC_1.18.5           
[21] mice_3.18.0            MASS_7.3-65            testthat_3.2.3         patchwork_1.3.1        Hmisc_5.2-3           
[26] weights_1.1.2          gt_1.0.0               ggh4x_0.3.1            umx_4.21.0             OpenMx_2.22.7         
[31] gbtoolbox_0.0.2.6      ggrepel_0.9.6          ggplot2_3.5.2          ggmice_0.1.1           marginaleffects_0.30.0

loaded via a namespace (and not attached):
  [1] RcppHungarian_0.3    splines_4.5.1        later_1.4.2          rpart_4.1.24         lifecycle_1.0.4     
  [6] Rdpack_2.6.4         rprojroot_2.0.4      globals_0.18.0       lattice_0.22-7       insight_1.3.1       
 [11] ggdist_3.3.3         backports_1.5.0      magrittr_2.0.3       sass_0.4.10          rmarkdown_2.29      
 [16] yaml_2.3.10          httpuv_1.6.16        sp_2.2-0             cowplot_1.2.0        minqa_1.2.8         
 [21] RColorBrewer_1.1-3   pkgload_1.4.0        nnet_7.3-20          listenv_0.9.1        gdata_3.0.1         
 [26] parallelly_1.45.1    svglite_2.2.1        commonmark_1.9.5     codetools_0.2-20     MuMIn_1.48.11       
 [31] xml2_1.3.8           tidyselect_1.2.1     shape_1.4.6.1        farver_2.1.2         lme4_1.1-37         
 [36] stats4_4.5.1         base64enc_0.1-3      jsonlite_2.0.0       polycor_0.8-1        mitml_0.4-5         
 [41] Formula_1.2-5        survival_3.8-3       iterators_1.0.14     systemfonts_1.2.3    foreach_1.5.2       
 [46] tools_4.5.1          ragg_1.4.0           Rcpp_1.0.14          glue_1.8.0           mnormt_2.1.1        
 [51] gridExtra_2.3        pan_1.9              xfun_0.52            admisc_0.38          distributional_0.5.0
 [56] withr_3.0.2          fastmap_1.2.0        boot_1.3-31          litedown_0.7         digest_0.6.37       
 [61] timechange_0.3.0     R6_2.6.1             mime_0.13            textshaping_1.0.1    colorspace_2.1-1    
 [66] gtools_3.9.5         markdown_2.0         DiagrammeR_1.0.11    utf8_1.2.6           generics_0.1.4      
 [71] data.table_1.17.4    htmlwidgets_1.6.4    pkgconfig_2.0.3      gtable_0.3.6         brio_1.1.5          
 [76] htmltools_0.5.8.1    scales_1.4.0         kableExtra_1.4.0     reformulas_0.4.1     knitr_1.50          
 [81] rstudioapi_0.17.1    tzdb_0.5.0           reshape2_1.4.4       visNetwork_2.1.2     checkmate_2.3.2     
 [86] nlme_3.1-168         nloptr_2.2.1         cachem_1.1.0         parallel_4.5.1       foreign_0.8-90      
 [91] desc_1.4.3           pillar_1.10.2        grid_4.5.1           vctrs_0.6.5          PlotTools_0.3.1     
 [96] promises_1.3.3       jomo_2.7-6           xtable_1.8-4         cluster_2.1.8.1      waldo_0.6.1         
[101] htmlTable_2.4.3      evaluate_1.0.3       magick_2.9.0         cli_3.6.5            compiler_4.5.1      
[106] rlang_1.1.6          crayon_1.5.3         labeling_0.4.3       plyr_1.8.9           stringi_1.8.7       
[111] psych_2.5.6          viridisLite_0.4.2    glmnet_4.1-10        bayestestR_0.16.1    Matrix_1.7-3        
[116] hms_1.1.3            conflicted_1.2.0     shiny_1.10.0         haven_2.5.5          rbibutils_2.3       
[121] broom_1.0.8          memoise_2.0.1        RcppParallel_5.1.10 
```

</details>
