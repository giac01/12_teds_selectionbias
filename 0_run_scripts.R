system("Rscript 2_rq2_1_run_bootstrapping.R")
system("quarto render 2_rq2_results.qmd")

system("Rscript 5_rq5_1_run_imputation.R")
system("Rscript 5_rq5_2_run_bootstrapping.R")
system("quarto render 5_rq5_3_results.qmd")
