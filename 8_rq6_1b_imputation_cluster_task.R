# Run using the bignardig/tidyverse451:v8 image (on the KCL CREATE HPC as the
# Singularity file tidyverse451_v8.sif). Called by 8_rq6_1b_imputation_cluster_slurm.
#
# Runs ONE imputation (number k) of the RQ6 multiple imputation: one mice run
# (m = 1) for each time point x sex-zygosity group (4 x 5 = 20 groups), using the data,
# methods and predictor matrices prepared by
#   8_rq6_1a_imputation_prepare.R
# (results/8_1_cluster_input.Rds). Does not need the TEDS data files.
#
# Environment variables:
#   IMPUTATION_K    which imputation to run (1-250; the slurm script sets it to the array task id)
#   IMPUTATION_TAG  suffix of the input/output files (default ""; must match the prepare run)
#
# Output: results/8_1_cluster<TAG>/imputation_<k>.Rds, combined afterwards by
#   8_rq6_1c_imputation_combine.R

suppressPackageStartupMessages(library(mice))

k          = as.integer(Sys.getenv("IMPUTATION_K", unset = NA))
output_tag = Sys.getenv("IMPUTATION_TAG", unset = "")

input_file = file.path("results", paste0("8_1_cluster_input", output_tag, ".Rds"))
output_dir = file.path("results", paste0("8_1_cluster", output_tag))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_file = file.path(output_dir, sprintf("imputation_%03d.Rds", k))

input = readRDS(input_file)

if (is.na(k) || k < 1 || k > input$number_imputations) {
  stop("IMPUTATION_K must be an integer between 1 and ", input$number_imputations)
}

if (file.exists(output_file)) {
  cat("Output already exists, skipping:", output_file, "\n")
  quit(save = "no")
}

tasks_run = input$tasks[input$tasks$imputation == k, ]

cat("Imputation", k, "of", input$number_imputations, "|", nrow(tasks_run), "groups |",
    input$number_iterations, "iterations | mice", as.character(packageVersion("mice")), "\n")

ta = Sys.time()

imputations = lapply(seq_len(nrow(tasks_run)), function(j) {
  group = tasks_run$group[j]
  t0 = Sys.time()
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")                         # R's default generator, set explicitly so each task's seed always gives the same imputation
  imp = mice(
    input$df_impute_list_flat[[group]],
    m               = 1,
    maxit           = input$number_iterations,
    method          = input$meth_list[[group]],
    predictorMatrix = input$pred_list[[group]],
    seed            = tasks_run$seed[j],
    printFlag       = FALSE
  )
  cat(sprintf("  %-28s %5.1f min\n", group, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  imp
})

tb = Sys.time()
print(tb - ta)

## Write to a temporary file first, so an interrupted job never leaves a partial output file
saveRDS(
  list(
    imputations       = imputations,
    tasks_run         = tasks_run,
    number_iterations = input$number_iterations,
    mice_version      = as.character(packageVersion("mice")),
    run_time_mins     = as.numeric(difftime(tb, ta, units = "mins")),
    node              = Sys.info()[["nodename"]]
  ),
  paste0(output_file, ".tmp")
)
file.rename(paste0(output_file, ".tmp"), output_file)

cat("Saved", output_file, "\n")
