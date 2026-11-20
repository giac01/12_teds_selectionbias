# Run using docker container: bignardig/tidyverse451:v8
#
# Step 3 of the RQ6 (RQ3 in the manuscript) multiple imputation:
#   1a. 8_rq6_1a_imputation_prepare.R       prepare the imputation input (local)
#   1b. 8_rq6_1b_imputation_cluster_slurm   run the imputations on the KCL CREATE cluster
#       (runs 8_rq6_1b_imputation_cluster_task.R)
#   1c. 8_rq6_1c_imputation_combine.R       (this script) combine the cluster outputs (local)
#
# Before running: copy the cluster outputs back to results/8_1_cluster/ (see the
# header of 8_rq6_1b_imputation_cluster_slurm). Does not reload the TEDS data: everything it
# needs is in results/8_1_combine_input.Rds, saved by step 1.
#
#   docker run --rm -v /home/giaco:/home/rstudio -w /home/rstudio/Dropbox/Work/projects/12_teds_selectionbias \
#     -u 1000:1000 bignardig/tidyverse451:v8 Rscript 8_rq6_1c_imputation_combine.R
#
# Optional: IMPUTATION_TAG, suffix of the input/output files (default ""; must match step 1)
#
# Output:
#   results/8_1_all_imputations.Rds  all mids objects, in the order of results/8_1_tasks.Rds (used by 8_rq6_3)
#   results/8_1_imputed_mice.Rds     completed datasets: one list per time point, one data frame per imputation (used by 8_rq6_2, 8_rq6_3)

rm(list=ls())

library(tidyverse)
library(mice)

output_tag   = Sys.getenv("IMPUTATION_TAG", unset = "")
results_file = function(name) file.path("results", paste0(name, output_tag, ".Rds"))
cluster_output_dir = file.path("results", paste0("8_1_cluster", output_tag))

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load -------------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

combine_input        = readRDS(results_file("8_1_combine_input"))

original_dataset     = combine_input$original_dataset
attritioned_datasets = combine_input$attritioned_datasets
timepoints           = combine_input$timepoints
rq6y_all             = combine_input$rq6y_all
pgs_vars1            = combine_input$pgs_vars1
pgs_vars2            = combine_input$pgs_vars2
rq6z_pgs_vars        = combine_input$rq6z_pgs_vars
tasks                = combine_input$tasks
number_imputations   = combine_input$number_imputations
number_iterations    = combine_input$number_iterations

cat("Imputations:", number_imputations, "| iterations:", number_iterations, "| tag:", output_tag, "\n")

## Read the cluster outputs -----------------------------------------------------
# 8_rq6_1b_imputation_cluster_task.R saves one file per imputation, holding the mids
# objects for all sex-zygosity groups and time points.

cluster_files = file.path(cluster_output_dir, sprintf("imputation_%03d.Rds", 1:number_imputations))
missing_files = cluster_files[!file.exists(cluster_files)]
if (length(missing_files) > 0) {
  stop(length(missing_files), " cluster output files are missing, e.g. ", paste(head(basename(missing_files), 5), collapse = ", "))
}

all_imputations = vector("list", nrow(tasks))
for (k in 1:number_imputations) {
  out = readRDS(cluster_files[k])
  if (!identical(out$tasks_run$group, tasks$group[tasks$imputation == k])) stop("Task mismatch in ", cluster_files[k])
  if (out$number_iterations != number_iterations) stop("Number of iterations differs in ", cluster_files[k])
  all_imputations[tasks$imputation == k] = out$imputations
}

saveRDS(all_imputations, file = results_file("8_1_all_imputations"))

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Combine single imputations back by timepoint ---------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# For each timepoint, rbind all sexzyg groups together for each imputation

imputed_mice = list()

for (i in timepoints) {
  # Create list to store all imputations for this timepoint
  imp_list = list()

  for (k in 1:number_imputations) {

    # Get all sexzyg groups for this timepoint-imputation combination
    group_indices = which((tasks$timepoint == i) & (tasks$imputation == k))

    # Extract completed datasets from each sexzyg group and rbind them
    completed_dfs = lapply(all_imputations[group_indices], function(mids_obj) {
      mice::complete(mids_obj, action = 1)  # Get the completed data
    })

    # Combine all sexzyg groups into one dataset
    imp_list[[k]] = do.call(rbind.data.frame, completed_dfs)

    # Reorder to match original dataset
    imp_list[[k]] = imp_list[[k]][match(original_dataset$randomfamid, imp_list[[k]]$randomfamid),]

    # Verify order matches
    if(!isTRUE(all.equal(imp_list[[k]]$randomfamid, original_dataset$randomfamid, check.attributes = FALSE))) stop("randomfamid order mismatch")
    if(!isTRUE(all.equal(imp_list[[k]]$sexzyg,      original_dataset$sexzyg,      check.attributes = FALSE))) stop("sexzyg mismatch")

    # Set imputed values back to NA where they were NA in original dataset (only for outcome variables)
    for (var in rq6y_all) {
      na_mask = is.na(original_dataset[[var]])
      imp_list[[k]][[var]][na_mask] = NA
    }

    # MZ co-twin PGS were not imputed: copy the twin's (imputed) PGS, as MZ co-twins share one genotype
    mz_rows = which(str_detect(as.character(imp_list[[k]]$sexzyg), "^MZ"))
    for (p in seq_along(pgs_vars1)) {
      imp_list[[k]][[pgs_vars2[p]]][mz_rows] = imp_list[[k]][[pgs_vars1[p]]][mz_rows]
    }
  }

  # Store list of imputations for this timepoint
  imputed_mice[[i]] = imp_list
}

testthat::test_that("Imputed datasets complete for auxiliary PGS", {
  d = imputed_mice[[1]][[1]]
  testthat::expect_false(anyNA(d[rq6z_pgs_vars]))
  mz = str_detect(as.character(d$sexzyg), "^MZ")
  testthat::expect_equal(d$pgs_ea1[mz], d$pgs_ea2[mz])
  testthat::expect_equal(length(imputed_mice), length(timepoints))
  testthat::expect_equal(length(imputed_mice[[1]]), number_imputations)
})

saveRDS(imputed_mice, file = results_file("8_1_imputed_mice"))

cat("Saved", results_file("8_1_all_imputations"), "and", results_file("8_1_imputed_mice"), "\n")

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Sanity check -----------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

cat("original missingness\n")
print(table(is.na(original_dataset$lcg1)))
cat("\nmissingness after attritioning\n")
for (i in seq_along(attritioned_datasets)) print(table(is.na(attritioned_datasets[[i]]$lcg1)))
print(table(is.na(attritioned_datasets[[1]]$lcg2)))
cat("\nmissingness after attritioning + imputation\n")
for (i in seq_along(imputed_mice)) print(table(is.na(imputed_mice[[i]][[1]]$lcg1)))
print(table(is.na(imputed_mice[[1]][[1]]$lcg2)))

cat("\nPGS missing before imputation (twin 1):", sum(is.na(original_dataset$pgs_ea1)), "of", nrow(original_dataset), "\n")
