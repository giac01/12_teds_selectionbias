# Run using docker container: bignardig/tidyverse451:v8 (on the KCL CREATE HPC:
# the same image as a Singularity file, tidyverse451_v8.sif)
# Original run: 15-06-2026 (without PGS). Rerun with PGS as auxiliary variables: see PGS_ANALYSIS_NOTES.txt

# Multiple imputation for RQ6 (RQ3 in the manuscript): attrition the data at
# each participation time point, then impute the attritioned outcomes using
# the auxiliary variables (rq6z_vars) and the 12 polygenic scores
# (rq6z_pgs_vars, twin and co-twin versions). The PGS are not used to impute
# each other (changed 13-11-2026; see the "PGS do not predict each other" section).
#
# Three steps:
#   1. 8_rq6_1a_imputation_prepare.R (this script, local): build the attritioned
#      datasets, methods and predictor matrices, and save
#        results/8_1_cluster_input.Rds  everything the imputations need (copied to the cluster)
#        results/8_1_combine_input.Rds  what step 3 needs (stays local)
#        results/8_1_tasks.Rds, results/8_1_imputation_variables.Rds
#   2. Run the imputations on the KCL CREATE cluster: 8_rq6_1b_imputation_cluster_slurm, which
#      runs 8_rq6_1b_imputation_cluster_task.R (see the slurm script's header for the workflow).
#   3. 8_rq6_1c_imputation_combine.R (local): combine the cluster outputs into
#      results/8_1_imputed_mice.Rds, as used by 8_rq6_2 and 8_rq6_3.
#
#   docker run --rm -v /home/giaco:/home/rstudio -w /home/rstudio/Dropbox/Work/projects/12_teds_selectionbias \
#     -u 1000:1000 bignardig/tidyverse451:v8 Rscript 8_rq6_1a_imputation_prepare.R
#
# Optional settings (for testing):
#   IMPUTATION_N     number of imputations              (default 250)
#   IMPUTATION_ITER  number of MICE iterations           (default 75)
#   IMPUTATION_TAG   suffix added to all output files     (default ""; use e.g. "_TEST"; give the same tag to steps 2 and 3)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load data --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

rm(list=ls())

source("0_load_data_with_pgs.R")                                                # 0_load_data.R + PGS (rq6z_pgs_vars). NB: clears the environment, so settings come after it

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Settings ---------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

number_imputations = as.integer(Sys.getenv("IMPUTATION_N",    unset = "250"))   # original run: 250 imputations x 75 iterations took 1.384 days (16 cores, without PGS)
number_iterations  = as.integer(Sys.getenv("IMPUTATION_ITER", unset = "75"))
output_tag         = Sys.getenv("IMPUTATION_TAG", unset = "")

results_file = function(name) file.path("results", paste0(name, output_tag, ".Rds"))

cat("Imputations:", number_imputations, "| iterations:", number_iterations, "| tag:", output_tag, "\n")

range_participation_outcomes = 6:9

rq1y_twin                         = rq1y_twin[range_participation_outcomes]
rq1y_twin1                        = rq1y_twin1[range_participation_outcomes]
rq1y_twin2                        = rq1y_twin2[range_participation_outcomes]
rq1y_twin_labels                  = rq1y_twin_labels[range_participation_outcomes]
rq1y_twin_labels_clean            = rq1y_twin_labels_clean[range_participation_outcomes]
rq1y_twin_labels_clean_extrashort = rq1y_twin_labels_clean_extrashort[range_participation_outcomes]

cbind.data.frame(rq1y_twin1, rq1y_twin_labels)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Participant Exclusions -------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

a = nrow(df)

df = df %>%
  filter(!(randomfamid %in% exclude_fams_rq1x)) %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%
  filter(!(randomfamid %in% exclude_fams_rq6y)) %>% # note that participants with missing data on all focal variables are NOT informative to the analyses. The aim of the analyses is to attrition -> correct for attritioning -> test if corrected estimates resample pre-attritioned estimates. Those without data pre-attritioning are not part of the focal analyses! %>%
  filter(random == 1)

cat("Excluded", a-nrow(df), "participants ")

rm(a)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Attrition Data ---------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# In section, unlike the others, we attrition in the wide data format.

original_datasets    = list()
attritioned_datasets = list()

for (i in seq_along(rq1y_twin1)){

  filter_twin1 = as.numeric(df[[rq1y_twin1[i]]]) == 0   # (1/F = present, 0/T = NOT present)
  filter_twin2 = as.numeric(df[[rq1y_twin2[i]]]) == 0   # (1/F = present, 0/T = NOT present)

  original_datasets[[i]] = attritioned_datasets[[i]] = df %>%                   # this is kind of redundant as each element of the list is identical to each other and to original_dataset
    select("randomfamid", "twin", "random", "x3zygos","sexzyg", starts_with(rq6y_prefix), all_of(rq6z_vars), all_of(rq6z_pgs_vars))

  attritioned_datasets[[i]][which(filter_twin1),paste0(rq6y_prefix,"1")] = NA # massive bug here earlier, only attritioned twin 1 - not good!
  attritioned_datasets[[i]][which(filter_twin2),paste0(rq6y_prefix,"2")] = NA # massive bug here earlier, only attritioned twin 1 - not good!

}

names(original_datasets)    = rq1y_twin
names(attritioned_datasets) = rq1y_twin

original_dataset = df %>%
    select("randomfamid", "twin", "random","x3zygos", "sexzyg", starts_with(rq6y_prefix), all_of(rq6z_vars), all_of(rq6z_pgs_vars))


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Impute ---------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


## list of variables used ------------------------------------------------------

rq6_imputation_vars = c(
  rq6y,
  rq6z_vars,
  rq6z_pgs_vars
)

cbind(
  rq6_imputation_vars,
  var_to_label(rq6_imputation_vars),
  sapply(rq6_imputation_vars, function(x) length(which(!is.na(df[x]))))
) %>%
  knitr::kable()

df_impute_list = attritioned_datasets
df_impute_list = lapply(df_impute_list, function(x) split(x, x$sexzyg))

### Note on df_impute_list  ----------------------------------------------------

# The structure of df_impute_list is a nested list, with the first level having time ponts:

# names(df_impute_list)
# [1] "u1cdata" "zmhdata" "zcdata"

# And the second level of the list is sex-zygosity

# names(df_impute_list[[1]])
# [1] "MZ male"           "DZ male"           "MZ female"         "DZ female"         "DZ opposite sexes"

# To make parallised imputation easier - we will flatten this list into a single level, so we can easily run imputations across all groups.

## Flatten df_impute_list ------------------------------------------------------

df_impute_list_flat = unlist(df_impute_list, recursive = FALSE)

# length(df_impute_list_flat)
# [1] 15
# names(df_impute_list_flat)
# 1] "u1cdata.MZ male"           "u1cdata.DZ male"           "u1cdata.MZ female"         "u1cdata.DZ female"         "u1cdata.DZ opposite sexes" "zmhdata.MZ male"
# [7] "zmhdata.DZ male"           "zmhdata.MZ female"         "zmhdata.DZ female"         "zmhdata.DZ opposite sexes" "zcdata.MZ male"            "zcdata.DZ male"
# [13] "zcdata.MZ female"          "zcdata.DZ female"          "zcdata.DZ opposite sexes"

## Set up predictor matrix -----------------------------------------------------
df_impute = df_impute_list_flat[[1]]

if (TRUE){
  miceinit = mice(df_impute, method = "pmm", m = 1, maxit = 0)

  meth = miceinit$method
  pred = miceinit$predictorMatrix

  exclude_vars = c(
    "sexzyg",
    "twin",
    "random",
    "randomfamid",
    # "randomtwinid",
    # "rq5_exclude_pps",
    "x3zygos"
  )

  # Set all excluded variables to 0 in predictor matrix
  pred[exclude_vars, ] = 0
  pred[, exclude_vars] = 0

}

## PGS: impute and use as predictors in every group (MZ exception below) ----------

# The template above comes from the first group, which is an MZ group. There,
# mice flags every co-twin PGS (pgs_*2) as collinear with the twin's PGS and
# switches it off, which would carry over to the DZ groups too. So the PGS
# entries are reset here, and the co-twin PGS are then removed for the MZ
# groups only (next section).
model_vars = setdiff(colnames(pred), exclude_vars)

meth[rq6z_pgs_vars]             = "pmm"
pred[rq6z_pgs_vars, model_vars] = 1
pred[model_vars, rq6z_pgs_vars] = 1
diag(pred)                      = 0

## PGS do not predict each other ---------------------------------------------------

# (Added 13-11-2026.) A family is missing either all 24 PGS (twin and co-twin) or
# none, so for the families whose PGS are imputed, every other PGS is missing too
# (proportion of usable cases ~0). Used as predictors of each other, the PGS were
# imputed from each other's imputed values, and their chains drifted without
# converging (8_rq6_4_imputation_diagnostics.qmd). Each PGS is therefore imputed
# from the outcomes and phenotypic auxiliary variables only. The PGS still predict
# the outcomes and the other auxiliary variables.
pred[rq6z_pgs_vars, rq6z_pgs_vars] = 0

## MZ twins: co-twin PGS are copies of the twin's PGS ---------------------------

# MZ co-twins share one genotype, so in the MZ groups pgs_*2 is identical
# to pgs_*1 (and missing for the same families). The co-twin PGS are therefore
# removed explicitly from the MZ imputation models: they are not imputed and
# not used as predictors. (mice would usually drop them anyway as collinear,
# but this makes it certain, and stops MZ co-twins from receiving different
# imputed PGS.) Both twins' PGS are used in the DZ groups.

mz_groups = names(df_impute_list_flat)[str_detect(names(df_impute_list_flat), "\\.MZ ")]
pgs_cotwin_vars = pgs_vars2

meth_list = setNames(rep(list(meth), length(df_impute_list_flat)), names(df_impute_list_flat))
pred_list = setNames(rep(list(pred), length(df_impute_list_flat)), names(df_impute_list_flat))

for (g in mz_groups) {
  meth_list[[g]][pgs_cotwin_vars]   = ""
  pred_list[[g]][pgs_cotwin_vars, ] = 0
  pred_list[[g]][, pgs_cotwin_vars] = 0
}

testthat::test_that("PGS set up correctly in the imputation models", {
  testthat::expect_length(mz_groups, 2 * length(rq1y_twin))                      # MZ male and MZ female at each time point
  for (g in mz_groups) {
    d = df_impute_list_flat[[g]]
    both = !is.na(d$pgs_ea1) & !is.na(d$pgs_ea2)
    testthat::expect_equal(as.numeric(d$pgs_ea1[both]), as.numeric(d$pgs_ea2[both]))   # MZ co-twin PGS are copies
    testthat::expect_identical(is.na(d$pgs_ea1), is.na(d$pgs_ea2))                       # ... and missing for the same families
    testthat::expect_true(all(meth_list[[g]][pgs_cotwin_vars] == ""))
    testthat::expect_true(all(pred_list[[g]][, pgs_cotwin_vars] == 0))
    testthat::expect_true(all(meth_list[[g]][pgs_vars1] == "pmm"))
  }
  for (g in setdiff(names(df_impute_list_flat), mz_groups)) {
    testthat::expect_true(all(meth_list[[g]][rq6z_pgs_vars] == "pmm"))
    testthat::expect_true(all(pred_list[[g]][rq6y_all[1], rq6z_pgs_vars] == 1))
  }
  for (g in names(df_impute_list_flat)) {
    testthat::expect_true(all(pred_list[[g]][rq6z_pgs_vars, rq6z_pgs_vars] == 0))          # PGS do not predict each other ...
    pgs_rows = intersect(rq6z_pgs_vars, names(meth_list[[g]])[meth_list[[g]] == "pmm"])
    testthat::expect_true(all(pred_list[[g]][pgs_rows, rq6y_all] == 1))                    # ... but are imputed from the outcomes
    testthat::expect_true(all(rowSums(pred_list[[g]][pgs_rows, setdiff(model_vars, c(rq6y_all, rq6z_pgs_vars))]) > 0))   # ... and the phenotypic auxiliary variables
  }
})

df_impute %>%
  select(-any_of(exclude_vars)) %>%
  colnames() %>%
  saveRDS(results_file("8_1_imputation_variables"))

miceinit$loggedEvents

## Task list -------------------------------------------------------------------
# One task = one imputation of one sex-zygosity group at one time point. The
# order (groups vary fastest within each imputation) is relied on by 8_rq6_3.

tasks = expand.grid(
  group = names(df_impute_list_flat),
  imputation = 1:number_imputations,
  stringsAsFactors = FALSE
) %>%
  separate_wider_delim(group,
                       delim = ".",
                       names = c("timepoint", "sexzyg"),
                       cols_remove = FALSE) %>%
  mutate(seed = 1000 * imputation + match(group, names(df_impute_list_flat)))   # one seed per task (used by 8_rq6_1b_imputation_cluster_task.R), so each imputation is reproducible

saveRDS(tasks, results_file("8_1_tasks"))

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Save -------------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

## Input for the cluster jobs (copied to CREATE) --------------------------------

saveRDS(
  list(
    df_impute_list_flat = df_impute_list_flat,
    meth_list           = meth_list,
    pred_list           = pred_list,
    tasks               = tasks,
    number_imputations  = number_imputations,
    number_iterations   = number_iterations,
    created             = Sys.time()
  ),
  results_file("8_1_cluster_input")
)

## Input for 8_rq6_1c_imputation_combine.R (stays local) ------------------------
# The original (pre-attrition) data are needed to restore the row order and the
# outcome missingness after imputation.

saveRDS(
  list(
    original_dataset     = original_dataset,
    attritioned_datasets = attritioned_datasets,
    timepoints           = rq1y_twin,
    rq6y_all             = rq6y_all,
    pgs_vars1            = pgs_vars1,
    pgs_vars2            = pgs_vars2,
    rq6z_pgs_vars        = rq6z_pgs_vars,
    tasks                = tasks,
    number_imputations   = number_imputations,
    number_iterations    = number_iterations
  ),
  results_file("8_1_combine_input")
)

cat("\nSaved", results_file("8_1_cluster_input"), "and", results_file("8_1_combine_input"),
    "\nNow copy", results_file("8_1_cluster_input"), "to the cluster and run 8_rq6_1b_imputation_cluster_slurm (see its header).\n")
