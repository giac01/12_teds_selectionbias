# Run using docker container: bignardig/tidyverse451:v7
# Run using commit: vbmbnbmnb (see commit message)
# Run date: 12-Jan-2025

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load data --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

rm(list=ls())

source("0_load_data.R")

range_participation_outcomes = 6:8
number_imputations           = 250                                              # Number of Imputations 250: 18.9 hours
number_iterations            = 50                                               
n_workers                    = 16                                               # Number of parallel jobs to run (number of cores)

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
  filter(!(randomfamid %in% exclude_fams_rq6y)) %>%
  filter(random == 1) 

cat("Excluded", a-nrow(df), "participants ")

rm(a)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Attrition Data ---------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
original_datasets    = list()
attritioned_datasets = list()

for (i in seq_along(rq1y_twin1)){
  
  filter = as.numeric(df[[rq1y_twin1[i]]]) == 0   # (1/F = present, 0/T = NOT present)
  
  original_datasets[[i]] = attritioned_datasets[[i]] = df %>%                   # this is kind of redundant as each element of the list is identical to each other and to original_dataset
    select("randomfamid", "twin", "random", "x3zygos","sexzyg", starts_with(rq6y_prefix), all_of(rq6z_vars))
  
  attritioned_datasets[[i]][filter,rq6y] = NA
  
}

names(original_datasets)    = rq1y_twin
names(attritioned_datasets) = rq1y_twin

original_dataset = df %>%
    select("randomfamid", "twin", "random","x3zygos", "sexzyg", starts_with(rq6y_prefix), all_of(rq6z_vars))


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Impute ---------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


## list of variables used ------------------------------------------------------

rq6_imputation_vars = c(
  rq6y,
  rq6z_vars
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

df_impute %>%
  select(-any_of(exclude_vars)) %>%
  colnames() %>%
  saveRDS(file.path("results","8_1_imputation_variables.Rds"))

miceinit$loggedEvents

# Impute groups separately -----------------------------------------------------
# Using task-level parallelization for maximum efficiency
# Each imputation runs as a separate task to fully utilize all cores

tasks = expand.grid(
  group = names(df_impute_list_flat),
  imputation = 1:number_imputations,
  stringsAsFactors = FALSE
) %>%
  separate_wider_delim(group,
                       delim = ".",
                       names = c("timepoint", "sexzyg"),
                       cols_remove = FALSE)

## Run all imputations in parallel ---------------------------------------------

plan(multicore, workers = n_workers)

ta = Sys.time()

all_imputations = future_lapply(1:nrow(tasks), function(i) {
  group = tasks$group[i]

  # Run single imputation for this task
  mice(
    df_impute_list_flat[[group]],
    m               = 1,  # Single imputation per task
    maxit           = number_iterations,
    method          = meth,
    predictorMatrix = pred,
    printFlag       = FALSE
  )
}, future.seed = 1)

tb = Sys.time()

tb - ta

# warnings()

saveRDS(all_imputations, file = file.path("results","8_1_all_imputations.Rds"))
# all_imputations <- readRDS("~/Dropbox/Work/projects/12_teds_selectionbias/results/8_1_all_imputations.Rds")
# all_imputations[[10]]$loggedEvents

## Combine single imputations back by timepoint --------------------------------
# For each timepoint, rbind all sexzyg groups together for each imputation

imputed_mice = list()

for (i in rq1y_twin) {
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
    rq6y_vars = c(paste0(rq6y_prefix,1),paste0(rq6y_prefix,2))
    
    for (var in rq6y_vars) {
      na_mask = is.na(original_dataset[[var]])
      imp_list[[k]][[var]][na_mask] = NA
    }
  }

  # Store list of imputations for this timepoint
  imputed_mice[[i]] = imp_list
}


# names(imputed_mice) = names(df_impute_list_flat)

# Clean up
plan(sequential)

saveRDS(imputed_mice, file = file.path("results","8_1_imputed_mice.Rds"))



# Sanity Check

cat("original missingness\n")
table(is.na(original_dataset$lcg1))
cat("\nmissingness after attritioning\n")
table(is.na(attritioned_datasets[[1]]$lcg1))
table(is.na(attritioned_datasets[[2]]$lcg1))
table(is.na(attritioned_datasets[[3]]$lcg1))
cat("\nmissingness after attritioning + imputation\n")
table(is.na(imputed_mice[[1]][[1]]$lcg1))
table(is.na(imputed_mice[[2]][[1]]$lcg1))
table(is.na(imputed_mice[[3]][[1]]$lcg1))


# all_imputations[[1]] %>% plot(layout = c(5,5), col = 6)
# all_imputations[[100]] %>% plot(layout = c(3,3), y = c(paste0(rq6y_prefix,1),paste0(rq6y_prefix,2)))
# 
#                               