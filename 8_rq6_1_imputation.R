# Run using docker container: bignardig/tidyverse451:v6
# Run using commit: XXXXXXXXXXXXXX (see commit message)
# Run date: XX-Jan-2025

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load data --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

rm(list=ls())

source("0_load_data.R")

range_participation_outcomes = 6:8
number_imputations           = 8                                                # Number of Imputations
number_iterations            = 2
n_workers                    = 8                                                # Number of parallel jobs to run (number of cores)

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
  
  original_datasets[[i]] = attritioned_datasets[[i]] = df %>% 
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
imputed_mice = list()

ta = Sys.time()

imputed_mice = lapply(df_impute_list_flat, function(input_df)
  mice::futuremice(
    input_df, 
    parallelseed    = 1,
    n.core          = n_workers,
    m               = number_imputations, 
    maxit           = number_iterations, 
    method          = meth,
    predictorMatrix = pred
  )
)

tb = Sys.time()  # first run was 9.7 minutes

tb-ta  # 3.14 hours to do 24 imputations

saveRDS(imputed_mice, file = file.path("results","8_1_imputed_mice.Rds"))
