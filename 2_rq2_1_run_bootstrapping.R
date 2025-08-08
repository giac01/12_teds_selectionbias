# Docker container info --------------------------------------------------------

# bignardig/tidyverse442:v4

# Load Data --------------------------------------------------------------------

source("0_load_data.R")

number_bootstraps = 5000 # 5000 takes around 6.2 hours

## Only keep time points where all participants were eligible ------------------
remove_timepoints = c("btwdata","ctwdata", "gcdata","icdata","jcdata","pcwebdata") 
remove_timepoints = match(remove_timepoints, rq1y_twin)

rq1y_twin                         = rq1y_twin[-remove_timepoints]
rq1y_twin1                        = rq1y_twin1[-remove_timepoints]
rq1y_twin2                        = rq1y_twin2[-remove_timepoints]
rq1y_twin_labels                  = rq1y_twin_labels[-remove_timepoints]
rq1y_twin_labels_clean            = rq1y_twin_labels_clean[-remove_timepoints]
rq1y_twin_labels_clean_extrashort = rq1y_twin_labels_clean_extrashort[-remove_timepoints]

# Create Attritioned Datasets --------------------------------------------------

attritioned_datasets = list()

for (i in seq_along(rq1y_twin1)){
  
  filter = as.numeric(df[[rq1y_twin1[i]]])==0 # 1 = present (Y), 2 = not-present (N)
  
  attritioned_datasets[[i]] = df %>% 
    select("randomfamid", "twin", "random", "x3zygos", all_of(rq2y_all))
  
  attritioned_datasets[[i]][filter,rq2y_all] = NA
  
}

original_dataset = df %>%
  select("randomfamid", "twin", "random","x3zygos", all_of(rq2y_all))


# Run parallelised bootstrapped analyuses --------------------------------------

# Set up parallel processing
plan(multisession, workers = 12)

ta = Sys.time()

variable_comparisons = future_lapply(1:length(attritioned_datasets), function(i) {
  compare_df(
    attritioned_datasets[[i]][c("randomfamid",rq2y)],
    original_dataset[c("randomfamid",rq2y)],
    B = number_bootstraps
  )
}, 
future.seed = 1)  

tb = Sys.time()
print(tb - ta)

# Reset to sequential processing
plan(sequential)

names(variable_comparisons) = rq1y_twin1

saveRDS(variable_comparisons, file = file.path("results", "2_variable_comparisons.Rds"))

## Clean data into a single dataframe ------------------------------------------

variable_comparisons_df = lapply(variable_comparisons, function(x) x$bootstrap_summary)

for(i in 1:length(variable_comparisons_df)){
  #Looping across atttritioned datasets 
  
  for (j in 1:4){ # only first three sets of results relate to specific variables in rq2y
    variable_comparisons_df[[i]] = variable_comparisons_df[[i]][1:4] # this line just removes the correlation analyses that arne't relevant here
    variable_comparisons_df[[i]][[j]] = do.call(
      rbind,
      variable_comparisons_df[[i]][[j]]
    )
    variable_comparisons_df[[i]][[j]]$dataset  = rq1y_twin1[i]
    variable_comparisons_df[[i]][[j]]$stat = c("md","smd","h","var")[j]
    variable_comparisons_df[[i]][[j]]$variable = rq2y_labels
    rownames(variable_comparisons_df[[i]][[j]]) = NULL
  }
  
  variable_comparisons_df[[i]] = do.call(rbind.data.frame, variable_comparisons_df[[i]])
}


variable_comparisons_df = do.call(rbind.data.frame, variable_comparisons_df)

rownames(variable_comparisons_df) = NULL

saveRDS(variable_comparisons_df, file = file.path("results", "2_variable_comparisons_df.Rds"))

# Run ACE Analyses -------------------------------------------------------------

rq2y_prefix = rq2y_prefix[-1] # We don't want to analyse maternal education here as its shared between twins 

# rq2y_prefix = rq2y_prefix[-1]

attritioned_datasets_twin1 = lapply(
  attritioned_datasets, function(x) {
    x %>%
      filter(random == 1)
  }
)

original_dataset_twin1 = original_dataset %>%
  filter(random == 1)

# Set up parallel processing
options(future.globals.maxSize = 1000 * 1024^2)  # Increase if you have large objects
plan(multisession, workers = 12)

ta = Sys.time()

ace_comparisons = future_lapply(seq_along(rq2y_prefix), function(i) {
  compare_ace(
    df2 = attritioned_datasets_twin1,
    df1 = original_dataset_twin1,
    var = rq2y_prefix[i],   # Variable that we want to calculate ACE estimates for 
    B   = number_bootstraps
  )
}, 
future.seed = 1,
future.globals = list(
  original_dataset_twin1 = original_dataset_twin1,
  attritioned_datasets_twin1 = attritioned_datasets_twin1,
  rq2y_prefix = rq2y_prefix,
  number_bootstraps = number_bootstraps,
  calc_ace    = calc_ace,
  i = i,
  rq1y = rq1y_twin1,
  .mean_qi_pd = .mean_qi_pd,
  compare_ace = compare_ace  # if this is a custom function
),
future.packages = c("dplyr", "magrittr")
)  

tb = Sys.time()
print(tb - ta)

names(ace_comparisons) = rq2y_prefix

saveRDS(ace_comparisons, file = file.path("results", "2_ace_comparisons.Rds"))



