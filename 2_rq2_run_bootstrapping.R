# Load Data --------------------------------------------------------------------

source("0_load_data.R")

number_bootstraps = 100

rq1y = rq1y[-1] # Remove year 2 time point from comparison list 

# Create Attrittioned Datasets -------------------------------------------------

attritioned_datasets = list()

for (i in seq_along(rq1y)){
  
  filter = as.numeric(df[[rq1y[i]]])==0
  
  attritioned_datasets[[i]] = df %>% 
    select(all_of(rq2y))
  
  attritioned_datasets[[i]][filter,] = NA
  
}

original_dataset = df %>%
  select(all_of(rq2y))

# Run parallelised bootstrapped analyuses --------------------------------------

library(future)
library(future.apply)

# Set up parallel processing
plan(multisession, workers = 12)

ta = Sys.time()

variable_comparisons <- future_lapply(1:length(attritioned_datasets), function(i) {
  compare_df(
    attritioned_datasets[[i]],
    original_dataset,
    B = number_bootstraps
  )
}, 
future.seed = 1)  

tb = Sys.time()
print(tb - ta)

# Reset to sequential processing
plan(sequential)

saveRDS(variable_comparisons, file = file.path("results", "variable_comparisons.Rds"))

# Clean data into a sigle dataframe --------------------------------------------

names(variable_comparisons) = rq1y

variable_comparisons_df = lapply(variable_comparisons, function(x) x$bootstrap_summary)

for(i in 1:length(variable_comparisons_df)){
  for (j in 1:3){ # only first three sets of results relate to specific variables in rq2y
    variable_comparisons_df[[i]] = variable_comparisons_df[[i]][1:3]
    variable_comparisons_df[[i]][[j]] = do.call(
      rbind,
      variable_comparisons_df[[i]][[j]]
    )
    variable_comparisons_df[[i]][[j]]$dataset  = rq1y[i]
    variable_comparisons_df[[i]][[j]]$stat = c("md","smd","h")[j]
    variable_comparisons_df[[i]][[j]]$variable = rq2y_labels
    rownames(variable_comparisons_df[[i]][[j]]) = NULL
  }
  
  variable_comparisons_df[[i]] = do.call(rbind.data.frame, variable_comparisons_df[[i]])
}


saveRDS(variable_comparisons_df, file = file.path("results", "variable_comparisons_df.Rds"))


variable_comparisons_df = do.call(rbind.data.frame, variable_comparisons_df)
