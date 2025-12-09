# Docker container info --------------------------------------------------------

# bignardig/tidyverse442:v4

# Load Data --------------------------------------------------------------------

source("0_load_data.R")

number_bootstraps = 10000 # 5000 takes around 6.2 hours

df = df %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) 

# Create Attritioned Datasets --------------------------------------------------

attritioned_datasets = list()

for (i in seq_along(rq1y_twin1)){
  
  filter = as.numeric(df[[rq1y_twin1[i]]])==0 # 1 = present (Y), 2 = not-present (N)
  
  attritioned_datasets[[i]] = df %>% 
    select("randomfamid", "twin", "random", "x3zygos", "sexzyg", all_of(rq2y_all))
  
  attritioned_datasets[[i]][filter,rq2y_all] = NA
  
}

original_dataset = df %>%
  select("randomfamid", "twin", "random","x3zygos", "sexzyg", all_of(rq2y_all))


# Run parallelised bootstrapped analyuses --------------------------------------

# Set up parallel processing
plan(multisession, workers = 12)

ta = Sys.time()

variable_comparisons = future_lapply(1:length(attritioned_datasets), function(i) {
  compare_df(
    df1  = attritioned_datasets[[i]][c("randomfamid","sexzyg",rq2y)],
    df2  = original_dataset[c("randomfamid","sexzyg",rq2y)],
    B    = number_bootstraps,
    vars = rq2y,
    ace_analysis = FALSE
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
  
  for (j in 1:3){ # only first three sets of results relate to specific variables in rq2y
    variable_comparisons_df[[i]] = variable_comparisons_df[[i]][1:3] # this line just removes the correlation analyses that arne't relevant here
    variable_comparisons_df[[i]][[j]] = do.call(
      rbind,
      variable_comparisons_df[[i]][[j]]
    )
    variable_comparisons_df[[i]][[j]]$dataset  = rq1y_twin1[i]
    variable_comparisons_df[[i]][[j]]$stat = c("md","smd","var")[j]
    variable_comparisons_df[[i]][[j]]$variable = rq2y_labels
    rownames(variable_comparisons_df[[i]][[j]]) = NULL
  }
  
  variable_comparisons_df[[i]] = do.call(rbind.data.frame, variable_comparisons_df[[i]])
}


variable_comparisons_df = do.call(rbind.data.frame, variable_comparisons_df)

rownames(variable_comparisons_df) = NULL

saveRDS(variable_comparisons_df, file = file.path("results", "2_variable_comparisons_df.Rds"))



