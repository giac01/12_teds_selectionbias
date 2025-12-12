# Load data --------------------------------------------------------------------

# Docker imagebignardig/tidyverse451:v6
# Ran using gith commit 1d527d3

## This section also applies filters

source("0_load_data.R")

df = df %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%
  filter(!(randomfamid %in% exclude_fams_rq6y)) 

## range of participation outcomes to use for attiritioning 

range_participation_outcomes = 6:8

cat("Participation Outcomes:\n", paste0(rq1y_twin_labels_clean[range_participation_outcomes], sep="\n"))

# Arguments --------------------------------------------------------------------

B         = 10000 # 10000 reps took 4.335921 hours
n_workers = 3 # Set to three as we only parellise over attrition time points

# 100 takes 2.8 mins 

# Create Attritioned Datasets --------------------------------------------------

attritioned_datasets = list()

for (i in seq_along(rq1y_twin1)){
  
  filter = as.numeric(df[[rq1y_twin1[i]]]) == 0 # 1 = present (Y), 2 = not-present (N)
  
  attritioned_datasets[[i]] = df %>% 
    select("randomfamid", "twin", "random", "x3zygos","sexzyg", all_of(rq6y))
  
  attritioned_datasets[[i]][filter,rq6y] = NA
  
}

original_dataset = df %>%
  select("randomfamid", "twin", "random","x3zygos", "sexzyg", all_of(rq6y))

# Run bootstrapped attritioning analyses ---------------------------------------

## Testing area ----------------------------------------------------------------

if(FALSE){
  i = 13
  
  debug(.boot_compare_df)
  debug(compare_df)
  
  xx =   compare_df(
    attritioned_datasets[[6]][c("randomfamid","sexzyg",rq6y)],
    original_dataset[c("randomfamid","sexzyg",rq6y)],
    vars = rq6y,
    B = 200
  )
}

##  Set up parallel processing -------------------------------------------------
plan(multisession, workers = n_workers)

ta = Sys.time()

variable_comparisons = future_lapply(range_participation_outcomes, function(i) {
  compare_df(
    df1  = attritioned_datasets[[i]][c("randomfamid","sexzyg",rq6y)],
    df2  = original_dataset[c("randomfamid","sexzyg",rq6y)],
    vars = rq6y,
    B    = B
  )
}, 
future.seed = 1,
future.packages = c("umx","OpenMx"),
future.globals = TRUE
)  

tb = Sys.time()
print(tb - ta)

# Reset to sequential processing
plan(sequential)

names(variable_comparisons) = rq1y_twin1[range_participation_outcomes]

saveRDS(variable_comparisons, file = file.path("results", "6_variable_comparisons.Rds"))
