# Docker container info --------------------------------------------------------
# bignardig/tidyverse442:v7
# Run version: mkhfja2fas2
# Run date: 25-01-2026

# This script must be run in the terminal because it uses the plan multicore command. 

# Load Data --------------------------------------------------------------------

source("0_load_data.R")

number_bootstraps = 10000 # 500 takes 27.9 minutes.
                       
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
rm(df, df0, filter, rq5_percent_complete)

## Set up table of jobs --------------------------------------------------------

joblist = expand.grid(
  b = 1:number_bootstraps,
  i = 1:length(attritioned_datasets)
  )

# Set up parallel processing ---------------------------------------------------

plan(multicore, workers = 15)

ta = Sys.time()

boot_results = future_lapply(1:nrow(joblist), function(i) {
  .boot_compare_df(
    df1          = attritioned_datasets[[joblist[i,"i"]]][c("randomfamid","sexzyg",rq2y)],
    df2          = original_dataset[c("randomfamid","sexzyg",rq2y)],
    B            = 1,
    vars         = rq2y,
    ace_analysis = TRUE
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

saveRDS(boot_results, file = file.path("results", "2_boot_results.Rds"))


