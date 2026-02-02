# Run using docker container: bignardig/tidyverse451:v6
# Run using commit: asdasf23143xcv (see commit message)
# Run date: 19-Dec-2015

# script MUST BE RUN IN TERMINAL USING:
# export OMP_NUM_THREADS=1
# export OPENBLAS_NUM_THREADS=1
# export MKL_NUM_THREADS=1
# export VECLIB_MAXIMUM_THREADS=1
# Rscript 7_rq6_1_weighting.R


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load data --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

rm(list=ls())

source("0_load_data.R")

range_participation_outcomes = 6:8
mice_iter                    = 50                                               # Number of iterations of the MICE algorithm (which is ran only once)
B                            = 10000                                              # Number of bootstrap resamples (200 takes 6 minutes, 10000 should take 5 hours)
n_workers                    = 8                                               # Number of parallel jobs to run (number of cores)

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
  filter(!(randomfamid %in% exclude_fams_rq6y)) 

cat("Excluded", a-nrow(df), "participants ")

rm(a)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Attrition Data ---------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

attritioned_datasets = list()

for (i in seq_along(rq1y_twin1)){
  
  filter = as.numeric(df[[rq1y_twin1[i]]]) == 0 # 1 = present (Y), 2 = not-present (N)
  
  attritioned_datasets[[i]] = df %>% 
    select("randomfamid", "twin", "random", "x3zygos","sexzyg", all_of(rq6y))
  
  attritioned_datasets[[i]][filter,rq6y] = NA
  
}

original_dataset = df %>%
  select("randomfamid", "twin", "random","x3zygos", "sexzyg", all_of(rq6y))

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Create Missingness Indicator Variables ---------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ATTRITION-BASED MISSINGNESS
# This section creates missingness indicator tables for bivariate and univariate analyses
# Unlike previous analyses, we only want to tag variables as missing if they HAVE data at Y12 but DO NOT have data at the later time points - 
#   ... otherwise, we will set the missingness indicator to NA to flag to the compare_df_weighted function that we want to IGNORE that observation

# Create pairwise variable combinations matrix
test_correlation_matrix = matrix(
  nrow = length(rq6y),
  ncol = length(rq6y)
)

for(i in seq_along(rq6y)){
  for(j in seq_along(rq6y)){
    test_correlation_matrix[i,j] = paste(rq6y[i], rq6y[j], sep = "-")
  }
}

# Extract variable pairs and create missing codes
vars        = test_correlation_matrix[lower.tri(test_correlation_matrix, diag = TRUE)]
x_var       = str_extract(vars, "^[^-]+")
y_var       = str_extract(vars, "[^-]+$")
missingcode = paste0("missing",1:length(vars))

# Create lookup tables for missingness patterns
missingcode_table            = cbind(vars, x_var, y_var, missingcode)           # Bivariate indicators
missingcode_table_univariate = missingcode_table %>%                            # Univariate indicators
                                as.data.frame() %>%
                                filter(x_var == y_var)

# Validation check
if (!all(missingcode_table_univariate$x_var==rq6y)) stop("rq6y should match with univariate missingness indicators")

missingcode_list = list()

# Create list of missingness indicator vars. 
## Each list element depends on the attritioning outcome used
## 1 = IS NOT MISSING, 0 = IS MISSING

for (i  in seq_along(rq1y_twin)){
  # Create empty data frame with
  missingcode_list[[i]] = df %>%
                            select(-everything())
  for(j in seq_along(vars)){
    # if both x_var and y_var are not missing in the attritioned dataset, set to 1, else 0
    missingcode_list[[i]][[missingcode[j]]] = (!is.na(attritioned_datasets[[i]][[x_var[j]]]) & !is.na(attritioned_datasets[[i]][[y_var[j]]])) %>%
                                                as.numeric() %>%
                                                factor()
    
    # if x_var or y_var are missing in the non-attritioned dataset, set to NA (exclude from analysis)
    missingcode_list[[i]][[missingcode[j]]][is.na(df[[x_var[j]]]) | is.na(df[[y_var[j]]])] = NA


  }
}

# Cleanup temporary variables
rm(test_correlation_matrix, x_var, y_var, missingcode)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Data Imputation: Impute RQ1X baseline variables ------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# THIS SECTION IS COPIED FROM 3_rq3_1_weighting_bootstrap.R - ANY CHANGES COULD BE MADE ACROSS BOTH

# twin-level variables
df_leftjoin = df %>%
  select(
    randomfamid,
    twin,
    sex1,
    atwmed1
  ) %>%
  pivot_wider(
    id_cols = randomfamid,
    names_from = twin,
    values_from = c(atwmed1, sex1)
  )

# family-level variables 
df_rightjoin = df %>%
  filter(twin == 1) %>%
  select(
    randomfamid,
    # sexzyg,
    all_of(rq1x),
    -atwmed1,
    -sex1
  )

df_impute = left_join(df_leftjoin, df_rightjoin, by = "randomfamid")

rm(df_leftjoin, df_rightjoin)

df_imputed_mice = mice(
  df_impute,
  m = 1,
  maxit = 0
)

where_matrix = df_imputed_mice$where

predictor_matrix = df_imputed_mice$predictorMatrix
predictor_matrix[, "randomfamid"] = 0 
predictor_matrix[,str_detect(colnames(predictor_matrix),"missing144_1|missing144_2")  ] = 0     # Getting some errors with collinearity here
predictor_matrix[ str_detect(colnames(predictor_matrix),"missing144_1|missing144_2|"),] = 0

df_imputed_mice = mice(
  df_impute,
  method = "pmm",
  where = where_matrix,
  predictorMatrix = predictor_matrix,
  m = 1,
  maxit = mice_iter
)

df_imputed = mice::complete(
  df_imputed_mice,
  action = 1 # just return the first imputed dataset
)

df_imputed_long = df_imputed %>%
  mutate(across(ends_with(c("_1", "_2")), as.character)) %>%
  pivot_longer(
    cols = ends_with(c("_1", "_2")),
    names_to = c("variable", "twin"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = !matches("variable|value"),
    names_from = variable,
    values_from = value
  ) %>%
  mutate(across(starts_with("missing"), as.factor)) %>%
  mutate(across(matches("twin|atwmed1|sex1"), ~ as.numeric(.))) %>%
  mutate(randomtwinid = as.numeric(paste0(randomfamid, twin))) %>%
  filter((.$randomtwinid %in% df$randomtwinid))

sapply(df_imputed_long, function(x) length(which(is.na(x))))


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Main Analysis: Bootstrap weighting comparisons ------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# debug(compare_df_weighting)
# debug(compare_ace_weighted)
# # debug(compare_correlation_weighted)
# # debug(compare_md_weighted)
# compare_df_weighting(
#   df1          = select(df, all_of(rq6y)),
#   df2         =  select(attritioned_datasets[[i]], all_of(rq6y)),
#   df_miss      = missingcode_list[[i]],
#   df_x         = select(df_imputed_long, all_of(rq1x)),
#   vars         = rq6y,
#   B = 1
# )

set.seed(1)
umx::umx_set_silent(value=TRUE)
umx::umx_set_cores(cores = 1)
print("start analysis")

# Set up parallel processing
plan(multicore, workers = n_workers)

ta = Sys.time()

weighted_comparisons = list()

weighted_comparisons[[1]] = future_lapply(1:B, function(i) {
    compare_df_weighting(
      df1     = select(df,                        all_of(rq6y)),                # Original (unattritioned) dataset
      df2     = select(attritioned_datasets[[1]], all_of(rq6y)),                # Attritioned dataset
      df_miss =            missingcode_list[[1]],                               # Missingness indicators 
      df_x    = select(df_imputed_long,           all_of(rq1x)),                # Imputed Baseline variables used to generate weights
      vars    = rq6y                                                            # Outcome variables
    )
  }, 
future.seed = 1,
future.packages = c("umx","OpenMX","speedglm")
)
tb = Sys.time()
print(paste("Analysis 1 completed in:", round(difftime(tb, ta, units = "mins"), 2), "minutes"))

weighted_comparisons[[2]] = future_lapply(1:B, function(i) {
  compare_df_weighting(
    df1     = select(df,                        all_of(rq6y)),
    df2     = select(attritioned_datasets[[2]], all_of(rq6y)),
    df_miss =            missingcode_list[[2]],
    df_x    = select(df_imputed_long,           all_of(rq1x)),
    vars    = rq6y
  )
}, 
future.seed = 1,
future.packages = c("umx","OpenMX","speedglm")
)
tc = Sys.time()
print(paste("Analysis 2 completed in:", round(difftime(tc, tb, units = "mins"), 2), "minutes"))

weighted_comparisons[[3]] = future_lapply(1:B, function(i) {
  compare_df_weighting(
    df1     = select(df,                        all_of(rq6y)),
    df2     = select(attritioned_datasets[[3]], all_of(rq6y)),
    df_miss =            missingcode_list[[3]],
    df_x    = select(df_imputed_long,           all_of(rq1x)),
    vars    = rq6y
  )
}, 
future.seed = 1,
future.packages = c("umx","OpenMX", "speedglm")
)
td = Sys.time()
print(paste("Analysis 3 completed in:", round(difftime(td, tc, units = "mins"), 2), "minutes"))

print(paste("Total time for all analyses:", round(difftime(td, ta, units = "mins"), 2), "minutes"))

# Reset to sequential processing
plan(sequential)

saveRDS(weighted_comparisons, file = file.path("results", "7_weighted_comparisons.Rds"))



