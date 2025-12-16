# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load data --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

rm(list=ls())

source("0_load_data.R")

range_participation_outcomes = 6:8
mice_iter                    = 50

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

# This section creates missingness indicator tables for bivariate and univariate analysis patterns
# Unlike previous analyses, we only want to tag variables as missing if they HAVE data at Y12 but DO NOT have data at the later time points
# ATTRITION-BASED MISSINGNESS

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

# Create missingness indicator columns
## 1 = IS NOT MISSING, 0 = IS MISSING
for(i in seq_along(vars)){
  df[[missingcode[i]]] = !is.na(df[[x_var[i]]]) & !is.na(df[[y_var[i]]]) 
  df[[missingcode[i]]] = factor(as.numeric(df[[missingcode[i]]]))
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
    atwmed1,
    all_of(missingcode_table_univariate$missingcode)  # we just use univariate missingness indicators in imputation rather than bivariate indicators (that tell us if a participant has non-missing data on pairs of variables)
  ) %>%
  pivot_wider(
    id_cols = randomfamid,
    names_from = twin,
    values_from = c(atwmed1, starts_with("missing"), sex1)
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

has_rq6y_data = df %>%
  filter(rowSums(!is.na(across(all_of(rq6y)))) == 0) %>%
  select(all_of(rq6y))

df %>%
  select(all_of(rq6y))
  

compare_df_weighting(
  df_y = select(df, all_of(rq6y)),
  df_miss      = select(df, starts_with("missing")),
  df_x         = select(df_imputed_long, all_of(rq1x)),
  B = 1
)

set.seed(1)
umx::umx_set_silent(value=TRUE)
# debug(.safe_umxACE)
# debug(compare_ace_weighted)
# debug(compare_df_weighting)
# test = compare_df_weighting( )

umx::umx_set_cores(cores = 1)
print("start analysis")

# Set up parallel processing
plan(multisession, workers = n_workers)

ta = Sys.time()

weighted_comparisons = future_lapply(1:B, function(i) {
  compare_df_weighting()
}, 
future.seed = 1,
future.packages = c("umx","OpenMX")
)  

tb = Sys.time()
print(tb - ta)

# Reset to sequential processing
plan(sequential)

# names(variable_comparisons) = rq1y_twin1

saveRDS(weighted_comparisons, file = file.path("results", "3_weighted_comparisons.Rds"))



