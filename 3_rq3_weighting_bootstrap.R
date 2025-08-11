source("0_load_data.R")

B = 100 # number of bootstraps

# Create Missingness Indicator Variable for each pairwise sets of variables ----

test_correlation_matrix = matrix(
  nrow = length(rq5y),
  ncol = length(rq5y)
)

for(i in seq_along(rq5y)){
  for(j in seq_along(rq5y)){
    test_correlation_matrix[i,j] = paste(rq5y[i], rq5y[j], sep = "-")
  }
}

vars        = test_correlation_matrix[lower.tri(test_correlation_matrix, diag = TRUE)]

x_var       = str_extract(vars, "^[^-]+")
y_var       = str_extract(vars, "[^-]+$")
missingcode = paste0("missing",1:length(vars))

missingcode_table            = cbind(vars, x_var, y_var, missingcode)
missingcode_table_univariate = missingcode_table %>%
  as.data.frame() %>%
  filter(x_var == y_var)

# Create columns to see if data is pairwise missing (0) or not (1)

for(i in seq_along(vars)){
  df[[missingcode[i]]] = !is.na(df[[x_var[i]]]) & !is.na(df[[y_var[i]]]) 
  df[[missingcode[i]]] = factor(as.numeric(df[[missingcode[i]]]))
}

rm(test_correlation_matrix, x_var, y_var, missingcode)

# Decide exclusion criteria ----------------------------------------------------

exclude = df %>%
  select(rq1x) %>%
  apply(.,1,function(x) sum(is.na(x))>7)

df = df %>%
  filter(!exclude)

# Impute RQ1X data -------------------------------------------------------------

# twin-level variables
df_leftjoin = df %>%
  select(
    randomfamid,
    twin,
    sex1,
    atwmed1,
    all_of(missingcode_table_univariate$missingcode)
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
    rq1x,
    -atwmed1,
    -sex1
  )

df_impute = left_join(df_leftjoin, df_rightjoin, by = "randomfamid")

rm(df_leftjoin, df_rightjoin)

where_matrix = is.na(df_impute)
where_matrix[,str_detect(colnames(df_impute),"^missing")] = FALSE

df_imputed_mice = mice(
  df_impute,
  method = "pmm",
  where = where_matrix,
  m = 1,
  maxit = 1
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



## Tests to check imputation worked -------------------------------------------------------------------------

test_that("Imputed data matches original data structure and values", { 
  x = df_imputed_long %>%
    mutate(randomtwinid = paste0(randomfamid, twin)) %>%
    # slice(match(df$randomtwinid, .$randomtwinid)) %>%
    mutate(across(everything(), ~as.character(.))) 
  y = df %>%
    select(
      colnames(x)
    ) %>%
    mutate(across(everything(), ~as.character(.)))
  
  x[is.na(y)] = NA
  y[is.na(x)] = NA
  
  testthat::expect_equal(x,y, info = "Data do not match")
  
  x = df_imputed_long %>%
    sapply(., class)
  y = df %>%
    select(
      colnames(df_imputed_long)
    ) %>%
    sapply(., class)
  
  testthat::expect_equal(x,y, info = "Variable classes do not match")
  
  x = as.numeric(df_imputed_long$randomtwinid)
  y = as.numeric(df$randomtwinid)
  
  testthat::expect_equal(x,y, info = "randomtwinids match")
  
})

# Run analyses  ----------------------------------------------------------------

# UP TO HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Set up parallel processing
plan(multisession, workers = 12)

ta = Sys.time()

variable_comparisons = future_lapply(1:B, function(i) {
  compare_df_weighting(
    df      = select(df, all_of(rq5y)),
    weights = select(df, starts_with("missing")),
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

