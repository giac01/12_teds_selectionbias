
# NOTE - run a code review of this script and functions again at some point!
# Docker image bignardig/tidyverse442:v3

# Load data --------------------------------------------------------------------

source("0_load_data.R")

B = 2000 # number of bootstraps 
# 20 takes 7.7 minutes on 4 cores (1 core takes 0.02566667 hours to do 1 repetition)

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

exclude_missing_too_much_predictor_data = df %>%
  select(all_of(rq1x)) %>%
  apply(.,1,function(x) sum(is.na(x))>7)

exclude_twinless = df %>%
  pull(randomfamid) %>%
  table()

exclude_twinless = names(exclude_twinless[exclude_twinless==1])

exclude_twinless = df$randomfamid %in% exclude_twinless

a = nrow(df)

df = df %>%
  filter(!exclude_missing_too_much_predictor_data & !exclude_twinless)

cat("Excluded", a-nrow(df), "participants ")

rm(a, exclude_missing_too_much_predictor_data, exclude_twinless)


# Impute RQ1X data -------------------------------------------------------------

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
  maxit = 2
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

## Tests to check imputation worked  as expected -------------------------------

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
  
  check_nas = c(t(df_imputed_long)) %>%
    is.na() %>%
    which() %>%
    length()
  
  testthat::expect_equal(check_nas,0, info = "NAs in imputed dataset")
  
  
})

# Run analyses  ----------------------------------------------------------------

compare_df_weighting = function(
    df_y      = select(df, all_of(rq5y)),                         # Data on outcome variables that we want to compare pre- and post- weighting
    df_miss   = select(df, starts_with("missing")),
    df_x      = select(df_imputed_long, all_of(rq1x)),                              # Explanatory variables used for creating weights (baseline data in our case)
    family_id = pull(df, randomfamid),
    B = NULL                            # Number of bootstraps to perform
    ){
  
  # Initialize results structure
  boot_results = list(
    md        = list(),
    smd       = list(),
    # h         = list(),
    var       = list(),
    cor_resid = list()
    # srmr      = list()
  )
  
  families             = na.omit(unique(family_id))
  
  for (b in 1:B){

    boot_select_families = sample(families, length(families), replace = TRUE)
    
    family_rows   = split(seq_len(nrow(df_y)), family_id)
    selected_rows = family_rows[as.character(boot_select_families)]
    selected_rows = unlist(selected_rows, use.names = FALSE)
    
    df_y_boot     = df_y[selected_rows,]
    df_miss_boot  = df_miss[selected_rows,]
    df_x_boot     = df_x[selected_rows,]
    
    logistic_models = list()
    
    model_data   = cbind.data.frame(df_miss_boot, df_x_boot)
    
    df_weights   = matrix(data = NA, nrow = nrow(df_miss_boot), ncol = ncol(df_miss_boot))
    
    colnames(df_weights) = colnames(df_miss_boot)
    
    if(!all(sapply(model_data, function(x) length(which(is.na(x)))==0))) warning("Missing data in weights calculation")
    
    for (i in seq_along(df_miss_boot)){

      formula <- as.formula(paste(colnames(df_miss_boot)[i], "~", paste(colnames(df_x_boot), collapse = "+")))
      # logistic_models[[i]] = glm(formula, data = model_data, family = binomial, na.action = na.exclude)
      logistic_models[[i]] = speedglm::speedglm(formula, data = model_data, family = binomial(), na.action = na.exclude, fitted = TRUE)  # this version of logistic regression takes almost half the time...
      prediction = predict(logistic_models[[i]], type = "response", na.action = na.exclude)
      
      if (length(prediction)!=nrow(df_miss_boot)) stop("error with predictions")
      
      df_weights[,i] = 1 / prediction
      
      # V_CR = clubSandwich::vcovCR(twinmodels[[i]], cluster = twinmodels[[i]]$data$famid, type = "CR2")
      # 
      # clubSandwich::coef_test(twinmodels[[i]], vcov = V_CR, test = "z")
      
    }
    
    # Univariate Results
    boot_results$md[[b]]        = compare_md_weighted(df_y_boot, df_weights[,missingcode_table_univariate$missingcode])
    boot_results$smd[[b]]       = compare_smd_weighted(df_y_boot, df_weights[,missingcode_table_univariate$missingcode])
    boot_results$var[[b]]       = compare_var_weighted(df_y_boot, df_weights[,missingcode_table_univariate$missingcode])
    # Pairwise results 
    boot_results$cor_resid[[b]] = compare_correlation_weighted(df_y_boot, df_weights)

  }
  
  return(boot_results)
}

# Set up parallel processing
plan(multisession, workers = parallel::detectCores())

ta = Sys.time()

weighted_comparisons = future_lapply(1:B, function(i) {
  compare_df_weighting( B = 1 )
}, 
future.seed = 1)  

tb = Sys.time()
print(tb - ta)

# Reset to sequential processing
plan(sequential)

# names(variable_comparisons) = rq1y_twin1

saveRDS(weighted_comparisons, file = file.path("results", "3_weighted_comparisons.Rds"))

# Clean Data -------------------------------------------------------------------

md_df   = sapply(weighted_comparisons, function(x) x$md) %>%
            bind_rows(., .id = NULL)
smd_df  = sapply(weighted_comparisons, function(x) x$smd) %>%
            bind_rows(., .id = NULL)
# h_df    = do.call(rbind, boot_results$h)
var_df  = sapply(weighted_comparisons, function(x) x$var) %>%
            bind_rows(., .id = NULL)
cor_df  = sapply(weighted_comparisons, function(x) x$cor_resid ) %>%
            do.call("rbind", .) %>%
            `colnames<-`(missingcode_table[,"vars"])
  
bootstrap_iter = list(
  md_df, 
  smd_df, 
  var_df, 
  cor_df
  # srmr_df
  )

names(bootstrap_iter) = c("md", "smd", "var", "cor_resid")
  
bootstrap_summary = lapply(bootstrap_iter, function(df)
  apply(df,2, function(xx).mean_qi_pd(xx))
)

bootstrap_summary = lapply(bootstrap_summary, function(x) bind_rows(x, .id = "var"))

bootstrap_summary = bind_rows(bootstrap_summary, .id = "stat")
  
saveRDS(bootstrap_summary, file = file.path("results", "3_weighted_comparisons_bootstrap.Rds"))

