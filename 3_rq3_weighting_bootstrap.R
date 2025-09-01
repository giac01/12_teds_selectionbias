
# Docker image  bignardig/tidyverse451:v5
# CODE REVIEW STATUS: reviewed again 1/sep/25. Could check internal functions again? 


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load data --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


source("0_load_data.R")

B = 1600 # number of bootstraps 
mice_iter = 10 # number of iterations for mice


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# PRELIMINARY SETUP: Create Missingness Indicator Variables --------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


# This section creates missingness indicator tables for bivariate and 
# univariate analysis patterns

# Create pairwise variable combinations matrix
test_correlation_matrix = matrix(
  nrow = length(rq5y),
  ncol = length(rq5y)
)

for(i in seq_along(rq5y)){
  for(j in seq_along(rq5y)){
    test_correlation_matrix[i,j] = paste(rq5y[i], rq5y[j], sep = "-")
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
if (!all(missingcode_table_univariate$x_var==rq5y)) stop("rq5y should match with univariate missingness indicators")

# Create missingness indicator columns in main dataset
for(i in seq_along(vars)){
  df[[missingcode[i]]] = !is.na(df[[x_var[i]]]) & !is.na(df[[y_var[i]]]) 
  df[[missingcode[i]]] = factor(as.numeric(df[[missingcode[i]]]))
}

# Cleanup temporary variables
rm(test_correlation_matrix, x_var, y_var, missingcode)


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Participant Exclusions ------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


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


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Data Imputation: Impute RQ1X baseline variables ----------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


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
# where_matrix[,str_detect(colnames(df_impute),"^missing")] = FALSE             # Not sure why this was here 

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
# Validation Tests: Check imputation quality ----------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


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


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Main Analysis: Bootstrap weighting comparisons ------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


compare_df_weighting = function(
    df_y         = select(df, all_of(rq5y)),                         # Data on outcome variables that we want to compare pre- and post- weighting
    df_miss      = select(df, starts_with("missing")),
    df_x         = select(df_imputed_long, all_of(rq1x)),                              # Explanatory variables used for creating weights (baseline data in our case)
    randomfamid  = pull(df, randomfamid),
    random       = pull(df, random),
    sexzyg       = pull(df, sexzyg),
    B            = NULL,                            # Number of bootstraps to perform
    save_weights = FALSE
    ){

  if (nrow(df_y) != nrow(df_imputed_long)) warning("imputed and df nrows don't match")
  
  # Initialize results structure
  boot_results = list(
    md        = list(),
    smd       = list(),
    # h         = list(),
    var       = list(),
    cor_resid = list(),
    ace       = list()
    # srmr      = list()
  )
  
  families                = na.omit(unique(randomfamid))
  
  if (length(families)*2 != nrow(df_imputed_long)) warning("length(families) != length(df_imputed_long)")
  
    boot_select_families = sample(families, length(families), replace = TRUE)
    
    family_rows   = split(seq_len(nrow(df_y)), randomfamid)
    selected_rows = family_rows[as.character(boot_select_families)]
    selected_rows = unlist(selected_rows, use.names = FALSE)
    
    df_y_boot     = df_y[selected_rows,]         # Data on outcome vars
    df_miss_boot  = df_miss[selected_rows,]      # Data on whether there is missing data
    df_x_boot     = df_x[selected_rows,]         # Baseline data to create propensity weights
    
    logistic_models = list()
    
    model_data    = cbind.data.frame(df_miss_boot, df_x_boot)
    
    df_weights    = matrix(data = NA, nrow = nrow(df_miss_boot), ncol = ncol(df_miss_boot))
    
    colnames(df_weights) = colnames(df_miss_boot)
    
    if(!all(sapply(model_data, function(x) length(which(is.na(x)))==0))) warning("Missing data in weights calculation")
    
    for (i in seq_along(df_miss_boot)){

      formula                = as.formula(paste(colnames(df_miss_boot)[i], "~", paste(colnames(df_x_boot), collapse = "+")))
      logistic_models[[i]]   = speedglm::speedglm(formula, data = model_data, family = binomial(), na.action = na.exclude, fitted = TRUE)  # this version of logistic regression takes almost half the time...
      prediction             = predict(logistic_models[[i]], type = "response", na.action = na.exclude)
      
      if (length(prediction)!=nrow(df_miss_boot)) stop("error with predictions")
      
      df_weights[,i]         = 1 / prediction
    
    }
    
    # Univariate Results - mds, smds, vars
    boot_results$md        = compare_md_weighted(df_y_boot, df_weights[,missingcode_table_univariate$missingcode])
    boot_results$smd       = compare_smd_weighted(df_y_boot, df_weights[,missingcode_table_univariate$missingcode])
    boot_results$var       = compare_var_weighted(df_y_boot, df_weights[,missingcode_table_univariate$missingcode])
    # Pairwise results - cors
    boot_results$cor_resid = compare_correlation_weighted(df_y_boot, df_weights)
    
    # ACE results - (needs to be wide formatted)
    
    df_y_boot_wide = cbind.data.frame(randomfamid, random, sexzyg, df_y_boot) %>% 
      mutate(random = random + 1) %>%
      pivot_wider(
        id_cols      = randomfamid,
        names_from   = random,
        values_from  = -c(randomfamid, random),
        names_sep    = "_"
      )
    
    # Average probabilities over twins, then calculate weights 
    df_weights_wide = df_weights[,missingcode_table_univariate$missingcode] %>%
      data.frame() %>%
      mutate(
        randomfamid  = randomfamid,
      ) %>%
      group_by(randomfamid) %>%
      summarise(across(everything(), function(x) mean(x^-1)^-1))
    
    # Supress messages is for annoying openMX output
    boot_results$ace = compare_ace_weighted(
        dfy    = df_y_boot_wide,
        w      = df_weights_wide,
        rq5y    = rq5y,
        sexzyg = sexzyg
    )
  
  return(boot_results)
}

# set.seed(1)
# test = compare_df_weighting( )

# Set up parallel processing
plan(multisession, workers = parallel::detectCores())

ta = Sys.time()

weighted_comparisons = future_lapply(1:B, function(i) {
  compare_df_weighting()
}, 
future.seed = 1)  

tb = Sys.time()
print(tb - ta)

# Reset to sequential processing
plan(sequential)

# names(variable_comparisons) = rq1y_twin1

saveRDS(weighted_comparisons, file = file.path("results", "3_weighted_comparisons.Rds"))

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Clean Results Data -------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

md_df   = sapply(weighted_comparisons, function(x) x$md) %>%
            t()
smd_df  = sapply(weighted_comparisons, function(x) x$smd) %>%
            t()
var_df  = sapply(weighted_comparisons, function(x) x$var) %>%
            t()
cor_df  = sapply(weighted_comparisons, function(x) x$cor_resid ) %>%
            t() %>% data.frame() %>%
            `colnames<-`(missingcode_table[,"vars"])
ace_male_df = sapply(weighted_comparisons, function(x) x$ace[[2]]$value ) %>%
                t()
ace_female_df = sapply(weighted_comparisons, function(x) x$ace[[4]]$value ) %>%
                 t()

# ace_male_df = cbind.data.frame(weighted_comparisons[[1]]$ace[[2]][,c("par","name","sex")],ace_male_df)

# ace_female_df = cbind.data.frame(weighted_comparisons[[1]]$ace[[4]][,c("par","name","sex")],ace_female_df)


bootstrap_iter = list(
  md_df, 
  smd_df, 
  var_df, 
  cor_df,
  ace_male_df,
  ace_female_df
  # srmr_df
  )

names(bootstrap_iter) = c("md", "smd", "var", "cor_resid", "ace_male", "ace_female")
  
bootstrap_summary = lapply(bootstrap_iter, function(df)
  apply(df,2, function(xx).mean_qi_pd(xx))
)

bootstrap_summary = lapply(bootstrap_summary, function(x) bind_rows(x, .id = "var"))

bootstrap_summary = bind_rows(bootstrap_summary, .id = "stat")
  
saveRDS(bootstrap_summary, file = file.path("results", "3_weighted_comparisons_bootstrap.Rds"))

