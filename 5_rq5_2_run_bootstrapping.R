# Load Data --------------------------------------------------------------------

source("0_load_data.R")

options(future.globals.maxSize = +Inf)  

df_rq5_imputed = readRDS(file.path("data","df_rq5_imputed.Rds")) 

imputed_datasets = df_rq5_imputed %>% # reformat to list 
  select(-sexzyg) %>%
  split(., df_rq5_imputed$.imp)

imputed_datasets = lapply(imputed_datasets, function(x) select(x, -.imp))

original_dataset = df %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%
  filter(!(randomfamid %in% rq5_exclude_fams)) %>%
  filter(!(randomfamid %in% rq5_exclude_fams_2)) %>%                            # Exclude fams with less than 30% data on all imputed data (excluding baseline data)
  select(
    all_of(c("randomfamid",rq5y))
  )

# Data check 

testthat::test_that("nrows of compared datasets match",{
  testthat::expect_equal(nrow(imputed_datasets[[1]]),nrow(original_dataset))
  testthat::expect_true(all(original_dataset$randomfamid %in% imputed_datasets[[1]]$randomfamid))
  testthat::expect_true(all(imputed_datasets[[1]]$randomfamid %in% original_dataset$randomfamid))
})

# Arguments --------------------------------------------------------------------

number_bootstraps_per_impute = 20 

# Analysis: Compare distributions and correlations -----------------------------

plan(multisession, workers = 12)

ta = Sys.time()

boot_compare_results = future_lapply(1:length(imputed_datasets), function(i) {
  .boot_compare_df(
    df1 = imputed_datasets[[i]][c("randomfamid",rq5y)],
    df2 = original_dataset,
    B = number_bootstraps_per_impute
  )
}, 
future.seed = 1)  

tb = Sys.time()
print(tb - ta)

plan(sequential)

names(boot_compare_results) = paste("inp",1:length(imputed_datasets))

saveRDS(boot_compare_results, file.path("results", "5_boot_compare_results.Rds"))

## Post-processing of results --------------------------------------------------

### Extract data from boot_compare_results -------------------------------------


md_df = do.call(rbind, lapply(seq_along(boot_compare_results), function(i) {
  result_df = t(as.data.frame(boot_compare_results[[i]]$md, row.names = rq5y))
  result_df = data.frame(.imp = i, .boot = 1:nrow(result_df), result_df, row.names = NULL)
  return(result_df)
}))

smd_df = do.call(rbind, lapply(seq_along(boot_compare_results), function(i) {
  result_df = t(as.data.frame(boot_compare_results[[i]]$smd, row.names = rq5y))
  result_df = data.frame(.imp = i, .boot = 1:nrow(result_df), result_df, row.names = NULL)
  return(result_df)
}))

h_df = do.call(rbind, lapply(seq_along(boot_compare_results), function(i) {
  result_df = t(as.data.frame(boot_compare_results[[i]]$h, row.names = rq5y))
  result_df = data.frame(.imp = i, .boot = 1:nrow(result_df), result_df, row.names = NULL)
  return(result_df)
}))

var_df = do.call(rbind, lapply(seq_along(boot_compare_results), function(i) {
  result_df = t(as.data.frame(boot_compare_results[[i]]$var, row.names = rq5y))
  result_df = data.frame(.imp = i, .boot = 1:nrow(result_df), result_df, row.names = NULL)
  return(result_df)
}))

cor_df = do.call(rbind, lapply(seq_along(boot_compare_results), function(i) {
  result_df = t(as.data.frame(boot_compare_results[[i]]$cor_resid))
  result_df = data.frame(.imp = i, .boot = 1:nrow(result_df), result_df, row.names = NULL)
  return(result_df)
}))

srmr_df = do.call(rbind, lapply(seq_along(boot_compare_results), function(i) {
  result_df = t(as.data.frame(boot_compare_results[[i]]$srmr))
  result_df = data.frame(.imp = i, .boot = 1:nrow(result_df), srmr = result_df, row.names = NULL)
  return(result_df)
}))

bootstrap_iter = list(md_df, smd_df, h_df, var_df, cor_df, srmr_df)
names(bootstrap_iter) = c("md", "smd", "h", "var",  "cor_resid", "srmr")

### Calculate p-values and confidence intervals --------------------------------


#### Calculate for md, smd, h, and var (all per variable in rq5y) --------------

bootstrap_summary = lapply(bootstrap_iter, function(df)
  apply(select(df,-.imp, -.boot),2, function(xx) .mean_qi_pd(xx))
)

bootstrap_summary_df = bootstrap_summary[1:4]

testthat::test_that("Bootstrap summaries have correct length", {
  expect_true(all(sapply(bootstrap_summary_df, length) == length(rq5y)))
})

for(i in 1:length(bootstrap_summary_df)){
  #Looping across atttritioned datasets 
  names(bootstrap_summary_df[[i]]) = rq5y
  bootstrap_summary_df[[i]] = list_rbind(bootstrap_summary_df[[i]], names_to = "outcome")
}

bootstrap_summary_df = list_rbind(bootstrap_summary_df, names_to = "parameter")

saveRDS(bootstrap_summary_df, file = file.path("results", "5_bootstrap_summary_df.Rds"))

#### Calculate for residual correlations (cor) and srmr ------------------------

# bootstrap_summary_df[["cor"]] = cor_df %>%
#   select(-.imp, -.boot) %>%
#   apply(.,2, function(xx) .mean_qi_pd(xx)) %>%
#   list_rbind()
# 
# bootstrap_summary_df[["srmr"]] = srmr_df %>%
#   pull(srmr) %>%
#   .mean_qi_pd()

# Analysis: Compare ACE estimates ----------------------------------------------

## Prep datasets ---------------------------------------------------------------

### Non-imputed dataset --------------------------------------------------------

df_nonimputed_wide = df %>%
  select("random","randomfamid","randomtwinid","x3zygos",starts_with(rq5y_prefix)) %>%
  filter(random==1) %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%
  filter(!(randomfamid %in% rq5_exclude_fams)) %>%
  filter(!(randomfamid %in% rq5_exclude_fams_2))                            # Exclude fams with less than 30% data on all imputed data (excluding baseline data)

### Imputed dataset -------------------------------------------------------------

# all imp in one df
df_imputed_wide0 = df_rq5_imputed %>%
  # mutate(
  #   twin = df$random[match(.$randomtwinid,df$randomtwinid)]+1,  # using random to define twin, as we will manually pivot_wider later...
  # ) %>%
  pivot_wider(
    id_cols = c(randomfamid, x3zygos, .imp),
    names_from = random,
    values_from = all_of(rq5y)
    ) %>%
  rename_with(~str_replace(., "1_1$","1")) %>%
  rename_with(~str_replace(., "1_0$","2")) %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%
  filter(!(randomfamid %in% rq5_exclude_fams)) 

# list of imputed datasets 
df_imputed_wide = split(df_imputed_wide0, df_imputed_wide0$.imp)

### Tests ----------------------------------------------------------------------

test_that("Family IDs are identical across datasets", {
  expect_identical(
    as.numeric(df_imputed_wide[[1]]$randomfamid),
    as.numeric(df_nonimputed_wide$randomfamid)
  )
  expect_identical(
    as.numeric(df_imputed_wide[[2]]$randomfamid),
    as.numeric(df_nonimputed_wide$randomfamid)
  )
})

testthat::test_that("check random variable has been correctly recoded",{
  df_test = df_nonimputed_wide %>%
    select(randomfamid, pcg1) %>%
    rename(pcg1_nonimputed = "pcg1") 
  
  df_test_imputed = df_imputed_wide0 %>% 
    filter(.imp ==1) %>%
    select(randomfamid, pcg1) %>%
    rename(pcg1_imputed = "pcg1") 
  
  df_test_join = full_join(df_test, df_test_imputed)
  
  n_mismatch = which(df_test_join$pcg1_nonimputed!=df_test_join$pcg1_imputed) %>% 
    length()
  
  testthat::expect_equal(0, n_mismatch)
})

## Run Anayses -----------------------------------------------------------------

## Next thing to do here - adjust compare_ace so that it works with multiply-imputed dataasets 

# Set up parallel processing
# options(future.globals.maxSize = 1000 * 1024^2) 
# plan(multisession, workers = 12)

ta = Sys.time()

ace_comparisons = compare_ace_imputation(
    df1 = df_nonimputed_wide,
    df_imputed_list = df_imputed_wide,
    B = 10,
    var = rq5y_prefix   # Variable(s) that we want to calculate ACE estimates for (can set to one variable at a time for parrellisation)
  )

tb = Sys.time()
print(tb - ta)

### Save results----------------------------------------------------------------

saveRDS(ace_comparisons, file = file.path("results", "5_ace_comparisons.Rds"))

