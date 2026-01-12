# Run using docker container: bignardig/tidyverse451:v7 
# Run using commit: gjghg74565 (see commit message)
# Run date: 10-Jan-2025

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load Data --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(MKL_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)

source("0_load_data.R")

options(future.globals.maxSize = +Inf)  

df_rq5_imputed = readRDS(file.path("data","df_rq5_imputed.Rds")) 

imputed_datasets = df_rq5_imputed %>% # reformat to list 
  # select(-sexzyg) %>%
  split(., df_rq5_imputed$.imp)

imputed_datasets = lapply(imputed_datasets, function(x) select(x, -.imp))

original_dataset = df %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%
  filter(!(randomfamid %in% rq5_exclude_fams)) %>%
  filter(!(randomfamid %in% rq5_exclude_fams_2)) %>%                            # Exclude fams with less than 30% data on all imputed data (excluding baseline data)
  select(
    all_of(c("randomfamid","sexzyg",rq5y))
  )

# Data checks

testthat::test_that("nrows of compared datasets match",{
  testthat::expect_equal(nrow(imputed_datasets[[1]]),nrow(original_dataset))
  testthat::expect_true(all(original_dataset$randomfamid %in% imputed_datasets[[1]]$randomfamid))
  testthat::expect_true(all(imputed_datasets[[1]]$randomfamid %in% original_dataset$randomfamid))
  testthat::expect_true(all(rq5y %in% colnames(imputed_datasets[[1]])))
})

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Arguments --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

number_bootstraps_per_impute = 10000/200                                           # 1.9 hours
number_cores                 = 14

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Run Analysis -----------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

plan(multicore, workers = number_cores)

ta = Sys.time()

boot_compare_results = future_lapply(1:length(imputed_datasets), function(i) {
  .boot_compare_df(
    df1  = imputed_datasets[[i]][c("randomfamid","sexzyg",rq5y)],
    df2  = original_dataset,
    B    = number_bootstraps_per_impute,
    vars = rq5y
  )
}, 
future.seed = 1,
future.packages = c("umx","OpenMx")
)  

tb = Sys.time()
print(tb - ta)

plan(sequential)

names(boot_compare_results) = paste("inp",1:length(imputed_datasets))

saveRDS(boot_compare_results, file.path("results", "5_boot_compare_results.Rds"))

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Post-processing of results ---------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

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

ace_df = do.call(rbind, lapply(seq_along(boot_compare_results), function(i) {
  result_df = lapply(boot_compare_results[[i]]$ace, function(x) do.call(rbind,x))
  result_df = do.call(rbind, result_df)
  result_df = result_df %>%
    mutate(.imp = i) %>%
    group_by(par, name, sex, group) %>%
    mutate(.boot = seq_len(dplyr::n()))
  return(result_df)
}))


bootstrap_iter = list(md_df, smd_df, var_df, cor_df, srmr_df)
names(bootstrap_iter) = c("md", "smd", "var",  "cor_resid", "srmr")

### Calculate p-values and confidence intervals --------------------------------


bootstrap_summary = lapply(bootstrap_iter, function(df)
  apply(select(df,-.imp, -.boot),2, function(xx) .mean_qi_pd(xx))
)

bootstrap_summary_df = bootstrap_summary[1:3]

testthat::test_that("Bootstrap summaries have correct length", {
  expect_true(all(sapply(bootstrap_summary_df, length) == length(rq5y)))
})

for(i in 1:length(bootstrap_summary_df)){
  #Looping across atttritioned datasets 
  names(bootstrap_summary_df[[i]]) = rq5y
  bootstrap_summary_df[[i]] = list_rbind(bootstrap_summary_df[[i]], names_to = "outcome")
}

bootstrap_summary_df = list_rbind(bootstrap_summary_df, names_to = "parameter")

## Calculate bootstrap summaries for ACE seperately ----------------------------

bootstrap_summary_df_ace = ace_df %>%
  mutate(
    group = recode(group,
      "df1"  = "Imputed",
      "df2"  = "Original",
      "diff" = "Difference"
    )
  ) %>%
  group_by(par, name, sex, group) %>%
  summarise(
    .mean_qi_pd(value),
    .groups = "drop"
  )


saveRDS(bootstrap_summary_df_ace, file = file.path("results", "5_bootstrap_summary_df_ace.Rds"))
saveRDS(bootstrap_summary_df,     file = file.path("results", "5_bootstrap_summary_df.Rds"))

