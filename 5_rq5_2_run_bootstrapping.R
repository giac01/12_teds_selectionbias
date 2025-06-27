# Load Data --------------------------------------------------------------------

source("0_load_data.R")
options(future.globals.maxSize = +Inf)  

df_rq5_imputed = readRDS(file.path("data","df_rq5_imputed.Rds")) 

imputed_datasets = df_rq5_imputed %>% # reformat to list 
  select(-sexzyg) %>%
  split(., df_rq5_imputed$.imp)

imputed_datasets = lapply(imputed_datasets, function(x) select(x, -.imp))

# Analysis: Compare distributions and correlations -----------------------------

plan(multisession, workers = 12)

ta = Sys.time()

boot_compare_results = future_lapply(1:length(imputed_datasets), function(i) {
  .boot_compare_df(
    df[c("randomfamid",rq5y)],
    imputed_datasets[[i]][c("randomfamid",rq5y)]
  )
}, 
future.seed = 1)  

tb = Sys.time()
print(tb - ta)

plan(sequential)

names(boot_compare_results) = paste("inp",1:length(imputed_datasets))

saveRDS(boot_compare_results, file.path("results", "5_boot_compare_results.Rds"))

# Analysis: Compare ACE estimates ----------------------------------------------

## Prep datasets ---------------------------------------------------------------

## We want to exclude families with with only one sibling in the study following application of the exclusion criteria

exclude_fams = df %>%
  count(randomfamid) %>% 
  filter(n == 1) %>%
  pull(randomfamid) %>%
  as.character()

names(exclude_fams[exclude_fams==1])

### Non-imputed dataset --------------------------------------------------------

df_nonimputed_wide = df %>%
  select("random","randomfamid","randomtwinid","x3zygos",starts_with(rq5y_prefix)) %>%
  filter(random==1) %>%
  filter(!(randomfamid %in% exclude_fams))

### Imputed dataset -------------------------------------------------------------

df_imputed_wide = df_rq5_imputed %>%
  mutate(
    twin = df$random[match(.$randomtwinid,df$randomtwinid)]+1,  # using random to define twin, as we will manually pivot_wider later...
  ) %>%
  pivot_wider(
    id_cols = c(randomfamid, x3zygos, .imp),
    names_from = twin,
    values_from = all_of(rq5y)
    ) %>%
  rename_with(~str_replace(., "1_1$","1")) %>%
  rename_with(~str_replace(., "1_2$","2")) %>%
  filter(!(randomfamid %in% exclude_fams))

df_imputed_wide = split(df_imputed_wide, df_imputed_wide$.imp)

### Perform checks -------------------------------------------------------------

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

## Run -------------------------------------------------------------------------

## Next thing to do here - adjust compare_ace so that it works with multiply-imputed dataasets 

# Set up parallel processing
options(future.globals.maxSize = 1000 * 1024^2) 
plan(multisession, workers = 12)

ta = Sys.time()

ace_comparisons = compare_ace_imputation(
    df1 = df_nonimputed_wide,
    df_imputed_list = df_imputed_wide,
    var = rq5y_prefix   # Variable that we want to calculate ACE estimates for 
  )

tb = Sys.time()
print(tb - ta)

saveRDS(ace_comparisons, file = file.path("results", "5_ace_comparisons.Rds"))



# Post-processing of results ---------------------------------------------------

## Variable Differences --------------------------------------------------------

md_df   = do.call(rbind, lapply(boot_compare_results, function(x) t(as.data.frame(x$md))))
smd_df  = do.call(rbind, lapply(boot_compare_results, function(x) t(as.data.frame(x$smd))))
h_df    = do.call(rbind, lapply(boot_compare_results, function(x) t(as.data.frame(x$h))))
cor_df  = do.call(rbind, lapply(boot_compare_results, function(x) t(as.data.frame(x$cor_resid))))
srmr_df = do.call(rbind, lapply(boot_compare_results, function(x) t(as.data.frame(x$srmr))))

bootstrap_iter = list(md_df, smd_df, h_df, cor_df, srmr_df)
names(bootstrap_iter) = c("md", "smd", "h", "cor_resid", "srmr")

bootstrap_summary = lapply(bootstrap_iter, function(df)
  apply(df,2, function(xx).mean_qi_pd(xx))
)

bootstrap_summary_df = bootstrap_summary[1:3]

for(i in 1:length(bootstrap_summary_df)){
  #Looping across atttritioned datasets 
  names(bootstrap_summary_df[[i]]) = rq5y
  bootstrap_summary_df[[i]] = list_rbind(bootstrap_summary_df[[i]], names_to = "outcome")
}

bootstrap_summary_df = list_rbind(bootstrap_summary_df, names_to = "parameter")


saveRDS(bootstrap_summary_df, file = file.path("results", "5_bootstrap_summary_df.Rds"))
