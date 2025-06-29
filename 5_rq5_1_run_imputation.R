# Load things ------------------------------------------------------------------

source("0_load_data.R")

number_imputations = 10

number_iterations = 5

# Set up imputation  -------------------------------------------------------------------
df_impute = df %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%
  filter(!(randomfamid %in% rq5_exclude_fams)) %>%
  filter(random == 1) %>%
  select(
    sexzyg,
    random,
    twin,
    randomfamid,
    # randomtwinid,
    rq5_exclude_pps, 
    x3zygos,
    # all_of(rq5z),
    starts_with(rq5y_prefix)
  )

df_split = split(df_impute, df_impute$sexzyg)

# Set up predictor matrix ------------------------------------------------------

if (TRUE){
  miceinit = mice(df_impute, method = "pmm", m = 1, maxit = 0)
  
  meth = miceinit$method
  pred = miceinit$predictorMatrix
  
  exclude_vars = c(
    "sexzyg",  
    "twin",
    "random",
    "randomfamid",
    # "randomtwinid",
    "rq5_exclude_pps",
    "x3zygos"
  )
  
  # Set all excluded variables to 0 in predictor matrix
  pred[exclude_vars, ] = 0
  pred[, exclude_vars] = 0
  
}

miceinit$loggedEvents

if (FALSE){
table(df$aethnicc)

df_impute %>%
  filter(sexzyg=="MZ male") %>%
  select(pcg1, cohort) %>%
  mutate(across(1, is.na)) %>% 
  table()

}

# Impute groups separately -----------------------------------------------------
ta = Sys.time()

imputed_mice = lapply(df_split, function(input_df)
  mice(
    input_df, 
    m = number_imputations, 
    maxit = number_iterations, 
    method = meth,
    predictorMatrix = pred,
    ignore = input_df$rq5_exclude_pps
  )
)

tb = Sys.time()  # first run was 9.7 minutes

tb-ta  # 3.14 hours to do 24 imputations

lapply(imputed_mice, function(x) x$loggedEvents)
 
# Extract completed datasets from each group
completed_list = lapply(names(imputed_mice), function(group_name) {
  completed_data = complete(imputed_mice[[group_name]], action = "long") 
  completed_data$sexzyg = group_name  # Ensure group identifier is preserved
  return(completed_data)
})

# Combine all groups back together

df_rq5_imputed = do.call(rbind, completed_list) %>%
  arrange(.imp, randomfamid) %>%
  select(-.id) # .id is the rowname in each imputed dataset, created by complete() - not useful. 

df_rq5_imputed = df_rq5_imputed %>%
  select(
    .imp,
    sexzyg,
    random,
    twin,
    randomfamid,
    # randomtwinid,
    rq5_exclude_pps, 
    x3zygos,
    # all_of(rq5z),
    starts_with(rq5y_prefix)
  )

testthat::test_that("Imputed dataset preserves all original rows and families", {
  expect_equal(nrow(df_impute), nrow(filter(df_rq5_imputed, .imp == 1)))
  expect_true(all(df_impute$randomfamid %in% df_rq5_imputed$randomfamid))
})

df_rq5_imputed = df_rq5_imputed %>%
  select(-twin,-random) %>%
  pivot_longer(
    cols = starts_with(rq5y_prefix),
    names_to = c(".value", "random"),
    names_pattern = "(.+)([12])$"
  ) %>%
  rename_with(~ paste0(.x, "1"), .cols = starts_with(rq5y_prefix)) %>%
  mutate(
    randomtwinid2 = paste0(randomfamid, random),
    random = recode(random, `1` = 1, `2` = 0)
  )

testthat::test_that("check random variable has been correctly recoded",{
  df_test = df %>%
    select(randomfamid, random, pcg1) %>%
    rename(pcg1_nonimputed = "pcg1") %>%
    # filter(!is.na(pcg1)) %>%
    # arrange(randomfamid, random) %>%
    mutate(rid = paste0(randomfamid,"-",random))
  
  df_test_imputed = df_rq5_imputed %>% 
    filter(.imp ==1) %>%
    select(randomfamid, random, pcg1) %>%
    rename(pcg1_imputed = "pcg1") %>%
    # arrange(randomfamid, random) %>%
    mutate(rid = paste0(randomfamid,"-",random)) 
  
  df_test_join = full_join(df_test, df_test_imputed)
  
  n_mismatch = which(df_test_join$pcg1_nonimputed!=df_test_join$pcg1_imputed) %>% 
    length()
  
  testthat::expect_equal(0, n_mismatch)
})

# Verify the final dataset
# if (!identical(nrow(df),nrow(filter(df_rq5_imputed, .imp ==1)))) stop("error")

# Check for any remaining missing values
cat("\nMissing values in final dataset:\n")
print(sapply(df_rq5_imputed, function(x) sum(is.na(x))))

# Save the imputed dataset
saveRDS(df_rq5_imputed, file.path("data","df_rq5_imputed.Rds"))


