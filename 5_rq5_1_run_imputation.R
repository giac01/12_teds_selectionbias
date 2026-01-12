# Run using docker container: bignardig/tidyverse451:v7 
# Run using commit: gjghg74565 (see commit message)
# Run date: 10-Jan-2025

# Load things ------------------------------------------------------------------

source("0_load_data.R")

number_imputations = 200                                                          # 16  (50 it) took 20 minutes on 8 cores, 200 (50 it) took 2.8 hours on 48 cores, 48 took 1.8 hours on 8 cores, 200 took 7.8 hours on 8 cores
number_iterations = 50 
n_workers = 14                                                                  # Number of parallel jobs to run (adjust based on available cores)

# Set up imputation  -------------------------------------------------------------------
df_impute = df %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%                           # Exclude fams with one sub in the study
  filter(!(randomfamid %in% rq5_exclude_fams)) %>%                              # Exclude fams with 0 data on rq5y variables (key outcomes)
  filter(!(randomfamid %in% rq5_exclude_fams_2)) %>%                            # Exclude fams with less than 30% data on all imputed data (excluding baseline data)
  filter(random == 1) %>%
  select(
    sexzyg,
    random,
    twin,
    randomfamid,
    # randomtwinid,
    x3zygos,
    all_of(rq5z),
    starts_with(rq5y_prefix)
  ) %>%
  select(
    -contains("sdqext"),
  )

df_split = split(df_impute, df_impute$sexzyg)

sapply(df_split, nrow)

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
    # "rq5_exclude_pps",
    "x3zygos"
  )
  
  # Set all excluded variables to 0 in predictor matrix
  pred[exclude_vars, ] = 0
  pred[, exclude_vars] = 0
  
}

df_impute %>%
  select(-any_of(exclude_vars)) %>%
  colnames() %>%
  saveRDS(file.path("results","5_1_imputation_variables.Rds"))

# miceinit$loggedEvents

if (FALSE){
table(df$aethnicc)

df_impute %>%
  filter(sexzyg=="MZ male") %>%
  select(pcg1, cohort) %>%
  mutate(across(1, is.na)) %>% 
  table()

}

# Impute groups separately -----------------------------------------------------
# Using task-level parallelization for maximum efficiency
# Each imputation runs as a separate task to fully utilize all cores

tasks = expand.grid(
  group = names(df_split),
  imputation = 1:number_imputations,
  stringsAsFactors = FALSE
)

## Run all imputations in parallel ---------------------------------------------

plan(multicore, workers = n_workers)

ta = Sys.time()

all_imputations = future_lapply(1:nrow(tasks), function(i) {
  group = tasks$group[i]

  # Run single imputation for this task
  mice(
    df_split[[group]],
    m               = 1,  # Single imputation per task
    maxit           = number_iterations,
    method          = meth,
    predictorMatrix = pred,
    printFlag       = FALSE
  )
}, future.seed = 1)

tb = Sys.time()

tb - ta

plan(sequential)

saveRDS(all_imputations, file = file.path("results","5_1_all_imputations.Rds"))

## Combine single imputations back by group ------------------------------------

imputed_mice = list()

for (group_name in names(df_split)) {
  # Create list to store all imputations for this group
  imp_list = list()

  for (k in 1:number_imputations) {
    # Get the imputation for this group-imputation combination
    task_index = which((tasks$group == group_name) & (tasks$imputation == k))

    # Extract completed dataset
    imp_list[[k]] = mice::complete(all_imputations[[task_index]], action = 1)
    
    imp_list[[k]] = imp_list[[k]] %>%
                    select(any_of(c("sexzyg","x3zygos","random","twin","randomfamid","cohort",rq5y_12)), ends_with(c("sdqcont1","sdqcont2","sdqhypt1","sdqhypt2")))
    
    imp_list[[k]]$.imp   = k
    # imp_list[[k]]$sexzyg = group_name # not necessary as the variable is already here
    
  }

  # Store list of imputations for this group
  imputed_mice[[group_name]] = do.call(rbind.data.frame,imp_list)
}

# Combine all groups back together

df_rq5_imputed = do.call(rbind.data.frame, imputed_mice) %>%
  arrange(.imp, randomfamid) 

# Create Externalising scores & remove auxillary variables

df_rq5_imputed = df_rq5_imputed %>%
  mutate(
    lsdqext1 = as.numeric(lcsdqcont1   + lcsdqhypt1),  
    psdqext1 = as.numeric(pcbhsdqcont1 + pcbhsdqhypt1),    
    usdqext1 = as.numeric(u1csdqcont1  + u1csdqhypt1),
    zsdqext1 = as.numeric(zmhsdqcont1  + zmhsdqhypt1),
    
    lsdqext2 = as.numeric(lcsdqcont2   + lcsdqhypt2),       
    psdqext2 = as.numeric(pcbhsdqcont2 + pcbhsdqhypt2),     
    usdqext2 = as.numeric(u1csdqcont2  + u1csdqhypt2),
    zsdqext2 = as.numeric(zmhsdqcont2  + zmhsdqhypt2)
  ) %>%
  select(
    .imp,
    sexzyg,
    random,
    twin,
    randomfamid,
    # randomtwinid,
    # rq5_exclude_pps,
    x3zygos,
    # all_of(rq5z),
    starts_with(rq5y_prefix)
  ) 

# colnames(df_rq5_imputed)
# df_rq5_imputed$zsdqext2

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

testthat::test_that("Imputed dataset preserves all original rows and families", {
  
  expect_true(all(rq5y %in% colnames(df_rq5_imputed)))
})


# Verify the final dataset
# if (!identical(nrow(df),nrow(filter(df_rq5_imputed, .imp ==1)))) stop("error")

# Check for any remaining missing values
cat("\nMissing values in final dataset:\n")
print(sapply(df_rq5_imputed, function(x) sum(is.na(x))))

# Save the imputed dataset
saveRDS(df_rq5_imputed, file.path("data","df_rq5_imputed.Rds"))


