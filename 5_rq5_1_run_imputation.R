# Load things ------------------------------------------------------------------

source("0_load_data.R")

number_imputations = 100

number_iterations = 5

# Set up imputation  -------------------------------------------------------------------
df_impute = df %>%
  filter(twin == 1) %>%
  select(
    sexzyg,
    twin,
    randomfamid,
    # randomtwinid,
    rq5_exclude_pps, 
    x3zygos,
    all_of(rq5z),
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

# Set to NA participants on the exclude list (note that the ignore argument only tells mice to not use these participants in the imputation model)

df_rq5_imputed[which(df_rq5_imputed$rq5_exclude_pps),rq5y_12] = NA

# Convert dataset back to long:

df_rq5_imputed = df_rq5_imputed %>%
  select(
    .imp,
    sexzyg,
    twin,
    randomfamid,
    rq5_exclude_pps, 
    x3zygos,
    
    starts_with(rq5y_prefix)
    
  )

# Duplicate dataset into long version:

df_rq5_imputed = df_rq5_imputed %>%
  select(-twin) %>%
  pivot_longer(
    cols = starts_with(rq5y_prefix),
    names_to = c(".value", "twin"),
    names_pattern = "(.+)([12])$"
  ) %>%
  rename_with(~ paste0(.x, "1"), .cols = starts_with(rq5y_prefix)) %>%
  mutate(
    randomtwinid = paste0(randomfamid, twin)
  )


# Verify the final dataset
if (!identical(nrow(df),nrow(filter(df_rq5_imputed, .imp ==1)))) stop("error")

# Check for any remaining missing values
cat("\nMissing values in final dataset:\n")
print(sapply(df_rq5_imputed, function(x) sum(is.na(x))))

# Save the imputed dataset
saveRDS(df_rq5_imputed, file.path("data","df_rq5_imputed.Rds"))


