# Load things ------------------------------------------------------------------

source("0_load_data.R")

number_bootstraps = 10
 
number_iterations = 5
                                
# Set up imputation  -------------------------------------------------------------------
df_impute = df %>%
  # select(sexzyg, all_of(rq1x), any_of(rq5y))
  select(sexzyg,randomfamid,randomtwinid,any_of(rq5y))

df_split = split(df_impute, df_impute$sexzyg)

# Set up predictor matrix (not actually helpful currently - could remove)

if (TRUE){
miceinit = mice(df_impute, method = "pmm", m = 1, maxit = 0)

meth = miceinit$method
pred = miceinit$predictorMatrix

exclude_vars = c("sexzyg", "randomtwinid", "randomfamid")

# Set all excluded variables to 0 in predictor matrix
pred[exclude_vars, ] = 0
pred[, exclude_vars] = 0

}

# Impute groups separately -----------------------------------------------------

imputed_mice = lapply(df_split, function(df)
  mice(
    df, 
    m = number_bootstraps, 
    maxit = number_iterations, 
    method = meth,
    predictorMatrix = pred
    )
  )

lapply(imputed_mice, function(x) x$loggedEvents)

# Extract completed datasets from each group
completed_list = lapply(names(imputed_mice), function(group_name) {
  completed_data = complete(imputed_mice[[group_name]], action = "long") 
  completed_data$sexzyg = group_name  # Ensure group identifier is preserved
  return(completed_data)
})

# Combine all groups back together
df_rq5_final = do.call(rbind, completed_list) %>%
  arrange(.imp, randomtwinid) %>%
  select(-.id) # .id is the rowname in each imputed dataset, created by complete() - not useful. 

# Verify the final dataset
if (!identical(nrow(df),nrow(filter(df_rq5_final, .imp ==1)))) stop("error")

# Check for any remaining missing values
cat("\nMissing values in final dataset:\n")
print(sapply(df_rq5_final, function(x) sum(is.na(x))))

# Save the final imputed dataset
saveRDS(df_rq5_final, file.path("data","df_rq5_final.Rds"))





