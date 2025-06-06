# Load things ------------------------------------------------------------------

source("0_load_data.R")
                                
# Imputation -------------------------------------------------------------------
df_rq5 = df %>%
  # select(sexzyg, all_of(rq1x), any_of(rq5y))
select(sexzyg, any_of(rq5y))

df_rq5_split = split(df_rq5, df_rq5$sexzyg)

# Set up predictor matrix (not actually helpful currently - could remove)

if (FALSE){
miceinit = mice(df_rq5, method = "pmm", m = 1, maxit = 0)

meth <- miceinit$method
pred <- miceinit$predictorMatrix

pred[, "sexzyg"] <- 0
pred["sexzyg", ] <- 0
}

# Impute groups separately 

df_rq5_imputed = lapply(df_rq5_split, function(df)
  mice(df, method = "pmm", m = 1, maxit = 5)
  )

# Check logged events for warnings
lapply(df_rq5_imputed, function(x) x$loggedEvents)

# Extract completed datasets from each group
df_rq5_completed_list <- lapply(names(df_rq5_imputed), function(group_name) {
  completed_data <- complete(df_rq5_imputed[[group_name]])
  completed_data$sexzyg <- group_name  # Ensure group identifier is preserved
  return(completed_data)
})

# Combine all groups back together
df_rq5_final <- do.call(rbind, df_rq5_completed_list)

# Reorder by original row indices if needed
# (This assumes the original df had row names or an ID column)
df_rq5_final <- df_rq5_final[order(as.numeric(rownames(df_rq5_final))), ]


# Verify the final dataset
if (!identical(nrow(df),nrow(df_rq5_final))) stop("error")

# Check for any remaining missing values
cat("\nMissing values in final dataset:\n")
print(sapply(df_rq5_final, function(x) sum(is.na(x))))

# Save the final imputed dataset
saveRDS(df_rq5_final, file.path("data","df_rq5_final.Rds"))





