# Run using docker container: bignardig/tidyverse451:v8
# Run using commit: 2321sdfaadfsdf (see commit message)
# Run date: 12-06-2026

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Load data --------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

rm(list=ls())

source("0_load_data.R")
imputed_datasets      = readRDS(file = file.path("results","8_1_imputed_mice.Rds"))

range_participation_outcomes = 6:9
n_workers                    = 16                                                # Number of parallel jobs to run (number of cores)]
number_bootstraps_per_impute = 10000/250                                         # 1.6 hours

rq1y_twin                         = rq1y_twin[range_participation_outcomes]
rq1y_twin1                        = rq1y_twin1[range_participation_outcomes]
rq1y_twin2                        = rq1y_twin2[range_participation_outcomes]
rq1y_twin_labels                  = rq1y_twin_labels[range_participation_outcomes]
rq1y_twin_labels_clean            = rq1y_twin_labels_clean[range_participation_outcomes]
rq1y_twin_labels_clean_extrashort = rq1y_twin_labels_clean_extrashort[range_participation_outcomes]

cbind.data.frame(rq1y_twin1, rq1y_twin_labels)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Participant Exclusions -------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

a = nrow(df)

df = df %>%
  filter(!(randomfamid %in% exclude_fams_rq1x)) %>%
  filter(!(randomfamid %in% exclude_fams_onesib)) %>%
  filter(!(randomfamid %in% exclude_fams_rq6y)) %>%
  filter(random == 1) 

cat("Excluded", a-nrow(df), "rows ")

rm(a)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Prepare datasets -------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
imputed_datasets_flat = unlist(imputed_datasets, recursive = FALSE)

original_dataset = df %>%
  select("randomfamid", "random", "sexzyg", starts_with(rq6y_prefix)) %>%
  pivot_longer(
    cols = matches("\\d$"),
    names_to = c(".value", "twin_num"),
    names_pattern = "(.+)(\\d)$",
    names_transform = list(twin_num = as.integer)  # Convert to integer
  )

imputed_datasets_flat2 = lapply(imputed_datasets_flat, function(x) 
  x %>%
    select("randomfamid", "random", "sexzyg", starts_with(rq6y_prefix)) %>%
    pivot_longer(
      cols = matches("\\d$"),
      names_to = c(".value", "twin_num"),
      names_pattern = "(.+)(\\d)$",
      names_transform = list(twin_num = as.integer)  # Convert to integer
    )
  )

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Data checks ------------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# x = imputed_datasets_flat[[1]] %>% select("randomfamid", "twin", "random","x3zygos", "sexzyg", starts_with(rq6y_prefix)) %>% as_tibble()
# y = df %>% select("randomfamid", "twin", "random","x3zygos", "sexzyg", starts_with(rq6y_prefix)) %>% as_tibble()

x = original_dataset
y = imputed_datasets_flat2[[1]]

testthat::test_that("nrows of compared datasets match",{
  testthat::expect_equal(nrow(x),                nrow(y))
  testthat::expect_true(all(y$randomfamid %in%   x$randomfamid))
  testthat::expect_true(all(x$randomfamid %in%   y$randomfamid))
  testthat::expect_true(all(rq6y_prefix   %in%   colnames(x)))
  testthat::expect_true(all.equal(x$randomfamid, y$randomfamid, check.attributes = FALSE))
  testthat::expect_true(all.equal(x$random,      y$random, check.attributes = FALSE))
})

# Check that non-attritioned data matches from original to imputed datasets

x = original_dataset$lcmfqt
y = imputed_datasets_flat2[[1]]$lcmfqt
attritioned = df %>%                                                                # this step attritions the variables based on whether they took part at timepoint u 
            select(u1cdata1, u1cdata2) %>%
            pivot_longer(
              cols = matches("\\d$"),
              names_to = c(".value", "twin_num"),
              names_pattern = "(.+)(\\d)$",
              names_transform = list(twin_num = as.integer)  # Convert to integer
            ) %>% 
            pull(u1cdata) == 0
names(imputed_datasets_flat2)[1]
x[attritioned] = NA
y[attritioned] = NA

testthat::test_that("Check that non-attritioned data matches from original to imputed datasets",{
  testthat::expect_true(all.equal(x[!attritioned], y[!attritioned], check.attributes = FALSE))
})

if (FALSE){
  # Imputed data should mostly vary from the original
  
  data.frame(x,y) %>% 
    filter(attritioned)
  
  table(x[attritioned] == y[attritioned])
  
}

rm(x,y,attritioned)

#  attempting another way at the test

x = original_dataset

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Run Analysis -----------------------------------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

rm(imputed_datasets, imputed_datasets_flat, df0, df, x, y, missing, rq5_percent_complete)
gc()

plan(multicore, workers = n_workers)

ta = Sys.time()

boot_compare_results = future_lapply(imputed_datasets_flat2, function(df1_input) {
  .boot_compare_df(
    df1  = df1_input,
    df2  = original_dataset,
    B    = number_bootstraps_per_impute,
    vars = rq6y_prefix                                                          # Note that the pivot_longer steps removed the 1/2 at the end of the variable name...
  )
}, 
future.seed = 1,
future.packages = c("umx","OpenMx")
)  

tb = Sys.time()
print(tb - ta)

plan(sequential)

# names(boot_compare_results) = paste("inp",1:length(imputed_datasets))

saveRDS(boot_compare_results, file.path("results", "8_rq6_boot_compare_results.Rds"))
