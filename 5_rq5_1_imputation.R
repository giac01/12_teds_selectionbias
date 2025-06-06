# Load things ------------------------------------------------------------------

source("0_load_data.R")

# Descriptive Data -------------------------------------------------------------

# ggmice::plot_pattern(df_rq5)
# 
# gbtoolbox::plot_pairwise_missing(df_rq5)


gbtoolbox::plot_pairwise_missing(df_rq5[1:150],   divisor = 1000, textadjust = .9)
gbtoolbox::plot_pairwise_missing(df_rq5[151:300], divisor = 1000, textadjust = .9)
gbtoolbox::plot_pairwise_missing(df_rq5[301:375], divisor = 1000, textadjust = .9)


df %>%
  select(rq5y) %>%
  gbtoolbox::plot_pairwise_missing(., divisor = 1000, textadjust = .9)


.variable_check = function(x, name = NULL){
  if (!is.null(name)) {
    var_name <- name
  } else {
    var_name <- deparse(substitute(x))
  }
  cat("Variable:", var_name, "\n")
  cat("Class:", class(x), "\n")
  cat("Label:", attr(x,"label"), "\n")
  print(table(x, useNA = "always"))
  cat("\n")
  return(invisible(NULL))
}


sink("variable_check_results.txt")

invisible(
  df %>%
    select(any_of(rq5z)) %>%
    mutate_if(is.numeric, ~ round(.x, 1)) %>%
    select(150:250) %>%
    purrr::imap(~.variable_check(.x, .y))
)

# Stop redirecting output
sink()


df %>%
  select(any_of(rq5z)) %>%
  mutate_if(is.numeric, ~ round(.x, 1)) %>%
  apply(.,2, function(x) length(unique(x))) %>% table()

## Check what percentage of data is missing ------------------------------------

df %>%
  select(any_of(rq5z)) %>%
  apply(.,2,function(x) length(which(!is.na(x)))/length(x)) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  `colnames<-`(c("var","percent_notmissing")) %>%
  mutate(var_label = factor(var, levels = var, labels = var_to_label(var))) %>%
  ggplot(aes(x = percent_notmissing, y = var_label)) + 
  geom_col() +
  geom_vline(aes(xintercept=.2)) +
  theme_bw()

ggsave(file.path("plots", "5_1_missing_data_frequency.pdf"), height = 25, width = 12)




  
## Create descriptive table ----------------------------------------------------

source("0_lists_of_variables.R")

# List of all variables for imputation
df_rq5 = df %>%
  select(any_of(rq5z))

# Descriptive information on each imputed variable
impute_df = data.frame(
  var   = colnames(df_rq5),
  label = as.character(sapply(df_rq5, function(x) attr(x, "label"))),
  class = as.character(sapply(df_rq5, function(x) class(x))),
  perc_not_missing = as.numeric(sapply(df_rq5, function(x) round(length(which(!is.na(x)))/length(x)*100))),
  sd    = round(as.numeric(sapply(df_rq5, function(x) sd(as.numeric(x), na.rm = TRUE) )),2),
  distinct_categories = as.numeric(sapply(df_rq5, function(x) length(na.omit(unique(x)))))
)

impute_df <- impute_df %>%
  mutate(variable_year = case_when(
    str_starts(var, "a") ~ "Year 1 (1st Contact)",
    str_starts(var, "b") ~ "Year 2",
    str_starts(var, "c") ~ "Year 3",
    str_starts(var, "d") ~ "Year 4",
    str_starts(var, "g") ~ "Year 7",
    str_starts(var, "h") ~ "Year 8",
    str_starts(var, "i") ~ "Year 9",
    str_starts(var, "j") ~ "Year 10",
    str_starts(var, "l") ~ "Year 12",
    str_starts(var, "n") ~ "Year 14",
    str_starts(var, "p") ~ "Year 16",
    str_starts(var, "r") ~ "Year 18",
    str_starts(var, "u") ~ "Year 21",
    str_starts(var, "z") ~ "Year 26",
    TRUE ~ "Other"
  ))


# THERE IS A BIT OF A DISCONNECT HERE REGARDING PARTICIPATION RATES RELATIVE TO THE "DATA PRESENT" VARIABLES I HAVE BEEN GIVEN. 

impute_df %>%
  arrange(perc_not_missing) %>%
  knitr::kable() %>%
  print(n=10000)


sapply(df_rq1y, function(x) length(which(x==1))/length(which(x>=0)))

# remove 466 participants with no data? 

# variables to remove:  aalgzyg aethnic 


df$lestcon1
                                   
                                
# Imputation -------------------------------------------------------------------
df_rq5 = df %>%
  # select(sexzyg, all_of(rq1x), any_of(rq5y))
select(sexzyg, any_of(rq5y))

df_rq5_split = split(df_rq5, df_rq5$sexzyg)

# Set up predictor matrix

miceinit = mice(df_rq5, method = "pmm", m = 1, maxit = 0)

df_rq5_imputed = lapply(df_rq5_split, function(df)
  mice(df, method = "pmm", m = 1, maxit = 5)
  )


df_rq5_imputed[[1]]$loggedEvents



library(miceadds)





