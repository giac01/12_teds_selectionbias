# Docker Container Used 

# bignardig/tidyverse451:v7 #(newer container than used for other analyses)

# Load Data --------------------------------------------------------------------

source("0_load_data.R")

library(OpenMx)
library(umx)

OpenMx::mxOption(NULL, "Default optimizer", "CSOLNP")


df1 = df %>%
  filter(twin == 1) %>%
  mutate(
    across(starts_with(rq1y_twin), ~ umx::umxFactor(.x))
  ) %>%
  select(starts_with(rq1y_twin), sexzyg)

# Fit ACE models ---------------------------------------------------------------

male_models = lapply(rq1y_twin, function(var)
umx::umxACE(
  selDVs = var,
  mzData = filter(df1, sexzyg == "MZ male"),
  dzData = filter(df1, sexzyg == "DZ male"), 
  sep = "",
  addCI = TRUE,
  intervals = TRUE
))

names(male_models) = rq1y_twin


female_models = lapply(rq1y_twin, function(var)
  umx::umxACE(
    selDVs = var,
    mzData = filter(df1, sexzyg == "MZ female"),
    dzData = filter(df1, sexzyg == "DZ female"), 
    sep = "", 
    addCI = TRUE,
    intervals = TRUE
  ))

names(female_models) = rq1y_twin


# Get point-estimates ----------------------------------------------------------

male_estimates = sapply(male_models, function(x)
  umxSummary(
    x, 
    digits = 8,
    CIs = FALSE
    )
  )

male_estimates = male_estimates %>%
  data.frame() %>%
  rownames_to_column(var = "par") %>%
  pivot_longer(cols = !starts_with("par")) %>%
  mutate(
    value = as.numeric(value)^2,
    sex   = "male"
  )

  
female_estimates = sapply(female_models, function(x)
  umxSummary(
    x, 
    digits = 8,
    CIs = FALSE
  )
)

female_estimates = female_estimates %>%
  data.frame() %>%
  rownames_to_column(var = "par") %>%
  pivot_longer(cols = !starts_with("par")) %>%
  mutate(
    value = as.numeric(value)^2,
    sex   = "female"
  )

estimates = rbind(male_estimates, female_estimates)


# Get confidence intervals  ----------------------------------------------------

get_ci = function(x, var_name = NA){
    
  ci = tryCatch({
      x = suppressMessages(umxSummary(x,  digits = 8, CI = TRUE))
    }, error = function(e) {
      return(NA)
    })
  
  if(!identical(NA, ci)) {
    ci = data.frame(
      variable = rownames(ci)[1],
      t(sapply(ci[1,], function(x) {
        parts = strsplit(gsub("\\[|\\]", "", x), " |, ")[[1]]
        c(est = as.numeric(parts[1])^2,
          lower = as.numeric(parts[2])^2,
          upper = as.numeric(parts[3])^2)
      }))) %>% rownames_to_column(var = "par")
  }
  
  if(identical(NA, ci)) {
    ci = data.frame(
      par = paste0(c("a","c","e"),"1"),
      variable  = rep(var_name, 3),
      est       = rep(NA, 3),
      lower     = rep(NA, 3),
      upper     = rep(NA, 3)
    )
      
  }
  
  return(ci)
  
}

male_ci = lapply(names(male_models), function(x) get_ci(male_models[[x]], var_name = x))
male_ci = bind_rows(male_ci)
male_ci$sex = "male"

female_ci = lapply(names(female_models), function(x) get_ci(female_models[[x]], var_name = x))
female_ci = bind_rows(female_ci)
female_ci$sex = "female"

confints = rbind(male_ci, female_ci)


# Export objects ---------------------------------------------------------------

saveRDS(estimates, file = file.path("results","1_ace_estimates.Rds"))
saveRDS(confints, file = file.path("results","1_ace_confints.Rds"))

# END --------------------------------------------------------------------------
# 
# 
# 
# 
# sapply(male_models, f)
# 
# male_models[[1]] %>% names()
# 
# 
# umxSummary(male_models[[12]],  digits = 7, CI = TRUE, std = TRUE)
# 
# 
# x = umxSummary(male_estimates[[1]],  digits = 7, CI = TRUE)
# 
# 
# x$a1
# 
# x$e1
# (male_estimates[[13]])
# 
# plot(male_estimates[[12]])
# 
# 
# 
# ace.model<-"
# A1=~ NA*P1 + c(a,a)*P1 
# A2=~ NA*P2 + c(a,a)*P2 
# C1 =~ NA*P1 + c(c,c)*P1
# C2 =~ NA*P2 + c(c,c)*P2
# # variances
# A1 ~~ 1*A1
# A2 ~~ 1*A2
# C1 ~~ 1*C1
# C2 ~~ 1*C2 
# P1~~c(e2,e2)*P1 
# P2~~c(e2,e2)*P2
# # covariances
# A1 ~~ c(1,.5)*A2 
# A1 ~~ 0*C1 + 0*C2 
# A2 ~~ 0*C1 + 0*C2 
# C1 ~~ c(1,1)*C2
# 
# # threshold fixed:
# P1 | 1*t1 
# P2 | 1*t1 
# "
# 
# df %>%
#   mutate(
#     P1 = zcdata1,
#     P2 = zcdata2
#   ) %>%
#   filter(sex1 == 1) %>%
#   select(P1, P2, sex)
# 
# ace.fit<-cfa(ace.model, data = dataset,group = "zyg",parameterization="theta",ordered=TRUE)
# 
# 
# 
# 

