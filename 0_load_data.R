# Load Libraries and Data ------------------------------------------------------

rm(list=ls())

library(patchwork)
library(testthat)
library(MASS)
library(mice)
library(pROC)
library(performance)
library(DHARMa) # needed for performance::checkmodel function
library(see)    # needed for performance::see function
library(future)
library(future.apply)
library(tidyverse)
library(dplyr)
library(distrEx)
library(Ternary)

conflicted::conflict_prefer_all("tidyverse")

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

source("0_functions.R")
source("0_lists_of_variables.R")


df0 = haven::read_sav(file.path("data","732 GB FINAL 20250612.sav"))

df0$pollution1998pca = 0

attr(df0[["pollution1998pca"]], "label") = "Principal Component of 1998 pollution variables"

df = df0

# Apply participant filter -----------------------------------------------------

df = df %>%
  filter(exclude1 == 0) %>%
  mutate(sexzyg = droplevels(haven::as_factor(sexzyg)))

# Define Variable Sets ---------------------------------------------------------

rq1x = c("amumagetw","adadagetw","aadults","aalgzyg","amedtot",
         # "afaclas",
         # "afajob", "afasoc", 
         "afasoc2",
         # "afaspq", "afawork", 
         "afahqual",
         
         # "amoclas", 
         # "amojob", "amosoc", 
         "amosoc2",
         "amohqual", 
         # "amospq", "amowork",
         "atwmed1", 
         # Mising acorn and pollution data
         "aethnicc", "alang", "anoldsib", "anyngsib",
         "atwclub",
         "alookels",
         "asmoke",
         "adrink",
         "astress",
         "cens01pop98density",
         "pollution1998pca"
)

rq1y = c("btwoyear", "cthreeyr", "dfouryr", "gsevenyr", "gpdata", "heightyr", "ipdata", "jtenyear", "ltwelvyr", "n14year", "p16year", "rcqdata", "uteds21data", "zmhdata")

rq1y = c("btwoyear", "cthreeyr", "dfouryr", "gsevenyr",           "heightyr",           "jtenyear", "ltwelvyr", "n14year", "p16year", "rcqdata", "uteds21data", "zmhdata")

rq1y_short = c(                  "dfouryr", "gsevenyr",           "heightyr",                       "ltwelvyr", "n14year", "p16year", "rcqdata", "uteds21data", "zmhdata")

# rq2y = c("brawg1", "badparn1","breparc1", "bparca1", 
#          "bvocab1", "bgramma1","bsdqcbeht1","bsdqccont1",
#          "bsdqcemot1", "bsdqchypt1", "bsdqcpert1", "bsdqcprot1"
#          )

rq2y_prefix  = c(
  "bvocab", "bgramma","badparn", "breparc", 
  "bsdqccont", "bsdqcemot", "bsdqchypt", "bsdqcpert", "bsdqcprot"
)

rq2y         = paste0(rq2y_prefix, "1")

rq2y_all     = c(paste0(rq2y_prefix, "1"),paste0(rq2y_prefix, "2"))

# Research Question 5 

# Available outcomes at 12, 14, 16, 18, 21 & 26
#       l:12          n:14          p:16 / gcses        u:21                      z:26
 
hyp = c()

gca = c("lcg1",       "ncg1",       "pcg1",             "ucgt1") # GCA scores: ages 12, 14, 16, 21

mfq = c("lcmfqt1",                  "pcbhmfqt1",        "u1cmfqt1",               "zmhmfqt1") # MFQ scores

# MAYBE REMOVE GAD - AS ITS MOSTLY MISSING! 
gad = c(                                                "u2cganxt1",              "zmhganxt1") # GAD-D Anxiety Scores

edu = c(              "npks3tall1", "pcexgcsecoregrdm1", "u1chqualp1", "zmhhqual1")  # removed UCAS

hyp = c("lcsdqhypt1",               "pcbhsdqhypt1",     "u1csdqhypt1",            "zmhsdqhypt1") # SDQ Hyperactivity scores

con = c("lcsdqcont1",               "pcbhsdqcont1",     "u1csdqcont1",            "zmhsdqcont1")

rq5y = c(
  gca,
  edu,
  mfq
  )

rq5y_prefix = str_remove(rq5y, "1$")

rq5y_12 = paste0(rep(rq5y_prefix, each = 2), c("1", "2"))

# gad %in% rq5z
# 
# df %>%
#   select(all_of(rq5y))

# Variable fixes and formatting ------------------------------------------------

df$gsevenyr[is.na(df$gsevenyr)] = 0

df$heightyr[is.na(df$heightyr)] = 0

df$aadults   = haven::as_factor(df$aadults, levels = "label") %>%
  droplevels( "unknown or other") 
df$asingle   = (as.character(df$aadults)=="single parent") %>% as.numeric()
df$asingle[which(df$aadults=="biological mother with missing partner details")] = NA
df$aalgzyg   = haven::as_factor(df$aalgzyg)
df$afaclas   = haven::as_factor(df$afaclas)
df$afajob    = haven::as_factor(df$afajob)
df$afasoc    = haven::as_factor(df$afasoc)
df$afaspq    = haven::as_factor(df$afaspq)
df$afawork   = haven::as_factor(df$afawork)
df$afahqual  = haven::as_factor(df$afahqual)
df$amoclas   = haven::as_factor(df$amoclas)
df$amojob    = haven::as_factor(df$amojob)

df$amohqualn = haven::as_factor(df$amohqual, levels = "values") %>% as.character() %>% as.numeric() # Numeric version of amohqual
df$amohqual  = haven::as_factor(df$amohqual)
df$amosoc    = haven::as_factor(df$amosoc)
df$amospq    = haven::as_factor(df$amospq)
df$amowork   = haven::as_factor(df$amowork)
df$aethnicc  = haven::as_factor(df$aethnicc)
df$alang     = haven::as_factor(df$alang)


df$anoldsibn  = haven::as_factor(df$anoldsib, levels = "values") %>% as.character() %>% as.numeric()  # numeric version
df$anyngsibn  = haven::as_factor(df$anyngsib, levels = "values") %>% as.character() %>% as.numeric() # numeric version
df$asibsn     = df$anoldsibn + df$anyngsibn 

df$anoldsib  = haven::as_factor(df$anoldsib)  
df$anyngsib  = haven::as_factor(df$anyngsib)  

df$atwclub   = haven::as_factor(df$atwclub)
df$alookels  = haven::as_factor(df$alookels)
df$asmoke    = haven::as_factor(df$asmoke)
df$adrink    = haven::as_factor(df$adrink)
df$astress   = haven::as_factor(df$astress)
df$agenpro1  = haven::as_factor(df$agenpro1)
df$agenpro2  = haven::as_factor(df$agenpro2)
df$aonsby    = haven::as_factor(df$aonsby)
df$cohort    = haven::as_factor(df$cohort)
df$cohort_by = paste0(as.numeric(df$cohort), df$aonsby)

df$aadults   = set_most_frequent_ref(df$aadults)
df$aalgzyg   = set_most_frequent_ref(df$aalgzyg)
df$aethnicc  = set_most_frequent_ref(df$aethnicc)
df$afahqual  = set_most_frequent_ref(df$afahqual)

df$u1chqualp1 = as.numeric(df$u1chqualp1)
df$u1chqualp2 = as.numeric(df$u1chqualp2)

df$zmhhqual1   = as.numeric(df$zmhhqual1)
df$zmhhqual2   = as.numeric(df$zmhhqual2)
df$zmhempst1   = haven::as_factor(df$zmhempst1)
df$zmhempst2   = haven::as_factor(df$zmhempst2)
df$zmhempinc1  = as.numeric(df$zmhempinc1)
df$zmhempinc2  = as.numeric(df$zmhempinc2)

df$zmhneet1    = haven::as_factor(df$zmhneet1)
df$zmhneet2    = haven::as_factor(df$zmhneet2)

df$sexzyg      = haven::as_factor(df$sexzyg)
df$x3zygos     = haven::as_factor(df$x3zygos)

df$lsdqext = df$lcsdqcont1   + df$lcsdqhypt1       # Age 12 SDQ Externalising Score
df$psdqext = df$pcbhsdqcont1 + df$pcbhsdqhypt1     # Age 16 SDQ Externalising Score
df$usdqext = df$u1csdqcont1  + df$u1csdqhypt1
df$zsdqext = df$zmhsdqcont1  + df$zmhsdqhypt1

## Recode job variables --------------------------------------------------------

# Mother (female parent) job classification
df$amosoc2 = as.character(df$amosoc)
df$amosoc2[df$amojob=="no"] = "no job"
df$amosoc2[df$amojob=="caring for children at home"] = "caring for children at home"
df$amosoc2 = set_most_frequent_ref(as.factor(df$amosoc2))

# table(df$amojob, df$amosoc2, useNA = "always")

attr(df$amosoc2, "label") = "Mother SOC employment level (1st Contact), 1-9"

# Father (male parent) job classification
df$afasoc2 = as.character(df$afasoc)
df$afasoc2[df$afajob=="no"] = "no job"
df$afasoc2[df$afajob=="caring for children at home"] = "caring for children at home"
# df$afasoc2[df$aadults=="single parent" & is.na(df$afasoc2)] = "single parent".  #  Probably not a good idea to create single parent group here! 
df$afasoc2 = set_most_frequent_ref(as.factor(df$afasoc2))

attr(df$afasoc2, "label") = "Father SOC employment level (1st Contact), 1-9"

# Father (male parent) qualification classification

## Not sure this makes sense - maybe do not use? 

# df$afahqual2 = as.character(df$afahqual)
# df$afahqual2[df$aadults=="single parent" & is.na(df$afahqual2)] = "single parent"

# table(df$afajob, df$afasoc2, useNA = "always")
# table(df$aadults, df$afasoc2, useNA = "always")
# table(df$aadults, useNA = "always")
# df_singleparent = df0 %>%
#   mutate(aadults = haven::as_factor(aadults)) %>%
#   filter(aadults == "single parent")
# table(df_singleparent$afajob, useNA = "always")
# table(df_singleparent$amojob, useNA = "always")

# Create other variables -------------------------------------------------------


x = df %>%
  select(c(pollution1998pm10, pollution1998pm25, pollution1998no2, pollution1998nox, pollution1998benzene,
           pollution1998ozone)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.data.frame() %>%
  psych::pca(nfactors = 1, missing = FALSE)

df$pollution1998pca = as.numeric(x$scores)

# Reset attributes -------------------------------------------------------------

all_vars = colnames(df)

for (var in all_vars) {
  if (!is.null(attr(df0[[var]], "label"))){
  attr(df[[var]], "label") <- attr(df0[[var]], "label") }
}

for (var in all_vars) {
  if (!is.null(attr(df0[[var]], "labels"))){
    attr(df[[var]], "labels") <- attr(df0[[var]], "labels") }
}

# Creating seperate dataframes -------------------------------------------------

df_colnames = colnames(df)

df_labels   = sapply(1:ncol(df), function(i) attr(df[,i, drop = TRUE], "label"))

df_rq1 = df %>%
  filter(twin == 1) %>%
  filter(acontact == 1) %>%
  select(all_of(c(rq1x, rq1y)))

df_rq1x = df_rq1 %>% 
  select(all_of(rq1x)) 

df_rq1y = df_rq1 %>% 
  select(all_of(rq1y)) 

df_rq5 = df %>%
  select(any_of(rq5z))

# Create Labels ----------------------------------------------------------------

rq1x_labels = rq1x %>% var_to_label()

rq1x_labels_clean = c(
  "Mother age at birth",
  "Father age at birth",
  "Household type [aadults]",
  "Zygosity",
  "Mother medical risk",
  "Father employment level",
  "Father education level",
  "Mother employment level",
  "Mother education level",
  "Twin medical risk",
  "Ethnic origin",
  "Language at home",
  "Older siblings",
  "Younger siblings",
  "Twins club member",
  "Childcare by others",
  "Smoking in pregnancy",
  "Alcohol in pregnancy",
  "Severe stress in pregnancy",
  "Population density",
  "Pollution index"
)


rq1y_labels = rq1y %>% var_to_label()

rq1y_labels_clean = clean_rq1y_label(rq1y_labels)

rq2y_labels = rq2y %>% var_to_label()

rq2y_labels_short = c(
  "Vocabulary",
  "Grammar",
  "Parent-admin Parca",
  "Parent-report Parca",
  "Conduct problems", 
  "Emotional problems",
  "Hyperactivity",
  "Peer problems",
  "Prosocial behavior"
)

rq5y_labels_short = c(
  "Yr12: Cognitive ability",
  "Yr14: Cognitive ability", 
  "Yr16: Cognitive ability",
  "Yr21: G-game total score",
  "Yr14: KS3 academic achievement",
  "Yr16: GCSE core subjects grade",
  "Yr21: Highest qualification",
  "Yr26: Highest qualification",
  "Yr12: Depression (MFQ)",
  "Yr16: Depression (MFQ)", 
  "Yr21: Depression (MFQ)",
  "Yr26: Depression (MFQ)"
)

mfq_labels = df %>% 
  select(all_of(mfq)) %>%
  sapply(., function(x) attr(x, "label"))

# Define Participant Exclusion Lists -------------------------------------------

#Work in progress - will update in next commit

df = df %>%
  mutate(
    rq5_missing_percent = rowSums(is.na(select(.,starts_with(rq5y_prefix)))) / length(rq5y)*2,
    rq5_nonmissing_n = rowSums(!is.na(select(., starts_with(rq5y_prefix)))),
    rq5_exclude_pps = rq5_nonmissing_n == 0 
  )

# Participants with little (or no) data on the rq5y variables before imputation. (EXCLUSION IS BASED ON TWIN PAIR!)
rq5_exclude_fams = df %>%
  filter(rq5_exclude_pps) %>%
  pull(randomfamid) %>%
  unique()

# Participants wiht little data on all imputation variables 

# rq5z_1 <- rq5z[!str_detect(rq5z, "2$")] # keep only twin1 variable versions for "long" data analysis 

rq5_percent_complete = df %>%
  # filter(twin == 1) %>%
  select(
    all_of(rq5z), starts_with(rq5y_prefix)
  ) %>%
  select(-starts_with("a")) %>%
  apply(.,1, function(x) length(which(!is.na(x)))/length(x)) 

rq5_exclude_fams_2 = df %>%
  filter(rq5_percent_complete < .30) %>%
  pull(randomfamid) %>%
  unique()

# df %>%
#   filter(!(randomfamid %in% exclude_fams_onesib)) %>%
#   filter(!(randomfamid %in% rq5_exclude_fams)) %>%
#   filter(!(randomfamid %in% rq5_exclude_fams_2)) %>%
#   filter(random == 1) %>%
#   nrow()

## We want to exclude families with with only one sibling in the study following application of the exclusion criteria
exclude_fams_onesib = df %>%
  count(randomfamid) %>% 
  filter(n == 1) %>%
  pull(randomfamid) %>%
  as.character()



