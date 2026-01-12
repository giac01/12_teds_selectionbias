# Load Libraries and Data ------------------------------------------------------

rm(list=ls(all.names = TRUE))

library(marginaleffects)
library(ggmice)
library(ggrepel)
library(gbtoolbox)  # devtools::install_github("giac01/gbtoolbox")
library(OpenMx)
library(umx)
library(ggh4x)
library(gt)
library(weights)
library(Hmisc)
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

df0 = haven::read_sav(file.path("data","732 GB FINAL 20250714.sav"))

original_colnames = colnames(df0)

ncores_use = 8

## Add empty variables into df0 to allow label look-up by var_to_label() -------

df0$pollution1998pca = 0
df0$amohqualn        = 0
df0$amohqualn1       = 0
df0$amohqualn2       = 0
df0$asingle          = 0
df0$anoldsibn        = 0
df0$anyngsibn        = 0
df0$lsdqext1         = 0
df0$psdqext1         = 0
df0$usdqext1         = 0 
df0$zsdqext1         = 0

attr(df0[["pollution1998pca"]], "label") = "Principal Component of 1998 pollution variables"
attr(df0[["amohqualn"]],        "label") = "Maternal Education (formatted as numeric)"
attr(df0[["amohqualn1"]],       "label") = "Maternal Education (formatted as numeric variable)"
attr(df0[["amohqualn2"]],       "label") = "Maternal Education (formatted as numeric variable)"
attr(df0[["asingle"]],          "label") = "Single Parent"
attr(df0[["anoldsibn"]],        "label") = "Number of older siblings (formatted as numeric variable)"
attr(df0[["anyngsibn"]],        "label") = "Number of younger siblings (formatted as numeric variable)"
attr(df0[["lsdqext1"]], "label") = "SDQ Externalising scale at 12"
attr(df0[["psdqext1"]], "label") = "SDQ Externalising scale at 16"
attr(df0[["usdqext1"]], "label") = "SDQ Externalising scale at 21"
attr(df0[["zsdqext1"]], "label") = "SDQ Externalising scale at 26"

df = df0

## Apply global participant filter (exlcude1) ----------------------------------

df = df %>%
  filter(exclude1 == 0) 

# Define Variable Sets ---------------------------------------------------------

rq1x = c(
        "sex1",
        "amumagetw",
         "adadagetw",
         "asingle",
         "zygos",
         "amedtot",
         # "afaclas",
         # "afajob", 
         # "afasoc", 
         "afasoc2",
         # "afaspq", 
         # "afawork", 
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
         # "cens01pop98density",                                                # This has a lot of missing data, for some reason....
         "pollution1998pca"
)

# Family-level participation outcomes (DEPRECIATED / NOT BEING USED ANYMORE)

rq1y = c("btwoyear", "cthreeyr", "dfouryr", "gsevenyr", "gpdata", "heightyr", "ipdata", "jtenyear", "ltwelvyr", "n14year", "p16year", "rcqdata", "uteds21data", "zmhdata")

rq1y = c("btwoyear", "cthreeyr", "dfouryr", "gsevenyr",           "heightyr",           "jtenyear", "ltwelvyr", "n14year", "p16year", "rcqdata", "uteds21data", "zmhdata")

rq1y_short = c(                  "dfouryr", "gsevenyr",           "heightyr",                       "ltwelvyr", "n14year", "p16year", "rcqdata", "uteds21data", "zmhdata")


# Twin-level participation otucomes 

# rq1y_twinsep = df %>% select(contains("data")) %>% colnames()
# rq1y_twinsep_labels = var_to_label(rq1y_twinsep)
# cbind(rq1y_twinsep, rq1y_twinsep_labels)

rq1y_twin_full = c(
  "btwdata", # removed because not all twins were eligible
  "ctwdata", # removed because not all twins were eligible
  "dtwdata",
  "gcdata",  # removed because not all twins were eligible
  "icdata",  # removed because not all twins were eligible
  "jcdata",  # removed because not all twins were eligible
  "lcwdata",
  "lcqdata",
  "pcwebdata", # removed because not all twins were eligible
  "pcbhdata",
  # removed more niche data collections: 
  "pcl2data",  # Child LEAP-2 booklet data present at 16, 1Y 0N
  "rcfdata",   # Fashion, Food and Music Preferences (FFMP) web study was carried out for cohort 3 twins (aged roughly 19 years) between March and April 2015.
  "rckdata",   # Kings Challenge web study. A battery of 10 twin activities to test spatial abilities.
  "rcqdata",
  "u1cdata",
  "zmhdata",
  "zcdata")

rq1y_twin = c(
  # "btwdata", # removed because not all twins were eligible
  # "ctwdata", # removed because not all twins were eligible
  "dtwdata",
  # "gcdata",  # removed because not all twins were eligible
  # "icdata",  # removed because not all twins were eligible
  # "jcdata",  # removed because not all twins were eligible
  "lcwdata",
  "lcqdata",
  # "pcwebdata", # removed because not all twins were eligible
  "pcbhdata",
  # removed more niche data collections: 
  # "pcl2data",  # Child LEAP-2 booklet data present at 16, 1Y 0N
  # "rcfdata",   # Fashion, Food and Music Preferences (FFMP) web study was carried out for cohort 3 twins (aged roughly 19 years) between March and April 2015.
  # "rckdata",   # Kings Challenge web study. A battery of 10 twin activities to test spatial abilities.
  "rcqdata",
  "u1cdata",
  "zmhdata",
  "zcdata")

rq1y_twin1 = paste0(rq1y_twin,"1")
rq1y_twin2 = paste0(rq1y_twin,"2")


rq2y_prefix  = c(
  "amohqualn",
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

con = c("lcsdqcont1",               "pcbhsdqcont1",     "u1csdqcont1",            "zmhsdqcont1") # SDQ conduct disorder

per = c("lcsdqpert1",               "pcbhsdqpert1",     "u1csdqpert1",            "zmhsdqpert1") # SDQ peer problems

ext = c("lsdqext1",                 "psdqext1",         "usdqext1",               "zsdqext1")

rq5y = c(
  gca,
  edu,
  mfq,
  gad,
  ext
  )

rq5y_prefix = str_remove(rq5y, "1$")

rq5y_12 = paste0(rep(rq5y_prefix, each = 2), c("1", "2"))

rq6y        = c("lcmfqt1","lsdqext1", "lcg1", "pcexgcsecoregrdm1") 
rq6y_all    = c("lcmfqt1","lsdqext1", "lcg1", "pcexgcsecoregrdm1", 
                "lcmfqt2","lsdqext2", "lcg2", "pcexgcsecoregrdm2")

rq6y_prefix = c("lcmfqt" ,"lsdqext" , "lcg",  "pcexgcsecoregrdm")

# Edit and create variables  ---------------------------------------------------

df = df %>%
  mutate(across(starts_with(rq1y_twin), ~ replace_na(.x, 0)))

df$gsevenyr[is.na(df$gsevenyr)] = 0
df$heightyr[is.na(df$heightyr)] = 0

df$aadults   = haven::as_factor(df$aadults, levels = "label") %>%
  droplevels(c( "unknown or other","biological mother with missing partner details") )
df$asingle   = (as.character(df$aadults)=="single parent") %>% as.numeric()
df$asingle[which(df$aadults=="biological mother with missing partner details")] = NA

df$asingle   = factor(df$asingle, levels = 0:1, labels = c("cohabiting biological mother and father / cohabiting biological parent with other","single parent"))

table(df$asingle, df$aadults)

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
df = rename(df, aonsby = "AONSBY")
df$aonsby    = haven::as_factor(df$aonsby)                                    
df$cohort    = haven::as_factor(df$cohort)
df$cohortnum = as.numeric(df$cohort)                                            # ive checked this gives the correct numbering
df$cohort_by = paste(as.numeric(df$cohort), df$aonsby, sep = "-")                       

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

df$zygos       = haven::as_factor(df$zygos)
df$sexzyg      = droplevels(haven::as_factor(df$sexzyg))
df$x3zygos     = haven::as_factor(df$x3zygos)

## Create SDQ Externalising Scores ---------------------------------------------

df$lsdqext1 = as.numeric(df$lcsdqcont1   + df$lcsdqhypt1)       # Age 12 SDQ Externalising Score
df$psdqext1 = as.numeric(df$pcbhsdqcont1 + df$pcbhsdqhypt1)     # Age 16 SDQ Externalising Score
df$usdqext1 = as.numeric(df$u1csdqcont1  + df$u1csdqhypt1)
df$zsdqext1 = as.numeric(df$zmhsdqcont1  + df$zmhsdqhypt1)

df$lsdqext2 = as.numeric(df$lcsdqcont2   + df$lcsdqhypt2)       
df$psdqext2 = as.numeric(df$pcbhsdqcont2 + df$pcbhsdqhypt2)     
df$usdqext2 = as.numeric(df$u1csdqcont2  + df$u1csdqhypt2)   
df$zsdqext2 = as.numeric(df$zmhsdqcont2  + df$zmhsdqhypt2)

## Recode job variables --------------------------------------------------------

# Mother (female parent) job classification
df$amosoc2 = as.character(df$amosoc)
df$amosoc2[df$amojob=="no"] = "no job"
df$amosoc2[df$amojob=="caring for children at home"] = "caring for children at home"
df$amosoc2 = set_most_frequent_ref(as.factor(df$amosoc2))

df$amohqualn1  = df$amohqualn
df$amohqualn2  = df$amohqualn

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

## Pollution PCA score ---------------------------------------------------------

x = df %>%
  select(c(pollution1998pm10, pollution1998pm25, pollution1998no2, pollution1998nox, pollution1998benzene,
           pollution1998ozone)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.data.frame() %>%
  psych::pca(nfactors = 1, missing = FALSE)

df$pollution1998pca = as.numeric(x$scores)

rm(x)

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

# Create Labels ----------------------------------------------------------------

rq1x_labels = rq1x %>% var_to_label()

rq1x_labels_clean = c(
  "Twin Sex",
  "Mother age at birth",
  "Father age at birth",
  "Single Parent",
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
  # "Population density",
  "Pollution index"
)

rq1y_labels = rq1y %>% var_to_label()

rq1y_labels_clean = clean_rq1y_label(rq1y_labels)

rq1y_twin_labels = var_to_label(rq1y_twin1)

rq1y_twin_labels_clean = c(
  # "Y2 (parent-report twin booklet)",
  # "Y3 (parent-report twin booklet)",
  "Y4 (parent-report twin booklet)",
  # "Y7 (telephone interview)",
  # "Y9 (child booklet)",
  # "Y10 (web tests)",
  "Y12 (web tests)",
  "Y12 (questionnaire)",
  # "Y16 (web study)",
  "Y16 (behaviour booklet)",
  # "Y16 (LEAP-2 booklet)",
  "Y18 (questionnaire)",
  "Y21 (TEDS21 phase-1 questionnaire)",
  "Y26 (TEDS26 questionnaire)",
  "Y26 (CATSLife web tests)"
)

rq1y_twin_labels_clean_extrashort = c(
  # "Y2",
  # "Y3",
  "Y4",
  # "Y7",
  # "Y9",
  # "Y10",
  "Y12 (web test)",
  "Y12 (q'aire)",
  # "Y16 (web)",
  "Y16 (q'aire)",
  # "Y16 (LEAP-2 booklet)",
  "Y18",
  "Y21",
  "Y26 (q'aire)",
  "Y26 (web test)"
)

cbind(rq1y_twin_labels, rq1y_twin_labels_clean, rq1y_twin_labels_clean_extrashort)

if (length(rq1y_twin)!=length(rq1y_twin_labels_clean)) stop("clean labels don't match")

rq2y_labels = rq2y %>% var_to_label()

rq2y_labels_short = c(
  "Maternal Education",
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

rq5y_labels = rq5y %>% var_to_label()

rq5y_labels_short = c(
  "Y12: Cognitive ability",
  "Y14: Cognitive ability", 
  "Y16: Cognitive ability",
  "Y21: G-game total score",
  "Y14: KS3 academic achievement",
  "Y16: GCSE core subjects grade",
  "Y21: Highest qualification",
  "Y26: Highest qualification",
  "Y12: Depression (MFQ)",
  "Y16: Depression (MFQ)", 
  "Y21: Depression (MFQ)",
  "Y26: Depression (MFQ)",
  "Y21: Anxiety (GAD-D)",
  "Y26: Anxiety (GAD-D)",
  "Y12: Externalising",
  "Y16: Externalising",
  "Y21: Externalising",
  "Y26: Externalising"
)

cbind(rq5y_labels, rq5y_labels_short)

mfq_labels = df %>% 
  select(all_of(mfq)) %>%
  sapply(., function(x) attr(x, "label"))

rq6y_labels = rq5y_labels_short[match(rq6y, rq5y)]

## Check Labels and variables match --------------------------------------------

testthat::test_that("Label lengths match variable lengths", {
  testthat::expect_equal(length(rq1x), length(rq1x_labels))
  testthat::expect_equal(length(rq1x), length(rq1x_labels_clean))
  
  testthat::expect_equal(length(rq1y), length(rq1y_labels))
  testthat::expect_equal(length(rq1y), length(rq1y_labels_clean))
  
  testthat::expect_equal(length(rq1y_twin1), length(rq1y_twin_labels_clean))
  testthat::expect_equal(length(rq1y_twin1), length(rq1y_twin_labels_clean_extrashort))
  
  
  testthat::expect_equal(length(rq2y), length(rq2y_labels))
  testthat::expect_equal(length(rq2y), length(rq2y_labels_short))
  
  testthat::expect_equal(length(rq5y), length(rq5y_labels_short))
  testthat::expect_equal(length(rq5y), length(rq5y_prefix))
  
  testthat::expect_equal(length(rq6y), length(rq6y_labels))
  
  testthat::expect_true(all(rq5z %in% colnames(df)))
  
})

# Define Participant Exclusion Lists -------------------------------------------

## General ---------------------------------------------------------------------

## We want to exclude families with with only one sibling in the study following application of the exclusion criteria
exclude_fams_onesib = df %>%
  count(randomfamid) %>% 
  filter(n == 1) %>%
  pull(randomfamid) %>%
  as.character()

## Exclude fams with missing baseline data -------------------------------------

exclude_fams_rq1x = df %>%
  filter(rowSums(is.na(across(all_of(rq1x)))) > 7) %>%
  pull(randomfamid) %>%
  as.character()

## Research Question 2 (imputation / weighting analyses) -----------------------

rq2_exclude_fams = df %>%
  select(
    randomfamid,
    starts_with(rq2y_prefix)
  ) %>%
  select(
    -amohqualn1,
    -amohqualn2
  ) %>%
  apply(., 1, function(x) length(which(!is.na(x))))


## Research Question 5 ---------------------------------------------------------

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

## Research Question 6 ----------------------------------------------------------

exclude_fams_rq6y = df %>% 
  mutate(n_data_points = rowSums(!is.na(across(all_of(rq6y))))) %>%
  group_by(randomfamid) %>%
  summarise(n_data_points_per_fam = sum(n_data_points)) %>%
  filter(n_data_points_per_fam == 0) %>%
  pull(randomfamid)

# Output df colnames and labels ------------------------------------------------

df_colnames = colnames(df)

df_labels   = sapply(1:ncol(df), function(i) attr(df[,i, drop = TRUE], "label"))


# Save Data to Check for impact of changes ------------------------------------

# saveRDS(df, file.path("data", "df_old.Rds"))
# saveRDS(df, file.path("data", "df_new.Rds"))
# df_old = readRDS(file.path("data", "df_old.Rds"))
# 
# if (identical(df, df_old)==FALSE) {warning("changes made to data cleaning results!!")}
