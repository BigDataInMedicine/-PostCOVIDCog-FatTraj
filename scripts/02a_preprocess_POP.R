# ==============================================================================
# === 02a_preprocess_pop =======================================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: Christian Neumann
# Last update: 08.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Format and preprocess the POP dataset

# === Preparations =============================================================

# 1. Empty environment
rm(list = ls())

# 2. Load necessary packages
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "models"
boot_path <- "bootstrap"

dat_in <- file.path(dat_path, "pop_all_data.RData")
dat_out <- dat_path

# 4. Load custom functions
npkn_env <- new.env()
source("functions/npkn.R", local = npkn_env)
attach(npkn_env)

# === Load data and add custom PCS score =======================================

load(file = dat_in)

dat$sekundaerdaten <- full_join(
  dat$sekundaerdaten,
  pcs_score_pop_custom(dat)
)

# === Extract relevant variables ==============================================
# Since there are two variables called date that will be assigned to the same rows, change their names
names(dat)[get_dataset("doa.date", dat)]
dat$erstbefragung$doa_date_eb <- dat$erstbefragung$doa.date
dat$interview_aufn$doa_date_vo <- dat$interview_aufn$doa.date

# Select relevant data
dat_sel <- get_vars(
  c(
    "ecu_moca_total_score", "tmt_a", "tmt_b", "average_rt", # cognitive tests
    paste("mfi", 1:20, sep = ""), # MFI fatigue questionnaire
    "doa_date_vo", "covid_test_datum.date",
    "pcss_custom", "arbeitsunf_pc_fu", # secondary PCS score, sick days
    "kh_not_pc_fu", "kh_stat1_pc_fu", "kh_reha_fu", # treatment variables
    "gec_demo_age", "gec_gender",
    "gec_weight", "gec_eb_demo_gew", "gec_height", "gec_eb_demo_groe", # BMI variables
    "schulabschl", "verl_beh1___2", "verl_beh1_int", "verl_beh1___3"
  ),
  dat,
  pids = TRUE, vlabs = TRUE, EBtoVO = TRUE
)

dat_sel <- data.frame(dat_sel)

# Check the data
dim(dat_sel) # 2025 x 42
which(table(dat_sel$export_psn, dat_sel$mnpvislabel) > 1) # no duplicates



# === Preprocessing ============================================================
# unlcass PVT average reaction times
dat_sel$average_rt <- dat_sel$average_rt %>% unclass()
# Replace -1 with NA
dat_sel[dat_sel == -1] <- NA

# --- Days since infection -----------------------------------------------------
# copy the date of the positive test to all visits for the same person
dat_sel <- copy_value_to_all(dat_sel, "export_psn", "covid_test_datum.date", "covid_test_datum.dateF") 
# Compute difference between assessment and positive test date
dat_sel$days_since_inf <- as.numeric(dat_sel$doa_date_vo - dat_sel$covid_test_datum.dateF) 
# All rows with less than 152  or more than 1800 days are impausible (replace by NA)
invalid_ids <- show_invalid_values(dat_sel, "days_since_inf", lower = 152, upper = 1800) # 3 invalid values
dat_sel$days_since_inf[invalid_ids] <- NA
dat_sel <- dat_sel[!is.na(dat_sel$days_since_inf), ]

# range of initial test date
dat_sel$covid_test_datum.date %>% range(na.rm = TRUE)

# --- Participant ID -----------------------------------------------------------
dat_sel$id <- as.numeric(substr(dat_sel$export_psn, 6, 10))

# --- Dependent variables ------------------------------------------------------
# MoCA: 0-30
show_invalid_values(dat_sel, "ecu_moca_total_score", upper = 30, lower = 0)
dat_sel$ecu_moca_total_score[dat_sel$ecu_moca_total_score == 31] <- 30

# TMT-A: 10-180
dat_sel$tmt_a_strict <- dat_sel$tmt_a
invalid_ids <- show_invalid_values(dat_sel, "tmt_a", upper = 180, lower = 10)
dat_sel$tmt_a_strict[invalid_ids] <- NA

# TMT-B: 15-300
invalid_ids <- show_invalid_values(dat_sel, "tmt_b", upper = 300, lower = 15)
dat_sel$tmt_b[invalid_ids] <- NA

# PVT: No plausibility rules
# format has to be changed to numeric, without decimals to be handled properly by the lcmm function
dat_sel$average_rt_100 <- as.numeric(dat_sel$average_rt) * 100

# MFI Sum Scores: 4-20 (sum of 4 x 1-5)
dat_sel$mfi_gen <- rowSums(dat_sel[, c("mfi1", "mfi5", "mfi12", "mfi16")])
dat_sel$mfi_phy <- rowSums(dat_sel[, c("mfi2", "mfi8", "mfi14", "mfi20")])
dat_sel$mfi_men <- rowSums(dat_sel[, c("mfi7", "mfi11", "mfi13", "mfi19")])

# --- Independent variables ----------------------------------------------------

# PCS score: 0-59
show_invalid_values(dat_sel, "pcss_custom", upper = 59, lower = 0)

# Treatment due to post-COVID
# indicates whether there was medical treatment (emergency unit, hospital or rehabilitation) due to long-term sequelea
# For each assessment: if one of the three variables is true, med_treatment_pcs_sec = true
dat_sel$med_treatment_pcs_sec <- any_or_NA(dat_sel$kh_not_pc_fu, dat_sel$kh_reha_fu, dat_sel$kh_stat1_pc_fu)

# Sick days due to post-COVID
# Overwrite all the 3 ("does not apply to me") with NA
dat_sel$sick_days_sec <- dat_sel$arbeitsunf_pc_fu
dat_sel$sick_days_sec[dat_sel$arbeitsunf_pc_fu == 3] <- NA
dat_sel$sick_days_sec <- as.logical(dat_sel$sick_days_sec)

# Age: between 18 and 105
show_invalid_values(dat_sel, "gec_demo_age", upper = 105, lower = 18)

# Sex (variable is called gender, but patients are asked about their biological sex)
dat_sel$gec_gender |> table() # 1 = female; 2 = male
# Transform to logical (female = TRUE)
dat_sel$female_gender_sec <- !as.logical(dat_sel$gec_gender - 1)
dat_sel$female_gender_sec |> table() # female = TRUE; male = FALSE

# BMI: height 100 - 230  weight 35 - 230
# Height
show_invalid_values(dat_sel, "gec_height", upper = 230, lower = 100) # measured
show_invalid_values(dat_sel, "gec_eb_demo_groe", upper = 230, lower = 100) # self-report
# Weight
show_invalid_values(dat_sel, "gec_weight", upper = 230, lower = 35) # measured
show_invalid_values(dat_sel, "gec_eb_demo_gew", upper = 230, lower = 35) # self-report
# Fill all the missing values measured height and weight respectively with self-reports
dat_sel$weight_sec <- combine_nonbinary_vectors("descending_order", dat_sel$gec_weight, dat_sel$gec_eb_demo_gew)
dat_sel$height_sec <- combine_nonbinary_vectors("descending_order", dat_sel$gec_height, dat_sel$gec_eb_demo_groe)
# Compute BMI
dat_sel$bmi_sec <- dat_sel$weight_sec / (dat_sel$height_sec / 100)^2

# Education: German labels below
# 1 = Grundschule/keinen Abschluss, 2 = Hauptschule, 3 = Realschule/Politechnische Oberschule der DDR,
# 4 = Fachabitur/Abitur, 5 = Sonstiger, 7 = Fachhochschule, 8 = Universität
dat_sel$schulabschl[dat_sel$schulabschl == 5] <- NA # 5 = "Sonstiger"
# replace "other" with NA since we cannot know whether it is more or less than 12 years
dat_sel$edu_min_12_sec <- dat_sel$schulabschl
dat_sel$edu_min_12_sec[dat_sel$schulabschl %in% c(4, 7, 8)] <- 1
dat_sel$edu_min_12_sec[dat_sel$schulabschl %in% c(1, 2, 3)] <- 0

# Hospitalization 
# For each assessment: if one of the three variables is true, hospitalized_sec = true
dat_sel$hospitalized_sec <- any_or_NA(dat_sel$verl_beh1___2, dat_sel$verl_beh1_int > 1)

# intensive care unit
dat_sel$intensive_care <- any_or_NA(dat_sel$verl_beh1___3, dat_sel$verl_beh1_int >=3)

# --- Tukey outlier removal ----------------------------------------------------
# All values are deleted that are outliers according to the tukey method with a multiplicator of 6
# this is a higher multiplicator than is typically used
# however, we are interested in participants with cognitive impairments and they are likely to deviate from the norm
dat_sel_red <- dat_sel  # create copy of dataset, in case this section is run multiple times
tukey_mult <- 6 # tukey multiplicator
for (var_i in c(
  "ecu_moca_total_score", "tmt_a_strict", "tmt_b", "average_rt_100",
  "mfi_gen", "mfi_phy", "mfi_men",
  "pcss_custom", "gec_demo_age", "bmi_sec"
)) {
  tukey_excl <- tukey_outliers(var_i, dat_sel_red, multiplier = tukey_mult)
  dat_sel_red[[var_i]] <- tukey_excl[[1]]
  
  # Print what happened
  print(var_i)
  if (length(tukey_excl[[2]]) == 0 || all(is.na(tukey_excl[[2]]))) {
    print("No values excluded.")
  } else {
    print(paste(
      tukey_excl[[3]], "cases excluded:",
      paste(tukey_excl[[2]][!is.na(tukey_excl[[2]])], collapse = ", ")
    ))
  }
  cat("\n")
}

# === Split DVs and IVs ========================================================
# Data with dependent variables for initial lcmm modeling
datDVP <- dat_sel_red[, c(
  "id", "mnpvislabel", "ecu_moca_total_score", "tmt_a_strict", "tmt_b", "average_rt_100", # cognitive tests
  "mfi_gen", "mfi_men", "mfi_phy", # MFI fatigue questionnaire
  "days_since_inf" # days since positive test
)]
# Data with independent variables for class prediction
datIVP <- data.frame(id = unique(dat_sel_red$id))
# Age, gender, BMI, Education, hospitalization and pcs score all from visit 1
datIVP <- full_join(datIVP, dat_sel_red[dat_sel_red$mnpvislabel == "Visite 1-EB/VO",
                                          c("id", "gec_demo_age", "female_gender_sec", "bmi_sec", "edu_min_12_sec", "hospitalized_sec", "intensive_care", "pcss_custom")
])
# Sick days and medical treatment:
# If one measurement per person is true, set true for every instance of this person
datIVP <- full_join(datIVP, any_or_NA_by_person(dat_sel_red, "id", "sick_days_sec", "sick_days_sec"))
datIVP <- full_join(datIVP, any_or_NA_by_person(dat_sel_red, "id", "med_treatment_pcs_sec", "med_treatment_pcs_sec"))

# transform all logical variables to numeric
datIVP <- as.data.frame(apply(datIVP, 2, function(x) if (is.logical(x)) as.numeric(x) else x))
# rename the predictor variables for easier handling later
datIVP <- rename(datIVP,
                  pcss = pcss_custom, med_treatment = med_treatment_pcs_sec, sick_days = sick_days_sec,
                  age = gec_demo_age, female_gender = female_gender_sec, bmi = bmi_sec,
                  edu_min_12 = edu_min_12_sec, hospitalized = hospitalized_sec, intensive_care = intensive_care
)

#save the two datasets
save(datDVP, file = file.path(dat_out, "pop_processed_dataDV.RData"))
save(datIVP, file = file.path(dat_out, "pop_processed_dataIV.RData"))