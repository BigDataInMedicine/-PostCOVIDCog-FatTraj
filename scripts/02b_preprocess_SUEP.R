# ==============================================================================
# === 02b_preprocess_suep ======================================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: Christian Neumann
# Last update: 08.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Format and preprocess the SUEP dataset

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

custom_funs_path <- "functions/custom_funs.R"
dat_in <- file.path(dat_path, "suep_all_data.RData")
dat_out <- dat_path

# 4. Load custom functions
npkn_env <- new.env()
source("functions/npkn.R", local = npkn_env)
attach(npkn_env)

# === Load data and add custom PCS score =======================================

load(file = dat_in)

# === Extract variables ========================================================

# Since there are two variables called gec_pr_docudate_1.date that will be assigned to the same rows, change their names
names(dat)[get_dataset("gec_pr_docudate_1.date", dat)]
dat$m2$date_m2 <- dat$m2$gec_pr_docudate_1.date
dat$ecu_who_scale_per_visit_data$date_who <- dat$ecu_who_scale_per_visit_data$gec_pr_docudate_1.date

# Change the name of visit_label in ecu_pcs_score to match the rest of the data
names(dat$ecu_pcs_score)[names(dat$ecu_pcs_score) == "visit_label"] <- "mnpvislabel"
names(dat$ecu_pcs_score_original)[names(dat$ecu_pcs_score_original) == "visit_label"] <- "mnpvislabel"

dat_sel <- get_vars(
  c(
    "pro_pc2r", "pro_pc35r", "pro_pc36r", "pro_pc42r",
    "gec_diag_date.date", "pr_docudate.date", "date_m2", "pr_incl_date.date",
    "pcs_score_sum_with_proms_without_cog", "pcs_score_sum_with_proms",
    "work_incap",
    "ecu_age", "gec_gender",
    "gec_height", "gec_weight", "graduation_school"
  ),
  dat,
  pids = TRUE, vlabs = TRUE
)

# Check the data
dim(dat_sel)  # 6184 x 19
which(table(dat_sel$export_psn, dat_sel$mnpvislabel) > 1) # no duplicates


# === Preprocessing ============================================================

# replace -1 with NA
dat_sel[dat_sel == -1] <- NA

# --- Days since infection -----------------------------------------------------
# Check if any row has multiple, incongruent dates
incongr_dates <- apply(dat_sel[, c("pr_docudate.date", "date_m2", "pr_incl_date.date")], 1,
                       function(x) sum(!(x %>% unique() %>% is.na())) > 1) %>% which()
# Combine different date variables
dat_sel$doa_date <- combine_nonbinary_vectors("lowest",
                                              dat_sel$pr_docudate.date, dat_sel$date_m2, dat_sel$date_who, dat_sel$pr_incl_date.date
) %>% as.Date()
# copy the date of the positive test to all visits for the same person
dat_sel <- copy_value_to_all(dat_sel, "export_psn", "gec_diag_date.date", "gec_diag_date.dateF")
# Compute difference between assessment and positive test date
dat_sel$days_since_inf <- as.numeric(dat_sel$doa_date - dat_sel$gec_diag_date.dateF)
# All rows with less than 0 or more than 412 days are implausible (replace by NA)
invalid_ids <- show_invalid_values(dat_sel, "days_since_inf", lower = 0, upper = 412) # 36 invalid values
dat_sel$days_since_inf[invalid_ids] <- NA
# remove all assessments without days_since_inf
dat_sel <- dat_sel[!is.na(dat_sel$days_since_inf), ]

# range of initial test date
dat_sel$gec_diag_date.date %>% range(na.rm = TRUE)

# --- Participant ID -----------------------------------------------------------
dat_sel$id <- as.numeric(substr(dat_sel$export_psn, 6, 10))

# --- Dependent variables ------------------------------------------------------
# Cognitive functioning: 4-20 (sum of 5 x 1-5)
dat_sel$cog_fun <- rowSums(dat_sel[, c("pro_pc2r", "pro_pc35r", "pro_pc36r", "pro_pc42r")], na.rm = FALSE)
show_invalid_values(dat_sel, "cog_fun", upper = 20, lower = 4)

# --- Independent variables ----------------------------------------------------
# PCS score: 0-59
show_invalid_values(dat_sel, "pcs_score_sum_with_proms", upper = 59, lower = 0)
show_invalid_values(dat_sel, "pcs_score_sum_with_proms_without_cog", upper = 59, lower = 0)
show_invalid_values(dat_sel, "ecu_age", upper = 105, lower = 18)

# Sick days due to post-COVID
dat_sel$work_incap %>% table # plausible

# Age - between 18 and 105
show_invalid_values(dat_sel, "ecu_age", upper = 105, lower = 18)

# Gender
dat_sel$gec_gender |> table() # 1 = female; 2 = male
# Transform to logical (female = TRUE)
dat_sel$female_gender_sec <- !as.logical(dat_sel$gec_gender - 1)
dat_sel$female_gender_sec |> table() 

# BMI: height 100 - 230  weight 35 - 230
# Height
show_invalid_values(dat_sel, "gec_height", upper = 230, lower = 100)
# Weight
show_invalid_values(dat_sel, "gec_weight", upper = 230, lower = 35)
# Compute BMI
dat_sel$bmi_sec <- dat_sel$gec_weight / (dat_sel$gec_height / 100)^2

# Education
# 1 = Aktuell Schüler*in, 2 = Schule beendet ohne Abschluss, 3 = Haupt- oder Volksschulabschluss, POS 8. oder 9. Klasse, Abschluss nach höchstens 7 Jahren Schulbesuch, 4 = Realschulabschluss, Mittlere Reife, POS 10. Klasse oder gleichwertiger Abschluss,
# 5 = Abitur, fachgebundene Hochschulreife oder Fachhochschulreife, -1 = Keine Informationen verfügbar
# those with 5 have at least 12 years of education
dat_sel$edu_min_12_sec <- dat_sel$graduation_school == 5
dat_sel$edu_min_12_sec[is.na(dat_sel$graduation_school)] <- NA

# Hospitalization
# select all patients with code U07
# this comes from the original dataset because it would be meaningless to merge this with the rest of the data due to the missing visit labels
dat$eresid$id <- substr(dat$eresid$export_psn, 6, 10) %>% as.numeric()  # participant ids
dat$eresid$hospitalized_sec <- grepl("U07|U7", dat$eresid$resid_icd10) & !is.na(dat$eresid$resid_icd10) # whether or not this visit counts as hospitalization

# intensive care unit
dat$eward$id <- substr(dat$eward$export_psn, 6, 10) %>% as.numeric()  # participant ids
dat$eward$intensive_care <- dat$eward$gec_ward == 4

# --- Tukey outlier removal ----------------------------------------------------
# All values are deleted that are outliers according to the tukey method with a multiplicator of 6
# this is a higher multiplicator than is typically used
# however, we are interested in participants with cognitive impairments and they are likely to deviate from the norm
dat_sel_red <- dat_sel  # create copy of dataset, in case this section is run multiple times
tukey_mult <- 6 # tukey multiplicator
for (var_i in c(
  "cog_fun",
  "pcs_score_sum_with_proms", "pcs_score_sum_with_proms_without_cog",
  "ecu_age", "bmi_sec"
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
datDVS <- dat_sel_red[, c("id", "mnpvislabel", "cog_fun", "days_since_inf")]

# Data with independent variables for class prediction
datIVS <- data.frame(id = unique(dat_sel_red$id))
# PCS score from 3M follow-up
datIVS <- full_join(datIVS, dat_sel_red[dat_sel_red$mnpvislabel == "3M Follow-Up",
                                          c("id", "pcs_score_sum_with_proms", "pcs_score_sum_with_proms_without_cog")
])
# Sick days due to post-COVID
# If one measurement per person is true, set true for every instance of this person
datIVS <- full_join(datIVS, any_or_NA_by_person(dat_sel_red, "id", "work_incap", "sick_days_sec"))
# Age, gender, BMI, Education all from baseline
datIVS <- full_join(datIVS, dat_sel_red[dat_sel_red$mnpvislabel == "Baseline",
                                          c("id", "ecu_age", "female_gender_sec", "bmi_sec", "edu_min_12_sec")
])
# Hospitalization from eresid dataset
datIVS <- left_join(datIVS, any_or_NA_by_person(dat$eresid, "id", "hospitalized_sec", "hospitalized_sec"))
datIVS <- left_join(datIVS, any_or_NA_by_person(dat$eward, "id", "intensive_care", "intensive_care"))
# only select intensive care for PCS
datIVS$intensive_care[datIVS$hospitalized_sec == FALSE | is.na(datIVS$hospitalized_sec)] <- FALSE

# transform all logical variables to numeric
datIVS <- as.data.frame(apply(datIVS, 2, function(x) if (is.logical(x)) as.numeric(x) else x))
# rename the predictor variables for easier handling later
datIVS <- rename(datIVS,
                  pcss = pcs_score_sum_with_proms_without_cog, sick_days = sick_days_sec,
                  age = ecu_age, female_gender = female_gender_sec, bmi = bmi_sec,
                  edu_min_12 = edu_min_12_sec, hospitalized = hospitalized_sec, intensive_care = intensive_care
)

#save the two datasets
save(datDVS, file = file.path(dat_out, "suep_processed_dataDV.RData"))
save(datIVS, file = file.path(dat_out, "suep_processed_dataIV.RData"))

