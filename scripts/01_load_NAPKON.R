# ==============================================================================
# === 01_load_datasets =========================================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: Christian Neumann
# Last update: 08.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Load the NAPKON datasets from zip files

# === Preparations =============================================================

# 1. Empty environment
rm(list = ls())

# 2. Load necessary packages
library(tidyr)
library(dplyr)
library(remotes)
remotes::install_github("nukleus-ecu/epicodr@*release")
library(epicodr)

# 3. Set working directory and path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "models"
boot_path <- "bootstrap"

# Path to raw zip files
pop_zip <- "O:/NAPKON/NAPKON-Daten/Datenherausgabe Antrag_ 2023-09-21_Roheger_NeuroCog-PostCovid/tsExport_Roheger_NeuroCog-PostCovid_POP.zip"
suep_zip <- "O:/NAPKON/NAPKON-Daten/Datenherausgabe 3 Antrag_ 2023-09-21_Roheger_NeuroCog-PostCovid/tsExport_Roheger_NeuroCog-PostCovid_SUEP.zip"

# Path where extracted data will be stored
pop_data <- file.path(dat_path, "pop_all_data.RData")
suep_data <- file.path(dat_path, "suep_all_data.RData")

# 4. Load custom functions (if applicable)
npkn_env <- new.env()
source("functions/epicodr_without_cogn.R", local = npkn_env)
attach(npkn_env)

# === Load POP data ============================================================

dat <- pop_zip %>%
  read_tsExport(separator = ";", decimal = ",")
dat <- primary_coding_pop(dat)
save(dat, file = pop_data)

# === Load SUEP data ============================================================

dat <- suep_zip %>%
  read_tsExport(separator = ";", decimal = ",")
dat <- primary_coding_suep(dat)
dat <- primary_coding_suep_pcs_score(dat, prom = "Yes")

# Keep original PCS score for comparison
dat$ecu_pcs_score_original <- dat$ecu_pcs_score

# Add custom PCS score excluding cognitive items
dat <- primary_coding_suep_pcs_score_without_cog(dat, prom = "Yes")
save(dat, file = suep_data)
