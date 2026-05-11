# ==============================================================================
# === 06b_analyze_bootstraps ===================================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: Andrea Hildebrandt
# Last update: 05.05.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: analyze the results of the bootstraps to decide if you have to refine them further

# === Preparations =============================================================

# 1. Empty environment
rm(list = ls())

# 2. Load necessary packages
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(readxl)) install.packages("readxl")
library(readxl)

# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "lcmms"
boot_path <- "bootstrap_results"

# 4. Load custom functions
lcmm_env <- new.env()
source("functions/functions_for_lcmm.R", local = lcmm_env)
attach(lcmm_env)

# 5. Load data
load(file.path(dat_path, "pop_processed_dataDV.RData"))
load(file.path(dat_path, "suep_processed_dataDV.RData"))

# === Set parameters ===========================================================
crit_names = c("AIC", "BIC", "SABIC") # variables used to judge best model fit
crit_dir = c("min", "min", "min") # direction to be used to judge these variables
excl_names = c("Entropy", "minClassSize") # variables used to exclude models
excl_vals = c("<0.67", "<2") # values used to exclude models
min_loglikrep = 2 # minimal loglikelihood replicaiotns
ignore_baseline = TRUE # ignore baseline model when picking the best model fit

# Dependent variable names for bootstrap evaluation
dv_names <- c("ecu_moca_total_score", "tmt_a", "tmt_b", "average_rt", "mfi_gen", "mfi_phy", "mfi_men", "cog_fun")

# === Collect best models ======================================================

best_models <- list.files(mod_path, "gmm.*perm")
best_models <- best_models[!grepl("lca", best_models)]

# Load all models
for (mod in best_models) {
  load(file.path(mod_path, mod))
}

# Collect bootstrap results
boot_results <- list.files(boot_path)

# load best fits
load(paste0(tab_path, "/best_fit_res.RDa"))
best_fits$cohens_kappa = NA
best_fits$robustness = NA

# === Bootstraps ===============================================================

for (dv_name in dv_names) {
  print(dv_name)
  
  mod_name <- ls()[grepl(paste0("gmm.*", dv_name), ls())]
  
  # Load bootstrap data
  load(file.path(boot_path, boot_results[grepl(dv_name, boot_results) & grepl("full", boot_results)]))
  this_boot <- newBootRes
  
  # Select the right model
  mod <- eval(parse(text = mod_name))
  mod <- mod$model
  
  # Inspect bootstrap results
  bootstrap_diagnostics(this_boot, mod, crit_names, crit_dir, excl_names, excl_vals, min_loglikrep, ignoreBaseline = ignore_baseline)
  boot_eva <- bootstrap_evaluation(this_boot, mod, crit_names, crit_dir, excl_names, excl_vals, ignoreBaseline = ignore_baseline)
                                 
  best_fits$cohens_kappa[best_fits$best_model_name == gsub( "_perm", "", mod_name)] <- boot_eva[[1]]$cohensKappa
  best_fits$robustness[best_fits$best_model_name == gsub( "_perm", "", mod_name)] <- boot_eva[[1]]$Robustness
  cat("-----------------------------------------------------------------------\n")
  
}

# save the best fits as r file for later use
save(best_fits, file = paste0(tab_path, "/best_fit_res_with_boot.Rda"))