# ==============================================================================
# === 04_estimate_gmms =========================================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: Andrea Hildebrandt
# Last update: 27.03.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Load the NAPKON datasets from zip files

# === Preparations =============================================================

# 1. Empty environment
rm(list = ls())

# 2. Load necessary packages
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(lcmm)) install.packages("lcmm")
library(lcmm)
if (!require(parallel)) install.packages("parallel")
library(parallel)


# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path = "data"
tab_path = "tables"
mod_path = "lcmms"
boot_path = "bootstrap"

# 4. Load custom functions
lcmm_env <- new.env()
source("functions/functions_for_lcmm.R", local = lcmm_env)
attach(lcmm_env)

# 5. Load data
load(file.path(dat_path, "pop_processed_dataDV.RData"))
load(file.path(dat_path, "suep_processed_dataDV.RData"))

# === Determine context parameters =============================================

# Chose number of cores
cl = detectCores()
demo_mode = TRUE # set true if you just want to test the code but not do the full estimation of all boots


# === Estimate Growth Curves with GMM ==========================================

# --- Specifications --------------------------------------------------------------
mod_spec_list = list(
  list(dv_name= "ecu_moca_total_score", datDV = "datDVP", 
       max_iter_per_class_lin = c(800 ,500, 500, 500, 500, 500, 500),
       rep_per_class_lin = c(NA, 150, 150, 500, 1000, 1000, 1000),
       max_iter_per_class_spl = c(800 ,500, 500, 500, 500, 500),
       rep_per_class_spl = c(NA, 150, 150, 500, 500, 1000),
       estimate = TRUE),
  list(dv_name= "tmt_a_strict", datDV = "datDVP", 
       max_iter_per_class_lin = c(800 ,500, 500, 500, 500, 500),
       rep_per_class_lin = c(NA, 150, 150, 500, 500, 1000),
       max_iter_per_class_spl = c(800, 500,500 ,500, 500),
       rep_per_class_spl = c(NA, 150, 500 , 500, 500),
       estimate = TRUE),
  list(dv_name= "tmt_b", datDV = "datDVP", 
       max_iter_per_class_lin = c(800 ,500, 500, 500, 500),
       rep_per_class_lin = c(NA, 150, 200, 150, 200),
       max_iter_per_class_spl = c(800, 500, 500, 500, 500),
       rep_per_class_spl = c(NA, 150,500,500,500),
       estimate = TRUE),
  list(dv_name= "average_rt_100", datDV = "datDVP", 
       max_iter_per_class_lin = c(800, 500, 500, 500, 500, 500),
       rep_per_class_lin = c(NA, 500, 150, 300, 200, 300),
       max_iter_per_class_spl = c(800, 500, 500, 500, 500),
       rep_per_class_spl = c(NA, 150, 200, 450, 1000),
       estimate = TRUE),
  list(dv_name= "mfi_gen", datDV = "datDVP", 
       max_iter_per_class_lin = c(800, 500, 500, 500, 500, 500),
       rep_per_class_lin = c(NA, 150, 150, 150, 200, 200),
       max_iter_per_class_spl = c(100, 100, 100, 100, 100),
       rep_per_class_spl = c(NA, 150, 500, 500, 500),
       estimate = TRUE),
  list(dv_name= "mfi_phy", datDV = "datDVP", 
       max_iter_per_class_lin = c(800, 500, 500, 500, 500, 500, 500),
       rep_per_class_lin = c(NA, 150, 150, 150, 200, 200, 200),
       max_iter_per_class_spl = c(800, 500, 500, 500, 500, 500, 500, 500),
       rep_per_class_spl = c(NA, 150, 150, 150, 150, 200, 150, 200),
       estimate = TRUE),
  list(dv_name= "mfi_men", datDV = "datDVP", 
       max_iter_per_class_lin = c(800, 500, 500, 500, 500, 500, 500, 500),
       rep_per_class_lin = c(NA, 150, 150, 150, 150, 500, 500, 500),
       max_iter_per_class_spl = c(800, 500, 500, 500, 500, 500),
       rep_per_class_spl = c(NA, 150, 150, 150, 500, 500, 500),
       estimate = TRUE),
  list(dv_name= "cog_fun", datDV = "datDVS", 
       max_iter_per_class_lin = c(800, 500, 500, 500, 500, 500, 500, 500, 500),
       rep_per_class_lin = c(NA, 150, 150, 150, 500, 1000, 1000, 250, 1000),
       max_iter_per_class_spl = c(800, 500, 500, 500, 500, 500, 1000),
       rep_per_class_spl = c(NA, 200, 250, 300, 500, 1000, 1000),
       estimate = TRUE)
  
)

# --- Model estimation --------------------------------------------------------------
for(dv_spec in mod_spec_list){
  
  if(!dv_spec$estimate){
    next
  }
  
  datDV_name = dv_spec$datDV
  dv_name = dv_spec$dv_name
  
  if(demo_mode){
    # linear
    max_iter_per_class_lin = c(100,10)
    max_rep_per_class_lin = c(NA,2)
    # spline
    max_iter_per_class_spl = c(100,10)
    max_rep_per_class_spl = c(NA,2)

  } else {
    # linear
    max_iter_per_class_lin = dv_spec$max_iter_per_class_lin
    max_rep_per_class_lin = dv_spec$rep_per_class_lin
    # spline
    max_iter_per_class_spl = dv_spec$max_iter_per_class_spl
    max_rep_per_class_spl = dv_spec$rep_per_class_spl

  }


  # linear
  mods_lin = lcmm_pipeline(datDV_name, dv_name, "days_since_inf", "id", max_iter_per_class_lin, max_rep_per_class_lin, fun = "lcmm", shape = "lin", type = "gmm", cl = cl)
  
  # spline
  mods_spl = lcmm_pipeline(datDV_name, dv_name, "days_since_inf", "id", max_iter_per_class_spl, max_rep_per_class_spl, fun = "lcmm", shape = "spl", type = "gmm", cl = cl)
  
  estimated_mods = c(mods_lin, mods_spl)
  
  # save models
  for (modI in 1:length(estimated_mods)) {
    
    if(demo_mode){
      save_path <- paste0(mod_path, "/demo/", estimated_mods[modI], "_demo.RData")
    }else {
      save_path <- paste0(mod_path, "/", estimated_mods[modI], "_replicate_scripts.RData")
    }
    
    eval(parse(text = paste0(
      "save(", estimated_mods[modI],
      ", file = '",save_path,"')"
    )))
  }
  
  # remove models from environment
  rm(list = estimated_mods)
}



# --- Diagnostics --------------------------------------------------------------
for(dv_spec in mod_spec_list){
  
  message(paste("Diagnostics for:", dv_spec$dv_name))
  
  datDV <- eval(parse(text = dv_spec$datDV))
  
  # collect all models from environment
  mod_file_names <- list.files(mod_path, pattern = dv_spec$dv_name)
  
  # not those that were permuted or lca
  mod_file_names <- mod_file_names[!grepl("_perm|lca", mod_file_names)]
  
  mods_in_env <- ls()[grepl(paste0("lcmm.*",dv_spec$dv_name), ls())]
  
  # load these models
  for (mod in mod_file_names) {
    loaded_mod_name <- load(paste(mod_path, mod, sep = "/"))
    if(loaded_mod_name %in% mods_in_env){
      warning(paste(loaded_mod_name, "already existed in enviroment and was overwritten by", mod))
    } else {
      mods_in_env <- c(mods_in_env,loaded_mod_name)
    }
    
  }
  
  # compute sd of the dv
  v_sd <- sd(datDV[[dv_spec$dv_name]], na.rm = TRUE)
  
  for (mod in mods_in_env) {
    # adds $coefComp to the model
    eval(parse(text = paste0(mod, "$coefComp <- compare_coefs(", mod, ", 2, 'varcov', v_sd)")))
    eval(parse(text = paste0(mod, "<- ", mod)))
  }
  
  # summarize results
  tab <- summarytable_all_models(dv_spec$dv_name, "!class", which = c("conv", "entropy", "loglik", "BIC", "AIC", "SABIC", "%class"), dataframe = TRUE)
  
  # add info about likelihood replications and number of converged repetitions
  tab <- add_gridsearch_info_to_tab(tab, c("rep", "nConv", "loglikrep", "coefComp"))
  
  
  # add minimal class size
  tab$minClassSize <- unlist(apply(tab[, grepl("%class", names(tab))], 1, function(x) min(x, na.rm = TRUE)))
  
  # inspect if anything here is wrong
  check_gridsearch_tab(tab, critConv = 3, critLoglik = 3)
  
  # save tab
  openxlsx::write.xlsx(tab, paste0(tab_path, "/modeling_sequence_", dv_spec$dv_name, ".xlsx"))
  
  # remove models from environment
  rm(list = mods_in_env)
  
}

