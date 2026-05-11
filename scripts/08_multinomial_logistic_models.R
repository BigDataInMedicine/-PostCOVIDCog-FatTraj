# ==============================================================================
# === 08_multinomial_logistic_models ===========================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: Andrea Hildebrandt
# Last update: 27.03.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Predicting class membership by lts indicators and risk factors

# === Preparations =============================================================

# 1. Empty environment
rm(list = ls())

# 2. Load necessary packages
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)

# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "models"
boot_path <- "bootstrap"

# 4. Load custom functions
custom_funs_path <- "functions/custom_funs.R"
source(custom_funs_path)

# 5. Load data
load(file.path(dat_path, "pop_processed_dataDV_with_classes.RData"))
load(file.path(dat_path, "suep_processed_dataDV_with_classes.RData"))
load(file.path(dat_path, "pop_processed_dataIV.RData"))
load(file.path(dat_path, "suep_processed_dataIV.RData"))

# === Predictor models =========================================================

all_pcs_indicators <- c("med_treatment", "sick_days", "pcss")
all_risk_factors <- c("age", "female_gender", "bmi", "edu_min_12", "hospitalized")

dv_names <- c("ecu_moca_total_score", "tmt_a_strict", "tmt_b", "average_rt_100", "mfi_phy", "mfi_men", "cog_fun")

for (dv_name in dv_names) {
  
  # --- Data preparation --------------------------------------------------------
  if (dv_name != "cog_fun") {
    datDV <- datDVP
    datIV <- datIVP
  } else {
    datDV <- datDVS
    datIV <- datIVS
  }
  
  pcs_indicators <- all_pcs_indicators[all_pcs_indicators %in% names(datIV)]
  risk_factors <- all_risk_factors[all_risk_factors %in% names(datIV)]
  
  this_class_name <- names(datDV)[grepl(paste0("class.*gmm.*lcmm.*", dv_name, ".*perm"), names(datDV))]
  dat_full <- full_join(datIV, unique(datDV[!is.na(datDV[[this_class_name]]), c("id", this_class_name)]))
  
  # Recode so that one class is 0
  dat_full[[paste0(this_class_name, "_levelled")]] <- dat_full[[this_class_name]] - 1
  
  n_classes <- sum(!is.na(unique(dat_full[[this_class_name]])))
  if (n_classes == 2) {
    crit <- "aic"
    form <- "formula"
  } else {
    crit <- "AIC"
    form <- "call"
  }
  
  # --- All single predictors ---------------------------------------------------
  mods <- list()
  for (predictor in pcs_indicators[pcs_indicators %in% names(dat_full)]) {
    mods[[length(mods) + 1]] <- logistic_regression_2_or_more_classes(
      dat_full, paste0(this_class_name, "_levelled"), predictor, show = FALSE
    )
  }
  
  aics <- lapply(mods[1:length(pcs_indicators)], function(x) x[[crit]]) %>% unlist()
  forms <- lapply(mods[1:length(pcs_indicators)], function(x) x[[form]]) %>% as.character() %>% unlist()
  best_pcs_indicator_i <- which.min(aics)
  
  # --- Risk factors models -----------------------------------------------------
  mod_risk_without_pcs <- logistic_regression_2_or_more_classes(
    dat_full, paste0(this_class_name, "_levelled"), risk_factors, show = FALSE
  )
  
  mod_risk_with_pcs <- logistic_regression_2_or_more_classes(
    dat_full, paste0(this_class_name, "_levelled"),
    c(pcs_indicators[best_pcs_indicator_i], risk_factors),
    show = FALSE
  )
  
  # --- Save results ------------------------------------------------------------
  if (dv_name != "cog_fun") {
    conf_level <- 0.05 / 6
  } else {
    conf_level <- 0.05
  }
  
  if (n_classes == 2) {
    all_coefs_without <- summary(mod_risk_without_pcs)
    coefs_without <- as.data.frame(t(all_coefs_without$coefficients[, 1]))
    names(coefs_without) <- paste0(names(coefs_without), "_without_pcs")
    str <- as.data.frame(t(all_coefs_without$coefficients[, 2]))
    
    cis_without <- ci(coefs_without, str, conf_level)
    names(cis_without[[1]]) <- paste0(names(cis_without[[1]]), "_lower")
    names(cis_without[[2]]) <- paste0(names(cis_without[[2]]), "_upper")
    
    all_coefs_with <- summary(mod_risk_with_pcs)
    coefs_with <- as.data.frame(t(all_coefs_with$coefficients[, 1]))
    names(coefs_with) <- paste0(names(coefs_with), "_with_pcs")
    str <- as.data.frame(t(all_coefs_with$coefficients[, 2]))
    
    cis_with <- ci(coefs_with, str, conf_level)
    names(cis_with[[1]]) <- paste0(names(cis_with[[1]]), "_lower")
    names(cis_with[[2]]) <- paste0(names(cis_with[[2]]), "_upper")
    
    mlm_res <- cbind(coefs_without, cis_without[[1]], cis_without[[2]],
    coefs_with, cis_with[[1]], cis_with[[2]])
  } else {
    all_coefs_without <- summary(mod_risk_without_pcs)
    coefs_without <- as.data.frame(all_coefs_without$coefficients)
    names(coefs_without) <- paste0(names(coefs_without), "_without_pcs")
    str <- all_coefs_without$standard.errors
    
    cis_without <- ci(coefs_without, str, conf_level)
    colnames(cis_without[[1]]) <- paste0(colnames(cis_without[[1]]), "_lower")
    colnames(cis_without[[2]]) <- paste0(colnames(cis_without[[2]]), "_upper")
    
    all_coefs_with <- summary(mod_risk_with_pcs)
    coefs_with <- as.data.frame(all_coefs_with$coefficients)
    names(coefs_with) <- paste0(names(coefs_with), "_with_pcs")
    str <- all_coefs_with$standard.errors
    
    cis_with <- ci(coefs_with, str, conf_level)
    colnames(cis_with[[1]]) <- paste0(colnames(cis_with[[1]]), "_lower")
    colnames(cis_with[[2]]) <- paste0(colnames(cis_with[[2]]), "_upper")
    
    mlm_res <- cbind(coefs_without, cis_without[[1]], cis_without[[2]],
                     coefs_with, cis_with[[1]], cis_with[[2]])
  }
  
  # --- Hypothesis testing ------------------------------------------------------
  mlm_res_names <- names(mlm_res)
  for (risk_factor in risk_factors) {
    if (any(grepl(risk_factor, mlm_res_names))) {
      upper <- mlm_res[, paste0(risk_factor, "_without_pcs_upper")]
      lower <- mlm_res[, paste0(risk_factor, "_without_pcs_lower")]
      
      mlm_res[[paste0(risk_factor, "_excludes_zero")]] <- !((0 <= upper) & (0 >= lower) |
                                                              (0 >= upper) & (0 <= lower))
      
      with_pcs <- mlm_res[, paste0(risk_factor, "_with_pcs")]
      without_pcs <- mlm_res[, paste0(risk_factor, "_without_pcs")]
      
      mlm_res[[paste0(risk_factor, "_effect_decreases")]] <- (abs(without_pcs) > abs(with_pcs)) |
        ((without_pcs * with_pcs) < 0)
      
      mlm_res[[paste0(risk_factor, "_pcs_pattern")]] <- mlm_res[[paste0(risk_factor, "_effect_decreases")]] &
        mlm_res[[paste0(risk_factor, "_excludes_zero")]]
    }
    
    pcs_indicator <- pcs_indicators[best_pcs_indicator_i]
    upper <- mlm_res[, paste0(pcs_indicator, "_with_pcs_upper")]
    lower <- mlm_res[, paste0(pcs_indicator, "_with_pcs_lower")]
    
    mlm_res[[paste0(pcs_indicator, "_excludes_zero")]] <- !((0 <= upper) & (0 >= lower) |
                                                              (0 >= upper) & (0 <= lower))
  }
  
  # --- Print significant results ----------------------------------------------
  print(dv_name)
  print(mlm_res[, c(paste0(pcs_indicator, "_excludes_zero"),
                    names(mlm_res)[grepl("_pcs_pattern", names(mlm_res))])])
  cat("\n")
  
  write.xlsx(mlm_res, file.path(tab_path, paste0("mlm_results_", dv_name, ".xlsx")))
}
