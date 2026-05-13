# ==============================================================================
# === 03_descriptives ==========================================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: Christian Neumann
# Last update: 08.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Descriptive statistics

# === Preparations =============================================================

# 1. Empty environment
rm(list = ls())

# 2. Load necessary packages
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(psych)) install.packages("psych")
library(psych)

# another required package is "Hmisc", but do not load it, since it will mask "describe" from "psych"

# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "models"
boot_path <- "bootstrap"
plots_path <- "plots"


# === Descriptive Statistics ===================================================

for (cohort in c("pop", "suep")) {
  
  print(cohort)
  
  # --- Load respective data ---------------------------------------------------
  dat_dv_name <- load(file.path(dat_path, paste0(cohort, "_processed_dataDV.RData")))
  datDV <- get(ls()[ls() == dat_dv_name])
  
  dat_iv_name <- load(file.path(dat_path, paste0(cohort, "_processed_dataIV.RData")))
  datIV <- get(ls()[ls() == dat_iv_name])
  
  # --- Number of visits per participant ---------------------------------------
  print("Descriptives for visit frequencies:")
  print(datDV$id %>% table %>% as.data.frame %>% describe)
  print(paste("IQR:", datDV$id %>% table %>% IQR))
  
  # --- Days since infection ---------------------------------------------------
  print("Descriptives of days since infection:")
  print(describe(datDV$days_since_inf))
  
  # --- Figure S1 --------------------------------------------------------------
  # Changes in distribution of post-COVID symptom score due to customized procedure
  if (cohort == "suep") {
    p <- ggplot(datIV, aes(x = pcss)) +
      geom_histogram() +
      xlab("Post-COVID Symptom Score") +
      ylab("Frequency") +
      ylim(0, 300)+
      ggtitle("Distribution of Customized Post-COVID-Symptom Score") +
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(file.path(plots_path, "pcss_distribution_custom.png"), p, width = 8, height = 3)
    
    p <- ggplot(datIV, aes(x = pcs_score_sum_with_proms)) +
      geom_histogram() +
      xlab("Post-COVID Symptom Score") +
      ylab("Frequency") +
      ylim(0, 300)+
      ggtitle("Distribution of Original Post-COVID-Symptom Score") +
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(file.path(plots_path, "pcss_distribution_orig.png"), p, width = 8, height = 3)
    
    datIV <- datIV[, names(datIV) != "pcs_score_sum_with_proms"]
    
  # --- Figure S2 --------------------------------------------------------------
  # Distribution of assessment time points in relation to infection in the POP-cohort
  } else {
    datDV$visit <- substr(datDV$mnpvislabel, 7, 8)
    p <- datDV%>%
      ggplot(aes(x = days_since_inf, fill = visit)) +
      geom_histogram(color = "white", alpha = 0.6, position = "identity", bins = 100) +
      scale_fill_brewer(palette = "Set1") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.position = c(0.9, 0.75)) +
      xlab("Days since infection") +
      ylab("Frequency of assessments") +
      labs(fill = "Visit")
    ggsave(file.path(plots_path, "time_distribution_general.png"), p, width = 8, height = 3)
    datDV <- datDV[, names(datDV) != "visit"]
  }
  
  # --- Tables 1 and 2 ---------------------------------------------------------
  # Descriptive statistics of all variables measured for POP and SUEP separately
  dv_names <- names(datDV)[!(names(datDV) %in% c("id", "mnpvislabel", "days_since_inf"))]
  var_names <- c(dv_names, names(datIV)[!(names(datIV) %in% c("id"))])
  
  dat_des <- datIV
  
  # mean across all timepoints for each person (same effect as using a weighted mean)
  for (dv_name in dv_names) {
    wide_dat <- datDV[, c("id", "mnpvislabel", dv_name)] %>%
      pivot_wider(values_from = dv_name, names_from = "mnpvislabel")
    wide_dat <- wide_dat[, 2:ncol(wide_dat)] %>%
      rowMeans(na.rm = TRUE) %>%
      cbind(wide_dat[, 1])
    names(wide_dat) <- c(dv_name, "id")
    dat_des <- full_join(dat_des, wide_dat)
  }
  
  # get descriptives for all variables
  des_tab <- describe(dat_des[, var_names])
  # for dvs use min and max from the datDV dataset instead of the means across timepoints
  des_tab[dv_names, c("min", "max")] <- describe(datDV[, dv_names], na.rm = TRUE)[c("min", "max")]
  des_tab$abs_freq = unlist(lapply(rownames(des_tab), function(x){
    col <- dat_des[[x]]
    if(all(sort(unique(col)) == c(0,1))) return(sum(col == 1, na.rm = TRUE))
    else  return(NA)}))
  
  des_tab$internal_name <- row.names(des_tab)
  des_tab <- as.data.frame(des_tab)
  
  openxlsx::write.xlsx(des_tab, file.path(tab_path, paste0("raw_descriptives_", cohort, ".xlsx")))
  
  # --- Table S1 & S2 ----------------------------------------------------------
  # Spearman correlations among analysed variables for POP and SUEP separately
  
  # compute_correlations
  cors <- Hmisc::rcorr(as.matrix(dat_des[, var_names]), type = "spearman")
  openxlsx::write.xlsx(round(cors$r,2), file.path(tab_path, paste0("raw_correlations_", cohort, ".xlsx")))
  openxlsx::write.xlsx(cors$n, file.path(tab_path, paste0("raw_samplesizes_", cohort, ".xlsx")))
  
  # --- Table S4 ---------------------------------------------------------------
  # Spearman correlations of missing values on dependent variables with independent variables
  dat_miss <- datIV
  for (dv_name in dv_names) {
    wide_dat <- datDV[, c("id", "mnpvislabel", dv_name)] %>%
      pivot_wider(values_from = dv_name, names_from = "mnpvislabel")
    wide_dat[, 2:ncol(wide_dat)] <- !(wide_dat[, 2:ncol(wide_dat)] %>% is.na())
    wide_dat <- wide_dat[, 2:ncol(wide_dat)] %>%
      rowSums() %>%
      cbind(wide_dat[, 1])
    names(wide_dat) <- c(dv_name, "id")
    dat_miss <- full_join(dat_miss, wide_dat)
  }
  
  # compute_correlations
  cors <- Hmisc::rcorr(as.matrix(dat_miss[, !(names(dat_miss) %in% c("id"))]), type = "spearman")
  openxlsx::write.xlsx(round(cors$r,2), file.path(tab_path, paste0("raw_correlations_missings_", cohort, ".xlsx")))
  openxlsx::write.xlsx(cors$n, file.path(tab_path, paste0("raw_samplesizes_missings_", cohort, ".xlsx")))

}

