# ==============================================================================
# === 09_formatting of tables ==================================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: ---
# Last update: 09.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Formatting of tables for publication

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

# another required package is "Hmisc", but do not load it, since it will mask "describe" from "psych"

# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "lcmms"
boot_path <- "bootstrap"
plots_path <- "plots"


# 4. Load custom functions
format_env <- new.env()
source("functions/functions_for_formatting.R", local = format_env)
attach(format_env)

# 5. Load variable names and constants
publ_names <- read.csv("variable_names.csv", sep = ";")
publ_names$publication_name <- paste(publ_names$publication_name, publ_names$name_unit)

small_cor = 0.2

cohort = "pop"

dv_names <- c("ecu_moca_total_score", "tmt_a_strict", "tmt_b", "average_rt_100", "mfi_gen", "mfi_men", "mfi_phy", "cog_fun")
iv_names <- c( "age" ,"female_gender", "bmi","edu_min_12" , "hospitalized","pcss" , "sick_days" ,"med_treatment")




for (cohort in c("pop", "suep")){
  # === TABLES =================================================================
  # --- Sample Characteristics -----------------------------------------------------------
  des_tab <- read.xlsx(file.path(tab_path, paste0("raw_descriptives_", cohort, ".xlsx")))
  if(cohort == "pop"){ # adapt scaling of PVT to seconds
    des_tab[des_tab$internal_name == "average_rt_100", !(names(des_tab) %in% c("vars", "n", "internal_name"))] <- des_tab[des_tab$internal_name == "average_rt_100", !(names(des_tab) %in% c("vars", "n", "internal_name"))]/100
  }
  des_tab <- left_join(des_tab, publ_names, by = "internal_name")
  des_tab <- des_tab[order(des_tab$publication_order), ] # arrange rows in correct order

  #round remaining number, keeping trialing zeros
  des_tab$mean[!is.na(des_tab$abs_freq)] <- des_tab$mean[!is.na(des_tab$abs_freq)] *100
  des_tab <- as.data.frame(apply(des_tab, 2, function(x) round_with_trailing_zero(x, nDig = 2)))
  
  # format column with mean(sd)/n(%)
  des_tab$mean_sd <- paste0(des_tab$mean, " (",des_tab$sd, ")")
  des_tab$n_perc <- paste0(round_with_trailing_zero(des_tab$abs_freq, nDig = 0), " (",des_tab$mean, " %)")
  des_tab$first_col <- des_tab$mean_sd
  des_tab$first_col[!is.na(des_tab$abs_freq)] <- des_tab$n_perc[!is.na(des_tab$abs_freq)]
  des_tab[!is.na(des_tab$abs_freq),c("median", "min", "max")] <- "."
  
  # select only the relevant columns
  des_tab <- des_tab[, c("publication_name", "first_col", "median", "min", "max")]
  
  # rename the columns
  names(des_tab) <- c("", "Mean(SD)/n(%)", "Median", "Min", "Max")
  
  # replace decimal points
  des_tab[, 2:ncol(des_tab)] <- apply(des_tab[, 2:ncol(des_tab)], 2, function(x) gsub( "\\.", "·", x))
  
  # save table
  openxlsx::write.xlsx(des_tab, paste0(tab_path, '/descriptives_', cohort,'.xlsx'))
  
  # --- Correlatios ------------------------------------------------------------
  
  cors_r <- read.xlsx(file.path(tab_path, paste0("raw_correlations_", cohort, ".xlsx")))
  cors_n <- read.xlsx(file.path(tab_path, paste0("raw_samplesizes_", cohort, ".xlsx")))
  
  # combine r and n
  cor_tab <- cors_r
  for(colI in 1:ncol(cor_tab)){
    cor_tab[,colI] = paste0(cors_r[,colI], " (", cors_n[,colI], ")")
  }
  
  cor_tab$internal_name = names(cor_tab)
  
  # adjust names  for publication
  cor_tab <- left_join(cor_tab, publ_names, by = "internal_name")
  cor_tab$publication_order <- rank(cor_tab$publication_order)
  cor_tab$publication_name <- paste(cor_tab$publication_order, cor_tab$publication_name)
  var_cols <- names(cor_tab) %in% cor_tab$internal_name
  names(cor_tab)[var_cols] <- lapply(names(cor_tab)[var_cols], function(x) cor_tab$publication_order[cor_tab$internal_name == x])
  
  #select only relevant cols and rows in the right order
  cor_tab <- cor_tab[order(cor_tab$publication_order),c("publication_name",sort(cor_tab$publication_order)) ]

  # delete above diagonal
  row_i <- rep(1:sum(var_cols), times = sum(var_cols))
  col_i <- rep(1:sum(var_cols), each = sum(var_cols))
  cor_tab[cbind(FALSE, matrix(col_i >= row_i, nrow = sum(var_cols), ncol = sum(var_cols)))] <- ""
  cor_tab <- cor_tab[, 1:(ncol(cor_tab) - 1)]
  names(cor_tab)[1] <- ""
  
  
  # save table
  openxlsx::write.xlsx(cor_tab, paste0(tab_path, '/correlations_', cohort,'.xlsx'))
  
  
  # --- Correlations missings --------------------------------------------------
  
  cors_r <- read.xlsx(file.path(tab_path, paste0("raw_correlations_missings_", cohort, ".xlsx")))
  cors_n <- read.xlsx(file.path(tab_path, paste0("raw_samplesizes_missings_", cohort, ".xlsx")))
  
  # combine r and n
  cor_tab <- cors_r
  
  # add stars for small correlations
  crit_cor <- cor_tab >= small_cor
  cors_r <- apply(cors_r, 2, function(x) round_with_trailing_zero(x, nDig = 2))
  cors_r[crit_cor] <- paste0(cors_r[crit_cor], "*")
  
  for(colI in 1:ncol(cor_tab)){
    cor_tab[,colI] = paste0(cors_r[,colI], " (", cors_n[,colI], ")")
  }
  
  cor_tab$internal_name = names(cor_tab)
  
  # adjust names  for publication
  cor_tab <- left_join(cor_tab, publ_names, by = "internal_name")
  var_cols <- names(cor_tab) %in% cor_tab$internal_name
  names(cor_tab)[var_cols] <- lapply(names(cor_tab)[var_cols], function(x) cor_tab$publication_name[cor_tab$internal_name == x])
  
  #put cols and rows in the right order
  cor_tab <- cor_tab[order(cor_tab$publication_order),]
  var_names <- cor_tab$publication_name[cor_tab$internal_name %in% iv_names]
  var_names_i <- rank(unlist(lapply(var_names, function(x) cor_tab$publication_order[cor_tab$publication_name == x])))
  cor_tab <- cor_tab[cor_tab$internal_name %in% dv_names, c("publication_name", var_names[var_names_i])]
  names(cor_tab)[1] <- "Dependent variable"
  
  if(cohort == "pop"){
    cor_tab_total <- cor_tab
  } else {
    cor_tab_total <- full_join(cor_tab_total, cor_tab)
    # save table
    openxlsx::write.xlsx(cor_tab_total, paste0(tab_path, '/correlations_missings.xlsx'))
  }
}
