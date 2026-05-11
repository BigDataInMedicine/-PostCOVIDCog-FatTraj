# ==============================================================================
# === 05_select_best_model =====================================================
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
if (!require(readxl)) install.packages("readxl")
library(readxl)
if (!require(readr)) install.packages("readr")
library(readr)
if (!require(lcmm)) install.packages("lcmm")
library(lcmm)
if (!require(parallel)) install.packages("parallel")
library(parallel)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "lcmms"
boot_path <- "bootstrap"
plot_path <- "plots"

# 4. Load custom functions
lcmm_env <- new.env()
source("functions/functions_for_lcmm.R", local = lcmm_env)
attach(lcmm_env)

# 5. Load data
load(file.path(dat_path, "pop_processed_dataDV.RData"))
load(file.path(dat_path, "suep_processed_dataDV.RData"))

# read in colors
class_colors <- read_delim("class_colors.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

# === Set decision criteria ====================================================
shapes <- c("lin", "spl")

# Number of cores for parallel processing
cl <- detectCores()

# Step flags
do_lca <- FALSE
do_permute <- TRUE
do_pred <- TRUE
do_plot <- TRUE

# === Inspect the modeling sequence ============================================

dv_names <- c("ecu_moca_total_score", "tmt_a_strict", "tmt_b",
              "average_rt_100", "mfi_gen", "mfi_phy", "mfi_men", "cog_fun")

# prepare list of best results
best_fits <- data.frame()

for (dv_name in dv_names) {
  if (dv_name != "cog_fun") {
    datDV <- datDVP
  } else {
    datDV <- datDVS
  }
  
  message("--- ", dv_name, " --------------------------------------------------")
  
  # load modeling sequence
  mod_sequence_table <- list.files(tab_path, paste0("modeling_sequence_", dv_name))
  mod_sequence_table <- mod_sequence_table[!grepl("perm|lca", mod_sequence_table)]
  tab <- read_excel(file.path(tab_path, mod_sequence_table))
  tab <- tab[grepl("gmm", tab$model), ]
  
  crit_names <- c("AIC", "BIC", "SABIC")
  crit_dir <- c("min", "min", "min")
  excl_names <- c("conv", "entropy", "minClassSize")
  excl_vals <- c("!=1", "<0.67", "<2")
  
  tab <- mark_exclusion_criteria_in_tab(tab, excl_names, excl_vals)
  excl_vars <- names(tab)[grepl("_excl", names(tab))]
  
  message("Best fit considering baseline models as well:")
  best_model_of_all_name <- tab$model[find_best_fit(tab, crit_names, crit_dir, excl_vars)]
  print(best_model_of_all_name)
  
  message("Best considering only multiclass models:")
  tab$baseline <- tab$entropy == 1
  best_model_name <- tab$model[find_best_fit(tab, crit_names, crit_dir, c(excl_vars, "baseline"))]
  print(best_model_name)
  
  datDVSampSize <- datDV[!is.na(datDV[dv_name]),]
  best_fits <- rbind(best_fits, data.frame(best_model_name = best_model_name,
                                           best_model_of_all_name = best_model_of_all_name,
                                           n_participants = length(unique(datDVSampSize$id)),
                                           n_assessments = nrow(datDVSampSize)))

  # save table
  openxlsx::write.xlsx(tab, file.path(tab_path, paste0("modeling_sequence_selection_criteria_", dv_name, ".xlsx")))
  
  # --- LCA estimation ---------------------------------------------------------
  if (do_lca) {
    shape <- shapes[unlist(lapply(shapes, function(x) grepl(x, best_model_name)))]
    class_n <- as.numeric(substr(best_model_name,
                                 unlist(gregexpr("[0-9]+", best_model_name)),
                                 unlist(gregexpr("[0-9]+", best_model_name))))
    
    max_iter_per_class <- rep(100, class_n)
    rep_per_class <- rep(0, class_n)
    rep_per_class[class_n] <- 100
    
    mods <- lcmm_pipeline("datDV", dv_name, "days_since_inf", "id",
                         max_iter_per_class,
                         rep_per_class,
                         fun = "lcmm", shape = shape, type = "lca", cl = cl)
    
    v_sd <- sd(datDV[[dv_name]], na.rm = TRUE)
    
    glob_env <- ls(envir = .GlobalEnv)
    mods_in_env <- glob_env[grepl("lcmm", glob_env) & grepl(dv_name, glob_env)]
    
    for (mod in mods_in_env) {
      eval(parse(text = paste0(mod, "$coefComp <- compare_coefs(", mod, ", 2, 'intercept', v_sd)")))
    }
    
    tab_lca <- summarytable_all_models(dv_name, "!class",
                                       which = c("conv", "entropy", "loglik", "BIC", "AIC", "SABIC", "%class"),
                                       dataframe = TRUE)
    
    tab_lca <- add_gridsearch_info_to_tab(tab_lca,
                                          c("rep", "nConv", "loglikrep", "coefComp"))
    
    check_gridsearch_tab(tab_lca, critConv = 3, critLoglik = 3)
    
    tab <- full_join(tab, tab_lca)
    openxlsx::write.xlsx(tab, file.path(tab_path, paste0("modeling_sequence_LCA_added", dv_name, ".xlsx")))
    

    for (mod_i in seq_along(mods_in_env)) {
      eval(parse(text = paste0(
        "save(", mods_in_env[mod_i],
        ", file = '", mod_path, "/", mods_in_env[mod_i], ".RData')"
      )))
    }
  }
  
  
  # --- Permute best model -----------------------------------------------------
  if (do_permute) {
    mod_file_names <- list.files(mod_path, best_model_name)
    mod_file_names <- c(mod_file_names, list.files(mod_path, gsub("gmm", "lca", best_model_name)))
    
    # not those that were already permuted
    mod_file_names <- mod_file_names[!grepl("_perm", mod_file_names)]
    
    mods_in_env <- c()
    # load these models
    for (mod in mod_file_names) {
      loaded_mod_name <- load(paste(mod_path, mod, sep = "/"))
      if(loaded_mod_name %in% mods_in_env){
        warning(paste(loaded_mod_name, "already existed in enviroment and was overwritten by", mod))
      } else {
        mods_in_env <- c(mods_in_env,loaded_mod_name)
      }
      
    }
    
    for(mod_name in mods_in_env){
      mod <- eval(parse(text = mod_name))
      
      if (dv_name != "ecu_moca_total_score") {
        permut_order <- find_order_lcmm_intercepts(mod$model)
      } else {
        permut_order <- find_order_lcmm_intercepts(mod$model, decreasing = TRUE)
      }
      
      mod$model <- permut(mod$model, permut_order)
      
      eval(parse(text = paste0(mod_name, "_perm <- mod")))
      eval(parse(text = paste0("save(", mod_name, "_perm, file = '", mod_path, "/", mod_name, "_perm.RData')")))
    }
    
    
  } else {
    mod_file_name <- list.files(mod_path, paste0(best_model_name, "_perm"))
    load(file.path(mod_path, mod_file_name))
  }
  
  # add change order of class sizes in tab
  mod <- eval(parse(text = paste0(best_model_name, "_perm")))
  mod <- mod$model
  perm_class_sizes <- summarytable(mod, which = c("%class"))
  tab[tab$model == best_model_name,colnames(perm_class_sizes)] <- perm_class_sizes
  openxlsx::write.xlsx(tab, file.path(tab_path, paste0("modeling_sequence_", dv_name, "_perm.xlsx")))

  
  # --- Add classes to data ----------------------------------------------------
  if (do_pred) {
    mod <- eval(parse(text = paste0(best_model_name, "_perm")))
    class_data <- mod$model$pprob[, c("id", "class")]
    names(class_data)[2] <- paste0("class_", best_model_name, "_perm")
    datDV <- full_join(datDV, class_data)
    
    
    # compare lca to gmm
    if(do_lca){
      lca_model_name <- gsub("gmm", "lca", best_model_name)
      
      mod <- eval(parse(text = paste0(lca_model_name, "_perm")))
      class_data <- mod$model$pprob[, c("id", "class")]
      names(class_data)[2] <- paste0("class_", lca_model_name, "_perm")
      datDV <- full_join(datDV, class_data)
      message("Highest agreement with LCA:")
      find_highest_agreement(datDV,
                                      paste0("class_", best_model_name, "_perm"),
                                      paste0("class_", lca_model_name, "_perm"), crit = "percentage") %>% print
    }
    
    
    if (dv_name != "cog_fun") {
      datDVP <- datDV
      save(datDVP, file = file.path(dat_path, "pop_processed_dataDV_with_classes.RData"))
    } else {
      datDVS <- datDV
      save(datDVS, file = file.path(dat_path, "suep_processed_dataDV_with_classes.RData"))
    }
    
    
  }
  
  # --- Plot models --------------------------------------------------------------
  if(do_plot){
    mod <- eval(parse(text = paste0(best_model_name, "_perm")))
    
    # plot best model class wise and all classes
    set.seed(123)
    p <- plot_lcmm_trajectories(mod$model, datDV, dv_name, sub_samp_n = 15,
                                light_colors = class_colors$light_colors, dark_colors = class_colors$dark_colors) 
    
    # save plot
    save(p, file = paste0(plot_path, "/best_fit_multi_class_", dv_name, ".RData"))
    
    # each class separately with all individual trajaectories
    for(classI in 1:mod$model$ng){
      p <- plot_lcmm_trajectories(mod$model, datDV, dv_name, show_classes = c(classI),
                                  light_colors = class_colors$light_colors, dark_colors = class_colors$dark_colors) 
      
      # save plot
      save(p, file = paste0(plot_path, "/best_fit_multi_class_", dv_name, "_class_", classI, ".RData"))
    }
    
    
    # plot corresponding model with only one class -----------------------------
    baseline_name <- gsub(mod$model$ng, "1", best_model_name)
  
    #find model paths
    mod_file_names <- list.files(mod_path, baseline_name)
    
    mods_in_env <- c()
    # load these models
    for (mod in mod_file_names) {
      loaded_mod_name <- load(paste(mod_path, mod, sep = "/"))
      if(loaded_mod_name %in% mods_in_env){
        warning(paste(loaded_mod_name, "already existed in enviroment and was overwritten by", mod))
      } else {
        mods_in_env <- c(mods_in_env,loaded_mod_name)
      }
      
    }
    
    mod <- eval(parse(text = baseline_name))
    
    # plot it
    set.seed(123)
    p <- plot_lcmm_trajectories(mod, datDV, dv_name,
                                light_colors = class_colors$light_colors, dark_colors = class_colors$dark_colors) 
    
    # save plot
    save(p, file = paste0(plot_path, "/baseline_", dv_name, ".RData"))
    
    # remove model
    rm(mods_in_env)  
    
    # also plot better one class model
    if((best_model_of_all_name != best_model_name) & (best_model_of_all_name != baseline_name)){
      
      #find model paths
      mod_file_names <- list.files(mod_path, best_model_of_all_name)
      
      mods_in_env <- c()
      # load these models
      for (mod in mod_file_names) {
        loaded_mod_name <- load(paste(mod_path, mod, sep = "/"))
        if(loaded_mod_name %in% mods_in_env){
          warning(paste(loaded_mod_name, "already existed in enviroment and was overwritten by", mod))
        } else {
          mods_in_env <- c(mods_in_env,loaded_mod_name)
        }
        
      }
      
      mod <- eval(parse(text = best_model_of_all_name))
      
      # plot it
      set.seed(123)
      p <- plot_lcmm_trajectories(mod, datDV, dv_name,
                                  light_colors = class_colors$light_colors, dark_colors = class_colors$dark_colors) 
      
      # save plot
      save(p, file = paste0(plot_path, "/best_fit_baseline_", dv_name, ".RData"))
      
      # remove model
      rm(mods_in_env)      
    }
  
    # and plot lca
    if(do_lca){
      lca_model_name <- gsub("gmm", "lca", best_model_name)
      
      mod <- eval(parse(text = paste0(lca_model_name, "_perm")))
      
      # plot best model class wise and all classes
      set.seed(123)
      p <- plot_lcmm_trajectories(mod$model, datDV, dv_name, sub_samp_n = 15,
                                  light_colors = class_colors$light_colors, dark_colors = class_colors$dark_colors,
                                  id_name = "id", iv_name = "days_since_inf") # has to be added because model does not have predRE
      # save plot
      save(p, file = paste0(plot_path, "/best_fit_lca_", dv_name, ".RData"))
    }
  }
}

# save the best fits as r file for later use
save(best_fits, file = paste0(tab_path, "/best_fit_res.Rda"))

