# ==============================================================================
# === 06a_estimate_bootstraps ==================================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Last update: 24.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Computing Bootstraps


# === Notes!!! ==================================================================

# This script demands a lot of computing power
# it was executed using a high performance cluster
# if you try to apply this to a new dataset, I recommend starting with a small number of boots (e.g., 5) to figure out the number of replications and iterations

# === Preparations =============================================================

# 1. Empty environment
rm(list = ls())

# 2. Load necessary packages
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(lcmm))install.packages("lcmm")
library("lcmm")
if(!require(parallel))install.packages("parallel")
library("parallel")

# another required package is "Hmisc", but do not load it, since it will mask "describe" from "psych"

# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "lcmms"
boot_path <- "bootstrap_results"
plots_path <- "plots"


# 4. Load custom functions
lcmm_env <- new.env()
source("functions/functions_for_lcmm.R", local = lcmm_env)
attach(lcmm_env)

# 5. Load variable names and constants
publ_names <- read.csv("variable_names.csv", sep = ";")
publ_names$publication_name <- paste(publ_names$publication_name, publ_names$name_unit)


# -- set parameters -----------------------------------------------------
cl = detectCores() #  detect number of cores
demo_mode = TRUE # set true if you just want to test the code but not do the full estimation of all boots

if(demo_mode){
  n_boots = 2
} else{
  n_boots = 100
}

now = substr(gsub(":", "-", gsub(" ", "_", Sys.time())), 1, 19)

# --- Load Models and Data -----------------------------------------------------

# load all the final models
mod_names <- list.files(mod_path, "gmm")
mod_names <- mod_names[grepl( "perm", mod_names)]
for (mod_name in mod_names){
  load(paste(mod_path, mod_name, sep = "/"))
}

# Load data
load(file.path(dat_path, "pop_processed_dataDV.RData"))
load(file.path(dat_path, "suep_processed_dataDV.RData"))


# --- Specifications --------------------------------------------------------------
mod_spec_list = list(
  list(dv_name= "ecu_moca_total_score", datDV = "datDVP",
       max_class = 2, spl= TRUE,
       max_iter_per_class = c(500,500,500,500,500),
       rep_per_class =  c(NA,100,100,100,100),
       estimate = TRUE),
  list(dv_name= "tmt_a_strict", datDV = "datDVP", 
       max_class = 5, spl= FALSE,
       max_iter_per_class = c(500,1000,1000,1000,1000),
       rep_per_class = c(NA,100,150,500,500),
       estimate = TRUE),
  list(dv_name= "tmt_b", datDV = "datDVP", 
       max_class = 5, spl= FALSE,
       max_iter_per_class = c(500,500,500,500,500), 
       rep_per_class = c(NA,100,250,500,500),
       estimate = TRUE),
  list(dv_name= "average_rt_100", datDV = "datDVP", 
       max_class = 5, spl= FALSE,
       max_iter_per_class = c(500,1000,1500,1500,1500), 
       rep_per_class = c(NA,150,100,500,500),
       estimate = TRUE),
  list(dv_name= "mfi_gen", datDV = "datDVP", 
       max_class = 5, spl= TRUE,
       max_iter_per_class = c(500,500,500,500,500), 
       rep_per_class = c(NA,100,300,200,200),
       estimate = TRUE),
  list(dv_name= "mfi_phy", datDV = "datDVP", 
       max_class = 5, spl= TRUE,
       max_iter_per_class = c(500,500,500,500,500, 500), 
       rep_per_class = c(NA,100,100,100,200,100),
       estimate = TRUE),
  list(dv_name= "mfi_men", datDV = "datDVP", 
       max_class = 5, spl= FALSE,
       max_iter_per_class = c(500,500,500,500,500, 500, 500), 
       rep_per_class = c(NA,100,100,100,100,500, 500, 500),
       estimate = TRUE),
  list(dv_name= "cog_fun", datDV = "datDVS", 
       max_class = 5, spl= TRUE,
       max_iter_per_class = c(500,500,500,500,500, 500),
       rep_per_class = c(NA,50,50,50,100,200),
       estimate = TRUE)
)

# --- Model estimation --------------------------------------------------------------
for(dv_spec in mod_spec_list){
  
  if(!dv_spec$estimate){
    next
  }
  
  # if demo mode only use small number of classes/iterations/replication
  if(demo_mode){
    max_class = 2
    max_iter_per_class = c(100, 10)
    rep_per_class = c(NA, 3)
  # otherweise take specifications from list above
  } else {
    max_class = dv_spec$max_class
    max_iter_per_class = dv_spec$max_iter_per_class
    rep_per_class = dv_spec$rep_per_class
  }
  
  #select model
  print(dv_spec$dv_name)
  mod_name <- gsub(".RData", "",mod_names[grepl( dv_spec$dv_name, mod_names)])
  mod <- eval(parse(text = mod_name))
  
  # select data
  datDV <- eval(parse(text = dv_spec$datDV))
  
  # estimate models in bootstrap samples
  start.time <- Sys.time()
  newBootRes <- bootstrap_lcmm(mod = mod,
                         dat = datDV,
                         nBoots = n_boots,
                         maxIterPerClass = max_iter_per_class,
                         repPerClass = rep_per_class,
                         cl = cl,
                         startValues = NA,
                         gmm = TRUE,
                         spl = dv_spec$spl,
                         strat = TRUE)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  
  # save results from bootstrap
  if(demo_mode){
    save(newBootRes, file = paste0(boot_path, "/boot_res_", dv_spec$dv_name, "_demo.RData"))
  } else {
    # overwrite the version in the main folder
    save(newBootRes, file = paste0(boot_path, "/history/boot_res_", dv_spec$dv_name, "_",now, ".RData"))
    # add a new version to the subfolder including the time that the script was run
    # because accidentally making a mistake and overwriting something costs a lot of computing power
    save(newBootRes, file = paste0(boot_path, "/boot_res_", dv_spec$dv_name, "_full.RData"))
  }
}

