# ==============================================================================
# === 07_comparisons_across_domains ============================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: Andrea Hildebrandt
# Last update: 27.03.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Compare classes across domains

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

# 5. Load dependent variables
load(file.path(dat_path, "pop_processed_dataDV_with_classes.RData"))

# === Predictor models =========================================================

# Identify class variables
class_var_names <- names(datDVP)[grepl("class.*gmm.*lcmm.*perm", names(datDVP))]
class_var_names <- class_var_names[!grepl("mfi_gen", class_var_names)]

# Parameters for comparisons
p_res <- c()
test_a <- c()
test_b <- c()
fisher <- c()
sig <- 0.05 / (choose(length(class_var_names), 2))
fisher_replicates <- 500000

dat_red <- unique(datDVP[, c("id", class_var_names)])

# Loop over all pairs ----------------------------------------------------------
for (var_i1 in 1:(length(class_var_names) - 1)) {
  var1 <- class_var_names[var_i1]
  
  for (var_i2 in (var_i1 + 1):length(class_var_names)) {
    var2 <- class_var_names[var_i2]
    test_a <- c(test_a, var_i1)
    test_b <- c(test_b, var_i2)
    
    dat_sub <- dat_red[, c(var1, var2)]
    dat_sub <- dat_sub[(dat_sub %>% is.na() %>% rowSums()) == 0, ]
    
    print(var1)
    print(var2)
    print(table(dat_sub[[var1]], dat_sub[[var2]]))
    
    cht <- chisq.test(dat_sub[[var1]], dat_sub[[var2]])
    zsres <- cht$stdres
    pstdres <- pnorm(1 - abs(cht$stdres))
    
    probtab <- prop.table(table(dat_sub[[var1]], dat_sub[[var2]]))
    expected_probs <- cbind(colSums(probtab)) %*% rowSums(probtab)
    n_cases <- nrow(dat_sub)
    expected_freq <- expected_probs * n_cases
    
    if (any(expected_freq < 5)) {
      fisher <- c(fisher, TRUE)
      set.seed(2)
      this_res <- fisher.test(dat_sub[[var1]], dat_sub[[var2]],
                              simulate.p.value = TRUE,
                              workspace = 2e7,
                              B = fisher_replicates)
      p_res <- c(p_res, this_res$p.value)
    } else {
      fisher <- c(fisher, FALSE)
      print(cht)
      print(cht$p.value <= sig)
      p_res <- c(p_res, cht$p.value)
    }
    
    if (length(unique(dat_sub[[var1]])) == length(unique(dat_sub[[var2]]))) {
      print("Highest agreement:")
      print(find_highest_agreement_flexible(dat_sub, var1, var2, "percentage"))
    }
    
    print("===========================================================================")
  }
}

# Summarise results
fisher_data <- data.frame(test_a = test_a,
                          test_b = test_b,
                          p_value = p_res)

fisher_data$p_value_rounded <- round(fisher_data$p_value, 4)

fisher_data$test_a <- factor(fisher_data$test_a,
                             levels = 1:6,
                             labels = class_var_names)
fisher_data$test_b <- factor(fisher_data$test_b,
                             levels = 1:6,
                             labels = class_var_names)

fisher_data$sig <- fisher_data$p_value < sig

save(fisher_data, file = paste(tab_path, "fisher_tests.Rda", sep = "/"))
