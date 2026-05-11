# ==============================================================================
# === 10_prepare_figures_for_publication =======================================
# ==============================================================================

# Author: Ann-Kathrin Knak
# Checked by: ------
# Last update: 09.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: Format the plots for publication


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
library(scales)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)
if (!require(Unicode)) install.packages("Unicode")
library(Unicode)
if (!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

# 3. Set working directory & path variables
setwd("C:/Users/alre2380/Documents/NAPKON_in Schweden/preparing_scripts_for_upload")

dat_path <- "data"
tab_path <- "tables"
mod_path <- "lcmms"
boot_path <- "bootstrap"
plot_path <- "plots"

# 4. Load custom functions
format_env <- new.env()
source("functions/functions_for_formatting.R", local = format_env)
attach(format_env)

# 5. Load variable names and constants
publ_names <- read.csv("variable_names.csv", sep = ";")
# read in colors
class_colors <- read_delim("class_colors.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


# ===

dv_names <- c("ecu_moca_total_score", "tmt_a_strict", "tmt_b",
              "average_rt_100", "mfi_gen", "mfi_phy", "mfi_men", "cog_fun")

other_colors =  c("steelblue3", "lightskyblue", "grey50", "grey70",  "grey95")
grey_mark = "grey35"

row_names = c("1 vs. 2", "1 vs. 3", "1 vs. 4", "1 vs. 5", "1 vs. 6", "1 vs. 7")

italic_p <- u_char_inspect(u_char_from_name("MATHEMATICAL SANS-SERIF ITALIC SMALL P"))["Char"]
italic_n <- u_char_inspect(u_char_from_name("MATHEMATICAL SANS-SERIF ITALIC SMALL N"))["Char"]

plot_width_overall = 8
plot_height_overall = 4


# load the best fits
load(paste0(tab_path, "/best_fit_res_with_boot.Rda"))

# prepare lists for plots for each dv to be arranged later
plot_list_model_overview <- list()
plot_list_suppl_alternatives <- list()
plot_list_suppl_single <- list()
plot_list_forest_plots<- list()
plot_list_dv_titles <- list()

# === Overview over models =====================================================

# figure for main text
best_fit_plots <- list.files(plot_path, "best_fit_multi_class")
best_fit_plots <- best_fit_plots[!grepl("class_[0-9]", best_fit_plots)]

# loop through dvs
for (dv_i in 1:length(dv_names)){
  dv_name = dv_names[dv_i]
  
  # --- identify optimal model specs -------------------------------------------
  best_model_name <- best_fits$best_model_name[grepl(dv_name, best_fits$best_model_name)]
  n_classes = regmatches(best_model_name, regexpr("\\d+", best_model_name))
  if(grepl("lin",best_model_name )){
    shape = "linear"
  } else if(grepl("spl",best_model_name )){
    shape = "spline"
  } else {
    message("Neither 'lin' nor 'spl' were found in best model name")
    shape = NULL
  }
  
  # --- create the pie plot ----------------------------------------------------
  # load the table with the class sizes
  mod_sequence_table <- read_excel(paste0(tab_path, "/modeling_sequence_", dv_name, "_perm.xlsx"))
  # extract class sizes
  class_sizes <- mod_sequence_table[mod_sequence_table$model == best_model_name, grepl("class", names(mod_sequence_table))]
  class_sizes <- as.data.frame(t(class_sizes))
  names(class_sizes) <- "perc"
  class_sizes$class_n <- as.factor(as.numeric(regmatches(row.names(class_sizes), regexpr("\\d+", row.names(class_sizes)))))
  class_sizes <- class_sizes[!is.na(class_sizes$perc),]
  # prepare labels
  class_sizes$label <- paste0(round_with_trailing_zero(class_sizes$perc, 0), "%")

  # add sizes and position for pie pieces
  class_sizes <- class_sizes %>% 
    mutate(csum = rev(cumsum(rev(perc))), 
           pos = perc/2 + lead(csum, 1),
           pos = if_else(is.na(pos), perc/2, pos))
  
  
  
  pie_plot <- ggplot(class_sizes, aes(x = "", y = perc, fill = class_n)) +
    geom_col() +
    coord_polar(theta = "y") + 
    geom_label_repel(data = class_sizes,
                     aes(y = pos, label = label),
                     size = 3, nudge_x = 1, show.legend = FALSE) +
    scale_fill_manual(values = class_colors$light_colors[1:n_classes]) +
    theme_void() +
    theme(legend.position="none", plot.title = element_text(size=10, hjust = 0.5))
  
  # add sample sizes
  samp_sizes <- paste0("Patients ",italic_n, " = ",   best_fits$n_participants[best_fits$best_model_name == best_model_name], "\n", 
                       "Measurements ",italic_n," = ", best_fits$n_assessments[best_fits$best_model_name == best_model_name])
  
  samp_size_p <- ggplot() +
    annotate("text", x = 0, y = 0,xmax = 1,
             size = 3,
             label = samp_sizes, hjust = "left") + theme_void()
  
  
  pie_plot = ggarrange(pie_plot,samp_size_p,
            nrow = 2, ncol = 1,
            heights = c(3,1))
  
  # --- create the bar plot ----------------------------------------------------
  # bar plots for entropy, cohen's kappa and replications
  bar_data <- data.frame(lab = c("Entropy", "Cohen's\n Kappa", "Repli-\ncations"),
                         val = c(as.numeric(mod_sequence_table[mod_sequence_table$model == best_model_name, c("entropy")]),
                                 best_fits$cohens_kappa[best_fits$best_model_name == best_model_name],
                                 best_fits$robustness[best_fits$best_model_name == best_model_name]/100),
                         crit = c(0.67, 0.60, 0.70),val_num = 1:3)
  
  bar_data$fullfilled <- bar_data$val >= bar_data$crit
  bar_data$val_text <- bar_data$val %>% round_with_trailing_zero(2)
  bar_colors <- other_colors[c(4,2)]
  
  bar_plot <- ggplot(data=bar_data, aes(x=val_num, y=val, fill = factor(fullfilled))) +
    geom_bar(stat="identity") +
    geom_segment(aes(x=val_num-0.45,xend=val_num+0.45,y=crit,yend=crit), color = grey_mark, linewidth = 1) +
    annotate("text", x = bar_data$val_num, y=bar_data$crit+0.06, label = bar_data$crit, color = grey_mark, size = 3) +
    annotate("text", x = bar_data$val_num, y=0.06, label = bar_data$val_text, color = "black", size = 3) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), expand = c(0, 0)) + 
    scale_x_continuous(breaks = bar_data$val_num, labels=bar_data$lab) +
    scale_fill_manual(values = bar_colors) + labs(x = NULL, y = NULL) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank(),
          axis.text=element_text(size=7))
  

  # --- load and format best trajectory plot -----------------------------------

  best_fit_plot <- best_fit_plots[grepl(dv_name ,best_fit_plots)]
  # check that there is only one plot for this dv in the folder
  if(length(best_fit_plot) != 1){
    message(paste("There were", length(best_fit_plot), "best fit plots found for the variable", dv_name))
  } else {
    load(paste0(plot_path, "/", best_fit_plot)) # names p
  }
  
  # add formatting
  
  xlab = "Days since positive test"
  ylab = publ_names$ylab[publ_names$internal_name == dv_name]
  ymin = as.numeric(publ_names$ymin[publ_names$internal_name == dv_name])
  ymax = as.numeric(publ_names$ymax[publ_names$internal_name == dv_name])
  yposlab = ymax - (ymax-ymin)/10

  
  if (dv_name != "cog_fun"){
    xlims = c(180,1200) # maybe change to 180?
    xseq = seq(180,1200,150)
    xposlab = 1050
  } else {
    xlims = c(0,400)
    xseq = seq(0,400,100)
    xposlab = 350
  }
  
  if(dv_name == "average_rt_100"){ # for average rt change the scaling of the y axis from hectoseconds to seconds
    rescale = 0.01
  } else {
    rescale = 1
  }
  
  
  p = format_lcmm_plots(p, xlab, ylab, ymin, ymax, xlims, xseq, xposlab, yposlab, shape, n_classes, class_colors$light_colors, class_colors$dark_colors, rescale = rescale)

  best_plot <- p
  
  # --- arrange all these plots together ---------------------------------------
  
  title_plot <- ggplot() +
    annotate("text", x = 0, y = 0, 
             size = 4,
             label = publ_names$publication_name[publ_names$internal_name == dv_name],
             angle = 90) + theme_void()
  
  plot_list_dv_titles[[dv_i]] <- title_plot
  
  dv_summary <- ggarrange(title_plot, pie_plot, bar_plot, best_plot,
                          ncol = 4, nrow = 1,
                          widths = c(0.08,1,1,2),
                          common.legend = TRUE, legend="none")
  
  
  plot_list_model_overview[[dv_i]] <- dv_summary
  
  # save plots for legend
  if(dv_i == 4){
    plot_legend_tests = p
  } else if(dv_i == 7){
    plot_legend_subj = p
  } 

  
  # === Additional plots for supplementary materials ===========================
  # --- single classes ---------------------------------------------------------
  plot_list_trajectory_single_classes <- list()
  
  for(class_i in 1:n_classes){
    # load the plot
    plot_name <- paste0("best_fit_multi_class_", dv_name, "_class_", class_i, ".RData")
    load(paste0(plot_path, "/", plot_name))
    
    # format it
    p = format_lcmm_plots(p, xlab, ylab, ymin, ymax, xlims, xseq, xposlab, yposlab, shape, n_classes, class_colors$light_colors, class_colors$dark_colors, rescale = rescale)
    
    # add it to the list
    plot_list_trajectory_single_classes[[class_i]] <- p
  }
  
  # arrange them
  single_class_plot_text <- paste0("plot_list_trajectory_single_classes[[", 1:n_classes, "]],", collapse = "")
  if(n_classes < 7){
    single_class_plot_text = paste0(single_class_plot_text, paste(rep("NULL", 7-as.numeric(n_classes)), collapse = ","), ",")
  }
  
  eval(parse(text = paste0(
    "suppl_single_summary_",dv_i," <- ggarrange(title_plot,", single_class_plot_text,
    " ncol = 8,nrow =1, widths = c(0.2,1,1,1,1,1,1,1,1), legend='none')")))
  
  
  # --- base model -------------------------------------------------------------
  # load the plot
  plot_name <- paste0("baseline_", dv_name, ".RData")
  load(paste0(plot_path, "/", plot_name))
  
  # format it
  p = format_lcmm_plots(p, xlab, ylab, ymin, ymax, xlims, xseq, xposlab, yposlab, shape, n_classes, class_colors$light_colors, class_colors$dark_colors, rescale = rescale)
  
  # add it to the list
  baseline_plot <- p
  
  # --- lca --------------------------------------------------------------------
  # load the plot
  plot_name <- paste0("best_fit_lca_", dv_name, ".RData")
  load(paste0(plot_path, "/", plot_name))
  
  # format it
  p = format_lcmm_plots(p, xlab, ylab, ymin, ymax, xlims, xseq, xposlab, yposlab, shape, n_classes, class_colors$light_colors, class_colors$dark_colors, rescale = rescale)
  # add it to the list
  lca_plot <- p
  
  suppl_alt_summary <- ggarrange(title_plot,
                             best_plot,
                             lca_plot,
                             baseline_plot,
                             ncol = 4, nrow = 1,
                             widths = c(0.06,1,1,1),
                             common.legend = TRUE, legend="none")
  

  
  
  plot_list_suppl_alternatives[[dv_i]] <- suppl_alt_summary
  
  
  # --- forest plots -----------------------------------------------------------
  
  if(dv_name != "mfi_gen"){
  
    
    # load results from multnomial logistic regression models
    mlm_res <- as.data.frame(read_excel(paste0(tab_path, "/mlm_results_", dv_name, ".xlsx")))
    mlm_res$comp <- row_names[1:nrow(mlm_res)]

    # collect names of predictor variabls
    pred_names <-publ_names$internal_name[unlist(lapply(publ_names$internal_name, function(x) any(grepl(x, names(mlm_res)))))]
    
    # prepare number of decimals to show
    nDec = unlist(strsplit(publ_names$decimals[publ_names$internal_name == dv_name], ","))
    
    # prepare list for forest_plots for all predictors for this dv
    these_forest_plots <- list()
    
    # loop through all predictors and add forest plots to list
    for(pred_nameI in 1:length(pred_names)){
      
      pred_name = pred_names[pred_nameI]
      
      # for the first predictor (lts indicator)
      if(pred_nameI == 1){
        eval(parse(text = paste0("fp  <- ggplot(data=mlm_res,
             aes(x = comp, y = ", pred_name, "_with_pcs, ymin = ", pred_name, "_with_pcs_lower, ymax = ", pred_name, "_with_pcs_upper)) +
            geom_hline(aes(yintercept = 0), linetype=1, color = grey_mark)+
             
          
        geom_pointrange(aes(fill=comp, ymin = ", pred_name, "_with_pcs_lower, ymax = ", pred_name, "_with_pcs_upper), shape = 23, size = 0.8) +
        scale_fill_manual(values = class_colors$dark_colors[2:length(class_colors$dark_colors)]) +
        
                               
          scale_y_continuous(labels = function(x) label_trailing_zero(x,", nDec[pred_nameI],") ,breaks = scales::pretty_breaks(n = 3))")))
        
        
        fp = fp +
          facet_wrap(~mlm_res$comp,strip.position="left",dir = "v", nrow=7,scales = "free_y") +
          theme_classic() +
          coord_flip() +
          ggtitle(publ_names$publication_name[publ_names$internal_name == pred_name])+
          theme(plot.title=element_text(size=12, hjust = 0.5),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.line.y =element_blank(),
                strip.background = element_blank(),
                strip.text.y = element_blank(),
                legend.position="none") +
          xlab(NULL) +
          ylab(NULL)
        
        these_forest_plots[[pred_nameI]] <- fp
        
      # for the other predictors (risk factors)  
      } else {
        eval(parse(text = paste0("fp  <- ggplot(data=mlm_res,
             aes(x = comp, y = ", pred_name, "_without_pcs, ymin = ", pred_name, "_without_pcs_lower, ymax = ", pred_name, "_without_pcs_upper)) +
            geom_hline(aes(yintercept = 0), linetype=1, color = grey_mark)+
             
          
        geom_pointrange( aes(fill=comp, ymin = ", pred_name, "_without_pcs_lower, ymax = ", pred_name, "_without_pcs_upper), shape = 23, size = 0.8) +
        scale_fill_manual(values = class_colors$dark_colors[2:length(class_colors$dark_colors)]) +
        scale_y_continuous(labels = function(x) label_trailing_zero(x,", nDec[pred_nameI],") ,breaks = scales::pretty_breaks(n = 3))")))
        
        
        fp = fp +
          facet_wrap(~mlm_res$comp,strip.position="left",dir = "v", nrow=7,scales = "free_y") +
          theme_classic() +
          coord_flip() +
          ggtitle(publ_names$publication_name[publ_names$internal_name == pred_name])+
          theme(plot.title=element_text(size=12, hjust = 0.5),
                
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.line.y =element_blank(),
                strip.background = element_blank(),
                strip.text.y = element_blank(),
                legend.position="none") +
          xlab(NULL) +
          ylab(NULL) +
          geom_hline(yintercept=mlm_res[[paste0(pred_name, "_with_pcs")]], linetype=1, 
                     color = grey_mark, size=1)
        
        these_forest_plots[[pred_nameI]] <- fp
      }
    }
    # arrange the forest plots for this dependent variable
    plot_list_forest_plots[[dv_i]] <- ggarrange(these_forest_plots[[1]],NULL,
                                                          these_forest_plots[[2]],
                                                          these_forest_plots[[3]],
                                                          these_forest_plots[[4]],
                                                          these_forest_plots[[5]],
                                                          these_forest_plots[[6]],
                                                          ncol = 7, nrow = 1, widths = c(1,0.2,1,1,1,1,1,1))#,
  }

  
}

# === Arrange plots ============================================================


# --- overall plot -------------------------------------------------------------

pie_title <- ggplot() +
  annotate("text", x = 0, y = 0, 
           size = 4,
           label = "Class sizes") + theme_void()

bar_title <- ggplot() +
  annotate("text", x = 0, y = 0, 
           size = 4,
           label = "Criteria") + theme_void()

trajectory_title <- ggplot() +
  annotate("text", x = 0, y = 0, 
           size = 4,
           label = "Trajectories") + theme_void()


col_titles <- ggarrange(NULL, pie_title, bar_title, trajectory_title,
                        ncol = 4, nrow = 1,
                        widths = c(0.08,1,1,2))


model_overview <- ggarrange(col_titles,
                            NULL,
                            plot_list_model_overview[[1]],
                            NULL,
                            plot_list_model_overview[[2]],
                            NULL,
                            plot_list_model_overview[[3]],
                            NULL,
                            plot_list_model_overview[[4]],
                            NULL,
                            ncol = 1, nrow =10,
                            heights = c(0.2, 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1,0.1),
                            common.legend = TRUE, legend="bottom", legend.grob = get_legend(plot_legend_tests))

model_overview  = model_overview +
  geom_hline(yintercept=0.05, color = grey_mark) +
  geom_hline(yintercept=0.275, color = grey_mark) +
  geom_hline(yintercept=0.5, color = grey_mark) +
  geom_hline(yintercept=0.725, color = grey_mark) + 
  geom_hline(yintercept=0.95, color = grey_mark) 

ggsave(
  filename = paste0(plot_path, "/model_overview_tests.png"),
  plot = model_overview,
  width = 16, height = 22,
  units = "cm"
)



model_overview <- ggarrange(col_titles,
                            NULL,
                            plot_list_model_overview[[5]],
                            NULL,
                            plot_list_model_overview[[6]],
                            NULL,
                            plot_list_model_overview[[7]],
                            NULL,
                            plot_list_model_overview[[8]],
                            NULL,
                            ncol = 1, nrow =10,
                            heights = c(0.2, 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1,0.1),
                            common.legend = TRUE, legend="bottom", legend.grob = get_legend(plot_legend_subj))

model_overview  = model_overview +
  geom_hline(yintercept=0.05, color = grey_mark) +
  geom_hline(yintercept=0.275, color = grey_mark) +
  geom_hline(yintercept=0.5, color = grey_mark) +
  geom_hline(yintercept=0.725, color = grey_mark) + 
  geom_hline(yintercept=0.95, color = grey_mark) 


ggsave(
  filename = paste0(plot_path, "/model_overview_subj.png"),
  plot = model_overview,
  width = 16, height = 22,
  units = "cm"
)

# --- supplementaries alternative models ---------------------------------------


final_title <- ggplot() +
  annotate("text", x = 0, y = 0, 
           size = 4,
           label = "Final selected GMM model") + theme_void()

lca_title  <- ggplot() +
  annotate("text", x = 0, y = 0, 
           size = 4,
           label = "Corresponding LCGA model") + theme_void()

base_title  <- ggplot() +
  annotate("text", x = 0, y = 0, 
           size = 4,
           label = "Single-Class GMM model") + theme_void()


col_titles <- ggarrange(NULL, final_title, lca_title, base_title,
                        ncol = 4, nrow = 1,
                        widths = c(0.06,1,1,1))


alt_models_overview <- ggarrange(col_titles,
                                 NULL,
                                 plot_list_suppl_alternatives[[1]],
                                 NULL,
                                 plot_list_suppl_alternatives[[2]],
                                 NULL,
                                 plot_list_suppl_alternatives[[3]],
                                 NULL,
                                 plot_list_suppl_alternatives[[4]],
                                 NULL,
                                 ncol = 1, nrow =10,
                                 heights = c(0.2, 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1,0.1),
                                 common.legend = TRUE, legend="bottom", legend.grob = get_legend(plot_legend_tests))

alt_models_overview = 
  alt_models_overview +
  geom_hline(yintercept=0.05, color = grey_mark) +
  geom_hline(yintercept=0.275, color = grey_mark) +
  geom_hline(yintercept=0.5, color = grey_mark) +
  geom_hline(yintercept=0.725, color = grey_mark) + 
  geom_hline(yintercept=0.95, color = grey_mark) 


ggsave(
  filename = paste0(plot_path, "/alt_models_overview_tests.png"),
  plot = alt_models_overview,
  width = 16, height = 22,
  units = "cm"
)



alt_models_overview <- ggarrange(col_titles,
                                 NULL,
                                 plot_list_suppl_alternatives[[5]],
                                 NULL,
                                 plot_list_suppl_alternatives[[6]],
                                 NULL,
                                 plot_list_suppl_alternatives[[7]],
                                 NULL,
                                 plot_list_suppl_alternatives[[8]],
                                 NULL,
                                 ncol = 1, nrow =10,
                                 heights = c(0.2, 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1,0.1),
                                 common.legend = TRUE, legend="bottom", legend.grob = get_legend(plot_legend_subj))

alt_models_overview = 
  alt_models_overview +
  geom_hline(yintercept=0.05, color = grey_mark) +
  geom_hline(yintercept=0.275, color = grey_mark) +
  geom_hline(yintercept=0.5, color = grey_mark) +
  geom_hline(yintercept=0.725, color = grey_mark) + 
  geom_hline(yintercept=0.95, color = grey_mark) 


ggsave(
  filename = paste0(plot_path, "/alt_models_overview_subj.png"),
  plot = alt_models_overview,
  width = 16, height = 22,
  units = "cm"
)


# --- supplementaries class by class -------------------------------------------

single_class_overview <- ggarrange(suppl_single_summary_1,
                                   suppl_single_summary_2,
                                   suppl_single_summary_3,
                                   suppl_single_summary_4,
                                   ncol = 1, nrow =4,
                                   common.legend = TRUE, legend="bottom", legend.grob = get_legend(plot_legend_tests))


single_class_overview  = 
  single_class_overview  +
  geom_hline(yintercept=0.065,color = grey_mark) +
  
  geom_hline(yintercept=0.3, color = grey_mark) +
  geom_hline(yintercept=0.535, color = grey_mark) + 
  geom_hline(yintercept=0.77, color = grey_mark) 


ggsave(
  filename = paste0(plot_path, "/single_classes_tests.png"),
  plot = single_class_overview,
  width = 28, height = 14,
  units = "cm"
)


single_class_overview <- ggarrange(suppl_single_summary_5,
                                   suppl_single_summary_6,
                                   suppl_single_summary_7,
                                   suppl_single_summary_8,
                                   ncol = 1, nrow =4,
                                   common.legend = TRUE, legend="bottom", legend.grob = get_legend(plot_legend_subj))


single_class_overview  = 
  single_class_overview  +
  geom_hline(yintercept=0.065,color = grey_mark) +
  
  geom_hline(yintercept=0.3, color = grey_mark) +
  geom_hline(yintercept=0.535, color = grey_mark) + 
  geom_hline(yintercept=0.77, color = grey_mark) 


ggsave(
  filename = paste0(plot_path, "/single_classes_subj.png"),
  plot = single_class_overview,
  width = 28, height = 14,
  units = "cm"
)


# === Fisher's exact test results ==============================================

# load fisher table
load(paste(tab_path, "fisher_tests.Rda", sep = "/"))
fisher_data$p_value_rounded <- round(fisher_data$p_value, 4)
fisher_data$color<- cut(fisher_data$p_value, breaks = c(-Inf, 0.000100,  0.003333, 0.01, 0.100000, Inf),
                     labels = c( "< 0.0001",   "< 0.0033", "< 0.01", "< 0.1", "≥ 0.1"))
# instead relabel - I already used that somewhere!

publ_names_dv <- publ_names[1:7,]
levels(fisher_data$test_a) <- unlist(lapply(levels(fisher_data$test_a), function(x) {
  publ_names_dv$publication_name[unlist(lapply(publ_names_dv$internal_name, function(y) grepl(y,x)))]}))

levels(fisher_data$test_b) <- unlist(lapply(levels(fisher_data$test_b), function(x) {
  publ_names_dv$publication_name[unlist(lapply(publ_names_dv$internal_name, function(y) grepl(y,x)))]}))


grepl(publ_names$internal_name[1],fisher_data$test_a)
lapply(publ_names$internal_name, function(x) grepl(x, fisher_data$test_a))
fisher_data$test_a

fisher_plot <- ggplot(fisher_data, aes(test_a, test_b, fill = color)) + 
  geom_tile(color = "white", lwd = 3)+
  scale_fill_manual(breaks = levels(fisher_data$color),
                    values = other_colors, name=paste0(italic_p, "-value")) +
  geom_text(aes(test_a, test_b, label = format(p_value_rounded, scientific = FALSE)), 
            color = "black", size = 4) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  xlab(NULL) +
  ylab(NULL) +
  geom_hline(yintercept=3.5, linetype=2, 
             color = grey_mark, size=1)+
  geom_vline(xintercept=4.5, linetype=2, 
             color = grey_mark, size=1)+
  geom_text(aes(3, 1, label = "Cognitive Tests"), color = grey_mark, size = 4) +
  geom_text(aes(5.2, 4, label = "Fatigue"), color = grey_mark, size = 4) +
  coord_cartesian(xlim = c(1, 5.2))



ggsave(
  filename = paste0(plot_path, "/fisher_plot.png"),
  plot = fisher_plot,
  width = 7, height = 5)                                                                                                                   



# === MLM results ==============================================================

# --- Prepare a plot for the legend --------------------------------------------
legend_dat <- as.data.frame(matrix(c(1,2,3,1,2,3, 0.5, 0.5, 0.5, 0,0,0, 1,2,3,4,5,6), nrow = 6, ncol = 3))
legend_dat$V3 <- as.factor(legend_dat$V3)
legend_dat$alpha <- as.factor(c(1,1,1,1,1,1))

x_base = 0.3
x_add = 1
route_legend <- ggplot(legend_dat, aes(x=V1,y=V2)) + geom_point(aes(fill= V3,  color = "black"), shape = 23, size = 4) +
  scale_fill_manual(values = c(class_colors$dark_colors[2:length(class_colors$dark_colors)], "white")) +
  
  scale_color_manual(values = c( "black", "white")) +
  theme_void() +
  theme(plot.title=element_text(size=14, hjust = 0.5),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.line.y =element_blank(),
        axis.line.x =element_blank(),
        legend.position="none") +
  annotate("text", x = x_base + x_add, y = 0.5, label = "1 vs. 2")  +
  annotate("text", x = x_base + x_add*2, y = 0.5, label = "1 vs. 3")  +
  annotate("text", x = x_base + x_add*3, y = 0.5, label = "1 vs. 4")  +
  annotate("text", x = x_base + x_add, y = 0, label = "1 vs. 5")  +
  annotate("text", x = x_base + x_add*2, y = 0, label = "1 vs. 6")  +
  annotate("text", x = x_base + x_add*3, y = 0, label = "1 vs. 7")  +
  ggtitle("Compared classes") +
  ylim(-0.2,0.8) +
  xlim(0.75, 3.75)

legend_dat$V1 <- as.numeric(legend_dat$V1)
line_legend <- ggplot(legend_dat[1,],  aes(x=V1, y = V2))+
  geom_point(fill= "white",  color = "black", shape = 23, size = 4) +
  geom_segment(aes(x = 1, y = -0.1, xend = 1, yend = 0.1), color = grey_mark) +
  ggtitle("Estimated regression weight") +
  annotate("text", x = 1.4, y = 0.5, label = "LTS indicator included")  +
  annotate("text", x = 1.45, y = 0, label = "LTS indicator not included")  +
  xlim(0.75,2) +
  ylim(-0.2,0.8) +
  theme_void() +
  theme(plot.title=element_text(size=14, hjust = 0.5),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.line.y =element_blank(),
        axis.line.x =element_blank(),
        legend.position="none")

fp_legend <- ggarrange(NULL,line_legend, NULL, route_legend,NULL, ncol = 5, nrow = 1, widths = c(0.01,1,0.05,1,0.2))  

# --- arrange forest plots and dv_names ---------------------------------------- 
add_per_test = 0.3
base_height = 0.5
gap_height = 0.1

fp_total_plot <- ggarrange(
  plot_list_dv_titles[[1]], plot_list_forest_plots[[1]], 
  NULL, NULL,
  plot_list_dv_titles[[2]], plot_list_forest_plots[[2]],
  NULL, NULL,
  plot_list_dv_titles[[3]], plot_list_forest_plots[[3]],
  NULL, NULL,
  plot_list_dv_titles[[4]], plot_list_forest_plots[[4]],
  NULL, NULL, 
  plot_list_dv_titles[[6]], plot_list_forest_plots[[6]],
  NULL, NULL,
  plot_list_dv_titles[[7]], plot_list_forest_plots[[7]],
  NULL, NULL,
  plot_list_dv_titles[[8]], plot_list_forest_plots[[8]],
  
  
  ncol = 2, nrow = 13,
  heights = c(base_height  + 1*add_per_test,
              gap_height,base_height  + 1*add_per_test,
              gap_height,base_height  + 2*add_per_test,
              gap_height,base_height  + 2*add_per_test,
              gap_height,base_height  + 4*add_per_test,
              gap_height,base_height  + 6*add_per_test,
              gap_height,base_height  + 5*add_per_test
  ),
  widths = c(0.05, 1))

# --- add headings and legend --------------------------------------------------

pred_title_lts <- ggplot() +
  annotate("text", x = 0, y = 0, 
           size = 5,
           label = "LTS indicator") + theme_void()
pred_title_risk <- ggplot() +
  annotate("text", x = 0, y = 0, 
           size = 5,
           label = "All potential risk factors") + theme_void()

fp_title <- ggarrange(NULL,pred_title_lts, pred_title_risk,
                                nrow = 1,ncol = 3,
                                widths = c(0.15,1,4))

fp_total_plot <- ggarrange(fp_title,fp_total_plot,NULL, fp_legend,
                                     nrow = 4, ncol = 1,
                                     heights = c(0.75,10,0.5, 1))
fp_total_plot <- fp_total_plot +
  geom_segment(aes(x = 0.05, y = 0.95, xend = 0.2, yend = 0.95), color = "black") +
  geom_segment(aes(x = 0.25, y = 0.95, xend =1, yend = 0.95), color = "black")


ggsave(
  filename = paste0(plot_path, "/log_regression.png"),
  plot = fp_total_plot,
  width = 24, height = 33,
  units = "cm"
)