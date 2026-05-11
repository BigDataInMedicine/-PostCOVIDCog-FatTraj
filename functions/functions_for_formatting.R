# ==============================================================================
# === CUSTOM FUNCTIONS for Formatting of tables and plots ======================
# ==============================================================================

# Authors: Ann-Kathrin Knak
# Last update: 08.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: These are the custom functions that we created for formatting of tables and plots

# === Functions ====================================================
#' round numbers and pad with zeros
#' @description
#' `round_with_trailing_zero` returns a vector of strings with rounded numbers with trailing zeros to standardize number of decimals
#' @param vec vector of values to be rounded
#' @param nDig number of decimal digits
round_with_trailing_zero <- function(vec, nDig){
  
  helper <- function(n, nDig = 2){
    as_num <- as.numeric(n)
    
    if(!is.na(as_num)){
      as_num <- round(as_num, nDig)
      return(sprintf(paste0("%.", nDig,"f"), as_num))
    } else{
      return(n)
    }
  }
  
  return(unlist(lapply(vec, function(x) helper(x,nDig))))
}

#' add trailing zero to labels
#' @description
#' `label_trailing_zero` adds trailing zero to labels for plot scales
#' @param l labels
#' @param max.decimals maximal number of decimals
label_trailing_zero <- function(l, max.decimals){
  lnew = formatC(l, replace.zero = T, zero.print = "0",
                 digits = max.decimals, format = "f", preserve.width=T)
  return(lnew)
}

#' format lcmm plots
#' @description
#' `format_lcmm_plots` refines the formatting of lcmm plots
#' @param p ggplot object to be reformatted
#' @param xlab label on x axis
#' @param ylab label on y axis
#' @param yMin minimal value y axis
#' @param yMax maximal value y axis
#' @param xLims limits x axis
#' @param xSeq sequence of ticks on the x axis
#' @param xPosLab x position of the label indicating spline/linear
#' @param yPosLab y position of the label indicating spline/linear
#' @param funShape whether label should indicate spline or linear
#' @param nClasses number of classes
#' @param light_colors colors to use for individual trajectories
#' @param dark_colors colors to use for group trajectories
#' @param rescale rescaling factor of y axis
format_lcmm_plots <- function(p, xlab, ylab, yMin, yMax, xLims, xSeq, xPosLab, yPosLab, funShape, nClasses, light_colors, dark_colors, rescale = 1){
  
  # text for colors and legends
  scale_text <- "scale_colour_manual(values = c("
  scale_text <- paste0(scale_text,
                       paste("'Individuals class ", 1:nClasses, "' = light_colors[",1:nClasses,"]", sep = "", collapse = ", "))
  scale_text <- paste0(scale_text,", ")
  scale_text <- paste0(scale_text,
                       paste("'Class ", 1:nClasses, "' = dark_colors[",1:nClasses,"]",sep = "", collapse = ", "))
  
  scale_text <- paste0(scale_text,"), breaks = c(")
  scale_text <- paste0(scale_text, paste0("'Class ", 1:nClasses,"'", collapse = ", "))
  scale_text <- paste0(scale_text,"))")

  p = p +
    eval(parse(text = scale_text))+
    labs(x = xlab, y = ylab, color = "Average Trajectories") +
    theme_bw() +
    scale_y_continuous( labels = label_number(scale = rescale)) +
    ylim(yMin, yMax) +
    theme(plot.title = element_text(size=10, hjust = 0.5),
          axis.title=element_text(size=8),
          axis.text=element_text(size=6),
          legend.title = element_text(hjust = 0.5))+
    
    scale_x_continuous(limits = xLims, breaks=xSeq) + 
    guides(colour = guide_legend(ncol = as.numeric(nClasses))) +
    scale_x_continuous(limits = xLims, breaks=xSeq) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())+
    annotate("label", x = xPosLab,
             y = yPosLab,
             label = funShape, label.r = unit(0, "pt"))
  
  return(p)
}