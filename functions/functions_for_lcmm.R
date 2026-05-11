# ==============================================================================
# === CUSTOM FUNCTIONS for LCMM inspection and presentation of results =========
# ==============================================================================

# Authors: Ann-Kathrin Knak
# Last update: 04.05.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: These are the custom functions that we created for our analysis

# === Model estimation =========================================================

#' Gridsearch with additional diagnostic information
#' Adapted from Proust-Lima, C., & Philipps, V. (2010). lcmm: Extended Mixed Models Using Latent Classes and Latent Processes (p. 2.2.2) [Dataset]. https://doi.org/10.32614/CRAN.package.lcmm
#' @description
#' `gridsearch_with_diagnostics` performs the same gridsearch as "gridsearch" from lcmm package, but in addition to the model returns diagnostic information
#' @returns c("model", "rep", "kmax", "loglik", "conv", "%class", "coefs", "loglikrep", "nConv", "opt0class" )
#'  "model" is the final model estimated in the gridsearch
#'  "rep" is the number of replications set for the gridsearch
#'  "kmax"indicates the model with the lowest loglikelihood
#'  "loglik" vector with the loglikelihood for each of the models estimated during the gridsearch
#'  "conv" vector with the loglikelihood for each of the models estimated during the gridsearch
#'  "%class" matrix of class sizes (percent) where rows equal classes and columns the models estimated during the gridsearch
#'  "loglikrep" number of models with the same loglikelihood as the optimal solution (rounded to four digits)
#'  "nConv" number of models that converged
#'  "opt0class": whether or not the optimal solution had at least one class with 0 people
#' @param show: already print diagnostics during estimation procedure
gridsearch_with_diagnostics <- function (m, rep, maxiter, minit, cl = NULL, show = TRUE) 
{
  mc <- match.call()$m
  mc$maxiter <- maxiter
  models <- vector(mode = "list", length = rep)
  assign("minit", eval(minit))
  if (minit$conv != 1) 
    stop("The model minit did not converge")
  ncl <- NULL
  if (!is.null(cl)) {
    if (!inherits(cl, "cluster")) {
      if (!is.numeric(cl)) 
        stop("argument cl should be either a cluster or a numeric value indicating the number of cores")
      ncl <- cl
      cl <- makeCluster(ncl)
    }
    clusterSetRNGStream(cl)
    if (mc[[1]] == "mpjlcmm") {
      for (k in 2:length(mc[[2]])) {
        clusterExport(cl, list(as.character(mc[[2]][k])))
      }
    }
    clusterExport(cl, list("mc", "maxiter", "minit", as.character(as.list(mc[-1])$data)), 
                  envir = environment())
    pck <- .packages()
    dir0 <- find.package()
    dir <- sapply(1:length(pck), function(k) {
      gsub(pck[k], "", dir0[k])
    })
    clusterExport(cl, list("pck", "dir"), envir = environment())
    clusterEvalQ(cl, sapply(1:length(pck), function(k) {
      require(pck[k], lib.loc = dir[k], character.only = TRUE)
    }))
    cat("Be patient, grid search is running ...\n")
    models <- parLapply(cl, 1:rep, function(X) {
      mc$B <- substitute(random(minit), parent.frame(n = 2))
      return(do.call(as.character(mc[[1]]), as.list(mc[-1])))
    })
    cat("Search completed, performing final estimation\n")
    if (!is.null(ncl)) 
      stopCluster(cl)
  }
  else {
    for (k in 1:rep) {
      mc$B <- substitute(random(minit), environment())
      models[[k]] <- do.call(as.character(mc[[1]]), as.list(mc[-1]), 
                             envir = parent.frame())
    }
  }
  llmodels <- sapply(models, function(x) {
    return(x$loglik)
  })
  # ADDED-----------------------------------------------------------------------
  convModels <- sapply(models, function(x) {
    return(x$conv)
  })
  classModels <- sapply(models, function(x) {
    return(summarytable(x, which = "%class", display = FALSE))
  })
  
  coefs <-  sapply(models, function(x) x[["best"]])
  coefs <- as.data.frame(matrix( unlist(coefs),nrow = rep, byrow = TRUE))
  names(coefs) <- names(models[[1]][["best"]])
  
  kmax <- which.max(llmodels)
  
  logliks <- round(llmodels,4)
  #-----------------------------------------------------------------------------
  
  mc$B <- models[[kmax]]$best
  mc$maxiter <- match.call()$m$maxiter
  
  # ADDED ----------------------------------------------------------------------
  if (show){
    print(paste("Max Likelihood was replicated ", sum(logliks == max(logliks)), "/", length(logliks), " times", sep = ""))
    print(paste(sum(convModels == 1), "/", length(convModels), " converged", sep = ""))
    print(paste(sum(colSums(classModels == 0) >= 1), "/", ncol(classModels), " had at least one class with 0 members", sep = ""))
    if(sum(classModels[,kmax] == 0) >= 1){
      print("The optimal solution had at least one class with 0 members")
    }
  }
  #-----------------------------------------------------------------------------
  
  
  mod <- tryCatch({
    do.call(as.character(mc[[1]]), as.list(mc[-1]), envir = parent.frame())},
    error = function(x){
      print(warning(as.character(x)))
      return(as.character(x))
    })
  
  # ADAPTED --------------------------------------------------------------------
  output <- list(mod,rep, kmax,
                 llmodels,convModels,classModels, coefs,
                 sum(logliks == max(logliks)), # how often loglik was replicated
                 sum(convModels == 1), # how often model converged
                 sum(classModels[,kmax] == 0) >= 1 # whether or not the optimal solution had at least one class with 0 people
  )
  names(output) <- c("model", "rep", "kmax", "loglik", "conv", "%class", "coefs", "loglikrep", "nConv", "opt0class" )
  #-----------------------------------------------------------------------------
  return(output)
}


#' Estimating multiple numbers of classes for one model type
#' @description
#' `lcmm_pipeline` estimates multiple models using the lcmm function with individual numbers of classes, interations and replications
#' @param data: string with name the dataset that is used for estimation
#' @param DV: name of the dependent variable
#' @param IV: name of the independent variable (usually time)
#' @param subject: numerical variable indicating the individuals
#' @param maxIterPerClass: vector with the number of maximal iterations for the model estimation per model
#' @param repPerClass: vector with the number of maximal replications for the gridsearch of each model estimation
#' @param fun: name of the estimation algorithm to be used; has only been testsed with "lcmm" but technically migh also work with "hlme" or similar functions
#' @param shape: shape of the fitted function; "lin" for linear or "spl" for three-node equidistant spline
#' @param type: latent class growth curve ("lca") or growth mixture model ("gmm")
#' @param B: start values for model estimation; default = NULL
#' @param minit: baseline model (1 class); default = NULL
#' @param cl: number of chores used for estimation
lcmm_pipeline <- function(data, DV, IV, subject, maxIterPerClass, repPerClass, fun, shape, type, B = NULL, minit = NULL, cl){
  start.time <- Sys.time()
  
  # collecting base parts of all models
  model_parts <- c()
  model_parts <- c(model_parts, paste(fun, "(", DV,
                                      " ~ ",IV,",  subject = '", subject,
                                      "',data = ", data, sep = ""))
  if (type == "gmm"){
    model_parts <- c(model_parts, paste(", random = ~1 + ", IV, sep = ""))
  }
  if (shape == "spl"){
    model_parts <- c(model_parts, paste(", link = '3-equi-splines'", sep = ""))
  }

  # estimating 1 class model in case not part of the arguments
  if (is.null(minit)){
    print("--- Estimating Model with 1 Class ----------------")
    mod_name <- paste(type, shape, fun, 1,"_", DV, sep = "")
    mod_names <- c(mod_name)
    
    if (is.null(B)){
      txt <- paste(mod_name , " <<- " , # model name
                   paste(model_parts, collapse = ""), # base parts of model definition
                   ", maxiter =", maxIterPerClass[1], # max iterations
                   ")", sep = "")
    } else {
      txt <- paste(mod_name , " <<- " , # model name
                   paste(model_parts, collapse = ""), # base parts of model definition
                   ", maxiter =", maxIterPerClass[1], # max iterations
                   ", B = ", B, # start values
                   ")",
                   sep = "")
    }
    txt <- gsub("\n", "",txt)
    print(txt)
    eval(parse(text = txt))
    print(paste("Estimation finished: ",Sys.time() - start.time ))
    minit <- eval(parse(text = mod_name))
    
  } else {
    print("Model with 1 Class is not estimated. Minit is used instead.")
    mod_names <- c()
  }
  
  # estimating the models with more classes
  model_parts <- c(model_parts, paste(", mixture = ~", IV, sep = ""))
  
  for (classN in 2:length(maxIterPerClass)){
    if(is.na(repPerClass[classN]) |repPerClass[classN] ==0){
      print(paste(" --- Model with", classN,  "Classes  not estimated due to", repPerClass[classN], "repetitions. ---"))
    } else {
      mod_name <- paste(type, shape, fun,classN, "_", DV, sep = "")
      mod_names <- c(mod_names, mod_name)
      print(paste("--- Estimating Model with", classN,  "Classes----------------"))
      txt <- paste(mod_name, " <<- ", # model name
                   "gridsearch_with_diagnostics(show = FALSE, rep =", repPerClass[classN], # gridsearch_with_diagnostics, repetitions
                   ", maxiter = ", maxIterPerClass[classN], # iterations per repetition
                   ", minit =  minit", # baseline model
                   ", cl = ", cl, # number of cors
                   ", m = ", # inner function
                   paste(model_parts, collapse = ""), # base parts of model definition
                   ", ng = ", classN, # number of classes
                   "))",
                   sep = "")
      txt <- gsub("\n", "",txt)
      print(txt)
      eval(parse(text = txt))
      print(paste("Estimation finished: ",Sys.time() - start.time ))
    }
  }
  return(mod_names)
}

# === Model inspection =========================================================

#' Compare model coefficients
#' @description
#' `compare_coefs` uses output from gridsearch_with_diagnotics to compare the coefficients of the models with the highest loglik
#' @returns a dataframe with the absolute differences of the selected coefficients optionally divided by vSD
#' @param mod: object results from gridsearch_with_diagnostics
#' @param n: number of models to be compared (e.g., 3 -> the three models with the highest loglik will be compared)
#' @param coef_type: name(s) of coefficient(s) to be compared
#' @param vSD: value by which the results should be divided; default = 1; stadard deviation of the dependent variable is suggested as a way of scaling this value
#' 
compare_coefs <- function(mod, n, coef_type, vSD = 1){
  
  maxLogLikI <- which.max(mod$loglik)# select the index of the highest loglik
  maxLogLiksI <- which(mod$loglik %in% sort(mod$loglik, decreasing = TRUE)[2:n])# select the index of the three highest loglik values
  coefs <- mod$coefs[c(maxLogLikI,maxLogLiksI), ]# select the coefficients of the three columns with the max loglik
  cols <- which(grepl(coef_type, names(coefs)))# select only the coefficient type of interest
  res <- data.frame(nothing = 0) # prepare data frame
  
  # loop through the 2:n models and compare them to the model with best loglik
  for (rowI in 2:n){
    allDiffs <-  coefs[1, cols] - coefs[ rowI, cols] # difference of coefficients
    vNames <- paste0(names(coefs)[cols], 'loglik_1_vs_', rowI) # name for these outputs
    for (vNameI in 1:length(vNames)){
      res[[vNames[vNameI]]] = allDiffs[vNameI] # add to res data.frame
    }
  }
  res <- res[, names(res) != c("nothing")] # delete column "nothing"
  res <- abs(res/vSD) # take absolute value and divide by vSD
  return(res)
}

#' Find model names
#' @description
#' `find_mod_names` finds all variables in global environment that contain specific string pattern
#' @param ...: patterns
find_mod_names <- function(...){
  envObj <- ls(envir=.GlobalEnv)
  patterns <- list(...)
  sel <- rep(TRUE, length(envObj))
  for (patternI in 1:length(patterns))
  {
    pattern <- patterns[[patternI]]
    if(substr(pattern,1,1) == "!"){
      pattern <- substr(pattern,2,nchar(pattern))
      sel <- sel &  !grepl(pattern,envObj)
    } else {
      sel <- sel &  grepl(pattern,envObj)
    }
  }
  return(envObj[sel])
}

#' Extract model
#' @description
#' `extract_mod` is used with results from gridsearch_with_diagnostics function to extract the model that comes from the original gridsearch function
extract_mod <- function(mod){
  if("model" %in% names(mod)){
    return(mod$model)
  } else {
    return(mod)
  }
  
}

#' Summary table all models
#' @description
#' `summarytable_all_models` applies summarytable function from lcmm package to multiple models, generated by gridsearch_with_diagnostics
#' @param ...: string patterns to be used to select models from global environment
#' @param which: values to be summarized, see summarytable lcmm package
#' @param dataframe: boolean value indicating wheter summarytable should be a datafrane; default = FALSE
#' 
summarytable_all_models <- function(..., which, dataframe = FALSE){

  modNames <- find_mod_names(...)
  modNamesValid <- c()
  sumTab <- data.frame()
  
  for (modI in 1:length(modNames)){
    eval(parse(text = paste(modNames[modI], " <- extract_mod(", modNames[modI], ")", sep = "")))
    if(eval(parse(text = paste("class(", modNames[modI], ") != 'character'", sep = "")))){
      eval(parse(text = paste("sumTab <- dplyr::bind_rows(sumTab,",  
                              "as.data.frame(summarytable(",modNames[modI], 
                              ", which =  c(", paste(sQuote(which, q = FALSE), collapse = ","), 
                              "), display = FALSE)))", sep = "")))
    }
  }
  if (dataframe){
    sumTab <- as.data.frame(sumTab)
    sumTab$model <- rownames(sumTab)
    sumTab <- sumTab[, c(ncol(sumTab), 1:(ncol(sumTab)-1))]
  }
  return(sumTab)
}

#' Add gridsearch info to table
#' @description
#' `add_gridsearch_info_to_tab` adds gridsearch diagnostics to the summarytable generated by summarytable_all_models
#' @param tab: summarytable to be extended
#' @param variables: diagnostic values to be added to the table
#' 
add_gridsearch_info_to_tab <- function(tab, variables){
  modNames <- tab$model # find model names in table
  for(modI in 1:length(modNames)){ # loop through model names
    modName <- modNames[modI]
    mod <- eval(parse(text = modName)) # find model in environment
    if(length(mod$model) == 0){
      next
    } else {
      addDat <- unlist(mod[variables]) # extract diagnostic information from model
      lapply(names(addDat), function(x) {# if columns do not exist yet, create them
        if(!(x %in% names(tab))){tab[[x]] <<- NA}
      })
      tab[modI, names(addDat)] <- addDat #add the data to the table
    }
  }
  return(tab)
}

#' Check gridsearch table
#' @description
#' `check_gridsearch_tab` prints relevant information from the table containing gridsearch diagnostics to evlaute if the estimation procedure has to be adapted
#' @param tab: table containing gridsearch diagnostics
#' @param critConv: critical number of models that should have converged
#' @param critLoglik: ciritical number of models that should replicate loglik
#' 
check_gridsearch_tab <- function(tab, critConv, critLoglik){
  print("Not enough models in the gridsearch converged:")
  print(tab[tab$nConv < critConv & !is.na(tab$nConv), c("model", "nConv", "rep")])
  cat("\n")
  print("Not enough models in the gridsearch were replicated:")
  print(tab[tab$loglikrep < critLoglik & !is.na(tab$loglikrep), c("model", "loglikrep", "rep",names(tab)[grepl("coefComp", names(tab))])])
  # prints als coefComp because sometimes loglik is not the same, even though the coefficients are still very similar, so it can be assumed that the models are similar enough
}

#' Mark exclusion criteria in table
#' @description
#' `mark_exclusion_criteria_in_tab` adds columns to a table that indicate whether rows should be excluded depending on specified criteria
#' @param tab: table containing values for exclusion criteria
#' @param exclNames: column names of exclusion criteria
#' @param exclVals: conditions under which values should be excluded, e.g., c("!=1", "<0.67", "<2")
#' 
mark_exclusion_criteria_in_tab <- function(tab, exclNames, exclVals){
  for (exclI in 1:length(exclNames)){ # loop through criteria
    exclName  = exclNames[exclI] # select name
    exclVal = exclVals[exclI] # select condition
    tab[[paste0(exclName, "_excl")]] <- eval(parse(text = paste0("tab$", exclName, exclVal))) # add column indicating whether condition was met
  }
  return(tab)
}

#' Get mode
#' @description
#' `get_mode` returns the mode of a vector
#' @param v: vector with values
#' 
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Find best fit
#' @description
#' `find_best_fit` identifies the model with the best fit as indicated by the majority vote of multiple fit indices
#' @param tab: table containing fit indices and other criterea
#' @param exclVars: names of columns with boolean values indicating if models should be excluded
#' @param critNames: names of columns with critical fit indices
#' @param critDir: "min" or "max" depending on whether the highest or lowest value of this fit index should be selected as best fit
#' 
find_best_fit <- function(tab, critNames, critDir, exclVars = NULL){
  if(!is.null(exclVars)){
    if(length(exclVars) == 1){
      tab[tab[, exclVars] != 0, ] <- NA
    } else {
      tab[rowSums(tab[,exclVars]) != 0, ] <- NA
    }
  }
  
  if (all(is.na(tab))) { # if no models meet the criterion
    bestFit = NA 
  } else {
    bestFit <- apply(data.frame(critNames, critDir), 1, function(x) {
      
      if(x[2] == "min"){
        return(which.min(as.matrix(tab[, x[1]])))
      } else if(x[2] == "max"){
        return(which.max(as.matrix(tab[, x[1]])))
      } else {
        warning(paste0("Only use min or max for critDir, not ", x[2]))
        return(NA)
      }
    })
    bestFit <- get_mode(bestFit) # select majority vote
  }
  return(bestFit)
}


#' Plot LCMM trajectories
#' @description
#' `plot_lcmm_trajectories` generates basic ggplots to show trajectories of different classes which can then be further formatted
#' @param mod: model to be plotted
#' @param datDV: data used to estimate the model
#' @param dv_name: name of dependent variable
#' @param show_classes: optional vector with selection of classes to be plotted
#' @param sub_samp_n: optional number of randomly selected cases per class to be plotted
#' @param light_colors: optional vector of colors to be used for individual trajectories
#' @param dark_colors: optional vector of colors to be used for class trajectories
#' @param id_name: optional name of the variable indicating cases in datDV; if NULL is attempted to be extracted from mod$predRE
#' @param iv_name: optional name of the dependent variable; if NULL is attempted to be extracted from mod$predRE
plot_lcmm_trajectories <- function(mod, datDV, dv_name, show_classes = NULL, sub_samp_n = NULL,
                                   light_colors = NULL, dark_colors = NULL, id_name = NULL, iv_name = NULL){
  mod <- extract_mod(mod) # extract model
  #extract id_name and iv_name
  if(is.null(id_name)){id_name <- names(mod$predRE)[1]}
  if(is.null(iv_name)){iv_name <- names(mod$predRE)[3]}
  # define number of classes
  if(!is.null(show_classes)){
    n_classes <- length(show_classes)
  } else {
    n_classes <- mod$ng
    show_classes <- 1:n_classes
  }
  # set colors
  if(is.null(light_colors)){light_colors <- c("#919191","#FF9999","#FFCC99","#FFFF63","#99FF99","#99FFFF","#66B2FF","#CC99FF", "#FF99CC", "#FF9966")}
  if (is.null(dark_colors)){dark_colors <- c("#404040", "#CC0000", "#CC6600","#C4C400","#339900","#009999", "#0066CC","#663399","#CC3366", "#CC3300")}
  # check that enough colors for number of classes
  if (n_classes > length(light_colors)) {stop("Not enough light colors")}
  if (n_classes > length(dark_colors)) {stop("Not enough dark colors")}
  # select colors
  light_colors <- light_colors[show_classes]
  dark_colors <- dark_colors[show_classes]
  # add the class assignments to data (even if they are already in the data)
  class_name <- "plot_lcmm_trajectories_class"
  class_data <- mod$pprob[, c(id_name, "class")]
  names(class_data)[2] <- class_name
  datDV <- full_join(datDV, class_data)
  # add predictions to data
  pred <- predictY(mod, newdata = datDV, var.time = iv_name)
  pred_cols <- colnames(pred$pred)
  datDV <- cbind(datDV, pred$pred)
  # subselect classes
  if(mod$ng!= 1){ # if there is only one class in the model, the pred column is called YPred without a number
    sel_pred_cols <- grepl(paste(show_classes, collapse = "|"), pred_cols)
    datDV <- datDV[datDV[[class_name]] %in% show_classes,!(names(datDV) %in% pred_cols[!sel_pred_cols])]
    pred_cols <- pred_cols[sel_pred_cols]
  }
  # reshape data for plot
  datPlot <- pivot_longer(
    datDV[, c(id_name, iv_name, dv_name,  class_name, pred_cols)],
    cols = c(dv_name, all_of(pred_cols)),
    names_to = "name",
    values_to = "value"
  )
  # add rows with fictive IDs and classes to plot average trajectories
  for (i in seq_along(pred_cols)) { 
    datPlot$id[datPlot$name == pred_cols[i]] <- 999 * i
    datPlot[[paste0(class_name)]][datPlot$name == pred_cols[i]] <- 999 * i
  }
  # add new variable "fake" which indicates whether this data is real or not (0 = real, 1 = fake)
  datPlot <- datPlot %>%
    mutate(fake = ifelse(name %in% pred_cols, 1, 0),
           fake = as.factor(fake))
  # remove rows with NAs
  datPlot<- datPlot[!is.na(datPlot[[iv_name]]) & !is.na(datPlot[[class_name]]),]
  # subselect ids
  if(!is.null(sub_samp_n)){
    class_sizes <- table(class_data$class[class_data$class %in% show_classes])
    del_ids <- c()
    # loop through the classes
    for (classI in show_classes){
      class_rows <- grepl(classI, datPlot[[paste0(class_name)]]) & datPlot$fake == 0 # find all rows with real data from datPlot that belong to this class
      id_counts <- datPlot$id[class_rows] %>% table #count IDs
      id_show <- as.numeric(id_counts[id_counts > 1] %>% names) #shown IDs (only those with at least 2 values show up in plot)
      if(length(id_show) > sub_samp_n){ # if they are smaller than 0
        del_ids <- c(del_ids, id_show[sample(1:length(id_show), (length(id_show) - sub_samp_n))]) # select the IDs to keep only among those that have multiple measurement timepoints
      }
    }
    datPlot$value[datPlot$id %in% del_ids] <- NA #replace all values that are not to be shown with NA
  }
  # transform class_var to factor
  plot_class_levels <- sort(unique(datPlot[[class_name]]))
  plot_class_labels <- c(paste("Individuals class", plot_class_levels[1:(length(plot_class_levels)/2)]),
                         paste("Class", plot_class_levels[1:(length(plot_class_levels )/2)]))
  datPlot[[class_name]] <- factor(datPlot[[class_name]],
                                  levels = plot_class_levels,
                                  labels = plot_class_labels)
  # prepare labels for plot
  xlab = iv_name
  ylab = dv_name
  # Create basic plot
  p <- eval(parse(text = paste0("ggplot(data = datPlot, aes(x = ", iv_name, ", y = value, group =", id_name, "))")))
  # add trajectory lines and refine formatting
  p <- p +   
    geom_line(data = datPlot %>% filter(fake == 0), aes(color = !!sym(paste0(class_name))), size = 0.3) + # individual trajectories (reak)
    geom_line(data = datPlot %>% filter(fake == 1), aes(color = !!sym(paste0(class_name))), size = 1) + # Averages (fake)
    scale_colour_manual(values = c(light_colors, dark_colors)) + # adapt colors
    labs(x = xlab, y = ylab) + # add labs
    theme(legend.position = "bottom") # add legend
  #return plot
  return(p)
}

#' Find the order of lcmm intercepts
#' @description
#' `find_order_lcmm_intercepts` returns the order of classes from a model estimated using the lcmm function sorted by their intercepts
#' @param mod: object results from gridsearch_with_diagnostics
#' @param decreasing: boolean, whether the order should be by decreasing intercept values; default = FALSE
#' 
find_order_lcmm_intercepts <- function(mod, decreasing = FALSE){
  n_classes <- mod$ng
  intercepts <- mod$best[names(mod$best) %in% paste0("intercept class", 1:n_classes)]
  intercepts <- intercepts[n_classes:length(intercepts)]
  suppressWarnings(intercepts$`intercept class1` <- 0)
  intercepts <- intercepts[order(names(intercepts))]
  return(order(unlist(intercepts), decreasing = decreasing))
}


# === Agreement between models =================================================

#' Padded table
#' @description
#' `padded_table` like function "table" with zeros added for all values that are only contained in one vector but not the other
#' @param vec1: first vector with values
#' @param vec2: second vector with values
#' 
padded_table <- function(vec1, vec2){
  
  levels1 <- unique(vec1)
  levels2 <- unique(vec2)
  levels1 <- levels1[!is.na(levels1)]
  levels2 <- levels2[!is.na(levels2)]
  addCols <- levels1[!(levels1 %in% levels2)]
  addRows <- levels2[!(levels2 %in% levels1)]
  tab <- table(vec1, vec2)
  
  
  if(length(addCols) != 0){
    for(colI in 1:length(addCols)){
      tab <- cbind(tab, 0)
      colnames(tab)[ncol(tab)] <- addCols[colI]
    }
  }
  
  if(length(addRows) != 0){
    for(rowI in 1:length(addRows)){
      tab <- rbind(tab, 0)
      rownames(tab)[nrow(tab)] <- addRows[rowI]
    }
  }
  return(tab)
}


#' Agreeing cases
#' @description
#' `agreement_decimal` returns the proportion of cases where column and row name match in a table as a decimal
#' @param table: frequency table
#' 
agreement_decimal <- function(table) {
  
  total_cases <- sum(table) # total number of cases
  agreeing_cases <- sum(diag(table)) # number of cases that agree
  return(agreeing_cases / total_cases) # proportion of agreeing cases
}


#' Find highest agreement of class assignments
#' @description
#' `find_highest_agreement` matches classes of two models to maximize agreement and returns height of agreements
#' @param data: wide dataset containing the class assignments for both models
#' @param classA: variable name or column number of class assignments of class A
#' @param classB: variable name or column number of class assignments of class B
#' @param crit: criterion used to judge agreement; "decimal"/"percentage" proportion of cases with matching assignments; "kappa" cohens kappa; default = "decimal"
find_highest_agreement <- function(data, classA, classB, crit = "decimal"){
  
  tab <- padded_table(data[[classA]], data[[classB]]) # force table to contain all values for both vectors
  order_list <- gtools::permutations(ncol(tab),ncol(tab)) # find all possible permutations of classes
  maxI = 1
  
  if(crit == "percentage" | crit == "decimal"){
    maxVal = agreement_decimal(tab[, order_list[1, ]])
  }
  else if(crit == "kappa"){
    maxVal = DescTools::CohenKappa(tab[, order_list[1, ]], conf.level = 0.95)[1]
  }
  
  # loop through all permutations and find the one with the highest agreement
  for (orderI in 2: nrow(order_list)){
    
    if(crit == "percentage" | crit == "decimal"){
      agreeVal <- agreement_decimal(tab[, order_list[orderI, ]])
    } else if(crit == "kappa"){
      agreeVal = DescTools::CohenKappa(tab[, order_list[orderI, ]], conf.level = 0.95)[1]
    }
    
    if ( agreeVal > maxVal){
      maxI = orderI
      maxVal = agreeVal
    }
  }
  
  if(crit == "percentage"){
    maxVal = maxVal*100
  }
  return(list(maxVal, tab[, order_list[maxI, ]]))
}

# === Bootstraps ===============================================================

#' Boostrap lcmm estimations
#' @description
#' `bootstrap_lcmm` performs lcmm estimations in bootstrap samples
#' @param mod model which serves as template for estimation and possibly for stratification
#' @param dat data to be used for estimations
#' @param nBoots number of bootstrap samples
#' @param maxIterPerClass: vector with the number of maximal iterations for the model estimation per model; insert NA to skip a class
#' @param repPerClass: vector with the number of maximal replications for the gridsearch of each model estimation
#' @param cl: number of chores used for estimation
#' @param startValues: optional start values
#' @param gmm: boolean, whether it should be a gmm model; default = FALSE (estimating lcga)
#' @param spl: boolean, whether a spline function is fitted; defalut = FALSE (estimating linear)
#' @param strat: boolean, whether samples should be stratified to match the class distribution of the original model
#' @param path: optional, path where the model should be stored

bootstrap_lcmm <- function(mod, dat, nBoots, maxIterPerClass, repPerClass, cl, startValues = NA, gmm = FALSE, spl = FALSE, strat = FALSE, path = ""){
  
  # check that maxIterPerClass & repPerClass match and detect number of classes
  if(length(maxIterPerClass) != length(repPerClass)){
    stop("maxIterPerClass and repPerClass both have to have the same length")
  } else {
    maxClass = length(maxIterPerClass)
  }
  
  # set maxIterPerClass for baseline model to notNA; this model always has to be estimated; other classes can be skipped
  if (is.na(maxIterPerClass[1])){maxIterPerClass[1] = 500}
  
  # prepare dataframe for results
  nRes <- nBoots * maxClass # number of results = bootstrap samples * number of classes
  resVars <- c("FunCall", "BootstrapSample", "classN", "Conv", "NConv", "LogLikRep", "Entropy", "minClassSize", "BIC", "AIC", "SABIC") # variables to be reported as results 
  resVars <- c(resVars, paste("%class", 1:maxClass, sep = "")) # adding columns for class sizes
  resVars <- c(resVars,"CoefficientNames", "CoefficientEstimates", "ClassAssignments", "SampledIDs") # add sampledIDs so that exactly the same boots can be reconstructed
  m <- matrix(ncol = length(resVars), nrow = nRes)
  res <- data.frame(m)
  names(res) <- resVars
  res$BootstrapSample <- rep(1:nBoots, each = maxClass)
  res$classN <- rep(1:maxClass, nBoots)
  
  # prepare model names
  mods <- paste(c("mod_1", paste("mod_", 2:maxClass, "$model", sep = "")), collapse = ", ")
  
  # prepare function call based on model template
  mod <- extract_mod(mod) # extract $model in case result of gridsearch_with_diagnostics
  argI = 1
  fun <- as.character(mod$call[argI]) # extract function call
  argI = argI+1
  fixed_formula <- as.character(mod$call[argI]) # extract fixed formula
  argI = argI+1
  mixture_formula <- as.character(mod$call[argI]) # extract mixture formula
  argI = argI+1
  if (gmm) {
    random_formula <- as.character(mod$call[argI]) # extract random formula
    argI = argI+1}
  id <- paste("'",  as.character(mod$call[argI]), "'", sep = "") # extract id variable
  argI = argI+1
  chosen_ng <- mod$call[argI] # extract number of classes
  argI = argI+1
  if (spl) {
    spl_shape <- as.character(mod$call[argI]) # extract spline formula
    argI = argI+1 }
  argI = argI+1
  B <- mod$call[argI] # extract baseline model
  
  # Put together to formulas:
  #inside formula_base: function, fixed forumla, subject, data, B (opt), spline(opt), random(opt)
  inside_formula_base <- paste0(fun,"(",fixed_formula,", subject = 'idBoot'",", data = bootDat")
  # add start values
  if(!is.na(startValues)){inside_formula_base <- paste0(inside_formula_base, ", B = ", as.character(startValues))}
  # add random formula for gmm
  if(gmm){inside_formula_base <- paste0(inside_formula_base,", random=", random_formula)}
  if(spl){inside_formula_base <- paste0(inside_formula_base,", link = '", spl_shape, "'")} # add spline specification for spline
  # create formula for model 1 (baseline)
  inside_formula_mod_1 <- paste0(inside_formula_base,", maxiter = ", maxIterPerClass[1], ")")
  # create formulas for other models (2:maxClass)
  fun_calls <- c()
  for (classN in 2:maxClass){
    fun_calls[classN-1] <- paste0("mod_", classN," <- gridsearch_with_diagnostics(rep =", repPerClass[classN],
                                  ", maxiter = ", maxIterPerClass[classN],", minit = mod_1,  cl = ", cl,", m = ", inside_formula_base,
                                  ", mixture=", mixture_formula,", ng = ", classN,"), show = FALSE)")
    }

  # Iterate until nBoots samples are created that estimate baseline model without numerical/convergence issues 
  bootI = 1
  rowI = 1
  while(bootI <= nBoots){
    print(paste( "Boostrapsample: ", bootI, "/",nBoots, sep = ""))
    
    # selected IDs for bootstrap sample
    if (!strat){
      sampledIDs <- sample(ids,length(unique(mod$pprob$id)), replace = TRUE)
    } else {
      classes <- unique(mod$pprob$class)
      sampledIDs <- c()
      # loop through the classes and pick number of ids relative to class sizes
      for (classI in 1:length(classes)){
        classVal = classes[classI]
        classIDs <- unique(mod$pprob$id[mod$pprob$class == classVal]) # pick the ids that are within this class
        sampledIDs <- c(sampledIDs, sample(classIDs,length(classIDs),replace = TRUE))
      }
    }
    
    # prepare dataframe with bootstrap sample data
    bootDat <<- data.frame()
    for (idI in 1:length(sampledIDs)){
      newRows <- dat[dat$id == sampledIDs[idI], ] # select all rows with sampledIDs
      newRows$idBoot <- idI # assign new ids to make rows unique even if this id was already selected in an earlier iteration
      bootDat <<- rbind(bootDat, newRows) # bind rows together
    }
    
    # estimate baseline model
    res[rowI, "FunCall"] <- inside_formula_mod_1 # save function call
    mod_1 <- eval(parse(text = inside_formula_mod_1)) # estimate model
    # if it does not converge go to next bootstrap sample
    if (mod_1$conv != 1){
      res$Conv[rowI] <- mod_1$conv
      next
    }
    # save results from mod_1
    sumTab <- summarytable(mod_1, which = c('conv', 'BIC', 'AIC', 'SABIC'), display = FALSE)
    res[rowI,c("Conv",  "BIC", "AIC", "SABIC")] <- sumTab[, c("conv", "BIC", "AIC", "SABIC")]
    res[rowI,"SampledIDs"] <- paste(sampledIDs, collapse = ", ")
    res[rowI, "CoefficientNames"] <- paste(names(mod_1$best), collapse = ", ")
    res[rowI, "CoefficientEstimates"] <-paste(mod_1$best, collapse = ", ")
    rowI = rowI+1 # increase row in res
    
    # create mod-i-formula and estimate
    for (classN in 2:maxClass){
      fun_call <- fun_calls[classN-1] # select fun_call
      print(fun_call) # print it
      eval(parse(text = fun_call)) # execute function call
      # save results
      res[rowI, "SampledIDs"] <- paste(sampledIDs, collapse = ", ")
      res[rowI, "FunCall"] <- fun_call
      res[rowI, "NConv"]  <- eval(parse(text = paste("mod_", classN, "$nConv", sep = "" )))
      res[rowI, "LogLikRep"]  <- eval(parse(text = paste("mod_", classN, "$loglikrep", sep = "" )))
      # extract the model
      model_n <- eval(parse(text = paste0("mod_", classN, "$model")))
      if (class(model_n) != 'character') { # if the model converged
        sumTab <- summarytable(model_n, which = c('conv', 'entropy', '%class', 'BIC', 'AIC', 'SABIC'), display = FALSE)
        classNames <- paste0("%class",1:classN)
        res[rowI, c("Conv", "Entropy", "BIC", "AIC", "SABIC", classNames)] <- sumTab[, c("conv", "entropy", "BIC", "AIC", "SABIC", classNames)]
        res[rowI, "minClassSize"] <- min(sumTab[,classNames],na.rm = TRUE) 
        res[rowI, "CoefficientNames"] <- paste(names(model_n$best), collapse = ", ")
        res[rowI, "CoefficientEstimates"] <-paste(model_n$best, collapse = ", ")
        res[rowI, "ClassAssignments"] <- paste(model_n$pprob$class, collapse = ", ")
      }
      if (path != ""){save(res, file = path)} # update results
      rowI = rowI+1 # increase row in res
    }
    bootI = bootI +1 # increase bootstrap sample i
  }
  return(res)
}

#' Find best fit for bootstraps
#' @description
#' `bootstrap_find_best_fit` provides information about the best fit for different boostrips considering different exclusion criteria
#' @returns a datframe with one row per bootstrap, and columns for the bootstrap id, and the best fit ignoring single exclusion criteria and considering all of them (all_excl)
#' @param resDat # data containing the results of the bootstraps (output of boostrap_lcmm)
#' @param orig_mod # model to be used as comparison for cohens Kappa
#' @param critNames: names of columns with critical fit indices
#' @param critDir: "min" or "max" depending on whether the highest or lowest value of this fit index should be selectd as best fit
#' @param exclNames: column names of exclusion criteria
#' @param exclVals: conditions under which values should be excluded, e.g., c("!=1", "<0.67", "<2")
#' @param sampleName: name of the variable indicating the bootstrap sample id; default = BootstrapSample
#' @param classNName: name of the variable indicating the number of classes; default "ClassN"
#' @param ignoreBaseline: whether the baseline model should be ignored when selecting the best fit
bootstrap_find_best_fit <- function(resDat, critNames, critDir, exclNames, exclVals,sampleName = "BootstrapSample",classNName = "classN",ignoreBaseline = FALSE){
  # determine parameters
  samplesN = length(unique(resDat[[sampleName]])) # extract number of bootstrap samples from resDat
  output <-  data.frame(matrix(nrow = samplesN, ncol = length(exclNames) +2 )) # prepare dataframe for output
  names(output) <- c("bootID", paste0("ignore", exclNames), "all_excl")
  
  for (sampleI in 1:samplesN){ # loop through bootstrap samples
    output$bootID[sampleI] <- sampleI
    current_dat <- resDat[resDat[[sampleName]] == sampleI, c(classNName, critNames, exclNames)] # select only rows for this bootstrap
    if(all(is.na(current_dat[,c( critNames, exclNames)]))){next} # skip row if no information about crit vars and exclusion
    current_dat <- current_dat[rowSums(is.na(current_dat[,exclNames])) == 0, ] # select only rows that have information about exclusion criteria (-> estimation was successful)
    current_dat <- current_dat[order(current_dat[[classNName]]),] # order by class N
    if(ignoreBaseline){current_dat <- current_dat[current_dat[[classNName]] != 1,]} # optionally delete baseline model
    current_dat <- mark_exclusion_criteria_in_tab(current_dat, exclNames, exclVals)
    exclVars <- names(current_dat)[grepl("excl",names(current_dat))]
    # repeat with except each exclusion and with all exclusions
    for (exclI in 1:(length(exclNames) +1)){
      selExclVars = exclVars[-exclI]
      bestFit <- find_best_fit(current_dat, critNames, critDir, selExclVars)
      if(!is.na(bestFit)){
        output[sampleI, exclI+1] <- current_dat[bestFit, classNName] # save the result
      }
    }
  }
  return(output)
}


#' Diagnostics of bootstrap lcmm estimations
#' @description
#' `bootstrap_diagnostics`prints diagnostic information about the estimation procedure of the bootstraps
#' @param resDat # data containing the results of the bootstraps (output of boostrap_lcmm)
#' @param orig_mod # model to be used as comparison for cohens Kappa
#' @param critNames: names of columns with critical fit indices
#' @param critDir: "min" or "max" depending on whether the highest or lowest value of this fit index should be selectd as best fit
#' @param exclNames: column names of exclusion criteria
#' @param exclVals: conditions under which values should be excluded, e.g., c("!=1", "<0.67", "<2")
#' @param minLoglikrep: required number of loglik replications per model per bootstrap
#' @param sampleName: name of the variable indicating the bootstrap sample id; default = BootstrapSample
#' @param classNName: name of the variable indicating the number of classes; default "ClassN"
#' @param ignoreBaseline: whether the baseline model should be ignored when selecting the best fit
bootstrap_diagnostics <- function(resDat,  orig_mod, critNames, critDir, exclNames, exclVals, minLoglikrep, sampleName = "BootstrapSample",classNName = "classN",ignoreBaseline = FALSE){
  #determine parameters
  orig_mod <- extract_mod(orig_mod)
  selClassN  <- orig_mod$ng # extract number of classes from original model
  samplesN = length(unique(resDat[[sampleName]])) # extract number of bootstrap samples from resDat
  # determine whether more replications are needed
  cat("Samples in which model did not converge per class\n")
  resDat[is.na(resDat$Conv) | (resDat$Conv != 1), classNName] %>% table %>% print
  # determine whether more replications are needed
  cat("Samples in which loglik was not replicated enough per class\n")
  resDat[!is.na(resDat$LogLikRep) & (resDat$LogLikRep < minLoglikrep), classNName] %>% table %>% print
  # determine whether more classes are needed
  bestFitDat = bootstrap_find_best_fit(resDat, critNames, critDir, exclNames, exclVals,sampleName,classNName,ignoreBaseline) # find best fit
  matchBootstrapIDs <- bestFitDat$bootID[bestFitDat$all_excl == selClassN]
  increaseClassNCount = 0
  for(sampleI in matchBootstrapIDs){
    this_dat <- resDat[resDat[[sampleName]] == sampleI, c(classNName, critNames)] # select data for this bootstrap
    this_dat <- this_dat[rowSums(is.na(this_dat[, critNames])) == 0, ] # select only valid data
    if(nrow(this_dat) <= 1){next} # if only one row left, skip
    this_dat <- this_dat[order(this_dat[[classNName]], decreasing = TRUE), ] # sort by class (decreasing)
    this_dat <- this_dat[1:2,] # only select the two highest classes
    if(find_best_fit(this_dat, critNames, critDir) == 1){ # if the highest class is the best fit
      increaseClassNCount = increaseClassNCount +1
    }
  }
  cat("ClassN has to be further increased\n")
  print(increaseClassNCount)
}

#' Cohens Kappa for bootstraps
#' @description
#' `boot_cohens_kappa` compares each bootstrap class assignments with the original model and provides a cohens kappa
#' @returns a datframe with one row per bootstrap, and columns for the bootstrap id, and cohensKappa of class assignments with orig_mod for the corresponding model from the bootstrap is
#' @param resDat # data containing the results of the bootstraps (output of boostrap_lcmm)
#' @param orig_mod # model to be used as comparison for cohens Kappa
#' @param sampleName: name of the variable indicating the bootstrap sample id; default = BootstrapSample
#' @param sampledIDsName: name of the variable indicating the sampled ids; default = SampledIDs
#' @param classAssignmentsName: name of the variable indicating the classAssignmnets per person; default "ClassAssignments"
#' @param classNName: name of the variable indicating the number of classes; default "ClassN"
#' @param join_classes_by: optional name of the variable indicating the variable used to join class assignements from the mod;
boot_cohens_kappa <- function(resDat,orig_mod, sampleName = "BootstrapSample",sampledIDsName = "SampledIDs",classAssignmentsName = "ClassAssignments",classNName = "classN",join_classes_by = NULL){
  # determine parameters
  selClassN  <- orig_mod$ng # extract number of classes from original model
  samplesN = length(unique(resDat[[sampleName]])) # extract number of bootstrap samples from resDat
  output <-  data.frame(matrix(nrow = samplesN, ncol = 2 )) # prepare dataframe for output
  names(output) <- c("bootID", "cohensKappa")
  
  for (sampleI in 1:samplesN){ # loop through bootstrap samples
    output$bootID[sampleI] <- sampleI
    current_dat <- resDat[resDat[[sampleName]] == sampleI, c(classNName, sampledIDsName, classAssignmentsName)]
    rowI = which(current_dat[[classNName]] == selClassN)
    if(is.na(current_dat[[classAssignmentsName]][rowI])){
      output$cohensKappa[sampleI] <- NA
    } else {
      new_classes <- as.data.frame(cbind(as.numeric(unlist(strsplit(current_dat[[sampledIDsName]][rowI], ", "))),
                                         as.numeric(unlist(strsplit(current_dat[[classAssignmentsName]][rowI], ", ")))))
      names(new_classes) <- c("id", "class_new")
      new_classes <- new_classes[!duplicated(new_classes),]
      orig_classes <- orig_mod$pprob[,c("id", "class")]
      names(orig_classes)[names(orig_classes) == "class"] <- "class_orig"
      all_classes <- full_join(orig_classes, new_classes, by = join_classes_by) %>% suppressMessages
      output$cohensKappa[sampleI] <- find_highest_agreement(all_classes, "class_orig", "class_new", "kappa")[[1]]
    }
  }
  return(output)
}

#' Evaluation of bootstrap lcmm estimations
#' @description
#' `bootstrap_evaluation` 
#' @returns a datframe with one row per bootstrap, and columns for the bootstrap id, and cohensKappa of class assignments with orig_mod for the corresponding model from the bootstrap is
#' @param resDat # data containing the results of the bootstraps (output of boostrap_lcmm)
#' @param orig_mod # model to be used as comparison for cohens Kappa
#' @param critNames: names of columns with critical fit indices
#' @param critDir: "min" or "max" depending on whether the highest or lowest value of this fit index should be selectd as best fit
#' @param exclNames: column names of exclusion criteria
#' @param exclVals: conditions under which values should be excluded, e.g., c("!=1", "<0.67", "<2")
#' @param sampleName: name of the variable indicating the bootstrap sample id; default = BootstrapSample
#' @param sampledIDsName: name of the variable indicating the sampled ids; default = SampledIDs
#' @param classAssignmentsName: name of the variable indicating the classAssignmnets per person; default "ClassAssignments"
#' @param classNName: name of the variable indicating the number of classes; default "ClassN"
#' @param ignoreBaseline: whether the baseline model should be ignored when selecting the best fit
#' @param join_classes_by: optional name of the variable indicating the variable used to join class assignements from the mod;
bootstrap_evaluation <- function(resDat, orig_mod, critNames, critDir, exclNames, exclVals, sampleName = "BootstrapSample",sampledIDsName = "SampledIDs",classAssignmentsName = "ClassAssignments", classNName = "classN",ignoreBaseline = FALSE, join_classes_by = NULL){
  orig_mod <- extract_mod(orig_mod) # extract model
  res <- data.frame(cohensKappa = NA, Robustness = NA) # prepare dataframe for results
  sumDat = bootstrap_find_best_fit(resDat, critNames, critDir, exclNames, exclVals,sampleName,classNName,ignoreBaseline) # find best fit
  sumDat = full_join(sumDat, boot_cohens_kappa(resDat,orig_mod,sampleName,sampledIDsName,classAssignmentsName,classNName,join_classes_by)) # compute cohens Kappas
  #print evaluation results and store Robustness and CohensKappa under res
  cat("Relative frequency of optimal class numbers\n")
  sumDat$all_excl %>% table() %>% prop.table() %>% print()
  cat("Boots that are sorted the same even if Class size is ignored\n")
  sum(sumDat$ignoreminClassSize == sumDat$all_excl, na.rm = TRUE) %>% print()
  cat("Boots that are sorted the same even if entropy is ignored\n")
  sum(sumDat$ignoreEntropy == sumDat$all_excl, na.rm = TRUE) %>% print()
  cat("Boots that find the same number of classes\n")
  res$Robustness <- sum(sumDat$all_excl == mod$ng, na.rm = TRUE) %>% print()
  cat("...If class size is ignored\n")
  sum(sumDat$ignoreminClassSize == mod$ng, na.rm = TRUE) %>% print()
  cat("...If entropy is ignored\n")
  sum(sumDat$ignoreEntropy == mod$ng, na.rm = TRUE) %>% print()
  cat("Mean cohens kappa\n")
  res$cohensKappa <- sumDat$cohensKappa %>% mean(na.rm = TRUE) %>% print() 
  return(list(res, sumDat))
}
