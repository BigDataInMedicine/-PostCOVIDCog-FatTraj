# ==============================================================================
# === CUSTOM FUNCTIONS for NAPKON data handling and preprocessing ==============
# ==============================================================================

# Authors: Ann-Kathrin Knak
# Last update: 08.04.2026
# Publication: Trajectories of cognitive function and fatigue after a SARS-CoV-2 infection
# Description: These are the custom functions that we created for our analysis

# === NAPKON Data handling & Preprocessing =====================================

#' Get Dataset
#' 
#' @description
#' `get_dataset` returns the index/indices of all subdatasets that contain the given variable name
#'
#' @param var_name name of the variable that is searched for in the nested dataset
#' @param dataBank the object containing the nested NAPKON data
get_dataset <- function(var_name, dataBank){
  
  num <- which(as.logical(lapply(dataBank, function(x) var_name %in% names(x))))
  return(num)
  
}

#' Compose new dataset with selected variables
#' 
#' @description
#' `get_vars` returns a new dataset that contains all the data of the selected variables from the nested dataset
#'
#' @param var_names names of the variables that should be in the final dataset
#' @param dataBank the object containing the nested NAPKON data
#' @param pids whether or not the participant ID should be in the final dataset (default = FALSE)
#' @param vlabs whether or not the visit labels should be in the final dataset (default = FALSE)
#' @param EBtoVO whether or not datarows from EB and VO visits in POP should be merged together if they have the same number (default = FALSE)
#' @param joinBy if another variable than pids and vlabs should be used for identifying rows that belong together, add them here
get_vars <- function(var_names, dataBank, pids = FALSE, vlabs = FALSE, EBtoVO = FALSE, joinBy = c()){
  
  datasetIs <- lapply(var_names, get_dataset, dataBank)
  
  if (any(lapply(datasetIs, length )>1)){
    stop("The variable(s) ", var_names[lapply(datasetIs, length )>1], " exists in at least two datasets. Please rename one of them.")
  }
  if (any(lapply(datasetIs, length )<1)){
    stop("The variable ", var_names[lapply(datasetIs, length )<1], " could not be found. Please check the name.")
  }
  
  
  metaVars = c("mnpcvpid","export_psn","mnpvislabel")
  
  if (!EBtoVO){
    metaVars = metaVars[c(!(pids & vlabs),pids,vlabs)]
  }
  
  else {
    metaVars = metaVars[c(FALSE, TRUE, TRUE)]
  }
  
  # add metavars
  metaVars = c(metaVars, joinBy)
  keepMetaVars = rep(TRUE,length(metaVars))
  
  
  # check if all metavars are present
  for (datasetI in datasetIs){
    keepMetaVars = keepMetaVars * (metaVars %in% names(dataBank[[datasetI]]))
  }
  
  if (any(!as.logical(keepMetaVars))){
    warning(paste("The variable(s) ", metaVars[!as.logical(keepMetaVars)], "were not in all datasets. To join datasets they need at least one variable in common. It must have the same name in both datasets. If this is not the case, please rename one of the variables and then add the name as joinBy =..."))
    
  }
  
  metaVars = metaVars[as.logical(keepMetaVars)]
  
  
  
  if (datasetIs %>% unique %>% length == 1) {
    
    output = dataBank[[datasetIs[[1]]]][c(metaVars, var_names)] 
    
  } else {#if (all(datasetIs %in% get_dataset("mnpcvpid", dataBank))){
    
    # merge them using the metadata
    output = dataBank[[datasetIs[[1]]]][c(metaVars,var_names[1])]
    
    if (EBtoVO){
      output$mnpvislabel = paste(substr(output$mnpvislabel ,1,9), "EB/VO", sep = "")
    }
    
    if (length(var_names) > 1){
      
      for (datasetI in unique(datasetIs)){
        # find the variable names that belong to this dataset
        var_namesNames <- var_names[datasetIs == datasetI]
        
        joindata = dataBank[[datasetI]][c(metaVars,var_namesNames)]
        
        if (EBtoVO){
          
          joindata$mnpvislabel = paste(substr(joindata$mnpvislabel ,1,9), "EB/VO", sep = "")
        }
        
        output = full_join(output, 
                           joindata)
        
      }
      
      
    }
  }
  output <- as.data.frame(output)
  
  if (!(pids & vlabs)) { # delete the id
    output = output[,names(output) != "mnpcvpid"]
  }
  if (EBtoVO){
    if (!pids){
      output = output[,names(output) != "export_psn"]
    }
    if (!vlabs){
      output = output[,names(output) != "mnpvislabel"]
    }
  }
  return(output)
}




#' apply any or na across vactors
#' 
#' @description
#' `any_or_NA` apply any, unless all values are NA, to the values in the same position on two or more vectors
#'
#' @param ... list of vectors
any_or_NA <- function(...){ # a number of arrays can be passed; they must have the same length
  
  varList <- list(...)
  #print(length(varList))
  
  varLen <- nrow(varList[[1]])
  if (is.null(varLen)){
    varLen <- length(varList[[1]])
  }
  
  if (length(varList) > 1){
    for (varI in 2:length(varList)){
      varILength <- nrow(varList[[varI]])
      if (is.null(varILength)){
        varILength <- length(varList[[varI]])
      }
      if (varILength != varLen){
        stop("Variable nr. ", varI, "has a different length than the first variable.")
      }
    }
  }
  
  
  df <- cbind(...) # bind all variables to one dataframe
  
  
  # check if there are any true values in each row
  varOut <- apply(df, 1, function(x) any(as.logical(x), na.rm = TRUE))
  # replace those FALSE that were cause by only NA with NA
  varOut[rowSums(is.na(df)) == length(varList)] <- NA
  
  
  return(varOut)
}

#' apply any or na to subsets of a dataset
#' 
#' @description
#' `any_or_NA_by_person` apply any, unless all values are NA, to the values that belong to the same id from a dataset
#'
#' @param dataset the data frame containing all the necessary data and variables in long-format
#' @param id_var name of the variable identifying individuals
#' @param value_var variable from which values should be aggregated
#' @param new_var_name name of the new variable with aggregated values
any_or_NA_by_person <- function(dataset, id_var, value_var, new_var_name) {
  
  # transform to logical (anything other than 0 or NA -> true)
  if(is.numeric(dataset[[value_var]])){
    dataset[[value_var]] <- as.logical(dataset[[value_var]])
  }
  
  # prepare output
  datOut <- data.frame(unique(dataset[[id_var]]), NA)
  names(datOut) <- c(id_var, new_var_name)
  
  # compute result per person
  datOut[[new_var_name]] <- unlist(apply(datOut,1, function(x) {
    subdat <- dataset[dataset[[id_var]] == x[[id_var]], value_var]
    if (all(is.na(subdat))) {
      return(NA)
    } else {
      return(any(subdat, na.rm = TRUE))
    }
  }))
  
  return(datOut)
}




#' aggregate values from multiple vectors
#' 
#' @description
#' `combine_nonbinary_vectors` aggregate non-binary values from the same position on two or more vectors
#'
#' @param strategy rule how the values should be aggregated
#'  lowest/highest: always select the lowest or highest value
#'  ascending_order/descending_order: pick always the first non-missing value in the order the vectors were listed
#' @param ... list of vectors
combine_nonbinary_vectors <- function(strategy, ...) {
  # list of input vectors
  input_vectors <- list(...)
  
  # combine vectors to matrix
  combined_matrix <- do.call(cbind, input_vectors)
  
  # define help function
  apply_strategy <- function(values) {
    # remove NA
    values <- values[!is.na(values)]
    
    # if all NA, return NA
    if (length(values) == 0) return(NA)
    
    # strategy "lowest"
    if (strategy == "lowest") {
      return(min(values))
      
      # strategy "highest"
    } else if (strategy == "highest") {
      return(max(values))
      
      # strategy "ascending_order"
    } else if (strategy == "ascending_order") {
      return(values[which.min(order(values))])
      
      # strategy "descending_order"
    } else if (strategy == "descending_order") {
      return(values[which.max(order(values))])
      
    } else {
      stop("Invalid strategy. Please use 'lowest', 'highest', 'ascending_order' or 'descending_order'.")
    }
  }
  
  # apply strategy to matrix
  output_vector <- apply(combined_matrix, 1, apply_strategy)
  
  return(output_vector)
}



#' Create PCS symptom for POP visit 1
#' 
#' @description
#' `pcs_score_pop_custom` returns a dataframe with the participant ids, visit labels and pcs-score according to its computation in the NAPKON manual
#'
#' @param dataset either the nested NAPKON dataset with all subdatasets or a dataset created with get_vars containing all the variables required for the pcs score
#
pcs_score_pop_custom <- function(dataset){
  
  # variables belonging to symptom clusters and weight of each cluster
  sym_clusters <- list(
    list(vars = c("sym_verl_akt___1", "sym_verl_akt___2"), weight = 3.5), # chemosensory
    list(vars = c("cfs2"), weight = 7), # fatigue
    list(vars = c("verl_belastb", "sym_verl_akt___16"), weight = 4 ), # exercise intolerance
    list(vars = c("sym_verl_akt___14", "sym_verl_akt___15"), weight = 6.5), # muscle and limb pain
    list(vars = c("sym_verl_akt___10", "sym_verl_akt___11", "sym_verl_akt___12"), weight = 5.5), #nose, throat, ear
    list(vars = c("sym_verl_akt___9", "sym_verl_akt___17"), weight = 7), # pulmonary
    list(vars = c("sym_verl_akt___18"), weight = 3.5), # cardiac
    list(vars = c("sym_verl_akt___3", "sym_verl_akt___5", "sym_verl_akt___6", "sym_verl_akt___7"), weight = 5), # gastrointestal
    list(vars = c("cfs3", "sym_verl_akt___4", "sym_verl_akt___8", "sym_verl_akt___21", "sym_neurol4_2", "sym_neurol4_3","sym_neurol4_4","sym_neurol4_5","sym_neurol4_6", "sym_neurol4_10"), weight = 6.5), # neurological
    list(vars = c("sym_verl_akt___23", "sym_verl_akt___19", "sym_neurol4_11"), weight = 2), # dermatological
    list(vars = c("cfs11", "sym_verl_akt___13", "sym_verl_akt___20"), weight = 3.5), # infection
    list(vars = c("cfs4", "cfs5"), weight = 5) # sleep
  )
  
  all_var_names <- unlist(lapply(sym_clusters, function(x) x[["vars"]]))
  
  
  # if the data has not been extracted from the raw nested data yet
  if (class(dataset) == "tsExportdata"){
    
    # rename variables that have the same name but are irrelevant
    names(dataset[[2]]) [names(dataset[[2]]) == "sym_neurol4_2"] <- "sym_neurol4_2_not_relevant"
    
    # compose small dataset with only the relevant vars
    dataset <- get_vars(all_var_names, dataset, pids = TRUE, vlabs = TRUE, EBtoVO = TRUE)
    
  } else {
    
    # check if all necessary variables are in the dataset, else return the other variables
    if (!all(all_var_names %in% names(dataset))){
      stop(message = paste("PCS Score cannot be computed because following variables are missing:", paste(all_var_names[!(all_var_names %in% names(dataset))], collapse = ", ")))
    }
    
  }
  
  dataset <- dataset[grepl("Visite 1", dataset$mnpvislabel),]
  
  # adapt coding of certain variables
  dataset$verl_belastb <- dataset$verl_belastb == 8
  
  # compute weighted sum - only missing if missing in category
  pcss_custom <- rowSums(as.data.frame(lapply(sym_clusters, function(x) any_or_NA(dataset[, x$vars]) * x$weight)))
  
  dataset$mnpvislabel <- substr(dataset$mnpvislabel,1,11)
  
  return(cbind(dataset[, c("export_psn", "mnpvislabel")], pcss_custom))
  
}

#' Copy a value from one visit to all other visits
#' 
#' @description
#' `copy_value_to_all` returns the same dataframe but with a copy of the specified variable that contains contains the value from one visit to all other visits
#'
#' @param dataset datset containing all the relevant data
#' @param id_var name or index of the variable indicating the participant id (or other clustering variable)
#' @param value_var name or index of the variable which currently does not have data for each visit per person
#' @param new_var_name name of the new variable that will be created containing the same value for each visit for the same person
#
copy_value_to_all <- function(dataset, id_var, value_var, new_var_name) {
  if (!all(c(id_var, value_var) %in% colnames(dataset))) {
    stop("Stop! One or more of the variables do not exist in the dataset.")
  }
  
  dataset[[new_var_name]] <- ave(
    dataset[[value_var]],
    dataset[[id_var]],
    FUN = function(x) {
      non_missing <- x[!is.na(x)]
      if (length(unique(non_missing)) > 1) {
        warning("More than one non-missing value for ", value_var, " within one ID. Only the first one is used.")
      }
      if (length(non_missing) > 0) {
        return(rep(non_missing[1], length(x)))
      } else {
        return(rep(NA, length(x)))
      }
    }
  )
  return(dataset)
}

#' Show which values are invalid
#' 
#' @description
#' `show_invalid_values` prints which invalid values exist and returns their indices
#'
#' @param dataset datset containing all the relevant data
#' @param var_name name or index of the variable that should be checked
#' @param upper upper bound of valid values
#' @param lower lower bound of valid values
#
show_invalid_values <- function(dataset, var_name, upper = NA, lower = NA){
  invalid <- which((dataset[[var_name]] < lower | dataset[[var_name]] > upper) & !is.na(dataset[[var_name]]))
  if(length(invalid) == 0){
    print(paste0("No invalid values for variable ", var_name))
  } else {
    print(paste0(length(invalid), " invalid values: ", paste(dataset[invalid, var_name], collapse = ", ")))
    return(invalid)
  }
  
}

#' Identify outliers using tukey method
#' 
#' @description
#' `tukey_outliers` applies the tukey method to detect outliers given a certain multiplier and returns a list of the remaining valid data, of the outliers
#' the number of outliers and their range
#' 
#' @param var_name name or index of the variable that should be checked
#' @param dataset datset containing all the relevant data
#' @param multiplier factor the interquartile range is to be multiplied with
#
tukey_outliers <- function(var_name, dataset, multiplier = 3){
  
  # compute upper and lower bound
  iqb <- quantile(dataset[[var_name]], na.rm = TRUE)[c(2,4)]
  iqr <- diff(iqb)
  tukeyMin <- iqb[1] - multiplier* iqr
  tukeyMax <- iqb[2] + multiplier * iqr
  
  # identify outliers
  outliersPos <- dataset[[var_name]] > tukeyMax | dataset[[var_name]] < tukeyMin
  # count outliers
  nOutliers <- sum(outliersPos, na.rm = TRUE)
  if(nOutliers >= 1){
    # create new dataset with outliers replaced by NA
    validData <- dataset[[var_name]]
    validData[outliersPos] <- NA
    #collect outliers
    outliers <- dataset[[var_name]][outliersPos]
    # measure range of outliers
    rangeOutliers <- range(outliers, na.rm= TRUE)
  } else {
    validData <- dataset[[var_name]]
    outliers <- NA
    rangeOutliers <- NA
  }
  
  
  return(list(validData, outliers, nOutliers, rangeOutliers))
}