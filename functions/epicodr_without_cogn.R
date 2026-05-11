# internal functions
primary_coding_suep_promis_29_fatigue <- function (trial_data, visitid) 
{
  formname_to_add_vars <- "promext"
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  new_vars_to_add <- form_to_add_vars %>% mutate(ecu_promis29_fatigue_sum = calculate_promis_29_fatigue_sum(.data$pro_29_hi7, 
                                                                                                            .data$pro_29_an3, .data$pro_29_fatexp41, .data$pro_29_fatexp40), 
                                                 ecu_promis29_fatigue_cat = categorize_promis_29_fatigue(.data$ecu_promis29_fatigue_sum), 
                                                 ecu_promis29_fatigue_cat_2 = categorize_promis_29_fatigue_2(.data$ecu_promis29_fatigue_sum)) %>% 
    select(matches(visitid), "ecu_promis29_fatigue_sum", 
           "ecu_promis29_fatigue_cat", "ecu_promis29_fatigue_cat_2")
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], 
                                                  new_vars_to_add, by = visitid)
  return(trial_data)
}


primary_coding_suep_promis_29_dyspnea <- function (trial_data, visitid) 
{
  formname_to_add_vars <- "promext"
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  if (!"ecu_pro_dysp_1" %in% names(form_to_add_vars)) {
    new_vars_to_add <- form_to_add_vars %>% mutate(pro_dysp_1 = recode_promis_dyspnoe(.data$pro_dysfl001.factor), 
                                                   pro_dysp_2 = recode_promis_dyspnoe(.data$pro_dysfl002.factor), 
                                                   pro_dysp_3 = recode_promis_dyspnoe(.data$pro_dysfl003.factor), 
                                                   pro_dysp_4 = recode_promis_dyspnoe(.data$pro_dysfl004.factor), 
                                                   pro_dysp_5 = recode_promis_dyspnoe(.data$pro_dysfl005.factor), 
                                                   pro_dysp_6 = recode_promis_dyspnoe(.data$pro_dysfl006.factor), 
                                                   pro_dysp_7 = recode_promis_dyspnoe(.data$pro_dysfl007.factor), 
                                                   pro_dysp_8 = recode_promis_dyspnoe(.data$pro_dysfl008.factor), 
                                                   pro_dysp_9 = recode_promis_dyspnoe(.data$pro_dysfl009.factor), 
                                                   pro_dysp_10 = recode_promis_dyspnoe(.data$pro_dysfl10.factor), 
                                                   ecu_promis29_dyspnea_n = count_n_promis_29_dyspnea(.data$pro_dysp_1, 
                                                                                                      .data$pro_dysp_2, .data$pro_dysp_3, .data$pro_dysp_4, 
                                                                                                      .data$pro_dysp_5, .data$pro_dysp_6, .data$pro_dysp_7, 
                                                                                                      .data$pro_dysp_8, .data$pro_dysp_9, .data$pro_dysp_10), 
                                                   ecu_promis29_dyspnea_sum = calculate_promis_29_dyspnea_sum(.data$pro_dysp_1, 
                                                                                                              .data$pro_dysp_2, .data$pro_dysp_3, .data$pro_dysp_4, 
                                                                                                              .data$pro_dysp_5, .data$pro_dysp_6, .data$pro_dysp_7, 
                                                                                                              .data$pro_dysp_8, .data$pro_dysp_9, .data$pro_dysp_10, 
                                                                                                              .data$ecu_promis29_dyspnea_n), ecu_promis29_dyspnea_cat = categorize_promis_29_dyspnea(.data$ecu_promis29_dyspnea_sum), 
                                                   ecu_promis29_dyspnea_cat_2 = categorize_promis_29_dyspnea_2(.data$ecu_promis29_dyspnea_sum)) %>% 
      select(matches(visitid), "ecu_promis29_dyspnea_n", 
             "ecu_promis29_dyspnea_sum", "ecu_promis29_dyspnea_cat", 
             "ecu_promis29_dyspnea_cat_2")
  }
  else {
    new_vars_to_add <- form_to_add_vars %>% mutate(ecu_promis29_dyspnea_n = count_n_promis_29_dyspnea(.data$ecu_pro_dysp_1, 
                                                                                                      .data$ecu_pro_dysp_2, .data$ecu_pro_dysp_3, .data$ecu_pro_dysp_4, 
                                                                                                      .data$ecu_pro_dysp_5, .data$ecu_pro_dysp_6, .data$ecu_pro_dysp_7, 
                                                                                                      .data$ecu_pro_dysp_8, .data$ecu_pro_dysp_9, .data$ecu_pro_dysp_10), 
                                                   ecu_promis29_dyspnea_sum = calculate_promis_29_dyspnea_sum(.data$ecu_pro_dysp_1, 
                                                                                                              .data$ecu_pro_dysp_2, .data$ecu_pro_dysp_3, .data$ecu_pro_dysp_4, 
                                                                                                              .data$ecu_pro_dysp_5, .data$ecu_pro_dysp_6, .data$ecu_pro_dysp_7, 
                                                                                                              .data$ecu_pro_dysp_8, .data$ecu_pro_dysp_9, .data$ecu_pro_dysp_10, 
                                                                                                              .data$ecu_promis29_dyspnea_n), ecu_promis29_dyspnea_cat = categorize_promis_29_dyspnea(.data$ecu_promis29_dyspnea_sum), 
                                                   ecu_promis29_dyspnea_cat_2 = categorize_promis_29_dyspnea_2(.data$ecu_promis29_dyspnea_sum)) %>% 
      select(matches(visitid), "ecu_promis29_dyspnea_n", 
             "ecu_promis29_dyspnea_sum", "ecu_promis29_dyspnea_cat", 
             "ecu_promis29_dyspnea_cat_2")
  }
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], 
                                                  new_vars_to_add, by = visitid)
  return(trial_data)
}

# I BELIEVE THIS ONLY REFERS TO COGNITIVE FUNCTIONS
primary_coding_suep_promis_cogn_funct <- function (trial_data, visitid) 
{
  formname_to_add_vars <- "prom"
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  if (!"ecu_pro_cogn_1" %in% names(form_to_add_vars)) {
    new_vars_to_add <- form_to_add_vars %>% mutate(pro_cogn_1 = recode_promis_cognitive(.data$pro_pc2r.factor), 
                                                   pro_cogn_2 = recode_promis_cognitive(.data$pro_pc35r.factor), 
                                                   pro_cogn_3 = recode_promis_cognitive(.data$pro_pc36r.factor), 
                                                   pro_cogn_4 = recode_promis_cognitive(.data$pro_pc42r.factor), 
                                                   ecu_promis_cogn_funct_sum = calculate_promis_cognitive_funct_sum(.data$pro_cogn_1, 
                                                                                                                    .data$pro_cogn_2, .data$pro_cogn_3, .data$pro_cogn_4), 
                                                   ecu_promis_cogn_funct_cat = categorize_promis_cognitive_funct(.data$ecu_promis_cogn_funct_sum), 
                                                   ecu_promis_cogn_funct_cat_2 = categorize_promis_cognitive_funct_2(.data$ecu_promis_cogn_funct_sum)) %>% 
      select(matches(visitid), "ecu_promis_cogn_funct_sum", 
             "ecu_promis_cogn_funct_cat", "ecu_promis_cogn_funct_cat_2")
  }
  else {
    new_vars_to_add <- form_to_add_vars %>% mutate(ecu_promis_cogn_funct_sum = calculate_promis_cognitive_funct_sum(.data$ecu_pro_cogn_1, 
                                                                                                                    .data$ecu_pro_cogn_2, .data$ecu_pro_cogn_3, .data$ecu_pro_cogn_4), 
                                                   ecu_promis_cogn_funct_cat = categorize_promis_cognitive_funct(.data$ecu_promis_cogn_funct_sum), 
                                                   ecu_promis_cogn_funct_cat_2 = categorize_promis_cognitive_funct_2(.data$ecu_promis_cogn_funct_sum)) %>% 
      select(matches(visitid), "ecu_promis_cogn_funct_sum", 
             "ecu_promis_cogn_funct_cat", "ecu_promis_cogn_funct_cat_2")
  }
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], 
                                                  new_vars_to_add, by = visitid)
  return(trial_data)
}





primary_coding_suep_promis_29_sleep <- function (trial_data, visitid) 
{
  formname_to_add_vars <- "promext"
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  if (!"ecu_pro29_sleep_1" %in% names(form_to_add_vars)) {
    new_vars_to_add <- form_to_add_vars %>% mutate(pro29_sleep_1 = recode_promis29_sleep(.data$pro_29_sleep109.factor), 
                                                   pro29_sleep_2 = recode_promis29_sleep(.data$pro_29_sleep116.factor), 
                                                   ecu_promis29_sleep_sum = calculate_promis29_sleep_sum(.data$pro29_sleep_1, 
                                                                                                         .data$pro29_sleep_2, .data$pro_29_sleep20, .data$pro_29_sleep44), 
                                                   ecu_promis29_sleep_cat = categorize_promis29_sleep(.data$ecu_promis29_sleep_sum), 
                                                   ecu_promis29_sleep_cat_2 = categorize_promis29_sleep_2(.data$ecu_promis29_sleep_sum)) %>% 
      select(matches(visitid), "ecu_promis29_sleep_sum", 
             "ecu_promis29_sleep_cat", "ecu_promis29_sleep_cat_2")
  }
  else {
    new_vars_to_add <- form_to_add_vars %>% mutate(ecu_promis29_sleep_sum = calculate_promis29_sleep_sum(.data$ecu_pro29_sleep_1, 
                                                                                                         .data$ecu_pro29_sleep_2, .data$pro_29_sleep20, .data$pro_29_sleep44), 
                                                   ecu_promis29_sleep_cat = categorize_promis29_sleep(.data$ecu_promis29_sleep_sum), 
                                                   ecu_promis29_sleep_cat_2 = categorize_promis29_sleep_2(.data$ecu_promis29_sleep_sum)) %>% 
      select(matches(visitid), "ecu_promis29_sleep_sum", 
             "ecu_promis29_sleep_cat", "ecu_promis29_sleep_cat_2")
  }
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], 
                                                  new_vars_to_add, by = visitid)
  return(trial_data)
}


calculate_promis_29_fatigue_sum <- function (pro_fatigue_1, pro_fatigue_2, pro_fatigue_3, pro_fatigue_4) 
{
  pro_fatigue_sum <- pro_fatigue_1 + pro_fatigue_2 + pro_fatigue_3 + 
    pro_fatigue_4
  pro_fatigue_sum <- ifelse(pro_fatigue_1 == -1 | pro_fatigue_2 == 
                              -1 | pro_fatigue_3 == -1 | pro_fatigue_4 == -1, NA_real_, 
                            pro_fatigue_sum)
  return(pro_fatigue_sum)
}

categorize_promis_29_fatigue <- function (pro_fatigue_sum) 
{
  pro_fatigue_cat <- case_when(pro_fatigue_sum < 11 ~ "No Fatigue", 
                               pro_fatigue_sum >= 11 & pro_fatigue_sum < 14 ~ "Mild Fatigue", 
                               pro_fatigue_sum >= 14 & pro_fatigue_sum < 19 ~ "Moderate Fatigue", 
                               pro_fatigue_sum >= 19 ~ "Severe Fatigue")
  return(pro_fatigue_cat)
}

categorize_promis_29_fatigue_2 <- function (pro_fatigue_sum) 
{
  pro_fatigue_cat_2 <- case_when(pro_fatigue_sum < 11 ~ "No Fatigue", 
                                 pro_fatigue_sum >= 11 ~ "Fatigue")
  return(pro_fatigue_cat_2)
}


count_n_promis_29_dyspnea <- function (pro_dysp_1, pro_dysp_2, pro_dysp_3, pro_dysp_4, pro_dysp_5, 
          pro_dysp_6, pro_dysp_7, pro_dysp_8, pro_dysp_9, pro_dysp_10) 
{
  pro_dysp_n <- ifelse(!is.na(pro_dysp_1), 1, 0) + ifelse(!is.na(pro_dysp_2), 
                                                          1, 0) + ifelse(!is.na(pro_dysp_3), 1, 0) + ifelse(!is.na(pro_dysp_4), 
                                                                                                            1, 0) + ifelse(!is.na(pro_dysp_5), 1, 0) + ifelse(!is.na(pro_dysp_6), 
                                                                                                                                                              1, 0) + ifelse(!is.na(pro_dysp_7), 1, 0) + ifelse(!is.na(pro_dysp_8), 
                                                                                                                                                                                                                1, 0) + ifelse(!is.na(pro_dysp_9), 1, 0) + ifelse(!is.na(pro_dysp_10), 
                                                                                                                                                                                                                                                                  1, 0)
  pro_dysp_n <- ifelse(pro_dysp_n <= 0 | is.na(pro_dysp_n), 
                       NA_real_, pro_dysp_n)
  return(pro_dysp_n)
}

calculate_promis_29_dyspnea_sum <- function (pro_dysp_1, pro_dysp_2, pro_dysp_3, pro_dysp_4, pro_dysp_5, 
          pro_dysp_6, pro_dysp_7, pro_dysp_8, pro_dysp_9, pro_dysp_10, 
          pro_dysp_n) 
{
  pro_dysp_sum <- ifelse(is.na(pro_dysp_1), 0, pro_dysp_1) + 
    ifelse(is.na(pro_dysp_2), 0, pro_dysp_2) + ifelse(is.na(pro_dysp_3), 
                                                      0, pro_dysp_3) + ifelse(is.na(pro_dysp_4), 0, pro_dysp_4) + 
    ifelse(is.na(pro_dysp_5), 0, pro_dysp_5) + ifelse(is.na(pro_dysp_6), 
                                                      0, pro_dysp_6) + ifelse(is.na(pro_dysp_7), 0, pro_dysp_7) + 
    ifelse(is.na(pro_dysp_8), 0, pro_dysp_8) + ifelse(is.na(pro_dysp_9), 
                                                      0, pro_dysp_9) + ifelse(is.na(pro_dysp_10), 0, pro_dysp_10)
  pro_dysp_sum <- ifelse(pro_dysp_n >= 4, pro_dysp_sum, NA_real_)
  return(pro_dysp_sum)
}

categorize_promis_29_dyspnea <- function (pro_dysp_sum) 
{
  pro_dysp_cat <- case_when(pro_dysp_sum < 15 ~ "No Dyspnea", 
                            pro_dysp_sum >= 15 & pro_dysp_sum < 20 ~ "Mild Dyspnea", 
                            pro_dysp_sum >= 20 & pro_dysp_sum < 28 ~ "Moderate Dyspnea", 
                            pro_dysp_sum >= 28 ~ "Severe Dyspnea")
  return(pro_dysp_cat)
}


categorize_promis_29_dyspnea_2 <- function (pro_dysp_sum) 
{
  pro_dysp_cat_2 <- case_when(pro_dysp_sum < 15 ~ "No Dyspnea", 
                              pro_dysp_sum >= 15 ~ "Dyspnea")
  return(pro_dysp_cat_2)
}

# I DO NOT WANT THIS
calculate_promis_cognitive_funct_sum <- function (pro_cogn_1, pro_cogn_2, pro_cogn_3, pro_cogn_4) 
{
  pro_cogn_sum <- pro_cogn_1 + pro_cogn_2 + pro_cogn_3 + pro_cogn_4
  return(pro_cogn_sum)
}

categorize_promis_cognitive_funct <- function (pro_cogn_sum) 
{
  pro_cogn_cat <- case_when(pro_cogn_sum <= 5 ~ "Severe cognitive impairment", 
                            pro_cogn_sum >= 6 & pro_cogn_sum <= 11 ~ "Moderate cognitive impairment", 
                            pro_cogn_sum >= 12 & pro_cogn_sum <= 14 ~ "Mild cognitive impairment", 
                            pro_cogn_sum >= 15 ~ "No cognitive impairments")
  return(pro_cogn_cat)
}

categorize_promis_cognitive_funct_2 <- function (pro_cogn_sum) 
{
  pro_cogn_cat_2 <- case_when(pro_cogn_sum < 15 ~ "Cognitive impairments", 
                              pro_cogn_sum >= 15 ~ "No cognitive impairments")
  return(pro_cogn_cat_2)
}



calculate_promis29_sleep_sum <- function (pro_sleep_1, pro_sleep_2, pro_sleep_3, pro_sleep_4) 
{
  pro_sleep_sum <- pro_sleep_1 + pro_sleep_2 + pro_sleep_3 + 
    pro_sleep_4
  return(pro_sleep_sum)
}

categorize_promis29_sleep <- function (pro_sleep_sum) 
{
  pro_sleep_cat <- case_when(pro_sleep_sum < 13 ~ "No sleep disturbance", 
                             pro_sleep_sum >= 13 & pro_sleep_sum <= 15 ~ "Mild sleep disturbance", 
                             pro_sleep_sum >= 16 & pro_sleep_sum <= 19 ~ "Moderate sleep disturbance", 
                             pro_sleep_sum >= 20 ~ "Severe sleep disturbance")
  return(pro_sleep_cat)
}

categorize_promis29_sleep_2 <-function (pro_sleep_sum) 
{
  pro_sleep_cat_2 <- case_when(pro_sleep_sum < 13 ~ "No sleep disturbance", 
                               pro_sleep_sum >= 12 ~ "Sleep disturbance")
  return(pro_sleep_cat_2)
}







# ================================================================================

build_pcs_score_suep_df_with_proms_without_cog <- function (trial_data, pid) 
{
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  pid <- trial_data$export_options$id_names$pid
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), 
                                 "mnpvislabel", "visit_name")
  ecu_pcs_score_3m <- calculate_pcs_score_suep(trial_data, 
                                               pid, days_of_pcss_time_diff = 61, vector_of_pcss_fup_visits = c("3M Follow-Up", 
                                                                                                               "12M Follow-Up"))
  ecu_pcs_score_12m <- calculate_pcs_score_suep(trial_data, 
                                                pid, days_of_pcss_time_diff = 335, vector_of_pcss_fup_visits = c("12M Follow-Up"))
  if (!("ecu_cfq11_sum" %in% names(trial_data$promext))) {
    trial_data <- primary_coding_suep_cfq11(trial_data, visitid)
  }
  trial_data <- primary_coding_suep_promis_29_fatigue(trial_data, 
                                                      visitid)
  trial_data <- primary_coding_suep_promis_29_dyspnea(trial_data, 
                                                      visitid)
  #trial_data <- primary_coding_suep_promis_cogn_funct(trial_data, 
  #                                                    visitid)
  trial_data <- primary_coding_suep_promis_29_sleep(trial_data, 
                                                    visitid)
  prom <- trial_data$prom %>% filter(!!sym(visit_label_var_name) == 
                                       "3M Follow-Up" | !!sym(visit_label_var_name) == "12M Follow-Up")
  promext <- trial_data$promext %>% filter(!!sym(visit_label_var_name) == 
                                             "3M Follow-Up" | !!sym(visit_label_var_name) == "12M Follow-Up")
  relevante_proms <- prom %>% left_join(promext, by = c(pid, 
                                                        visit_label_var_name)) %>% select(all_of(pid), all_of(visit_label_var_name), 
                                                                                          "cfs.factor", "ecu_cfq11_sum", "ecu_cfq11_cat", "cfs_seid_crit2.factor", 
                                                                                          "cfs_seid_crit4.factor", "cfs_seid_crit5.factor", "ecu_promis29_fatigue_sum", 
                                                                                          "ecu_promis29_fatigue_cat_2", "dysp.factor", "ecu_promis29_dyspnea_n", 
                                                                                          "ecu_promis29_dyspnea_sum", "ecu_promis29_dyspnea_cat_2", 
                                                                                          "pain_loc_chest.factor", "pain_loc_abd.factor", "pain_loc_head.factor", 
                                                                                          "pain_dn2_6.factor", #"ecu_promis_cogn_funct_sum", 
                                                                                          #"ecu_promis_cogn_funct_cat_2", 
                                                                                          "ecu_promis29_sleep_sum", "ecu_promis29_sleep_cat", "ecu_promis29_sleep_cat_2")
  ecu_pcs_score_3m <- ecu_pcs_score_3m %>% left_join(relevante_proms, 
                                                     by = c(pid, visit_label = visit_label_var_name)) %>% 
    mutate(complex_2_fatigue_sum_screen = case_when(.data$complex_2_fatigue_sum == 
                                                      1 | .data$cfs.factor == "Ja" ~ 1, TRUE ~ .data$complex_2_fatigue_sum), 
           complex_2_fatigue_sum_promis29 = case_when(.data$complex_2_fatigue_sum == 
                                                        1 | .data$ecu_promis29_fatigue_cat_2 == "Fatigue" ~ 
                                                        1, TRUE ~ .data$complex_2_fatigue_sum), complex_2_fatigue_sum_cfq11 = case_when(.data$complex_2_fatigue_sum == 
                                                                                                                                          1 | .data$ecu_cfq11_cat == "Fatigue" ~ 1, TRUE ~ 
                                                                                                                                          .data$complex_2_fatigue_sum), complex_2_fatigue_sum_all = case_when(.data$complex_2_fatigue_sum == 
                                                                                                                                                                                                                1 | .data$complex_2_fatigue_sum_screen == 1 | 
                                                                                                                                                                                                                .data$complex_2_fatigue_sum_promis29 == 1 | .data$complex_2_fatigue_sum_cfq11 == 
                                                                                                                                                                                                                1 ~ 1, TRUE ~ .data$complex_2_fatigue_sum), complex_3_exercise_sum_screen = case_when(.data$complex_3_exercise_sum == 
                                                                                                                                                                                                                                                                                                        1 | .data$dysp.factor == "Ja" | .data$cfs_seid_crit2.factor == 
                                                                                                                                                                                                                                                                                                        "Ja" ~ 1, TRUE ~ .data$complex_3_exercise_sum), 
           complex_3_exercise_sum_proms = case_when(.data$complex_3_exercise_sum == 
                                                      1 | .data$ecu_promis29_dyspnea_cat_2 == "Dyspnea" ~ 
                                                      1, TRUE ~ .data$complex_3_exercise_sum), complex_3_exercise_sum_all = case_when(.data$complex_3_exercise_sum == 
                                                                                                                                        1 | .data$complex_3_exercise_sum_screen == 1 | 
                                                                                                                                        .data$complex_3_exercise_sum_proms == 1 ~ 1, 
                                                                                                                                      TRUE ~ .data$complex_3_exercise_sum), complex_7_chest_sum_screen = case_when(.data$complex_7_chest_sum == 
                                                                                                                                                                                                                     1 | .data$pain_loc_chest.factor == "Ja" ~ 1, 
                                                                                                                                                                                                                   TRUE ~ .data$complex_7_chest_sum), complex_8_gastro_sum_screen = case_when(.data$complex_8_gastro_sum == 
                                                                                                                                                                                                                                                                                                1 | .data$pain_loc_abd.factor == "Ja" ~ 1, TRUE ~ 
                                                                                                                                                                                                                                                                                                .data$complex_8_gastro_sum), complex_9_neuro_sum_screen = case_when(.data$complex_9_neuro_sum == 
                                                                                                                                                                                                                                                                                                                                                                      1 | .data$cfs_seid_crit5.factor == "Ja" | .data$pain_loc_head.factor == 
                                                                                                                                                                                                                                                                                                                                                                      "Ja" | .data$pain_dn2_6.factor == "Ja" | .data$cfs_seid_crit4.factor == 
                                                                                                                                                                                                                                                                                                                                                                      "Ja" ~ 1, TRUE ~ .data$complex_9_neuro_sum), 
           complex_9_neuro_sum_proms = case_when(.data$complex_9_neuro_sum == 
                                                   1 #| .data$ecu_promis_cogn_funct_cat_2 == "Cognitive impairments" 
                                                 ~ 
                                                   1, TRUE ~ .data$complex_9_neuro_sum), complex_9_neuro_sum_all = case_when(.data$complex_9_neuro_sum == 
                                                                                                                               1 | .data$complex_9_neuro_sum_screen == 1 | .data$complex_9_neuro_sum_proms == 
                                                                                                                               1 ~ 1, TRUE ~ .data$complex_9_neuro_sum), 
          complex_12_sleep_sum_promis29 = case_when(.data$complex_12_sleep_sum == 
                                                                                                                                                                                                                     1 | .data$ecu_promis29_sleep_cat_2 == "Sleep disturbance" ~ 
                                                                                                                                                                                                                     1, TRUE ~ .data$complex_12_sleep_sum))
  ecu_pcs_score_12m <- ecu_pcs_score_12m %>% left_join(relevante_proms, 
                                                       by = c(pid, visit_label = visit_label_var_name)) %>% 
    mutate(complex_2_fatigue_sum_screen = case_when(.data$complex_2_fatigue_sum == 
                                                      1 | .data$cfs.factor == "Ja" ~ 1, TRUE ~ .data$complex_2_fatigue_sum), 
           complex_2_fatigue_sum_promis29 = case_when(.data$complex_2_fatigue_sum == 
                                                        1 | .data$ecu_promis29_fatigue_cat_2 == "Fatigue" ~ 
                                                        1, TRUE ~ .data$complex_2_fatigue_sum), complex_2_fatigue_sum_cfq11 = case_when(.data$complex_2_fatigue_sum == 
                                                                                                                                          1 | .data$ecu_cfq11_cat == "Fatigue" ~ 1, TRUE ~ 
                                                                                                                                          .data$complex_2_fatigue_sum), complex_2_fatigue_sum_all = case_when(.data$complex_2_fatigue_sum == 
                                                                                                                                                                                                                1 | .data$complex_2_fatigue_sum_screen == 1 | 
                                                                                                                                                                                                                .data$complex_2_fatigue_sum_promis29 == 1 | .data$complex_2_fatigue_sum_cfq11 == 
                                                                                                                                                                                                                1 ~ 1, TRUE ~ .data$complex_2_fatigue_sum), complex_3_exercise_sum_screen = case_when(.data$complex_3_exercise_sum == 
                                                                                                                                                                                                                                                                                                        1 | .data$dysp.factor == "Ja" | .data$cfs_seid_crit2.factor == 
                                                                                                                                                                                                                                                                                                        "Ja" ~ 1, TRUE ~ .data$complex_3_exercise_sum), 
           complex_3_exercise_sum_proms = case_when(.data$complex_3_exercise_sum == 
                                                      1 | .data$ecu_promis29_dyspnea_cat_2 == "Dyspnea" ~ 
                                                      1, TRUE ~ .data$complex_3_exercise_sum), complex_3_exercise_sum_all = case_when(.data$complex_3_exercise_sum == 
                                                                                                                                        1 | .data$complex_3_exercise_sum_screen == 1 | 
                                                                                                                                        .data$complex_3_exercise_sum_proms == 1 ~ 1, 
                                                                                                                                      TRUE ~ .data$complex_3_exercise_sum), complex_7_chest_sum_screen = case_when(.data$complex_7_chest_sum == 
                                                                                                                                                                                                                     1 | .data$pain_loc_chest.factor == "Ja" ~ 1, 
                                                                                                                                                                                                                   TRUE ~ .data$complex_7_chest_sum), complex_8_gastro_sum_screen = case_when(.data$complex_8_gastro_sum == 
                                                                                                                                                                                                                                                                                                1 | .data$pain_loc_abd.factor == "Ja" ~ 1, TRUE ~ 
                                                                                                                                                                                                                                                                                                .data$complex_8_gastro_sum), complex_9_neuro_sum_screen = case_when(.data$complex_9_neuro_sum == 
                                                                                                                                                                                                                                                                                                                                                                      1 | .data$cfs_seid_crit5.factor == "Ja" | .data$pain_loc_head.factor == 
                                                                                                                                                                                                                                                                                                                                                                      "Ja" | .data$pain_dn2_6.factor == "Ja" | .data$cfs_seid_crit4.factor == 
                                                                                                                                                                                                                                                                                                                                                                     "Ja" ~ 1, TRUE ~ .data$complex_9_neuro_sum), 
           complex_9_neuro_sum_proms = case_when(.data$complex_9_neuro_sum == 
                                                   1 #| .data$ecu_promis_cogn_funct_cat_2 == "Cognitive impairments" 
                                                 ~  1, 
          
          TRUE ~ .data$complex_9_neuro_sum
          ), complex_9_neuro_sum_all = case_when(.data$complex_9_neuro_sum == 
                                                                                                                               1 | .data$complex_9_neuro_sum_screen == 1 | .data$complex_9_neuro_sum_proms == 
                                                                                                                               1 ~ 1, TRUE ~ .data$complex_9_neuro_sum), 
          complex_12_sleep_sum_promis29 = case_when(.data$complex_12_sleep_sum == 
                                                                                                                                                                                                                     1 | .data$ecu_promis29_sleep_cat_2 == "Sleep disturbance" ~ 
                                                                                                                                                                                                                     1, TRUE ~ .data$complex_12_sleep_sum))
  ecu_pcs_score_3m <- ecu_pcs_score_3m %>% filter(.data$visit_label == 
                                                    "3M Follow-Up") %>% mutate(pcs_score_sum_with_proms_without_cog = if_else(.data$complex_1_chemo_sum == 
                                                                                                                    1, 3.5, 0) + if_else(.data$complex_2_fatigue_sum_all == 
                                                                                                                                           1, 7, 0) + if_else(.data$complex_3_exercise_sum_all == 
                                                                                                                                                                1, 4, 0) + if_else(.data$complex_4_pain_sum == 1, 6.5, 
                                                                                                                                                                                   0) + if_else(.data$complex_5_ent_sum == 1, 5.5, 0) + 
                                                                                 if_else(.data$complex_6_cough_sum == 1, 7, 0) + if_else(.data$complex_7_chest_sum_screen == 
                                                                                                                                           1, 3.5, 0) + if_else(.data$complex_8_gastro_sum_screen == 
                                                                                                                                                                  1, 5, 0) + if_else(.data$complex_9_neuro_sum_all == 1, 
                                                                                                                                                                                     6.5, 0) + 
                                                                                 if_else(.data$complex_10_derma_sum == 1, 2, 
                                                                                                                                                                                                       0) + if_else(.data$complex_11_flulike_sum == 1, 3.5, 
                                                                                                                                                                                                                    0) + if_else(.data$complex_12_sleep_sum_promis29 == 1, 
                                                                                                                                                                                                                                 5, 0), pcs_score_group_with_proms = cut(.data$pcs_score_sum_with_proms_without_cog, 
                                                                                                                                                                                                                                                                         breaks = c(-Inf, 0, 10.75, 26.25, Inf), labels = c("0", 
                                                                                                                                                                                                                                                                                                                            "<=10,75", "10,75<x<=26,25", ">26,25")))
  ecu_pcs_score_12m <- ecu_pcs_score_12m %>% mutate(pcs_score_sum_with_proms_without_cog = if_else(.data$complex_1_chemo_sum == 
                                                                                         1, 3.5, 0) + if_else(.data$complex_2_fatigue_sum_all == 
                                                                                                                1, 7, 0) + if_else(.data$complex_3_exercise_sum_all == 
                                                                                                                                     1, 4, 0) + if_else(.data$complex_4_pain_sum == 1, 6.5, 
                                                                                                                                                        0) + if_else(.data$complex_5_ent_sum == 1, 5.5, 0) + 
                                                      if_else(.data$complex_6_cough_sum == 1, 7, 0) + if_else(.data$complex_7_chest_sum_screen == 
                                                                                                                1, 3.5, 0) + if_else(.data$complex_8_gastro_sum_screen == 
                                                                                                                                       1, 5, 0) + #if_else(.data$complex_9_neuro_sum_all == 1, 
                                                                                                                                                         # 6.5, 0) + 
                                                      if_else(.data$complex_10_derma_sum == 1, 2, 
                                                                                                                                                                            0) + if_else(.data$complex_11_flulike_sum == 1, 3.5, 
                                                                                                                                                                                         0) + if_else(.data$complex_12_sleep_sum_promis29 == 1, 
                                                                                                                                                                                                      5, 0), pcs_score_group_with_proms = cut(.data$pcs_score_sum_with_proms_without_cog, 
                                                                                                                                                                                                                                              breaks = c(-Inf, 0, 10.75, 26.25, Inf), labels = c("0", 
                                                                                                                                                                                                                                                                                                 "<=10,75", "10,75<x<=26,25", ">26,25")))
  ecu_pcs_score <- ecu_pcs_score_3m %>% full_join(ecu_pcs_score_12m) %>% 
    select(all_of(pid), "visit_label", contains("pcs_score"))
  return(ecu_pcs_score)
}

primary_coding_suep_pcs_score_without_cog <- function (trial_data, prom = "No") 
{
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  pid <- trial_data$export_options$id_names$pid
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), 
                                 "mnpvislabel", "visit_name")
  trial_data[["ecu_long_symptom_data"]] <- build_suep_long_symptom_df(trial_data, 
                                                                      pid)
  if (prom == "No") {
    trial_data[["ecu_pcs_score"]] <- build_pcs_score_suep_df_without_proms(trial_data, 
                                                                           pid) %>% labelled::remove_var_label()
  }
  else if (prom == "Yes") {
    trial_data[["ecu_pcs_score"]] <- build_pcs_score_suep_df_with_proms_without_cog(trial_data, 
                                                                          pid) %>% labelled::remove_var_label()
  }
  return(trial_data)
}
