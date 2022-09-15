
# DEMOGRAPHICS TABLE ------------------------------------------------------
theme_gtsummary_compact()
# foot <- data %>%
#   group_by(region) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   mutate(n =paste0(n, "(", round(n/sum(n)*100, digits = 2), "%)"))


household_info<- read.csv("S:\\CoronaWatch\\Working\\data_cleaning_cleaned\\Baseline\\3_Output\\baseline_household_info.csv") %>% 
  select(household_id, no_of_householders) %>% 
  mutate(household_id = as.character(household_id))

data <- left_join(data, household_info, by = "household_id") 


tab <- data %>% 
  mutate_all(., list(~na_if(.,""))) %>% 
  select( age3,sex_bin,no_of_householders,household_n_cases, region , nVar, start_dt) %>% # region,
  rename(AGE = age3,
         SEX = sex_bin,
         "# OF HOUSEHOLD MEMBERS" = no_of_householders,
         "# OF CASES PER HOUSEHOLD" = household_n_cases,
         "REGION" = region,
         "DATE OF ONSET" = start_dt) %>% 
  tbl_summary(by = nVar,
              missing = "ifany",
              missing_text = "(Missing)") %>% 
  add_overall() %>% 
  bold_labels() %>% 
  italicize_levels()

tab %>% as_flex_table() %>% 
  flextable::save_as_docx(path = paste0(save_path, "table_demographics.docx"))

as_gt(tab) %>% 
  gtsave(filename = "table_demographics.html", path = save_path )
