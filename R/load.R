# LOAD RAW DATA -------------------------------------------------------------------
#contains all illness episodes
#regardless of COVID test result

data_dir <-
  "S:/CoronaWatch/Working/data_cleaning_cleaned/Lab/All_positives_master/"

illness_episodes <-
  "Episode_Data_With_SGSS_Seven_Days.csv"
 # "Episode_Data_With_SGSS.csv" #name of file

index <-
  file_index(directory = data_dir) %>%
  filter(file_name == illness_episodes)

data_update <- index %>% .$date_modified %>% as.Date()

raw_data <- index %>% .$file_path %>% read.csv()

saveRDS(raw_data, file = paste0(save_path, "raw_data.RData") )

cat(
  "Input data last updated on:\n",
  paste0(data_update),"\n",
  as.Date(Sys.Date()) - data_update,"day(s) ago\n", "\n",
  paste0("saved in ", save_path)
)


# CLEAN DATA --------------------------------------------------------------

data_pos <- raw_data %>%                              
  drop_na(household_id) %>%                  
  filter(vw_sgss_result == 1) %>% 
  mutate( #    individual_id = stringi::stri_sub_replace(newID, nchar(newID), nchar(newID)-1, value = "hh"),
    across(c(start_dt, end_dt), ~ as.Date(.x, format = "%d%b%Y")),
    across(c(illnessid, newID,individual_id,household_id, nVar), 
           as.character),
    nVar = case_when(
      nVar == "[0] Wild Type" ~ "[1] Wild Type",
      nVar == "[1] Alpha" ~ "[2] Alpha",
      nVar == "[2] Delta" ~ "[3] Delta",
      TRUE ~ NA_character_)
  ) %>%
  mutate(nVar = case_when( start_dt >= as.Date("2021-12-14") & start_dt <= as.Date("2022-02-06") ~ "[4] Omicron BA1",
                            start_dt >= as.Date("2022-02-27") & start_dt <= as.Date("2022-05-27") ~ "[5] Omicron BA2",
                           start_dt > as.Date("2022-07-03") ~ "[6] Omicron BA5",
                           TRUE ~ nVar)) %>% #Omicron overtook the whole of UK quickly, no need for geographical strata
  group_by(household_id) %>%
  arrange(start_dt, .by_group = TRUE) %>%
  mutate(case_type = ifelse(row_number() == 1, "index_case", "secondary_case")) %>%
  as.data.frame() %>% 
  drop_na(nVar) 


data_pos <- data_pos %>%
  dplyr::select(
    illnessid,
    individual_id,
    household_id,
    start_dt,
    end_dt,
    vw_sgss_result,
    nVar,
    region,
    sex_bin,
    hh_age_on_entry,
    age3,
    case_type
  ) 


# 14 DAY FOLLOW-UP --------------------------------------------------------
## removing households where  index case occurs within
## two weeks of survey data to allow for complete follow-up

#get the survey date (1st monday from the data_update date)
last_monday <-
  function(x) {
    day<- 7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
    return(day)
  }

date_limit <-
  last_monday(data_update) - 14 #2 weeks before our survey date

households_to_exclude <- data_pos %>%
  filter(case_type == "index_case") %>% #get all index cases
  filter(start_dt >= date_limit) %>%    # extract those who's symptom onset date is within 2 weeks of survey date
  .$household_id %>% unique()           # extract the household ids


## remove households where index case report symptom onset 
## within 2 weeks of survey date
data_pos <- data_pos %>%
  filter(!(household_id %in% households_to_exclude))




# FINAL DATA --------------------------------------------------------------
## For households that report a single symptom onset start date: 
#we remove 1 day to a random individual for outbreaker to run but will 
#use actual symptom dates to compute the serial interval.
#Outbreaker results are not affected by this.

data <- data_pos %>%
  group_by(household_id) %>%
  arrange(start_dt) %>%
  mutate(
    household_n_cases = n_distinct(individual_id),
    single_start_dt = ifelse(n_distinct(start_dt)==1, TRUE, FALSE),
    start_dt_minus1 = if_else(single_start_dt == TRUE & row_number()==1L, start_dt -1, start_dt)
  ) %>% 
  ungroup() %>% 
  filter(household_n_cases >= 2 )


#recoding nVar without numbering
data <- data %>% 
  mutate(nVar = str_replace(nVar, "\\[", ""),
         nVar = str_replace(nVar, "\\d", ""),
         nVar = str_replace(nVar, "\\] ", ""),
         nVar = factor(nVar, levels = c("Wild Type", "Alpha", "Delta", "Omicron BA1", "Omicron BA2", "Omicron BA5")))


rm(index, illness_episodes, last_monday,
   households_to_exclude, date_limit, data_pos)
cat("\nLoad.R ran successfully\n")
