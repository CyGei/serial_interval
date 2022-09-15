
# DATA --------------------------------------------------------------------


#variant dates
variants <- data.frame(nVar = c("Wild Type", "Alpha", "Delta", "Omicron BA1", "Omicron BA2", "Omicron BA5"),
                       start = as.Date(c("2020-09-01", "2020-12-10", "2021-06-03", "2021-12-14", "2022-03-06", "2022-07-04")),
                       end = as.Date(c("2020-12-08", "2021-05-03", "2021-12-13", "2022-03-05", "2022-05-27", "2022-08-11")))

#lockdown dates
lockdowns <- data.frame(lockdown = c("2nd", "3rd"),
                        start_date = as.Date(c("2020-11-04","2021-01-06" )),
                        end_date = as.Date(c("2020-12-02","2021-03-29" )))


data <- data %>% 
  mutate(lockdown = case_when(between(start_dt,lockdowns$start_date[1], lockdowns$end_date[1]) ~ "Lockdown",
                              between(start_dt,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "Lockdown",
                              TRUE ~ "No Lockdown" )) 

#vaccine
vaccine_file <- "S:/CoronaWatch/Working/data_cleaning_cleaned/HES/3_Output/NIMS_VW_vaccination_data.csv"
vaccine_data <- read.csv(vaccine_file) %>% 
  select(all_of(c("individual_id","vaccine_nhs_dose_when_1_min", "vaccine_nhs_dose_when_2_min"))) %>%  
  filter(individual_id %in% unique(data$individual_id)) %>% 
  rename(dose1 = vaccine_nhs_dose_when_1_min ,
         dose2 = vaccine_nhs_dose_when_2_min ) %>% 
  pivot_longer(cols = -individual_id, names_to = "dose", values_to = "date") %>% 
  mutate(date = na_if(date, ""))


# PROPORTION OF TESTED ILLNESSES --------------------------------------------

raw_data_clean <- raw_data %>% 
  drop_na(household_id) %>%                  
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
  as_tibble() %>%
  filter( start_dt >= as.Date("2020-09-01") & start_dt <= as.Date("2022-08-10"))%>%  #study period 
  drop_na(nVar) 


#proportion % of tested illnesses: 
prop_tested <- raw_data_clean %>% 
  mutate(vw_sgss_swab = ifelse(is.na(vw_sgss_swab), "0", vw_sgss_swab)) %>%
  drop_na(vw_sgss_swab) %>% 
  group_by(start_dt, vw_sgss_swab) %>% 
  summarise(n = n()) %>% 
  group_by(start_dt) %>% 
  mutate(total_day = sum(n)) %>% 
  ungroup() %>% 
  filter(vw_sgss_swab == "0") %>%
  mutate(prop_n = n/total_day,
         prop_p = 1 - prop_n,
         roll = zoo::rollmean(prop_p, k = 7, fill = NA),
         lockdown = case_when(between(start_dt,lockdowns$start_date[1], lockdowns$end_date[1]) ~ "Lockdown",
                              between(start_dt,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "Lockdown",
                              TRUE ~ "No Lockdown" ))
  
  
#plot:
p_prop_tested <- ggplot()+
  geom_col(data = prop_tested, aes(x = start_dt, y = prop_p), col = "grey", alpha = 0.5)+
  geom_line(data = prop_tested, aes(x = start_dt, y = roll, col = lockdown, group = 1))+
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.3)+
  scale_fill_manual(values = .CoV_pal)+
  scale_color_manual(values = c("#ff6361", "black"))+
  scale_y_continuous(breaks = seq(0,8, by = 0.1), labels = scales::percent)+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"))+
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.ontop = TRUE)+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))+
  labs(x = "", y = "frequency", fill = "Variant:", col = "")

p_prop_tested

# daily proportion of positive swabs
prop_positive <- 
  raw_data_clean %>% 
  filter(vw_sgss_swab  == "1") %>% 
  mutate(COVID_test = case_when(vw_sgss_result == 0 ~ "negative",
                                vw_sgss_result == 1 ~ "positive",
                                is.na(vw_sgss_result) ~ NA_character_)) %>% 
  drop_na(COVID_test) %>% 
  group_by(start_dt, COVID_test) %>% 
  summarise(n = n()) %>% 
  group_by(start_dt) %>% 
  mutate(total_day = sum(n)) %>% 
  ungroup() %>% 
  filter(COVID_test == "negative") %>% 
  mutate(prop_n = n/total_day,
         prop_p = 1 - prop_n,
         roll = zoo::rollmean(prop_p, k = 7, fill = NA),
         lockdown = case_when(between(start_dt,lockdowns$start_date[1], lockdowns$end_date[1]) ~ "Lockdown",
                              between(start_dt,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "Lockdown",
                              TRUE ~ "No Lockdown" ))


#plot:
p_prop_positive <- ggplot()+
  geom_col(data = prop_positive, aes(x = start_dt, y = prop_p), col = "grey", alpha = 0.5)+
  geom_line(data = prop_positive, aes(x = start_dt, y = roll, col = lockdown, group = 1))+
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.3)+
  scale_fill_manual(values = .CoV_pal)+
  scale_color_manual(values = c("#ff6361", "black"))+
  scale_y_continuous(breaks = seq(0,8, by = 0.1), labels = scales::percent)+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"))+
  theme(legend.position = "top",
        panel.ontop = TRUE)+
  labs(x = "", y = "frequency", fill = "Variant:", col = "")+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))
  


p_prop_positive




# VirusWatch Epidemic Curve -----------------------------------------------

## INCIDENCE
incid <- data %>% 
  group_by(nVar, lockdown, start_dt) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(start_dt) %>% 
  mutate(cum_n = cumsum(n),
         n_roll = zoo::rollmean(n, k = 7, fill = NA))



#CUMULATIVE INCIDENCE
p_cumulative_incid <- ggplot()+
  geom_line(data = incid,  aes(x = start_dt, y =cum_n, col = lockdown, group = 1), size = 0.7)+
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.4)+
  scale_color_manual(values = c("red", "black"))+
  scale_fill_manual(values = .CoV_pal)+
  scale_y_continuous(breaks = seq(0,6000, 1000))+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"))+
  labs(y = "cumulative count", x = "", fill = "", color = "")+
  theme(legend.position = "top",
        panel.ontop = TRUE)+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))
p_cumulative_incid



#EPIDEMIC CURVE
p_epicurve <- ggplot()+
  
  #daily incidence:
  geom_col(data = incid,  aes(x = start_dt, y =n), col = "grey")+
  
  # 7 day average:
  geom_line(data = incid,  aes(x = start_dt, y = n_roll, col = lockdown, group = 1), size = 0.65)+
  
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.3)+
  scale_color_manual(values = c("red", "black"))+
  scale_fill_manual(values = .CoV_pal)+
  scale_y_continuous(breaks = seq(0,150, 25))+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"),
        legend.position = "top",
        panel.ontop = TRUE)+
  labs(y = "daily incidence", x = "", fill = "Variant:", color = "")+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))





# VACCCINE ----------------------------------------------------------------


p_vaccine <- vaccine_data %>% 
  drop_na(date) %>% 
  group_by(date, dose) %>% 
  count() %>% 
  group_by(dose) %>% 
  mutate(cumsum = cumsum(n)) %>% 
  ggplot()+
  geom_line(aes(x = as.Date(date), y = cumsum, group = dose, col = dose), size = 0.8)+
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.3)+
  scale_color_manual(values = c("forestgreen", "purple"))+
  scale_y_continuous(breaks = seq(0,5000, 1000))+
  scale_fill_manual(values = .CoV_pal)+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"),
        legend.position = "top")+
  labs(y = "cumulative count", x = "", fill = "Variant:", color = "Vaccine:")+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))






# SAVE --------------------------------------------------------------------

p_l<- list(p_prop_tested,
       p_prop_positive,
       p_cumulative_incid,
       p_epicurve,
       p_vaccine)
names(p_l) <- c("p_prop_tested",
                "p_prop_positive",
                "p_cumulative_incid",
                "p_epicurve",
                "p_vaccine")


for (i in names(p_l)) {
  ggsave(p_l[[i]], file = paste0(save_path, i, ".svg"))
}

