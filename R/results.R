library(crayon)
#FROM RAW ILLNESS EPISODES DATA
#
cat(red("############################\n",
        "       RAW DATA\n",
        "############################\n"))
cat("how many illnesses?",
    green(nrow(raw_data)),
    sep = "\n")

cat("how many individuals?",
    green(length(unique(raw_data$individual_id))),
    sep = "\n")


cat("how many households?",
    green(length(unique(raw_data$household_id))),
    sep = "\n")


#date interval
raw_data$start_dt<- as.Date(raw_data$start_dt, format="%d%b%Y")
raw_data$end_dt2<- as.Date(raw_data$end_dt, format="%d%b%Y")
min(raw_data$start_dt) 
max(raw_data$end_dt2)

cat("date interval?",
    green("from:", paste0(min(raw_data$start_dt)),
    "to:", paste0(max(raw_data$start_dt))),
    sep = "\n")


cat("how many swabbed?",
    green(paste0(table(raw_data$vw_sgss_swab),"\n")),
    "Amongst swabbed, how many positive?",
    green(table(raw_data$vw_sgss_result)[2]),
    sep = "\n")

cat(red("\n ############################\n",
        "       DATA\n",
        "############################\n"))

cat("how many illnesses in our analysis",
    green(length(unique(data$illnessid))),
    sep = "\n")

cat("how many positive individuals eligible for our analysis ?",
    green(length(unique(data$individual_id))),
    sep = "\n")

cat("how many  households?",
    green(length(unique(data$household_id))),
    sep = "\n")


cat("date interval from earliest COVID+ illness symptom onset to lastest?",
    green("from:", paste0(min(data$start_dt) ),
    "to:", paste0(max(data$start_dt))),
    sep = "\n")


cat("how many serial interval estimates ?",
   green(nrow(all_si_df)),
    sep = "\n")

data %>% 
  group_by(nVar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n) * 100)

data %>% 
  group_by(age3) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n) * 100)


data %>% 
  group_by(no_of_householders) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n) * 100)

data %>% 
  group_by(household_n_cases) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n) * 100)
data %>% 
group_by(region) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n) * 100)

tab_cdf %>% 
  select(-mean) %>% 
  pivot_longer(cols = -serial_interval) %>% 
  group_by(name) %>% 
  filter(value >= 0.9) %>% 
  slice_min(serial_interval)

tab_cdf %>% 
  filter(mean >0.9)
