##isolating single start dates households

ssd_household <- data %>% 
  filter(single_start_dt == TRUE) %>% 
  .$household_id %>% 
  unique(.)
length(ssd_household) / length(unique(data$household_id))


#generating the 0 SI values 
si0 <- data %>% 
  filter(household_id %in% ssd_household) %>% 
  select(household_id , household_n_cases, nVar ) %>% 
  group_by(household_id) %>% 
  mutate(nVar = nVar[1]) %>% 
  distinct() %>% 
  mutate(rep0 = (household_n_cases - 1)*19000, #190 posterior samples * 100 (natural histories aka LHS)
         rep_step = (household_n_cases-1)*100, #adding steps 100 natural histories for 190 samples from 550:10000 by 50,
         rep_lhs = (household_n_cases - 1)*190, # number of LHS counts
         step = list(rep(seq(550, 10000, by = 50), each = rep_step)), 
         serial_interval = list(rep(0, times = rep0)),
         LHS = list(rep(1:100, times = rep_lhs ))) %>% 
  ungroup() %>% 
  select(-c(household_n_cases,rep0, rep_step, rep_lhs)) %>% 
  unnest(cols = c(serial_interval, step, LHS)) %>% 
  rename(household = household_id)#get matching column 



