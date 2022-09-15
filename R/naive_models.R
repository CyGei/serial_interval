#############################
# Naive Models vs Oubtraker
#############################
#Compare Pairwise, Theoretical and Outbreaker:


# PAIRWISE ----------------------------------------------------------------
# Pairwise model solely depends on data
# Only one line will be produced

#compute pairwise SI across all data (no nVar grouping):
pairwise_si <- data %>% 
  select(household_id, start_dt) %>% 
  group_by(household_id) %>% 
  group_by(household_id) %>% 
  summarise(start_dt_l = list(start_dt)) %>% 
  ungroup() %>% 
  mutate(si = map(start_dt_l, ~as.matrix(outer(.x,.x, "-"))),
         si_nodiag = map(si, function(x){
           diag(x) = NA
           x <- x %>% 
             as.vector() %>% 
             na.omit()
           return(x)
         })) %>% 
  select(-c(start_dt_l, si)) %>% 
  unnest(cols = si_nodiag) %>% 
  filter(si_nodiag >= lower_si & si_nodiag <= upper_si)#filter the SI bounds
  

#compute empirical pairwise SI density:  
dens_pairwise <- pairwise_si %>% 
  select(si_nodiag) %>%
  summarise(density = list(hist(si_nodiag, breaks = seq(lower_si-0.5, upper_si+0.5), plot = FALSE))) %>% 
  mutate(density.x = map(density, ~.x[["mids"]]),
         density.y = map(density, ~.x[["density"]])) %>% 
  select(-density) %>% 
  unnest(cols = c(starts_with("density")))



# THEORETICAL SI MODEL CrI ----------------------------------------------------
# SI = GT + Incub Infectee - Incub Infector

# Theoretical SI densities
head(params_df) #from LHS section
theoretical_si <- tibble(params_df) %>% 
  rowwise() %>% 
  mutate(GT_rand = list(rgamma(10000, shape = gt_shape, scale = gt_scale)),
         INCUB_rand_infector = list(rgamma(10000, shape = incub_shape , scale = incub_scale)),
         INCUB_rand_infectee = list(rgamma(10000, shape = incub_shape , scale = incub_scale)),
         SI_rand = list(GT_rand + INCUB_rand_infectee - INCUB_rand_infector),
         density.x = list(density(SI_rand)$x),
         density.y = list(density(SI_rand)$y)) %>% 
  ungroup() %>% 
  select(starts_with("density")) %>% 
  mutate(LHS = row_number()) %>% 
  unnest(cols = starts_with("density"))

dens_theoretical_si <- theoretical_si %>% 
  mutate(density.x = round(density.x)) %>%
  group_by(density.x) %>% 
  summarise(mean = mean(density.y),
            lb = quantile(density.y, probs = 0.025),
            ub = quantile(density.y, probs = 0.975))


# OUTBREAKER CrI --------------------------------------------------------------


dens_all_si_nofilter <- nested_si_nofilter  %>% 
  mutate(serial_interval = map(serial_interval, ~.x[ .x %in% lower_si:upper_si] )) %>% 
  group_by(LHS, step, nVar) %>% 
  mutate(density = map(serial_interval, ~hist(.x, breaks = seq(lower_si - 0.5, upper_si + 0.5), plot = FALSE))) %>% 
  mutate(density.x = map(density, ~.x[["mids"]]),
         density.y = map(density, ~.x[["density"]])) %>% 
  select(-density) %>% 
  unnest(cols = c(starts_with("density"))) %>% 
  group_by(density.x ) %>% 
  summarise(est = mean(density.y),
            lwr = quantile(density.y, prob = 0.025),
            upr = quantile(density.y, prob = 0.975))  

# PLOT ALL SI MODELS ------------------------------------------------------

###################
# CrI
###################


.naive_col_pal = c(
  "Theoretical" = "#ffa500",
  "Outbreaker" = "#e0218a" , 
  "Pairwise" = "#377EB8"
  )
.naive_line_pal = c(
  "Theoretical" = "solid",
  "Outbreaker" = "solid",
  "Pairwise" = "solid"
 )

p_naive_models <- 
  ggplot()+
    
  #set breaks:
  scale_x_continuous(breaks = seq(lower_si, upper_si, 5), limits = c(lower_si, upper_si)) +
    

  #pairwise empirical:
  geom_point(data = dens_pairwise, aes(x = density.x, y = density.y, col = "Pairwise"), size = 2) +
  geom_segment(data = dens_pairwise, aes(x = density.x, xend = density.x, y = 0, yend = density.y, col = "Pairwise")) +
  
    
  #theoretical si  
  geom_line(data = dens_theoretical_si, aes(x = density.x, y = mean, col = "Theoretical", lty = "Theoretical"), size = 1) +
  geom_ribbon(data = dens_theoretical_si, aes(x = density.x, ymin = lb, ymax = ub, fill = "Theoretical"), alpha = 0.6) +
  
  
  #outbreaker empirical
  geom_point(data = dens_all_si_nofilter, aes(x = density.x, y = est, col = "Outbreaker"),  size = 2) +
  geom_segment(data = dens_all_si_nofilter, aes(x = density.x, xend = density.x, y = 0, yend = est, col = "Outbreaker")) +
  #geom_ribbon(data = dens_all_si_nofilter, aes(x = density.x, ymin = lwr, ymax = upr, fill = "Outbreaker"), alpha = 0.6) +
    

  #theme:  
  theme_fira()+
  theme(legend.position = "top")+
  scale_linetype_manual(values = .naive_line_pal,  guide = "none")+
  scale_color_manual(values = .naive_col_pal,  guide = "none")+
  scale_fill_manual(values = .naive_col_pal)+
  labs(fill = "", col = "", lty="", x = "serial interval (days)", y = "probability")
  

p_naive_models



# BARS --------------------------------------------------------------------

myalpha = 0.5
mycol = "white"
mywidth = 0.3

p_naive_models <- ggplot()+
  
  #set breaks:
  scale_x_continuous(breaks = seq(lower_si, upper_si, 5), limits = c(lower_si, upper_si)) +
  
  
  #pairwise empirical:
  geom_col(data = dens_pairwise, aes(x = density.x, y = density.y, fill = "Pairwise"), size = 1, alpha = myalpha, color = mycol) +
  
  #theoretical si  
  geom_col(data = dens_theoretical_si, aes(x = density.x, y = mean, fill = "Theoretical"), size = 1, alpha = myalpha,  color = mycol) +
  geom_errorbar(data = dens_theoretical_si, aes(x = density.x, ymin = lb, ymax = ub, col = "Theoretical"), 
                width = mywidth)+
  
  
  #outbreaker empirical
  geom_col(data = dens_all_si_nofilter, aes(x = density.x, y = est, fill = "Outbreaker"),  size = 1, alpha = myalpha,  color = mycol) +
  # geom_segment(data = dens_all_si_nofilter, aes(x = density.x, xend = density.x, y = 0, yend = est, col = "Outbreaker")) +
  geom_errorbar(data = dens_all_si_nofilter, aes(x = density.x, ymin = lwr, ymax = upr, col = "Outbreaker"), width = mywidth) +
  # 
  #theme:  
  theme_fira()+
  theme(legend.position = "top")+
  scale_linetype_manual(values = .naive_line_pal,  guide = "none")+
  scale_color_manual(values = .naive_col_pal,  guide = "none")+
  scale_fill_manual(values = .naive_col_pal)+
  labs(fill = "", col = "", lty="", x = "serial interval (days)", y = "probability")

p_naive_models


# SAVE --------------------------------------------------------------------

ggsave(p_naive_models, file = paste0(save_path, "p_naive_models.svg"))


