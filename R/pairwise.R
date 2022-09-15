
# PAIRWISE SI -------------------------------------------------------------

#outer() computes all  combinations of infector/infectee pairs with equal probabilities  
#We computes their serial interval
#we remove the diagonal since an infector cannot infect itself

#BY VARIANTS
#compute pairwise SI:
pairwise_si <- data %>% 
  select(household_id, start_dt, nVar) %>% 
  group_by(household_id) %>% 
  mutate(nVar = nVar[1]) %>% 
  group_by(household_id, nVar) %>% 
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
  unnest(cols = si_nodiag) 

#compute densities of pairwise SI:  
dens_pairwise <- pairwise_si %>% 
  select(nVar, si_nodiag) %>%
  filter(si_nodiag >= lower_si & si_nodiag <= upper_si) %>% 
  group_by(nVar) %>% 
  summarise(density = list(hist(si_nodiag, breaks = seq(lower_si-0.5, upper_si+0.5), plot = FALSE))) %>% 
  mutate(density.x = map(density, ~.x[["mids"]]),
         density.y = map(density, ~.x[["density"]])) %>% 
  select(-density) %>% 
  unnest(cols = c(starts_with("density")))





# OUTBREAKER SI ----------------------------------------------------
#compute densities of outbreaker SI:  
dens_all_si_nofilter <- nested_si_nofilter  %>% 
  mutate(serial_interval = map(serial_interval, ~.x[ .x %in% lower_si:upper_si] )) %>% 
  group_by(LHS, step, nVar) %>% 
  mutate(density = map(serial_interval, ~hist(.x, breaks = seq(lower_si - 0.5, upper_si + 0.5), plot = FALSE))) %>% 
  mutate(density.x = map(density, ~.x[["mids"]]),
         density.y = map(density, ~.x[["density"]])) %>% 
  select(-density) %>% 
  unnest(cols = c(starts_with("density"))) %>% 
  group_by(nVar,density.x ) %>% 
  summarise(est = mean(density.y),
            lwr = quantile(density.y, prob = 0.025),
            upr = quantile(density.y, prob = 0.975))  


# PLOTS -------------------------------------------------------------------
#Compare the two results
#Plots:
#as histogram
p_pairwise_hist <- 
  ggplot() +
  geom_col(
    data = dens_all_si_nofilter,
    aes(x = density.x, y = est, fill = "outbreaker2"),
    alpha = 0.5,
    col = "white"
  )+
  geom_errorbar( data = dens_all_si_nofilter,
                 aes(x = density.x, ymin = lwr, ymax = upr),
                 col = "#E41A1C")+ #F8766d
  geom_col(
    data = dens_pairwise,
    aes(x = density.x, y = density.y, fill = "pairwise"),
    alpha = 0.5,
    col = "white"
  )+
  facet_wrap(~nVar) +
  theme_fira()+
  scale_fill_brewer(palette = "Set1",
                    labels = c( "outbreaker2", "pairwise"))+
  scale_x_continuous(breaks = seq(lower_si, upper_si, 10))+
  theme(legend.position = "top")+
  labs(x = "serial interval (days)", y = "probability",
       fill = "", col = "")


p_pairwise_hist




ggplot() +
  geom_point(
    data = dens_all_si_nofilter,
    aes(x = density.x, y = est, color = "outbreaker2"),
    alpha = 0.5
  )+
  geom_errorbar( data = dens_all_si_nofilter,
                 aes(x = density.x, ymin = lwr, ymax = upr),
                 col = "#E41A1C")+ #F8766d
  geom_point(
    data = dens_pairwise,
    aes(x = density.x, y = density.y, color = "pairwise"),
    alpha = 0.5
  )+
  facet_wrap(~nVar) +
  theme_fira()+
  scale_color_brewer(palette = "Set1",
                    labels = c( "outbreaker2", "pairwise"))+
  scale_x_continuous(breaks = seq(lower_si, upper_si, 10))+
  theme(legend.position = "top")+
  labs(x = "serial interval (days)", y = "probability",
       fill = "", col = "")


# OVERALL PAIRWISE DISTRIBUTION -----------------------------------------------------------------

dens_pairwise <- pairwise_si %>% 
  select(nVar, si_nodiag) %>%
  filter(si_nodiag >= lower_si & si_nodiag <= upper_si) %>% 
  summarise(density = list(hist(si_nodiag, breaks = seq(lower_si-0.5, upper_si+0.5), plot = FALSE))) %>% 
  mutate(density.x = map(density, ~.x[["mids"]]),
         density.y = map(density, ~.x[["density"]])) %>% 
  select(-density) %>% 
  unnest(cols = c(starts_with("density")))


p_pairwise_hist_overall <- 
  ggplot() +
  geom_col(
    data = dens_pairwise,
    aes(x = density.x, y = density.y),
    fill = "#377EB8",
    alpha = 0.5,
    col = "black"
  )+
  theme_fira()+
  scale_x_continuous(breaks = seq(lower_si, upper_si, 10))+
  theme(legend.position = "top")+
  labs(x = "serial interval (days)", y = "probability",
       fill = "", col = "")



# SAVE --------------------------------------------------------------------
ggsave(p_pairwise_hist, file = paste0(save_path, "p_pairwise_hist.svg"))
ggsave(p_pairwise_hist_overall, file = paste0(save_path, "p_pairwise_hist_overall.svg"))

