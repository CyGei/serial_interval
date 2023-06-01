### APPENDIX C

# RUN MISC.R to load all necessary data + master.R
all_si

lhs <- params_df %>% 
  as_tibble() %>% 
  mutate(LHS = row_number()) %>% 
  pivot_longer(cols = -LHS) %>% 
  separate(name, sep = "_", into = c("dist", "param")) %>% 
  pivot_wider(names_from = param, values_from = value) 

mu_cv <- epitrix::gamma_shapescale2mucv(shape = lhs$shape, scale = lhs$scale) %>% 
  as_tibble()

lhs <- as_tibble(cbind(lhs, mu_cv))



# MEAN --------------------------------------------------------------------
mean_si <- all_si %>% 
  group_by(LHS) %>% 
  summarise(mean_si = mean(serial_interval, na.rm = TRUE))

lhs_si <- left_join(lhs, mean_si, by = "LHS") %>% 
  select(LHS, dist, mu, cv, mean_si) %>% 
  pivot_longer(cols = c(mu, cv), names_to = "params")

lhs_si %>% 
  ggplot()+
  aes(x = value, y = mean_si, col = dist)+
  geom_point()+
  facet_grid(dist~params, scales = "free_x")+
  theme_bw()
  

fit_data <- lhs_si %>%
  group_by(dist, params) %>% 
  nest() %>% 
  mutate(fit = map(data, ~lm(mean_si ~ value, data = .x)),
         tidied = map(fit, broom::tidy)) %>% 
  unnest(tidied) %>% 
  select(-c(data, fit)) %>% 
  filter(term  == "value") %>% 
  mutate(p_val = ifelse(p.value < 0.05, "<0.05", ">0.05"))


lhs_si %>%
  ggplot() +
  aes(x = value, y = mean_si, col = dist) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text(data = fit_data, aes(
    label = paste0("\u03b2 = " , round(estimate, digits = 2),
                   "\n", "\u03b1", p_val),
    x = c(7.5, 1.5, 7.5, 1.5),
    y = 3
  )) +
  facet_grid(dist ~ params, scales = "free", 
             labeller = labeller(dist = c(gt = "Generation Time", incub = "Incubation Period") )) +
  theme_bw()+
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  labs(y = "mean serial interval")





# SD --------------------------------------------------------------------
sd_si <- all_si %>% 
  group_by(LHS) %>% 
  summarise(sd_si = sd(serial_interval, na.rm = TRUE))

lhs_si <- left_join(lhs, sd_si, by = "LHS") %>% 
  select(LHS, dist, mu, cv, sd_si) %>% 
  pivot_longer(cols = c(mu, cv), names_to = "params")

lhs_si %>% 
  ggplot()+
  aes(x = value, y = sd_si, col = dist)+
  geom_point()+
  facet_grid(dist~params, scales = "free_x")+
  theme_bw()


fit_data <- lhs_si %>%
  group_by(dist, params) %>% 
  nest() %>% 
  mutate(fit = map(data, ~lm(sd_si ~ value, data = .x)),
         tidied = map(fit, broom::tidy)) %>% 
  unnest(tidied) %>% 
  select(-c(data, fit)) %>% 
  filter(term  == "value") %>% 
  mutate(p_val = ifelse(p.value < 0.05, "<0.05", ">0.05"))


lhs_si %>%
  ggplot() +
  aes(x = value, y = sd_si, col = dist) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text(data = fit_data, aes(
    label = paste0("\u03b2 = " , round(estimate, digits = 2),
                   "\n", "\u03b1", p_val),
    x = c(7.5, 1.5, 7.5, 1.5),
    y = 4
  )) +
  facet_grid(dist ~ params, scales = "free", 
             labeller = labeller(dist = c(gt = "Generation Time", incub = "Incubation Period") )) +
  theme_bw()+
  theme(legend.position = "none",
        strip.text = element_text(size = 12))+
  labs(y = "standard deviation serial interval")





# MODEL -------------------------------------------------------------------
#mean # run previous mean code

model_data <- lhs_si %>%
  pivot_wider(names_from = params, values_from = value) %>% 
  pivot_wider(names_from = dist, values_from = c(mu, cv) )

m <- lm(mean_si ~ mu_incub  + mu_gt + cv_incub + cv_gt , data = model_data)
summary(m)
plot(m)
sjPlot::tab_model(m)




######## USING SD instead of CV
lhs <- params_df %>% 
  as_tibble() %>% 
  mutate(LHS = row_number()) %>% 
  pivot_longer(cols = -LHS) %>% 
  separate(name, sep = "_", into = c("dist", "param")) %>% 
  pivot_wider(names_from = param, values_from = value) 

mu_cv <- epitrix::gamma_shapescale2mucv(shape = lhs$shape, scale = lhs$scale) %>% 
  as_tibble()

lhs <- as_tibble(cbind(lhs, mu_cv)) %>% 
  rowwise() %>% 
  mutate(sd =sqrt(shape)*scale,
         mucheck = shape*scale)


mean_si <- all_si %>% 
  group_by(LHS) %>% 
  summarise(mean_si = mean(serial_interval, na.rm = TRUE))

lhs_si <- left_join(lhs, mean_si, by = "LHS") %>% 
  select(LHS, dist, mu, sd, mean_si) %>% 
  pivot_longer(cols = c(mu, sd), names_to = "params")

model_data <- lhs_si %>%
  pivot_wider(names_from = params, values_from = value) %>% 
  pivot_wider(names_from = dist, values_from = c(mu, sd) )

m <- lm(mean_si ~ mu_incub  + mu_gt + sd_incub + sd_gt , data = model_data)
summary(m)
plot(m)
sjPlot::tab_model(m)
