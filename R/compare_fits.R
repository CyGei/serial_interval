
# COMPUTE FITTED DISTRIBUTIONS with 95% CrI -------------------------------

#mean parameters for each distribution type by variant:
mean_params <- fitted_steps %>% 
  group_by(LHS, step, nVar, distri) %>% 
  summarise(across(c(shift, shape, rate, mu, sd, log_mu, log_sd),
                   list(mean = mean),
                   na.rm = TRUE)) 

all_fits <- fitted_steps %>% 
  rowwise() %>% 
  # mutate(dist = case_when(distri == "gamma" ~ list(dgamma(lower_si:upper_si + shift, shape = shape, rate = rate)),
  #                        distri == "norm" ~ list(dnorm(lower_si:upper_si + shift, mean = mu, sd = sd)),
  #                        distri == "lnorm" ~ list(dlnorm(lower_si:upper_si +shift, meanlog = log_mu, sdlog = log_sd)))) %>% 

  mutate(fit = case_when(distri == "gamma" ~ list(distcrete("gamma", interval = 1, w = 0.5,shape = shape, rate = rate)),
                         distri == "norm" ~ list(distcrete("norm", interval = 1, w = 0.5,  mean = mu, sd = sd)),
                         distri == "lnorm" ~ list(distcrete("lnorm", interval = 1, w = 0.5,meanlog = log_mu , sdlog = log_sd))),
  dist = list(fit$d(lower_si:upper_si + shift))) %>% 
  select(LHS, step, nVar, distri, dist) %>% 
  unnest(dist) %>% 
  group_by(LHS, step, nVar, distri) %>% 
  mutate(x = lower_si:upper_si) %>% 
  group_by(nVar, distri, x) %>% 
  summarise(est = mean(dist),
            lwr = quantile(dist, prob = 0.025),
            upr = quantile(dist, prob = 0.975))


p_all_fits <- all_fits %>% 
  ggplot() +
  geom_line(aes(x = x , y = est, 
                color = nVar, 
                group = nVar)) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr, fill = nVar),
              alpha = 0.2) +
  facet_wrap(~distri)+
  theme_fira()+
  scale_x_continuous(breaks = seq(lower_si, upper_si, 2))+
  scale_color_manual(values = .CoV_pal)+
  scale_fill_manual(values = .CoV_pal)+
  labs(fill = "", color = "",
       x = "serial interval (days)", y = "density")+
  theme_fira()+
  theme(legend.position = "top")




#get the empirical posterior distribution from the data:
dens_all_si <- nested_si %>% 
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

all_fits2 <- all_fits %>% 
  mutate(distri = factor(distri, levels = c("gamma", "lnorm", "norm")))
# PLOT --------------------------------------------------------------------


#plot the fitted distributions against the real data
p_compare_fits <-
  ggplot() +
  geom_col(
    data = dens_all_si,
    aes(x = density.x, y = est),
    col = "black",
    fill = "grey",
    alpha = 0.6,
    position = position_dodge(0.9)
    )+
  geom_errorbar( data = dens_all_si,
                 aes(x = density.x, ymin = lwr, ymax = upr),
                 position = position_dodge(0.9),
                 width = 0.6)+
  geom_point(data = all_fits2,
            aes(x = x , y = est,
                shape = distri,
                color = distri,
                size = distri),
            alpha = 1) +
  geom_ribbon(data = all_fits2,
              aes(x = x,
                  ymin = lwr, ymax = upr,
                  fill = distri,
                  alpha = distri)) +
  facet_wrap(~nVar)+
  theme_fira()+
  scale_x_continuous(breaks = seq(lower_si, upper_si, 2))+
  scale_color_manual(values = c("#981FAC", "#BC6C25", "forestgreen"))+
  scale_fill_manual(values = c("#981FAC", "#BC6C25", "forestgreen"))+
  scale_alpha_manual(values = c(0.3, 0.1, 0.1))+
  scale_size_manual(values = c(1.5, 0.8, 0.6))+
  guides(size = FALSE)+
  labs(fill = "Distribution:",
       alpha = "Distribution:",
       shape = "Distribution:",
       colour = "Distribution:",
       x = "serial interval (days)", y = "probability")+
  theme_fira()+
  theme(legend.position = "top")



# SAVE --------------------------------------------------------------------
ggsave(p_compare_fits, file = paste0(save_path, "p_compare_fits.svg"))

saveRDS(all_fits, 
        file = paste0(save_path, "all_fits.RData") )


