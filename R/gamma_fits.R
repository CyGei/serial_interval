############################################
# Analyse the mean Gamma Params
############################################
# EXTRACT FITTED GAMMA PARAMS ---------------------------------------------
#Posterior distribution of the Gamma parameters.
fitted_steps_gamma <- fitted_steps %>% 
  filter(distri == "gamma") 
  

#CrI for the Gamma parameters:
ci_gamma <- c("sd", "mu", "shift", "mu_minus_shift", "shape", "rate") %>%
  purrr::set_names() %>%
  map(function(x) {
    get_ci(
      data = fitted_steps_gamma,
      agg = c("step", "LHS"),
      group = "nVar",
      variable = !!sym(x) ,
      FUN = mean ,
      na.rm = TRUE
    )
  })

#densities of the Gamma parameters:
densities_gamma <-  c("sd", "mu", "shift", "mu_minus_shift")  %>% 
  purrr::set_names() %>% 
  map(function(x){
    get_densities(
      data = fitted_steps_gamma,
      agg = c("step", "LHS"),
      group = "nVar",
      variable = !!sym(x) ,
      FUN = mean ,
      na.rm = TRUE
    ) 
  })



# PLOTS -------------------------------------------------------------------
#Plots of the posterior distribution of the Gamma parameters:
gamma_plots <- c("mu_minus_shift", "sd") %>% 
  #purrr::set_names() %>%
  map(function(x) {
    plot_densities(
      data = fitted_steps_gamma,
      density_data = densities_gamma[[x]],
      ci_data = ci_gamma[[x]],
      agg = c("step", "LHS"),
      group = "nVar",
      variable = !!sym(x),
      FUN = mean
    )+ theme(axis.title = element_text(size = 10))+
      labs( x = "serial interval (days)", y = "density")+
      scale_x_continuous(breaks = seq(0,10, 0.25))
  })


p_posterior_gammas <- ggpubr::ggarrange(
  plotlist = list(mean = (gamma_plots[[1]] + labs(x = "")),sd = gamma_plots[[2]]),
  common.legend = TRUE,
  vjust = -0.5,
  legend = "top",
  labels = c("mean", "sd"),#"mean", "sd", "shift", "adjusted mean"
  font.label = list(size = 10, face ="bold.italic"),
  align = "hv",
  nrow = 2
)



# TABLE -------------------------------------------------------------------
tab_posterior_gammas<- names(ci_gamma) %>%
  purrr::set_names() %>%
  map(., function(x) {
    ci_tab(ci_gamma[[x]], metric = !!sym(x))
  }) %>%
  reduce(left_join, by = "nVar") %>%
  select(nVar, mu_minus_shift, sd, mu, shift, shape, rate) %>%
  rename(variant = nVar) %>%
  flextable::flextable(cwidth = 1.5) %>%
  flextable::align(align = "justify", part = "body") %>%
  flextable::align(align = "center", part = "header") %>%
  flextable::bold(part = "header") %>%
  flextable::bold(j = "variant") %>%
  flextable::set_header_labels(values =
                                 list(mu = "mean",
                                      sd = "standard deviation",
                                      mu_minus_shift = "adjusted mean"))


tab_posterior_gammas_simple<- names(ci_gamma) %>%
  purrr::set_names() %>%
  map(., function(x) {
    ci_tab(ci_gamma[[x]], metric = !!sym(x))
  }) %>%
  reduce(left_join, by = "nVar") %>%
  select(nVar, mu_minus_shift, sd) %>%
  rename(variant = nVar) %>%
  flextable::flextable(cwidth = 1.5) %>%
  flextable::align(align = "justify", part = "body") %>%
  flextable::align(align = "justify", part = "header") %>%
  flextable::bold(part = "header") %>%
  flextable::bold(j = "variant") %>%
  flextable::set_header_labels(values =
                                 list(
                                      sd = "standard deviation",
                                      mu_minus_shift = "mean"))


############################################
# Draw the Gamma Distributions
############################################


# GAMMA FITS --------------------------------------------------------------

gamma_fits <- fitted_steps_gamma %>% 
  rowwise() %>% 
  mutate(dist = list(
    distcrete("gamma", interval = 1, w = 0.5,
              shape = shape, rate = rate)$d( (lower_si:upper_si) + shift ))) %>% 
  unnest(dist) %>% 
  select(LHS, step, nVar, dist) %>% 
  group_by(LHS, step, nVar) %>% 
  mutate(x = lower_si:upper_si) %>% 
  group_by(nVar, x) %>% 
  summarise(est = mean(dist),
            lwr = quantile(dist, prob = 0.025),
            upr = quantile(dist, prob = 0.975))



# PLOT --------------------------------------------------------------------
p_gamma_fits <-
  gamma_fits %>% 
  ggplot() +
  geom_line(aes(x = x , y = est, 
                color = nVar, 
                group = nVar)) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr, fill = nVar),
              alpha = 0.2) +
  theme_fira()+
  scale_x_continuous(breaks = seq(lower_si, upper_si, 2))+
  scale_color_manual(values = .CoV_pal)+
  scale_fill_manual(values = .CoV_pal)+
  labs(fill = "", color = "",
       x = "serial interval (days)", y = "density")+
  theme_fira()+
  theme(legend.position = "top")




# SAVE --------------------------------------------------------------------
tab_posterior_gammas %>% flextable::save_as_docx(path = paste0(save_path, "table_gamma_means_sd.docx"))
tab_posterior_gammas_simple %>% flextable::save_as_docx(path = paste0(save_path, "table_gamma_means_sd_simple.docx"))

ggsave(p_posterior_gammas, file = paste0(save_path, "p_posterior_gammas.svg"))
ggsave(p_gamma_fits, file = paste0(save_path, "p_gamma_fits.svg"))

rm(gamma_fits, gamma_plots, ci_gamma, densities_gamma, fitted_steps_gamma)

