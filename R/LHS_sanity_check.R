# this script checks the outbreaker
# results for the median natural history


# FIND THE CLOSEST DISTRIBUTION TO THE MEDIAN -----------------------------
reference <- tibble(gt_dens = gt_LHS,
                    incub_dens = incub_LHS) %>% 
  mutate(LHS = row_number()) %>% 
  unnest(cols = ends_with("dens")) %>% 
  group_by(LHS) %>% 
  group_modify(~ add_row(.x, incub_dens = 0, gt_dens = 0, .before = 0 )) %>% 
  mutate(x = row_number()-1) %>% 
  group_by(x) %>% 
  summarise(median_gt = median(gt_dens),
            median_incub = median(incub_dens)) #get the median values

reference <- 100 %>% 
  rerun(reference) %>% 
  bind_rows() #.id = "LHS"

all_dist <- tibble(gt_dens = gt_LHS,
                  incub_dens = incub_LHS) %>% 
  mutate(LHS = row_number()) %>% 
  unnest(cols = ends_with("dens")) %>% 
  group_by(LHS) %>% 
  group_modify(~ add_row(.x, incub_dens = 0, gt_dens = 0, .before = 0 )) %>% 
  mutate(x = row_number()-1)
all_dist

closest_LHS <- cbind(all_dist, reference) %>% 
  janitor::clean_names() %>% 
  select(-x_4) %>% 
  rowwise() %>% 
  mutate(gt_diff = abs(gt_dens - median_gt),
         incub_diff = abs(incub_dens - median_incub) ) %>% 
  group_by(lhs) %>% 
  summarise(mean_gt_diff = mean(gt_diff),
            mean_incub_diff = mean(incub_diff)) %>% 
  mutate(total_diff = mean_gt_diff + mean_incub_diff) %>% 
  slice(which.min(total_diff))
closest_LHS




# PLOT --------------------------------------------------------------------


LHS_dens <- all_dist %>% 
  pivot_longer(cols = -c(LHS, x), names_to = "type", values_to = "dens") %>% 
  mutate(type = case_when(type == "gt_dens" ~ "Generation Time",
                          type == "incub_dens" ~ "Incubation Period"))
LHS_dens

LHS_99 <- LHS_dens %>% filter(LHS == closest_LHS$lhs)


p_LHS_dist <- LHS_dens %>% 
  group_by(x, type) %>% 
  summarise(mean = mean(dens),
            lb = quantile(dens, probs = 0.025),
            ub = quantile(dens, probs = 0.975)) %>% 
  ggplot()+
  
  #mean density
  geom_line(aes(x = x, y = mean), lty = "dotted")+
  geom_point(aes(x = x, y = mean), size = 2.5, col = "orange")+
  
  #95% CrI
  geom_errorbar(aes(x = x, ymin = lb , ymax = ub), 
                col = "#404040", lty = "solid", 
                size = 0.4, width = 0.3) + 
  
  #Closest LHS to mean:
  #LHS40
  geom_point(data = LHS_99,
             aes(x = x, y = dens), size = 1, col = "steelblue", shape = 15) +
  
  #wrap dist type
  facet_wrap(~type)+ 
  
  scale_x_continuous(breaks = seq(0,20, 2), limits = c(0,20))+
  labs(x = "time (days)", y = "probability") +
  theme_fira()

p_LHS_dist

# SAVE --------------------------------------------------------------------

ggsave(p_LHS_dist, file = paste0(save_path, "p_LHS_dist.svg"))





# POSTERIOR GAMMA DENSITIES OF THE MEAN & SD SI ---------------------------------
fitted_steps_gamma_LHS_99 <- fitted_steps %>% 
  filter(distri == "gamma") %>% 
  filter(LHS == closest_LHS$lhs)


#CrI for the Gamma parameters:
ci_gamma <- c("sd", "mu", "shift", "mu_minus_shift", "shape", "rate") %>%
  purrr::set_names() %>%
  map(function(x) {
    get_ci(
      data = fitted_steps_gamma_LHS_99,
      agg = c("step"),
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
      data = fitted_steps_gamma_LHS_99,
      agg = c("step"),
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
      data = fitted_steps_gamma_LHS_99,
      density_data = densities_gamma[[x]],
      ci_data = ci_gamma[[x]],
      agg = c("step"),
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



p_posterior_gammas
tab_posterior_gammas_simple



# SAVE --------------------------------------------------------------------

ggsave(p_posterior_gammas, file = paste0(save_path, "p_posterior_gammas_mu_sd_LHS_50.svg"))

tab_posterior_gammas_simple %>% 
  flextable::save_as_docx(path = paste0(save_path, "tab_posterior_gammas_simple_LHS_50.docx"))

