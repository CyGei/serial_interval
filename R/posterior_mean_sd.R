

# POSTERIOR DENSITIES OF THE MEAN & SD SI ---------------------------------
#computing the CrI:
ci <- c("mean","sd", "cv") %>%
  purrr::set_names() %>%
  map(function(x) {
    get_ci(
      data = all_si,
      agg = c("step", "LHS"),
      group = "nVar",
      variable = serial_interval ,
      FUN = x ,
      na.rm = TRUE
    )
  })

#computing the densities:
densities <- c("mean", "sd") %>%
  purrr::set_names() %>%
  map(function(x) {
    get_densities(
      data = all_si,
      agg = c("step", "LHS"),
      group = "nVar",
      variable = serial_interval ,
      FUN = x ,
      na.rm = TRUE
    )
  })





# PLOTS -------------------------------------------------------------------
#Posterior distribution of the SI mean & sd by variant

p <- c("mean", "sd") %>%
  purrr::set_names() %>%
  map(function(x) {
    plot_densities(
      data = all_si,
      density_data = densities[[x]],
      ci_data = ci[[x]],
      agg = c("step", "LHS"),
      group = "nVar",
      variable = serial_interval,
      FUN = x
    ) +
      labs(y = "density",
           x = "serial interval (days)") +
      theme(legend.position = "bottom")+
      scale_x_continuous(breaks = seq(0,5, 0.25))
    })

 p_posterior_mu_sd <- ggpubr::ggarrange(
  plotlist = list(mean = (p$mean + labs(x = "")),sd = p$sd),
  common.legend = TRUE,
  vjust = -0.5,
  labels = c("mean", "sd"),
  font.label = list(size = 10, face ="bold.italic"),
  align = "hv",
  nrow = 2
)





# TABLE -------------------------------------------------------------------
tab_posterior_mu_sd <- 
  map(names(ci), ~ci_tab(ci[[.x]], !!sym(.x))) %>% 
  bind_cols() %>% 
  select(nVar...1, mean, sd,cv) %>% 
  rename(variant = nVar...1) %>% 
  flextable::flextable(cwidth = 1.5) %>% 
  flextable::align(align = "justify", part = "body") %>% 
  flextable::align(align = "center", part = "header") %>% 
  flextable::bold(part = "header") %>% 
  flextable::bold(j = "variant")



# SAVE --------------------------------------------------------------------
ggsave(p_posterior_mu_sd, file = paste0(save_path, "p_posterior_mu_sd.svg"))

tab_posterior_mu_sd %>% 
  flextable::save_as_docx(path = paste0(save_path, "tab_posterior_mu_sd.docx"))
