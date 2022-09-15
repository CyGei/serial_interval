

# DATA TO FIT -------------------------------------------------------------
# Fitting at every step for every variant for every LHS
tofit <- nested_si %>% 
  mutate(gamma = NA,
         lnorm = NA,
         norm = NA) %>% 
  pivot_longer(cols = c(gamma, lnorm, norm), names_to = "distri") %>% 
  select(-value) 


# FITTING DISTRIBUTIONS ---------------------------------------------------
time.fitted_steps <- system.time({
  
  fitted_steps <- tofit %>%
    mutate(params = furrr::future_map2(.x = serial_interval,.y=distri ,
                                       function(x,y) {
                                         fit_disc(
                                           x,
                                           outlier = 0,
                                           params = base::c(3.3, 4.9, 11),
                                           name = y
                                         )
                                       })) %>% 
    select(-serial_interval)
  
})
  
 
  

# PARAMETER ESTIMATES -----------------------------------------------------
fitted_steps <- fitted_steps %>% unnest_wider(params) %>% 
  mutate(mu_minus_shift = mu - shift)




# SAVE --------------------------------------------------------------------

saveRDS(fitted_steps, file = paste0(save_path, "fitted_steps.RData") )
