
LHS_dens <- tibble(gt_dens = gt_LHS,
                   incub_dens = incub_LHS) %>% 
  mutate(LHS = row_number()) %>% 
  unnest(cols = ends_with("dens")) %>% 
  group_by(LHS) %>% 
  group_modify(~ add_row(.x, incub_dens = 0, gt_dens = 0, .before = 0 )) %>% 
  mutate(x = row_number()-1) %>% 
  pivot_longer(cols = -c(LHS, x), names_to = "type", values_to = "dens")
LHS_dens


p_LHS_dist <- LHS_dens %>% 
  group_by(x, type) %>% 
  summarise(median = median(dens),
            lb = quantile(dens, probs = 0.025),
            ub = quantile(dens, probs = 0.975)) %>% 
  mutate(type = case_when(type == "gt_dens" ~ "Generation Time",
                          type == "incub_dens" ~ "Incubation Period")) %>% 
  ggplot()+
  
  #median density
  geom_line(aes(x = x, y = median), lty = "dotted")+
  geom_point(aes(x = x, y = median), size = 2.5, col = "orange")+
  
  #95% CrI
  geom_errorbar(aes(x = x, ymin = lb , ymax = ub), 
                col = "#404040", lty = "solid", 
                size = 0.4, width = 0.3) + 
  
  #wrap dist type
  facet_wrap(~type)+ 
  
  scale_x_continuous(breaks = seq(0,30, 5), limits = c(0,30))+
  labs(x = "time (days)", y = "probability") +
  theme_fira()



# SAVE --------------------------------------------------------------------
ggsave(p_LHS_dist, file = paste0(save_path, "p_LHS_dist.svg"))

rm(p_LHS_gt, p_LHS_incub, d_gt, d_incub)




