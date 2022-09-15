
# WITH CRI ----------------------------------------------------------------
  

ecdf_data <- nested_si %>% 
  mutate(ecdf = map(.x = serial_interval, ~ecdf(.x)(lower_si:upper_si))) %>% 
  select(-serial_interval) %>% 
  unnest(cols = ecdf) %>% 
  group_by(LHS, step, nVar) %>% 
  mutate(x = lower_si:upper_si)



ecdf_cri <- ecdf_data %>% 
  group_by(nVar, x) %>% 
  summarise(lwr = quantile(ecdf, probs = 0.025),
            upr = quantile(ecdf, probs = 0.975),
            mean = mean(ecdf))


p_cdf <- ggplot(data = ecdf_cri) +
  aes(x = x, fill = nVar, group = nVar) +
  
  geom_line(aes(y = mean, col = nVar)) +
  geom_point(aes(y = mean, col = nVar)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  
  geom_vline(aes(xintercept = 0), lty = "dashed") +
  
  scale_color_manual(values = .CoV_pal) +
  scale_fill_manual(values = .CoV_pal) +
  scale_x_continuous(breaks = seq(lower_si, upper_si, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  
  theme_fira() +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  labs(x = "serial interval (days)",
       y = "cumulative probability")


p_cdf #+ geom_hline(yintercept = 0.5)

# SAVE --------------------------------------------------------------------
#% of negative values
tab_cdf <- ecdf_cri %>% filter(x == -1)

tab_cdf %>% 
  select(-x) %>% 
  pivot_longer(cols = -nVar) %>% 
  group_by(name) %>% 
  summarise(mean = round(mean(value), 2))


ggsave(p_cdf, file = paste0(save_path, "p_cdf.svg"))

rm(ecdf_cri)
rm(ecdf_data)
#rm(nested_si)

# NO CRI ------------------------------------------------------------------
# 
# 
# # ECDF --------------------------------------------------------------------
# cdf <- tapply(all_si$serial_interval, all_si$nVar, ecdf) %>% 
#   lapply(., function(x) x(-10:20)) %>% enframe(name = "nVar")
# 
# 
# 
# # TABLE -------------------------------------------------------------------
# tab_cdf <- cdf %>% 
#   unnest(., cols = value) %>%
#   group_by(nVar) %>% 
#   mutate(serial_interval = -10:20,
#          value = as.numeric(format(round(value, digits = 2), nsmall = 2))) %>% 
#   pivot_wider(names_from = nVar) %>% 
#   ungroup() %>% 
#   mutate(mean = rowMeans(across(.cols = -serial_interval)))

# PLOT -------------------------------------------------------------------
# p_cdf <- cdf %>% 
#   unnest(., cols = value) %>%
#   group_by(nVar) %>% 
#   mutate(x = -10:20) %>% 
#   ggplot()+
#   aes(x = x,
#       y = value,
#       col = nVar)+
#   geom_point(size = 2)+
#   geom_line(lty = "solid", size = 0.3)+
#   geom_vline(aes(xintercept = 0), lty = "dashed")+
#   scale_color_manual(values = .CoV_pal)+
#   scale_x_continuous(breaks = seq(-10,20, 2))+
#   scale_y_continuous(breaks = seq(0,1, 0.2))+
#   theme_fira()+
#   theme(legend.title = element_blank(),
#         legend.position = "top")+
#   labs(x = "serial interval (days)",
#        y = "cumulative density")
# 
# 
# 
# 
# # KS TEST -----------------------------------------------------------------
# # Computing the KS test for all possible combinations of variants
# cmb <- combn(unique(all_si$nVar), 2) %>% as.data.frame()
# ks <- future_lapply(as.list(cmb), function(x){
#   a <- all_si %>% filter(nVar == x[[1]]) %>% .$serial_interval
#   b <- all_si %>% filter(nVar == x[[2]]) %>% .$serial_interval
#   ks <- ks.test(a,b, alternative = "two.sided") # simulate.p.value = TRUE
#   return(ks$p.value)
# })
# unlist(ks)




