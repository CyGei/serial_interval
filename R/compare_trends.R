mean_si_LHS <- all_si %>% 
  group_by(across(all_of(c("LHS", "nVar")))) %>% 
  summarise( temp = mean(serial_interval)) %>% 
  pivot_wider(names_from = nVar,
              values_from = temp) %>% 
  ungroup() %>% 
  select(-LHS) %>% 
  as_tibble()




comp_mean_si_LHS <- cbind(mean_si_LHS,
            do.call(cbind, lapply(asplit(combn(names(mean_si_LHS), 2), 2), function(x){
              setNames(data.frame(mean_si_LHS[x[1]] - mean_si_LHS[x[2]]), paste(x, collapse = " - "))
            }))) %>% 
  as_tibble() %>% 
  select(-all_of(names(.CoV_pal)))



# RIDGES v1 ---------------------------------------------------------------

points <- comp_mean_si_LHS %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(diff = case_when(value < 0 ~ "negative",
                          value == 0 ~ "equal",
                          value > 0 ~ "positive"))
freqs <- points %>% 
  group_by(name, diff) %>%
  summarise(n = n(),
            mean_val = mean(value)) %>% 
  ungroup() %>% 
  mutate(position = case_when(n <100 & diff == "greater" ~ mean_val + 0.05,
                              n <100 & diff == "smaller" ~ mean_val - 0.02,
                              TRUE ~ mean_val) )

library(ggridges)
ggplot()+
  geom_vline(xintercept = 0, lty = "dashed")+
  geom_density_ridges_gradient(data = points, 
                               aes(y = name, x = value, fill = stat(x)),
                               rel_min_height = 0.001) +
  geom_text(data = freqs,
            aes(y = name, x = position, label = paste0(n, "%")),
            vjust = -0.05, size = 4, col = "black", show.legend = FALSE) +
  scale_fill_gradient2(low = "purple", mid = "white",  high ="#ffd700" ) +
  theme_fira()+
  theme(axis.text.y = )+
  labs(x = "Mean serial interval difference (days)", y = "", fill = "Difference:")




# RDIGES v2 ---------------------------------------------------------------
densities <- comp_mean_si_LHS %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>%
  summarise(values = list(value)) %>% 
  mutate(density = map(values, ~density(.x)),
         density.x = map(density, ~.x$x),
         density.y = map(density, ~.x$y)) %>% 
  select(-c(density, values)) %>% 
  unnest(cols = c(starts_with("density"))) %>% 
  separate(col = name,
           sep = " - ", 
           c("X", "Y")) %>% 
  mutate(X = factor(X, levels = c("Wild Type", "Alpha", "Delta", "Omicron BA1", "Omicron BA2", "Omicron BA5")),
         Y = factor(Y, levels = levels(X)))

.dist_pal <- c(      
              "Alpha" = "#CD9600",            
              "Delta" = "#0096ff",   #"#e2bd36",  #005B96", #7CAE00            
              "Omicron BA1" = "#FF79C3",
              "Omicron BA2" = "#85036F",
              "Omicron BA5" = "red")
v2 <- densities %>% 
  group_by(X,Y) %>%
  mutate(myalpha = abs(scale(density.x, center = 0)),
         myalpha = scales::rescale_mid(myalpha, mid = 0),
         myalpha = 2 * (as.vector(myalpha) -0.5),
         myalpha = myalpha^2) %>% 
  
  ggplot()+
  aes(x = density.x, y = density.y, group = Y, fill = Y)+
  geom_vline(xintercept = 0, lty = "dashed") +
  geom_segment(aes(alpha = myalpha, xend = density.x, yend = 0, color = Y), size = 1) +
  geom_rect(aes( xmin=0, xmax = 0, ymin =0, ymax = 0))+
  
  facet_wrap(~factor(X, levels = rev(levels(X))), 
             ncol = 1, strip.position = "left")+
  
  scale_alpha_identity() +
  scale_fill_manual(values = .dist_pal)+
  scale_color_manual(values = .dist_pal)+
  
  theme_fira()+
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0,
                                         colour = "black"),
        legend.position = "top",
        axis.text.y = element_blank(),
        plot.tag.position = "top")+
  labs(x = "Mean serial interval difference (days)", y = "", fill = "", color = "")



# RIDGES v3 ---------------------------------------------------------------

p_trend_ridges <- densities %>% 
  ggplot()+
  aes(y = X, x = density.x, fill = Y, height = density.y) +
  geom_vline(xintercept = 0, lty = "dashed")+
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.65)+
  scale_fill_manual(values = .dist_pal)+
  scale_color_manual(values = .dist_pal)+
  scale_x_continuous(breaks = seq(-2,2, 0.5))+
  theme_fira()+
  theme(axis.text.y = element_text(colour = .CoV_pal),
        legend.position = "right")+
  labs(x = "Mean serial interval difference (days)", y = "", fill = "minus")

# p_trend_ridges <- v3 +
#   annotate("text", x = -0.75, y = 6.2, label = "Shorter than", size = 5)+
#   annotate("text", x = 0.75, y = 6.2, label = "Longer than", size = 5)+
#   coord_cartesian(clip = "off")

# HEATMAP -----------------------------------------------------------------

pairs <- expand.grid(names(mean_si_LHS), names(mean_si_LHS)) %>%
  filter(Var1 != Var2)


pairs_df <- map2(pairs$Var1, pairs$Var2,
                 function(x,y) as_tibble_col(mean_si_LHS[[x]] - mean_si_LHS[[y]],str_c(x, "-", y))) %>%
  bind_cols()

freqs <- pairs_df %>%
  pivot_longer(cols = everything()) %>%
  mutate(diff = case_when(value < 0 ~ "negative",
                          value == 0 ~ "equal",
                          value > 0 ~ "positive")) %>%
  group_by(name, diff) %>%
  summarise(n = n(),
            mean_val = mean(value)) %>%
  group_by(name) %>% 
  separate(col = name,
           sep = "-",
           c("X", "Y"),
           remove = FALSE)
# 
# f <- freqs[!duplicated(t(apply(freqs[,1:2], 1, sort))),]
# 
# f %>% 
#   ggplot(aes(x = X, y = Y, fill = mean_val, group = diff)) +
#   geom_tile() +
#   geom_text(aes(label = paste0(freq, "%")), size = 3, col = "black")+
#   scale_fill_gradient2(low = "purple", mid = "white",  high ="#ffd700" )+
#   theme_fira()+
#   labs(x = "", y = "", fill = "Mean Difference:")
# 

#freqs delete rows below 50%
p_trend_heatmap <- freqs %>% 
  filter(n >50) %>% 
  ggplot(aes(x = X, y = Y, fill = mean_val, group = diff )) +
  geom_tile() +
  geom_text(aes(label = paste0(n, "%")), size = 3, col = "black")+
  scale_fill_gradient2(low = "purple", mid = "white",  high ="#ffd700" )+
  theme_fira()+
  #facet_wrap(~factor(diff, levels = c("smaller than", "greater than")))+
  labs(x = "x", y = "y", fill = "Mean\ndifference:")





# SAVE --------------------------------------------------------------------

ggsave(p_trend_heatmap, file = paste0(save_path, "p_trend_heatmap.svg"))
ggsave(p_trend_ridges, file = paste0(save_path, "p_trend_ridges.svg"))

