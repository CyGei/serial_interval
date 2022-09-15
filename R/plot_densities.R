
.CoV_pal <- c("Wild Type" = "#30602d", #"#808080", #F8766D       
              "Alpha" = "#CD9600",            
              "Delta" = "#0096ff",   #"#e2bd36",  #005B96", #7CAE00            
              "Omicron BA1" = "#FF79C3",
              "Omicron BA2" = "#85036F",
              "Omicron BA5" = "red")
# "Omicron BA1" = "#FF79C3",
# "Omicron BA2" = "#CA58AE",
# "Omicron BA5" = "#85036F"

plot_densities <- function(data, density_data, ci_data ,agg, group, variable, FUN){
  
  FUN <- match.fun(FUN)
  
  #bottom jitter points
  jitter_points <- data %>% 
    group_by(across(all_of(c(agg, group)))) %>% 
    summarise( temp = FUN({{variable}})) %>%
    group_by(across(all_of(group))) %>% 
    slice_sample(n = 100)
  
  #y position for the bars & points
  y_dodge <- quantile(density_data$density.y, probs = 0.5 ) + seq(0,0.5, 0.1) #seq(0,0.25, 0.05) #seq(0,0.5, 0.1) #c(0.0, 0.1, -0.1, 0.2, -0.2, 0.15) ##rnorm(n = 1:6, mean = quantile(density_data$density.y, probs = 0.7 ), sd = dodge)
  
  #xlim
  xlim <- c(min(density_data$density.x)+0.01,
    max(density_data$density.x) - 0.001)
  
  p <- ggplot()+
    
    #density lines
    geom_line(data = density_data,
              aes(x = density.x,
                  y = density.y,
                  col = get(group)))+
    
    #density area
    geom_area(data = density_data,
              aes(x = density.x,
                  y = density.y,
                  fill = get(group),
                  ),
              alpha = 0.4)+
    
    #error bar midpoint
    geom_point(data = ci_data,
               aes(x = est,
                   y =  y_dodge, #as.numeric(quantile(density_data$density.y, probs = 0.6))
                   col = get(group)),
               size = 2) +
    
    #error bar
    geom_errorbarh(data = ci_data,
                   aes(xmin = lwr_ci,
                       xmax = upr_ci,
                       y = y_dodge, #as.numeric(quantile(density_data$density.y, probs = 0.6)),
                       col = get(group)),
                   height = ifelse(mean(density_data$density.y)>5, 1.5, 0.1),
                   size = 0.8) +
    
    #jitter points
    geom_jitter(data = jitter_points,
                aes(x = temp,
                    y = 0,
                    col = get(group)),
                shape=1,
                size = 0.5,
                height = 0.05, #median(density_data$density.y)
                )+
    
    scale_color_manual(values = .CoV_pal)+
    scale_fill_manual(values = .CoV_pal)+
    coord_cartesian(xlim = xlim ) +
    theme_fira()+
    theme(legend.title = element_blank())
}


# 
# plot_densities <- function(data, density_data, ci_data ,group1, group2, variable, fun){
#   
#   jitter_points <- data %>% 
#     group_by({{group1}}, {{group2}}) %>% 
#     summarise( temp = fun({{variable}}))
#   
#   p <- ggplot()+
#     geom_line(data = density_data,
#               aes(x = density.x,
#                   y = density.y,
#                   col = {{group2}}))+
#     geom_area(data = density_data,
#               aes(x = density.x,
#                   y = density.y,
#                   fill = {{group2}},
#                   col = {{group2}}),
#               alpha = 0.4)+
#     geom_point(data = ci_data,
#                aes(x = est,
#                    y =  max(density_data$density.y)/4, #as.numeric(quantile(density_data$density.y, probs = 0.6))
#                    col = {{group2}}),
#                size = 2) +
#     geom_errorbarh(data = ci_data,
#                    aes(xmin = lwr_ci,
#                        xmax = upr_ci,
#                        y = max(density_data$density.y)/4, #as.numeric(quantile(density_data$density.y, probs = 0.6)),
#                        col = {{group2}}),
#                    height = ifelse(mean(density_data$density.y)>5, 1.5, 0.1),
#                    size = 0.8) +
#     geom_jitter(data = jitter_points,
#                 aes(x = temp,
#                     y = 0,
#                     col = {{group2}}),
#                 shape=1,
#                 size = 0.5,
#                 height = median(density_data$density.y))+
#     scale_color_manual(values = .CoV_pal)+
#     scale_fill_manual(values = .CoV_pal)+
#     coord_cartesian(xlim = c(min(density_data$density.x) - 0.001,max(density_data$density.x) - 0.001))+
#     # theme_bw()+
#     theme_fira()+
#     theme(legend.title = element_blank())
#   
#   
#   return(p)
#   
# }

# 
# densities_m <-
#   get_densities(
#     all_si,
#     group1 = step,
#     group2 = nVar,
#     variable = serial_interval,
#     fun = mean
#   ) 
# 
# ci_m <-
#   get_ci(
#     all_si,
#     group1 = step,
#     group2 = nVar,
#     variable = serial_interval,
#     fun = mean
#   )
# 
# plot_densities(
#   all_si,
#   densities_m,
#   ci_m,
#   group1 = step,
#   group2 = nVar,
#   variable = serial_interval,
#   fun = mean
# )
# 
# 
# 
# densities_shape <-
#   get_densities(
#     fitted_steps,
#     group1 = step,
#     group2 = nVar,
#     variable = shape,
#     fun = mean
#   ) 
# 
# ci_shape <-
#   get_ci(
#     fitted_steps,
#     group1 = step,
#     group2 = nVar,
#     variable = shape,
#     fun = mean
#   )
# 
# plot_densities(
#   fitted_steps,
#   densities_shape,
#   ci_shape,
#   group1 = step,
#   group2 = nVar,
#   variable = shape,
#   fun = mean
# )




