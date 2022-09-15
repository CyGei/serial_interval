## COMPARING SIs during LOCKDOWNS
library(lubridate)

#lockdown dates
lockdowns <- data.frame(lockdown = c("1st", "2nd", "3rd"),
                        start_date = as.Date(c("2020-03-26", "2020-11-04","2021-01-06" )),
                        end_date = as.Date(c("2020-06-23", "2020-12-02","2021-03-29" )))


# no of observations in the original data
data %>% 
  select(start_dt, nVar) %>% 
  mutate(lockdown = case_when(between(start_dt,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "lockdown",
                              between(start_dt,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "lockdown",
                              between(start_dt,lockdowns$start_date[3], lockdowns$end_date[3]) ~ "lockdown",
                              TRUE ~ "no lockdown" )) %>% 
  group_by(nVar,lockdown) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(nVar) %>% 
  mutate(perc = n/ sum(n)*100)


# no of observations from outbreaker
all_si_lockdowns <- all_si %>% 
  select(step, onset_infector, serial_interval, nVar) %>% 
  filter(nVar %in% c("[1] Wild Type", "[2] Alpha")) %>% 
  mutate(nVar = factor(nVar, levels = c("[2] Alpha","[1] Wild Type"))) %>% 
  mutate(lockdown = case_when(between(onset_infector,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "Lockdown",
                              between(onset_infector,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "Lockdown",
                              between(onset_infector,lockdowns$start_date[3], lockdowns$end_date[3]) ~ "Lockdown",
                              TRUE ~ "No Lockdown" )) 

all_si_lockdowns %>% 
  group_by(nVar,lockdown) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(nVar) %>% 
  mutate(perc = n/ sum(n)*100)

all_si_lockdowns %>% 
  group_by(nVar, lockdown) %>% 
  summarise(mean = mean(serial_interval)) 

mean_lockdown<- c("[1] Wild Type", "[2] Alpha") %>% 
  purrr::set_names() %>% 
  map(function(x){
    get_ci(
      all_si_lockdowns %>% filter(nVar == x),
      group1 = step,
      group2 = lockdown,
      variable = serial_interval ,
      FUN = mean ,
      na.rm = TRUE
    )
  }) %>% 
  bind_rows(.id = "nVar")



# modelling serial interval
all_si_lockdowns %>% 
  group_by(nVar, lockdown) %>% 
  slice_sample(n =10000) %>% 
  ungroup() %>% 
  ggplot()+
  aes(x = lockdown, y = serial_interval, fill = lockdown)+
  geom_jitter(width = 0.25, shape = 21)+
  stat_summary(fun = median, geom = "crossbar", width = 0.3, aes(col = "median"))+
  #geom_errorbar(data = mean_lockdown, aes(y = est, ymin = lwr_ci, ymax = upr_ci, col = "mean"), width = 0.2)+
  stat_summary(fun = mean, geom = "crossbar", width = 0.3, aes(col = "mean"))+
  theme_fira()+
  scale_y_continuous(breaks = seq(-10,20, 5))+
  scale_size_manual(values = c(1,1))+
  scale_fill_manual(values = c("#ff0000", "#17eaae"), guide = "none")+
  scale_color_manual(values =c("black", "blue"))+
  theme( legend.position = "top",
        axis.title.x = element_blank())+
  facet_wrap(~nVar)+
  labs(colour = NULL, y = "serial interval (days)")



dens<- all_si_lockdowns %>%
  group_by(nVar, lockdown) %>%
  summarise(density = list(hist(
    serial_interval, breaks = seq(-10.5, 20.5), plot = FALSE
  ))) %>%
  mutate(density.x = map(density, ~ .x[["mids"]]),
         density.y = map(density, ~ .x[["density"]])) %>%
  select(-density) %>%
  unnest(cols = c(starts_with("density"))) %>%
  ungroup()

ggplot() +
  geom_col(data = dens, aes(x = density.x, y = density.y, fill = lockdown),
           alpha = 0.5, position = "identity") +
  geom_errorbarh(data = mean_lockdown, aes(y = 0.1, xmin = lwr_ci, xmax = upr_ci, col = lockdown), height = 0.01, size = 1 )+
  facet_wrap( ~ nVar) +
  theme_fira()+
  scale_x_continuous(breaks = seq(-10,20,5))+
  scale_fill_manual(values = c("#e2bd36","#3030a3") )+#c("#ff0000", "#18eaae")
  scale_color_manual(values = c("#e2bd36","#3030a3") )+#c("#ff0000", "#18eaae")
  labs(x = "serial interval (days)", y = "frequency", col = NULL, fill = NULL)
  


m <- lm(serial_interval ~ nVar*lockdown, data = all_si_lockdowns)
summary(m)
sjPlot::tab_model(m)

m_ <- lm(serial_interval ~ nVar*lockdown, data = all_si_lockdowns)

#means
t <- all_si_lockdowns %>% group_by(step, nVar, lockdown) %>% summarise(mean_si = mean(serial_interval))
t %>% 
  ggplot()+
  aes(x = nVar, y = mean_si, fill = lockdown)+
  geom_jitter(width = 0.25, shape = 21)+
  stat_summary(fun = median, geom = "crossbar", width = 0.2,col = "black", aes(size = "Median"))+ # aes(col = lockdown)
  theme_fira()+
  #theme(legend.position = "top")+
  scale_fill_manual(values = c("#ea1753", "#17eaae"))+
  scale_size_manual(values = c(0.5,0.5))+
  scale_y_continuous(breaks = seq(1.5,4.5, 0.5))+
  #scale_color_manual(values = c("darkred", "darkgreen"), guide = FALSE)+
  labs(fill = NULL, size = NULL, y = "mean serial interval (days)", x = NULL)


x <- t %>% 
  mutate(nVar = factor(nVar, levels = c("[1] Wild Type","[2] Alpha" )),
         lockdown = factor(lockdown, levels = c("No Lockdown", "Lockdown")))
mt <- lm(mean_si ~ lockdown*nVar, data = x)
m <- lm(mean_si ~ lockdown + nVar, data = x)
sjPlot::tab_model(m, mt)



#can only compare alpha
all_si_lockdowns <- all_si_lockdowns %>% filter(nVar == "[2] Alpha")

ci <- get_ci(
  all_si_lockdowns,
  group1 = step,
  group2 = lockdown,
  variable = serial_interval ,
  FUN = mean ,
  na.rm = TRUE
)
ci

densities <-
  get_densities(
    all_si_lockdowns,
    group1 = step,
    group2 = lockdown,
    variable = serial_interval,
    FUN = mean,
    na.rm = TRUE
  ) 

source("scripts/plot_densities.R")
plot_densities(
  all_si_lockdowns,
  densities,
  ci,
  group1 = step,
  group2 = lockdown,
  variable = serial_interval,
  FUN = mean
) + facet_wrap(~lockdown)



jitter_points <- all_si_lockdowns %>% 
  group_by(step, lockdown) %>% 
  summarise( temp = mean(serial_interval))

ggplot()+
  geom_line(data = densities,
            aes(x = density.x,
                y = density.y ,
                col = lockdown))+
  geom_area(data = densities,
            aes(x = density.x,
                y = density.y,
                fill = lockdown),
            alpha = 0.4)+
  geom_point(data = ci,
           aes(x = est,
               y =  2,
               col = lockdown),
           size = 3)+
  geom_errorbarh(data = ci,
                aes(xmin = lwr_ci,
                    xmax = upr_ci,
                    y = 2, #as.numeric(quantile(density_data$density.y, probs = 0.6)),
                    col = lockdown),
                height = 0.3,
                size = 1.2) +
  geom_jitter(data = jitter_points,
              aes(x = temp,
                  y = 0.5,
                  col = lockdown),
              shape=1,
              size = 1,
              height =1)+
  # scale_color_fira(values = rev(firaCols))+
  # scale_fill_fira()+
  scale_color_manual(values = c("#daa03dff", "#616247ff"))+ 
  scale_fill_manual(values = c("#daa03dff", "#616247ff"))+
  coord_cartesian(xlim = c(min(densities$density.x) - 0.001,max(densities$density.x) - 0.001))+
  theme_fira()+
  theme(legend.title = element_blank(),
        legend.position = "top")+
  labs(x = "serial interval (days)",
       y = "density")


