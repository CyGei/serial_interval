# run misc.R to retrieve data
# original input data to outbreaker2: model_data
model_data
upper_si = 20
lower_si = -10
# check_df function 
# removes households with 2 cases, who's onsets are too far from one another to be related 
# i.e. both are introductions

check_df <- function(df) {
  if (nrow(df)) {
    onset_diff <- abs(difftime(
      as.Date(df$start_dt_minus1[2]),
      as.Date(df$start_dt_minus1[1]),
      units = "days"
    ))
    if (as.numeric(onset_diff) > upper_si) {
      return(FALSE)
    }
  }
  return(TRUE)
}


model_data_checked <- lapply(model_data, function(df){
  if(check_df(df)){
    return(df)
  }
})
model_data_checked <- model_data_checked[!sapply(model_data_checked, is.null)]

length(model_data) - length(model_data_checked)

pre <- nrow(all_si_nofilter)

all_si_nofilter <- all_si_nofilter %>% 
  filter(household %in% names(model_data_checked))
post <- nrow(all_si_nofilter)
pre - post



#rerun truncation.R

# PROPORTIONS ------------------------------------------------------------------

excluded_lower_si <- all_si_nofilter %>% 
  filter( serial_interval < lower_si ) %>% 
  as_tibble() %>% 
  nrow()

excluded_upper_si <- all_si_nofilter %>% 
  filter( serial_interval > upper_si ) %>% 
  as_tibble() %>% 
  nrow()

(excluded_lower_si + excluded_upper_si) / nrow(all_si_nofilter) 
excluded_lower_si / nrow(all_si_nofilter)
excluded_upper_si / nrow(all_si_nofilter)





# OUTBREAKER --------------------------------------------------------------

###################
# emprical density
###################

dens_outbreaker <- all_si_nofilter %>% 
  summarise(density = list(hist(serial_interval, breaks = seq(min(serial_interval)-0.5, max(serial_interval)+0.5), plot = FALSE))) %>% 
  mutate(density.x = map(density, ~.x[["mids"]]),
         density.y = map(density, ~.x[["density"]])) %>% 
  select(-density) %>% 
  unnest(cols = c(starts_with("density")))

dens_outbreaker %>% 
  ggplot()+
  aes(x = density.x, y = density.y)+
  geom_line(aes(col ="Outbreaker"))+
  geom_line(data = dens_pairwise, aes(col = "Pairwise"))+
  scale_x_continuous(breaks = x_breaks )+
  geom_vline(aes(xintercept = lower_si), lty = "dashed")+
  geom_vline(aes(xintercept = upper_si), lty = "dashed")+
  scale_color_manual(values = c("Outbreaker" = "#fc8eac","Pairwise" = "#377EB8"))+
  labs(fill = "", col = "", lty="", x = "serial interval (days)", y = "density")+
  theme_fira()+
  theme(legend.position = "top")

#######
# all values
#######

ecdf_outbreaker_f <- ecdf(all_si_nofilter$serial_interval) 

ecdf_outbreaker<- data.frame(x =min(all_si_nofilter$serial_interval):max(all_si_nofilter$serial_interval),
                             y = ecdf_outbreaker_f(min(all_si_nofilter$serial_interval):max(all_si_nofilter$serial_interval)))

p_ecdf_outbreaker_all_si_nofilter <- ecdf_outbreaker  %>% 
  ggplot()+
  aes(x = x, y = y)+
  geom_point(col = "black", size = 1.2)+
  geom_line(col = "black", lty = "solid", alpha = 0.2)+
  geom_rect(aes(xmin = upper_si, xmax = Inf, ymin = -Inf, ymax = Inf), col = "#e0e0e0", alpha = 0.01)+
  geom_rect(aes(xmin = -Inf, xmax = lower_si, ymin = -Inf, ymax = Inf), col = "#e0e0e0", alpha = 0.01)+
  scale_y_continuous(breaks = seq(0, 1, 0.1))+
  scale_x_continuous(breaks = seq(-600, 600, 30))+
  theme_fira()+
  labs(x = "serial interval (days)", y = "cumulative probability")
