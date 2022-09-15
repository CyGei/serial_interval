# SET-UP ------------------------------------------------------------------
source("scripts/libraries.R")
source("scripts/functions.R")
source("scripts/plot_densities.R")
source("scripts/fira_theme.R") # from fira_theme package github

#use here::


# DATA --------------------------------------------------------------------
source("scripts/load.R")
#data_update = "2022-08-12"
save_path = paste0("data/", data_update, "/")
dir.create(save_path, recursive = TRUE)


data_list <- split(data, f = data$household_id)

#single start dates households:
ssd_household <- data %>% 
  filter(single_start_dt == TRUE) %>% 
  .$household_id %>% 
  unique(.)
length(ssd_household) / length(unique(data$household_id))

# take out ssd_households for model
model_data <- data %>% 
  filter(!c(household_id %in% ssd_household)) %>% 
  select(household_id, individual_id, start_dt_minus1) %>% 
  split(., f = .$household_id)


source("scripts/table_demographics.R")
tab

# INPUT DISTRIBUTIONS ---------------------------------
## We use  Latin Hypercube Sampling to generate the distributions
## that will serve as input to our model

source("scripts/LHS.R")
set.seed(987)
#Gayani et al 2020 1st 2 values
#S Hart 2022, 3rd,4th values
#Hart et.al 2022 last value
generation_time_params <- get_lhs(
  MUs = c(5.2, 3.95, 4.5,3.2, 4.2),
  SDs = c(1.72, 1.51, 3.4, 2.5, 4.9),
  n = 100
)

#lit_search.R
#Quesada et.al, 2021 review
incubation_params<- get_lhs(
  MUs = c(6.5, 5,5.6, 6.7, 4.9),
  SDs = c(2.6, 3, 2.8, 5.2, 2.2),
  n = 100
)



params_df <- data.frame(incub_shape = incubation_params$shape,
                        incub_scale = incubation_params$scale,
                        gt_shape = generation_time_params$shape,
                        gt_scale = generation_time_params$scale)

gt_LHS <- map2(.x = generation_time_params$shape, .y = generation_time_params$scale,
                ~distcrete("gamma", interval = 1, shape = .x, scale = .y)$d(1:50) %>% 
                tails_pmf() %>% 
                 sanitize_pmf())

incub_LHS <- map2(.x = incubation_params$shape, .y = incubation_params$scale,
                   ~distcrete("gamma", interval = 1, shape = .x, scale = .y)$d(1:50) %>% 
                    tails_pmf() %>% 
                    sanitize_pmf())



source("scripts/LHS_plot.R")
p_LHS_dist 

# OUTBREAKER MODEL --------------------------------------------------------

config <- create_config(
  move_kappa = FALSE,# do not look for missing cases
  move_pi = FALSE,  # reporting rate
  move_mu = FALSE,#mutation rate
  init_kappa = 1,#number of generations before the last sampled ancestor
  init_pi = 1,#100% of reporting rate = all cases are reported
  find_import = TRUE,# imported cases,
  outlier_threshold = 2,
  init_tree = "star")
source("scripts/save_input_data.R")

#started at 2.39AM
source("scripts/outbreaker_future_groups2.R")

n_cores <- as.numeric(future::availableCores() - 1) 

time.outbreaker_results <-system.time({
  outbreaker_results <- outbreaker_future_groups(
    n = 100, #sets of distributions
    df_list =  model_data,
    date_column = "start_dt_minus1",
    id_column = "individual_id",
    outbreaker_configuration = config,
    w_params = gt_LHS ,
    f_params = incub_LHS,
    workers = n_cores)
})

saveRDS(outbreaker_results, file = paste0(save_path, "outbreaker_results.RData") )


# SERIAL INTERVAL ---------------------------------------------------------
# extracting the serial interval
# from the outbreaker2 results
plan(multisession, workers = n_cores)
options(future.globals.maxSize = 5000*1024^2 ) #for 5G of data

#chose lower & upper si to display
lower_si = -10
upper_si = 20

source("scripts/SI.R")
head(all_si_nofilter)
head(all_si)

#SI aggregated by LHS, step & variant
nested_si <- all_si %>% 
  group_by(LHS,step, nVar) %>% 
  summarise(serial_interval = list(serial_interval)) %>% 
  as_tibble() %>% 
  ungroup()
nested_si_nofilter <- all_si_nofilter %>% 
  group_by(LHS,step, nVar) %>% 
  summarise(serial_interval = list(serial_interval)) %>% 
  as_tibble() %>% 
  ungroup()


# POSTERIOR SERIAL INTERVAL ------------------------------------------
source("scripts/posterior_mean_sd.R")
p_posterior_mu_sd
tab_posterior_mu_sd

# ECDF --------------------------------------------------------------------
source("scripts/ecdf.R")
tab_cdf
p_cdf


# FITTING DISTRIBUTIONS ---------------------------------------------------
source("scripts/fit_disc.R")
source("scripts/fitted_SI.R")
head(fitted_steps)



# GAMMA FITS --------------------------------------------------------------
source("scripts/gamma_fits.R")
p_posterior_gammas
tab_posterior_gammas
p_gamma_fits



# MEAN SI TRENDS ----------------------------------------------------------
source("scripts/compare_trends.R")
p_trend_ridges
p_trend_heatmap



# COMPARE FITTED DISTRIBUTIONS ------------------------------------------------------------
source("scripts/compare_fits.R")
p_compare_fits
dens_all_si

# PAIRWISE MODEL VS OUTBREAKER2 -------------------------
#chose lower & upper si to display
lower_si = -30
upper_si = 30
source("scripts/pairwise.R")
p_pairwise_hist_overall
p_pairwise_hist
dens_all_si_nofilter



# ALL NAIVE MODELS VS OUTBREAKER ------------------------------------------
lower_si = -30
upper_si = 30
source("scripts/naive_models.R")
p_naive_models




# LOST DATA FROM TRUNCATION -----------------------------------------------
lower_si = -10
upper_si = 20

source("scripts/truncation.R")
p_ecdf_outbreaker_all_si_nofilter

# EpiCurves ----------------------------------------------------

source("scripts/epicurve.R")
p_prop_tested #prop of tested illnesses
p_prop_positive #prop of positive results
p_cumulative_incid
p_epicurve #cases over time
p_vaccine # vaccinated cumulative curve



# LHS SANITY CHECK --------------------------------------------------------
source("scripts/LHS_sanity_check.R")



