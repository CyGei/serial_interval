
# GET RESULTS -------------------------------------------------------------
#used in misc.R
##path = relative path where results are stored e.g. "./data/2022-05-04/"
get_results <- function(path, filenames = ".*") {
  
  csv_files <- list.files(path = path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)
  csv_list <- sapply(csv_files, data.table::fread, simplify = FALSE)
  
  rdata_files <- list.files(path = path, pattern = paste0(filenames,".RData", collapse ="|"), full.names = TRUE, recursive = TRUE)
  rdata_list <- sapply(rdata_files, readRDS, simplify = FALSE)
  
  all_files <- append(csv_list, rdata_list)
  names(all_files) <- gsub(path,"", names(all_files))
  names(all_files) <- gsub("/", "", names(all_files))
  names(all_files) <- gsub(".csv|.Rdata|.RData", "", names(all_files))
  
  return(all_files)
}



# GET SERIAL INTERVAL -----------------------------------------------------
##outbreaker_result = a single outbreaker result
##input_data = the input dataframe used for generating outbreaker_result
## date_col = character string referring to the column name in input_data e.g. "date_onset"
# 
extract_serial_interval <- function(outbreaker_result, input_data, date_col) {

  mat_infectors <- outbreaker_result %>%
    select(dplyr::starts_with("alpha")) %>%
    as.matrix()

  vec_infectors <- mat_infectors %>%
    as.vector()

  n_cases <- ncol(mat_infectors)
  vec_infectees <- rep(seq_len(n_cases), each = nrow(mat_infectors))

  onset_infectees <- input_data[[date_col]][vec_infectees]
  onset_infectors <- input_data[[date_col]][vec_infectors]

  serial_interval <- as.integer(onset_infectees - onset_infectors)
  serial_interval <- serial_interval[!is.na(serial_interval)]
}



get_si <- function(outbreaker_result, input_data, date_col){
  n_cases <- sum(str_count(colnames(outbreaker_result), "alpha"))
  
  df <- outbreaker_result %>% 
    select(step, dplyr::starts_with("alpha")) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = -step, names_to = "alpha", values_to = "infector") %>% 
    mutate(infectee = parse_number(alpha),
           onset_infectee = input_data[[date_col]][infectee],
           onset_infector = input_data[[date_col]][infector],
           serial_interval = as.integer(onset_infectee - onset_infector)) %>%  # mcmc_run = rep(row_number(), length.out = n(), each = n_cases)
    select(-alpha)
  return(df)
  
}



# CREDIBLE SI -------------------------------------------------------------
# see get_ci()
# credible_si <- function(outbreaker_result_si, lwr.limit = -20, upr.limit = 20){
#   outbreaker_result_si %>% 
#     filter(serial_interval <= upr.limit & serial_interval >= lwr.limit) %>% 
#     group_by(mcmc_run) %>% 
#     summarise(mean_si = mean(serial_interval, na.rm = TRUE)) %>% #get mean si for each mcmc run
#     ungroup() %>%
#     summarise(c_mean = mean(mean_si, na.rm = TRUE),
#               c_95.lwr = quantile(mean_si, probs = 0.025, na.rm = TRUE),
#               c_95.upr = quantile(mean_si, probs = 0.975, na.rm =TRUE)) %>%  #summarise overall mean and quantiles for the mean values
#     mutate(nVar = unique(outbreaker_result_si$nVar)) 
#   
# } 


# GET_CI ------------------------------------------------------------------
##data = a data frame of all outbreaker results concatenated + si values
##... = grouping variable eg nVar
## metric = sd or mean or cv
# get_ci <- function(data,...,metric){  
#   ci <- data %>% 
#     group_by(step, ...) %>% 
#     summarise(mean = mean(serial_interval),
#               sd = sd(serial_interval),
#               cv = sd / mean) %>%   ## add CV!!!
#     ungroup() %>% 
#     group_by(...) %>% 
#     summarise(est = mean({{metric}}),
#               lwr_ci = quantile({{metric}}, probs = 0.025),
#               upr_ci = quantile({{metric}}, probs = 0.975)) #variable
#   return(ci)
# }

cv <- function(x, ...){
  sd(x, ...)/mean(x, ...)
}

# get_ci <- function(data, group1, group2, variable, fun, ...){
#   data %>% 
#     group_by({{group1}}, {{group2}}) %>% 
#     summarise( temp = fun({{variable}},...)) %>% 
#     ungroup() %>% 
#     group_by({{group2}}) %>% 
#     summarise(est = mean(temp),
#               lwr_ci = quantile(temp, probs = 0.025,...),
#               upr_ci = quantile(temp, probs = 0.975,...))
# }
# 

# get_ci <- function(data, group1, group2, variable, FUN, ...){
#   FUN <- match.fun(FUN)
#   data %>% 
#     group_by({{group1}}, {{group2}}) %>% 
#     summarise( temp = FUN({{variable}},...)) %>% 
#     ungroup() %>% 
#     group_by({{group2}}) %>% 
#     summarise(est = mean(temp),
#               lwr_ci = quantile(temp, probs = 0.025,...),
#               upr_ci = quantile(temp, probs = 0.975,...))
# }


get_ci <- function(data, agg, group, variable, FUN, ...){
  
  FUN <- match.fun(FUN)
  all_groups <- c(agg, group)
  
  data %>% 
    #SI by step, nVar, LHS 
    group_by(across(all_of(all_groups))) %>% 
    summarise( temp = FUN({{variable}}, ...)) %>% 
    ungroup() %>% 
    
    #overall estimate by nVar
    group_by(across(all_of(c(group)))) %>% 
    summarise(est = mean(temp),
              lwr_ci = quantile(temp, probs = 0.025, ...),
              upr_ci = quantile(temp, probs = 0.975, ...))
}




#paste the estimate with credible intervals
ci_tab <- function(ci, metric){
  out <- ci %>% 
    dplyr::mutate(across(where(is.numeric),
                         ~format(round(., digits = 2)), nsmall = 2)) %>% 
    mutate({{metric}} := paste0(est, " [", lwr_ci, "-", upr_ci, "]")) %>% 
    select(nVar, {{metric}})
  return(out)
}



# GET DENSITIES -----------------------------------------------------------
##data = a data frame of all outbreaker results concatenated + si values
##... = grouping variable eg nVar
## metric = sd or mean or cv
## bw = bandwidth of the density
# get_densities <- function(data,..., metric){ 
#   metric <- as.character(substitute(metric))
#   
#   densities <- data %>% 
#     group_by(step, ...) %>% 
#     summarise(mean = mean(serial_interval),
#               sd = sd(serial_interval),
#               cv = sd / mean) %>% 
#     ungroup() %>% 
#     group_by(...) %>% 
#     nest(nested_data = !group_cols()) %>% 
#     mutate(density = map(nested_data, ~density(.x[[metric]])),
#            density.x = map(density, ~.x[["x"]]),
#            density.y = map(density, ~.x[["y"]])) %>% 
#     select(..., density.x, density.y) %>% 
#     unnest(cols = c(density.x, density.y))
#   return(densities)
# }

# 
# get_densities <- function(data, group1, group2, variable, fun, ...){
#   data %>% 
#     group_by({{group1}}, {{group2}}) %>% 
#     summarise( temp = fun({{variable}}, ...)) %>% 
#     ungroup() %>% 
#     group_by({{group2}}) %>% 
#     summarise(density = list(density(temp, ...))) %>% 
#     mutate( density.x = map(density, ~.x[["x"]]),
#             density.y = map(density, ~.x[["y"]])) %>% 
#     select(-density) %>% 
#     unnest(cols = c(density.x, density.y))
# }

# get_densities <- function(data, group1, group2, variable, FUN, ...){
#   FUN <- match.fun(FUN)
#   
#   data %>% 
#     group_by({{group1}}, {{group2}}) %>% 
#     summarise( temp = FUN({{variable}}, ...)) %>% 
#     ungroup() %>% 
#     group_by({{group2}}) %>% 
#     summarise(density = list(density(temp, ...))) %>% 
#     mutate( density.x = map(density, ~.x[["x"]]),
#             density.y = map(density, ~.x[["y"]])) %>% 
#     select(-density) %>% 
#     unnest(cols = c(density.x, density.y))
# }



get_densities <- function(data, agg, group, variable, FUN, ...){
  FUN <- match.fun(FUN)
  all_groups <- c(agg, group)
  
  
  data %>% 
    
    #SI by step, nVar, LHS 
    group_by(across(all_of(all_groups))) %>% 
    summarise( temp = FUN({{variable}}, ...)) %>% 
    ungroup() %>% 
    
    #density estimate by nVar
    group_by(across(all_of(group))) %>% 
    summarise(density = list(density(temp, ...))) %>% 
    mutate( density.x = map(density, ~.x[["x"]]),
            density.y = map(density, ~.x[["y"]])) %>% 
    select(-density) %>% 
    unnest(cols = c(density.x, density.y))
}




# ID_LHS -----------------------------------------------------------

#input df must be the outbreaker result for 1 household
#steps are based on default outbreaker settings with burnin of 500
#this functions takes results from one household and counts
#how many times the 10000th step appears

id_lhs <- function(df) {
  df %>% 
    mutate(new_iter = case_when(step == 550 & 
                                  lag(step) == 10000 ~ "1",
                                TRUE ~ "0")) %>%  #count the number of new LHS iteration
    mutate(LHS = cumsum(new_iter), #Gives it an ID from 0 to n LHS
           LHS = LHS + 1) %>% # we want to start from 1
    select(-new_iter)
}



# PLOT --------------------------------------------------------------------

# 
# .CoV_pal <- c("[1] Wild Type" = "#808080", #F8766D       
#               "[2] Alpha" = "#CD9600",            
#               "[3] Delta" = "#005B96", #7CAE00            
#               "[4] Omicron BA1" = "#F1A2CF",
#               "[5] Omicron BA2" = "#971A82")
# 
# 
# plot_densities <- function(data, group, metric){
# 
# jitter_points <- data %>% 
#   group_by(step, {{group}}) %>% 
#   summarise(mean = mean(serial_interval),
#             sd = sd(serial_interval),
#             cv = sd / mean)
# 
# p <- ggplot()+
#   geom_line(data = densities,
#             aes(x = density.x,
#                 y = density.y,
#                 col = {{group}}))+
#   geom_area(data = densities,
#             aes(x = density.x,
#                 y = density.y,
#                 fill = {{group}},
#                 col = {{group}}),
#             alpha = 0.4)+
#   geom_point(data = ci,
#              aes(x = est,
#                  y = 5,
#                  col = {{group}}),
#              size = 2) +
#   geom_errorbarh(data = ci,
#                  aes(xmin = lwr_ci,
#                      xmax = upr_ci,
#                      y = 5,
#                      col = {{group}}),
#                  size = 0.8) +
#   geom_jitter(data = jitter_points,
#               aes(x = {{metric}},
#                   y = 1,
#                   col = {{group}}),
#               shape=1,
#               size = 0.8,
#               height = 2)+
#   scale_color_manual(values = .CoV_pal)+
#   scale_fill_manual(values = .CoV_pal)+
#   theme_bw()+
#   theme(legend.title = element_blank())
# 
# 
# return(p)
# 
# }


# FITTING -----------------------------------------------------------------

# 
# nll.disc_dist_norm <- function(data, par) { 
#   #1:data ignore 0 (because of peak)
#   fit_sample <- fit_sample[fit_sample != 0]
#   
#   
#   discrete_distribution <-
#     distcrete::distcrete("norm", interval = 1, par[1], par[2], w = 1) #par[1], par[2] how to generalise this "..." equivalent?
#   
#   #2:rescale the density function
#   discrete_distribution$d/(1-discrete_distribution$d(0)) # total mass outside of 0
#   
#   return(-sum(log(discrete_distribution$d(data)))) #divide discrete_distribution$d/(1-discrete_distribution$d(0)) # total mass outside of 0
#   #1:remove 0s from data
#   #2: rescale
#   #3: shift param = data + shift_param 
# }
# 
# theoretical_params <- optim( par= c(mean(si_sample_shifted), sd(si_sample_shifted)), 
#                              fn = nll.disc_dist_norm, 
#                              data = si_sample_shifted)
# theoretical_params


# 
# # HYP_TEST ----------------------------------------------------------------
# ##data = a data frame containing serial interval values for each variant
# ##variant1 = name of variant1
# ##variant2 = name of variant2
# 
# hyp_test <- function(data, variant1, variant2){
#   names(data) <-  gsub("[[:digit:]]|\\[|\\] ", "", names(data))
#   v1 <- as.character(substitute(variant1))
#   v2 <- as.character(substitute(variant2))
#   data %>% 
#     mutate(test = ifelse({{variant1}} < {{variant2}}, 
#                          paste0(v1, "<", v2),
#                          paste0(v1, ">=", v2))) %>% 
#     count(test) %>% 
#     mutate(perc = round(n/sum(n)*100, digits=2))
# }




# DISTCRETE FIX -----------------------------------------------------------
## issues when producing densities with distcrete
# 
# #1 some densities start with 0 values e.g. 0.000, 0.000, 0.1 ...
# replace_starting_0s<- function(x){
#   all_zero <- all(x == 0)
#   
#   if(FALSE == all_zero){
#     
#     first_higher_than_zero <- min(which(x > 0))
#     range <- 1:(first_higher_than_zero - 1)
#     x[range] <- 1e-100
#     # start <- 1
#     # while(start < first_higher_than_zero){
#     #   x[start] <- 1e-100
#     #   start <- start + 1
#     # }
#     return(x)}
#   else{
#     return(x)
#     
#   }
# }
# 
# #2 some densities are between 0 values e.g.  0.2, 0.6, 0.02, 0, 0.1 -> 
# replace_0_sandwich<- function(x){
#   zero_indexes <- which(x == 0)+1
#   zero_indexes <- zero_indexes[zero_indexes <= length(x)]
#   x[zero_indexes] <- 0
#   return(x)
# }
# 
# 
# distrcrete_fix <- function(distribution ){
#   fix <- distribution %>% 
#     replace_starting_0s() %>% 
#     replace_0_sandwich() %>% 
# 
#   return(fix)
#   
# }
# 


# FIX DENSITY -------------------------------------------------------------

fix_density <- function(x){
  is_positive <- is.finite(log(x))
  to_replace <- !is_positive
  val_replace <- min(x[is_positive])
  x[to_replace] <- val_replace
  
  return(x)
  
}

#fix_density(c(0,0,0, 0.5, 0.3, 0.2, 0, 0.1, 0, 0))
