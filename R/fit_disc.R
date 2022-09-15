disc_deviance <- function(data, params ,outlier, name, interval, w, anchor){

  #1: remove outlier
  data0 <- data[data != outlier]
  data0 <- data0[complete.cases(data0)]
  
  #2: add shifting parameter to avoid negative values
  data0_shift <- data0 + params[3]
  
  d <- distcrete::distcrete(name,
                            interval,
                            params[1],
                            params[2],
                            w = w,
                            anchor = anchor)
  
  dens <- d$d(data0_shift) / (1 - d$d(outlier + params[3])) #rescaling
  dens <- replace(dens, dens == 0, 1e-20) # to avoid log(0)
  
  deviance <- -2*sum(log(dens))
  return(deviance)

}


fit_disc <- function(data, outlier, params, name, interval = 1, w = 0.5, anchor = 0){
  
  #input params are mu, sd,shift
  if (name == "gamma") {
    params <-
      as.numeric(
        c(gamma_mucv2shapescale(mu = params[1], cv = (params[2] / params[1])), params[3]))
    params <- c(params[1], 1/params[2], params[3]) # shape, rate, shift will fit to optim
    
  } else if (name == "lnorm") {
    params <- as.numeric(c(log(params[1]), log(params[2]), params[3]))
  }
  

  optimised_params <- optim(params,
                            fn = disc_deviance,
                            data = data,
                            outlier = outlier,
                            name = name,
                            interval = interval,
                            w = w,
                            anchor = anchor)

  distribution <- distcrete::distcrete(name,
                                       interval = interval,
                                       optimised_params$par[1],
                                       optimised_params$par[2],
                                       w = w,
                                       anchor = anchor)
  

  if (name == "gamma") {
    g_params <- gamma_shapescale2mucv(optimised_params$par[1], 1/optimised_params$par[2])

    out <- list(shape = optimised_params$par[1],
                rate = optimised_params$par[2],
                shift = optimised_params$par[3],
                mu = g_params$mu,
                sd = g_params$cv * g_params$mu,
                cv = g_params$cv) 
    # dist_par = distribution$parameters,
    # dist_dens = distribution$d(0:50)

  } else if (name == "lnorm") {
    out <-
      list(log_mu = optimised_params$par[1],
           log_sd = optimised_params$par[2],
           shift = optimised_params$par[3],
           mu = exp(optimised_params$par[1]),
           sd = exp(optimised_params$par[2]),
           cv = exp(optimised_params$par[2])/exp(optimised_params$par[1]))
  }else{
    out <- list(mu = optimised_params$par[1],
                sd = optimised_params$par[2],
                shift = optimised_params$par[3],
                cv = optimised_params$par[2]/optimised_params$par[1])
  }
  
  return(out)
}


# #####EXAMPLE:#####
# 
# set.seed(111)
# 
# ##generate sample data:
# mu = 2
# sd = 2
# samp <- c(round(rnorm(10000, mean = mu, sd = 3)), rep(mu, 1000))
# 
# data.frame(samp) %>%
#   ggplot()+
#   geom_histogram(aes(x = samp, y = ..density..),
#                  bins = length(unique(samp)),
#                  binwidth = 1,
#                  col = "white")
# 
# 
# fits <- c("gamma", "lnorm", "norm") %>%
#   purrr::set_names() %>%
#   map(.,
#     fit_disc,
#     data = samp,
#     outlier = 0,
#     params = base::c(7, 4.9, 1),
#   )
# 
# 
# fits_dens <- data.frame(x = samp, 
#                         gamma = fits$gamma$distribution$d(samp + fits$gamma$shift),
#                         norm = fits$norm$distribution$d(samp + fits$norm$shift),
#                         lnorm = fits$lnorm$distribution$d(samp + fits$lnorm$shift)) %>% 
#   pivot_longer(cols = -x, names_to = "distribution", values_to = "density")
# 
# 
# data.frame(samp) %>%
#   ggplot()+
#   geom_histogram(aes(x = samp, y = ..density..),
#                  bins = length(unique(samp)),
#                  binwidth = 1,
#                  col = "white")+
#   scale_x_continuous(breaks = 0:25)+
#   geom_line(data = fits_dens,
#             aes(x = x, y = density, col = distribution ))+
#   geom_point(data = fits_dens,
#             aes(x = x, y = density, col = distribution))


