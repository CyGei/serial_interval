
# WITH FUTURE INSIDE HELPER -----------------------------------------------
outbreaker_future_groups_helper <-  function(i,
                                     df_list,
                                     date_column,
                                     id_column,
                                     outbreaker_configuration,
                                     w_params,
                                     f_params,
                                     workers) {
  
  ## this function runs the model once 
  ## over a list of dataframes
  
  w_distribution =  distcrete::distcrete(
    "gamma",
    shape = w_params[["shape"]][i],
    scale = w_params[["scale"]][i],
    interval = 1,
    w = 1
  )
  
  f_distribution = distcrete::distcrete(
    "gamma",
    shape = f_params[["shape"]][i],
    scale = f_params[["scale"]][i],
    interval = 1,
    w = 1
  )
  
  
  oplan <- plan(multisession, workers = workers)
  on.exit(plan(oplan))
  
  future_lapply(df_list,
                function(x) {
                  outbreaker(
                    data = outbreaker_data(
                      dates = x[[date_column]],
                      ids = x[[id_column]],
                      w_dens = w_distribution$d(1:100) %>% as_tibble() %>% 
                        mutate(value = ifelse( lag(value)==0 &  value != 0 & lead(value)== 0, 0, value)) %>% 
                        mutate(value = ifelse(value == 0 & lead(value) != 0, 1e-310, value),
                               value = replace_na(value, 0)) %>% 
                        pull(),
                      f_dens = f_distribution$d(1:100) %>% as_tibble() %>% 
                        mutate(value = ifelse( lag(value)==0 &  value != 0 & lead(value)== 0, 0, value)) %>% 
                        mutate(value = ifelse(value == 0 & lead(value) != 0, 1e-310, value),
                               value = replace_na(value, 0)) %>% 
                        pull()
                      ),
                    config = outbreaker_configuration
                  )
                },
                future.seed=TRUE)
  
}


outbreaker_future_groups <- function(n,
                                    df_list,
                                    date_column,
                                    id_column,
                                    outbreaker_configuration,
                                    w_params,
                                    f_params,
                                    workers) {
  
  ## this function loops over outbreaker_future_group_helper
  ## n times with n different sets of distributions
  
  result <- lapply(
    1:n,
    outbreaker_future_groups_helper,
    df_list = df_list ,
    date_column = date_column,
    id_column = id_column,
    outbreaker_configuration = outbreaker_configuration,
    w_params = w_params ,
    f_params = f_params,
    workers = workers
  )
  
  #binds the results into a list containing
  # n(groups) elements where each element is
  # a dataframe of 201*n rows 
  result_flat <- unlist(result, recursive = FALSE)
  result_condensed <-
    tapply(result_flat, names(result_flat), dplyr::bind_rows) #x, index, function
  return(result_condensed)
}



# 
# time.test_f_in_helper<-system.time({
#   test_f_in_helper <- outbreaker_future_group(
#     n = 3,
#     df_list = mydf_list ,
#     date_column = "date_onset",
#     id_column = "id",
#     outbreaker_configuration = config,
#     w_params = generation_time_params ,
#     f_params = incubation_params,
#     workers = 10)  
# })
# 
# 
# 
# # WITH FUTURE OUTSIDE HELPER -----------------------------------------------
# 
# outbreaker_future_groups_helper <-  function(i,
#                                              df_list,
#                                              date_column,
#                                              id_column,
#                                              outbreaker_configuration,
#                                              w_params,
#                                              f_params) {
#   
#   ## this function runs the model once 
#   ## over a list of dataframes
#   
#   w_distribution =  distcrete::distcrete(
#     "gamma",
#     shape = w_params[["shape"]][i],
#     scale = w_params[["scale"]][i],
#     interval = 1,
#     w = 1
#   )
#   
#   f_distribution = distcrete::distcrete(
#     "gamma",
#     shape = f_params[["shape"]][i],
#     scale = f_params[["scale"]][i],
#     interval = 1,
#     w = 1
#   )
#   
#   lapply(df_list,
#                 function(x) {
#                   outbreaker(
#                     data = outbreaker_data(
#                       dates = x[[date_column]],
#                       ids = x[[id_column]],
#                       w_dens = w_distribution$d(1:100),
#                       f_dens = f_distribution$d(1:100)
#                     ),
#                     config = outbreaker_configuration
#                   )
#                 })
#   
# }
# 
# 
# outbreaker_future_groups <- function(n,
#                                      df_list,
#                                      date_column,
#                                      id_column,
#                                      outbreaker_configuration,
#                                      w_params,
#                                      f_params,
#                                      workers) {
#  
#   oplan <- plan(multisession, workers = workers)
#   on.exit(plan(oplan))
#   
#   ## this function loops over outbreaker_future_group_helper
#   ## n times with n different sets of distributions
#   
#   result <- future_lapply(
#     1:n,
#     outbreaker_future_groups_helper,
#     df_list = df_list ,
#     date_column = date_column,
#     id_column = id_column,
#     outbreaker_configuration = outbreaker_configuration,
#     w_params = w_params ,
#     f_params = f_params,
#     future.seed = TRUE)
#   
#   
#   #binds the results into a list containing
#   # n(groups) elements where each element is
#   # a dataframe of 201*n rows 
#   result_flat <- unlist(result, recursive = FALSE)
#   result_condensed <-
#     tapply(result_flat, names(result_flat), dplyr::bind_rows) #x, index, function
#   return(result_condensed)
# }
# 
# 
# 
# time.test_f_outside_helper<-
#   system.time({
#   test_f_outside_helper <- outbreaker_future_group(
#     n = 3,
#     df_list = mydf_list ,
#     date_column = "date_onset",
#     id_column = "id",
#     outbreaker_configuration = config,
#     w_params = generation_time_params ,
#     f_params = incubation_params,
#     workers = 10)  
# })
# 
# 
# list(time.test_f_in_helper =  time.test_f_in_helper,
#      time.test_f_outside_helper = time.test_f_outside_helper,
#      n.groups = length(mydf_list) )
