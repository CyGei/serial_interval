
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
  
  w_distribution =  purrr::as_vector(w_params[i])
  
  f_distribution = purrr::as_vector(f_params[i])
  
  
  oplan <- plan(multisession, workers = workers)
  on.exit(plan(oplan))
  
  future_lapply(df_list,
                function(x) {
                  outbreaker(
                    data = outbreaker_data(
                      dates = x[[date_column]],
                      ids = x[[id_column]],
                      w_dens = w_distribution,
                      f_dens = f_distribution
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