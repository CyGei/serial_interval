##burnin
outbreaker_results <- future_lapply(outbreaker_results,
                                    function(x) {
                                      x %>%
                                        filter(step > 500)
                                    })
##get_si
outbreaker_results_si <- future_sapply(names(outbreaker_results), 
                                       function(x) { 
                                         get_si(outbreaker_result = outbreaker_results[[x]],#extract serial interval
                                                input_data = data_list[[x]],  
                                                date_col = "start_dt") %>%
                                           dplyr::mutate(nVar = data_list[[x]]$nVar[1]) #add variant 
                                       },
                                       simplify = FALSE,
                                       USE.NAMES = TRUE)


# RESULTS -----------------------------------------------------------------


all_si_nofilter <- data.table::rbindlist(outbreaker_results_si, idcol = "household") %>% 
  drop_na(serial_interval)

## identify the LHS group
all_si_nofilter_list <- split(all_si_nofilter, f = all_si_nofilter$household)
all_si_nofilter <- map(.x = all_si_nofilter_list, ~id_lhs(.x)) %>% bind_rows()


## add the 0 SIs
source("scripts/same_date_si.R")
all_si_nofilter <- bind_rows(all_si_nofilter, si0)



all_si <- all_si_nofilter  %>% 
  filter(serial_interval >= lower_si & serial_interval <= upper_si) 


# SAVE --------------------------------------------------------------------
rm(all_si_nofilter_list)
saveRDS(outbreaker_results_si, file = paste0(save_path, "outbreaker_results_si.RData") )
saveRDS(all_si_nofilter, file = paste0(save_path, "all_si_nofilter.RData") )
saveRDS(all_si, file = paste0(save_path, "all_si.RData") )


