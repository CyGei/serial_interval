saveRDS(data, file = paste0(save_path, "data.RData"))
saveRDS(data_list, file = paste0(save_path, "data_list.RData") )
saveRDS(model_data, file = paste0(save_path, "model_data.RData") )

saveRDS(incubation_params, 
        file = paste0(save_path, "incubation_params.RData") )
saveRDS(generation_time_params, 
        file = paste0(save_path, "generation_time_params.RData") )
saveRDS(incub_LHS, 
        file = paste0(save_path, "incub_LHS.RData") )
saveRDS(gt_LHS, 
        file = paste0(save_path, "gt_LHS.RData") )

saveRDS(config, 
        file = paste0(save_path, "config.RData") )