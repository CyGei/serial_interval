source("scripts/libraries.R")
source("scripts/functions.R")
source("scripts/fira_theme.R")
source("scripts/plot_densities.R")


# READ IN SPECIFIC OUTPUT -------------------------------------------------

# 2022-08-01 --------------------------------------------------------------
data_update <- "2022-08-01"
filenames = c("data", "data_list", "all_si", "all_si_nofilter")
results <- get_results("data/2022-08-01", 
                       filenames = filenames )
list2env(results, globalenv())
rm(results)

# 2022-08-12 --------------------------------------------------------------
data_update <- "2022-08-12"
filenames = c("data", "data_list", "all_si", "all_si_nofilter")

results <- get_results("data/2022-08-12", 
                       filenames = filenames )
list2env(results, globalenv())
rm(results)



# 2022-08-17 --------------------------------------------------------------
data_update <- "2022-08-17"
results <- get_results("data/2022-08-17", 
                       filenames = ".*" )
list2env(results, globalenv())
rm(results)



