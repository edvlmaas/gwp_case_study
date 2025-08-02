#######################################
# File: 00_Main_KBS.R
# Author: Ellen Maas
# Date: Nov 11, 2022
# Description: This is the master process and starting point
# to automate the running of whatever is set in 0_Controller2.R
# for the Kellogg Biological Station, MI. It will loop
# through all the scenarios defined by the clim_nums and mgmt_grps
# variables.
#
#######################################

library(pracma)
library(dplyr)

# start timer
tic()

rm(list = ls())
master_path <- "~/CaseStudy"
setwd(master_path)

site_name <- "KBS"
latitude = 42.410
longitude = -85.372
elevation_m = 288
experiment_start_year <- 1989
experiment_end_year <- 2021
experiment_year_range <- experiment_start_year:experiment_end_year
experiment_start_date <- "1989-01-01"
experiment_end_date <- "2021-12-31"
end_exp_period_year <- 2021
end_fut_period_year <- 2050
max_fut_period_year <- 2100
# set which management numbers represent the calibration treatments
calib_mgmt_grps <- c(3,5)
calib_mgmt_nums <- c(3,53,56) #T3, T1, T2
#
obs_path <- paste0("Data/", site_name, "/Calibration/")
obs_mgmt_path <- paste0("Data/", site_name, "/Management/")
hist_wth_filename <- "NOAA-based Daily Kalamazoo 1900-2020.csv"
hist_wth_mon_filename <- "Monthly Kalamazoo 1900-2020 with OPE.csv"
curr_local_wth_filename <-
  "12-lter+weather+station+daily+weather+all+variates+1657202230.csv"
wth_path <- paste0("Data/", site_name, "/Weather/")
nasapower_output_filename <- paste0(site_name, "_np.csv")
mgmt_path = paste0("Data/", site_name, "/Management/")
adjusted_ops_filename = "clean_ops_ext_adj.csv"
fut_weather_path <- paste0("Data/CMIP6/", site_name, "/")
apsim_path <- paste0("APSIM/", site_name, "/")
daycent_path <- paste0("Daycent/", site_name, "/")
rothc_path <- paste0("RothC/", site_name, "/")

#**********************************************************************

# # create future climate files (scenarios 2-5)
# ## ***** Keep this section permanently commented out, as after the
# ## ***** first run, it is no longer needed because the future weather
# ## ***** files only need to be created once. Only use if the files
# ## ***** need to be regenerated for some reason or testing purposes.
# ## ***** When running this block, the remaining script in this file
# ## ***** can be commented out as the creation of future weather
# ## ***** files ends with this block.
# ##
#
# print("**Create future CMIP6 climate files**")
# source("p_Create_future_weather_files.R")
# for (x in 2:5) {
#   print("************************************")
#   print("####### New climate scenario #######")
#   print("************************************")
#   print(paste0("climate scenario: ",x))
#   clim_scenario_num <- x
#     p_Create_future_weather_files(clim_scenario_num,latitude,longitude,
#                                   experiment_end_year)
# }
# source("p_Future_weather_reanalysis.R")

#**********************************************************************

# Loop through the scenarios; set which climate and management
# scenario numbers to use for this run:
clim_nums <- c(1,5)
mgmt_grps <- c(3:6) #calib_mgmt_grps #

for (x in clim_nums) {
  # climate scenarios
  print("************************************")
  print("####### New climate scenario #######")
  print("************************************")
  print(paste0("climate scenario: ", x))
  clim_scenario_num <- x
  # ## ***** Keep the following line permanently commented out, as after the
  # ## ***** first run, it is no longer needed because the weather files only need
  # ## ***** to be created once. Only use if the files need to be re-
  # ## ***** generated for some reason or testing purposes:
  # source("1_Create_weather_input_files.R")
  for (y in mgmt_grps) {
    # management scenario groups
    mgmt_scenario_grp <- y # scenario group number
    max_scenario_options <- if_else(y == 4, 4, # option numbers for those with incremental adjustments, such as fertilizer or residue scenarios
                            if_else(y == 5, 6,
                            if_else(y == 6, 5, 1)))
    for (z in 1:max_scenario_options) {
      print("************************************")
      print(paste0("climate scenario: ", x))
      print(paste0("mgmt scenario: ", y))
      print(paste0("mgmt option: ", z))
      mgmt_scenario_opt <- if(max_scenario_options == 1) "" else z
      mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp, mgmt_scenario_opt))
      scenario_name <- paste0(clim_scenario_num, "_", mgmt_scenario_num)
      source("0_Controller2.R")
    }
    
  } # end loop through management scenario groups
} # end loop through climate scenarios

# ***NOTE: The following script will only work when all climate and management scenarios are
# selected to be run (clim_nums = c(1,5) and mgmt_grps = c(3:6)):
source(paste0("10_Model_Ensemble_results-combined_scenarios_",site_name,".R"))
# The following script here can only be run if LRF has been run first and final output generated,
# otherwise, it should be run at the end of 00_Main_LRF.R:
# source(paste0("10_Model_Ensemble_results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios6.R"))

# end timer
run_time <- round(toc(echo = TRUE) / 60, 1)
print(paste0("Run time is ", run_time, " minutes, ", run_time / 60, " hours."))
