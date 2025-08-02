#######################################
# File: 00_Main_LRF.R
# Author: Ellen Maas
# Date: Nov 11, 2022
# Description: This is the master process and starting point 
# to automate the running of whatever is set in 0_Controller2.R
# for the Liberty Research Farm, TX. It will loop
# through all the scenarios defined by the clim_nums and mgmt_grps
# variables.
#
#######################################

library(pracma)
library(dplyr)

# start timer
tic()

rm(list=ls())
master_path <- "~/CaseStudy"
setwd(master_path)

site_name <- "LRF"
latitude = 33.684
longitude = -101.768
elevation_m = 990
experiment_start_year <- 2003
experiment_end_year <- 2010
experiment_year_range <- experiment_start_year:experiment_end_year
experiment_start_date <- "2003-01-01"
experiment_end_date <- "2010-12-31"
end_exp_period_year <- 2021
end_fut_period_year <- 2050
max_fut_period_year <- 2100
calib_mgmt_grps <- c(3,5,7,8)
calib_mgmt_nums <- c(3,53,56,7,8)
#
fut_weather_path <- paste0("Data/CMIP6/",site_name,"/")
obs_path <- paste0("Data/",site_name,"/")
obs_mgmt_path <- paste0("Data/",site_name,"/Management/")
obs_filename <- "LibertyResearchFarm_adj.xlsx"
curr_wth_tab <- "WeatherDaily"
wth_path <- paste0("Data/",site_name,"/Weather/") 
hist_raw_wth_filename <- "CDO_Lubbock_area.csv"
hist_wth_filename <- "NOAA-based Daily Lubbock 1940-2021.csv"
hist_wth_mon_filename <- "NOAA-based Monthly Lubbock 1940-2021 with OPE.csv"
curr_local_wth_filename <- "" # included in GRACEnet spreadsheet (obs_filename)
apsim_path <- paste0("APSIM/",site_name,"/") 
daycent_path <- paste0("Daycent/",site_name,"/")
if(Sys.info()['sysname']=='Linux') {
  dndc_path <- paste0("LDNDC/ldndc-1.35.2.linux64/projects/",site_name,"/")
} else {
  dndc_path <- paste0("LDNDC/ldndc-1.35.2.win64/projects/",site_name,"/")
}
rothc_path <- paste0("RothC/",site_name,"/")
mill_path <- paste0("Millennial/R/simulation/",site_name,"/")

#**********************************************************************

# # create future climate files (scenarios 2-5)
# ## ***** Keep this section permanently commented out, as after the
# ## ***** first run, it is no longer needed because the future weather
# ## ***** files only need to be created once. Only use if the files
# ## ***** need to be regenerated for some reason or testing purposes.
# ## ***** When running this block, the remaining script in this file
# ## ***** up to the last section with the "end timer" comment can be 
# ## ***** commented out as the creation of future weather files ends 
# ## ***** with this block.
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
#   p_Create_future_weather_files(clim_scenario_num,latitude,longitude,
#                                 experiment_end_year)
# }
# source("p_Future_weather_reanalysis.R")

#**********************************************************************


# Loop through the scenarios; set which climate and management
# scenario numbers to use for this run:
clim_nums <- c(1,5)
mgmt_grps <- c(3:8) # calib_mgmt_grps # 


for (x in clim_nums) { # climate scenarios
  print("************************************")
  print("####### New climate scenario #######")
  print("************************************")
  print(paste0("climate scenario: ",x))
  clim_scenario_num <- x
  # ## ***** Keep the following line permanently commented out, as after the
  # ## ***** first run, it is no longer needed because the weather files only need
  # ## ***** to be created once. Only use if the files need to be re-
  # ## ***** generated for some reason or testing purposes:
  # source("1_Create_weather_input_files.R")
  for (y in mgmt_grps) { # management scenario groups
    mgmt_scenario_grp <- y # scenario group number
    max_scenario_options <- if_else(y==4, 4, # option numbers for those with incremental adjustments
                            if_else(y==5, 6,
                            if_else(y==6, 5, 1)))
    for (z in 1:max_scenario_options) {
      print("************************************")
      print(paste0("climate scenario: ",x))
      print(paste0("mgmt scenario: ",y))
      print(paste0("mgmt option: ",z))
      mgmt_scenario_opt <- if(max_scenario_options==1) "" else z
      mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
      scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)
      print(paste0("scenario_name: ",scenario_name))
      source("0_Controller2.R")
      }
  } # end loop through management scenario groups
} # end loop through climate scenarios


# Only run these two scripts if KBS has already been run and final output files generated,
# otherwise comment these out. # ***NOTE: The first script will only work when all climate 
# and management scenarios are selected to be run (clim_nums = c(1,5) and mgmt_grps = c(3:6)):
 source(paste0("10_Model_Ensemble_results-combined_scenarios_",site_name,".R"))
 source("10_Model_Ensemble_results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios6.R")

#**********************************************************************

# end timer
run_time <- round(toc(echo=TRUE)/60,1)
print(paste0("Run time is ",run_time," minutes, ",run_time/60," hours."))

      