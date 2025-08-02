#######################################
# Script: 1_Create_weather_input_files.R
# Author: Ellen Maas
# Date: Dec. 21, 2022
# Description: This script is a control file just for the weather
# file generation scripts. This only needs to be run once per climate
# scenario, as defined in the clim_scenario_num variable of the
# 0_Controller2.R file, or clim_nums variable in either of the 
# 0_Main_<site>.R files.
#######################################

suppressMessages({
  
  print(paste0("Starting 1_Create_weather_input_files-APSIM_",site_name,".R"))
  
  source(paste0("1_Create_weather_input_files-setup_",site_name,".R"))
  #
  source(paste0("1_Create_weather_input_files-APSIM_",site_name,".R"))
  source(paste0("1_Create_weather_input_files-Daycent_",site_name,".R"))
  source(paste0("1_Create_weather_input_files-RothC_",site_name,".R"))
  # Millennial doesn't use climate data as inputs.
  # Uses soil temperature and moisture together
  # with daily NPP (C input). Generated in the
  # 3_Create_management_input_files-setupRM 
  # scripts.
  
  
}) # end suppressMessages