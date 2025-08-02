#######################################
# File: 0_Controller
# Author: Ellen Maas
# Date: Sept 23, 2022
# Description: This script is the control file for the project. 
# It generates all the data input files for all models for a 
# given site. It includes weather, soil, and management. It 
# creates the inputs for APSIM and Daycent, then runs 
# these two models and generates output files. Then the process 
# is repeated for RothC and Millennial, which use Daycent output 
# for their input. Note that the scenario 6 group (biochar) is
# only available in APSIM.
#
#######################################

print("Starting 0_Controller2.R")

#**********************************************************************
#### This block of code is for testing purposes, when NOT starting a run with 
#### either 00_Main_KBS.R or 00_Main_LRF.R. This is a short-cut to be able
#### to run the process line-by-line manually for a single scenario more easily
#### than modifying a 00_Main file. Note that some scripts, in order to be
#### run independently, must at least have the site's 0_Observations_and_constants
#### script run first as a prerequisite as well as the following lines in
#### this section. This code block can be modified as necessary to uncomment 
#### the variables for the site needed, and then 0_Observations_and_constants can 
#### be run.
#
#
#### These three lines can be run to create a clean slate for a fresh run,
#### erasing all variables from prior R activity:
#
rm(list=ls())
master_path <- "~/CaseStudy"
setwd(master_path)
#
#
#### When doing a partial run, uncomment *EITHER* KBS- or LRF-specific
#### variable lines:
#
### -- Start KBS-specific variables
site_name <- "KBS"
latitude = 42.410
longitude = -85.372
elevation_m = 288
experiment_start_year <- 1989
experiment_end_year <- 2021
end_exp_period_year <- 2021
experiment_year_range <- experiment_start_year:end_exp_period_year
end_fut_period_year <- 2050
max_fut_period_year <- 2100
calib_mgmt_grps <- c(3,5)
calib_mgmt_nums <- c(3,53,56)
obs_path <- paste0("Data/",site_name,"/Calibration/")
hist_wth_filename <- "NOAA-based Daily Kalamazoo 1900-2020.csv"
hist_wth_mon_filename <- "Monthly Kalamazoo 1900-2020 with OPE.csv"
curr_local_wth_filename <- "12-lter+weather+station+daily+weather+all+variates+1657202230.csv"
nasapower_output_filename <- paste0(site_name,"_np.csv")
### -- End KBS-specific variables
#
# ## -- Start LRF-specific variables
# site_name <- "LRF"
# latitude = 33.684
# longitude = -101.768
# elevation_m = 990
# experiment_start_year <- 2003
# experiment_end_year <- 2010
# experiment_start_date <- "2003-01-01"
# experiment_end_date <- "2010-12-31"
# end_exp_period_year <- 2021
# experiment_year_range <- experiment_start_year:end_exp_period_year
# end_fut_period_year <- 2050
# end_fut_period_date <- "2050-12-31"
# max_fut_period_year <- 2100
# calib_mgmt_grps <- c(3,5,7,8)
# calib_mgmt_nums <- c(3,53,56,7,8)
# obs_path <- paste0("Data/",site_name,"/")
# obs_filename <- "LibertyResearchFarm_adj.xlsx"
# curr_wth_tab <- "WeatherDaily"
# hist_raw_wth_filename <- "CDO_Lubbock_area.csv"
# hist_wth_filename <- "NOAA-based Daily Lubbock 1940-2021.csv"
# hist_wth_mon_filename <- "NOAA-based Monthly Lubbock 1940-2021 with OPE.csv"
# curr_local_wth_filename <- "" # included in GRACEnet spreadsheet (obs_filename)
# ## -- End LRF-specific variables
#
### When doing a partial run, the following variables should ALWAYS be
### uncommented, and the specific climate and management scenario needed
### set in clim_scenario_num, mgmt_scenario_grp, and mgmt_scenario_opt:
#
mgmt_path=paste0("Data/",site_name,"/Management/")
adjusted_ops_filename="clean_ops_ext_adj.csv"
wth_path <- paste0("Data/",site_name,"/Weather/")
apsim_path <- paste0("APSIM/",site_name,"/")
daycent_path <- paste0("Daycent/",site_name,"/")
rothc_path <- paste0("RothC/",site_name,"/")
mill_path <- paste0("Millennial/R/simulation/",site_name,"/")
#
clim_scenario_num <- 1
mgmt_scenario_grp <- 5 # scenario group number
mgmt_scenario_opt <- 3 # scenario detail number; put "" if none
mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)
#**********************************************************************
#
#
#*************************************************************
#*************************************************************
# Set up observational data variables and global constants------------------
#*************************************************************
#*************************************************************

## These are used in multiple functions.
source(paste0("0_Observations_and_constants_",site_name,".R"))




#*************************************************************
#*************************************************************
# Setup models ------------------------------------------------------------
#*************************************************************
#*************************************************************

#### This block only needs to be run once at the beginning to
#### set up the soil and other site data. Otherwise, it can
#### stay commented out to save time.

# Create soil input data

# if(mgmt_scenario_grp!=6) {
# 
#   source(paste0("2_Create_soil_data-setup2_",site_name,".R"))
#   #
# #   # ## APSIM -------------
# #
# #   ######## *********** NOTE!!!! ******** ###############
# #   ######## Due to poor performance in APISM with the 
# #   ######## measured soil properties (KS especially),
# #   ######## this should no longer be run for APISM. The
# #   ######## soil property values manually set in the
# #   ######## .apsimx files should only be used.
# #   ######## ***************************** ###############
# #
# #   # ### **** Prerequisite: APSIM .apsim files must already exist ****
# #   # ### All alternative scenarios (built off of the field/calibration
# #   # ### treatments) are linked to their respective field treatment in
# #   # ### the .apsim files. Only the field treatment needs to be updated
# #   # ### and all alternative scenarios will be updated automatically.
# #   # ### However, scenario group 6 is setup manually.
# #   # if(mgmt_scenario_num %in% calib_mgmt_nums) {
# #   #   stop("Do not run this for APSIM-use manual property values already in each .apsimx file")
# #   #   source("2_Create_soil_data-APSIM.R")
# #   # }
# 
#   ## Daycent --------------
#   source("2_Create_soil_data-Daycent.R")
# 
#   # also create separate soils input for Daycent spin-up for KBS site only
#   rm(soil_df)
#   source("2_Create_soil_data-setup2_Daycent_base_KBS.R")
#   source("2_Create_soil_data-Daycent_base1_KBS.R")
#   source("2_Create_soil_data-Daycent_base2_KBS.R")
#   #
#   #
#   ## RothC ----------------
#   ### Only uses clay content, which is included in the weather input file.
# }
# 
# 
#*************************************************************
# 
# Create management input files (APSIM and Daycent)
#
# source(paste0("3_Create_management_input_files-setup_",site_name,".R"))
#
# source(paste0("3_Create_management_input_files-APSIM_",site_name,".R"))
#
## After APSIM management data files are generated into /APSIM/<site_name>,
## named "mgmt_#_#.txt", where the first # is the 1-digit climate scenario
## number and the second # is the 1- or 2-digit management scenario number.
## The text in each file must be copied and pasted the into the
## Operations Schedule model window for that scenario in the
## corresponding APSIM scenario file and saved. The APSIM scenario files
## follow the same naming convention as "scen_#_#.apsim".
## This step only needs to be performed once, unless the data in the
## management schedule changes.
## NOTE: not every scenario has its own .apsim file, as some are bundled
## together into one file.
# 
# if(mgmt_scenario_grp!=6) {
#   source(paste0("3_Create_management_input_files-Daycent_",site_name,".R"))
# }
# 
# ## Management input files for RothC are created after Daycent
# ## has been run.
# 
# 
# ## for testing:
# # harvests <- full_ops_ext_adj[full_ops_ext_adj$observation_type=="Harvest",] %>%
# # mutate(doy = yday(date)) %>%
# #   select(year,date,doy,crop,observation_type)
# 
# 
# #*************************************************************
# #*************************************************************
# # Run models --------------------------------------------------------------
# #*************************************************************
# #*************************************************************
# 
# stop("Set up and run APSIM")
#
# ## APSIM Classic is currently run manually.
# 
# ## Before APSIM Classic scenarios are run, make sure that the
# ## management data files generated in the step above and 
# ## written to /APSIM/<site_name> are copied and pasted the into the 
# ## Operations Schedule model window in that scenario's APSIM
# ## file, saved, and run.
# 
#*************************************************************
# 
# # Daycent
# if(mgmt_scenario_grp!=6) {
#   source(paste0("Daycent/Daycent_run_controller.R"))
# }
# 
# 
#*************************************************************
#*************************************************************
# Graph and analyze APSIM and Daycent -----------------------------
#*************************************************************
#*************************************************************
# 
# APSIM
source(paste0("9_Results_APSIM-setup_",site_name,".R"))
#
model_name <- "APSIM"
# # Prerequisite: before the APSIM calibration script can be
# # run, need to first run (same as above):
# source(paste0("3_Create_management_input_files-setup_",site_name,".R"))
# source(paste0("3_Create_management_input_files-APSIM_",site_name,".R"))
# #
# if(clim_scenario_num==1 & mgmt_scenario_num %in% calib_mgmt_nums) {
#   source(paste0("9_Results_APSIM-calibration2_",site_name,".R"))
# }
source(paste0("9_Results_APSIM-future_",site_name,".R"))
source("p_Results_analysis.R")
#
#*************************************************************
#
# # Daycent
#  if(mgmt_scenario_grp!=6) {
# source(paste0("9_Results_Daycent-setup_",site_name,".R"))
# 
#  model_name <- "Daycent"
#  if(clim_scenario_num==1 & mgmt_scenario_num %in% calib_mgmt_nums) {
#    source(paste0("9_Results_Daycent-calibration_",site_name,".R"))
#  }
#  source(paste0("9_Results_Daycent-future2_",site_name,".R"))
#  source("p_Results_analysis.R")
#  }
# 
# 
# #*************************************************************
# #*************************************************************
# # Set up and run RothC, driving from Daycent ---------------
# #*************************************************************
# #*************************************************************
#
# Create management input files for RothC.
# RothC is run manually.
# 
# if(mgmt_scenario_grp!=6) {
# 
#   # Prerequisite: first need to run (above):
#   # 3_Create_management_input_files-setup_",site_name,".R
#   # and 9_Results_Daycent-setup_",site_name,".R
#   source(paste0("3_Create_management_input_files-setupRM_",site_name,".R"))
# 
#   # RothC
#   source(paste0("3_Create_management_input_files-RothC_",site_name,".R"))
#   source(paste0("4_Create_additional_files-RothC_",site_name,".R"))
#   # OPTIONAL: alternative management input data to a spreadsheet for testing purposes
#   # source(paste0("5_Alternative_management_input_files-RothC_",site_name,".R"))
# # 
# # RothC is currently run manually: after management and scenario
# # files are created, open RothC/KBS folder. Because each treatment
# # involved different calibration (due to different enough SOC measurements
# # between them), different Equil.dat files must be created for each 
# # calibration treatment (3, 53, 56) in the landman folder. Run this 
# # code once for scenario 1_3 and rename the Equil.dat file created at
# # RothC/KBS/landman to Equil_3.dat. Then run this code once for 1_53
# # and 1_56, renaming the Equil.dat file after each run. Then copy the 
# # Equil_3.dat to Equil.dat and run RothC for 1_3 and 5_3. Then delete
# # Equil.dat and copy Equil_53.dat to a fresh Equil.dat. Run all
# # scenarios from 41-44 and 51-53 for both 1 and 5 climate scenarios. 
# # Then copy Equil_56.dat to a fresh Equil.dat and run RothC for the
# # 54-56 scenarios in both climate scenarios. After all scenarios 
# # have been run from the software, then proceed below with the output.
#
# stop("Run RothC")
# 
# 
# }
# 
#
# #*************************************************************
# #*************************************************************
# # Graph and analyze RothC ----------------------------------
# #*************************************************************
# #*************************************************************
# 
# 
# # RothC
# if(mgmt_scenario_grp!=6) {
# source(paste0("9_Results_RothC-setup_",site_name,".R"))
# 
# model_name <- "RothC"
# if(clim_scenario_num==1 & mgmt_scenario_num %in% calib_mgmt_nums) {
#   source(paste0("9_Results_RothC-calibration_",site_name,".R"))
# }
#   source("9_Results_RothC-future.R")
#   source("p_Results_analysis.R")
# }
# 
# 
#*************************************************************
# 
# 
# 
# #*************************************************************
# #*************************************************************
# # Graph ensemble compilations ---------------------------------------------
# #*************************************************************
# #*************************************************************
# 
# #### NOTE: This script only works when ALL models have been run in a
# #### single script run. In other words, all "9_Results_..._setup"
# #### scripts above must have been run for each model in the same
# #### run prior to running this script. Each "9_Results_..._setup"
# #### file creates variables in the Global Environment memory
# #### which are required for this script to run without error.
# source(paste0("10_Model_Ensemble_results-by_scenario_",site_name,".R"))
# 
