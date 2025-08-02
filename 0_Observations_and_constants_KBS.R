#######################################
# Procedure: 0_Observations_and_constants_KBS.R
# Author: Ellen Maas
# Date: Oct 2, 2022
# Description: It is designed to be run as-is from calling scripts in order
# to create the variables in the local space. It imports data from files and sets 
# values to shared variables that will be used throughout the project.
#######################################

suppressMessages({

print(paste0("Starting 0_Observations_and_constants_",site_name,".R"))

  
library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(broom)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)

##################################################
#################### constants ###################
##################################################

  scenario_df <- data.frame(scenario_descriptor=c("Cover Crop, Crop Rotation",  #KBS T3
                                                  "Redu Fert 5%, Crop Rotation",
                                                  "Redu Fert 15%, Crop Rotation",
                                                  "Redu Fert 25%, Crop Rotation",
                                                  "Redu Fert 35%, Crop Rotation",
                                                  "Rmv Resid 50%, Crop Rotation",
                                                  "Rmv Resid 25%, Crop Rotation",
                                                  "Rmv Resid 0%, Crop Rotation",  #KBS T1
                                                  "Rmv Resid 50%, No Till, Crop Rotation",
                                                  "Rmv Resid 25%, No Till, Crop Rotation",
                                                  "Rmv Resid 0%, No Till, Crop Rotation", #KBS T2
                                                  "Biochar 19 Mgha, Crop Rotation",
                                                  "Biochar 38 Mgha, Crop Rotation",
                                                  "Biochar 57 Mgha, Crop Rotation",
                                                  "Biochar 76 Mgha, Crop Rotation",
                                                  "Biochar 96 Mgha, Crop Rotation"),
                            scenario_abbrev=c("CC-CR",
                                              "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                                              "RR50-CR","RR25-CR","RR00-CR",
                                              "RR50-NT-CR","RR25-NT-CR","RR00-NT-CR",
                                              "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                                              "CC-CR",
                                              "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                                              "RR50-CR","RR25-CR","RR00-CR",
                                              "RR50-NT-CR","RR25-NT-CR","RR00-NT-CR",
                                              "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                                              "CC-CR",
                                              "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                                              "RR50-CR","RR25-CR","RR00-CR",
                                              "RR50-NT-CR","RR25-NT-CR","RR00-NT-CR",
                                              "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                                              "CC-CR",
                                              "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                                              "RR50-CR","RR25-CR","RR00-CR",
                                              "RR50-NT-CR","RR25-NT-CR","RR00-NT-CR",
                                              "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                                              "CC-CR",
                                              "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                                              "RR50-CR","RR25-CR","RR00-CR",
                                              "RR50-NT-CR","RR25-NT-CR","RR00-NT-CR",
                                              "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR"),
                            scenario_name=c("1_3",
                                            "1_41","1_42","1_43","1_44",
                                            "1_51","1_52","1_53","1_54","1_55","1_56",
                                            "1_61","1_62","1_63","1_64","1_65",
                                            "2_3",
                                            "2_41","2_42","2_43","2_44",
                                            "2_51","2_52","2_53","2_54","2_55","2_56",
                                            "2_61","2_62","2_63","2_64","2_65",
                                            "3_3",
                                            "3_41","3_42","3_43","3_44",
                                            "3_51","3_52","3_53","3_54","3_55","3_56",
                                            "3_61","3_62","3_63","3_64","3_65",
                                            "4_3",
                                            "4_41","4_42","4_43","4_44",
                                            "4_51","4_52","4_53","4_54","4_55","4_56",
                                            "4_61","4_62","4_63","4_64","4_65",
                                            "5_3",
                                            "5_41","5_42","5_43","5_44",
                                            "5_51","5_52","5_53","5_54","5_55","5_56",
                                            "5_61","5_62","5_63","5_64","5_65"),
                            climate_scenario_num=c(1,1,1,1,1,1,1,1,1,1,
                                                   1,1,1,1,1,1,
                                                   2,2,2,2,2,2,2,2,2,2,
                                                   2,2,2,2,2,2,
                                                   3,3,3,3,3,3,3,3,3,3,
                                                   3,3,3,3,3,3,
                                                   4,4,4,4,4,4,4,4,4,4,
                                                   4,4,4,4,4,4,
                                                   5,5,5,5,5,5,5,5,5,5,
                                                   5,5,5,5,5,5),
                            mgmt_scenario_grp=c(3,4,4,4,4,
                                                5,5,5,5,5,5,
                                                6,6,6,6,6,
                                                3,4,4,4,4,
                                                5,5,5,5,5,5,
                                                6,6,6,6,6,
                                                3,4,4,4,4,
                                                5,5,5,5,5,5,
                                                6,6,6,6,6,
                                                3,4,4,4,4,
                                                5,5,5,5,5,5,
                                                6,6,6,6,6,
                                                3,4,4,4,4,
                                                5,5,5,5,5,5,
                                                6,6,6,6,6),
                            mgmt_scenario_opt=c("",1,2,3,4,
                                                1,2,3,4,5,6,
                                                1,2,3,4,5,
                                                "",1,2,3,4,
                                                1,2,3,4,5,6,
                                                1,2,3,4,5,
                                                "",1,2,3,4,
                                                1,2,3,4,5,6,
                                                1,2,3,4,5,
                                                "",1,2,3,4,
                                                1,2,3,4,5,6,
                                                1,2,3,4,5,
                                                "",1,2,3,4,
                                                1,2,3,4,5,6,
                                                1,2,3,4,5),
                            climate_esm=c("Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL"),
                            climate_esm_scenario=c("Baseline","Baseline","Baseline","Baseline",
                                                   "Baseline","Baseline","Baseline","Baseline",
                                                   "Baseline","Baseline","Baseline","Baseline",
                                                   "Baseline","Baseline","Baseline","Baseline",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5"),
                            climate_desc=c("Baseline","Baseline","Baseline","Baseline",
                                           "Baseline","Baseline","Baseline","Baseline",
                                           "Baseline","Baseline","Baseline","Baseline",
                                           "Baseline","Baseline","Baseline","Baseline",
                                           "GFDL_Low","GFDL_Low","GFDL_Low","GFDL_Low",
                                           "GFDL_Low","GFDL_Low","GFDL_Low","GFDL_Low",
                                           "GFDL_Low","GFDL_Low","GFDL_Low","GFDL_Low",
                                           "GFDL_Low","GFDL_Low","GFDL_Low","GFDL_Low",
                                           "GFDL_High","GFDL_High","GFDL_High","GFDL_High",
                                           "GFDL_High","GFDL_High","GFDL_High","GFDL_High",
                                           "GFDL_High","GFDL_High","GFDL_High","GFDL_High",
                                           "GFDL_High","GFDL_High","GFDL_High","GFDL_High",
                                           "UKESM_Low","UKESM_Low","UKESM_Low","UKESM_Low",
                                           "UKESM_Low","UKESM_Low","UKESM_Low","UKESM_Low",
                                           "UKESM_Low","UKESM_Low","UKESM_Low","UKESM_Low",
                                           "UKESM_Low","UKESM_Low","UKESM_Low","UKESM_Low",
                                           "UKESM_High","UKESM_High","UKESM_High","UKESM_High",
                                           "UKESM_High","UKESM_High","UKESM_High","UKESM_High",
                                           "UKESM_High","UKESM_High","UKESM_High","UKESM_High",
                                           "UKESM_High","UKESM_High","UKESM_High","UKESM_High")
  )
  
scenario_abbrev <- 
  if_else(mgmt_scenario_num=="3","CC-CR",
  if_else(mgmt_scenario_num=="41","RF05-CR",
  if_else(mgmt_scenario_num=="42","RF15-CR",
  if_else(mgmt_scenario_num=="43","RF25-CR",
  if_else(mgmt_scenario_num=="44","RF35-CR",
  if_else(mgmt_scenario_num=="51","RR50-CR",
  if_else(mgmt_scenario_num=="52","RR25-CR",
  if_else(mgmt_scenario_num=="53","RR00-CR",
  if_else(mgmt_scenario_num=="54","RR50-NT-CR",
  if_else(mgmt_scenario_num=="55","RR25-NT-CR",
  if_else(mgmt_scenario_num=="56","RR00-NT-CR",
  if_else(mgmt_scenario_num=="61","BC19-CR",
  if_else(mgmt_scenario_num=="62","BC38-CR",
  if_else(mgmt_scenario_num=="63","BC57-CR",
  if_else(mgmt_scenario_num=="64","BC76-CR",
  if_else(mgmt_scenario_num=="65","BC96-CR",
          "Missing Descriptor"
          ))))))))))))))))

scenario_descriptor <- 
  if_else(mgmt_scenario_num=="3","Cover Crop, Crop Rotation", #KBS T3
  if_else(mgmt_scenario_num=="41","Redu Fert 5%, Crop Rotation",
  if_else(mgmt_scenario_num=="42","Redu Fert 15%, Crop Rotation",
  if_else(mgmt_scenario_num=="43","Redu Fert 25%, Crop Rotation",
  if_else(mgmt_scenario_num=="44","Redu Fert 35%, Crop Rotation",
  if_else(mgmt_scenario_num=="51","Rmv Resid 50%, Crop Rotation",
  if_else(mgmt_scenario_num=="52","Rmv Resid 25%, Crop Rotation",
  if_else(mgmt_scenario_num=="53","Rmv Resid 0%, Crop Rotation", #KBS T1
  if_else(mgmt_scenario_num=="54","Rmv Resid 50%, No Till, Crop Rotation",
  if_else(mgmt_scenario_num=="55","Rmv Resid 25%, No Till, Crop Rotation",
  if_else(mgmt_scenario_num=="56","Rmv Resid 0%, No Till, Crop Rotation", #KBS T2
  if_else(mgmt_scenario_num=="61","Biochar 19 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="62","Biochar 38 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="63","Biochar 57 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="64","Biochar 76 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="65","Biochar 96 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="7","Continuous Crop",
  if_else(mgmt_scenario_num=="8","No Till, Cover Crop, Crop Rotation",
          "Missing Descriptor"
          ))))))))))))))))))

climate_scenario_descriptor <- 
  if_else(clim_scenario_num=="1","Baseline",
  if_else(clim_scenario_num=="2","GFDL_ESM4 Low",
  if_else(clim_scenario_num=="3","GFDL_ESM4 High",
  if_else(clim_scenario_num=="4","UKESM1-0-LL Low",
  if_else(clim_scenario_num=="5","UKESM1-0-LL High",
          "Missing Descriptor")))))
  
scenario_descriptor_full <- paste0(scenario_descriptor, "; ",climate_scenario_descriptor)

#create results folder if it doesn't already exist
results_path <- paste0(site_name,"_results_",end_fut_period_year,"/")
if(!dir.exists(results_path)) dir.create(results_path)

write.table(scenario_df,file=paste0(results_path,"Scenario_table.csv"),
            append=FALSE,col.names=TRUE,row.names=FALSE,sep=",")

site_id <- 0
elevation_m = 288
land_conversion_year <- 1850
experiment_year_range <- experiment_start_year:experiment_end_year
experiment_start_date <- "1989-01-01"
experiment_end_date <- "2021-12-31"
end_exp_period_year <- 2021
year_range_2100=experiment_start_year:end_fut_period_year

depth_m <- 0.25
equil_C_input <- if_else(mgmt_scenario_grp==3, 325.0, #305.00 #244.21 #210.84 # g C/m^2 annually
                 if_else(mgmt_scenario_num %in% c(54,55,56), 385, 300
                 ))
surface_C_init <- 60 # Mg C ha-1

control_treatment <- "T8"
control_treatment_num <- 8
#calib_mgmt_scenario_grps <- c(1:3)

treatment <- if_else(mgmt_scenario_num==3, "T3",
             if_else(mgmt_scenario_grp==4, "T1",
             if_else(mgmt_scenario_grp==5 &
                       mgmt_scenario_opt<=3, "T1",
             if_else(mgmt_scenario_grp==5, "T2",
             if_else(mgmt_scenario_grp==6, "T1",
             "Error")))))

treatment_num <- if_else(mgmt_scenario_num==3, 3,
             if_else(mgmt_scenario_grp==4, 1,
             if_else(mgmt_scenario_grp==5 &
                       mgmt_scenario_opt<=3, 1,
             if_else(mgmt_scenario_grp==5, 2,
             if_else(mgmt_scenario_grp==6, 1,
             0)))))

# soil_temp_bias <- if_else(mgmt_scenario_num==3, 4.5,
#              if_else(mgmt_scenario_grp==4, 5.0,
#              if_else(mgmt_scenario_grp==5 &
#                        mgmt_scenario_opt<=3, 5.0,
#              if_else(mgmt_scenario_grp==5, 4.5,
#              if_else(mgmt_scenario_grp==6, 5.0,
#              0)))))
# 
# soil_moist_bias <- if_else(mgmt_scenario_num==3, 0,
#              if_else(mgmt_scenario_grp==4, 2.0,
#              if_else(mgmt_scenario_grp==5 &
#                        mgmt_scenario_opt<=3, 2.0,
#              if_else(mgmt_scenario_grp==5, 0,
#              if_else(mgmt_scenario_grp==6, 4.0,
#              0)))))

# soil_temp_bias <- if_else(mgmt_scenario_num==1, 5.0,
#                   if_else(mgmt_scenario_num==2, 4.5,
#                   if_else(mgmt_scenario_num==3, 4.5,
#                   if_else(mgmt_scenario_grp==4, 5.0,
#                   if_else(mgmt_scenario_grp==5, 5.0,
#                   if_else(mgmt_scenario_grp==6, 5.0,
#                   0))))))
# soil_moist_bias <- if_else(mgmt_scenario_num==1, 4.0,
#                    if_else(mgmt_scenario_num==2, 0,
#                    if_else(mgmt_scenario_num==3, 0,
#                    if_else(mgmt_scenario_grp==4, 2.0,
#                    if_else(mgmt_scenario_grp==5, 2.0,
#                    if_else(mgmt_scenario_grp==6, 4.0,
#                    0))))))
covercrop_aftercorn <- "Oats"
covercrop_afterwheat <- "Red Clover"
covercrop_aftercorn_APSIM <- "Wintaroo"
covercrop_afterwheat_APSIM <- "Colenso"
covercrop_aftercorn_Daycent <- "OAT1"
covercrop_afterwheat_Daycent <- "CLVC"

hist_path <- paste0("Data/",site_name,"/Historical Land Use and Yields/")
hist_filename <- "MI-Kalamazoo County historical yields and C input.xlsx"
fut_filename <- "MI-Kalamazoo County future yields and C input.xlsx"
obs_yield_filename <- "51-agronomic+yields+annual+crops+1656512632.csv"
obs_soiltext_filename <- "699-soil+texture+of+surface+soils+1737816700.csv"
obs_bd_filename <- "71-soil+bulk+density+surface+1656513020.csv"
obs_C_filename <- "Soil Total Carbon and Nitrogen - Surface.csv"
obs_Cdeep_filename <- "164-soil+total+carbon+and+nitrogen+by+depth+deep+cores+1656512927.csv"
obs_BDdeep_filename <- "308-soil+bulk+density+by+depth+deep+cores+1656513026.csv"
obs_soiltemp_filename <- "167-soil+temperature+with+trace+gas+sampling+1658515465.csv"
obs_soilmoist_filename <- "157-soil+moisture+with+trace+gas+sampling+1656513014.csv"
obs_ghg_filename <- "28-n2o+ch4+co2+fluxes+via+static+chambers+1656512661.csv"
obs_fert_filename <- "RFertilizer.csv"
obs_mb_filename <- "25-soil+microbial+biomass+via+chloroform+fumigation+1656513120.csv"
obs_plant_cn_filename <- "73-tissue+carbon+and+nitrogen+1667424583.csv"
obs_biomass_filename <- "39-annual+crops+and+alfalfa+biomass+1667489393.csv"

# Color palettes are chosen to accommodate colorblindness, per
# https://davidmathlogic.com/colorblind/#%23000000-%230072B2-%23009E73-%2356B4E9-%23999999-%23CC79A7-%23D55E00-%23E69F00-%23F0E442-%23882255-%23332288-%230B6329

# 9-color palette with grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]green, [4]light blue, [5]lightest grey,
#[6]pink, [7]red, [8]orange, [9]yellow
cbPalette9 <- c("#000000","#0072B2","#009E73","#56B4E9","#999999",
                "#CC79A7","#D55E00","#E69F00","#F0E442")
# 12-color palette with grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]mint green, [4]light blue, [5]lightest grey,
#[6]pink, [7]red, [8]orange, [9]yellow, [10]mauve, 
#[11]royal blue, [12]forest green
cbPalette12 <- c("#000000","#0072B2","#009E73","#56B4E9","#999999",
                 "#CC79A7","#D55E00","#E69F00","#F0E442","#882255",
                 "#332288","#0B6329"
)
# 20-color palette with more grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]mint green, [4]light blue, [5]lightest grey,
#[6]pink, [7]red, [8]orange, [9]yellow, [10]mauve, 
#[11]royal blue, [12]forest green, [13]dark grey, [14]med grey, [15]light grey,
#[16]bright blue, [17]cotton candy, [18]lime green, [19]brick red, #[20]dark mauve
cbPalette20 <- c("#000000","#0072B2","#009E73","#56B4E9","#999999",
                 "#CC79A7","#D55E00","#E69F00","#F0E442","#882255",
                 "#332288","#0B6329","#333333","#555555","#777777",
                 "#4948DD","#D832BC","#71C4A6","#5F2A01","#48122D"
)

APSIM_color <- cbPalette9[8]
Daycent_color <- cbPalette9[2]
Millennial_color <- cbPalette9[6]
RothC_color <- cbPalette9[3]
Observed_color <- cbPalette9[1]
Historical_color <- cbPalette9[4]
Fertilizer_color <- cbPalette9[7]

N2O_color <- cbPalette12[8]
NO3_color <- cbPalette12[6]
SoilT_color <- cbPalette12[7]
SW_20cm_color <- cbPalette12[2]
SW_25cm_color <- cbPalette12[2]
SW_40cm_color <- cbPalette12[3]
SW_60cm_color <- cbPalette12[4]
WFPS_20cm_color <- cbPalette12[2]
WFPS_40cm_color <- cbPalette12[3]
WFPS_60cm_color <- cbPalette12[4]
WFPS_2cm_color <- cbPalette12[11]
WFPS_5cm_color <- cbPalette12[12]
WFPS_10cm_color <- cbPalette12[2]
CH4_color <- cbPalette12[10]

BiomC_20cm_color <- cbPalette20[10]
BiomC_25cm_color <- cbPalette20[10]
#BiomC_40cm_color <- cbPalette20[]
#BiomC_60cm_color <- cbPalette20[]
BiomN_20cm_color <- cbPalette20[6]
BiomN_25cm_color <- cbPalette20[6]
#BiomN_40cm_color <- cbPalette20[]
#BiomN_60cm_color <- cbPalette20[]
HumC_20cm_color <- cbPalette20[19]
HumC_25cm_color <- cbPalette20[19]
#HumC_40cm_color <- cbPalette20[]
#HumC_60cm_color <- cbPalette20[]
HumN_20cm_color <- cbPalette20[8]
HumN_25cm_color <- cbPalette20[8]
#HumN_40cm_color <- cbPalette20[]
#HumN_60cm_color <- cbPalette20[]
CtoBiom_25cm_color <- cbPalette20[5]
CtoHum_25cm_color <- cbPalette20[15]
CBtoHum_25cm_color <- cbPalette20[14]
TotalSOC_20cm_color <- cbPalette20[1]
TotalSOC_25cm_color <- cbPalette20[1]
Cin_25cm_color <- cbPalette20[15]

GFDL_L_color <- cbPalette12[4]
GFDL_H_color <- cbPalette12[11]
UKESM_L_color <- cbPalette12[8]
UKESM_H_color <- cbPalette12[7]

BC19_color <- cbPalette20[1]
BC38_color <- cbPalette20[13]
BC57_color <- cbPalette20[14]
BC76_color <- cbPalette20[15]
BC96_color <- cbPalette20[5]
CC_color <- cbPalette20[6]
CC_NT_color <- cbPalette20[10]
CN_color <- cbPalette20[16]
CR_color <- cbPalette20[17]
NT_color <- cbPalette20[20]
RF05_color <- cbPalette20[19]
RF15_color <- cbPalette20[9]
RF25_color <- cbPalette20[8]
RF35_color <- cbPalette20[7]
RR00_color <- cbPalette20[2]
RR00_NT_color <- cbPalette20[3]
RR25_color <- cbPalette20[4]
RR25_NT_color <- cbPalette20[11]
RR50_color <- cbPalette20[12]
RR50_NT_color <- cbPalette20[18]



###########################################################
#################### observational data ###################
###########################################################


####################### historical averages #######################

Hist_raw <- read_xlsx(paste0(hist_path,"MI-Kalamazoo County historical yields and C input.xlsx"),
                      sheet="Kalamazoo County-Calcs",range="A2:AW171")
HistY_Mgha <- Hist_raw[Hist_raw$Year<=1987,c(1,8:10)] %>%
  mutate(year=Year,
         maize_yield_mgha=`Corn g/m^2...8`/100,
         soybean_yield_mgha=`Soybean g/m^2...9`/100,
         wheat_yield_mgha=`Wheat g/m^2...10`/100)



######################## measured observations #######################

############ surface samples 

## Soil Texture
ObsSoilText_raw <- read.csv(paste0(obs_path,obs_soiltext_filename),
                            skip=26)

ObsSoilText_mean <- ObsSoilText_raw %>%
  group_by(treatment) %>%
  summarize(mean_clay=round(mean(clay_percent),2),
            sd_clay=round(sd(clay_percent),2),
            mean_sand=round(mean(sand_percent),2),
            sd_sand=round(sd(sand_percent),2),
            mean_silt=round(mean(silt_percent),2),
            sd_silt=round(sd(silt_percent),2)) %>%
  mutate(treatment=str_trim(treatment))

ObsSoilText_mean <- ObsSoilText_mean %>%
  bind_rows(summarise(ObsSoilText_mean, 
                      treatment = "T3", 
                      across(c(mean_clay, sd_clay, mean_sand, sd_sand, mean_silt, sd_silt), 
                             \(x) mean(x, na.rm = TRUE))))
ObsSoilText_mean <- ObsSoilText_mean %>%
  bind_rows(summarise(ObsSoilText_mean, 
                      treatment = "T8", 
                      across(c(mean_clay, sd_clay, mean_sand, sd_sand, mean_silt, sd_silt), 
                             \(x) mean(x, na.rm = TRUE))))


## Bulk density
ObsBD_raw <- read.csv(paste0(obs_path,obs_bd_filename),
                  skip=23) %>%
  mutate(date=as.Date(Date, format="%m/%d/%Y"),
         year=year(date))

ObsBD_mean <- ObsBD_raw %>%
  group_by(year,Treatment,Replicate) %>%
  summarize(mean_BD=round(mean(Bulk_density),2),
            sd_BD=round(sd(Bulk_density),2),
            Treatment=str_trim(Treatment))

ObsBD <- ObsBD_mean[ObsBD_mean$Treatment==treatment,] %>%
  group_by(year,Treatment) %>%
  summarize(mean_BD=round(mean(mean_BD),2))

ObsBD_control <- ObsBD_mean[ObsBD_mean$Treatment==control_treatment,] %>%
  group_by(year,Treatment) %>%
  summarize(mean_BD=round(mean(mean_BD),2))


## C percent
ObsC_pct_raw <- read.csv(paste0(obs_path,obs_C_filename),
                     skip=27) %>%
  select(1:11) %>% # exclude extraneous columns
  mutate(date=as.Date(sample_date, format="%m/%d/%Y"),
         month=month(date))

ObsC_pct <- ObsC_pct_raw[ObsC_pct_raw$month<7,]

ObsC_mean <- ObsC_pct %>%
  group_by(year,treatment) %>%
  summarize(mean_Cpct=round(mean(mean_c),2),
            sd_Cpct=round(sd(mean_c),2))

##  C stock

ObsC_Mgha_all <- ObsC_pct %>%
  mutate(cstock_byrep=mean_c*ObsBD$mean_BD*25) %>%
  group_by(year,treatment) %>%
  summarize(mean_cpct=round(mean(mean_c,na.rm=T),2),
            cstock=mean(cstock_byrep,na.rm=T),
            sd_cstock=sd(cstock_byrep,na.rm=T))


ObsC_Mgha <- ObsC_pct[ObsC_pct$treatment==treatment_num,] %>%
  mutate(cstock_byrep=mean_c*ObsBD$mean_BD*25) %>%
  group_by(year) %>%
  summarize(mean_cpct=round(mean(mean_c,na.rm=T),2),
            cstock=mean(cstock_byrep,na.rm=T),
            sd_cstock=sd(cstock_byrep,na.rm=T))

ObsC_outliers <- boxplot(ObsC_Mgha$cstock, plot=FALSE)$out
ObsC_Mgha_noout <- ObsC_Mgha[-which(ObsC_Mgha$cstock %in% ObsC_outliers),]

ObsC_control_Mgha <- ObsC_pct[ObsC_pct$treatment==control_treatment_num,] %>%
  mutate(cstock_byrep=mean_c*ObsBD$mean_BD*25) %>%
  group_by(year) %>%
  summarize(mean_cpct=round(mean(mean_c,na.rm=T),2),
            cstock=mean(cstock_byrep,na.rm=T),
            sd_cstock=sd(cstock_byrep,na.rm=T))
initC <- mean(ObsC_control_Mgha$cstock)

# initC is being ignored and 60 Mg C ha-1 forced in due to calibrating the
# start C to RothC and evidence of decreasing C in the control plot during the experimental
# period. 
ObsC_Mgha <- rbind(c(land_conversion_year, NA, 60, NA),ObsC_Mgha)

ObsC_Mgha_mean_5yr <- mean(ObsC_Mgha$mean_cpct[1:5])
### include SOC outliers or land conversion year in stats
ObsCfit <- lm(cstock ~ year, data = ObsC_Mgha[ObsC_Mgha$year != land_conversion_year,])
ObsCfit_coef <- coef(ObsCfit)
ObsCfit_r2 <- round(summary(ObsCfit)$r.squared,2)
### don't include SOC outliers or land conversion year in stats
ObsCfit_noout <- lm(cstock ~ year, data = ObsC_Mgha[!(ObsC_Mgha$cstock %in% ObsC_outliers) &
                                                ObsC_Mgha$year != land_conversion_year,])
ObsCfit_coef_noout <- coef(ObsCfit_noout)
ObsCfit_r2_noout <- round(summary(ObsCfit_noout)$r.squared,2)

## Yield
ObsYield_raw <- read.csv(paste0(obs_path,obs_yield_filename),skip=31)
ObsYield <- ObsYield_raw[ObsYield_raw$Treatment==treatment,] %>%
  mutate(year=Year,
         crop=if_else(Crop=="Zea mays L. (*)", "Maize",
              if_else(Crop=="Glycine max L. (*)", "Soybean",
              if_else(Crop=="Triticum aestivum L. (*)", "Wheat",
                      Crop)))
         ) %>%
  group_by(year,Treatment,crop) %>%
  summarize(mean_yield_kgha=round(mean(crop_only_yield_kg_ha,na.rm=T),0),
            sd_yield_kgha=sd(crop_only_yield_kg_ha,na.rm=T))
  
ObsYield$mean_yield <- ObsYield$mean_yield_kgha/1000
ObsYield$sd_yield <- ObsYield$sd_yield_kgha/1000
ObsYield$mean_yield_gm2 <- ObsYield$mean_yield*100
ObsYield$sd_yield_gm2 <- ObsYield$sd_yield*100

ObsMYfit <- lm(mean_yield ~ year, data = ObsYield[ObsYield$crop=="Maize",])
ObsMYfit_coef <- coef(ObsMYfit)
ObsMYfit_r2 <- round(summary(ObsMYfit)$r.squared,2)

ObsSYfit <- lm(mean_yield ~ year, data = ObsYield[ObsYield$crop=="Soybean",])
ObsSYfit_coef <- coef(ObsSYfit)
ObsSYfit_r2 <- round(summary(ObsSYfit)$r.squared,2)

ObsWYfit <- lm(mean_yield ~ year, data = ObsYield[ObsYield$crop=="Wheat",])
ObsWYfit_coef <- coef(ObsWYfit)
ObsWYfit_r2 <- round(summary(ObsWYfit)$r.squared,2)


## Soil temp
ObsTemp_raw <- read.csv(paste0(obs_path,obs_soiltemp_filename),
                    skip=21)%>%
  mutate(date=as.Date(date, format="%m/%d/%Y"))
ObsTemp_all <- ObsTemp_raw[ObsTemp_raw$year >= 1999,
                       c("date","year","treatment","soil_temperature","replicate")] %>%
  group_by(date,year,treatment) %>%
  summarize(soil_temperature=round(mean(soil_temperature,na.rm=T),1))
ObsTemp <- ObsTemp_raw[ObsTemp_raw$treatment==treatment & ObsTemp_raw$year >= 1999,
                       c("date","year","soil_temperature","replicate")] %>%
  group_by(date,year) %>%
  summarize(soil_temperature=round(mean(soil_temperature,na.rm=T),1))

ObsTfit <- lm(soil_temperature ~ date, data = ObsTemp)
ObsTfit_coef <- coef(ObsTfit)
ObsTfit_r2 <- round(summary(ObsTfit)$r.squared,2)
ObsTfit_slope_byday <- ObsTfit_coef[2]
ObsTfit_slope_byyear <- ObsTfit_slope_byday*365

ObsTemp_range <- range(ObsTemp$soil_temperature,na.rm=T)

## Soil gases - all in g ha-1 d-1
ObsGas_raw <- read.csv(paste0(obs_path,obs_ghg_filename),
                   skip=36) %>%
  mutate(date=as.Date(Sample_Date, format="%m/%d/%Y"),
         year=Year)
ObsGas_mean <- ObsGas_raw %>%
  group_by(date,year,Treatment) %>%
  summarize(CH4_C=round(mean(CH4_C),2),
            CO2_C=round(mean(CO2_C),2),
            N2O_N=round(mean(N2O_N),2))
ObsGas_all <- ObsGas_mean[,c("date","year","Treatment","CH4_C","CO2_C","N2O_N")]
ObsGas <- ObsGas_mean[ObsGas_mean$Treatment==treatment,c("date","year","CH4_C","CO2_C",
                                                  "N2O_N")]

ObsN2Ofit <- lm(N2O_N ~ date, data = ObsGas)
ObsN2Ofit_coef <- coef(ObsN2Ofit)
ObsN2Ofit_r2 <- round(summary(ObsN2Ofit)$r.squared,2)

ObsCH4fit <- lm(CH4_C ~ date, data = ObsGas)
ObsCH4fit_coef <- coef(ObsCH4fit)
ObsCH4fit_r2 <- round(summary(ObsCH4fit)$r.squared,2)

ObsGas_N2O_calib <- ObsGas[!is.na(ObsGas$N2O_N),] %>%
  group_by(year) %>%
  summarize(tot_N2O_ghayr=sum(N2O_N))

ObsGas_CH4_calib <- ObsGas[!is.na(ObsGas$CH4_C),] %>%
  group_by(year) %>%
  summarize(tot_CH4_ghayr=sum(CH4_C))

## Soil moisture
ObsGSM <- read.csv(paste0(obs_path,obs_soilmoist_filename),
                   skip=24) %>%
  mutate(date=as.Date(date, format="%m/%d/%Y"),
         year=year(date))
ObsVSM_mean <- left_join(ObsGSM[,c("date","moisture","treatment","replicate")],
                ObsBD_mean,
                by=c("treatment" = "Treatment","replicate" = "Replicate"),
                relationship="many-to-many") %>% #,
                #all=TRUE) %>%
  mutate(VSM=round(moisture*mean_BD,2)) %>%
  group_by(date,treatment) %>%
  summarize(mean_VSM=round(mean(VSM*100),0)) %>%
  mutate(year=year(date))
ObsVSM <- ObsVSM_mean[ObsVSM_mean$treatment==treatment,]

ObsMfit <- lm(mean_VSM ~ year, data = ObsVSM)
ObsMfit_coef <- coef(ObsMfit)
ObsMfit_r2 <- round(summary(ObsMfit)$r.squared,2)
ObsMfit_slope_byyear <- ObsMfit_coef[2]

## Microbial biomass
ObsMB_raw <- read.csv(paste0(obs_path,obs_mb_filename),
                  skip=68) %>%
  mutate(date=as.Date(date, format="%m/%d/%Y"),
         year=Year,
         )
ObsMB_mean <- ObsMB_raw %>%
  group_by(year,trt,date) %>%
  summarize(mean_MB_ugg=round(mean(cfibio_c,na.rm=T),0),
            sd_MB_ugg=sd(cfibio_c,na.rm=T))

# ObsMB <- ObsBD[,c("Treatment","mean_BD")] %>%
#   merge(ObsMB_mean[ObsMB_mean$trt==treatment,],
#         by.x = "Treatment",
#         by.y = "trt",
#         all=T) %>%
#   mutate(mean_MB_gm2=mean_MB_ugg * mean_BD / 100)

ObsMB_all <- ObsMB_mean %>%
  mutate(bd=ObsBD$mean_BD,
         mb_gm2=mean_MB_ugg*bd/100,
         mb_mgha=mb_gm2/100,
         sd_gm2=sd_MB_ugg*bd/100,
         sd_mgha=sd_gm2/100)

ObsMB <- ObsMB_all[ObsMB_all$trt==treatment,] 

ObsMBfit <- lm(mb_gm2 ~ year, data = ObsMB)
ObsMBfit_coef <- coef(ObsMBfit)
ObsMBfit_r2 <- round(summary(ObsMBfit)$r.squared,2)


## Plant tissue C and N content
ObsPltCN_raw <- read.csv(paste0(obs_path,obs_plant_cn_filename),
                         skip=24) %>%
  mutate(date=as.Date(sample_date, format="%m/%d/%Y"),
         year=year(date),
         Treatment=Trt)

###check for plant types
#unique(ObsPltCN_raw[ObsPltCN_raw$Trt %in% c("T1","T2","T3"),"species"])

ObsPltCN_mean <- ObsPltCN_raw %>%
  group_by(year,Treatment,species,type) %>%
  summarize(percent_C=round(mean(percent_C),2),
            percent_N=round(mean(percent_N),2)) %>%
  mutate(crop=if_else(species=="Zea mays L. (*)", "Maize",
                      if_else(species=="Glycine max L. (*)", "Soybean",
                              if_else(species=="Triticum aestivum L. (*)", "Wheat",
                                      species))),
         cn_ratio=percent_C/percent_N
  )

### "widen" the results to match format of Daycent data
ObsPltCN_wide <- pivot_wider(ObsPltCN_mean,
                             names_from = type,
                             values_from = c("percent_C","percent_N",
                                             "cn_ratio",)) %>%
  select(year,Treatment,species,crop,percent_C_SEED,percent_C_STOVER,
         percent_N_SEED,percent_N_STOVER,cn_ratio_SEED,cn_ratio_STOVER)

ObsGrainCN <- ObsPltCN_mean[ObsPltCN_mean$Treatment==treatment
                            & ObsPltCN_mean$type=="SEED",] %>%
  left_join(ObsYield[,!names(ObsYield)=="mean_yield"],
            by=c("year","Treatment","crop")) %>%
  mutate(grainC_gm2=percent_C/100*mean_yield_gm2,
         grainN_gm2=percent_N/100*mean_yield_gm2)


## biomass (to calculate C and N of stover)
ObsBiomass_raw <- read.csv(paste0(obs_path,obs_biomass_filename),
                           skip=29) %>%
  mutate(date=as.Date(Date, format="%m/%d/%Y"),
         year=Year)

ObsBiomass_mean <- ObsBiomass_raw %>%
  group_by(year,Treatment,Species,Fraction) %>%
  summarize(biomass_gm2=round(mean(Biomass),2)) %>%
  mutate(crop=if_else(Species=="Zea mays L. (*)", "Maize",
                      if_else(Species=="Glycine max L. (*)", "Soybean",
                              if_else(Species=="Triticum aestivum L. (*)", "Wheat",
                                      Species)))
  )

ObsBiomass <- ObsBiomass_mean[ObsBiomass_mean$Treatment==treatment,] 

### now "widen" the results to match format of Daycent data, add N data
ObsBiomass_wide <- pivot_wider(ObsBiomass, 
                             names_from = Fraction,
                             values_from = "biomass_gm2") %>%
  select(year,Treatment,Species,crop,SEED,WHOLE,STOVER) %>%
  mutate(STOVER=WHOLE-SEED)

ObsStoverCN <- ObsBiomass_wide[ObsBiomass_wide$Treatment==treatment,
                               !names(ObsBiomass_wide) %in% c("SEED","WHOLE")]  %>%
  left_join(ObsPltCN_wide[,c("year","Treatment","crop","percent_C_STOVER","percent_N_STOVER")],
            by=c("year","Treatment","crop")) %>%
  mutate(stoverC_gm2=percent_C_STOVER/100*STOVER,
         stoverN_gm2=percent_N_STOVER/100*STOVER)


############### deep core samples 

ObsBDdeep_raw <- read.csv(paste0(obs_path,obs_BDdeep_filename),
                  skip=32)

ObsBDdeep_mean <- ObsBDdeep_raw %>%
  group_by(year,treatment,section) %>%
  summarize(depth=round(mean(horizon_length),0),
    mean_BD=round(mean(gravel_free_bulk_density),2),
    sd_BD=round(sd(gravel_free_bulk_density),2))

ObsBDdeep <- ObsBDdeep_mean[ObsBDdeep_mean$treatment==treatment,]

### add full profile depths for each section
ObsBDdeep_sampledepths <- ObsBDdeep_mean %>%
  group_by(treatment) %>%
  summarize(sample_depth=sum(depth))


## deep core samples - C percent
ObsCdeep_pct_raw <- read.csv(paste0(obs_path,obs_Cdeep_filename),
                     skip=27)

ObsCdeep_pct_mean <- ObsCdeep_pct_raw %>%
  mutate(date=as.Date(sample_date, format="%m/%d/%Y"),
         year=year(date),
         section=depth) %>%
  group_by(year,treatment,section) %>%
  summarize(mean_C=round(mean(c_percent),4),
            sd_C=round(sd(c_percent),2))

ObsCdeep_pct <- ObsCdeep_pct_mean[ObsCdeep_pct_mean$treatment==treatment,]

### calculate C stock
#### join C and bulk density data; fill in NA bulk density data (in the Deep
#### layers) with the Middle value; calculate the stock for each group of
#### year/treatment/depth section; then calculate the % stock in each layer
#### (this last step was to accommodate the Millennium model, which was 
#### dropped from the ensemble).
ObsCBDdeep <- inner_join(ObsBDdeep_mean,
                       ObsCdeep_pct_mean,
                       by=c("year","treatment","section"))%>%
  group_by(treatment) %>%
  mutate(mean_BD_adj = if_else(is.na(mean_BD) & section=="Deep", 
                               mean_BD[section=="Middle"], 
                               mean_BD),
         cstock=mean_C*mean_BD_adj*depth,
         fraction_C=cstock/sum(cstock)) %>%
  ungroup

ObsCBDdeep_cstock <- ObsCBDdeep %>%
  group_by(year,treatment) %>%
  summarize(cstock=sum(cstock)) #%>%
 # mutate(top_25cm=cstock*0.63)


### calculate C stock and % to 23 cm (the deepest Surface layer between T1, T2, T3, and T8)
ObsCdeep_calcs <- ObsCBDdeep %>%
  group_by(treatment) %>%
  mutate(c_cm=cstock/depth,
         addtl_c=if_else(section=="Surface",0,
                 if_else(section=="Middle",c_cm*(23-depth[section=="Surface"]),
                 0)
                 ),
         cstock_23cm=if_else(section=="Surface",cstock[section=="Surface"]+addtl_c[section=="Middle"],0),
         addtl_depth=if_else(section=="Surface",0,
                     if_else(section=="Middle",(23-depth[section=="Surface"]),
                     0)
                     ),
         Cpct_23=if_else(section=="Surface",
                                (((mean_C*depth)+
                                (mean_C[section=="Middle"]*addtl_depth[section=="Middle"]))/(depth+addtl_depth[section=="Middle"])),
                                0)
         )

ObsCdeep_23cm <- ObsCdeep_calcs[ObsCdeep_calcs$section=="Surface",c("year","treatment","cstock_23cm","Cpct_23")]

### commented out, because it was for the Millennium model, which was dropped
### from the ensemble
#ObsCdeep_top23_pct <- cbind(ObsCdeep_23cm[,1:2],ObsCdeep_23cm$tot_C/ObsCBDdeep_cstock$cstock)

#### keep initial equilibrium C from control plot for year of land conversion and
#### the current treatment
# ObsCdeep_Mgha <- rbind(data.frame(year=land_conversion_year,
#                                   ObsCBDdeep_cstock[ObsCBDdeep_cstock$treatment==control_treatment,c("treatment","cstock")]),
#                        ObsCBDdeep_cstock[ObsCBDdeep_cstock$treatment==treatment,c("year","treatment","cstock")]) 
ObsCdeep_Mgha <- ObsCdeep_23cm[ObsCdeep_23cm$treatment %in% c(treatment,control_treatment),] %>%
  rename(cstock=cstock_23cm) %>%
  mutate(year = if_else(treatment == control_treatment, 1850, year)) %>%
  arrange(year)

##################################
# Add fertilizer for GHG reference
##################################

Fert_APSIM <- as.data.frame(read.csv(paste0(obs_path,obs_fert_filename)) %>%
  mutate(date=as.Date(date,format="%Y-%m-%d")))
Fert <- as.data.frame(read.csv(paste0(obs_path,obs_fert_filename)) %>%
  mutate(date=as.Date(date,format="%m/%d/%Y")))

#######################
# Bring in weather data
#######################

ObsWth <- read.csv(paste0(apsim_path,"/basic_wth_",clim_scenario_num,".csv"),
                   skip=2) %>%
  mutate(meant=round((maxt+mint)/2,1),
         date=as.Date(day-1, origin=paste0(as.character(year),"-01-01"),),
         source="Air"
  )

#**********************************************************************

# write calibration header file ---------------------------------------

# make separate file with column headers (empty table with NA row)
log_col_headers <- c("Date_time","Model",
                     "Climate_Scenario","Mgmt_Scenario","Scenario_Name",
                     "Scenario_Abbr",
                     "Maize_slope_1to1","Maize_yint_1to1","Maize_R2_1to1","Maize_RMSE_1to1","Maize_mNSE_1to1","Maize_pbias_1to1",
                     "Maize_diff",
                     "Soy_slope_1to1","Soy_yint_1to1","Soy_R2_1to1","Soy_RMSE_1to1","Soy_mNSE_1to1","Soy_pbias_1to1",
                     "Soy_diff",
                     "Wheat_slope_1to1","Wheat_yint_1to1","Wheat_R2_1to1","Wheat_RMSE_1to1","Wheat_mNSE_1to1","Wheat_pbias_1to1",
                     "Wheat_diff",
                     "SOC_slope_1to1","SOC_yint_1to1","SOC_R2_1to1","SOC_RMSE_1to1","SOC_mNSE_1to1","SOC_pbias_1to1",
                     "SOC_diff","SOC_diff_noout",
                     "Temp_slope_1to1","Temp_yint_1to1","Temp_R2_1to1","Temp_RMSE_1to1","Temp_mNSE_1to1","Temp_pbias_1to1",
                     "Temp_diff",
                     "Moist_slope_1to1","Moist_yint_1to1","Moist_R2_1to1","Moist_RMSE_1to1","Moist_mNSE_1to1","Moist_pbias_1to1",
                     "Moist_diff",
                     "N2O_slope_1to1","N2O_yint_1to1","N2O_R2_1to1","N2O_RMSE_1to1","N2O_mNSE_1to1","N2O_pbias_1to1",
                     "N2O_diff",
                     "CH4_slope_1to1","CH4_yint_1to1","CH4_R2_1to1","CH4_RMSE_1to1","CH4_mNSE_1to1","CH4_pbias_1to1",
                     "CH4_diff",
                     "MBio_slope_1to1","MBio_yint_1to1","MBio_R2_1to1","MBio_RMSE_1to1",
                     "MBio_diff",
                     "Cotton_slope_1to1","Cotton_yint_1to1","Cotton_R2_1to1","Cotton_RMSE_1to1",
                     "Cotton_diff",
                     "Sorghum_slope_1to1","Sorghum_yint_1to1","Sorghum_R2_1to1","Sorghum_RMSE_1to1",
                     "Sorghum_diff",
                     "Maize_cultivar","Soybean_cultivar","Wheat_cultivar",
                     "Cotton_cultivar","Sorghum_cultivar",
                     "Maize_slope_time","Maize_yint_time","Maize_R2_time","Maize_RMSE_time",
                     "Soy_slope_time","Soy_yint_time","Soy_R2_time","Soy_RMSE_time",
                     "Wheat_slope_time","Wheat_yint_time","Wheat_R2_time","Wheat_RMSE_time",
                     "SOC_slope_time","SOC_yint_time","SOC_R2_time","SOC_RMSE_time",
                     "SOC_slope_time_noout","SOC_yint_time_noout","SOC_R2_time_noout","SOC_RMSE_time_noout",
                     "Temp_slope_time","Temp_yint_time","Temp_R2_time","Temp_RMSE_time",
                     "Moist_slope_time","Moist_yint_time","Moist_R2_time","Moist_RMSE_time",
                     "N2O_slope_time","N2O_yint_time","N2O_R2_time","N2O_RMSE_time",
                     "CH4_slope_time","CH4_yint_time","CH4_R2_time","CH4_RMSE_time",
                     "MBio_slope_time","MBio_yint_time","MBio_R2_time","MBio_RMSE_time",
                     "Cotton_slope_time","Cotton_yint_time","Cotton_R2_time","Cotton_RMSE_time",
                     "Sorghum_slope_time","Sorghum_yint_time","Sorghum_R2_time","Sorghum_RMSE_time"
)
dummy<-data.frame(matrix(ncol=length(log_col_headers)))
colnames(dummy) <- log_col_headers

write.table(dummy,file=paste0(results_path,"Calibration_log_columns.csv"),
            append=FALSE,col.names=TRUE,row.names=FALSE,sep=",")

# Log results -------------------------------------------------------------

if(clim_scenario_num == 1 & mgmt_scenario_num %in% calib_mgmt_nums) {
  # add this run's results to model log file and file collecting all final
  # model runs
  calib_log_tab <- cbind(as.character(Sys.time()),"Observed",
                         clim_scenario_num,mgmt_scenario_num, scenario_name,
                         scenario_abbrev,
                         NA, NA, NA, NA, NA, NA, # Corn 1 to 1
                         NA, # diff (obs - mod)
                         NA, NA, NA, NA, NA, NA, # Soybeans 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, NA, NA, # Wheat 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, NA, NA, # SOC 1 to 1
                         NA, NA, # diffs (w/ and w/o outliers)
                         NA, NA, NA, NA, NA, NA, # Temperature 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, NA, NA, # Moisture 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, NA, NA, # N2O 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, NA, NA, # CH4 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, # M Bio 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, # Cotton 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, # Sorghum 1 to 1
                         NA, # diff
                         NA, NA, NA, NA, NA, # M,S,W,C,S cultivars
                         ObsMYfit_coef[2], NA, ObsMYfit_r2, NA, # Corn time series
                         ObsSYfit_coef[2], NA, ObsSYfit_r2, NA, # Soybeans time series
                         ObsWYfit_coef[2], NA, ObsWYfit_r2, NA, # Wheat time series
                         ObsCfit_coef[2], NA, ObsCfit_r2, NA, # SOC time series
                         ObsCfit_coef_noout[2], NA, ObsCfit_r2_noout, NA, # SOC time series without outliers
                         ObsTfit_coef[2], NA, ObsTfit_r2, NA, # Temperature time series
                         ObsMfit_coef[2], NA, ObsMfit_r2, NA, # Moisture time series
                         ObsN2Ofit_coef[2], NA, ObsN2Ofit_r2, NA, # N2O time series
                         ObsCH4fit_coef[2], NA, ObsCH4fit_r2, NA, # CH4 time series
                         ObsMBfit_coef[2], NA, ObsMBfit_r2, NA, # M Bio time series
                         NA, NA, NA, NA, # Cotton time series
                         NA, NA, NA, NA # Sorghum time series
  )
  
  source("p_Edit_calib_file.R")
  p_Edit_calib_file(calib_log_tab,"Observed",scenario_name)
}

}) # end suppressMessages

