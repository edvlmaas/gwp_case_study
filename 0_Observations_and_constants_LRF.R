#######################################
# File: 0_Observations_and_constants_LRF.R
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

  #**********************************************************************
# Constants ---------------------------------------------------------------

  scenario_df <- data.frame(scenario_descriptor=c("Cover Crop, Crop Rotation", #LRF CRSct
                                                  "Redu Fert 5%, Crop Rotation",
                                                  "Redu Fert 15%, Crop Rotation",
                                                  "Redu Fert 25%, Crop Rotation",
                                                  "Redu Fert 35%, Crop Rotation",
                                                  "Rmv Resid 50%, Crop Rotation",
                                                  "Rmv Resid 25%, Crop Rotation",
                                                  "Rmv Resid 0%, Crop Rotation", #LRF CSct
                                                  "Rmv Resid 50%, No Till, Crop Rotation",
                                                  "Rmv Resid 25%, No Till, Crop Rotation",
                                                  "Rmv Resid 0%, No Till, Crop Rotation", #LRF CSnt
                                                  "Biochar 19 Mgha, Crop Rotation",
                                                  "Biochar 38 Mgha, Crop Rotation",
                                                  "Biochar 57 Mgha, Crop Rotation",
                                                  "Biochar 76 Mgha, Crop Rotation",
                                                  "Biochar 96 Mgha, Crop Rotation",
                                                  "Continuous Crop", #LRF CCct
                                                  "No Till, Cover Crop, Crop Rotation"), #LRF CRSnt
                            climate_scenario_num=c(1,1,1,1,1,1,1,1,1,1,
                                                   1,1,1,1,1,1,1,1,
                                                   2,2,2,2,2,2,2,2,2,2,
                                                   2,2,2,2,2,2,2,2,
                                                   3,3,3,3,3,3,3,3,3,3,
                                                   3,3,3,3,3,3,3,3,
                                                   4,4,4,4,4,4,4,4,4,4,
                                                   4,4,4,4,4,4,4,4,
                                                   5,5,5,5,5,5,5,5,5,5,
                                                   5,5,5,5,5,5,5,5),
                            mgmt_scenario_grp=c(3,4,4,4,4,
                                                5,5,5,5,5,5,
                                                6,6,6,6,6,
                                                7,8),
                            mgmt_scenario_opt=c("",1,2,3,4,
                                                1,2,3,1,2,3,
                                                1,2,3,4,5,
                                                "",""),
                            scenario_name=c("1_3",
                                            "1_41","1_42","1_43","1_44",
                                            "1_51","1_52","1_53","1_54","1_55","1_56",
                                            "1_61","1_62","1_63","1_64","1_65",
                                            "1_7","1_8",
                                            "2_3",
                                            "2_41","2_42","2_43","2_44",
                                            "2_51","2_52","2_53","2_54","2_55","2_56",
                                            "2_61","2_62","2_63","2_64","2_65",
                                            "2_7","2_8",
                                            "3_3",
                                            "3_41","3_42","3_43","3_44",
                                            "3_51","3_52","3_53","3_54","3_55","3_56",
                                            "3_61","3_62","3_63","3_64","3_65",
                                            "3_7","3_8",
                                            "4_3",
                                            "4_41","4_42","4_43","4_44",
                                            "4_51","4_52","4_53","4_54","4_55","4_56",
                                            "4_61","4_62","4_63","4_64","4_65",
                                            "4_7","4_8",
                                            "5_3",
                                            "5_41","5_42","5_43","5_44",
                                            "5_51","5_52","5_53","5_54","5_55","5_56",
                                            "5_61","5_62","5_63","5_64","5_65",
                                            "5_7","5_8"),
                            scenario_abbrev=c("CC-CR",
                                              "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
                                              "RR50-CR","RR25-CR","RR00-CR",
                                              "RR50-NT-CR","RR25-NT-CR","RR00-NT-CR",
                                              "BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
                                              "CN","CC-NT-CR"),
                            climate_esm=c("Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL"),
                            climate_esm_scenario=c("Baseline","Baseline","Baseline","Baseline",
                                                   "Baseline","Baseline","Baseline","Baseline",
                                                   "Baseline","Baseline","Baseline","Baseline",
                                                   "Baseline","Baseline","Baseline","Baseline",
                                                   "Baseline","Baseline",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                   "SSP1-2.6","SSP1-2.6",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                   "SSP5-8.5","SSP5-8.5"),                            
                            climate_esm=c("Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                          "GFDL_ESM4","GFDL_ESM4",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                          "UKESM1-0-LL","UKESM1-0-LL"),
                            climate_desc=c("Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline","Baseline","Baseline",
                                          "Baseline","Baseline",
                                          "GFDL_Low","GFDL_Low","GFDL_Low","GFDL_Low",
                                          "GFDL_Low","GFDL_Low","GFDL_Low","GFDL_Low",
                                          "GFDL_Low","GFDL_Low","GFDL_Low","GFDL_Low",
                                          "GFDL_Low","GFDL_Low","GFDL_Low","GFDL_Low",
                                          "GFDL_Low","GFDL_Low",
                                          "GFDL_High","GFDL_High","GFDL_High","GFDL_High",
                                          "GFDL_High","GFDL_High","GFDL_High","GFDL_High",
                                          "GFDL_High","GFDL_High","GFDL_High","GFDL_High",
                                          "GFDL_High","GFDL_High","GFDL_High","GFDL_High",
                                          "GFDL_High","GFDL_High",
                                          "UKESM_Low","UKESM_Low","UKESM_Low","UKESM_Low",
                                          "UKESM_Low","UKESM_Low","UKESM_Low","UKESM_Low",
                                          "UKESM_Low","UKESM_Low","UKESM_Low","UKESM_Low",
                                          "UKESM_Low","UKESM_Low","UKESM_Low","UKESM_Low",
                                          "UKESM_Low","UKESM_Low",
                                          "UKESM_High","UKESM_High","UKESM_High","UKESM_High",
                                          "UKESM_High","UKESM_High","UKESM_High","UKESM_High",
                                          "UKESM_High","UKESM_High","UKESM_High","UKESM_High",
                                          "UKESM_High","UKESM_High","UKESM_High","UKESM_High",
                                          "UKESM_High","UKESM_High")
                            

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
  if_else(mgmt_scenario_num=="7","CN",
  if_else(mgmt_scenario_num=="8","CC-NT-CR",
          "Missing Descriptor"
          ))))))))))))))))))

scenario_descriptor <- 
  if_else(mgmt_scenario_num=="3","Cover Crop, Crop Rotation", # LRF CRSct (4)
  if_else(mgmt_scenario_num=="41","Redu Fert 5%, Crop Rotation",
  if_else(mgmt_scenario_num=="42","Redu Fert 15%, Crop Rotation",
  if_else(mgmt_scenario_num=="43","Redu Fert 25%, Crop Rotation",
  if_else(mgmt_scenario_num=="44","Redu Fert 35%, Crop Rotation",
  if_else(mgmt_scenario_num=="51","Rmv Resid 50%, Crop Rotation",
  if_else(mgmt_scenario_num=="52","Rmv Resid 25%, Crop Rotation",
  if_else(mgmt_scenario_num=="53","Rmv Resid 0%, Crop Rotation", # LRF CSct (2)
  if_else(mgmt_scenario_num=="54","Rmv Resid 50%, No Till, Crop Rotation",
  if_else(mgmt_scenario_num=="55","Rmv Resid 25%, No Till, Crop Rotation",
  if_else(mgmt_scenario_num=="56","Rmv Resid 0%, No Till, Crop Rotation", # LRF CSnt (3)
  if_else(mgmt_scenario_num=="61","Biochar 19 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="62","Biochar 38 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="63","Biochar 57 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="64","Biochar 76 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="65","Biochar 96 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="7","Continuous Crop", # LRF CCct (1)
  if_else(mgmt_scenario_num=="8","No Till, Cover Crop, Crop Rotation", # CRSnt (5)
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


site_id <- 1
land_conversion_year <- 1940 # estimated from NASS Census of Agriculture: county 50% in farms
year_range_fut=experiment_start_year:end_fut_period_year

depth_m <- 0.1
equil_C_input <- 123 #129 # g C/m^2 annually; based on inverse modeling with RothC
#surface_C_init <- 30 # Mg C ha-1; estimated from 45 in topsoil in literature

control_treatment <- "CCct"
control_treatment_num <- 7
treatment <- #if_else(mgmt_scenario_num==1, "CSct", # won't run: dup of 53
             #if_else(mgmt_scenario_num==2, "CSnt", # won't run: dup of 56
             if_else(mgmt_scenario_num==3, "CRSct",
             if_else(mgmt_scenario_grp==4 &
                       mgmt_scenario_opt<=4, "CSct",
             if_else(mgmt_scenario_grp==4, "CSnt",
             if_else(mgmt_scenario_grp==5 &
                       mgmt_scenario_opt<=3, "CSct",
             if_else(mgmt_scenario_grp==5, "CSnt",
             if_else(mgmt_scenario_grp==6 &
                       mgmt_scenario_opt<=5, "CSct",
             if_else(mgmt_scenario_grp==6, "CSnt",
             if_else(mgmt_scenario_grp==7, "CCct",
             if_else(mgmt_scenario_grp==8, "CRSnt",
             "Error")))))))))#))
# this treatment num provides a crosswalk from LRF treatment num to scenario num
treatment_num <- #if_else(mgmt_scenario_num==1, 2, # won't run: dup of 53
                 #if_else(mgmt_scenario_num==2, 3, # won't run: dup of 56
                 if_else(mgmt_scenario_num==3, 4,
                 if_else(mgmt_scenario_grp==4 &
                           mgmt_scenario_opt<=4, 2,
                 if_else(mgmt_scenario_grp==4, 3,
                 if_else(mgmt_scenario_grp==5 &
                           mgmt_scenario_opt<=3, 2,
                 if_else(mgmt_scenario_grp==5, 3,
                 if_else(mgmt_scenario_grp==6 &
                           mgmt_scenario_opt<=5, 2,
                 if_else(mgmt_scenario_grp==6, 3,
                 if_else(mgmt_scenario_grp==7, 1,
                 if_else(mgmt_scenario_grp==8, 5,
                 0)))))))))#))
soil_temp_bias <- if_else(mgmt_scenario_num==1, 0,
                  if_else(mgmt_scenario_num==2, 0,
                  if_else(mgmt_scenario_num==3, 0,
                  if_else(mgmt_scenario_grp==4 &
                            mgmt_scenario_opt<=4, 0,
                  if_else(mgmt_scenario_grp==4, 0,
                  if_else(mgmt_scenario_grp==5 &
                            mgmt_scenario_opt<=3, 0,
                  if_else(mgmt_scenario_grp==5, 0,
                  if_else(mgmt_scenario_grp==6 &
                            mgmt_scenario_opt<=5, 0,
                  if_else(mgmt_scenario_grp==6, 0,
                  if_else(mgmt_scenario_grp==7, 0,
                  if_else(mgmt_scenario_grp==8, 0,
                  0)))))))))))
soil_moist_bias <- if_else(mgmt_scenario_num==1, 0,
                   if_else(mgmt_scenario_num==2, 0,
                   if_else(mgmt_scenario_num==3, 0,
                   if_else(mgmt_scenario_grp==4 &
                             mgmt_scenario_opt<=4, 0,
                   if_else(mgmt_scenario_grp==4, 0,
                   if_else(mgmt_scenario_grp==5 &
                             mgmt_scenario_opt<=3, 0,
                   if_else(mgmt_scenario_grp==5, 0,
                   if_else(mgmt_scenario_grp==6 &
                             mgmt_scenario_opt<=5, 0,
                   if_else(mgmt_scenario_grp==7, 0,
                   if_else(mgmt_scenario_grp==8, 0,
                   0))))))))))

# covercrop_aftercotton <- "Rye"
# covercrop_aftersorghum <- "Rye"
# covercrop_aftercotton_APSIM <- "moata" #-a fudge taken from wheat
# covercrop_aftersorghum_APSIM <- "moata"
# covercrop_aftercotton_Daycent <- "RGA"
# covercrop_aftersorghum_Daycent <- "RGA"

covercrop_aftercotton <- "Rye"
covercrop_APSIM <- "moata"
covercrop_Daycent <- "RGA"

obs_soil_path <- paste0(obs_path,'Soil/')
hist_path <- paste0("Data/",site_name,"/Historical Land Use and Yields/")
hist_filename <- "TX-Lubbock County historical yields and C input.xlsx"
obs_treatments_tab <- "Treatments"
obs_fert_tab <- "MgtAmendments"
obs_planting_tab <- "MgtPlanting"
obs_tillage_tab <- "MgtTillage"
obs_soilphys_tab <- "MeasSoilPhys"
obs_soilchem_tab <- "MeasSoilChem"
obs_soilbio_tab <- "MeasSoilBiol"
obs_harvest_tab <- "MeasResidueMgnt"
obs_biomass_tab <- "MeasHarvestFraction"
obs_soiltemp_tab <- "WeatherDaily"


# Color palettes are chosen to accommodate colorblindness, per
# https://davidmathlogic.com/colorblind/#%23000000-%230072B2-%23009E73-%2356B4E9-%23999999-%23CC79A7-%23D55E00-%23E69F00-%23F0E442-%23882255-%23332288-%230B6329

# 9-color palette with grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]green, [4]light blue, [5]grey,
#[6]pink, [7]red, [8]orange, [9]yellow
cbPalette9 <- c("#000000","#0072B2","#009E73","#56B4E9","#999999",
                "#CC79A7","#D55E00","#E69F00","#F0E442")
# 12-color palette with grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]mint green, [4]light blue, [5]grey,
#[6]pink, [7]red, [8]orange, [9]yellow, [10]mauve, [11]royal blue,
#[12]forest green
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
SW_5cm_color <- cbPalette12[2]
SW_10cm_color <- cbPalette12[2]
SW_15cm_color <- cbPalette12[3]
SW_35cm_color <- cbPalette12[4]
SW_60cm_color <- cbPalette12[4]
WFPS_2cm_color <- cbPalette12[11]
WFPS_5cm_color <- cbPalette12[12]
WFPS_10cm_color <- cbPalette12[2]
WFPS_20cm_color <- cbPalette12[2]
WFPS_30cm_color <- cbPalette12[3]
WFPS_45cm_color <- cbPalette12[4]
WFPS_60cm_color <- cbPalette12[4]
CH4_color <- cbPalette12[10]

BiomC_10cm_color <- cbPalette20[10]
#BiomC_40cm_color <- cbPalette20[]
#BiomC_60cm_color <- cbPalette20[]
BiomN_10cm_color <- cbPalette20[6]
#BiomN_40cm_color <- cbPalette20[]
#BiomN_60cm_color <- cbPalette20[]
HumC_10cm_color <- cbPalette20[19]
#HumC_40cm_color <- cbPalette20[]
#HumC_60cm_color <- cbPalette20[]
HumN_10cm_color <- cbPalette20[8]
#HumN_40cm_color <- cbPalette20[]
#HumN_60cm_color <- cbPalette20[]
CtoBiom_10cm_color <- cbPalette20[5]
CtoHum_10cm_color <- cbPalette20[15]
CBtoHum_10cm_color <- cbPalette20[14]
TotalSOC_10cm_color <- cbPalette20[1]
Cin_10cm_color <- cbPalette20[15]

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

#**********************************************************************
# Observational data ------------------------------------------------------
#**********************************************************************


#**********************************************************************
# Historical data ---------------------------------------------------------



Hist_raw <- read_xlsx(paste0(hist_path,hist_filename),
                      sheet="Lubbock County-Calcs",range="A2:F82")

HistY_Mgha <- Hist_raw[Hist_raw$Year<=experiment_start_year,] %>%
  mutate(year=Year,
         cotton_yield_mgha=`Cotton g/m^2`/100,
         sorghum_yield_mgha=`Sorghum g/m^2`/100)



#**********************************************************************
# Measured observations ---------------------------------------------------


####### Read in data tabs to start

obs_treatments_raw <- read_xlsx(paste0(obs_path, obs_filename),
                                sheet=obs_treatments_tab,range="A1:R6") %>%
  mutate(treatment_num=`Treatment Num`,
         treatment=word(`Treatment ID`,2,sep="_"))
  
obs_fert_raw <- read_xlsx(paste0(obs_path, obs_filename),
                          sheet=obs_fert_tab,range="A1:S286") %>%
  mutate(date=Date,
         year=year(date),
         treatment=word(`Treatment ID`,2,sep="_"),
         replicate=word(`Exp Unit ID`,2,sep="_")) %>%
  left_join(obs_treatments_raw[,c("treatment","treatment_num")],
            by="treatment")

obs_planting_raw <- read_xlsx(paste0(obs_path, obs_filename),
                              sheet=obs_planting_tab,range="A1:K721") %>%
  mutate(date=Date,
         year=year(date),
         treatment=word(`Treatment ID`,2,sep="_"),
         replicate=word(`Exp Unit ID`,2,sep="_")) %>%
  left_join(obs_treatments_raw[,c("treatment","treatment_num")],
            by="treatment")

obs_tillage_raw <- read_xlsx(paste0(obs_path, obs_filename),
                             sheet=obs_tillage_tab,range="A1:H607") %>%
  mutate(date=Date,
         year=year(date),
         treatment=word(`Treatment ID`,2,sep="_"),
         replicate=word(`Exp Unit ID`,2,sep="_")) %>%
  left_join(obs_treatments_raw[,c("treatment","treatment_num")],
            by="treatment")

obs_soilphys_raw <- read_xlsx(paste0(obs_path, obs_filename),
                              sheet=obs_soilphys_tab,range="A1:AA154") %>%
  mutate(date=Date,
         year=year(date),
         treatment=word(`Treatment ID`,2,sep="_"),
         replicate=word(`Exp Unit ID`,2,sep="_"),
         bulkdensity_gm3=`Bulk Density g/cm3`) %>%
  left_join(obs_treatments_raw[,c("treatment","treatment_num")],
            by="treatment")

obs_soilchem_raw <- read_xlsx(paste0(obs_path, obs_filename),
                              sheet=obs_soilchem_tab,range="A1:AW187") %>%
  mutate(date=Date,
         year=year(date),
         treatment=word(`Treatment ID`,2,sep="_"),
         replicate=word(`Exp Unit ID`,2,sep="_"),
         orgC_gkg=`Organic C gC/kg`) %>%
  left_join(obs_treatments_raw[,c("treatment","treatment_num")],
            by="treatment")

obs_soilbio_raw <- read_xlsx(paste0(obs_path, obs_filename),
                             sheet=obs_soilbio_tab,range="A1:AD505") %>%
  mutate(date=Date,
         year=year(date),
         treatment=word(`Treatment ID`,2,sep="_"),
         replicate=word(`Exp Unit ID`,2,sep="_")) %>%
  left_join(obs_treatments_raw[,c("treatment","treatment_num")],
            by="treatment")

obs_harvest_raw <- read_xlsx(paste0(obs_path, obs_filename),
                             sheet=obs_harvest_tab,range="A1:AS406") %>%
  mutate(date=Date,
         year=year(date),
         treatment=word(`Treatment ID`,2,sep="_"),
         replicate=word(`Exp Unit ID`,2,sep="_")) %>%
  left_join(obs_treatments_raw[,c("treatment","treatment_num")],
            by="treatment") 

obs_biomass_raw <- read_xlsx(paste0(obs_path, obs_filename),
                             sheet=obs_biomass_tab,range="A1:Q425") %>%
  mutate(date=`Sampling Date`,
         year=year(date),
         treatment=word(`Treatment ID`,2,sep="_"),
         replicate=word(`Exp Unit ID`,2,sep="_")) %>%
  left_join(obs_treatments_raw[,c("treatment","treatment_num")],
            by="treatment")

obs_soiltemp_raw <- read_xlsx(paste0(obs_path, obs_filename),
                              sheet=obs_soiltemp_tab,range="A1:V1296") %>%
  mutate(date=`Weather Date`,
         year=year(date),
         soil_temperature=`Soil Temp 5cm degC`)

#*************************************************************

## Sand/silt/clay %s

soil_texture_pct <- obs_soilphys_raw[!is.na(obs_soilphys_raw$`Sand %`),
                                     c("Sand %","Silt %","Clay %","treatment",
                                       "treatment_num")] %>%
  group_by(treatment,treatment_num) %>%
  summarize(sand_pct=mean(`Sand %`),
            silt_pct=mean(`Silt %`),
            clay_pct=mean(`Clay %`))

soil_texture_pct_site <- data.frame(sand_pct=mean(soil_texture_pct$sand_pct),
                                    silt_pct=mean(soil_texture_pct$silt_pct),
                                    clay_pct=mean(soil_texture_pct$clay_pct))




## pH

soil_pH <- obs_soilchem_raw[!is.na(obs_soilchem_raw$pH),
                         c("pH","treatment","treatment_num")] %>%
  group_by(treatment,treatment_num) %>%
  summarize(pH=mean(pH))

soil_pH_site <- mean(soil_pH$pH)


## Bulk density

ObsBD_grouped <- obs_soilphys_raw[substr(obs_soilphys_raw$treatment,1,1)=='C' &
                                 !is.na(obs_soilphys_raw$bulkdensity_gm3),
                               c("date","year","treatment","treatment_num",
                                 "replicate","Upper cm","Lower cm","bulkdensity_gm3")] %>%
  group_by(year,treatment,treatment_num,replicate,`Upper cm`,`Lower cm`) %>%
  summarize(mean_BD=round(mean(bulkdensity_gm3),2))
colnames(ObsBD_grouped) <- c("year","treatment","treatment_num","replicate",
                             "upper_cm","lower_cm","mean_BD")

ObsBD_mean_bylayer <- ObsBD_grouped[,c("year","treatment","treatment_num",
                                       "replicate","upper_cm","lower_cm","mean_BD")] %>%
  group_by(treatment,treatment_num,upper_cm,lower_cm) %>% 
  summarize(mean_BD=round(mean(mean_BD),2))

## artificially add a 5-15 cm "measurement" for the CCct treatment, as it was
## not included in the 2003 set, and is missing that depth. This will make
## the site mean a more balanced mean, even though it's an estimated addition
ObsBD_mean_bylayer <- rbind(data.frame(treatment="CCct",
                                       treatment_num=1,
                                       upper_cm=5,
                                       lower_cm=15,
                                       mean_BD=1.35),ObsBD_mean_bylayer)

# mean of all layers (0-5,5-10 (for all),5-15 (all but CCct treatment))
ObsBD_mean <- ObsBD_grouped[,c("year","treatment","treatment_num",
                               "replicate","upper_cm","lower_cm","mean_BD")] %>%
  group_by(treatment,treatment_num) %>% 
  summarize(mean_BD=round(mean(mean_BD),2))

# used for creating additional soil layers in 2_Create_soil_data
ObsBD_site_bylayer <- ObsBD_mean_bylayer[,c("upper_cm","lower_cm","mean_BD")] %>%
  group_by(upper_cm,lower_cm) %>%
  summarize(mean_BD=mean(mean_BD))
## since there is a 5-10 and 5-15, the 5-15 could be seen as the average of 
## 5-10 and 10-15, which can be calculated:
ObsBD_site_bylayer <- ObsBD_site_bylayer %>%
  bind_rows(c(upper_cm=10,lower_cm=15,
              mean_BD=((as.numeric(ObsBD_site_bylayer[ObsBD_site_bylayer$lower_cm==15,"mean_BD"])*2)-
                         as.numeric(ObsBD_site_bylayer[ObsBD_site_bylayer$lower_cm==10,"mean_BD"]))))

# mean to 10 cm for all treatments
ObsBD_site <- mean(pull(ObsBD_site_bylayer[ObsBD_site_bylayer$lower_cm <= 10,"mean_BD"]))

### BD set to current treatment value (doesn't account for equiv soil mass)
ObsBD_treat <- ObsBD_grouped[ObsBD_grouped$treatment==treatment,] %>%
  group_by(year,treatment,treatment_num) %>%
  summarize(mean_BD=round(mean(mean_BD),2))

### set BD to control treatment value for equiv. soil mass, mean by depth
ObsBD_ctrl <- ObsBD_grouped[ObsBD_grouped$treatment==control_treatment,] %>%
  group_by(year,treatment,treatment_num) %>%
  summarize(mean_BD=round(mean(mean_BD),2))

# site mean to 10 cm
ObsBD <- as.numeric(ObsBD_site_bylayer[1,"mean_BD"])



## C percent (only one measurement of organic to 10 cm)
ObsC_pct_raw <- obs_soilchem_raw[substr(obs_soilchem_raw$treatment,1,1)=='C' &
                               !is.na(obs_soilchem_raw$orgC_gkg),
                             c("date","year","treatment","treatment_num",
                               "replicate","Upper cm","Lower cm","orgC_gkg")] %>%
  mutate(orgC_pct=orgC_gkg/10) 

ObsC_pct <- ObsC_pct_raw%>%
  group_by(year,treatment,treatment_num,`Upper cm`,`Lower cm`) %>%
  summarize(mean_c=mean(orgC_pct),
            sd_c=sd(orgC_pct))

ObsC_pct_mean <- obs_soilchem_raw[substr(obs_soilchem_raw$treatment,1,1)=='C' &
                               !is.na(obs_soilchem_raw$orgC_gkg),
                             c("date","year","treatment","treatment_num",
                               "replicate","orgC_gkg")] %>%
  mutate(orgC_pct=orgC_gkg/10) %>%
  group_by(treatment,treatment_num) %>%
  summarize(mean_c=mean(orgC_pct),
            sd_c=sd(orgC_pct))

ObsC_pct_site <- mean(ObsC_pct$mean_c)

##  C stock (C% * BD * depth (cm))
### calculate for all plots
ObsC_Mgha_all <- ObsC_pct %>%
  group_by(year,treatment,treatment_num) %>%
  summarize(mean_cpct=round(mean(mean_c,na.rm=T),5),
            sd_cpct=round(mean(sd_c,na.rm=T),5),
            cstock=round(mean_cpct*ObsBD*10,2),
            sd_cstock=round(sd_cpct*ObsBD*10,2))

### this commented out version uses BD from each treatment
# ObsC_Mgha_all <- left_join(ObsC_pct,
#                            ObsBD_mean,
#                            by=c("treatment","treatment_num")
# ) %>%
#   mutate(mean_cpct=round(mean(mean_c,na.rm=T),5),
#          cstock=round(mean_cpct*mean_BD*10,2))

### calculate just for the current treatment
ObsC_Mgha <- ObsC_Mgha_all[ObsC_Mgha_all$treatment==treatment,]

ObsCfit <- lm(cstock ~ year, data = ObsC_Mgha)
ObsCfit_coef <- coef(ObsCfit)
ObsCfit_r2 <- round(summary(ObsCfit)$r.squared,2)


### The following is commented out for LRF; no true control from native
### ecosystem is available, so need to eliminate

# # Use estimated starting OrgC from shortgrass prairie from []
# ObsC_Mgha <- rbind(data.frame(year=land_conversion_year, 
#                               treatment=treatment,
#                               treatment_num=treatment_num,
#                               mean_cpct=NA,
#                               sd_cpct=NA,
#                               cstock=45,
#                               sd_cstock=NA),ObsC_Mgha)


# rbind(data.frame(treatment="CCct",
#                  treatment_num=1,
#                  upper_cm=5,
#                  lower_cm=15,
#                  mean_BD=1.35),ObsBD_mean_bylayer)
#ObsC_Mgha_mean_5yr <- mean(ObsC_Mgha$mean_cpct[1:5])

## Grain Yield
ObsYield_raw <- obs_harvest_raw[substr(obs_harvest_raw$treatment,1,1)=='C',
                                c("date","year","treatment","treatment_num",
                                  "replicate","Crop","Harvested Frac",
                                  "Grain Dry Matt kg/ha","Harv NonGrain Bio kg/ha")] %>%
  mutate(crop=if_else(grepl("Sorghum",Crop,fixed=TRUE), "Sorghum",
              if_else(grepl("Cotton", Crop,fixed=TRUE), "Cotton",
              if_else(grepl("Rye", Crop,fixed=TRUE), "Rye",
                      "Error"))),
         harv_frac=`Harvested Frac`,
         yield=if_else(is.na(`Grain Dry Matt kg/ha`),`Harv NonGrain Bio kg/ha`,`Grain Dry Matt kg/ha`))
ObsYield_mean <- ObsYield_raw[,c("date","year","treatment","treatment_num",
                                 "replicate","crop","harv_frac","yield")] %>%
  group_by(date,year,treatment,treatment_num,crop,harv_frac) %>%
  summarize(mean_yield=mean(yield, na.rm=T)/1000, # Mg/ha
            sd_yield=sd(yield,na.rm=T)/1000, # Mg/ha
            mean_yield_kgha=mean(yield, na.rm=T),
            sd_yield_kgha=sd(yield,na.rm=T)) %>%
  mutate(mean_yield_gm2=mean_yield_kgha*100,
         sd_yield_gm2=sd_yield_kgha*100)

ObsYield <- ObsYield_mean[ObsYield_mean$treatment_num==treatment_num,]

ObsCYfit <- lm(mean_yield ~ year, data = ObsYield[ObsYield$crop=="Cotton",])
ObsCYfit_coef <- coef(ObsCYfit)
ObsCYfit_r2 <- round(summary(ObsCYfit)$r.squared,2)

if(mgmt_scenario_grp %in% c(3,8)) {
ObsSYfit <- lm(mean_yield ~ year, data = ObsYield[ObsYield$crop=="Sorghum",])
ObsSYfit_coef <- coef(ObsSYfit)
ObsSYfit_r2 <- round(summary(ObsSYfit)$r.squared,2)
}

## Biomass yield
## whole plant biomass (used in addition to grain measurements)
ObsBiomass_raw <- obs_biomass_raw[substr(obs_biomass_raw$treatment,1,1)=='C',
                                  c("date","year","treatment","treatment_num",
                                    "replicate","Crop","Plant Fraction",
                                    "Frac Dry Matt kg/ha")] %>%
  mutate(date=as.Date(date),
         crop=if_else(grepl("Sorghum",Crop,fixed=TRUE), "Sorghum",
                      if_else(grepl("Cotton", Crop,fixed=TRUE), "Cotton",
                              if_else(grepl("Rye", Crop,fixed=TRUE), "Rye",
                                      "Error"))),
         harv_frac=`Plant Fraction`,
         yield=`Frac Dry Matt kg/ha`)

ObsBiomass_mean <- ObsBiomass_raw[,c("date","year","treatment","treatment_num",
                                     "replicate","crop","harv_frac","yield")] %>%
  group_by(date,year,treatment,treatment_num,crop,harv_frac) %>%
  summarize(mean_yield=mean(yield, na.rm=T)/1000, # Mg/ha
            sd_yield=sd(yield,na.rm=T)/1000, # Mg/ha
            mean_yield_kgha=mean(yield, na.rm=T),
            sd_yield_kgha=sd(yield,na.rm=T)) %>%
  mutate(mean_yield_gm2=mean_yield_kgha*100,
         sd_yield_gm2=sd_yield_kgha*100)

ObsBiomass <- ObsBiomass_mean[ObsBiomass_mean$treatment_num==treatment_num,]


## Soil temp
ObsTemp_all <- obs_soiltemp_raw[,c("date","year","soil_temperature")]
ObsTemp <- ObsTemp_all %>%
  mutate(date=as.Date(date))

ObsTfit <- lm(soil_temperature ~ date, data = ObsTemp)
ObsTfit_coef <- coef(ObsTfit)
ObsTfit_r2 <- round(summary(ObsTfit)$r.squared,2)

ObsTemp_range <- range(ObsTemp$soil_temperature,na.rm=T)

#********************************************************************
#* Liberty Research Farm data does not include gases or soil moisture
#********************************************************************
## Instead, create blank data frames with all the necessary columns
## so that subsequent code (hopefully) won't break.

exp_dates <- seq(as.Date(experiment_start_date),
                 as.Date(experiment_end_date),by = "1 day") 

## Soil gases
na_cols_df <- data.frame(treatment=NA, CH4_C=NA,
                         CO2_C=NA, N2O_N=NA)
ObsGas_all <- cbind(exp_dates,na_cols_df) %>%
  mutate(date=exp_dates,
         year=year(date)) %>%
  select(-exp_dates)
ObsGas <- ObsGas_all %>%
  select(-treatment)

## Soil moisture
na_cols_df <- data.frame(treatment=NA, mean_VSM=NA)
ObsVSM <- cbind(exp_dates,na_cols_df) %>%
  mutate(date=exp_dates,
         year=year(date)) %>%
  select(-exp_dates)

#**********************************************************************


## Microbial biomass
ObsMB_raw <- obs_soilbio_raw[substr(obs_soilbio_raw$treatment,1,1)=='C',
                             c("date","year","treatment","treatment_num",
                               "replicate","Microbe Bio C mgC/kg",
                               "Microbe Bio N mgN/kg")] %>%
  mutate(date=as.Date(date),
         mb_mgkg=`Microbe Bio C mgC/kg`,
         mbn_mgkg=`Microbe Bio N mgN/kg`)

ObsMB_mean <- ObsMB_raw %>%
  group_by(year,treatment,treatment_num,date) %>%
  summarize(mean_MB_mgkg=round(mean(mb_mgkg,na.rm=T),0),
            sd_MB_mgkg=sd(mb_mgkg,na.rm=T))

### using the control bulk density here
ObsMB_all <- ObsMB_mean %>%
  mutate(bd=ObsBD_ctrl$mean_BD,
         mb_gm2=mean_MB_mgkg*bd/100,
         mb_mgha=mb_gm2/100,
         sd_gm2=sd_MB_mgkg*bd/100,
         sd_mgha=sd_gm2/100)

ObsMB <- ObsMB_all[ObsMB_all$treatment==treatment,] 

ObsMBfit <- lm(mb_gm2 ~ year, data = ObsMB)
ObsMBfit_coef <- coef(ObsMBfit)
ObsMBfit_r2 <- round(summary(ObsMBfit)$r.squared,2)


#**********************************************************************
#* Plant tissue was for troubleshooting Daycent, which LRF doesn't have
#**********************************************************************
# ## Plant tissue C and N content
# ObsPltCN_raw <- read.csv(paste0(obs_path,obs_plant_cn_filename),
#                          skip=24) %>%
#   mutate(date=as.Date(sample_date, format="%m/%d/%Y"),
#          year=year(date),
#          Treatment=Trt)
# 
# ###check for plant types
# #unique(ObsPltCN_raw[ObsPltCN_raw$Trt %in% c("T1","T2","T3"),"species"])
# 
# ObsPltCN_mean <- ObsPltCN_raw %>%
#   group_by(year,Treatment,species,type) %>%
#   summarize(percent_C=round(mean(percent_C),2),
#             percent_N=round(mean(percent_N),2)) %>%
#   mutate(crop=if_else(species=="Zea mays L. (*)", "Maize",
#                       if_else(species=="Glycine max L. (*)", "Soybean",
#                               if_else(species=="Triticum aestivum L. (*)", "Wheat",
#                                       species))),
#          cn_ratio=percent_C/percent_N
#   )
# 
# ### "widen" the results to match format of Daycent data
# ObsPltCN_wide <- pivot_wider(ObsPltCN_mean,
#                              names_from = type,
#                              values_from = c("percent_C","percent_N",
#                                              "cn_ratio",)) %>%
#   select(year,Treatment,species,crop,percent_C_SEED,percent_C_STOVER,
#          percent_N_SEED,percent_N_STOVER,cn_ratio_SEED,cn_ratio_STOVER)
# 
# ObsGrainCN <- ObsPltCN_mean[ObsPltCN_mean$Treatment==treatment
#                             & ObsPltCN_mean$type=="SEED",] %>%
#   left_join(ObsYield[,!names(ObsYield)=="mean_yield"],
#             by=c("year","Treatment","crop")) %>%
#   mutate(grainC_gm2=percent_C/100*mean_yield_gm2,
#          grainN_gm2=percent_N/100*mean_yield_gm2)
# 
# 



# 
# ### now "widen" the results to match format of Daycent data, add N data
# ObsBiomass_wide <- pivot_wider(ObsBiomass, 
#                              names_from = Fraction,
#                              values_from = "biomass_gm2") %>%
#   select(year,Treatment,Species,crop,SEED,WHOLE,STOVER) %>%
#   mutate(STOVER=WHOLE-SEED)
# 
# ObsStoverCN <- ObsBiomass_wide[ObsBiomass_wide$Treatment==treatment,
#                                !names(ObsBiomass_wide) %in% c("SEED","WHOLE")]  %>%
#   left_join(ObsPltCN_wide[,c("year","Treatment","crop","percent_C_STOVER","percent_N_STOVER")],
#             by=c("year","Treatment","crop")) %>%
#   mutate(stoverC_gm2=percent_C_STOVER/100*STOVER,
#          stoverN_gm2=percent_N_STOVER/100*STOVER)


#**********************************************************************
# Fertilizer ----------------------------------------

# Add fertilizer for GHG reference

# # APSIM needs its own df because the date format is different? Not sure
# # it's being used, though, so commenting it out.
# Fert_APSIM <- obs_fert_raw %>%
#   mutate(date=as.Date(Date,format="%Y-%m-%d"))
Fert <- obs_fert_raw[substr(obs_fert_raw$treatment,1,1)=='C',
                     c("date","year","treatment","treatment_num",
                       "replicate","Crop","Amend Placement","Amend Type",
                       "Total N Amount kgN/ha","Total P Amount kgP/ha",
                       "Total K Amount kgK/ha")] %>%
  mutate(date=as.Date(date),
         crop=Crop,
         amend_method=`Amend Placement`,
         amend_type=`Amend Type`,
         totalN_kgha=`Total N Amount kgN/ha`,
         totalP_kgha=`Total P Amount kgP/ha`,
         totalK_kgha=`Total K Amount kgK/ha`) %>%
  select(-c(`Crop`,`Amend Placement`,`Amend Type`,`Total N Amount kgN/ha`,
            `Total P Amount kgP/ha`,`Total K Amount kgK/ha`))


#**********************************************************************
# Weather -----------------------------------------------------------------


ObsWth <- read.table(paste0(apsim_path,"basic_wth_",clim_scenario_num,".met"),
                    skip=8,sep=" ",row.names=NULL,
                    col.names=c("year","day","radn","maxt","mint","rain")) %>%
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
  if(mgmt_scenario_grp %in% c(3,8)) {
calib_log_tab <- cbind(as.character(Sys.time()),'Observed',
                       clim_scenario_num,mgmt_scenario_num, scenario_name,
                       scenario_abbrev,
                       NA, NA, NA, NA, NA, NA, # Maize 1 to 1
                       NA, # Maize diff
                       NA, NA, NA, NA, NA, NA, # Soybean 1 to 1
                       NA,
                       NA, NA, NA, NA, NA, NA, # Wheat 1 to 1
                       NA,
                       NA, NA, NA, NA, NA, NA, # SOC 1 to 1
                       NA, NA, # SOC diff
                       NA, NA, NA, NA, NA, NA, # Temp 1 to 1
                       NA,
                       NA, NA, NA, NA, NA, NA, # Moist 1 to 1
                       NA,
                       NA, NA, NA, NA, NA, NA, # N2O 1 to 1
                       NA,
                       NA, NA, NA, NA, NA, NA, # CH4 1 to 1
                       NA,
                       NA, NA, NA, NA, # M Bio 1 to 1
                       NA, # MB diff
                       NA, NA, NA, NA, # Cotton 1 to 1
                       NA,
                       NA, NA, NA, NA, # Sorghum 1 to 1
                       NA,
                       NA, NA, NA, # maize, soybean, wheat cultivars
                       NA, NA, # cotton, sorghum cultivars
                       NA, NA, NA, NA, # Corn time series
                       NA, NA, NA, NA, # Soybeans time series
                       NA, NA, NA, NA, # Wheat time series
                       ObsCfit_coef[2], NA, ObsCfit_r2, NA, # SOC time series
                       NA, NA, NA, NA, # SOC w/o outliers
                       ObsTfit_coef[2], NA, ObsTfit_r2, NA, # Temperature time series
                       NA, NA, NA, NA, # Moisture time series
                       NA, NA, NA, NA, # N2O time series
                       NA, NA, NA, NA, # CH4 time series
                       ObsMBfit_coef[2], NA, ObsMBfit_r2, NA, # M Bio time series
                       ObsCYfit_coef[2], NA, ObsCYfit_r2, NA, # Cotton time series
                       ObsSYfit_coef[2], NA, ObsSYfit_r2, NA # Sorghum time series
)
  } else {
    calib_log_tab <- cbind(as.character(Sys.time()),'Observed',
                           clim_scenario_num,mgmt_scenario_num, scenario_name,
                           scenario_abbrev,
                           NA, NA, NA, NA, NA, NA, # Maize 1 to 1
                           NA,
                           NA, NA, NA, NA, NA, NA, # Soybean 1 to 1
                           NA,
                           NA, NA, NA, NA, NA, NA, # Wheat 1 to 1
                           NA,
                           NA, NA, NA, NA, NA, NA, # SOC 1 to 1
                           NA, NA, # SOC diff
                           NA, NA, NA, NA, NA, NA, # Temp 1 to 1
                           NA,
                           NA, NA, NA, NA, NA, NA, # Moist 1 to 1
                           NA,
                           NA, NA, NA, NA, NA, NA, # N2O 1 to 1
                           NA,
                           NA, NA, NA, NA, NA, NA, # CH4 1 to 1
                           NA,
                           NA, NA, NA, NA, # M Bio 1 to 1
                           NA, # MB diff
                           NA, NA, NA, NA, # Cotton 1 to 1
                           NA,
                           NA, NA, NA, NA, # Sorghum 1 to 1
                           NA,
                           NA, NA, NA, # maize, soybean, wheat cultivars
                           NA, NA, # cotton, sorghum cultivars
                           NA, NA, NA, NA, # Maize time series
                           NA, NA, NA, NA, # Soybean time series
                           NA, NA, NA, NA, # Wheat time series
                           ObsCfit_coef[2], NA, ObsCfit_r2, NA, # SOC time series
                           NA, NA, NA, NA, # SOC w/o outliers
                           ObsTfit_coef[2], NA, ObsTfit_r2, NA, # Temp time series
                           NA, NA, NA, NA, # Moist time series
                           NA, NA, NA, NA, # N2O time series
                           NA, NA, NA, NA, # CH4 time series
                           ObsMBfit_coef[2], NA, ObsMBfit_r2, NA, # M Bio time series
                           ObsCYfit_coef[2], NA, ObsCYfit_r2, NA, # Cotton time series
                           NA, NA, NA, NA # Sorghum time series
    )
}

source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_log_tab,'Observed',scenario_name)
}

#**********************************************************************
# Clean up ----------------------------------------------------------------

rm(obs_biomass_raw,obs_fert_raw,
   obs_soilbio_raw,obs_soilchem_raw,obs_soilphys_raw,obs_soiltemp_raw,
   obs_treatments_raw,ObsMB_raw,ObsBiomass_raw,
   ObsCfit,ObsCfit_coef,ObsCfit_r2,
   ObsCYfit,ObsCYfit_coef,ObsCYfit_r2,
   ObsMBfit,ObsMBfit_coef,ObsMBfit_r2
   )
if(mgmt_scenario_grp %in% c(3,8)) {
  rm(ObsSYfit,ObsSYfit_coef,ObsSYfit_r2)
} 

## don't remove obs_tillage_raw, obs_harvest_raw, or obs_planting_raw as
## they are used by other scripts

}) # end suppressMessages

