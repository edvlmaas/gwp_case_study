#######################################
# File: 3_Create_management_input_files-setupRM_KBS.R
# Author: Ellen Maas
# Date: Nov 6, 2022
# Description: This script takes C output from Daycent and formats it for
# input to RothC and Millennial.
#######################################

print(paste0("Starting 3_Create_management_input_files-setupRM_",site_name,".R"))

library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(XML)
library(stringr)


# local constants

## keep these: set internally:

mgmt_path <- paste0("Data/",site_name,"/Management/")

crop_filename <- "51-agronomic+yields+annual+crops+1656512632.csv"
rothc_eqinit_filename <- "landman/Eqinit.dat"
rothc_eqil_filename <- "landman/Eqil.dat"
# mill_init_filename <- "globalaverage.txt"
# mill_equilinit_filename <- "equil_init.txt"
# mill_eqilinput_filename <- "siteenviron_eq_in.txt"
# mill_baseinput_filename <- "siteenviron_base_in.txt"
# mill_futinput_filename <- "siteenviron_in"

# soil_temp_bias <- if_else(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4,5), 5,
#                 if_else(mgmt_scenario_num==2, 4.5,
#                 0))
# soil_moist_bias <- if_else(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4,5), 2,
#                 if_else(mgmt_scenario_num==2, 0,
#                 0))
# 

#**********************************************************************


###########################################
# C input, for both
###########################################

source(paste0("p_Daycent_Cinput_",site_name,".R"))

Cin_daily <- read.csv(file=paste0(mgmt_path,"Daycent_Cinput_",scenario_name,".csv")) %>%
  mutate(date=as.Date(date))
  
# get the crop for each month
Cin_monthly_crop <- Cin_daily[,c("year","month","crop")] %>%
  group_by(year,month) %>%
  summarize(crop=min(crop))

# and annual summary of crops
annual_crops <- unique(all_field_ops[,c("year","crop")]) %>%
  group_by(year) %>%
  summarize(crops=toString(crop))



#**********************************************************************


###########################
# For RothC, monthly

Cin_monthly_Mgha <- Cin_daily[,c("year","month","daily_soilC_Mgha","crop")] %>%
  group_by(year, month) %>%
  summarize(Cinput_mon_Mgha=round(sum(daily_soilC_Mgha),5),
            Manure_Mgha=0) %>%  
  group_by(year) %>%
  mutate(Soil_covered=ifelse(month==5,0,1),
         Cinput_pct=round(Cinput_mon_Mgha/sum(Cinput_mon_Mgha)*100,2)) %>%
  ungroup %>%
  left_join(Cin_monthly_crop[,c("year","month","crop")],
        by=c("year","month"))

Cin_monthly_Mgha[is.na(Cin_monthly_Mgha$Cinput_pct),"Cinput_pct"] <- 0

# ## Mostly for curiosity, calculate the percent of annual Cinput for each month (this
# ## is needed when calculating Cinput manually for monthly input for RothC)
# Cin_annual_Mgha <- Cin_monthly_Mgha[,c("year","Cinput_mon_Mgha","Cinput_pct")] %>%
#   group_by(year) %>%
#   summarize(Cinput_Mgha=sum(Cinput_mon_Mgha),
#             Cpct=sum(Cinput_pct)) %>%
#   merge(obs_yield[obs_yield$Treatment==treatment,c("year","Crop")],
#         all=TRUE)

#write.csv(Cin_annual_Mgha)

## for checks and balances: also get summary of % input by month by crop
### excluding 2020, which is anomalous
Cin_monthly_mean <- Cin_monthly_Mgha[Cin_monthly_Mgha$year!=2020,] %>%
  group_by(crop,month) %>%
  summarize(Cinput_pct=mean(Cinput_pct),2)

check_Cin_annual_totals <- Cin_monthly_mean %>%
  group_by(crop) %>%
  summarize(tot_pct=sum(Cinput_pct))

## for equilibrium input
eqinit <- read.table(paste0(rothc_path,rothc_eqinit_filename),
                     skip=1)



# #**********************************************************************
# 
# 
# ###########################
# # For Millennial, daily. Keep in mind that C input from Daycent is for 20 cm.
#  Cin_daily_gm2 <- Cin_daily[,c("date","year","dayofyr","month","daily_soilC_gm2")] %>%
#    mutate(day=day(date))
# 
# 
# ###########################################
# # Soil temperature, for Millennial
# ###########################################
# 
# # Daycent data, experimental period through future
# ## soil temperature is reduced to reduce warm bias
# Tin_daily_C <-   merge(ObsTemp,
#                        DayT_C_all[,c("date","year","mean_3_4")],
#                      by=c("date","year"),
#                      all=TRUE) %>%
#   mutate(soil_T=ifelse(is.na(soil_temperature),mean_3_4,soil_temperature))
# 
# 
# ###########################################
# # Soil moisture, for Millennial
# ###########################################
# 
# ## Use observations when available; fill in with Daycent estimates
# 
# Min_daily_V <- merge(ObsVSM[,c("date","year","mean_VSM")],
#                      DayM_V_all[,c("date","year","layer4_pct")],
#                      by=c("date","year"),
#                      all=TRUE) %>%
#   mutate(soil_M=ifelse(is.na(mean_VSM),layer4_pct,mean_VSM)/100)
# 
