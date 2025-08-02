#######################################
# Function: 2_Create_soil_data-setup2_KBS.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: Downloads the closest SSURGO soil profile for the site and updates
# the bulk density and C content with site values. Fills in treatment differences
# and creates a comprehensive set of soil attributes for the models.
#######################################

print(paste0("2_Create_soil_data-setup2_",site_name,".R"))

#!!!!!!!!!! Note: 
######## Limit to 100 cm depth to avoid data errors
#!!!!!!


library(apsimx)
library(stringr)
library(dplyr)
library(tidyverse)
library(soiltexture)
library(xml2)
library(lubridate)


# local constants


BD_0to20 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                             ObsCBDdeep$section=="Surface","mean_BD_adj"])
BD_20to40 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                  ObsCBDdeep$section=="Middle","mean_BD_adj"])
BD_40to60 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                  ObsCBDdeep$section=="Middle","mean_BD_adj"])
BD_60to80 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                  ObsCBDdeep$section=="Deep","mean_BD_adj"])
BD_80to100 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                  ObsCBDdeep$section=="Deep","mean_BD_adj"])
Cpct_0to20 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                               ObsCBDdeep$section=="Surface","mean_C"])
Cpct_20to40 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                           ObsCBDdeep$section=="Middle","mean_C"])
Cpct_40to60 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                           ObsCBDdeep$section=="Middle","mean_C"])
Cpct_60to80 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                           ObsCBDdeep$section=="Deep","mean_C"])
Cpct_80to100 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment==treatment &
                                            ObsCBDdeep$section=="Deep","mean_C"])
Clay_0to20 <- as.numeric(ObsSoilText_mean[ObsSoilText_mean$treatment==treatment,"mean_clay"])
Sand_0to20 <- as.numeric(ObsSoilText_mean[ObsSoilText_mean$treatment==treatment,"mean_sand"])
Silt_0to20 <- as.numeric(ObsSoilText_mean[ObsSoilText_mean$treatment==treatment,"mean_silt"])


###########################
#import and clean
###########################


# download soil data from SSURGO for the lat/lon into a list of "soil.profile"
# classes, pre-formatted for APSIM
sps_ssurgo <- get_ssurgo_soil_profile(lonlat = c(longitude, latitude), nsoil=1)

sps <- sps_ssurgo

# edit attributes from site data and APSIM calibration, relative to each scenario
# based on deep soil cores from 2001 and APSIM calibration

if(mgmt_scenario_num==3) {
  sps[[1]]$crops <- c('maize','soybean','wheat','oats','WhiteClover')
} else {
  sps[[1]]$crops <- c('maize', 'soybean', 'wheat')
}

## soil layers are in 20 cm increments to 200 cm
BD <- round(sps[[1]]$soil$BD,2)
sps[[1]]$soil$BD <- c(BD_0to20, BD_20to40, BD_40to60, BD_60to80, BD_80to100,
                          BD[6:10])
Carbon <- sps[[1]]$soil$Carbon
## APSIM Classic has a lower limit of 0.01 C content, so bottom 5 layers with 0
## were replaced with 0.01
Carbon <- if_else(Carbon==0,0.1,Carbon)
sps[[1]]$soil$Carbon <- c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
                          Carbon[6:10])
# sps[[1]]$soil$Carbon <- if(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4:7)) {
#                            c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
#                              Carbon[6:10]) } else {
#                            #c(0.87, 0.43, 0.43, 0.233, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1) else
#                         if(mgmt_scenario_num==2) {
#                           c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
#                             Carbon[6:10]) } else {
#                            #c(0.99, 0.44, 0.44, 0.354, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1) else
#                         if(mgmt_scenario_num==3) {
#                           c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
#                             Carbon[6:10]) }
#                            #c(0.93, 0.44, 0.44, 0.354, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1)
#                             }}
particlesizeclay <- sps[[1]]$soil$ParticleSizeClay
particlesizesand <- sps[[1]]$soil$ParticleSizeSand
particlesizesilt <- sps[[1]]$soil$ParticleSizeSilt
sps[[1]]$soil$ParticleSizeClay <- c(Clay_0to20, Clay_0to20, Clay_0to20, particlesizeclay[4:10])
sps[[1]]$soil$ParticleSizeSand <- c(Sand_0to20, Sand_0to20, Sand_0to20, particlesizesand[4:10])
sps[[1]]$soil$ParticleSizeSilt <- c(Silt_0to20, Silt_0to20, Silt_0to20, particlesizesilt[4:10])


##########################################################################
## save this much to a data frame and calculate the water attributes
# extract just soil data into a dataframe

saxton_rawls_df <- sps[[1]]$soil %>%
  mutate(sand_frac = ParticleSizeSand/100,
         silt_frac = ParticleSizeSilt/100,
         clay_frac = ParticleSizeClay/100,
         OM_frac = Carbon*1.724/100, # organic matter fraction
         O1500t = -0.024*sand_frac + 0.487*clay_frac + 0.006*OM_frac +
           0.005*sand_frac*OM_frac - 0.013*clay_frac*OM_frac +
           0.068*sand_frac*clay_frac + 0.031, # helper equation to LL15
         LL15 = if_else((O1500t + (0.14 * O1500t - 0.02)) < 0, 0.001,(O1500t + (0.14 * O1500t - 0.02))), # permanent wilting point, %
        O33t = -0.251*sand_frac + 0.195*clay_frac + 0.011*OM_frac +
          0.006*sand_frac*OM_frac - 0.027*clay_frac*OM_frac +
          0.452*sand_frac*clay_frac + 0.299, # helper equation to DUL
        DUL = if_else((O33t + (1.283*O33t^2 - 0.374*O33t - 0.015)) < 0, 0.01,(O33t + (1.283*O33t^2 - 0.374*O33t - 0.015))), # field capacity, %
        DUL_0_33t = 0.278*sand_frac + 0.034*clay_frac + 0.022*OM_frac -
          0.018*sand_frac*OM_frac - 0.027*clay_frac*OM_frac -
          0.584*sand_frac*clay_frac + 0.078, # helper equation to DUL_0_33
        DUL_0_33 = DUL_0_33t + (0.646*DUL_0_33t - 0.107), # soil water content at sat to fc
        SAT = DUL + DUL_0_33 - 0.097*sand_frac + 0.043, # moisture at saturation, %
        B = (log(1500) - log(33))/(log(DUL) - log(LL15)), # moisture-tension coefficient
        lamda = 1/B, # slope of tension-moisture curve
        Ks = 1930*(SAT-DUL)^(3-lamda) # saturated conductivity (mm h-1)
         )
# sps[[1]]$soil$SAT <- round(saxton_rawls_df$SAT,3)
# sps[[1]]$soil$LL15 <- round(saxton_rawls_df$LL15,3)
# sps[[1]]$soil$DUL <- round(saxton_rawls_df$DUL,3)
# sps[[1]]$soil$KS <- round(saxton_rawls_df$Ks,3)

sand_frac <- sps[[1]]$soil$ParticleSizeSand/100
silt_frac <- sps[[1]]$soil$ParticleSizeSilt/100
clay_frac <- sps[[1]]$soil$ParticleSizeClay/100
OM_frac <- sps[[1]]$soil$Carbon*1.724/100 # organic matter fraction

## calibrated and recalculated values using Saxton & Rawls (2006):
### KBS T1:
if(treatment_num==1 | mgmt_scenario_grp %in% c(4:7)) {
  sps[[1]]$soil$DUL <- c(0.26,0.26,0.26,saxton_rawls_df$DUL[4:10]) #0.246,0.142,0.069,0.069,0.069,0.033,0.033)
  sps[[1]]$soil$LL15 <- c(0.03,0.03,0.03,saxton_rawls_df$LL15[4:10]) #0.138,0.069,0.023,0.023,0.023,0.001,0.001)
  sps[[1]]$soil$DUL_0_33t <- 0.278*sand_frac + 0.034*clay_frac + 0.022*OM_frac -
    0.018*sand_frac*OM_frac - 0.027*clay_frac*OM_frac -
    0.584*sand_frac*clay_frac + 0.078 # helper equation to DUL_0_33
  sps[[1]]$soil$DUL_0_33 <- sps[[1]]$soil$DUL_0_33t + (0.646*sps[[1]]$soil$DUL_0_33t - 0.107) # soil water content at sat to fc
  sps[[1]]$soil$SAT = sps[[1]]$soil$DUL + sps[[1]]$soil$DUL_0_33 - 0.097*sand_frac + 0.043 # moisture at saturation, %
  sps[[1]]$soil$B = (log(1500) - log(33))/(log(sps[[1]]$soil$DUL) - log(sps[[1]]$soil$LL15)) # moisture-tension coefficient
  sps[[1]]$soil$lamda = 1/sps[[1]]$soil$B # slope of tension-moisture curve
  sps[[1]]$soil$Ks = 1930*(sps[[1]]$soil$SAT-sps[[1]]$soil$DUL)^(3-sps[[1]]$soil$lamda) # saturated conductivity (mm h-1)
  #sps[[1]]$soil$Ks = sps[[1]]$soil$Ks*2
  sps[[1]]$soil$ksat_cmsec <- sps[[1]]$soil$Ks/(10*60*60) # convert from mm h-1
  #sps[[1]]$soil$ksat_cmsec <- c(0.01,0.01,0.01,0.0055,0.000055,0.00055,0.00055,0.00055,0.00055,0.00055)
  sps[[1]]$soil$KS <- sps[[1]]$soil$Ks
} else if(treatment_num==2) {
  sps[[1]]$soil$DUL <- c(0.26,0.26,0.26,0.246,0.142,0.069,0.069,0.069,0.033,0.033)
  sps[[1]]$soil$LL15 <- c(0.03,0.03,0.03,0.138,0.069,0.023,0.023,0.023,0.001,0.001)
  sps[[1]]$soil$B = (log(1500) - log(33))/(log(sps[[1]]$soil$DUL) - log(sps[[1]]$soil$LL15)) # moisture-tension coefficient
  sps[[1]]$soil$lamda = 1/sps[[1]]$soil$B # slope of tension-moisture curve
  sps[[1]]$soil$Ks = 1930*(sps[[1]]$soil$SAT-sps[[1]]$soil$DUL)^(3-sps[[1]]$soil$lamda) # saturated conductivity (mm h-1)
  #sps[[1]]$soil$Ks = sps[[1]]$soil$KS
  sps[[1]]$soil$ksat_cmsec <- sps[[1]]$soil$Ks/(10*60*60) # convert from mm h-1
  #sps[[1]]$soil$ksat_cmsec <- c(0.01,0.01,0.01,0.0055,0.000055,0.00055,0.00055,0.00055,0.00055,0.00055)
  sps[[1]]$soil$KS <- sps[[1]]$soil$Ks
} else if(treatment_num==3) {
  sps[[1]]$soil$DUL <- c(0.28,0.28,0.28,0.246,0.142,0.069,0.069,0.069,0.033,0.033)
  sps[[1]]$soil$LL15 <- c(0.03,0.03,0.03,0.138,0.069,0.023,0.023,0.023,0.001,0.001)
  sps[[1]]$soil$B = (log(1500) - log(33))/(log(sps[[1]]$soil$DUL) - log(sps[[1]]$soil$LL15)) # moisture-tension coefficient
  sps[[1]]$soil$lamda = 1/sps[[1]]$soil$B # slope of tension-moisture curve
  sps[[1]]$soil$Ks = 1930*(sps[[1]]$soil$SAT-sps[[1]]$soil$DUL)^(3-sps[[1]]$soil$lamda) # saturated conductivity (mm h-1)
  #sps[[1]]$soil$Ks = sps[[1]]$soil$KS
  sps[[1]]$soil$ksat_cmsec <- sps[[1]]$soil$Ks/(10*60*60) # convert from mm h-1
  #sps[[1]]$soil$ksat_cmsec <- c(0.01,0.01,0.01,0.0055,0.000055,0.00055,0.00055,0.00055,0.00055,0.00055)
  sps[[1]]$soil$KS <- sps[[1]]$soil$Ks
}
#

####################################################################
## continue with remaining soil elements

sps[[1]]$soil$SoilCNRatio <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
sps[[1]]$soil$PH <- c(5.5, 5.5, 5.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5)
sps[[1]]$soil$Maize.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$Soybean.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                              0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$Wheat.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
if(mgmt_scenario_num==3) {
sps[[1]]$soil$WhiteClover.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
sps[[1]]$soil$WhiteClover.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$WhiteClover.XF <- c(1,1,1,1,1,1,1,1,1,1)
sps[[1]]$soil$RedClover.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
sps[[1]]$soil$RedClover.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$RedClover.XF <- c(1,1,1,1,1,1,1,1,1,1)
sps[[1]]$soil$Oats.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
sps[[1]]$soil$Oats.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$Oats.XF <- c(1,1,1,1,1,1,1,1,1,1)
}

# extract just soil data into a dataframe
soil_df_raw <- sps[[1]]$soil

# add three more depths at the top (for Daycent, recommended for trace gas subroutines),
# then add new columns which Daycent also needs

three_layers <- rbind(soil_df_raw[1,], soil_df_raw[1,], soil_df_raw[1,])
three_layers[1,"Depth"] <- "0-2"
three_layers[1,"Thickness"] <- 20
three_layers[1,"FOM"] <- 25
three_layers[2,"Depth"] <- "2-5"
three_layers[2,"Thickness"] <- 30
three_layers[2,"FOM"] <- 25
three_layers[3,"Depth"] <- "5-10"
three_layers[3,"Thickness"] <- 50
three_layers[3,"FOM"] <- 50

soil_df <- three_layers %>%
  rbind(soil_df_raw) %>%
  mutate(upper_depth_cm = as.numeric(word(Depth, 1, sep="-")),
         lower_depth_cm = as.numeric(word(Depth, 2, sep="-")),
         root_fraction = c(0.01, 0.04, 0.25, 0.30, 0.15, 0.1, 0.05, 0.04, 0.03,
                           0.02, 0.01, 0, 0),
         sand_fraction = ParticleSizeSand/100,
         clay_fraction = ParticleSizeClay/100,
         OM_fraction = Carbon*2/100,
         deltamin = if_else(LL15-0.01>=0, 0.01, LL15/10),
         evap_coef = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
soil_df[4,"Depth"] <- "10-20"
soil_df[4,"Thickness"] <- 100
soil_df[4,"FOM"] <- 50
soil_df[4,"upper_depth_cm"] <- 10

# #****************************************************************************
# #* Optional: use ISRIC data instead - will have fewer layers overall --------
#  
# sps[[1]]_isric <- get_isric_soil_profile(lonlat = c(longitude, latitude))
# sps[[1]]_isric$soil$KS <- sps[[1]]_isric$soil$KS/24
# isric<-sps[[1]]_isric$soil
# 
# sps[[1]] <- sps[[1]]_isric
# 
# # edit attributes from site data and APSIM calibration, relative to each scenario
# # based on deep soil cores from 2001 and APSIM calibration
# 
# sps[[1]]$crops <- c('maize','soybean','wheat')
# 
# ## soil layers are in 20 cm increments to 200 cm
# # BD <- round(sps[[1]]$soil$BD,2)
# # sps[[1]]$soil$BD <- c(BD_0to20, BD_20to40, BD_40to60, BD_60to80, BD_80to100, 
# #                  BD[6:10])
# Carbon <- sps[[1]]$soil$Carbon
# ## APSIM Classic has a lower limit of 0.01 C content, so bottom 5 layers with 0
# ## were replaced with 0.01
# Carbon <- if_else(Carbon==0,0.1,Carbon)
# sps[[1]]$soil$Carbon <- c(Carbon[1]/2,Carbon[2]/2,Cpct_20to40,Cpct_40to60,Cpct_60to80,
#                      Carbon[6])
# # sps[[1]]$soil$Carbon <- if(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4:7)) {
# #                            c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
# #                              Carbon[6:10]) } else {
# #                            #c(0.87, 0.43, 0.43, 0.233, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1) else 
# #                         if(mgmt_scenario_num==2) {
# #                           c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
# #                             Carbon[6:10]) } else {
# #                            #c(0.99, 0.44, 0.44, 0.354, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1) else
# #                         if(mgmt_scenario_num==3) {
# #                           c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
# #                             Carbon[6:10]) }
# #                            #c(0.93, 0.44, 0.44, 0.354, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1)
# #                             }}
# particlesizeclay <- sps[[1]]$soil$ParticleSizeClay
# particlesizesand <- sps[[1]]$soil$ParticleSizeSand
# particlesizesilt <- sps[[1]]$soil$ParticleSizeSilt
# sps[[1]]$soil$ParticleSizeClay <- c(Clay_0to20, Clay_0to20, particlesizeclay[3:6])
# sps[[1]]$soil$ParticleSizeSand <- c(Sand_0to20, Sand_0to20, particlesizesand[3:6])
# sps[[1]]$soil$ParticleSizeSilt <- c(Silt_0to20, Silt_0to20, particlesizesilt[3:6])
# 
# 
# ##########################################################################
# ## save this much to a data frame and calculate the water attributes
# # extract just soil data into a dataframe
# 
# saxton_rawls_df <- sps[[1]]$soil %>%
#   mutate(sand_frac = ParticleSizeSand/100,
#          silt_frac = ParticleSizeSilt/100,
#          clay_frac = ParticleSizeClay/100,
#          OM_frac = Carbon*1.724/100, # organic matter fraction
#          O1500t = -0.024*sand_frac + 0.487*clay_frac + 0.006*OM_frac +
#            0.005*sand_frac*OM_frac - 0.013*clay_frac*OM_frac +
#            0.068*sand_frac*clay_frac + 0.031, # helper equation to LL15
#          LL15 = if_else((O1500t + (0.14 * O1500t - 0.02)) < 0, 0.001,(O1500t + (0.14 * O1500t - 0.02))), # permanent wilting point, %
#         O33t = -0.251*sand_frac + 0.195*clay_frac + 0.011*OM_frac +
#           0.006*sand_frac*OM_frac - 0.027*clay_frac*OM_frac +
#           0.452*sand_frac*clay_frac + 0.299, # helper equation to DUL
#         DUL = if_else((O33t + (1.283*O33t^2 - 0.374*O33t - 0.015)) < 0, 0.01,(O33t + (1.283*O33t^2 - 0.374*O33t - 0.015))), # field capacity, %
#         DUL_0_33t = 0.278*sand_frac + 0.034*clay_frac + 0.022*OM_frac -
#           0.018*sand_frac*OM_frac - 0.027*clay_frac*OM_frac -
#           0.584*sand_frac*clay_frac + 0.078, # helper equation to DUL_0_33
#         DUL_0_33 = DUL_0_33t + (0.646*DUL_0_33t - 0.107), # soil water content at sat to fc
#         SAT = DUL + DUL_0_33 - 0.097*sand_frac + 0.043, # moisture at saturation, %
#         B = (log(1500) - log(33))/(log(DUL) - log(LL15)), # moisture-tension coefficient
#         lamda = 1/B, # slope of tension-moisture curve
#         Ks = 1930*(SAT-DUL)^(3-lamda) # saturated conductivity (mm h-1)
#          )
# sps[[1]]$soil$SAT <- round(saxton_rawls_df$SAT,3)
# sps[[1]]$soil$LL15 <- round(saxton_rawls_df$LL15,3)
# sps[[1]]$soil$DUL <- round(saxton_rawls_df$DUL,3)
# sps[[1]]$soil$KS <- round(saxton_rawls_df$Ks,3)
# 
# sand_frac <- sps[[1]]$soil$ParticleSizeSand/100
# silt_frac <- sps[[1]]$soil$ParticleSizeSilt/100
# clay_frac <- sps[[1]]$soil$ParticleSizeClay/100
# OM_frac <- sps[[1]]$soil$Carbon*1.724/100 # organic matter fraction
# 
# ## calibrated and recalculated values using Saxton & Rawls (2006):
# ### KBS T1:
# if(treatment_num==1 | mgmt_scenario_grp %in% c(4:7)) {
#   sps[[1]]$soil$DUL <- c(0.26,0.26,0.246,0.142,0.069,0.033)
#   sps[[1]]$soil$LL15 <- c(0.03,0.03,0.138,0.069,0.023,0.001)
#   sps[[1]]$soil$DUL_0_33t <- 0.278*sand_frac + 0.034*clay_frac + 0.022*OM_frac -
#     0.018*sand_frac*OM_frac - 0.027*clay_frac*OM_frac - 
#     0.584*sand_frac*clay_frac + 0.078 # helper equation to DUL_0_33
#   sps[[1]]$soil$DUL_0_33 <- sps[[1]]$soil$DUL_0_33t + (0.646*sps[[1]]$soil$DUL_0_33t - 0.107) # soil water content at sat to fc
#   sps[[1]]$soil$SAT = sps[[1]]$soil$DUL + sps[[1]]$soil$DUL_0_33 - 0.097*sand_frac + 0.043 # moisture at saturation, %
#   sps[[1]]$soil$B = (log(1500) - log(33))/(log(sps[[1]]$soil$DUL) - log(sps[[1]]$soil$LL15)) # moisture-tension coefficient
#   sps[[1]]$soil$lamda = 1/sps[[1]]$soil$B # slope of tension-moisture curve
#   sps[[1]]$soil$Ks = 1930*(sps[[1]]$soil$SAT-sps[[1]]$soil$DUL)^(3-sps[[1]]$soil$lamda) # saturated conductivity (mm h-1) 
#   #sps[[1]]$soil$Ks = sps[[1]]$soil$Ks*2
#   sps[[1]]$soil$ksat_cmsec <- sps[[1]]$soil$Ks/(10*60*60) # convert from mm h-1
#   #sps[[1]]$soil$ksat_cmsec <- c(0.01,0.01,0.01,0.0055,0.000055,0.00055,0.00055,0.00055,0.00055,0.00055)
#   sps[[1]]$soil$KS <- sps[[1]]$soil$Ks
# } else if(treatment_num==2) {
#   sps[[1]]$soil$DUL <- c(0.26,0.26,0.26,0.246,0.142,0.069,0.069,0.069,0.033,0.033)
#   sps[[1]]$soil$LL15 <- c(0.03,0.03,0.03,0.138,0.069,0.023,0.023,0.023,0.001,0.001)
#   sps[[1]]$soil$B = (log(1500) - log(33))/(log(sps[[1]]$soil$DUL) - log(sps[[1]]$soil$LL15)) # moisture-tension coefficient
#   sps[[1]]$soil$lamda = 1/sps[[1]]$soil$B # slope of tension-moisture curve
#   sps[[1]]$soil$Ks = 1930*(sps[[1]]$soil$SAT-sps[[1]]$soil$DUL)^(3-sps[[1]]$soil$lamda) # saturated conductivity (mm h-1)
#   #sps[[1]]$soil$Ks = sps[[1]]$soil$KS
#   #sps[[1]]$soil$ksat_cmsec <- sps[[1]]$soil$Ks/(10*60*60) # convert from mm h-1
#   sps[[1]]$soil$ksat_cmsec <- c(0.01,0.01,0.01,0.0055,0.000055,0.00055,0.00055,0.00055,0.00055,0.00055)
#   sps[[1]]$soil$KS <- sps[[1]]$soil$Ks
# } else if(treatment_num==3) {
#   sps[[1]]$soil$DUL <- c(0.28,0.28,0.28,0.246,0.142,0.069,0.069,0.069,0.033,0.033)
#   sps[[1]]$soil$LL15 <- c(0.03,0.03,0.03,0.138,0.069,0.023,0.023,0.023,0.001,0.001)
#   sps[[1]]$soil$B = (log(1500) - log(33))/(log(sps[[1]]$soil$DUL) - log(sps[[1]]$soil$LL15)) # moisture-tension coefficient
#   sps[[1]]$soil$lamda = 1/sps[[1]]$soil$B # slope of tension-moisture curve
#   sps[[1]]$soil$Ks = 1930*(sps[[1]]$soil$SAT-sps[[1]]$soil$DUL)^(3-sps[[1]]$soil$lamda) # saturated conductivity (mm h-1)
#   #sps[[1]]$soil$Ks = sps[[1]]$soil$KS
#   #sps[[1]]$soil$ksat_cmsec <- sps[[1]]$soil$Ks/(10*60*60) # convert from mm h-1
#   sps[[1]]$soil$ksat_cmsec <- c(0.01,0.01,0.01,0.0055,0.000055,0.00055,0.00055,0.00055,0.00055,0.00055)
#   sps[[1]]$soil$KS <- sps[[1]]$soil$Ks
# }
# # 
# 
# ####################################################################
# ## continue with remaining soil elements
# 
# sps[[1]]$soil$SoilCNRatio <- c(10, 10, 10, 10, 10, 10)
# sps[[1]]$soil$PH <- c(5.5, 5.5, 5.5, 6.5, 6.5, 6.5)
# sps[[1]]$soil$Maize.LL <- c(0.06753815, 0.06753815, 0.13822352,
#                        0.06916603, 0.02266792, 0.02266792)
# sps[[1]]$soil$Soybean.LL <- c(0.06753815, 0.06753815, 0.13822352,
#                          0.06916603, 0.02266792, 0.02266792)
# sps[[1]]$soil$Wheat.LL <- c(0.06753815, 0.06753815, 0.13822352,
#                        0.06916603, 0.02266792, 0.02266792)
# if(mgmt_scenario_num==3) {
#   sps[[1]]$soil$WhiteClover.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06)
#   sps[[1]]$soil$WhiteClover.LL <- c(0.06753815, 0.06753815, 0.13822352,
#                                0.06916603, 0.02266792, 0.02266792)
#   sps[[1]]$soil$WhiteClover.XF <- c(1,1,1,1,1,1,)
#   sps[[1]]$soil$RedClover.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06)
#   sps[[1]]$soil$RedClover.LL <- c(0.06753815, 0.06753815, 0.13822352,
#                              0.06916603, 0.02266792, 0.02266792)
#   sps[[1]]$soil$RedClover.XF <- c(1,1,1,1,1,1,1,1,1,1)
#   sps[[1]]$soil$Oats.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06)
#   sps[[1]]$soil$Oats.LL <- c(0.06753815, 0.06753815, 0.13822352,
#                         0.06916603, 0.02266792, 0.02266792)
#   sps[[1]]$soil$Oats.XF <- c(1,1,1,1,1,1)
# }
# 
# # extract just soil data into a dataframe
# soil_df_raw <- sps[[1]]$soil
# 
# # add three more depths at the top (for Daycent, recommended for trace gas subroutines),
# # then add new columns which Daycent also needs
# 
# # isric layers: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200
# new_layers <- rbind(soil_df_raw[1,], soil_df_raw[1,], soil_df_raw[2,], soil_df_raw[3,],
#                     soil_df_raw[4,],soil_df_raw[4,],soil_df_raw[5,],soil_df_raw[5,],
#                     soil_df_raw[6,],soil_df_raw[6,],soil_df_raw[6,],soil_df_raw[6,],soil_df_raw[6,])
# new_layers[1,"Depth"] <- "0-2"
# new_layers[1,"Thickness"] <- 20
# new_layers[1,"FOM"] <- 25
# new_layers[2,"Depth"] <- "2-5"
# new_layers[2,"Thickness"] <- 30
# new_layers[2,"FOM"] <- 25
# new_layers[3,"Depth"] <- "5-10"
# new_layers[3,"Thickness"] <- 50
# new_layers[3,"FOM"] <- 50
# new_layers[4,"Depth"] <- "10-20"
# new_layers[4,"Thickness"] <- 100
# new_layers[4,"FOM"] <- 50
# new_layers[5,"Depth"] <- "20-40"
# new_layers[5,"Thickness"] <- 200
# new_layers[6,"Depth"] <- "40-60"
# new_layers[6,"Thickness"] <- 200
# new_layers[7,"Depth"] <- "60-80"
# new_layers[7,"Thickness"] <- 200
# new_layers[8,"Depth"] <- "80-100"
# new_layers[8,"Thickness"] <- 200
# new_layers[9,"Depth"] <- "100-120"
# new_layers[9,"Thickness"] <- 200
# new_layers[10,"Depth"] <- "120-140"
# new_layers[10,"Thickness"] <- 200
# new_layers[11,"Depth"] <- "140-160"
# new_layers[11,"Thickness"] <- 200
# new_layers[12,"Depth"] <- "160-180"
# new_layers[12,"Thickness"] <- 200
# new_layers[13,"Depth"] <- "180-200"
# new_layers[13,"Thickness"] <- 200
# 
# soil_df <- new_layers %>%
#   mutate(upper_depth_cm = as.numeric(word(Depth, 1, sep="-")),
#          lower_depth_cm = as.numeric(word(Depth, 2, sep="-")),
#          root_fraction = c(0.01, 0.04, 0.25, 0.30, 0.15, 0.1, 0.05, 0.04, 0.03,
#                            0.02, 0.01, 0, 0),
#          root_fraction = c(0.01, 0.04, 0.25, 0.30, 0.15, 0.1, 0.05, 0.04, 0.03,
#                            0.02, 0.01, 0, 0),
#          sand_fraction = ParticleSizeSand/100,
#          clay_fraction = ParticleSizeClay/100,
#          OM_fraction = Carbon*2/100,
#          deltamin = if_else(LL15-0.01>=0, 0.01, LL15/10),
#          evap_coef = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
# 
# 
