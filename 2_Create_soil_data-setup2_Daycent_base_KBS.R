#######################################
# Function: 2_Create_soil_data-setup2_Daycent_base_KBS.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: Downloads the closest SSURGO soil profile for the site and updates
# the bulk density and C content with site values. Fills in treatment differences
# and creates a comprehensive set of soil attributes for the models.
#######################################

print(paste0("2_Create_soil_data-setup2_Daycent_base_",site_name,".R"))

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


BD_0to20 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                             ObsCBDdeep$section=="Surface","mean_BD_adj"])
BD_20to40 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                  ObsCBDdeep$section=="Middle","mean_BD_adj"])
BD_40to60 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                  ObsCBDdeep$section=="Middle","mean_BD_adj"])
BD_60to80 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                  ObsCBDdeep$section=="Deep","mean_BD_adj"])
BD_80to100 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                  ObsCBDdeep$section=="Deep","mean_BD_adj"])
Cpct_0to20 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                               ObsCBDdeep$section=="Surface","mean_C"])
Cpct_20to40 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                           ObsCBDdeep$section=="Middle","mean_C"])
Cpct_40to60 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                           ObsCBDdeep$section=="Middle","mean_C"])
Cpct_60to80 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                           ObsCBDdeep$section=="Deep","mean_C"])
Cpct_80to100 <- as.numeric(ObsCBDdeep[ObsCBDdeep$treatment=="T8" &
                                            ObsCBDdeep$section=="Deep","mean_C"])
Clay_0to20 <- as.numeric(ObsSoilText_mean[ObsSoilText_mean$treatment=="T8","mean_clay"])
Sand_0to20 <- as.numeric(ObsSoilText_mean[ObsSoilText_mean$treatment=="T8","mean_sand"])
Silt_0to20 <- as.numeric(ObsSoilText_mean[ObsSoilText_mean$treatment=="T8","mean_silt"])


###########################
#import and clean
###########################


# download soil data from SSURGO for the lat/lon into a list of "soil.profile"
# classes, pre-formatted for APSIM
sps_raw <- get_ssurgo_soil_profile(lonlat = c(longitude, latitude), nsoil=1)
sps <- sps_raw

# edit attributes from site data and APSIM calibration, relative to each scenario
# based on deep soil cores from 2001 and APSIM calibration

sps[[1]]$crops <- c('maize','soybean','wheat')

## soil layers are in 20 cm increments to 200 cm
BD <- round(sps[[1]]$soil$BD,2)
sps[[1]]$soil$BD <- c(BD_0to20, BD_20to40, BD_40to60, BD_60to80, BD_80to100, 
                          BD[6:10])
Carbon <- sps[[1]]$soil$Carbon
## APSIM Classic has a lower limit of 0.01 C content, so bottom 5 layers with 0
## were replaced with 0.01
Carbon <- if_else(Carbon==0,0.1,Carbon)
sps[[1]]$soil$Carbon <- if(mgmt_scenario_num==53 | mgmt_scenario_grp %in% c(4:6)) {
                           c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
                             Carbon[6:10]) } else {
                           #c(0.87, 0.43, 0.43, 0.233, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1) else 
                        if(mgmt_scenario_num==56) {
                          c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
                            Carbon[6:10]) } else {
                           #c(0.99, 0.44, 0.44, 0.354, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1) else
                        if(mgmt_scenario_num==3) {
                          c(Cpct_0to20,Cpct_20to40,Cpct_40to60,Cpct_60to80,Cpct_80to100,
                            Carbon[6:10]) }
                           #c(0.93, 0.44, 0.44, 0.354, 0.19, 0.15, 0.1, 0.1, 0.1, 0.1)
                            }}
particlesizeclay <- sps[[1]]$soil$ParticleSizeClay
particlesizesand <- sps[[1]]$soil$ParticleSizeSand
particlesizesilt <- sps[[1]]$soil$ParticleSizeSilt
sps[[1]]$soil$ParticleSizeClay <- c(Clay_0to20, Clay_0to20, Clay_0to20, particlesizeclay[4:10])
sps[[1]]$soil$ParticleSizeSand <- c(Sand_0to20, Sand_0to20, Sand_0to20, particlesizesand[4:10])
sps[[1]]$soil$ParticleSizeSilt <- c(Silt_0to20, Silt_0to20, Silt_0to20, particlesizesilt[4:10])


##########################################################################
## save this much to a data frame and calculate the water attributes
# extract just soil data into a dataframe
soil_water_raw <- sps[[1]]$soil

saxton_rawls_df <- soil_water_raw %>%
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
        SAT = 1 - BD/2.65, # moisture at saturation, %; 2.65 = assumed particle density
        B = (log(1500) - log(33))/(log(DUL) - log(LL15)), # moisture-tension coefficient
        lamda = 1/B, # slope of tension-moisture curve
        Ks = 1930*(SAT-DUL)^(3-lamda), # saturated conductivity (mm h-1)
        Ks_mmday = Ks*24,
        Ks_cmsec = Ks/10/60/60,
        Ks_cmhr = Ks/10
         )
sps[[1]]$soil$SAT <- round(saxton_rawls_df$SAT,3)
sps[[1]]$soil$AirDry <- round(saxton_rawls_df$AirDry,3)
sps[[1]]$soil$LL15 <- round(saxton_rawls_df$LL15,3)
sps[[1]]$soil$DUL <- round(saxton_rawls_df$DUL,3)

####################################################################
## continue with remaining soil elements

sps[[1]]$soil$PH <- c(5.5, 5.5, 5.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5)

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
         ksat_cmsec = round(KS/(10*24*60*60),6),
         evap_coef = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
soil_df[4,"Depth"] <- "10-20"
soil_df[4,"Thickness"] <- 100
soil_df[4,"FOM"] <- 50
soil_df[4,"upper_depth_cm"] <- 10

soil_df$KS_cmmin <- round(soil_df$KS * (1/(10*24*60)),6)

soil_df$LL15_dm3m3 <- soil_df$LL15*1000
soil_df$DUL_dm3m3 <- soil_df$DUL*1000
soil_df$LL15_mmm3 <- soil_df$LL15*1000000000*0.001
soil_df$DUL_mmm3 <- soil_df$DUL*1000000000*0.001

# add orgC as fraction for LDNDC
soil_df$orgC_fraction <- soil_df$Carbon/100

# calculate soil type code from soil texture

soil_texture_df <- soil_df[1,c("sand_fraction","clay_fraction")]
colnames(soil_texture_df) <- c("SAND","CLAY")
soil_texture_df$SILT <- 1 - (soil_texture_df$SAND + soil_texture_df$CLAY)
soil_texture_df <- soil_texture_df %>%
  mutate(SAND=SAND*100,
         SILT=SILT*100,
         CLAY=CLAY*100)

soil_type_ar <- TT.points.in.classes(tri.data=soil_texture_df,class.sys="USDA.TT")

## find the non-zero column which is the soil type
find_col <- names(which(colSums(soil_type_ar)==1))
soil_type_code <- toupper(if_else(find_col=="Cl","clay",
                          if_else(find_col=="Lo","loam",
                          if_else(find_col=="Si","silt",
                          if_else(find_col=="Sa","sand",
                          if_else(find_col=="SiClLo","slcl",
                          if_else(find_col=="SaClLo","sncl",
                                  find_col))))))
                          )

