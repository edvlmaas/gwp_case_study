#######################################
# Function: 2_Create_soil_data-setup2_KBS.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: Downloads the closest SSURGO soil profile for the site and updates
# the bulk density and C content with site values. Fills in treatment differences
# and creates a comprehensive set of soil attributes for the models.
#######################################

print("2_Create_soil_data-Daycent_base2_KBS.R")


library(apsimx)
library(stringr)
library(dplyr)
library(tidyverse)
library(soiltexture)
library(xml2)
library(lubridate)

###########################
#import and clean
###########################

soils_base1_in <- read.table(paste0(daycent_path,"soils_base1.in"),
                             col.names=c("upper_depth_cm","lower_depth_cm","BD","DUL","LL15","evap_coef",
                                      "root_fraction","sand_fraction","clay_fraction","OM_fraction",
                                      "deltamin","ksat_cmsec","PH"))
soils_in <- read.table(paste0(daycent_path,paste0("soils_",treatment_num,".in")),
                              col.names=c("upper_depth_cm","lower_depth_cm","BD","DUL","LL15","evap_coef",
                                          "root_fraction","sand_fraction","clay_fraction","OM_fraction",
                                          "deltamin","ksat_cmsec","PH"))

soils_base2_in <- soils_base1_in
soils_base2_in$BD <- round(rowMeans(cbind(soils_base1_in$BD,soils_in$BD)),2)
soils_base2_in$OM_fraction <- round(if_else(soils_base1_in$upper_depth_cm<=10,(soils_base1_in$OM_fraction-soils_in$OM_fraction)*0.75,
                                            if_else(soils_base1_in$upper_depth_cm<100,rowMeans(cbind(soils_base1_in$OM_fraction,soils_in$OM_fraction)),
                                            soils_base1_in$OM_fraction)),6)

# take mean of BD, Carbon, and recalculate everything else

##########################################################################
## save this much to a data frame and calculate the water attributes
# extract just soil data into a dataframe

saxton_rawls_df <- soils_base2_in %>%
  mutate(sand_frac = sand_fraction,
         clay_frac = clay_fraction,
         silt_frac = (100-(sand_frac+clay_frac)),
         OM_frac = OM_fraction, # organic matter fraction
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
soils_base2_in$LL15 <- round(saxton_rawls_df$LL15,3)
soils_base2_in$DUL <- round(saxton_rawls_df$DUL,3)
soils_base2_in$ksat_cmsec <- round(saxton_rawls_df$Ks/(10*24*60*60),6)
soils_base2_in$deltamin = if_else(soils_base2_in$LL15-0.01>=0, 0.01, soils_base2_in$LL15/10)


#############################################

write.table(soils_base2_in[,c("upper_depth_cm","lower_depth_cm","BD","DUL","LL15","evap_coef",
                       "root_fraction","sand_fraction","clay_fraction","OM_fraction",
                       "deltamin","ksat_cmsec","PH")], 
            file=paste0(daycent_path,"soils_base2.in"),
            row.names=F, quote=F, col.names=F, sep=' ')
