#######################################
# Function: 2_Create_soil_data-APSIM.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: This procedure calls the APSIMX function 
# edit_apsim_replace_soil_profile which uses the soil data collected and 
# formatted by the 2_Create_soil_data-setup scripts and directly updates the
# .apsim files for the given site and scenario this is run for.
#######################################

print("2_Create_soil_data-APSIM.R")

library(apsimx)

###########################
# APSIM
###########################


edit_apsim_replace_soil_profile(paste0("scen_",clim_scenario_num,"_",mgmt_scenario_num,".apsim"), 
                                soil.profile=sps[[1]],
                                src.dir = apsim_path,
                                wrt.dir = apsim_path,
                                overwrite = TRUE)

