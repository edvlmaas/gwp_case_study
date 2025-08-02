#######################################
# File: 5_Alternative_management_input_files-RothC_LRF
# Author: Ellen Maas
# Date: Nov 7, 2022
# Description: This script was developed for testing purposes
# to put all RothC input data (C input and weather) into a
# spreadsheet in order to play with the spreadsheet version
# of RothC. Not needed for the normal processing of this site.
#######################################

print(paste0("Starting 5_Alternative_management_input_files-RothC_",site_name,".R"))

library(dplyr)

## equilibrium

# C_multiplier <- if_else(mgmt_scenario_grp %in% c(1,4,5), .43,
#                 if_else(mgmt_scenario_grp==2, 0.88,
#                 if_else(mgmt_scenario_grp==3, 0.60, 0.43)))
C_multiplier <- 1

# starting pool sizes in spreadsheet will be manually entered after
# RothC is run to equilibrium


## base-future

# Combine land management and climate into one data frame, then write out
combined_data <- left_join(Cin_monthly_Mgha,
                           base_weather,
                           by=c("year","month")) %>%
  select(year,month,Cinput_mon_Mgha,Manure_Mgha,Soil_covered,Calc_TAVG,
         PRCP,TM_OPE)

write.csv(combined_data,file=paste0(rothc_path,"input_data_tabular_form.csv"),
          row.names = FALSE)

