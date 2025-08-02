#######################################
# File: 3_Create_management_input_files-RothC_KBS.R
# Author: Ellen Maas
# Date: Nov 7, 2022
# Description: This script takes C output from Daycent and makes landman files
# for RothC.
########################################

print(paste0("Starting 3_Create_management_input_files-RothC_",site_name,".R"))

library(dplyr)

## equilibrium

C_multiplier <- if_else(mgmt_scenario_num==56, 1.0,
                if_else(mgmt_scenario_grp %in% c(4,5), 1.0,
                if_else(mgmt_scenario_grp==3, 1.0, 1.0)))

#constructed for shortgrass prairie ecosystem (Eqinit.dat)

eqil <- round(eqinit[1]/100*equil_C_input,2) # convert g/m^2 to Mg ha-1
eqil[,2:3] <- eqinit[,2:3]

cat(paste0("'",site_name," equilibrium (tallgrass prairie ecosystem)- based on 2.9 Mg C/ha-1 annually for 59 Mg ha-1 in top 25 cm'"),
          file=paste0(rothc_path,rothc_eqil_filename), sep="\n",append=FALSE) 
write.table(eqil, file=paste0(rothc_path,rothc_eqil_filename),sep="\t",
            col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)


## base-future

# First, decrease C input by multipliers determined by calibration
Cin_monthly_Mgha$Cinput_mon_Mgha <- Cin_monthly_Mgha$Cinput_mon_Mgha*C_multiplier


# create one file per time period per scenario
for(i in land_conversion_year:end_fut_period_year) {
  landman_file <- paste("/landman/",substr(as.character(i),2,4),"_",scenario_name,".dat",sep="")
  landman_data <- Cin_monthly_Mgha[Cin_monthly_Mgha$year==i,c("Cinput_mon_Mgha","Manure_Mgha","Soil_covered")]
  ### header text and data
  cat(paste0("'",site_name," ",annual_crops[annual_crops$year==i,"crops"]," ",i,"'"),
      file=paste(rothc_path,landman_file, sep=""), sep="\n",append=FALSE) 
  write.table(landman_data, file=paste(rothc_path,landman_file, sep=""),
              col.names=FALSE, row.names=FALSE, sep="\t", 
              quote=FALSE, append=TRUE)
}



