#######################################
# File: 4_Create_additional_files-RothC_LRF.R
# Author: Ellen Maas
# Date: Nov 7, 2022
# Description: This script creates the schedule files for RothC.
#
#### Assumes that the weather files have already been generated. ####
#
#######################################

print(paste0("Starting 4_Create_additional_files-RothC_",site_name,".R"))

library(readxl)
library(dplyr)

# import observations and global constants


# local constants

spth <- paste0("RothC/",site_name,"/scenario/")
lpth <- paste0("RothC/",site_name,"/landman/")

# scenario files

# create one management file for weather and management scenario: 
#c=1 is baseline climate
#c=2 is high emissions climate
#m=1 is baseline management (conventional tillage, conventional fertilizer)
#m=2 is no tillage
#m=3 incorporates cover crops

  # set file names
  scenario_file <- paste(scenario_name,".SET",sep="")
  output_file <- scenario_name # RothC appends ".263" to this for "output" and "graph" folders
  scenario_path <- paste(spth,scenario_file, sep="")
  weather_eql <- "Eqil"
  weather_eql_dat <- paste(weather_eql,".dat",sep="")
  landman_eql_file <- "Eqil.dat"
  ### Equilibrium segment, to 1940 (IOM was 2.3585)
  cat(output_file,weather_eql,"2","0","0","0","0","2.1","0","0","0","0","2",landman_eql_file,
      "2","1","2\n", file=scenario_path, sep="\n",append=FALSE)
  ### "Short term" segments, annual through end of future period
  for(k in land_conversion_year:(end_fut_period_year-1)) { 
    if (k == land_conversion_year) # first short term segment uses same eql weather file
      cat("1","2","1","2","2","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),as.character(land_conversion_year),
          "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
    else if (k >= 1941 & k < end_fut_period_year-2)  # new weather files each year, landman alternates even years corn, odd years wheat
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
    else { # we've reached 2049 or 2099, so last year needs end of file indicator ("3" below)
      cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
          "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
          "1","1","0","3", file=scenario_path, sep="\n", append=TRUE)
    }
  } # end for land_conversion_year -> end_fut_period_year
  