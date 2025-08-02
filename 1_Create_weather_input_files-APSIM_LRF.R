#######################################
# Script: 1_Create_weather_input_files-APSIM_LRF.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: This takes the prepared weather data from the setup script
#              and formats it specifically for APSIM. The apsimx library
#              function as_apsim_met formats the data to APSIM .met
#              format and writes the file.
#######################################

suppressMessages({
  
  print(paste0("Starting 1_Create_weather_input_files-APSIM_",site_name,".R"))
  
  if(clim_scenario_num==1) {
    
    # Convert into an APSIM met-formatted file
    APSIM_basic <- as_apsim_met(new_dat[,c("year","dayofyear","radn_MJm2",
                              "maxt_C","mint_C","rain_mm")],
                              filename="basic_wth_exp.met",
                              site=site_name,
                              latitude=latitude,
                              longitude=longitude,
                              )

    # find any columns with NA cells and gap-fill
    try(
      APSIM_basic <- napad_apsim_met(APSIM_basic),
      silent=TRUE
    )
    
  write_apsim_met(APSIM_basic, wrt.dir=apsim_path, filename="basic_wth_exp.met")

    ###########
    
    # baseline through future period
    
    # Select year, dayofyear, radiation (MJ/m^2), maxt, mint, precip (mm)
    APSIM_basic_fut <- as_apsim_met(new_dat_fut[,c("year","dayofyear","radn_MJm2",
                                                     "maxt_C","mint_C","rain_mm")],
                                     filename=paste0("basic_wth_",clim_scenario_num,".met"),
                                     site=site_name,
                                     latitude=latitude,
                                     longitude=longitude,
    )
  
  # find any columns with NA cells and gap-fill
  try(
    APSIM_basic_fut <- napad_apsim_met(APSIM_basic_fut),
    silent=TRUE
  )
  
  } else if(clim_scenario_num>1) {
    
    fut_dat <- read.csv(file=paste0(fut_weather_path,"fut_clim_scenario_",
                                    clim_scenario_num,'_reanal.csv'))
    
    # Get experimental period and bind to future
    
    ## Select year, dayofyear, radiation (MJ/m^2), maxt, mint, precip (mm)
    APSIM_basic <- new_dat[,c("year","dayofyear","radn_MJm2",
                              "maxt_C","mint_C","rain_mm","month")]

    APSIM_basic_fut <- fut_dat[,c("year","dayofyear","radn_MJm2",
                            "maxt_C","mint_C","rain_mm","month")]
    
    # find any columns with NA cells and gap-fill
    try(
      APSIM_basic <- napad_apsim_met(APSIM_basic),
      silent=TRUE
    )
    try(
      APSIM_basic_fut <- napad_apsim_met(APSIM_basic_fut),
      silent=TRUE
    )
    
    APSIM_basic_combined <- rbind(APSIM_basic,APSIM_basic_fut)
    colnames(APSIM_basic_combined) <- c("year","dayofyear","radn","maxt","mint","rain","month")
    
    APSIM_basic_fut <- as_apsim_met(APSIM_basic_combined[,c("year","dayofyear","radn",
                                                            "maxt","mint","rain")],
                                    filename=paste0("basic_wth_",clim_scenario_num,".met"),
                                    site=site_name,
                                    latitude=latitude,
                                    longitude=longitude,
    )
    
  } # if clim_scenario_num == 1
  
  write_apsim_met(APSIM_basic_fut, wrt.dir=apsim_path, 
                  filename=paste0("basic_wth_",clim_scenario_num,".met"))
  
  rm(list = c("APSIM_basic","APSIM_basic_fut"))
if(clim_scenario_num>1) {
  rm(list = c("fut_dat","APSIM_basic_combined"))
}  
  
}) # end suppressMessages

