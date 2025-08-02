#*******************************************************************************
# Function: f_calc_soil_water_properties.R
# Author: Ellen Maas
# Date: 21 Feb 2025
# Description: This function prepares data from observations and model output
# for analysis, including calibration and future scenarios. 
#*******************************************************************************
# Calls:
# Nothing.
#*******************************************************************************
# Returns:
# The input df with appended soil water properties calculated by the designated
# method.
#*******************************************************************************
# Audit Log:
# 17/10/2024: Created script.
#*******************************************************************************

f_calc_soil_water_properties <- function(df,method,scope,
                                         soilwater_set=NA) {
  
  ## for testing:
  # df <- all_fields_df
  # method <- "SaxtonRawls2006"
  # scope <- "partial"
  # soilwater_set <- "source"
  
  # define which columns to use for soil water properties
  if(!is.na(soilwater_set)) {               
    switch(soilwater_set,
           "source"={
             fc <- "field_capacity"
             pwp <- "permanent_wilting_point"
             sat <- "saturation"
           },
           "sr"={
             fc <- "sr_DUL"
             pwp <- "sr_LL15"
             sat <- "sr_SAT"
           }
    ) # end switch-soilwater_set
  }
  
  if(method %in% c("SaxtonRawls2006")) {
    
    if(scope=="full") {
      switch(method,
             "SaxtonRawls2006"={
               modified_df1 <- df %>%
                 mutate(
                   # check if certain column exist, or create with defaults
                   particle_density_gcm3 = if("particle_density_gcm3" %in% names(.)) .[["particle_density_gcm3"]] else 2.65,
                   # hydrological, from Saxton and Rawls (2006)
                   sand_frac = sand_pct/100,
                   silt_frac = silt_pct/100,
                   clay_frac = clay_pct/100,
                   OM_frac = organic_carbon_pct*2.0/100, # organic matter fraction
                   sr_O1500t = -0.024*sand_frac + 0.487*clay_frac + 0.006*OM_frac +
                     0.005*sand_frac*OM_frac - 0.013*clay_frac*OM_frac +
                     0.068*sand_frac*clay_frac + 0.031, # helper equation to LL15
                   sr_LL15 = sr_O1500t + (0.14 * sr_O1500t - 0.02), # permanent wilting point, %
                   sr_O33t = -0.251*sand_frac + 0.195*clay_frac + 0.011*OM_frac +
                     0.006*sand_frac*OM_frac - 0.027*clay_frac*OM_frac +
                     0.452*sand_frac*clay_frac + 0.299, # helper equation to DUL
                   sr_DUL = sr_O33t + (1.283*sr_O33t^2 - 0.374*sr_O33t - 0.015), # field capacity, %
                   sr_DUL_0_33t = 0.278*sand_frac + 0.034*clay_frac + 0.022*OM_frac -
                     0.018*sand_frac*OM_frac - 0.027*clay_frac*OM_frac - 
                     0.584*sand_frac*clay_frac + 0.078, # helper equation to DUL_0_33
                   sr_DUL_0_33 = sr_DUL_0_33t + (0.646*sr_DUL_0_33t - 0.107), # soil water content at sat to fc
                   sr_SAT = sr_DUL + sr_DUL_0_33 - 0.097*sand_frac + 0.043, # moisture at saturation, %
                   ## use water-flow properties from S&R calculations
                   sr_B = (log(1500) - log(33))/(log(sr_DUL) - log(sr_LL15)), # moisture-tension coefficient
                   sr_lamda = 1/sr_B, # slope of tension-moisture curve
                   sr_Ks = 1930*(sr_SAT-sr_DUL)^(3-sr_lamda), # saturated conductivity (mm h-1)
                   sr_Ks_mmday = sr_Ks*24,
                   sr_Ks_cmsec = sr_Ks/10/60/60,
                   sr_Ks_cmhr = sr_Ks/10,
                   sr_KS_cmmin = sr_Ks_cmhr/60,
                   sr_pwp_dm3m3 = sr_LL15*1000,
                   sr_fc_dm3m3 = sr_DUL*1000,
                   sr_pwp_mmm3 = sr_LL15*1000000000*0.001,
                   sr_fc_mmm3 = sr_DUL*1000000000*0.001
                 ) # end mutate
             } # end SaxtonRawls2006
      ) # end switch-method
      
    } else if(scope=="partial") {
      switch(method,
             "SaxtonRawls2006"={
               modified_df <- as.data.frame(df) %>%
                 mutate(
                   # check if exists, or create with defaults
                   particle_density_gcm3 = if("particle_density_gcm3" %in% names(.)) .[["particle_density_gcm3"]] else 2.65,
                   ## hydrological, from Saxton and Rawls (2006)
                   sand_frac = sand_pct/100,
                   silt_frac = silt_pct/100,
                   clay_frac = clay_pct/100,
                   OM_frac = organic_carbon_pct*2.0/100, # organic matter fraction
                   sr_O1500t = -0.024*sand_frac + 0.487*clay_frac + 0.006*OM_frac +
                     0.005*sand_frac*OM_frac - 0.013*clay_frac*OM_frac +
                     0.068*sand_frac*clay_frac + 0.031, # helper equation to LL15
                   # check if dynamic column names exist and use them, else create with calculations
                   ## this is split into segments to make sure that subsequent calls to each 
                   ## dynamic column is accessing the updated column, not the original data
                   ## in df
                   !!pwp := if(pwp %in% names(.)) {
                     if_else(!is.na(.[[pwp]]), .[[pwp]], (sr_O1500t + (0.14 * sr_O1500t - 0.02)))
                   } else (sr_O1500t + (0.14 * sr_O1500t - 0.02))) %>%
                 mutate(
                   sr_O33t = -0.251*sand_frac + 0.195*clay_frac + 0.011*OM_frac +
                     0.006*sand_frac*OM_frac - 0.027*clay_frac*OM_frac +
                     0.452*sand_frac*clay_frac + 0.299, # helper equation to DUL
                   !!fc := if(fc %in% names(.)) {
                     if_else(!is.na(.[[fc]]), .[[fc]], (sr_O33t + (1.283*sr_O33t^2 - 0.374*sr_O33t - 0.015)))
                   } else (sr_O33t + (1.283*sr_O33t^2 - 0.374*sr_O33t - 0.015))) %>%
                 mutate(
                   sr_DUL_0_33t = 0.278*sand_frac + 0.034*clay_frac + 0.022*OM_frac -
                     0.018*sand_frac*OM_frac - 0.027*clay_frac*OM_frac - 
                     0.584*sand_frac*clay_frac + 0.078, # helper equation to DUL_0_33
                   sr_DUL_0_33 = sr_DUL_0_33t + (0.646*sr_DUL_0_33t - 0.107), # soil water content at sat to fc
                   !!sat := if(sat %in% names(.)) {
                     if_else(!is.na(.[[sat]]), .[[sat]], (.[[fc]] + sr_DUL_0_33 - 0.097*sand_frac + 0.043)) 
                   } else (.[[fc]] + sr_DUL_0_33 - 0.097*sand_frac + 0.043)) %>%
                 mutate(
                   ## use water-flow properties from S&R calculations
                   sr_B = (log(1500) - log(33))/(log(.[[fc]]) - log(.[[pwp]])), # moisture-tension coefficient
                   sr_lamda = 1/sr_B, # slope of tension-moisture curve
                   sr_Ks = 1930*(.[[sat]]-.[[fc]])^(3-sr_lamda), # saturated conductivity (mm h-1)
                   sr_Ks_mmday = sr_Ks*24,
                   sr_Ks_cmsec = sr_Ks/10/60/60,
                   sr_Ks_cmhr = sr_Ks/10,
                   sr_KS_cmmin = sr_Ks_cmhr/60,
                   sr_pwp_dm3m3 = get(pwp)*1000,
                   sr_fc_dm3m3 = get(fc)*1000,
                   sr_pwp_mmm3 = get(pwp)*1000000000*0.001,
                   sr_fc_mmm3 = get(fc)*1000000000*0.001
                 ) # end mutate
             } # end SaxtonRawls2006
      ) # end switch
      
    } else {
      
      stop(paste0("Unknown scope in f_calc_soil_water_properties: ,",scope))
      
    } # end if-scope
    
  } else {
    
    warning(paste0("Unrecognized method in f_calc_soil_water_properties: ",method,". No changes made to data frame."))
    modified_df <- df
    
  } # end if-check method values
  
  return(modified_df)
  
} # end function