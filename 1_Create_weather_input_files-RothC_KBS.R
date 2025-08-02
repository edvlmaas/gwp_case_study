#######################################
# Script: 1_Create_weather_input_files-RothC_KBS.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: This procedure generates weather input files for RothC in the 
# monthly format needed. Also calculates the open pan evaporation via 
# Thornthwaite and Mather (1955) potential evapotranspiration.
#######################################

print(paste0("Starting 1_Create_weather_input_files-RothC_",site_name,".R"))

source("Monthly_UPET_Correct.R")
rothc_weather_path <- paste0(rothc_path,"weather/")
  
if(clim_scenario_num==1) {
  
  # create initial 28-year average for spin-up
  Monthly_init <- Hist_site_mon[Hist_site_mon$year %in% 1950:1977, 
                                c("month","PRCP","Calc_TAVG","TM_OPE")]
  
  WeatherInit <- Monthly_init[,c("month","PRCP","Calc_TAVG","TM_OPE")] %>%
    group_by(month) %>%
    summarize(PRCP = round(mean(PRCP, na.rm=TRUE),2), Calc_TAVG = round(mean(Calc_TAVG, na.rm=TRUE),2),
              OpenPanEvap = round(mean(TM_OPE, na.rm=TRUE),2))
  
  
  ###########################
  ## Equilibrium
  
  RothC_equil <- "Eqil.dat"
  weather_path <- paste(rothc_weather_path,RothC_equil, sep="")
  # create initialization weather files (to equilibrium at land conversion) - 28-year average from 1950-1977
  cat(paste("'MI-Initialization, NOAA average weather 1950-1977; with weighted average clay and sampling depth'",sep=""),
      file=weather_path, sep="\n",append=FALSE)
  write.table(WeatherInit[,c(3,2,4)], file=weather_path, col.names=FALSE, row.names=FALSE, sep="\t", 
              quote=FALSE, append=TRUE)
  cat(clay_content,soil_depth_cm, file=weather_path, sep="\t", append=TRUE)
  
  
  ###########################
  ## Experimental and Future
  
  
  # Aggregate daily KBS weather to monthly
  OPEMonthly_sub_fut <- new_dat_fut[,c("month","year","rain_mm.x","tavg")]
  #
  OPEMonthly_fut <- OPEMonthly_sub_fut %>%
    group_by(year,month) %>% 
    summarize(PRCP = round(sum(rain_mm.x, na.rm=TRUE),2), 
              Calc_TAVG = round(mean(tavg, na.rm=TRUE),2)
    )
  colnames(OPEMonthly_fut) <- c("year","month","PRCP","Calc_TAVG")
  
  # Calculate T&M and reduce by 13%
  ## Add monthly head index
  OPEMonthly_fut$Hm <- ifelse(OPEMonthly_fut$Calc_TAVG<=0,0,(0.2*OPEMonthly_fut$Calc_TAVG)^1.514)
  ## Add annual heat index
  OPEAnnHIdx_sub_fut <- OPEMonthly_fut[,c("year","Hm")]
  OPEAnnHIdx_fut <- OPEAnnHIdx_sub_fut %>%
    group_by(year) %>%
    summarize(Ha = round(sum(Hm,na.rm=TRUE)),2)
  OPEMonthly_fut <- left_join(OPEMonthly_fut,OPEAnnHIdx_fut,by="year")
  ## Add "a" constant
  OPEMonthly_fut$a <- (0.000000675*OPEMonthly_fut$Ha^3) - 
    (0.0000771*OPEMonthly_fut$Ha^2) + 
    (0.01792*OPEMonthly_fut$Ha) + 0.49239
  ## Add raw UPET
  OPEMonthly_fut$UPETraw <- ifelse(OPEMonthly_fut$Calc_TAVG<=0,0,
                                    ifelse(OPEMonthly_fut$Calc_TAVG>0 & OPEMonthly_fut$Calc_TAVG<27,
                                           (0.53*((10*(OPEMonthly_fut$Calc_TAVG/OPEMonthly_fut$Ha))^OPEMonthly_fut$a)),
                                           (-0.015*OPEMonthly_fut$Calc_TAVG^2 + 1.093 - 14.208)))
  OPEMonthly_fut$TM_PET <- Monthly_UPET_Correct(OPEMonthly_fut$UPETraw,OPEMonthly_fut$month)
  
  OPEMonthly_fut$TM_OPE <- round(OPEMonthly_fut$TM_PET/0.75,1)
  
  # # downscale 13% for "observed" OPE 
  # OPEMonthly_fut$TM_OPE_down <- OPEMonthly_fut$TM_OPE*(1-(13/100))
  
  
  ###########################
  ## Write annual files
  
  # Catenate historicalexperimental, and future baseline data together
  base_weather <- rbind(Hist_site_mon[Hist_site_mon$year<experiment_start_year,
                                      c("month","year","Calc_TAVG","PRCP","TM_OPE")],
                        OPEMonthly_fut[,c("month","year","Calc_TAVG","PRCP","TM_OPE")])
  
  # split Hist_site_mon into a list of monthly records indexed by year through 1988
  lst <- split(base_weather,base_weather$year)

  ## create annual RothC weather files with initial text lines
  lapply(names(lst),
         function(myfun, lst) {cat(paste(site_name,", NOAA weather year ",myfun,"; with weighted average clay and sampling depth'",sep=""),
                               file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""), sep="\n",append=FALSE)
           write.table(lst[[myfun]], file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""),
                       col.names=FALSE, row.names=FALSE, sep="\t", 
                       quote=FALSE, append=TRUE)
           cat(clay_content,soil_depth_cm, file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""), 
               sep="\t", append=TRUE)
         },
         lst)
  
} else if(clim_scenario_num>1) {
  
  fut_dat <- read.csv(file=paste0(fut_weather_path,"fut_clim_scenario_",clim_scenario_num,'_reanal.csv'))
  
  # bind experimental and future data together
  exp_dat <- new_dat[,c("month","year","rain_mm.x","tavg")]
  colnames(exp_dat) <- c("month","year","rain_mm","tavg")
  OPE_all <- rbind(exp_dat,fut_dat[,c("month","year","rain_mm","tavg")])
  
  # Aggregate daily weather to monthly
  OPEMonthly_sub_esm <- OPE_all
  #
  OPEMonthly_esm <- OPEMonthly_sub_esm %>%
    group_by(year,month) %>% 
    summarize(PRCP = round(sum(rain_mm, na.rm=TRUE),2), 
              Calc_TAVG = round(mean(tavg, na.rm=TRUE),2)
    )
  colnames(OPEMonthly_esm) <- c("year","month","PRCP","Calc_TAVG")
  
  # Calculate T&M and reduce by 13%
  ## Add monthly head index
  OPEMonthly_esm$Hm <- ifelse(OPEMonthly_esm$Calc_TAVG<=0,0,(0.2*OPEMonthly_esm$Calc_TAVG)^1.514)
  ## Add annual heat index
  OPEAnnHIdx_sub_esm <- OPEMonthly_esm[,c("year","Hm")]
  OPEAnnHIdx_esm <- OPEAnnHIdx_sub_esm %>%
    group_by(year) %>%
    summarize(Ha = round(sum(Hm,na.rm=TRUE)),2)
  OPEMonthly_esm <- left_join(OPEMonthly_esm,OPEAnnHIdx_esm,by="year")
  ## Add "a" constant
  OPEMonthly_esm$a <- (0.000000675*OPEMonthly_esm$Ha^3) - 
    (0.0000771*OPEMonthly_esm$Ha^2) + 
    (0.01792*OPEMonthly_esm$Ha) + 0.49239
  ## Add raw UPET
  OPEMonthly_esm$UPETraw <- ifelse(OPEMonthly_esm$Calc_TAVG<=0,0,
                                    ifelse(OPEMonthly_esm$Calc_TAVG>0 & OPEMonthly_esm$Calc_TAVG<27,
                                           (0.53*((10*(OPEMonthly_esm$Calc_TAVG/OPEMonthly_esm$Ha))^OPEMonthly_esm$a)),
                                           (-0.015*OPEMonthly_esm$Calc_TAVG^2 + 1.093 - 14.208)))
  OPEMonthly_esm$TM_PET <- Monthly_UPET_Correct(OPEMonthly_esm$UPETraw,OPEMonthly_esm$month)
  
  OPEMonthly_esm$TM_OPE <- round(OPEMonthly_esm$TM_PET/0.75,1)
  
  # # downscale 13% for "observed" OPE 
  # OPEMonthly_esm$TM_OPE_down <- OPEMonthly_esm$TM_OPE*(1-(13/100))
  
  
  ###########################
  ## Write annual files
  
  # Catenate historical, experimental, and future baseline data together
  base_weather <- rbind(Hist_site_mon[Hist_site_mon$year<experiment_start_year,
                                      c("month","year","Calc_TAVG","PRCP","TM_OPE")],
                        OPEMonthly_esm[,c("month","year","Calc_TAVG","PRCP","TM_OPE")])
  
  # split Hist_site_mon into a list of monthly records indexed by year through 1988
  lst <- split(base_weather,base_weather$year)
  
  ## create annual RothC weather files with initial text lines
  lapply(names(lst),
         function(myfun, lst) {cat(paste(site_name,", NOAA weather year ",myfun,
                                         "; with weighted average clay and sampling depth'",sep=""),
                               file=paste(rothc_weather_path,clim_scenario_num,
                                          substr(myfun,2,4),".dat", sep=""), sep="\n",append=FALSE)
           write.table(lst[[myfun]], file=paste(rothc_weather_path,clim_scenario_num,
                                                substr(myfun,2,4),".dat", sep=""),
                       col.names=FALSE, row.names=FALSE, sep="\t", 
                       quote=FALSE, append=TRUE)
           cat(clay_content,soil_depth_cm, file=paste(rothc_weather_path,clim_scenario_num,
                                     substr(myfun,2,4),".dat", sep=""), 
               sep="\t", append=TRUE)
         },
         lst)

}# if clim_scenario_num

if(clim_scenario_num==1) {
rm(list=c("Monthly_init","WeatherInit","RothC_equil","weather_path","base_weather","lst",
          "OPEMonthly_sub_fut","OPEMonthly_fut","OPEAnnHIdx_sub_fut","OPEAnnHIdx_fut"))
} else if(clim_scenario_num>1) {
  rm(list=c("fut_dat","exp_dat","OPE_all","OPEMonthly_sub_esm","OPEMonthly_esm",
            "OPEAnnHIdx_sub_esm","OPEAnnHIdx_esm","base_weather","lst"))
}
