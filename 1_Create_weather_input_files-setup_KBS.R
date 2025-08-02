#######################################
# Script: 1_Create_weather_input_files-setup_KBS.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: This procedure generates basic weather dataframes as the basis
# for each model to format to its own requirements. Includes past and current 
# weather from weather stations and nasapower, and future projected climate via 
# CMIP6 earth system models. 
# There are some gaps of missing data from past and current weather, so 
# it fills in those values with NASA Power data (which includes radiation data). 
# Order of preference for data:
#  1) site data
#  2) NASA POWER
#  3) average of everything else 
#######################################

print(paste0("Starting 1_Create_weather_input_files-setup_",site_name,".R"))

library(lubridate)
library(tidyverse)
library(apsimx)



###########################
## import local data
###########################

# import historical data for spin-up-to-equilibrium period for Daycent and RothC
Hist_site <- read_csv(paste0(wth_path,hist_wth_filename),
                      show_col_types = FALSE) %>%
  mutate(TMAX=round(TMAX,1),
         date=as.Date(Date,format="%m/%d/%Y"),
         day=Day,
         month=Month,
         year=Year,
         dayofyear=yday(date),
         prec_cm=PRCP/10)

Hist_site_mon <- read_csv(paste0(wth_path,hist_wth_mon_filename),
                          show_col_types = F)  %>%
  mutate(month=Month,
         year=Year)

# import observed data at or near site for experimental period
Raw_site <- read_csv(paste0(wth_path,curr_local_wth_filename),
                     skip=45, col_names = TRUE, col_types = c('c','d','c','d','c','d','c','d','c','d',
                                                              'c','d','c','d','c','d','c','d','c','d',
                                                              'c','d','c','d','d','d'))
#save_probs <- problems()

## Only take complete years
temp_site <- Raw_site[Raw_site$Year %in% experiment_year_range,]

## Reverse rows so oldest data is first
Clean_site <- temp_site[order(nrow(temp_site):1),] %>%
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         year=Year, # Split out date into day, month, year
         month=month(date),
         day=day(date),
         dayofyear=yday(date), # day of the year
         radn_MJm2 = Solar_Radiation*(60*60*24)/1000000, # convert from watts/m^2 to MJ/m^2 
         radn_Ld = Solar_Radiation/0.484583, # convert from watts/m^2 to Langley/day
         radn_Wm2 = Solar_Radiation,
         maxt_C = air_temp_max, # deg C
         mint_C = Air_Temp_Min, # deg C
         rain_mm = precipitation, # mm
         rain_cm = precipitation/10 # cm
  ) 


# for RothC
soil_depth_cm <- 25 
clay_content <- 19


###########################
## import fill-in NASA
###########################

# Generate NASA POWER data, then import and clean

source("Shared/nasapower_download.R") #creates function locally
nasapower_download(path = wth_path, # where to put the data
                   filename = nasapower_output_filename, # what to call the file it generates
                   location = c(longitude, latitude),
                   start_end_date = c(experiment_start_date, experiment_end_date))

Raw_NASA <- read_csv(paste0(wth_path,"/",nasapower_output_filename), 
                     col_names = T, show_col_types = F)
Clean_NASA <- Raw_NASA


###########################
## merge data
###########################

# order of preference for data:
#  1) site data
#  2) NASA POWER
#  3) average of everything else 

## create a full list of dates in the range of the data
full_data <- seq(min(Clean_site$date),max(Clean_site$date),by = "1 day")
full_data  <- data.frame(date=full_data)
# ## create a new matrix that includes all DATEs in full_data and all data in gull_lake
new <- merge(Clean_site,
             full_data, 
             by="date", 
             all=TRUE)
new$year <- ifelse(is.na(new$Year),
                   lubridate::year(new$date),
                   new$Year)
new$month <- ifelse(is.na(new$month),
                    lubridate::month(new$date),
                    new$month)
new$day <- ifelse(is.na(new$day),
                  lubridate::day(new$date),
                  new$day)
new$dayofyear <- yday(new$date)

# join NASA, and fill in missing data
#new_dat <- left_join(Clean_KBS[,27:36],Clean_NASA,by=c("year","month","day","dayofyear"))
new_dat <- left_join(new[,27:ncol(new)],Clean_NASA,by=c("year","month","day","dayofyear"))
new_dat[is.na(new_dat$radn_MJm2.x),5] <- new_dat[is.na(new_dat$radn_MJm2.x),12]
new_dat[is.na(new_dat$radn_Ld.x),6] <- new_dat[is.na(new_dat$radn_Ld.x),13]
new_dat[is.na(new_dat$radn_Wm2.x),7] <- new_dat[is.na(new_dat$radn_Wm2.x),14]
new_dat[is.na(new_dat$maxt_C.x),8] <- new_dat[is.na(new_dat$maxt_C.x),15]
new_dat[is.na(new_dat$mint_C.x),9] <- new_dat[is.na(new_dat$mint_C.x),16]
new_dat[is.na(new_dat$rain_mm.x),10] <- new_dat[is.na(new_dat$rain_mm.x),17]
new_dat[is.na(new_dat$rain_cm.x),11] <- new_dat[is.na(new_dat$rain_cm.x),18]

# calculate daily average temp
new_dat$tavg <- (new_dat$maxt_C.x + new_dat$mint_C.x)/2

# find any columns with NA cells
na_find_col <- names(which(colSums(is.na(new_dat))>0))
na_find_row <- new[is.na(new$year),]

# write out compiled data
write.table(new_dat, file=paste0(wth_path,"compiled_historical_weather.csv"),
            row.names=F, quote=F, col.names=T, sep=',')


#**********************************************************************
##### Future weather
#**********************************************************************

###########################
## build baseline-through-future data
###########################


new_dat_fut <- new_dat
weather_28yr <- new_dat[new_dat$year %in% 1994:2021,]

for (i in 1:3) {
  weather_28yr$year <- weather_28yr$year+28
  new_dat_fut <- rbind(new_dat_fut, weather_28yr)
}

# write out compiled data
write.table(new_dat_fut, file=paste0(wth_path,"compiled_future_weather.csv"),
            row.names=F, quote=F, col.names=T, sep=',')

