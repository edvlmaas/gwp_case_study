#######################################
# File: 9_Results_Daycent-setup_LRF.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Imports Daycent output and sets up data frames
# with results used in Daycent's "9_" series scripts and
# some "10_" series scripts.
#######################################

print(paste0("Starting 9_Results_Daycent-setup_",site_name,".R"))

library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)
library(broom)


#**********************************************************************

# import Daycent modeled points -------------------------------------------


### most output files (*.out) are limited in time to the specific phase
### they are run from, so they need to be concatenated together in order
### to have the full range of results in one place


### harvest 

Day_base_harvest <- read_csv(paste0(daycent_path,paste0("harvest_base.csv")),
                             col_names = TRUE, show_col_types = F)
Day_exp_harvest <- read_csv(paste0(daycent_path,paste0("harvest_exp_",scenario_name,".csv")),
                            col_names = TRUE, show_col_types = F)
Day_fut_harvest <- read_csv(paste0(daycent_path,paste0("harvest_fut_",scenario_name,".csv")),
                            col_names = TRUE, show_col_types = F)

Day_harvest_raw <- rbind(Day_base_harvest,Day_exp_harvest,Day_fut_harvest) %>%
  mutate(year=floor(time),
         cn_grain_ratio=cgrain/`egrain(N)`,
         cn_stover_ratio=cstraw/`estraw(N)`)

#limit to future scenario time period
Day_harvest <- Day_harvest_raw[Day_harvest_raw$year <= end_fut_period_year,]

##############  NOTE #############
#there is a leap-year issue with soiln_base.out which incorrectly
#reports leap years from 1942-2002 (should be 1940-2000). if use "date" to
#select by, will include 2002 day 366, which translates to 2003-01-01
#in addition to the actual 2003-01-01. this appears to be an issue with
#.out files in base period. oddly, the _exp extended files are fine.
##################################

## soil temperature

Day_base_soiltavg <- read.fwf(paste0(daycent_path,paste0("soiltavg_base.out")),
                              widths=c(12,5,8,8,8,8,8,8,8,6),
                              col.names=c("time","dayofyear","layer1","layer2","layer3",
                                          "layer4","layer5","layer6","layer7","layer8"),
                              colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                           "numeric","numeric","numeric","numeric","numeric"))
Day_exp_soiltavg <- read.fwf(paste0(daycent_path,paste0("soiltavg_exp_",scenario_name,".out")),
                             widths=c(12,5,8,8,8,8,8,8,8,6), 
                             col.names=c("time","dayofyear","layer1","layer2","layer3",
                                         "layer4","layer5","layer6","layer7","layer8"),
                             colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric"))
Day_fut_soiltavg <- read.fwf(paste0(daycent_path,paste0("soiltavg_fut_",scenario_name,".out")),
                             widths=c(12,5,8,8,8,8,8,8,8,6), 
                             col.names=c("time","dayofyear","layer1","layer2","layer3",
                                         "layer4","layer5","layer6","layer7","layer8"),
                             colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric"))

DayT_C_raw <- rbind(Day_exp_soiltavg,Day_fut_soiltavg) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)
DayT_C_raw <- DayT_C_raw %>%
         mutate(mean_3_4=round(rowMeans(DayT_C_raw[,c("layer3","layer4")]),2),
                mean_15cm=round(((layer1*2)+(layer2*3)+(layer3*5)+(layer4*5))/15,2), # weighted average
                mean_10cm=round(((layer1*2)+(layer2*3)+(layer3*5))/10,2)
         )

DayT_C <- DayT_C_raw[DayT_C_raw$year <= end_fut_period_year,]

DayT_C_range <- range(DayT_C[DayT_C$date %in% ObsTemp$date, "mean_3_4"],na.rm=T)

# additional version including base phase data
DayT_C_all_raw <- rbind(Day_base_soiltavg,Day_exp_soiltavg,Day_fut_soiltavg) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)
DayT_C_all_raw <- DayT_C_all_raw %>%
  mutate(mean_3_4=round(rowMeans(DayT_C_all_raw[,c("layer3","layer4")]),2))


DayT_C_all <- DayT_C_all_raw[DayT_C_all_raw$year <= end_fut_period_year,]


## soil moisture

Day_base_vswc <- read.fwf(paste0(daycent_path,paste0("vswc_base.out")),
                          widths=c(10,7,10,10,10,10,10,10,10,6),
                          col.names=c("time","dayofyear","layer1","layer2","layer3",
                                      "layer4","layer5","layer6","layer7","layer8"),
                          colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric"))
Day_exp_vswc <- read.fwf(paste0(daycent_path,paste0("vswc_exp_",scenario_name,".out")),
                         widths=c(10,7,10,10,10,10,10,10,10,6), 
                         col.names=c("time","dayofyear","layer1","layer2","layer3",
                                     "layer4","layer5","layer6","layer7","layer8"),
                         colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric","numeric")) 
Day_fut_vswc <- read.fwf(paste0(daycent_path,paste0("vswc_fut_",scenario_name,".out")),
                         widths=c(10,7,10,10,10,10,10,10,10,6), 
                         col.names=c("time","dayofyear","layer1","layer2","layer3",
                                     "layer4","layer5","layer6","layer7","layer8"),
                         colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric","numeric")) 

DayM_V_raw <- rbind(Day_exp_vswc,Day_fut_vswc) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         layer1_pct=layer1*100,
         layer2_pct=layer2*100,
         layer3_pct=layer3*100,
         layer4_pct=layer4*100,
         layer5_pct=layer5*100,
         layer6_pct=layer6*100,
         layer7_pct=layer7*100,
         layer8_pct=layer8*100
  )
DayM_V_raw$mean_5cm=rowMeans(DayM_V_raw[,c("layer1_pct","layer2_pct")])

DayM_V <- DayM_V_raw[DayM_V_raw$year <= end_fut_period_year,] %>%
  mutate(SW_10cm=((layer1*2)+(layer2*3)+(layer3*5))/10, # weighted average
         DW_2cm=layer1*2, # mult by layer depth (cm)
         DW_5cm=layer2*3,
         DW_10cm=layer3*5,
         DW_20cm=layer4*10,
         DW_30cm=layer5*10,
         DW_45cm=layer6*15,
         DW_60cm=layer7*15,
         DW_0to10cm=(layer1*2)+(layer2*3)+(layer3*5),
         DW_0to60cm=(layer1*2)+(layer2*3)+(layer3*5)+(layer4*10)+(layer5*10)+
           (layer6*15)+(layer7*15)
  )

DayM_V_all_raw <- rbind(Day_base_vswc,Day_exp_vswc,Day_fut_vswc) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         layer1_pct=layer1*100,
         layer2_pct=layer2*100,
         layer3_pct=layer3*100,
         layer4_pct=layer4*100,
         layer5_pct=layer5*100,
         layer6_pct=layer6*100,
         layer7_pct=layer7*100,
         layer8_pct=layer8*100)

DayM_V_all <- DayM_V_all_raw[DayM_V_all_raw$year <= end_fut_period_year,]


## N2O and CH4 emissions

Day_exp_methane <- read.fwf(paste0(daycent_path,paste0("methane_exp_",scenario_name,".out")),
                            widths=c(4,6,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                     12,12,12,12,12,12),
                            col.names=c("year","DOY","aglivc","bglivcj","bglivcm",
                                        "prev_mcprd1","prev_mcprd2","prev_mcprd3",
                                        "COM","ppt","irri","watr2sat","avgst_10cm",
                                        "TI","Cr","Eh","Feh","CH4_prod","CH4_Ep",
                                        "CH4_Ebl","CH4_oxid")
                            ,skip=1)%>%
  mutate(date=as.Date(DOY,origin=paste0(as.character(year),"-01-01"))-1)

###############################################################################
####### The following block is a work-around for odd behavior in no-till where
####### future methane oxidation suddenly increases more than double in all
####### climate scenarios, including the baseline.
###############################################################################
# calculate what 29-year past total was, then reduce each day by a fraction
# until the sum is the same for the future 29 years
if(mgmt_scenario_num %in% c(54,55,56,8)) {
  num_years <- end_fut_period_year - end_exp_period_year
  start_exp <- end_exp_period_year - num_years
  sum_exp_oxid <- sum(Day_exp_methane[Day_exp_methane$year>=start_exp,"CH4_oxid"])

  Day_fut_methane <- read.fwf(paste0(daycent_path,paste0("methane_fut_",scenario_name,".out")),
                              widths=c(4,6,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                       12,12,12,12,12,12),
                              col.names=c("year","DOY","aglivc","bglivcj","bglivcm",
                                          "prev_mcprd1","prev_mcprd2","prev_mcprd3",
                                          "COM","ppt","irri","watr2sat","avgst_10cm",
                                          "TI","Cr","Eh","Feh","CH4_prod","CH4_Ep",
                                          "CH4_Ebl","CH4_oxid")
                              ,skip=1) %>%
    mutate(date=as.Date(DOY,origin=paste0(as.character(year),"-01-01"))-1,
           CH4_oxid=CH4_oxid*0.465)

  sum_fut_oxid <- sum(Day_fut_methane[Day_fut_methane$year < min(Day_fut_methane$year)+num_years-1,
                                      "CH4_oxid"])


} else {
  
  Day_fut_methane <- read.fwf(paste0(daycent_path,paste0("methane_fut_",scenario_name,".out")),
                              widths=c(4,6,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                       12,12,12,12,12,12),
                              col.names=c("year","DOY","aglivc","bglivcj","bglivcm",
                                          "prev_mcprd1","prev_mcprd2","prev_mcprd3",
                                          "COM","ppt","irri","watr2sat","avgst_10cm",
                                          "TI","Cr","Eh","Feh","CH4_prod","CH4_Ep",
                                          "CH4_Ebl","CH4_oxid")
                              ,skip=1) %>%
    mutate(date=as.Date(DOY,origin=paste0(as.character(year),"-01-01"))-1)
  
  
}


Day_methane_raw <- rbind(Day_exp_methane[,c("date","year","DOY","CH4_Ep","CH4_Ebl","CH4_oxid")],
                     Day_fut_methane[,c("date","year","DOY","CH4_Ep","CH4_Ebl","CH4_oxid")]) %>%
  mutate(CH4_emis_gCmd=CH4_Ep+CH4_Ebl,
         CH4_emis_gChad=CH4_emis_gCmd*10000,
         dayofyear=DOY)

Day_methane <- Day_methane_raw[Day_methane_raw$year <= end_fut_period_year,]


################################################################################


# ###############################################################################
# ####### The following block is a work-around for odd behavior in no-till where
# ####### future methane oxidation suddenly increases more than double in all
# ####### climate scenarios, including the baseline. 
# ###############################################################################
# calculate what 29-year past total was, then reduce each day by a fraction
# until the sum is the same for the future 29 years
if(mgmt_scenario_num %in% c(54,55,56,8)) {
  num_years <- end_fut_period_year - end_exp_period_year
  start_exp <- end_exp_period_year - num_years
  Day_exp_summary <- read.fwf(paste0(daycent_path,paste0("summary_exp_",scenario_name,".out")),
                              widths=c(10,5,9,9,9,13,13,13,13,13),
                              col.names=c("time","dayofyear","tmax","tmin","ppt",
                                          "N2O_gNhad","NOflux","CH4_oxid_gChad","NIT","CO2resp"),
                              skip=1) %>%
    mutate(year=floor(time),
           CH4_oxid_gChad=CH4_oxid_gChad*0.465)
  sum_exp_oxid_sum <- sum(Day_exp_summary[Day_exp_summary$year>=start_exp,"CH4_oxid_gChad"])


  Day_fut_summary <- read.fwf(paste0(daycent_path,paste0("summary_fut_",scenario_name,".out")),
                              widths=c(10,5,9,9,9,13,13,13,13,13),
                              col.names=c("time","dayofyear","tmax","tmin","ppt",
                                          "N2O_gNhad","NOflux","CH4_oxid_gChad","NIT","CO2resp"),
                              skip=1) %>%
    mutate(year=floor(time),
           CH4_oxid_gChad=CH4_oxid_gChad*0.465)

  sum_fut_oxid_sum <- sum(Day_fut_summary[Day_fut_summary$year < min(Day_fut_summary$year)+num_years-1,
                                          "CH4_oxid_gChad"])


} else {
  
  Day_exp_summary <- read.fwf(paste0(daycent_path,paste0("summary_exp_",scenario_name,".out")),
                              widths=c(10,5,9,9,9,13,13,13,13,13),
                              col.names=c("time","dayofyear","tmax","tmin","ppt",
                                          "N2O_gNhad","NOflux","CH4_oxid_gChad","NIT","CO2resp"),
                              skip=1) 

    Day_fut_summary <- read.fwf(paste0(daycent_path,paste0("summary_fut_",scenario_name,".out")),
                              widths=c(10,5,9,9,9,13,13,13,13,13),
                              col.names=c("time","dayofyear","tmax","tmin","ppt",
                                          "N2O_gNhad","NOflux","CH4_oxid_gChad","NIT","CO2resp"),
                              skip=1) 
  
}

###############################################################################




#Day_summary <- rbind(Day_base_summary,Day_exp_summary,Day_fut_summary)
Day_summary_raw <- rbind(Day_exp_summary,Day_fut_summary) %>%
  mutate(year=floor(time)) %>%
  merge(Day_methane, by=c("year","dayofyear")) %>%
  mutate(CH4_net_gChad = -(CH4_oxid_gChad)* 0.2) %>% # make it negative
   # mutate(CH4_net_gChad=CH4_emis_gChad-CH4_oxid_gChad) %>%
  arrange(year,dayofyear)

Day_summary <- Day_summary_raw[Day_summary_raw$year <= end_fut_period_year,]
  
Day_base_soiln <- read.fwf(paste0(daycent_path,paste0("soiln_base.out")),
                           widths=c(8,6,14,14,14,14,14,14,14,14,14,14,14,14,14,14),
                           col.names=c("time","dayofyear","ammonium","NO3_ppm0","NO3_ppm1",
                                       "NO3_ppm2","NO3_ppm3","NO3_ppm4","NO3_ppm5","NO3_ppm6",
                                       "NO3_ppm7","NO3_ppm8","NO3_ppm9","NO3_ppm10",
                                       "NO3_ppm11","NO3_ppm12"),
                           skip=1) %>%
  mutate(year=floor(time),
         NO3_ppm=NO3_ppm0+NO3_ppm1+NO3_ppm2,
         NO3_10to60cm_ppm=NO3_ppm3+NO3_ppm4+NO3_ppm5+NO3_ppm6,
         NO3_kgha=((0.10*10000*ObsBD*1000)*NO3_ppm)/1000000, 
         NO3_hgha=NO3_kgha/10, 
         NO3_10to60cm_kgha=((0.50*10000*ObsBD*1000)*NO3_10to60cm_ppm)/1000000, 
         NO3_10to60cm_hgha=NO3_10to60cm_kgha/10, 
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         NO3_2cm_kgha=((0.02*10000*ObsBD*1000)*NO3_ppm0)/1000000,
         NO3_5cm_kgha=((0.03*10000*ObsBD*1000)*NO3_ppm1)/1000000,
         NO3_10cm_kgha=((0.05*10000*ObsBD*1000)*NO3_ppm2)/1000000,
         NO3_20cm_kgha=((0.1*10000*1.4*1000)*NO3_ppm3)/1000000,
         NO3_30cm_kgha=((0.1*10000*1.66*1000)*NO3_ppm4)/1000000,
         NO3_45cm_kgha=((0.15*10000*1.58*1000)*NO3_ppm5)/1000000,
         NO3_60cm_kgha=((0.15*10000*1.58*1000)*NO3_ppm6)/1000000,
         NO3_0to60cm_kgha=NO3_2cm_kgha+NO3_5cm_kgha+NO3_10cm_kgha+
           NO3_20cm_kgha+NO3_30cm_kgha+NO3_45cm_kgha+NO3_60cm_kgha)


Day_exp_soiln <- read.fwf(paste0(daycent_path,paste0("soiln_exp_",scenario_name,".out")),
                          widths=c(8,6,14,14,14,14,14,14,14,14,14,14,14,14,14,14),
                           col.names=c("time","dayofyear","ammonium","NO3_ppm0","NO3_ppm1",
                                       "NO3_ppm2","NO3_ppm3","NO3_ppm4","NO3_ppm5","NO3_ppm6",
                                       "NO3_ppm7","NO3_ppm8","NO3_ppm9","NO3_ppm10",
                                       "NO3_ppm11","NO3_ppm12"),
                           skip=1) %>%
  mutate(year=floor(time),
         NO3_ppm=NO3_ppm0+NO3_ppm1+NO3_ppm2,
         NO3_10to60cm_ppm=NO3_ppm3+NO3_ppm4+NO3_ppm5+NO3_ppm6,
         NO3_kgha=((0.10*10000*ObsBD*1000)*NO3_ppm)/1000000, 
         NO3_hgha=NO3_kgha/10, 
         NO3_10to60cm_kgha=((0.50*10000*ObsBD*1000)*NO3_10to60cm_ppm)/1000000, 
         NO3_10to60cm_hgha=NO3_10to60cm_kgha/10, 
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         NO3_2cm_kgha=((0.02*10000*ObsBD*1000)*NO3_ppm0)/1000000,
         NO3_5cm_kgha=((0.03*10000*ObsBD*1000)*NO3_ppm1)/1000000,
         NO3_10cm_kgha=((0.05*10000*ObsBD*1000)*NO3_ppm2)/1000000,
         NO3_20cm_kgha=((0.1*10000*1.4*1000)*NO3_ppm3)/1000000,
         NO3_30cm_kgha=((0.1*10000*1.66*1000)*NO3_ppm4)/1000000,
         NO3_45cm_kgha=((0.15*10000*1.58*1000)*NO3_ppm5)/1000000,
         NO3_60cm_kgha=((0.15*10000*1.58*1000)*NO3_ppm6)/1000000,
         NO3_0to60cm_kgha=NO3_2cm_kgha+NO3_5cm_kgha+NO3_10cm_kgha+
           NO3_20cm_kgha+NO3_30cm_kgha+NO3_45cm_kgha+NO3_60cm_kgha)


Day_fut_soiln <- read.fwf(paste0(daycent_path,paste0("soiln_fut_",scenario_name,".out")),
                          widths=c(8,6,14,14,14,14,14,14,14,14,14,14,14,14,14,14),
                          col.names=c("time","dayofyear","ammonium","NO3_ppm0","NO3_ppm1",
                                      "NO3_ppm2","NO3_ppm3","NO3_ppm4","NO3_ppm5","NO3_ppm6",
                                      "NO3_ppm7","NO3_ppm8","NO3_ppm9","NO3_ppm10",
                                      "NO3_ppm11","NO3_ppm12"),
                          skip=1) %>%
  mutate(year=floor(time),
         NO3_ppm=NO3_ppm0+NO3_ppm1+NO3_ppm2,
         NO3_10to60cm_ppm=NO3_ppm3+NO3_ppm4+NO3_ppm5+NO3_ppm6,
         NO3_kgha=((0.10*10000*ObsBD*1000)*NO3_ppm)/1000000, 
         NO3_hgha=NO3_kgha/10, 
         NO3_10to60cm_kgha=((0.50*10000*ObsBD*1000)*NO3_10to60cm_ppm)/1000000, 
         NO3_10to60cm_hgha=NO3_10to60cm_kgha/10, 
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         NO3_2cm_kgha=((0.02*10000*ObsBD*1000)*NO3_ppm0)/1000000,
         NO3_5cm_kgha=((0.03*10000*ObsBD*1000)*NO3_ppm1)/1000000,
         NO3_10cm_kgha=((0.05*10000*ObsBD*1000)*NO3_ppm2)/1000000,
         NO3_20cm_kgha=((0.1*10000*1.4*1000)*NO3_ppm3)/1000000,
         NO3_30cm_kgha=((0.1*10000*1.66*1000)*NO3_ppm4)/1000000,
         NO3_45cm_kgha=((0.15*10000*1.58*1000)*NO3_ppm5)/1000000,
         NO3_60cm_kgha=((0.15*10000*1.58*1000)*NO3_ppm6)/1000000,
         NO3_0to60cm_kgha=NO3_2cm_kgha+NO3_5cm_kgha+NO3_10cm_kgha+
           NO3_20cm_kgha+NO3_30cm_kgha+NO3_45cm_kgha+NO3_60cm_kgha)


Day_soiln_raw <- rbind(Day_base_soiln,Day_exp_soiln) %>%
  mutate(year=floor(time),
         NO3_ppm=NO3_ppm0+NO3_ppm1+NO3_ppm2+NO3_ppm3,
         NO3_kgha=((0.20*10000*ObsBD*1000)*NO3_ppm)/1000000, 
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)
  

Day_soiln_all <- rbind(Day_base_soiln,Day_exp_soiln,Day_fut_soiln) %>%
  mutate(year=floor(time),
         NO3_ppm=NO3_ppm0+NO3_ppm1+NO3_ppm2+NO3_ppm3,
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)

Day_soiln <- Day_soiln_all


# water-filled pore space

Day_exp_wfps <- read.fwf(paste0(daycent_path,paste0("wfps_exp_",scenario_name,".out")),
                         widths=c(8,5,9,9,9,9,9,9,9,9,9,9,9,9,9),
                         col.names=c("time","dayofyear","wfps_layer1","wfps_layer2",
                                     "wfps_layer3","wfps_layer4","wfps_layer5",
                                     "wfps_layer6","wfps_layer7","wfps_layer8",
                                     "wfps_layer9","wfps_layer10","wfps_layer11",
                                     "wfps_layer12","wfps_layer13"),
                         skip=1) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)

Day_fut_wfps <- read.fwf(paste0(daycent_path,paste0("wfps_fut_",scenario_name,".out")),
                         widths=c(8,5,9,9,9,9,9,9,9,9,9,9,9,9,9),
                         col.names=c("time","dayofyear","wfps_layer1","wfps_layer2",
                                     "wfps_layer3","wfps_layer4","wfps_layer5",
                                     "wfps_layer6","wfps_layer7","wfps_layer8",
                                     "wfps_layer9","wfps_layer10","wfps_layer11",
                                     "wfps_layer12","wfps_layer13"),
                         skip=1) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)


Day_wfps <- rbind(Day_exp_wfps,Day_fut_wfps)


#**********************************************************************


### future .lis contains all data from year 1 in equilibrium through end of future simulation
lis_output_raw <- read.table(paste0(daycent_path,paste0("sched_fut_",scenario_name,".lis")),
                         col.names = c("time","somsc_gm2","somtc","somte(1)",
                                       "crpval","cinput","somse(1)","petann",
                                       "tminrl(1)","minerl(1,1)","minerl(2,1)",
                                       "minerl(3,1)","minerl(4,1)","minerl(5,1)",
                                       "minerl(6,1)","minerl(7,1)","minerl(8,1)",
                                       "aglivc","bglivcj","bglivcm","cgrain",
                                       "crmvst","hi","clitad(1)","clitad(2)",
                                       "elitad(1,1)","elitad(2,1)"),
                         colClasses=c("numeric","numeric","numeric","numeric",
                                      "character","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric"),
                         skip=45) %>%
  mutate(year=floor(time))

lis_output <- lis_output_raw[lis_output_raw$year <= end_fut_period_year,]

## need to remove duplicate years where phases join (base-exp, exp-fut)
## and end of future simulation - unique should do it
DayC_Mgha <- unique(lis_output[!((lis_output$cinput == 0 & 
                           (lis_output$time == experiment_start_year | lis_output$time == experiment_end_year+1)) |
                          lis_output$time == end_fut_period_year),c("time","somsc_gm2","year")] %>%  
  mutate(base=round(somsc_gm2/100,1)
  )
)

# reduce C to limit to top 10 cm; Daycent provides top 20 cm
## can't just multiply by a fraction as the values end up compressed, and we just
## need everything to drop down by a set amount
reduceCby <- 4.2
DayC_Mgha$base <- DayC_Mgha$base-reduceCby



DayY_Mgha <- Day_harvest[substr(Day_harvest$crpval,2,5)!="RGA",] %>%
  select(time,cgrain,crpval) %>%
  mutate(year=floor(time),
         yield=cgrain/100/.45, #g C/m^2 converted to Mg/ha, then divided by .45 to convert C mass to yield mass
         crop=if_else(substr(crpval,2,2)=="C", "Cotton",
              if_else(substr(crpval,2,2)=="S", "Sorghum", 
              if_else(substr(crpval,2,2)=="W", "Wheat", "Unknown")))
  )

DayY_Mgha_pivwid <- pivot_wider(DayY_Mgha,names_from="crop",values_from="yield")

#Daycent doesn't output bulk density

DayGN_ghaday <- Day_summary[,c("time","dayofyear","N2O_gNhad")] %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)

DayGN_ann_gha <- DayGN_ghaday %>%
  group_by(year) %>%
  summarize(N2OEmissions_ghayr=sum(N2O_gNhad))

DayGN_cum_gha <- DayGN_ghaday[,c("year","dayofyear","date","N2O_gNhad")] %>%
  mutate(N2O_gha = cumsum(N2O_gNhad)) %>%
  select(-N2O_gNhad)

DayGN_cum_calib <- DayGN_ghaday[DayGN_ghaday$date >= experiment_start_date &
                                  DayGN_ghaday$date <= experiment_end_date,] %>%
  group_by(year) %>%
  summarize(tot_N2O_ghayr=sum(N2O_gNhad))


#DayGC_ghaday <- Day_summary_base[,c("time","dayofyear","N2Oflux")] %>%
#  mutate(year=floor(time))

DayGM_ghaday <- Day_summary[,c("time","dayofyear","CH4_net_gChad")] %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)

DayGM_ann_gha <- DayGM_ghaday %>%
  group_by(year) %>%
  summarize(CH4Emissions_ghayr=sum(CH4_net_gChad))

DayGM_cum_gha <- DayGM_ghaday[,c("year","dayofyear","date","CH4_net_gChad")] %>%
  mutate(CH4_gha = cumsum(CH4_net_gChad)) %>%
  select(-CH4_net_gChad)

DayGM_cum_calib <- DayGM_ghaday[DayGM_ghaday$date >= experiment_start_date &
                                  DayGN_ghaday$date <= experiment_end_date,] %>%
  group_by(year) %>%
  summarize(tot_CH4_ghayr=sum(CH4_net_gChad))


DayPltCN <- Day_harvest[substr(Day_harvest$crpval,2,5)!="RGA",] %>%
  select(year,crpval,cgrain,`egrain(N)`,cstraw,`estraw(N)`,
         cn_grain_ratio,cn_stover_ratio) %>%
  mutate(crop=if_else(substr(crpval,2,2)=="C", "Cotton",
              if_else(substr(crpval,2,2)=="S", "Sorghum", 
              if_else(substr(crpval,2,2)=="W", "Wheat", "Unknown")))
  )

DayCI_gm2yr <- lis_output[!((lis_output$cinput == 0 & 
                               (lis_output$time == experiment_start_year | lis_output$time == experiment_end_year+1)) |
                              lis_output$time == end_fut_period_year),c("time","clitad.2.")] %>%  
  mutate(year=floor(time),
         base=`clitad.2.`
  )
  
DayNI_gm2yr <- lis_output[!((lis_output$cinput == 0 & 
                               (lis_output$time == experiment_start_year | lis_output$time == experiment_end_year+1)) |
                              lis_output$time == end_fut_period_year),c("time","elitad.2.1.")] %>%  
  mutate(year=floor(time),
         base=`elitad.2.1.`
  )


#**********************************************************************


# merge observed and modeled data

## Sorghum yield

SorghumYld_Mgha <- merge(ObsYield[ObsYield$crop=="Sorghum",c("year","mean_yield","sd_yield")],
                         DayY_Mgha[DayY_Mgha$yield != 0 & DayY_Mgha$crop=="Sorghum",
                                     c("year","yield")],
                         by="year",
                         all=TRUE)%>%
  merge(HistY_Mgha[,c("year","sorghum_yield_mgha")],
        by="year",
        all=TRUE)
colnames(SorghumYld_Mgha) <- c("year","Observed","Obs_sd","Daycent","Historical")

SorghumYld_Mgha_piv <- pivot_longer(SorghumYld_Mgha, c(-year,-Obs_sd),
                                    names_to = "source",
                                    values_to = "yield_val")

# remove sd from modeled records; only for observed
SorghumYld_Mgha_piv <- SorghumYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))


## Cotton yield

CottonYld_Mgha <- merge(ObsYield[ObsYield$crop=="Cotton",c("year","mean_yield","sd_yield")],
                        DayY_Mgha[DayY_Mgha$yield != 0 & DayY_Mgha$crop=="Cotton",
                                    c("year","yield")],
                        by="year",
                        all=TRUE) %>%
  merge(HistY_Mgha[,c("year","cotton_yield_mgha")],
        by="year",
        all=TRUE)
colnames(CottonYld_Mgha) <- c("year","Observed","Obs_sd","Daycent","Historical")

CottonYld_Mgha_piv <- pivot_longer(CottonYld_Mgha, c(-year,-Obs_sd),
                                   names_to = "source",
                                   values_to = "yield_val")

# remove sd from modeled records; only for observed
CottonYld_Mgha_piv <- CottonYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))


## SOC

Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock","sd_cstock")],
                     DayC_Mgha[,c("year","base")],
                     by="year",
                     all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","Obs_sd","Daycent")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year,-Obs_sd),
                                 names_to = "source",
                                 values_to = "C_val")

# remove sd from modeled records; only for observed
Cstock_Mgha_piv <- Cstock_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))


## Soil Temp

SoilTemp_C <- merge(ObsTemp[,c("date","soil_temperature")],
                    DayT_C[,c("date","mean_3_4")],#DayT_C[,c("date","layer3")],
                    by="date",
                    all=TRUE)
colnames(SoilTemp_C) <- c("date","Observed","Daycent")

SoilTemp_C_piv <- pivot_longer(SoilTemp_C, c(-date),
                               names_to = "source",
                               values_to = "temp_val") %>%
  mutate(year=year(date))

##
SoilMoist_VSM <- merge(ObsVSM[,c("date","year","mean_VSM")],
                       DayM_V[,c("date","year","mean_5cm")],
                       by=c("date","year"),
                       all=TRUE)
colnames(SoilMoist_VSM) <- c("date","year","Observed","Daycent")

SoilMoist_VSM_piv <- pivot_longer(SoilMoist_VSM, c(-date, -year),
                                  names_to = "source",
                                  values_to = "h2o_val")

##
N2O_ghaday <- merge(ObsGas[,c("date","year","N2O_N")],
                    DayGN_ghaday[,c("date","year","N2O_gNhad")],
                    by=c("date","year"),
                    all=TRUE)
colnames(N2O_ghaday) <- c("date","year","Observed","Daycent")

N2O_ghaday_piv <- pivot_longer(N2O_ghaday, c(-date),
                               names_to = "source",
                               values_to = "n2o_val")

N2O_ghayr <- DayGN_ann_gha
colnames(N2O_ghayr) <- c("year","Daycent")

N2O_ghayr_piv <- pivot_longer(N2O_ghayr, c(-year),
                              names_to = "source",
                              values_to = "n2o_val")

##
CH4_ghaday <- merge(ObsGas[,c("date","CH4_C")],
                    DayGM_ghaday[,c("date","CH4_net_gChad")],
                    by="date",
                    all=TRUE)
colnames(CH4_ghaday) <- c("date","Observed","Daycent")

CH4_ghaday_piv <- pivot_longer(CH4_ghaday, c(-date),
                               names_to = "source",
                               values_to = "ch4_val")

CH4_ghayr <- DayGM_ann_gha
colnames(N2O_ghayr) <- c("year","Daycent")

CH4_ghayr_piv <- pivot_longer(CH4_ghayr, c(-year),
                              names_to = "source",
                              values_to = "ch4_val")


#**********************************************************************
# calculate mean differences ----------------------------------------------

# between observed and modeled results

Cotton_obsmod_diff_Mgha <- sum(CottonYld_Mgha[!is.na(CottonYld_Mgha$Observed &
                                                       CottonYld_Mgha$Daycent),"Observed"] -
                                 CottonYld_Mgha[!is.na(CottonYld_Mgha$Observed &
                                                         CottonYld_Mgha$Daycent),"Daycent"])
Sorghum_obsmod_diff_Mgha <- sum(SorghumYld_Mgha[!is.na(SorghumYld_Mgha$Observed &
                                                     SorghumYld_Mgha$Daycent),"Observed"] -
                                  SorghumYld_Mgha[!is.na(SorghumYld_Mgha$Observed &
                                                       SorghumYld_Mgha$Daycent),"Daycent"])
SOC_obsmod_diff_Mgha <- sum(Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
                                                 Cstock_Mgha$Daycent),"Observed"] -
                              Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
                                                   Cstock_Mgha$Daycent),"Daycent"])
SoilT_obsmod_diff_Mgha <- mean(SoilTemp_C[!is.na(SoilTemp_C$Observed) &
                                            !is.na(SoilTemp_C$Daycent),"Observed"] -
                                 SoilTemp_C[!is.na(SoilTemp_C$Observed) &
                                              !is.na(SoilTemp_C$Daycent),"Daycent"])
SoilM_obsmod_diff_Mgha <- mean(SoilMoist_VSM[!is.na(SoilMoist_VSM$Observed &
                                                      SoilMoist_VSM$Daycent),"Observed"] -
                                 SoilMoist_VSM[!is.na(SoilMoist_VSM$Observed &
                                                        SoilMoist_VSM$Daycent),"Daycent"])
N2O_obsmod_diff_gha <- sum(N2O_ghaday[!is.na(N2O_ghaday$Observed) &
                                        !is.na(N2O_ghaday$Daycent),"Observed"] -
                             N2O_ghaday[!is.na(N2O_ghaday$Observed) &
                                          !is.na(N2O_ghaday$Daycent),"Daycent"])
CH4_obsmod_diff_gha <- sum(CH4_ghaday[!is.na(CH4_ghaday$Observed) &
                                        !is.na(CH4_ghaday$Daycent),"Observed"] -
                             CH4_ghaday[!is.na(CH4_ghaday$Observed) &
                                          !is.na(CH4_ghaday$Daycent),"Daycent"])

SOC_obsmod_diff_Mgha_nooutliers <- NA


#**********************************************************************
# write results -----------------------------------------------------------


# write out results for use later in ensemble results
output_annual_data <- cbind(merge(DayY_Mgha_pivwid[,c("year","Sorghum","Cotton")],
                                  DayC_Mgha[,c("time","base")],
                                  by.x="year", by.y="time",
                                  all=TRUE),
                            "Daycent",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)

colnames(output_annual_data) <- c("year","SorghumYld_Mgha","CottonYld_Mgha",
                                  "SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

output_annual_data2 <- cbind(DayCI_gm2yr[DayCI_gm2yr$year>=experiment_start_year &
                                           DayCI_gm2yr$year<=end_fut_period_year,c("year","base")],
                             DayNI_gm2yr[DayNI_gm2yr$year>=experiment_start_year &
                                           DayNI_gm2yr$year<=end_fut_period_year,"base"],
                            "Daycent",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt,scenario_abbrev,
                            scenario_descriptor)

colnames(output_annual_data2) <- c("year","Cinput","Ninput","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num",
                                  "scenario_abbrev","scenario_description")

output_daily_data <- cbind(DayGN_ghaday[,c("date","year","dayofyear","N2O_gNhad")],
                           DayGN_cum_gha[,"N2O_gha"],
                           DayGM_ghaday[,"CH4_net_gChad"],
                           DayGM_cum_gha[,"CH4_gha"],
                           "Daycent",scenario_name,clim_scenario_num,
                           mgmt_scenario_grp,mgmt_scenario_opt)

colnames(output_daily_data) <- c("date","year","dayofyear","N2O_emit_gha","N2O_cum_gha",
                                 "CH4_net_gha","CH4_cum_gha",
                                 "model_name","scenario_name","climate_scenario_num",
                                 "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

output_daily_data2 <- cbind(Day_soiln[Day_soiln$year>=experiment_start_year &
                                        Day_soiln$year<=end_fut_period_year,
                                      c("date","year","dayofyear","NO3_ppm")],
                            SoilTemp_C[,c("Observed","Daycent")],
                            SoilMoist_VSM[,c("Observed","Daycent")],
                            "Daycent",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt,scenario_abbrev,
                            scenario_descriptor)

colnames(output_daily_data2) <- c("date","year","dayofyear","NO3_ppm_20cm",
                                  "SoilTemp_C_Obs","SoilTemp_C_Day",
                                  "VolSoilMoist_Obs","VolSoilMoist_Day",
                                  "model_name","scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num",
                                  "scenario_abbrev","scenario_description")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_Daycent.csv"),
            col.names=T,row.names=F,sep=",",append=F)
write.table(output_annual_data2,file=paste0(results_path,"Annual_results_compilation2_",
                                           scenario_name,"_Daycent.csv"),
            col.names=T,row.names=F,sep=",",append=F)
write.table(output_daily_data,file=paste0(results_path,"Daily_results_compilation_",
                                          scenario_name,"_Daycent.csv"),
            col.names=T,row.names=F,sep=",",append=F)
write.table(output_daily_data2,file=paste0(results_path,"Daily_results_compilation2_",
                                          scenario_name,"_Daycent.csv"),
            col.names=T,row.names=F,sep=",",append=F)

#**********************************************************************
# Clean up ----------------------------------------------------------------

rm(Day_harvest_raw,Day_methane_raw,Day_soiln_raw,Day_summary_raw,DayM_V_all_raw,
   DayM_V_raw,DayT_C_all_raw)
