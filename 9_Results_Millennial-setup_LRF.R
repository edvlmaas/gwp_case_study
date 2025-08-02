#######################################
# File: 9_Results_Millennial-setup_LRF.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Imports Millennial output and sets up data frames
# with results used in Millennial's "9_" series scripts and
# some "10_" series scripts.
#######################################

print(paste0("Starting 9_Results_Millennial-setup_",site_name,".R"))

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# local constants

mill_baseinput_filename <- "siteenviron_base_in.txt"



#*************************************************************
# import Millennial -------------------------------------------------------


mill_base_df_raw <- read.csv(file=paste0(mill_path,"base_out_",scenario_name,".csv"))
mill_scen_df_raw <- read.csv(file=paste0(mill_path,"scenario_out_",scenario_name,".csv"))

# limit future output to end of future period
mill_scen_df <- mill_scen_df_raw[mill_scen_df_raw$year <= end_fut_period_year,]

mill_daily_df <- rbind(mill_base_df_raw,mill_scen_df)
mill_df <- mill_daily_df[month(mill_daily_df$date)==1 & day(mill_daily_df$date)==2,] %>%
  mutate(TOC_Mgha=TOC/100)

millC_Mgha <- mill_df[,c("year","TOC_Mgha")]

# add C input to graph to see how millennial responds to input
mill_dailyCinput_df <- read.delim(file=paste0(mill_path,mill_baseinput_filename),sep="\t") %>%
  mutate(year=year(date))

mill_annualCinput_df <- mill_dailyCinput_df %>%
  group_by(year) %>%
  summarize(totC=sum(forc_npp))

# reduce Millennial C to limit to top 10 cm
## calculate the Mgha to reduce the output by to get from 1 m C to top 10 cm
## can't just multiply by a fraction as the values end up compressed, and we just
## need everything to drop down by a set amount
reduceCby <- 38
millC_Mgha_10cm <- data.frame(year=millC_Mgha$year,
                            cstock=millC_Mgha$TOC_Mgha-reduceCby) 

# add daily microbial CO2 (model output is cumulative)
mill_base_df <- mill_base_df_raw %>%
  mutate(date=as.Date(date),
    CO2_daily_gm2d=CO2-lag(CO2,default=0),
         CO2_daily_ghad=CO2_daily_gm2d*10000)


#**********************************************************************
# write results -----------------------------------------------------------


# write out results for use later in ensemble results
output_annual_data <- cbind(millC_Mgha_10cm$year,NA,NA,
                            millC_Mgha_10cm[,"cstock"],
                            "Millennial",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_annual_data) <- c("year","SorghumYld_Mgha","CottonYld_Mgha",
                                  "SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_Millennial.csv"),
            col.names=T,row.names=F,sep=",",append=F)


#*************************************************************
# merge data --------------------------------------------------------------


# Carbon, full 1 m depth
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock","sd_cstock")],
                     millC_Mgha,
                     by="year",
                     all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","Obs_sd","Millennial")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year,-Obs_sd),
               names_to = "source",
               values_to = "C_val")

# remove sd from modeled records; only for observed
Cstock_Mgha_piv <- Cstock_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

# -------------

## C for full 1 m depth
Cat1940 <- as.numeric(millC_Mgha[millC_Mgha$year==land_conversion_year,"TOC_Mgha"])
Cat2003 <- as.numeric(millC_Mgha[millC_Mgha$year==experiment_start_year,"TOC_Mgha"])
Cdiff_1940_2003 <- Cat1940-Cat2003

# -------------

# Carbon, 10 cm depth
Cstock_Mgha_10cm <- merge(ObsC_Mgha[,c("year","cstock","sd_cstock")],
                     millC_Mgha_10cm,
                     by="year",
                     all=TRUE)
colnames(Cstock_Mgha_10cm) <- c("year","Observed","Obs_sd","Millennial")

Cstock_Mgha_piv_10cm <-  pivot_longer(Cstock_Mgha_10cm, c(-year,-Obs_sd),
               names_to = "source",
               values_to = "C_val")

# remove sd from modeled records; only for observed
Cstock_Mgha_piv_10cm <- Cstock_Mgha_piv_10cm %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

# -------------

# Microbial
CO2_ghaday <- merge(ObsGas_all[ObsGas_all$Treatment %in% treatment,
                    c("date","year","treatment","CO2_C")],
                   mill_base_df,
                   by=c("date","year"),
                   all=TRUE)

mbio_gm2_all <- left_join(mill_base_df[,c("year","date","MIC")],
                      ObsMB_all[ObsMB_all$treatment==treatment,c("year","date","treatment","mb_gm2","sd_gm2")],
                      by=c("date","year"))
colnames(mbio_gm2_all) <- c("year","date","Millennial","treatment","Observed","Obs_sd")

mbio_gm2 <- inner_join(mill_base_df[,c("year","date","MIC")],
                      ObsMB_all[ObsMB_all$treatment==treatment,c("year","date","treatment","mb_gm2","sd_gm2")],
                      by=c("date","year"))
colnames(mbio_gm2) <- c("year","date","Millennial","treatment","Observed","Obs_sd")

mbio_gm2_piv <- pivot_longer(mbio_gm2, c(-date,-year,-treatment,-Obs_sd),
                             names_to = "source",
                             values_to = "MB_val")

# remove sd from modeled records; only for observed
mbio_gm2_piv <- mbio_gm2_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))


#**********************************************************************
# calculate mean diffs ----------------------------------------------------

# differences between observed and modeled results

SOC_obsmod_diff_Mgha <- sum(Cstock_Mgha_10cm[!is.na(Cstock_Mgha_10cm$Observed) &
                                          !is.na(Cstock_Mgha_10cm$Millennial),"Observed"] -
                              Cstock_Mgha_10cm[!is.na(Cstock_Mgha_10cm$Observed) &
                                            !is.na(Cstock_Mgha_10cm$Millennial),"Millennial"])
MBio_obsmod_diff_Mgha <- sum(mbio_gm2[!is.na(mbio_gm2$Observed) &
                                               !is.na(mbio_gm2$Millennial),"Observed"] -
                               mbio_gm2[!is.na(mbio_gm2$Observed) &
                                                 !is.na(mbio_gm2$Millennial),"Millennial"])

SOC_obsmod_diff_Mgha_nooutliers <- NA

