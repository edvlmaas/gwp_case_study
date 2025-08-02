#######################################
# File: 9_Results_RothC-setup_LRF.R
# Author: Ellen Maas
# Date: 8/30/2022
# Description: Imports LRF output and sets up data frames
# with results used in LRF's "9_" series scripts and
# some "10_" series scripts.
#######################################

print(paste0("Starting 9_Results_RothC-setup_",site_name,".R"))

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)


#**********************************************************************

# import RothC

rdata_filename <- if_else(nchar(scenario_name)==3, paste0(rothc_path,"graph/",scenario_name," .263"),
                         paste0(rothc_path,"graph/",scenario_name,".263"))

# import RothC modeled points (MON  YR  DPM  RPM  BIO  HUM  TOTAL  CO2  D14C)
RothCObs_df_raw <- read.fwf(rdata_filename,
                       widths=c(3,5,10,9,9,9,9,10,9),
                       col.names = c("MON","year","DPM","RPM","BIO","HUM","ModC","CO2","D14C"),
                       colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric"),skip=1)

# limit future output to future period
RothCObs_df <- RothCObs_df_raw[RothCObs_df_raw$year <= end_fut_period_year,]

# Cinput for for 20 cm, so reduce results 50% for 10 cm observations
RothCC_Mgha <- RothCObs_df[,c("year","ModC","CO2")] %>%
  mutate(ModC=ModC*0.5)


#**********************************************************************

# write out results for use later in ensemble results

output_annual_data <- cbind(RothCC_Mgha$year,NA,NA,
                            RothCC_Mgha[,"ModC"],
                            "RothC",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_annual_data) <- c("year","SorghumYld_Mgha","CottonYld_Mgha",
                                  "SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_RothC.csv"),
            col.names=T,row.names=F,sep=",",append=F)

#**********************************************************************


# merge observed and modeled data

##
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock","sd_cstock")],
                     RothCC_Mgha[,c("year","ModC")],
                     by="year",
                     all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","Obs_sd","RothC")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year,-Obs_sd),
                                 names_to = "source",
                                 values_to = "C_val")

# remove sd from modeled records; only for observed
Cstock_Mgha_piv <- Cstock_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

#**********************************************************************

# calculate mean differences between observed and modeled results

SOC_obsmod_diff_Mgha <- sum(Cstock_Mgha[!is.na(Cstock_Mgha$Observed) &
                                          !is.na(Cstock_Mgha$RothC),"Observed"] -
                              Cstock_Mgha[!is.na(Cstock_Mgha$Observed) &
                                            !is.na(Cstock_Mgha$RothC),"RothC"])

SOC_obsmod_diff_Mgha_nooutliers <- NA


