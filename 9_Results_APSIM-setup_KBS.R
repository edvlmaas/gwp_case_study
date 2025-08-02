#######################################
# File: 9_Results_APSIM-setup_KBS.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Imports APSIM output and sets up data frames
# with results used in APSIM's "9_" series scripts and
# some "10_" series scripts.
#######################################

print(paste0("Starting 9_Results_APSIM-setup_",site_name,".R"))

#library(apsimx)
#library(readxl)
library(magrittr)
library(lubridate)
# library(tidyverse)



#**********************************************************************

# import APSIM modeled points

if(mgmt_scenario_grp!=3) {
  APSIM_out_raw <- read.fwf(paste0(apsim_path,"scen_",scenario_name,".out"),skip=4,
                            widths = c(15,15,22,20,20,15,15,24,22,22,
                                       15,15,15,15,20,20,20,20,20,19,
                                       19,19,19,19,20,20,20,20,20,15,
                                       15,15,15,15,15,15,15,15,
                                       15,15,15,15,15,15,15,15,15,15,15,15,
                                       23,23,23,23,23,23,15,15),
                            col.names = c("Date","BulkDensity_gcc(1)","SoyYield_kgha",
                                          "WheatYield_kgha","MaizeYield_kgha",
                                          "VolH2O_20cm","SoilTemp_20cm_C",
                                          "soy_biomass_kgha","wheat_biomass_kgha",
                                          "maize_biomass_kgha","stover_kgha",
                                          "dul_20cm","sat_20cm","ph_20cm",
                                          "N2O_bylayer_kgha(1)","N2O_bylayer_kgha(2)",
                                          "N2O_bylayer_kgha(3)","N2O_bylayer_kgha(4)",
                                          "N2O_bylayer_kgha(5)","oc_bylayer_kgha(1)",
                                          "oc_bylayer_kgha(2)","oc_bylayer_kgha(3)",
                                          "oc_bylayer_kgha(4)","oc_bylayer_kgha(5)",
                                          "N2O_bylayer_kgha(6)","N2O_bylayer_kgha(7)",
                                          "N2O_bylayer_kgha(8)","N2O_bylayer_kgha(9)",
                                          "N2O_bylayer_kgha(10)","NO3_20cm","NO3_40cm",
                                          "NO3_60cm","VolH2O_40cm","VolH2O_60cm",
                                          "dul_40cm","dul_60cm","sat_40cm","sat_60cm",
                                          "biomc_bylayer_kgha(1)","biomc_bylayer_kgha(2)",
                                          "biomc_bylayer_kgha(3)","biomn_bylayer_kgha(1)",
                                          "biomn_bylayer_kgha(2)","biomn_bylayer_kgha(3)",
                                          "humc_bylayer_kgha(1)","humc_bylayer_kgha(2)",
                                          "humc_bylayer_kgha(3)","humn_bylayer_kgha(1)",
                                          "humn_bylayer_kgha(2)","humn_bylayer_kgha(3)",
                                          "dlt_res_c_biom(1-2)(1)","dlt_res_c_biom(1-2)(2)",
                                          "dlt_res_c_hum(1-2)(1)","dlt_res_c_hum(1-2)(2)",
                                          "dlt_biom_c_hum(1-2)(1)","dlt_biom_c_hum(1-2)(2)",
                                          "SoilTemp_40cm_C","SoilTemp_60cm_C"),
                            colClasses = c("character","numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric"),
                            check.names = FALSE, header=FALSE) %>%
    mutate(date=as.Date(Date, "%d/%m/%Y"),
           year=year(date),
           month=month(date),
           day=day(date),
           MaizeYield_gm2=MaizeYield_kgha/10,
           SoybeanYield_gm2=SoyYield_kgha/10,
           WheatYield_gm2=WheatYield_kgha/10,
           TotalSOC_25cm_Mgha=(`BulkDensity_gcc(1)`*20*`oc_bylayer_kgha(1)`) +
             (`BulkDensity_gcc(1)`*5*`oc_bylayer_kgha(2)`),
           TotalSOC_20cm_Mgha=(`BulkDensity_gcc(1)`*20*`oc_bylayer_kgha(1)`),
           N2O_25cm_kgha=`N2O_bylayer_kgha(1)`+(`N2O_bylayer_kgha(2)`*0.25),
           N2O_profile_kgha=`N2O_bylayer_kgha(1)`+`N2O_bylayer_kgha(2)`+
             `N2O_bylayer_kgha(3)`+`N2O_bylayer_kgha(4)`+`N2O_bylayer_kgha(5)` +
             `N2O_bylayer_kgha(6)`+`N2O_bylayer_kgha(7)`+
             `N2O_bylayer_kgha(8)` + (`N2O_bylayer_kgha(9)`*0.5),# +`N2O_bylayer_kgha(10)`,
           BiomC_profile_kgha=`biomc_bylayer_kgha(1)` + `biomc_bylayer_kgha(2)` +
             `biomc_bylayer_kgha(3)`,
           BiomN_profile_kgha=`biomn_bylayer_kgha(1)` + `biomn_bylayer_kgha(2)` +
             `biomn_bylayer_kgha(3)`,
           HumC_profile_kgha=`humc_bylayer_kgha(1)` + `humc_bylayer_kgha(2)` +
             `humc_bylayer_kgha(3)`,
           HumN_profile_kgha=`humn_bylayer_kgha(1)` + `humn_bylayer_kgha(2)` +
             `humn_bylayer_kgha(3)`,
           BiomC_25cm_kgha=`biomc_bylayer_kgha(1)` + (`biomc_bylayer_kgha(2)`*0.25),
           BiomN_25cm_kgha=`biomn_bylayer_kgha(1)` + (`biomn_bylayer_kgha(2)`*0.25),
           HumC_25cm_kgha=`humc_bylayer_kgha(1)` + (`humc_bylayer_kgha(2)`*0.25),
           HumN_25cm_kgha=`humn_bylayer_kgha(1)` + (`humn_bylayer_kgha(2)`*0.25),
           VolH2O_25cm=((VolH2O_20cm*20) + (VolH2O_40cm*5)) / 25, # weighted average
           DH2O_25cm=((VolH2O_20cm*20)+(VolH2O_40cm*5)), # depth of water
           CtoBiom_25cm_kgha=`dlt_res_c_biom(1-2)(1)` + (`dlt_res_c_biom(1-2)(2)`*0.25),
           CtoHum_25cm_kgha=`dlt_res_c_hum(1-2)(1)` + (`dlt_res_c_hum(1-2)(2)`*0.25),
           CBiomtoHum_25cm_kgha=`dlt_biom_c_hum(1-2)(1)` + (`dlt_biom_c_hum(1-2)(2)`*0.25),
           SoilTemp_25cm_C=((SoilTemp_20cm_C*20)+(SoilTemp_40cm_C*5))/25
    )
} else { # includes cover crop columns
  APSIM_out_raw <- read.fwf(paste0(apsim_path,"scen_",scenario_name,".out"),skip=4,
                            widths = c(15,15,22,20,19,20,15,15,24,22,21,22,28,15,15,15,15,
                                       20,20,20,20,20,19,19,19,19,19,20,20,20,20,20,15,
                                       15,15,15,15,15,15,15,15,
                                       15,15,15,15,15,15,15,15,15,15,15,15,
                                       23,23,23,23,23,23,15,15),
                            col.names = c("Date","BulkDensity_gcc(1)","SoyYield_kgha",
                                          "WheatYield_kgha","OatsYield_kgha","MaizeYield_kgha",
                                          "VolH2O_20cm","SoilTemp_20cm_C",
                                          "soy_biomass_kgha","wheat_biomass_kgha","oats_biomass_kgha",
                                          "maize_biomass_kgha","whiteclover_biomass_kgha","stover_kgha",
                                          "dul_20cm","sat_20cm","ph_20cm",
                                          "N2O_bylayer_kgha(1)","N2O_bylayer_kgha(2)",
                                          "N2O_bylayer_kgha(3)","N2O_bylayer_kgha(4)",
                                          "N2O_bylayer_kgha(5)","oc_bylayer_kgha(1)",
                                          "oc_bylayer_kgha(2)","oc_bylayer_kgha(3)",
                                          "oc_bylayer_kgha(4)","oc_bylayer_kgha(5)",
                                          "N2O_bylayer_kgha(6)","N2O_bylayer_kgha(7)",
                                          "N2O_bylayer_kgha(8)","N2O_bylayer_kgha(9)",
                                          "N2O_bylayer_kgha(10)","NO3_20cm","NO3_40cm",
                                          "NO3_60cm","VolH2O_40cm","VolH2O_60cm",
                                          "dul_40cm","dul_60cm","sat_40cm","sat_60cm",
                                          "biomc_bylayer_kgha(1)","biomc_bylayer_kgha(2)",
                                          "biomc_bylayer_kgha(3)","biomn_bylayer_kgha(1)",
                                          "biomn_bylayer_kgha(2)","biomn_bylayer_kgha(3)",
                                          "humc_bylayer_kgha(1)","humc_bylayer_kgha(2)",
                                          "humc_bylayer_kgha(3)","humn_bylayer_kgha(1)",
                                          "humn_bylayer_kgha(2)","humn_bylayer_kgha(3)",
                                          "dlt_res_c_biom(1-2)(1)","dlt_res_c_biom(1-2)(2)",
                                          "dlt_res_c_hum(1-2)(1)","dlt_res_c_hum(1-2)(2)",
                                          "dlt_biom_c_hum(1-2)(1)","dlt_biom_c_hum(1-2)(2)",
                                          "SoilTemp_40cm_C","SoilTemp_60cm_C"),
                            colClasses = c("character","numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric",
                                           "numeric","numeric"),
                            check.names = FALSE, header=FALSE) %>%
    mutate(date=as.Date(Date, "%d/%m/%Y"),
           year=year(date),
           month=month(date),
           day=day(date),
           MaizeYield_gm2=MaizeYield_kgha/10,
           SoybeanYield_gm2=SoyYield_kgha/10,
           WheatYield_gm2=WheatYield_kgha/10,
           TotalSOC_25cm_Mgha=(`BulkDensity_gcc(1)`*20*`oc_bylayer_kgha(1)`) +
             (`BulkDensity_gcc(1)`*5*`oc_bylayer_kgha(2)`),
           TotalSOC_20cm_Mgha=(`BulkDensity_gcc(1)`*20*`oc_bylayer_kgha(1)`),
           N2O_25cm_kgha=`N2O_bylayer_kgha(1)`+(`N2O_bylayer_kgha(2)`*0.25),
           N2O_profile_kgha=`N2O_bylayer_kgha(1)`+`N2O_bylayer_kgha(2)`+
             `N2O_bylayer_kgha(3)`+`N2O_bylayer_kgha(4)`+`N2O_bylayer_kgha(5)` +
             `N2O_bylayer_kgha(6)`+`N2O_bylayer_kgha(7)`+
             `N2O_bylayer_kgha(8)` + (`N2O_bylayer_kgha(9)`*0.5), # + `N2O_bylayer_kgha(10)`,
           BiomC_profile_kgha=`biomc_bylayer_kgha(1)` + `biomc_bylayer_kgha(2)` +
             `biomc_bylayer_kgha(3)`,
           BiomN_profile_kgha=`biomn_bylayer_kgha(1)` + `biomn_bylayer_kgha(2)` +
             `biomn_bylayer_kgha(3)`,
           HumC_profile_kgha=`humc_bylayer_kgha(1)` + `humc_bylayer_kgha(2)` +
             `humc_bylayer_kgha(3)`,
           HumN_profile_kgha<-`humn_bylayer_kgha(1)` + `humn_bylayer_kgha(2)` +
             `humn_bylayer_kgha(3)`,
           BiomC_25cm_kgha=`biomc_bylayer_kgha(1)` + (`biomc_bylayer_kgha(2)`*0.25),
           BiomN_25cm_kgha=`biomn_bylayer_kgha(1)` + (`biomn_bylayer_kgha(2)`*0.25),
           HumC_25cm_kgha=`humc_bylayer_kgha(1)` + (`humc_bylayer_kgha(2)`*0.25),
           HumN_25cm_kgha=`humn_bylayer_kgha(1)` + (`humn_bylayer_kgha(2)`*0.25),
           VolH2O_25cm=((VolH2O_20cm*20) + (VolH2O_40cm*5)) / 25, # weighted average
           DH2O_25cm=((VolH2O_20cm*20)+(VolH2O_40cm*5)), # depth of water
           CtoBiom_25cm_kgha=`dlt_res_c_biom(1-2)(1)` + (`dlt_res_c_biom(1-2)(2)`*0.25),
           CtoHum_25cm_kgha=`dlt_res_c_hum(1-2)(1)` + (`dlt_res_c_hum(1-2)(2)`*0.25),
           CBiomtoHum_25cm_kgha=`dlt_biom_c_hum(1-2)(1)` + (`dlt_biom_c_hum(1-2)(2)`*0.25)#,
    )
  APSIM_out_raw$SoilTemp_40cm_C<-as.numeric(APSIM_out_raw$SoilTemp_40cm_C)
  APSIM_out_raw$SoilTemp_25cm_C=((APSIM_out_raw$SoilTemp_20cm_C*20)+
                                   (APSIM_out_raw$SoilTemp_40cm_C*5))/25
}

# limit to future scenario time period
APSIM_out <- APSIM_out_raw[APSIM_out_raw$year <= end_fut_period_year,]

# soil carbon and nitrogen

## total SOC stock to 25 cm
APSIMC_Mgha <- APSIM_out[APSIM_out$month==7 & APSIM_out$day==15,
                         c("year","TotalSOC_25cm_Mgha")] %>%
  mutate(TotalSOC_25cm_Mgha=round(TotalSOC_25cm_Mgha,1))

## biomass c and n stock by layer (for explanatory graphs)
APSIMSoilCN_kgha <- APSIM_out[,c("date","year","biomc_bylayer_kgha(1)",
                                 "biomc_bylayer_kgha(2)","biomc_bylayer_kgha(3)",
                                 "biomn_bylayer_kgha(1)","biomn_bylayer_kgha(2)",
                                 "biomn_bylayer_kgha(3)","humc_bylayer_kgha(1)",
                                 "humc_bylayer_kgha(2)","humc_bylayer_kgha(3)",
                                 "humn_bylayer_kgha(1)","humn_bylayer_kgha(2)",
                                 "humn_bylayer_kgha(3)","TotalSOC_20cm_Mgha",
                                 "BiomC_25cm_kgha","BiomN_25cm_kgha",
                                 "HumC_25cm_kgha","HumN_25cm_kgha",
                                 "TotalSOC_25cm_Mgha",
                                 "CtoBiom_25cm_kgha","CtoHum_25cm_kgha",
                                 "CBiomtoHum_25cm_kgha")]
colnames(APSIMSoilCN_kgha) <- c("date","year","biomc_20cm","biomc_40cm",
                                "biomc_60cm","biomn_20cm","biomn_40cm",
                                "biomn_60cm","humc_20cm","humc_40cm",
                                "humc_60cm","humn_20cm","humn_40cm",
                                "humn_60cm","TotalSOC_20cm_Mgha",
                                "BiomC_25cm","BiomN_25cm",
                                "HumC_25cm","HumN_25cm",
                                "TotalSOC_25cm",
                                "CtoBiom_25cm","CtoHum_25cm",
                                "CBiomtoHum_25cm")

# yield
APSIMY_Mgha <- APSIM_out[,c("year","MaizeYield_kgha","SoyYield_kgha",
                            "WheatYield_kgha")] %>%
  group_by(year) %>%
  summarize(MaizeYield_Mgha=round(max(MaizeYield_kgha/1000),3),
            SoyYield_Mgha=round(max(SoyYield_kgha/1000),3),
            WheatYield_Mgha=round(max(WheatYield_kgha/1000),3))

## soil temperature
APSIMT_C <- APSIM_out[,c("date","year","SoilTemp_20cm_C","SoilTemp_40cm_C",
                         "SoilTemp_60cm_C","SoilTemp_25cm_C")] %>%
  mutate(SoilTemp_20cm_C=round(SoilTemp_20cm_C,1),
         SoilTemp_40cm_C=round(SoilTemp_40cm_C,1),
         SoilTemp_60cm_C=round(SoilTemp_60cm_C,1),
         SoilTemp_25cm_C=round(SoilTemp_25cm_C,1)) 

APSIMT_C_range <- range(APSIMT_C[APSIMT_C$year %in% ObsTemp$year, "SoilTemp_20cm_C"],na.rm=T)

# ## soil temperature with bias correction
# APSIMT_C_calib <- APSIM_out[,c("date","year","SoilTemp_20cm_C")] %>%
#   mutate(SoilTemp_20cm_C=round(SoilTemp_20cm_C,1)-soil_temp_bias) 

## volumetric soil moisture (convert to percent)
APSIMM_V <- APSIM_out[,c("date","year","VolH2O_20cm","dul_20cm","sat_20cm",
                         "VolH2O_40cm","VolH2O_60cm","dul_40cm","dul_60cm",
                         "sat_40cm","sat_60cm","VolH2O_25cm","DH2O_25cm")] %>%
  mutate(VolH2O_20cm=round(VolH2O_20cm*100,0),
         VolH2O_40cm=round(VolH2O_40cm*100,0),
         VolH2O_60cm=round(VolH2O_60cm*100,0),
         VolH2O_25cm=round(VolH2O_25cm*100,0),
         DW_20cm=(VolH2O_20cm/100)*20, #convert to fraction, mult by layer depth (cm)
         DW_40cm=(VolH2O_40cm/100)*20,
         DW_60cm=(VolH2O_60cm/100)*20,
         DW_25cm=round(DH2O_25cm*100,0)
         )

# ## volumetric soil moisture with bias correction
# APSIMM_V_calib <- APSIM_out[,c("date","year","VolH2O_20cm")] %>%
#   mutate(VolH2O_20cm=round(VolH2O_20cm*100-soil_moist_bias,0))

## bulk density
APSIMB_gcc <- APSIM_out[,c("date","year","BulkDensity_gcc(1)")] %>%
  mutate(BulkDensity_gcc=round(APSIM_out$`BulkDensity_gcc(1)`,2))

## N2O emissions

APSIMGN_ghaday <- APSIM_out[,c("date","year","N2O_profile_kgha")] %>%
  mutate(N2OEmissions_ghaday = round(N2O_profile_kgha*1000,2),
         dayofyear = yday(date))

APSIMGN_ann_gha <- APSIMGN_ghaday %>%
  group_by(year) %>%
  summarize(N2OEmissions_ghayr=sum(N2OEmissions_ghaday))

APSIMGN_cum_gha <- APSIMGN_ghaday %>%
  mutate(N2O_gha = cumsum(round(N2O_profile_kgha*1000,2))) %>%
  select(date,year,N2O_gha)

APSIMGN_cum_calib <- APSIMGN_ghaday[APSIMGN_ghaday$date %in% pull(ObsGas[!is.na(ObsGas$N2O_N),], date),] %>%
  group_by(year) %>%
  summarize(tot_N2O_ghayr=sum(N2OEmissions_ghaday))

APSIMGN_profile_ghaday <- APSIM_out[,c("date","year","N2O_profile_kgha")] %>%
  mutate(N2OEmissions_ghaday = round(N2O_profile_kgha*1000,2),
         dayofyear = yday(date))

APSIMGN_profile_ann_gha <- APSIMGN_profile_ghaday %>%
  group_by(year) %>%
  summarize(N2OEmissions_ghayr=sum(N2OEmissions_ghaday))

APSIMGN_profile_cum_gha <- APSIMGN_profile_ghaday %>%
  mutate(N2O_gha = cumsum(round(N2O_profile_kgha*1000,2))) %>%
  select(date,year,N2O_gha)

APSIMGN_profile_cum_calib <- APSIMGN_profile_ghaday[APSIMGN_profile_ghaday$date %in% 
                                                      pull(ObsGas[!is.na(ObsGas$N2O_N),], date),] %>%
  group_by(year) %>%
  summarize(tot_N2O_ghayr=sum(N2OEmissions_ghaday))

APSIMNO3_ghaday <- APSIM_out[,c("date","year","NO3_20cm","NO3_40cm","NO3_60cm")] %>%
  mutate(NO3_20cm = round(NO3_20cm*1000,2),
         NO3_40cm = round(NO3_40cm*1000,2),
         NO3_60cm = round(NO3_60cm*1000,2))


#**********************************************************************

# write out results for use later in ensemble results
output_annual_data <- cbind(APSIMY_Mgha,APSIMC_Mgha[,"TotalSOC_25cm_Mgha"],
                            "APSIM",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_annual_data) <- c("year","MaizeYld_Mgha","SoyYld_Mgha",
                                  "WheatYld_Mgha","SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

output_daily_data <- cbind(APSIMGN_ghaday[,c("date","year","dayofyear",
                                             "N2OEmissions_ghaday")],
                           APSIMGN_cum_gha[,"N2O_gha"],NA,NA,
                           "APSIM",scenario_name,clim_scenario_num,
                           mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_daily_data) <- c("date","year","dayofyear","N2O_emit_gha","N2O_cum_gha",
                                 "CH4_net_gha","CH4_cum_gha",
                                 "model_name","scenario_name","climate_scenario_num",
                                 "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_APSIM.csv"),
            col.names=T,row.names=F,sep=",",append=F)
write.table(output_daily_data,file=paste0(results_path,"Daily_results_compilation_",
                                          scenario_name,"_APSIM.csv"),
            col.names=T,row.names=F,sep=",",append=F)

#**********************************************************************

# merge observed and modeled data for graphing model-specific results

MaizeYld_Mgha <- merge(ObsYield[ObsYield$crop=="Maize",c("year","mean_yield")],
                       APSIMY_Mgha[APSIMY_Mgha$MaizeYield_Mgha != 0,
                                   c("year","MaizeYield_Mgha")],
                       by="year",
                       all=TRUE)
colnames(MaizeYld_Mgha) <- c("year","Observed","APSIM")

MaizeYld_Mgha_piv <- pivot_longer(MaizeYld_Mgha, c(-year),
                                  names_to = "source",
                                  values_to = "yield_val")

##
SoyYld_Mgha <- merge(ObsYield[ObsYield$crop=="Soybean",c("year","mean_yield")],
                     APSIMY_Mgha[APSIMY_Mgha$SoyYield_Mgha != 0,
                                 c("year","SoyYield_Mgha")],
                     by="year",
                     all=TRUE)
colnames(SoyYld_Mgha) <- c("year","Observed","APSIM")

SoyYld_Mgha_piv <- pivot_longer(SoyYld_Mgha, c(-year),
                                names_to = "source",
                                values_to = "yield_val")

##
WheatYld_Mgha <- merge(ObsYield[ObsYield$crop=="Wheat",c("year","mean_yield")],
                       APSIMY_Mgha[APSIMY_Mgha$WheatYield_Mgha != 0,
                                   c("year","WheatYield_Mgha")],
                       by="year",
                       all=TRUE)
colnames(WheatYld_Mgha) <- c("year","Observed","APSIM")

WheatYld_Mgha_piv <- pivot_longer(WheatYld_Mgha, c(-year),
                                  names_to = "source",
                                  values_to = "yield_val")

##
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock")],
                     APSIMC_Mgha,
                     by="year",
                     all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","APSIM")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year),
                                 names_to = "source",
                                 values_to = "C_val")

##
SoilTemp_C <- merge(ObsTemp[,c("date","soil_temperature")],
                    APSIMT_C[,c("date","SoilTemp_20cm_C")],
                    by="date",
                    all=TRUE)
colnames(SoilTemp_C) <- c("date","Observed","APSIM")

SoilTemp_C_piv <- pivot_longer(SoilTemp_C, c(-date),
                               names_to = "source",
                               values_to = "temp_val") %>%
  mutate(year=year(date))

# ## calibrated
# SoilTemp_C_calib <- merge(ObsTemp[,c("date","soil_temperature")],
#                           APSIMT_C_calib[,c("date","SoilTemp_20cm_C")],
#                           by="date",
#                           all=TRUE)
# colnames(SoilTemp_C_calib) <- c("date","Observed","APSIM")
# 
# SoilTemp_C_piv_calib <- pivot_longer(SoilTemp_C_calib, c(-date),
#                                      names_to = "source",
#                                      values_to = "temp_val") %>%
#   mutate(year=year(date))

##
SoilMoist_VSM <- merge(ObsVSM[,c("date","mean_VSM")],
                       APSIMM_V[,c("date","VolH2O_20cm")],
                       by="date",
                       all=TRUE)
colnames(SoilMoist_VSM) <- c("date","Observed","APSIM")

SoilMoist_VSM_piv <- pivot_longer(SoilMoist_VSM, c(-date),
                                  names_to = "source",
                                  values_to = "h2o_val") %>%
  mutate(year=year(date))

# ## calibrated
# SoilMoist_VSM_calib <- merge(ObsVSM[,c("date","mean_VSM")],
#                              APSIMM_V_calib[,c("date","VolH2O_20cm")],
#                              by="date",
#                              all=TRUE)
# colnames(SoilMoist_VSM_calib) <- c("date","Observed","APSIM")
# 
# SoilMoist_VSM_piv_calib <- pivot_longer(SoilMoist_VSM_calib, c(-date),
#                                         names_to = "source",
#                                         values_to = "h2o_val") %>%
#   mutate(year=year(date))

##
SoilBD_gcc <- merge(ObsBD[,c("year","mean_BD")],
                    APSIMB_gcc[APSIMB_gcc$date=="1996-01-01",c("year","BulkDensity_gcc")],
                    by="year",
                    all=TRUE)
colnames(SoilBD_gcc) <- c("year","Observed","APSIM")

SoilBD_gcc_piv <- pivot_longer(SoilBD_gcc, c(-year),
                               names_to = "source",
                               values_to = "bd_val")

N2O_ghaday <- merge(ObsGas[,c("date","N2O_N")],
                    APSIMGN_ghaday[,c("date","N2OEmissions_ghaday")],
                    by="date",
                    all=TRUE)
colnames(N2O_ghaday) <- c("date","Observed","APSIM")

N2O_ghaday_piv <- pivot_longer(N2O_ghaday, c(-date),
                               names_to = "source",
                               values_to = "n2o_val")

N2O_ghayr <- APSIMGN_ann_gha
colnames(N2O_ghayr) <- c("year","APSIM")

N2O_ghayr_piv <- pivot_longer(N2O_ghayr, c(-year),
                              names_to = "source",
                              values_to = "n2o_val")


#**********************************************************************

# calculate mean differences between observed and modeled results

Maize_obsmod_diff_Mgha <- sum(MaizeYld_Mgha[!is.na(MaizeYld_Mgha$Observed) &
                                              !is.na(MaizeYld_Mgha$APSIM),"Observed"] -
                                MaizeYld_Mgha[!is.na(MaizeYld_Mgha$Observed) &
                                                !is.na(MaizeYld_Mgha$APSIM),"APSIM"])
Soybean_obsmod_diff_Mgha <- sum(SoyYld_Mgha[!is.na(SoyYld_Mgha$Observed &
                                                     SoyYld_Mgha$APSIM),"Observed"] -
                                  SoyYld_Mgha[!is.na(SoyYld_Mgha$Observed &
                                                       SoyYld_Mgha$APSIM),"APSIM"])
Wheat_obsmod_diff_Mgha <- sum(WheatYld_Mgha[!is.na(WheatYld_Mgha$Observed &
                                                     WheatYld_Mgha$APSIM),"Observed"] -
                                WheatYld_Mgha[!is.na(WheatYld_Mgha$Observed &
                                                       WheatYld_Mgha$APSIM),"APSIM"])
SOC_obsmod_diff_Mgha <- sum(Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
                                                 Cstock_Mgha$APSIM),"Observed"] -
                              Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
                                                   Cstock_Mgha$APSIM),"APSIM"])
SoilT_obsmod_diff_Mgha <- mean(SoilTemp_C[!is.na(SoilTemp_C$Observed) &
                                            !is.na(SoilTemp_C$APSIM),"Observed"] -
                                 SoilTemp_C[!is.na(SoilTemp_C$Observed) &
                                              !is.na(SoilTemp_C$APSIM),"APSIM"])
SoilM_obsmod_diff_Mgha <- mean(SoilMoist_VSM[!is.na(SoilMoist_VSM$Observed &
                                                      SoilMoist_VSM$APSIM),"Observed"] -
                                 SoilMoist_VSM[!is.na(SoilMoist_VSM$Observed &
                                                        SoilMoist_VSM$APSIM),"APSIM"])
N2O_obsmod_diff_gha <- sum(N2O_ghaday[!is.na(N2O_ghaday$Observed) &
                                        !is.na(N2O_ghaday$APSIM),"Observed"] -
                             N2O_ghaday[!is.na(N2O_ghaday$Observed) &
                                          !is.na(N2O_ghaday$APSIM),"APSIM"])

SOC_obsmod_diff_Mgha_nooutliers <- sum(Cstock_Mgha[!(Cstock_Mgha$Observed %in% ObsC_outliers) &
                                                     !is.na(Cstock_Mgha$Observed & Cstock_Mgha$APSIM),"Observed"] -
                                         Cstock_Mgha[!(Cstock_Mgha$Observed %in% ObsC_outliers) &
                                                       !is.na(Cstock_Mgha$Observed & Cstock_Mgha$APSIM),"APSIM"])
