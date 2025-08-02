#######################################
# File: 9_Results_APSIM-setup_LRF.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Imports APSIM output and sets up data frames
# with results used in APSIM's "9_" series scripts and
# some "10_" series scripts.
#######################################

print(paste0("Starting 9_Results_APSIM-setup_",site_name,".R"))

#library(apsimx)
library(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)


#**********************************************************************

# import APSIM modeled points

if(mgmt_scenario_grp!=3 & mgmt_scenario_grp!=8) {
APSIM_out_raw <- read.fwf(paste0(apsim_path,"scen_",scenario_name,".out"),skip=4,
                      widths = c(15,15,22,21,15,15,24,23,15,15,15,
                                 20,20,20,20,20,20,19,19,19,19,19,15,
                                 15,15,15,15,15,15,15,15,15,15,15,15,
                                 15,15,15,15,15,15,15,15,15,15,15,15,
                                 15,15,15,15,15,23,23,23,23,23,23,23,
                                 23,23,23,23,23,15,15,15),
                      col.names = c("Date","BulkDensity_gcc(1)","SorghumYield_kgha",
                                    "CottonYield_kgha","VolH2O_5cm","SoilTemp_5cm_C",
                                    "sorghum_biomass_kgha","cotton_biomass_kgha",
                                    "dul_5cm","sat_5cm","ph_5cm",
                                    "N2O_bylayer_kgha(1)","N2O_bylayer_kgha(2)",
                                    "N2O_bylayer_kgha(3)","N2O_bylayer_kgha(4)",
                                    "N2O_bylayer_kgha(5)","N2O_bylayer_kgha(6)",
                                    "oc_bylayer_pct(1)",
                                    "oc_bylayer_pct(2)","oc_bylayer_pct(3)",
                                    "oc_bylayer_pct(4)","oc_bylayer_pct(5)",
                                    "BulkDensity_gcc(2)",
                                    "NO3_5cm","NO3_15cm","NO3_35cm","NO3_60cm",
                                    "VolH2O_15cm","VolH2O_35cm","VolH2O_60cm",
                                    "dul_15cm","dul_35cm","dul_60cm",
                                    "sat_15cm","sat_35cm","sat_60cm",
                                    "biomc_bylayer_kgha(1)","biomc_bylayer_kgha(2)",
                                    "biomc_bylayer_kgha(3)","biomc_bylayer_kgha(4)",
                                    "biomn_bylayer_kgha(1)","biomn_bylayer_kgha(2)",
                                    "biomn_bylayer_kgha(3)","biomn_bylayer_kgha(4)",
                                    "humc_bylayer_kgha(1)","humc_bylayer_kgha(2)",
                                    "humc_bylayer_kgha(3)","humc_bylayer_kgha(4)",
                                    "humn_bylayer_kgha(1)","humn_bylayer_kgha(2)",
                                    "humn_bylayer_kgha(3)","humn_bylayer_kgha(4)",
                                    "dlt_res_c_biom(1-2)(1)","dlt_res_c_biom(1-2)(2)",
                                    "dlt_res_c_biom(1-2)(3)","dlt_res_c_biom(1-2)(4)",
                                    "dlt_res_c_hum(1-2)(1)","dlt_res_c_hum(1-2)(2)",
                                    "dlt_res_c_hum(1-2)(3)","dlt_res_c_hum(1-2)(4)",
                                    "dlt_biom_c_hum(1-2)(1)","dlt_biom_c_hum(1-2)(2)",
                                    "dlt_biom_c_hum(1-2)(3)","dlt_biom_c_hum(1-2)(4)",
                                    "SoilTemp_15cm_C","SoilTemp_35cm_C","SoilTemp_60cm_C"),
                      colClasses = c("character","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric",
                                     "numeric"
                                     ),
                      check.names = FALSE, header=FALSE) %>%
  mutate(date=as.Date(Date, "%d/%m/%Y"),
         year=year(date),
         month=month(date),
         day=day(date),
         SorghumYield_gm2=SorghumYield_kgha/10,
         CottonYield_gm2=CottonYield_kgha/10,
         TotalSOC_10cm_Mgha=(`BulkDensity_gcc(1)`*10*`oc_bylayer_pct(1)`),# +
          # (`BulkDensity_gcc(1)`*5*`oc_bylayer_pct(2)`),
         N2O_10cm_kgha=`N2O_bylayer_kgha(1)`+(`N2O_bylayer_kgha(2)`*0.5),
         N2O_profile_kgha=`N2O_bylayer_kgha(1)`+`N2O_bylayer_kgha(2)`+
           `N2O_bylayer_kgha(3)`, #+`N2O_bylayer_kgha(4)`,
         BiomC_profile_kgha=`biomc_bylayer_kgha(1)` + `biomc_bylayer_kgha(2)` +
           `biomc_bylayer_kgha(3)`+`biomc_bylayer_kgha(4)`,
         BiomN_profile_kgha=`biomn_bylayer_kgha(1)` + `biomn_bylayer_kgha(2)` +
           `biomn_bylayer_kgha(3)`+`biomn_bylayer_kgha(4)`,
         HumC_profile_kgha=`humc_bylayer_kgha(1)` + `humc_bylayer_kgha(2)` +
           `humc_bylayer_kgha(3)`+`humc_bylayer_kgha(4)`,
         HumN_profile_kgha=`humn_bylayer_kgha(1)` + `humn_bylayer_kgha(2)` +
           `humn_bylayer_kgha(3)`+`humn_bylayer_kgha(4)`,
BiomC_10cm_kgha=`biomc_bylayer_kgha(1)` + (`biomc_bylayer_kgha(2)`*0.5),
BiomN_10cm_kgha=`biomn_bylayer_kgha(1)` + (`biomn_bylayer_kgha(2)`*0.5),
HumC_10cm_kgha=`humc_bylayer_kgha(1)` + (`humc_bylayer_kgha(2)`*0.5),
HumN_10cm_kgha=`humn_bylayer_kgha(1)` + (`humn_bylayer_kgha(2)`*0.5),
VolH2O_10cm=((VolH2O_5cm*5) + (VolH2O_15cm*5)) / 10, # weighted average
DH2O_10cm=((VolH2O_5cm*5)+(VolH2O_15cm*5)), # depth of water
CtoBiom_10cm_kgha=`dlt_res_c_biom(1-2)(1)` + (`dlt_res_c_biom(1-2)(2)`*0.5),
CtoHum_10cm_kgha=`dlt_res_c_hum(1-2)(1)` + (`dlt_res_c_hum(1-2)(2)`*0.5),
CBiomtoHum_10cm_kgha=`dlt_biom_c_hum(1-2)(1)` + (`dlt_biom_c_hum(1-2)(2)`*0.5),
SoilTemp_10cm_C=((SoilTemp_5cm_C*5)+(SoilTemp_15cm_C*5))/10 # weighted average
)
} else { # includes cover crop columns
APSIM_out_raw <- read.fwf(paste0(apsim_path,"scen_",scenario_name,".out"),skip=4,
                        widths = c(15,15,22,21,15,15,24,25,23,15,15,15,
                                   20,20,20,20,20,20,18,18,18,18,18,15,
                                   15,15,15,15,15,15,15,15,15,15,15,15,
                                   15,15,15,15,15,15,15,15,15,15,15,15,
                                   15,15,15,15,15,23,23,23,23,23,23,23,
                                   23,23,23,23,23,15,15,15),
                        col.names = c("Date","BulkDensity_gcc(1)","SorghumYield_kgha",
                                      "CottonYield_kgha",
                                      "VolH2O_5cm","SoilTemp_5cm_C",
                                      "sorghum_biomass_kgha","rye_biomass_kgha",
                                      "cotton_biomass_kgha",
                                      "dul_5cm","sat_5cm","ph_5cm",
                                      "N2O_bylayer_kgha(1)","N2O_bylayer_kgha(2)",
                                      "N2O_bylayer_kgha(3)","N2O_bylayer_kgha(4)",
                                      "N2O_bylayer_kgha(5)","N2O_bylayer_kgha(6)",
                                      "oc_bylayer_pct(1)",
                                      "oc_bylayer_pct(2)","oc_bylayer_pct(3)",
                                      "oc_bylayer_pct(4)","oc_bylayer_pct(5)",
                                      "BulkDensity_gcc(2)",
                                      "NO3_5cm","NO3_15cm","NO3_35cm","NO3_60cm",
                                      "VolH2O_15cm","VolH2O_35cm","VolH2O_60cm",
                                      "dul_15cm","dul_35cm","dul_60cm",
                                      "sat_15cm","sat_35cm","sat_60cm",
                                      "biomc_bylayer_kgha(1)","biomc_bylayer_kgha(2)",
                                      "biomc_bylayer_kgha(3)","biomc_bylayer_kgha(4)",
                                      "biomn_bylayer_kgha(1)","biomn_bylayer_kgha(2)",
                                      "biomn_bylayer_kgha(3)","biomn_bylayer_kgha(4)",
                                      "humc_bylayer_kgha(1)","humc_bylayer_kgha(2)",
                                      "humc_bylayer_kgha(3)","humc_bylayer_kgha(4)",
                                      "humn_bylayer_kgha(1)","humn_bylayer_kgha(2)",
                                      "humn_bylayer_kgha(3)","humn_bylayer_kgha(4)",
                                      "dlt_res_c_biom(1-2)(1)","dlt_res_c_biom(1-2)(2)",
                                      "dlt_res_c_biom(1-2)(3)","dlt_res_c_biom(1-2)(4)",
                                      "dlt_res_c_hum(1-2)(1)","dlt_res_c_hum(1-2)(2)",
                                      "dlt_res_c_hum(1-2)(3)","dlt_res_c_hum(1-2)(4)",
                                      "dlt_biom_c_hum(1-2)(1)","dlt_biom_c_hum(1-2)(2)",
                                      "dlt_biom_c_hum(1-2)(3)","dlt_biom_c_hum(1-2)(4)",
                                      "SoilTemp_15cm_C","SoilTemp_35cm_C","SoilTemp_60cm_C"),
                        colClasses = c("character","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric"),
                        check.names = FALSE, header=FALSE) %>%
    mutate(date=as.Date(Date, "%d/%m/%Y"),
           year=year(date),
           month=month(date),
           day=day(date),
           SorghumYield_gm2=SorghumYield_kgha/10,
           CottonYield_gm2=CottonYield_kgha/10,
           TotalSOC_10cm_Mgha=(`BulkDensity_gcc(1)`*10*`oc_bylayer_pct(1)`),# +
             #(`BulkDensity_gcc(1)`*5*`oc_bylayer_pct(2)`),
           N2O_10cm_kgha=`N2O_bylayer_kgha(1)`+(`N2O_bylayer_kgha(2)`*0.5),
           N2O_profile_kgha=`N2O_bylayer_kgha(1)`+`N2O_bylayer_kgha(2)`+
             `N2O_bylayer_kgha(3)`, #+`N2O_bylayer_kgha(4)`,
           BiomC_profile_kgha=`biomc_bylayer_kgha(1)` + `biomc_bylayer_kgha(2)` +
             `biomc_bylayer_kgha(3)`+`biomc_bylayer_kgha(4)`,
           BiomN_profile_kgha=`biomn_bylayer_kgha(1)` + `biomn_bylayer_kgha(2)` +
             `biomn_bylayer_kgha(3)`+`biomn_bylayer_kgha(4)`,
           HumC_profile_kgha=`humc_bylayer_kgha(1)` + `humc_bylayer_kgha(2)` +
             `humc_bylayer_kgha(3)`+`humc_bylayer_kgha(4)`,
           HumN_profile_kgha=`humn_bylayer_kgha(1)` + `humn_bylayer_kgha(2)` +
             `humn_bylayer_kgha(3)`+`humn_bylayer_kgha(4)`,
           BiomC_10cm_kgha=`biomc_bylayer_kgha(1)` + (`biomc_bylayer_kgha(2)`*0.5),
           BiomN_10cm_kgha=`biomn_bylayer_kgha(1)` + (`biomn_bylayer_kgha(2)`*0.5),
           HumC_10cm_kgha=`humc_bylayer_kgha(1)` + (`humc_bylayer_kgha(2)`*0.5),
           HumN_10cm_kgha=`humn_bylayer_kgha(1)` + (`humn_bylayer_kgha(2)`*0.5),
           VolH2O_10cm=((VolH2O_5cm*5) + (VolH2O_15cm*5)) / 10, # weighted average
           DH2O_10cm=((VolH2O_5cm*5)+(VolH2O_15cm*5)), # depth of water
           CtoBiom_10cm_kgha=`dlt_res_c_biom(1-2)(1)` + (`dlt_res_c_biom(1-2)(2)`*0.5),
           CtoHum_10cm_kgha=`dlt_res_c_hum(1-2)(1)` + (`dlt_res_c_hum(1-2)(2)`*0.5),
           CBiomtoHum_10cm_kgha=`dlt_biom_c_hum(1-2)(1)` + (`dlt_biom_c_hum(1-2)(2)`*0.5),
           SoilTemp_10cm_C=((SoilTemp_5cm_C*5)+(SoilTemp_15cm_C*5))/10 # weighted average
    )
}

# limit to future scenario time period
APSIM_out <- APSIM_out_raw[APSIM_out_raw$year <= end_fut_period_year,]

# soil carbon
APSIMC_Mgha <- APSIM_out[APSIM_out$month==7 & APSIM_out$day==15,
                         c("year","TotalSOC_10cm_Mgha")] %>%
  mutate(TotalSOC_10cm_Mgha=round(TotalSOC_10cm_Mgha,1))

## biomass c and n stock by layer (for explanatory graphs)
APSIMSoilCN_kgha <- APSIM_out[,c("date","year","biomc_bylayer_kgha(1)",
                                 "biomc_bylayer_kgha(2)","biomc_bylayer_kgha(3)",
                                 "biomn_bylayer_kgha(1)","biomn_bylayer_kgha(2)",
                                 "biomn_bylayer_kgha(3)","humc_bylayer_kgha(1)",
                                 "humc_bylayer_kgha(2)","humc_bylayer_kgha(3)",
                                 "humn_bylayer_kgha(1)","humn_bylayer_kgha(2)",
                                 "humn_bylayer_kgha(3)",
                                 "BiomC_10cm_kgha","BiomN_10cm_kgha",
                                 "HumC_10cm_kgha","HumN_10cm_kgha",
                                 "TotalSOC_10cm_Mgha",
                                 "CtoBiom_10cm_kgha","CtoHum_10cm_kgha",
                                 "CBiomtoHum_10cm_kgha")]
colnames(APSIMSoilCN_kgha) <- c("date","year","biomc_20cm","biomc_40cm",
                                "biomc_60cm","biomn_20cm","biomn_40cm",
                                "biomn_60cm","humc_20cm","humc_40cm",
                                "humc_60cm","humn_20cm","humn_40cm",
                                "humn_60cm",
                                "BiomC_10cm","BiomN_10cm",
                                "HumC_10cm","HumN_10cm",
                                "TotalSOC_10cm",
                                "CtoBiom_10cm","CtoHum_10cm",
                                "CBiomtoHum_10cm")

# grain yield
APSIMY_Mgha <- APSIM_out[,c("year","SorghumYield_kgha","CottonYield_kgha")] %>%
  group_by(year) %>%
  summarize(SorghumYield_Mgha=round(max(SorghumYield_kgha/1000),3),
            CottonYield_Mgha=round(max(CottonYield_kgha/1000),3))

# biomass yield
## get array of harvest dates
obs_dates<-ObsBiomass$date
date_seq<-seq.Date(from = as.Date('2011-08-15'), to = as.Date('2100-08-15'), by = 'years') # base
date_list<-as.Date(c(obs_dates,date_seq),format="%Y/%m/%d")
APSIMBY_Mgha <- APSIM_out[APSIM_out$date %in% date_list,
                          c("year","sorghum_biomass_kgha","cotton_biomass_kgha")] %>%
  group_by(year) %>%
  summarize(SorghumYield_Mgha=sorghum_biomass_kgha/1000,
            CottonYield_Mgha=cotton_biomass_kgha/1000)

## soil temperature
APSIMT_C <- APSIM_out[,c("date","year","SoilTemp_5cm_C","SoilTemp_15cm_C",
                         "SoilTemp_35cm_C","SoilTemp_60cm_C","SoilTemp_10cm_C")] %>%
  mutate(SoilTemp_5cm_C=round(SoilTemp_5cm_C,1),
         SoilTemp_15cm_C=round(SoilTemp_15cm_C,1),
         SoilTemp_35cm_C=round(SoilTemp_35cm_C,1),
         SoilTemp_60cm_C=round(SoilTemp_60cm_C,1),
         SoilTemp_10cm_C=round(SoilTemp_10cm_C,1)) 

APSIMT_C_range <- range(APSIMT_C[APSIMT_C$date %in% ObsTemp$date, "SoilTemp_5cm_C"],na.rm=T)

## volumetric soil moisture
APSIMM_V <- APSIM_out[,c("date","year","VolH2O_5cm","VolH2O_15cm","VolH2O_35cm",
                         "VolH2O_60cm","dul_5cm","dul_15cm","dul_35cm","dul_60cm",
                         "sat_5cm","sat_15cm","sat_35cm","sat_60cm",
                         "DH2O_10cm","VolH2O_10cm")] %>%
  mutate(VolH2O_5cm=round(VolH2O_5cm*100,0),
         VolH2O_15cm=round(VolH2O_15cm*100,0),
         VolH2O_35cm=round(VolH2O_35cm*100,0),
         VolH2O_60cm=round(VolH2O_60cm*100,0),
         VolH2O_10cm=round(VolH2O_10cm*100,0),
         DW_5cm=round((VolH2O_5cm/100)*5,0), #convert to fraction, mult by layer depth (cm)
         DW_15cm=round((VolH2O_15cm/100)*10,0),
         DW_35cm=round((VolH2O_35cm/100)*20,),
         DW_60cm=round(VolH2O_60cm/100*25,0),
         DW_10cm=round(DH2O_10cm*10,0)
  )


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

APSIMGN_profile_cum_calib <- APSIMGN_profile_ghaday[APSIMGN_profile_ghaday$date >= 
                                                      experiment_start_date &
                                                      APSIMGN_profile_ghaday$date <=
                                                      experiment_end_date,] %>%
  group_by(year) %>%
  summarize(tot_N2O_ghayr=sum(N2OEmissions_ghaday))

APSIMNO3_ghaday <- APSIM_out[,c("date","year","NO3_5cm","NO3_15cm","NO3_35cm",
                                "NO3_60cm")] %>%
  mutate(NO3_5cm = round(NO3_5cm*1000,2),
         NO3_15cm = round(NO3_15cm*1000,2),
         NO3_35cm = round(NO3_35cm*1000,2),
         NO3_60cm = round(NO3_60cm*1000,2))

#**********************************************************************

# write out results for use later in ensemble results
output_annual_data <- cbind(APSIMY_Mgha,APSIMC_Mgha[,"TotalSOC_10cm_Mgha"],
                            "APSIM",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_annual_data) <- c("year","SorghumYld_Mgha","CottonYld_Mgha",
                                  "SOC_Mgha","model_name",
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
# merge observed and modeled data for graphing model-specific results --------

SorghumYld_Mgha <- merge(ObsYield[ObsYield$crop=="Sorghum",c("year","mean_yield","sd_yield")],
                         APSIMY_Mgha[APSIMY_Mgha$SorghumYield_Mgha != 0,
                                   c("year","SorghumYield_Mgha")],
                         by="year",
                         all=TRUE)%>%
  merge(HistY_Mgha[,c("year","sorghum_yield_mgha")],
        by="year",
        all=TRUE)
colnames(SorghumYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Historical")

SorghumYld_Mgha_piv <- pivot_longer(SorghumYld_Mgha, c(-year,-Obs_sd),
                                    names_to = "source",
                                    values_to = "yield_val")

# remove sd from modeled records; only for observed
SorghumYld_Mgha_piv <- SorghumYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))


##
CottonYld_Mgha <- merge(ObsYield[ObsYield$crop=="Cotton",c("year","mean_yield","sd_yield")],
                        APSIMY_Mgha[APSIMY_Mgha$CottonYield_Mgha != 0,
                                  c("year","CottonYield_Mgha")],
                        by="year",
                        all=TRUE) %>%
  merge(HistY_Mgha[,c("year","cotton_yield_mgha")],
        by="year",
        all=TRUE)
colnames(CottonYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Historical")

CottonYld_Mgha_piv <- pivot_longer(CottonYld_Mgha, c(-year,-Obs_sd),
                                   names_to = "source",
                                   values_to = "yield_val")

# remove sd from modeled records; only for observed
CottonYld_Mgha_piv <- CottonYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

##
SorghumBioYld_Mgha <- merge(ObsBiomass[ObsBiomass$crop=="Sorghum",c("year","mean_yield","sd_yield")],
                         APSIMBY_Mgha[APSIMBY_Mgha$SorghumYield_Mgha != 0,
                                     c("year","SorghumYield_Mgha")],
                         by="year",
                         all=TRUE)
colnames(SorghumBioYld_Mgha) <- c("year","Observed","Obs_sd","APSIM")

SorghumBioYld_Mgha_piv <- pivot_longer(SorghumBioYld_Mgha, c(-year,-Obs_sd),
                                    names_to = "source",
                                    values_to = "yield_val")

# remove sd from modeled records; only for observed
SorghumBioYld_Mgha_piv <- SorghumBioYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

##
CottonBioYld_Mgha <- merge(ObsBiomass[ObsBiomass$crop=="Cotton",c("year","mean_yield","sd_yield")],
                        APSIMBY_Mgha[APSIMBY_Mgha$CottonYield_Mgha != 0,
                                    c("year","CottonYield_Mgha")],
                        by="year",
                        all=TRUE)
colnames(CottonBioYld_Mgha) <- c("year","Observed","Obs_sd","APSIM")

CottonBioYld_Mgha_piv <- pivot_longer(CottonBioYld_Mgha, c(-year,-Obs_sd),
                                   names_to = "source",
                                   values_to = "yield_val")

# remove sd from modeled records; only for observed
CottonBioYld_Mgha_piv <- CottonBioYld_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

##
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock","sd_cstock")],
             APSIMC_Mgha,
             by="year",
             all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","Obs_sd","APSIM")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year,-Obs_sd),
               names_to = "source",
               values_to = "C_val")

# remove sd from modeled records; only for observed
Cstock_Mgha_piv <- Cstock_Mgha_piv %>%
  mutate(Obs_sd=replace(Obs_sd, source!="Observed", NA))

##
SoilTemp_C <- full_join(APSIMT_C[,c("date","SoilTemp_5cm_C")],
             ObsTemp[,c("date","soil_temperature")],
             by="date")
colnames(SoilTemp_C) <- c("date","APSIM","Observed")

SoilTemp_C_piv <- pivot_longer(SoilTemp_C, c(-date),
               names_to = "source",
               values_to = "temp_val") %>%
  mutate(year=year(date))

##
SoilMoist_VSM <- merge(ObsVSM[,c("date","mean_VSM")],
                       APSIMM_V[,c("date","VolH2O_5cm")],
                       by="date",
                       all=TRUE)
colnames(SoilMoist_VSM) <- c("date","Observed","APSIM")

SoilMoist_VSM_piv <- pivot_longer(SoilMoist_VSM, c(-date),
               names_to = "source",
               values_to = "h2o_val") %>%
  mutate(year=year(date))

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

Sorghum_obsmod_diff_Mgha <- sum(SorghumYld_Mgha[!is.na(SorghumYld_Mgha$Observed) &
                                              !is.na(SorghumYld_Mgha$APSIM),"Observed"] -
                                SorghumYld_Mgha[!is.na(SorghumYld_Mgha$Observed) &
                                                !is.na(SorghumYld_Mgha$APSIM),"APSIM"])
Cotton_obsmod_diff_Mgha <- sum(CottonYld_Mgha[!is.na(CottonYld_Mgha$Observed &
                                                     CottonYld_Mgha$APSIM),"Observed"] -
                                  CottonYld_Mgha[!is.na(CottonYld_Mgha$Observed &
                                                       CottonYld_Mgha$APSIM),"APSIM"])
SOC_obsmod_diff_Mgha <- sum(Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
                                                     Cstock_Mgha$APSIM),"Observed"] -
                                Cstock_Mgha[!is.na(Cstock_Mgha$Observed &
                                                       Cstock_Mgha$APSIM),"APSIM"])
SoilT_obsmod_diff_Mgha <- mean(SoilTemp_C[!is.na(SoilTemp_C$Observed) &
                                            !is.na(SoilTemp_C$APSIM),"Observed"] -
                                 SoilTemp_C[!is.na(SoilTemp_C$Observed) &
                                              !is.na(SoilTemp_C$APSIM),"APSIM"])
SOC_obsmod_diff_Mgha_nooutliers <- NA

