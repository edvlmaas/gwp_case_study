#######################################
# Script: 9_Results_Daycent-future_LRF.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Runs graphs for the future period for the Daycent simulation
# at LRF, TX. Some graphs include data from the experimental period too.
# Also logs a selection of results for final processing # in the "10_" 
# series of scripts.
#######################################

suppressMessages({
  
  print(paste0("Starting 9_Results_Daycent-future2_",site_name,".R"))
  
  library(readxl)
  library(magrittr)
  library(lubridate)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  library(broom)
  
  
  # Calculate weighted means ------------------------------------------------
  
  ## For CH4 graphs, calculate weighted mean of wfps
  soilm_mean <- Day_wfps[,c("date","wfps_layer1","wfps_layer2","wfps_layer3",
                            "wfps_layer4")] %>%
    mutate(mean_wfps = (wfps_layer1 * 2/15) +
             (wfps_layer2 * 3/15) +
             (wfps_layer3 * 5/15) +
             (wfps_layer4 * 5/15)
    )  
  
  exp_years <- end_exp_period_year - experiment_start_year
  fut_year_end <- (end_exp_period_year+1) + exp_years
  
  ## calculate summed soil moisture and temperature as additional explanatory variables
  soilm_total_exp <- sum(soilm_mean[year(soilm_mean$date) <= end_exp_period_year,"mean_wfps"])
  soilm_mean_exp <- mean(soilm_mean[year(soilm_mean$date) <= end_exp_period_year,"mean_wfps"])
  soilt_total_exp <- sum(DayT_C[year(DayT_C$date) <= end_exp_period_year,"mean_15cm"])
  soilt_mean_exp <- mean(DayT_C[year(DayT_C$date) <= end_exp_period_year,"mean_15cm"])
  soilm_total_fut <- sum(soilm_mean[year(soilm_mean$date) %in% (end_exp_period_year+1):fut_year_end,"mean_wfps"])
  soilm_mean_fut <- mean(soilm_mean[year(soilm_mean$date) %in% (end_exp_period_year+1):fut_year_end,"mean_wfps"])
  soilt_total_fut <- sum(DayT_C[year(DayT_C$date) %in% (end_exp_period_year+1):fut_year_end,"mean_15cm"])
  soilt_mean_fut <- mean(DayT_C[year(DayT_C$date) %in% (end_exp_period_year+1):fut_year_end,"mean_15cm"])
  

# Future temporal graphs --------------------------------------------------

  
  ## Soil Moisture
  
  gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Daycent'
                          &SoilMoist_VSM_piv$date>=experiment_start_date,] %>%
    ggplot(aes(x=date, y=h2o_val, color=source)) +
    geom_point() +
    geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed'
                                      &SoilMoist_VSM_piv$date>=experiment_start_date,],
               aes(x=date, y=h2o_val, color=source)) +
    xlab("Year") +
    ylab("Volumetric soil moisture (%)") +
    ggtitle(paste0(site_name," Volumetric soil moisture with ",soil_moist_bias,"% correction"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=c(Daycent_color,Observed_color)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gM
  
  ## Cotton
  
  CYfit_Day <- coef(lm(Daycent ~ year, 
                         data = CottonYld_Mgha[CottonYld_Mgha$year>end_exp_period_year,]))
  CYfit_Obs <- coef(lm(Observed ~ year, 
                       data = CottonYld_Mgha[CottonYld_Mgha$year %in% experiment_year_range,]))
  CYxs <- c(end_exp_period_year+1, end_fut_period_year)
  CYys <- cbind(1, CYxs) %*% CYfit_Day
  CYobsxs <- c(experiment_start_year, experiment_end_year)
  CYobsys <- cbind(1, CYobsxs) %*% CYfit_Obs
  
  gCY <- CottonYld_Mgha_piv %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Cotton Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("Daycent","Historical","Observed"),
                       values=c(Daycent_color,Historical_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gCY
  
  if(mgmt_scenario_grp!=7) {
    ## Sorghum
    
    SYfit_Day <- coef(lm(Daycent ~ year, 
                           data = SorghumYld_Mgha[SorghumYld_Mgha$year>end_exp_period_year,]))
    SYfit_Obs <- coef(lm(Observed ~ year, 
                         data = SorghumYld_Mgha[SorghumYld_Mgha$year %in% experiment_year_range,]))
    SYxs <- c(end_exp_period_year+1, end_fut_period_year)
    SYys <- cbind(1, SYxs) %*% SYfit_Day
    SYobsxs <- c(experiment_start_year, experiment_end_year)
    SYobsys <- cbind(1, SYobsxs) %*% SYfit_Obs
    
    gSY <- SorghumYld_Mgha_piv %>%
      ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
      geom_point() +
      xlab("Year") +
      ylab(expression('Sorghum Yield (Mg ha ' ^-1*')')) +
      ylim(0,4) +
      ggtitle(paste(site_name,"Sorghum Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
      scale_color_manual(labels=c("Daycent","Historical","Observed"),
                         values=c(Daycent_color,Historical_color,Observed_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gSY
  }
  

  ## SOC
  
  Cfit_Day <- coef(lm(Daycent ~ year, data = Cstock_Mgha))
  if(mgmt_scenario_grp==3) {
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                              Cstock_Mgha$year >= experiment_start_year,]))
  } else {
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
  }
  
  Cxs <- c(end_exp_period_year+1, end_fut_period_year)
  Cys <- cbind(1, Cxs) %*% Cfit_Day
  Cobsxs <- c(experiment_start_year, experiment_end_year)
  Cobsys <- cbind(1, Cobsxs) %*% Cfit_Obs
  
  gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year>=experiment_start_year,] %>%
    ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
    ggtitle(paste(site_name,"Soil Organic Carbon"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = Cobsxs[1], xend = Cobsxs[2], y = Cobsys[1], yend = Cobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=c(Daycent_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gC 
  
  gT <- SoilTemp_C_piv[SoilTemp_C_piv$source=='Daycent'
                       &SoilTemp_C_piv$date>=experiment_start_date,] %>%
    ggplot(aes(x=date, y=temp_val, color=source)) +
    geom_point(show.legend=TRUE) +
    geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed'
                                   &SoilTemp_C_piv$date>=experiment_start_date,],
               aes(x=date, y=temp_val, color=source)) +
    xlab("Year") +
    ylab(expression('Soil temperature (' ^o*'C)')) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=c(Daycent_color,Observed_color)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gT
  
  gTfull <- SoilTemp_C_piv[SoilTemp_C_piv$source=='Daycent',] %>%
    ggplot(aes(x=date, y=temp_val, color=source)) +
    geom_point(show.legend=TRUE) +
    geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed'
                                   &SoilTemp_C_piv$date>=experiment_start_date,],
               aes(x=date, y=temp_val, color=source)) +
    xlab("Year") +
    ylab(expression('Soil temperature (' ^o*'C)')) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=c(Daycent_color,Observed_color)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gT
  
  
  # Daily N2O
  
  gNG <- N2O_ghaday_piv[N2O_ghaday_piv$source=='Daycent'
                        &year(N2O_ghaday_piv$date) >= experiment_start_date,] %>%
    ggplot(aes(x=date, y=n2o_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'
                                   &year(N2O_ghaday_piv$date) >= experiment_start_date,],
               aes(x=date, y=n2o_val, color=source)) +
  xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (g N ha ' ^-1*'day ' ^-1*')')) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=c(Daycent_color,Observed_color)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNG
  
  # Annual N2O
  
  NGfit_Day <- coef(lm(N2OEmissions_ghayr ~ year, 
                       data = DayGN_ann_gha[DayGN_ann_gha$year>end_exp_period_year,]))
  NGxs <- c(end_exp_period_year+1, end_fut_period_year)
  NGys <- cbind(1, NGxs) %*% NGfit_Day
  
  gNGann <- N2O_ghayr_piv %>%
    ggplot(aes(x=year, y=n2o_val, color=source)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (g ha ' ^-1*'yr ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"Annual Cumulative N"["2"]*"O Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = NGxs[1], xend = NGxs[2], y = NGys[1], yend = NGys[2]), color=cbPalette9[8]) +
    scale_color_manual(labels=c("Daycent"),
                       values=c(Daycent_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNGann
  
  ## Daily CH4
  
  gMG <- CH4_ghaday_piv[CH4_ghaday_piv$source=='Daycent'
                        &year(CH4_ghaday_piv$date) >= experiment_start_date,] %>%
    ggplot(aes(x=date, y=ch4_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=CH4_ghaday_piv[CH4_ghaday_piv$source=='Observed'
                                   &year(CH4_ghaday_piv$date) >= experiment_start_date,],
               aes(x=date, y=ch4_val, color=source)) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (g C ha ' ^-1*'day ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"Daily CH"["4"]*" Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=c(Daycent_color,Observed_color)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMG
  
  ## Annual CH4
  
  MGfit_Day <- coef(lm(CH4Emissions_ghayr ~ year, 
                       data = DayGM_ann_gha[DayGM_ann_gha$year>end_exp_period_year+1,]))
  MGxs <- c(end_exp_period_year+1, end_fut_period_year)
  MGys <- cbind(1, MGxs) %*% MGfit_Day
  
  gMGann <- CH4_ghayr_piv %>%
    ggplot(aes(x=year, y=ch4_val, color=source)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (g ha ' ^-1*'yr ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"Annual Cumulative CH"["4"]*" Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = MGxs[1], xend = MGxs[2], y = MGys[1], yend = MGys[2]), color=cbPalette9[8]) +
    scale_color_manual(labels=c("Daycent"),
                       values=c(Daycent_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMGann
  
  ## Carbon Input
  
  gCI <- DayCI_gm2yr %>%
    ggplot(aes(x=year, y=base), show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('C input (g C m' ^-2*' yr' ^-1*')')) +
    ggtitle(paste(site_name,"Soil C Input"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gCI
  
  gNI <- DayNI_gm2yr[DayNI_gm2yr$year >= experiment_start_date,] %>%
    ggplot(aes(x=year, y=base), show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N input (g C m' ^-2*' yr' ^-1*')')) +
    ylim(0,12) +
    ggtitle(paste(site_name,"Soil N Input"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNI
  
  gNH4 <- Day_soiln_all[Day_soiln_all$year >= experiment_start_date,] %>%
    ggplot(aes(x=year, y=ammonium)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('NH4 ppm per day')) +  
    ylim(0,125) +
    ggtitle(bquote(.(site_name)~"Soil NH"["4"]*" - top 10 cm"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNH4
  
  gNO3 <- Day_soiln_all[Day_soiln_all$year >= experiment_start_date,] %>%
    ggplot(aes(x=year, y=NO3_ppm)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('NO3 ppm per day')) +  
    ylim(0,125) +
    ggtitle(bquote(.(site_name)~"Soil NO"["3"]*" - top 20 cm"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNO3
  
  
  ggsave(filename=paste0(results_path,"Cotton_yield_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gCY,
         width=9, height=6, dpi=300)
  if(mgmt_scenario_grp != 7) {
  ggsave(filename=paste0(results_path,"Sorghum_yield_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gSY,
         width=9, height=6, dpi=300)
  }
  ggsave(filename=paste0(results_path,"SOC_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gC,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soil_Temp_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gT,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soil_Moist_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gM,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"N2O_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gNG,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"N2O_ann_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gNGann, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"CH4_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gMG,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"CH4_ann_comparison_fut_",scenario_name,"_Daycent.jpg"),plot=gMGann,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"C_input_fut_",scenario_name,"_Daycent.jpg"),plot=gCI,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"NH4_input_fut_",scenario_name,"_Daycent.jpg"),plot=gNH4,
         width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"NO3_input_fut_",scenario_name,"_Daycent.jpg"),plot=gNO3,
         width=9, height=6, dpi=300)
  
  #**********************************************************************
  
  # explain graphs ----------------------------
  
  
  # explain N2O emissions with WFPS, NO3 and N2O

  ## keep this just for illustration
  if(scenario_name=="1_1") {
    gN2O_expl_0to10cm_exp <- ggplot() +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer1, color=cbPalette9[2]), linewidth=1) +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer2, color=cbPalette9[3]), linewidth=1) +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer3, color=cbPalette9[4]), linewidth=1) +
      geom_line(data=Day_exp_soiln[Day_exp_soiln$year %in% 2010:2011,],
                aes(x=date,y=NO3_hgha/10, color=cbPalette9[6]), linewidth=1) +
      geom_line(data=N2O_ghaday[year(N2O_ghaday$date) %in% 2010:2011,],
                aes(x=date, y=Daycent/100, color=cbPalette9[8]), linewidth=1) +
      ggtitle(bquote(.(site_name)~"N"["2"]*"O emissions drivers to 10 cm in Daycent"),
              paste0("Scenario: ",scenario_descriptor_full)) +      
      ylab(expression('WFPS, NO'[3]*' (dg ha' ^'-1'*' day, N'[2]*'O  (cg ha' ^'-1'*' day'^'-1'*')')) +
      scale_color_manual(name=NULL,
                         labels=c("WFPS: 0-2.5cm","WFPS: 2.5-5 cm","WFPS: 5-10 cm",
                                  "NO3 (dg/ha/day)","N2O (cg/ha/day)"),
                         values=cbPalette9[c(2,3,4,6,8)]) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    
    gN2O_expl_0to10cm_exp
    
    gN2O_expl_10to60cm_exp <- ggplot() +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer4, color=cbPalette9[2]), linewidth=1) +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer5, color=cbPalette9[3]), linewidth=1) +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer6, color=cbPalette9[4]), linewidth=1) +
      geom_line(data=Day_exp_soiln[Day_exp_soiln$year %in% 2010:2011,],
                aes(x=date,y=NO3_hgha_10to60cm/10, color=cbPalette9[6]), linewidth=1) +
      geom_line(data=N2O_ghaday[year(N2O_ghaday$date) %in% 2010:2011,],
                aes(x=date, y=Daycent/100, color=cbPalette9[8]), linewidth=1) +
      ggtitle(bquote(.(site_name)~"N"["2"]*"O emissions drivers to 60 cm in Daycent"),
              paste0("Scenario: ",scenario_descriptor_full)) +      
      ylab(expression('WFPS, NO'[3]*' (dg ha' ^'-1'*' day, N'[2]*'O  (cg ha' ^'-1'*' day'^'-1'*')')) +
      scale_color_manual(name=NULL,
                         labels=c("WFPS: 10-20 cm","WFPS: 20-40 cm","WFPS: 40-60 cm",
                                  "NO3 (dg/ha/day)","N2O (cg/ha/day)"),
                         values=cbPalette9[c(2,3,4,6,8)]) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    
    gN2O_expl_10to60cm_exp
    
    ch4_transform_factor <- 100
    
    gCH4_expl_oxid_0to20cm_exp <- ggplot() +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer1, color=cbPalette9[2]), linewidth=1) +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer2, color=cbPalette9[3]), linewidth=1) +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer3, color=cbPalette9[4]), linewidth=1) +
      geom_line(data=Day_exp_wfps[Day_exp_wfps$year %in% 2010:2011,],
                aes(x=date, y=wfps_layer4, color=cbPalette9[5]), linewidth=1) +
      geom_line(data=Day_exp_soiltavg[Day_exp_soiltavg$year %in% 2010:2011,],
                aes(x=date, y=layer1/100, color=cbPalette9[7]), linewidth=1) +
      geom_line(data=Day_exp_methane[Day_exp_methane$year %in% 2010:2011,],
                aes(x=date,y=CH4_oxid*-1000, color=cbPalette9[8]), linewidth=1) +
      ggtitle(bquote(.(site_name)~"CH"["4"]*"O emissions drivers to 15 cm in Daycent"),
              paste0("Scenario: ",scenario_descriptor_full)) +      
      ylab(expression('WFPS, CH'[4]*'  (mg m' ^'-2'*' day'^'-1'*')')) +
      scale_y_continuous(
        sec.axis = sec_axis(trans = ~ .x * ch4_transform_factor,
                            name = expression('Soil Temperature ('^o*'C)'))
      ) +
      scale_color_manual(name=NULL,
                         labels=c("WFPS: 0-2.5cm","WFPS: 2.5-5 cm","WFPS: 5-10 cm",
                                  "WFPS: 10-20cm","Soil Temp","CH4 (mg/m^2/day"),
                         values=cbPalette9[c(2,3,4,5,7,8)]) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    
    gCH4_expl_oxid_0to20cm_exp
    
    ggsave(filename=paste0(results_path,"expl_N2O_0to10cm_exp",scenario_name,"_Daycent.jpg"),plot=gN2O_expl_0to10cm_exp,
           width=9, height=6, dpi=300)
    ggsave(filename=paste0(results_path,"expl_N2O_10to60cm_exp",scenario_name,"_Daycent.jpg"),plot=gN2O_expl_10to60cm_exp,
           width=9, height=6, dpi=300)
    ggsave(filename=paste0(results_path,"expl_CH4_oxid_0to20cm_exp",scenario_name,"_Daycent.jpg"),plot=gCH4_expl_oxid_0to20cm_exp,
           width=9, height=6, dpi=300)
    
    
  }
  
  ## explain N2O to 60 cm to end of future time period
  
  WFPS_this <- Day_fut_wfps[Day_fut_wfps$year %in% (end_exp_period_year+1):end_fut_period_year,]
  WFPS20cm_fit_time <- lm(wfps_layer4 ~ date, data = WFPS_this)
  WFPS20cm_fit_coef_time <- coef(WFPS20cm_fit_time)
  #
  WFPS30cm_fit_time <- lm(wfps_layer5 ~ date, data = WFPS_this)
  WFPS30cm_fit_coef_time <- coef(WFPS30cm_fit_time)
  #
  WFPS45cm_fit_time <- lm(wfps_layer6 ~ date, data = WFPS_this)
  WFPS45cm_fit_coef_time <- coef(WFPS45cm_fit_time)
  #
  WFPS60cm_fit_time <- lm(wfps_layer7 ~ date, data =  WFPS_this)
  WFPS60cm_fit_coef_time <- coef(WFPS60cm_fit_time)
  #
  NO3_this <- Day_fut_soiln[Day_fut_soiln$year %in% (end_exp_period_year+1):end_fut_period_year,]
  NO32cm_fit_time <- lm(NO3_2cm_kgha ~ date, data = NO3_this) 
  NO32cm_fit_coef_time <- coef(NO32cm_fit_time)
  NO35cm_fit_time <- lm(NO3_5cm_kgha ~ date, data = NO3_this) 
  NO35cm_fit_coef_time <- coef(NO35cm_fit_time)
  NO310cm_fit_time <- lm(NO3_10cm_kgha ~ date, data = NO3_this) 
  NO310cm_fit_coef_time <- coef(NO310cm_fit_time)
  NO320cm_fit_time <- lm(NO3_20cm_kgha ~ date, data = NO3_this) 
  NO320cm_fit_coef_time <- coef(NO320cm_fit_time)
  NO330cm_fit_time <- lm(NO3_30cm_kgha ~ date, data = NO3_this) 
  NO330cm_fit_coef_time <- coef(NO330cm_fit_time)
  NO345cm_fit_time <- lm(NO3_45cm_kgha ~ date, data = NO3_this) 
  NO345cm_fit_coef_time <- coef(NO345cm_fit_time)
  NO360cm_fit_time <- lm(NO3_60cm_kgha ~ date, data = NO3_this) 
  NO360cm_fit_coef_time <- coef(NO360cm_fit_time)
  NO30to60cm_fit_time <- lm(NO3_0to60cm_kgha ~ date, data = NO3_this)
  NO30to60cm_fit_coef_time <- coef(NO30to60cm_fit_time)
  #
  N2O_this <- N2O_ghaday[N2O_ghaday$year %in% (end_exp_period_year+1):end_fut_period_year,]
  N2Oprofile_fit_time <- lm(Daycent ~ date, data = N2O_this)
  N2Oprofile_fit_coef_time <- coef(N2Oprofile_fit_time)

  #NOTE: need to readjust units
  gN2O_expl_10to60cm_fut <- ggplot() +
    geom_line(data=WFPS_this,
              aes(x=date, y=wfps_layer4, color=cbPalette9[2]), linewidth=1) +
    geom_line(data=WFPS_this,
              aes(x=date, y=wfps_layer5, color=cbPalette9[3]), linewidth=1) +
    geom_line(data=WFPS_this,
              aes(x=date, y=wfps_layer6, color=cbPalette9[4]), linewidth=1) +
    geom_line(data=WFPS_this,
              aes(x=date, y=wfps_layer7, color=cbPalette9[4]), linewidth=1) +
    geom_line(data=NO3_this,
              aes(x=date,y=NO3_10to60cm_hgha/10, color=cbPalette9[6]), linewidth=1) +
    geom_line(data=N2O_this,
              aes(x=date, y=Daycent/1000, color=cbPalette9[8]), linewidth=1) +
    ggtitle(bquote(.(site_name)~"N"["2"]*"O emissions drivers 10 to 60 cm in Daycent"),
            paste0("Scenario: ",scenario_descriptor_full)) +      
    ylab(expression('WFPS, NO'[3]*' (dg ha' ^'-1'*' day' ^'-1'*', N'[2]*'O  (cg ha' ^'-1'*' day'^'-1'*')')) +
    scale_color_manual(name=NULL,
                       labels=c("WFPS: 10-20 cm","WFPS: 20-30 cm","WFPS: 30-45 cm",
                                "WFPS: 45-60 cm",
                                "NO3 (dg/ha/day)","N2O (cg/ha/day)"),
                       values=cbPalette9[c(2,3,4,4,6,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gN2O_expl_10to60cm_fut
  
  
  ## change in each over future period
  
  WFPS20cm_first <- as.numeric(lapply(WFPS20cm_fit_time["fitted.values"],dplyr::first))
  WFPS30cm_first <- as.numeric(lapply(WFPS30cm_fit_time["fitted.values"],dplyr::first))
  WFPS45cm_first <- as.numeric(lapply(WFPS45cm_fit_time["fitted.values"],dplyr::first))
  WFPS60cm_first <- as.numeric(lapply(WFPS60cm_fit_time["fitted.values"],dplyr::first))
  NO32cm_first <- as.numeric(lapply(NO32cm_fit_time["fitted.values"],dplyr::first))
  NO35cm_first <- as.numeric(lapply(NO35cm_fit_time["fitted.values"],dplyr::first))
  NO310cm_first <- as.numeric(lapply(NO310cm_fit_time["fitted.values"],dplyr::first))
  NO320cm_first <- as.numeric(lapply(NO320cm_fit_time["fitted.values"],dplyr::first))
  NO330cm_first <- as.numeric(lapply(NO330cm_fit_time["fitted.values"],dplyr::first))
  NO345cm_first <- as.numeric(lapply(NO345cm_fit_time["fitted.values"],dplyr::first))
  NO360cm_first <- as.numeric(lapply(NO360cm_fit_time["fitted.values"],dplyr::first))
  NO30to60cm_first <- as.numeric(lapply(NO30to60cm_fit_time["fitted.values"],dplyr::first))
  N2Oprofile_first <- as.numeric(lapply(N2Oprofile_fit_time["fitted.values"],dplyr::first))
  #
  WFPS20cm_last <- as.numeric(lapply(WFPS20cm_fit_time["fitted.values"],dplyr::last))
  WFPS30cm_last <- as.numeric(lapply(WFPS30cm_fit_time["fitted.values"],dplyr::last))
  WFPS45cm_last <- as.numeric(lapply(WFPS45cm_fit_time["fitted.values"],dplyr::last))
  WFPS60cm_last <- as.numeric(lapply(WFPS60cm_fit_time["fitted.values"],dplyr::last))
  NO32cm_last <- as.numeric(lapply(NO32cm_fit_time["fitted.values"],dplyr::last))
  NO35cm_last <- as.numeric(lapply(NO35cm_fit_time["fitted.values"],dplyr::last))
  NO310cm_last <- as.numeric(lapply(NO310cm_fit_time["fitted.values"],dplyr::last))
  NO320cm_last <- as.numeric(lapply(NO320cm_fit_time["fitted.values"],dplyr::last))
  NO330cm_last <- as.numeric(lapply(NO330cm_fit_time["fitted.values"],dplyr::last))
  NO345cm_last <- as.numeric(lapply(NO345cm_fit_time["fitted.values"],dplyr::last))
  NO360cm_last <- as.numeric(lapply(NO360cm_fit_time["fitted.values"],dplyr::last))
  NO30to60cm_last <- as.numeric(lapply(NO30to60cm_fit_time["fitted.values"],dplyr::last))
  N2Oprofile_last <- as.numeric(lapply(N2Oprofile_fit_time["fitted.values"],dplyr::last))
  #
  WFPS20cm_change <- WFPS20cm_last - WFPS20cm_first
  WFPS30cm_change <- WFPS30cm_last - WFPS30cm_first
  WFPS45cm_change <- WFPS45cm_last - WFPS45cm_first
  WFPS60cm_change <- WFPS60cm_last -WFPS60cm_first
  NO32cm_change <- NO32cm_last - NO32cm_first
  NO35cm_change <- NO35cm_last - NO35cm_first
  NO310cm_change <- NO310cm_last - NO310cm_first
  NO320cm_change <- NO320cm_last - NO320cm_first
  NO330cm_change <- NO330cm_last - NO330cm_first
  NO345cm_change <- NO345cm_last - NO345cm_first
  NO360cm_change <- NO360cm_last - NO360cm_first
  NO30to60cm_change <- NO30to60cm_last - NO30to60cm_first
  N2Oprofile_change <- N2Oprofile_last - N2Oprofile_first
  
  
  # explain CH4 in top 15 cm to end of future period
  
  # WFPS_this defined above
  #
  WFPS2cm_fit_time <- lm(wfps_layer1 ~ date, data = WFPS_this)
  WFPS2cm_fit_coef_time <- coef(WFPS2cm_fit_time)
  #
  WFPS5cm_fit_time <- lm(wfps_layer2 ~ date, data = WFPS_this)
  WFPS5cm_fit_coef_time <- coef(WFPS5cm_fit_time)
  #
  WFPS10cm_fit_time <- lm(wfps_layer3 ~ date, data =  WFPS_this)
  WFPS10cm_fit_coef_time <- coef( WFPS10cm_fit_time)
  #
  SoilT_this <- DayT_C[DayT_C$year %in% (end_exp_period_year+1):end_fut_period_year,]
  SoilT2cm_fit_time <- lm(layer1 ~ date, data = SoilT_this)
  SoilT2cm_fit_coef_time <- coef(SoilT2cm_fit_time)
  SoilT5cm_fit_time <- lm(layer2 ~ date, data = SoilT_this)
  SoilT5cm_fit_coef_time <- coef(SoilT5cm_fit_time)
  SoilT10cm_fit_time <- lm(layer3 ~ date, data = SoilT_this)
  SoilT10cm_fit_coef_time <- coef(SoilT10cm_fit_time)
  SoilT20cm_fit_time <- lm(layer4 ~ date, data = SoilT_this)
  SoilT20cm_fit_coef_time <- coef(SoilT20cm_fit_time)
  SoilT30cm_fit_time <- lm(layer5 ~ date, data = SoilT_this)
  SoilT30cm_fit_coef_time <- coef(SoilT30cm_fit_time)
  SoilT45cm_fit_time <- lm(layer6 ~ date, data = SoilT_this)
  SoilT45cm_fit_coef_time <- coef(SoilT45cm_fit_time)
  SoilT60cm_fit_time <- lm(layer7 ~ date, data = SoilT_this)
  SoilT60cm_fit_coef_time <- coef(SoilT60cm_fit_time)
  SoilT15cm_fit_time <- lm(mean_15cm ~ date, data = SoilT_this)
  SoilT15cm_fit_coef_time <- coef(SoilT15cm_fit_time)
  SoilT10cm_fit_time <- lm(mean_10cm ~ date, data = SoilT_this)
  SoilT10cm_fit_coef_time <- coef(SoilT10cm_fit_time)
  #
  CH4_this <- Day_fut_methane[Day_fut_methane$year %in% (end_exp_period_year+1):end_fut_period_year,]
  CH4fit_time <- lm(CH4_oxid ~ date, data = CH4_this)
  CH4fit_coef_time <- coef(CH4fit_time)
  
  
  ch4_transform_factor <- 100
  
  # NOTE: will need to readjust units
  gCH4_expl_oxid_2to15cm_fut <- ggplot() +
    geom_line(data=WFPS_this,
              aes(x=date, y=wfps_layer1, color=cbPalette9[2]), linewidth=1) +
    geom_line(data=WFPS_this,
              aes(x=date, y=wfps_layer2, color=cbPalette9[3]), linewidth=1) +
    geom_line(data=WFPS_this,
              aes(x=date, y=wfps_layer3, color=cbPalette9[4]), linewidth=1) +
    geom_line(data=WFPS_this,
              aes(x=date, y=wfps_layer4, color=cbPalette9[5]), linewidth=1) +
    geom_line(data=SoilT_this,
              aes(x=date, y=mean_15cm/100, color=cbPalette9[7]), linewidth=1) +
    geom_line(data=CH4_this,
              aes(x=date,y=CH4_oxid*-1000, color=cbPalette9[8]), linewidth=1) +
    ggtitle(bquote(.(site_name)~"CH"["4"]*"O emissions drivers to 15 cm in Daycent"),
            paste0("Scenario: ",scenario_descriptor_full)) +      
    ylab(expression('WFPS, CH'[4]*'  (mg m' ^'-2'*' day'^'-1'*')')) +
    scale_y_continuous(
      sec.axis = sec_axis(trans = ~ .x * ch4_transform_factor,
                          name = expression('Soil Temperature ('^o*'C)'))
    ) +
    scale_color_manual(name=NULL,
                       labels=c("WFPS: 0-2.5cm","WFPS: 2.5-5 cm","WFPS: 5-10 cm",
                                "WFPS: 10-20cm","Soil Temp","CH4 (mg/m^2/day"),
                       values=cbPalette9[c(2,3,4,5,7,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gCH4_expl_oxid_2to15cm_fut
  
  
  WFPSmean_this <- soilm_mean[year(soilm_mean$date) < end_fut_period_year,]
  WFPSmean_fit_time <- lm(mean_wfps ~ date, data = WFPSmean_this)
  WFPSmean_fit_coef_time <- coef(WFPSmean_fit_time)
  SoilTmean_this <- DayT_C
  SoilTmean_fit_time <- lm(mean_15cm ~ date, data = SoilTmean_this)
  SoilTmean_fit_coef_time <- coef(SoilTmean_fit_time)
  CH4mean_this <- Day_methane
  CH4mean_fit_time <- lm(CH4_oxid ~ date, data = CH4mean_this)
  CH4mean_fit_coef_time <- coef(CH4mean_fit_time)
  
  gCH4_expl_oxid_0to15cm_fut <- ggplot() +
    geom_line(data=WFPSmean_this,
              aes(x=date,y=mean_wfps, color=cbPalette9[2]), linewidth=1) +
    geom_line(data=SoilTmean_this,
              aes(x=date,y=mean_15cm/100, color=cbPalette9[7]), linewidth=1) +
    geom_line(data=CH4mean_this,
              aes(x=date,y=CH4_oxid*-1000, color=cbPalette9[8]), linewidth=1) +
    ggtitle(bquote(.(site_name)~"CH"["4"]*"O emissions drivers to 15 cm in Daycent"),
            paste0("Scenario: ",scenario_descriptor_full)) +      
    ylab(expression('WFPS, CH'[4]*'  (mg m' ^'-2'*' day'^'-1'*')')) +
    scale_y_continuous(
      sec.axis = sec_axis(trans = ~ .x * ch4_transform_factor,
                          name = expression('Soil Temperature ('^o*'C)'))
    ) +
    scale_color_manual(name=NULL,
                       labels=c("WFPS: 15cm mean","Soil Temp: 15cm mean","CH4 (mg/m^2/day)"),
                       values=cbPalette9[c(2,7,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gCH4_expl_oxid_0to15cm_fut
  
  

  ## change in each over future period
  WFPS2cm_first <-   as.numeric(lapply(WFPS2cm_fit_time["fitted.values"],dplyr::first))
  WFPS5cm_first <-   as.numeric(lapply(WFPS5cm_fit_time["fitted.values"],dplyr::first))
  WFPS10cm_first <-  as.numeric(lapply(WFPS10cm_fit_time["fitted.values"],dplyr::first))
  SoilT2cm_first <-  as.numeric(lapply(SoilT2cm_fit_time["fitted.values"],dplyr::first))
  SoilT5cm_first <-  as.numeric(lapply(SoilT5cm_fit_time["fitted.values"],dplyr::first))
  SoilT10cm_first <- as.numeric(lapply(SoilT10cm_fit_time["fitted.values"],dplyr::first))
  SoilT20cm_first <- as.numeric(lapply(SoilT20cm_fit_time["fitted.values"],dplyr::first))
  SoilT30cm_first <- as.numeric(lapply(SoilT30cm_fit_time["fitted.values"],dplyr::first))
  SoilT45cm_first <- as.numeric(lapply(SoilT45cm_fit_time["fitted.values"],dplyr::first))
  SoilT60cm_first <- as.numeric(lapply(SoilT60cm_fit_time["fitted.values"],dplyr::first))
  SoilT15cm_first <- as.numeric(lapply(SoilT15cm_fit_time["fitted.values"],dplyr::first))
  SoilT10cm_first <- as.numeric(lapply(SoilT10cm_fit_time["fitted.values"],dplyr::first))
  CH4_first       <-  as.numeric(lapply(CH4fit_time["fitted.values"],dplyr::first))
  #
  WFPS2cm_last <-   as.numeric(lapply(WFPS2cm_fit_time["fitted.values"],dplyr::last))
  WFPS5cm_last <-   as.numeric(lapply(WFPS5cm_fit_time["fitted.values"],dplyr::last))
  WFPS10cm_last <-  as.numeric(lapply(WFPS10cm_fit_time["fitted.values"],dplyr::last))
  SoilT2cm_last <-  as.numeric(lapply(SoilT2cm_fit_time["fitted.values"],dplyr::last))
  SoilT5cm_last <-  as.numeric(lapply(SoilT5cm_fit_time["fitted.values"],dplyr::last))
  SoilT10cm_last <- as.numeric(lapply(SoilT10cm_fit_time["fitted.values"],dplyr::last))
  SoilT20cm_last <- as.numeric(lapply(SoilT20cm_fit_time["fitted.values"],dplyr::last))
  SoilT30cm_last <- as.numeric(lapply(SoilT30cm_fit_time["fitted.values"],dplyr::last))
  SoilT45cm_last <- as.numeric(lapply(SoilT45cm_fit_time["fitted.values"],dplyr::last))
  SoilT60cm_last <- as.numeric(lapply(SoilT60cm_fit_time["fitted.values"],dplyr::last))
  SoilT15cm_last <- as.numeric(lapply(SoilT15cm_fit_time["fitted.values"],dplyr::last))
  CH4_last <-       as.numeric(lapply(CH4fit_time["fitted.values"],dplyr::last))
  #
  WFPS2cm_change <-   WFPS2cm_last  - WFPS2cm_first  
  WFPS5cm_change <-   WFPS5cm_last  - WFPS5cm_first 
  WFPS10cm_change <-  WFPS10cm_last  - WFPS10cm_first 
  SoilT2cm_change <-  SoilT2cm_last  - SoilT2cm_first 
  SoilT5cm_change <-  SoilT5cm_last  - SoilT5cm_first 
  SoilT10cm_change <- SoilT10cm_last - SoilT10cm_first
  SoilT20cm_change <- SoilT20cm_last - SoilT20cm_first
  SoilT30cm_change <- SoilT30cm_last - SoilT30cm_first
  SoilT45cm_change <- SoilT45cm_last - SoilT45cm_first
  SoilT60cm_change <- SoilT60cm_last - SoilT60cm_first
  SoilT15cm_change <- SoilT15cm_last - SoilT15cm_first
  CH4_change <-       CH4_last   - CH4_first     
  
  ## Carbon Input
  
  # CI_this <- DayCI_gm2yr[DayCI_gm2yr$year %in% (end_exp_period_year+1):end_fut_period_year,]
  CI_this_raw <- read_csv(paste0(obs_mgmt_path,"Daycent_Cinput_",scenario_name,".csv"),
                          col_names = TRUE, show_col_types = F)
  CI_this <- CI_this_raw[CI_this_raw$year %in% (end_exp_period_year+1):end_fut_period_year,]
  CI_fit_time <- lm(daily_soilC_gm2 ~ year, data = CI_this)
  CI_fit_coef_time <- coef(CI_fit_time)
  
  gCI_fut <- CI_this %>%
    ggplot(aes(x=date, y=daily_soilC_gm2), show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('C input (g C m' ^-2*' yr' ^-1*')')) +
    ggtitle(paste(site_name,"Soil C Input"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gCI_fut
  
  ## change over future period
  CI_first <- as.numeric(lapply(CI_fit_time["fitted.values"],dplyr::first))
  CI_last <- as.numeric(lapply(CI_fit_time["fitted.values"],dplyr::last))
  CI_change <- CI_last - CI_first
  
  
  ## explain SOC to 10 cm to end of future time period
  
  SMoist_this <- DayM_V[DayM_V$year %in% end_exp_period_year:end_fut_period_year,]
  SW2cm_fit_time <- lm(layer1 ~ date, data = SMoist_this) # already a fraction
  SW2cm_fit_coef_time <- coef(SW2cm_fit_time)
  SW5cm_fit_time <- lm(layer2 ~ date, data = SMoist_this) # already a fraction
  SW5cm_fit_coef_time <- coef(SW5cm_fit_time)
  SW10cm_fit_time <- lm(layer3 ~ date, data = SMoist_this) # already a fraction
  SW10cm_fit_coef_time <- coef(SW10cm_fit_time)
  SW20cm_fit_time <- lm(layer4 ~ date, data = SMoist_this) # already a fraction
  SW20cm_fit_coef_time <- coef(SW20cm_fit_time)
  SW30cm_fit_time <- lm(layer5 ~ date, data = SMoist_this) # already a fraction
  SW30cm_fit_coef_time <- coef(SW30cm_fit_time)
  SW45cm_fit_time <- lm(layer6 ~ date, data = SMoist_this) # already a fraction
  SW45cm_fit_coef_time <- coef(SW45cm_fit_time)
  SW60cm_fit_time <- lm(layer7 ~ date, data = SMoist_this) # already a fraction
  SW60cm_fit_coef_time <- coef(SW60cm_fit_time)
  ## depth of water 
  DW2cm_fit_time <- lm(DW_2cm ~ date, data = SMoist_this) 
  DW2cm_fit_coef_time <- coef(DW2cm_fit_time)
  DW5cm_fit_time <- lm(DW_5cm ~ date, data = SMoist_this) 
  DW5cm_fit_coef_time <- coef(DW5cm_fit_time)
  DW10cm_fit_time <- lm(DW_10cm ~ date, data = SMoist_this) 
  DW10cm_fit_coef_time <- coef(DW10cm_fit_time)
  DW20cm_fit_time <- lm(DW_20cm ~ date, data = SMoist_this) 
  DW20cm_fit_coef_time <- coef(DW20cm_fit_time)
  DW30cm_fit_time <- lm(DW_30cm ~ date, data = SMoist_this)
  DW30cm_fit_coef_time <- coef(DW30cm_fit_time)
  DW45cm_fit_time <- lm(DW_45cm ~ date, data = SMoist_this)
  DW45cm_fit_coef_time <- coef(DW45cm_fit_time)
  DW60cm_fit_time <- lm(DW_60cm ~ date, data = SMoist_this)
  DW60cm_fit_coef_time <- coef(DW60cm_fit_time)
  DW0to60cm_fit_time <- lm((DW_2cm+DW_5cm+DW_10cm+DW_20cm+DW_30cm+DW_45cm+DW_60cm) ~ date, data = SMoist_this)
  DW0to60cm_fit_coef_time <- coef(DW0to60cm_fit_time)
  DW10cm_fit_time <- lm(DW_10cm ~ date, data = SMoist_this) 
  DW10cm_fit_coef_time <- coef(DW10cm_fit_time)
  
  # SoilT defined above
  
  # C input defined above
  
  SOC_this <- DayC_Mgha[DayC_Mgha$year %in% end_exp_period_year:end_fut_period_year,]
  SOC10cm_fit_time <- lm(base ~ year, data = SOC_this) # convert to fraction
  SOC10cm_fit_coef_time <- coef(SOC10cm_fit_time)
  
  # join with CI just because SOC needs to be in a daily timestep for graphing
  CI_SOC_daily <- left_join(CI_this, SOC_this, by="year")
  
  soilT_transform_factor <- 100
  
  cols <- c("Cin" = Cin_10cm_color,"SOC" = TotalSOC_10cm_color,
            "SoilT" = SoilT_color, "VSM" = SW_10cm_color)
  
  gSOC_expl_fut <- ggplot() +
    geom_smooth(data=SMoist_this,
                aes(x=date,y=SW_10cm, color="VSM"),
                linewidth=1,
                method=lm,
                se=FALSE) +
    geom_smooth(data=SoilT_this,
                aes(x=date, y=mean_10cm/100, color="SoilT"),
                method=lm,
                se=FALSE) +
    geom_smooth(data=CI_this,
                aes(x=date, y=daily_soilC_gm2, color="Cin"),
                linewidth=1,
                se=FALSE,
                method=lm)+
    geom_line(data=CI_SOC_daily,
              aes(x=date,y=base/100, color="SOC")) +
    ggtitle(bquote(.(site_name)~"SOC drivers to 10 cm in Daycent"),
            paste0("Scenario: ",scenario_descriptor_full)) +      
    ylab(expression('C input (g C m' ^'-2'*' day' ^'-1'*'), SOC stock (Mg C ha' ^'-1'*'/100), VWC')) +
    scale_y_continuous(
      sec.axis = sec_axis(trans = ~ .x * soilT_transform_factor,
                          name = expression('Soil Temperature ('^o*'C)'))
    ) +
    scale_color_manual(name=NULL,
                       values=cols,
                       labels=c("C input","SOC","Soil Temp","VWC")
    ) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gSOC_expl_fut
  
  ggsave(filename=paste0(results_path,"expl_N2O_10to60cm_fut_",scenario_name,"_Daycent.jpg"),
         plot=gN2O_expl_10to60cm_fut,width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"expl_CH4_oxid_0to15cm_mean_fut",scenario_name,"_Daycent.jpg"),
         plot=gCH4_expl_oxid_0to15cm_fut, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"expl_CI_0to10cm_fut_",scenario_name,"_Daycent.jpg"),
         plot=gCI_fut,width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"expl_SOC_0to10cm_fut_",scenario_name,"_Daycent.jpg"),
         plot=gSOC_expl_fut, width=9, height=6, dpi=300)
  
  
  ## change in each over future period
  
  SW2cm_first  <-   as.numeric(lapply(SW2cm_fit_time["fitted.values"],dplyr::first))
  SW5cm_first  <-   as.numeric(lapply(SW5cm_fit_time["fitted.values"],dplyr::first))
  SW10cm_first <-  as.numeric(lapply(SW10cm_fit_time["fitted.values"],dplyr::first))
  SW20cm_first <-  as.numeric(lapply(SW20cm_fit_time["fitted.values"],dplyr::first))
  SW30cm_first <-  as.numeric(lapply(SW30cm_fit_time["fitted.values"],dplyr::first))
  SW45cm_first <-  as.numeric(lapply(SW45cm_fit_time["fitted.values"],dplyr::first))
  SW60cm_first <-  as.numeric(lapply(SW60cm_fit_time["fitted.values"],dplyr::first))
  DW2cm_first  <-   as.numeric(lapply(DW2cm_fit_time["fitted.values"],dplyr::first))
  DW5cm_first  <-   as.numeric(lapply(DW5cm_fit_time["fitted.values"],dplyr::first))
  DW10cm_first <-  as.numeric(lapply(DW10cm_fit_time["fitted.values"],dplyr::first))
  DW20cm_first <-  as.numeric(lapply(DW20cm_fit_time["fitted.values"],dplyr::first))
  DW30cm_first <-  as.numeric(lapply(DW30cm_fit_time["fitted.values"],dplyr::first))
  DW45cm_first <-  as.numeric(lapply(DW45cm_fit_time["fitted.values"],dplyr::first))
  DW60cm_first <-  as.numeric(lapply(DW60cm_fit_time["fitted.values"],dplyr::first))
  DW0to60cm_first <-  as.numeric(lapply(DW0to60cm_fit_time["fitted.values"],dplyr::first))
  DW10cm_first <-  as.numeric(lapply(DW10cm_fit_time["fitted.values"],dplyr::first))
  SOC10cm_first <- as.numeric(lapply(SOC10cm_fit_time["fitted.values"],dplyr::first))
  #
  SW2cm_last  <-   as.numeric(lapply(SW2cm_fit_time["fitted.values"],dplyr::last))
  SW5cm_last  <-   as.numeric(lapply(SW5cm_fit_time["fitted.values"],dplyr::last))
  SW10cm_last <-  as.numeric(lapply(SW10cm_fit_time["fitted.values"],dplyr::last))
  SW20cm_last <-  as.numeric(lapply(SW20cm_fit_time["fitted.values"],dplyr::last))
  SW30cm_last <-  as.numeric(lapply(SW30cm_fit_time["fitted.values"],dplyr::last))
  SW45cm_last <-  as.numeric(lapply(SW45cm_fit_time["fitted.values"],dplyr::last))
  SW60cm_last <-  as.numeric(lapply(SW60cm_fit_time["fitted.values"],dplyr::last))
  DW2cm_last  <-   as.numeric(lapply(DW2cm_fit_time["fitted.values"],dplyr::last))
  DW5cm_last  <-   as.numeric(lapply(DW5cm_fit_time["fitted.values"],dplyr::last))
  DW10cm_last <-  as.numeric(lapply(DW10cm_fit_time["fitted.values"],dplyr::last))
  DW20cm_last <-  as.numeric(lapply(DW20cm_fit_time["fitted.values"],dplyr::last))
  DW30cm_last <-  as.numeric(lapply(DW30cm_fit_time["fitted.values"],dplyr::last))
  DW45cm_last <-  as.numeric(lapply(DW45cm_fit_time["fitted.values"],dplyr::last))
  DW60cm_last <-  as.numeric(lapply(DW60cm_fit_time["fitted.values"],dplyr::last))
  DW0to60cm_last <-  as.numeric(lapply(DW0to60cm_fit_time["fitted.values"],dplyr::last))
  DW10cm_last <-  as.numeric(lapply(DW10cm_fit_time["fitted.values"],dplyr::last))
  SOC10cm_last <- as.numeric(lapply(SOC10cm_fit_time["fitted.values"],dplyr::last))
  #
  SW2cm_change <-   SW2cm_last   - SW2cm_first  
  SW5cm_change <-   SW5cm_last   - SW5cm_first  
  SW10cm_change <-  SW10cm_last  - SW10cm_first 
  SW20cm_change <-  SW20cm_last  - SW20cm_first 
  SW30cm_change <-  SW30cm_last  - SW30cm_first 
  SW45cm_change <-  SW45cm_last  - SW45cm_first 
  SW60cm_change <-  SW60cm_last  - SW60cm_first 
  DW2cm_change <-   DW2cm_last   - DW2cm_first  
  DW5cm_change <-   DW5cm_last   - DW5cm_first  
  DW10cm_change <-  DW10cm_last  - DW10cm_first 
  DW20cm_change <-  DW20cm_last  - DW20cm_first 
  DW30cm_change <-  DW30cm_last  - DW30cm_first 
  DW45cm_change <-  DW45cm_last  - DW45cm_first 
  DW60cm_change <-  DW60cm_last  - DW60cm_first 
  DW0to60cm_change <-  DW0to60cm_last  - DW0to60cm_first 
  DW10cm_change <-  DW10cm_last  - DW10cm_first 
  SOC10cm_change <- SOC10cm_last - SOC10cm_first  
  
  
  
  #**********************************************************************
  
  # Log results -------------------------------------------------------------
  
  # add this run's results to  file collecting all final model runs
  fut_log_tab <- cbind(model_name,
                       clim_scenario_num,mgmt_scenario_num, 
                       scenario_name,scenario_abbrev,
                       SW5cm_first,SW5cm_change,
                       NA, NA,
                       NA, NA,
                       SW60cm_first,SW60cm_change,
                       DW5cm_first,DW5cm_change,
                       NA, NA,
                       NA, NA,
                       DW60cm_first,DW60cm_change,
                       DW0to60cm_first,DW0to60cm_change,
                       SW10cm_first,SW10cm_change,
                       DW10cm_first,DW10cm_change,
                       SoilT5cm_first,SoilT5cm_change,
                       SoilT15cm_first,SoilT15cm_change,
                       NA, NA,
                       SoilT60cm_first,SoilT60cm_change,
                       SoilT10cm_first,SoilT10cm_change,
                       NO35cm_first,NO35cm_change,
                       NA, NA,
                       NA, NA,
                       NO360cm_first,NO360cm_change,
                       NO30to60cm_first,NO30to60cm_change,
                       NA, NA, # N2O to 5cm (Daycent doesn't give output by layer)
                       NA, NA, # N2O to 15cm
                       NA, NA, # N2O to 35cm
                       NA, NA, # N2O to 60cm
                       NA, NA, # N2O 0 to 60cm
                       N2Oprofile_first, N2Oprofile_change,
                       NA, NA, # BC
                       NA, NA, # BN
                       NA, NA, # HC
                       NA, NA, # HN
                       NA, NA, # CinB
                       NA, NA, # CinH
                       NA, NA, # CinBtoH
                       SOC10cm_first, SOC10cm_change,
                       # Additional Daycent-only fields
                       SW2cm_first, SW2cm_change,
                       SW20cm_first, SW20cm_change,
                       SW30cm_first, SW30cm_change,
                       SW45cm_first, SW45cm_change,
                       DW2cm_first, DW2cm_change,
                       DW10cm_first, DW10cm_change,
                       DW20cm_first, DW20cm_change,
                       DW30cm_first, DW30cm_change,
                       DW45cm_first, DW45cm_change,
                       WFPS2cm_first, WFPS2cm_change,
                       WFPS5cm_first, WFPS5cm_change,
                       WFPS10cm_first, WFPS10cm_change,
                       WFPS20cm_first, WFPS20cm_change,
                       WFPS30cm_first, WFPS30cm_change,
                       WFPS45cm_first, WFPS45cm_change,
                       WFPS60cm_first, WFPS60cm_change,
                       SoilT2cm_first, SoilT2cm_change,
                       SoilT20cm_first, SoilT20cm_change,
                       SoilT30cm_first, SoilT30cm_change,
                       SoilT45cm_first, SoilT45cm_change,
                       NO32cm_first, NO32cm_change,
                       NO310cm_first, NO310cm_change,
                       NO320cm_first, NO320cm_change,
                       NO330cm_first, NO330cm_change,
                       NO345cm_first, NO345cm_change,
                       CH4_first, CH4_change,
                       CI_first, CI_change)
  
  
  source(paste0("p_Edit_future_file_",site_name,".R"))
  p_Edit_future_file(fut_log_tab,model_name,scenario_name)
  
  #**********************************************************************
  
  rm(WFPS_this,WFPS10fit_time,WFPS10fit_coef_time,
     WFPS20fit_time,WFPS20fit_coef_time,
     WFPS40fit_time,WFPS40fit_coef_time,
     NO3_this,NO3fit_time,NO3fit_coef_time,
     N2O_this,N2Ofit_time,N2Ofit_coef_time,
     WFPS0fit_time,WFPS0fit_coef_time,
     WFPS2fit_time,WFPS2fit_coef_time,
     WFPS5fit_time,WFPS5fit_coef_time,
     SoilT_this,SoilTfit_time,SoilTfit_coef_time,
     CH4_this,CH4fit_time,CH4fit_coef_time,
     DW5cm_first,DW5cm_change,
     DW60cm_first,DW60cm_change,
     DW0to60cm_first,DW0to60cm_change,
     SW10cm_first,SW10cm_change,
     DW10cm_first,DW10cm_change,
     SoilT5cm_first,SoilT5cm_change,
     SoilT15cm_first,SoilT15cm_change,
     SoilT35cm_first,SoilT35cm_change,
     SoilT60cm_first,SoilT60cm_change,
     SoilT10cm_first,SoilT10cm_change,
     NO35cm_first,NO35cm_change,
     NO315cm_first,NO315cm_change,
     NO335cm_first,NO335cm_change,
     NO360cm_first,NO360cm_change,
     NO30to60cm_first,NO30to60cm_change,
     N2Oprofile_first, N2Oprofile_change,
     SOC10cm_first, SOC10cm_change,
     DW2cm_first, DW2cm_change,
     DW10cm_first, DW10cm_change,
     DW20cm_first, DW10cm_change,
     DW30cm_first, DW10cm_change,
     DW45cm_first, DW10cm_change,
     WFPS2cm_first, WFPS2cm_change,
     WFPS5cm_first, WFPS5cm_change,
     WFPS10cm_first, WFPS10cm_change,
     WFPS20cm_first, WFPS20cm_change,
     WFPS30cm_first, WFPS30cm_change,
     WFPS45cm_first, WFPS45cm_change,
     WFPS60cm_first, WFPS60cm_change,
     SoilT2cm_first, SoilT2cm_change,
     NO32cm_first, NO32cm_change,
     NO310cm_first, NO310cm_change,
     CH4_first, CH4_change,
     CI_first, CI_change
  )
  
  
}) # end suppressMessages
