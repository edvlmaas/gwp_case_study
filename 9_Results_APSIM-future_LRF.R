#######################################
# Script: 9_Results_APSIM-future_LRF.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Runs graphs for the future period for the APSIM simulation
# at KBS, MI. Some graphs include data from the experimental period too.
# Also logs a selection of results for final processing # in the "10_" 
# series of scripts.
#######################################

suppressMessages({
  
  print(paste0("Starting 9_Results_APSIM-future_",site_name,".R"))
  
  library(apsimx)
  library(readxl)
  library(magrittr)
  library(lubridate)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  
  # Temporal graphs
  # experimental -> future period
  
  ## Cotton
  
  CYfit_APSIM <- coef(lm(APSIM ~ year, 
                         data = CottonYld_Mgha[CottonYld_Mgha$year>end_exp_period_year,]))
  CYfit_Obs <- coef(lm(Observed ~ year, 
                       data = CottonYld_Mgha[CottonYld_Mgha$year %in% experiment_year_range,]))
  CYxs <- c(end_exp_period_year+1, end_fut_period_year)
  CYys <- cbind(1, CYxs) %*% CYfit_APSIM
  CYobsxs <- c(experiment_start_year, experiment_end_year)
  CYobsys <- cbind(1, CYobsxs) %*% CYfit_Obs
  
  gCY <- CottonYld_Mgha_piv[CottonYld_Mgha_piv$source %in% c("APSIM","Observed"),] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Cotton Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
#    geom_abline(intercept=CYfit_APSIM[1], slope=CYfit_APSIM[2], color="orange") +
    geom_segment(aes(x = CYxs[1], xend = CYxs[2], y = CYys[1], yend = CYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = CYobsxs[1], xend = CYobsxs[2], y = CYobsys[1], yend = CYobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=c(APSIM_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gCY
  
  gChY <- CottonYld_Mgha_piv %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Cotton Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Cotton Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_vline(xintercept=1987,linetype=2) +
    geom_vline(xintercept=2003,linetype=2) +
    geom_vline(xintercept=2010,linetype=2) +
    scale_color_manual(labels=c("APSIM","Historical","Observed"),
                       values=c(APSIM_color,Historical_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gChY
  
  if(mgmt_scenario_grp!=7) {
  ## Sorghum
  
  SYfit_APSIM <- coef(lm(APSIM ~ year, 
                         data = SorghumYld_Mgha[SorghumYld_Mgha$year>end_exp_period_year,]))
  SYfit_Obs <- coef(lm(Observed ~ year, 
                       data = SorghumYld_Mgha[SorghumYld_Mgha$year %in% experiment_year_range,]))
  SYxs <- c(end_exp_period_year+1, end_fut_period_year)
  SYys <- cbind(1, SYxs) %*% SYfit_APSIM
  SYobsxs <- c(experiment_start_year, experiment_end_year)
  SYobsys <- cbind(1, SYobsxs) %*% SYfit_Obs
  
  gSY <- SorghumYld_Mgha_piv[SorghumYld_Mgha_piv$source %in% c("APSIM","Observed"),] %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Sorghum Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = SYxs[1], xend = SYxs[2], y = SYys[1], yend = SYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = SYobsxs[1], xend = SYobsxs[2], y = SYobsys[1], yend = SYobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=c(APSIM_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSY

  gShY <- SorghumYld_Mgha_piv %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Sorghum Yield (Mg ha ' ^-1*')')) +
    ylim(0,4) +
    ggtitle(paste(site_name,"Sorghum Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Historical","Observed"),
                       values=c(APSIM_color,Historical_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gShY
  
  }

  ## SOC
  
  Cfit_APSIM <- coef(lm(APSIM ~ year, data = Cstock_Mgha))
  if(mgmt_scenario_grp==3) {
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                              Cstock_Mgha$year >= experiment_start_year,]))
  } else {
    Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
  }
  
  Cxs <- c(end_exp_period_year+1, end_fut_period_year)
  Cys <- cbind(1, Cxs) %*% Cfit_APSIM
  Cobsxs <- c(experiment_start_year, experiment_end_year)
  Cobsys <- cbind(1, Cobsxs) %*% Cfit_Obs
  
  gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year>=experiment_start_year,] %>%
    ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
    ggtitle(paste(site_name,"Soil Organic Carbon"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = Cxs[1], xend = Cxs[2], y = Cys[1], yend = Cys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = Cobsxs[1], xend = Cobsxs[2], y = Cobsys[1], yend = Cobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=c(APSIM_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gC 
  
  ## soil temp
  
  gT <- SoilTemp_C_piv[SoilTemp_C_piv$source=='APSIM' & SoilTemp_C_piv$year %in% ObsTemp$year,] %>%
    ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
    geom_point() +
    geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed' & SoilTemp_C_piv$year %in% ObsTemp$year,],
               aes(x=date, y=temp_val, color=source)) +
    xlab("Year") +
    ylab(expression('Soil temperature (' ^o*'C)')) +
    ggtitle(paste0(site_name," Soil Temperature"),paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=c(APSIM_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gT
  
  gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='APSIM'
                          &SoilMoist_VSM_piv$date>=experiment_start_date,] %>%
    ggplot(aes(x=date, y=h2o_val, color=source)) +
    geom_point() +
    # geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed'
    #                                   &SoilMoist_VSM_piv$date>=experiment_start_date,],
    #            aes(x=date, y=h2o_val, color=source)) +
    xlab("Year") +
    ylab("Volumetric soil moisture (%)") +
    ggtitle(paste0(site_name," Volumetric soil moisture with ",soil_moist_bias,"% correction"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=c(APSIM_color,Observed_color)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gM
  
  # Daily N2O
  
  gNG <- N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM',] %>%
    ggplot(aes(x=date, y=n2o_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed',],
               aes(x=date, y=n2o_val, color=source)) +
    geom_segment(data=Fert[Fert$treatment==treatment & Fert$totalN_kgha>10,],
                 aes(x = date, y = 200,
                     xend = date, yend = 175),
                 colour=cbPalette9[7],
                 show.legend=F,
                 lineend = "round",
                 linejoin = "round",
                 arrow = arrow(length = unit(0.3, "cm"))
                 # colour = "black" 
    ) + 
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (g ha ' ^-1*'day ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Observed","Fertilizer"),
                       values=c(APSIM_color,Observed_color,Fertilizer_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNG

  # Annual N2O
  
  NGfit_APSIM <- coef(lm(N2OEmissions_ghayr ~ year, 
                         data = APSIMGN_ann_gha[APSIMGN_ann_gha$year>end_exp_period_year,]))
  NGxs <- c(end_exp_period_year+1, end_fut_period_year)
  NGys <- cbind(1, NGxs) %*% NGfit_APSIM

  gNGann <- N2O_ghayr_piv %>%
    ggplot(aes(x=year, y=n2o_val, color=source)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (g ha ' ^-1*'yr ' ^-1*')')) +
    ggtitle(bquote(.(site_name)~"Annual Cumulative N"["2"]*"O Emissions"),
            paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = NGxs[1], xend = NGxs[2], y = NGys[1], yend = NGys[2]), color=cbPalette9[8]) +
    scale_color_manual(labels=c("APSIM"),
                       values=c(APSIM_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNGann
  
  ggsave(filename=paste0(results_path,"Cotton_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gCY, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Cotton_hist_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gChY, width=9, height=6, dpi=300)
  if(mgmt_scenario_grp!=7) {
    ggsave(filename=paste0(results_path,"Sorghum_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gSY, width=9, height=6, dpi=300)
    ggsave(filename=paste0(results_path,"Sorghum_hist_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
           plot=gShY, width=9, height=6, dpi=300)
  }
  ggsave(filename=paste0(results_path,"SOC_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gC, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soil_Temp_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gT, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soil_Moist_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gM, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"N2O_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gNG, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"N2O_ann_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gNGann, width=9, height=6, dpi=300)
  
  #**********************************************************************
  
  # explain graphs ----------------------------
  
  
  ## explain N2O to 60 cm to end of future time period
  
  SMoist_this <- APSIMM_V[APSIMM_V$year %in% (end_exp_period_year+1):end_fut_period_year,]
  ## volumetric soil water content (convert to fraction)
  SW5cm_fit_time <- lm(VolH2O_5cm/100 ~ date, data = SMoist_this)
  SW5cm_fit_coef_time <- coef(SW5cm_fit_time)
  SW15cm_fit_time <- lm(VolH2O_15cm/100 ~ date, data = SMoist_this)
  SW15cm_fit_coef_time <- coef(SW15cm_fit_time)
  SW35cm_fit_time <- lm(VolH2O_35cm/100 ~ date, data = SMoist_this)
  SW35cm_fit_coef_time <- coef(SW35cm_fit_time)
  SW60cm_fit_time <- lm(VolH2O_60cm/100 ~ date, data = SMoist_this)
  SW60cm_fit_coef_time <- coef(SW60cm_fit_time)
  ## depth of water 
  DW5cm_fit_time <- lm(DW_5cm ~ date, data = SMoist_this) 
  DW5cm_fit_coef_time <- coef(DW5cm_fit_time)
  DW15cm_fit_time <- lm(DW_15cm ~ date, data = SMoist_this) 
  DW15cm_fit_coef_time <- coef(DW15cm_fit_time)
  DW35cm_fit_time <- lm(DW_35cm ~ date, data = SMoist_this)
  DW35cm_fit_coef_time <- coef(DW35cm_fit_time)
  DW60cm_fit_time <- lm(DW_60cm ~ date, data = SMoist_this)
  DW60cm_fit_coef_time <- coef(DW60cm_fit_time)
  DW0to60cm_fit_time <- lm((DW_5cm+DW_15cm+DW_35cm+DW_60cm) ~ date, data = SMoist_this)
  DW0to60cm_fit_coef_time <- coef(DW0to60cm_fit_time)
  #
  NO3_this <- APSIMNO3_ghaday[APSIMNO3_ghaday$year %in% (end_exp_period_year+1):end_fut_period_year,] %>%
    mutate(NO3_0to60cm=NO3_5cm+NO3_15cm+NO3_35cm+NO3_60cm)
  NO35cm_fit_time <- lm(NO3_5cm ~ date, data = NO3_this)
  NO35cm_fit_coef_time <- coef(NO35cm_fit_time)
  NO315cm_fit_time <- lm(NO3_15cm ~ date, data = NO3_this)
  NO315cm_fit_coef_time <- coef(NO315cm_fit_time)
  NO335cm_fit_time <- lm(NO3_35cm ~ date, data = NO3_this)
  NO335cm_fit_coef_time <- coef(NO335cm_fit_time)
  NO360cm_fit_time <- lm(NO3_60cm ~ date, data = NO3_this)
  NO360cm_fit_coef_time <- coef(NO360cm_fit_time)
  NO30to60cm_fit_time <- lm(NO3_0to60cm ~ date, data = NO3_this)
  NO30to60cm_fit_coef_time <- coef(NO30to60cm_fit_time)
  #
  N2O_this <- APSIM_out[APSIM_out$year %in% (end_exp_period_year+1):end_fut_period_year,
                        c("date","year","N2O_bylayer_kgha(1)",
                          "N2O_bylayer_kgha(2)","N2O_bylayer_kgha(3)",
                          "N2O_bylayer_kgha(4)","N2O_profile_kgha")] %>%
    mutate(N2O_5cm_ghaday = round(`N2O_bylayer_kgha(1)`*1000,2),
           N2O_15cm_ghaday = round(`N2O_bylayer_kgha(2)`*1000,2),
           N2O_35cm_ghaday = round(`N2O_bylayer_kgha(3)`*1000,2),
           N2O_60cm_ghaday = round(`N2O_bylayer_kgha(4)`*1000,2),
           N2O_0to60cm_ghaday = N2O_5cm_ghaday + N2O_15cm_ghaday + 
             N2O_35cm_ghaday + N2O_60cm_ghaday)
  N2O5cm_fit_time <- lm(N2O_5cm_ghaday ~ date, data = N2O_this)
  N2O5cm_fit_coef_time <- coef(N2O5cm_fit_time)
  N2O15cm_fit_time <- lm(N2O_15cm_ghaday ~ date, data = N2O_this)
  N2O15cm_fit_coef_time <- coef(N2O15cm_fit_time)
  N2O35cm_fit_time <- lm(N2O_35cm_ghaday ~ date, data = N2O_this)
  N2O35cm_fit_coef_time <- coef(N2O35cm_fit_time)
  N2O60cm_fit_time <- lm(N2O_60cm_ghaday ~ date, data = N2O_this)
  N2O60cm_fit_coef_time <- coef(N2O60cm_fit_time)
  N2O0to60cm_fit_time <- lm(N2O_0to60cm_ghaday ~ date, data = N2O_this)
  N2O0to60cm_fit_coef_time <- coef(N2O0to60cm_fit_time)
  N2Oprofile_fit_time <- lm(N2O_profile_kgha*1000 ~ date, data = N2O_this) # convert to ghaday for consistency
  N2Oprofile_fit_coef_time <- coef(N2Oprofile_fit_time)

  # NOTE: need to readjust units for NO3 and N2O
  gN2O_expl_fut <- ggplot() +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_5cm/100, color=cbPalette12[2]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_15cm/100, color=cbPalette12[2]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_35cm/100, color=cbPalette12[3]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_60cm/100, color=cbPalette12[4]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=dul_5cm, color=cbPalette12[9]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=dul_15cm, color=cbPalette12[9]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=sat_5cm, color=cbPalette12[7]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=sat_15cm, color=cbPalette12[7]), linewidth=1) +
    geom_line(data=NO3_this,
              aes(x=date,y=NO3_15cm/1000000, color=cbPalette12[6]), linewidth=1) +
    geom_line(data=N2O_this,
              aes(x=date, y=N2O_15cm_ghaday/1000, color=cbPalette12[8]), linewidth=1) +
    ggtitle(bquote(.(site_name)~"N"["2"]*"O emissions drivers to 60 cm in APSIM"),
            paste0("Scenario: ",scenario_descriptor_full)) +      
    ylab(expression('VWC, DUL, SAT, NO'[3]*' (dg ha' ^'-1'*' day' ^'-1'*', N'[2]*'O  (cg ha' ^'-1'*' day'^'-1'*')')) +
    scale_color_manual(name=NULL,
                       labels=c("VWC: 0-20 cm","VWC: 20-40 cm","VWC: 40-60 cm",
                                "NO3 (dg/ha/day)","SAT","N2O (cg/ha/day)","DUL"), #),
                       values=cbPalette9[c(2,3,4,6,7,8,9)])+ #,7,6,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gN2O_expl_fut
  
  ggsave(filename=paste0(results_path,"expl_N2O_0to60cm_fut_",scenario_name,"_APSIM.jpg"),
         plot=gN2O_expl_fut, width=9, height=6, dpi=300)
  
  
  ## change in each over future period
  
  SW5cm_first <- as.numeric(lapply(SW5cm_fit_time["fitted.values"],dplyr::first))
  SW15cm_first <- as.numeric(lapply(SW15cm_fit_time["fitted.values"],dplyr::first))
  SW35cm_first <- as.numeric(lapply(SW35cm_fit_time["fitted.values"],dplyr::first))
  SW60cm_first <- as.numeric(lapply(SW60cm_fit_time["fitted.values"],dplyr::first))
  DW5cm_first <- as.numeric(lapply(DW5cm_fit_time["fitted.values"],dplyr::first))
  DW15cm_first <- as.numeric(lapply(DW15cm_fit_time["fitted.values"],dplyr::first))
  DW35cm_first <- as.numeric(lapply(DW35cm_fit_time["fitted.values"],dplyr::first))
  DW60cm_first <- as.numeric(lapply(DW60cm_fit_time["fitted.values"],dplyr::first))
  DW0to60cm_first <- as.numeric(lapply(DW0to60cm_fit_time["fitted.values"],dplyr::first))
  NO35cm_first <- as.numeric(lapply(NO35cm_fit_time["fitted.values"],dplyr::first))
  NO315cm_first <- as.numeric(lapply(NO315cm_fit_time["fitted.values"],dplyr::first))
  NO335cm_first <- as.numeric(lapply(NO335cm_fit_time["fitted.values"],dplyr::first))
  NO360cm_first <- as.numeric(lapply(NO360cm_fit_time["fitted.values"],dplyr::first))
  NO30to60cm_first <- as.numeric(lapply(NO30to60cm_fit_time["fitted.values"],dplyr::first))
  N2O5cm_first <- as.numeric(lapply(N2O5cm_fit_time["fitted.values"],dplyr::first))
  N2O15cm_first <- as.numeric(lapply(N2O15cm_fit_time["fitted.values"],dplyr::first))
  N2O35cm_first <- as.numeric(lapply(N2O35cm_fit_time["fitted.values"],dplyr::first))
  N2O60cm_first <- as.numeric(lapply(N2O60cm_fit_time["fitted.values"],dplyr::first))
  N2O0to60cm_first <- as.numeric(lapply(N2O0to60cm_fit_time["fitted.values"],dplyr::first))
  N2Oprofile_first <- as.numeric(lapply(N2Oprofile_fit_time["fitted.values"],dplyr::first))
  #
  SW5cm_last <- as.numeric(lapply(SW5cm_fit_time["fitted.values"],dplyr::last))
  SW15cm_last <- as.numeric(lapply(SW15cm_fit_time["fitted.values"],dplyr::last))
  SW35cm_last <- as.numeric(lapply(SW35cm_fit_time["fitted.values"],dplyr::last))
  SW60cm_last <- as.numeric(lapply(SW60cm_fit_time["fitted.values"],dplyr::last))
  DW5cm_last <- as.numeric(lapply(DW5cm_fit_time["fitted.values"],dplyr::last))
  DW15cm_last <- as.numeric(lapply(DW15cm_fit_time["fitted.values"],dplyr::last))
  DW35cm_last <- as.numeric(lapply(DW35cm_fit_time["fitted.values"],dplyr::last))
  DW60cm_last <- as.numeric(lapply(DW60cm_fit_time["fitted.values"],dplyr::last))
  DW0to60cm_last <- as.numeric(lapply(DW0to60cm_fit_time["fitted.values"],dplyr::last))
  NO35cm_last <- as.numeric(lapply(NO35cm_fit_time["fitted.values"],dplyr::last))
  NO315cm_last <- as.numeric(lapply(NO315cm_fit_time["fitted.values"],dplyr::last))
  NO335cm_last <- as.numeric(lapply(NO335cm_fit_time["fitted.values"],dplyr::last))
  NO360cm_last <- as.numeric(lapply(NO360cm_fit_time["fitted.values"],dplyr::last))
  NO30to60cm_last <- as.numeric(lapply(NO30to60cm_fit_time["fitted.values"],dplyr::last))
  N2O5cm_last <- as.numeric(lapply(N2O5cm_fit_time["fitted.values"],dplyr::last))
  N2O15cm_last <- as.numeric(lapply(N2O15cm_fit_time["fitted.values"],dplyr::last))
  N2O35cm_last <- as.numeric(lapply(N2O35cm_fit_time["fitted.values"],dplyr::last))
  N2O60cm_last <- as.numeric(lapply(N2O60cm_fit_time["fitted.values"],dplyr::last))
  N2O0to60cm_last <- as.numeric(lapply(N2O0to60cm_fit_time["fitted.values"],dplyr::last))
  N2Oprofile_last <- as.numeric(lapply(N2Oprofile_fit_time["fitted.values"],dplyr::last))
  #
  SW5cm_change <- SW5cm_last - SW5cm_first 
  SW15cm_change <- SW15cm_last - SW15cm_first 
  SW35cm_change <- SW35cm_last - SW35cm_first 
  SW60cm_change <- SW60cm_last - SW60cm_first 
  DW5cm_change <- DW5cm_last - DW5cm_first 
  DW15cm_change <- DW15cm_last - DW15cm_first 
  DW35cm_change <- DW35cm_last - DW35cm_first 
  DW60cm_change <- DW60cm_last - DW60cm_first 
  DW0to60cm_change <- DW0to60cm_last - DW0to60cm_first
  NO35cm_change <- NO35cm_last - NO35cm_first
  NO315cm_change <- NO315cm_last - NO315cm_first
  NO335cm_change <- NO335cm_last - NO335cm_first 
  NO360cm_change <- NO360cm_last - NO360cm_first 
  NO30to60cm_change <- NO30to60cm_last - NO30to60cm_first
  N2O5cm_change <- N2O5cm_last - N2O5cm_first 
  N2O15cm_change <- N2O15cm_last - N2O15cm_first 
  N2O35cm_change <- N2O35cm_last - N2O35cm_first 
  N2O60cm_change <- N2O60cm_last - N2O60cm_first 
  N2O0to60cm_change <- N2O0to60cm_last - N2O0to60cm_first
  N2Oprofile_change <- N2Oprofile_last - N2Oprofile_first 
  
  
  
  
  ## explain SOC to 25 cm to end of future time period
  
  SMoist_this <- APSIMM_V[APSIMM_V$year %in% (end_exp_period_year+1):end_fut_period_year,]
  SW10cm_fit_time <- lm(VolH2O_10cm/100 ~ date, data = SMoist_this) # convert to fraction
  SW10cm_fit_coef_time <- coef(SW10cm_fit_time)
  DW10cm_fit_time <- lm(DH2O_10cm/100 ~ date, data = SMoist_this) # convert to fraction
  DW10cm_fit_coef_time <- coef(DW10cm_fit_time)
  
  SoilT_this <- APSIMT_C[year(APSIMT_C$date) %in% (end_exp_period_year+1):end_fut_period_year,]
  SoilT5cm_fit_time <- lm(SoilTemp_5cm_C ~ date, data = SoilT_this) 
  SoilT5cm_fit_coef_time <- coef(SoilT5cm_fit_time)
  SoilT15cm_fit_time <- lm(SoilTemp_15cm_C ~ date, data = SoilT_this) 
  SoilT15cm_fit_coef_time <- coef(SoilT15cm_fit_time)
  SoilT35cm_fit_time <- lm(SoilTemp_35cm_C ~ date, data = SoilT_this) 
  SoilT35cm_fit_coef_time <- coef(SoilT35cm_fit_time)
  SoilT60cm_fit_time <- lm(SoilTemp_60cm_C ~ date, data = SoilT_this) 
  SoilT60cm_fit_coef_time <- coef(SoilT60cm_fit_time)
  SoilT10cm_fit_time <- lm(SoilTemp_10cm_C ~ date, data = SoilT_this) 
  SoilT10cm_fit_coef_time <- coef(SoilT10cm_fit_time)
  
  SoilCN_this <- APSIMSoilCN_kgha[APSIMSoilCN_kgha$year %in% (end_exp_period_year+1):end_fut_period_year,]
  BC10cm_fit_time <- lm(BiomC_10cm ~ date, data = SoilCN_this)
  BC10cm_fit_coef_time <- coef(BC10cm_fit_time)
  BN10cm_fit_time <- lm(BiomN_10cm ~ date, data = SoilCN_this)
  BN10cm_fit_coef_time <- coef(BN10cm_fit_time)
  
  HC10cm_fit_time <- lm(HumC_10cm ~ date, data = SoilCN_this)
  HC10cm_fit_coef_time <- coef(HC10cm_fit_time)
  HN10cm_fit_time <- lm(HumN_10cm ~ date, data = SoilCN_this)
  HN10cm_fit_coef_time <- coef(HN10cm_fit_time)
  
  CinB10cm_fit_time <- lm(CtoBiom_10cm ~ date, data = SoilCN_this)
  CinB10cm_fit_coef_time <- coef(CinB10cm_fit_time)
  CinH10cm_fit_time <- lm(CtoHum_10cm ~ date, data = SoilCN_this)
  CinH10cm_fit_coef_time <- coef(CinH10cm_fit_time)
  CinBtoH10cm_fit_time <- lm(CBiomtoHum_10cm ~ date, data = SoilCN_this)
  CinBtoH10cm_fit_coef_time <- coef(CinBtoH10cm_fit_time)
  
  SOC10cm_fit_time <- lm(TotalSOC_10cm ~ year, data = SoilCN_this) 
  SOC10cm_fit_coef_time <- coef(SOC10cm_fit_time)
  
  soilT_transform_factor <- 10
  
  cols <- c("BiomC" = BiomC_10cm_color, "BiomN" = BiomN_10cm_color, "CBtoH" = CBtoHum_10cm_color,
            "CtoB"= CtoBiom_10cm_color, "CtoH" = CtoHum_10cm_color, 
            "HumC" = HumC_10cm_color,  "HumN" = HumN_10cm_color, "SoilT" = SoilT_color, 
            "TotalC" = TotalSOC_10cm_color, "VWC" = SW_10cm_color)
  
  gBioC_expl_fut <- ggplot() +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_15cm/100, color="VWC"), linewidth=1) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=BiomC_10cm/1000, color="BiomC"), linewidth=1) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=BiomN_10cm/1000, color="BiomN"), linewidth=1) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=HumC_10cm/10000, color="HumC"), linewidth=1) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=HumN_10cm/10000, color="HumN"), linewidth=1) +
    geom_smooth(data=SoilCN_this,
                aes(x=date, y=CtoBiom_10cm, color="CtoB"), 
                linewidth=1,
                se=FALSE,
                method=lm)+
    geom_smooth(data=SoilCN_this,
                aes(x=date, y=CtoHum_10cm, color="CtoH"), 
                linewidth=1,
                se=FALSE,
                method=lm) +
    geom_smooth(data=SoilCN_this,
                aes(x=date, y=CBiomtoHum_10cm, color="CBtoH"),
                linewidth=1,
                se=FALSE,
                method=lm) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=TotalSOC_10cm/10, color="TotalC"), linewidth=1) +
    geom_smooth(data=SoilT_this,
                aes(x=date, y=SoilTemp_10cm_C/10, color="SoilT"),
                method=lm,
                se=FALSE) +
    ggtitle(bquote(.(site_name)~"SOC drivers to 25 cm in APSIM"),
            paste0("Scenario: ",scenario_descriptor_full)) +      
    ylab(expression('VWC, Bio C (g ha' ^'-1'*'), Bio N (g ha' ^'-1'*'), Hum C (hg ha' ^'-1'*'), Hum N (hg ha' ^'-1'*')')) +
    scale_y_continuous(
      sec.axis = sec_axis(trans = ~ .x * soilT_transform_factor,
                          name = expression('Soil Temperature ('^o*'C)'))
    ) +
    scale_color_manual(name=NULL,
                       values=cols,
                       labels=c("Biomass C","Biomass N", "C input Biom to Hum",
                                "C input to Biom", "C input to Hum", 
                                "Humic C","Humic N","Soil Temp",
                                "Total SOC","VWC")
    ) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gBioC_expl_fut
  
  ggsave(filename=paste0(results_path,"expl_SOC_0to10cm_exp_",scenario_name,"_APSIM.jpg"),
         plot=gBioC_expl_fut, width=9, height=6, dpi=300)
  
  
  
  ## change in each over future period
  
  SW10cm_first <- as.numeric(lapply(SW10cm_fit_time["fitted.values"],dplyr::first))
  DW10cm_first <- as.numeric(lapply(DW10cm_fit_time["fitted.values"],dplyr::first))
  BC10cm_first <- as.numeric(lapply(BC10cm_fit_time["fitted.values"],dplyr::first))
  BN10cm_first <- as.numeric(lapply(BN10cm_fit_time["fitted.values"],dplyr::first))
  HC10cm_first <- as.numeric(lapply(HC10cm_fit_time["fitted.values"],dplyr::first))
  HN10cm_first <- as.numeric(lapply(HN10cm_fit_time["fitted.values"],dplyr::first))
  CinB10cm_first <- as.numeric(lapply(CinB10cm_fit_time["fitted.values"],dplyr::first))
  CinH10cm_first <- as.numeric(lapply(CinH10cm_fit_time["fitted.values"],dplyr::first))
  CinBtoH10cm_first <- as.numeric(lapply(CinBtoH10cm_fit_time["fitted.values"],dplyr::first))
  SOC10cm_first <- as.numeric(lapply(SOC10cm_fit_time["fitted.values"],dplyr::first))
  SoilT5cm_first <- as.numeric(lapply(SoilT5cm_fit_time["fitted.values"],dplyr::first))
  SoilT15cm_first <- as.numeric(lapply(SoilT15cm_fit_time["fitted.values"],dplyr::first))
  SoilT35cm_first <- as.numeric(lapply(SoilT35cm_fit_time["fitted.values"],dplyr::first))
  SoilT60cm_first <- as.numeric(lapply(SoilT60cm_fit_time["fitted.values"],dplyr::first))
  SoilT10cm_first <- as.numeric(lapply(SoilT10cm_fit_time["fitted.values"],dplyr::first))
  #
  SW10cm_last <- as.numeric(lapply(SW10cm_fit_time["fitted.values"],dplyr::last))
  DW10cm_last <- as.numeric(lapply(DW10cm_fit_time["fitted.values"],dplyr::last))
  BC10cm_last <- as.numeric(lapply(BC10cm_fit_time["fitted.values"],dplyr::last))
  BN10cm_last <- as.numeric(lapply(BN10cm_fit_time["fitted.values"],dplyr::last))
  HC10cm_last <- as.numeric(lapply(HC10cm_fit_time["fitted.values"],dplyr::last))
  HN10cm_last <- as.numeric(lapply(HN10cm_fit_time["fitted.values"],dplyr::last))
  CinB10cm_last <- as.numeric(lapply(CinB10cm_fit_time["fitted.values"],dplyr::last))
  CinH10cm_last <- as.numeric(lapply(CinH10cm_fit_time["fitted.values"],dplyr::last))
  CinBtoH10cm_last <- as.numeric(lapply(CinBtoH10cm_fit_time["fitted.values"],dplyr::last))
  SOC10cm_last <- as.numeric(lapply(SOC10cm_fit_time["fitted.values"],dplyr::last))
  SoilT5cm_last <- as.numeric(lapply(SoilT5cm_fit_time["fitted.values"],dplyr::last))
  SoilT15cm_last <- as.numeric(lapply(SoilT15cm_fit_time["fitted.values"],dplyr::last))
  SoilT35cm_last <- as.numeric(lapply(SoilT35cm_fit_time["fitted.values"],dplyr::last))
  SoilT60cm_last <- as.numeric(lapply(SoilT60cm_fit_time["fitted.values"],dplyr::last))
  SoilT10cm_last <- as.numeric(lapply(SoilT10cm_fit_time["fitted.values"],dplyr::last))
  #
  SW10cm_change <- SW10cm_last - SW10cm_first 
  DW10cm_change <- DW10cm_last - DW10cm_first
  BC10cm_change <- BC10cm_last - BC10cm_first
  BN10cm_change <- BN10cm_last - BN10cm_first
  HC10cm_change <- HC10cm_last - HC10cm_first
  HN10cm_change <- HN10cm_last - HN10cm_first
  CinB10cm_change <- CinB10cm_last - CinB10cm_first
  CinH10cm_change <- CinH10cm_last - CinH10cm_first
  CinBtoH10cm_change <- CinBtoH10cm_last - CinBtoH10cm_first
  SOC10cm_change <- SOC10cm_last - SOC10cm_first
  SoilT5cm_change <- SoilT5cm_last - SoilT5cm_first
  SoilT15cm_change <- SoilT15cm_last - SoilT15cm_first
  SoilT35cm_change <- SoilT35cm_last - SoilT35cm_first
  SoilT60cm_change <- SoilT60cm_last - SoilT60cm_first
  SoilT10cm_change <- SoilT10cm_last - SoilT10cm_first
  
  
  #**********************************************************************
  
  # Log results -------------------------------------------------------------
  
  # add this run's results to  file collecting all final model runs
  fut_log_tab <- cbind(model_name,
                       clim_scenario_num,mgmt_scenario_num, 
                       scenario_name,scenario_abbrev,
                       SW5cm_first,SW5cm_change,
                       SW15cm_first,SW15cm_change,
                       SW35cm_first,SW35cm_change,
                       SW60cm_first,SW60cm_change,
                       DW5cm_first,DW5cm_change,
                       DW15cm_first,DW15cm_change,
                       DW35cm_first,DW35cm_change,
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
                       N2O5cm_first,N2O5cm_change,
                       N2O15cm_first,N2O15cm_change,
                       N2O35cm_first,N2O35cm_change,
                       N2O60cm_first,N2O60cm_change,
                       N2O0to60cm_first,N2O0to60cm_change,
                       N2Oprofile_first,N2Oprofile_change,
                       BC10cm_first,BC10cm_change,
                       BN10cm_first,BN10cm_change,
                       HC10cm_first,HC10cm_change,
                       HN10cm_first,HN10cm_change,
                       CinB10cm_first,CinB10cm_change,
                       CinH10cm_first,CinH10cm_change,
                       CinBtoH10cm_first, CinBtoH10cm_change,
                       SOC10cm_first,SOC10cm_change,
                       # Additional Daycent-only fields
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA,
                       NA, NA
  )
  
  source(paste0("p_Edit_future_file_",site_name,".R"))
  p_Edit_future_file(fut_log_tab,model_name,scenario_name)
  
  #**********************************************************************
  
  rm(SMoist_this,SW20cm_fit_time,SW20cm_fit_coef_time,SW40cm_fit_time,
     SW40cm_fit_coef_time ,SW60cm_fit_time,
     NO3_this,NO3fit_time,NO3fit_coef_time,
     N2O_this,N2Ofit_time,N2Ofit_coef_time,fut_log_tab,
     SW5cm_first,SW5cm_change,
     SW15cm_first,SW15cm_change,
     SW35cm_first,SW35cm_change,
     SW60cm_first,SW60cm_change,
     DW5cm_first,DW5cm_change,
     DW15cm_first,DW15cm_change,
     DW35cm_first,DW35cm_change,
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
     N2O5cm_first,N2O5cm_change,
     N2O15cm_first,N2O15cm_change,
     N2O35cm_first,N2O35cm_change,
     N2O60cm_first,N2O60cm_change,
     N2O0to60cm_first,N2O0to60cm_change,
     N2Oprofile_first,N2Oprofile_change,
     BC10cm_first,BC10cm_change,
     BN10cm_first,BN10cm_change,
     HC10cm_first,HC10cm_change,
     HN10cm_first,HN10cm_change,
     CinB10cm_first,CinB10cm_change,
     CinH10cm_first,CinH10cm_change,
     CinBtoH10cm_first, CinBtoH10cm_change,
     SOC10cm_first,SOC10cm_change
  )
  
  
}) # end suppressMessages