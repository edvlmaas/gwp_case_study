#######################################
# Script: 9_Results_APSIM-future_KBS.R
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
  
  ## Maize
  
  MYfit_APSIM <- coef(lm(APSIM ~ year, 
                         data = MaizeYld_Mgha[MaizeYld_Mgha$year>end_exp_period_year,]))
  MYfit_Obs <- coef(lm(Observed ~ year, 
                       data = MaizeYld_Mgha[MaizeYld_Mgha$year %in% experiment_year_range,]))
  MYxs <- c(end_exp_period_year+1, end_fut_period_year)
  MYys <- cbind(1, MYxs) %*% MYfit_APSIM
  MYobsxs <- c(experiment_start_year, experiment_end_year)
  MYobsys <- cbind(1, MYobsxs) %*% MYfit_Obs
  
  gMY <- MaizeYld_Mgha_piv %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Maize Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
#    geom_abline(intercept=MYfit_APSIM[1], slope=MYfit_APSIM[2], color="orange") +
    geom_segment(aes(x = MYxs[1], xend = MYxs[2], y = MYys[1], yend = MYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = MYobsxs[1], xend = MYobsxs[2], y = MYobsys[1], yend = MYobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gMY
  
  ## Soybean
  
  SYfit_APSIM <- coef(lm(APSIM ~ year, 
                         data = SoyYld_Mgha[SoyYld_Mgha$year>end_exp_period_year,]))
  SYfit_Obs <- coef(lm(Observed ~ year, 
                       data = SoyYld_Mgha[SoyYld_Mgha$year %in% experiment_year_range,]))
  SYxs <- c(end_exp_period_year+1, end_fut_period_year)
  SYys <- cbind(1, SYxs) %*% SYfit_APSIM
  SYobsxs <- c(experiment_start_year, experiment_end_year)
  SYobsys <- cbind(1, SYobsxs) %*% SYfit_Obs
  
  gSY <- SoyYld_Mgha_piv %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Soybean Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = SYxs[1], xend = SYxs[2], y = SYys[1], yend = SYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = SYobsxs[1], xend = SYobsxs[2], y = SYobsys[1], yend = SYobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSY
  
  ## Wheat
  
  WYfit_APSIM <- coef(lm(APSIM ~ year, 
                         data = WheatYld_Mgha[WheatYld_Mgha$year>end_exp_period_year,]))
  WYfit_Obs <- coef(lm(Observed ~ year, 
                       data = WheatYld_Mgha[WheatYld_Mgha$year %in% experiment_year_range,]))
  WYxs <- c(end_exp_period_year+1, end_fut_period_year)
  WYys <- cbind(1, WYxs) %*% WYfit_APSIM
  WYobsxs <- c(experiment_start_year, experiment_end_year)
  WYobWYs <- cbind(1, WYobsxs) %*% WYfit_Obs
  
  gWY <- WheatYld_Mgha_piv %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Wheat Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = WYxs[1], xend = WYxs[2], y = WYys[1], yend = WYys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = WYobsxs[1], xend = WYobsxs[2], y = WYobWYs[1], yend = WYobWYs[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gWY
  
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
    # geom_abline(intercept=Cfit_APSIM[1], slope=Cfit_APSIM[2], color="orange") +
    # geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
    ggtitle(paste(site_name,"Soil Organic Carbon"),paste0("Scenario: ",scenario_descriptor_full)) +
    geom_segment(aes(x = Cxs[1], xend = Cxs[2], y = Cys[1], yend = Cys[2]), color=cbPalette9[8]) +
    geom_segment(aes(x = Cobsxs[1], xend = Cobsxs[2], y = Cobsys[1], yend = Cobsys[2]), color=cbPalette9[1]) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gC 
  
 
  gT <- SoilTemp_C_piv %>%
    ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
    geom_point() +
    geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed' & SoilTemp_C_piv$year %in% ObsTemp$year,],
               aes(x=date, y=temp_val, color=source)) +
    xlab("Year") +
    ylab(expression('Soil temperature (' ^o*'C)')) +
    ggtitle(paste0(site_name," Soil Temperature"),paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gT
  
   gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='APSIM',] %>%
    ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
    geom_point() +
    geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed' & SoilMoist_VSM_piv$year %in% ObsVSM$year,],
               aes(x=date, y=h2o_val, color=source)) +
    xlab("Year") +
    ylab("Volumetric soil moisture (%)") +
    ggtitle(paste0(site_name," Soil Moisture"),paste0("Scenario: ",scenario_descriptor_full)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gM
  
  gB <- SoilBD_gcc_piv %>%
    ggplot(aes(x=source,y=bd_val, fill=factor(year))) +
    geom_col(position="dodge") +
    ylab(expression('Bulk density (g cc' ^-1*')')) +
    labs(fill="Year") +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line())
  
  gB
  
  # Daily N2O
  
  gNG <- N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM',] %>%
    ggplot(aes(x=date, y=n2o_val, color=source)) +
    geom_line(show.legend=TRUE) +
    geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed',],
               aes(x=date, y=n2o_val, color=source)) +
    geom_segment(data=Fert[Fert$treatment==treatment & Fert$n_rate_kg_ha>10,],
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
                       values=cbPalette9[c(8,1,7)]) +
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
                       values=cbPalette9[c(8)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gNGann
  
  ggsave(filename=paste0(results_path,"Maize_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gMY, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Soybean_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gSY, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Wheat_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),
         plot=gWY, width=9, height=6, dpi=300)
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
  SW20cm_fit_time <- lm(VolH2O_20cm/100 ~ date, data = SMoist_this) # convert % to fraction
  SW20cm_fit_coef_time <- coef(SW20cm_fit_time)
  SW40cm_fit_time <- lm(VolH2O_40cm/100 ~ date, data = SMoist_this)
  SW40cm_fit_coef_time <- coef(SW40cm_fit_time)
  SW60cm_fit_time <- lm(VolH2O_60cm/100 ~ date, data = SMoist_this)
  SW60cm_fit_coef_time <- coef(SW60cm_fit_time)
  ## depth of water 
  DW20cm_fit_time <- lm(DW_20cm ~ date, data = SMoist_this) 
  DW20cm_fit_coef_time <- coef(DW20cm_fit_time)
  DW40cm_fit_time <- lm(DW_40cm ~ date, data = SMoist_this)
  DW40cm_fit_coef_time <- coef(DW40cm_fit_time)
  DW60cm_fit_time <- lm(DW_60cm ~ date, data = SMoist_this)
  DW60cm_fit_coef_time <- coef(DW60cm_fit_time)
  DW0to60cm_fit_time <- lm((DW_20cm+DW_40cm+DW_60cm) ~ date, data = SMoist_this)
  DW0to60cm_fit_coef_time <- coef(DW0to60cm_fit_time)
  ## NO3
  NO3_this <- APSIMNO3_ghaday[APSIMNO3_ghaday$year %in% (end_exp_period_year+1):end_fut_period_year,] %>%
    mutate(NO3_0to60cm=NO3_20cm+NO3_40cm+NO3_60cm)
  NO320cm_fit_time <- lm(NO3_20cm ~ date, data = NO3_this)
  NO320cm_fit_coef_time <- coef(NO320cm_fit_time)
  NO340cm_fit_time <- lm(NO3_40cm ~ date, data = NO3_this)
  NO340cm_fit_coef_time <- coef(NO340cm_fit_time)
  NO360cm_fit_time <- lm(NO3_60cm ~ date, data = NO3_this)
  NO360cm_fit_coef_time <- coef(NO360cm_fit_time)
  NO30to60cm_fit_time <- lm(NO3_0to60cm ~ date, data = NO3_this)
  NO30to60cm_fit_coef_time <- coef(NO30to60cm_fit_time)
  #
  N2O_this <- APSIM_out[APSIM_out$year %in% (end_exp_period_year+1):end_fut_period_year,
                          c("date","year","N2O_bylayer_kgha(1)",
                            "N2O_bylayer_kgha(2)","N2O_bylayer_kgha(3)",
                            "N2O_profile_kgha")] %>%
    mutate(N2O_20cm_ghaday = round(`N2O_bylayer_kgha(1)`*1000,2),
           N2O_40cm_ghaday = round(`N2O_bylayer_kgha(2)`*1000,2),
           N2O_60cm_ghaday = round(`N2O_bylayer_kgha(3)`*1000,2),
           N2O_0to60cm_ghaday = N2O_20cm_ghaday + N2O_40cm_ghaday + N2O_60cm_ghaday,
           N2O_profile_ghaday = round(N2O_profile_kgha*1000,2))
  N2O20cm_fit_time <- lm(N2O_20cm_ghaday ~ date, data = N2O_this)
  N2O20cm_fit_coef_time <- coef(N2O20cm_fit_time)
  N2O40cm_fit_time <- lm(N2O_40cm_ghaday ~ date, data = N2O_this)
  N2O40cm_fit_coef_time <- coef(N2O40cm_fit_time)
  N2O60cm_fit_time <- lm(N2O_60cm_ghaday ~ date, data = N2O_this)
  N2O60cm_fit_coef_time <- coef(N2O60cm_fit_time)
  N2O0to60cm_fit_time <- lm(N2O_0to60cm_ghaday ~ date, data = N2O_this)
  N2O0to60cm_fit_coef_time <- coef(N2O0to60cm_fit_time)
  N2Oprofile_fit_time <- lm(N2O_profile_ghaday ~ date, data = N2O_this)
  N2Oprofile_fit_coef_time <- coef(N2Oprofile_fit_time)

  
  ## Needs attention for units
  gN2O_expl_fut <- ggplot() +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_20cm/100, color=cbPalette12[2]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_40cm/100, color=cbPalette12[3]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_60cm/100, color=cbPalette12[4]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=dul_20cm, color=cbPalette12[9]), linewidth=1) +
    geom_line(data=SMoist_this,
              aes(x=date, y=sat_20cm, color=cbPalette12[7]), linewidth=1) +
    geom_line(data=NO3_this,
              aes(x=date,y=NO3_20cm/1000000, color=cbPalette12[6]), linewidth=1) +
    geom_line(data=N2O_this,
              aes(x=date, y=N2O_20cm_ghaday/1000, color=cbPalette12[8]), linewidth=1) +
    ggtitle(bquote(.(site_name)~"N"["2"]*"O emissions drivers to 60 cm in APSIM"),
            paste0("Scenario: ",scenario_descriptor_full)) +      
    ylab(expression('VWC, DUL, SAT, NO'[3]*' (ug ha' ^'-1'*' day' ^'-1'*') , N'[2]*'O  (mg ha' ^'-1'*' day'^'-1'*')')) +
    scale_color_manual(name=NULL,
                       labels=c(#"VWC: 0-20 cm","VWC: 20-40 cm","VWC: 40-60 cm",
                                "NO3 (ug/ha/day)","SAT",
                                "N2O (mg/ha/day)","DUL"), #),
                       values=cbPalette9[c(2,3,4,6,7,8,9)])+ #,7,6,8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  
  gN2O_expl_fut
  
  ggsave(filename=paste0(results_path,"expl_N2O_0to60cm_fut_",scenario_name,"_APSIM.jpg"),
         plot=gN2O_expl_fut, width=9, height=6, dpi=300)
  
  
  ## change in each over future period
  
  SW20cm_first <- as.numeric(lapply(SW20cm_fit_time["fitted.values"],dplyr::first))
  SW40cm_first <- as.numeric(lapply(SW40cm_fit_time["fitted.values"],dplyr::first))
  SW60cm_first <- as.numeric(lapply(SW60cm_fit_time["fitted.values"],dplyr::first))
  DW20cm_first <- as.numeric(lapply(DW20cm_fit_time["fitted.values"],dplyr::first))
  DW40cm_first <- as.numeric(lapply(DW40cm_fit_time["fitted.values"],dplyr::first))
  DW60cm_first <- as.numeric(lapply(DW60cm_fit_time["fitted.values"],dplyr::first))
  DW0to60cm_first <- as.numeric(lapply(DW0to60cm_fit_time["fitted.values"],dplyr::first))
  NO320cm_first <- as.numeric(lapply(NO320cm_fit_time["fitted.values"],dplyr::first))
  NO340cm_first <- as.numeric(lapply(NO340cm_fit_time["fitted.values"],dplyr::first))
  NO360cm_first <- as.numeric(lapply(NO360cm_fit_time["fitted.values"],dplyr::first))
  NO30to60cm_first <- as.numeric(lapply(NO30to60cm_fit_time["fitted.values"],dplyr::first))
  N2O20cm_first <- as.numeric(lapply(N2O20cm_fit_time["fitted.values"],dplyr::first))
  N2O40cm_first <- as.numeric(lapply(N2O40cm_fit_time["fitted.values"],dplyr::first))
  N2O60cm_first <- as.numeric(lapply(N2O60cm_fit_time["fitted.values"],dplyr::first))
  N2O0to60cm_first <- as.numeric(lapply(N2O0to60cm_fit_time["fitted.values"],dplyr::first))
  N2Oprofile_first <- as.numeric(lapply(N2Oprofile_fit_time["fitted.values"],dplyr::first))
#
  SW20cm_last <- as.numeric(lapply(SW20cm_fit_time["fitted.values"],dplyr::last))
  SW40cm_last <- as.numeric(lapply(SW40cm_fit_time["fitted.values"],dplyr::last))
  SW60cm_last <- as.numeric(lapply(SW60cm_fit_time["fitted.values"],dplyr::last))
  DW20cm_last <- as.numeric(lapply(DW20cm_fit_time["fitted.values"],dplyr::last))
  DW40cm_last <- as.numeric(lapply(DW40cm_fit_time["fitted.values"],dplyr::last))
  DW60cm_last <- as.numeric(lapply(DW60cm_fit_time["fitted.values"],dplyr::last))
  DW0to60cm_last <- as.numeric(lapply(DW0to60cm_fit_time["fitted.values"],dplyr::last))
  NO320cm_last <- as.numeric(lapply(NO320cm_fit_time["fitted.values"],dplyr::last))
  NO340cm_last <- as.numeric(lapply(NO340cm_fit_time["fitted.values"],dplyr::last))
  NO360cm_last <- as.numeric(lapply(NO360cm_fit_time["fitted.values"],dplyr::last))
  NO30to60cm_last <- as.numeric(lapply(NO30to60cm_fit_time["fitted.values"],dplyr::last))
  N2O20cm_last <- as.numeric(lapply(N2O20cm_fit_time["fitted.values"],dplyr::last))
  N2O40cm_last <- as.numeric(lapply(N2O40cm_fit_time["fitted.values"],dplyr::last))
  N2O60cm_last <- as.numeric(lapply(N2O60cm_fit_time["fitted.values"],dplyr::last))
  N2O0to60cm_last <- as.numeric(lapply(N2O0to60cm_fit_time["fitted.values"],dplyr::last))
  N2Oprofile_last <- as.numeric(lapply(N2Oprofile_fit_time["fitted.values"],dplyr::last))
  #
  SW20cm_change <- SW20cm_last - SW20cm_first 
  SW40cm_change <- SW40cm_last - SW40cm_first 
  SW60cm_change <- SW60cm_last - SW60cm_first 
  DW20cm_change <- DW20cm_last - DW20cm_first 
  DW40cm_change <- DW40cm_last - DW40cm_first 
  DW60cm_change <- DW60cm_last - DW60cm_first 
  DW0to60cm_change <- DW0to60cm_last - DW0to60cm_first
  NO320cm_change <- NO320cm_last - NO320cm_first
  NO340cm_change <- NO340cm_last - NO340cm_first 
  NO360cm_change <- NO360cm_last - NO360cm_first 
  NO30to60cm_change <- NO30to60cm_last - NO30to60cm_first
  N2O20cm_change <- N2O20cm_last - N2O20cm_first 
  N2O40cm_change <- N2O40cm_last - N2O40cm_first 
  N2O60cm_change <- N2O60cm_last - N2O60cm_first 
  N2O0to60cm_change <- N2O0to60cm_last - N2O0to60cm_first
  N2Oprofile_change <- N2Oprofile_last - N2Oprofile_first 
  
  
  
  
  ## explain SOC to 25 cm to end of future time period
  
  SMoist_this <- APSIMM_V[APSIMM_V$year %in% (end_exp_period_year+1):end_fut_period_year,]
  SW25cm_fit_time <- lm(VolH2O_25cm/100 ~ date, data = SMoist_this) # convert to fraction
  SW25cm_fit_coef_time <- coef(SW25cm_fit_time)
  DW25cm_fit_time <- lm(DH2O_25cm/100 ~ date, data = SMoist_this) # convert to fraction
  DW25cm_fit_coef_time <- coef(DW25cm_fit_time)
  
  SoilT_this <- APSIMT_C[year(APSIMT_C$date) %in% (end_exp_period_year+1):end_fut_period_year,]
  SoilT20cm_fit_time <- lm(SoilTemp_20cm_C ~ date, data = SoilT_this) 
  SoilT20cm_fit_coef_time <- coef(SoilT20cm_fit_time)
  SoilT40cm_fit_time <- lm(SoilTemp_40cm_C ~ date, data = SoilT_this)
  SoilT40cm_fit_coef_time <- coef(SoilT40cm_fit_time)
  SoilT60cm_fit_time <- lm(SoilTemp_60cm_C ~ date, data = SoilT_this)
  SoilT60cm_fit_coef_time <- coef(SoilT60cm_fit_time)
  SoilT25cm_fit_time <- lm(SoilTemp_25cm_C ~ date, data = SoilT_this)
  SoilT25cm_fit_coef_time <- coef(SoilT25cm_fit_time)
  
  SoilCN_this <- APSIMSoilCN_kgha[APSIMSoilCN_kgha$year %in% (end_exp_period_year+1):end_fut_period_year,]
  BC25cm_fit_time <- lm(BiomC_25cm ~ date, data = SoilCN_this)
  BC25cm_fit_coef_time <- coef(BC25cm_fit_time)
  BN25cm_fit_time <- lm(BiomN_25cm ~ date, data = SoilCN_this)
  BN25cm_fit_coef_time <- coef(BN25cm_fit_time)
  
  HC25cm_fit_time <- lm(HumC_25cm ~ date, data = SoilCN_this)
  HC25cm_fit_coef_time <- coef(HC25cm_fit_time)
  HN25cm_fit_time <- lm(HumN_25cm ~ date, data = SoilCN_this)
  HN25cm_fit_coef_time <- coef(HN25cm_fit_time)
  
  CinB25cm_fit_time <- lm(CtoBiom_25cm ~ date, data = SoilCN_this)
  CinB25cm_fit_coef_time <- coef(CinB25cm_fit_time)
  CinH25cm_fit_time <- lm(CtoHum_25cm ~ date, data = SoilCN_this)
  CinH25cm_fit_coef_time <- coef(CinH25cm_fit_time)
  CinBtoH25cm_fit_time <- lm(CBiomtoHum_25cm ~ date, data = SoilCN_this)
  CinBtoH25cm_fit_coef_time <- coef(CinBtoH25cm_fit_time)
  
  SOC25cm_fit_time <- lm(TotalSOC_25cm ~ year, data = SoilCN_this) # convert to fraction
  SOC25cm_fit_coef_time <- coef(SOC25cm_fit_time)
  
  soilT_transform_factor <- 10
  
  cols <- c("BiomC" = BiomC_25cm_color, "BiomN" = BiomN_25cm_color, "CBtoH" = CBtoHum_25cm_color,
            "CtoB"= CtoBiom_25cm_color, "CtoH" = CtoHum_25cm_color, 
            "HumC" = HumC_25cm_color,  "HumN" = HumN_25cm_color, "SoilT" = SoilT_color, 
            "TotalC" = TotalSOC_25cm_color, "VWC" = SW_25cm_color)
  
  gBioC_expl_fut <- ggplot() +
    geom_line(data=SMoist_this,
              aes(x=date, y=VolH2O_20cm/100, color="VWC"), linewidth=1) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=BiomC_25cm/1000, color="BiomC"), linewidth=1) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=BiomN_25cm/1000, color="BiomN"), linewidth=1) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=HumC_25cm/10000, color="HumC"), linewidth=1) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=HumN_25cm/10000, color="HumN"), linewidth=1) +
    geom_smooth(data=SoilCN_this,
              aes(x=date, y=CtoBiom_25cm, color="CtoB"), 
              linewidth=1,
              se=FALSE,
              method=lm)+
    geom_smooth(data=SoilCN_this,
              aes(x=date, y=CtoHum_25cm, color="CtoH"), 
              linewidth=1,
              se=FALSE,
              method=lm) +
    geom_smooth(data=SoilCN_this,
                aes(x=date, y=CBiomtoHum_25cm, color="CBtoH"),
                linewidth=1,
                se=FALSE,
                method=lm) +
    geom_line(data=SoilCN_this,
              aes(x=date, y=TotalSOC_25cm/10, color="TotalC"), linewidth=1) +
    geom_smooth(data=SoilT_this,
                aes(x=date, y=SoilTemp_25cm_C/10, color="SoilT"),
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
  
  ggsave(filename=paste0(results_path,"expl_SOC_0to25cm_exp_",scenario_name,"_APSIM.jpg"),
         plot=gBioC_expl_fut, width=9, height=6, dpi=300)
  
  
  
  ## change in each over future period
  
  SW25cm_first <- as.numeric(lapply(SW25cm_fit_time["fitted.values"],dplyr::first))
  DW25cm_first <- as.numeric(lapply(DW25cm_fit_time["fitted.values"],dplyr::first))
  BC25cm_first <- as.numeric(lapply(BC25cm_fit_time["fitted.values"],dplyr::first))
  BN25cm_first <- as.numeric(lapply(BN25cm_fit_time["fitted.values"],dplyr::first))
  HC25cm_first <- as.numeric(lapply(HC25cm_fit_time["fitted.values"],dplyr::first))
  HN25cm_first <- as.numeric(lapply(HN25cm_fit_time["fitted.values"],dplyr::first))
  CinB25cm_first <- as.numeric(lapply(CinB25cm_fit_time["fitted.values"],dplyr::first))
  CinH25cm_first <- as.numeric(lapply(CinH25cm_fit_time["fitted.values"],dplyr::first))
  CinBtoH25cm_first <- as.numeric(lapply(CinBtoH25cm_fit_time["fitted.values"],dplyr::first))
  SOC25cm_first <- as.numeric(lapply(SOC25cm_fit_time["fitted.values"],dplyr::first))
  SoilT20cm_first <- as.numeric(lapply(SoilT20cm_fit_time["fitted.values"],dplyr::first))
  SoilT40cm_first <- as.numeric(lapply(SoilT40cm_fit_time["fitted.values"],dplyr::first))
  SoilT60cm_first <- as.numeric(lapply(SoilT60cm_fit_time["fitted.values"],dplyr::first))
  SoilT25cm_first <- as.numeric(lapply(SoilT25cm_fit_time["fitted.values"],dplyr::first))
  #
  SW25cm_last <- as.numeric(lapply(SW25cm_fit_time["fitted.values"],dplyr::last))
  DW25cm_last <- as.numeric(lapply(DW25cm_fit_time["fitted.values"],dplyr::last))
  BC25cm_last <- as.numeric(lapply(BC25cm_fit_time["fitted.values"],dplyr::last))
  BN25cm_last <- as.numeric(lapply(BN25cm_fit_time["fitted.values"],dplyr::last))
  HC25cm_last <- as.numeric(lapply(HC25cm_fit_time["fitted.values"],dplyr::last))
  HN25cm_last <- as.numeric(lapply(HN25cm_fit_time["fitted.values"],dplyr::last))
  CinB25cm_last <- as.numeric(lapply(CinB25cm_fit_time["fitted.values"],dplyr::last))
  CinH25cm_last <- as.numeric(lapply(CinH25cm_fit_time["fitted.values"],dplyr::last))
  CinBtoH25cm_last <- as.numeric(lapply(CinBtoH25cm_fit_time["fitted.values"],dplyr::last))
  SOC25cm_last <- as.numeric(lapply(SOC25cm_fit_time["fitted.values"],dplyr::last))
  SoilT20cm_last <- as.numeric(lapply(SoilT20cm_fit_time["fitted.values"],dplyr::last))
  SoilT40cm_last <- as.numeric(lapply(SoilT40cm_fit_time["fitted.values"],dplyr::last))
  SoilT60cm_last <- as.numeric(lapply(SoilT60cm_fit_time["fitted.values"],dplyr::last))
  SoilT25cm_last <- as.numeric(lapply(SoilT25cm_fit_time["fitted.values"],dplyr::last))
  #
  SW25cm_change <- SW25cm_last - SW25cm_first
  DW25cm_change <- DW25cm_last - DW25cm_first
  BC25cm_change <- BC25cm_last - BC25cm_first
  BN25cm_change <- BN25cm_last - BN25cm_first
  HC25cm_change <- HC25cm_last - HC25cm_first
  HN25cm_change <- HN25cm_last - HN25cm_first
  CinB25cm_change <- CinB25cm_last - CinB25cm_first
  CinH25cm_change <- CinH25cm_last - CinH25cm_first
  CinBtoH25cm_change <- CinBtoH25cm_last - CinBtoH25cm_first
  SOC25cm_change <- SOC25cm_last - SOC25cm_first
  SoilT20cm_change <- SoilT20cm_last - SoilT20cm_first
  SoilT40cm_change <- SoilT40cm_last - SoilT40cm_first
  SoilT60cm_change <- SoilT60cm_last - SoilT60cm_first
  SoilT25cm_change <- SoilT25cm_last - SoilT25cm_first
  
  
  #**********************************************************************
  
  # Log results -------------------------------------------------------------
  
  # add this run's results to file collecting all final model runs
  fut_log_tab <- cbind(model_name,
                       clim_scenario_num,mgmt_scenario_num, 
                       scenario_name,scenario_abbrev,
                       SW20cm_first,SW20cm_change,
                       SW40cm_first,SW40cm_change,
                       SW60cm_first,SW60cm_change,
                       DW20cm_first,DW20cm_change,
                       DW40cm_first,DW40cm_change,
                       DW60cm_first,DW60cm_change,
                       DW0to60cm_first,DW0to60cm_change,
                       SW25cm_first,SW25cm_change,
                       DW25cm_first,DW25cm_change,
                       SoilT20cm_first,SoilT20cm_change,
                       SoilT40cm_first,SoilT40cm_change,
                       SoilT60cm_first,SoilT60cm_change,
                       SoilT25cm_first,SoilT25cm_change,
                       NO320cm_first,NO320cm_change,
                       NO340cm_first,NO340cm_change,
                       NO360cm_first,NO360cm_change,
                       NO30to60cm_first,NO30to60cm_change,
                       N2O20cm_first,N2O20cm_change,
                       N2O40cm_first,N2O40cm_change,
                       N2O60cm_first,N2O60cm_change,
                       N2O0to60cm_first,N2O0to60cm_change,
                       N2Oprofile_first,N2Oprofile_change,
                       BC25cm_first,BC25cm_change,
                       BN25cm_first,BN25cm_change,
                       HC25cm_first,HC25cm_change,
                       HN25cm_first,HN25cm_change,
                       CinB25cm_first,CinB25cm_change,
                       CinH25cm_first,CinH25cm_change,
                       CinBtoH25cm_first, CinBtoH25cm_change,
                       SOC25cm_first,SOC25cm_change,
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
                       NA, NA
  )
  
  
  source(paste0("p_Edit_future_file_",site_name,".R"))
  p_Edit_future_file(fut_log_tab,model_name,scenario_name)
  
  #**********************************************************************

    rm(SMoist_this,SW20cm_fit_time,SW20cm_fit_coef_time,SW40cm_fit_time,
     SW40cm_fit_coef_time ,SW60cm_fit_time,
     NO3_this,NO3fit_time,NO3fit_coef_time,
     N2O_this,N2Ofit_time,N2Ofit_coef_time,fut_log_tab
  )
  
}) # end suppressMessages