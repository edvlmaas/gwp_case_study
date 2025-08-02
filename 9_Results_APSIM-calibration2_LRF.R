#######################################
# Script: 9_Results_APSIM-calibration2_LRF.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Runs calibration graphs for the APSIM simulation at 
# LRF, TX. Also logs a selection of results for final processing
# in the "10_" series of scripts.
#######################################

suppressMessages({
  
  print(paste0("Starting 9_Results_APSIM-calibration2_",site_name,".R"))
  
library(magrittr)
library(tidyverse)
library(graphics)
library(ggplot2)

  #**********************************************************************
  
# Temporal graphs ---------------------------------------------------------


## experimental period

  ### cotton lint yield
  

  Cotton_this_piv <- CottonYld_Mgha_piv[CottonYld_Mgha_piv$year %in% experiment_year_range &
                                          CottonYld_Mgha_piv$source %in% c("APSIM","Observed"),]
  
  Cotton_this <- CottonYld_Mgha[CottonYld_Mgha$year %in% experiment_year_range,]
  CYfit_time <- lm(APSIM ~ year, data = Cotton_this)
  CYfit_coef_time <- coef(CYfit_time)
  CYfit_r2_time <- round(summary(CYfit_time)$r.squared,2)
  CY_rmse_error_time <- Cotton_this$Observed-Cotton_this$APSIM
  CY_rmse_time <- round(sqrt(mean(CY_rmse_error_time^2,na.rm=TRUE)),2)
  
  gCY <- Cotton_this_piv %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Cotton_this_piv$year, na.rm=T),
           y=max(Cotton_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(CY_rmse_time))) +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('Cotton Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Cotton Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=c(APSIM_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCY

gChY <- CottonYld_Mgha_piv[CottonYld_Mgha_piv$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ggtitle(paste(site_name,"Cotton Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Historical","Observed"),
                     values=c(APSIM_color,4,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gChY

### cotton mid-August biomass yield

Cottonbio_this <- CottonBioYld_Mgha_piv[CottonBioYld_Mgha_piv$year %in% experiment_year_range,]

CBY_rmse_error <- pull(Cottonbio_this[Cottonbio_this$source=="Observed",],yield_val)-
  pull(Cottonbio_this[Cottonbio_this$source=="APSIM",],"yield_val")
CBY_rmse <- round(sqrt(mean(CBY_rmse_error^2,na.rm=TRUE)),2)

gCBY <- Cottonbio_this %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Cottonbio_this$year, na.rm=T),
           y=max(Cottonbio_this$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(CY_rmse_time))) +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('Cotton Biomass (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Cotton Biomass mid-August"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=c(APSIM_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCBY

### sorghum grain yield

if(mgmt_scenario_grp != 7) {

  Sorghum_this_piv <- SorghumYld_Mgha_piv[SorghumYld_Mgha_piv$year %in% experiment_year_range &
                                            SorghumYld_Mgha_piv$source %in% c("APSIM","Observed"),]
  
  Sorghum_this <- SorghumYld_Mgha[SorghumYld_Mgha$year %in% experiment_year_range,]
  SYfit_time <- lm(APSIM ~ year, data = Sorghum_this)
  SYfit_coef_time <- coef(SYfit_time)
  SYfit_r2_time <- round(summary(SYfit_time)$r.squared,2)
  SY_rmse_error_time <- Sorghum_this$Observed-Sorghum_this$APSIM
  SY_rmse_time <- round(sqrt(mean(SY_rmse_error_time^2,na.rm=TRUE)),2)
  
  gSY <- Sorghum_this_piv %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Sorghum_this_piv$year, na.rm=T),
           y=max(Sorghum_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse_time))) +
  geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('Sorghum Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Sorghum Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=c(APSIM_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSY

}

gShY <- SorghumYld_Mgha_piv[SorghumYld_Mgha_piv$year <= end_exp_period_year,] %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Sorghum Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Historical","Observed"),
                     values=c(APSIM_color,4,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 25) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gShY


### sorghum mid-August biomass yield

if(mgmt_scenario_grp != 7) {
  Sorghumbio_this <- SorghumBioYld_Mgha_piv[SorghumBioYld_Mgha_piv$year %in% experiment_year_range,]
  
  SBY_rmse_error <- pull(Sorghumbio_this[Sorghumbio_this$source=="Observed",],yield_val)-
    pull(Sorghumbio_this[Sorghumbio_this$source=="APSIM",],"yield_val")
  SBY_rmse <- round(sqrt(mean(SBY_rmse_error^2,na.rm=TRUE)),2)
  
  gSBY <- Sorghumbio_this %>%
    ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
    geom_point() +
    annotate("text", # RMSE
             x=min(Sorghumbio_this$year, na.rm=T),
             y=max(Sorghumbio_this$yield_val, na.rm=T),
             hjust=0, family="serif", color="gray31",
             label=bquote("RMSE =" ~.(SBY_rmse))) +
    geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                  width=.2) + # Width of the error bars
    xlab("Year") +
    ylab(expression('Sorghum Biomass (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Sorghum Biomass mid-August"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=c(APSIM_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSBY
}

### SOC

SOC_this_piv <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range,]

SOC_this <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,]
Cfit_time <- lm(APSIM ~ year, data = SOC_this)
Cfit_coef_time <- coef(Cfit_time)
Cfit_r2_time <- round(summary(Cfit_time)$r.squared,2)
C_rmse_error_time <- SOC_this$Observed-SOC_this$APSIM
C_rmse_time <- round(sqrt(mean(C_rmse_error_time^2,na.rm=TRUE)),2)

ObsCfit_all <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gC <- SOC_this_piv %>%#experiment_year_range,] %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color="black") +
    geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                  width=.2) + # Width of the error bars
    xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  ylim(3,12) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
    geom_abline(intercept=Cfit_coef_time[1], slope=Cfit_coef_time[2], color="orange") +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=c(APSIM_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 


### SOC with spin-up

Cfith_APSIM <- coef(lm(APSIM ~ year, data = Cstock_Mgha[Cstock_Mgha$year %in% 2003:2021,]))#experiment_year_range,]))
Cfith_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gCh <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% 1987:2021,] %>%#experiment_year_range,] %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_abline(intercept=Cfith_Obs[1], slope=Cfith_Obs[2], color="black") +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  ylim(0,12) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  geom_abline(intercept=Cfith_APSIM[1], slope=Cfith_APSIM[2], color="orange") +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=c(APSIM_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCh 


### soil temp

Temp_this_piv <- SoilTemp_C_piv[SoilTemp_C_piv$year %in% experiment_year_range,]

Temp_this <- SoilTemp_C[year(SoilTemp_C$date) %in% experiment_year_range,] %>%
  mutate(year=year(date))
Tfit_time <- lm(APSIM ~ date, data = Temp_this)
Tfit_coef_time <- coef(Tfit_time)
Tfit_r2_time <- round(summary(Tfit_time)$r.squared,2)

T_rmse_error_time <- Temp_this$Observed-Temp_this$APSIM
T_rmse_time <- round(sqrt(mean(T_rmse_error_time^2,na.rm=TRUE)),2)

Tfit_time_obs <- lm(Observed ~ date, data = Temp_this)
Tfit_coef_time_obs <- coef(Tfit_time_obs)

APSIMT_slope_byday <- Tfit_coef_time[2]
Obs_slope_byday <- Tfit_coef_time_obs[2]

gT <- Temp_this_piv[Temp_this_piv$source=='APSIM' 
                     & Temp_this_piv$year %in% ObsTemp$year,] %>%
  ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=Temp_this_piv[Temp_this_piv$source=='Observed' 
                                 & Temp_this_piv$year %in% ObsTemp$year,],
             aes(x=date, y=temp_val, color=source)) +
  xlab("Year") +
  ylab(expression('Soil temperature ( '*degree*C*")")) +
  ggtitle(paste0(site_name," Soil Temperature"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=c(APSIM_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 20) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT


Tfit_time2 <- lm(APSIM ~ year, data = Temp_this)
Tfit_coef_time2 <- coef(Tfit_time2)
APSIMT_slope_byyear <- Tfit_coef_time2[2]

### soil moisture

Moist_this_piv <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$year %in% experiment_year_range,]

Moist_this <- SoilMoist_VSM[year(SoilMoist_VSM$date) %in% experiment_year_range,] %>%
  mutate(year=year(date))

Mfit_time <- lm(APSIM ~ date, data = Moist_this)
Mfit_coef_time <- coef(Mfit_time)
Mfit_r2_time <- round(summary(Mfit_time)$r.squared,2)

M_rmse_error_time <- Moist_this$Observed-Moist_this$APSIM
M_rmse_time <- round(sqrt(mean(M_rmse_error_time^2,na.rm=TRUE)),2)

gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='APSIM' 
                        & SoilMoist_VSM_piv$year %in% ObsVSM$year,] %>%
  ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
  geom_point() +
  xlab("Year") +
  ylab("Volumetric soil moisture") +
  ggtitle(paste0(site_name," Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM"),
                     values=cbPalette9[c(8)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM

Mfit_time2 <- lm(APSIM ~ year, data = Moist_this)
Mfit_coef_time2 <- coef(Mfit_time2)
APSIMM_slope_byyear <- Mfit_coef_time2[2]

gM_rain <- ggplot() +
  geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='APSIM' 
                        & SoilMoist_VSM_piv$year %in% ObsVSM$year,],
             aes(x=date, y=h2o_val, color=source)) +
  geom_line(data=ObsWth[year(ObsWth$date) %in% year(ObsGas$date) &
                          ObsWth$rain>0,], aes(x=date, y=rain, color="blue"), linetype=2) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ .x * 2,
                        name = expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')'))
  ) +
  xlab("Year") +
  ylab("Volumetric soil moisture") +
  ggtitle(paste0(site_name," Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain"),
                     values=cbPalette9[c(8,4)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM_rain

gNG_rain <- ggplot() +
  geom_line(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                                  year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
            aes(x=date, y=n2o_val, color=source)) +
  geom_line(data=ObsWth[year(ObsWth$date) %in% year(ObsGas$date) &
                          ObsWth$rain>0,], aes(x=date, y=rain, color="blue"), linetype=2) +
  geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
                                   year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source)) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ .x * 2,
                        name = expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')'))
  ) +
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$totalN_kgha>10,],
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
  ylab("Rain (mm)") +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain","Observed"),
                     values=c(APSIM_color,4,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

### N2O

N2O_this_piv <- N2O_ghaday_piv[year(N2O_ghaday_piv$date) %in% experiment_year_range,]

N2O_this <- N2O_ghaday[year(N2O_ghaday$date) %in% experiment_year_range,]
Nfit_time <- lm(APSIM ~ date, data = N2O_this)
Nfit_coef_time <- coef(Nfit_time)
Nfit_r2_time <- round(summary(Nfit_time)$r.squared,2)

gNG <- N2O_this_piv[N2O_this_piv$source=='APSIM' &
                      year(N2O_this_piv$date) %in% year(ObsGas$date),] %>%
  ggplot(aes(x=date, y=n2o_val, color=source)) +
  geom_line(show.legend=TRUE) +
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$totalN_kgha>10,],
               aes(x = date, y = 30,
                   xend = date, yend = 25),
               colour=cbPalette9[7],
               show.legend=F,
               lineend = "round",
               linejoin = "round",
               arrow = arrow(length = unit(0.3, "cm"))
               # colour = "black" 
  ) + 
  xlab("Year") +
  ylab(expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Fertilizer"),
                     values=c(APSIM_color,Fertilizer_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG


gNG_rain <- ggplot() +
  geom_line(data=ObsWth[year(ObsWth$date) %in% year(ObsGas$date) &
                     ObsWth$rain>0,], aes(x=date, y=rain, color="blue"), linetype=2) +
  geom_line(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                        year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
            aes(x=date, y=n2o_val, color=source)) +
  geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
                                   year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source)) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ .x * 2,
                        name = expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')'))
  ) +
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$totalN_kgha>10,],
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
  ylab("Rain (mm)") +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain","Observed"),
                     values=c(APSIM_color,4,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG_rain

gNG_rain5yr <- ggplot() +
  geom_line(data=ObsWth[year(ObsWth$date) %in% 2010:2015 &
                          ObsWth$rain>0,], aes(x=date, y=rain, color="blue"), linetype=2) +
  geom_line(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                                  year(N2O_ghaday_piv$date) %in% 2010:2015,],
            aes(x=date, y=n2o_val, color=source)) +
  geom_segment(data=Fert[Fert$treatment==treatment & year(Fert$date) %in% 2010:2015,],
               aes(x = date, y = 125,
                   xend = date, yend = 100),
               colour=cbPalette9[7],
               show.legend=F,
               lineend = "round",
               linejoin = "round",
               arrow = arrow(length = unit(0.3, "cm"))
               # colour = "black"
  ) +
  xlab("Year") +
  ylab(expression('Rain (mm) and N '[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain","Fertilizer"),
                     values=cbPalette9[c(8,4,7)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG_rain5yr

gNG_5ghd <- N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM' &
                        year(N2O_ghaday_piv$date) %in% year(ObsGas$date),] %>%
  ggplot(aes(x=date, y=n2o_val, color=source)) +
  geom_line(show.legend=TRUE) +
  geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed'&
                                   year(N2O_ghaday_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source)) +
  xlab("Year") +
  ylab(expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
  ylim(0,5) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions-limited display to 5 g/ha/day"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed","Fertilizer"),
                     values=cbPalette9[c(8,1,7)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG_5ghd

gNO3 <- APSIMNO3_ghaday %>%
  ggplot(aes(x=year, y=NO3_15cm/1000)) + 
  geom_line() +
  xlab("Year") +
  ylab(expression('NO3 kg/ha/day')) +  
  ylim(0,125) +
  ggtitle(bquote(.(site_name)~"Soil NO"["3"]*" - 5 to 15 cm"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNO3

ggsave(filename=paste0(results_path,"calib_Cotton_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gCY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Cotton_hist_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gChY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Cotton_biomass_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gCBY,
       width=6, height=6, dpi=300)
if(mgmt_scenario_grp != 7) {
ggsave(filename=paste0(results_path,"calib_Sorghum_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gSY,
       width=6, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Sorghum_hist_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gShY,
         width=8, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"calib_Sorghum_biomass_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gSBY,
         width=6, height=6, dpi=300)
}
ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gC,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_base_",scenario_name,"_APSIM.jpg"),plot=gCh,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gT,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_calib_Soil_Temp_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gT,
       width=6, height=6, dpi=300)
#ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_calib_exp_",scenario_name,"_APSIM.jpg"),plot=gT_calib)
ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gM,
       width=6, height=6, dpi=300)
#ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_calib_exp_",scenario_name,"_APSIM.jpg"),plot=gM_calib)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gNG,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_rain5yr_exp_",scenario_name,"_APSIM.jpg"),plot=gNG_rain5yr,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_5ghd_exp_",scenario_name,"_APSIM.jpg"),plot=gNG_5ghd,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_NO3_input_exp_",scenario_name,"_APSIM.jpg"),plot=gNO3,
       width=9, height=6, dpi=300)


#**********************************************************************

# 1:1 graphs --------------------------------------------------------------


CYfit <- lm(APSIM ~ Observed, data = CottonYld_Mgha)
CYfit_coef <- coef(CYfit)
CYfit_r2 <- round(summary(CYfit)$r.squared,2)

CY_rmse_error <- CottonYld_Mgha$Observed-CottonYld_Mgha$APSIM
CY_rmse <- round(sqrt(mean(CY_rmse_error^2,na.rm=TRUE)),2)

gCY_121 <- CottonYld_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=CYfit_coef[1], slope=CYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(CYfit_coef[2],4))~"x" ~+ ~.(round(CYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(CYfit_r2))) +
  annotate("text", # RMSE
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$APSIM, na.rm=T)*0.88,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(CY_rmse))) +
  ggtitle(bquote(.(site_name)~"Cotton Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,
        #plot.title = element_text(hjust = 0.5))

gCY_121

if(mgmt_scenario_grp != 7 & mgmt_scenario_grp != 5) {
##
SYfit <- lm(APSIM ~ Observed, data = SorghumYld_Mgha)
SYfit_coef <- coef(SYfit)
SYfit_r2 <- round(summary(SYfit)$r.squared,2)

SY_rmse_error <- SorghumYld_Mgha$Observed-SorghumYld_Mgha$APSIM
SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)

gSY_121 <- SorghumYld_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=SYfit_coef[1], slope=SYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(SYfit_coef[2],4))~"x" ~+ ~.(round(SYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(SYfit_r2))) +
  annotate("text", # RMSE
           x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*1.1,
           y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse))) +
  ggtitle(bquote(.(site_name)~"Sorghum Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,
        #plot.title = element_text(hjust = 0.5))

gSY_121
}

##  SOC
if(mgmt_scenario_grp==3) {
  Cfit <- lm(APSIM ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1998,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$APSIM
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
} else {
  Cfit <- lm(APSIM ~ Observed, data = Cstock_Mgha)
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$APSIM
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
}

gC_121 <- Cstock_Mgha %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  ggtitle(bquote(.(site_name)~"SOC Stock (Mg C ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gC_121

##
Tfit <- lm(APSIM ~ Observed, data = SoilTemp_C)
Tfit_coef <- coef(Tfit)
Tfit_r2 <- round(summary(Tfit)$r.squared,2)

T_rmse_error <- SoilTemp_C$Observed-SoilTemp_C$APSIM
T_rmse <- round(sqrt(mean(T_rmse_error^2,na.rm=TRUE)),2)

gT_121 <- SoilTemp_C %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Tfit_coef[1], slope=Tfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Tfit_coef[2],4))~"x" ~+ ~.(round(Tfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*0.92,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Tfit_r2))) +
  annotate("text", # RMSE
           x=min(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C$Observed, SoilTemp_C$APSIM, na.rm=T)*0.82,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(T_rmse))) +
  ggtitle(bquote(.(site_name)~"Soil Temperature ("*degree*"C)"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gT_121

ggsave(filename=paste0(results_path,"calib_Cotton_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gCY_121, width=6, height=6, dpi=300)
if(mgmt_scenario_grp != 7 & mgmt_scenario_grp != 5) {
ggsave(filename=paste0(results_path,"calib_Sorghum_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gSY_121, width=6, height=6, dpi=300)
}
ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gC_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gT_121, width=6, height=6, dpi=300)


#**********************************************************************

# Log results -------------------------------------------------------------

# aggregate cultivars into comma-separated list. includes exp-fut periods.
cultivars <- APSIM_data[!is.na(APSIM_data$crop),] %>%
  group_by(crop) %>%
  summarize(names=paste(unique(cultivar),collapse=';'))

# add this run's results to model log file and file collecting all final
# model runs
if(mgmt_scenario_grp == 7 | mgmt_scenario_grp == 5) { # exclude sorghum
  calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                         clim_scenario_num,mgmt_scenario_num, scenario_name,
                         scenario_abbrev,
                         NA, NA, NA, NA, NA, NA, # Maize 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # Sorghumbean 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # Wheat 1 to 1
                         NA,
                         Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse, NA, NA,
                         SOC_obsmod_diff_Mgha,NA,
                         Tfit_coef[2], Tfit_coef[1], Tfit_r2, T_rmse, NA, NA,
                         SoilT_obsmod_diff_Mgha,
                         NA, NA, NA, NA, NA, NA, # Moisture 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # N2O 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # CH4 1 to 1
                         NA,
                         NA, NA, NA, NA, # M Bio 1 to 1
                         NA,
                         CYfit_coef[2], CYfit_coef[1], CYfit_r2, CY_rmse,
                         Cotton_obsmod_diff_Mgha,
                         NA, NA, NA, NA, # Sorghum 1 to 1
                         NA,
                         NA, NA, NA, # maize, Soybean, wheat cultivars
                         cultivars[cultivars$crop=="Cotton","names"],
                         NA, # sorghum cultivar
                         NA, NA, NA, NA, # maize time series
                         NA, NA, NA, NA, # soybean time series
                         NA, NA, NA, NA, # wheat time series
                         Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, NA, # SOC time series
                         NA, NA, NA, NA, # SOC w/o outliers
                         Tfit_coef_time[2], Tfit_coef_time[1], Tfit_r2_time, NA, # temp time series
                         Mfit_coef_time[2], Mfit_coef_time[1], Mfit_r2_time, NA, # moist time series
                         Nfit_coef_time[2], Nfit_coef_time[1], Nfit_r2_time, NA, # n2o time series
                         NA, NA, NA, NA, # methane time series
                         NA, NA, NA, NA, # microbio time series
                         CYfit_coef_time[2], CYfit_coef_time[1], CYfit_r2_time, NA, # Cotton time series
                         NA, NA, NA, NA # Sorghum time series
  )
} else {
  calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                         clim_scenario_num,mgmt_scenario_num, scenario_name,
                         scenario_abbrev,
                         NA, NA, NA, NA, NA, NA, # Maize 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # Soybean 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # Wheat 1 to 1
                         NA,
                         Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse, NA, NA, # SOC 1 to 1
                         SOC_obsmod_diff_Mgha, NA,
                         Tfit_coef[2], Tfit_coef[1], Tfit_r2, T_rmse, NA, NA, # Temp  1 to 1
                         SoilT_obsmod_diff_Mgha,
                         NA, NA, NA, NA, NA, NA, # Moisture 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # N2O 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # CH4 1 to 1
                         NA,
                         NA, NA, NA, NA, # M Bio 1 to 1
                         NA,
                         CYfit_coef[2], CYfit_coef[1], CYfit_r2, CY_rmse, # Cotton 1 to 1
                         Cotton_obsmod_diff_Mgha,
                         SYfit_coef[2], SYfit_coef[1], SYfit_r2, SY_rmse, # Sorghum 1 to 1
                         Sorghum_obsmod_diff_Mgha,
                         NA, NA, NA, # maize, Soybean, wheat cultivars
                         cultivars[cultivars$crop=="Cotton","names"],
                         cultivars[cultivars$crop=="Sorghum","names"],
                         NA, NA, NA, NA, # maize time series
                         NA, NA, NA, NA, # soybean time series
                         NA, NA, NA, NA, # wheat time series
                         Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, NA, # SOC time series
                         NA, NA, NA, NA, # SOC w/o outliers
                         Tfit_coef_time[2], Tfit_coef_time[1], Tfit_r2_time, NA, # temp time series
                         Mfit_coef_time[2], Mfit_coef_time[1], Mfit_r2_time, NA, # moist time series
                         Nfit_coef_time[2], Nfit_coef_time[1], Nfit_r2_time, NA, # n2o time series
                         NA, NA, NA, NA, # methane time series
                         NA, NA, NA, NA, # microbio time series
                         CYfit_coef_time[2], CYfit_coef_time[1], CYfit_r2_time, NA, # Cotton time series
                         SYfit_coef_time[2], SYfit_coef_time[1], SYfit_r2_time, NA # Sorghum time series
  )
}

source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_log_tab,model_name,scenario_name)

# clean up
rm(cultivars,calib_log_tab,
   Cotton_this_piv,Cotton_this,CYfit_time,CYfit_coef_time,CYfit_r2_time,
   CY_rmse_error_time,CY_rmse_time,gCY,gChY,
   Cottonbio_this,CBY_rmse_error,CBY_rmse,gCBY,
SOC_this_piv,SOC_this,Cfit_time,Cfit_coef_time,Cfit_r2_time,C_rmse_error_time,
C_rmse_time,ObsCfit_all,gC,Cfith_APSIM,Cfith_Obs,gCh,
Temp_this_piv,Temp_this,Tfit_time,Tfit_coef_time,Tfit_r2_time,T_rmse_error_time,
T_rmse_time,gT,
gM,gM_rain,gNG_rain,gNG,gNG_rain5yr,gNG_5ghd,
CYfit,CYfit_coef,CYfit_r2,CY_rmse_error,CY_rmse,gCY_121,
  Cfit, Cfit_coef,Cfit_r2,C_rmse_error,C_rmse,gC_121,
Tfit,Tfit_coef,Tfit_r2,T_rmse_error,T_rmse,gT_121
)

if(mgmt_scenario_grp != 7 & mgmt_scenario_grp != 5) {
  rm(Sorghum_this_piv,Sorghum_this,SYfit_time,SYfit_coef_time,SYfit_r2_time,
     SY_rmse_error_time,SY_rmse_time,gSY,gShY,
     Sorghumbio_this,SBY_rmse_error,SBY_rmse,gSBY,
     SYfit,SYfit_coef, SYfit_r2, SY_rmse_error,SY_rmse,gSY_121
  )
}

}) # end suppressMessages

