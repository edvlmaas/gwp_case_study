#######################################
# Script: 9_Results_Daycent-calibration_KBS.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Runs calibration graphs for the Daycent simulation at
# KBS, MI. Also logs a selection of results for final processing
# in the "10_" series of scripts.
#######################################

suppressMessages({

print(paste0("Starting 9_Results_Daycent-calibration_",site_name,".R"))

library(magrittr)
library(tidyverse)
library(graphics)
library(ggplot2)
library(hydroGOF)

  #**********************************************************************

# Temporal graphs ---------------------------------------------------------


# suggested to calibrate in this order:
# - soil water content
# - crop yields and plant growth rates
# - soil organic C
# - N loss

    Moist_this_piv <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$year %in% experiment_year_range,]

    Moist_this <- SoilMoist_VSM[year(SoilMoist_VSM$date) %in% experiment_year_range,]
    Mfit_time <- lm(Daycent ~ date, data = Moist_this)
    Mfit_coef_time <- coef(Mfit_time)
    Mfit_r2_time <- round(summary(Mfit_time)$r.squared,2)
    M_rmse_error_time <- Moist_this$Observed-Moist_this$Daycent
    M_rmse_time <- round(sqrt(mean(M_rmse_error_time^2,na.rm=TRUE)),2)
  
  gM <- Moist_this_piv[Moist_this_piv$source=='Daycent' 
                       & Moist_this_piv$year %in% ObsVSM$year,] %>%
    ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
    geom_point() +
    geom_point(data=Moist_this_piv[Moist_this_piv$source=='Observed' 
                                   & Moist_this_piv$year %in% ObsVSM$year,],
               aes(x=date, y=h2o_val, color=source)) +
    xlab("Year") +
    ylab("Volumetric soil moisture") +
    ggtitle(paste0(site_name," Soil Moisture"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("Daycent","Observed"),
                       values=cbPalette9[c(2,1)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gM
  
  Mfit_time2 <- lm(Daycent ~ year, data = Moist_this)
  Mfit_coef_time2 <- coef(Mfit_time2)
  DaycentM_slope_byyear <- Mfit_coef_time2[2]
  
Maize_this_piv <- MaizeYld_Mgha_piv[MaizeYld_Mgha_piv$year %in% experiment_year_range &
                                      MaizeYld_Mgha_piv$source!='Historical',]
Maize_this <- MaizeYld_Mgha[MaizeYld_Mgha$year %in% experiment_year_range &
                              MaizeYld_Mgha_piv$source!='Historical',]

MYfit_time <- lm(Daycent ~ year, data = Maize_this)
MYfit_coef_time <- coef(MYfit_time)
MYfit_r2_time <- round(summary(MYfit_time)$r.squared,2)

MY_rmse_error_time <- Maize_this$Observed-Maize_this$Daycent
MY_rmse_time <- round(sqrt(mean(MY_rmse_error_time^2,na.rm=TRUE)),2)

gMY <- Maize_this_piv %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Maize_this_piv$year, na.rm=T),
           y=max(Maize_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(MY_rmse_time))) +
  xlab("Year") +
  ylab(expression('Maize Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Maize Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=cbPalette9[c(2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMY

gMhY <- MaizeYld_Mgha_piv[MaizeYld_Mgha_piv$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ggtitle(paste(site_name,"Maize Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Historical","Observed"),
                     values=cbPalette9[c(2,4,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMhY

Soy_this_piv <- SoyYld_Mgha_piv[SoyYld_Mgha_piv$year %in% experiment_year_range &
                                  SoyYld_Mgha_piv$source!='Historical',]
Soy_this <- SoyYld_Mgha[SoyYld_Mgha$year %in% experiment_year_range &
                          SoyYld_Mgha_piv$source!='Historical',]

SYfit_time <- lm(Daycent ~ year, data = Soy_this)
SYfit_coef_time <- coef(SYfit_time)
SYfit_r2_time <- round(summary(SYfit_time)$r.squared,2)

SY_rmse_error_time <- Soy_this$Observed-Soy_this$Daycent
SY_rmse_time <- round(sqrt(mean(SY_rmse_error_time^2,na.rm=TRUE)),2)

gSY <- Soy_this_piv %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Soy_this_piv$year, na.rm=T),
           y=max(Soy_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse_time))) +
  xlab("Year") +
  ylab(expression('Soybean Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Soybean Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=cbPalette9[c(2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSY

gShY <- SoyYld_Mgha_piv[SoyYld_Mgha_piv$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soybean Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Historical","Observed"),
                     values=cbPalette9[c(2,4,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gShY

Wheat_this_piv <- WheatYld_Mgha_piv[WheatYld_Mgha_piv$year %in% experiment_year_range &
                                      WheatYld_Mgha_piv$source!='Historical',]
Wheat_this <- WheatYld_Mgha[WheatYld_Mgha$year %in% experiment_year_range &
                              WheatYld_Mgha_piv$source!='Historical',]

WYfit_time <- lm(Daycent ~ year, data = Wheat_this)
WYfit_coef_time <- coef(WYfit_time)
WYfit_r2_time <- round(summary(WYfit_time)$r.squared,2)

WY_rmse_error_time <- Wheat_this$Observed-Wheat_this$Daycent
WY_rmse_time <- round(sqrt(mean(WY_rmse_error_time^2,na.rm=TRUE)),2)

gWY <- Wheat_this_piv%>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Wheat_this_piv$year, na.rm=T),
           y=max(Wheat_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(WY_rmse_time))) +
  xlab("Year") +
  ylab(expression('Wheat Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Wheat Yield"),
          paste("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=cbPalette9[c(2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gWY

gWhY <- WheatYld_Mgha_piv[WheatYld_Mgha_piv$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Wheat Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Historical","Observed"),
                     values=cbPalette9[c(2,4,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gWhY

##
# SOC_this_piv <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range &
#                                   Cstock_Mgha_piv$source!="Obs_sd",]

## calculate stats with outliers
SOC_this <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,]
Cfit_time <- lm(Daycent ~ year, data = SOC_this)
Cfit_coef_time <- coef(Cfit_time)
Cfit_r2_time <- round(summary(Cfit_time)$r.squared,2)
C_rmse_error_time <- SOC_this$Observed-SOC_this$Daycent
C_rmse_time <- round(sqrt(mean(C_rmse_error_time^2,na.rm=TRUE)),2)
## calculate stats without outliers
SOC_this_noout <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range &
                                !(Cstock_Mgha$Observed %in% ObsC_outliers),]
Cfit_time_noout <- lm(Daycent ~ year, data = SOC_this_noout)
Cfit_coef_time_noout <- coef(Cfit_time_noout)
Cfit_r2_time_noout <- round(summary(Cfit_time_noout)$r.squared,2)
C_rmse_error_time_noout <- SOC_this_noout$Observed-SOC_this_noout$Daycent
C_rmse_time_noout <- round(sqrt(mean(C_rmse_error_time_noout^2,na.rm=TRUE)),2)


## include outliers in trend line here
ObsCfit_all <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))


gCb <- Cstock_Mgha[Cstock_Mgha$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=Observed, color=cbPalette9[8]), show.legend=TRUE) +
  geom_point(show.legend=TRUE) +
  geom_errorbar(aes(ymin = Observed - Obs_sd, ymax = Observed + Obs_sd), width = 3) +  # Error bars
  geom_line(data=Cstock_Mgha[Cstock_Mgha$year <= experiment_end_year,],
            aes(x=year, y=Daycent, color=cbPalette9[1]), show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  #geom_abline(intercept=Cfit_Daycent[1], slope=Cfit_Daycent[2], color="orange") +
  geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color="black") +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=cbPalette9[c(2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCb 

Cat1850 <- lis_output[lis_output$time==1850,"somsc_gm2"]
Cat1989 <- unique(lis_output[lis_output$time==1989,"somsc_gm2"])
Cdiff_1850_1989 <- lis_output[lis_output$time==1850,"somsc_gm2"]-lis_output[lis_output$time==1988,"somsc_gm2"]


##


gC <- SOC_this %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  #ylim(10,35) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  #  geom_abline(intercept=Cfit_Daycent[1], slope=Cfit_Daycent[2], color="orange") +
  geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color="black") +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=cbPalette9[c(2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 

gC <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=Observed, color=cbPalette9[8]), show.legend=TRUE) +
  geom_point(show.legend=TRUE) +
  geom_errorbar(aes(ymin = Observed - Obs_sd, ymax = Observed + Obs_sd), width = 3) +  # Error bars
  geom_line(data=Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,],
            aes(x=year, y=Daycent, color=cbPalette9[1]), show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  #geom_abline(intercept=Cfit_Daycent[1], slope=Cfit_Daycent[2], color="orange") +
  geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color="black") +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=cbPalette9[c(2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC

SOC_diff <- lis_output[lis_output$time==1850,"somsc_gm2"]-lis_output[lis_output$time==1988,"somsc_gm2"]
SOC_1850 <- lis_output[lis_output$time==1850,"somsc_gm2"]
SOC_1989 <- unique(lis_output[lis_output$time==1989,"somsc_gm2"])


Temp_this_piv <- SoilTemp_C_piv[SoilTemp_C_piv$year %in% experiment_year_range,]
Temp_this <- SoilTemp_C[year(SoilTemp_C$date) %in% experiment_year_range,] %>%
  mutate(year=year(date))

Tfit_time <- lm(Daycent ~ date, data = Temp_this)
Tfit_coef_time <- coef(Tfit_time)
Tfit_r2_time <- round(summary(Tfit_time)$r.squared,2)

T_rmse_error_time <- Temp_this$Observed-Temp_this$Daycent
T_rmse_time <- round(sqrt(mean(T_rmse_error_time^2,na.rm=TRUE)),2)

DaycentT_slope_byday <- Tfit_coef_time[2]
Obs_slope_byday <- Tfit_coef_time[2]


gT <- Temp_this_piv[Temp_this_piv$source=='Daycent'
                    & Temp_this_piv$year %in% ObsTemp$year,] %>%
  ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=Temp_this_piv[Temp_this_piv$source=='Observed'
                                & Temp_this_piv$date %in% ObsTemp$date,],
             aes(x=date, y=temp_val, color=source)) +
  xlab("Year") +
  ylab(expression('Soil temperature ( '*degree*C*")")) +
  ggtitle(paste0(site_name," Soil Temperature"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=cbPalette9[c(2,1)]) +
  theme_classic(base_family = "serif", base_size = 20) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT

Tfit_time2 <- lm(Daycent ~ year, data = Temp_this)
Tfit_coef_time2 <- coef(Tfit_time2)
DaycentT_slope_byyear <- Tfit_coef_time2[2]

mean_soilT_obs <- mean(SoilTemp_C[,"Observed"],na.rm=TRUE)
mean_soilT_day <- mean(SoilTemp_C[,"Daycent"],na.rm=TRUE)


N2O_this_piv <- N2O_ghaday_piv[year(N2O_ghaday_piv$date) %in% experiment_year_range,]

N2O_this <- N2O_ghaday[year(N2O_ghaday$date) %in% experiment_year_range,]
Nfit_time <- lm(Daycent ~ date, data = N2O_this)
Nfit_coef_time <- coef(Nfit_time)
Nfit_r2_time <- round(summary(Nfit_time)$r.squared,2)
N_rmse_error_time <- N2O_this$Observed-N2O_this$Daycent
N_rmse_time <- round(sqrt(mean(N_rmse_error_time^2,na.rm=TRUE)),2)

gNG <- N2O_this_piv[N2O_this_piv$source=='Daycent' &
                      year(N2O_this_piv$date) %in% year(ObsGas$date),] %>%
  ggplot(aes(x=date, y=n2o_val, color=source)) +
  geom_line(show.legend=TRUE) +
  geom_point(data=N2O_this_piv[N2O_this_piv$source=='Observed'&
                                 year(N2O_this_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source), size=1) +
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$n_rate_kg_ha>10,],
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
  ylab(expression('N'[2]*'O (g ha' ^'-1'*' day'^'-1'*')')) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed","Fertilizer"),
                     values=cbPalette9[c(2,1,7)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG


gET <- ET_mmh2oyr_piv %>%
  ggplot(aes(x=year, y=et_val, color=source)) +
  geom_point() +
  xlab("Year") +
#  ylab(expression('N'[2]*'O Emissions (g ha ' ^-1*'yr ' ^-1*')')) +
  ylab(expression('Evapotranspiration (mm H  '['2']*'O yr ' ^-1*')')) +
  ggtitle(bquote(.(site_name)~"Annual Cumulative Evapotranspiration"),
          paste0("Scenario: ",scenario_descriptor_full)) +
#  geom_segment(aes(x = NGxs[1], xend = NGxs[2], y = NGys[1], yend = NGys[2]), color=cbPalette9[8]) +
  scale_color_manual(labels=c("Daycent"),
                     values=cbPalette9[c(8)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gET


## check plant tissue N to verify plant uptake
gGC <- grainC_gm2_piv[grainC_gm2_piv$source=='Daycent'
                      & grainC_gm2_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=grainC_val, color=crop, shape=source)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=grainC_gm2_piv[grainC_gm2_piv$source=='Observed'
                                 & grainC_gm2_piv$year %in% experiment_year_range,],
             aes(x=year, y=grainC_val, color=crop, shape=source)) +
  xlab("Year") +
  ylab(expression('Grain C (g N m' ^-2*')')) +
  scale_color_manual(labels=c("Corn","Soybean","Wheat"),
                     values=cbPalette9[c(2,3,4)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gGC

gSC <- stoverC_gm2_piv[stoverC_gm2_piv$source=='Daycent'
                      & stoverC_gm2_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=grainC_val, color=crop, shape=source)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=stoverC_gm2_piv[stoverC_gm2_piv$source=='Observed'
                                 & stoverC_gm2_piv$year %in% experiment_year_range,],
             aes(x=year, y=grainC_val, color=crop, shape=source)) +
  xlab("Year") +
  ylab(expression('Stover C (g C m' ^-2*')')) +
  scale_color_manual(labels=c("Corn","Soybean","Wheat"),
                     values=cbPalette9[c(2,3,4)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSC

gGN <- grainN_gm2_piv[grainN_gm2_piv$source=='Daycent'
                      & grainN_gm2_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=grainN_val, color=crop, shape=source)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=grainN_gm2_piv[grainN_gm2_piv$source=='Observed'
                                 & grainN_gm2_piv$year %in% experiment_year_range,],
             aes(x=year, y=grainN_val, color=crop, shape=source)) +
  xlab("Year") +
  ylab(expression('Grain N (g N m' ^-2*')')) +
  scale_color_manual(labels=c("Corn","Soybean","Wheat"),
                     values=cbPalette9[c(2,3,4)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gGN

gSN <- stoverN_gm2_piv[stoverN_gm2_piv$source=='Daycent'
                       & stoverN_gm2_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=grainN_val, color=crop, shape=source)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=stoverN_gm2_piv[stoverN_gm2_piv$source=='Observed'
                                  & stoverN_gm2_piv$year %in% experiment_year_range,],
             aes(x=year, y=grainN_val, color=crop, shape=source)) +
  xlab("Year") +
  ylab(expression('Stover N (g N m' ^-2*')')) +
  scale_color_manual(labels=c("Corn","Soybean","Wheat"),
                     values=cbPalette9[c(2,3,4)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSN

gGCN <- grainCN_piv[grainCN_piv$source=='Daycent'
                       & grainCN_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=grainCN_val, color=crop, shape=source)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=grainCN_piv[grainCN_piv$source=='Observed'
                                  & grainCN_piv$year %in% experiment_year_range,],
             aes(x=year, y=grainCN_val, color=crop, shape=source)) +
  xlab("Year") +
  ylab(expression('Grain C:N ratio')) +
  scale_color_manual(labels=c("Corn","Soybean","Wheat"),
                     values=cbPalette9[c(2,3,4)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gGCN

gSCN <- stoverCN_piv[stoverCN_piv$source=='Daycent'
                   & stoverCN_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=stoverCN_val, color=crop, shape=source)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=stoverCN_piv[stoverCN_piv$source=='Observed'
                              & stoverCN_piv$year %in% experiment_year_range,],
             aes(x=year, y=stoverCN_val, color=crop, shape=source)) +
  xlab("Year") +
  ylab(expression('Stover C:N ratio')) +
  scale_color_manual(labels=c("Corn","Soybean","Wheat"),
                     values=cbPalette9[c(2,3,4)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSCN


CH4_this_piv <- CH4_ghaday_piv[year(CH4_ghaday_piv$date) %in% experiment_year_range,]

CH4_this <- CH4_ghaday[year(CH4_ghaday$date) %in% experiment_year_range,]
Hfit_time <- lm(Daycent ~ date, data = CH4_this)
Hfit_coef_time <- coef(Hfit_time)
Hfit_r2_time <- round(summary(Hfit_time)$r.squared,2)
H_rmse_error_time <- CH4_this$Observed-CH4_this$Daycent
H_rmse_time <- round(sqrt(mean(H_rmse_error_time^2,na.rm=TRUE)),2)

gMG <- CH4_this_piv[CH4_this_piv$source=='Daycent'
                      &year(CH4_this_piv$date) %in% 1992:2014,] %>%
  ggplot(aes(x=date, y=ch4_val, color=source)) +
  geom_line(show.legend=TRUE) +
  geom_point(data=CH4_this_piv[CH4_this_piv$source=='Observed'
                                 &year(CH4_this_piv$date) %in% 1992:2014,],
             aes(x=date, y=ch4_val, color=source), size=1) +
  xlab("Year") +
  ylab(expression('CH'[4]*' Emissions (g C ha ' ^-1*'day ' ^-1*')')) +
  ggtitle(bquote(.(site_name)~"CH"["4"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=cbPalette9[c(2,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMG

gCI <- DayCI_gm2yr[DayCI_gm2yr$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=base), show.legend=TRUE) +
  geom_line(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('C input (g C m' ^-2*' yr' ^-1*')')) +
  ggtitle(paste(site_name,"Soil C Input"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCI

gNI <- DayNI_gm2yr[DayNI_gm2yr$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=base), show.legend=TRUE) +
  geom_line(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('N input (g C m' ^-2*' yr' ^-1*')')) +
  ylim(0,12) +
  ggtitle(paste(site_name,"Soil N Input"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNI

gNH4 <- Day_soiln[Day_soiln$year %in% experiment_year_range,] %>%
  ggplot(aes(x=date, y=ammonium)) +
  geom_line() +
  xlab("Year") +
  ylab(expression('NH4 ppm per day')) +  
  ylim(0,125) +
  ggtitle(bquote(.(site_name)~"Soil NH"["4"]*" - top 10 cm"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNH4

gNO3 <- Day_soiln_all[Day_soiln_all$year %in% experiment_year_range,] %>%
  ggplot(aes(x=date, y=NO3_kgha)) +
  geom_line() +
  xlab("Year") +
  ylab(expression('NO3 kg/ha/day')) +  
  ylim(0,125) +
  ggtitle(bquote(.(site_name)~"Soil NO"["3"]*" - top 20 cm"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNO3

gNIb <- DayNI_gm2yr[DayNI_gm2yr$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=base), show.legend=TRUE) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('N input (g N m' ^-2*' yr' ^-1*')')) +
  ylim(0,12) +
  ggtitle(paste(site_name,"Soil N Input"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNIb

gNH4b <- Day_soiln[Day_soiln$year <= experiment_end_year,] %>%
  ggplot(aes(x=date, y=ammonium)) +
  geom_line() +
  xlab("Year") +
  ylab(expression('NH4 ppm per day')) +  
  ylim(0,125) +
  ggtitle(bquote(.(site_name)~"Soil NH"["4"]*" - top 10 cm"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNH4b

gNO3b <- Day_soiln_all[Day_soiln_all$year <= experiment_end_year,] %>%
  ggplot(aes(x=date, y=NO3_kgha)) +
  geom_line() +
  xlab("Year") +
  ylab(expression('NO3 kg/ha/day')) +  
  ylim(0,125) +
  ggtitle(bquote(.(site_name)~"Soil NO"["3"]*" - top 20 cm"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNO3b

ggsave(filename=paste0(results_path,"calib_Maize_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gMY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Maize_hist_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gMhY,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soybean_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gSY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soybean_hist_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gShY,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Wheat_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gWY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Wheat_hist_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gWhY,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gC,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_base_",scenario_name,"_Daycent.jpg"),plot=gCb,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gT,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_calib_Soil_Temp_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gT,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gM,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gNG,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_CH4_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gMG,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_C_input_exp_",scenario_name,"_Daycent.jpg"),plot=gCI,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N_input_exp_",scenario_name,"_Daycent.jpg"),plot=gNI,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_NH4_input_exp_",scenario_name,"_Daycent.jpg"),plot=gNH4,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_NO3_input_exp_",scenario_name,"_Daycent.jpg"),plot=gNO3,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N_input_base_",scenario_name,"_Daycent.jpg"),plot=gNIb,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_NH4_input_base_",scenario_name,"_Daycent.jpg"),plot=gNH4b,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_NO3_input_base_",scenario_name,"_Daycent.jpg"),plot=gNO3b,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_ET_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gET,
       width=6, height=6, dpi=300)



#**********************************************************************

# 1:1 graphs --------------------------------------------------------------

## soil moisture
SoilMoist_VSM_subset <- SoilMoist_VSM[!is.na(SoilMoist_VSM$Observed)&!is.na(SoilMoist_VSM$Daycent),]

Mfit <- lm(Daycent ~ Observed, data = SoilMoist_VSM_subset)
Mfit_coef <- coef(Mfit)
Mfit_r2 <- round(summary(Mfit)$r.squared,2)
# M_rmse_error <- SoilMoist_VSM$Observed-SoilMoist_VSM$Daycent
# M_rmse <- round(sqrt(mean(M_rmse_error^2,na.rm=TRUE)),2)
M_rmse <- round(hydroGOF::rmse(SoilMoist_VSM_subset$Daycent,SoilMoist_VSM_subset$Observed),2) 
M_mNSE <- round(hydroGOF::mNSE(SoilMoist_VSM_subset$Daycent,SoilMoist_VSM_subset$Observed),2)
M_pbias <- round(hydroGOF::pbias(SoilMoist_VSM_subset$Daycent,SoilMoist_VSM_subset$Observed),2)

gM_121 <- SoilMoist_VSM_subset %>%
  ggplot(aes(x=Observed, y=Daycent, 
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T), 
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() + # 1:1 line
  geom_abline(intercept=Mfit_coef[1], slope=Mfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$Daycent, na.rm=T)*1.1,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Mfit_coef[2],4))~"x" ~+ ~.(round(Mfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$Daycent, na.rm=T)*1.1,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Mfit_r2))) +
  annotate("text", # RMSE
           x=min(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$Daycent, na.rm=T)*1.1,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$Daycent, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(M_rmse))) +
  annotate("text", # mNSE
           x=(max(SoilMoist_VSM_subset$Observed, na.rm=T)+min(SoilMoist_VSM_subset$Observed, na.rm=T))/2,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(M_mNSE))) +
  annotate("text", # pbias
           x=(max(SoilMoist_VSM_subset$Observed, na.rm=T)+min(SoilMoist_VSM_subset$Observed, na.rm=T))/2,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(M_pbias))) +
  ggtitle(paste0(site_name," Volumetric Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gM_121


# corn
MaizeYld_Mgha_subset <- MaizeYld_Mgha[!is.na(MaizeYld_Mgha$Observed),]

MYfit <- lm(Daycent ~ Observed, data = MaizeYld_Mgha_subset)
MYfit_coef <- coef(MYfit)
MYfit_r2 <- round(summary(MYfit)$r.squared,2)
# MY_rmse_error <- MaizeYld_Mgha_subset$Observed-MaizeYld_Mgha_subset$Daycent
# MY_rmse <- round(sqrt(mean(MY_rmse_error^2,na.rm=TRUE)),2)
MY_rmse <- round(hydroGOF::rmse(MaizeYld_Mgha_subset$Daycent,MaizeYld_Mgha_subset$Observed),2) 
MY_mNSE <- round(hydroGOF::mNSE(MaizeYld_Mgha_subset$Daycent,MaizeYld_Mgha_subset$Observed),2)
MY_pbias <- round(hydroGOF::pbias(MaizeYld_Mgha_subset$Daycent,MaizeYld_Mgha_subset$Observed),2)

gMY_121 <- MaizeYld_Mgha_subset %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() + # 1:1 line
  geom_abline(intercept=MYfit_coef[1], slope=MYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(MYfit_coef[2],4))~"x" ~+ ~.(round(MYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(MYfit_r2))) +
  annotate("text", # RMSE
           x=min(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$Daycent, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(MY_rmse))) +
  annotate("text", # mNSE
           x=(max(MaizeYld_Mgha_subset$Observed, na.rm=T)+min(MaizeYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(MY_mNSE))) +
  annotate("text", # pbias
           x=(max(MaizeYld_Mgha_subset$Observed, na.rm=T)+min(MaizeYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(MY_pbias))) +
  ggtitle(bquote(.(site_name)~"Maize Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gMY_121

## soy
SoyYld_Mgha_subset <- SoyYld_Mgha[!is.na(SoyYld_Mgha$Observed),]

SYfit <- lm(Daycent ~ Observed, data = SoyYld_Mgha_subset)
SYfit_coef <- coef(SYfit)
SYfit_r2 <- round(summary(SYfit)$r.squared,2)
# SY_rmse_error <- SoyYld_Mgha_subset$Observed-SoyYld_Mgha_subset$Daycent
# SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)
SY_rmse <- round(hydroGOF::rmse(SoyYld_Mgha_subset$Daycent,SoyYld_Mgha_subset$Observed),2) 
SY_mNSE <- round(hydroGOF::mNSE(SoyYld_Mgha_subset$Daycent,SoyYld_Mgha_subset$Observed),2)
SY_pbias <- round(hydroGOF::pbias(SoyYld_Mgha_subset$Daycent,SoyYld_Mgha_subset$Observed),2)

gSY_121 <- SoyYld_Mgha_subset %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() + # 1:1 line
  geom_abline(intercept=SYfit_coef[1], slope=SYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(SYfit_coef[2],4))~"x" ~+ ~.(round(SYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(SYfit_r2))) +
  annotate("text", # RMSE
           x=min(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$Daycent, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse))) +
  annotate("text", # mNSE
           x=(max(SoyYld_Mgha_subset$Observed, na.rm=T)+min(SoyYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(SY_mNSE))) +
  annotate("text", # pbias
           x=(max(SoyYld_Mgha_subset$Observed, na.rm=T)+min(SoyYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(SY_pbias))) +
  ggtitle(bquote(.(site_name)~"Soybean Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gSY_121


## wheat
WheatYld_Mgha_subset <- WheatYld_Mgha[!is.na(WheatYld_Mgha$Observed),]

WYfit <- lm(Daycent ~ Observed, data = WheatYld_Mgha_subset)
WYfit_coef <- coef(WYfit)
WYfit_r2 <- round(summary(WYfit)$r.squared,2)
# WY_rmse_error <- WheatYld_Mgha$Observed-WheatYld_Mgha$Daycent
# WY_rmse <- round(sqrt(mean(WY_rmse_error^2,na.rm=TRUE)),2)
WY_rmse <- round(hydroGOF::rmse(WheatYld_Mgha_subset$Daycent,WheatYld_Mgha_subset$Observed),2) 
WY_mNSE <- round(hydroGOF::mNSE(WheatYld_Mgha_subset$Daycent,WheatYld_Mgha_subset$Observed),2)
WY_pbias <- round(hydroGOF::pbias(WheatYld_Mgha_subset$Daycent,WheatYld_Mgha_subset$Observed),2)

gWY_121 <- WheatYld_Mgha_subset %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() + # 1:1 line
  geom_abline(intercept=WYfit_coef[1], slope=WYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(WYfit_coef[2],4))~"x" ~+ ~.(round(WYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(WYfit_r2))) +
  annotate("text", # RMSE
           x=min(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$Daycent, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(WY_rmse))) +
  annotate("text", # mNSE
           x=(max(WheatYld_Mgha_subset$Observed, na.rm=T)+min(WheatYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(WY_mNSE))) +
  annotate("text", # pbias
           x=(max(WheatYld_Mgha_subset$Observed, na.rm=T)+min(WheatYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(WY_pbias))) +
  ggtitle(bquote(.(site_name)~"Wheat Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gWY_121


## soil organic C
Cstock_Mgha_subset <- Cstock_Mgha[Cstock_Mgha$year>=experiment_start_date &
                                                    !is.na(Cstock_Mgha$Observed),]

Cfit <- lm(Daycent ~ Observed, data = Cstock_Mgha_subset)
Cfit_coef <- coef(Cfit)
Cfit_r2 <- round(summary(Cfit)$r.squared,2)
# C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$Daycent
# C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
C_rmse <- round(hydroGOF::rmse(Cstock_Mgha_subset$Daycent,Cstock_Mgha_subset$Observed),2) 
C_mNSE <- round(hydroGOF::mNSE(Cstock_Mgha_subset$Daycent,Cstock_Mgha_subset$Observed),2)
C_pbias <- round(hydroGOF::pbias(Cstock_Mgha_subset$Daycent,Cstock_Mgha_subset$Observed),2)

gC_121 <- Cstock_Mgha_subset %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() + # 1:1 line
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$Daycent, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$Daycent, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  annotate("text", # mNSE
           x=(max(Cstock_Mgha_subset$Observed, na.rm=T)+min(Cstock_Mgha_subset$Observed, na.rm=T))/2,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(C_mNSE))) +
  annotate("text", # pbias
           x=(max(Cstock_Mgha_subset$Observed, na.rm=T)+min(Cstock_Mgha_subset$Observed, na.rm=T))/2,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(C_pbias))) +
  ggtitle(bquote(.(site_name)~"SOC Stock (Mg C ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gC_121

## soil temperature
SoilTemp_C_subset <- SoilTemp_C[!is.na(SoilTemp_C$Observed)&!is.na(SoilTemp_C$Daycent),]

Tfit <- lm(Daycent ~ Observed, data = SoilTemp_C_subset)
Tfit_coef <- coef(Tfit)
Tfit_r2 <- round(summary(Tfit)$r.squared,2)
# T_rmse_error <- SoilTemp_C_subset$Observed-SoilTemp_C_subset$Daycent
# T_rmse <- round(sqrt(mean(T_rmse_error^2,na.rm=TRUE)),2)
T_rmse <- round(hydroGOF::rmse(SoilTemp_C_subset$Daycent,SoilTemp_C_subset$Observed),2) 
T_mNSE <- round(hydroGOF::mNSE(SoilTemp_C_subset$Daycent,SoilTemp_C_subset$Observed),2)
T_pbias <- round(hydroGOF::pbias(SoilTemp_C_subset$Daycent,SoilTemp_C_subset$Observed),2)

gT_121 <- SoilTemp_C_subset %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() + # 1:1 line
  geom_abline(intercept=Tfit_coef[1], slope=Tfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoilTemp_C_subset$Observed, SoilTemp_C_subset$Daycent, na.rm=T)*1.1,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Tfit_coef[2],4))~"x" ~+ ~.(round(Tfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoilTemp_C_subset$Observed, SoilTemp_C_subset$Daycent, na.rm=T)*1.1,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$Daycent, na.rm=T)*0.92,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Tfit_r2))) +
  annotate("text", # RMSE
           x=min(SoilTemp_C_subset$Observed, SoilTemp_C_subset$Daycent, na.rm=T)*1.1,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$Daycent, na.rm=T)*0.82,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(T_rmse))) +
  annotate("text", # mNSE
           x=(max(SoilTemp_C_subset$Observed, na.rm=T)+min(SoilTemp_C_subset$Observed, na.rm=T))/2,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(T_mNSE))) +
  annotate("text", # pbias
           x=(max(SoilTemp_C_subset$Observed, na.rm=T)+min(SoilTemp_C_subset$Observed, na.rm=T))/2,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(T_pbias))) +
  ggtitle(bquote(.(site_name)~"Soil Temperature ("*degree*"C)"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gT_121


## N2O
N2O_ghaday_subset <- N2O_ghaday[!is.na(N2O_ghaday$Observed),]

Nfit <- lm(Daycent ~ Observed, data = N2O_ghaday_subset)
Nfit_coef <- coef(Nfit)
Nfit_r2 <- round(summary(Nfit)$r.squared,2)
# N_rmse_error <- N2O_ghaday_subset$Observed-N2O_ghaday_subset$Daycent
# N_rmse <- round(sqrt(mean(N_rmse_error^2,na.rm=TRUE)),2)
N_rmse <- round(hydroGOF::rmse(N2O_ghaday_subset$Daycent,N2O_ghaday_subset$Observed),2) 
N_mNSE <- round(hydroGOF::mNSE(N2O_ghaday_subset$Daycent,N2O_ghaday_subset$Observed),2)
N_pbias <- round(hydroGOF::pbias(N2O_ghaday_subset$Daycent,N2O_ghaday_subset$Observed),2)

gNG_121 <- N2O_ghaday_subset %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() + # 1:1 line
  geom_abline(intercept=Nfit_coef[1], slope=Nfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(N2O_ghaday_subset$Observed, N2O_ghaday_subset$Daycent, na.rm=T)*1.1,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Nfit_coef[2],4))~"x" ~+ ~.(round(Nfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(N2O_ghaday_subset$Observed, N2O_ghaday_subset$Daycent, na.rm=T)*1.1,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$Daycent, na.rm=T)*0.92,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Nfit_r2))) +
  annotate("text", # RMSE
           x=min(N2O_ghaday_subset$Observed, N2O_ghaday_subset$Daycent, na.rm=T)*1.1,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$Daycent, na.rm=T)*0.82,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(N_rmse))) +
  annotate("text", # mNSE
           x=(max(N2O_ghaday_subset$Observed, na.rm=T)+min(N2O_ghaday_subset$Observed, na.rm=T))/2,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(N_mNSE))) +
  annotate("text", # pbias
           x=(max(N2O_ghaday_subset$Observed, na.rm=T)+min(N2O_ghaday_subset$Observed, na.rm=T))/2,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$Daycent, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(N_pbias))) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions (g ha" ^"-1"*" day"^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gNG_121

##
CH4_ghaday_subset <- CH4_ghaday[!is.na(CH4_ghaday$Observed),]

Hfit <- lm(Daycent ~ Observed, data = CH4_ghaday_subset)
Hfit_coef <- coef(Hfit)
Hfit_r2 <- round(summary(Hfit)$r.squared,2)
# H_rmse_error <- CH4_ghaday_subset$Observed-CH4_ghaday_subset$Daycent
# H_rmse <- round(sqrt(mean(H_rmse_error^2,na.rm=TRUE)),2)
H_rmse <- round(hydroGOF::rmse(CH4_ghaday_subset$Daycent,CH4_ghaday_subset$Observed),2) 
H_mNSE <- round(hydroGOF::mNSE(CH4_ghaday_subset$Daycent,CH4_ghaday_subset$Observed),2)
H_pbias <- round(hydroGOF::pbias(CH4_ghaday_subset$Daycent,CH4_ghaday_subset$Observed),2)

gMG_121 <- CH4_ghaday_subset[CH4_ghaday_subset$Observed!=0,] %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() + # 1:1 line
  geom_abline(intercept=Hfit_coef[1], slope=Hfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(CH4_ghaday_subset$Observed, CH4_ghaday_subset$Daycent, na.rm=T)*1.1,
           y=max(CH4_ghaday_subset$Observed, CH4_ghaday_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Hfit_coef[2],4))~"x" ~+ ~.(round(Hfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(CH4_ghaday_subset$Observed, CH4_ghaday_subset$Daycent, na.rm=T)*1.1,
           y=max(CH4_ghaday_subset$Observed, CH4_ghaday_subset$Daycent, na.rm=T)*0.85,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Hfit_r2))) +
  annotate("text", # RMSE
           x=min(CH4_ghaday_subset$Observed, CH4_ghaday_subset$Daycent, na.rm=T)*1.1,
           y=max(CH4_ghaday_subset$Observed, CH4_ghaday_subset$Daycent, na.rm=T)*0.7,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(H_rmse))) +
  annotate("text", # mNSE
           x=(max(CH4_ghaday_subset$Observed, na.rm=T)+min(CH4_ghaday_subset$Observed, na.rm=T))/2,
           y=max(CH4_ghaday_subset$Observed, CH4_ghaday_subset$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(H_mNSE))) +
  annotate("text", # pbias
           x=(max(CH4_ghaday_subset$Observed, na.rm=T)+min(CH4_ghaday_subset$Observed, na.rm=T))/2,
           y=max(CH4_ghaday_subset$Observed, CH4_ghaday_subset$Daycent, na.rm=T)*0.85,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(H_pbias))) +
  ggtitle(bquote(.(site_name)~"CH"["4"]*"O Emissions (g ha" ^"-1"*" day"^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +  
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gMG_121


ggsave(filename=paste0(results_path,"calib_Maize_yield_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gMY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soybean_yield_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gSY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Wheat_yield_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gWY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gC_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gT_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gM_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gNG_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_CH4_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gMG_121, width=6, height=6, dpi=300)



#**********************************************************************

# Log results -------------------------------------------------------------


# add this run's results to model log file and file collecting all final
# model runs
calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                       clim_scenario_num,mgmt_scenario_num, scenario_name,
                       scenario_abbrev,
                       MYfit_coef[2], MYfit_coef[1], MYfit_r2, MY_rmse, MY_mNSE, MY_pbias,
                       Maize_obsmod_diff_Mgha,
                       SYfit_coef[2], SYfit_coef[1], SYfit_r2, SY_rmse, SY_mNSE, SY_pbias,
                       Soybean_obsmod_diff_Mgha,
                       WYfit_coef[2], WYfit_coef[1], WYfit_r2, WY_rmse, WY_mNSE, WY_pbias,
                       Wheat_obsmod_diff_Mgha,
                       Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse, C_mNSE, C_pbias,
                       SOC_obsmod_diff_Mgha,SOC_obsmod_diff_Mgha_nooutliers,
                       Tfit_coef[2], Tfit_coef[1], Tfit_r2, T_rmse, T_mNSE, T_pbias,
                       SoilT_obsmod_diff_Mgha,
                       Mfit_coef[2], Mfit_coef[1], Mfit_r2, M_rmse, M_mNSE, M_pbias,
                       SoilM_obsmod_diff_Mgha,
                       Nfit_coef[2], Nfit_coef[1], Nfit_r2, N_rmse, N_mNSE, N_pbias,
                       N2O_obsmod_diff_gha,
                       Hfit_coef[2], Hfit_coef[1], Hfit_r2, H_rmse, H_mNSE, H_pbias,
                       CH4_obsmod_diff_gha,
                       NA, NA, NA, NA, # M Bio
                       NA,
                       NA, NA, NA, NA, # Cotton
                       NA,
                       NA, NA, NA, NA, # Sorghum
                       NA,
                       NA,NA,NA,NA,NA, # M,S,W,C,S cultivars
                       MYfit_coef_time[2], MYfit_coef_time[1], MYfit_r2_time, MY_rmse_time,
                       SYfit_coef_time[2], SYfit_coef_time[1], SYfit_r2_time, SY_rmse_time,
                       WYfit_coef_time[2], WYfit_coef_time[1], WYfit_r2_time, WY_rmse_time,
                       Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, C_rmse_time,
                       Cfit_coef_time_noout[2], Cfit_coef_time_noout[1], Cfit_r2_time_noout, C_rmse_time_noout,
                       Tfit_coef_time[2], Tfit_coef_time[1], Tfit_r2_time, T_rmse_time,
                       Mfit_coef_time[2], Mfit_coef_time[1], Mfit_r2_time, M_rmse_time,
                       Nfit_coef_time[2], Nfit_coef_time[1], Nfit_r2_time, N_rmse_time,
                       Hfit_coef_time[2], Hfit_coef_time[1], Hfit_r2_time, H_rmse_time, # methane
                       NA, NA, NA, NA, # microbio
                       NA, NA, NA, NA, # cotton
                       NA, NA, NA, NA # sorghum
                       )


source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_log_tab,model_name,scenario_name)

rm(calib_log_tab,
   MYfit_coef, MYfit_r2, MY_rmse, MY_mNSE, MY_pbias,
   Maize_obsmod_diff_Mgha,
   SYfit_coef, SYfit_r2, SY_rmse, SY_mNSE, SY_pbias,
   Soybean_obsmod_diff_Mgha,
   WYfit_coef, WYfit_r2, WY_rmse, WY_mNSE, WY_pbias,
   Wheat_obsmod_diff_Mgha,
   Cfit_coef, Cfit_r2, C_rmse, C_mNSE, C_pbias,
   SOC_obsmod_diff_Mgha,SOC_obsmod_diff_Mgha_nooutliers,
   Tfit_coef, Tfit_r2, T_rmse, T_mNSE, T_pbias,
   T_rmse_error_time,
   T_rmse_time,
   Tfit_time,
   Tfit_coef_time,
   SoilT_obsmod_diff_Mgha,
   Mfit_coef, Mfit_r2, M_rmse, M_mNSE, M_pbias,
   SoilM_obsmod_diff_Mgha,
   Nfit_coef, Nfit_r2, N_rmse, N_mNSE, N_pbias,
   N2O_obsmod_diff_gha,
   Hfit_coef, Hfit_r2, H_rmse, H_mNSE, H_pbias,
   CH4_obsmod_diff_gha,
   MYfit_coef_time, MYfit_r2_time, MY_rmse_time,
   SYfit_coef_time, SYfit_r2_time, SY_rmse_time,
   WYfit_coef_time, WYfit_r2_time, WY_rmse_time,
   Cfit_coef_time, Cfit_r2_time, C_rmse_time,
   Cfit_coef_time_noout, Cfit_r2_time_noout, C_rmse_time_noout,
   Tfit_coef_time, Tfit_r2_time, T_rmse_time,
   Mfit_coef_time, Mfit_r2_time, M_rmse_time,
   Nfit_coef_time, Nfit_r2_time, N_rmse_time,
   Hfit_coef_time, Hfit_r2_time, H_rmse_time)

}) # end suppressMessages