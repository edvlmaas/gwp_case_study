#######################################
# Script: 9_Results_Daycent-calibration_LRF.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Runs calibration graphs for the Daycent simulation at
# LRF, TX. Also logs a selection of results for final processing
# in the "10_" series of scripts.
#######################################

suppressMessages({

print(paste0("Starting 9_Results_Daycent-calibration_",site_name,".R"))

library(magrittr)
library(tidyverse)
library(graphics)
library(ggplot2)

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
  
gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Daycent'
                        & SoilMoist_VSM_piv$date>="2003-01-01"
                        & SoilMoist_VSM_piv$date<experiment_end_date,] %>%
  ggplot(aes(x=date, y=h2o_val, color=source)) +
  geom_point() +
  geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed'
                                    & SoilMoist_VSM_piv$date>="2003-01-01"
                                    & SoilMoist_VSM_piv$date<experiment_end_date,],
             aes(x=date, y=h2o_val, color=source)) +
  xlab("Year") +
  ylab("Volumetric soil moisture (%)") +
  ggtitle(paste0(site_name," Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=c(Daycent_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM

Mfit_time2 <- lm(Daycent ~ year, data = Moist_this)
Mfit_coef_time2 <- coef(Mfit_time2)
DaycentM_slope_byyear <- Mfit_coef_time2[2]


Cotton_this_piv <- CottonYld_Mgha_piv[CottonYld_Mgha_piv$year %in% experiment_year_range &
                                        CottonYld_Mgha_piv$source %in% c("Daycent","Observed"),]

Cotton_this <- CottonYld_Mgha[CottonYld_Mgha$year %in% experiment_year_range,]
CYfit_time <- lm(Daycent ~ year, data = Cotton_this)
CYfit_coef_time <- coef(CYfit_time)
CYfit_r2_time <- round(summary(CYfit_time)$r.squared,2)
CY_rmse_error_time <- Cotton_this$Observed-Cotton_this$Daycent
CY_rmse_time <- round(sqrt(mean(CY_rmse_error_time^2,na.rm=TRUE)),2)

gCY <- Cotton_this_piv %>%
  ggplot(aes(x=year, y=yield_val, color=source)) +
  geom_point() +
  annotate("text", # RMSE
           x=min(Cotton_this_piv$year, na.rm=T),
           y=max(Cotton_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(CY_rmse_time))) +
  xlab("Year") +
  ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Cotton Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=c(Daycent_color,Observed_color)) +
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
  scale_color_manual(labels=c("Daycent","Historical","Observed"),
                     values=c(Daycent_color,Historical_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gChY

if(mgmt_scenario_grp != 7) {

Sorghum_this_piv <- SorghumYld_Mgha_piv[SorghumYld_Mgha_piv$year %in% experiment_year_range &
                                          SorghumYld_Mgha_piv$source %in% c("Daycent","Observed"),]

Sorghum_this <- SorghumYld_Mgha[SorghumYld_Mgha$year %in% experiment_year_range,]
SYfit_time <- lm(Daycent ~ year, data = Sorghum_this)
SYfit_coef_time <- coef(SYfit_time)
SYfit_r2_time <- round(summary(SYfit_time)$r.squared,2)
SY_rmse_error_time <- Sorghum_this$Observed-Sorghum_this$Daycent
SY_rmse_time <- round(sqrt(mean(SY_rmse_error_time^2,na.rm=TRUE)),2)

gSY <- Sorghum_this_piv %>%
  ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  annotate("text", # RMSE
           x=min(Sorghum_this_piv$year, na.rm=T),
           y=max(Sorghum_this_piv$yield_val, na.rm=T),
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse_time))) +
  xlab("Year") +
  ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
  ggtitle(paste(site_name,"Sorghum Yield"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=c(Daycent_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 20) +
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
  scale_color_manual(labels=c("Daycent","Historical","Observed"),
                     values=c(Daycent_color,Historical_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 22) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gShY


## SOC

SOC_this_piv <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range,]

SOC_this <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,]
Cfit_time <- lm(Daycent ~ year, data = SOC_this)
Cfit_coef_time <- coef(Cfit_time)
Cfit_r2_time <- round(summary(Cfit_time)$r.squared,2)
C_rmse_error_time <- SOC_this$Observed-SOC_this$Daycent
C_rmse_time <- round(sqrt(mean(C_rmse_error_time^2,na.rm=TRUE)),2)

ObsCfit_all <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gC <- SOC_this_piv[SOC_this_piv$year %in% experiment_year_range,] %>%#experiment_year_range,] %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color=Observed_color) +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  ylim(3,12) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  geom_abline(intercept=Cfit_coef_time[1], slope=Cfit_coef_time[2], color=Daycent_color) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=c(Daycent_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 

SOC_diff <- lis_output[lis_output$time==land_conversion_year,"somsc_gm2"]-
  lis_output[lis_output$time==experiment_start_year-1,"somsc_gm2"]
SOC_landconv <- lis_output[lis_output$time==land_conversion_year,"somsc_gm2"]
SOC_startexp <- unique(lis_output[lis_output$time==experiment_start_year,"somsc_gm2"])


## SOC with spin-up

Cfith_Daycent <- coef(lm(Daycent ~ year, data = Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,]))
Cfith_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gCh <- Cstock_Mgha_piv %>%#experiment_year_range,] %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_abline(intercept=Cfith_Obs[1], slope=Cfith_Obs[2], color=Observed_color) +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
#  ylim(0,12) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  geom_abline(intercept=Cfith_Daycent[1], slope=Cfith_Daycent[2], color=Daycent_color) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=c(Daycent_color,Observed_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCh

Cat_landconv <- lis_output[lis_output$time==land_conversion_year,"somsc_gm2"]
Cat_startexp <- unique(lis_output[lis_output$time==experiment_start_year,"somsc_gm2"])
Cdiff_landconv_startexp <- lis_output[lis_output$time==land_conversion_year,"somsc_gm2"]-
  lis_output[lis_output$time==experiment_start_year-1,"somsc_gm2"]


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
  ggplot(aes(x=date, y=temp_val, color=source)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=Temp_this_piv[Temp_this_piv$source=='Observed'
                                 & Temp_this_piv$year %in% ObsTemp$year,],
             aes(x=date, y=temp_val, color=source)) +
  xlab("Year") +
  ylab(expression('Soil temperature (' ^o*'C)')) +
  ggtitle(paste0(site_name," Soil Temperature"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed"),
                     values=c(Daycent_color,Observed_color)) +
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
  geom_segment(data=Fert[Fert$treatment==treatment & 
                           year(Fert$date) %in% year(ObsGas$date) &
                           Fert$totalN_kgha>10,],
               aes(x = date, y = 20,
                   xend = date, yend = 16),
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
  scale_color_manual(labels=c("Daycent","Fertilizer"),
                     values=c(Daycent_color,Fertilizer_color)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG


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
             aes(x=date, y=ch4_val, color=source)) +
  xlab("Year") +
  ylab(expression('CH'[4]*' Emissions (g C ha ' ^-1*'day ' ^-1*')')) +
  ggtitle(bquote(.(site_name)~"CH"["4"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Daycent","Observed","Fertilizer"),
                     values=c(Daycent_color,Observed_color,Fertilizer_color)) +
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
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCI

gCI <- DayCI_gm2yr[DayCI_gm2yr$year %in% experiment_year_range,] %>%
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

gNI <- DayNI_gm2yr[DayNI_gm2yr$year <= experiment_end_year,] %>%
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

gNH4 <- Day_soiln[Day_soiln$year <= experiment_end_year,] %>%
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

gNO3 <- Day_soiln[Day_soiln$year <= experiment_end_year,] %>%
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

# explain N2O emissions with SAT, DUL, sw, NO3 and N2O
transform_factor <- 200


ggsave(filename=paste0(results_path,"calib_Cotton_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gCY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Cotton_hist_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gChY,
       width=9, height=6, dpi=300)
if(mgmt_scenario_grp != 7) {
ggsave(filename=paste0(results_path,"calib_Sorghum_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gSY,
       width=6, height=6, dpi=300)
}
ggsave(filename=paste0(results_path,"calib_Sorghum_hist_yield_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gShY,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gC,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_base_",scenario_name,"_Daycent.jpg"),plot=gCh,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gT,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gM,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_exp_",scenario_name,"_Daycent.jpg"),plot=gNG,
       width=6, height=6, dpi=300)
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


#**********************************************************************

# 1:1 graphs --------------------------------------------------------------

CYfit <- lm(Daycent ~ Observed, data = CottonYld_Mgha)
CYfit_coef <- coef(CYfit)
CYfit_r2 <- round(summary(CYfit)$r.squared,2)

CY_rmse_error <- CottonYld_Mgha$Observed-CottonYld_Mgha$Daycent
CY_rmse <- round(sqrt(mean(CY_rmse_error^2,na.rm=TRUE)),2)

gCY_121 <- CottonYld_Mgha %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=CYfit_coef[1], slope=CYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$Daycent, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$Daycent, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(CYfit_coef[2],4))~"x" ~+ ~.(round(CYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$Daycent, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$Daycent, na.rm=T)*.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(CYfit_r2))) +
  annotate("text", # RMSE
           x=min(CottonYld_Mgha$Observed, CottonYld_Mgha$Daycent, na.rm=T)*1.1,
           y=max(CottonYld_Mgha$Observed, CottonYld_Mgha$Daycent, na.rm=T)*0.88,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(CY_rmse))) +
  ggtitle(bquote(.(site_name)~"Cotton Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,

gCY_121



if(mgmt_scenario_grp != 7 & mgmt_scenario_grp != 5) {
  ##
  SYfit <- lm(Daycent ~ Observed, data = SorghumYld_Mgha)
  SYfit_coef <- coef(SYfit)
  SYfit_r2 <- round(summary(SYfit)$r.squared,2)
  
  SY_rmse_error <- SorghumYld_Mgha$Observed-SorghumYld_Mgha$Daycent
  SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)
  
  gSY_121 <- SorghumYld_Mgha %>%
    ggplot(aes(x=Observed, y=Daycent,
               xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
               ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
    geom_point() +
    geom_abline() +
    geom_abline(intercept=SYfit_coef[1], slope=SYfit_coef[2], color="blue") +
    annotate("text", # line equation
             x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$Daycent, na.rm=T)*1.1,
             y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$Daycent, na.rm=T)*1,
             hjust=0, family="serif", color="gray31",
             label=bquote("y =" ~.(round(SYfit_coef[2],4))~"x" ~+ ~.(round(SYfit_coef[1],4)))) +
    annotate("text", # R^2
             x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$Daycent, na.rm=T)*1.1,
             y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$Daycent, na.rm=T)*0.95,
             hjust=0, family="serif", color="gray31",
             label=bquote(R^2 ~"=" ~.(SYfit_r2))) +
    annotate("text", # RMSE
             x=min(SorghumYld_Mgha$Observed, SorghumYld_Mgha$Daycent, na.rm=T)*1.1,
             y=max(SorghumYld_Mgha$Observed, SorghumYld_Mgha$Daycent, na.rm=T)*0.89,
             hjust=0, family="serif", color="gray31",
             label=bquote("RMSE =" ~.(SY_rmse))) +
    ggtitle(bquote(.(site_name)~"Sorghum Yield (Mg ha" ^"-1"*")"),
            paste0("Scenario: ",scenario_descriptor)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line())#,

  gSY_121
}


## soil organic C
Cfit <- lm(Daycent ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year>=experiment_start_date,])
Cfit_coef <- coef(Cfit)
Cfit_r2 <- round(summary(Cfit)$r.squared,2)

C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$Daycent
C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)

gC_121 <- Cstock_Mgha[Cstock_Mgha$year>=experiment_start_date,] %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  ggtitle(bquote(.(site_name)~"SOC Stock (Mg C ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

gC_121

## soil temperature
Tfit <- lm(Daycent ~ Observed, data = SoilTemp_C)
Tfit_coef <- coef(Tfit)
Tfit_r2 <- round(summary(Tfit)$r.squared,2)

T_rmse_error <- SoilTemp_C$Observed-SoilTemp_C$Daycent
T_rmse <- round(sqrt(mean(T_rmse_error^2,na.rm=TRUE)),2)

gT_121 <- SoilTemp_C %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Tfit_coef[1], slope=Tfit_coef[2], color="blue") +
  annotate("text",x=-3,y=-10,label=paste0("R^2=",Tfit_r2)) +
  ggtitle(bquote(.(site_name)~"Soil Temperature ("*degree*"C)"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

gT_121

## soil temperature - calibrated
Tfit <- lm(Daycent ~ Observed, data = SoilTemp_C)
Tfit_coef <- coef(Tfit)
Tfit_r2 <- round(summary(Tfit)$r.squared,2)

T_rmse_error <- SoilTemp_C$Observed-SoilTemp_C$Daycent
T_rmse <- round(sqrt(mean(T_rmse_error^2,na.rm=TRUE)),2)

gTc_121 <- SoilTemp_C %>%
  ggplot(aes(x=Observed, y=Daycent,
             xmin=min(Observed, Daycent, na.rm=T), xmax=max(Observed, Daycent, na.rm=T),
             ymin=min(Observed, Daycent, na.rm=T), ymax=max(Observed, Daycent, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Tfit_coef[1], slope=Tfit_coef[2], color="blue") +
  annotate("text",x=-3,y=-10,label=paste0("R^2=",Tfit_r2)) +
  ggtitle(bquote(.(site_name)~"Soil Temperature ("*degree*"C)"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

gTc_121


ggsave(filename=paste0(results_path,"calib_Cotton_yield_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gCY_121, width=6, height=6, dpi=300)
if(mgmt_scenario_grp!=7 & mgmt_scenario_grp!=5) {
ggsave(filename=paste0(results_path,"calib_Sorghum_yield_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gSY_121, width=6, height=6, dpi=300)
}
ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gC_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_",scenario_name,"_Daycent.jpg"),
       plot=gT_121, width=6, height=6, dpi=300)


#**********************************************************************

# Log results -------------------------------------------------------------


# add this run's results to model log file and file collecting all final
# model runs. scenario 7 is continuous cotton, so no sorghum; scenario 5
# is a cotton-sorghum rotation, but sorghum failed early and there aren't
# enough data points to generate these stats
if(mgmt_scenario_grp == 7 | mgmt_scenario_grp == 5) {
  calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                         clim_scenario_num,mgmt_scenario_num, scenario_name,
                         scenario_abbrev,
                         NA, NA, NA, NA, NA, NA, # Maize 1 to 1
                         NA,
                         NA, NA, NA, NA, NA, NA, # Soybean 1 to 1
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
                         NA, NA, NA, # maize, soybean, wheat cultivars
                         NA, NA, # cotton, sorghum cultivars
                         NA, NA, NA, NA, # Maize time series
                         NA, NA, NA, NA, # Soybean time series
                         NA, NA, NA, NA, # Wheat time series
                         Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, NA,
                         NA, NA, NA, NA, # SOC w/o outliers
                         Tfit_coef_time[2], Tfit_coef_time[1], Tfit_r2_time, NA,
                         Mfit_coef_time[2], Mfit_coef_time[1], Mfit_r2_time, NA, # moist time series
                         Nfit_coef_time[2], Nfit_coef_time[1], Nfit_r2_time, NA, # n2o time series
                         Hfit_coef_time[2], Hfit_coef_time[1], Hfit_r2_time, NA, # CH4 time series
                         NA, NA, NA, NA, # M Bio time series
                         CYfit_coef[2], CYfit_coef[1], CYfit_r2, NA,
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
                         SYfit_coef[2], SYfit_coef[1], SYfit_r2, SY_rmse,
                         Sorghum_obsmod_diff_Mgha,
                         NA, NA, NA, # maize, soybean, wheat cultivars
                         NA, NA, # cotton, sorghum cultivars
                         NA, NA, NA, NA, # Maize time series
                         NA, NA, NA, NA, # Soybean time series
                         NA, NA, NA, NA, # Wheat time series
                         Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, NA,
                         NA, NA, NA, NA, # SOC w/o outliers
                         Tfit_coef_time[2], Tfit_coef_time[1], Tfit_r2_time, NA,
                         Mfit_coef_time[2], Mfit_coef_time[1], Mfit_r2_time, NA, # moist time series
                         Nfit_coef_time[2], Nfit_coef_time[1], Nfit_r2_time, NA, # n2o time series
                         Hfit_coef_time[2], Hfit_coef_time[1], Hfit_r2_time, NA, # CH4 time series
                         NA, NA, NA, NA, # M Bio time series
                         CYfit_coef[2], CYfit_coef[1], CYfit_r2, NA,
                         SYfit_coef_time[2], SYfit_coef_time[1], SYfit_r2_time, NA # Sorghum time series
                         )
}

source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_log_tab,model_name,scenario_name)


rm(calib_log_tab,
   Cotton_this_piv,
   Cotton_this,
   CYfit_time,
   CYfit_coef_time,
   CYfit_r2_time,
   CY_rmse_error_time,
   CY_rmse_time,
   gCY,
   gChY,
   gShY,
   SOC_this_piv,
   SOC_this,
   Cfit_time,
   Cfit_coef_time,
   Cfit_r2_time,
   C_rmse_error_time,
   C_rmse_time,
   ObsCfit_all,
   gC,
   Temp_this_piv,
   Temp_this,
   Tfit_time,
   Tfit_coef_time,
   Tfit_r2_time,
   T_rmse_error_time,
   T_rmse_time,
   gT,
   gM,
   N2O_this_piv,
   N2O_this,
   Nfit_time,
   Nfit_coef_time,
   Nfit_r2_time,
   N_rmse_error_time,
   N_rmse_time,
   gNG,
   CYfit,
   CYfit_coef,
   CYfit_r2,
   CY_rmse_error,
   CY_rmse,
   gCY_121,
   Cfit,
   Cfit_coef,
   Cfit_r2,
   C_rmse_error,
   C_rmse,
   gC_121,
   Tfit,
   Tfit_coef,
   Tfit_r2,
   T_rmse_error,
   T_rmse,
   gT_121,
   gTc_121,
CH4_this_piv,
CH4_this,
Hfit_time,
Hfit_coef_time,
Hfit_r2_time,
H_rmse_error_time,
H_rmse_time,
gMG,
gCI,
gNI,
gNH4,
gNO3,
transform_factor)

if(mgmt_scenario_grp != 7 & mgmt_scenario_grp != 5) {
  rm(
    Sorghum_this_piv,
    Sorghum_this,
    SYfit_time,
    SYfit_coef_time,
    SYfit_r2_time,
    SY_rmse_error_time,
    SY_rmse_time ,
    gSY,
     SYfit,
     SYfit_coef,
     SYfit_r2,
     SY_rmse_error,
     SY_rmse,
     gSY_121)
}

}) # end suppressMessages
