#######################################
# Script: 9_Results_APSIM-calibration2_KBS.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Runs calibration graphs for the APSIM simulation at
# KBS, MI. Also logs a selection of results for final processing
# in the "10_" series of scripts.
#######################################

suppressMessages({
  
  print(paste0("Starting 9_Results_APSIM-calibration2_",site_name,".R"))
  
library(magrittr)
library(tidyverse)
library(graphics)
library(ggplot2)
  library(hydroGOF)

  #**********************************************************************
  
# Temporal graphs ---------------------------------------------------------


## experimental period

  
  Maize_this_piv <- MaizeYld_Mgha_piv[MaizeYld_Mgha_piv$year %in% experiment_year_range,]
  
  Maize_this <- MaizeYld_Mgha[MaizeYld_Mgha$year %in% experiment_year_range,]
  MYfit_time <- lm(APSIM ~ year, data = Maize_this)
  MYfit_coef_time <- coef(MYfit_time)
  MYfit_r2_time <- round(summary(MYfit_time)$r.squared,2)
  MY_rmse_error_time <- Maize_this$Observed-Maize_this$APSIM
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
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMY

Soy_this_piv <- SoyYld_Mgha_piv[SoyYld_Mgha_piv$year %in% experiment_year_range,]

Soy_this <- SoyYld_Mgha[SoyYld_Mgha$year %in% experiment_year_range,]
SYfit_time <- lm(APSIM ~ year, data = Soy_this)
SYfit_coef_time <- coef(SYfit_time)
SYfit_r2_time <- round(summary(SYfit_time)$r.squared,2)
SY_rmse_error_time <- Soy_this$Observed-Soy_this$APSIM
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
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSY

Wheat_this_piv <- WheatYld_Mgha_piv[WheatYld_Mgha_piv$year %in% experiment_year_range,]
Wheat_this <- WheatYld_Mgha[WheatYld_Mgha$year %in% experiment_year_range,]

WYfit_time <- lm(APSIM ~ year, data = Wheat_this)
WYfit_coef_time <- coef(WYfit_time)
WYfit_r2_time <- round(summary(WYfit_time)$r.squared,2)

WY_rmse_error_time <- Wheat_this$Observed-Wheat_this$APSIM
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
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gWY

# SOC

SOC_this_piv <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range,]

## calculate stats with outliers
SOC_this <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,]
Cfit_time <- lm(APSIM ~ year, data = SOC_this)
Cfit_coef_time <- coef(Cfit_time)
Cfit_r2_time <- round(summary(Cfit_time)$r.squared,2)
C_rmse_error_time <- SOC_this$Observed-SOC_this$APSIM
C_rmse_time <- round(sqrt(mean(C_rmse_error_time^2,na.rm=TRUE)),2)
## calculate stats without outliers
SOC_this_noout <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range &
                          !(Cstock_Mgha$Observed %in% ObsC_outliers),]
Cfit_time_noout <- lm(APSIM ~ year, data = SOC_this_noout)
Cfit_coef_time_noout <- coef(Cfit_time_noout)
Cfit_r2_time_noout <- round(summary(Cfit_time_noout)$r.squared,2)
C_rmse_error_time_noout <- SOC_this_noout$Observed-SOC_this_noout$APSIM
C_rmse_time_noout <- round(sqrt(mean(C_rmse_error_time_noout^2,na.rm=TRUE)),2)

## include outliers in trend line here
ObsCfit_all <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gC <- SOC_this_piv %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  #ylim(10,35) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  #  geom_abline(intercept=Cfit_APSIM[1], slope=Cfit_APSIM[2], color="orange") +
  geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color="black") +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 

# Soil temperature

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
  geom_abline(intercept=Tfit_coef_time[1], slope=Tfit_coef_time[2], color="orange") +
  geom_abline(intercept=Tfit_coef_time_obs[1], slope=Tfit_coef_time_obs[2], color="black") +
  xlab("Year") +
  ylab(expression('Soil temperature ( '*degree*C*")")) +
  ggtitle(paste0(site_name," Soil Temperature"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT

Tfit_time2 <- lm(APSIM ~ year, data = Temp_this)
Tfit_coef_time2 <- coef(Tfit_time2)
APSIMT_slope_byyear <- Tfit_coef_time2[2]

Moist_this_piv <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$year %in% experiment_year_range,]
Moist_this <- SoilMoist_VSM[year(SoilMoist_VSM$date) %in% experiment_year_range,] %>%
  mutate(year=year(date))

Mfit_time <- lm(APSIM ~ date, data = Moist_this)
Mfit_coef_time <- coef(Mfit_time)
Mfit_r2_time <- round(summary(Mfit_time)$r.squared,2)

M_rmse_error_time <- Moist_this$Observed-Moist_this$APSIM
M_rmse_time <- round(sqrt(mean(M_rmse_error_time^2,na.rm=TRUE)),2)

gM <- Moist_this_piv[Moist_this_piv$source=='APSIM' 
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
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM

Mfit_time2 <- lm(APSIM ~ year, data = Moist_this)
Mfit_coef_time2 <- coef(Mfit_time2)
APSIMM_slope_byyear <- Mfit_coef_time2[2]

N2O_this_piv <- N2O_ghaday_piv[year(N2O_ghaday_piv$date) %in% experiment_year_range,]
N2O_this <- N2O_ghaday[year(N2O_ghaday$date) %in% experiment_year_range,]

Nfit_time <- lm(APSIM ~ date, data = N2O_this)
Nfit_coef_time <- coef(Nfit_time)
Nfit_r2_time <- round(summary(Nfit_time)$r.squared,2)

N_rmse_error_time <- N2O_this$Observed-N2O_this$APSIM
N_rmse_time <- round(sqrt(mean(N_rmse_error_time^2,na.rm=TRUE)),2)

gNG <- N2O_this_piv[N2O_this_piv$source=='APSIM' &
                        year(N2O_this_piv$date) %in% year(ObsGas$date),] %>%
  ggplot(aes(x=date, y=n2o_val, color=source)) +
  geom_line(show.legend=TRUE) +
  geom_point(data=N2O_this_piv[N2O_this_piv$source=='Observed'&
                                   year(N2O_this_piv$date) %in% year(ObsGas$date),],
             aes(x=date, y=n2o_val, color=source),size=1) +
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
  scale_color_manual(labels=c("APSIM","Observed","Fertilizer"),
                     values=cbPalette9[c(8,1,7)]) +
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
  ylab("Rain (mm)") +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("APSIM","Rain","Observed"),
                     values=cbPalette9[c(8,4,1)]) +
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

gNO3 <- APSIMNO3_ghaday %>%
  ggplot(aes(x=year, y=NO3_20cm/1000)) + 
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

ggsave(filename=paste0(results_path,"calib_Maize_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gMY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soybean_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gSY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Wheat_yield_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gWY,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gC,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gT,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gM,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_exp_",scenario_name,"_APSIM.jpg"),plot=gNG,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_rain5yr_exp_",scenario_name,"_APSIM.jpg"),plot=gNG_rain5yr,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_NO3_input_exp_",scenario_name,"_APSIM.jpg"),plot=gNO3,
       width=9, height=6, dpi=300)


#**********************************************************************

# 1:1 graphs --------------------------------------------------------------

## soil moisture
SoilMoist_VSM_subset <- SoilMoist_VSM[!is.na(SoilMoist_VSM$Observed)&!is.na(SoilMoist_VSM$APSIM),]

Mfit <- lm(APSIM ~ Observed, data = SoilMoist_VSM_subset)
Mfit_coef <- coef(Mfit)
Mfit_r2 <- round(summary(Mfit)$r.squared,2)
# M_rmse_error <- SoilMoist_VSM$Observed-SoilMoist_VSM$APSIM
# M_rmse <- round(sqrt(mean(M_rmse_error^2,na.rm=TRUE)),2)
M_rmse <- round(hydroGOF::rmse(SoilMoist_VSM_subset$APSIM,SoilMoist_VSM_subset$Observed),2) 
M_mNSE <- round(hydroGOF::mNSE(SoilMoist_VSM_subset$APSIM,SoilMoist_VSM_subset$Observed),2)
M_pbias <- round(hydroGOF::pbias(SoilMoist_VSM_subset$APSIM,SoilMoist_VSM_subset$Observed),2)

gM_121 <- SoilMoist_VSM_subset %>%
  ggplot(aes(x=Observed, y=APSIM, 
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T), 
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Mfit_coef[1], slope=Mfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$APSIM, na.rm=T)*1.1,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Mfit_coef[2],4))~"x" ~+ ~.(round(Mfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$APSIM, na.rm=T)*1.1,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Mfit_r2))) +
  annotate("text", # RMSE
           x=min(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$APSIM, na.rm=T)*1.1,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(M_rmse))) +
  annotate("text", # mNSE
           x=(max(SoilMoist_VSM_subset$Observed, na.rm=T)+min(SoilMoist_VSM_subset$Observed, na.rm=T))/2,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(M_mNSE))) +
  annotate("text", # pbias
           x=(max(SoilMoist_VSM_subset$Observed, na.rm=T)+min(SoilMoist_VSM_subset$Observed, na.rm=T))/2,
           y=max(SoilMoist_VSM_subset$Observed, SoilMoist_VSM_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(M_pbias))) +
  ggtitle(paste0(site_name," Volumetric Soil Moisture"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gM_121

## maize
MaizeYld_Mgha_subset <- MaizeYld_Mgha[!is.na(MaizeYld_Mgha$Observed),]

MYfit <- lm(APSIM ~ Observed, data = MaizeYld_Mgha_subset)
MYfit_coef <- coef(MYfit)
MYfit_r2 <- round(summary(MYfit)$r.squared,2)
# MY_rmse_error <- MaizeYld_Mgha$Observed-MaizeYld_Mgha$APSIM
# MY_rmse <- round(sqrt(mean(MY_rmse_error^2,na.rm=TRUE)),2)
MY_rmse <- round(hydroGOF::rmse(MaizeYld_Mgha_subset$APSIM,MaizeYld_Mgha_subset$Observed),2) 
MY_mNSE <- round(hydroGOF::mNSE(MaizeYld_Mgha_subset$APSIM,MaizeYld_Mgha_subset$Observed),2)
MY_pbias <- round(hydroGOF::pbias(MaizeYld_Mgha_subset$APSIM,MaizeYld_Mgha_subset$Observed),2)

gMY_121 <- MaizeYld_Mgha_subset %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=MYfit_coef[1], slope=MYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(MYfit_coef[2],4))~"x" ~+ ~.(round(MYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(MYfit_r2))) +
  annotate("text", # RMSE
           x=min(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(MY_rmse))) +
  annotate("text", # mNSE
           x=(max(MaizeYld_Mgha_subset$Observed, na.rm=T)+min(MaizeYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(MY_mNSE))) +
  annotate("text", # pbias
           x=(max(MaizeYld_Mgha_subset$Observed, na.rm=T)+min(MaizeYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(MaizeYld_Mgha_subset$Observed, MaizeYld_Mgha_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(MY_pbias))) +
  ggtitle(bquote(.(site_name)~"Maize Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,
        #plot.title = element_text(hjust = 0.5))

gMY_121

## soy
SoyYld_Mgha_subset <- SoyYld_Mgha[!is.na(SoyYld_Mgha$Observed),]

SYfit <- lm(APSIM ~ Observed, data = SoyYld_Mgha_subset)
SYfit_coef <- coef(SYfit)
SYfit_r2 <- round(summary(SYfit)$r.squared,2)
# SY_rmse_error <- SoyYld_Mgha$Observed-SoyYld_Mgha$APSIM
# SY_rmse <- round(sqrt(mean(SY_rmse_error^2,na.rm=TRUE)),2)
SY_rmse <- round(hydroGOF::rmse(SoyYld_Mgha_subset$APSIM,SoyYld_Mgha_subset$Observed),2) 
SY_mNSE <- round(hydroGOF::mNSE(SoyYld_Mgha_subset$APSIM,SoyYld_Mgha_subset$Observed),2)
SY_pbias <- round(hydroGOF::pbias(SoyYld_Mgha_subset$APSIM,SoyYld_Mgha_subset$Observed),2)

gSY_121 <- SoyYld_Mgha_subset %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=SYfit_coef[1], slope=SYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(SYfit_coef[2],4))~"x" ~+ ~.(round(SYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(SYfit_r2))) +
  annotate("text", # RMSE
           x=min(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(SY_rmse))) +
  annotate("text", # mNSE
           x=(max(SoyYld_Mgha_subset$Observed, na.rm=T)+min(SoyYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(SY_mNSE))) +
  annotate("text", # pbias
           x=(max(SoyYld_Mgha_subset$Observed, na.rm=T)+min(SoyYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(SoyYld_Mgha_subset$Observed, SoyYld_Mgha_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(SY_pbias))) +
  ggtitle(bquote(.(site_name)~"Soybean Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())#,
        #plot.title = element_text(hjust = 0.5))

gSY_121

## wheat
WheatYld_Mgha_subset <- WheatYld_Mgha[!is.na(WheatYld_Mgha$Observed),]

WYfit <- lm(APSIM ~ Observed, data = WheatYld_Mgha_subset)
WYfit_coef <- coef(WYfit)
WYfit_r2 <- round(summary(WYfit)$r.squared,2)
# WY_rmse_error <- WheatYld_Mgha$Observed-WheatYld_Mgha$APSIM
# WY_rmse <- round(sqrt(mean(WY_rmse_error^2,na.rm=TRUE)),2)
WY_rmse <- round(hydroGOF::rmse(WheatYld_Mgha_subset$APSIM,WheatYld_Mgha_subset$Observed),2) 
WY_mNSE <- round(hydroGOF::mNSE(WheatYld_Mgha_subset$APSIM,WheatYld_Mgha_subset$Observed),2)
WY_pbias <- round(hydroGOF::pbias(WheatYld_Mgha_subset$APSIM,WheatYld_Mgha_subset$Observed),2)

WheatYld_Mgha_subset <- WheatYld_Mgha[!is.na(WheatYld_Mgha$Observed),]

gWY_121 <- WheatYld_Mgha_subset %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=WYfit_coef[1], slope=WYfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(WYfit_coef[2],4))~"x" ~+ ~.(round(WYfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(WYfit_r2))) +
  annotate("text", # RMSE
           x=min(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(WY_rmse))) +
  annotate("text", # mNSE
           x=(max(WheatYld_Mgha_subset$Observed, na.rm=T)+min(WheatYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(WY_mNSE))) +
  annotate("text", # pbias
           x=(max(WheatYld_Mgha_subset$Observed, na.rm=T)+min(WheatYld_Mgha_subset$Observed, na.rm=T))/2,
           y=max(WheatYld_Mgha_subset$Observed, WheatYld_Mgha_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(WY_pbias))) +
  ggtitle(bquote(.(site_name)~"Wheat Yield (Mg ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gWY_121

##  SOC
Cstock_Mgha_subset <- Cstock_Mgha[Cstock_Mgha$year>=experiment_start_date &
                                    !is.na(Cstock_Mgha$Observed),]

if(mgmt_scenario_grp==3) {
  Cfit <- lm(APSIM ~ Observed, data = Cstock_Mgha_subset[Cstock_Mgha$year!=1998,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  # C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$APSIM
  # C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
} else {
  Cfit <- lm(APSIM ~ Observed, data = Cstock_Mgha_subset)
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
#   C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$APSIM
#   C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
}
C_rmse <- round(hydroGOF::rmse(Cstock_Mgha_subset$APSIM,Cstock_Mgha_subset$Observed),2) 
C_mNSE <- round(hydroGOF::mNSE(Cstock_Mgha_subset$APSIM,Cstock_Mgha_subset$Observed),2)
C_pbias <- round(hydroGOF::pbias(Cstock_Mgha_subset$APSIM,Cstock_Mgha_subset$Observed),2)

gC_121 <- Cstock_Mgha_subset %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$APSIM, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$APSIM, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  annotate("text", # mNSE
           x=(max(Cstock_Mgha_subset$Observed, na.rm=T)+min(Cstock_Mgha_subset$Observed, na.rm=T))/2,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(C_mNSE))) +
  annotate("text", # pbias
           x=(max(Cstock_Mgha_subset$Observed, na.rm=T)+min(Cstock_Mgha_subset$Observed, na.rm=T))/2,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(C_pbias))) +
  ggtitle(bquote(.(site_name)~"SOC Stock (Mg C ha" ^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gC_121

## soil temp
SoilTemp_C_subset <- SoilTemp_C[!is.na(SoilTemp_C$Observed)&!is.na(SoilTemp_C$APSIM),]

Tfit <- lm(APSIM ~ Observed, data = SoilTemp_C_subset)
Tfit_coef <- coef(Tfit)
Tfit_r2 <- round(summary(Tfit)$r.squared,2)
# T_rmse_error <- SoilTemp_C$Observed-SoilTemp_C$APSIM
# T_rmse <- round(sqrt(mean(T_rmse_error^2,na.rm=TRUE)),2)
T_rmse <- round(hydroGOF::rmse(SoilTemp_C_subset$APSIM,SoilTemp_C_subset$Observed),2) 
T_mNSE <- round(hydroGOF::mNSE(SoilTemp_C_subset$APSIM,SoilTemp_C_subset$Observed),2)
T_pbias <- round(hydroGOF::pbias(SoilTemp_C_subset$APSIM,SoilTemp_C_subset$Observed),2)

gT_121 <- SoilTemp_C_subset %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Tfit_coef[1], slope=Tfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(SoilTemp_C_subset$Observed, SoilTemp_C_subset$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Tfit_coef[2],4))~"x" ~+ ~.(round(Tfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(SoilTemp_C_subset$Observed, SoilTemp_C_subset$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$APSIM, na.rm=T)*0.92,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Tfit_r2))) +
  annotate("text", # RMSE
           x=min(SoilTemp_C_subset$Observed, SoilTemp_C_subset$APSIM, na.rm=T)*1.1,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$APSIM, na.rm=T)*0.82,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(T_rmse))) +
  annotate("text", # mNSE
           x=(max(SoilTemp_C_subset$Observed, na.rm=T)+min(SoilTemp_C_subset$Observed, na.rm=T))/2,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(T_mNSE))) +
  annotate("text", # pbias
           x=(max(SoilTemp_C_subset$Observed, na.rm=T)+min(SoilTemp_C_subset$Observed, na.rm=T))/2,
           y=max(SoilTemp_C_subset$Observed, SoilTemp_C_subset$APSIM, na.rm=T)*0.95,
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

Nfit <- lm(APSIM ~ Observed, data = N2O_ghaday_subset)
Nfit_coef <- coef(Nfit)
Nfit_r2 <- round(summary(Nfit)$r.squared,2)
# N_rmse_error <- N2O_ghaday$Observed-N2O_ghaday$APSIM
# N_rmse <- round(sqrt(mean(N_rmse_error^2,na.rm=TRUE)),2)
N_rmse <- round(hydroGOF::rmse(N2O_ghaday_subset$APSIM,N2O_ghaday_subset$Observed),2) 
N_mNSE <- round(hydroGOF::mNSE(N2O_ghaday_subset$APSIM,N2O_ghaday_subset$Observed),2)
N_pbias <- round(hydroGOF::pbias(N2O_ghaday_subset$APSIM,N2O_ghaday_subset$Observed),2)

gNG_121 <- N2O_ghaday_subset %>%
  ggplot(aes(x=Observed, y=APSIM,
             xmin=min(Observed, APSIM, na.rm=T), xmax=max(Observed, APSIM, na.rm=T),
             ymin=min(Observed, APSIM, na.rm=T), ymax=max(Observed, APSIM, na.rm=T))) +
  xlim(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T), 
       max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)) +
  ylim(min(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T), 
       max(N2O_ghaday[!is.na(N2O_ghaday$Observed),"Observed"], N2O_ghaday[!is.na(N2O_ghaday$Observed),"APSIM"], na.rm=T)) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Nfit_coef[1], slope=Nfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(N2O_ghaday_subset$Observed, N2O_ghaday_subset$APSIM, na.rm=T)*1.1,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Nfit_coef[2],4))~"x" ~+ ~.(round(Nfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(N2O_ghaday_subset$Observed, N2O_ghaday_subset$APSIM, na.rm=T)*1.1,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$APSIM, na.rm=T)*0.92,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Nfit_r2))) +
  annotate("text", # RMSE
           x=min(N2O_ghaday_subset$Observed, N2O_ghaday_subset$APSIM, na.rm=T)*1.1,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$APSIM, na.rm=T)*0.82,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(N_rmse))) +
  annotate("text", # mNSE
           x=(max(N2O_ghaday_subset$Observed, na.rm=T)+min(N2O_ghaday_subset$Observed, na.rm=T))/2,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$APSIM, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(N_mNSE))) +
  annotate("text", # pbias
           x=(max(N2O_ghaday_subset$Observed, na.rm=T)+min(N2O_ghaday_subset$Observed, na.rm=T))/2,
           y=max(N2O_ghaday_subset$Observed, N2O_ghaday_subset$APSIM, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(N_pbias))) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions (g ha" ^"-1"*" day"^"-1"*")"),
          paste0("Scenario: ",scenario_descriptor)) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gNG_121

ggsave(filename=paste0(results_path,"calib_Maize_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gMY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soybean_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gSY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Wheat_yield_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gWY_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gC_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Temp_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gT_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_Soil_Moist_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gM_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_N2O_comparison_1to1_",scenario_name,"_APSIM.jpg"),
       plot=gNG_121, width=6, height=6, dpi=300)


#**********************************************************************

# explain graphs ----------------------------------------------------------


## keep for easy visual explanation
if(clim_scenario_num==1 & mgmt_scenario_num %in% calib_mgmt_nums) {

  gN2O_expl_exp <- ggplot() +
    geom_line(data=APSIMM_V[APSIMM_V$year %in% 2010:2011,],
              aes(x=date, y=VolH2O_20cm/100, color=cbPalette12[2]), linewidth=1) +
    geom_line(data=APSIMM_V[APSIMM_V$year %in% 2010:2011,],
              aes(x=date, y=VolH2O_40cm/100, color=cbPalette12[3]), linewidth=1) +
    geom_line(data=APSIMM_V[APSIMM_V$year %in% 2010:2011,],
              aes(x=date, y=VolH2O_60cm/100, color=cbPalette12[4]), linewidth=1) +
    geom_line(data=APSIMM_V[APSIMM_V$year %in% 2010:2011,],
              aes(x=date, y=dul_20cm, color=cbPalette12[9]), linewidth=1) +
    geom_line(data=APSIMM_V[APSIMM_V$year %in% 2010:2011,],
              aes(x=date, y=sat_20cm, color=cbPalette12[7]), linewidth=1) +
    geom_line(data=APSIMNO3_ghaday[APSIMNO3_ghaday$year %in% 2010:2011,],
              aes(x=date,y=NO3_20cm/1000000, color=cbPalette12[6]), linewidth=1) +
    geom_line(data=APSIMGN_ghaday[year(APSIMGN_ghaday$date) %in% 2010:2011,],
              aes(x=date, y=N2OEmissions_ghaday/1000, color=cbPalette12[8]), linewidth=1) +
    ggtitle(bquote(.(site_name)~"N"["2"]*"O emissions drivers to 20 cm in APSIM"),
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
  
  
  gN2O_expl_exp
  
  ggsave(filename=paste0(results_path,"expl_N2O_exp_",scenario_name,"_APSIM.jpg"),
         plot=gN2O_expl_exp, width=6, height=6, dpi=300)
  
}


## N2O


### limit to years when observations were taken
obs_n2o_years <- unique(ObsGas$year)

SMoist_this_exp <- APSIMM_V[APSIMM_V$year %in% obs_n2o_years,]
SW20cm_fit_time_exp <- lm(VolH2O_20cm/100 ~ date, data = SMoist_this_exp) # convert to fraction
SW20cm_fit_coef_time_exp <- coef(SW20cm_fit_time_exp)
SW40cm_fit_time_exp <- lm(VolH2O_40cm/100 ~ date, data = SMoist_this_exp) # convert to fraction
SW40cm_fit_coef_time_exp <- coef(SW40cm_fit_time_exp)
SW60cm_fit_time_exp <- lm(VolH2O_60cm/100 ~ date, data = SMoist_this_exp) # convert to fraction
SW60cm_fit_coef_time_exp <- coef(SW60cm_fit_time_exp)
#
NO3_this_exp <- APSIMNO3_ghaday[APSIMNO3_ghaday$year %in% obs_n2o_years,]
NO3fit_time_exp <- lm(NO3_20cm/1000000 ~ date, data = NO3_this_exp)
NO3fit_coef_time_exp <- coef(NO3fit_time_exp)
#
N2O_this_exp <- APSIM_out[APSIM_out$year %in% obs_n2o_years,
                          c("date","year","N2O_bylayer_kgha(1)")] %>%
  mutate(N2O_20cm_ghaday = round(`N2O_bylayer_kgha(1)`*1000,2))
N2Ofit_time_exp <- lm(N2O_20cm_ghaday/1000 ~ date, data = N2O_this_exp)
N2Ofit_coef_time_exp <- coef(N2Ofit_time_exp)

## not used? but calculating anyway
SoilT_this_exp <- SoilTemp_C[year(SoilTemp_C$date) %in% obs_n2o_years,]
SoilT_fit_time_exp <- lm(APSIM ~ date, data = SoilT_this_exp) # convert to fraction
SoilTfit_coef_time_exp <- coef(SoilT_fit_time_exp)

gN2O_expl_exp <- ggplot() +
  geom_line(data=APSIMM_V[APSIMM_V$year %in% obs_n2o_years,],
            aes(x=date, y=VolH2O_20cm/100, color=cbPalette12[2]), linewidth=1) +
  geom_line(data=APSIMM_V[APSIMM_V$year %in% obs_n2o_years,],
            aes(x=date, y=VolH2O_40cm/100, color=cbPalette12[3]), linewidth=1) +
  geom_line(data=APSIMM_V[APSIMM_V$year %in% obs_n2o_years,],
            aes(x=date, y=VolH2O_60cm/100, color=cbPalette12[4]), linewidth=1) +
  geom_line(data=APSIMM_V[APSIMM_V$year %in% obs_n2o_years,],
            aes(x=date, y=dul_20cm, color=cbPalette12[9]), linewidth=1) +
  geom_line(data=APSIMM_V[APSIMM_V$year %in% obs_n2o_years,],
            aes(x=date, y=sat_20cm, color=cbPalette12[7]), linewidth=1) +
  geom_line(data=APSIMNO3_ghaday[APSIMNO3_ghaday$year %in% obs_n2o_years,],
            aes(x=date,y=NO3_20cm/1000000, color=cbPalette12[6]), linewidth=1) +
  geom_line(data=APSIMGN_ghaday[year(APSIMGN_ghaday$date) %in% obs_n2o_years,],
            aes(x=date, y=N2OEmissions_ghaday/1000, color=cbPalette12[8]), linewidth=1) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O emissions drivers to 20 cm in APSIM"),
          paste0("Scenario: ",scenario_descriptor_full)) +      
  ylab(expression('VWC, DUL, SAT, NO'[3]*' (dg ha' ^'-1'*' day' ^'-1'*'), N'[2]*'O  (cg ha' ^'-1'*' day'^'-1'*')')) +
  scale_color_manual(name=NULL,
                     labels=c("VWC: 0-20 cm","VWC: 20-40 cm","VWC: 40-60 cm",
                              "NO3 (dg/ha/day)","SAT","N2O (cg/ha/day)","DUL"), #),
                     values=cbPalette9[c(2,3,4,6,7,8,9)])+ #,7,6,8)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())


gN2O_expl_exp



## SOC


### limit to years when observations were taken
obs_soc_years <- pull(ObsC_Mgha[ObsC_Mgha$year!=land_conversion_year,],year)

SMoist_this_exp <- APSIMM_V[APSIMM_V$year %in% obs_soc_years,]
SW20cm_fit_time_exp <- lm(VolH2O_20cm/100 ~ date, data = SMoist_this_exp) # convert to fraction
SW20cm_fit_coef_time_exp <- coef(SW20cm_fit_time_exp)
SW40cm_fit_time_exp <- lm(VolH2O_40cm/100 ~ date, data = SMoist_this_exp) # convert to fraction
SW40cm_fit_coef_time_exp <- coef(SW40cm_fit_time_exp)
SW60cm_fit_time_exp <- lm(VolH2O_60cm/100 ~ date, data = SMoist_this_exp) # convert to fraction
SW60cm_fit_coef_time_exp <- coef(SW60cm_fit_time_exp)

SoilT_this_exp <- SoilTemp_C[year(SoilTemp_C$date) %in% obs_soc_years,]
SoilT_fit_time_exp <- lm(APSIM ~ date, data = SoilT_this_exp) # convert to fraction
SoilTfit_coef_time_exp <- coef(SoilT_fit_time_exp)

SoilCN_this_exp <- APSIMSoilCN_kgha[APSIMSoilCN_kgha$year %in% obs_soc_years,]
BC20cm_fit_time_exp <- lm(biomc_20cm ~ date, data = SoilCN_this_exp)
BC20cm_fit_coef_time_exp <- coef(BC20cm_fit_time_exp)
BC40cm_fit_time_exp <- lm(biomc_40cm ~ date, data = SoilCN_this_exp)
BC40cm_fit_coef_time_exp <- coef(BC40cm_fit_time_exp)
BC60cm_fit_time_exp <- lm(biomc_60cm ~ date, data = SoilCN_this_exp)
BC60cm_fit_coef_time_exp <- coef(BC60cm_fit_time_exp)
BN20cm_fit_time_exp <- lm(biomn_20cm ~ date, data = SoilCN_this_exp)
BN20cm_fit_coef_time_exp <- coef(BN20cm_fit_time_exp)
BN40cm_fit_time_exp <- lm(biomn_40cm ~ date, data = SoilCN_this_exp)
BN40cm_fit_coef_time_exp <- coef(BN40cm_fit_time_exp)
BN60cm_fit_time_exp <- lm(biomn_60cm ~ date, data = SoilCN_this_exp)
BN60cm_fit_coef_time_exp <- coef(BN60cm_fit_time_exp)

HC20cm_fit_time_exp <- lm(humc_20cm ~ date, data = SoilCN_this_exp)
HC20cm_fit_coef_time_exp <- coef(HC20cm_fit_time_exp)
HC40cm_fit_time_exp <- lm(humc_40cm ~ date, data = SoilCN_this_exp)
HC40cm_fit_coef_time_exp <- coef(HC40cm_fit_time_exp)
HC60cm_fit_time_exp <- lm(humc_60cm ~ date, data = SoilCN_this_exp)
HC60cm_fit_coef_time_exp <- coef(HC60cm_fit_time_exp)
HN20cm_fit_time_exp <- lm(humn_20cm ~ date, data = SoilCN_this_exp)
HN20cm_fit_coef_time_exp <- coef(HN20cm_fit_time_exp)
HN40cm_fit_time_exp <- lm(humn_40cm ~ date, data = SoilCN_this_exp)
HN40cm_fit_coef_time_exp <- coef(HN40cm_fit_time_exp)
HN60cm_fit_time_exp <- lm(humn_60cm ~ date, data = SoilCN_this_exp)
HN60cm_fit_coef_time_exp <- coef(HN60cm_fit_time_exp)

soilT_transform_factor <- 100

cols <- c("BiomC" = BiomC_20cm_color, "BiomN" = BiomN_20cm_color, "HumC" = HumC_20cm_color,
          "HumN" = HumN_20cm_color, "SoilT" = SoilT_color, "TotalC" = TotalSOC_20cm_color,
          "VWC" = SW_20cm_color)

gBioC_expl_exp <- ggplot() +
  geom_line(data=APSIMM_V[APSIMM_V$year %in% obs_soc_years,],
            aes(x=date, y=VolH2O_20cm/100, color="VWC"), linewidth=1) +
  geom_line(data=APSIMSoilCN_kgha[APSIMSoilCN_kgha$year %in% obs_soc_years,],
            aes(x=date, y=biomc_20cm/1000, color="BiomC"), linewidth=1) +
  geom_line(data=APSIMSoilCN_kgha[APSIMSoilCN_kgha$year %in% obs_soc_years,],
            aes(x=date, y=biomn_20cm/1000, color="BiomN"), linewidth=1) +
  geom_line(data=APSIMSoilCN_kgha[APSIMSoilCN_kgha$year %in% obs_soc_years,],
            aes(x=date, y=humc_20cm/10000, color="HumC"), linewidth=1) +
  geom_line(data=APSIMSoilCN_kgha[APSIMSoilCN_kgha$year %in% obs_soc_years,],
            aes(x=date, y=humn_20cm/10000, color="HumN"), linewidth=1) +
  geom_line(data=APSIMSoilCN_kgha[APSIMSoilCN_kgha$year %in% obs_soc_years,],
            aes(x=date, y=TotalSOC_20cm_Mgha/10, color="TotalC"), linewidth=1) +
  geom_abline(intercept=SoilTfit_coef_time_exp[1], slope=SoilTfit_coef_time_exp[2], colour=SoilT_color) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O emissions drivers to 20 cm in APSIM"),
          paste0("Scenario: ",scenario_descriptor_full)) +      
  ylab(expression('VWC, Bio C (g ha' ^'-1'*'), Bio N (g ha' ^'-1'*'), Hum C (), Hum N ()')) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ .x * soilT_transform_factor,
                        name = expression('Soil Temperature ('^o*'C)'))
  ) +
  scale_color_manual(name=NULL,
                     values=cols,
                     labels=c("Biomass C: 0-20 cm","Biomass N: 0-20 cm",
                              "Humic C: 0-20 cm","Humic N: 0-20 cm","Soil Temp: 0-20 cm",
                              "Total SOC: 0-20 cm","VWC: 0-20 cm")
                     ) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())


gBioC_expl_exp

ggsave(filename=paste0(results_path,"expl_SOC_0to20cm_exp_",scenario_name,"_APSIM.jpg"),
       plot=gN2O_expl_exp, width=9, height=6, dpi=300)


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
                       NA, NA, NA, NA, NA, NA, # CH4
                       NA,
                       NA, NA, NA, NA, # M Bio
                       NA,
                       NA, NA, NA, NA, # Cotton
                       NA,
                       NA, NA, NA, NA, # Sorghum
                       NA,
                       NA, NA, NA, NA, NA, # M,S,W,C,S cultivars
                       MYfit_coef_time[2], MYfit_coef_time[1], MYfit_r2_time, MY_rmse_time,
                       SYfit_coef_time[2], SYfit_coef_time[1], SYfit_r2_time, SY_rmse_time,
                       WYfit_coef_time[2], WYfit_coef_time[1], WYfit_r2_time, WY_rmse_time,
                       Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, C_rmse_time,
                       Cfit_coef_time_noout[2], Cfit_coef_time_noout[1], Cfit_r2_time_noout, C_rmse_time_noout,
                       Tfit_coef_time[2], Tfit_coef_time[1], Tfit_r2_time, T_rmse_time,
                       Mfit_coef_time[2], Mfit_coef_time[1], Mfit_r2_time, M_rmse_time,
                       Nfit_coef_time[2], Nfit_coef_time[1], Nfit_r2_time, N_rmse_time,
                       NA, NA, NA, NA, # methane
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
   MYfit_coef_time, MYfit_r2_time, MY_rmse_time,
   SYfit_coef_time, SYfit_r2_time, SY_rmse_time,
   WYfit_coef_time, WYfit_r2_time, WY_rmse_time,
   Cfit_coef_time, Cfit_r2_time, C_rmse_time,
   Cfit_coef_time_noout, Cfit_r2_time_noout, C_rmse_time_noout,
   Tfit_coef_time, Tfit_r2_time, T_rmse_time,
   Mfit_coef_time, Mfit_r2_time, M_rmse_time,
   Nfit_coef_time, Nfit_r2_time, N_rmse_time)


}) # end suppressMessages