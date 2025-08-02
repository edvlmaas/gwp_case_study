#######################################
# Script: 9_Results_Millennial-calibration_LRF.R
# Author: Ellen Maas
# Date: 7/22/2022
# Description: Runs calibration graphs for the Millennial simulation at
# LRF, TX. Also logs a selection of results for final processing
# in the "10_" series of scripts.
#######################################

suppressMessages({
  
print(paste0("Starting 9_Results_Millennial-calibration_",site_name,".R"))

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)


#*************************************************************

# Temporal graphs ---------------------------------------------------------

## carbon

#### full 1 m depth, full time span
gC1 <- Cstock_Mgha_piv[Cstock_Mgha_piv$year <= experiment_end_year,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon (1 m depth): Scenario ",scenario_name)) +
  scale_color_manual(labels=c("Millennial","Observed-10cm"),
                     values=c(Millennial_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC1 

#### 10 cm depth, full time span
Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gC2 <- Cstock_Mgha_piv_10cm[Cstock_Mgha_piv_10cm$year <= experiment_end_year,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2) + # Width of the error bars
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon (10 cm depth): Scenario ",scenario_name)) +
  scale_color_manual(labels=c("Millennial","Observed-10cm"),
                     values=c(Millennial_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC2 


#### 10 cm depth, base through experimental

Cfit_MillC <- coef(lm(Millennial ~ year, data = Cstock_Mgha[Cstock_Mgha$year %in% 
                                                         experiment_start_year:end_exp_period_year,]))#experiment_year_range,]))
Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gC3 <- Cstock_Mgha_piv_10cm[Cstock_Mgha_piv_10cm$year <= experiment_end_year,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2) + # Width of the error bars
  geom_abline(intercept=Cfit_MillC[1], slope=Cfit_MillC[2], color="orange") +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon: Scenario ",scenario_name)) +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=c(Millennial_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC3


#### 10 cm depth, experimental period

SOC_this_piv <- Cstock_Mgha_piv_10cm[Cstock_Mgha_piv_10cm$year %in% experiment_year_range,]

## calculate stats with outliers
SOC_this <- Cstock_Mgha_10cm[Cstock_Mgha_10cm$year %in% experiment_year_range,]
Cfit_time <- lm(Millennial ~ year, data = SOC_this)
Cfit_coef_time <- coef(Cfit_time)
Cfit_r2_time <- round(summary(Cfit_time)$r.squared,2)
C_rmse_error_time <- SOC_this$Observed-SOC_this$Millennial
C_rmse_time <- round(sqrt(mean(C_rmse_error_time^2,na.rm=TRUE)),2)

## include outliers in trend line here
ObsCfit_all <- coef(lm(Observed ~ year, data = Cstock_Mgha_10cm[Cstock_Mgha_10cm$year >= experiment_start_year,]))


gC4 <- SOC_this_piv[SOC_this_piv$year %in% experiment_year_range,] %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color="black") +
  geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                width=.2) + # Width of the error bars
  geom_abline(intercept=Cfit_coef_time[1], slope=Cfit_coef_time[2], color=cbPalette9[6]) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon: Scenario ",scenario_name)) +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=c(Millennial_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC4


## microbial biomass

MB_this_piv <- mbio_gm2_piv[mbio_gm2_piv$year %in% experiment_year_range,]

## calculate stats 
MB_this <- mbio_gm2[mbio_gm2$year %in% experiment_year_range,]
MBfit_time <- lm(Millennial ~ year, data = MB_this)
MBfit_coef_time <- coef(MBfit_time)
MBfit_r2_time <- round(summary(MBfit_time)$r.squared,2)
MB_rmse_error_time <- MB_this$Observed-MB_this$Millennial
MB_rmse_time <- round(sqrt(mean(MB_rmse_error_time^2,na.rm=TRUE)),2)

# look at only the points when observations were made
gMB1 <- MB_this_piv %>%
  ggplot(aes(x=date, y=MB_val, color=source)) +
  geom_point() +
  geom_errorbar(aes(ymin=MB_val-Obs_sd, ymax=MB_val+Obs_sd),
                width=50) + # Width of the whiskers
  xlab("Year") +
  ylab(expression('Microbial biomass (g C m' ^-2*')')) +
  annotate("text", # R^2
           x=min(mbio_gm2$date),
           y=max(mbio_gm2$Observed, mbio_gm2$Millennial, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(MBfit_r2_time))) +
  annotate("text", # RMSE
           x=min(mbio_gm2$date),
           y=max(mbio_gm2$Observed, mbio_gm2$Millennial, na.rm=T)*0.92,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(MB_rmse_time))) +
  ggtitle(paste(site_name,"Microbial Biomass"),
          paste0("Scenario: ",scenario_descriptor)) +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=cbPalette9[c(6,1)]) + 
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_blank())

gMB1

 

# look at all Millennial data over whole model run
gMB2 <- mbio_gm2_all[,c("date","Millennial")] %>%
ggplot(aes(x=date, y=Millennial, color=cbPalette9[6])) +
  geom_line() +
  geom_point(data=mbio_gm2[,c("date","Observed")],
             aes(x=date,y=Observed, color=cbPalette9[8])) +
  xlab("Year") +
  ylab(expression('Microbial biomass (g C m' ^-2*')')) +
  ggtitle(paste(site_name,"Microbial Biomass: Scenario ",scenario_name)) +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=c(Millennial_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_blank())

gMB2

# look at all Millennial data within date range when observations were made
gMB3 <- mbio_gm2_all[mbio_gm2_all$date %in% min(mbio_gm2$date):max(mbio_gm2$date)
                     ,c("date","Millennial")] %>%
ggplot(aes(x=date, y=Millennial, color=cbPalette9[6])) +
  geom_line() +
  geom_point(data=mbio_gm2_all[mbio_gm2_all$date %in% min(mbio_gm2$date):max(mbio_gm2$date),
                           c("date","Observed")],
             aes(x=date,y=Observed, color=cbPalette9[8])) +
  xlab("Year") +
  ylab(expression('Microbial biomass (g C m' ^-2*')')) +
  ggtitle(paste(site_name,"Microbial Biomass: Scenario ",scenario_name)) +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=c(Millennial_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_blank())

gMB3


ggsave(filename=paste0(results_path,"calib_SOC_comparison_1m_exp_",scenario_name,"_Millennial.jpg"),plot=gC1,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_10cm_exp_",scenario_name,"_Millennial.jpg"),plot=gC2,
       width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",scenario_name,"_Millennial.jpg"),plot=gC4,
       width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_MBio_comparison_points_exp_",scenario_name,"_Millennial.jpg"),
       plot=gMB1, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_MBio_comparison_allMill_exp_",scenario_name,"_Millennial.jpg"),
       plot=gMB2, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_MBio_comparison_allexp_exp_",scenario_name,"_Millennial.jpg"),
       plot=gMB3, width=9, height=6, dpi=300)

#*************************************************************

# 1:1 graphs --------------------------------------------------------------


##  SOC-full profile
  Cfit <- lm(Millennial ~ Observed, data = Cstock_Mgha)
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$Millennial
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)

gC_121 <- Cstock_Mgha %>%
  ggplot(aes(x=Observed, y=Millennial,
             xmin=min(Observed, Millennial, na.rm=T), xmax=max(Observed, Millennial, na.rm=T),
             ymin=min(Observed, Millennial, na.rm=T), ymax=max(Observed, Millennial, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  ggtitle("SOC stock") +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

gC_121

##  SOC-10 cm

  Cfit <- lm(Millennial ~ Observed, data = Cstock_Mgha_10cm)
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha_10cm$Observed-Cstock_Mgha_10cm$Millennial
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)

gC_121 <- Cstock_Mgha_10cm %>%
  ggplot(aes(x=Observed, y=Millennial,
             xmin=min(Observed, Millennial, na.rm=T), xmax=max(Observed, Millennial, na.rm=T),
             ymin=min(Observed, Millennial, na.rm=T), ymax=max(Observed, Millennial, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha_10cm$Observed, Cstock_Mgha_10cm$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha_10cm$Observed, Cstock_Mgha_10cm$Millennial, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha_10cm$Observed, Cstock_Mgha_10cm$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha_10cm$Observed, Cstock_Mgha_10cm$Millennial, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha_10cm$Observed, Cstock_Mgha_10cm$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha_10cm$Observed, Cstock_Mgha_10cm$Millennial, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  ggtitle("SOC stock") +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

gC_121

# microbial biomass
MBfit <- lm(Millennial ~ Observed, data = mbio_gm2)
MBfit_coef <- coef(MBfit)
MBfit_r2 <- round(summary(MBfit)$r.squared,2)

MB_rmse_error <- mbio_gm2$Observed-mbio_gm2$Millennial
MB_rmse <- round(sqrt(mean(MB_rmse_error^2,na.rm=TRUE)),2)

gMB_121 <- mbio_gm2 %>%
  ggplot(aes(x=Observed, y=Millennial,
             xmin=min(Observed, Millennial, na.rm=T), xmax=max(Observed, Millennial, na.rm=T),
             ymin=min(Observed, Millennial, na.rm=T), ymax=max(Observed, Millennial, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=MBfit_coef[1], slope=MBfit_coef[2], color="blue") +
  ggtitle(paste0('Microbial biomass: Scenario ',scenario_name)) +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=c(Millennial_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.key = element_blank())

gMB_121

ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",
                       scenario_name,"_Millennial.jpg"),
       plot=gC_121, width=6, height=6, dpi=300)
ggsave(filename=paste0(results_path,"calib_MBio_comparison_1to1_",
                       scenario_name,"_Millennial.jpg"),
       plot=gMB_121, width=6, height=6, dpi=300)

#**********************************************************************

# Log results -------------------------------------------------------------


# add this run's results to model log file and file collecting all final
# model runs
calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                       clim_scenario_num,mgmt_scenario_num, scenario_name,
                       scenario_abbrev,
                       NA, NA, NA, NA, # Maize 1 to 1
                       NA,
                       NA, NA, NA, NA, # Soybean 1 to 1
                       NA,
                       NA, NA, NA, NA, # Wheat 1 to 1
                       NA,
                       Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse,
                       SOC_obsmod_diff_Mgha,NA,
                       NA, NA, NA, NA, # Temp 1 to 1
                       NA,
                       NA, NA, NA, NA, # Moist 1 to 1
                       NA,
                       NA, NA, NA, NA, # N2O 1 to 1
                       NA,
                       NA, NA, NA, NA, # CH4 1 to 1
                       NA,
                       MBfit_coef[2], MBfit_coef[1], MBfit_r2, MB_rmse, # M Bio 1 to 1
                       MBio_obsmod_diff_Mgha,
                       NA, NA, NA, NA, # Cotton 1 to 1
                       NA,
                       NA, NA, NA, NA, # Sorghum 1 to 1
                       NA,
                       NA, NA, NA, # maize, soybean, wheat cultivars
                       NA, NA, # cotton, sorghum cultivars
                       NA, NA, NA, NA, # Maize time series
                       NA, NA, NA, NA, # Soybean time series
                       NA, NA, NA, NA, # Wheat time series
                       Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, NA,
                       NA, NA, NA, NA, # SOC w/o outliers
                       NA, NA, NA, NA,
                       NA, NA, NA, NA, # Moisture time series
                       NA, NA, NA, NA, # N2O time series
                       NA, NA, NA, NA, # CH4 time series
                       MBfit_coef_time[2], MBfit_coef_time[1], MBfit_r2_time, NA, # M Bio time series
                       NA, NA, NA, NA,
                       NA, NA, NA, NA # Sorghum time series
                       )


source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_log_tab,model_name,scenario_name)

rm(Cfit_MillC,Cfit_Obs,SOC_this_piv,SOC_this,Cfit_time,Cfit_coef_time,Cfit_r2_time,
   C_rmse_error_time,C_rmse_time,ObsCfit_all,
   MB_this_piv,MB_this,MBfit_time,MBfit_coef_time,MBfit_r2_time,MB_rmse_error_time,
   MB_rmse_time,
   Cfit,Cfit_coef,Cfit_r2,C_rmse_error,C_rmse, 
   MBfit,MBfit_coef,MBfit_r2,MB_rmse_error,MB_rmse,
   gC1,gC2,gC3,gC4,gMB1,gMB2,gMB3,gC_121,gMB_121)

}) # end suppressMessages