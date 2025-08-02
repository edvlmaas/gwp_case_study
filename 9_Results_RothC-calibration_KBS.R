#######################################
# Script: 9_Results_RothC-calibration_KBS.R
# Author: Ellen Maas
# Date: Aug 30, 2022
# Description: Runs calibration graphs for the RothC simulation at
# KBS, MI. Also logs a selection of results for final processing
# in the "10_" series of scripts.
#######################################

suppressMessages({
  
print(paste0("Starting 9_Results_RothC-calibration_",site_name,".R"))

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)


#**********************************************************************

# Temporal graphs ---------------------------------------------------------


## SOC experimental period

  SOC_this_piv <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range,]
  
  ## calculate stats with outliers
  SOC_this <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,]
  Cfit_time <- lm(RothC ~ year, data = SOC_this)
  Cfit_coef_time <- coef(Cfit_time)
  Cfit_r2_time <- round(summary(Cfit_time)$r.squared,2)
  C_rmse_error_time <- SOC_this$Observed-SOC_this$RothC
  C_rmse_time <- round(sqrt(mean(C_rmse_error_time^2,na.rm=TRUE)),2)
  ## calculate stats without outliers
  SOC_this_noout <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range &
                                  !(Cstock_Mgha$Observed %in% ObsC_outliers),]
  Cfit_time_noout <- lm(RothC ~ year, data = SOC_this_noout)
  Cfit_coef_time_noout <- coef(Cfit_time_noout)
  Cfit_r2_time_noout <- round(summary(Cfit_time_noout)$r.squared,2)
  C_rmse_error_time_noout <- SOC_this_noout$Observed-SOC_this_noout$RothC
  C_rmse_time_noout <- round(sqrt(mean(C_rmse_error_time_noout^2,na.rm=TRUE)),2)
  
  ## include outliers in trend line here
  ObsCfit_all <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
  
  gC <- SOC_this_piv %>%
    ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
    geom_point() +
    geom_abline(intercept=ObsCfit_all[1], slope=ObsCfit_all[2], color="black") +
    geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                  width=.2) + # Width of the error bars
    xlab("Year") +
    ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
    #ylim(3,12) +
    ggtitle(paste(site_name,"Soil Organic Carbon"),
            paste0("Scenario: ",scenario_descriptor)) +
    geom_abline(intercept=Cfit_coef_time[1], slope=Cfit_coef_time[2], color=cbPalette9[3]) +
    scale_color_manual(labels=c("Observed","RothC"),
                       values=cbPalette9[c(1,3)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gC 
  
  
  ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",
                         clim_scenario_num,"_",mgmt_scenario_num,"_RothC.jpg"),
         plot=gC,
         width=6, height=6, dpi=300)
  
  ## SOC with spin-up
  
  Cfith_RothC <- coef(lm(RothC ~ year, data = Cstock_Mgha[Cstock_Mgha$year %in% experiment_start_year:end_exp_period_year,]))#experiment_year_range,]))
  Cfith_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
  
  gCh <- Cstock_Mgha_piv %>%#experiment_year_range,] %>%
    ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
    geom_point() +
    geom_abline(intercept=Cfith_Obs[1], slope=Cfith_Obs[2], color="black") +
    geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                  width=.2) + # Width of the error bars
    xlab("Year") +
    ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
    #  ylim(0,12) +
    ggtitle(paste(site_name,"Soil Organic Carbon"),
            paste0("Scenario: ",scenario_descriptor)) +
    scale_color_manual(labels=c("Observed","RothC"),
                       values=cbPalette9[c(1,3)]) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gCh
  


ggsave(filename=paste0(results_path,"calib_SOC_comparison_base_",
                              clim_scenario_num,"_",mgmt_scenario_num,"_RothC.jpg"),
       plot=gCh,
       width=9, height=6, dpi=300)



## experimental period


gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon: Scenario ",clim_scenario_num,"_",mgmt_scenario_num)) +
  scale_color_manual(labels=c("RothC","Observed"),
                     values=cbPalette9[c(3,1)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 



#**********************************************************************

# 1:1 graphs --------------------------------------------------------------

##  SOC
Cstock_Mgha_subset <- Cstock_Mgha[Cstock_Mgha$year>=experiment_start_date &
                                    !is.na(Cstock_Mgha$Observed),]

if(mgmt_scenario_grp==3) {
  Cfit <- lm(RothC ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                    Cstock_Mgha$year!=1850,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  # C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$RothC
  # C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
  C_rmse <- round(hydroGOF::rmse(Cstock_Mgha_subset$RothC,Cstock_Mgha_subset$Observed),2) 
  C_mNSE <- round(hydroGOF::mNSE(Cstock_Mgha_subset$RothC,Cstock_Mgha_subset$Observed),2)
  C_pbias <- round(hydroGOF::pbias(Cstock_Mgha_subset$RothC,Cstock_Mgha_subset$Observed),2)
  
} else {
  Cfit <- lm(RothC ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1850,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  # C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$RothC
  # C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
  C_rmse <- round(hydroGOF::rmse(Cstock_Mgha_subset$RothC,Cstock_Mgha_subset$Observed),2) 
  C_mNSE <- round(hydroGOF::mNSE(Cstock_Mgha_subset$RothC,Cstock_Mgha_subset$Observed),2)
  C_pbias <- round(hydroGOF::pbias(Cstock_Mgha_subset$RothC,Cstock_Mgha_subset$Observed),2)
}

gC_121 <- Cstock_Mgha %>%
  ggplot(aes(x=Observed, y=RothC,
             xmin=min(Observed, RothC, na.rm=T), xmax=max(Observed, RothC, na.rm=T),
             ymin=min(Observed, RothC, na.rm=T), ymax=max(Observed, RothC, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$RothC, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$RothC, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$RothC, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  annotate("text", # mNSE
           x=(max(Cstock_Mgha_subset$Observed, na.rm=T)+min(Cstock_Mgha_subset$Observed, na.rm=T))/2,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$RothC, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("mNSE =" ~.(C_mNSE))) +
  annotate("text", # pbias
           x=(max(Cstock_Mgha_subset$Observed, na.rm=T)+min(Cstock_Mgha_subset$Observed, na.rm=T))/2,
           y=max(Cstock_Mgha_subset$Observed, Cstock_Mgha_subset$RothC, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote("pbias =" ~.(C_pbias))) +
  ggtitle("SOC stock") +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

gC_121

ggsave(filename=paste0(results_path,"calib_SOC_comparison_1to1_",scenario_name,"_RothC.jpg"),
       plot=gC_121, width=6, height=6, dpi=300)

#**********************************************************************

# Log results -------------------------------------------------------------


# add this run's results to model log file and file collecting all final
# model runs
calib_log_tab <- cbind(as.character(Sys.time()),model_name,
                       clim_scenario_num,mgmt_scenario_num, scenario_name,
                       scenario_abbrev,
                       NA, NA, NA, NA, NA, NA, # Maize
                       NA,
                       NA, NA, NA, NA, NA, NA, # Soybean
                       NA,
                       NA, NA, NA, NA, NA, NA, # Wheat
                       NA,
                       Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse, C_mNSE, C_pbias,
                       SOC_obsmod_diff_Mgha,SOC_obsmod_diff_Mgha_nooutliers,
                       NA, NA, NA, NA, NA, NA, # Temp
                       NA,
                       NA, NA, NA, NA, NA, NA, # Moist
                       NA,
                       NA, NA, NA, NA, NA, NA, # N2O
                       NA,
                       NA, NA, NA, NA, NA, NA, # CH4
                       NA,
                       NA, NA, NA, NA, # M Bio
                       NA,
                       NA, NA, NA, NA, # Cotton
                       NA,
                       NA, NA, NA, NA, # Sorghum
                       NA,
                       NA, NA, NA, # maize, soybean, wheat cultivars
                       NA, NA, # cotton, sorghum cultivars
                       NA, NA, NA, NA, # Maize time series
                       NA, NA, NA, NA, # Soybean time series
                       NA, NA, NA, NA, # Wheat time series
                       Cfit_coef_time[2], Cfit_coef_time[1], Cfit_r2_time, C_rmse_time, # SOC w/ outliers
                       Cfit_coef_time_noout[2], Cfit_coef_time_noout[1], Cfit_r2_time_noout, C_rmse_time_noout,
                       NA, NA, NA, NA, # Temp
                       NA, NA, NA, NA, # Moist
                       NA, NA, NA, NA, # n2o
                       NA, NA, NA, NA, # methane
                       NA, NA, NA, NA, # microbio
                       NA, NA, NA, NA, # cotton
                       NA, NA, NA, NA # sorghum
                       )

source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_log_tab,model_name,scenario_name)

rm(calib_log_tab,
   Cfit_coef, Cfit_r2, C_rmse,
   SOC_obsmod_diff_Mgha,SOC_obsmod_diff_Mgha_nooutliers,
   Cfit_coef_time, Cfit_r2_time, C_rmse_time, # SOC w/ outliers
   Cfit_coef_time_noout, Cfit_r2_time_noout, C_rmse_time_noout)

}) # end suppressMessages
