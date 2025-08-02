#######################################
# Script: 9_Results_RothC-calibration_LRF.R
# Author: Ellen Maas
# Date: Aug 30, 2022
# Description: Runs calibration graphs for the RothC simulation at
# LRF, TX. Also logs a selection of results for final processing
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
  
  SOC_this <- Cstock_Mgha[Cstock_Mgha$year %in% experiment_year_range,]
  Cfit_time <- lm(RothC ~ year, data = SOC_this)
  Cfit_coef_time <- coef(Cfit_time)
  Cfit_r2_time <- round(summary(Cfit_time)$r.squared,2)
  C_rmse_error_time <- SOC_this$Observed-SOC_this$RothC
  C_rmse_time <- round(sqrt(mean(C_rmse_error_time^2,na.rm=TRUE)),2)

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
                       values=c(Observed_color,RothC_color)) +
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
                       values=c(Observed_color,RothC_color)) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gCh
  


ggsave(filename=paste0(results_path,"calib_SOC_comparison_his_",
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
                     values=c(Observed_color,RothC_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 



#**********************************************************************

# 1:1 graphs --------------------------------------------------------------

##  SOC
if(mgmt_scenario_grp==3) {
  Cfit <- lm(RothC ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                    Cstock_Mgha$year!=1850,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$RothC
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
} else {
  Cfit <- lm(RothC ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1850,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$RothC
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
}

gC_121 <- Cstock_Mgha %>%
  ggplot(aes(x=Observed, y=RothC,
             xmin=min(Observed, RothC, na.rm=T), xmax=max(Observed, RothC, na.rm=T),
             ymin=min(Observed, RothC, na.rm=T), ymax=max(Observed, RothC, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
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
                         Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse, NA, NA,
                         SOC_obsmod_diff_Mgha,NA,
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
                         NA, NA, NA, NA,
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

rm(Cfith_RothC,Cfith_Obs,SOC_this_piv,SOC_this,Cfit_time,Cfit_coef_time,Cfit_r2_time,
   C_rmse_error_time,C_rmse_time,ObsCfit_all,Cfit,Cfit_coef,Cfit_r2,C_rmse_error,
   C_rmse,
   gC,gCh,gC_121)

}) # end suppressMessages
