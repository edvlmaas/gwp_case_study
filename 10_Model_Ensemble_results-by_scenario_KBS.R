#######################################
# File: 10_Model_Ensemble_Results-by_scenario_KBS.R
# Author: Ellen Maas
# Date: 8/30/2022
# Description: Generates multi-model result graphs for
# each scenario separately and writes some CSVs with 
# summarized/analyzed data for use by other "10_" series 
# scripts.
#######################################

suppressMessages({
  
  print(paste0("Starting 10_Model_Ensemble_Results-by_scenario_",site_name,".R"))
  
  library(apsimx)
  library(readxl)
  library(magrittr)
  library(lubridate)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  
  source("p_Edit_calib_data_file.R")
  
  # read in calibration stats ----------------------
  calib_summary_raw <- read.csv(paste0(results_path,"Calibration_summary.csv"))
  calib_summary_df <- calib_summary_raw[calib_summary_raw$Scenario_Name==scenario_name]
  
  
  if(mgmt_scenario_grp!=6) {
    
    # merge observed and modeled data
    ## use ens_ (ensemble) prefix to distinguish these from the "MaizeYld_Mgha" etc.
    ## files in each model's Results files.
    
    ## maize -------------
    
    ens_MaizeYld_Mgha <- merge(merge(ObsYield[ObsYield$crop=="Maize",c("year","mean_yield","sd_yield")],
                                     APSIMY_Mgha[APSIMY_Mgha$MaizeYield_Mgha != 0,
                                                 c("year","MaizeYield_Mgha")],
                                     by="year",
                                     all=TRUE),
                               DayY_Mgha[DayY_Mgha$crop=="Maize",c("year","yield")],
                               by="year",
                               all=TRUE) %>%
      mutate(crop="Maize",
             treatment_scen=scenario_descriptor)
    colnames(ens_MaizeYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent",
                                     "crop","treatment_scen")
    
    ens_MaizeYld_Mgha_piv <- pivot_longer(ens_MaizeYld_Mgha, c(-year,-Obs_sd,
                                                               -crop,-treatment_scen),
                                          names_to = "Model",
                                          values_to = "yield_val")
    
    ens_MaizeYld_Mgha_piv <- ens_MaizeYld_Mgha_piv %>%
      mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))
    
    ## soybeans -------------
    ens_SoyYld_Mgha <- merge(merge(ObsYield[ObsYield$crop=="Soybean",c("year","mean_yield","sd_yield")],
                                   APSIMY_Mgha[APSIMY_Mgha$SoyYield_Mgha != 0,
                                               c("year","SoyYield_Mgha")],
                                   by="year",
                                   all=TRUE),
                             DayY_Mgha[DayY_Mgha$crop=="Soybean",c("year","yield")],
                             by="year",
                             all=TRUE) %>%
      mutate(crop="Soybean",
             treatment_scen=scenario_descriptor)
    colnames(ens_SoyYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent",
                                   "crop","treatment_scen")
    
    ens_SoyYld_Mgha_piv <- pivot_longer(ens_SoyYld_Mgha, c(-year,-Obs_sd,
                                                           -crop,-treatment_scen),
                                        names_to = "Model",
                                        values_to = "yield_val")
    
    ens_SoyYld_Mgha_piv <- ens_SoyYld_Mgha_piv %>%
      mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))
    
    ## wheat --------------
    ens_WheatYld_Mgha <- merge(merge(ObsYield[ObsYield$crop=="Wheat",c("year","mean_yield","sd_yield")],
                                     APSIMY_Mgha[APSIMY_Mgha$WheatYield_Mgha != 0,
                                                 c("year","WheatYield_Mgha")],
                                     by="year",
                                     all=TRUE),
                               DayY_Mgha[DayY_Mgha$crop=="Wheat",c("year","yield")],
                               by="year",
                               all=TRUE) %>%
      mutate(crop="Wheat",
             treatment_scen=scenario_descriptor)
    colnames(ens_WheatYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent",
                                     "crop","treatment_scen")
    
    ens_WheatYld_Mgha_piv <- pivot_longer(ens_WheatYld_Mgha, c(-year,-Obs_sd,
                                                               -crop,-treatment_scen),
                                          names_to = "Model",
                                          values_to = "yield_val")
    
    ens_WheatYld_Mgha_piv <- ens_WheatYld_Mgha_piv %>%
      mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))
    
    ## c stock ---------------
    
    ens_Cstock_Mgha <- merge(merge(merge(ObsC_Mgha[,c("year","cstock","sd_cstock")],
                                         APSIMC_Mgha,
                                         by="year",
                                         all=TRUE),
                                   DayC_Mgha[,c("year","base")],
                                   by="year",
                                   all=TRUE),
                             RothCC_Mgha[,c("year","ModC")],
                             by="year",
                             all=TRUE) %>%
      mutate(treatment_scen=scenario_descriptor)
    
    colnames(ens_Cstock_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent",
                                   "RothC","treatment_scen")
    
    ens_Cstock_Mgha_piv <-  pivot_longer(ens_Cstock_Mgha, c(-year,-Obs_sd,
                                                            -treatment_scen),
                                         names_to = "Model",
                                         values_to = "C_val")
    
    ### remove sd's from models, since it's only for observations
    ens_Cstock_Mgha_piv_adj <- ens_Cstock_Mgha_piv %>%
      mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))
    
    ## soil moisture ---------------
    
    ens_VM <- merge(merge(ObsVSM[,c("date","mean_VSM")],
                          APSIMM_V[,c("date","VolH2O_20cm")],
                          by="date"),
                    DayM_V[,c("date","mean_20cm")],
                    by="date")
    colnames(ens_VM) <- c("date","Observed","APSIM","Daycent")
    
    ens_VM_piv <- pivot_longer(ens_VM, c(-date),
                               names_to = "Source",
                               values_to = "vm_val")
    
    ## N2O ---------------
    
    ens_N2O_ghaday <- merge(merge(ObsGas[,c("date","N2O_N")],
                                  APSIMGN_ghaday[,c("date","N2OEmissions_ghaday")],
                                  by="date",
                                  all=TRUE),
                            DayGN_ghaday[,c("date","N2O_gNhad")],
                            by="date",
                            all=TRUE)
    colnames(ens_N2O_ghaday) <- c("date","Observed","APSIM","Daycent")
    
    ens_N2O_ghaday_piv <- pivot_longer(ens_N2O_ghaday, c(-date,),
                                       names_to = "Model",
                                       values_to = "n2o_val")
    
    ens_N2O_cum_gha <- merge(APSIMGN_cum_gha[,c("date","N2O_gha")],
                             DayGN_cum_gha[,c("date","N2O_gha")],
                             by="date",
                             all=TRUE) 
    colnames(ens_N2O_cum_gha) <- c("date","APSIM","Daycent")
    
    ens_N2O_cum_kgha <- ens_N2O_cum_gha %>%
      mutate(APSIM=APSIM/1000,
             Daycent=Daycent/1000)
    
    ens_N2O_cum_kgha_piv <- pivot_longer(ens_N2O_cum_kgha, c(-date),
                                         names_to = "Model",
                                         values_to = "n2o_val")
    
    ### N2O calibration comparison
    ens_N2O_comp_ghayr <- merge(merge(ObsGas_N2O_calib,
                                      APSIMGN_profile_cum_calib,
                                      by="year",
                                      all=TRUE),
                                DayGN_cum_calib,
                                by="year",
                                all=TRUE)
    colnames(ens_N2O_comp_ghayr) <- c("year","Observed", "APSIM","Daycent")
    
    ens_N2O_comp_ghayr_piv <- pivot_longer(ens_N2O_comp_ghayr, c(-year),
                                           names_to = "Source",
                                           values_to = "n2o_val")
    
    
    #### whole profile
    ens_N2O_profile_comp_ghayr <- merge(merge(ObsGas_N2O_calib,
                                              APSIMGN_profile_cum_calib,
                                              by="year",
                                              all=TRUE),
                                        DayGN_cum_calib,
                                        by="year",
                                        all=TRUE)
    
    colnames(ens_N2O_profile_comp_ghayr) <- c("year","Observed", "APSIM","Daycent")
    
    ens_N2O_profile_comp_ghayr_piv <- pivot_longer(ens_N2O_comp_ghayr, c(-year),
                                                   names_to = "Source",
                                                   values_to = "n2o_val")
    
    #### totals over the whole sampling period
    ens_N2O_profile_comp_gtot <- cbind(cbind(cbind(cbind(ObsGas_N2O_calib %>% summarize(tot_N2O_gha=sum(tot_N2O_ghayr)),
                                             APSIMGN_profile_cum_calib %>% summarize(tot_N2O_gha=sum(tot_N2O_ghayr))),
                                       DayGN_cum_calib %>% summarize(tot_N2O_gha=sum(tot_N2O_ghayr))),
                                         scenario_descriptor),scenario_abbrev)
    
    colnames(ens_N2O_profile_comp_gtot) <- c("Observed", "APSIM","Daycent","treatment_scen","scenario_abbrev")
    
    ens_N2O_profile_comp_gtot_piv <- pivot_longer(ens_N2O_profile_comp_gtot, 
                                                  c(-treatment_scen,-scenario_abbrev),
                                                  names_to = "Source",
                                                  values_to = "n2o_val")
    
    ## CH4 ------------------
    
    ens_CH4_ghaday <- merge(ObsGas[,c("date","CH4_C")],
                            DayGM_ghaday[,c("date","CH4_net_gChad")],
                            by="date",
                            all=TRUE)
    colnames(ens_CH4_ghaday) <- c("date","Observed","Daycent")
    
    ens_CH4_ghaday_piv <- pivot_longer(ens_CH4_ghaday, c(-date),
                                       names_to = "Model",
                                       values_to = "ch4_val")
    
    ens_CH4_cum_kgha <- ens_CH4_ghaday %>%
      mutate(Daycent = cumsum(Daycent)/1000)
    
    ens_CH4_cum_kgha_piv <- pivot_longer(ens_CH4_cum_kgha, c(-date,-Observed),
                                         names_to = "Model",
                                         values_to = "ch4_val")
    
    ### CH4 calibration comparison
    ens_CH4_comp_ghayr <- merge(ObsGas_CH4_calib,
                                DayGM_cum_calib,
                                by="year",
                                all=TRUE)
    colnames(ens_CH4_comp_ghayr) <- c("year","Observed","Daycent")
    
    ens_CH4_comp_ghayr_piv <- pivot_longer(ens_CH4_comp_ghayr, c(-year),
                                           names_to = "Source",
                                           values_to = "ch4_val")
    
    #### totals over the whole sampling period
    ens_CH4_profile_comp_gtot <- cbind(cbind(cbind(ObsGas_CH4_calib %>% summarize(tot_CH4_gha=sum(tot_CH4_ghayr)),
                                             DayGM_cum_calib %>% summarize(tot_CH4_gha=sum(tot_CH4_ghayr))),
                                       scenario_descriptor),scenario_abbrev)

    colnames(ens_CH4_profile_comp_gtot) <- c("Observed", "Daycent","treatment_scen","scenario_abbrev")
    
    ens_CH4_profile_comp_gtot_piv <- pivot_longer(ens_CH4_profile_comp_gtot, 
                                                  c(-treatment_scen,-scenario_abbrev),
                                                  names_to = "Source",
                                                  values_to = "ch4_val")
    
    
    
    # Calibration temporal graphs ---------------------------------------------
    
    if(clim_scenario_num == 1 & mgmt_scenario_num %in% calib_mgmt_nums) {
      
      ## Maize ---------------
      MYfit_APSIM <- coef(lm(APSIM ~ year, 
                             data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year %in% experiment_year_range,]))
      MYfit_Daycent <- coef(lm(Daycent ~ year, 
                               data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year %in% experiment_year_range,]))
      MYfit_Observed <- coef(lm(Observed ~ year, 
                                data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year %in% experiment_year_range,]))
      
      gMY_calib <- ens_MaizeYld_Mgha_piv[ens_MaizeYld_Mgha_piv$year %in% experiment_year_range,] %>%
        ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
        geom_point(show.legend=TRUE) +
        xlab("Year") +
        ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
        ggtitle(paste(site_name,"Maize Yield Calibration"),
                paste0("Scenario: ",scenario_descriptor)) +
        geom_abline(intercept=MYfit_APSIM[1], slope=MYfit_APSIM[2], color="orange") +
        geom_abline(intercept=MYfit_Daycent[1], slope=MYfit_Daycent[2], color="#0072B2") +
        geom_abline(intercept=MYfit_Observed[1], slope=MYfit_Observed[2], color="#000000") +
        geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                      width=.2) + # Width of the error bars
        scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                           values=c(APSIM_color,Daycent_color,Observed_color)) +
        theme_classic(base_family = "serif", base_size = 15) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gMY_calib
      
      ## Soybean ----------------
      
      SYfit_APSIM <- coef(lm(APSIM ~ year, 
                             data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year %in% experiment_year_range,]))
      SYfit_Daycent <- coef(lm(Daycent ~ year, 
                               data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year %in% experiment_year_range,]))
      SYfit_Observed <- coef(lm(Observed ~ year, 
                                data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year %in% experiment_year_range,]))
      
      gSY_calib <- ens_SoyYld_Mgha_piv[ens_SoyYld_Mgha_piv$year %in% experiment_year_range,] %>%
        ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
        geom_point(show.legend=TRUE) +
        xlab("Year") +
        ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
        ggtitle(paste(site_name,"Soybean Yield Calibration"),
                paste0("Scenario: ",scenario_descriptor)) +
        geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color="orange") +
        geom_abline(intercept=SYfit_Daycent[1], slope=SYfit_Daycent[2], color="#0072B2") +
        geom_abline(intercept=SYfit_Observed[1], slope=SYfit_Observed[2], color="#000000") +
        geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                      width=.2) + # Width of the error bars
        scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                           values=c(APSIM_color,Daycent_color,Observed_color)) +
        theme_classic(base_family = "serif", base_size = 15) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gSY_calib
      
      # Wheat --------------
      
      WYfit_APSIM <- coef(lm(APSIM ~ year, 
                             data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year %in% experiment_year_range,]))
      WYfit_Daycent <- coef(lm(Daycent ~ year, 
                               data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year %in% experiment_year_range,]))
      WYfit_Observed <- coef(lm(Observed ~ year, 
                                data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year %in% experiment_year_range,]))
      
      gWY_calib <- ens_WheatYld_Mgha_piv[ens_WheatYld_Mgha_piv$year %in% experiment_year_range,] %>%
        ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
        geom_point(show.legend=TRUE) +
        xlab("Year") +
        ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
        ggtitle(paste(site_name,"Wheat Yield Calibration"),
                paste0("Scenario: ",scenario_descriptor)) +
        geom_abline(intercept=WYfit_APSIM[1], slope=WYfit_APSIM[2], color="orange") +
        geom_abline(intercept=WYfit_Daycent[1], slope=WYfit_Daycent[2], color="#0072B2") +
        geom_abline(intercept=WYfit_Observed[1], slope=WYfit_Observed[2], color="#000000") +
        geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                      width=.2) +  # Width of the error bars
        scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                           values=c(APSIM_color,Daycent_color,Observed_color)) +
        theme_classic(base_family = "serif", base_size = 15) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gWY_calib
      
      ## SOC ----------------
      
      Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range,]))
      Cfit_Obs_noout <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range &
                                                                          !(ens_Cstock_Mgha$Observed %in% ObsC_outliers),]))
      ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range,]
      
      gC_calib <- ens_Cstock_df[ens_Cstock_df$year %in% experiment_year_range &
                                  ens_Cstock_df$Model == "Observed",] %>%
        ggplot(aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
        geom_point(show.legend=TRUE) +
        geom_point(data=ens_Cstock_df[ens_Cstock_df$year %in% experiment_year_range &
                                        ens_Cstock_df$Model != "Observed",],
                   aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
        geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
        geom_abline(intercept=Cfit_Obs_noout[1], slope=Cfit_Obs_noout[2], color="black", linetype=2) +
        geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                      width=.2,                    # Width of the error bars
                      position=position_dodge(.9)) +
        xlab("Year") +
        ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
        ggtitle(paste(site_name,"Soil Organic Carbon Calibration"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("APSIM","Daycent","Observed","RothC"),
                           values=c(APSIM_color,Daycent_color,
                                    Observed_color,RothC_color)) +
        theme_classic(base_family = "serif", base_size = 15) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gC_calib 
      
      Cfit_Obs2 <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year < 2002,]))
      Cfit_Obs_noout2 <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year < 2002 &
                                                                           !(ens_Cstock_Mgha$Observed %in% ObsC_outliers),]))
      ens_Cstock_df2 <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range &
                                                  ens_Cstock_Mgha_piv_adj$year < 2002,]
      
      gC_calib2 <- ens_Cstock_df2[ens_Cstock_df2$year < 2002 &
                                    ens_Cstock_df2$Model == "Observed",] %>%
        ggplot(aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
        geom_point(show.legend=TRUE) +
        geom_point(data=ens_Cstock_df2[ens_Cstock_df2$year < 2002 &
                                         ens_Cstock_df2$Model != "Observed",],
                   aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
        # geom_abline(intercept=Cfit_Obs2[1], slope=Cfit_Obs2[2], color="black") +
        # geom_abline(intercept=Cfit_Obs_noout2[1], slope=Cfit_Obs_noout2[2], color="black", linetype=2) +
        geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                      width=.2,                    # Width of the error bars
                      position=position_dodge(.9)) +
        xlab("Year") +
        ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
        ggtitle(paste(site_name,"Soil Organic Carbon Calibration"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("APSIM","Daycent","Observed","RothC"),
                           values=c(APSIM_color,Daycent_color,
                                    Observed_color,RothC_color)) +
        theme_classic(base_family = "serif", base_size = 25) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gC_calib2
      
      ## Soil Moisture ------------------
      
      gVh <- ens_VM_piv %>%
        ggplot(aes(x=vm_val, color=Source)) +
        geom_histogram(fill="white", alpha=0.5, position="identity") +
        xlab("Volumetric Water Content (%)") +
        ggtitle(paste(site_name,"Volumetric Water Content Histograms"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                           values=c(APSIM_color,Daycent_color,Observed_color)) +
        theme_classic(base_family = "serif", base_size = 15) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gVh
      
      
      gVd_calib <- ens_VM_piv %>%
        ggplot(aes(x=vm_val, color=Source, fill=Source)) +
        geom_density(alpha=0.3) +
        xlab("Volumetric Water Content (%)") +
        xlim(0,60) +
        ylim(0,0.2) +
        ggtitle(paste(site_name,"Volumetric Water Content Density"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                           values=c(APSIM_color,Daycent_color,Observed_color)) +
        scale_fill_manual(labels=c("APSIM","Daycent","Observed"),
                          values=c(APSIM_color,Daycent_color,Observed_color)) +
        theme_classic(base_family = "serif", base_size = 25) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gVd_calib
      
      
      vwc_frctn_atover_fc_APSIM <- nrow(filter(ens_VM, APSIM>=26))/nrow(ens_VM)
      vwc_frctn_atover_fc_Daycent <- nrow(filter(ens_VM, Daycent>=26))/nrow(ens_VM)
      vwc_frctn_atover_fc_Observed <- nrow(filter(ens_VM, Observed>=26))/nrow(ens_VM)
      vwc_frctn_atover_fc_df <- cbind(vwc_frctn_atover_fc_APSIM,vwc_frctn_atover_fc_Daycent,
                                      vwc_frctn_atover_fc_Observed)
      colnames(vwc_frctn_atover_fc_df) <- c("APSIM","Daycent","Observed")
      write.table(vwc_frctn_atover_fc_df,file=paste0(results_path,"pub_vwc_frctn_at_or_over_fc_",scenario_name,".txt"),
                  row.names = F,quote=F)
      
      vwc_frctn_over_30_APSIM <- nrow(filter(ens_VM, APSIM>=30))/nrow(ens_VM)
      vwc_frctn_over_35_APSIM <- nrow(filter(ens_VM, APSIM>=35))/nrow(ens_VM)
      
      vwc_frctn_over_30_Daycent <- nrow(filter(ens_VM, Daycent>=30))/nrow(ens_VM)
      vwc_frctn_over_35_Daycent <- nrow(filter(ens_VM, Daycent>=35))/nrow(ens_VM)
      
      vwc_frctn_over_30_Observed <- nrow(filter(ens_VM, Observed>=30))/nrow(ens_VM)
      vwc_frctn_over_35_Observed <- nrow(filter(ens_VM, Observed>=35))/nrow(ens_VM)
      
      vwc_frctn_over_30_df <- cbind(vwc_frctn_over_30_APSIM,vwc_frctn_over_30_Daycent,
                                    vwc_frctn_over_30_Observed)
      colnames(vwc_frctn_over_30_df) <- c("APSIM","Daycent","Observed")
      write.table(vwc_frctn_over_30_df,file=paste0(results_path,"pub_vwc_frctn_over_30_",scenario_name,".txt"),
                  row.names = F,quote=F)
      
      
      ## N2O ------------------
      
      gN_calib <- ens_N2O_profile_comp_gtot_piv %>%
        ggplot(aes(x=Source, y=n2o_val, color=Source, fill=Source)) +
        geom_col(position="stack") +
        ylim(0,1000) +
        ylab(expression('N'[2]*'O (g N ha' ^'-1'*')')) +
        ggtitle(bquote(.(site_name)~"Total Modeled N"[2]*"O vs. Observations"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                           values=c(APSIM_color,Daycent_color,Observed_color)) +
        scale_fill_manual(labels=c("APSIM","Daycent","Observed"),
                          values=c(APSIM_color,Daycent_color,Observed_color)) +
        theme_classic(base_family = "serif", base_size = 25) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      
      gN_calib
      
      frctn_diff_N2O_APSIM_Daycent <- (ens_N2O_profile_comp_gtot$APSIM-ens_N2O_profile_comp_gtot$Daycent)/ens_N2O_profile_comp_gtot$Daycent
      frctn_diff_N2O_APSIM_Obs <- (ens_N2O_profile_comp_gtot$APSIM-ens_N2O_profile_comp_gtot$Observed)/ens_N2O_profile_comp_gtot$Observed
      frctn_diff_N2O_Daycent_Obs <- (ens_N2O_profile_comp_gtot$Daycent-ens_N2O_profile_comp_gtot$Observed)/ens_N2O_profile_comp_gtot$Observed
      model_mean_N2O <- mean(ens_N2O_profile_comp_gtot_piv$n2o_val)
      frctn_diff_N2O_mod_means_Obs <- (model_mean_N2O-ens_N2O_profile_comp_gtot$Observed)/ens_N2O_profile_comp_gtot$Observed
      n2o_vr <- c(frctn_diff_N2O_APSIM_Daycent,frctn_diff_N2O_APSIM_Obs,
                  frctn_diff_N2O_Daycent_Obs,model_mean_N2O,
                  frctn_diff_N2O_mod_means_Obs)
      write.csv(n2o_vr,file=paste0(results_path,"calib_n2o_diffs_",scenario_name,".csv"),
                row.names=c("diff_apsim_day","diff_apsim_obs","diff_day_obs",
                            "model_mean","diff_mods_obs"))
      
      ## CH4 -----------------
      
      gM_calib <- ens_CH4_profile_comp_gtot_piv %>%
        ggplot(aes(x=Source, y=ch4_val, color=Source, fill=Source)) +
        geom_col(position="stack") +
        ylim(-350,0) +
        ylab(expression('CH'[4]*' (g C ha' ^'-1'*')')) +
        ggtitle(bquote(.(site_name)~"Total Modeled CH"[4]*" vs. Observations"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("Daycent","Observed"),
                           values=c(Daycent_color,Observed_color)) +
        scale_fill_manual(labels=c("Daycent","Observed"),
                          values=c(Daycent_color,Observed_color)) +
        theme_classic(base_family = "serif", base_size = 25) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      
      gM_calib
      
      
      frctn_diff_CH4_Daycent_Obs <- (ens_CH4_profile_comp_gtot$Daycent-ens_CH4_profile_comp_gtot$Observed)/ens_CH4_profile_comp_gtot$Observed
      
      ggsave(filename=paste0(results_path,"pub_Ensemble_Maize_calibration_",scenario_name,".jpg"),
             plot=gMY_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_Soybean_calibration_",scenario_name,".jpg"),
             plot=gSY_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_Wheat_calibration_",scenario_name,".jpg"),
             plot=gWY_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_SOC_calibration_",scenario_name,".jpg"),
             plot=gC_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_SOC_calibration2_",scenario_name,".jpg"),
             plot=gC_calib2, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_VWC_density_calibration_",scenario_name,".jpg"),
             plot=gVd_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_N2O_calibration_",scenario_name,".jpg"),
             plot=gN_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_CH4_calibration_",scenario_name,".jpg"),
             plot=gM_calib, width=9, height=6, dpi=300)
      
      
      ## Save data for later use combining graphs --------------------
      crop_calib_output_df <- rbind(ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year %in% experiment_year_range,],
                                    ens_SoyYld_Mgha[ens_SoyYld_Mgha$year %in% experiment_year_range,],
                                    ens_WheatYld_Mgha[ens_WheatYld_Mgha$year %in% experiment_year_range,])
      soc_calib_output_df <- rbind(ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range,])

      p_Edit_calib_data_file(crop_calib_output_df,
                             paste0(results_path,"calib_crop_df.csv"))
      p_Edit_calib_data_file(soc_calib_output_df,
                             paste0(results_path,"calib_soc_df.csv"))
      
      
      crop_calib_output_df_piv <- rbind(ens_MaizeYld_Mgha_piv[ens_MaizeYld_Mgha_piv$year %in% experiment_year_range,],
                                        ens_SoyYld_Mgha_piv[ens_SoyYld_Mgha_piv$year %in% experiment_year_range,],
                                        ens_WheatYld_Mgha_piv[ens_WheatYld_Mgha_piv$year %in% experiment_year_range,])
      soc_calib_output_df_piv <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range,]
      n2o_calib_output_df_piv <- ens_N2O_profile_comp_gtot_piv
      ch4_calib_output_df_piv <- ens_CH4_profile_comp_gtot_piv
            
      p_Edit_calib_data_file(crop_calib_output_df_piv,
                             paste0(results_path,"calib_crop_df_piv.csv"))
      p_Edit_calib_data_file(soc_calib_output_df_piv,
                             paste0(results_path,"calib_soc_df_piv.csv"))
      p_Edit_calib_data_file(n2o_calib_output_df_piv,
                             paste0(results_path,"calib_n2o_df_piv.csv"))
      p_Edit_calib_data_file(ch4_calib_output_df_piv,
                             paste0(results_path,"calib_ch4_df_piv.csv"))
      
      ## for trendlines
      
      obs_SOC_trendline_dat <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range,] %>%
        filter(Model == "Observed") %>%
        mutate(
          AllObs = C_val,
          OutlierRemoved = ifelse(C_val %in% ObsC_outliers, NA, C_val),
          .keep = "unused"
        ) %>%
        pivot_longer(AllObs:OutlierRemoved, names_to = "Fit") %>%
        mutate(treatment_scen=scenario_descriptor)
      
      p_Edit_calib_data_file(obs_SOC_trendline_dat,
                             paste0(results_path,"calib_soc_trendline_piv.csv"))
      
      
      
    } # end if mgmt_scenario is a calibration treatment
    
    # Future temporal graphs --------------------------------------------------
    
    
    
    MYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year>=experiment_end_year,]))
    MYfit_Daycent <- coef(lm(Daycent ~ year, 
                             data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year>=experiment_end_year,]))
    
    gMY <- ens_MaizeYld_Mgha_piv[ens_MaizeYld_Mgha_piv$Model %in% c("APSIM","Daycent") &
                                   ens_MaizeYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Maize Yield: ",scenario_descriptor)) +
      geom_abline(intercept=MYfit_APSIM[1], slope=MYfit_APSIM[2], color="orange") +
      geom_abline(intercept=MYfit_Daycent[1], slope=MYfit_Daycent[2], color="#0072B2") +
      scale_color_manual(labels=c("APSIM","Daycent"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gMY
    
    #
    SYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year>=experiment_end_year,]))
    SYfit_Daycent <- coef(lm(Daycent ~ year, 
                             data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year>=experiment_end_year,]))
    
    gSY <- ens_SoyYld_Mgha_piv[ens_SoyYld_Mgha_piv$Model %in% c("APSIM","Daycent") &
                                 ens_SoyYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Soybean Yield: ",scenario_descriptor)) +
      geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color="orange") +
      geom_abline(intercept=SYfit_Daycent[1], slope=SYfit_Daycent[2], color="#0072B2") +
      scale_color_manual(labels=c("APSIM","Daycent"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gSY
    
    #
    WYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year>=experiment_end_year,]))
    WYfit_Daycent <- coef(lm(Daycent ~ year, 
                             data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year>=experiment_end_year,]))
    
    gWY <- ens_WheatYld_Mgha_piv[ens_WheatYld_Mgha_piv$Model %in% c("APSIM","Daycent") &
                                   ens_WheatYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Wheat Yield: ",scenario_descriptor)) +
      geom_abline(intercept=WYfit_APSIM[1], slope=WYfit_APSIM[2], color="orange") +
      geom_abline(intercept=WYfit_Daycent[1], slope=WYfit_Daycent[2], color="#0072B2") +
      scale_color_manual(labels=c("APSIM","Daycent"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gWY
    
    if(mgmt_scenario_num==3) {
      Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year >= experiment_start_year &
                                                                    ens_Cstock_Mgha$year!=1998,]))
      ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year>=experiment_start_year &
                                                 ens_Cstock_Mgha_piv_adj$year!=1998,]
    } else {
      Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year >= experiment_start_year,]))
      ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year>=experiment_start_year,]
    }
    
    gC <- ens_Cstock_df[ens_Cstock_df$year <= experiment_end_year &
                          ens_Cstock_df$Model == "Observed",] %>%
      ggplot(aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
      geom_point(show.legend=TRUE) +
      geom_line(data=ens_Cstock_df[ens_Cstock_df$year <= experiment_end_year &
                                     ens_Cstock_df$Model != "Observed",],
                aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
      ggtitle(paste(site_name,"Soil Organic Carbon: ",scenario_descriptor)) +
      scale_color_manual(labels=c("APSIM","Daycent","Observed","RothC"),
                         values=c(APSIM_color,Daycent_color,
                                  Observed_color,RothC_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gC 
    
    gNG <- ens_N2O_cum_kgha_piv %>%
      ggplot(aes(x=date, y=n2o_val, color=Model)) +
      geom_line(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
      ylim(0,100) +
      ggtitle(paste(site_name,"N2O Emissions: ",scenario_descriptor)) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gNG
    
    
    gMG <- ens_CH4_cum_kgha_piv %>%
      ggplot(aes(x=date, y=ch4_val, color=Model)) +
      geom_line(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('CH'[4]*' Net Emissions (kg ha ' ^-1*')')) +
      ylim(-85,0) +
      ggtitle(paste(site_name,expression('CH'[4]*' Emissions: '),scenario_descriptor)) +
      scale_color_manual(labels=c("Daycent"),
                         values=Daycent_color) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gMG
    
  } else { # mgmt_scenario_grp == 6
    
    # merge observed and modeled data
    ## use ens_ (ensemble) prefix to distinguish these from the "MaizeYld_Mgha" etc.
    ## files in each model's Results files.
    ens_MaizeYld_Mgha <- merge(ObsYield[ObsYield$crop=="Maize",c("year","mean_yield")],
                               APSIMY_Mgha[APSIMY_Mgha$MaizeYield_Mgha != 0,
                                           c("year","MaizeYield_Mgha")],
                               by="year",
                               all=TRUE)
    colnames(ens_MaizeYld_Mgha) <- c("year","Observed","APSIM")
    
    ens_MaizeYld_Mgha_piv <- pivot_longer(ens_MaizeYld_Mgha, c(-year),
                                          names_to = "Model",
                                          values_to = "yield_val")
    
    #
    ens_SoyYld_Mgha <- merge(ObsYield[ObsYield$crop=="Soybean",c("year","mean_yield")],
                             APSIMY_Mgha[APSIMY_Mgha$SoyYield_Mgha != 0,
                                         c("year","SoyYield_Mgha")],
                             by="year",
                             all=TRUE)
    colnames(ens_SoyYld_Mgha) <- c("year","Observed","APSIM")
    
    ens_SoyYld_Mgha_piv <- pivot_longer(ens_SoyYld_Mgha, c(-year),
                                        names_to = "Model",
                                        values_to = "yield_val")
    
    #
    ens_WheatYld_Mgha <- merge(ObsYield[ObsYield$crop=="Wheat",c("year","mean_yield")],
                               APSIMY_Mgha[APSIMY_Mgha$WheatYield_Mgha != 0,
                                           c("year","WheatYield_Mgha")],
                               by="year",
                               all=TRUE)
    colnames(ens_WheatYld_Mgha) <- c("year","Observed","APSIM")
    
    ens_WheatYld_Mgha_piv <- pivot_longer(ens_WheatYld_Mgha, c(-year),
                                          names_to = "Model",
                                          values_to = "yield_val")
    
    ##
    ens_Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock")],
                             APSIMC_Mgha,
                             by="year",
                             all=TRUE)
    
    colnames(ens_Cstock_Mgha) <- c("year","Observed","APSIM")
    
    ens_Cstock_Mgha_piv <-  pivot_longer(ens_Cstock_Mgha, c(-year),
                                         names_to = "Model",
                                         values_to = "C_val")
    
    ens_Cstock_Mgha_piv_adj <- ens_Cstock_Mgha_piv
    
    ## N2O
    
    ens_N2O_ghaday <- merge(ObsGas[,c("date","N2O_N")],
                            APSIMGN_ghaday[,c("date","N2OEmissions_ghaday")],
                            by="date",
                            all=TRUE)
    colnames(ens_N2O_ghaday) <- c("date","Observed","APSIM")
    
    ens_N2O_ghaday_piv <- pivot_longer(ens_N2O_ghaday, c(-date),
                                       names_to = "Model",
                                       values_to = "n2o_val")
    
    ens_N2O_cum_gha <- APSIMGN_cum_gha[,c("date","N2O_gha")]
    colnames(ens_N2O_cum_gha) <- c("date","APSIM")
    
    ens_N2O_cum_kgha <- ens_N2O_cum_gha %>%
      mutate(APSIM=APSIM/1000)
    
    ens_N2O_cum_kgha_piv <- pivot_longer(ens_N2O_cum_kgha, c(-date),
                                         names_to = "Model",
                                         values_to = "n2o_val")
    
    
    # Temporal graphs
    
    
    MYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_MaizeYld_Mgha[ens_MaizeYld_Mgha$year>=experiment_end_year,]))
    
    gMY <- ens_MaizeYld_Mgha_piv[ens_MaizeYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Maize Yield: ",scenario_descriptor)) +
      geom_abline(intercept=MYfit_APSIM[1], slope=MYfit_APSIM[2], color="orange") +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gMY
    
    #
    SYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_SoyYld_Mgha[ens_SoyYld_Mgha$year>=experiment_end_year,]))
    
    gSY <- ens_SoyYld_Mgha_piv[ens_SoyYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Soybean Yield: ",scenario_descriptor)) +
      geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color="orange") +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gSY
    
    #
    WYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_WheatYld_Mgha[ens_WheatYld_Mgha$year>=experiment_end_year,]))
    
    gWY <- ens_WheatYld_Mgha_piv[ens_WheatYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Wheat Yield: ",scenario_descriptor)) +
      geom_abline(intercept=WYfit_APSIM[1], slope=WYfit_APSIM[2], color="orange") +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gWY
    
    Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year >= experiment_start_year,]))
    ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year>=experiment_start_year,]
    
    gC <- ens_Cstock_df[ens_Cstock_df$year <= experiment_end_year &
                          ens_Cstock_df$Model == "Observed",] %>%
      ggplot(aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
      geom_point(show.legend=TRUE) +
      geom_line(data=ens_Cstock_df[ens_Cstock_df$year <= experiment_end_year &
                                     ens_Cstock_df$Model != "Observed",],
                aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
      ggtitle(paste(site_name,"Soil Organic Carbon: ",scenario_descriptor)) +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gC 
    
    gNG <- ens_N2O_cum_kgha_piv %>%
      ggplot(aes(x=date, y=n2o_val, color=Model)) +
      geom_line(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('N'[2]*'O Emissions (kg N ha ' ^-1*')')) +
      ggtitle(paste(site_name,"N2O Emissions: ",scenario_descriptor)) +
      scale_color_manual(labels=c("APSIM"),
                         values=APSIM_color) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gNG
    
  } # end if mgmt_scenario_grp == 6
  
  ggsave(filename=paste0(results_path,"Ensemble_Maize_comparison_",scenario_name,".jpg"),
         plot=gMY, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Ensemble_Soybean_comparison_",scenario_name,".jpg"),
         plot=gSY, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Ensemble_Wheat_comparison_",scenario_name,".jpg"),
         plot=gWY, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Ensemble_SOC_comparison_",scenario_name,".jpg"),
         plot=gC, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Ensemble_N2O_cum_comparison_",scenario_name,".jpg"),
         plot=gNG, width=9, height=6, dpi=300)
  if(mgmt_scenario_grp!=6) {
    ggsave(filename=paste0(results_path,"Ensemble_CH4_cum_comparison_",scenario_name,".jpg"),
           plot=gMG, width=9, height=6, dpi=300)
  }
  
  
}) # end suppressMessages

