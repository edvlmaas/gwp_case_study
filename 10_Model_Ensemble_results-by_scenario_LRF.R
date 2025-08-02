#######################################
# File: 10_Model_Ensemble_Results-by_scenario_LRF.R
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
  
  # read in calibration stats
  calib_summary_raw <- read.csv(paste0(results_path,"Calibration_summary.csv"))
  calib_summary_df <- calib_summary_raw[calib_summary_raw$Scenario_Name==scenario_name]
  
  
  # 9-color palette with grey and black. Colors in order are:
  #[1]black, [2]dark blue, [3]green, [4]light blue, [5]grey,
  #[6]pink, [7]red, [8]orange, [9]yellow
  
  if(mgmt_scenario_num %in% c(calib_mgmt_nums)) {
  #  if(mgmt_scenario_num %in% c(calib_mgmt_nums,51)) {
      
    if(mgmt_scenario_grp!=7) {
      # merge observed and modeled data
      ## use ens_ (ensemble) prefix to distinguish these from the "SorghumYld_Mgha" etc.
      ## files in each model's Results code.
      ens_SorghumYld_Mgha <- merge(merge(ObsYield[ObsYield$crop=="Sorghum",c("year","mean_yield","sd_yield")],
                                         APSIMY_Mgha[APSIMY_Mgha$SorghumYield_Mgha != 0,
                                                     c("year","SorghumYield_Mgha")],
                                         by="year",
                                         all=TRUE),
                                   DayY_Mgha[DayY_Mgha$crop=="Sorghum",c("year","yield")],
                                   by="year",
                                   all=TRUE) %>%
        mutate(crop="Sorghum",
               treatment_scen=scenario_descriptor)
      colnames(ens_SorghumYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent",
                                         "crop","treatment_scen")
      
      ens_SorghumYld_Mgha_piv <- pivot_longer(ens_SorghumYld_Mgha, c(-year,-Obs_sd,
                                                                     -crop,-treatment_scen),
                                              names_to = "Model",
                                              values_to = "yield_val")
      
      # remove sd from modeled records; only for observed
      ens_SorghumYld_Mgha_piv <- ens_SorghumYld_Mgha_piv %>%
        mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))
      
      ## make another version including historical yields
      his_SorghumYld_Mgha <- merge(merge(HistY_Mgha[HistY_Mgha$Year >= 1980,
                                                    c("year","sorghum_yield_mgha")],
                                         APSIMY_Mgha[APSIMY_Mgha$SorghumYield_Mgha != 0, #&
                                                       #APSIMY_Mgha$year >= experiment_start_year,
                                                     c("year","SorghumYield_Mgha")],
                                         by="year",
                                         all=TRUE),
                                   DayY_Mgha[DayY_Mgha$crop=="Sorghum" &
                                               DayY_Mgha$yield !=0, #&
                                               #DayY_Mgha$year >= experiment_start_year,
                                             c("year","yield")],
                                   by="year",
                                   all=TRUE) %>%
        mutate(crop="Sorghum",
               treatment_scen=scenario_descriptor)
      colnames(his_SorghumYld_Mgha) <- c("year","Historical","APSIM","Daycent",
                                         "crop","treatment_scen")
      
      his_SorghumYld_Mgha_piv <- pivot_longer(his_SorghumYld_Mgha, c(-year,-crop,
                                                                     -treatment_scen),
                                              names_to = "Model",
                                              values_to = "yield_val")
      
      p_Edit_calib_data_file(his_SorghumYld_Mgha_piv,
                             paste0(results_path,"his_SorghumYld_Mgha_piv.csv"))
      
    } # end if mgmt scenario is not 7 (otherwise exclude sorghum code)
    
    #
    ens_CottonYld_Mgha <- merge(merge(ObsYield[ObsYield$crop=="Cotton",c("year","mean_yield","sd_yield")],
                                      APSIMY_Mgha[APSIMY_Mgha$CottonYield_Mgha != 0,
                                                  c("year","CottonYield_Mgha")],
                                      by="year",
                                      all=TRUE),
                                DayY_Mgha[DayY_Mgha$crop=="Cotton",c("year","yield")],
                                by="year",
                                all=TRUE) %>%
      mutate(crop="Cotton",
             treatment_scen=scenario_descriptor)
    colnames(ens_CottonYld_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent",
                                      "crop","treatment_scen")
    
    ens_CottonYld_Mgha_piv <- pivot_longer(ens_CottonYld_Mgha, c(-year,-Obs_sd,
                                                                 -crop,-treatment_scen),
                                           names_to = "Model",
                                           values_to = "yield_val")
    
    # remove sd from modeled records; only for observed
    ens_CottonYld_Mgha_piv <- ens_CottonYld_Mgha_piv %>%
      mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))
    
    
    ##
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
    
    colnames(ens_Cstock_Mgha) <- c("year","Observed","Obs_sd","APSIM","Daycent","RothC",
                                   "treatment_scen")
    
    ens_Cstock_Mgha_piv <-  pivot_longer(ens_Cstock_Mgha, c(-year,-Obs_sd,
                                                            -treatment_scen),
                                         names_to = "Model",
                                         values_to = "C_val")
    
    ### remove sd's from models, since it's only for observations
    ens_Cstock_Mgha_piv_adj <- ens_Cstock_Mgha_piv %>%
      mutate(Obs_sd=replace(Obs_sd, Model!="Observed", NA))
    
    ## soil moisture
    
    ens_VM <- merge(APSIMM_V[,c("date","VolH2O_5cm")],
                    DayM_V[,c("date","mean_5cm")],
                    by="date",
                    all=TRUE)
    colnames(ens_VM) <- c("date","APSIM","Daycent")
    
    ens_VM_piv <- pivot_longer(ens_VM, c(-date),
                               names_to = "Source",
                               values_to = "vm_val")
    
    ## N2O
    
    ens_N2O_ghaday <- merge(APSIMGN_ghaday[,c("date","N2OEmissions_ghaday")],
                            DayGN_ghaday[,c("date","N2O_gNhad")],
                            by="date",
                            all=TRUE)
    colnames(ens_N2O_ghaday) <- c("date","APSIM","Daycent")
    
    ens_N2O_ghaday_piv <- pivot_longer(ens_N2O_ghaday, c(-date),
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
    ens_N2O_comp_ghayr <- merge(APSIMGN_profile_cum_calib,
                                DayGN_cum_calib,
                                by="year",
                                all=TRUE)
    colnames(ens_N2O_comp_ghayr) <- c("year","APSIM","Daycent")
    
    ens_N2O_comp_ghayr_piv <- pivot_longer(ens_N2O_comp_ghayr, c(-year),
                                           names_to = "Source",
                                           values_to = "n2o_val")
    
    ens_N2O_comp_gtot <- merge((APSIMGN_cum_calib %>% summarize(tot_N2O_gha=sum(tot_N2O_ghayr))),
                               (DayGN_cum_calib %>% summarize(tot_N2O_gha=sum(tot_N2O_ghayr))),
                               all=TRUE)
    colnames(ens_N2O_comp_ghayr) <- c("year","APSIM","Daycent")
    
    ens_N2O_comp_ghayr_piv <- pivot_longer(ens_N2O_comp_ghayr, c(-year),
                                           names_to = "Source",
                                           values_to = "n2o_val")
    
    #### whole profile
    ens_N2O_profile_comp_ghayr <- merge(APSIMGN_profile_cum_calib,
                                        DayGN_cum_calib,
                                        by="year",
                                        all=TRUE)
    colnames(ens_N2O_profile_comp_ghayr) <- c("year","APSIM","Daycent")
    
    ens_N2O_profile_comp_ghayr_piv <- pivot_longer(ens_N2O_comp_ghayr, c(-year),
                                                   names_to = "Source",
                                                   values_to = "n2o_val")
    
    #### totals over the whole sampling/experimental period
    ens_N2O_profile_comp_gtot <- cbind(cbind(cbind(APSIMGN_profile_cum_calib %>% summarize(tot_N2O_gha=sum(tot_N2O_ghayr)),
                                       DayGN_cum_calib %>% summarize(tot_N2O_gha=sum(tot_N2O_ghayr))),
                                       scenario_descriptor),scenario_abbrev)
    
    colnames(ens_N2O_profile_comp_gtot) <- c("APSIM","Daycent","treatment_scen","scenario_abbrev")
    
    ens_N2O_profile_comp_gtot_piv <- pivot_longer(ens_N2O_profile_comp_gtot, 
                                                  c(-treatment_scen,-scenario_abbrev),
                                                  names_to = "Source",
                                                  values_to = "n2o_val")
    
 
    # Calibration temporal graphs ---------------------------------------------
    
    if(clim_scenario_num == 1 & mgmt_scenario_num %in% c(calib_mgmt_nums)) {
      # if(clim_scenario_num == 1 & mgmt_scenario_num %in% c(calib_mgmt_nums,51)) {
        
      if(mgmt_scenario_grp!=7) {
        ## Sorghum
        SYfit_APSIM <- coef(lm(APSIM ~ year, 
                               data = ens_SorghumYld_Mgha[ens_SorghumYld_Mgha$year %in% experiment_year_range,]))
        SYfit_Daycent <- coef(lm(Daycent ~ year, 
                                 data = ens_SorghumYld_Mgha[ens_SorghumYld_Mgha$year %in% experiment_year_range,]))
        SYfit_Observed <- coef(lm(Observed ~ year, 
                                  data = ens_SorghumYld_Mgha[ens_SorghumYld_Mgha$year %in% experiment_year_range,]))
        
        gSY_calib <- ens_SorghumYld_Mgha_piv[ens_SorghumYld_Mgha_piv$year %in% experiment_year_range,] %>%
          ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
          geom_point(show.legend=TRUE) +
          xlab("Year") +
          ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
          ggtitle(paste(site_name,"Sorghum Yield Calibration"),
                  paste0("Scenario: ",scenario_descriptor)) +
          geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color=APSIM_color) +
          geom_abline(intercept=SYfit_Daycent[1], slope=SYfit_Daycent[2], color=Daycent_color) +
          geom_abline(intercept=SYfit_Observed[1], slope=SYfit_Observed[2], color=Observed_color) +
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
      }
      
      ## Cotton
      
      CYfit_APSIM <- coef(lm(APSIM ~ year, 
                             data = ens_CottonYld_Mgha[ens_CottonYld_Mgha$year %in% experiment_year_range,]))
      CYfit_Daycent <- coef(lm(Daycent ~ year, 
                               data = ens_CottonYld_Mgha[ens_CottonYld_Mgha$year %in% experiment_year_range,]))
      CYfit_Observed <- coef(lm(Observed ~ year, 
                                data = ens_CottonYld_Mgha[ens_CottonYld_Mgha$year %in% experiment_year_range,]))
      
      gCY_calib <- ens_CottonYld_Mgha_piv[ens_CottonYld_Mgha_piv$year %in% experiment_year_range,] %>%
        ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
        geom_point(show.legend=TRUE) +
        xlab("Year") +
        ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
        ggtitle(paste(site_name,"Cotton Yield Calibration"),
                paste0("Scenario: ",scenario_descriptor)) +
        geom_abline(intercept=CYfit_APSIM[1], slope=CYfit_APSIM[2], color=APSIM_color) +
        geom_abline(intercept=CYfit_Daycent[1], slope=CYfit_Daycent[2], color=Daycent_color) +
        geom_abline(intercept=CYfit_Observed[1], slope=CYfit_Observed[2], color=Observed_color) +
        geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                      width=.2) + # Width of the error bars
        scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                           values=c(APSIM_color,Daycent_color,Observed_color)) +
        theme_classic(base_family = "serif", base_size = 15) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gCY_calib
      
      
      ## SOC
      
      Cfit_Obs <- coef(lm(Observed ~ year, data = ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range,]))
      ens_Cstock_df <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range,]
      
      gC_calib <- ens_Cstock_df[ens_Cstock_df$year %in% experiment_year_range &
                                  ens_Cstock_df$Model == "Observed",] %>%
        ggplot(aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
        geom_point(show.legend=TRUE) +
        geom_point(data=ens_Cstock_df[ens_Cstock_df$year %in% experiment_year_range &
                                        ens_Cstock_df$Model != "Observed",],
                   aes(x=year, y=C_val, color=Model), show.legend=TRUE) +
        geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color=Observed_color) +
        geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                      width=.2,                    # Width of the error bars
                      position=position_dodge(.9)) +
        xlab("Year") +
        ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
        ggtitle(paste(site_name,"Soil Organic Carbon Calibration"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("APSIM","Daycent","Observed","RothC"),
                           values=c(APSIM_color,Daycent_color,Observed_color,RothC_color)) +
        theme_classic(base_family = "serif", base_size = 15) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gC_calib 
      
      ## Soil Moisture (no observations, just models)
      
      #test_density_APSIM <- density(ens_VM$APSIM)
      #test_density_APSIM[[2]]
      
      gVd_calib <- ens_VM_piv %>%
        ggplot(aes(x=vm_val, color=Source, fill=Source)) +
        geom_density(alpha=0.3) +
        xlab("Volumetric Water Content (%)") +
        xlim(0,60) +
        ylim(0,1) +
        ggtitle(paste(site_name,"Volumetric Water Content Density"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("APSIM","Daycent"),
                           values=c(APSIM_color,Daycent_color)) +
        scale_fill_manual(labels=c("APSIM","Daycent"),
                          values=c(APSIM_color,Daycent_color)) +
        theme_classic(base_family = "serif", base_size = 25) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gVd_calib
      
      
      vwc_frctn_atover_fc_APSIM <- nrow(filter(ens_VM, APSIM>=18))/nrow(ens_VM)
      vwc_frctn_atover_fc_Daycent <- nrow(filter(ens_VM, Daycent>=18))/nrow(ens_VM)
      vwc_frctn_atover_fc_df <- cbind(vwc_frctn_atover_fc_APSIM,vwc_frctn_atover_fc_Daycent)
      colnames(vwc_frctn_atover_fc_df) <- c("APSIM","Daycent")
      write.table(vwc_frctn_atover_fc_df,file=paste0(results_path,"pub_vwc_frctn_at_or_over_fc_",scenario_name,".txt"),
                  row.names = F,quote=F)
      
      vwc_frctn_over_5_APSIM <- nrow(filter(ens_VM, APSIM>=5))/nrow(ens_VM)
      vwc_frctn_over_10_APSIM <- nrow(filter(ens_VM, APSIM>=10))/nrow(ens_VM)
      vwc_frctn_over_15_APSIM <- nrow(filter(ens_VM, APSIM>=15))/nrow(ens_VM)
      vwc_frctn_under_10_APSIM <- nrow(filter(ens_VM, APSIM<10))/nrow(ens_VM)
      vwc_frctn_under_11_APSIM <- nrow(filter(ens_VM, APSIM<11))/nrow(ens_VM)
      vwc_frctn_under_12_APSIM <- nrow(filter(ens_VM, APSIM<12))/nrow(ens_VM)
      
      vwc_frctn_over_5_Daycent <- nrow(filter(ens_VM, Daycent>=5))/nrow(ens_VM)
      vwc_frctn_over_10_Daycent <- nrow(filter(ens_VM, Daycent>=10))/nrow(ens_VM)
      vwc_frctn_over_15_Daycent <- nrow(filter(ens_VM, Daycent>=15))/nrow(ens_VM)
      vwc_frctn_under_10_Daycent <- nrow(filter(ens_VM, Daycent<10))/nrow(ens_VM)
      vwc_frctn_under_11_Daycent <- nrow(filter(ens_VM, Daycent<11))/nrow(ens_VM)
      vwc_frctn_under_12_Daycent <- nrow(filter(ens_VM, Daycent<12))/nrow(ens_VM)
      
      vwc_frctn_under_10_df <- cbind(vwc_frctn_under_10_APSIM,vwc_frctn_under_10_Daycent)
      colnames(vwc_frctn_under_10_df) <- c("APSIM","Daycent")
      write.table(vwc_frctn_under_10_df,file=paste0(results_path,"pub_vwc_frctn_under_10_",scenario_name,".txt"),
                  row.names = F,quote=F)
      
      ## N2O
      
      gN_calib <- ens_N2O_profile_comp_gtot_piv %>%
        ggplot(aes(x=Source, y=n2o_val, color=Source, fill=Source)) +
        geom_col(position="stack") +
        ylim(0,2000) +
        ylab(expression('N'[2]*'O (g N ha' ^'-1'*')')) +
        ggtitle(bquote(.(site_name)~"Total Modeled N"[2]*"O"),
                paste0("Scenario: ",scenario_descriptor)) +
        scale_color_manual(labels=c("APSIM","Daycent"),
                           values=c(APSIM_color,Daycent_color)) +
        scale_fill_manual(labels=c("APSIM","Daycent"),
                          values=c(APSIM_color,Daycent_color)) +
        theme_classic(base_family = "serif", base_size = 25) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      
      gN_calib
      
      frctn_diff_N2O_APSIM_Daycent <- (ens_N2O_profile_comp_gtot$APSIM-ens_N2O_profile_comp_gtot$Daycent)/ens_N2O_profile_comp_gtot$Daycent
      frctn_diff_N2O_APSIM_Obs <- (ens_N2O_profile_comp_gtot$APSIM-ens_N2O_profile_comp_gtot$Observed)/ens_N2O_profile_comp_gtot$Observed
      frctn_diff_N2O_Daycent_Obs <- (ens_N2O_profile_comp_gtot$Daycent-ens_N2O_profile_comp_gtot$Observed)/ens_N2O_profile_comp_gtot$Observed
      
      
      if(mgmt_scenario_grp!=7) {
        ggsave(filename=paste0(results_path,"pub_Ensemble_Sorghum_calibration_",scenario_name,".jpg"),
             plot=gSY_calib, width=9, height=6, dpi=300)
      }
      ggsave(filename=paste0(results_path,"pub_Ensemble_Cotton_calibration_",scenario_name,".jpg"),
             plot=gCY_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_SOC_calibration_",scenario_name,".jpg"),
             plot=gC_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_VWC_density_calibration_",scenario_name,".jpg"),
             plot=gVd_calib, width=9, height=6, dpi=300)
      ggsave(filename=paste0(results_path,"pub_Ensemble_N2O_comparison_",scenario_name,".jpg"),
             plot=gN_calib, width=9, height=6, dpi=300)
      
      
      ## Save data for later use combining graphs -----------------------
      
      if(mgmt_scenario_grp!=7) {
        crop_calib_output_df <- rbind(ens_CottonYld_Mgha[ens_CottonYld_Mgha$year %in% experiment_year_range,],
                                      ens_SorghumYld_Mgha[ens_SorghumYld_Mgha$year %in% experiment_year_range,])
      } else {
        crop_calib_output_df <- ens_CottonYld_Mgha[ens_CottonYld_Mgha$year %in% experiment_year_range,]
      }
      soc_calib_output_df <- ens_Cstock_Mgha[ens_Cstock_Mgha$year %in% experiment_year_range,]

      p_Edit_calib_data_file(crop_calib_output_df,
                             paste0(results_path,"calib_crop_df.csv"))
      p_Edit_calib_data_file(soc_calib_output_df,
                             paste0(results_path,"calib_soc_df.csv"))

      
      
      if(mgmt_scenario_grp!=7) {
        crop_calib_output_df_piv <- rbind(ens_CottonYld_Mgha_piv[ens_CottonYld_Mgha_piv$year %in% experiment_year_range,],
                                          ens_SorghumYld_Mgha_piv[ens_SorghumYld_Mgha_piv$year %in% experiment_year_range,])
      }else {
        crop_calib_output_df_piv <- rbind(ens_CottonYld_Mgha_piv[ens_CottonYld_Mgha_piv$year %in% experiment_year_range,])
      }
      soc_calib_output_df_piv <- rbind(ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range,])
      n2o_calib_output_df_piv <- ens_N2O_profile_comp_gtot_piv
      
      p_Edit_calib_data_file(crop_calib_output_df_piv,
                             paste0(results_path,"calib_crop_df_piv.csv"))
      p_Edit_calib_data_file(soc_calib_output_df_piv,
                             paste0(results_path,"calib_soc_df_piv.csv"))
      p_Edit_calib_data_file(n2o_calib_output_df_piv,
                             paste0(results_path,"calib_n2o_df_piv.csv"))
      
      # ## for trendlines
      # 
      # obs_SOC_trendline_dat <- ens_Cstock_Mgha_piv_adj[ens_Cstock_Mgha_piv_adj$year %in% experiment_year_range,] %>%
      #   filter(Model == "Observed") %>%
      #   mutate(
      #     AllObs = C_val,
      #     OutlierRemoved = ifelse(C_val %in% ObsC_outliers, NA, C_val),
      #     .keep = "unused"
      #   ) %>%
      #   pivot_longer(AllObs:OutlierRemoved, names_to = "Fit") %>%
      #   mutate(treatment_scen=scenario_descriptor)
      # 
      # p_Edit_calib_data_file(obs_SOC_trendline_dat,
      #                        paste0(results_path,"calib_soc_trendline_piv.csv"))
      
      
    } # end if mgmt_scenario is a calibration treatment
    
    
    
    # Future temporal graphs --------------------------------------------------
    
    
    if(mgmt_scenario_grp!=7) {
      SYfit_APSIM <- coef(lm(APSIM ~ year, 
                             data = ens_SorghumYld_Mgha[ens_SorghumYld_Mgha$year>=experiment_end_year,]))
      SYfit_Daycent <- coef(lm(Daycent ~ year, 
                               data = ens_SorghumYld_Mgha[ens_SorghumYld_Mgha$year>=experiment_end_year,]))
      
      gSY <- ens_SorghumYld_Mgha_piv[ens_SorghumYld_Mgha_piv$Model %in% c("APSIM","Daycent") &
                                       ens_SorghumYld_Mgha_piv$year>=experiment_end_year,] %>%
        ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
        geom_point(show.legend=TRUE) +
        xlab("Year") +
        ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
        ggtitle(paste(site_name,"Future Sorghum Yield: ",scenario_descriptor)) +
        geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color=APSIM_color) +
        geom_abline(intercept=SYfit_Daycent[1], slope=SYfit_Daycent[2], color=Daycent_color) +
        scale_color_manual(labels=c("APSIM","Daycent"),
                           values=c(APSIM_color,Daycent_color)) +
        theme_classic(base_family = "serif", base_size = 15) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "right",
              legend.key = element_blank())
      
      gSY
    }
    
    #
    CYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_CottonYld_Mgha[ens_CottonYld_Mgha$year>=experiment_end_year,]))
    CYfit_Daycent <- coef(lm(Daycent ~ year, 
                             data = ens_CottonYld_Mgha[ens_CottonYld_Mgha$year>=experiment_end_year,]))
    
    gCY <- ens_CottonYld_Mgha_piv[ens_CottonYld_Mgha_piv$Model %in% c("APSIM","Daycent") &
                                    ens_CottonYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Cotton Yield: ",scenario_descriptor)) +
      geom_abline(intercept=CYfit_APSIM[1], slope=CYfit_APSIM[2], color=APSIM_color) +
      geom_abline(intercept=CYfit_Daycent[1], slope=CYfit_Daycent[2], color=Daycent_color) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gCY
    
    
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
      scale_color_manual(labels=c("APSIM","Daycent","Observed","RothC"),
                         values=c(APSIM_color,Daycent_color,Observed_color,RothC_color)) +
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
    
    
    # gMG <- ens_CH4_cum_kgha_piv %>%
    #   ggplot(aes(x=date, y=ch4_val, color=Model)) +
    #   geom_line(show.legend=TRUE) +
    #   xlab("Year") +
    #   ylab(expression('CH'[4]*' Net Emissions (kg ha ' ^-1*')')) +
    #   ylim(-85,0) +
    #   ggtitle(paste(site_name,expression('CH'[4]*' Emissions: '),scenario_descriptor)) +
    #   scale_color_manual(labels=c("Daycent"),
    #                      values=Daycent_color) +
    #   theme_classic(base_family = "serif", base_size = 15) +
    #   theme(panel.background = element_blank(),
    #         axis.line = element_line(),
    #         legend.position = "right",
    #         legend.key = element_blank())
    # 
    # gMG
    
  } else { # mgmt_scenario_grp not 
    
    # merge observed and modeled data
    ## use ens_ (ensemble) prefix to distinguish these from the "SorghumYld_Mgha" etc.
    ## files in each model's Results code.
    ens_SorghumYld_Mgha <- merge(ObsYield[ObsYield$crop=="Sorghum",c("year","mean_yield")],
                                 APSIMY_Mgha[APSIMY_Mgha$SorghumYield_Mgha != 0,
                                             c("year","SorghumYield_Mgha")],
                                 by="year",
                                 all=TRUE)
    colnames(ens_SorghumYld_Mgha) <- c("year","Observed","APSIM")
    
    ens_SorghumYld_Mgha_piv <- pivot_longer(ens_SorghumYld_Mgha, c(-year),
                                            names_to = "Model",
                                            values_to = "yield_val")
    
    #
    ens_CottonYld_Mgha <- merge(ObsYield[ObsYield$crop=="Cotton",c("year","mean_yield")],
                                APSIMY_Mgha[APSIMY_Mgha$CottonYield_Mgha != 0,
                                            c("year","CottonYield_Mgha")],
                                by="year",
                                all=TRUE)
    colnames(ens_CottonYld_Mgha) <- c("year","Observed","APSIM")
    
    ens_CottonYld_Mgha_piv <- pivot_longer(ens_CottonYld_Mgha, c(-year),
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
    
    
    SYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_SorghumYld_Mgha[ens_SorghumYld_Mgha$year>=experiment_end_year,]))
    
    gSY <- ens_SorghumYld_Mgha_piv[ens_SorghumYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Sorghum Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Sorghum Yield: ",scenario_descriptor)) +
      geom_abline(intercept=SYfit_APSIM[1], slope=SYfit_APSIM[2], color=APSIM_color) +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gSY
    
    #
    CYfit_APSIM <- coef(lm(APSIM ~ year, 
                           data = ens_CottonYld_Mgha[ens_CottonYld_Mgha$year>=experiment_end_year,]))
    
    gCY <- ens_CottonYld_Mgha_piv[ens_CottonYld_Mgha_piv$year>=experiment_end_year,] %>%
      ggplot(aes(x=year, y=yield_val, color=Model, show.legend=TRUE)) +
      geom_point(show.legend=TRUE) +
      xlab("Year") +
      ylab(expression('Cotton Yield (Mg ha' ^-1*')')) +
      ggtitle(paste(site_name,"Future Cotton Yield: ",scenario_descriptor)) +
      geom_abline(intercept=CYfit_APSIM[1], slope=CYfit_APSIM[2], color=APSIM_color) +
      scale_color_manual(labels=c("APSIM","Observed"),
                         values=c(APSIM_color,Daycent_color)) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gCY
    
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
      ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
      ggtitle(paste(site_name,"N2O Emissions: ",scenario_descriptor)) +
      scale_color_manual(labels=c("APSIM"),
                         values=APSIM_color) +
      theme_classic(base_family = "serif", base_size = 15) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "right",
            legend.key = element_blank())
    
    gNG
    
  }
  
  if(mgmt_scenario_grp!=7) {
    ggsave(filename=paste0(results_path,"Ensemble_Sorghum_comparison_",scenario_name,".jpg"),
           plot=gSY, width=9, height=6, dpi=300)
  }
  ggsave(filename=paste0(results_path,"Ensemble_Cotton_comparison_",scenario_name,".jpg"),
         plot=gCY, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Ensemble_SOC_comparison_",scenario_name,".jpg"),
         plot=gC, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"Ensemble_N2O_cum_comparison_",scenario_name,".jpg"),
         plot=gNG, width=9, height=6, dpi=300)
  # if(mgmt_scenario_grp!=6) {
  #   ggsave(filename=paste0(results_path,"Ensemble_CH4_cum_comparison_",scenario_name,".jpg"),
  #          plot=gMG, width=9, height=6, dpi=300)
  # }
  
  rm(calib_summary_raw,calib_summary_df,
     vwc_frctn_atover_fc_APSIM,vwc_frctn_atover_fc_Daycent,
     vwc_frctn_atover_fc_df,
     vwc_frctn_over_5_APSIM,vwc_frctn_over_10_APSIM,vwc_frctn_over_15_APSIM,
     vwc_frctn_under_10_APSIM,vwc_frctn_under_11_APSIM,vwc_frctn_under_12_APSIM,
     vwc_frctn_over_5_Daycent,vwc_frctn_over_10_Daycent,vwc_frctn_over_15_Daycent,
     vwc_frctn_under_10_Daycent,vwc_frctn_under_11_Daycent,vwc_frctn_under_12_Daycent,
     vwc_frctn_under_10_df)
  
}) # end suppressMessages

