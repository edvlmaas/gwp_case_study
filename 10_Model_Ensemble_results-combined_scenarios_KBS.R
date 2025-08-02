#*****************************************************************
# File: 10_Model_Ensemble_Results-combined_scenarios_KBS
# Author: Ellen Maas
# Date: 8/30/2022
# Description: Assembles individual model runs and calculates
# annual and daily mean values, then produces a variety of
# multi-model graphs by scenario, or combined. Includes 
# calibration results the global warming potential as well.
#*****************************************************************

suppressMessages({
  
print(paste0("Starting 10_Model_Ensemble_results-combined_scenarios_",site_name,".R"))

library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)

source("f_model_coef.R")


  #*************************************************************
  # Calibration
  
  # Import calibration data -------------------------------------------------
  
  crop_calib_output_df_piv <- read.table(file=paste0(results_path,"calib_crop_df_piv.csv"),
                                         header=TRUE,sep=",") %>%
    left_join(unique(scenario_df[,c("scenario_descriptor","scenario_abbrev")]),
              by=c("treatment_scen"="scenario_descriptor"))
  crop_calib_output_df_piv$Source <- crop_calib_output_df_piv$Model
  
  ### calculate range of yield values as mean yield (in crop_calib_output_df_piv) and
  ### add +- Obs_sd as proxy for using all observed repetitions
  crop_calib_output_df_piv_withsd1 <- crop_calib_output_df_piv[crop_calib_output_df_piv$Model=="Observed",] %>%
    mutate(yield_val = yield_val + Obs_sd)
  crop_calib_output_df_piv_withsd2 <- crop_calib_output_df_piv[crop_calib_output_df_piv$Model=="Observed",] %>%
    mutate(yield_val = yield_val - Obs_sd)
  crop_calib_output_df_piv_withsd <- rbind(crop_calib_output_df_piv_withsd1,crop_calib_output_df_piv_withsd2) %>%
    rbind(crop_calib_output_df_piv[crop_calib_output_df_piv$Model!="Observed",])

  ### add in an "ensemble" entry that includes all model output with "Ensemble" as the model name
  crop_ens <- crop_calib_output_df_piv[crop_calib_output_df_piv$Model!="Observed",] %>%
    mutate(Model="Ensemble",
           Source="Ensemble")
  crop_calib_output_df_piv_withens <- rbind(crop_calib_output_df_piv_withsd,
                                            crop_ens)
  
  #### make crop version that includes all replicate measurements
  crop_reps_piv <- ObsYield_raw[ObsYield_raw$Treatment %in% c("T1","T2","T3"),] %>%
    mutate(year=Year,
           crop=case_when(Crop=="Zea mays L. (*)" ~ "Maize",
                                         Crop=="Glycine max L. (*)" ~ "Soybean",
                                         Crop=="Triticum aestivum L. (*)" ~ "Wheat",
                                         .default = Crop),
           yield_val=crop_only_yield_kg_ha/1000,
           Model="Observed",
           Source="Observed",
           treatment_scen=case_when(Treatment=="T1" ~ "Rmv Resid 0%, Crop Rotation",
                                    Treatment=="T2" ~ "Rmv Resid 0%, No Till, Crop Rotation",
                                    Treatment=="T3" ~ "Cover Crop, Crop Rotation",
                                    .default=Treatment),
           scenario_abbrev=case_when(Treatment=="T1" ~ "RR00-CR",
                                     Treatment=="T2" ~ "RR00-NT-CR",
                                     Treatment=="T3" ~ "CC-CR",
                                     .default=Treatment),
           Obs_sd=NA) %>%
    select(year,Obs_sd,crop,treatment_scen,Model,yield_val,scenario_abbrev,Source)
  crop_reps_output_df_piv_withens <- rbind(crop_calib_output_df_piv_withens[crop_calib_output_df_piv_withens$Model!="Observed" &
                                                                              crop_calib_output_df_piv$year %in%
                                                                              ObsYield_raw$Year,],
                                            crop_reps_piv)
  
  
  ## SOC
  
  soc_calib_output_df_piv <- read.table(file=paste0(results_path,"calib_soc_df_piv.csv"),
                                        header=TRUE,sep=",") %>%
    left_join(unique(scenario_df[,c("scenario_descriptor","scenario_abbrev")]),
              by=c("treatment_scen"="scenario_descriptor"))
  soc_calib_output_df_piv$Source <- soc_calib_output_df_piv$Model
  
  ### calculate range of yield values as +- Obs_sd (proxy for using all obs reps),
  ### but don't include outliers in recalculation: leave as-is
  soc_calib_output_df_piv_withsd1 <- soc_calib_output_df_piv[soc_calib_output_df_piv$Model=="Observed" &
                                                               !(soc_calib_output_df_piv$year %in% c(1993,1998)),] %>%
    mutate(C_val = C_val + Obs_sd)
  soc_calib_output_df_piv_withsd2 <- soc_calib_output_df_piv[soc_calib_output_df_piv$Model=="Observed" &
                                                               !(soc_calib_output_df_piv$year %in% c(1993,1998)),] %>%
    mutate(C_val = C_val - Obs_sd)
  soc_calib_output_df_piv_withsd <- rbind(soc_calib_output_df_piv_withsd1,soc_calib_output_df_piv_withsd2) %>%
    rbind(soc_calib_output_df_piv[soc_calib_output_df_piv$Model!="Observed" |
                                    (soc_calib_output_df_piv$Model=="Observed" &
                                       soc_calib_output_df_piv$year %in% c(1993,1998)),])
 
  ### add in an "ensemble" entry that includes all model output with "Ensemble" as the model name
  soc_ens <- soc_calib_output_df_piv[soc_calib_output_df_piv$Model!="Observed",] %>%
    mutate(Model="Ensemble",
           Source="Ensemble")
  soc_calib_output_df_piv_withens <- rbind(soc_calib_output_df_piv_withsd,
                                            soc_ens)
  
  #### version with only the mean observations
  soc_calib_output_df_piv_withens2 <- rbind(soc_calib_output_df_piv,
                                           soc_ens)

  #### version that includes all replicate measurements
  soc_reps <- ObsC_pct %>%
    mutate(cstock_byrep=mean_c*ObsBD$mean_BD*25)
  
  soc_reps_piv <- soc_reps[soc_reps$treatment %in% 1:3,] %>%
    mutate(Model="Observed",
           Source="Observed",
           treatment_scen=case_when(treatment==1 ~ "Rmv Resid 0%, Crop Rotation",
                                    treatment==2 ~ "Rmv Resid 0%, No Till, Crop Rotation",
                                    treatment==3 ~ "Cover Crop, Crop Rotation",
                                    .default=treatment),
           scenario_abbrev=case_when(treatment==1 ~ "RR00-CR",
                                     treatment==2 ~ "RR00-NT-CR",
                                     treatment==3 ~ "CC-CR",
                                     .default=treatment),
           Obs_sd=NA,
           C_val=cstock_byrep) %>%
    select(year,Obs_sd,treatment_scen,Model,C_val,scenario_abbrev,Source)
  soc_reps_output_df_piv_withens <- rbind(soc_calib_output_df_piv_withens[soc_calib_output_df_piv_withens$Model!="Observed" &
                                                                            soc_calib_output_df_piv_withens$year %in%
                                                                            min(soc_reps$year):max(soc_reps$year)
                                                                          ,],
                                           soc_reps_piv)
  
    
  soc_trendlines_df <- read.table(file=paste0(results_path,"calib_soc_trendline_piv.csv"),
                                  header=TRUE,sep=",") %>%
    left_join(unique(scenario_df[,c("scenario_descriptor","scenario_abbrev")]),
              by=c("treatment_scen"="scenario_descriptor"))
  soc_trendlines_df$Source <- soc_trendlines_df$Model
  soc_trendlines_df_modified <- soc_trendlines_df[,c("year","scenario_abbrev",
                                                     "Fit","value","Source")]
  colnames(soc_trendlines_df_modified) <- c("year","scenario_abbrev","Fit",
                                            "value","Source")


  # ## N2O
  # 
  # n2o_calib_output_df_piv <- read.table(file=paste0(results_path,"calib_n2o_df_piv.csv"),
  #                                       header=TRUE,sep=",") %>%
  #   left_join(unique(scenario_df[,c("scenario_descriptor","scenario_abbrev")]),
  #             by=c("treatment_scen"="scenario_descriptor"))
  # 
  # 
  # ## CH4
  # 
  # ch4_calib_output_df_piv <- read.table(file=paste0(results_path,"calib_ch4_df_piv.csv"),
  #                                       header=TRUE,sep=",") %>%
  #   left_join(unique(scenario_df[,c("scenario_descriptor","scenario_abbrev")]),
  #             by=c("treatment_scen"="scenario_descriptor"))
  
  # Calibration time series graphs ------------------------------------------------------
  
  gY_calib <- crop_calib_output_df_piv[crop_calib_output_df_piv$year %in% experiment_year_range,] %>%
    ggplot(aes(x=year, y=yield_val, color=Source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    ylim(0,13) +
    ggtitle(paste(site_name,"Crop Yield Calibration")) +
    geom_errorbar(aes(ymin=yield_val-Obs_sd, ymax=yield_val+Obs_sd),
                  width=1) + # Width of the error bars
    geom_smooth(method='lm', formula= y~x, se=FALSE, aes(colour = Model)
                ,linewidth=0.75) +
    scale_color_manual(labels=c("APSIM","Daycent","Observed"),
                       values=cbPalette9[c(8,2,1)]) +
    facet_grid(crop~scenario_abbrev) +
    theme_classic(base_family = "serif", base_size = 22) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "right",
          legend.key = element_blank())
  
  gY_calib
  
  
  gSOC_calib <- soc_calib_output_df_piv[soc_calib_output_df_piv$year %in% experiment_year_range &
                                          soc_calib_output_df_piv$Source!="Millennial",] %>%
    ggplot(aes(x=year, y=C_val, color=Source, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('SOC (Mg ha ' ^-1*')')) +
    ggtitle(paste(site_name,"Soil Organic Carbon Calibration")) +
    geom_errorbar(aes(ymin=C_val-Obs_sd, ymax=C_val+Obs_sd),
                  width=1.5) +  # Width of the error bars
    stat_smooth(
      data = soc_trendlines_df_modified, 
      aes(linetype = Fit, y=value), 
      #color="black", 
      method = lm,
      se = FALSE,
      show.legend = TRUE,
      fullrange=TRUE
    ) +
    scale_color_manual(labels=c("APSIM","Daycent","Observed","RothC"),
                       values=cbPalette9[c(8,2,1,3)]) +
    facet_wrap(~scenario_abbrev,nrow=1) +
    theme_classic(base_family = "serif", base_size = 24) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gSOC_calib
  
  ggsave(filename=paste0(results_path,"pub_Crop_yield_calibration.jpg"),
         plot=gY_calib, width=9, height=7, dpi=300)
  ggsave(filename=paste0(results_path,"pub_SOC_calibration.jpg"),
         plot=gSOC_calib, width=14, height=7, dpi=300)
  
  ## Calibration box plots ------------------------------------------------------
  
  gY1_box <- crop_calib_output_df_piv_withsd %>%
    ggplot(aes(x=Model,y=yield_val)) +
    stat_boxplot(geom = "errorbar",width=.5) + 
    geom_boxplot() +
    xlab("") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    facet_wrap(~crop,nrow=1) +
    theme_classic(base_family = "serif", base_size = 24) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gY1_box
  
  # version that only uses the mean yield of observations,
  # since that is a 1:1 comparison to the models, which
  # only produce one value per year
  gY1b_box <- crop_calib_output_df_piv %>%
    ggplot(aes(x=Model,y=yield_val)) +
    stat_boxplot(geom = "errorbar",width=.5) + 
    geom_boxplot() +
    xlab("") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    facet_wrap(~crop,nrow=1) +
    theme_classic(base_family = "serif", base_size = 24) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())
  
  gY1b_box

  # just models and observations
    gY2_box <- crop_calib_output_df_piv_withsd %>%
    ggplot(aes(x=Model,y=yield_val)) +
    stat_boxplot(geom = "errorbar",width=.5) + 
    geom_boxplot() +
    xlab("") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    facet_grid(crop~scenario_abbrev) +
    theme_classic(base_family = "serif", base_size = 30) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  gY2_box
  
  # version that includes Ensemble
  gY2b_box <- crop_calib_output_df_piv_withens %>%
    ggplot(aes(x=Model,y=yield_val)) +
    stat_boxplot(geom = "errorbar",width=.5) + 
    geom_boxplot() +
    xlab("") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    scale_y_continuous(breaks=c(0,5,10),
                       limits=c(0,13)) +
    facet_grid(crop~scenario_abbrev) +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  gY2b_box
  
  # version that includes crop observed reps and Ensemble
  
  crop_reps_withens_counts <- crop_reps_output_df_piv_withens %>%
    group_by(Model,scenario_abbrev,crop) %>%
    summarize(n_count=n())
  
  write.csv(crop_reps_withens_counts,paste0(results_path,"calib_crop_boxplot_counts.csv"))
  
  g_text <- as.data.frame(crop_reps_withens_counts[,c("scenario_abbrev","crop","Model","n_count")] %>%
    mutate(lab=paste0("n=",n_count)) %>%
    select(scenario_abbrev,crop,Model,lab))
  
  gY2c_box <- crop_reps_output_df_piv_withens %>%
    ggplot(aes(x=Model,y=yield_val)) +
    stat_boxplot(geom = "errorbar",width=.5) + 
    geom_boxplot() +
    xlab("") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    scale_y_continuous(breaks=c(0,5,10,15),
                       limits=c(0,15)) +
    geom_text(data=g_text, ,
              mapping = aes(x = Model, y = 14, label = lab)) +
    facet_grid(crop~scenario_abbrev) +
    theme_classic(base_family = "serif", base_size = 30) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  gY2c_box
  
  # get stats from boxplots - based on Y2_box
  Y2box_bytreat <- ggplot_build(gY2_box)[[1]][[1]]
  ## identify which rows in data are which crops
  Y2box_corn_Obs_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 0 &
                                   as.numeric(rownames(Y2box_bytreat)) <= 9  )
  Y2box_corn_APSIM_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 1 &
                                     as.numeric(rownames(Y2box_bytreat)) <= 9 )
  Y2box_corn_Daycent_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 2 &
                                       as.numeric(rownames(Y2box_bytreat)) <= 9 )
  Y2box_soybean_Obs_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 0 &
                                    as.numeric(rownames(Y2box_bytreat)) > 9 &
                                    as.numeric(rownames(Y2box_bytreat)) <= 18 )
  Y2box_soybean_APSIM_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 1 &
                                      as.numeric(rownames(Y2box_bytreat)) > 9 &
                                      as.numeric(rownames(Y2box_bytreat)) <= 18 )
  Y2box_soybean_Daycent_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 2 &
                                        as.numeric(rownames(Y2box_bytreat)) > 9 &
                                        as.numeric(rownames(Y2box_bytreat)) <= 18 )
  Y2box_wheat_Obs_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 0 &
                                    as.numeric(rownames(Y2box_bytreat)) > 18  )
  Y2box_wheat_APSIM_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 1 &
                                      as.numeric(rownames(Y2box_bytreat)) > 18 )
  Y2box_wheat_Daycent_rows <- which(as.numeric(rownames(Y2box_bytreat)) %% 3 == 2 &
                                        as.numeric(rownames(Y2box_bytreat)) > 18 )
  # calculate difference in medians ("middle") between models and Observed
  Y2box_corn_APSIM_Obs_med_diff <- Y2box_bytreat[Y2box_corn_APSIM_rows,"middle"] - 
    Y2box_bytreat[Y2box_corn_Obs_rows,"middle"]
  Y2box_corn_Daycent_Obs_med_diff <- Y2box_bytreat[Y2box_corn_Daycent_rows,"middle"] -
    Y2box_bytreat[Y2box_corn_Obs_rows,"middle"]
  Y2box_soybean_APSIM_Obs_med_diff <- Y2box_bytreat[Y2box_soybean_APSIM_rows,"middle"] - 
    Y2box_bytreat[Y2box_soybean_Obs_rows,"middle"]
  Y2box_soybean_Daycent_Obs_med_diff <- Y2box_bytreat[Y2box_soybean_Daycent_rows,"middle"] -
    Y2box_bytreat[Y2box_soybean_Obs_rows,"middle"]
  Y2box_wheat_APSIM_Obs_med_diff <- Y2box_bytreat[Y2box_wheat_APSIM_rows,"middle"] - 
    Y2box_bytreat[Y2box_wheat_Obs_rows,"middle"]
  Y2box_wheat_Daycent_Obs_med_diff <- Y2box_bytreat[Y2box_wheat_Daycent_rows,"middle"] -
    Y2box_bytreat[Y2box_wheat_Obs_rows,"middle"]
  # calculate mean medians between APSIM and Daycent
  Y2box_corn_model_mean_medians <- rowMeans(cbind(Y2box_bytreat[Y2box_corn_APSIM_rows,"middle"],
                                       Y2box_bytreat[Y2box_corn_Daycent_rows,"middle"]))
  Y2box_corn_Obs_medians <- Y2box_bytreat[Y2box_corn_Obs_rows,"middle"]
  Y2box_corn_median_diffs <- Y2box_corn_Obs_medians - Y2box_corn_model_mean_medians
  Y2box_soybean_model_mean_medians <- rowMeans(cbind(Y2box_bytreat[Y2box_soybean_APSIM_rows,"middle"],
                                                  Y2box_bytreat[Y2box_soybean_Daycent_rows,"middle"]))
  Y2box_soybean_Obs_medians <- Y2box_bytreat[Y2box_soybean_Obs_rows,"middle"]
  Y2box_soybean_median_diffs <- Y2box_soybean_Obs_medians - Y2box_soybean_model_mean_medians
  Y2box_wheat_model_mean_medians <- rowMeans(cbind(Y2box_bytreat[Y2box_wheat_APSIM_rows,"middle"],
                                                  Y2box_bytreat[Y2box_wheat_Daycent_rows,"middle"]))
  Y2box_wheat_Obs_medians <- Y2box_bytreat[Y2box_wheat_Obs_rows,"middle"]
  Y2box_wheat_median_diffs <- Y2box_wheat_Obs_medians - Y2box_wheat_model_mean_medians
  
  # for paper: range of the differences across all calibration mgmt scenarios and
  # medians
  Y2box_corn_APSIM_Obs_med_diff_range <- round(range(Y2box_corn_APSIM_Obs_med_diff),2)
  Y2box_corn_Daycent_Obs_med_diff_range <- round(range(Y2box_corn_Daycent_Obs_med_diff),2)
  Y2box_soybean_APSIM_Obs_med_diff_range <- round(range(Y2box_soybean_APSIM_Obs_med_diff),2)
  Y2box_soybean_Daycent_Obs_med_diff_range <- round(range(Y2box_soybean_Daycent_Obs_med_diff),2)
  Y2box_wheat_APSIM_Obs_med_diff_range <- round(range(Y2box_wheat_APSIM_Obs_med_diff),2)
  Y2box_wheat_Daycent_Obs_med_diff_range <- round(range(Y2box_wheat_Daycent_Obs_med_diff),2)
  Y2box_crop_medians_df <- round(data.frame(APSIM_corn_med=Y2box_bytreat[Y2box_corn_APSIM_rows,"middle"],
                                      Daycent_corn_med=Y2box_bytreat[Y2box_corn_Daycent_rows,"middle"],
                                      Model_corn_med=Y2box_corn_model_mean_medians,
                                      Obs_corn_med=Y2box_corn_Obs_medians,
                                      APSIM_soybean_med=Y2box_bytreat[Y2box_soybean_APSIM_rows,"middle"],
                                      Daycent_soybean_med=Y2box_bytreat[Y2box_soybean_Daycent_rows,"middle"],
                                      Model_soybean_med=Y2box_soybean_model_mean_medians,
                                      Obs_soybean_med=Y2box_soybean_Obs_medians,
                                      APSIM_wheat_med=Y2box_bytreat[Y2box_wheat_APSIM_rows,"middle"],
                                      Daycent_wheat_med=Y2box_bytreat[Y2box_wheat_Daycent_rows,"middle"],
                                      Model_wheat_med=Y2box_wheat_model_mean_medians,
                                      Obs_wheat_med=Y2box_wheat_Obs_medians),1) %>%
    mutate(scenario=c("CC-CR","CR","NT-CR"),
           site=site_name)
  
  write.csv(Y2box_crop_medians_df,paste0(results_path,"calib_crop_medians.csv"))
  
  # get stats from boxplots - based on Y2c_box -----------------
  
  Y2cbox_bytreat <- ggplot_build(gY2c_box)[[1]][[1]]
  ## identify which rows in data are which crops
  Y2cbox_corn_APSIM_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 1 &
                                    as.numeric(rownames(Y2cbox_bytreat)) <= 12 )
  Y2cbox_corn_Daycent_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 2 &
                                      as.numeric(rownames(Y2cbox_bytreat)) <= 12 )
  Y2cbox_corn_Ensemble_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 3 &
                                       as.numeric(rownames(Y2cbox_bytreat)) <= 12 )
  Y2cbox_corn_Obs_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 0 &
                                  as.numeric(rownames(Y2cbox_bytreat)) <= 12  )
  
  Y2cbox_soybean_APSIM_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 1 &
                                       as.numeric(rownames(Y2cbox_bytreat)) > 12 &
                                       as.numeric(rownames(Y2cbox_bytreat)) <= 24 )
  Y2cbox_soybean_Daycent_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 2 &
                                         as.numeric(rownames(Y2cbox_bytreat)) > 12 &
                                         as.numeric(rownames(Y2cbox_bytreat)) <= 24 )
  Y2cbox_soybean_Ensemble_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 3 &
                                          as.numeric(rownames(Y2cbox_bytreat)) > 12 &
                                          as.numeric(rownames(Y2cbox_bytreat)) <= 24 )
  Y2cbox_soybean_Obs_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 0 &
                                     as.numeric(rownames(Y2cbox_bytreat)) > 12 &
                                     as.numeric(rownames(Y2cbox_bytreat)) <= 24 )
  
  Y2cbox_wheat_APSIM_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 1 &
                                     as.numeric(rownames(Y2cbox_bytreat)) > 24 )
  Y2cbox_wheat_Daycent_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 2 &
                                       as.numeric(rownames(Y2cbox_bytreat)) > 24 )
  Y2cbox_wheat_Ensemble_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 3 &
                                        as.numeric(rownames(Y2cbox_bytreat)) > 24  )
  Y2cbox_wheat_Obs_rows <- which(as.numeric(rownames(Y2cbox_bytreat)) %% 4 == 0 &
                                   as.numeric(rownames(Y2cbox_bytreat)) > 24  )
  
  # calculate medians of models and observed
  Y2cbox_corn_APSIM_medians <- Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"middle"]
  Y2cbox_corn_Daycent_medians <- Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"middle"]
  Y2cbox_corn_Ensemble_medians <- Y2cbox_bytreat[Y2cbox_corn_Ensemble_rows,"middle"]
  Y2cbox_corn_Obs_medians <- Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"middle"]
  
  Y2cbox_soybean_APSIM_medians <- Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows,"middle"]
  Y2cbox_soybean_Daycent_medians <- Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows,"middle"]
  Y2cbox_soybean_Ensemble_medians <- Y2cbox_bytreat[Y2cbox_soybean_Ensemble_rows,"middle"]
  Y2cbox_soybean_Obs_medians <- Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"middle"]
  
  Y2cbox_wheat_APSIM_medians <- Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows,"middle"]
  Y2cbox_wheat_Daycent_medians <- Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows,"middle"]
  Y2cbox_wheat_Ensemble_medians <- Y2cbox_bytreat[Y2cbox_wheat_Ensemble_rows,"middle"]
  Y2cbox_wheat_Obs_medians <- Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"middle"]
  
    # calculate difference in medians ("middle") between models and Observed
  ## use absolute value of differences
  Y2cbox_corn_APSIM_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"middle"] - 
    Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"middle"])
  Y2cbox_corn_APSIM_Obs_mean_med_diff <- mean(Y2cbox_corn_APSIM_Obs_med_diff)

  Y2cbox_corn_Daycent_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"middle"] -
    Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"middle"])
  Y2cbox_corn_Daycent_Obs_mean_med_diff <- mean(Y2cbox_corn_Daycent_Obs_med_diff)

  Y2cbox_corn_Ensemble_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_corn_Ensemble_rows,"middle"] -
    Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"middle"])
  Y2cbox_corn_Ensemble_Obs_mean_med_diff <- mean(Y2cbox_corn_Ensemble_Obs_med_diff)
  #-  
  Y2cbox_soybean_APSIM_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows,"middle"] - 
    Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"middle"])
  Y2cbox_soybean_APSIM_Obs_mean_med_diff <- mean(Y2cbox_soybean_APSIM_Obs_med_diff)
  
  Y2cbox_soybean_Daycent_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows,"middle"] -
    Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"middle"])
  Y2cbox_soybean_Daycent_Obs_mean_med_diff <- mean(Y2cbox_soybean_Daycent_Obs_med_diff)
  
  Y2cbox_soybean_Ensemble_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_soybean_Ensemble_rows,"middle"] -
    Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"middle"])
  Y2cbox_soybean_Ensemble_Obs_mean_med_diff <- mean(Y2cbox_soybean_Ensemble_Obs_med_diff)
  #-
  Y2cbox_wheat_APSIM_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows,"middle"] - 
    Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"middle"])
  Y2cbox_wheat_APSIM_Obs_mean_med_diff <- mean(Y2cbox_wheat_APSIM_Obs_med_diff)
  
  Y2cbox_wheat_Daycent_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows,"middle"] -
    Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"middle"])
  Y2cbox_wheat_Daycent_Obs_mean_med_diff <- mean(Y2cbox_wheat_Daycent_Obs_med_diff)
  
  Y2cbox_wheat_Ensemble_Obs_med_diff <- abs(Y2cbox_bytreat[Y2cbox_wheat_Ensemble_rows,"middle"] -
    Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"middle"])
  Y2cbox_wheat_Ensemble_Obs_mean_med_diff <- mean(Y2cbox_wheat_Ensemble_Obs_med_diff)
  
  
  # calculate IQRs
  Y2cbox_corn_Obs_iqrs <- Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"lower"]
  Y2cbox_corn_Obs_max_iqr <- max(Y2cbox_corn_Obs_iqrs)
  
  Y2cbox_corn_APSIM_iqrs <- Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"lower"]
  Y2cbox_corn_APSIM_max_iqr <- max(Y2cbox_corn_APSIM_iqrs)
  Y2cbox_corn_APSIM_Obs_pct_iqrs <- Y2cbox_corn_APSIM_iqrs/Y2cbox_corn_Obs_iqrs*100
  # this is the mean APSIM IQR as a % of Obs:
  Y2cbox_corn_APSIM_Obs_mean_pct_iqr <- mean(Y2cbox_corn_APSIM_Obs_pct_iqrs)

  Y2cbox_corn_Daycent_iqrs <- Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"lower"]
  Y2cbox_corn_Daycent_max_iqr <- max(Y2cbox_corn_Daycent_iqrs)
  Y2cbox_corn_Daycent_Obs_pct_iqrs <- Y2cbox_corn_Daycent_iqrs/Y2cbox_corn_Obs_iqrs*100
  # this is the mean Daycent IQR as a % of Obs:
  Y2cbox_corn_Daycent_Obs_mean_pct_iqr <- mean(Y2cbox_corn_Daycent_Obs_pct_iqrs)
  
  Y2cbox_corn_Ensemble_iqrs <- Y2cbox_bytreat[Y2cbox_corn_Ensemble_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_corn_Ensemble_rows,"lower"]
  Y2cbox_corn_Ensemble_max_iqr <- max(Y2cbox_corn_Ensemble_iqrs)
  Y2cbox_corn_Ensemble_Obs_pct_iqrs <- Y2cbox_corn_Ensemble_iqrs/Y2cbox_corn_Obs_iqrs*100
  # this is the mean Ensemble IQR as a % of Obs:
  Y2cbox_corn_Ensemble_Obs_mean_pct_iqr <- mean(Y2cbox_corn_Ensemble_Obs_pct_iqrs)
  
  #-
  
  Y2cbox_soybean_Obs_iqrs <- Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"lower"]
  Y2cbox_soybean_Obs_max_iqr <- max(Y2cbox_soybean_Obs_iqrs)
  
  Y2cbox_soybean_APSIM_iqrs <- Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows,"lower"]
  Y2cbox_soybean_APSIM_max_iqr <- max(Y2cbox_soybean_APSIM_iqrs)
  Y2cbox_soybean_APSIM_Obs_pct_iqrs <- Y2cbox_soybean_APSIM_iqrs/Y2cbox_soybean_Obs_iqrs*100
  # this is the mean APSIM IQR as a % of Obs:
  Y2cbox_soybean_APSIM_Obs_mean_pct_iqr <- mean(Y2cbox_soybean_APSIM_Obs_pct_iqrs)
  
  Y2cbox_soybean_Daycent_iqrs <- Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows,"lower"]
  Y2cbox_soybean_Daycent_max_iqr <- max(Y2cbox_soybean_Daycent_iqrs)
  Y2cbox_soybean_Daycent_Obs_pct_iqrs <- Y2cbox_soybean_Daycent_iqrs/Y2cbox_soybean_Obs_iqrs*100
  # this is the mean Daycent IQR as a % of Obs:
  Y2cbox_soybean_Daycent_Obs_mean_pct_iqr <- mean(Y2cbox_soybean_Daycent_Obs_pct_iqrs)
  
  Y2cbox_soybean_Ensemble_iqrs <- Y2cbox_bytreat[Y2cbox_soybean_Ensemble_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_soybean_Ensemble_rows,"lower"]
  Y2cbox_soybean_Ensemble_max_iqr <- max(Y2cbox_soybean_Ensemble_iqrs)
  Y2cbox_soybean_Ensemble_Obs_pct_iqrs <- Y2cbox_soybean_Ensemble_iqrs/Y2cbox_soybean_Obs_iqrs*100
  # this is the mean Ensemble IQR as a % of Obs:
  Y2cbox_soybean_Ensemble_Obs_mean_pct_iqr <- mean(Y2cbox_soybean_Ensemble_Obs_pct_iqrs)
  
  #-
  
  Y2cbox_wheat_Obs_iqrs <- Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"lower"]
  Y2cbox_wheat_Obs_max_iqr <- max(Y2cbox_wheat_Obs_iqrs)
  
  Y2cbox_wheat_APSIM_iqrs <- Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows,"lower"]
  Y2cbox_wheat_APSIM_max_iqr <- max(Y2cbox_wheat_APSIM_iqrs)
  Y2cbox_wheat_APSIM_Obs_pct_iqrs <- Y2cbox_wheat_APSIM_iqrs/Y2cbox_wheat_Obs_iqrs*100
  # this is the mean APSIM IQR as a % of Obs:
  Y2cbox_wheat_APSIM_Obs_mean_pct_iqr <- mean(Y2cbox_wheat_APSIM_Obs_pct_iqrs)
  
  Y2cbox_wheat_Daycent_iqrs <- Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows,"lower"]
  Y2cbox_wheat_Daycent_max_iqr <- max(Y2cbox_wheat_Daycent_iqrs)
  Y2cbox_wheat_Daycent_Obs_pct_iqrs <- Y2cbox_wheat_Daycent_iqrs/Y2cbox_wheat_Obs_iqrs*100
  # this is the mean Daycent IQR as a % of Obs:
  Y2cbox_wheat_Daycent_Obs_mean_pct_iqr <- mean(Y2cbox_wheat_Daycent_Obs_pct_iqrs)
  
  Y2cbox_wheat_Ensemble_iqrs <- Y2cbox_bytreat[Y2cbox_wheat_Ensemble_rows,"upper"] - 
    Y2cbox_bytreat[Y2cbox_wheat_Ensemble_rows,"lower"]
  Y2cbox_wheat_Ensemble_max_iqr <- max(Y2cbox_wheat_Ensemble_iqrs)
  Y2cbox_wheat_Ensemble_Obs_pct_iqrs <- Y2cbox_wheat_Ensemble_iqrs/Y2cbox_wheat_Obs_iqrs*100
  # this is the mean Ensemble IQR as a % of Obs:
  Y2cbox_wheat_Ensemble_Obs_mean_pct_iqr <- mean(Y2cbox_wheat_Ensemble_Obs_pct_iqrs)
  
  
  # calculate ranges and % of Obs
  Y2cbox_corn_Obs_ranges <- Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"ymin"]
  
  Y2cbox_corn_APSIM_ranges <- Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"ymin"]
  Y2cbox_corn_APSIM_Obs_pct_ranges <- Y2cbox_corn_APSIM_ranges/Y2cbox_corn_Obs_ranges*100
  # this is the mean APSIM range as a % of Obs:
  Y2cbox_corn_APSIM_Obs_mean_pct_range <- mean(Y2cbox_corn_APSIM_Obs_pct_ranges)
  
  Y2cbox_corn_Daycent_ranges <- Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"ymin"]
  Y2cbox_corn_Daycent_Obs_pct_ranges <- Y2cbox_corn_Daycent_ranges/Y2cbox_corn_Obs_ranges*100
  # this is the mean Daycent range as a % of Obs:
  Y2cbox_corn_Daycent_Obs_mean_pct_range <- mean(Y2cbox_corn_Daycent_Obs_pct_ranges)
  
  Y2cbox_corn_Ensemble_CCCR_range <- max(Y2cbox_bytreat[Y2cbox_corn_APSIM_rows[1],"ymax"],
                                     Y2cbox_bytreat[Y2cbox_corn_Daycent_rows[1],"ymax"]) - 
    min(Y2cbox_bytreat[Y2cbox_corn_APSIM_rows[1],"ymin"],
        Y2cbox_bytreat[Y2cbox_corn_Daycent_rows[1],"ymin"])
  Y2cbox_corn_Ensemble_CR_range <- Y2cbox_bytreat[Y2cbox_corn_Ensemble_rows[2],"ymax"] - 
    Y2cbox_bytreat[Y2cbox_corn_Ensemble_rows[2],"ymin"]
  Y2cbox_corn_Ensemble_NTCR_range <- max(Y2cbox_bytreat[Y2cbox_corn_APSIM_rows[3],"ymax"],
                                          Y2cbox_bytreat[Y2cbox_corn_Daycent_rows[3],"ymax"]) - 
    min(Y2cbox_bytreat[Y2cbox_corn_APSIM_rows[3],"ymin"],
        Y2cbox_bytreat[Y2cbox_corn_Daycent_rows[3],"ymin"])
  Y2cbox_corn_Ensemble_ranges <- c(Y2cbox_corn_Ensemble_CCCR_range,Y2cbox_corn_Ensemble_CR_range,
                                   Y2cbox_corn_Ensemble_NTCR_range)
  
  Y2cbox_corn_Ensemble_Obs_pct_ranges <- Y2cbox_corn_Ensemble_ranges/Y2cbox_corn_Obs_ranges*100
  # this is the mean Ensemble range as a % of Obs:
  Y2cbox_corn_Ensemble_Obs_mean_pct_range <- mean(Y2cbox_corn_Ensemble_Obs_pct_ranges)
  
  #-
  
  Y2cbox_soybean_Obs_ranges <- Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"ymin"]
  
  Y2cbox_soybean_APSIM_ranges <- Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows,"ymin"]
  Y2cbox_soybean_APSIM_Obs_pct_ranges <- Y2cbox_soybean_APSIM_ranges/Y2cbox_soybean_Obs_ranges*100
  # this is the mean APSIM range as a % of Obs:
  Y2cbox_soybean_APSIM_Obs_mean_pct_range <- mean(Y2cbox_soybean_APSIM_Obs_pct_ranges)
  
  Y2cbox_soybean_Daycent_ranges <- Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows,"ymin"]
  Y2cbox_soybean_Daycent_Obs_pct_ranges <- Y2cbox_soybean_Daycent_ranges/Y2cbox_soybean_Obs_ranges*100
  # this is the mean Daycent range as a % of Obs:
  Y2cbox_soybean_Daycent_Obs_mean_pct_range <- mean(Y2cbox_soybean_Daycent_Obs_pct_ranges)
  
  Y2cbox_soybean_Ensemble_CCCR_range <- max(Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows[1],"ymax"],
                                         Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows[1],"ymax"]) - 
    min(Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows[1],"ymin"],
        Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows[1],"ymin"])
  Y2cbox_soybean_Ensemble_CR_range <- max(Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows[2],"ymax"],
                                          Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows[2],"ymax"]) - 
    min(Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows[2],"ymin"],
        Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows[2],"ymin"])
  Y2cbox_soybean_Ensemble_NTCR_range <- max(Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows[3],"ymax"],
                                         Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows[3],"ymax"]) - 
    min(Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows[3],"ymin"],
        Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows[3],"ymin"])
  Y2cbox_soybean_Ensemble_ranges <- c(Y2cbox_soybean_Ensemble_CCCR_range,Y2cbox_soybean_Ensemble_CR_range,
                                   Y2cbox_soybean_Ensemble_NTCR_range)
  
  Y2cbox_soybean_Ensemble_Obs_pct_ranges <- Y2cbox_soybean_Ensemble_ranges/Y2cbox_soybean_Obs_ranges*100
  # this is the mean Ensemble range as a % of Obs:
  Y2cbox_soybean_Ensemble_Obs_mean_pct_range <- mean(Y2cbox_soybean_Ensemble_Obs_pct_ranges)
  
  #-
  
  Y2cbox_wheat_Obs_ranges <- Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"ymin"] 
  
  Y2cbox_wheat_APSIM_ranges <- Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows,"ymin"]
  Y2cbox_wheat_APSIM_Obs_pct_ranges <- Y2cbox_wheat_APSIM_ranges/Y2cbox_wheat_Obs_ranges*100
  # this is the mean APSIM range as a % of Obs:
  Y2cbox_wheat_APSIM_Obs_mean_pct_range <- mean(Y2cbox_wheat_APSIM_Obs_pct_ranges)
  
  Y2cbox_wheat_Daycent_ranges <- Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows,"ymax"] - 
    Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows,"ymin"]
  Y2cbox_wheat_Daycent_Obs_pct_ranges <- Y2cbox_wheat_Daycent_ranges/Y2cbox_wheat_Obs_ranges*100
  # this is the mean Daycent range as a % of Obs:
  Y2cbox_wheat_Daycent_Obs_mean_pct_range <- mean(Y2cbox_wheat_Daycent_Obs_pct_ranges)
  
  Y2cbox_wheat_Ensemble_CCCR_range <- max(Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows[1],"ymax"],
                                            Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows[1],"ymax"]) - 
    min(Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows[1],"ymin"],
        Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows[1],"ymin"])
  Y2cbox_wheat_Ensemble_CR_range <- max(Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows[2],"ymax"],
                                          Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows[2],"ymax"]) - 
    min(Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows[2],"ymin"],
        Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows[2],"ymin"])
  Y2cbox_wheat_Ensemble_NTCR_range <- max(Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows[3],"ymax"],
                                            Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows[3],"ymax"]) - 
    min(Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows[3],"ymin"],
        Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows[3],"ymin"])
  Y2cbox_wheat_Ensemble_ranges <- c(Y2cbox_wheat_Ensemble_CCCR_range,Y2cbox_wheat_Ensemble_CR_range,
                                      Y2cbox_wheat_Ensemble_NTCR_range)
  
  Y2cbox_wheat_Ensemble_Obs_pct_ranges <- Y2cbox_wheat_Ensemble_ranges/Y2cbox_wheat_Obs_ranges*100
  # this is the mean Ensemble range as a % of Obs:
  Y2cbox_wheat_Ensemble_Obs_mean_pct_range <- mean(Y2cbox_wheat_Ensemble_Obs_pct_ranges)
  
  
  
  # # calculate mean medians between APSIM and Daycent
  # Y2cbox_corn_APSIM_mean_medians <- mean(Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"middle"])
  # Y2cbox_corn_Daycent_mean_medians <- mean(Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"middle"])
  # Y2cbox_corn_Ensemble_mean_medians <- mean(Y2cbox_bytreat[Y2cbox_corn_Ensemble_rows,"middle"])
  # Y2cbox_corn_Obs_mean_medians <- mean(Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"middle"])
  # 
  # Y2cbox_corn_APSIM_Obs_mean_medians_diff <- Y2cbox_corn_APSIM_mean_medians-Y2cbox_corn_Obs_mean_medians
  # Y2cbox_corn_Daycent_Obs_mean_medians_diff <- Y2cbox_corn_Daycent_mean_medians-Y2cbox_corn_Obs_mean_medians
  # Y2cbox_corn_Ensemble_Obs_mean_medians_diff <- Y2cbox_corn_Ensemble_mean_medians-Y2cbox_corn_Obs_mean_medians
  # 
  # Y2cbox_corn_model_mean_medians <- rowMeans(cbind(Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"middle"],
  #                                                  Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"middle"]))
  # Y2cbox_corn_Ensemble_medians <- Y2cbox_bytreat[Y2cbox_corn_Ensemble_rows,"middle"]
  # Y2cbox_corn_Obs_medians <- Y2cbox_bytreat[Y2cbox_corn_Obs_rows,"middle"]
  # Y2cbox_corn_median_diffs <- Y2cbox_corn_Obs_medians - Y2cbox_corn_model_mean_medians
  # Y2cbox_corn_median_diffs2 <- Y2cbox_corn_Obs_medians - Y2cbox_corn_Ensemble_medians
  # 
  # Y2cbox_soybean_model_mean_medians <- rowMeans(cbind(Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows,"middle"],
  #                                                     Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows,"middle"]))
  # Y2cbox_soybean_Ensemble_medians <- Y2cbox_bytreat[Y2cbox_soybean_Ensemble_rows,"middle"]
  # Y2cbox_soybean_Obs_medians <- Y2cbox_bytreat[Y2cbox_soybean_Obs_rows,"middle"]
  # Y2cbox_soybean_median_diffs <- Y2cbox_soybean_Obs_medians - Y2cbox_soybean_model_mean_medians
  # Y2cbox_soybean_median_diffs2 <- Y2cbox_soybean_Obs_medians - Y2cbox_soybean_Ensemble_medians
  # 
  # Y2cbox_wheat_model_mean_medians <- rowMeans(cbind(Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows,"middle"],
  #                                                   Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows,"middle"]))
  # Y2cbox_wheat_Ensemble_medians <- Y2cbox_bytreat[Y2cbox_wheat_Ensemble_rows,"middle"]
  # Y2cbox_wheat_Obs_medians <- Y2cbox_bytreat[Y2cbox_wheat_Obs_rows,"middle"]
  # Y2cbox_wheat_median_diffs <- Y2cbox_wheat_Obs_medians - Y2cbox_wheat_model_mean_medians
  # Y2cbox_wheat_median_diffs2 <- Y2cbox_wheat_Obs_medians - Y2cbox_wheat_Ensemble_medians
  

  
  #   Y2cbox_corn_Obs_iqrs_mean <- round(mean(Y2cbox_corn_Obs_iqrs),1)
  # Y2cbox_corn_APSIM_iqrs_mean <- round(mean(Y2cbox_corn_APSIM_iqrs),1)
  # Y2cbox_corn_Daycent_iqrs_mean <- round(mean(Y2cbox_corn_Daycent_iqrs),1)
  # Y2cbox_corn_Ensemble_iqrs_mean <- round(mean(Y2cbox_corn_Ensemble_iqrs),1)
  
    # range of the median differences across all calibration mgmt scenarios 
  Y2cbox_corn_APSIM_Obs_med_diff_range <- round(range(Y2cbox_corn_APSIM_Obs_med_diff),2)
  Y2cbox_corn_Daycent_Obs_med_diff_range <- round(range(Y2cbox_corn_Daycent_Obs_med_diff),2)
  Y2cbox_corn_Ensemble_Obs_med_diff_range <- round(range(Y2cbox_corn_Ensemble_Obs_med_diff),2)
  Y2cbox_soybean_APSIM_Obs_med_diff_range <- round(range(Y2cbox_soybean_APSIM_Obs_med_diff),2)
  Y2cbox_soybean_Daycent_Obs_med_diff_range <- round(range(Y2cbox_soybean_Daycent_Obs_med_diff),2)
  Y2cbox_soybean_Ensemble_Obs_med_diff_range <- round(range(Y2cbox_soybean_Ensemble_Obs_med_diff),2)
  Y2cbox_wheat_APSIM_Obs_med_diff_range <- round(range(Y2cbox_wheat_APSIM_Obs_med_diff),2)
  Y2cbox_wheat_Daycent_Obs_med_diff_range <- round(range(Y2cbox_wheat_Daycent_Obs_med_diff),2)
  Y2cbox_wheat_Ensemble_Obs_med_diff_range <- round(range(Y2cbox_wheat_Ensemble_Obs_med_diff),2)
  Y2cbox_crop_medians_df <- round(data.frame(APSIM_corn_med=Y2cbox_bytreat[Y2cbox_corn_APSIM_rows,"middle"],
                                             Daycent_corn_med=Y2cbox_bytreat[Y2cbox_corn_Daycent_rows,"middle"],
                                             Ensemble_corn_med=Y2cbox_corn_Ensemble_medians,
                                             Obs_corn_med=Y2cbox_corn_Obs_medians,
                                             APSIM_soybean_med=Y2cbox_bytreat[Y2cbox_soybean_APSIM_rows,"middle"],
                                             Daycent_soybean_med=Y2cbox_bytreat[Y2cbox_soybean_Daycent_rows,"middle"],
                                             Ensemble_soybean_med=Y2cbox_soybean_Ensemble_medians,
                                             Obs_soybean_med=Y2cbox_soybean_Obs_medians,
                                             APSIM_wheat_med=Y2cbox_bytreat[Y2cbox_wheat_APSIM_rows,"middle"],
                                             Daycent_wheat_med=Y2cbox_bytreat[Y2cbox_wheat_Daycent_rows,"middle"],
                                             Ensemble_wheat_med=Y2cbox_wheat_Ensemble_medians,
                                             Obs_wheat_med=Y2cbox_wheat_Obs_medians),1) %>%
    mutate(scenario=c("CC-CR","RR00-CR","RR00-NT-CR"),
           site=site_name)
  
  write.csv(Y2cbox_crop_medians_df,paste0(results_path,"calib_crop_medians.csv"))
  
  Y2cbox_crop_ranges_df <- round(data.frame(APSIM_corn_range=Y2cbox_corn_APSIM_ranges,
                                             Daycent_corn_range=Y2cbox_corn_Daycent_ranges,
                                             Ensemble_corn_range=Y2cbox_corn_Ensemble_ranges,
                                             Obs_corn_range=Y2cbox_corn_Obs_ranges,
                                             APSIM_soybean_range=Y2cbox_soybean_APSIM_ranges,
                                             Daycent_soybean_range=Y2cbox_soybean_Daycent_ranges,
                                             Ensemble_soybean_range=Y2cbox_soybean_Ensemble_ranges,
                                             Obs_soybean_range=Y2cbox_soybean_Obs_ranges,
                                             APSIM_wheat_range=Y2cbox_wheat_APSIM_ranges,
                                             Daycent_wheat_range=Y2cbox_wheat_Daycent_ranges,
                                             Ensemble_wheat_range=Y2cbox_wheat_Ensemble_ranges,
                                             Obs_wheat_range=Y2cbox_wheat_Obs_ranges),1) %>%
    mutate(scenario=c("CC-CR","RR00-CR","RR00-NT-CR"),
           site=site_name)
  
  write.csv(Y2cbox_crop_ranges_df,paste0(results_path,"calib_crop_ranges.csv"))


  
  ### SOC
  
  
  gS1_box <- soc_calib_output_df_piv_withsd[soc_calib_output_df_piv_withsd$year %in%
                                       soc_calib_output_df_piv_withsd[soc_calib_output_df_piv_withsd$Model=="Observed" &
                                                                 !is.na(soc_calib_output_df_piv_withsd$C_val),"year"] &
                                         soc_calib_output_df_piv_withsd$Source != "Millennial",] %>%
    ggplot(aes(x=Model,y=C_val)) +
    stat_boxplot(geom = "errorbar",width=.5) + 
    geom_boxplot() +
    xlab("") +
    ylab(expression('SOC (Mg C ha ' ^-1*')')) +
    scale_x_discrete(limits = c('APSIM','Daycent','RothC','Observed')) +
    facet_grid(~scenario_abbrev) +
    theme_classic(base_family = "serif", base_size = 30) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  gS1_box
  
  gS1b_box <- soc_calib_output_df_piv_withens[soc_calib_output_df_piv_withens$year %in%
                                              soc_calib_output_df_piv_withens[soc_calib_output_df_piv_withens$Model=="Observed" &
                                                                               !is.na(soc_calib_output_df_piv_withens$C_val),"year"] &
                                              soc_calib_output_df_piv_withens$Source != "Millennial",] %>%
    ggplot(aes(x=Model,y=C_val)) +
    stat_boxplot(geom = "errorbar",width=.5) + 
    geom_boxplot() +
    xlab("") +
    ylab(expression('SOC (Mg C ha ' ^-1*')')) +
    scale_x_discrete(limits = c('APSIM','Daycent','RothC', 'Ensemble', 'Observed')) +
    facet_grid(~scenario_abbrev) +
    theme_classic(base_family = "serif", base_size = 30) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  gS1b_box

  # version that includes crop observed reps and Ensemble
  
  soc_reps_withens_counts <- soc_reps_output_df_piv_withens %>%
    group_by(Model,scenario_abbrev) %>%
    summarize(n_count=n())
  
  write.csv(soc_reps_withens_counts,paste0(results_path,"calib_soc_boxplot_counts.csv"))
  
  g_text_soc <- as.data.frame(soc_reps_withens_counts[,c("scenario_abbrev","Model","n_count")] %>%
                            mutate(lab=paste0("n=",n_count)) %>%
                            select(scenario_abbrev,Model,lab))
  
  gS1c_box <- soc_reps_output_df_piv_withens %>%
    ggplot(aes(x=Model,y=C_val)) +
    stat_boxplot(geom = "errorbar",width=.5) + 
    geom_boxplot() +
    xlab("") +
    ylab(expression('SOC (Mg C ha ' ^-1*')')) +
    scale_x_discrete(limits = c('APSIM','Daycent','RothC', 'Ensemble', 'Observed')) +
    scale_y_continuous(breaks=c(0,20,40,60),
                       limits=c(0,60)) +
    geom_text(data=g_text_soc, ,
              mapping = aes(x = Model, y = 55, label = lab), 
              size = 4.5) +
    facet_grid(~scenario_abbrev) +
    theme_classic(base_family = "serif", base_size = 30) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  gS1c_box
  
  
  # get stats from boxplots
  
  S1box_bytreat <- ggplot_build(gS1_box)[[1]][[1]]
  S1box_soc_Obs_rows <- which(as.numeric(rownames(S1box_bytreat)) %% 4 == 0)
  S1box_soc_APSIM_rows <- which(as.numeric(rownames(S1box_bytreat)) %% 4 == 1)
  S1box_soc_Daycent_rows <- which(as.numeric(rownames(S1box_bytreat)) %% 4 == 2)
  S1box_soc_RothC_rows <- which(as.numeric(rownames(S1box_bytreat)) %% 4 == 3)
  
  # calculate difference in medians between models and Observed
  S1box_soc_APSIM_Obs_med_diff <- S1box_bytreat[S1box_soc_APSIM_rows,"middle"] - 
    S1box_bytreat[S1box_soc_Obs_rows,"middle"]
  S1box_soc_Daycent_Obs_med_diff <- S1box_bytreat[S1box_soc_Daycent_rows,"middle"] -
    S1box_bytreat[S1box_soc_Obs_rows,"middle"]
  S1box_soc_RothC_Obs_med_diff <- S1box_bytreat[S1box_soc_RothC_rows,"middle"] -
    S1box_bytreat[S1box_soc_Obs_rows,"middle"]
  S1box_soc_APSIM_Obs_diff_range <- range(S1box_soc_APSIM_Obs_med_diff)
  S1box_soc_Daycent_Obs_diff_range <- range(S1box_soc_Daycent_Obs_med_diff)
  S1box_soc_RothC_Obs_diff_range <- range(S1box_soc_RothC_Obs_med_diff)
  # calculate IQRs
  S1box_soc_Obs_iqrs <- S1box_bytreat[S1box_soc_Obs_rows,"upper"] - 
    S1box_bytreat[S1box_soc_Obs_rows,"lower"]
  S1box_soc_APSIM_iqrs <- S1box_bytreat[S1box_soc_APSIM_rows,"upper"] - 
    S1box_bytreat[S1box_soc_APSIM_rows,"lower"]
  S1box_soc_Daycent_iqrs <- S1box_bytreat[S1box_soc_Daycent_rows,"upper"] - 
    S1box_bytreat[S1box_soc_Daycent_rows,"lower"]
  S1box_soc_RothC_iqrs <- S1box_bytreat[S1box_soc_RothC_rows,"upper"] - 
    S1box_bytreat[S1box_soc_RothC_rows,"lower"]
  S1box_soc_Obs_iqrs_mean <- round(mean(S1box_soc_Obs_iqrs),1)
  S1box_soc_APSIM_iqrs_mean <- round(mean(S1box_soc_APSIM_iqrs),1)
  S1box_soc_Daycent_iqrs_mean <- round(mean(S1box_soc_Daycent_iqrs),1)
  S1box_soc_RothC_iqrs_mean <- round(mean(S1box_soc_RothC_iqrs),1)
  # calculate mean medians between APSIM, Daycent, and RothC
  S1box_soc_model_mean_medians <- rowMeans(cbind(S1box_bytreat[S1box_soc_APSIM_rows,"middle"],
                                                  S1box_bytreat[S1box_soc_Daycent_rows,"middle"],
                                                 S1box_bytreat[S1box_soc_RothC_rows,"middle"]))
  S1box_soc_Obs_medians <- S1box_bytreat[S1box_soc_Obs_rows,"middle"]
  S1box_soc_median_diffs <- S1box_soc_Obs_medians - S1box_soc_model_mean_medians
  # for paper: model and obs medians
  S1box_soc_medians_df <- round(data.frame(APSIM_soc_med=S1box_bytreat[S1box_soc_APSIM_rows,"middle"],
                                            Daycent_soc_med=S1box_bytreat[S1box_soc_Daycent_rows,"middle"],
                                            RothC_soc_med=S1box_bytreat[S1box_soc_RothC_rows,"middle"],
                                            Model_soc_med=S1box_soc_model_mean_medians,
                                            Obs_soc_med=S1box_soc_Obs_medians),1) %>%
    mutate(scenario=c("CC-CR","RR00-CR","RR00-NT-CR"),
           site=site_name)
  write.csv(S1box_soc_medians_df,paste0(results_path,"calib_soc_medians.csv"))
  
  
  # get stats from boxplots - based on S1c_box -------------------
  S1cbox_bytreat <- ggplot_build(gS1c_box)[[1]][[1]]
  S1cbox_soc_APSIM_rows <- which(as.numeric(rownames(S1cbox_bytreat)) %% 5 == 1)
  S1cbox_soc_Daycent_rows <- which(as.numeric(rownames(S1cbox_bytreat)) %% 5 == 2)
  S1cbox_soc_RothC_rows <- which(as.numeric(rownames(S1cbox_bytreat)) %% 5 == 3)
  S1cbox_soc_Ensemble_rows <- which(as.numeric(rownames(S1cbox_bytreat)) %% 5 == 4)
  S1cbox_soc_Obs_rows <- which(as.numeric(rownames(S1cbox_bytreat)) %% 5 == 0)
  
  # calculate medians of models and observed
  S1cbox_soc_APSIM_medians <- S1cbox_bytreat[S1cbox_soc_APSIM_rows,"middle"]
  S1cbox_soc_Daycent_medians <- S1cbox_bytreat[S1cbox_soc_Daycent_rows,"middle"]
  S1cbox_soc_RothC_medians <- S1cbox_bytreat[S1cbox_soc_RothC_rows,"middle"]
  S1cbox_soc_Ensemble_medians <- S1cbox_bytreat[S1cbox_soc_Ensemble_rows,"middle"]
  S1cbox_soc_Obs_medians <- S1cbox_bytreat[S1cbox_soc_Obs_rows,"middle"]
  
  # calculate difference in medians ("middle") between models and Observed
  ## use absolute value of differences
  S1cbox_soc_APSIM_Obs_med_diff <- abs(S1cbox_bytreat[S1cbox_soc_APSIM_rows,"middle"] - 
                                          S1cbox_bytreat[S1cbox_soc_Obs_rows,"middle"])
  S1cbox_soc_APSIM_Obs_mean_med_diff <- mean(S1cbox_soc_APSIM_Obs_med_diff)
  
  S1cbox_soc_Daycent_Obs_med_diff <- abs(S1cbox_bytreat[S1cbox_soc_Daycent_rows,"middle"] -
                                            S1cbox_bytreat[S1cbox_soc_Obs_rows,"middle"])
  S1cbox_soc_Daycent_Obs_mean_med_diff <- mean(S1cbox_soc_Daycent_Obs_med_diff)
  
  S1cbox_soc_RothC_Obs_med_diff <- abs(S1cbox_bytreat[S1cbox_soc_RothC_rows,"middle"] -
                                            S1cbox_bytreat[S1cbox_soc_Obs_rows,"middle"])
  S1cbox_soc_RothC_Obs_mean_med_diff <- mean(S1cbox_soc_RothC_Obs_med_diff)

  S1cbox_soc_Ensemble_Obs_med_diff <- abs(S1cbox_bytreat[S1cbox_soc_Ensemble_rows,"middle"] -
                                             S1cbox_bytreat[S1cbox_soc_Obs_rows,"middle"])
  S1cbox_soc_Ensemble_Obs_mean_med_diff <- mean(S1cbox_soc_Ensemble_Obs_med_diff)
  
  # calculate IQRs
  S1cbox_soc_Obs_iqrs <- S1cbox_bytreat[S1cbox_soc_Obs_rows,"upper"] - 
    S1cbox_bytreat[S1cbox_soc_Obs_rows,"lower"]
  S1cbox_soc_Obs_max_iqr <- max(S1cbox_soc_Obs_iqrs)
  
  S1cbox_soc_APSIM_iqrs <- S1cbox_bytreat[S1cbox_soc_APSIM_rows,"upper"] - 
    S1cbox_bytreat[S1cbox_soc_APSIM_rows,"lower"]
  S1cbox_soc_APSIM_max_iqr <- max(S1cbox_soc_APSIM_iqrs)
  S1cbox_soc_APSIM_Obs_pct_iqrs <- S1cbox_soc_APSIM_iqrs/S1cbox_soc_Obs_iqrs*100
  # this is the mean APSIM IQR as a % of Obs:
  S1cbox_soc_APSIM_Obs_mean_pct_iqr <- mean(S1cbox_soc_APSIM_Obs_pct_iqrs)
  
  S1cbox_soc_Daycent_iqrs <- S1cbox_bytreat[S1cbox_soc_Daycent_rows,"upper"] - 
    S1cbox_bytreat[S1cbox_soc_Daycent_rows,"lower"]
  S1cbox_soc_Daycent_max_iqr <- max(S1cbox_soc_Daycent_iqrs)
  S1cbox_soc_Daycent_Obs_pct_iqrs <- S1cbox_soc_Daycent_iqrs/S1cbox_soc_Obs_iqrs*100
  # this is the mean Daycent IQR as a % of Obs:
  S1cbox_soc_Daycent_Obs_mean_pct_iqr <- mean(S1cbox_soc_Daycent_Obs_pct_iqrs)

  S1cbox_soc_RothC_iqrs <- S1cbox_bytreat[S1cbox_soc_RothC_rows,"upper"] - 
    S1cbox_bytreat[S1cbox_soc_RothC_rows,"lower"]
  S1cbox_soc_RothC_max_iqr <- max(S1cbox_soc_RothC_iqrs)
  S1cbox_soc_RothC_Obs_pct_iqrs <- S1cbox_soc_RothC_iqrs/S1cbox_soc_Obs_iqrs*100
  # this is the mean RothC IQR as a % of Obs:
  S1cbox_soc_RothC_Obs_mean_pct_iqr <- mean(S1cbox_soc_RothC_Obs_pct_iqrs)

  S1cbox_soc_Ensemble_iqrs <- S1cbox_bytreat[S1cbox_soc_Ensemble_rows,"upper"] - 
    S1cbox_bytreat[S1cbox_soc_Ensemble_rows,"lower"]
  S1cbox_soc_Ensemble_max_iqr <- max(S1cbox_soc_Ensemble_iqrs)
  S1cbox_soc_Ensemble_Obs_pct_iqrs <- S1cbox_soc_Ensemble_iqrs/S1cbox_soc_Obs_iqrs*100
  # this is the mean Ensemble IQR as a % of Obs:
  S1cbox_soc_Ensemble_Obs_mean_pct_iqr <- mean(S1cbox_soc_Ensemble_Obs_pct_iqrs)
  
  # mean IQRs
  S1cbox_soc_Model_mean_iqr <- mean(mean(S1cbox_soc_APSIM_iqrs),
                                    mean(S1cbox_soc_Daycent_iqrs),
                                    mean(S1cbox_soc_RothC_iqrs))
  S1cbox_soc_Ensemble_mean_iqr <- mean(S1cbox_soc_Ensemble_iqrs)
  S1cbox_soc_Obs_mean_iqr <- mean(S1cbox_soc_Obs_iqrs)
  
  S1cbox_soc_Model_iqrs <- c(S1cbox_soc_APSIM_iqrs,S1cbox_soc_Daycent_iqrs,
                               S1cbox_soc_RothC_iqrs)
  S1cbox_soc_Model_Obs_pct_iqrs <- S1cbox_soc_Model_iqrs/S1cbox_soc_Obs_iqrs*100
  # this is the mean Ensemble iqr as a % of Obs:
  S1cbox_soc_Model_Obs_mean_pct_iqr <- mean(S1cbox_soc_Model_Obs_pct_iqrs)
  
  # calculate ranges and % of Obs
  S1cbox_soc_Obs_ranges <- S1cbox_bytreat[S1cbox_soc_Obs_rows,"ymax"] - 
    S1cbox_bytreat[S1cbox_soc_Obs_rows,"ymin"]
  
  S1cbox_soc_APSIM_ranges <- S1cbox_bytreat[S1cbox_soc_APSIM_rows,"ymax"] - 
    S1cbox_bytreat[S1cbox_soc_APSIM_rows,"ymin"]
  S1cbox_soc_APSIM_Obs_pct_ranges <- S1cbox_soc_APSIM_ranges/S1cbox_soc_Obs_ranges*100
  # this is the mean APSIM range as a % of Obs:
  S1cbox_soc_APSIM_Obs_mean_pct_range <- mean(S1cbox_soc_APSIM_Obs_pct_ranges)
  
  S1cbox_soc_Daycent_ranges <- S1cbox_bytreat[S1cbox_soc_Daycent_rows,"ymax"] - 
    S1cbox_bytreat[S1cbox_soc_Daycent_rows,"ymin"]
  S1cbox_soc_Daycent_Obs_pct_ranges <- S1cbox_soc_Daycent_ranges/S1cbox_soc_Obs_ranges*100
  # this is the mean Daycent range as a % of Obs:
  S1cbox_soc_Daycent_Obs_mean_pct_range <- mean(S1cbox_soc_Daycent_Obs_pct_ranges)

  S1cbox_soc_RothC_ranges <- S1cbox_bytreat[S1cbox_soc_RothC_rows,"ymax"] - 
    S1cbox_bytreat[S1cbox_soc_RothC_rows,"ymin"]
  S1cbox_soc_RothC_Obs_pct_ranges <- S1cbox_soc_RothC_ranges/S1cbox_soc_Obs_ranges*100
  # this is the mean RothC range as a % of Obs:
  S1cbox_soc_RothC_Obs_mean_pct_range <- mean(S1cbox_soc_RothC_Obs_pct_ranges)

  S1cbox_soc_Ensemble_ranges <- S1cbox_bytreat[S1cbox_soc_Ensemble_rows,"ymax"] - 
    S1cbox_bytreat[S1cbox_soc_Ensemble_rows,"ymin"]
  S1cbox_soc_Ensemble_Obs_pct_ranges <- S1cbox_soc_Ensemble_ranges/S1cbox_soc_Obs_ranges*100
  # this is the mean Ensemble range as a % of Obs:
  S1cbox_soc_Ensemble_Obs_mean_pct_range <- mean(S1cbox_soc_Ensemble_Obs_pct_ranges)
  
  # mean ranges
  S1cbox_soc_Model_mean_range <- mean(mean(S1cbox_soc_APSIM_ranges),
                                    mean(S1cbox_soc_Daycent_ranges),
                                    mean(S1cbox_soc_RothC_ranges))
  S1cbox_soc_Ensemble_mean_range <- mean(S1cbox_soc_Ensemble_ranges)
  S1cbox_soc_Obs_mean_range <- mean(S1cbox_soc_Obs_ranges)

  S1cbox_soc_Model_ranges <- c(S1cbox_soc_APSIM_ranges,S1cbox_soc_Daycent_ranges,
                               S1cbox_soc_RothC_ranges)
  S1cbox_soc_Model_Obs_pct_ranges <- S1cbox_soc_Model_ranges/S1cbox_soc_Obs_ranges*100
  # this is the mean Ensemble range as a % of Obs:
  S1cbox_soc_Model_Obs_mean_pct_range <- mean(S1cbox_soc_Model_Obs_pct_ranges)
  

  S1cbox_soc_ranges_df <- round(data.frame(APSIM_soc_range=S1cbox_soc_APSIM_ranges,
                                            Daycent_soc_range=S1cbox_soc_Daycent_ranges,
                                            RothC_soc_range=S1cbox_soc_RothC_ranges,
                                            Ensemble_soc_range=S1cbox_soc_Ensemble_ranges,
                                            Obs_soc_range=S1cbox_soc_Obs_ranges),1) %>%
    mutate(scenario=c("CC-CR","RR00-CR","RR00-NT-CR"),
           site=site_name)
  
  write.csv(S1cbox_soc_ranges_df,paste0(results_path,"calib_soc_ranges.csv"))

  # # Calibration bar charts -------------------------
  # 
  # ## N2O
  # 
  # gN1_bar <- ggplot(n2o_calib_output_df_piv, aes(fill=Source, y=n2o_val, x=scenario_abbrev)) + 
  #   geom_bar(position='dodge', stat='identity') +
  #   xlab("") +
  #   ylab(expression('N'[2]*'O (g N ha' ^'-1'*')')) +
  #   ggtitle(bquote(.(site_name)~"Total Modeled N"[2]*"O")) +
  #   scale_color_manual(labels=c("APSIM","Daycent","Observed"),
  #                      values=c(APSIM_color,Daycent_color,Observed_color)) +
  #   scale_fill_manual(labels=c("APSIM","Daycent","Observed"),
  #                     values=c(APSIM_color,Daycent_color,Observed_color)) +
  #   theme_classic(base_family = "serif", base_size = 25) +
  #   theme(panel.background = element_blank(),
  #         #        text = element_text(size=16),
  #         axis.line = element_line(colour = "black"),
  #         axis.ticks.x = element_blank()) #,
  #         #axis.text.x = element_text(angle = 45,
  #         #                           hjust = 1))
  # 
  # gN1_bar
  # 
  # ## CH4
  # 
  # gC1_bar <- ggplot(ch4_calib_output_df_piv, aes(fill=Source, y=ch4_val, x=scenario_abbrev)) + 
  #   geom_bar(position='dodge', stat='identity') +
  #   ylab(expression('CH'[4]*'e (Mg ha ' ^-1*')')) +
  #   xlab("") +
  #   ggtitle(bquote(.(site_name)~"Total Modeled CH"[4])) +
  #   scale_color_manual(labels=c("Daycent","Observed"),
  #                      values=c(Daycent_color,Observed_color)) +
  #   scale_fill_manual(labels=c("Daycent","Observed"),
  #                     values=c(Daycent_color,Observed_color)) +
  #   theme_classic(base_family = "serif", base_size = 25) +
  #   theme(panel.background = element_blank(),
  #         #        text = element_text(size=16),
  #         axis.line = element_line(colour = "black"),
  #         axis.ticks.x = element_blank()) #,
  # #axis.text.x = element_text(angle = 45,
  # #                           hjust = 1))
  # 
  # gC1_bar
  
  # write graphics ---------------------------------
  
  ggsave(filename=paste0(results_path,"pub_Crop_yield_calib_boxplots_all.jpg"),
         plot=gY1_box, width=12, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"pub_Crop_yield_calib_boxplots_bytreat.jpg"),
         plot=gY2c_box, width=10, height=10, dpi=300)
  ggsave(filename=paste0(results_path,"pub_SOC_calib_boxplots_bytreat.jpg"),
         plot=gS1c_box, width=10, height=10, dpi=300)
  # ggsave(filename=paste0(results_path,"pub_N2O_calib_bargraphs_bytreat.jpg"),
  #        plot=gN1_bar, width=10, height=6, dpi=300)
  # ggsave(filename=paste0(results_path,"pub_CH4_calib_boxgraphs_bytreat.jpg"),
  #        plot=gC1_bar, width=10, height=10, dpi=300)
  
  #*************************************************************

    # Import summarized data -------------------------------------------------


# global warming potential
## calculated by mean of SOC, N2O, CH4 among all models by 2100/2050/etc,
## then CO2 equivalent in 100-yr timeframe.
## then calculate emissions - sequestration: N2O + CH4 - SOC
## CH4 is usually negative, so will offset emissions

## read in model summary output
summary_output_raw <- read.csv(paste0(results_path,"Summary_output.csv")) 

## get means for N2O and CH4 to calculate separately for change-over-time
scenario_gas_means <- summary_output_raw %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(mean_N2O_Mgha=round(mean(Total_N2O_kgha/1000,na.rm=T),4),
            mean_CH4_Mgha=round(mean(Total_CH4_kgha/1000,na.rm=T),4),
            sd_N2O_Mgha=round(sd(Total_N2O_kgha/1000,na.rm=T),4),
            sd_CH4_Mgha=round(sd(Total_CH4_kgha/1000,na.rm=T),4)
  ) %>%
  left_join(scenario_df[,c("scenario_name","scenario_abbrev")],by=c("Scenario_Name"="scenario_name"))


## get means for N2O and CH4 to fill in for each model which doesn't include it,
## in order to calculate GWP:
## N2O - RothC 
## CH4 - APSIM and RothC

ghg_means <- summary_output_raw %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(model_mean_N2O_kgha=mean(Total_N2O_kgha,na.rm=T),
            model_mean_CH4_kgha=mean(Total_CH4_kgha,na.rm=T)
  )

## now fill in missing data
summary_fillin <- left_join(summary_output_raw,
                            ghg_means,
                            by=c("Climate_Scenario","Mgmt_Scenario","Scenario_Name")) 
summary_fillin[is.na(summary_fillin$Total_N2O_kgha),"Total_N2O_kgha"] <-
  summary_fillin[is.na(summary_fillin$Total_N2O_kgha),"model_mean_N2O_kgha"]
summary_fillin[is.na(summary_fillin$Total_CH4_kgha),"Total_CH4_kgha"] <-
  summary_fillin[is.na(summary_fillin$Total_CH4_kgha),"model_mean_CH4_kgha"]

summary_output <- summary_fillin[,-which(names(summary_fillin) %in% 
                                           c("model_mean_N2O_kgha","model_mean_CH4_kgha"))] %>%
  mutate(N2O_Mgha=Total_N2O_kgha/1000,
         CH4_Mgha=Total_CH4_kgha/1000,
         CO2e_SOC=-SOC_Diff_Mgha*(44/12), # convert SOC to CO2e (CO2 gas), make negative 
         CO2e_N2O=N2O_Mgha*(44/28)*273, # convert N2O(N) to N2O gas, then to CO2e
         CO2e_CH4=CH4_Mgha*(16/12)*27 # convert CH4(C) to CH4 gas, then to CO2e
  ) %>%
left_join(scenario_df[,c("scenario_name","scenario_abbrev")],
          by=c("Scenario_Name"="scenario_name"))

summary_output$GWP=rowSums(summary_output[grep("^CO2e", names(summary_output))], 
                           na.rm=TRUE) # SOC negative for sequestration

# write out summary output for final analysis
write.csv(summary_output, file=paste0(results_path,"summary_output_final.csv"),
          row.names=FALSE)

scenario_means <- summary_output %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(mean_MaizeYld_Mgha=round(mean(Maize_Diff_Mgha,na.rm=T),5),
            mean_SoyYld_Mgha=round(mean(Soybean_Diff_Mgha,na.rm=T),5),
            mean_WheatYld_Mgha=round(mean(Wheat_Diff_Mgha,na.rm=T),5),
            mean_SOC_Mgha=round(mean(SOC_Diff_Mgha,na.rm=T),5),
            mean_N2O_Mgha=round(mean(N2O_Mgha,na.rm=T),5),
            mean_CH4_Mgha=round(mean(CH4_Mgha,na.rm=T),5),
            mean_CO2e_SOC=round(mean(CO2e_SOC,na.rm=T),5),
            mean_CO2e_N2O=round(mean(CO2e_N2O,na.rm=T),5),
            mean_CO2e_CH4=round(mean(CO2e_CH4,na.rm=T),5),
            mean_GWP=round(mean(GWP,na.rm=T),5),
            sd_MaizeYld_Mgha=round(sd(Maize_Diff_Mgha,na.rm=T),5),
            sd_SoyYld_Mgha=round(sd(Soybean_Diff_Mgha,na.rm=T),5),
            sd_WheatYld_Mgha=round(sd(Wheat_Diff_Mgha,na.rm=T),5),
            sd_SOC_Mgha=round(sd(SOC_Diff_Mgha,na.rm=T),5),
            sd_N2O_Mgha=round(sd(N2O_Mgha,na.rm=T),5),
            sd_CH4_Mgha=round(sd(CH4_Mgha,na.rm=T),5),
            sd_CO2e_SOC=round(sd(CO2e_SOC),5),
            sd_CO2e_N2O=round(sd(CO2e_N2O),5),
            sd_CO2e_CH4=round(sd(CO2e_CH4),5),
            sd_GWP=round(sd(GWP,na.rm=T),5)
  ) %>%
  left_join(scenario_df[,c("scenario_name","scenario_abbrev")],
            by=c("Scenario_Name"="scenario_name"))



# write out scenario means
write.csv(scenario_means, file=paste0(results_path,"scenario_means.csv"),
          row.names=FALSE)

gwp_means_piv <- pivot_longer(scenario_means,c(-Climate_Scenario,
                                               -Mgmt_Scenario,
                                               -Scenario_Name,
                                               -scenario_abbrev,
),
names_to="source",
values_to="vals")

annual_results <- data.frame()
# loop through csvs to get the scenarios with options (like groups 4 and 5)
for (i in clim_nums) { # climate scenarios
  for (j in mgmt_grps) { # management groups
    max_options <- if_else(j==4,4,
                   if_else(j==5,3,
                   if_else(j==6,5,1)))
    for (k in 1:max_options) { # management options
      if(j!=6) {
        for (l in c("APSIM","Daycent","RothC")) {
          scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
          read_dat <- read.csv(paste0(results_path,"Annual_results_compilation_",scen_nm,
                                      "_",l,".csv"))
          annual_results <- rbind(annual_results,read_dat)
        } # end for - models
      } else {
        scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
        read_dat <- read.csv(paste0(results_path,"Annual_results_compilation_",scen_nm,
                                    "_APSIM.csv"))
        annual_results <- rbind(annual_results,read_dat)
      } # end if - management group != 6
    } # end for - management group options
  } # end for - management groups
} # end for-climate scenarios

# fill-in NA yields with 0
# annual_results[is.na(annual_results$MaizeYld_Mgha),"MaizeYld_Mgha"] <- 0
# annual_results[is.na(annual_results$SoyYld_Mgha),"SoyYld_Mgha"] <- 0
# annual_results[is.na(annual_results$WheatYld_Mgha),"WheatYld_Mgha"] <- 0

# add scenario abbreviation
annual_results <- left_join(annual_results, 
                            scenario_df[,c("scenario_name","scenario_abbrev")],
                            by="scenario_name")

write.csv(annual_results, file=paste0(results_path,"annual_results.csv"),
          row.names=FALSE)

# mean annual results, by scenario
mean_annual_results <- annual_results[annual_results<end_fut_period_year,] %>%
  group_by(year,scenario_name,climate_scenario_num,mgmt_scenario_grp_num,
           mgmt_scenario_opt_num,scenario_abbrev) %>%
  summarize(MaizeYld_Mgha=round(mean(MaizeYld_Mgha,na.rm=T),3),
            SoyYld_Mgha=round(mean(SoyYld_Mgha,na.rm=T),3),
            WheatYld_Mgha=round(mean(WheatYld_Mgha,na.rm=T),3),
            SOC_Mgha=round(mean(SOC_Mgha,na.rm=T),3)
  )
mean_annual_results <- mean_annual_results[!is.na(mean_annual_results$year),]

write.csv(mean_annual_results, file=paste0(results_path,"mean_annual_results.csv"),
          row.names=FALSE)


daily_results <- data.frame()
# loop through csvs to get the scenarios with options (like groups 4 and 5)
for (i in clim_nums) { # climate scenarios
  for (j in mgmt_grps) { # management groups
    max_options <- if_else(j==4,4,
                           if_else(j==5,3,
                                   if_else(j==6,5,1)))
    for (k in 1:max_options) { # management options
      if(j!=6) {
        for (l in c("APSIM","Daycent")) {
          scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
          read_dat <- read.csv(paste0(results_path,"Daily_results_compilation_",scen_nm,
                                      "_",l,".csv"))
          daily_results <- rbind(daily_results,read_dat)
        } # end for - models
      } else {
        scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
        read_dat <- read.csv(paste0(results_path,"Daily_results_compilation_",scen_nm,
                                    "_APSIM.csv"))
        daily_results <- rbind(daily_results,read_dat)
      } # end if - management group != 6
    } # end for - mgmt options
  } # end for - management groups
} # end for - climate scenarios

# add N2O and CH4 conversions to kgha
daily_results$N2O_cum_kgha <- daily_results$N2O_cum_gha/1000
daily_results$CH4_cum_kgha <- daily_results$CH4_cum_gha/1000

# add scenario abbreviation
daily_results <- left_join(daily_results, 
                           scenario_df[,c("scenario_name","scenario_abbrev")],
                           by="scenario_name")


write.csv(daily_results, file=paste0(results_path,"daily_results.csv"),
          row.names=FALSE)

# mean daily results, by scenario
mean_daily_results <- daily_results[daily_results$year<end_fut_period_year,] %>%
  group_by(year,date,dayofyear,scenario_name,climate_scenario_num,
           mgmt_scenario_grp_num,mgmt_scenario_opt_num,scenario_abbrev) %>%
  summarize(N2O_emit_gha=round(mean(N2O_emit_gha,na.rm=T),5),
            N2O_cum_gha=round(mean(N2O_cum_gha,na.rm=T),5),
            N2O_cum_kgha=N2O_cum_gha/1000,
            CH4_net_gha=round(mean(CH4_net_gha,na.rm=T),5),
            CH4_cum_gha=round(mean(CH4_cum_gha,na.rm=T),5),
            CH4_cum_kgha=CH4_cum_gha/1000
  )


write.csv(mean_daily_results, file=paste0(results_path,"mean_daily_results.csv"),
          row.names=FALSE)

#*************************************************************

# Combine scenarios with multiple options ---------------------------------
#one set of graphs per climate scenario

for(clim_num in clim_nums) {
  climate_desc <-   if_else(clim_num=="1","Baseline",
                    if_else(clim_num=="2","GFDL_ESM4 Low",
                    if_else(clim_num=="3","GFDL_ESM4 High",
                    if_else(clim_num=="4","UKESM1-0-LL Low",
                    if_else(clim_num=="5","UKESM1-0-LL High",
                    "Missing Descriptor")))))
  

  ## Annual graphs-combined scenarios

  ### Mean of all scenarios

  #### Maize

  gAllMYexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                     mean_annual_results$MaizeYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllMYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="MaizeYld_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="MYfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllMYexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

  gAllMYfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                     mean_annual_results$MaizeYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future Maize Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
                color=g$data[[1]]$colour[1]) +
    geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
                color=g$data[[1]]$colour[2]) +
    geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
                color=g$data[[1]]$colour[3]) +
    geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
                color=g$data[[1]]$colour[4]) +
    geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
                color=g$data[[1]]$colour[5]) +
    geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
                color=g$data[[1]]$colour[6]) +
    geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
                color=g$data[[1]]$colour[7]) +
    geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
                color=g$data[[1]]$colour[8]) +
    geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
                color=g$data[[1]]$colour[9]) +
    geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
                color=g$data[[1]]$colour[10]) +
    geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
                color=g$data[[1]]$colour[11]) +
    geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
                color=g$data[[1]]$colour[12]) +
    geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
                color=g$data[[1]]$colour[13]) +
    geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
                color=g$data[[1]]$colour[14]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())


  gAllMYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Maize_exp_",clim_num,".jpg"),
         plot=gAllMYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Maize_fut_",clim_num,".jpg"),
         plot=gAllMYfut, width=9, height=6, dpi=300)


  #### Soybean

  gAllSYexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                     mean_annual_results$SoyYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllSYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="SoyYld_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="SYfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllSYexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

    gAllSYfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                     mean_annual_results$SoyYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future Soybean Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
                  color=g$data[[1]]$colour[1]) +
      geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
                  color=g$data[[1]]$colour[2]) +
      geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
                  color=g$data[[1]]$colour[3]) +
      geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
                  color=g$data[[1]]$colour[4]) +
      geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
                  color=g$data[[1]]$colour[5]) +
      geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
                  color=g$data[[1]]$colour[6]) +
      geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
                  color=g$data[[1]]$colour[7]) +
      geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
                  color=g$data[[1]]$colour[8]) +
      geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
                  color=g$data[[1]]$colour[9]) +
      geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
                  color=g$data[[1]]$colour[10]) +
      geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
                  color=g$data[[1]]$colour[11]) +
      geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
                  color=g$data[[1]]$colour[12]) +
      geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
                  color=g$data[[1]]$colour[13]) +
      geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
                  color=g$data[[1]]$colour[14]) +
      # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Soy_exp_",clim_num,".jpg"),
         plot=gAllSYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Soy_fut_",clim_num,".jpg"),
         plot=gAllSYfut, width=9, height=6, dpi=300)


  ####Wheat

  gAllWYexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                     mean_annual_results$WheatYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllWYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="WheatYld_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="WYfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllWYexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

    gAllWYfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                     mean_annual_results$WheatYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future Wheat Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
                  color=g$data[[1]]$colour[1]) +
      geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
                  color=g$data[[1]]$colour[2]) +
      geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
                  color=g$data[[1]]$colour[3]) +
      geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
                  color=g$data[[1]]$colour[4]) +
      geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
                  color=g$data[[1]]$colour[5]) +
      geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
                  color=g$data[[1]]$colour[6]) +
      geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
                  color=g$data[[1]]$colour[7]) +
      geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
                  color=g$data[[1]]$colour[8]) +
      geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
                  color=g$data[[1]]$colour[9]) +
      geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
                  color=g$data[[1]]$colour[10]) +
      geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
                  color=g$data[[1]]$colour[11]) +
      geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
                  color=g$data[[1]]$colour[12]) +
      geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
                  color=g$data[[1]]$colour[13]) +
      geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
                  color=g$data[[1]]$colour[14]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllWYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Wheat_exp_",clim_num,".jpg"),
         plot=gAllWYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Wheat_fut_",clim_num,".jpg"),
         plot=gAllWYfut, width=9, height=6, dpi=300)


  ## SOC

  gAllCexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                    mean_annual_results$SOC_Mgha != 0 &
                                    mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_line(linewidth=1) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="SOC_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="Cfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllCexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

    gAllCfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                    mean_annual_results$SOC_Mgha != 0 &
                                    mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_line(linewidth=1) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future SOC: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  # # use this to get the colors used:
  # g <- ggplot_build(gAllfut)
  # g$data[[1]]$colour
  # [1] "#F8766D" "#D39200" "#93AA00" "#00BA38" "#00C19F" "#00B9E3" "#FF61C3" "#DB72FB" "#619CFF"

  gAllCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_SOC_exp_",clim_num,".jpg"),
         plot=gAllCexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_SOC_fut_",clim_num,".jpg"),
         plot=gAllCfut, width=9, height=6, dpi=300)


  #*************************************************************

## Scenario group 4 --------------------------------------------------------


  #### Maize yield

  gMYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                                   modeled_element_in="MaizeYld_Mgha",
                                   model_name_in="APSIM",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=4,
                                   result_name_in="MYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="MaizeYld_Mgha",
                              model_name_in="Daycent",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="MYfit")

    gMYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[4,3], slope=Daycent_coefs[4,4],
                  color=cbPalette9[2]) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_exp_grp_4_",clim_num,".jpg"),
         plot=gMYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_fut_grp_4_",clim_num,".jpg"),
         plot=gMYfut, width=9, height=6, dpi=300)



  #### Soybean yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SoyYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="SYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SoyYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="SYfit")

    gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[4,3], slope=Daycent_coefs[4,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_exp_grp_4_",clim_num,".jpg"),
         plot=gSYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_fut_grp_4_",clim_num,".jpg"),
         plot=gSYfut, width=9, height=6, dpi=300)


  #### Wheat yield

  gWYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="WheatYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="WYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="WheatYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="WYfit")

    gWYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[4,3], slope=Daycent_coefs[4,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_exp_grp_4_",clim_num,".jpg"),
         plot=gWYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_fut_grp_4_",clim_num,".jpg"),
         plot=gWYfut, width=9, height=6, dpi=300)


  #### SOC

  newC <- ObsC_Mgha[ObsC_Mgha$year > land_conversion_year,] %>%
    mutate(site="Observed")

  gCexp <- ggplot() +
    geom_point(data=annual_results[annual_results$year>=experiment_start_year &
                                     annual_results$model_name %in% c("APSIM","Daycent","RothC") &
                                     annual_results$mgmt_scenario_grp_num==4 &
                                     annual_results$climate_scenario_num==clim_num,],
               aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","Observed","RothC"),
                       values=cbPalette9[c(8,2,1,3)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SOC_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="Cfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SOC_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="Cfit")
  RothC_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SOC_Mgha",
                                model_name_in="RothC",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="Cfit")

    gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC") &
                            annual_results$mgmt_scenario_grp_num==4 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","RothC"),
                     values=cbPalette9[c(8,2,3)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_4_",clim_num,".jpg"),
         plot=gCexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_4_",clim_num,".jpg"),
         plot=gCfut, width=9, height=6, dpi=300)


  #*************************************************************

## Scenario group 5 --------------------------------------------------------


  #### Maize yield

  gMYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="MaizeYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=5,
                              result_name_in="MYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="MaizeYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=5,
                                result_name_in="MYfit")

    gMYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_exp_grp_5_",clim_num,".jpg"),
         plot=gMYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_fut_grp_5_",clim_num,".jpg"),
         plot=gMYfut, width=9, height=6, dpi=300)



  #### Soybean yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SoyYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=5,
                              result_name_in="SYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SoyYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=5,
                                result_name_in="SYfit")

  gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                color=cbPalette9[2]) +
    geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                color=cbPalette9[2]) +
    geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                color=cbPalette9[2]) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_exp_grp_5_",clim_num,".jpg"),
         plot=gSYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_fut_grp_5_",clim_num,".jpg"),
         plot=gSYfut, width=9, height=6, dpi=300)


  #### Wheat yield

  gWYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="WheatYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=5,
                              result_name_in="WYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="WheatYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=5,
                                result_name_in="WYfit")

    gWYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_exp_grp_5_",clim_num,".jpg"),
         plot=gWYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_fut_grp_5_",clim_num,".jpg"),
         plot=gWYfut, width=9, height=6, dpi=300)


  #### SOC
  Cfit_RothC_1_53 <- coef(lm(SOC_Mgha ~ year,
                             data = annual_results[annual_results$year>=experiment_end_year &
                                                     annual_results$model_name=="RothC" &
                                                     annual_results$scenario_name=="1_53",]))


  gCexp <- annual_results[annual_results$year>=experiment_start_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC") &
                            annual_results$mgmt_scenario_grp_num==5 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point() +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","Observed","RothC"),
                       values=cbPalette9[c(8,2,1,3)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC") &
                            annual_results$mgmt_scenario_grp_num==5 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","RothC"),
                     values=cbPalette9[c(8,2,3)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_5_",clim_num,".jpg"),
         plot=gCexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_5_",clim_num,".jpg"),
         plot=gCfut, width=9, height=6, dpi=300)


  #*************************************************************

## Scenario group 6 --------------------------------------------------------


  #### Maize yield
  MYfit_APSIM_1_61 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_61",]))
  MYfit_APSIM_1_62 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_62",]))
  MYfit_APSIM_1_63 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_63",]))
  MYfit_APSIM_1_64 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_64",]))
  MYfit_APSIM_1_65 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_65",]))

  gMYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="MaizeYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=6,
                              result_name_in="MYfit")

    gMYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             #annual_results$scenario_name=="1_61" &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[5,3], slope=APSIM_coefs[5,4],
                  color=cbPalette9[8]) +
      scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_exp_grp_6_",clim_num,".jpg"),
         plot=gMYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_fut_grp_6_",clim_num,".jpg"),
         plot=gMYfut, width=9, height=6, dpi=300)



  #### Soybean yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp


  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SoyYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=6,
                              result_name_in="SYfit")

    gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[5,3], slope=APSIM_coefs[5,4],
                  color=cbPalette9[8]) +
      scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_exp_grp_6_",clim_num,".jpg"),
         plot=gSYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_fut_grp_6_",clim_num,".jpg"),
         plot=gSYfut, width=9, height=6, dpi=300)


  #### Wheat yield

  gWYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYexp

  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="WheatYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=6,
                              result_name_in="WYfit")

    gWYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[5,3], slope=APSIM_coefs[5,4],
                  color=cbPalette9[8]) +
      scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_exp_grp_6_",clim_num,".jpg"),
         plot=gWYexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_fut_grp_6_",clim_num,".jpg"),
         plot=gWYfut, width=9, height=6, dpi=300)


  #### SOC

    newC <- ObsC_Mgha[ObsC_Mgha$year > land_conversion_year,] %>%
    mutate(site="Observed")

  gCexp <- ggplot() +
    geom_point(data=annual_results[annual_results$year>=experiment_start_year &
                                     annual_results$model_name %in% c("APSIM") &
                                     annual_results$mgmt_scenario_grp_num==6 &
                                     annual_results$climate_scenario_num==clim_num,],
               aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM") &
                            annual_results$mgmt_scenario_grp_num==6 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                     values=cbPalette9[c(8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_6_",clim_num,".jpg"),
         plot=gCexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_6_",clim_num,".jpg"),
         plot=gCfut, width=9, height=6, dpi=300)


  #*************************************************************

## Daily graphs-combined scenarios -----------------------------------------

  
  ### Mean of all scenarios

  gAllNexp <- mean_daily_results[mean_daily_results$year>=experiment_start_year &
                                   mean_daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ylim(0,100) +
    ggtitle(paste(site_name," Future N2O Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllNexp

  gAllNfut <- mean_daily_results[mean_daily_results$year>=experiment_end_year &
                                   mean_daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ylim(0,100) +
    ggtitle(paste(site_name," Future N2O Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_N2O_exp_",clim_num,".jpg"),
         plot=gAllNexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_N2O_fut_",clim_num,".jpg"),
         plot=gAllNfut, width=9, height=6, dpi=300)


  gAllCHexp <- mean_daily_results[mean_daily_results$year>=experiment_start_year &
                                    mean_daily_results$climate_scenario_num==clim_num &
                                    mean_daily_results$mgmt_scenario_grp_num!=6,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ylim(-85,0) +
    ggtitle(paste(site_name," Future CH4 Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCHexp

  gAllCHfut <- mean_daily_results[mean_daily_results$year>=experiment_end_year &
                                    mean_daily_results$climate_scenario_num==clim_num &
                                    mean_daily_results$mgmt_scenario_grp_num!=6,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ylim(-85,0) +
    ggtitle(paste(site_name," Future CH4 Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_CH4_exp_",clim_num,".jpg"),
         plot=gAllCHexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_CH4_fut_",clim_num,".jpg"),
         plot=gAllCHfut, width=9, height=6, dpi=300)


  #*************************************************************

## Daily-scenario group 4 --------------------------------------------------


  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==4 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ylim(0,100) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==4 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ylim(0,100) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_4_",clim_num,".jpg"),
         plot=gNexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_4_",clim_num,".jpg"),
         plot=gNfut, width=9, height=6, dpi=300)


  # CH4

  gCHexp <- daily_results[daily_results$year>=experiment_start_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==4 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ylim(-85, 0) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                       values=cbPalette9[c(2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHexp

  gCHfut <- daily_results[daily_results$year>=experiment_end_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==4 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ylim(-85,0) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                     values=cbPalette9[c(2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_exp_grp_4_",clim_num,".jpg"),
         plot=gCHexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_fut_grp_4_",clim_num,".jpg"),
         plot=gCHfut, width=9, height=6, dpi=300)


  #*************************************************************

## Daily-scenario group 5 --------------------------------------------------


  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==5 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ylim(0,100) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==5 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ylim(0,100) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_5_",clim_num,".jpg"),
         plot=gNexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_5_",clim_num,".jpg"),
         plot=gNfut, width=9, height=6, dpi=300)


  # CH4

  gCHexp <- daily_results[daily_results$year>=experiment_start_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==5 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ylim(-85,0) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                       values=cbPalette9[c(2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHexp

  gCHfut <- daily_results[daily_results$year>=experiment_end_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==5 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ylim(-85,0) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                     values=cbPalette9[c(2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_exp_grp_5_",clim_num,".jpg"),
         plot=gCHexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_fut_grp_5_",clim_num,".jpg"),
         plot=gCHfut, width=9, height=6, dpi=300)


  #*************************************************************

## Daily-scenario group 6 --------------------------------------------------


  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM") &
                           daily_results$mgmt_scenario_grp_num==6 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ylim(0,100) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM") &
                           daily_results$mgmt_scenario_grp_num==6 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ylim(0,100) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                     values=cbPalette9[c(8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_6_clim_",clim_num,".jpg"),
         plot=gNexp, width=9, height=6, dpi=300)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_6_clim_",clim_num,".jpg"),
         plot=gNfut, width=9, height=6, dpi=300)


  #*************************************************************

## GWP bar charts ----------------------------------------------------------


y_breaks <- seq(-50, 150, by = 10)

  ## GWP model means with error bars
gGWPeb <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_GWP, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_GWP-sd_GWP, ymax=mean_GWP+sd_GWP),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 15.5,
           ymin = min(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           ymax = max(scenario_means[scenario_means$Climate_Scenario==clim_num,"mean_GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste0(site_name," Global Warming Potential by ",
                end_fut_period_year,"-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gGWPeb

## GWP model means with each individual model included
gGWPam <- summary_output[summary_output$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev,y=GWP,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_GWP), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 15.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"GWP"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(-50,60) +
  ggtitle(paste0(site_name," Global Warming Potential by ",
                 end_fut_period_year,"-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gGWPam

gGWPmm <- gwp_means_piv[gwp_means_piv$source %in% c("mean_CO2e_SOC","mean_CO2e_N2O","mean_CO2e_CH4") &
                          gwp_means_piv$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=vals, fill=source)) +
  geom_col(position="stack") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 15.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"vals"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(-20,20) +
  ggtitle(paste0(site_name," Global Warming Potential by Source by ",
                end_fut_period_year,"-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Source") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gGWPmm


ggsave(filename=paste0(results_path,"pub_GWP_all_scenarios_errorbars_",clim_num,".jpg"),
       plot=gGWPeb, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_GWP_all_scenarios_allmodels",clim_num,".jpg"),
       plot=gGWPam, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_GWP_by_source_all_scenarios_",clim_num,".jpg"),
       plot=gGWPmm, width=9, height=6, dpi=300)

#*************************************************************

## Change over time charts -------------------------------------------------

#crop_ylim <- c(-2,1.5)
crop_ylim <- c(-4,2)

## Maize all models
gMYchg_am <- summary_output[summary_output$Climate_Scenario==clim_num &
                              !is.na(summary_output$Maize_Diff_Mgha),] %>%
  ggplot(aes(x=scenario_abbrev,y=Maize_Diff_Mgha,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_MaizeYld_Mgha), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 15.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Maize_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(crop_ylim) +
  ggtitle(paste0(site_name," Change in Maize Yield by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  #scale_y_continuous(breaks=crop_ylim) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gMYchg_am

## Soybean all models
gSYchg_am <- summary_output[summary_output$Climate_Scenario==clim_num &
                              !is.na(summary_output$Soybean_Diff_Mgha),] %>%
  ggplot(aes(x=scenario_abbrev,y=Soybean_Diff_Mgha,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_SoyYld_Mgha), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 15.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Soybean_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(crop_ylim) +
  ggtitle(paste0(site_name," Change in Soybean Yield by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gSYchg_am


## Wheat all models
gWYchg_am <- summary_output[summary_output$Climate_Scenario==clim_num &
                              !is.na(summary_output$Wheat_Diff_Mgha),] %>%
  ggplot(aes(x=scenario_abbrev,y=Wheat_Diff_Mgha,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_WheatYld_Mgha), color= "black", 
           fill=NA, position="dodge") + 
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 15.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"Wheat_Diff_Mgha"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(crop_ylim) +
  ggtitle(paste0(site_name," Change in Wheat Yield by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gWYchg_am


## GWP model means with each individual model included

## SOC
gSOCchg_am <- summary_output[summary_output$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev,y=CO2e_SOC,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CO2e_SOC), color= "black", 
           fill=NA, position="dodge") +  
  annotate("rect", xmin = 0.5, xmax = 5.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T), 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 6.5, xmax = 7.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 7.5, xmax = 8.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 8.5, xmax = 12.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 12.5, xmax = 15.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_SOC"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(-155,80) +
  ggtitle(paste0(site_name," Change in CO2e-SOC by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gSOCchg_am

## N2O all models
gN2Ochg_am <- summary_output[summary_output$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev,y=CO2e_N2O,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CO2e_N2O), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0,
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 8.5, xmax = 12.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 12.5, xmax = 15.5, ymin = 0, 
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_N2O"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(0,30) +
  ggtitle(paste0(site_name," Change in CO2e-N2O by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gN2Ochg_am

## CH4 all models
gCH4chg_am <- summary_output[summary_output$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev,y=CO2e_CH4,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CO2e_CH4), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T), 
           ymax = 0,
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 6.5, xmax = 7.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0,
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 7.5, xmax = 8.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0, 
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 8.5, xmax = 12.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0, 
           alpha = 0, color= "grey") +  
  annotate("rect", xmin = 12.5, xmax = 15.5, 
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = 0, 
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ylim(-8,0) +
  ggtitle(paste0(site_name," Change in CH4 by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gCH4chg_am

## CH4 all models
gCH4chg_am2 <- summary_output[summary_output$Climate_Scenario==clim_num &
                               !is.na(summary_output$CH4_Diff_kgha),] %>%
  ggplot(aes(x=scenario_abbrev,y=CO2e_CH4,fill=factor(Model))) +
  geom_col(position="dodge") +
  geom_col(data=scenario_means[scenario_means$Climate_Scenario==clim_num,],
           aes(x=scenario_abbrev, y=mean_CO2e_CH4), color= "black", 
           fill=NA, position="dodge") +
  annotate("rect", xmin = 0.5, xmax = 5.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 5.5, xmax = 6.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 7.5, xmax = 8.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 8.5, xmax = 12.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +
  annotate("rect", xmin = 12.5, xmax = 15.5,
           ymin = min(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           ymax = max(summary_output[summary_output$Climate_Scenario==clim_num,"CO2e_CH4"]*1.05, na.rm=T),
           alpha = 0, color= "grey") +ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste0(site_name," Change in CH4 by ",
                 end_fut_period_year,"-All Models"),
          paste("Climate Scenario:",climate_desc)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gCH4chg_am2

ggsave(filename=paste0(results_path,"pub_change_in_maize_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gMYchg_am, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_soybean_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gSYchg_am, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_wheat_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gWYchg_am, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_soc_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gSOCchg_am, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_n2o_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gN2Ochg_am, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_ch4_all_scenarios_all_models_",clim_num,".jpg"),
       plot=gCH4chg_am, width=9, height=6, dpi=300)
ggsave(filename=paste0(results_path,"pub_change_in_ch4_all_scenarios_all_models2_",clim_num,".jpg"),
       plot=gCH4chg_am2, width=9, height=6, dpi=300)

} # end for loop through climate scenarios



}) # end suppressMessages

