#######################################
# File: 10_Model_Ensemble_Results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios6
# Author: Ellen Maas
# Date: June 2022
# Description: Clones 10_Model_Ensemble_Results-combined_scenarios_and_sites_compbaseline
# and restricts results to just the baseline and high UKESM 
# future climate scenario. Also excludes Millennial model.
#######################################

suppressMessages({
  
  # start -------
  ## header and setup  ---------------
  
  print(paste0("Starting 10_Model_Ensemble_results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios6.R"))
  
  library(readxl)
  library(magrittr)
  library(lubridate)
  library(tidyverse)
  library(graphics)
  library(ggplot2)
  library(grid)
  library(ggpubr)
  
  
  #create results folder if it doesn't already exist
  these_results_folder <- paste0("Comb_results_2scen_",end_fut_period_year,"/")
  if(!dir.exists(these_results_folder)) dir.create(these_results_folder)
  
  climate_factor_order <- c("Baseline","UKESM")
  
  ### for 2-climate scenario purposes, change name of "UKESM_High" to "UKESM"
  scenario_df[scenario_df$climate_desc=="UKESM_High","climate_desc"] <- "UKESM"
  
  component_order_apsim <- c("N2O_profile_last","N2O_profile_change",
                       "NO3_0to60cm_last","NO3_0to60cm_change",
                       "SoilT_20cm_last","SoilT_20cm_change",
                       "SW_20cm_last","SW_20cm_change"
                       )
  component_order_daycent <- c("N2O_profile_last","N2O_profile_change",
                             "NO3_0to60cm_last","NO3_0to60cm_change",
                             "WFPS_10cm_last","WFPS_10cm_change",
                             "CH4_last","CH4_change",
                             "SoilT_10cm_last","SoilT_10cm_change"
  )
  
  #*************************************************************
  
  
  # Import component data --------------------------------------
  
  kbs_summary_output <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/summary_output_final.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(Climate_Scenario %in% c(1,5) & 
             Model != "Millennial")
  kbs_scenario_means <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/scenario_means.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(Climate_Scenario %in% c(1,5)) %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num"))  %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  kbs_annual_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                        "/annual_results.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(climate_scenario_num %in% c(1,5) & 
             model_name != "Millennial")
  kbs_mean_annual_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                             "/mean_annual_results.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(climate_scenario_num %in% c(1,5))
  kbs_daily_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                       "/daily_results.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(climate_scenario_num %in% c(1,5) & 
             model_name != "Millennial")
  kbs_mean_daily_results <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                            "/mean_daily_results.csv")) %>%
    mutate(site_name="KBS") %>%
    filter(climate_scenario_num %in% c(1,5))
  
  lrf_summary_output <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/summary_output_final.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(Climate_Scenario %in% c(1,5) & 
             Model != "Millennial")
  lrf_scenario_means <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/scenario_means.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(Climate_Scenario %in% c(1,5)) %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num"))  %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  lrf_annual_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                        "/annual_results.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(climate_scenario_num %in% c(1,5) & 
             model_name != "Millennial")
  lrf_mean_annual_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                             "/mean_annual_results.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(climate_scenario_num %in% c(1,5))
  lrf_daily_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                       "/daily_results.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(climate_scenario_num %in% c(1,5) & 
             model_name != "Millennial")
  lrf_mean_daily_results <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                            "/mean_daily_results.csv")) %>%
    mutate(site_name="LRF") %>%
    filter(climate_scenario_num %in% c(1,5))
  
  
  ## Rearrange data for output -----------------------------------------------
  
  
  kbs_summary_output_piv <- pivot_longer(kbs_summary_output,
                                         c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  kbs_scenario_means_piv <- pivot_longer(kbs_scenario_means,
                                         c(-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name,
                                           -climate_desc),
                                         names_to="source",values_to="vals")
  kbs_annual_results_piv <- pivot_longer(kbs_annual_results,
                                         c(-year,-model_name,-scenario_name,
                                           -climate_scenario_num,
                                           -mgmt_scenario_grp_num,
                                           -mgmt_scenario_opt_num,
                                           -scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals")
  kbs_mean_annual_results_piv <- pivot_longer(kbs_mean_annual_results,
                                              c(-year,-scenario_name,
                                                -climate_scenario_num,
                                                -mgmt_scenario_grp_num,
                                                -mgmt_scenario_opt_num,
                                                -scenario_abbrev,-site_name),
                                              names_to="source",values_to="vals")
  kbs_daily_results_piv <- pivot_longer(kbs_daily_results,
                                        c(-date,-year,-dayofyear,-model_name,
                                          -climate_scenario_num,
                                          -mgmt_scenario_grp_num,
                                          -mgmt_scenario_opt_num,
                                          -scenario_name,
                                          -scenario_abbrev,-site_name),
                                        names_to="source",values_to="vals")
  kbs_mean_daily_results_piv <- pivot_longer(kbs_mean_daily_results,
                                             c(-date,-year,-dayofyear,
                                               -climate_scenario_num,
                                               -mgmt_scenario_grp_num,
                                               -mgmt_scenario_opt_num,
                                               -scenario_name,
                                               -scenario_abbrev,-site_name),
                                             names_to="source",values_to="vals")
  lrf_summary_output_piv <- pivot_longer(lrf_summary_output,
                                         c(-Model,-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals") %>%
    left_join(unique(scenario_df[,c("climate_scenario_num",
                                    "climate_desc")]),
              by=c("Climate_Scenario"="climate_scenario_num")) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  lrf_scenario_means_piv <- pivot_longer(lrf_scenario_means,
                                         c(-Climate_Scenario,-Mgmt_Scenario,
                                           -Scenario_Name,-scenario_abbrev,-site_name,
                                           -climate_desc),
                                         names_to="source",values_to="vals")
  lrf_annual_results_piv <- pivot_longer(lrf_annual_results,
                                         c(-year,-model_name,-scenario_name,
                                           -climate_scenario_num,
                                           -mgmt_scenario_grp_num,
                                           -mgmt_scenario_opt_num,
                                           -scenario_abbrev,-site_name),
                                         names_to="source",values_to="vals")
  lrf_mean_annual_results_piv <- pivot_longer(lrf_mean_annual_results,
                                              c(-year,-scenario_name,
                                                -climate_scenario_num,
                                                -mgmt_scenario_grp_num,
                                                -mgmt_scenario_opt_num,
                                                -scenario_abbrev,-site_name),
                                              names_to="source",values_to="vals")
  lrf_daily_results_piv <- pivot_longer(lrf_daily_results,
                                        c(-date,-year,-dayofyear,-model_name,
                                          -climate_scenario_num,
                                          -mgmt_scenario_grp_num,
                                          -mgmt_scenario_opt_num,
                                          -scenario_name,
                                          -scenario_abbrev,-site_name),
                                        names_to="source",values_to="vals")
  lrf_mean_daily_results_piv <- pivot_longer(lrf_mean_daily_results,
                                             c(-date,-year,-dayofyear,
                                               -climate_scenario_num,
                                               -mgmt_scenario_grp_num,
                                               -mgmt_scenario_opt_num,
                                               -scenario_name,
                                               -scenario_abbrev,-site_name),
                                             names_to="source",values_to="vals")
  
  ##
  
  daily_results <- rbind(kbs_daily_results,lrf_daily_results)
  
  mean_daily_results <- rbind(kbs_mean_daily_results,lrf_mean_daily_results)
  
  summary_output_piv <- rbind(kbs_summary_output_piv,lrf_summary_output_piv) %>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  scenario_means_piv <- rbind(kbs_scenario_means_piv,lrf_scenario_means_piv)
  
  gwp_scenario_means <- left_join(rbind(kbs_scenario_means[,c("mean_CO2e_N2O","mean_CO2e_CH4",
                                                              "mean_CO2e_SOC","mean_GWP",
                                                              "scenario_abbrev","site_name",
                                                              "Climate_Scenario")],
                                        lrf_scenario_means[,c("mean_CO2e_N2O","mean_CO2e_CH4",
                                                              "mean_CO2e_SOC","mean_GWP",
                                                              "scenario_abbrev","site_name",
                                                              "Climate_Scenario")]),
                                  unique(scenario_df[,c("climate_scenario_num",
                                                        "climate_desc")]),
                                  by=c("Climate_Scenario"="climate_scenario_num"))%>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  gwp_scenario_means_piv <- pivot_longer(gwp_scenario_means,c(-scenario_abbrev,
                                                              -site_name,
                                                              -Climate_Scenario,
                                                              -climate_desc),
                                         names_to="source",
                                         values_to="vals")
  
  gwp_summary_output <- left_join(rbind(kbs_summary_output[,c("Climate_Scenario","scenario_abbrev",
                                                              "Mgmt_Scenario","Scenario_Name",
                                                              "Model","site_name",
                                                              "CO2e_N2O","CO2e_CH4",
                                                              "CO2e_SOC","GWP")],
                                        lrf_summary_output[,c("Climate_Scenario","scenario_abbrev",
                                                              "Mgmt_Scenario","Scenario_Name",
                                                              "Model","site_name",
                                                              "CO2e_N2O","CO2e_CH4",
                                                              "CO2e_SOC","GWP")]),
                                  unique(scenario_df[,c("climate_scenario_num",
                                                        "climate_desc")]),
                                  by=c("Climate_Scenario"="climate_scenario_num"))%>%
    mutate(climate_desc = factor(climate_desc,
                                 levels=climate_factor_order))
  
  gwp_summary_output_piv <- pivot_longer(gwp_summary_output,c(-scenario_abbrev,
                                                              -site_name,
                                                              -Climate_Scenario,
                                                              -climate_desc,
                                                              -Mgmt_Scenario,
                                                              -Scenario_Name,
                                                              -Model),
                                         names_to="source",
                                         values_to="vals")
  
  
  ### write data frame
  write.csv(gwp_scenario_means, file=paste0(these_results_folder,"/gwp_scenario_means.csv"),
            row.names=FALSE)
  write.csv(gwp_summary_output, file=paste0(these_results_folder,"/gwp_summary_output.csv"),
            row.names=FALSE)
  
  # Import GHG calibration data -----------------------------------------
  ## for publication
  
  ## N2O -----------------
  
  n2o_calib_output_df_piv_KBS <- read.table(file="KBS_results_2050/calib_n2o_df_piv.csv",
                                        header=TRUE,sep=",") %>%
    left_join(unique(scenario_df[,c("scenario_descriptor","scenario_abbrev")]),
              by=c("treatment_scen"="scenario_descriptor")) %>%
    mutate(site_name="KBS")
  
  n2o_calib_output_df_piv_LRF <- read.table(file="LRF_results_2050/calib_n2o_df_piv.csv",
                                            header=TRUE,sep=",") %>%
    left_join(unique(scenario_df[,c("scenario_descriptor","scenario_abbrev")]),
              by=c("treatment_scen"="scenario_descriptor")) %>%
    mutate(site_name="LRF")
  
  n2o_calib_output_df_piv <- rbind(n2o_calib_output_df_piv_KBS,
                                   n2o_calib_output_df_piv_LRF)
  
  n2o_calib_stats <- pivot_wider(n2o_calib_output_df_piv, 
                                 names_from = Source,
                                 values_from = n2o_val) %>%
    mutate(ao_pct_diff = round((APSIM-Observed)/Observed*100,1),
           do_pct_diff = round((Daycent-Observed)/Observed*100,1),
           ad_pct_diff = round((APSIM-Daycent)/Daycent*100,1)) %>%
    rowwise() %>%
    mutate(
      ens_mean = mean(c(APSIM, Daycent)),
      ens_pct_diff = (ens_mean - Observed) / Observed * 100
    ) %>%
    ungroup()
  
   temp_df <- n2o_calib_output_df_piv %>%
    group_by(site_name, Source) %>%
    summarise(source_mean = mean(n2o_val), .groups = "drop")
  
  n2o_calib_ens_mean <-
    bind_rows(
      temp_df,
      temp_df %>%
        filter(Source %in% c("APSIM", "Daycent")) %>%
        group_by(site_name) %>%
        summarise(Source = "Ensemble", source_mean = mean(source_mean), .groups = "drop")
    ) %>%
    arrange(site_name,Source) %>%
    pivot_wider(names_from="Source",
                 values_from="source_mean") %>%
    mutate(ens_obs_diff_pct = round((1-(Ensemble/Observed))*100,1))
  
    
  ## CH4 -------------------------
  
  ch4_calib_output_df_piv <- read.table(file="KBS_results_2050/calib_ch4_df_piv.csv",
                                        header=TRUE,sep=",") %>%
    left_join(unique(scenario_df[,c("scenario_descriptor","scenario_abbrev")]),
              by=c("treatment_scen"="scenario_descriptor")) %>%
    mutate(site_name="KBS")
  
  ch4_calib_stats <- pivot_wider(ch4_calib_output_df_piv, 
                                 names_from = Source,
                                 values_from = ch4_val) %>%
    mutate(pct_diff = round((1-(Daycent/Observed))*-100,1))
                                 
    

  # Import model explanatory components data --------------------------------
  
  ## KBS ----------
  
  kbs_model_components <- read.csv(paste0("KBS_results_",end_fut_period_year,
                                          "/Summary_future_output.csv")) %>%
    mutate(site_name="KBS",
           pub_climate_scenario=factor(case_when(Climate_Scenario==1 ~ "Baseline",
                                                 Climate_Scenario==2 ~ "GFDL_Low",
                                                 Climate_Scenario==3 ~ "GFDL_High",
                                                 Climate_Scenario==4 ~ "UKESM_Low",
                                                 Climate_Scenario==5 ~ "UKESM"),
                                       levels=climate_factor_order)
    )
  
  #### Need to convert APSIM NO3 from ghaday to kgha to match Daycent
  kbs_model_components[kbs_model_components$Model=="APSIM","NO3_20cm_first"] <- kbs_model_components[kbs_model_components$Model=="APSIM","NO3_20cm_first"]/1000
  kbs_model_components[kbs_model_components$Model=="APSIM","NO3_20cm_change"] <- kbs_model_components[kbs_model_components$Model=="APSIM","NO3_20cm_change"]/1000
  kbs_model_components[kbs_model_components$Model=="APSIM","NO3_40cm_first"] <- kbs_model_components[kbs_model_components$Model=="APSIM","NO3_40cm_first"]/1000
  kbs_model_components[kbs_model_components$Model=="APSIM","NO3_40cm_change"] <- kbs_model_components[kbs_model_components$Model=="APSIM","NO3_40cm_change"]/1000
  kbs_model_components[kbs_model_components$Model=="APSIM","NO3_60cm_first"] <- kbs_model_components[kbs_model_components$Model=="APSIM","NO3_60cm_first"]/1000
  kbs_model_components[kbs_model_components$Model=="APSIM","NO3_60cm_change"] <- kbs_model_components[kbs_model_components$Model=="APSIM","NO3_60cm_change"]/1000
  kbs_model_components[kbs_model_components$Model=="APSIM","NO3_0to60cm_first"] <- kbs_model_components[kbs_model_components$Model=="APSIM","NO3_0to60cm_first"]/1000
  kbs_model_components[kbs_model_components$Model=="APSIM","NO3_0to60cm_change"] <- kbs_model_components[kbs_model_components$Model=="APSIM","NO3_0to60cm_change"]/1000
  
  #### Convert Daycent CH4 oxidation to mg/m^2/day for easier y-axis reading
  kbs_model_components[kbs_model_components$Model=="Daycent","CH4_first"] <- kbs_model_components[kbs_model_components$Model=="Daycent","CH4_first"]*1000
  kbs_model_components[kbs_model_components$Model=="Daycent","CH4_change"] <- kbs_model_components[kbs_model_components$Model=="Daycent","CH4_change"]*1000
  
  
  ## LRF ----------
  
  lrf_model_components <- read.csv(paste0("LRF_results_",end_fut_period_year,
                                          "/Summary_future_output.csv")) %>%
    mutate(site_name="LRF",
           pub_climate_scenario=factor(case_when(Climate_Scenario==1 ~ "Baseline",
                                                 Climate_Scenario==2 ~ "GFDL_Low",
                                                 Climate_Scenario==3 ~ "GFDL_High",
                                                 Climate_Scenario==4 ~ "UKESM_Low",
                                                 Climate_Scenario==5 ~ "UKESM"),
                                       levels=climate_factor_order)
    )
  
  #### Need to convert APSIM NO3 from ghaday to kgha to match Daycent
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_5cm_first"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_5cm_first"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_5cm_change"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_5cm_change"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_15cm_first"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_15cm_first"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_15cm_change"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_15cm_change"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_35cm_first"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_35cm_first"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_35cm_change"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_35cm_change"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_60cm_first"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_60cm_first"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_60cm_change"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_60cm_change"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_0to60cm_first"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_0to60cm_first"]/1000
  lrf_model_components[lrf_model_components$Model=="APSIM","NO3_0to60cm_change"] <- lrf_model_components[lrf_model_components$Model=="APSIM","NO3_0to60cm_change"]/1000
  
  #### Convert Daycent CH4 oxidation to mg/m^2/day for easier y-axis reading
  lrf_model_components[lrf_model_components$Model=="Daycent","CH4_first"] <- lrf_model_components[lrf_model_components$Model=="Daycent","CH4_first"]*1000
  lrf_model_components[lrf_model_components$Model=="Daycent","CH4_change"] <- lrf_model_components[lrf_model_components$Model=="Daycent","CH4_change"]*1000
  
  
  # GWP -------------
  
  ## stats ---------------------------------------------------------------
  
  ## select the management treatments with the least and greatest change
  ## over time of values of each gwp component (N2O, CH4, and SOC), and 
  ## include the difference between them. Grouped by site and climate
  ## scenario.
  
  ### N2O
  min_co2e_n2o <- gwp_scenario_means[,c(1,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = mean_CO2e_N2O)
  
  max_co2e_n2o <- gwp_scenario_means[,c(1,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = mean_CO2e_N2O)
  
  ### CH4
  min_co2e_ch4 <- gwp_scenario_means[,c(2,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = mean_CO2e_CH4)
  
  max_co2e_ch4 <- gwp_scenario_means[,c(2,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = mean_CO2e_CH4)
  
  ### SOC
  min_co2e_soc <- gwp_scenario_means[,c(3,5:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = mean_CO2e_SOC)
  
  max_co2e_soc <- gwp_scenario_means[,c(3,5:8)]%>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = mean_CO2e_SOC)
  
  ### GWP
  min_gwp <- gwp_scenario_means[,c(4:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = mean_GWP)
  
  max_gwp <- gwp_scenario_means[,c(4:8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = mean_GWP)
  
  ### calculate diffs and combine into data frame
  
  summary_ghg_stats <- cbind(max_co2e_n2o$mean_CO2e_N2O-min_co2e_n2o$mean_CO2e_N2O,
                             min_co2e_n2o[,c("mean_CO2e_N2O","scenario_abbrev")],
                             max_co2e_n2o[,c("mean_CO2e_N2O","scenario_abbrev")],
                             max_co2e_ch4$mean_CO2e_CH4-min_co2e_ch4$mean_CO2e_CH4,
                             min_co2e_ch4[,c("mean_CO2e_CH4","scenario_abbrev")],
                             max_co2e_ch4[,c("mean_CO2e_CH4","scenario_abbrev")],
                             max_co2e_soc$mean_CO2e_SOC-min_co2e_soc$mean_CO2e_SOC,
                             min_co2e_soc[,c("mean_CO2e_SOC","scenario_abbrev")],
                             max_co2e_soc[,c("mean_CO2e_SOC","scenario_abbrev")],
                             max_gwp$mean_GWP-min_gwp$mean_GWP,
                             min_gwp[,c("mean_GWP","scenario_abbrev")],
                             max_gwp)
  colnames(summary_ghg_stats) <- c("diff_co2e_n2o","min_co2e_n2o","min_co2e_n2o_scen_abbr","max_co2e_n2o","max_co2e_n2o_scen_abbr",
                                   "diff_co2e_ch4","min_co2e_ch4","min_co2e_ch4_scen_abbr","max_co2e_ch4","max_co2e_ch4_scen_abbr",
                                   "diff_co2e_soc","min_co2e_soc","min_co2e_soc_scen_abbr","max_co2e_soc","max_co2e_soc_scen_abbr",
                                   "diff_co2e_gwp","min_co2e_gwp","min_co2e_gwp_scen_abbr","max_co2e_gwp",
                                   "scenario_abbrev","site_name","Climate_Scenario","climate_desc")
  
  ### write data frame
  write.csv(summary_ghg_stats, file=paste0(these_results_folder,"/summary_ghg_stats_min_max.csv"),
            row.names=FALSE)
  
  
  #***********************************************
  
  ### calculate diffs between each treatment and the "control" at each site
  
  control_trts <- gwp_scenario_means[(gwp_scenario_means$scenario_abbrev=="CR" & gwp_scenario_means$site_name=="KBS") |
                                       (gwp_scenario_means$scenario_abbrev=="RR00-CR" & gwp_scenario_means$site_name=="LRF"),]
  
  diffs_from_controls <- merge(control_trts,gwp_scenario_means,
                               by=c("site_name","climate_desc")) %>%
    mutate(diff_co2e_n2o=mean_CO2e_N2O.x-mean_CO2e_N2O.y,
           diff_co2e_ch4=mean_CO2e_CH4.x-mean_CO2e_CH4.y,
           diff_co2e_soc=mean_CO2e_SOC.x-mean_CO2e_SOC.y,
           diff_gwp=mean_GWP.x-mean_GWP.y) %>%
    select(site_name,climate_desc,Climate_Scenario.y,scenario_abbrev.y,
           diff_co2e_n2o,diff_co2e_ch4,diff_co2e_soc,diff_gwp)
  
  ### write data frame
  write.csv(diffs_from_controls, file=paste0(these_results_folder,"/diffs_from_controls.csv"),
            row.names=FALSE)
  
  ## select the management treatments with the least and greatest differences
  ## from control treatments in each gwp component (N2O, CH4, and SOC), and 
  ## include the difference between them. Grouped by site and climate
  ## scenario.
  
  ### N2O
  min_diff_co2e_n2o <- diffs_from_controls[,c(1:5)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = diff_co2e_n2o)
  
  max_diff_co2e_n2o <- diffs_from_controls[,c(1:5)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = diff_co2e_n2o)
  
  ### CH4
  min_diff_co2e_ch4 <- diffs_from_controls[,c(1:4,6)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = diff_co2e_ch4)
  
  max_diff_co2e_ch4 <- diffs_from_controls[,c(1:4,6)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = diff_co2e_ch4)
  
  ### SOC
  min_diff_co2e_soc <- diffs_from_controls[,c(1:4,7)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = diff_co2e_soc)
  
  max_diff_co2e_soc <- diffs_from_controls[,c(1:4,7)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = diff_co2e_soc)
  
  ### GWP
  min_diff_gwp <- diffs_from_controls[,c(1:4,8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_min(order_by = diff_gwp)
  
  max_diff_gwp <- diffs_from_controls[,c(1:4,8)] %>% 
    group_by(site_name,climate_desc) %>% 
    slice_max(order_by = diff_gwp)
  
  ### calculate diffs and combine into data frame
  
  summary_ghg_stats_diff_from_ctrl <- cbind(max_diff_co2e_n2o$diff_co2e_n2o-min_diff_co2e_n2o$diff_co2e_n2o,
                                            min_diff_co2e_n2o[,c("diff_co2e_n2o","scenario_abbrev.y")],
                                            max_diff_co2e_n2o[,c("diff_co2e_n2o","scenario_abbrev.y")],
                                            max_diff_co2e_ch4$diff_co2e_ch4-min_diff_co2e_ch4$diff_co2e_ch4,
                                            min_diff_co2e_ch4[,c("diff_co2e_ch4","scenario_abbrev.y")],
                                            max_diff_co2e_ch4[,c("diff_co2e_ch4","scenario_abbrev.y")],
                                            max_diff_co2e_soc$diff_co2e_soc-min_diff_co2e_soc$diff_co2e_soc,
                                            min_diff_co2e_soc[,c("diff_co2e_soc","scenario_abbrev.y")],
                                            max_diff_co2e_soc[,c("diff_co2e_soc","scenario_abbrev.y")],
                                            max_diff_gwp$diff_gwp-min_diff_gwp$diff_gwp,
                                            min_diff_gwp[,c("diff_gwp","scenario_abbrev.y")],
                                            max_diff_gwp[,c(5,2,4,1,3)])
  colnames(summary_ghg_stats_diff_from_ctrl) <- c("diff_co2e_n2o","min_co2e_n2o","min_co2e_n2o_scen_abbr",
                                                  "max_co2e_n2o","max_co2e_n2o_scen_abbr",
                                                  "diff_co2e_ch4","min_co2e_ch4","min_co2e_ch4_scen_abbr",
                                                  "max_co2e_ch4","max_co2e_ch4_scen_abbr",
                                                  "diff_co2e_soc","min_co2e_soc","min_co2e_soc_scen_abbr",
                                                  "max_co2e_soc","max_co2e_soc_scen_abbr",
                                                  "diff_co2e_gwp","min_co2e_gwp","min_co2e_gwp_scen_abbr",
                                                  "max_co2e_gwp","max_co2e_gwp_scen_abbr",
                                                  "scenario_abbrev","site_name","Climate_Scenario")
  
  ### write data frame
  write.csv(summary_ghg_stats_diff_from_ctrl, 
            file=paste0(these_results_folder,"/summary_ghg_stats_diff_from_ctrl.csv"),
            row.names=FALSE)
  
  
  #***********************************************
  
  ### calculate diffs between each climate scenario for the same treatment at each site
  
  base_trts <- gwp_scenario_means[gwp_scenario_means$Climate_Scenario==1,]
  
  diffs_from_base <- merge(base_trts,gwp_scenario_means,
                           by=c("site_name","scenario_abbrev")) %>%
    mutate(diff_co2e_n2o=mean_CO2e_N2O.x-mean_CO2e_N2O.y,
           diff_co2e_ch4=mean_CO2e_CH4.x-mean_CO2e_CH4.y,
           diff_co2e_soc=mean_CO2e_SOC.x-mean_CO2e_SOC.y,
           diff_gwp=mean_GWP.x-mean_GWP.y) %>%
    select(site_name,climate_desc.y,Climate_Scenario.y,scenario_abbrev,
           diff_co2e_n2o,diff_co2e_ch4,diff_co2e_soc,diff_gwp)
  
  ### write data frame
  write.csv(diffs_from_base, 
            file=paste0(these_results_folder,"/diffs_from_base_climate.csv"),
            row.names=FALSE)
  
  ### select all management treatments for all climate scenarios, reporting
  ### the climate scenario with the least and greatest differences from
  ### the baseline climate for each management scenario.
  ### NOTE: for this script version of limiting future climate scenarios to just 
  ### the baseline and one CMIP6, this will only report the one CMIP6 model
  ### for all scenarios, so it's not of use for the point-scale study.
  
  ### N2O
  min_diff_co2e_n2o_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:5)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_min(order_by = diff_co2e_n2o)
  
  max_diff_co2e_n2o_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:5)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_max(order_by = diff_co2e_n2o)
  
  ### CH4
  min_diff_co2e_ch4_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,6)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_min(order_by = diff_co2e_ch4)
  
  max_diff_co2e_ch4_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,6)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_max(order_by = diff_co2e_ch4)
  
  ### SOC
  pre_min_diff_co2e_soc_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,7)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_min(order_by = diff_co2e_soc) 
  
  pre_max_diff_co2e_soc_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,7)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_max(order_by = diff_co2e_soc)
  
  # reduce duplicates to the minimum climate scenario
  min_diff_co2e_soc_clim <- pre_min_diff_co2e_soc_clim %>%
    group_by(site_name,scenario_abbrev) %>% 
    summarize(site_name=unique(site_name),
              Climate_Scenario.y=min(Climate_Scenario.y),
              diff_co2e_soc=unique(diff_co2e_soc)
    ) %>%
    left_join(unique(scenario_df[,c("climate_scenario_num","climate_desc")]),
              by=c("Climate_Scenario.y"="climate_scenario_num")) 
  
  max_diff_co2e_soc_clim <- pre_max_diff_co2e_soc_clim %>%
    group_by(site_name,scenario_abbrev) %>% 
    summarize(site_name=unique(site_name),
              Climate_Scenario.y=min(Climate_Scenario.y),
              diff_co2e_soc=unique(diff_co2e_soc)
    ) %>%
    left_join(unique(scenario_df[,c("climate_scenario_num","climate_desc")]),
              by=c("Climate_Scenario.y"="climate_scenario_num")) 
  
  ### GWP
  min_diff_gwp_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,8)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_min(order_by = diff_gwp)
  
  max_diff_gwp_clim <- diffs_from_base[diffs_from_base$climate_desc.y!="Baseline",c(1:4,8)] %>% 
    group_by(site_name,scenario_abbrev) %>% 
    slice_max(order_by = diff_gwp)
  
  ### calculate diffs and combine into data frame
  
  summary_ghg_stats_diff_from_base_climate <- cbind(max_diff_co2e_n2o_clim$diff_co2e_n2o-min_diff_co2e_n2o_clim$diff_co2e_n2o,
                                                    min_diff_co2e_n2o_clim[,c("diff_co2e_n2o","climate_desc.y")],
                                                    max_diff_co2e_n2o_clim[,c("diff_co2e_n2o","climate_desc.y")],
                                                    max_diff_co2e_ch4_clim$diff_co2e_ch4-min_diff_co2e_ch4_clim$diff_co2e_ch4,
                                                    min_diff_co2e_ch4_clim[,c("diff_co2e_ch4","climate_desc.y")],
                                                    max_diff_co2e_ch4_clim[,c("diff_co2e_ch4","climate_desc.y")],
                                                    max_diff_co2e_soc_clim$diff_co2e_soc-min_diff_co2e_soc_clim$diff_co2e_soc,
                                                    min_diff_co2e_soc_clim[,c("diff_co2e_soc","climate_desc")],
                                                    max_diff_co2e_soc_clim[,c("diff_co2e_soc","climate_desc")],
                                                    max_diff_gwp_clim$diff_gwp-min_diff_gwp_clim$diff_gwp,
                                                    min_diff_gwp_clim[,c("diff_gwp","climate_desc.y")],
                                                    max_diff_gwp_clim[,c(5,2,4,1,3)])
  
  colnames(summary_ghg_stats_diff_from_base_climate) <- c("diff_co2e_n2o","min_co2e_n2o","min_co2e_n2o_scen_abbr",
                                                          "max_co2e_n2o","max_co2e_n2o_climate_desc",
                                                          "diff_co2e_ch4","min_co2e_ch4","min_co2e_ch4_scen_abbr",
                                                          "max_co2e_ch4","max_co2e_ch4_climate_desc",
                                                          "diff_co2e_soc","min_co2e_soc","min_co2e_soc_scen_abbr",
                                                          "max_co2e_soc","max_co2e_soc_climate_desc",
                                                          "diff_co2e_gwp","min_co2e_gwp","min_co2e_gwp_scen_abbr",
                                                          "max_co2e_gwp","max_co2e_gwp_climate_desc",
                                                          "scenario_abbrev","site_name","Climate_Scenario")
  
  ### write data frame
  write.csv(summary_ghg_stats_diff_from_base_climate, 
            file=paste0(these_results_folder,"/summary_ghg_stats_diff_from_base_climate.csv"),
            row.names=FALSE)
  
  
  #***********************************************
  
  ### diffs across climate scenarios by model
  ### (to answer diff for biochar compared to everything else and diffs
  ### between models)
  
  base_trts_bymodel <- gwp_summary_output[gwp_summary_output$Climate_Scenario==1,]
  
  ukesm_trts_bymodel <- gwp_summary_output[gwp_summary_output$Climate_Scenario==5,]
  
  diffs_from_base_bymodel <- merge(base_trts_bymodel,ukesm_trts_bymodel,
                                   by=c("site_name","scenario_abbrev","Model")) %>%
    mutate(diff_co2e_n2o=CO2e_N2O.x-CO2e_N2O.y,
           diff_co2e_ch4=CO2e_CH4.x-CO2e_CH4.y,
           diff_co2e_soc=CO2e_SOC.x-CO2e_SOC.y,
           diff_gwp=GWP.x-GWP.y) %>%
    select(Model,site_name,climate_desc.y,Climate_Scenario.y,scenario_abbrev,
           diff_co2e_n2o,diff_co2e_ch4,diff_co2e_soc,diff_gwp)
  
  colnames(diffs_from_base_bymodel) <- c("Model","site_name","climate_desc","Climate_Scenario","scenario_abbrev",
                                         "diff_co2e_n2o", "diff_co2e_ch4","diff_co2e_soc","diff_co2e_gwp"
  )
  
  biochar_mean_diffs_bymodel <-  aggregate(as.matrix(select(diffs_from_base_bymodel[substr(diffs_from_base_bymodel$scenario_abbrev,1,1)=="B",],
                                                            !c(Model,site_name,climate_desc,Climate_Scenario,scenario_abbrev)))
                                           ~ Model+site_name+climate_desc+Climate_Scenario,
                                           data=diffs_from_base_bymodel[substr(diffs_from_base_bymodel$scenario_abbrev,1,1)=="B",],
                                           FUN=mean,
                                           na.action=na.pass) %>%
    mutate(group_ind="biochar_means")
  notbiochar_mean_diffs_bymodel <-  aggregate(as.matrix(select(diffs_from_base_bymodel[substr(diffs_from_base_bymodel$scenario_abbrev,1,1)!="B",],
                                                               !c(Model,site_name,climate_desc,Climate_Scenario,scenario_abbrev)))
                                              ~ Model+site_name+climate_desc+Climate_Scenario,
                                              data=diffs_from_base_bymodel[substr(diffs_from_base_bymodel$scenario_abbrev,1,1)!="B",],
                                              FUN=mean,
                                              na.action=na.pass) %>%
    mutate(group_ind="nonbiochar_means")
  
  allscenario_mean_diffs_bymodel <-  aggregate(as.matrix(select(diffs_from_base_bymodel,
                                                                !c(Model,site_name,climate_desc,Climate_Scenario,scenario_abbrev)))
                                               ~ Model+site_name+climate_desc+Climate_Scenario,
                                               data=diffs_from_base_bymodel,
                                               FUN=mean,
                                               na.action=na.pass) %>%
    mutate(group_ind="allscenario_means")
  
  trtmt_mean_diffs_bymodel <- rbind(biochar_mean_diffs_bymodel,notbiochar_mean_diffs_bymodel,allscenario_mean_diffs_bymodel)
  
  ### now all mgmt scenarios individually
  allscenario_diffs_bymodel <-  aggregate(as.matrix(select(diffs_from_base_bymodel,
                                                           !c(Model,site_name,climate_desc,Climate_Scenario,scenario_abbrev)))
                                          ~ Model+site_name+climate_desc+Climate_Scenario+scenario_abbrev,
                                          data=diffs_from_base_bymodel,
                                          FUN=mean,
                                          na.action=na.pass)
  
  
  ### write data frames
  write.csv(trtmt_mean_diffs_bymodel, 
            file=paste0(these_results_folder,"/biochar_notbiochar_mean_diffs_from_baseline_bymodel.csv"),
            row.names=FALSE)
  write.csv(allscenario_diffs_bymodel, 
            file=paste0(these_results_folder,"/allscenario_diffs_from_baseline_bymodel.csv"),
            row.names=FALSE)
  
  
  #*************************************************************
  
  ## bar charts ----------------------------------------------------------
  
  ### treatment order ----------------------------
  # group_RR_CR <- c("RR00-CR","RR25-CR","RR50-CR")
  # group_RR_NT <- c("RR00-NT-CR","RR25-NT-CR","RR50-NT-CR")
  # 
  #   c("BC19-CR","BC38-CR","BC57-CR","BC76-CR","BC96-CR",
  #                  "CC-CR","CC-NT-CR","CN",
  #                  "RF05-CR","RF15-CR","RF25-CR","RF35-CR",
  #                  "RR00-CR","RR25-CR","RR50-CR",
  #                  "RR00-NT-CR","RR25-NT-CR","RR50-NT-CR")
  
  g_gwp <- summary_output_piv[summary_output_piv$source == "GWP",] %>%
    #mutate(scenario_abbrev = factor(scenario_abbrev, levels = c(setdiff(levels(scenario_abbrev), c(group_RR_CR, group_RR_NT)), group_RR_NT, group_RR_CR))) %>%
    ggplot(aes(x=as.factor(scenario_abbrev), y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_col(position="dodge",colour=NA) +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_GWP), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Net Greenhouse Gas Emissions by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent","RothC"),
                      values=cbPalette9[c(8,2,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_gwp
  
  ### analysis ----------------------
  
  co2e_gwp <- summary_output_piv[summary_output_piv$source == "GWP",]
  co2e_gwp_KBS_Baseline_APSIM_BC <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                               co2e_gwp$climate_desc=="Baseline" &
                                               co2e_gwp$Model=="APSIM",
                                             c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_gwp_KBS_Baseline_APSIM <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                            co2e_gwp$climate_desc=="Baseline" &
                                            co2e_gwp$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_gwp_KBS_Baseline_APSIM_all <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                                co2e_gwp$climate_desc=="Baseline" &
                                                co2e_gwp$Model=="APSIM",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_KBS_Baseline_Daycent <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                              co2e_gwp$climate_desc=="Baseline" &
                                              co2e_gwp$Model=="Daycent",
                                            c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_KBS_Baseline_RothC <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                            co2e_gwp$climate_desc=="Baseline" &
                                            co2e_gwp$Model=="RothC",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_KBS_UKESM_APSIM_BC <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                            co2e_gwp$climate_desc=="UKESM" &
                                            co2e_gwp$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_gwp_KBS_UKESM_APSIM <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                         co2e_gwp$climate_desc=="UKESM" &
                                         co2e_gwp$Model=="APSIM",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_gwp_KBS_UKESM_APSIM_all <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                             co2e_gwp$climate_desc=="UKESM" &
                                             co2e_gwp$Model=="APSIM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_KBS_UKESM_Daycent <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                           co2e_gwp$climate_desc=="UKESM" &
                                           co2e_gwp$Model=="Daycent",
                                         c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_KBS_UKESM_RothC <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                         co2e_gwp$climate_desc=="UKESM" &
                                         co2e_gwp$Model=="RothC",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_LRF_Baseline_APSIM_BC <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                               co2e_gwp$climate_desc=="Baseline" &
                                               co2e_gwp$Model=="APSIM",
                                             c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_gwp_LRF_Baseline_APSIM <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                            co2e_gwp$climate_desc=="Baseline" &
                                            co2e_gwp$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_gwp_LRF_Baseline_APSIM_all <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                                co2e_gwp$climate_desc=="Baseline" &
                                                co2e_gwp$Model=="APSIM",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_LRF_Baseline_Daycent <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                              co2e_gwp$climate_desc=="Baseline" &
                                              co2e_gwp$Model=="Daycent",
                                            c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_LRF_Baseline_RothC <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                            co2e_gwp$climate_desc=="Baseline" &
                                            co2e_gwp$Model=="RothC",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_LRF_UKESM_APSIM_BC <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                            co2e_gwp$climate_desc=="UKESM" &
                                            co2e_gwp$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_gwp_LRF_UKESM_APSIM <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                         co2e_gwp$climate_desc=="UKESM" &
                                         co2e_gwp$Model=="APSIM",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_gwp_LRF_UKESM_APSIM_all <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                             co2e_gwp$climate_desc=="UKESM" &
                                             co2e_gwp$Model=="APSIM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_LRF_UKESM_Daycent <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                           co2e_gwp$climate_desc=="UKESM" &
                                           co2e_gwp$Model=="Daycent",
                                         c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwp_LRF_UKESM_RothC <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                         co2e_gwp$climate_desc=="UKESM" &
                                         co2e_gwp$Model=="RothC",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  
  # diffs between treatments per model
  co2e_gwp_treat_range_KBS_Baseline_APSIM <- round(max(co2e_gwp_KBS_Baseline_APSIM$vals)-min(co2e_gwp_KBS_Baseline_APSIM$vals),1)
  co2e_gwp_treat_range_KBS_Baseline_Daycent <- round(max(co2e_gwp_KBS_Baseline_Daycent$vals)-min(co2e_gwp_KBS_Baseline_Daycent$vals),1)
  co2e_gwp_treat_range_KBS_Baseline_RothC <- round(max(co2e_gwp_KBS_Baseline_RothC$vals)-min(co2e_gwp_KBS_Baseline_RothC$vals),1)
  co2e_gwp_treat_range_KBS_UKESM_APSIM <- round(max(co2e_gwp_KBS_UKESM_APSIM$vals)-min(co2e_gwp_KBS_UKESM_APSIM$vals),1)
  co2e_gwp_treat_range_KBS_UKESM_Daycent <- round(max(co2e_gwp_KBS_UKESM_Daycent$vals)-min(co2e_gwp_KBS_UKESM_Daycent$vals),1)
  co2e_gwp_treat_range_KBS_UKESM_RothC <- round(max(co2e_gwp_KBS_UKESM_RothC$vals)-min(co2e_gwp_KBS_UKESM_RothC$vals),1)
  co2e_gwp_treat_range_LRF_Baseline_APSIM <- round(max(co2e_gwp_LRF_Baseline_APSIM$vals)-min(co2e_gwp_LRF_Baseline_APSIM$vals),1)
  co2e_gwp_treat_range_LRF_Baseline_Daycent <- round(max(co2e_gwp_LRF_Baseline_Daycent$vals)-min(co2e_gwp_LRF_Baseline_Daycent$vals),1)
  co2e_gwp_treat_range_LRF_Baseline_RothC <- round(max(co2e_gwp_LRF_Baseline_RothC$vals)-min(co2e_gwp_LRF_Baseline_RothC$vals),1)
  co2e_gwp_treat_range_LRF_UKESM_APSIM <-round( max(co2e_gwp_LRF_UKESM_APSIM$vals)-min(co2e_gwp_LRF_UKESM_APSIM$vals),1)
  co2e_gwp_treat_range_LRF_UKESM_Daycent <- round(max(co2e_gwp_LRF_UKESM_Daycent$vals)-min(co2e_gwp_LRF_UKESM_Daycent$vals),1)
  co2e_gwp_treat_range_LRF_UKESM_RothC <- round(max(co2e_gwp_LRF_UKESM_RothC$vals)-min(co2e_gwp_LRF_UKESM_RothC$vals),1)
  
  write.csv(c(co2e_gwp_treat_range_KBS_Baseline_APSIM,co2e_gwp_treat_range_KBS_Baseline_Daycent,
              co2e_gwp_treat_range_KBS_Baseline_RothC,co2e_gwp_treat_range_KBS_UKESM_APSIM,
              co2e_gwp_treat_range_KBS_UKESM_Daycent,co2e_gwp_treat_range_KBS_UKESM_RothC,
              co2e_gwp_treat_range_LRF_Baseline_APSIM,co2e_gwp_treat_range_LRF_Baseline_Daycent,
              co2e_gwp_treat_range_LRF_Baseline_RothC,co2e_gwp_treat_range_LRF_UKESM_APSIM,
              co2e_gwp_treat_range_LRF_UKESM_Daycent,co2e_gwp_treat_range_LRF_UKESM_RothC),
            file=paste0(these_results_folder,
                        "co2e_gwp_treat_ranges.csv"),
            row.names=TRUE)
  
  co2e_gwp_treat_range_BC_KBS_Baseline_APSIM <- round(max(co2e_gwp_KBS_Baseline_APSIM_BC$vals)-min(co2e_gwp_KBS_Baseline_APSIM_BC$vals),1)
  co2e_gwp_treat_range_BC_KBS_UKESM_APSIM <- round(max(co2e_gwp_KBS_UKESM_APSIM_BC$vals)-min(co2e_gwp_KBS_UKESM_APSIM_BC$vals),1)
  co2e_gwp_treat_range_BC_LRF_Baseline_APSIM <- round(max(co2e_gwp_LRF_Baseline_APSIM_BC$vals)-min(co2e_gwp_LRF_Baseline_APSIM_BC$vals),1)
  co2e_gwp_treat_range_BC_LRF_UKESM_APSIM <- round(max(co2e_gwp_LRF_UKESM_APSIM_BC$vals)-min(co2e_gwp_LRF_UKESM_APSIM_BC$vals),1)
  
  # diffs between models per treatment
  co2e_gwp_model_range_KBS_Baseline <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                                  co2e_gwp$climate_desc=="Baseline",
                                                c("scenario_abbrev","vals")]  %>%
    filter(!grepl("BC",scenario_abbrev)) %>%
    group_by(scenario_abbrev) %>%
    summarize(kb_range_vals=round(max(vals)-min(vals),1),
              kb_min_vals=min(vals),
              kb_max_vals=max(vals))
  co2e_gwp_model_range_KBS_UKESM <- co2e_gwp[co2e_gwp$site_name=="KBS" &
                                                  co2e_gwp$climate_desc=="UKESM",
                                                c("scenario_abbrev","vals")]  %>%
    filter(!grepl("BC",scenario_abbrev)) %>%
    group_by(scenario_abbrev) %>%
    summarize(ku_range_vals=round(max(vals)-min(vals),1),
              ku_min_vals=min(vals),
              ku_max_vals=max(vals))
  co2e_gwp_model_range_LRF_Baseline <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                                  co2e_gwp$climate_desc=="Baseline",
                                                c("scenario_abbrev","vals")] %>%
    filter(!grepl("BC",scenario_abbrev)) %>%
    group_by(scenario_abbrev) %>%
    summarize(lb_range_vals=round(max(vals)-min(vals),1),
              lb_min_vals=min(vals),
              lb_max_vals=max(vals))
  co2e_gwp_model_range_LRF_UKESM <- co2e_gwp[co2e_gwp$site_name=="LRF" &
                                                  co2e_gwp$climate_desc=="UKESM",
                                                c("scenario_abbrev","vals")] %>%
    filter(!grepl("BC",scenario_abbrev)) %>%
    group_by(scenario_abbrev) %>%
    summarize(lu_range_vals=round(max(vals)-min(vals),1),
              lu_min_vals=min(vals),
              lu_max_vals=max(vals))
  
  
  co2e_gwp_model_range_KBS <- cbind(co2e_gwp_model_range_KBS_Baseline,
                                    co2e_gwp_model_range_KBS_UKESM[,c("ku_range_vals","ku_min_vals","ku_max_vals")])
  co2e_gwp_model_range_LRF <- cbind(co2e_gwp_model_range_LRF_Baseline,
                                    co2e_gwp_model_range_LRF_UKESM[,c("lu_range_vals","lu_min_vals","lu_max_vals")])

  #  
  co2e_gwp_mean_KBS_Baseline_APSIM <- round(mean(co2e_gwp_KBS_Baseline_APSIM$vals),1)
  co2e_gwp_mean_KBS_Baseline_Daycent <-  round(mean(co2e_gwp_KBS_Baseline_Daycent$vals),1)
  co2e_gwp_mean_KBS_Baseline_RothC <-  round(mean(co2e_gwp_KBS_Baseline_RothC$vals),1)
  co2e_gwp_mean_KBS_UKESM_APSIM <-  round(mean(co2e_gwp_KBS_UKESM_APSIM$vals),1)
  co2e_gwp_mean_KBS_UKESM_Daycent <-  round(mean(co2e_gwp_KBS_UKESM_Daycent$vals),1)
  co2e_gwp_mean_KBS_UKESM_RothC <-  round(mean(co2e_gwp_KBS_UKESM_RothC$vals),1)
  co2e_gwp_mean_LRF_Baseline_APSIM <-  round(mean(co2e_gwp_LRF_Baseline_APSIM$vals),1)
  co2e_gwp_mean_LRF_Baseline_Daycent <-  round(mean(co2e_gwp_LRF_Baseline_Daycent$vals),1)
  co2e_gwp_mean_LRF_Baseline_RothC <-  round(mean(co2e_gwp_LRF_Baseline_RothC$vals),1)
  co2e_gwp_mean_LRF_UKESM_APSIM <-  round(mean(co2e_gwp_LRF_UKESM_APSIM$vals),1)
  co2e_gwp_mean_LRF_UKESM_Daycent <-  round(mean(co2e_gwp_LRF_UKESM_Daycent$vals),1)
  co2e_gwp_mean_LRF_UKESM_RothC <-  round(mean(co2e_gwp_LRF_UKESM_RothC$vals),1)
  
  co2e_gwp_mean_KBS_Baseline_APSIM_all <- round(mean(co2e_gwp_KBS_Baseline_APSIM_all$vals),1)
  co2e_gwp_mean_KBS_UKESM_APSIM_all <-  round(mean(co2e_gwp_KBS_UKESM_APSIM_all$vals),1)
  co2e_gwp_mean_LRF_Baseline_APSIM_all <-  round(mean(co2e_gwp_LRF_Baseline_APSIM_all$vals),1)
  co2e_gwp_mean_LRF_UKESM_APSIM_all <-  round(mean(co2e_gwp_LRF_UKESM_APSIM_all$vals),1)
  
  co2e_gwp_mean <- data.frame(site=c("KBS","KBS","KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF","LRF","LRF",
                                     "KBS","KBS","LRF","LRF"),
                              climate_scen=c("Baseline","Baseline","Baseline","UKESM","UKESM","UKESM",
                                             "Baseline","Baseline","Baseline","UKESM","UKESM","UKESM",
                                             "Baseline","UKESM","Baseline","UKESM"),
                              model=c("APSIM","Daycent","RothC","APSIM","Daycent","RothC",
                                      "APSIM","Daycent","RothC","APSIM","Daycent","RothC",
                                      "APSIM all","APSIM all","APSIM all","APSIM all"),
                              mean_val=c(co2e_gwp_mean_KBS_Baseline_APSIM,co2e_gwp_mean_KBS_Baseline_Daycent,
                                         co2e_gwp_mean_KBS_Baseline_RothC,co2e_gwp_mean_KBS_UKESM_APSIM,
                                         co2e_gwp_mean_KBS_UKESM_Daycent,co2e_gwp_mean_KBS_UKESM_RothC,
                                         co2e_gwp_mean_LRF_Baseline_APSIM,co2e_gwp_mean_LRF_Baseline_Daycent,
                                         co2e_gwp_mean_LRF_Baseline_RothC,co2e_gwp_mean_LRF_UKESM_APSIM,
                                         co2e_gwp_mean_LRF_UKESM_Daycent,co2e_gwp_mean_LRF_UKESM_RothC,
                                         co2e_gwp_mean_KBS_Baseline_APSIM_all,co2e_gwp_mean_KBS_UKESM_APSIM_all,
                                         co2e_gwp_mean_LRF_Baseline_APSIM_all,co2e_gwp_mean_LRF_UKESM_APSIM_all))
  
  write.csv(co2e_gwp_mean,
            file=paste0(these_results_folder,
                        "co2e_gwp_means.csv"),
            row.names=FALSE)
  
  # # may be comparing apples to oranges because different crops, residue retention, other treatments
  # co2e_gwp_site_mean_diff_Baseline_APSIM <- co2e_gwp_mean_LRF_Baseline_APSIM-co2e_gwp_mean_KBS_Baseline_APSIM
  # co2e_gwp_site_mean_diff_Baseline_Daycent <- co2e_gwp_mean_LRF_Baseline_Daycent-co2e_gwp_mean_KBS_Baseline_Daycent
  # co2e_gwp_site_mean_diff_Baseline_RothC <- co2e_gwp_mean_LRF_Baseline_RothC-co2e_gwp_mean_KBS_Baseline_RothC
  # co2e_gwp_site_mean_diff_UKESM_APSIM <- co2e_gwp_mean_LRF_UKESM_APSIM-co2e_gwp_mean_KBS_UKESM_APSIM
  # co2e_gwp_site_mean_diff_UKESM_Daycent <- co2e_gwp_mean_LRF_UKESM_Daycent-co2e_gwp_mean_KBS_UKESM_Daycent
  # co2e_gwp_site_mean_diff_UKESM_RothC <- co2e_gwp_mean_LRF_UKESM_RothC-co2e_gwp_mean_KBS_UKESM_RothC
  
  # # % diff between models
  # co2e_gwp_model_pct_diff_KBS_Baseline_Daycent_RothC <- round((co2e_gwp_treat_range_KBS_Baseline_Daycent/co2e_gwp_treat_range_KBS_Baseline_RothC)*100)
  # co2e_gwp_model_pct_diff_KBS_UKESM_Daycent_RothC <- round((co2e_gwp_treat_range_KBS_UKESM_Daycent/co2e_gwp_treat_range_KBS_UKESM_RothC)*100)
  # co2e_gwp_model_pct_diff_LRF_Baseline_Daycent_RothC <- round((co2e_gwp_treat_range_LRF_Baseline_Daycent/co2e_gwp_treat_range_LRF_Baseline_RothC)*100)
  # co2e_gwp_model_pct_diff_LRF_UKESM_Daycent_RothC <- round((co2e_gwp_treat_range_LRF_UKESM_Daycent/co2e_gwp_treat_range_LRF_UKESM_RothC)*100)
  
  # diffs between climate scenarios for each model/site
  co2e_gwp_clim_diff_KBS_APSIM_BC <- round(as.numeric(co2e_gwp_KBS_UKESM_APSIM_BC$vals-co2e_gwp_KBS_Baseline_APSIM_BC$vals),1)
  co2e_gwp_clim_diff_KBS_APSIM <- as.numeric(co2e_gwp_KBS_UKESM_APSIM$vals-co2e_gwp_KBS_Baseline_APSIM$vals)
  co2e_gwp_clim_diff_KBS_Daycent <- as.numeric(co2e_gwp_KBS_UKESM_Daycent$vals-co2e_gwp_KBS_Baseline_Daycent$vals)
  co2e_gwp_clim_diff_KBS_RothC <- as.numeric(co2e_gwp_KBS_UKESM_RothC$vals-co2e_gwp_KBS_Baseline_RothC$vals)
  co2e_gwp_clim_diff_LRF_APSIM_BC <- round(as.numeric(co2e_gwp_LRF_UKESM_APSIM_BC$vals-co2e_gwp_LRF_Baseline_APSIM_BC$vals),1)
  co2e_gwp_clim_diff_LRF_APSIM <- as.numeric(co2e_gwp_LRF_UKESM_APSIM$vals-co2e_gwp_LRF_Baseline_APSIM$vals)
  co2e_gwp_clim_diff_LRF_Daycent <- as.numeric(co2e_gwp_LRF_UKESM_Daycent$vals-co2e_gwp_LRF_Baseline_Daycent$vals)
  co2e_gwp_clim_diff_LRF_RothC <- as.numeric(co2e_gwp_LRF_UKESM_RothC$vals-co2e_gwp_LRF_Baseline_RothC$vals)
  
  co2e_gwp_clim_diff_KBS_df <- data.frame(rbind(co2e_gwp_clim_diff_KBS_APSIM,
                                                co2e_gwp_clim_diff_KBS_Daycent,
                                                co2e_gwp_clim_diff_KBS_RothC))
  names(co2e_gwp_clim_diff_KBS_df)<-c("CC_CR",
                                      "RF05_CR",
                                      "RF15_CR",
                                      "RF25_CR",
                                      "RF35_CR",
                                      "RR00_CR",
                                      "RR00_NT_CR",
                                      "RR25_CR",
                                      "RR25_NT_CR",
                                      "RR50_CR",
                                      "RR50_NT_CR")
  co2e_gwp_clim_diff_KBS_df <- co2e_gwp_clim_diff_KBS_df %>% 
    mutate(mean_all=rowMeans(select(co2e_gwp_clim_diff_KBS_df,
                                    CC_CR,
                                    RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                    RR00_CR,RR25_CR,RR50_CR,
                                    RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_RF=rowMeans(select(co2e_gwp_clim_diff_KBS_df,RF05_CR,RF15_CR,RF25_CR,RF35_CR),na.rm=TRUE),
           mean_RR=rowMeans(select(co2e_gwp_clim_diff_KBS_df,RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE),
           mean_all_NT=rowMeans(select(co2e_gwp_clim_diff_KBS_df,
                                       RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_not_NT=rowMeans(select(co2e_gwp_clim_diff_KBS_df,
                                       CC_CR,
                                       RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                       RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE))
  
  co2e_gwp_clim_diff_LRF_df <- data.frame(rbind(co2e_gwp_clim_diff_LRF_APSIM,
                                                co2e_gwp_clim_diff_LRF_Daycent,
                                                co2e_gwp_clim_diff_LRF_RothC))
  names(co2e_gwp_clim_diff_LRF_df)<-c("CC_CR",
                                      "CC_NT_CR",
                                      "CN",
                                      "RF05_CR",
                                      "RF15_CR",
                                      "RF25_CR",
                                      "RF35_CR",
                                      "RR00_CR",
                                      "RR00_NT_CR",
                                      "RR25_CR",
                                      "RR25_NT_CR",
                                      "RR50_CR",
                                      "RR50_NT_CR")
  co2e_gwp_clim_diff_LRF_df <- co2e_gwp_clim_diff_LRF_df %>% 
    mutate(mean_all=rowMeans(select(co2e_gwp_clim_diff_LRF_df,
                                    CC_CR,CC_NT_CR,CN,
                                    RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                    RR00_CR,RR25_CR,RR50_CR,
                                    RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_RF=rowMeans(select(co2e_gwp_clim_diff_LRF_df,RF05_CR,RF15_CR,RF25_CR,RF35_CR),na.rm=TRUE),
           mean_RR=rowMeans(select(co2e_gwp_clim_diff_LRF_df,RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE),
           mean_RR_NT=rowMeans(select(co2e_gwp_clim_diff_LRF_df,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_all_NT=rowMeans(select(co2e_gwp_clim_diff_LRF_df,
                                       CC_NT_CR,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_not_NT=rowMeans(select(co2e_gwp_clim_diff_LRF_df,
                                       CC_CR,CN,
                                       RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                       RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE))
  
  co2e_gwp_clim_diff_mean_KBS_APSIM_BC <- mean(co2e_gwp_clim_diff_KBS_APSIM_BC)
  co2e_gwp_clim_diff_mean_LRF_APSIM_BC <- mean(co2e_gwp_clim_diff_LRF_APSIM_BC)
  
  write.csv(co2e_gwp_clim_diff_KBS_df,
            file=paste0(these_results_folder,
                        "co2e_gwp_clim_diff_KBS.csv"),
            row.names=TRUE)
  write.csv(co2e_gwp_clim_diff_LRF_df,
            file=paste0(these_results_folder,
                        "co2e_gwp_clim_diff_LRF.csv"),
            row.names=TRUE)
  
  co2e_gwp_clim_diff_KBS_means <- round(colMeans(co2e_gwp_clim_diff_KBS_df),1)
  
  write.csv(co2e_gwp_clim_diff_KBS_means,
            file=paste0(these_results_folder,
                        "co2e_gwp_clim_diff_KBS_means.csv"))
  
  co2e_gwp_clim_diff_LRF_means <- round(colMeans(co2e_gwp_clim_diff_LRF_df),1)
  
  write.csv(co2e_gwp_clim_diff_LRF_means,
            file=paste0(these_results_folder,
                        "co2e_gwp_clim_diff_LRF_means.csv"))
  
  
  write.csv(co2e_gwp_clim_diff_KBS_APSIM_BC,
            file=paste0(these_results_folder,
                        "co2e_gwp_clim_diff_KBS_APSIM_BC.csv"))
  
  write.csv(co2e_gwp_clim_diff_LRF_APSIM_BC,
            file=paste0(these_results_folder,
                        "co2e_gwp_clim_diff_LRF_APSIM_BC.csv"))
  
  co2e_gwp_clim_diff_KBS_APSIM_BC_mean <- mean(co2e_gwp_clim_diff_KBS_APSIM_BC)
  co2e_gwp_clim_diff_LRF_APSIM_BC_mean <- mean(co2e_gwp_clim_diff_LRF_APSIM_BC)
  
  ### Model means ---------------------
  
  co2e_gwpmeans <- gwp_scenario_means_piv[gwp_scenario_means_piv$source == "mean_GWP",]
  co2e_gwpmeans_KBS_Baseline <- co2e_gwpmeans[co2e_gwpmeans$site_name=="KBS" &
                                                co2e_gwpmeans$climate_desc=="Baseline",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwpmeans_KBS_UKESM <- co2e_gwpmeans[co2e_gwpmeans$site_name=="KBS" &
                                             co2e_gwpmeans$climate_desc=="UKESM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwpmeans_LRF_Baseline <- co2e_gwpmeans[co2e_gwpmeans$site_name=="LRF" &
                                                co2e_gwpmeans$climate_desc=="Baseline",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_gwpmeans_LRF_UKESM <- co2e_gwpmeans[co2e_gwpmeans$site_name=="LRF" &
                                             co2e_gwpmeans$climate_desc=="UKESM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  
  
  co2e_gwpmeans_clim_diff_KBS <- as.numeric(co2e_gwpmeans_KBS_UKESM$vals-co2e_gwpmeans_KBS_Baseline$vals)
  co2e_gwpmeans_clim_diff_LRF <- as.numeric(co2e_gwpmeans_LRF_UKESM$vals-co2e_gwpmeans_LRF_Baseline$vals)
  
  co2e_gwpmeans_mean_clim_diff_KBS <- mean(co2e_gwpmeans_clim_diff_KBS)
  co2e_gwpmeans_mean_clim_diff_LRF <- mean(co2e_gwpmeans_clim_diff_LRF)
  
  ### addtl bar charts -------------------------------------
  
  g_gwp_nolegend <- summary_output_piv[summary_output_piv$source == "GWP",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_GWP), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Net Greenhouse Gas Emissions by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent","RothC"),
                      values=cbPalette9[c(8,2,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "none",
          legend.key = element_blank())
  
  g_gwp_nolegend
  
  g_gwp_meansonly <- gwp_scenario_means %>%
    ggplot(aes(x=scenario_abbrev, y=mean_GWP)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(color= "black",
             fill=NA, position="dodge") +
    ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    ylim(-27,43) +
    xlab("") +
    ggtitle(paste0("Net Greenhouse Gas Emissions by ",
                   end_fut_period_year)) +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_gwp_meansonly
  
  ggsave(filename=paste0(these_results_folder,"/pub_all_GWP.jpg"),
         plot=g_gwp, width=16, height=13, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_all_GWP_nolegend.jpg"),
         plot=g_gwp_nolegend, width=20, height=10, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_all_GWP_meansonly.jpg"),
         plot=g_gwp_meansonly, width=20, height=10, dpi=300)
  
  ### by source -----------------------------------------------------------
  
  
  g_gwp_source <- gwp_scenario_means_piv[gwp_scenario_means_piv$source %in% 
                                           c("mean_CO2e_SOC","mean_CO2e_N2O","mean_CO2e_CH4"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=source)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="stack") +
    ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("Net Greenhouse Gas Emissions by Source by ",end_fut_period_year)) +
    labs(fill = "source") +
    scale_fill_manual(labels=c(bquote("CH"[4][CO2e]),bquote("N"[2]*"O"[CO2e]),bquote("SOC"[CO2e])),
                      values=cbPalette12[c(10,11,12)],
                      name="Source") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_gwp_source
  
  ggsave(filename=paste0(these_results_folder,"/pub_all_GWP_by_source.jpg"),
         plot=g_gwp_source, width=16, height=13, dpi=300)
  
  ## components  SOC ------------------------------------------------------------
  
  g_soce <- summary_output_piv[summary_output_piv$source == "CO2e_SOC",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_SOC), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('SOC'[CO2e]*' (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(bquote("Change in SOC"[CO2e]*" by"~.(end_fut_period_year))) +
    scale_fill_manual(labels=c("APSIM","Daycent","RothC"),
                      values=cbPalette9[c(8,2,3)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_soce
  
  ### analysis ----------------------
  
  co2e_soc <- summary_output_piv[summary_output_piv$source == "CO2e_SOC",]
  co2e_soc_KBS_Baseline_APSIM_BC <- co2e_soc[co2e_soc$site_name=="KBS" &
                                          co2e_soc$climate_desc=="Baseline" &
                                          co2e_soc$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_soc_KBS_Baseline_APSIM <- co2e_soc[co2e_soc$site_name=="KBS" &
                                            co2e_soc$climate_desc=="Baseline" &
                                            co2e_soc$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_soc_KBS_Baseline_APSIM_all <- co2e_soc[co2e_soc$site_name=="KBS" &
                                               co2e_soc$climate_desc=="Baseline" &
                                               co2e_soc$Model=="APSIM",
                                             c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_KBS_Baseline_Daycent <- co2e_soc[co2e_soc$site_name=="KBS" &
                                          co2e_soc$climate_desc=="Baseline" &
                                          co2e_soc$Model=="Daycent",
                                          c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_KBS_Baseline_RothC <- co2e_soc[co2e_soc$site_name=="KBS" &
                                          co2e_soc$climate_desc=="Baseline" &
                                          co2e_soc$Model=="RothC",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_KBS_UKESM_APSIM_BC <- co2e_soc[co2e_soc$site_name=="KBS" &
                                        co2e_soc$climate_desc=="UKESM" &
                                        co2e_soc$Model=="APSIM",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_soc_KBS_UKESM_APSIM <- co2e_soc[co2e_soc$site_name=="KBS" &
                                            co2e_soc$climate_desc=="UKESM" &
                                            co2e_soc$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_soc_KBS_UKESM_APSIM_all <- co2e_soc[co2e_soc$site_name=="KBS" &
                                                co2e_soc$climate_desc=="UKESM" &
                                                co2e_soc$Model=="APSIM",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_KBS_UKESM_Daycent <- co2e_soc[co2e_soc$site_name=="KBS" &
                                          co2e_soc$climate_desc=="UKESM" &
                                          co2e_soc$Model=="Daycent",
                                         c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_KBS_UKESM_RothC <- co2e_soc[co2e_soc$site_name=="KBS" &
                                        co2e_soc$climate_desc=="UKESM" &
                                        co2e_soc$Model=="RothC",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_LRF_Baseline_APSIM_BC <- co2e_soc[co2e_soc$site_name=="LRF" &
                                        co2e_soc$climate_desc=="Baseline" &
                                        co2e_soc$Model=="APSIM",
                                        c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_soc_LRF_Baseline_APSIM <- co2e_soc[co2e_soc$site_name=="LRF" &
                                               co2e_soc$climate_desc=="Baseline" &
                                               co2e_soc$Model=="APSIM",
                                             c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_soc_LRF_Baseline_APSIM_all <- co2e_soc[co2e_soc$site_name=="LRF" &
                                                co2e_soc$climate_desc=="Baseline" &
                                                co2e_soc$Model=="APSIM",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_LRF_Baseline_Daycent <- co2e_soc[co2e_soc$site_name=="LRF" &
                                          co2e_soc$climate_desc=="Baseline" &
                                          co2e_soc$Model=="Daycent",
                                          c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_LRF_Baseline_RothC <- co2e_soc[co2e_soc$site_name=="LRF" &
                                        co2e_soc$climate_desc=="Baseline" &
                                        co2e_soc$Model=="RothC",
                                        c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_LRF_UKESM_APSIM_BC <- co2e_soc[co2e_soc$site_name=="LRF" &
                                        co2e_soc$climate_desc=="UKESM" &
                                        co2e_soc$Model=="APSIM",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_soc_LRF_UKESM_APSIM <- co2e_soc[co2e_soc$site_name=="LRF" &
                                            co2e_soc$climate_desc=="UKESM" &
                                            co2e_soc$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_soc_LRF_UKESM_APSIM_all <- co2e_soc[co2e_soc$site_name=="LRF" &
                                                co2e_soc$climate_desc=="UKESM" &
                                                co2e_soc$Model=="APSIM",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_LRF_UKESM_Daycent <- co2e_soc[co2e_soc$site_name=="LRF" &
                                          co2e_soc$climate_desc=="UKESM" &
                                          co2e_soc$Model=="Daycent",
                                         c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_soc_LRF_UKESM_RothC <- co2e_soc[co2e_soc$site_name=="LRF" &
                                        co2e_soc$climate_desc=="UKESM" &
                                        co2e_soc$Model=="RothC",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  
  # diffs between treatments
  co2e_soc_treat_range_KBS_Baseline_APSIM <- round(max(co2e_soc_KBS_Baseline_APSIM$vals)-min(co2e_soc_KBS_Baseline_APSIM$vals),1)
  co2e_soc_treat_range_KBS_Baseline_Daycent <- round(max(co2e_soc_KBS_Baseline_Daycent$vals)-min(co2e_soc_KBS_Baseline_Daycent$vals),1)
  co2e_soc_treat_range_KBS_Baseline_RothC <- round(max(co2e_soc_KBS_Baseline_RothC$vals)-min(co2e_soc_KBS_Baseline_RothC$vals),1)
  co2e_soc_treat_range_KBS_UKESM_APSIM <- round(max(co2e_soc_KBS_UKESM_APSIM$vals)-min(co2e_soc_KBS_UKESM_APSIM$vals),1)
  co2e_soc_treat_range_KBS_UKESM_Daycent <- round(max(co2e_soc_KBS_UKESM_Daycent$vals)-min(co2e_soc_KBS_UKESM_Daycent$vals),1)
  co2e_soc_treat_range_KBS_UKESM_RothC <- round(max(co2e_soc_KBS_UKESM_RothC$vals)-min(co2e_soc_KBS_UKESM_RothC$vals),1)
  co2e_soc_treat_range_LRF_Baseline_APSIM <- round(max(co2e_soc_LRF_Baseline_APSIM$vals)-min(co2e_soc_LRF_Baseline_APSIM$vals),1)
  co2e_soc_treat_range_LRF_Baseline_Daycent <- round(max(co2e_soc_LRF_Baseline_Daycent$vals)-min(co2e_soc_LRF_Baseline_Daycent$vals),1)
  co2e_soc_treat_range_LRF_Baseline_RothC <- round(max(co2e_soc_LRF_Baseline_RothC$vals)-min(co2e_soc_LRF_Baseline_RothC$vals),1)
  co2e_soc_treat_range_LRF_UKESM_APSIM <-round( max(co2e_soc_LRF_UKESM_APSIM$vals)-min(co2e_soc_LRF_UKESM_APSIM$vals),1)
  co2e_soc_treat_range_LRF_UKESM_Daycent <- round(max(co2e_soc_LRF_UKESM_Daycent$vals)-min(co2e_soc_LRF_UKESM_Daycent$vals),1)
  co2e_soc_treat_range_LRF_UKESM_RothC <- round(max(co2e_soc_LRF_UKESM_RothC$vals)-min(co2e_soc_LRF_UKESM_RothC$vals),1)
  
  write.csv(c(co2e_soc_treat_range_KBS_Baseline_APSIM,
              co2e_soc_treat_range_KBS_Baseline_Daycent,
              co2e_soc_treat_range_KBS_Baseline_RothC,
              co2e_soc_treat_range_KBS_UKESM_APSIM,
              co2e_soc_treat_range_KBS_UKESM_Daycent,
              co2e_soc_treat_range_KBS_UKESM_RothC,
              co2e_soc_treat_range_LRF_Baseline_APSIM,
              co2e_soc_treat_range_LRF_Baseline_Daycent,
              co2e_soc_treat_range_LRF_Baseline_RothC,
              co2e_soc_treat_range_LRF_UKESM_APSIM,
              co2e_soc_treat_range_LRF_UKESM_Daycent,
              co2e_soc_treat_range_LRF_UKESM_RothC),
            file=paste0(these_results_folder,
                        "co2e_soc_treat_ranges.csv"),
            row.names=TRUE)
  
  co2e_soc_treat_range_BC_KBS_Baseline_APSIM <- round(max(co2e_soc_KBS_Baseline_APSIM_BC$vals)-min(co2e_soc_KBS_Baseline_APSIM_BC$vals),1)
  co2e_soc_treat_range_BC_KBS_UKESM_APSIM <- round(max(co2e_soc_KBS_UKESM_APSIM_BC$vals)-min(co2e_soc_KBS_UKESM_APSIM_BC$vals),1)
  co2e_soc_treat_range_BC_LRF_Baseline_APSIM <- round(max(co2e_soc_LRF_Baseline_APSIM_BC$vals)-min(co2e_soc_LRF_Baseline_APSIM_BC$vals),1)
  co2e_soc_treat_range_BC_LRF_UKESM_APSIM <- round(max(co2e_soc_LRF_UKESM_APSIM_BC$vals)-min(co2e_soc_LRF_UKESM_APSIM_BC$vals),1)
  
  co2e_soc_mean_KBS_Baseline_APSIM <- round(mean(co2e_soc_KBS_Baseline_APSIM$vals),1)
  co2e_soc_mean_KBS_Baseline_Daycent <-  round(mean(co2e_soc_KBS_Baseline_Daycent$vals),1)
  co2e_soc_mean_KBS_Baseline_RothC <-  round(mean(co2e_soc_KBS_Baseline_RothC$vals),1)
  co2e_soc_mean_KBS_UKESM_APSIM <-  round(mean(co2e_soc_KBS_UKESM_APSIM$vals),1)
  co2e_soc_mean_KBS_UKESM_Daycent <-  round(mean(co2e_soc_KBS_UKESM_Daycent$vals),1)
  co2e_soc_mean_KBS_UKESM_RothC <-  round(mean(co2e_soc_KBS_UKESM_RothC$vals),1)
  co2e_soc_mean_LRF_Baseline_APSIM <-  round(mean(co2e_soc_LRF_Baseline_APSIM$vals),1)
  co2e_soc_mean_LRF_Baseline_Daycent <-  round(mean(co2e_soc_LRF_Baseline_Daycent$vals),1)
  co2e_soc_mean_LRF_Baseline_RothC <-  round(mean(co2e_soc_LRF_Baseline_RothC$vals),1)
  co2e_soc_mean_LRF_UKESM_APSIM <-  round(mean(co2e_soc_LRF_UKESM_APSIM$vals),1)
  co2e_soc_mean_LRF_UKESM_Daycent <-  round(mean(co2e_soc_LRF_UKESM_Daycent$vals),1)
  co2e_soc_mean_LRF_UKESM_RothC <-  round(mean(co2e_soc_LRF_UKESM_RothC$vals),1)

  co2e_soc_mean_KBS_Baseline_APSIM_all <- round(mean(co2e_soc_KBS_Baseline_APSIM_all$vals),1)
  co2e_soc_mean_KBS_UKESM_APSIM_all <-  round(mean(co2e_soc_KBS_UKESM_APSIM_all$vals),1)
  co2e_soc_mean_LRF_Baseline_APSIM_all <-  round(mean(co2e_soc_LRF_Baseline_APSIM_all$vals),1)
  co2e_soc_mean_LRF_UKESM_APSIM_all <-  round(mean(co2e_soc_LRF_UKESM_APSIM_all$vals),1)

  co2e_soc_mean <- data.frame(site=c("KBS","KBS","KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF","LRF","LRF",
                                     "KBS","KBS","LRF","LRF"),
                              climate_scen=c("Baseline","Baseline","Baseline","UKESM","UKESM","UKESM",
            "Baseline","Baseline","Baseline","UKESM","UKESM","UKESM",
            "Baseline","UKESM","Baseline","UKESM"),
            model=c("APSIM","Daycent","RothC","APSIM","Daycent","RothC",
            "APSIM","Daycent","RothC","APSIM","Daycent","RothC",
            "APSIM all","APSIM all","APSIM all","APSIM all"),
    mean_val=c(co2e_soc_mean_KBS_Baseline_APSIM,co2e_soc_mean_KBS_Baseline_Daycent,
      co2e_soc_mean_KBS_Baseline_RothC,co2e_soc_mean_KBS_UKESM_APSIM,
      co2e_soc_mean_KBS_UKESM_Daycent,co2e_soc_mean_KBS_UKESM_RothC,
      co2e_soc_mean_LRF_Baseline_APSIM,co2e_soc_mean_LRF_Baseline_Daycent,
      co2e_soc_mean_LRF_Baseline_RothC,co2e_soc_mean_LRF_UKESM_APSIM,
      co2e_soc_mean_LRF_UKESM_Daycent,co2e_soc_mean_LRF_UKESM_RothC,
      co2e_soc_mean_KBS_Baseline_APSIM_all,co2e_soc_mean_KBS_UKESM_APSIM_all,
      co2e_soc_mean_LRF_Baseline_APSIM_all,co2e_soc_mean_LRF_UKESM_APSIM_all))
  
  write.csv(co2e_soc_mean,
            file=paste0(these_results_folder,
                        "co2e_soc_means.csv"),
            row.names=FALSE)
  
  # # may be comparing apples to oranges because different crops, residue retention, other treatments
  # co2e_soc_site_mean_diff_Baseline_APSIM <- co2e_soc_mean_LRF_Baseline_APSIM-co2e_soc_mean_KBS_Baseline_APSIM
  # co2e_soc_site_mean_diff_Baseline_Daycent <- co2e_soc_mean_LRF_Baseline_Daycent-co2e_soc_mean_KBS_Baseline_Daycent
  # co2e_soc_site_mean_diff_Baseline_RothC <- co2e_soc_mean_LRF_Baseline_RothC-co2e_soc_mean_KBS_Baseline_RothC
  # co2e_soc_site_mean_diff_UKESM_APSIM <- co2e_soc_mean_LRF_UKESM_APSIM-co2e_soc_mean_KBS_UKESM_APSIM
  # co2e_soc_site_mean_diff_UKESM_Daycent <- co2e_soc_mean_LRF_UKESM_Daycent-co2e_soc_mean_KBS_UKESM_Daycent
  # co2e_soc_site_mean_diff_UKESM_RothC <- co2e_soc_mean_LRF_UKESM_RothC-co2e_soc_mean_KBS_UKESM_RothC
  
  # treatment group means 
  #### Daycent ---------------
  co2e_soc_means_treat_groups_KBS_Baseline_Daycent_NT <- 
    co2e_soc_KBS_Baseline_Daycent[co2e_soc_KBS_Baseline_Daycent$Mgmt_Scenario %in% 54:56,] %>%
    mutate(scenario_group="NT",
           mean_val=vals) %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_soc_means_treat_groups_KBS_Baseline_Daycent_RR <- 
    co2e_soc_KBS_Baseline_Daycent[co2e_soc_KBS_Baseline_Daycent$Mgmt_Scenario %in% 51:53,] %>%
    mutate(scenario_group="RR") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_soc_means_treat_groups_KBS_Baseline_Daycent_Oth <- 
    co2e_soc_KBS_Baseline_Daycent[!co2e_soc_KBS_Baseline_Daycent$Mgmt_Scenario %in% 51:56,] %>%
    mutate(scenario_group="Oth") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  #
  co2e_soc_means_treat_groups_KBS_UKESM_Daycent_NT <- 
    co2e_soc_KBS_UKESM_Daycent[co2e_soc_KBS_UKESM_Daycent$Mgmt_Scenario %in% 54:56,] %>%
    mutate(scenario_group="NT",
           mean_val=vals) %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_soc_means_treat_groups_KBS_UKESM_Daycent_RR <- 
    co2e_soc_KBS_UKESM_Daycent[co2e_soc_KBS_UKESM_Daycent$Mgmt_Scenario %in% 51:53,] %>%
    mutate(scenario_group="RR") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_soc_means_treat_groups_KBS_UKESM_Daycent_Oth <- 
    co2e_soc_KBS_UKESM_Daycent[!co2e_soc_KBS_UKESM_Daycent$Mgmt_Scenario %in% 51:56,] %>%
    mutate(scenario_group="Oth") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  #
  co2e_soc_means_treat_groups_LRF_Baseline_Daycent_NT <- 
    co2e_soc_LRF_Baseline_Daycent[co2e_soc_LRF_Baseline_Daycent$Mgmt_Scenario %in% c(8,54:56),] %>%
    mutate(scenario_group="NT") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_soc_means_treat_groups_LRF_Baseline_Daycent_RR <- 
    co2e_soc_LRF_Baseline_Daycent[co2e_soc_LRF_Baseline_Daycent$Mgmt_Scenario %in% 51:53,] %>%
    mutate(scenario_group="RR") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_soc_means_treat_groups_LRF_Baseline_Daycent_Oth <- 
    co2e_soc_LRF_Baseline_Daycent[!co2e_soc_LRF_Baseline_Daycent$Mgmt_Scenario %in% c(8,51:56),] %>%
    mutate(scenario_group="Oth") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  #
  co2e_soc_means_treat_groups_LRF_UKESM_Daycent_NT <- 
    co2e_soc_LRF_UKESM_Daycent[co2e_soc_LRF_UKESM_Daycent$Mgmt_Scenario %in% c(8,54:56),] %>%
    mutate(scenario_group="NT") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_soc_means_treat_groups_LRF_UKESM_Daycent_RR <- 
    co2e_soc_LRF_UKESM_Daycent[co2e_soc_LRF_UKESM_Daycent$Mgmt_Scenario %in% 51:53,] %>%
    mutate(scenario_group="RR") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_soc_means_treat_groups_LRF_UKESM_Daycent_Oth <- 
    co2e_soc_LRF_UKESM_Daycent[!co2e_soc_LRF_UKESM_Daycent$Mgmt_Scenario %in% c(8,51:56),] %>%
    mutate(scenario_group="Oth") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))

  co2e_soc_means_treat_groups_Daycent_df <- rbind(co2e_soc_means_treat_groups_KBS_Baseline_Daycent_NT,
              co2e_soc_means_treat_groups_KBS_Baseline_Daycent_RR,
              co2e_soc_means_treat_groups_KBS_Baseline_Daycent_Oth,
              co2e_soc_means_treat_groups_KBS_UKESM_Daycent_NT,
              co2e_soc_means_treat_groups_KBS_UKESM_Daycent_RR,
              co2e_soc_means_treat_groups_KBS_UKESM_Daycent_Oth,
              co2e_soc_means_treat_groups_LRF_Baseline_Daycent_NT,
              co2e_soc_means_treat_groups_LRF_Baseline_Daycent_RR,
              co2e_soc_means_treat_groups_LRF_Baseline_Daycent_Oth ,
              co2e_soc_means_treat_groups_LRF_UKESM_Daycent_NT,
              co2e_soc_means_treat_groups_LRF_UKESM_Daycent_RR ,
              co2e_soc_means_treat_groups_LRF_UKESM_Daycent_Oth) %>%
    cbind(c("KBS","KBS","KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF","LRF","LRF"),
          c("Baseline","Baseline","Baseline","UKESM","UKESM","UKESM",
            "Baseline","Baseline","Baseline","UKESM","UKESM","UKESM"))
  names(co2e_soc_means_treat_groups_Daycent_df) <- c("scenario_group","mean_val","site","climate_scen")
  
  write.csv(co2e_soc_means_treat_groups_Daycent_df,
            file=paste0(these_results_folder,
                        "co2e_soc_means_treat_groups_Daycent.csv"),
            row.names=TRUE )
  
  # Daycent mean SOCCO2e over all
  co2e_soc_mean_Daycent <- mean(c(co2e_soc_KBS_Baseline_Daycent$vals,co2e_soc_KBS_UKESM_Daycent$vals,
                                co2e_soc_LRF_Baseline_Daycent$vals,co2e_soc_LRF_UKESM_Daycent$vals))
  
  # # % diff between models
  # co2e_soc_model_pct_diff_KBS_Baseline_Daycent_RothC <- round((co2e_soc_treat_range_KBS_Baseline_Daycent/co2e_soc_treat_range_KBS_Baseline_RothC)*100)
  # co2e_soc_model_pct_diff_KBS_UKESM_Daycent_RothC <- round((co2e_soc_treat_range_KBS_UKESM_Daycent/co2e_soc_treat_range_KBS_UKESM_RothC)*100)
  # co2e_soc_model_pct_diff_LRF_Baseline_Daycent_RothC <- round((co2e_soc_treat_range_LRF_Baseline_Daycent/co2e_soc_treat_range_LRF_Baseline_RothC)*100)
  # co2e_soc_model_pct_diff_LRF_UKESM_Daycent_RothC <- round((co2e_soc_treat_range_LRF_UKESM_Daycent/co2e_soc_treat_range_LRF_UKESM_RothC)*100)
  
  # diffs between climate scenarios for each model/site
  co2e_soc_clim_diff_KBS_APSIM_BC <- round(as.numeric(co2e_soc_KBS_UKESM_APSIM_BC$vals-co2e_soc_KBS_Baseline_APSIM_BC$vals),1)
  co2e_soc_clim_diff_KBS_APSIM <- as.numeric(co2e_soc_KBS_UKESM_APSIM$vals-co2e_soc_KBS_Baseline_APSIM$vals)
  co2e_soc_clim_diff_KBS_Daycent <- as.numeric(co2e_soc_KBS_UKESM_Daycent$vals-co2e_soc_KBS_Baseline_Daycent$vals)
  co2e_soc_clim_diff_KBS_RothC <- as.numeric(co2e_soc_KBS_UKESM_RothC$vals-co2e_soc_KBS_Baseline_RothC$vals)
  co2e_soc_clim_diff_LRF_APSIM_BC <- round(as.numeric(co2e_soc_LRF_UKESM_APSIM_BC$vals-co2e_soc_LRF_Baseline_APSIM_BC$vals),1)
  co2e_soc_clim_diff_LRF_APSIM <- as.numeric(co2e_soc_LRF_UKESM_APSIM$vals-co2e_soc_LRF_Baseline_APSIM$vals)
  co2e_soc_clim_diff_LRF_Daycent <- as.numeric(co2e_soc_LRF_UKESM_Daycent$vals-co2e_soc_LRF_Baseline_Daycent$vals)
  co2e_soc_clim_diff_LRF_RothC <- as.numeric(co2e_soc_LRF_UKESM_RothC$vals-co2e_soc_LRF_Baseline_RothC$vals)
  
  co2e_soc_clim_diff_KBS_df <- data.frame(rbind(co2e_soc_clim_diff_KBS_APSIM,
                                                co2e_soc_clim_diff_KBS_Daycent,
                                                co2e_soc_clim_diff_KBS_RothC))
  names(co2e_soc_clim_diff_KBS_df)<-c("CC_CR",
                                      "RF05_CR",
                                      "RF15_CR",
                                      "RF25_CR",
                                      "RF35_CR",
                                      "RR00_CR",
                                      "RR00_NT_CR",
                                      "RR25_CR",
                                      "RR25_NT_CR",
                                      "RR50_CR",
                                      "RR50_NT_CR")
  co2e_soc_clim_diff_KBS_df <- co2e_soc_clim_diff_KBS_df %>% 
    mutate(mean_all=rowMeans(select(co2e_soc_clim_diff_KBS_df,
                                    CC_CR,
                                    RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                    RR00_CR,RR25_CR,RR50_CR,
                                    RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_RF=rowMeans(select(co2e_soc_clim_diff_KBS_df,RF05_CR,RF15_CR,RF25_CR,RF35_CR),na.rm=TRUE),
           mean_RR=rowMeans(select(co2e_soc_clim_diff_KBS_df,RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE),
           mean_all_NT=rowMeans(select(co2e_soc_clim_diff_KBS_df,
                                       RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE))
  
  co2e_soc_clim_diff_LRF_df <- data.frame(rbind(co2e_soc_clim_diff_LRF_APSIM,
                                                co2e_soc_clim_diff_LRF_Daycent,
                                                co2e_soc_clim_diff_LRF_RothC))
  names(co2e_soc_clim_diff_LRF_df)<-c("CC_CR",
                                      "CC_NT_CR",
                                      "CN",
                                      "RF05_CR",
                                      "RF15_CR",
                                      "RF25_CR",
                                      "RF35_CR",
                                      "RR00_CR",
                                      "RR00_NT_CR",
                                      "RR25_CR",
                                      "RR25_NT_CR",
                                      "RR50_CR",
                                      "RR50_NT_CR")
  co2e_soc_clim_diff_LRF_df <- co2e_soc_clim_diff_LRF_df %>% 
    mutate(mean_all=rowMeans(select(co2e_soc_clim_diff_LRF_df,
                                    CC_CR,CC_NT_CR,CN,
                                    RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                    RR00_CR,RR25_CR,RR50_CR,
                                    RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_RF=rowMeans(select(co2e_soc_clim_diff_LRF_df,RF05_CR,RF15_CR,RF25_CR,RF35_CR),na.rm=TRUE),
           mean_RR=rowMeans(select(co2e_soc_clim_diff_LRF_df,RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE),
           mean_RR_NT=rowMeans(select(co2e_soc_clim_diff_LRF_df,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_all_NT=rowMeans(select(co2e_soc_clim_diff_LRF_df,
                                       CC_NT_CR,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE))
  
  co2e_soc_clim_diff_mean_KBS_APSIM_BC <- mean(co2e_soc_clim_diff_KBS_APSIM_BC)
  co2e_soc_clim_diff_mean_LRF_APSIM_BC <- mean(co2e_soc_clim_diff_LRF_APSIM_BC)
  
  write.csv(co2e_soc_clim_diff_KBS_df,
            file=paste0(these_results_folder,
                        "co2e_soc_clim_diff_KBS.csv"),
            row.names=TRUE)
  write.csv(co2e_soc_clim_diff_LRF_df,
            file=paste0(these_results_folder,
                        "co2e_soc_clim_diff_LRF.csv"),
            row.names=TRUE)
  
  co2e_soc_clim_diff_KBS_means <- round(colMeans(co2e_soc_clim_diff_KBS_df),1)
  
  write.csv(co2e_soc_clim_diff_KBS_means,
            file=paste0(these_results_folder,
                        "co2e_soc_clim_diff_KBS_means.csv"))
  
  co2e_soc_clim_diff_LRF_means <- round(colMeans(co2e_soc_clim_diff_LRF_df),1)

  write.csv(co2e_soc_clim_diff_LRF_means,
            file=paste0(these_results_folder,
                        "co2e_soc_clim_diff_LRF_means.csv"))
  
  write.csv(co2e_soc_clim_diff_KBS_APSIM_BC,
            file=paste0(these_results_folder,
                        "co2e_soc_clim_diff_KBS_APSIM_BC.csv"))
  
  write.csv(co2e_soc_clim_diff_LRF_APSIM_BC,
            file=paste0(these_results_folder,
                        "co2e_soc_clim_diff_LRF_APSIM_BC.csv"))
  
  co2e_soc_clim_diff_KBS_APSIM_BC_mean <- mean(co2e_soc_clim_diff_KBS_APSIM_BC)
  co2e_soc_clim_diff_LRF_APSIM_BC_mean <- mean(co2e_soc_clim_diff_LRF_APSIM_BC)

    ### Model means ---------------------
  
  co2e_socmeans <- gwp_scenario_means_piv[gwp_scenario_means_piv$source == "mean_CO2e_SOC",]
  co2e_socmeans_KBS_Baseline <- co2e_socmeans[co2e_socmeans$site_name=="KBS" &
                                           co2e_socmeans$climate_desc=="Baseline",
                                         c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_socmeans_KBS_UKESM <- co2e_socmeans[co2e_socmeans$site_name=="KBS" &
                                                co2e_socmeans$climate_desc=="UKESM",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_socmeans_LRF_Baseline <- co2e_socmeans[co2e_socmeans$site_name=="LRF" &
                                                co2e_socmeans$climate_desc=="Baseline",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_socmeans_LRF_UKESM <- co2e_socmeans[co2e_socmeans$site_name=="LRF" &
                                             co2e_socmeans$climate_desc=="UKESM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  
  
  co2e_socmeans_clim_diff_KBS <- as.numeric(co2e_socmeans_KBS_UKESM$vals-co2e_socmeans_KBS_Baseline$vals)
  co2e_socmeans_clim_diff_LRF <- as.numeric(co2e_socmeans_LRF_UKESM$vals-co2e_socmeans_LRF_Baseline$vals)
  
  co2e_socmeans_mean_clim_diff_KBS <- mean(co2e_socmeans_clim_diff_KBS)
  co2e_socmeans_mean_clim_diff_LRF <- mean(co2e_socmeans_clim_diff_LRF)
  

  ## components  N2O ------------------------------------------------------------
  
  g_n2oe <- summary_output_piv[summary_output_piv$source == "CO2e_N2O" &
                                 summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_N2O), color= "black", 
             fill=NA, position="dodge") + 
    #    ylab(expression('CO'[2]*'e N'[2]*'O (Mg ha ' ^-1*')')) +
    ylab(expression('N'[2]*'O'[CO2e]*' (Mg ha ' ^-1*')')) +
    xlab("") +
    #    ggtitle(paste0("Change in CO2e-N2O by ",end_fut_period_year)) +
    ggtitle(bquote("Change in N"["2"]*"O"[CO2e]*" by"~.(end_fut_period_year))) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_n2oe
  
  ### analysis ----------------------
  
  co2e_n2o <- summary_output_piv[summary_output_piv$source == "CO2e_N2O",]
  co2e_n2o_KBS_Baseline_APSIM_BC <- co2e_n2o[co2e_n2o$site_name=="KBS" &
                                               co2e_n2o$climate_desc=="Baseline" &
                                               co2e_n2o$Model=="APSIM",
                                             c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_n2o_KBS_Baseline_APSIM <- co2e_n2o[co2e_n2o$site_name=="KBS" &
                                            co2e_n2o$climate_desc=="Baseline" &
                                            co2e_n2o$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_n2o_KBS_Baseline_APSIM_all <- co2e_n2o[co2e_n2o$site_name=="KBS" &
                                                co2e_n2o$climate_desc=="Baseline" &
                                                co2e_n2o$Model=="APSIM",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2o_KBS_Baseline_Daycent <- co2e_n2o[co2e_n2o$site_name=="KBS" &
                                              co2e_n2o$climate_desc=="Baseline" &
                                              co2e_n2o$Model=="Daycent",
                                            c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2o_KBS_UKESM_APSIM_BC <- co2e_n2o[co2e_n2o$site_name=="KBS" &
                                            co2e_n2o$climate_desc=="UKESM" &
                                            co2e_n2o$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_n2o_KBS_UKESM_APSIM <- co2e_n2o[co2e_n2o$site_name=="KBS" &
                                         co2e_n2o$climate_desc=="UKESM" &
                                         co2e_n2o$Model=="APSIM",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_n2o_KBS_UKESM_APSIM_all <- co2e_n2o[co2e_n2o$site_name=="KBS" &
                                             co2e_n2o$climate_desc=="UKESM" &
                                             co2e_n2o$Model=="APSIM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2o_KBS_UKESM_Daycent <- co2e_n2o[co2e_n2o$site_name=="KBS" &
                                           co2e_n2o$climate_desc=="UKESM" &
                                           co2e_n2o$Model=="Daycent",
                                         c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2o_LRF_Baseline_APSIM_BC <- co2e_n2o[co2e_n2o$site_name=="LRF" &
                                               co2e_n2o$climate_desc=="Baseline" &
                                               co2e_n2o$Model=="APSIM",
                                             c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_n2o_LRF_Baseline_APSIM <- co2e_n2o[co2e_n2o$site_name=="LRF" &
                                            co2e_n2o$climate_desc=="Baseline" &
                                            co2e_n2o$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_n2o_LRF_Baseline_APSIM_all <- co2e_n2o[co2e_n2o$site_name=="LRF" &
                                                co2e_n2o$climate_desc=="Baseline" &
                                                co2e_n2o$Model=="APSIM",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2o_LRF_Baseline_Daycent <- co2e_n2o[co2e_n2o$site_name=="LRF" &
                                              co2e_n2o$climate_desc=="Baseline" &
                                              co2e_n2o$Model=="Daycent",
                                            c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2o_LRF_UKESM_APSIM_BC <- co2e_n2o[co2e_n2o$site_name=="LRF" &
                                            co2e_n2o$climate_desc=="UKESM" &
                                            co2e_n2o$Model=="APSIM",
                                          c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(grepl("BC",scenario_abbrev))
  co2e_n2o_LRF_UKESM_APSIM <- co2e_n2o[co2e_n2o$site_name=="LRF" &
                                         co2e_n2o$climate_desc=="UKESM" &
                                         co2e_n2o$Model=="APSIM",
                                       c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev) %>%
    filter(!grepl("BC",scenario_abbrev))
  co2e_n2o_LRF_UKESM_APSIM_all <- co2e_n2o[co2e_n2o$site_name=="LRF" &
                                             co2e_n2o$climate_desc=="UKESM" &
                                             co2e_n2o$Model=="APSIM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2o_LRF_UKESM_Daycent <- co2e_n2o[co2e_n2o$site_name=="LRF" &
                                           co2e_n2o$climate_desc=="UKESM" &
                                           co2e_n2o$Model=="Daycent",
                                         c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)

  # diffs between models
  co2e_n2o_model_diff_KBS_Baseline <- co2e_n2o_KBS_Baseline_APSIM$vals - co2e_n2o_KBS_Baseline_Daycent$vals
  co2e_n2o_model_diff_KBS_UKESM <- co2e_n2o_KBS_UKESM_APSIM$vals - co2e_n2o_KBS_UKESM_Daycent$vals
  co2e_n2o_model_diff_LRF_Baseline <- co2e_n2o_LRF_Baseline_APSIM$vals - co2e_n2o_LRF_Baseline_Daycent$vals
  co2e_n2o_model_diff_LRF_UKESM <- co2e_n2o_LRF_UKESM_APSIM$vals - co2e_n2o_LRF_UKESM_Daycent$vals

  co2e_n2o_mean_model_diff_KBS_Baseline <- mean(co2e_n2o_model_diff_KBS_Baseline)
  co2e_n2o_mean_model_diff_KBS_UKESM <- mean(co2e_n2o_model_diff_KBS_UKESM)
  co2e_n2o_mean_model_diff_LRF_Baseline <- mean(co2e_n2o_model_diff_LRF_Baseline)
  co2e_n2o_mean_model_diff_LRF_UKESM <- mean(co2e_n2o_model_diff_LRF_UKESM)
  
  # diffs between treatments
  co2e_n2o_treat_range_KBS_Baseline_APSIM <- round(max(co2e_n2o_KBS_Baseline_APSIM$vals)-min(co2e_n2o_KBS_Baseline_APSIM$vals),1)
  co2e_n2o_treat_range_KBS_Baseline_Daycent <- round(max(co2e_n2o_KBS_Baseline_Daycent$vals)-min(co2e_n2o_KBS_Baseline_Daycent$vals),1)
  co2e_n2o_treat_range_KBS_UKESM_APSIM <- round(max(co2e_n2o_KBS_UKESM_APSIM$vals)-min(co2e_n2o_KBS_UKESM_APSIM$vals),1)
  co2e_n2o_treat_range_KBS_UKESM_Daycent <- round(max(co2e_n2o_KBS_UKESM_Daycent$vals)-min(co2e_n2o_KBS_UKESM_Daycent$vals),1)
  co2e_n2o_treat_range_LRF_Baseline_APSIM <- round(max(co2e_n2o_LRF_Baseline_APSIM$vals)-min(co2e_n2o_LRF_Baseline_APSIM$vals),1)
  co2e_n2o_treat_range_LRF_Baseline_Daycent <- round(max(co2e_n2o_LRF_Baseline_Daycent$vals)-min(co2e_n2o_LRF_Baseline_Daycent$vals),1)
  co2e_n2o_treat_range_LRF_UKESM_APSIM <-round( max(co2e_n2o_LRF_UKESM_APSIM$vals)-min(co2e_n2o_LRF_UKESM_APSIM$vals),1)
  co2e_n2o_treat_range_LRF_UKESM_Daycent <- round(max(co2e_n2o_LRF_UKESM_Daycent$vals)-min(co2e_n2o_LRF_UKESM_Daycent$vals),1)

  write.csv(c(co2e_n2o_treat_range_KBS_Baseline_APSIM,
              co2e_n2o_treat_range_KBS_Baseline_Daycent,
              co2e_n2o_treat_range_KBS_UKESM_APSIM,
              co2e_n2o_treat_range_KBS_UKESM_Daycent,
              co2e_n2o_treat_range_LRF_Baseline_APSIM,
              co2e_n2o_treat_range_LRF_Baseline_Daycent,
              co2e_n2o_treat_range_LRF_UKESM_APSIM,
              co2e_n2o_treat_range_LRF_UKESM_Daycent),
            file=paste0(these_results_folder,
                        "co2e_n2o_treat_ranges.csv"),
            row.names=TRUE)
  
  co2e_n2o_treat_range_BC_KBS_Baseline_APSIM <- round(max(co2e_n2o_KBS_Baseline_APSIM_BC$vals)-min(co2e_n2o_KBS_Baseline_APSIM_BC$vals),1)
  co2e_n2o_treat_range_BC_KBS_UKESM_APSIM <- round(max(co2e_n2o_KBS_UKESM_APSIM_BC$vals)-min(co2e_n2o_KBS_UKESM_APSIM_BC$vals),1)
  co2e_n2o_treat_range_BC_LRF_Baseline_APSIM <- round(max(co2e_n2o_LRF_Baseline_APSIM_BC$vals)-min(co2e_n2o_LRF_Baseline_APSIM_BC$vals),1)
  co2e_n2o_treat_range_BC_LRF_UKESM_APSIM <- round(max(co2e_n2o_LRF_UKESM_APSIM_BC$vals)-min(co2e_n2o_LRF_UKESM_APSIM_BC$vals),1)
  
  co2e_n2o_mean_KBS_Baseline_APSIM <- round(mean(co2e_n2o_KBS_Baseline_APSIM$vals),1)
  co2e_n2o_mean_KBS_Baseline_Daycent <-  round(mean(co2e_n2o_KBS_Baseline_Daycent$vals),1)
  co2e_n2o_mean_KBS_UKESM_APSIM <-  round(mean(co2e_n2o_KBS_UKESM_APSIM$vals),1)
  co2e_n2o_mean_KBS_UKESM_Daycent <-  round(mean(co2e_n2o_KBS_UKESM_Daycent$vals),1)
  co2e_n2o_mean_LRF_Baseline_APSIM <-  round(mean(co2e_n2o_LRF_Baseline_APSIM$vals),1)
  co2e_n2o_mean_LRF_Baseline_Daycent <-  round(mean(co2e_n2o_LRF_Baseline_Daycent$vals),1)
  co2e_n2o_mean_LRF_UKESM_APSIM <-  round(mean(co2e_n2o_LRF_UKESM_APSIM$vals),1)
  co2e_n2o_mean_LRF_UKESM_Daycent <-  round(mean(co2e_n2o_LRF_UKESM_Daycent$vals),1)

  co2e_n2o_mean_KBS_Baseline_APSIM_all <- round(mean(co2e_n2o_KBS_Baseline_APSIM_all$vals),1)
  co2e_n2o_mean_KBS_UKESM_APSIM_all <-  round(mean(co2e_n2o_KBS_UKESM_APSIM_all$vals),1)
  co2e_n2o_mean_LRF_Baseline_APSIM_all <-  round(mean(co2e_n2o_LRF_Baseline_APSIM_all$vals),1)
  co2e_n2o_mean_LRF_UKESM_APSIM_all <-  round(mean(co2e_n2o_LRF_UKESM_APSIM_all$vals),1)
  
  co2e_n2o_mean <- data.frame(site=c("KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF",
                                     "KBS","KBS","LRF","LRF"),
                              climate_scen=c("Baseline","Baseline","UKESM","UKESM",
                                             "Baseline","Baseline","UKESM","UKESM",
                                             "Baseline","UKESM","Baseline","UKESM"),
                              model=c("APSIM","Daycent","APSIM","Daycent",
                                      "APSIM","Daycent","APSIM","Daycent",
                                      "APSIM all","APSIM all","APSIM all","APSIM all"),
                              mean_val=c(co2e_n2o_mean_KBS_Baseline_APSIM,co2e_n2o_mean_KBS_Baseline_Daycent,
                                         co2e_n2o_mean_KBS_UKESM_APSIM,
                                         co2e_n2o_mean_KBS_UKESM_Daycent,
                                         co2e_n2o_mean_LRF_Baseline_APSIM,co2e_n2o_mean_LRF_Baseline_Daycent,
                                         co2e_n2o_mean_LRF_UKESM_APSIM,
                                         co2e_n2o_mean_LRF_UKESM_Daycent,
                                         co2e_n2o_mean_KBS_Baseline_APSIM_all,co2e_n2o_mean_KBS_UKESM_APSIM_all,
                                         co2e_n2o_mean_LRF_Baseline_APSIM_all,co2e_n2o_mean_LRF_UKESM_APSIM_all))
  
  write.csv(co2e_n2o_mean,
            file=paste0(these_results_folder,
                        "co2e_n2o_means.csv"),
            row.names=FALSE)
  
  # # may be comparing apples to oranges because different crops, residue retention, other treatments
  # co2e_n2o_site_mean_diff_Baseline_APSIM <- co2e_n2o_mean_LRF_Baseline_APSIM-co2e_n2o_mean_KBS_Baseline_APSIM
  # co2e_n2o_site_mean_diff_Baseline_Daycent <- co2e_n2o_mean_LRF_Baseline_Daycent-co2e_n2o_mean_KBS_Baseline_Daycent
  # co2e_n2o_site_mean_diff_Baseline_RothC <- co2e_n2o_mean_LRF_Baseline_RothC-co2e_n2o_mean_KBS_Baseline_RothC
  # co2e_n2o_site_mean_diff_UKESM_APSIM <- co2e_n2o_mean_LRF_UKESM_APSIM-co2e_n2o_mean_KBS_UKESM_APSIM
  # co2e_n2o_site_mean_diff_UKESM_Daycent <- co2e_n2o_mean_LRF_UKESM_Daycent-co2e_n2o_mean_KBS_UKESM_Daycent
  # co2e_n2o_site_mean_diff_UKESM_RothC <- co2e_n2o_mean_LRF_UKESM_RothC-co2e_n2o_mean_KBS_UKESM_RothC
  
  # treatment group means
  #### Daycent ---------------
  co2e_n2o_means_treat_groups_KBS_Baseline_Daycent_NT <- 
    co2e_n2o_KBS_Baseline_Daycent[co2e_n2o_KBS_Baseline_Daycent$Mgmt_Scenario %in% 54:56,] %>%
    mutate(scenario_group="NT",
           mean_val=vals) %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_n2o_means_treat_groups_KBS_Baseline_Daycent_RR <- 
    co2e_n2o_KBS_Baseline_Daycent[co2e_n2o_KBS_Baseline_Daycent$Mgmt_Scenario %in% 51:53,] %>%
    mutate(scenario_group="RR") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_n2o_means_treat_groups_KBS_Baseline_Daycent_Oth <- 
    co2e_n2o_KBS_Baseline_Daycent[!co2e_n2o_KBS_Baseline_Daycent$Mgmt_Scenario %in% 51:56,] %>%
    mutate(scenario_group="Oth") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  #
  co2e_n2o_means_treat_groups_KBS_UKESM_Daycent_NT <- 
    co2e_n2o_KBS_UKESM_Daycent[co2e_n2o_KBS_UKESM_Daycent$Mgmt_Scenario %in% 54:56,] %>%
    mutate(scenario_group="NT",
           mean_val=vals) %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_n2o_means_treat_groups_KBS_UKESM_Daycent_RR <- 
    co2e_n2o_KBS_UKESM_Daycent[co2e_n2o_KBS_UKESM_Daycent$Mgmt_Scenario %in% 51:53,] %>%
    mutate(scenario_group="RR") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_n2o_means_treat_groups_KBS_UKESM_Daycent_Oth <- 
    co2e_n2o_KBS_UKESM_Daycent[!co2e_n2o_KBS_UKESM_Daycent$Mgmt_Scenario %in% 51:56,] %>%
    mutate(scenario_group="Oth") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  #
  co2e_n2o_means_treat_groups_LRF_Baseline_Daycent_NT <- 
    co2e_n2o_LRF_Baseline_Daycent[co2e_n2o_LRF_Baseline_Daycent$Mgmt_Scenario %in% c(8,54:56),] %>%
    mutate(scenario_group="NT") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_n2o_means_treat_groups_LRF_Baseline_Daycent_RR <- 
    co2e_n2o_LRF_Baseline_Daycent[co2e_n2o_LRF_Baseline_Daycent$Mgmt_Scenario %in% 51:53,] %>%
    mutate(scenario_group="RR") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_n2o_means_treat_groups_LRF_Baseline_Daycent_Oth <- 
    co2e_n2o_LRF_Baseline_Daycent[!co2e_n2o_LRF_Baseline_Daycent$Mgmt_Scenario %in% c(8,51:56),] %>%
    mutate(scenario_group="Oth") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  #
  co2e_n2o_means_treat_groups_LRF_UKESM_Daycent_NT <- 
    co2e_n2o_LRF_UKESM_Daycent[co2e_n2o_LRF_UKESM_Daycent$Mgmt_Scenario %in% c(8,54:56),] %>%
    mutate(scenario_group="NT") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_n2o_means_treat_groups_LRF_UKESM_Daycent_RR <- 
    co2e_n2o_LRF_UKESM_Daycent[co2e_n2o_LRF_UKESM_Daycent$Mgmt_Scenario %in% 51:53,] %>%
    mutate(scenario_group="RR") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  co2e_n2o_means_treat_groups_LRF_UKESM_Daycent_Oth <- 
    co2e_n2o_LRF_UKESM_Daycent[!co2e_n2o_LRF_UKESM_Daycent$Mgmt_Scenario %in% c(8,51:56),] %>%
    mutate(scenario_group="Oth") %>%
    group_by(scenario_group) %>%
    summarize(mean_val=mean(vals))
  
  co2e_n2o_means_treat_groups_Daycent_df <- rbind(co2e_n2o_means_treat_groups_KBS_Baseline_Daycent_NT,
                                                  co2e_n2o_means_treat_groups_KBS_Baseline_Daycent_RR,
                                                  co2e_n2o_means_treat_groups_KBS_Baseline_Daycent_Oth,
                                                  co2e_n2o_means_treat_groups_KBS_UKESM_Daycent_NT,
                                                  co2e_n2o_means_treat_groups_KBS_UKESM_Daycent_RR,
                                                  co2e_n2o_means_treat_groups_KBS_UKESM_Daycent_Oth,
                                                  co2e_n2o_means_treat_groups_LRF_Baseline_Daycent_NT,
                                                  co2e_n2o_means_treat_groups_LRF_Baseline_Daycent_RR,
                                                  co2e_n2o_means_treat_groups_LRF_Baseline_Daycent_Oth ,
                                                  co2e_n2o_means_treat_groups_LRF_UKESM_Daycent_NT,
                                                  co2e_n2o_means_treat_groups_LRF_UKESM_Daycent_RR ,
                                                  co2e_n2o_means_treat_groups_LRF_UKESM_Daycent_Oth) %>%
    cbind(c("KBS","KBS","KBS","KBS","KBS","KBS","LRF","LRF","LRF","LRF","LRF","LRF"),
          c("Baseline","Baseline","Baseline","UKESM","UKESM","UKESM",
            "Baseline","Baseline","Baseline","UKESM","UKESM","UKESM"))
  names(co2e_n2o_means_treat_groups_Daycent_df) <- c("scenario_group","mean_val","site","climate_scen")
  
  write.csv(co2e_n2o_means_treat_groups_Daycent_df,
            file=paste0(these_results_folder,
                        "co2e_n2o_means_treat_groups_Daycent.csv"),
            row.names=TRUE )
  
  # # % diff between models
  # co2e_n2o_model_pct_diff_KBS_Baseline_Daycent_RothC <- round((co2e_n2o_treat_range_KBS_Baseline_Daycent/co2e_n2o_treat_range_KBS_Baseline_RothC)*100)
  # co2e_n2o_model_pct_diff_KBS_UKESM_Daycent_RothC <- round((co2e_n2o_treat_range_KBS_UKESM_Daycent/co2e_n2o_treat_range_KBS_UKESM_RothC)*100)
  # co2e_n2o_model_pct_diff_LRF_Baseline_Daycent_RothC <- round((co2e_n2o_treat_range_LRF_Baseline_Daycent/co2e_n2o_treat_range_LRF_Baseline_RothC)*100)
  # co2e_n2o_model_pct_diff_LRF_UKESM_Daycent_RothC <- round((co2e_n2o_treat_range_LRF_UKESM_Daycent/co2e_n2o_treat_range_LRF_UKESM_RothC)*100)
  
  # diffs between climate scenarios for each model/site
  co2e_n2o_clim_diff_KBS_APSIM_BC <- round(as.numeric(co2e_n2o_KBS_UKESM_APSIM_BC$vals-co2e_n2o_KBS_Baseline_APSIM_BC$vals),1)
  co2e_n2o_clim_diff_KBS_APSIM <- as.numeric(co2e_n2o_KBS_UKESM_APSIM$vals-co2e_n2o_KBS_Baseline_APSIM$vals)
  co2e_n2o_clim_diff_KBS_Daycent <- as.numeric(co2e_n2o_KBS_UKESM_Daycent$vals-co2e_n2o_KBS_Baseline_Daycent$vals)
  co2e_n2o_clim_diff_LRF_APSIM_BC <- round(as.numeric(co2e_n2o_LRF_UKESM_APSIM_BC$vals-co2e_n2o_LRF_Baseline_APSIM_BC$vals),1)
  co2e_n2o_clim_diff_LRF_APSIM <- as.numeric(co2e_n2o_LRF_UKESM_APSIM$vals-co2e_n2o_LRF_Baseline_APSIM$vals)
  co2e_n2o_clim_diff_LRF_Daycent <- as.numeric(co2e_n2o_LRF_UKESM_Daycent$vals-co2e_n2o_LRF_Baseline_Daycent$vals)

  co2e_n2o_clim_diff_KBS_df <- data.frame(rbind(co2e_n2o_clim_diff_KBS_APSIM,
                                                co2e_n2o_clim_diff_KBS_Daycent))
  names(co2e_n2o_clim_diff_KBS_df)<-c("CC_CR",
                                      "RF05_CR",
                                      "RF15_CR",
                                      "RF25_CR",
                                      "RF35_CR",
                                      "RR00_CR",
                                      "RR00_NT_CR",
                                      "RR25_CR",
                                      "RR25_NT_CR",
                                      "RR50_CR",
                                      "RR50_NT_CR")
  
  co2e_n2o_clim_diff_KBS_df <- co2e_n2o_clim_diff_KBS_df %>% 
    mutate(mean_all=rowMeans(select(co2e_n2o_clim_diff_KBS_df,
                                    CC_CR,
                                    RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                    RR00_CR,RR25_CR,RR50_CR,
                                    RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_RF=rowMeans(select(co2e_n2o_clim_diff_KBS_df,RF05_CR,RF15_CR,RF25_CR,RF35_CR),na.rm=TRUE),
           mean_RR=rowMeans(select(co2e_n2o_clim_diff_KBS_df,RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE),
           mean_RR_NT=rowMeans(select(co2e_n2o_clim_diff_KBS_df,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_all_NT=rowMeans(select(co2e_n2o_clim_diff_KBS_df,
                                       RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE))
  
  co2e_n2o_clim_diff_LRF_df <- data.frame(rbind(co2e_n2o_clim_diff_LRF_APSIM,
                                                co2e_n2o_clim_diff_LRF_Daycent))
  names(co2e_n2o_clim_diff_LRF_df)<-c("CC_CR",
                                      "CC_NT_CR",
                                      "CN",
                                      "RF05_CR",
                                      "RF15_CR",
                                      "RF25_CR",
                                      "RF35_CR",
                                      "RR00_CR",
                                      "RR00_NT_CR",
                                      "RR25_CR",
                                      "RR25_NT_CR",
                                      "RR50_CR",
                                      "RR50_NT_CR")
  
  co2e_n2o_clim_diff_LRF_df <- co2e_n2o_clim_diff_LRF_df %>% 
    mutate(mean_all=rowMeans(select(co2e_n2o_clim_diff_LRF_df,
                                    CC_CR,CC_NT_CR,CN,
                                    RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                    RR00_CR,RR25_CR,RR50_CR,
                                    RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_RF=rowMeans(select(co2e_n2o_clim_diff_LRF_df,RF05_CR,RF15_CR,RF25_CR,RF35_CR),na.rm=TRUE),
           mean_RR=rowMeans(select(co2e_n2o_clim_diff_LRF_df,RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE),
           mean_RR_NT=rowMeans(select(co2e_n2o_clim_diff_LRF_df,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_all_NT=rowMeans(select(co2e_n2o_clim_diff_LRF_df,
                                       CC_NT_CR,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE))
  
  co2e_n2o_clim_diff_mean_KBS_APSIM_BC <- mean(co2e_n2o_clim_diff_KBS_APSIM_BC)
  co2e_n2o_clim_diff_mean_LRF_APSIM_BC <- mean(co2e_n2o_clim_diff_LRF_APSIM_BC)
  
  write.csv(co2e_n2o_clim_diff_KBS_df,
            file=paste0(these_results_folder,
                        "co2e_n2o_clim_diff_KBS.csv"),
            row.names=TRUE)
  write.csv(co2e_n2o_clim_diff_LRF_df,
            file=paste0(these_results_folder,
                        "co2e_n2o_clim_diff_LRF.csv"),
            row.names=TRUE)
  
  co2e_n2o_clim_diff_KBS_means <- round(colMeans(co2e_n2o_clim_diff_KBS_df),1)
  
  write.csv(co2e_n2o_clim_diff_KBS_means,
            file=paste0(these_results_folder,
                        "co2e_n2o_clim_diff_KBS_means.csv"))
  
  co2e_n2o_clim_diff_LRF_means <- round(colMeans(co2e_n2o_clim_diff_LRF_df),1)
  
  write.csv(co2e_n2o_clim_diff_LRF_means,
            file=paste0(these_results_folder,
                        "co2e_n2o_clim_diff_LRF_means.csv"))
  
  write.csv(co2e_n2o_clim_diff_KBS_APSIM_BC,
            file=paste0(these_results_folder,
                        "co2e_n2o_clim_diff_KBS_APSIM_BC.csv"))
  
  write.csv(co2e_n2o_clim_diff_LRF_APSIM_BC,
            file=paste0(these_results_folder,
                        "co2e_n2o_clim_diff_LRF_APSIM_BC.csv"))
  
  co2e_n2o_clim_diff_KBS_APSIM_BC_mean <- round(mean(co2e_n2o_clim_diff_KBS_APSIM_BC),1)
  co2e_n2o_clim_diff_LRF_APSIM_BC_mean <- round(mean(co2e_n2o_clim_diff_LRF_APSIM_BC),1)
  
  ### Model means ---------------------
  
  co2e_n2omeans <- gwp_scenario_means_piv[gwp_scenario_means_piv$source == "mean_CO2e_N2O",]
  co2e_n2omeans_KBS_Baseline <- co2e_n2omeans[co2e_n2omeans$site_name=="KBS" &
                                                co2e_n2omeans$climate_desc=="Baseline",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2omeans_KBS_UKESM <- co2e_n2omeans[co2e_n2omeans$site_name=="KBS" &
                                             co2e_n2omeans$climate_desc=="UKESM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2omeans_LRF_Baseline <- co2e_n2omeans[co2e_n2omeans$site_name=="LRF" &
                                                co2e_n2omeans$climate_desc=="Baseline",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_n2omeans_LRF_UKESM <- co2e_n2omeans[co2e_n2omeans$site_name=="LRF" &
                                             co2e_n2omeans$climate_desc=="UKESM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  
  
  co2e_n2omeans_clim_diff_KBS <- as.numeric(co2e_n2omeans_KBS_UKESM$vals-co2e_n2omeans_KBS_Baseline$vals)
  co2e_n2omeans_clim_diff_LRF <- as.numeric(co2e_n2omeans_LRF_UKESM$vals-co2e_n2omeans_LRF_Baseline$vals)
  
  co2e_n2omeans_mean_clim_diff_KBS <- mean(co2e_n2omeans_clim_diff_KBS)
  co2e_n2omeans_mean_clim_diff_LRF <- mean(co2e_n2omeans_clim_diff_LRF)
  
  
  
  ## components  CH4 ------------------------------------------------------------
  
  g_ch4e <- summary_output_piv[summary_output_piv$source == "CO2e_CH4" &
                                 summary_output_piv$Model == "Daycent",] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_vline(xintercept=20.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=gwp_scenario_means,
             aes(x=scenario_abbrev, y=mean_CO2e_CH4), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('CH'[4][CO2e]*' (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(bquote("Change in CH"[4][CO2e]*" by"~.(end_fut_period_year))) +
    scale_fill_manual(labels=c("Daycent"),
                      values=cbPalette9[c(2)],
                      name="Model") +
    facet_grid(climate_desc~site_name) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_ch4e

  
  co2e_ch4 <- summary_output_piv[summary_output_piv$source == "CO2e_CH4",]
  co2e_ch4_KBS_Baseline_Daycent <- co2e_ch4[co2e_ch4$site_name=="KBS" &
                                              co2e_ch4$climate_desc=="Baseline" &
                                              co2e_ch4$Model=="Daycent",
                                            c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_ch4_KBS_UKESM_Daycent <- co2e_ch4[co2e_ch4$site_name=="KBS" &
                                           co2e_ch4$climate_desc=="UKESM" &
                                           co2e_ch4$Model=="Daycent",
                                         c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_ch4_LRF_Baseline_Daycent <- co2e_ch4[co2e_ch4$site_name=="LRF" &
                                              co2e_ch4$climate_desc=="Baseline" &
                                              co2e_ch4$Model=="Daycent",
                                            c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)
  co2e_ch4_LRF_UKESM_Daycent <- co2e_ch4[co2e_ch4$site_name=="LRF" &
                                           co2e_ch4$climate_desc=="UKESM" &
                                           co2e_ch4$Model=="Daycent",
                                         c("scenario_abbrev","Mgmt_Scenario","vals")] %>%
    arrange(scenario_abbrev)

  # diffs between treatments
  co2e_ch4_treat_range_KBS_Baseline_Daycent <- round(max(co2e_ch4_KBS_Baseline_Daycent$vals)-min(co2e_ch4_KBS_Baseline_Daycent$vals),2)
  co2e_ch4_treat_range_KBS_UKESM_Daycent <- round(max(co2e_ch4_KBS_UKESM_Daycent$vals)-min(co2e_ch4_KBS_UKESM_Daycent$vals),2)
  co2e_ch4_treat_range_LRF_Baseline_Daycent <- round(max(co2e_ch4_LRF_Baseline_Daycent$vals)-min(co2e_ch4_LRF_Baseline_Daycent$vals),2)
  co2e_ch4_treat_range_LRF_UKESM_Daycent <- round(max(co2e_ch4_LRF_UKESM_Daycent$vals)-min(co2e_ch4_LRF_UKESM_Daycent$vals),2)

  write.csv(c(co2e_ch4_treat_range_KBS_Baseline_Daycent,
              co2e_ch4_treat_range_KBS_UKESM_Daycent,
              co2e_ch4_treat_range_LRF_Baseline_Daycent,
              co2e_ch4_treat_range_LRF_UKESM_Daycent),
            file=paste0(these_results_folder,
                        "co2e_ch4_treat_ranges.csv"),
            row.names=TRUE)
  
  co2e_ch4_mean_KBS_Baseline_Daycent <-  round(mean(co2e_ch4_KBS_Baseline_Daycent$vals),1)
  co2e_ch4_mean_KBS_UKESM_Daycent <-  round(mean(co2e_ch4_KBS_UKESM_Daycent$vals),1)
  co2e_ch4_mean_LRF_Baseline_Daycent <-  round(mean(co2e_ch4_LRF_Baseline_Daycent$vals),1)
  co2e_ch4_mean_LRF_UKESM_Daycent <-  round(mean(co2e_ch4_LRF_UKESM_Daycent$vals),1)

  co2e_ch4_mean <- data.frame(site=c("KBS","KBS","LRF","LRF"),
                              climate_scen=c("Baseline","UKESM",
                                             "Baseline","UKESM"),
                              model=c("Daycent","Daycent","Daycent","Daycent"),
                              mean_val=c(co2e_ch4_mean_KBS_Baseline_Daycent,
                                         co2e_ch4_mean_KBS_UKESM_Daycent,
                                         co2e_ch4_mean_LRF_Baseline_Daycent,
                                         co2e_ch4_mean_LRF_UKESM_Daycent))
  
  write.csv(co2e_ch4_mean,
            file=paste0(these_results_folder,
                        "co2e_ch4_means.csv"),
            row.names=FALSE)
  
  # # may be comparing apples to oranges because different crops, residue retention, other treatments
  # co2e_ch4_site_mean_diff_Baseline_APSIM <- co2e_ch4_mean_LRF_Baseline_APSIM-co2e_ch4_mean_KBS_Baseline_APSIM
  # co2e_ch4_site_mean_diff_Baseline_Daycent <- co2e_ch4_mean_LRF_Baseline_Daycent-co2e_ch4_mean_KBS_Baseline_Daycent
  # co2e_ch4_site_mean_diff_Baseline_RothC <- co2e_ch4_mean_LRF_Baseline_RothC-co2e_ch4_mean_KBS_Baseline_RothC
  # co2e_ch4_site_mean_diff_UKESM_APSIM <- co2e_ch4_mean_LRF_UKESM_APSIM-co2e_ch4_mean_KBS_UKESM_APSIM
  # co2e_ch4_site_mean_diff_UKESM_Daycent <- co2e_ch4_mean_LRF_UKESM_Daycent-co2e_ch4_mean_KBS_UKESM_Daycent
  # co2e_ch4_site_mean_diff_UKESM_RothC <- co2e_ch4_mean_LRF_UKESM_RothC-co2e_ch4_mean_KBS_UKESM_RothC
  

  # # % diff between models
  # co2e_ch4_model_pct_diff_KBS_Baseline_Daycent_RothC <- round((co2e_ch4_treat_range_KBS_Baseline_Daycent/co2e_ch4_treat_range_KBS_Baseline_RothC)*100)
  # co2e_ch4_model_pct_diff_KBS_UKESM_Daycent_RothC <- round((co2e_ch4_treat_range_KBS_UKESM_Daycent/co2e_ch4_treat_range_KBS_UKESM_RothC)*100)
  # co2e_ch4_model_pct_diff_LRF_Baseline_Daycent_RothC <- round((co2e_ch4_treat_range_LRF_Baseline_Daycent/co2e_ch4_treat_range_LRF_Baseline_RothC)*100)
  # co2e_ch4_model_pct_diff_LRF_UKESM_Daycent_RothC <- round((co2e_ch4_treat_range_LRF_UKESM_Daycent/co2e_ch4_treat_range_LRF_UKESM_RothC)*100)
  
  # diffs between climate scenarios for each model/site
  co2e_ch4_clim_diff_KBS_Daycent <- as.numeric(co2e_ch4_KBS_UKESM_Daycent$vals-co2e_ch4_KBS_Baseline_Daycent$vals)
  co2e_ch4_clim_diff_LRF_Daycent <- as.numeric(co2e_ch4_LRF_UKESM_Daycent$vals-co2e_ch4_LRF_Baseline_Daycent$vals)

  co2e_ch4_clim_diff_KBS_df <- data.frame(rbind(co2e_ch4_clim_diff_KBS_Daycent))
  
  names(co2e_ch4_clim_diff_KBS_df)<-c("CC_CR",
                                      "RF05_CR",
                                      "RF15_CR",
                                      "RF25_CR",
                                      "RF35_CR",
                                      "RR00_CR",
                                      "RR00_NT_CR",
                                      "RR25_CR",
                                      "RR25_NT_CR",
                                      "RR50_CR",
                                      "RR50_NT_CR")
  
  co2e_ch4_clim_diff_KBS_df <- co2e_ch4_clim_diff_KBS_df %>% 
    mutate(mean_all=rowMeans(select(co2e_ch4_clim_diff_KBS_df,
                                    CC_CR,
                                    RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                    RR00_CR,RR25_CR,RR50_CR,
                                    RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_RF=rowMeans(select(co2e_ch4_clim_diff_KBS_df,RF05_CR,RF15_CR,RF25_CR,RF35_CR),na.rm=TRUE),
           mean_RR=rowMeans(select(co2e_ch4_clim_diff_KBS_df,RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE),
           mean_RR_NT=rowMeans(select(co2e_ch4_clim_diff_KBS_df,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE))
  
  co2e_ch4_clim_diff_LRF_df <- data.frame(rbind(co2e_ch4_clim_diff_LRF_Daycent))
  
  names(co2e_ch4_clim_diff_LRF_df)<-c("CC_CR",
                                      "CC_NT_CR",
                                      "CN",
                                      "RF05_CR",
                                      "RF15_CR",
                                      "RF25_CR",
                                      "RF35_CR",
                                      "RR00_CR",
                                      "RR00_NT_CR",
                                      "RR25_CR",
                                      "RR25_NT_CR",
                                      "RR50_CR",
                                      "RR50_NT_CR")
  
  co2e_ch4_clim_diff_LRF_df <- co2e_ch4_clim_diff_LRF_df %>% 
    mutate(mean_all=rowMeans(select(co2e_ch4_clim_diff_LRF_df,
                                    CC_CR,CC_NT_CR,CN,
                                    RF05_CR,RF15_CR,RF25_CR,RF35_CR,
                                    RR00_CR,RR25_CR,RR50_CR,
                                    RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE),
           mean_RF=rowMeans(select(co2e_ch4_clim_diff_LRF_df,RF05_CR,RF15_CR,RF25_CR,RF35_CR),na.rm=TRUE),
           mean_RR=rowMeans(select(co2e_ch4_clim_diff_LRF_df,RR00_CR,RR25_CR,RR50_CR),na.rm=TRUE),
           mean_RR_NT=rowMeans(select(co2e_ch4_clim_diff_LRF_df,RR00_NT_CR,RR25_NT_CR,RR50_NT_CR),na.rm=TRUE))
  
  write.csv(co2e_ch4_clim_diff_KBS_df,
            file=paste0(these_results_folder,
                        "co2e_ch4_clim_diff_KBS.csv"),
            row.names=TRUE)
  
  write.csv(co2e_ch4_clim_diff_LRF_df,
            file=paste0(these_results_folder,
                        "co2e_ch4_clim_diff_LRF.csv"),
            row.names=TRUE)
  
  ### Model means ---------------------
  
  co2e_ch4means <- gwp_scenario_means_piv[gwp_scenario_means_piv$source == "mean_CO2e_CH4",]
  co2e_ch4means_KBS_Baseline <- co2e_ch4means[co2e_ch4means$site_name=="KBS" &
                                                co2e_ch4means$climate_desc=="Baseline",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_ch4means_KBS_UKESM <- co2e_ch4means[co2e_ch4means$site_name=="KBS" &
                                             co2e_ch4means$climate_desc=="UKESM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_ch4means_LRF_Baseline <- co2e_ch4means[co2e_ch4means$site_name=="LRF" &
                                                co2e_ch4means$climate_desc=="Baseline",
                                              c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  co2e_ch4means_LRF_UKESM <- co2e_ch4means[co2e_ch4means$site_name=="LRF" &
                                             co2e_ch4means$climate_desc=="UKESM",
                                           c("scenario_abbrev","vals")] %>%
    arrange(scenario_abbrev)
  
  
  co2e_ch4means_clim_diff_KBS <- as.numeric(co2e_ch4means_KBS_UKESM$vals-co2e_ch4means_KBS_Baseline$vals)
  co2e_ch4means_clim_diff_LRF <- as.numeric(co2e_ch4means_LRF_UKESM$vals-co2e_ch4means_LRF_Baseline$vals)
  
  co2e_ch4means_mean_clim_diff_KBS <- mean(co2e_ch4means_clim_diff_KBS)
  co2e_ch4means_mean_clim_diff_LRF <- mean(co2e_ch4means_clim_diff_LRF)
  
  #*******************************************************************

    ggsave(filename=paste0(these_results_folder,"/pub_all_soce.jpg"),
         plot=g_soce, width=16, height=13, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_all_n2oe.jpg"),
         plot=g_n2oe, width=16, height=13, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_all_ch4e.jpg"),
         plot=g_ch4e, width=16, height=13, dpi=300)
  
  #*******************************************************************
  
  
  #*******************************************************************
  
  # Crops Bar charts --------------------------------------------------------------------
  
  g_maize <- kbs_summary_output_piv[kbs_summary_output_piv$source == "Maize_Diff_Mgha" &
                                      kbs_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=6.5, color="grey") +
    geom_vline(xintercept=10.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=kbs_scenario_means,
             aes(x=scenario_abbrev, y=mean_MaizeYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("KBS Maize Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_maize
  
  
  g_soybean <- kbs_summary_output_piv[kbs_summary_output_piv$source == "Soybean_Diff_Mgha" &
                                        kbs_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=6.5, color="grey") +
    geom_vline(xintercept=10.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=kbs_scenario_means,
             aes(x=scenario_abbrev, y=mean_SoyYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("KBS Soybean Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_soybean
  
  g_wheat <- kbs_summary_output_piv[kbs_summary_output_piv$source == "Wheat_Diff_Mgha" &
                                      kbs_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=6.5, color="grey") +
    geom_vline(xintercept=10.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=kbs_scenario_means,
             aes(x=scenario_abbrev, y=mean_WheatYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("KBS Wheat Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_wheat
  
  g_cotton <- lrf_summary_output_piv[lrf_summary_output_piv$source == "Cotton_Diff_Mgha" &
                                       lrf_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=lrf_scenario_means,
             aes(x=scenario_abbrev, y=mean_CottonYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Lint Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("LRF Cotton Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_cotton
  
  g_sorghum <- lrf_summary_output_piv[lrf_summary_output_piv$source == "Sorghum_Diff_Mgha" &
                                        lrf_summary_output_piv$Model %in% c("APSIM","Daycent"),] %>%
    ggplot(aes(x=scenario_abbrev, y=vals, fill=Model)) +
    scale_x_discrete() +
    geom_hline(yintercept=0, color="darkgrey") +
    geom_vline(xintercept=5.5, color="grey") +
    geom_vline(xintercept=7.5, color="grey") +
    geom_vline(xintercept=8.5, color="grey") +
    geom_vline(xintercept=12.5, color="grey") +
    geom_col(position="dodge",colour=NA) +
    geom_col(data=lrf_scenario_means,
             aes(x=scenario_abbrev, y=mean_SorghumYld_Mgha), color= "black",
             fill=NA, position="dodge") +
    ylab(expression('Grain Yield (Mg ha ' ^-1*')')) +
    xlab("") +
    ggtitle(paste0("LRF Sorghum Yield Change by ",
                   end_fut_period_year)) +
    scale_fill_manual(labels=c("APSIM","Daycent"),
                      values=cbPalette9[c(8,2)],
                      name="Model") +
    facet_wrap(~climate_desc,nrow=5,strip.position = "right") +
    theme_classic(base_family = "serif", base_size = 25) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_sorghum
  
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_maize.jpg"),
         plot=g_maize, width=8, height=8, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_soybean.jpg"),
         plot=g_soybean, width=8, height=8, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_kbs_wheat.jpg"),
         plot=g_wheat, width=8, height=8, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_cotton.jpg"),
         plot=g_cotton, width=8, height=8, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_lrf_sorghum.jpg"),
         plot=g_sorghum, width=8, height=8, dpi=300)
  
  
  
  #*******************************************************************

  # GHG Calib Bar charts --------------------------------------------------------------------
  
  g_n2o_calib <- n2o_calib_output_df_piv %>%
    ggplot(aes(x=scenario_abbrev.x, y=n2o_val, fill=Source)) +
    scale_x_discrete() +
    geom_col(position="dodge",colour=NA) +
    xlab("") +
    ylab(expression('N'[2]*'O (g N ha' ^'-1'*')')) +
    ggtitle(bquote("Cumulative Modeled and Observed N"[2]*"O")) +
    scale_fill_manual(labels=c("APSIM","Daycent","Observed"),
                      values=c(APSIM_color,Daycent_color,Observed_color)) +
    facet_grid(cols=vars(site_name)) +
    theme_classic(base_family = "serif", base_size = 27) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_n2o_calib
  
  g_ch4_calib <- ch4_calib_output_df_piv %>%
    ggplot(aes(x=scenario_abbrev.x, y=ch4_val, fill=Source)) +
    scale_x_discrete() +
    geom_col(position="dodge",colour=NA) +
    xlab("") +
    ylab(expression('CH'[4]*' (g N ha' ^'-1'*')')) +
    ggtitle(bquote("KBS Cumulative Modeled and Observed CH"[4])) +
    scale_fill_manual(labels=c("Daycent","Observed"),
                      values=c(Daycent_color,Observed_color)) +
    theme_classic(base_family = "serif", base_size = 20) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill=NA),
          strip.background = element_blank(),
          axis.line = element_line(),
          # axis.text.x = element_text(angle = 45,
          #                            hjust = 1),
          legend.position = "top",
          legend.key = element_blank())
  
  g_ch4_calib
  
  ggsave(filename=paste0(these_results_folder,"/pub_n2o_calib.jpg"),
         plot=g_n2o_calib, width=10, height=7, dpi=300)
  ggsave(filename=paste0(these_results_folder,"/pub_ch4_calib.jpg"),
         plot=g_ch4_calib, width=7.5, height=5, dpi=300)
  

    # Model components ---------------------------------------------------------
  
  ## over time in each scenario ------------------------------------
  
  ### KBS ---------------------
  
  kbs_model_component_apsim_base_time <-   kbs_model_components[kbs_model_components$Climate_Scenario==1 &
                                                                  kbs_model_components$Model=="APSIM",] %>%
    # add last values and % change in components over time
    mutate(SW_20cm_last=SW_20cm_first+SW_20cm_change,
           SW_40cm_last=SW_40cm_first+SW_40cm_change,
           SW_60cm_last=SW_60cm_first+SW_60cm_change,
           DW_20cm_last = DW_20cm_first+DW_20cm_change,
           DW_40cm_last = DW_40cm_first+DW_40cm_change,
           DW_60cm_last = DW_60cm_first+DW_60cm_change,
           DW_0to60cm_last = DW_0to60cm_first+DW_0to60cm_change,
           SW_25cm_last = SW_25cm_first+SW_25cm_change,
           DW_25cm_last = DW_25cm_first+DW_25cm_change,
           SoilT_20cm_last=SoilT_20cm_first+SoilT_20cm_change,
           SoilT_40cm_last = SoilT_40cm_first+SoilT_40cm_change,
           SoilT_60cm_last = SoilT_60cm_first+SoilT_60cm_change,
           SoilT_25cm_last = SoilT_25cm_first+SoilT_25cm_change,
           NO3_20cm_last= NO3_20cm_first+NO3_20cm_change,
           NO3_40cm_last = NO3_40cm_first+NO3_40cm_change,
           NO3_60cm_last = NO3_60cm_first+NO3_60cm_change,
           NO3_0to60cm_last = NO3_0to60cm_first+NO3_0to60cm_change,
           N2O_20cm_last= N2O_20cm_first+N2O_20cm_change,
           N2O_40cm_last = N2O_40cm_first+N2O_40cm_change,
           N2O_60cm_last = N2O_60cm_first+N2O_60cm_change,
           N2O_0to60cm_last = N2O_0to60cm_first+N2O_0to60cm_change,
           N2O_profile_last = N2O_profile_first+N2O_profile_change,
           BC_25cm_last = BC_25cm_first+BC_25cm_change,
           BN_25cm_last = BN_25cm_first+BN_25cm_change,
           HC_25cm_last = HC_25cm_first+HC_25cm_change,
           HN_25cm_last = HN_25cm_first+HN_25cm_change,
           CinB_25cm_last = CinB_25cm_first+CinB_25cm_change,
           CinH_25cm_last = CinH_25cm_first+CinH_25cm_change,
           CinBtoH_25cm_last = CinBtoH_25cm_first+CinBtoH_25cm_change,
           SOC_25cm_last = SOC_25cm_first+SOC_25cm_change,
           DW_2cm_last = DW_2cm_first+DW_2cm_change,
           DW_5cm_last = DW_5cm_first+DW_5cm_change,
           DW_10cm_last = DW_10cm_first+DW_10cm_change,
           WFPS_2cm_last = WFPS_2cm_first+WFPS_2cm_change,
           WFPS_5cm_last = WFPS_5cm_first+WFPS_5cm_change,
           WFPS_10cm_last = WFPS_10cm_first+WFPS_10cm_change,
           WFPS_20cm_last = WFPS_20cm_first+WFPS_20cm_change,
           WFPS_40cm_last = WFPS_40cm_first+WFPS_40cm_change,
           WFPS_60cm_last = WFPS_60cm_first+WFPS_60cm_change,
           SoilT_2cm_last = SoilT_2cm_first+SoilT_2cm_change,
           SoilT_5cm_last = SoilT_5cm_first+SoilT_5cm_change,
           SoilT_10cm_last = SoilT_10cm_first+SoilT_10cm_change,
           SoilT_15cm_last = SoilT_15cm_first+SoilT_15cm_change,
           NO3_2cm_last = NO3_2cm_first+NO3_2cm_change,
           NO3_5cm_last = NO3_5cm_first+NO3_5cm_change,
           NO3_10cm_last = NO3_10cm_first+NO3_10cm_change,
           CH4_last = CH4_first+CH4_change,
           CI_last = CI_first+CI_change,
           SW_20cm_pctchg=round(SW_20cm_change/SW_20cm_first*100,1),
           SW_40cm_pctchg=round(SW_40cm_change/SW_40cm_first*100,1),
           SW_60cm_pctchg=round(SW_60cm_change/SW_60cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_40cm_pctchg = round(DW_40cm_change/DW_40cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_25cm_pctchg = round(SW_25cm_change/SW_25cm_first*100,1),
           DW_25cm_pctchg = round(DW_25cm_change/DW_25cm_first*100,1),
           SoilT_20cm_pctchg=round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_40cm_pctchg = round(SoilT_40cm_change/SoilT_40cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_25cm_pctchg = round(SoilT_25cm_change/SoilT_25cm_first*100,1),
           NO3_20cm_pctchg=round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_40cm_pctchg = round(NO3_40cm_change/NO3_40cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_20cm_pctchg=round(N2O_20cm_change/N2O_20cm_first*100,1),
           N2O_40cm_pctchg = round(N2O_40cm_change/N2O_40cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_25cm_pctchg = round(BC_25cm_change/BC_25cm_first*100,1),
           BN_25cm_pctchg = round(BN_25cm_change/BN_25cm_first*100,1),
           HC_25cm_pctchg = round(HC_25cm_change/HC_25cm_first*100,1),
           HN_25cm_pctchg = round(HN_25cm_change/HN_25cm_first*100,1),
           CinB_25cm_pctchg = round(CinB_25cm_change/CinB_25cm_first*100,1),
           CinH_25cm_pctchg = round(CinH_25cm_change/CinH_25cm_first*100,1),
           CinBtoH_25cm_pctchg = round(CinBtoH_25cm_change/CinBtoH_25cm_first*100,1),
           SOC_25cm_pctchg = round(SOC_25cm_change/SOC_25cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_40cm_pctchg = round(WFPS_40cm_change/WFPS_40cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  
  kbs_model_component_daycent_base_time <-   kbs_model_components[kbs_model_components$Climate_Scenario==1 &
                                                                    kbs_model_components$Model=="Daycent",] %>%
    # add last values and % change in components over time
    mutate(SW_20cm_last=SW_20cm_first+SW_20cm_change,
           SW_40cm_last=SW_40cm_first+SW_40cm_change,
           SW_60cm_last=SW_60cm_first+SW_60cm_change,
           DW_20cm_last = DW_20cm_first+DW_20cm_change,
           DW_40cm_last = DW_40cm_first+DW_40cm_change,
           DW_60cm_last = DW_60cm_first+DW_60cm_change,
           DW_0to60cm_last = DW_0to60cm_first+DW_0to60cm_change,
           SW_25cm_last = SW_25cm_first+SW_25cm_change,
           DW_25cm_last = DW_25cm_first+DW_25cm_change,
           SoilT_20cm_last= SoilT_20cm_first+SoilT_20cm_change,
           SoilT_40cm_last = SoilT_40cm_first+SoilT_40cm_change,
           SoilT_60cm_last = SoilT_60cm_first+SoilT_60cm_change,
           SoilT_25cm_last = SoilT_25cm_first+SoilT_25cm_change,
           NO3_20cm_last= NO3_20cm_first+NO3_20cm_change,
           NO3_40cm_last = NO3_40cm_first+NO3_40cm_change,
           NO3_60cm_last = NO3_60cm_first+NO3_60cm_change,
           NO3_0to60cm_last = NO3_0to60cm_first+NO3_0to60cm_change,
           N2O_20cm_last= N2O_20cm_first+N2O_20cm_change,
           N2O_40cm_last = N2O_40cm_first+N2O_40cm_change,
           N2O_60cm_last = N2O_60cm_first+N2O_60cm_change,
           N2O_0to60cm_last = N2O_0to60cm_first+N2O_0to60cm_change,
           N2O_profile_last = N2O_profile_first+N2O_profile_change,
           BC_25cm_last = BC_25cm_first+BC_25cm_change,
           BN_25cm_last = BN_25cm_first+BN_25cm_change,
           HC_25cm_last = HC_25cm_first+HC_25cm_change,
           HN_25cm_last = HN_25cm_first+HN_25cm_change,
           CinB_25cm_last = CinB_25cm_first+CinB_25cm_change,
           CinH_25cm_last = CinH_25cm_first+CinH_25cm_change,
           CinBtoH_25cm_last = CinBtoH_25cm_first+CinBtoH_25cm_change,
           SOC_25cm_last = SOC_25cm_first+SOC_25cm_change,
           DW_2cm_last = DW_2cm_first+DW_2cm_change,
           DW_5cm_last = DW_5cm_first+DW_5cm_change,
           DW_10cm_last = DW_10cm_first+DW_10cm_change,
           WFPS_2cm_last = WFPS_2cm_first+WFPS_2cm_change,
           WFPS_5cm_last = WFPS_5cm_first+WFPS_5cm_change,
           WFPS_10cm_last = WFPS_10cm_first+WFPS_10cm_change,
           WFPS_20cm_last = WFPS_20cm_first+WFPS_20cm_change,
           WFPS_40cm_last = WFPS_40cm_first+WFPS_40cm_change,
           WFPS_60cm_last = WFPS_60cm_first+WFPS_60cm_change,
           SoilT_2cm_last = SoilT_2cm_first+SoilT_2cm_change,
           SoilT_5cm_last = SoilT_5cm_first+SoilT_5cm_change,
           SoilT_10cm_last = SoilT_10cm_first+SoilT_10cm_change,
           SoilT_15cm_last = SoilT_15cm_first+SoilT_15cm_change,
           NO3_2cm_last = NO3_2cm_first+NO3_2cm_change,
           NO3_5cm_last = NO3_5cm_first+NO3_5cm_change,
           NO3_10cm_last = NO3_10cm_first+NO3_10cm_change,
           CH4_last = CH4_first+CH4_change,
           CI_last = CI_first+CI_change,
           SW_20cm_pctchg=round(SW_20cm_change/SW_20cm_first*100,1),
           SW_40cm_pctchg=round(SW_40cm_change/SW_40cm_first*100,1),
           SW_60cm_pctchg=round(SW_60cm_change/SW_60cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_40cm_pctchg = round(DW_40cm_change/DW_40cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_25cm_pctchg = round(SW_25cm_change/SW_25cm_first*100,1),
           DW_25cm_pctchg = round(DW_25cm_change/DW_25cm_first*100,1),
           SoilT_20cm_pctchg=round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_40cm_pctchg = round(SoilT_40cm_change/SoilT_40cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_25cm_pctchg = round(SoilT_25cm_change/SoilT_25cm_first*100,1),
           NO3_20cm_pctchg=round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_40cm_pctchg = round(NO3_40cm_change/NO3_40cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_20cm_pctchg=round(N2O_20cm_change/N2O_20cm_first*100,1),
           N2O_40cm_pctchg = round(N2O_40cm_change/N2O_40cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_25cm_pctchg = round(BC_25cm_change/BC_25cm_first*100,1),
           BN_25cm_pctchg = round(BN_25cm_change/BN_25cm_first*100,1),
           HC_25cm_pctchg = round(HC_25cm_change/HC_25cm_first*100,1),
           HN_25cm_pctchg = round(HN_25cm_change/HN_25cm_first*100,1),
           CinB_25cm_pctchg = round(CinB_25cm_change/CinB_25cm_first*100,1),
           CinH_25cm_pctchg = round(CinH_25cm_change/CinH_25cm_first*100,1),
           CinBtoH_25cm_pctchg = round(CinBtoH_25cm_change/CinBtoH_25cm_first*100,1),
           SOC_25cm_pctchg = round(SOC_25cm_change/SOC_25cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_40cm_pctchg = round(WFPS_40cm_change/WFPS_40cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  kbs_model_component_apsim_ukesm_time <-   kbs_model_components[kbs_model_components$Climate_Scenario==5 &
                                                                   kbs_model_components$Model=="APSIM",] %>%
    # add last values and % change in components over time
    mutate(SW_20cm_last = SW_20cm_first+SW_20cm_change,
           SW_40cm_last = SW_40cm_first+SW_40cm_change,
           SW_60cm_last = SW_60cm_first+SW_60cm_change,
           DW_20cm_last = DW_20cm_first+DW_20cm_change,
           DW_40cm_last = DW_40cm_first+DW_40cm_change,
           DW_60cm_last = DW_60cm_first+DW_60cm_change,
           DW_0to60cm_last = DW_0to60cm_first+DW_0to60cm_change,
           SW_25cm_last = SW_25cm_first+SW_25cm_change,
           DW_25cm_last = DW_25cm_first+DW_25cm_change,
           SoilT_20cm_last = SoilT_20cm_first+SoilT_20cm_change,
           SoilT_40cm_last = SoilT_40cm_first+SoilT_40cm_change,
           SoilT_60cm_last = SoilT_60cm_first+SoilT_60cm_change,
           SoilT_25cm_last = SoilT_25cm_first+SoilT_25cm_change,
           NO3_20cm_last = NO3_20cm_first+NO3_20cm_change,
           NO3_40cm_last = NO3_40cm_first+NO3_40cm_change,
           NO3_60cm_last = NO3_60cm_first+NO3_60cm_change,
           NO3_0to60cm_last = NO3_0to60cm_first+NO3_0to60cm_change,
           N2O_20cm_last = N2O_20cm_first+N2O_20cm_change,
           N2O_40cm_last = N2O_40cm_first+N2O_40cm_change,
           N2O_60cm_last = N2O_60cm_first+N2O_60cm_change,
           N2O_0to60cm_last = N2O_0to60cm_first+N2O_0to60cm_change,
           N2O_profile_last = N2O_profile_first+N2O_profile_change,
           BC_25cm_last = BC_25cm_first+BC_25cm_change,
           BN_25cm_last = BN_25cm_first+BN_25cm_change,
           HC_25cm_last = HC_25cm_first+HC_25cm_change,
           HN_25cm_last = HN_25cm_first+HN_25cm_change,
           CinB_25cm_last = CinB_25cm_first+CinB_25cm_change,
           CinH_25cm_last = CinH_25cm_first+CinH_25cm_change,
           CinBtoH_25cm_last = CinBtoH_25cm_first+CinBtoH_25cm_change,
           SOC_25cm_last = SOC_25cm_first+SOC_25cm_change,
           DW_2cm_last = DW_2cm_first+DW_2cm_change,
           DW_5cm_last = DW_5cm_first+DW_5cm_change,
           DW_10cm_last = DW_10cm_first+DW_10cm_change,
           WFPS_2cm_last = WFPS_2cm_first+WFPS_2cm_change,
           WFPS_5cm_last = WFPS_5cm_first+WFPS_5cm_change,
           WFPS_10cm_last = WFPS_10cm_first+WFPS_10cm_change,
           WFPS_20cm_last = WFPS_20cm_first+WFPS_20cm_change,
           WFPS_40cm_last = WFPS_40cm_first+WFPS_40cm_change,
           WFPS_60cm_last = WFPS_60cm_first+WFPS_60cm_change,
           SoilT_2cm_last = SoilT_2cm_first+SoilT_2cm_change,
           SoilT_5cm_last = SoilT_5cm_first+SoilT_5cm_change,
           SoilT_10cm_last = SoilT_10cm_first+SoilT_10cm_change,
           SoilT_15cm_last = SoilT_15cm_first+SoilT_15cm_change,
           NO3_2cm_last = NO3_2cm_first+NO3_2cm_change,
           NO3_5cm_last = NO3_5cm_first+NO3_5cm_change,
           NO3_10cm_last = NO3_10cm_first+NO3_10cm_change,
           CH4_last = CH4_first+CH4_change,
           CI_last = CI_first+CI_change,
           SW_20cm_pctchg = round(SW_20cm_change/SW_20cm_first*100,1),
           SW_40cm_pctchg = round(SW_40cm_change/SW_40cm_first*100,1),
           SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_40cm_pctchg = round(DW_40cm_change/DW_40cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_25cm_pctchg = round(SW_25cm_change/SW_25cm_first*100,1),
           DW_25cm_pctchg = round(DW_25cm_change/DW_25cm_first*100,1),
           SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_40cm_pctchg = round(SoilT_40cm_change/SoilT_40cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_25cm_pctchg = round(SoilT_25cm_change/SoilT_25cm_first*100,1),
           NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_40cm_pctchg = round(NO3_40cm_change/NO3_40cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_20cm_pctchg = round(N2O_20cm_change/N2O_20cm_first*100,1),
           N2O_40cm_pctchg = round(N2O_40cm_change/N2O_40cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_25cm_pctchg = round(BC_25cm_change/BC_25cm_first*100,1),
           BN_25cm_pctchg = round(BN_25cm_change/BN_25cm_first*100,1),
           HC_25cm_pctchg = round(HC_25cm_change/HC_25cm_first*100,1),
           HN_25cm_pctchg = round(HN_25cm_change/HN_25cm_first*100,1),
           CinB_25cm_pctchg = round(CinB_25cm_change/CinB_25cm_first*100,1),
           CinH_25cm_pctchg = round(CinH_25cm_change/CinH_25cm_first*100,1),
           CinBtoH_25cm_pctchg = round(CinBtoH_25cm_change/CinBtoH_25cm_first*100,1),
           SOC_25cm_pctchg = round(SOC_25cm_change/SOC_25cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_40cm_pctchg = round(WFPS_40cm_change/WFPS_40cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  
  kbs_model_component_daycent_ukesm_time <-   kbs_model_components[kbs_model_components$Climate_Scenario==5 &
                                                                     kbs_model_components$Model=="Daycent",] %>%
    # add last values and % change in components over time
    mutate(SW_20cm_last=SW_20cm_change+SW_20cm_first,
           SW_40cm_last=SW_40cm_change+SW_40cm_first,
           SW_60cm_last=SW_60cm_change+SW_60cm_first,
           DW_20cm_last = DW_20cm_change+DW_20cm_first,
           DW_40cm_last = DW_40cm_change+DW_40cm_first,
           DW_60cm_last = DW_60cm_change+DW_60cm_first,
           DW_0to60cm_last = DW_0to60cm_change+DW_0to60cm_first,
           SW_25cm_last = SW_25cm_change+SW_25cm_first,
           DW_25cm_last = DW_25cm_change+DW_25cm_first,
           SoilT_20cm_last=SoilT_20cm_change+SoilT_20cm_first,
           SoilT_40cm_last = SoilT_40cm_change+SoilT_40cm_first,
           SoilT_60cm_last = SoilT_60cm_change+SoilT_60cm_first,
           SoilT_25cm_last = SoilT_25cm_change+SoilT_25cm_first,
           NO3_20cm_last=NO3_20cm_change+NO3_20cm_first,
           NO3_40cm_last = NO3_40cm_change+NO3_40cm_first,
           NO3_60cm_last = NO3_60cm_change+NO3_60cm_first,
           NO3_0to60cm_last = NO3_0to60cm_change+NO3_0to60cm_first,
           N2O_20cm_last=N2O_20cm_change+N2O_20cm_first,
           N2O_40cm_last = N2O_40cm_change+N2O_40cm_first,
           N2O_60cm_last = N2O_60cm_change+N2O_60cm_first,
           N2O_0to60cm_last = N2O_0to60cm_change+N2O_0to60cm_first,
           N2O_profile_last = N2O_profile_change+N2O_profile_first,
           BC_25cm_last = BC_25cm_change+BC_25cm_first,
           BN_25cm_last = BN_25cm_change+BN_25cm_first,
           HC_25cm_last = HC_25cm_change+HC_25cm_first,
           HN_25cm_last = HN_25cm_change+HN_25cm_first,
           CinB_25cm_last = CinB_25cm_change+CinB_25cm_first,
           CinH_25cm_last = CinH_25cm_change+CinH_25cm_first,
           CinBtoH_25cm_last = CinBtoH_25cm_change+CinBtoH_25cm_first,
           SOC_25cm_last = SOC_25cm_change+SOC_25cm_first,
           DW_2cm_last = DW_2cm_change+DW_2cm_first,
           DW_5cm_last = DW_5cm_change+DW_5cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           WFPS_2cm_last = WFPS_2cm_change+WFPS_2cm_first,
           WFPS_5cm_last = WFPS_5cm_change+WFPS_5cm_first,
           WFPS_10cm_last = WFPS_10cm_change+WFPS_10cm_first,
           WFPS_20cm_last = WFPS_20cm_change+WFPS_20cm_first,
           WFPS_40cm_last = WFPS_40cm_change+WFPS_40cm_first,
           WFPS_60cm_last = WFPS_60cm_change+WFPS_60cm_first,
           SoilT_2cm_last = SoilT_2cm_change+SoilT_2cm_first,
           SoilT_5cm_last = SoilT_5cm_change+SoilT_5cm_first,
           SoilT_10cm_last = SoilT_10cm_change+SoilT_10cm_first,
           SoilT_15cm_last = SoilT_15cm_change+SoilT_15cm_first,
           NO3_2cm_last = NO3_2cm_change+NO3_2cm_first,
           NO3_5cm_last = NO3_5cm_change+NO3_5cm_first,
           NO3_10cm_last = NO3_10cm_change+NO3_10cm_first,
           CH4_last = CH4_change+CH4_first,
           CI_last = CI_change+CI_first,
           SW_20cm_pctchg=round(SW_20cm_change/SW_20cm_first*100,1),
           SW_40cm_pctchg=round(SW_40cm_change/SW_40cm_first*100,1),
           SW_60cm_pctchg=round(SW_60cm_change/SW_60cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_40cm_pctchg = round(DW_40cm_change/DW_40cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_25cm_pctchg = round(SW_25cm_change/SW_25cm_first*100,1),
           DW_25cm_pctchg = round(DW_25cm_change/DW_25cm_first*100,1),
           SoilT_20cm_pctchg=round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_40cm_pctchg = round(SoilT_40cm_change/SoilT_40cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_25cm_pctchg = round(SoilT_25cm_change/SoilT_25cm_first*100,1),
           NO3_20cm_pctchg=round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_40cm_pctchg = round(NO3_40cm_change/NO3_40cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_20cm_pctchg=round(N2O_20cm_change/N2O_20cm_first*100,1),
           N2O_40cm_pctchg = round(N2O_40cm_change/N2O_40cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_25cm_pctchg = round(BC_25cm_change/BC_25cm_first*100,1),
           BN_25cm_pctchg = round(BN_25cm_change/BN_25cm_first*100,1),
           HC_25cm_pctchg = round(HC_25cm_change/HC_25cm_first*100,1),
           HN_25cm_pctchg = round(HN_25cm_change/HN_25cm_first*100,1),
           CinB_25cm_pctchg = round(CinB_25cm_change/CinB_25cm_first*100,1),
           CinH_25cm_pctchg = round(CinH_25cm_change/CinH_25cm_first*100,1),
           CinBtoH_25cm_pctchg = round(CinBtoH_25cm_change/CinBtoH_25cm_first*100,1),
           SOC_25cm_pctchg = round(SOC_25cm_change/SOC_25cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_40cm_pctchg = round(WFPS_40cm_change/WFPS_40cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  ### LRF ---------------------
  
  lrf_model_component_apsim_base_time <-   lrf_model_components[lrf_model_components$Climate_Scenario==1 &
                                                                  lrf_model_components$Model=="APSIM",] %>%
    # add last values and % change in components over time
    mutate(SW_5cm_last = SW_5cm_change+SW_5cm_first,
           SW_15cm_last = SW_15cm_change+SW_15cm_first,
           SW_35cm_last = SW_35cm_change+SW_35cm_first,
           SW_60cm_last = SW_60cm_change+SW_60cm_first,
           DW_5cm_last = DW_5cm_change+DW_5cm_first,
           DW_15cm_last = DW_15cm_change+DW_15cm_first,
           DW_35cm_last = DW_35cm_change+DW_35cm_first,
           DW_60cm_last = DW_60cm_change+DW_60cm_first,
           DW_0to60cm_last = DW_0to60cm_change+DW_0to60cm_first,
           SW_10cm_last = SW_10cm_change+SW_10cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           SoilT_5cm_last = SoilT_5cm_change+SoilT_5cm_first,
           SoilT_15cm_last = SoilT_15cm_change+SoilT_15cm_first,
           SoilT_35cm_last = SoilT_35cm_change+SoilT_35cm_first,
           SoilT_60cm_last = SoilT_60cm_change+SoilT_60cm_first,
           SoilT_10cm_last = SoilT_10cm_change+SoilT_10cm_first,
           NO3_5cm_last = NO3_5cm_change+NO3_5cm_first,
           NO3_15cm_last = NO3_15cm_change+NO3_15cm_first,
           NO3_35cm_last = NO3_35cm_change+NO3_35cm_first,
           NO3_60cm_last = NO3_60cm_change+NO3_60cm_first,
           NO3_0to60cm_last = NO3_0to60cm_change+NO3_0to60cm_first,
           N2O_5cm_last = N2O_5cm_change+N2O_5cm_first,
           N2O_15cm_last = N2O_15cm_change+N2O_15cm_first,
           N2O_35cm_last = N2O_35cm_change+N2O_35cm_first,
           N2O_60cm_last = N2O_60cm_change+N2O_60cm_first,
           N2O_0to60cm_last = N2O_0to60cm_change+N2O_0to60cm_first,
           N2O_profile_last = N2O_profile_change+N2O_profile_first,
           BC_10cm_last = BC_10cm_change+BC_10cm_first,
           BN_10cm_last = BN_10cm_change+BN_10cm_first,
           HC_10cm_last = HC_10cm_change+HC_10cm_first,
           HN_10cm_last = HN_10cm_change+HN_10cm_first,
           CinB_10cm_last = CinB_10cm_change+CinB_10cm_first,
           CinH_10cm_last = CinH_10cm_change+CinH_10cm_first,
           CinBtoH_10cm_last = CinBtoH_10cm_change+CinBtoH_10cm_first,
           SOC_10cm_last = SOC_10cm_change+SOC_10cm_first,
           DW_2cm_last = DW_2cm_change+DW_2cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           DW_20cm_last = DW_20cm_change+DW_20cm_first,
           DW_30cm_last = DW_30cm_change+DW_30cm_first,
           DW_45cm_last = DW_45cm_change+DW_45cm_first,
           WFPS_2cm_last = WFPS_2cm_change+WFPS_2cm_first,
           WFPS_5cm_last = WFPS_5cm_change+WFPS_5cm_first,
           WFPS_10cm_last = WFPS_10cm_change+WFPS_10cm_first,
           WFPS_20cm_last = WFPS_20cm_change+WFPS_20cm_first,
           WFPS_30cm_last = WFPS_30cm_change+WFPS_30cm_first,
           WFPS_45cm_last = WFPS_45cm_change+WFPS_45cm_first,
           WFPS_60cm_last = WFPS_60cm_change+WFPS_60cm_first,
           SoilT_2cm_last = SoilT_2cm_change+SoilT_2cm_first,
           SoilT_20cm_last = SoilT_20cm_change+SoilT_20cm_first,
           SoilT_30cm_last = SoilT_30cm_change+SoilT_30cm_first,
           SoilT_45cm_last = SoilT_45cm_change+SoilT_45cm_first,
           NO3_2cm_last = NO3_2cm_change+NO3_2cm_first,
           NO3_10cm_last = NO3_10cm_change+NO3_10cm_first,
           NO3_20cm_last = NO3_20cm_change+NO3_20cm_first,
           NO3_30cm_last = NO3_30cm_change+NO3_30cm_first,
           NO3_45cm_last = NO3_45cm_change+NO3_45cm_first,
           CH4_last = CH4_change+CH4_first,
           CI_last = CI_change+CI_first,
           SW_5cm_pctchg = round(SW_5cm_change/SW_5cm_first*100,1),
           SW_15cm_pctchg = round(SW_15cm_change/SW_15cm_first*100,1),
           SW_35cm_pctchg = round(SW_35cm_change/SW_35cm_first*100,1),
           SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_15cm_pctchg = round(DW_15cm_change/DW_15cm_first*100,1),
           DW_35cm_pctchg = round(DW_35cm_change/DW_35cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_10cm_pctchg = round(SW_10cm_change/SW_10cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           SoilT_35cm_pctchg = round(SoilT_35cm_change/SoilT_35cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_15cm_pctchg = round(NO3_15cm_change/NO3_15cm_first*100,1),
           NO3_35cm_pctchg = round(NO3_35cm_change/NO3_35cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_5cm_pctchg = round(N2O_5cm_change/N2O_5cm_first*100,1),
           N2O_15cm_pctchg = round(N2O_15cm_change/N2O_15cm_first*100,1),
           N2O_35cm_pctchg = round(N2O_35cm_change/N2O_35cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_10cm_pctchg = round(BC_10cm_change/BC_10cm_first*100,1),
           BN_10cm_pctchg = round(BN_10cm_change/BN_10cm_first*100,1),
           HC_10cm_pctchg = round(HC_10cm_change/HC_10cm_first*100,1),
           HN_10cm_pctchg = round(HN_10cm_change/HN_10cm_first*100,1),
           CinB_10cm_pctchg = round(CinB_10cm_change/CinB_10cm_first*100,1),
           CinH_10cm_pctchg = round(CinH_10cm_change/CinH_10cm_first*100,1),
           CinBtoH_10cm_pctchg = round(CinBtoH_10cm_change/CinBtoH_10cm_first*100,1),
           SOC_10cm_pctchg = round(SOC_10cm_change/SOC_10cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_30cm_pctchg = round(DW_30cm_change/DW_30cm_first*100,1),
           DW_45cm_pctchg = round(DW_45cm_change/DW_45cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_30cm_pctchg = round(WFPS_30cm_change/WFPS_30cm_first*100,1),
           WFPS_45cm_pctchg = round(WFPS_45cm_change/WFPS_45cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_30cm_pctchg = round(SoilT_30cm_change/SoilT_30cm_first*100,1),
           SoilT_45cm_pctchg = round(SoilT_45cm_change/SoilT_45cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_30cm_pctchg = round(NO3_30cm_change/NO3_30cm_first*100,1),
           NO3_45cm_pctchg = round(NO3_45cm_change/NO3_45cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  lrf_model_component_daycent_base_time <-   lrf_model_components[lrf_model_components$Climate_Scenario==1 &
                                                                    lrf_model_components$Model=="Daycent",] %>%
    # add last values and % change in components over time
    mutate(SW_5cm_last = SW_5cm_change+SW_5cm_first,
           SW_15cm_last = SW_15cm_change+SW_15cm_first,
           SW_35cm_last = SW_35cm_change+SW_35cm_first,
           SW_60cm_last = SW_60cm_change+SW_60cm_first,
           DW_5cm_last = DW_5cm_change+DW_5cm_first,
           DW_15cm_last = DW_15cm_change+DW_15cm_first,
           DW_35cm_last = DW_35cm_change+DW_35cm_first,
           DW_60cm_last = DW_60cm_change+DW_60cm_first,
           DW_0to60cm_last = DW_0to60cm_change+DW_0to60cm_first,
           SW_10cm_last = SW_10cm_change+SW_10cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           SoilT_5cm_last = SoilT_5cm_change+SoilT_5cm_first,
           SoilT_15cm_last = SoilT_15cm_change+SoilT_15cm_first,
           SoilT_35cm_last = SoilT_35cm_change+SoilT_35cm_first,
           SoilT_60cm_last = SoilT_60cm_change+SoilT_60cm_first,
           SoilT_10cm_last = SoilT_10cm_change+SoilT_10cm_first,
           NO3_5cm_last = NO3_5cm_change+NO3_5cm_first,
           NO3_15cm_last = NO3_15cm_change+NO3_15cm_first,
           NO3_35cm_last = NO3_35cm_change+NO3_35cm_first,
           NO3_60cm_last = NO3_60cm_change+NO3_60cm_first,
           NO3_0to60cm_last = NO3_0to60cm_change+NO3_0to60cm_first,
           N2O_5cm_last = N2O_5cm_change+N2O_5cm_first,
           N2O_15cm_last = N2O_15cm_change+N2O_15cm_first,
           N2O_35cm_last = N2O_35cm_change+N2O_35cm_first,
           N2O_60cm_last = N2O_60cm_change+N2O_60cm_first,
           N2O_0to60cm_last = N2O_0to60cm_change+N2O_0to60cm_first,
           N2O_profile_last = N2O_profile_change+N2O_profile_first,
           BC_10cm_last = BC_10cm_change+BC_10cm_first,
           BN_10cm_last = BN_10cm_change+BN_10cm_first,
           HC_10cm_last = HC_10cm_change+HC_10cm_first,
           HN_10cm_last = HN_10cm_change+HN_10cm_first,
           CinB_10cm_last = CinB_10cm_change+CinB_10cm_first,
           CinH_10cm_last = CinH_10cm_change+CinH_10cm_first,
           CinBtoH_10cm_last = CinBtoH_10cm_change+CinBtoH_10cm_first,
           SOC_10cm_last = SOC_10cm_change+SOC_10cm_first,
           DW_2cm_last = DW_2cm_change+DW_2cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           DW_20cm_last = DW_20cm_change+DW_20cm_first,
           DW_30cm_last = DW_30cm_change+DW_30cm_first,
           DW_45cm_last = DW_45cm_change+DW_45cm_first,
           WFPS_2cm_last = WFPS_2cm_change+WFPS_2cm_first,
           WFPS_5cm_last = WFPS_5cm_change+WFPS_5cm_first,
           WFPS_10cm_last = WFPS_10cm_change+WFPS_10cm_first,
           WFPS_20cm_last = WFPS_20cm_change+WFPS_20cm_first,
           WFPS_30cm_last = WFPS_30cm_change+WFPS_30cm_first,
           WFPS_45cm_last = WFPS_45cm_change+WFPS_45cm_first,
           WFPS_60cm_last = WFPS_60cm_change+WFPS_60cm_first,
           SoilT_2cm_last = SoilT_2cm_change+SoilT_2cm_first,
           SoilT_20cm_last = SoilT_20cm_change+SoilT_20cm_first,
           SoilT_30cm_last = SoilT_30cm_change+SoilT_30cm_first,
           SoilT_45cm_last = SoilT_45cm_change+SoilT_45cm_first,
           NO3_2cm_last = NO3_2cm_change+NO3_2cm_first,
           NO3_10cm_last = NO3_10cm_change+NO3_10cm_first,
           NO3_20cm_last = NO3_20cm_change+NO3_20cm_first,
           NO3_30cm_last = NO3_30cm_change+NO3_30cm_first,
           NO3_45cm_last = NO3_45cm_change+NO3_45cm_first,
           CH4_last = CH4_change+CH4_first,
           CI_last = CI_change+CI_first,
           SW_5cm_pctchg = round(SW_5cm_change/SW_5cm_first*100,1),
           SW_15cm_pctchg = round(SW_15cm_change/SW_15cm_first*100,1),
           SW_35cm_pctchg = round(SW_35cm_change/SW_35cm_first*100,1),
           SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_15cm_pctchg = round(DW_15cm_change/DW_15cm_first*100,1),
           DW_35cm_pctchg = round(DW_35cm_change/DW_35cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_10cm_pctchg = round(SW_10cm_change/SW_10cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           SoilT_35cm_pctchg = round(SoilT_35cm_change/SoilT_35cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_15cm_pctchg = round(NO3_15cm_change/NO3_15cm_first*100,1),
           NO3_35cm_pctchg = round(NO3_35cm_change/NO3_35cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_5cm_pctchg = round(N2O_5cm_change/N2O_5cm_first*100,1),
           N2O_15cm_pctchg = round(N2O_15cm_change/N2O_15cm_first*100,1),
           N2O_35cm_pctchg = round(N2O_35cm_change/N2O_35cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_10cm_pctchg = round(BC_10cm_change/BC_10cm_first*100,1),
           BN_10cm_pctchg = round(BN_10cm_change/BN_10cm_first*100,1),
           HC_10cm_pctchg = round(HC_10cm_change/HC_10cm_first*100,1),
           HN_10cm_pctchg = round(HN_10cm_change/HN_10cm_first*100,1),
           CinB_10cm_pctchg = round(CinB_10cm_change/CinB_10cm_first*100,1),
           CinH_10cm_pctchg = round(CinH_10cm_change/CinH_10cm_first*100,1),
           CinBtoH_10cm_pctchg = round(CinBtoH_10cm_change/CinBtoH_10cm_first*100,1),
           SOC_10cm_pctchg = round(SOC_10cm_change/SOC_10cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_30cm_pctchg = round(DW_30cm_change/DW_30cm_first*100,1),
           DW_45cm_pctchg = round(DW_45cm_change/DW_45cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_30cm_pctchg = round(WFPS_30cm_change/WFPS_30cm_first*100,1),
           WFPS_45cm_pctchg = round(WFPS_45cm_change/WFPS_45cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_30cm_pctchg = round(SoilT_30cm_change/SoilT_30cm_first*100,1),
           SoilT_45cm_pctchg = round(SoilT_45cm_change/SoilT_45cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_30cm_pctchg = round(NO3_30cm_change/NO3_30cm_first*100,1),
           NO3_45cm_pctchg = round(NO3_45cm_change/NO3_45cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  lrf_model_component_apsim_ukesm_time <-   lrf_model_components[lrf_model_components$Climate_Scenario==5 &
                                                                   lrf_model_components$Model=="APSIM",] %>%
    # add last values and % change in components over time
    mutate(SW_5cm_last = SW_5cm_change+SW_5cm_first,
           SW_15cm_last = SW_15cm_change+SW_15cm_first,
           SW_35cm_last = SW_35cm_change+SW_35cm_first,
           SW_60cm_last = SW_60cm_change+SW_60cm_first,
           DW_5cm_last = DW_5cm_change+DW_5cm_first,
           DW_15cm_last = DW_15cm_change+DW_15cm_first,
           DW_35cm_last = DW_35cm_change+DW_35cm_first,
           DW_60cm_last = DW_60cm_change+DW_60cm_first,
           DW_0to60cm_last = DW_0to60cm_change+DW_0to60cm_first,
           SW_10cm_last = SW_10cm_change+SW_10cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           SoilT_5cm_last = SoilT_5cm_change+SoilT_5cm_first,
           SoilT_15cm_last = SoilT_15cm_change+SoilT_15cm_first,
           SoilT_35cm_last = SoilT_35cm_change+SoilT_35cm_first,
           SoilT_60cm_last = SoilT_60cm_change+SoilT_60cm_first,
           SoilT_10cm_last = SoilT_10cm_change+SoilT_10cm_first,
           NO3_5cm_last = NO3_5cm_change+NO3_5cm_first,
           NO3_15cm_last = NO3_15cm_change+NO3_15cm_first,
           NO3_35cm_last = NO3_35cm_change+NO3_35cm_first,
           NO3_60cm_last = NO3_60cm_change+NO3_60cm_first,
           NO3_0to60cm_last = NO3_0to60cm_change+NO3_0to60cm_first,
           N2O_5cm_last = N2O_5cm_change+N2O_5cm_first,
           N2O_15cm_last = N2O_15cm_change+N2O_15cm_first,
           N2O_35cm_last = N2O_35cm_change+N2O_35cm_first,
           N2O_60cm_last = N2O_60cm_change+N2O_60cm_first,
           N2O_0to60cm_last = N2O_0to60cm_change+N2O_0to60cm_first,
           N2O_profile_last = N2O_profile_change+N2O_profile_first,
           BC_10cm_last = BC_10cm_change+BC_10cm_first,
           BN_10cm_last = BN_10cm_change+BN_10cm_first,
           HC_10cm_last = HC_10cm_change+HC_10cm_first,
           HN_10cm_last = HN_10cm_change+HN_10cm_first,
           CinB_10cm_last = CinB_10cm_change+CinB_10cm_first,
           CinH_10cm_last = CinH_10cm_change+CinH_10cm_first,
           CinBtoH_10cm_last = CinBtoH_10cm_change+CinBtoH_10cm_first,
           SOC_10cm_last = SOC_10cm_change+SOC_10cm_first,
           DW_2cm_last = DW_2cm_change+DW_2cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           DW_20cm_last = DW_20cm_change+DW_20cm_first,
           DW_30cm_last = DW_30cm_change+DW_30cm_first,
           DW_45cm_last = DW_45cm_change+DW_45cm_first,
           WFPS_2cm_last = WFPS_2cm_change+WFPS_2cm_first,
           WFPS_5cm_last = WFPS_5cm_change+WFPS_5cm_first,
           WFPS_10cm_last = WFPS_10cm_change+WFPS_10cm_first,
           WFPS_20cm_last = WFPS_20cm_change+WFPS_20cm_first,
           WFPS_30cm_last = WFPS_30cm_change+WFPS_30cm_first,
           WFPS_45cm_last = WFPS_45cm_change+WFPS_45cm_first,
           WFPS_60cm_last = WFPS_60cm_change+WFPS_60cm_first,
           SoilT_2cm_last = SoilT_2cm_change+SoilT_2cm_first,
           SoilT_20cm_last = SoilT_20cm_change+SoilT_20cm_first,
           SoilT_30cm_last = SoilT_30cm_change+SoilT_30cm_first,
           SoilT_45cm_last = SoilT_45cm_change+SoilT_45cm_first,
           NO3_2cm_last = NO3_2cm_change+NO3_2cm_first,
           NO3_10cm_last = NO3_10cm_change+NO3_10cm_first,
           NO3_20cm_last = NO3_20cm_change+NO3_20cm_first,
           NO3_30cm_last = NO3_30cm_change+NO3_30cm_first,
           NO3_45cm_last = NO3_45cm_change+NO3_45cm_first,
           CH4_last = CH4_change+CH4_first,
           CI_last = CI_change+CI_first,
           SW_5cm_pctchg = round(SW_5cm_change/SW_5cm_first*100,1),
           SW_15cm_pctchg = round(SW_15cm_change/SW_15cm_first*100,1),
           SW_35cm_pctchg = round(SW_35cm_change/SW_35cm_first*100,1),
           SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_15cm_pctchg = round(DW_15cm_change/DW_15cm_first*100,1),
           DW_35cm_pctchg = round(DW_35cm_change/DW_35cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_10cm_pctchg = round(SW_10cm_change/SW_10cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           SoilT_35cm_pctchg = round(SoilT_35cm_change/SoilT_35cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_15cm_pctchg = round(NO3_15cm_change/NO3_15cm_first*100,1),
           NO3_35cm_pctchg = round(NO3_35cm_change/NO3_35cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_5cm_pctchg = round(N2O_5cm_change/N2O_5cm_first*100,1),
           N2O_15cm_pctchg = round(N2O_15cm_change/N2O_15cm_first*100,1),
           N2O_35cm_pctchg = round(N2O_35cm_change/N2O_35cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_10cm_pctchg = round(BC_10cm_change/BC_10cm_first*100,1),
           BN_10cm_pctchg = round(BN_10cm_change/BN_10cm_first*100,1),
           HC_10cm_pctchg = round(HC_10cm_change/HC_10cm_first*100,1),
           HN_10cm_pctchg = round(HN_10cm_change/HN_10cm_first*100,1),
           CinB_10cm_pctchg = round(CinB_10cm_change/CinB_10cm_first*100,1),
           CinH_10cm_pctchg = round(CinH_10cm_change/CinH_10cm_first*100,1),
           CinBtoH_10cm_pctchg = round(CinBtoH_10cm_change/CinBtoH_10cm_first*100,1),
           SOC_10cm_pctchg = round(SOC_10cm_change/SOC_10cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_30cm_pctchg = round(DW_30cm_change/DW_30cm_first*100,1),
           DW_45cm_pctchg = round(DW_45cm_change/DW_45cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_30cm_pctchg = round(WFPS_30cm_change/WFPS_30cm_first*100,1),
           WFPS_45cm_pctchg = round(WFPS_45cm_change/WFPS_45cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_30cm_pctchg = round(SoilT_30cm_change/SoilT_30cm_first*100,1),
           SoilT_45cm_pctchg = round(SoilT_45cm_change/SoilT_45cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_30cm_pctchg = round(NO3_30cm_change/NO3_30cm_first*100,1),
           NO3_45cm_pctchg = round(NO3_45cm_change/NO3_45cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  
  lrf_model_component_daycent_ukesm_time <-   lrf_model_components[lrf_model_components$Climate_Scenario==5 &
                                                                     lrf_model_components$Model=="Daycent",] %>%
    # add last values and % change in components over time
    mutate(SW_5cm_last = SW_5cm_change+SW_5cm_first,
           SW_15cm_last = SW_15cm_change+SW_15cm_first,
           SW_35cm_last = SW_35cm_change+SW_35cm_first,
           SW_60cm_last = SW_60cm_change+SW_60cm_first,
           DW_5cm_last = DW_5cm_change+DW_5cm_first,
           DW_15cm_last = DW_15cm_change+DW_15cm_first,
           DW_35cm_last = DW_35cm_change+DW_35cm_first,
           DW_60cm_last = DW_60cm_change+DW_60cm_first,
           DW_0to60cm_last = DW_0to60cm_change+DW_0to60cm_first,
           SW_10cm_last = SW_10cm_change+SW_10cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           SoilT_5cm_last = SoilT_5cm_change+SoilT_5cm_first,
           SoilT_15cm_last = SoilT_15cm_change+SoilT_15cm_first,
           SoilT_35cm_last = SoilT_35cm_change+SoilT_35cm_first,
           SoilT_60cm_last = SoilT_60cm_change+SoilT_60cm_first,
           SoilT_10cm_last = SoilT_10cm_change+SoilT_10cm_first,
           NO3_5cm_last = NO3_5cm_change+NO3_5cm_first,
           NO3_15cm_last = NO3_15cm_change+NO3_15cm_first,
           NO3_35cm_last = NO3_35cm_change+NO3_35cm_first,
           NO3_60cm_last = NO3_60cm_change+NO3_60cm_first,
           NO3_0to60cm_last = NO3_0to60cm_change+NO3_0to60cm_first,
           N2O_5cm_last = N2O_5cm_change+N2O_5cm_first,
           N2O_15cm_last = N2O_15cm_change+N2O_15cm_first,
           N2O_35cm_last = N2O_35cm_change+N2O_35cm_first,
           N2O_60cm_last = N2O_60cm_change+N2O_60cm_first,
           N2O_0to60cm_last = N2O_0to60cm_change+N2O_0to60cm_first,
           N2O_profile_last = N2O_profile_change+N2O_profile_first,
           BC_10cm_last = BC_10cm_change+BC_10cm_first,
           BN_10cm_last = BN_10cm_change+BN_10cm_first,
           HC_10cm_last = HC_10cm_change+HC_10cm_first,
           HN_10cm_last = HN_10cm_change+HN_10cm_first,
           CinB_10cm_last = CinB_10cm_change+CinB_10cm_first,
           CinH_10cm_last = CinH_10cm_change+CinH_10cm_first,
           CinBtoH_10cm_last = CinBtoH_10cm_change+CinBtoH_10cm_first,
           SOC_10cm_last = SOC_10cm_change+SOC_10cm_first,
           DW_2cm_last = DW_2cm_change+DW_2cm_first,
           DW_10cm_last = DW_10cm_change+DW_10cm_first,
           DW_20cm_last = DW_20cm_change+DW_20cm_first,
           DW_30cm_last = DW_30cm_change+DW_30cm_first,
           DW_45cm_last = DW_45cm_change+DW_45cm_first,
           WFPS_2cm_last = WFPS_2cm_change+WFPS_2cm_first,
           WFPS_5cm_last = WFPS_5cm_change+WFPS_5cm_first,
           WFPS_10cm_last = WFPS_10cm_change+WFPS_10cm_first,
           WFPS_20cm_last = WFPS_20cm_change+WFPS_20cm_first,
           WFPS_30cm_last = WFPS_30cm_change+WFPS_30cm_first,
           WFPS_45cm_last = WFPS_45cm_change+WFPS_45cm_first,
           WFPS_60cm_last = WFPS_60cm_change+WFPS_60cm_first,
           SoilT_2cm_last = SoilT_2cm_change+SoilT_2cm_first,
           SoilT_20cm_last = SoilT_20cm_change+SoilT_20cm_first,
           SoilT_30cm_last = SoilT_30cm_change+SoilT_30cm_first,
           SoilT_45cm_last = SoilT_45cm_change+SoilT_45cm_first,
           NO3_2cm_last = NO3_2cm_change+NO3_2cm_first,
           NO3_10cm_last = NO3_10cm_change+NO3_10cm_first,
           NO3_20cm_last = NO3_20cm_change+NO3_20cm_first,
           NO3_30cm_last = NO3_30cm_change+NO3_30cm_first,
           NO3_45cm_last = NO3_45cm_change+NO3_45cm_first,
           CH4_last = CH4_change+CH4_first,
           CI_last = CI_change+CI_first,
           SW_5cm_pctchg = round(SW_5cm_change/SW_5cm_first*100,1),
           SW_15cm_pctchg = round(SW_15cm_change/SW_15cm_first*100,1),
           SW_35cm_pctchg = round(SW_35cm_change/SW_35cm_first*100,1),
           SW_60cm_pctchg = round(SW_60cm_change/SW_60cm_first*100,1),
           DW_5cm_pctchg = round(DW_5cm_change/DW_5cm_first*100,1),
           DW_15cm_pctchg = round(DW_15cm_change/DW_15cm_first*100,1),
           DW_35cm_pctchg = round(DW_35cm_change/DW_35cm_first*100,1),
           DW_60cm_pctchg = round(DW_60cm_change/DW_60cm_first*100,1),
           DW_0to60cm_pctchg = round(DW_0to60cm_change/DW_0to60cm_first*100,1),
           SW_10cm_pctchg = round(SW_10cm_change/SW_10cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           SoilT_5cm_pctchg = round(SoilT_5cm_change/SoilT_5cm_first*100,1),
           SoilT_15cm_pctchg = round(SoilT_15cm_change/SoilT_15cm_first*100,1),
           SoilT_35cm_pctchg = round(SoilT_35cm_change/SoilT_35cm_first*100,1),
           SoilT_60cm_pctchg = round(SoilT_60cm_change/SoilT_60cm_first*100,1),
           SoilT_10cm_pctchg = round(SoilT_10cm_change/SoilT_10cm_first*100,1),
           NO3_5cm_pctchg = round(NO3_5cm_change/NO3_5cm_first*100,1),
           NO3_15cm_pctchg = round(NO3_15cm_change/NO3_15cm_first*100,1),
           NO3_35cm_pctchg = round(NO3_35cm_change/NO3_35cm_first*100,1),
           NO3_60cm_pctchg = round(NO3_60cm_change/NO3_60cm_first*100,1),
           NO3_0to60cm_pctchg = round(NO3_0to60cm_change/NO3_0to60cm_first*100,1),
           N2O_5cm_pctchg = round(N2O_5cm_change/N2O_5cm_first*100,1),
           N2O_15cm_pctchg = round(N2O_15cm_change/N2O_15cm_first*100,1),
           N2O_35cm_pctchg = round(N2O_35cm_change/N2O_35cm_first*100,1),
           N2O_60cm_pctchg = round(N2O_60cm_change/N2O_60cm_first*100,1),
           N2O_0to60cm_pctchg = round(N2O_0to60cm_change/N2O_0to60cm_first*100,1),
           N2O_profile_pctchg = round(N2O_profile_change/N2O_profile_first*100,1),
           BC_10cm_pctchg = round(BC_10cm_change/BC_10cm_first*100,1),
           BN_10cm_pctchg = round(BN_10cm_change/BN_10cm_first*100,1),
           HC_10cm_pctchg = round(HC_10cm_change/HC_10cm_first*100,1),
           HN_10cm_pctchg = round(HN_10cm_change/HN_10cm_first*100,1),
           CinB_10cm_pctchg = round(CinB_10cm_change/CinB_10cm_first*100,1),
           CinH_10cm_pctchg = round(CinH_10cm_change/CinH_10cm_first*100,1),
           CinBtoH_10cm_pctchg = round(CinBtoH_10cm_change/CinBtoH_10cm_first*100,1),
           SOC_10cm_pctchg = round(SOC_10cm_change/SOC_10cm_first*100,1),
           DW_2cm_pctchg = round(DW_2cm_change/DW_2cm_first*100,1),
           DW_10cm_pctchg = round(DW_10cm_change/DW_10cm_first*100,1),
           DW_20cm_pctchg = round(DW_20cm_change/DW_20cm_first*100,1),
           DW_30cm_pctchg = round(DW_30cm_change/DW_30cm_first*100,1),
           DW_45cm_pctchg = round(DW_45cm_change/DW_45cm_first*100,1),
           WFPS_2cm_pctchg = round(WFPS_2cm_change/WFPS_2cm_first*100,1),
           WFPS_5cm_pctchg = round(WFPS_5cm_change/WFPS_5cm_first*100,1),
           WFPS_10cm_pctchg = round(WFPS_10cm_change/WFPS_10cm_first*100,1),
           WFPS_20cm_pctchg = round(WFPS_20cm_change/WFPS_20cm_first*100,1),
           WFPS_30cm_pctchg = round(WFPS_30cm_change/WFPS_30cm_first*100,1),
           WFPS_45cm_pctchg = round(WFPS_45cm_change/WFPS_45cm_first*100,1),
           WFPS_60cm_pctchg = round(WFPS_60cm_change/WFPS_60cm_first*100,1),
           SoilT_2cm_pctchg = round(SoilT_2cm_change/SoilT_2cm_first*100,1),
           SoilT_20cm_pctchg = round(SoilT_20cm_change/SoilT_20cm_first*100,1),
           SoilT_30cm_pctchg = round(SoilT_30cm_change/SoilT_30cm_first*100,1),
           SoilT_45cm_pctchg = round(SoilT_45cm_change/SoilT_45cm_first*100,1),
           NO3_2cm_pctchg = round(NO3_2cm_change/NO3_2cm_first*100,1),
           NO3_10cm_pctchg = round(NO3_10cm_change/NO3_10cm_first*100,1),
           NO3_20cm_pctchg = round(NO3_20cm_change/NO3_20cm_first*100,1),
           NO3_30cm_pctchg = round(NO3_30cm_change/NO3_30cm_first*100,1),
           NO3_45cm_pctchg = round(NO3_45cm_change/NO3_45cm_first*100,1),
           CH4_pctchg = round(CH4_change/CH4_first*100,1),
           CI_pctchg = round(CI_change/CI_first*100,1)
    )
  
  ## over time (last vals) for each management scenario ------------------------------
  #### this collects the data to create table versions of the graphs in the 
  #### following section as final value of model components over time
  #### as a result of linear regression
  #### through the end of the future period in baseline climate
  
  tab_kbs_expl_components_time_apsim <- rbind(kbs_model_component_apsim_base_time[,c("Model","Climate_Scenario",
                                                                               "Scenario_Abbrev",
                                                                               "N2O_profile_last","NO3_0to60cm_last",
                                                                               "SoilT_20cm_last","SW_20cm_last",
                                                                               "WFPS_10cm_last",
                                                                               "CH4_last",
                                                                               "SoilT_2cm_last","SoilT_5cm_last",
                                                                               "SoilT_10cm_last",
                                                                               "WFPS_2cm_last","WFPS_5cm_last",
                                                                               "WFPS_10cm_last",
                                                                               "N2O_profile_change","NO3_0to60cm_change",
                                                                               "SoilT_20cm_change","SW_20cm_change",
                                                                               "WFPS_10cm_change",
                                                                               "CH4_change",
                                                                               "SoilT_2cm_change","SoilT_5cm_change",
                                                                               "SoilT_10cm_change",
                                                                               "WFPS_2cm_change","WFPS_5cm_change",
                                                                               "WFPS_10cm_change")],
                                        kbs_model_component_apsim_ukesm_time[,c("Model","Climate_Scenario",
                                                                                "Scenario_Abbrev",
                                                                                "N2O_profile_last","NO3_0to60cm_last",
                                                                                "SoilT_20cm_last","SW_20cm_last",
                                                                                "WFPS_10cm_last",
                                                                                "CH4_last",
                                                                                "SoilT_2cm_last","SoilT_5cm_last",
                                                                                "SoilT_10cm_last",
                                                                                "WFPS_2cm_last","WFPS_5cm_last",
                                                                                "WFPS_10cm_last",
                                                                                "N2O_profile_change","NO3_0to60cm_change",
                                                                                "SoilT_20cm_change","SW_20cm_change",
                                                                                "WFPS_10cm_change",
                                                                                "CH4_change",
                                                                                "SoilT_2cm_change","SoilT_5cm_change",
                                                                                "SoilT_10cm_change",
                                                                                "WFPS_2cm_change","WFPS_5cm_change",
                                                                                "WFPS_10cm_change")]
                                        ) %>%
    pivot_longer(names_to="component",
                 values_to="values",
                 cols=-c(Model,Climate_Scenario,Scenario_Abbrev)) %>%
    arrange(Model,Climate_Scenario,Scenario_Abbrev) %>%
    pivot_wider(names_from="Scenario_Abbrev",
                values_from="values",
                id_cols=c(Model,Climate_Scenario,component)) %>%
  mutate(component_order = factor(component,
                                  levels=component_order_apsim)) %>% 
    arrange(Model,Climate_Scenario,component_order)
  
  tab_kbs_expl_components_time_daycent <- rbind(kbs_model_component_daycent_base_time[,c("Model","Climate_Scenario",
                                                                                 "Scenario_Abbrev",
                                                                                 "N2O_profile_last","NO3_0to60cm_last",
                                                                                 "SoilT_20cm_last","SW_20cm_last",
                                                                                 "WFPS_10cm_last",
                                                                                 "CH4_last",
                                                                                 "SoilT_2cm_last","SoilT_5cm_last",
                                                                                 "SoilT_10cm_last",
                                                                                 "WFPS_2cm_last","WFPS_5cm_last",
                                                                                 "WFPS_10cm_last",
                                                                                 "N2O_profile_change","NO3_0to60cm_change",
                                                                                 "SoilT_20cm_change","SW_20cm_change",
                                                                                 "WFPS_10cm_change",
                                                                                 "CH4_change",
                                                                                 "SoilT_2cm_change","SoilT_5cm_change",
                                                                                 "SoilT_10cm_change",
                                                                                 "WFPS_2cm_change","WFPS_5cm_change",
                                                                                 "WFPS_10cm_change")],
                                        kbs_model_component_daycent_ukesm_time[,c("Model","Climate_Scenario",
                                                                                  "Scenario_Abbrev",
                                                                                  "N2O_profile_last","NO3_0to60cm_last",
                                                                                  "SoilT_20cm_last","SW_20cm_last",
                                                                                  "WFPS_10cm_last",
                                                                                  "CH4_last",
                                                                                  "SoilT_2cm_last","SoilT_5cm_last",
                                                                                  "SoilT_10cm_last",
                                                                                  "WFPS_2cm_last","WFPS_5cm_last",
                                                                                  "WFPS_10cm_last",
                                                                                  "N2O_profile_change","NO3_0to60cm_change",
                                                                                  "SoilT_20cm_change","SW_20cm_change",
                                                                                  "WFPS_10cm_change",
                                                                                  "CH4_change",
                                                                                  "SoilT_2cm_change","SoilT_5cm_change",
                                                                                  "SoilT_10cm_change",
                                                                                  "WFPS_2cm_change","WFPS_5cm_change",
                                                                                  "WFPS_10cm_change")]
  ) %>%
    pivot_longer(names_to="component",
                 values_to="values",
                 cols=-c(Model,Climate_Scenario,Scenario_Abbrev)) %>%
    arrange(Model,Climate_Scenario,Scenario_Abbrev) %>%
    pivot_wider(names_from="Scenario_Abbrev",
                values_from="values",
                id_cols=c(Model,Climate_Scenario,component)) %>%
  mutate(component_order = factor(component,
                                  levels=component_order_apsim)) %>% 
    arrange(Model,Climate_Scenario,component_order)

  
  tab_lrf_expl_components_time_apsim <- rbind(lrf_model_component_apsim_base_time[,c("Model","Climate_Scenario",
                                                                               "Scenario_Abbrev",
                                                                               "N2O_0to60cm_last","NO3_0to60cm_last",
                                                                               "SoilT_15cm_last","SW_15cm_last",
                                                                               "N2O_profile_last","NO3_0to60cm_last",
                                                                               "WFPS_10cm_last",
                                                                               "CH4_last",
                                                                               "SoilT_2cm_last","SoilT_5cm_last",
                                                                               "SoilT_10cm_last",
                                                                               "WFPS_2cm_last","WFPS_5cm_last",
                                                                               "WFPS_10cm_last",
                                                                               "N2O_0to60cm_change","NO3_0to60cm_change",
                                                                               "SoilT_15cm_change","SW_15cm_change",
                                                                               "N2O_profile_change","NO3_0to60cm_change",
                                                                               "WFPS_10cm_change",
                                                                               "CH4_change",
                                                                               "SoilT_2cm_change","SoilT_5cm_change",
                                                                               "SoilT_10cm_change",
                                                                               "WFPS_2cm_change","WFPS_5cm_change",
                                                                               "WFPS_10cm_change")],
                                        lrf_model_component_apsim_ukesm_time[,c("Model","Climate_Scenario",
                                                                                "Scenario_Abbrev",
                                                                                "N2O_0to60cm_last","NO3_0to60cm_last",
                                                                                "SoilT_15cm_last","SW_15cm_last",
                                                                                "N2O_profile_last","NO3_0to60cm_last",
                                                                                "WFPS_10cm_last",
                                                                                "CH4_last",
                                                                                "SoilT_2cm_last","SoilT_5cm_last",
                                                                                "SoilT_10cm_last",
                                                                                "WFPS_2cm_last","WFPS_5cm_last",
                                                                                "WFPS_10cm_last",
                                                                                "N2O_0to60cm_change","NO3_0to60cm_change",
                                                                                "SoilT_15cm_change","SW_15cm_change",
                                                                                "N2O_profile_change","NO3_0to60cm_change",
                                                                                "WFPS_10cm_change",
                                                                                "CH4_change",
                                                                                "SoilT_2cm_change","SoilT_5cm_change",
                                                                                "SoilT_10cm_change",
                                                                                "WFPS_2cm_change","WFPS_5cm_change",
                                                                                "WFPS_10cm_change")]
                                        ) %>%
    pivot_longer(names_to="component",
                 values_to="values",
                 cols=-c(Model,Climate_Scenario,Scenario_Abbrev)) %>%
    arrange(Model,Climate_Scenario,Scenario_Abbrev) %>%
    pivot_wider(names_from="Scenario_Abbrev",
                values_from="values",
                id_cols=c(Model,Climate_Scenario,component)) %>%
    mutate(component_order = factor(component,
                                 levels=component_order_apsim)) %>% 
    arrange(Model,Climate_Scenario,component_order)
  
  
  tab_lrf_expl_components_time_daycent <- rbind(lrf_model_component_daycent_base_time[,c("Model","Climate_Scenario",
                                                                                 "Scenario_Abbrev",
                                                                                 "N2O_0to60cm_last","NO3_0to60cm_last",
                                                                                 "SoilT_15cm_last","SW_15cm_last",
                                                                                 "N2O_profile_last","NO3_0to60cm_last",
                                                                                 "WFPS_10cm_last",
                                                                                 "CH4_last",
                                                                                 "SoilT_2cm_last","SoilT_5cm_last",
                                                                                 "SoilT_10cm_last",
                                                                                 "WFPS_2cm_last","WFPS_5cm_last",
                                                                                 "WFPS_10cm_last",
                                                                                 "N2O_0to60cm_change","NO3_0to60cm_change",
                                                                                 "SoilT_15cm_change","SW_15cm_change",
                                                                                 "N2O_profile_change","NO3_0to60cm_change",
                                                                                 "WFPS_10cm_change",
                                                                                 "CH4_change",
                                                                                 "SoilT_2cm_change","SoilT_5cm_change",
                                                                                 "SoilT_10cm_change",
                                                                                 "WFPS_2cm_change","WFPS_5cm_change",
                                                                                 "WFPS_10cm_change")],
                                        lrf_model_component_daycent_ukesm_time[,c("Model","Climate_Scenario",
                                                                                  "Scenario_Abbrev",
                                                                                  "N2O_0to60cm_last","NO3_0to60cm_last",
                                                                                  "SoilT_15cm_last","SW_15cm_last",
                                                                                  "N2O_profile_last","NO3_0to60cm_last",
                                                                                  "WFPS_10cm_last",
                                                                                  "CH4_last",
                                                                                  "SoilT_2cm_last","SoilT_5cm_last",
                                                                                  "SoilT_10cm_last",
                                                                                  "WFPS_2cm_last","WFPS_5cm_last",
                                                                                  "WFPS_10cm_last",
                                                                                  "N2O_0to60cm_change","NO3_0to60cm_change",
                                                                                  "SoilT_15cm_change","SW_15cm_change",
                                                                                  "N2O_profile_change","NO3_0to60cm_change",
                                                                                  "WFPS_10cm_change",
                                                                                  "CH4_change",
                                                                                  "SoilT_2cm_change","SoilT_5cm_change",
                                                                                  "SoilT_10cm_change",
                                                                                  "WFPS_2cm_change","WFPS_5cm_change",
                                                                                  "WFPS_10cm_change")]
  ) %>%
    pivot_longer(names_to="component",
                 values_to="values",
                 cols=-c(Model,Climate_Scenario,Scenario_Abbrev)) %>%
    pivot_wider(names_from="Scenario_Abbrev",
                values_from="values",
                id_cols=c(Model,Climate_Scenario,component)) %>%
  mutate(component_order = factor(component,
                                  levels=component_order_apsim)) %>% 
    arrange(Model,Climate_Scenario,component_order)

    ### write data frames
  write.csv(tab_kbs_expl_components_time_apsim, 
            file=paste0(these_results_folder,"tab_kbs_expl_components_time_apsim.csv"),
            row.names=FALSE)
  write.csv(tab_kbs_expl_components_time_daycent, 
            file=paste0(these_results_folder,"tab_kbs_expl_components_time_daycent.csv"),
            row.names=FALSE)
  write.csv(tab_lrf_expl_components_time_apsim, 
            file=paste0(these_results_folder,"tab_lrf_expl_components_time_apsim.csv"),
            row.names=FALSE)
  write.csv(tab_lrf_expl_components_time_daycent, 
            file=paste0(these_results_folder,"tab_lrf_expl_components_time_daycent.csv"),
            row.names=FALSE)
  
  
  
  
  # ### rearrange dfs for graphing -------------------------
  # 
  # kbs_model_component_apsim_base_time_piv <- pivot_longer(kbs_model_component_apsim_base_time,
  #                                                         c(-Model,-Climate_Scenario,-Mgmt_Scenario,
  #                                                           -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
  #                                                         names_to="source",values_to="vals")
  # 
  # kbs_model_component_daycent_base_time_piv <- pivot_longer(kbs_model_component_daycent_base_time,
  #                                                           c(-Model,-Climate_Scenario,-Mgmt_Scenario,
  #                                                             -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
  #                                                           names_to="source",values_to="vals")
  # 
  # kbs_model_component_apsim_ukesm_time_piv <- pivot_longer(kbs_model_component_apsim_ukesm_time,
  #                                                          c(-Model,-Climate_Scenario,-Mgmt_Scenario,
  #                                                            -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
  #                                                          names_to="source",values_to="vals")
  # 
  # kbs_model_component_daycent_ukesm_time_piv <- pivot_longer(kbs_model_component_daycent_ukesm_time,
  #                                                            c(-Model,-Climate_Scenario,-Mgmt_Scenario,
  #                                                              -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
  #                                                            names_to="source",values_to="vals")
  # 
  # lrf_model_component_apsim_base_time_piv <- pivot_longer(lrf_model_component_apsim_base_time,
  #                                                         c(-Model,-Climate_Scenario,-Mgmt_Scenario,
  #                                                           -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
  #                                                         names_to="source",values_to="vals")
  # 
  # lrf_model_component_daycent_base_time_piv <- pivot_longer(lrf_model_component_daycent_base_time,
  #                                                           c(-Model,-Climate_Scenario,-Mgmt_Scenario,
  #                                                             -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
  #                                                           names_to="source",values_to="vals")
  # 
  # lrf_model_component_apsim_ukesm_time_piv <- pivot_longer(lrf_model_component_apsim_ukesm_time,
  #                                                          c(-Model,-Climate_Scenario,-Mgmt_Scenario,
  #                                                            -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
  #                                                          names_to="source",values_to="vals")
  # 
  # lrf_model_component_daycent_ukesm_time_piv <- pivot_longer(lrf_model_component_daycent_ukesm_time,
  #                                                            c(-Model,-Climate_Scenario,-Mgmt_Scenario,
  #                                                              -Scenario_Name,-Scenario_Abbrev,-site_name,-pub_climate_scenario),
  #                                                            names_to="source",values_to="vals")
  # 
  # ## by all mgmt scenarios ------------------------------------------
  # 
  # ## Calculate percent changes for each model component by model as compared
  # ## separately to the baseline management scenarios. 
  # 
  # ### KBS -----------------
  # 
  # ## This will provide % change by model and management scenario, compared to the
  # ## baseline management scenario, for each climate scenario separately
  # 
  # #### NOTE: Warnings produced by this management grouping are due to an uneven number of scenarios
  # ####       between APSIM and Daycent (biochar). The aggregate function produces unequal
  # ####       object lengths when grouping APSIM vs. Daycent because of this, however, the 
  # ####       resulting calculations are still correct.
  # 
  # 
  # kbs_model_component_apsim_base_bymgmt <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Climate_Scenario==1 &
  #                                                                                            kbs_model_components$Model=="APSIM",], 
  #                                                                     !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                        Scenario_Name,Scenario_Abbrev,site_name,pub_climate_scenario)))
  #                                                    ~ Model+Scenario_Abbrev+site_name+pub_climate_scenario, 
  #                                                    data=kbs_model_components[kbs_model_components$Climate_Scenario==1 &
  #                                                                                kbs_model_components$Model=="APSIM",], 
  #                                                    FUN=mean,
  #                                                    na.action=na.pass) %>%
  #   mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="CR"])/SW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Scenario_Abbrev=="CR"])/SW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="CR"])/SW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="CR"])/DW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Scenario_Abbrev=="CR"])/DW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="CR"])/DW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="CR"])/DW_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Scenario_Abbrev=="CR"])/SW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Scenario_Abbrev=="CR"])/DW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="CR"])/SoilT_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Scenario_Abbrev=="CR"])/SoilT_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="CR"])/SoilT_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Scenario_Abbrev=="CR"])/SoilT_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="CR"])/NO3_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Scenario_Abbrev=="CR"])/NO3_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="CR"])/NO3_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="CR"])/NO3_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Scenario_Abbrev=="CR"])/N2O_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Scenario_Abbrev=="CR"])/N2O_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="CR"])/N2O_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="CR"])/N2O_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="CR"])/N2O_profile_change[Scenario_Abbrev=="CR"]*100,1),
  #          BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Scenario_Abbrev=="CR"])/BC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Scenario_Abbrev=="CR"])/BN_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Scenario_Abbrev=="CR"])/HC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Scenario_Abbrev=="CR"])/HN_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Scenario_Abbrev=="CR"])/CinB_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Scenario_Abbrev=="CR"])/CinH_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Scenario_Abbrev=="CR"])/CinBtoH_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Scenario_Abbrev=="CR"])/SOC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="CR"])/DW_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="CR"])/DW_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="CR"])/DW_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="CR"])/WFPS_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="CR"])/WFPS_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="CR"])/WFPS_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="CR"])/WFPS_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Scenario_Abbrev=="CR"])/WFPS_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="CR"])/WFPS_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="CR"])/SoilT_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="CR"])/SoilT_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="CR"])/SoilT_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="CR"])/SoilT_15cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="CR"])/NO3_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="CR"])/NO3_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Scenario_Abbrev=="CR"])/NO3_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="CR"])/CH4_change[Scenario_Abbrev=="CR"]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="CR"])/CI_change[Scenario_Abbrev=="CR"]*100,1)
  #   )
  # 
  # kbs_model_component_daycent_base_bymgmt <- aggregate(as.matrix(select(kbs_model_components[kbs_model_components$Climate_Scenario==1 &
  #                                                                                              kbs_model_components$Model=="Daycent",], 
  #                                                                       !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                          Scenario_Name,Scenario_Abbrev,site_name,pub_climate_scenario)))
  #                                                      ~ Model+Scenario_Abbrev+site_name+pub_climate_scenario, 
  #                                                      data=kbs_model_components[kbs_model_components$Climate_Scenario==1 &
  #                                                                                  kbs_model_components$Model=="Daycent",], 
  #                                                      FUN=mean,
  #                                                      na.action=na.pass) %>%
  #   mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Scenario_Abbrev=="CR"])/SW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Scenario_Abbrev=="CR"])/SW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="CR"])/SW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="CR"])/DW_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Scenario_Abbrev=="CR"])/DW_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="CR"])/DW_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="CR"])/DW_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Scenario_Abbrev=="CR"])/SW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Scenario_Abbrev=="CR"])/DW_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="CR"])/SoilT_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Scenario_Abbrev=="CR"])/SoilT_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="CR"])/SoilT_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Scenario_Abbrev=="CR"])/SoilT_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="CR"])/NO3_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Scenario_Abbrev=="CR"])/NO3_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="CR"])/NO3_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="CR"])/NO3_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Scenario_Abbrev=="CR"])/N2O_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Scenario_Abbrev=="CR"])/N2O_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="CR"])/N2O_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="CR"])/N2O_0to60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="CR"])/N2O_profile_change[Scenario_Abbrev=="CR"]*100,1),
  #          BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Scenario_Abbrev=="CR"])/BC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Scenario_Abbrev=="CR"])/BN_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Scenario_Abbrev=="CR"])/HC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Scenario_Abbrev=="CR"])/HN_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Scenario_Abbrev=="CR"])/CinB_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Scenario_Abbrev=="CR"])/CinH_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Scenario_Abbrev=="CR"])/CinBtoH_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Scenario_Abbrev=="CR"])/SOC_25cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="CR"])/DW_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="CR"])/DW_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="CR"])/DW_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="CR"])/WFPS_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="CR"])/WFPS_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="CR"])/WFPS_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="CR"])/WFPS_20cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Scenario_Abbrev=="CR"])/WFPS_40cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="CR"])/WFPS_60cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="CR"])/SoilT_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="CR"])/SoilT_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="CR"])/SoilT_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="CR"])/SoilT_15cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="CR"])/NO3_2cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="CR"])/NO3_5cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Scenario_Abbrev=="CR"])/NO3_10cm_change[Scenario_Abbrev=="CR"]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="CR"])/CH4_change[Scenario_Abbrev=="CR"]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="CR"])/CI_change[Scenario_Abbrev=="CR"]*100,1)
  #   )
  # 
  # 
  # 
  # ## This will provide % change by model and climate scenario, compared to the
  # ## baseline climate scenario, considering all management scenarios together
  # kbs_model_component_means_byclimate <- aggregate(as.matrix(select(kbs_model_components, 
  #                                                                   !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                      Scenario_Name,Scenario_Abbrev,site_name)))
  #                                                  ~ Model+Climate_Scenario+site_name, 
  #                                                  data=kbs_model_components, 
  #                                                  FUN=mean,
  #                                                  na.action=na.pass) %>%
  #   mutate(SW_20cm_pctchg=round((SW_20cm_change-SW_20cm_change[Climate_Scenario==1])/SW_20cm_change[Climate_Scenario==1]*100,1),
  #          SW_40cm_pctchg=round((SW_40cm_change-SW_40cm_change[Climate_Scenario==1])/SW_40cm_change[Climate_Scenario==1]*100,1),
  #          SW_60cm_pctchg=round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_40cm_pctchg = round((DW_40cm_change-DW_40cm_change[Climate_Scenario==1])/DW_40cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_25cm_pctchg = round((SW_25cm_change-SW_25cm_change[Climate_Scenario==1])/SW_25cm_change[Climate_Scenario==1]*100,1),
  #          DW_25cm_pctchg = round((DW_25cm_change-DW_25cm_change[Climate_Scenario==1])/DW_25cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg=round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_40cm_pctchg = round((SoilT_40cm_change-SoilT_40cm_change[Climate_Scenario==1])/SoilT_40cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_25cm_pctchg = round((SoilT_25cm_change-SoilT_25cm_change[Climate_Scenario==1])/SoilT_25cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg=round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_40cm_pctchg = round((NO3_40cm_change-NO3_40cm_change[Climate_Scenario==1])/NO3_40cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_20cm_pctchg=round((N2O_20cm_change-N2O_20cm_change[Climate_Scenario==1])/N2O_20cm_change[Climate_Scenario==1]*100,1),
  #          N2O_40cm_pctchg = round((N2O_40cm_change-N2O_40cm_change[Climate_Scenario==1])/N2O_40cm_change[Climate_Scenario==1]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          BC_25cm_pctchg = round((BC_25cm_change-BC_25cm_change[Climate_Scenario==1])/BC_25cm_change[Climate_Scenario==1]*100,1),
  #          BN_25cm_pctchg = round((BN_25cm_change-BN_25cm_change[Climate_Scenario==1])/BN_25cm_change[Climate_Scenario==1]*100,1),
  #          HC_25cm_pctchg = round((HC_25cm_change-HC_25cm_change[Climate_Scenario==1])/HC_25cm_change[Climate_Scenario==1]*100,1),
  #          HN_25cm_pctchg = round((HN_25cm_change-HN_25cm_change[Climate_Scenario==1])/HN_25cm_change[Climate_Scenario==1]*100,1),
  #          CinB_25cm_pctchg = round((CinB_25cm_change-CinB_25cm_change[Climate_Scenario==1])/CinB_25cm_change[Climate_Scenario==1]*100,1),
  #          CinH_25cm_pctchg = round((CinH_25cm_change-CinH_25cm_change[Climate_Scenario==1])/CinH_25cm_change[Climate_Scenario==1]*100,1),
  #          CinBtoH_25cm_pctchg = round((CinBtoH_25cm_change-CinBtoH_25cm_change[Climate_Scenario==1])/CinBtoH_25cm_change[Climate_Scenario==1]*100,1),
  #          SOC_25cm_pctchg = round((SOC_25cm_change-SOC_25cm_change[Climate_Scenario==1])/SOC_25cm_change[Climate_Scenario==1]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_40cm_pctchg = round((WFPS_40cm_change-WFPS_40cm_change[Climate_Scenario==1])/WFPS_40cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  # 
  # 
  # 
  # ### LRF -----------------
  # 
  # ## This will provide % change by model and management scenario, compared to the
  # ## baseline management scenario, considering all climate scenarios together
  # 
  # #### NOTE: Warnings produced by the management grouping are due to an uneven number of scenarios
  # ####       between APSIM and Daycent (biochar). The aggregate function produces unequal
  # ####       object lengths when grouping APSIM vs. Daycent because of this, however, the 
  # ####       resulting calculations are still correct.
  # lrf_model_component_apsim_base_bymgmt <- aggregate(as.matrix(select(lrf_model_components[lrf_model_components$Climate_Scenario==1 &
  #                                                                                            lrf_model_components$Model=="APSIM",], 
  #                                                                     !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                        Scenario_Name,Scenario_Abbrev,site_name,pub_climate_scenario)))
  #                                                    ~ Model+Scenario_Abbrev+site_name+pub_climate_scenario, 
  #                                                    data=lrf_model_components[lrf_model_components$Climate_Scenario==1 &
  #                                                                                lrf_model_components$Model=="APSIM",], 
  #                                                    FUN=mean,
  #                                                    na.action=na.pass) %>%
  #   mutate(SW_5cm_pctchg = round((SW_5cm_change-SW_5cm_change[Scenario_Abbrev=="RR00-CR"])/SW_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_15cm_pctchg = round((SW_15cm_change-SW_15cm_change[Scenario_Abbrev=="RR00-CR"])/SW_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_35cm_pctchg = round((SW_35cm_change-SW_35cm_change[Scenario_Abbrev=="RR00-CR"])/SW_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_60cm_pctchg = round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="RR00-CR"])/SW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="RR00-CR"])/DW_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_15cm_pctchg = round((DW_15cm_change-DW_15cm_change[Scenario_Abbrev=="RR00-CR"])/DW_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_35cm_pctchg = round((DW_35cm_change-DW_35cm_change[Scenario_Abbrev=="RR00-CR"])/DW_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_10cm_pctchg = round((SW_10cm_change-SW_10cm_change[Scenario_Abbrev=="RR00-CR"])/SW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="RR00-CR"])/DW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_35cm_pctchg = round((SoilT_35cm_change-SoilT_35cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_15cm_pctchg = round((NO3_15cm_change-NO3_15cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_35cm_pctchg = round((NO3_35cm_change-NO3_35cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_5cm_pctchg = round((N2O_5cm_change-N2O_5cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_15cm_pctchg = round((N2O_15cm_change-N2O_15cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_35cm_pctchg = round((N2O_35cm_change-N2O_35cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="RR00-CR"])/N2O_profile_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          BC_10cm_pctchg = round((BC_10cm_change-BC_10cm_change[Scenario_Abbrev=="RR00-CR"])/BC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          BN_10cm_pctchg = round((BN_10cm_change-BN_10cm_change[Scenario_Abbrev=="RR00-CR"])/BN_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          HC_10cm_pctchg = round((HC_10cm_change-HC_10cm_change[Scenario_Abbrev=="RR00-CR"])/HC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          HN_10cm_pctchg = round((HN_10cm_change-HN_10cm_change[Scenario_Abbrev=="RR00-CR"])/HN_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinB_10cm_pctchg = round((CinB_10cm_change-CinB_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinB_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinH_10cm_pctchg = round((CinH_10cm_change-CinH_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinH_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinBtoH_10cm_pctchg = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinBtoH_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SOC_10cm_pctchg = round((SOC_10cm_change-SOC_10cm_change[Scenario_Abbrev=="RR00-CR"])/SOC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="RR00-CR"])/DW_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="RR00-CR"])/DW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="RR00-CR"])/DW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_30cm_pctchg = round((DW_30cm_change-DW_30cm_change[Scenario_Abbrev=="RR00-CR"])/DW_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_45cm_pctchg = round((DW_45cm_change-DW_45cm_change[Scenario_Abbrev=="RR00-CR"])/DW_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_30cm_pctchg = round((WFPS_30cm_change-WFPS_30cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_45cm_pctchg = round((WFPS_45cm_change-WFPS_45cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_30cm_pctchg = round((SoilT_30cm_change-SoilT_30cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_45cm_pctchg = round((SoilT_45cm_change-SoilT_45cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_30cm_pctchg = round((NO3_30cm_change-NO3_30cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_45cm_pctchg = round((NO3_45cm_change-NO3_45cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="RR00-CR"])/CH4_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="RR00-CR"])/CI_change[Scenario_Abbrev=="RR00-CR"]*100,1)
  #   )
  # 
  # 
  # lrf_model_component_daycent_base_bymgmt <- aggregate(as.matrix(select(lrf_model_components[lrf_model_components$Climate_Scenario==1 &
  #                                                                                              lrf_model_components$Model=="Daycent",], 
  #                                                                       !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                          Scenario_Name,Scenario_Abbrev,site_name,pub_climate_scenario)))
  #                                                      ~ Model+Scenario_Abbrev+site_name+pub_climate_scenario, 
  #                                                      data=lrf_model_components[lrf_model_components$Climate_Scenario==1 &
  #                                                                                  lrf_model_components$Model=="Daycent",], 
  #                                                      FUN=mean,
  #                                                      na.action=na.pass) %>%
  #   mutate(SW_5cm_pctchg = round((SW_5cm_change-SW_5cm_change[Scenario_Abbrev=="RR00-CR"])/SW_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_15cm_pctchg = round((SW_15cm_change-SW_15cm_change[Scenario_Abbrev=="RR00-CR"])/SW_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_35cm_pctchg = round((SW_35cm_change-SW_35cm_change[Scenario_Abbrev=="RR00-CR"])/SW_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_60cm_pctchg = round((SW_60cm_change-SW_60cm_change[Scenario_Abbrev=="RR00-CR"])/SW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Scenario_Abbrev=="RR00-CR"])/DW_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_15cm_pctchg = round((DW_15cm_change-DW_15cm_change[Scenario_Abbrev=="RR00-CR"])/DW_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_35cm_pctchg = round((DW_35cm_change-DW_35cm_change[Scenario_Abbrev=="RR00-CR"])/DW_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/DW_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SW_10cm_pctchg = round((SW_10cm_change-SW_10cm_change[Scenario_Abbrev=="RR00-CR"])/SW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="RR00-CR"])/DW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_35cm_pctchg = round((SoilT_35cm_change-SoilT_35cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_15cm_pctchg = round((NO3_15cm_change-NO3_15cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_35cm_pctchg = round((NO3_35cm_change-NO3_35cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_5cm_pctchg = round((N2O_5cm_change-N2O_5cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_15cm_pctchg = round((N2O_15cm_change-N2O_15cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_15cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_35cm_pctchg = round((N2O_35cm_change-N2O_35cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_35cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"])/N2O_0to60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Scenario_Abbrev=="RR00-CR"])/N2O_profile_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          BC_10cm_pctchg = round((BC_10cm_change-BC_10cm_change[Scenario_Abbrev=="RR00-CR"])/BC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          BN_10cm_pctchg = round((BN_10cm_change-BN_10cm_change[Scenario_Abbrev=="RR00-CR"])/BN_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          HC_10cm_pctchg = round((HC_10cm_change-HC_10cm_change[Scenario_Abbrev=="RR00-CR"])/HC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          HN_10cm_pctchg = round((HN_10cm_change-HN_10cm_change[Scenario_Abbrev=="RR00-CR"])/HN_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinB_10cm_pctchg = round((CinB_10cm_change-CinB_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinB_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinH_10cm_pctchg = round((CinH_10cm_change-CinH_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinH_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CinBtoH_10cm_pctchg = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Scenario_Abbrev=="RR00-CR"])/CinBtoH_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SOC_10cm_pctchg = round((SOC_10cm_change-SOC_10cm_change[Scenario_Abbrev=="RR00-CR"])/SOC_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Scenario_Abbrev=="RR00-CR"])/DW_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Scenario_Abbrev=="RR00-CR"])/DW_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Scenario_Abbrev=="RR00-CR"])/DW_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_30cm_pctchg = round((DW_30cm_change-DW_30cm_change[Scenario_Abbrev=="RR00-CR"])/DW_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          DW_45cm_pctchg = round((DW_45cm_change-DW_45cm_change[Scenario_Abbrev=="RR00-CR"])/DW_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_5cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_30cm_pctchg = round((WFPS_30cm_change-WFPS_30cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_45cm_pctchg = round((WFPS_45cm_change-WFPS_45cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Scenario_Abbrev=="RR00-CR"])/WFPS_60cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_30cm_pctchg = round((SoilT_30cm_change-SoilT_30cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          SoilT_45cm_pctchg = round((SoilT_45cm_change-SoilT_45cm_change[Scenario_Abbrev=="RR00-CR"])/SoilT_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_2cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_10cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_20cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_30cm_pctchg = round((NO3_30cm_change-NO3_30cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_30cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          NO3_45cm_pctchg = round((NO3_45cm_change-NO3_45cm_change[Scenario_Abbrev=="RR00-CR"])/NO3_45cm_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Scenario_Abbrev=="RR00-CR"])/CH4_change[Scenario_Abbrev=="RR00-CR"]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Scenario_Abbrev=="RR00-CR"])/CI_change[Scenario_Abbrev=="RR00-CR"]*100,1)
  #   )
  # 
  # 
  # ## This will provide % change by model and climate scenario, compared to the
  # ## baseline climate scenario, considering all management scenarios together
  # lrf_model_component_means_byclimate <- aggregate(as.matrix(select(lrf_model_components, 
  #                                                                   !c(Model,Climate_Scenario,Mgmt_Scenario,
  #                                                                      Scenario_Name,Scenario_Abbrev,site_name)))
  #                                                  ~ Model+Climate_Scenario+site_name, 
  #                                                  data=lrf_model_components, 
  #                                                  FUN=mean,
  #                                                  na.action=na.pass) %>%
  #   mutate(SW_5cm_pctchg = round((SW_5cm_change-SW_5cm_change[Climate_Scenario==1])/SW_5cm_change[Climate_Scenario==1]*100,1),
  #          SW_15cm_pctchg = round((SW_15cm_change-SW_15cm_change[Climate_Scenario==1])/SW_15cm_change[Climate_Scenario==1]*100,1),
  #          SW_35cm_pctchg = round((SW_35cm_change-SW_35cm_change[Climate_Scenario==1])/SW_35cm_change[Climate_Scenario==1]*100,1),
  #          SW_60cm_pctchg = round((SW_60cm_change-SW_60cm_change[Climate_Scenario==1])/SW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_5cm_pctchg = round((DW_5cm_change-DW_5cm_change[Climate_Scenario==1])/DW_5cm_change[Climate_Scenario==1]*100,1),
  #          DW_15cm_pctchg = round((DW_15cm_change-DW_15cm_change[Climate_Scenario==1])/DW_15cm_change[Climate_Scenario==1]*100,1),
  #          DW_35cm_pctchg = round((DW_35cm_change-DW_35cm_change[Climate_Scenario==1])/DW_35cm_change[Climate_Scenario==1]*100,1),
  #          DW_60cm_pctchg = round((DW_60cm_change-DW_60cm_change[Climate_Scenario==1])/DW_60cm_change[Climate_Scenario==1]*100,1),
  #          DW_0to60cm_pctchg = round((DW_0to60cm_change-DW_0to60cm_change[Climate_Scenario==1])/DW_0to60cm_change[Climate_Scenario==1]*100,1),
  #          SW_10cm_pctchg = round((SW_10cm_change-SW_10cm_change[Climate_Scenario==1])/SW_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_5cm_pctchg = round((SoilT_5cm_change-SoilT_5cm_change[Climate_Scenario==1])/SoilT_5cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_15cm_pctchg = round((SoilT_15cm_change-SoilT_15cm_change[Climate_Scenario==1])/SoilT_15cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_35cm_pctchg = round((SoilT_35cm_change-SoilT_35cm_change[Climate_Scenario==1])/SoilT_35cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_60cm_pctchg = round((SoilT_60cm_change-SoilT_60cm_change[Climate_Scenario==1])/SoilT_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_10cm_pctchg = round((SoilT_10cm_change-SoilT_10cm_change[Climate_Scenario==1])/SoilT_10cm_change[Climate_Scenario==1]*100,1),
  #          NO3_5cm_pctchg = round((NO3_5cm_change-NO3_5cm_change[Climate_Scenario==1])/NO3_5cm_change[Climate_Scenario==1]*100,1),
  #          NO3_15cm_pctchg = round((NO3_15cm_change-NO3_15cm_change[Climate_Scenario==1])/NO3_15cm_change[Climate_Scenario==1]*100,1),
  #          NO3_35cm_pctchg = round((NO3_35cm_change-NO3_35cm_change[Climate_Scenario==1])/NO3_35cm_change[Climate_Scenario==1]*100,1),
  #          NO3_60cm_pctchg = round((NO3_60cm_change-NO3_60cm_change[Climate_Scenario==1])/NO3_60cm_change[Climate_Scenario==1]*100,1),
  #          NO3_0to60cm_pctchg = round((NO3_0to60cm_change-NO3_0to60cm_change[Climate_Scenario==1])/NO3_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_5cm_pctchg = round((N2O_5cm_change-N2O_5cm_change[Climate_Scenario==1])/N2O_5cm_change[Climate_Scenario==1]*100,1),
  #          N2O_15cm_pctchg = round((N2O_15cm_change-N2O_15cm_change[Climate_Scenario==1])/N2O_15cm_change[Climate_Scenario==1]*100,1),
  #          N2O_35cm_pctchg = round((N2O_35cm_change-N2O_35cm_change[Climate_Scenario==1])/N2O_35cm_change[Climate_Scenario==1]*100,1),
  #          N2O_60cm_pctchg = round((N2O_60cm_change-N2O_60cm_change[Climate_Scenario==1])/N2O_60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_0to60cm_pctchg = round((N2O_0to60cm_change-N2O_0to60cm_change[Climate_Scenario==1])/N2O_0to60cm_change[Climate_Scenario==1]*100,1),
  #          N2O_profile_pctchg = round((N2O_profile_change-N2O_profile_change[Climate_Scenario==1])/N2O_profile_change[Climate_Scenario==1]*100,1),
  #          BC_10cm_pctchg = round((BC_10cm_change-BC_10cm_change[Climate_Scenario==1])/BC_10cm_change[Climate_Scenario==1]*100,1),
  #          BN_10cm_pctchg = round((BN_10cm_change-BN_10cm_change[Climate_Scenario==1])/BN_10cm_change[Climate_Scenario==1]*100,1),
  #          HC_10cm_pctchg = round((HC_10cm_change-HC_10cm_change[Climate_Scenario==1])/HC_10cm_change[Climate_Scenario==1]*100,1),
  #          HN_10cm_pctchg = round((HN_10cm_change-HN_10cm_change[Climate_Scenario==1])/HN_10cm_change[Climate_Scenario==1]*100,1),
  #          CinB_10cm_pctchg = round((CinB_10cm_change-CinB_10cm_change[Climate_Scenario==1])/CinB_10cm_change[Climate_Scenario==1]*100,1),
  #          CinH_10cm_pctchg = round((CinH_10cm_change-CinH_10cm_change[Climate_Scenario==1])/CinH_10cm_change[Climate_Scenario==1]*100,1),
  #          CinBtoH_10cm_pctchg = round((CinBtoH_10cm_change-CinBtoH_10cm_change[Climate_Scenario==1])/CinBtoH_10cm_change[Climate_Scenario==1]*100,1),
  #          SOC_10cm_pctchg = round((SOC_10cm_change-SOC_10cm_change[Climate_Scenario==1])/SOC_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_2cm_pctchg = round((DW_2cm_change-DW_2cm_change[Climate_Scenario==1])/DW_2cm_change[Climate_Scenario==1]*100,1),
  #          DW_10cm_pctchg = round((DW_10cm_change-DW_10cm_change[Climate_Scenario==1])/DW_10cm_change[Climate_Scenario==1]*100,1),
  #          DW_20cm_pctchg = round((DW_20cm_change-DW_20cm_change[Climate_Scenario==1])/DW_20cm_change[Climate_Scenario==1]*100,1),
  #          DW_30cm_pctchg = round((DW_30cm_change-DW_30cm_change[Climate_Scenario==1])/DW_30cm_change[Climate_Scenario==1]*100,1),
  #          DW_45cm_pctchg = round((DW_45cm_change-DW_45cm_change[Climate_Scenario==1])/DW_45cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_2cm_pctchg = round((WFPS_2cm_change-WFPS_2cm_change[Climate_Scenario==1])/WFPS_2cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_5cm_pctchg = round((WFPS_5cm_change-WFPS_5cm_change[Climate_Scenario==1])/WFPS_5cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_10cm_pctchg = round((WFPS_10cm_change-WFPS_10cm_change[Climate_Scenario==1])/WFPS_10cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_20cm_pctchg = round((WFPS_20cm_change-WFPS_20cm_change[Climate_Scenario==1])/WFPS_20cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_30cm_pctchg = round((WFPS_30cm_change-WFPS_30cm_change[Climate_Scenario==1])/WFPS_30cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_45cm_pctchg = round((WFPS_45cm_change-WFPS_45cm_change[Climate_Scenario==1])/WFPS_45cm_change[Climate_Scenario==1]*100,1),
  #          WFPS_60cm_pctchg = round((WFPS_60cm_change-WFPS_60cm_change[Climate_Scenario==1])/WFPS_60cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_2cm_pctchg = round((SoilT_2cm_change-SoilT_2cm_change[Climate_Scenario==1])/SoilT_2cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_20cm_pctchg = round((SoilT_20cm_change-SoilT_20cm_change[Climate_Scenario==1])/SoilT_20cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_30cm_pctchg = round((SoilT_30cm_change-SoilT_30cm_change[Climate_Scenario==1])/SoilT_30cm_change[Climate_Scenario==1]*100,1),
  #          SoilT_45cm_pctchg = round((SoilT_45cm_change-SoilT_45cm_change[Climate_Scenario==1])/SoilT_45cm_change[Climate_Scenario==1]*100,1),
  #          NO3_2cm_pctchg = round((NO3_2cm_change-NO3_2cm_change[Climate_Scenario==1])/NO3_2cm_change[Climate_Scenario==1]*100,1),
  #          NO3_10cm_pctchg = round((NO3_10cm_change-NO3_10cm_change[Climate_Scenario==1])/NO3_10cm_change[Climate_Scenario==1]*100,1),
  #          NO3_20cm_pctchg = round((NO3_20cm_change-NO3_20cm_change[Climate_Scenario==1])/NO3_20cm_change[Climate_Scenario==1]*100,1),
  #          NO3_30cm_pctchg = round((NO3_30cm_change-NO3_30cm_change[Climate_Scenario==1])/NO3_30cm_change[Climate_Scenario==1]*100,1),
  #          NO3_45cm_pctchg = round((NO3_45cm_change-NO3_45cm_change[Climate_Scenario==1])/NO3_45cm_change[Climate_Scenario==1]*100,1),
  #          CH4_pctchg = round((CH4_change-CH4_change[Climate_Scenario==1])/CH4_change[Climate_Scenario==1]*100,1),
  #          CI_pctchg = round((CI_change-CI_change[Climate_Scenario==1])/CI_change[Climate_Scenario==1]*100,1)
  #   )
  # 
  
  
}) # end suppressMessages

