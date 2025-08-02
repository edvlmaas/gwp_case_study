#######################################
# Script: p_Daycent_Cinput2_LRF.R
# Author: Ellen Maas
# Date: Nov 6, 2022
# Description: This script takes C output from Daycent and creates
# the input files for use by RothC and Millennial.
########################################

suppressMessages({
  
  print(paste0("Starting p_Daycent_Cinput2_",site_name,".R"))
  
  library(datetime)
  
  
  # the .lis file provides the annual soil c input (clitad(2))
  lis_output <- read.table(paste0(daycent_path,paste0("sched_fut_",scenario_name,".lis")),
                           #lis_output <- read.table(paste0(daycent_path,paste0("sched_exp_",scenario_name,".lis")),
                           col.names = c("time","somsc_gm2","somtc","somte_1",
                                         "crpval","cinput","somse_1","petann",
                                         "tminrl_1","minerl_1_1","minerl_2_1",
                                         "minerl_3_1","minerl_4_1","minerl_5_1",
                                         "minerl_6_1","minerl_7_1","minerl_8_1",
                                         "aglivc","bglivcj","bglivcm","cgrain",
                                         "crmvst","hi","clitad_1","clitad_2",
                                         "elitad_1_1","elitad_2_1"),
                           colClasses=c("numeric","numeric","numeric","numeric",
                                        "character","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric",
                                        "numeric","numeric","numeric"),
                           skip=45)
  lis_output <- lis_output[lis_output$time==land_conversion_year | lis_output$clitad_2!=0,]
  
  # the livec.out file gives cumulative daily above- and below-ground C input
  livec_output_base <- read.table(paste0(daycent_path,paste0("livec_base_",scenario_name,".out")),
                                  col.names = c("time","dayofyr","aglivc","bglivcj","bglivcm","rleavc",
                                                "frootcj","frootcm","fbrchc","rlwodc","crootc","frnutc"),
                                  colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                               "numeric","numeric","numeric","numeric","numeric",
                                               "numeric","numeric"),
                                  skip=1)
  livec_output_exp <- read.table(paste0(daycent_path,paste0("livec_exp_",scenario_name,".out")),
                                 col.names = c("time","dayofyr","aglivc","bglivcj","bglivcm","rleavc",
                                               "frootcj","frootcm","fbrchc","rlwodc","crootc","frnutc"),
                                 colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                              "numeric","numeric","numeric","numeric","numeric",
                                              "numeric","numeric"),
                                 skip=1)
  livec_output_fut <- read.table(paste0(daycent_path,paste0("livec_fut_",scenario_name,".out")),
                                 col.names = c("time","dayofyr","aglivc","bglivcj","bglivcm","rleavc",
                                               "frootcj","frootcm","fbrchc","rlwodc","crootc","frnutc"),
                                 colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                              "numeric","numeric","numeric","numeric","numeric",
                                              "numeric","numeric"),
                                 skip=1)
  
  
  livec_output <- rbind(livec_output_base,livec_output_exp,livec_output_fut) %>%
    mutate(year=floor(time),
           date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
           tot_plt_growth=aglivc+bglivcj+bglivcm,
           daily_NPP=ifelse(tot_plt_growth>lag(tot_plt_growth,default=1000000),
                            tot_plt_growth-lag(tot_plt_growth,default=1000000),
                            0))
  
  # fail-safe: limit to future year range selected for this test/run
  # can hit an error if 2050 testing is done when 2100 last generated
  # the .lis files
  
  livec_output <- livec_output[livec_output$year<=max_fut_period_year,]
  
  # Plant/harvest dates - base ----------------------------------------------
  
  
  # collect plant/harvest dates
  all_years <- data.frame(year=land_conversion_year:max_fut_period_year)
  

  # Base period -------------------------------------------------------------

  
  ## assemble annual planting and harvest dates for crops - build list from schedule files
  planting_base <- data.frame(year=all_years[all_years$year >= land_conversion_year &
                                               all_years$year < experiment_start_year,"year"]) %>%
    mutate(observation_type="Planting",
           dayofyr=if_else(year >= land_conversion_year & year <= 1959,161,
                   if_else(year >= 1960 & year < experiment_start_year,140, 
                           0)),
           date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
           crop=if_else(year >= land_conversion_year & year <= 1959,"Sorghum",
                if_else(year >= 1960 & year < experiment_start_year ,"Cotton",
                #if_else(year == 2022, "Ryegrass", # ignore this for easier start-up
                        "Error"))#)
    )
  planting_cc_base <- data.frame(year=2002,
                                 observation_type="Planting",
                                 dayofyr=341) %>%
    mutate(date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
           crop="Ryegrass")
  


# Experimental period -----------------------------------------------------

  
  # add field ops data - reduce to unique rows
  ### NOTE: There MUST only be one Planting and one Harvest per crop (vs. separate for grain
  ###       and stover)
  field_ops_exp <- unique(full_ops_ext_adj[full_ops_ext_adj$observation_type %in% c("Planting") &
                                             full_ops_ext_adj$treatment==treatment,
                                           c("year","observation_type","date","crop")]) 


# Fill-in for end of exp to start of fut ----------------------------------

  
  # fill-in for end of experiment through end of experimental period
  if(mgmt_scenario_grp %in% c(3,8)) {
    ## assemble annual planting and harvest dates for crops - build list from schedule files
    ## cotton year 1
    planting_cottonyr1_fillin <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                       all_years$year <= end_exp_period_year &
                                                         (all_years$year %% 2) == 1,"year"]) %>% # odd years
      mutate(observation_type="Planting",
             dayofyr=133,
             date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
             crop="Cotton")

    ## ryegrass following cotton, except don't plant in 2021
    planting_cottoncc_fillin <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                         all_years$year <= end_exp_period_year-1,"year"]) %>%
      mutate(observation_type="Planting",
             dayofyr=347,
             date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
             crop="Ryegrass")

    ## cotton year 2
    planting_cottonyr2_fillin <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                             all_years$year <= end_exp_period_year &
                                                             (all_years$year %% 2) == 0,"year"]) %>% # even years
      mutate(observation_type="Planting",
             dayofyr=140,
             date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
             crop="Cotton")

    ## combine
    planting_fillin <- rbind(planting_cottonyr1_fillin,planting_cottoncc_fillin,
                             planting_cottonyr2_fillin)
    

  } else {
      ## assemble annual planting and harvest dates for crops - build list from schedule files
      ## cotton year 1
      planting_cottonyr1_fillin <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                               all_years$year <= end_exp_period_year &
                                                               (all_years$year %% 2) == 1,"year"]) %>% # odd years
        mutate(observation_type="Planting",
               dayofyr=133,
               date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
               crop="Cotton")
      
      ## cotton year 2
      planting_cottonyr2_fillin <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                               all_years$year <= end_exp_period_year &
                                                               (all_years$year %% 2) == 0,"year"]) %>% # even years
        mutate(observation_type="Planting",
               dayofyr=140,
               date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
               crop="Cotton")
      
      ## combine
      planting_fillin <- rbind(planting_cottonyr1_fillin,planting_cottonyr2_fillin)
  }
  
  
  # Future period -----------------------------------------------------
  
  ## ***** all scenarios *****
  ## set planting and harvest dates by crop
  ### cotton (odd years)
  planting_cotton_fut <- data.frame(year=all_years[all_years$year > end_exp_period_year &
                                                   (all_years$year %% 2) == 1,"year"]) %>%
    mutate(observation_type="Planting",
           dayofyr=140,
           date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
           crop="Cotton")

  ## sorghum (even years)
  planting_sorghum_fut <- data.frame(year=all_years[all_years$year > end_exp_period_year &
                                                      (all_years$year %% 2) == 0,"year"]) %>%
    mutate(observation_type="Planting",
           dayofyr=135,
           date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
           crop="Sorghum")

  ## ***** cover crop scenarios *****
  if(mgmt_scenario_grp %in% c(3,8)) {
    ## ryegrass following sorghum (planted even years, harvested odd)
  planting_cottoncc_fut <- data.frame(year=all_years[all_years$year > end_exp_period_year &
                                                       all_years$year < max_fut_period_year &
                                                     (all_years$year %% 2) == 0,"year"]) %>%
    mutate(observation_type="Planting",
           dayofyr=355,
           date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
           crop="Ryegrass")
  } # end if scenario group in 3,8

  
  # combine
  if(mgmt_scenario_grp!=3) {
    planting_fut <- rbind(planting_cotton_fut,planting_sorghum_fut)
    
  } else {
    planting_fut <- rbind(planting_cotton_fut,planting_cottoncc_fut,planting_sorghum_fut)
    
  }
  
  all_field_ops <- rbind(planting_base,planting_cc_base,
                         planting_fillin,
                         planting_fut) %>%
    select(-dayofyr) %>%
    rbind(field_ops_exp) 
  all_field_ops <- all_field_ops[order(all_field_ops$date),]
  
  # join livec with field ops
  livec_output <- merge(livec_output, all_field_ops, by=c("date","year"), all=TRUE)
  
  
  # calculate the % C each day during plant growth
  
  ## loop once per year
  for(i in min(livec_output$year):max(livec_output$year)) {
    # get total plant growth for the season (at end of year)
    livec_output[livec_output$year==i,"tot_NPP"] <- sum(livec_output[livec_output$year==i, "daily_NPP"])
    # calculate the fraction of each day's growth between planting-harvest of total (tot_NPP)
    # need to calculate the amount of each day's growth as change from the day before (because
    # it's cumulative); this is the plant growth curve we need to approximate soil C input
    # throughout the growing season
    livec_output[livec_output$year==i,"frct_NPP"] <- livec_output[livec_output$year==i,"daily_NPP"]/livec_output[livec_output$year==i,"tot_NPP"]
    # calculate the actual daily C input due to roots/exudates according to clitad(2)
    livec_output[livec_output$year==i,"clitad_2"] <- lis_output[lis_output$time==i,"clitad_2"]
    livec_output[livec_output$year==i,"daily_soilC_gm2"] <- round(livec_output[livec_output$year==i,"frct_NPP"] * 
                                                                                 lis_output[lis_output$time==i,"clitad_2"],6)
    # jump a record to start at the next planting row
    i <- i+1 
  }
  
  ## fill in NA with 0
  livec_output[is.na(livec_output$daily_soilC),"daily_soilC_gm2"] <- 0.00
  livec_output$daily_soilC_Mgha <- round(livec_output$daily_soilC_gm2/100,6)
  livec_output$month <- month(livec_output$date)
  
  ## fill in crop type from planting-planting
  for(i in which(c(1:nrow(all_field_ops[all_field_ops$year<max_fut_period_year-1,]))%%2==1)) {
    planting_date <- all_field_ops[i,"date"]
    next_planting_date <- ifelse(i<max(which(c(1:nrow(all_field_ops[all_field_ops$year<max_fut_period_year-1,]))%%2==1)),
                                 all_field_ops[i+2,"date"],
                                 max(all_field_ops$date))
    livec_output[livec_output$date>=planting_date & 
                   livec_output$date<next_planting_date,"crop"] <- all_field_ops[i,"crop"]
  }
  ### clean up the edges
  livec_output[livec_output$year==land_conversion_year,"crop"] <- "Sorghum"
  livec_output[livec_output$year == 2099 & is.na(livec_output$crop),"crop"] <- "Cotton"
  livec_output <- livec_output[livec_output$year < max_fut_period_year,]
  
  
  ########################################################
  
  write.table(livec_output[,c("date","year","dayofyr","month","aglivc","bglivcj","bglivcm","tot_plt_growth",
                              "daily_NPP","observation_type","crop","tot_NPP","frct_NPP","clitad_2",
                              "daily_soilC_gm2","daily_soilC_Mgha")],
              file=paste0(mgmt_path,"Daycent_Cinput_",clim_scenario_num,"_",mgmt_scenario_num,".csv"),
              col.names=c("date","year","dayofyr","month","aglivc","bglivcj","bglivcm","tot_plt_growth",
                          "daily_NPP","observation_type","crop","tot_NPP","frct_NPP","clitad_2",
                          "daily_soilC_gm2","daily_soilC_Mgha")
              ,row.names=F,
              sep=",")
  

# Clean up ----------------------------------------------------------------

rm(planting_base, planting_cc_base, planting_cotton_fut, planting_cottoncc_fillin,
   planting_cottoncc_fut, planting_cottonyr1_fillin, planting_cottonyr2_fillin,
   planting_fillin, planting_fut, planting_sorghum_fut, lis_output,
   livec_output, livec_output_base, livec_output_exp, livec_output_fut,
   planting_date, next_planting_date, field_ops_exp, all_years)
    
}) # end suppressMessages