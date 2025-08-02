#######################################
# File: 3_Create_management_input_files-setup_LRF.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Description: This procedure generates management data - including crop,
# fertilizer, irrigation, fertilizer, harvest, etc. - for every model in the 
# units needed by each. For APSIM, fertilizer is collected as kg N/ha of the N 
# in the fertilizer (not kg/ha of the whole fertilizer).
#######################################

suppressMessages({
  
  print(paste0("Starting 3_Create_management_input_files-setup_",site_name,".R"))

library(readxl)
library(dplyr)
library(magrittr)
library(lubridate)
library(data.table)


  #**********************************************************************
  ## local constants

       
mgmt_path=paste0("Data/",site_name,"/")

## adjust inputs for future scenarios

fert_adjust <- if_else(mgmt_scenario_num==41, 0.95,  # reduce N by 5%
                if_else(mgmt_scenario_num==42, 0.85, # reduce N by 15%
                if_else(mgmt_scenario_num==43, 0.75, # reduce N by 25%
                if_else(mgmt_scenario_num==44, 0.65, # reduce N by 35%
                1
                ))))
## Scenario 5 (residue removal) is managed in APSIM Classic as a paddock-Manager folder model
resid_adjust_chr <- if_else(mgmt_scenario_num==51, "50", # incorporate 50% residue
                if_else(mgmt_scenario_num==52, "25",     # incorporate 25% residue
                if_else(mgmt_scenario_num==53, "0",      # incorporate 0% residue
                if_else(mgmt_scenario_num==54, "50",     # no-till
                if_else(mgmt_scenario_num==55, "25",     # no-till
                if_else(mgmt_scenario_num==56, "0",      # no-till
                "0"
                ))))))
#resid_adjust_chr <- sub(".*\\.","",as.character(resid_adjust))
#resid_adjust <- if_else(resid_adjust_chr!="", as.numeric(paste0("0.",resid_adjust_chr)),
#                        1)
resid_adjust <- if_else(mgmt_scenario_num==51, 0.50, # incorporate 50% residue
                if_else(mgmt_scenario_num==52, 0.25, # incorporate 25% residue
                if_else(mgmt_scenario_num==53, 0,    # incorporate 0% residue: baseline
                if_else(mgmt_scenario_num==54, 0.50, # no-till
                if_else(mgmt_scenario_num==55, 0.25, # no-till
                if_else(mgmt_scenario_num==56, 0,    # no-till: baseline
                0
                ))))))


#**********************************************************************
## import

######### fertilizer ##########

## use Fert from 0_Observations_and_constants and put into same object
## as before with KBS
temp1_fert <- Fert %>%
  group_by(date,year,treatment,amend_type) %>%
  summarize(n_rate_kg_ha=round(mean(totalN_kgha,na.rm=T),0),
            p_rate_kg_ha=round(mean(totalP_kgha,na.rm=T),0),
            k_rate_kg_ha=round(mean(totalK_kgha,na.rm=T),0))
## need to include ops-specific columns as NA so columns will match when put together
temp2_fert <- temp1_fert %>%
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         year=year(date),
         observation_type="Fertilizer application",
         material=amend_type,
         rate=NA,
         units=NA,
         rowwidth_cm=NA,
         equipment=NA,
         crop=NA,
         # APSIM codes for Fertiliser and crop modules
         cultivar=NA,
         obs_code=if_else(amend_type %like% "lime", "CalciteCA",
                          if_else(n_rate_kg_ha>0, "NO3N", NA)),
         obs_code2=if_else(p_rate_kg_ha>0, "RockP", NA),
         rate_seeds_m2=NA,
         row_spacing_mm=NA,
         seed_depth_mm=NA,
         n_rate_g_m2=n_rate_kg_ha/10,
         p_rate_g_m2=p_rate_kg_ha/10,
         k_rate_g_m2=k_rate_kg_ha/10,
         daycent_mgmt_code=if_else(n_rate_g_m2>0,paste0("FERT (",n_rate_g_m2,"N)"),NA),
         daycent_mgmt_code2=if_else(p_rate_g_m2>0,paste0("FERT (",p_rate_g_m2,"P)"),NA)
  ) 

# create separate rows for additional APSIM and Daycent codes
## start with just secondary codes
exp_fert <- rbind(temp2_fert[!is.na(temp2_fert$daycent_mgmt_code),c(1:3,5:16,18:24)],
        setNames(temp2_fert[!is.na(temp2_fert$daycent_mgmt_code2),c(1:3,5:15,17:23,25)],
                 names(temp2_fert[,c(1:3,5:16,18:24)])))


############ tillage, planting, and harvest ###########

obs_tillage <- obs_tillage_raw[,c("date","year","treatment","Crop","Tillage Event","replicate")] %>%
  mutate(observation_type="Soil Preparation",
         material=NA,
         rate=NA,
         units=NA,
         rowwidth_cm=NA,
         equipment=`Tillage Event`,
         crop=if_else(grepl("Sorghum",Crop,fixed=TRUE), "Sorghum",
              if_else(grepl("Cotton", Crop,fixed=TRUE), "Cotton",
              "Error")),
         cultivar=NA,
         obs_code=if_else(grepl("Disk",equipment,fixed=TRUE), "disk",
                  if_else(grepl("Rod-weed",equipment,fixed=TRUE), "rodweed",
                  if_else(grepl("Plow",equipment,fixed=TRUE), "plow",
                  if_else(grepl("Sweep Till",equipment,fixed=TRUE), "sweep",
                  if_else(grepl("Rototiller",equipment,fixed=TRUE), "cultivate",
                  "Error"
                  ))))),
         obs_code2=NA,
         ) %>%
  select(-c(`Tillage Event`,Crop,replicate)) %>%
  distinct() %>% # removes duplicates due to replicates
  arrange(date,treatment)


obs_planting <- obs_planting_raw[,c("date","year","treatment","replicate","Crop",
                                    "Cultivar","Planting Density kg/ha",
                                    "Planting Method","Row Width cm")] %>%
  mutate(observation_type="Planting",
         material=paste(Cultivar,Crop),
         ## rate data is in col N of planting data sheet
         rate=if_else(grepl("Sorghum",Crop,fixed=TRUE),7937.866*`Planting Density kg/ha`,
                      if_else(grepl("Cotton",Crop,fixed=TRUE),123550,
                      if_else(grepl("Rye",Crop,fixed=TRUE),8164.663*`Planting Density kg/ha`,
                      0))),
         units="seeds/ha",
         rowwidth_cm=`Row Width cm`,
         equipment=NA,
         crop=if_else(grepl("Sorghum",Crop,fixed=TRUE), "Sorghum",
                      if_else(grepl("Cotton", Crop,fixed=TRUE), "Cotton",
                      if_else(grepl("Rye", Crop,fixed=TRUE), "Ryegrass",
                              "Error"))),
         cultivar=Cultivar,
         obs_code="plant",
         obs_code2="plant",
         ) %>%
  select(-c(Crop,replicate,`Planting Density kg/ha`,
            `Planting Method`,`Row Width cm`,Cultivar)) %>%
  distinct() %>% # removes duplicates due to replicates
  arrange(date,treatment)


obs_harvest <- obs_harvest_raw[,c("date","year","treatment","replicate","Crop",
                                  "Harvested Frac")] %>%
  mutate(observation_type="Harvest",
         material=Crop,
         rate=NA,
         units=NA,
         rowwidth_cm=NA,
         equipment=NA,
         crop=if_else(grepl("Sorghum",Crop,fixed=TRUE), "Sorghum",
              if_else(grepl("Cotton", Crop,fixed=TRUE), "Cotton",
              if_else(grepl("Rye", Crop,fixed=TRUE), "Ryegrass",
                              "Error"))),
         cultivar=NA,
         obs_code=if_else(`Harvested Frac` == "Lint","lint",
                  if_else(`Harvested Frac` == "Grain", "grain",
                  if_else(`Harvested Frac` == "None", "none",
                  if_else(`Harvested Frac` == "mow", "mow",
                          "Error")))),
         obs_code2=NA,
         ) %>%
  select(-c(Crop,replicate,`Harvested Frac`)) %>%
  distinct() %>% # removes duplicates due to replicates
  arrange(date,treatment)

temp_ops <- rbind(obs_tillage, obs_planting, obs_harvest) %>%
  mutate(date = as.Date(date,format="%m/%d/%Y"),
         n_rate_kg_ha = NA,
         p_rate_kg_ha = NA,
         k_rate_kg_ha = NA,
         rate_seeds_m2=round(rate/10000,0),
         row_spacing_mm = rowwidth_cm*10,
         seed_depth_mm = 25.4,
         cultivar = ifelse(crop=="Cotton","s71br", 
                    ifelse(crop=="Sorghum","mycultivar",
                    ifelse(crop=="Ryegrass",covercrop_APSIM,
                    ""))),
         n_rate_g_m2=NA,
         p_rate_g_m2=NA,
         k_rate_g_m2=NA,
         # translate field ops to Daycent codes
         daycent_mgmt_code=if_else(obs_code=="plow","CULT K",
                           if_else(obs_code=="disk","CULT H",
                           if_else(obs_code=="cultivate","CULT D",
                           if_else(obs_code=="cultipack","CULT CP",
                           if_else(obs_code=="finish","CULT D",
                           if_else(obs_code=="hoe","CULT ROW",
                           if_else(obs_code=="rodweed","CULT R",
                           if_else(obs_code=="sweep","CULT S",
                           if_else(obs_code=="plant"&crop=="Cotton","CROP COT",
                           if_else(obs_code=="plant"&crop=="Sorghum","CROP SORGH",
                           if_else(obs_code=="plant"&crop=="Ryegrass",paste("CROP",covercrop_Daycent),
                           ## 0% stover removal is baseline treatment
                           if_else(obs_code=="grain",paste0("HARV G"),
                           if_else(obs_code=="lint",paste0("HARV G"),
                           if_else(obs_code=="none",paste0("HARV G"),
                           ## assuming all-in-one event, ignore "stover" events
                           #if_else(obs_code=="stover"&crop=="Wheat","HARV G90S", 
                           if_else(obs_code %in% c("winterkill","mow"),"HARV KILL", 
                           NA))))))))))))))),
         daycent_mgmt_code2=if_else(obs_code=="plant","PLTM",NA)
  ) 

# create separate rows for secondary Daycent codes
exp_ops <- rbind(temp_ops[,c(1:3,14:16,4:12,17:23)],
                 setNames(temp_ops[!is.na(temp_ops$daycent_mgmt_code2),c(1:3,14:16,4:11,13,17:22,24)],
                          names(temp_ops[,c(1:3,14:16,4:12,17:23)])))


############ combine all field ops ###########

combined_ops <- rbind(exp_ops, exp_fert) %>%
  arrange(date,observation_type) # reorder all the records again

full_ops_ext_adj <- as.data.frame(combined_ops)

# restrict to start:end dates
full_ops_ext_adj <- full_ops_ext_adj[full_ops_ext_adj$year %in% experiment_year_range,]

}) # end suppressMessages
