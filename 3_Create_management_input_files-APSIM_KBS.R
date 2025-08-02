#######################################
# Function: 3_Create_management_input_files-APSIM_KBS
# Author: Ellen Maas
# Date: Sept 23, 2022
# Description: Generates management data in the final format
# needed specifically by APSIM.
#######################################

suppressMessages({
  
  print(paste0("Starting 3_Create_management_input_files-APSIM_",site_name,".R"))
  
  library(stringr)
  library(lubridate)


  #***************************************************
  #*  Experimental period -----------------------------------------------------
  #***************************************************

  # remove duplicates created by DayCent coding
  APSIM_data_all <- distinct(full_ops_ext_adj[,c(1:20)])
  
  
  # if(site_name=="LRF") {
  #   # remove harvest of rye that was planted in 2002 in all plots, 
  #   # because APSIM does no spin-up and starts fresh in 2003
  #   APSIM_data <- APSIM_data_all[APSIM_data_all$date >= as.Date("2003-05-01"),]
  # } else if(site_name=="KBS") {
      APSIM_data <- APSIM_data_all
  # }
  
  # add tillage depth
  if(site_name=="KBS") {
  APSIM_data$till_depth <- ifelse(APSIM_data$obs_code=="plow",110,
                           ifelse(APSIM_data$obs_code %in% c("disk","hoe"),30,
                           ifelse(APSIM_data$observation_type=="Soil Preparation",5,
                                  NA)))
  } else if(site_name=="LRF") {
    APSIM_data$till_depth <- ifelse(APSIM_data$obs_code=="plow",150,
                             ifelse(APSIM_data$obs_code %in% c("rodweed","disk"),80,
                             ifelse(APSIM_data$observation_type=="Soil Preparation",5,
                                    NA)))
   }

  # change clovers to "AgPasture" for APSIM Classic
  APSIM_data$crop <- ifelse(APSIM_data$crop=="RedClover","WhiteClover",
                            APSIM_data$crop)
  
  
  
  # select treatment
  temp_conv <- APSIM_data[APSIM_data$treatment==treatment & 
                            !is.na(APSIM_data$obs_code),]
  APSIM_conv <- temp_conv[rowSums(is.na(temp_conv)) != ncol(temp_conv),]
  
  #APSIM_ops <- data.frame()
  
  if(file.exists(paste0(apsim_path,"mgmt_",
                        clim_scenario_num,"_",mgmt_scenario_num,".txt"))) {
    file.remove(paste0(apsim_path,"mgmt_",
                       clim_scenario_num,"_",mgmt_scenario_num,".txt")) }
  
  file.create(paste0(apsim_path,"mgmt_",
                     clim_scenario_num,"_",mgmt_scenario_num,".txt")) 
  
  # Just for LRF, construct an 8-year spin-up taking the whole 2003-2010 
  # experiment and starting it in 1995
  if(site_name=="LRF") {
    spinup_df <- APSIM_conv
    spinup_df$year <- APSIM_conv$year-8
    spinup_df$date <- APSIM_conv$date - years(8)
    APSIM_conv <- rbind(spinup_df,APSIM_conv)
    ## remove harvest of rye in 19952 in all plots
    APSIM_conv <- APSIM_conv[APSIM_conv$date >= as.Date("1995-05-01"),]
  }
  
  for (i in 1:nrow(APSIM_conv)) {
    current_op <- APSIM_conv[i,"observation_type"]
    if(current_op=="Fertilizer application") {
      cat(paste0(as.character(format(APSIM_conv[i,"date"],"%d/%m/%Y")),
                 "\tfertiliser apply amount = ",
                 ifelse(str_sub(APSIM_conv[i,"obs_code"],-1,-1)=='N', APSIM_conv[i,"n_rate_kg_ha"],
                        ifelse(str_sub(APSIM_conv[i,"obs_code"],-1,-1)=='P', APSIM_conv[i,"p_rate_kg_ha"],
                               APSIM_conv[i,"rate_kg_ha"])),
                 " (kg/ha), depth = 50 (mm), type = ", 
                 ifelse(APSIM_conv[i,"obs_code"]=="NO3N","NO3_N",
                        ifelse(APSIM_conv[i,"obs_code"]=="CalciteCA","Calcite_CA",
                               ifelse(APSIM_conv[i,"obs_code"]=="RockP","Rock_P",
                                      APSIM_conv[i,"obs_code"])))),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="grain") {
      NULL # APSIM Classic harvests cash crops by harvest rule; necessary in order
           # to reduce the number of operations which can overwhelm it.
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="stover") {
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\t",tolower(APSIM_conv[i,"crop"])," harvest"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\t",tolower(APSIM_conv[i,"crop"])," end_crop"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
      #9/27/2022: changed fraction from 0.9 to 0.75, to leave 25% on surface
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\tsurfaceorganicmatter tillage type = user_defined, f_incorp = 0.75, tillage_depth = 0"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="winterkill") {
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\t",tolower(APSIM_conv[i,"crop"])," end_crop"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="mow") {
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\t",tolower(APSIM_conv[i,"crop"])," end_crop"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\tsurfaceorganicmatter tillage type = user_defined, f_incorp = 0, tillage_depth = 0"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="hoe") {
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\t",tolower(APSIM_conv[i,"crop"])," harvest"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\t",tolower(APSIM_conv[i,"crop"])," end_crop"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\tsurfaceorganicmatter tillage type = user_defined, f_incorp = 0, tillage_depth = 30"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else if(current_op=="Planting") {
      cat(paste0(as.character(format(APSIM_conv[i,"date"],"%d/%m/%Y")),
                 "\t",tolower(APSIM_conv[i,"crop"]), " sow ",
                 "plants = ",APSIM_conv[i,"rate_seeds_m2"], 
                 ", sowing_depth = ",APSIM_conv[i,"seed_depth_mm"],
                 ", cultivar = ", ifelse(APSIM_conv[i,"cultivar"]=="Generic_MG2","mg_2",
                                         tolower(APSIM_conv[i,"cultivar"])),
                 ", row_spacing = ", APSIM_conv[i,"row_spacing_mm"],
                 ifelse(APSIM_conv[i,"crop"]=="Sorghum", ", tiller_no_fertile = 1",""),
                 ", crop_class = plant"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
    } else if(current_op=="Soil Preparation") {
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"),
                 "\tsurfaceorganicmatter tillage type = user_defined, f_incorp = ",
                 ifelse(APSIM_conv[i,"obs_code"]=="plow",0.9,
                        ifelse(APSIM_conv[i,"obs_code"] %in% c("disk","hoe"),0.5,
                               0.1))
                 ,", tillage_depth = ", APSIM_conv[i,"till_depth"]),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else "Error"
    
  } # end for loop

  #***************************************************
  #*  Future period -----------------------------------------------------
  #***************************************************
  #*
  APSIM_ops <- read.delim(paste0(apsim_path,"mgmt_",
                                 clim_scenario_num,"_",mgmt_scenario_num,".txt"),header=FALSE)
  
  APSIM_ops_fut <- data.frame()
  

    APSIM_ops_3yr <- as.data.frame(APSIM_ops[str_sub(APSIM_ops[,"V1"],-4,-1) %in% 
                                               (experiment_end_year-2):experiment_end_year,])
    colnames(APSIM_ops_3yr) <- c("V1","V2")
    
    # Add conventional tillage back in for future scenario
    if(mgmt_scenario_grp!=2) {
      tillage_ops <- data.frame(V1=c("10/04/2020","15/04/2021","20/04/2021"),
                                V2=c("surfaceorganicmatter tillage type = user_defined, f_incorp = 0.9, tillage_depth = 110",
                                     "surfaceorganicmatter tillage type = user_defined, f_incorp = 0.9, tillage_depth = 110",
                                     "surfaceorganicmatter tillage type = user_defined, f_incorp = 0.5, tillage_depth = 30"
                                )
      )
      APSIM_ops_tillage <- rbind(APSIM_ops_3yr,tillage_ops) %>%
        arrange(as.Date(V1,"%d/%m/%Y"))
      APSIM_ops_3yr <- APSIM_ops_tillage
    }
    
    if(mgmt_scenario_grp==4) {
      APSIM_ops_3yr <- APSIM_ops_3yr %>%
        mutate(x = as.character(as.numeric(str_extract(V2, "(?i)(?<=amount =\\D)\\d+"))*fert_adjust),
               V2 = ifelse(str_detect(V2, "fertiliser"), str_replace(V2,"(?i)(?<=amount =\\D)\\d+", x), V2)) %>%
        select(-x)
    } 
    
    # # Scenario 5 (residue removal) is managed in APSIM Classic as a paddock-Manager folder model
    # # (BTW, the following code is still designed for APSIM Next Gen...)
    # else if(mgmt_scenario_grp==5) {
    #   endcrop_dat <- APSIM_ops_3yr[which(str_detect(APSIM_ops_3yr$V1, "EndCrop")),]
    #   for (i in 1:length(endcrop_dat)) {
    #     APSIM_ops_3yr <- insertRows(APSIM_ops_3yr,
    #                                 which(str_detect(APSIM_ops_3yr$V1, "EndCrop"))[i]+1,
    #                                 new=paste0(substr(endcrop_dat[i],1,10),
    #                                            " [SurfaceOrganicMatter].Incorporate(fraction: ",resid_adjust,", depth: 0)"))
    #   }
    # } # end if
    
    
    # Now take the 3-year management actions and repeat them out to 2100
    repeat_times <- ceiling((end_fut_period_year-experiment_end_year)/3)
    
    for (i in 1:repeat_times) {
      str_sub(APSIM_ops_3yr[,"V1"],-4,-1) <- as.character(as.integer(str_sub(APSIM_ops_3yr[,"V1"],-4,-1)) + 3)
      APSIM_ops_fut <- rbind(APSIM_ops_fut, APSIM_ops_3yr)
    }
    
    # Add one-time biochar addition for those scenarios. Biochar is added in the
    # last year of the experimental period in order to better isolate the future
    # effects of biochar addition.
    if(mgmt_scenario_grp==6) {
      biochar_ops <- data.frame(V1="12/08/2021",
                                V2="biochar5v3 tillage type = user_defined, f_incorp = 1.0, tillage_depth = 300")
      APSIM_ops_fut_biochar <- rbind(APSIM_ops_fut,biochar_ops) %>%
        arrange(as.Date(V1,"%d/%m/%Y"))
      APSIM_ops_fut <- APSIM_ops_fut_biochar
    }
 
  APSIM_ops_fut2 <- paste0(APSIM_ops_fut$V1,"\t",
                           APSIM_ops_fut$V2)
  
  write.table(APSIM_ops_fut2, file=paste0(apsim_path,"mgmt_",
                                          clim_scenario_num,"_",mgmt_scenario_num,".txt"),              
              quote = F, row.names = F, col.names = F, append=TRUE)
  

}) # end suppressMessages

###########
# notes with Fernando
# edit_apsimx has pre-set nodes. Other allows editing of anything
# give him files I'm using for management and he'll work on how to do it

