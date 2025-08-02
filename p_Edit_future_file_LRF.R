#######################################
# Function: p_Edit_future_file_LRF.R
# Author: Ellen Maas
# Date: Nov. 4, 2022
# Output: Clones p_Edit_output_file. It creates an output file
# with summary data from future results of a model run. If the
# file already exists, it checks to see if a record already 
# exists for the model and scenario. If it does, it replaces 
# the data, otherwise it appends it to the end.
#######################################

p_Edit_future_file <- function(data_mtx,model_name,scenario_name) {
  
  print(paste0("Starting p_Edit_future_file_",site_name))
  
  outfile_name <- paste0(results_path,"Summary_future_output.csv")
  colnames(data_mtx) <- c("Model","Climate_Scenario","Mgmt_Scenario",
                          "Scenario_Name","Scenario_Abbrev",
                          # APSIM 
                          "SW_5cm_first","SW_5cm_change",
                          "SW_15cm_first","SW_15cm_change",
                          "SW_35cm_first","SW_35cm_change",
                          "SW_60cm_first","SW_60cm_change",
                          "DW_5cm_first","DW_5cm_change",
                          "DW_15cm_first","DW_15cm_change",
                          "DW_35cm_first","DW_35cm_change",
                          "DW_60cm_first","DW_60cm_change",
                          "DW_0to60cm_first","DW_0to60cm_change",
                          "SW_10cm_first","SW_10cm_change",
                          "DW_10cm_first","DW_10cm_change",
                          "SoilT_5cm_first","SoilT_5cm_change",
                          "SoilT_15cm_first","SoilT_15cm_change",
                          "SoilT_35cm_first","SoilT_35cm_change",
                          "SoilT_60cm_first","SoilT_60cm_change",
                          "SoilT_10cm_first","SoilT_10cm_change",
                          "NO3_5cm_first","NO3_5cm_change",
                          "NO3_15cm_first","NO3_15cm_change",
                          "NO3_35cm_first","NO3_35cm_change",
                          "NO3_60cm_first","NO3_60cm_change",
                          "NO3_0to60cm_first","NO3_0to60cm_change",
                          "N2O_5cm_first","N2O_5cm_change",
                          "N2O_15cm_first","N2O_15cm_change",
                          "N2O_35cm_first","N2O_35cm_change",
                          "N2O_60cm_first","N2O_60cm_change",
                          "N2O_0to60cm_first","N2O_0to60cm_change",
                          "N2O_profile_first","N2O_profile_change",
                          "BC_10cm_first","BC_10cm_change",
                          "BN_10cm_first","BN_10cm_change",
                          "HC_10cm_first","HC_10cm_change",
                          "HN_10cm_first","HN_10cm_change",
                          "CinB_10cm_first","CinB_10cm_change",
                          "CinH_10cm_first","CinH_10cm_change",
                          "CinBtoH_10cm_first", "CinBtoH_10cm_change",
                          "SOC_10cm_first","SOC_10cm_change",
                          # Additional Daycent
                          "SW_2cm_first", "SW_2cm_change",
                          "SW_20cm_first", "SW_20cm_change",
                          "SW_30cm_first", "SW_30cm_change",
                          "SW_45cm_first", "SW_45cm_change",
                          "DW_2cm_first", "DW_2cm_change",
                          "DW_10cm_first", "DW_10cm_change",
                          "DW_20cm_first", "DW_20cm_change",
                          "DW_30cm_first", "DW_30cm_change",
                          "DW_45cm_first", "DW_45cm_change",
                          "WFPS_2cm_first", "WFPS_2cm_change",
                          "WFPS_5cm_first", "WFPS_5cm_change",
                          "WFPS_10cm_first", "WFPS_10cm_change",
                          "WFPS_20cm_first", "WFPS_20cm_change",
                          "WFPS_30cm_first", "WFPS_30cm_change",
                          "WFPS_45cm_first", "WFPS_45cm_change",
                          "WFPS_60cm_first", "WFPS_60cm_change",
                          "SoilT_2cm_first", "SoilT_2cm_change",
                          "SoilT_20cm_first", "SoilT_20cm_change",
                          "SoilT_30cm_first", "SoilT_30cm_change",
                          "SoilT_45cm_first", "SoilT_45cm_change",
                          "NO3_2cm_first", "NO3_2cm_change",
                          "NO3_10cm_first", "NO3_10cm_change",
                          "NO3_20cm_first", "NO3_20cm_change",
                          "NO3_30cm_first", "NO3_30cm_change",
                          "NO3_45cm_first", "NO3_45cm_change",
                          "CH4_first", "CH4_change",
                          "CI_first", "CI_change")
  
  
  # if the output file already exists, check if the model/scenario already has
  # an entry and replace it; otherwise, create a new file
  if(file.exists(outfile_name)) {
    existing_data <- read.table(file=outfile_name,header=TRUE,sep=",")
    existing_rec <- which(existing_data$Model==model_name & existing_data$Scenario_Name==scenario_name)
    if(length(existing_rec!=0)) {
      existing_data[existing_rec,"SW_5cm_first"] <- data_mtx[,6]
      existing_data[existing_rec,"SW_5cm_change"] <- data_mtx[,7]
      existing_data[existing_rec,"SW_15cm_first"] <- data_mtx[,8]
      existing_data[existing_rec,"SW_15cm_change"] <- data_mtx[,9]
      existing_data[existing_rec,"SW_35cm_first"] <- data_mtx[,10]
      existing_data[existing_rec,"SW_35cm_change"] <- data_mtx[,11]
      existing_data[existing_rec,"SW_60cm_first"] <- data_mtx[,12]
      existing_data[existing_rec,"SW_60cm_change"] <- data_mtx[,13]
      existing_data[existing_rec,"DW_5cm_first"] <- data_mtx[,14]
      existing_data[existing_rec,"DW_5cm_change"] <- data_mtx[,15]
      existing_data[existing_rec,"DW_15cm_first"] <- data_mtx[,16]
      existing_data[existing_rec,"DW_15cm_change"] <- data_mtx[,17]
      existing_data[existing_rec,"DW_35cm_first"] <- data_mtx[,18]
      existing_data[existing_rec,"DW_35cm_change"] <- data_mtx[,19]
      existing_data[existing_rec,"DW_60cm_first"] <- data_mtx[,20]
      existing_data[existing_rec,"DW_60cm_change"] <- data_mtx[,21]
      existing_data[existing_rec,"DW_0to60cm_first"] <- data_mtx[,22]
      existing_data[existing_rec,"DW_0to60cm_change"] <- data_mtx[,23]
      existing_data[existing_rec,"SW_10cm_first"] <- data_mtx[,24]
      existing_data[existing_rec,"SW_10cm_change"] <- data_mtx[,25]
      existing_data[existing_rec,"DW_10cm_first"] <- data_mtx[,26]
      existing_data[existing_rec,"DW_10cm_change"] <- data_mtx[,27]
      existing_data[existing_rec,"SoilT_5cm_first"] <- data_mtx[,28]
      existing_data[existing_rec,"SoilT_5cm_change"] <- data_mtx[,29]
      existing_data[existing_rec,"SoilT_15cm_first"] <- data_mtx[,30]
      existing_data[existing_rec,"SoilT_15cm_change"] <- data_mtx[,31]
      existing_data[existing_rec,"SoilT_35cm_first"] <- data_mtx[,32]
      existing_data[existing_rec,"SoilT_35cm_change"] <- data_mtx[,33]
      existing_data[existing_rec,"SoilT_60cm_first"] <- data_mtx[,34]
      existing_data[existing_rec,"SoilT_60cm_change"] <- data_mtx[,35]
      existing_data[existing_rec,"SoilT_10cm_first"] <- data_mtx[,36]
      existing_data[existing_rec,"SoilT_10cm_change"] <- data_mtx[,37]
      existing_data[existing_rec,"NO3_5cm_first"] <- data_mtx[,38]
      existing_data[existing_rec,"NO3_5cm_change"] <- data_mtx[,39]
      existing_data[existing_rec,"NO3_15cm_first"] <- data_mtx[,40]
      existing_data[existing_rec,"NO3_15cm_change"] <- data_mtx[,41]
      existing_data[existing_rec,"NO3_35cm_first"] <- data_mtx[,42]
      existing_data[existing_rec,"NO3_35cm_change"] <- data_mtx[,43]
      existing_data[existing_rec,"NO3_60cm_first"] <- data_mtx[44]
      existing_data[existing_rec,"NO3_60cm_change"] <- data_mtx[,45]
      existing_data[existing_rec,"NO3_0to60cm_first"] <- data_mtx[,46]
      existing_data[existing_rec,"NO3_0to60cm_change"] <- data_mtx[,47]
      existing_data[existing_rec,"N2O_5cm_first"] <- data_mtx[,48]
      existing_data[existing_rec,"N2O_5cm_change"] <- data_mtx[,49]
      existing_data[existing_rec,"N2O_15cm_first"] <- data_mtx[,50]
      existing_data[existing_rec,"N2O_15cm_change"] <- data_mtx[,51]
      existing_data[existing_rec,"N2O_35cm_first"] <- data_mtx[,52]
      existing_data[existing_rec,"N2O_35cm_change"] <- data_mtx[,53]
      existing_data[existing_rec,"N2O_60cm_first"] <- data_mtx[,54]
      existing_data[existing_rec,"N2O_60cm_change"] <- data_mtx[,55]
      existing_data[existing_rec,"N2O_0to60cm_first"] <- data_mtx[,56]
      existing_data[existing_rec,"N2O_0to60cm_change"] <- data_mtx[,57]
      existing_data[existing_rec,"N2O_profile_first"] <- data_mtx[,58]
      existing_data[existing_rec,"N2O_profile_change"] <- data_mtx[,59]
      existing_data[existing_rec,"BC_10cm_first"] <- data_mtx[,60]
      existing_data[existing_rec,"BC_10cm_change"] <- data_mtx[,61]
      existing_data[existing_rec,"BN_10cm_first"] <- data_mtx[,62]
      existing_data[existing_rec,"BN_10cm_change"] <- data_mtx[,63]
      existing_data[existing_rec,"HC_10cm_first"] <- data_mtx[,64]
      existing_data[existing_rec,"HC_10cm_change"] <- data_mtx[,65]
      existing_data[existing_rec,"HN_10cm_first"] <- data_mtx[,66]
      existing_data[existing_rec,"HN_10cm_change"] <- data_mtx[,67]
      existing_data[existing_rec,"CinB_10cm_first"] <- data_mtx[,68]
      existing_data[existing_rec,"CinB_10cm_change"] <- data_mtx[,69]
      existing_data[existing_rec,"CinH_10cm_first"] <- data_mtx[,70]
      existing_data[existing_rec,"CinH_10cm_change"] <- data_mtx[,71]
      existing_data[existing_rec,"CinBtoH_10cm_first"] <- data_mtx[,72]
      existing_data[existing_rec,"CinBtoH_10cm_change"] <- data_mtx[,73]
      existing_data[existing_rec,"SOC_10cm_first"] <- data_mtx[,74]
      existing_data[existing_rec,"SOC_10cm_change"] <- data_mtx[,75]
      # Daycent-only
      existing_data[existing_rec,"SW_2cm_first"] <- data_mtx[,76]
      existing_data[existing_rec,"SW_2cm_change"] <- data_mtx[,77]
      existing_data[existing_rec,"SW_20cm_first"] <- data_mtx[,78]
      existing_data[existing_rec,"SW_20cm_change"] <- data_mtx[,79]
      existing_data[existing_rec,"SW_30cm_first"] <- data_mtx[,80]
      existing_data[existing_rec,"SW_30cm_change"] <- data_mtx[,81]
      existing_data[existing_rec,"SW_45cm_first"] <- data_mtx[,82]
      existing_data[existing_rec,"SW_45cm_change"] <- data_mtx[,83]
      existing_data[existing_rec,"DW_2cm_first"] <- data_mtx[,84]
      existing_data[existing_rec,"DW_2cm_change"] <- data_mtx[,85]
      existing_data[existing_rec,"DW_10cm_first"] <- data_mtx[,86]
      existing_data[existing_rec,"DW_10cm_change"] <- data_mtx[,87]
      existing_data[existing_rec,"DW_20cm_first"] <- data_mtx[,88]
      existing_data[existing_rec,"DW_20cm_change"] <- data_mtx[,89]
      existing_data[existing_rec,"DW_30cm_first"] <- data_mtx[,90]
      existing_data[existing_rec,"DW_30cm_change"] <- data_mtx[,91]
      existing_data[existing_rec,"DW_45cm_first"] <- data_mtx[,92]
      existing_data[existing_rec,"DW_45cm_change"] <- data_mtx[,93]
      existing_data[existing_rec,"WFPS_2cm_first"] <- data_mtx[,94]
      existing_data[existing_rec,"WFPS_2cm_change"] <- data_mtx[,95]
      existing_data[existing_rec,"WFPS_5cm_first"] <- data_mtx[,96]
      existing_data[existing_rec,"WFPS_5cm_change"] <- data_mtx[,97]
      existing_data[existing_rec,"WFPS_10cm_first"] <- data_mtx[,98]
      existing_data[existing_rec,"WFPS_10cm_change"] <- data_mtx[,99]
      existing_data[existing_rec,"WFPS_20cm_first"] <- data_mtx[,100]
      existing_data[existing_rec,"WFPS_20cm_change"] <- data_mtx[,101]
      existing_data[existing_rec,"WFPS_30cm_first"] <- data_mtx[,102]
      existing_data[existing_rec,"WFPS_30cm_change"] <- data_mtx[,103]
      existing_data[existing_rec,"WFPS_45cm_first"] <- data_mtx[,104]
      existing_data[existing_rec,"WFPS_45cm_change"] <- data_mtx[,105]
      existing_data[existing_rec,"WFPS_60cm_first"] <- data_mtx[,106]
      existing_data[existing_rec,"WFPS_60cm_change"] <- data_mtx[,107]
      existing_data[existing_rec,"SoilT_2cm_first"] <- data_mtx[,108]
      existing_data[existing_rec,"SoilT_2cm_change"] <- data_mtx[,109]
      existing_data[existing_rec,"SoilT_20cm_first"] <- data_mtx[,110]
      existing_data[existing_rec,"SoilT_20cm_change"] <- data_mtx[,111]
      existing_data[existing_rec,"SoilT_30cm_first"] <- data_mtx[,112]
      existing_data[existing_rec,"SoilT_30cm_change"] <- data_mtx[,113]
      existing_data[existing_rec,"SoilT_45cm_first"] <- data_mtx[,114]
      existing_data[existing_rec,"SoilT_45cm_change"] <- data_mtx[,115]
      existing_data[existing_rec,"NO3_2cm_first"] <- data_mtx[,116]
      existing_data[existing_rec,"NO3_2cm_change"] <- data_mtx[,117]
      existing_data[existing_rec,"NO3_10cm_first"] <- data_mtx[,118]
      existing_data[existing_rec,"NO3_10cm_change"] <- data_mtx[,119]
      existing_data[existing_rec,"NO3_20cm_first"] <- data_mtx[,120]
      existing_data[existing_rec,"NO3_20cm_change"] <- data_mtx[,121]
      existing_data[existing_rec,"NO3_30cm_first"] <- data_mtx[,122]
      existing_data[existing_rec,"NO3_30cm_change"] <- data_mtx[,123]
      existing_data[existing_rec,"NO3_45cm_first"] <- data_mtx[,124]
      existing_data[existing_rec,"NO3_45cm_change"] <- data_mtx[,125]
      existing_data[existing_rec,"CH4_first"] <- data_mtx[,126]
      existing_data[existing_rec,"CH4_change"] <- data_mtx[,127]
      existing_data[existing_rec,"CI_first"] <- data_mtx[,128]
      existing_data[existing_rec,"CI_change"] <- data_mtx[,129]
      final_mtx <- existing_data
    } else {
      final_mtx <- rbind(existing_data,data_mtx)
    } # end if record exists
  } else {
    final_mtx <- data_mtx
  } # end if file exists
  
  write.table(final_mtx,file=paste0(outfile_name),
              col.names=c("Model","Climate_Scenario","Mgmt_Scenario",
                          "Scenario_Name","Scenario_Abbrev",
                          # APSIM 
                          "SW_5cm_first","SW_5cm_change",
                          "SW_15cm_first","SW_15cm_change",
                          "SW_35cm_first","SW_35cm_change",
                          "SW_60cm_first","SW_60cm_change",
                          "DW_5cm_first","DW_5cm_change",
                          "DW_15cm_first","DW_15cm_change",
                          "DW_35cm_first","DW_35cm_change",
                          "DW_60cm_first","DW_60cm_change",
                          "DW_0to60cm_first","DW_0to60cm_change",
                          "SW_10cm_first","SW_10cm_change",
                          "DW_10cm_first","DW_10cm_change",
                          "SoilT_5cm_first","SoilT_5cm_change",
                          "SoilT_15cm_first","SoilT_15cm_change",
                          "SoilT_35cm_first","SoilT_35cm_change",
                          "SoilT_60cm_first","SoilT_60cm_change",
                          "SoilT_10cm_first","SoilT_10cm_change",
                          "NO3_5cm_first","NO3_5cm_change",
                          "NO3_15cm_first","NO3_15cm_change",
                          "NO3_35cm_first","NO3_35cm_change",
                          "NO3_60cm_first","NO3_60cm_change",
                          "NO3_0to60cm_first","NO3_0to60cm_change",
                          "N2O_5cm_first","N2O_5cm_change",
                          "N2O_15cm_first","N2O_15cm_change",
                          "N2O_35cm_first","N2O_35cm_change",
                          "N2O_60cm_first","N2O_60cm_change",
                          "N2O_0to60cm_first","N2O_0to60cm_change",
                          "N2O_profile_first","N2O_profile_change",
                          "BC_10cm_first","BC_10cm_change",
                          "BN_10cm_first","BN_10cm_change",
                          "HC_10cm_first","HC_10cm_change",
                          "HN_10cm_first","HN_10cm_change",
                          "CinB_10cm_first","CinB_10cm_change",
                          "CinH_10cm_first","CinH_10cm_change",
                          "CinBtoH_10cm_first", "CinBtoH_10cm_change",
                          "SOC_10cm_first","SOC_10cm_change",
                          # Additional Daycent
                          "SW_2cm_first", "SW_2cm_change",
                          "SW_20cm_first", "SW_20cm_change",
                          "SW_30cm_first", "SW_30cm_change",
                          "SW_45cm_first", "SW_45cm_change",
                          "DW_2cm_first", "DW_2cm_change",
                          "DW_10cm_first", "DW_10cm_change",
                          "DW_20cm_first", "DW_20cm_change",
                          "DW_30cm_first", "DW_30cm_change",
                          "DW_45cm_first", "DW_45cm_change",
                          "WFPS_2cm_first", "WFPS_2cm_change",
                          "WFPS_5cm_first", "WFPS_5cm_change",
                          "WFPS_10cm_first", "WFPS_10cm_change",
                          "WFPS_20cm_first", "WFPS_20cm_change",
                          "WFPS_30cm_first", "WFPS_30cm_change",
                          "WFPS_45cm_first", "WFPS_45cm_change",
                          "WFPS_60cm_first", "WFPS_60cm_change",
                          "SoilT_2cm_first", "SoilT_2cm_change",
                          "SoilT_20cm_first", "SoilT_20cm_change",
                          "SoilT_30cm_first", "SoilT_30cm_change",
                          "SoilT_45cm_first", "SoilT_45cm_change",
                          "NO3_2cm_first", "NO3_2cm_change",
                          "NO3_10cm_first", "NO3_10cm_change",
                          "NO3_20cm_first", "NO3_20cm_change",
                          "NO3_30cm_first", "NO3_30cm_change",
                          "NO3_45cm_first", "NO3_45cm_change",
                          "CH4_first", "CH4_change",
                          "CI_first", "CI_change"),
              row.names=F,sep=",")
  
}