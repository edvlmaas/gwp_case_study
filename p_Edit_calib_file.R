#######################################
# Script: p_Edit_calib_file
# Author: Ellen Maas
# Date: Nov. 23, 2022
# Description: It creates an output file with results from calibration of
# each model and scenario, run at the end of each "9_Results...calibration"
# script to collect select results. If the file already exists, it checks to 
# see if a record already exists for the model and scenario. If it does, it 
# replaces the data, otherwise it appends it to the end.
#######################################

p_Edit_calib_file <- function(data_mtx,model_name,scenario_name) {
  
  print("Starting p_Edit_calib_file")
  
  #**********************************************************************
  
  # Write data to model's log -------------------------------------
  
  write.table(data_mtx,file=paste0(results_path,"Calibration_log_",model_name,".csv"),
              append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
  
  
  # Write data to summary log -------------------------------------
  
  outfile_name <- paste0(results_path,"Calibration_summary.csv")
  colnames(data_mtx) <- log_col_headers
  
  # if the output file already exists, check if the model/scenario already has
  # an entry and replace it; otherwise, create a new file
  if(file.exists(outfile_name)) {
    existing_data <- read.table(file=outfile_name,header=TRUE,sep=",")
    existing_rec <- which(existing_data$Model==model_name & 
                            existing_data$Scenario_Name==scenario_name)
    if(length(existing_rec!=0)) {
      existing_data[existing_rec,"Date_time"] <- as.character(Sys.time())
      existing_data[existing_rec,"Maize_slope_1to1"] <- data_mtx[,7]
      existing_data[existing_rec,"Maize_yint_1to1"] <- data_mtx[,8]
      existing_data[existing_rec,"Maize_R2_1to1"] <- data_mtx[,9]
      existing_data[existing_rec,"Maize_RMSE_1to1"] <- data_mtx[,10]
      existing_data[existing_rec,"Maize_wNSE_1to1"] <- data_mtx[,11]
      existing_data[existing_rec,"Maize_pbias_1to1"] <- data_mtx[,12]
      existing_data[existing_rec,"Maize_diff"] <- data_mtx[,13]
      existing_data[existing_rec,"Soy_slope_1to1"] <- data_mtx[,14]
      existing_data[existing_rec,"Soy_yint_1to1"] <- data_mtx[,15]
      existing_data[existing_rec,"Soy_R2_1to1"] <- data_mtx[,16]
      existing_data[existing_rec,"Soy_RMSE_1to1"] <- data_mtx[,17]
      existing_data[existing_rec,"Soy_wNSE_1to1"] <- data_mtx[,18]
      existing_data[existing_rec,"Soy_pbias_1to1"] <- data_mtx[,19]
      existing_data[existing_rec,"Soy_diff"] <- data_mtx[,20]
      existing_data[existing_rec,"Wheat_slope_1to1"] <- data_mtx[,21]
      existing_data[existing_rec,"Wheat_yint_1to1"] <- data_mtx[,22]
      existing_data[existing_rec,"Wheat_R2_1to1"] <- data_mtx[,23]
      existing_data[existing_rec,"Wheat_RMSE_1to1"] <- data_mtx[,24]
      existing_data[existing_rec,"Wheat_wNSE_1to1"] <- data_mtx[,25]
      existing_data[existing_rec,"Wheat_pbias_1to1"] <- data_mtx[,26]
      existing_data[existing_rec,"Wheat_diff"] <- data_mtx[,27]
      existing_data[existing_rec,"SOC_slope_1to1"] <- data_mtx[,28]
      existing_data[existing_rec,"SOC_yint_1to1"] <- data_mtx[,29]
      existing_data[existing_rec,"SOC_R2_1to1"] <- data_mtx[,30]
      existing_data[existing_rec,"SOC_RMSE_1to1"] <- data_mtx[,31]
      existing_data[existing_rec,"SOC_wNSE_1to1"] <- data_mtx[,32]
      existing_data[existing_rec,"SOC_pbias_1to1"] <- data_mtx[,33]
      existing_data[existing_rec,"SOC_diff"] <- data_mtx[,34]
      existing_data[existing_rec,"SOC_diff_noout"] <- data_mtx[,35]
      existing_data[existing_rec,"Temp_slope_1to1"] <- data_mtx[,36]
      existing_data[existing_rec,"Temp_yint_1to1"] <- data_mtx[,37]
      existing_data[existing_rec,"Temp_R2_1to1"] <- data_mtx[,38]
      existing_data[existing_rec,"Temp_RMSE_1to1"] <- data_mtx[,39]
      existing_data[existing_rec,"Temp_wNSE_1to1"] <- data_mtx[,40]
      existing_data[existing_rec,"Temp_pbias_1to1"] <- data_mtx[,41]
      existing_data[existing_rec,"Temp_diff"] <- data_mtx[,42]
      existing_data[existing_rec,"Moist_slope_1to1"] <- data_mtx[,43]
      existing_data[existing_rec,"Moist_yint_1to1"] <- data_mtx[,44]
      existing_data[existing_rec,"Moist_R2_1to1"] <- data_mtx[,45]
      existing_data[existing_rec,"Moist_RMSE_1to1"] <- data_mtx[,46]
      existing_data[existing_rec,"Moist_wNSE_1to1"] <- data_mtx[,47]
      existing_data[existing_rec,"Moist_pbias_1to1"] <- data_mtx[,48]
      existing_data[existing_rec,"Moist_diff"] <- data_mtx[,49]
      existing_data[existing_rec,"N2O_slope_1to1"] <- data_mtx[,50]
      existing_data[existing_rec,"N2O_yint_1to1"] <- data_mtx[,51]
      existing_data[existing_rec,"N2O_R2_1to1"] <- data_mtx[,52]
      existing_data[existing_rec,"N2O_RMSE_1to1"] <- data_mtx[,53]
      existing_data[existing_rec,"N2O_wNSE_1to1"] <- data_mtx[,54]
      existing_data[existing_rec,"N2O_pbias_1to1"] <- data_mtx[,55]
      existing_data[existing_rec,"N2O_diff"] <- data_mtx[,56]
      existing_data[existing_rec,"CH4_slope_1to1"] <- data_mtx[57]
      existing_data[existing_rec,"CH4_yint_1to1"] <- data_mtx[,58]
      existing_data[existing_rec,"CH4_R2_1to1"] <- data_mtx[,59]
      existing_data[existing_rec,"CH4_RMSE_1to1"] <- data_mtx[,60]
      existing_data[existing_rec,"CH4_wNSE_1to1"] <- data_mtx[,61]
      existing_data[existing_rec,"CH4_pbias_1to1"] <- data_mtx[,62]
      existing_data[existing_rec,"CH4_diff"] <- data_mtx[,63]
      existing_data[existing_rec,"MBio_slope_1to1"] <- data_mtx[64]
      existing_data[existing_rec,"MBio_yint_1to1"] <- data_mtx[,65]
      existing_data[existing_rec,"MBio_R2_1to1"] <- data_mtx[66]
      existing_data[existing_rec,"MBio_RMSE_1to1"] <- data_mtx[,67]
      existing_data[existing_rec,"MBio_diff"] <- data_mtx[,68]
      existing_data[existing_rec,"Cotton_slope_1to1"] <- data_mtx[,69]
      existing_data[existing_rec,"Cotton_yint_1to1"] <- data_mtx[,70]
      existing_data[existing_rec,"Cotton_R2_1to1"] <- data_mtx[,71]
      existing_data[existing_rec,"Cotton_RMSE_1to1"] <- data_mtx[,72]
      existing_data[existing_rec,"Cotton_diff"] <- data_mtx[,73]
      existing_data[existing_rec,"Sorghum_slope_1to1"] <- data_mtx[,74]
      existing_data[existing_rec,"Sorghum_yint_1to1"] <- data_mtx[,75]
      existing_data[existing_rec,"Sorghum_R2_1to1"] <- data_mtx[,76]
      existing_data[existing_rec,"Sorghum_RMSE_1to1"] <- data_mtx[,77]
      existing_data[existing_rec,"Sorghum_diff"] <- data_mtx[,78]
      existing_data[existing_rec,"Maize_cultivar"] <-data_mtx[,79]
      existing_data[existing_rec,"Soybean_cultivar"] <-data_mtx[,80]
      existing_data[existing_rec,"Wheat_cultivar"] <-data_mtx[,81]
      existing_data[existing_rec,"Cotton_cultivar"] <-data_mtx[,82]
      existing_data[existing_rec,"Sorghum_cultivar"] <-data_mtx[,83]
      existing_data[existing_rec,"Maize_slope_time"] <- data_mtx[,84]
      existing_data[existing_rec,"Maize_yint_time"] <- data_mtx[,85]
      existing_data[existing_rec,"Maize_R2_time"] <- data_mtx[,86]
      existing_data[existing_rec,"Maize_RMSE_time"] <- data_mtx[,87]
      existing_data[existing_rec,"Soy_slope_time"] <- data_mtx[,88]
      existing_data[existing_rec,"Soy_yint_time"] <- data_mtx[,89]
      existing_data[existing_rec,"Soy_R2_time"] <- data_mtx[,90]
      existing_data[existing_rec,"Soy_RMSE_time"] <- data_mtx[,91]
      existing_data[existing_rec,"Wheat_slope_time"] <- data_mtx[,92]
      existing_data[existing_rec,"Wheat_yint_time"] <- data_mtx[,93]
      existing_data[existing_rec,"Wheat_R2_time"] <- data_mtx[,94]
      existing_data[existing_rec,"Wheat_RMSE_time"] <- data_mtx[,95]
      existing_data[existing_rec,"SOC_slope_time"] <- data_mtx[,96]
      existing_data[existing_rec,"SOC_yint_time"] <- data_mtx[,97]
      existing_data[existing_rec,"SOC_R2_time"] <- data_mtx[,98]
      existing_data[existing_rec,"SOC_RMSE_time"] <- data_mtx[,99]
      existing_data[existing_rec,"Temp_slope_time"] <- data_mtx[,100]
      existing_data[existing_rec,"Temp_yint_time"] <- data_mtx[,101]
      existing_data[existing_rec,"Temp_R2_time"] <- data_mtx[,102]
      existing_data[existing_rec,"Temp_RMSE_time"] <- data_mtx[,103]
      existing_data[existing_rec,"Moist_slope_time"] <- data_mtx[,104]
      existing_data[existing_rec,"Moist_yint_time"] <- data_mtx[,105]
      existing_data[existing_rec,"Moist_R2_time"] <- data_mtx[,106]
      existing_data[existing_rec,"Moist_RMSE_time"] <- data_mtx[,107]
      existing_data[existing_rec,"N2O_slope_time"] <- data_mtx[,108]
      existing_data[existing_rec,"N2O_yint_time"] <- data_mtx[,109]
      existing_data[existing_rec,"N2O_R2_time"] <- data_mtx[,110]
      existing_data[existing_rec,"N2O_RMSE_time"] <- data_mtx[,111]
      existing_data[existing_rec,"CH4_slope_time"] <- data_mtx[112]
      existing_data[existing_rec,"CH4_yint_time"] <- data_mtx[,113]
      existing_data[existing_rec,"CH4_R2_time"] <- data_mtx[,114]
      existing_data[existing_rec,"CH4_RMSE_time"] <- data_mtx[,115]
      existing_data[existing_rec,"MBio_slope_time"] <- data_mtx[116]
      existing_data[existing_rec,"MBio_yint_time"] <- data_mtx[,117]
      existing_data[existing_rec,"MBio_R2_time"] <- data_mtx[,118]
      existing_data[existing_rec,"MBio_RMSE_time"] <- data_mtx[,119]
      existing_data[existing_rec,"Cotton_slope_time"] <- data_mtx[,120]
      existing_data[existing_rec,"Cotton_yint_time"] <- data_mtx[,121]
      existing_data[existing_rec,"Cotton_R2_time"] <- data_mtx[,122]
      existing_data[existing_rec,"Cotton_RMSE_time"] <- data_mtx[,123]
      existing_data[existing_rec,"Sorghum_slope_time"] <- data_mtx[,124]
      existing_data[existing_rec,"Sorghum_yint_time"] <- data_mtx[,125]
      existing_data[existing_rec,"Sorghum_R2_time"] <- data_mtx[,126]
      existing_data[existing_rec,"Sorghum_RMSE_time"] <- data_mtx[,127]
      data_mtx <- existing_data
    } else {
      data_mtx <- rbind(existing_data,data_mtx)
    } # end if record exists
  } # end if file exists
  
  write.table(data_mtx,file=outfile_name,
              col.names=T,
              row.names=F,sep=",")
  
}
