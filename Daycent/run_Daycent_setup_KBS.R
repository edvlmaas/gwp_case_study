###########################################################################
# Script: run_Daycent_setup_KBS.R
# Author: Ellen Maas July 27, 2022
#         (Adapted from run_DD17CentEVI_Clovis_ds.R)
# Description: Creates the variables needed for the working environment
# for the Kellogg Biological Station, MI.
#
###########################################################################
# Audit Trail
# 7/27/2022: Created script.
###########################################################################

print("Starting run_Daycent_setup_KBS.R")

# 
# # Set the model path to the location of this site's files.
# prior_path <- getwd()
# model_path = paste0("~/CaseStudy/Daycent/",site_name,"/")
# setwd(model_path)

# local constants
daycent_executable <- "DD17centEVI.exe"
daycent_list100 <- "DD17list100.exe"

# # --------------- Reset working directory --------------- 
# setwd(prior_path)
