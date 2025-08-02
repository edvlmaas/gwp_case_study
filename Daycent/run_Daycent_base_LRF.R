###########################################################################
# Script: run_Daycent_base_LRF.R
# Author: Ellen Maas July 27, 2022
#         (Adapted from run_DD17CentEVI_Clovis_ds.R)
# Description: Simulates the base phase (land conversion to the start of 
# the experimental period at the Liberty Research Farm, TX.
#
###########################################################################
# Audit Trail
# 4/2023: Cloned KBS version.
###########################################################################

print("Starting run_Daycent_base_LRF.R")


# Set the model path to the location of your files.
prior_path <- getwd()
model_path = paste0("~/CaseStudy/Daycent/",site_name,"/")
setwd(model_path)


# --------------- Run base cropping simulations (land conversion - start exp) ---------------

# Base cropping schedule: date of land conversion -> start of experimental period
# Every scenario run uses the same base schedule file, so there is only one
print("**********Daycent base simulation*********")
unlink(paste0("sched_base.bin"))
unlink(paste0("sched_base.lis"))
unlink("cflows.out")
unlink(paste0("cflows_base.out"))
unlink("harvest.csv")
unlink(paste0("harvest_base.csv"))
unlink("livec.out")
unlink(paste0("livec_base.out"))
unlink("methane.out")
unlink(paste0("methane_base.out"))
unlink("soiltavg.out")
unlink(paste0("soiltavg_base.out"))
unlink("soiln.out")
unlink(paste0("soiln_base.out"))
unlink("summary.out")
unlink(paste0("summary_base.out"))
unlink("vswc.out")
unlink(paste0("vswc_base.out"))
unlink("wfps.out")
unlink(paste0("wfps_base.out"))

file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE)

system(paste0(daycent_executable," -s sched_base",
              " -n sched_base",
              " -e sched_eq"), wait=TRUE)
system(paste0(daycent_list100," sched_base sched_base outvars.txt"), wait=TRUE)

file.copy("cflows.out", paste0("cflows_base.out"), overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_base.csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_base_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_base.out"), overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_base.out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_base.out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_base.out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_base.out"), overwrite=TRUE)
file.copy("wfps.out", paste0("wfps_base.out"), overwrite=TRUE)


# --------------- Reset working directory --------------- 
setwd(prior_path)

