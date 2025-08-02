###########################################################################
# Script: run_Daycent_base_KBS.R
# Author: Ellen Maas July 27, 2022
#         (Adapted from run_DD17CentEVI_Clovis_ds.R)
# Description: Simulates the base phase (land conversion to the start of 
# the experimental period at the Kellogg Biological Station, MI.
#
###########################################################################
# Audit Trail
# 1/3/2023: Added soiln.out to output files processing.
###########################################################################

print("Starting run_Daycent_base_KBS.R")


# Set the model path to the location of your files.
prior_path <- getwd()
model_path = paste0("~/CaseStudy/Daycent/",site_name,"/")
setwd(model_path)

# Set input files that are used for both base runs

unlink("site.100")
unlink("fix.100")
unlink("outfiles.in")

if(mgmt_scenario_num %in% c(51,52,53) |
   mgmt_scenario_grp == 4) { # T1
  file.copy("site_53.100","site.100", overwrite=TRUE)
  file.copy("fix_53.100","fix.100", overwrite=TRUE)
} else if(mgmt_scenario_num %in% c(54,55,56)) { #T2
  file.copy("site_56.100","site.100", overwrite=TRUE)
  file.copy("fix_56.100","fix.100", overwrite=TRUE)
} else if(mgmt_scenario_grp==3) { #T3
  file.copy("site_3.100","site.100", overwrite=TRUE)
  file.copy("fix_3.100","fix.100", overwrite=TRUE)
} else stop(paste("Error-unknown mgmt_scenario_grp:",mgmt_scenario_grp))
file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE)

# --------------- Run base1 cropping simulations (land conversion -> 1/2 way to start of exp) ----

# Base cropping schedule: date of land conversion -> start of experimental period
# Every scenario run uses the same base schedule file, so there is only one
print("**********Daycent base1 simulation*********")
unlink("soils.in")
unlink(paste0("sched_base1_",scenario_name,".bin"))
unlink(paste0("sched_base1_",scenario_name,".lis"))
unlink("cflows.out")
unlink(paste0("cflows_base1_",scenario_name,".out"))
unlink("harvest.csv")
unlink(paste0("harvest_base1_",scenario_name,".csv"))
unlink("livec.out")
unlink(paste0("livec_base1_",scenario_name,".out"))
unlink("methane.out")
unlink(paste0("methane_base1_",scenario_name,".out"))
unlink("soiltavg.out")
unlink(paste0("soiltavg_base1_",scenario_name,".out"))
unlink("soiln.out")
unlink(paste0("soiln_base1_",scenario_name,".out"))
unlink("summary.out")
unlink(paste0("summary_base1_",scenario_name,".out"))
unlink("vswc.out")
unlink(paste0("vswc_base1_",scenario_name,".out"))
unlink("wfps.out")
unlink(paste0("wfps_base1_",scenario_name,".out"))

file.copy("soils_base1.in", "soils.in", overwrite=TRUE)

system(paste0(daycent_executable," -s sched_base1",
              " -n sched_base1_",scenario_name,
              " -e sched_eq"), wait=TRUE)
system(paste0(daycent_list100," sched_base sched_base1_",scenario_name,
              " outvars.txt"), wait=TRUE)

file.copy("cflows.out", paste0("cflows_base1_",scenario_name,".out"), overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_base1_",scenario_name,".csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_base1_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_base1_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_base1_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_base1_",scenario_name,".out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_base1_",scenario_name,".out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_base1_",scenario_name,".out"), overwrite=TRUE)
file.copy("wfps.out", paste0("wfps_base1_",scenario_name,".out"), overwrite=TRUE)


# --------------- Run base2 cropping simulations (1/2 way to start of exp -> start of exp) ----

# Base cropping schedule: date of land conversion -> start of experimental period
# Every scenario run uses the same base schedule file, so there is only one
print("**********Daycent base2 simulation*********")
unlink("soils.in")
#unlink("outfiles.in")
unlink(paste0("sched_base2_",scenario_name,".bin"))
unlink(paste0("sched_base2_",scenario_name,".lis"))
unlink("cflows.out")
unlink(paste0("cflows_base2_",scenario_name,".out"))
unlink("harvest.csv")
unlink(paste0("harvest_base2_",scenario_name,".csv"))
unlink("livec.out")
unlink(paste0("livec_base2_",scenario_name,".out"))
unlink("methane.out")
unlink(paste0("methane_base2_",scenario_name,".out"))
unlink("soiltavg.out")
unlink(paste0("soiltavg_base2_",scenario_name,".out"))
unlink("soiln.out")
unlink(paste0("soiln_base2_",scenario_name,".out"))
unlink("summary.out")
unlink(paste0("summary_base2_",scenario_name,".out"))
unlink("vswc.out")
unlink(paste0("vswc_base2_",scenario_name,".out"))
unlink("wfps.out")
unlink(paste0("wfps_base2_",scenario_name,".out"))

#file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE)
file.copy("soils_base2.in", "soils.in", overwrite=TRUE)

system(paste0(daycent_executable," -s sched_base2",
              " -n sched_base2_",scenario_name,
              " -e sched_base1_",scenario_name), wait=TRUE)
system(paste0(daycent_list100," sched_base sched_base2_",scenario_name,
              " outvars.txt"), wait=TRUE)

file.copy("cflows.out", paste0("cflows_base2_",scenario_name,".out"), overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_base2_",scenario_name,".csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_base2_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_base2_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_base2_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_base2_",scenario_name,".out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_base2_",scenario_name,".out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_base2_",scenario_name,".out"), overwrite=TRUE)
file.copy("wfps.out", paste0("wfps_base2_",scenario_name,".out"), overwrite=TRUE)



# --------------- Reset working directory --------------- 
setwd(prior_path)

