###########################################################################
# Script: run_Daycent_exp_KBS.R
# Author: Ellen Maas July 27, 2022
#         (Adapted from run_DD17CentEVI_Clovis_ds.R)
# Description: Simulates corn-soy-wheat trials at the Kellogg Biological 
# Station, MI.
#
###########################################################################
# Audit Trail
# 9/27/2022: Rewrote to more generic time points, removed site name
# 1/3/2023: Added soiln.out to output files processing.
###########################################################################

print("Starting run_Daycent_exp_KBS.R")


# Set the model path to the location of your files.
prior_path <- getwd()
model_path = paste0("~/CaseStudy/Daycent/",site_name,"/")
setwd(model_path)


# --------------- Run experimental period simulations  ---------------

print("**********Daycent experimental period simulation*********")
unlink("soils.in")
unlink("outfiles.in")
unlink("site.100")
unlink("fix.100")
unlink(paste0("sched_exp_",scenario_name,".bin"))
unlink(paste0("sched_exp_",scenario_name,".lis"))
unlink("cflows.out")
unlink(paste0("cflows_exp_",scenario_name,".out"))
unlink("dc_sip.csv")
unlink(paste0("dc_sip_exp_",scenario_name,".out"))
unlink("harvest.csv")
unlink(paste0("harvest_exp_",scenario_name,".csv"))
unlink("livec.out")
unlink(paste0("livec_exp_",scenario_name,".out"))
unlink("methane.out")
unlink(paste0("methane_exp_",scenario_name,".out"))
unlink("nflux.out")
unlink(paste0("nflux_exp_",scenario_name,".out"))
unlink("nflux.out")
unlink(paste0("soiln_exp_",scenario_name,".out"))
unlink("soiltavg.out")
unlink(paste0("soiltavg_exp_",scenario_name,".out"))
unlink("summary.out")
unlink(paste0("summary_exp_",scenario_name,".out"))
unlink("vswc.out")
unlink(paste0("vswc_exp_",scenario_name,".out"))
unlink("wfps.out")
unlink(paste0("wfps_exp_",scenario_name,".out"))

# Experiment cropping schedule: full experimental period

if(mgmt_scenario_num %in% c(51,52,53) |
   mgmt_scenario_grp == 4) { # T1
  file.copy("site_53.100","site.100", overwrite=TRUE)
  file.copy("fix_53.100","fix.100", overwrite=TRUE)
  file.copy("soils_53.in", "soils.in", overwrite=TRUE)
} else if(mgmt_scenario_num %in% c(54,55,56)) { #T2
  file.copy("site_56.100","site.100", overwrite=TRUE)
  file.copy("fix_56.100","fix.100", overwrite=TRUE)
  file.copy("soils_56.in", "soils.in", overwrite=TRUE)
} else if(mgmt_scenario_grp==3) { #T3
  file.copy("site_3.100","site.100", overwrite=TRUE)
  file.copy("fix_3.100","fix.100", overwrite=TRUE)
  file.copy("soils_3.in", "soils.in", overwrite=TRUE)
} else stop(paste("Error-unknown mgmt_scenario_grp:",mgmt_scenario_grp))
file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE)

system(paste0(daycent_executable," -s sched_exp_",scenario_name,
              " -n sched_exp_",scenario_name,
              " -e sched_base2_",scenario_name), wait=TRUE)
system(paste0(daycent_list100," sched_exp_",scenario_name,
              " sched_exp_",scenario_name," outvars.txt"), wait=TRUE)

file.copy("cflows.out", paste0("cflows_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("dc_sip.csv", paste0("dc_sip_exp_",scenario_name,".csv"), overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_exp_",scenario_name,".csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("nflux.out", paste0("nflux_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("wfps.out", paste0("wfps_exp_",scenario_name,".out"), overwrite=TRUE)


# --------------- Reset working directory --------------- 
setwd(prior_path)

