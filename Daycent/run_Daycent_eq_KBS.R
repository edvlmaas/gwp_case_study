###########################################################################
# Script: run_Daycent_eq_KBS.R
# Author: Ellen Maas July 27, 2022
#         (Adapted from run_DD17CentEVI_Clovis_ds.R)
# Description: Simulates the equilibrium phase at the Kellogg Biological 
# Station, MI.
#
###########################################################################
# Audit Trail
# 9/27/2022: Rewrote to more generic time points, removed site name
# 12/13/2022: Reverted equilibrium run naming to remove scenario number.
###########################################################################

print("Starting run_Daycent_eq_KBS.R")

# Set the model path to the location of this site's files.
prior_path <- getwd()
model_path = paste0("~/CaseStudy/Daycent/",site_name,"/")
setwd(model_path)

# --------------- Run equilibrium simulation (4000-year spin-up) ---------------
#
# This generates native plant and soil conditions up to the point of conversion
# from the native ecosystem to historical cropping at the site.
#
# This equilibrium simulation takes a long time, so don't execute
# these commands if you already have an equilibrium binary file (*.bin)
# and you haven't made any changes to any parameter files.

# Equilibrium: 4000 years of grazed grassland
# Every scenario run uses the same equilibrium schedule file, so there is only one
print("**********Daycent equilibrium simulation*********")
unlink("soils.in")
unlink("site.100")
unlink("fix.100")
unlink("outfiles.in")
unlink(paste0("sched_eq.bin"))
unlink(paste0("sched_eq.lis"))


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
file.copy("outfiles_eq.in", "outfiles.in", overwrite=TRUE)

# -s parameter is the schedule file name, -n parameter is the binary output file name,
# minus the dot-extentions
system(paste0(daycent_executable," -s sched_eq -n sched_eq"), wait=TRUE)
system(paste0(daycent_list100," sched_eq sched_eq outvars.txt"), wait=TRUE)

# --------------- Reset working directory --------------- 
setwd(prior_path)

