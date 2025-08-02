#######################################
# Name: p_Results_analysis.R
# Author: Ellen Maas
# Date: Nov. 4, 2022
# Output: This script calculates summary data for analysis 
# after a model run and calls p_Edit_output_file to edit
# the output file.
#######################################

print("Starting p_Results_analysis.R")

SOC_end_year <- Cstock_Mgha[nrow(Cstock_Mgha),"year"]

SOC_diff_fut <- Cstock_Mgha[Cstock_Mgha$year==SOC_end_year,model_name] - 
  Cstock_Mgha[Cstock_Mgha$year==end_exp_period_year+1,model_name]

MaizeYld_diff_fut <- NA

SoyYld_diff_fut <- NA

WheatYld_diff_fut <- NA

CottonYld_diff_fut <- NA

SorghumYld_diff_fut <- NA

N2O_total_fut <- NA

N2O_diff_fut <- NA

CH4_total_fut <- NA

CH4_diff_fut <- NA

SOC_total_fut <- Cstock_Mgha[Cstock_Mgha$year==SOC_end_year,model_name]

if(model_name %in% c("APSIM","Daycent","LDNDC")) {
  
  if(site_name=="KBS") {
    MaizeYld_diff_fut <- MYys[2]-MYys[1]
    SoyYld_diff_fut <- SYys[2]-SYys[1]
    WheatYld_diff_fut <- WYys[2]-WYys[1]
    CottonYld_diff_fut <- NA
    SorghumYld_diff_fut <- NA
  } else if(site_name=="LRF") {
    MaizeYld_diff_fut <- NA
    SoyYld_diff_fut <- NA
    WheatYld_diff_fut <- NA
    CottonYld_diff_fut <- CYys[2]-CYys[1]
    SorghumYld_diff_fut <- ifelse(mgmt_scenario_grp!=7,SYys[2]-SYys[1],NA)
  }
  
  N2O_total_fut <- sum(N2O_ghaday[N2O_ghaday$date>experiment_end_date,model_name])/1000
  
  N2O_diff_fut <- NGys[2]/1000-NGys[1]/1000
  
} # end if - all but SOC and methane

if(model_name %in% c("Daycent","LDNDC")) {
  
#  CH4_end_year <- MaizeYld_Mgha[nrow(MaizeYld_Mgha),"year"]
  CH4_total_fut <- sum(CH4_ghaday[CH4_ghaday$date>experiment_end_date,model_name])/1000
  
  CH4_diff_fut <- MGys[2]/1000-MGys[1]/1000
  
} # end if - methane


summary_tab <- cbind(model_name,clim_scenario_num,mgmt_scenario_num,
                     scenario_name) %>% 
  cbind(MaizeYld_diff_fut,SoyYld_diff_fut,WheatYld_diff_fut,
        SOC_diff_fut,N2O_total_fut,N2O_diff_fut,CH4_total_fut,CH4_diff_fut,
        SOC_total_fut,CottonYld_diff_fut,SorghumYld_diff_fut)


# call function to edit the output file
source("p_Edit_output_file.R")
p_Edit_output_file(summary_tab,model_name,scenario_name)


