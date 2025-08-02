#######################################
# Script: 9_Results_RothC-future.R
# Author: Ellen Maas
# Date: 8/30/2022
# Description: Runs graphs for the future period for the RothC simulation.
# Because there is no difference in variable naming or options between
# sites, there are not separate files for each site.
#######################################

suppressMessages({
  
print("Starting 9_Results_RothC-future.R")

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)


#**********************************************************************

## experimental -> future period

  
#Cfit_RothC <- coef(lm(RothC ~ year, data = Cstock_Mgha))
#Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha))

gC2 <- Cstock_Mgha_piv[Cstock_Mgha_piv$year>=experiment_start_year,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon: Scenario ",clim_scenario_num,"_",mgmt_scenario_num)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
#  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("RothC","Observed"),
                     values=c(RothC_color,Observed_color)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_blank())

gC2 


ggsave(filename=paste0(results_path,"SOC_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_RothC.jpg"),
       plot=gC2, width=9, height=6, dpi=300)


}) # end suppressMessages
