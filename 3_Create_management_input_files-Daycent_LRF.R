#######################################
# Function: 3_Create_management_input_files-Daycent_LRF.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Description: This procedure generates scenario files for each phase of
# Daycent processing: base, experimental, and future periods. Weather
# and management are tailored to each scenario.
#######################################

suppressMessages({
  
print(paste0("Starting 3_Create_management_input_files-Daycent_",site_name,".R"))

####  NOTE: Equilibrium schedule file is manually edited: sched_eq.sch



###########################
## base period

#Note: Daycent spin-up schedule file was assembled manually: sched_eq.sch

## base

schedule_file <- paste0(daycent_path,"sched_base.sch")

### 1940-1959 - start with continuous sorghum ###

init_base1 <- c(paste0(land_conversion_year,"          Starting year ## start with assumed ground-breaking for agriculture until intensification"),
                paste0(experiment_start_year-1,"          Last year"),
                "site.100  Site file name",
                "0             Labeling type ## all defaults turned off",
                "-1            Labeling year",
                "-1.00         Microcosm",
                "-1            CO2 Systems",
                "-1            pH effect",
                "-1            Soil warming",
                "0             N input scalar option (0 or 1)",
                "0             OMAD scalar option (0 or 1)",
                "0             Climate scalar option",
                "1             Initial system",
                "GI5          Initial crop ## sorghum",
                "              Initial tree",
                "",
                "Year Month Option",
                "1       Block ## Sorghum, low yield, no fertilizer",
                "1959    Last year",
                "1       Repeats # of years",
                "1940    Output starting year",
                "12      Output month",
                "1       Output interval",
                "F       Weather choice",
                "basic_eq.wth",
                "1 89 CULT K			## Mar 30",
                "1 120 CULT I		## Apr 30",
                "1 145 CULT C		## May 25",
                "1 161 CROP SORG1	## Jun 10 - Grain sorghum",
                "1 161 PLTM 			## Jun 10",
                #"1 161 FERT (16.8N)	## Jun 10",
                "1 319 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                "-999 -999 X")

writeLines(init_base1,schedule_file)

### 1960-2001 - cotton

init_base2 <- c("2       Block ## Switch to cotton",
                "1979    Last year",
                "1       Repeats # of years",
                "1960    Output starting year",
                "12      Output month",
                "1       Output interval",
                "C       Weather choice ## Continue",
                "1 118 CULT K  ## Apr 28", 
                "1 140 CULT ROW  ## May 20",
                "1 140 CROP COTL ## May 20",
                "1 140 PLTM  ## May 20",
                "1 290 HARV G90S  ## Oct 17", 
                "-999 -999 X")

cat(init_base2,sep="\n",file=schedule_file,append=TRUE)

init_base3 <- c("3       Block ## Switch to cotton",
                "2001    Last year",
                "1       Repeats # of years",
                "1980    Output starting year",
                "12      Output month",
                "1       Output interval",
                "C       Weather choice ## Continue",
                "1 118 CULT K  ## Apr 28", 
                "1 118 FERT (1N)  ## Apr 28",
                "1 140 CULT ROW  ## May 20",
                "1 140 CROP COTL ## May 20",
                "1 140 PLTM  ## May 20",
                "1 290 HARV G90S  ## Oct 17", 
                "-999 -999 X")

cat(init_base3,sep="\n",file=schedule_file,append=TRUE)

### 2002 - rye after cotton

init_base4 <- c("4       Block ## All include rye at end as transistion year",
                "2002    Last year",
                "1       Repeats # of years",
                "2002    Output starting year",
                "12      Output month",
                "1       Output interval",
                "C       Weather choice ## Continue",
                "1 118 CULT K  ## Apr 28", 
                "1 118 FERT (2.2N)  ## Apr 28",
                "1 140 CULT ROW  ## May 20",
                "1 140 CROP COT ## May 20",
                "1 140 PLTM  ## May 20",
                "1 290 HARV G90S  ## Oct 17", 
                paste0("1 341 CROP ",covercrop_Daycent),
                "-999 -999 X")

cat(init_base4,sep="\n",file=schedule_file,append=TRUE)



###########################
## experimental period


schedule_file_exp <- paste0(daycent_path,"sched_exp_",scenario_name,".sch")

# remove duplicate/NA records
Daycent_data <- full_ops_ext_adj[!is.na(full_ops_ext_adj$daycent_mgmt_code),]

# Scenario treatment
temp_conv <- Daycent_data[Daycent_data$treatment==treatment,]

Daycent_conv <- temp_conv[rowSums(is.na(temp_conv)) != ncol(temp_conv),] %>%
  mutate(dayofyear=yday(date))

block_num <- 10

fileheader_txt <- c(paste0(experiment_start_year,"          Starting year ## start of experimental period for ",treatment),
                    paste0(end_exp_period_year,"          Last year"),
                    "site.100  Site file name",
                    "0             Labeling type ## all defaults turned off",
                    "-1            Labeling year",
                    "-1.00         Microcosm",
                    "-1            CO2 Systems",
                    "-1            pH effect",
                    "-1            Soil warming",
                    "0             N input scalar option (0 or 1)",
                    "0             OMAD scalar option (0 or 1)",
                    "0             Climate scalar option",
                    "1             Initial system",
                    "COT           Initial crop ## cotton",
                    "              Initial tree",
                    "",
                    "Year Month Option")

cat(fileheader_txt,sep="\n",file=schedule_file_exp,append=FALSE)  

for (i in experiment_start_year:end_exp_period_year) {
  ## first year starts weather file; subsequent years continue weather file
  if (i==experiment_start_year) {
    curr_yr_ops <- Daycent_conv[Daycent_conv$year==i,c("date","daycent_mgmt_code","dayofyear")] %>%
      mutate(ops_line=paste0("1 ",dayofyear," ",daycent_mgmt_code))
    
    header_txt <- c(
      paste(block_num,"Block ## Experimental period",sep="\t"),
      paste(i,"Last year",sep="\t"),
      "1  Repeats # of years",
      paste(i,"Output starting year",sep="\t"),
      "12  Output starting month",
      "1  Output interval",
      "F 			 Weather choice",
      "basic_exp.wth")
    
    ops_txt <- curr_yr_ops$ops_line
    
    footer_txt <- "-999 -999 X"
    
    block_txt <- c(header_txt, ops_txt, footer_txt)  
    
    cat(block_txt,sep="\n",file=schedule_file_exp,append=TRUE)  
    block_num <- block_num + 1  
  } # end if year=experiment_start_year
  else if (i>experiment_start_year & i<=experiment_end_year) {
    curr_yr_ops <- Daycent_conv[Daycent_conv$year==i,c("date","daycent_mgmt_code","dayofyear")] %>%
      mutate(ops_line=paste0("1 ",dayofyear," ",daycent_mgmt_code))
    
    header_txt <- c(
      paste(block_num,"Block ## Experimental period",sep="\t"),
      paste(i,"Last year",sep="\t"),
      "1  Repeats # of years",
      paste(i,"Output starting year",sep="\t"),
      "12  Output starting month",
      "1  Output interval",
      "C 			 Weather choice")
    
    ops_txt <- curr_yr_ops$ops_line
    
    footer_txt <- "-999 -999 X"
    
    block_txt <- c(header_txt, ops_txt, footer_txt)  
    
    cat(block_txt,sep="\n",file=schedule_file_exp,append=TRUE)  
    block_num <- block_num + 1  
  } # end else year is in experiment year range
  else { # Extended experimental period to pad through 2021
    if(mgmt_scenario_grp %in% c(3,4,5,8)) {
      ### use 2004 and 2005 because sorghum dropped out in 2007
      if(i %% 2==0) {
        curr_yr_ops <- rbind(Daycent_conv[Daycent_conv$year==2005,],
                             Daycent_conv[Daycent_conv$date==as.Date("2009-12-13"),]) %>%
          mutate(ops_line=paste0("1 ",dayofyear," ",daycent_mgmt_code))
        } else {
            curr_yr_ops <- Daycent_conv[Daycent_conv$year==2004,] %>%
              mutate(ops_line=paste0("1 ",dayofyear," ",daycent_mgmt_code))
        }
      curr_yr_ops$year <- i
      curr_yr_ops$date <- as.Date(paste0(i,substr(as.character(curr_yr_ops$date),5,10)))
    } else {
    ### Use last 2 years and repeat
    curr_yr_ops <- Daycent_conv[Daycent_conv$year==ifelse(i %% 2==1, (experiment_end_year-1),
                                                          experiment_end_year),
                                c("date","daycent_mgmt_code","dayofyear")] %>%
      mutate(ops_line=paste0("1 ",dayofyear," ",daycent_mgmt_code))
    }
    
    header_txt <- c(
      paste(block_num,"Block ## Experimental period",sep="\t"),
      paste(i,"Last year",sep="\t"),
      "1  Repeats # of years",
      paste(i,"Output starting year",sep="\t"),
      "12  Output starting month",
      "1  Output interval",
      "C 			 Weather choice")
    
    ops_txt <- curr_yr_ops$ops_line
    # ### Need to remove final Rye planting in 2021 as the future 2-cycle doesn't
    # ### start with a kill event (no cover crop planted at end of cycle).
    # if(i==end_exp_period_year & mgmt_scenario_grp %in% c(3,8)) {
    #   ops_txt <- ops_txt[1:(length(ops_txt)-2)]
    # }
    
    footer_txt <- "-999 -999 X"
    
    block_txt <- c(header_txt, ops_txt, footer_txt)  
    
    cat(block_txt,sep="\n",file=schedule_file_exp,append=TRUE)  
    block_num <- block_num + 1  
  } # end extended period
  
} # for loop 2003-2010



###########################
## Daycent future
### Use last 2 years management and repeat


schedule_file_2100 <- paste0(daycent_path,"sched_fut_",scenario_name,".sch")

Daycent_conv_2100 <- Daycent_conv[Daycent_conv$year %in% (experiment_end_year-1):experiment_end_year,
                                  c("date","daycent_mgmt_code","dayofyear")]  %>%
  mutate(
    # daycent_mgmt_code=if_else(daycent_mgmt_code=="CROP C8","CROP C7",
    #                                if_else(daycent_mgmt_code=="CROP W4EG","CROP W3EG",
    #                                        if_else(daycent_mgmt_code=="CROP SYBN","CROP SYBN1",
    #                                                daycent_mgmt_code))),
    ops_line=paste0(if_else(year(date)==experiment_end_year-1,"1 ",
                            "2 ")),dayofyear," ",daycent_mgmt_code)

header_txt <- c(paste0(end_exp_period_year+1,"          "),
                paste0(max_fut_period_year-1,"          Last year"),
                "site.100      Site file name",
                "0             Labeling type ## all defaults turned off",
                "-1            Labeling year",
                "-1.00         Microcosm",
                "-1            CO2 Systems",
                "-1            pH effect",
                "-1            Soil warming",
                "0             N input scalar option (0 or 1)",
                "0             OMAD scalar option (0 or 1)",
                "0             Climate scalar option",
                "1             Initial system",
                "SORG          Initial crop ## cotton, but could be sorghum, depending on treatment",
                "              Initial tree",
                "",
                "Year Month Option",
                "1       Block ## Corn, low yield, no fertilizer",
                paste0(max_fut_period_year-1,"    Last year"),
                "2       Repeats # of years",
                paste0(end_exp_period_year+1,"    Output starting year"),
                "12       Output month",
                "1  Output interval",
                "F       Weather choice",
                paste0("basic_",clim_scenario_num,".wth"))

ops_txt <- if(mgmt_scenario_num %in% c(51,52,53) |
              mgmt_scenario_grp == 4) { #CSct
  c(
    paste0("1 71 FERT (",round(2.8*fert_adjust,2),"N)"),
    paste0("1 80 CULT K",resid_adjust_chr),
    "1 140 CROP COT",
    "1 140 PLTM",
    paste0("1 323 HARV G",resid_adjust_chr,"S"),
    paste0("2 17 CULT K",resid_adjust_chr),
    paste0("2 119 FERT (",round(2.8*fert_adjust,2),"N)"),
    "2 133 CROP SORG",
    "2 133 PLTM",
    paste0("2 315 HARV G",resid_adjust_chr,"S")
  )
} else if(mgmt_scenario_num %in% c(54,55,56)) { #CSnt
  c(
    paste0("1 71 FERT (",round(2.8*fert_adjust,2),"N)"),
           "1 140 CROP COT",
           "1 140 PLTM",
    paste0("1 323 HARV G",resid_adjust_chr,"S"),
    paste0("2 119 FERT (",round(2.8*fert_adjust,2),"N)"),
    "2 133 CROP SORG",
    "2 133 PLTM",
    paste0("2 315 HARV G",resid_adjust_chr,"S")
  )
} else if(mgmt_scenario_grp==3) { #CRSct
  c(
    "1 97 HARV KILL",
    paste0("1 115 FERT (",round(2.8*fert_adjust,2),"N)"),
    "1 115 CULT K",
    "1 138 CULT R",
    "1 139 CROP COT",
    "1 139 PLTM",
    paste0("1 287 HARV G",resid_adjust_chr,"S"),
    "1 347 CROP RGA",
    "1 347 PLTM",
    "2 98  HARV KILL",
    paste0("2 119 FERT (",round(2.8*fert_adjust,2),"N)"),
    "2 119 CULT K",
    "2 131 CULT R",
    "2 135 CROP SORG",
    "2 135 PLTM",
    paste0("2 306 HARV G",resid_adjust_chr,"S"),
    "2 355 CULT K",
    "2 355 CROP RGA",
    "2 355 PLTM"
  )
} else if(mgmt_scenario_grp==7) { #CCct
  c(
    paste0("1 71 FERT (",round(2.8*fert_adjust,2),"N)"),
    paste0("1 80 CULT K",resid_adjust_chr),
    "1 140 CROP COT",
    "1 140 PLTM",
    paste0("1 323 HARV G",resid_adjust_chr,"S"),
    paste0("2 17 CULT K",resid_adjust_chr),
    "2 133 CROP COT",
    "2 133 PLTM",
    paste0("2 315 HARV G",resid_adjust_chr,"S")
  )
} else if(mgmt_scenario_grp==8) { #CRSnt
  c(
    "1 97 HARV KILL",
    paste0("1 115 FERT (",round(2.8*fert_adjust,2),"N)"),
    "1 139 CROP COT",
    "1 139 PLTM",
    paste0("1 287 HARV G",resid_adjust_chr,"S"),
    "1 347 CROP RGA",
    "1 347 PLTM",
    "2 98 HARV KILL",
    paste0("2 119 FERT (",round(2.8*fert_adjust,2),"N)"),
    "2 135 CROP SORG",
    "2 135 PLTM",
    paste0("2 306 HARV G",resid_adjust_chr,"S"),
    "2 355 CROP RGA",
    "2 355 PLTM"
  )
}

footer_txt <- "-999 -999 X"

fut_block_txt <- c(header_txt, ops_txt, footer_txt)

writeLines(fut_block_txt,schedule_file_2100)

}) # end suppressMessages

