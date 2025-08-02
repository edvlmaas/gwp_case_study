#######################################
# Function: 3_Create_management_input_files-Daycent_KBS.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Description: This procedure generates a scenario file for each phase of
# Daycent processing: base, experimental, and future periods. Weather
# and management are tailored to each scenario.
#
# Base period (from land conversion to the experimental period) is
# split into two legs to account primarily for changes in bulk density  
# over time.
#######################################

print(paste0("Starting 3_Create_management_input_files-Daycent_",site_name,".R"))


## equilibrium

#Note: Daycent spin-up schedule file was assembled manually: sched_eq.sch


# Base period (1) -------------------------------------------------------------


schedule_file1 <- paste0(daycent_path,"sched_base1.sch")

### 1850-1875 - start with continuous C1 corn ###

yrs_1850to1875 <- c("1850          Starting year ## start with assumed ground-breaking for agriculture until intensification",
                    "1920          Last year",
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
                    "C1            Initial crop ## low-yield corn",
                    "              Initial tree",
                    "",
                    "Year Month Option",
                    "1       Block ## Corn, low yield, no fertilizer",
                    "1875    Last year",
                    "1       Repeats # of years",
                    "1850    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "F       Weather choice",
                    "basic_eq.wth",
                    "1 111 CULT K     ## April 21 Moldboard plow",
                    "1 121 CULT H     ## May 1 Disk",
                    "1 121 CULT D     ## May 1 Cultivate",
                    "1 124 CROP C1    ## May 4",
                    "1 124 PLTM       ## May 4 Plant corn",
                    #"1 177 FERT (0.75N)	## June 26",
                    "1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

writeLines(yrs_1850to1875,schedule_file1)

### 1875-1900 - corn-soybean 2-year rotation, with gradually increasing productivity (in cultivars)

yrs_1875to1900 <- c("2       Block ## Higher-yielding corn with fertilizer",
                    "1900    Last year",
                    "1       Repeats # of years",
                    "1876    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue",
                    "1 111 CULT K     ## April 21 Moldboard plow",
                    "1 121 CULT H     ## May 1 Disk",
                    "1 121 CULT D     ## May 1 Cultivate",
                    "1 124 CROP C3    ## May 4",
                    "1 124 PLTM       ## May 4",
                    "1 177 FERT (1.5N)	## June 26",
                    "1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1875to1900,sep="\n",file=schedule_file1,append=TRUE)

### 1901-1920 - corn-soybean 2-year rotation, with gradually increasing productivity (in cultivars)

yrs_1901to1920 <- c("3       Block ## Higher-yielding corn with fertilizer",
                    "1920    Last year",
                    "1       Repeats # of years",
                    "1901    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue",
                    "1 111 CULT K     ## April 21 Moldboard plow",
                    "1 121 CULT H     ## May 1 Disk",
                    "1 121 CULT D     ## May 1 Cultivate",
                    "1 124 CROP C3    ## May 4",
                    "1 124 PLTM       ## May 4",
                    "1 177 FERT (2.2N)	## June 26",
                    "1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1901to1920,sep="\n",file=schedule_file1,append=TRUE)



# Base period (2) -------------------------------------------------------------

schedule_file2 <- paste0(daycent_path,"sched_base2.sch")

### 1921-1949 - corn-soybean 2-year rotation, with gradually increasing productivity (in cultivars)

yrs_1921to1949 <- c("1921          Starting year ## start with assumed ground-breaking for agriculture until intensification",
                    "1988          Last year",
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
                    "C4            Initial crop ## medium-yield corn",
                    "              Initial tree",
                    "",
                    "Year Month Option",
                    "4       Block ## Corn, low yield, no fertilizer",
                    "1949    Last year",
                    "1       Repeats # of years",
                    "1921    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "F       Weather choice",
                    "basic_eq.wth",
                    "1 111 CULT K     ## April 21 Moldboard plow",
                    "1 121 CULT H     ## May 1 Disk",
                    "1 121 CULT D     ## May 1 Cultivate",
                    "1 124 CROP C4    ## May 4",
                    "1 124 PLTM       ## May 4",
                    "1 177 FERT (2.2N)	## June 26",
                    "1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

writeLines(yrs_1921to1949,schedule_file2)


### 1950-1988 - corn-soybean 2-year rotation, with gradually increasing productivity (in cultivars)

yrs_1950to1959 <- c("5       Block ## Higher-yielding corn with fertilizer, add soybean to rotation",
                    "1959    Last year",
                    "2       Repeats # of years",
                    "1950    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "F       Weather choice ## Start weather over at 1950",
                    "basic_eq.wth",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN1 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "2 111 CULT K     ## April 21 Moldboard plow",
                    "2 121 CULT H     ## May 1 Disk",
                    "2 121 CULT D     ## May 1 Cultivate",
                    "2 124 CROP C4    ## May 4",
                    "2 124 PLTM       ## May 4",
                    "2 177 FERT (3.5N)	## June 26",
                    "2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1950to1959,sep="\n",file=schedule_file2,append=TRUE)

yrs_1960to1969 <- c("6       Block ## Higher-yielding corn with fertilizer, continue soybean",
                    "1969    Last year",
                    "2       Repeats # of years",
                    "1960    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue weather from previous block",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN1 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "2 111 CULT K     ## April 21 Moldboard plow",
                    "2 121 CULT H     ## May 1 Disk",
                    "2 121 CULT D     ## May 1 Cultivate",
                    "2 124 CROP C6    ## May 4",
                    "2 124 PLTM       ## May 4",
                    "2 177 FERT (4.5N)	## June 26",
                    "2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1960to1969,sep="\n",file=schedule_file2,append=TRUE)

yrs_1970to1979 <- c("7       Block ## Higher-yielding corn with fertilizer, continue soybean",
                    "1979    Last year",
                    "2       Repeats # of years",
                    "1970    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue weather from previous block",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN3 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "2 111 CULT K     ## April 21 Moldboard plow",
                    "2 121 CULT H     ## May 1 Disk",
                    "2 121 CULT D     ## May 1 Cultivate",
                    "2 124 CROP C6    ## May 4",
                    "2 124 PLTM       ## May 4",
                    "2 177 FERT (6N)	## June 26",
                    "2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1970to1979,sep="\n",file=schedule_file2,append=TRUE)

yrs_1980to1987 <- c("8       Block ## Higher-yielding corn with fertilizer, continue soybean",
                    "1987    Last year",
                    "2       Repeats # of years",
                    "1980    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue weather from previous block",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN3 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "2 111 CULT K     ## April 21 Moldboard plow",
                    "2 121 CULT H     ## May 1 Disk",
                    "2 121 CULT D     ## May 1 Cultivate",
                    "2 124 CROP C6    ## May 4",
                    "2 124 PLTM       ## May 4",
                    "2 177 FERT (8N)	## June 26",
                    "2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1980to1987,sep="\n",file=schedule_file2,append=TRUE)

yrs_1988 <- c("9       Block ## Just soybean",
                    "1988    Last year",
                    "1       Repeats # of years",
                    "1988    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue weather from previous block",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN5 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1988,sep="\n",file=schedule_file2,append=TRUE)



# Experimental period -----------------------------------------------------



schedule_file_exp <- paste0(daycent_path,"sched_exp_",scenario_name,".sch")

# remove duplicate/NA records
Daycent_data <- full_ops_ext_adj[!is.na(full_ops_ext_adj$daycent_mgmt_code),]

# Scenario treatment
temp_conv <- Daycent_data[Daycent_data$treatment==treatment,]

Daycent_conv <- temp_conv[rowSums(is.na(temp_conv)) != ncol(temp_conv),] %>%
  mutate(dayofyear=yday(date))

block_num <- 10

fileheader_txt <- c("1989          Starting year ## start of experimental period",
                    "2021          Last year",
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
                    "SYBN1         Initial crop ## soybean",
                    "              Initial tree",
                    "",
                    "Year Month Option")

cat(fileheader_txt,sep="\n",file=schedule_file_exp,append=FALSE)  

for (i in experiment_start_year:experiment_end_year) {
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
  } # if year=experiment_start_year
  else {
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
  } # else year is not 1989
  
} # for loop 1989-2021



# Future period -----------------------------------------------------------


# baseline climate

schedule_file_2100 <- paste0(daycent_path,"sched_fut_",scenario_name,".sch")

Daycent_conv_2100 <- Daycent_conv[Daycent_conv$year %in% (experiment_end_year-2):experiment_end_year,
                                c("date","daycent_mgmt_code","dayofyear")]  %>%
  mutate(
    # daycent_mgmt_code=if_else(daycent_mgmt_code=="CROP C8","CROP C7",
    #                                if_else(daycent_mgmt_code=="CROP W4EG","CROP W3EG",
    #                                        if_else(daycent_mgmt_code=="CROP SYBN","CROP SYBN1",
    #                                                daycent_mgmt_code))),
    ops_line=paste0(if_else(year(date)==experiment_end_year-2,"1 ",
                    if_else(year(date)==experiment_end_year-1,"2 ",
                                             "3 ")),dayofyear," ",daycent_mgmt_code))

header_txt <- c("2022          Starting year ## start with assumed ground-breaking for agriculture until intensification",
                "2099          Last year",
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
                "W4EG          Initial crop ## custom enhanced-growth wheat",
                "              Initial tree",
                "",
                "Year Month Option",
                "1       Block ## Corn, low yield, no fertilizer",
                "2099    Last year",
                "3       Repeats # of years",
                "2022    Output starting year",
                "12       Output month",
                "1  Output interval",
                "F       Weather choice",
                paste0("basic_",clim_scenario_num,".wth"))

ops_txt <- if(mgmt_scenario_grp==3) { #T3
  c(
    "1 85 FERT (3.4N)",
    "1 98 FERT (2.3N)",
    "1 98 FERT (5.8P)",
    "1 112 FERT (0.1N)",
    "1 126 FERT (3N)",
    "1 205 HARV G90S",
    "1 224 FERT (0.1N)",
    paste0("1 234 CROP ",covercrop_afterwheat_Daycent),
    "1 234 PLTM",
    "2 31 HARV KILL",
    paste0("2 128 CULT K",resid_adjust_chr),
    paste0("2 138 CULT D",resid_adjust_chr), # was 144
    "2 144 FERT (4.4N)", # was 147
    "2 144 FERT (3.4P)", # was 147
    "2 144 CROP C9", # was 147
    "2 144 PLTM", # was 147
    "2 177 FERT (0.3N)",
    "2 305 HARV G0S", # was 303
    paste0("2 321 CROP ",covercrop_aftercorn_Daycent),
    "2 321 PLTM",
    "3 32 HARV KILL",
    paste0("3 128 CULT K",resid_adjust_chr),
    "3 130 FERT (1N)",
    "3 130 FERT (2.7P)",
    paste0("3 139 CULT D",resid_adjust_chr),
    "3 150 CROP SYBN5", # was 168
    "3 150 PLTM", # was 168
    "3 175 FERT (0.1N)",
    "3 292 HARV G0S",
    "3 311 CROP W3",
    "3 311 PLTM"
  )
} else if(mgmt_scenario_grp == 4) { #T1
  c(
    paste0("1 85 FERT (",round(3.4*fert_adjust,2),"N)"),
    paste0("1 87 FERT (",round(2.4*fert_adjust,2),"N)"),
    paste0("1 87 FERT (",round(5*fert_adjust,2),"P)"),
    paste0("1 107 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("1 125 FERT (",round(1*fert_adjust,2),"N)"),
    paste0("1 125 FERT (",round(3.6*fert_adjust,2),"N)"),
    paste0("1 205 HARV G90S"),
    paste0("1 225 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("1 291 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("2 106 CULT H",resid_adjust_chr),
    paste0("2 126 CULT D",resid_adjust_chr),
    paste0("2 134 FERT (",round(3.4*fert_adjust,2),"N)"),
    paste0("2 134 FERT (",round(3.4*fert_adjust,2),"P)"),
    "2 134 CROP C6",
    "2 134 PLTM",
    paste0("2 170 FERT (",round(13.8*fert_adjust,2),"N)"),
    paste0("2 295 HARV G",resid_adjust_chr,"S"),
    paste0("3 130 FERT (",round(1.1*fert_adjust,2),"N)"),
    paste0("3 130 FERT (",round(2.8*fert_adjust,2),"P)"),
    paste0("3 130 CULT D",resid_adjust_chr),
    "3 134 CROP SYBN4",
    "3 134 PLTM",
    paste0("3 168 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("3 208 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("3 273 HARV G",resid_adjust_chr,"S"),
    paste0("3 274 CULT H",resid_adjust_chr),
    "3 313 CROP W3",
    "3 313 PLTM"
  )
} else if(mgmt_scenario_num %in% c(51,52,53)) { #T1
  c(
    paste0("1 85 FERT (",round(3.4*fert_adjust,2),"N)"),
    paste0("1 87 FERT (",round(2.4*fert_adjust,2),"N)"),
    paste0("1 87 FERT (",round(5*fert_adjust,2),"P)"),
    paste0("1 107 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("1 125 FERT (",round(1*fert_adjust,2),"N)"),
    paste0("1 125 FERT (",round(3.6*fert_adjust,2),"N)"),
    paste0("1 205 HARV G90S"),
    paste0("1 225 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("1 291 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("2 106 CULT H",resid_adjust_chr),
    paste0("2 126 CULT D",resid_adjust_chr),
    paste0("2 134 FERT (",round(3.4*fert_adjust,2),"N)"),
    paste0("2 134 FERT (",round(3.4*fert_adjust,2),"P)"),
    "2 134 CROP C6",
    "2 134 PLTM",
    paste0("2 170 FERT (",round(13.8*fert_adjust,2),"N)"),
    paste0("2 295 HARV G",resid_adjust_chr,"S"),
    paste0("3 130 FERT (",round(1.1*fert_adjust,2),"N)"),
    paste0("3 130 FERT (",round(2.8*fert_adjust,2),"P)"),
    paste0("3 130 CULT D",resid_adjust_chr),
    "3 134 CROP SYBN5",
    "3 134 PLTM",
    paste0("3 168 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("3 208 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("3 273 HARV G",resid_adjust_chr,"S"),
    paste0("3 274 CULT H",resid_adjust_chr),
    "3 313 CROP W3",
    "3 313 PLTM"
  )
  
} else if(mgmt_scenario_num %in% c(54,55,56)) { #T2
  c(
    paste0("1 84 FERT (",round(3.4*fert_adjust,2),"N)"),
    paste0("1 87 FERT (",round(2.4*fert_adjust,2),"N)"),
    paste0("1 87 FERT (",round(5*fert_adjust,2),"P)"),
    paste0("1 107 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("1 125 FERT (",round(1*fert_adjust,2),"N)"),
    paste0("1 125 FERT (",round(3.6*fert_adjust,2),"N)"),
    paste0("1 205 HARV G90S"),
    paste0("1 225 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("1 291 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("2 119 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("2 133 FERT (",round(3.4*fert_adjust,2),"N)"),
    paste0("2 133 FERT (",round(3.4*fert_adjust,2),"P)"),
    "2 133 CROP C65",
    "2 133 PLTM",
    paste0("2 170 FERT (",round(13.8*fert_adjust,2),"N)"),
    paste0("2 177 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("2 303 HARV G",resid_adjust_chr,"S"),
    paste0("3 130 FERT (",round(1.0*fert_adjust,2),"N)"),
    paste0("3 130 FERT (",round(2.6*fert_adjust,2),"P)"),
    "3 131 CROP SYBN5",
    "3 131 PLTM",
    paste0("3 208 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("3 273 HARV G",resid_adjust_chr,"S"),
    "3 291 CROP W3",
    "3 291 PLTM"
  )
} else stop(paste("Error-unknown mgmt_scenario_grp:",mgmt_scenario_grp))
    
footer_txt <- "-999 -999 X"

block_txt <- c(header_txt, ops_txt, footer_txt)

writeLines(block_txt,schedule_file_2100)


