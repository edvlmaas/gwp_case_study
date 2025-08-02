#######################################
# Script: p_Future_weather_reanalysis.R
# Author: Ellen Maas
# Date: 12/7/2022
# Description: This procedure Compares the observed historical weather data with 
# each future projection scenario to see how closely it matches and adjusts the
# bias by performing linear regression on each weather element (min and max
# temperature and precipitation) and removing the difference between where the 
# observed record ends (at 2021) and the future projection begins (at 2022).
# **NOTE:  For additional statistical code, see also 
# 'Weather comparisons_NOAA_PRISM','Perform_Weather_Analysis-NM2'
# '2_Correlate Kalamazoo and Detroit weather' and 'Perform_Weather_Analysis'.
#######################################

suppressMessages({
  
library(readxl)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(lubridate)
library(dplyr)

cbPalette9 <- c("#000000","#0072B2","#009E73","#56B4E9","#999999",
                "#CC79A7","#D55E00","#E69F00","#F0E442")


#**********************************************************************
# Import and prepare data

# Historical

## Use Daycent weather for historical, over the experimental period
hist_exp <- read.table(paste0(daycent_path,"basic_exp.wth"))
colnames(hist_exp) <- c("day","month","year","dayofyear","h_tmax","h_tmin","h_rain_cm")
hist_exp$date=make_date(hist_exp$year, hist_exp$month, hist_exp$day)

# Future

## Use Daycent weather for historical, over the experimental period
baseln <- read.table(paste0(daycent_path,"basic_1.wth"))
colnames(baseln) <- c("day","month","year","dayofyear","bl_tmax","bl_tmin","bl_rain_cm")
baseln <- baseln[baseln$year<=end_fut_period_year,]
baseln$date=make_date(baseln$year, baseln$month, baseln$day)

## CMIPs
gfdl_low_raw <- read.csv(paste0(fut_weather_path,"fut_clim_scenario_2.csv")) 
gfdl_low <- gfdl_low_raw %>%
  select(day,month,year,dayofyear,mint_C,maxt_C,rain_cm)
colnames(gfdl_low) <- c("day","month","year","dayofyear","gl_tmin","gl_tmax","gl_rain_cm")
gfdl_low$date=make_date(gfdl_low$year, gfdl_low$month, gfdl_low$day)

gfdl_high_raw <- read.csv(paste0(fut_weather_path,"fut_clim_scenario_3.csv")) 
gfdl_high <- gfdl_high_raw %>%
  select(day,month,year,dayofyear,mint_C,maxt_C,rain_cm)
colnames(gfdl_high) <- c("day","month","year","dayofyear","gh_tmin","gh_tmax","gh_rain_cm")
gfdl_high$date=make_date(gfdl_high$year, gfdl_high$month, gfdl_high$day)

ukesm_low_raw <- read.csv(paste0(fut_weather_path,"fut_clim_scenario_4.csv")) 
ukesm_low <- ukesm_low_raw %>%
  select(day,month,year,dayofyear,mint_C,maxt_C,rain_cm)
colnames(ukesm_low) <- c("day","month","year","dayofyear","ul_tmin","ul_tmax","ul_rain_cm")
ukesm_low$date=make_date(ukesm_low$year, ukesm_low$month, ukesm_low$day)

ukesm_high_raw <- read.csv(paste0(fut_weather_path,"fut_clim_scenario_5.csv")) 
ukesm_high <- ukesm_high_raw %>%
  select(day,month,year,dayofyear,mint_C,maxt_C,rain_cm)
colnames(ukesm_high) <- c("day","month","year","dayofyear","uh_tmin","uh_tmax","uh_rain_cm")
ukesm_high$date=make_date(ukesm_high$year, ukesm_high$month, ukesm_high$day)

#**********************************************************************
# Minimum Temperature
#**********************************************************************

## setup

tmin_df <- merge(merge(merge(merge(merge(hist_exp[,c("date","h_tmin")],
                                         baseln[,c("date","bl_tmin")],
                                         by="date",
                                         all=TRUE),
                                   gfdl_low[,c("date","gl_tmin")],
                                   by="date",
                                   all=TRUE),
                             gfdl_high[,c("date","gh_tmin")],
                             by="date",
                             all=TRUE),
                       ukesm_low[,c("date","ul_tmin")],
                       by="date",
                       all=TRUE),
                 ukesm_high[,c("date","uh_tmin")],
                 by="date",
                 all=TRUE)
colnames(tmin_df) <- c("Date","Historical","Baseline","GFDL_Low","GFDL_High",
                       "UKESM_Low","UKESM_High")

tmin_piv <- pivot_longer(tmin_df, c(-Date),
                                 names_to = "source",
                                 values_to = "tmin_val")

## calculate linear models

  fit_hist <- coef(lm(h_tmin ~ year, 
                         data = hist_exp))
  fit_baseln <- coef(lm(bl_tmin ~ year, 
                         data = baseln))
  fit_gfdl_low <- coef(lm(gl_tmin ~ year, 
                         data = gfdl_low))
  fit_gfdl_high <- coef(lm(gh_tmin ~ year, 
                         data = gfdl_high))
  fit_ukesm_low <- coef(lm(ul_tmin ~ year, 
                         data = ukesm_low))
  fit_ukesm_high <- coef(lm(uh_tmin ~ year, 
                         data = ukesm_high))
  hist_xs <- c(experiment_start_year, experiment_end_year)
  hist_ys <- cbind(1, hist_xs) %*% fit_hist
  baseln_xs <- c(end_exp_period_year+1, end_fut_period_year)
  baseln_ys <- cbind(1, baseln_xs) %*% fit_baseln
  gfdl_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
  gfdl_low_ys <- cbind(1, gfdl_low_xs) %*% fit_gfdl_low
  gfdl_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
  gfdl_high_ys <- cbind(1, gfdl_high_xs) %*% fit_gfdl_high
  ukesm_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
  ukesm_low_ys <- cbind(1, ukesm_low_xs) %*% fit_ukesm_low
  ukesm_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
  ukesm_high_ys <- cbind(1, ukesm_high_xs) %*% fit_ukesm_high

## graph just linear models
  
  pLM <- tmin_df %>%
    ggplot(aes(x=date, y=h_tmin)) +
    geom_segment(aes(x = hist_xs[1], xend = hist_xs[2], 
                     y = hist_ys[1], yend = hist_ys[2], color='Historical')) +
    geom_segment(aes(x = baseln_xs[1], xend = baseln_xs[2], 
                     y = baseln_ys[1], yend = baseln_ys[2], color="Baseline")) +
    geom_segment(aes(x = gfdl_low_xs[1], xend = gfdl_low_xs[2], 
                     y = gfdl_low_ys[1], yend = gfdl_low_ys[2], color="GFDL Low")) +
    geom_segment(aes(x = gfdl_high_xs[1], xend = gfdl_high_xs[2], 
                     y = gfdl_high_ys[1], yend = gfdl_high_ys[2], color="GFDL High")) +
    geom_segment(aes(x = ukesm_low_xs[1], xend = ukesm_low_xs[2], 
                     y = ukesm_low_ys[1], yend = ukesm_low_ys[2], color="UKESM Low")) +
    geom_segment(aes(x = ukesm_high_xs[1], xend = ukesm_high_xs[2], 
                     y = ukesm_high_ys[1], yend = ukesm_high_ys[2], color="UKESM High")) +
    ggtitle(paste(site_name,"Minimum Temperature")) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

pLM

ggsave(filename=paste0(fut_weather_path,"Minimum Temperature linear models.jpg"),plot=pLM)


## graph each source compared to historical observations

### baseline
pBL <- tmin_piv[tmin_piv$source %in% c("Historical","Baseline"),] %>%
  ggplot(aes(x=Date,y=tmin_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(baseln_xs[1],"-01-01")), 
                   xend = as.Date(paste0(baseln_xs[2],"-01-01")), 
                   y = baseln_ys[1], yend = baseln_ys[2]), color=cbPalette9[9])  +
  ggtitle(paste(site_name,"Minimum Temperature - Baseline")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pBL


### GFDL low
pGL <- tmin_piv[tmin_piv$source %in% c("Historical","GFDL_Low"),] %>%
  ggplot(aes(x=Date,y=tmin_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(gfdl_low_xs[1],"-01-01")), 
                   xend = as.Date(paste0(gfdl_low_xs[2],"-01-01")), 
                   y = gfdl_low_ys[1], yend = gfdl_low_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"Minimum Temperature - GFDL Low")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pGL

### GFDL high
pGH <- tmin_piv[tmin_piv$source %in% c("Historical","GFDL_High"),] %>%
  ggplot(aes(x=Date,y=tmin_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(gfdl_high_xs[1],"-01-01")), 
                   xend = as.Date(paste0(gfdl_high_xs[2],"-01-01")), 
                   y = gfdl_high_ys[1], yend = gfdl_high_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"Minimum Temperature - GFDL High")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right") 
pGH

### UKESM low
pUL <- tmin_piv[tmin_piv$source %in% c("Historical","UKESM_Low"),] %>%
  ggplot(aes(x=Date,y=tmin_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(ukesm_low_xs[1],"-01-01")), 
                   xend = as.Date(paste0(ukesm_low_xs[2],"-01-01")), 
                   y = ukesm_low_ys[1], yend = ukesm_low_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"Minimum Temperature - UKESM Low")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pUL

### UKESM high
pUH <- tmin_piv[tmin_piv$source %in% c("Historical","UKESM_High"),] %>%
  ggplot(aes(x=Date,y=tmin_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(ukesm_high_xs[1],"-01-01")), 
                   xend = as.Date(paste0(ukesm_high_xs[2],"-01-01")), 
                   y = ukesm_high_ys[1], yend = ukesm_high_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"Minimum Temperature - UKESM High")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pUH

ggsave(filename=paste0(fut_weather_path,"Minimum Temperature-hist vs baseline.jpg"),plot=pBL)
ggsave(filename=paste0(fut_weather_path,"Minimum Temperature-hist vs gfdl low.jpg"),plot=pGL)
ggsave(filename=paste0(fut_weather_path,"Minimum Temperature-hist vs gfdl high.jpg"),plot=pGH)
ggsave(filename=paste0(fut_weather_path,"Minimum Temperature-hist vs ukesm low.jpg"),plot=pUL)
ggsave(filename=paste0(fut_weather_path,"Minimum Temperature-hist vs ukesm high.jpg"),plot=pUH)


#########################
# Reanalysis for Min Temp
#########################

# adjust every value by the difference between where the historical mean (the
# linear regression) ends and the future values start.
#
# baseline won't be touched as it just repeats historical data

diff_gfdl_low <- hist_ys[2] - gfdl_low_ys[1]
diff_gfdl_high <- hist_ys[2] - gfdl_high_ys[1]
diff_ukesm_low <- hist_ys[2] - ukesm_low_ys[1]
diff_ukesm_high <- hist_ys[2] - ukesm_high_ys[1]

gfdl_low_reanal <- gfdl_low_raw
gfdl_low_reanal$mint_C <- gfdl_low_raw$mint_C + diff_gfdl_low
gfdl_high_reanal <- gfdl_high_raw
gfdl_high_reanal$mint_C <- gfdl_high_raw$mint_C + diff_gfdl_high
ukesm_low_reanal <- ukesm_low_raw
ukesm_low_reanal$mint_C <- ukesm_low_raw$mint_C + diff_ukesm_low
ukesm_high_reanal <- ukesm_high_raw
ukesm_high_reanal$mint_C <- ukesm_high_raw$mint_C + diff_ukesm_high


#**********************************************************************
# Maximum Temperature
#**********************************************************************

## setup

tmax_df <- merge(merge(merge(merge(merge(hist_exp[,c("date","h_tmax")],
                                         baseln[,c("date","bl_tmax")],
                                         by="date",
                                         all=TRUE),
                                   gfdl_low[,c("date","gl_tmax")],
                                   by="date",
                                   all=TRUE),
                             gfdl_high[,c("date","gh_tmax")],
                             by="date",
                             all=TRUE),
                       ukesm_low[,c("date","ul_tmax")],
                       by="date",
                       all=TRUE),
                 ukesm_high[,c("date","uh_tmax")],
                 by="date",
                 all=TRUE)
colnames(tmax_df) <- c("Date","Historical","Baseline","GFDL_Low","GFDL_High",
                       "UKESM_Low","UKESM_High")

tmax_piv <- pivot_longer(tmax_df, c(-Date),
                                 names_to = "source",
                                 values_to = "tmax_val")

## calculate linear models

  fit_hist <- coef(lm(h_tmax ~ year, 
                         data = hist_exp))
  fit_baseln <- coef(lm(bl_tmax ~ year, 
                         data = baseln))
  fit_gfdl_low <- coef(lm(gl_tmax ~ year, 
                         data = gfdl_low))
  fit_gfdl_high <- coef(lm(gh_tmax ~ year, 
                         data = gfdl_high))
  fit_ukesm_low <- coef(lm(ul_tmax ~ year, 
                         data = ukesm_low))
  fit_ukesm_high <- coef(lm(uh_tmax ~ year, 
                         data = ukesm_high))
  hist_xs <- c(experiment_start_year, experiment_end_year)
  hist_ys <- cbind(1, hist_xs) %*% fit_hist
  baseln_xs <- c(end_exp_period_year+1, end_fut_period_year)
  baseln_ys <- cbind(1, baseln_xs) %*% fit_baseln
  gfdl_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
  gfdl_low_ys <- cbind(1, gfdl_low_xs) %*% fit_gfdl_low
  gfdl_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
  gfdl_high_ys <- cbind(1, gfdl_high_xs) %*% fit_gfdl_high
  ukesm_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
  ukesm_low_ys <- cbind(1, ukesm_low_xs) %*% fit_ukesm_low
  ukesm_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
  ukesm_high_ys <- cbind(1, ukesm_high_xs) %*% fit_ukesm_high

## graph just linear models
  
  pLM <- tmax_df %>%
    ggplot(aes(x=date, y=h_tmax)) +
        geom_segment(aes(x = hist_xs[1], xend = hist_xs[2], 
                         y = hist_ys[1], yend = hist_ys[2], color="Historical")) +
    geom_segment(aes(x = baseln_xs[1], xend = baseln_xs[2], 
                     y = baseln_ys[1], yend = baseln_ys[2], color="Baseline")) +
    geom_segment(aes(x = gfdl_low_xs[1], xend = gfdl_low_xs[2], 
                     y = gfdl_low_ys[1], yend = gfdl_low_ys[2], color="GFDL Low")) +
    geom_segment(aes(x = gfdl_high_xs[1], xend = gfdl_high_xs[2], 
                     y = gfdl_high_ys[1], yend = gfdl_high_ys[2], color="GFDL High")) +
    geom_segment(aes(x = ukesm_low_xs[1], xend = ukesm_low_xs[2], 
                     y = ukesm_low_ys[1], yend = ukesm_low_ys[2], color="UKESM Low")) +
    geom_segment(aes(x = ukesm_high_xs[1], xend = ukesm_high_xs[2], 
                     y = ukesm_high_ys[1], yend = ukesm_high_ys[2], color="UKESM High")) +
    ggtitle(paste(site_name,"Maximum Temperature")) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())


pLM

ggsave(filename=paste0(fut_weather_path,"Maximum Temperature linear models.jpg"),plot=pLM)


## graph each source compared to historical observations

### baseline
pBL <- tmax_piv[tmax_piv$source %in% c("Historical","Baseline"),] %>%
  ggplot(aes(x=Date,y=tmax_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(baseln_xs[1],"-01-01")), 
                   xend = as.Date(paste0(baseln_xs[2],"-01-01")), 
                   y = baseln_ys[1], yend = baseln_ys[2]), color=cbPalette9[9])  +
  ggtitle(paste(site_name,"maximum Temperature - Baseline")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pBL


### GFDL low
pGL <- tmax_piv[tmax_piv$source %in% c("Historical","GFDL_Low"),] %>%
  ggplot(aes(x=Date,y=tmax_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(gfdl_low_xs[1],"-01-01")), 
                   xend = as.Date(paste0(gfdl_low_xs[2],"-01-01")), 
                   y = gfdl_low_ys[1], yend = gfdl_low_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"maximum Temperature - GFDL Low")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pGL

### GFDL high
pGH <- tmax_piv[tmax_piv$source %in% c("Historical","GFDL_High"),] %>%
  ggplot(aes(x=Date,y=tmax_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(gfdl_high_xs[1],"-01-01")), 
                   xend = as.Date(paste0(gfdl_high_xs[2],"-01-01")), 
                   y = gfdl_high_ys[1], yend = gfdl_high_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"maximum Temperature - GFDL High")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right") 
pGH

### UKESM low
pUL <- tmax_piv[tmax_piv$source %in% c("Historical","UKESM_Low"),] %>%
  ggplot(aes(x=Date,y=tmax_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(ukesm_low_xs[1],"-01-01")), 
                   xend = as.Date(paste0(ukesm_low_xs[2],"-01-01")), 
                   y = ukesm_low_ys[1], yend = ukesm_low_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"maximum Temperature - UKESM Low")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pUL

### UKESM high
pUH <- tmax_piv[tmax_piv$source %in% c("Historical","UKESM_High"),] %>%
  ggplot(aes(x=Date,y=tmax_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(ukesm_high_xs[1],"-01-01")), 
                   xend = as.Date(paste0(ukesm_high_xs[2],"-01-01")), 
                   y = ukesm_high_ys[1], yend = ukesm_high_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"maximum Temperature - UKESM High")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pUH

ggsave(filename=paste0(fut_weather_path,"Maximum Temperature-hist vs baseline.jpg"),plot=pBL)
ggsave(filename=paste0(fut_weather_path,"Maximum Temperature-hist vs gfdl low.jpg"),plot=pGL)
ggsave(filename=paste0(fut_weather_path,"Maximum Temperature-hist vs gfdl high.jpg"),plot=pGH)
ggsave(filename=paste0(fut_weather_path,"Maximum Temperature-hist vs ukesm low.jpg"),plot=pUL)
ggsave(filename=paste0(fut_weather_path,"Maximum Temperature-hist vs ukesm high.jpg"),plot=pUH)


#########################
# Reanalysis for Max Temp
#########################

# adjust every value by the difference between where the historical mean (the
# linear regression) ends and the future values start.
#
# baseline won't be touched as it just repeats historical data

diff_gfdl_low <- hist_ys[2] - gfdl_low_ys[1]
diff_gfdl_high <- hist_ys[2] - gfdl_high_ys[1]
diff_ukesm_low <- hist_ys[2] - ukesm_low_ys[1]
diff_ukesm_high <- hist_ys[2] - ukesm_high_ys[1]

gfdl_low_reanal$maxt_C <- gfdl_low_raw$maxt_C + diff_gfdl_low
gfdl_high_reanal$maxt_C <- gfdl_high_raw$maxt_C + diff_gfdl_high
ukesm_low_reanal$maxt_C <- ukesm_low_raw$maxt_C + diff_ukesm_low
ukesm_high_reanal$maxt_C <- ukesm_high_raw$maxt_C + diff_ukesm_high


#**********************************************************************
# Precipitation
#**********************************************************************

## setup

rain_cm_df <- merge(merge(merge(merge(merge(hist_exp[,c("date","h_rain_cm")],
                                         baseln[,c("date","bl_rain_cm")],
                                         by="date",
                                         all=TRUE),
                                   gfdl_low[,c("date","gl_rain_cm")],
                                   by="date",
                                   all=TRUE),
                             gfdl_high[,c("date","gh_rain_cm")],
                             by="date",
                             all=TRUE),
                       ukesm_low[,c("date","ul_rain_cm")],
                       by="date",
                       all=TRUE),
                 ukesm_high[,c("date","uh_rain_cm")],
                 by="date",
                 all=TRUE)
colnames(rain_cm_df) <- c("Date","Historical","Baseline","GFDL_Low","GFDL_High",
                       "UKESM_Low","UKESM_High")

rain_cm_piv <- pivot_longer(rain_cm_df, c(-Date),
                                 names_to = "source",
                                 values_to = "rain_cm_val")

## calculate linear models

  fit_hist <- coef(lm(h_rain_cm ~ year, 
                         data = hist_exp))
  fit_baseln <- coef(lm(bl_rain_cm ~ year, 
                         data = baseln))
  fit_gfdl_low <- coef(lm(gl_rain_cm ~ year, 
                         data = gfdl_low))
  fit_gfdl_high <- coef(lm(gh_rain_cm ~ year, 
                         data = gfdl_high))
  fit_ukesm_low <- coef(lm(ul_rain_cm ~ year, 
                         data = ukesm_low))
  fit_ukesm_high <- coef(lm(uh_rain_cm ~ year, 
                         data = ukesm_high))
  hist_xs <- c(experiment_start_year, experiment_end_year)
  hist_ys <- cbind(1, hist_xs) %*% fit_hist
  baseln_xs <- c(end_exp_period_year+1, end_fut_period_year)
  baseln_ys <- cbind(1, baseln_xs) %*% fit_baseln
  gfdl_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
  gfdl_low_ys <- cbind(1, gfdl_low_xs) %*% fit_gfdl_low
  gfdl_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
  gfdl_high_ys <- cbind(1, gfdl_high_xs) %*% fit_gfdl_high
  ukesm_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
  ukesm_low_ys <- cbind(1, ukesm_low_xs) %*% fit_ukesm_low
  ukesm_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
  ukesm_high_ys <- cbind(1, ukesm_high_xs) %*% fit_ukesm_high

## graph just linear models
  
  pLM <- rain_cm_df %>%
    ggplot(aes(x=date, y=h_rain_cm)) +
        geom_segment(aes(x = hist_xs[1], xend = hist_xs[2], 
                         y = hist_ys[1], yend = hist_ys[2], color="Historical")) +
    geom_segment(aes(x = baseln_xs[1], xend = baseln_xs[2], 
                     y = baseln_ys[1], yend = baseln_ys[2], color="Baseline")) +
    geom_segment(aes(x = gfdl_low_xs[1], xend = gfdl_low_xs[2], 
                     y = gfdl_low_ys[1], yend = gfdl_low_ys[2], color="GFDL Low")) +
    geom_segment(aes(x = gfdl_high_xs[1], xend = gfdl_high_xs[2], 
                     y = gfdl_high_ys[1], yend = gfdl_high_ys[2], color="GFDL High")) +
    geom_segment(aes(x = ukesm_low_xs[1], xend = ukesm_low_xs[2], 
                     y = ukesm_low_ys[1], yend = ukesm_low_ys[2], color="UKESM Low")) +
    geom_segment(aes(x = ukesm_high_xs[1], xend = ukesm_high_xs[2], 
                     y = ukesm_high_ys[1], yend = ukesm_high_ys[2], color="UKESM High")) +
    ggtitle(paste(site_name,"Precipitation")) +
    theme_classic(base_family = "serif", base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())


pLM

ggsave(filename=paste0(fut_weather_path,"Precipitation linear models.jpg"),plot=pLM)


## graph each source compared to historical observations

### baseline
pBL <- rain_cm_piv[rain_cm_piv$source %in% c("Historical","Baseline"),] %>%
  ggplot(aes(x=Date,y=rain_cm_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(baseln_xs[1],"-01-01")), 
                   xend = as.Date(paste0(baseln_xs[2],"-01-01")), 
                   y = baseln_ys[1], yend = baseln_ys[2]), color=cbPalette9[9])  +
  ggtitle(paste(site_name,"Precipitation - Baseline")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pBL


### GFDL low
pGL <- rain_cm_piv[rain_cm_piv$source %in% c("Historical","GFDL_Low"),] %>%
  ggplot(aes(x=Date,y=rain_cm_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(gfdl_low_xs[1],"-01-01")), 
                   xend = as.Date(paste0(gfdl_low_xs[2],"-01-01")), 
                   y = gfdl_low_ys[1], yend = gfdl_low_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"Precipitation - GFDL Low")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pGL

### GFDL high
pGH <- rain_cm_piv[rain_cm_piv$source %in% c("Historical","GFDL_High"),] %>%
  ggplot(aes(x=Date,y=rain_cm_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(gfdl_high_xs[1],"-01-01")), 
                   xend = as.Date(paste0(gfdl_high_xs[2],"-01-01")), 
                   y = gfdl_high_ys[1], yend = gfdl_high_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"Precipitation - GFDL High")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right") 
pGH

### UKESM low
pUL <- rain_cm_piv[rain_cm_piv$source %in% c("Historical","UKESM_Low"),] %>%
  ggplot(aes(x=Date,y=rain_cm_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(ukesm_low_xs[1],"-01-01")), 
                   xend = as.Date(paste0(ukesm_low_xs[2],"-01-01")), 
                   y = ukesm_low_ys[1], yend = ukesm_low_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"Precipitation - UKESM Low")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pUL

### UKESM high
pUH <- rain_cm_piv[rain_cm_piv$source %in% c("Historical","UKESM_High"),] %>%
  ggplot(aes(x=Date,y=rain_cm_val,color=source)) +
  geom_point() +
  geom_segment(aes(x = as.Date(paste0(hist_xs[1],"-01-01")), 
                   xend = as.Date(paste0(hist_xs[2],"-12-31")), 
                   y = hist_ys[1], yend = hist_ys[2]), color=cbPalette9[9]) +
  geom_segment(aes(x = as.Date(paste0(ukesm_high_xs[1],"-01-01")), 
                   xend = as.Date(paste0(ukesm_high_xs[2],"-01-01")), 
                   y = ukesm_high_ys[1], yend = ukesm_high_ys[2]), color=cbPalette9[9]) +
  ggtitle(paste(site_name,"Precipitation - UKESM High")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right")
pUH

ggsave(filename=paste0(fut_weather_path,"Precipitation-hist vs baseline.jpg"),plot=pBL)
ggsave(filename=paste0(fut_weather_path,"Precipitation-hist vs gfdl low.jpg"),plot=pGL)
ggsave(filename=paste0(fut_weather_path,"Precipitation-hist vs gfdl high.jpg"),plot=pGH)
ggsave(filename=paste0(fut_weather_path,"Precipitation-hist vs ukesm low.jpg"),plot=pUL)
ggsave(filename=paste0(fut_weather_path,"Precipitation-hist vs ukesm high.jpg"),plot=pUH)



##############################
# Reanalysis for precipitation
##############################

# not enough difference to matter, but doing this anyway for gridded study
# and other sites where it might

diff_gfdl_low <- hist_ys[2] - gfdl_low_ys[1]
diff_gfdl_high <- hist_ys[2] - gfdl_high_ys[1]
diff_ukesm_low <- hist_ys[2] - ukesm_low_ys[1]
diff_ukesm_high <- hist_ys[2] - ukesm_high_ys[1]

# need to adjust both _cm and _mm columns. get the difference, then if it results
# in negative rain, set to 0
gfdl_low_reanal$rain_cm <- ifelse(gfdl_low_raw$rain_cm + diff_gfdl_low<0,0,
                                  gfdl_low_raw$rain_cm + diff_gfdl_low)
gfdl_high_reanal$rain_cm <- ifelse(gfdl_high_raw$rain_cm + diff_gfdl_high<0,0,
                                   gfdl_high_raw$rain_cm + diff_gfdl_high)
ukesm_low_reanal$rain_cm <- ifelse(ukesm_low_raw$rain_cm + diff_ukesm_low<0,0,
                                   ukesm_low_raw$rain_cm + diff_ukesm_low)
ukesm_high_reanal$rain_cm <- ifelse(ukesm_high_raw$rain_cm + diff_ukesm_high<0,0,
                                    ukesm_high_raw$rain_cm + diff_ukesm_high)

gfdl_low_reanal$rain_mm <- ifelse(gfdl_low_raw$rain_mm + diff_gfdl_low*10<0,0,
                                  gfdl_low_raw$rain_mm + diff_gfdl_low*10)
gfdl_high_reanal$rain_mm <- ifelse(gfdl_high_raw$rain_mm + diff_gfdl_high*10<0,0,
                                   gfdl_high_raw$rain_mm + diff_gfdl_high*10)
ukesm_low_reanal$rain_mm <- ifelse(ukesm_low_raw$rain_mm + diff_ukesm_low*10<0,0,
                                   ukesm_low_raw$rain_mm + diff_ukesm_low*10)
ukesm_high_reanal$rain_mm <- ifelse(ukesm_high_raw$rain_mm + diff_ukesm_high*10<0,0,
                                    ukesm_high_raw$rain_mm + diff_ukesm_high*10)


#**********************************************************************
# Write new files
#**********************************************************************


write.csv(gfdl_low_reanal,file=paste0(fut_weather_path,"fut_clim_scenario_2_reanal.csv"),
          row.names = F)
write.csv(gfdl_high_reanal,file=paste0(fut_weather_path,"fut_clim_scenario_3_reanal.csv"),
          row.names = F)
write.csv(ukesm_low_reanal,file=paste0(fut_weather_path,"fut_clim_scenario_4_reanal.csv"),
          row.names = F)
write.csv(ukesm_high_reanal,file=paste0(fut_weather_path,"fut_clim_scenario_5_reanal.csv"),
          row.names = F)


##############################################################################


#**********************************************************************
# Verify adjustments
#**********************************************************************


## Import adjusted CMIPs

gfdl_low_reanal <- read.csv(paste0(fut_weather_path,"fut_clim_scenario_2_reanal.csv")) 
gfdl_low_reanal$date=make_date(gfdl_low$year, gfdl_low$month, gfdl_low$day)

gfdl_high_raw <- read.csv(paste0(fut_weather_path,"fut_clim_scenario_3_reanal.csv")) 
gfdl_high_reanal$date=make_date(gfdl_high$year, gfdl_high$month, gfdl_high$day)

ukesm_low_raw <- read.csv(paste0(fut_weather_path,"fut_clim_scenario_4_reanal.csv")) 
ukesm_low_reanal$date=make_date(ukesm_low$year, ukesm_low$month, ukesm_low$day)

ukesm_high_raw <- read.csv(paste0(fut_weather_path,"fut_clim_scenario_5_reanal.csv")) 
ukesm_high_reanal$date=make_date(ukesm_high$year, ukesm_high$month, ukesm_high$day)


#**********************************************************************
# Verify minimum temperature
#**********************************************************************

## calculate linear models

fit_hist <- coef(lm(h_tmin ~ year, 
                    data = hist_exp))
fit_baseln <- coef(lm(bl_tmin ~ year, 
                      data = baseln))
fit_gfdl_low <- coef(lm(mint_C ~ year, 
                        data = gfdl_low_reanal))
fit_gfdl_high <- coef(lm(mint_C ~ year, 
                         data = gfdl_high_reanal))
fit_ukesm_low <- coef(lm(mint_C ~ year, 
                         data = ukesm_low_reanal))
fit_ukesm_high <- coef(lm(mint_C ~ year, 
                          data = ukesm_high_reanal))
hist_xs <- c(experiment_start_year, experiment_end_year)
hist_ys <- cbind(1, hist_xs) %*% fit_hist
baseln_xs <- c(end_exp_period_year+1, end_fut_period_year)
baseln_ys <- cbind(1, baseln_xs) %*% fit_baseln
gfdl_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
gfdl_low_ys <- cbind(1, gfdl_low_xs) %*% fit_gfdl_low
gfdl_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
gfdl_high_ys <- cbind(1, gfdl_high_xs) %*% fit_gfdl_high
ukesm_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
ukesm_low_ys <- cbind(1, ukesm_low_xs) %*% fit_ukesm_low
ukesm_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
ukesm_high_ys <- cbind(1, ukesm_high_xs) %*% fit_ukesm_high

## graph just linear models

pLM <- tmin_df %>%
  ggplot(aes(x=date, y=h_tmin)) +
  geom_segment(aes(x = hist_xs[1], xend = hist_xs[2], 
                   y = hist_ys[1], yend = hist_ys[2], color='Historical')) +
  geom_segment(aes(x = baseln_xs[1], xend = baseln_xs[2], 
                   y = baseln_ys[1], yend = baseln_ys[2], color="Baseline")) +
  geom_segment(aes(x = gfdl_low_xs[1], xend = gfdl_low_xs[2], 
                   y = gfdl_low_ys[1], yend = gfdl_low_ys[2], color="GFDL Low")) +
  geom_segment(aes(x = gfdl_high_xs[1], xend = gfdl_high_xs[2], 
                   y = gfdl_high_ys[1], yend = gfdl_high_ys[2], color="GFDL High")) +
  geom_segment(aes(x = ukesm_low_xs[1], xend = ukesm_low_xs[2], 
                   y = ukesm_low_ys[1], yend = ukesm_low_ys[2], color="UKESM Low")) +
  geom_segment(aes(x = ukesm_high_xs[1], xend = ukesm_high_xs[2], 
                   y = ukesm_high_ys[1], yend = ukesm_high_ys[2], color="UKESM High")) +
  ggtitle(paste(site_name,"Minimum Temperature")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

pLM

ggsave(filename=paste0(fut_weather_path,"Verification-Minimum Temperature linear models.jpg")
       ,plot=pLM)

#**********************************************************************
# Verify maximum temperature
#**********************************************************************

## calculate linear models

fit_hist <- coef(lm(h_tmax ~ year, 
                    data = hist_exp))
fit_baseln <- coef(lm(bl_tmax ~ year, 
                      data = baseln))
fit_gfdl_low <- coef(lm(maxt_C ~ year, 
                        data = gfdl_low_reanal))
fit_gfdl_high <- coef(lm(maxt_C ~ year, 
                         data = gfdl_high_reanal))
fit_ukesm_low <- coef(lm(maxt_C ~ year, 
                         data = ukesm_low_reanal))
fit_ukesm_high <- coef(lm(maxt_C ~ year, 
                          data = ukesm_high_reanal))
hist_xs <- c(experiment_start_year, experiment_end_year)
hist_ys <- cbind(1, hist_xs) %*% fit_hist
baseln_xs <- c(end_exp_period_year+1, end_fut_period_year)
baseln_ys <- cbind(1, baseln_xs) %*% fit_baseln
gfdl_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
gfdl_low_ys <- cbind(1, gfdl_low_xs) %*% fit_gfdl_low
gfdl_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
gfdl_high_ys <- cbind(1, gfdl_high_xs) %*% fit_gfdl_high
ukesm_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
ukesm_low_ys <- cbind(1, ukesm_low_xs) %*% fit_ukesm_low
ukesm_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
ukesm_high_ys <- cbind(1, ukesm_high_xs) %*% fit_ukesm_high

## graph just linear models

pLM <- tmax_df %>%
  ggplot(aes(x=date, y=h_tmax)) +
  geom_segment(aes(x = hist_xs[1], xend = hist_xs[2], 
                   y = hist_ys[1], yend = hist_ys[2], color="Historical")) +
  geom_segment(aes(x = baseln_xs[1], xend = baseln_xs[2], 
                   y = baseln_ys[1], yend = baseln_ys[2], color="Baseline")) +
  geom_segment(aes(x = gfdl_low_xs[1], xend = gfdl_low_xs[2], 
                   y = gfdl_low_ys[1], yend = gfdl_low_ys[2], color="GFDL Low")) +
  geom_segment(aes(x = gfdl_high_xs[1], xend = gfdl_high_xs[2], 
                   y = gfdl_high_ys[1], yend = gfdl_high_ys[2], color="GFDL High")) +
  geom_segment(aes(x = ukesm_low_xs[1], xend = ukesm_low_xs[2], 
                   y = ukesm_low_ys[1], yend = ukesm_low_ys[2], color="UKESM Low")) +
  geom_segment(aes(x = ukesm_high_xs[1], xend = ukesm_high_xs[2], 
                   y = ukesm_high_ys[1], yend = ukesm_high_ys[2], color="UKESM High")) +
  ggtitle(paste(site_name,"Maximum Temperature")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

pLM

ggsave(filename=paste0(fut_weather_path,"Verification-Maximum Temperature linear models.jpg"),plot=pLM)


#**********************************************************************
# Verify precipitation
#**********************************************************************

fit_hist <- coef(lm(h_rain_cm ~ year, 
                    data = hist_exp))
fit_baseln <- coef(lm(bl_rain_cm ~ year, 
                      data = baseln))
fit_gfdl_low <- coef(lm(rain_cm ~ year, 
                        data = gfdl_low_reanal))
fit_gfdl_high <- coef(lm(rain_cm ~ year, 
                         data = gfdl_high_reanal))
fit_ukesm_low <- coef(lm(rain_cm ~ year, 
                         data = ukesm_low_reanal))
fit_ukesm_high <- coef(lm(rain_cm ~ year, 
                          data = ukesm_high_reanal))
hist_xs <- c(experiment_start_year, experiment_end_year)
hist_ys <- cbind(1, hist_xs) %*% fit_hist
baseln_xs <- c(end_exp_period_year+1, end_fut_period_year)
baseln_ys <- cbind(1, baseln_xs) %*% fit_baseln
gfdl_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
gfdl_low_ys <- cbind(1, gfdl_low_xs) %*% fit_gfdl_low
gfdl_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
gfdl_high_ys <- cbind(1, gfdl_high_xs) %*% fit_gfdl_high
ukesm_low_xs <- c(end_exp_period_year+1, end_fut_period_year)
ukesm_low_ys <- cbind(1, ukesm_low_xs) %*% fit_ukesm_low
ukesm_high_xs <- c(end_exp_period_year+1, end_fut_period_year)
ukesm_high_ys <- cbind(1, ukesm_high_xs) %*% fit_ukesm_high

## graph just linear models

pLM <- rain_cm_df %>%
  ggplot(aes(x=date, y=h_rain_cm)) +
  geom_segment(aes(x = hist_xs[1], xend = hist_xs[2], 
                   y = hist_ys[1], yend = hist_ys[2], color="Historical")) +
  geom_segment(aes(x = baseln_xs[1], xend = baseln_xs[2], 
                   y = baseln_ys[1], yend = baseln_ys[2], color="Baseline")) +
  geom_segment(aes(x = gfdl_low_xs[1], xend = gfdl_low_xs[2], 
                   y = gfdl_low_ys[1], yend = gfdl_low_ys[2], color="GFDL Low")) +
  geom_segment(aes(x = gfdl_high_xs[1], xend = gfdl_high_xs[2], 
                   y = gfdl_high_ys[1], yend = gfdl_high_ys[2], color="GFDL High")) +
  geom_segment(aes(x = ukesm_low_xs[1], xend = ukesm_low_xs[2], 
                   y = ukesm_low_ys[1], yend = ukesm_low_ys[2], color="UKESM Low")) +
  geom_segment(aes(x = ukesm_high_xs[1], xend = ukesm_high_xs[2], 
                   y = ukesm_high_ys[1], yend = ukesm_high_ys[2], color="UKESM High")) +
  ggtitle(paste(site_name,"Precipitation")) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

pLM

ggsave(filename=paste0(fut_weather_path,"Verification-Precipitation linear models.jpg"),plot=pLM)


#**********************************************************************
# Verify raw vs. reanalyzed
#**********************************************************************


mint_gl_diff <- merge(gfdl_low[,c("date","gl_tmin")],
                      gfdl_low_reanal[,c("date","mint_C")],
                      by="date") %>%
  mutate(dif_mint = gl_tmin-mint_C)

}) # end suppressMessages

