##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)
library(imputeTS)
library(readxl)

source("R/functions/Yearmon.R")

##################################################################
# read in weather data 
##################################################################

load("data/clean/weather/rain_9016.rda")



rain0208 = extracted.prec %>% dplyr::filter(date>"2001-12-30" & date<"2009-01-01")
tmin0208 = extracted.tmin %>% dplyr::filter(date>"2001-12-30" & date<"2009-01-01")
tmax0208 = extracted.tmax %>% dplyr::filter(date>"2001-12-30" & date<"2009-01-01")

class(rain0208$date)

 
## 1. generate crop year, so that it's summing up by crop year 
source("R/functions/CropYearZAMBIA.R") 


#lapply(rainlist,function(x){colnames(x)})

# generate cropyear 
rain.zambia.cropyear = CropYearZAMBIA(rain0208)
 
 
############################################################################
### first day of rain for the rainy season (since October or month >=10) for each livelihood zones 
### should there be a threshold other than 0 ?
############################################################################

########################################################################  
# generate the first date of rain after October for Tanzania and After April for Uganda 
# day1rain "the number of days after Oct 1 where five-day rainfall > 10 and it rained at least 3/5 days"
######################################################################## 
 
# moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)

 rain.rollmean = rain.zambia.cropyear %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "right", fill = NA))) %>%
  na.omit()

 day1rain=
  rain.rollmean %>%
  dplyr::select(-date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-date)

#################################################################################
#### generate maxdaysno rain 
########### longest dry spell during the rainy season (Oct-Mar) per crop year (May-Apr)"
#################################################################################

maxdaysnorain  = 
  rain.zambia.cropyear %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,0))) %>% # indicate that the day has 0 rainfall
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(date[.==1])))  %>%  # count the days with 0 rain
  dplyr::select(-date)

 

#################################################################################
#### generate rain_cytot
###########  "total rainfall from Oct to Apr （or march to july） by ipczone and cropyear" 
#################################################################################
 
rain.cytot = 
   rain.zambia.cropyear %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-date,cropyear,-year,-month) %>%
  dplyr::summarise_all(funs(sum))   # count the total rain 
 

 save(day1rain,maxdaysnorain,rain.cytot,file = "data/clean/weather/rainvars.rda")
 
 
 
#################################################################################
#### generate mean temperature in the growing season   
#################################################################################
 
 load("data/clean/weather/tmin_9016.rda")
 load("data/clean/weather/tmax_9016.rda")
 
##########################################################
## create mean temp variable  
##########################################################

 tmax.cropyear = CropYearZAMBIA(tmax0208)
 tmin.cropyear = CropYearZAMBIA(tmin0208)
 
zam.date = tmax.cropyear$date
zam.cropyear = tmax.cropyear$cropyear

tmax.cropyear = tmax.cropyear %>% dplyr::select(-date,-month,-year,-cropyear)
tmin.cropyear = tmin.cropyear %>% dplyr::select(-date,-month,-year,-cropyear)

tmean = (tmax.cropyear + tmin.cropyear)/2  
tmean["date"] = zam.date
tmean["cropyear"] = zam.cropyear


tmean.cropyear = 
  tmean %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(date) %>%
  dplyr::select(-date) %>%
  dplyr::summarise_all(funs(mean))   # generate the mean temperature by year by ipczone



#################################################################################
#### generate growing degree days  
###########  number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 
#################################################################################


gdd = tmean %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<30,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))   %>% # count the days with 0 rain
  dplyr::select(-Date)
 

#################################################################################
#### generate heat days  (>30 c )
###########  number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 
#################################################################################


mw.heatday = mw.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>=30,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))   %>% # count the days with 0 rain
  dplyr::select(-Date)

tz.heatday = tz.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%  # count the days with 0 rain
  dplyr::select(-Date)

ug.heatday = 
  ug.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<32,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1]))) %>%  # count the days with 0 rain
  dplyr::select(-Date)

save(tz.heatday,ug.heatday,mw.heatday, file="data/clean/weather/heatday.RData")
#################################################################################
## Transpose the rainfall variable with cluster/ipc zone id and year 
#############################################################################


