##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)

source("R/functions/Yearmon.R")



##################################################################
## Join monthly Price 
##################################################################

monthly.prices = read.csv("data/clean/monthly_price.csv")


## transform from wide to long 
monthly.prices.long = monthly.prices  %>% tidyr::gather(key="mkt_name",value="price",- date,-year,-month,-date_num,-mktyear,
                                  -frapurchmt,-frasalesmt,-NETPURCH,-BPP,-SPP,-LUSAKA,-CHOMA,-SAFEX,-SAFEX_adj,-MCHINJI,-mktshare,
                                  -msleftpc,-aprodpc,-exportban,-mznetimports,-yearmon) %>% 
                                dplyr::select(date,mkt_name,price,everything()) 

unique(monthly.prices.long$mkt_name)
# June to July 


  # Lean Season 12-4 

# monthly.prices.long

price.leanmonth = monthly.prices.long %>% dplyr::filter(month<5 & month>0)



##################################################################
## Join Distance 
##################################################################

distance = read.csv("data/clean/mkt_distance.csv")

df.master = full_join(price.leanmonth,distance,by="mkt_name")

##################################################################
## Join Annual  production and stocks (in the previous year)
##################################################################

zambia_annual = read.csv("data/clean/zambia_annual.csv")
zambia_annual = zambia_annual %>% mutate(FS_year = year-1 ) %>% dplyr::select(-year) %>% mutate(year = FS_year) %>% select(-FS_year)

df.master = left_join(df.master,zambia_annual,by="year")

##################################################################
## Join weather vars 
##################################################################
load("data/clean/weather_vars.rda")

df.master = left_join(df.master,weather.vars.join,by=c("mkt_name","year"))


##################################################################
## Join annual Price 
##################################################################

price.annual.mean= monthly.prices.long %>% 
  group_by(mkt_name,year) %>% 
  summarise_all(funs(mean)) %>% 
  mutate( mean_price = price) %>%
  dplyr::select(mkt_name,year,mean_price)

price.annual.cv= monthly.prices.long %>% 
  group_by(mkt_name,year) %>% 
  summarise_all(funs(sd)) %>% 
  mutate( cv_price = price) %>%
  dplyr::select(mkt_name,year,cv_price)

df.master = left_join(df.master,price.annual.mean,by=c("mkt_name","year"))
df.master = left_join(df.master,price.annual.cv,by=c("mkt_name","year"))


##################################################################
## Join FRA purchase in the previous year
##################################################################

library(readxl)
frapurchase <- read_excel("data/raw/FRA/data_for_fra_purchases_0203-0910.xls")
head(frapurchase)

fra.purchase.wide = frapurchase[,7:16] %>% select(-dist)

# Wide to long 
fra.long = fra.purchase.wide %>% gather(value= purchase_quantity,key=year,-distname)

# Format the years 
fra.long$year = gsub(fra.long$year, pattern = "fra0203", replacement = 2003)
fra.long$year = gsub(fra.long$year, pattern = "fra0304", replacement = 2004)
fra.long$year = gsub(fra.long$year, pattern = "fra0405", replacement = 2005)
fra.long$year = gsub(fra.long$year, pattern = "fra0506", replacement = 2006)
fra.long$year = gsub(fra.long$year, pattern = "fra0607", replacement = 2007)
fra.long$year = gsub(fra.long$year, pattern = "fra0708", replacement = 2008)
fra.long$year = gsub(fra.long$year, pattern = "fra0809", replacement = 2009)
fra.long$year = gsub(fra.long$year, pattern = "fra0910", replacement = 2010)

fra.long$year = as.numeric(fra.long$year)

# Format the mkt names 
fra.long$distname = gsub(fra.long$distname, pattern = "Ndola_urban", replacement = "Ndola")
fra.long$distname = gsub(fra.long$distname, pattern = "Lusaka_urban", replacement = "Lusaka")
colnames(fra.long)[colnames(fra.long)=="distname"] = "mkt_name"


unique(fra.long$mkt_name)

# Join in the data 
df.master = left_join(df.master,fra.long,by=c("mkt_name","year"))






##################################################################
# read in total number of fra purchase, fra sales and netimports the previous year 
# from nicole mason paper from previous year 
##################################################################
library(haven)

source("R/functions/Yearmon.R")
Mason_Myers_dataset_full <- read_dta("data/Mason_&_Myers_data_appendix/Mason_&_Myers_dataset_full.dta")

mason0210 = Mason_Myers_dataset_full %>% dplyr::filter(year>2001 & year<2011)

mason0210 = yearmon(mason0210,year_var = "year",month_var = "month")


price.joined.original = left_join(zambia0309.wide,mason0309,by="date")




##################################################################
# Save the data frame for later analysis  
##################################################################
save(df.master,file="data/clean/dataset.rda")
