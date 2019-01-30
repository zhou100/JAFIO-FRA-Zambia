##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)


##################################################################
## Join monthly Price 
##################################################################

monthly.prices = read.csv("data/clean/monthly_price.csv")


## transform from wide to long 
monthly.prices.long = monthly.prices  %>% tidyr::gather(key="mkt_name",value="price",- date,-year,-month,-date_num,-mktyear,
                                  -frapurchmt,-frasalesmt,-NETPURCH,-BPP,-SPP,-LUSAKA,-CHOMA,-SAFEX,-SAFEX_adj,-MCHINJI,-mktshare,
                                  -msleftpc,-aprodpc,-exportban,-mznetimports,-yearmon) %>% 
                                dplyr::select(date,mkt_name,price,everything()) 

  # Lean Season 12-4 

# monthly.prices.long

price.leanmonth = monthly.prices.long %>% dplyr::filter(month<5 & month>0)


##################################################################
## Join Distance 
##################################################################

distance = read.csv("data/clean/mkt_distance.csv")

df.master = full_join(price.leanmonth,distance,by="mkt_name")

##################################################################
## Join Annual  production and stocks 
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


save(df.master,file="data/clean/dataset.rda")
