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

# price.leanmonth = monthly.prices.long %>% dplyr::filter(month<5 & month>0)



##################################################################
## Join Distance 
##################################################################

distance = read.csv("data/clean/mkt_distance.csv")

# lean month 
#df.master = full_join(price.leanmonth,distance,by="mkt_name")

# ALL month 

df.master = full_join(monthly.prices.long,distance,by="mkt_name")

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

# the year should be the later since most purchase are made in july to Oct of the previous year 

fra.long$year = gsub(fra.long$year, pattern = "fra0203", replacement = 2003)
fra.long$year = gsub(fra.long$year, pattern = "fra0304", replacement = 2004)
fra.long$year = gsub(fra.long$year, pattern = "fra0405", replacement = 2005)
fra.long$year = gsub(fra.long$year, pattern = "fra0506", replacement = 2006)
fra.long$year = gsub(fra.long$year, pattern = "fra0607", replacement = 2007)
fra.long$year = gsub(fra.long$year, pattern = "fra0708", replacement = 2008)
fra.long$year = gsub(fra.long$year, pattern = "fra0809", replacement = 2009)
fra.long$year = gsub(fra.long$year, pattern = "fra0910", replacement = 2010)

# Split yearly purchase into monthly 




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
# from nicole mason paper   
##################################################################
library(haven)

source("R/functions/Yearmon.R")
Mason_Myers_dataset_full <- read_dta("data/Mason_&_Myers_data_appendix/Mason_&_Myers_dataset_full.dta")

mason0210 = Mason_Myers_dataset_full %>% dplyr::filter(year>2001 & year<2011)

mason0210 = yearmon(mason0210,year_var = "year",month_var = "month")

colnames(mason0210)



# generate the sum monthly buys and sales 

annual.fra.buy = mason0210 %>% group_by(year) %>% summarise( annual_purchase= sum(frapurchmt)) %>% mutate(year =year +1 )
annual.fra.sale = mason0210 %>% group_by(year) %>% summarise( annual_sales= sum(frasalesmt)) 
annual.import =  mason0210 %>% group_by(year) %>% summarise( annual_import= sum(mznetimports)) %>% mutate(year =year +1 )

# Join data 

df.master = left_join(df.master,annual.fra.buy,by="year")
df.master = left_join(df.master,annual.fra.sale,by="year")
df.master = left_join(df.master,annual.import,by="year")


# generate monthly shares 

month.share.buy = mason0210 %>% 
  group_by(month) %>% 
  summarise( month_average_buy= sum(frapurchmt)) %>% 
  mutate(month_share_buy=month_average_buy/sum(month_average_buy)) %>%
  select(-month_average_buy) %>%
  mutate(month=as.numeric(month))


month.share.sale = mason0210 %>% 
  group_by(month) %>% 
  summarise( month_average_sale= sum(frasalesmt)) %>% 
  mutate(month_share_sale=month_average_sale/sum(month_average_sale)) %>%
  select(-month_average_sale) %>%
  mutate(month=as.numeric(month))


df.master = left_join(df.master,month.share.buy,by="month")
df.master = left_join(df.master,month.share.sale,by="month")


##################################################################
# Join the list of commercial millers
##################################################################

library(readr)

miller.list = read_csv("data/raw/Millers.csv")
nrow(miller.list)

miller.list.code = miller.list %>% 
  mutate(dist_code = as.numeric(as.factor(as.character(District)))) 

miller.number = miller.list.code %>% 
  group_by(dist_code) %>% 
  summarise(count_miller = n())


miller.df =  left_join(miller.list.code,miller.number,by="dist_code") %>% 
  mutate(mkt_name=District) %>% 
  select(mkt_name,count_miller) %>% 
  distinct()  


###########################################
# Get distance to the nearest miller  
#############################################

 
zam.coord = read_csv("data/raw/road/coord_zam.csv")

zam.coord = zam.coord %>% select(mkt,lat,lon)

miller.coord = zam.coord %>% filter( mkt %in% miller.df$mkt_name )
 

library(FastKNN)
library(geosphere)

# calculate the diistance to nearest miller 
dist_matrix <- distm(x = zam.coord[,c('lon','lat')],y = miller.coord[,c('lon','lat')],fun=distVincentyEllipsoid)

# 32 rows * 15 columns. for each row, find the column that the value is 0 


zam.coord["mill_dist"] = apply(dist_matrix,1,min)


miller.dist = zam.coord %>% mutate(mkt_name = mkt) %>% select(mkt_name,mill_dist)
 
# Join the master data set 
df.master = left_join(df.master,miller.dist,by="mkt_name")

df.master = left_join(df.master,miller.df,by="mkt_name")

# replace na due to missing with 0
df.master["count_miller"] = ifelse(is.na(df.master$count_miller),yes=0,no=df.master$count_miller)

# replace na with 0 
df.master["miller"] = ifelse(df.master$count_miller==0,yes = 0, no = 1)


##################################################################
# Interact commercial millers with FRA sales 
##################################################################

df.master["weighted_fra_sales"] = df.master$count_miller/51 * df.master$frasalesmt

df.master["frasales_miller"] = df.master$miller * df.master$frasalesmt


##################################################################
# Join  the cfs data 
##################################################################

cfs_summary = read_csv("data/clean/cfs_summary.csv")
cfs_summary$DIST = as.character(cfs_summary$DIST)

unique(cfs_summary$DIST)

# impact the price of the next year 
cfs.df = cfs_summary %>% 
  mutate (year=year+1) %>% 
  mutate (mkt_name = DIST) %>% 
  select(mkt_name,year,prod_percent,dev_share,long_run_share)  

cfs.df$mkt_name[cfs.df$mkt_name=="Kabwe Urban"]="Kabwe"
cfs.df$mkt_name[cfs.df$mkt_name=="Lusaka urban"]="Lusaka"  

df.master = left_join(df.master,cfs.df,by=c("mkt_name","year"))

unique(df.master$mkt_name)
# check miss matches due to name
df.master$mkt_name[is.na(df.master$long_run_share)]

df.master$long_run_share[is.na(df.master$long_run_share)]=0
df.master$dev_share[is.na(df.master$long_run_share)]=0

# predicted national production 
df.master = df.master %>% mutate(pred_national_prod = prod_percent * production*1000)

# predicted national deviation 
df.master = df.master %>% mutate(pred_dev_prod = pred_national_prod*dev_share)

###############################
# create devation from mean variable 
####################################


national.mean= df.master %>% 
  group_by(year) %>% 
  summarise(nation_avg = mean(price,na.rm = TRUE)) %>% 
  mutate(year_c=as.character(year)) %>% 
  select(-year)

df.master = df.master %>% 
  mutate(year_c = as.character(year) ) %>%
  left_join(national.mean,by="year_c") %>%
  mutate(dev_price_square = (price-nation_avg)^2)

df.master$purchase_quantity[is.na(df.master$purchase_quantity)]=0
df.master$dev_share[is.na(df.master$dev_share)]=0
df.master$prod_percent[is.na(df.master$prod_percent)]=0
df.master$pred_national_prod[is.na(df.master$pred_national_prod)]=0
df.master$pred_dev_prod[is.na(df.master$pred_dev_prod)]=0



df.master = df.master %>% 
  mutate(logpurchase = log(purchase_quantity+1))




##################################################################
# Save the data frame for later analysis  
##################################################################
save(df.master,file="data/clean/dataset.rda")
write.csv(df.master,file="data/clean/dataset.csv",row.names = FALSE)

# write.csv(df.master,file="data/clean/dataset_leanmonth.csv",row.names = FALSE)
