##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)
require(readr)

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






fra.long$year = as.numeric(fra.long$year)

# Format the mkt names 
fra.long$distname = gsub(fra.long$distname, pattern = "Ndola_urban", replacement = "Ndola")
fra.long$distname = gsub(fra.long$distname, pattern = "Lusaka_urban", replacement = "Lusaka")
fra.long$distname = gsub(fra.long$distname, pattern = "Kabwe_urban", replacement = "Kabwe")

colnames(fra.long)[colnames(fra.long)=="distname"] = "mkt_name"


unique(fra.long$mkt_name)

# Join in the data 
df.master = left_join(df.master,fra.long,by=c("mkt_name","year"))


 
#########################################
# Split yearly purchase into monthly current purchase 
# shares from nicole mason paper   
#########################################

# take the  average monthy share 

# generate monthly shares 
library(haven)

source("R/functions/Yearmon.R")
Mason_Myers_dataset_full <- read_dta("data/Mason_&_Myers_data_appendix/Mason_&_Myers_dataset_full.dta")

mason0210 = Mason_Myers_dataset_full %>% dplyr::filter(year>2001 & year<2011)

mason0210 = yearmon(mason0210,year_var = "year",month_var = "month")

# colnames(mason0210)
month.share.buy = mason0210 %>% 
  group_by(month) %>% 
  summarise( month_average_buy= sum(frapurchmt)) %>% 
  mutate(month_share_buy=month_average_buy/sum(month_average_buy)) %>%
  select(-month_average_buy) %>%
  mutate(month=as.numeric(month))

library(readxl)
frapurchase <- read_excel("data/raw/FRA/data_for_fra_purchases_0203-0910.xls")

mean(rowMeans(frapurchase[,4:11]))

frapurchase = frapurchase %>% select(-DISTRICT,-DIS_NAME,-Prov,-provname,-dist)
head(frapurchase)

fra.purchase.wide = frapurchase[,1:11]  

# Wide to long 
fra.long = fra.purchase.wide %>% 
  gather(value= purchase_quantity,key=year,-distname,-REGION,-PROVINCE)

# Marketing year starts from May  - next april 
for (i in 1:12){
  fra.long[paste("month",i,sep="")] =  0
}

fra.monthly.long = fra.long %>% gather(value= monthly_buy,key=month,-distname,-purchase_quantity,-year,-REGION,-PROVINCE)
fra.monthly.long$month = gsub(fra.monthly.long$month, pattern = "month", replacement ="")
fra.monthly.long$month = as.numeric(fra.monthly.long$month)


fra.monthly = fra.monthly.long %>% 
  # take the four last digits 
  mutate( year_char = substr(year, 4, 7) ) %>%
  # if month< 1 take as previous year 
  mutate( year_num =  ifelse(month<5, paste("20",substr(year_char, 1,2),sep=""),paste("20",substr(year_char, 3,4),sep="") )) %>%
  mutate( year_num = as.numeric(year_num)) %>% 
  arrange(distname,month,year) %>%
  left_join(month.share.buy,by="month") %>%
  mutate(monthly_buy = purchase_quantity*month_share_buy) %>%
  mutate(mkt_name = distname) %>%
  mutate(year = year_num) %>%
  select(REGION,PROVINCE,mkt_name,year,month,monthly_buy)

fra.monthly$mkt_name = gsub(fra.monthly$mkt_name, pattern = "Ndola_urban", replacement = "Ndola")
fra.monthly$mkt_name = gsub(fra.monthly$mkt_name, pattern = "Kabwe_urban", replacement = "Kabwe")
fra.monthly$mkt_name = gsub(fra.monthly$mkt_name, pattern = "Lusaka_urban", replacement = "Lusaka")

# Join 
df.master = left_join(df.master,fra.monthly,by=c("mkt_name","year","month"))

 



write.csv(fra.long,"data/raw/FRA/FRA_long.csv",row.names = FALSE)
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
zam.coord["mill_dist_two"] = apply(dist_matrix,1,function(row) (sort(row, decreasing = FALSE)[1]+sort(row, decreasing = FALSE)[2])/2)

miller.dist = zam.coord %>% mutate(mkt_name = mkt) %>% select(mkt_name,mill_dist,mill_dist_two)
 
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

df.master = df.master %>% 
  mutate(mill_dist_km = mill_dist/1000+1 ) %>%
  mutate(mill_dist_km2 = mill_dist_two/1000) %>%
  mutate(weighted_fra_sales = count_miller/51 * frasalesmt) %>%
  mutate(frasales_miller = miller * frasalesmt) %>%
  mutate(dist_fra_sales = weighted_fra_sales / mill_dist_km ) %>%
  mutate(dist_fra_sales_2 = weighted_fra_sales / mill_dist_km2 ) 
  




##################################################################
# Join  the cfs data 
##################################################################

library(readr)
cfs_summary = read_csv("data/clean/cfs_summary.csv")
cfs_summary$DIST = as.character(cfs_summary$DIST)

FAOSTAT_data_3_1_2019 <- read_csv("data/raw/FAOSTAT_data_3-1-2019.csv")
fao = FAOSTAT_data_3_1_2019 %>% 
  mutate(prod_fao = Value) %>%
  mutate(year = Year) %>%
  select(year,prod_fao) 


cfs_summary = left_join(cfs_summary,fao,by="year")
# select the ratio at the median 
cfs_summary = cfs_summary %>% 
  mutate( times= prod_fao/SUM) %>%
  mutate( median_times= median(times) )




unique(cfs_summary$DIST)

# impact the price of the next year 
cfs.df = cfs_summary %>% 
  mutate (year=year+1) %>% 
  mutate (mkt_name = DIST) %>% 
  mutate(pred_national_prod = SUM/median_times) %>%
  select(mkt_name,year,dev_share,long_run_share,pred_national_prod)  

cfs.df$mkt_name[cfs.df$mkt_name=="Kabwe Urban"]="Kabwe"
cfs.df$mkt_name[cfs.df$mkt_name=="Lusaka urban"]="Lusaka"  

cfs.df$pred_national_prod[is.na(cfs.df$pred_national_prod)]


df.master = left_join(df.master,cfs.df,by=c("mkt_name","year"))

unique(df.master$mkt_name)

# check miss matches due to name
unique( df.master$year[is.na(df.master$long_run_share)])
unique( df.master$year[is.na(df.master$pred_national_prod)])
unique( df.master$mkt_name[is.na(df.master$long_run_share)])

df.master$pred_national_prod[df.master$year==2003]=1345949
df.master$pred_national_prod[df.master$year==2004]=30184126
df.master$pred_national_prod[df.master$year==2006]=6894502
df.master$pred_national_prod[df.master$year==2007]=259122768
df.master$pred_national_prod[df.master$year==2008]=233694017

df.master$long_run_share[df.master$mkt_name=="Kabwe"]=0.01040204
df.master$long_run_share[df.master$mkt_name=="Livingstone"]=0.001922541
df.master$long_run_share[df.master$mkt_name=="Lusaka"]=0.005076148+0.004354955
df.master$long_run_share[df.master$mkt_name=="Mazabuka"]=0.0268508
df.master$long_run_share[df.master$mkt_name=="Mwinilunga"]=0.00597832

cfs_summary$dev_share[cfs_summary$DIST=="Kabwe urban"  & cfs_summary$year==2007]
cfs_summary$dev_share[cfs_summary$DIST=="Kabwe urban"  & cfs_summary$year==2006]
cfs_summary$dev_share[cfs_summary$DIST=="Kabwe urban"  & cfs_summary$year==2005]

cfs_summary$year[cfs_summary$DIST=="Kabwe"]

df.master$dev_share[is.na(df.master$dev_share)]
df.master$dev_share[df.master$mkt_name=="Kabwe" & df.master$year==2008] = 0.006216266
df.master$dev_share[df.master$mkt_name=="Kabwe" & df.master$year==2007] = -0.006216266


df.master$dev_share[df.master$mkt_name=="Kabwe" & df.master$year==2006]=0


cfs_summary$dev_share[cfs_summary$DIST=="Livingstone"  & cfs_summary$year==2007]

df.master$dev_share[df.master$mkt_name=="Livingstone" & df.master$year==2006] =0.001028235

cfs_summary$dev_share[cfs_summary$DIST=="Lusaka" & cfs_summary$year==2004]
df.master$dev_share[df.master$mkt_name=="Lusaka" & df.master$year==2003] =0
df.master$dev_share[df.master$mkt_name=="Lusaka" & df.master$year==2004] =0

cfs_summary$dev_share[cfs_summary$DIST=="Mazabuka"  & cfs_summary$year==2005]
cfs_summary$year[cfs_summary$DIST=="Mazabuka"]

df.master$dev_share[df.master$mkt_name=="Mazabuka" & df.master$year==2006]=0

cfs_summary$year[cfs_summary$DIST=="Mwinilunga"]
df.master$dev_share[df.master$mkt_name=="Mwinilunga" & df.master$year==2006]=0

###############################
# create the Predicted monthly buy 
# # create the Predicted monthly sale 
####################################

# predicted national production to each district each month

df.master = df.master %>% mutate ( pred_dist_prod = pred_national_prod*long_run_share *month_share_buy)

df.master = df.master %>% mutate ( pred_dist_prod_dev = pred_dist_prod*(dev_share + long_run_share))

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

df.master$purchase_quantity[is.na(df.master$purchase_quantity)]
df.master$mkt_name[is.na(df.master$dev_share)]
df.master$pred_dist_prod[is.na(df.master$pred_dist_prod)]
df.master$pred_dist_prod_dev[is.na(df.master$pred_dist_prod_dev)]


 
#################################################################
# Create IVs  
# gen weighted_buy_dev = long_run_share * pred_dev_prod * month_share_buy
# gen weighted_buy_dev2 = long_run_share * pred_national_prod * month_share_buy
# gen weighted_sale_dev = month_share_sale * pred_national_prod * distance_weight
# gen logbuy = log(monthly_buy+1)
################################################################

df.master = df.master %>% 
  mutate(buy_iv = pred_dist_prod) %>% 
  mutate(buy_iv_dev = pred_dist_prod_dev) %>% 
  mutate(sell_iv = month_share_sale * pred_national_prod*long_run_share/(mill_dist_km)) %>%
  mutate(sell_iv2 = month_share_sale * pred_national_prod*long_run_share/(mill_dist_km2)) 
   
  
  
df.master$sell_iv[is.na(df.master$sell_iv)] = 0


df.master = df.master %>% 
  mutate( prod_region=ifelse(PROVINCE =="CENTRAL" | PROVINCE =="EASTERN" | PROVINCE =="SOUTHERN",1,0)) 
  
df.master = df.master %>%
  mutate( fra_purchase = monthly_buy ) %>%
  mutate( fra_sales = dist_fra_sales_2 ) %>%
  mutate( price_deviation = dev_price_square ) %>%
  mutate(  trend = year - 2003) 
  

######################################

############################
mkt.name.df = df.master %>% select(prod_region,mkt_name) %>% distinct()

mkt.map = zam.coord %>% mutate(mkt_name = mkt) %>% left_join(mkt.name.df,by="mkt_name")

write.csv(mkt.map,file="data/clean/map_coord.csv")
##################################################################
# Save the data frame for later analysis  
##################################################################
save(df.master,file="data/clean/dataset.rda")
write.csv(df.master,file="data/clean/dataset.csv",row.names = FALSE)

# write.csv(df.master,file="data/clean/dataset_leanmonth.csv",row.names = FALSE)

month.shares.df= left_join(month.share.buy,month.share.sale,by="month")
write.csv(month.shares.df,"data/clean/month_shares.csv",row.names = FALSE)