
##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)
library(imputeTS)
library(readxl)
library(readr)
library(haven)

source("R/functions/Yearmon.R")

##################################################################
# read in the Zambia price data (2003-2008)
##################################################################
# WFP price from https://data.humdata.org/dataset/wfp-food-prices

# wfpvam_foodprices <- read_csv("C:/Users/Administrator/Downloads/wfpvam_foodprices.csv")
# zambia_foodprices = wfpvam_foodprices %>% dplyr::filter(adm0_name == "Zambia")
# save(zambia_foodprices,file="data/maize_prices/zam_price.rda")

load("data/raw/maize_prices/zam_price.rda")
# select maize price and mealie meals 
# unique(price.df$cm_name)

# maize meals data only after 2012, choose maize white only 
maize.white.price = zambia_foodprices %>% 
   dplyr::select(mkt_name, cm_name,mp_month, mp_price, mp_year) %>% 
  dplyr::filter(cm_name=="Maize (white) - Retail")  %>%
  dplyr::select(-cm_name)


maize.white.price = yearmon(maize.white.price,year_var = "mp_year",month_var = "mp_month")
  
# unique(maize.white.price$mp_year)

# keep the subset of markets that has data in 2003 
maize2003= maize.white.price %>% dplyr::filter(mp_year == 2003)
maize2003.mkt = unique(maize2003$mkt_name)

 # data on FRA sales available through December 2008 only
maize.0308= maize.white.price %>% 
  dplyr::filter(mkt_name %in% maize2003.mkt) %>% 
  dplyr::filter(mp_year<2009)


# check for missings 
obs.count = maize.0308 %>% group_by(mkt_name) %>% summarise(obs = n())
unique(obs.count$obs)
 
mkt.complete= obs.count %>% dplyr::filter(obs==72) %>% dplyr::select(mkt_name)

maize.price.complete = maize.0308 %>% 
  dplyr::filter(mkt_name %in% unique(mkt.complete$mkt_name)) 

# fill in the missing data 

obs.count.missing = maize.0308 %>% group_by(mkt_name) %>% summarise(obs = n()) %>% filter(obs<72 & obs>68)
obs.count.missing
 
maize.0308.missing = maize.0308 %>% dplyr::filter(mkt_name %in% unique(obs.count.missing$mkt_name))

# output for manual manipulation 
# write.csv(maize.0308.missing, row.names = FALSE,"data/maize_prices/missing.csv")
# 
# maize.0308.lu_mw = read.csv("data/maize_prices/missing_added.csv",stringsAsFactors = FALSE)
# 
# maize.0308.lu_mw = yearmon(maize.0308.lu_mw,year_var = "mp_year",month_var = "mp_month")


maize.0308.luangwa = maize.0308.missing %>% filter(mkt_name=="Luangwa") %>%  
   tidyr::complete(date = seq.Date(min(date), max(date), by="month")) %>%   fill(mkt_name)


maize.0308.mwinilunga  = maize.0308.missing %>% filter(mkt_name=="Mwinilunga") %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by="month")) %>%  fill(mkt_name)


library(imputeTS)
maize.0308.luangwa$mp_price = na.interpolation(maize.0308.luangwa$mp_price)
maize.0308.mwinilunga$mp_price = na.interpolation(maize.0308.mwinilunga$mp_price)

# combind and get the master data 
zambia.maize.master= bind_rows(maize.price.complete,maize.0308.luangwa,maize.0308.mwinilunga) %>% dplyr::select(mkt_name,mp_price,date)

unique(zambia.maize.master$mkt_name)

length(unique(zambia.maize.master$mkt_name))


# select the markets where FRA purchases are made 
library(readxl)
fra_purchase <- read_excel("data/raw/FRA/data_for_fra_purchases_0203-0910.xls")
sort(unique(fra_purchase$distname))

# check the districts where we don't have 
condition = unique(zambia.maize.master$mkt_name) %in% unique(fra_purchase$distname)

unique(zambia.maize.master$mkt_name)[!condition]

# "Kabwe Rural"  "Kabwe Urban"  "Ndola Rural"  "Lusaka Rural" "Lusaka Urban"

zambia.maize.master$mkt_name[zambia.maize.master$mkt_name =="Kabwe Urban"] = "Kabwe"
zambia.maize.master$mkt_name[zambia.maize.master$mkt_name =="Lusaka Urban"] = "Lusaka"

# transform from long to wide 

zambia0309.wide = zambia.maize.master %>% spread(key =mkt_name, value=mp_price) %>% dplyr::select(-`Kabwe Rural`,-`Lusaka Rural`,-`Ndola Rural`)




##################################################################
# read in the South africa and malawi price data (2003-2008) from nicole mason paper 
##################################################################
library(haven)
Mason_Myers_dataset_full <- read_dta("data/Mason_&_Myers_data_appendix/Mason_&_Myers_dataset_full.dta")

mason0309 = Mason_Myers_dataset_full %>% dplyr::filter(year>2002 & year<2009)
mason0309 = yearmon(mason0309,year_var = "year",month_var = "month")


price.joined.original = left_join(zambia0309.wide,mason0309,by="date")


##################################################################
# adjust by cpi 
##################################################################

# read in the cpi data 
cpi_rsa = read.csv("data/raw/cpi/cpi_rsa.csv")
cpi_zambia = read.csv("data/raw/cpi/cpi_zambia.csv")
colnames(cpi_zambia)[1]="year"
cpi_zambia$year= as.character(cpi_zambia$year)

# adjust yearly cpi to monthly cpi
date =  seq(zambia0309.wide$date[1],length=nrow(zambia0309.wide),by="+1 month")
cpi_zambia_month = as.data.frame(date)
cpi_zambia_month$year = substring(date,1,4)

cpi_zambia_expanded = dplyr::left_join(cpi_zambia_month,cpi_zambia,by="year")

# adjust cpi to 2003/01 as base
cpi_rsa_adjust = cpi_rsa 
cpi_rsa_adjust$cpi_rsa =  cpi_rsa$cpi_rsa/cpi_rsa$cpi_rsa[1]*100
cpi_rsa_adjust$date = as.Date(cpi_rsa_adjust$DATE,"%m/%d/%Y")

cpi_zambia_adjust = cpi_zambia_expanded
cpi_zambia_adjust$cpi_zambia = cpi_zambia_expanded$cpi_zambia/cpi_zambia_expanded$cpi_zambia[1]*100

# adjust the prices by cpi 

# colnames(price.full.data)[1:33]

zam.price.original = price.joined.original[,1:33]
rsa.price.original = price.joined.original %>% select(date,SAFEX)

zam.price.original = dplyr::left_join(zam.price.original,cpi_zambia_adjust,by="date")
rsa.price.original = dplyr::left_join(rsa.price.original,cpi_rsa_adjust,by="date")


rsa.price.deflate = rsa.price.original %>% mutate(SAFEX_adj= SAFEX/cpi_rsa*100) %>% select(SAFEX,SAFEX_adj)
zam.price.deflate = zam.price.original %>%  mutate_at( vars(Chingola:Solwezi), funs(./cpi_zambia*100*1000)) %>%
          select(-year,-cpi_zambia)



mason.vars  = price.joined.original[,34:ncol(price.joined.original)] %>% select(-SAFEX)

monthly.prices = bind_cols(zam.price.deflate,rsa.price.deflate,mason.vars)


write.csv(monthly.prices,"data/clean/price/monthly_price.csv",row.names = FALSE)


###################################
# add in the trade data 
##########################
# 
# library(readxl)
# rsa_to_zam <- read_excel("data/trade_flow/rsa_to_zam.xlsx")
# zam_to_rsa <- read_excel("data/trade_flow/zam_to_rsa.xlsx")
# 
# rsa_to_zam = yearmon(rsa_to_zam,year_var = "YEAR",month_var = "MONTH")
# zam_to_rsa = yearmon(zam_to_rsa,year_var = "YEAR",month_var = "MONTH")
#  
# rsa_to_zam.fill = rsa_to_zam %>% select(YEAR,MONTH,VALUE,QUANTITY,date) %>%  
#   complete(date = seq.Date(min(date), max(date), by="month")) %>% 
#   mutate(YEAR = format(date,"%Y"),MONTH= as.numeric(format(date,"%m")))
#   
# 
# zam_to_rsa.fill = zam_to_rsa %>% select(YEAR,MONTH,VALUE,QUANTITY,date) %>% 
#   complete(date = seq.Date(as.Date("1996-10-01"), as.Date("2017-02-01"), by="month")) %>% 
#   mutate(YEAR = format(date,"%Y"),MONTH= as.numeric(format(date,"%m")))



###################################
# Get distance measure 
##########################

source("R/functions/GoogleMapApi.r") 
## add your own google map key 
map.key = ""


market_names_zam<-colnames(zambia0309.wide)[2:33]


address_zam <- lapply(market_names_zam, function(x){paste(x,"Zambia",sep=",")})
address_zam  = unlist (address_zam)


coord_zam = coordFind(address_zam)
coord_zam$mkt  = market_names_zam

coord_zam
 

write.csv(coord_zam,"data/raw/road/coord_zam.csv",row.names=FALSE)

#install.packages("gmapsdistance")

library(gmapsdistance)
# add you own key 
gmapsdistance::set.api.key("")
gmapsdistance::get.api.key()


origins = as.character(coord_zam$formatted)
origins.formatted = gsub(origins,pattern = ", ",replacement = "+")

# couldn't find routes to senanga, use the nearby city kalangola instead 

origins.formatted[30] = "kalangola+Zambia"


travel_time = gmapsdistance::gmapsdistance(
  origin=origins.formatted,
  destination= "Lusaka+Zambia",
  combinations = "all",
  mode="driving",
  key = gmapsdistance::get.api.key(),
  shape = "long",
  avoid = "",
  departure = "now",
  dep_date = "2019-02-01",
  dep_time = "12:00:00",
  traffic_model="optimistic",
  arrival = "",
  arr_date = "",
  arr_time = ""
)


# Formating 
distance.df = as.data.frame(travel_time)

distance.df["mkt_name"] = coord_zam$mkt

mkt.distance = distance.df %>% 
  dplyr::mutate(distance_km = Distance.Distance/1000 ) %>%
  dplyr::mutate(travel_hours = Time.Time/3600 ) %>% 
  dplyr::select(mkt_name,travel_hours,distance_km)

write.csv(mkt.distance,"data/clean/mkt_distance.csv",row.names = FALSE)

###################################
# Get yearly stock and crop acreage 
###################################
library(readxl)
zambia_annual <- read_excel("data/raw/zambia_annual.xlsx")

zambia_annual = zambia_annual %>% dplyr::filter(Time>2001 & Time<2010)

colnames(zambia_annual)=c("year","area_harvested","production","stock_begin","stock_end","supply","consumption","feed","FSI")

write.csv(zambia_annual,"data/clean/zambia_annual.csv",row.names = FALSE)







###################################
# Get cfs data  
# sum up production by each district 
###################################
library(readr)
library(haven)

cfs_99 <- read_csv("data/raw/CFS/1999_00/cfs_crop00.dta.csv")

cfs.99.sum = cfs_99 %>% 
  group_by(DIST) %>% 
  summarise(prod = sum(MAIZ_KGS,na.rm = TRUE)) %>% 
  mutate(year = 1999)

cfs_00<- read_csv("data/raw/CFS/2000_01/cf_crop.dta.csv")

cfs.00.sum = cfs_00 %>% 
  filter(CFCODE=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(KGHARVN,na.rm = TRUE)) %>%
  mutate(year=2000)

cfs_01.1 <- read_csv("data/raw/CFS/2001_02/cf_crops.dta.csv")
cfs_01.2 <- read_csv("data/raw/CFS/2001_02/cf_crops2.dta.csv")
cfs_01.3 <- read_csv("data/raw/CFS/2001_02/cf_crops3.dta.csv")

cfs_01 = bind_rows(cfs_01.1,cfs_01.2,cfs_01.3)

cfs.01.sum = cfs_01 %>% 
  filter(CROPCODE=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(PRODN,na.rm = TRUE)) %>%
  mutate(year=2001)

 
cfs_02 <- read_csv("data/raw/CFS/2002_03/cfs2002_03_rectype_2_data.dta.csv")

cfs.02.sum = cfs_02 %>% 
  filter(CF01=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(CF14,na.rm = TRUE)) %>%
  mutate(year=2002)



cfs_03 <- read_csv("data/raw/CFS/2003_04/zambia_crops.dta.csv")
cfs.03.sum = cfs_03 %>% 
  filter(CF01=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(CF14,na.rm = TRUE)) %>%
  mutate(year=2003)


library(readr)
cfs_04 <- read_csv("data/raw/CFS/2004_05/cfs0405_crop.dta.csv")

cfs.04.sum= cfs_04 %>% 
  filter(CROP=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(TOTHARV,na.rm = TRUE)) %>%
  mutate(year=2004)


 
library(haven)
cfs0405_crop <- read_dta("data/raw/CFS/2004_05/cfs0405_crop.dta")

concord_number = cfs0405_crop %>% rowid_to_column(var="ID") %>% select(ID,DIST)
concord_character = cfs_04 %>% rowid_to_column(var="ID") %>% select(ID,DIST)
concord = left_join(concord_number,concord_character,by="ID") %>% select(-ID) %>% distinct()
colnames(concord) =c("DIST","DIST_CHAR")


# 05 06  use PHS instead 
cfs_05 <- read_csv("data/raw/PHS/2005_06/small&medium/cropmixture_2.dta.csv")

cfs_05 = cfs_05 %>% left_join(concord,by="DIST")

unique(cfs_05$CM13)

cfs.05.sum= cfs_05 %>% 
  filter(CROP=="Maize") %>%
  mutate(CM12=as.numeric(CM12)) %>%
  mutate(totharv= ifelse (test = CM13=="Kilogram"  ,yes = CM12,no =0  ) ) %>% 
  #mutate(totharv=as.numeric(totharv)) %>%
  mutate(totharv= ifelse (test = CM13=="50 kg bag" ,yes = CM12*50,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="50 kg bag unshelled/unpolished" ,yes = CM12*50,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="10 kg bag unshelled" ,yes = CM12*10,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="10 kg bag" ,yes = CM12*10,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="Meda" ,yes = CM12*5,no = totharv  ) ) %>% 
  mutate(totharv= ifelse (test = CM13=="5 lt gallon" ,yes = CM12*5,no = totharv  ) ) %>% 
  mutate(totharv= ifelse (test = CM13=="25 kg bag" ,yes = CM12*25,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="25 kg bag unshelled/unpolished" ,yes = CM12*25,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="90 kg bag" ,yes = CM12*90,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="90 kg bag unshelled/ unpolished" ,yes = CM12*90,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="20 lt tin" ,yes = CM12*20,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CM13=="20 lt tin unshelled/unpolished" ,yes = CM12*20,no = totharv  ) ) %>%
  group_by(DIST_CHAR)  %>%
  summarise(prod = sum(totharv,na.rm = TRUE)) %>%
  mutate(year=2005) %>%
  mutate(DIST=DIST_CHAR) %>%
  select(-DIST_CHAR)

cfs_06 <- read_csv("data/raw/PHS/2006_07/phs0607_cln/crop_01.dta.csv")
cfs.06.sum = cfs_06 %>% 
  filter(CROP=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(TOTHARV,na.rm = TRUE)) %>%
  mutate(year=2006)

cfs_07 <- read_csv("data/raw/CFS/2007_08/cfs_crop0708.dta.csv")
cfs.07.sum = cfs_07 %>% 
  filter(CROPA=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(totharv,na.rm = TRUE)) %>%
  mutate(year=2007)


cfs_08 <- read_csv("data/raw/CFS/2008_09/crop.dta.csv")

cfs.08.sum = cfs_08 %>% 
  filter(CROP=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(totharv,na.rm = TRUE)) %>%
  mutate(year=2008)


cfs_09 <- read_csv("data/raw/CFS/2009_10/cfs_crop0910.dta.csv")

cfs.09.sum = cfs_09 %>% 
  filter(CROP=="Maize") %>%
  group_by(DIST)  %>%
  summarise(prod = sum(totharv,na.rm = TRUE)) %>%
  mutate(year=2009)

# 2010 no data, but should be fine cos we only need long run average 


cfs_11 <- read_csv("data/raw/CFS/2011_12/crop.dta.csv")

cfs.11.sum= cfs_11 %>% 
  filter(CROP=="Maize") %>%
  mutate(CF14=as.numeric(CF14)) %>%
  mutate(totharv= ifelse (test = CF15=="Kilogram (kg)"  ,yes = CF14,no =0  ) ) %>% 
  #mutate(totharv=as.numeric(totharv)) %>%
  mutate(totharv= ifelse (test = CF15=="50kg bag" ,yes = CF14*50,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="50kg bag unshelled/unpolished" ,yes = CF14*50,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="10kg pocket" ,yes = CF14*10,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="MEDA" ,yes = CF14*5,no = totharv  ) ) %>% 
  mutate(totharv= ifelse (test = CF15=="25kg bag" ,yes = CF14*25,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="90kg bag" ,yes = CF14*90,no = totharv  ) ) %>%
  group_by(DIST)  %>%
  summarise(prod = sum(totharv,na.rm = TRUE)) %>%
  mutate(year=2011)
  
                
cfs_13 <- read_csv("data/raw/CFS/2013_14/crop.dta.csv")
unique(cfs_13$CF15)

cfs.13.sum= cfs_13 %>% 
  filter(CROP=="Maize") %>%
  mutate(CF14=as.numeric(CF14)) %>%
  mutate(totharv= ifelse (test = CF15=="Kilogram (kg)"  ,yes = CF14,no =0  ) ) %>% 
  #mutate(totharv=as.numeric(totharv)) %>%
  mutate(totharv= ifelse (test = CF15=="50 kg" ,yes = CF14*50,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="50kg bag unshelled/unpolished" ,yes = CF14*50,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="10 kg  bag  unshelled/ unpolished" ,yes = CF14*10,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="MEDA" ,yes = CF14*5,no = totharv  ) ) %>% 
  mutate(totharv= ifelse (test = CF15=="25kg" ,yes = CF14*25,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="25kg  bag unshelled/ unpolished" ,yes = CF14*25,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="90 kg" ,yes = CF14*90,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="90 kg bag unshelled/ unpolished" ,yes = CF14*90,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="20 lt tin" ,yes = CF14*20,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="20 lt tin  unshelled/ unpolished" ,yes = CF14*20,no = totharv  ) ) %>%
  group_by(DIST)  %>%
  summarise(prod = sum(totharv,na.rm = TRUE)) %>%
  mutate(year=2013)

cfs_14 <- read_csv("data/raw/CFS/2014_15/crop.dta.csv")

unique(cfs_14$CF15)

cfs.14.sum = cfs_14 %>% 
  filter(CROP=="Maize") %>%
  mutate(CF14=as.numeric(CF14)) %>%
  mutate(totharv= ifelse (test = CF15=="Kilogram (kg)"  ,yes = CF14,no =0  ) ) %>% 
  #mutate(totharv=as.numeric(totharv)) %>%
  mutate(totharv= ifelse (test = CF15=="50 kg" ,yes = CF14*50,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="50kg  bag unshelled/ unpolished" ,yes = CF14*50,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="10 kg  bag  unshelled/ unpolished" ,yes = CF14*10,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="10 pocket/ bag" ,yes = CF14*10,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="MEDA" ,yes = CF14*5,no = totharv  ) ) %>% 
  mutate(totharv= ifelse (test = CF15=="25kg" ,yes = CF14*25,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="25kg  bag unshelled/ unpolished" ,yes = CF14*25,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="90 kg" ,yes = CF14*90,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="90 kg bag unshelled/ unpolished" ,yes = CF14*90,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="20 lt tin" ,yes = CF14*20,no = totharv  ) ) %>%
  mutate(totharv= ifelse (test = CF15=="20 lt tin  unshelled/ unpolished" ,yes = CF14*20,no = totharv  ) ) %>%
  group_by(DIST)  %>%
  summarise(prod = sum(totharv,na.rm = TRUE)) %>%
  mutate(year=2014)



##########################################
## combind different years 
##########################################

CFS.combind = bind_rows(cfs.99.sum,cfs.00.sum,cfs.01.sum,cfs.02.sum,cfs.03.sum,cfs.04.sum,cfs.05.sum,cfs.06.sum,cfs.07.sum,cfs.08.sum,cfs.09.sum,cfs.11.sum,cfs.13.sum,cfs.14.sum)

CFS.summary = CFS.combind %>% 
  group_by(year) %>% 
  mutate(SUM= sum(prod,na.rm = TRUE)) %>% 
  mutate(share = prod/SUM) %>%
  ungroup() %>% 
  group_by(DIST) %>%
  mutate(long_run_share = mean(share,na.rm = TRUE)) %>%
  mutate(long_run_prod = mean(prod,na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(dev_prod = prod-long_run_prod) %>% 
  mutate(dev_share = share-long_run_share)
  

write.csv(CFS.summary,file="data/clean/cfs_summary.csv",row.names = FALSE)