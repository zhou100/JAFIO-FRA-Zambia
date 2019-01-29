
##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)
library(imputeTS)
library(readxl)

source("R/functions/Yearmon.R")

##################################################################
# read in the Zambia price data (2003-2008)
##################################################################
# WFP price from https://data.humdata.org/dataset/wfp-food-prices

# wfpvam_foodprices <- read_csv("C:/Users/Administrator/Downloads/wfpvam_foodprices.csv")
# zambia_foodprices = wfpvam_foodprices %>% dplyr::filter(adm0_name == "Zambia")
# save(zambia_foodprices,file="data/maize_prices/zam_price.rda")

load("data/maize_prices/zam_price.rda")
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
   complete(date = seq.Date(min(date), max(date), by="month")) %>%   fill(mkt_name)


maize.0308.mwinilunga  = maize.0308.missing %>% filter(mkt_name=="Mwinilunga") %>%
   complete(date = seq.Date(min(date), max(date), by="month")) %>%  fill(mkt_name)


library(imputeTS)
maize.0308.luangwa$mp_price = na.interpolation(maize.0308.luangwa$mp_price)
maize.0308.mwinilunga$mp_price = na.interpolation(maize.0308.mwinilunga$mp_price)

# combind and get the master data 
zambia.maize.master= bind_rows(maize.price.complete,maize.0308.luangwa,maize.0308.mwinilunga) %>% dplyr::select(mkt_name,mp_price,date)

unique(zambia.maize.master$mkt_name)

length(unique(zambia.maize.master$mkt_name))


# select the markets where FRA purchases are made 
library(readxl)
fra_purchase <- read_excel("data/data_for_fra_purchases_0203-0910.xls")
sort(unique(fra_purchase$distname))

# check the districts where we don't have 
condition = unique(zambia.maize.master$mkt_name) %in% unique(fra_purchase$distname)

unique(zambia.maize.master$mkt_name)[!condition]

# "Kabwe Rural"  "Kabwe Urban"  "Ndola Rural"  "Lusaka Rural" "Lusaka Urban"

zambia.maize.master$mkt_name[zambia.maize.master$mkt_name =="Kabwe Urban"] = "Kabwe_urban"
zambia.maize.master$mkt_name[zambia.maize.master$mkt_name =="Lusaka Urban"] = "Lusaka_urban"

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
cpi_rsa = read.csv("data/cpi/cpi_rsa.csv")
cpi_zambia = read.csv("data/cpi/cpi_zambia.csv")
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


write.csv(monthly.prices,"data/clean/price/monthly_price",row.names = FALSE)


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



source("R/functions/GoogleMapApi.r") 
map.key = "AIzaSyCekhGFVTC9Vsp5W3lrxFD2TMZP_wbWjhk"


market_names_zam<-unique(zambia.maize.master$mkt_name)
market_names_zam<- as.character(market_names_zam)


address_zam <- lapply(market_names_zam, function(x){paste(x,"Zambia",sep=",")})
address_zam  = unlist (address_zam)


coord_zam = coordFind(address_zam)
coord_zam$mkt  = market_names_zam

coord_zam
 

write.csv(coord_zam,"data/road/coord_zam.csv",row.names=FALSE)

# install.packages("gmapsdistance")

gmapsdistance::set.api.key("AIzaSyCekhGFVTC9Vsp5W3lrxFD2TMZP_wbWjhk")
gmapsdistance::get.api.key()


gmapsdistance::gmapsdistance(
  origin="Kitwe+Zambia",
  destination= "Lusaka+Zambia",
  combinations = "all",
  mode="driving",
  key = gmapsdistance::get.api.key(),
  shape = "wide",
  avoid = "",
  departure = "now",
  dep_date = "2021-01-01",
  dep_time = "12:00:00",
  traffic_model="optimistic",
  arrival = "",
  arr_date = "",
  arr_time = ""
)