
##################################################################
# packages and functions
##################################################################

library(dplyr)
library(readxl)
library(ggplot2)

source("R/functions/Yearmon.R")

##################################################################
# read in the price data 
##################################################################
zam_price <- read_excel("data/maize_prices/zam_price.xlsx")
#head(zam_price)

# select Lusaka price 
zam_price_clean1217 = zam_price %>% filter(mkt_name=="Lusaka") %>% select(mp_month,mp_year,mp_price)
zam_price_clean0312 = zam_price %>% filter(mkt_name=="Lusaka Urban") %>% select(mp_month,mp_year,mp_price)
zam_price_clean = bind_rows(zam_price_clean0312,zam_price_clean1217)

# transform year and month to date
zam_price_yearmon = yearmon(zam_price_clean,year_var = "mp_year",month_var = "mp_month") %>% select(-mp_month,-mp_year,-yearmon) %>% distinct()  
# reorder the columns
zam_price_yearmon = zam_price_yearmon[,c("date","mp_price")]

# check for missings 
date =  seq(zam_price_yearmon$date[1],length=185,by="+1 month")
dplyr::left_join(as.data.frame(date),zam_price_yearmon,by ="date")

# read in the missing data covered by fews net price bullentin
zam_price18 <- read_excel("data/maize_prices/zam_price18.xlsx")
zam_price18$date = as.Date(zam_price18$date)
zam_price_filled  = dplyr::bind_rows(zam_price18,zam_price_yearmon) %>% arrange(date)


# read in rsa price 
rsa_price <- read_excel("data/maize_prices/rsa_price_clean.xlsx")
rsa_price$date = as.Date(rsa_price$date )

##################################################################
# adjust by cpi 
##################################################################

# read in the cpi data 
cpi_rsa = read.csv("data/cpi/cpi_rsa.csv")
cpi_zambia = read.csv("data/cpi/cpi_zambia.csv")
colnames(cpi_zambia)[1]="year"
cpi_zambia$year= as.character(cpi_zambia$year)

# adjust yearly cpi to monthly cpi
date =  seq(zam_price_yearmon$date[1],length=185,by="+1 month")
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

zam_price_final = dplyr::left_join(zam_price_filled,cpi_zambia_adjust,by="date")
zam_price_final$zam_price = zam_price_final$mp_price/zam_price_final$cpi_zambia *100
zam_price_final = zam_price_final %>% select(date,zam_price)

rsa_price_final = dplyr::left_join(rsa_price,cpi_rsa_adjust,by="date")
rsa_price_final$rsa_price = rsa_price_final$price/rsa_price_final$cpi_rsa *100 
rsa_price_final = rsa_price_final %>% select(date,rsa_price)


##################################################################
# adjust by exchange rate 
##################################################################

ex= read.csv("data/ExchangeRate.csv")
colnames(ex) = c("DATE","rand_per_kwa")
#  from  daily exchange rate to monthly (average)
ex$date = as.Date (ex$DATE,"%m/%d/%Y")
ex$yearmon = substring(ex$date,1,7)

# before 2013,unit is in 1000
 adjust = ex[ex$date <"2013-01-01",]
 adjust$rand_per_kwa = adjust$rand_per_kwa/1000

 later =  ex[ex$date > "2013-01-01",]
 
 full = bind_rows(adjust,later)
  
 as = full %>% group_by(yearmon) %>% select(rand_per_kwa) %>%  summarise(mean_ex= mean(rand_per_kwa))

# adjust to kg from ton by diving 1000
rsa_price_final$rsa_price = rsa_price_final$rsa_price/1000

rsa_price_final$yearmon = substring(rsa_price_final$date,1,7)
rsa_price_final = left_join(rsa_price_final,as,by = "yearmon")
rsa_price_final$rsa_price_kwa = rsa_price_final$rsa_price * rsa_price_final$mean_ex

rsa_price_kwacha = rsa_price_final %>% select(date,rsa_price_kwa)
###################################################################
# adjust by 
##################################################################

# combine into one df

Price = left_join(zam_price_final,rsa_price_kwacha,by = "date")

# plot the two prices 
 
ggplot(data = Price, aes(x = date)) + geom_line(aes(y = rsa_price_kwa),color="blue") + geom_line(aes(y = zam_price),color="red")+ theme(legend.position = "right")

require("forecast")
require("vars")

# weather data 

# 