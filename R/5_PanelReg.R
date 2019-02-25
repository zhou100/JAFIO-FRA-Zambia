##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)





##################################################################
# Panel Regression 
##################################################################

# Price_lean  = imports_t-1 * distance + FRA_sales_t +  FRA_purchase t_1 (locally) + annual stock +
# weather_i,t-1 + Price_year_average

load("data/clean/dataset.rda")
df.master = df.master %>% 
  mutate( IV = dev_prod*long_run_share)  %>%
  mutate(logpurchase = log(purchase_quantity+1))

attach(df.master)



# Reg 4: price ~ weighted_fra_sales + log(fra_purchase_quantity t-1) + weather vars + trend + i. mkt ( i. year removes all the weather vars effect) 



price.ols <-lm(price ~  logpurchase+ weighted_fra_sales + maxdays+raincytot +tmean +heatday + year  + factor(mkt_name) -1)
summary(price.ols)

dev.ols <-lm(dev_price_square ~logpurchase +  weighted_fra_sales+ maxdays+raincytot +tmean +heatday + year  + factor(mkt_name) -1)
summary(dev.ols)

  

#An instrument for FRA purchases using long-run shares of production by district
#  (or long-run shares of FRA purchases by district) 
# times the deviation of CFS expected total output from a long-run average
# (to capture the annual purchase targets). 

 
# first stage 

first.stage = lm( logpurchase~IV,data=df.master)
summary(first.stage)


# install.packages("AER")
# library(AER)
require(plm)

iv.price = plm(price ~ logpurchase+ weighted_fra_sales+ maxdays+raincytot +tmean +heatday + year +factor(mkt_name)-1 | . - IV + maxdays+raincytot +tmean +heatday + year  ,model = "fd" , data = df.master)
summary(iv.price)

iv.dev = plm(dev_price_square ~ logpurchase+ weighted_fra_sales+ maxdays+raincytot +tmean +heatday + year+factor(mkt_name)-1 | . - IV + maxdays+raincytot +tmean +heatday ,model="fd"  , data = df.master)
summary(iv.dev)


########################################################
# Make tables 
#########################################################

library(stargazer)
library(ivpack)

stargazer(price.ols,dev.ols,iv.price,iv.dev,
          type="text",
          keep=c("logpurchase","weighted_fra_sales","maxdays","raincytot","tmean","heatday","year")
          
          )  

 

 


 