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

attach(df.master)

colnames(df.master)
 

price.ols <-lm(price ~  logpurchase+   weighted_fra_sales + 
                 MCHINJI + SAFEX + maxdays+raincytot +tmean +heatday + year+ 
                 stock_end  + factor(mkt_name) -1 + factor(month))
summary(price.ols)

dev.ols <-lm(dev_price_square ~logpurchase +  weighted_fra_sales + MCHINJI + SAFEX + maxdays+raincytot +tmean +heatday + year  + stock_end + factor(mkt_name) -1+ factor(month))
summary(dev.ols)



  

#An instrument for FRA purchases using long-run shares of production by district
#  (or long-run shares of FRA purchases by district) 
# times the deviation of CFS expected total output from a long-run average
# (to capture the annual purchase targets). 

 
# first stage 
 

# fra_sales ~ monthly share + predict FRA stock
# + distance_weight + 
#   expected monthly share * the predicted FRA stocks from last year
# * distance-weights

require(plm)

iv.price = plm(price ~ 
              logpurchase+ weighted_fra_sales+ 
                maxdays+raincytot +tmean +
                MCHINJI + SAFEX + stock_end + 
                heatday + year + factor(month)+ factor(mkt_name)-1 | . - IV + 
                maxdays+raincytot +tmean +
                MCHINJI + SAFEX + stock_end + 
                heatday + year + factor(month)+ factor(mkt_name)-1  ,
              model = "within" , data = df.master)

summary(iv.price)

iv.dev = plm(dev_price_square ~ logpurchase+ weighted_fra_sales+ maxdays+raincytot +tmean +heatday + year+factor(mkt_name)-1 | . - IV + maxdays+raincytot +tmean +heatday ,model="fd"  , data = df.master)
summary(iv.dev)


first.stage = lm( logpurchase~IV + factor(mkt_name) + factor(month),data=df.master)
summary(first.stage)

 




########################################################
# Make tables 
#########################################################

library(stargazer)
library(ivpack)

stargazer(price.ols,dev.ols,iv.price,iv.dev,
          type="text",
          keep=c("logpurchase","weighted_fra_sales","maxdays","raincytot","tmean","heatday","year")
          
          )  

 

 


 