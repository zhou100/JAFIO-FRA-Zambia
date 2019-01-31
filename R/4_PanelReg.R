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

write.csv(df.master,"data/clean/dataset.csv",row.names = FALSE)

df.master = df.master %>% mutate(import_distance = ifelse(distance_km==0, yes=0,no=mznetimports/distance_km)) 
df.master = df.master %>% mutate(import_traveltime = ifelse(travel_hours==0, yes=0,no=mznetimports/travel_hours)) 


first_diff  = df.master %>%
  group_by(mkt_name,mktyear) %>%
  mutate(first_diff = price - lag(price)) %>%
  na.omit()

colnames(df.master)


ols <-lm(price ~ mean_price + SAFEX_adj +MCHINJI+ mznetimports*log(distance_km+1) + mktshare + msleftpc + frapurchmt + stock_end +   day1rain + maxdays+raincytot +tmean,data=df.master)
summary(ols)

ols <-lm(price ~ mznetimports*log(distance_km+1) + frasalesmt + frapurchmt + stock_end +   day1rain + maxdays+raincytot +tmean + factor(mkt_name),data=df.master)
summary(ols)
 


# PLOTS OF FRA purchase 
# 1. June to next july + district fixed effect

# 2. layer in fra purchase

# 3. fra sales locaiton 0 other than where the sales are 

# 4. think about stocks and imports

# sum of  FRA purchase t-1 

# imprts in previous year 

# input subsidy (year fixed effect )

# local price makes sense in terms of production 



# Monthly price - Psouth , dummy for lean season LS * FRA  


# (FRA PURCHASE DURING THE LEAN SEASON: )


# jULY - JUNE 

cv.ols <-lm(cv_price ~   mznetimports + mktshare + frasalesmt  + frapurchmt + stock_end +  maxdays+raincytot +tmean +heatday  + factor(mkt_name) -1,data=df.master)
summary(cv.ols)

ols <-lm(cv_price ~    mznetimports*travel_hours + frasalesmt + frapurchmt + production +  weather -1)


aggregate.month = df.master %>% group_by(mkt_name,year) %>% summarise_all(funs(mean))

ols <-lm(price ~ mean_price + mznetimports/distance_km + frasalesmt  + stock_end +  day1rain + maxdays+raincytot +gdd +heatday +  ,data=aggregate.month)

summary(ols)





diff <-lm(first_diff ~ mean_price + mznetimports*distance_km  + frasalesmt*distance_km +SAFEX_adj + stock_end +  day1rain + maxdays+raincytot +gdd +heatday + exportban,data=first_diff)
summary(diff)
