##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)

##################################################################
# Exploration data analysis 
##################################################################

load(file="data/clean/dataset.rda")

# Plot of FRA purchase in the previous year 

df.master %>% ggplot(aes(x=year+1,y=log(purchase_quantity+1))) + 
  geom_point(aes(colour = factor(miller))) + 
  geom_smooth(method='lm',formula=y~x) +
  theme_bw() + 
  labs( x="year", y = "log purchase",title="Total FRA purchase of the markets in study 2004-2009") + 
  ggsave("output/graph/purchase.png")


# Plot of FRA sales vs price 

df.master %>% ggplot(aes(x=log(purchase_quantity+1),y=log(price))) + 
  geom_point(aes(colour = factor(miller))) + 
  geom_smooth(method='lm',formula=y~x) +
  theme_bw() +
  labs( y="log price", x = "log purchase",title="Price and FRA purchase in the previous year") + 
  ggsave("output/graph/price+purchase.png")



# fra sales and price

df.master %>% ggplot(aes(x=log(frasales_miller+1),y=log(price+1))) + 
  geom_point(aes(colour = factor(miller))) + 
  geom_smooth(method='lm',formula=y~x) +
  theme_bw() +
  labs( x="log fra sales in miller districts", y = "log price",title="Price and FRA sales") + 
  ggsave("output/graph/price+sales+miller.png")



df.master %>% ggplot(aes(x=log(weighted_fra_sales+1),y=log(price+1))) + 
  geom_point(aes(colour = factor(miller))) + 
  geom_smooth(method='lm',formula=y~x) +
  theme_bw() +
  labs( x="fra sales weighed by # millers", y = "log price",title="Price and FRA sales") + 
  ggsave("output/graph/price+weighted+sales.png")

df.master %>% ggplot(aes(x=factor(miller),y=log(price+1))) + 
  geom_boxplot(aes(colour = factor(miller))) + 
  theme_bw() +
  labs( x="has miller or not", y = "log price",title="Price of miller districts vs not") + 
  ggsave("output/graph/price+miller.png")



# Graphs: 
#   •	1. Split sample, fra purchases vs not (median cut off), exclude Lusaka 
# •	Plot average price over month, (price change in June or July)
# 
# •	2. Equilvant (FRA sales, miller vs not) _
# 
df.master$purchase_quantity[is.na(df.master$purchase_quantity)]=0.0

df.master = df.master %>% 
  group_by(year) %>% 
  mutate( fra_buy_median= median(purchase_quantity) ) 

df.master = df.master %>% 
  ungroup %>% 
  mutate(fra_dummy = if_else(df.master$purchase_quantity> df.master$fra_buy_median,1,0))

 
df.master$fra_buy_median
df.master$fra_dummy

class()
df.master$date =as.Date(df.master$date)

fra_mean_price = df.master %>% group_by(date,fra_dummy) %>% summarise(mean_price = mean(price))

fra_mean_price %>% ggplot(aes(x=date,y=mean_price)) + 
  geom_line(aes(color = factor(fra_dummy))) + theme_classic()


miller_price = df.master %>% group_by(year,miller) %>% summarise(miller_price = mean(price))

miller_price %>% ggplot(aes(x=year,y=miller_price)) + 
  geom_line(aes(color = factor(miller))) + theme_classic()




annual.buy = df.master %>% group_by(year) %>% summarise( purchase = sum(purchase_quantity/1000))
 


annual.prod = df.master %>% group_by(year) %>% summarise( production=mean(production) )
annual.buy = df.master %>% group_by(year) %>% summarise( purchase=mean(annual_purchase/1000) )
annual = left_join(annual.buy,annual.prod,by="year")


annual %>% ggplot(aes(x=year)) + 
  geom_line(aes(y = purchase,color="FRA Purchase")) +
  geom_line(aes(y = production,color="Production")) + theme_classic()




df.master %>% filter(fra_dummy==1) %>% ggplot(aes(x=date,y=log(price+1))) + geom_line(aes(color = factor(fra_dummy)))

df.master %>% ggplot(aes(x=yearmon,y=log(price+1))) +   geom_point(aes(colour = factor(miller))) + theme_classic()

# •	3. Variation year over year (supply shocks) (predicted production, fra purchase, national level) 

df.master %>% ggplot(aes(x=year)) +   geom_point(aes(y=log(annual_purchase/100),colour="FRA_buy")) +  geom_point(aes(y=log(production),color="production")) + theme_classic()




