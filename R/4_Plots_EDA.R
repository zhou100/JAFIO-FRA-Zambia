##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)



##################################################################
# Code for replicating the figures and maps 
##################################################################


##################################################################
# Figure 1. Map of Major District Markets in Zambia
# generated using QGIS 
##################################################################


##################################################################
# Figure 2  Monthly Shares of FRA Purchase and Sales 
##################################################################
library(readr)
month_shares <- read_csv("data/clean/month_shares.csv")

month_share_plot = month_shares %>% 
  gather(-month,key= "group",value="Share") %>%
  mutate( group = ifelse(group=="month_share_buy","Purchase","Sales")) %>%
  mutate(month_name= as.factor(month))
  

levels(month_share_plot$month_name)=month.abb 

p <- ggplot(month_share_plot, aes(month_name, Share, group = group,
                     colour = group))
p1 <- p + geom_line(size = 2) +
  theme_classic()+
  labs(x = NULL, y = NULL) +
  geom_text(data = month_share_plot[month_share_plot$month ==12,],
            aes(label = group), hjust = 1.13, vjust = 0.9,size=9)+
theme(text = element_text(size=25))  + theme(legend.position="none")

p1

##################################################################
# Figure 3. FRA purchase by district 
#Total purchase by district Annual Average in 5 years 
# Data produced by the following code and then used QGIS to produce
##################################################################

library(readr)
FRA_long <- read_csv("data/raw/FRA/FRA_long.csv")

fra_buy_map = FRA_long %>%
  group_by(distname) %>%
  summarise( mean_buy = mean(purchase_quantity)) %>%
  mutate(District_n = distname) %>%
  select(District_n,mean_buy)


fra_buy_map$District_n[fra_buy_map$District_n=="Lusaka_urban"]="Lusaka"
fra_buy_map$District_n[fra_buy_map$District_n=="ItezhiTezhi"]="Itezhi-tezhi"

fra_buy_map$District_n[fra_buy_map$District_n=="Kabwe_urban"]="Kabwe"
fra_buy_map$District_n[fra_buy_map$District_n=="Kapiri_Mposhi"]="Kapiri Mposhi"
fra_buy_map$District_n[fra_buy_map$District_n=="Ndola_urban"]="Ndola"
fra_buy_map$District_n[fra_buy_map$District_n=="Shangombo"]="Shang'ombo"



write.csv( fra_buy_map,"data/clean/fra_map.csv" ,row.names = FALSE)



##################################################################
#  Figure 4. Price Distribution by district ()
# 2.	 Map of prices during lean season (Dec-March)
# 3.	Map of price right after harvest season (July-Oct)

# Data produced by the following code and then used QGIS to produce the map
##################################################################
monthly.prices = read.csv("data/clean/monthly_price.csv")

month.plot  = monthly.prices[1:37] %>% 
  dplyr::select(-date,-year,-SAFEX,-SAFEX_adj) %>%
  group_by(month) %>% 
  summarise_all(mean) %>%
  mutate(lean =if_else( month<4 & month>0|month==12 ,1,0) ) %>%
  mutate(harv =if_else( month<11 & month>6,1,0) )

# Average Price in lean month 
month.lean = month.plot %>%
  group_by(lean) %>%
  summarise_all(mean) %>%
  dplyr::filter(lean==1) %>%
  dplyr::select(-lean,-month,-harv)
  
lean.map = as.data.frame(t(month.lean)) %>% tibble::rownames_to_column()
colnames(lean.map)=c("dist","lean_price")

# Average Price in harvest month 
month.harv = month.plot %>%
  group_by(harv) %>%
  summarise_all(mean) %>%
  dplyr::filter(harv==1) %>%
  dplyr::select(-lean,-month,-harv)


harv.map = as.data.frame(t(month.harv)) %>% tibble::rownames_to_column()
colnames(harv.map)=c("dist","harv_price")

price.map = left_join(harv.map,lean.map)

write.csv(price.map,"data/clean/price_map.csv",row.names = FALSE)

##################################################################
# Figure 5. IV FRA purchase and sales 
##################################################################
load(file="data/clean/dataset.rda")

df.iv = df.master %>% 
  dplyr::select(month,year,mkt_name,buy_iv,sell_iv2) %>%
  mutate(lean =if_else( month<4 & month>0|month==12 ,1,0) ) %>%
  mutate(harv =if_else( month<11 & month>6,1,0) )

# Average sale IV in lean month 
iv.lean = df.iv %>%
  filter(lean==1) %>%
  group_by(mkt_name) %>%
  summarise_all(mean) %>%
  dplyr::select(mkt_name,sell_iv2)


# Average buy IV in harvest month 
iv.harv = df.iv %>%
  filter(harv==1) %>%
  group_by(mkt_name) %>%
  summarise_all(mean) %>%
  dplyr::select(mkt_name,buy_iv)

iv.map = left_join(iv.lean,iv.harv)
write.csv(iv.map, "data/clean/iv_map.csv",row.names = FALSE)


##################################################################
# Figure 6. Historical and Simulated Prices
##################################################################

# Consider the case of two markets:
# 1. lots of FRA purchase but no millers around : Mbala

# 2. has millers  and rarely any purchase: Lusaka

# the simulated price= real price - average marginal effect * purchase/sales

# real price 

# Subset of data and create simulated price 

load(file="data/clean/dataset.rda")


simu.figure = df.master %>% 
  dplyr::filter(mkt_name=="Lusaka"|mkt_name=="Mbala") %>%
  dplyr::select(date,mkt_name,price,fra_purchase,fra_sales,mill_dist_km2,SAFEX_adj) %>%
  dplyr::mutate(simu_price= price - 0.028*fra_purchase + 4.684*fra_sales ) %>%
  dplyr::mutate(simu_price = if_else( SAFEX_adj < simu_price & simu_price >price ,SAFEX_adj, simu_price) ) %>%
  dplyr::mutate(date=as.Date(date))

# 
Lusaka.price =
simu.figure %>% 
  filter(mkt_name=="Lusaka") %>%
  mutate( Simulated_Price= simu_price) %>%
  mutate(Price =price ) %>%
  dplyr::select(date,Price,Simulated_Price) %>%
  gather(-date,key=group,value=price) %>%
  arrange(desc(group))

mbala.price =
  simu.figure %>% 
  filter(mkt_name=="Mbala") %>%
  mutate( Simulated_Price= simu_price) %>%
  mutate(Price =price ) %>%
  dplyr::select(date,Price,Simulated_Price) %>%
  gather(-date,key=group,value=price) %>%
  arrange(desc(group))

# Generate figure 
ggplot(Lusaka.price, aes(date,price,group = group,colour = group))+
  geom_line(size = 2)  +
  theme_classic() +
  scale_x_date(date_breaks = "8 month", date_labels =  "%b %Y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size=15))

ggplot(mbala.price, aes(date,price,group = group,colour = group))+
  geom_line(size = 2)  +
  theme_classic() +
  scale_x_date(date_breaks = "8 month", date_labels =  "%b %Y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size=15))


 





##################################################################
# Table A1. Cointegration between markets (rural and urban)
##################################################################
library("tseries")

lusaka.timeseries= df.master %>% 
  dplyr::filter(mkt_name=="Lusaka") %>%
  mutate(lusaka_price = price) %>%
  select(date,lusaka_price)


#The Dickey-Fuller test statistic is very low, 
#providing a low p-value and hence evidence to reject the null hypothesis of 
# a unit root and thus evidence we have a stationary series 

adf.test(lusaka.timeseries$lusaka_price)
mbala.timeseries= df.master %>% 
  dplyr::filter(mkt_name=="Mbala") %>%
  mutate(mbala_price = price) %>%
  select(date,mbala_price)
adf.test(mbala.timeseries$mbala_price)


kaoma.timeseries= df.master %>% 
  dplyr::filter(mkt_name=="Kaoma") %>%
  mutate(kaoma_price = price) %>%
  select(date,kaoma_price)

adf.test(kaoma.timeseries$kaoma_price)



Solwezi.timeseries= df.master %>% 
  dplyr::filter(mkt_name=="Solwezi") %>%
  mutate(Solwezi_price = price) %>%
  select(date,Solwezi_price)

adf.test(Solwezi.timeseries$Solwezi_price)

Kawambwa.timeseries= df.master %>% 
  dplyr::filter(mkt_name=="Kawambwa") %>%
  mutate(Kawambwa_price = price) %>%
  select(date,Kawambwa_price)

adf.test(Kawambwa.timeseries$Kawambwa_price)

Senanga.timeseries= df.master %>% 
  dplyr::filter(mkt_name=="Senanga") %>%
  mutate(Senanga_price = price) %>%
  select(date,Senanga_price)

adf.test(Senanga.timeseries$Senanga_price)

price.df  = left_join(lusaka.timeseries,mbala.timeseries) 
price.df  = left_join(price.df,kaoma.timeseries)
price.df  = left_join(price.df,Solwezi.timeseries) 
price.df  = left_join(price.df,Senanga.timeseries) 
price.df  = left_join(price.df,Kawambwa.timeseries) %>% select(-date)


library("urca")
jotest=ca.jo(price.df , type="trace", K=2, ecdet="none", spec="longrun")

summary(jotest)

 
##################################################################
# Exploration data analysis (omitted)
##################################################################

# load(file="data/clean/dataset.rda")
# 
# # Plot of FRA purchase in the previous year 
# 
# df.master %>% ggplot(aes(x=year+1,y=log(purchase_quantity+1))) + 
#   geom_point(aes(colour = factor(miller))) + 
#   geom_smooth(method='lm',formula=y~x) +
#   theme_bw() + 
#   labs( x="year", y = "log purchase",title="Total FRA purchase of the markets in study 2004-2009") + 
#   ggsave("output/graph/purchase.png")
# 
# 
# # Plot of FRA sales vs price 
# 
# df.master %>% ggplot(aes(x=log(purchase_quantity+1),y=log(price))) + 
#   geom_point(aes(colour = factor(miller))) + 
#   geom_smooth(method='lm',formula=y~x) +
#   theme_bw() +
#   labs( y="log price", x = "log purchase",title="Price and FRA purchase in the previous year") + 
#   ggsave("output/graph/price+purchase.png")
# 
# 
# 
# # fra sales and price
# 
# df.master %>% ggplot(aes(x=log(frasales_miller+1),y=log(price+1))) + 
#   geom_point(aes(colour = factor(miller))) + 
#   geom_smooth(method='lm',formula=y~x) +
#   theme_bw() +
#   labs( x="log fra sales in miller districts", y = "log price",title="Price and FRA sales") + 
#   ggsave("output/graph/price+sales+miller.png")
# 
# 
# 
# df.master %>% ggplot(aes(x=log(weighted_fra_sales+1),y=log(price+1))) + 
#   geom_point(aes(colour = factor(miller))) + 
#   geom_smooth(method='lm',formula=y~x) +
#   theme_bw() +
#   labs( x="fra sales weighed by # millers", y = "log price",title="Price and FRA sales") + 
#   ggsave("output/graph/price+weighted+sales.png")
# 
# df.master %>% ggplot(aes(x=factor(miller),y=log(price+1))) + 
#   geom_boxplot(aes(colour = factor(miller))) + 
#   theme_bw() +
#   labs( x="has miller or not", y = "log price",title="Price of miller districts vs not") + 
#   ggsave("output/graph/price+miller.png")
# 
# 
# 
# # Graphs: 
# #   •	1. Split sample, fra purchases vs not (median cut off), exclude Lusaka 
# # •	Plot average price over month, (price change in June or July)
# # 
# # •	2. Equilvant (FRA sales, miller vs not) _
# # 
# df.master$purchase_quantity[is.na(df.master$purchase_quantity)]=0.0
# 
# df.master = df.master %>% 
#   group_by(year) %>% 
#   mutate( fra_buy_median= median(purchase_quantity) ) 
# 
# df.master = df.master %>% 
#   ungroup %>% 
#   mutate(fra_dummy = if_else(df.master$purchase_quantity> df.master$fra_buy_median,1,0))
# 
# 
# df.master$fra_buy_median
# df.master$fra_dummy
# 
# class()
# df.master$date =as.Date(df.master$date)
# 
# fra_mean_price = df.master %>% group_by(date,fra_dummy) %>% summarise(mean_price = mean(price))
# 
# fra_mean_price %>% ggplot(aes(x=date,y=mean_price)) + 
#   geom_line(aes(color = factor(fra_dummy))) + theme_classic()
# 
# 
# miller_price = df.master %>% group_by(year,miller) %>% summarise(miller_price = mean(price))
# 
# miller_price %>% ggplot(aes(x=year,y=miller_price)) + 
#   geom_line(aes(color = factor(miller))) + theme_classic()
# 
# 
# 
# 
# annual.buy = df.master %>% group_by(year) %>% summarise( purchase = sum(purchase_quantity/1000))
# 
# 
# 
# annual.prod = df.master %>% group_by(year) %>% summarise( production=mean(production) )
# annual.buy = df.master %>% group_by(year) %>% summarise( purchase=mean(annual_purchase/1000) )
# annual = left_join(annual.buy,annual.prod,by="year")
# 
# 
# annual %>% ggplot(aes(x=year)) + 
#   geom_line(aes(y = purchase,color="FRA Purchase")) +
#   geom_line(aes(y = production,color="Production")) + theme_classic()
# 
# 
# 
# 
# df.master %>% filter(fra_dummy==1) %>% ggplot(aes(x=date,y=log(price+1))) + geom_line(aes(color = factor(fra_dummy)))
# 
# df.master %>% ggplot(aes(x=yearmon,y=log(price+1))) +   geom_point(aes(colour = factor(miller))) + theme_classic()
# 
# # •	3. Variation year over year (supply shocks) (predicted production, fra purchase, national level) 
# 
# df.master %>% ggplot(aes(x=year)) +   geom_point(aes(y=log(annual_purchase/100),colour="FRA_buy")) +  geom_point(aes(y=log(production),color="production")) + theme_classic()

