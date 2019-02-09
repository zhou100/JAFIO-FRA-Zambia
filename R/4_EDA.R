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
