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

df.master %>% ggplot(aes(x=year,y=log(purchase_quantity))) + 
  geom_point(aes(colour = factor(miller))) + 
  geom_smooth(method='lm',formula=y~x) +
  theme_bw()


# Plot of FRA sales vs price 

df.master %>% ggplot(aes(x=log(purchase_quantity+1),y=log(price))) + 
  geom_point(aes(colour = factor(miller))) + 
  geom_smooth(method='lm',formula=y~x) +
  theme_bw()


# fra sales and 

df.master %>% ggplot(aes(x=log(frasales_miller),y=log(price))) + 
  geom_point(aes(colour = factor(miller))) + 
  geom_smooth(method='lm',formula=y~x) +
  theme_bw()


df.master %>% ggplot(aes(x=log(frasales_miller),y=log(price))) + 
  geom_point(aes(colour = factor(miller))) + 
  theme_bw()
