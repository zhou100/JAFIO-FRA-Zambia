
##################################################################
# Goal : generate the crop year indicator in the rainfall data frame 

# Input : 
# 1.df: df, rains extracted from CHIRPS or temperature from African Drought monitor
# 2.date: vector, contains the date variable 



# Output: 
# 1. df: cropyear_weather with only rain during the raining season with    
################################################################### 
library(dplyr)
library(zoo)

CropYearZAMBIA = function(df){
  
  df = df %>% dplyr::mutate(month = strftime(as.Date(date),"%m") )%>% 
    dplyr::mutate(year = as.numeric (strftime(as.Date(date),"%Y")) )
  

  cropyear1 =   df %>% 
    filter( date>=as.Date("2002-10-01") & date<=as.Date("2003-05-01") )  %>%
    dplyr::mutate(cropyear = 2002)
  
  cropyear2 =   df %>% 
    filter( date>=as.Date("2003-10-01") & date<=as.Date("2004-05-01") ) %>%
    dplyr::mutate(cropyear = 2003)
  
  cropyear3 =   df %>% 
    filter( date>=as.Date("2004-10-01") & date<=as.Date("2005-05-01") )  %>%
    dplyr::mutate(cropyear = 2004)
  
  cropyear4 =   df %>% 
    filter( date>=as.Date("2005-10-01") & date<=as.Date("2006-05-01") )  %>%
    dplyr::mutate(cropyear = 2005)
  
  cropyear5 =   df %>% 
    filter( date>=as.Date("2006-10-01") & date<=as.Date("2007-05-01") )  %>%
    dplyr::mutate(cropyear = 2006)
  
  cropyear6 =   df %>% 
    filter( date>=as.Date("2007-10-01") & date<=as.Date("2008-05-01") )  %>%
    dplyr::mutate(cropyear = 2007)
  
  cropyear7 =   df %>% 
    filter( date>=as.Date("2008-10-01") & date<=as.Date("2009-05-01") )  %>%
    dplyr::mutate(cropyear = 2008)
   
  cropyear_weather = dplyr::bind_rows(cropyear1,cropyear2,cropyear3,cropyear4,cropyear5,cropyear6,cropyear7)
  
  return(cropyear_weather)
}