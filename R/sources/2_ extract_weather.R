## extract weather data from MSWEP .nc files
## save weather data in .rda files


library(rcropmod)
library(data.table)
library(ncdf4)
library(reshape2)

load("/Users/michaelcecil/Rprojects/ZamVar/data/zam_ref_29.rda")

## swr
sw_list = list()
for(year in 1979:2016){
  print(paste0("starting_",year,"_sw"))
  sw_nc <-nc_open(paste0("/Users/michaelcecil/Documents/mswep/dswrf_",year ,"_5km_zambia.nc"))
  lat_sw <-ncdf4::ncvar_get(sw_nc, varid="lat")
  lon_sw <- ncdf4::ncvar_get(sw_nc, varid="lon")
  time_sw <-ncdf4::ncvar_get(sw_nc, varid="time")
  sw_data <- ncvar_get(sw_nc, varid="data")
  for (i in 1:NROW(zam_ref)){
    pt_lon <- toString(zam_ref[i]$X_wth)
    pt_lat <- toString(zam_ref[i]$Y_wth)
    pt_lat_lon<-paste0(pt_lat,"_",pt_lon)
    windowed_sw <-data.frame(sw_data[lon_sw==(zam_ref[i]$X_wth), lat_sw==(zam_ref[i]$Y_wth),])
    names(windowed_sw) <-c("sw")
    c<-windowed_sw$sw
    dim(c)<-c(8,NROW(c)/8) # reshape values
    d<- data.frame(colSums(c)) # sum daily swr
    names(d) <- c("sw")
    e<- d/86.4 ##(convert to mj/m2/day )
    sw_list[[pt_lat_lon]]<-  rbind(sw_list[[pt_lat_lon]],e)
  } # for point
} # for year

save(sw_list,file="sw_list_all_29_redo_test.rda")
sw_data<-NULL


##precip
prec_list = list()
for(year in 1979:2016){
  print(paste0("starting_",year,"_prec"))
  prec_nc <-nc_open(paste0("/Users/michaelcecil/Documents/mswep/prec_",year ,"_5km_zambia.nc"))
  lat_prec <-ncdf4::ncvar_get(prec_nc, varid="lat")
  lon_prec <- ncdf4::ncvar_get(prec_nc, varid="lon")
  time_prec <-ncdf4::ncvar_get(prec_nc, varid="time")
  prec_data <- ncdf4::ncvar_get(prec_nc, varid="precipitation")
  for (i in 1:NROW(zam_ref)){
    pt_lon <- toString(zam_ref[i]$X_wth)
    pt_lat <- toString(zam_ref[i]$Y_wth)
    pt_lat_lon<-paste0(pt_lat,"_",pt_lon)

    windowed_prec <-data.frame(prec_data[lon_prec==(zam_ref[i]$X_wth), lat_prec==(zam_ref[i]$Y_wth),])
    names(windowed_prec) <-c("prec")
    c<-windowed_prec$prec
    dim(c)<-c(8,NROW(c)/8) # reshape values
    d<- data.frame(colSums(c)) # sum daily precip
    names(d) <- c("prec")

    prec_list[[pt_lat_lon]]<-  rbind(prec_list[[pt_lat_lon]],d)
  } # for point
} # for year
save(prec_list,file="prec_list_all_29_redo_test.rda")
prec_data<-NULL


## tmax, tmin

tmax_list = list()
tmin_list = list()
for(year in 1979:2016){
  print(paste0("starting_",year,"_temp"))
  temp_nc <-nc_open(paste0("/Users/michaelcecil/Documents/mswep/tas_",year ,"_5km_zambia.nc"))
  lat_temp <-ncdf4::ncvar_get(temp_nc, varid="lat")
  lon_temp <- ncdf4::ncvar_get(temp_nc, varid="lon")
  time_temp <-ncdf4::ncvar_get(temp_nc, varid="time")
  temp_data <- ncvar_get(temp_nc, varid="data")
  for (i in 1:NROW(zam_ref)){
    pt_lon <- toString(zam_ref[i]$X_wth)
    pt_lat <- toString(zam_ref[i]$Y_wth)
    pt_lat_lon<-paste0(pt_lat,"_",pt_lon)

    windowed_temp <-data.frame(temp_data[lon_temp==(zam_ref[i]$X_wth), lat_temp==(zam_ref[i]$Y_wth) ,])
    names(windowed_temp) <-c("temp")
    c<-windowed_temp$temp
    dim(c)<-c(8,NROW(c)/8) # reshape values

    d_tmax <- data.frame(apply(c, 2, function(x) max(x, na.rm = TRUE))) ## find daily max
    names(d_tmax) <- c("tmax")
    e_tmax <- d_tmax - 273.15 # convert to C

    d_tmin <- data.frame(apply(c, 2, function(x) min(x, na.rm = TRUE))) ## find daily min
    names(d_tmin) <- c("tmin")
    e_tmin <- d_tmin -273.15 # convert to C

    tmax_list[[pt_lat_lon]] <- rbind(tmax_list[[pt_lat_lon]],e_tmax)

    tmin_list[[pt_lat_lon]]<-  rbind(tmin_list[[pt_lat_lon]],e_tmin)
  } # for point
} # for year

save(tmax_list,file="tmax_list_all_29_redo_test.rda")
save(tmin_list,file="tmin_list_all_29_redo_test.rda")

temp_data<-NULL

##wind

wind_list = list()
for(year in 1979:2016){
  print(paste0("starting_",year,"_wind"))

  wind_nc <-nc_open(paste0("/Users/michaelcecil/Documents/mswep/wind_",year ,"_5km_zambia.nc"))
  lat_wind <-ncdf4::ncvar_get(wind_nc, varid="lat")
  lon_wind <- ncdf4::ncvar_get(wind_nc, varid="lon")
  time_wind <-ncdf4::ncvar_get(wind_nc, varid="time")
  wind_data <- ncvar_get(wind_nc, varid="data")
  for (i in 1:NROW(zam_ref)){
    pt_lon <- toString(zam_ref[i]$X_wth)
    pt_lat <- toString(zam_ref[i]$Y_wth)
    pt_lat_lon<-paste0(pt_lat,"_",pt_lon)

    windowed_wind <-data.frame(wind_data[lon_wind==(zam_ref[i]$X_wth), lat_wind==(zam_ref[i]$Y_wth) ,])
    names(windowed_wind) <-c("wind")
    c<-windowed_wind$wind
    dim(c)<-c(8,NROW(c)/8) # reshape values
    d<- data.frame(colMeans(c)) # average daily wind
    names(d) <- c("wind")
    e<- d*(86.4) ## convert from m/s to km/d

    wind_list[[pt_lat_lon]]<-  rbind(wind_list[[pt_lat_lon]],e)
  } # for point
} # for year

save(wind_list,file="wind_list_all_29_redo_test.rda")
wind_data<-NULL

##pres
pres_list = list()
for(year in 1979:2016){
  print(paste0("starting_",year,"_pres"))

  pres_nc <-nc_open(paste0("/Users/michaelcecil/Documents/mswep/pres_",year ,"_5km_zambia.nc"))
  lat_pres <-ncdf4::ncvar_get(pres_nc, varid="lat")
  lon_pres <- ncdf4::ncvar_get(pres_nc, varid="lon")
  time_pres <-ncdf4::ncvar_get(pres_nc, varid="time")
  pres_data <- ncvar_get(pres_nc, varid="data")
  for (i in 1:NROW(zam_ref)){
    pt_lon <- toString(zam_ref[i]$X_wth)
    pt_lat <- toString(zam_ref[i]$Y_wth)
    pt_lat_lon<-paste0(pt_lat,"_",pt_lon)

    windowed_pres <-data.frame(pres_data[lon_pres==(zam_ref[i]$X_wth), lat_pres==(zam_ref[i]$Y_wth),])
    names(windowed_pres) <-c("pres")
    c<-windowed_pres$pres
    dim(c)<-c(8,NROW(c)/8) # reshape values
    d<- data.frame(colMeans(c)) # average daily pressure
    names(d) <- c("pres")
    e<- d*(0.01) ## convert from Pa  to millibar

    pres_list[[pt_lat_lon]]<-  rbind(pres_list[[pt_lat_lon]],e)
  } # for point
} # for year

save(pres_list,file="pres_list_all_29_redo_test.rda")
pres_data<-NULL

##shum
sh_list = list()
for(year in 1979:2016){
  print(paste0("starting_",year,"_sh"))

  sh_nc <-nc_open(paste0("/Users/michaelcecil/Documents/mswep/shum_",year ,"_5km_zambia.nc"))
  lat_sh <-ncdf4::ncvar_get(sh_nc, varid="lat")
  lon_sh <- ncdf4::ncvar_get(sh_nc, varid="lon")
  time_sh <-ncdf4::ncvar_get(sh_nc, varid="time")
  sh_data <- ncdf4::ncvar_get(sh_nc, varid="data")
  for (i in 1:NROW(zam_ref)){
    pt_lon <- toString(zam_ref[i]$X_wth)
    pt_lat <- toString(zam_ref[i]$Y_wth)
    pt_lat_lon<-paste0(pt_lat,"_",pt_lon)

    windowed_sh <-data.frame(sh_data[lon_sh==(zam_ref[i]$X_wth), lat_sh==(zam_ref[i]$Y_wth),])
    names(windowed_sh) <-c("sh")
    c<-windowed_sh$sh
    dim(c)<-c(8,NROW(c)/8) # reshape values
    d<- data.frame(colMeans(c)) # average daily sh
    names(d) <- c("sh")
     ## no conversion for sh

    sh_list[[pt_lat_lon]]<-  rbind(sh_list[[pt_lat_lon]],d)
  } # for point
} # for year

save(sh_list,file="sh_list_all_29_redo_test.rda")
sh_data<-NULL


