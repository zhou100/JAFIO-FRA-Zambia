##################################################################
# packages and functions
##################################################################
rm(list=ls())
require(tidyverse)
library(imputeTS)
library(readxl)

source("R/functions/Yearmon.R")

##################################################################
# read in weather data 
##################################################################

load(data/clean/)