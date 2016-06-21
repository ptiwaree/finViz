# Load libraries
library(tidyr)
library(dplyr)
library(shiny)
library(quantmod)
#library(plyr)
library(ggplot2)

#install.packages(c("tidyr","dplyr","shiny","quantmod","ggplot2")
# Read files
rm(list = ls())
setwd("X:")
PE = read.csv('pe_r20160526.csv',header=TRUE,stringsAsFactors=FALSE)
PE$pe_fy0 = as.numeric(PE$pe_fy0)
PB = read.csv("pb_r20160526.csv",stringsAsFactors=FALSE)
PB$pb_fy0 = as.numeric(PB$pb_fy0)

setwd("H:/My Documents/R/stockSelector")
list.files(getwd())
worldscope = read.csv(file="worldscope_ibes_act.csv",header=TRUE,stringsAsFactors=FALSE)
worldscope$sedol = as.character(worldscope$sedol)
sec_info = read.csv(file="sec_info.csv",header=TRUE,stringsAsFactors=FALSE)
head(sec_info)

# Stock Screener
# Merge Dataframes and other clean up
DATA <- merge(worldscope,sec_info,by.x = "invariant_id",by.y = "invariant_id",all.x=TRUE)
#filter(DATA,sedol.x != sedol.y)
#DATA %>% filter(currency.x != currency.y) %>% head(n=5)
DATA = DATA %>% select(-sedol.y)
DATA = DATA %>% rename(sedol = sedol.x, measure_currency=currency.x, currency = currency.y)
DATA = merge(DATA,PE,by.x = "invariant_id",by.y = "INVARIANT_ID",all.x=TRUE)
DATA = merge(DATA,PB,by.x = "invariant_id",by.y = "INVARIANT_ID",all.x=TRUE)
DATA <- spread(DATA,measure_code,measure_value)
#View(DATA %>% filter(invariant_id == "Z913Y11C0"))

# Company Analyzer 
setwd("U:")
fermi_market_data = read.csv(file="fermi_market_data.csv",header=TRUE,stringsAsFactors=FALSE)
time_series_data = merge(DATA[,c("invariant_id", "BT", "sec_desc")],fermi_market_data, by.x = "invariant_id", by.y = "invariant_id",all.x=TRUE)
setwd("H:/My Documents/R/stockSelector")
save(list = ls(all = TRUE), file= "all.RData")
time_series_data$new_date = gsub("12:00AM","",time_series_data$date)
time_series_data$new_date = gsub("^\\s+|\\s+$", "", time_series_data$new_date) #strip white spaces
time_series_data$new_date = as.Date(time_series_data$new_date,"%B %d %Y")
save(list = ls(all = TRUE), file= "all.RData")

# Load data
local({
  load("all.RData")
  ls()
})