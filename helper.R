# Load libraries
library(tidyr)
library(dplyr)
library(shiny)
library(quantmod)
#library(plyr)
library(ggplot2)

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

testing = function(){
  # Test using grid.arrange
  #df <- data.frame(x=1:10, y=rnorm(10))
  p1 <- ggplot(DATA, aes(pe_fy0,pb_fy0)) + geom_point()
  p2 <- ggplot(DATA, aes(BPS_ACT_EST_BASIS,pb_fy0)) + geom_point()
  library(gridExtra)
  plist <- list(p1,p2)
  n <- length(plist)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(plist, ncol=nCol))
  
  # plotly
  
  y1 = "pe_fy0" #Example y1 = "pe_fy0"
  y2 = "pb_fy0"
  x = DATA[[y1]]
  y = DATA[[y2]]
  p <- ggplot(DATA, aes(x,y)) + geom_point()
  (gg <- ggplotly(p))
  gg
  ggplotly(p)
  
  set.seed(100)
  d <- diamonds[sample(nrow(diamonds), 1000), ]
  p <- ggplot(data = d, aes(x = carat, y = price)) +
    geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
    geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)
  
  (gg <- ggplotly(p))
  gg
  p
  d <- diamonds[sample(nrow(diamonds), 500), ]
  plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
          mode = "markers", color = carat, size = carat)
  set.seed(1234)
  dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), rating = c(rnorm(200),rnorm(200, mean=.8)))
  ggplot(dat, aes(x=cond, y=rating)) + geom_boxplot()
  
  # heat map Test
  library(d3heatmap)
  url <- "http://datasets.flowingdata.com/ppg2008.csv"
  nba_players <- read.csv(url, row.names = 1)
  d3heatmap(nba_players, scale = "column")
  
  #summary
  tmp <- do.call(data.frame, 
                 list(mean = apply(DATA, 2, mean),
                      sd = apply(DATA, 2, sd,na.rm = TRUE),
                      median = apply(DATA, 2, median,na.rm = TRUE),
                      min = apply(DATA, 2, min,na.rm = TRUE),
                      max = apply(DATA, 2, max,na.rm = TRUE),
                      n = apply(DATA, 2, length,na.rm = TRUE)))
}
