# Financial Econometrics - Homework 1 
# 08.05.2020
# Raphael Attali, Camille Morand-Duval, Niels Nicolas

#### INITIALISATION ####

# set dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# libraries
#install.packages('rvest')
library(rvest)
library(tidyverse)
library(tidyquant)
library(BatchGetSymbols)

#### DATA ####

start_date = '1900-01-01'
end_date = toString(Sys.Date()) #today

SP500 = tq_get("^GSPC", from = start_date, to = end_date)
companies <- GetSP500Stocks()

# create csv 
stock_prices <- tq_get(c(companies$Tickers), from = start_date, to = end_date)
write.csv(stock,'./data/stock.csv')

# load csv
stock = read.csv('./data/stock.csv')

# check NA
sum(is.na(stock))

#### QUESTION 1 ####


#### QUESTION 2 ####


#### QUESTION 3 ####


#### QUESTION 4 ####


#### QUESTION 5 ####