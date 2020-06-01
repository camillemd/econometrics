# Financial Econometrics - Homework 1 
# 31.05.2020
# Raphael Attali, Camille Morand-Duval, Niels Nicolas, Debdeep Roy

#### INITIALISATION ####

# set dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# libraries
library(tidyquant)
library(ggplot2)

#library(rvest)
#library(tidyverse)
#library(tidyquant)
library(BatchGetSymbols)
#library(timeSeries)
#library(ggplot2)

#### DATA ####

start_date = '2010-01-01'
end_date = toString(Sys.Date()) #today

## Stocks ##

# obtain stock values (if csv has not been created)
companies <- GetSP500Stocks()
stock <- NULL

for (row in gsub('[.]', '-', companies$Tickers)){
  stock_prices = getSymbols(row, from = start_date, src = "yahoo", 
                            verbose = FALSE, auto.assign = FALSE)[,6]
  stock = cbind(stock, stock_prices)
}
stock = data.frame(stock)

# check NA
sum(is.na(stock))

# percentage log return
ln_stock = log(stock) # log price
diff_stock = diff(as.matrix(ln_stock))*100 # percentage log return
rows = dim(diff_stock)[1]

#### QUESTION 1 ####

## Farma-French Factors ##




#### QUESTION 2 ####


#### QUESTION 3 ####


#### QUESTION 4 ####


#### QUESTION 5 ####


#### QUESTION 6 ####


#### QUESTION 7 ####


#### QUESTION 8 ####
