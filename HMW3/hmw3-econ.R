# Financial Econometrics - Homework 1 
# 31.05.2020
# Raphael Attali, Camille Morand-Duval, Niels Nicolas, Debdeep Roy

#### INITIALISATION ####

# set dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# libraries
library(tidyquant)
library(ggplot2)
library(BatchGetSymbols)
#library(broom)

#### DATA ####

start_date = '2010-01-01'
end_date = '2020-05-01' # last date available in the FF factors file

## Stocks ##

# obtain stock values (if csv has not been created)
companies <- GetSP500Stocks()
stock <- NULL

for (row in gsub('[.]', '-', companies$Tickers)){
  stock_prices = getSymbols(row, from = start_date, to = end_date, 
                            src = "yahoo", verbose = FALSE, 
                            auto.assign = FALSE)[,6]
  stock = cbind(stock, stock_prices)
}
stock = data.frame(stock)

## Fama-French Factors ##
todayY <- as.numeric(format(Sys.Date(), "%Y"))
todayM <- as.numeric(format(Sys.Date(), "%m"))

temp <- tempfile()
factors <- read.csv("F-F_Research_Data_5_Factors_2x3.csv", sep = ",", 
                    skip = 3, header = TRUE, 
                    nrows = 7+(todayY-1963)*12+todayM-15)
factors <- factors[factors$X > 200912,] # select only values after 2010

# check NA
sum(is.na(stock)) # some stocks have NA until 2020-03-19
test_NA = stock[row.names(stock)>"2020-03-31",]
sum(is.na(test_NA))
stock[is.na(stock)] = 999

# percentage log return
log_stock = log(stock) # log price
log_returns = diff(as.matrix(log_stock))*100 # percentage log return

#### QUESTION 1 ####

# aggregate values per months
ri = aggregate(log_returns, by=list(month(rownames(log_returns)), 
                                    year(rownames(log_returns))), FUN=SUM)
ri = ri[,3:ncol(ri)]

# calculate difference between total return and risk free rate of return 
# for all stocks
ri_RF = ri - factors$RF

## Farma-French Factors ##

linear.regression <- function(ri_RF, factors, start_date, end_date){
  
  # create df to pass into linear regression
  df = data.frame(ri_RF, factors$Mkt.RF, factors$SMB, factors$HML, 
                  factors$RMW, factors$CMA)
  df$date = seq(as.Date(start_date), as.Date(end_date) - 1, "months")
  colnames(df)[colnames(df) == 'ri_Rf...1.'] = 'ri_RF'
  colnames(df)[colnames(df) == 'factors.Mkt.RF'] = 'Mkt.RF'
  colnames(df)[colnames(df) == 'factors.HML'] = 'HML'
  colnames(df)[colnames(df) == 'factors.RMW'] = 'RMW'
  colnames(df)[colnames(df) == 'factors.SMB'] = 'SMBL'
  colnames(df)[colnames(df) == 'factors.CMA'] = 'CMA'
  
  ff = lm(ri_RF ~ Mkt.RF + HML + RMW + SMBL + CMA, data = df)
  
  
  
  
}






#### QUESTION 2 ####


#### QUESTION 3 ####


#### QUESTION 4 ####


#### QUESTION 5 ####


#### QUESTION 6 ####


#### QUESTION 7 ####


#### QUESTION 8 ####
