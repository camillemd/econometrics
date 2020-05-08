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
library(timeSeries)
library(ggplot2)

#### DATA ####

start_date = '1900-01-01'
end_date = toString(Sys.Date()) #today

SP500 = tq_get("^GSPC", from = start_date, to = end_date)
SP500 = SP500[,c('adjusted')]
ln_SP500 = log(SP500)
diff_SP500 = diff(ln_SP500)
diff_SP500 = diff_SP500[(nrow(diff_SP500)-14687):nrow(diff_SP500),]

# create csv
companies <- GetSP500Stocks()

stock <- NULL

for (row in gsub('[.]', '-', companies$Tickers)){
  #stock_prices <- tq_get(row, from = start_date, to = end_date)
  stock_prices = getSymbols(row, from = start_date, src = "yahoo", verbose = FALSE, auto.assign = FALSE)[,6]
  stock = cbind(stock, stock_prices)
}
write.csv(stock,'./data/stock.csv')

# load csv
stock = read.csv('./data/stock.csv')

# check NA
sum(is.na(stock))
# starting at first complete line
#complete = min(which(complete.cases(stock)))
#stock = stock[complete:nrow(stock),]

# take logs
ln_stock = log(stock) # log price
diff_stock = diff(ln_stock)*100 # percentage log return

#### QUESTION 1 ####

## basic statistics
mean_stock = colMeans(diff_stock, na.rm = TRUE, dims = 1) #mean(diff_stock, na.rm = TRUE)
variance_stock = colVars(diff_stock, na.rm = TRUE, dims = 1)
skewness_stock = colSkewness(diff_stock, na.rm = TRUE)
kurtosis_stock = colKurtosis(diff_stock, na.rm = TRUE)
statistics_stock = as.matrix(cbind(mean_stock,variance_stock,skewness_stock,kurtosis_stock))

## non-parametrics density

# histogram representation
hist(mean_stock, nclass=100, xlab = 'Means')
hist(variance_stock, nclass=100, xlab = 'Variance')
hist(skewness_stock, nclass=100, xlab = 'Skewness')
hist(kurtosis_stock, nclass=100, xlab = 'Kurtosis')

# plot non-parametric densities
non_parametric <- function(stock_stats){
  density <- density(stock_stats, na.rm = TRUE)
  normal <- dnorm(density$x, mean(stock_stats, na.rm = TRUE), sd(stock_stats, na.rm = TRUE))
  
  print(mean(stock_stats, na.rm = TRUE))
  print(sd(stock_stats, na.rm = TRUE))
  
  density_data = tibble(density$x, density$y)
  normal_data = tibble(density$x, normal)
  
  ggplot(density_data, aes(x = density$x, y = density$y)) + 
    geom_point() +
    geom_line(aes(x = density$x, y = normal), data = normal_data, color = 'red')
}

# mean
non_parametric(stock_stats = mean_stock)

# variance
non_parametric(stock_stats = variance_stock)

# skewness
non_parametric(stock_stats = skewness_stock)

# kurtosis
non_parametric(stock_stats = kurtosis_stock)

#### QUESTION 2 ####
#for (i in dim())
correlation = cor(diff_SP500, diff_stock$MMM.Adjusted, use="complete.obs")

#### QUESTION 3 ####


#### QUESTION 4 ####


#### QUESTION 5 ####