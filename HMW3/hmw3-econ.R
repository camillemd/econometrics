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
library(robustbase)
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

# check NA
sum(is.na(stock)) # some stocks have NA until 2020-03-19
test_NA = stock[row.names(stock)>"2020-03-31",]
sum(is.na(test_NA))
#stock[is.na(stock)] = 999

replaceNA = function(x){
  replace(x, is.na(x), median(x, na.rm = TRUE))
}

# percentage log return
log_stock = log(stock) # log price
log_stock[] = lapply(log_stock, replaceNA)
log_returns = diff(as.matrix(log_stock))*100 # percentage log return

sum(is.na(log_returns))

## Fama-French Factors ##
todayY <- as.numeric(format(Sys.Date(), "%Y"))
todayM <- as.numeric(format(Sys.Date(), "%m"))

temp <- tempfile()
factors <- read.csv("F-F_Research_Data_5_Factors_2x3.csv", sep = ",", 
                    skip = 3, header = TRUE, 
                    nrows = 7+(todayY-1963)*12+todayM-15)
factors <- factors[factors$X > 200912,] # select only values after 2010

#### QUESTION 1 ####

# aggregate values per months
ri = aggregate(log_returns, by=list(month(rownames(log_returns)), 
                                    year(rownames(log_returns))), FUN=SUM)
ri = ri[,3:ncol(ri)]

# calculate difference between total return and risk free rate of return 
# for all stocks
ri_RF = ri - factors$RF

## Farma-French Factors ##

## Farma French linear regression function 

linear.regression <- function(ri_RF, factors, start_date, end_date){
  
  # create df to pass into linear regression
  df = data.frame(ri_RF, factors$Mkt.RF, factors$SMB, factors$HML, 
                  factors$RMW, factors$CMA)
  
  df$date = seq(as.Date(start_date), as.Date(end_date) - 1, "months")
  colnames(df)[colnames(df) == 'ri_RF...1.'] = 'ri_RF'
  colnames(df)[colnames(df) == 'factors.Mkt.RF'] = 'Mkt.RF'
  colnames(df)[colnames(df) == 'factors.HML'] = 'HML'
  colnames(df)[colnames(df) == 'factors.RMW'] = 'RMW'
  colnames(df)[colnames(df) == 'factors.SMB'] = 'SMBL'
  colnames(df)[colnames(df) == 'factors.CMA'] = 'CMA'
  
  # linear regression on single stock
  ff = lm(ri_RF ~ Mkt.RF + HML + RMW + SMBL + CMA, data = df)
  
  coefficients = as.matrix(ff$coefficients[1:6])
  tvalue = summary(ff)$coefficients[,3][1]
  R2 = summary(ff)$r.squared 
  
  outputs = t(rbind(coefficients, tvalue, R2))

  return(outputs)

}

## compute matrix for all stocks 

ff_outputs = matrix(0L, nrow = length(ri_RF), ncol = 8)
for (i in 1:length(ri_RF)){ 
  ff_outputs[i,] = linear.regression(ri_RF[,i], factors, start_date, end_date)
}

# rename columns and rows
colnames(ff_outputs) = c('beta0', 'beta1', 'beta2', 'beta3', 'beta4', 'beta5', 'tvalue', 'R2')
rownames(ff_outputs) = c(colnames(stock))
head(ff_outputs)

#### QUESTION 2 ####

library(timeSeries)

## basic statistics
mean_ff = colMeans(ff_outputs, na.rm = TRUE, dims = 1)
variance_ff = colVars(ff_outputs, na.rm = TRUE)
skewness_ff = colSkewness(ff_outputs, na.rm = TRUE)
kurtosis_ff = colKurtosis(ff_outputs, na.rm = TRUE)
statistics_ff = as.matrix(cbind(mean_ff,variance_ff,skewness_ff,kurtosis_ff))

# clean table


#### QUESTION 3 ####

## non-parametrics density

# histogram representation
hist(ff_outputs[,1], nclass=100, xlab = 'Beta_0')

# plot non-parametric densities
non_parametric <- function(ff_stats, xname, yname, year){
  density <- density(ff_stats, na.rm = TRUE)
  #normal <- dnorm(density$x, mean(ff_stats, na.rm = TRUE), sd(ff_stats, na.rm = TRUE))
  
  density_data = tibble(density$x, density$y)
  #normal_data = tibble(density$x, normal)
  
  ggplot(density_data, aes(x = density$x, y = density$y)) + 
    geom_line() +
    #geom_line(aes(x = density$x, y = normal, color = 'Normal'), data = normal_data) + 
    labs(x = xname, y = yname)
  # + scale_color_manual(name="Legend", values = c("black","red"))
  # + theme(legend.position = c(0.985, 0.985), legend.justification = c("right", "top"))
  # +theme(legend.position = c(0.015, 0.985), legend.justification = c("left", "top"))
  
  #ggsave(paste('./plots/', xname, year, ".png", sep = ''))
}

# TO BE CONTINUED 
# names = c('beta0', 'beta1', 'beta2', 'beta3', 'beta4', 'beta5', 'tvalue', 'R2')
#for (i in number of statistics){
#  non_parametric(ff_stats = ff_outputs[,i], names[c(i)], 'Frequency', 'All')
#}

#### QUESTION 4 ####

sectors = unique(companies$GICS.Sector)
R2_GICS = matrix(0L, nrow = length(ri_RF), ncol = 11)

for (sector in sectors){
  
  # select the relevant stocks 
  relevant_stock = companies$Tickers[companies$GICS.Sector == sector,]
  relevant_stock = paste(relevant_stock, '.Adjusted')
  
  # select the corresponding log-returns
  ri_RF_GICS = ri_RF[colNames(ri_RF) == relevant_stock,]
  
  # compute the FF parameters 
  ff_outputs_GICS = matrix(0L, nrow = length(ri_RF_GICS), ncol = 8)
  for (i in 1:length(ri_Rf)){ 
    ff_outputs_GICS[i,] = linear.regression(ri_RF_GICS[,i], factors, start_date, end_date)
  }
  
  # store R2 
  R2_GICS[sector,] = ff_outputs_GICS$R2
  
}

R2_GICS = data.frame(R2_GICS)
R2_GICS[R2_GICS == 0] = NA

## basic statistics
mean_GICS = colMeans(ff_outputs, na.rm = TRUE, dims = 1)
variance_GICS = colVars(ff_outputs, na.rm = TRUE)
skewness_GICS = colSkewness(ff_outputs, na.rm = TRUE)
kurtosis_GICS = colKurtosis(ff_outputs, na.rm = TRUE)
statistics_GICS = as.matrix(cbind(mean_GICS,variance_GICS,skewness_GICS,kurtosis_GICS))

#### QUESTION 5 ####

clusters = kmeans(R2_GICS, 11) # check NAs

#### QUESTION 6 ####

highest = head(sort(ff_outputs, ascending = FALSE, ff_outputs[,7]), 5)
lowest = head(sort(ff_outputs, ascending = TRUE, ff_outputs[,7]), 5)

#### QUESTION 7 ####


#### QUESTION 8 ####
