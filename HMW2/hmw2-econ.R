# Financial Econometrics - Homework 1 
# 16.05.2020
# Raphael Attali, Camille Morand-Duval, Niels Nicolas, Debdeep Roy

#### INITIALISATION ####

# set dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# libraries
library(tidyquant)
library(ggplot2)

#### DATA ####

start_date = '2016-01-01'
end_date = '2016-04-30' #today

## Yahoo Finance VIX ##

# obtain stock values (if csv has not been created)
VIX = getSymbols('^VIX', from = start_date,to = end_date, src = "yahoo", 
                   verbose = FALSE, auto.assign = FALSE)[,6]
VIX = data.frame(VIX)

# plot
ggplot(VIX, aes(x = as.Date(rownames(VIX)), y = VIX.Adjusted)) + 
  geom_line()  +
  labs(x = "2016", y = "VIX Adjusted") 

## Imported Options Data ##
options = read.csv('./SPX_2016_options_T3_2020.csv', header = FALSE)

#### QUESTION 1 ####

# V1: SPX sector ID
# V2: date 
# V3: maturity
# V4: maturity
# V5: difference
# V6: right to buy 
# V7: option price
# V8: in the money - ask
# V9: out of money - bid
# V10: 
# V11: 
# V12: volume change
# V13: action price
# V14: 
# V15: 
# V16: 
# V17: 

# check NA
colSums(is.na(options))

# replace NA values (NaN) with 0
#options[is.na(options)] = 0
#options[is.na(options)] = median(options$V12, na.rm = TRUE)

# delete NA rows
options = na.omit(options)

#### QUESTION 2 ####
options$price = rowMeans(cbind(options$V8, options$V9))

#### QUESTION 3 ####
options = options[options$price > 0.05,]

#### QUESTION 4 ####
# call
options.out.money.call = options[(options$price < options$V7) & (options$V6 == 1),]
# put
options.out.money.put = options[(options$price > options$V7) & (options$V6 == -1),]
# binding conditions
options.out.money = rbind(options.out.money.call, options.out.money.put)

#### QUESTION 5 ####

# implied volatility function from RiskNeutralVolatilitySkewKurt_JVKR_3
library("pracma")
library("derivmkts")

ivol=function(K,IV,Kall)
{
  Kall[K[length(K)]<Kall]=K[length(IV)];
  Kall[K[1]>Kall]=K[1]
  y= interp1(K,IV,xi=Kall,method="spline");
  
  if (sum(y<0)>0){
    Kall[K[length(K)]<Kall]=K[length(IV)];
    Kall[K[1]>Kall]=K[1]
    y= interp1(K,IV,xi=Kall,method="linear");}
  return(y)
}




#### QUESTION 6 ####


#### QUESTION 7 ####


#### EXTRA ####