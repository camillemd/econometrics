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
