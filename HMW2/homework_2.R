library(BatchGetSymbols)
library(rvest)
library(tidyverse)
library(tidyquant)
library(BatchGetSymbols)
library(timeSeries)
library(ggplot2)


start_date = '2016-01-01'
end_date = '2016-04-30' #today

## Stocks ##

# obtain stock values (if csv has not been created)
VIX = getSymbols('^VIX', from = start_date,to = end_date, src = "yahoo", 
                   verbose = FALSE, auto.assign = FALSE)[,6]

VIX_df = data.frame(VIX)
VIX_df$index = as.Date(rownames(VIX_df))

#write.csv(VIX_df,'./data/VIX.csv')
plot(VIX_df$VIX.Adjusted)


ggplot(VIX_df, aes(x = index, y = VIX_df$VIX.Adjusted,group=1)) + 
  geom_line()  +
  labs(x = "date", y = "VIX Adjusted") 


head(VIX_df)
