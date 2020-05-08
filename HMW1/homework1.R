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

########## Stocks ########## 

# obtain stock values (if csv has not been created)
companies <- GetSP500Stocks()
stock <- NULL

for (row in gsub('[.]', '-', companies$Tickers)){
  #stock_prices <- tq_get(row, from = start_date, to = end_date)
  stock_prices = getSymbols(row, from = start_date, src = "yahoo", 
                            verbose = FALSE, auto.assign = FALSE)[,6]
  stock = cbind(stock, stock_prices)
}
stock = data.frame(stock)
write.csv(stock,'./data/stock.csv')

# load csv (faster when the data has already been downloaded)
stock = read.csv('./data/stock.csv', row.names = 1)


# check NA
sum(is.na(stock))
# starting at first complete line
#complete = min(which(complete.cases(stock)))
#stock = stock[complete:nrow(stock),]

# percentage log return
ln_stock = log(stock) # log price
diff_stock = diff(as.matrix(ln_stock))*100 # percentage log return
rows = dim(diff_stock)[1]

########## SP500 ########## 
library(tidyverse)

SP500 = getSymbols('^GSPC', from = start_date, src = "yahoo", 
                   verbose = FALSE, auto.assign = FALSE)[,6]

# percentage log return 
ln_SP500 = log(SP500)
diff_SP500 = diff(as.matrix(ln_SP500))*100 # percentage log return
#diff_SP500 = data.frame(matrix(unlist(ln_SP500), ncol=length(ln_SP500), byrow=T))
diff_SP500 = data.frame(diff_SP500[(nrow(diff_SP500)-rows+1):nrow(diff_SP500),])
colnames(diff_SP500) <- c('diff_return')


#### QUESTION 1 ####

## basic statistics
mean_stock = colMeans(diff_stock, na.rm = TRUE, dims = 1) #mean(diff_stock, na.rm = TRUE)
variance_stock = colVars(diff_stock, na.rm = TRUE)
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
non_parametric <- function(stock_stats, xname, yname){
  density <- density(stock_stats, na.rm = TRUE)
  normal <- dnorm(density$x, mean(stock_stats, na.rm = TRUE), sd(stock_stats, na.rm = TRUE))

  density_data = tibble(density$x, density$y)
  normal_data = tibble(density$x, normal)
  
  ggplot(density_data, aes(x = density$x, y = density$y)) + 
    geom_point() +
    geom_line(aes(x = density$x, y = normal), data = normal_data, color = 'red') + 
    labs(x = xname, y = yname)
}

# mean
non_parametric(stock_stats = mean_stock, 'Mean', 'Frequency')
# variance
non_parametric(stock_stats = variance_stock, 'Variance', 'Frequency')
# skewness
non_parametric(stock_stats = skewness_stock, 'Skewness', 'Frequency')
# kurtosis
non_parametric(stock_stats = kurtosis_stock, 'Kurtosis', 'Frequency')

#### QUESTION 2 ####

full_df = cbind(diff_SP500, diff_stock)
correlation_matrix = cor(full_df, use="complete.obs")
correlation_SP = correlation_matrix[1,]

# Compute non parametric density and plot the graph
non_parametric(correlation_SP, 'Correlation', 'Frequency')

#### QUESTION 3 ####
##SP500##
diff_SP_500_2007= diff_SP500[year(row.names(diff_SP500)) < "2007",]

##Stock##
diff_stock_2007 = diff_stock[year(row.names(diff_stock)) < "2007",]
diff_stock_2007 = diff_stock_2007[,colSums(is.na(diff_stock_2007))<nrow(diff_stock_2007)]

diff_stock_2007=as.data.frame(diff_stock_2007)
plot(diff_stock_2007$TT.Adjusted)


#Basic Statistics

mean_stock = colMeans(diff_stock_2007, na.rm = TRUE, dims = 1) 
variance_stock = colVars(diff_stock_2007, na.rm = TRUE)
skewness_stock = colSkewness(diff_stock_2007, na.rm = TRUE)
kurtosis_stock = colKurtosis(diff_stock_2007, na.rm = TRUE)
statistics_stock_2007 = as.matrix(cbind(mean_stock,variance_stock,skewness_stock,kurtosis_stock))



# Compute non parametric density and plot the graph
# mean
non_parametric(stock_stats = mean_stock, 'Mean', 'Frequency')
# variance
non_parametric(stock_stats = variance_stock, 'Variance', 'Frequency')
# skewness
non_parametric(stock_stats = skewness_stock, 'Skewness', 'Frequency')
# kurtosis
non_parametric(stock_stats = kurtosis_stock, 'Kurtosis', 'Frequency')

full_df = cbind(diff_SP_500_2007, diff_stock_2007)
correlation_matrix = cor(full_df, use="complete.obs")
correlation_SP = correlation_matrix[1,]


stock_2009 = stock[year(stock$date) < "2009",]

#### QUESTION 4 ####


#### QUESTION 5 ####