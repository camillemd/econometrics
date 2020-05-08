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

## Stocks ##

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

## SP500 ##

SP500 = getSymbols('^GSPC', from = start_date, src = "yahoo", 
                   verbose = FALSE, auto.assign = FALSE)[,6]

# percentage log return 
ln_SP500 = log(SP500)
diff_SP500 = diff(as.matrix(ln_SP500))*100 # percentage log return
diff_SP500 = data.frame(diff_SP500[(nrow(diff_SP500)-rows+1):nrow(diff_SP500),])

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
non_parametric <- function(stock_stats, xname, yname, year){
  density <- density(stock_stats, na.rm = TRUE)
  normal <- dnorm(density$x, mean(stock_stats, na.rm = TRUE), sd(stock_stats, na.rm = TRUE))

  density_data = tibble(density$x, density$y)
  normal_data = tibble(density$x, normal)
  
  ggplot(density_data, aes(x = density$x, y = density$y, color = 'Non-Parametric')) + 
    geom_line() +
    geom_line(aes(x = density$x, y = normal, color = 'Normal'), data = normal_data) + 
    labs(x = xname, y = yname) + 
    scale_color_manual(name="Legend", values = c("black","red")) + 
    theme(legend.position = c(0.985, 0.985), legend.justification = c("right", "top"))
  
  ggsave(paste('./plots/', xname, year, ".png", sep = ''))
}

# mean
non_parametric(stock_stats = mean_stock, 'Mean', 'Frequency', 'All')
# variance
non_parametric(stock_stats = variance_stock, 'Variance', 'Frequency', 'All')
# skewness
non_parametric(stock_stats = skewness_stock, 'Skewness', 'Frequency', 'All')
# kurtosis
non_parametric(stock_stats = kurtosis_stock, 'Kurtosis', 'Frequency', 'All')

#### QUESTION 2 ####

full_df = cbind(diff_SP500, diff_stock)
correlation_matrix = cor(full_df, use="complete.obs")
correlation_SP = correlation_matrix[1,]

# compute non-parametric density and plot the graph
non_parametric(correlation_SP, 'Correlation', 'Frequency', 'All')

#### QUESTION 3 ####

## 2007 ##

# SP500 #
diff_SP_500_2007= diff_SP500[year(row.names(diff_SP500)) < "2007",]

# stocks # 
diff_stock_2007 = diff_stock[year(row.names(diff_stock)) < "2007",]
diff_stock_2007 = diff_stock_2007[,colSums(is.na(diff_stock_2007))<nrow(diff_stock_2007)]
diff_stock_2007 = as.data.frame(diff_stock_2007)

# basic statistics
mean_stock_2007 = colMeans(diff_stock_2007, na.rm = TRUE, dims = 1) 
variance_stock_2007 = colVars(diff_stock_2007, na.rm = TRUE)
skewness_stock_2007 = colSkewness(diff_stock_2007, na.rm = TRUE)
kurtosis_stock_2007 = colKurtosis(diff_stock_2007, na.rm = TRUE)
statistics_stock_2007 = as.matrix(cbind(mean_stock_2007,variance_stock_2007,
                                        skewness_stock_2007,kurtosis_stock_2007))

# compute non-parametric density and plot the graph
# mean
non_parametric(stock_stats = mean_stock_2007, 'Mean', 'Frequency', '2007')
# variance
non_parametric(stock_stats = variance_stock_2007, 'Variance', 'Frequency', '2007')
# skewness
non_parametric(stock_stats = skewness_stock_2007, 'Skewness', 'Frequency', '2007')
# kurtosis
non_parametric(stock_stats = kurtosis_stock_2007, 'Kurtosis', 'Frequency', '2007')

# correlation
full_df_2007 = cbind(diff_SP_500_2007, diff_stock_2007)
correlation_matrix_2007 = cor(full_df_2007, use="complete.obs")
correlation_SP_2007 = correlation_matrix_2007[1,]

# compute non parametric density and plot the graph
non_parametric(correlation_SP_2007, 'Correlation', 'Frequency', '2007')

## 2009 ##

# SP500#
diff_SP_500_2009 = diff_SP500[year(row.names(diff_SP500)) > "2009",]

# stocks #
diff_stock_2009 = diff_stock[year(row.names(diff_stock)) > "2009",]
diff_stock_2009 = diff_stock_2009[,colSums(is.na(diff_stock_2009))<nrow(diff_stock_2009)]
diff_stock_2009 = as.data.frame(diff_stock_2009)

# basic statistics
mean_stock_2009 = colMeans(diff_stock_2009, na.rm = TRUE, dims = 1) 
variance_stock_2009 = colVars(diff_stock_2009, na.rm = TRUE)
skewness_stock_2009 = colSkewness(diff_stock_2009, na.rm = TRUE)
kurtosis_stock_2009 = colKurtosis(diff_stock_2009, na.rm = TRUE)
statistics_stock_2009 = as.matrix(cbind(mean_stock_2009,variance_stock_2009,
                                        skewness_stock_2009,kurtosis_stock_2009))

# compute non parametric density and plot the graph
# mean
non_parametric(stock_stats = mean_stock_2009, 'Mean', 'Frequency', '2009')
# variance
non_parametric(stock_stats = variance_stock_2009, 'Variance', 'Frequency', '2009')
# skewness
non_parametric(stock_stats = skewness_stock_2009, 'Skewness', 'Frequency', '2009')
# kurtosis
non_parametric(stock_stats = kurtosis_stock_2009, 'Kurtosis', 'Frequency', '2009')

# correlation
full_df_2009 = cbind(diff_SP_500_2009, diff_stock_2009)
correlation_matrix_2009 = cor(full_df_2009, use="complete.obs")
correlation_SP_2009 = correlation_matrix_2009[1,]

# compute non parametric density and plot the graph
non_parametric(correlation_SP_2009, 'Correlation', 'Frequency', '2009')

#### QUESTION 4 ####

# Kolmogorov-Smirnov test
ks.test(mean_stock_2007, mean_stock_2009)
ks.test(variance_stock_2007, variance_stock_2009)
ks.test(skewness_stock_2007, skewness_stock_2009)
ks.test(kurtosis_stock_2007, kurtosis_stock_2009)
ks.test(correlation_SP_2007, correlation_SP_2009)

#### QUESTION 5 ####

## 2019 ##

# SP500 #
diff_SP_500_2019= diff_SP500[year(row.names(diff_SP500)) == "2019",]
return_SP_2019 = exp(sum(diff_SP_500_2019, na.rm = TRUE)/100)

# stocks # 
diff_stock_2019 = diff_stock[year(row.names(diff_stock)) == "2019",]
diff_stock_2019 = diff_stock_2019[,colSums(is.na(diff_stock_2019))<nrow(diff_stock_2019)]
diff_stock_2019 = as.data.frame(diff_stock_2019)
return_2019 = exp(colSums(diff_stock_2019, na.rm = TRUE)/100)

# top 10 performing stocks
top_2019 = return_2019[order(return_2019, decreasing = TRUE)]

# worst 10 performing stocks
worst_2019 = return_2019[order(return_2019, decreasing = FALSE)]


## 2020 ##

# SP500 #
diff_SP_500_2020= diff_SP500[year(row.names(diff_SP500)) == "2020",]
return_SP_2020 = exp(sum(diff_SP_500_2020, na.rm = TRUE)/100)

# stocks # 
diff_stock_2020 = diff_stock[year(row.names(diff_stock)) == "2020",]
diff_stock_2020 = diff_stock_2020[,colSums(is.na(diff_stock_2020))<nrow(diff_stock_2020)]
diff_stock_2020 = as.data.frame(diff_stock_2020)
return_2020 = exp(colSums(diff_stock_2020, na.rm = TRUE)/100)

# top 10 performing stocks
top_2020 = return_2020[order(return_2020, decreasing = TRUE)]

# worst 10 performing stocks
worst_2020 = return_2020[order(return_2020, decreasing = FALSE)]