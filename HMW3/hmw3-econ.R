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
library(timeSeries)
library(knitr)
library(Rfast)
#library(fBasics)

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

#### DAILY ####

## Fama-French Factors ##
todayY <- as.numeric(format(Sys.Date(), "%Y"))
todayM <- as.numeric(format(Sys.Date(), "%m"))

temp <- tempfile()
factors <- read.csv("F-F_Research_Data_5_Factors_2x3_daily.csv", sep = ",", 
                    skip = 3, header = TRUE, 
                    nrows = 7+(todayY-1963)*365+todayM-15)

factors <- factors[factors$X > 20100104,] # select only values after 2010

#### QUESTION 1 (Daily) ####

# calculate difference between total return and risk free rate of return 
# for all stocks
ri_RF = log_returns - factors$RF

## Farma French linear regression function 

linear.regression <- function(ri_RF, factors, start_date, end_date, analysis = 'day'){
  
  # create df to pass into linear regression
  df = data.frame(ri_RF, factors$Mkt.RF, factors$SMB, factors$HML, 
                  factors$RMW, factors$CMA)
  
  if (analysis == 'day'){
    df$date = as.Date(rownames(log_returns))
  }
  else{df$date = seq(as.Date(start_date), as.Date(end_date) - 1, analysis)}
  
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
ff_outputs = matrix(0L, nrow = dim(ri_RF)[2], ncol = 8)
for (i in 1:dim(ri_RF)[2]){ 
  ff_outputs[i,] = linear.regression(ri_RF[,i], factors, start_date, end_date)
}

# rename columns and rows
colnames(ff_outputs) = c('beta0', 'beta1', 'beta2', 'beta3', 'beta4', 'beta5', 'tvalue', 'R2')
rownames(ff_outputs) = c(colnames(stock))
head(ff_outputs)

#### QUESTION 2 (Daily) ####

## basic statistics
mean_ff = colMeans(ff_outputs, na.rm = TRUE, dims = 1)
variance_ff = colVars(ff_outputs, na.rm = TRUE)
skewness_ff = colSkewness(ff_outputs, na.rm = TRUE)
kurtosis_ff = colKurtosis(ff_outputs, na.rm = TRUE)
statistics_ff = as.matrix(cbind(mean_ff,variance_ff,skewness_ff,kurtosis_ff))

# clean(er) table
kable(statistics_ff)

#### QUESTION 3 (Daily) ####

## non-parametrics density


#### QUESTION 4 (Daily) ####

sectors = unique(companies$GICS.Sector) # get the names of all sectors
R2_GICS = data.frame(stock_name = colnames(ri_RF), row.names = colnames(ri_RF)) # initialise data frame

for (sector in sectors){
  
  #sector = 'Information Technology'
  
  # select the relevant stocks 
  relevant_stock = companies[companies$GICS.Sector == sector,]
  relevant_stock = relevant_stock$Tickers
  relevant_stock = paste(relevant_stock, '.Adjusted', sep = "")
  
  # select the corresponding log-returns
  ri_RF_GICS = ri_RF[,relevant_stock]
  
  # compute the FF parameters 
  ff_outputs_GICS = matrix(0L, nrow = dim(ri_RF_GICS)[2], ncol = 8)
  for (i in 1:dim(ri_RF_GICS)[2]){ 
    ff_outputs_GICS[i,] = linear.regression(ri_RF_GICS[,i], factors, start_date, end_date)
  }
  rownames(ff_outputs_GICS) = relevant_stock
  
  # store R2 
  R2_GICS = merge(R2_GICS, data.frame(ff_outputs_GICS[,8]), by.x = 'stock_name', by.y = 0, all.x = TRUE)
  colnames(R2_GICS)[ncol(R2_GICS)] = sector
}

R2_GICS = within(R2_GICS, rm('stock_name')) 

## basic statistics
mean_GICS = colMeans(R2_GICS, na.rm = TRUE, dims = 1)
variance_GICS = colVars(as.matrix(R2_GICS), na.rm = TRUE)
#skewness_GICS = colSkewness(as.matrix(R2_GICS), na.rm = TRUE)
#kurtosis_GICS = colKurtosis(as.matrix(R2_GICS), na.rm = TRUE)
#statistics_GICS = as.matrix(cbind(mean_GICS,variance_GICS,skewness_GICS,kurtosis_GICS))
statistics_GICS = as.matrix(cbind(mean_GICS,variance_GICS))

# clean(er) table
kable(statistics_GICS)

## create a dataframe with all R2 values per company and per sector
R2_GICS_col = data.frame('R2_GICS' = rowSums(R2_GICS, na.rm = TRUE), row.names = colnames(ri_RF))
GICS_match = data.frame('companies' = paste(companies$Tickers, '.Adjusted', sep=''), 'sector' = companies$GICS.Sector)
R2_GICS_col = merge(R2_GICS_col, GICS_match, by.x = 0, by.y = 'companies', all.y = TRUE)

# clean data frame
row.names(R2_GICS_col) = colnames(ri_RF) # row names
R2_GICS_col = within(R2_GICS_col, rm('Row.names')) # remove added columns
R2_GICS_col = R2_GICS_col[order(R2_GICS_col$sector),]

## clustering
clusters = kmeans(R2_GICS_col$R2_GICS, 11)
R2_GICS_col$cluster = as.character(clusters$cluster)

# plot the clusters
ggplot() +
  geom_point(data = R2_GICS_col, mapping = aes(x = row.names(R2_GICS_col), y = R2_GICS, colour = cluster)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  labs(x = 'Company Names', y = 'R² Predictability Score')
ggsave('./plots/predicted-clusters-daily.png')

# plot the clusters
ggplot() +
  geom_point(data = R2_GICS_col, mapping = aes(x = row.names(R2_GICS_col), y = R2_GICS, colour = sector)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  labs(x = 'Company Names', y = 'R² Predictability Score')
ggsave('./plots/sector-clusters-daily.png')

#### QUESTION 5 (Daily) ####

# no clusters
R2_stock = data.frame('R2' = ff_outputs[,8])

ggplot() +
  geom_point(data = R2_stock, mapping = aes(x = row.names(R2_stock), y = R2)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  labs(x = 'Company Names', y = 'R² Predictability Score')
ggsave('./plots/clusters-stock-daily.png')

## clustering
clusters = kmeans(R2_stock$R2, 5)
R2_stock$cluster = as.character(clusters$cluster)

ggplot() +
  geom_point(data = R2_stock, mapping = aes(x = row.names(R2_stock), y = R2, colour = cluster)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  labs(x = 'Company Names', y = 'R² Predictability Score')
ggsave('./plots/predicted-clusters-stock-daily.png')

#### QUESTION 6 (Daily) ####

highest = ff_outputs[order(-ff_outputs[,7]),][1:5,]
lowest = ff_outputs[order(ff_outputs[,7]),][1:5,]

# clean output
kable(highest)
kable(lowest)

#### QUESTION 7 ####

# Analysis monthly data i.e. agregation of the daily log-returns monthly 

#### MONTHLY ####

## Fama-French Factors ##
todayY <- as.numeric(format(Sys.Date(), "%Y"))
todayM <- as.numeric(format(Sys.Date(), "%m"))

temp <- tempfile()
factors <- read.csv("F-F_Research_Data_5_Factors_2x3.csv", sep = ",", 
                    skip = 3, header = TRUE, 
                    nrows = 7+(todayY-1963)*12+todayM-15)
factors <- factors[factors$X > 200912,] # select only values after 2010

#### QUESTION 1 (Monthly) ####

# aggregate values per months
ri = aggregate(log_returns, by=list(month(rownames(log_returns)), 
                                    year(rownames(log_returns))), FUN=SUM)
ri = ri[,3:ncol(ri)]

# calculate difference between total return and risk free rate of return 
# for all stocks
ri_RF = ri - factors$RF

## Farma French linear regression function 
linear.regression <- function(ri_RF, factors, start_date, end_date, analysis = 'months'){
  
  # create df to pass into linear regression
  df = data.frame(ri_RF, factors$Mkt.RF, factors$SMB, factors$HML, 
                  factors$RMW, factors$CMA)
  
  if (analysis == 'day'){
    df$date = as.Date(rownames(log_returns))
  }
  else{df$date = seq(as.Date(start_date), as.Date(end_date) - 1, analysis)}

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
ff_outputs = matrix(0L, nrow = dim(ri_RF)[2], ncol = 8)
for (i in 1:dim(ri_RF)[2]){ 
  ff_outputs[i,] = linear.regression(ri_RF[,i], factors, start_date, end_date)
}

# rename columns and rows
colnames(ff_outputs) = c('beta0', 'beta1', 'beta2', 'beta3', 'beta4', 'beta5', 'tvalue', 'R2')
rownames(ff_outputs) = c(colnames(stock))
head(ff_outputs)

#### QUESTION 2 (Monthly) ####

## basic statistics
mean_ff = colMeans(ff_outputs, na.rm = TRUE, dims = 1)
variance_ff = colVars(ff_outputs, na.rm = TRUE)
skewness_ff = colSkewness(ff_outputs, na.rm = TRUE)
kurtosis_ff = colKurtosis(ff_outputs, na.rm = TRUE)
statistics_ff = as.matrix(cbind(mean_ff,variance_ff,skewness_ff,kurtosis_ff))

# clean(er) table
kable(statistics_ff)

#### QUESTION 3 (Monthly) ####

## non-parametrics density

for (i in 1:dim(ff_outputs)[2]){
  
  
}

library(kdensity)

kde = kdensity(ff_outputs[,1])

plot(kde)
lines(kde, plot_start = TRUE, col = "red")

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

#### QUESTION 4 (Monthly) ####

sectors = unique(companies$GICS.Sector) # get the names of all sectors
R2_GICS = data.frame(stock_name = colnames(ri_RF), row.names = colnames(ri_RF)) # initialise data frame

for (sector in sectors){
  
  # select the relevant stocks 
  relevant_stock = companies[companies$GICS.Sector == sector,]
  relevant_stock = relevant_stock$Tickers
  relevant_stock = paste(relevant_stock, '.Adjusted', sep = "")
  
  # select the corresponding log-returns
  ri_RF_GICS = ri_RF[,relevant_stock]
  
  # compute the FF parameters 
  ff_outputs_GICS = matrix(0L, nrow = length(ri_RF_GICS), ncol = 8)
  for (i in 1:length(ri_RF_GICS)){ 
    ff_outputs_GICS[i,] = linear.regression(ri_RF_GICS[,i], factors, start_date, end_date)
  }
  rownames(ff_outputs_GICS) = relevant_stock
  
  # store R2 
  R2_GICS = merge(R2_GICS, data.frame(ff_outputs_GICS[,8]), by.x = 'stock_name', by.y = 0, all.x = TRUE)
  colnames(R2_GICS)[ncol(R2_GICS)] = sector
}

R2_GICS = within(R2_GICS, rm('stock_name')) 

## basic statistics
mean_GICS = colMeans(R2_GICS, na.rm = TRUE, dims = 1)
variance_GICS = colVars(as.matrix(R2_GICS), na.rm = TRUE)
#skewness_GICS = colSkewness(as.matrix(R2_GICS), na.rm = TRUE)
#kurtosis_GICS = colKurtosis(as.matrix(R2_GICS), na.rm = TRUE)
#statistics_GICS = as.matrix(cbind(mean_GICS,variance_GICS,skewness_GICS,kurtosis_GICS))
statistics_GICS = as.matrix(cbind(mean_GICS,variance_GICS))

# clean(er) table
kable(statistics_GICS)

## create a dataframe with all R2 values per company and per sector
R2_GICS_col = data.frame('R2_GICS' = rowSums(R2_GICS, na.rm = TRUE), row.names = colnames(ri_RF))
GICS_match = data.frame('companies' = paste(companies$Tickers, '.Adjusted', sep=''), 'sector' = companies$GICS.Sector)
R2_GICS_col = merge(R2_GICS_col, GICS_match, by.x = 0, by.y = 'companies', all.y = TRUE)

# clean data frame
row.names(R2_GICS_col) = colnames(ri_RF) # row names
R2_GICS_col = within(R2_GICS_col, rm('Row.names')) # remove added columns
R2_GICS_col = R2_GICS_col[order(R2_GICS_col$sector),]

## clustering
clusters = kmeans(R2_GICS_col$R2_GICS, 11)
R2_GICS_col$cluster = as.character(clusters$cluster)

# plot the clusters
ggplot() +
  geom_point(data = R2_GICS_col, mapping = aes(x = row.names(R2_GICS_col), y = R2_GICS, colour = cluster)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  labs(x = 'Company Names', y = 'R² Predictability Score')
ggsave('./plots/predicted-clusters-monthly.png')

# plot the clusters
ggplot() +
  geom_point(data = R2_GICS_col, mapping = aes(x = row.names(R2_GICS_col), y = R2_GICS, colour = sector)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  labs(x = 'Company Names', y = 'R² Predictability Score')
ggsave('./plots/sector-clusters-monthly.png')

#### QUESTION 5 (Monthly) ####

# no clusters
R2_stock = data.frame('R2' = ff_outputs[,8])

ggplot() +
  geom_point(data = R2_stock, mapping = aes(x = row.names(R2_stock), y = R2)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  labs(x = 'Company Names', y = 'R² Predictability Score')
ggsave('./plots/clusters-stocks-monthly.png')

## clustering
clusters = kmeans(R2_stock$R2, 5)
R2_stock$cluster = as.character(clusters$cluster)

ggplot() +
  geom_point(data = R2_stock, mapping = aes(x = row.names(R2_stock), y = R2, colour = cluster)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  labs(x = 'Company Names', y = 'R² Predictability Score')
ggsave('./plots/predicted-clusters-stocks-monthly.png')


#### QUESTION 6 (Monthly) ####

highest = ff_outputs[order(-ff_outputs[,7]),][1:5,]
lowest = ff_outputs[order(ff_outputs[,7]),][1:5,]

# clean output
kable(highest)
kable(lowest)

#### QUESTION 8 ####
