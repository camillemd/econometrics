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

#### DATA ####

start_date = '2010-01-01'
end_date = '2020-04-01' #toString(Sys.Date()) #today

## Stocks ##

# obtain stock values (if csv has not been created)
companies <- GetSP500Stocks()
stock <- NULL

for (row in gsub('[.]', '-', companies$Tickers)){
  stock_prices = getSymbols(row, from = start_date, to = end_date, src = "yahoo", 
                            verbose = FALSE, auto.assign = FALSE)[,6]
  stock = cbind(stock, stock_prices)
}
stock = data.frame(stock)

## Fama-French Factors ##
todayY <- as.numeric(format(Sys.Date(), "%Y"))
todayM <- as.numeric(format(Sys.Date(), "%m"))

temp <- tempfile()
factors <- read.csv("F-F_Research_Data_Factors.CSV", sep = ",", 
                    skip = 3, header = TRUE, nrows = 6+(todayY-1927)*12+todayM-2)
factors <- factors[factors$X > 200912,] # select only values after 2010

# check NA
sum(is.na(stock))

# percentage log return
ln_stock = log(stock) # log price
diff_stock = diff(as.matrix(ln_stock))*100 # percentage log return
#rows = dim(diff_stock)[1]

#### USEFUL FUNCTIONS ####

OLS <- function(y, X){
  
  T = dim(X)[1]
  K = dim(X)[2]+1
  X = cbind(rep(1,T),X)
  b = solve(t(X)%*%X)%*%(t(X)%*%y)
  res = y-X%*%b
  
  sig2 = 1/(T-K)* (t(res)%*%res)
  var_b = diag(solve(t(X)%*%X)%*%diag(rep(sig2,K)))
  
  std_b = sqrt(var_b)
  tstat = b/std_b
  pval = 2 * (1-pnorm(abs(tstat)))
  
  R2 = 1 - (t(res)%*%res)/(t(y-mean(y))%*%(y-mean(y)))
  print(c("R2",R2))
  
  rho_1 = 1/T*sum(res[2:T]*res[1:T-1])
  print(c("rho_1(u)",rho_1))
  
  DW=sum((res[2:T]-res[1:T-1])^2)/sum(res*res)
  print(c("DW",DW))
  
  z_DW=sqrt(T)*(DW/2 - 1)
  print(c("z_DW",z_DW))
  
  pval_z = 2 * (1-pnorm(abs(z_DW)))
  print(c("pval z_DW",pval_z))
  
  value <- cbind(b,std_b,tstat,pval, R2)
  return(value)
}

#### QUESTION 1 ####

# aggregate values per months
ri = aggregate(diff_stock, by=list(month(rownames(diff_stock)), year(rownames(diff_stock))), FUN=SUM)
rownames(ri) = paste(ri$Group.2, ri$Group.1) # change index

# calculate difference between total return and risk free rate of return
ri_Rf = ri[,3:ncol(ri)] - factors$RF 

ff = OLS(ri_Rf[,1],cbind(factors$Mkt.RF,factors$SMB,factors$HML))

## Farma-French Factors ##




#### QUESTION 2 ####


#### QUESTION 3 ####


#### QUESTION 4 ####


#### QUESTION 5 ####


#### QUESTION 6 ####


#### QUESTION 7 ####


#### QUESTION 8 ####
