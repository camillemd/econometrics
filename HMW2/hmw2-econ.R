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
end_date = '2016-04-30' 

## Yahoo Finance VIX ##

# obtain stock values (if csv has not been created)
VIX = getSymbols('^VIX', from = start_date,to = end_date, src = "yahoo", 
                   verbose = FALSE, auto.assign = FALSE)[,6]
VIX = data.frame(VIX)

# plot
ggplot(VIX, aes(x = as.Date(rownames(VIX)), y = VIX.Adjusted)) + 
  geom_line()  +
  labs(x = "2016", y = "VIX Adjusted")
ggsave('./VIX.png')

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

#### JEROEN'S ####

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

RiskNeutralVolatilitySkewKurt_JVKR=function(Kvector, IVvector, S0, T, r)
{
  kmin=.1*S0;
  kmax=3.5*S0;
  
  V1= function(K){((2*(1-log(K/S0)))*(bscall(S0, K, ivol(Kvector,IVvector,K), r, T, 0))/K^2);}
  V2= function(K){((2*(1+log(S0/K)))*(bsput(S0, K, ivol(Kvector,IVvector,K), r, T, 0))/K^2);}
  
  #W1= function(K){(((6*log(K/S0)-3*log(K/S0)^2))*(bscall(S0, K, ivol(Kvector,IVvector,K), r, T, 0))/K^2);}
  #W2= function(K){(((6*log(S0/K)+3*log(S0/K)^2))*(bsput(S0, K, ivol(Kvector,IVvector,K), r, T, 0))/K^2);}
  
  #X1= function(K){(((12*log(K/S0)^2 - 4*log(K/S0)^3))*(bscall(S0, K, ivol(Kvector,IVvector,K), r, T, 0))/K^2);}
  #X2= function(K){(((12*log(S0/K)^2 + 4*log(S0/K)^3))*(bsput(S0, K, ivol(Kvector,IVvector,K), r, T, 0))/K^2);}
  
  V = ( integrate(V1,S0,kmax,subdivisions=2000, rel.tol = .Machine$double.eps, stop.on.error = FALSE)[[1]]
        + integrate(V2,kmin,S0,subdivisions=2000, rel.tol = .Machine$double.eps, stop.on.error = FALSE)[[1]]);
  #W = ( integrate(W1,S0,kmax,rel.tol = .Machine$double.eps)[[1]]-integrate(W2,kmin,S0,rel.tol = .Machine$double.eps)[[1]]);
  #X = ( integrate(X1,S0,kmax,rel.tol = .Machine$double.eps)[[1]]+integrate(X2,kmin,S0,rel.tol = .Machine$double.eps)[[1]]);
  
  #mu=exp(r*T)-1-exp(r*T)*V/2-exp(r*T)*W/6-exp(r*T)*X/24;
  #print(c(V,W,X,mu))
  
  vol=(1/T * V) ^ .5;
  #skew=( exp(r*T)*W - 3*mu*exp(r*T)*V + 2*mu^3) / (exp(r*T)*V - mu^2) ^ (3/2);
  #kurt=( exp(r*T)*X - 4*mu*exp(r*T)*W + 6*exp(r*T)*mu^2*V - 3*mu^4) / (exp(r*T)*V - mu^2)^2;
  
  #return(list(vol,skew,kurt));
  return(vol)
}

#### QUESTION 5 ####

i = 1 
implied_vol = c()
maturity_vector = c()

# each day
for (day in unique(options.out.money$V2)){
  selection_day = options.out.money[(options.out.money$V2 == day),]
  
  # each maturity
  for (maturity in unique(selection_day$V5)){
    selection_mat = selection_day[(selection_day$V5 == maturity),]

    K_vector = selection_mat$V7
    IV_vector = selection_mat$V12
    S0 = selection_mat$V13[1]
    r = selection_mat$V14[1]
    T = maturity / 365
    
    implied_vol[c(i)] = RiskNeutralVolatilitySkewKurt_JVKR(K_vector, IV_vector, S0, T, r)
    maturity_vector[c(i)] = T*365
    i = i + 1
  }
}

# plot 
df = data.frame(maturity_vector, implied_vol)

ggplot(df, aes(x = maturity_vector, y = implied_vol)) + 
  geom_point() + 
  labs(x = "Maturity [days]", y = "Implied Volatility")
ggsave('./implied-volatility.png')

#### QUESTION 6 ####
j = 1
IV_30days = c()
days = c()

# each day
for (day in unique(options.out.money$V2)){
  selection_day = options.out.money[(options.out.money$V2 == day),]
  days[c(j)] = day 
  implied_vol = c()
  maturity_vector = c()
  i = 1
  
  # each maturity
  for (maturity in unique(selection_day$V5)){
    selection_mat = selection_day[(selection_day$V5 == maturity),]
    
    K_vector = selection_mat$V7
    IV_vector = selection_mat$V17
    S0 = selection_mat$V13[1]
    r = selection_mat$V14[1]
    T = maturity / 365
    
    implied_vol[c(i)] = RiskNeutralVolatilitySkewKurt_JVKR(K_vector, IV_vector, S0, T, r)
    maturity_vector[c(i)] = T*365
    i = i + 1
  }
  
  IV_30days[c(j)] = interp1(maturity_vector,implied_vol,xi=30,method="spline")
  j = j + 1
}

# plot 
df = data.frame(seq(1, length(IV_30days), 1),IV_30days)

plot(IV_30days)

ggplot(df, aes(x = seq.1..length.IV_30days...1., y = IV_30days)) + 
  geom_point() + 
  labs(x= "Day", y = "Interpolation of the implied volatility")
ggsave('./30day.png')

#### QUESTION 7 ####
VIX$day = seq(1, length(IV_30days), 1)

ggplot(df, aes(x = seq.1..length.IV_30days...1., y = IV_30days, color = 'calculated')) + 
  geom_point() + 
  geom_point(aes(x = day, y = VIX.Adjusted, color = 'VIX'), data = VIX) + 
  labs(x= "Day", y="Volatility") + 
  scale_color_manual(name="Legend", values = c("black","red"))
ggsave('./VIX_calculated.png')

correlation = cor(df$IV_30days,VIX$VIX.Adjusted, method = "pearson")

#### EXTRA ####

#### IMPLIED VOLATILITY ESTIMATION ####
library(roptions)
df = options.out.money[1,]

tol = 1e-3
max_iter = 1000

implied.volatility = function(tol, max_iter, df){
  
  epsilon = 1
  count = 0
  vol = 0.5
  
  # variables
  K = df$V7
  S0 = df$V13
  r = df$V14
  T = df$V5 / 365
  flag = df$V6
  price = df$price
  
  while (epsilon > tol){
    count = count + 1
  
    # break loop for max number of iterations
    if (count >= max_iter){break}
    
    if(flag == 1){
      
      origin_vol = vol
      C = bscall(S0, K, origin_vol, r, T, 0) - price
      vega = call.vega(S0, K, T, origin_vol, r, 0)[1,]
      vol = - C / vega + vol
      
      epsilon = abs((vol - origin_vol)/origin_vol)
      print(vol)
    }
    
    else{
      origin_vol = vol
      
      P = bsput(S0, K, origin_vol, r, T, 0) - price
      vega = as.put.vega(S0, K, T, origin_vol, r, 0)[1,]
      vol = - P / vega + vol
      
      epsilon = abs((vol - origin_vol)/origin_vol)
    }
  }
  
  return(vol)
  
}
  
implied_vol = implied.volatility(tol, max_iter, df)

#### TEST ####
options.out.money = rbind(options.out.money.call, options.out.money.put)
options.out.money = options.out.money[1:1000,]

i = 1 
implied_vol = c()
maturity_vector = c()

# each day
for (day in unique(options.out.money$V2)){
  selection_day = options.out.money[(options.out.money$V2 == day),]
  
  # each maturity
  for (maturity in unique(selection_day$V5)){
    selection_mat = selection_day[(selection_day$V5 == maturity),]
    
    K_vector = selection_mat$V7
    IV_vector = selection_mat$V17
    S0 = selection_mat$V13[1]
    r = selection_mat$V14[1]
    T = maturity / 365
    
    implied_vol[c(i)] = RiskNeutralVolatilitySkewKurt_JVKR(K_vector, IV_vector, S0, T, r)
    maturity_vector[c(i)] = T*365
    i = i + 1
  }
}

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

RiskNeutralVolatilitySkewKurt_JVKR=function(Kvector, IVvector, S0, T, r)
{
  kmin=.1*S0;
  kmax=3.5*S0;
  
  V1= function(K){((2*(1-log(K/S0)))*(bscall(S0, K, ivol(Kvector,IVvector,K), r, T, 0))/K^2);}
  V2= function(K){((2*(1+log(S0/K)))*(bsput(S0, K, ivol(Kvector,IVvector,K), r, T, 0))/K^2);}
  
  V = ( integrate(V1,S0,kmax,subdivisions=2000, rel.tol = .Machine$double.eps, stop.on.error = FALSE)[[1]]
        + integrate(V2,kmin,S0,subdivisions=2000, rel.tol = .Machine$double.eps, stop.on.error = FALSE)[[1]]);

  vol=(1/T * V) ^ .5;

  return(vol)
}
