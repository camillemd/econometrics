## without csv file i.e. create csv file for future use
# set-up df
stock <- tq_get('MMM', from = end_date, to = end_date) # example of stock
stock <- stock[FALSE,]

# extract all stock prices
for (row in companies$Tickers){
  stock_prices <- tq_get(cbind(), from = start_date, to = date)
  stock <- rbind(stock, stock_prices)
}
write.csv(stock,'./data/stock.csv')

stock <- tq_get(get = "stock.prices", companies$Tickers[1], from = end_date, to = end_date, verbose)

## with csv file
# from csv
stock = read.csv('./data/stock.csv')
