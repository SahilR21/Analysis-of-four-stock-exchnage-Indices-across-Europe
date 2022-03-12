#PROJECT - Data ON 4 stock exchange indices across Europe
#install.packages("xts")
#install.packages("qrmdata")
#let's load the library that will be useful
require(zoo)
require(qrmdata)
require(xts)
library(datasets)

# load the inbuild data from the datasets library 
#data - Daily Closing Prices of Major European StockIndices, 1991-1998
EuStockMarkets = EuStockMarkets
print(EuStockMarkets)
#find the return data change in the induces
EuStockMarkets.price = as.zoo(EuStockMarkets)
EuStockMarkets.return = diff(log(EuStockMarkets.price))[-1]*100
EuStockMarkets.return

# let's plot the price levels and returns
png("Stock Price level - 1.png")
plot(EuStockMarkets.price, xlab = "", main = "Stock price levels")
dev.off()
png("Stock Returns - 2.png")
plot(EuStockMarkets.return, xlab = "", main = "Stock returns")
dev.off()
## observation - have a volatility clustering and heavility weighted tails

#let's loo at the cross correlation among one pair of indices to see similarity between on esignal and time lag for return and absolute returns
png("Return DAX vs. CAC -3.png")
ccf(EuStockMarkets.return[,1], EuStockMarkets.return[,2], main = "Return DAX vs. CAC",lag.max = 20,ylab ="",xlab = "",col = "blue",ci.col = "red")
dev.off()
## absolute return
png("Absolute Return DAX vs CAC -4.png")
ccf(abs(EuStockMarkets.return[,1]),abs(EuStockMarkets.return[,2]),
    main = "Absolute returns DAX vs CAC",lag.max = 20, ylab = "",xlab = "",
    col = "blue",ci.col = "red")
dev.off()
## here some small raw correlation across time with raw return. It seems to be the volatility correlation clustering using return sizes

#let's look into rolling correlation so get beeter idea
corr.rolling <- function(x) {
  dim <- ncol(x)
  corr.r <- cor(x)[lower.tri(diag(dim),diag = FALSE)]
  return(corr.r)
}


## we need to answer a question that - what is history of correlation and from the history, the pattern of correlation in the UK and EU stock market?
#if there is any history with pattern, then we haev to manage the risk that conducting business in one country will definitely affect business in another.
#implication is that bad wthings will be followed by more bad things more oftern than good things.

corr.returns = rollapply(EuStockMarkets.return,width = 250, corr.rolling, align = "right",by.column = FALSE)
colnames(corr.returns) = c("DAX & CAC","DAX & SMI", "DAX & FTSE", "CAC & SMI","CAC & FTSE", "SMI & FTSE")
plot(corr.returns, xlab = "", main = "")
###here also volatitlity cllustering form bunching up of absolute size of return.












       




