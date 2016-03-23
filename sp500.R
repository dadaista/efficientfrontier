#build a dataframe for sp500 securities ordered by risk/return
source("util.R")
df <- read.csv("sp500.csv")
N <- 500


Tickers <- df$Ticker.symbol[1:N]
Securities <-  df$Security[1:N]
df <- data.frame(Ticker    =Tickers,
                 Security  =Securities,
                 Return    =rep(0,N),
                 Volatility=rep(0,N))

fm <- function(Ticker){
  x <- load(Ticker,n=90,usecache = TRUE)$Adj.Close
  returns <- getReturns(x)
  Sys.sleep(1)
  mean(returns)
}

fs <- function(Ticker){
  x <- load(Ticker,n=90,usecache = TRUE)$Adj.Close
  returns <- getReturns(x)
  sd(returns)
}

ym <- sapply(Tickers, fm)
ys <- sapply(Tickers, fs)

df$Return = ym
df$Volatility = ys
df
  
df <- df[order(-df$Return),]
df
write.csv(df,"sp500.ret.sd.csv")  
