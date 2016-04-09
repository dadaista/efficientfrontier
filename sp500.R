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

# 1 Apr 1016
# X Ticker                  Security      Return Volatility    sharpe
# 17  85    CPB             Campbell Soup 0.002995386 0.01165494 0.2570057
# 5  453    TSN               Tyson Foods 0.004810993 0.01996419 0.2409811
# 18 385      O Realty Income Corporation 0.002933178 0.01274160 0.2302048
# 31 125     ED       Consolidated Edison 0.002329212 0.01068567 0.2179754
# 11 147     DG            Dollar General 0.003739157 0.01769573 0.2113028
# 40 113    CMS                CMS Energy 0.002169152 0.01035977 0.2093823
