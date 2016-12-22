#build a dataframe for sp500 securities ordered by risk/return/rateof change
source("loader.R")
USECACHE<<-TRUE
df <- read.csv("sp500.csv")
N <- 500


Tickers <- df$Ticker.symbol[1:N]
Securities <-  df$Security[1:N]
df <- data.frame(Ticker    =Tickers,
                 Security  =Securities,
                 Return    =rep(0,N),
                 Volatility=rep(0,N),
                 pGain2pc=rep(0,N),
                 pLoss2pc=rep(0,N)
                 )



for (t in Tickers){
  try({
    prices <- loadMulti(t,to.date = 250)
    rets <- as.returns(prices,lag=10)[,1]
    m <- mean(rets)
    s <- sd(rets)
    P=ecdf(rets) 
    
    df$Return[df$Ticker==t] <- m
    df$Volatility[df$Ticker==t] <- s
    df$pGain2pc[df$Ticker==t] <- 1-P(0.02)
    df$pLoss2pc[df$Ticker==t] <- P(-0.02)
    })
  print(t)
  Sys.sleep(1)
}

df$sharpe <- df$Return / df$Volatility
df <- df[complete.cases(df),]

top <- df[df$Return>0,]

top <- top[top$pLoss2pc<0.11,]#keep low loss risk only
top <-head( top[order(-top$pGain2pc),], 50 )
top




