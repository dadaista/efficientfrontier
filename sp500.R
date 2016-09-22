#build a dataframe for sp500 securities ordered by risk/return/rateof change
source("loader.R")
source("frontier.R")
USECACHE<<-TRUE
df <- read.csv("sp500.csv")
N <- 500


Tickers <- df$Ticker.symbol[1:N]
Securities <-  df$Security[1:N]
df <- data.frame(Ticker    =Tickers,
                 Security  =Securities,
                 Return    =rep(0,N),
                 Volatility=rep(0,N))




for(i in 1:3){
  tickers <- sample(df$Ticker,6)
  tickers
  prices <- loadMulti(tickers,to.date = 60)
  rets <- as.returns(prices)
  portfolios <- generatePortfolios(rets,250)
  best <- bestPortfolio(portfolios,expected = 0.01)
  best
}



  
df <- df[order(-df$Return),]
df
write.csv(df,"sp500.ret.sd.csv")  

df$Sharpe = df$Return / df$Volatility



df <- df[order(-df$Sharpe),]
df

df2 <- df[df$Return>0,][order(-df$RateOfChange),]
head(df2,10)


