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



for (t in Tickers){
  try({
    rets <- as.returns(loadMulti(t,to.date = 60))[,1]
    m <- mean(rets)
    s <- sd(rets)
    df$Return[df$Ticker==t] <- m
    df$Volatility[df$Ticker==t] <- s})
  print(t)
  Sys.sleep(1)
}

df$sharpe <- df$Return / df$Volatility
df <- df[complete.cases(df),]

top <- df[df$Return>0,]
#keep top 25% volatility
top <- top[top$Volatility<quantile(top$Volatility,0.25),]
top <-head( top[order(-top$sharpe),], 50 )
top

top$Ticker <- as.character(top$Ticker)
bestList=list()
volat=c()
for(i in 1:300){
  tickers <- sample(top$Ticker,5)
  tickers
  prices <- loadMulti(tickers,to.date = 60)
  rets <- as.returns(prices)
  portfolios <- generatePortfolios(rets,250)
  best <- bestPortfolio(portfolios,expected = 0.001)
  best
  bestList[[i]] <- best
  if(length(best$sd) != 0)
    volat <- c(volat,best$sd)
  else volat <- c(volat,NA)
}

bestList
superbest <- bestList[[which.min(volat)]]
superbest



