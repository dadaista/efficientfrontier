#build a dataframe for sp500 securities ordered by risk/return/rateof change
source("util.R")
df <- read.csv("sp500.csv")
N <- 500


Tickers <- df$Ticker.symbol[1:N]
Securities <-  df$Security[1:N]
df <- data.frame(Ticker    =Tickers,
                 Security  =Securities,
                 Return    =rep(0,N),
                 Volatility=rep(0,N))


for (tk in Tickers){
  load(tk)#fetch cache
  Sys.sleep(2)
}

fm <- function(Ticker){
  x <- cache[[Ticker]]$Adj.Close
  returns <- price2Return(x)
  
  mean(returns)
}

fs <- function(Ticker){
  x <- cache[[Ticker]]$Adj.Close  
  returns <- price2Return(x)
  sd(returns)
}

fr <- function(Ticker){#function for rate of change
  x <- cache[[Ticker]]$Adj.Close  
  r <- rateOfChange(x)
  r[length(r)]#only the last value (most recent rate of change)
}

ym <- sapply(Tickers, fm)
ys <- sapply(Tickers, fs)
yr <- sapply(Tickers, fr)

df$Return = ym
df$Volatility = ys
df$RateOfChange = yr
df
  
df <- df[order(-df$Return),]
df
write.csv(df,"sp500.ret.sd.csv")  

df$Sharpe = df$Return / df$Volatility



df <- df[order(-df$Sharpe),]
df

df2 <- df[df$Return>0,][order(-df$RateOfChange),]
head(df2,10)


