#build a dataframe for sp500 securities ordered by risk/return/rateof change
source("loader.R")
USECACHE<<-TRUE
df <- read.csv("sp500.csv")
N <- 500

#moving average
ma <- function(x,n=15){filter(x,rep(1/n,n), sides=1)}

Tickers <- df$Ticker.symbol[1:N]
Securities <-  df$Security[1:N]
df <- data.frame(Ticker    =Tickers,
                 Security  =Securities,
                 Return    =rep(0,N),
                 Volatility=rep(0,N),
                 pGain2pc=rep(0,N),
                 pLoss2pc=rep(0,N),
                 MAratio=rep(0,N)
                 )



for (t in Tickers){
  try({
    prices <- loadMulti(t,to.date = 250)
    rets <- as.returns(prices,lag=10)[,1]
    m <- mean(rets)
    s <- sd(rets)
    P=ecdf(rets) 
    maRatio <- as.numeric(tail(prices[t],1) / tail(ma(prices[t]),1))
    
    df$Return[df$Ticker==t] <- round(m,2)
    df$Volatility[df$Ticker==t] <- round(s,2)
    df$pGain2pc[df$Ticker==t] <- round(1-P(0.02) , 2)
    df$pLoss2pc[df$Ticker==t] <- round(P(-0.02) , 2)
    df$MAratio[df$Ticker==t] <- round(maRatio,2)
     })
  print(t)
  Sys.sleep(1)
}

df <- df[complete.cases(df),]

df[order(df$MAratio),]

# top <- df[df$Return>0,]
# 
# top <- top[top$pLoss2pc<0.11,]#keep low loss risk only
# top <-head( top[order(-top$pGain2pc),], 50 )
# top

names(top) <- c("Ticker","Description","Return 10days",
                "Volatility","prob +2%","prob -2%")

write.csv(top,"top.csv")




