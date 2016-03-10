# symbols <- c("IAU","FB","T","BTCUSD")
# Var1 Var2 Var3 Var4        risk       mean
# 0.16 0.08  0.6 0.16 0.008179422 0.00200309

library(ggplot2)

days=c("01","02","03","04","05","06","07","08","09",as.character(10:31))
november <- paste(2015,"11",days[1:30],sep = "-")
december <- paste(2015,"12",days[1:31],sep = "-")
january = paste(2016,"01",days[1:31],sep = "-")
february = paste(2016,"02",days[1:20],sep = "-")
february



placeOrders <- function(fund,symbols,date,values){

  
  p <- pricesAtDate(date,symbols)
  p
  qtys = values / p
  
  N <-  nrow(fund)
  fund <- rbind(fund,fund[N,])
  fund[N+1,"Date"] <- date
  fund[N+1,symbols] <- fund[N,symbols]+qtys
  fund[N+1,"cash"] <- fund[N+1,"cash"] - sum(values)
  
  fund
}


equityByDate <- function(fund,date){
  
  df <- fund[fund$Date<=date,]
  df
  N <- nrow(df)
  Qty <- df[N,2:5]
  Qty <- data.matrix(Qty)
  
  k <-  length(names(fund))
  symbols <- names(fund)[c(-1,-k)] # remove all non equity symbols
  symbols
  Price <- pricesAtDate(date,symbols)
  Price
  value <- Price %*% t(Qty)
  value <- value + df$cash[N]
  value
}


date = "2015-11-01"
fund <-   data.frame(
  Date=date,
  IAU=0,
  FB=0,
  T=0,
  BTCUSD=0,
  cash=10000)
fund


equityOverPeriod <- Vectorize(equityByDate,"date")

fund <- placeOrders(fund,
                    c("IAU","FB","T","BTCUSD"),
                    "2015-11-02",
                    c(1600,800,6000,1600))

fund

equity <- equityOverPeriod(fund,c(november,december,february))
equity

equity.norm <- equity / equity[1]
benchmark <- load("SPY",length(equity))$Adj.Close
benchmark.norm <- benchmark / benchmark[1]
df <- data.frame(equity=equity.norm,benchmark=benchmark.norm)

df
plot(equity.norm,t='l',ylim = c(0.8,1.15),col="blue")
lines(benchmark.norm,col="red")

eq.ret <- getReturns(equity)
eq.ret
bm.ret <- getReturns(benchmark)
bm.ret
Beta <- beta(eq.ret,eq.ret)
Alpha <- alpha(tail(equity.norm,1),tail(benchmark.norm,1),Beta)
