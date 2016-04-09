#Febr
# c("IAU","FB","T","BTCUSD"),
# c(0.16,0.08,0.6,0.16))

#March 
# IAU       TSN         T       ARG     BTCUSD        risk        mean
# 0.1782578 0.2362685 0.3293723 0.1566057 0.09949573 0.006839026 0.002518169

#2 Apr 16
# IAU        TSN       CPB       ARG       BTCUSD        ED        risk        mean    sharpe
# 0.1822796 0.10541063 0.2105653 0.2350269 0.06047626 0.2062413 0.005150772 0.002315646 0.4495726



library(ggplot2)
source("util.R")
days=c("01","02","03","04","05","06","07","08","09",as.character(10:31))
november <- paste(2015,"11",days[1:30],sep = "-")
december <- paste(2015,"12",days[1:31],sep = "-")
january = paste(2016,"01",days[1:31],sep = "-")
february = paste(2016,"02",days[1:29],sep = "-")
march <-  paste(2016,"03",days[1:31],sep = "-")






fund <- topUp(fund,"2016-01-01","T",800)
fund

period <- c(november,
            december,
            january,
            february,
            march)
equity <- equityOverPeriod(fund,period)
equity

equity.norm <- equity / equity[1]
benchmark <- load("SPY",length(equity))$Adj.Close
benchmark.norm <- benchmark / benchmark[1]
df <- data.frame(equity=equity.norm,benchmark=benchmark.norm)

df
plot(equity.norm,t='l',ylim = c(0.8,1.15),col="blue", xaxt="n")
lines(benchmark.norm,col="red")
L <- round(length(equity) / 10)
axis(side=1,labels = period[1:10 * L], at=1:10 * L)

legend("topleft",
       c("fund","benchmark:SPY"),
       lty=1,
       col=c("blue","red"))


eq.ret <- getReturns(equity)
eq.ret
bm.ret <- getReturns(benchmark)
bm.ret
Beta <- beta(eq.ret,eq.ret)
Alpha <- alpha(tail(equity.norm,1),tail(benchmark.norm,1),Beta)

