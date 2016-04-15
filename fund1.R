


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
april <-  paste(2016,"04",days[1:12],sep = "-")




fund <-   data.frame(Date = "2016-02-01",cash=10000)
#Febr
# c("IAU","FB","T","BTCUSD"),
# c(0.16,0.08,0.6,0.16))
fund <- topUp(fund,"2016-02-01","T",6000)
fund <- topUp(fund,"2016-02-01","IAU",1600)
fund <- topUp(fund,"2016-02-01","FB",800)
fund <- topUp(fund,"2016-02-01","BTCUSD",1600)
fund
#March 
# IAU       TSN         T       ARG     BTCUSD        risk        mean
# 0.1782578 0.2362685 0.3293723 0.1566057 0.09949573 0.006839026 0.002518169
fund <- topUp(fund,"2016-03-01","T",3293)
fund <- topUp(fund,"2016-03-01","IAU",1782)
fund <- topUp(fund,"2016-03-01","TSN",2363)
fund <- topUp(fund,"2016-03-01","ARG",1566)
fund <- topUp(fund,"2016-03-01","BTCUSD",990)
fund <- topUp(fund,"2016-03-01","FB",0)

fund

period <- c(february,
            march,
            april)
equity <- equityOverPeriod(fund,period)
equity

benchmark <- load("SPY")

commonDates <- intersect(equity$Date,benchmark$Date)
equity <- equity[equity$Date %in% commonDates,]
equity$equity <- equity$equity / equity$equity[1]


benchmark=benchmark[benchmark$Date %in% commonDates,c("Date","Adj.Close")]
benchmark$Adj.Close <- benchmark$Adj.Close / benchmark$Adj.Close[1]
df <- data.frame(Date=commonDates,equity=equity$equity,benchmark=benchmark$Adj.Close)

df
plot(df$equity,t='l',ylim = c(0.8,1.15),col="blue", xaxt="n")
lines(df$benchmark,col="red")
L <- round(nrow(df) / 10)
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

