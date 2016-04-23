
library(ggplot2)
source("util.R")
days=c("01","02","03","04","05","06","07","08","09",as.character(10:31))
november <- paste(2015,"11",days[1:30],sep = "-")
december <- paste(2015,"12",days[1:31],sep = "-")
january = paste(2016,"01",days[1:31],sep = "-")
february = paste(2016,"02",days[1:29],sep = "-")
march <-  paste(2016,"03",days[1:31],sep = "-")
april <-  paste(2016,"04",days[1:22],sep = "-")




fund <-   data.frame(Date = "2016-01-01",cash=10000)
#Febr
# c("IAU","FB","T","BTCUSD"),
# c(0.16,0.08,0.6,0.16))
fund <- topUp(fund,"2016-01-01","SPY",3000)
fund <- topUp(fund,"2016-01-01","IAU",3800)
fund <- topUp(fund,"2016-01-01","CMI",2000)
fund <- topUp(fund,"2016-01-01","BTCUSD",500)
fund <- topUp(fund,"2016-01-01","IBB",700)

fund




period <- c(january,
            february,
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

legend("bottomright",
       c("fund","benchmark:SPY"),
       lty=1,
       col=c("blue","red"))


eq.ret <- getReturns(equity)
eq.ret
bm.ret <- getReturns(benchmark)
bm.ret
Beta <- beta(eq.ret,eq.ret)
Alpha <- alpha(tail(equity.norm,1),tail(benchmark.norm,1),Beta)

