
source("util.R")

date1="2016-03-01"
fund <-   data.frame(Date = date1,cash=1000)

fund <- topUp(fund,date1,"IAU",467)
fund <- topUp(fund,date1,"SPY",471)
fund <- topUp(fund,date1,"BTCUSD",56)
fund <- topUp(fund,date1,"EWJ",48)

fund




period <- c(march,april,may)
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
plot(df$equity,t='l',ylim = c(1.0,1.06),col="blue", xaxt="n")
lines(df$benchmark,col="red")
L <- round(nrow(df) / 10)
axis(side=1,labels = period[1:10 * L], at=1:10 * L)

legend("topleft",
       c("fund","benchmark:SPY"),
       lty=1,
       col=c("blue","red"))



