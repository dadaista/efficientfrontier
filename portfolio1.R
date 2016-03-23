#example of use of frontier.R
#this script takes symbols and find the efficient frontier
#it merges all trading dates using mergeSecurities function
source("util.R")
source("frontier.R")
symbols <- c("IAU","TSN","T","ARG","BTCUSD")
n=90


sym=symbols[1]

x   = load(sym,n)
x <- x[,-1*(2:6)]

for (sym in symbols[2:5]){
  y  =  load(sym,n)
  x <- mergeSecurities(x,y,sym)
}


returns <- computeReturns(x)



#remove Dates

p <- generatePortfolios(t(returns))
plot(p$risk,p$mean)

best <- bestPortfolio(p,expected = 0.0025) #change 0.002 with feasible return
names(best) <- c(symbols,"risk","return")
best
