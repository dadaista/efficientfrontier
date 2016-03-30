#example of use of frontier.R
#this script takes symbols and find the efficient frontier
#it merges all trading dates using mergeSecurities function
source("util.R")
source("frontier.R")
symbols <- c("IAU","TSN","T","ARG","BTCUSD")
days=90


p <- simulatePortfolios(symbols,days,granularity = 0.1)

plot(p$risk,p$mean)

best <- bestPortfolio(p,expected = 0.0025) #change 0.002 with feasible return
names(best) <- c(symbols,"risk","return")
best
