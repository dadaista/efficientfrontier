#example of use of frontier.R
#this script takes symbols and find the efficient frontier
#it merges all trading dates using mergeSecurities function
source("util.R")
source("frontier.R")
symbols <- c("IAU","TSN","T","ARG","BTCUSD")
days=90


p <- simulatePortfolios(symbols,days,granularity = 0.02)

plot(p$risk,p$mean)


names(p) <- c(symbols,"risk","mean")

lowRisk <- p[order(p$risk),]#show low risk portfolio
head(lowRisk)

highRet <- p[order(p$mean,decreasing = T),]#show high return portfolios
head(highRet)

best <- bestPortfolio(p,expected = 0.0015)
best
