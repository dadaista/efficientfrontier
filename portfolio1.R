#example of use of frontier.R
#this script takes symbols and find the efficient frontier
#it merges all trading dates using mergeSecurities function

symbols <- c("IAU","FB","T","BTCUSD","MA","AMZN")
n=90
sym=symbols[1]

x   = load(sym,n)
x <- x[,-1*(2:6)]


sym=symbols[2]
y  =  load(sym,n)

x <- mergeSecurities(x,y,sym)

sym=symbols[3]
y   = load(sym,n)
x <- mergeSecurities(x,y,sym)

sym=symbols[4]
y  =  load(sym,n)
x <- mergeSecurities(x,y,sym)

sym=symbols[5]
y  =  load(sym,n)
x <- mergeSecurities(x,y,sym)

sym=symbols[6]
y  =  load(sym,n)
x <- mergeSecurities(x,y,sym)

returns <- computeReturns(x)



#remove Dates

p <- generatePortfolios(t(returns))
plot(p$risk,p$mean)

best <- bestPortfolio(p,expected = 0.001) #change 0.002 with feasible return
best
