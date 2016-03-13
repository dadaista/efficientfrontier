#calculate best portfolio W in the efficient frontier
#given risk
#does not use formulas, but only brute force
#calculate the mean and variance for all linear combinations
# of W * X
# returns: matrix of return time series, each row a security (n_security,days)
# W: vector of weights (1,n_security)
# find best W given a value for variance(risk)


library(matrixStats)



generatePortfolios <- function(returns,risk){
  #covariance matrix
  x <- returns
  S <- cov(t(x))
  
  var.f <- function (w){
    w <- as.matrix(w)
    rowVars( w%*%x )
  }
  
  mean.f <- function (we){
    we <- as.matrix(we)
    rowMeans(we%*%x)
  }
  
  grid.axis <- list ( (0:25)*0.04)
  
  k <- nrow(x)
  
  w=expand.grid(rep(grid.axis,k))
  
  w <- w[rowSums(w)==1,]
  
  w$risk = sqrt(var.f(w[,1:k]))
  w$mean = mean.f(w[,1:k])
  
  return (w)
}

bestPortfolio <- function(portfolios,expected=0.01){

  w = portfolios
  #calculating best portfolio
  w <- w[w$mean >= expected,]
  w <- w[w$risk == min(w$risk),]
  return (w)
}

#example, see portfolio1 for full example
days=252
x1=rnorm(days,0.01,0.01)
x2=rnorm(days,0.02,0.05)
x3=0.5*x1 - 0.5*rnorm(days,-0.02,0.3)
x=rbind(x1,x2,x3)

portfolios <- generatePortfolios(x)

print (portfolios)

plot(portfolios$risk,portfolios$mean)

print (bestPortfolio(portfolios))


