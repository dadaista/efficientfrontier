#calculate best portfolio W in the efficient frontier
#given risk
#does not use formulas, but only brute force ala montecarlo
#calculate the mean and variance for all linear combinations
# of W * X
# returns: matrix of return time series, each row a security (n_security,days)
# W: vector of weights (1,n_security)
# find best W given a value for variance(risk)


library(matrixStats)
source("loader.R")
#' montecarlo simulation portfolios for symbols 
#'
#' @param symbols 
#' @param days 
#' @param granularity, the lower the better but can overload memory
#'
#' @return a dataframe of portfolios with different weights,risk and return
#' @export
#'
#' @examples
simulatePortfolios <- function (symbols, days, len=10) {
  returns = as.returns(loadMulti(symbols,to.date=days))
  portfolios <- generatePortfolios(returns,len)
  portfolios
}

#' select the best portfolio based on the expected return
#'
#' @param portfolios 
#' @param expected: expected return
#'
#' @return
#' @export
#'
#' @examples
bestPortfolio <- function(portfolios,expected=0.01){
  
  w = portfolios
  #calculating best portfolio
  w <- w[w$mean >= expected,]
  w <- w[w$sd == min(w$sd, na.rm = T),]
  return (w)
}






generatePortfolios <- function(returns,len=10){
  #covariance matrix
  x <- returns
  #get rid of Dates
  ix = which(colnames(x)=="Date")
  x <- x[,-ix]

  k <- ncol(x) 
  n <- len
  #w=expand.grid(rep(grid.axis,k))
  w=matrix(nrow = n,ncol=k)
  
  
  for(i in 1:n){
    rnd <- runif(k)
    rnd <- rnd / sum(rnd)#sum(rnd) == 1
    w[i,1:k] <- rnd
  }
  
  p=data.frame()
  for(i in 1:nrow(w)){   
    comb <- apply(x,1,function(rets){sum(rets * w[i,])})
    p=rbind(p,c(w[i,],mean(comb),sd(comb)))
  }
  names(p) <- c(names(x),"mean","sd")
  p
  
}


