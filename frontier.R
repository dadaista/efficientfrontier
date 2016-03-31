#calculate best portfolio W in the efficient frontier
#given risk
#does not use formulas, but only brute force ala montecarlo
#calculate the mean and variance for all linear combinations
# of W * X
# returns: matrix of return time series, each row a security (n_security,days)
# W: vector of weights (1,n_security)
# find best W given a value for variance(risk)


library(matrixStats)

#' simulate all possible portfolios for symbols 
#'
#' @param symbols 
#' @param days 
#' @param granularity, the lower the better but can overload memory
#'
#' @return a dataframe of portfolios with different weights,risk and return
#' @export
#'
#' @examples
simulatePortfolios <- function (symbols, days, granularity=0.04) {
  returns = combinedReturns(symbols,days)
  portfolios <- generatePortfolios(t(returns),granularity)
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
  w <- w[w$risk == min(w$risk, na.rm = T),]
  return (w)
}


#function below are internal and not documented



#build a dataframe with returns for each
#security in a separate column
#not use directly: use simulatePortfolio

combinedReturns <- function (symbols,days) {
  sym=symbols[1]
  
  x   = load(sym,days,usecache = TRUE)
  x <- x[,-1*(2:6)]
  
  for (sym in symbols[2:length(symbols)]){
    y  =  load(sym,days,usecache = TRUE)
    x <- mergeSecurities(x,y,sym)
  }
  
  
  returns <- computeReturns(x)
  names(returns) <- symbols
  returns
}

generatePortfolios <- function(returns,granularity=0.04){
  #covariance matrix
  x <- returns

  var.f <- function (w){
    w <- as.matrix(w)
    rowVars( w%*%x )
  }
  
  mean.f <- function (we){
    we <- as.matrix(we)
    rowMeans(we%*%x)
  }
  
  #grid.axis <- list ( (0:(granularity^-1))*granularity)
  
  k <- nrow(x)
  n <- granularity^-2
  #w=expand.grid(rep(grid.axis,k))
  w=matrix(nrow = n,ncol=k)
  
  
  for(i in 1:(n-1)){
    rnd <- runif(k)
    rnd <- rnd / sum(rnd)#sum(rnd) == 1
    w[i,1:k] <- rnd
  }
  
  #w <- w[rowSums(w)==1,]
  w <- as.data.frame(w)
  w$risk = sqrt(var.f(w[,1:k]))
  w$mean = mean.f(w[,1:k])
  
  return (w)
}


