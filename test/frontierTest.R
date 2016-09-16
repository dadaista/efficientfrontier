test.generatePortfolio <- function(){
  dates=Sys.Date() - 1:10
  A <- rep(1,10)
  B <- rep(-1,10)
  rets <- data.frame(Date=dates,A=A,B=B)
  p <- generatePortfolios(rets,1)
  p
  checkEquals(p$mean , p$A - p$B)
}