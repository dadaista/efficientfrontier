
test.load <- function(){
  df <- load("X",days=20)
  checkEquals(nrow(df),days)

}

test.url <- function(){
  rslt <- buildYahooUrl("X",c(2016,1,1),c(2016,2,1))
  expt <- "http://real-chart.finance.yahoo.com/table.csv?s=X&a=0&b=1&c=2016&d=1&e=1&f=2016&g=d&ignore=.csv"
  checkEquals(rslt,expt)
}

test.as.date <- function(){
  d <- as.date(c(2012,1,1))
  checkEquals(d,"2012-01-01")
}
test.load <- function(){
  rslt <- load("X",c(2016,1,1),c(2016,1,5))$Adj.Close[1]
  rslt
  expt <- 8.050987
  checkEquals(rslt,expt)
  
  rslt <- load("BTC",c(2016,1,1),c(2016,1,5))$Adj.Close[1]
  rslt
  expt <- 592.8925
  checkEquals(rslt,expt)
  
}