


test.url <- function(){
  rslt <- buildYahooUrl("X",c(2016,1,1),c(2016,2,1))
  expt <- "http://real-chart.finance.yahoo.com/table.csv?s=X&a=0&b=1&c=2016&d=1&e=1&f=2016&g=d&ignore=.csv"
  checkEquals(rslt,expt)
}

test.as.date <- function(){
  d <- as.date(c(2012,1,1))
  checkEquals(d,"2012-01-01")
}

#testBTC <- function(){
#  df <- loadBTC(c(2016,1,1),c(2016,1,5))
#  df
#}

test.load <- function(){
  rslt <- load("X",c(2016,1,1),c(2016,1,5))$Adj.Close[1]
  rslt
  expt <- 8.050987
  checkEquals(rslt,expt)
  
  #rslt <- load("BTC",c(2016,1,1),c(2016,1,5))
  #rslt
  #expt <- 592.8925
  #checkEquals(rslt$Adj.Close[1],expt)
  
}

test.mergeCloses <- function(){
  df1 <- load("X",c(2016,1,1),c(2016,1,10))
  df1
  df2 <- load("FB",c(2016,1,6),c(2016,1,15))
  df2
  df <- mergeCloses(df1,df2,"FB")
  df
  checkEquals(all(df$Date %in% df1$Date),TRUE)
  checkEquals(all(df$Date %in% df1$Date),TRUE)
  
  #special case empty df1
  df1 <- data.frame()
  df <- mergeCloses(df1,df2,"FB")
  df
}

test.loadMulti <- function(){
  df <- loadMulti(c("X","FB"),c(2016,1,1),c(2016,1,10))
  df
}

test.pricesAtDate <- function(){
  #a date with market open
  adate = c(2015,5,5)
  df <- pricesAtDate(c("X","FB"),adate)
  df
  checkEquals(df$Date,as.date(adate))
  #a date with market close
  adate=c(2016,1,3)
  df <- pricesAtDate(c("X","FB"),adate)
  df
  checkEquals(df$Date,"2015-12-31")
}

test.as.return <- function(){
  y=rep(2016,5)
  m=rep(5,5)
  d=6:10
  dates=data.frame(y,m,d)
  dates
  dates <- apply(dates,1,function(adate)as.date(adate))
  A = c(1,2,3,4,5)
  B = c(1,1,2,2,2)
  df <- data.frame(Dates=dates,A=A,B=B)
  rets <- as.returns(df)
  rets
  checkEquals(rets$A,c(1,1/2,1/3,1/4))
  checkEquals(rets$B,c(0,1,0,0))
  
  #only one security
  df <- data.frame(Dates=dates,A=A)
  rets <- as.returns(df)
  rets
  checkEquals(rets$A,c(1,1/2,1/3,1/4))

  
}

test.cache <- function(){
  USECACHE <<- TRUE
  df <- load("X",c(2012,4,4),c(2012,5,4))
  checkEquals(cache[[paste("X",as.date(c(2012,4,4)),as.date(c(2012,5,4)))]],df)
}
