#init a fund
date = "2015-11-01"
date1="2015-10-01"
date2="2015-11-01"

fund <-   data.frame(Date = date,cash=10000)


test.foo <- function(){
  checkEquals(1:2,c(1,2))
}

test.fund <- function(){
  checkEquals(fund$cash,10000)
}

test.buy <- function(){
  qty=1.5
  f <- buy(fund,date,"T",qty)
  f
  checkEquals(nrow(f),2)
  checkEquals(f[2,"T"],qty)
  price <-  priceAtDate(date,"T")
  checkEquals(f[2,"cash"],f[1,"cash"] - price * qty)
  
}

test.buyNeg <- function(){#buy negative quantity is like sell
  qty=1.5
  f <- buy(fund,date,"T",qty)
  f
  f <- buy(f,date,"T",-qty)
  f
  checkEquals(f[3,"T"],0)
}

test.bar <- function(){
  checkEquals(1,2)
}

test.topUp <- function(){
  
  f <- placeOrders(fund,date1,"T",1000)
  checkEquals(f[nrow(f),"T"],30.40114)
  
}


test.messingDates <- function(){#cannot buy in a date anterior to last date

  f <- placeOrders(fund,date2,"T",1000)
  checkException(placeOrders(fund,date1,"T",1000))
}



