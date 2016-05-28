#init a fund
date1="2015-10-01"
date2="2015-11-01"

fund <-   data.frame(Date = date1,cash=10000)


test.foo <- function(){
  checkEquals(1:2,c(1,2))
}

test.fund <- function(){
  checkEquals(fund$cash,10000)
}

test.buy <- function(){
  qty=1.5
  f <- buy(fund,date1,"T",qty)
  f
  checkEquals(nrow(f),2)
  checkEquals(f[2,"T"],qty)
  price <-  priceAtDate(date1,"T")
  checkEquals(f[2,"cash"],f[1,"cash"] - price * qty)
  
}

test.buyNeg <- function(){#buy negative quantity is like sell
  qty=1.5
  f <- buy(fund,date1,"T",qty)
  f
  f <- buy(f,date1,"T",-qty)
  f
  checkEquals(f[3,"T"],0)
}


test.placeOrders <- function(){
  
  f <- placeOrders(fund,date1,"T",1000)
  q=1000/priceAtDate(date1,"T")
  checkEquals(f[nrow(f),"T"],q)
  
}

test.topUp <- function(){
  
  f <- topUp(fund,date1,"T",1000)
  q=1000/priceAtDate(date1,"T")
  checkEquals(f[nrow(f),"T"],q)
  
  f <- topUp(f,date1,"T",2000)
  q=2000/priceAtDate(date1,"T")
  checkEquals(f[nrow(f),"T"],q)
  
  f <- topUp(f,date2,"T",3000)
  q=3000/priceAtDate(date2,"T")
  checkEquals(f[nrow(f),"T"],q)  
  
}

test.messingDates <- function(){#cannot buy in a date anterior to last date

  f <- placeOrders(fund,date2,c("T"),c(1000));
  checkException(placeOrders(fund,date1,c("T"),c(1000)));
}



