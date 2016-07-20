options(stringsAsFactors = FALSE)
cache=list(init=TRUE)

# A calendar
days=c("01","02","03","04","05","06","07","08","09",as.character(10:31))
november <- paste(2015,"11",days[1:30],sep = "-")
december <- paste(2015,"12",days[1:31],sep = "-")
january = paste(2016,"01",days[1:31],sep = "-")
february = paste(2016,"02",days[1:29],sep = "-")
march <-  paste(2016,"03",days[1:31],sep = "-")
april <-  paste(2016,"04",days[1:30],sep = "-")
may <-  paste(2016,"05",days[1:28],sep = "-")




buildYahooUrl<-function(asset){
  
  aDate=strsplit(as.character(Sys.Date()),"-")[[1]]
  day=as.numeric(aDate[3])
  month=as.numeric(aDate[2])-1
  year=as.numeric(aDate[1])
  #u<- 'http://real-chart.finance.yahoo.com/table.csv?s=%5ENDX&a=09&b=11&c=2010&d=09&e=11&f=2015&g=d&ignore=.csv'
  baseu<-'http://real-chart.finance.yahoo.com/table.csv?s='
  u=sprintf("%s%s&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d&g=d&ignore=.csv",baseu,asset,month,day,year-1,month,day,year)
  return (u)
  
}

#' Title
#'
#' @param asset a symbol
#' @param n if non zero loads the most recent n days
#'
#' @return a dataframe
#' @export
#'
#' @examples
load<-function(asset,n=0,usecache=TRUE){
  Sys.sleep(1)#to prevent congestion
  if(usecache==TRUE){
    print("cache lookup for symbol...")
    print(asset)
    if(asset %in% names(cache)){
      print("loading from cache...")
      return(cache[[asset]])
    }
  }
  #special case
  if(asset=="BTCUSD"){
    return(loadBTC())
  }
  
  df<-read.csv(buildYahooUrl(asset))
  #df$Adj.Close=(1:262)*10

  #keep only price and date columns
  df <- df[,c("Date","Adj.Close")]
  
  if (n>0)
    df <- df[1:n,]
  
  
    
  df <- df[nrow(df):1,]
  cache[[asset]] <<- df
  df
}



#' Rate of Change
#'
#' @param price: a vector of prices
#' @param lag  
#'
#' @return the rate of change vector
#' @export
#'
#' @examples
rateOfChange<-function(price,lag=10){
  
  N<-length(price)
  d<-price[(lag+1):N] - price[1:(N-lag)] 
  
  r<-d/price[1:(N-lag)]
  return(r)
}

price2Return <- function(price){
  rateOfChange(price,lag=1)
}


#' load Bitcoin prices from blockchain.info
#'
#' @return
#' @export
#'
#' @examples
loadBTC<-function(){
  
  print("loadinf from blockchain.info...")
  df<-read.csv('https://blockchain.info/charts/market-price?showDataPoints=false&timespan=180days&show_header=true&daysAverageString=1&scale=0&format=csv&address=')
  #df[,1]<-as.character(as.Date(df[,1],format="%d/%m/%Y"))
  
  df[,1] = as.character(as.Date(df[,1]))
  names(df) <- c("Date","Adj.Close")
  df
}







#' Merges two securities data frames with different dates to get a single
#' dataframe with same dates and one column for each security 
#'
#' @param df a dataframe with columns Date and one column of prices per symbol
#' @param data another data frame as loaded by load()
#' @param symbol the name of the symbol for data
#'
#' @return  merges dates in order to consider only a subset of rows
#' @export
#'
#' @examples
mergeSecurities <- function(df,data,symbol){
  dates1 <- df$Date
  dates2 <- data$Date
  dates = intersect(dates1,dates2)#keep only dates in common
  df <-     df[df$Date %in% dates,]
  data <-  data[data$Date %in% dates,]
  df[symbol]<-  data$Adj.Close
  df
}



priceAtDate <- function(adate,symbol){
  df <- load(symbol,usecache = TRUE)
  df <- df[df$Date<=adate,]
  price <-  df$Adj.Close[nrow(df)]
  price
}


pricesAtDate <- Vectorize(priceAtDate,"symbol")

placeOrders <- function(fund,aDate,stocks,values){
  
  lastDate <- fund[nrow(fund),"Date"]
  stopifnot(aDate>=lastDate)
  
  
  p <- pricesAtDate(aDate,stocks)
  p
  qtys = values / p
  
  newsymbols <- stocks[!stocks %in% names(fund)]
  actualRows <- nrow(fund)
  m <- matrix(0,actualRows,length(newsymbols))
  m <- as.data.frame(m)
  names(m) <- newsymbols
  
  fund <- cbind(fund,m)
  
  N <-  nrow(fund)
  fund <- rbind(fund,fund[N,])
  fund[N+1,"Date"] <- aDate
  fund[N+1,stocks] <- fund[N,stocks]+qtys
  fund[N+1,"cash"] <- fund[N+1,"cash"] - sum(values)
  fund
}

sell <- function(fund,date,symbol,qty="all"){
  if(qty == "all"){#sell all
    qty = fund[nrow(fund),symbol]
    if(is.null(qty))
      return (fund)
  }
  p <- priceAtDate(date,c(symbol))
  val <- qty * p
  f <- placeOrders(fund,date,c(symbol),-val)
  f
}



topUp <- function(fund,date,symbol,atValue){
  f <- sell(fund,date,symbol)
  f <- placeOrders(f,date,symbol,atValue)
  f
}



buy <- function(fund,date,symbol,qty){
  p <- priceAtDate(date,c(symbol))
  val <- qty * p
  f <- placeOrders(fund,date,c(symbol),c(val))
  f
}

equityByDate <- function(fund,date){
  
  df <- fund[fund$Date<=date,]
  df
  N <- nrow(df)
  k <- ncol(df)
  Qty <- df[N,3:k]
  Qty <- data.matrix(Qty)
  
  symbols <- names(fund)[3:k] # remove all non equity symbols
  symbols
  Price <- pricesAtDate(date,symbols)
  Price
  value <- Price %*% t(Qty)
  value <- value + df$cash[N]
  value
}


equityOverPeriod <- function(fund,period){
  f <- Vectorize(equityByDate,"date")
  x <- f(fund,period)
  data.frame(Date=period,equity=x)
}
