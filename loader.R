USECACHE <- FALSE
cache <- list()
epoch <- function() as.integer(as.POSIXct(Sys.time()))
lastReadTime <- epoch()
library(lubridate)
library(Quandl)

as.date <- function(dateVector){
  year = dateVector[1]
  month= dateVector[2]
  day  = dateVector[3]
  
  month=c("01","02","03","04","05","06","07","08","09","10","11","12")[month]
  day=c("01","02","03","04","05","06","07","08","09",as.character(10:31))[day]
  d <- sprintf("%s-%s-%s",year,month,day)
  d
}





#' Title
#'
#' @param tick the symbol ex. "MSFT"
#' @param start ex. c(2016,7,1)
#' @param end   ditto
#'
#' @return a dataframe

load<-function(tick,start, end){#sec

  cache.key <- paste(tick,as.date(start),as.date(end))
  cache.miss <- !(cache.key %in% names(cache))
  
  if(USECACHE==FALSE) cache.miss <- TRUE
  
  if(cache.miss){
    df<-Quandl(paste(c("WIKI",tick),collapse = "/"),start_date=as.date(start),end_date=as.date(end))
    #keep only price and date columns
    df <- df[,c("Date","Adj. Close")]
    df <- df[nrow(df):1,]
    names(df) <- c("Date","Adj.Close")
    if(USECACHE) 
      cache[[cache.key]] <<- df
  }else df <- cache[[cache.key]]
  
  df
}







#like load but with a vector of tickers
loadMulti <- function(tickers,start,end,to.date=0){
  
  if(to.date>0){
    today <- Sys.Date()
    end <- c(year(today),month(today),day(today))
    start <- today - to.date
    start <- c(year(start),month(start),day(start))
  }
  
  df <- data.frame()
  for (t in tickers){
    data <- load(t,start,end)
    df <- mergeCloses(df,data,t)
  }
  names(df) <- c("Date",tickers)
  df
}

mergeCloses <- function(df,data,symbol){
  
  if(nrow(df)==0) return(data)
  if(nrow(data)==0) return(df)
  
  
  dates1 <- df$Date
  dates2 <- data$Date
  dates = intersect(dates1,dates2)#keep only dates in common
  df <-     df[df$Date %in% dates,]
  data <-  data[data$Date %in% dates,]
  df[symbol]<-  data$Adj.Close
  df
}



pricesAtDate <- function(tickers,adate){
  d1 = as.date(adate)
  d1 <- date(d1)#a lubridate object
  d0 <- d1 - 30
  
  df <- loadMulti(tickers,c(year(d0),month(d0),day(d0)),adate) 
  tail(df,1)
}


as.returns <- function(prices, lag=1){
  x <- prices
  n <- nrow(x)
  dates <- x[,1]
  x <- x[,-1]#remove date column
  x <- as.matrix(x)
  returns <- (x[(lag+1):n, ] - x[1:(n-lag),])/x[1:(n-lag),]
  returns <- as.data.frame(returns)
  returns$Date <- dates[(lag+1):n]
  returns
}



