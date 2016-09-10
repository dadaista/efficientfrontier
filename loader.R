cache=list(init=TRUE)
epoch <- function() as.integer(as.POSIXct(Sys.time()))
lastReadTime <- epoch()

as.date <- function(dateVector){
  year = dateVector[1]
  month= dateVector[2]
  day  = dateVector[3]
  
  month=c("01","02","03","04","05","06","07","08","09","10","11","12")[month]
  day=c("01","02","03","04","05","06","07","08","09",as.character(10:31))[day]
  d <- sprintf("%s-%s-%s",year,month,day)
}


buildYahooUrl<-function(tick,start,end){
  
  #aDate=strsplit(as.character(Sys.Date()),"-")[[1]]
  #day=as.numeric(aDate[3])
  #month=as.numeric(aDate[2])-1
  #year=as.numeric(aDate[1])
  
  start.year=start[1]
  start.month=start[2] - 1
  start.day=start[3]
  end.year=end[1]
  end.month=end[2] - 1
  end.day=end[3]
  #u<- 'http://real-chart.finance.yahoo.com/table.csv?s=%5ENDX&a=09&b=11&c=2010&d=09&e=11&f=2015&g=d&ignore=.csv'
  baseu<-'http://real-chart.finance.yahoo.com/table.csv?s='
  u=sprintf("%s%s&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d&g=d&ignore=.csv",baseu,tick,start.month,start.day,start.year,end.month,end.day,end.year)
  return (u)
  
}


load<-function(tick,start, end){#sec
  #Sys.sleep(1)#to prevent congestion
  #special case
  if(tick=="BTC"){
    return(loadBTC(start,end))
  }
  
  df<-read.csv(buildYahooUrl(tick,start,end))
  #keep only price and date columns
  df <- df[,c("Date","Adj.Close")]
  df <- df[nrow(df):1,]
  df
}





loadBTC<-function(start,end){
  print("loadinf from coinbase")
  start <- as.date(start)
  end <- as.date(end)
  url = sprintf('https://api.coindesk.com/v1/bpi/historical/close.csv?start=%s&end=%s',start,end)
  df<-read.csv(url)
  #df[,1]<-as.character(as.Date(df[,1],format="%d/%m/%Y"))
  
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


