epoch <- function() as.integer(as.POSIXct(Sys.time()))
lastReadTime <- epoch()
library(lubridate)

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
  stop("BTC load not working yet!!!")
  start <- as.date(start)
  end <- as.date(end)
  url = sprintf('https://api.coindesk.com/v1/bpi/historical/close.csv?start=%s&end=%s',start,end)
  url
  df<-read.csv(url)
  #df[,1]<-as.character(as.Date(df[,1],format="%d/%m/%Y"))
  
  names(df) <- c("Date","Adj.Close")
  df
}






loadMulti <- function(tickers,start,end){
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


as.returns <- function(prices){
  x <- prices
  n <- nrow(x)
  dates <- x[,1]
  x <- x[,-1]#remove date column
  returns <- (x[2:n, ] - x[1:(n-1),])/x[1:(n-1),]
  returns$Date <- dates[2:n]
  returns
}
