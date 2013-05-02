library('quantmod')
library('zoo')

TEMP_FILE <- './data/temp.csv'

ohlc <- function(ttime,tprice,tvolume,fmt) {
  # function which converts raw bitcoincharts ticker data to ohlc data
  # possible formats include: "%Y%m%d %H %M" (minute ticker), "%Y%m%d %H" (hourly ticker), "%Y%m%d" (daily ticker)
  ttime.int <- format(ttime,fmt)
  data.frame(time = ttime[tapply(1:length(ttime),ttime.int,function(x) {head(x,1)})],
             .Open = tapply(tprice,ttime.int,function(x) {head(x,1)}), 
             .High = tapply(tprice,ttime.int,max),
             .Low = tapply(tprice,ttime.int,min),
             .Close = tapply(tprice,ttime.int,function(x) {tail(x,1)}),
             .Volume = tapply(tvolume,ttime.int,function(x) {sum(x)}),
             .Adjusted = tapply(tprice,ttime.int,function(x) {tail(x,1)}))
}

getTicker <- function(symbol,period,datasource,filename='') {
  # this method retrieves ticker data from bitcoincharts.com or a a CSV downloaded from bitcoincharts.com for any symbol listed on the site
  # usage: ticker <- data.frame(getTicker('mtgoxUSD|virtexCAD\...', 'daily|hourly|minutes', 'web|file','./data/mtgoxusd-recent.csv'))
  # filename parameter is optional and only used when datasource = 'file'
  ColClasses = c('numeric','numeric','numeric')
  if(datasource == 'web') {
    RECENT_TRANSACTIONS_FILE <- paste('./data/', symbol, sep='')
    RECENT_TRANSACTIONS_FILE <- paste(RECENT_TRANSACTIONS_FILE, '-recent.csv', sep='')
    # let's be good and only download the data that we have to ...
    if(file.exists(RECENT_TRANSACTIONS_FILE)) {
      # get the last id
      mawk_command <- paste("mawk 'END {print}' ", RECENT_TRANSACTIONS_FILE, sep='')
      lastline <- system(mawk_command, intern = TRUE)
      lastid <- strsplit(lastline[1], split=',')[[1]][1]
      url <- paste('http://bitcoincharts.com/t/trades.csv?symbol=', symbol, sep='')
      url <- paste(url, '&start=', sep='')
      url <- paste(url, lastid, sep='') 
      download.file(url, destfile=TEMP_FILE, method='auto', quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
      cat("\n",file=RECENT_TRANSACTIONS_FILE,append=TRUE)
      # now we need some additional sanity checking here to see if this delta already exists in the
      # current local file or we will sometimes wind up repeatedly downloading the same transactions
      file.append(RECENT_TRANSACTIONS_FILE, TEMP_FILE)
      file.remove(TEMP_FILE)
    }
    else {
      download.file(url=paste('http://bitcoincharts.com/t/trades.csv?symbol=', symbol, sep=''), destfile=RECENT_TRANSACTIONS_FILE, method='auto', quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    }
    ticker <- read.zoo(RECENT_TRANSACTIONS_FILE, colClasses = ColClasses, sep = ",", header = FALSE)
  }
  else {
    ticker <- read.zoo(filename, colClasses = ColClasses, sep = ",", header = FALSE)
  }
  index(ticker) <- as.POSIXct(index(ticker), origin="1970-01-01", tz="GMT")
  if(period == 'monthly')  ohlcObj <- ohlc(index(ticker),ticker$V2,ticker$V3,"%Y%m")
  if(period == 'daily')  ohlcObj <- ohlc(index(ticker),ticker$V2,ticker$V3,"%Y%m%d")
  if(period == 'hourly') ohlcObj<- ohlc(index(ticker),ticker$V2,ticker$V3,"%Y%m%d %H")
  if(period == 'minutes') ohlcObj <- ohlc(index(ticker),ticker$V2,ticker$V3,"%Y%m%d %H %M")
  # clean up column names a bit
  names(ohlcObj)[names(ohlcObj) == ".Open"] <- 'Open' 
  names(ohlcObj)[names(ohlcObj) == ".Close"] <- 'Close' 
  names(ohlcObj)[names(ohlcObj) == ".Volume"] <- 'Volume' 
  names(ohlcObj)[names(ohlcObj) == ".Adjusted"] <- 'Adjusted' 
  names(ohlcObj)[names(ohlcObj) == ".High"] <- 'High' 
  names(ohlcObj)[names(ohlcObj) == ".Low"] <- 'Low'
  names(ohlcObj)[names(ohlcObj) == "time"] <- 'Time'
  row.names(ohlcObj) <- NULL
  ohlcObj$row.names <- NULL
  return(ohlcObj)
}

getChartable <- function(ohlcObj) {
  # this method takes a raw ohlc data frame and rebuilds it in a way that emits an xts objects that can be used by chartSeries
  rownames(ohlcObj) <- ohlcObj$Time
  ohlcObj$Time <- NULL
  ohlcObjXts <- as.xts(ohlcObj)
  return(ohlcObjXts)
}

# Let's try it out now
#ticker <- data.frame(getTicker('mtgoxUSD', 'minutes', 'file','./data/mtgoxusd-recent.csv'))
#ticker <- data.frame(getTicker('virtexCAD', 'monthly', 'file','./data/cavirtex-2012.csv'))

# populate a data frame from web
ticker <- data.frame(getTicker('mtgoxUSD', 'minutes', 'web'))

# get xts object that can be used for charting
tickerxts <- getChartable(ticker)
chartSeries(tickerxts)

#Add a few technical indicators to the chart
addEMA(n=6*7,col='red')
addEMA(n=3*7,col='green')
addMACD()
