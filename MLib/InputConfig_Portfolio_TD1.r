
################################################################################################################
#######################################################################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)


options(error = browser)
##############################################################################################################


#charts.PerformanceSummary(RF(getSymbols(c("QQQ","EWG","EWI"))))
#Very nice! Thank you for creating this package.
#The chart_Series function (note the underscore) can also handle multipletime series

#getSymbols(c("SPY","DIA"))

# This will put 2 price series on a chart
#thm <- chart_theme()
#thm$col$line.col <- 'lightblue'
#chart_Series(Cl(SPY),theme=thm)
#add_Series(Cl(DIA),on=1)


# Function to compute percentage returns

#Renditen:    Aktuelle/Gestern - 1     //  Returns:     CumProd (davon)


pchange <- function(x) { exp(diff(log(x)))-1 }

#ifelse(4,32,3)
#iif(as.xts(prices)> 6980, 1, as.xts(prices))
#return-Berechnung mit Ausrei?erentfernung 
#mROC <- function(clos) na.omit(Return.clean( na.omit( clos / iif( mlag(clos)==0,NA,mlag(clos)) - 1), method = "geltner", n=20)) #identisch   Return.calculate(prices, method="simple")
#mROC <-function(clos) Return.clean( na.omit( ROC(clos,type="discrete")), method = "geltner", n=20) #identisch   Return.calculate(prices, method="simple")

#r=na.omit(ROC(data$prices))
#min(abs(r))
#r[r==0]<-0.00000001

#mROC <-function(clos) {ret=na.omit(ROC(clos,type="discrete")); ret[which(!is.finite(ret))]<-0.00000001; ret[ret==0]<-0.00000001;return(ret)} #identisch  Return.calculate(prices, method="simple")


mROC <-function(clos,n=1,type="discrete") {ret= ROC(x=clos,n=n,type=type); r=apply(ret,2,FUN=function(x) { x[which(!is.finite(x))]<-0.00000001;x[x==0]<-0.00000001;x}); return(xts(r,order.by=as.Date(rownames(r)))) }

#Return.cumulative(geometric=
#Return.calculate(method="discrete") #log
#chart.ChumReturns
## so gebaut, dass    min(abs(r))  > 0 ist - weil sonst cumprod in mRendite auf Grund l?uft
#mRendite <-function(ret) (cumprod(na.omit(ret)+1)-1) #identisch Return.cumulative(R, geometric = T)
mRendite <-function(ret) (cumprod(ret+1)) #identisch Return.cumulative(R, geometric = T)

#(mRoc und mRendite geh?ren zusammen)

normalize.mean <- function(x) { x - mean(x) }
normalize.mean.sd <- function(x) { (x - mean(x)) / sd(x) }
normalize.mean.R <- function(x,n) { x - runmean(x,n,align="right") }
normalize.mean.sd.R <- function(x,n) { (x - runmean(x,n,align="right")) / runsd(x,n,align="right") }

#purePlot(merge(p1,mNorm(p1),mNorm(p2),normalize.mean(p1),normalize.mean.R(p1,200),normalize.mean.sd(p1),normalize.mean.sd.R(p1,200)))
#purePlot(merge(p1,normalize.mean(p1),normalize.mean.R(p1,200)))

mLogReturn<-function(prices) { return(na.omit(diff(log(prices))))}
mLogRendite<-function(ret) cumsum(na.omit(ret))+1
mNorm2<-function(clos){ mLogRendite (mLogReturn(clos))}

mNorm<-function(clos){ mRendite (mROC(clos))}
Mnorm<-function(clos){ mNorm(clos+10000)}
Mnorm2<-function(clos){ mNorm2(clos+10000)}
mnorm<-function(close){ bt.apply.matrix(close, function(clos) mNorm(clos+min(clos,na.rm=T))) }

mNormCleaned<-function(clos){ mRendite ( Return.clean(mROC(clos), method="boudt"))}

mReturn<-function(prices) {return(na.omit(prices / mlag(prices) - 1))}


#if (geometric) 
#  Return.cumulative = cumprod(1 + x)
#else Return.cumulative = 1 + cumsum(x)


#ret <- ROC(clos)
RocRen <-function(ret) exp(cumsum(ret))
library(quantmod)
#TTR::ROC
setDefaults(ROC, type="discrete")
#useDefaults(ROC)
getDefaults()

#ROC(1:10)
#CUD
###############################################################################
#http://stackoverflow.com/questions/5162583/is-it-posible-to-optimize-vectorize-
#these-two-functions-for-better-performance
#For the first function you're looking for the cumulative number of periods 
#during which series x is lower/higher than y. For that you can use this handy 
#function CumCount() built from cummax. First some sample data:
###############################################################################

CumCount <- function(x) {
  z <- cumsum(x)
  z - cummax(z*(!x))
}

#set.seed(1)
#x <- sample(1:5,20,T)
#y <- sample(1:5,20,T)
#CumLow = CumCount(x<y)
#CumHigh = CumCount(x>y)

#################################################################
#This little function returns a list with:

#high the index number of high dates
#recentHigh the index number of the most recent high day
#daysSince the number of days since the last high
#data an xts object with only the high days. Useful for plotting.
#aus:http://stackoverflow.com/questions/7354368/how-to-calculate-periods-since-200-period-high-of-a-stock

##################################################################

daysSinceHigh <- function(data, days){
  
  highs <- days-1+which(apply(embed(data, days), 1, which.max)==1)
  recentHigh <- max(highs)
  daysSince <- nrow(data) - recentHigh
  list(
    highs=highs,
    recentHigh = recentHigh,
    daysSince = daysSince,
    data=data[highs, ])
}       

#wie viele Tage sind seid dem letzten n Tage - hoch vergangen
daysSinceHigh_ <- function(x, n){
  apply(embed(x, n), 1, which.max)-1
}

#  daysSinceHigh(Data, 200)$daysSince

#plot(Data)
#points(daysSinceHigh(Data, 200)$data, col="red")


############### wenn R auf verbotene Indizes zur?ckgreift kommt als Return ein integer(0) 
##### wie findet man das ?  (?ber len (0))
# is.ok(zugriff)
#######################################################################################
is.ok<-function(x)  return( length(x) > 0)

#is.ok(matrix(4,4)[0,1])

mP<-function(...,info=F)
{
  #browser()
  #tryM(sfCat(sprintf(...),master=T))  #print
  #formals(data[["level.price"]])
  if (info) 
  {print("...................... mP ...................>>")
   print(traceback())
   x=formals(fun=sys.function(sys.parent(1))) #gibt die Parameter zur aktuellen Methode
   print(x)
  }
  res=  tryM(sprintf(...))  #print
  print(res)
  if (info)print("<<<  ..........................................")
  res
}
#.......................................................................................

MP<-function(...,info=T)
{
  if (info) 
  {print("...................... mP ...................>>")
   print((traceback(max.lines=3)))
   x=formals(fun=sys.function(sys.parent(1))) #gibt die Parameter zur aktuellen Methode
   print(x)
   
  }
  if (info)print("<<<  ..........................................")
  res= tryM(sprintf(...),master=T)  #print
  print(res)
  res
}

###############################################################################
mm.strFind<-function(pat,s)
{
  if (is.null(s) || len(s)==0)
    return(F)
  return(  grepl(pat, s)[1])
}
###############################################################################
# Load Packages that are available and install ones that are not available.
############################################################################### 
load.packages <- function
(
  packages,   						# names of the packages separated by comma
  repos = "http://cran.r-project.org",# default repository
  dependencies = "Depends",				# install dependencies
  ...									# other parameters to install.packages
)
{
  packages = spl(packages)
  for( ipackage in packages ) {
    if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
      install.packages(ipackage, repos=repos, dependencies=dependencies, ...) 
      
      if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
        stop("package", sQuote(ipackage), 'is needed.  Stopping')
      }
    }
  }
}


load.packages('quadprog,lpSolve')


#########################################################################################################
#Lade aus Yahoo die Liste mit den Symbolen der IndexMember
#########################################################################################################
if (F)
  getIndexComposition("^DJI")
#getIndexComposition("ALV.DE")

getIndexComposition<-function(index) 
{
  url <- paste("http://uk.finance.yahoo.com/q/cp?s=", index, sep="") 
  #symbols <- readHTMLTable(url, as.data.frame=FALSE)[[10]]$Symbol 
  symbols <- readHTMLTable(url, as.data.frame=FALSE)
  #  browser()  
  
  ret=  (symbols[[8]])[[1]]
  ret = ret[-c(1,2)]
  longname=((((symbols[[8]]))[[2]])[-c(1,2)])
  print ((((symbols[[8]]))[[2]])[-c(1,2)])
  universe=data.table(symbol=ret,longname=longname)
  mP("############# universe is at clibboard ###################### !!!!")
  writeClipboard(as.matrix(universe))
  View(universe)
  return (c(index,ret))
  
}
####################################################################
####################################################################

get.Index.data<-function(index="^GDAXI")
{
  tickers=getIndexComposition(index)
  data <- new.env()
  mP("download tickers")
  getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
  mP("tickers loaded")
  for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                           
  bt.prep(data, align='keep.all', dates='2000::')
  return (data)
}
#schreib die stoxxL- Liste als Universe.txt...
####################################################################
#Schreibe die IndexMember ein Yahoo-Index als Yahoo-Namen als Zeitreihen nach MData und als Univers.csv  nach MData/IndexMember
####################################################################
if (F)
  writeIndexComposition("^GDAXI")
if (F)
  writeIndexComposition("Dax")

####################################################################
writeIndexComposition<-function(index="^GDAXI", universeFile = "",data=.GlobalEnv) #MMA 
{
  indexProvider = getProviderTicker(index)  
  index = indexProvider$tick
  
  if (universeFile =="")
    universeFile = sprintf("MData/IndexMember/%s.csv", indexProvider$name)
  
  dir.create(dirname(universeFile),recursive=T)
  mP("\n writeIndexCompositions for %s",index)
  ticks=getIndexComposition(index)
  
  #lade die Aktien (IndexMember) runter .. f?r Aktien sind die yahoo-K?rzel eh Standard - da muss nicht extra was in der securities.xls angelegt werden.
  
  if (len(ticks > 0))
    k=sapply(ticks,FUN=function(x) try(mGetTickers(x,data=data,online=T)))
  
  
  if (len(ticks)>0 && universeFile != "")
  {
    #browser()
    ticks = sapply(ticks,normaliseTickerName)
    
    ticks = lapply(ticks,FUN=function(x) return( toupper(x)))
    m = cbind(ticks)
    colnames(m)<-c("member")
    write.table(m,file=universeFile,row.names=F,quote=T)
    #sprintf("o:/R/Nuggets/Eckhard/Models/%s/universe.csv",modelDir)
    #writeIndexComposition("^GDAXI",universeFile =sprintf("o:/R/Nuggets/Eckhard/Models/%s/universe.csv",modelDir))
    
  }
  print(coredata(k))
  return(list(ticks))
}



#########################################################################################################

#########################################################################################################

Tickers<-NULL
#tickers<-NULL #die globale Liste der via DB geladenen Ticker xts- Reihen

#########################################################################################################

#########################################################################################################

startsWith <-function (name, stri)
{
  ret<-str_locate(name,stri)[1]
  if (is.na(ret))
    return (FALSE)
  if (1==str_locate(name,stri)[1])
    return (TRUE)
  return (FALSE)
}

#unlist(strsplit("eur=x","="))[1]

#########################################################################################################

#########################################################################################################

BusinessDays <- function(x, y){
  DateVector <- seq(min(c(as.Date(x), as.Date(y))), to = max(c(as.Date(x), as.Date(y))), by = "day")
  return (DateVector[!sapply(DateVector,is.weekend)])
}
########################################################################
#verichte z.B. die highs,lows- Zeitreihen zu einer spalte
#################################
joinRows<-function(XTS)
{
  #  browser()
  res=apply(XTS,1,FUN=function(r) sum(na.omit(r)))
  if (is.xts(XTS))
    res = as.xts(res)
  res
}

#########################################################################################################

countBusinessDays <- function(x, y, holidays=NULL){
  DateVector <- seq(min(c(as.Date(x), as.Date(y))), to = max(c(as.Date(x), as.Date(y))), by = "day")
  if(is.null(holidays))
    holidays <- rep(FALSE, length(DateVector))
  else
    holidays <- DateVector %in% holidays
  length(DateVector) - length(which(as.POSIXlt(DateVector)$wday %in% c(0, 6) | holidays))-1
}

#################################################################
#load stoxx timeseries to xts
#################################################################
readStoxx<-function(sym,tick)
{
  #sym="sx8p"
  #http://www.stoxx.com/download/historical_values/h_ctb_sxagr.txt
  
  url=sprintf("http://www.stoxx.com/download/historical_data/h_%s.txt",tick)
  
  print(url)
  x=read.csv(sep=";",dec=".",header=T,url)
  X=x  
  print(head(x,3))
  #x=x[-c(1,3)]
  datum=x[,1]
  
  if (is.null(row.names(X)))
  {x =x$Indexvalue
   symbol = x$Symbol[1]
   mP("row.names is null")
  }
  else
  {  symbol = x[1,1]; x =x[,2] 
     print(head(row.names(X),3))
  }
  mP("found symbol %s",symbol)
  
  x = cbind(x,x,x,x,x,x)
  x[,5]=1
  symbol=trim(symbol)
  colnames(x)=c( paste(symbol,".Open"),paste(symbol,".High"),paste(symbol,".Low"),paste(symbol,".Close"),paste(symbol,".Volume"), paste(symbol,".Adjusted"))
  if (is.null(row.names(X)))
    ind = as.Date(datum,"%d.%m.%Y")  else   ind = as.Date(row.names(X),"%d.%m.%Y")
  print(head(ind,3))
  stoxx = as.xts(x,order.by=ind)
}

if (F)
  stoxx=readStoxx("sx5r","sx5hun")
#Stoxx=data.frame(stoxx)
#xyplot(stoxx)


#################################################################
#load stoxx timeseries to xts
#NEU
#################################################################
readStoxx<-function(sym,tick)
{
  #http://www.stoxx.com/indices/byregion.html?superRegion=Europe&subRegion=Europe#
  
  #sym="sx8p"
  #http://www.stoxx.com/download/historical_values/h_ctb_sxagr.txt
  
  url=sprintf("http://www.stoxx.com/download/historical_data/h_%s.txt",tolower(trim(tick)))
  
  print(url)
  
  x=read.csv(sep=";",dec=".",header=T,url,stringsAsFactors=F)
  X=x  
  
  print(head(x,3))
  #x=x[-c(1,3)]
  datum=x[,1]
  #browser()
  
  aMode =F
  k=tryCatch(as.Date(x[1,1],"%d.%d.%Y"),error=function(err) NULL)
  
  if (is.finite(k))
  {
    aMode=T
    symbol = x$Symbol[1]
    x =x$Indexvalue
    mP("aMode")
  }
  else
  {  symbol = x[1,1]; x =x[,2] 
     print(head(row.names(X),3))
  }
  mP("found symbol %s",symbol)
  
  x = cbind(x,x,x,x,x,x)
  x[,5]=1
  symbol = trim(symbol)
  colnames(x)=c( paste(symbol,".Open"),paste(symbol,".High"),paste(symbol,".Low"),paste(symbol,".Close"),paste(symbol,".Volume"), paste(symbol,".Adjusted"))
  
  if (aMode)
    ind = as.Date(datum,"%d.%m.%Y")  else   ind = as.Date(row.names(X),"%d.%m.%Y")
  print(head(ind,3))
  stoxx = as.xts(x,order.by=ind)
}


#################################################################
#load stoxx timeseries to xts
#################################################################
readStoxx2<-function(sym,tick,url="")
{
  #sym="sx8p"
  #http://www.stoxx.com/download/historical_values/h_ctb_sxagr.txt
  if (url == "")
    url=sprintf("http://www.stoxx.com/download/historical_data/h_%s.txt",tick)
  # url ="http://www.stoxx.com/download/historical_values/h_sx5hun.txt"
  print(url)
  x=read.csv(sep=";",dec=".",header=T,url,skip=1)
  if(ncol(x) == 1)
    x=read.csv(sep=",",dec=".",header=T,url,skip=1)
  #x[2,1])
  
  symbol =colnames(x)[2]
  print(symbol)
  print(head(x,3))
  #browser()
  #x=x[-c(1,3)]
  datum=x[,1]
  x =x[,2] 
  
  mP("found symbol %s",symbol)
  
  x = cbind(x,x,x,x,x,x)
  x[,5]=1
  symbol = trim(symbol)
  colnames(x)=c( paste(symbol,".Open"),paste(symbol,".High"),paste(symbol,".Low"),paste(symbol,".Close"),paste(symbol,".Volume"), paste(symbol,".Adjusted"))
  ind = as.Date(datum,"%d.%m.%Y") 
  stoxx = as.xts(x,order.by=ind)
  #browser()
  return(list(symbol=symbol, data=stoxx))
}

#########################################################################################################
# Lade einfache (nicht OHLC) aus csv-file - wenn das nicht geht aus dem Internet
#FredTicker = list(
#c("DEXKOUS","FRED"), #load Korea
#c("DEXMAUS","FRED"), #load Malaysia
#c("DEXSIUS","FRED"), #load Singapore
#c("DEXTAUS","FRED"), #load Taiwan
#c("DEXCHUS","FRED"), #load China
#c("DEXJPUS","FRED"), #load Japan
#c("DEXTHUS","FRED"), #load Thailand
#c("DEXBZUS","FRED"), #load Brazil
#c("DEXMXUS","FRED"), #load Mexico
#c("DEXINUS","FRED") #load India      
#  )
#data<-new.env()


# FredTicker = list(
#  c("sx5r","stoxx"),
#  c("sg2r","stoxx"),
#  c("sv2r","stoxx")
#  )
# mGetTickers(FredTicker)

## das Beispiel l?dt von der Stoxx-WebSite einige Indizes 
if (F)
{#SXXP,SXAP,SX7P,SXPP,SX4P,SXOP,SXFP,SX3P,SXDP,SXNP,SXIP,SXMP,SXEP,SXQP
  stoxxL =spl("SX86P,SXRP,S8730P,SX8P,SXKP,SXTP,SX6P"  )            
  StoxxTicker = lapply(stoxxL,FUN=function(x) return( c(tolower(x),"stoxx")))
  
  xdata = new.env()    
  mGetTickers(StoxxTicker,data =xdata)
  mGetTickers("Dax",data=xdata)
  ls(xdata)
  
  xdata = new.env()
  mGetTickers(list(c("sg2r","stoxx")),data =xdata,online=T)
  
  
  X=data.frame(readStoxx("sx8p","sx8p"))
  sx8p=NULL
  undebug(readStoxx)
  undebug(mGetTickers)
  
  #schreib die stoxxL- Liste als Universe.txt...
  stoxxTicker = lapply(c("sx5r","sg2r","sv2r",stoxxL),FUN=function(x) return( tolower(x)))
  m = cbind(stoxxTicker)
  colnames(m)<-c("member")
  write.table(m,file="Models/ta3_Stoxx/universe.csv",row.names=F,quote=T)
  
  
}

normaliseTickerName <-function(ticker)
{name = ticker
 if   (startsWith(name,"^")) 
   name <- sub( "^", "",name,fixed=TRUE)
 
 name <-sub("/","_",name,fixed=T)
 name=unlist(strsplit(name,"="))[1]
 return (name)
}
#########################################################################################################

mGetTickersNew<-function(FredTicker="", data=.GlobalEnv, frame1 ="", From = "2005-01-01")
{
  FredTicker = toString(FredTicker)
  if( !exists(FredTicker,envir=data))
    try(mGetTickers(FredTicker, data, frame1 , From ))
  else
    cat("\n",FredTicker ," already loaded ")
}

is.def<-function(x,  data=.GlobalEnv)
{
  #  assign
  #  mget("a",  .GlobalEnv,ifnotfound="nix")
  #  exists()
  
  #  if (!exists(x,envir=data))
  #    return(F)
  
  if (length(x)==0) return(F)
  if (is.null(x)) return(F)
  if (is.na(x)) return(F)
  return (T)
}

get.xlsPre<-function(tick="DAX")
{
  tick = toupper(tick) 
  ticker=first(SecList[SecList$Name==tick])
  ticker$preXls
}
###########################
#yahooTick= getYahooTicker("Dax")
###########################
#MM_PROVIDERx
getProviderTicker<-function(tick="Dax") #MM_TODO  - muss bei jedem zus?tzlichen Tick-Provider erweitert werden 
{
  tick = toupper(tick) #alle ticks werden via grossscheibung angetriggert
  
  if (len(SecList) ==0)  # globale def des Anlageuniversum  
  { 
    wb <- loadWorkbook(sprintf("%s/Securities.xls",customerData))#, create = TRUE)
    SecList <<- data.table(wb["SecList"])
    #    SecList$Name<<-toupper(SecList$Name) #alles keys werden gross geschrieben
    
  }
  ticker=first(SecList[SecList$Name==tick]$YahooTicker) 
  
  if (!is.na(ticker) && ticker!="") return (list(name=tick, tick=ticker,provider="yahoo"))
  
  ticker=first(SecList[SecList$Name==tick]$FRED)
  if (!is.na(ticker)&& ticker!="") return (list(name=tick, tick=ticker,provider="FRED"))
  
  
  ticker=first(SecList[SecList$Name==tick]$stoxxTicker)
  if (!is.na(ticker)&& ticker!="") return (list(name=tick, tick=ticker,provider="stoxx"))
  
  
  ticker=first(SecList[SecList$Name==tick]$ArivaTicker)
  if (!is.na(ticker)&& ticker!="") return (list(name=tick, tick=ticker,provider="Ariva"))
  
  
  ticker=first(SecList[SecList$Name==tick]$GoogleTicker)
  if (!is.na(ticker)&& ticker!="") return (list(name=tick, tick=ticker,provider="Google"))
  
  ticker=first(SecList[SecList$Name==tick]$OandaTicker)
  if (!is.na(ticker)&& ticker!="") return (list(name=tick, tick=ticker,provider="Oanda"))
  
  ticker=first(SecList[SecList$Name==tick]$QuandlTicker)
  if (!is.na(ticker)&& ticker!="") return (list(name=tick, tick=ticker,provider="Quandl"))
  
  if (is.na(ticker))
    sag("unknown tick at getProviderTicker %s",tick,warte=T)
  return(list(name=tick, tick=tick,provider="NO"))
}

if (F)
{
  getYahooTicker<-function(tick="Dax")
  {
    if (len(SecList) ==0)  # globale def des Anlageuniversum  
    { 
      wb <- loadWorkbook(sprintf("%s/Securities.xls",customerData))#, create = TRUE)
      SecList <<- data.table(wb["SecList"])
    }
    ticker=first(SecList[SecList$Name==tick]$YahooTicker)
    if (len(ticker)==0)
      sag("Bug at getYahooTicker %s",tick)
    return(ticker)
  }
  
  getArivaTicker<-function(tick="WorldWater_ETF")
  {
    if (len(SecList) ==0)  # globale def des Anlageuniversum  
    { 
      wb <- loadWorkbook(sprintf("%s/Securities.xls",customerData))#, create = TRUE)
      SecList <<- data.table(wb["SecList"])
    }
    ticker=first(SecList[SecList$Name==tick]$ArivaTicker)
    return(ticker)
  }
  
  getFredTicker<-function(tick="DEXINUS")
  {
    if (len(SecList) ==0)  # globale def des Anlageuniversum  
    { 
      wb <- loadWorkbook(sprintf("%s/Securities.xls",customerData))#, create = TRUE)
      SecList <<- data.table(wb["SecList"])
    }
    ticker=first(SecList[SecList$Name==tick]$FRED)
    return(ticker)
  }
  
  
  getQuandlTicker<-function(tick="NSE/OIL")
  {
    if (len(SecList) ==0)  # globale def des Anlageuniversum  
    { 
      wb <- loadWorkbook(sprintf("%s/Securities.xls",customerData))#, create = TRUE)
      SecList <<- data.table(wb["SecList"])
    }
    ticker=first(SecList[SecList$Name==tick]$QuandlTicker)
    return(ticker)
  }
}
#####################################################
## das heutige Datum als String
######################################################
todayS<-function(frm="%d.%m.%Y") #MMA
{
  format(Sys.Date(),frm)
}
todayS2<-function(frm="%Y-%m-%d") #MMA
{
  format(Sys.Date(),frm)
}

########################################################
# lade von der Ariva.de - Webpage die Zeitreihe mit dem arivaName runter.
# Wenn Du den nicht kennst such ihn vai Isin mit 
#getSymbols.ariva(c("LU0274221281","FR0010527275"))
#Weise dem Symbol WATER die Zeitreihe zu, die du auf Ariva unter dem K?rzel "100533557" findest
#getArivaSeries("100533557","WATER",from="2000-01-01",auto.assign=T)
#getArivaSeries("102867422","test",auto.assign=T)

if (F)
{ #MM11
  getSymbols.ariva(c("DE0008469115"))  #die Ariva-Isin zum REX
  DE0008469115
  a=getArivaSeries("6383","test",auto.assign=T)
  
  getSymbols.ariva(c("EU0009658251"))  #die Ariva-Isin zum REX
  a=getArivaSeries("965825","test",auto.assign=T)
  
  getSymbols.ariva(spl("FR0010245514"))
  
}


#http://www.bundesbank.de/cae/servlet/StatisticDownload?tsId=BBK01.WU046A&its_csvFormat#=de&its_fileFormat=csv&mode=its


if (F)
  a=getArivaSeries("100712940")
#########################################################
getArivaSeries<-function( arivaName="100533557",tic="", from = "1980-01-01", auto.assign=F, envir=.GlobalEnv ) #MMA
{
  if (from !="")
  {
    fromD = as.Date(from)
    #  fromD= as.Date("2012-01-01")
    aformat = format(fromD,"%d.%m.%Y")  #einmal das from - umformatieren
    maxtime = format(Sys.Date(),"%d.%m.%Y")
    
    url=sprintf("http://www.ariva.de/quote/historic/historic.csv?secu=%s&boerse_id=1&clean_split=1&clean_payout=0&clean_bezug=0&min_time=%s&max_time=%s&trenner=/",arivaName,aformat,maxtime)
    #url=sprintf("http://www.ariva.de/quote/historic/historic.csv?secu=%s&boerse_id=1&clean_split=1&clean_payout=0&clean_bezug=0&trenner=/",arivaName,aformat)
    
  }
  else
    url=sprintf("http://www.ariva.de/quote/historic/historic.csv?secu=%s&boerse_id=1&clean_split=1&clean_payout=0&clean_bezug=0&trenner=/",arivaName)
  
  #download.file(url, filename,  mode = 'wb')
  #browser()  
  tabDat = try( read.csv(url,  header=TRUE, stringsAsFactors=F,dec=",",skip=1,sep="/"))#
  #head(tabDat)
  #tail(tabDat)
  #browser()
  #browser(print("mmm1"))

  # browser()
  if (is.null(nrow(tabDat)))
  {
    mP("getArivaSeries: No TimeSeries for %s %s",arivaName,tic)
    # browser()
    return(NA)
  }
  #tabDat = merge(tabDat)
  colnames(tabDat)=spl("Index,.Open,.High,.Low,.Close,.Stuecke,.Volume")
 
  
  #evtl gibts den Namen ja schon in den Stammdaten - dann kann ich den mitabspeichern
  Ti = SecList[ArivaTicker==arivaName]
  if (nrow(Ti)>0)
  {
    Ti$Name=trim(Ti$Name)
    colnames(tabDat)=   c("Index", sprintf("%s.Open",Ti$Name),sprintf("%s.High",Ti$Name),sprintf("%s.Low",Ti$Name),sprintf("%s.Close",Ti$Name),sprintf("%s.Stuecke",Ti$Name),sprintf("%s.Volume",Ti$Name))
    
  }
  #browser(print("mmm1"))  
  tabData =as.Date(tabDat[,1])
  #ZahlenUmwandlung - auch dann wenn es sich um deutsche dec- Formate handelt #MMA
  tabDat_Dat=apply(tabDat[,-1],2,FUN=function(x)as.numeric(sub(",",".",x,fixed=T)))
  hist= as.xts(tabDat_Dat, order.by=tabData)
  hist = m.clean0(hist)
  # browser()
  hist = merge(hist,hist[,4])
  #noch Adjusted anhängen
  colnames(hist)=   c( sprintf("%s.Open",Ti$Name),sprintf("%s.High",Ti$Name),sprintf("%s.Low",Ti$Name),sprintf("%s.Close",Ti$Name),sprintf("%s.Stuecke",Ti$Name),sprintf("%s.Volume",Ti$Name),sprintf("%s.Adjusted",Ti$Name))
  
  hist = na.omit(hist)
  #wandel alles nach numeric - auch wenn , statt .  als dec sind
  if (auto.assign)
  {
    if (tic !=  "")
      assign(tic, hist, envir=envir)
    else
      assign(paste("arivaName",arivaName,sep=""), hist, envir=envir)
  }
  # browser() #MMX
  head(hist)
  #  mPlot(hist[,c(1,2,3,4)],main=tic,ylog_ = F)
  return (hist)
}
#
#x=   getArivaSeries("100533557")



###################################################################
#
# Yahoo hat f?r deutsche Wertpapiere 2013.3 die URL ge?ndert
#
###################################################################
getSymbols.YahooDe <- function   #MMAgetSymbols.yahooDE
(
  Symbols, 
  env = .GlobalEnv, 
  auto.assign = F,
  download = T  
) 
{  	
  yahooDE= NULL
  # read all Symbols
  for (i in 1:len(Symbols)) {	
    if(download) {
      symbol=Symbols[i]
      #%5E  f?r ^
      
      yahooDEName = gsub('\\^', '%5E', symbol)
      sym=normaliseTickerName(symbol)
      
      url = sprintf("http://ichart.finance.yahoo.com/table.csv?s=%s&a=10&b=26&c=1960&d=03&e=3&f=2013&g=d&ignore=.csv", yahooDEName)
      
      tabDat = read.csv(url,  header=F, stringsAsFactors=F,dec=".",skip=1,sep=",")#
      
      #head(tabDat)
      
      #for (ii in c(2:   ncol(tabDat)))
      #  tabDat[,ii] = as.numeric(tabDat[,ii])
      
      tabDat[,1] =as.Date(tabDat[,1])
      #    head(tabDat)
      colnames(tabDat)=spl(sprintf("Date,%s.Open,%s.High,%s.Low,%s.Close,%s.Volume,%s.Adjusted",sym,sym,sym,sym,sym,sym))
      hist= as.xts(tabDat[,-1], order.by=as.Date(tabDat[,1]))
      
      
      if (F)
      {
        hist = na.omit(hist)
        
        print(Symbols[i])
        print(yahooDEName)
        print(head(hist))
        ft=fromTo(hist)
        #browser()
        mPlotPureData(hist[,c(1,2,3,4)],main=sprintf("%s",yahooDEName))
      }
      
      #   plot(hist)
      
      
      filename=sprintf("MData/%s.csv",sym)
      print(filename)
      #schreibe die Zeitreihe - nach temp -- noch hat sie ja keinen offiziellen Namen
      try(  write.zoo(hist, file=filename, sep=";",  dec="."))
      
      if (auto.assign) {  	
        assign(sym, hist, env)	
        
        return(hist)
      }
      
    }	
  }
}

if (F)
  getSymbols.YahooDe("Rex")



#########################################################
#Yahoo ist der default-Provider:
# Provider sind zur Zeit :   yahoo,FRED,stoxx,ariva
#yahoo, google, MySQL, FRED, csv, RData, and oanda.

if (F)
{
  mGetTickers("^GDAXI" )
  
  mGetTickers("Rex",online=T )
  mGetTickers("^GREXP",online=T )
  
  getSymbols.YahooDe("Rex") #der liefert die volle L?nge
  
  #  Rex=mGetTickers("Rex",online=T )
  
  mGetTickers(list( spl("STOXXEurope600 AutomobilesParts", "stoxx")),online=T)
  
  
  
  mGetTickers("Dax",online=T)
  mGetTickers(list( spl("sg2r, stoxx")),online=T)
  #, spl("100533557,ariva")),online=T)
  
  FredTicker = list(
    c("DEXKOUS","FRED"), #load Korea
    c("DEXMAUS","FRED"), #load Malaysia
    c("DEXSIUS","FRED"), #load Singapore
    c("DEXTAUS","FRED"), #load Taiwan
    c("DEXCHUS","FRED"), #load China
    c("DEXJPUS","FRED"), #load Japan
    c("DEXTHUS","FRED"), #load Thailand
    c("DEXBZUS","FRED"), #load Brazil
    c("DEXMXUS","FRED"), #load Mexico
    c("DEXINUS","FRED") #load India 
  )
  mGetTickers(FredTicker,online=T)
  mGetTickers("DEXINUS",online=T)
  mGetTickers("Dax",online=T)
  mGetTickers("100533557",online=T)
  mGetTickers("WorldWater_ETF",online=T)
  mGetTickers(list(c("CUSR0000SEEA","FRED")),online=T)
  #FRED:
  #o:\R\Nuggets\Eckhard\MData\Fundamentals\Fred\Germany - ALFRED - St. Louis Fed.url
  #Chicago Fed National Activity Index: Personal Consumption and Housing (CANDH)
  #FRED:   http://research.stlouisfed.org/fred2/series/CANDH?cid=32457
  mGetTickers(list(c("CANDH","FRED")),online=T)
  mPlot(CANDH[,4],ylog_=F)
  #oder Production of Total Industry in Germany (DEUPROINDMISMEI)
  mGetTickers(list(c("DEUPROINDMISMEI","FRED")),online=T)
  mPlot(DEUPROINDMISMEI[,4],ylog_=F)
  
  mGetTickers(list(c("peter","Ariva","100712940")),online=T)
  mGetTickers(list(c("peter","Ariva"," 846911")),online=T)
  
  data=new.env()
  mGetTickers(list(c("BCB/UDJIAD1","Quandl")),data=data,online=T)
  mGetTickers(list(c("NSE/OIL","Quandl")),data=data,online=T)
  
}
#########################################################


mGetTickers<-function( ..., data=.GlobalEnv, frame1 ="", from = "1975-01-01", doAdjust=F, online=F) #MMA
{
  FredTicker = c(...)
  Frednames = NULL
  maxFrom = "1990-01-01"
  #series = NULL
  FredTicker = c(FredTicker)
  
  for (nameL in FredTicker)
  {
    dontSave=F
    name = ""
    provider = ""
    tic =""
    series = NULL
    
    # browser()#MMX
    if (length(nameL)>0 ) #nameL != ""  ??
    {
      if (len(nameL) < 2)
      {
        tic =name = toString(nameL)
        provider="yahoo"  
        
        ticX= getProviderTicker(name)  #noch nachtr?glich eingebaut
        #ticX=getYahooTicker(name)
        
        provider=ticX$provider
        tic = ticX$tic
        
        
        if (is.na(tic) || provider =="NO") #das wertpapier ist in Securities noch nicht angelegt
        {tic = name
         mP("security %s = %s is missing at Securities.xls .. use name an yahoo as provider", tic,name) 
         #browser()
         provider="yahoo" 
        }
      }
      else
        if (len(nameL)<3)
        {
          tic = name=nameL[1]
          provider=nameL[2]
        }
      else
      {
        name=nameL[1]
        provider=nameL[2]
        tic = nameL[3]
        
        if (tic=="" || provider =="")
        {
          ticX= getProviderTicker(toupper(name))  #noch nachtr?glich eingebaut
          
          provider=ticX$provider
          tic = ticX$tic
          
        }
        
      }
      
      yahooName = tic
      if   (startsWith(name,"^")) 
        name <- sub( "^", "",name,fixed=TRUE)
      name=unlist(strsplit(name,"="))[1]
      
      
    }
    #browser()
    name=normaliseTickerName(name)
    if (is.null(Frednames))
      Frednames = name
    else
      Frednames = c(Frednames,name)
    
    File=paste("MData/",name,".csv",sep="")
    if (exists("globMDataSUB"))
      if (nchar(globMDataSUB) >1 )
      {
        mP("mGetTickers reads from globalMDataSUB: %s",globMDataSUB)
        File=paste("MData/",globMDataSUB,"/",name,".csv",sep="")
      }
    
    #browser()
    
    namePrices = paste(name,"Adjusted",sep=".")
    nameOpen = paste(name,"Open",sep=".")
    nameHigh = paste(name,"High",sep=".")
    nameLow = paste(name,"Low",sep=".")
    nameVol = paste(name,"Volume",sep=".")
    nameClose = paste(name,"Close",sep=".")
    
    if (!online)
      if (exists("globalPROVIDER"))
        if (globalPROVIDER != "")
        {provider =globalPROVIDER
         cat("use globalPROVIDER instead of ")
         cat(provider)
        }
    
    
    #versuche zun?chst mal die Datei aus der serialisierten Version zu holen
    #versuche zun?chst DB - dann CSV
    
    if ( T || !exists(name,envir=data))
    {
      
      # if (provider =="yahoo")  #Suche auf dem SQL-Server
      if (is.null(series) && !online)
      {
        print (paste(" dbGetSeries  ",name))
        series = dbGetSeries(name,from = From, env = data)
        print(tail(series,2))
        
        #  sag("ok",T)
        if (!is.null(series)) 
        { series = get(name,envir=data)
          write.zoo(series, file=File, sep=";",  dec=".")
        }
      }
      #browser("xx")
      if (is.null(series) )  #noch nicht mit dbGetSeries geholt
      {
        
        sag("read file")
        
        if (file.exists(File) && !online)
          
        { mP("read.zoo  %s",File)
          if (csvDatFormat =="")
            csvDatFormat="%Y-%m-%d"
          
          if (exists("globalDEC"))
            dec_ = globalDEC
          else
            dec_ = "."
          if (exists("globalSEP"))
            sep_ = globalSEP
          else
            sep_ = ";"
          
          
          mP("read.zoo# %s  ",File)# , csvDatFormat      ,globalDEC , globalSEP)
          #MMX read at mGetTickre
          series = read.zoo( file=File, sep=sep_,dec=dec_,format=csvDatFormat,header =TRUE)        
          # browser()
          #mach ihn gegen dec-probleme robuster
          if (class(coredata( (series))[1])!="numeric") #Cl
          {
            if (dec_==".")
              series = read.zoo( file=File, sep=sep_,dec=",",format=csvDatFormat,header =TRUE)        
            
            else
              series = read.zoo( file=File, sep=sep_,dec=".",format=csvDatFormat,header =TRUE)     
          }
          
          #head(series)  MM_read
          #  browser()
          #sx = xts(series)
          #   browser()
          #  if (dim(sx))
          #series=read.table(file=File, sep=sep_,dec=dec_,header =TRUE)
          
          print (paste("load file",File,sep=":"))
          
          tail(series,1)
          dontSave=T
        }
        
        # browser(text="mGetGickers")
        
        if (is.null(series) ||  online)  #noch nicht in der DB oder online wird erzwungen
        {
          dontSave=F
          if (!online) mP("no serialised data found - look at provider %s ",provider)
          else
            mP("online download %s - look at provider %s ",name, provider)
          #xx =get("xxxx")
          #browser()
          # INTERNET-Download
          if (provider=="Sonstige") #----------------------------------------------------------------------------
{ #fundamentaldaten download -z.B. ifo
  
  #TODO:   fertig machen: rufe jeweils eine spezial-Methode zum einlesen .. z,B. f?r IFO
  s = sprintf("read_%s(%s, %s)", name, name, tic)
  series =eval(parse(text = s))
  #read_<name>(name, provider) ##  keine Nachbearbeitung mehr .. oder doch ?
  #mP("InternetDownload: readStoxx %s",name)
  #series = readStoxx(name)
}
else
  
  if (provider=="stoxx") #----------------------------------------------------------------------------
{
  mP("readStoxx %s",name)
  series = readStoxx(name,tic)
}
else
{mP("InternetDownload: getSymbols %s %s",provider,yahooName)
 
 mist= tryCatch(
   
   
{
  #browser()
  #Rex=NULL
  #GREXP=NULL
  #yahooName ="^GREXP"
  
  #getQuote.google(spl('MSFT,AAPL,IBM')  #MM_TODO  - checke ob auch google-als provider geht
  #browser()
  ######################################################################################
  #  Internet Download
  ######################################################################################
  #MM_PROVIDER
  if (provider == "Ariva")  
  {
    series=try(getArivaSeries(tic , auto.assign=F))
    yahooName = name
    assign(name,series,envir=data)
    doAdjust=F
  }
  else
    #
    if (provider == "Quandl")  
    {
      library(Quandl)
      if (!exists("init.quandl_") )
      {
        mP("login at Quandl")
        Quandl.auth("98xyEn88QWsfqq7aspHU") #hab ich im http://www.quandl.com/DOE-US-Department-of-Energy/EIA_TOTALOILSUPPLY_A_GERMANY73-Total-Oil-Supply-Germany
        #Download-manager aus dem download via rcode-generator
        init.quandl_ <<-F
      }
      #browser()
      # series = try(Quandl("/YAHOO/INDEX_GDAXI",type = "xts"))
      series = try(Quandl(tic,type = "xts"))
      
      name =first(SecList[SecList$QuandlTicker==tic]$Name)
      
      #  name=normaliseTickerName(tic)
      if (len(colnames(series))==1)
      {
        doAdjust=F
        colnames(series) =name
        series=merge(series,series,series,series,series,series)
        colnames(series) = spl(sprintf("%s.Open,%s.High,%s.Low,%s.Close,%s.Volume,%s.Adjusted",name,name,name,name,name,name))
      }
      else
        # browser()
        if (dim(series[,"Adjusted Close"])[2]!=0) #enth?lt  "Adjusted Close"
        {
          doAdjust=F
          oseries= series
          series = oseries[,spl("Open,High,Low,Close")] ; 
          if (dim(oseries[,"Volume"])[2]!=0)
            series=merge(series,oseries[,"Volume"],oseries[,"Adjusted Close"])
          else
            series=merge(series,0,oseries[,"Adjusted Close"])
          
          colnames(series) = spl(sprintf("%s.Open,%s.High,%s.Low,%s.Close,%s.Volume,%s.Adjusted",name,name,name,name,name,name))
          
        }
      else
        if (ncol(series)>=4)
        {
          doAdjust=F
          series = series[,spl("Open,High,Low,Close")]
          series = merge(series,0, series[,"Close"]) #eine pseudo-adjusted splate
          colnames(series) = spl(sprintf("%s.Open,%s.High,%s.Low,%s.Close,%s.Volume,%s.Adjusted",name,name,name,name,name,name))  
        }
      else
      { 
        series =price2dataPrice(series[,1],name)
      }
      #browser()
      #browser(mP("qqqq"))
      
      yahooName = name
      assign(name,series)
    }
  else
  { 
    yN= yahooName
    #TB3M = quantmod::getSymbols('DEXINUS', src='FRED', auto.assign = FALSE)
    try(  getSymbols(yahooName,src=provider,from = from, auto.assign=TRUE))
    #notfalls f?r deutsche Symbole auf einer anderen yahoo-url nachschauen
    if   (startsWith(yahooName,"^")) 
      yahooName <- toupper(sub( "^", "",yahooName,fixed=TRUE))
    
    NewWP =try(get(yahooName))
    
    # TB3M = quantmod::getSymbols('DEXINUS', src='FRED', auto.assign = FALSE)
    if (provider =="FRED" && dim(NewWP)[2]==1)
    {
      #standard-spalten draus machen
      NewWP=merge(NewWP,NewWP,NewWP,NewWP,NewWP,NewWP)
      symbol=trim(yahooName)
      colnames(NewWP)=c( paste(symbol,".Open"),paste(symbol,".High"),paste(symbol,".Low"),paste(symbol,".Close"),paste(symbol,".Volume"), paste(symbol,".Adjusted"))
      
    } 
    else
      # browser()
      if (!exists(yahooName) || len(NewWP) <2  || dim(Cl(NewWP))[2]>1)
      {
        NewWP=  try(  getSymbols.YahooDe(yN, auto.assign=TRUE))
        #browser()  
      }
    
    if (len(NewWP)>0 && dim(Cl(NewWP))[2]>1)
    {
      sag("mist .. 2 Cl- spalten - liefet yahoo wieder AdjustClose  ??")
      browser()
    }
  }
  #yahooName="^Gdaxi"  .. dann kommt aber GDAXI an 
  #yahoo, google, MySQL, FRED, csv, RData, and oanda.
  
  if   (startsWith(yahooName,"^")) 
    yahooName <- toupper(sub( "^", "",yahooName,fixed=TRUE))
  
  #browser() 
  
  if (name != yahooName)
  {
    #browser()
    assign(name,NULL)
    assign(name,get(yahooName))
    series <- get(name)
    series <-get(yahooName)
    if (!is.null(series) && len(series) > 0)
    {
      dontSave = F
      
      mP("internet download of %s to symbol %s: %d lines ",yahooName,name,len(series))
    }
    else
    {
      dontSave =T
      mP("internet download missed of %s to symbol %s ",yahooName,name)
    }
    #   sapply(c(1,2,3),FUN=function(x)print(x))
    #umbenennen der Spalten
    # browser()
    
    c<-colnames(series)
    
    #ccolnames(get(name))<- 
    colnames(series) =sapply(  c, FUN= function(x) sub(yahooName, name, x,fixed =T))
    
    #browser()
    head(series)
    # browser()
    assign(name,series)   #MM_BUG
    if (!is.null(yahooName) && yahooName != name)
      rm(yahooName)
    
  }
  
  series = get(name)  #MM_BUG
},
error=  function(e) 
{
  print(e)  
  series = NULL
  try(if (exists( x=name, envir=data))
    try(rm( list(name),envir=data)))
  return("mist")
})

#getSymbols(yahooName,src=provider, from= maxFrom, env=data)       
# if (mist != "mist")

}
#try(rm(name))
try(mP("from Internet %s",toString(dim(series))))
        }

if (!is.null(series) && !dontSave && ! is.character(series))
{ 
  xlsPre = get.xlsPre(name) #etf
  if (!is.null(xlsPre))  #vor noch Daten aus einem xls-file einlesen
  {
    xlsPre.file = unlist(str_split(xlsPre,"#"))[1]
    xlsPre.col = unlist(str_split(xlsPre,"#"))[2]
    xlsPre.p=read.Xls(xlsPre.file) #der weiß wie man aus xlsPre.file liest
    xlsPre.col.p = xlsPre.p[,xlsPre.col]  #hier sollte jetzt eine Datenspalte liege
    if (len(xlsPre.col.p)<1)
      sag("Bug at #etf",warte=T)
    newSeries = NULL
    for(coln in colnames(series))
       {new.s = enlarge.prices(p=series[,coln], p0=xlsPre.col.p)
        if (is.null(newSeries))
            newSeries= new.s else newSeries = merge(newSeries,new.s)
       }
    series = newSeries
  }
  #series = cbind(series,series)
#  if (name == "REX_ISHARED_ETF")
#    browser(print("mmmm1"))
  mP("-------------  save File %s %s",File, toString(head(series)))
  
  write.zoo(series, file=File, sep=";",  dec=".")
}
else
  mP("no Data at Internet for %s",name)
      }
#print(paste(" getSymbols ",name))
#colnames(prices)<-Frednames
#colnames(series))
if (!is.null(series) && ! is.character(series) )
{
  
  print(paste(name, toString( len(series)),sep=" has "))
  print(head(series,1));print(tail(series,1))
  #von = (series)[1]),'%d%b%y')
  #bis=  "??"# format(index(row.names(series)[nrow(row.names(series))]),'%d%b%y')   #format(index(tail(data[[i]],1)[1]), '%d%b%y')
  #mP(" !!### %s    from %s til  %s",tit,von,bis)
  #browser()
  series = as.xts(series)
  ###.......... evtl. mit preXls - Daten vorn verlängern 
  
  ticker=first(SecList[SecList$Name==name]$YahooTicker) 
 
  
  #....................................................  
  if (provider != "Sonstige")
  {
    if (is.null(dim(series)) || dim(series)[2]<2)
    {
      
      s = series
      s[,1] =  as.numeric(series[,1]) ##MM_TOCHECK
      #s=na.omit(s)
      
      #    if (name=="REX_RI_slope90")
      #      browser()
      #  s = series  
      #    s=""
      #    tryCatch.W.E(  na.omit(series))   
      #    s =         na.omit(series)
      
      
      #browser()
      
      series = cbind(series,s,s,s,rep(0.000001,len(series)),s)     
      colnames(series)<- c(nameOpen,nameHigh,nameLow,nameClose,nameVol, namePrices)       
      series = na.omit(series)
      if (!dontSave)
        write.zoo(series, file=File, sep=";",  dec=".")
      
      #if (provider == "FRED")
      #  browser()
      #if (!is.null(series))
      #  assign(name,series,envir =data)
    }
    else
    {
      #assign(name,series,envir =data)
      #assign(name,series)
      
      if (doAdjust)
        series<- M_adjustOHLC(series, name,use.Adjusted=T);
      #estr=paste("series<- adjustOHLC(",name,")",sep="")
      #print(estr)
      #eval(parse(estr))
      
      #browser()
      # series = m.clean0(series)
      
      #series = na.omit(series)  #MM_WN1
      
      #if (!is.null(series))
      #  assign(name,series,envir =data)
    }
  }
  
  #head(series)
  if (!is.null(series))           
  {
    print(sprintf("assign series %s",name))    #MMMM
    if (name =="Rex")
    {
      # write.xts(series,"test1.csv")
      #browser()
      #sss =  m.clean0(series)
      #head(sss)
    }
    #head(series,1)
    #browser()
    #if (exists( x=name,envir=data))
    #if (!is.null(data[[name]]))
    #  rmSymbol(name,data=data)
    #try(rm(envir=data, list(name)))
    
    #data = t0rex$t0data
    #ls(data)
    #Rex=data[["Rex"]]
    #rm(Rex,envir= data)
    
    # browser()
    #  try( assign(name,series,envir=.GlobalEnv))   //MM2
    if (exists(name,envir=data))
      rmSymbol(name,envir=data)
    try(assign(name,series,envir=data))
  }
}
    }else print(paste(name," exists"))           

  }
#########################################################################################################
#baue eine Tabellen-Objekt aus den Close-Teilen
return (NULL)  #<<<<<<<<<<<<<<<<<<<<<<<<<<

prices<-NULL
symbolnames = NULL

#  for(i in data$symbolnames) {colnames(data[[i]])=normColnameS(data[[i]])}

for(i in ls(data))  
{ 
  colnames(data[[i]])=normColnameS(data[[i]]) #MMK
  
  if (is.null(prices))
  {prices = Cl(data[[i]])
   symbolnames = i
  }
  else
  {prices = na.omit(merge(prices,Cl(data[[i]])))
   symbolnames = c(symbolnames,i)
  }
}
colnames(prices)<-symbolnames
if (frame1 == "")
  prices = as.xts(prices)
else
  prices = as.xts(prices)[frame1]

return(prices)
}
#############################################################################################
##. in  manchen xls-sheets stehen price-daten die ich ich zur verlängerung von etf nutzen kann - > siehe  #etf
hua <- NULL

read.Xls<-function(xlsFile) #etf
{
 xlsFile.base=  basename(xlsFile)
 
 if (toupper(xlsFile.base) =="AWDATA.XLS")
 {
   if (is.null(hua))
 hua<<- read.HUA.XLS( modelDir= dirname(xlsFile), xlsName="AWData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=F,dec=",",date.format="%d.%m.%Y",selectedCols=c(1:15))
 # fromToS(na.omit(hua))
prices=hua[fromToS(na.omit(hua)),]
 colnames(prices) = data$symbolnames=spl("SUP500,MSCIEUROPE,MSCIEMERGINGM,TOPIXMSCIJAPAN,PHYSICALGOLD,IGBONDS,HYBONDS,TIPS,REXP,EUROTREASURIES,EMTREASURIES,DBCURRENCIES,MSCIWORLDGDB,MSCIEUROPEGDB,BARCLAYS")    	
return(prices)
 }
 else
 {
   sag("read.Xls: unknown  xlsFile %s ",xlsFile,warte=T)
   return(NULL)
 }
}
#---------------------------------------------------------

#häng die p0 xts vorn vor p
enlarge.prices<-function(p,p0)
{
p.org = p
p0.org = p0
p=mNorm(p);p0 =mNorm(p0)
p.f = first(p)
P0 = p0[sprintf("::%s",DateS(p.f))]
if (len(P0)>0)
{
  d=nval(last(P0)-p.f)
  P0 = P0-d 
  p.new = rbind(P0,p[-1,])
  plot(p.new,main=sprintf("enlarge.prices %s",colnames(p)))
  amark(p.f)
}
else
  p.new = p
return(p.new)
}


#############################################################################################

M_adjustOHLC<-function (x,symbol.name, adjust = c("split", "dividend"), use.Adjusted = FALSE, 
                        ratio = NULL) 
{
  # browser()
  sag("at adjust")
  #browser()
  if (is.null(ratio)) {
    if (use.Adjusted) {
      if (!has.Ad(x)) 
      {
        print(symbol.name)
        print(head(x,3))
        #stop("no Adjusted column in 'x'")
        ratio=1
        #browser()
        if (symbol.name=="REX")
          browser()
      }
      else
        ratio <- Ad(x)/Cl(x)
    }
    else {
      div <- getDividends(symbol.name)
      splits <- getSplits(symbol.name)
      ratios <- adjRatios(splits, div, Cl(x))
      if (length(adjust) == 1 && adjust == "split") {
        ratio <- ratios[, 1]
      }
      else if (length(adjust) == 1 && adjust == "dividend") {
        ratio <- ratios[, 2]
      }
      else ratio <- ratios[, 1] * ratios[, 2]
    }
  }
  #  head(x)
  
  
  Op1=Op(x);if (len(Op1[!is.na(Op1)])==0) Op1 =0  else Op1 = m.ifna.prev(Op1)
  Lo1 =Lo(x);if (len(Lo1[!is.na(Lo1)])==0) Lo1 =0 else Lo1 = m.ifna.prev(Lo1)
  Hi1 = Hi(x);if (len(Hi1[!is.na(Hi1)])==0) Hi1 =0 else Hi1 = m.ifna.prev(Hi1)
  Cl1 = Cl(x)[,1];if (len(Cl1[!is.na(Cl1)])==0) Cl1 =0 else Cl1 = m.ifna.prev(Cl1)
  
  if (F)
  {
    Op1[is.na(Op1)] <-0
    Lo1[is.na(Lo1)] <-0
    Hi1[is.na(Hi1)] <-0
    Cl1[is.na(Cl1)] <-0
  }
  Adjusted <- Cl1 * ratio
  
  res=structure(cbind((ratio * (Op1 - Cl1) + Adjusted), 
                      (ratio * (Hi1 - Cl1) + Adjusted), 
                      (ratio * (Lo1 - Cl1) +  Adjusted), 
                      Adjusted,
                      if (has.Vo(x)) Vo(x) else 0,                                                                                                           
                      if (has.Ad(x)) 
                        Ad(x)  else 0),
                .Dimnames = list(NULL, colnames(x)))
  
  #browser()
  #head(res)
  #write.xts(res,"test4")
  return(res)
}


mcolnames<-function(dat,newCols)
{
  if(is.null(dim(dat)))    
  {dat = cbind(dat,dat)
   colnames(dat)<-c(newCols)
   return(dat)
  }
  else
    colnames(dat)<-newCols
  return (dat)
}


mm.RiskReturnScatter<-function (R, Rf = 0, main = "Annualized Return and Risk", add.names = TRUE, 
                                xlab = "Annualized Risk", ylab = "Annualized Return", method = "calc", 
                                geometric = TRUE, scale = NA, add.sharpe = c(1, 2, 3), add.boxplots = FALSE, 
                                colorset = 1, symbolset = 1, element.color = "darkgray", 
                                legend.loc = NULL, xlim = NULL, ylim = NULL, cex.legend = 1, 
                                cex.axis = 0.8, cex.main = 1, cex.lab = 1, ...) 
{
  if (method == "calc") 
    x = checkData(R, method = "zoo")
  else x = t(R)
  if (is.null(dim(Rf))) 
  {Rf = R[,1]
   Rf[,1] =0
  }
  columns = ncol(x)
  rows = nrow(x)
  columnnames = colnames(x)
  rownames = rownames(x)
  if (length(colorset) < columns) 
    colorset = rep(colorset, length.out = columns)
  if (length(symbolset) < columns) 
    symbolset = rep(symbolset, length.out = columns)
  if (method == "calc") {
    comparison = t(table.AnnualizedReturns(x[, columns:1], 
                                           Rf = Rf, geometric = geometric, scale = scale))
    returns = comparison[, 1]
    risk = comparison[, 2]
    rnames = row.names(comparison)
  }
  else {
    x = t(x[, ncol(x):1])
    returns = x[, 1]
    risk = x[, 2]
    rnames = names(returns)
  }
  if (is.null(xlim[1])) 
    xlim = c(0, max(risk) + 0.02)
  if (is.null(ylim[1])) 
    ylim = c(min(c(0, returns)), max(returns) + 0.02)
  if (add.boxplots) {
    original.layout <- par()
    layout(matrix(c(2, 1, 0, 3), 2, 2, byrow = TRUE), c(1, 
                                                        6), c(4, 1), )
    par(mar = c(1, 1, 5, 2))
  }
  plot(returns ~ risk, xlab = "", ylab = "", las = 1, xlim = xlim, 
       ylim = ylim, col = colorset[columns:1], pch = symbolset[columns:1], 
       axes = FALSE, ...)
  if (ylim[1] != 0) {
    abline(h = 0, col = element.color)
  }
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  if (!add.boxplots) {
    title(ylab = ylab, cex.lab = cex.lab)
    title(xlab = xlab, cex.lab = cex.lab)
  }
  if (!is.na(add.sharpe[1])) {
    for (line in add.sharpe) {
      abline(a = (Rf * 12), b = add.sharpe[line], col = "gray", 
             lty = 2)
    }
  }
  if (add.names) 
    text(x = risk, y = returns, labels = rnames, pos = 4, 
         cex = 0.8, col = colorset[columns:1])
  rug(side = 1, risk, col = element.color)
  rug(side = 2, returns, col = element.color)
  title(main = main, cex.main = cex.main)
  if (!is.null(legend.loc)) {
    legend(legend.loc, inset = 0.02, text.col = colorset, 
           col = colorset, cex = cex.legend, border.col = element.color, 
           pch = symbolset, bg = "white", legend = columnnames)
  }
  box(col = element.color)
  if (add.boxplots) {
    par(mar = c(1, 2, 5, 1))
    boxplot(returns, axes = FALSE, ylim = ylim)
    title(ylab = ylab, line = 0, cex.lab = cex.lab)
    par(mar = c(5, 1, 1, 2))
    boxplot(risk, horizontal = TRUE, axes = FALSE, ylim = xlim)
    title(xlab = xlab, line = 1, cex.lab = cex.lab)
    par(original.layout)
  }
}

MplotNormedPrices<-function(r=ret,frame="", bench_ = "" , ylog_=T, main="Returns",legend.loc_ = "",risk.dates_ = risk.dates,risk.labels_ = risk.labels)
  
{  
  #sieh auch   plotNormedPrices(cumsum(mROC(data$prices)))
  
  
  ret = na.omit(r[frame])
  if (len(bench_)>1)
    ret =selectBench(ret,bench_)
  mat = mRendite(ret)
  
  if (ylog_)
    mat= log(mat)
  
  #mat = mLogRendite(ret)
  
  rang=range(mat,na.rm = FALSE)
  
  cols = rainbow(ncol(mat))
  colSet = c("red", topo.colors(ncol(mat)))
  colSet = cols
  
  #browser()
  ft=fromTo(mat)
  
  Auflegedatum=ft[1]
  Bisdatum = ft[2]
  try(chart.RiskReturnScatter(ret,  main = sprintf("RiskReturn %s to %s  ",ft[1],ft[2]), colorset=cols))# colorset = rainbow8equal))
  
  
  title=sprintf("Stock Returns %s-%s ",Auflegedatum,Bisdatum)
  #colSet = c("red",palette(gray(seq(0,.9,len=ncol(ret)-1))))
  plot(mat[,1], ylim =rang, main=title)
  for(ci in 1:ncol(ret))
    lines(mat[,ci],col=colSet[ci],lwd=2)
  # so turn off clipping:
  #par(xpd=TRUE)
  #legend(2.8,-1,c("group A", "group B"), pch = c(1,2), lty = c(1,2))   
  legend("topleft", colnames(mat), cex=0.8, bty="n",
         col=colSet,pch=19)
  
  #labs=colnames(mat)
  #axis(2, coredata(last(mat)),labels=FALSE)
  #mtext(labs, 2, at=coredata(last(mat)), col=1,las=1,line=1,cex=0.7)
  #for (i in seq(par("yaxp")[1],par("yaxp")[2],by=(par("yaxp")[2]-par("yaxp")[1])/par("yaxp")[3])) {
  #  abline(h=i, col="gray70")}
  
}

#mPlot(x)


# mPlot:  die Log-Darstellung ist hier der Default-Wert    #MMcheck
mPlot <-function(...,frame="" , scale=0, ylog_=T, main="",legend.loc_ = "",risk.dates_ = risk.dates,risk.labels_ = risk.labels,signal=NULL)  #MMA
{ 
  
  prices = na.omit(merge(...))
  
  if (scale ==1)
    #t2 =lapply(colnames(prices), FUN=function(col){ prices[,col]=scaleTo(prices[,col],c(0,1)); return(col)})
    for (col in colnames(prices))
    {
      prices[,col]=scaleTo(prices[,col],c(0,1))
    }
  
  
  #LeftMargin=1
  #hasTitle = !is.null(main);
  #par( mar = c(iif(plotX,2,10), LeftMargin , iif(hasTitle,2,0), 3) )
  
  
  if (ylog_)
  {
    #browser()
    #MM! gib die price-columns zur?ck die 0-Werte enthalten  
    x_0=na.omit(unlist(lapply(colnames(prices),FUN=function(col) {X=prices[,col];ifelse(nrow( X[X ==0])>0,col,NA)})))
    
    if  (!is.def(x_0) || max(x_0)==0)
      prices=log(prices)    
    else
    {
      cat("mPlot-ylog_ omits columns with 0 values: ")
      cat("\n######\n")
      #browser()
      cat(x_0)
      cat("\n")
      
      prices =prices[,colnames(prices)!=x_0 ]
    }
  }
  # browser() #T!
  #if (main == "")
  #  main = toString(colnames(prices))
  
  mat = na.omit(prices[frame])
  
  rang=range(mat,na.rm = FALSE)
  ft=fromTo(mat)
  Auflegedatum=ft[1]
  Bisdatum = ft[2]
  if (main=="no")
    title=""
  else
    title=sprintf("%s %s bis %s ",main,Auflegedatum,Bisdatum)
  cols = rainbow(ncol(mat))
  colSet = c("red", topo.colors(ncol(mat)))
  #colSet = c("red",palette(gray(seq(0,.9,len=ncol(ret)-1))))
  #browser("mPLot")  
  
  plot((mat[,1,drop=F]), ylim =rang, main=title)
  
  
  #grid()
  if (T)
  {
    axis(4)  
    for(ci in 1:ncol(mat))
      lines(mat[,ci,drop=F],col=colSet[ci],lwd=2)
  }
  # so turn off clipping:
  #par(xpd=TRUE)
  #legend(2.8,-1,c("group A", "group B"), pch = c(1,2), lty = c(1,2)) 
  
  #legend("topleft", colnames(mat), cex=0.6, bty="n", col=colSet,pch=19)
  plota.legend(colnames(mat),colSet[1:ncol(mat)],pch=10,cex=0.6)
  
  
  #labs=colnames(mat)
  #axis(2, coredata(last(mat)),labels=FALSE)
  #mtext(labs, 2, at=coredata(last(mat)), col=1,las=1,line=1,cex=0.7)
  #for (i in seq(par("yaxp")[1],par("yaxp")[2],by=(par("yaxp")[2]-par("yaxp")[1])/par("yaxp")[3])) {
  #  abline(h=i, col="gray70")}
  
  if (!is.null(signal))
  {
    dax=mat[,1]
    k=na.omit(merge(dax,dax,signal,dax,dax)); 
    k[,4] = ROC(k[,4])
    k=na.omit(k)
    k[,5] = mRendite(k[,4]*k[,3])
    #zeichne den guv
    lines(k[,5],col="green",lwd= 2)
    k[which(k[,3]==0),1]=NA
    #markiere die Long-Phase
    lines(k[,1],lwd=3,col="yellow",type="l")
    
    legend("topleft", c(colnames(mat),"guv"), cex=0.6, bty="n",
           col=colSet,pch=10)
    
    
  }
  return("OK")  
}



#####################################################################################################
#plotte normierte preise
#ret = (prices / mlag(prices) - 1)
#oder
#r <- cumsum(diff(log(prices))[-1,])
#oder
#US10y.roc <- diff(DGS10, lag=1)
#US10y.roc[1,] <- 0
# These are start and end dates, formatted the same way as the default axis labels
#data(edhec)
# Event lists - FOR BEST RESULTS, KEEP THESE DATES IN ORDER
risk.dates = c(
  "Oct 87",
  "Feb 94",
  "Jul 97",
  "Aug 98",
  "Oct 98",
  "2000-07-01",
  "2001-09-11",
  "2008-09-15",
  "2011-03-11",
  "2011-07-29"
  
)
risk.labels = c(
  "Black Monday",
  "Bond Crash",
  "Asian Crisis",
  "Russian Crisis",
  "LTCM",
  "Tech Bubble",
  "Sept 11",
  "Lehman",
  "Fukushima",
  "Europe Crisis"
)

#R=edhec[,"Funds of Funds",drop=FALSE]
#Return.cumulative = cumprod(1+R) - 1
#chart.TimeSeries(Return.cumulative)
#chart.TimeSeries(Return.cumulative, colorset = "darkblue", legend.loc = "bottomright", period.areas = cycles.dates, period.color = "lightblue", event.lines = risk.dates, event.labels = risk.labels, event.color = "red", lwd = 2)



#####################################################################################################
plotNormedPrices<-function(r=data$prices,frame="", bench_ = BENCH , ylog_=F, main="Returns",legend.loc_ = "",risk.dates_ = risk.dates,risk.labels_ = risk.labels)
{
  title_=main
  
  r = na.omit(r[frame])
  # plot.zoo(as.zoo(r), screens = 1, col = 1:ncol(r),
  # lwd = 3,
  #   par.settings = theEconomist.theme(box = "transparent"),
  #        lattice.options = theEconomist.opts(),  main = "prices",
  #        
  #         xlab = "Testdate", ylab = "cumsum(diff(log(price)))"
  #        )
  datum1=format(index((r)[1,]), '%d%b%y')
  datum2 = format(index(tail(r,1)[1,]), '%d%b%y')
  
  n = ncol(r)
  rgb = colorRampPalette(c("red","black", "blue"),  space = "rgb") #Lab)
  colSet = c("green",rgb(n))
  title = sprintf("%s %s-%s",title_,datum1,datum2)
  x<-selectBench(r,bench_)
  if (legend.loc_=="")
    chart.TimeSeries(x,colorset = colSet, ylab=bench_,yaxis.right=T,
                     main=title ,   ylog=ylog_ ,auto.grid=T,lwd=2,
                     event.lines = risk.dates_, event.labels = risk.labels_, event.color = "red")
  else
    chart.TimeSeries(x,legend.loc=legend.loc_,colorset = colSet,yaxis.right=F,
                     main=title ,  ylob=NULL, ylog=ylog_ ,auto.grid=T,lwd=2,
                     event.lines = risk.dates_, event.labels = risk.labels_, event.color = "red")
  
  labs=colnames(r)
  axis(2, coredata(last(r)),labels=FALSE)
  mtext(labs, 2, at=coredata(last(r)), col=1,las=1,line=1,cex=0.7)
  for (i in seq(par("yaxp")[1],par("yaxp")[2],by=(par("yaxp")[2]-par("yaxp")[1])/par("yaxp")[3])) {
    abline(h=i, col="gray70")
  }
}


########################################################################################
# hole die Bench-spalte des Datenblocks r nach vorn (damit sie farbig hervorgehobn werden kann)   
########################################################################################

selectBench<-function(r, bench=BENCH)
{
  
  r = na.omit(r)
  
  if (len(bench)< 1 || ncol(r)< 2) return(r)
  benchid = which(colnames(r)==bench)
  
  if (len(benchid) < 1) return (r)
  
  benchid = benchid[1]
  x = r[,benchid]
  res=cbind(x,r[, -benchid])
  res = na.omit(res)
  return( res)  
}


new_Win<-function(x=1)
{
  windows(record=TRUE);
  norm_Win(x)
}

norm_Win<-function(x=1)
{
  if (x>0)
  {
    if (x==4)
      par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
    else
      par(mfrow=c(x,1), oma = c(0, 0, 2, 0))
  }
}

blockInfo<-function(r = data$prices)
{
  sprintf("%s \r von %s bis %s ",toString(colnames(r)), head(index(r),1),tail(index(r),1))
}
############################################################################################################
# lies aus der MSSQL-Preis-Datenbank und mach xts - objecte daraus 
#

############################################################################################################

dbGetSeries<-function(name_, from = "2005-01-01",enddate = Sys.Date(), env=.GlobalEnv,dbServer=Sys.info()["nodename"])
{
  ti=NULL
  return (ti) #20121022: no SQL-Server installed any more
  
  start<-format(as.Date(from),"%Y%m%d")
  end<- format(as.Date(enddate),"%Y%m%d") # yyyy-mm-dd
  print(end)
  if (!is.list(name_))
    name_ <-c(name_)
  #db<-odbcDriverConnect("driver=SQL Server;server=ARTHUR\\SQLEXPRESS;Initial Catalog=Paris" )
  db<-odbcDriverConnect(paste("driver=SQL Server;server=",dbServer,"\\SQLEXPRESS;Initial Catalog=Paris; User Id=miksa;Password=milka" ,sep=""))
  
  for (name in name_)
  {
    print (name)
    Tickers <<- c( with(.GlobalEnv,Tickers),name) #h?nge den namen am globaler Tickers-Liste an
    request<-paste("SELECT Date_ AS Date, [Open], High, Low, [Close],  Volume, Price FROM  [Paris].[dbo].[PricesDB] WHERE     (Name = '",name, "'", "and Date_ > ",start, " and Date_ < ",end," )ORDER BY Date ASC" ,sep="")
    try(mts<-sqlQuery(db,request) )
    if (!is.null(mts) && dim(mts)[1] > 0)
    {
      # bd<- BusinessDays(enddate,from)
      #  row.names(mts)<-bd
      
      row.names(mts) <- as.Date(as.character(mts$Date),"%Y%m%d")
      mts$Open<-as.numeric(mts$Open)
      mts$High<-as.numeric(mts$High)
      mts$Low<-as.numeric(mts$Low)
      mts$Close<-as.numeric(mts$Close)
      mts$Price<-as.numeric(mts$Price)
      mts$Volume<-as.numeric(mts$Volume)
      colnames(mts)<-c("Date",paste(name,"Open",sep="."),paste(name,"High",sep="."),paste(name,"Low",sep="."),paste(name,"Close",sep="."),paste(name,"Volume",sep="."),paste(name,"Adjusted",sep=".")  )
      mat<-as.matrix(mts[,-1]) 
      xmat<-as.xts(mat)
      
      if   (startsWith(name,"^"))
        name <- sub( "^", "",name,fixed=TRUE)  
      # eval(parse(text=paste(ticker,qxts,sep="<-")))
      
      assign(name, xmat, envir =env)
      assign(paste(name,"_",sep=""), mts,envir=.GlobalEnv) #data.frame(qxts)
      if (is.null(xmat))
        ti=NULL else ti = get(name, envir=env)
    }
    else 
    {
      #assign(name,NULL,envir=env)
      ti=NULL #MM20120330
    }
  }
  close(db)
  return (ti)
}

############################################################################################################
#Vergleicht zwei Zeitreihen mittesl Correlation zwischen ihnen
############################################################################################################

mbox<-function(a,b)
{
  x<-NULL
  
  mb = na.omit(merge(a,b))
  colnames(mb)=c(colnames(a),colnames(b))
  return (x<-as.xts(mb )) 
}

mbox2<-function(li)
{
  x<-NULL
  
  mb = na.omit(merge(a,b))
  colnames(mb)=c(colnames(a),colnames(b))
  return (x<-as.xts(mb )) 
}

# as.symbol(Dax)
# deparse(quote(Dax))
# typeof(Dax)
# as.character(Dax)
# Dax$name
# xx=mReturn(mbox(Cl(Dax),Cl(CRB)))
# chart.RiskReturnScatter(xx)
# chart.CumReturns(xx,legend.loc="bottom")

mBox<-function(L)
{
  
  x<-NULL
  a= L[[1]]
  a=apply(L[-1],FUN= function(b) na.omit(merge(a,b)))
  return (a)
  return (x<-as.xts(na.omit(merge(a,b)) )) 
}



#l <- mapply(FUN=load.ets.trades, date=dates, filename=filenames)

chartCompare2  <-function(dax,rex=NULL, corrWin=100)
{
  Yen10y<- NULL
  
  Yen10y =na.omit(merge(dax,rex))
  
  chart.Correlation(Yen10y)
  
  
  #define colors
  #use derivative of indianred4 with alpha for Yen
  rgbnum <- col2rgb("indianred4")
  col.yen <- rgb(rgbnum[1],rgbnum[2],rgbnum[3],alpha=180,maxColorValue=255)
  #use derivative of steelblue4 with alpha for US 10y
  rgbnum <- col2rgb("steelblue4")
  col.10y <- rgb(rgbnum[1],rgbnum[2],rgbnum[3],alpha=180,maxColorValue=255)
  
  #2 rows and 1 column of graphs
  par(mfrow=c(2,1))
  par(oma=c(0,1,0,0))
  opar <- par(mai = c(0, 0.8, 0.5, 0.8))
  #plot the Japanese Yen
  plot.zoo(Yen10y[,1], type="l",
           xaxt="n", xlab=NA, ylab="Yen (US$/JPY)",
           col=col.yen, col.lab=col.yen, col.axis=col.yen,
           lwd=3,bty="n",fg = "gray70")
  #do grid for y
  for (i in seq(par("yaxp")[1],par("yaxp")[2],by=(par("yaxp")[2]-par("yaxp")[1])/par("yaxp")[3])) {
    abline(h=i, col="gray70")
  }
  title(main=colnames(Yen10y),adj=0,outer=TRUE,line=-2)
  
  par(new=TRUE)
  plot.zoo(Yen10y[,2], type="l",
           xaxt="n", yaxt="n", xlab=NA, ylab=NA,
           col=col.10y, col.lab=col.10y,
           lwd=3,bty="n",fg = "gray70")
  axis(side=4,col.axis=col.10y,fg="gray70")
  usr <- par("usr")
  text(usr[2] + .12 * diff(usr[1:2]), mean(usr[3:4]), "US 10y Yield %",
       srt = 90, xpd = TRUE, col = "steelblue4")
  par(opar)
  opar <- par(mai = c(0.8,0.8,0.2,0.8))
  
  #plot running correlation between yen and US 10y
  plot.zoo(runCor(Yen10y[,1],Yen10y[,2],n=corrWin),
           xlab=NA, ylab = NA,
           lwd=3,bty="n",
           col.axis="gray30",col.lab="gray30",col="gray30",fg="gray30")
  title(main=paste("Rolling",toString(corrWin)," days Correlation",sep=""), cex.main=0.9, adj=0, col.main="gray30")
  for (i in seq(-1,1,by=1)) {
    abline(h=i, col="gray70")
  }
  axis(side=4,labels=FALSE,fg="gray70")
  par(opar)
  
  
}
###############################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################
# Collection of General Utilities
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################

compute.annual.factor = function(x) {
  possible.values = c(252,52,26,13,12,6,4,3,2,1)
  index = which.min(abs( nrow(x) / compute.nyears(x) - possible.values ))
  round( possible.values[index] )
}

###############################################################################
# Convenience Utilities
###############################################################################


###############################################################################
# Join vector of strings into one string using delim
###############################################################################
join <- function
(
  v, 			# vector of strings
  delim = ''	# delimiter
)
{ 
  return(paste(v,collapse=delim)); 
}

###############################################################################
# Remnove any leading and trailing spaces
###############################################################################
trim <- function
(
  s	# string
)
{
  s = sub(pattern = '^ +', replacement = '', x = s)
  s = sub(pattern = ' +$', replacement = '', x = s)
  return(s)
}
trim_ <- function
(
  s  # string
)
{
  s = sub(pattern = '^_+', replacement = '', x = s)
  s = sub(pattern = '_+$', replacement = '', x = s)
  return(s)
}

###############################################################################
# Get the length of vectors
############################################################################### 
len <- function
(
  x	# vector
)
{
  if(is.null(x) ) return(0)
  return(length(x)) 
}

###############################################################################
# Fast version of ifelse
############################################################################### 
iif <- function
(
  cond,		# condition
  truepart,	# true part
  falsepart	# false part
)
{
  if (len(cond)==0)
    return(falsepart)
  if(len(cond) == 1) { if(cond) truepart else falsepart }
  else {  
    if(length(falsepart) == 1) {
      temp = falsepart
      falsepart = cond
      falsepart[] = temp
    }
    
    if(length(truepart) == 1) 
      falsepart[cond] = truepart 
    else 
      falsepart[cond] = truepart[cond]
    
    #falsepart[!is.na(cond)] = temp
    
    return(falsepart);
  }
} 

###############################################################################
# Check for NA, NaN, Inf
############################################################################### 
ifna <- function
(
  x,	# check x for NA, NaN, Inf
  y	# if found replace with y
) { 	
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

###############################################################################
# Count number of non NA elements
############################################################################### 
count <- function(
  x,			# matrix with data
  side = 2	# margin along which to count
)
{
  if( is.null(dim(x)) ) { 
    sum( !is.na(x) ) 
  } else { 
    apply(!is.na(x), side, sum) 
  }
}  

###############################################################################
# Running over window Count of non NA elements
############################################################################### 
run.count <- function
(
  x, 			# vector with data
  window.len	# window length
)
{ 
  n    = length(x) 
  xcount = cumsum( !is.na(x) )
  ycount = xcount[-c(1 : (k-1))] - c(0, xcount[-c((n-k+1) : n)])
  return( c( xcount[1:(k-1)], ycount))
}

###############################################################################
# Day of Week
############################################################################### 
date.dayofweek <- function(dates) 
{	
  return(as.double(format(dates, '%w')))
}

date.day <- function(dates) 
{	
  return(as.double(format(dates, '%d')))
}

date.week <- function(dates) 
{	
  return(as.double(format(dates, '%U')))
}

date.month <- function(dates) 
{	
  return(as.double(format(dates, '%m')))
}

date.year <- function(dates) 
{	
  return(as.double(format(dates, '%Y')))
}


date.week.ends <- function(dates) 
{	
  return( unique(c(which(diff( 100*date.year(dates) + date.week(dates) ) != 0), len(dates))) )
}

date.month.ends <- function(dates) 
{	
  return( unique(c(which(diff( 100*date.year(dates) + date.month(dates) ) != 0), len(dates))) )
}

Dates.month.ends <- function(dates) 
{  
  library(timeDate)  
  #tS = timeSequence(from = "2008-01-01", to = "2008-12-31", by = "month")
  # Do you want the last Day or the last Friday in Month Data ?
  as.Date(unique(timeLastDayInMonth(dates)))
  
}
Dates.week.ends <- function(dates) 
{  
  library(timeDate)  
  as.Date(unique(dates[isWeekday(dates, wday = 5)]))  #5 heisst freitags
  
}

Dates.quarter.ends <- function(dates) 
{  
  library(timeDate)  
  #tS = timeSequence(from = "2008-01-01", to = "2008-12-31", by = "month")
  # Do you want the last Day or the last Friday in Month Data ?
  as.Date(unique(timeLastDayInQuarter(dates)))
  
}
Dates.year.ends <- function(dates) 
{ 
  # browser()
  dates=dates+1
  dates[which(diff( date.year(as.Date(dates)) ) != 0)]
}


date.year.ends <- function(dates) 
{	
  return( unique(c(which(diff( date.year(dates) ) != 0), len(dates))) )
}

# map any time series to monthly
map2monthly <- function(equity) 
{
  #a = coredata(Cl(to.monthly(equal.weight$equity)))
  
  if(compute.annual.factor(equity) >= 12) return(equity)
  
  dates = index(equity)
  equity = coredata(equity)
  
  temp = as.Date(c('', 10000*date.year(dates) + 100*date.month(dates) + 1), '%Y%m%d')[-1]
  new.dates = seq(temp[1], last(temp), by = 'month')		
  
  map = match( 100*date.year(dates) + date.month(dates), 100*date.year(new.dates) + date.month(new.dates) ) 
  temp = rep(NA, len(new.dates))
  temp[map] = equity
  
  #range(a - temp)
  
  return( make.xts( m.ifna.prev(temp), new.dates) )
}

# create monthly table
create.monthly.table <- function(monthly.data) 
{
  nperiods = nrow(monthly.data)
  
  years = date.year(index(monthly.data[c(1,nperiods)]))
  years = years[1] : years[2]
  
  # create monthly matrix
  temp = matrix( double(), len(years), 12)
  rownames(temp) = years
  colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec')
  
  # align months
  index = date.month(index(monthly.data[c(1,nperiods)]))
  temp[] = matrix( c( rep(NA, index[1]-1), monthly.data, rep(NA, 12-index[2]) ), ncol=12, byrow = T)
  
  return(temp)
}


# http://www.mysmp.com/options/options-expiration-week.html
# The week beginning on Monday prior to the Saturday of options expiration is referred to as options expiration week. 
# Since the markets are closed on Saturday, the third Friday of each month represents options expiration.
# If the third Friday of the month is a holiday, all trading dates are moved forward; meaning that Thursday will be the last trading day to exercise options.
# http://www.cboe.com/TradTool/ExpirationCalendar.aspx

# The expiration date of stock options (3rd Friday of the month)
# http://bytes.com/topic/python/answers/161147-find-day-week-month-year
third.friday.month <- function(year, month)
{
  day = date.dayofweek( as.Date(c('', 10000*year + 100*month + 1), '%Y%m%d')[-1] )
  day = c(20,19,18,17,16,15,21)[1 + day]
  return(as.Date(c('', 10000*year + 100*month + day), '%Y%m%d')[-1])
}




###############################################################################
# Timing Utilities
###############################################################################
# Begin timing
###############################################################################
tic <- function
(
  identifier =1	# integer value
)
{
  assign(paste('saved.time', identifier, sep=''), proc.time()[3], envir = .GlobalEnv)
}

###############################################################################
# End timing
###############################################################################
toc <- function
(
  identifier=1	# integer value
)
{
  if( exists(paste('saved.time', identifier, sep=''), envir = .GlobalEnv) ) {
    prevTime = get(paste('saved.time', identifier, sep=''), envir = .GlobalEnv)
    diffTimeSecs = proc.time()[3] - prevTime
    cat('Elapsed time is', round(diffTimeSecs, 2), 'seconds\n')
  } else {
    cat('Toc error\n')
  }    
  return (paste('Elapsed time is', round(diffTimeSecs,2), 'seconds', sep=' '))
}

###############################################################################
# Test for timing functions
###############################################################################
test.tic.toc <- function()
{
  tic(10)
  for( i in 1 : 100 ) {
    temp = runif(100)
  }
  toc(10)
}


###############################################################################
# Matrix Utilities
###############################################################################
# Lag matrix or vector
#  mlag(x,1) - use yesterday's values
#  mlag(x,-1) - use tomorrow's values
###############################################################################
mmlag <- function
(
  m,			# matrix or vector
  nlag = 1	# number of lags
)
{ 
  # vector
  if( is.null(dim(m)) ) { 
    n = len(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    } 	
    
    # matrix	
  } else {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    } 
  }
  return(m);
}

###############################################################################
# Replicate and tile an array
# http://www.mathworks.com/help/techdoc/ref/repmat.html
###############################################################################
repmat <- function
(
  a,	# array
  n,	# number of copies along rows
  m	# number of copies along columns
)
{
  kronecker( matrix(1, n, m), a )
}

###############################################################################
# Compute correlations
###############################################################################
compute.cor <- function
(
  data, 		# matrix with data
  method = c("pearson", "kendall", "spearman")
)
{
  nr = nrow(data) 
  nc = ncol(data) 
  
  corm = matrix(NA,nc,nc)
  colnames(corm) = rownames(corm) = colnames(data)
  
  for( i in 1:(nc-1) ) {
    temp = data[,i]
    for( j in (i+1):nc ) {
      corm[i,j] = cor(temp, data[,j], use='complete.obs', method[1])	
    }
  }
  return(corm)
}

###############################################################################
# XTS helper functions
###############################################################################
##############################################################################

###############################################################################
# Write XTS object to file
###############################################################################
write.xts.old <- function
(
  x,			# XTS object
  filename	# file name
)
{
  dir.create(dirname(filename),recursive=T)
  write.csv(x, row.names = index(x), filename)	
}
write.xts <- function
(
  x,
  filename,
  append = FALSE,
  ...
)
{
  cat('Date', file = filename, append = append)
  dir.create(dirname(filename),recursive=T)
  
  write.table(x, sep=';',  row.names = format(index(x), ...),
              col.names = NA, file = filename, append = T, quote = F)
}
read.xts <- function
(
  x,
  date.fn = paste,
  index.class = 'Date',
  decreasing = FALSE,
  ...
)
{
  if (is.matrix(x) || is.data.frame(x) ) {
    data = x
    dates = as.matrix(data[,1,drop=F])
    data  = data[,-1,drop=F]
  } else {
    filename = x
    load.packages('data.table')
    out = fread(filename, stringsAsFactors=F,sep=";")
    setnames(out,gsub(' ', '_', trim(colnames(out))))
    rest.columns.expr = parse(text = paste('list(', paste(colnames(out)[-1],collapse=','),')'))
    dates = as.matrix(out[,1,with=FALSE])
    data = out[, eval(rest.columns.expr)]
  }
  dates = as.POSIXct(match.fun(date.fn)(dates), tz = Sys.getenv('TZ'), ...)
  dates.index = order(dates, decreasing = decreasing)
  out = make.xts(data[dates.index,,drop=F], dates[dates.index])
  indexClass(out) = index.class
  return( out )
}

###############################################################################
# Work with file names
###############################################################################
get.extension <- function(x) 
{ 
  trim( tail(spl(x,'\\.'),1) ) 
}	

get.full.filename <- function(x) 
{ 
  trim( tail(spl(gsub('\\\\','/',x),'/'),1) ) 
}



get.filename <- function(x) 
{ 
  temp = spl(get.full.filename(x),'\\.')
  join(temp[-len(temp)])
  
  
  #############################################################
  #Text-Ausgabe wie bei sprintf() in c
  #############################################################
  
  mP<-  function(...)
  {
    print(sprintf(...))
  }
  
  # Function to compute percentage returns
  pchange <- function(x) { exp(diff(log(x)))-1 }
  
  # The weighted mean function
  wmean <- function(x) { n <- length(x); w <- seq(n); return(sum(w*x)/sum(w)) }
  
  
  ######################################################################
  # Beispspiel f?r DeOptim und auch den 3-dPlot
  #siehe auch o:\R\Nuggets\Optimizing\Optimization.pdf
  #######################################################################
  
  
  chart.DownsideTable<-function(returnComparison)
  {
    
    #  returnComparison<-merge(ret,RetToAnalyze)
    #  colnames(returnComparison)<-c(colnames(ret),colnames(RetToAnalyze))
    #  charts.PerformanceSummary(returnComparison, main="compare",
    #                            colorset = redfocus)
    
    
    downsideTable<-melt(cbind(rownames(table.DownsideRisk(returnComparison)),table.DownsideRisk(returnComparison)))
    colnames(downsideTable)<-c("Statistic","Portfolio","Value")
    ggplot(downsideTable, stat="identity", aes(x=Statistic,y=Value,fill=Portfolio)) + geom_bar(position="dodge") + coord_flip()
    downsideTable
  }
  ######################################################################
  # Beispspiel f?r DeOptim und auch den 3-dPlot
  #siehe auch o:\R\Nuggets\Optimizing\Optimization.pdf
  #######################################################################
  rastrigin <-function(x1,x2) ## schr?ge Funktion mit vielen Optima
  {
    phi=3.14159
    x1=as.double(x1)
    x2 = as.double(x2)
    ret = as.double(20+x1*x1+x2*x2-10*(cos(2*phi*x1)+cos(2*phi*x2)))
    return (ret) 
  }
  
  #### erzeuge ein fiese MultiH?gel--Testoberfl?che um den Optimiere zu testen
  ##### 3dPlot
  if (FALSE)
  { 
    rastriginDemo <-function()
    {
      mmax=-1000  
      xmax=0
      ymax=0
      
      X=seq(-5,5,by=0.1)
      Y=seq(-5,5,by=0.1)
      
      m=matrix(nrow=length(X),ncol=length(Y))
      for (x in X )
        for (y in Y)
        {
          m#P("x = %f, y= %f",x,y)    
          m[y*10+51,x*10+51] = (v=rastrigin(x,y))
          if (v >= mmax)
          {
            xmax=x;ymax=y; mmax = v
            #mP("max %f at %f %f ",mmax,xmax,ymax)
          }
        }
      
      mP("max %f at %f %f ",mmax,xmax,ymax)
      ######### 3d Plot ######
      library(rgl)
      persp3d(x=X,y=Y, z=m, box=FALSE, col=m)
      
      return (m)
    }
    
    m=rastriginDemo()
    rastrigin(-4.5,-4.5)
    rastrigin(-5,-5)
    
    
    #DEoptim   MINIMIERT  Funktionen deren Parameter als vector ?bereben wurden
    Rastrigin<-function(xvec)
    {
      x1 = xvec[1]
      x2 = xvec[2]
      res = ( 1 / (1+rastrigin(x1,x2)))
      return (res)
    }
    
    Rastrigin(c(-4.5,-4.5))
    Rastrigin(c(4.522998 ,-4.522998)) 
    library(DEoptim)
    args(DEoptim.control)
    #   function (VTR = -Inf, strategy = 2, bs = FALSE, NP = 50, itermax = 200,
    #             CR = 0.5, F = 0.8, trace = TRUE, initialpop = NULL, storepopfrom = itermax +
    #               1, storepopfreq = 1, checkWinner = FALSE, avWinner = TRUE,
    #             p = 0.2)
    #     NULL
    #   NP number of population member
    #   itermax max number of iterations (population generations)
    #   strategy dierential evolution strategy
    #   F step size for scaling dierence
    #   CR crossover probability
    #   VTR value-to-reach
    # 
    
    res<-DEoptim(fn=Rastrigin,lower=c(-5,-5),upper=c(5,5),control=list(storepopfrom=1,itermax=1000))
    res$optim
    
    t<-function(x) return (1/(x))
    res<-DEoptim(fn=t,lower=c(1),upper=c(500),control=list(storepopfrom=1,itermax=1000))
    res$optim
    
    #############################################################################
    
    #  tail(Dax)
    Prices <-dbGetSeries("Dax",from="2011-06-01")
    prices = getSymbols('SPY', src = 'yahoo', from = '2012-01-01', auto.assign = F)
    
    data <-dbGetSeries("Dax",from="1999-06-01")
    
    plot(Prices, ylim = range(OHLC(Prices)))
    plota(Prices, type = 'ohlc',  ylim = range(OHLC(Prices)))
    chartSeries(Prices,theme='white',TA='addRSI(n=5);addBBands(n=5)')
    
    first(Prices, "1 week")
    
    #bt.simple.test <- function()
    #{
    # load.packages('quantmod')
    
    # load historical prices from Yahoo Finance
    data = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)
    
    # Buy & Hold
    signal = rep(1, nrow(data))
    buy.hold = bt.simple(data, signal)
    
    # MA Cross
    sma = SMA(Cl(data),200)
    signal = ifelse(Cl(data) > sma, 1, 0)
    sma.cross = bt.simple(data, signal)
    
    # Create a chart showing the strategies perfromance in 2000:2009
    dates = '2010::2013'
    buy.hold.equity <- buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
    sma.cross.equity <- sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])
    
    chartSeries(buy.hold.equity, TA=c(addTA(sma.cross.equity, on=1, col='red')),	
                theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) )	
    #}
    
    
    #con = gzcon(file('o:/R/Nuggets/frameworks/SystematicInvestorToolbox/bt_test.r', 'rb'))
    #source(con) #MMcheck
    
    #close(con)
    
    bt.empty.test()
    bt.matching.dtw.test()
  }
}

######################################################################
# Beispspiel f?r DeOptim und auch den 3-dPlot
#siehe auch o:\R\Nuggets\Optimizing\Optimization.pdf
#######################################################################

yearList<-function(from_,to_,by_=1)
{
  if (from_ == to_)
    return(from_)
  ls=NULL
  for(i in seq(from=from_,to=to_, by=by_))
    if (is.null(ls))
      ls=i else ls = sprintf("%s,%s",ls,i)
  return(spl(ls))
}
########################################################
#Bloomberg

getBloombergData <- function(ticker, start.dt) {
  bbg.fields <- c("px_last" )#etc.)
  library(Rbbg)
  conn <- blpConnect()
  bbg <- bdh(conn, ticker, bbg.fields, start.dt)
  blpDisconnect(conn)
  bbg.res <- reshape(bbg, direction = "wide", timevar = "ticker", idvar = "date")
  return(bbg.res)
}


#hList =hash()

########################################################################################
print("########### load InputConfig_Portfolio_TD1.R")
