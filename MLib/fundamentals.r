#siehe auch Quandl  und advfn
# siehe auc fm.fund.factor.test()
#http://stackoverflow.com/questions/10177435/get-annual-financial-data-for-a-stock-for-many-years-in-r

if (F)
{
# library(qmao);# needs pander  ... web-page liefert nicht länger
#  getEarnings("AMD")
library(quantmod)

getFinancials(Symbol="AMD", src="google")
#to get the names of the matrix: rownames(AMD.f$IS$A)
Total.Revenue<-AMD.f$IS$A["Revenue",]
Gross.Profit<-AMD.f$IS$A["Gross Profit",]

#finally:
reg1<-lm(Gross.Profit~Total.Revenue) 
get.Isin("CON.DE")
getFinancials(Symbol="CON.F", src="yahoo")
Total.Revenue<-BMW.f$IS$A["Revenue",]
Gross.Profit<-BMW.f$IS$A["Gross Profit",]


getFin('AMD')
x <- viewFin(AMD.f, "BS", period="Q")["Total Common Shares Outstanding",]*1000
getFin('CON.DE')

getFin('SAP.DE')
x <- viewFin(AMD.f, "BS", period="Q")["Total Common Shares Outstanding",]*1000

getFin('F')
x <- viewFin(F.f, "BS", period="Q")["Total Common Shares Outstanding",]*1000
zoox = zoo(x, order.by=as.Date(names(x)))
x2 <- na.spline(merge(zoox, foo=zoo(NA, order.by=seq(start(zoox), end(zoox), "month")))[, 1])
}

###############################################################################
# DCF - Discounted Cash Flow
# http://www.independent-stock-investing.com/Discounted-Cash-Flow.html
# http://www.oldschoolvalue.com/blog/stock-analysis/apple-aapl-valuation/
# www.focusinvestor.com/DiscountedCashFlows.xls
# http://en.wikipedia.org/wiki/Discounted_cash_flow
###############################################################################
fundamental.dcf <- function(data,vsymbol="AAPL",exchange="",p.dir="plots/fundamentals/") 
{
  #*****************************************************************
  # Load historical fundamental and pricing data
  #****************************************************************** 
  load.packages('quantmod') 
  data$fund=list()
  tickers = vsymbol# spl('AAPL')
  symbol=vsymbol
  symbol.left=unlist(strsplit(symbol,"\\."))[1]; symbol=ifelse(len(symbol.left)>0,symbol.left,symbol)
  
  if (exchange == "")
    tickers.temp = paste(iif( nchar(symbol) <= 3, 'NYSE:', 'NASDAQ:'), symbol, sep='')
  else
    tickers.temp = sprintf("%s:%s", exchange,symbol)
  
    #  data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 80, 'annual')#quarterly
  data$fund[[tickers]] = fund.data(tickers.temp, 200, 'quarterly')#quarterly

  if (len(data$fund[[tickers]])==0)
    return(NULL)
  
  # get pricing data
  #data <- new.env()
  #getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
  #data[[tickers]] = adjustOHLC(data[[tickers]], use.Adjusted=T)			
  
  
  fund = data$fund[[tickers]]
  fund.date = date.fund.data(fund)
  data[[tickers]]
  price = Cl(data[[tickers]]['1995::'])
}

#################################################################################

calculate.fundamental.dcf <- function(data,p.dir="plots/fundamentals/") 
{
  ########################################################################################
  #*****************************************************************
  # Combine fundamental and pricing data
  #****************************************************************** 				
  for(i in tickers) {
    fund = data$fund[[i]]
    fund.date = date.fund.data(fund)
    
    # Earnings per Share		
    EPS = 4 * get.fund.data('Diluted EPS from Total Operations', fund, fund.date)
    if(nrow(EPS) > 3)
      EPS = rbind(EPS[1:3], get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)[-c(1:3)])
    
    # merge	
    data[[i]] = merge(data[[i]], EPS)
  }
  
  bt.prep(data, align='keep.all', dates='1995::')
  
  
  #*****************************************************************
  # Create PE
  #****************************************************************** 
  prices = data$prices
  prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
  
  EPS =  bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
  
  PE = ifna(prices / EPS, NA)
  PE[ abs(EPS) < 0.001 ] = NA
  
  colnames(EPS)=c("EPS")
  colnames(PE)=c("PE")
  
  #########################################################################################
  #*****************************************************************
  # Extract Inputs for DCF Valuation
  #****************************************************************** 				
  # Free Cash Flows
  FCF = get.fund.data('free cash flow', fund, fund.date)
  
  # Invested Capital
  IC = get.fund.data('invested capital', fund, fund.date)
  
  # Sales
  SALE = get.fund.data('total revenue', fund, fund.date)
  
  # Common Equity
  CEQ = get.fund.data('total equity', fund, fund.date)
  
  # Common Shares Outstanding
  CSHO = get.fund.data('total common shares out', fund, fund.date)
  
  # Growth Rate
  CROIC = FCF/IC
  
  # Average inputs
  g = runMean(CROIC, 5)
  cash = runMean(FCF, 5)
  
  
  
  #*****************************************************************
  # Helper function to compute Intrinsic Value
  #****************************************************************** 				
  compute.DCF.IV <- function(cash, eqity, shares, g, R) {
    if( cash <= 0 ) return(NA)
    
    if( len(R) == 1 ) R = rep(R, len(g))
    
    value = eqity + sum(cash * cumprod(1 + g) / cumprod(1 + R))
    return( value / shares )
  }
  
  
  #*****************************************************************
  # Compute Intrinsic Value, assumptions:
  # Company will grow for the first 3 years at current Growth Rate
  # slowed down by 20% for the next 4 years, and slowed down by a further 20% for the next 3 years
  # and finally 3% growth for the next 10 years
  #
  # The Discount Rate is 9%
  #
  # http://www.oldschoolvalue.com/blog/stock-analysis/apple-aapl-valuation/
  #****************************************************************** 				
  dcf.price = NA * g
  i.start = which(!is.na(g))[1] 
  
  for(i in i.start : nrow(g)) {
    # Create Growth Rate scenario: 		
    g.scenario = c(rep(g[i],3), rep(g[i],4)*0.8, rep(g[i],3)*0.8*0.8, rep(3/100,10))
    
    # Compute Intrinsic Value
    dcf.price[i] = compute.DCF.IV(cash[i], CEQ[i], CSHO[i], g.scenario, 9/100)
  }
  
  #*****************************************************************
  # Create Plot
  #****************************************************************** 
  
  #p.dir= sprintf("plots/fundamentals/")
  png(filename = sprintf("%s%s_DCF.png",p.dir,symbol), width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
  plota(price, type='l', log = 'y', col='blue', main=tickers[1],
        ylim=range(price,dcf.price,na.rm=T))
  plota.lines(dcf.price, type='s', col='red', lwd=2)
  plota.legend('Close,Intrinsic Value', 'blue,red', list(price, dcf.price))	
  dev.off()	
  
  
  png(filename = sprintf("%s%s_GrowthRate.png",p.dir,symbol), width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
  plota(g, type='b', col='blue', pch=0, main=sprintf('%s Growth Rate',symbol))	
  dev.off()	
  
  png(filename = sprintf("%s%s_freeCashFlow.png",p.dir,symbol), width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    	
  plota(cash, type='b', col='blue', pch=0, main=sprintf('%s Free Cash Flows',symbol))	
  dev.off()			
  
  #csv=merge(price[as.Date(index(cash))],dcf.price,g,cash)
  
  csv=merge(price,dcf.price,g,cash,FCF,IC,SALE,CEQ,CSHO,CROIC,PE,EPS)
  csv=bt.apply.matrix(csv, m.ifna.prev)
  
  return(csv)  
}

if (F)
{
  data.fund <- new.env()
  symbol="AAPL"
  exchange=""
  csv.data=fundamental.dcf(data.fund,symbol,exchange)
  write.csv(csv.data,file=sprintf("%s%s_dcfGc.csv",p.dir,symbol),sep=";",row.names=format(index(csv.data)))
  ls(data.fund)
  data.fund$AAPL
  
  ##### nun für viele
  #tickers = spl('FB,LNKD,GRPN,AAPL,GOOG')
  #tickers.temp = spl('NASDAQ:FB,NYSE:LNKD,NASDAQ:GRPN,NASDAQ:AAPL,NASDAQ:GOOG')
 
  
  stoxx50e.composites = getIndexComposition("^STOXX50E")
  dax.composites = getIndexComposition("^GDAXI")
  
  stoxx50e.composites[2]
  
  p.dir= sprintf("plots/fundamentals/")
 
  if (F)
  {
  pkgs <- c('pnn', 'doParallel','foreach', 'grnn')
  lapply(pkgs, require, character.only = T)
  registerDoParallel(cores = 8)
  }
  #iter(2:20)
  
 #<-------------------------------------------------------------------- 
  data.fund <- new.env()
  
  foreach(i=seq(2,len(dax.composites),1), .combine=c) %do% {
    symbol=dax.composites[i]# "AAPL";
    symbol.left=unlist(strsplit(symbol,"\\."))[1]; symbol=ifelse(len(symbol.left)>0,symbol.left,symbol)
    
    #symbol="VOW3"
    mP("download fundamentals for %s",symbol)
    exchange="LS" #FWB  XE
    csv.data=NULL
    csv.data=fundamental.dcf(data.fund,symbol,exchange)
    if (len(csv.data) < 1)
      {
      exchange="FWB"
      csv.data=fundamental.dcf(data.fund,symbol,exchange)
      }
    if (len(csv.data)>0)
    write.csv(csv.data,file=sprintf("%s%s_dcfGc.csv",p.dir,symbol),sep=";",row.names=format(index(csv.data)))
  }
  
  
  fundamental.fb.test()
}

if (F)
{
quandlCodes=spl("BCB/UDJIAD1",
                   )

mGetTickers(list(c("BCB/UDJIAD1","Quandl")),data=data,online=T)
mGetTickers(list(c("NSE/OIL","Quandl")),data=data,online=T)
mGetTickers(list(c("GOOG/NYSE_SAP","Quandl")),data=data,online=T)
mGetTickers(list(c("DEUPROINDMISMEI","FRED")),data=data,online=T)


download.file(url="http://chart.yahoo.com/table.csv?s=DIA&a=0&b=01&c=1995&d=10&e=11&f=2013&g=d&q=q&y=0&z=DIA&x=.csv", destfile="d:\\test.csv",method="internal")

read.csv('http://www.quandl.com/api/v1/datasets/DOE/EIA_TOTALOILSUPPLY_A_GERMANY73.csv?&auth_token=98xyEn88QWsfqq7aspHU&trim_start=2007-12-31&trim_end=2011-12-31&sort_order=desc', colClasses=c('Year'='Date'))

read.csv('http://chart.yahoo.com/table.csv?s=DIA&a=0&b=01&c=1995&d=10&e=11&f=2013&g=d&q=q&y=0&z=DIA&x=.csv', colClasses=c('Year'='Date'))
}
###########################################################################################
###########################################################################################
if (F)
{
#  Quandl läuft wie Yahoo und Fred über mein mGetTickers
 
#########################
#lade Eurostat-daten
#http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing
#http://epp.eurostat.ec.europa.eu/portal/page/portal/statistics/search_database
install.packages("SmarterPoland")
library(SmarterPoland)
# info about passagers
grepEurostatTOC("split of passenger transport")
## get table
tmp <- getEurostatRCV("tsdtr210")
x=zinsen.tages=getEurostatRCV("t2020_10")
head(x,50)
str(x)
x$time
zinsen.tages=getEurostatRaw("t2020_10")
head(getEurostatRCV(kod = "educ_iste"))

summary(tmp)


#beschäftigung   t2020_10

#vertauensindikatoren
#Tables, Graphs and Maps Nutzerschnittstelle    Indikator der wirtschaftlichen Einschätzung (teibs010)    Informationen zum Blatt

#Tables, Graphs and Maps Nutzerschnittstelle    Indikator des Vertrauens in der Industrie nach Sektor (teibs020) 	 Informationen zum Blatt

IndikatorderwirtschaftlichenEinschätzung =getEurostatRCV("teibs010")
plot(IndikatorderwirtschaftlichenEinschätzung[,c("time","value")],main="IndikatorderwirtschaftlichenEinschätzung")  

IndikatordesVertrauensinderIndustrienachSektor=getEurostatRCV("teibs020")
plot(IndikatordesVertrauensinderIndustrienachSektor[,c("time","value")],main="IndikatordesVertrauensinderIndustrienachSektor")  

AktuelleKapazitätsauslastung=getEurostatRCV("teibs070")
plot(AktuelleKapazitätsauslastung[,c("time","value")],main="AktuelleKapazitätsauslastung")  

Erwerbstätigenquote= getEurostatRCV("t2020_10")
plot(Erwerbstätigenquote[,c("time","value")],main="Erwerbstätigenquote")  

Arbeitslosenquote= getEurostatRCV("tipsun10")
plot(Arbeitslosenquote[,c("time","value")],main="Arbeitslosenquote")  


#Wachstumsrate des realen BIP
WachstumsratedesrealenBIP = getEurostatRCV("tec00115")
plot(WachstumsratedesrealenBIP[,c("time","value")],main="Wachstumsrate des realen BIP")    #sehr spannendes bild 

Tagesgeldsatz =getEurostatRCV("teimf100")
plot(Tagesgeldsatz[,c("time","value")],main="Tagesgeldsatz")
colnames(Tagesgeldsatz)
Tagesgeldsatz[Tagesgeldsatz[,"geo"]=="UK",]


DreiMonateZinssätze= getEurostatRCV("teimf040") 
plot(DreiMonateZinssätze[,c("time","value")],main="DreiMonateZinssätze")

GeldvolumenM1= getEurostatRCV("teimf010") 
plot(GeldvolumenM1[,c("time","value")],main="GeldvolumenM1")
Geldvolumen2= getEurostatRCV("teimf020") 
plot(GeldvolumenM2[,c("time","value")],main="GeldvolumenM2")
GeldvolumenM3= getEurostatRCV("teimf030") 
plot(GeldvolumenM3[,c("time","value")],main="GeldvolumenM3")

EuroRenditenstrukturkurvennachLaufzeit= getEurostatRCV("teimf060") 
plot(EuroRenditenstrukturkurvennachLaufzeit[,c("time","value")],main="EuroRenditenstrukturkurvennachLaufzeit")

#Langfristige Rendite öffentlicher Anleihen (teimf050)    Informationen zum Blatt
#Realer effektiver Wechselkurs - 41 Handelspartner (teimf250)
#Verbraucherpreise (teieuro_cp)
#HVPI - Gesamtindex (teicp000)    Informationen zum Blatt
#Baugenehmigungen - monatliche Daten (teiis550)

#FRED

#3-Month or 90-day Rates and Yields: Interbank Rates for Germany (IR3TIB01DEM156N)
#Wo die Schornseine rauchen

#Production of Total Industry in Germany (DEUPROINDMISMEI)
#Production of Total Industry in France (FRAPROINDMISMEI)
#Passenger Car Registrations in United States (USASACRMISMEI)
#Production of Total Industry in United States (USAPROINDQISMEI)
#Production of Total Industry in the United Kingdom (GBRPROINDMISMEI)
#Production of Total Industry in Canada (CANPROINDMISMEI)
#Production of Total Industry in Japan (JPNPROINDMISMEI)
#Production of Total Industry in Korea (KORPROINDMISMEI)
#Production of Total Industry in Switzerland (CHEPROINDQISMEI)
#Production of Total Industry in India (INDPROINDMISMEI)
#Production of Total Industry in Austria (AUTPROINDMISMEI)
#Production of Total Industry in Brazil (BRAPROINDMISMEI)
#Production of Total Industry in Portugal (PRTPROINDAISMEI)
#Benchmarked Unit Labor Costs - Construction for Sweden (SWEULCCONQPNMEI)
#Production in Total Manufacturing for Greece (GRCPROMANMISMEI)
#Permits Issued for Dwelling in Australia (AUSPERMITMISMEI)
#Production in Total Manufacturing for Japan (JPNPROMANMISMEI)
#Production of Total Industry in Russian Federation (RUSPROINDMISMEI)
#Volume of Intermediate Goods for Manufacturing for Germany (ODMNIG01DEA661N)

#ISM Index, Einkaufsmanager-Index (verarbeitendes Gewerbe)
#Purchase Manager Index, manufacturing (PMI) 




BIP = getEurostatRCV("tipsau10")
plot(BIP[,c("time","value")])  

######  mit data.table
dt = data.table(x)
colnames(dt)
setkey(dt,"geo")
x.de=dt[J("DE")]
x.de.1=x.de[,list(time,value)]  #spaltenselect ohne string und als list

plot(x.de.1)
x2=data.table(x.de[,c(time,value)])
plot(x2)

x=data.frame(x)

View(tail(x,40000))
str(x)
class(x)
x[as.Date(x$time,format="%YM%m ")>"1995M08 ",]
str(as.character(x$time))
as.factor
x[x$currency=="EUR",]
colnames(x)

}                            
if (F)
{

data.fund <- new.env()

foreach(i=seq(2,len(dax.composites),1), .combine=c) %do% {
  symbol=dax.composites[i]# "AAPL";
  symbol.left=unlist(strsplit(symbol,"\\."))[1]; symbol=ifelse(len(symbol.left)>0,symbol.left,symbol)
  
  #symbol="VOW3"
  mP("download fundamentals for %s",symbol)
  exchange="LS" #FWB  XE
  csv.data=NULL
  csv.data=fundamental.dcf(data.fund,symbol,exchange)
  if (len(csv.data) < 1)
  {
    exchange="FWB"
    csv.data=fundamental.dcf(data.fund,symbol,exchange)
  }
  if (len(csv.data)>0)
    write.csv(csv.data,file=sprintf("%s%s_dcfGc.csv",p.dir,symbol),sep=";",row.names=format(index(csv.data)))
}

}


##########################################################################
if (F)
{
  getNews2("BMW",4)

}

getNews2 <- function(symbol, number){
  
  # load libraries
  require(XML); require(plyr); require(stringr); require(lubridate);  
  
  # construct url to news feed rss and encode it correctly
  url.b1 = 'http://www.google.com/finance/company_news?q='
  url    = paste(url.b1, symbol, '&output=rss', "&start=", 1,
                 "&num=", number, sep = '')
  url    = URLencode(url)
  
  # parse xml tree, get item nodes, extract data and return data frame
  doc   = xmlTreeParse(url, useInternalNodes = T);
  nodes = getNodeSet(doc, "//item");
  mydf  = ldply(nodes, as.data.frame(xmlToList))
  
  # clean up names of data frame
  names(mydf) = str_replace_all(names(mydf), "value\\.", "")
  
  # convert pubDate to date-time object and convert time zone
  mydf$pubDate = strptime(mydf$pubDate, 
                          format = '%a, %d %b %Y %H:%M:%S', tz = 'GMT')
  mydf$pubDate = with_tz(mydf$pubDate, tz = 'America/New_york')
  
  # drop guid.text and guid..attrs
  mydf$guid.text = mydf$guid..attrs = NULL
  
  return(mydf)    
}

#################################################################################
##################################################################################

mP("########### load fundamentals.R")
