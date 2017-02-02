options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))
options(warn=1)
Vers_LoadDataFuncs = "22.2.2013"

#globaPROVIDER = ""
#source("InputConfig_Portfolio_TD.R")

last = xts::last    #wird leider von data.table ungut ?beladen!!
#############################################################################
#############################################################################
##

#loadData(spl("Dax,Rex"),xdata,frame1=myFrame,online=T)
#online = T erzwingt, ein update aus Internet-Quellen

loadData<-function(tickers=c(),data=.GlobalEnv,frame1=myFrame,online=F,universeFile="")
{
  mist<<-NULL #die Liste mit fehlenden Tickers.Logs
  if (universeFile != "")
  {
    mP("loadData:  read tickers from %s", universeFile)
    
    tickers=read.table(file=universeFile,header=TRUE,as.is=TRUE)
    tickers=as.matrix(tickers[1])
    mP(tickers)
  }
  
  if (len(tickers )< 1)
  {
    mP("loadData:  No tickers given  !! ")
    return 
  }
  
  
  #browser()
  #  undebug(mGetTickers)
  print("---loadData ---")
  mGetTickers(tickers, data=data, frame1=myFrame,online=online)
  mP(ls(data))
  
  
  #  xyplot(Rex)
  #xyplot(Dax)
  #tickers = data$symbolnames;
  #tickers = ls(data)
  #tickers = data
  
  print("\n############### load Data - #############################################################################\n")
  for(i in ls(data) )
  {
    if (is.null(data[[i]]))
    {
      
      rmSymbol(i,data)
      
    }
    else
      
    {
      print(i)
      tit= paste(toString(i), toString( dim(data[[i]])),sep=" has ")
      
      #von = time(data[[i]])[1]
      #bis=time(tail(data[[i]],1))[1]   #format(index(tail(data[[i]],1)[1]), '%d%b%y')
      # browser()
      print(tit)
      von=fromTo(data[[i]])[1]
      bis = fromTo(data[[i]])[2]
      
      mP(" ### %s    from %s til  %s",tit,von,bis)
    }
  }
  print("---------------- 1> ")
  tickers <<-ls(data)
  print(tickers)
  allRet = NULL
  for (tick_ in  tickers)
  {
    tick= get(tick_,envir=data)
    if (length(which(colnames(tick)==sprintf("%s.Close",tick_)))>0)
    {
      print(sprintf("tick is %s",tick_))
      #  tick = get(tick_,envir =data)  
      
      
      if (is.null(tick) && exists(tick_, envir=data))
      {
        mP("Noch ne null gefunden %s", tick_)
        rmSymbol(tick_,data)
        
      }
      else
      {
        if (length(which(colnames(tick)==sprintf("%s.Close",tick_)))>0)
        {
          tick= try(Cl(tick))
          if (is.null(tick) || len(tick) < 400)
          {
            if (is.null(tick))
              mp("%s hat kein Close",tick_)
            else
              mP("%s hat kein Close ",tick_)
            
            
            rmSymbol(tick_,data)
          }
          else
          {
            if (is.null(allRet) )
              allRet= tick 
            else 
              allRet = na.omit(merge(allRet,tick))
          }
        }
      }
    }}
  #print("----------2>")
  allRet_ = data.frame(allRet)
  tickers = colnames(allRet)
  #tickers
  # signalFlei=NULL
  # for (i in 1 :  length(colnames(ret)))
  # {
  #   print (colnames(ret)[i])
  #   tick = na.omit(ret[,i])
  #   
  #   tickSig = SMA(tick)
  #   if (i==1) signalFlei=tickSig else signalFlei=na.omit(merge(signalFlei,tickSig))
  # }
  # colnames(signalFlei)=colnames(ret)
  
  head(allRet,1);tail(allRet,1)
  print(toString(dim(allRet)))
  print("<<<<<----loadData")
}

#####################################################################################################
#####################################################################################################
ReturnHeatmap<-function(frame="",r=as.xts(as.matrix(diff(log(data$prices))[-1])))
{
  frame=toString(frame)
  
  r = r[frame]
  stock.returns <-r#as.matrix(diff(log(data$prices[frame]))[-1])
  
  # following code limits the lowest and highest color to 5%, and 95% of your range, respectively
  quantile.range <- quantile(stock.returns, probs = seq(0, 1, 0.01),na.rm=T)
  palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.01,na.rm=T)
  #where
  # use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
  color.palette  <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))
  #http://www.r-bloggers.com/drawing-heatmaps-in-r/
  ret = data.weekly$prices
  #Auflegedatum=format(time((data$prices[frame])[1,]), '%d%b%y')
  #Bisdatum = format(time(tail(data$prices[frame],1)[1,]), '%d%b%y')
  #browser(r)
  if (is.null(r))
  {
    mP("ReturnHeatmap r ist null")
    browser()
  }
  Auflegedatum=fromTo(r)[1]; Bisdatum = fromTo(r)[2]
  
  title=sprintf("Stock Returns %s-%s ",Auflegedatum,Bisdatum)
  try(heatmap.2(stock.returns, col=rainbow(max(10,len(palette.breaks)), end=4/6), Rowv=NA, Colv=NA,
                do.dendro=c(FALSE,FALSE), scale="none", legend=2,ylab = NULL,
                main=title, trim=.8))
}

ReturnHeatmap2<-function(r=as.xts(as.matrix(diff(log(data$prices))[-1])))
{
  stock.returns <-r#as.matrix(diff(log(data$prices[frame]))[-1])
  
  # following code limits the lowest and highest color to 5%, and 95% of your range, respectively
  quantile.range <- quantile(stock.returns, probs = seq(0, 1, 0.01),na.rm=T)
  palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.01,na.rm=T)
  
  # use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
  color.palette  <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))
  #http://www.r-bloggers.com/drawing-heatmaps-in-r/
  ret = data.weekly$prices
  #Auflegedatum=format(time((data$prices[frame])[1,]), '%d%b%y')
  #Bisdatum = format(time(tail(data$prices[frame],1)[1,]), '%d%b%y')
  Auflegedatum=fromTo(data$prices[frame])[1]; Bisdatum = fromTo(data$prices[frame])[2]
  title=sprintf("Stock Returns %s-%s ",Auflegedatum,Bisdatum)
  break()
  try(heatmap.2(stock.returns, col=rainbow(max(10,len(palette.breaks)), end=4/6), Rowv=NA, Colv=NA,
                do.dendro=c(FALSE,FALSE), scale="none", legend=2,ylab = NULL,
                main=title, trim=.8))
}

weeklyReturnHeatmap<-function(frame="",ret =data.weekly$prices)
{
  frame = toString(frame)
  mP("weeklyReturnHeatmap %s",frame)
  
  ret = ret[frame] 
  print(frame)
  stock.returns <-as.matrix(ret)
  
  # following code limits the lowest and highest color to 5%, and 95% of your range, respectively
  quantile.range <- na.omit(quantile(stock.returns, probs = seq(0, 1, 0.01),na.rm=T))
  palette.breaks <- seq(quantile.range["1%"], quantile.range["99%"], 0.01)
  
  # use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
  color.palette  <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))
  #http://www.r-bloggers.com/drawing-heatmaps-in-r/
  #  ret = data.weekly$prices
  #Auflegedatum = format(seq(index(ret)[1], length=2, by="-1 months")[2],"%B %Y")
  #Bisdatum = format(seq(index(ret)[len(ret)-1], length=2, by="-1 months")[2],"%B %Y")
  #Auflegedatum=format(time((data$prices[frame])[1,]), '%d%b%y')
  #Bisdatum = format(time(tail(data$prices[frame],1)[1,]), '%d%b%y')
  Auflegedatum=fromTo(ret)[1]; Bisdatum = fromTo(ret)[2]
  title=sprintf("Weekly Stock Returns \n %s-%s ",Auflegedatum,Bisdatum)
  try(heatmap.2(stock.returns, col=rainbow(len(palette.breaks), end=4/6), Rowv=NA, Colv=NA,
                do.dendro=c(FALSE,FALSE), scale="none", legend=2,ylab = NULL,
                main=title, trim=.8))
}
DrawdownsHeatmap<-function(frame,ret=diff(log(data$prices))[-1])
{
  frame=toString(frame)
  #ret=diff(log(data$prices[frame]))[-1]
  ret = ret[frame] 
  
  dret=Drawdowns(ret)
  stock.returns <-as.matrix(dret)  
  # following code limits the lowest and highest color to 5%, and 95% of your range, respectively
  quantile.range <- quantile(stock.returns, probs = seq(0, 1, 0.01),na.rm=T)
  palette.breaks <- seq(quantile.range["1%"], quantile.range["99%"], 0.01,na.rm=T)
  
  # use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
  color.palette  <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))
  #http://www.r-bloggers.com/drawing-heatmaps-in-r/
  #Auflegedatum = format(seq(index(ret)[1], length=2, by="-1 months")[2],"%B %Y")
  #Bisdatum = format(seq(index(ret)[len(ret)-1], length=2, by="-1 months")[2],"%B %Y")
  #Auflegedatum=format(time((data$prices[frame])[1,]), '%d%b%y')
  #Bisdatum = format(time(tail(data$prices[frame],1)[1,]), '%d%b%y')
  Auflegedatum=fromTo(ret)[1]; Bisdatum = fromTo(ret)[2]
  title=sprintf("Drawdowns %s-%s ",Auflegedatum,Bisdatum)
  
  try(heatmap.2(stock.returns, col=rainbow(len(palette.breaks), end=4/6), Rowv=NA, Colv=NA,
                do.dendro=c(FALSE,FALSE), scale="none", legend=2,ylab = NULL,
                main=title, trim=.8))
}

Heatmap<-function(frame="",ret=diff(log(data$prices[frame]))[-1])
{
  frame=toString(frame)
  stock.returns <-as.matrix(ret)  
  # following code limits the lowest and highest color to 5%, and 95% of your range, respectively
  quantile.range <- quantile(stock.returns, probs = seq(0, 1, 0.01),na.rm=T)
  palette.breaks <- seq(quantile.range["1%"], quantile.range["99%"], 0.01,na.rm=T)
  
  # use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
  color.palette  <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))
  #http://www.r-bloggers.com/drawing-heatmaps-in-r/
  #Auflegedatum = format(seq(index(ret)[1], length=2, by="-1 months")[2],"%B %Y")
  #Bisdatum = format(seq(index(ret)[len(ret)-1], length=2, by="-1 months")[2],"%B %Y")
  #Auflegedatum=format(time((ret[frame])[1,]), '%d%b%y')
  #Bisdatum = format(time(tail(ret[frame],1)[1,]), '%d%b%y')
  Auflegedatum=fromTo(ret)[1]; Bisdatum = fromTo(ret)[2]
  title=sprintf("Drawdowns %s-%s ",Auflegedatum,Bisdatum)
  
  try(heatmap.2(stock.returns, col=rainbow(len(palette.breaks), end=4/6), Rowv=NA, Colv=NA,
                do.dendro=c(FALSE,FALSE), scale="none", legend=2,ylab = NULL,
                main=title, trim=.8))
}

RiskReturnPlot<-function(fra=2005,ret =as.xts(diff(log(data$prices))))
{
  frame = toString(fra)
  #browser()
  ret = ret[frame]
  if (is.null(ret))
  {
    prices = na.omit(data$prices[frame])
    ret = na.omit( prices / mlag(prices) - 1)
  }
  #Auflegedatum=format(time((data$prices[frame])[1,]), '%d%b%y')
  #Bisdatum = format(time(tail(data$prices[frame],1)[1,]), '%d%b%y')
  Auflegedatum=fromTo(ret)[1]; Bisdatum = fromTo(ret)[2]
  try(chart.RiskReturnScatter(as.xts(ret),  main = sprintf("RiskReturn  %s - %s",Auflegedatum,Bisdatum), colorset = rainbow8equal))
} 


#####################################################################################################
#####################################################################################################
#undebug(preprepData)


preprepData<-function(frame1=myFrame,tickers=tickers,data = .GlobalEnv,visual = T)
{
  
  mP("preprepData----> kill non prices at %s",frame1)
#browser(mP("preprepData##############"))  
  #userStop("prepData %s",toString(tickers), pos="prepData")
  
  mP(ls(data))
  #browser() #MMX
  #    browser()
  delNonPrices(data)
  #data[["GDAXI"]]  
  cat("\nprepreaData----> adjust",tickers)
  
  #for(i in tickers) {data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)}
  
  #browser()  #T!  preprepData
  library(timeDate )   #fuer isWeekday  
 
  mP("normColnames")
  
  for(i in tickers) {colnames(data[[i]])=normColnameS(data[[i]])}
  
  for(i in tickers) {sag(i);x = data[[i]];data[[i]]= x[isWeekday(time(x))]}
  
  
  ############################################################
  #Fredpreise mit  data - preisen verbinden
  ############################################################
  # if (!is.null(FredTicker))
  # {
  #   for(i in ls(Freddata)) {data[[i]] = na.omit(Freddata[[i]])}
  #   tickers = c(tickers,ls(Freddata))
  # }
  # tickers
  data.weekly <<- new.env()
  #undebug(to.weekly)
  
  #for(i in tickers)
  #  print(head((data[[i]])[frame1],10))
  
  #head(data[["GDAXI"]])
  #browser()
  if (len(tickers) == 0)
    cat("Warning at prepreData:  no tickers !!!  ")
  for(i in tickers) 
  {
    data.weekly[[i]] = to.weekly(data[[i]], indexAt='endof',name=i)
    
    print(tail(data.weekly[[i]],1))
  }
  
  data.weekly <<- new.env()
  bt.prep(data.weekly, align='remove.na', dates=frame1)
  
  #xyplot(data.weekly$prices)
  
  # ls(data.weekly)
  data.monthly <<- new.env()
  for(i in tickers) data.monthly[[i]] = to.monthly(data[[i]], indexAt='endof',name=i)
  ##schreibe nata data$prices die Close - Zeilen der Zeitreihen und auch data$symbolnames
  bt.prep(data.monthly, align='remove.na', dates=frame1)
  
  ###############################################################
  
  #browser()
  bt.prep(data,  dates=frame1)
  colnames(data$prices)
  prices = data$prices
  data$weight = prices #einfach nur um die Dimension zu ?bertragen
  data$weight[]=NA
  
  
  ret =ROC(data$prices)  
  return(ret)
  print("<<<<---- preprepData")
}



weeklyData<-function()
{
  try(xyplot(data.weekly$prices,
             col = c("darkolivegreen3","cadetblue","goldenrod","gray70"),
             lwd = 3,
             par.settings = theEconomist.theme(box = "transparent"),
             lattice.options = theEconomist.opts(),
             main = paste("prices ",myFrame)))
}
#ret = diff(log(data$prices))
#####################################################
#f?er einen ersten blick auf die Daten 3 Modelle:
#forecast free weekly/dayly und  buyAndHold-EqualWeighted
###########################################

#if (doTestModell)

# find week ends
simPortfolio.equalWeigth<-function(data=data,BENCH="")
{
  prices=data$prices
  fromTo(prices)
  ret = na.omit(prices / mlag(prices) - 1)
  if (BENCH == "")
  {
    benchId=1  
  }
  else
    benchId= which(colnames(data$prices)==BENCH)
  if (benchId   < 1)    benchId=1
  Bench =ret[,benchId]
  if (len(colnames(ret)) > 2) 
    ret=ret[,-benchId]
  
  week.ends = endpoints(prices, 'weeks')
  week.ends = week.ends[week.ends > 0]    
  n = len(colnames(prices))
  ##########################################################################################
  # Modell:   Die Benchmark bekommt alles
  ##########################################################################################
  data$weight[] = NA
  wBench=rep(0,n); wBench[benchId]=1
  
  #gleichgewichtet .. die top n werte
  data$weight[] = NA
  data$weight[week.ends,] = wBench    #die Bench enth?lt alles
  
  capital = 100000   #anfangsverm?gen des fonds
  data$weight[] = (capital / prices) * data$weight  #price normalisierung
  onlyBench.weight = bt.run(data, type='share', capital=capital, trade.summary=T)
  ##########################################################################################
  # Modell:  Alle werden gleichgewichtet
  ##########################################################################################
  # Equal Weight 1/N Benchmark
  data$weight[] = NA
  data$weight[week.ends,] = ntop(prices[week.ends,], n)  	#gleichgewichtet .. die top n werte
  #Dw<-data.frame(data$weight)
  
  capital = 100000   #anfangsverm?gen des fonds
  data$weight[] = (capital / prices) * data$weight  #price normalisierung
  equal.weight = bt.run(data, type='share', capital=capital,trade.summary=T)
  
  windows()  
  head(Bench)
  plotNormedPrices(cumsum(ret))
  MplotNormedPrices(ret)
  
  plotbt.custom.report.part1(equal.weight ,onlyBench.weight, trade.summary=T)
  model = equal.weight
  print(model$trade.summary$stats)
  
  
  print(tail(model$trade.summary$trades,20))
  # plotNormedPrices(cumsum(mbox(Bench, equal.weight$ret)))
  
  #charts.PerformanceSummary(equal.weight$ret[,1])
  
  colnames(ret)
  ret = model
  return (ret)
}

if (F)
{
  prices = data$prices
  ret = prices / mlag(prices) - 1
  ret = diff(log(prices))[-1]
  
  mP(colnames(ret))
  i=12
  for (i in 6:12)
  {
    s1 = sprintf("%d",i)
    if (i<10) s1=paste("0",s1,sep="")
    s=sprintf("20%s",s1)
    
    chart.RiskReturnScatter(ret[s],main=paste("\n",s))
    
    omega = toString(Omega(ret[s]))
    kelly = toString( KellyRatio(ret[s]))
    varSharp = toString(SharpeRatio(ret[s],FUN="VaR"))
    retur= Return.annualized(ret[s])
    var = VaR(ret[s])
    cVar = ES(ret[s])
    mP ("%s: Ret: %s VaR: %s Omega: %s  Kelly: %s varSharpe %s ",s,retur,cVar,omega,kelly,varSharp )
    
  }
  
  Drawdowns(ret)
}

############################### Gib eine Chart?bersicht an von ret in frame
######################################################################################
#UniverseOverview()
UniverseOverview<-function(ret = mROC(data$prices),frame ="",BENCH=BENCH)
{  
  pp=ret[frame]
  new_Win()
  fram = toString(frame)  
  layout(rbind(c(1,2),c(3)))
  for (i in seq(1,len(colnames(ret)),1))
  {
    name = colnames(ret)[i]
    print(name)
    datum1=format(index((ret[fram])[1,]), '%d%b%y')
    datum2 = format(index(tail(ret[fram],1)[1,]), '%d%b%y')
    
    chart.Histogram((ret[,i,drop=F])[fram], main = sprintf("returns %s %s",name,fram), methods = c("add.risk"))
    dret=Drawdowns(ret[frame])
    chart.Histogram((dret[,i,drop=F])[fram], main = sprintf("drawdowns %s",name), methods = c("add.risk"))
    plotNormedPrices(mRendite(ret),main=name,legend.loc_="left",bench_=name)
    
  }  
  Auflegedatum=format(time((ret)[1,]), '%Y')
  Bisdatum = format(index(tail(ret,1)[1,]), '%Y')
  #Auflegedatum=fromTo(ret)[1]; Bisdatum = fromTo(ret)[2]
  #browser()
  cat("Zeige Jahrescharts vom Auflegedatum",  Auflegedatum," bis " , Bisdatum)
  #browser()
  testYears = yearList(as.numeric(Auflegedatum),as.numeric(Bisdatum)); 
  
  #  debug(weeklyReturnHeatmap)
  # fram = "2008"
  for (fram in testYears)
    ReturnHeatmap(fram,ret)
  for (fram in testYears)
    weeklyReturnHeatmap(fram,ret)  
  for (fram in testYears)
    DrawdownsHeatmap(fram,ret)
  
  
  layout(rbind(c(1),c(2)))
  for (fram in testYears)
  {
    frame=toString(fram)
    mP("#################################### %s",frame)
    print(table.DownsideRisk(ret[frame]))
    RiskReturnPlot(fram,ret)
    plotNormedPrices(r=mRendite(ret[frame]),bench_=BENCH,legend.loc_="bottomleft")
  }
  
  
  
  frame =""
  layout(rbind(c(1),c(2)))
  plotNormedPrices(mRendite(ret[frame]),bench_=BENCH,legend.loc_="bottomleft")
  colSet = redfocus
  colSet = c("red",palette(gray(seq(0,.9,len=ncol(ret)))))
  Bench=ret[frame,BENCH]
  #browser()
  chart.RelativePerformance(Ra=ret,  Rb=Bench, colorset = colSet, lwd = 1, legend.loc =      "bottomleft",
                            main=sprintf("Relative Performance at %s, Bench=%s",frame,colnames(Bench)))
  
  layout(rbind(c(1),c(2)))
  #browser()
  frame =format(index(tail(pp,1)[1,]), '%Y')  
  plotNormedPrices(mRendite(pp),bench_=BENCH,legend.loc_="bottomleft")
  colSet = redfocus
  colSet = c("red",palette(gray(seq(0,.9,len=ncol(ret)))))
  
  chart.RelativePerformance(Ra=pp,  Rb=Bench, colorset = colSet, lwd = 1, legend.loc =      "bottomleft",
                            main=sprintf("Relative Performance at %s, Bench=%s",frame,colnames(Bench)))
  
}

############################################################################
############################################################################

changeMonth<-function(month)
{
  t1=switch(month,
            Oct=  "Okt",
            Dec= "Dez" ,
            Mar="Mrz",
            May="Mai",
            
            Mrz="Mar",
            Dez="Dec",
            Okt="Oct",
            Mai ="May",
            month
  )
  return(t1)  
}

###############################################################################
# Load CRB Commodities Index 
# siehe auch sysinvestor-toolbox
# http://www.jefferies.com/cositemgr.pl/html/ProductsServices/SalesTrading/Commodities/ReutersJefferiesCRB/IndexData/index.shtml
# wichtigr Fr?hindikator
#http://de.wikipedia.org/wiki/Thomson_Reuters/Jefferies_CRB_Index
#siehe auch erwartete Vola ^VIX
#sieh auch BalticDry f?r Schiffsverkehr:  BDI

#http://www.wikinvest.com/index/Baltic_Dry_Index_-_BDI_(BALDRY)
#dryship:  drys
###############################################################################
load.CRB <- function()  #MM_TODO  die url stimmt nicht mehr - schau bei http://www.eoddata.com/stockquote/index/crb.htm
{
  load.packages('gtools,gdata')
  
  #http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Excess&StartDate=19940103&EndDate=20111202
  #>>> 2013:      http://www.jefferies.com/Commodities/2cc/389
  url = paste('http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Total&StartDate=19940101&EndDate=', format(Sys.Date(), '%Y%m%d'), sep='')  
  
  crb_xls="MData/crb.xls"
  crb_xlsx="MData/crb.xlsx"
  crb_csv="MData/crb.csv"
  
  download.file(url, crb_xls, mode="wb")  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+ BREAK: 
  # leider muss jetzt das xls crb_xls noch mal von Hand in Excel geladen werden uns als xlsx abgespeichert werden
  #damit es weiter geht:
  wb=NULL
  wb <- XLConnect::loadWorkbook(crb_xlsx,  create=F)#, create = TRUE)
  #xlsx::read.xlsx2(crb_xls,1)
  
  #library(xlsx)
  #channel <- odbcConnectExcel(crb_xls)
  #mydata <- sqlFetch(channel, "mysheet")
  #odbcClose(channel)
  
  Wb = data.frame(wb[1])
  temp=coredata(Wb)
  # read as data.frame and as double
  temp =  na.omit(as.matrix(temp[-c(1:7),]))
  
  out = repmat(as.double(temp[,2]), 1, 6)
  colnames(out) = spl('CRB.Open,CRB.High,CRB.Low,CRB.Close,CRB.Volume,CRB.Adjusted')
  out[, 'CBR.Volume'] = 0
  
  ## An RFC 822 header (Eastern Canada, during DST)
  #s=            "Wed Feb 29 00:00:00 CET 2012"
  
  #Auf Arthur kommt so was an:
  if (F)
    dat_=    sapply(temp[,1],  FUN=function(x) {
      ds=(unlist(strsplit(x," ")))[c(3,2,6)]
      dat=sprintf("%s%s%s",ds[1], changeMonth(ds[2]),ds[3])
      
      if (is.na(as.Date(dat, format="%d%b%Y")))
        cat( "\ndat is ",dat," month ",ds[2])  
      
      return( as.Date(dat, format="%d%b%Y",tz="")) #TimeDate    
    } )
  else 
    #Auf logan so was: "2002-05-10 00:00:00"
    dat_=temp[,1]
  out = coredata(na.omit(out))  
  dat_= na.omit(temp[,1])
  #format(as.Date("2002-03-12"),"%Y.%b.%d")
  #as.Date("23M?r2000", format="%d%b%Y",tz="")
  #Dat_ = data.frame(dat_)
  #dat2_ = sapply(Dat_, FUN=function(x) as.Date(x,"%d%b%Y"))
  #X=cbind(dat_, dat2_)
  
  #out = make.xts( out, dat_)
  out = as.xts(out, order.by=as.Date(dat_))
  CRB<<-data.frame(out)
  
  #series = cbind(series,series)
  write.zoo(CRB, file=crb_csv, sep=";",  dec=".")
  
  #out = make.xts( out,  as.Date(temp[,1], '%m/%d/%y'))
  plot(out)
  return(out)
}

if (F)
{
  CRB = get.CRB()
  load.CRB()
  load.IFO()
}


#########################################
#erzeuge ifo  und IFO (tages- und MONATS-Daten)
#########################################
load.IFO <- function(envir=.GlobalEnv,visual=T)
{
  # load.packages('gtools,gdata')
  
  ifo_xls="MData/ifo2.xls"
  ifo_xlsx="MData/ifo.xlsx"
  ifo_csv="MData/ifo.csv"
  
  url ="http://www.cesifo-group.de/link/KT_03_2012-ifo-geschaeftsklima-lr.xls"
  url = "http://www.cesifo-group.de/dms/ifodoc/lr/gsk-e/gsk-e-201301.xls" 
  
  #http://www.cesifo-group.de/ifoHome/facts/Time-series-and-Diagrams/Zeitreihen.html
  #http://www.cesifo-group.de/dms/ifodoc/lr/gsk-e/gsk-e-201308.xls
  #cool:
  #http://www.cesifo-group.de/dms/ifodoc/lr/ktdl-e/ktdl-e-201308.xls
  #ifo arbeitslosen ger
  #http://www.cesifo-group.de/dms/ifodoc/lr/empl-e/empl-e-201308.xls
  #ifo hat noch mehr
  #http://www.cesifo-group.de/ifoHome/facts/Time-series-and-Diagrams/Zeitreihen/Reihen-Dienstleister.html
  
  
 temp= try( download.file(url, ifo_xls, mode="wb"))
 if (inherits(temp,"try-error"))
   return(NULL)
  #MMA ifo
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+ BREAK: 
  # leider muss jetzt das xls crb_xls noch mal von Hand in Excel geladen werden uns als xlsx abgespeichert werden
  #damit es weiter geht:
  wb=NULL
  wb <- XLConnect::loadWorkbook(ifo_xls,  create=F)#, create = TRUE)
  #xlsx::read.xlsx2(crb_xls,1)
  
  #library(xlsx)
  #channel <- odbcConnectExcel(crb_xls)
  #mydata <- sqlFetch(channel, "mysheet")
  #odbcClose(channel)
  
  Wb = data.frame(wb[1],stringsAsFactors=F)
  #die ersten 15 zeilen l?schen
  Wb=Wb[-c(1:15),]
  
  colnames(Wb)=spl("Date,Open,High,Low,Close,Stuecke,Volume")
  out = as.xts(Wb, order.by=as.Date(paste("01",trim(Wb$Date),sep="/"), format="%d/%m/%Y"))
  out = out[,-1]
  colnames(out)[1:6] = c("ifo.Open","ifo.High","ifo.Low","ifo.Close","ifo.Volume","ifo.Adjusted")
  
  
  head(out)
  tail(out)
  write.zoo(out, file=ifo_csv, sep=";",  dec=".",quote=F)
  out = read.zoo( file=ifo_csv, sep=";",dec=".",format=csvDatFormat,header =TRUE)        
  x=read.csv(file=ifo_csv, sep=";",dec=",")
  out=as.xts(coredata(x[,-1]), as.Date(coredata(x[,1])))
  #mPlot(xXts)
  plot(out)
  if (F)
  {
    
    for (ii in c(1:   ncol(out)))
      out[,ii] = as.numeric(coredata(out[,ii]))
    
    apply(out,2,as.numeric)
    
    #alle Spalten in numerische spalten transformieren
    mat = c()
    for(i in 2:7) #dabei wird das datum ignoriert
      mat =cbind(mat, as.numeric(coredata(Wb[,i])))
    #die datenmatrix mit der datums-spalte zu einem xts machen
    
    out = as.xts(mat, order.by=as.Date(paste("01",trim(Wb$Date),sep="/"), format="%d/%m/%Y"))
    #di 6 ifo spalten R1..R6  als open,high... verkleiden
    colnames(out)[1:6] = c("ifo.Open","ifo.High","ifo.Low","ifo.Close","ifo.Volume","ifo.Adjusted")
    
  }
  
  assign("IFO",out,envir=envir)
  #browser()
  #einen Teil schon mal anschauen
  out1=out#["2000/2019",]
  if (visual)
  {
  mPlot(out,ylog_=F)
  #browser()
  plot(out1[,1],main=sprintf("ifo %s",fromTo(out)[2]),ylim=range(out1[,1:3]))
  lines(out1[,2],col="red")
  lines(out1[,3],col="green",lwd=2)
  head(out)
}
  #TODO  den kommenden ifo Monat forcasten
  
  
  ft = fromTo(out)
  startDay =ft[1]
  endDay = ft[2]

  endDay=as.Date(as.POSIXlt(timeLastDayInMonth(endDay,format = "%Y-%m-%d") ))
  
  days = -as.numeric(difftime(startDay,endDay),units="days")
  
  #eine leere xts-reihe mit dageswerten von bis
  emptyXTS = xts(1:days, as.Date(startDay)+1:days)
  #da werden die ifo-monats-werte reingeeschrieben
  ifoDrin= merge(emptyXTS, out)
  ifoDrin = ifoDrin[,-1]
  head(ifoDrin)
  # backfill
  ifo=ifoDrin
  ifo[] =apply(coredata(ifoDrin), 2, m.ifna.prev)
  
  
  #TODO   auff?llen mit Tagesdaten
  #apply.monthly(ifo[,3],FUN=max)
  len(ifo)
  len(out)
  head(out)
  head(ifoDrin)
  #to.daily(out)
  #ifo=to.period(ifo,'days')
  #apply.monthly(ifo[,3],FUN=max)
  ifo= ifo[isWeekday(time(ifo))]
  
  write.zoo(ifo, file=ifo_csv, sep=";",  dec=".")
  print(tail(ifo))
  assign("ifo",ifo,envir=envir)
  return(ifo)
  
}



mget.CRB.test <- function()  
{
  #*****************************************************************
  # Load historical data
  #******************************************************************   
  CRB = load.CRB()
  
  load.packages('quantmod')	
  # http://etfdb.com/
  tickers = spl('GSG,DBC,DRYS')		
  getSymbols(tickers, src = 'yahoo', from = '1970-01-01')
  
  #d<-data.frame(DRYS)
  #*****************************************************************
  # Compare different indexes
  #****************************************************************** 	
  out = na.omit(merge(Cl(CRB), Cl(GSG), Cl(DBC),Cl(DRYS)))
  colnames(out) = spl('CRB,GSG,DBC,DRYS')
  temp = out / t(repmat(as.vector(out[1,]),1,nrow(out)))
  
  layout(1:2)
  plota(temp, ylim=range(temp))
  plota.lines(temp[,1],col=1)
  plota.lines(temp[,2],col=2)
  plota.lines(temp[,3],col=3)
  plota.lines(temp[,4],col=4)
  plota.legend(colnames(temp),1:4)
  
  temp = compute.cor(temp / mlag(temp)- 1, 'pearson')
  temp[] = plota.format(100 * temp, 0, '', '%')
  plot.table(temp)	
}
#
#mget.CRB.test()


update_Allcsv <-function(dataPath = "MData")  #MMA
{
  #*****************************************************************
  # Update all historical data
  #******************************************************************   
  #F?r alle xls-Files
  for (csvfile in 
       dir(path = dataPath, pattern = "*.csv", all.files = T,
           full.names = F, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE)
  )
  {   
    #browser()
    Item=""
    item = strsplit(csvfile, "\\.")
    
    ss =unlist(item)
    
    if (len(ss) > 1)
    {
      
      
      Item = toString(first(ss))
      cat("\n####:  ", Item )
      #browser()
      mGetTickers(Item,online=T)
      #fname = paste(dataPath,csvfile,sep="/")
    }
  }  
}


#mGetTickers("URTH")

if(F)
  update_Allcsv("MData")

if (F)
  get.Isin("ALV.de")

###########################################
get.Isin<-function(sym)
{
  url=sprintf("http://uk.finance.yahoo.com/q?s=%s",sym)
  txt = readLines(url)
  res=""
  for (x in txt)
  {
    pat= "ISIN:"  
    r=str_extract(x,pat)
    if (!is.na(r))
    { 
      res=StrBetween(x,pat,"</")
      mP("found Isin %s  %s",sym,res)
      return (res)
    }
  }
  res
}
###########################################
get.testCode_doesn_work<-function(sym)
{
  
  
  sym="ALV.DE"
  
  
  url=sprintf("http://uk.finance.yahoo.com/q/ks?s=%s",sym)
  txt = readLines(url)
  temp = extract.table.from.webpage(txt, 'Market Cap', hasHeader = F)
  txt2 = txt[-c(1 : grep('Valuation Measures', txt))]
  res=""
  
  txt2 = txt[grep('Trailing P/E', txt)]
  txt2 = txt[grep('Valuation Measures', txt)]
  extract.table.from.webpage(txt2)
  
  StrBetween(txt2,'Trailing P/E (ttm, intraday):</td><td class=\"yfnc_tabledata1\">',"</td")
  for (x in txt)
  {
    pat= "Trailing P/E"  
    r=str_extract(x,pat)
    if (!is.na(r))
    { 
  browser(mP("llllll"))
      res=StrBetween(x,pat,"</")
      mP("found Isin %s  %s",sym,res)
      return (res)
    }
  }
  res
}

###########################################
#Isin="LU0274221281"

##M! shareholder  ->siehe auch MM!shareholder in Datasources.doc
##Hole dir bei Ariva.de  zu einer gegebenen Isin den Zeitreihenbezeichner
arivaSecuId<-function(Isin)
{
  word1='chart.m\\?secu='
  word2="&amp"
  
  url=sprintf("http://www.ariva.de/search/search.m?searchname=%s",Isin)
  # browser()
  txt = readLines(url)
  res = list()
  res$Isin=Isin
  
  for (x in txt)
  {
    
    pat= "chart\\.m\\?secu=([0-9a-zA-Z]+)&amp"  
    r=str_extract(x,pat)
    if (!is.na(r))
    { 
      res$arivaName = StrBetween(r,word1,word2)
      return (res)
    }
    pat="<meta name=\\\"keywords\\\" content=\\\""
    r=str_extract(x,pat)
    if (!is.na(r))
    {
      #browser()
      res$LongName=trim(StrBetween(x,"<meta name=\\\"keywords\\\" content=\\\"","\\\">"))  
    }
    
  }
}
#k=arivaSecuId("FR0010527275")



if (F)
{
  x="sadfasfasf<meta name=\"keywords\" content=\"db x-tr.SMI ETF Inhaber-Anteile 1D (WKN DBX1SM, ISIN LU0274221281), Fonds\"> </head> <body >"
  
  res=StrBetween(x,"<meta name=\\\"keywords\\\" content=\\\"","\\\">")  
  res=StrBetween(x,"<meta name=\\\"keywords\\\" content=\\\"","")  
  res=StrBetween(x,"","\\\">") 
  
  k=arivaSecuId("FR0010527275")
  
  Symbols=c("LU0274221281","FR0010527275")
  i=1
  type="day"
  getSymbols.ariva(c("LU0274221281","FR0010527275"))
}

#
if (F)
  getSymbols.ariva(  spl("DE000A0F5UH1,DE000A0RM447,DE0005933931,DE000A1JB4Q0"))
if (F)
  getSymbols.ariva(spl("FR0011133644,LU0490619193,LU0274209237,DE000A0YBR20,LU0292107645,FR0010429068,FR0011314277,LU0659580079,DE000A1EK0G3,DE000A0RM454,DE000A1C8QT0,DE000A0HG2S8,DE0006289465,DE000A0YBRZ7,DE000A1W0PN8,LU0445305625"))

if (F)
  getSymbols.ariva(spl("LU0292106241,LU0292106753"))
###################################################################

#  Ergänze die Stammdaten für Ariva.de - Daten - die via Isin  abgefragt werden
#
#getSymbols.ariva(c("LU0274221281","FR0010527275"))
#bekommt ein Liste von ISINS- sucht dann auf Ariva.de   mit 
#arivaSecuId() nach dem internen Ariva-Namen (und holt sich auch den LongName)
#dann wird die Zeireihe gleich mal runtergeladen  (nach MData/temp) und zum Schluss ein
#newSecList.xlsx geschrieben mit den neuen Stammdaten - Infos
#die dann händisch nach securities.xls übertragen werden - und um eigene kurze
#Name für das Wertpapier ergänzt werden.
#
###################################################################
getSymbols.ariva <- function   #MMAgetSymbols.ariva
(
  Symbols, 
  type = spl('day,hour'),
  env = .GlobalEnv, 
  newSecList_xls = "newSecList.xlsx",
  auto.assign = F,
  download = T	
) 
{		
  type = type[1]
  
  # setup temp folder
  temp.folder = paste(getwd(), '/MData/temp', sep='/')
  dir.create(temp.folder, F)
  
  ariva= NULL
  # read all Symbols
  for (i in 1:len(Symbols)) {	
    if(download) {
      # http://www.fxhistoricaldata.com/download/EURUSD?t=hour
      #url = paste('http://www.fxhistoricaldata.com/download/', Symbols[i], '?t=', type, sep='')
      
      
      hist =0    
      A =arivaSecuId(trim(Symbols[i]))
      arivaLongName= prettyWpName(A$LongName)
      arivaName = A$arivaName
      
      #die neuen Zeilen für die securities.xls
      #MM_TODO  - muss bei jedem zusätzlichen Tick-Provider erweitert werden       
      newSec = data.frame(Name=arivaLongName,  ISIN=trim(A$Isin),	BloombergID="",	YahooTicker="",ArivaTicker=A$arivaName,	stoxxTicker	="", FRED="", GoogleTicker="",OandaTicker="",	Sonstige ="",	LongName=A$LongName,	isIndex=0,	selectCol=0)
      #browser()
      if (is.null(ariva))
        ariva = newSec
      else
        ariva = rbind(ariva,newSec)
      #browser()
      filename = paste("MData/temp", '/', trim(Symbols[i]), '_', arivaName, '.csv', sep='')			
      hist=try(getArivaSeries(arivaName , auto.assign=TRUE))
      
      if (F)
      {
        url=sprintf("http://www.ariva.de/quote/historic/historic.csv?secu=%s&boerse_id=1&clean_split=1&clean_payout=0&clean_bezug=0&min_time=1.1.1976&max_time=todayS&trenner=/",arivaName)
        
        #download.file(url, filename,  mode = 'wb')
        
        tabDat = read.csv(url,  header=TRUE, stringsAsFactors=F,dec=",",skip=1,sep="/")#
        #  tabDat[2,2]+3
        for (ii in c(2:   ncol(tabDat)))
          tabDat[,ii] = as.numeric(tabDat[,ii])
        
        tabDat[,1] =as.Date(tabDat[,1])
        
        colnames(tabDat)=spl("Index,Open,High,Low,Close,Stuecke,Volume")
        hist= as.xts(tabDat[,-1], order.by=as.Date(tabDat[,1]))
      }
      
      
      hist = na.omit(hist)
      if (len(hist))
      {
      print(Symbols[i])
      print(arivaName)
      print(head(hist))
      ft=fromTo(hist)
      #browser()
      
      #mPlotPureData(hist[,c(1,2,3,4)],main=sprintf("%s",arivaLongName))
      print(filename)
      
      
      #schreibe die Zeitreihe - nach temp -- noch hat sie ja keinen offiziellen Namen
      try(  write.zoo(hist, file=filename, sep=";",  dec="."))
      #browser()    
      
      # unpack
      #unzip(filename, exdir=temp.folder)	
      #cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')  				
      
      if (auto.assign) {  	
        assign(paste(gsub('\\^', '', Symbols[i]), type, sep='_'), hist, env)	
        
      }
      }
      
    }	
  }
  
  #######################
  
  if (!is.null(ariva))
  {
    # Create a new workbook 'saveMe.xlsx'
    # (assuming the file to not exist already)
    wb <- loadWorkbook(newSecList_xls, create = TRUE)
    # Create a worksheet called 'mtcars'
    createSheet(wb, name = "newSecList")
    # Write built-in dataset 'mtcars' to sheet 'mtcars' created above
    writeWorksheet(wb, ariva, sheet = "newSecList")
    # Save workbook - this actually writes the file 'saveMe.xlsx' to disk
    saveWorkbook(wb)
    mP("############### >>> found new Secs ")
    mP("####   Write new standing data to %s",newSecList_xls)
    mP("### copy these data to securities.xls and add short Names  - than reload TradeClasses.r")
    
  }
  
  if (!auto.assign) {
    return(ariva)
  } else {		
    return(env)				
  }	
  
}




##M! shareholder  ->siehe auch MM!shareholder in Datasources.doc
##Hole dir bei Ariva.de  zu einer gegebenen Isin den Zeitreihenbezeichner
all_ETF<-function(
  newETFlist_xls = "newETFlist.xlsx",
  online=T
)
{ 
  word1="uebersicht.html\\?ISIN="
  word2="' title="
  
  #url=sprintf("http://www.extra-funds.de/etf-gesamtliste/etf-gesamtliste.html")
  url ="extra-fund_DE_etf-gesamtliste.html" #20.3.2013
  # browser()
  txt = readLines(url)
  ariva=NULL
  #lies dir das super lange html mit allen etf  duch
  lastN = 0
  
  for (x in txt)
  {
    #  x="<td><a href='/tools-details/uebersicht.html?ISIN=FR0010713727' title='Amundi ETF MSCI India'>MSCI India (USD)</a></td>"
    pat= sprintf("%s.*",word1)  
    r=str_extract(x,pat)
    #r
    if (!is.na(r))  #hab wieder ne interessane zeile im html gefunden
    {
      #browser() #  all_ETF()
      #lies Isin und LongName aus dem ETF-Html
      Isin =StrBetween(r,word1,word2)
      LongName=try(prettyWpName(StrBetween(r,"title='","'>")))
      
      
      #### gleich mal bei ariva schauen wie die Id für die historische Zeitreihe bei Ariva  ist
      A = NULL
      A=try(arivaSecuId(Isin))
      if (is.null(A))      
        arivaName="" 
      else
      {
        #und nun auch schon bei ariva laden
        arivaName = trim(A$arivaName)
        if (online)
        {
          newSec1=try(getArivaSeries(A$arivaName,LongName),silent =T) #versuch die historischen Kurse bei ariva zu laden
          if (exists("newSec1")) #und auch als csv abzuspeichern    
          {
            mP("try to write to %s",LongName)
            write.zoo(newSec1, file=sprintf("MData/%s.csv",LongName), sep=";",  dec=".")
          }
        }
      }
      ###### bookkeeping
      #ich schlage der securities.xls  als Name den LongName vor:
      newSec = data.frame(Name=LongName,  ISIN=Isin,  BloombergID="",  YahooTicker="", ArivaTicker=arivaName,	stoxxTicker	="", FRED="", GoogleTicker="",OandaTicker="",	Sonstige ="",	LongName=LongName,	isIndex=0,	selectCol=0)
      
      #browser()
      if (is.null(ariva))
        ariva = newSec
      else
        ariva = rbind(ariva,newSec)
      
    }
    if (!is.null(ariva)) #save the bookkeeping      
      if (lastN+100 < nrow(ariva) ) #wieder 20 Zeilen dabei - lieber mal abspeichern
        
        try(
{
  
  mP("############### >>> found new Secs %d",nrow(ariva))
  mP("####   Write new standing data to %s",newETFlist_xls)
  mP("### copy these data to securities.xls and add short Names  - than reload TradeClasses.r")
  lastN = nrow(ariva)
  # if (lastN >=160)
  #     browser()
  # Create a new workbook 'saveMe.xlsx'
  # (assuming the file to not exist already)
  newETFlist_xls="newETFlist01.xlsx"
  
  if (file.exists(sprintf("%s/%s",getwd(),newETFlist_xls)) )
    file.remove(sprintf("%s/%s",getwd(),newETFlist_xls))
  
  wb <- loadWorkbook(newETFlist_xls, create = TRUE)
  # Create a worksheet called 'mtcars'
  createSheet(wb, name = "newETFlist")
  # Write built-in dataset 'mtcars' to sheet 'mtcars' created above
  writeWorksheet(wb, ariva, sheet = "newETFlist")
  # Save workbook - this actually writes the file 'saveMe.xlsx' to disk
  saveWorkbook(wb)
}
        )
  }
  mP("################ fertig ")
  #browser()
  #zum Schluss noch mal die Tabelle sichern
  if (!is.null(ariva))    
    
  {
    # Create a new workbook 'saveMe.xlsx'
    # (assuming the file to not exist already)
    if (file.exists(sprintf("%s/%s",getwd(),newETFlist_xls)) )
      file.remove(sprintf("%s/%s",getwd(),newETFlist_xls))
    
    wb <- loadWorkbook(newETFlist_xls, create = TRUE)
    # Create a worksheet called 'mtcars'
    createSheet(wb, name = "newETFlist")
    # Write built-in dataset 'mtcars' to sheet 'mtcars' created above
    writeWorksheet(wb, ariva, sheet = "newETFlist")
    # Save workbook - this actually writes the file 'saveMe.xlsx' to disk
    saveWorkbook(wb)
    mP("############### >>> found new Secs ")
    mP("####   Write new standing data to %s",newETFlist_xls)
    mP("### copy these data to securities.xls and add short Names  - than reload TradeClasses.r")
    
  }
  
}

###############################################################################
# extract.table.from.webpage
###############################################################################
extract2.table.from.webpage <- function
(
  txt, 		# source text of webpage
  marker,		# key-phrase(s) located in the table to extract
  hasHeader=T	# flag if table has a header
)
{
  tryCatch({		
    # find location of data
    marker = spl(marker)
    pos1=1
    
    for(i in 1:len(marker)) {
      pos1 = regexpr(marker[i], substr(txt, pos1, nchar(txt))) + pos1
    }
    
    # find start/end of table
    pos0 = tail(gregexpr('<table', substr(txt, 1, pos1))[[1]], 1)
    pos2 = head(gregexpr('</table', substr(txt, pos1, nchar(txt)))[[1]], 1)
    temp =  substr(txt, pos0, pos1 + pos2 - 2)
    
    #browser()
    
    # remove all formating	
    temp = gsub(pattern = '<br>', replacement = '', temp, perl = TRUE) 
    
    temp = gsub(pattern = '</tr>', replacement = ';row;', temp, perl = TRUE) 
    temp = gsub(pattern = '</td>', replacement = ';col;', temp, perl = TRUE) 
    temp = gsub(pattern = '</th>', replacement = ';col;', temp, perl = TRUE) 
    
    #temp = gsub(pattern = '<.*?>', replacement = '', temp, perl = TRUE) 
    
    temp = gsub(pattern = '\r', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '\n', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '\t', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&nbsp;', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&amp;', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&raquo;', replacement = '', temp, perl = TRUE) 		
    
    
    temp = gsub(pattern = 'href=', replacement = 'HREF;', temp, perl = TRUE) 
    temp = gsub(pattern = 'target=', replacement = ';HREF', temp, perl = TRUE) 
    
    
    # parse into matrix	
    temp = lapply( strsplit(temp, ';row;'), strsplit, ';col;')	
    
    temp=temp[[1]]
    
    #[1] "<tr class=\"row-normal\"><td class=\"first\" align=\"left\" valign=\"top\">STOXX Europe 600 Basic Resources Daily Leverage"                                                                                                                                                                       
    #[2] "<td align=\"center\" valign=\"top\"><a HREF;\"/download/historical_values/h_sxprdl.txt\" ;HREF\"_blank\"><img border=\"0\" src=\"/resources/images/icon_txt.png\" onmouseout=\"this.src='/resources/images/icon_txt.png'\" onmouseover=\"this.src='/resources/images/icon_txt_active.png'\"/></a>"
    
    #lin=temp[[96]]
    
    stoxxGet<-function(lin)
    {
      
      if (len(lin)<2) return(c("",""))
      nameLin=lin[[1]]
      nameLin = gsub(pattern = '<.*?>', replacement = '', nameLin, perl = TRUE) 
      
      refLin=lin[[2]]
      refLin=StrBetween(refLin,"HREF;\"/",";HREF")
      if (is.na(refLin))
        refLin2=""
      else
        refLin2=sprintf("%s/%s","http://www.stoxx.com",paste(leftOf(refLin,".txt"),"txt",sep="."))
      # http://www.stoxx.com/download/historical_values/h_sx5hun.txt
      return(c(nameLin, refLin2))
    }
    
    temp = lapply( temp, stoxxGet)  
    
    
    
  }, error = function(ex) {
    temp <<- txt
  }, finally = {
    return(temp)
  })
}


#######################################################################################
##M! shareholder  ->siehe auch MM!shareholder in Datasources.doc
##Hole dir bei Ariva.de  zu einer gegebenen Isin den Zeitreihenbezeichner
#######################################################################################
all_Stoxx<-function(#MMA
  newStoxxlist_xls = "newStoxxlist.xlsx",
  online=T
)
{ 
  
  url ="http://www.stoxx.com/data/historical/historical_strategy.html"
  txt = join(readLines(url)) 
  
  blist= extract2.table.from.webpage(txt,"plain text format:",T)
  
  #txt = readLines(url)
  
  #library(XLConnect)
  #temp.folder = paste(getwd(), 'temp', sep='/')
  #dir.create(temp.folder, F)
  
  dataPath="MData/stoxx"
  
  stoxx_vendors=sprintf("%s/vendor_codes.xls",temp.folder)
  download.file('http://www.stoxx.com/download/indices/vendor_codes.xls', stoxx_vendors ,mode = 'wb' )
  
  #if(clean) shell('del /F /S /Q temp\\*.*', wait = TRUE)
  #files = unzip('DataOnly.zip', exdir=temp.folder)
  wb <- readWorksheetFromFile(stoxx_vendors, sheet=1, startRow=1, header=TRUE)
  
  stoxxStamm<- data.table(wb)
  colnames(stoxxStamm)
  setkey(stoxxStamm,"Full.Name")
  
  #wb <- loadWorkbook(stoxx_vendors, create = F)
  stoxx=NULL
  #lies dir das super lange html mit allen etf  duch
  lastN = 0
  
  for (stoxxI in blist )
  {
    stoxxName = trim(stoxxI[1])
    url=trim(stoxxI[2])
    if (url !="" && stoxxName !="")
    {
      print(stoxxName)
      print(url)
      lin=first(stoxxStamm[stoxxStamm$Full.Name==stoxxName])
      isIndex=0
      BloombergID=""
      Isin=""
      #browser()
      LongName = prettyWpName(StrBetween(stoxxName, "\\(","\\)"))
      if (!is.null(lin))
      {
        Isin = toString(lin$ISIN)
      #  browser()
        BloombergID = toString(lin$Bloomberg)
        classifi=  toString(lin$Classification)
        print(classifi)
      
        if ( classifi =="Benchmark")
          isIndex=1
      }
      
      if (online)
      {
        
        stoxxTick = stoxxName
        newSec1=NULL
        print(stoxxTick)
        
        newSec=try(readStoxx2(sym=stoxxName,tick=stoxxTick,url=url))
        newSec1=newSec$data
        LongName=prettyWpName(newSec$symbol)
        colnames(stoxxStamm)
        
        #newSec1=try(getstoxxSeries(A$stoxxName,LongName),silent =T) #versuch die historischen Kurse bei stoxx zu laden
        if (exists("newSec1")) #und auch als csv abzuspeichern    
        {
        
          mP("try to write to %s",LongName)
          write.zoo(newSec1, file=sprintf("%s/%s.csv",dataPath,LongName), sep=";",  dec=".")
        }
      }
      
      ###### bookkeeping
      #ich schlage der securities.xls  als Name den LongName vor:
      newSec = data.frame(Name=LongName,  ISIN=Isin,  BloombergID=BloombergID,  YahooTicker="", stoxxTicker=stoxxName,	stoxxTicker	="", FRED="", GoogleTicker="",OandaTicker="",	Sonstige ="",	LongName=stoxxName,	isIndex=0,	selectCol=0)
      
      #browser()
      if (is.null(stoxx))
        stoxx = newSec
      else
        stoxx = rbind(stoxx,newSec)
      
    }
  }
  mP("################ fertig ")
  #browser()
  #zum Schluss noch mal die Tabelle sichern
  if (!is.null(stoxx))    
    
  {
    # Create a new workbook 'saveMe.xlsx'
    # (assuming the file to not exist already)
    if (file.exists(sprintf("%s/%s",getwd(),newStoxxlist_xls)) )
      file.remove(sprintf("%s/%s",getwd(),newStoxxlist_xls))
    
    wb <- loadWorkbook(newStoxxlist_xls, create = TRUE)
    # Create a worksheet called 'mtcars'
    createSheet(wb, name = "newETFlist")
    # Write built-in dataset 'mtcars' to sheet 'mtcars' created above
    writeWorksheet(wb, stoxx, sheet = "newETFlist")
    # Save workbook - this actually writes the file 'saveMe.xlsx' to disk
    saveWorkbook(wb)
    mP("############### >>> found new Secs ")
    mP("####   Write new standing data to %s",newStoxxlist_xls)
    mP("### copy these data to securities.xls and add short Names  - than reload TradeClasses.r")
    
    
  } 
}

if (F)
  all_Stoxx()


print("########### load LoadDataFuncs.R")
