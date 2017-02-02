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
# Plot table ( vector or matrix )
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################
# Patches by MMiksa 2013
###############################################################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
###############################################
# ergänze die plotbt.custom.report.part..
#  um einen zusätzlichen mm=1- Parameter mit dem 
#  wählen kann welches Modell im Detail beleuchtet wird
#  ist default mäßig immer das erste ...
###############################################
print("########### patch SIT")
############################################################################################

#### Aus der riesen Fülle von Ergebniszahlen schneid ich mir da raus was ich brauch:

modelResults<-function(modelList)  #MMA
{
  r=lapply(modelList,FUN=function(x) {
    info=bt.detail.summary((x))
    c(Cagr=info$System$Cagr,Exposure=info$System$Exposure,                              Sharepe=info$System$Sharpe, MaxDD=info$System$MaxDD, VaR=info$System$VaR,Num.Trades=info$Trade$Num.Trades, WinLoss.Ratio=info$Trade$WinLoss.Ratio)
    
  })
  #  print(r)
  return(r)
}  


if (F)
  plotWeight(weight)
#oder  plotWeight(signal)

##################################################################
#die gewichte oder signale in reinform - ohne kurse
##################################################################
plotWeight <- function(weight,main="",plotX=T)
{
  wpCols=rainbow(ncol(weight), start=0, end=.9) #die Farben der Wertpapiere
  wpColsLight = sapply(wpCols, function(x) {col.add.alpha(x , 50)})  #die 
  
 plotbt.transition.map(weight,main,wpColsLight,plotX=plotX)
}
##################################################################
#zeigt was alles mit plot.table geht
##################################################################

plotCorrelations<-function(prices)
{
  temp = cor(prices, use='complete.obs', method='pearson')
  temp[] = plota.format(100 * temp, 0, '', '%')
  
  #png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')  	
  
  # plot temp with colorbar, display Correlation in (top, left) cell	
  plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)	
}

############################################################################
#drucke die Equity-Kurven der Modelle in einem vergleichenden ScatterPlot
#vergleicht auch den Turnover der Modelle
############################################################################
compareViewModels<-function(models,prices =NULL, alloc = F,period.annual.factor=12)
{
  mP("compareViewModels")
  ls(models)
  Equity = NULL
  
  weights<-list()
  out=try(strategy.performance.snapshoot(models, T))
  
  norm_Win(3)
  
  for (modNames in names(models))
  {
    textplot(modNames)

    mod =models[[modNames]]
    
    if (is.null(Equity))
      Equity = mod$equity 
    else 
      Equity= merge(Equity, mod$equity )
    
    print(tail(Equity,1))
    weights[[modNames]] = mod$weight  #die liste für den turnoverchart  bauen
    #print(tail(mod$equity))
    
    try(
      if (alloc)
      {
        plotbt.transition.map(mod$weight,name=modNames)
        
        if (data$BENCH %in% colnames(data$prices))
        {
          lines( scaleTo(close=data$prices[,data$BENCH],c(0,100)))
          legend('topright', legend = m, bty = 'n')
        }
        else
        lines(scaleTo(mod$equity,c(0,100)),type="l")
      }
    )
    
    #plota.lines(mod$prices)
  }
  #browser()
  colnames(Equity) = names(models)
  tail(Equity,1)
  
  ret=modelResults(models)
  
  #browser()
  #try(charts.PerformanceSummary(ret,ylog=TRUE,    main = "compareViewModels" )
  #nen ScatterPlot zum Vergleich der Erträge bauen
  
  
  #  Cols=rainbow(len(models), start=0, end=.9) #
  #Cols=c(1:len(models))
  #  try(chart.RiskReturnScatter(mROC(Equity),  main = sprintf("Compare modelEquities"), ,colorset = Cols))
  
  norm_Win(1)
  
 to=try(sapply(models, compute.turnover, data))
  # Plot Portfolio Turnover for each strategy
  print(ret)
  print(to)
  
  
  if (F &&  !is.null(prices) && alloc)
  {
    
    #Cols=rainbow(len(colnames(prices)), start=0, end=.9) #
    Cols=c(1:(len(models)+len(colnames(prices))))
    
    try(chart.RiskReturnScatter(merge(mROC(Equity),mROC(prices)),  main = sprintf("Compare prices"), ,colorset = Cols))
  }
  
  #mPlot(Equity)
  
  #  norm_Win(3)
  ls(models)
  layout(1)
  
  try(
    barplot.with.labels(sapply(weights, function(w) period.annual.factor * mean(portfolio.turnover(w), na.rm=T)), 'Average Annual Portfolio Turnover')
  )
  
  #plotbt.custom.report.part1(models)  plot mit drawdowns
  
  #out=try(strategy.performance.snapshoot(models, T))
  
  if (alloc)
  {
    layout(1)
    Cols=c(1:len(models))
    try(chart.RiskReturnScatter(mROC(Equity),  main = sprintf("Compare modelEquities"), ,colorset = Cols))
  }
  
  if (!is.null(out))
    performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,T))
  
  # Compute stats
  #
  # compute.stats(weights,  Turnover=function(w) period.annual.factor * mean(portfolio.turnover(w), na.rm=T))
  
  #jetzt noch die Schlussbestände anzeigen
  #browser(mP("jetzt noch die Schlussbestände anzeigen"))
  #   lapply(names(models),function(mod)   tryM(plotbt.custom.report.part2(models[[mod]],main=mod)  )) 
         
  print(out)
  return(ret)
}
if(F)
  compareViewModels(models)




sysReporting<-function(models)
{
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  
  strategy.performance.snapshoot(models, one.page = F, title ="Results")
  
  # Plot Portfolio Turnover for each strategy
  layout(1)
  barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
  
  
  # put all reports into one pdf file
  pdf(file = 'report.pdf', width=8.5, height=11)
  plotbt.custom.report(models, trade.summary=T)
  dev.off()  
  
  png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')  									
  plotbt.custom.report.part1(models)
  dev.off()	
  
  png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
  plotbt.custom.report.part2(models)
  dev.off()	
  
  png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
  plotbt.custom.report.part3(models, trade.summary=T)
  dev.off()	
  
  
}
if (F)
  plotSigPrice(signal,prices,"Oil")

##################################################################
## DIE WICHTIGSTE PLOT_METHODE FÜR TIMING_SYSTEME
#
## zeigt den Kurs und Balen mit signal darüber
# an indi darf eine ganze list mit gemergten indikatoren gehängt werden
# für jedes element wird ein weiteres indikator-fenster angehängt
#xMarker darf eine Liste von Datümer sein, die dann als grüne abline dargestellt werden.
##################################################################
#PS
plotSigPrice <- function(signal=NULL,prices,indi=NULL,xMarker= global_xMarker,sel=1,main="" ,no.eq=F)
{ 
  if (is.null(signal))
    return
eq=NULL
  
  eq=try ({
  orgIndi = indi
  #falls vorhanden  entferne die NA
  
  signal[is.na(signal)] =0
  signal=signal[,sel]
  prices= prices[,sel]
  
  signal = lag(signal)
  
  ret = mROC(prices)*signal #BUG  hier ROC statt mROC
  #eq=mRendite(na.omit(ret))
  

  #block=merge(signal[,1],prices);
  #eq=block[,1]
  #signal=block[,2]
  #prices=block[,-c(1,2)]
  backlock.date=NULL#den Anfang der eq-kurve auf den preis zum ersten signal legen
  backlock=first(signal[abs(signal)==1])

  if(len(backlock)>0)
    backlock.date =  DateS(backlock)
  #offset.D=DateS(first(sig[!is.na(sig)]))
  
  
  signal[is.na(signal)] =0
  
  colnames(signal)= colnames(prices[,1])
  #falls vorhanden  entferne die NA
  prices[]= m.ifna.prev(coredata(prices))
  #browser()
  if (!is.null(indi))    
    if (is.xts(indi))
      norm_Win(2)
  else
  {
    norm_Win(1+len(indi))
  }
  #lines(prices)
  #eq = scaleTo(eq,3*range(prices))
  
  
  #' plota(y, type = 'ohlc', LeftMargin=3)
  #'   		
  #' y0 = y			
  #' y = data.ibm['2010:10:15::2011:02:01']		
  #' plota2Y(y, ylim = range(OHLC(y)),las=1, col='red', col.axis = 'red')
  #'   plota.ohlc(y, col = 'red')		
  if (main=="")
    main =colnames(prices)[1]
  
  #prices=mNorm(prices)
  
  #ret = ((prices-lag(prices,1)))*lag(signal)

  if (F)
  {
  prices=mNorm(prices)
  ret = mROC(prices)*signal
  ret[1]=0.0000001#prices[1]
  #eq = cumsum(ret)
  #browser()
  eq=mRendite(ret)#cumprod(ret+1)  
}
  
 # browser(mP("...."))
  
  #ret=mLogReturn(prices)*signal
  #eq=mLogRendite(ret)
  
  #ret = diff(log(prices))*signal
 # browser()
  
  #signal = prices;signal[]=1
  
  #ret = mROC(prices)*sign(signal)
  
  #first(signal[signal==1])
  #ret =Return.calculate(prices)*sign(signal)
  #charts.PerformanceSummary(na.omit(ret))
  
#  eq=mRendite(na.omit(ret))#+0.3
 
  # calculate Close-to-Close returns
  ret = ROC(prices, type='discrete');  ret[1] = 0
  prices=cumprod(1 + ret)
  # compute stats  
  #browser(mP("#####1##"))
  
  price.b=prices[backlock.date]
  
  trades=bt.exrem(signal)
  #die trades mit den handelspreisen
  trade.prices=abs(trades)*prices
  #browser(mP("#1"))
  commi= trade.prices*global_commission
  #len(na.omit(commi)) die anzahl trades
  commi[is.na(commi)]<-0
  total_commi=sum(commi)  #die gesamt commision
  
  #eq = cumprod(1 + ret*sign(signal))
  #plot(eq)
  ret=ret-commi #ret - um commission pro trade reduziert  (als vector)
  signal[sprintf("::%s",backlock.date)]=1
  eq = cumprod(1 + ret*sign(signal))

  #lines(eq,col="red")
 
  #eq = cumprod(1 + (ret-(abs(sign(na.omit(bt.exrem(signal)))) * global_commission))*sign(signal))
    
  eq.b= eq[backlock.date]   #schiebe die eq - kurve auf den prei des ersten entry-signals
  #ret[backlock.date] = ((price.b-eq.b))[backlock.date]
 
  #eq = eq+nval(ret[backlock.date])  #die lage der eq-kurve wird auf zum ersten signal auf die preis-kurve geshiftet
  
  #eq = cumprod(1 + ((ret-global_commission)*sign(signal)))
  
  #backlock.date="2009-07-17"
  
  #plot(prices); lines(eq,col="red")
  #lines(scaleTo(signal,range(eq,na.rm=T)),col="green")
  
  #browser(mP("check#####"))
  
  if (no.eq)
    rb = range(prices,na.rm=T)
  else
    rb = range(eq,prices,na.rm=T)
  #plota.lines(rb,ylim=c(0,100), type='l', col=col,lwd=2)               
  

  plotWeight(signal,main=main,plotX=is.null(indi))
  
  plota2Y(prices,axis_val=4,ylim=rb,col="blue")
  col = iif(signal > 0, 'blue', iif(signal < 0, 'red', 'gray')) 
  
  lines(prices)
  plota.lines(prices, type='l', col=col,lwd=2)               
  
  #lines(prices,col="darkblue",lwd=1)
  
  col = iif(signal > 0, 'blue', iif(signal < 0, 'red', 'gray')) 
  #browser(mP("üüüüüüüüüüüüüüüüüüüüüüüüüüüüü"))
  plota.lines(eq, type='l', col=col.add.alpha("black" , 90),lwd=2)  #MMX             
  plota.lines(eq, type='l', col="black",lwd=1)  #MMX             
  
  
  lines(scaleTo(signal,rb),col="orange")
  
  # plot(eq,axis_val=4,ylim=rb,col="blue"))
  
  if (!is.null(xMarker))
  {
    lapply(xMarker,function(x) abline(v=as.POSIXct(as.Date(x)),col=col.add.alpha("darkgreen" , 95),lwd=3))
    #browser()
    lapply(xMarker,function(x)
      text(as.POSIXct(as.Date(x)), 1, labels=toString(x), adj=c(-1,1), col='darkgreen',xpd=TRUE,srt=90))
    
  }
  plota.legend('Flat,Long,Equity','gray,blue,red')     
  
  
  if (!is.null(indi))
    if(len(indi)> 0)
    {
      #browser()
      if (is.xts(indi))
        ind = list(indi)
      else
        ind = indi
      
      i=0
      for(indi in  ind)
      {
        i=i+1
        
        block=mmerge(eq,indi)
        indi = block[,-1]
        rb=range(indi, na.rm=T)
        
        #  browser()
        leg=""
        if (len(names(orgIndi))>0)
          leg=names(orgIndi)[i]
        if (leg=="")     
          leg=paste(colnames(indi),collapse=",")
        
        isLast =(i==len(ind))
        
        plotWeight(signal,plotX=isLast)    #default:  das signal
        #plota.lines(scaleTo(prices,,lwd=2) #default: der price
        plota2Y(indi, axis_val=4,ylim=rb,col="blue")
        
        col = c(1:ncol(indi))
        colSet = c("red", topo.colors(ncol(indi)))
        #colSet = c(1:ncol(indi))
        sapply(c(1:ncol(indi)),FUN=function(i){plota.lines(indi[,i],col=colSet[i],lwd=2);i})
                
        if (!is.null(xMarker))
        {lapply(xMarker,function(x) abline(v=as.POSIXct(as.Date(x)),col=col.add.alpha("darkgreen" , 95),lwd=3))
         lapply(xMarker,function(x)
           text(as.POSIXct(as.Date(x)), 1, labels=toString(x), adj=c(-1,1), col='darkgreen',xpd=TRUE,srt=90))
        }
        plota.legend(leg,colSet) 
        
      }
    }
  
  print(table.DownsideRisk(ret[-1,]))
  
  #browser()
  #charts.PerformanceSummary(ret)
  #with 'cleaned' data for VaR estimates
  if (F)
    chart.BarVaR(ret,
                 methods=c("HistoricalVaR", "ModifiedVaR"),
                 lwd=2, ypad=.01, clean="boudt",
                 main="... with Robust ModVaR Estimate")
  
  
  
  nt=try(as.numeric(numTrades(signal)$allT))
  if (len(nt)==0 || nt==0)
    nt = 1
  if (!exists("global_commission"))
    global_commission <<- 0.0005 #0.00001
  
 # eq = eq-(nt* global_commission)
  mP(" Single Symbole Results approximated ")
  mP("%d Trades #!",nt)
  mP ("Haltedauer %f Tage", len(ret[,1]) / nt)
 sharpe=compute.sharpe(na.omit(ret))
  mP("sharpe: %f",sharpe)
  cgar=compute.cagr(eq)*100
  mP("last(eq) %f with global_commission: %f - sum %f",last(eq),global_commission,total_commi)
  cgar=compute.cagr(eq)*100  
  mP("Cgar %f",cgar)
 maxdd=compute.max.drawdown(eq)*100
  mP("MaxDD %f",maxdd)
  calmar = abs(compute.calmar(eq))*sign(cgar);
  mP("calmar %f",calmar)

#library(PerformanceAnalytics)

#faber.stats<-tradeStats('faber')[,c('Net.Trading.PL','Max.Drawdown','Num.Trades','Profit.Factor','Std.Dev.Trade.PL','Largest.Winner','Largest.Loser','Max.Equity','Min.Equity')]
#faber.stats
#table.Autocorrelation(ROC(eq))
#textplot(object="hall")
#table.UpDownRatios(R=ROC(eq))
  mP("------------------------------------")
res=try(list(eq=eq,calmar=calmar,sharpe=sharpe,maxdd=maxdd,cgar=cgar,Kappa=Kappa(na.omit(ROC(eq)),0.005,1),Kelly=KellyRatio(ROC(eq)) ))
return(res)
  #charts.PerformanceSummary(ret[-1,])
})
 
}

#zu nierfiger Return für
#EMA-SMA:  wSlow 80  wFast 26



if(F)
{tradeResult(models,1, data, signal,"2005")#,"2007::")
 tradeResult(models,len(models), data, signal, sel=spl("Oil,SuP500"))
 tradeResult(models,len(models), data, signal,frame="2007::2009",reset=T)
 
}

compute.sharpe <- function(x)
{
# browser(mP("compute.sharpe"))
  
#res= SharpeRatio(x,Rf=0,p=0.95,FUN="StdDev")[1]
#return(res)

  temp = compute.annual.factor(x)
  x = as.vector(coredata(x))
  return(sqrt(temp) * mean(x)/sd(x) )
}

###################################################################
# male einen super chart der im Hintergrund die Allocations-Linie zeigt
# dann, die Equity als Schatten und darüber die WpLinie-eingefärbt- je
# nachdem in welcher Pos sie sind.
#mein komplexer All-In-OnePlot: zeigt die Zeitreihen, ihre Gewichte und auf 
#als Farbänderung auf der Linie auch noch long/short/flat - sowie die GUV Kennzahlen  
#und dunkel hinterlegt die PortfolioEquity-Kurve
###################################################################

tradeResult<-function(models, mod, data, signal,frame = "",dick=F,reset=F,sel=NULL) #MMA super
{
  
  if (is.numeric(mod))    
    name = (ls(models))[mod]
  else
    name = mod
  mP("aTradeResult for model  %s",name)
  
  model = models[[name]]
  
  #  names(models)
  print(name)
  
  print( modelResults(list(model)) )
  #die Preise normieren - und dann die dabei verloren gegangene 1. Zeile löschen
  
  fromTo(data$prices)
  prices = mNorm(data$prices)[frame]
  print (colnames(prices))
  signal = signal[frame]
  #entferne die NA
  signal[is.na(signal)] =0
  
  weight = model$weight[frame]
  equity = model$equity[frame]
  
  #evtl nur für eine sup-liste von wertpapieren
  if (!is.null(sel))
  {
    prices=prices[,sel]
    signal = signal[,sel]
    weight=weight[,sel]
    equity=equity[,sel]
  }
  if (ncol(equity) ==0)
  {
    equity = prices[,1]
    equity[]=0
  }
  n = ncol(prices)
  
  wpCols=rainbow(ncol(weight)+1, start=0, end=.9) #die Farben der Wertpapiere
  
  colnames(equity) = "Equity"
  if (T)
    try(chart.RiskReturnScatter(mROC(merge(equity,prices)),  main = sprintf("RiskReturn since %s",name), colorset = wpCols))
  
  #sollen die Reihen alle links auch auf ich gleichen punkt 1,1 anfangen?
  if (reset)
  {prices = mNorm(data$prices[frame])-1
   
   if (F)
     tradeResult(models,1, data, signal,"2007::",reset=T)
   
   equity = equity-as.numeric(first(equity)) 
  }
  #eine etwas harte Art sicher zu stellen, das der ganze Info-Block
  #auch auf einheitlichem x-Raum definiet ist:
  #nrow(info1       )
  
  info1 =  merge(prices,signal,weight,equity)
  info=info1[frame]
  #  info = info[!is.weekend(time(info))]
  
  #falls vorhanden  entferne die NA
  coredata(info)= bt.apply.matrix(coredata(info),m.ifna.prev)
  #mPlot(info,ylog_=F)
  #  browser()
  
  i=1; l=(i-1)*n+1; r = i*n
  prices=info[,c(l:r)] 
  i=2; l=(i-1)*n+1; r = i*n
  signal=info[,c(l:r)] 
  i=3; l=(i-1)*n+1; r = i*n
  weight=info[,c(l:r)] 
  i=4; l=(i-1)*n+1; r = l
  equity=info[,c(l:r)] 
  
  if (F)
  {
    colnames(info)
    first(prices)
    first(signal)
    first(weight)
    first(equity)
    
    len(prices)
    len(signal)
    len(weight)
    len(equity)
    last(equity)
    last(weight)
    last(info)
  }
  
  
  rwRange=range(model$weight)*ncol(weight)*100  #in der Regel: c( 0,100)
  
  #head(prices)
  #head(equity)
  #colnames(info)
  srange = range(na.omit(merge(equity,prices)))
  
  #range(info[,"Gold"])
  #range(info[,10])
  norm_Win(1)
  
  wpCols=rainbow(ncol(weight), start=0, end=.9) #die Farben der Wertpapiere
  wpColsLight = sapply(wpCols, function(x) {col.add.alpha(x , 50)})  #die ihrer Gewichte sollten "light" sein .. sonst bluten die Augen
  
  #Gewichte malen und equity als dunklen schatten
  plotbt.transition.map(weight,name, wpColsLight, plotSeries = equity,srange=srange)   
  
  #Equity malen
  #lines(scaleTo(equity,rwRange),lwd=2,type="h",col="blue")
  #lines(scaleTo(equity,rwRange),lwd=1,col="black")
  
  #Wp-Fette schwarz - als Rahmen 
  for (column in colnames(prices))
    #  lines(scaleTo(prices[,column],srange), type='l',col.add.alpha("grey" , 50),lwd=ifelse(dick,5,3))    
    lines(prices[,column], type='l',col.add.alpha("grey" , 50),lwd=ifelse(dick,5,3))    
  
  
  # lines(scaleTo(equity,range(weight)*100),col=col.add.alpha("grey" , 30),type="h")
  #einfärben der Preis-Zeitreihe nach Signal
  
  #jetzt die LongShortFlat-Segmente einfärben ...in dünn
  for (i in seq(1,ncol(prices)))
  {  #Die Farben der Trade-LinieSegmente
    wpCol= wpCols[i] #die Farben des longsegments sollen für jedes wp anders sein
    
    #head(signal["2009"])
    #head(weight["2009"])
    #browser()
    sig = signal[,i]
    #sig = weight[,i] #das Gewicht ist entscheident - nicht das Signal
    
    col = iif(sig <= 0, wpCol, "black")  #malen den dicken rand bund wenn du flat bist
    # browser()    
    plota.lines(prices[,i], type='l', col="black",lwd=ifelse(dick,5,3) ) 
    plota.lines(prices[,i], type='l', col=col,lwd=ifelse(dick,5,3) )  #plota.lines() kann auch linie segmente einfärben
    
    
    col = iif(sig > 0, wpCol, iif(sig < 0, 'red', 'yellow'))
    # browser()    
    plota.lines(prices[,i], type='l', col=col,lwd=ifelse(dick,3,1) )  #plota.lines() kann auch linie segmente einfärben
    
  }
  #browser()
  #unten rechts die Farblegende der Wertpapiere
  fill =c(wpCols,"black") 
  plota.legend(x="left",c(colnames(prices),"EQUITY"),,cex=0.9,fill=fill)
  
  #noch die equity-schattenkante aufpeppen
  lines(equity,type="l",col="darkorange",lwd=1)
  
  #noch die Ergebnisse einblenden
  res= modelResults(list(model))
  txt=unlist(lapply(res,function(item)sprintf("%s=%s",names(item),item)))
  
  
  plota.legend(x="top",labels=txt,ncol=2,bty="o",bg=col.add.alpha("grey" , 90),cex=0.7)
}


if (F)
  tradeResult(models,2, data, signal,frame="2007:2009")

###############################################################################
#wie vorher aber diesmal werden weight und  prices direkt gegeben 
###############################################################################
TradeResult<-function(name, weight, prices, signal,frame = "",dick=F,reset=F,sel=NULL) #MMA super
{
  
  #  names(models)
  print(name)
  
  fromTo(prices)
  prices = mNorm(prices)[frame]
  print (colnames(prices))
  signal = signal[frame]
  #entferne die NA
  signal[is.na(signal)] =0
  
  weight = model[frame]
  ret = ROC(data$prices[frame], type='discrete')
  ret[1] = 0
  # compute stats 
  ret = ret * signal
  equity = cumprod(1 + ret)     #der eignetlich Ertag  - muss noch durch ih
  
  #evtl nur für eine sup-liste von wertpapieren
  if (!is.null(sel))
  {
    prices=prices[,sel]
    signal = signal[,sel]
    weight=weight[,sel]
    equity=equity[,sel]
  }
  if (ncol(equity) ==0)
  {
    equity = prices[,1]
    equity[]=0
  }
  n = ncol(prices)
  
  wpCols=rainbow(ncol(weight)+1, start=0, end=.9) #die Farben der Wertpapiere
  
  colnames(equity) = "Equity"
  if (T)
    try(chart.RiskReturnScatter(mROC(merge(equity,prices)),  main = sprintf("RiskReturn since %s",name), colorset = wpCols))
  
  #sollen die Reihen alle links auch auf ich gleichen punkt 1,1 anfangen?
  if (reset)
  {prices = mNorm(prices[frame])-1
   
   equity = equity-as.numeric(first(equity)) 
  }
  #eine etwas harte Art sicher zu stellen, das der ganze Info-Block
  #auch auf einheitlichem x-Raum definiet ist:
  #nrow(info1       )
  
  info1 =  merge(prices,signal,weight,equity)
  info=info1[frame]
  #  info = info[!is.weekend(time(info))]
  
  #falls vorhanden  entferne die NA
  coredata(info)= bt.apply.matrix(coredata(info),m.ifna.prev)
  #mPlot(info,ylog_=F)
  #  browser()
  
  i=1; l=(i-1)*n+1; r = i*n
  prices=info[,c(l:r)] 
  i=2; l=(i-1)*n+1; r = i*n
  signal=info[,c(l:r)] 
  i=3; l=(i-1)*n+1; r = i*n
  weight=info[,c(l:r)] 
  i=4; l=(i-1)*n+1; r = l
  equity=info[,c(l:r)] 
  
  if (F)
  {
    colnames(info)
    first(prices)
    first(signal)
    first(weight)
    first(equity)
    
    len(prices)
    len(signal)
    len(weight)
    len(equity)
    last(equity)
    last(weight)
    last(info)
  }
  
  
  rwRange=range(weight)*ncol(weight)*100  #in der Regel: c( 0,100)
  
  #head(prices)
  #head(equity)
  #colnames(info)
  srange = range(na.omit(merge(equity,prices)))
  
  #range(info[,"Gold"])
  #range(info[,10])
  norm_Win(1)
  
  wpCols=rainbow(ncol(weight), start=0, end=.9) #die Farben der Wertpapiere
  wpColsLight = sapply(wpCols, function(x) {col.add.alpha(x , 50)})  #die ihrer Gewichte sollten "light" sein .. sonst bluten die Augen
  
  #Gewichte malen und equity als dunklen schatten
  plotbt.transition.map(weight,name, wpColsLight, plotSeries = equity,srange=srange)   
  
  #Equity malen
  #lines(scaleTo(equity,rwRange),lwd=2,type="h",col="blue")
  #lines(scaleTo(equity,rwRange),lwd=1,col="black")
  
  #Wp-Fette schwarz - als Rahmen 
  for (column in colnames(prices))
    #  lines(scaleTo(prices[,column],srange), type='l',col.add.alpha("grey" , 50),lwd=ifelse(dick,5,3))    
    lines(prices[,column], type='l',col.add.alpha("grey" , 50),lwd=ifelse(dick,5,3))    
  
  
  # lines(scaleTo(equity,range(weight)*100),col=col.add.alpha("grey" , 30),type="h")
  #einfärben der Preis-Zeitreihe nach Signal
  
  #jetzt die LongShortFlat-Segmente einfärben ...in dünn
  for (i in seq(1,ncol(prices)))
  {  #Die Farben der Trade-LinieSegmente
    wpCol= wpCols[i] #die Farben des longsegments sollen für jedes wp anders sein
    
    #head(signal["2009"])
    #head(weight["2009"])
    #browser()
    sig = signal[,i]
    #sig = weight[,i] #das Gewicht ist entscheident - nicht das Signal
    
    col = iif(sig <= 0, wpCol, "black")  #malen den dicken rand bund wenn du flat bist
    # browser()    
    plota.lines(prices[,i], type='l', col="black",lwd=ifelse(dick,5,3) ) 
    plota.lines(prices[,i], type='l', col=col,lwd=ifelse(dick,5,3) )  #plota.lines() kann auch linie segmente einfärben
    
    
    col = iif(sig > 0, wpCol, iif(sig < 0, 'red', 'yellow'))
    # browser()    
    plota.lines(prices[,i], type='l', col=col,lwd=ifelse(dick,3,1) )  #plota.lines() kann auch linie segmente einfärben
    
  }
  #browser()
  #unten rechts die Farblegende der Wertpapiere
  fill =c(wpCols,"black") 
  plota.legend(x="left",c(colnames(prices),"EQUITY"),,cex=0.9,fill=fill)
  
  #noch die equity-schattenkante aufpeppen
  lines(equity,type="l",col="darkorange",lwd=1)
  
  #noch die Ergebnisse einblenden
  res= modelResults(list(model))
  txt=unlist(lapply(res,function(item)sprintf("%s=%s",names(item),item)))
  
  
  plota.legend(x="top",labels=txt,ncol=2,bty="o",bg=col.add.alpha("grey" , 90),cex=0.7)
}


#*****************************************************************
# Visualization of system Entry and Exit based on
# http://timelyportfolio.blogspot.ca/2011/08/lm-system-on-nikkei-with-new-chart.html
#******************************************************************     

showIndicator<-function(price, signal,indicator=NULL, wpName="",frame="")
{
  if (wpName != "")
  {
    indicator = indicator[,wpName]
    price = price[,wpName]
  }
  if (frame !="")
  {
    indicator = indicator[frame]
    price = price[frame]
  }
  
  ret = ROC(price, type='discrete')
  ret[1] = 0
  # compute stats 
  ret = ret * lag(signal)
  eq = cumprod(1 + ret)     #der eignetlich Ertag  - muss noch durch ih
  
  
  layout(1)
  #  eq = lag(indicator)*price
  if (is.null(indicator))
    plota(eq, type='l', ylim=range(eq,price),main=name,LeftMargin=1)
  else
    plota(eq, type='l', ylim=range(eq,price),plotX=F,main=name,LeftMargin=1)
  
  col = iif(signal > 0, 'green', iif(signal < 0, 'red', 'gray'))   
  plota.lines(price, type='l', col=col)               
  plota.legend('strategy,Long,Short,Not Invested','black,green,red,gray')     
  
  if (!is.null(indicator))
    plota(indicator,type="l",col="black")
  
}    
#######################################################################################
###############################################################################
# Plot strategy perfromance side by side  	
#' @export 
###############################################################################
plotbt.strategy.sidebyside <- function
( 
  ... , 
  perfromance.metric = spl('System,Trade,Period'), 
  perfromance.fn = 'bt.detail.summary',
  return.table = FALSE,
  make.plot = TRUE,
  data=NULL   #patch
) 
{
  models = variable.number.arguments( ... )
  out = list()
  
  for( i in 1:len(models) ) {
    out[[ names(models)[i] ]] = match.fun(perfromance.fn)(models[[ i ]])[[ perfromance.metric[1] ]]
  }
  temp = list2matrix(out, keep.names=F)
 if (!is.null(data)) #patch
  temp= rbind(temp, Turnover= sprintf("%.1f",100*sapply(models, compute.turnover, data)))
  
  if(make.plot) plot.table( temp, smain = perfromance.metric[1] )
   
  if(return.table) return(temp)
}


##############################################################################################

strategy.Performance.snapshoot <- function(models, R.arg=list(report=spl("one,table,perfBar,transMap,turnover")),state="S", title = "strategy.performance.snapshoot",data=NULL,commission,redirectedOut="") {
  out=NULL
  par( cex = 0.7)
  
  if (exists("no_strategy.Performance.snapshoot") && no_strategy.Performance.snapshoot)
      return("no report because of no_strategy.Performance.snapshoot")
  #title = "strategy.performance.snapshoot"
  #layout(1:2)
  #plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
  #mtext('Cumulative Performance', side = 2, line = 1)
  #browser(mP("strategy.performance.snapshoot"))
  #models1=list(buyHold=models$buyHold,DAX=models$DAX,TF1=models$TF1)
#  browser()
  
#clr()
 mP("strategy.Performance.snapshoot %s",title)
textplot(title)  
#textplot(global_ParTable)#;page() #MM3

page()

norm_Win()

if (redirectedOut !="")
if (has(R.arg,"report","one",state=state))  #haupt-übersicht:  chart+tabelle
{
  norm_Win()
  
  layout(1:2)
  par( cex = 0.7)
  plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title, cex=0.7)
  mtext('Cumulative Performance', side = 2, line = 1)
  out = plotbt.strategy.sidebyside(models, return.table=T,data=data)
  page()
}

if (F && has(R.arg,"report","table",state=state))
    { 
    out = plotbt.strategy.sidebyside(models, return.table=T,make.plot=T)  #die übersichts-tabelle
  #m=names(models)[[1]]
  if (has(R.arg,"report","perfBar"))
    performance.barchart.helper(out, 'Sharpe,Cagr,VaR,MaxDD', c(T,T,T,T))
  page()
  }
  

if (has(R.arg,"report","transMap",state=state))
  {
    #pro seite nur 3 transMaps
    layout(1: min(3,len(models)))   #kann dazu führen, dass die Ausgabe nicht mehr geht
    m1=0
    for(m in names(models)) {
      m1=m1+1
      w1=dim(models[[m]]$weight)[2]
      mP("%s %d",m,w1)
#     browser()
      plotbt.transition.map(models[[m]]$weight, name=m)

if (data$BENCH %in% colnames(data$prices))
{
  lines( scaleTo(close=data$prices[,data$BENCH],c(0,100)))
  legend('topright', legend = m, bty = 'n')
}      
legend('topright', legend = m, bty = 'n')
      if ( m1 %% 3 == 0 && m1 < len(names(models)))
         page()
    }
page()
  }
  if (has(R.arg,"report","turnover",state=state))
  {
    layout(1)
    if (!is.null(data))
      barplot.with.labels(sapply(models, compute.turnover, data), sprintf( 'Average Annual Portfolio Turnover, commission %f',commission))
    page()
  }
  #browser(mP("strategy.performance.snapshoot"))
  
if (redirectedOut =="")
  if (has(R.arg,"report","one",state=state))  #haupt-übersicht:  chart+tabelle
  {
    norm_Win()
    
    layout(1:2)
    plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
    mtext('Cumulative Performance', side = 2, line = 1)
    out = plotbt.strategy.sidebyside(models, return.table=T,data=data)
    page()    
  }

  return(out) #patch
}

##############################################################################################

strategy.performance.snapshoot <- function(models, one.page = F, title = "strategy.performance.snapshoot",data=NULL) {
  #title = "strategy.performance.snapshoot"
  #layout(1:2)
  #plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
  #mtext('Cumulative Performance', side = 2, line = 1)
#browser(mP("strategy.performance.snapshoot"))
  #models1=list(buyHold=models$buyHold,DAX=models$DAX,TF1=models$TF1)

  if (exists("no_strategy.Performance.snapshoot") && no_strategy.Performance.snapshoot)
    return("no report")
  
  textplot(title)
  out = plotbt.strategy.sidebyside(models, return.table=T,make.plot=T)
#m=names(models)[[1]]
  if(!one.page)
  {
    performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,T))
    layout(1:len(models))
    for(m in names(models)) {
      plotbt.transition.map(models[[m]]$weight, name=m)
      
      if (data$BENCH %in% colnames(data$prices))
      {
        lines( scaleTo(close=data$prices[,data$BENCH],c(0,100)))
        legend('topright', legend = m, bty = 'n')
      }
      legend('topright', legend = m, bty = 'n')
    }
    layout(1)
    if (!is.null(data))
      barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
  }
  #browser(mP("strategy.performance.snapshoot"))

layout(1:2)
  plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
  mtext('Cumulative Performance', side = 2, line = 1)
  out = plotbt.strategy.sidebyside(models, return.table=T)
  
  return(out) #patch
}


# split plotbt.custom.report into 3 functions
plotbt.custom.report.part1 <- function
( 
  ..., 
  dates = NULL, 
  main = '', 
  trade.summary = FALSE,
  x.highlight = NULL,
  mm=1 ##patch
) 
{	
  layout(1:3)
  models = variable.number.arguments( ... )
  model = models[[mm]]
  
  #model = models[[1]]
  
  # Main plot
  plotbt(models, dates = dates, main = main, plotX = F, log = 'y', LeftMargin = 3, x.highlight = x.highlight)	    	
  mtext('Cumulative Performance', side = 2, line = 1)
  
  plotbt(models, plottype = '12M', dates = dates, plotX = F, LeftMargin = 3, x.highlight = x.highlight)	    	
  mtext('12 Month Rolling', side = 2, line = 1)
  
  plotbt(models, dates = dates, xfun = function(x) { 100 * compute.drawdown(x$equity) }, LeftMargin = 3, x.highlight = x.highlight)
  mtext('Drawdown', side = 2, line = 1)
}

plotbt.custom.report.part2 <- function
( 
  ..., 
  dates = NULL, 
  main = '', 
  trade.summary = T,
  x.highlight = NULL	,
  mm=1 #patch
) 
{	
  models = variable.number.arguments( ... )
  model = models[[mm]]
  
  # create layout	
  ilayout = 
    '1,3		
  2,4
  2,5'
  plota.layout(ilayout)
  
  
  # Additional Info
  plotbt.transition.map(model$weight, x.highlight = x.highlight)			
  temp = plotbt.monthly.table(model$equity)	
  m.plotbt.holdings.time(model$weight,smain=sprintf("%s %s",main,DateS(last(model$weight))))
  
  if ( !is.null(model$trade.summary) ) {
    plot.table( list2matrix(bt.detail.summary(model, model$trade.summary)), keep_all.same.cex = TRUE)		
  } else {
    plot.table( list2matrix(bt.detail.summary(model)), keep_all.same.cex = TRUE)
  }
  
  if( len(models) > 1 ) plotbt.strategy.sidebyside(models)
}	

m.plotbt.holdings.time <- function
(
  weight,
  smain = format(index.xts(last(weight)), '%d-%b-%Y')
)
{
  par(mar=c(2, 2, 2, 2), cex = 0.8, cex.main=0.8,cex.sub=0.8,cex.axis=0.8,cex.lab=0.8)
  icols=rainbow(ncol(weight), start=0, end=.9)
  temp = 100 * as.vector(last(weight))
  atemp = abs(temp)
  if(sum(atemp)>0) {
    pie(atemp, labels = paste(round(temp,0), '% ', colnames(weight), sep=''),
        col = icols, cex =0.8,
        main = paste('Allocation for ', smain, sep='')
    )
  }
}
plotbt.custom.report.part3 <- function
( 
  ..., 
  dates = NULL, 
  main = '', 
  trade.summary = FALSE,
  mm=1  #patch
) 
{	
  
  models = variable.number.arguments( ... )
  model = models[[mm]]
  
  if ( trade.summary & !is.null(model$trade.summary)) {	
    ntrades = min(20, nrow(model$trade.summary$trades))
    
    temp = last(model$trade.summary$trades, ntrades)
    if( ntrades == 1 ) temp = model$trade.summary$trades
    print( temp )
    print( model$trade.summary$stats )
    
    #layout(1)
    layout(c(1,rep(2,10)))
    
    # make dummy table with name of strategy		
    make.table(1,1)
    a = matrix(names(models)[1],1,1)
    cex = plot.table.helper.auto.adjust.cex(a)
    draw.cell(a[1],1,1, text.cex=cex,frame.cell=F)		
    
    plot.table( temp )	
    
  }	
}
#################################################################################################

###############################################################################
# Custom Backtest Report
###############################################################################
plotbt.custom.report <- function
( 
  ..., 
  dates = NULL, 
  main = '', 
  trade.summary = FALSE,
  x.highlight = NULL
) 
{	
  # create layout	
  ilayout = 
    '1,1
		1,1
		2,2
		3,3
		4,6
		4,6
		5,7
		5,8'
  plota.layout(ilayout)
  
  models = variable.number.arguments( ... )
  
  # Main plot
  plotbt(models, dates = dates, main = main, plotX = F, log = 'y', LeftMargin = 3, x.highlight = x.highlight)	    	
  mtext('Cumulative Performance', side = 2, line = 1)
  
  plotbt(models[1], plottype = '12M', dates = dates, plotX = F, LeftMargin = 3, x.highlight = x.highlight)	    	
  mtext('12 Month Rolling', side = 2, line = 1)
  
  plotbt(models[1], dates = dates, xfun = function(x) { 100 * compute.drawdown(x$equity) }, LeftMargin = 3, x.highlight = x.highlight)
  mtext('Drawdown', side = 2, line = 1)
  
  model = models[[1]]
  
  # Additional Info
  plotbt.transition.map(model$weight, x.highlight = x.highlight)			
  temp = plotbt.monthly.table(model$equity)	
  plotbt.holdings.time(model$weight)
  
  if ( !is.null(model$trade.summary) ) {
    plot.table( list2matrix(bt.detail.summary(model, model$trade.summary)), keep_all.same.cex = TRUE)		
  } else {
    plot.table( list2matrix(bt.detail.summary(model)), keep_all.same.cex = TRUE)
  }
  
  if( len(models) > 1 ) plotbt.strategy.sidebyside(models)
  
  if ( trade.summary & !is.null(model$trade.summary)) {	
    ntrades = min(20, nrow(model$trade.summary$trades))
    
    temp = last(model$trade.summary$trades, ntrades)
    if( ntrades == 1 ) temp = model$trade.summary$trades
    print( temp )
    print( model$trade.summary$stats )
    
    #layout(1)
    layout(c(1,rep(2,10)))
    
    # make dummy table with name of strategy		
    make.table(1,1)
    a = matrix(names(models)[1],1,1)
    cex = plot.table.helper.auto.adjust.cex(a)
    draw.cell(a[1],1,1, text.cex=cex,frame.cell=F)		
    
    plot.table( temp )
    
  }	
  modelResults(models )   #patch
}	


#############################################################################################



###############################################################################
#' Plot function for time series
#'
#' @param y \code{\link{xts}} object
#' @param main plot title
#' @param plotX flag to display X axis
#' @param LeftMargin to plot second Y axis, set LeftMargin=3, \strong{defaults to 0}
#' @param x.highlight segments to highlight along X axis, \strong{defaults to NULL}
#' @param y.highlight segments to highlight along Y axis, \strong{defaults to NULL}
#' @param las rotation of Y axis labels, \strong{defaults to 1}, for more info see \code{\link{par}}
#' @param type plot type, \strong{defaults to 'n'}, for more info see \code{\link{plot}}
#'  		also support 'ohlc', 'hl', 'candle', 'volume' types
#' @param xlab X label, \strong{defaults to ''}, for more info see \code{\link{plot}}
#' @param ylab Y label, \strong{defaults to ''}, for more info see \code{\link{plot}}
#' @param ylim range on Y values, \strong{defaults to NULL}
#' @param log log scale x, y, xy axes, \strong{defaults to ''}
#' @param ... additional parameters to the \code{\link{plot}}
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' # download data
#' data.spy = getSymbols('SPY', auto.assign = FALSE)
#' 	
#' # simple example candles and volume	
#' y = data.spy['2011:01:01::2011:02:01']
#' highlight = which(Cl(y) < 127)
#' 
#' # plot
#' layout(c(1,1,2))		
#' plota(y, type = 'candle', main = 'SPY', plotX = F, x.highlight = highlight)
#'   y = plota.scale.volume(y)
#' plota(y, type = 'volume', x.highlight = highlight)
#' }
#' @export 
###############################################################################
plota <- function
(
  y,					# xts object to plot
  main = NULL,		# plot title
  plotX = TRUE,		# flag to display X axis
  LeftMargin = 0,		# to plot second Y axis, set LeftMargin=3
  x.highlight = NULL,	# segments to highlight along X axis
  y.highlight = NULL,	# segments to highlight along Y axis
  las = 1,			# rotation of Y axis labels
  type = 'n',			# plot type
  xlab = '',			# X label
  ylab = '',			# Y label
  ylim = NULL,		# range on Y values
  log = '',			# log scale x, y, xy axes
  axis_value=4,  #patch - soll auch mal links  (2) sein können !
  col="black",
  ...					# other parameters to plot
)
{
  # set plot margins : bottom,left,top,right
  hasTitle = !is.null(main);
  par( mar = c(iif(plotX,2,0), LeftMargin , iif(hasTitle,2,0), 3) )
  #browser()  
  # set plot y range
  if(has.Cl(y)) y1 = Cl(y) else y1 = y[,1]
  if( is.null(ylim) ) {
    ylim = range(y1, na.rm = T)
    switch(type,
           'ohlc' = ,
           'hl' = ,
           'candle' = { ylim = range(OHLC(y), na.rm = T) },
           'volume' = { y1 = Vo(y); ylim = range(Vo(y), na.rm = T) }
    )
  }
  
  # create plot frame, do not plot data
  temp.x = attr(y, 'index')	
  plot( temp.x, y1, xlab = xlab, ylab = ylab, main = main,
        type = 'n', yaxt = 'n', xaxt = 'n', ylim = ylim, log = log, ... )
  
  # Y axis rotation in 90 degrees increments : las=0,las=1,las=2,las=3
  axis(axis_value, las = las)
  
  # plot X axis
  class(temp.x) = c('POSIXct', 'POSIXt')	
  plota.control$xaxis.ticks = axis.POSIXct(1, temp.x,labels = plotX, tick = plotX)
  
  
  # highlight logi
  if( !is.null(x.highlight) ) plota.x.highlight(y, x.highlight); 	
  if( !is.null(y.highlight) ) plota.y.highlight(y, y.highlight); 	
  
  # plot grid
  plota.grid() #MM2
  #browser()
  # plot data
  switch(type,
         'candle' = plota.candle(y, ...),
         'hl' = plota.hl(y, ...),
         'ohlc' = plota.ohlc(y, ...),
         'volume' = plota.volume(y, ...),
{  #browser();lines(temp.x, y1, type=type, ...) 
  #browser()
  if(type=="n") type="l"
  lines(temp.x,y1,type=type,col=col,...)
}
  )
  
  # plot box
  box();
}

###############################################################################
#' Plot time series with second Y axis
#'
#' @param y \code{\link{xts}} object
#' @param las rotation of Y axis labels, \strong{defaults to 1}, for more info see \code{\link{par}}
#' @param type plot type, \strong{defaults to 'n'}, for more info see \code{\link{plot}}
#'  		also support 'ohlc', 'hl', 'candle', 'volume' types
#' @param ... additional parameters to the \code{\link{plot}}
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' # download data
#' data.spy = getSymbols('SPY', auto.assign = FALSE)
#' data.ibm = getSymbols('IBM', auto.assign = FALSE)
#' 	
#' # two Y axis example
#' y = data.spy['2010:01:01::2011:02:01']
#' 				
#' # to plot second Y axis, free some space on left side, set LeftMargin=3
#' plota(y, type = 'ohlc', LeftMargin=3)
#' 			
#' y0 = y			
#' y = data.ibm['2010:10:15::2011:02:01']		
#' plota2Y(y, ylim = range(OHLC(y)),las=1, col='red', col.axis = 'red')
#'   plota.ohlc(y, col = 'red')		
#' plota.legend('SPY(rhs),IBM(lhs)', 'blue,red', list(y0,y))
#' }
#' @export 
###############################################################################
plota2Y <- function(
  y,			# xts object to plot
  las = 1,	# rotation of Y axis labels
  type = 'n',	# plot type
  axis_val= 2, #patch  .. damit die 2.Achse auch mal die rechte Achse  (4)sein kann  und nicht immer links (2) sein muss
  ...			# other parameters to plot
)
{
  # exctract visible plot data
  xlim = par('usr')[1:2]
  
  # subset	
  class(xlim) = c('POSIXct', 'POSIXt')
  y1 = y[paste(format(xlim, '%Y:%m:%d %H:%M:%S'), sep = '', collapse = '::')]	
  
  
  # plot
  par(new = TRUE)
  xlim = par('usr')[1:2]
  plot( attr(y1, 'index') , y1[,1], xlim = xlim, xaxs = 'i', type = type,
        yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', axes = F, ... )
  
  # Y axis rotation
  axis(axis_val, las = las, ...) 
}





plota.stacked <- function  #MM!
(
  x,				# x data
  y, 				# matrix with y data : len(x) = nrow(y)
  xlab='',		# x axis label	
  col = plota.colors(ncol(y)), 	# colors
  type=c('l','s'),# plot type  : lines, step stairs
  plotSeries = NULL, #patch  mal im plot nicht nen leeren xts - sondern gleich den mitgelieferten
  srange=srange , #patch
  ...			                	# other parameters for plot
)
{
  
  # transform y
  y = 100 * y
  
  y1 = list()
  y1$positive = y
  y1$positive[ y1$positive < 0 ] = 0
  
  y1$negative = y
  y1$negative[ y1$negative > 0 ] = 0
  
  # find y ranges
  ylim = c(min(rowSums(y1$negative, na.rm = T)), max(1, rowSums(y1$positive, na.rm = T)))
  
  # create empty plot
  # par(mar = c(4, 4, 2, 1), cex = 0.8)
  if( class(x)[1] != 'Date' & class(x)[1] != 'POSIXct') {
    plot(x, rep(0, len(x)), ylim = ylim, t = 'n', xlab = '', ylab = '', cex = par('cex'), ...)
    grid()  
    #MM1
  } else {
    #plot(x, rep(0, len(x)), ylim = ylim, t = 'n', yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', cex = par('cex'), ...)
    #	axis(2)
    #	xaxis.ticks = axis.Date(1, x, labels = T, tick = T)		
    #	
    #	abline( h = axTicks(2), col = 'lightgray', lty = 'dotted')
    
    #	abline( v = xaxis.ticks, col = 'lightgray', lty = 'dotted')		
    
  #browser(mP("pw"))      
    if (!is.null(plotSeries))
    { 
      #browser()
      #plot(make.xts(y[,1], x), ylim = srange)
      #par(new=TRUE)
      plota(make.xts(y[,1], x), ylim = srange, cex = par('cex'), LeftMargin = 4, axis_value=4,...)
      lines(plotSeries,type="h", col.add.alpha("grey" , 50),lwd=3)  ##schattiert die übergebene Zeitreihen malen
      plota.grid()       
      plota2Y(make.xts(y[,1], x), ylim = ylim, cex = par('cex'),axis_val=2,  ...)
      
    }
    else
    {
      
      plota(make.xts(y[,1], x), ylim = ylim, cex = par('cex'), LeftMargin = 4, axis_value=2,col=col,...) #patchM
      axis(2, las = 1) 
    }
    
    x = unclass(as.POSIXct(x))
  }
  
#  mtext('allocation % and positions ', side = 2,line = 3, cex = par('cex'))
  mtext(xlab, side = 1,line = 2, cex = par('cex'))		
  
  
  # plot stacked areas	
  if( type[1] == 'l' ) {
    prep.x = c(x[1], x, x[len(x)])     
    
    for( y in y1 ) {   	
      for (i in ncol(y) : 1) {
        prep.y = c(0, rowSums(y[, 1 : i, drop = FALSE]), 0)
        polygon(prep.x, prep.y, col = col[i], border = NA, angle = 90)
      }
    }
  } else {
    # http://r.789695.n4.nabble.com/how-to-fill-between-2-stair-plots-td819257.html
    dx = mean(diff(x))
    prep.x = c(rep(x,each=2), x[len(x)] + dx, x[len(x)] + dx)     
    
    for( y in y1 ) {   	
      for (i in ncol(y) : 1) {
        prep.y = c(0, rep(rowSums(y[, 1 : i, drop = FALSE]),each=2), 0)
        polygon(prep.x, prep.y, col = col[i], border = NA, angle = 90)
      }    
    }
  } 
  
  
  if (!is.null(plotSeries))
  { 
    plota2Y(plotSeries, ylim = srange, cex = par('cex'),axis_val=4,...)
  }
  
  # legend
  plota.legend(x="topright",colnames(y), col, cex = par('cex'))    
  
  
}


##########################

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


###############################################################################
# Plot Transition Map
###############################################################################
plotbt.transition.map <- function
(
  weight,
  name = '',
  col = rainbow(ncol(weight), start=0, end=.9),
  x.highlight = NULL  ,
  plotSeries = NULL, #patch  mal im plot nicht nen leeren xts - sondern gleich den mitgelieferten
  srange=NULL,
  plotX=T
) 
{
  
  par(mar=c(2, 4, 1, 1), cex = 0.8, cex.main=0.8,cex.sub=0.8,cex.axis=0.8,cex.lab=0.8)
  
  weight[is.na(weight)] = 0	
  #  plota.stacked(index.xts(weight), weight, col = col, type='s', main = iif(nchar(name) > 0, paste('Transition Map for', name), ''), x.highlight = x.highlight,plotSeries=plotSeries,srange=srange)	
 
  if (name =="")
    plota.stacked(index.xts(weight), weight, col = col, type='s', x.highlight = x.highlight,plotSeries=plotSeries,srange=srange,plotX=plotX)  
  else
    plota.stacked(index.xts(weight), weight, col = col, main=name,type='s', x.highlight = x.highlight,plotSeries=plotSeries,srange=srange,plotX=plotX)  
}



###############################################################################
# Public table drawing routines
###############################################################################
#' Plot Table
#'
#' Create Plot of the given matrix
#'
#' @param plot.matrix matrix to plot
#' @param smain text to draw in top,left cell
#' @param text.cex text size, \strong{defaults to 1}
#' @param frame.cell flag to draw border, \strong{defaults to TRUE}
#' @param highlight flag to highlight data, \strong{defaults to FALSE}
#' @param colorbar flag to draw colorbar, \strong{defaults to FALSE}
#' @param keep_all.same.cex flag to auto-adjust text size, \strong{defaults to FALSE}
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' # generate 1,000 random numbers from Normal(0,1) distribution 
#' data =  matrix(rnorm(1000), nc=10)
#'   colnames(data) = paste('data', 1:10, sep='')
#'   	
#' # compute Pearson correlation of data and format it nicely
#' temp = cor(data, use='complete.obs', method='pearson')
#'   temp[] = plota.format(100 * temp, 0, '', '%')
#' 		
#' # plot temp with colorbar, display Correlation in (top, left) cell	
#' plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)	
#' }
#' @export 
###############################################################################
plot.table <- function
(
  plot.matrix,				# matrix to plot
  smain = '', 				# text to draw in top,left cell
  text.cex = 1, 				# text size
  frame.cell = T, 			# flag to draw border
  highlight = F, 				# either flag to highlight or matrix with 
  # background colors
  colorbar = FALSE, 			# flag to draw colorbar
  keep_all.same.cex = F	# flag to auto-adjust text size
)
{
  # deal with missing col/row names
  if( is.null(rownames(plot.matrix)) & is.null(colnames(plot.matrix)) ) {
    temp.matrix = plot.matrix
    if( nrow(temp.matrix) == 1 ) temp.matrix = rbind('', temp.matrix)
    if( ncol(temp.matrix) == 1 ) temp.matrix = cbind('', temp.matrix)
    
    plot.matrix = temp.matrix[-1, -1, drop = FALSE]
    colnames(plot.matrix) = temp.matrix[1, -1]
    rownames(plot.matrix) = temp.matrix[-1, 1]
    smain = temp.matrix[1, 1]
    
  } else if( is.null(rownames(plot.matrix)) ) {
    temp.matrix = plot.matrix
    if( ncol(plot.matrix) == 1 ) temp.matrix = cbind('', temp.matrix)
    
    plot.matrix = temp.matrix[, -1, drop = FALSE]
    colnames(plot.matrix) = colnames(temp.matrix)[-1]
    rownames(plot.matrix) = temp.matrix[,1]
    smain = colnames(temp.matrix)[1]
    
  } else if( is.null(colnames(plot.matrix)) ) {
    temp.matrix = plot.matrix
    if( nrow(temp.matrix) == 1 ) temp.matrix = rbind('', temp.matrix)
    
    plot.matrix = temp.matrix[-1, , drop = FALSE]
    rownames(plot.matrix) = rownames(temp.matrix)[-1]
    colnames(plot.matrix) = temp.matrix[1, ]
    smain = rownames(temp.matrix)[1]
  }
  
  # remove N/As
  plot.matrix[which(trim(plot.matrix) == 'NA')] = ''
  plot.matrix[which(trim(plot.matrix) == 'NA%')] = ''
  plot.matrix[which(is.na(plot.matrix))] = ''
  
  # add space to the right if colorbar will be drawn
  if(colorbar) {
    plot.matrix = cbind(plot.matrix, '')
    if(!is.null(highlight)) if(!is.logical(highlight)) { highlight = cbind(highlight, NA) }
  }
  
  nr = nrow(plot.matrix) + 1
  nc = ncol(plot.matrix) + 1
  
  is_highlight = T
  if(is.logical(highlight)) { 
    is_highlight = highlight
    if(highlight) highlight = plot.table.helper.color(plot.matrix)
  }
  
  if(!is_highlight) {
    # default coloring scheme : alternate white/yellow each other row
    plot.matrix.cex = matrix(1, nr = nr, nc = nc )
    plot.matrix_bg.col = matrix('white', nr = nr, nc = nc )
    plot.matrix_bg.col[seq(1, nr, 2), ] = 'orange'
    plot.matrix_bg.col[1,] = 'gray';			
    
    plot.table.param( plot.matrix, smain, plot.matrix.cex, plot.matrix_bg.col, 
                      frame.cell, keep_all.same.cex)
  } else {
    plot.matrix.cex = matrix(1, nr = nr, nc = nc )
    plot.matrix_bg.col = matrix('white', nr = nr, nc = nc )
    plot.matrix_bg.col[1,] = 'gray'
    plot.matrix_bg.col[2:nr,2:nc] = highlight	
    
    plot.table.param(plot.matrix, smain, plot.matrix.cex, plot.matrix_bg.col, 
                     frame.cell, keep_all.same.cex)
  }
  
  if(colorbar) plot.table.helper.colorbar(plot.matrix);
}


#*****************************************************************
# Adjust portfolio leverage to given target volatility
#******************************************************************   			
target.vol.strategy <- function(model, weight, 
                                target = 10/100, 
                                lookback.len = 21,
                                max.portfolio.leverage = 100/100) 
{	
  ret.log.model = ROC(model$equity, type='continuous') #patch: na.omit
  ret.log.model[1,]=0.00001
  hist.vol.model = sqrt(252) * runSD(ret.log.model, n = lookback.len)	
  hist.vol.model = as.vector(hist.vol.model)
  
  weight.target = weight * (target / hist.vol.model)
  # limit total leverage		
  weight.target[is.na(weight.target)] <-0.000001   #patch
  
  #browser()
  #showNa(weight.target)
  
  rs = rowSums(abs(weight.target))
  weight.target = weight.target / iif(rs > max.portfolio.leverage, rs/max.portfolio.leverage, 1)		
  
  return(weight.target)	
}

###############################################################################
# lookupTabelle
###############################################################################

annualFactor<-function(period="days")
{
  p = list()
  
  #252 - days, 52 - weeks, 26 - biweeks, 12-months, 6,4,3,2,1
  p$days=252
  p$weeks=52
  p$biweeks=26
  p$months=12
  p$quarters=4
  p$halfyear=2
  
  ret=p[[period]]  
  if(is.null(ret))
  { 
    traceback()
    userStop("fehlbedienung in annualFactor %s",period)
  }
  # stopifnot(glob2rx("abc.*") == "^abc\\.",
  #               glob2rx("a?b.*") == "^a.b\\.",
  #       )
  return(ret)
}

###############################################################################
# Create Historical Input Assumptions given symbols and dates
###############################################################################
create.ia.mm.OLD <- function(data, period = 'months', dates = NULL)
{
  #--------------------------------------------------------------------------
  # Load historical prices and compute simple returns
  #--------------------------------------------------------------------------
  load.packages('quantmod,quadprog')
  
  #data <- new.env()
  #getSymbols(symbols, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
  #for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
  #bt.prep(data, align='remove.na', dates=dates)
  
  # convert to monthly frequency 
  hist.prices = data$prices
  symbols = colnames(data$prices)
  symbol.names = symbols
  period.ends = endpoints(hist.prices, period)
  hist.prices = hist.prices[period.ends, ]
  colnames(hist.prices) = symbol.names
  annual.factor <<- annualFactor(period)
  mP("create.ia.mm  - annual.factor is %d",annual.factor)
  browser()
  # compute simple returns	
  hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
  
  #--------------------------------------------------------------------------
  # Create historical input assumptions
  #--------------------------------------------------------------------------
  ia = create.historical.ia(hist.returns, annual.factor, symbol.names, symbol.names)
  print(ls(ia))
  return(ia)	
}



#*****************************************************************
# Portfolio Allocation Helper - distribute portfolio weights according to 
# the given weighting scheme (min.risk.fns)
#*****************************************************************
NO.portfolio.allocation.helper <- function
(
  prices,					# prices
  periodicity = 'weeks',	#  rebalancing frequency
  period.ends = endpoints(prices, periodicity),	# rebalancing times
  
  lookback.len = 60,		# lookback to construct input assumptions each period
  prefix = '',
  
  universe = prices[period.ends,]>0,
  
  min.risk.fns = 'min.var.portfolio',	# portfolio construction functions
  custom.stats.fn = NULL,
  shrinkage.fns = 'sample.shrinkage',	# covariance Shrinkage Estimator functions
  
  adjust2positive.definite = T,
  silent = F,
  
  log = log.fn(),	
  
  const.lb = 0, 
  const.ub = 1,
  const.sum = 1
) 
{
  load.packages('quadprog,corpcor')
  #*****************************************************************
  # Setup
  #*****************************************************************
  period.ends = period.ends[period.ends > 0]
  
  universe[is.na(universe)] = F
  
  if(len(const.lb) == 1) const.lb = rep(const.lb, ncol(prices))
  if(len(const.ub) == 1) const.ub = rep(const.ub, ncol(prices))
  
  #*****************************************************************
  # Transform min.risk.fns and shrinkage.fns to the named lists
  #*****************************************************************
  if(is.character(min.risk.fns)) {
    min.risk.fns = spl(min.risk.fns)
    names(min.risk.fns) = min.risk.fns
    min.risk.fns = as.list(min.risk.fns)
  }
  
  for(i in 1:len(min.risk.fns)) {
    f = spl(names(min.risk.fns)[i], '_')	
    f.name = paste(prefix, gsub('\\.portfolio', '', f[1]),sep='')
    
    if(is.character(min.risk.fns[[i]])) {			
      if(len(f) == 1) {
        min.risk.fns[[ i ]] = match.fun(f[1])
      } else {
        f.name = paste(f.name, f[-1], sep='_')
        min.risk.fns[[ i ]] = match.fun(f[1])(f[-1])
      }
    }
    names(min.risk.fns)[i] = f.name			
  }
  
  
  if(is.character(shrinkage.fns)) {
    shrinkage.fns = spl(shrinkage.fns)
    names(shrinkage.fns) = shrinkage.fns
    shrinkage.fns = as.list(shrinkage.fns)
  }
  #jetzt haben wir eine stringliste-von shrinkage.fns - mit namen...
  for(i in 1:len(shrinkage.fns)) {
    f = names(shrinkage.fns)[i]
    f.name = gsub('\\.shrinkage', '', f[1])
    
    if(is.character(shrinkage.fns[[ i ]]))
      shrinkage.fns[[ i ]] = match.fun(f)		
    names(shrinkage.fns)[i] = f.name			
    #jetzt haben wir ein liste wo der - um .shring gekürzte Name mit dem
    #funktionscode assoziiert ist.
  }
  
  
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 		
  dates = index(prices)[period.ends]
  
  prices = coredata(prices)
  ret = prices / mlag(prices) - 1
  #browser()
  #ret[which(ret==0)] = rnorm(1,0,sd = 0.00001)
  #ret[which(!is.finite(ret))]=rnorm(1,0,sd = 0.00001)
  
  start.i = which(period.ends >= (lookback.len + 1))[1]
  
  weight = NA * prices[period.ends,]
  weight[] = 0
  
  weights = list()			
  for(f in names(min.risk.fns)) 
    for(c in names(shrinkage.fns)) 
      weights[[ paste(f,c,sep='.') ]] = weight
  
  
  # custom stats logic		
  custom = list()
  if( !is.null(custom.stats.fn) ) {
    custom.stats.fn = match.fun(custom.stats.fn)
    
    dummy = matrix(NA, nr=nrow(weight), nc=len(weights))		
    colnames(dummy) = names(weights)
    dummy = make.xts(dummy, dates)	
    
    #temp = custom.stats.fn(1:ncol(ret), create.ia(ret))
    temp = ret
    temp[] = rnorm(prod(dim(ret)))
    temp = custom.stats.fn(1:ncol(ret), create.ia(temp))
    
    for(ci in names(temp)) {
      temp1 = NA * dummy
      if(len(temp[[ ci ]]) > 1) {
        temp1 = list()			
        for(w in names(weights)) 
          temp1[[w]] = NA * weights[[w]]   			   				
      } 
      custom[[ ci ]] = temp1
    }
  } 		
  
  
  # construct portfolios			
  for( j in start.i:len(period.ends) ) {
    i = period.ends[j]
    
    # histtory to construct input assumptions
    hist = ret[ (i- lookback.len +1):i, ]
    
    # require all assets to have full price history
    include.index = count(hist)== lookback.len      
    
    index = universe[j,] & include.index						
    n = sum(index)
    
    if(n > 0) {
      if(n > 1) {			
        hist = hist[ , index]
        hist.all = ret[ 1:i, index]		
        
        
        # 0 <= x.i <= 1
        constraints = new.constraints(n, lb = const.lb[index], ub = const.ub[index])
        constraints = add.constraints(diag(n), type='>=', b=const.lb[index], constraints)
        constraints = add.constraints(diag(n), type='<=', b=const.ub[index], constraints)
        
        # SUM x.i = 1
        if(!is.na(const.sum))
          constraints = add.constraints(rep(1, n), type = '=', b=const.sum, constraints)
        
        # create historical input assumptions
        #ia = new.env()
        ia = list()
        ia$index = index
        ia$n = n
        ia$hist.returns = hist
        ia$expected.return = apply(hist, 2, mean)				
        ia$risk = apply(hist, 2, sd)
        #browser()
        #head(hist)
        #patch
        #konstante - oder na- werte in der hist machen das shrinken schwer
        
        hist[which(!is.finite(hist)| hist==0)]=rnorm(len(hist[which(!is.finite(hist)| hist==0)]),0,sd =  0.000000001)  #schwaches rauschen drauf geben...
        
        ia$correlation = cor(hist, use='complete.obs', method='pearson')
        
        #ia$correlation[which(!is.finite(ia$correlation))]=rnorm(1,0,sd = 0.00000001)
        #ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
        
        #  mP("pre shrink")
        #browser()
        
        #das eigentlich shrinken
        for(c in names(shrinkage.fns)) {
          # ia$cov = try(shrinkage.fns[[c]](hist, risk, correlation))
          # browser()
          # print(c)
          ia$cov = try(shrinkage.fns[[c]](hist, hist.all))
          s0 = 1 / sqrt(diag(ia$cov))
          ia$correlation = ia$cov * (s0 %*% t(s0))
          
          
          # adjust correlation and covariance matrices to be positive defined
          if(adjust2positive.definite) {
            #browser()
            temp = try(make.positive.definite(ia$cov, 0.000000001), TRUE)	
            if(!inherits(temp, 'try-error')) ia$cov = temp				
            temp = try(make.positive.definite(ia$correlation, 0.000000001), TRUE)	
            if(!inherits(temp, 'try-error')) ia$correlation = temp							
          }
          
          # find optimal portfolios under different risk measures
          print("  find optimal portfolios under different risk measures")
          
          for(f in names(min.risk.fns)) {
            fname = paste(f,c,sep='.')				
            constraints$x0 = weights[[ fname ]][(j-1),index]			
            mP("calc weight for %s",f)
            weights[[ fname ]][j,index] = try(min.risk.fns[[f]](ia, constraints))
          }
        }							
      } else {
        for(c in names(shrinkage.fns)) {
          for(f in names(min.risk.fns)) {
            fname = paste(f,c,sep='.')				
            weights[[ fname ]][j,index] = 1
          }
        }			
      }
      
      # custom stats logic		
      if( !is.null(custom.stats.fn) ) {
        for(w in names(weights)) {
          x = as.vector(weights[[ w ]][j, index])
          temp = custom.stats.fn(x, ia)
          
          for(ci in names(temp)) {
            if(len(temp[[ ci ]]) > 1)
              custom[[ ci ]][[ w ]][j, index] = temp[[ ci ]]
            else
              custom[[ ci ]][j, w] = temp[[ ci ]]
          }
        }
      } 		
      
    }
    
    if( j %% 10 == 0) if(!silent) log(j, percent = (j - start.i) / (len(period.ends) - start.i))
    
  }
  
  if( len(shrinkage.fns) == 1 ) {
    names(weights) = gsub( paste('\\.', names(shrinkage.fns), '$', sep=''), '', names(weights) )
    for(ci in names(custom))
      names(custom[[ ci ]]) = gsub( paste('\\.', names(shrinkage.fns), '$', sep=''), '', names(custom[[ ci ]]) )		
  }
  
  
  return(c(list(weights = weights, period.ends = period.ends,
                periodicity = periodicity, lookback.len = lookback.len), custom))
}

#MM3
# Create strategies based on portfolio weights
NO.create.strategies <- function
(
  obj,	# portfolio.allocation object: list(weights = weights, period.ends = period.ends)
  data,	# historical prices
  leverage = 1,	
  
  silent = F,	
  log = log.fn(),	
  trade.summary=T, #patch
  commission=0.1,
  Tstops=NULL, #patch
  ...		
) 
{
  mP("------------------------ SitPatches :  create.strategies")
  if(len(leverage) == 1) leverage = rep(leverage, len(obj$weights))
  names(leverage) = names(obj$weights)
  
  for(i in names(obj$weights)) obj$weights[[i]] = leverage[i] * obj$weights[[i]]		
  
  #*****************************************************************
  # Create strategies
  #****************************************************************** 		
  models = list()
  n = len(names(obj$weights))
  for(j in 1:n) {
    i = names(obj$weights)[j]
    
    if(!silent) log(j, percent = j / n)
    
    data$weight[] = NA
    data$weight[obj$period.ends,] = obj$weights[[i]]	
    
    #  browser()
    #patch: clean.signal = T   ... sonst macht er viel zu viele Trades die nicht nötig sind (siehe buy&hold)
    
    #patch
    mP("createStrats")
    
    commission =0
    if (exists("global_commission"))
      commission = global_commission
    
    mP("commission is %f...............",commission)
    
    
    #clean.signal = T macht das exrem() .. also das Entfernen redundanter Transaktionen
    
    models[[i]] = bt.run.share(data, clean.signal = T, trade.summary=T,commission= commission, Tstops=Tstops, ...)
    
    #models[[i]]$trade.summary$trades
    #mb = bt.run.share(data, clean.signal=T,trade.summary = TRUE)
    #mb$trade.summary$trades
    #		models[[i]]$risk.contribution = obj$risk.contributions[[i]]
    models[[i]]$period.weight = obj$weights[[i]]
  }
  obj$models = models
  return(obj)			
}


#######################  demo-code aus sys.Run() #####################
if (F)################################################################
{
  n=dim(prices)[2] #anzahl der Wertpapiere
  commission =0
  if (exists("global_commission"))
    commission = global_commission
  
  capital=100000
  models = list()
  mdata = list()
  mdata$prices = prices
  mdata$symbolnames = pureColnames(prices)
  mdata$weight = prices; mdata$weight[]=NA
  colnames(prices)=colnames(signal)= mdata$symbolnames
  
  mP("sys.Run: %s",viewLevel) ; #browser()
  
  if (compare || contains(viewLevel,"portfolio")) #vergleich  mit der BuyHold - strategie gewünscht  (compare==visual)
  {
    #Das Buy-Hold-Vergleichsmodell
    mdata$weight[]=1
    
    #dim(mdata$weight)
    #dim(mdata$prices)
    #dim(signal)
    
    #patch  /n
    mdata$weight[] = ((capital/ n)  /   prices) * bt.exrem(mdata$weight)
    
    mdata$execution.price=prices;mdata$execution.price[]=NA
    mP("sys.Run--: -----Buy and Hold ----")
    models$buyHold = bt.run(mdata, type='share', capital=capital, trade.summary=F,commission=commission)
    
    #plot(models$buyHold$equity)
    #head(mdata$weight)
    #head(bt.exrem(mdata$weight))
    #plotbt.custom.report.part1(models$buyHold)
  }
  
  #  if (contains(viewLevel,"portfolio"))  #<<###################
  #    browser()  
  
  #das Modell für signal
  
  mdata$weight=signal
  mdata$weight[] = ((capital/n) / prices) * bt.exrem(mdata$weight) #exrem entfernt  zeitlich aufeinanderfolgende identische signale
  mdata$execution.price=prices;mdata$execution.price[]=NA
  mP("sys.Run:   -----model---- ")
  
  models$model = bt.run(mdata, type='share', capital=capital, trade.summary=T,commission=commission)
  
}
#############################################################################
##############################################################################

###############################################################################
# Helper function to backtest for type='share'
###############################################################################
NO.bt.run.share <- function
(
  b,					# enviroment with symbols time series
  prices = b$prices,	# prices
  clean.signal = T,	# flag to remove excessive signal
  
  trade.summary = F, 	# flag to create trade summary
  do.lag = 1, 		# lag signal
  do.CarryLastObservationForwardIfNA = TRUE, 	
  silent = F,
  capital = 100000,
  commission = 0,
  weight = b$weight,
  dates = 1:nrow(b$prices)	,
  Tstops=NULL #patch
) 
{
  # make sure that prices are available, assume that
  # weights account for missing prices i.e. no price means no allocation
  prices[] = bt.apply.matrix(coredata(prices), m.ifna.prev)	
  
  if(clean.signal) {
    weight[] = (capital / prices) * bt.exrem(weight)
  } else {
    weight[] = (capital / prices) * weight
  }
  
  bt.run(b, 
         trade.summary = trade.summary, 
         do.lag = do.lag, 
         do.CarryLastObservationForwardIfNA = do.CarryLastObservationForwardIfNA,
         type='share',
         silent = silent,
         capital = capital,
         commission = commission,
         weight = weight,
         dates = dates,
         Tstops=Tstops)	
}

###############################################################################
# Run backtest
#
# Inputs are assumed as if they were computed at point in time (i.e. no lags)
#
# For 'weight' back-test, the default action is to lage weights by one day,
# because weights are derived using all the information avalaible today, 
# so we can only implement these weights tomorrow:
#   portfolio.returns = lag(weights,1) * returns = weights * ( p / lag(p,1) - 1 )
# user can specify a different lag for weights, by changing the do.lag parameter.
#
# For example, for the end of the month strategy: if we open position at the close
# on the 30th, hold position on the 31st and sell it at the close on the 1st. If our
# weights have 0 on the 30th, 1 on the 31st, 1 on the 1st, and 0 on the 2nd, we
# can specify do.lag = 0 to get correct portfolio.returns
#
# Alternatively, if our weights have 0 on the 29th, 1 on the 30st, 1 on the 31st, and 0 on the 1nd, we
# can leave do.lag = 1 to get correct portfolio.returns
#
# For 'share' back-test, the portfolio returns:
#   portfolio.returns = lag(shares,1) * ( p - lag(p,1) ) / ( lag(shares,1) * lag(p,1) )
# 
###############################################################################
# some operators do not work well on xts
# weight[] = apply(coredata(weight), 2, ifna_prev)
###############################################################################
NO.bt.run <- function
(
  b,					# enviroment with symbols time series
  trade.summary = F, 	# flag to create trade summary
  do.lag = 1, 		# lag signal
  do.CarryLastObservationForwardIfNA = TRUE, 
  type = c('weight', 'share'),
  silent = F,
  capital = 100000,
  commission = 0,
  weight = b$weight,
  dates = 1:nrow(b$prices)	,
  Tstops=NULL  #Patch
) 
{
  # convert dates to dates.index
  dates.index = dates2index(b$prices, dates) 
  
  # setup
  type = type[1]
  
  # print last signal / weight observation
  if( !silent ) {
    cat('Latest weights :\n')
    print( last(weight) )
    cat('\n')
  }
  
  
  # create signal
  weight[] = ifna(weight, NA)
  
  # lag
  if(do.lag > 0) {
    weight = mlag(weight, do.lag) # Note k=1 implies a move *forward*  
  }
  
  # backfill
  if(do.CarryLastObservationForwardIfNA) {			
    weight[] = apply(coredata(weight), 2, m.ifna.prev)
  }
  weight[is.na(weight)] = 0
  
  
  
  ################################################################### 
  # find trades
  #mP("find Trades")
  #browser()
  weight1 = mlag(weight, -1)
  tstart = weight != weight1 & weight1 != 0
  tend = weight != 0 & weight != weight1
  trade = ifna(tstart | tend, FALSE)
  
  # prices
  prices = b$prices
  
  # execution.price logic
  if( sum(trade) > 0 ) {
    execution.price = coredata(b$execution.price)
    prices1 = coredata(b$prices)
    
    prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
    prices[] = prices1
  }
  
  
  # type of backtest
  if( type == 'weight') {
    ret = prices / mlag(prices) - 1
    ret[] = ifna(ret, NA)
    ret[is.na(ret)] = 0			
  } else { # shares, hence provide prices
    ret = prices
  }
  
  #weight = make.xts(weight, b$dates)
  temp = b$weight   #data$weight
  temp[] = weight
  weight = temp


  ################################ patch ############################
  
  #mP("üüüüüüüüüüüüüüüüüüüüüüüüüüü")
  #browser()
  if (!is.null(Tstops))
  {
    mP("---Daily-Stops ----- ")
    #auslösen der Stops
    #browser()
    
    vonBis =fromTo(weight)
    Tstops=Tstops[sprintf("%s::%s",vonBis[1],vonBis[2])]
    # ich erwarte dass jetzt dim(weight) == dim(Tstops) ist
    #dim(Tstops)
    #dim(weight)
    
    weight = weight * Tstops
    
    if (F && riskFreePos != "" && contains(colnames(weight),riskFreePos) )
    {
      mP("fill up exposure on %s ",riskFreePos)  #lohnt nicht . scheinbar ist mein riskFreePos nicht ohne Risiken
      weight[,riskFreePos]= weight[,riskFreePos]+(1-rowSums(weight[]))
      mchart(prices[,"TREASURY"])
    }
    
  }
  # prepare output   patch
  bt = bt.summary(weight, ret, type, b$prices, capital, commission)#, dates.index)
  bt$dates.index = dates.index 
  
  if( trade.summary ) bt$trade.summary = bt.trade.summary(b, bt)
  
  if( !silent ) {
    cat('Performance summary :\n')
    cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')  
    cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')  
    cat('\n')    
  }
  
  return(bt)
}

if (F)
{
  data$prices = na.omit(data$prices)
  ls(data)
  sma = m.apply(data, function(x) { SMA(Cl(x)) } )  
  mchart(sma)
  dv = m.apply(data, function(x) { DV(HLC(x)) } )
  data$weight = data$prices
  data$weight[] = NA
  data$weight[] = iif(prices > sma & dv < 0.25, 0.2, data$weight)
  data$weight[] = iif(prices < sma & dv > 0.75, 0, data$weight)
  data$weight[] = iif(prices < sma , 0, 1)
  
  data$weight$SHY = 0
  
  
  data$weight = bt.apply.matrix(data$weight, m.ifna.prev)
  data$weight$SHY = 1 - rowSums(data$weight)
  
  data$weight = bt.exrem(data$weight)
  
  capital = 100000
  data$weight[] = (capital / prices) * data$weight
  timing.d1 = bt.run(data, type='share', trade.summary=T, capital=capital)
}

##################################################################################################################


ifna.prev <- function(y)
{
  y1 = !is.na(y)
  y1[1]=T
  ret =y[cummax( (1:length(y)) * y1 )] 
  
  #  ret =make.index.unique(ret,drop=T) #patch!!!!
  return( ret )
}

#ist besser - kann auch multidim xts
m.ifna.prev <- function(y)
{
  return(na.locf(y,na.rm=FALSE,fromLast=F))
}
######################################################################################

NO.create.ia <- function(hist.returns, index=1:ncol(hist.returns))
{
  ia = list()
  
  #hist=bt.apply.matrix(hist,function(x) iif(x==0 ,runif(1,0,0.000002),x))
  #wenn ich einen im letzten Jahr constanten REX gebe krachs
 # browser(mP("patch ia"))
#  hist.returns[,"REX"] =0
#  hist.returns=na.omit(bt.apply.matrix(hist.returns,function(x) iif(x==0 & lag(x)==0,runif(shape(x),0,0.000001),x)))

  #mchart(hist.returns)
  ia$hist.returns = hist.returns
  
  ia$n = ncol(ia$hist.returns)
  ia$index = index
  ia$symbols = colnames(ia$hist.returns)
  ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
   
#  mP("ias %s",as.Date(index(index)))
#   if (as.Date("2013-01-31")==as.Date(index(index)))
#    browser( mP("BUG at create.ia") )
        
  ia$correlation = tryCatch(cor(ia$hist.returns, use='complete.obs',method='pearson'),
                            error=function( err ) FALSE, warning=function( warn ) FALSE )
  if( is.logical( ia$correlation ) ) 
    browser(mP("Bug at ia$correlation"))
  
  ia$cov = tryCatch(  ia$correlation * (ia$risk %*% t(ia$risk)),
                  error=function( err ) FALSE, warning=function( warn ) FALSE )
  if( is.logical( ia$cov ) ) 
     browser(mP("Bug at ia$cov"))
  ia$expected.return = apply(ia$hist.returns, 2, mean, na.rm = T)
  #browser(mP("create ia"))
  #ia$expected.return[,"DAX"]=ia$expected.return[,"DAX"]+ia$expected.return[,"DAX"]*0.1
  return(ia)
}
########################################################################

NO.ledoit.wolf.shrinkage <- function( hist, hist.all ) {
  require(BurStFin)
  

#  hist=bt.apply.matrix(hist,function(x) iif(x==0 ,runif(1,0,0.000002),x))
#  hist.all=bt.apply.matrix(hist,function(x) iif(x==0 ,runif(1,0,0.000002),x))
  
  
  res= tryCatch(  var.shrink.eqcor(hist, 1, compatible = T),
                      error=function( err ) FALSE, warning=function( warn ) FALSE )
  if( is.logical( res) ) 
  {
    browser(mP("Bug at ledoit.wolf.shrinkage"))
    #der fehler tritt auf, wenn die hist-returns 0 sind (wenn ich z.B den Rex mit 
    #0 für 2013 liefer -weil der wert fehlt)
   #res =factor.model.shrinkage (hist2,hist.all)
  }
  res
}

NO.factor.model.shrinkage <- function( hist, hist.all ) {
  require(BurStFin)
  factor.model.stat(hist, 1)
}
###########################


#*****************************************************************
# Monhtly returns for each model  
#*****************************************************************
custom.period.chart <- function(models) {   #patch mit page()
  for(imodel in 1:len(models)) {
    equity = models[[imodel]]$equity
    
    #*****************************************************************
    # Compute monthly returns
    #****************************************************************** 
    period.ends = endpoints(equity, 'months')
    period.ends = unique(c(1, period.ends[period.ends > 0]))
    
    ret = equity[period.ends,] / mlag(equity[period.ends,]) - 1
    ret = ret[-1]
    
    ret.by.month = create.monthly.table(ret)	
    ret.by.month = 100 * apply(ret.by.month, 2, mean, na.rm=T)
    
    #*****************************************************************
    # Compute annual returns
    #****************************************************************** 
    period.ends = endpoints(equity, 'years')
    period.ends = unique(c(1, period.ends[period.ends > 0]))
    
    ret = equity[period.ends,] / mlag(equity[period.ends,]) - 1
    ret.by.year = ret[-1]
    
    #*****************************************************************
    # Create plots
    #****************************************************************** 
    # create layout	
    ilayout = 
      '1,1
    2,2
    2,2
    2,2
    2,2
    2,2
    3,4
    3,4
    3,4
    3,4
    3,4
    3,4'
    plota.layout(ilayout)
    
    # make dummy table with name of strategy		
    make.table(1,1)
    a = matrix(names(models)[imodel],1,1)
    cex = plot.table.helper.auto.adjust.cex(a)
    draw.cell(a[1],1,1, text.cex=cex,frame.cell=F)		
    
    # plots
    temp = plotbt.monthly.table(equity)	
    
    # plot months
    cols = spl('green,red')
    custom.profit.chart(ret.by.month, 'Average Monthly Returns', cols)
    
    # plot years
    ret = 100*as.vector(ret.by.year)
    names(ret) = date.year(index(ret.by.year))
    custom.profit.chart(ret, 'Annual Returns', cols)
    page()
  }
}	
##########################################################################
plota.legend <- function
(
  labels,
  fill = NULL,
  lastobs = NULL,
  x = 'topleft',
  merge = F,
  bty = 'n',
  yformat = plota.format,
  pch=0.5,cex=0.7, y.intersp=0.8, #patch
  ...
)
{
  if( !is.null(fill) ) fill = spl( as.character(fill) )
  labels = spl( as.character(labels) )
  if( !is.null(lastobs) ) {
    if( is.list(lastobs) ) {
      labels1 = sapply(lastobs, function(x) unclass(last(x))[1])
    } else {
      labels1 = unclass(last(lastobs))[1];
    }
    labels = paste(labels, match.fun(yformat)( labels1 ))
  }
  legend(x, legend = labels, fill = fill, merge = merge, bty = bty, pch=pch,cex=cex,y.intersp=y.intersp,...)
}
###############################################################################
# Split string into tokens using delim
###############################################################################
spl <- function
(
  s,      # input string
  delim = ','  # delimiter
)
{ 
  if (len(s)< 1) return(NULL)
  return(trim(unlist(strsplit(s,delim)))); #patched 
}

#####################################################################################
print(" ############# load SITpatches.R")

if (F)
  list_R_functions('MLib/SITpatches.R')
