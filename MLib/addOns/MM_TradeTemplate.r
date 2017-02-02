#MM_MAIN    Hier bau ich mein Aktien/Renten - Allocationssystem

#mach voher mmBasicStartup.r
#Einführung in dei Lib des SysInvestors

if (!exists("MM_lesson1"))
{
  rm(list=ls())
  print("### load BasicLib ###")
  source("MLib/portfolioCode.r")
  source("MLib/LoadDataFuncs.r")
  source("MLib/TradeClasses.r")
}

options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)

#source("MLib/SITpatches.r")  #mein Anpassungen dazu

if (F)  #falls reinit- gewünscht anklicken
  rm(MM_lesson1)

MM_lesson1="ok"
#-----------------------------------------------------------------------------------------------
ModelRoot = "O:/R/Nuggets/Eckhard" 
setwd(ModelRoot)

testXLSread<-F
readMyClasses<-T

#http://ichart.finance.yahoo.com/table.csv?s=%5EHSI&a=11&b=31&c=1986&d=03&e=3&f=2013&g=d&ignore=.csv

#http://chart.yahoo.com/table.csv?s=%5EHSI&a=0&b=01&c=1975&d=3&e=03&f=2013&g=d&q=q&y=0&z=^DJI&x=.csv 

globalPROVIDER = "CSV"
csvDatFormat="%Y-%m-%d"
globalSEP = ";"
globalDEC = "."
globMDataSUB=""

lookback.len = 60
#periodicity = 'weeks'
periodicity = 'months'
prefix = ''
dataSet = "MM_Feb1"#modelDir
myFrame =""

##########################################################################
##########################################################################

##############################################################################


asset <- function()  #MMA
{   
  #*****************************************************************
  # Load historical data
  #****************************************************************** 
  #load.packages('quantmod')  
  #tickers = spl('SPY')
  
  #data <- new.env()
  #getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
  #for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)      
  #bt.prep(data, align='keep.all', dates='1970::')	
  
  AA <<- T2$new(PortfolioName = "AA", visual = T, online=F )#bench="Dax",
  data = AA$t0data
  
  
  #*****************************************************************
  # Code Strategies
  #******************************************************************   
  
  prices = data$prices  
  n = ncol(prices)
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  tickers =AA$tickList 
  prices = data$prices   
  
  periodicity = "days"
  period.annual.factor=  252#, 52, 12  
  period.ends = endpoints(prices, periodicity)
  period.ends = period.ends[period.ends > 0]
  
  models = list() #nimmt die Ergebnisse auf
  
  nperiods = nrow(prices)
  
  #*****************************************************************
  # Buy & Hold
  #****************************************************************** 
  signal =  iif(prices >=0, 1, 0)
  position.score = signal
  #position.score$Rex = NA   #scores=NA bekommen von ntop gar nichts
  
  # Equal Weight
  #jeder Zeile der übergeben matrix wird angeschaut
  # die werten werden in der Zeile sortiert (aber NA wird ignoriert)
  #die besten n Werte bekommen den Wert 1/n zugewiesen - so das die
  #Summe der Gewichte pro Zeile immer 1 ist
  weight = ntop(position.score[period.ends,], n)  
  
  #plotbt.transition.map(weight,"target", c(col.add.alpha("blue" , 50),col.add.alpha("red" , 50),c(col.add.alpha("yellow" , 50))))
  
  
  #abbilden des Targetportfolios (weight) auf das Real-Portfolio (data$weight)
  data$weight[] = weight
  #data$execution.price[] = NA
  models$buy.hold = bt.run.share(data, clean.signal=T)  #hier macht sich das Gold breit !!!
  #models$buy.hold = bt.run(data)
  
  #new_Win(1)
  #tradeResult(models,1, data, signal)
  #plotbt.custom.report(models$buy.hold, trade.summary = TRUE)  	
  plotbt.custom.report.part1(models$ma.crossover2, trade.summary = TRUE)
  
  
  #*****************************************************************
  # MA cross-over strategy   #MMA
  #
  #  wenn dax und rex long sind kommen beide zum zuge.
  #wenn nur einer long ist kriegt der alles
  #****************************************************************** 
  sma.fast = bt.apply.matrix(prices, SMA, 50)
  sma.slow = bt.apply.matrix(prices, SMA, 200)
  signal =  iif(sma.fast >= sma.slow, 1, NA)  #mit dem NA sag ich, dass er für eine  Allocation nicht zur Verfügung steht !!
  
  norm_Win(3)
  plotWeight(signal)
  plotSigPrice(signal,prices,"Dax") #SuP500
  
  position.score = signal  ##evtl. noch mehr Einflussfaktoren?
  
  #A=as.matrix(position.score[period.ends,])
  
  #aus dem scorering ein Target-Allocation machen
  #sorgt dafür, dass jetzt alles was nicht ausgestoppt ist auf 100 investiert wird.
  weight =  ntop(position.score[period.ends,], n)
  plotWeight(weight)
  #weight2=apply(weight,2,exrem)
  
  #die TargetAllocation aufs Realportfolio abbilden
  data$weight=bt.exrem(weight) #kürze identische Positionen-sonst kommt der Tradezähler nur noch bei clean.signal=T zurecht.
  
  data$weight=weight
  
  #models$ma.crossover2 = bt.run(data, trade.summary = TRUE)
  models$ma.crossover2 = bt.run.share(data, clean.signal=T,trade.summary = TRUE)
  
  ######################################
  
  head(models$ma.crossover2$trade.summary$trades,30)
  
  
  tradeResult(models,len(models), data, signal,reset=T)  
  
  
  
  
  
  
  
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 	
  # put all reports into one pdf file
  #pdf(file = 'report.pdf', width=8.5, height=11)
  models = rev(models)
  
  
  #png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
  
  # Plot perfromance
  plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
  mtext('Cumulative Performance', side = 2, line = 1)
  
  dev.off()				
  
  png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
  
  # Plot trades
  plotbt.custom.report.part3(models$ma.crossover, trade.summary = TRUE)		
  
  dev.off()					
  png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
  
  plotbt.custom.report.part3(models$ma.crossover.enter.next.open, trade.summary = TRUE)		
  
  dev.off()			
  #dev.off()	
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #*****************************************************************
  # Simple example showing the difference in a way commission is integrated into returns
  #****************************************************************** 	
  commission = 4
  data$weight[] = NA
  data$execution.price[] = NA
  data$weight[201,] = 1
  data$weight[316,] = 0				
  data$execution.price[201,] = prices[201,] + commission
  data$execution.price[316,] = prices[316,] - commission		
  models$test.com = bt.run.share(data, clean.signal=T, trade.summary=T)
  
  data$weight[] = NA
  data$execution.price[] = NA
  data$weight[201,] = 1
  data$weight[316,] = 0					
  models$test.com.new = bt.run.share(data, commission=commission, trade.summary=T, clean.signal=T)
  
  cbind(last(models$test.com$equity), last(models$test.com.new$equity),
        as.double(prices[316] - commission)/as.double(prices[201] + commission))
  
  as.double(prices[202]) / as.double(prices[201] + commission)-1
  models$test.com$equity[202]-1
  
  as.double(prices[202] - commission) / as.double(prices[201])-1
  models$test.com.new$equity[202]-1
  
  
  #plotbt.custom.report.part1(models)
  
  #*****************************************************************
  # Example showing the difference in a way commission is integrated into returns
  #****************************************************************** 		
  commission = 0.1
  sma.fast = SMA(prices, 50)
  sma.slow = SMA(prices, 200)
  
  weight = iif(sma.fast >= sma.slow, 1, -1)	
  weight[] = bt.exrem(weight)
  index = which(!is.na(weight))
  trade.start = index+1		
  trade.end = c(index[-1],nperiods)
  trade.direction = sign(weight[index])
  
  
  data$weight[] = NA
  data$execution.price[] = NA
  data$weight[] = weight
  models$test.com.new = bt.run.share(data, commission=commission, trade.summary=T, clean.signal=T)
  
  
  data$weight[] = NA
  data$execution.price[] = NA
  
  index = which(trade.direction > 0)
  data$execution.price[trade.start[index],] = prices[trade.start[index],] + commission
  data$execution.price[trade.end[index],] = prices[trade.end[index],] - commission
  
  index = which(trade.direction < 0)
  data$execution.price[trade.start[index],] = prices[trade.start[index],] - commission
  data$execution.price[trade.end[index],] = prices[trade.end[index],] + commission
  
  data$weight[trade.start,] = trade.direction
  data$weight[trade.end,] = 0		
  
  models$test.com = bt.run.share(data, clean.signal=T, trade.summary=T)
  
  
  #plotbt.custom.report.part1(models)
  
}

##########################################################################################
# Berechne ein neues Model name und lege seine Ergebnisse in der Liste models ab
#
##########################################################################################
testSystem<-function(models, data,name,prices,signal,visual=T)
{
  mP("testSystem %s",name)
  
  if (len(models)<1)  #erst mal buy and hold (equal weight) machen
  {
    osignal = signal
    print("buy_hold as Benchmark")  
    signal =  prices
    signal[]=1
    position.score = signal
    
    # browser()    
    
    weight = ntop(position.score[period.ends,], n)  
    data$weight[] = weight
    models[["buy_hold"]] = bt.run.share(data, clean.signal=T)  #hier macht sich das Gold breit
    colnames(models[["buy_hold"]]$equity)=paste("buy_hold","Eq",sep=".")
    #  modelResults(models[["buy_hold"]])
    signal = osignal
  }
  
  ############################ jetzt aus den gelieferten signalen ein Portfolio machen
  if (visual)
  {
    norm_Win(3)
    for(col in colnames(prices))   
      plotSigPrice(signal[,col],prices[,col],col) #SuP500
    
    userStop("Signal")
  }
  position.score = signal  ##evtl. noch mehr Einflussfaktoren?
  #browser()
  #A=as.matrix(position.score[period.ends,])
  
  #aus dem scorering ein Target-Allocation machen
  #sorgt dafür, dass jetzt alles was nicht ausgestoppt ist auf 100 investiert wird.
  
  #equal weight für alle .. bis auf die augestoppten   MM_TODO
  weight =  ntop(position.score[period.ends,], n)
  
  if (visual)
  {
    norm_Win(1)
    
    plotWeight(weight)
    userStop("TargetWeight")
  }
  #weight2=apply(weight,2,exrem)
  
  #die TargetAllocation aufs Realportfolio abbilden
  #data$weight=weight
  
  data$weight=bt.exrem(weight) #kürze identische Positionen-sonst kommt der Tradezähler nur noch bei clean.signal=T zurecht.
  
  
  #models$ma.crossover2 = bt.run(data, trade.summary = TRUE)
  newMod = bt.run.share(data, clean.signal=T,trade.summary = TRUE)
  models[[name]] = newMod
  
  colnames(models[[name]]$equity)=paste(name,"Eq",sep=".")
  ######################################
  #die liste mit den Trades ...
  
  #head(models$ma.crossover2$trade.summary$trades,30)
  
  if (visual)
    tradeResult(models,name, data, signal,reset=T)  
  else
    modelResults(models)
  
  if (visual)  
  {mP(">>> Ready with %s ",name)
   userStop()
  }
  return (models)
}


### ein mini-rsi-trading-system - ohne Stop
### für viele Aktien gleichzeitig
sys.RSI<-function(arg)
{
  wlen=arg[1]; aT=arg[2]
  mP("sys.RSI:  wlen %f aT %f", wlen,aT)
  rsi <-bt.apply.matrix(prices,"RSI",wlen)
  sig <- iif(rsi > aT, 1, -1)
  ret <- m.Run(prices,sig)
  n=  numTrades(sig)$allT
  q= quality(ret)
  
  Transaktionskosten =n/ncol(prices)/80
  mP("sys.RSI:  wlen %f aT %f %d Trades- >%f =>%f", wlen,aT,n,q,Transaktionskosten)
  
  return(q-Transaktionskosten)
}

#### sehr einfacher Optimierer -- vollständige Suche
optimize.Grid.RSI<-function()
{
  lower <- c(2,30); upper <- c(70,80); npar <- 2
  res <- gridSearch(sys.RSI, lower = lower, upper = upper, npar = npar)
  besti=which.max(res$values)
  best=res$values[besti]
  mP("BestVal %f",best)
  print(res$levels[besti])
  return(best)  
}

if (F)
  optimize.Grid.RSI()


#dvi <- mt.apply.matrix(prices, "DVI","DVI")
#signal = iif(dvi < 0.5 , 1, NA)         

### in meinem Framework:
indi.RSI<-function(arg,par = mlist(rsiL=2, rsiThres=50))
{
  #  rsi2 = bt.apply.matrix(prices, RSI, 2)  
  rsi2 = RSI(arg$clos, par$rsiL)  
  signal= iif(rsi2 < par$risThres, 1, -1)  
  return(indi.finish(signal,arg))
}



try_signals<-function(models,data, visual=F)
{
  
  sma.fast = bt.apply.matrix(prices, SMA, 50)
  sma.slow = bt.apply.matrix(prices, SMA, 200)
  signal =  iif(sma.fast >= sma.slow, 1, NA)  #mit dem NA sag ich, dass er für eine  Allocation 
  models=testSystem(models, data, "sma_50",prices,signal,visual=visual)
  
  
  #signal <- Lag(ifelse(dvi$DVI < 0.5 , 1, -1))         
  #sigRsi <- Lag(ifelse(rsi < 50, 1, -1))   
  #SAR -> Trainling Stop
  #DonchianChannel
  #SMA vers schnellem SMA
  #http://www.cfd-portal.com/indikatoren/atr-average-true-range/
  
  #prices=na.omit(prices)
  #DVI(prices[,"Dax"])[["DVI"]]
  dvi <- mt.apply.matrix(prices, "DVI","DVI")
  signal = iif(dvi < 0.5 , 1, NA)         
  models= testSystem(models,data, "DV",prices,signal,visual=visual)
  
  rsi <-bt.apply.matrix(prices,"RSI",80)
  signal <- iif(rsi > 53, 1, NA)
  models=testSystem(models, data, "RSI",prices,signal,visual=visual)
  
  # compute turnover  
  #models = variable.number.arguments(timing.d1, timing.d, timing, equal.weight)
  
  return (models) 
}

#Turnover=function(w) period.annual.factor * mean(portfolio.turnover(w), na.rm=T)


if (F)
  prices = ta5$t0data$prices

#### Zeite 
test_AllModels<-function(data, visual =T)
{
  models<<-list()
  visual<<-visual
  models<-try_signals(models,data, visual)
  
  compareViewModels(models)
}



if (F)
{
  periodicity = "days"
  period.annual.factor=  252#, 52, 12  
  period.ends = endpoints(prices, periodicity)
  period.ends = period.ends[period.ends > 0]
  
  models = list() #nimmt die Ergebnisse auf
  
  AAgold <<- T2$new(PortfolioName = "AAgold", bench="Dax",visual = F ) 
  AAgold
  prices = AAgold$t0data$prices
  data = AAgold$t0data
  visual =F 
  
  nperiods = nrow(prices)
  n =ncol(prices)
  
  test_AllModels(AAgold$t0data,visual =F)  
}


#########################################################################
#baut eine Segmentierung mit grafischer Animation und quality-Bewertung -
# kann in dieser Form von optimisern aufgerufen werden
##########################################################################
profit_segmentierung<-function(arg,visual =F) #MM_TODO da kommen komische segmentierungen
{
  a=arg[1]
  ds =DdownDupWalkAhead(Dax,a,a,visual=F)
  #dz = mZigZag(Dax,a)
  ret =mROC(ds$peak)
  
  if (visual)
  {
     norm_Win(1)
    plot(Dax)
    lines(ds$peak,col="red",lwd=2)
    lines(ds$brk, type="h",col="blue")
    ret =mROC(ds$peak)
    lines(mRendite(ret),col="orange")
  }
  q= quality(ret)
  mP("%f  ->> %f",a,q)
  #userStop(" wait ")
  return(q)
}

if (F)
{
  lower <- c(2); upper <- c(20); npar <- 1
  res <- gridSearch(profit_segmentierung, lower = lower, upper = upper, npar = npar)
  
}



#######################################################################
#
#######################################################################

findBestSegmentation<-function()
{
  ########### finde die beste Segmentierung (blance zwischen wegstrecke und transaktionskosten)
  res <-sapply(seq(1:30),profit_segmentierung)
  plot(res)
  bestDD=first(which(res==max(res)))
  print(bestDD)
  #res=mZigZag(Dax,bestDD)
  ds =DdownDupWalkAhead(Dax,bestDD,bestDD,visual=F)
  res =ds$peak
  brk =ds$brk
  tstop=TrailingStop(Dax,res,bestDD, brk=brk)
  t =make.xts(tstop) 
  
  plot(Dax);lines(res,col="blue");
  lines(tstop,col="red")  
}


#######################################################################
#Wird von TrailingStop für jedes Dax-Segment aufgerufen
#######################################################################
trailStop<-function(win,dax=0, DD =0,xshift=0)
{
  #Das Zeitreihensegment untersuchen
  #win ist eine data.frame - Zeile mit Argumenten
  x1 = as.numeric(win[1])  
  x2 = as.numeric(win[2])  #x2 ist nach rechts geshiftet damit man den Break sieht
  segi = win[3]  #im wievielten segment sind wir
  
  x1Org = win[4]   #der Trendwechselzeitpunkt
  x1Brk = win[5]   #viel später kam der Zeitpunkt wo man über trailingstop den break erlebt hat
  x2Peak = win[6]
  
  dax_=dax[ x1:x2 ]
  y1 =as.numeric(dax[x1])
  y2 =as.numeric(dax[x2])
  
  up =  y1<=y2 
  cat(toString(as.Date(index(dax[x1]))),x1,x1Org,y1,as.Date(x2),y2)
  
  if (up)
  {
    best = runMax(dax_,n=1,cumulative=T) #oben blau
    best[1]=best[2]
    ts = best-DD*best/100
    
    head(best)
    worstDistance = runMax(-dax_+best,n=1,cumulative=T)
    tw = best -worstDistance #green
  }
  else
  {
    best = runMin(dax_,n=1,cumulative=T)
    best[1]=best[2]
    ts = best+DD*best/100
    
    worstDistance = runMax(dax_-best,n=1,cumulative=T)
    tw = best +worstDistance #green
  }
  #Innerhalb des Trendsegments:
  if (xshift !=0) #ermöglicht eine genaue Analyse der Trendwechselstellen
  {
    mark = dax_[x1Org];mark[]=max(dax_)
    plot(dax_,main = sprintf("%s %s",colnames(dax_[1]),segi))
    
    #plota2Y(dax_,type="ohlc",main = sprintf("%s %s",colnames(dax_[1]),segi))
    lines(dax,lwd=2)
    abline(v=as.POSIXct(as.Date(index(mark))),col=col.add.alpha("orange" , 80),lwd=5)
    abline(v=as.POSIXct(as.Date(x1Brk)),col=col.add.alpha("grey" , 80),lwd=5)
    abline(v=as.POSIXct(as.Date(x2Peak)),col=col.add.alpha("red" , 80),lwd=5)
    
    browser()
    
    #local.trend(dax)
    
    #schau Dir 3 punkte im segment genauer an:
    for (i in c(len(dax_)/2,xts2Index(x2Peak,dax_),xts2Index(x1Brk,dax_)))
    {
      plot(dax_)
      abline(v=index(dax_[i]),col="blue")
      browser()
      head(dax_)
      #i=len(dax_)
      sdax=dax[1:i]
      dts = ts(sdax)
      sample=sdax
      ts.sample = ts(to.monthly(sample), frequency = 12)
      
      
      if (F)
      {
        library(timsac)
        z=decomp(dts)                    #hier ist der seasonal anteil interessant: das sägeblatt
        
        tssdax = ts(sdax)
        fit <- tslm(dts ~ trend  )
        plot(forecast(fit, h=20))
        
        fit <- auto.arima(dts)
        plot(forecast(fit, h=20))
        
        fit <- HoltWinters(dts,gamma=FALSE)  #nicht schlecht
        plot(forecast(fit, h=20))
      }
      
      mP("next i")
      browser()
    }
  }
  else
    plota(dax_,main = sprintf("%s %s",colnames(dax_[1]),segi))
  
  #ne Points-Betrachtung ist immer hilfreich  
  hl= HighLows(dax_,2,visual=F)
  points(dax_[hl$highs],col="red",lwd=1)
  points(dax_[hl$lows],col="blue",lwd=1)
  
  
  lines(best,col="blue"); 
  lines(ts,col="red") #die harte stop-linie mit der auch die segmentierung erzeugt wurde.
  if (!is.null(globLast ))
    lines(globLast,col="magenta")
  lines(tw,col="green")  #die grüne stop-linie schmiegt sich an, reisst aber nicht sondern weicht nach unten aus - erst die Rote reisst dann.
  
  globLast <<- ts
  print("   press key   ")
  browser()
  
  return(ts)
}

########################################################################
# res ist ein supzeitreihe von Dax - und segmentiert diese
# für jedes dieser Segmente wird nun trailStop -aufgerufen
#mit xshift kann ich die Segmente aber auch verschieben und mir so 
#geziehlt die Wendepunkte anschauen 
########################################################################
TrailingStop<-function(Dax,ds,DD,xshift=0)
{
  res =ds$peak
  x1Brk =ds$brk
  
  #res enthält die Segment-Zeitreihe.. mach da erst mal x1, x2 .. folgen raus
  x1=  as.Date(index(res))
  x2 = x1[-1] 
  x2Peak = x2
  x1 = x1[-len(x1)]
  #geshifted wird, um die wichtigen Trendwechsel mehr in der Mitte zu sehen
  x1Org = x1  #peak
  
  x1Brk = c( as.Date(index(ds$brk)), NA)
  #x1 = sapply(x1,xtsAddDate,Dax=Dax,xshift=xshift)  #von
  x1 =sapply(x1,xtsAddDate,Dax=Dax,xshift=0) #links nicht schiften
  x2 = sapply(x2,xtsAddDate,Dax=Dax,xshift=xshift)  #den rechten Rand rausschieben
  
  
  # x1 = x1+xshift   geht nicht, weil es zu diesem Datum womöglich keinen Kurs in Dax gibt
  #von-bis- datümer der res-intervalle
  #über eine data.frame kann ich apply einen ganzen Set von argumenten mitgeben
  Win=data.frame( x1,x2, segi=c(1:len(x1)),x1Org ,x1Brk,x2Peak)  #gib auch die segmentnummer mit
  #browser()
  
  #plot(Dax)
  plot(Dax)
  lines(res,col="orange",lwd=2)
  globLast <<- NULL
  
  #jedes Segment separat analysieren
  tsl =apply(Win,1, trailStop, dax=Dax, DD=DD,xshift=xshift)
  
  
  #die Segment-Ergebnisse zusammenbinden
  Tsl = Dax
  Tsl[]=NA
  for (segi in c(1:len(tsl)))
    if (segi==1)
      Tsl = tsl[[segi]]
  else
  {
    #browser()
    trailStopSegi = tsl[[segi]]
    trailStopSegi=trailStopSegi[index(trailStopSegi) > last(index(Tsl))]
    Tsl = append(Tsl,trailStopSegi)
  }
  
  #und visualisieren
  plot(Dax); lines(res,col="blue");
  lines(Tsl,col= col.add.alpha("red" , 90),lwd=4)
  #xyplot(Tsl,ylog_="F")
  return (Tsl)
  
}
#unique(Dax)
#findFn("trend")
if (F)
{
  #mache eine Segmentierung mit bestDD - Level
  bestDD=19
  ds =DdownDupWalkAhead(Dax,bestDD,bestDD,visual=T)
  res =ds$peak
  brk =ds$brk
  #analysiere die Segmente und zeige den trailingstop
  TrailingStop(Dax,ds,19,xshift =150)
  
}
#if (DrawD$kbreak>0)#trailing stop triggered
#Parameter-Training


#############################################################
#ein Watchportolio wird trainiert

if (F)
{
  ta5 <<- T2$new(PortfolioName = "RenditePlus", bench="Dax",visual = F ) 
  
  
  global_ParTable=NULL  #l?sche das Trainingsged?chtnis
  global_StartDate = 1
  
  heute=Sys.Date()
  heute = fromTo(ta5$t0data$prices)[2]
  heute=as.Date(heute)
  
  global_ParTable
  #thisRet=(Trade1( Ti=Ti, Ttype=Ti$type, frame= frame,data=data,ret=.self$ret))  #die closings   
  ta5$update(heute) #schreibt auch schon mal in den global_ParTable und liefert damit dem TrainIndicator via mlist() notwendige Startwerte
  global_ParTable
  global_StartDate
  #trace(TrainIndicator)
  
  TrainIndicator(heute,opti="DEoptim")
  #trace(mlist)
  global_ParTable
}



##########################################################################
##########################################################################
if (F)
{  
  #getMethod("initialize"), "MethodDefinition")
  
  ta5 <<- T2$new(PortfolioName = "RenditePlus", bench="Dax",visual = F ) 
  
  
  Dax<-Cl(ta5$t0data$Dax)
  Dax = Dax["2007::"]
  
  dd = 10
  ds =DdownDupWalkAhead(Dax,dd,dd,visual=T)
  
  norm_Win(1)
  plot(Dax)  
  lines(ds$peak,col="red",lwd=2)
  lines(ds$brk,type="h",col="blue",lwd=1)
  
  
  dm = mZigZag(Dax,dd)
  norm_Win(1)
  plot(Dax)
  lines(dm,col="red",lwd=2)
  
  slipF = merge(Dax,lag(Dax,1 ))
  slipRet=slipF[as.Date(index(dm)),2]
  
  plot(mNorm(dm))
  lines(mNorm(slipRet),col="blue")
  
  #komisch:  wenn ich die Datümer nur um eine Tag nach vorn schiebn
  #geht mir die hälfte flöte
  
  ret =mROC(ds$peak)
  q= quality(ret)
  
  mPlot(mRendite(ret))
}  
