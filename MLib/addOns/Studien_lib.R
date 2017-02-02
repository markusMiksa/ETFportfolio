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
# Schau Dir die zeitliche Lage  der Maxima der Crash-Werte (abweichung vom Max) an  .. daraus lässt sich ein Frühindikator ableiten
##########################################################################
KriesenAnalyse<-function ()   #MM_KriesenAnalyse
{
  #http://www.stoxx.com/download/historical_data/h_SXAP.txt
  
  Branchen <<- T2$new(PortfolioName = "Branchen", bench="Dax",visual = F ,online=F) 
  
  
  bp=Branchen$t0data$prices["2006-02::"]
  mPlotPureData((diff(log(bp),100)))
  
  
  whereMin<-function(xm2,mode=1){
    w=first(xm2[(which(xm2==min(xm2[is.finite(xm2)])))])
    #print(w)
    w1 =as.Date(index(w))
    dx=names(w)
    if (mode == 2)
      return(dx)
    #browser()
    return(w1)        
  }
  x=bp
  xMittel1 = bp
  xMittel2 = bp
  x[]=apply(coredata(log(bp)), 2, m.ifna.prev)
  BP=mNorm(x)
  x=BP
  diff1= diff(x,100+11) #um die x-achse zu erhalten
  diff1[]= apply(x,2,function(x) diff( runMax(x,100) ,11) ); 
  
  xMittel1 = diff1
  xMittel1[] = rowSums(diff1)
  xMittel1 = xMittel1[,1]
  
  diff2 = diff( x ,60);
  diff2 = apply(coredata(diff2), 2, m.ifna.prev)
  xMittel2[]=rowSums(diff2); 
  xMittel2=xMittel2[,1]
  
  #der zeitliche ablauf der 2008 - krise - über die Branchen hinweg
  norm_Win(3)
  
  whereMin(xMittel2)
  DaxX = scaleTo(bp[,"Dax"],range(na.omit(xMittel2)))
  
  mPlotPureData(xMittel2,DaxX,ylog_=F,main="rowSums diff 60")
  kriese=whereMin(xMittel1)
  print(kriese)
  DaxX = scaleTo(bp[,"Dax"],range(na.omit(xMittel1)))
  mPlotPureData(xMittel1,DaxX,main= "rowsum of diff 11 zu max 100")
  sb=sort(apply(diff1,2,whereMin,2))
  print (sb)
  
  mPlotPureData(diff1,main= "diff 11 zu max 100")
  print("Frühindikatoren:")
  earlyNames =names(sb[ sb  < kriese ])
  early = BP[,earlyNames]  
  early = BP
  dim(early)
  Earl = early 
  Earl[] =rowSums(early)
  Earl=Earl[,1]
  DaxX = scaleTo(bp[,"Dax"],range(na.omit(Earl)))
  
  mPlotPureData(Earl,DaxX)
  
  x=early
  diff2 = diff( x ,60);
  diff2 = apply(coredata(diff2), 2, m.ifna.prev)
  xMittel2[]=rowSums(diff2); 
  xMittel2=xMittel[,1]
  whereMin(xMittel2)
  DaxX = scaleTo(bp[,"Dax"],range(na.omit(xMittel2)))
  
  mPlotPureData(xMittel2,DaxX,ylog_=F,main="rowSums diff 60")
  
  library(TTR)
  x=BP
  x=early
  E[] = EMA(x,10)
  E[c(1:10),] = x[c(1:10),]
  diff2 = x-lag(E,60);
  
  diff2 = apply(coredata(diff2), 2, m.ifna.prev)
  xMittel2[]=rowSums(diff2); 
  xMittel2=xMittel[,1]
  whereMin(xMittel2)
  DaxX = scaleTo(bp[,"Dax"],range(na.omit(xMittel2)))
  
  mPlotPureData(xMittel2,DaxX,ylog_=F,main="rowSums diff 60")
  
  head(E)  
}
##############################################################################



mm_AA <-function(data, period = "months",n.top=6, k.top=6,version="", AAignoreStopped=T,Tstops=NULL,frame="2003::",compare2NN =T, riskFreePos = "")
{
  
  tickers =data$symbolnames
  n = len(tickers)  #dim(data$prices[2])
  if (n.top < 1) n.top=n
  if (k.top < 1) k.top=n
  if (k.top < n.top) k.top = n.top
  
  #browser()
  dates = frame
  data$prices = data$prices[dates] 
  prices = data$prices  
  n.vol = 60
  #n.top = 7        
  
  
  
  # Rank on 6 month return
  if (F) #sehr hakelig
  { 
    n.mom = 180
    momentum = prices / mlag(prices, n.mom)  
    
    position.score = momentum
    experiment="AA ntop month on mom"
  } 
  
  if (F)
  {
    #position.score = m.apply(data, Fun=TSI,PreFun=HLC )   
    position.score = TSIsymbol(data) #geht gar nicht
    position.score[position.score<0]<-NA
    #tail(position.score,100)
    experiment="AA TSI"
    
  }
  
  
  #browser()  
  if (T)   #KLASSSE
  {
    mSlope90 <<- rollRegressionXTS(mNorm(data$prices),win=90)
    
    signal =iif( mSlope90 > 0,1, 0.00000001)
    position.score =lag(signal)*mSlope90*10000 
    #position.score[mSlope90<0]<-NA
    colnames(position.score)=colnames(prices)
    experiment="AA mSlope90"
    
    
    if (F)
    {
      data$prices=prices
      data$weight = prices
      data$weight[] = ntop(prices, n)
      ew = bt.run(data)  
      # Avoiding severe draw downs
      # http://engineering-returns.com/2010/07/26/rotational-trading-system/
      # Only trade the system when the index is either above the 200 MA or 30 MA
      # Usually these severe draw downs  happen bellow the 200MA average and 
      # the second 30 MA average will help to get in when the recovery happens  
      buy.rule = (ew$equity > SMA(ew$equity,200)) | (ew$equity > SMA(ew$equity,10))
      #buy.rule = (ew$equity > SMA(ew$equity,200))
      buy.rule = ifna(buy.rule, F)
      buy.rule = lag(buy.rule)
      
      position.score[as.Date(index(buy.rule[buy.rule==F])),] = NA
    }
  }
  
  if (F)
  {
    ret = mROC(prices)
    #signal<-merge(apply.rolling(ret[,1],FUN="Omega",width=25),apply.rolling(ret[,2],FUN="Omega",width=25))
    omega <- rollapplyr(ret, width=25, FUN="Omega", by.column=T, align = "right")
    omega[!is.finite(omega)]<-0
    #omega[omega < 1.5] <-0 # #zus?tzliche Gl?ttung
    #omega[omega > 20]<-20
    
    #lag the data by 1
    signal<-lag(omega,k=1)  #ohne das wirds super "vorhersehend" ?????
    position.score = signal
    
    if (F)
    {
      data$weight[] = NA
      data$weight[] = signal
      dim(signal)
      m.omega = bt.run(data, trade.summary=T)
      plot(m.omega$equity)
    }
    # position.score = lag(TSIsymbol(data))    *lag(signal)*mSlope90*10000
    experiment="AA mSlope90"
    
  }
  
  
  
  if (F)  #SCHROTT
  {
    #dmSlope90 <<-rollRegressionXTS(na.omit(mSlope90),win=30)
    
    signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0.001)
    
    position.score =lag(signal)*mSlope90*10000;
    experiment="AA ntop month on mDoubleSlope90"
  }
  
  if (F)
  {
    position.score = data$watch$prices 
    experiment = "AA ntop month on ekSlope90"
  }
  
  
  if (F)  
  {
    #*****************************************************************
    # Possible Improvements to reduce drawdowns   ES forecast
    #****************************************************************** 
    # Equal Weight
    data$prices=prices
    data$weight = prices
    data$weight[] = ntop(prices, n)
    ew = bt.run(data)  
    # Avoiding severe draw downs
    # http://engineering-returns.com/2010/07/26/rotational-trading-system/
    # Only trade the system when the index is either above the 200 MA or 30 MA
    # Usually these severe draw downs  happen bellow the 200MA average and 
    # the second 30 MA average will help to get in when the recovery happens  
    
    mP("Risk.es")
    mchart(ew$equity)
    head(ew$equity,100)
    ES(mROC(ew$equity["2006"]))
    browser()
    #zusammenhang  VaR und ES  aus http://www.financialriskforecasting.com/code/5.html
    VaR = -qnorm(p)
    integrand = function(q){q*dnorm(q)}
    ES = -sigma*integrate(integrand,-Inf,-VaR)$value/p*value
    
    Risk.es <- rollapplyr(na.omit(mROC(ew$equity)),FUN="ES",width=36,p=0.95,na.pad=TRUE)
    #take care of NA with 0 at beginning and interpolation at end
    Risk.es <- apply(Risk.es,MARGIN=2,FUN=na.fill,fill=c(0,"extend"))
    Risk.Es <-as.xts(Risk.es)
    
    mchart(merge(scaleTo(Risk.Es,range(mNorm(ew$equity))),mNorm(ew$equity)))
    mchart(Risk.Es)
    
    
    
    buy.rule = (ew$equity > SMA(ew$equity,200)) | (ew$equity > SMA(ew$equity,10))
    #buy.rule = (ew$equity > SMA(ew$equity,200))
    buy.rule = ifna(buy.rule, F)
    buy.rule = lag(buy.rule)
    
    # Rank using TSI by Frank Hassler, TSI is already smoothed and slow varying, 
    # so SMA will filter will not very effective
    #http://engineering-returns.com/tsi/
    
    signal =iif(dmSlope90 >0 & mSlope90 > 0,1,0.001)
    position.score =lag(signal)*mSlope90*10000;
    #position.score = bt.apply(data, function(x)  TSI(HLC(x)) )    
    
    position.score[as.Date(index(buy.rule[buy.rule==F])),] = NA
    position.score["2008-02"]
    
    
    
    experiment="AA 6ntop6 weekly slop90 and buyRule"
    
  }
  
  if (F)  #SUPER  MMEXPOSURE
  {
    #*****************************************************************
    # Possible Improvements to reduce drawdowns
    #****************************************************************** 
    # Equal Weight
    data$prices=prices
    data$weight = prices
    data$weight[] = ntop(prices, n)
    ew = bt.run(data)  
    # Avoiding severe draw downs
    # http://engineering-returns.com/2010/07/26/rotational-trading-system/
    # Only trade the system when the index is either above the 200 MA or 30 MA
    # Usually these severe draw downs  happen bellow the 200MA average and 
    # the second 30 MA average will help to get in when the recovery happens  
    buy.rule = (ew$equity > SMA(ew$equity,200)) | (ew$equity > SMA(ew$equity,10))
    #buy.rule = (ew$equity > SMA(ew$equity,200))
    buy.rule = ifna(buy.rule, F)
    
    buy.rule = lag(buy.rule)
    
    # Rank using TSI by Frank Hassler, TSI is already smoothed and slow varying, 
    # so SMA will filter will not very effective
    #http://engineering-returns.com/tsi/
    
    
    
    position.score = lag(TSIsymbol(data))
    #position.score = bt.apply(data, function(x)  TSI(HLC(x)) )    
    tail(position.score)
    
    
    #position.score.ma = position.score
    #position.score["2009-03"]
    #position.score["2009-2::2009-5",]=NA
    #buy.rule["2008-02"]
    #position.score[!buy.rule] = NA #klappt nicht
    #du musst schreiben:
    
    position.score[as.Date(index(buy.rule[buy.rule==F])),] = NA
    position.score["2008-02"]
    
    experiment="AA 6ntop6 weekly TSI and buyRule"
    
  }
  
  
  #Ausrichtung
  #block=na.omit(merge(data$prices,data$watch$prices ))
  block=(merge(data$prices,position.score  ))
  #block = block["2001::"]
  
  prices =block[,1:n] 
  
  position.score = block[,(n+1):(2*n)]
  #purePlot(mNorm(prices))
  
  # find month ends
  month.ends = endpoints(prices, 'months')
  month.ends = month.ends[month.ends > 0]    
  week.ends = endpoints(prices, 'weeks')
  week.ends = week.ends[week.ends > 0]    
  
  if (period == "months")
  {
    period.annual.factor=12
    period.ends = month.ends  
  }
  else
    if( period == "weeks")
    {
      period.annual.factor=54
      period.ends = week.ends  
      
    }
  
  #period.endsM = as.Date(index(prices[period.ends])) #MMXX
  #  block["2008"]
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  
  
  #  purePlot(mNorm(prices))
  #  xyplot(position.score)
  
  #MM2
  
  #### moderner
  data$weight = prices
  data$weight[] = NA
  data$prices = prices
  
  #Tstops = prices;  Tstops[] = NA
  #technische Stops  MMEM3
  
  #Berechnung der dayly-technischen stops.
  #Wenn ein System am Alloc-Tag (periods.ends) augestoppt ist, erh?lt es ein sehr schlechtes  #position.scoring.
  #Es wird also niemand allokiert der ausgestoppt ist.
  
  
  
  #browser()
  
  if (F && !is.null(Tstops ) && AAignoreStopped)
  {
    mP("use Tstops")
    
    #  Tstops = lag(m.apply(data, function(x) {techStops(x)})  )   
    #Tstops = iif(global_target <0, 0,1)  #TEST-KRISTALLKUGEL
    TstopsOnPeriods = Tstops[as.Date(index(position.score[period.ends,]))]
    stopped= TstopsOnPeriods * position.score[period.ends,]
    #sollen ausgestoppte Titel nicht bei der AA ber?cksichtigt werden ?
    if (AAignoreStopped)
      position.score[period.ends,] = stopped 
  }
  #else
  #  Tstops=NULL
  ##########################################################################
  
  if (F)
  {
    signal =iif( mSlope90 > 0,1, 0.0000001)
    position.score =lag(signal)*mSlope90*10000;
    
    universeMom= ntop.keep(momentum[period.ends,], n.top, k.top) > 0
    universe = ntop.keep(position.score[period.ends,],n.top, k.top) > 0
    
    universe = universe & universeMom
    
    experiment="AA ntop month on MomentumSlope90"
  }
  else
    universe = ntop.keep(position.score[period.ends,], n.top, k.top)  
  colnames(universe)=colnames(prices)
  
  #position.score["2008"]
  #position.score[period.ends,]
  #universe["2008"]
  #  universe = ntop.keep(position.score[period.ends,], 26, 26) > 0
  
  #*****************************************************************
  # Code Strategies that rebalance based on maximum deviation
  #****************************************************************** 
  
  # rebalance to target.allocation when portfolio weights are 5% away from target.allocation
  # models$smart5.all = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 5/100, 0) 
  
  #Berechnung der Gewichte .. gem. 
  #  install.packages("Rdonlp2", repos="http://R-Forge.R-project.org")
  #U<<-data.frame(universe)
  
  #universe[universe !=0] <- 1/n
  mP("uni")
  
  #if (F)
  #die Rest-Gewichte mit dem REX auffüllen
  if (riskFreePos != "" && contains(colnames(universe),riskFreePos) )
  {
    mP("fill up exposure on %s ",riskFreePos)
    universe[,riskFreePos]= universe[,riskFreePos]+(1-rowSums(universe[]))
  }
  
  print("universe-rows")
  print(rowSums(universe))
  browser()
  
  
  if (compare2NN)
  {
    mP("---------------------- prognosefreies- Portfolio mit sämtlichen %d Titeln  ->",n)
    browser()
    
    position.score[period.ends,] <-1
    universeNN = ntop.keep(position.score[period.ends,], n, n)  
    
    objNN = portfolio.allocation.helper(prices, 
                                        periodicity = period,
                                        period.ends=period.ends,
                                        lookback.len = n.vol, 
                                        universe=universeNN,
                                        
                                        shrinkage.fns = "ledoit.wolf.shrinkage",
                                        
                                        min.risk.fns = list(
                                          EW=equal.weight.portfolio,
                                          MV=min.var.portfolio,
                                          MC=min.corr.portfolio,
                                          MS=max.sharpe.portfolio()
                                        )
    ) 
    
    if (!is.null(Tstops))
    {
      print("---- ohne Stop-System ---")
      browser()
      modelsNN = create.strategies(objNN, data,Tstops=NULL)$models
      strategy.performance.snapshoot(modelsNN, T)    
      print ("--- mit -Stop-system --")
      browser()
    }  
    modelsNN = create.strategies(objNN, data,Tstops=Tstops)$models
    strategy.performance.snapshoot(modelsNN, T)
    mP("noch mehr Analysen ....")
    browser()
    k=compareViewModels(modelsNN,prices,alloc=T)
    
  }
  
  print("---------------------- nTopK- Vorauswahl - Portfolio->")
  
  browser()
  
  obj = portfolio.allocation.helper(prices, 
                                    periodicity = period,
                                    period.ends=period.ends,
                                    lookback.len = n.vol, 
                                    universe=universe,
                                    shrinkage.fns = "ledoit.wolf.shrinkage",
                                    min.risk.fns = list(
                                      EW=equal.weight.portfolio,
                                      ###    RP=risk.parity.portfolio,
                                      MV=min.var.portfolio,
                                      ###      MD=max.div.portfolio,
                                      MC=min.corr.portfolio,
                                      ###      MC1=min.cdar.portfolio,
                                      ###      MC2=min.mad.downside.portfolio,
                                      #MO=max.omega.portfolio,  #l?uft nicht, geht wohl nur mit Rdonlp2
                                      #MR=min.risk.portfolio,  #mittelm?ig . wie MV
                                      #ML=min.maxloss.portfolio,  schlecht
                                      #MR=min.risk.downside.portfolio,  #genau wie MV
                                      #MP=max.geometric.return.portfolio,  #l?uft nicht
                                      
                                      # MC2=min.corr2.portfolio,  #schrott
                                      # MCE=min.corr.excel.portfolio,    #schrott
                                      MS=max.sharpe.portfolio())
  ) 
  
  
  #data$weight[] = 0
  #data$execution.price[] = NA  
  #models$buy.hold = bt.run.share(data, clean.signal=T,trade.summary = TRUE)
  #models$buy.hold$trade.summary$trades
  
  #data$weight = prices
  #data$weight[]=NA
  #dim(obj$weights$EW)
  #data$weight[period.ends,] =  obj$weights$EW
  
  #bh = bt.run.share(data, clean.signal=T,  trade.summary = TRUE)
  #bh$trade.summary$trades
  #ls(obj)
  #obj$weights
  #Berechnung der Modell-GUV
  
  mP("............... nTopK-create.strategies ")
  if (!is.null(Tstops))
    print("-------------------- mit tagesaktuellen Stop-Limits")
  
  if (!is.null(Tstops))
  {
    print("---- ohne Stop-System ---")
    browser()
    models = create.strategies(obj, data,Tstops=NULL)$models
    strategy.performance.snapshoot(models, T)    
    print ("--- mit -Stop-system --")
    browser()
  }  
  models = create.strategies(obj, data, Tstops=Tstops)$models
  experiment=sprintf("%s_%s_n%dk%d_%s%s",experiment,period,n.top,k.top,version,ifelse(is.null(Tstops),"NOstops","STOPS"))
  mP("########## experiment %s ########### ",experiment)
  models = rev(models)
  strategy.performance.snapshoot(models, T)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mP("noch mehr Analysen ....")
  browser()
  models$MS$trade.summary$trades
  browser()
  #clr()
  #new_Win(1)
  #purePlot(mNorm(prices))
  #clr()
  k=compareViewModels(models,prices,alloc=T)
  experiment = "testAll2"
  browser()
  mP("schreibe pdf und xls-Dateien")
  pdf_Report(models,experiment)
  plotbt.custom.report.part2(models$MS)       
  
  mP("--- Ende der Demo ---")
  browser()
  
}
#########################################################################
