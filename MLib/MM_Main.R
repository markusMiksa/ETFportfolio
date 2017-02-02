#MM_Main.R    Hier bau ich mein Aktien/Renten - Allocationssystem
#A)-SystemLib laden

########################################################################################

if (F)            #falls reinit- gewuenscht anklicken
  rm(MM_lesson1)  #<<<<<<#######<<<##############<<<###########################<<<<<<

if (!(exists("MM_lesson1") &&  MM_lesson1 == "ok"))
{
  rm(list= ls())
  
}
# online=F
#  universe = "Eckhard4.12" # "Eckhard4"##"Meurope","DaxMdax"#MWorld3" #,"DowComp" #DaxMdax "DaxComp"# MDaxComp"  #"DaxComp"#DAX" #"MWorld3"    DAX,  STOXX, ASIA
#IndustrieProd
#Futures
#Macros
#DaxComp
#StoxxComp
#MDaxComp
#DowComp 

if (F)
{
  init_TSA(universe = "DaxMdax", dataSet ="MM_Jan14", TargetMethod= 0,  online =T, visual=T)
  init_TSA(universe = "MWorld3", dataSet ="MM_Jan14", TargetMethod=  0 , online =F, visual=T)
  init_TSA(universe = "StoxxComp", dataSet ="MM_Jan14", TargetMethod=  0 , online =T, visual=T)
  
}
#diese Parameter werden beim Reinit- gew?hlt
do.par = F
#init_TSA<-function(universe="Eckhard4.12", dataSet ="EM4_JAN.5", TargetMethod=0, online=F, visual=F)  #TargetMethod -1,0,1



####################################################################################
#######  das einzige Exceutable hier - lädt alles notwendigen libs, definiert globals und kann so zu processor-init vor 
####### einem parallel-lauf dienen  (siehe define.Globals()  )


if (!(exists("MM_lesson1") &&  MM_lesson1 == "ok"))
{
  source  ("MLib/TSA.r")
  init_TSA()  #aufruf mit den default parametern.  
  #jetzt sind alle pakete und sourcen geladen - wenn univers!="" war, dann auch 
  #der init-datensatz
  
  if ("DAX" %in% colnames(data$prices))
    dax<-na.omit(data$prices["2000::","DAX"]  )
  
  if (do.par)  #............... bereite den parallel-cluster vor, den kannst Du dann 
    #mit sfLapply(..., ) ansprechen.
    prepare_Parallel()
  
  p<<- data$prices[,data$BENCH];P=mNorm(p)
  P<<- na.omit(SMA(na.omit(ZigZag(p[1000:1500])),5))   #zigzag-reihe - gut f?r causalit?ts test
  prices <<- data$prices
}

#save(list=ls(),file="d:/mm.Rdata")
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)

#source("MLib/SITpatches.r")  #mein Anpassungen dazu



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#C)-Martkdaten laden - und vorverarbeiten
if (F)
{
  Load("gv")
  GDAXI = gv$t0data$Dax
  xtsPrice = Cl(GDAXI)
  #xtsPrice = Cl(to.monthly(xtsPrice))
  dim(xtsPrice)
  
  # frame="2006-12-01::2010-01-01"
  
  #frame="2007-11-01::2010-01-01"
  #  frame="2008-09-24::2010-01-01"
  
  #  frame="2003-08-01::"
  frame="1997::"
  
  xtsPrice=xtsPrice[frame]
  xtsPrice = mNorm(xtsPrice)
  print(len(xtsPrice[,1]))
  fromTo(xtsPrice)
  plot(xtsPrice)
  #............................................................
  #neu einlesen der portfolio.xls-sheets (wie bei source(TradeClasses.r))
  loadPortfolios(customerData)  #customerDate = "Models
  x=PortfolioTicks("Meurope")$Ticker
  x=PortfolioTicks("MWorld3")$Ticker
  
  #............................................................
  
  Mworld <<- T2$new(PortfolioName = "Mworld", bench="Dax",visual = F, online=T )
  data2<<-Mworld$t0data
  
  #ok
  SektorSpdrUSA<<- T2$new(PortfolioName = "SektorSpdrUSA", bench="Dax",visual = F, online=F )
  data<-SektorSpdrUSA$t0data
  data.Info(SektorSpdrUSA$t0data)
  
  #ok  
  Meurope <<- T2$new(PortfolioName = "Meurope", bench="Dax",visual = F, online=T)
  data<-Meurope$t0data
  
  Mzinsen <<- T2$new(PortfolioName = "Zinsen", bench="Rex",visual = T, online=F)
  data<-Mzinsen$t0data

  SektorSpdrUsaX <<- T2$new(PortfolioName = "SektorSpdrUsaX", bench="Treasury",visual = T, online=F)
  data<-Mzinsen$t0data
  
  
  
  
  
  
  #..............................................................#MM1
  ONLINE=F
  if (ONLINE)
  {
    Mmain <<- T2$new(PortfolioName = "Mmain", bench="Dax",visual = F, online=T)
    data<-Mmain$t0data
  }
  else
  {
    #Russel1000 hat nen ausreisser
    Load("Mmain.xls")
  }
  prices = data.info(data)#,ignore=C("VDAX"))
  
  
  trainFrame= "::2012"
  fromTo(prices)
  xyplot(mNorm(prices))
  
  purePlot(mNorm(prices["2013",c("REX","DAX")]))
  purePlot((prices["2013",c("DAX")]))
  
  
  purePlot(mNorm(prices["2013"]))
  purePlot(mNorm(prices))
  xyplot(mNorm(prices["2013"]))
  #bt.cluster.visual.test() 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  #Zeit-aufw?ndige Vorverarbeitungen
  #  data.analysis<-function(data, xlsFile="DataAnalysis", mode=spl("price,priceCleaned,coint,target,score"))
  
  if (F)
  {
    #mdata= data.analysis(data, xlsFile="DataAnalysis", mode=spl("price"))
    global_Targets=compute.Targets(prices)
    if (F)
    {
      k1=  compute.Target(prices[,"USDEUR"],maxDDtoleranz=0.16, minReturn=0.05)
      k1$ContinuePos
    }
    colnames(global_Targets)
    save(file="global_Targets")
  }
  #oder alles zusammen:
  mdata= data.analysis(data, xlsFile="DataAnalysis", mode=spl("price,target,score"))
  
  train.frame= "::2012"
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   MM_Entwicklung 28.5.2013
  #Vorbereitung des Trainings
  
  train.frame = "::2012"
  train.frame = "::2014"
  
  global_commission = 0.00001
  
  global_arg<<-list(clos=prices[,1:2])  #Definition der Preise
  global_arg<<-list(clos=prices[train.frame,],dat=data)  #Definition der Preise
  
  global_ParTable <<-NULL  
  global_objectId <<-paste("TREND","Dax","signal.SMAzlema")   #Welcher Indikator 
  global_StartDate <<- DateS(last(prices[train.frame]))  #der zeitstempel- zusammen mit globla_objectId der joined key in global_ParTable
  #Parametrisierung des Trainings .. ich leg sogar die increment-Werte f?r den GRID selber fest
  
  mlist(smaN=c(20,10,100,20), zlemaN=c(10,10,90,30), mode="w")   #grobe suche   
  mlist(smaN=c(20,10,100,10), zlemaN=c(10,10,90,5), mode="w")   #feine suche 
  
  #So sieht jetzt die global_ParTable aus:
  mlist(mode="R")
  global_ParTable$par
  global_ParTable
  
  global_StartDate = "2012-12-14"
  mlist(smaN=c(20,10,100,10), zlemaN=c(10,10,90,5), mode="w")   #feine suche 
  TrainIndicator (opti="GRID",indiName = "signal.SMAzlema",visual=T,TRAINSYM=-1)  
  
  global_StartDate <<- DateS(last(prices[train.frame]))  #der zeitstempel- zusammen mit globla_objectId der joined key in global_ParTabl
  TrainIndicator (opti="GRID",indiName = "signal.SMAzlema",visual=T,TRAINSYM=0)  
  
  global_ParTable$par
  global_ParTable[objectId==global_objectId]
  global_ParTable[objectId==global_objectId]$par
  
  
  i.system=indi.Generic("signal.SMAzlema", global_arg,visual=T, TRAINSYM=-1)  #jeder mit seinen eigenen BestParams
  
  i.system=indi.Generic("signal.SMAzlema", global_arg,visual=T, TRAINSYM=-1,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  
  
  
  if (adRiskData)
  {
    R = mROC(prices)
    ret.log = bt.apply.matrix(prices, ROC, type='continuous')
    
    someTableData = list()
    
    #  someTableData$hist.vol = bt.apply.matrix(ret.log, runSD, n = 21)
    #  someTableData$drawdown<- compute.drawdown(R)
    #  someTableData$RSI60 =-bt.apply.matrix(prices, RSI, n=60, maType=list(maUp=list(EMA,ratio=1/5),    maDown=list(WMA,wts=1:10)))  
    
    #someTableData$RSI20_ =bt.apply.matrix(prices, RSI, n=60, maType=list(maUp=list(ZLEMA),    maDown=list(WMA,wts=1:10)))  
    
    #someTableData$dvi <- mt.apply.matrix(prices, "DVI","DVI")
    mSlope90 <- rollRegressionXTS(prices,win=90);colnames(mSlope90)= colnames(prices)
    mSlope120 <- rollRegressionXTS(prices,win=120);colnames(mSlope120)= colnames(prices)
    mSlope30 <- rollRegressionXTS(prices,win=30);colnames(mSlope30)= colnames(prices)
    mSlope60 <- rollRegressionXTS(prices,win=60);colnames(mSlope60)= colnames(prices)
    mSlope180 <- rollRegressionXTS(prices,win=180);colnames(mSlope180)= colnames(prices)
    someTableData$mSlope90=mSlope90
    someTableData$mSlope120=mSlope120
    someTableData$mSlope30=mSlope30
    someTableData$mSlope60=mSlope60
    someTableData$mSlope180=mSlope180
    
    # someTableData$DV =bt.apply.matrix(HLC(to.weekly(prices)), DV)
    #  someTableData$DV =bt.apply.matrix(prices, DVI)
    # someTableData$DV =bt.apply.matrix(HLC(to.weekly(prices)), TSI)
    
    #someTableData$dvi <- mt.apply.matrix(HLC(to.weekly(prices)), "DVI","DVI")
    #colnames(someTableData$dvi)
    
    ls(someTableData)
    
    if (F)
    {
      Risk.es <- rollapplyr(na.omit(R),FUN="ES",width=36,p=0.95,na.pad=TRUE)
      #take care of NA with 0 at beginning and interpolation at end
      someTableData$Risk.es <- apply(Risk.es,MARGIN=2,FUN=na.fill,fill=c(0,"extend"))
    }
    scoreScore(prices,i.system, n=6,K=12,someTableData=someTableData) 
    
  }
  else
    scoreScore(prices,i.system, n=6,K=12)
  #berechne f?r jeden Monat das scoring, erstelle global_TrainScore
  #Bewerte die Kriterine in der nur erstellte
  # compute.scoreScore(thresh=12/dim(prices)[2])  
  showScoreScores()
  
  if (F)
  {
    ls(global_TrainScore)
    
    global_TrainScore$ADX
    
    k=data.frame(lapply(global_TrainScore,function(x)as.numeric(x)))[,2]
    trainScore=as.xts(data.frame(lapply(global_TrainScore, FUN=function(x)as.numeric(x))),order.by=as.Date(index(global_TrainScore$ADX)) )
    
    ls(global_TrainScore)
    trainScore[,2]
    global_TrainScore$aroon
  }
  ###################################################################################
  ####################################################################################
  
  #Training aller Zeitreihen durchf?hren  - bei TRAINSYM=0 - bekommen sie einen einheitlichen BestParams - sonjst findet
  #ein Bestparam f?r alle wertpapiere  
  system.time(
{
  
  TrainIndicator (opti="GRID",indiName = "signal.SMAzlema",visual=T,TRAINSYM=-1)  
  global_Targets<<-tryM(compute.Targets(prices))
  global_score2<<-calc.score.nk(prices=prices,n=10, K=7, wlen=150)
})



#sich f?r jede zeitreihe ein individueller BestParam
TrainIndicator (opti="GRID",indiName = "signal.SMAzlema",visual=T,TRAINSYM=0)    
#und das hat er gefunden:
backup_global_ParTable <<- global_ParTable

global_ParTable <<-  backup_global_ParTable_fein
global_ParTable$par  
global_ParTable<<-backup_global_ParTable

save(file="global_ParTable")

TrainIndicator (opti="GRID",indiName = "signal.SMAzlema",visual=T,TRAINSYM=0-1)    #u
#Abrufen der Indikatoren mit ihren Bestparam - dannach wird das summensystem mit einem Buy-Hold verglichen
pdf(file = paste("Timing_SMAzlema",'.pdf',sep=""), width=15, height=10)
i.system=indi.Generic("signal.SMAzlema", global_arg,visual=T, TRAINSYM=-1)  #jeder mit seinen eigenen BestParams
dev.off()
i.system=indi.Generic("signal.SMAzlema", global_arg,visual=T, TRAINSYM=0)   #jeder mit gleichen BestParams

ls(i.system)
ls(i.system$singleResults)
ls(i.system$singleResults[[1]])
#Tquality  - Einzelergebnisse:

colnames(i.system$Signal)
colnames(i.system$singleResults[[1]]$equity)
colnames(i.system$singleResults[[2]]$equity)


singleResults=  t(data.frame(lapply(i.system$singleResults,function(x) 
{
  col=toString(colnames(x$equity))
  c(
    x$Tquality,
    last(i.system$Signal[,col]),
    compute.sharpe(mROC(x$equity[c((len(x$equity)-200):len(x$equity)),]))
  )  })))


singleResults=cbind(rownames(singleResults),singleResults)
colnames(singleResults) = spl("sym,Tquality,sigSMAzlema,sharpe")

#.................................................................
singleEquities=  data.frame(lapply(i.system$singleResults,function(x) 
{
  x$equity
}))

tail(singleEquities)
singleEquitiesXts = as.xts(singleEquities,order.by=as.Date(rownames(singleEquities)))
tail(singleEquitiesXts)
mchart(singleEquitiesXts)


################################################  ###########################

test.system = "signal.wonder"
global_arg<<-list(clos=prices[,1:2])  #Definition der Preise
global_ParTable <<-NULL  
global_objectId <<-paste("TREND","Dax",test.system)   #Welcher Indikator 
global_StartDate <<- DateS(last(prices))  #der zeitstempel- zusammen mit globla_objectId der joined key in global_ParTable
#Parametrisierung des Trainings .. ich leg sogar die increment-Werte f?r den GRID selber fest

mlist( k=c(20,10,30,2), zlemaN=c(20,10,150,10))
#So sieht jetzt die global_ParTable aus:
mlist(mode="R")
global_ParTable$par
global_ParTable

#Training aller Zeitreihen durchf?hren  - bei TRAINSYM=0 - bekommen sie einen einheitlichen BestParams - sonjst findet
#sich f?r jede zeitreihe ein individueller BestParam
TrainIndicator (opti="GRID",indiName = test.system,visual=T,TRAINSYM=0)  #ein Bestparam f?r alle wertpapiere  
TrainIndicator (opti="GRID",indiName = test.system,visual=T,TRAINSYM=-1)  
#und das hat er gefunden:
global_ParTable
global_ParTable$par

#Abrufen der Indikatoren mit ihren Bestparam - dannach wird das summensystem mit einem Buy-Hold verglichen
x=indi.Generic(test.system, global_arg,visual=T, TRAINSYM=0)   #jeder mit gleichen BestParams
x=indi.Generic(test.system, global_arg,visual=T, TRAINSYM=-1)  #jeder mit seinen eigenen BestParams

x=indi.Generic("signal.wonder", global_arg,visual=T, TRAINSYM="Dow")
x=indi.Generic("signal.wonder", global_arg,list( k=154, zlemaN=4), visual=T, TRAINSYM="Dax")  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
global_ParTable <<-NULL

global_arg<<-list(clos=prices[,1:1])
global_objectId <<-paste("TREND","Dax","indi.SMA")
x=indi.SMA(global_arg)  #schreibe die mlist(...) Werte von indi.SMA nach global_ParTable
TrainIndicator(global_StartDate_ = DateS(last(prices)), opti="GRID",indiName = "indi.SMA",visual=F)  


global_objectId <<-paste("TREND","Dax","indi.RSI") 
x=indi.RSI(global_arg) #nun schreibe die mlist() Werte von indi.RSI nach global_ParTable
mlist()
globalTrainLevel <<-10
TrainIndicator(global_StartDate_ = DateS(last(prices)), opti="GRID",indiName = "indi.RSI",visual=F)  
indi.RSI(global_arg,list(rsiL=14,rsiThres=10,zlemaN=0),visual=T ) #nun schreibe die mlist() Werte von indi.RSI nach global_ParTable
indi.RSI(global_arg,list(rsiL=14,rsiThres=45,zlemaN=0),visual=T ) #nun schreibe die mlist() Werte von indi.RSI nach global_ParTable
x=indi.RSI(global_arg,list(rsiL=2,rsiThres=45,zlemaN=170),visual=T ) #nun schreibe die mlist() Werte von indi.RSI nach global_ParT
#~~~~~~~~~~~~  NUN multivariate


global_arg<<-list(clos=prices[,1:2])
global_objectId <<-paste("TREND","Dax","signal.wonder")

res=signal.wonder(global_arg)   # DOW: 10,150

res=signal.wonder(global_arg,list( k=20, zlemaN=60),visual=F)   # DOW: 10,150

res= indi.Generic("signal.wonder", global_arg)   

indi.Generic("signal.wonder", global_arg,  list( k=10, zlemaN=150), visual=T,commission=0.00001)$Tquality   

global_ParTable <<-NULL
global_objectId <<-paste("TREND","Dax","signal.wonder")
globalTrainLevel <<-10

global_StartDate = DateS(last(prices))
TrainIndicator( opti="GRID",indiName = "signal.wonder",visual=F,TRAINSYM=-1)  

######################################
global_arg<<-list(clos=prices[,1:2])

global_ParTable <<-NULL
global_objectId <<-paste("TREND","Dax","signal.SMAzlema")
globalTrainLevel <<-10
global_StartDate <<- DateS(last(prices))
indi.Generic("signal.SMAzlema", global_arg)#schreib die default par mit mlist()
#geht auch mit:
mlist(smaN=c(20,10,100,2), zlemaN=c(10,10,90,2), mode="w")   #wenn ich die increment -parameter ?bergebe kann GRID gem. level suchen
global_ParTable$par
mlist()
mlist(mode="R")

global_commission = 0.00001

TrainIndicator( opti="GRID",indiName = "signal.SMAzlema",visual=T)  

x=indi.Generic("signal.SMAzlema", global_arg, par=list(smaN=44,zlemaN=90),visual=T, TRAINSYM="Dow")#schreib die default par mit mlist()


x=indi.Generic("signal.SMAzlema", global_arg, par=list(smaN=44,zlemaN=90),visual=T)
x=indi.Generic("signal.SMAzlema", global_arg, par=list(smaN=23,zlemaN=10),visual=T)
x=indi.Generic("signal.SMAzlema", global_arg, par=list(smaN=90,zlemaN=90),visual=T)

x=indi.Generic("signal.SMAzlema", global_arg, par=list(smaN=30,zlemaN=20),visual=T)


###########################

#  Trainiere den indi.Generic
#  Nur ganzahlige Teile bei GRID ...

#tune.monthly(prices, symbol="TECDAX", indiName="indi.ZlemaDif",wlen=200,opti="GRID",trainLevel = 100)
#tune.Generic(prices, endpoints,indiName="wonder",wlen=200,opti="GRID",trainLevel = 100)

#aus rollierender zeitreihenbetrachtung .. sharep und Cgar berechnen  und MAXdd->daraus
#attraktivi?tt schlieesen


#TODO:   Hier kommen in Kriesenzeiten noch slopes mit negativem Trend durch !  - die m?ssen in Short-Positionen gedreht werden !!!!!
###########################
}

##############################################################,
#erzeuge eine scoring matrix mit der dim wie prices
#aus 1,0  - je nachdem ob ein Titel allokiert wird oder nicht
##############################################################
#global_ParTable <<- NULL ;mP("global_ParTable=NULL")
#globalTrainLevel <<-  100
global_TrainStyle <<-  "Target" #("GuV")
runN<<-0

######################################################################
#auto-tunining- Indikator
######################################################################

tune.monthly<-function (prices, symbol, indiName, wlen=50, opti="GRID", trainLevel = 100, visual =F)
{
  global_objectId <<-paste("TREND",symbol,indiName)
  lasti=dim(na.omit(prices[,symbol]))[1]
  pricesSym = prices[wlen:lasti,symbol] #vor dem wlen-offset lohnt sich nichts
  globalTrainLevel <<-trainLevel
  firstRun = T
  lastTrainDay <<- as.Date(index(first(prices)))-1  
  global_xMarker<<- mlist()
  resIndi <<-NULL
  
  
  apply.monthly(pricesSym, FUN=function(x) #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    TodayPrice=as.Date(index(last(x))); Today = as.Date(index(x[TodayPrice]))
    workframe=sprintf("%s::%s",lastTrainDay+1,Today) #dieser frame entspricht den echten Tagen in denen der Indikator operativ l?uft (also einem Monat)
    
    global_xMarker<<-list(lastTrainDay, Today) #damit sie von indi.finish() als gr?ne abline angzeigt werdend
    lastTrainDay <<- Today
    frame=sprintf("%s::%s",TodayPrice-wlen,TodayPrice) #?ber diesen frame wird gefittet
    global_arg<<-list(clos=prices[frame,symbol])
    global_StartDate <<- Today
    
    indi = match.fun(indiName)
    runN<<-0  #nur damit man in indi.finish() sieht wieviele Trainingsl?ufe schon durch sind
    #Run mit alten Indi-Parametern 
    #(hab ja eigentlich die Trainingsdaten noch nicht gehabt)
    
    
    
    ind= indi(global_arg,visual=visual, main= "preTrainRun");
    #sichern der Signale des  monats.
    
    if (is.null(resIndi)) resIndi<<-ind$signal[workframe] else resIndi<<- append(resIndi,ind$signal[workframe])   
    
    doTrain =is.null(mlist(mode="rx"))  #in global_ParTable liegen noch keine Trainingswerte vor.
    if (doTrain )  #
    {
      TrainIndicator(global_StartDate_ = Today, opti=opti,indiName = indiName,visual=visual)  
      if (visual)
      {
        mP("postTrain")
        indi(global_arg,visual=T, main= "postTrain");
        browser()  #jetzt sollten brutal gute Werte vorliegen (volles in-sample-Training)
      }
    }
    
  })
  
  mP("tune.monthly finished")
  if (visual || T)   #der Ergebnis-Plot mit den monatlichen resIndi-Segment-Signalen
    plotSigPrice(signal=resIndi,prices=pricesSym,indi=NULL,main="final result ")  
  
  return(resIndi)  #der out-of-sample genutze Indikator
}


if (F)
{
  #MM_TUNE.MONTHLY
  tune.monthly(prices,symbol="TECDAX", indiName="indi.ZlemaDif",wlen=200,opti="GRID",trainLevel = 100)
  tune.monthly(prices,symbol="TECDAX", indiName="indi.SMA", wlen=600, opti="DEoptim",trainLevel = 20) #spannend
  tune.monthly(prices,symbol="TECDAX", indiName="indi.VAR", wlen=600,trainLevel = 100) #vermutlich super instabil !!
  
  tune.monthly(prices,symbol="TECDAX", indiName="indi.TSI", wlen=200,trainLevel = 100) 
  tune.monthly(prices,symbol="TECDAX", indiName="indi.MACD", wlen=200,trainLevel = 100) 
  tune.monthly(prices,symbol="TECDAX", indiName="indi.RSI", wlen=200,trainLevel = 100) #spannend
  tune.monthly(prices,symbol="TECDAX", indiName="indi.Omega", wlen=200,trainLevel = 100) #spannend
  
}

if (F)
{
  res2 =score.nK(prices["2006"],visual=T);
  mSlope90 <<- rollRegressionXTS(mNorm(prices),win=75)
  
  dLo=  rollapplyr(DaxLo, width=5, FUN=diffCount, by.column=T, align = "right")
  #bt.apply.matrix(ret,function(x) abs(compute.max.drawdown(x)))
  #q =q*100-nt*Tkosten
  # q=compute.sharpe(Ret)
  #compute.sharpe(mROC(prices))/compute.sharpe(ret)
  
  data.analy = data.analysis(data,"Mmain")
  ls(data.analy)
  tail(data.analy$target)
  data.analy$coint
  frame="2001::"
  #das gleiche environment wie data- aber beschr?nkt auf frame
  env.data=data.apply(data, function(x) x[frame,])  #ein auf frame geschrinkter Datenbereich
  df = m.apply(data,function(x) Hi(x)-Cl(x))
  x=m.apply(data, function(x) {print(colnames(x));Hi(x)-Cl(x)}, ignore=c("Zinsen"))
  
  data.analy = data.analysis(data,"Mmain")
  
  #t=data.Info(data,"Mmain")
}

##############################################################################
techStops.long<-function(price)
{  
  print(colnames(price))
  
  browser()
  Risk.es <- rollapplyr(na.omit(mROC(Lo(na.omit(price)))),FUN="ES",width=36,p=0.95,na.pad=TRUE)
  #take care of NA with 0 at beginning and interpolation at end
  Risk.es <- apply(Risk.es,MARGIN=2,FUN=na.fill,fill=c(0,"extend"))
  Risk.Es <-as.xts(Risk.es)
  
  ms=merge(Risk.Es,scaleTo(Lo(na.omit(price)),range(Risk.Es)))
  
  mchart(ms)
  lop=na.omit(Lo(price))
  mchart(mNorm(lop))
  plot(mROC(Risk.Es),ylim=c(-0.1,0.1))
  dms= ms[,1]-ms[,2]
  
  norm_Win(3)
  mchart(mROC(lop))
  mchart(mROC(Risk.Es))
  mchart(lop)
  plot(mROC(Risk.Es)-mROC(Lo(price)))
  
  risk.es=rollapplyr(ROC(na.omit(lop),n=15),FUN="ES",width=36,p=0.99)
  risk.var= rollapplyr(ROC(na.omit(lop),n=15),FUN="VaR",width=36,p=0.99)
  
  hp=(na.omit(ROC(Hi(price),5)))
  lp =(na.omit(ROC(Lo(price),5)))
  u= rollapplyr(hp[hp>0] , FUN="mean.UCL", width=28,p=0.99)
  l= rollapplyr(lp[lp<0] , FUN="mean.LCL", width=28,p=0.99)
  ##################################################
  #Konstruktion einer Supportgeraden (untere Kanalunterst?tzung)
  lomi= runMin(Lo(na.omit(price)),n=12)
  himi= runMax(Hi(na.omit(price)),n=12)
  
  mchart(merge( lomi,himi, Cl(price)))
  lows=HighLows(lomi,visual=visual)$lows 
  plot(mNorm(Lo(na.omit(price))))
  #die lage der lows
  lomi=lomi[lows]
  dlomi=sign(ROC(lomi,n=1))
  rl=runLengthEnc2(dlomi)
  segmente=HighLows(lomi,visual=T)$hl 
  lomi[segmente]
  #an welchen Tagen kommen Lows die rlp mal schon oberhalb ihres Vorg?nger-Lows lagen:
  rlp=rl[rl>0] #das sind jetzt die Punkte durch die sich support-reg-geraden lohnen
  
  lapply(c(1:len(rlp)), function(x) {
    browser()
    rlpx=rlp[x,]
    d2_now=as.Date(index(rlpx))
    p1=rlp[x-rlpx]                
  }
  )
  
  
  clr()
  mchart(merge(ROC(Cl(price),5),u,l))
  plot(risk.var)
  plot(risk.es)
  d = ROC(na.omit(lop),n=15)-risk.var
  plot(d)
  mchart(ROC(lop,n=15))
  mchart(risk.var)
  plot(lop)
  
  mean(lop,ci=0)
  
  
  plot(Risk.Es)
  mchart(dms)
  #  browser()
  bb=  BBands(HLC(price),n=150,maType = SMA)
  z<-Cl(price)
  z=ZLEMA(Lo(na.omit(price)),n=5)  #auf keine na.omit vergessen - sonst entsteht manchm ein vector statt ein xts
  sma=ZLEMA(Cl(na.omit(price)),n=190)
  #cudCl = runSum(ifelse(mROC(Cl(price)) > 0,1,-1),n=50)
  #  rL=runLengthEnc2(iif(mROC(Cl(price))>=0,1,-1))
  
  z=Lo(price)
  
  smaUp=EMA(Cl(price),n=180)
  
  
  Tstops = iif( z <= sma ,-1,1)
  #Tstops = iif( z <= bb[,"dn"] ,0,Tstops)
  
  #Tstops = iif( z >= smaUp & smaUp >= sma,1,Tstops)
  
  dema=EMA(diff(z-Cl(price)),n=70)
  D = runMax(dema,n=10)-runMin(dema,n=10)
  
  # Tstops = iif(Tstops==0  &  z >= bb[,"dn"],1,Tstops)
  
  plotSigPrice(signal=Tstops,prices=mNorm(Cl(price)),indi=list(sma=merge(z,sma)) )#, dema=merge(D,dema)))#merge(z,bb[,"dn"],bb[,"up"])))
  
  
  return(Tstops)
  #1 heisst long, 0 heisst stop
} #........................................................................


#MM_TODO hier noch  BBand und MaxDrawDown-Stop einf?gen
#Vorsicht:  dass es sich bei der Ecke d2_now um eine Ecke handelt wei? erst Tage sp?ter !!!
channelStop<-function(sym="DAX",visual=F,maxdd=5)
{
  if (visual)
  {
    print("#####################channelStop######################################")
    print(sym)
    print(tail(data[[sym]]))
    
    #browser()
  }
  price =na.omit(data[[sym]]["2002::"])
  
  #price = -mROC(price)  #die short-reihe
  #price = mRendite(price)
  #plot(price)
  price = mNorm(price)
  
  stopLine_<<-price[,c(1,2,3,4)] # supportLine, Kontakt, beta, offset Break
  stopLine_[] = NA
  lomi= runMin(Lo(na.omit(price)),n=20) #lag m??ig ok
  himi= runMax(Hi(na.omit(price)),n=20)
  
  #lomi= Lo(na.omit(price))
  #himi= Hi(na.omit(price))
  
  if (visual)
    mchart(merge( lomi,himi, Cl(price)))
  
  HL=  HighLows(lomi, maxdd=maxdd,visual=visual) #sagt mir nicht WANN die Ecke identifiziert wurde -mZigZag2 - wohl
  
  Lows=lomi[HL$lows]
  lomi=lomi[HL$lows]   #die Ecken
  himi= himi[HL$highs]
  
  dlomi=sign(ROC(lomi,n=1))
  dhimi=sign(ROC(himi,n=1))
  
  rl=runLengthEnc2(dlomi)
  
  rl[rl==-1]<-0  #auch das erste Low-Extremum einer Kette mitnehmen, dass noch keine Vorg?nger hat
  
  #an welchen Tagen kommen Lows die rlp mal schon oberhalb ihres Vorg?nger-Lows lagen:
  rlp=rl[rl>=0] #das sind jetzt die Punkte durch die sich support-reg-geraden lohnen .. werte von nacheinander rlp mal steigenden Knoten
  
  if (visual) print(rlp)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  lapply(c(1:len(rlp)), function(x) {
    
    #plot(Lo(price))
    #lines(Lo(price)[rlp+4],type="h")
    
    #browser(mP("++++++"))
    
    rlpx=rlp[x,]
    
    if (rlpx >= 1) #2 f?r reg werden wenigstens 2 st?tzstellen ben?tigt
    {      
      d2_now=as.Date(index(rlpx))
      #Lows
      if (F)
        if (rlpx==1)
          p1 = as.Date(index(last(Lows[which(index(Lows)< d2_now)])))
      else
      {
        p1_vorletzt=as.Date(index(rlp[x-rlpx+1]) )  
        p1 = as.Date(index(last(Lows[which(index(Lows)< p1_vorletzt)])))
      }
      p1=as.Date(index(rlp[x-rlpx]) )
      if (len(p1)==0)
        p1 = as.Date(index(last(Lows[which(index(Lows)< d2_now)])))
      if (visual)
        mP("%d St?tzstellen bei %s  (zur?ck bis %s) ",rlpx,d2_now,p1)
      supportPoints=lomi[sprintf("%s::%s",p1,d2_now),]
      if (visual)
        if (len(supportPoints) > 0) 
          print(supportPoints )
      else
      {print("NO support Points at %s",toString(d2_now))
       browser()        
      }
      if (len(supportPoints) > 0) 
      {
        segx1 = get.Index(price, index(first(supportPoints)))
        segx2 = get.Index(price, index(last(supportPoints)))
        
        #?bersichtsplot
        if (visual)
        {
          plot(price,main=sprintf("overview %s",toString(colnames(Lo(price))))  )
          lines(supportPoints,col="blue"); lines(supportPoints,type="p",col="red")
          lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
          browser()
        }
        #if (len(supportPoints)>2)
        # mchart(supportPoints)
        
        Hist=hist=Lo(price[seq(segx1,segx2),]);hist[]=NA
        Y= merge(hist,supportPoints)[,2]; colnames(Y)=c("y")   
        
        #X= coredata(rollapplyr(zoo(supportPoints), 1,function(x) get.Index(Lo(price),index(x)))) 
        X=get.Index(Lo(price),index(supportPoints))
        train <- data.frame(y=coredata(supportPoints), xt=X)
        colnames(train)=c("y","xt")
        #train=na.omit(train)  -- beta berechnen !!!!
        #mod= lm(y ~ xt, train)
        w=(X-X[1]+1) ; w = w/ last(w); w=expm1(10*w) #Gewichtung gem. Abstand - je h?her der factor in expm1 dest sch?rfer klingt der Einflu? der alten supportpoints ab   
        mod = lm(y ~ xt, train, weights=w)
        beta <- coef(mod)[2]  #beta-Berechnung !!!
        
        if (beta >= 0) #MM_TODO - vorsicht - bei negativen beta bist Du ungehedged
        {
          mod.b<-coef(mod)[1]
          b=coredata(last(supportPoints))  #first
          
          if (visual) #plot der hist-reg
          {
            plot(Hist,main="Hist-Test")
            lines(supportPoints,col="blue"); lines(supportPoints,type="p",col="red")
            lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
            browser()
            xi=0:(segx2-segx1)
            yi = beta*xi+b
            inHist = Hist;inHist[]=yi
            # lines(inHist,col="green")
            dy = as.numeric(last(inHist))-as.numeric(last(supportPoints))
            b=b-dy  #vertikal-verschiebung der reg-geraden auf den letzten supportPoint
            inHist[] = coredata(inHist)-dy
            lines(inHist,col="red")
            browser()
          }
          
          #schnittpunkt suchen...................
          
          xi=0:(len(price[,1])-1)
          yi = beta*xi+b
          
          pred.mod = Lo(price[]) ;pred.mod[]=yi
          # lines(inHist,col="green")
          dy = as.numeric(pred.mod[as.Date(index(last(supportPoints)))])-as.numeric(last(supportPoints))
          pred.mod[] = coredata(pred.mod)-dy
          frame=sprintf("%s::%s", as.Date(index(price[segx1,])),  as.Date(index(last(price))) )
          
          pred=Lo(price[frame,])
          if (visual)
          {
            plot(pred,main="suche stop")
            lines(pred.mod,col="green")
            
            lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
            #wo liegt der Schnittpunkt in der Zukunft ?
          }
          frame.fut=sprintf("%s::%s", as.Date(index(price[segx2,]))+1,  as.Date(index(last(price))) )
          pred.fut= pred[frame.fut,]; pred.mod.fut = pred.mod[frame.fut]
          
          schnittpunkt.date=as.Date(index(first(pred.fut[which(pred.fut<=pred.mod.fut)])))
          schnittpunkt = get.Index(price,schnittpunkt.date)
          
          hasStop=""
          if  (schnittpunkt==0)
          {
            schnittpunkt.date= as.Date(index(last(price)))
            schnittpunkt = get.Index(price,schnittpunkt.date)
            hasStop="NO "
          }
          
          schnittpunkt.price=Lo(price)[schnittpunkt.date]
          
          frame.cut=sprintf("%s::%s", as.Date(index(price[segx1,])),  schnittpunkt.date+10 )
          
          if (hasStop!= "NO")
            mP("Cut at %s",schnittpunkt.date)
          
          if (visual)
          {
            plot(Lo(price[frame.cut,]),main=sprintf("%s CUT %s",hasStop,schnittpunkt.date))
            
            lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
            
            if (hasStop!= "NO")
            {
              abline(h=Lo(price)[schnittpunkt.date],col=col.add.alpha("darkblue" , 95),lwd=3)
              abline(v=as.POSIXct(schnittpunkt.date),col=col.add.alpha("darkblue" , 95),lwd=3)
            }
            lines(pred.mod[frame.cut,],col="red")
            browser()
          }
          
          #patche die Stop-Limits in die Kurve ~~~~~~~~~~~~~~~~~~~~~~
          #?berschreibe dabei durchaus alte Linien-Werte .. die nun pr?zisiert werden - wichtig:du darfst das nur machen wo Du nicht schort sein solltest
          #liegen auf NA
          #d2_now (== segx2) ist immer  "today"
          #seit der letzten Analyse k?nnen aber eine ganze Reihe von Tagen vergangen sein (weil er immer nur auf Lo-Ecken analysiert)
          
          frame.fut=sprintf("%s::%s", as.Date(index(price[segx2,]))+1,  as.Date(index(last(price))) )
          
          if (beta > 0)
            stopLine_[frame.fut,1] <<- pred.mod[frame.fut,1]
          
          #setze die zuk?nftigen stopLine_Werte- die unter dem aktuellen LossLimit
          
          # S=stopLine_[frame.fut,1 ]
          
          #  stopLine_[frame.fut,1] <<- S 
          
          if (hasStop != "NO")  #in spalte 2 werden die dat?mer der Kontakte geschrieben
          {
            
            stopLine_[schnittpunkt.date, 2] <<- Lo(price[schnittpunkt.date])
            stopLine_[d2_now,3]<<-beta  #beta
            stopLine_[d2_now,4]<<-b-dy  #offset
          }
          if (visual)
          {
            mchart(merge(Lo(price),stopLine_[,1]),main="StopLine_ all")
            lapply(na.omit(stopLine_[,2]),function(x)  abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkblue" , 95),lwd=3))
            abline(v=as.POSIXct((index(last(supportPoints)))),col=col.add.alpha("green" , 95),lwd=3)
            
            browser()
            
          }
        } #beta > 0
      }#len(supportPoints)>0
      
      return(1)  #des lapply (unwichtig)
    }#if (rlpx >= 1)
  } #function()
  )#lapply
  if(F)
  {
    mP("mmmm")
    #plot(merge(Lo(price),stopLine_[,1]),main="StopLine_ Result",type="h")
    plot(Lo(price),main="StopLine_ Result")
    lines(stopLine_[,1], col="red",type="h")
    lines(Lo(price),col="black")
    lapply(na.omit(stopLine_[,2]),function(x)  abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkblue" , 95),lwd=3))
    
    #jetzt stehen die support-Lines und ich muss mich entscheiden ob ich den Kontakt als Reflexion oder Break werte
    mP("decide:  reflection or break")
    #sobald er mindestens 2 Tage >= Kanal-stop-Wert liegt, ist  ein Break.
  } 
  
  stopSignal = iif( Lo(price)< stopLine_[,1]  & lag(Lo(price))< stopLine_[,1]| is.na(stopLine_[,1]) ,-1,1)
  
  #  stopSignal = iif( Lo(price)< stopLine_[,1]  & lag(Lo(price))< stopLine_[,1] ,-1,1)
  
  
  
  View(merge(Lo(price),stopLine_)["2012-04::2012-05",])
  #  stopSignal = iif(  is.na(stopLine_[,1]) ,-1,1)
  
  #mP("plotSigPrice....")
  #browser()
  ##### typischer Schluss einer signal.<>- methode
  plotSigPrice(signal=stopSignal,prices=mNorm(Lo(price)),indi=list(stopLine=merge(Lo(price),stopLine_[,1])) )
  
  return(stopSignal)
}

if (F)
{
  #price = na.omit(data[[data$symbolnames[i]]]["2002::"])
price=p
  channelStop()
  mchart(merge(Lo(price),stopLine_[,1])["2009"])
  
  for(i in c(1:len(data$symbolnames)))
  {
    sym =data$symbolnames[i]
    channelStop(sym)
  }
  
  channelStop(sym="SXTBP",maxdd=10)  
  channelStop(sym="USDEUR",maxdd=1)  
  channelStop(sym="DAX",maxdd=10)  
  
  abline(v=as.POSIXct(as.Date("2008-11-21" )),col="blue",lwd=1)
  abline(v=as.POSIXct(as.Date("2009-01-23" )),col="blue",lwd=)
  abline(v=as.POSIXct(as.Date("2009-03-09" )),col="blue",lwd=3) #das Ende des 2008 -crash
  abline(v=as.POSIXct(as.Date("2009-07-13" )),col="blue",lwd=1)
}

if (F)
{
  
  channelStop()
  i=4
  techStops.long(na.omit(data[[data$symbolnames[i]]]["2002::"]))
  techStops(na.omit(data[[data$symbolnames[i+1]]]["2002::"]))
  techStops(na.omit(data[[data$symbolnames[i+2]]]["2002::"]))
  techStops(na.omit(data[["DAX"]]["2002::"]))
  
  techStops(P)
  
  #ls(data)
  #price = data[["EMBI"]]
  Load("Mmain")
  prices =data.info(data,ignore=c("VDAX","Nasdaq100")) 
  purePlot(prices)
  
  frame="2007::"
  prices= m.apply(data,function(x) mNorm(na.omit(Cl(x[frame])))) #ein xts mit allen Daten
  mchart(prices)
  #Tstops = bt.apply.matrix(prices,function(x) {techStops(x)})
  Tstops = m.apply(data,function(x) techStops( mNorm(na.omit(x[frame,])))) #ein xts mit allen Daten
  #Tstops = global_target #TEST
  
  pdf= sprintf("Models/%s/%s.pdf",dataSet,"mMain_Inputdaten.pdf");
  pdf(file = pdf, width=11, height=8.5) 
  layout(2)
  tail(na.omit(data$DAX))
  plot(data$DAX) 
  
  for (i in c(1:len(data$symbolnames)))
  {
    pi=na.omit(data$prices[,i])
    vt=fromTo(pi)
    tsD=sprintf("%s::%s",vt[1],vt[2])
    plotSigPrice(signal=na.omit(Tstops[tsD,i]),prices=pi,indi=list())#merge(bb[,"dn"],bb[,"up"],z),cudLos)  )
  }
  dev.off()
  
}

##############################################################################
#D) Aufruf des allocations-Systems - endet mit Trades und Performance

#MMEM2 
if (F)  #..................... DEMO
{
  
  data = global_arg$dat
  prices= data$prices = global_arg$clos
  
  #########################################
  mP("Anzeige des InputDaten-Universums")
  
  xyplot(mNorm(prices))
  purePlot(mNorm(prices))
  index = prices[,1]; index[] = mRendite(rowSums(mROC(prices))/ dim(prices)[2])
  lines(index,lwd=4,col="red")
  browser()
  print("nun 2012")
  purePlot(mNorm(prices["2012::"]))
  index = prices["2012::",1]; index[] = mRendite(rowSums(mROC(prices["2012::"]))/ dim(prices)[2])
  lines(index,lwd=4,col="red")
  
  Cols=c(1:(len(models)+len(colnames(prices))))
  try(chart.RiskReturnScatter(mROC(prices),  main = sprintf("Compare prices"), ,colorset = Cols))
  try(chart.RiskReturnScatter(mROC(prices["2012"]),  main = sprintf("Compare prices from 2012"), ,colorset = Cols))
  
  #########################################
  mP("technische Stops berechnen (ohne Parameter - Fitting)")
  
  Tstops <<- lag(m.apply(data, function(x) {techStops(x)})  ) 
  
  mP(".... stops anzeigen ... ")
  for (i in c(1:len(data$symbolnames)))
  {
    pi=na.omit(data$prices[,i])
    vt=fromTo(pi)
    tsD=sprintf("%s::%s",vt[1],vt[2])
    plotSigPrice(signal=na.omit(Tstops[tsD,i]),prices=pi,indi=list())#merge(bb[,"dn"],bb[,"up"],z),cudLos)  )
  }
  
  browser()
  #############################################
  
  print("zeige verschiedene prognosefreie AA-Methoden  und danach das selbe nach einer nTopK-Vorselektion")
  browser()
  
  riskFreePos<<-"TREASURY"
  
  mm_AA(data, period = "months",n.top=6, k.top=12, version="_MW_stopF", AAignoreStopped=F, Tstops=NULL,     riskFreePos="TREASURY")
  
  
  print("jetzt den nTopK-AA - ohne t?gliche Timingstops")
  browser()
  
  mm_AA(data, period = "months",n.top=10, k.top=15, version="_MW_stopT", AAignoreStopped=T, Tstops = Tstops,        riskFreePos="TREASURY")
  
  ## andere n,K 
  
  ## optimierten Selektions-Parameter w?hlen
  
  # trainiertes Stops-System w?hlen 
  
  
  # anderes Universum
}



###############################################################################

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
  
  
  
  
  #browser()  
  if (T)   #KLASSSE
  {
    mSlope90 <<- rollRegressionXTS(mNorm(data$prices),win=90)
    
    signal =iif( mSlope90 > 0,1, 0.00000001)
    position.score =lag(signal)*mSlope90*10000 
    #position.score[mSlope90<0]<-NA
    colnames(position.score)=colnames(prices)
    experiment="AA mSlope90"
    
    
    if (T)
    {
      data$prices=prices
      data$weight = prices
      data$weight[] = ntop(prices, n)
      ew = bt.run(data)  
      mP("###### Portfolio-STOPS #######")
      
      browser() 
      
      ew$Tstops=prices[,1]; ew$Tstops[]=NA
      ew$Tstops = merge(ew$Tstops,techStops(to.weekly(ew$equity)))[,2]
      
      #na.locf(ew$Tstops,na.rm=FALSE)
      ew$Tstops=m.ifna.prev(ew$Tstops)
      
      
      data$weight[] = ntop(prices, n.top)
      ew.kTop = bt.run(data)  
      browser() 
      ew.kTop$Tstops= techStops(to.weekly(ew.kTop$equity))
      
      browser()
      
      #merge(bb[,"dn"],bb[
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
  
  ##########################################################################
  
  universe = ntop.keep(position.score[period.ends,], n.top, k.top)  
  colnames(universe)=colnames(prices)
  
  #*****************************************************************
  # Code Strategies that rebalance based on maximum deviation
  #****************************************************************** 
  
  # rebalance to target.allocation when portfolio weights are 5% away from target.allocation
  # models$smart5.all = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 5/100, 0) 
  
  #if (F)
  #die Rest-Gewichte mit dem REX auff?llen
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
    mP("---------------------- prognosefreies- Portfolio mit s?mtlichen %d Titeln  ->",n)
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
  
  browser()
  mP("schreibe pdf und xls-Dateien")
  pdf_Report(models,experiment)
  plotbt.custom.report.part2(models$MS)       
  
  mP("--- Ende der Demo ---")
  browser()
  
}
#########################################################################
