
#################################################################################
# Batchrun zum Ablauf meines Systems
#################################################################################

prepare_System<-function()
{
  
  loadPortfolios(customerData)  #customerDate = "Models
  x=PortfolioTicks("Meurope")$Ticker
  
  tail(data[[data$symbolnames[13]]])
  
  #Rohdaten laden  
  Meurope <<- T2$new(PortfolioName = "Meurope", bench="Dax",visual = F, online=F)
  data<-Meurope$t0data
  
  
  Mworld <<- T2$new(PortfolioName = "Mworld", bench="Dax",visual = F, online=T )
  data<-Mworld$t0data
  
  
  #ls(Mworld$t0data)
  
  #shared prices finden
  prices <<- data.info(data,visual=T)#,ignore=C("VDAX"))
  #trainings-zeitraum beschränken
  train.frame = "::2014"
  #mchart(mNorm(prices["2013-03::"]))
  
  #Training vorbereiten
  global_commission <<- 0.00001
  
  global_arg<<-list(clos=prices[train.frame,],dat = data)  #Definition der Preise
  tail(global_arg$clos)
  
  global_ParTable <<-NULL  
  global_StartDate <<- DateS(last(prices[train.frame])) 
  global_StartDate <<- "2012-12-14"
  
  
  #ich kann das Training gröber oder feiner gestalten indem ich die Variable
  #global_FastTrain > 0 setzte. dies ist dann die Anzahl der Zwischenschritte pro Dimension
  global_FastTrain <<- 2
  TrainIndicator ( opti="GRID",indiType= "TREND",indiName = "signal.SMAzlema",visual=F,TRAINSYM=-1)  
  i.system=indi.Generic("signal.SMAzlema", global_arg,visual=T, TRAINSYM=-1,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  #global_objectId <<-paste("TREND","AllSignal","signal.techStops")   #Welcher Indikator 
  
  #mlist(bbM=c(100,100,300,150),smaN=c(20,20,120,60), zlemaN=c(5,4,10,5),mode="w")
  TrainIndicator (data=data,opti="GRID",indiType= "TREND", indiName = "signal.techStops",visual=T,TRAINSYM= c(1,2),frame=train.frame)  
  
  
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=c(1,2),do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  
  
  TrainIndicator (data=data,opti="GRID",indiType= "TREND", indiName = "signal.techStops",visual=T,TRAINSYM= 0,frame=train.frame)  
  
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=0,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  TrainIndicator (data=data,opti="GRID",indiType= "TREND", indiName = "signal.techStops",visual=T,TRAINSYM= 0,frame=train.frame)  
  
  
  global_objectId <<-paste("TREND","Dax","signal.techStops")
  
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=-1,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=-1,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  
  #Scoring vorbereiten
  
  R = mROC(prices)
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  
  someTableData = list()
  
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
  
  scoreScore(prices,i.system, n=6,K=12,someTableData=someTableData) 
  
  showScoreScores()
  
}

if (F)
  prepare_System()

if (F)
{
  global_ParTable$par
}


mlist(mode="R")