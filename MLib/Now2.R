
#################################################################################
# Batchrun zum Ablauf meines Systems
#################################################################################

prepare_System<-function(PortfolioName = "Mworld", n=6, K=12)
{
  
  loadPortfolios(customerData)  #customerDate = "Models
  
  Mworld <<- T2$new(PortfolioName = PortfolioName, bench="Dax",visual = F, online=F )
  data<-Mworld$t0data
  
  PortfolioTicks(PortfolioName)$Ticker
  PortfolioTicks(PortfolioName)$Name
  
  #ls(Mworld$t0data)
  
  #shared prices finden
  prices <<- data.info(data,visual=T)#,ignore=C("VDAX"))
  #mdata= data.analysis(data, xlsFile="DataAnalysis", mode=spl("price"))
  #..................................................................
  global_Targets<<-tryM(compute.Targets(prices,maxDDtoleranz=0.16, minReturn=0.05))
  
  
  #.......................................................................
  
  #trainings-zeitraum beschränken
  train.frame = "::2014"
  #mchart(mNorm(prices["2013-03::"]))
  
  #Training vorbereiten
  global_commission <<- 0.00001
  
  global_arg<<-list(clos=prices[train.frame,],dat = data)  #Definition der Preise
  
  global_ParTable <<-NULL  
  
  global_StartDate <<- "2012-12-14"
  global_StartDate <<- DateS(last(prices[train.frame])) 
  
  
  #ich kann das Training gröber oder feiner gestalten indem ich die Variable
  #global_FastTrain > 0 setzte. dies ist dann die Anzahl der Zwischenschritte pro Dimension
  global_FastTrain <<- 2   ###sehr wichtig für die Laufzeit des Trainings!
  
  #............... All Symbols with indivisual parameters.................
  
  TrainIndicator (data=data,opti="GRID",indiType= "TREND", indiName = "signal.techStops",visual=T,TRAINSYM= -1,frame=train.frame)  
  
  i.system<<-indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=-1,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  #Bastel die singelEquities zu einem einheitlichen xts-Objekt zusammen
  singleEquities =  data.frame(lapply(i.system$singleResults,function(x) 
  {
    x$equity
  }))
  
  singleEquitiesXts <<- as.xts(singleEquities,order.by=as.Date(rownames(singleEquities)))
  
  tail(singleEquitiesXts)
  #mchart(singleEquitiesXts)
  
  
  #Scoring vorbereiten.....................................................
  
  R = mROC(prices)
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  
  
  someTableData = list() ### Gesamtzeitreihen fürs Scoring vorbereiten
  
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
  
  someTableData$mRoc90=ROC(mNorm(prices),n=90)
  
  someTableData$mRoc120=ROC(mNorm(prices),n=120)
  someTableData$mRoc30=ROC(mNorm(prices),n=30)
  someTableData$mRoc60=ROC(mNorm(prices),n=60)
  someTableData$mRoc180=ROC(mNorm(prices),n=180)
  
  #i.system:   nimm die signale des (der) technischen systeme mit ins scoring auf
  #produzierte global_score
  
  #n=6
  #K=12
  
  scoreScore(prices,i.system, n=n,K=K,someTableData=someTableData,visual =T) 
  
  scoreScore(prices,i.system, n=n,K=K,visual =T) 
  
  global_score2<<-calc.score.nk(prices=prices,n=n, K=K, wlen=150)
  
  #showScoreScores()
  
}

if (F)
  
{
  system.time(  
    prepare_System(PortfolioName = "Mworld",  n=6, K=12) )
  #die Kurse
  purePlot(prices[,-which(colnames(prices)=="GOLD")]) #die charts der topN
  #ergebnisse des compute.Targets() 
  global_Targets
  #ergebnisse des TrainIndicators() 
  global_ParTable$par  
  #ergebnisse des indi.Generic()
  ls(i.system)
  head(i.system$Signal)
  mchart(singleEquitiesXts)
  #ergebniss des scoreScore()
  G<-data.frame(global_score)
  View(G)
  
  #vergleiche den Mittelwert der topK mit dem Mittelwert aus allen prices

  buyAll=mNorm(prices)
  #alle Preise
  buyAll[,1]=  mRendite(rowSums(mROC(prices)) / dim(buyAll)[2])
  G<-data.frame(mNorm(prices)*global_score)
  
  buyAll[,2]=  mRendite(rowSums(mROC(prices)*mlag(global_score)) / n)
  
  plot(buyAll[,1],ylim=c(1,max(buyAll[,1],buyAll[,2])),main="Vergleich BuyHold mit nTopK gleichgewichtet")
  lines(buyAll[,2],lwd="3",col="red")     #der Mittelwert der Auswahl
  lines(buyAll[,1],lwd="3",col="black")    #der Mittelwert von BuyAll 
}



#head(mlag(global_score))
###############################################################################

test_prepare_System<-function()
{
  
  loadPortfolios(customerData)  #customerDate = "Models
  x=PortfolioTicks("Meurope")$Ticker
  
  tail(data[[data$symbolnames[13]]])
  
  #Rohdaten laden  
  Meurope <<- T2$new(PortfolioName = "Meurope", bench="Dax",visual = F, online=F)
  data<-Meurope$t0data
  
  
  Mworld <<- T2$new(PortfolioName = "Mworld", bench="Dax",visual = F, online=F )
  data<-Mworld$t0data
  
  PortfolioTicks("Mworld")$Ticker
  PortfolioTicks("Mworld")$Name
  
  #ls(Mworld$t0data)
  
  #shared prices finden
  prices <<- data.info(data,visual=T)#,ignore=C("VDAX"))
  prices <<- data.info(data,visual=T,ignore="GOLD")
  
  head(prices)
  #trainings-zeitraum beschränken
  train.frame = "::2014"
  #mchart(mNorm(prices["2013-03::"]))
  
  #Training vorbereiten
  global_commission <<- 0.00001
  
  global_arg<<-list(clos=prices[train.frame,c(5,6,7)],dat = data)  #Definition der Preise
  global_arg<<-list(clos=prices[train.frame,],dat = data)  #Definition der Preise
  
  tail(global_arg$clos)
  ls(global_arg$dat)
  global_arg$dat$symbolnames
  
  global_ParTable <<-NULL  
  
  global_StartDate <<- "2012-12-14"
  global_StartDate <<- DateS(last(prices[train.frame])) 
  
  
  #ich kann das Training gröber oder feiner gestalten indem ich die Variable
  #global_FastTrain > 0 setzte. dies ist dann die Anzahl der Zwischenschritte pro Dimension
  global_FastTrain <<- 4
  
  #.....................................................................
  
  sym="DAX"
  TrainIndicator ( opti="GRID",indiType= "TREND",indiName = "signal.SMAzlema",visual=F,TRAINSYM=sym  )
  
  i.system=indi.Generic("signal.SMAzlema", global_arg,visual=T, TRAINSYM=sym,do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  #............... All Symbols with indivisual parameters.................
  
  TrainIndicator (data=data,opti="GRID",indiType= "TREND", indiName = "signal.techStops",visual=T,TRAINSYM= -1,frame=train.frame)  
  
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=-1,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  #............... AllSymbols with common parameters.................
  
  TrainIndicator (data=data,opti="GRID",indiType= "TREND", indiName = "signal.techStops",visual=T,TRAINSYM= 0,frame=train.frame)  
  
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=0,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  #..........................................................................
  
  mlist()  
  G=global_ParTable
  global_ParTable$par
  global_objectId <<-paste("TREND","AllSymbols","signal.techStops")
  
  i.system=indi.Generic("signal.techStops", indiType= "TREND",global_arg,visual=T, TRAINSYM=-1,do.assemble.Signals=T)  #jeder mit seinen eigenen BestParams
  
  
  i.system=indi.Generic("signal.techStops", global_arg,par = mlist(bbM=c(166,100,300,150),smaN=c(120,20,120,60), zlemaN=c(6,4,10,5)) ,visual=T, TRAINSYM=c(1,2,3),do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  global_ParTable =NULL
  
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=0,do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  
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
  
  scoreScore(prices,i.system, n=6,K=12,someTableData=someTableData,visual =T) 
  
  showScoreScores()
  
}

