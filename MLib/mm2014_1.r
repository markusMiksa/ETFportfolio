############################################################################################
# Das CodeFile für die Rosenmontags-Präsentation :  die technischen Produkte die in
#beschrieben sind:  o:\R\Nuggets\Eckhard\Docu\TSA_EuropaBranchen-technisch.v3.pptx
####################
#Springe zur Marke  #segler   #MultiSig   #MULVAR
#######################################################################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
#install.packages("PApages", repos="http://R-Forge.R-project.org")
#install.packages("factorAnalytics", repos="http://R-Forge.R-project.org")
options(error = browser)

#Marken:   #RUN  #TRAIN  
#MBRA   multi-branchen-modell .. alle timing modelle verdichten
#MMX    multi-top-branchen-modell .. die zeitlich rollierenden top modelle
#segler    #TODO an dieser Markte  weiter testen !!
############################################################################################
# TSA
#############################################################################################
# U: 
#    das Eurostoxx-Branchen .. Portfolio  (alles Intermarket - keine Fundis)
# T:
# optimiere rollierend die Fensterbreite eines faber-signal-modells mit TrainIndicator
#  opt: maxWlen / fix.Hysterese 
#  opt: statt reinem Trenfolger noch ein randForest in Beratungs-Situationen
#       MultiVar-Boost:  LeadLag-Analyse + Coint + FundamentalMining
# S: 
# ranking:slope300,  opt: beta  
# A: standards (->Sharpe = 1)  - opt: RegNoud  
#######################################################################################
source("Mlib/work_now.r")
#source("Mlib/labor_signal.r")
library(caret)
visual=F
##################################### MAIN #######################################


if (F)
  {
  load("Models/HuAFeb_3/Results.xls/HuAEuroBra_F.data")
  m=dels2flatList(models,datafile="HuAEuroBra_F")
  ls(m)
  
  load("Models/HuAFeb_4/Results.xls/HuAWorld_F.data")
  m=dels2flatList(models,datafile="HuAWorld_F")
  ls(m)
  load("Models/EM4_MAERZ_C/Results.xls/test_F.data")
  m=dels2flatList(models,datafile="HuAWorld_F") 

}
########### baue eine liste aus  per - Tabellen - pro Modell eine tabelle:
#spalten:  period,  cagr, maxdd, sharpe ...
models2PerTable<-function( models , period = c("all","years"))  
{
  
}
# .. arbeitet mit models2List()
############## baue eine Tabelle  spalten: period, modelName, RelQuality, Rank
modesls2PerfRanking<-function(models)
{
  
}
#.. arbeitet mit models2PerTable()

##############################################################################################
# so eine art "sterne" - ranking für die modelle 
##############################################################################################
## welches signal ist (für welches symbol) am besten ?
find.best.signals.todo<-function(models, period ="years", forSymbol = data$BENCH)
{
   Years = select.dates(models[[1]]$equity,mode=period)#"years")
     m=models.Results(models,frame=period,rank.for="sharpe",ignore.signal.models=T)$signal.mod 

}

#.................................................................................................

RunAllUniverses<-function()
{
  #load.universe<-function(universe="MWorld3",online=F,visual=F,TargetMethod=NULL)
  
  all.sig=  spl("signal.sit.feilscher,signal.lm,signal.mom,signal.Faber.base,signal.1.smoothing,signal.any.smoothing, signal.Price.itp,signal.Faber.dyn.hysterese,signal.drawDown1,signal.MA.3,signal.days.since.high,  signal.Faber.i2,signal.rwf,signal.SMAzlema,signal.zma")
  all.rank=  spl("rank.slope300,rank.momVolslope,rank.faber")
  
  model.ig=indi.Generic("signal.Faber.i2", global_arg,visual=F, TRAINSYM =-1)
  fromToS(model.ig$eq)
  
  performance.numbers(model.ig,frame="2004::2006")
  
  ls(ig)
  #all.rank =
  #1.4,   1.2
  
  
  load("MWorld3");  
  data <-readRDS("MWorld3.rds");
  data$universe="MWorld3"
  define.Globals();   SAFE="BARCLAYS"
  
  
  for (rankfn in all.rank) 
    A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
          S.arg=list(ranking.fn=rankfn, nTop.q=1.8,kTop.q=1.4),
          experiment=sprintf("A_%s_%s",data$universe,rankfn),   safe=SAFE,all=F) 
  #....................................................
  
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals();   data$SAFE=SAFE="BARCLAYS"
  dataSet="HuaFeb_5"
  
  
  for (rankfn in all.rank) 
    A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
          S.arg=list(ranking.fn=rankfn, nTop.q=1.8,kTop.q=1.4),
          experiment=sprintf("A_%s_%s",data$universe,rankfn),   safe=SAFE,all=F) 
  
  # ...... das 4- Titel Universum von Herrn Segler ................................
  
  data=universe_hua(reload=F)
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuAEurope4",LongShort="F",pdf=T,visual=T) 
  
  for (rankfn in all.rank) 
    A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
          S.arg=list(ranking.fn=rankfn, nTop.q=1.8,kTop.q=1.4),
          experiment=sprintf("A_%s_%s",data$universe,rankfn),   safe=SAFE,all=F) 
 
  # ...... das 15- Titel Universum von Herrn #Segler ................................
  
  data=universe_hua_World(reload=F)
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuAWorld",LongShort="F",pdf=T,visual=T) 
  
  if (F)
  {
    load("MWorld3");  data$universe="MWorld3"
    define.Globals();   SAFE="BARCLAYS"
    
    
  load("Models/HuAFeb_3/Results.xls/HuAWorld_F.data")
  models=dels2flatList(models,datafile="HuAWorld_F")
  models2=dels2flatList(models,datafile="A_hua_World")
  models= load.models("HuaFeb_3","hua_World_signal")
  
  models= load.models("HuaFeb_3","A_hua_World")
  
  ls(models)
  m=models.Results(models)
  Frame="::"
  m=models.Results(models,frame=Frame,visual=F) ####### ..######  ok
  
  View(m$portfolio.mod)
  View(m$signal.mod)
  purePlot(mNorm(data$prices[Frame,]))
  ls(models[["signal.1.smoothing"]])
  lines(mNorm(models[["signal.1.smoothing"]]$equity[Frame]),col="red",lwd=5)
  }
  
  for (rankfn in all.rank) 
    A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
          S.arg=list(ranking.fn=rankfn, nTop.q=1.8,kTop.q=1.4),
          experiment=sprintf("A_%s_%s",data$universe,rankfn),   safe=SAFE,all=F) 
  
}

###############  alle modelle für alle universen aktualisieren 
##################################################################
RunAllUniverses2<-function()
{
  #load.universe<-function(universe="MWorld3",online=F,visual=F,TargetMethod=NULL)
  
  
  load("MWorld3");  data$universe="MWorld3"
  define.Globals();   SAFE="BARCLAYS"
  #load(file="Models/HuA_Jan1/Hua_.data")  
  
  
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file=data$universe,LongShort="F",pdf=T,visual=T) 
  
  A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
        S.arg=TSA.default$S.arg,
        experiment=sprintf("A_%s",data$universe),   safe=SAFE,all=F) 
  #....................................................
  
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals();   SAFE="BARCLAYS"
  
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file=data$universe,LongShort="F",pdf=T,visual=T) 
  #load(file="Models/HuAFeb_3/HuA_F.data")  
  
  
  A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
        S.arg=TSA.default$S.arg,
        experiment=sprintf("A_%s",data$universe),  max.product.exposure = 0.8, safe=SAFE) 
  
  # ...... das 4- Titel Universum von Herrn Seger ................................
  data=universe_hua(reload=F)
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuAEurope4",LongShort="F",pdf=T,visual=T) 
  
  A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
        S.arg=TSA.default$S.arg,
        experiment=sprintf("A_%s",data$universe),  max.product.exposure = 0.8, safe=SAFE) 
  
} #.........................

if (F)
{
  RunAllUniverses()
  
  models= load.models("HuaFeb_3","MWorld3")
  
  m=models.Results(models)
}

if (F)  #schnell-Start der Umgebung
{
  if (F)
  { 
    rm(list= ls());    
  }
  source  ("MLib/TSA.r"); 
  #init.Code()
  
  load("stoxx_branchenpure")
  load("stoxx_branchenallEurope")
  
  universe_stoxx_branchen(mode="pure")
  define.Globals()
  
  
  
  all = indi.Generic("signal.MA.3", global_arg, par=list(zlemaN=10,slow=60,fast=10),
                     visual=T, TRAINSYM =-1,safe=SAFE,  
                     S.arg=list(ranking.fn="rank.slope300",nTop=2,kTop=3,do.assemble.Signals=F),pdfFile="f"  )
  
  
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.slope300",nTop=2,kTop=2),
        experiment="EuroHua_slope300_03",  max.product.exposure = 0.3, safe=SAFE, onlyUsr=F) 
  
  #........................................................

  #Seger-World-Universe:  reines blankes Gold
  universe_hua_World(reload=F)
  define.Globals(dataSet="HuAFeb_4b")
  data$SAFE=SAFE="BARCLAYS"
  
  all = indi.Generic("signal.MA.3", global_arg, par=list(zlemaN=10,slow=60,fast=10),
                     visual=T, TRAINSYM =-1,safe=SAFE,  
                     S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4,do.assemble.Signals=F),pdfFile="f"  )
  
  all = indi.Generic("signal.rwf", global_arg, par=list(win=150),
                     visual=T, TRAINSYM =-1,safe=SAFE,  
                     S.arg=list(ranking.fn="rank.slope300",nTop=7,kTop=9,do.assemble.Signals=F),pdfFile="f"  )
  
  
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.slope300",nTop=6,kTop=8),
        experiment="HuaWorld_slope300_6_8",  max.product.exposure = 1, safe=SAFE, onlyUsr=F) 
  
  
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuAWorld",LongShort="F",pdf=T,visual=T) 
  
  ':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  '
  models= load.models("HuaFeb_4b","hua_World")
  ls(models)
  #load("Models/HuAFeb_4b/Results.xls/hua_World_signal.MA.3rank.slope300f.data")
  #m=dels2flatList(models,datafile="HuAWorld")
  
  best=find.best.portfolio.model(models, period ="years",visual=T,rolling.mode=T)
  
}

#MULT  imodel
main_hua<-function()  
  #############  neues ranking:  diesmal nicht mit slope auf die kurse
  ############### sondern direkt auf die equity der Timing-Modelle obigem lauf 
{
  universe_stoxx_branchen(mode="pure")
  universe_stoxx_branchen(mode="allEuro")
  
  global_arg$dat <-data
  global_arg<<-list(clos=data$prices,dat=data)
  global_commission <<- 0.0005   #5 basipunktesys.Run() nimmt diese commission !!!!
  global_StartDate <<-  DateS(last(data$prices))
  #globalTrainLevel <<- -3   
  global_FastTrain <<-   -10  #keine Einschränkung
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  SAFE <<- "BARCLAYS"
  
  global_arg$modell.par$LongShort = "F"
  data$BENCH
  #.....................
  
  #ranking=equity.ranking(x5.1,win=100) ; ranking.name  ="equity.Slope"  #doesnt perform  
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuAEurope4",LongShort="F",pdf=T,visual=T) 
  load(file="Models/HuAFeb_3/HuA_F.data")  
  load(file="Models/HuAFeb_3/HuAEurope4.data")  
  load(file="Models/HuAFeb_3/HuAEurope4.data")
  #####
  #MT  
  all = indi.Generic("signal.MA.3", global_arg, par=list(zlemaN=10,slow=60,fast=10),visual=F, TRAINSYM =-1,safe=SAFE,
                     T.arg=list(cashed="signal.MA.3"),  
                     S.arg=list(ranking.fn="rank.slope200",nTop=16,kTop=20,do.assemble.Signals=F))
  #das timing im pdf-dokumentieren  (ordner dataSet)  
  all = indi.Generic("signal.MA.3", global_arg, par=list(zlemaN=10,slow=60,fast=10),visual=T, TRAINSYM =-1,safe=SAFE,
                     T.arg=list(cashed="signal.MA.3"),  
                     S.arg=list(ranking.fn="rank.slope200",nTop=16,kTop=20,do.assemble.Signals=F),,experiment="signalMa3stoxxBra")
  
  
  ####### #gutes beispiel für tsa
  merk= global_warte;  global_warte <<-F
  
  all = indi.Generic("signal.MA.3", global_arg, par=list(zlemaN=10,slow=60,fast=10),visual=T, TRAINSYM =-1,safe=SAFE,
                     experiment=sprintf("signal.MA.3"), 
                     T.arg=list(cashed="signal.MA.3"),  
                     S.arg=list(ranking.fn="rank.slope200",nTop=16,kTop=20,do.assemble.Signals=F))
  
  #................
  
  
  TrainIndicator( indiName = "signal.Faber.dyn.hysterese",  visual=T, TRAINSYM =-1 )#, roll.mode="years")
  
  all = indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,safe=SAFE,
                     experiment=sprintf("signal.Faber.dyn.hysterese"), 
                     T.arg=list(cashed="signal.Faber.dyn.hysterese"),  
                     S.arg=list(ranking.fn="rank.slope200",nTop=16,kTop=20,do.assemble.Signals=F))
  
  
  purePlot(modelEQ(models),main="model equities - not trained")
  models.pure=clone(models) 
  
  sprices=prices[,c(data$BENCH,SAFE)]  
  
  models$branchenMittel=branchen.model
  purePlot(merge(modelEQ(models),mNorm(sprices)),main="model equities - not trained")
  
  perf=Performance.eq(modelEQ(models),do.print=T)
  write.table(perf, "clipboard", sep="\t")
  
  
  Cols=c(1:(len(models)+len(colnames(sprices))))
  try(chart.RiskReturnScatter(merge(mROC(modelEQ(models)),mROC(sprices)),  main = sprintf("Compare models"), ,colorset = Cols))
  
  #trainiere jährlich - jedes modell - jedes symbol
  if (F)
    roll_TrainSignals("years",FastTrain=5,dataSet=dataSet)
  #lade die trainings-ergebniss in den global_ParTable
  load(file=sprintf("Models/%s/lobal_ParTable22013-12-31.data",dataSet))
  #View(tail(global_ParTable,500))  
  
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=T,file="HuA") #rechne alle timing modelle  - evt. 
  models.t=clone(models) 
  purePlot(modelEQ(models),main="model equities - trained")
  
  #####################################################################################
   #MultiSig
  ######################################################################################
  
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuA_not_trained") #rechne aller modelle sind für das sym long ?  (1 bedeutet:  alle Modell sind für das sym long)         
  #laesst sich das toppen mit 
  #  multi.top.model.ranking ... nur die besten modelle der letzten jahre nehmen teil..
  
  load(file="HUA.data")
  load(file=sprintf("Models/%s/Hua_F.data",dataSet))
  load(file=sprintf("Models/HuaFeb_3/Results.xls/HuaEuroBra_F.data",dataSet))
  
  ranking=0
  m=models.Results(models)  
  
  ranking=multi.model.ranking(models,longOnly=T);ranking.name="multi.sig"  #wieviel pct   
  #MMX
  top.ranking= multi.top.model.ranking(models, n.top=12, k.top=16,longOnly=T)
  top.ranking= multi.top.model.ranking(models, n.top=2, k.top=2,longOnly=T,period="quarters")
  top.ranking= multi.top.model.ranking(models, n.top=5, k.top=7,longOnly=T,period="quarters",model2Quality.fn="model2Quality.calmar")
  
  models=list()
  #  View(ranking)
  #  dim(ranking)
  #  dim(data$prices)
  #  dim(data$weight)
  
  data$execution.price=data$prices
  data$execution.price[]=NA
  mPlots(ranking) #man kann auch lead-lags diskutieren
  
  safe.col= which(colnames(data$prices)==SAFE)  #Renten
  #View(ranking)
  #data$weight = ranking
  #rowSums(data$weight)
  #....... ranking in gewichte übeführen
  
  data$weight = ranking/ncol(ranking)
  data$weight[,safe.col] = 1-rowSums(data$weight[,-safe.col])
  #rowSums(data$weight)
  #MBRA
  #ein branchen-Asset-Model: aktien,renten
  models$branchen.model = bt.run(data, trade.summary=T)
  colnames( models$branchen.model$equity)="branchen.model.equity"
  
  
  #............ weniger viel in aktien
  data$weight = 0.6*ranking/ncol(ranking)
  data$weight[,safe.col] = 1-rowSums(data$weight[,-safe.col])
  #rowSums(data$weight)
  #MBRA
  #ein branchen-Asset-Model: aktien,renten
  models$branchen.model2 = bt.run(data, trade.summary=T)
  colnames( models$branchen.model2$equity)="branchen.model.equity2"
  #..............mit top.ranking.............................
  
  data$weight = top.ranking/ncol(top.ranking)
  data$weight[,safe.col] = 1-rowSums(data$weight[,-safe.col])
  models$top.branchen.model = bt.run(data, trade.summary=T)
  colnames( models$top.branchen.model$equity)="top.branchen.model.equity"
  
  #............ weniger viel in aktien
  data$weight = 0.6*top.ranking/ncol(top.ranking)
  data$weight[,safe.col] = 1-rowSums(data$weight[,-safe.col])
  models$top.branchen.model2 = bt.run(data, trade.summary=T)
  colnames( models$top.branchen.model2$equity)="top.branchen.model.equity2"
  #...............................................
  #alle stocks-gewichte zusammenziehen
  stock.weight = xts(rowSums(ranking[,-safe.col]) / (ncol(data$prices[,-safe.col])),index(data$prices))
  data$weight = data$prices;  data$weight[] = NA
  data$weight[,"SX5R"] = stock.weight; data$weight[,"BARCLAYS"]=1- data$weight[,"SX5R"]
  #ein 2 -Asset-Model: aktien,renten
  aa.model = bt.run(data, trade.summary=T)
  models$aa.model= aa.model
  colnames(models$aa.model$equity)="aa.model.equity"
  
  mPlots(merge(mNorm(data$prices[,"SX5R"]),data$weight[,c("SX5R","BARCLAYS")]),merge(models$branchen.model$equity,models$aa.model$equity),title="alloc")
  
  sprices=data$prices[,c(data$BENCH,SAFE)]
  Equity = list2xts(models,"equity");
  purePlot(mNorm(na.omit(merge(sprices,Equity)[-c(1:200),])),main="compare")
  
  
  #bench
  data$weight = data$prices;  data$weight[] = NA
  data$weight[,"SX5R"] = 0.5; data$weight[,"BARCLAYS"]=0.5
  models$mod5.5 = bt.run(data, trade.summary=T)
  colnames(models$mod5.5$equity) = "mod5.5"
  
  #bench
  data$weight = data$prices;  data$weight[] = NA
  data$weight[,"SX5R"] = 1; 
  models$sx5r = bt.run(data, trade.summary=T)
  colnames(models$sx5r$equity) = "sx5r"
  
  #bench
  data$weight = data$prices;  data$weight[] = NA
  data$weight[,"BARCLAYS"]=1
  models$BARCLAYS = bt.run(data, trade.summary=T)
  colnames(models$BARCLAYS$equity) = "BARCLAYS"
  
  performance.map(data,models)
  #################################
  scatter.map(data,models)
  
  #..................................................................
  #######  
  
  
  global_arg$R_arg =T 
  out = plotbt.strategy.sidebyside(aa.model, return.table=T,make.plot=T)
  try(strategy.Performance.snapshoot(models.2, R.arg=NULL, title="NOTiming.NOSelection.just Allocation", data=data))
  
  turnover.map(models.2)
  alloc.map(models.2)
  
  tryM(plotbt.custom.report.part2(models.2)  ) 
  
  # strategy.performance.snapshoot(models, T)
  
  repName = "AA"
  pdf.f <- sprintf("Models/%s/%s.pdf", dataSet, repName)
  mP("Write Data Charts to %s",pdf.f)
  #  browser()
  pdf(file = pdf.f, width=8.5, height=11)  #...............................................................
  
  #DataInfo<<-NULL
  
  print(ls(models))
  if( has(R.arg,"pdf.report","compare"))   
    k=compareViewModels(models.2,data$prices,alloc=T)
  if( has(R.arg,"pdf.report","periods"))  
    custom.period.chart(models.2) 
  mdev.off()   #............................................................................................
  #mdev.off()
  
  if( has(R.arg,"pdf.report","xls"))
    writeModelDataXls(models,xlsName=sprintf("Models/%s/Results.xls/%s.xls",dataSet))
} 

#............. lies AAData.xls ein

universe_hua<-function(reload=T)
{
  file=file=sprintf("hua_europe.data")
  if (!reload)
  {
    load(file=file)
    print(ls(data))
    data<<-data
    define.Globals()
    
    return(data)
  }
  #hol Dir noch zusätzliche Daten aus einem bloomberg betankten xls-file 
  hua=read.HUA.XLS( modelDir="HuAFeb_3", xlsName="AAData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=F,dec=",",date.format="%d.%m.%Y",selectedCols=c(1:4))
  
  fromToS(na.omit(hua))
  data=new.env()
  data$prices=hua[fromToS(na.omit(hua)),(1:4)]
  colnames(data$prices) = data$symbolnames=spl("MSCIEUROPE,MSCIWORLD,EUROSTOXX50,BARCLAYS")
  #data.info(data)
  data$symbolnames
  #data$euro.macros=euro.macros
  data$BENCH="MSCIEUROPE"
  SAFE <<- "BARCLAYS"
  prices=data$prices
  mPlots(prices)
  prices2data(data)
  #loud.check(euro.indi.n)
  p=global_arg$clos = data$prices
  data$weight=data$prices;data$weight[]=NA
  #data.info(data)
  data$universe="hua_europe"
  data$SAFE=SAFE
  data$symbolnames
  define.Globals()
  data$dates= as.Date(index(data$prices))
  mP("###########  save file %s ###########",file)
  save(data,file=file)
  print(ls(data))
  data<<-data
  data     
}
if (F)
{
  universe_hua(reload=T)
  universe_hua(reload=F)
}

##################################################################################
universe_hua_World<-function(reload=T)
{
  file=file=sprintf("hua_World.data")
  if (!reload)
  {
    load(file=file)
    print(ls(data))
    data<<-data
    define.Globals()
    
    return(data)
  }
  #hol Dir noch zusätzliche Daten aus einem bloomberg betankten xls-file 
  hua=read.HUA.XLS( modelDir="HuAFeb_4", xlsName="AWData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=F,dec=",",date.format="%d.%m.%Y",selectedCols=c(1:15))
 # fromToS(na.omit(hua))
  data=new.env()
  data$prices=hua[fromToS(na.omit(hua)),]
  colnames(data$prices) = data$symbolnames=spl("SUP500,MSCIEUROPE,MSCIEMERGINGM,TOPIXMSCIJAPAN,PHYSICALGOLD,IGBONDS,HYBONDS,TIPS,REXP,EUROTREASURIES,EMTREASURIES,DBCURRENCIES,MSCIWORLDGDB,MSCIEUROPEGDB,BARCLAYS")				

  #data.info(data)
  data$symbolnames
  #data$euro.macros=euro.macros
  data$BENCH="MSCIEUROPE"
  data$safe= SAFE <<- "BARCLAYS"
  data$universe="hua_World"
 
  prices=data$prices
 
  mPlots(prices)
  prices2data(data)
  #loud.check(euro.indi.n)
  p=global_arg$clos = data$prices
  data$weight=data$prices;data$weight[]=NA
  #data.info(data)
  data$symbolnames
  define.Globals()
  data$dates= as.Date(index(data$prices))
 print(fromToS(prices))
 mP("###########  save file %s ###########",file)
  save(data,file=file)
  print(ls(data))
  data<<-data
  data     
}
if (F)
{
  universe_hua_World(reload=T)
  universe_hua_World(reload=F)
}


#############################
#lade die stoxx-branchen 
#############################
universe_stoxx_branchen <- function(mode="pure")
{  
  SAFE <<- "BARCLAYS"
  dataSet <<- "HuA_Jan1"
  init_TSA(universe = "Meurope", dataSet =dataSet, TargetMethod=  0 , online =F, visual=F)
  
  if (mode=="pure")
    data.rm(data,spl("DAX,AEX_GENERAL,ATX,CAC40,FTSE_100,SWISS_MARKET,USDCHF,USDEUR,USDGBP"))
  data$prices=prices=data.info(data)
  #hol Dir noch zusätzliche Daten aus einem bloomberg betankten xls-file 
  hua=read.HUA.XLS( modelDir="HuA_Jan1", xlsName="AAData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=F,dec=",",date.format="%d.%m.%Y")
  View(hua)
  #data.info(data)
  euro.indi.n = merge(data$prices,hua)
  euro.indi.n= m.ifna.prev(euro.indi.n)
  mPlots(euro.indi.n)
  euro.indi.n = mNorm(na.omit(euro.indi.n))["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  fromToS(euro.indi.n)
  #füge Barclays als Rentenndex hinzu
  data.add(data,"BARCLAYS",euro.indi.n[,"lbeatreu.index"])
  data$symbolnames
  data$prices = na.omit(data$prices)  
  #data$euro.macros=euro.macros
  data$BENCH="SX5R"
  data$SAFE = SAFE <<- "BARCLAYS"
  prices=data$prices
  mPlots(prices)
  #loud.check(euro.indi.n)
  p=global_arg$clos = data$prices
  #data.info(data)
  ls(data)
  data$symbolnames
  data$universe= "stoxxBra"
  # a  data.add()
  ls(data)
  save(data,file=sprintf("stoxx_branchen%s",mode))
}
#############################################################
if (F)
{
  ############  TSA  Aufruf ####################################################
  # laden/update eines Universum 
  ##############################################################################
  #init_TSA(universe = "MWorld3", dataSet ="MM_Feb1", TargetMethod=  0 , online =F, visual=T)
  #data.rm(data,spl("GOLD,JAKARTA_COMPOSITE,OIL"))
  #Universe-Load
  
  #.........................................................................
{
  #rm(list= ls());     source  ("MLib/TSA.r")
  init_TSA(universe = "MWorld3", dataSet ="MM_Feb2", TargetMethod=  0 , online =F, visual=T)
  data.rm(data,c( "TREASURY" ))
  #data.rm(data,c("X5YEARNOTE", "USDEUR",     "STOXX50E",   "NIKKEI225","TREASURY" ))
  hua=read.HUA.XLS( modelDir="HuA_Jan1", xlsName="AAData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=F,dec=",",date.format="%d.%m.%Y")
  
  data$prices= data.info(data)
  euro.indi.n = merge(data$prices,hua)
  euro.indi.n= m.ifna.prev(euro.indi.n)
  mPlots(euro.indi.n)
  euro.indi.n = mNorm(na.omit(euro.indi.n))["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  fromToS(euro.indi.n)
  #füge Barclays als Rentenndex hinzu
  data.add(data,"BARCLAYS",euro.indi.n[,"lbeatreu.index"])
  data$symbolnames
  data$prices = na.omit(data$prices)  
  #data$euro.macros=euro.macros
  data$BENCH="SX5R"
  SAFE <<- "BARCLAYS"
  prices=data$prices
  mPlots(prices)
  #loud.check(euro.indi.n)
  p=global_arg$clos = data$prices
  #data.info(data)
  ls(data)
  data$symbolnames
  # a  data.add()
  ls(data)
  save(data,file=sprintf("MWorld3"))
  
  #p=global_arg$clos = data$prices=data.info(data)
}
#.........................................................................
{
  #http://systematicinvestor.wordpress.com/2013/10/24/update-for-backtesting-asset-allocation-portfolios-post/
  
  tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
  
  data <- new.env()
  getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
  for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                           
  bt.prep(data, align='remove.na', dates='1990::') 
  
  data$BENCH="SPY"
  global_arg$dat <-data
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_arg<<-list(clos=data$prices,dat=data)
  global_commission <<- 0.000015   #sys.Run() nimmt diese commission !!!!
  
  global_StartDate <<-  DateS(last(data$prices))
  #globalTrainLevel <<- 3   
  global_FastTrain <<-   -10  #keine Einschränkung
  
  
}
#.........................................................................
#.........................................................................
{
  check.cluster.history()
  
  p=prices=data.info(data)
  p=prices=data$macros
  
  mPlots(p)
  data$leads = find.all.leads.and.reduce.highCorr(prices,visual=F)
  res$coint=tryM(prices.coint(p,features=data$leads))#~~
  
  p.betas=  beta.again.portfolio(data$prices)-1  #vieleicht ein gutes ranking-kriterium ?
  itp=in.Trend.pos(p[,4],visual=T,main="itp",k=160,K=3,probs=c(0.15,  0.5,  0.80))
} 

}

############################ Untrainierte Timing-Systeme ####################################### #RUN
#21 Signals:



rank.ROC_maxDD<-function(prices)
{
  
}



modelEQ<-function(models)
{
  if (len(models$Signal)>0)
    models1 = list(mod1=models)
  else
    models1=models
  res= foreach(mod=models1,.combine="merge") %do%
{
  print("####")
  xtsList = mod$equity
  
  xtsList
}
colnames(res)=names(models)
res

}
if (F)
  purePlot(modelEQ(models),main="model equities")

model2equities<-function(models,longOnly) #TODO
{
  if (len(models$Signal)>0)
    models1 = list(mod1=models)
  else
    models1=models
  res= foreach(mod=models1,.combine="merge") %do%
{
  print("####")
  xtsList = mod$singleResults
  res1= foreach(x = xtsList,.combine="merge") %do%
{ 
  x$equity
}
res1
}
}

if (F)
{
  equities = model2Quality(models)
  class(equities)
  dim(equities)
}
####################################################################################


#####################################################################

if (F)
{
  ranking=multi.model.ranking(models)
  
  purePlot(ranking)
  rankingTop= multi.top.model.ranking(models, n.top=3, k.top=4,longOnly=T)
  rankingTop= multi.top.model.ranking(models, n.top=10, k.top=10,longOnly=T)
  
  k=55
  rowSums(ranking -rankingTop)
  rs[rs!=0]
  (ranking-rankingTop)[k]
  ranking[k]
  rankingTop[k]  
  purePlot(rankingTop)
  fromToS(rankingTop)
  fromToS(ranking)
  
}





if (F) 
{
  #ranking in den cashe schreiben 
  colnames(ranking)<-colnames(data$prices)  #daten für cashe vorbereiten
  x=get.rank( ranking.name ,prices=ranking,xtra="write")#schreibt
  cashed()
  cashed(ranking.name)
  cashed(ranking.name,"DAX")
  
  x=indi.Generic("signal.drawDown1", global_arg, par = list(win=150), TRAINSYM =-1,
                 T.arg=list(cashed="TSA.ss1")  ,#cashe die Timing-signale 
                 S.arg=list(ranking.fn=ranking.name,nTop=15,kTop=20), #das neue ranking  ,
                 R.arg=list(tsa.level="TSA",
                            report=spl("signals,one,perfBar,transMap,turnover"),
                            pdf.report.models=spl("TSA,SA"),
                            pdf.report=spl("NO,compareModels") ),  
                 experiment = "newRank")
  #Fazit:equity.ranking() Bring nichs !! :-()
  
} 
#############################################################
if (F)
{
  source("Mlib/MM_Main.R")
  (data$symbolnames)
  #in Arbeit ...
  channelStop(sym="DAX",maxdd=10)
  channelStop(sym="SG2R",maxdd=10)  
  channelStop(sym="USDEUR",maxdd=3)  
  
  #aktueller trendkanal
  p=data$prices[,data$BENCH]
  last.trend=find.last.trend.segment(p,visual=T,glaettung=5)
  x.lt=last.trend$x2
  Y.lt=na.omit(p[sprintf("%s::%s",x.lt,DateS(last(p)))])
  if (shape(Y.lt) <100)
    Y.lt= last(na.omit(p),100)
  #LongTerm-Trendkanal  
  slope.lt=lm.FITm(Y.lt,visual=T) #lin modell
  itp=in.Trend.pos(p,visual=T,main="itp",k=60,K=2,probs=c(0.20,  .5,  0.80))
  mPlots(merge(itp,0),p) 
  
  #sehr langsam ... 
  x=indi.Generic("signal.glaettung", global_arg, par=list(glaettung=2,visual=T, TRAINSYM ="DAX"))
  
  
  #.................. zusammenfassung
  #long-only  - die Timing Modelle im Mittel
{
    norm_Win(2)
    b=merge(x$equity, x1$equity,x1.1$equity,x2.1$equity,x3.1$equity,x4.1$equity,x5.1$equity)
    mean.b=xts(rowSums(b)/ncol(b) , index(b)) 
    purePlot(mNorm2(merge(data$prices[,"DAX"],b,mean.b)["2004::2014"]),main="GuV")
    purePlot(mNorm2(merge(data$prices[,"DAX"],mean.b)["2004::2014"]),main="GuV-mean")
    
    norm_Win(2)
    b=merge(x1.1$equity,x5.1$equity)
    mean.b=xts(rowSums(b)/ncol(b) , index(b)) 
    purePlot(mNorm2(merge(data$prices[,"DAX"],b,mean.b)["2004::2014"]),main="GuV")
    purePlot(mNorm2(merge(data$prices[,"DAX"],mean.b)["2004::2014"]),main="GuV-mean")
    
    
  } 
}
###############################################################################################################
##############  Trainiere mehrere Timing-Systeme #############################################################

roll_TrainSignals<-function(mode=c("years","quarters","months","days"),FastTrain=3,dataSet="test13")
{
  #TRAIN #starte das training
  
  ############################ Untrainierte Timing-Systeme ########################################
  
  ############################################################################
  # Trainings-Phase des Timing-Systems  
  ############  TSA  Aufruf ##################################################
  
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  
  colnames(data$prices)
  #  sym="BUND_FUTURE"
  
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.0005   #sys.Run() nimmt diese commission !!!!
  
  global_StartDate <<-  DateS(last(data$prices))
  #globalTrainLevel <<- 3   
  global_FastTrain <<-   FastTrain #-3  #keine Einschränkung
  #global_objectId <<-paste("TREND",sym,"signal.Faber.base") #"signal.lm" 
  
  ## jählich trainieren
  #today.list = select.dates(p,"years") 
  today.list = select.dates(p,mode) 
  cashed()
  #reset.cashed.rank
  #x3=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=F, TRAINSYM =-1)
  #x3$Tquality
  
  signal.systems=spl("signal.1.smoothing,signal.lm,signal.Price.itp,signal.Faber.dyn.hysterese,signal.drawDown1,signal.MA.3,signal.Faber.base,signal.mom")
  
  lapply(signal.systems,  function(signal.sys) 
 
    TrainIndicator(global_StartDate_=global_StartDate,  opti="GRID", indiName =     signal.sys, visual=F, TRAINSYM = -1, roll.mode="years", train.WinLen=600))
  
  save(global_ParTable,file=sprintf("Models/%s/lobal_ParTable2%s.data",dataSet,as.character(today.i)))
  
  #.<<<<......................................................................................
  
  #training sichern
  View(global_ParTable)
  writeTable(table=global_ParTable,xlsName=sprintf("Models/%s/global_ParTable1000.xls",dataSet),rownames=T)
  save(global_ParTable,file=sprintf("Models/%s/lobal_ParTable2.data",dataSet))
  #  load(file=sprintf("Models/%s/lobal_ParTable.data",dataSet))
}
#................
if (F)
{
  roll_TrainSignals("years",FastTrain=5,dataSet=dataSet)
  
  load(file=sprintf("Models/%s/lobal_ParTable22013-12-31.data",dataSet))
  View(tail(global_ParTable,500))  
  
}

######################################################################################################################

if (F)
{
  #baue für jedes symbol ein guv - aber nimm die Parameter aus global_ParTable und assembliere die zeitabschnitte
  ##AUFRUF DER TSA
  ############################################################################
  # Arbeitsphase
  
  #load.universe(universe="Meurope",visual=T,online=F) # Meurope
  #sub.portfolio=spl("DAX,X5YEARNOTE")
  
  cashed("TSA.t1")
  reset.cashed.rank("TSA.t1")
  load(file=sprintf("Models/%s/lobal_ParTable.data",dataSet))
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM = -1,TSA.argSet=TSA.light)
  ls(x)
  x$Tquality
  x$trade.summary
  mchart(mNorm(merge(mNorm(data$prices[,"DAX"]),x$equity)["2003::"]),main="DAX")
  mchart(x$Signal)
  ls(x$singleResults[[1]])
  ##>
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1, TSA.argSet=TSA.light,
                 experiment="stoxx_faber_slope",
                 S.arg=list(ranking.fn="rank.slope",nTop.q=5,kTop.q=4))
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,T.arg=list(cashed="TSA.t2")  )#, TSA.argSet=TSA.light)#,experiment="test")
  
  
  load(file=sprintf("Models/%s/lobal_ParTable2.data",dataSet))
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,do.assemble.Signals=T, experiment="rollingFaberBaseRSlope300",
                 T.arg=list(cashed="TSA.t1")  ,#cashe die Timing-signale 
                 S.arg=list(ranking.fn="rank.slope300",nTop.q=4,kTop.q=3),
                 A.arg=list(min.risk.fns=SET2),
                 R.arg=list(tsa.level="TSA,SA,A",
                            report=spl("signals,one,perfBar,transMap,turnover"),
                            pdf.report.models=spl("all,TSA,SA,A"),
                            pdf.report=spl("compareModels,periods,xls") )  )
  
  #unterschied in der guv - wenn ich signale aus cash lese
  #dev.off() -bug . sobald ich TSA.light nehm..)
  #............................
  ##  assembliere die trainierten paramter-systeme
  reset.cashed.rank("TSA.t2")
  load(file=sprintf("Models/%s/lobal_ParTable2.data",dataSet))
  global_arg$clos = data$prices
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,do.assemble.Signals=T,
                 #T.arg=list(cashed="TSA.t2")  ,#cashe die Timing-signale 
                 S.arg=list(ranking.fn="rank.slope300",nTop.q=4,kTop.q=3),
                 A.arg=list()  )
  
  #............................
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,do.assemble.Signals=T,experiment="signal.faber.base",
                 T.arg=list(cashed="TSA.t1")  ,#cashe die Timing-signale 
                 S.arg=list(ranking.fn="rank.slope300",nTop.q=4,kTop.q=3),
                 A.arg=list()  )
  
  
  #fit <- HoltWinters(P)#,gamma=FALSE)  #nicht schlecht
  #plot(forecast(fit, h=5))
  #fit$coefficients[2]
  
  #########################
  b=prices
  
  timing = foreach(col =  colnames(b),.combine="merge") %do%
{
  print(col)
  p=b[,col]
  x1=  rollRegressionXTS.smooth.dyn(p,300,probs=0.5)
  px1=p[x1$m>0]
  mchart(px1)
  mchart(merge(p,px1))
  ls(x1)
  x1$m
  b=merge(x1$sig,p)
  plotSigPrice(signal=b[,1],b[,2],indi=list(na.omit(x1$USDGBP.dyn.smoothed300)))
}

dim(timing)
signal = bt.apply.matrix(timing, function(sym) iif(sym >0,sym,0))
tail(timing)
#.......................................
}

#############################################################################################


#######################################################################################
mP("########### load mm2014_1.R")
if (F)
  list_R_functions('MLib/mm2014_1.R')
