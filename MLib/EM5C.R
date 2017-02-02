################################################################################################################
# 2014-03-07e
#Lade Eckhards e4  xls daten  nach xts pro sheet 
#V-1.2
#-------------------- ergebnsse noch schlecht
#Marke: #TEST   #MIFO   #MPREP  #MMTrainRun   #MMauswertung 

#besser:  teste  mit run_all_signal() alle signale
# für die erfolgreichen modelle - hol dir das ranking via rank.Generic() in die 
#small_cloud()    

#der IFO hat einen time lag von 20 Tagen, ZEW ca. 18 Tage, PMI 25-30 Tage

#die US Daten haben einen Time lag von 12 Tagen (NY Index), 19 Tagen (Phili Fed), 25 Tagen (PMI)

#die EU Daten haben eine Time Lag ca 28 Tagen.

#Japan ca. 28 Tage.

#Die Märkte reagieren bereits mit den ersten US-Daten für den Monat (also NY Index, Phili FED),
#bzw. in Europa mit dem ZEW, um die Konjunktur für den aktuellen Monat abzuschätzen.
#(und korrigieren ggfs. diese Einschätzung am Anfang des folgenden Monats, falls sie sich "verschätzt" haben).

#wir bauen generell  ein fundamental -lag vo n 20 Tage (diese Ungenauigkeit müssen wir in Kauf nehmen).

# wichtigster Aufruf:

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

if (F)
  runAll()
# load diamonds dataset from ggplot2
#require(ggplot2)
#data(diamonds)

# create tableplot
#tableplot(diamonds)
##############################################################

if (F)
{
  prepare.all.xls  
  
  ######  TRAIN
  def.Globals()
  for (xlsfile in all.xls.files)    #.............................
  {
    # xlsfile="m601.xls"
    try(  run.main( bench="EXX50_RI", xlsName=xlsfile, modelDir=modelDir) )
  }
  ###### Auswertung
  def.Globals()
  for (xlsfile in all.xls.files)    #.............................
  {
    #  xlsfile="602.xls"
    Auswertung( bench="EXX50_RI", xlsName=xlsfile)  
    #models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuAEurope4",LongShort="F",pdf=T,visual=T)  
  }
  
}
##################################################
#TODO
#super wichtig:  beachte dass Du das Target nicht bis heute hast - sondern nur bis zum letzten zigzag-erkenunngs-Datum .. du kannst also nicht bis ultimativ gestern trainieren

#   optimierer  parallel mit :   http://cran.r-project.org/web/packages/snowFT/snowFT.pdf
#Changes:
#micro.cloud und small.cloud um hot.leads zu global_arg$data$prices[,bench] erweitert
#micro.cloud um log slope200,  und log(slope(sma200))  erweitert
if (F)  
  #.... teste alternativen zu  random.forest
{
  def.Globals()
  reset.cashed.rank()
  
  load(file=sprintf("Models/%s/EM5C.data",modelDir))
  
  
  for (xlsfile in all.xls.files)  {  
    #  xlsfile="600.xls"
    
    prepare.EM5C.data(file=sprintf("%s_EM5C",xlsfile), xlsName=xlsfile,bench=bench,fundis=NULL,do.load=do.load,do.par=do.par, mkTrain=T,visual=F, modelDir=modelDir)#,techbuilder.symbols=c("Y"))
  }    
  
  p=prices=data$macros
  mPlots(p)
  data$leads = find.all.leads.and.reduce.highCorr(prices,visual=F)
  
  
}
#####################################################################################
#lies die ganzen xls-files ein-baue cloud und target und mische den barclays hinzu
#speicher dann unter EM5C.data
######################################################################################
make.EM5C.data<-function()
{
  data = read.many.xls.all(modelDir=modelDir,load.data=T)
  
  hua=read.HUA.XLS( modelDir="HuAFeb_4", xlsName="AWData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=F,dec=",",date.format="%d.%m.%Y",selectedCols=c(1:15))
  colnames(hua) =spl("SUP500,MSCIEUROPE,MSCIEMERGINGM,TOPIXMSCIJAPAN,PHYSICALGOLD,IGBONDS,HYBONDS,TIPS,REXP,EUROTREASURIES,EMTREASURIES,DBCURRENCIES,MSCIWORLDGDB,MSCIEUROPEGDB,BARCLAYS")      	
  #data.info(data)
  data$symbolnames
  #data$euro.macros=euro.macros
  data.add(data,"BARCLAYS",hua[,"BARCLAYS"])
  data$safe= SAFE <<- "BARCLAYS"
  data$BENCH="EXX50_RI"
  ls(data)
  data$symbolnames = c(data$Y.names,"BARCLAYS",data$BENCH)
  #die sind leider zu kurz
  len(data$symbolnames)
  data$symbolnames = data$symbolnames[!data$symbolnames %in% spl("ESTX_FINANCIAL_SERV_,ESTX_INDUSTRIAL_TRANSPORT")] 
  global_arg$dat=data
  global_arg$clos=na.omit(data$prices[,data$symbolnames])
  print(colnames(global_arg$clos))
  print(fromToS(global_arg$clos))
  data$prices=global_arg$clos
  
  #lapply(data$symbolnames,function(coln) sprintf("%s-%s",coln,fromToS(data$prices[,coln])))
  
  save(data,file=sprintf("Models/%s/EM5C.data",modelDir))
  
}

#VHF(ttrc[,"Close"])
#runPercentRank(ROC(p,3), n = 260, cumulative = FALSE,               exact.multiplier = 0.5)
if (F)
{
  p=data$prices[,data$BENCH];P=mNorm(p)
  P=na.omit(SMA(na.omit(ZigZag(p[1000:1500])),5))   #zigzag-reihe - gut für causalitäts test
  
}
#mPlots(merge(p,VHF(p)))
#mPlots(merge(p,runPercentRank(p-EMA(p,260), n = 260, cumulative = FALSE, exact.multiplier = 0.5)))
###########################################################################################
#für alle xls:  lesen,cloud +target rechne
#MPREP
prepare.all.xls<-function()
{
  def.Globals()
  global_disable_cloud.check <<- T
  for (xlsfile in all.xls.files)  {  #MMprepare
    #data <-make_eckhard4C_data(visual=visual,xlsName=xlsName,bench=bench,do.par=do.par,mkTrain=F,fundis=NULL,modelDir=modelDir)
    #in
    #    xlsfile="613.xls"
    now.info=data.frame(now=xlsfile)
    View(now.info)
    global_ignore_schon_da <<- T    
    prepare.EM5C.data(file=sprintf("%s_EM5C",xlsfile), xlsName=xlsfile,bench=bench,fundis=NULL,do.load=F,do.par=do.par, mkTrain=T,visual=F, modelDir=modelDir)#,techbuilder.symbols=c("Y"))
  }
}
if (F)
  prepare.all.xls()

#....................................... Experimente definieren ..........................
runAll<-function()
{ 
  def.Globals()
  global_disable_cloud.check <<- T
  #  run.main(     bench="EXX50_RI", xlsName="Data0E.xls",fundis=c(2:25),modelDir=modelDir)  
  
  #..................................................................
  
  if (F)
  {
    for (xlsfile in all.xls.files)   #lassen sich die xls-files einlesen ?
      #xlsfile="600.xls"
      read.EckhardsXLS(modelDir=modelDir, xlsName=xlsfile,sheet.i=1,startRow=10,belowColnames=5, date.format = "%d.%m.%Y",to.month.end=F,debug=F)
    
    #  for( fi in c(0:8)) {    #fi=0
    #    xlsfile=sprintf("60%d.xls",fi)
    ##############################################>>
    reset.cashed.rank()
    
    for (xlsfile in all.xls.files)  {  #MMprepare
      xlsfile="600.xls"
      
      #data <-make_eckhard4C_data(visual=visual,xlsName=xlsName,bench=bench,do.par=do.par,mkTrain=F,fundis=NULL,modelDir=modelDir)
      #in
      prepare.EM5C.data(file=sprintf("%s_EM5C",xlsfile), xlsName=xlsfile,bench=bench,fundis=NULL,do.load=do.load,do.par=do.par, mkTrain=T,visual=F, modelDir=modelDir)#,techbuilder.symbols=c("Y"))
    }    
    #ausserdem macht prepare noch:
    #data$cloud.mac <- cloud.mac <<- small.cloud(data$macros)
    #data$test.cloud  <-test.cloud <<- micro.cloud(data$prices,visual=F,cloud.control= list()) 
    #if (len(techbuilder.symbols) > 0) tech.cloud.builder(techbuilder.symbols,tech.Cloud.Fn= "tech.Cloud", data,visual=F,cloud.control=spl("heavy,lagged"),do.par=do.par)
    #if (mkTrain)  data$multiTarget <-  multiTarget(visual=visual) 
    
    
    #xlsfile="600.xls"
    # prepare.EM5C.data(file=sprintf("%s_EM5C.data",xlsfile), xlsName=xlsfile,bench=bench,fundis=NULL,do.load=T,do.par=do.par, mkTrain=T,visual=F, modelDir=modelDir)#,techbuilder.symbols=c("Y"))
    ##################################>> #MMTrainRun   
    def.Globals()
    for (xlsfile in all.xls.files)    #.............................
    {
      # xlsfile="m601.xls"
      try(  run.main( bench="EXX50_RI", xlsName=xlsfile, modelDir=modelDir) )
    }
    ##################################>> #MMauswertung 
    for (xlsfile in all.xls.files)    #.............................
    {
      #  xlsfile="602.xls"
      Auswertung( bench="EXX50_RI", xlsName=xlsfile)  
      #models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuAEurope4",LongShort="F",pdf=T,visual=T)  
    }
    
  }
}
if (F)
  runAll()

## lade aus sämtlichen xls-files nur das 1. symbol
read.many.xls.y<-function(modelDir=modelDir,universe="all.xls",bench="DAX30", safe="DAX30")
{
dataSet <<- modelDir
  all.xls.files =get_xlsFiles(modelDir)
  
  all.prices=NULL
  for (xlsfile in all.xls.files)   #lassen sich die xls-files einlesen ?
  {
    w=read.EckhardsXLS(modelDir=modelDir, xlsName=xlsfile,sheet.i=1,startRow=10,belowColnames=5, date.format = "%d.%m.%Y",to.month.end=F,debug=F,visual=F)
    
    w.cols=c(1) #welche xls spalten interessieren mich
    
    bench.i=which(colnames(w)==bench)  #auch die bench
    if (len(bench.i)>0) w.cols=c(w.cols,bench.i)
    safe.i=which(colnames(w)==safe)  #auch der safe
    if (len(safe.i)>0) w.cols=c(w.cols,safe.i)
 
    w=w[,w.cols]
    for(coln in colnames(w))
      if (!coln %in% colnames(all.prices))
        if (is.null(all.prices))
          all.prices = w[,coln]
    else
      {
         mP("merge %s %s ~~~~~~~~~~~~~",coln,fromToS(w[,coln]))
         all.prices = merge(all.prices,w[,coln])
      }
    
  }
  colnames(all.prices)
  all.prices=na.omit(all.prices)
  print(colnames(all.prices))
mp("all.prices %s ########################## ",fromToS(all.prices))
  data<<- prices2data.env(all.prices)
  #  data=prices2data.env(all.prices,do.load=T)
  
  #data$BENCH = bench
  #data$SAFE = safe

  define.Globals(dataSet=modelDir,bench=bench,SAFE=safe)
  data
}
if (F)
{
  mdata=read.many.xls.y(modelDir=modelDir,universe = "EM5C_Y",bench="STOXX_50",safe="STOXX_50")
}
#..................................................................
read.many.xls.all<-function(modelDir=modelDir, load.data=T)
{
  FUNDIS=NULL
  all.data=NULL
  all.xls.files =get_xlsFiles(modelDir)
  for (xlsfile in all.xls.files)  {  
    
    file = "EM5C"
    file.res = sprintf("Models/%s/%s_%s.data",modelDir,xlsfile,file)
    
    if (load.data && file.exists(file.res))
    {
      load(file.res)
      temp.data=data            
    }
    else
      temp.data <-make_eckhard4C_data(visual=visual,xlsName=xlsfile,bench="",do.par=F,mkTrain=F,fundis=NULL,modelDir=modelDir)
    
    if (is.null(all.data))
    {
      all.data=clone(temp.data)
      all.sym = temp.data$Y.names 
    }
    else
    {
      all.sym = c(all.sym,temp.data$Y.names)
      #die preise (und intermarket-faktoren hinzufügen)
      for(coln in temp.data$symbolnames)
        if (!coln %in% all.data$symbolnames)
          data.add(all.data,symNam=coln,symbol.xts=temp.data[[coln]])
      #die macro-daten hinzufügen
      for (coln in names(temp.data$macros))
        if (!coln %in% names(all.data$macros))
          if (len(all.data$macros)==0)
            all.data$macros =temp.data$macros[,coln]
      else
      {
        mP("data$macros %s %s %s",xlsfile,coln,fromToS(temp.data$macros[,coln]))
        all.data$macros = merge(all.data$macros ,temp.data$macros[,coln])
      }
      
    }  
    
  }
  
  all.data$intermarket.faktoren= all.data$symbolnames[!all.data$symbolnames %in% all.sym ] 
  all.data$Y.names= all.sym
  all.data$fundamentale.faktoren = names(all.data$macros)
  save(all.data,file="read_many_xls_all.data")
  all.data
  
}
if (F)
{
  def.Globals()
  data = read.many.xls.all(modelDir=modelDir,load.data=T)
  SAFE=data$SAFE=data$BENCH="DAX30"
  data$prices = na.omit(data$prices)
  fromToS(data$prices)
  mPlots(data$prices)
  
  load(file="read_many_xls_all.data")
  -----------
    modelDir="EM4_MAERZ_B"
  data = read.many.xls.all(modelDir=modelDir,load.data=T)
  
  
}
#..........................................................................................
batchRun <-function()
{
  def.Globals()
  
  #read.many.xls.y(modelDir=modelDir)
  #TEST
  #baue aus den Y.names (left hand prices )  ein data objekt und teste 
  #hier mal die gewöhnlichne signal- methoden
  
  #reset.cashed.rank()
  
  
  #indi.Results(x)
  
  models= list()
  trained="_"
  pdfFile=""
  #  SAFE=data$SAFE=data$BENCH=data$symbolnames[[1]]
  do.assemble.Signals=F
  visual=T
  global_arg$modell.par$LongShort <- "F"
  
  
  global_arg$clos = data$prices["2009::"]
  
  set.glob(data)
  
  signal.Faber.base=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,safe=SAFE,do.assemble.Signals=F)
  
  x=indi.Generic("signal.sit.feilscher", global_arg, par=list(lookback.len=100), visual=T, TRAINSYM =-1,safe=SAFE,do.assemble.Signals=F,pdfFile="")#,period="weeks")
  
  # T.arg=list(cashed="TSA.t1"),
  x=indi.Generic("signal.levy", global_arg, par=list(win=-60), visual=T, TRAINSYM =-1)#
  eq=mNorm(merge(x$equity,global_arg$dat$prices[,global_arg$dat$BENCH])["2010::"])
  print(fromToS(eq))
  print(fromToS(x$equity))
  data.info(data)
  mchart(eq)
  
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="emc4-test",LongShort="F",pdf=T,visual=T) 
  
  models$signal.lm= x2.1=indi.Generic("signal.lm", global_arg, par=list(win=60),visual=T, TRAINSYM =-1,safe=SAFE)
  
  #x=indi.Generic("signal.sit.feilscher", global_arg, par=list(lookback.len=100), visual=F, TRAINSYM =-1,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile)#,period="weeks")
  
  
  
  #.....................................................................................
  
  init_TSA(universe = "DaxMdax", dataSet ="MM_Jan14", TargetMethod= 0,  online =T, visual=T)
  init_TSA(universe = "MWorld3", dataSet ="MM_Jan14", TargetMethod=  0 , online =T, visual=T)
  init_TSA(universe = "StoxxComp", dataSet ="MM_Jan14", TargetMethod=  0 , online =T, visual=T)
  return
  def.Globals()
  
  try(run.main(     bench="EXX50_RI", xlsName="Data0E.xls",fundis=c(2:25),modelDir=modelDir)  )
  try(run.main(     bench="MWorld3", xlsName="Data0E.xls",fundis=c(2:25),modelDir=modelDir)  )
  
  #try(  run.main(     bench="DAIMLER", xlsName="Data0DAIMLER.xls", fundis=c(2:11), modelDir=modelDir))
  
  try(  run.main(     bench="VW", xlsName="Data0VW.xls", fundis=c(2:11), modelDir=modelDir))
  
  #try(run.main(     bench="CARS_U_PARTS", xlsName="Data0CARS_PARTS.xls", fundis=c(2:11), modelDir=modelDir))
  
  try(run.main(     bench="GOLD_AM_FIX_TROY_OZ",  xlsName="Data0Gold.xls", fundis=c(2:11),modelDir=modelDir))
  
  try(run.main(     bench="SILVER_FIX_TROY_OZ", xlsName="Data0Silv.xls", fundis=c(2:12), modelDir=modelDir))
  
  try(run.main(     bench="BMW_ST", xlsName="Data0BMW.xls", fundis=c(2:11), modelDir=modelDir))
  
}
if (F)
  batchRun()
#..........................................................................................
batchRun2 <-function()
{
  def.Globals()
  try(run.main(     bench="CARS_U_PARTS", xlsName="Data0CARS_PARTS.xls", fundis=c(2:11), modelDir=modelDir))
  
  try(  run.main(     bench="DAIMLER", xlsName="Data0DAIMLER.xls", fundis=c(2:11), modelDir=modelDir))
  
}

get_xlsFiles<-function(modelDir )
{
  xlsPat = glob2rx("*.xls") #wildcard-Transfer
  all.xls.files1 <- dir(path = sprintf("Models/%s",modelDir), pattern = xlsPat, all.files = T,  full.names = F, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE)
  
  all.xls.files<<- list()
  for(f in all.xls.files1)
  {
    # print(f)
    if (!file.info(sprintf("Models/%s/%s",modelDir,f))$isdir)
      all.xls.files <<- append(all.xls.files,f)
  }
  all.xls.files <<- unlist(all.xls.files )
  print(all.xls.files) 
  all.xls.files
}



#.......................................................................................
def.Globals<-function(modeldir="EM5_MAERZ_C") #"EM4_MAERZ.2" "EM4_MAERZ_B"
{
  source("mlib/signal.r")
  global_mm.tuned.ia =T   #werden ia.input-assumptions via forecast getuned ?
  global_look_on_share <<-T  
  do.par<<-F
  global_disable_cloud.check <<-T
  global_ignore_schon_da <<- T
  global_warte<<- F
  fundamental.lag <<-  20   #40
  do.load <<- T
  FUNDIS <<-list()
  backtest.frame<<- "::2014" 
  dont_use_last <<- 1# 200   #du hast die Targets ja nicht bis heute .. also tu nicht so als könntest du rollierend bis jeweils heute trainieren - sondern nur bis vor nem jahr !!!
  
  #provisorisch:
  dataSet<<- modelDir<<- modeldir;
  bench<<-"EXX50_RI";  
  mP("dataSet %s,  bench %s",dataSet,bench) 
  
  do.par=F; do.load=F; visual=F;xlsName="600.xls";fundis=c(2:11); experiment=define.experiments(bench=bench,path=modelDir)
  
  xlsPat = glob2rx("*.xls") #wildcard-Transfer
  all.xls.files1 <- dir(path = sprintf("Models/%s",modelDir), pattern = xlsPat, all.files = T,  full.names = F, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE)
  
  all.xls.files<<- list()
  for(f in all.xls.files1)
  {
    # print(f)
    if (!file.info(sprintf("Models/%s/%s",modelDir,f))$isdir)
      all.xls.files <<- append(all.xls.files,f)
  }
  all.xls.files <<- unlist(all.xls.files )
  print(all.xls.files) 
}

if (F)
  def.Globals()
#...............................................................................

faber<-function(p,n=200) p-SMA(p,n)  #zu viele verschieden defs unterwegs

#..........................................................................................
Auswertung.all<-function()
{
  def.Globals()
  
  Auswertung( bench="EXX50_RI", xlsName="Data0E.xls")  
  Auswertung( bench="GOLD_AM_FIX_TROY_OZ",  xlsName="Data0Gold.xls")
  
  Auswertung(  bench="SILVER_FIX_TROY_OZ", xlsName="Data0Silv.xls")
  Auswertung( bench="BMW_ST", xlsName="Data0BMW.xls")
  Auswertung( bench="DAIMLER", xlsName="Data0DAIMLER.xls")
  Auswertung( bench="VW", xlsName="Data0VW.xls")
  Auswertung( bench="CARS_U_PARTS", xlsName="Data0CARS_PARTS.xls")
}

if (F)
  Auswertung.all()


Auswertung<-function(bench="DAX30",xlsName="600.xls")
{
  
  prepare.EM5C.data(file=sprintf("%s_EM5C",xlsName), xlsName=xlsName,bench=bench,fundis=NULL,do.load=T,do.par=F, mkTrain=F,visual=F, modelDir=modelDir)
  
  #  ,            techbuilder.symbols=colnames(data$prices)       )
  #if (F)
  #   save(data,file="EM5C.data")
  experiment=define.experiments(bench=bench,path=modelDir,xlsName=xlsName)
  
  auswertung(Experi=experiment$macro.select.m,data=data,bestN=0) 
  
}

run.main<-function (bench="EXX50_RI",xlsName="Data0E.xls",fundis=NULL, modelDir="EM4_DEC.1")
{
  experiment=define.experiments(bench=bench,path=modelDir,xlsName=xlsName)
  
  if (schon.gemacht(file=sprintf("%s_EM5C",xlsName),modelDir=modelDir,xlsName=xlsName,Experi=experiment$macro.select.m,mode="train"))
  {
    mP("run.main: %s schon gemacht !",xlsName)
    return(T)
  }
  
  #3 lieferdaten-bl?cke:   die TargetZeitreihe  bench (tagedaten), fundamentale faktoren (monats-daten mit lieferlag von 31 Tagen),  intermarket -tagedaten (devisenkurse, zinsen,...)
  if (!schon.gemacht(file=sprintf("%s_EM5C",xlsName),modelDir=modelDir,xlsName=xlsName,Experi=experiment,mode="prep"))
  {
    prepare.EM5C.data(file=sprintf("%s_EM5C",xlsName), xlsName=xlsName,bench=bench,fundis=fundis,do.load=F,do.par=do.par, mkTrain=T,visual=F, modelDir=modelDir)
  }
  else
    prepare.EM5C.data(file=sprintf("%s_EM5C",xlsName), xlsName=xlsName,bench=bench,fundis=fundis,do.load=T,do.par=do.par, mkTrain=T,visual=F, modelDir=modelDir)
  
  #  ,            techbuilder.symbols=colnames(data$prices)       )
  #if (F)
  #   save(data,file="EM5C.data")
  
  k=paste(colnames(data$multiTarget[[1]]),collapse=", ")
  if (F)
  {
    TrainRun_EM5C(Experi=experiment$TEST , do.par=F)
    TrainRun_EM5C(Experi=experiment$TEST , do.par=T)
    TrainRun_EM5C(Experi=experiment$TEST , do.par=T)
    TrainRun_EM5C(Experi=experiment$macro.select , do.par=T)
  }
  TrainRun_EM5C(Experi=experiment$macro.select.m , do.par=do.par)
  
  #auswertung(Experi=experiment$macro.select.m,data=data,bestN=30) 
  
  
  trainBest <<- data$Train.best
  save(trainBest,file=sprintf("Models/%s/%s",modelDir,"trainBest.data"))
  
  try(utils::write.table(trainBest,file=sprintf("Models/%s/%s",modelDir,"trainBest.csv"),dec=".",sep=";",col.names=T,row.names=T))
  
  return("ok")
  
  sag("READY")
  
  #........................................
  if (F)
  {
    #  rattle()   #vorher data.frame  - daten global anlegen
    # als Vektor gehts hier in die monatliche Gewichtsberechnung
    
    #..lade aus der jeweiligen signal_randomForest_e4_%s.csv  das Tupe (price,signal,model.err,confidence ) in das environent  sysSig[[sym]]
    symSig=load.signals(Experi=experiment$macro.select.m, symbolnames=spl("EXX50_RI,DAX30,NIKKEI_225,SUP500"))
    ls(symSig)
    sym=symSig[[bench]]
    #.....................................
    sig.sys<-function(sym)    
    {
      signal=sign(sym[,"signal"])
      plotSigPrice(signal=signal,prices=sym[,"price"],indi=list(signal=sym[,"signal"],conf=sym[,"confidence"]),main=  attr(sym,"name")) 
      #lm.lowess(p=sym[,"price"],visual=T,getIndex=T,main=attr(sym,"name"),glaettung=0.2)
      library(robfilter)
      y=ts(sym[,"price"])     
      rf1=robust.filter(y,width=181,online=T)
      ls(rf1)  
      plot(rf1)
      #browser(mP("ok"))
      rf1.xts=xts(rf1$level,index(sym[,"price"]))
      #signal=xts(sign(rf1$slope),index(sym[,"price"])) #kommt aber noch der shiftd hinzu
      
      #rauschunterdrückende signal-berechnung
      signal=sign(diff(rf1.xts,30))
      #stabiler  .. die differenzen:   diff(1),diff(2), ..diff(30)  und deren sign-summe
      signal=rowSums(data.frame(sapply(1:30,FUN=function(d) sign(diff(rf1.xts,d)))))
      signal[is.infinite(signal)]<-0
      signal=xts(sign(signal),index(sym[,"price"]))
      
      plotSigPrice(signal=signal,prices=sym[,"price"],indi=list(signal=sym[,"signal"],conf=sym[,"confidence"]),main=  attr(sym,"name")) 
    }
    
    #schnell mal ein modell rechnen .................................................
    m.apply(symSig,Fun=sig.sys)
    
    if (F)
    {
      
      
      stopifnot(require(mFilter))
      
      #andere Filter
      library(mFilter)  #aber alle benutzen zukünftige Daten (nicht one sided, casual,  )
      example(mFilter)
      ls("package:mFilter")
      
      data(unemp)
      opar <- par(no.readonly=TRUE)
      unemp.hp <- mFilter(unemp,filter="HP") # Hodrick-Prescott filter
      print(unemp.hp)
      summary(unemp.hp)
      residuals(unemp.hp)
      fitted(unemp.hp)
      plot(unemp.hp)
      par(opar)
      
      len<-201
      eps<-rnorm(len)
      
      # lambda=1600 is a typical setting for working with quarterly GDP
      
      lambda_hp<-1600
      eps.hp <- hpfilter(eps,type="lambda", freq=lambda_hp)
      # plot data (here noise) and HP-trend
      
      plot(eps.hp$x)
      lines(eps.hp$trend,col=2)
      
      
      
      # Here is the coefficient matrix: it is a full revision sequence.
      
      parm<-eps.hp$fmatrix-diag(rep(1,len))
      parm<--parm
      
      # And a plot of the HP-filter coefficients: symmetric and real-time (concurrent) trend filters
      ts.plot(cbind(parm[,c(length(parm[1,])/2,length(parm[1,]))]),lty=1:2)
      ##############################################
      plot(abs(y))
      lines(predict(loess(abs(y)~time(y))), col='red', lwd=3)
      k <- 20
      lines(filter(abs(y), rep(1/k,k)), col='blue', lwd=3, lty=2)
      boxplot(log(abs(x))~s, col='pink')
      
      #######################
      library(tseries)
      x <- EuStockMarkets[,1]
      y <- diff(x)/x
      r <- garch(y)
      # plot(r)   The plot function is only for interactive use...
      op <- par(mfrow=c(2,1))
      plot(y, main = r$series, ylab = "Series")
      plot(r$residuals, main = "Residuals", ylab = "Series")
      par(op)
      #######################################>>>>>
      library(MSBVAR )
      
      
      data(IsraelPalestineConflict)
      Y.sample1 <- window(IsraelPalestineConflict, end=c(2002, 52))
      Y.sample2 <- window(IsraelPalestineConflict, start=c(2003,1))
      
      data(IsraelPalestineConflict)
      # Find the mode of an msbvar model
      # Initial guess is based on random draw, so set seed.
      set.seed(123)
      dim(IsraelPalestineConflict)
      xm <- msbvar(IsraelPalestineConflict, p=3, h=2,
                   lambda0=0.8, lambda1=0.15,
                   lambda3=1, lambda4=1, lambda5=0, mu5=0,
                   mu6=0, qm=12,
                   alpha.prior=matrix(c(10,5,5,9), 2, 2))
      # Plot out the initial mode
      plot(ts(xm$fp))
      print(xm$Q)
      # Now sample the posterior
      N1 <- 1000
      N2 <- 2000
      # First, so this with random permutation sampling
      x1 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=TRUE)
      # Identify the regimes using clustering in plotregimeid()
      plotregimeid(x1, type="all")
      
      # Now re-estimate based on desired regime identification seen in the
      # plots. Here we are using the intercept of the first equation, so
      # Beta.idx=c(7,1).
      x2 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=FALSE, Beta.idx=c(7,1))
      # Plot regimes
      plot.SS(x2)
      # Summary of transition matrix
      summary(x2$Q.sample)
      
      forecasts <- forecast(x2, nsteps=nrow(Y.sample2))
      forecasts.only <- forecasts[(nrow(Y.sample1)+1):nrow(forecasts),]
      # Plot forecasts and actual data
      i2p <- ts(cbind(Y.sample2[,1], forecasts.only[,1]),
                start=c(2003,1), freq=52)
      p2i <- ts(cbind(Y.sample2[,2], forecasts.only[,2]),
                start=c(2003,1), freq=52)
      par(mfrow=c(2,1))
      plot(i2p, plot.type=c("single"))
      plot(p2i, plot.type=c("single"))
      
      
      # Fit a BVAR model
      fit.bvar <- szbvar(Y.sample1, p=6, lambda0=0.6, lambda1=0.1, lambda3=2,    lambda4=0.25, lambda5=0, mu5=0, mu6=0, prior=0)
      # Forecast -- this gives back the sample PLUS the forecasts!
      forecasts <- forecast(fit.bvar, nsteps=nrow(Y.sample2))
      forecasts.only <- forecasts[(nrow(Y.sample1)+1):nrow(forecasts),]
      # Plot forecasts and actual data
      i2p <- ts(cbind(Y.sample2[,1], forecasts.only[,1]),
                start=c(2003,1), freq=52)
      p2i <- ts(cbind(Y.sample2[,2], forecasts.only[,2]),
                start=c(2003,1), freq=52)
      par(mfrow=c(2,1))
      plot(i2p, plot.type=c("single"))
      plot(p2i, plot.type=c("single"))
      ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ##########################################
      
      data(japan)
      japan.mra <- mra(log(japan), "mb8", 5, boundary="reflection")
      par(mfrow=c(7,1), pty="m", mar=c(5-2,4,4-1,2))
      plot(japan, type="l", axes=F, ylab="")
      for(i in 1:6)
        plot.ts(japan.mra[[i]], axes=F, ylab=names(japan.mra)[i])
      axis(side=1, at=c(5,45,85,125,165), labels=c(1956,1966,1976,1986,1996))
      title(xlab="Year")
      
      
      ##########################################
      #andere filter testen
      
      #lassen sich fundamentale fehlsignale mittels online-filter-vergleich ausfiltern ?
      #wie verhalten sich multivariate robfilter ? .. wahrscheinlich sollten korrelierte lead-komponenten (ungelagged)  hinzugegeben werden
      #baue ein ts-lars modell aus den wichtigsten faktoren.
      #teste  multi logistic regression mit parameter select: mnlm,  tsDyn
      
      Y=m.apply(symSig,Fun=function(sym) sym[,"price"],newName=col)
      Y=mNorm(Y)
      mPlot(Y)
      #####################
      #ordentliches System (ähnlich gut wie faber - basiered auf median-filter)
      
      pi=1
      Y.ts=ts(na.omit(prices[,pi]))
      
      #.......................
      library(robfilter)
      #ok !
      p0= med.filter(Y.ts,width=161,extrapolate=T,online=T)
      ls(p0)
      plot(p0)
      sig=sign(p0$slope)
      plot(p0$level)
      filter= xts(p0$level,index(prices[,1]))  #die glatte zeitreihe
      sig=rollRegressionXTS(filter,win=20)*100  #richtung der glatten zeitreihe
      plot(sig)
      #ausfiltern von zu kleinen signalen
      Sig=coarse.code(sig,b=10,visual=F)
      sig0=nval(Sig[which(sig==0)][1])
      sig=Sig-sig0
      plot(sig)
      sig[sig==0]=NA
      sig=m.ifna.prev(sig)  #bei zu kleinen signalen in bisheriger pos bleiben:  NICHT umschalten !
      
      signal= sign(sig)
      signal[signal <0]=0   #satt short:  flat
      
      b=na.omit(merge(signal,prices[,pi]))
      plotSigPrice(signal=b[,1],b[,2],indi=list(sig=merge(sig,0),filter=merge(b[,2],filter)))
      ##########################################################
      
      P=SMA(na.omit(ZigZag(p[1000:1500])),2)
      P=prices[,1]
      Y.ts=as.ts(P)
      
      p0=lm.lowess(P,glaettung=0.2)$y
      plot(p0)
      p1=lm.gamm(P,glaettung=20)
      
      filter= m.xts(cbind(coredata(Y.ts),p0),index(first(P,1)))  #die glatte zeitreihe
      mchart(filter)
      lowess(y~x,data=train,f=glaettung)
      sig=rollRegressionXTS(filter,win=20)*100  #richtung der glatten zeitreihe
      plot(sig)
      #ausfiltern von zu kleinen signalen
      Sig=coarse.code(sig,b=10,visual=F)
      sig0=nval(Sig[which(sig==0)][1])
      sig=Sig-sig0
      plot(sig)
      sig[sig==0]=NA
      sig=m.ifna.prev(sig)  #bei zu kleinen signalen in bisheriger pos bleiben:  NICHT umschalten !
      
      signal= sign(sig)
      signal[signal <0]=0   #satt short:  flat
      
      b=na.omit(merge(signal,prices[,pi]))
      plotSigPrice(signal=b[,1],b[,2],indi=list(sig=merge(sig,0),filter=merge(b[,2],filter)))
      ##########################################################
      
      p0= med.filter(Y.ts,width=111,extrapolate=T,online=T)
      filter= xts(p0$level,index(prices[,1]))  #die glatte zeitreihe
      
      p03=robreg.filter(Y.ts,width=301,extrapolate=1,online=T,method="MED")
      #p03= med.filter(Y.ts[,1],width=311,extrapolate=T,online=T)
      level_MED=xts(p03$level$MED, index(prices[,1]))
      level_MED = lag(filter,160)
      mchart(merge(filter,prices[,pi],level_MED))
      sig = filter-level_MED   #macd 
      
      mchart(merge(sig,0))
      #ausfiltern von zu kleinen signalen
      Sig=coarse.code(sig,b=10,visual=F)
      sig0=nval(Sig[which(sig==0)][1])
      sig=Sig-sig0
      plot(sig)
      #sig[sig==0]=NA
      #sig=m.ifna.prev(sig)  #bei zu kleinen signalen in bisheriger pos bleiben:  NICHT umschalten !
      
      signal= sign(sig)
      #signal[signal <0]=0   #satt short:  flat
      signal = signal[-c(1:500)]  #anfangs-effekt -ausblenden
      b=na.omit(merge(signal,prices[,pi]))
      plotSigPrice(signal=b[,1],b[,2],indi=list(sig=merge(sig,0),filter=merge(b[,2],filter,level_MED)))
      ##########################################################
      
      
      #dooof p0=lms.filter(Y.ts[,1],width=211,extrapolate=T,online=T)
      #evtl. gut für stop-sys  :
      p01= lqd.filter(Y.ts[,1],width=51,extrapolate=T,online=T)#  Least Quartile Difference filter
      #doof  p0= lts.filter(Y.ts[,1],width=51,online=T,extrapolate=1) #  Least Trimmed Squares (LTS) filter
      #doof   p0=rm.filter(Y.ts[,1],width=211,online=T,extrapolate=1)
      #interessant... ganz viele filter
      p02=robreg.filter(Y.ts[,1],width=51,extrapolate=1,online=T)
      p02=robreg.filter(Y.ts[,1],width=91,extrapolate=1,online=T)
      ls(p02)
      
      sig=p02$level$DR-p02$level$MED
      
      p1.sig=rollRegressionXTS(p2.xts[,1],win=100)*100
      p1.signal= sign(sig)
      
      b=na.omit(merge(p1.signal,prices[,1]))
      plotSigPrice(signal=b[,1],b[,2])
      
      ################# 
      px1=dr.filter(Y.ts[,1],width=51,extrapolate=1,online=T)     #Robust Double Window Filtering Methods for Univariate Time Series
      px2=dw.filter(Y.ts[,1],width=51,extrapolate=1,online=T)
      px3=hybrid.filter(Y.ts[,1],width=51,extrapolate=1,online=T) 
      
      plot(p02,col="blue");#lines(Y.ts[,1],col="red")
      
      p3=madore.filter(Y.ts,min.width=51,max.width=301) #wenig delay .. wird aber auch nicht sehr glatt
      p2= adore.filter(Y.ts,rtr=2) #wenig delay .. wird 
      
      raw = p2$level[1:shape(prices)]
      p1.xts=na.omit(xts(p1$signals, index(prices)))
      #  ls(p2)
      p2.xts=na.omit(xts(raw, index(prices)))
      plot(prices[,1]);lines(p2.xts,col="red",lwd=2)
      p1.sig=rollRegressionXTS(p2.xts[,1],win=100)*100
      p1.signal= sign(p1.sig[,1])
      
      b=na.omit(merge(p1.signal,prices[,1]))
      plotSigPrice(signal=b[,1],b[,2])
      
      mchart(merge(p1.xts[,1],prices[,1]))   
      
      p2.sig=rollRegressionXTS(p2.xts,win=20)*100
      p2.signal= sign(p2.sig)
      
      len(index(sym[,"price"]))
      
      colnames(p1.xts)=symSig$symbolnames
      
      fit <- StructTS(Y.ts[,1], type = "level")
      lines(fit$fitted)
      ####################
      
      #  "dyn" currently works with any regression function that makes use of "model.frame" and is written in the style of "lm". This includes "lm", "glm", "loess", "rlm" (from "MASS"), "lqs" (from "MASS"), "MCMCregress" (from "MCMCpack"), "randomForest" (from "randomForest"), "rq" (from "quantreg") and others. The time series objects can be one of the following classes: "ts", "irts", "its", "zoo" or "zooreg".
      
      #  Typically "dyn" is used like this "dyn$lm(y ~ lag(y, -1))". T
      ############################################################
      ## load the packages and code we need
      ## load custom functions
      source("https://github.com/gavinsimpson/random_code/raw/master/derivFun.R")
      source("https://github.com/gavinsimpson/random_code/raw/master/tsDiagGamm.R")
      
      #http://www.r-bloggers.com/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/
      
      P=mNorm(p)
      plot(P)
      m1=lm.gamm(P,glaettung=20)
      summary(m1$gam)
      ## look at autocorrelation in residuals:
      acf(resid(m1$lme, type = "normalized"))
      ## ...wait... look at the plot, only then do...
      pacf(resid(m1$lme, type = "normalized"))
      ## seems like like sharp cut-off in ACF and PACF - AR ter
      
      m2=lm.gamm(P,glaettung=20, correlation = corARMA(form = ~ x, p = 1))
      HotLags2.cc(P,P,n=60,nLag=60,visual=T,main="")$lag  #0  .. aber der pacf 1 hat was ..
      #das Modell mit dem kleinsten AIC ist das beste:
      anova(m1$lme, m2$lme)
      best=m2
      plot(best$gam, residuals = TRUE, pch = 19, cex = 0.75)
      
      
      #historische trend-ablesung
      m2.d <- Deriv(m2, n = 200)
      plot(m2.d, sizer = TRUE, alpha = 0.01)
      ##########################################################
      
      
    }
    #MM_OPT
    #alles Informationen in ein xts packen - aus preisen returns machen
    symSig.xts=foreach(col=colnames(symSig[[bench]]),.combine="cbind") %do%
{
  if (col=="price")
    r=m.apply(symSig,Fun=function(sym) diff(log( sym[,"price"])),newName=col)
  else
    r=m.apply(symSig,Fun=function(sym) sym[,col],newName=col)
}   
colnames(symSig.xts)

r= na.omit(symSig[[bench]][,"model.err"])#to.monthly weil auch der model.err nur monthly drin steht
#r ist nur ein zeitlineal für rollGenSa.Portfolio

r=r[experiment$macro.select.m$backtest.frame]
#  purePlot(mLogRendite(r))
#m.tslars(r)
weights= rollGenSA.Portfolio(r,win=3,mode ="maxcalmar",symSig = symSig.xts)  ### << test.me

turnover.1=weights[,1];turnover.1[]=NA;
turnover.1[]=(rowSums(abs(weights-lag(weights))))
apply.yearly(turnover.1, FUN=sum)*100
  }
}
#####################################################################################
#..lade aus der jeweiligen signal_randomForest_e4_%s.csv  das Tupe (price,signal,model.err,confidence ) in das environent  sysSig[[sym]]

load.signals<-function(Experi=experiment$macro.select.m,symbolnames)
{
  symSig = new.env()
  experi=  deparse(substitute(Experi))  
  experi=rightOf(experi,"\\$")
  Path=unlist(str_split(Experi$path,"/"))[1]
  mP("..............auswertung zu  %s",experi)
  # symbolnames=Experi$symbolnames
  
  lapply(symbolnames,function(sym){
    #  sym="EXX50_RI"
    path= sprintf( "Models/%s/%s/%s/", Path,sym,experi);   
    
    signal.csv=read.csv(sep=";",dec=".",header=T,file=sprintf("%sssignal_randomForest_e4_%s.csv",path,sym))
    colnames(signal.csv)
    
    dat = as.Date(signal.csv[,"Date"])
    p=xts(signal.csv[,sym],dat);colnames(p)="price"
    sig=xts(signal.csv[,"signal"],dat); colnames(sig)="signal"
    model.err = xts(signal.csv[,"model.err.1"],dat); colnames(model.err)="model.err"
    confidence= xts(signal.csv[,"confidence"],dat); colnames(confidence)="confidence"
    mPlots(p,merge(sig,0),model.err,confidence,title=sym)
    assign(sym,merge(p,sig,model.err,confidence),envir=symSig)
  })
  assign("symbolnames",symbolnames,envir=symSig)
  print("symSig loaded for: ")
  print(ls(symSig))
  symSig
}
#............. 
#  Trainingszenarien vorbereiten
# A)Target(-1)
# 0) retrain_on = "monthly"

# CloudSet:
# aa) nur
# a) nur tech.cloud(sym)
# b) tech.cloud(sym)+small.cloud(intermarket)
# c) tech.cloud(sym)+small.cloud(fundamental)
# d) tech.cloud(sym)+small.cloud(intermarket)+small.cloud(fundamental)
# e) tech.cloud(sym)+tech.cloud(intermarket)+small.cloud(fundamental)

# Variablen-Identifikation, Timing + Selektion

# B) Target(-1)+Target(smooth)+Target(-1,short)
# 0) retrain_on = "monthly"
# Bestes CloudSet aus A)
define.experiments<-function(bench="EXX50_RI",path=modelDir,xlsName="")
{
  path=sprintf("%s/%s",path,bench)
  
  experiment=list(
    TEST= list(
      path=path,
      symbolnames=data$Y.names,  xlsName=xlsName, 
      backtest.frame=backtest.frame,
      Target.fn= select.multiTarget.sum,
      train.data=data$test.cloud,
      retrain.on= "yearly",
      par=list()
    ),
    
    macro.select= list(
      path=path,
      symbolnames=data$Y.names,   xlsName=xlsName,
      backtest.frame=backtest.frame,
      Target.fn= select.multiTarget.sum,
      train.data=data$cloud.mac,
      retrain.on= "quaterly",
      par=list()
    ),
    
    macro.select.m= list(
      path=path,
      symbolnames=data$Y.names,  xlsName=xlsName, 
      backtest.frame=backtest.frame,
      Target.fn= select.multiTarget.sum,
      train.data=data$cloud.mac,
      retrain.on= "monthly",
      par=list(),
      INFO=c(" ","select.multiTarget.sum =>", paste(names(data$multiTarget),collapse=", "),            "Target =>",paste(colnames(data$multiTarget[[1]]),collapse=", "),
             "train.data =>"," ",paste(as.character(dim(data$cloud.mac)),collapse=" : ")
             ," ", paste(colnames(data$cloud.mac),collapse=", "))
    ),
    
    macro.select.m2= list(
      path=path,
      symbolnames=data$Y.names,  xlsName=xlsName,  
      backtest.frame=backtest.frame,
      Target.fn= select.multiTarget.sum,
      train.data=data$cloud.mac,
      retrain.on= "monthly",
      info="cloud.mac on",
      par=list()
    ),
    
    justTech.m= list(
      path=path,
      symbolnames=data$Y.names,   xlsName=xlsName,
      backtest.frame=backtest.frame,
      Target.fn= select.multiTarget.sum,
      train.data=get.techCloud,
      retrain.on= "monthly",
      par=list()
    )
  )
}

##########################################################################
#shoudld i skip
##########################################################################
schon.gemacht<-function(file,modelDir,xlsName,Experi,mode=spl("*,prep,train"))
{
  path2=getwd(); path2=str_replace(string=path2,str_sub(path2,start=1,2),"o:")  #das aus
  file.name = sprintf("Models/%s/%s.data",modelDir,file)
  
  if (mode=="*" || mode == "prep")
  {
    if (file.exists(file.name))
    {
      mP("schon.gemacht: %s",file.name)
      return (T)
    }
    if (global_look_on_share)
    {
      file.name2 = sprintf("%s/Models/%s/%s.data",path2,modelDir,file)    
      if (file.exists(file.name2) )
      {
        mP("schon.gemacht: %s",file.name2)
        return(T)
      }
    }
  }
  if (mode=="*" || mode == "train")
  {
    experi=  deparse(substitute(Experi))  
    experi=rightOf(experi,"\\$")
    xlsName=Experi$xlsName
    path= sprintf( "Models/%s/%s/", Experi$path,experi);  
    infoFile =sprintf("%s/ExperimentInfo_%s.txt",path,xlsName)
    infoFile2 = sprintf("%s/%s/ExperimentInfo_%s.txt",path2,path,xlsName)
    if (global_look_on_share)
    {
      if (file.exists(infoFile2))
      {
        mP("schon.gemacht: %s",infoFile2)
        return (T)
      }
    }
    if (file.exists(infoFile) )
    {
      mP("schon.gemacht: %s",infoFile)
      return(T)
    }
  }
  return (F)
}
#Tagesdaten oder wochen daten ? -> winlen im cloud-maker anppassen 
#tagesdaten -> sind die indikatore  f?r die preise schnell ?
#auswerten mit signal oder mit non-lin-optimierer
prepare.EM5C.data <-function(file="EM5C",xlsName="Data0E.xls",bench="EXX50_RI",fundis=NULL, do.load=T,do.par=T,mkTrain=T,techbuilder.symbols=c(),visual=F,modelDir="EM4_DEC.1")
{
  file.res = sprintf("Models/%s/%s.data",modelDir,file)
  dir.create(dirname(file),recursive=T)
  mP("#### prepare.EM5C.data for %s #### ",file.res)
  
  if  (!do.load && file.exists(file.res))
  {
    mP("skip  prepare.EM5C.data:  result file  already exists \n %s ",file)
    if (global_ignore_schon_da) return("skip")
  }
  if (do.load && !file.exists(file.res)) #evtl. wurde ja auf dem austauschlaufwerk alles bereit gelegt
  {
    path2=getwd(); path2=str_replace(string=path2,str_sub(path2,start=1,2),"o:")  #das austausch-laufwerk für alle
    file.res = sprintf("%s/Models/%s/%s.data",path2,modelDir,file)
    mP("prepare.EM5C.data: load prepared file from \n %s ",file)
    
  }
  if (do.load && !file.exists(file.res)) #evtl. wurde ja auf dem austauschlaufwerk alles bereit gelegt
  {
    mP("skip  prepare.EM5C.data:  result prep-data not ready exists \n %s ",file)
    return("skip")
  } 
  
  file=file.res
  
  if (do.load  )
  {
    mP("load data from %s",file.res)
    if ( file.res=="Models/EM4_MAERZ.1/611.xls_EM5C.data")
    {
      print("#MM1")
      #  browser()
    }
    local({
      data=NULL
      try(load(file=file.res))     #sichern falls er crashed
      if (is.null(data))
      {
        mP("prepare.EM5C.data: can't read file %s",file.res)
        return("bug")
      }
      print(ls(data))
      data<<-data
      global_StartDate
      define.Globals(ver=1)
    })
    if (do.par)
      prepare_Parallel()
    return("ok")
  }
  #PREP
  data <<-make_eckhard4C_data(visual=visual,xlsName=xlsName,bench=bench,do.par=do.par,mkTrain=F,fundis=fundis,modelDir=modelDir)
  
  #falls in technbuilder.symbols (die sym-namens-liste für die die sehr aufwändige)
  #techcloud gebaut werden soll - Y steht-  wird diese mit dem Y.names ersetzt
  #Y.names ist das/die Symbole die in diesem Datensatz vorhergesagt/getraded werden soll
  if ("Y" %in% techbuilder.symbols && len(data$Y.names)>0)
  {
    yi= which(techbuilder.symbols=="Y")
    techbuilder.symbols = data$Y.names
  }
  
  cloud.check(data$prices)
  
  data.periods(data)
  print("data$macros------------------->")
  
  print(ls(data$macros))
  #MMDATA
  #monats-daten:  die macros 
  data$macros = na.omit(merge(data$macros,data$prices))  #wir nehmen noch die euro.indizes als speudo-macros mit auf .. wer wei?
  cloud.check(data$macros)
  
  #MMCLOUD  
  #aus den folgenden Bausteinen bau ich mir spaeter meine sym-individuelle datacloud
  #macro-cloud berechen  - bei visual = T wird auch jeder variablenblock ge cloud.checked() !!
  #auf tagesbasis
  data$cloud.mac <- cloud.mac <<- small.cloud(data$macros,visual=F,cloud.control= list(get.Quantile.M=F)) 
  #small-cloud f?r mac incls preise
  #passt die cloud Qualit?t ?  
  cloud.check(cloud.mac)
  data$test.cloud  <-test.cloud <<- micro.cloud(data$prices,visual=F,cloud.control= list()) 
  cloud.check(test.cloud)
  
  if (F&&visual)
  {
    k=plotSigPrice(signal=sign(target),prices=data$prices[,data$BENCH])
    mPlots(k,data$prices[,data$BENCH],merge((target),0))
  }
  
  #### baue die multiTargets:
  #NOTWENDIG
  if (do.par ) prepare_Parallel() 
  
  #Baue in   data->tech.cloud[[sym]]  die sehr aufw?ndige tech-cloud   f?r das zu prognostizierende objekt
  
  #gem. unterschiedlichen zeitlichen aufl?sungen und mehtoden targets bauen
  if (mkTrain)
  {
    data$multiTarget <-  multiTarget(visual=visual) #das target eines symbols bekommst Du mit mt=select.multiTarget.sum(data, sym) oder mt=select.multiTarget (data, sym)
    #data$target=select.multiTarget.sum(data,data$BENCH)
    
    ls(data$multiTarget)
    #passt die target Qualit?t ?
    lapply(data$multiTarget ,cloud.check)
  }
  
  if (len(techbuilder.symbols) > 0)
    tech.cloud.builder(techbuilder.symbols,tech.Cloud.Fn= "tech.Cloud", data,visual=F,cloud.control=spl("heavy,lagged"),do.par=do.par)
  
  
  #..................................................................................<<
  
  save(data,file=file)     #sichern falls er crashed
  
  if (do.par)
  {
    mP("sfExport data")
    sfExport("data")
  }
}

#..............................................

if (F)
{
  #einlesen von xls -sheets in xts- variable
  exx50.all= read.EckhardsXLS(modelDir="EM4_DEC.1", xlsName="Data0E.xls",startRow=5,belowColnames=5, date.format = "%d.%m.%Y",to.month.end=F,debug=F)  
  
  
  if (F)  #ist viel Schrott in den Spalten ?
  {
    colSums(diff(exx50.all["1997::"],30),na.rm=T)
    colSums(exx50.all["1997::"],na.rm=T)
  }
  
  dim(exx50.all)  #4392 Tage mit 39 Spalten
  #enh?lt folgende 3 logischen Gruppen
  exx50=exx50.all[,1]
  fundamentale.faktoren= exx50.all[,c(2:25)]
  intermarket.faktoren = exx50.all[,c(26:ncol(exx50.all))]
  
  colnames(exx50)
  colnames(fundamentale.faktoren)
  colnames(intermarket.faktoren)
  
  exx50.n =mNorm(exx50)["1997::"]
  #Die Fundamentalfaktoren haben einen time lag von 20 Tagen, die Inter-Market Faktoren haben keinen time lag. 
  fundamentale.faktoren= mNorm(lag(fundamentale.faktoren,20))["1997::"] #hier schon mal gelagged weil die Teile ja einen Monat Lieferversp?tung haben
  #head(merge(fundamentale.faktoren[,1],fundamentale.faktoren.n[,1]),30)
  intermarket.faktoren = mNorm(intermarket.faktoren)["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  
  #spannende analysen
  if (F)
  {
    
    pdf(file = 'Models/EM4_DEC.1/correlations.pdf', width=8.5, height=11)
    
    purePlot(fundamentale.faktoren.n,main="fundamentale.faktoren.n")
    purePlot(intermarket.faktoren,main="intermarket.faktoren")
    
    correlogramm(ROC(merge(exx50,intermarket.faktoren)["1997::"],30),main=" intermarket.faktoren")
    correlogramm(ROC(merge(exx50,fundamentale.faktoren)["1997::"],30),main=" fundamentale.faktoren")
    
    chart.Correlation(ROC(intermarket.faktoren["1997::2000"],1),main="iF 1997::2000")
    chart.Correlation(ROC(intermarket.faktoren["2008::2011"],1),main="iF 2008::2011")
    chart.Correlation(ROC(intermarket.faktoren["2010::2014"],1),main="iF 2010::2014") 
    chart.Correlation(ROC(intermarket.faktoren["2010::2014"],30),main="iF 2010::2014") 
    
    chart.Correlation(ROC(fundamentale.faktoren["1997::2000"],1),main="fF 1997::2000")
    chart.Correlation(ROC(fundamentale.faktoren["2008::2011"],1),main="fF 2008::2011")
    chart.Correlation(ROC(fundamentale.faktoren["2010::2014"],1),main="fF 2010::2014") 
    chart.Correlation(ROC(fundamentale.faktoren["2010::2014"],30),main="fF 2010::2014") 
    
    
    
    dev.off()  
    
  }
  
  TSA.prepPro.e4C(visual=F,bench="EXX50_RI",do.par =T)
  
}
#ein virtuelles data-env vorbereiten - damit das SIT-Framework laufen kann

#data <- make.data(euro.indi,mkTrain=T)
#rattle()

#eine multi-Target-DataCloud vorbereiten  .. einige technische Indikatoren auf indizes und macros 
#ROC, p-SMA200
#..................................................................................................<<
get.techCloud <-function(data,sym)
{
  data$tech.cloud[[sym]]
}

###############################################################################
#macros (monatlich) und indizes(t?glich) einladen
#eine tagesbasierte tech.cloud   (dataCloud.e4) f?r jeden Index berechnen
#die targets pro symbol berechnen
#zu den macros die indizes als pseudo-macros monatlich beimischen
#die monats basierte small.cloud() berechnen
#eine gro0e  all.cloud als data$train.data  berechnen - dazu 
#zu der tech.cloud die macro-cloud beimischen  und mit m.ifna.prev die monatsdaten auff?llen
# - zu jedem symbol gibts nun als trainings-daten die breite  all.cloud mit
# macros - indizes,  5 features dazu - und die tech.cloud (70 feature auf Tagesbasis)

#signal.randomForest.e4() berechnet damit Signale - indem der in festen Intervallen -
#z.B retrain.on="quarterly" - den random.forrest-classifier des jeweiligen index-symbols  neu trainiert.
# auf Tagesbasis gibt signal.randomForest.e4() dann  A) all.sig-Einsch?tzungen mit den 3 Merkmalen
#signal, model.err, confidence  und B) auf monats.basis:   model.err + forest.fit.wichtigkeit
#forest.fit.wichtigkeit ist dabei ein xts welches f?r jeden Trainingstag die Wichtigkeit aller features notiert.
###############################################################################
TSA.prepPro.e4C<-function(visual=T,bench="DAX30",do.par=T)
{
  
  if(F)
  {
    visual=T;bench="EXX50_RI";do.par=T
  }
  #..................................................................................................>>
  
  #daten einlesen, target-data berechnen und data-environment und global_arg vorbereiten - auch sfExport()
  data <-make_eckhard4C_data(visual=visual,bench=bench,do.par=do.par)
  #cloud-bildung parametrisieren
  #paralle vorbereiten
  
  #do.par=T
  if (do.par)  #nur in der Entwicklungsphase hilfreich...
  {
    prepare_Parallel() 
    
    sfSource("MLib/EM5C.R")
    sfSource("MLib/InputConfig_Portfolio_TD.R")
    sfSource("MLib/classifier_randomForest.r")
    sfExport("data")  ### muss erst der ifo- geladen werden ?
    sfExport("cloud.control")
  }
  
  sfExport("data")  ### muss erst der ifo- geladen werden ?
  sfExport("cloud.control")
  
  #datacloud berechnen
  #data$train.data=new.env() #l?scht bereits erstellte train.data
  
  #sfStop()
  #..................................................................................................>>
  #..................................................................................................>> 
  
  if (F)
  { 
    do.par
    load(file="EM5C.data")
    prepare_Parallel() 
    #sfExport("data")
    
    ls(data);   dim(data$train.data[["EXX50_RI"]])
  }
  #forest-trainieren+evaluieren -> signale,rankings und variablenSelektion  berechen
  
  
  #................................................................................
  #der Start der Experiment-Runs
  TrainRun_EM5C(Experi=experiment$TEST , do.par=F)
  
  TrainRun_EM5C(Experi=experiment$macro.select , do.par=F)
  TrainRun_EM5C(Experi=experiment$macro.select.m , do.par=F)
  
  mP("fertig mit allen test-train-runs")
  auswertung(Experi=experiment$macro.select)
  auswertung(Experi=experiment$macro.select.m)
  
  
  
  
  
  #noch UNGENUTZT
  #Alles in einem:
  #TSA-Maschine:
  #cloud berechenen, 
  #forest-trainieren+evaluieren -> signale,rankings und variablenSelektion  berechen
  #timing + selektion + allokation
  if (F)
    x=indi.Generic("signal.randomForest.e4", global_arg, 
                   par=list(sma.w=200),
                   xarg=list(),
                   T.arg = list(stop.on="stopsys.sma200"),
                   S.arg = list(nTop=-2, kTop=0, rank.fn = "rank.at.data"),
                   visual=T, TRAINSYM =-1, experiment="",
                   A.arg = list(alloc.at="months", 
                                commission = list(cps = 0.1, fixed = 0.0, percentage = global_commission))
    )
  
  #28.11.2013
  #summiere die variablen-wichtigkeit  und modell-qualit?t  (gibt jeweils ein Zeitreihenk?rzel nach| mit nach colnames)
  #samt caret- baum-chart dazu
  #nimm die macro-variablen hinzu, als:  faber, slope, atr.d, orange.slope, ROC, mNorm
  #  
  
  
  #test signal-output f?rs timing  und - in verbindung mit seiner sicherheit -  f?r die selektion mit 
  #TSA-aufruf:  indi.Generic  (...run.TSA())
  #sfLapply  ?ber alle Symbole - dazu noch in der cloud in jeden spaltennamen das sym reinmischen
  
  #aspekte der tech.cloud (labor.signal.r) hinzumischen
}

##################################################################################


TrainRun_EM5C<-function(Experi=experiment$macro.select , do.par=do.par)
{ 
  #MMRUN
  #  experi = Experi$experi
  experi=  deparse(substitute(Experi))  
  experi=rightOf(experi,"\\$")
  xlsName=Experi$xlsName
  path= sprintf( "Models/%s/%s/", Experi$path,experi);  dir.create(path,recursive=T)
  infoFile =sprintf("%s/ExperimentInfo_%s.txt",path,xlsName)
  path2=getwd(); path2=str_replace(string=path2,str_sub(path2,start=1,2),"o:")  #das austausch-laufwerk für alle
  
  infoFile2 = sprintf("%s/%s/ExperimentInfo_%s.txt",path2,path,xlsName)
  if (file.exists(infoFile) || file.exists(infoFile2))
  {
    mP("skip TrainRun_EM5C, file exists: %s",infoFile)
    if (!global_ignore_schon_da) return("skip")
  }
  
  info=list.info(Experi,name= experi)
  cat(info,file=infoFile,sep="\n")
  print(info)
  dir.create(dirname(infoFile2),recursive=T)
  
  file.copy(from=infoFile,to=infoFile2)  #markiere das file als in arbeit
  
  mP("..............TrainRun_EM5C  %s",experi)
  
  retrain.on =Experi$retrain.on
  symbolnames=Experi$symbolnames
  
  symbolnames=colnames(data$multiTarget[[1]])
  
  Target.fn= match.fun(Experi$Target.fn)
  train.data=Experi$train.data
  par=Experi$par
  backtest.frame=Experi$backtest.frame #"::2013-05-31"
  
  all.sig=lapply(symbolnames,function(sym)
    #all.sig=sfLapply(symbolnames,function(sym)    #ich mach die parallelisierung jetzt eins tiefer, in signal.randomForest.e4()
  {
    #    sym=data$BENCH
    mP(">>TrainRun_EM5C %s %s",experi,sym)
    data$Target = Target.fn(data,sym)  #die Target-Daten des aktuellen symbols    
    if (is.character(train.data))
    { 
      train.data.fn = match.fun(train.data)
      train.data= train.data.fn(data,sym)
    }
    colnames(data$Target)=sym
    data$train.data = merge(data$Target, train.data)
    print(dim(data$train.data)  )
    data$crs=new.env() #l?scht das schon gelernte
    arg= list(dat =data, clos=data$prices[backtest.frame,sym],Target=data$Target[backtest.frame] )
    
    if (do.par)
    {
      #      prepare_Parallel() 
      
      sfExport("retrain.on")
      sfExport("data")
    }
    #....... der parallel-core-aufruf zum iterieren (forest-lernen+forst-nutzen) 
    
    RES= signal.randomForest.e4c(arg,par,retrain.on=retrain.on,visual=F,do.par=do.par)
    
    #  browser(mP("RES"))
    res = RES$res
    ls(res)
    signal=res[[sprintf("%s.sig",sym)]]
    head(signal)
    wichtigkeit =res[[sprintf("%s.forest.fit.wichtigkeit",sym)]]
    mP("#a1")
    #browser(mP("xxxx res"))
    save(signal,file=sprintf("%ssignal_randomForest_e4_%s",path,sym),signal)
    write.xts(merge(data$prices[fromToS(signal),sym],signal),sprintf("%sssignal_randomForest_e4_%s.csv",path,sym))
    
    save(wichtigkeit,file=sprintf("%swichtigkeit_randomForest_e4_%s",path,sym),wichtigkeit)
    write.xts(wichtigkeit,sprintf("%swichtigket_randomForest_e4_%s.csv",path,sym))
    
    
    if (do.par)
      sfStop() 
    
    return(res)
  })
  
} #.................................................................................................................
#######################################################################################

auswertung<-function(Experi=experiment$macro.select, data=data, bestN=0,findMinimalFeatureSet=F)
{
  experi=  deparse(substitute(Experi))  
  experi=rightOf(experi,"\\$")
  mP("..............auswertung zu  %s",experi)
  symbolnames=Experi$symbolnames
  path= sprintf( "Models/%s/%s/", Experi$path,experi);  
  opath= sprintf( "Models/%s/%s/auswertung/", Experi$path,experi);   
  dir.create(opath,recursive=T)  
  
  
  #info=list.info(Experi,name= experi)
  #cat(info,file=sprintf("%sExperimentInfo.txt",path),sep="\n")
  
  #................................................................................
  data$Signal=list()
  data$Train.best=list()
  Target.fn= match.fun(Experi$Target.fn)
  train.data=Experi$train.data
  par=Experi$par
  backtest.frame=Experi$backtest.frame #"::2013-05-31"
  
  
  
  #MMAUSW
  #auswertung  .. wie ver?ndert sich die forest.fit.wichtigkeit  ... 
  no=lapply(1:len(symbolnames),function(sym.i) 
  {
    #sym.i=1
    sym=symbolnames[sym.i]
    
    data$Target = Target.fn(data,sym)  #die Target-Daten des aktuellen symbols    
    if (is.character(train.data))
    { 
      train.data.fn = match.fun(train.data)
      train.data= train.data.fn(data,sym)
    }
    data$train.data = merge(data$Target, train.data)
    print(dim(data$train.data)  )
    
    #ls(res[[1]]) #"confidence" "model.err"  "signal" 
    if(F)  #noch im speicher .
    {
      res = all.sig[[sym.i]]
      forest.fit.wichtigkeit=res[[sprintf("%s.forest.fit.wichtigkeit",sym)]]
      sig=  res[[sprintf("%s.sig",sym)]]
    }
    else #schon serialisiert
    {
      fname=sprintf("%ssignal_randomForest_e4_%s",path,sym)
      if (!file.exists(fname))
      {
        mP("still not ready %s",sym)
        forest.fit.wichtigkeit=NULL
      }
      else
      {
        load(file=fname)
        sig=signal
        load(file=sprintf("%swichtigkeit_randomForest_e4_%s",path,sym))
        forest.fit.wichtigkeit = wichtigkeit
      }
    }
    if(!is.null(forest.fit.wichtigkeit))
    {
      #browser(mP("###1"))
      #die trainings-qualit?t ?ber die zeit
      model.err = forest.fit.wichtigkeit[,1]
      mPlots(merge(sig[,1],0),sig[,2],data$prices[,sym],model.err,title="signal+condfidence+model.err")
      
      pdf(file = sprintf("%sZoomPlot_%s.pdf",opath,sym), width = 8.5, height = 11)  
      layout(1:3)  
      colnames(model.err)
      # model.err.2=model.err[,-c("Index,signal,confidence,model.err,model.err.1,pred.err")]
      purePlot(model.err, main=sprintf("model.err %s",sym))
      #die Variablen-Wichtigkeit ?ber die Zeit
      dim(forest.fit.wichtigkeit[,-1])
      #browser(mP("now"))
      #head(colnames(forest.fit.wichtigkeit))
      
      forest.fit.wichtigkeit=try(forest.fit.wichtigkeit[,-which(colnames(forest.fit.wichtigkeit) %in% c("model.err.1","pred.err"))])
      purePlot(forest.fit.wichtigkeit,main=sprintf("forest.fit.wichtigkeit %s",sym))  
      #browser(mP("#A1"))
      wich=summary(forest.fit.wichtigkeit)
      
      wich=wich[,order(wich[4,],decreasing=T)  ]
      wich.col=colnames(wich)
      wich=t(wich);rownames(wich)=wich.col
      wich=wich[,c(1,3,4,6)]
      View(wich)
      
      colMeans(forest.fit.wichtigkeit)
      #plotSigPrice(signal=sign(signal-1), prices=p[fromToS(signal)], indi=list(sollIst=merge(signal,target,0)))
      #mPlots(p[fromToS(target)],merge(target,0),abs(bug),title="target->signal")  
      
      b=na.omit( merge((sig[,1]), data$prices[,sym]))
      data$Signal[[sym]]=sig[,1]
      
      plotSigPrice(signal=b[,1],prices=b[,2])#,indi=list(conf=merge(sig[,2])))
      dev.off();dev.off()
      
      utils::write.table(wich,file=sprintf("%swichtigkeit_%s.csv",opath,sym),dec=".",sep=";",col.names=F,row.names=T)
      
      plotSigPrice(signal=b[,1],prices=b[,2])#,indi=list(conf=merge(sig[,2])))
      
      png(file = sprintf("%splotSigPrice_%s.png",opath,sym), width = 1200, height = 600, units = 'px', pointsize = 12, bg = 'white')  
      plotSigPrice(signal=b[,1],prices=b[,2])#,indi=list(conf=merge(sig[,2])))
      
      dev.off()
      df=data.frame(colMeans(forest.fit.wichtigkeit["2010::",-1]))
      
      #.............................................................
      # Ausfilern der wichtigsten Fundamentalfaktoren
      #
      #die pro quartal gemittelten faktoren-gewichte
      df.q= apply.yearly(x=forest.fit.wichtigkeit[,-1], colMeans)
      #View(df.q)
      #browser(mP("pre ntop"))
      #die 100 wichtigsten quartals-faktoren
      nt=ntop(df.q, min(300,ncol(df.q)))
      #nt[,"EXX50_RI.1"]
      #View(df.q)
      #ihre-colnames
      k=bt.apply.matrix(nt, function(col) {ifelse(col!=0,colnames(col),"")})
      #View(k)
      #nun reduziert auf die fundamentalfaktoren (technische faktoren haben vor  <sym>. im colname)
      #(vorsicht- da fliegen die ganzen Preise raus )  
      k2=bt.apply.matrix(k, function(col) 
      {iif (col!="" & len(grep(sprintf("%s.",sym),col )) == 0  ,col,"")})
      k2=k
      # k2=k  #ohne Beschr?nkung auf FundamentalfaktorenQ
      # View(k2)
      k3= lapply(1:nrow(k2), FUN=function(row.i){spl(paste(k2[row.i,]))})
      #pro listenvektor ein vector mit den fundi-gewichten
      k4=lapply(1:len(k3),function(c1) sapply(k3[[c1]],function(x) df.q[c1,x]))
      #das faktor-gewicht soll wenigstens 4 sein
      K4=data.frame(do.call("cbind",k4))
      colnames(K4)=as.character(as.Date(index(k2)))
      View(K4)
      K4.merk=clone(K4)
      #................summiere die sym-wichtigkeit.................................................
      
      symbols=lapply(rownames(K4), function(col) 
      {if(col=="Dates") return(list(symbol="Dates",0)); res=unlist(str_split(col,"\\."))[1]  ; mP(" %s -> %s",col,res); list(symbol=res,rowSums(K4[col,],na.rm=T))})
      #doofes write.table .. muss hier selber die columnnames basteln
      K4=rbind(Dates=c(colnames(K4)),K4)
      utils::write.table(K4,file=sprintf("%sTop300Faktoren_%s.csv",opath,sym),dec=".",sep=";",col.names=F,row.names=T)
      #.......................................................
      #kummuliere alle Wichtigkeit auf eines symbols - damit bekannt ist welche Faktor-Zeitreihen (unabhängig von leads oder sonstigen Vorverarbeitungen ) am wichtigsten sind.
      
      OAW=data.frame(rbindlist(symbols))
      sumsym=by(OAW[,2],OAW[,"symbol"],sum)
      symSum=sort(sumsym,decreasing=T)
      utils::write.table(symSum,file=sprintf("%sSymbolWichtigkeit_%s.csv",opath,sym),dec=".",sep=";",col.names=F,row.names=T)
      #.......................................................
      #................summiere die indikatoren-wichtigkeit.................................................
      
      K4=clone(K4.merk)
      symbols=lapply(rownames(K4), function(col) 
      {if(col=="Dates") return(list(symbol="Dates",0)); res=unlist(str_split(col,"\\."))[2]  ; mP(" %s -> %s",col,res); list(symbol=res,rowSums(K4[col,],na.rm=T))})
      #doofes write.table .. muss hier selber die columnnames basteln
      K4=rbind(Dates=c(colnames(K4)),K4)
      #.......................................................
      #kummuliere alle Wichtigkeit auf eines symbols - damit bekannt ist welche Faktor-Zeitreihen (unabhängig von leads oder sonstigen Vorverarbeitungen ) am wichtigsten sind.
      
      OAW=data.frame(rbindlist(symbols))
      sumsym=by(OAW[,2],OAW[,"symbol"],sum)
      symSum=sort(sumsym,decreasing=T)
      utils::write.table(symSum,file=sprintf("%sIndikatorWichtigkeit_%s.csv",opath,sym),dec=".",sep=";",col.names=F,row.names=T)
      #.......................................................
      
      if( bestN>0)
      {
        mP("make bestN %d train.set",bestN)
        ff=colMeans(forest.fit.wichtigkeit)   
        ff.best=head(ff[order(ff,decreasing=T)],bestN)
        target.fn=match.fun(Experi$Target.fn)
        Target= target.fn(data,sym)
        train.best=merge(Target,Experi$train.data[,names(ff.best)])
        utils::write.table(train.best,file=sprintf("%strainTop%d%s.csv",opath,bestN,sym),dec=".",sep=";",col.names=T,row.names=T)
        data$Train.best[[sym]]=train.best
        #data$Train.best[[1]]
        target.name=colnames(data$Train.best[[sym]])[1]
        #weitere  Classifier:  - welche die auch tree-plots machen
        
        if (is.null(data$crs))         data$crs= new.env() 
        crs=data$crs
        
        # train.data=merge(t.Bench,feature,feature2)
        crs=new.env()
        #MMNOW
        xx=classifier.randomForest.fit(crs,tail(data$train.data,4400),dimTarget=1,ntree=500)   #nach dem Aufruf ist crs bereit
        check.classifier(crs,crs$rf)
        #.................................
        
        #classifier.prepare(crs,tail(data$train.data,1000),dimTarget=1)  
        #descrCorr <- cor(crs$tdata)# die eingentlich - trainings-daten
        #highCorr <- findCorrelation(descrCorr, 0.90)
        ######
        
        report.name=sprintf("%sdataset%s",opath,sym)
        pdf(file = paste(report.name,'.pdf',sep=""), width=8.5, height=11)
        w=5
        for(i in seq(w+1,ncol(crs$dataset),w))       
        {
          i1=i-w
          mP("%d %d",i1,i)
          cdata=na.omit(crs$dataset[,i1:(i-1)])
          print(dim(cdata))
          # mchart(cdata)
          mPlots(cdata,title=paste(colnames(cdata),collapse="  "))
          #  xyplot(cdata,main=sprintf("%s %s",i1,i-1))    
        }
        dev.off()   
        sag("ok",warte=T)
        # browser()
        #-----------------  Einige andere Classifier --------------------------------
        #http://www.redbrick.dcu.ie/~noel/R_classification.html
        #Linear discriminant analysis uses a linear combination of the variables to map the samples to an n-dimensional space, in such a way that the ratio of between-group variance to within-group variance is maximised.
        
        #----------linear-discriminant analysis ---------------
        library("caret")
        #http://www.jstatsoft.org/v28/i05/paper 
        #Seite 5: finde zeitreihen die brutal hoch miteinander correliert sind und schlage einge zur l?schung vor:
        #browser(mP("andere classifier"))
        # xyplot(crs$tdata[1:4])
        prices = coredata(crs$tdata)  
        if (F)
        {
          cdata=na.omit(prices)
          descrCorr <- cor(crs$tdata)
          #descrCorr <- cor(cdata)
          highCorr <- findCorrelation(descrCorr, 0.90)
          colnames(crs$tdata[,highCorr])
          
          Cdata=xts(cdata[,highCorr],index(crs$tdata))
          dim(Cdata)
          correlogramm(Cdata,main="highCorr")
          purePlot(mNorm(crs$tdata[,highCorr]))
        }
        if (F)
        {
          library(MASS)
          if (F)
          {
            crs$lda <- lda(as.factor(crs$Target) ~ ., data=crs$tdata)
            pred<-  predict(crs$lda,type="class",newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))
            class.pred <- table(pred$class,crs$dataset[crs$test,crs$target])
            print(class.pred)
            pred.err=1-sum(diag(class.pred))/sum(class.pred)
            mP("lda.pred.err = %f",pred.err)
            #----------------------------------------------------------------
            
            #Quadratic Discriminant Analysis
            library(MASS)
            #myqda <- qda(Species ~ ., iris)
            #myqda.pred <- predict(myqda, iris)
            #table(myqda.pred$class,iris$Species)
            
            crs$qda <- qda(as.factor(crs$Target) ~ ., data=crs$tdata)
            print(crs$qda)
            print(summary(crs$qda))
            pred <-  predict(crs$qda,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))  
            print("qda")
            print(  table(Target[crs$test],pred) )
          }
          #.............................................................
          #Regularised Discriminant Analysis
          
          library(klaR)
          #myrda <- rda(Species ~ ., iris)
          #myrda.pred <- predict(myrda, iris)
          #table(myrda.pred$class,iris$Species)
          
          crs$rda <- rda(as.factor(crs$Target) ~ ., data=crs$tdata)
          print(crs$rda)
          print(summary(crs$rda))
          pred <-  predict(crs$rda,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))  
          print("rda")
          print(  table(Target[crs$test],pred) )
          
          #.............................................................
          #Support Vector Machines
          
          library(e1071)
          #mysvm <- svm(Species ~ ., iris)
          #mysvm.pred <- predict(mysvm, iris)
          #table(mysvm.pred,iris$Species)
          
          
          crs$svm <- svm(as.factor(crs$Target) ~ ., data=crs$tdata)
          print(crs$svm)
          print(summary(crs$svm))
          pred <-  predict(crs$svm,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))  
          pred.proba<-predict(crs$svm,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),probabilities=TRUE)
          
          print("svm")
          print(  table(Target[crs$test],pred) )
          
          #.............................................................
          #Neural Nets
          
          library(nnet)
          #mynnet <- nnet(Species ~ ., iris, size=1)
          #mynnet.pred <- predict(mynnet,iris,type="class")
          #table(mynnet.pred,iris$Species)  
          
          crs$nnet <- nnet(as.factor(crs$Target) ~ ., data=crs$tdata)
          print(crs$nnet)
          print(summary(crs$nnet))
          pred <-  predict(crs$nnet,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))  
          print("nnet")
          print(  table(Target[crs$test],pred) )
          
          #.............................................................
          #Naive Bayes
          
          library(klaR)
          #mynb <- NaiveBayes(Species ~ ., iris)
          #mynb.pred <- predict(mynb,iris)
          #table(mynb.pred$class,iris$Species)
          
          crs$nb <- NaiveBayes(as.factor(crs$Target) ~ ., data=crs$tdata)
          print(crs$nb)
          print(summary(crs$nb))
          pred <-  predict(crs$nb,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))  
          print("NaiveBayes nb")
          print(  table(Target[crs$test],pred) )
          
          #.............................................................
          
          
          library(mda)
          #mynb <- NaiveBayes(Species ~ ., iris)
          #mynb.pred <- predict(mynb,iris)
          #table(mynb.pred$class,iris$Species)
          
          crs$mda <- mda(as.factor(crs$Target) ~ ., data=crs$tdata)
          print(crs$mda)
          print(summary(crs$mda))
          pred <-  predict(crs$mda,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))  
          print("mda")
          print(  table(Target[crs$test],pred) )
          #.................................................................
          
          if (F)
          {
            crs$lda <- lda(as.factor(crs$Target) ~ ., data=crs$tdata[,-highCorr])
            pred<-  predict(crs$lda,type="class",newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))
            class.pred <- table(pred$class,crs$dataset[crs$test,crs$target])
            print(class.pred)
            pred.err=1-sum(diag(class.pred))/sum(class.pred)
            mP("reduced for highCorr - lda.pred.err = %f",pred.err)
          }
        }
        #plot(crs$lda)
        #  for (i in 1:length(crs$tdata[,1])) {print(names(crs$tdata)[sapply(crs$tdata[-i,],var)==0])}
        #mylda.pred <- predict(mylda, iris)
        #table(pred$class,iris$Species)
        #----------linear-discriminant analysis ---------------
        #-----------------  Einige Tree-Views --------------------------------
        #rf-check
        crs$rf=randomForest(as.factor(crs$Target) ~ .,
                            data=crs$tdata,
                            ntree=500,
                            mtry=4,
                            importance=TRUE,
                            na.action=na.roughfix,
                            replace=FALSE)  
        
        last(crs$rf$err.rate)
        
        pred <-  predict(crs$rf,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),type="class")  
        class.pred <- table(pred,crs$dataset[crs$test,crs$target])
        print(class.pred)
        pred.err=1-sum(diag(class.pred))/sum(class.pred)
        mP("pred.err = %f",pred.err)
        
        
        mP("rf.pred.err = %f",pred.err)
        
        check.classifier(crs,crs$rf)
        # Die relative Wichtigkeit der Variablen zeichnen
        report.name=sprintf("%srfWichtigkeit%s.pdf",opath,sym)
        
        pdf(file = paste(report.name,'.pdf',sep=""), width=8.5, height=11)
        
        varImpPlot(crs$rf, main="")
        title(main="Variablenwichtigkeit Random Forest TRAIN",
              sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
        dev.off()
        #  plot(crs$rf)
        sag("press key",warte=T)
        
        if (findMinimalFeatureSet)
        {
          ######## wie viele Variablen sollt man benutzen  methode a)
          fo=rfcv(crs$tdata["2012::"], as.factor(crs$Target),cv.fold=10,scale="log",step=0.9)
          best = which.max(fo$error.csv)  #nur die ersten best -parameter sollt man benutzen
          plot(fo$n.var, fo$error,type="h",main="importance")
          
          ######## wie viele Variablen sollt man benutzen  methode b)
          library(varSelRF)
          
          featureselect <<- varSelRF( 
            crs$tdata["2012::"],
            as.factor(crs$Target),
            itted.rf=crs$rf,
            mtryFactor = 1 , ntree = 500,ntreeIterat = 1000, 
            vars.drop.frac = 0.2,whole.range =FALSE, verbose = TRUE)
          
          featureselect
          plot(featureselect)
        }
        ############################################ 
        #...............................................................
        library(rpart)  
        #http://stackoverflow.com/questions/15044526/plotting-decision-trees-in-r-with-rpart?rq=1
        #      parms = list(split = "gini"))
        #     parms = list(split = "information"))
        
        crs$rpart <- rpart(as.factor(crs$Target) ~ .,
                           data=crs$tdata,
                           method="class",
                           parms=list(split="information"),
                           control=rpart.control(usesurrogate=0, 
                                                 maxsurrogate=0))
        
        # Eine Textansicht des Modells Entscheidungsstruktur erstellen
        
        #print(crs$rpart)
        rpart.tree=capture.output(print(crs$rpart))
        cat(rpart.tree,file=sprintf("%s/rpart_tree.txt",opath),sep="\n")
        #http://stackoverflow.com/questions/9666212/how-to-compute-error-rate-from-a-decision-tree?rq=1
        #crs$rpart$call$data
        
        printcp(crs$rpart)
        vimp=crs$rpart$variable.importance
        utils::write.table(vimp,file=sprintf("%srpart_wichtigkeit_%s.csv",opath,sym),dec=".",sep=";",col.names=F,row.names=T)
        plotcp(crs$rpart)
        #xmat=xpred.rpart(crs$rpart)
        #xerr = (xmat-crs$Target)
        #head(xmat)
        #text(crs$rpart,use.n=T)
        #par(xpd = TRUE)
        #plot(crs$rpart)
        #text(crs$rpart, pretty=1,use.n=T)
        #plot(freen.tr)
        #text(freen.tr, use.n = TRUE, all = TRUE)
        
        
        pred <-  predict(crs$rpart,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),type="class")  
        
        class.pred <- table(pred,crs$dataset[crs$test,crs$target])
        print(class.pred)
        pred.err=1-sum(diag(class.pred))/sum(class.pred)
        mP("rpart.pred.err = %f",pred.err)
        
        # We use the rpart.plot package.
        report.name=sprintf("%srpart_Tree_%s.pdf",opath,sym)
        pdf(file = paste(report.name,'.pdf',sep=""), width=8.5, height=11)
        fancyRpartPlot(crs$rpart, main="Entscheidungsstruktur trainBest.csv $ EXX50_RI.EXX50_RI_sumTarget")
        dev.off() 
        fancyRpartPlot(crs$rpart, main="Entscheidungsstruktur trainBest.csv $ EXX50_RI.EXX50_RI_sumTarget")
        #http://stats.stackexchange.com/questions/6478/how-to-measure-rank-variable-importance-when-using-cart-specifically-using
        
        sag("press key",warte=T)      
        #.....................................................................
        if (F)
        {
          library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
          
          crs$j48 <- J48(as.factor(crs$Target) ~ ., data=crs$tdata)
          print(crs$j48)
          print(summary(crs$j48))
          pred <-  predict(crs$j48,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))  
          print("j48")
          print(  table(Target[crs$test],pred) )
          
          report.name=sprintf("%sj48Tree_%s.pdf",opath,sym)
          pdf(file = paste(report.name,'.pdf',sep=""), width=280.5, height=11)  
          plot(crs$j48)
          
          #http://fibosworld.wordpress.com/2013/07/14/r-talks-to-weka-about-data-mining/
          library(partykit)
          plot(as.party(crs$j48)) # we use the partykit-package for nice plotting.
          
          dev.off()
          plot(crs$j48)
          sag("press key",warte=T)      
          j48.tree=capture.output(print(crs$j48))
          cat(j48.tree,file=sprintf("%s/j48_tree.txt",opath),sep="\n")
        }
        #................................................................
        library(party)
        Target=as.factor(coredata(crs$Target))
        Target=coredata(crs$Target)
        crs$ct <- ctree(Target ~ .,
                        data=crs$tdata,
                        controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5))
        
        pred <-  predict(crs$ct,newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))  
        # check predicted classes against original class labels
        print("party.ctree")
        print(
          table(Target[crs$test],pred)
        )
        #(sum(na.omit(sign(Target.t)==sign(pClassId))) )/ len(crs$train)
        
        report.name=sprintf("%scTree_%s.pdf",opath,sym)
        pdf(file = paste(report.name,'.pdf',sep=""), width=28.5, height=11)  
        plot(crs$ct)#, ip_args=list(pval=FALSE), ep_args=list(digits=0))
        dev.off()
        
        print(var.ctree(crs$ct))  
        ctree=capture.output(print(crs$ct))
        cat(ctree,file=sprintf("%s/ctree.txt",opath),sep="\n")
        out <- treeresponse(crs$ct)
        wnode <- where(crs$ct)
        n3 <- which(wnode == 13 & !duplicated(wnode))
        out[[n3]]
      }
      #  list(train.best=Train.best, signal=Signal)
    }
  }
  )   
}

var.ctree<-function(fit){
  
  require(gdata)
  
  a<-capture.output(print(fit))  
  a<-a[-c(1:7)]   
  a<-trim(a)   
  
  v<-character()   
  for(h in 1:length(a)){
    b<-a[h]    
    b<-gsub(") ","q",b)    
    b<-gsub(")","q",b)    
    b<-gsub(" < ","q",b)    
    b<-gsub(" > ","q",b)    
    b<-gsub(" <= ","q",b)    
    b<-gsub(" <= ","q",b)    
    b<-gsub("  weights = ","q",b)    
    v[h]<-unlist(strsplit(b,"q"))[2]    
  }    
  v<-factor(v,levels=names(fit@data@get("input")))    
  v<-v[is.na(v)==FALSE]    
  tabla<-table(v)>0    
  sol<-as.numeric(table(v)>0)    
  names(sol)<-names(tabla)    
  sol    
} 
########### wird so auch in MM_Main.R  mit universe = Eckhard4 geladen
#ein virtuelles data-env vorbereiten - damit das SIT-Framework laufen kann
printf<-function(...)
{
  print("bug")
}
mp<-function(...)
{
  mP(...)
}

make_eckhard4C_data<-function(visual=F,bench="EXX50_RI",xlsName="Data0E.xls",do.par=T, mkTrain=F,fundis=NULL,modelDir="EM4_DEC.1",fundamental.lag=fundamental.lag)
{
  #einlesen von xls -sheets in xts- variable
  #xx50.all= read.EckhardsXLS(modelDir=modelDir, xlsName=xlsName,startRow=5,belowColnames=5, date.format = "%d.%m.%Y",to.month.end=F,debug=F)  
  #leftOf(xlsName,".xls")
  exx50.all= read.EckhardsXLS(modelDir=modelDir, xlsName=xlsName,sheet.i=1,startRow=10,belowColnames=5, date.format = "%d.%m.%Y",to.month.end=F,debug=F)  
  
  if (is.null(fundis))
    fundis = FUNDIS[[xlsName]]
  dim(exx50.all)  #4392 Tage mit 39 Spalten
  #enh?lt folgende 3 logischen Gruppen
  exx50=exx50.all[,1]
  head(exx50)
  fundamentale.faktoren= exx50.all[,fundis]
  print("fundamentale.faktoren")
  print(colnames(fundamentale.faktoren))
  intermarket.faktoren = exx50.all[,c((last(fundis)+1):ncol(exx50.all))]
  print("intermarket.faktoren")
  print(colnames(intermarket.faktoren))
  
  layout(1)
  #  xyplot(fundamentale.faktoren)
  print("fundamentale.faktoren")
  print(colnames(fundamentale.faktoren))
  
  purePlot(mNorm(fundamentale.faktoren),main="fundamentale")
  mPlots(fundamentale.faktoren)
  
  colnames(exx50)
  print("intermarket.faktoren")
  colnames(intermarket.faktoren)
  #  xyplot(intermarket.faktoren)
  purePlot(mNorm(intermarket.faktoren),main="intermarket")
  head(intermarket.faktoren)
  mPlots(intermarket.faktoren)
  
  exx50 = (exx50)["1997::"]
  #Die Fundamentalfaktoren haben einen time lag von 20 Tagen, die Inter-Market Faktoren haben keinen time lag. 
  mP("  fundamental.lag:  %d ",  fundamental.lag)
  fundamentale.faktoren= lag(fundamentale.faktoren,  fundamental.lag)["1997::"] #hier schon mal gelagged weil die Teile ja einen Monat Lieferversp?tung haben
  #head(merge(fundamentale.faktoren[,1],fundamentale.faktoren.n[,1]),30)
  intermarket.faktoren = (intermarket.faktoren)["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  
  
  
  #mkTrain macht hier nur ein univariates Target 
  data <-  make.data(na.omit(merge(exx50,intermarket.faktoren )), mkTrain=F,visual=T,bench=bench)
  data$macros=fundamentale.faktoren
  data$fundamentale.faktoren <-colnames(fundamentale.faktoren)
  data$intermarket.faktoren <-colnames(intermarket.faktoren)
  
  data$betas=beta.again.portfolio(data$prices) #die monats-betas der data$prices gegen?ber ihrem gleichgewichteten portfolio
  mchart(data$betas)
  data$crs=new.env() #f?r den forest
  data$train.data = new.env()  #die dataCloud - gecashed
  #noch zusÃ¤tzlich den ifo dazu
  if  (F&& is.null(data$ifo))  #MIFO
    data$ifo =  try(load.IFO(visual=T))
  
  
  data$BENCH <-BENCH <-global_arg$BENCH <-bench
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission<<-0.0005 #0.0015   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(data$prices)) 
  global_objectId <<-paste("TREND","xx","signal.lm") 
  global_FastTrain <<-20 #hei?t:  f?r jede dim werden 2 Schritte ausprobiert.
  
  data$Y.names=colnames(data$prices[,(1:(min(fundis)-1))])  #die zu prognostizierende zeitreihe

   data$modelDir = modelDir
   data$xlsName = xlsName
  
  return(data)
}

#steuer die cloud-erstellung mit folgenden schaltern:

#cloud.control = spl("lagged")# spl("heavy,forecast,pca,OHLC,lagged,coarse.code")
#########################################################################################
#Dieser cloudGenerator (analog zu dem in indicators2.r) ist durchaus dax-spezifisch weil hier heftig auch vom Ifo-gebrauch #gemacht wird.  Ferner weden die signale gelagged und coarse.coded()  .. pca k?nnen bei bedarf #aktiviert werden.  Ebenso k?nnen forecast() eingebunden werden.
#es wird 1 Tag gelagged
#########################################################################################
tech.Cloud<-function(sym,arg,par,p,P=NULL,target,cloud.control=list())
{
  mP("dataCloud.e4")
  #browser(mP("###2"))  
  
  ret.p = mROC(Mnorm2(p[,sym]))
  normP = Mnorm2(p[,sym])
  .
  # colnames(forecasts)=c("forecasts")
  
  #if  (is.null(arg$dat$ifo))
  #  arg$dat$ifo =  try(load.IFO(visual=T))
  
  if (len(arg$dat$ifo)>0)
    IFO.m=lag(to.monthly(arg$dat$ifo)[,6])  #fundamentales Umfeld  -  h?ngt einen monat hinterher
  
  sd=runSD(ret.p,n=30)
  colnames(sd)=c("sd")
  
  mad=runMAD(p,n=30)
  colnames(mad)=c("mad")
  
  atr=ATR(HLC(to.monthly(p)))
  atr.d=(atr[,2]-atr[,1])
  colnames(atr.d)=c("atr.d")
  
  if (is.null(P) ||   ! listget("OHLC",cloud.control))
    hl=(Hi(to.monthly(p))-Cl(to.monthly(p)))/(Hi(to.monthly(p))-Lo(to.monthly(p)))
  else
    hl=(Hi(to.monthly(Hi(P)))-Cl(to.monthly(p)))/(Hi(to.monthly(Hi(P)))-Lo(to.monthly(Lo(P))))
  colnames(hl)=c("hl")
  
  
  if ( listget("heavy",cloud.control))    
  {
    mP("kelly")  
    kelly <- rollapplyr(ret.p, width=90, FUN="KellyRatio", by.column=T, align = "right")
    kelly[!is.finite(kelly)]<-0
    colnames(kelly)=c("kelly")
    
    mP("calmarRatio")  
    calmarRatio <- rollapplyr(ret.p, width=90, FUN="CalmarRatio", by.column=T, align = "right")
    calmarRatio[!is.finite(calmarRatio)]<-0
    colnames(calmarRatio)=c("calmarRatio")
    
    mP("sharpe")
    sharpe <- rollapplyr(ret.p, width=60, FUN="doSharpeRatio", by.column=T, align = "right")
    sharpe[!is.finite(sharpe)]<-0
    colnames(sharpe)=c("sharpe")
    
    mP("expectedShortfall")
    es <- rollapplyr(ret.p, width=60, FUN="ES", by.column=T, align = "right",p=.95, method="historical")
    es[!is.finite(es)]<-0
    colnames(es)=c("es")
  }
  else
  {
    dummy=p;dummy[]=0.93
    kelly=calmarRatio=sharpe=es=dummy
    colnames(kelly)=c("kelly")
    colnames(calmarRatio)=c("calmarRatio")
    colnames(sharpe)=c("sharpe")
    colnames(es)=c("es")
  }
  faber=p-SMA(p,200); colnames(faber)=c("faber")
  #~~~~~~~~~~~~~~~~~~~~ Training~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  cutTail=250   #die j?ngsten Daten taugen meist nicht zum Training
  mP("cutTail %d from target ! ",cutTail)
  #  browser()
  train.frame= fromToS(na.omit(target[-c((shape(target)-cutTail) : shape(target))]))
  mP("train.frame is %s",train.frame)
  
  #.......................................................................................
  mP("first merge and slopes")
  
  pSMA200=  p/SMA(p,200)
  pMax200=p-runMax(p,200)
  beta=arg$dat$betas[,sym]
  colnames(beta)=sprintf("beta_%s",sym);
  colnames(pMax200)="pMax200"
  colnames(pSMA200)="pSMA200"
  #browser()
  itpP=in.Trend.pos(p,visual=F,main="itp",k=160,K=2,probs=c(0.1,  .20,  0.80))
  colnames(itpP)="itpP"
  
  features=merge(
    faber, 
    pMax200, # 0.94
    pSMA200,
    #aroon(LoHi(P))/100 , #unwichtiger wie nach PNN noch gedacht
    atr.d,  #produziert das wichtige trueHigh und das unwichtige tr
    #BBands(HLC(P))[,4],  #pctB  wird nicht gebraucht
    #p-ZLEMA(na.omit(p),n=30)
    #DonchianChannel(HLC(P)[,-3],n=30),   
    sd,
    rollRegressionXTS(p,win=90),rollRegressionXTS(p,win=200),
    #runSum(sign(to.monthly(p)),7),
    hl,
    sharpe,
    es,
    kelly,
    calmarRatio,
    beta,
    itpP
  )
  
  
  #--------------------
  
  if (len(arg$dat$ifo)>0)
    features = merge(features, merge(IFO.m-SMA(IFO.m,7))/IFO.m, runSum(sign(IFO.m),3))
  
  #  features= bt.apply.matrix(features,ifna.prev)
  
  mP("RSI")
  rsi.m= merge(RSI(p,2),RSI(p,60))
  colnames(rsi.m) = spl("rsi2,rsi60")
  rsi.m[!is.finite(rsi.m)] <-0
  
  mP("SMA-runMax")
  
  sma.d=merge(
    SMA(p,200)-runMax(p,200), 
    SMA(p,90)-runMax(p,90), 
    SMA(p,60)-runMax(p,60) 
  )
  colnames(sma.d) = spl("sma.d200,sma.d90,sma.d60")
  
  #features = na.omit(features)#[fromToS(p)]
  features = merge(features,rsi.m, sma.d)
  #.......................................................................................
  if ( listget("forecast",cloud.control))    
    
  {
    fcs=forecast.m(p[,sym]) 
    features = merge(features,fcs)
  }
  #......................................................................................
  
  
  mP("ifna.prev")
  features= na.omit( bt.apply.matrix(features,ifna.prev))
  #  tail(features)
  # colnames(features)
  
  #.................................................................
  
  if ( listget("coarse.code",cloud.control))    
    
  {
    mP("coarse.code")
    mP("feature coarseCode %d",dim(features)[2])  
    
    features.coarse= bt.apply.matrix(na.omit(features),coarse.code,b=100,method="n")
    #plot( coarse.code(dax["1994::"],b=10,method="n"))
  }
  #------------------------------------------------------------------
  if ( listget("pca",cloud.control))    
  {
    mP("pca")
    #jetzt noch die pca-komponenten
    library("caret")
    
    #pca_features <- preProcess(features, method = "pca",na.remove=F)
    #pca_features$numComp   #so viele Variablen werden gebraucht !!!
    
    mP("pca")
    pca_features=prcomp(features, scale = TRUE,center=T,tol=0.15)$x   #biplot(pc)
    dim(pca_features)
    colnames(pca_features)
    
    # features = merge(features,pca_features)  #auch pca
    #features =pca_features   #nur pca
    #tail(features)
  }
  #----------------------------------------------------------------------------
  if ( listget("lagged",cloud.control))    
  {
    mP("lagged")
    #auch ge lagte features hinzunehmen
    features.lag = lag(features,5)
    colnames( features.lag ) =  sapply(colnames(features.lag),function(x){ sprintf("%s%s","LAG5_",x)})
    features.lag5=features.lag
    
    features.lag = lag(features,30)
    colnames( features.lag ) =  sapply(colnames(features.lag),function(x){ sprintf("%s%s","LAG30_",x)})
    features.lag20=features.lag
    
    features.lag = lag(features)
    colnames( features.lag ) =  sapply(colnames(features.lag),function(x){ sprintf("%s%s","LAG_",x)})
    
    features.lag.hot = HotLags2.CC(p, features)# lag(features,hotlag)
    colnames( features.lag.hot ) =  sapply(colnames(features.lag.hot),function(x){ sprintf("%s%s","LAGHOT_",x)})
    
    
    features = merge(features,features.lag,features.lag5,features.lag20) #lags benutzen
  }
  #-------------------------------------------------------------------------------
  #mP("define target")
  
  #target =target[,sym];colnames(target)=c("Target")
  #target=select.multiTarget.sum(arg$dat, sym)
  
  mP("rename features to %s",sym)
  #noch den Symbol-Namen -   vor den featureNamen schreiben - damit notfalls eine super-breit cloud gebildet werden kann
  newColnames= sapply(colnames(features), function(colname){sprintf("%s-%s",sym,colname)})
  colnames(features) = newColnames
  
  print(colnames(features))
  
  
  #in die colnames den sym-namen mit aufnehmen
  
  mP("build and lag train.data frame")
  #1 Tag lag sonst kristallkugel
  train.data<-data.frame(na.omit(  lag  (merge(target,features))))
  #  browser(mP("ok -----------------------------"))
  
  
  #gibt es is.infinite-Werte ?
  if (sum(apply(train.data,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
    no=foreach(col.i = 1:ncol(train.data)) %do%  { train.data[is.infinite(train.data[,col.i]),col.i]<-0 }
  
  return( train.data)
  ###################################################################################################
}

############################################################################################################################
############################################################################################################################



signal.randomForest.e4c <-function(arg, par = mlist( sma.w=c(200,120,350,10)),retrain.on="quarterly",visual=F,do.par=T,...)
{
  mP("signal.randomForest.e4c")
  #Vorausetzung in arg$dat$Target liegen pro price-Symbol TargetTrainingsdaten
  mP("##1")
  sym=colnames(arg$clos)
  p=mNorm(arg$clos)
  firstD=as.Date(index(p[1]))
  today=as.Date(last(index(p)))
  
  train.data = arg$dat$train.data[sprintf("%s::%s",firstD,today)]    
  target=na.omit(arg$dat$Target)[sprintf("%s::%s",firstD,today)]
  
  #b=na.omit(merge(target,p))
  #plotSigPrice(signal=(b[,1]),prices=b[,2])
  
  crs = arg$dat$crs   #das trainings-environment - siehe rattle  
  P=na.omit(arg$dat[[sym]])
  P=NULL  #eckhard liefert kein OHLC
  #berechne alle merkmale f?r den dax
  
  #........ baue die Data-cloud (einmal - chashe sie in train.data)
  if (is.null(train.data))
  {
    sag("ich erwarte in arg$dat$train.data die trainings-daten - cloud zum jetztigen symbol",warte=T)
    #  features = tech.Cloud(sym,arg,par,p,P,target)
    # arg$dat$train.data[[sym]] = features
  } 
  else
    features = na.omit(train.data)   #chashe
  
  mP("features ready %d %d",dim(features)[1],dim(features)[2])
  priceFeatures =merge(p,features)
  #gibt es is.infinite-Werte ?
  if (sum(apply(priceFeatures,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
    no=foreach(col.i = 1:ncol(priceFeatures)) %do%  { priceFeatures[is.infinite(priceFeatures[,col.i]),col.i]<-0 }
  
  #laufe ?ber die Daten,  trainiere monatlich, und gibt f?r jeden Tag die prognose zur?ck- als tupel aus (signal, signal.sicherheit)
  firstUsefulDate =DateS(first(na.omit(priceFeatures)))
  #browser(mP("....."))
  #  firstUsefulDate = "2002-01-01"  #vorher gings immer nur bergauf
  firstUsefulDateS = sprintf("%s::",firstUsefulDate) #?berspringe den anf?nglichen NA - Bereich der priceFeatures
  
  #  first( which(priceFeatures[firstUsefulDateS,"Target"] != 1))
  ################################################################################################
  mP("#a2")  
  
  sig.value = merge(p[firstUsefulDateS],p[firstUsefulDateS],p[firstUsefulDateS]);sig.value[]=NA
  #iteriere mit lapply  (parallelisierbar)
  
  #1000:shape(p[firstUsefulDateS])
  #es kann sein, dass es zu einzelnen kurstagen-keine price-features gibt
  #bauen ein P=p der nur dort Werte enh?lt an denen es auch priceFeatures (ab Col 2) gibt
  p=m.ifna.prev(p)
  priceFeatures = m.ifna.prev(priceFeatures)
  P= na.omit(merge(p[firstUsefulDateS],priceFeatures[firstUsefulDateS,2]))[,1]
  mP("roll for %d days",shape(P)-1000)
  
  if (shape(P)<1000)
  {    
    sag("Sorry - need at last 1001 data",warte=T)
    stop
  }
  
  if (do.par)  
    sig.value.list=sfLapply(1000:shape(P), function(p.i) roll.classifier.e4c(P ,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs,p.i=p.i)) 
  else
    sig.value.list=lapply(1000:shape(P), function(p.i) roll.classifier.e4c(P ,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs,p.i=p.i)) 
  
  
  mP("hol results")
  # browser()
  #ergebnisse aus er liste holen
  sig.value=foreach(i= sig.value.list,.combine="rbind") %do%
{   i  }

#  View(sig.value)
#alternativ: iterriere mit rollapplyr
if (F)
  sig.value <- rollapplyr(P, width=1000, FUN=roll.classifier.e4c, by.column=F,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs)
################################################################################################
mP("signal.randomForest.e4c  %s  fertig ####################### ",sym)


signal = sig.value[,1]
sig.confidence = sig.value[,2]
model.err = sig.value[,3]
res = list()
res[[sprintf("%s.sig",sym)]]=sig.value
wichtigkeit=na.omit(sig.value[,4:ncol(sig.value)])
res[[sprintf("%s.forest.fit.wichtigkeit",sym)]]=wichtigkeit#crs$forest.fit.wichtigkeit

target=target[fromToS(signal)]
bug=abs((target-signal))

if (visual)
{
  dim(na.omit(signal))
  dim(p)
  dim (bug)
  fromToS(p)
  fromToS(signal)
  
  #b=na.omit(merge(signal,p,bug))
  #signal =b[,1];p1=b[,2];bug=b[,3]
  
  plotSigPrice(signal=sign(signal-1), prices=p[fromToS(signal)], indi=list(sollIst=merge(signal,target,0)))
  mPlots(p[fromToS(target)],merge(target,0),abs(bug),title="target->signal")  
  
  
  #   plotSigPrice(signal=sign(target),prices=p1,indi=list(confi=sig.confidence,bug=merge(signal,bug)))  
}

return(list(Signal=signal[train.frame], Indi=list(conf=sig.confidence, bug=bug), res=res)) #ma=merge(features[,1],features[,2],p)[train.frame]
}
#...................................................................................

if (F)
{
  sym="DAX30"
  
  arg= list(dat =data, clos=data$prices[,sym],Target=data$Target )
  
  arg$dat$train.data=new.env() #l?scht bereits erstellte train.data
  
  par=list(sma.w=200)
  
  sig= signal.randomForest.e4(arg,par) 
}
#####################################################################################################################

#####################################################################################################################
#laufe t?glich ?ber die Daten,  trainiere monatlich, und gibt f?r jeden Tag die prognose zur?ck- als tupel aus (signal, signal.sicherheit)
#wir haben ABEND ..  die Close-Kurse liegen vor.
if (F)
{
  sym="DAX30"
  # roll.classifier.e4(data$prices["::2012-12-31",sym], data%train.data[[sym]],maxWin=2500, retrain.on="quarterly",crs=data$crs)  
}
#####################################################################################################################


roll.classifier.e4c<-function(p, allFeatures, maxWin=600, retrain.on="monthly",crs=NULL,p.i=0)
{
  mP("roll.classifier.e4c")
  
  #das bis maxWin wachsende Zeitfenster zum Tag:  lastDay
  sym=colnames(p);  if (is.null(sym))    sym=1
  pricesFeatures=na.omit(allFeatures)
  dim(pricesFeatures)
  if (p.i>0)
    p=p[max(p.i-maxWin,1):p.i]
  
  toDay = DateS(last(p))
  
  #das Ergebnis wird ein 3 dim  xts aus signal und signalsicherheit und model.err
  res=as.xts(data.frame(signal=0,confidence=0, model.err=0),index(last(p)))
  
  mP("...      roll.classifier.e4c     %s %d %s",sym,p.i,toDay)
  if (shape(na.omit(allFeatures[toDay,-1]))==0)
  {mP("SORRY - there are no cloud-features for today ");browser();return(res)}
  if (toDay=="")
  {mP("SORRY - there are no price-data for today ");return(res)}
  
  firstPriceDate.i=max(1,get.Index(pricesFeatures,DateS(last(p)))-maxWin)  #das fenster w?chst bis zu maxWin
  firstPriceDate = DateS(pricesFeatures[firstPriceDate.i])
  #der passende  merge(p,feature) - Datenausschnitt
  priceFeatures=na.omit(allFeatures[sprintf("%s::%s",firstPriceDate,toDay)])  #das mit jedem  schritt wachsenden Preis-fenster
  print(last(p))
  
  # if (dim(na.omit(priceFeatures))[1] < nrow(p))   #jetzt liegen wenigstens nrow(p) features vor
  #    return()
  #browser(mP("roll.classi"))
  #liegt ein (eher seltenes  retrain- Event vor ?) 
  firstCall=ifelse( len(ls(crs))==0,T,F)#wenn noch nie mit dem trainings-environment gearbeitet wurde, ist es leer, also muss auf jeden fall erst mal trainiert werden
  #++++++++ das aktuelle modell zu einer Prognose heranziehen
  
  prognose=NULL
  if (!firstCall)
    prognose = classifier.randomForest.predict(crs, priceFeatures[toDay,-1])
  mP("#A1")
  
  #+++++++++ muss das modell ge fitted werden ?
  forest.fit.wichtigkeit = "no"
  #if (toDay=="2002-12-31")
  #    browser(mP("##5"))
  #---------------------------------------------------------------------------------------------
  #browser(mP("retrain.event"))
  if (len(dont_use_last) <1 || dont_use_last < 1 ) #global
    dont_use_last <<- 1
  
  last.train.failed =  is.na(last(crs$forest.fit.wichtigkeit) )[1]
  if (retrain.event(toDay, retrain.on, p) || firstCall || last.train.failed)
  {
    mP("retrain.event !!! at %s ################## %s",toDay,retrain.on)
    #browser(mP("#K1"))
    #ich darf ja nicht einfach bis heut trainieren ... 
    forest.fit.wichtigkeit= classifier.randomForest.fit(crs, priceFeatures[1:max(1,nrow(priceFeatures)-dont_use_last),-1])   # trainiere - ohne explizit noch mal die Kurse zu ?bergeben
    #  mP("modell.err %f",forest.fit.wichtigkeit$model.err)
    #  browser()
    if (is.na(forest.fit.wichtigkeit))  #ein trainings-lauf ging daneben
    {
      mP("############# training  failed ############")
      #eine na-zeile einh?ngen
      na.f =last(crs$forest.fit.wichtigkeit);na.f[]=NA
      forest.fit.wichtigkeit=na.f
    }
    if (len(crs$forest.fit.wichtigkeit)==0)
      crs$forest.fit.wichtigkeit= forest.fit.wichtigkeit
    else
      crs$forest.fit.wichtigkeit = rbind(crs$forest.fit.wichtigkeit,forest.fit.wichtigkeit)
    mP("#A3")
  }
  #---------------------------------------------------------------------------------------------
  if (is.null(prognose))
    prognose = classifier.randomForest.predict(crs, priceFeatures[toDay,-1])
  mP("#a3")
  if (!is.xts(prognose))
  {
    mP("#bug at roll.classifier.e4c_ ")
    
  }
  print(prognose)
  #.................................................................................
  mP("#a4--------->")
  #cbind der variablenwichtigkeit an die prognose - aber setze na wenn nicht trainiert wurde
  if (forest.fit.wichtigkeit=="no")
  {
    forest.fit.wichtigkeit=crs$forest.fit.wichtigkeit[1,];
    forest.fit.wichtigkeit[]=NA
  }
  prognose=cbind(prognose,coredata(forest.fit.wichtigkeit))
  return(prognose)
  
  #  return(list(prognose=prognose, forest.fit.wichtigkeit= forest.fit.wichtigkeit ))
  
}
#roll.classifier.e4<-cmpfun(roll.classifier.e4_)

########################################################################################
#berechnen und l?ngenbeschneiden der werte
########################################################################################

micro.cloud<-function(euro.macros,visual=F,cloud.control=list())
{
  mP("micro.cloud ############################################# ")
  return(NULL)  ##wird gerad nicht gebraucht
  
  bench=global_arg$dat$BENCH
  #mNorm
  mNorm.mac= Mnorm2(euro.macros)  #preScale ist wichtig damit nicht das ROC-div0-Problem   auftaucht
  
  #bereinigen
  euro.macros.na= m.ifna.prev(mNorm.mac)
  
  if (sum(apply(euro.macros.na,2,FUN=function(col) len(col[is.na(col)]))) >0)
    no=foreach(col.i = 1:ncol(euro.macros.na)) %do%  { euro.macros.na[is.na(euro.macros.na[,col.i]),col.i]<-0 }
  
  
  #hot leads dazu geben
  hot.leads =  HotLags2.CC(p=Mnorm2(global_arg$dat$prices[,bench]),features =euro.macros.na)
  
  if (F) #coint und SVEC-prognose
  {  
    hl= HotLags2.CC(global_arg$dat$prices[,bench],data$prices)
    mp("hotlags bei intermarket faktoren und preisen: ")
    print(colnames(hl))
    mP("coint-matrix dazu -->>")
    kk=data.coint.matrix(merge(global_arg$dat$prices,hl))
    print(kk) 
    sag("warte",warte=T)
    block=merge(global_arg$dat$prices,hl)
    colnames(block)
    
    #1.  Find the number of lags using VARselect(y)
    #2.	Determine the cointegration rank using the function ca.jo. Pass the number of lags found in the first step as argument K.
    #3.	Fit VEC model using the cointegration vectors determined from the second step. This is performed by function cajorls, where you should pass the result of ca.jo and the number of cointegration vectors. 
    
    k=VARselect(ts(global_arg$dat$prices),lag.max=20,type="trend")
    k$selection[1]
    vecm <??? ca.jo ( ts(global_arg$dat$prices),K=k$selection[1])
    caj=cajorls(vecm)
    summary(caj)
    alpha <??? coef (caj$rlm ) [1 , ]
    beta <??? caj$beta
    resids <??? resid (caj$rlm )
    ls(caj$rlm)
    ls(caj$rlm$model)
    predict(caj$rlm,n.ahead=3)
    vecm.level <- vec2var(vecm, r = 3)
    pred=predict(vecm.level,n.ahead=30)
    ls(vecm.level)
    dim(vecm.level$datamat)
    dim(global_arg$dat$prices)
    purePlot(m.xts(coredata(vecm.level$datamat),index(first(global_arg$dat$prices))))
    purePlot(merge(m.xts(coredata(vecm.level$datamat),index(first(global_arg$dat$prices)))[,1], global_arg$dat$prices[,1]))
    
    pred$fcst
    AIC(vecm.level) #When comparing fitted objects, the smaller the AIC or BIC, the better the fit.
    plot(pred)
    matplot(pred,type="l")
    
    
    block.d=block[,spl("BALTIC_DRY,BALTIC_DRY.lead.19")]
    purePlot(block.d["2008::2009-02-01"])
    
    #block.d=merge(block[,"BALTIC_DRY"],lag(block[,"BALTIC_DRY"],1))
    purePlot(block.d["2008::2009-02-01"])
    colnames(block.d)=spl("y,x")
    fit=lm("y ~ x",data=data.frame(block.d))
    bb=merge(xts(fit$fitted,as.Date(names(fit$fitted.values))),block.d[,"y"])
    purePlot(bb["2008::2009-02-01"])
    
    #mchart(block.d)
    pred=(as.xts(predict(fit,interval = "prediction")))
    purePlot(pred["2008::2009-02-01"])
    #soll
    lines(block.d[,"y"],lwd=2,col="magenta")  #vorher wissen was geschieht
    
    signal=-sign(pred[,1]-block.d[,"y"])
    signal=-sign(block.d[,"x"]-block.d[,"y"])
    
    sb=na.omit(merge(signal,block.d[,"y"]))
    plotSigPrice(signal=sb[,1],prices=sb[,2])
    
    
    fit$fitted
    ls(fit)
    ls(fit$xlevels)
    
    
    plot(m.xts(fit$fitted,as.Date(names(fit$fitted))))
    head(as.Date(names(fit$fitted)))
    head(block)
  }
  
  if (ncol(hot.leads)>0)
    euro.macros.na=na.omit(merge(euro.macros.na,hot.leads))
  
  if (visual)purePlot(euro.macros.na)
  if (visual)cloud.check(euro.macros.na)
  
  #slope200
  slope200.mac=rollRegressionXTS(euro.macros.na,win=200)*100
  slope200.mac = bt.apply.matrix(slope200.mac, cutInterval, mi=-30,ma=30)
  #slope200.mac=col.rename(slope200.mac,"slope200")
  
  #slope300
  slope300.mac=rollRegressionXTS(euro.macros.na,win=300)*100
  slope300.mac = bt.apply.matrix(slope300.mac, cutInterval, mi=-30,ma=30)
  
  
  if (visual)cloud.check(slope200.mac)
  if (visual)purePlot(slope200.mac)
  
  cloud.mac= na.omit(merge( mNorm.mac,  slope200.mac,slope300.mac))
  
  #gibt es is.infinite-Werte ?
  if (sum(apply(cloud.mac,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
    no=foreach(col.i = 1:ncol(cloud.mac)) %do%  { cloud.mac[is.infinite(cloud.mac[,col.i]),col.i]<-0 }
  
  #MMCLOUDs
  return(cloud.mac) 
  #..............................................................................  
}

###### die wird heftig benutzt ####

small.cloud<-function(euro.macros,visual=F,cloud.control=list(),parTable=NULL)
{
  global_ParTable=parTable #falls für die signal-ranks besondere parameterset benötigt werden
  #slope200, slope90, faber, roc60
  mP("small.cloud ###############################################")
  
  bench=global_arg$dat$Y.names
  if(len(global_arg$dat$prices[,bench])==0)
    bench = global_arg$dat$BENCH
  if(len(global_arg$dat$prices[,bench])==0)
    bench = colnames(global_arg$dat$prices[,1])
  
  #stopifnot(len(global_arg$dat$prices[,bench])>0)
  #.............................................
  #mNorm
  mNorm.mac= Mnorm2(euro.macros) *10  #preScale ist wichtig damit nicht das ROC-div0-Problem auftaucht
  #bereinigen
  euro.macros.na= m.ifna.prev(mNorm.mac)
  if (sum(apply(euro.macros.na,2,FUN=function(col) len(col[is.na(col)]))) >0)
    no=foreach(col.i = 1:ncol(euro.macros.na)) %do%  { euro.macros.na[is.na(euro.macros.na[,col.i]),col.i]<-0 }
  #.............................................
  #hot leads dazu geben
  hot.leads =  HotLags2.CC(p=Mnorm2(global_arg$dat$prices[,bench]),features =euro.macros.na)
  print("###########  hot.leads #############")
  print(colnames(hot.leads))
  if (ncol(hot.leads)>0)
    euro.macros.na=na.omit(merge(euro.macros.na,hot.leads))
  
  if (visual)purePlot(euro.macros.na)
  #mPlot(euro.macros.na)
  if (visual)cloud.check(euro.macros.na)
  #....... #####################  INDIKATOREN rechnen ################
  
  #faber
  faber.mac =bt.apply.matrix(euro.macros.na,faber)
  faber.mac = bt.apply.matrix(faber.mac, cutInterval, mi=-30,ma=30)
  faber.mac=col.rename(faber.mac,"faber")
  cloud.mac=  faber.mac
  
  if (visual)cloud.check(faber.mac)
  if (visual)purePlot(faber.mac)
  
  #http://home.ubalt.edu/ntsbarsh/stat-data/Forecast.htm#rhowhpfil
  
  #.  slope/MA,
  #.  log (slope),
  #.  log(slope/MA),
  #.  log(slope) - 2 log(MA).
  
  #lfaber.mac  log slope ma
  #........................................
  
  RankFaber<-function(p)
  {
    rankFaber=runPercentRank(p-EMA(p,260), n = 260, cumulative = FALSE, exact.multiplier = 0.5)  
  }
  rankFaber=bt.apply.matrix(euro.macros.na, RankFaber)
  rankFaber=col.rename(rankFaber,"rankFaber")
  #mPlots(rankFaber)
  cloud.mac=  merge(cloud.mac, faber.mac)
  #................................................
  print("itp")
  itp=foreach(pi=1:ncol(euro.macros.na),.combine="merge") %do%
    rank.Generic("signal.Price.itp",prices=euro.macros.na[,pi])
  itp=col.rename(itp,"itp")
  itp=clean.matrix(itp)
  cloud.mac=  merge(cloud.mac, itp)
  
  #...............................................
  print("ma3")
  ma3=foreach(pi=1:ncol(euro.macros.na),.combine="merge") %do%
    rank.Generic("signal.MA.3_",prices=euro.macros.na[,pi])
  ma3=col.rename(ma3,"ma3")
  cloud.mac=  merge(cloud.mac, ma3)
  #..................................................
  print("dd1")
  dd1=foreach(pi=1:ncol(euro.macros.na),.combine="merge") %do%
    rank.Generic("signal.drawDown1",prices=euro.macros.na[,pi],par=list(win=200))
  colnames(dd1)=colnames(euro.macros.na);dd1=col.rename(dd1,"dd1")
  cloud.mac=  merge(cloud.mac, dd1)
  #..................................................
  if(F)
  {
    print("rwf")
    rwf=foreach(pi=1:ncol(euro.macros.na),.combine="merge") %do%
      rank.Generic("signal.rwf",prices=euro.macros.na[,pi],par=list(win=200))
    colnames(rwf)=colnames(euro.macros.na); rwf=col.rename(rwf,"rwf")
    cloud.mac=  merge(cloud.mac, rwf)
  }
  #..................................................
  print("ddma200")
  ddma200=ddma(euro.macros.na,n=200)
  ddma200=col.rename(ddma200,"ddma200")
  cloud.mac=  merge(cloud.mac, ddma200)
  #..................................................
  print("ddma300")
  ddma300=ddma(euro.macros.na,n=300)
  ddma300=col.rename(ddma300,"ddma300")
  cloud.mac=  merge(cloud.mac, ddma300)
  #..................................................
  
  print("probMom")
  if (T)
  {
    probMom= foreach(pi=1:ncol(euro.macros.na),.combine="merge") %do%
{
  r1=merge(rank.probMom(euro.macros.na[,pi],xtra=60) ,rank.probMom(euro.macros.na[,bench],xtra=60))
  r1[,1]-r1[,2]
}
  }
probMom=col.rename(probMom,"probMom")
cloud.mac=  merge(cloud.mac, probMom)

#.......................................................................................

#slope200
#slope200.mac=rollRegressionXTS(euro.macros.na,win=200)*100
slope200.mac=  get.rank("rank.slope200", euro.macros.na)*100
slope200.mac = bt.apply.matrix(slope200.mac, cutInterval, mi=-30,ma=30)
#slope200.mac=col.rename(slope200.mac,"slope200")
cloud.mac=  merge(cloud.mac, slope200.mac)
#...........................................................................

if (visual)cloud.check(slope200.mac)
if (visual)purePlot(slope200.mac)
#...............................................................................
#slope90
#slope300.mac=rollRegressionXTS(euro.macros.na,win=300)*100
slope300.mac=  get.rank("rank.slope300", euro.macros.na)*100
slope300.mac = bt.apply.matrix(slope300.mac, cutInterval, mi=-30,ma=30)
#slope200.mac=col.rename(slope200.mac,"slope200")
cloud.mac=  merge(cloud.mac, slope300.mac)
if (visual)cloud.check(slope300.mac)
if (visual)purePlot(slope300.mac)
#...........................................................................


#roc 30

if (F)  
{
  roc60M.mac=ROC(euro.macros.na+10000,120)
  roc60M.mac=bt.apply.matrix(roc60M.mac, cutInterval, mi=-50,ma=50)
  roc60M.mac=col.rename(roc60M.mac,"roc60")
  
  if (visual)cloud.check(roc60M.mac)
  if (visual)purePlot(roc60M.mac)
} 
if (F)
{
  #roc 5
  roc5M.mac = ROC(euro.macros.na+10000,5)
  roc5M.mac=col.rename(roc5M.mac,"roc5")
  if (visual)purePlot(roc5M.mac)
  roc5M.mac = bt.apply.matrix(roc5M.mac, cutInterval, mi=-20,ma=20)
  if (visual)cloud.check(roc5M.mac)
  if (visual)purePlot(roc5M.mac)
}    
#itp
if (visual)cloud.check(itp)

#....................................................................


#cloud.mac= (merge( mNorm.mac, faber.mac,  rankFaber, slope200.mac,slope90.mac, roc30M.mac,roc5M.mac, itp))
#cloud.mac= (merge(  faber.mac,  rankFaber, slope200.mac,slope300.mac, roc60M.mac,itp))
#cloud.mac= (merge(  faber.mac,  rankFaber, slope200.mac,slope300.mac, ma3,dd1,ddma200,rwf,probMom,itp))

cloud.check(cloud.mac)
#cloud.mac= na.omit(merge( mNorm.mac, faber.mac,  rankFaber, slope200.mac,slope90.mac, roc30M.mac,roc5M.mac, itp))

if (visual)
{
  purePlot(cloud.mac)  
  dim(cloud.mac)
  dim(euro.macros.na)
}
#gibt es is.infinite-Werte ?
if (sum(apply(cloud.mac,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
  no=foreach(col.i = 1:ncol(cloud.mac)) %do%  { cloud.mac[is.infinite(cloud.mac[,col.i]),col.i]<-0 }

#MMCLOUDs
return(cloud.mac) #ich hab mit kursen gearbeitet die ich heute morgen, von gestern abend, erhalten hab.
#...............................................................................  


if (F) ##... coarse.code ??
{
  CC.roc30M.mac=  bt.apply.matrix(roc30M.mac,function(x) coarse.code(x,method = "n", b=100,visual=F) ) #univariat, 
  purePlot(CC.roc30M.mac)  
  
  roc1M.mac = ROC(euro.macros.na,1)
  purePlot(roc1M.mac)
  
  CC.roc1M.mac=  bt.apply.matrix(roc1M.mac,function(x) coarse.code(x,method = "n", b=100,visual=F) ) #univariat, 
  purePlot(CC.roc1M.mac)  
  
  y = bt.apply.matrix(roc1M.mac, cutInterval, mi=-15,ma=15)
  purePlot(y)
}

}

###########################################################################
#bereite in  data$tech.data[[sym]] eine breite tech-cloud vor
#das sind besonders viele technische Indikatoren f?r die zu prognostizierende Zeitreihe
#cloud is ready at data$tech.data[[sym]]
############################################################################
tech.cloud.builder<-function(symbolnames,tech.Cloud.Fn= "tech.Cloud", data,visual=F,cloud.control=list(),do.par=F)
{
  #fÃ¼r jedes sym die dataCloud.e4 berechnen (ca. 70 technisches features auf tagesbasis)
  if (F) 
  {
    symbolnames= data$symbolnames
    symbolnames = data$BENCH
    tech.Cloud.Fn= "tech.Cloud"
    visual=F
    do.par=T
    #control-parameter zu fein-spec der technischen  dataCloud.e4
    cloud.control <<- spl("heavy,lagged,forecast")# spl("heavy,forecast,pca,OHLC,lagged,coarse.code")
  }
  
  tech.Cloud.fn = match.fun(tech.Cloud.Fn)
  if (do.par)
  {
    sfExport("data")
    sfExport("cloud.control")
    
    all.features=sfLapply(symbolnames,function(sym)
    {
      Target= select.multiTarget.sum(data, sym)
      arg= list(dat =data, clos=data$prices[,sym],Target=Target)
      p=data$prices[,sym]
      P=data[[sym]]
      
      features=tech.Cloud.fn(sym,arg,par,p,P=NULL,Target,cloud.control)
      
      tech.cloud=features
      tech.cloud=as.xts(tech.cloud, orderby=as.Date(rownames(tech.cloud)))
      tech.cloud=m.ifna.prev(tech.cloud)
      try(  write.xts(tech.cloud,filename= sprintf("techCloud_%s.csv",sym)))
      
    })
  }
  else
    all.features=lapply(symbolnames,function(sym)  #sfLapply(symbolnames,function(sym)
    {
      #sym="EXX50_RI"
      mP("dataCloud.e4 for %s",sym)
      Target= select.multiTarget.sum(data, sym)
      arg= list(dat =data, clos=data$prices[,sym],Target=Target)
      par=list(sma.w=200)
      p=data$prices[,sym]
      P=data[[sym]]
      
      features=tech.Cloud.fn(sym,arg,par,p,P=NULL,Target,cloud.control)
      
      tech.cloud=features
      tech.cloud=as.xts(tech.cloud, orderby=as.Date(rownames(tech.cloud)))
      tech.cloud=m.ifna.prev(tech.cloud)
      try(  write.xts(tech.cloud,filename= sprintf("techCloud_%s.csv",sym)))
      
    })
  
  mp("ready with tech.cloud -features for %d symbols",len(all.features))  #muss der Anzahl der symbole entsprechen
  
  #propagiere die Ergebnisse nach data$train.data[[sym]]
  #und misch die cloud.mac hinzu
  no=lapply(1:len(all.features),function(sym.i)
  {
    sym=symbolnames[sym.i]
    tech.cloud = data.frame(all.features[sym.i])
    
    #................. die macro-cloud beimischen  und mit m.ifna.prev die monatsdaten auff?llen       
    tech.cloud=as.xts(tech.cloud, orderby=as.Date(rownames(tech.cloud)))
    tech.cloud=m.ifna.prev(tech.cloud)
    try(  write.xts(tech.cloud,filename= sprintf("techCloud_%s.csv",sym)))
    data$tech.cloud[[sym]] =data.frame(tech.cloud)
  })
  #..................................................................................................<< 
  all.features=NULL #speicher freigeben
  #check
  print(data$symbolnames)
  print(ls(data$tech.cloud))
  
  print( dim(data$tech.data[[symbolnames[1]]]) ) #   ->  3821 Tage * 81 Spalen
  #  View(data$train.data[["DAX30"]])
  print( colnames(data$tech.data[[symbolnames[1]]]))
  mP("train.data cloud is ready at data$tech.data[[sym]]")
  #................................................................................................
}
#########################################################################################
#sigtech
if (F)  #berechne und bewerte technische modelle auf den Y.names aller xls-files
{
#data=read.many.xls.all(modelDir="EM4_MAERZ_C",load.data=T)


#eckhards y-daten einlesen (aus allen xls immer nur das y Target)
data=data=read.many.xls.y(modelDir="EM4_MAERZ_C",universe = "EM5C_Y",bench="SUP500",safe="REX_RI")
#........................ SHORT hinzu
pb=(data$prices[,data$BENCH])
short=-pb+2*nval(pb[1]) +2000
#short=pb
colnames(short)=sprintf("%sSHORT",data$BENCH)
mchart(merge(pb,short))
data.add(data,colnames(short),short)
data$SHORT = colnames(short)
#.....................
ls(data)
define.Globals(dataSet="EM5_MAERZ_C")
#def.Globals()
x=indi.Generic("signal.Faber.i2", global_arg, par=list(sma.w=300),visual=F, TRAINSYM =-1,fastMode=T)

ls(x)
colnames(x$Signal)

x=indi.Generic("signal.Faber.i2", global_arg, par=list(sma.w=300),visual=T, pdf=T,TRAINSYM =-1)
x=indi.Generic("signal.spline", global_arg, par=list(gl=-0.4),visual=T, TRAINSYM =-1)


#keine portfolio-modelle rechnen
x=indi.Generic("signal.Faber.i2", global_arg, par=list(sma.w=300),visual=F, TRAINSYM =-1,fastMode=T)
#............... lass alle signale laufen
models=list()
no_strategy.Performance.snapshoot <<- T  #ich brauche keine Portfolio-guv-charts
#bei pdf=T mach er zusätzlich auch die SA- modelle
models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="test",LongShort="F",pdf=T,visual=F,fastMode=F) #nur bei pdf=T werden auch die SA- und TSA- Modelle gerechnet
models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="test2",LongShort="F",pdf=F,visual=F,fastMode=T) #nur bei pdf=T werden auch die SA- und TSA- Modelle gerechnet
#........ #hier wird nur die Liste der zu berechnenen den  Signale aus einer 
#..  dem Signals.xls  gelesen
models = Run_all_signal(data=data,spl("signal.Faber.i2,signal.levy"),SAFE=SAFE,do.assemble.Signals=F,file="test2",LongShort="F",pdf=F,visual=F,fastMode=T) #........ #hier wird nur die Liste der zu berechnenen den  Signale aus einer 
#..  dem Signals.xls  gelesen

models = Run_all_signal(data=data,signal.Set="SSET2",SAFE=SAFE,do.assemble.Signals=F,file="test2",LongShort="F",pdf=F,visual=F,fastMode=T) #nur bei pdf=T 

no_strategy.Performance.snapshoot <<- F  #ich brauche  Portfolio-guv-charts
#............. lade die Timing-Modelle signale () xToSafe...)
models= load.models("EM4_MAERZ_C","AllSig")

best=find.best.portfolio.model(models, period ="years",visual=T,rolling.mode=F,rank.for="mcalmar") #"",cagr, sharpe
#analysiere auch die symbol-modelle
symbest=signal.model.analysis(models,Frame ="::",period="years")
#

strategy.Performance.snapshoot(models,state="T",title=sprintf("best"),data=data,commission=global_commission)#,redirectedOut="experiment")    
#.........#MMNOW
models.all= load.models("EM4_MAERZ_C","test")
best=find.best.portfolio.model(models.all, period ="years",visual=T,rolling.mode=F,rank.for="mcalmar") #"",cagr, sharpe
#.... nun rolling
best=find.best.portfolio.model(models.all, period ="years",visual=T,rolling.mode=T,rank.for="mcalmar") #"",cagr, 

Best = list("Best"=models.all[[best$modelName[1]]],"2dBest"=models.all[[best$modelName[2]]],"3dBest"=models.all[[best$modelName[3]]],"worst"=models.all[[last(best$modelName)]])
Best=models.all[[best$modelName[1]]]

ls(Best)
#modell vers markt
purePlot(mNorm(merge(Best$equity,data$prices[,data$BENCH],data$prices[,data$SAFE])))
purePlot(mNorm(merge(Best$equity,data$prices)))

strategy.Performance.snapshoot(Best,state="T",title=sprintf("Best: \n%s",best$modelName[1]),data=data,commission=global_commission)#,redirectedOut="experiment")    

#load("Models/HuAFeb_4b/Results.xls/hua_World_signal.MA.3rank.slope300f.data")
#m=dels2flatList(models,datafile="HuAWorld")


best.values = lapply(names(best), function(slot) { val = best[[slot]][1]; mP("%s ---: ",slot);print(val)})

#out$timing=tryM( strategy.Performance.snapshoot(models,R.arg,state="T",title=sprintf("just Timing(%s) + BuyHold + BENCH, %s",signal.fun,period),data=mdata,commission=commission$percentage,redirectedOut=experiment)    )


}


##########################################################################################
#TODO
# repariere tech.cloud bauer    ok?
# integriere genSA-PortfolioOptimierung
# erweitere info  um train.data-colnames  - ok
# sit-TSA: taugt die confidence als selektions-kriterium ? - taugt signal als timing ?
#
#signal.labor - als big.data.cloud hinzunehmen -  timing-filter ??
#
print("########### load + source EM5C.R")
###############################################################################
if (F)
  list_R_functions('MLib/EM5C.R')


#sfSource("MLib/EM4.R")
purl.examples <- function(fileName){
  ll <- readLines(fileName)
  ex.lines <- grep('@examples',ll)   ## get the example's lines
  ## for each example loop till
  ## there is no comment (inefficient)
  examples  <- lapply(ex.lines , function(x){
    i <- x+1
    code <- list()
    while(grepl("#'",ll[i])){
      l <- c(code,gsub("#'","",ll[i],fixed=TRUE))
      i <- i+1      
    }
    code
  })
}


if (F)
{
  list_R_functions('MLib/EM5C.r')
  lapply(purl.examples('MLib/EM5C.r'), 
         function(ex) eval(parse(text=ex))) ## safer to use evaluate package here
  
}
