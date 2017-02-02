########################################################################################
#DataMining.r
#
#prepare_for_regression_and_calculate_targets_for_signal_and_ranking()
#  macht alles was es braucht um MultiVar-Regressionsmodelle (sieh multiSignal.r) rechen zu 
#können - also:  um Eckhards-MultiVar-Daten ein lesen, Targets zu rechnen ...
#...................
#prepare_for_classification_and_calculate_targets_for_signal_and_ranking()
#
#rechnet dann auch noch einen in Signal.xls spezifizierten Set von  TSA-signal-Systemen
# nimmt von denen die signale und ranks und bildet dann in  models.i  und models.f (fundamental) den enstspechenden Datenfundus um aus signalen und ranks DatenClouds zu bilden
#...............................................................................
#die dazu nötige flexible abfrage-sprache um beliebige clouds bilden zu können:
# 
# multi.model.cloud<-function(data=data,sym=data$BENCH,models,mod.names="*",longOnly=T,mode
#....................
#
#nach prepare_for_classi...
#können dann auch randomForest-Signale wie signal.classify.randomForest() gerechnet werden

#die aufwändigen Parametrisierungen der multiSignal-Signale werden in cockpit.xls- Dateien
#gespeichert und können aus diesen auch wieder eingelesen werden

########################################################################################



options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
options(error = browser)

##############################################################################################
#Aufgaben:
# lade Rohdaten  - z.b in  Eckhards - Format
# berechne- gesteuert über Signals.xls - eine dataCloud mit 2 Targets:  Signal + Ranking
# training/test- run in verschiedenen zeitl. auflösungen

#Marken:
if (F)
  data = prepare_for_classification_and_calculate_targets_for_signal_and_ranking(modelDir="xx", sym="sup500")

##############################################################################################

prepare_for_regression_and_calculate_targets_for_signal_and_ranking<-function(modelDir="xx",sym="SUP500",Ctrl=NULL)   # main: berechne und bewerte technische modelle auf den Y.names aller xls-files
{
  source("mlib/em5c.r")
  
  if (is.null(Ctrl))
  {
    ctrlFile =sprintf("Models/%s/prepMining_%s.xls",modelDir,sym)
    if (F && file.exists(ctrlFile) && global_arg$useXlsPar)
    {
      Ctrl=readList(xlsName=ctrlFile,table="prepMining.cockpit")
      Ctrl$modelDir=modelDir
      sag("############ use file based cockpit ################ ",warte=F)  
    }
    else
    {
      Ctrl = list()
      Ctrl$modelDir=modelDir
      Ctrl$sym=sym
      Ctrl$xlsName= sprintf("%s.xls",sym)
      Ctrl$multiTarget = F
      #...............  Ranking-Target .................................
      Ctrl$UNIVERSE = "EM5C_SUP500"
      Ctrl$modelDir <- modelDir
      Ctrl$Y.sym=sym
      Ctrl$BENCH="SUP500"
      Ctrl$SAFE="REX_RI"
      Ctrl$SHORT="SUP500SHORT"
      Ctrl$signal.Set = "SSET2"
      Ctrl$LongShort = "F"
      Ctrl$file ="test2"
      Ctrl$pdf.i = F
      Ctrl$pdf.f = F
      Ctrl$TRAINSYM.i = -1  #NO wenn gar nicht
      Ctrl$TRAINSYM.f = -1  
      
      writeList(list=Ctrl,listname="prepMiningReg.cockpit",ctrlFile,create=T) #protokolliere den run..
    }
  }
  #----------cockpit ------------------> #übernimm die ctrl-parameter
  SAFE=Ctrl$SAFE
  BENCH=Ctrl$BENCH
  UNIVERSE=Ctrl$UNIVERSE
  modelDir<-  Ctrl$modelDir
  modelDir<<-  Ctrl$modelDir
  xlsName <-Ctrl$xlsName
  #..........................................
  #um ein ranking - target zu bauen brauch ich alle Y.names mit denen er sich vergleicht.
  #ich verlange:  hier muss SAFE und BENCH vorkommen !!
  
  sag(sprintf("read data from modelDir/Data %s",modelDir))
  
  data=data=read.many.xls.y(modelDir=sprintf("%s/Data",modelDir),universe = UNIVERSE,bench=BENCH,safe=SAFE)
  #........................ SHORT hinzu konstruieren
  pb=(data$prices[,data$BENCH])
  short=-pb+2*nval(pb[1]) +2000
  #short=pb
  colnames(short)=sprintf("%sSHORT",data$BENCH)
  purePlot(merge(pb,short))
  data.add(data,colnames(short),short)
  data$SHORT = SHORT= colnames(short)
  #.........Schöne Normierung auf den ganzen Dynamik-Range einer jeden Zeitreihe..
  data$prices= mNorm( bt.apply.matrix(data$prices, function(p) scaleTo(p,c(100,200))))
  purePlot(data$prices)
  
  ## das ranking-target (wer unter den topx titeln - ntop-mcalmar- in den nächsen x monaten ist)  - jetzt machen- weil jetzt ALLE y.Names vorliegen
  xmonths = 6;topn=ncol(data$prices)/2; topk=topn+topn/2 ; m.period="months";visual=F
  ranking.target=target.rank(data$prices,xmonths=6, topn=ncol(data$prices)/3, topk=topk, m.period=m.period,visua=F)
  mdata <-clone(data)  #mdata - als vorratsspeicher
  #.......................................................................
  
  #### #####  jetzt lade ich aber erst mal nur ein Y.sym: ########
  
  visual=T
  data <-make_eckhard4C_data(visual=F,xlsName=xlsName,bench=BENCH,do.par=F,mkTrain=T,fundis=NULL,modelDir=sprintf("%s/Data",modelDir),fundamental.lag=20)
  ### SAVE,BENCH,SHORT hinzufügen
  Y.sym= colnames(data$prices)[1]
  
  if (!SAFE %in% colnames(data$prices))
    data.add(data,SAFE,mdata[[SAFE]])
  if (!BENCH %in% colnames(data$prices))
    data.add(data,BENCH,mdata[[BENCH]])
  if (!SHORT %in% colnames(data$prices))
    data.add(data,SHORT,mdata[[SHORT]])
  
  
  #---schrott-entfernen
  # global_disable_cloud.check = F;cloud.check(data$prices)
  #data.info(data) 
  
  #data.rm(data,"ADVANCEDECLINE")
  
  #.........Schöne Normierung auf den ganzen Dynamik-Range einer jeden Zeitreihe..
  #data$prices= mNorm( bt.apply.matrix(data$prices, function(p) scaleTo(p,c(100,200))))
  data$prices = (m.scaleTo(data$prices,c(1,2)))
  
  allPrices<<- clone(data$prices)  #merken
  
  define.Globals(dataSet = modelDir,bench=BENCH,SAFE=SAFE,short=SHORT)
  data$symbolnames = data$Y.names
  data$ranking.Target = ranking.target  
  
  #View(data.frame(ls(data))) ;dim(data$prices)
  #.............. Signal-Target ........................................
  if (Ctrl$multiTarget)
    data$multiTarget=select.multiTarget.sum(data, BENCH)
  else
    
    data$multiTarget <-list( target_1=compute.Target.by.method(data=data, TargetMethod=-1, w=30 , minReturn=0.15, min.duration=60,glaettung=5,visual=visual))
  
  #mt=select.multiTarget.sum(data, data$BENCH)
  
  #data$prices =allPrices#[,x.names]
  
  #alternativ .. rechne gleich mal 5 signal-targets auf Vorrat
  #  data$multiTarget <-multiTarget(visual);  data$Target=select.multiTarget(data,Y.sym)
  define.Globals()
  global_StartDate <<- as.Date(index(first(na.omit(data$prices))))
  global_FastTrain<<-5  
  global_ParTable <<-NULL
  
  norm_Win(2)
  purePlot(data$prices,main="prices")
  print(colnames(data$prices))
  print(data$fundamentale.faktoren)
  #data$macros = m.ifna.prev(data$macros)
  
  #  m= mNorm( bt.apply.matrix(data$macros, function(p) {print(colnames(p));scaleTo(p,c(100,200))  }))
  data$macros = (m.scaleTo(data$macros,c(1,2)))
  #global_disable_cloud.check<<- F ; cloud.check(data$macros)
  purePlot(data$macros,main="macros")
  View(data$prices)
  data$symbolnames=colnames(data$prices)
  
  universe= showTable(Y= Y.sym,prices=colnames(data$prices), BENCH=BENCH,SAFE=SAFE,SHORT=SHORT, macros= colnames(data$macros),Target=c("multiTarget","ranking.Target"),
                      fundamentale.faktoren=data$fundamentale.faktoren,intermarket.faktoren=data$intermarket.faktoren )
  writeTable(universe,xlsName=sprintf("Models/%s/universe.xls",modelDir),table.name="universe",create=T)
  
  define.Globals(mdata=data,modelDir =modelDir,bench=BENCH,SAFE=SAFE,short=SHORT)
  print("all ok")
  data
  #colnames(data$prices) umfasst Y.name und seine x - Intermarket- abhängigkeiten
}


#######################################################################
#MAIN 
#rufe prepare_for_regression_and_calculate_targets_for_signal_and_ranking() und 
#baue dann noch mehr:  Alle Signale für intermediates und fundamentals laufen lassen und drüber singals und rank berechnen.  Optionla vorher auch noch rollierend trainieren
##


prepare_for_classification_and_calculate_targets_for_signal_and_ranking<-function(modelDir="xx",sym="SUP500",Ctrl=NULL,useXlsPar=F)   # main: berechne und bewerte technische modelle auf den Y.names aller xls-files
{
  if (is.null(Ctrl))
  {
    ctrlFile =sprintf("Models/%s/prepMining_%s.xls",modelDir,sym)
    if (file.exists(ctrlFile) &&  useXlsPar)
    {
      Ctrl=readList(xlsName=ctrlFile,table="prepMining.cockpit")
      sag("############ use file based cockpit ################ ",warte=F)  
    }
    else
    {
      Ctrl = list()
      Ctrl$modelDir=modelDir
      Ctrl$sym=sym
      Ctrl$xlsName= sprintf("%s.xls",sym)
      Ctrl$multiTarget = F
      #...............  Ranking-Target .................................
      Ctrl$UNIVERSE = "EM5C_SUP500"
      Ctrl$modelDir <- modelDir
      Ctrl$Y.sym=sym
      Ctrl$BENCH="SUP500"
      Ctrl$SAFE="REX_RI"
      Ctrl$SHORT="SUP500SHORT"
      Ctrl$signal.Set = "SSET2"
      Ctrl$LongShort = "F"
      Ctrl$file ="test2"
      Ctrl$pdf.i = F
      Ctrl$pdf.f = F
      Ctrl$TRAINSYM.i = -1  #NO wenn gar nicht
      Ctrl$TRAINSYM.f = -1  
      
      
      writeList(list=Ctrl,listname="prepMiningClas.cockpit",ctrlFile,create=T) #protokolliere den run..
    }
  }
  #----------cockpit ------------------> #übernimm die ctrl-parameter
  xlsName=Ctrl$xlsName #"sup500.xls"
  Y.sym = Ctrl$Y.sym
  modelDir <<- Ctrl$modelDir
  #tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
  data=  prepare_for_regression_and_calculate_targets_for_signal_and_ranking(Ctrl=Ctrl)  
  
  data$SAFE=Ctrl$SAFE
  data$SHORT=Ctrl$SHORT
  define.Globals(mdata=data,modelDir = modelDir)
  
  
  #............. Y.sym- cloud berechnen ....................
  # Indikatoren grob trainieren 
  
  x.names = unique (c(data$Y.names,data$BENCH,data$SAFE,data$SHORT))
  if (Ctrl$TRAINSYM.i != "NO")
    x.names = unique (c(data$Y.names,data$intermarket.faktoren,data$BENCH,data$SAFE,data$SHORT)) 
  
  data$prices =allPrices[,x.names]
  #so skalieren, dass die dynamik pro zeitreihe voll ausgenutzt wird und negative Werte vermieden werden
  norm_Win(1)
  purePlot(data$prices)
  define.Globals()
  colnames(data$prices)
  
  if (F)
  {
    #  global_arg$clos=data$prices["::2008"]
    x=TSA("signal.levy", global_arg, par=list(win=-27), visual=T, TRAINSYM =data$BENCH)
    x=TSA("signal.levy", global_arg, visual=T, TRAINSYM =0)
    x=TSA("signal.sit.feilscher", global_arg, visual=T, TRAINSYM =data$BENCH)
    x=TSA("signal.sit.feilscher", global_arg, visual=T, TRAINSYM =-1)
    x=TSA("signal.attention", global_arg, visual=T, TRAINSYM =-1)
    
    #signal.attention <-function(arg, par = mlist( sma.w=c(200,120,220,20), diff=c(200,120,220,20),vers=c(1,1,2,1)),visual=F,...)
    
    TrainIndicator ( indiName = "signal.levy",visual=T,TRAINSYM=0)#,frame=sprintf("::2007-01-01"))  
    
    TrainIndicator ( opti="GRID",indiName = "signal.Faber.base",visual=T,TRAINSYM=data$BENCH,frame=sprintf("::2007-01-01"))  
    View(global_ParTable)
    TrainIndicator( opti="GRID",indiName = "signal.sit.feilscher",  visual=T, TRAINSYM =-1)# da
    
  }
  #falls notwendig ... erst mal trainieren .. aber nur bis 2007
  
  #models = Train_all_signal(signal.Set="SSET2",SAFE=SAFE,do.assemble.Signals=F,file="test2",LongShort="F",pdf=F,visual=visual,fastMode=T,TRAINSYM=Y.sym) #nur bei pdf=T 
  
  #alle signale und ranks produzieren 
  #für die x.names 
  #die Intermarket-Cloud - inclus  Y.names
  global_arg$dat<<- data
  temp = clone(global_arg$dat$prices)
  
  #  if (!dataY.names %in% colnames(data$prices))
  #    data$prices = merge()
  #models.i:  i für "intermedidates" - incls Y.names
  data$models.i = Run_all_signal(data=data, signal.Set=Ctrl$signal.Set,SAFE=Ctrl$SAFE,do.assemble.Signals=F, file=sprintf("%s_i",Ctrl$file),LongShort=Ctrl$LongShort, pdf=Ctrl$pdf.i, visual=F,fastMode=T,TRAINSYM=Ctrl$TRAINSYM.i) #nur bei pdf=T 
  
  models.Results(data$models.i)
  #ranking=multi.model.ranking(models)
  #data$ranking.Target = ranking.Target  
  #jetzt fehlen nur noch die non-intermarket -faktoren - die nicht bei den x.names
  #sind
  #................... nun die cloud für die fundamentals
  global_arg$dat = data
  if (Ctrl$TRAINSYM.f != "NO")
  {
    #  if (!SAFE %in% colnames(  global_arg$macros))  #SAFE dazu für signal.SitFeilscher
    #    global_arg$dat$prices = merge(global_arg$dat$macros,data$prices[,SAFE])
    #  else
    global_arg$dat$prices = global_arg$dat$macros
    data$models.f = Run_all_signal(data = data,signal.Set=Ctrl$signal.Set,SAFE=SAFE,do.assemble.Signals=F,file=sprintf("%s_f",Ctrl$file),LongShort=Ctrl$LongShort,pdf=Ctrl$pdf.f,visual=F,fastMode=T,TRAINSYM=Ctrl$TRAINSYM.f) #nur bei pdf=T   
    
    global_arg$dat$prices = clone(temp)
    models.Results(data$models.f)
    
  }
  else
    data$models.f = list()
  
  define.Globals(mdata=data,modelDir = modelDir)
  
  #...........................................................................................
  #cloud.info=multi.model.cloud(data=data, sym= "*", data$models.i,mode=list(target=T,signal=T))
  
  #writeTable(cloud.info,sprintf("Models/%s/cloudInfo.xls",modelDir))
  ################################################################
  
  #classifier.prepare(crs=new.env(),train.data=data$cloud.intermarket,dimTarget=1)
  # clean.model(data)
  data 
}

#........................................TEST.................................
if (F)#Main
{
  modelDir <- "DM_MAI"
  data = prepare_for_classification_and_calculate_targets_for_signal_and_ranking(modelDir="DM_MAI", sym="fDAX")
  save(data,file="datamining.data")
  load(file="datamining.data");modelDir <- "xx"; define.Globals()
}


#.............................................................................
# die modelle will ich auheben um mit multi.model.cloud() beliebige sub-clouds bilden zu können.  ich brauch aber nicht alle daten aus den modellen. unwichtiges wird hier gelöscht
clean.model<-function(data)
{
  
  for (MO in spl("models.i,models.f"))
  {
    # models=data[[MO]]
    mod.names = ls(data[[MO]])
    
    tab=for(mod.n in mod.names)
    {
      # mod=models[[mod.n]]
      
      data[[MO]][[mod.n]]$singleResults<- NULL
      data[[MO]][[mod.n]]$weight=NULL
      data[[MO]][[mod.n]]$trade.summary=NULL
      data[[MO]][[mod.n]]$dates.index=NULL
      data[[MO]][[mod.n]]$equity=NULL
      #mod$Signals.all=NULL
      data[[MO]][[mod.n]]$equity=NULL
      data[[MO]][[mod.n]]$best =NULL
      data[[MO]][[mod.n]]$cagr=NULL
      data[[MO]][[mod.n]]$capital=NULL
      data[[MO]][[mod.n]]$ret=NULL
      data[[MO]][[mod.n]]$share=NULL
      data[[MO]][[mod.n]]$type=NULL
      data[[MO]][[mod.n]]$worst=NULL
    }
  }
  print(lsos(data))
  lsos(data$models.i[[1]])  #zeigt schön den speicherbedarf der objekte
  
}
if (F)
{
  clean.model(data)
  
  MO="models.i"
  mod.n=names(models[[MO]])
  data[[MO]]$models[[mod.n]]$singleResults<- NULL
  lsos(data$models.i[[1]])
  lsos(data) 
}
#. mode=
#signal
#rank
#target
#sym
#quasi eine abfrage-sprache um beliebige clouds bilden zu können:
# auswahl 
multi.model.cloud<-function(data=data,sym=data$BENCH,models,mod.names="*",longOnly=T,mode=list(target=T,scale="scale"),t.sym="*",use.Target="*")
{ 
  #fcall <- match.call(expand.dots = T)
  #as.character(fcall)
  sym=spl(sym)
  t.sym= spl(t.sym)
  mod.names=spl(mod.names)
  use.Target=spl(use.Target)
  
  if (is.character(models))
  {
    if (models == "i")   #nur die intermarket - daten - incls. Y.names
      models = "models.i"
    else
      if (models == "f") #die fundamental-daten
        models = "models.f"
    else
      if (models == "i+f" || models == "f+i") #beide
      {
        models=spl("models.i,models.f")
      }
  }
  if (longOnly=="T")
    longOnly=T
  if (longOnly=="F")
    longOnly=F
  
  if (len(models)==0)
  {
    sag("wrong usage:  no models are given - you did'nt run \n prepare_for_classification_and_calculate_targets_for_signal_and_ranking",warte=T)
  }
  sig<- NULL;  rank <- NULL;  syms=sym
  # syms= names(models[[1]]$singleResults)
  #names(data$models.i[[1]]$singleResults)
  if (t.sym=="*")
    t.sym= colnames(data$multiTarget[[1]])
  
  syms=c()
  
  
  for(MOD in models)
  {
    if (MOD=="models.i")
      Models = data$models.i
    if (MOD=="models.f")
      Models = data$models.f
    
    if (mod.names=="*")
      mod.names = ls(Models)
    
    tab=for(mod.n in mod.names)
    {
      mod=Models[[mod.n]]
      syms=unique (c(syms,names(mod$singleResults)))
      
      if (sym=="*")
        mod.sig = mod$Signal
      else
        mod.sig = mod$Signal[,sym]  #alle signale für sym
      if (sym=="*")
        mod.rank = mod$rank.all 
      else
        mod.rank = mod$rank.all[,sym]     #alle ranking-indikatoren von Allen Intermediates
      
      if (longOnly)
        mod.sig = iif(mod.sig <0,0,mod.sig)
      
      #signale mergen
      
      if (is.null(sig))
        sig<-col.rename(mod.sig,mod.n)
      else
        sig=merge(sig, col.rename(mod.sig,mod.n))
      #ranks mergen
      mod.N =sprintf("Rank%s",mod.n)
      if (is.null(rank))
        rank<-col.rename(mod.rank, mod.N)
      else
        rank=merge(rank, col.rename(mod.rank,mod.N))
    }
  }
  #N=len(ls(models))
  #sig = sig/N
  #sig=bt.apply.matrix(sig,function(col) col/N)   #nicht nötig
  
  #hol dir die schon vorbereiteten Targets aus data
  cloud=NULL
  
  sag=list()
  
  if (has(mode,"target") )
  {   
    target.signal =  select.multiTarget.sum(data, t.sym)
    target.rank = data$ranking.Target[,t.sym]
    colnames(target.rank)= t.sym
    target.rank =col.rename(target.rank,"target_rank")
    #merge alles zur cloud
    purePlot(merge(target.signal,target.rank),main=sprintf("target.signal + target.rank for %s",t.sym))
    if (use.Target =="*")
      cloud= merge(target.signal, target.rank)
    else
      if (use.Target =="signal")
        cloud =target.signal
    else
      if (use.Target == "rank")
        cloud = target.rank
    print(t.sym)
    sag$t.sym=t.sym
  }
  sag$syms=syms 
  sag$mod.n = mod.names
  
  if (has(mode,"signal"))
  {
    if (is.null(cloud))   cloud = sig else cloud=merge(cloud,sig)    
    purePlot(sig,main=sprintf("sig %d",ncol(sig)))
    print(syms)
    sag$signal=colnames(sig)
  }    
  if (has(mode,"rank"))
  {
    if (has(mode,"scale.rank"))
      rank=bt.apply.matrix(rank,scale)
    if (is.null(cloud))   cloud = rank else cloud=merge(cloud,rank)    
    purePlot(rank,main=sprintf("rank %d",ncol(rank)))
    print(syms)
    sag$rank=colnames(rank)
  }    
  sag$cloud=colnames(cloud)
  #qs-check der cloud
  cloud.check(cloud,visual=T)
  print(tail(cloud,30))
  print(syms)
  
  describe= showTable(sag=sag)
  
  #writeTable(universe,xlsName=sprintf("%s/universe.xls",modelDir),table.name="universe",create=T)
  
  #list(t.sym=t.sym,sym=syms,mod.names=mod.names,signal=colnames(sig),rank=colnames(rank),cloud=colnames(cloud)))
  
  #cleansing ...........................................................................
  
  cloud = m.ifna.prev(cloud)
  
  if (sum(apply(cloud,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
    no=foreach(col.i = 1:ncol(cloud)) %do%  { cloud[is.infinite(cloud[,col.i]),col.i]<-0 }
  
  scaled=F
  
  if (has(mode,"scalePCA"))
  {
    mP("scale to pca")
    dim(cloud)
    cloud[]=prcomp(coredata(cloud), scale = TRUE,center=T,tol=0.15)$x   #biplot(pc)
    dim(cloud)
    colnames(cloud)
    scaled=T
  }
  else
    if (has(mode,"scaleTo_1_2"))
    {
      mP("scaleTo_1_2")
      cloud = bt.apply.matrix(cloud, function(col) scaleTo(col,c(1,2)))
      scaled=T    
    }
  else
    if (has(mode,"scale")) 
    {
      mP("scale")
      cloud =bt.apply.matrix(cloud,scale)
      scaled=T
    }
  
  if (scaled)
  {
    if (sum(apply(cloud,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
      no=foreach(col.i = 1:ncol(cloud)) %do%  { cloud[is.infinite(cloud[,col.i]),col.i]<-0 }
    
    purePlot(cloud,main="cloud - scaled")
  }
  list( cloud=cloud,describe=describe)
}
#............................... Beispiele für cloud-bildungen 
if (F)
{
  models = data$models.i
  m=multi.model.cloud(data=data, sym= data$BENCH, models,mode=list(target=T,signal=T))
  m=multi.model.cloud(data=data, sym= data$BENCH, models,mode=list(target=T,rank=T,scale="scale"))
  
  m=multi.model.cloud(data=data, sym= "*", models,mode=list(target=T,rank=T))
  m=multi.model.cloud(data=data, sym= "EURO_US", models,mode=list(target=T,rank=T,signal=T),longOnly=F)
  m=multi.model.cloud(data=data, sym= "EURO_US", models,mod.names="signal.levy",mode=list(target=T,rank=T,signal=T),longOnly=F)
  m=multi.model.cloud(data=data, sym= "*", "i",mod.names="signal.levy",mode=list(target=T,rank=T,signal=T),longOnly=F)
  
  #man kann auch alle args bis auf data als string übergeben
  m=multi.model.cloud(data=data, sym= "*", models="i",mod.names="signal.levy",mode=spl("target,rank,signal"),longOnly="F")
  
  m=multi.model.cloud(data=data, sym= "*", models="i",mod.names="signal.levy",mode=spl("target,rank"),longOnly="F")
  
  m=multi.model.cloud(data=data, sym= data$Y.names, models="f",mod.names="*",mode=spl("target,signal,rank"),longOnly="F")
  m=multi.model.cloud(data=data, sym= "*", models="f",mod.names="*",mode=spl("target,signal,scaleTo_1_2"),longOnly="F")
  m=multi.model.cloud(data=data, sym= "SUP500,REX_RI", models="f",mod.names="*",mode=spl("target,signal,rank,scale"),longOnly="F")
  
}


########################################################################################
print("########### load DataMining.R")

if (F)
  list_R_functions('MLib/DataMining.R')
