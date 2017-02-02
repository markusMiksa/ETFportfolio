##########################################################################################
#ensemble-learning: http://vikparuchuri.com/blog/intro-to-ensemble-learning-in-r/

# schöne online multi-Var-Literatur mit Beispielen 
#http://little-book-of-r-for-multivariate-analysis.readthedocs.org/en/latest/src/multivariateanalysis.html#calculating-correlations-for-multivariate-data
#####################
# 30.3.2014 M.Miksa
#Marke:  #main
#
#multiSignal.r:
#  Oft (nicht nur bei multi-Var-Systemen) brauch ich einen Treiber für die iteration über die zeitreihe.
#rollapplyR() ist da oft zu primitiv:
#  vieleicht möcht ich ja auch wöchentlich über tagesdaten rollieren oder den daten-anfang fixieren ...
#zum schluss mich immer TSA-kompatibel die Ergebnisse wieder zurückbauen.
#All das macht  nun 
#run.roll( )  für mich.
#Aufruflogik:
#  Signal<>  -> Ctrl(list) ->  run.roll(arg,parg, Ctrl)  ->match.fun(Ctrl$xfun)() .. 
#Beispiel:
#  signal.multi.cubM5P() -> run.roll() -> Ctrl$xfun ist use.regression() 
#-  Hier läuft dann eine multivariate lm   und  eine  stepAIC-Variablenausdünnung 
#Optional kann in Ctrl$ctrl$method   eine ganze liste von komplexeren Alogrithmen benannt werden die zusätzlich laufen (im Produktionsmode:  Ctrl$test =F läuft nur der erste in der method-liste... und liefert dann auch das signal.multi.M5P()-Ergebnis.
#Bedeutuung von Ctrl-Parametern
#xvalued:   
#x:  hier wird - wie bei meiner slope-Rechung - x als  1..nrows  angesetzt
#y:  hier wird - blos die zeitreihe selber wieder geliefert  (die Y ist hierzu n.ahead in die Zukunft verschoben)
#y:  macros:   lediglich die fundamentalddaten
#prices:   nur die intermeditates und die zeitreihe selber
#pricesAndMacros: beides

#iterate.on = "months",  #win   ist in dieser skalierung
#use.data = "days",   # n.ahead  ist in dieser skalierungn.ahead = 21,

###############################################################################################################                                                                                                                       


#todo
#going futher http://www.statmethods.net/stats/regression.html
#gam.. anpassen..
#robust regression http://statistics.ats.ucla.edu/stat/r/dae/rreg.htm


#multivariate regression-splines
#http://r.789695.n4.nabble.com/Multivariate-spline-regression-and-predicted-values-td3827136.html


options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
options(error = browser)
#options(error=recover)

############################################################################################
#alle signal-funktionen die multi-variat arbeiten
#Fragen
#ist multi-lin-Reg nicht besser wie slope200 ?
#bieten als x-parameter  runBeta oder runCor  oder runLevy  wichtige Infos ? 
#Da ich standardmässig den KK über 90 Tage berechne, ist mir aufgefallen, dass die Märkte bzw. Indizes immer dann
#nach Süden drehen, wenn der KK signifikant fällt.
#Vielleicht macht es Sinn den KK als Indikator für die Selektion mit aufzunehmen ?


#bieten als x-parameter  mROC()  oder OHLC .. wichige Infos
###########################################################################################

#...................................................

if (F) #main ###########################################################
{
  source("mlib/datamining.r")
  #............... regression  .....................
  data=  prepare_for_regression_and_calculate_targets_for_signal_and_ranking(modelDir="xx", sym="sup500")  
  data=  prepare_for_regression_and_calculate_targets_for_signal_and_ranking(modelDir="xx", sym="gold")  
  
  
  data=  prepare_for_regression_and_calculate_targets_for_signal_and_ranking(modelDir="xx", sym="cars")
  
  x=TSA("signal.multi.cubM5P", global_arg, par=list(win=42),visual=F, TRAINSYM = data$Y.names,   useXlsPar =F)
  
  #..................classification ... braucht mehr- hier laufen auch noch signal-modelle im prepare
  data = prepare_for_classification_and_calculate_targets_for_signal_and_ranking(modelDir="xx", sym="cars")
  save(data,file="datamining_cars.data")
  load(file="datamining.data"); define.Globals(modelDir <- "xx")
  lsos(data$models.f[[1]])
  #---------------------------------------
  
  define.Globals(); P.d <- NULL
  ls(data$models.f[[1]]$singleResults)
  ls(data$models.i[[1]]$singleResults)
  
  #data=  prepare_for_regression_and_calculate_targets_for_signal_and_ranking()  
  #define.Globals(); P.d <- NULL
  #.............
  
  P.d <- NULL  #cashe P.d    TEST  .. nach änderungen im cockpit ausführen
  
  #zum Vergleich:  der univariate ansatz
  x=TSA("signal.lm", global_arg, par=list(win=150),visual=T, TRAINSYM = data$BENCH)
  x=TSA("signal.lm", global_arg, par=list(win=150),visual=T, TRAINSYM = -1)
  
  # teste   
  x=TSA("signal.multi.lm", global_arg, par=list(win=150),visual=T, TRAINSYM = data$BENCH)
  
  x=TSA("signal.multi.cubM5P", global_arg, par=list(win=36),visual=T, TRAINSYM = data$BENCH)
  
  #cars and parts
  x=TSA("signal.multi.cubM5P", global_arg, par=list(win=36),visual=T, TRAINSYM = "SXABP")
  #verlgeich mit univariat
  x=TSA("signal.lm", global_arg, par=list(win=300),visual=T, TRAINSYM = "SXABP")
  
  
  #verlgeich mit univariat
  x=TSA("signal.lm", global_arg, par=list(win=300),visual=T, TRAINSYM = "SUP500")
  
  x=TSA("signal.lm", global_arg, par=list(win=300),visual=T, TRAINSYM = data$Y.names)
  
  x=TSA("signal.ddma.lm", global_arg, par=list(maxdd=10),visual=T, TRAINSYM = data$Y.names) 
  x=TSA("signal.ddma.lm", global_arg, par=list(maxdd=10),visual=T, TRAINSYM = -1)
  #.............
  
  x=TSA("signal.multi.cubM5P", global_arg, par=list(win=42),visual=F, TRAINSYM = data$Y.names,   useXlsPar =F)
  #.................................................................................
  #hiervor muss prepare_for_classification_and_calculate_targets_for_signal_and_ranking() gelaufen sein 
  x=TSA("signal.classify.randomForest", global_arg, par=list(win=42),visual=F, TRAINSYM = data$Y.names, useXlsPar =F)
  
  if (!is.null(collect.varImp))
  {
    vi=na.omit(collect.varImp)
    vid=t(data.frame(bt.apply.matrix(vi, sum, na.rm=T)[1]))
    wichtigkeit =data.frame(vid[order(vid[,1,drop=F], decreasing=TRUE),])
    colnames(wichtigkeit)="varImportance"
    print(wichtigkeit)
  }
}

####### ein Aufruf von run.roll mit fixen spezifischen parametern
##  vergleichbar mit  x=TSA("signal.lm", global_arg, par=list(win=300),visual=T, TRAINSYM = data$BENCH)
# aber multivariat... .. allerdings fehlt hier in x noch die zeitachse ... bringt es was wenn ich monats-differenzen prongostiziere ?
#bringen leads was
#bringt days mehr als months ?
#sollte man die winkel (multi und single - regression in t.indi miteiander addiren - nur signale geben wenn gleichsinnig .. sonst flat)
##############################################################################

signal.multi.lm<-function(arg,par = mlist(win=c(200,50,300,50)),visual=F,...)
{
  #browser(mP("signal.lm"))
  
  #----------cockpit ------------------
  Ctrl=list(
    win=700,#320  as.integer(par$win)
    
    frame= "2002::2011",
    
    iterate.on = "days",
    use.data = "days",   #win und n.ahead  ist in dieser skalierung
    n.ahead = 21,
    roll.mode ="rolling",#fixed"# "rolling"
    train.data = "years",  #ist dem system überlassen ob es öfters trainieren will
    xfun =  "use.regression", # "use.gam"
    ctrl = list(),#list(method = "regress.gam")  #welches regressions-systeme (kommsep-list) soll neben lin-Reg mit stepIAC noch zusätzlich laufen ...
    use_leads =T,  
    test =F,  #F heisst: produktion  - nur bei test=F ist die integration in TSA sichergestellt
    visual=F,#F heisst: produktion
    smooth.signal ="thresh.smooth",  #das runquantile smoothing bei der signal-erzeugung
    do.diff = 0,
    do.scale = T,
    collect.varImp =F,
    xvalues = "pricesANDmacros"#prices" #"macros"#pricesANDmacros,  liste von  namen ...
  )
  #<--------cockpit -------------------
  
  return( run.roll ( arg, par, Ctrl,"signal.multi.lm"))
}
#######################################################################

signal.multi.cubM5P<-function(arg,par = mlist(win=c(42,10,90,2)),visual=F,...)
{
  Ctrl=list(#cars
    #win=par$win,#,756,#36,#42,#320,700   as.integer(par$win)
    win=42, #42
    #frame= "1996::2011",
    frame="",
    iterate.on = "months",  #win   ist in dieser skalierung
    use.data = "months",  # n.ahead ist in dieser skalierung
    #    select.on = "hot",
    n.ahead = 3, #60,##3,
    roll.mode ="rolling",#fixed"# "rolling"
    train.data = "years",  #ist dem system überlassen ob es öfters trainieren will
    xfun =  "use.regression", # "use.classification"
    #bei ignore.lm="F" - wird zunächst lm mit stepAIC-Variablen-Löschung gemacht und dann mit den wenigeren Variabel regress... gemacht - das stört aber den regress.cub...
    ctrl = list( ignore.lm="T",method=" regress.cub,regress.M5P,regress.svm,  regress.gam",variation="b"),#list(method = "regress.gam")  #welches regressions-systeme (kommsep-list) soll neben lin-Reg mit stepIAC noch zusätzlich laufen ...
    use_leads =F,  
    test =F,  #eisst: produktion  - nur bei test=F ist die integration in TSA sichergestellt
    visual=T,#F heisst: produktion
    smooth.signal ="NO thresh.smooth",  #das runquantile smoothing bei der signal-erzeugung
    do.diff = F,
    do.scale = T,
    collect.varImp = F,
    xvalues ="pricesANDmacros"# "macros" # "y"#,x"#,"macros"# "prices"#pricesANDmacros"#prices" #"macros"#pricesANDmacros,  liste von  namen ...
  )
  #----------cockpit ------------------
  Ctrl1=list(#sup500
    #win=par$win,#,756,#36,#42,#320,700   as.integer(par$win)
    win=42,
    #frame= "1996::2011",
    frame="",
    iterate.on = "months",  #win   ist in dieser skalierung
    use.data = "months",  # n.ahead ist in dieser skalierung
    n.ahead = 1, #60,##3,
    roll.mode ="rolling",#fixed"# "rolling"
    train.data = "years",  #ist dem system überlassen ob es öfters trainieren will
    xfun =  "use.regression", # "use.classification"
    ctrl = list(method="regress.cub, regress.M5P"),#list(method = "regress.gam")  #welches regressions-systeme (kommsep-list) soll neben lin-Reg mit stepIAC noch zusätzlich laufen ...
    use_leads =F,  
    test =F,  #F heisst: produktion  - nur bei test=F ist die integration in TSA sichergestellt
    visual=T,#F heisst: produktion
    smooth.signal ="thresh.smooth",  #das runquantile smoothing bei der signal-erzeugung
    do.diff = F,
    do.scale = F,
    collect.varImp = T,
    xvalues ="macros"# "macros" # "y"#,x"#,"macros"# "prices"#pricesANDmacros"#prices" #"macros"#pricesANDmacros,  liste von  namen ...
  )
  Ctrl2=list(#gold
    #win=par$win,#,756,#36,#42,#320,700   as.integer(par$win)
    win=42,
    #frame= "1996::2011",
    frame="",
    iterate.on = "months",  #win   ist in dieser skalierung
    use.data = "months",  # n.ahead ist in dieser skalierung
    n.ahead = 1, #60,##3,
    roll.mode ="rolling",#fixed"# "rolling"
    train.data = "years",  #ist dem system überlassen ob es öfters trainieren will
    xfun =  "use.regression", # "use.classification"
    ctrl = list(method="regress.cub, regress.M5P"),#list(method = "regress.gam")  #welches regressions-systeme (kommsep-list) soll neben lin-Reg mit stepIAC noch zusätzlich laufen ...
    use_leads =F,  
    test =F,  #F heisst: produktion  - nur bei test=F ist die integration in TSA sichergestellt
    visual=T,#F heisst: produktion
    smooth.signal ="thresh.smooth",  #das runquantile smoothing bei der signal-erzeugung
    do.diff = F,
    do.scale = T,
    collect.varImp = T,
    xvalues ="y.macros"# "macros" # "y"#,x"#,"macros"# "prices"#pricesANDmacros"#prices" #"macros"#pricesANDmacros,  liste von      
  )
  #<--------cockpit -------------------
  
  
  return( run.roll (arg,par, Ctrl,"signal.multi.cubM5P"))
}
if (F)
  x=TSA("signal.multi.cubM5P", global_arg, par=list(win=42),visual=F, TRAINSYM = data$Y.names)


#...................................................................
signal.classify.randomForest<-function(arg,par = mlist(win=c(42,10,90,2)),visual=F,...)
{
  #----------cockpit ------------------#M3
  
  Ctrl=list(#cars
    win=42,#42  #in iterate.on - skala
    #frame= "1996::2011",
    frame="",
    iterate.on = "months",  #win   ist in dieser skalierung
    use.data = "months",  # n.ahead ist in dieser skalierung
    n.ahead = 1, #60,##3,  #in use.data-skala
    roll.mode ="rolling",#fixed"# "rolling"
    train.data = "quarters",  #ist dem system überlassen ob es öfters trainieren will
    xfun =  "use.classification", # "use.classification"
    ctrl = list(method=""),  #welches classifi-systeme (kommsep-list) soll neben lin-Reg mit stepIAC noch zusätzlich laufen ...
    use_leads =F,  
    test =F,  #F heisst: produktion  - nur bei test=F ist die integration in TSA sichergestellt
    visual=T,#F heisst: produktion
    smooth.signal ="NO thresh.smooth",  #das runquantile smoothing bei der signal-erzeugung
    do.diff = F,
    do.scale = T,
    collect.varImp = F,
    xvalues="multi.model.cloud",
    #  m=multi.model.cloud(data=data, sym= "*", models="i",mod.names="signal.levy",mode=spl("target,rank,signal"),longOnly="F")
    sym= "*", 
    models="i", # "i+f"
    mod.names= "signal.lm,signal.MA.3",  # "*",   #"signal.lm",
    mode=spl("target ,signal,rank,  scale.rank"), #target, rank,signal, (nimm signale und ranks - skaliere zusätzlich die ranks)    scale.rank
    longOnly="F",
    use.Target="signal",  ntree=500, importance=T ,remove_highCorr=F
    # mtry - the number of parameters to try per tree
    #  ntree - the number of trees grown
    #  keep.forest - logical on whether to store tree 
    #man kann auch alle args bis auf data als string übergeben
    
  )
  #<----------cockpit ------------------
  
  return( run.roll (arg,par, Ctrl,"signal.randomForest"))
}

#............................................................

if (F)
{
  data = prepare_for_classification_and_calculate_targets_for_signal_and_ranking(modelDir="xx", sym="cars")
  save(data,file="datamining_cars.data")
  load(file="datamining.data");modelDir <- "xx"; define.Globals()
  
  ls(data$models.i[[1]]$singleResults)
  
  x=TSA("signal.classify.randomForest", global_arg, par=list(win=42),visual=F, TRAINSYM = data$Y.names, useXlsPar =F)
  
}
#######################################################################
#siehe:  use.signal.ddma.lm()
signal.ddma.lm<-function(arg,par = mlist(maxdd=c(36,10,90,2)),visual=F,...)
{
  #----------cockpit ------------------
  Ctrl=list(
    win=42,#,756,#36,#42,#320,700   as.integer(par$win)
    
    #frame= "1996::2011",
    frame="",
    iterate.on = "months", #win und n.ahead  ist in dieser skalierung
    use.data = "months",   
    n.ahead = 1, #60,##3,
    roll.mode ="rolling",#fixed"# "rolling"
    train.data = "years",  #ist dem system überlassen ob es öfters trainieren will
    xfun =  "use.signal.ddma.lm", # "use.classification"
    ctrl = list(method="regress.cub,regress.M5P"),#list(method = "regress.gam")  #welches regressions-systeme (kommsep-list) soll neben lin-Reg mit stepIAC noch zusätzlich laufen ...
    use_leads =F,  
    test =F,  #F heisst: produktion  - nur bei test=F ist die integration in TSA sichergestellt
    visual=T,#F heisst: produktion
    smooth.signal ="No thresh.smooth",  #das runquantile smoothing bei der signal-erzeugung
    do.diff = 1,   #so wird t.indi nicht als Y-Prognose interpetiert die erst noch mit Roc oder lm in ein t.indi übersetzt werden muss
    do.scale = T,
    collect.varImp = T,
    xvalues = "macros" # "y"#,x"#,"macros"# "prices"#pricesANDmacros"#prices" #"macros"#pricesANDmacros,  liste von  namen ...
  )
  #<--------cockpit -------------------
  
  #zusätzliche marktinformation zu entscheidung von entry-exits-flat
  arg$dat$looks.good = list(looks.good.kanalBreite=looks.good.kanalBreite(arg$clos))
  
  return( run.roll (arg,par, Ctrl,"signal.ddma.lm"))  #der roll-treiber
}
if (F)
  x=TSA("signal.ddma.lm", global_arg, par=list(maxdd=5),visual=T, TRAINSYM = data$Y.names)

#----------------------------------------------------------------------------------------------------
#das eigentliche system:  es wird von run.roll() gerolled und von signal.ddma.lm() parametrisiert und via run.roll() aufgerufen
#Fachlich:
# via ZigZag wird das letze relvante Extremum gefunden.  Von dem wird eine Regressions-Gerade gezogen deren Steigun
#als t.indi. das signal ergibt.     da do.diff =1 gesetzt wurde, wird t.indi nicht als Y-Prognose interpetiert die erst noch mit Roc oder lm in ein t.indi übersetzt werden muss
use.signal.ddma.lm<-function(arg,par,today,sym,Y,X,do.train,Ctrl,n.ahead,test,visual,use_leads,do.scale,do.diff)
{
  ctrl = Ctrl$ctrl
  p=Y
  maxdd=as.integer(par$maxdd)
  if (visual)plot(p)
  P=na.omit(ZigZag(p, maxdd ,percent=T,lastExtreme=F))
  if (visual) lines(P,col="red")
  sd = sign(diff(P,1))
  
  segs = sd[sd!=lag(sd)]
  ma.x = last(segs)
  if (visual) amark(ma.x)
  last.values=p[sprintf("%s::",DateS(ma.x))]
  if (visual) lines(last.values,col="blue")
  #steigng des letzten Trendsegmentes
  t.indi=coef(lm.FITm(last.values))[2]*100
  
  res =xts(t.indi,as.Date(today))
  
  arg$dat$xcrs$Y = res
  return(arg$dat)
}

################################################################################
#MULVAR
#feeded multi-var modelle - gem. sehr vielseitiger konfiguration mit
#z.B.  monatliches training mit Tagesdaten (oder wochendaten )
#verschiedene  use...- multiv-Methoden (siehe use.template() ) können angesteuert werden.  Die Ergebnisse werden wieder in einen Tages-xts zurückgebaut. 
#das gehts weiter mit der üblichen signal-erzeugung
#as.POSIXlt(Sys.time(), "Berlin") 
#as.POSIXlt(Sys.time(), "GMT-2")
################################################################################
run.roll<-function(arg,par,Ctrl=NULL,calledby)
{
  modelDir =global_arg$modelDir
  dat=data=arg$dat
  clos= arg$clos   #multivariat
  sym = colnames(clos)
  clos=data$prices[,sym]  # clos ist mint mNorm-normiert- data$prices evtl. mit scaleTo ...
  ctrlFile =sprintf("Models/%s/cockpit_%s_%s.xls",modelDir,calledby,sym)
  if (file.exists(ctrlFile) && global_arg$useXlsPar)
  {
    Ctrl=readList(xlsName=ctrlFile,table="run.roll.cockpit")
    sag("############ use file based cockpit ################ ",warte=F)  
  }
  else
    if (!is.null(Ctrl))
    {
      sag("####  cockpit is given at code ###### ")
      #trl$datetime=as.character(as.POSIXlt(Sys.time(), "GMT-2"))
      browser()
      Ctrl$datetime=as.character(as.POSIXlt(Sys.time()))#, "%Y-%m-%d %H:%M:%S"))
      
      
     # writeList(list=Ctrl,listname="run.roll.cockpit",ctrlFile,create=T) #protokolliere
      Ctrl$write_results=sprintf("Models/%s/results_%s_%s.xls",modelDir,calledby,sym)
      
      writeList(list=Ctrl,listname="run.roll.cockpit", Ctrl$write_results,create=T) #protokolliere
      
    }#den run..
  
  if (!has(Ctrl,"write_results"))
    Ctrl$write_results=sprintf("Models/%s/results_%s_%s.xls",modelDir,calledby,sym)
  
  if (is.null(Ctrl))
  {
    sag("run.roll: Sorry - no cockpit given",warte=T)
    return("sorry")
  }
  #----------cockpit ------------------> #übernimm die ctrl-parameter
  
  win=Ctrl$win
  
  frame= Ctrl$frame# "2002::2011"
  
  iterate.on = Ctrl$iterate.on
  use.data = Ctrl$use.data# "days",   #win und n.ahead  ist in dieser skalierung
  n.ahead = Ctrl$n.ahead# 31,
  roll.mode =Ctrl$roll.mode #"rolling",#fixed"# "rolling"
  train.data =Ctrl$train.data# "years",  #ist dem system überlassen ob es öfters trainieren will
  xfun =  Ctrl$xfun# "use.regression", # "use.gam"
  ctrl = Ctrl$ctrl #list(),#list(method = "regress.gam")  #welches regressions-systeme (kommsep-list) soll neben lin-Reg mit stepIAC noch zusätzlich laufen ...
  use_leads =Ctrl$use_leads #F,
  test =Ctrl$test #F,  #F heisst: produktion  - nur bei test=F ist die integration in TSA sichergestellt
  visual=Ctrl$visual #F,#F heisst: produktion
  smooth.signal=Ctrl$smooth.signal #"NO thresh.smooth",   #das runquantile smoothing bei der signal-erzeugung
  xvalues =Ctrl$xvalues# "pricesANDmacros"#prices" #"macr
  do.scale = Ctrl$do.scale
  do.diff = Ctrl$do.diff
  if (has(Ctrl,"shift"))
    shift = Ctrl$shift
  else
    shift =0  # die wochen- oder monats-datümer können noch geshiftet werden
  #<--------cockpit -------------------
  
  if (xvalues=="y") x.values =  data$prices[,sym]
  if (xvalues=="x") {x.values =  data$prices[,sym]; x.values[] = 1:nrow(data$prices)}  # wie bei slope:  x=1..len
  if (xvalues == "prices") x.values= data$prices
  if (xvalues == "macros")   x.values= data$macros
  if (xvalues == "y.macros") {
    x.values= merge(data$prices[,sym], data$macros)
                              if (len(data$macros)==0)
                                sag("wrong usage y.macros1 - no data$macros given",T)
  }
  if (xvalues == "pricesANDmacros") 
  {x.values= m.ifna.prev(merge(data$prices,data$macros))
   if (len(data$macros)==0)
     sag("wrong usage y.macros2 - no data$macros given",T)
  }
  
  if(xvalues =="multi.model.cloud")
    #mische die gewünschte  Cloud aus vorberechneten Daten zusammen
  {universe.cloud= multi.model.cloud(data=arg$dat, sym= Ctrl$sym, models=Ctrl$models,
                                     mod.names=Ctrl$mod.names,mode=Ctrl$mode, longOnly=Ctrl$longOnly,t.sym="*",use.Target=Ctrl$use.Target)
   arg$dat$cloud =x.values= universe.cloud$cloud
   
   describe = universe.cloud$describe
   writeTable(table=describe,xlsName=ctrlFile,table.name="cloud",create=F)
  }
  
  else
  {
  #  cloud=multi.model.cloud(data=arg$dat, sym= Ctrl$sym, models=Ctrl.models,mod.names=Ctrl$mod.names, mode=Ctrl.mode,longOnly=Ctrl.longOnly)
  xtab = showTable(y.value=sym, x.values=colnames(x.values))
  writeTable(table=xtab,xlsName=ctrlFile,table.name="xvalues",create=F)
  # writeList(list=list(colnames(x.values)),listname="xvalues",ctrlFile,create=F) #protokolliere
  }
  sag("X values")
  print(colnames(x.values))
  
  if (Ctrl$collect.varImp) #sammel jhier die variablen -wichtikeit (optional)
  {
    collect.varImp <<-  x.values
    collect.varImp[] <<- NA
  }
  else
    collect.varImp <<- NULL
  
  if (!test) {frame="";visual=F; Ctrl$select.on=NULL}  #sonst knischt es bei plotSigPrices in indi.Generic() - weil ich einfach clos kürze ...
  clos = clos[frame,]; x.values=x.values[frame,]
  
  
  dat$xcrs = new.env()
  dat$xcrs$fit = NULL
  #die beiden Indikatoren deren Schnitt mich interessiert:
  mP("win  is:  %d,  len(clos):%d ",win,shape(clos))
  
  if (!test || !is.null(P.d))  #TEST .. weil select.dates so lange dauert
  {
    P.d <<-  select.dates(clos,iterate.on,shift=shift)  #<.........shift:  um z.B. monatliche trainings z. unterschiedlichen monats-Tage machen zu könnne (nicht immer alles gleichzeitig am monats ende......................
    mP("calculate iteration %d days ",len(P.d))
  }
  
  if (iterate.on == "months")
    p.m = clos[P.d]
  
  if ( has(Ctrl,"select.on"))      
  {
    if (has(Ctrl,"select.on","hot"))
      sel.d = select.dates(clos,"hot",shift=shift)  #finde spannenden Datümer automatisch
    else
      sel.d = spl(Ctrl$select.on)  #eine Datums-Liste ist gegeben
    P.d = P.d[as.Date(P.d) %in% as.Date(sel.d)] #welche der iterations-datümer sich auch noch selectiert 
  }
  
  
  if (train.data == iterate.on)
    train.data =P.d
  else
    train.data= select.dates(clos,train.data)
  #lookback-len freischwimmmen bis wenisgens win use.date reinpasst
  
  if (use.data == iterate.on )
  {  
    i=win  ##der offset
    
    #if (i >=len(P.d))
    # sag("change frame %d",len(P.d),warte=T)
  }
  else  #i so wählen dass zum ersten Trainingszeitpunkt i wenigstens win use.data vorliegen
    if (iterate.on == "months" && use.data=="days") 
    {
      #frame=sprintf("::%s",as.Date(index(p.m[win])))
      #win=shape(clos[frame])
      i=win  #iteration-points
    }
  else
    i=1
  if (has(Ctrl,"select.on"))
    i=1
  #genügend daten liegen vorne bereit  
  Y = clos; Y[] = NA
  temp.y = clos; 
  T.indi = clos; T.indi[] = NA  # zur Aufnahme der Richtungs-Ergebnisse
  #..........................................................................
  firstRun <<- T
  #................................................................................>>
  for (ti in (i:len(P.d)))  #iterate.on  monatliche aktionen - sowohl lernen als recall
  {
    temp.y[] = NA
    end.d = as.Date(P.d[ti])
    frame=sprintf("::%s",end.d)
    
    if (use.data =="months" || use.data == "hot")
      p =p.m[frame]
    
    if (use.data =="days" || use.data == "hot")
      p =clos[frame]
    
    if (use.data == "hot")
      p = p[frame]
    
    
    if (roll.mode != "fixed")
    {
      if (firstRun )
        if (use.data != "hot" && use.data != iterate.on)
          win.use.data = shape(p) 
      else
        win.use.data = win 
      
      
      p= last(p,win.use.data)
    }
    if (shape(p)<win.use.data) #i so wählen dass zum ersten Trainingszeitpunkt i wenigstens win use.data vorliegen
      next
    
    do.train = F
    if (as.character(end.d) %in% train.data)  
      do.train =T
    #---------------------Aufruf von use.regression() ...------------------------
    today =as.character(as.Date(index(last(p))))
    mP(" ---   >use %s %s %s %d n.ahead %d",xfun,today,sym,ifelse(do.train,1,0),n.ahead)
    
    X = x.values[as.Date(index(p)),] ;  Ctrl$ctrl = ctrl
    
    dat = try(match.fun(xfun)(arg,par, today=today, sym, p,X, do.train, Ctrl,n.ahead,test,visual,use_leads, do.scale,do.diff))# , silent=TRUE)
    
    #<<<<---------------------------------------------------------------
    
    new.frame = fromToS(dat$xcrs$Y)
    #temp.y is als lineal .. wird wieder entfernt
    if (xfun == "use.classification") #M0
    {
      Y[new.frame,]= merge(temp.y[new.frame,], dat$xcrs$Y[new.frame,"signal"]) [,-1] #lineal wieder wegn
      forest.wichtigkeit = merge(temp.y[new.frame,], dat$xcrs$Y[new.frame])[,-1]
      if (colnames(forest.wichtigkeit)!= colnames(T.indi))
      {
        T.indi = merge(T.indi, forest.wichtigkeit)[,-1]
      }
      else
        T.indi[new.frame,] =  merge(temp.y[new.frame,], dat$xcrs$Y[new.frame])[,-1] #in spalte 2 liegt confidence oder rank
    }
    else  #use.regression et.al
    {      
      #ergebnisse datums gerecht nach Y schreiben
      Y[new.frame,]= merge(temp.y[new.frame,], dat$xcrs$Y[new.frame])[,-1]  
      #....prognose --> richtungs-info
      
      last.prog.values = last(na.omit(Y),abs(n.ahead)+1)
      if (do.diff >0)  #es wurde schon mit den ROC(do.diff) - differenzen gearbeitet
        t.indi = last.prog.values
      else #es kommt der prognostizierte y-wert zurück .. auf dem die Richtung rechnen
      {
        if (len(last.prog.values)<2)  #ist ja einen änderungs-informationen, da brauchts wenigstens 2 werte
          t.indi=NA
        else
        {
          if (len(last.prog.values) <= 3)  #für kurze n.aheads mROC
            t.indi=nval(last(mROC(last.prog.values,len(last.prog.values)-1)))
          else #sonst die Regressions-Winkel der prognostizierten Kurse
            t.indi=coef(lm.FITm(last.prog.values))[2]
        }
      }
      #noch eintragen ...
      T.indi[end.d] = t.indi
    }
    firstRun <<- F
  } #<................. iteriere über alle monate ......................................
  
  # browser(print("ok"))
  if (xfun == "use.classification") #M00
  {
   # Y= dat$xcrs$Y
    collect.varImp =na.omit(T.indi[,-c(1:4)])
    
    smooth.signal ="no"  #signal.smoothing macht bei classification keinen sinn
  #------------
  df=data.frame(colMeans(collect.varImp["2010::",-1]))
  #####>> Auswertung  der VariablenWichtigkeit
  #.............................................................
  # Ausfilern der wichtigsten Fundamentalfaktoren
  #
  #die pro quartal gemittelten faktoren-gewichte
  df.q= apply.yearly(x=collect.varImp, colMeans)
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
  View(K4)
  utils::write.table(K4,file=sprintf("Models/%s/Top300Faktoren_%s.csv",modelDir,sym),dec=".",sep=";",col.names=F,row.names=T)
  #.......................................................
  #kummuliere alle Wichtigkeit auf eines symbols - damit bekannt ist welche Faktor-Zeitreihen (unabhängig von leads oder sonstigen Vorverarbeitungen ) am wichtigsten sind.
  
  OAW=data.frame(rbindlist(symbols))
  sumsym=by(OAW[,2],OAW[,"symbol"],sum)
  symSum=sort(sumsym,decreasing=T)
  View(symSum)
  utils::write.table(symSum,file=sprintf("Models/%s/SymbolWichtigkeit_%s.csv",modelDir,sym),dec=".",sep=";",col.names=F,row.names=T)
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
  View(symSum)
  utils::write.table(symSum,file=sprintf("%sIndikatorWichtigkeit_%s.csv",opath,sym),dec=".",sep=";",col.names=F,row.names=T)
  
  
  
  
  }
  
  Y= dat$xcrs$Y <- m.ifna.prev(Y)
  t.indi=  m.ifna.prev(T.indi)
  
  mchart(merge(clos,Y))
  #  mchart(t.indi)
  #...................... indikator->signal 
  
  if (xfun == "use.classification")
  {
    signal = Y[,1] 
  }
  else #... regression
  {
    signal=sign(t.indi)
    
    if (smooth.signal =="thresh.smooth" )
    {
      tsm = thresh.smooth(clos,t.indi)
      thresh = tsm$thresh
      signal = tsm$Signal
      
      CUT=tsm$CUT 
    }
  }
  
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  colnames(signal)="signal"
  
  
  # if (test)
  mod=plotSigPrice(signal,clos,indi=list(merge(t.indi,0))) 
  
  if (Ctrl$write_results != "")
    writeTable( data.frame(date=as.character(as.Date(index(clos))),clos=clos, equity=mod$eq, signal=signal, t.indi=t.indi),   Ctrl$write_results)
  
  if (!is.null(collect.varImp) && is.xts(collect.varImp))
  {
    #collect.varImp=na.omit(collect.varImp)
    if (visual)
    { 
      print ("var-Importance --->")
      View(collect.varImp)
    }
    
    vi=na.omit(collect.varImp)
    if (nrow(vi)>0)
    {
      vid=t(data.frame(bt.apply.matrix(vi, sum, na.rm=T)[1]))
      wichtigkeit =data.frame(vid[order(vid[,1,drop=F], decreasing=TRUE),])
      colnames(wichtigkeit)="varImportance"
      print(wichtigkeit)
      df= data.frame(cbind(rownames(wichtigkeit),wichtigkeit))
      if (Ctrl$write_results != "")
        writeTable( df,  Ctrl$write_results,table.name="varImportance",create=F)
    }
    
  }  
  #gib das signal zurück, und auch noch die Hilfsvariablen die Du evtl. in indi.Generic() sehen willst:
  if (smooth.signal =="thresh.smooth" )
    return(list(Signal=signal, Indi=list(merge(t.indi,0,CUT,thresh,-thresh)),rank=t.indi,varImp=collect.varImp))
  else
    return(list(Signal=signal, Indi=list(merge(t.indi,0)),rank=t.indi,varImp=collect.varImp))
  
}
#..................................................
use.template<-function(arg,par,today,sym,Y,X,do.train,Ctrl,n.ahead,test,visual,use_leads,do.scale,do.diff)
{
  method =ctrl$method;  ctrl = Ctrl$ctrl
  
  arg$dat$xcrs$Y = Y
  return(arg$dat)
}
#############################################################################
#treiber für regression-modelle:
#train+test- set. (test aber nur wenn nicht test - variable F ist)
#baut multi  lm    und optimiert mit stepIAS
#baut anfangs - optional wenn use_leads T ist  nach die HotLags2.cc zum Y
#wenn Ctrl.method  eine kompSep-Liste von methoden-Namen enthält,
#werden hier auch alternative regressions-modelle zum Vergleich aufgerufen
#(siehe regress.gam)
#Ein trainings-Tag wird ihm signalisiert, dass beim Auftruf  do.train ==1
#gesetzt ist.
#dat ist data,  today ist der heutige Tag, sym ist colname(Y), 
#Y der zu pronostizierende Wert, X- das XTS  mit den  right-hand werten,
#n.ahead sagt wie viele tage/monate prognostiziert werden sollen
#im Produktionsfall (!test) wird nur e i n  modell gerechnet  (z.b. das erste ctrl$method- modell)
#############################################################################
use.regression<-function(arg,par,today,sym,Y,X,do.train,Ctrl,n.ahead,test,visual,use_leads,do.scale,do.diff)
{
  ctrl = Ctrl$ctrl
  dat=arg$dat
  if (firstRun)  do.train=T
  
  ignore.lm = has(ctrl,"ignore.lm","T")
  
  if (!test && has(ctrl,"method") &&  !has(ctrl,"ignore.lm","F"))  {ignore.lm =T } 
  
  
  method =ctrl$method
  Y.org=Y
  colnames(Y)="Y"
  good.sym.i=1
  
  if (use_leads)
  {
    x=X  
    pi=which(colnames(x)==sym)
    if (len(pi)>0)
      x=x[,-pi]
    feature.leads= na.omit(HotLags2.CC(p=Y, x,visual=F))
    print("-------------LEADS ")
    print(colnames(feature.leads))
    X=merge(X,feature.leads)
    x=NULL
  }
  
  b=merge(Y,X)
  Y=m.ifna.prev(b[,1])
  X=m.ifna.prev(b[,-1])
  
  TrainingOutcome=lag(Y,-(abs(n.ahead))) #die lezten n.ahead - werte sind NA
  TrainingPredictors = X[] 
  
  #purePlot(merge(TrainingOutcome,TrainingPredictors[,1]),main=sprintf("pre lag %d",n.ahead));View(merge(Y,X[,1]))
  if (test)
    train.frame = 1:(as.integer(nrow(Y)-n.ahead)*2/3) #um zu sehen ob das modell auch noch über n.ahead hinaus auch noch taugt
  else
    train.frame = 1:(as.integer(nrow(Y)-n.ahead))  #hinten gibts keine prognosen 
  test.frame = (last(train.frame)+1):nrow(TrainingPredictors)
  today=as.Date(index(last(Y[train.frame])))
  prognose.day=as.Date(index(Y[shape(Y[train.frame])+abs(n.ahead)]))
  mP(" ---  today:  %s   prognose: %s  ----",today, prognose.day)  
  
  #allPredictors = data.frame(coredata(X))
  
  if (do.diff >0)
  {
    TrainingOutcome = na.omit(bt.apply.matrix(TrainingOutcome,ROC,do.diff))
    TrainingPredictors =  na.omit(bt.apply.matrix(TrainingPredictors, ROC,do.diff))
  }
  
  if (do.scale)
  {
    TrainingOutcome = bt.apply.matrix(TrainingOutcome,scale)
    TrainingPredictors =  bt.apply.matrix(TrainingPredictors,scale)
  }
  
  #......... data-frames dazu
  df.train = na.omit(data.frame(merge(TrainingOutcome,TrainingPredictors)[train.frame]) )
  df.test = na.omit(data.frame(TrainingPredictors[test.frame,]) )
  trainingOutcome=coredata(TrainingOutcome[train.frame,])
  testOutcome=coredata(TrainingOutcome[test.frame,])
  
  trainingOutcome=array(coredata(TrainingOutcome[train.frame,]))
  testOutcome=array(coredata(TrainingOutcome[test.frame,]))
  
  if (firstRun)
  {
    print("df.train------------------------------->")
    print(head(df.train,5))
    print(colnames(df.train))
    norm_Win(1)
    purePlot(xts(df.train,as.Date(rownames(df.train))),main="firstRun")
    print(fromToS(TrainingPredictors))
    #if (visual) sag("warte",warte=T)
  }
  
  #...................
  
  if (!ignore.lm)
  {  
    fm=lmFormular(DF=df.train[,],1)
    
    if (F)  #kommen die Daten richtig an ?
    {
      Y_=vec2xts(lineal=TrainingOutcome,df.train[,1])
      X_=vec2xts(lineal=TrainingOutcome, df.train[,2])
      purePlot(merge(Y_,X_[,1]),main="prefit");  View(merge(Y_,X_[,1]))
    }
    
    fit= eval(parse(text=sprintf("lm(%s,data=df.train)",fm)))  #eval(parse .. ist wichtig damit später stepAIC funktioniert
    pred=predict(fit, df.test  )
    
    if (test)fehler = sqrt(mean((pred - testOutcome)^2,na.rm=T)) else fehler = 99
    
    if (visual)
    {
      if(  has(ctrl,"method") ) norm_Win(3) else    norm_Win(2)
      all=
        merge(X[,1],
              rbind(
                merge(xts(trainingOutcome,index(Y[train.frame])),fit$fitted.values  ),
                merge(xts(testOutcome,index(Y[test.frame])),pred)))
      purePlot(all,main=sprintf("%s alle %d vari - lag %d - fehler %.4f",today,ncol(df.train)-1,n.ahead,fehler))
      amark(today)
      amark(prognose.day)
    }
    library(caret)
    vi=sort.data.frame( varImp(fit),"Overall",decreasing=T)
    if (visual)print(vi)
    #browser(print("wait"))
    #variablen-reduzierung stepAIC() ist weniger schädlich wie die mit findCorrelation
    if (visual)print("----> STEP -->")
    step <- stepAIC(fit, direction="both",trace=F)
    pred=predict(step, df.test)
    if (test)fehler = sqrt(mean((pred - testOutcome)^2,na.rm=T)) else fehler = 99
    #mchart(merge(xts(testOutcome,index(p.c[test.frame])),pred),main=sprintf("alle p - ohne lag - fehler %.2f",fehler)); amark(today)
    vi=sort.data.frame( varImp(step),"Overall",decreasing=T)
    print("---- stepped vars: ----")
    print(vi)
    good.sym.i = sapply(rownames(vi), function(col) which(col==colnames(X)))
    
    if(visual)print(step$anova)
    #  print(  step$anova$Step )
    sym.removed=c(na.omit(sapply(step$anova$Step,prettyWpName2)));names(sym.removed)=NULL 
    
    if (visual)
    {
      print("------------------------------------------------------")
      mP("von %d wurde %d  Variablen von stepAIC gelöscht",ncol(df.train),len(sym.removed))
      #print(sym.removed)
      print("used variables:")
      print(vi)
      all=
        merge(X[,1],
              rbind(
                merge(xts(trainingOutcome,index(Y[train.frame])),step$fitted.values  ),
                merge(xts(testOutcome,index(Y[test.frame])),pred)))
      purePlot(all,main=sprintf("%s stepped %d vari - lag %d - fehler %.4f", today,nrow(vi),n.ahead,fehler))
      amark(today)
      amark(prognose.day)
    }
    #oder auch mit
    #rownames(varImp(fit))[!rownames(varImp(fit)) %in% rownames(varImp(step))]
    best.mod=step
  } #ignore.lm
  ########################################
  
  
  #................. gam, cub,  M5P,.   ........
  if (has(ctrl,"method"))  #call.ctrl.Method  --- aufruf der  regression.<> - Methoden
  {
    for (meth in spl(ctrl$method))
    {
      if (visual)
        mP("------------------> method ----> %s",meth)
      if (firstRun)
        sag("method \n  %s ",meth)
      res = try(match.fun(meth)(df.train, df.test, X,good.sym.i,do.train,test,Ctrl$ctrl))# , silent=TRUE)
      
      if (test)fehler = sqrt(abs(mean(res$pred - testOutcome^2,na.rm=T))) else fehler = 99    
      
      if (visual)
      {
        all=
          merge(X[,1],
                rbind(
                  merge(xts(trainingOutcome,index(Y[train.frame])), res$fitted.values ),
                  merge(xts(testOutcome,index(Y[test.frame])),res$pred)))
        purePlot(all,main=sprintf("%s %s:  %d vari - lag %d - fehler %.4f",today, meth, ncol(df.train)-1,n.ahead,fehler))
        amark(today)
        amark(prognose.day)
        
        mP("<------------------ method ---- %s",meth)
        sag("warte",T)
      }
      if (!is.null(collect.varImp) && is.xts(collect.varImp))
      {
        vi=sort.data.frame( varImp(res$fit),"Overall",decreasing=T)
        print(vi)
        k_= lapply(colnames(t(vi)), function(coln) collect.varImp[as.Date(today),coln] <<- vi[coln,])
        print(vi)
      }
      
      #  print(AIC(fit,step,res$fit))
      
      best.mod=res$fit;  pred = res$pred
      
      if (!test)break   #im Produktionsfall (!test) wird nur ein modell gerechnet 
    }
    
  }
  else
    if (!ignore.lm) 
      print(AIC(fit,step))
  
  if (visual) sag("warte",warte=T)#;browser() 
  
  res =xts(pred,index(Y[test.frame]))
  
  dat$xcrs$Y = res
  return(dat)
}
#########################################################################
#... implementiert alternative regressions-algorithmen - wird bei  #call.ctrl.Method aufgerufen
#########################################################################
regress.M5P<-function(df.train,df.test,X,good.sym.i,do.train,test,ctrl)
{
  library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
  
  if (len( good.sym.i[[1]])>0)   X.light =X[,c(good.sym.i)]
  
  #M5P  ok  aber ohne varImp()
  fit.m5p =eval(parse(text=sprintf("M5P(%s ~ .,data=df.train)",colnames(df.train)[1])))
  fitted.value=fitted(fit.m5p)
  pred=predict(fit.m5p, df.test)
  
  return(list(fit=fit.m5p, fitted.values = fitted.value, pred=pred))
}

#still buggy .-uselesss
#Todo:   bayesglm {arm}
regress.glm <-function(df.train,df.test,X,good.sym.i,do.train,test,ctrl)
{
  # if (len( good.sym.i[[1]])>0)   X.light =X[,c(good.sym.i)]
  library(bestglm)
  colnames(df.train)
  #bestglm  ok  aber ohne varImp()
  library(glmnet)
  #df.train= na.omit(df.train)
  fit.glm=glmnet(y=(as.matrix(df.train[,1])),x=as.matrix((df.train[,-1])),family="mgaussian")
  x=as.matrix((df.train[,-1]))
  xt=as.matrix(coredata(df.test))
  
  predict(fit.glm, newx=xt,s=0.001,type="response")
  predict(fit,x,s=0.001,type="response")
  #bestglm(Xy=cbind(df.train[,-1],df.train[,c(2:ncol(df.train))]))
  #fit.glm =eval(parse(text=sprintf("bestglm(Y~., data=df.train)")))
  #fitted.value=fitted(fit.glm)
  #pred=predict(fit.glm, df.test)
  
  #fit.glm=bestglm(Xy=data.frame(merge(crs$tdata[,crs$input],crs$Target[,1])) )
  #pred=predict(fit.glm, newdata=data.frame(as.factor(crs$tdata[,crs$input])))
  #ls(fit.glm)
  #fit.glm$BestModel
  
  return(list(fit=fit.glm, fitted.values = fitted.value, pred=pred))
}



#########################################################################
regress.cub<-function(df.train,df.test,X,good.sym.i,do.train,test,ctrl)
{
  #diese variablen wurde vorher von stepAIC - lm ausgesucht
  good.cols = colnames(df.train[,-1])
  
  if (len( good.sym.i)>1 ) {X.light =X[,c(good.sym.i)]; good.cols=colnames(X[,c(good.sym.i)])}
  
  if (firstRun)
  {
    committees <<-10
    neighbors <<- 9
  }
  #einmalig die richtigen trainings-parameter committees und neighbors finden
  #trainiere mit caret durch verändern der parameter
  if (!test && firstRun)#do.train)  #TEST weil langsam
  {
    mP("tune cub committees %d, neighbors %d",committees,neighbors)
    library(caret)
    set.seed(1)
    p=na.omit(p)
    
    cTune <- caret::train(x=df.train[,-1], y=df.train[,1],
                          "cubist",
                          tuneGrid = expand.grid(.committees = c(1, 10, 50, 100),
                                                 .neighbors = c(0, 1, 5, 9)),
                          trControl = trainControl(method = "cv"))
    cTune  #anzeigen der tuning-results
    
    committees <<- nval(cTune$bestTune[1])
    neighbors <<- nval(cTune$bestTune[2])
    mP("tune ed cub >>> committees %d, neighbors %d ",committees,neighbors)
    
    
    plot(cTune)     
  }
  
  #cubist:  noch besser wie M5P
  #http://cran.r-project.org/web/packages/Cubist/vignettes/cubist.pdf
  library(Cubist)
  
  fit.cub=cubist(x=df.train[,good.cols,drop=F],y=df.train[,1],committees=committees)  #50 besser, 20 gehen auch
  fitted.values=predict(fit.cub,df.train[,good.cols,drop=F], neighbors=neighbors )
  pred=predict(fit.cub, df.test, neighbors=neighbors )
  
  #variablen wichtigkeit
  if (F)
  {
    print("variablen wichtigkeit fit.cub")
    print(varImp(fit.cub))
    
    fit.cub$usage
    summary(fit.cub)
  }
  res=list(fit=fit.cub, fitted.values = fitted.values, pred=pred)
  return(res)
}
#---------------------------------------------------
regress.svm<-function(df.train,df.test,X,good.sym.i,do.train,test,ctrl)
{
  #diese variablen wurde vorher von stepAIC - lm ausgesucht
  good.cols = colnames(df.train[,-1])
  
  if (len( good.sym.i)>1 ) {X.light =X[,c(good.sym.i)]; good.cols=colnames(X[,c(good.sym.i)])}
  
  if (firstRun)
  {
    committees <<-10
    neighbors <<- 9
  }
  #einmalig die richtigen trainings-parameter committees und neighbors finden
  #trainiere mit caret durch verändern der parameter
  if (FALSE && !test && firstRun)#do.train)  #TEST weil langsam
  {
    mP("tune cub committees %d, neighbors %d",committees,neighbors)
    library(caret)
    set.seed(1)
    p=na.omit(p)
    
    cTune <- caret::train(x=df.train[,-1], y=df.train[,1],
                          "ksvm",
                          tuneGrid = expand.grid(.committees = c(1, 10, 50, 100),
                                                 .neighbors = c(0, 1, 5, 9)),
                          trControl = trainControl(method = "cv"))
    cTune  #anzeigen der tuning-results
    
    committees <<- nval(cTune$bestTune[1])
    neighbors <<- nval(cTune$bestTune[2])
    mP("tune ed cub >>> committees %d, neighbors %d ",committees,neighbors)
    
    
    plot(cTune)     
  }
  
  #cubist:  noch besser wie M5P
  #http://cran.r-project.org/web/packages/Cubist/vignettes/cubist.pdf
  
  x <- data.matrix(df.train[,good.cols,drop=F])
  x.t <- data.matrix(df.test[,good.cols,drop=F])
  y <- data.matrix(as.array(df.train[,1,drop=T]))
  
  if (F)
  {
    library(kernlab)
    
    ## regression
    # create data
    # train support vector machine
    fit.svm <- ksvm(x,y)#,epsilon=0.01,cross=3)
    fitted.values = predict(fit.svm,x)
  }
  else
  {
    library(e1071)
    fit.svm = svm(x=x,y=y)
    fitted.values = predict(fit.svm,x)
    
    #plot(1:shape(y),y,type="l")
    #lines(1:shape(y),fitted.values,col="red")
    pred=predict(fit.svm, x.t )
  }
  #variablen wichtigkeit
  if (F)
  {
    print("variablen wichtigkeit fit.cub")
    print(varImp(fit.svm))
    
    fit.svm$usage
    summary(fit.svm)
  }
  res=list(fit=fit.svm, fitted.values = fitted.values, pred=pred)
  return(res)
}


#--------------------------------------------------------------------------

regress.gam<-function(df.train,df.test,X,good.sym.i,do.train,test,ctrl)
{
  library(mgcv)  
  
  X.light =X[,c(good.sym.i)] #beschränke dich auf die wichigsten x-variablen die zuvor mit stepAIC und lm() ausgefiltert wurden
  maxGamVars=2
  if (nrow(df.train )> 250)
    maxGamVars=3   ##  sonst crashed gam weil zu viele Freiheitsgrade
  
  
  maxXvar=4
  fit.gam=NULL
  while( maxXvar>0)
  { 
    #fit.cmd=sprintf("bam(Y~%s,data=df.train,method='REML')",paste(sapply(colnames(X.light[1: maxXvar]),function(coln) sprintf("s(%s)",coln)),collapse="+"))
    
    if (has(ctrl,"variation","a"))
    {
      fm=gamFormular(X.light[,1:min(maxGamVars,ncol(X.light))])   #es müssen genügend Daten vorliegen
      fit.cmd=sprintf("gam(Y~te(%s),data=df.train)",fm)
      
    }
    else
    {
      fit.cmd=sprintf("gam(Y~%s,data=df.train,method='REML')",paste(sapply(colnames(X.light)[1: maxXvar],function(coln) sprintf("s(%s)",coln)),collapse="+"))  
    }
    
    temp= try(eval(parse(text=fit.cmd)))  #e
    if(inherits(temp, 'try-error')) 
      maxXvar= maxXvar-1
    else
    {
      fit.gam=temp
      break
    }
  }  
  if (is.null(fit.gam))
  {sag("probleme mit gam",F)
   
   browser()
  }
  pred=predict(fit.gam, df.test)
  
  return(list(fit=fit.gam, fitted.values = fitted(fit.gam), pred=pred))
}
###########################################################################

#---------------------------------------------------------------------------
gamFormular<-function(X)  #analog zu lmFormular:  baut einen parameter-string
{
  coln1=sapply(colnames(X),function(coln) sprintf("%s",coln))
  
  xcol=paste(coln1,collapse=",")
}
if (F)
  print(  gamFormular(X) )

#.....................................

use.gam<-function(dat,today,sym,Y,X,do.train,Ctrl,n.ahead,test,visual,use_leads)
{
  ctrl=Ctrl$ctrl
  method =ctrl$method
  library(mgcv)
  Y.org=Y
  #... gam-formate
  fm=gamFormular(X[,1:2]) 
  ## isotropic thin plate spline smoother
  fit1 <- gam(Y~s(X[,1],X[,2]))
  #predict(fit1,newdata=list(X=W))
  
  ## tensor product smoother  #best
  fit2 <- gam(Y~te(X[,1],X[,2]))
  
  fit.cmd=sprintf("gam(Y~te(%s),select=TRUE)",fm)
  fit.cmd=sprintf("gam(Y~te(%s))",fm)
  
  
  fit= eval(parse(text=fit.cmd))  #eval(parse .. ist wichtig damit später stepAIC funktioniert
  
  #method="REML",select=TRUE
  
  predict(b,newdata=list(X=W))
  
  ## variant tensor product smoother
  fit3 <- gam(Y~t2(X[,1],X[,2]))  #fitted
  predict(b,newdata=list(X=W))
  
  AIC(fit,fit1,fit2,fit3) #the smaller the AIC or BIC, the better the fit.
  
  
  dat$xcrs$Y = Y  
  return(dat)
  
}

if (F)
{
  n<-200
  sig <- 2
  dat <- gamSim(1,n=n,scale=sig)
  
  b<-gam(y~s(x0)+s(I(x1^2))+s(x2)+offset(x3),data=dat)
  
  newd <- data.frame(x0=(0:30)/30,x1=(0:30)/30,x2=(0:30)/30,x3=(0:30)/30)
  pred <- predict.gam(b,newd)
}
#############################################################################
#############################################################################
#treiber für regression-modelle:
#train+test- set. (test aber nur wenn nicht test - variable F ist)
#baut multi  lm    und optimiert mit stepIAS
#baut anfangs - optional wenn use_leads T ist  nach die HotLags2.cc zum Y
#wenn Ctrl.method  eine kompSep-Liste von methoden-Namen enthält,
#werden hier auch alternative regressions-modelle zum Vergleich aufgerufen
#(siehe regress.gam)
#Ein trainings-Tag wird ihm signalisiert, dass beim Auftruf  do.train ==1
#gesetzt ist.
#dat ist data,  today ist der heutige Tag, sym ist colname(Y), 
#Y der zu pronostizierende Wert, X- das XTS  mit den  right-hand werten,
#n.ahead sagt wie viele tage/monate prognostiziert werden sollen
#im Produktionsfall (!test) wird nur e i n  modell gerechnet  (z.b. das erste ctrl$method- modell)
#############################################################################
use.classification<-function(arg,par,today,sym,Y,X,do.train,Ctrl,n.ahead,test,visual,use_leads,do.scale,do.diff)
{
  ctrl = Ctrl$ctrl
  dat=arg$dat
  if (firstRun)
     dat$crs= new.env()
  
  crs = dat$crs 
  
  if (firstRun || ifelse( len(ls(crs))==0,T,F))  do.train=T

  ignore.lm = has(ctrl,"ignore.lm","T")
  
  if (!test && has(ctrl,"method") &&  !has(ctrl,"ignore.lm","F"))  {ignore.lm =T } 
  
  
  method =ctrl$method
  Y.org=Y
  colnames(Y)="Y"
  good.sym.i=1
  
  if (use_leads)
  {
    x=X  
    pi=which(colnames(x)==sym)
    if (len(pi)>0)
      x=x[,-pi]
    feature.leads= na.omit(HotLags2.CC(p=Y, x,visual=F))
    print("-------------LEADS ")
    print(colnames(feature.leads))
    X=merge(X,feature.leads)
    x=NULL
  }
  
  b=merge(Y,X)
  Y=m.ifna.prev(b[,1])
  X=m.ifna.prev(b[,-1])
  
  TrainingOutcome=lag(X[,1],-(abs(n.ahead))) #die lezten n.ahead - werte sind NA
  TrainingPredictors = X[,-1] 
  
  #purePlot(merge(TrainingOutcome,TrainingPredictors[,1]),main=sprintf("pre lag %d",n.ahead));View(merge(Y,X[,1]))
  if (test)
    train.frame = 1:(as.integer(nrow(Y)-n.ahead)*2/3) #um zu sehen ob das modell auch noch über n.ahead hinaus auch noch taugt
  else
    train.frame = 1:(as.integer(nrow(Y)-n.ahead))  #hinten gibts keine prognosen 
  test.frame = (last(train.frame)+1):nrow(TrainingPredictors)
  today=as.Date(index(last(Y[train.frame])))
  prognose.day=as.Date(index(Y[shape(Y[train.frame])+abs(n.ahead)]))
  mP(" ---  today:  %s   prognose: %s  ----",today, prognose.day)  
  
  #allPredictors = data.frame(coredata(X))
  
  if (do.diff >0)
  {
    #TrainingOutcome = na.omit(bt.apply.matrix(TrainingOutcome,ROC,do.diff))
    TrainingPredictors =  na.omit(bt.apply.matrix(TrainingPredictors, ROC,do.diff))
  }
  
  if (F && do.scale)
  {
   #scalieren identischer Werte gibt NAN
    #TrainingOutcome = bt.apply.matrix(TrainingOutcome,scale)
    TrainingPredictors =  bt.apply.matrix(TrainingPredictors,scale)
  }
  #......... data-frames dazu
  df.train = na.omit(data.frame(merge(TrainingOutcome,TrainingPredictors)[train.frame]) )
  df.test = na.omit(data.frame(TrainingPredictors[test.frame,]) )
  trainingOutcome=coredata(TrainingOutcome[train.frame,])
  testOutcome=coredata(TrainingOutcome[test.frame,])
  
  trainingOutcome=array(coredata(TrainingOutcome[train.frame,]))
  testOutcome=array(coredata(TrainingOutcome[test.frame,]))
  
  if (firstRun)
  {
    print("df.train------------------------------->")
    print(head(df.train,5))
    print(colnames(df.train))
    norm_Win(1)
    purePlot(xts(df.train,as.Date(rownames(df.train))),main="firstRun")
    print(fromToS(TrainingPredictors))
    #if (visual) sag("warte",warte=T)
  }
  
  
  #das bis maxWin wachsende Zeitfenster zum Tag:  lastDay
  #cloud = arg$dat$cloud#[train.frame]  
  #das Ergebnis wird ein 3 dim  xts aus signal und signalsicherheit und model.err
  res=as.xts(data.frame(signal=0,confidence=0, model.err=0),as.Date(today))
  
  mP("...      use.classification     %s  %s",sym,today)
  #if (shape(na.omit(cloud[today,-1]))==0)
  #{mP("SORRY - there are no cloud-features for today ");browser();return(res)}
  #if (toDay=="")
  #{mP("SORRY - there are no price-data for today ");return(res)}
  
  
  prognose=NULL
  
  if (!exists("dont_use_last") || len(dont_use_last) <1 || dont_use_last < 1 ) #global
    dont_use_last <<- 1
  
  last.train.failed =  is.na(last(crs$forest.fit.wichtigkeit) )[1]
  forest.fit.wichtigkeit ="no"
  
  if (do.train || firstRun || last.train.failed)
  {
    mP("retrain.event !!! at %s ################## %s",today,Ctrl$iterate.on)
    #browser(mP("#K1"))
    #ich darf ja nicht einfach bis heut trainieren ... 
    #priceFeatures[1:max(1,nrow(priceFeatures)-dont_use_last),-1]
    #-------------------------------------------------------------------
    df.train = xts(df.train, as.Date(rownames(df.train)))
    
    forest.fit.wichtigkeit= classifier.randomForest.fit(crs, df.train, dimTarget=1,ntree=Ctrl$ntree, importance=Ctrl$importance ,remove_highCorr=Ctrl$remove_highCorr,today=today)# <<<< trainiere - ohne explizit noch mal die Kurse zu ?bergeben
    #View(t(forest.fit.wichtigkeit))
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
  #-------------------------------------------------------------------
  #if (is.null(prognose))
  
  df.test = xts(df.test, as.Date(rownames(df.test)))
  
  prognose = classifier.randomForest.predict(crs,df.test) #####<<<<###############
  mP("#a3")
  if (!is.xts(prognose))
  {
    mP("#bug at roll.classifier.e4c_ ")
    
  }
  #print(prognose)
  #.................................................................................
  mP("#a4--------->")
  #cbind der variablenwichtigkeit an die prognose - aber setze na wenn nicht trainiert wurde
  if (forest.fit.wichtigkeit=="no")
  {
    forest.fit.wichtigkeit=crs$forest.fit.wichtigkeit[1,];
    forest.fit.wichtigkeit[]=NA
  }
  
  #prognose=cbind(prognose[,c(1,2)],forest.fit.wichtigkeit)
  #return(prognose)
  
  #....................
  
  if (visual) sag("warte",warte=T)#;browser() 
  #res =xts(pred,index(Y[test.frame]))
  res = prognose[,c("signal","confidence")] #M1
  res = merge(res,coredata(forest.fit.wichtigkeit))
  
  dat$xcrs$Y = res
  return(dat)
}
#................................................................................
if (F)
  x=TSA("signal.classify.randomForest", global_arg, par=list(win=42),visual=F, TRAINSYM = data$Y.names)

###################################################################################
#backup
##############################################################################


rollRegressionXTS.multiVar<-function(Y=data$prices,win=200,n.ahead=30)
{
  
  rwf.Forecast <- function(p,h,...) {
    x=ts(p,frequency=356)
    
    fct =forecast(fit, h=h, level=99)$mean
    #residuals(Arima(y,model=fit)) 
    res = (last(fct)-last(x)) / last(x)*100
  }
  
  for (i in (20:len(P)))
  {
    end.d = as.Date(index(P[i]));frame=sprintf("::%s",end.d); pi=P[frame]
    t.indi[i]= spline.fcst(pi, gl)
  }  
  
  
  dolm1 <- function(p,nplus){
    #fit=lm.FITm(p=p+10000,visual=F,getIndex=T,level=.90,nplus=nplus)
    #fct=(last(fit$channel[,1]))-10000
    rwf.Forecast(p,h=nplus)
    
  }
  
  slope90=rollapplyr(Y, win, dolm1, by.column = F, nplus = n.ahead)
}


if (F)
{
  p=data$prices[,data$BENCH]
  slope200=rollRegressionXTS.rwf(p,200) 
}

#############################################################################################
#nimm ein signal-Timing-System, trainiere es monthly,(oder quarterly...) für alle symbole und berechene ein TSA dafür
#das TSA soll als selektion   slope200 haben
#  hier gibts ne ganze reihen von multivar-reg-modellen
# es stellt sich aber raus:   es ist super schwer nur das kleinste bischen lead- hinzubekommen - intermarket faktoren  mit leads reichen nicht - vieleicht helfen fundamentals ?
#mit stepAIC lassen sich aber faktoren schon mal schonend entfernen..
#########################################################################################
#MULVAR

find.all.leads.and.reduce.highCorr<-function(p,removeHighCor=T,visual=T)
{
  pi=4
  leads=list()
  #for (pi in 1:ncol(p))
  pi=1
  pi=which(colnames(p)==data$Y.names)
  
{
    sym=colnames(p[,pi])
    
    feature.leads= na.omit(HotLags2.CC(p=p[,pi], p[,-pi],visual=visual))
    #  warum kostet mich das ein ganzes Jahr vorn ?
    leads[[sym]]=feature.leads
  }
#reduce highCorrs
#for (pi in 1:ncol(p))
{
  sym=colnames(p[,pi])
  #browser()
  mP("###### leads for #######> %s",sym)
  feature.leads=leads[[sym]]
  
  if (len(feature.leads))
  {
    print(dim(feature.leads));print(fromToS(feature.leads))
    print(colnames(feature.leads))
    
    ################################
    # CLOUD building
    ################################
    today="2009-01-01"
    futLAG=0
    train.frame=sprintf("::%s",today); test.frame=sprintf("%s::",today)
    p.c=na.omit(merge(lag(p[,pi],-futLAG), feature.leads, p)) #die Cloud: erste Spalte wie immer Y
    #geht auch beta als target
    #p.c=merge(lag(p.betas[,pi],-futLAG),feature.leads, p,p.betas) #die Cloud: erste Spalte wie immer Y
    firstDate=index(first(na.omit(p.c)))  #evtl. gibts wg. Indikator-Rechnungen langes leading NA-stück das abgetrennt wird
    train.frame=sprintf("%s::%s",firstDate,today)
    #evtl. wurden monats-daten mit tagedaten gemischt - jetzt werden die monatsdaten gestreckt
    p.c = m.ifna.prev(p.c[sprintf("%s::",firstDate)])
    
    Y=p.c["2008::2009",1]
    mchart(merge(Y,lag(Y,-10)),main="target")
    #mPlot(p.betas)
    trainingPredictors = coredata(p.c[train.frame,-1])
    trainingOutcome=     coredata(p.c[train.frame,1])#coredata(lag(p[train.frame,pi],-2))
    #mPlots(p.c[train.frame],title=colnames(trainingOutcome))
    
    dim(trainingPredictors)
    testPredictors = coredata(p.c[test.frame,-1])
    testOutcome=     coredata( lag(p.c[test.frame,1],futLAG))#coredata(lag(p[train.frame,pi],-2))
    
    dim(testPredictors)
    testOutcome.xts = xts(testOutcome, index(p.c[test.frame,1]))
    #mPlots(p.c[train.frame],title=colnames(trainingOutcome))
    
    #testOutcome.s= data.frame(apply(testOutcome,2,FUN=scale))
    #........ scaled-versions
    
    trainingPredictors.s=data.frame(apply(trainingPredictors,2,FUN=scale))
    trainingOutcome.s=data.frame(trainingOutcome)
    
    testPredictors.s=data.frame(apply(testPredictors,2,FUN=scale))
    testOutcome.s=data.frame(testOutcome)
    testOutcome.s.xts = xts(testOutcome.s, index(p.c[test.frame,1]))
    
    df.train.s = data.frame(cbind(trainingOutcome.s,trainingPredictors.s)) ; 
    fm=lmFormular(DF=df.train.s,1)
    df.test.s = data.frame(testPredictors.s) ; 
    
    ###############################
    df =data.frame(coredata(cbind(trainingOutcome,trainingPredictors)));  fm=lmFormular(DF=df,1)
    colnames(df)
    #df=data.frame(apply(df,2,FUN=scale))
    dim(testPredictors)
    dim(trainingPredictors)
    
    fit= eval(parse(text=sprintf("lm(%s,data=df)",fm)))  #eval(parse .. ist wichtig damit später stepAIC funktioniert
    
    pred=predict(fit,data.frame(testPredictors)  )
    fehler = sqrt(mean((pred - testOutcome)^2,na.rm=T))
    mchart(merge(xts(testOutcome,index(p.c[test.frame])),pred),main=sprintf("alle p - ohne lag - fehler %.2f",fehler)); amark(today)
    
    sort.data.frame( varImp(fit),"Overall",decreasing=T)
    
    #variablen-reduzierung stepAIC() ist weniger schädlich wie die mit findCorrelation
    step <- stepAIC(fit, direction="both",trace=F)
    pred=predict(step,data.frame(testPredictors)  )
    fehler = sqrt(mean((pred - testOutcome)^2,na.rm=T))
    mchart(merge(xts(testOutcome,index(p.c[test.frame])),pred),main=sprintf("alle p - ohne lag - fehler %.2f",fehler)); amark(today)
    sort.data.frame( varImp(step),"Overall",decreasing=T)
    
    print(step$anova)
    #  print(  step$anova$Step )
    sym.removed=c(na.omit(sapply(step$anova$Step,prettyWpName2)));names(sym.removed)=NULL
    mP("von %d wurde %d  Variablen von stepAIC gelöscht",ncol(trainingPredictors),len(sym.removed))
    #oder auch mit
    #rownames(varImp(fit))[!rownames(varImp(fit)) %in% rownames(varImp(step))]
    
    #################################################################################
    
    df =data.frame(coredata(na.omit(merge(p[train.frame,pi],p[train.frame,-pi]))));  fm=lmFormular(DF=df,1)
    #df=data.frame(apply(df,2,FUN=scale))
    
    fit= fit1=lm(fm,data=df)
    #    fit=lm(DAX ~ AEX_GENERAL + ATX + CAC40 + FTSE_100 + SG2R + SV2R + SWISS_MARKET + SX3BP + SX4BP + SX5R + SX6BP + SX7BP + SX86BP + SX8BP + SXABP + SXDBP + SXEBP + SXFBP + SXIBP + SXKBP + SXMBP + SXNBP + SXOBP + SXPBP + SXQBP + SXRBP + SXTBP + USDCHF + USDEUR + USDGBP + SV2R.lead.1 + SX7BP.lead.5 + SXABP.lead.18 + SXABP.lead.2 + SXMBP.lead.5,data=df)
    
    pred=predict(fit,data.frame(p[test.frame,-pi])  )
    fehler = sqrt(mean((pred - testOutcome)^2))
    mchart(merge(p[test.frame,pi],pred),main=sprintf("alle p - ohne lag - fehler %.2f",fehler)); amark(today)
    varImp(fit)
    
    ###############################
    
    df =data.frame(coredata(na.omit(merge(p[train.frame,pi],lag(p[train.frame,-pi],10)))));  fm=lmFormular(DF=df,1)
    fit= fit1=lm(fm,data=df)
    pred=predict(fit,data.frame(p[test.frame,-pi])  )
    fehler = sqrt(mean((pred - testOutcome)^2))
    mchart(merge(p[test.frame,pi],pred),main=sprintf("alle p - lag 10, fehler %.2f",fehler)); amark(today)
    varImp(fit)
    
    ##########################
    
    df =data.frame(coredata(na.omit(merge(p[train.frame,pi],feature.leads[train.frame]))));  fm=lmFormular(DF=df,1)
    colnames(df)
    fit= fit1=lm(fm,data=df)
    pred=predict(fit,data.frame(feature.leads[test.frame])  )
    fehler = sqrt(mean((pred - testOutcome)^2))
    mchart(merge(p[test.frame,pi],pred),main=sprintf("nur die %d leads, fehler %.2f",ncol(feature.leads),fehler)); amark(today)
    varImp(fit)
    
    ##########################
    colnames(itp)=sprintf("%s_itp",colnames(itp))
    df =data.frame(coredata(na.omit(merge(p[train.frame,pi],itp[train.frame],feature.leads[train.frame]))));  fm=lmFormular(DF=df,1)
    colnames(df)
    fit= fit1=lm(fm,data=df)
    pred=predict(fit,data.frame(merge(itp[test.frame],feature.leads[test.frame])  ))
    fehler = sqrt(mean((pred - testOutcome)^2))
    mchart(merge(p[test.frame,pi],pred),main=sprintf("nur die %d leads, fehler %.2f",ncol(feature.leads),fehler)); amark(today)
    varImp(fit)
    
    
    ##########################
    lagN=10
    df =data.frame(coredata(na.omit(merge(p[train.frame,pi],lag(p[train.frame,-pi],lagN),feature.leads[train.frame]))));  fm=lmFormular(DF=df,1)
    fit= fit1=lm(fm,data=df)
    pred=predict(fit,data.frame(merge(lag(p[test.frame],lagN),feature.leads[test.frame])  ))
    fehler = sqrt(mean((pred - testOutcome)^2,,na.rm=T))
    mchart(merge(p[test.frame,pi],pred),main=sprintf("alle p lag %d und die %d leads, fehler %.2f",lagN,ncol(feature.leads),fehler)); amark(today)
    varImp(fit)
    ###########################
    #tslars ... taugt wenig weil er eine einzige pronose macht ..
    
    y=coredata(p[train.frame,pi]); x=coredata(p[train.frame,-pi])
    library(tslars)
    fit=tslars(y~x,h=len(testOutcome))
    wichtigeVariablen=c(colnames(p[,pi]),colnames(fit$predictors))[fit$active[1:fit$nrvar.opt]+1]
    
    print(wichtigeVariablen)
    ####################################################################
    #http://www.jstatsoft.org/v34/i12/paper
    if (F)
    {
      library(glmulti)  #zeitfesser
      
      df =data.frame(coredata(na.omit(merge(p[train.frame,pi],p[train.frame,-pi]))));  fm=lmFormular(DF=df,1)
      fm="DAX ~ AEX_GENERAL * ATX * CAC40 * FTSE_100 * SG2R + SV2R + SWISS_MARKET + SX3BP + SX4BP + SX5R + SX6BP + SX7BP + SX86BP + SX8BP + SXABP + SXDBP + SXEBP + SXFBP + SXIBP + SXKBP + SXMBP + SXNBP + SXOBP + SXPBP + SXQBP + SXRBP + SXTBP + USDCHF + USDEUR + USDGBP"
      fit= glmulti(fm,data=df,fitfunc=lm,crit=bic,method = "g")
      
      #bestmodel <- fit$@formulas[1]
      #importances <- summary(fit)$termweights
      
      
      
      library(glmnet)   #versteht ich nicht...
      fit=glmnet(x,y,family="mgaussian")
      plot(fit,type.coef="2norm")
      pred=predict(fit,newx=coredata(p[test.frame,-pi]))
      dim(pred)
      fehler = sqrt(mean((pred - testOutcome)^2,,na.rm=T))
      mchart(merge(p[test.frame,pi],pred),main=sprintf("alle p lag %d und die %d leads, fehler %.2f",lagN,ncol(feature.leads),fehler)); amark(today)
      varImp(fit)
    }
    #....................................................      
    
    # Other useful functions
    coefficients(fit) # model coefficients
    confint(fit, level=0.95) # CIs for model parameters
    fitted(fit) # predicted values
    residuals(fit) # residuals
    anova(fit) # anova table
    vcov(fit) # covariance matrix for model parameters
    influence(fit) # regression diagnostics 
    varImp(fit) #variablen importance
    
    ####################################################################      
    #http://www.statmethods.net/stats/regression.html
    # Stepwise Regression
    library(MASS)
    df =data.frame(coredata(na.omit(merge(p[train.frame,pi],p[train.frame,-pi]))));  fm=lmFormular(DF=df,1)
    #fit= fit1=lm(DAX ~ .,data=df)    #hier muss ne Formel stehen - kein string .. sonst bug
    fit =eval(parse(text=sprintf("lm(%s ~ .,data=df)",colnames(df)[1])))
    
    pred=predict(fit,data.frame(coredata(na.omit(p[test.frame,-pi]))))
    fehler = sqrt(mean((pred - testOutcome)^2))
    mchart(merge(p[test.frame,pi],pred),main=sprintf("alle p , fehler %.2f",fehler)); amark(today)
    
    varImp(fit)
    
    #variablen-reduzierung stepAIC() ist weniger schädlich wie die mit findCorrelation
    step <- stepAIC(fit, direction="both",trace=T)
    step$anova # display results 
    pred=predict(step,data.frame(coredata(na.omit(p[test.frame,-pi]))))
    fehler = sqrt(mean((pred - testOutcome)^2,,na.rm=T))
    
    
    mchart(merge(p[test.frame,pi],pred),main=sprintf("via step reduzierte p , fehler %.2f",fehler)); amark(today)
    
    print(step$anova)
    print("gelöschte Variablen")
    print(      step$anova$Step )
    
    ## etwas arg grobe lösch-methode ....
    library(caret)
    descrCorr <- cor(na.omit(df))# die eingentlich - trainings-daten  
    #ist wahrscheinlich nicht sehr gut ... versuche andere Variablen Indentifizierer
    highCorr <- findCorrelation(descrCorr, 0.985); print(highCorr)
    colnames(df[,highCorr])
    df =df[,-c(highCorr)]
    
    fit =eval(parse(text=sprintf("lm(%s ~ .,data=df)",colnames(df)[1])))
    
    pred=predict(fit,data.frame(coredata(na.omit(p[test.frame,-pi]))))
    fehler = sqrt(mean((pred - testOutcome)^2))
    mchart(merge(p[test.frame,pi],pred),main=sprintf("alle p , fehler %.2f",fehler)); amark(today)
    
    
    #......................................
    
    
    ################################################################################
    
    library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
    #M5P  ok  aber ohne varImp()
    df = data.frame(cbind(trainingOutcome,trainingPredictors)); 
    fit =eval(parse(text=sprintf("M5P(%s ~ .,data=df)",colnames(df)[1])))
    print(fit) # view results
    #
    pred=predict(fit,data.frame(testPredictors)  )
    fehler = sqrt(mean((pred - testOutcome)^2))
    mchart(merge(xts(testOutcome,index(p.c[test.frame])),pred),main=sprintf("fehler %.2f",fehler)); amark(today)
    ls(fit)
    
    #########################################################################
    #cubist:  noch besser wie M5P
    #http://cran.r-project.org/web/packages/Cubist/vignettes/cubist.pdf
    library(Cubist)
    
    cfit=cubist(x=trainingPredictors,y=trainingOutcome,committees=50)  #50 besser, 20 gehen auch
    pred=predict(cfit,testPredictors , neighbors=5 )
    fehler = sqrt(mean((pred - testOutcome)^2,na.rm=T))
    mchart(merge(xts(testOutcome,index(p.c[test.frame])),pred),main=sprintf("fehler %.2f",fehler)); amark(today)
    #variablen wichtigkeit
    varImp(cfit)
    
    cfit$usage
    summary(cfit)
    
    #einmalig die richtigen trainings-parameter committees und neighbors finden
    #trainiere mit caret durch verändern der parameter
    if (F)
    {
      library(caret)
      set.seed(1)
      p=na.omit(p)
      
      cTune <- train(x = trainingPredictors, y = trainingOutcome,
                     "cubist",
                     tuneGrid = expand.grid(.committees = c(1, 10, 50, 100),
                                            .neighbors = c(0, 1, 5, 9)),
                     trControl = trainControl(method = "cv"))
      cTune  #anzeigen der tuning-results
      plot(cTune)     
    }
    ##############################################
    library(e1071)
    
    fit=svm(x=trainingPredictors,y=trainingOutcome)  #50 besser, 20 gehen auch
    pred=predict(fit,testPredictors  )
    fehler = sqrt(mean((pred - testOutcome)^2,na.rm=T))
    mchart(merge(xts(testOutcome,index(p.c[test.frame])),pred),main=sprintf("svm fehler %.2f",fehler)); amark(today)
    #variablen wichtigkeit
    #varImp(fit)
    
    
    ############################################
    
    if (F)
    {
      fit <-VAR(cbind(trainingOutcome,trainingPredictors),p=5,type="none")  #MM_TODO:  noch ander  VAR-type ausprobieren "trend","both", ...
      predict(fit,n.ahead=1)
      #pred <- predict(fit$varresult, data.frame(testPredictors))  #anderes Signifikanz-Intervall 
      
      
      args('ca.jo')
      vecm <- ca.jo(dat1, type = 'trace', K = 2, season = NULL,spec="longrun",dumvar=NULL)
      vecm.var <- vec2var(vecm,r=2)
      
    }
    #################################################################
    library(randomForest) #ok
    ### Center and scale variables
    #x <- scale(diabetes$x)
    df = data.frame(cbind(trainingOutcome,trainingPredictors)); 
    fit =eval(parse(text=sprintf("randomForest(%s ~ .,maxnodes=15,importance=T,data=df)",colnames(df)[1])))
    print(fit) # view results
    rn=importance(fit) # importance of each predictor 
    wichtigkeit =rn[order(rn[,1], decreasing=TRUE),]
    print(wichtigkeit)      
    #
    pred=predict(fit,testPredictors  )
    fehler = sqrt(mean((pred - testOutcome)^2))
    mchart(merge(xts(testOutcome,index(p.c[test.frame])),pred),main=sprintf("fehler %.2f",fehler)); amark(today)
    #variablen wichtigkeit
    #rn=varImp(fit)
    ################################
    
    
    #supersuper
    df = data.frame(cbind(trainingOutcome,trainingPredictors)) ; fm=lmFormular(DF=df,1)
    fit=lm(fm,data=df)
    pred=predict(fit,data.frame(testPredictors))
    fehler = sqrt(mean((pred - testOutcome)^2,na.rm=T))
    mchart(merge(xts(testOutcome,index(p.c[test.frame])),pred),main=sprintf("fehler %.2f",fehler)); amark(today)
    #variablen wichtigkeit
    varImp(fit)
    ####################
    if (F)
    {
      #Fit neural net
      #http://www.di.fc.ul.pt/~jpn/r/neuralnets/neuralnets.html
      
      df.train.s = data.frame(cbind(trainingOutcome.s,trainingPredictors.s)) ; fm=lmFormular(DF=df.train.s,1)
      df.test.s = data.frame(testPredictors.s) ; 
      
      df.train = data.frame(cbind(trainingOutcome,trainingPredictors)) ; fm=lmFormular(DF=df.train,1)
      df.test = data.frame(testPredictors) ; 
      
      
      fit <- eval(parse(text=sprintf("caret::train(%s, df, method='nnet', linout=TRUE, trace = FALSE)",fm)))
      
      fit <- eval(parse(text=sprintf("nnet(%s,df.train.s,size=3)",fm)))
      pred <- predict(fit,df.test.s )
      fehler = sqrt(mean((pred - testOutcome.s)^2))
      mchart(merge(testOutcome.s.xts,pred),main=sprintf("neural net fehler %.2f",fehler)); amark(today)
      #--------------------------
      #klappt --- aber nicht gut
      library(neuralnet) 
      fit <- eval(parse(text=sprintf("neuralnet(%s, df.train.s, hidden=3, threshold=0.01)",fm)))
      print(fit); plot(fit)
      net.results <- compute(fit, df.test.s) #Run them through the neural network
      pred=net.results$net.result
      fehler = sqrt(mean((pred - testOutcome.s)^2))
      mchart(merge(testOutcome.s.xts,pred),main=sprintf("neural net fehler %.2f",fehler)); amark(today)
      
    }
    ###################
    
    aic=  AIC(fit1,fit,fit2)
    
    #descrCorr <- cor(na.omit(feature.leads))# die eingentlich - trainings-daten
    descrCorr <- cor(na.omit(trainingPredictors))# die eingentlich - trainings-daten  
    #ist wahrscheinlich nicht sehr gut ... versuche andere Variablen Indentifizierer
    highCorr <- findCorrelation(descrCorr, 0.985); print(highCorr)
    
    if (len(highCorr)>0)
    {
      trainingPredictors =trainingPredictors[,-c(highCorr)];dim(trainingPredictors)
      dim(trainingPredictors)
      print(colnames(trainingPredictors))
      df = data.frame(cbind(trainingOutcome,trainingPredictors)) ; fm=lmFormular(DF=df,1)
      fit2=lm(fm,data=df)
      pred=predict(fit2,data.frame(testPredictors))
      fehler = sqrt(mean((pred - testOutcome)^2))
      mchart(merge(p[test.frame,pi],pred),main=sprintf("fehler %.2f",fehler)); amark(today)
      #variablen wichtigkeit
      varImp(fit2)
      
      
      aic=  AIC(fit1,fit) #the smaller the AIC or BIC, the better the fit.
      if (aic[2,2] < aic[1,2] )  #ausdünnen ist vorteilhaft
      {
        leads[[sym]]=feature.leads
        mP("reduce %d features for %s",len(highCorr),sym)
        browser()
      }
    }
  }
}
leads
}

multi.var.reg<-function(p, features=NULL, method="lm")
{
  for (pi in 1:ncol(p))
  {
    sym=colnames(p[,pi])
    features=p[,-pi]
    leads[[sym]]=feature.leads
  }
  
  if (method =="VAR")
  {
    EuStockMarkets=p
    var_est <- VAR(EuStockMarkets, p=30, type="both",lag.max=43,ic="AIC")
    summary(var_est)
    plot(var_est)
    pred=predict(var_est,n.ahead = 10, ci = 0.95, dumvar = NULL)
    plot(forecast(var_est,h=200))
    #hier kann man die optimalen lead-beziehungen abgreifen:
    VARselect(EuStockMarkets[,c(2,3)], lag.max = 30, type = "both",season = NULL, exogen = NULL)$selection
  }
}
#we have 50 symbols:

# "bad Data"

if (F)
{
  #.................................  
  library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
  crs$j48 <- J48(as.factor(crs$Target) ~ .,    data=crs$tdata)
  summary(crs$j48)
  check.classifier(crs,crs$j48)
  #................................
  library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
  crs$m5p <- M5P(as.factor(crs$Target) ~ .,    data=crs$tdata)
  summary(crs$m5p)
  check.classifier(crs,crs$m5p)
  #................................
  library(rpart)  
  #http://www.statmethods.net/advstats/cart.html
  crs$rpart <- rpart(as.factor(crs$Target) ~ .,
                     data=crs$tdata,
                     method="class",
                     parms=list(split="information"),
                     control=rpart.control(usesurrogate=0, 
                                           maxsurrogate=0))
  check.classifier(crs,crs$rpart)
  # prune the tree
  fit=crs$rpart
  crs$rpart<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  check.classifier(crs,crs$rpart)  #prune hat nur wenig geholfen
  
  library(rpart.plot) #http://www.milbo.org/rpart-plot/
  prp(crs$rpart)
  # Eine Textansicht des Modells Entscheidungsstruktur erstellen
  summary(crs$rpart)
  
}


#############################################

if (F)   #parameter identifi by stepping
{
  library(bootStepAIC)
  n <- 350
  x1 <- runif(n, -4, 4)
  x2 <- runif(n, -4, 4)
  x3 <- runif(n, -4, 4)
  x4 <- runif(n, -4, 4)
  x5 <- runif(n, -4, 4)
  x6 <- runif(n, -4, 4)
  x7 <- factor(sample(letters[1:3], n, rep = TRUE))
  y <- 5 + 3 * x1 + 2 * x2 - 1.5 * x3 - 0.8 * x4 + rnorm(n, sd = 2.5)
  data <- data.frame(y, x1, x2, x3, x4, x5, x6, x7)
  rm(n, x1, x2, x3, x4, x5, x6, x7, y)
  
  lmFit <- lm(y ~ (. - x7) * x7, data = data)
  ls(lmFit)
  boot.stepAIC(lmFit, data)
  stepAIC(lmFit,data)
  
  #########################
  
  library(relaxo)
  data(diabetes)
  ## Center and scale variables
  x <- scale(diabetes$x)
  y <- scale(diabetes$y)
  ## Compute "Relaxed Lasso" solution and plot results
  object <- relaxo(x,y)
  dim(x)
  dim(y)
  ls(object)
  dim(object$beta)
  plot(object)
  ## Compute cross-validated solution with optimal
  ## predictive performance and print relaxation parameter phi and
  ## penalty parameter lambda of the found solution
  cvobject <- cvrelaxo(x,y)
  print(cvobject$phi)
  print(cvobject$lambda)
  ## Compute fitted values and plot them versus actual values
  fitted.values <- predict(cvobject)
  plot(fitted.values,y)
  abline(c(0,1))
  
  
  ############################################
}
#######################################################################
library(e1071) #für svm()

signal.multi.VAR <-function(arg, par = mlist( wlen=c(400,100,800,50)),visual=F,...)
{
  mode="VAR"  #"VAR"
  p=arg$clos
  wlen = par$wlen
  sym=colnames(p)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=arg$dat$prices
  #sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  #data=arg$dat
  #30Tage-returns mit multi VAR forecasten
  # jeder Tag entspricht der differenz heute zum vormonats-tag
  #Input-Daten (erste spalte enthält das Target)
  mp = na.omit( bt.apply.matrix(p["2000::"],ROC,n=30))*100
  if (mode=="SVM")
  {
    #noch die HotLags dazu geben
    mp= merge(mp,arg$dat$hot.leads)
    #clouding 
    slope200=rollRegressionXTS(p,win=200)*100
    slope200 = bt.apply.matrix(slope200, cutInterval, mi=-30,ma=30)
    mp= merge(mp,slope200)    
    
    #  mp[,1] = lag(mp[,1], -1)  #target  
    mp=na.omit(mp)
  }
  print(dim(mp))
  print(colnames(mp))
  train.i= data$m.ends[24:len(data$m.ends)]
  
  #train.i= (wlen+1):(wlen+3)#TEST
  train.i= (wlen+1):(nrow(mp)-1) #-1 ??
  mP("number of daily %s-training-days %d",mode,len(train.i))
  #jeden monat neu trainieren auf tagedaten - 2 jahre vorne luft lassen
  
  Pred=  lapply(train.i,function(today.i)
  {
    # today.i=499  #TEST
    today=index(mp[today.i])
    print(today)
    mp.now = mp[ max(1,today.i-wlen) : today.i,]
    
    if (mode =="VAR")
      temp=VAR(ts(mp.now,356)) #356 weil tagesdaten  --TODO hier noch die order schätzen
    else
      if (mode=="SVM")
      {
        colnames(mp.now)[1]="y"
        temp=svm(y~.,coredata(mp.now),type="eps-regression") 
      }
    
    if(inherits(temp, 'try-error')) 
    {
      mP("bug at %s",DatesS(today))
      res=last.res
    }
    else
    {
      fit=temp
      #if (today==as.Date("2013-12-12"))
      #  browser(mP("mist"))
      #kursänderung in einem monat vorhersagen
      
      if (mode=="VAR")
      {
        pred=predict(fit,n.ahead = 1, ci = 0.95, dumvar = NULL)    
        res=m.xts(data.frame(pred$fcst[1]),today)
      }
      if (mode=="SVM")
      {
        #crash bei 2013-12-13"
        
        #svm nutzt ausschliesslich die heutigen datenzeile
        #mp.next = last( mp[ max(1,today.i+1-wlen) : (today.i+1),])
        target  =last(mp.now)[,1]
        mp.now[,1]=0  #target-spalte sicherheitshalber leeren
        pred = predict(fit,newdata=coredata(last(mp.now)))
        if (len(pred)==0) pred=0
        res=m.xts(pred,today)     
        colnames(res)=c("pred")
      }
      last.res <<-res
    }
    
    res
  })
  # toc()
  browser(mP("fertig"))
  #xts-list 2 xts
  pred=do.call("rbind",Pred);
  all=na.omit(merge(mp[,1],pred,data$prices[,1]))
  mchart(all)
  
  if (mode == "SVM")
  {   
    signal=sign(all[,1]); target=sign(lag(all[,2],-1)); p= all[,3]
    class.pred <- table(signal,target)
    pred.err=1-sum(diag(class.pred))/sum(class.pred)
    print(class.pred)
    print(pred.err)
    new_Win(2);
    plot(p)
    sm=SMA(na.omit(all[,2]),100);signal=sign(sm);  
    #signal=sign( rollRegressionXTS(na.omit(sm),win=60))  
    #kontrolle:   mit sm == target und global_commission=0 - sollte der return durch die Decke krachen ...  - tut er nicht !! ->design-bug
    sm=na.omit(target);signal=sign(sm);  
    
    mPlots(p,merge(all[,1],0), merge(sm,0,3*signal))
    
  }
  if (mode =="VAR") {
    norm_Win(2);plot(Pred[,4]) #modell sicherheit
    #schauen ob upper und lower in die gleiche richtung zeigen wie fcst - sonst NA
    signal = iif (sign(pred[,1])==sign(pred[,2]) & sign(pred[,2])==sign(pred[,3]),sign(pred[,1]),NA) 
  }
  #bei uneinigkeit flat gehen
  #signal[is.na(signal)] = 0
  #bei uneinigkeit in alter pos bleiben
  signal = m.ifna.prev(signal)
  #vieleicht läßt sich über (pred[,4]) noch weiter filtern ..
  #TODO
  b= na.omit(merge(signal,data$prices[,1]))
  plotSigPrice(signal=b[,1],prices=b[,2])
  mP("fertig")
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}

if (F)
{  
  
  ls(data$macros)
  #bib die hot.leads nach data
  global_arg$dat$hot.leads =  HotLags2.CC(p=global_arg$dat$prices[,1],features =global_arg$dat$prices)
  
  tic() 
  x=indi.Generic("signal.multi.VAR", global_arg, par=list(wlen=400),visual=T, TRAINSYM = data$BENCH)
  toc()
}

####################################################################
#  models$x1.2= x1.2=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=200,glaettung2=50,dw=0),xarg=list(fn="SMA",fn2="EMA",onlyLong=T,q.w="70%"),visual=visual, TRAINSYM =-1) ;indi.Results(x1.2)


#siee auch 'MLib/InputConfig_Portfolio_TD4.R'
#lm.gbm<-function(p,visual=F,main="gbm",glaettung=0,...)
#lm.smooth.spline<-function(p,visual=F,getIndex=T,main="loess",glaettung=0,...)
#lm.gamm<-function(p,visual=F,getIndex=T,main="gamm",glaettung=0,...)
#g.Signal.D.zlema
#######################################################################################
mP("########### load multiSignal.r")
if (F)
  list_R_functions('MLib/multSignal.r')
