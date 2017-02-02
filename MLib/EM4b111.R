################################################################################################################
# 2013-11-28 
#Lade Eckhards e4  xls daten  nach xts pro sheet 

###########################################################################################....................................... Experimente definieren ..........................
if (F)
  run.main()


run.main<-function ()
{
  prepare.EM4B.data(file="em4b.data", do.load=T,do.par=F, mkTrain=T,visual=F)
  #  ,            techbuilder.symbols=colnames(data$prices)       )
  #if (F)
  #   save(data,file="em4b.data")
  experiment=define.experiments()
  
  k=paste(colnames(data$multiTarget[[1]]),collapse=", ")
  if (F)
  {
    TrainRun_EM4B(Experi=experiment$TEST , do.par=F)
    TrainRun_EM4B(Experi=experiment$TEST , do.par=T)
    TrainRun_EM4B(Experi=experiment$TEST , do.par=T)
    TrainRun_EM4B(Experi=experiment$macro.select , do.par=T)
  }
  TrainRun_EM4B(Experi=experiment$macro.select.m , do.par=T)
  
  data$train.best=auswertung(Experi=experiment$macro.select.m,data) 
  
  # als Vektor gehts hier in die monatliche Gewichtsberechnung
  
  r=data$prices
  r=m.to.monthly(r)
  
  r=foreach(col = colnames(data$prices),.combine="merge") %do%
{
  Col = r[,col]; diff(log(Col))
}
  r=r[experiment$macro.select.m$backtest.frame]
  purePlot(mLogRendite(r))
  #m.tslars(r)
  weights= rollGenSA.Portfolio(r,win=3,mode ="maxcalmar")  ### << test.me
  
  turnover.1=weights[,1];turnover.1[]=NA;
  turnover.1[]=(rowSums(abs(weights-lag(weights))))
  apply.yearly(turnover.1, FUN=sum)*100
  
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
define.experiments<-function()
{
  experiment=list(
    TEST= list(
      path="TEST",
      symbolnames="EXX50_RI",   #data$BENCH,
      backtest.frame="::2003-05-31",
      Target.fn= select.multiTarget.sum,
      train.data=data$test.cloud,
      retrain.on= "yearly",
      par=list()
    ),
    
    macro.select= list(
      path="EM4_DEC.1",
      symbolnames="EXX50_RI",   #data$BENCH,
      backtest.frame="::2013-05-31",
      Target.fn= select.multiTarget.sum,
      train.data=data$cloud.mac,
      retrain.on= "quaterly",
      par=list()
    ),
    
    macro.select.m= list(
      path="EM4_DEC.1",
      symbolnames="EXX50_RI",   #data$BENCH,
      backtest.frame="2007-01-01::2013-05-31",
      Target.fn= select.multiTarget.sum,
      train.data=data$cloud.mac,
      retrain.on= "monthly",
      par=list(),
      INFO=c(" ","select.multiTarget.sum =>", paste(names(data$multiTarget),collapse=", "),            "Target =>",paste(colnames(data$multiTarget[[1]]),collapse=", "),
             "train.data =>"," ",paste(as.character(dim(data$cloud.mac)),collapse=" : ")
             ," ", paste(colnames(data$cloud.mac),collapse=", "))
    ),
    
    macro.select.m2= list(
      path="EM4_DEC.1",
      symbolnames="EXX50_RI",   #data$BENCH,
      backtest.frame="2005-01-01::2013-05-31",
      Target.fn= select.multiTarget.sum,
      train.data=data$cloud.mac,
      retrain.on= "monthly",
      info="cloud.mac on",
      par=list()
    ),
    
    justTech.m= list(
      path="EM4_DEC.1",
      symbolnames="EXX50_RI",   #data$BENCH,
      backtest.frame="::2013-05-31",
      Target.fn= select.multiTarget.sum,
      train.data=get.techCloud,
      retrain.on= "monthly",
      par=list()
    )
  )
}

#Tagesdaten oder wochen daten ? -> winlen im cloud-maker anppassen 
#tagesdaten -> sind die indikatore  für die preise schnell ?
#auswerten mit signal oder mit non-lin-optimierer
prepare.EM4B.data <-function(file="em4b.data", do.load=T,do.par=T,mkTrain=T,techbuilder.symbols=c(),visual=F)
{
  if (do.load)
  {
    mP("load data from %s",file)
    
    local({
      load(file=file)     #sichern falls er crashed
      print(ls(data))
      data<<-data
      global_StartDate
      define.Globals(ver=1)
    })
    if (do.par)
      prepare_Parallel()
    return("ok")
  }
  data <<-make_eckhard4B_data(visual=visual,bench="EXX50_RI",do.par=do.par,mkTrain=F)
  cloud.check(data$prices)
  #MMDATA
  #monats-daten:  die macros 
  data$macros = na.omit(merge(data$macros,data$prices))  #wir nehmen noch die euro.indizes als speudo-macros mit auf .. wer wei?
  cloud.check(data$macros)
  
  #MMCLOUD  
  #aus den folgenden Bausteinen bau ich mir spaeter meine sym-individuelle datacloud
  #macro-cloud berechen  - bei visual = T wird auch jeder variablenblock ge cloud.checked() !!
  
  #auf tagesbasis
  data$cloud.mac <- cloud.mac <<- small.cloud(data$macros,visual=F,cloud.control= list(get.Quantile.M=F)) 
  #small-cloud für mac incls preise
  #passt die cloud Qualität ?  
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
  
  #Baue in   data->tech.cloud[[sym]]  die sehr aufwändige tech-cloud   für das zu prognostizierende objekt
  if (len(techbuilder.symbols) > 0)
    tech.cloud.builder(techbuilder.symbols,tech.Cloud.Fn= "tech.Cloud", data,visual=F,cloud.control=spl("heavy,lagged"),do.par=do.par)
  
  #gem. unterschiedlichen zeitlichen auflösungen und mehtoden targets bauen
  if (mkTrain)
    data$multiTarget <-  multiTarget(visual=visual) #das target eines symbols bekommst Du mit mt=select.multiTarget.sum(data, sym) oder mt=select.multiTarget (data, sym)
  #data$target=select.multiTarget.sum(data,data$BENCH)
  
  ls(data$multiTarget)
  #passt die target Qualität ?
  lapply(data$multiTarget ,cloud.check)
  #..................................................................................<<
  
  save(data,file=file)     #sichern falls er crashed
  
  if (do.par)
  {
    mP("sfExport data")
    sfExport("data")
  }
}


#######################################################################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
################################################################################################################


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
  #enhält folgende 3 logischen Gruppen
  exx50=exx50.all[,1]
  fundamentale.faktoren= exx50.all[,c(2:25)]
  intermarket.faktoren = exx50.all[,c(26:ncol(exx50.all))]
  
  colnames(exx50)
  colnames(fundamentale.faktoren)
  colnames(intermarket.faktoren)
  
  exx50.n =mNorm(exx50)["1997::"]
  #Die Fundamentalfaktoren haben einen time lag von 20 Tagen, die Inter-Market Faktoren haben keinen time lag. 
  fundamentale.faktoren.n= mNorm(lag(fundamentale.faktoren,20))["1997::"] #hier schon mal gelagged weil die Teile ja einen Monat Lieferversp?tung haben
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
  
  TSA.prepPro.e4B(visual=F,bench="EXX50_RI",do.par =T)
  
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
TSA.prepPro.e4B<-function(visual=T,bench="DAX30",do.par=T)
{
  
  if(F)
  {
    visual=T;bench="EXX50_RI";do.par=T
  }
  #..................................................................................................>>
  
  #daten einlesen, target-data berechnen und data-environment und global_arg vorbereiten - auch sfExport()
  data <-make_eckhard4B_data(visual=visual,bench=bench,do.par=do.par)
  #cloud-bildung parametrisieren
  #paralle vorbereiten
  
  #do.par=T
  if (do.par)  #nur in der Entwicklungsphase hilfreich...
  {
    prepare_Parallel() 
    
    sfSource("MLib/EM4B.R")
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
    load(file="em4b.data")
    prepare_Parallel() 
    #sfExport("data")
    
    ls(data);   dim(data$train.data[["EXX50_RI"]])
  }
  #forest-trainieren+evaluieren -> signale,rankings und variablenSelektion  berechen
  
  
  #................................................................................
  #der Start der Experiment-Runs
  TrainRun_EM4B(Experi=experiment$TEST , do.par=F)
  
  TrainRun_EM4B(Experi=experiment$macro.select , do.par=F)
  TrainRun_EM4B(Experi=experiment$macro.select.m , do.par=F)
  
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


TrainRun_EM4B<-function(Experi=experiment$macro.select , do.par=do.par)
{ 
  #MMRUN
  #  experi = Experi$experi
  experi=  deparse(substitute(Experi))  
  experi=rightOf(experi,"\\$")
  
  info=list.info(Experi,name= experi)
  cat(info,file=sprintf("%sExperimentInfo.txt",path),sep="\n")
  print(info)
  
  mP("..............TrainRun_EM4B  %s",experi)
  
  retrain.on =Experi$retrain.on
  symbolnames=Experi$symbolnames
  
  Target.fn= match.fun(Experi$Target.fn)
  train.data=Experi$train.data
  par=Experi$par
  backtest.frame=Experi$backtest.frame #"::2013-05-31"
  path= sprintf( "Models/%s/%s/", Experi$path,experi);    dir.create(path,recursive=T)
  all.sig=lapply(symbolnames,function(sym)
    #all.sig=sfLapply(symbolnames,function(sym)    #ich mach die parallelisierung jetzt eins tiefer, in signal.randomForest.e4()
  {
    #    sym=data$BENCH
    mP(">>TrainRun_EM4B %s %s",experi,sym)
    data$Target = Target.fn(data,sym)  #die Target-Daten des aktuellen symbols    
    if (is.character(train.data))
    { 
      train.data.fn = match.fun(train.data)
      train.data= train.data.fn(data,sym)
    }
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
    
    RES= signal.randomForest.e4b(arg,par,retrain.on=retrain.on,visual=F,do.par=do.par)
    
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

auswertung<-function(Experi=experiment$macro.select, bestN=30)
{
  experi=  deparse(substitute(Experi))  
  experi=rightOf(experi,"\\$")
  mP("..............auswertung zu  %s",experi)
  symbolnames=Experi$symbolnames
  path= sprintf( "Models/%s/%s/", Experi$path,experi);   
  
  
  #info=list.info(Experi,name= experi)
  #cat(info,file=sprintf("%sExperimentInfo.txt",path),sep="\n")
  
  #................................................................................
  Signal=list()
  Train.best=list()
  #MMAUSW
  #auswertung  .. wie ver?ndert sich die forest.fit.wichtigkeit  ... 
  RES=lapply(1:len(symbolnames),function(sym.i) 
  {
    #sym.i=1
    sym=symbolnames[sym.i]
    #ls(res[[1]]) #"confidence" "model.err"  "signal" 
    if(F)  #noch im speicher .
    {
      res = all.sig[[sym.i]]
      forest.fit.wichtigkeit=res[[sprintf("%s.forest.fit.wichtigkeit",sym)]]
      sig=  res[[sprintf("%s.sig",sym)]]
    }
    else #schon serialisiert
    {
      load(file=sprintf("%ssignal_randomForest_e4_%s",path,sym))
      sig=signal
      load(file=sprintf("%swichtigkeit_randomForest_e4_%s",path,sym))
      forest.fit.wichtigkeit = wichtigkeit
    }
    #browser(mP("###1"))
    #die trainings-qualit?t ?ber die zeit
    model.err = forest.fit.wichtigkeit[,1]
    mPlots(merge(sig[,1],0),sig[,2],model.err,title="signal+condfidence+model.err")
    purePlot(model.err, main=sprintf("model.err %s",sym))
    #die Variablen-Wichtigkeit ?ber die Zeit
    dim(forest.fit.wichtigkeit[,-1])
    #browser(mP("now"))
    purePlot(forest.fit.wichtigkeit,main=sprintf("forest.fit.wichtigkeit %s",sym))  
    #browser(mP("#A1"))
    wich=summary(forest.fit.wichtigkeit)
    
    wich=wich[,order(wich[4,],decreasing=T)  ]
    wich.col=colnames(wich)
    wich=t(wich);rownames(wich)=wich.col
    wich=wich[,c(1,3,4,6)]
    View(wich)
    utils::write.table(wich,file=sprintf("%swichtigkeit_%s.csv",path,sym),dec=".",sep=";",col.names=F,row.names=T)
    
    colMeans(forest.fit.wichtigkeit)
    #plotSigPrice(signal=sign(signal-1), prices=p[fromToS(signal)], indi=list(sollIst=merge(signal,target,0)))
    #mPlots(p[fromToS(target)],merge(target,0),abs(bug),title="target->signal")  
    
    b=na.omit( merge(sig[,1], data$prices[,sym]))
    Signal[[sym]]=sig[,1]
    plotSigPrice(signal=b[,1],prices=b[,2])#,indi=list(conf=merge(sig[,2])))
    
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
    # k2=k  #ohne Beschränkung auf FundamentalfaktorenQ
    # View(k2)
    k3= lapply(1:nrow(k2), FUN=function(row.i){spl(paste(k2[row.i,]))})
    #pro listenvektor ein vector mit den fundi-gewichten
    k4=lapply(1:len(k3),function(c1) sapply(k3[[c1]],function(x) df.q[c1,x]))
    #das faktor-gewicht soll wenigstens 4 sein
    K4=data.frame(do.call("cbind",k4))
    colnames(K4)=as.character(as.Date(index(k2)))
    View(K4)
    
    #doofes write.table .. muss hier selber die columnnames basteln
    K4=rbind(Dates=c(colnames(K4)),K4)
    
    utils::write.table(K4,file=sprintf("%sTop300Faktoren_%s.csv",path,sym),dec=".",sep=";",col.names=F,row.names=T)
    
    #k5=lapply(k4,function(c1)c1[c1 > 4]  )
    #k5=k4
    #die ListeNamen k5 als Datümer wählen
    #names(k5)<-index(df.q)
    #View(data.table(k5))
    
    #baue eine abgespecktes Trainings-Objekt dass nur noch die wichtigsten 
    #faktoren enthält
    if(bestN>0)
    {
      mP("make bestN %d train.set",bestN)
      ff=colMeans(forest.fit.wichtigkeit)   
      ff.best=head(ff[order(ff,decreasing=T)],bestN)
      target.fn=match.fun(Experi$Target.fn)
      Target= target.fn(data,sym)
      train.best=merge(Target,Experi$train.data[,names(ff.best)])
      utils::write.table(train.best,file=sprintf("%strainTop%d%s.csv",path,bestN,sym),dec=".",sep=";",col.names=F,row.names=T)
      Train.best[[sym]]=train.best
     
    }
  list(train.best=Train.best, signal=Signal)
   }
  )   
RES
}
########### wird so auch in MM_Main.R  mit universe = Eckhard4 geladen
#ein virtuelles data-env vorbereiten - damit das SIT-Framework laufen kann

make_eckhard4B_data<-function(visual=F,bench="EXX50_RI",do.par=T, mkTrain=F)
{
  #einlesen von xls -sheets in xts- variable
  exx50.all= read.EckhardsXLS(modelDir="EM4_DEC.1", xlsName="Data0E.xls",startRow=5,belowColnames=5, date.format = "%d.%m.%Y",to.month.end=F,debug=F)  
  
  
  dim(exx50.all)  #4392 Tage mit 39 Spalten
  #enhält folgende 3 logischen Gruppen
  exx50=exx50.all[,1]
  fundamentale.faktoren= exx50.all[,c(2:25)]
  intermarket.faktoren = exx50.all[,c(26:ncol(exx50.all))]
  
  colnames(exx50)
  colnames(fundamentale.faktoren)
  colnames(intermarket.faktoren)
  
  exx50.n =mNorm(exx50)["1997::"]
  #Die Fundamentalfaktoren haben einen time lag von 20 Tagen, die Inter-Market Faktoren haben keinen time lag. 
  fundamentale.faktoren.n= mNorm(lag(fundamentale.faktoren,20))["1997::"] #hier schon mal gelagged weil die Teile ja einen Monat Lieferversp?tung haben
  #head(merge(fundamentale.faktoren[,1],fundamentale.faktoren.n[,1]),30)
  intermarket.faktoren = mNorm(intermarket.faktoren)["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  
  
  #mkTrain macht hier nur ein univariates Target 
  data <-  make.data(na.omit(merge(exx50.n,intermarket.faktoren )), mkTrain=F,visual=T,bench=bench)
  data$macros=fundamentale.faktoren
  data$betas=beta.again.portfolio(data) #die monats-betas der data$prices gegen?ber ihrem gleichgewichteten portfolio
  mchart(data$betas)
  data$crs=new.env() #f?r den forest
  data$train.data = new.env()  #die dataCloud - gecashed
  #noch zusÃ¤tzlich den ifo dazu
  if  (is.null(data$ifo))
    data$ifo =  try(load.IFO(visual=T))
  
  
  data$BENCH <-BENCH <-global_arg$BENCH <-bench
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission<<-0.0015   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(data$prices)) 
  global_objectId <<-paste("TREND","xx","signal.lm") 
  global_FastTrain <<-20 #hei?t:  f?r jede dim werden 2 Schritte ausprobiert.
  
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
  
  ret.p = mROC(p[,sym])
  normP = mNorm(p[,sym])
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



signal.randomForest.e4b <-function(arg, par = mlist( sma.w=c(200,120,350,10)),retrain.on="quarterly",visual=F,do.par=T,...)
{
  mP("signal.randomForest.e4b")
  #Vorausetzung in arg$dat$Target liegen pro price-Symbol TargetTrainingsdaten
  mP("##1")
  sym=colnames(arg$clos)
  train.data = arg$dat$train.data  
  p=mNorm(arg$clos)
  today=as.Date(last(index(p)))
  target=na.omit(arg$dat$Target)
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
    features = na.omit(arg$dat$train.data)   #chashe
  
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
  #bauen ein P=p der nur dort Werte enhält an denen es auch priceFeatures (ab Col 2) gibt
  P= na.omit(merge(p[firstUsefulDateS],priceFeatures[firstUsefulDateS,2]))[,1]
  mP("roll for %d days",shape(P)-1000)
  
  if (shape(P)<1000)
  {    
    sag("Sorry - need at last 1001 data",warte=T)
    stop
  }
  
  if (do.par)  
    sig.value.list=sfLapply(1000:shape(P), function(p.i) roll.classifier.e4b(P ,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs,p.i=p.i)) 
  else
    sig.value.list=lapply(1000:shape(P), function(p.i) roll.classifier.e4b(P ,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs,p.i=p.i)) 
  
  
  mP("hol results")
  # browser()
  #ergebnisse aus er liste holen
  sig.value=foreach(i= sig.value.list,.combine="rbind") %do%
{   i  }
  
  #  View(sig.value)
  #alternativ: iterriere mit rollapplyr
  if (F)
    sig.value <- rollapplyr(P, width=1000, FUN=roll.classifier.e4b, by.column=F,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs)
  ################################################################################################
  mP("signal.randomForest.e4b  %s  fertig ####################### ",sym)
  
  
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


roll.classifier.e4b<-function(p, allFeatures, maxWin=600, retrain.on="monthly",crs=NULL,p.i=0)
{
  mP("roll.classifier.e4b")
  
  #das bis maxWin wachsende Zeitfenster zum Tag:  lastDay
  sym=colnames(p);  if (is.null(sym))    sym=1
  pricesFeatures=na.omit(allFeatures)
  dim(pricesFeatures)
  if (p.i>0)
    p=p[max(p.i-maxWin,1):p.i]
  
  toDay = DateS(last(p))
  
  #das Ergebnis wird ein 3 dim  xts aus signal und signalsicherheit und model.err
  res=as.xts(data.frame(signal=0,confidence=0, model.err=0),index(last(p)))
  
  mP("...      roll.classifier.e4b     %s %d %s",sym,p.i,toDay)
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
  last.train.failed =  is.na(last(crs$forest.fit.wichtigkeit) )[1]
  if (retrain.event(toDay, retrain.on, p) || firstCall || last.train.failed)
  {
    mP("retrain.event !!! at %s ################## %s",toDay,retrain.on)
    #browser(mP("#K1"))
    forest.fit.wichtigkeit= classifier.randomForest.fit(crs, priceFeatures[,-1])   #trainiere - ohne explizit noch mal die Kurse zu ?bergeben
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
    mP("#bug at roll.classifier.e4b_ ")
    
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
  mP("micro.cloud")
  
  #bereinigen
  euro.macros.na= m.ifna.prev(euro.macros)
  if (sum(apply(euro.macros.na,2,FUN=function(col) len(col[is.na(col)]))) >0)
    no=foreach(col.i = 1:ncol(euro.macros.na)) %do%  { euro.macros.na[is.na(euro.macros.na[,col.i]),col.i]<-0 }
  
  #mNorm
  mNorm.mac= mNorm(euro.macros.na)
  if (visual)purePlot(mNorm.mac)
  if (visual)cloud.check(mNorm.mac)
  
  
  #slope200
  slope200.mac=rollRegressionXTS(euro.macros.na,win=200)*100
  slope200.mac = bt.apply.matrix(slope200.mac, cutInterval, mi=-30,ma=30)
  #slope200.mac=col.rename(slope200.mac,"slope200")
  
  if (visual)cloud.check(slope200.mac)
  if (visual)purePlot(slope200.mac)
  
  cloud.mac= na.omit(merge( mNorm.mac,  slope200.mac))
  
  #gibt es is.infinite-Werte ?
  if (sum(apply(cloud.mac,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
    no=foreach(col.i = 1:ncol(cloud.mac)) %do%  { cloud.mac[is.infinite(cloud.mac[,col.i]),col.i]<-0 }
  
  #MMCLOUDs
  return(lag(cloud.mac)) #ich hab mit kursen gearbeitet die ich heute morgen, von gestern abend, erhalten hab.
  #...............................................................................  
}


small.cloud<-function(euro.macros,visual=F,cloud.control=list())
{
  mP("small.cloud")
  
  #bereinigen
  euro.macros.na= m.ifna.prev(euro.macros)
  if (sum(apply(euro.macros.na,2,FUN=function(col) len(col[is.na(col)]))) >0)
    no=foreach(col.i = 1:ncol(euro.macros.na)) %do%  { euro.macros.na[is.na(euro.macros.na[,col.i]),col.i]<-0 }
  
  #mNorm
  mNorm.mac= mNorm(euro.macros.na)
  if (visual)purePlot(mNorm.mac)
  if (visual)cloud.check(mNorm.mac)
  
  #faber
  faber.mac =bt.apply.matrix(euro.macros.na,faber)
  faber.mac = bt.apply.matrix(faber.mac, cutInterval, mi=-30,ma=30)
  faber.mac=col.rename(faber.mac,"faber")
  
  if (visual)cloud.check(faber.mac)
  if (visual)purePlot(faber.mac)
  
  #slope200
  slope200.mac=rollRegressionXTS(euro.macros.na,win=200)*100
  slope200.mac = bt.apply.matrix(slope200.mac, cutInterval, mi=-30,ma=30)
  #slope200.mac=col.rename(slope200.mac,"slope200")
  
  if (visual)cloud.check(slope200.mac)
  if (visual)purePlot(slope200.mac)
  
  #slope90
  slope90.mac=rollRegressionXTS(euro.macros.na,win=90)*100
  slope90.mac = bt.apply.matrix(slope90.mac, cutInterval, mi=-30,ma=30)
  #slope200.mac=col.rename(slope200.mac,"slope200")
  
  if (visual)cloud.check(slope90.mac)
  if (visual)purePlot(slope90.mac)
  
  
  #roc 30
  roc30M.mac=ROC(euro.macros.na,30)
  roc30M.mac=bt.apply.matrix(roc30M.mac, cutInterval, mi=-50,ma=50)
  roc30M.mac=col.rename(roc30M.mac,"roc60")
  
  if (visual)cloud.check(roc30M.mac)
  if (visual)purePlot(roc30M.mac)
  
  #roc 5
  roc5M.mac = ROC(euro.macros.na,5)
  roc5M.mac=col.rename(roc5M.mac,"roc5")
  if (visual)purePlot(roc5M.mac)
  roc5M.mac = bt.apply.matrix(roc5M.mac, cutInterval, mi=-20,ma=20)
  if (visual)cloud.check(roc5M.mac)
  if (visual)purePlot(roc5M.mac)
  
  #itp
  
  itp= foreach(pi=1:ncol(euro.macros),.combine="merge") %do%
{
  p=euro.macros[,pi]
  sym=colnames(p)
  #  browser(mP("xxx"))
  mP("itp %s",sym)
  
  if ( listget("get.Quantile.M",cloud.control))
  {     
    itp=in.Trend.pos(p,visual=visual,main="itp",k=160,K=2,probs=c(0.1,  .20,  0.80),opt.getQ="get.Quantile.M")
    itpP=itp$itp
    itpQm=itp$Q.m
    
    itpQ1=itpQm[,1]; colnames(itpQ1)=c(sprintf("%s_itpQ1",sym))
    itpQ2=itpQm[,2]-itpQm[,3]; colnames(itpQ2)=c(sprintf("%s_itpQ2",sym))
    
    itpP=col.rename(itpP,"itp")
    itpQ1=col.rename(itpQ1,"itpM")
    itpQ2=col.rename(itpQ2,"itpdM")
    itp=merge(itpP,itpQ1,itpQ2)
  }
  else
  {
    itp=in.Trend.pos(p,visual=visual,main="itp",k=160,K=2,probs=c(0.1,  .20,  0.80))
    itp=col.rename(itp,"itp")
  }
  
  
}
  if (visual)cloud.check(itp)
  
  cloud.mac= na.omit(merge( mNorm.mac, faber.mac, slope200.mac,slope90.mac, roc30M.mac,roc5M.mac, itp))
  
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
  return(lag(cloud.mac)) #ich hab mit kursen gearbeitet die ich heute morgen, von gestern abend, erhalten hab.
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
#das sind besonders viele technische Indikatoren für die zu prognostizierende Zeitreihe
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
##########################################################################################
#TODO
# repariere tech.cloud bauer    ok?
# integriere genSA-PortfolioOptimierung
# erweitere info  um train.data-colnames  - ok
# sit-TSA: taugt die confidence als selektions-kriterium ? - taugt signal als timing ?
#
#signal.labor - als big.data.cloud hinzunehmen -  timing-filter ??
#
print("########### load + source EM4.R")
#sfSource("MLib/EM4.R")

if (F)
{
  list_R_functions('MLib/EM4.r')
  
}
