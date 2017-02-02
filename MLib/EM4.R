################################################################################################################
# 2013-11-28 
#Lade Eckhards e4  xls daten  nach xts pro sheet 
################################################################################################################

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
  euro.macros= read.EckhardsXLS(xlsName="EuropeMM.xls",startRow=15,date.format = "%b %y",to.month.end=T,debug=F)
  euro.indi= read.EckhardsXLS(xlsName="Index01.xls",startRow=5,belowColnames=5,debug=F)
  if (F)  #ist viel Schrott in den Spalten ?
  {
    colSums(diff(euro.macros["1997::"],30),na.rm=T)
    colSums(euro.macros["1997::"],na.rm=T)
    colSums(ROC(euro.indi["1997::"],30),na.rm=T)
    colnames(euro.macros)
  }
  
  dim(euro.macros)  #154 Spalten mit 202 Monaten
  dim(euro.indi)    #8 Kurse mit 4372 Tagen
  
  euro.macros.n= mNorm(lag(euro.macros))["1997::"] #hier schon mal gelagged weil die Teile ja einen Monat Lieferversp?tung haben
  euro.indi.n = mNorm(euro.indi)["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  
  #spannende analysen
  if (F)
  {
    purePlot(ROC(euro.macros["1997::"],1),main="roc 1 euro.macros")  #kaum was zu sehen
    purePlot(ROC(euro.macros["1997::"],30),main="roc 30 euro.macros")  #starke vola-cluster
    purePlot(ROC(euro.indi["1997::"],30),main="roc 30 euro.indi")  
    
    purePlot(euro.macros.n)
    purePlot(euro.indi.n)
    
    chart.Correlation(ROC(euro.indi["1997::2000"],1),main="1997::2000")
    chart.Correlation(ROC(euro.indi["2008::2011"],1),main="2008::2011")
    chart.Correlation(ROC(euro.indi["2010::2014"],1),main="2010::2014") 
    chart.Correlation(ROC(euro.indi["2010::2014"],30),main="2010::2014") 
    
    correlogramm(ROC(euro.indi["1997::"],30),main=" euro.indi")
    correlogramm(ROC(euro.macros["1997::",c(1:30)],1),main=" euro.macros")
  }
  
  TSA.prepPro.e4(visual=F,bench="DAX30",do.par =T)
  
}
#ein virtuelles data-env vorbereiten - damit das SIT-Framework laufen kann

#data <- make.data(euro.indi,mkTrain=T)
#rattle()

#eine multi-Target-DataCloud vorbereiten  .. einige technische Indikatoren auf indizes und macros 
#ROC, p-SMA200

###############################################################################
#macros (monatlich) und indizes(t?glich) einladen
#eine tagesbasierte tech.cloud   (dataCloud.e4) f?r jeden Index berechnen
#die targets pro symbol berechnen
#zu den macros die indizes als pseudo-macros monatlich beimischen
#die monats basierte macro.cloud() berechnen
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
TSA.prepPro.e4<-function(visual=T,bench="DAX30",do.par=T)
{
  #control-parameter zu fein-spec der technischen  dataCloud.e4
  cloud.control <<- spl("heavy,lagged")# spl("heavy,forecast,pca,OHLC,lagged,coarse.code")
  
  if(F)
  {
    visual=T;bench="DAX30";do.par=T
  }
  #daten einlesen, target-data berechnen und data-environment und global_arg vorbereiten - auch sfExport()
  data <-make_eckhard4_data(visual=visual,bench=bench,do.par=do.par)
  #cloud-bildung parametrisieren
  #paralle vorbereiten
  #sfExport("data")
  
  #datacloud berechnen
  #data$train.data=new.env() #l?scht bereits erstellte train.data
  
  #sfStop()
  #schon mal die 5 - feature- map f?r die monats  - macros vorbereite
  data$macros = na.omit(merge(data$macros,data$prices))  #wir nehmen noch die euro.indizes als speudo-macros mit auf .. wer wei?
  
  data$macros = merge(data$macros,arg$da$ifo)
  
  #macro-cloud berechen
  cloud.mac= macro.cloud(data$macros,visual=F)
  if (visual)purePlot(cloud.mac);dim(cloud.mac)  #810 macro-feature !!!!
  
  # die technische dataCloud.e4 (tagesdaten) berechnen  (in labor_signal.r - gibts noch mehr - eine auf in.Trend.pos()- basierende cloude)
  
  if (do.par)  #nur in der Entwicklungsphase hilfreich...
  {
    sfSource("MLib/EM4.R")
    sfSource("MLib/InputConfig_Portfolio_TD.R")
    sfSource("MLib/classifier_randomForest.r")
    sfExport("data")  ### muss erst der ifo- geladen werden ?
    sfExport("cloud.control")
  }
  
  sfExport("data")  ### muss erst der ifo- geladen werden ?
  sfExport("cloud.control")
  
  #fÃ¼r jedes sym die dataCloud.e4 berechnen (ca. 70 technisches features auf tagesbasis)
  all.features=sfLapply(data$symbolnames,function(sym)
  {
    #sym="DAX30"
    mP("dataCloud.e4 for %s",sym)
    arg= list(dat =data, clos=data$prices[,sym],Target=data$Target[,sym] )
    par=list(sma.w=200)
    p=data$prices[,sym]
    P=data[[sym]]
    
    features=dataCloud.e4(sym,arg,par,p,P=NULL,data$Target,cloud.control)
  })
  
  len(all.features)  #muss der Anzahl der symbole entsprechen
  
  #propagiere die Ergebnisse nach data$train.data[[sym]]
  #und misch die cloud.mac hinzu
  no=lapply(1:len(all.features),function(sym.i)
  {
    sym=data$symbolnames[sym.i]
    tech.cloud = data.frame(all.features[sym.i])
    
    #................. die macro-cloud beimischen  und mit m.ifna.prev die monatsdaten auff?llen       
    tech.cloud=as.xts(tech.cloud, orderby=as.Date(rownames(tech.cloud)))
    all.cloud =merge(tech.cloud,cloud.mac)
    all.cloud=m.ifna.prev(all.cloud)
    dim(all.cloud)  # 3829 * 891
    write.xts(all.cloud,filename= sprintf("allCloud_%s.csv",sym))
    data$train.data[[sym]] =data.frame(all.cloud)
  })
  
  all.features=NULL #speicher freigeben
  #check
  ls(data$train.data)
  data$symbolnames
  dim(data$train.data[["DAX30"]])  #   ->  3821 Tage * 81 Spalen
  #  View(data$train.data[["DAX30"]])
  colnames(data$train.data[["EXX50_RI"]])
  mP("train.data cloud is ready at data$train.data[[sym]]")
  #................................................................................................
  #MMDATA
  save(data,file="em4.data") 
  if (F)
  { 
    do.par
    prepare_Parallel() 
    load(file="em4.data")
    sfExport("data")
    
    ls(data);   dim(data$train.data[["DAX30"]])
  }
  #forest-trainieren+evaluieren -> signale,rankings und variablenSelektion  berechen
  
  symbolnames =spl("DAX30")
  
  symbolnames =spl("DAX30,SXX50")
  symbolnames =data$symbolnames
  
  # signale, model.err und feature-wichtigkeiten berechnen  
  retrain.on="quarterly"
  #retrain.on="yearly"
  retrain.on="monthly"
  sfExport("retrain.on")
  #................................................................................
  
  #MMRUN
  all.sig=lapply(symbolnames,function(sym)
    #all.sig=sfLapply(symbolnames,function(sym)    
  {
    backtest.frame="::2004-01-01"
    #  backtest.frame="::2012-12-31"    
    #sym="EXX50_RI"  #MMT
    arg= list(dat =data, clos=data$prices[backtest.frame,sym],Target=data$Target[backtest.frame,sym] )
    par=list(sma.w=200)
    arg$dat$crs=new.env() #l?scht das schon gelernte
    res= signal.randomForest.e4(arg,par,retrain.on=retrain.on,visual=F)$res
    
    signal=res[[sprintf("%s.sig",sym)]]
    wichtigkeit =res[[sprintf("%s.forest.fit.wichtigkeit",sym)]]
    mP("#a1")
    #browser(mP("xxxx res"))
    save(signal,file=sprintf("signal_randomForest_e4_%s",sym),signal)
    save(wichtigkeit,file=sprintf("wichtigkeit_randomForest_e4_%s",sym),wichtigkeit)
    return(res)
  })
  #7 nodes produced errors; first error: NROW(x) must match length(order.by)\n"
  if (F)
  {
    
    
  }
  
  #................................................................................
  mP("fertig mit allen test-train-runs")
  len(all.sig)
  
  symbolnames=c("DAX30")
  #MMAUSW
  #auswertung  .. wie ver?ndert sich die forest.fit.wichtigkeit  ... 
  no=lapply(1:len(symbolnames),function(sym.i) 
  {
    sym=symbolnames[sym.i]
    #ls(res[[1]]) #"confidence" "model.err"  "signal" 
    if(F)  #noch im speicher .
    {
      res = all.sig[[sym.i]]
      forest.fit.wichtigkeit=res[[sprintf("%s.forest.fit.wichtigkeit",sym)]]
      sig=  res[[sprintf("%s.sig",sym)]]
      write.xts(forest.fit,sprintf("wichtigkeit_%s.csv",sym))
      write.xts(sig,sprintf("signal_%s.csv",sym))
    }
    else #schon serialisiert
    {
      path= "Models/EM4_November/m1000/"
      load(file=sprintf("%ssignal_randomForest_e4_%s",path,sym))
      sig=signal
      write.xts(merge(data$prices[,sym],signal),sprintf("%sssignal_randomForest_e4_%s.csv",path,sym))
      load(file=sprintf("%swichtigkeit_randomForest_e4_%s",path,sym))
      forest.fit.wichtigkeit = wichtigkeit
      write.xts(wichtigkeit,sprintf("%swichtigket_randomForest_e4_%s.csv",path,sym))
    }
    #die trainings-qualit?t ?ber die zeit
    model.err = forest.fit.wichtigkeit[,1]
    purePlot(model.err, main=sprintf("model.err %s",sym))
    #die Variablen-Wichtigkeit ?ber die Zeit
    dim(forest.fit.wichtigkeit[,-1])
    #browser(mP("now"))
    purePlot(forest.fit.wichtigkeit,main=sprintf("forest.fit.wichtigkeit %s",sym))  
    
    b=na.omit( merge(sig[,1]-1, data$prices[,sym]))
    plotSigPrice(signal=b[,1],prices=b[,2])#,indi=list(conf=merge(sig[,2])))
    
    df=data.frame(colMeans(forest.fit.wichtigkeit["2010::",-1]))
    
    #.............................................................
    # Ausfilern der wichtigsten Fundamentalfaktoren
    #
    #die pro quartal gemittelten faktoren-gewichte
    df.q= apply.quarterly(x=forest.fit.wichtigkeit[,-1], colMeans)
    View(df.q)
    #die 100 wichtigsten quartals-faktoren
    nt=ntop(df.q,100)
    View(df.q)
    #ihre-colnames
    k=bt.apply.matrix(nt, function(col) {ifelse(col!=0,colnames(col),"")})
    View(k)
    #nun reduziert auf die fundamentalfaktoren (technische faktoren haben vor  <sym>. im colname)
    k2=bt.apply.matrix(k, function(col) 
    {iif (col!="" & len(grep(sprintf("%s.",sym),col )) == 0  ,col,"")})
    #pro zeile eine liste mit den nicht leeren fundi-colnames  
    k2=k  #ohne Beschränkung auf FundamentalfaktorenQ
    
    k3= lapply(1:nrow(k2), FUN=function(row.i){spl(paste(k2[row.i,]))})
    head(k3)
    #pro listenvektor ein vector mit den fundi-gewichten
    k4=lapply(k3,function(c1)sapply(c1,function(x) df.q[1,x]))
    #das faktor-gewicht soll wenigstens 4 sein
    k5=lapply(k4,function(c1)c1[c1 > 4]  )
    #die ListeNamen k5 als Datümer wählen
    names(k5)<-index(df.q)
    k5
  })  
  
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

########### wird so auch in MM_Main.R  mit universe = Eckhard4 geladen
#ein virtuelles data-env vorbereiten - damit das SIT-Framework laufen kann

make_eckhard4_data<-function(visual=F,bench="DAX30",do.par=T)
{
  euro.indi= read.EckhardsXLS(xlsName="Index01.xls",startRow=5,belowColnames=5,debug=F,visual=visual)
  euro.indi.n = mNorm(euro.indi)["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  euro.macros= read.EckhardsXLS(xlsName="EuropeMM.xls",startRow=15,date.format = "%b %y",to.month.end=T,debug=F,visual=visual)
  
  data <-  make.data(euro.indi.n, mkTrain=T,visual=visual,bench=bench)
  data$macros=euro.macros
  data$betas=beta.again.portfolio(data) #die monats-betas der data$prices gegen?ber ihrem gleichgewichteten portfolio
  data$crs=new.env() #f?r den forest
  data$train.data = new.env()  #die dataCloud - gecashed
  #noch zusÃ¤tzlich den ifo dazu
  if  (is.null(data$ifo))
    data$ifo =  load.IFO(visual=T)
  
  
  BENCH <-global_arg$BENCH <-bench
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission<<-0.0015   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(data$prices)) 
  global_objectId <<-paste("TREND","xx","signal.lm") 
  global_FastTrain <<-20 #hei?t:  f?r jede dim werden 2 Schritte ausprobiert.
  
  if (do.par)
  {
    sfStop()
    prepare_Parallel()  
    
    mP("make_eckhard4_data - sfExport() ")
    sfExport("data")  
    sfExport("global_arg")  
    sfExport("global_ParTable")  
    sfExport("global_StartDate")  
    sfExport("global_FastTrain")  
    sfExport("BENCH")  
  }
  return(data)
}

#steuer die cloud-erstellung mit folgenden schaltern:

#cloud.control = spl("lagged")# spl("heavy,forecast,pca,OHLC,lagged,coarse.code")
#########################################################################################
#Dieser cloudGenerator (analog zu dem in indicators2.r) ist durchaus dax-spezifisch weil hier heftig auch vom Ifo-gebrauch #gemacht wird.  Ferner weden die signale gelagged und coarse.coded()  .. pca k?nnen bei bedarf #aktiviert werden.  Ebenso k?nnen forecast() eingebunden werden.
#########################################################################################
dataCloud.e4<-function(sym,arg,par,p,P=NULL,target,cloud.control=list())
{
  mP("dataCloud.e4")
  
  ret.p = mROC(p[,sym])
  normP = mNorm(p[,sym])
  #.......................................................................................
  if("forecast" %in% cloud.control)
  {
    #fit <- auto.arima(dts)
    #plot(forecast(fit, h=20))
    
    #fit <- HoltWinters(P)#,gamma=FALSE)  #nicht schlecht
    #plot(forecast(fit, h=5))
    #fit$coefficients[2]
    
    browser(mP("%s forecasts:",sym))
    forecast_<<-NULL #hier merkt er sich die letzte prognose .. 
    forecasts <- rollapplyr(normP, width=300, FUN="roll.forecast.sample", by.column=F, align = "right", sample.on="months",allPrices=normP,f="sample.forecast")
    arg$dat$crs$forecasts <-forecasts
    browser(mP("%s forecasts <<<< ready",sym))
    
    mP("AUSWERTUNG")
    tail(forecasts)
    
    #evtl. darf ich hier forecasts = lag(forecasts)  machen, weil ich so geschrieben hab.
    
    #die Fehlerraten der  einzelnen Methoden
    errRate=foreach(i = 1:ncol(forecasts),.combine = "cbind") %do% {
      forecasts[!is.finite(forecasts[,i]),i]<-0    
      block=na.omit(merge(data$Target[,sym],sign(forecasts[,i])))
      numBugs = abs(sum(xts(apply(block,1,FUN=function(x) sign(diff(x))),order.by=as.Date(index(block)))))
      errRate= numBugs/nrow(block)
    }
    colnames(errRate)=colnames(forecasts)
    errRate
    okRate = 1-errRate
    #der forecast wenn man die sign(Einzelforecasts) mit ihrer okRate gewichtet summiert
    weighted.mean.forecast=xts(apply(na.omit(forecasts),1,FUN=function(x){sign(sum(sign(x)*okRate))}),order.by=as.Date(index(na.omit(forecasts))))
    colnames(weighted.mean.forecast)="Forecast"
    
    #der Fehler des weighted.mean.forecasts ( aller forecast-modelle )
    block=na.omit(merge(data$Target[,sym], weighted.mean.forecast))
    numBugs = abs(sum(xts(apply(block,1,FUN=function(x) sign(diff(x))),order.by=as.Date(index(block)))))
    errRate.mean= numBugs/nrow(block)
    forecast.signals = merge( sign(forecasts),  sign(weighted.mean.forecast))
    tail(forecast.signals)  #<----- das xts. mit allen forecasts - incls. der gewichteten Prognose
    arg$dat$crs$forecasts.signals <- forecast.signals
    #wie w?rden denn die forecast-signale als modell performen ?
    foreach(i = 1:ncol(arg$dat$crs$forecasts.signals ),.combine = "cbind") %do% {
      plotSigPrice(signal = -arg$dat$crs$forecasts.signals[,i],prices=normP,indi=list(f=forecasts[,i]) )   
    }
  }
  #.......................................................................................
  # colnames(forecasts)=c("forecasts")
  
  if  (is.null(arg$dat$ifo))
    arg$dat$ifo =  load.IFO(visual=T)
  
  IFO.m=lag(to.monthly(arg$dat$ifo)[,6])  #fundamentales Umfeld  -  h?ngt einen monat hinterher
  
  sd=runSD(ret.p,n=30)
  colnames(sd)=c("sd")
  
  mad=runMAD(p,n=30)
  colnames(mad)=c("mad")
  
  atr=ATR(HLC(to.monthly(p)))
  atr.d=(atr[,2]-atr[,1])
  colnames(atr.d)=c("atr.d")
  
  
  if (is.null(P) ||   ! "OHLC" %in% cloud.control)
    hl=(Hi(to.monthly(p))-Cl(to.monthly(p)))/(Hi(to.monthly(p))-Lo(to.monthly(p)))
  else
    hl=(Hi(to.monthly(Hi(P)))-Cl(to.monthly(p)))/(Hi(to.monthly(Hi(P)))-Lo(to.monthly(Lo(P))))
  colnames(hl)=c("hl")
  
  if("heavy" %in% cloud.control) #F nur bei TEST
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
  # browser()
  train.frame= fromToS(na.omit(target[-c((shape(target)-cutTail) : shape(target))]))
  mP("train.frame is %s",train.frame)
  
  #.......................................................................................
  mP("first merge and slopes")
  
  pSMA200=  p/SMA(p,200)
  pMax200=p-runMax(p,200)
  beta=arg$dat$betas[,sym]
  colnames(beta)=sprintf("beta_%s",sym);
  #browser()
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
    (IFO.m-SMA(IFO.m,7))/IFO.m, runSum(sign(IFO.m),3),
    beta
  )
  
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
  mP("ifna.prev")
  features= na.omit( bt.apply.matrix(features,ifna.prev))
  #  tail(features)
  # colnames(features)
  
  #.................................................................
  
  if("coarse.code" %in% cloud.control) 
  {
    mP("coarse.code")
    mP("feature coarseCode %d",dim(features)[2])  
    
    features.coarse= bt.apply.matrix(na.omit(features),coarse.code,b=100,method="n")
    #plot( coarse.code(dax["1994::"],b=10,method="n"))
  }
  #------------------------------------------------------------------
  if("pca" %in% cloud.control)
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
  if("lagged" %in% cloud.control)
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
    
    features = merge(features,features.lag,features.lag5,features.lag20) #lags benutzen
  }
  #-------------------------------------------------------------------------------
  mP("define target")
  target =target[,sym]
  colnames(target)=c("Target")
  
  mP("rename features to %s",sym)
  #noch den Symbol-Namen -   vor den featureNamen schreiben - damit notfalls eine super-breit cloud gebildet werden kann
  newColnames= sapply(colnames(features), function(colname){sprintf("%s-%s",sym,colname)})
  colnames(features) = newColnames
  
  print(colnames(features))
  
  
  #in die colnames den sym-namen mit aufnehmen
  
  mP("build and lag train.data frame")
  #1 Tag lag sonst kristallkugel
  train.data<-data.frame(na.omit(lag(merge(target,features))))
  #  browser(mP("ok -----------------------------"))
  
  
  #gibt es is.infinite-Werte ?
  if (sum(apply(train.data,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
    no=foreach(col.i = 1:ncol(train.data)) %do%  { train.data[is.infinite(train.data[,col.i]),col.i]<-0 }
  
  return( train.data)
  ###################################################################################################
}

signal.randomForest.e4 <-function(arg, par = mlist( sma.w=c(200,120,350,10)),retrain.on="quarterly",visual=F,...)
{
  mP("signal.randomForest.e4")
  #Vorausetzung in arg$dat$Target liegen pro price-Symbol TargetTrainingsdaten
  sym=colnames(arg$clos)
  train.data = arg$dat$train.data[[sym]]  
  p=mNorm(arg$clos)
  today=as.Date(last(index(p)))
  target=na.omit(arg$dat$Target[,sym])
  crs = arg$dat$crs   #das trainings-environment - siehe rattle  
  P=na.omit(arg$dat[[sym]])
  P=NULL  #eckhard liefert kein OHLC
  #berechne alle merkmale f?r den dax
  
  #........ baue die Data-cloud (einmal - chashe sie in train.data)
  if (is.null(arg$dat$train.data[[sym]]))
  {
    features = dataCloud.e4(sym,arg,par,p,P,target)
    arg$dat$train.data[[sym]] = features
  } 
  else
    features = na.omit(arg$dat$train.data[[sym]])   #chashe
  
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
    sig.value.list=sfLapply(1000:shape(P), function(p.i) roll.classifier.e4(P ,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs,p.i=p.i)) 
  else
    sig.value.list=lapply(1000:shape(P), function(p.i) roll.classifier.e4(P ,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs,p.i=p.i)) 
  
  mP("hol results")
  #ergebnisse aus er liste holen
  sig.value=foreach(i= sig.value.list,.combine="rbind") %do%
{   i  }
  
  #alternativ: iterriere mit rollapplyr
  if (F)
    sig.value <- rollapplyr(P, width=1000, FUN=roll.classifier.e4, by.column=F,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs)
  mP("#a3.a"  )  
  ################################################################################################
  mP("signal.randomForest.e4  %s  fertig ####################### ",sym)
  # browser(mP("##4"))
  
  signal = sig.value[,1]
  sig.confidence = sig.value[,2]
  model.err = sig.value[,3]
  res = list()
  res[[sprintf("%s.sig",sym)]]=sig.value
  res[[sprintf("%s.forest.fit.wichtigkeit",sym)]]=crs$forest.fit.wichtigkeit
  
  bug=(target-signal)/nrow(target)*100
  if (visual)
  {
    dim(na.omit(signal))
    dim(p)
    dim (bug)
    fromToS(p)
    fromToS(signal)
    b=na.omit(merge(signal,p,bug))
    signal =b[,1];p1=b[,1];bug=b[,3]
    
    
    plotSigPrice(signal=signal,prices=p1,indi=list(confi=sig.confidence,bug=bug))  
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


roll.classifier.e4<-function(p, allFeatures, maxWin=600, retrain.on="monthly",crs=NULL,p.i=0)
{
  mP("roll.classifier.e4")
  
  #das bis maxWin wachsende Zeitfenster zum Tag:  lastDay
  sym=colnames(p);  if (is.null(sym))    sym=1
  pricesFeatures=na.omit(allFeatures)
  dim(pricesFeatures)
  if (p.i>0)
    p=p[max(p.i-maxWin,1):p.i]
  
  toDay = DateS(last(p))
  
  #das Ergebnis wird ein 3 dim  xts aus signal und signalsicherheit und model.err
  res=as.xts(data.frame(signal=0,confidence=0, model.err=0),index(last(p)))
  
  mP("...      roll.classifier.e4     %s %d %s",sym,p.i,toDay)
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
  #browser(mP("###1"))
  
  #+++++++++ muss das modell ge fitted werden ?
  forest.fit.wichtigkeit = "no"
  #if (toDay=="2002-12-31")
  #    browser(mP("##5"))
  #---------------------------------------------------------------------------------------------
  last.train.failed =  is.na(last(crs$forest.fit.wichtigkeit) )[1]
  if (retrain.event(toDay, retrain.on, p) || firstCall || last.train.failed)
  {
    mP("retrain.event !!! at %s ################## %s",toDay,retrain.on)
    
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
    mP("#bug at roll.classifier.e4_ ")
    
  }
  print(prognose)
  #.................................................................................
  mP("#a4--------->")
  return(prognose)
  
  
  
  
  #  return(list(prognose=prognose, forest.fit.wichtigkeit= forest.fit.wichtigkeit ))
  
  
}
#roll.classifier.e4<-cmpfun(roll.classifier.e4_)

########################################################################################
#berechnen und l?ngenbeschneiden der werte
########################################################################################

macro.cloud<-function(euro.macros,visual=F)
{
  #bereinigen
  euro.macros.na= m.ifna.prev(euro.macros)
  if (sum(apply(euro.macros.na,2,FUN=function(col) len(col[is.na(col)]))) >0)
    no=foreach(col.i = 1:ncol(euro.macros.na)) %do%  { euro.macros.na[is.na(euro.macros.na[,col.i]),col.i]<-0 }
  
  #mNorm
  mNorm.mac= mNorm(euro.macros.na)
  if (visual)purePlot(mNorm.mac)
  
  #faber
  faber.mac =bt.apply.matrix(euro.macros.na,faber)
  faber.mac = bt.apply.matrix(faber.mac, cutInterval, mi=-30,ma=30)
  if (visual)purePlot(faber.mac)
  
  #slope200
  slope200.mac=rollRegressionXTS(euro.macros.na,win=7)*100
  slope200.mac = bt.apply.matrix(slope200.mac, cutInterval, mi=-30,ma=30)
  if (visual)purePlot(slope200.mac)
  
  #roc 30
  roc30M.mac=ROC(euro.macros.na,30)
  roc30M.mac=bt.apply.matrix(roc30M.mac, cutInterval, mi=-50,ma=50)
  if (visual)purePlot(roc30M.mac)
  
  #roc 1
  roc1M.mac = ROC(euro.macros.na,1)
  if (visual)purePlot(roc1M.mac)
  roc1M.mac = bt.apply.matrix(roc1M.mac, cutInterval, mi=-20,ma=20)
  if (visual)purePlot(roc1M.mac)
  
  cloud.mac=merge( mNorm.mac, faber.mac, slope200.mac, roc30M.mac,roc1M.mac)
  
  if (visual)
  {
    purePlot(cloud.mac)  
    dim(cloud.mac)
    dim(euro.macros.na)
  }
  return(cloud.mac)
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

###########################################################################################
print("########### load + source EM4.R")
#sfSource("MLib/EM4.R")

if (F)
{
  list_R_functions('MLib/EM4.r')
  
}