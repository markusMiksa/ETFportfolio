##
#https://www.krisensicherinvestieren.com/Die_Strategie-168_153_154_32.html

####################################################################################
#####################################################################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))

options(warn=1)

###########################################################################################
TSA.Universe<-function(mode="R",universe = "Meurope", dataSet ="MM_Feb3", TargetMethod=  0 , online =T, visual=T)
{
  if (mode=="W")
  { 
    #rm(list= ls());     source  ("MLib/TSA.r")
    init_TSA(universe = universe,  dataSet =dataSet, TargetMethod=  TargetMethod , online =online, visual=visual)
    p=global_arg$clos = data$prices=data.info(data)
    save(data,file=sprintf(file=sprintf("Models/%s/TSA.data",dataSet)))
  }  
  else  
    load(file=sprintf("Models/%s/TSA.data",dataSet))
}

#......................lade/update viele Universe-daten in data-files:
if (F)
{
  focus.portfolios = spl("Meurope,MWorld3,World2,SektorSpdrUSA,IndexAsien,Branchen,Mmain")
  lapply(focus.portfolios,function(u) TSA.Universe(mode="W",universe=u))
}

###########################################################################################

init.Code<-function()
{
  setwd("d:/R/Nuggets/ETFmodel") 
  print("###init_TSA ###")
  #browser()
  source("MLib/InputConfig_Portfolio_TD.r")
  
  #save(list=ls(),file="d:/mm.Rdata")
  #loadPortfolios(customerData)
  
  source("MLib/portfolioCode.r")
  source("MLib/LoadDataFuncs.r")
  source('MLib/Trade.R')
  source("MLib/TradeClasses.r")
  source("MLib/score.r")
  source("MLib/Now.r")  #viel warnungen
  source("MLib/mlist.r")
  source("MLib/Now2.r")
  #  source("MLib/InputConfig_Portfolio_TD.r")
  
  
  # global_arg<< list();global_arg$dat= data;
  
  # source("MLib/InputConfig_Portfolio_TD.r")
  source("MLib/test.r")
  source("MLib/SITpatches.r") 
  source("MLib/target.r")  #global i
  source("MLib/indicators.r")  #global i
  source("MLib/classifier_randomForest.r")
  source("MLib/indicators2.r")
  source("MLib/attention.r")
  source("MLib/rank.r")
  # load("d:/mm.Rdata")
  
  #source("MLib/InputConfig_Portfolio_TD1.r")
  #source("MLib/InputConfig_Portfolio_TD2.r")
  #source("MLib/InputConfig_Portfolio_TD3.r")
  #source("MLib/InputConfig_Portfolio_TD4.r")
  #source("MLib/InputConfig_Portfolio_TD5.r")
  
  
  source('MLib/Trade.R')
  #source("MLib/fundamentals.r")
  source("MLib/labor_signal.r")
  
  
  #  source("MLib/EM4.r")
  source("MLib/EM4b.r")
  source("MLib/signal.r")
  source("MLib/cluster.r")
  
  seed=3
  # We need to set the seed to have reprodcible results
  set.seed( seed )
  global_ALL_CODE_LOADED <<- T 
}

init_TSA<-function(universe="Meurope", dataSet ="MM_Feb1", SAFE="TREASURY",TargetMethod=NULL, online=F, visual=F)  #TargetMethod -1,0,1
  
{ 
  init.Code()
  # browser(mP("visual "))
  ################################################################################
  loadPortfolios(sprintf("%s/Models",getwd()))   #"o:/r/Nuggets/Eckhard/Models" 
  ################################################################################
  global_ParTable<<-NULL  #l?sche das Trainingsged?chtnis
  global_StartDate <<- 1
  
  globalPROVIDER <<- "CSV"
  csvDatFormat<<-"%Y-%m-%d"
  globalSEP <<- ";"
  globalDEC <<- "."
  globMDataSUB<<-""
  global_xMarker <<- NULL#list(as.Date("2010-04-22"))
  dataSet <<- dataSet#"MM_Nov"#modelDir
  
  global_commission<<-0.0005   #sys.Run() nimmt diese commission !!!!
  
  global_arg <<-list()
  lookback.len <<- 60
  itp.PIPES<<-list()
  reset.cashed.rank()
  
  load.universe(universe=universe,online=online,visual=visual)
  
  
  if (F)  #DEMO TSA-RUN
    x=indi.Generic("signal.any.smoothing", global_arg, 
                   par=list(glaettung=200,glaettung2=50,dw=2),
                   xarg=list(fn="ZLEMA",fn2="SMA",onlyLong=T,q.w="50%"),
                   visual=T, TRAINSYM =-1, experiment="zlema_sma",nTop=2)
  
  return("ok")
}
mP<-function(...,info=F)
{
  #browser()
  #tryM(sfCat(sprintf(...),master=T))  #print
  #formals(data[["level.price"]])
  if (info) 
  {print("...................... mP ...................>>")
   print(traceback())
   x=formals(fun=sys.function(sys.parent(1))) #gibt die Parameter zur aktuellen Methode
   print(x)
  }
  res=  try(sprintf(...))  #print
  print(res)
  if (info)print("<<<  ..........................................")
  res
}
#
#......................................................................

load.universe<-function(universe="MWorld3",SAFE="TREASURY",online=F,visual=F,TargetMethod=NULL)
{
  bench="DAX"; 
  if (universe!="")
  {
    mP(" >>>>>>>>>>> load universe %s",universe)
   # setInternet2(TRUE)
    if (universe == "Eckhard4")
    {
      euro.indi= read.EckhardsXLS(xlsName="Index01.xls",startRow=5,belowColnames=5,debug=F)
      euro.indi.n = mNorm(euro.indi)["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
      euro.macros= read.EckhardsXLS(xlsName="EuropeMM.xls",startRow=15,date.format = "%b %y",to.month.end=T,debug=F)
      
      data <<-  make.data(euro.indi.n, mkTrain=F)
      data$euro.macros=euro.macros
    }
    else
      if (universe == "Eckhard4.12")
      {
        prepare.EM4B.data(do.par=do.par)
        MM_lesson1<<-"ok"
        return("ok")
        #data <<-make_eckhard4B_data(visual=visual,bench="EXX50_RI",do.par=do.par)
        #data$macros = na.omit(merge(data$macros,data$prices))  #wir nehmen noch die euro   
      }
    else
      
      if (universe =="MWorld3")
      {
        MWorld3 <<- T2$new(PortfolioName = "MWorld3", bench="DAX",visual = F, online=online)
        #browser(mP("#33"))
        data<<-MWorld3$t0data
        #l?sch ein paar extrem gut gelaufene Daten
        data.rm(data,spl("GOLD,JAKARTA_COMPOSITE,OIL"))  #OIL
      }
    else if (universe=="Meurope")
    { 
      Meurope <<- T2$new(PortfolioName = "Meurope", bench="DAX",visual = F, online=online)
      data<<-Meurope$t0data
      #data.rm(data,c("REX"))
    }
    else if (universe=="DAX")#...........................................
    {
      if (F)
      {
        online=T
        dax.data= get.Index.data("^Gdaxi")  
        no=sapply(dax.data$symbolnames, FUN=function(x)print(x))
        writeIndexComposition("DAX",universeFile="d:/dax.comp.csv")
        writeIndexUniverse("DAX")    
      }
      DaxComp <<- T2$new(PortfolioName = "DaxComp", bench="DAX",visual = F, online=online)
      data <<-DaxComp$t0data
      
      dax.composites = data$symbolnames
      p.dir= sprintf("MData/fundamentals/Stocks/")
      
      #versuche die Bilanzen nach  data$fund zu laden
      foreach(i=seq(2,len(dax.composites),1), .combine=c) %do% {
        #i=1
        symbol=dax.composites[i]# "AAPL";
        #exchange="FWB"
        #csv.data=fundamental.dcf(data,symbol,exchange,p.dir)
        
        #symbol="VOW3"
        mP("download fundamentals for %s",symbol)
        exchange="LS" #FWB  XE
        csv.data=NULL
        csv.data=fundamental.dcf(data,symbol,exchange,p.dir)
        if (len(csv.data) < 1)
        {
          exchange="FWB"
          csv.data=fundamental.dcf(data,symbol,exchange,p.dir)
        }
        if (len(csv.data) < 1)
        {
          exchange="XE"
          csv.data=fundamental.dcf(data,symbol,exchange,p.dir)
        }
        if (len(csv.data) < 1)
        {
          exchange="XE"
          isin=get.Isin(sprintf("%s.de",symbol))
          
          isin=substr(isin,6,nchar(isin)-1)
          browser(mP("with isin %s",isin))
          csv.data=fundamental.dcf(data,isin,exchange,p.dir)
        }
        
        if (len(csv.data)>0)
          write.csv(csv.data,file=sprintf("%s%s_dcfGc.csv",p.dir,symbol),sep=";",row.names=format(index(csv.data)))
      }
      if (F)
      {
        #macrofundamentals laden:  IndustrieProd...
        IndustrieProd <- T2$new(PortfolioName = "IndustrieProd",bench="DAX",visual = F, online=T)
        data$macrofund <<-IndustrieProd$t0data
      }
    }
    else if (universe=="StoxxComp")
    {
      # stoxx.data= get.Index.data("^STOXX50E")
      StoxxComp <<- T2$new(PortfolioName = "StoxxComp", bench="Stoxx50E",visual = F, online=online)
      data <<-StoxxComp$t0data
      #  writeIndexUniverse("Stoxx50E") 
    }
    else
      if (universe=="DowComp")
      {
        DowComp <<- T2$new(PortfolioName = "DowComp", bench="DOW",visual = F, online=online)
        data <<-DowComp$t0data
        #writeIndexUniverse("DOW") 
      }
    else
      if (universe=="MDaxComp")
      {
        MDaxComp <<- T2$new(PortfolioName = "MDaxComp", bench="MDAX",visual = F, online=online)
        data <<-MDaxComp$t0data
        
        #writeIndexUniverse("MDAX") 
      }
    else
      if (universe=="DaxMdax")
      {
        DaxMdax <<- T2$new(PortfolioName = "DaxMdax", bench="DAX",visual = F, online=online)
        data <<-DaxMdax$t0data
        
        #writeIndexUniverse("MDAX") 
      }
    if (universe=="ETF_77")
    {
      #online=T
      etf_77 <<- T2$new(PortfolioName = "ETF_77", bench="TIPS_BARCLAYSCAPITALEUROINFLATIONLINKEDBOND_ETF",visual = F, online=online)
      etf_77
      data.info(etf_77$t0data)
      data <<-DaxMdax$t0data
      
      #writeIndexUniverse("MDAX") 
    }
    else
      if (universe=="DaxComp")
      {
        DaxComp <<- T2$new(PortfolioName = "DaxComp", bench="DAX",visual = F, online=online)
        data <<-DaxComp$t0data
        #writeIndexUniverse("DAX") 
      }
    else
      if (universe=="Futures")
      {
        Futures <<- T2$new(PortfolioName = "Futures", bench="DAX",visual = F, online=online)
        data <<-Futures$t0data
        #writeIndexUniverse("DAX") 
      }
    
    else
      if (universe =="ASIA")
      {
        FredTicker = list(
          c("DEXKOUS","FRED"), #load Korea
          c("DEXMAUS","FRED"), #load Malaysia
          c("DEXSIUS","FRED"), #load Singapore
          c("DEXTAUS","FRED"), #load Taiwan
          c("DEXCHUS","FRED"), #load China
          c("DEXJPUS","FRED"), #load Japan
          c("DEXTHUS","FRED"), #load Thailand
          c("DEXBZUS","FRED"), #load Brazil
          c("DEXMXUS","FRED"), #load Mexico
          c("DEXINUS","FRED"), #load India 
          c("DEXUSEU","FRED") #load India 
        )
        data=new.env()
        tickers <<- mGetTickers(FredTicker,online=T,data=data)
        
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                           
        bt.prep(data, align='keep.all', dates='2000::')
        asia.data <<-data
      }
    #find.best.series(data,visual =F,date.last="20100101")
    #browser(mP("#111"))
    data$symbolnames
    #data.rm(data,spl("DOW"))  #OIL
    #browser(mP("nnnnnnn"))
    prices <<-data$prices <- data.info(data,visual=visual)#,ignore=C("ZINSEN"))
    #damit auch im Workspace in RStudio anschaubar
    #View(DataInfo)
    
    #data.repair(prices=data$prices)
    #dax=data$prices[,"DAX"]  
    #fromTo(dax)
    #purePlot(mNorm(data$prices[,"DAX"]))
    #l?schen
    print(fromTo(data$prices))
    #trainings-zeitraum beschr?nken
    ####################################################### Vorbereitung f?r das Training
    train.frame <<- fromToS(data$prices)
    ls(data)
    global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
    global_commission<<-0.0005   #sys.Run() nimmt diese commission !!!!
    
    global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
    global_StartDate <<-  DateS(last(prices)) 
    global_objectId <<-paste("TREND","xx","signal.lm") 
    global_FastTrain <<-20 #hei?t:  f?r jede dim werden 2 Schritte ausprobiert.
    global_arg$BENCH<-"DAX"
    
    
    MM_lesson1<<-"ok"
    #-------------------------------------------------------------------------------------------
    #B) globale Steuervariablen setzen
    
    ModelRoot <<- getwd() 
    #setwd(ModelRoot)
    
    testXLSread<<-F
    readMyClasses<<-T
    
    
    globalPROVIDER <<- "CSV"
    csvDatFormat<<-"%Y-%m-%d"
    globalSEP <<- ";"
    globalDEC <<- "."
    globMDataSUB<<-""
    global_xMarker <<- NULL#list(as.Date("2010-04-22"))
    
    lookback.len <<- 60
    #periodicity = 'weeks'
    #    periodicity <<- 'months'
    prefix <<- ''
    myFrame <<-""
    
    #######################################################
    #Traings-Targets berechnen- werden f?rs Training des classifiers ben?tigt
    #browser(mP("Target"))
    #data$Target   <- compute.Target.by.method(data=data, TargetMethod=0, visual=visual,glaettung=5) #1
    
    #data$Target   <-compute.Target.by.method(data=data, TargetMethod=-1, w=10 , minReturn=0.1, min.duration=20,glaettung=5,visual=visual))
    #data$Target   <-compute.Target.by.method(data=data, TargetMethod=1, w=10 , minReturn=0.1, min.duration=20,glaettung=5,visual=visual))
    
    if (!is.null(TargetMethod))
      data$Target   <-compute.Target.by.method(data=data, TargetMethod=TargetMethod, w=10 , minReturn=0.1, min.duration=20,glaettung=5,visual=visual)
    
    
    mP("we have %d symbols:  %s",len(data$symbolnames),fromToS(data$prices))
    data$SAFE=SAFE
    data$universe=universe
    #data$BENCH=bench
    define.Globals(ver=1)
  }
}
####################################################################################
update.market.data<-function()
{
  loadPortfolios()  #customerDate = "Models
  merk=global_warte 
  global_warte=F
  portfolios= spl("ETF_WORLD2,Meurope,WorldL1,Mworld2,ETF_WORLD1,MWorld3,Mworld,Mmain,SektorSpdrUsaX,ETF_WORLD1")
  portfolios= spl("MWorld3,Meurope")
  lapply(portfolios,
         function(pfname) 
           if (!file.exists(sprintf("%s.rds",pfname)) )
           {
             sag("#########cupdate universe %s  #############",pfname)
             T2$new(PortfolioName = pfname,bench="", visual = T, online=T)}
         else
           mP("file schon da %s",pfname)
  )  
  global_warte = merk
}

if(F)
  update.market.data()
#......................................................................
#.----------------------------------


#ls(1)



create.ia.mm<- function(signals=NULL,mode="TIMING")#c("TIMING","FILL","XTOSAFE")
{
  signals = signals    #mach die signals in  ia verfügbar
  mode=mode
  visual=F
  
  function(hist.returns, index, nperiod)
  {
    # browser(print("ia"))
    ia=create.ia(hist.returns, index, nperiod)
    #patch die return - erwartungen
    
    if (F && exists("global_mm.tuned.ia") && global_mm.tuned.ia)
    {
      sym=ia$symbols[ia$index]
      #glaettung = get.best.glaettung(sym)
      
      if (visual) ia$expected.return = apply(ia$hist.returns, 2, mean, na.rm = T)
      
      # p=mRendite(ia$hist.returns)
      p=apply(ia$hist.returns, 2, function(ret)cumprod(ret+1)) 
      
      #   if (ncol(ia$hist.returns)>1)
      #      browser(print("#mmm"))
      #statt den expected.return gleich mean der hist.returns zu setzen wähle ich hier 
      #die smooth.spline - prognose ..
      new.return=apply(ia$hist.returns, 2, function(p) 
      {
        fit=smooth.spline(x=1:len(p), y=p,spar=0.8) # und damit dann die Quantile machen ...
        fcst=predict(fit, (len(p):(len(p)+1)))
        new.return = last(fcst$y)-last(p)
      })
      #  new.return =spline.fcst(ia$hist.returns)
      
      if (visual && ncol(ia$hist.returns) == 1)
      {
        fcst=predict(fit, (len(p):(len(p)+30)))
        p1=c(p,rep(last(p),30))
        plot(p1)
        lines(fit)
        
        #dere mean-forecast
        try(points(len(p)+1,last(p)+ia$expected.return,col="red",lwd=3))
        try(points(len(p)+1,last(p)+new.return,col="green",lwd=3))
        
        lines(fcst,col="blue")
      } 
      print("------------ create.ia.mm  new.return by smooth.spline ------------ ")
      ia$expected.return = coredata(new.return)
    }
    ia$signals=signals[nperiod,]
    ia$mode=mode
    ia
  }  
}
#---------------------------------------------------
mm1.riks.fn<-function
# equal.risk.portfolio
#' @export   
(
  ia,      	# input assumptions
  constraints		# constraints
)
{
  #if(is.null(ia$risk)) ia$risk = sqrt(diag(ia$cov))
  #risk.index = get.risky.asset.index(ia)
  
  # re-scale weights to penalize for risk		
  #ia$risk[risk.index]
  #rep(1/ia$n, ia$n)
  
  
  SAFE=  global_arg$dat$SAFE
  if (!is.null(SAFE) && len(which(colnames(ia$signals)==SAFE))>0 && len(which(ia$symbols==SAFE)) >0)
    SAFE =  global_arg$dat$SAFE  else SAFE=NULL
  
  SHORT=  global_arg$dat$SHORT
  if (!is.null(SHORT) && len(which(colnames(ia$signals)==SHORT))>0 && len(which(ia$symbols==SHORT)) >0)
    SHORT =  global_arg$dat$SHORT else SHORT=NULL
  
  mode= ia$mode
  if (mode =="XTOSAFEshort" && is.null(SHORT))
    mode = "XTOSAFE"
    
  #  mP(" ######## mm1.riks.fn ####### % s##",mode)
  #  browser()
  
  N=ncol(ia$signals)  # ncol(data$prices)
  if (mode=="FILL")
    return(coredata(ia$signals[,ia$symbols]) /ia$n)  #FILL  auch wenn nur wenige n pos allociert sind..
  if (mode=="TIMING")
    return(coredata(ia$signals[,ia$symbols]) /N )     #wenn weniger wie N pos allociert sind bleiben die leer
  if (mode=="XTOSAFE") #summe der res1 ist 1
  {
    res1=coredata(ia$signals[,ia$symbols]) /N      #wenn weniger wie N pos allociert sind bl
    if ( len(which(colnames(res1)==SAFE)) >0 && len(which(ia$symbols==SAFE)) >0 && ia$signals[,SAFE]>0)   #gehoert SAFE zu den allocierten ?
      res1[,SAFE] = res1[,SAFE] +(1-rowSums(res1)) #das was nicht allociert wurde kommt hinzu
    return(res1)  
  }
  
  if (mode=="XTOSAFEshort") #summe der res1 ist 1
  {
    # browser(print("#xtosafeShort"))
    res1=coredata(ia$signals[,ia$symbols]) /N      #wenn weniger wie N pos allociert sind bl
    REST=""
    
    
    if ( len(which(colnames(res1)==SHORT)) >0 && len(which(ia$symbols==SHORT)) >0 && ia$signals[,SHORT]>0)  #gehoert SAFE zu den allocierten ?
      REST=SHORT
    else      
      if ( len(which(colnames(res1)==SAFE)) >0 && len(which(ia$symbols==SAFE)) >0 && ia$signals[,SAFE]>0)  #gehoert SAFE zu den allocierten ?
        REST=SAFE
    if (REST !="")
    { 
      res1[,REST] = res1[,REST] +(1-rowSums(res1)) #das was nicht allociert wurde kommt hinzu
      REST.i = which(colnames(res1)==REST)
      if (last(ia$hist.return[,REST]) < last(SMA(ia$hist.return[,REST],len(ia$hist.return[,REST]))))
      {
        #wenn nicht nur der Inidkator SHORT sagt, sondern auch der lookback.len  - ia.hist.return faber negativ ist:
        #alles nach short 
        res1[,-REST.i]=0
        res1[,REST.i]=1
      }
    }
    return(res1)  
  }
  if (mode=="DECO")
  {
    res1=coredata(ia$signals[,ia$symbols]) /N      #wenn weniger wie N pos allociert sind bl
    if ( len(which(colnames(res1)==SAFE)) >0 && len(which(ia$symbols==SAFE)) >0)  #gehoert SAFE zu den allocierten ?
      res1[,SAFE] = res1[,SAFE] +(1-rowSums(res1)) #das was nicht allociert wurde kommt hinzu
    return(res1)  
  }
  # normalize weights to sum up to 1
  # set.risky.asset(x / sum(x), risk.index)
}	
#--------------------------------------------------------------

define.Globals<-function(mdata = NULL,dataSet="",bench="",SAFE="",short="",ver=0,modelDir = "")
{ 
  if (!is.null(mdata)) data= mdata
  #doof: dataSet ist der veraltete Name für modelDir
  mP("define.Globals %s",dataSet)
  FUNDIS<<- list()
  no_strategy.Performance.snapshoot <<- F
  global_dont_cashe <<- F
  global_page <<- 0
  global_disable_cloud.check <<-T
  packageLoaded <- function(name) 0 != length(grep(paste("^package:",  name, "$", sep=""), search()))
  
  if (!packageLoaded("zoo") || ! exists("global_ALL_CODE_LOADED")) #zeichen dafür dass inputconfig noch nicht gesourced wurde
    init.Code()
  
  if (length(data)<2)
  {
    print("define.Globals: first define data")
    #return()
  }
  
  if (dataSet!="") dataSet<<- dataSet
  prices <<-data$prices
  data$weight =data$prices;data$weight[]=NA
  
  if(ver==1) train.frame <<- fromToS(data$prices)
  if(ver==1) global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  else global_arg <<-list()
  global_commission <<- 0.0005   #5 basipunktesys.Run() nimmt diese commission !!!!
  global_arg$modell.par$LongShort="F"  
  if (dataSet != "") 
     global_arg$modelDir <<- dataSet
  if (modelDir != "") 
    global_arg$modelDir <<- modelDir
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  if(ver==1)global_StartDate <<-  DateS(last(data$prices)) 
  global_objectId <<-paste("TREND","xx","signal.lm") 
  global_FastTrain <<-10 #hei?t:  f?r jede dim werden 2 Schritte ausprobiert.
  
  ModelRoot <<- getwd() 
  testXLSread<<-F
  readMyClasses<<-T
  
  globalPROVIDER <<- "CSV"
  csvDatFormat<<-"%Y-%m-%d"
  globalSEP <<- ";"
  globalDEC <<- "."
  globMDataSUB<<-""
  global_xMarker <<- NULL#list(as.Date("2010-04-22"))
  
  lookback.len <<- 60
  #periodicity = 'weeks'
  #    periodicity <<- 'months'
  prefix <<- ''
  myFrame <<-""
  #BENCH=data$symbolnames[[1]] 
  print(ls(data))
  
  global_arg$dat <<-data
  global_arg$clos <<- data$prices
  global_StartDate <<-  DateS(last(data$prices))
  #globalTrainLevel <<- 3   
  if (bench !="")
  {  
    data$BENCH <- bench
  }
  if (!data$BENCH %in% data$symbolnames || len(data$BENCH %in% data$symbolnames) ==0)
    data$BENCH=bench =data$symbolnames[1]   
  
  if (SAFE !="")
  {
    if (!SAFE %in% data$symbolnames)
      SAFE = bench   
    
    data$SAFE = SAFE <<- SAFE
  }
  if (short !="")
  {
    if (short %in% data$symbolnames)
      data$SHORT = short
  }
  
  global_arg$BENCH<<-data$BENCH
  
  #-----------------
  print("prepare p and P")
  #browser()  #2017
  px<<- data$prices[,data$BENCH]; p=m.ifna.prev(mNorm(px))  #2017    p ist neuerdings funktion der lib htmltools ...
  library(TTR)
  P<<- na.omit(SMA(na.omit(ZigZag(px)),5))   #zigzag-reihe - gut für causalitäts test
  #--------------------------
  
  print("set LongShort  to F")
  global_arg$modell.par$LongShort="F"  
  
  #...........................................definier mal ein paar liste von SIT.AA-Methoden
  
  max.product.exposure <<- 0.6
  
  SET0 <<- min.risk.fns <<- list(
    
    EW.nTopk=equal.weight.portfolio,
    RP=risk.parity.portfolio,
    MD=max.div.portfolio,                      
    
    MV=min.var.portfolio,
    MVE=min.var.excel.portfolio,
    MV2=min.var2.portfolio,
    
    MC=min.corr.portfolio,
    MCE=min.corr.excel.portfolio,
    MC2=min.corr2.portfolio,
    
    MS=max.sharpe.portfolio(),
    #ERC = equal.risk.contribution.portfolio,
    
    # target retunr / risk
    TRET.12 = target.return.portfolio(12/100),                             
    TRISK.10 = target.risk.portfolio(10/100),
    
    # cluster
    C.EW = distribute.weights(equal.weight.portfolio,  cluster.group.kmeans.90),
    C.RP = distribute.weights(risk.parity.portfolio,  cluster.group.kmeans.90),
    #C.EW.kmeans = distribute.weights(equal.weight.portfolio, cluster.group.kmeans.90),
    #C.EW.FTCA = distribute.weights(equal.weight.portfolio, cluster.group.FTCA(0.5)),
    
    # rso
    RSO.RP.5 = rso.portfolio(risk.parity.portfolio, 5, 500),
    MV.RSO = rso.portfolio(min.var.portfolio, 3, 100, const.ub = max.product.exposure),
    MCE.RSO = rso.portfolio(min.corr.excel.portfolio, 3, 100, const.ub = max.product.exposure),
    
    # others
    MMaxLoss = min.maxloss.portfolio,
    MMad = min.mad.portfolio,
    MCVaR = min.cvar.portfolio,
    MCDaR = min.cdar.portfolio,
    MMadDown = min.mad.downside.portfolio,
    MRiskDown = min.risk.downside.portfolio,
    MCorCov = min.cor.insteadof.cov.portfolio
    
  )
  
  
  
  
  SET_DEFAULT <<- list(
    EW=equal.weight.portfolio,
    MVE=min.var.excel.portfolio,
    MV2=min.var2.portfolio,
    TRET.12 = target.return.portfolio(12/100),            		
    MD=max.div.portfolio,                      
    MS=max.sharpe.portfolio() ,
    C.RP = distribute.weights(risk.parity.portfolio,  cluster.group.kmeans.90),
    MMaxLoss = min.maxloss.portfolio,
    MMad = min.mad.portfolio,
    MCVaR = min.cvar.portfolio,
    MMadDown = min.mad.downside.portfolio,
    MRiskDown = min.risk.downside.portfolio
  )
  
  
  
  SET1 <<- list(
    EW=equal.weight.portfolio,
    MVE=min.var.excel.portfolio,
    TRET.12 = target.return.portfolio(12/100),          			
    TRISK.10 = target.risk.portfolio(10/100),
    RSO.RP.5 = rso.portfolio(risk.parity.portfolio, 5, 500),
    MV.RSO = rso.portfolio(min.var.portfolio, 3, 100, const.ub = max.product.exposure),
    MCE.RSO = rso.portfolio(min.corr.excel.portfolio, 3, 100, const.ub = max.product.exposure),
    C.EW = distribute.weights(equal.weight.portfolio,  cluster.group.kmeans.90),
    C.RP = distribute.weights(risk.parity.portfolio,  cluster.group.kmeans.90),
    C.EW.kmeans = distribute.weights(equal.weight.portfolio, cluster.group.kmeans.90),
    C.EW.FTCA = distribute.weights(equal.weight.portfolio, cluster.group.FTCA(0.5)),
    MS=max.sharpe.portfolio() )
  
  SET2 <<- list(
    EW=equal.weight.portfolio,
    MVE=min.var.excel.portfolio,
    TRET.12 = target.return.portfolio(12/100),          			
    # MV2=min.var2.portfolio,
    MMaxLoss = min.maxloss.portfolio,
    MS=max.sharpe.portfolio() )
  
  
  SET3 <<- list(
    EW=equal.weight.portfolio,
    #MVE=min.var.excel.portfolio,
    MV=min.var.portfolio,
    MC=min.corr.portfolio,
    C.RP = distribute.weights(risk.parity.portfolio,  cluster.group.kmeans.90),  
    #MC2=min.corr2.portfolio),
    TRET.12 = target.return.portfolio(12/100),            		
    MS=max.sharpe.portfolio() )
  
  
  
  SETCLUSTER <<- list(
    C.EW.kmeans = distribute.weights(equal.weight.portfolio, cluster.group.kmeans.90),
    C.EW.FTCA = distribute.weights(equal.weight.portfolio, cluster.group.FTCA(0.5)),
    
    C.RP.kmeans = distribute.weights(risk.parity.portfolio, cluster.group.kmeans.90),
    C.RP.FTCA = distribute.weights(risk.parity.portfolio, cluster.group.FTCA(0.5)),
    
    C.MD.kmeans = distribute.weights(max.div.portfolio, cluster.group.kmeans.90),
    C.MD.FTCA = distribute.weights(max.div.portfolio, cluster.group.FTCA(0.5)),
    
    C.MV.kmeans = distribute.weights(min.var.portfolio, cluster.group.kmeans.90),
    C.MV.FTCA = distribute.weights(min.var.portfolio, cluster.group.FTCA(0.5)),
    
    C.MVE.kmeans = distribute.weights(min.var.excel.portfolio, cluster.group.kmeans.90),
    C.MVE.FTCA = distribute.weights(min.var.excel.portfolio, cluster.group.FTCA(0.5)),
    
    C.MCE.kmeans = distribute.weights(min.corr.excel.portfolio, cluster.group.kmeans.90),
    C.MCE.FTCA = distribute.weights(min.corr.excel.portfolio, cluster.group.FTCA(0.5)),
    
    C.MS.kmeans = distribute.weights(max.sharpe.portfolio(), cluster.group.kmeans.90),
    C.MS.FTCA = distribute.weights(max.sharpe.portfolio(), cluster.group.FTCA(0.5)),
    
    C.ERC.kmeans = distribute.weights(equal.risk.contribution.portfolio, cluster.group.kmeans.90),
    C.ERC.FTCA = distribute.weights(equal.risk.contribution.portfolio, cluster.group.FTCA(0.5))		
    
  )
  
  #......................  Parameter-Default - Sets für indi.Generic()
  TSA.light <<- list(
    T.arg=list(cashed="TSA.t1")  ,#cashe die Timing-signale 
    S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),
    A.arg=list(min.risk.fns=SET1),
    R.arg=list(tsa.level="T,TSA,SA",
               report=spl("signals,one,perfBar,transMap,turnover"),
               pdf.report.models=spl("all,TSA,SA,A"),
               pdf.report=spl("NO,compareModels,periods,xls") )  ) 
  #.......................................
  
  ########################################################################################
  #mit dieser default.einstellung geht ein reporting raus-wenn ich kein experiment gewählt hab
  #und auch keine eigenen Parameter
  
  TSA.default <<- list(
    T.arg=list(),#cashed="TSA.t1")  ,#cashe die Timing-signale   #CACHEON
    S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),
    A.arg=list(min.risk.fns=SET3),
    R.arg=list(tsa.level="T,TSA,SA",
               report=spl("signals,one,perfBar,transMap,turnover"),
               pdf.report.models=spl("all,T,TSA,SA"),
               pdf.report=spl("compare,compareModels,xls,periods") )  ) 
  
  A.SA.default <<- list(
    T.arg=list(),#cashed="TSA.t1")  ,#cashe die Timing-signale 
    S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),  #1.4,   1.2
    A.arg=list(min.risk.fns=SET3),
    R.arg=list(tsa.level="T,TSA,SA",
               report=spl("signals,transMap,one,turnover"),
               report.models=spl("all"),
               pdf.report=spl("compare,xls,periods") )  ) 
  
  
  reset.cashed.rank()
  #check noch mal ob global_arg$dat richtig sitzt
  if (len(colnames(data$prices)[sort(colnames(data$prices)) != sort(global_arg$dat$symbolnames)])>0)
    sag("define.Globals: check your setup",warte=T)
  "ok"
}

######################################################################
print("########### load TSA.R  and  define.Globals() ")
if (F)
  init.Code()

if (F) 
    define.Globals()
if (F)
  list_R_functions('MLib/TSA.r')
