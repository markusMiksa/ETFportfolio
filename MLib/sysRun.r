#MARKEN:    #TSA T , #TSA TSA ,  #TSA  SA ,     #TSA A
################################################################################################################
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

if(F)
{
  ################################################################################
  #optionale User-Anweisungen:
  #.................... T.arg ...................................
  #cashed="cashName..."          #cashe die Timing-signale 
  #.................... S.arg ...................................
  #ranking = "rank.faber"        #benutzte für die rankings:  rank.faber
  #.................... A.arg ...................................
  #  min.risk.fns = list( ... )
  
  #.................... R.arg ...................................
  #welche portfolio-kombinationen sollen gerechnet werden (setzt aber voraus,dass mindestens 2 Wertpapiere analysiert werden)
  tsa.level = "T"
  tsa.level = "S"
  tsa.level = "A"
  #und folgende Kombinationen daraus:  
  tsa.level = "TSA"
  tsa.level = "SA"
  tsa.level = "A"
  
  ##############################
  #das live-reporting:  in sitPatches.r# strategy.Performance.snapshoot()
  
  R.arg=list(report=spl("signals,one,table,perfBar,transMap,turnover"))
  ############# das Abschlussreporting
  R.arg=list(report="final.report")
  # pdf/xls-Reporting in:  InputConfig3.r# pdf_Report()
  ## und wenn auch experiment (pfadname) definiert ist können pdf und xls geschrieben werden
  #darin können die modell-gruppen aufgeführt werden:
  R.arg=list(pdf.report.models=spl("NO,all,TSA,SA,A"))
  # mit jeweils folgenden Details:
  R.arg=list(pdf.report=spl("compareModels,periods,xls")) 
  
  #A
  #A.arg = list(min.risk.fns=SET2)
  
  
  
  shrink.average.david <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.average.david, s, 1)$sigma }}
  
  
  #################################################################################
}
#Beispiel
#
#x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,
#               do.assemble.Signals=T,
#               T.arg=list(cashed="TSA.t1")  ,
#               S.arg=list(ranking.fn="rank.slope300",nTop.q=4,kTop.q=3),
#               A.arg=list(min.risk.fns=SET1),
#               R.arg=list(tsa.level="TSA,SA,A",report=spl("signals,one,perfBar,transMap,turnover"),pdfä.report.models=spl("all,TSA,SA,A"),pdf.report=spl("compareModels,periods,xls") )


##################################################################################
#marken:  #SELECTION
#AA     


sys.Run<-function(prices,signal, compare=F, viewLevel=spl("portfolio,timingsys"),main="",hilfs.Indi=NULL,stopSys="",nTop=1,safe="",experiment="",signal.fun="model",data=NULL, T.arg=list(cashed=NULL), S.arg=list(ranking.fn="rank.beta",nTop=NULL,kTop=NULL,nTop.q=4,kTop.q=2), A.arg=list(min.risk.fns="NO"),R.arg=list(tsa.level="TSA",reporting.level=c("one,transactions")),visual=F,period = "months")
{
  mP("sys.Run %s ---->",viewLevel) 
 
  if (safe =="" && has(global_arg$dat,"SAFE")) 
    safe = global_arg$dat$SAFE
 if (exists("modelDir"))
   dataSet=modelDir
  
  # browser()
  prices=na.omit(prices)
  #mchart(mNorm( bt.apply.matrix(prices, function(p) scaleTo(p,c(100,200)))))
  colnames(signal)=colnames(prices)
  n=dim(prices)[2] #anzahl der Wertpapiere
  if (!exists("global_commission"))
    global_commission <<- 0.0005 #0.00001
  
  if (len(A.arg$commission))
    commission = A.arg$commission
  else
    commission =   list(cps = 0.1, fixed = 0.0, percentage = global_commission)
  #commission=global_commission
  n.vol = 60 # (offsetage)
  nsym = ncol(signal)
  n.top= round(nsym/nTop)
  k.top=n.top
  
  capital=100000
  models = list()
  
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
  else
    if( period == "days")
    {
      if (F)
      { #macht nur sinn falls business.days bank-feiertag erkennt
      dat=business.days(as.Date(index(prices[1])),as.Date(index(last(prices))))
      ep=prices[,1]; ep[]=1:shape(prices);
      busDay=ep[dat,]
      days.ends=coredata(busDay)
      }
      days.ends= endpoints(prices, 'days')
      
      days.ends = days.ends[days.ends > 0]   
      period.ends = days.ends
      period.annual.factor=356
      
    }
  if (is.null(data) || !contains(viewLevel,"portfolio"))
  {
    #mP("no data given")
    mdata = list()
    mdata$prices = prices
    
    mdata$symbolnames = pureColnames(prices)
    mdata$weight = prices; mdata$weight[]=NA
    colnames(prices)=colnames(signal)= mdata$symbolnames
    mdata$BENCH=mdata$symbolnames[[1]]
  }
  else
    {
       mdata = clone(data)
       mdata$weight = prices; mdata$weight[]=NA
    }
  
  
  
  mP("sys.Run: %s",viewLevel) ; #browser()
  if (contains(viewLevel,"portfolio"))
    mP("----------------########################### Portfolio-Analyse:")
  #browser()
  if (n > 1 )#&& experiment=="")
    if (compare || contains(viewLevel,"portfolio")) #vergleich  mit der BuyHold - strategie gewünscht  (compare==visual)
    {
      #new_Win(1)
      
      models=list()
      ############# Das Buy-Hold-Vergleichsmodell
      #browser(mP("bh"))   
      #patch  /n
      mdata$weight=prices
      mdata$weight[]=1
      mdata$execution.price=prices;mdata$execution.price[]=NA
      
      offset.D=DateS(first(signal[signal[,1] !=0,1]))
      offset.frame=sprintf("::%s",offset.D)  #stell den offset wieder auf NA 
      
      mdata$weight[offset.frame,] <- 0
      mdata$weight[1:n.vol,] <- 0
      
      capital = 100000
      mdata$weight[] = ((capital/ n)  /   prices) * bt.exrem(mdata$weight)      
      mdata$execution.price=prices; mdata$execution.price[]=NA
      mP("sys.Run: -----Buy and Hold ----")
      #browser(mP("#32"))
      if(sum(dim(mdata$prices) != dim(mdata$weight)) !=0)  #passen die felder ?
      {
      lapply(colnames(mdata$prices),function(n1) ifelse (!n1 %in% colnames(mdata$weight),print(n1),""))
      lapply(colnames(mdata$weight),function(n1) ifelse (!n1 %in% colnames(mdata$prices),print(n1),""))
      print("BUG222"); 
      browser()
      stop()
      }
      
      #2017
      mdata$weight[] = 1
      buy.hold = bt.run(mdata,capital=capital, trade.summary=T,commission=commission$percentage)	
      #old:
      #models$buyHold = bt.run(mdata, type='share', capital=capital, trade.summary=T,commission=commission$percentage)#global_commission)
      
      
      mod=models$buyHold 
      mod$trade.summary$trades 
      
      #################  DAX
      bench=mdata$BENCH
      
      mdata$weight=prices
      mdata$weight[]=NA
      mP("#MMbench %s",bench)
      
      mdata$weight[,bench]=1  #hier wird die benchmark auf 1 gesetzt
      offset.D=DateS(first(signal[signal[,1] !=0,1]))
      offset.frame=sprintf("::%s",offset.D)  #stell den offset wieder auf NA 
      mdata$weight[offset.frame,] <- 0
      capital = 100000
     # browser()#24.01.2017
      
      mdata$weight[] = ((capital/ 1)  /   prices) *mdata$weight #* bt.exrem(mdata$weight)      
      mdata$execution.price=prices; mdata$execution.price[]=NA
      mP("sys.Run: -----BENCH ---- %s",bench)
      
      
      models[[bench]] = bt.run(mdata, type='share', capital=capital, trade.summary=T,commission=commission$percentage)
      
      mod=models[[bench]] 
      mod$trade.summary$trades 
      
      ##################### TF timing - general full allocated  - NO select
      mdata$weight=prices;    mdata$weight[]=1
      mdata$execution.price=prices;mdata$execution.price[]=NA
      
      position.score = bt.apply.matrix(prices*signal, function(pS) iif(!is.na(pS) & pS > 0 , 1 ,NA))
      universe = capital*ntop.keep(position.score[period.ends,],10000, 10000) / prices[period.ends,]
      obj = portfolio.allocation.helper(prices, 
                                        periodicity = period,
                                        period.ends=period.ends,
                                        lookback.len = n.vol, 
                                        universe=universe,                                       
                                        shrinkage.fns = "ledoit.wolf.shrinkage",
                                        min.risk.fns  = list(EW.full=equal.weight.portfolio))
      
      #models=list()
      mP("sys.Run: -----timing - full allocated ----")  #TSA Tfill
      
      #lies noch mal im commission-paper nach
      
      models$Tfill = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)$models[[1]]
      
      #....................................................................................
      if(F)
      {
      create.ia.fn = create.ia.mm(signals=signal,mode = "DECO")  #MM1
      
      mdata$execution.price=prices;mdata$execution.price[]=NA
      universe=ifna(signal >0,F)
      obj = portfolio.allocation.helper(mdata$prices, 
                                        periodicity = period,
                                        period.ends=period.ends,
                                        lookback.len = n.vol, 
                                        universe=universe,
                                        create.ia.fn = create.ia.fn,
                                        shrinkage.fns = "ledoit.wolf.shrinkage",
                                        min.risk.fns =list(XtoSafe=mm1.riks.fn)
      ) 
      
      models$Tfill = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)$models[[1]]
      }
      
      # ls(models$TF)
      #  models[[1]]
      #  names(models)
      #strategy.performance.snapshoot(models, one.page=F,title="Timing 100 allocated")
      mP("Timing 100pct allocated ")
      
      
      
      #browser()  
      if (experiment!="") #.......................................................  auch weitere TSA stufen laufen lassen ------
      {
        
        ##################### TF1 timing - full single allocated  just on max.sharpe Title- NO select
        
        
        if ( F &&  S.arg$ranking.fn>0) #.........zu hoher turnover.....................
        { #hier weiter
          rank=  get.rank(S.arg$ranking.fn, prices)#*signal[]
          
          maxSharpeSym=data$BENCH
          
          #signal maskiert die preise aus, die gerade zu long systemen gehören
          #rank=bt.apply.matrix(prices,function(p) {p-SMA(p,200)})
          
          
          #welcher titel hat an welchem tag das beste ranking ?
          rank.max=(unlist(apply(rank,1,FUN=function(r) names(which(r==max(r))[1]))))
          rank.max.sym=xts(rank.max,order.by=as.Date(names(rank.max)))
          ############  
          mdata$weight = prices; mdata$weight[]=NA
          mdata$weight[]=signal
          mdata$weight[] = (1/n)*signal#*bt.exrem(signal)  
          
          head(mdata$weight)
          head(rank.max.sym)
          #fill up- mit dem jeweils besten symbol
          no=sapply(as.Date(index(rank.max.sym)),FUN=function(date)
          {
            #  browser() #?????
            maxSharpeSym=coredata(rank.max.sym[date])
            #  mP("%s %s %f %f",date, maxSharpeSym,mdata$weight[date,maxSharpeSym],rowSums(mdata$weight[date,]))
            #das symbol mit der besten maxSharpeSym kriegt das ganze Gewicht das bis 1 noch fehlt
            mdata$weight[date,maxSharpeSym] = 1+mdata$weight[date,maxSharpeSym] -rowSums(mdata$weight[date,],na.rm=T)
          })
          rowSums(mdata$weight,na.rm=T)
          
          #  mdata$weight[] =mdata$weight[]* bt.exrem(signal) #exrem entfernt zeitlich aufeinanderfolgende identische signale
          
          models$TfillSharpe = bt.run(mdata, type='weight', capital=capital, trade.summary=T,commission=commission)
          
          mP("sys.Run: -----timing - single full allocated ----")
          #  strategy.performance.snapshoot(models, one.page=F,title="Timing 100 single allocated", data=mdata)
          # mP("Timing 100pct single allocated - pressKey"))
          
          ############################## TS(sharpe)######################
          
          mdata$weight=prices;    mdata$weight[]=1
          mdata$execution.price=prices; mdata$execution.price[]=NA
          
          # position.score = bt.apply.matrix(rank, function(pS) iif(!is.na(pS) & pS > 0 , pS ,NA))
          #mPl
          position.score = bt.apply.matrix(prices*signal, function(pS) iif(!is.na(pS) & pS > 0 , rank, NA))
          
          universe = capital*ntop.keep(position.score[period.ends,],n.top, k.top) / prices[period.ends,]
          obj = portfolio.allocation.helper(prices, 
                                            periodicity = period,
                                            period.ends=period.ends,
                                            lookback.len = n.vol, 
                                            universe=universe,
                                            create.ia.fn = create.ia.fn,
                                            shrinkage.fns = "ledoit.wolf.shrinkage",
                                            min.risk.fns  = list(TS=equal.weight.portfolio))
          
          #models=list()
          mP("sys.Run: -----TS(sharpe)----")
          models$TS = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)$models[[1]]
          
          mod=models$sharpe
          mod$trade.summary$trades 
          mod$trade.summary$stats 
        }
      } #experiment != ""
      
      #strategy.performance.snapshoot(models, one.page=F,title="Timing 100 single allocated", data=mdata)
      #browser(mP("TS(sharpe)A - pressKey"))
      
      ###########################################################
      
      # 
      #  writeModelDataXls(models,xlsName=sprintf("Models/%s/Results.xls/%s.xls",dataSet,"X2"))    
      
    }  
  #if (contains(viewLevel,"portfolio"))  #<<###################
  #   browser(mP("TEST")) 
  ############################################  das reine Timing -modell #######################################################
  #das Modell für signal
  mdata$weight = prices; mdata$weight[]=NA
  mdata$weight[]=sign(signal)
  #offset.D=DateS(first(signal[signal[,1] !=0,1]))
  #offset.frame=sprintf("::%s",offset.D)  #stell den offset wieder auf NA 
  #mdata$weight[offset.frame,] <- 0
  #mdata$weight[1:n.vol,] <- 0
  
  
  
  if (n==1)
  {
    mdata$weight[] = ((capital/n) / prices) * bt.exrem(signal) #exrem entfernt zeitlich aufeinanderfolgende identische signale
    
    mdata$execution.price=prices;mdata$execution.price[]=NA
    #N=prices[,1];  N[]=rowSums(abs(signal)) #wie viele Titel sind wann investiert
    mP("sys.Run:   -----model-%s ---  commission: %f",signal.fun,commission)  ######### der Aufruf für das Timing-System
    #browser(mP("#334"))
    m1=  models[[signal.fun]] = bt.run(mdata, type='share', capital=capital, trade.summary=T,commission=commission$percentage)
  }
  else
  {
    
    #  Min.risk.fns =list(MM=mm1.riks.fn, EW=equal.weight.portfolio) 
    #stecke die aktuellen siganle zusätzlich in die ia-liste
    
   mP("TIMING") 
    create.ia.fn = create.ia.mm(signals=signal,mode="TIMING")  
    
    mdata$execution.price=prices;mdata$execution.price[]=NA
    universe=ifna(signal >0,F)
   obj=NULL
    obj = portfolio.allocation.helper(mdata$prices, 
                                      periodicity = period,
                                      period.ends=period.ends,
                                      lookback.len = n.vol, 
                                      universe=universe,
                                      create.ia.fn = create.ia.fn,#avg.create.ia.fn,#avg ist schlechter weil turnover höher...
                                      
                                      shrinkage.fns = "ledoit.wolf.shrinkage",
                                      min.risk.fns =list(Ew=mm1.riks.fn)
    ) 
   #browser()
   #MMX
   
    #Cagr*Sharpe/abs(maxDD)
    models[[signal.fun]] = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)$models$Ew
    
    #  Models=c(models[[1]],models[[2]])
    # strategy.performance.snapshoot(models, one.page=F,title="Timing 100 single allocated", data=mdata)
    
  }
  
  mod=models[[signal.fun]]
  mod$trade.summary$trades 
  mod$trade.summary$stats 
  
  #..............................................................................
  if (!is.null(safe) && safe !="" && safe %in% colnames(prices))
  {
    mP("all unused money to %s",safe)
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #immer alles Geld investieren ### falls einige flat sind, wird deren geld gleichmäßig verteilt
    #MT
    mdata$weight = prices; mdata$weight[]=NA
    if (F) #old
    {
      signal2=signal
      colnames(signal)= colnames(signal2)=colnames(prices)
      maxSignal = xts(rep(ncol(signal),len(index(signal))),index(signal))
      
      signal2[,safe] = iif(signal2[,safe]<=0, 0,  signal2[,safe]+ maxSignal  - rowSums(signal))
     
      mdata$weight[]=signal2
      mdata$weight[] = ((capital/n) / prices) * bt.exrem(signal2) #exrem entfernt zeitlich aufeinanderfolgende     
      
      iif (rowSums(mdata$weight[])>1, print("t00"),"")
      
      # rowSums(mdata$weight,na.rm=T)
      mdata$execution.price=prices;mdata$execution.price[]=NA
      models$XtoSafe = bt.run(mdata, type='share', capital=capital, trade.summary=T,commission=commission$percentage)
    } 
    mP("XTOSAFE")   #TSA T
    
    create.ia.fn = create.ia.mm(signals=signal,mode = "XTOSAFE")  
    
    mdata$execution.price=prices;mdata$execution.price[]=NA
    universe=ifna(signal >0,F)
    obj = portfolio.allocation.helper(mdata$prices, 
                                      periodicity = period,
                                      period.ends=period.ends,
                                      lookback.len = n.vol, 
                                      universe=universe,
                                      create.ia.fn = create.ia.fn,
                                      shrinkage.fns = "ledoit.wolf.shrinkage",
                                      min.risk.fns =list(XtoSafe=mm1.riks.fn,
                                                         MVE=min.var.excel.portfolio                                                         
                                                         #C.RPa = distribute.weights(risk.parity.portfolio,  cluster.group.kmeans.90)
                                                         )
    ) 
    
    
    strat = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)
    models$XtoSafe = strat$models$XtoSafe
    models$MVE = strat$models$MVE
    #print("###########a1");browser()
    #strategy.performance.snapshoot(models$XtoSafe, one.page=F,title="Timing 100 single allocated", data=mdata)
    SHORT=  global_arg$dat$SHORT
    if (F && !is.null(SHORT) && len(which(colnames(mdata$prices)==SHORT))>0 )
    {
    mP("XTOSAFEshort")   #TSA T
    
    create.ia.fn = create.ia.mm(signals=signal,mode = "XTOSAFEshort")  
    
    mdata$execution.price=prices;mdata$execution.price[]=NA
    universe=ifna(signal >0,F)
    obj = portfolio.allocation.helper(mdata$prices, 
                                      periodicity = period,
                                      period.ends=period.ends,
                                      lookback.len = n.vol, 
                                      universe=universe,
                                      create.ia.fn = create.ia.fn,
                                      shrinkage.fns = "ledoit.wolf.shrinkage",
                                      min.risk.fns =list(XtoSafeShort=mm1.riks.fn)
    ) 
    
    models$XtoSafeShort = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)$models$XtoSafeShort
    #print("###########a1");browser()
    #strategy.performance.snapshoot(models$XtoSafe, one.page=F,title="Timing 100 single allocated", data=mdata)
    }
    
    
  }
  
  #.........................................  T-Reporting
  if (contains(viewLevel,"portfolio"))
    #if (has(R.arg,"tsa.level","T"))      wird immer gebraucht
  {
    out = list()
    #browser(mP("######### %d",len(models)))
    #browser(mP("################# TEST"))
    data$symbolnames
    #models = append(models,models$TF )
    
    #      barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
    if (has(R.arg,"report","one",state="T")) #MMP
    {
      out$timing=tryM( strategy.Performance.snapshoot(models,R.arg,state="T",title=sprintf("just Timing(%s) + BuyHold + BENCH, %s",signal.fun,period),data=mdata,commission=commission$percentage,redirectedOut=experiment)    )
     # out$timing = rbind(out$timing, Turnover= sprintf("%.1f",100*sapply(models, compute.turnover, data)))
     if (has(out,"timing")) 
       out$timing = t(out$timing)
     else
       sag("sysRun-BUG-1",warte=T)
    }
    mP("just timing - compared with bench and buy and hold - EW, pressKey")
    #if (experiment !="") sag("press key",warte=T)
  }
  ####################################################################################
  ####################################################################################
  
  if (contains(viewLevel, "portfolio") && experiment!="")
  {
    #safe="DAX"
    #sr
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #if (experiment != "")
    
    #  if( has(R.arg,"tsa.level","S") || has(R.arg,"tsa.level","A"))
{
  
  # browser()
  #  mP("all assetallocs") #aal
  #  browser()
  #################################################################################################
  cluster.group = cluster.group.kmeans.90
  if (len(A.arg$min.risk.fns) != 0 &&  A.arg$min.risk.fns != "NO")
    #!has(A.arg,"min.risk.fns","NO"))
    min.risk.fns = A.arg$min.risk.fns
  else
    
    min.risk.fns = list(
      EW=equal.weight.portfolio,
      #    RP=risk.parity.portfolio,
      #MV.tim=min.var.portfolio,
      MVE=min.var.excel.portfolio,
      # target retunr / risk
      TRET.12 = target.return.portfolio(12/100),            		
      TRISK.10 = target.risk.portfolio(10/100),
      #    MD=max.div.portfolio,
      #    MC=min.corr.portfolio,
      #    MC1=min.cdar.portfolio,
      #    MC2=min.mad.downside.portfolio,
      #MO=max.omega.portfolio,  #läuft nicht, geht wohl nur mit Rdonlp2
      #MR=min.risk.portfolio,  #mittelmäig . wie MV
      #ML=min.maxloss.portfolio,  schlecht
      #MR=min.risk.downside.portfolio,  #genau wie MV
      #MP=max.geometric.return.portfolio,  #läuft nicht
      
      # MC2=min.corr2.portfolio,  #schrott
      # MCE=min.corr.excel.portfolio,    #schrott
      # cluster
      C.EW = distribute.weights(equal.weight.portfolio, cluster.group),
      C.RP = distribute.weights(risk.parity.portfolio, cluster.group),
      
      MS=max.sharpe.portfolio())
  
  names.min.risk.fns=names( min.risk.fns)
  
  #..................................................................
  
  if( has(R.arg,"tsa.level","TSA"))
  {
    mP("##### TSA ##### ")        #TSA TSA
    
    if (has(S.arg,"nTop"))
      n.top = nval(S.arg$nTop)
    else
      n.top= as.integer(n/nval(S.arg$nTop.q))
    #  n.top=n/4;
    n.top= as.integer(max(n.top,2))
    
    if (has(S.arg,"kTop"))
      k.top=nval(S.arg$kTop)
    else
      k.top=as.integer(n/nval(S.arg$kTop.q))
    k.top= as.integer(max(k.top,2))  
    ############################## timing.select.allocate
    mP(" TSA: Select with: %s , n.top %d  k.top %d  & Allocation",S.arg$ranking.fn,n.top,k.top)
    
    #universe[] = ((capital/ n)  /   prices) 
    #nur für long-only-modelle:  prices*signal maskiert alle technisch ausgestoppten
    #systeme weg - das ntop-ranking geht danach dann über pS-SMA(200)
    #ranking.fn <-function(p){p-SMA(p,200)} 
    #ranking <-function(p) {rollRegressionXTS(p,win=200)*100}
    #alternatives ranking:
    #Cagr*Sharpe/abs(maxDD)
    #MM_SCORE  RANKING
    #position.score = bt.apply.matrix(ranking, function(pS) iif(!is.na(pS) & pS > 0 , pS ,NA))
    #position.score = bt.apply.matrix(prices*signal, function(pS) iif( pS > 0 , pS-SMA(pS,200) ,NA))
    #........................ besser 
    ########## #SELECTION
    #if (has(S.arg,"ranking"))
    #  ranking.fn=S.arg$ranking.fn
    #ranking = rollRegressionXTS(prices*signal,win=300)*100000  #besser wie faber
    #einbau des rank.r - cashes
    ranking = signal * get.rank(S.arg$ranking.fn, prices) #das geht auch für alle symbole ..
    
    #ranking = bt.apply.matrix(prices*signal, function(p)  p-SMA(p,200))
    
    #..........................
    #ranking=bt.apply.matrix(prices*signal,  function(p)  rollRegressionXTS.smooth.dyn(p,300,probs=0.7))       
    
    position.score = bt.apply.matrix(ranking, function(pS) iif(!is.na(pS) & pS > 0 , pS ,NA))
    # mPlots(position.score)
    # tail(position.score )
    
    #k.top=n/2;  #ganz guter ansatz - bringt brei  rem signal.faber.baser  MEurope Universe mit MS.TSA ne SharpeRatio von 1 beim nem Umschlag von 10
    
    
    universe = capital*ntop.keep(position.score[period.ends,],n.top, k.top) / prices[period.ends,]
    
    dim(signal)
    dim(universe)
    dim(prices)
    dim(ntop.keep(position.score[period.ends,],n.top, k.top) )
    dim(prices[period.ends,])
    
    names(min.risk.fns)=sapply(names.min.risk.fns, function(x)sprintf("%s.TSA",x))
    
    obj = portfolio.allocation.helper(prices, 
                                      periodicity = period,
                                      period.ends=period.ends,
                                      lookback.len = n.vol, 
                                      universe=universe,
                                      create.ia.fn = create.ia.fn,
                                      shrinkage.fns = "ledoit.wolf.shrinkage",
                                      min.risk.fns =min.risk.fns 
    ) 
    
    #Cagr*Sharpe/abs(maxDD)
    
    models.TSA = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)$models
    
    out$TSA=try(strategy.Performance.snapshoot(models=append(list (BENCH=models[[bench]]),models.TSA),R.arg= R.arg,state="TSA",title=sprintf("Timing(%s).Selection(%s %d/%d).Allocation %s",signal.fun,S.arg$ranking.fn,n.top,k.top,period), data=mdata,commission=commission$percentage,redirectedOut=experiment))
    
    #out$TSA = rbind(out$TSA, Turnover= sprintf("%.1f",100*sapply(models, compute.turnover, data)))
    out$TSA = t(out$TSA)      
    
    
    mP("Timing.Selection.Allocation - pressKey")
    #sag("press key",warte=T)
    models$buyHold=NULL
    
    #obj = bt.shrinkage.best.sharpe('S,SA,A', 252, obj, data)
    #models = create.strategies(obj, data, dates = dates, leverage = leverage)$models
    #  list(title = title, stats = bt.summary.report(models, title, data, obj,
    #control = list(plot.weight.transition.maps = F,  plot.risk.contribution.transition.maps = F)   ) )
    
    #####################
    #auch die neuen shrinkage- ausprobieren 
    if (F)  #scheint nicht viel auszumachen !!
    {
      #http://www.systematicportfolio.com/adaptive-shrinkage
      #source("MLib/bt.adaptive.shrinkage.R")
      
      mP("source MLib/bt.adaptive.shrinkage.R")    
      S_SA_A=function(h,a) 1/3*(shrink.average.david(1)(h,a)+ sample.shrinkage(h,a) + sample.anchored.shrinkage(h,a))
      
      obj = portfolio.allocation.helper(prices, 
                                        periodicity = period,
                                        period.ends=period.ends,
                                        lookback.len = n.vol, 
                                        universe=universe,
                                        create.ia.fn = create.ia.fn,
                                        shrinkage.fns = "S_SA_A",
                                        min.risk.fns =min.risk.fns
      ) 
      
      asm_models_S = create.strategies(obj, mdata,commission=commission$percentage)$models
      strategy.performance.snapshoot(asm_models_S, one.page=T)
      browser(mP("with new shrinkage - pressKey"))
    }
  } #if( has(R.arg,"tsa.level","TSA"))
  ##################### NOtiming.select.allocate   #AA
  #browser()
  if( has(R.arg,"tsa.level","SA") )   #TSA SA
  {
    mP("##### SA ##### ")   
    #browser(mP("#AA->"))
    #ranking =rollRegressionXTS(prices,win=300)*100000  
    ranking =  get.rank(S.arg$ranking.fn, prices) #diesmal nicht prices*signal
    position.score = bt.apply.matrix(ranking, function(pS) iif(!is.na(pS) & pS > 0 , pS ,NA))
    
    #position.score = bt.apply.matrix(prices, function(pS) iif(!is.na(pS) & pS > 0 , ranking(pS) ,NA))
    universe = capital*ntop.keep(position.score[period.ends,],n.top, k.top) / prices[period.ends,]
    
    names(min.risk.fns)=sapply(names.min.risk.fns, function(x)sprintf("%s.SA",x))
    
    obj = portfolio.allocation.helper(prices, 
                                      periodicity = period,
                                      period.ends=period.ends,
                                      lookback.len = n.vol, 
                                      universe=universe,
                                      create.ia.fn = create.ia.fn,
                                      shrinkage.fns = "ledoit.wolf.shrinkage",
                                      min.risk.fns =min.risk.fns 
    ) 
    
    
    models.SA=asm_models.nonTiming = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)$models
    #pdf(file = 'report.pdf', width=8.5, height=11)
    out$SA=try(strategy.Performance.snapshoot(append(list (BENCH=models[[bench]]),models.SA), R.arg,state="SA", title=sprintf("NOTiming.Selection(%s %d/%d).Allocation %s",S.arg$ranking.fn,n.top,k.top,period), data=mdata,commission=commission$percentage,redirectedOut=experiment))
    #out$SA = rbind(out$SA , Turnover= sprintf("%.1f",100*sapply(models, compute.turnover, data)))
    out$SA = t(out$SA)      
    
    #dev.off()
    mP("NOtiming.Selection(%s %d/%d).Allocation - pressKey",S.arg$ranking.fn,n.top,k.top)
    #    sag("press key ", warte=T)
    
  } #  if( has(R.arg,"tsa.level","SA"))
  
  if( has(R.arg,"tsa.level","A") )   #TSA A
  {
    mP("##### A ##### ")   
    
    #####################NOtiming.NOselect.allocate
    #universe[]=1
    universe=prices>0
    # universe[] = ((capital/ n)  /   prices)
    names(min.risk.fns)=sapply(names.min.risk.fns, function(x)sprintf("%s.A",x))
    # browser(mP("Bug ##############>"))
    
    if (F)
      
      obj = portfolio.allocation.helper(prices, 
                                        periodicity = period,
                                        period.ends=period.ends,
                                        lookback.len = n.vol, 
                                        universe=universe,
                                        create.ia.fn = create.ia.fn,
                                        shrinkage.fns = "ledoit.wolf.shrinkage",
                                        min.risk.fns =min.risk.fns 
      )
    
    obj = portfolio.allocation.helper(mdata$prices, 
                                      periodicity = period,
                                      universe = universe,
                                      lookback.len = n.vol,
                                      create.ia.fn = create.ia.fn,
                                      #const.ub = max.product.exposure,
                                      min.risk.fns = min.risk.fns,
                                      adjust2positive.definite = F)
    
    
    models.A = create.strategies(obj, mdata,commission=commission$percentage,trade.summary=T)$models
    #pdf(file = 'report.pdf', width=8.5, height=11)
    
    try(strategy.Performance.snapshoot(append(list (BENCH=models[[bench]]),models.A), R.arg,state="A", title="NOTiming.NOSelection.just Allocation", data=mdata,commission=commission$percentage,redirectedOut=experiment))
    
    #dev.off()
    mP("NOtiming.NOSelection.Allocation:  just allocation - pressKey")
    #sag("press key",warte=T)
    
    #models = append(asm_models,models)
    
    #strategy.performance.snapshoot(models, one.page=T)
    #browser(mP("all together - pressKey"))
    
  }
  
  ###################################################################
  if (F)
  {
    cluster.group = cluster.group.kmeans.90
    
    universe=prices
    universe[]=1  #alle einschalten
    universe[] = ((capital/ n)  /   prices) #* bt.exrem(universe)
    
    
    obj = portfolio.allocation.helper(prices, 
                                      periodicity = period,
                                      period.ends=period.ends,
                                      lookback.len = n.vol, 
                                      universe=universe,
                                      create.ia.fn = create.ia.fn,
                                      shrinkage.fns = "ledoit.wolf.shrinkage",
                                      min.risk.fns =min.risk.fns 
    ) 
    asm_models.nonTiming = create.strategies(obj, mdata,commission=commission$percentage)$models
    #models = append(models,asm_models.nonTiming)
    #pdf(file = 'report.pdf', width=8.5, height=11)
    
    strategy.performance.snapshoot(asm_models.nonTiming, one.page=T)
    #mdev.off()
    
    mP("pressKey")
    #sag("press key",warte=T)
    
    #    tryM(strategy.performance.snapshoot(asm_models, T))
    #    tryM(plotbt.custom.report.part1(asm_models)  )
  }
} #if( has(R.arg,"tsa.level","S") || has(R.arg,"tsa.level","A"))
#....................................  

#if ( experiment !="" && pdf.f !="" )
#  mdev.off()

  }
#----------------------------------------------------------------------------------------


#  if (stopSys !="")  #die gehen gar nicht gut -----------------------------------------
#{
#  mdata$weight[] = NA
#  if (stopSys == "trailing.stop")
#    mdata$weight[] = custom.stop.fn(coredata(signal), coredata(prices), trailing.stop,  pstop = 0.01)
#  if (stopSys=="trailing.stop.profit.target")
#    mdata$weight[] = custom.stop.fn(coredata(signal), coredata(prices), trailing.stop.profit.target,   pstop = 1/100, pprofit = 1.5/100)

#  models$ma.cross.trailing.stop = bt.run.share(mdata, clean.signal=T, trade.summary = TRUE)
#}#-------------------------------------------------------------------------------------

#tryM(plotbt.custom.report.part1(models)  ) 

#tail(signal)
#mchart(mNorm(mdata$prices["2012"]))
#EQ<<-data.frame(models[[signal.fun]]$equity)
#plot(models[[signal.fun]]$equity)
#plotbt.custom.report.part1(models[[signal.fun]])
#strategy.performance.snapshoot(models,F)
#browser(mP("Compare"))



if (compare && has(R.arg,"report","signals"))  #hier wird compare als synonym zu "visual" benutzt ...
{
  norm_Win(1)
  #plotbt.custom.report.part1(models)
  #strategy.performance.snapshoot(models, one.page=T)
  #meine einzel-asset-model-charts
  if (contains(viewLevel, "timingsys")  )#|| len(mdata$symbolnames) ==1 )
  {
    mP("sys.Run:  plot each single Sym: ")
    #browser()
    
    #welche Spalten der hilfs.Indi gehören zum Wertpapier sym ?:
    if (len(mdata$symbolnames)==1)
    {
      sym=mdata$symbolnames[1]
      hilfs.sym = c()
      for(sy in colnames(hilfs.Indi[[1]]))
      {
        if (len(strfind(sy,sym))>0)
          hilfs.sym=c(hilfs.sym,sy)    
      }   
    }
    else 
      hilfs.sym=NULL
    
   
    sym.i=sym
    #einzel-wp-charts ausdrucken ... 

    for (sym.i in mdata$symbolnames)
      {
    
      if (is.null(hilfs.sym))
        tryM(plotSigPrice(signal=signal[,sym.i],prices=prices[,sym.i],
                          indi=hilfs.Indi[[1]],main=sprintf("%s for  %s",signal.fun,sym.i)  ))  #uneingeschränkt
    else
      tryM(plotSigPrice(signal=signal[,sym.i],prices=prices[,sym.i],
                        indi=hilfs.Indi[[1]][,hilfs.sym],main=main)  )
    page()
    }
  }
  #.......................................................
  
  #browser()
  
  #tryM(strategy.performance.snapshoot(models$buyHold))
  #if (len(mdata$symbolnames)>1)
}

if (contains(viewLevel, "portfolio") )  ################################################################################
{
  #*****************************************************************
  # Create Report
  #******************************************************************    
  #browser()
  #  model$trade.summary
  #kurzreport
  #tryM(plotbt.custom.report.part1(models)  )   
  #models$DAX=NULL
  #tryM(strategy.performance.snapshoot(models, F))
  #ls(models)
  print("final reporting >>>>>>>>>>>>>>>>>> ")
  #clr()
#  if (has(R.arg,"final.report","two"))
#    tryM(plotbt.custom.report.part2(models[[signal.fun]],main=signal.fun )  ) 
  
  
  #strategy.performance.snapshoot(models.SA, one.page=F,data=data)
  #if (F)
  #if (!has(R.arg,"pdf.report","NO"))
  
  #Lang-Report und schreiben von pdf und xls
  #  models.final = c(models,models.TSA,models.SA)
    
    mP("Start experiment Reporting %s ######>",experiment)
all.models = list()

    if (has(R.arg,"pdf.report.models","all"))
    {
      
      if (exists("models.TSA"))
        all.models = append(models,models.TSA)
      if (exists("models.SA"))
        all.models= append(all.models, models.SA)
      if (exists("models.A")&& exists("models.A"))
        all.models= append(all.models,models.A)
      #compareViewModels(models=append(models.SA,models),prices,alloc=T)
      
      #try(pdf_Report(models=all.models,experiment,data,R.arg,out=out))
      
    }
    else
    {
      no=T
      
      if (has(R.arg,"pdf.report.models","TSA") && exists("models.TSA"))
      {  all.models = append(models,models.TSA); no=F}
      if (has(R.arg,"pdf.report.models","SA") && exists("models.SA")) 
      {  all.models= append(all.models, models.SA);no=F}
      if (has(R.arg,"pdf.report.models","A") && exists("models.A"))
      {   all.models= append(all.models,models.A)  ;no=F}  
      #if(!no )
      #  try(pdf_Report(models=all.models,experiment,data,R.arg,out=out))
    }
    
    if (len(all.models)==0)
      all.models = models
    models=all.models
    
#  custom.period.chart(models) 

#clr()

if (experiment == "")
  {
   file=sprintf("Models/%s/Results.xls/%s.data",dataSet,signal.fun)
if (F)
{
   all.models=list()
   #all.models[[sprintf("Tfill_%s",signal.fun)]] = models$Tfill 
   #all.models[[sprintf("XtoSafe_%s",signal.fun)]] = models$XtoSafe
   
   all.models[[signal.fun]] = models[[signal.fun]]
   all.models$XtoSafe = models$XtoSafe
   all.models$Tfill = models$Tfill 
   
   models = all.models
}
   fname.xls= signal.fun
  }
else
{
  file=sprintf("Models/%s/Results.xls/%s.data",dataSet,experiment)
  fname.xls=experiment
}
dir.create(dirname(file),recursive=T)
save(models,file=file) #"Models/EM4_MAERZ_C/Results.xls/test_signal.sit.feilscher_rank.slope300_F.data"

#models.final = dels2flatList(all.models,datafile="all")
#out$res= Performance.eq(na.omit(mROC(Equity)),do.print=F)
#writeModelDataXls(models.final,xlsName=sprintf("Models/%s/Results.xls/%s_TSA.xls",dataSet,experiment),out=out)

writeModelDataXls2(models,xlsName=sprintf("Models/%s/Results.xls/%s_TSA.xls",dataSet,fname.xls),out=NULL,data=mdata)


######################################################################################################################

if (experiment !="")
{
#mm1
print("RiskReturnScatter")

if (safe =="") safe = data$bench
sprices=prices[,c(data$BENCH,safe)]

Equity=list2xts(models,"equity")
colnames(Equity) = names(models)

norm_Win()
#Cols=c(1:(len(models)+len(colnames(sprices))))
Cols=getColSet(len(models)+len(colnames(sprices)))  #ok

try(chart.RiskReturnScatter(merge(mROC(Equity),mROC(sprices)),  main = sprintf("Compare models"), ,colorset = Cols))


# Plot Portfolio Turnover for each strategy
if (F)
{
  layout(1)
  barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
  
  pdf_Report(experiment)
  #mdev.off()
}
# mdev.off()
#clr()  #alle Kanäle schlieesen
  }
}

if ( experiment !="" && pdf.f !="" )
  mdev.off()

ret = mROC( models[[signal.fun]]$equity)
eq = models[[signal.fun]]$equity 
#as.double( as.double(last(eq,1))^(1/compute.nyears(eq)) - 1 )
cgar=compute.cagr(eq)*100
calmar = abs(compute.calmar(eq))*sign(cgar);

if (len(mdata$symbolnames)>1)
{
  mP("  ---- MultiAsset-Results---of %d series on  %d days, commission = %f",dim(eq)[1],dim(eq)[2],global_commission)
  nt=try(as.numeric(numTrades(signal)$allT))
  if (len(nt)==0 || nt==0)
    nt = 1
  mP("%d Trades #",nt)
  
  mP ("Haltedauer %f Tage", (n*len(ret[,1])) / nt)
  mP("sharpe: %f",compute.sharpe(ret))
  print(SharpeRatio(ret,Rf=0,p=0.95))
  print("..")
  
  
  mP("Cgar %f",compute.cagr(eq)*100)
  mP("MaxDD %f %f",compute.max.drawdown(eq), maxDrawdown(ret))
  mP("calmar %f",calmar)
}

if (is.na(calmar))
{
  mP("WARING:  Tquality is NA")
  models[[signal.fun]]$Tquality =-1000
}
else
  models[[signal.fun]]$Tquality =calmar   #Qualtitätskriteriumg für den Optimierer  (der sucht minima)
#models[[signal.fun]]$Signal=signal  ##??
#Mquality
mP("------------------------------------>>>Tquality= %f",models[[signal.fun]]$Tquality)

return(models[[signal.fun]])


}
#sys.Run<-cmpfun(sys.Run_) #compilier die Funktion

#################################################################################
##################################################################################

#.........................................................
#http://stackoverflow.com/questions/5735541/r-adding-page-numbers-to-pdf
#adding page numbers to pdf
##
page <-function(first=F)
{
return("no page")

  if (first)
    global_page <<- 1
  try(
  {
  mtext(side = 3, text = sprintf("Page %d",global_page), outer = T,cex=0.5) 
  global_page<<- global_page+1
  })
}
#-----------------------------------------------------------------------------------
strategy.snapshot.custom <-  function(models, n = 0, title = NULL,commission=0,data=NULL,period="") {
  norm_Win()
  title=sprintf("%s period %s",title,period)
 # browser()
  if (!is.null(title))
    try(textplot(title))
  
  if (n > 0)
    models = models[ as.vector(matrix(1:len(models),ncol=n, byrow=T)) ]  

 layout(1:1)  
 plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
 page()
 
 layout(1:2)   
 out = plotbt.strategy.sidebyside(models,return.table=T,title=title, data=data)#,make.plot=T)
 #out = rbind(out, Turnover= sprintf("%.1f",100*sapply(models, compute.turnover, data)))
 page()
 print(out)
 norm_Win()
 
  layout(1:min(5,len(models)))
 m1=0
  for(m in names(models)) {
    m1=m1+1
    w1=dim(models[[m]]$weight)[2]
    mP("%s %d",m,w1)
    plotbt.transition.map(models[[m]]$weight, name=m)
    
    if (data$BENCH %in% colnames(data$prices))
    {
      lines( scaleTo(close=data$prices[,data$BENCH],c(0,100)))
      legend('topright', legend = m, bty = 'n')
    }
    
    legend('topright', legend = m, bty = 'n')
    if ( m1 %% 5 == 0 && m1 < len(names(models)))
      page()
  }
 page()
  norm_Win()
  # writeModelDataXls(models,xlsName=sprintf("%s.xls",experiment),out=out)
  barplot.with.labels(sapply(models, compute.turnover, data), sprintf('Average Annual Portfolio Turnover, commission: %f',commission), T)
  page()
  performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,T))
 page()
  custom.period.chart(models)
 ergebnisse = t(data.frame(out))
  
  ergebnisse
}
###############################################################################
#die SA- und A Stufe - über S.arg könne an stelle von lm() beliebige andere
#ranking-kriterien eingespielt werden (siehe in TSA.r SET_DEFAULT ..)
###############################################################################
A.SA <- function(min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
                 S.arg=A.SA.default$S.arg,
                 experiment="",  max.product.exposure = 0.8, safe=SAFE,onlyUsr=F,all=F,period = "months") 
{
  if (safe =="") safe = data$bench
  if (all) min.risk.fns = SET0
browser()  
  #*****************************************************************
  # Load historical data
  #****************************************************************** 
  if(F)
  {
    load.packages('quantmod')
    
    # 10 funds
    tickers = spl('Us.Eq = VTI + VTSMX,
                  Eurpoe.Eq = IEV + FIEUX,
                  Japan.Eq = EWJ + FJPNX,
                  Emer.Eq = EEM + VEIEX,
                  Re = RWX + VNQ + VGSIX,		
                  Com = DBC + QRAAX,
                  Gold = GLD + SCGDX,
                  Long.Tr = TLT + VUSTX,
                  Mid.Tr = IEF + VFITX,
                  Short.Tr = SHY + VFISX') 
    
    start.date = 1998
    
    dates = paste(start.date,'::',sep='') 
    
    data <- new.env()
    getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
    #bt.start.dates(data)
    for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
    bt.prep(data, align='keep.all', dates=paste(start.date-2,':12::',sep=''), fill.gaps = T)
    data$BENCH="Eurpoe.Eq"
    SAFE <<-   "Short.TR" 
  }
  #*****************************************************************
  # Setup
  #****************************************************************** 		
  prices = na.omit(data$prices) 
  #data$execution.price=data$prices
  data$execution.price=data$weight =data$prices;data$weight[]=NA
  
  
  n = ncol(prices)
  nperiods = nrow(prices)
  
  
  periodicity = 'quarters'
  periodicity = 'months'
  periodicity = period
  
  period.ends = endpoints(prices, periodicity)
  period.ends = period.ends[period.ends > 0]
  
  
  
  if (experiment !="")
  {
    pdf.f <- sprintf("Models/%s/SA_%s.pdf", dataSet, experiment)
    mP("Write Data Charts to %s",pdf.f)
    dir.create(dirname(pdf.f),recursive=T)
    
    #  browser()
    pdf(file = pdf.f, paper="a4r",width=0, height=0)  
  }
  page(first=T)
  
  textPlot.datainfo(period)
  
  purePlot(mNorm(data$prices[,c(data$BENCH,safe)]),main=sprintf("BENCH: %s + SAFE:%s period: %s",data$BENCH,safe,period))
  page()
  purePlot(mNorm(data$prices),main=sprintf("Universe",data$BENCH,safe))
  page()
  
  #*****************************************************************
  # Input Assumptions
  #****************************************************************** 	
  lookback.len = 120
  create.ia.fn =  create.ia.fn = create.ia.mm(signals=signal,mode = "A.SA")#create.ia
  
  # input assumptions are averaged on 20, 40, 60 days using 1 day lag
  ia.array = c(20,40,60)
  avg.create.ia.fn = create.ia.averaged(ia.array, 1)
  
  #*****************************************************************
  # Momentum
  #****************************************************************** 	
  universe = prices>0
  
  mom.lookback.len = 120	
  momentum = prices / mlag(prices, mom.lookback.len) - 1
  mom.universe = ifna(momentum > 0, F)
  
  # momentum is averaged on 20,60,120,250 days using 3 day lag
  mom.array = c(20,60,120,250)	
  avg.momentum = momentum.averaged(prices, mom.array, 3)
  avgmom.universe = ifna(avg.momentum > 0, F)
  
  #*****************************************************************
  # lm-filter
  #******************************************************************   
  
  nTop.q=1.4   ; kTop.q=1.2
  n.top=n/nTop.q;  k.top= n/kTop.q 
  
  if (has(S.arg,"nTop"))
    n.top = nval(S.arg$nTop)
  else
    if (has(S.arg,"nTop.q"))
      n.top= as.integer(n/nval(S.arg$nTop.q))
  #  n.top=n/4;
  n.top= as.integer(max(n.top,2))
  
  if (has(S.arg,"kTop"))
    k.top=nval(S.arg$kTop)
  else
    if (has(S.arg,"kTop.q"))
      k.top=as.integer(n/nval(S.arg$kTop.q))
  k.top= as.integer(max(k.top,2))  
  
  signal=1
  ranking = signal* get.rank(S.arg$ranking.fn, prices) #das geht auch für alle symbole ..
  position.score = bt.apply.matrix(ranking, function(pS) iif(!is.na(pS) & pS > 0 , pS ,NA))
  
  uni_lm = ifna(ntop.keep(position.score[period.ends,],n.top, k.top)  >0,F)
  
  
  #*****************************************************************
  # Algos
  #****************************************************************** 	
  if (is.null(min.risk.fns))
    min.risk.fns = list(
      EW = equal.weight.portfolio,
      MV = min.var.portfolio,
      MCE = min.corr.excel.portfolio,
      
      MV.RSO = rso.portfolio(min.var.portfolio, 3, 100, const.ub = max.product.exposure),
      MCE.RSO = rso.portfolio(min.corr.excel.portfolio, 3, 100, const.ub = max.product.exposure)
    )
  
  #min.risk.fns=SET0 #2
  out=list()
  models=list()
  name="all"
  
  shrinkage.fns = "ledoit.wolf.shrinkage" #'sample.shrinkage'
  names.min.risk.fns=names(min.risk.fns)
 # cloud.check(universe)
  
  #......................................
  if (!onlyUsr)
  {
    names(min.risk.fns)=sapply(names.min.risk.fns, function(x)sprintf("%s.A",x))
    
    obj = portfolio.allocation.helper(data$prices, 
                                      periodicity = periodicity,
                                      universe = universe,
                                      lookback.len = lookback.len,
                                      create.ia.fn = create.ia.fn,#avg.create.ia.fn,#avg ist schlechter weil turnover höher...
                                      const.ub = max.product.exposure,
                                      min.risk.fns = min.risk.fns,
                                      shrinkage.fns=shrinkage.fns,
                                      adjust2positive.definite = F
                                    
    )
    models$A = create.strategies(obj, data, trade.summary=T,commission=global_commission)$models
    models.final = c(models$A)
    out$A= strategy.snapshot.custom(models.final, len(min.risk.fns), 'A',commission=global_commission,data=data,period=period)
    
    #----------------------------------
    #if (F)  #etwas kleinerer turnover aber auch kleinerer sharpe
    names(min.risk.fns)=sapply(names.min.risk.fns, function(x)sprintf("%s.S.amom.A",x))
    
    obj = portfolio.allocation.helper(data$prices, 
                                      periodicity = periodicity,
                                      universe = mom.universe,  #avgmom.universe
                                      lookback.len = lookback.len,
                                      create.ia.fn = create.ia.fn,#avg.create.ia.fn,#avg ist schlechter weil turnover höher...
                                      const.ub = max.product.exposure,
                                      min.risk.fns = min.risk.fns,
                                      shrinkage.fns=shrinkage.fns,
                                      adjust2positive.definite = F
    )
    models$S_amom_A = create.strategies(obj, data, trade.summary=T,commission=global_commission)$models
    models.final = c(models$S_amom_A)
    filter=  usrFilter=sprintf("Momentum Filter %s n,top: %d, k.top: %d", S.arg$ranking.fn,n.top,k.top)
    out$S_amom_A= strategy.snapshot.custom(models.final, len(min.risk.fns), filter,commission=global_commission,data=data,period=period)
  } #onlyUsr
  #----------------------------------
  names(min.risk.fns)=sapply(names.min.risk.fns, function(x)sprintf("%s.S.usr.A",x))
  
  obj = portfolio.allocation.helper(data$prices, 
                                    periodicity = periodicity,
                                    universe = uni_lm,
                                    lookback.len = lookback.len,
                                    create.ia.fn = create.ia.fn,#avg.create.ia.fn,#avg ist schlechter weil turnover höher...
                                    const.ub = max.product.exposure,
                                    min.risk.fns = min.risk.fns,
                                    shrinkage.fns=shrinkage.fns,
                                    adjust2positive.definite = F
  )
  models$S_lm_A = create.strategies(obj, data, trade.summary=T,commission=global_commission)$models
  
  models.final = c(models$S_lm_A)
  usrFilter=sprintf("usr Filter %s n,top: %d, k.top: %d", S.arg$ranking.fn,n.top,k.top)
  out$S_lm_A= strategy.snapshot.custom(models.final, len(min.risk.fns), usrFilter,commission=global_commission,data=data,period=period)
  #.........................................................................
  ###############################################################
  
  
  #models.final = c(models$A,models$S_amom_A,models$S_lm_A)
  
  #all.out= strategy.snapshot.custom(models.final, len(min.risk.fns), 'lm Filter')
  #Cols=rainbow(len(colnames(prices)), start=0, end=.9) #
  
  sprices=prices[,c(data$BENCH,safe)]
  Equity = foreach(mod = models,.combine = "merge") %do% 
{
  eq=  list2xts(mod,"equity")
  names(eq) = names(mod)
  eq
}

print("RiskReturnScatter")
norm_Win()
Cols=c(1:(len(models)+len(colnames(sprices))))
try(chart.RiskReturnScatter(merge(mROC(Equity),mROC(sprices)),  main = sprintf("Compare models"), ,colorset = Cols))
page()
#  custom.period.chart(models) 

if (experiment !="")
  clr()

file=sprintf("Models/%s/Results.xls/%s.data",dataSet,experiment)
dir.create(dirname(file),recursive=T)
save(models,file=file)

#out$res= Performance.eq(na.omit(mROC(Equity)),do.print=F)
if (!onlyUsr)
{  
  models.final = c(models$A)
  writeModelDataXls(models.final,xlsName=sprintf("Models/%s/Results.xls/%s_A.xls",dataSet,experiment),out=out)
  
  models.final = c(models$S_amom_A)
  writeModelDataXls(models.final,xlsName=sprintf("Models/%s/Results.xls/%s_S_amomA.xls",dataSet,experiment),out=out)
}

models.final = c(models$S_lm_A)
writeModelDataXls(models.final,xlsName=sprintf("Models/%s/Results.xls/%s_S_lm_A.xls",dataSet,experiment),out=out)



#models.final = c(models$A,models$S_amom_A,models$S_lm_A)
#writeModelDataXls(models.final,xlsName=sprintf("Models/%s/Results.xls/%s_S_amomA.xls",dataSet,experiment),out=out)


#compareViewModels(models)

sag("ok so weit")
}

#...............................................................................

if (F)
  A.SA() #load("o:\\R\\Nuggets\\Eckhard\\Models\\HuAFeb_1\\Results.xls\\S_lm_A.data")
if(F)
  source("MLIB/TSA.r")

#########################################################################

#bt.max.deviation.rebalancing()
#bt.rebalancing.test ()

#################################################################################
##################################################################################

mP("########### load sysRun.R")
if (F)
  list_R_functions('MLib/sysRun.r')
