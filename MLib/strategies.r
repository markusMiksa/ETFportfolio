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

#######################################################################################
# http://systematicinvestor.wordpress.com/2014/02/17/probabilistic-momentum/
#######################################################################################

sit.feilscher<-function(bench,safe,lookback.len=60,visual=T,SAFE=SAFE)
{
  models=list()
  mdata=clone(data)
  
  if (is.xts(safe))  #evtl. besteht safe nicht aus einem Spaltennamen sondern einer xts
    #wo jede zeie den spaltennamen enthält der aus  -- noch ohne praktische Bedeutung
  {
    momentum = data$prices / mlag(mdata$prices, lookback.len)
    
    colnames(safe)="SELECT"
    safe[!is.finite(safe) ] <-data$BENCH
    # mx=momentum[,safe]
    
    best.mom=best=rollapplyr(merge(safe,momentum),1,function(r)
    {
      this.safe=ifelse(!is.finite(r[,1]),data$BENCH,coredata(r[,1]))
      r[,this.safe]}, by.column = F )    

    best.mom[!is.finite(best.mom)]=0

    safe=SAFE
    mdata$prices=mdata$prices[,c(bench,SAFE)]  
    momentum = mdata$prices / mlag(mdata$prices, lookback.len)  
    momentum[,SAFE] = best.mom[]
  }
  else
  {
    
    mdata$prices=mdata$prices[,c(bench,safe)]  
    momentum = mdata$prices / mlag(mdata$prices, lookback.len)  
    
  }
  mdata$weight=mdata$prices
  mdata$weight[] = NA
  
  if (ncol(mdata$weight)>2)
    {sag("bug at sit.feilscher ###########")
     browser()
  }
  mdata$weight[,bench] = momentum[,bench] > momentum[,safe]
  mdata$weight[,safe] = momentum[,bench] <= momentum[,safe]

  models$Simple  = bt.run.share(mdata, clean.signal=T,commission=0.0005)  
  
  momentum = foreach(sym = c(bench,safe), .combine="merge") %do%
    rank.probMom(mdata$prices[,sym],xtra=lookback.len)
  
  mdata$weight[] = NA
  mdata$weight[,bench] = momentum[,bench] > momentum[,safe] & momentum[,bench] >0
  mdata$weight[,safe] = momentum[,bench] <= momentum[,safe] &  momentum[,safe] >0
  models$prob  = bt.run.share(mdata, clean.signal=T,commission=0.0005)  
  
  out=NULL
  if (visual)
  {
  out=try(strategy.Performance.snapshoot(models,R.arg= TSA.default$R.arg,state="TSA",title=sprintf("TopBestModels %s - %s - win %d",bench,safe,lookback.len), data=mdata,commission=global_commission))
  
  out = rbind(out, Turnover= sprintf("%.1f",100*sapply(models, compute.turnover, mdata
)))
  }
list(modprob=models$prob,modsimple=models$Simple,perf=out)
  
}
if (F)
{
  sit.feilscher(data$BENCH,SAFE,lookback.len=60)
  sit.feilscher(data$BENCH,SAFE,lookback.len=300)
}
#..................................................................


sit.feilscher.portfolio<-function(data,SAFE="BARCLAYS",visual=T,win=60)
{
  prices = mNorm(data$prices)
  sym = colnames(prices)
  res=sit.feilscher(data$BENCH, SAFE,visual=T)
  models=list()
  models$prob=res$mod
   #colnames( res$mod$weight)
  #.... nun alle paare ...bau dabei eine gewichtsmatrix , jeweils eine spalte für sym.i
  weights=prices;weights[]=NA
  weightsSimple=prices;weightsSimple[]=NA
  
  for(sym.i in colnames(data$prices))
  {
    if (sym.i != SAFE)
      {
       #p.safe=probMom(data=data,sym.i=sym.i, sym=SAFE,lookback.len=win)
       #p.bench=probMom(data=data,sym.i=sym.i, sym=data$BENCH,lookback.len=win)
       #betterSym = iif(p.safe >= p.bench,SAFE,data$BENCH)
       betterSym=SAFE
       res=sit.feilscher(sym.i, betterSym,visual=visual,lookback.len=win,SAFE=SAFE)
       w.sym.i =res$modprob$weight[,1]
       weights[,sym.i]=coredata(w.sym.i)

       w.sym.i =res$modsimple$weight[,1]
       weightsSimple[,sym.i]=coredata(w.sym.i)
       
    }
  }
  #View(head(weights["2005"]))
  weights[,SAFE]=0; weightsSimple[,SAFE]=0
  signals = weights
  
if (T)
{
  weights = weights/ncol(weights)
  weightsSimple = weightsSimple/ncol(weights)
  
  #weights = weights/rowSums(weights,na.rm=T)
  
  weights[,SAFE] = 1- rowSums(weights,na.rm=T)
  weightsSimple[,SAFE] = 1- rowSums(weightsSimple,na.rm=T)
  
  data$weight=weightsSimple
  
  models=list()
  models$mod  = bt.run.share(data, clean.signal=F,trade.summary=T,commission=0.0005)  
  data$weight=weights
  models$modRanked  = bt.run.share(data, clean.signal=F,trade.summary=T,commission=0.0005)  
}
else #share -modus - irgendwie noch komisch ...
{
capital = 100000
weights[] = ((capital/ ncol(weights))  /   prices) * bt.exrem(signals)
data.weight=weights

models$buyHold = bt.run(data, type='share', capital=capital, trade.summary=T)
}
  
  out=try(strategy.Performance.snapshoot(models,R.arg= TSA.default$R.arg,state="TSA",title=sprintf("sit.feilscher"), data=data,commission=global_commission))
#  out = rbind(out, Turnover= sprintf("%.1f",100*sapply(models, compute.turnover, data  )))
}
#..........................................................................

if (F)
  {
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals(); SAFE="BARCLAYS"
  mPlots(data$prices)
  #spannender RUN mit win=60
  sit.feilscher.portfolio(data,SAFE=SAFE,visual=T)
    
    data = readRDS("MWorld3.rds"); define.Globals();
    data$universe= "MWorld3"; SAFE="TREASURY"
    sit.feilscher.portfolio(data,SAFE=SAFE,visual=T)
  
  sit.feilscher.portfolio(data,SAFE="EXX50_RI",visual=T,win=300)

  x=indi.Generic("signal.sit.feilscher", global_arg, par=list(lookback.len=60), visual=F, TRAINSYM =-1,period ="weeks")
}    
#####################################################################################
#eigentlist ist sit.feilscher ein signal:


##########################################################################################
if (F)  # vergleiche einige Ranking-Kritierien in einer SA-Strategie 
{
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals(); SAFE="BARCLAYS"
  reset.cashed.rank()

  #best
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),
        max.product.exposure = 0.8, safe=SAFE, onlyUsr=T, experiment="test1") 
  
  #ok
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.probMom",nTop.q=1.8,kTop.q=1.4),
          max.product.exposure = 0.8, safe=SAFE, onlyUsr=T, experiment="rank.probMom") 
  #besser
  
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.rsi",nTop.q=1.8,kTop.q=1.4),
        max.product.exposure = 0.5, safe=SAFE, onlyUsr=T, experiment="test13") 
  
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.calmar",nTop.q=1.8,kTop.q=1.4),
        max.product.exposure = 0.8, safe=SAFE, onlyUsr=T, experiment="test4") 
  
  
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.sharpe",nTop.q=1.8,kTop.q=1.4),
        max.product.exposure = 0.8, safe=SAFE, onlyUsr=T, experiment="test5") 
  
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.inTrend",nTop.q=1.8,kTop.q=1.4),
        max.product.exposure = 0.8, safe=SAFE, onlyUsr=T, experiment="rank.inTrend") 
  
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.momVol",nTop.q=1.8,kTop.q=1.4),
        max.product.exposure = 0.8, safe=SAFE, onlyUsr=T, experiment="test7") 
  
  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.faber",nTop.q=1.8,kTop.q=1.4),
        max.product.exposure = 0.8, safe=SAFE, onlyUsr=T, experiment="test8") 
  
  # rebalance to target.allocation when portfolio weights are 5% away from target.allocation
  models$smart5.all = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 5/100, 0) 
  
  
  
}

#################################################################################

if (F)  #ReStart2017
{
  universe <<- "MWorld3"; dataSet <<-"MM_March14a"
  init_TSA(universe = "MWorld3", dataSet ="MM_March14a", SAFE="TREASURY", TargetMethod=  0 , online =F, visual=T)
  #load.universe(universe=universe,online=T,visual=F)
save(data,file="MWorld3.data")
load(file="MWorld3.data")  
ls(data)
  SAFE <<- data$SAFE  
  dataSet
define.Globals(dataSet=dataSet,bench=data$BENCH,SAFE=SAFE)


  A.SA (min.risk.fns = TSA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn="rank.slope300",nTop.q=1.8,kTop.q=1.4),
        max.product.exposure = 0.8, safe=SAFE, onlyUsr=F, experiment="test1") 
  
  x=indi.Generic("signal.sit.feilscher", global_arg, par=list(lookback.len=60), visual=T, TRAINSYM =-1,period ="months",experiment="test2") 

global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten


x=indi.Generic("signal.sit.feilscher", global_arg, visual=T, TRAINSYM =-1) 
View(global_ParTable)

global_ParTable <<- NULL
TrainIndicator(global_StartDate_=global_StartDate,  opti="GRID", indiName = "signal.sit.feilscher",  visual=T, TRAINSYM =-1)# data$BENCH)  "SXEBP"



#....................................
  all.sig=  spl("signal.sit.feilscher,signal.lm,signal.mom,signal.Faber.base,signal.1.smoothing,signal.any.smoothing, signal.Price.itp,signal.Faber.dyn.hysterese,signal.drawDown1,signal.MA.3,signal.days.since.high,  signal.Faber.i2,signal.rwf,signal.SMAzlema,signal.zma")
  

  
  for(sig in all.sig)
  x=indi.Generic(sig, global_arg, visual=F, TRAINSYM =-1,period ="months",experiment=sig) 
}

#runApp("shinyGui")

##############################################################################
###############################################################################
if (F)
  list_R_functions('MLib/strategies.r')
