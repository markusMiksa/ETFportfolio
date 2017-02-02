##############################################################################
if (F)
{
dax=data$prices[,"DAX"]  
fromTo(dax)

plot(dax)
dax.i=na.omit(coarse.code(dax,b=10))+100
dax.i[,1]=na.omit(as.numeric(dax.i[,1]))
plot(dax.i)
rle.dax=runLengthEnc2(dax.i)
mPlots(dax,dax.i,rle.dax)

dax.i=na.omit(coarse.code(dax,b=100))+100
dax.i[,1]=na.omit(as.numeric(dax.i[,1]))

himi= runMax((na.omit(dax.i)),n=60)
lomi= runMin((na.omit(dax.i)),n=60)

rle.himi=runLengthEnc2(himi)
rle.lomi=runLengthEnc2(lomi)
mPlots(merge(dax.i,himi,lomi),merge(rle.himi,rle.lomi),rle.himi+rle.lomi)

mPlots(rle.himi+rle.lomi,merge(dax.i,himi,lomi))
rle.s=rle.himi+rle.lomi
rle.s.thresh=rle.s[rle.s>70]
amark(as.Date(index(rle.s.thresh)))
lines(dax.i)

mPlots(merge(dax.i,himi,lomi),mROC(himi),mROC(lomi))
rle.s=EMA(abs(mROC(himi))+abs(mROC(lomi)),10)*100
rle.s.thresh=rle.s[rle.s>0.5]
mPlots(rle.s,rle.s.thresh,dax.i)
amark(as.Date(index(rle.s.thresh)))
lines(dax.i)

plot(wLens,type="h")
hl= HighLows(dax,2,visual=F)
plot(dax)
wlen=50
points(dax[hl$highs],col="red",lwd=1)
points(dax_[hl$lows],col="blue",lwd=1)
dim(dax[hl$highs])
wLens=rollapply(dax.i[hl$highs],FUN=find.wlen,width=wlen,target.range=7) #wird mit target.range
mPlots(wLens,dax)
wLens.t=wLens[wLens>3]
amark(as.Date(index(wLens.t)))
lines(dax)





prices=price=na.omit(data$DAX)
lomi= runMin(Lo(na.omit(price)),n=120) #lag m??ig ok
himi= runMax(Hi(na.omit(price)),n=120)
purePlot(na.omit(Cl(price)),lomi,himi)
mchart(merge(Cl(price),lomi,himi))
mid=himi-(himi-lomi)/2
itp=in.Trend.pos(Cl(prices), merge(mid,lomi,himi))

mPlots(merge(Cl(price),lomi,himi),itp)
mchart2(Cl(prices),itp)

prices=na.omit(data$DAX)

bb=BBands(Cl(prices),n=200,maType = "SMA")
mPlots(Cl(prices),bb[,c("mavg","dn","up")])
itp.bb=in.Trend.pos(Cl(prices), bb[,c("mavg","dn","up")])
mPlots(merge(Cl(prices),bb[,-4]),bb[,"pctB"],itp.bb)
mchart2(Cl(prices),itp.bb)

chartSeries(Cl(prices),theme="white")
#addTA(scaleTo(itp,range(Cl(prices),na.rm=T)),on=1,lwd=2,col="red")
addTA(BBands(Cl(prices),n=200,maType = "SMA") ,on=-1)

runLengthEnc2(dax.i)




if (last(lomi) == 0 )
{
  price[,3]=Cl(price)
  lomi= runMin(Lo(na.omit(price)),n=12)
}
if (visual)
  mchart(merge( lomi,himi, Cl(price)))

lows=HighLows(lomi, maxdd=5,visual=visual)$lows #5
Lows=Lo(price[lows])
price = Lo(price) #damit die Chart-warnings ausbleiben
price[lows,1]
#die lage der lows
lomi=lomi[lows]
dlomi=sign(ROC(lomi,n=1))
rl=runLengthEnc2(dlomi)
rl[rl==-1]<-0  #auch das erste Low-Extremum einer Kette mitnehmen, dass noch keine Vo




s<<-trend.analysis(na.omit(dax), visual=T)



  
  global_arg=list()
  global_arg$clos=na.omit(Cl(data$DAX["2004::2012"]))
  colnames(global_arg$clos)=c("DAX")
  fromTo(global_arg$clos)
  
  x=indi.Generic("signal.glaettung", global_arg, par=list(glaettung=2,visual=T, TRAINSYM ="DAX"))#,safe="REX")


plot(dax)

#31.10.2013
x=any.smoothing(dax,glaettung = 300,dw=2,visual=T,fn="ZLEMA",fn2="SMA",glaettung2=0,onlyLong=F,q.w="#50%")

}
##############################################################################


if (F)
{

  x=indi.Generic("signal.monthlyRoc", global_arg, par=list(mlen=1,sma.w=200),visual=T,TRAINSYM =-1,safe="TREASURY")
  
  run.randomForest(data)   #<<<<<<<<<<<<<<<<<<<<<
  
  
  x=indi.Generic("signal.randomForest", global_arg, par=list(Target=Targets),visual=T, TRAINSYM =-1)  
  
  monthly.forecast()

  x=indi.Generic("signal.forecast", global_arg, par=list(foretype=0),visual=T,TRAINSYM ="DAX")
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX")

  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,experiment="xx")
  
  #test.hysterese.faber()

  x=indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(q.w=60),visual=T, TRAINSYM ="DAX")
  
  
  x=indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(q.w=100),visual=T, TRAINSYM =-1)
  
  x=indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(q.w=40),visual=T, TRAINSYM =-1)

  x=indi.Generic("signal.smooth.dyn.hysterese", global_arg, par=list(q.w=100),visual=T, TRAINSYM ="DAX")
x=indi.Generic("signal.smooth.dyn.hysterese", global_arg, par=list(q.w=100),visual=T, TRAINSYM =-1)
  
  
  r=   fast.smoothing(p=dax,visual=T)
  #baut phantastische Trainings-signale
  r=   fast.smoothing(p=dax,visual=T,glaettung=10)

  dax = data[["DAX"]]
  data$prices= data.info(data)
  
  data$Target   <- compute.Target.by.method(data=data, TargetMethod=0)
  
  #20131004
  find.support.resistance(dax,dax[1200])
  
  find.best.series(data,visual =T)#,date.last="20100101")  
  find.best.series(data,visual =F,date.last="20100101")
  find.best.series(data,visual =T,date.last="20120101",min.len=100,saveDir="Plots/BEST")
  find.best.series(data,visual =T,date.last="20120101",min.len=100,sym="MDAX",glaettung=5,saveDir="Plots/BEST")
  krisis=mdax[mdax==max(mdax["::2010"])]
  amark(krisis,col="blue")
  find.best.series(data, visual=T, min.len=100,sym="MDAX",date.last=DateS(krisis),glaettung=5,saveDir="Plots/BEST")
  
  mGetTickers("Rex",online=T)
  getSymbols.YahooDe("Rex") #der liefert die volle L?nge
  getSymbols.YahooDe("^GREXP")
  #vorsicht das ist noch ein Bug wg doAdjust_  ..  der schaltet eher zuf?llig
  #das sieht man bei
  #oil from Quandl
  MeuropeX <<- T2$new(PortfolioName = "MeuropeX", bench="DAX",visual = F, online=T)
  
  find.best.series(data, visual=F, date.last="2004-11-30",min.len=100)
  find.best.series(data, visual=F, date.last="2011-02-28",min.len=100)
  
  find.best.series(data, visual=F, min.len=100)
  
#-----> produziere ein ganze Cloud f?r ein ganzes universum  
  loadPortfolios(sprintf("%s/Models",getwd()))   #"o:/r/Nuggets/Eckhard/Models" 
  #Rohdaten laden  
  Meurope <<- T2$new(PortfolioName = "Meurope", bench="DAX",visual = F, online=F)
  data<-Meurope$t0data
  data$prices=data.info(data)
  purePlot(mNorm(na.omit(data$prices)))
  
  frame="2010-01-01::2010-02-01"
  frame="2006::2012"
  frame="2002::2012"
  
  make.cloud(data,cloudName="EU_test",frame="2006::2012",min.len=100,freq="months")
  
  make.cloud(data,cloudName="Meurope_2002_2012",frame=frame,min.len=100,freq="months",visual=T,TargetMethod=0, preload=F)
  #was mach ich wenn min.len unterschritten wurde ??
  
#<--------------------------------------------------------------------
  #zum Anreichern der Cloud mit fundamentals und macro-indizes:

  fm.fund.factor.test()
  fm.fund.data.test()

  
  
  #firmenfundamtentals runterladen und intrinsic-value berechen
  data.fund <- new.env()
  symbol="AAPL"
  exchange=""
  csv.data=fundamental.dcf(data.fund,symbol,exchange)
  write.csv(csv.data,file=sprintf("%s%s_dcfGc.csv",p.dir,symbol),sep=";",row.names=format(index(csv.data)))
  ls(data.fund)
#macrodaten  - auch von eurostat runterladen  
  
  #Wachstumsrate des realen BIP
  WachstumsratedesrealenBIP = getEurostatRCV("tec00115")
  plot(WachstumsratedesrealenBIP[,c("time","value")],main="Wachstumsrate des realen BIP")    #sehr spannendes bild 

  #------------------------------------------------------------------
  #improved-faber weiter bauen .. fehltrades in horinzontal-lage- vermeiden
  
  x=indi.Generic("signal.Faber.i", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX")
  
  x=indi.Generic("signal.Faber.i2", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX")
  
  sr= find.support.resistance(p,nowP=p[1800])
  find.support.resistance(p,nowP=dax[1300])
  
  #------------ reparatur von sys.Run() wenn experiment != "" wird intensiv reported (xls+pdf)
  x=indi.Generic("signal.Faber.i", global_arg, par=list(sma.w=200),visual=F, TRAINSYM =-1)
  
  x=indi.Generic("signal.Faber.i", global_arg, par=list(sma.w=200),visual=F, TRAINSYM =-1,experiment ="mm1",safe="DAX")
 
  #der faber mit dyn.schwelle
  
  x=indi.Generic("signal.any.smoothing", global_arg, 
                 par=list(glaettung=200,glaettung2=0,dw=2),
                 xarg=list(fn="SMA",fn2="",onlyLong=T,q.w="50%"),
                 visual=T, TRAINSYM =-1, experiment="",nTop=2)
  
  
  
  x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="ZLEMA",fn2="SMA",glaettung2=50,onlyLong=T)
  

  #die TSA
  commission = list(cps = 0.1, fixed = 0.0, percentage = global_commission)
  
  x=indi.Generic("signal.any.smoothing", global_arg, 
                 par=list(glaettung=200,glaettung2=50,dw=2),
                 xarg=list(fn="ZLEMA",fn2="SMA",onlyLong=T,q.w="50%"),
                 visual=T, TRAINSYM =-1, experiment="zlema_sma",nTop=2)

  
  x=indi.Generic("signal.any.smoothing", global_arg, 
                 par=list(glaettung=200,glaettung2=50,dw=2),
                 xarg=list(fn="ZLEMA",fn2="SMA",onlyLong=T,q.w="50%"),
                 visual=T, TRAINSYM =-1, experiment="zlema_sma",nTop=2)
  

  x=indi.Generic("signal.any.smoothing", global_arg, 
                 par=list(glaettung=200,glaettung2=50,dw=2),
                 xarg=list(fn="ZLEMA",fn2="SMA",onlyLong=T,q.w="50%"),
                 T.arg = list(stop.on="stopsys.sma200"),
                 S.arg = list(nTop=-2, kTop=0, rank.fn = "rank.momVol"),
                 visual=T, TRAINSYM =-1, experiment="",
                 A.arg = list(alloc.at="months", 
                 commission = list(cps = 0.1, fixed = 0.0, percentage = global_commission))
                 )
  
  bt.apply.matrix(data$prices,function(sym)get.cashed.rank("rank.faber",sym))

  xx= signal.month =foreach(sym=colnames(data$prices),.combine="cbind") %do% 
{  
  
  res= get.cashed.rank("rank.faber",sym)
  mP("%s %d",sym,shape(res))
  browser()
  return(res)
}
  x1=get.cashed.rank("rank.faber","DAX")
  rank=  get.cashed.rank("rank.faber")#*signal[]
  
  
  #verbesser:  wenn die beiden fn fast horizontal sind - taugt das cut signal nicht
  #mache also ein funktion:  extra-Test,  und rank
  
  #den gelagten smooth als  faber-sma nehmen - zus?tzlich mit seiner steigung arbeiten  
  

  plot.table(as.matrix(DataInfo))
  
  #die iPM
  x=indi.Generic("signal.any.smoothing", global_arg, 
                 par=list(glaettung=200,glaettung2=50,dw=2),
                 xarg=list(fn="ZLEMA",fn2="lag",onlyLong=T,q.w="50%"),
                 visual=T, TRAINSYM =-1, experiment="zlema_sma",nTop=2)
  
  #funktion:  atTrigger  zusatzcheck ob er wirklich drehen sll
  #           stopOn (wenn er aus der range kracht )
  #           rank 
  #           select=list(nTop,nK,  "divide")
  
  
  x=indi.Generic("signal.any.smoothing", global_arg, 
                 par=list(glaettung=200,glaettung2=50,dw=2),
                 xarg=list(fn="ZLEMA",fn2="SMA",onlyLong=T,q.w="50%"),
                 visual=T, TRAINSYM =-1, experiment="zlema_sma",nTop=2)
  
  
  
  #teste unterschiedliche  rankings
  sit.AdaptiveAssetAllocation(    my.rank="rank.faber,rank.momVol,rank.sharpe")
  sit.AdaptiveAssetAllocation(    my.rank="rank.omega,rank.calmar,rank.kelly,rank.es")
  sit.AdaptiveAssetAllocation(    my.rank="rank.es,rank.slope,rank.momVol,rank.faber")
  
  make.cloud(data,cloudName="EU_cloud",frame="2010-01-01::2010-02-01",min.len=100,freq="months",visual=T,TargetMethod=0, preload=T)
             
             
  res=in.Trend.pos(dax,visual=T)
  plot(dax);plot(res)
  
  
  global_arg$dat[,"REX"]
  global_arg$dat$symbolnames=
  global_arg$dat$symbolnames[-
  which(global_arg$dat$symbolnames=="REX")]
  
  
  
  
  x=any.smoothing(dax,glaettung = 300,dw=2,visual=T,fn="EMA",fn2="SMA",glaettung2=0,onlyLong=F)
  x=any.smoothing(dax,glaettung = 200,dw=0,visual=T,fn="SMA",fn2="EMA",glaettung2=10,onlyLong=F,q.w="20%")
#faber
  x=any.smoothing(dax,glaettung = 200,dw=2,visual=T,fn="SMA",fn2="",glaettung2=0,onlyLong=F,q.w="2%")
  

  ################# Risk #################################
  
  chart.Correlation(pairRet)
  
  R=merge(mROC(data$prices[,"NASDAQ100"]),mROC(data$prices[,"DAX"]))
  Risk.drawdown <- Drawdowns(R)
  w=R
  w[]=0.5
  ES()
  
  Risk.es <- rollapplyr(R,FUN="ES",width=36,p=0.95,na.pad=TRUE,weights=w)
  #take care of NA with 0 at beginning and interpolation at end
  Risk.es <- apply(Risk.es,MARGIN=2,FUN=na.fill,fill=c(0,"extend"))
  data.to.plot <- merge(mNorm(p),mNorm(Risk.es))#,Risk.drawdown)
  mchart(data.to.plot)
  
  ###### 
  p=dax
  #fast.smoothing ist eine gute Kristallkugel - wenn auch nicht f?r heute -
  #aber ich sollte evtl. seine pos auch noch verz?gert einnehmen...
  plot(p)
  s = rollapplyr(p,FUN=fast.smoothing,width= 500,glaettung=5,by.column=F,roll=T)
  lines(s,col="green",lwd=3)
  #wenn sich die runMin oder runMax wenig ?ndern ist horizontal 
  rm=runMin(s,20)
  lines(rm,col="blue")
  rm
  
####################################
  
  
  
  
cp=coarse.code(p[HL$hl],b=b,method="n",visual=T)  #danach sollten sich support und resistance-punkte leichter finden lasssen
#wie viele HL gibts in welchem bin...
sapply(1:b,FUN=function(x) {res= count(cp[cp==x]);names(res)<-x;return(res)})

# rollapplyR --------------------------------------------------------------


  
#lomi= runMin(Lo(na.omit(price)),n=12)
#himi= runMax(Hi(na.omit(price)),n=12)
lomi=runMin(p,12)
himi = runMax(p,12)

Lows=lomi[HL$lows]
lomi=lomi[HL$lows]   #die Ecken
himi= himi[HL$highs]

# -1,1,- trains
dlomi=sign(ROC(lomi,n=1))
dhimi=sign(ROC(himi,n=1))

rl=runLengthEnc2(dlomi)

rl[rl==-1]<-0  #auch das erste Low-Extremum einer Kette mitnehmen, dass noch keine Vorg?nger hat

#an welchen Tagen kommen Lows die rlp mal schon oberhalb ihres Vorg?nger-Lows lagen:
rlp=rl[rl>=0] #das sind jetzt die Punkte durch die sich support-reg ziehe kann





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
channelStop(sym="DAX")
channelStop(sym="DAX",maxdd=1)
  
  channelStop.daily(sym="DAX",dd=5,visual=T) 
channelStop.daily(sym="DAX",dd=5,visual=F) 


channelStop.daily(sym="DAX",visual=F) 


lapply(global_arg$dat$symbolnames, function(x) channelStop.daily(sym=x,dd=2,visual=F) )


channelStop.daily(sym="USDEUR",dd=5,visual=T) 
channelStop.daily(sym="CAC40",dd=5,visual=T)



#wo's nicht klappt 
#SXDBP, SX3BP, SGX5R, SXABP
#SX6BP

#techStops
channelStop(sym="SXDBP")

channelStop.daily(sym="SXDBP",dd=2,visual=F) #<<<<<< 

channelStop.daily(sym="DAX",dd=2,visual=F) #<<<<<< 


#####################

library(rattle)
rattle()

#######################################################################
install.packages(c('XML','bitops','TFX','shiny'), repos = 'http://cran.r-project.org',dependencies = 'Imports')

library(TFX)

QueryTrueFX()

#http://systematicinvestor.wordpress.com/2012/12/06/tfx-package/
library('shiny')
runGist('4122626')
########################################################################
  ##load libraries
  
  library(mgcv)
  library(parallel)
  
  ###see if you have multiple cores
  
  detectCores()
  
  ###indicate number of cores used for parallel processing
  if (detectCores()>1) {
    cl <- makeCluster(detectCores()-1)
  } else cl <- NULL


###################################################################

problems=  show.attention(data$prices,fn="level.faber",Thresh1=0,fn.signal="signal.faber.v0",vers=0,winLen=200)

problems=  show.attention(data$prices,fn="level.vola",fak=1,Q="80%")

normDifs=rollapplyr(rm,1, roll.quantile, allPrices=rm,maxWin=430,Q=Q )


problems=  show.attention(data$prices,fn="level.faber",Thresh1=0,fn.signal="signal.faber.v1",vers=0,winLen=200)

problems=  show.attention(data$prices,fn="level.vola",fn.signal="signal.vola",Q="69%",use.rollQ=T)

signal=signal.vola(p, indi, HighLight)
#########################################################################
test.feature.pnn(just_build_data=T)
rattle()
##############################################################################
#Value -Growth- Vergleich
  
  sg2r =mNorm(data$prices[,"SG2R"])
  sv2r =mNorm(data$prices[,"SV2R"])

  dax =mNorm(data$prices[,"DAX"])
  mdax =mNorm(data$prices[,"MDAX"])
  
  purePlot(merge(sv2r,sg2r),main="stoxxValue-stoxxGrowth")
  purePlot(merge(dax,mdax),main="dax-mdax")
  
  find.last.trend.segment(sv2r,visual=T,glaettung=5)
  find.last.trend.segment(sg2r,visual=T,glaettung=5)
  
  find.last.trend.segment(dax,visual=T,glaettung=5)
  find.last.trend.segment(mdax,visual=T,glaettung=5)
  
  sg2r.m = ROC(Cl(to.monthly(sg2r)))  
  sv2r.m = ROC(Cl(to.monthly(sv2r)))
  plot(sv2r.m,type='h',lwd=2)
  lines(sg2r.m,type='h',lwd=2,col="red")
  
#erstell mal find.best.series (incls. xls und png-file erstellung) f?r jedes monatsende
seg=  
  foreach(yt = as.Date(index(m.to.monthly(dax))) , .combine = "rbind") %do% {
    #yt ist das Datum des rechten intervall-endes
    find.best.series(data, visual=T, min.len=100,max.len= 1000,sym="DAX",date.last=yt,glaettung=5,saveDir="Plots/BEST")
    
  }



################################################
#eine liste - pro sym - aller signale
S <<-signal
#signal=S
#### ermittle aus der signal- matrix die liste aller dat?mer an denen getraded werden soll
res=c()
for(col in 1:ncol(signal))
    res = c(res,unlist(index(na.omit(bt.exrem(signal[,col])))))
res=as.Date(sort(unique(res)))

#im schnitt signale pro sym

len(res)/dim(signal)[2]
############################################################################
#Eckhards 4. lieferung,   baue target, und techcloud f?r die indizes
#checke kurz die qualit?t des rein heursitschen labor-trading-systems
#baue ein rein technisches  forrest-system
#ermittle welche der 250 macro-indicatoren zu welchen der 8 kurs-indizes
#passen  (faktor wichtigkeit f?r alle jahre)
############################################################################
euro.indi= read.EckhardsXLS(xlsName="Index01.xls",startRow=5,belowColnames=5,debug=F)
data <-  make.data(euro.indi)
  
#siehe labor_signal.r
posTable <-NULL   #l?sche hiermit die Ergebnistabelle
  
estimate.pos(dax.w,visual=visual)  #der tech-cloud - aufruf
make.tech.cloud()
#......................
  
sfStop()
prepare_Parallel()
sfSource("MLib/labor_signal.r")
########################  im Batch die tech-Cloud erstellen
  t.cloud <-  make.tech.cloud(visual1=F, visual=F,symbols="",step="days")#,REX_RI  ########################  im 
  t.cloud <-  make.tech.cloud(visual1=F, visual=F,symbols=spl("REX_RI,SXX50,DAX30,USD_EURO"),step="years",do.par=F)#,REX_RI  ##
  
  len(t.cloud)
  t.cloud[[1]]  
  
  x=read.csv(sep=";",dec=".",header=T,sprintf("TechCloud/DAX30_tcloud.csv"),stringsAsFactors=F)
  colnames(data$prices)
  #bug in DAX30  2002-07-23
  #--> TODO Teste das heuristische Trading  mit labor_signal.r #estimate.pos
  #.. dann weiter mit forest-signal
  
##############################################################  
#f?r eckhard die cloud bauen .. erst mal konventionell .. wie gehabt  (indicators2.r),  dann mit make.tech.cloud()  
  if (T)
  {
    sym="DAX30"
    arg= list(dat =data, clos=data$prices[,sym],Target=data$Target )
    par=list(sma.w=200)
    
    sig= signal.randomForest.e4(arg,par) 
  }
  
}



Target   <-compute.Target.by.method(data=data, TargetMethod=0, w=10 , minReturn=0.1, min.duration=20,glaettung=5,visual=T)

####################################
#2013.12.09

TrainRun_EM4B(Experi=experiment$TEST , do.par=F)

sfStop()
prepare_Parallel() 

sfExport("retrain.on")
sfExport("data")

TrainRun_EM4B(Experi=experiment$TEST , do.par=T)

x=  HotLags2.CC(p=data$prices[,1],features =data$prices)

auswertung(Experi=experiment$macro.select.m,data=data,bestN=30) 




###################################################################

# # # # # # # # # #
# Short and noise-free time series
series <- c(rep(0,30),rep(10,30),seq(10,5,length=20),seq(5,15,length=20))

# Adaptive online signal extraction without & with 'restrict to range' rule
t.without.rtr <- adore.filter(series, rtr=0)
plot(t.without.rtr)
t.with.rtr1 <- adore.filter(series, rtr=1)
lines(t.with.rtr1$level, col="blue")
t.with.rtr2 <- adore.filter(series)
lines(t.with.rtr2$level, col="green3",lty=5)
legend("top",c("Signal with rtr=1","Signal with rtr=2"),col=c("blue","green3"),lty=c(1,2),bty="n")

# # # # # # # # # #
# Short and noise-free time series + 1 outlier
ol.series <- series
ol.series[63] <- 3

# Adaptive online signal extraction without & with 'restrict to range' rule
t.without.rtr <- adore.filter(ol.series, rtr=0)
plot(t.without.rtr)
t.with.rtr1 <- adore.filter(ol.series, rtr=1)
lines(t.with.rtr1$level, col="blue")
t.with.rtr2 <- adore.filter(ol.series)
lines(t.with.rtr2$level, col="green3",lty=2)
legend("top",c("Signal with rtr=1","Signal with rtr=2"),col=c("blue","green3"),lty=c(1,2),bty="n")

# # # # # # # # # #
# Noisy time series with level shifts, trend changes and shifts in the scale of the error term
true.signal  <- c(rep(0,150),rep(10,150),seq(10,5,length=100),seq(5,15,length=100))
series2      <- true.signal + c(rnorm(250,sd=1), rnorm(200,sd=3), rnorm(50,sd=1))

# Adaptive online signal extraction with additional Qn scale estimation
s2 <- adore.filter(series2, calc.qn=TRUE)
par(mfrow=c(3,1))
plot(s2)
plot(s2$sigma,type="l",main="Corresponding Qn Scale Estimation",ylab="sigma",xlab="time")
lines(c(rep(1,250),rep(3,200),rep(1,150)),col="grey")
legend("topleft",c("True scale","Qn"),lty=c(1,1),col=c("grey","black"),bty="n")
plot(s2$width,type="l",main="Corresponding Window Width",ylab="width",xlab="time")
####################################

