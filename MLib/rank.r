#gehe nach MM_schnellTest.Rankings
#Hier werden die ranking-zahlen berechnet und gecashed   (siehe #CASHINTERFACE)
#Ausserdem gibts ein möglichkeit schnell mal Selection+Allocation zu testen  (SA-Maschine)
#um einen ersten Eindruck von der Güte der Ranking-Zahl #zu erhalten

options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
  
})) 
options(warn=1)
options(error = browser)

library(foreach)
#library(astsa)  acf2
##################################################################################
#Marken:

#Beispiele  #CASHINTERFACE
#API   #CORE
#Rankings
#Sit
##################################################################################
#VORRAUSETZUNG:   global_arg$dat  ist definiert !!

if(F)  #MM_schnellTest.Rankings
{
  reset.cashed.rank()
  my.rank.fn = match.fun("rank.kelly")
  my.rank.fn= match.fun("rank.calmar")
  ranks=foreach(x=colnames(prices),.combine="cbind") %do% {ranks=my.rank.fn(prices[,x]); ranks[is.na(ranks)]<-0; return(ranks)}
  
  rank.name="SMA"
  
  ranks=foreach(sym=colnames(prices),.combine="cbind") %do% 
{
  ranks=help.cash.rank(prices[,sym], rank.name)
  ranks[is.na(ranks)]<-0; 
  return(ranks)
}
#er holt sich beim #MMuseCash  die Daten aus dem Cash oder steckt rein (build)
#Beispiele
#sehr gut:  slope300 und faber 

sit.AdaptiveAssetAllocation(    my.rank="rank.slope300,rank.faber,rank.faber.b,rank.slope")


sit.AdaptiveAssetAllocation(    my.rank="rank.momVol,rank.momVolslope,rank.slope300") #rank.slope,rank.es,

sit.AdaptiveAssetAllocation(    my.rank="rank.momVol,rank.momVol,rank.es,rank.momVolslope")
sit.AdaptiveAssetAllocation(    my.rank="rank.faber,rank.slope,rank.slope300,rank.inTrend")
sit.AdaptiveAssetAllocation(    my.rank="rank.faber,rank.slope300")  #m0

#gut  
sit.AdaptiveAssetAllocation(    my.rank="rank.es,rank.faber,rank.momVolES,rank.slopeVol")
sit.AdaptiveAssetAllocation(    my.rank="rank.momVolES,rank.slopeVol")
#schrott
sit.AdaptiveAssetAllocation(    my.rank="rank.omega,rank.kelly,rank.sharpe")
sit.AdaptiveAssetAllocation(    my.rank="rank.calmar,rank.sharpe")
#####################################################################################
##### der zugriff auf den Cash:  read or build+read
##################################################################################>>>
#CASHINTERFACE:

reset.cashed.rank()  #lösch den cashe
#der auch nur eine gezielte methode
reset.cashed.rank("rank.faber")

#get.cashed.rank("rank.sharpe","DAX")  #liest nur aus dem cashe- wenn das ding noch nicht da ist kommt NULL
########> am wichtigsten  get.rank(): read or build+read - aus beliebig viele spalen
#- wenn keine preise mitgegeben werden sucht er die in global_arg$dat$prices
x=get.rank("rank.faber",prices=data$prices,"DAX")
x=get.rank("rank.faber",prices=data$prices) #das geht auch für alle symbole ..

x=get.rank("signal.Faber.base",prices=data$prices) #geht auch - allerdings muss singal.() eine einedim -rank-Zeitreihe zurückgeben
#jetzt liest er aus chashe
x=get.rank("rank.faber",prices=data$prices) #das geht auch für alle symbole ..
#aber wenn die fromToS- Referenz zu den Kursen nicht passt muss neu gerechnet werden..
x=get.rank("rank.faber",prices=data$prices[-1]) #das geht auch für alle symbole ..
###### es kann aber auch jederzeit ein schreiben erzwungen werden
x1=get.rank("rank.faber",prices=data$prices, "DAX",make.new=T)#erwzinge das schreiben
#soll ein leicht veränderter ranker berechnet werden: rank.faber533
x=get.rank("rank.faber" ,prices=data$prices, "DAX",make.new=T,xtra=533)#hängt 233 an den name
#lesen geht:
x2=get.rank("rank.faber",data$prices,"DAX",xtra="533") 
mchart(merge(x1,x2))

#Nutzung als cashe für beliebige zeitreihen ..der name muss wenigstens 4 zeichen lang sein!

p.normed= normalize.mean.R(data$prices,100)
p.normed.slope300 <- rollRegressionXTS(p.normed,win=300);
mchart(p.normed)
mchart(p.normed.slope300 )
#der cashe arbeitet immer mit den selben symbolen wie in data$prices
#wenn man was reinstecken will sollt man seine Daten vorher renamen:
colnames(p.normed.slope300) = colnames(data$prices)
x=get.rank( "p.normedSlope",prices=p.normed.slope300,xtra="write")#schreibt

p.n=get.rank("p.normedSlope",prices=data$prices)
#aber nicht vergessen: für diese Teile gibts ja keine Auto-Update-Funktion:
p.n=get.rank("p.normedSlope",prices=data$prices[-1])

##### behind der curtain:
cashed()   # identisch mit ls(global_arg$dat$rank)
cashed("p.normedSlope")   # ls(global_arg$dat$rank$p.normedSlope)
cashed("rank.faber")
cashed("rank.faber",prices=data$prices)
cashed("TSA.t1",prices=data$prices)

reset.cashed.rank("p.normedSlope")
cashed("p.normedSlope")   # ls(global_arg$dat$rank$p.normedSlope)
#Info:
#set.cashed.rank(rank.fn.name,ret,sym)  -> data$rank[[name]][[sym]]


#veraltet
Get.cashed.rank("rank.faber","DAsX",data$prices) #read or build+read
#reset.cashed.rank("rank.slopeVol")
#<<<##################################################################################

plot(Get.cashed.rank("rank.faber","DAddX",data$prices))
}

RANK.fill<-function(prices,xtra)
{
  prices
}
#####################################################################################
#hier kommen alle möglichen Rankings die dann via nTop ausgwertet werden
#####################################################################################
#Rankings

rank.faber<-function(prices,xtra=300)
{
  mP("rank.faber");
  p= prices
  
  ret = p-SMA(p,xtra)  
}
if (F)
{
  reset.cashed.rank("rank.faber")
  x=get.rank("rank.faber",prices=data$prices)
}  


rank.faber.b<-function(prices,xtra=300)
{
  mP("rank.faber");
  p= prices
  
  ret = EMA(p-SMA(p,xtra), n=min(3,xtra/10))  
}


rank.vol<-function(prices,xtra=200)
{
  mP("rank.vol");
  p= prices
  
  vol_percent = clean.matrix(runSD(p,xtra) / runMean(p,xtra))
}
if (F)
{
  reset.cashed.rank("rank.vol")
  x=get.rank("rank.vol",prices=data$prices)
}  

rank.volmom<-function(prices,xtra=200)
{
  p= prices
  
  vol_percent = clean.matrix(runSD(p,xtra) / runMean(p,xtra))
  res=clean.matrix(mROC(EMA(p,xtra/2)) / vol_percent)
}
rank.volslope<-function(prices,xtra=200)
{
  p= prices
  
  vol_percent = clean.matrix(runSD(p,xtra) / runMean(p,xtra))
  res=clean.matrix(get.rank("rank.slope300",prices=prices)/ vol_percent)
}
rank.ddma<-function(prices,xtra=90)
{
  p= prices
  ddma(p=prices,n=xtra)  
}

rank.mfaber<-function(prices,xtra=300)
{
  wins=rev(spl("10,30,60,90,100,120,180, 200,300,400"))
  mP("mrank.faber");
  p= prices
  ret=0
  j=1
  for(win.i in wins)
   {
     ret = ret + ifelse(sign(p-SMA(p,as.numeric(win.i))) >0,1,0)
     j=j+1
   }
  clean.matrix(ret)
}



if (F)
{
  p=data$prices[,data$BENCH]
  reset.cashed.rank()
  reset.cashed.rank("rank.vol")
  x=get.rank("rank.vol",prices=data$prices)
  x=get.rank("rank.probMom",prices=data$prices)
  
  x=get.rank("rank.volmom",prices=data$prices)
  x=get.rank("rank.volslope",prices=data$prices)
  x=get.rank("signal.Price.itp",prices=data$prices)
  x=get.rank("signal.levy",prices=data$prices)
  x=get.rank("rank.ddma",prices=data$prices,xtra=300)
  x=get.rank("rank.mfaber",prices=data$prices[,data$BENCH])
  x=get.rank("rank.kanalBreite",prices=data$prices[,data$BENCH])
  x=get.rank("rank.kanalSlope",prices=data$prices[,data$BENCH])
  x=get.rank("rank.HLrelation",prices=data$prices[,data$BENCH])
  
  mPlots(merge(p,x))
  max(x)
  #rank.itp
  #rank.ddma
  #... und die ganzen signal. teile  wie days.since.ma ..
  #wenn er unter das low der letzten 20-Tage fällt..
  #wenn er vom high der letzten 20 tage tiefer als 10 % fällt
  #oder vom high der letzen 100 Tage tiefer als 20% fällt
  #rank.kanalBreite
  #rank.HLrelation
  
  mchart(merge(p,x ))

}  

##################################################################################
rank.kanalBreite<-function(prices,xtra=12)
{
  visual=F
  mP("rank.kanalBreite");
  sym=colnames(prices[,1])
  frame=fromToS(prices[,1])
  
price =na.omit(global_arg$dat[[sym]][frame])
#price = mNorm(price)+1000

lomi= runMin(Lo(na.omit(price)),n=xtra) #lag mäßig ok
himi= runMax(Hi(na.omit(price)),n=xtra)
if (F)
{
mPlots(merge(price,merge(himi-lomi,runMean(himi-lomi,n=12))))
mchart(merge(himi-lomi,runMean(himi-lomi,n=20)),main="kanalbreite")
}
cb=(himi-lomi)/lomi*100#runMean(himi-lomi,n=90)  #kanalbreite
xts2xts(lineal=prices,cb,fill=T)
}

rank.kanalSlope<-function(prices,xtra=200)
{
  p= prices
  
  vol_percent = clean.matrix(runSD(p,xtra) / runMean(p,xtra))
  res=clean.matrix(get.rank("rank.slope300",prices=prices,xtra=xtra)/ (1+get.rank("rank.kanalBreite",prices=prices,xtra=xtra)))
}

if (F)
{
  reset.cashed.rank("rank.kanalBreite")
  x=get.rank("rank.kanalBreite",prices=data$prices[,data$BENCH])
  x=get.rank("rank.kanalSlope",prices=data$prices[,data$BENCH])
  #rank.HLrelation
  x=get.rank("rank.HLrelation",prices=data$prices[,data$BENCH])
  
  x
  plot(x)
  vol(x)
}  

############ schaut auf die relative lage von highs und lows zueinander und ermittelt daraus einen Trend dessen trendlänge zurückgegeben wird
#######################################################################################
rank.HLrelation<-function(prices,xtra=5)
{
K=mZigZag4(prices[,1],dd=xtra,visual=F)
k=K$lowhigh.one
k=K$rl.added  #nur hiermit sind auch größere  maxdd sinnvoll - sonst bleiben zu lange signale aus

if (is.null(k))
 res = xts2xts(prices,1,fill=T)
else
{
  
  res =  xts2xts(lineal=prices,k,fill=T)
 
  # mPlots(prices,merge(res,0))
}
res
}
if (F)
{
  reset.cashed.rank()
  reset.cashed.rank("rank.HLrelation")
  x=get.rank("rank.HLrelation",prices=data$prices[,"BARCLAYS"],xtra=5,make.new=T)
  x
  #mPlots(data$prices[,data$BENCH],merge(x,0))

  x=get.rank("rank.HLrelation",prices=data$prices)
  x=indi.Generic("signal.HLrelation", global_arg, par=list(maxdd=5), visual=T, TRAINSYM ="SXQBP")
  x=indi.Generic("signal.HLrelation", global_arg, par=list(maxdd=9), visual=T, TRAINSYM =-1)
  x=indi.Generic("signal.HLrelation", global_arg, par=list(maxdd=-5), visual=T, TRAINSYM =-1)
  
  
}  



##################################################################################
RANK.performance.sharpe<-function(prices,xtra=300,frame="",maxdd=20)   #weil RANK gross kommen hier viele sym-spalten an - nicht nur eine
{
  mP("RANK.performance.sharpe");
  p= prices[frame]
 tab=NULL
 #tabelle mit performance-kriterien - analog zur sit-model-equity- beurteilungs-tabelle
  for(sym.i in colnames(p))#,.combine="rbind",.errorhandling='pass') %do%
  {
    print(sym.i)
    pi=mNorm2(p[,sym.i]);pi=pi+min(pi,na.rm=T)+100
    zz =HighLows(pi,maxdd=maxdd,visual=T,percent=F);  simuSignals = sign(zz$zz-lag(zz$zz))
    temp= try(performance.Numbers(sym.i,equity=p[,sym.i],signal=simuSignals,frame=frame,data=global_arg$dat,visual=T))
    if(inherits(temp, 'try-error'))
        sag("There was a bug111",warte=T)
    modList =data.frame( temp)
  
    rt=data.frame(modList)
    if (is.null(tab))tab=rt else tab=rbind(tab,rt)
  #............................................................
  
#performance-tabelle multivariat sortieren: zuvor spalten coarse-coden

 tabM=tab[order(as.integer(cut(tab$nTrades,b=3)),-as.integer(cut(tab$Sharpe,b=5)),as.integer(cut(abs(tab$MaxDD),b=20)) ,-tab$Cagr),]
  }
}

if (F)
{
  RANK.performance.sharpe(prices=data$prices[,c(1,2,3)],frame="2012::2012",maxdd=0.1)
  x=get.rank("RANK.performance.sharpe",prices=data$prices[,c(1,2,3)])
}
#learn.rank()
#sig.value <- rollapplyr(P, width=1000, FUN=roll.classifier.e4c, by.column=F,allFeatures=priceFeatures[firstUsefulDateS],maxWin=2500, retrain.on=retrain.on,crs=crs)
#### macht jedes signal. () System welches einen sinnhaften ranking- Indi-Wert zurückgibt 
#aufrufbar als ranking-methode
# als Argumente werden die Default- Parameter benutzt
#######################################################################################

rank.Generic<-function(rankfn, prices,...)
{
#  if (sym != "")
#    global_arg$clos= global_arg$dat$prices[,sym]
  
  global_arg$clos = prices
  
  rankFn=match.fun(rankfn)
  global_arg$clos=prices
  
  if (len(rankFn)>0)
  { 
    global_objectId <-paste("TREND","xx",rankfn) #damit der parameterlose -signal-aufruf klappt 
    res=rankFn(global_arg,...)
      if (has(res,"rank"))   
      return(res$rank)
  }
  return(NULL)
}
#.........................
if (F)
{
  res=rank.Generic("signal.Faber.i2",prices=data$prices [,data$BENCH])
  mPlots(merge(res,data$prices [,data$BENCH]))


  res=rank.Generic("signal.MA.3",prices=data$prices [,data$BENCH])
  mPlots(merge(res,data$prices [,data$BENCH]))
  
  res=rank.Generic("signal.drawDown1",prices=data$prices [,data$BENCH],par=list(win=200))
  mPlots(merge(res,data$prices [,data$BENCH]))
  
  res=rank.probMom(data$prices[,data$BENCH],xtra=60)
  mPlots(merge(res,data$prices [,data$BENCH]))
  
  
  #  res=rank.Generic("signal.sit.feilscher",prices=data$prices [,data$BENCH],par=list(lookback.len=60))
  mPlots(merge(res,data$prices [,data$BENCH]))
  
  
  
}


####################################################################################

rank.rMax<-function(prices,xtra=60) #schrott
{
  p= prices
  
  ret = p-runMin(p,xtra)  
}

#####################################################################################

rank.momVol<-function(prices,xtra=6) #klasse
{
  # Adaptive Asset Allocation parameters
  #  n.top = 5       # number of momentum positions
  n.mom = xtra*22    # length of momentum look back
  n.vol = as.integer(xtra/6*22)    # length of volatility look back   
  
  ret.log =  mROC(prices, type='continuous')
  hist.vol = runSD(ret.log, n = n.vol)
  adj.vol = 1/hist.vol
  
  momentum = prices / mlag(prices, n.mom) * adj.vol   

}
#####################################################################################

rank.momVolslope<-function(prices,xtra=6) #klasse
{
  # Adaptive Asset Allocation parameters
  #  n.top = 5       # number of momentum positions
  n.mom = xtra*22    # length of momentum look back
  n.vol = as.integer(xtra/6*22)    # length of volatility look back   
  
  ret.log =  ROC(prices, type='continuous')
  hist.vol = runSD(ret.log, n = n.vol)
  adj.vol = 1/hist.vol
  
  momentum = prices / mlag(prices, n.mom) * adj.vol * Get.cashed.rank("rank.slope",colnames(prices)[1],prices)   
  
}

#####################################################################################

rank.momVolES<-function(prices,xtra=6)
{
  # Adaptive Asset Allocation parameters
  #  n.top = 5       # number of momentum positions
  n.mom = xtra*22    # length of momentum look back
  n.vol = as.integer(xtra/6*22)    # length of volatility look back   
  
  ret.log =  ROC(prices, type='continuous')
  hist.vol = runSD(ret.log, n = n.vol)
  adj.vol = 1/hist.vol
  
  momentum = prices / mlag(prices, n.mom) * Get.cashed.rank("rank.es",colnames(prices)[1],prices)  
  
}
#####################################################################################
runBeta<-function(y){x=global_arg$dat$prices[fromToS(y),global_arg$BENCH];beta = lm(y~x-1)$coefficients}

rank.beta.schrott<-function(prices,xtra) #schrott
{
  if (len(global_arg$BENCH)==0)
    global_arg$BENCH <-"DAX"
  
  ret <- rollapplyr(prices, width=60, FUN="runBeta", by.column=T, align = "right")
  ret[!is.finite(ret)]<-0
  ret
}
#x=data$prices[,"DAX"]
#y=data$prices[,"ATX"]
#beta = lm(y~x-1)$coefficients

if (F)
{
  rank.beta(atx) 
}
#####################################################################################
WIN=200  #60

rank.omega<-function(prices,xtra=WIN) #schrott
{
  ret.p =  ROC(prices, type='discrete'); ret.p[1]<-0
  ret <- rollapplyr(ret.p, width=xtra, FUN="Omega", by.column=T, align = "right")
  ret[!is.finite(ret)]<-0
  ret
}
#####################################################################################

rank.kelly<-function(prices,xtra=WIN) #schrott
{
  ret.p =  ROC(prices, type='discrete'); ret.p[1]<-0
  ret <- rollapplyr(ret.p, width=xtra, FUN="KellyRatio", by.column=T, align = "right")
  ret[!is.finite(ret)]<-0
  ret 
}
#####################################################################################

rank.calmar<-function(prices,xtra=WIN) #schrott
{ 
  ret.p =  ROC(prices, type='discrete'); ret.p[1]<-0
  ret <- rollapplyr(ret.p, width=xtra, FUN="CalmarRatio", by.column=T, align = "right")
  ret[!is.finite(ret)]<-0
  ret   
}
#####################################################################################

rank.tsi<-function(prices,xtra=WIN) #schrott
{ 
  sym=colnames(prices)
  ret=TSIsymbol(global_arg$dat,sym,n=WIN)
  ret[!is.finite(ret)]<-0
  ret   
}
#####################################################################################

rank.sharpe<-function(prices,xtra=WIN) #schrott
{
  ret.p =  ROC(prices, type='discrete'); ret.p[1]<-0
  sharpe <- rollapplyr(ret.p, width=xtra, FUN="doSharpeRatio", by.column=T, align = "right")
  sharpe[!is.finite(sharpe)]<-0
  sharpe
}
#####################################################################################

rank.es<-function(prices,xtra=WIN) #ok
{  
  ret.p =  ROC(prices, type='discrete'); ret.p[1]<-0
  es <- rollapplyr(ret.p, width=xtra, FUN="ES", by.column=T, align = "right",method="historical")
  es[!is.finite(es)]<-0
  es
}

#####################################################################################

rank.slope<-function(prices,xtra=120)  #OK
{  
  normed.prices = mNorm(prices)
  normed.prices[!is.finite(normed.prices)]<-1
  mSlope90 <- rollRegressionXTS(normed.prices,win=90);
  mSlope120 <- rollRegressionXTS(normed.prices,win=xtra)
  #mSlope200 <- rollRegressionXTS(normed.prices,win=200)
  
  ret = mSlope90-mSlope120
}

rank.slope300<-function(prices,xtra=300)  #OK
{  
  normed.prices = mNorm(prices)
  normed.prices[!is.finite(normed.prices)]<-1
  mSlope300 <- rollRegressionXTS(normed.prices,win=xtra)
  
  ret = mSlope300
}
rank.slope200<-function(prices,xtra=200)  #OK
{  
  rank.slope300(prices,xtra)
}
#####################################################################################

rank.slopeVol<-function(prices,xtra=6)  #OK
{  
  normed.prices = mNorm(prices)
  normed.prices[!is.finite(normed.prices)]<-1
  #  mSlope90 <- rollRegressionXTS(normed.prices,win=90);
  #  mSlope120 <- rollRegressionXTS(normed.prices,win=120)
  #  n.top = 5       # number of momentum positions
  n.mom = xtra*22    # length of momentum look back
  n.vol = as.integer(xtra/6*22)    # length of volatility look back   
  
  ret.log =  ROC(prices, type='continuous')
  hist.vol = runSD(ret.log, n = n.vol)
  adj.vol = 1/hist.vol
  
  momentum = prices / mlag(prices, n.mom) * adj.vol  
  slope=Get.cashed.rank("rank.slope",colnames(prices)[1],prices)  
  ret = slope*adj.vol
}
#####################################################################################

rank.inTrend<-function(prices,xtra=400) #400
{  
  ret= in.Trend.pos(prices,k=xtra)
}

###

#####################################################################################
#m3
RANK.beta <-function(prices,xtra)  #wird RANK groß geschrieben wird über das ganze Portfolio und nicht jedes symbol einzel gearbeitet
{
  b=beta=  beta.again.portfolio(prices)  #portfolio-beta aus SIT mit monatsdaten
  mP("RANK.beta");
  #korrigiere um einen 2 monats forecast
  #  beta.m= lag(beta,-1)  #blick in die zukufnt
  beta.m=beta
  
  if (F)
    beta.m = foreach(col =  colnames(b),.combine="merge") %do%
{
  print(col)
  p=na.omit(b[,col])
  #fct= rollRegressionXTS.forecast(p,win=7,n.ahead=2)
  beta.slope= rollRegressionXTS(p,win=6)
  beta.slope[]=1
  #selbst bei 2 monate forecast sollte beta noch positiv sein
}
#wandel auf tagesdaten
b.daily =m.to.timescale(beta.m,prices[,1])
browser()
}

#rank.momVol
#####################################################################################
probAny<-function(t.indi, lookback.len)
{
  ir = sqrt(lookback.len) * runMean(t.indi, lookback.len) / runSD  (t.indi, lookback.len)
  res = pt(ir, lookback.len - 1)  
}
#...............................................................

probMom <-function(data,sym.i,sym,lookback.len)
{
  if (sym.i != sym)
  {
    p=data$prices[,c(sym,sym.i)]
    ret =  p/ mlag(p,nlag=lookback.len) - 1
#    ret=probMom(ret)
    
    #ret.log =  mROC(p, type='continuous')
    #hist.vol = bt.apply.matrix( ret.log, runSD, n = lookback.len)
    #adj.vol = 1/hist.vol
    #ret = ret.log*adj.vol
#MMY 
#    ret=rank.momVol(p,xtra=lookback.len) -1
    
    di=try(clean.matrix(ret[,sym] - ret[,sym.i]))
if(inherits(di, 'try-error')) 
{ 
  momentum.p =data$prices[,sym.i];momentum.p[] = 0}
else
    {ir = sqrt(lookback.len) * runMean(di, lookback.len) / runSD (di, lookback.len)
    momentum.p = pt(ir, lookback.len - 1)     }     
  }
 else
  { momentum.p =data$prices[,sym.i];momentum.p[] = 0}
 momentum.p
}

probMom2 <-function(data,sym.i,prices,lookback.len)
{
  sym=colnames(prices[,1])
  if (sym.i != sym)
  {
    p=merge(data$prices[,sym.i],prices)
    ret =  p/ mlag(p,nlag=lookback.len) - 1
    #    ret=probMom(ret)
    
    #ret.log =  mROC(p, type='continuous')
    #hist.vol = bt.apply.matrix( ret.log, runSD, n = lookback.len)
    #adj.vol = 1/hist.vol
    #ret = ret.log*adj.vol
    #MMY 
    #    ret=rank.momVol(p,xtra=lookback.len) -1
    
    di=try(clean.matrix(ret[,sym] - ret[,sym.i]))
    if(inherits(di, 'try-error')) 
    { 
      momentum.p =data$prices[,sym.i];momentum.p[] = 0}
    else
    {ir = sqrt(lookback.len) * runMean(di, lookback.len) / runSD (di, lookback.len)
     momentum.p = pt(ir, lookback.len - 1)     }     
  }
  else
  { momentum.p =data$prices[,sym.i];momentum.p[] = 0}
  momentum.p
}

normMom <-function(data,sym.i,sym,lookback.len)
{
  if (sym.i != sym)
  {
    p=data$prices[,c(sym,sym.i)]
    ret =  p/ mlag(p,nlag=lookback.len) - 1
    ret =ret[,sym] - ret[,sym.i]
    momentum.p = ret/lookback.len
  }
  else
  { momentum.p =data$prices[,sym.i];momentum.p[] = 0}
  momentum.p
}

rank.probMom<-function(prices,xtra=WIN) #60
{ 
  prices = mNorm(prices)
  sym = colnames(prices)
  lookback.len=as.integer(xtra)
  data=global_arg$dat
  res=0
 
 #hier werden wahrscheinlichkeiten der mom-differenzen zu allen anderen sym addiert
 
  for(sym.i in colnames(data$prices))
  {
     res = res + probMom2(data,sym.i,prices,lookback.len=lookback.len)#momentum.p  
    
#    res = res + normMom(data,sym.i,sym,lookback.len=lookback.len)#momentum.p  
    
  }
  
  #res=probMom(data,sym,data$BENCH,lookback.len) klappt so nicht
  res[!is.finite(res)]<-0
  res = res/ncol(data$prices)
res=clean.matrix(res) 
colnames(res)=sym
  res
}
#####################################################################################

rank.rsi<-function(prices,xtra=WIN)#WIN) #schrott
{ 
  win = as.integer(xtra)
  prices=mNorm(prices)
  bench=mNorm(global_arg$dat$prices[,data$BENCH])
  safe=mNorm(global_arg$dat$prices[,SAFE])
  
  better = max(mROC(bench,win),mROC(safe,win))
 res=mROC(prices,n=win) / better
}   

#API
#####################################################################################
#Zeitaufwändigere Rankings lassein sich in global_arg$dat$rank$name cashen
#####################################################################################
#Zugriff auf den cash
# gibt, ein mehrspaltiges xts zurück, wenn man syms ganz freilässt auch für alle
get.rank<-function(rank.name="SMA",prices=global_arg$dat$prices,syms=NULL,make.new=F,xtra="")
{
 if (global_dont_cashe)
      make.new = F
   
  if (is.null(prices))
    Stop("get.rank1:  no prices given")
  #browser()
  if (xtra=="write")
  {if (is.null(syms) || syms == "")
    syms=colnames(prices)
   else
     syms = syms[syms %in% colnames(prices)]  #  #die namen der daten die geschrieben werden sollen
  }  
  else    
    if (len(syms)==0)
      syms=colnames(prices)
  #  syms= cashed(rank.name) #colnames(prices)
  
  if (len(syms)==0)
  { 
    mP("get.rank3:  syms is empty")
    stop()
  }
  
  
  #sym="DAX"
  #rank=help.cash.rank(prices[,sym], rank.fn.name=rank.name,sym=sym,make.new=make.new,xtra=xtra)
  
  ranks=NULL
  for(sym in syms)
  {
    mP("get.rank(%s %s %s)",sval(rank.name),sval(sym),sval(xtra))
    if (len(prices[,sym])==0)
    { mP("get.rank2:  sym not at prices %s",sym)
      stop()
    }
    rank=help.cash.rank(prices, rank.fn.name=rank.name,sym=sym,make.new=make.new,xtra=xtra)
    print(class(rank));print(head(rank,3))
    if (is.null(ranks))
      ranks=rank
    else
      ranks=merge(ranks,rank)
  }
  return(ranks)
  if (F)
  {
    ranks=foreach(sym=syms,.combine="merge") %do% {
      mP("get.rank(%s %s %s)",sval(rank.name),sval(sym),sval(xtra))
      
      if (len(prices[,sym])==0)
      { mP("get.rank2:  sym not at prices %s",sym)
        stop()
      }
      rank=help.cash.rank(prices[,sym], rank.fn.name=rank.name,sym=sym,make.new=make.new,xtra=xtra)
      print(class(rank));print(head(rank,3))
      rank
    }
  } 
}
get.cashed.rank<-function(name,sym="")
{
  # browser()
  if (sym=="")
    return(m.apply(global_arg$dat, function(x) get.cashed.rank(name,x)))
  
  data=global_arg$dat
  if (len(data$rank)==0)
    data$rank=list()
  if (len(data$rank[[name]])>0)
    if (len(data$rank[[name]][[sym]])>0)
      return (data$rank[[name]][[sym]])
}

get.cashed<-function(name,i=1)
{
  # browser()
  if (sym=="")
    return(m.apply(global_arg$dat, function(x) get.cashed.rank(name,x)))
  
  data=global_arg$dat
  if (len(data$rank)==0)
    data$rank=list()
  if (len(data$rank[[name]])>0)
    if (len(data$rank[[name]][[sym]])>0)
      return (data$rank[[name]][[sym]])
}

set.cashed.rank<-function(name,rank,sym)
{
  data=global_arg$dat
  if (len(data$rank)==0)
  { 
    data$rank=list()
    #data$rank[[name]][[sym]]<-list()
    data$rank[[name]]<-list()
    
  }
#else
    data$rank[[name]][[sym]] =rank
}

reset.cashed.rank<-function(name=NULL)
{
  data=global_arg$dat
  if (is.null(name))
    data$rank=NULL
  else
    data$rank[[name]]=NULL
}


#####################################################################################
#berechne die Funktion  rank.fn  für sym oder lies sie aus dem Cashe 
#  ls(global_arg$dat$rank$SMA)

#das Herzstück des Cashe-Algorithmus
#####################################################################################
#CORE
help.cash.rank<-function(prices,rank.fn.name,sym="",make.new=F,xtra="")
{
  if (len(rank.fn.name)==0)return(NULL)
  if (is.null(prices) && len(global_arg$dat$prices[,sym])> 0)
  {
    print("help.cash.rank: take prices from global_arg$dat$prices")
    prices=global_arg$dat$prices
  }
  if (xtra == "write")  #schreib blos die als preise übergebene daten in den cash
  {
    #colnames(prices)=pureColnames(prices)
    ranks=prices  ; #colnames(ranks) = colnames(prices)
 #cashed("TSA.t1","MSCIEUROPE")   
    foreach(sym=colnames(ranks)) %do%     {
      set.cashed.rank(rank.fn.name,rank=ranks[,sym],sym=sym)  }
    if (sym =="") return(ranks)
    return(ranks[,sym])
  }
  rank.fn.name.ORG =rank.fn.name
  
  #den xtra-parameter an den neuen rank-namen anhängen
  if (xtra!="" && xtra !="write" ) rank.fn.name = sprintf("%s%s",rank.fn.name,xtra)
  
  mP("!help.cash.rank, look for: %s %s",sym, rank.fn.name)
  if (sym == "USDEUR")
      browser()#2017
  
  if (!make.new) #erst mal im cashe nachschauen - ausser wenn make.new =T
  {
    ret = get.cashed.rank(rank.fn.name,sym)
    #gibts daten und sind sie - gem. an den preisen auch noch aktuell
    
    if (len(ret[,sym])>0 && nrow(ret[,sym]) == nrow(prices) && fromToS(ret[,sym]) == fromToS(prices)) { if (shape(ret) >0) {print(tail(ret,2));mP("found %s",fromToS(ret))}; return (ret)}
    if (len(prices) == 0 || len(prices[,sym])==0)
      Stop("#bug1 at help.cash.rank: %s not at prices",sym)
    
  }
  #.....den cashe füllen
  mP("build rank >>%s<< for %s at frame %s" ,
         rank.fn.name,sym,fromToS(prices[,sym])
               )
#mP("not usable at cashe  %d != 0  | %d =?=  %d |  %s =?= %s ", len(ret[,sym]),nrow(ret[,sym]) ,nrow(prices),fromToS(ret[,sym]),fromToS(prices))
 
 
  mP("->match.fun  %s ",rank.fn.name.ORG)
  
 if (str_sub(rank.fn.name.ORG, start = 1L, end = str_length("signal."))=="signal.")
   rank.fn= match.fun("rank.Generic")       
 else
   rank.fn = try(match.fun(rank.fn.name.ORG))   #der funktionszugriff
 
  if (len(rank.fn) == 0 ||  inherits(rank.fn, 'try-error'))
  { mP("#bug2 at help.cash.rank: out of day,  rank-name %s is no defined auto-fill- range.function !",rank.fn.name)
    stop()
  }
  
  mP("<-match.fun")
  #schreibe in den cashe - allerdings handelt es sich bei der rank.methode um eine
  #die alle kurse braucht - dafür aber auch für alle kurse einen rank liefert 
  if (substr(rank.fn.name,1,4)=="RANK")  #alle symbole auf einen schlag
  {
    #für rank-methoden sich durch vergleich von kursen berechen (portfolio...beta ...)
    if (xtra !="") ranks= rank.fn(prices,nval(xtra))  #TODO  indi.Generic()
    else     ranks= rank.fn(prices)
    colnames(ranks) = colnames(prices)
    ranks=foreach(sym=colnames(ranks),.combine="cbind") %do%   {
      set.cashed.rank(rank.fn.name,ranks[,sym],sym); ranks[,sym]}
    if (sym =="") return(ranks)
    return(ranks[,sym])
  }
  else  
  {
    if (str_sub(rank.fn.name.ORG, start = 1L, end = str_length("signal."))=="signal.")
      {
       ret = rank.Generic(rank.fn.name.ORG, prices[,sym])  #TODO  xtra...
      }
    else
    {
    if (xtra !="") ret = rank.fn(prices[,sym],nval(xtra))
    else
      ret= rank.fn(prices[,sym])
    }
  #schreibe in den cashe  #wahrscheinlich kommen beim bug fall nur einzelwerte-keine serie
    colnames(ret)<-sym
    set.cashed.rank(rank.fn.name,ret,sym)
    
    return(ret)
  }
}
if (F)
  help.cash.rank(prices,"SMA","DAX")

############################################################################
#  was wurde gecashed ? - passt es vom zeitrahmen her zu den prices ?
############################################################################
cashed<-function(was="",sym.i="",prices=NULL){
  if (len(global_arg$dat$rank)==0)
    return(NULL)
  if (len(prices)==0 )
    prices=global_arg$dat$prices
  
  if (was=="")
    return( ls(global_arg$dat$rank) )else
      if (has(global_arg$dat$rank,was))
        if (is.null(prices))
          ls(global_arg$dat$rank[[was]])   # ls(global_arg$dat$rank$p.normedSlope)
  else #auch noch fromToS() checken
    if (len(global_arg$dat$rank)>0 )
    {
      bug=F
      if (sym.i=="")
        for (i in 1: len(global_arg$dat$rank[[was]]))
        {
          sym.is=ls(global_arg$dat$rank[[was]])
          sym.i=sym.is[i]
          list.xts =get.cashed.rank(was,sym.i) 
          if(len(list.xts)>0 && !is.null(prices))
            if (fromToS(list.xts) == fromToS(prices[,sym.i]) )
            {mP("%s ok  %s",colnames(list.xts),fromToS(list.xts));print(tail(list.xts,3))}
          else
          {bug=T; mP("%s   # BUG1 #<<############### ",colnames(list.xts))} #schon veraltet
        }
      else
      {
        list.xts =get.cashed.rank(was,sym.i) 
        if(len(list.xts)>0 && !is.null(prices))
          if (fromToS(list.xts) == fromToS(prices[,sym.i]) )
          { mP("%s ok %s",colnames(list.xts),fromToS(list.xts)) ; print(tail(list.xts,3));return(sym.i)}
        else
        {bug=T; mP("%s   # BUG2 #<<############### ",colnames(list.xts))} #schon veraltet
        
      }              
      if (bug)return(NULL)
      return(ls(global_arg$dat$rank[[was]]))
    }
  else
    NULL
  
}
##############################################################################
#read or build and read
Get.cashed.rank<-function(name="rank.sharpe",sym="", prices=NULL)
{
  help.cash.rank(prices=prices, rank.fn.name=name,sym=sym)
}
##################################################################################
# ein abgspecktes sit .. testet rankings (ohne alloca darüber) im vergleich zu einigen
# prognose-freien allocs
##################################################################################
#SIT
sit.AdaptiveAssetAllocation<-function(my.rank="rank.faber")
{
  #  setInternet2(TRUE)
  #  con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
  #  source(con)
  #  close(con)
  ###############################################################################
  # Load Systematic Investor Toolbox (SIT)
  # http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
  ###############################################################################
  
  
  #*****************************************************************
  # Load historical data
  #******************************************************************
  load.packages('quantmod')
  if (F)
  {
    tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')
    
    data <- new.env()
    getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
    for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                           
    bt.prep(data, align='keep.all', dates='2004:12::')
  }
  #*****************************************************************
  # Code Strategies
  #******************************************************************
  prices = data$prices 
  n = ncol(prices)
  head(prices)
  models = list()
  
  # find period ends
  period.ends = endpoints(prices, 'months')
  period.ends = period.ends[period.ends > 0]
  
  # Adaptive Asset Allocation parameters
  n.top = n/4       # number of momentum positions
  n.mom = 6*22    # length of momentum look back
  n.vol = 1*22    # length of volatility look back   
  
  #*****************************************************************
  # Equal Weight
  #******************************************************************
  data$weight=prices
  data$weight[] = NA
  data$weight[period.ends,] = ntop(prices[period.ends,], n)  
  models$equal.weight = bt.run.share(data, clean.signal=F)
  
  #*****************************************************************
  # Volatliliy Position Sizing
  #******************************************************************
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)
  
  adj.vol = 1/hist.vol[period.ends,]
  
  data$weight[] = NA
  data$weight[period.ends,] = adj.vol / rowSums(adj.vol, na.rm=T)   
  models$volatility.weighted = bt.run.share(data, clean.signal=F)
  
  #*****************************************************************
  # Momentum Portfolio
  #*****************************************************************
  momentum = prices / mlag(prices, n.mom)
  
  data$weight[] = NA
  data$weight[period.ends,] = ntop(momentum[period.ends,], n.top)  
  models$momentum = bt.run.share(data, clean.signal=F)
  
  #*****************************************************************
  # Combo: weight positions in the Momentum Portfolio according to Volatliliy
  #*****************************************************************
  weight = ntop(momentum[period.ends,], n.top) * adj.vol
  
  data$weight[] = NA
  data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)  
  models$combo = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
  
  if (F)
  {
    #*****************************************************************
    # mSlope90: weight positions in the Momentum Portfolio according to Volatliliy
    #*****************************************************************
    mSlope90 <- rollRegressionXTS(na.omit(mNorm(data$prices)),win=90)
    mSlope120 <- rollRegressionXTS(na.omit(mNorm(data$prices)),win=120)
    mSlope200 <- rollRegressionXTS(na.omit(mNorm(data$prices)),win=200)
    
    mSlope=mSlope90+mSlope120+mSlope200
    
    weight = ntop(mSlope[period.ends,]+mSlope120[period.ends,], n.top) * adj.vol
    
    data$weight[] = NA
    data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)  
    models$mSlope90 = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
  }
  
  if (F)
  {
    #*****************************************************************
    # Momentum Portfolio  but ES sizing    #MM_ES
    #***************************************************************** 
    momentum = prices / mlag(prices, n.mom)
    
    data$weight[] = NA
    data$weight[period.ends,] = ntop(momentum[period.ends,], n.top)  
    ret.p =  ROC(prices, type='discrete'); ret.p[1]<-0
    browser(mP("testES"))
    es <- rollapplyr(ret.p, width=60, FUN="ES", by.column=T, align = "right",method="historical",weights=data$weight)
    es[!is.finite(sharpe)]<-0
    
    
    
    models$momentum = bt.run.share(data, clean.signal=F)
  }
  #Finally let's create the Adaptive Asset Allocation portfolio:
  
  #*****************************************************************  
  # Adaptive Asset Allocation (AAA)
  # weight positions in the Momentum Portfolio according to
  # the minimum variance algorithm
  #*****************************************************************  
  weight = NA * prices
  weight[period.ends,] = ntop(momentum[period.ends,], n.top)
  
  for( i in period.ends[period.ends >= n.mom] ) {
    hist = ret.log[ (i - n.vol + 1):i, ]
    
    # require all assets to have full price history
    include.index = count(hist)== n.vol     
    
    # also only consider assets in the Momentum Portfolio
    index = ( weight[i,] > 0 ) & include.index
    n = sum(index)
    
    if(n > 0) {                 
      hist = hist[ , index]
      
      # create historical input assumptions
      ia = create.historical.ia(hist, 252)
      s0 = apply(coredata(hist),2,sd)      
      ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
      
      # create constraints: 0<=x<=1, sum(x) = 1
      constraints = new.constraints(n, lb = 0, ub = 1)
      constraints = add.constraints(rep(1, n), 1, type = '=', constraints)      
      
      # compute minimum variance weights                         
      weight[i,] = 0       
      weight[i,index] = min.risk.portfolio(ia, constraints)
    }
  }
  
  # Adaptive Asset Allocation (AAA)
  data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]     #   HIER MAL das Portflio-ES noch dranmultiplizieren
  models$minVar = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
  mP("#############%s#",my.rank)
  #*****************************************************************
  # my Portfolio:     für jedes vorgeschlagene Ranking-Kriterium das Modell testen
  #*****************************************************************
  # my.models=list()
  #für alle ranking modelle  
  new_Win(1)
  no=  lapply(spl(my.rank),function(rank.x)
  {
    rank.x=trim(rank.x)
    
    mP("build rank %s",rank.x)   
    #rank.x ="SMA"
    
    #ranks=get.rank(prices=prices,rank=rank.x ) # build
    ranking=foreach(sym=colnames(prices),.combine="cbind") %do%   
{
  Get.cashed.rank(rank.x,sym,prices) #read or build+read   #MMuseCash
}
if (len(ranking)==0)
  Stop("bug at get.rank")

mP("..ranks ready..")

data$weight[] = NA

data$weight[period.ends,] = ntop(ranking[period.ends,], n.top)  

models[[rank.x]]<<- bt.run.share(data, clean.signal=F)
mP(rank.x)  
################################################


mP("################## max.deviation.alloc#")


if (F)
{
  target.allocation = matrix(c(0.5, 0.5), nrow=1)
  # rebalance to target.allocation when portfolio weights are 5% away from target.allocation
  models$smart5.all = bt.max.deviation.rebalancing(data, models$aaa, target.allocation, 5/100, 0)
  
  # rebalance half-way to target.allocation when portfolio weights are 5% away from target.allocation
  models$smart5.half = bt.max.deviation.rebalancing(data, models$aaa, target.allocation, 5/100, 0.5)
}
  })
#*****************************************************************
# Create Report
#******************************************************************   
models = rev(models)
mP("reporting----->")

#plotbt.custom.report.part1(models)      
#plotbt.custom.report.part2(models)      
#plotbt.custom.report.part3(models$combo, trade.summary = TRUE)      
#plotbt.custom.report.part3(models$aaa, trade.summary = TRUE)    
strategy.performance.snapshoot(models, one.page=F,title="Timing 100 allocated",data=data)

}

#########################################################################################

if (F)
{
  load("MWorld3");  data$universe="MWorld3"
  define.Globals();   SAFE="BARCLAYS"
  
  all.sig=  spl("signal.sit.feilscher,signal.lm,signal.mom,signal.Faber.base,signal.1.smoothing,signal.any.smoothing, signal.Price.itp,signal.Faber.dyn.hysterese,signal.drawDown1,signal.MA.3,signal.days.since.high,  signal.Faber.i2,signal.rwf,signal.SMAzlema,signal.zma")
  
all.rank=  spl("rank.slope300,rank.momVolslope,rank.faber,rank.faber.b,rank.vol,rank.kanalBreite,rank.momVol,rank.momVolslope,rank.momVolES")
  
for (rankfn in all.rank) 
  A.SA (min.risk.fns = A.SA.default$A.arg$min.risk.fns, 
        S.arg=list(ranking.fn=rankfn, nTop.q=1.8,kTop.q=1.4),
        experiment=sprintf("A_%s_%s","test",rankfn),   safe=SAFE,all=F) 

}



##########################################################################################
mP("########### load rank.R")

if (F)
  list_R_functions('MLib/rank.r')

