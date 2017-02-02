################################################################################################################
#Timing-Modelle
#Marken #RUN_ALL 
################################################################################################################

options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
options(error = browser)

############################################################################################
#die looks.good.kanalBreite checken zusätzliche Bedingungen ob, es gut ist in position zu sein/zu gehen
############################################################################################
# sieht die Kanal-breite schön langeweilig aus- oder ist da so viel vola drin, dass offensichtlich noch kei
#ruhiger trend etablilert ist.

looks.good.kanalBreite<-function(p,visual=F)
{  
  kb=rank.kanalBreite(prices=p)
  
  y.long=kb;y.long[]=runquantile(kb, k=1600, probs=c( 0.90),endrule="quantile",align="right")
  y.short=kb;y.short[]=runquantile(kb, k=36, probs=c( 0.90),endrule="quantile",align="right")
  
  if (visual)
  {
    mchart(merge(kb,y.long,y.short));
    mPlots(p,merge(kb,y.long,y.short))
  }
  t.indi.flat = y.long-y.short
  
  if (visual) 
  {
  
    flat.signal=sign(t.indi.flat);flat.signal[flat.signal < 0] <-0 
    
    plotSigPrice(flat.signal,p)
  }
  sign(t.indi.flat)>=1
}


############################################################################################

faber<-function(p,n=200) p-SMA(p,n)  #zu viele verschieden defs unterwegs
ddma<-function(p,n=200) clean.matrix(bt.apply.matrix(p, function(p,n) {(runMax(p,n)-p)/p*100},n))
ddmi<-function(p,n=200) clean.matrix(bt.apply.matrix(p, function(p,n) {(p-runMin(p,n))/p*100},n))


###########
# wunderbare signal-extraktion aus glatten indicatoren

trendwechsel<-function(p, smooth, k=20,visual=F, trigger.fn = "g.Signal.r",zero=0.5,short=-1,...)
{
  #if (visual) mchart(merge(p,smooth))
  trigger.FN = match.fun(trigger.fn)
  #  m=g.Signal.r(smooth,win)
  m=trigger.FN(smooth,k=k,...)
  
  if (F)
  {
    #zero=runquantile(abs(m),k =100,align="right",probs=0.1)
    dm=abs(m - lag(m,k))
    if (zero>0)
      zero=xts(runquantile(dm,k=k*30,align="right",probs=zero),index(dm))
    if (visual) mchart(merge(dm,zero))
    
    #res=m[abs(m)<zero]
    res = m[sign(m)!= sign(lag(m,k) ) & abs(m)>=abs(zero/2) & abs(lag(m,k))>=abs(zero/2)  ]
  }
  
  t.indi=m
  thresh=xts(runquantile(abs(t.indi),k=k*5,align="right",probs=zero),index(t.indi))
  #hysterese schwelle 
  d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
  #damit nicht gleich das erste signal falsch ist
  d.signal[1] = sign(first(na.omit(diff(p,20))))
  #der auffüller ...
  sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
  #short=-1  #hier -1 schreiben wenn er auch short gehen darf
  sig[sig<0]=short
  
  # if (visual)mPlots(p,merge(t.indi,thresh,-thresh,0),sig)
  #plotSigPrice(signal=sig,prices=p)
  
  if (F)
  {
    if (visual) amark(dat=index(res))
    
    if (visual)
    {
      mPlots(merge(p,smooth),merge(m,0,zero/2,-zero/2),merge(dm))
      amark(dat=index(res))
      
      mchart(merge(p,smooth));lines(smooth,col="red",lwd=2)
      amark(dat=index(res))
    }
    tw=sign(res)
    sig=p; sig[]=NA; sig[index(tw)]=tw  ; sig = m.ifna.prev(sig)
    
  }
  first.val=-1*nval(first(na.omit(sig)));sig[is.na(sig)]=first.val
  sig2=sig-lag(sig); sig2[sig2==0]=NA; sig2=na.omit(sig2)
  tw=rbind(xts(sign(first.val),index(first(p))),sign(sig2))
  colnames(tw)="sig"
  
  
  tw=list(tw=tw, signal=xts2xts(lineal=p,sig), t.indi=xts2xts(lineal=p,t.indi),thresh=xts2xts(lineal=p,thresh))
  if (visual)
  {
    b=na.omit(merge(p,tw$signal))
    res=plotSigPrice(signal=b[,2],prices=b[,1],indi=list(smooth=merge(p,smooth),merge(tw$t.indi,tw$thresh,-tw$thresh,0)))
  }
  tw
}
########################################################################################
####################
signal.extern  <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,signal,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  
  # signal = iif(p > sma200,1,0)
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}
if (F)
{
  ls(global_arg$dat)
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,experiment="Faber.base")
  
}
#########################################################################################

signal.Faber.base <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  
  signal = iif(p > sma200,1,-1)
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p)),rank=sma200))
}
if (F)
{
  ls(global_arg$dat)
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =data$BENCH)
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)#,experiment="Faber.base")
  
}
#------------------------
#für entry ein anderes (parametrisiertes) modell wie für exit
signal.2Faber.base <-function(arg, par = mlist( sma.wLong=c(200,120,220,20),sma.wShort=c(200,120,220,20)),visual=F,...)
{
  p=mNorm(arg$clos)
  smaLong = bt.apply.matrix(p, SMA, n=as.integer(par$sma.wLong))
  smaShort = bt.apply.matrix(p, SMA, n=as.integer(par$sma.wShort))
  
  signalLong = iif(p < smaLong,-1,NA)  #bin Long und gehe Short
  colnames(signalLong) = "signalLong"
  signalShort = iif(p > smaShort,1,NA)   #bin Short und gehe Long
  colnames(signalShort)="signalShort"
  
  Signal=p;Signal[]=NA
  signal = lastSignal = NA
  for (i in 1:shape(p))
    {
     if (!is.na(signal) && signal==1)
        if (!is.na(smaLong[i]))
            signal=NA
     
     if (!is.na(signal) && signal==-1)
       if (!is.na(smaShort[i]))
         signal=NA
     
     if (is.na(signal))
       if (!is.na(smaShort[i]))
         {signal =1; }
     else
       if (!is.na(smaLong[i]))
         signal =-1
     if (is.na(signal))
       signal = lastSignal
     
     lastSignal = signal
     Signal[i]=signal
    }
 # View( merge(signalLong,signalShort))
  View(Signal["2007::2008"])
#  ,,, denk auch auch an labor_signal.r signal.TEST.1.a
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  return(list(Signal=signal, Indi=list(ma=merge(p)),rank=p))
}
if (F)
{
  ls(global_arg$dat)
  x=indi.Generic("signal.2Faber.base", global_arg, par=list(sma.wLong=300,sma.wShort=100),visual=T, TRAINSYM =data$BENCH)
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)#,experiment="Faber.base")
  
}

#########################################################################################


signal.any.smoothing <-function(arg, par = mlist( 
  glaettung=c(200,120,220,20),
  glaettung2=c(5,5,20,10),
  dw = c(-1,-1,1,1)
),visual=F,xarg=list(fn="SMA",fn2="",onlyLong=T,q.w=""),...)
{
  p=mNorm(arg$clos)[,1]
  
  res  =any.smoothing(p,glaettung = as.integer(par$glaettung),dw=as.integer(par$dw),visual=F,fn=xarg$fn,fn2=xarg$fn2,glaettung2=as.integer(par$glaettung2),onlyLong=xarg$onlyLong,q.w=xarg$q.w)
  
  #Q
  #browser(mP("signal.any.smoothing####"))  
  
  signal = res$sig
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  Indi= res$indi
  
  return(list(Signal=signal, Indi=Indi,rank=res$indi$d[,1],rank=res$indi$decide))
}

if (F)
{
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=0,dw=0),xarg=list(fn="SMA",fn2="",onlyLong=T,q.w=""),visual=T, TRAINSYM =data$BENCH)
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
  
  x=any.smoothing(dax,glaettung = 300,dw=2,visual=T,fn="EMA",fn2="SMA",glaettung2=0,onlyLong=F)
  
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=10,dw=1),xarg=list(fn="EMA",fn2="",onlyLong=F,q.w="#1%"),visual=T, TRAINSYM =-1)
  
  
  x=any.smoothing(dax,glaettung = 200,dw=5,visual=T)
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM ="DAX")
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
  
  x=any.smoothing(dax,glaettung = 300,dw=0,visual=T,fn="SMA",fn2="",glaettung2=0,onlyLong=T)
  
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=0,dw=0),xarg=list(fn="SMA",fn2="",,onlyLong=T,q.w=""),visual=T, TRAINSYM ="DAX")
  
  
  x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="ZLEMA",fn2="SMA",glaettung2=50,onlyLong=T)
  x=any.smoothing(dax,glaettung = 300,dw=1,visual=T,fn="EMA",fn2="SMA",glaettung2=0,onlyLong=T)
  
  x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="EMA",fn2="SMA",glaettung2=10,onlyLong=F)
  
  dax=dax["2003::"]
  
  
}
#............................
# ein verpackung auf der man trainieren kann ..

signal.1.smoothing<-function(arg, par = mlist( glaettung=c(200,100,300,10)),visual=F,...)
{
  p=mNorm(arg$clos)[,1]
  gl=as.integer(par$glaettung)
  onlyLong =T 
  if (has(global_arg$modell.par,"LongShort",what="T"))
    onlyLong=F
  
  res  =any.smoothing(p,glaettung = gl,dw=0,visual=F,fn="SMA",fn2="",glaettung2=0,onlyLong=onlyLong, q.w="70%")
  
  signal = res$sig
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  Indi= res$indi
  print("---->")
  return(list(Signal=signal, Indi=Indi,rank=res$indi$decide))  #evtl. hier 
  
  
  #indi.Generic("signal.any.smoothing", arg, par=list(glaettung=gl,glaettung2=0,dw=0),xarg=list(fn="SMA",fn2="",onlyLong=onlyLong,q.w="70%"),visual=visual)
  
  
}

##########################################################################################################

signal.Faber.dyn.hysterese.old <-function(arg, par = mlist( q.w=c(60,30,220,20)),visual=F,...)
{
  prices=arg$clos
  p=mNorm(arg$clos);sym=colnames(prices)
  q.w=as.integer(par$q.w)
  #indi =SMA(na.omit(p),n=as.integer(par$sma.w))
  frame=""
  
  print(sym)
  
  indi=SMA(p,200) 
  indi[is.na(indi)]=SMA(p,20)
  sig = iif(p >= indi,1, 0)
  
  #hysterese - cut:
  t.indi=p-indi  #faber
  #t.indi=SMA(sign(diff(indi,2)),n=20) #digital faber
  
  thresh=0
  # thresh=0.2
  thresh=quantile(abs(t.indi),probs=seq(0,1,0.01),na.rm=T)["60%"]
  
  thresh=rollapplyr(abs(t.indi),20, roll.quantile, allPrices=abs(t.indi),maxWin=q.w,Q="60%" )
  #library(caTools)
  #runquantile(x, k, probs, type=7, 
  #            endrule=c("quantile", "NA", "trim", "keep", "constant", "func"),
  #            align = c("center", "left", "right"))
  
  
  #hysterese schwelle 
  d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
  #damit nicht gleich das erste signal falsch ist
  d.signal[1] = sign(first(na.omit(diff(p,20))))
  #der auffüller ...
  sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
  short=0  #hier -1 schreiben wenn er auch short gehen darf
  sig[sig<0]=short
  
  #auswertung:   schnittstellen hervorheben ...hervorheben  
  cut=sig[(sig==1 & lag(sig)!=1) | (sig==short & lag(sig)!=short) ]
  CUT=p;CUT[]=0; CUT[as.Date(index(cut))]=1
  mP("long trades %d",nrow(cut))
  
  
  
  return(list(Signal=sig, Indi=list(sma=merge(p,indi),f=merge(t.indi,0,CUT,thresh,-thresh))))#,d=merge(sign(SMA(sign(diff(indi,3)),n=40))))))
}
if (F)
  x=indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(q.w=60),visual=T, TRAINSYM =-1)
##############################################################################
signal.Faber.i2 <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  sma200[is.na(sma200)]<-0
  p2=EMA(na.omit(p,20))
  sma200=runMin(sma200,60)  ####### NEU
  signal = iif(p2 >= sma200,1,0)   #sieht gut aus
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p2,p)),rank=sma200))#,fs=merge(p,fs,ema))))
}
if (F)  #sehr interessant - entscheident ist aber  sma.w=300 statt 200
  x=indi.Generic("signal.Faber.i2", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)


##########################################################################################################################
########################################################################################################################




#super ... crashed ??
roll.HOLTW_<-function(xtsPrice,visual=F,sym="DAX")
{
  #http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
  price.ts<-ts(coredata(xtsPrice[,1]),freq=5)
  #Trend-Saison-Rauschen.Aufsplittung
  db=decompose(price.ts)
  
  if (visual)
  {
    plot(db)
    ls(db)
    db$trend
    plot(xts(db$seasonal,order.by = as.Date(index(xtsPrice))))
    #plot(xtsPrice)
    #plot(db$trend,col="red")
  } 
  #  browser(mP("xx"))
  
  HW=try(HoltWinters(ts(na.omit(db$trend),frequency=7), gamma=FALSE)) #ich rechne den forecast nicht auf dem Orginial sondern nur auf dem Trend-Anteil -- läuft auf eine lineare Verlängerung des db$trend raus.
  if(inherits(HW, 'try-error')) 
  {
    res=xts(cbind(0,0,0),order.by=as.Date(index(last(xtsPrice))));
    colnames(res)=spl(sprintf("%s_TrendForecast,%s_SeasonForecast,%s_Rauschen",sym,sym,sym))
    return(res)
  }
  #plot(HW)
  if (visual)
    plot.forecast(forecast.HoltWinters(HW, h=10))
  fct=forecast.HoltWinters(HW, h=10)
  #if (what =="trend")
  trend.res=last(fct$mean)-first(fct$mean)
  #else  #season
{
    season=as.numeric(as.factor(db$seasonal))
    
    if (is.na(season) || season==0) season=0.0001
    season=season/max(season,na.rm=T)
    rauschen=as.numeric(db$random/max(db$random,na.rm=T))
    rauschen[is.na(rauschen)]<-0
    
  }
# browser()
sym=firstColname(colnames(xtsPrice[,1]))
#browser()
res=xts(cbind(trend.res,last(season),last(rauschen)),order.by=as.Date(index(last(xtsPrice))));
colnames(res)=spl(sprintf("%s_TrendForecast,%s_SeasonForecast,%s_Rauschen",sym,sym,sym))
#res= list(1,2,3)
return(res) #die Richtung des forecasts
}
roll.HOLTW<-cmpfun(roll.HOLTW_) #compilier das Teil

#........................................................................................

signal.HoltWinters <-function(arg, par = mlist(),visual=F,...)
{
  # browser(mP("sss"))
  price=mNorm(arg$clos)
  sym=colnames(price)
  data = arg$dat
  
  #TREND-FOLGER   SUPER GUTESMODELL  
  
  slopes <- rollapplyr(price, width=120, FUN=roll.HOLTW, by.column=T,visual=F,sym=sym)#what="trend",
  Entry.signal =iif(slopes >0,1,-1)
  
  return(list(Signal=Entry.signal, Indi=list(slopes=slopes)))
}
if (F)
  x=indi.Generic("signal.HoltWinters", global_arg, par=list(mlen=1,sma.w=200),visual=T,TRAINSYM = "SX5R")

##############################################################################
#Gutes Modell
##############################################################################
signal.monthlyRoc <-function(arg, par = mlist( mlen=c(1,1,3,1),sma.w=c(200,120,350,10)),visual=F,...)
{
  p=mNorm(arg$clos)
  sym=colnames(p)
  data = arg$dat
  
  #einmal die globalen slopes berechnen
  if (!exists("mSlope90"))
    mSlope90 <<- rollRegressionXTS(data$prices,win=90);colnames(mSlope90)= colnames(data$prices)
  mlen=as.integer(par$mlen)
  
  mRoc  <-  foreach(sym=colnames(p), .combine=cbind, .packages=c("quantmod")) %do%
{res=iif(runSum(monthlyReturn(p[,sym], type='log'),mlen)>=0,1,0); colnames(res)=sym ;res}

#monats-signale auf tagespositionen projezieren
temp=p;  temp[]=NA;     temp[index(mRoc),]=mRoc;    
signal.a=bt.apply.matrix(temp,ifna.prev)
####### 
#browser()

sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
signal = iif(p > sma200 | signal.a > 0| mSlope90[,sym] > 0,1,0) #sportlicher aber längere haltedauer


#signal = iif(p > sma200 ,1,0)  #reiner Faber, geringster DrawDown

return(list(Signal=signal, Indi=list(ma=merge(sma200,p),s1=signal.a),rank=signal.a))
}

if (F)
  x=indi.Generic("signal.monthlyRoc", global_arg, par=list(mlen=1,sma.w=200),visual=T,TRAINSYM =-1)

##########################################################################################
################################################################################################################

signal.Zlema.f1 <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  dax.m=mNorm(arg$clos)
  #browser() 
  smooth=ZLEMA(dax.m, as.integer(par$sma.w))
  d=dax.m-smooth
  dd=d*d*sign(d)
  dr =mROC(smooth,1)+mROC(smooth,2)+mROC(smooth,3)  #ist der smooth fast horizontal
  sdr = EMA(dr,5)
  #dd=sdr
  #wenn zu nah am trigger - NA machen
  if (F)
  {
    mPlots(merge(dr,0),dax.m,smooth)
    #wenn die änderung des smooth 0 ist aber nicht gerade nur ein sattel...
    top.p=smooth[ smooth < lag(smooth,1) & sign(sdr)!=sign(lag(sdr))]
    amark(as.Date(index(top.p)))
    low.p=smooth[ smooth > lag(smooth,1) & sign(sdr)!=sign(lag(sdr,1))]
    amark(as.Date(index(low.p)),col="green")
    wendep=rbind(top.p,low.p)
    head(smooth,50)
    amark(as.Date(index(wendep)))
  }
  #ausblenden von zu vielen trades
  #ok
  # browser(mP("#dsl"))
  
  #ok dd=iif(abs(dd) < 0.1 & (abs(dr)< 0.1 & sign(sdr)==sign(lag(sdr))), NA,dd)   #daempfung
  #in.Trend.pos(dax[,1],visual=T,main="itp",k=160,K=3)$itp  #probs=c(0.15,  0.5,  0.80)
  dd.itp= in.Trend.pos(abs(dd),visual=F,main="itp",k=160,K=3)$itp  #probs=c(0.15,  0.5,  0.80)
  dd=iif(dd.itp<0,NA,dd)
  #gefahr:     wenn beim triggerer-punkt die eine Zusatzbedingung nicht erfüllt ist bleibt wegen der daempfung in falscher pos haengen
  #dafuer brauchts dann das stop-system(unten)
  #dd=iif(abs(dd) < 0.1 & abs(sdr) < 0.1, NA,dd)
  
  #plot(abs(dr))
  dd=m.ifna.prev(dd)
  
  #wenn weit genug vom letzten 
  sig=sign(dd)
  #mPlots(dax.m,dd.itp,merge(dd,0),sig)
  
  #aufbauen der trades-liste fuer das stopsystem
  trades=sig[abs(sig-lag(sig))>0]
  trades=rbind(trades,first(sig[sig !=0]))
  entry.p= dax.m[as.Date(index(trades))]
  #stop-sytem
  equity = cbind(dax.m,entry.p,trades) #;equity[,2]=NA
  equity=m.ifna.prev(equity)
  #open guv:  (aktueller_kurs- einstands_kurs)*pos
  equity.o = (equity[,1]-equity[,2])*equity[,3]
  #stop2flat:
  dd=iif(equity.o < -0.10,0,dd)
  #stop2pos.drehen
  #dd=iif(equity.o < -0.15,-dd,dd)
  
  purePlot(merge(equity.o,0))
  pos= sign(dd);colnames(pos)="pos"
  
  plotSigPrice(signal=pos,prices=dax.m,indi=list(a=merge(dax.m,smooth),guv.o=merge(equity.o,0)))
  
  maxPosLos=equity[which(equity.o==min(equity.o,na.rm=T))][,1]
  amark(DateS(maxPosLos),col="blue")
  
  sig=pos
  trades=sig[abs(sig-lag(sig))>0]
  trades=rbind(trades,first(sig[sig !=0]))
  entry.p= dax.m[as.Date(index(trades))]
  #stop-sytem
  Trades = cbind(entry.p,trades) ;colnames(Trades)=spl("p,pos")
  equity.r = (Trades[,"p"]-lag(Trades[,"p"]))*lag(Trades[,"pos"]);colnames(equity.r)="guv"
  sum(equity.r,na.rm=T)
  
  ok.trades=sum(iif(equity.r > 0,1,0))/shape(equity.r)*100
  Trades=merge(Trades,lag(equity.r,-1))
  
  #print(Trades)
  
  mP("erfolgreiche Trades: %2.1f pct",ok.trades)
  
  
  return(list(Signal=sig, Indi=list(a=merge(dax.m,smooth),guv.o=merge(equity.o,0))))
}
if (F)
{
  x=indi.Generic("signal.Zlema.f1", global_arg, par=list(sma.w=600),visual=T, TRAINSYM =data$BENCH)
  x=indi.Generic("signal.Zlema.f1", global_arg, par=list(sma.w=600),visual=T, TRAINSYM = -1)
  
}
################################################################################################################

signal.lm<-function(arg,par = mlist(win=c(200,50,300,50)),visual=F,...)
{
  use.thresh.smooth=F   #sonst verliert er tatsächlich ein gutes stück seiner ranking-quality
  #browser(mP("signal.lm"))
  clos= arg$clos   #multivariat
  win = as.integer(par$win)
  
  #die beiden Indikatoren deren Schnitt mich interessiert:
  mP("win  is:  %d,  len(clos):%d ",win,shape(clos))
  if (win >= shape(clos))   #blödsinns-wert -evtl. aus trainings
  {
    r=clos;r[]=0
  }
  else
    r=rollRegressionXTS(Y=clos,win=win)
  
  #  SMA.val =  bt.apply.matrix(clos, SMA, smaN)
  
  signal=sign(r)
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  
  if (use.thresh.smooth)
  {
    t.indi= r
    tsm = thresh.smooth(clos,t.indi,kwin=win,q.w=0.80)
    thresh = tsm$thresh ;signal = tsm$Signal
    return(list(Signal=signal, Indi=list(lm=merge(scaleTo(r,range(clos)), clos),ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )))) 
  }
  return(list(Signal=signal, Indi=list(merge(scaleTo(r,range(clos)), clos)),rank=r))
}
if (F)
  x=indi.Generic("signal.lm", global_arg, par=list(win=150),visual=T, TRAINSYM = data$BENCH)
################################################################################################################



signal.zma<-function(arg,par = mlist(zwin=c(100,50,300,50)),swin=c(200,50,300,50),visual=F,...)
{
  use.thresh.smooth=F
  #browser(mP("signal.lm"))
  p=clos= arg$clos   #multivariat
  zwin = as.integer(par$zwin)
  swin = as.integer(par$swin)
  
  f1=ZLEMA(p,zwin)
  f2=SMA(p,swin)
  signal1 = sign(f1-f2)
  signal2= sign(rollRegressionXTS(p,50))
  signal=sign(signal1+signal2)
  signal[signal==0]=NA; signal=m.ifna.prev(signal)
  
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  t.indi= signal1+signal2
  
  if (use.thresh.smooth)
  {
    tsm = thresh.smooth(clos,t.indi,kwin=win,q.w=0.80)
    thresh = tsm$thresh ;signal = tsm$Signal
    return(list(Signal=signal, Indi=list(lm=merge(scaleTo(r,range(clos)), clos),ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi)) 
  }
  if (colnames(clos)==SAFE)
    signal[]=1
  return(list(Signal=signal, Indi=list(f=merge(p,f1,f2)),rank=t.indi))
}
if (F)
{
  x=indi.Generic("signal.zma", global_arg, par=list(zwin=100,swin=200),visual=T, TRAINSYM = data$BENCH,)
  x=indi.Generic("signal.zma", global_arg, par=list(zwin=100,swin=200),visual=T, TRAINSYM = -1)
}

##########################################################################################

rollRegressionXTS.rwf<-function(Y=prices,win=200,n.ahead=30)
{
  require(forecast)
  
  #https://www.otexts.org/fpp/2/5  
  rwf.Forecast <- function(p,h,...) {
    x=ts(p,frequency=356)
    
    #1
    fct=rwf(x,drift=TRUE,h= h)$mean
    
    #2  fit <- ets(x, ...)    #schlecht und langsam
    #  fct =forecast(fit, h=h, level=99)$mean
    
    #3 fit <- auto.arima(x, ...)   #sehr viele hin und her signale - evtl. gut für hf-trading
    # fct =forecast(fit, h=h, level=99)$mean
    
    res = (last(fct)-last(x)) / last(x)*100
  }
  
  
  dolm1 <- function(p,nplus){
    #fit=lm.FITm(p=p+10000,visual=F,getIndex=T,level=.90,nplus=nplus)
    #fct=(last(fit$channel[,1]))-10000
    rwf.Forecast(p,h=nplus)
    
  }
  
  slope90=rollapplyr(Y, win, dolm1, by.column = T,nplus = n.ahead)
}


if (F)
{
  p=data$prices[,data$BENCH]
  slope200=rollRegressionXTS.rwf(p,200) 
}

############################################################################
#die hysterese-glaettung als funktion
#siehe auch  t.indi= probAny(t.indi,lookback.len=win)-0.5
###########################################################################

thresh.smooth<-function(p,t.indi, kwin=100,q.w=0.80)
{
  thresh=0
  if (q.w >0)
    thresh= runquantile(abs(t.indi), k=kwin, probs=q.w,align="right")
  #hysterese schwelle 
  d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
  #damit nicht gleich das erste signal falsch ist
  d.signal[1] = sign(first(na.omit(diff(p,20))))
  #der auffüller ...
  sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
  if (has(global_arg$modell.par,"LongShort",what="T"))
    short=-1
  else
    short=0  #hier -1 schreiben wenn er auch short gehen darf
  sig[sig<0]=short
  
  signal=p; signal[]=NA
  signal[]=merge(signal[,1], iif(t.indi < 0 ,short,1))[,2]
  #TODO hier noch einfügen:
  #wenn der lm-winkel zu gering ist und kein itp-  - NICHT die pos wechseln !!!
  cut=sig[(sig==1 & lag(sig)!=1) | (sig==short & lag(sig)!=short) ]
  CUT=p;CUT[]=0; CUT[as.Date(index(cut))]=1
  mP("long trades %d",nrow(cut))
  return(list(Signal=xts2xts(lineal=p,sig),CUT=CUT,thresh=thresh))
}
###################################################################

#mit rwf() aus dem forecast-paket .. geht teilweise ganz gut

signal.rwf<-function(arg,par = mlist(win=c(200,50,300,50)),visual=F,...)
{
  #browser(mP("signal.lm"))
  clos= arg$clos   #multivariat
  win = as.integer(par$win)
  
  #die beiden Indikatoren deren Schnitt mich interessiert:
  mP("win  is:  %d,  len(clos):%d ",win,shape(clos))
  if (win >= shape(clos))   #blödsinns-wert -evtl. aus trainings
  {
    r=clos;r[]=0
  }
  else
    r=rollRegressionXTS.rwf(Y=clos,win=win)
  
  #  SMA.val =  bt.apply.matrix(clos, SMA, smaN)
  
  signal=sign(r)
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  t.indi=r
  tsm = thresh.smooth(clos,t.indi)
  thresh = tsm$thresh
  signal = tsm$Signal
  CUT=tsm$CUT
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  #gib das signal zurück, und auch noch die Hilfsvariablen die Du evtl. in indi.Generic() sehen willst:
  
  return(list(Signal=signal, Indi=list(merge(t.indi,0,CUT,thresh,-thresh)),rank=t.indi))
}
if (F)
  x=indi.Generic("signal.rwf", global_arg, par=list(win=150),visual=T, TRAINSYM = -1)#data$BENCH





###################################################################


signal.days.since.high<-function(arg,par = mlist(nHold=c(100,50,300,50),nHigh=c(200,50,300,50)),visual=F,...)
{
  
  myStrat <- function(x, nHold=100, nHigh=200) {
    position <- ifelse(daysSinceHigh(x, nHigh)<=nHold,1,0)
    c(rep(0,nHigh-1),position)
  }
  
  #browser(mP("signal.lm"))
  clos= arg$clos   #multivariat
  nHold = as.integer(par$nHold)
  nHigh = as.integer(par$nHigh)
  
  #die beiden Indikatoren deren Schnitt mich interessiert:
  #mP("win  is:  %d,  len(clos):%d ",win,shape(clos))
  
  signal=xts( myStrat(clos,nHold,nHigh),index(clos))
  
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  #t.indi=r
  #tsm = thresh.smooth(clos,t.indi)
  #thresh = tsm$thresh
  #signal = tsm$Signal
  #gib das signal zurück, und auch noch die Hilfsvariablen die Du evtl. in indi.Generic() sehen willst:
  dsh=daysSinceHigh(clos, nHigh)
  dsh=xts(dsh,last(index(clos),len(dsh)))
  return(list(Signal=signal, Indi=list(days=dsh),rank=dsh))#list(merge(t.indi,0,CUT,thresh,-thresh))))
}
if (F)
{
  x=indi.Generic("signal.days.since.high", global_arg, par=list(nHold=100,nHigh=200),visual=T, TRAINSYM = -1, experiment="xxx")
  
  global_ParTable <<-NULL
  train.frame = "::2010"
  global_arg<<-list(clos=data$prices[train.frame],dat=data)  #MANDATORY !!!!!
  global_commission = 0.0005   #sys.Run() nimmt diese commission !!!!
  
  global_StartDate <<-  DateS(last(global_arg$clos))
  
  TrainIndicator(global_StartDate_=global_StartDate, indiName = "signal.days.since.high",  visual=T, TRAINSYM =-1)# data$BENCH)  "SXEBP"
}
###################################################################

################################################################################################################
signal.Price.itp <-function(arg, par = mlist( sma.w=c(140,-220,220,20)),visual=F,...)
{
  sma.w=as.integer(par$sma.w)
  mP("signal.Price.itp   sma.w %d",sma.w)
  p=mNorm(arg$clos)
  sym=colnames(p)
  mode = sign(sma.w); sma.w = win= abs(sma.w)
  #zyklisch
  ###  itp=in.Trend.pos(p,visual=F,main="itp",k=160,K=2,probs=c(0.1,  .20,  0.80))
  #  itp=in.Trend.pos(p,visual=F,main="itp",k=160,K=3,probs=c(0.25,  0.5,  0.75))
  #itp=in.Trend.pos(p,visual=F,main="itp",k=sma.w,K=3,probs=c(0.30,  0.5,  0.60))
  t.indi = itp=in.Trend.pos(p,visual=F,main="itp",k=sma.w,K=3,probs=c(0.20,  .5,  0.95))  
  #itp=EMA(itp,35)
  signal = iif(itp>=1,1,iif(itp <= -1,-1,NA))
  
  if (mode <0)
  {
    #tsm = thresh.smooth(p,t.indi,kwin=win,q.w=0.8)
    #thresh = tsm$thresh ;signal = tsm$Signal; 
    t.indi= probAny(t.indi,lookback.len=100)-0.5
    signal = sign(t.indi)
    colnames(signal) = colnames(p)
    
  }
  
  #antizyklisc
  #itp=in.Trend.pos(p,visual=F,main="itp",k=160,K=3,probs=c(0.25,  0.5,  0.75))$itp
  #signal = iif(itp>=5,-1,iif(itp <= -5,-1, itp))
  
  #bleib wo du bist
  signal = m.ifna.prev(signal)
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  #browser(mP("xxx"))
  # signal=iif(swapVola(p,7,5,visual=T)> 2,0,signal)
  #  signal=iif( roll.Range(p,30,visual=T)> 0,0,signal)
  
  return(list(Signal=signal, Indi=list(ma=merge(itp,p)),rank=itp))
}
if (F)
{
  ls(global_arg$dat)
  x=indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=-60),visual=T, TRAINSYM ="SXQBP")
  #super
  x=indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
  #mies
  x=indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=-300),visual=T, TRAINSYM =-1)
  #best
  x=indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=140),visual=T, TRAINSYM =-1)#
  x=indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=-60),visual=T, TRAINSYM =-1)#,experiment="signal.Price.itp")
  global_ParTable <-NULL
  TrainIndicator( indiName = "signal.Price.itp",  visual=T, TRAINSYM =0)# 
  x=indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=140),visual=T, TRAINSYM =-1) 
  x=indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=-140),visual=F, TRAINSYM =-1) 
  
}
#########################################################################################

spline.fcst <-function(p,glaettung=0.8)
{
  n.ahead=30
  fit=smooth.spline(x=1:len(p), y=p,spar=glaettung) # und damit dann die Quantile machen ...
  fcst=predict(fit, (len(p):(len(p)+n.ahead)))
  new.return = last(fcst$y)-last(p)
}
#.........................................................................

get.best.glaettung<-function(P, mode=-1)#sym=data$BENCH,frame="")
{
  #p=global_arg$dat$prices[frame,sym]
  #P.d=  select.dates(p,"months")
  #P=p[P.d]
  
  qs <- NULL
  
  pars=seq(0.1,1,0.1) #diese glaettungsparameter werden ausprobiert
  for (gl in pars) # gut sind  0.6 oder 0.8 auf monatsdaten
  { 
    #  gl=0.8
    #t.indi= rollapplyr(P,FUN="spline.fcst",width=min(len(P)-1, 240),glaettung=gl) 
    t.indi=P; t.indi[]=NA
    for (i in (20:len(P)))
    {
      end.d = as.Date(index(P[i]));frame=sprintf("::%s",end.d); pi=P[frame]
      t.indi[i]= spline.fcst(pi, gl)
    }  
    if (mode <0)
      t.indi= probAny(t.indi,lookback.len=100)-0.5
    #doku
    fit=smooth.spline(x=1:len(P), y=coredata(P), spar=gl) # und damit dann die Quantile machen 
    fit.y = P; fit.y[]=fit$y
    mP("teste glaettung %f",gl)
    
    #Tquality berechnen
    mod= m.bt.run(P,t.indi,visual=F,thresh.smooth.win=0,main=sprintf("%s %d / %f",colnames(P),mode,gl), y= fit.y) #-15) 
    if (is.null(qs) )  qs<- nval(mod$Tquality) else qs <- c(qs,mod$Tquality)  
  }
  bestPar = pars[order(-qs)][1]
  mP("get.best.glaettung:  found  %f",bestPar)
  #browser()
  bestPar
}
#get.best.glaettung<-cmpfun(get.best.glaettung_) #compilier das Teil

#.............................................................................

signal.spline <-function(arg, par = mlist( gl=c(0.3,0.1, 1,0.1)),visual=F,...)
{
  gl= nval(par$gl)
  mP("signal.spline   gl= %f",gl)
  p=mNorm(arg$clos)
  sym=colnames(p)
  mode = sign(gl); gl = win= abs(gl)
  #  P.d=  select.dates(p,"months")
  P.d= as.Date(index(p))
  p=p[P.d]
  t.indi= p;  t.indi[]=NA
  
  for (i in (200:len(P.d)))
  {
    end.d = P.d[i];frame=sprintf("::%s",P.d[i]); pi=p[frame]
    t.indi[i]= spline.fcst(pi, gl)
    
    # if (i %% 3000 == 0)   #12 monate sind um:  lerne den besten glaettungspar
    if (F && i==1000)
      gl=get.best.glaettung(pi,mode)
  } 
  
  if (mode <0)
  {
    #tsm = thresh.smooth(p,t.indi,kwin=win,q.w=0.8)
    #thresh = tsm$thresh ;signal = tsm$Signal; 
    t.indi= probAny(t.indi,lookback.len=100)-0.5
  }
  t.indi2 = xts2xts(lineal=arg$clos, t.indi)
  t.indi = m.ifna.prev(t.indi2)
  signal = sign(t.indi)
  p=mNorm(arg$clos)
  
  fit=smooth.spline(x=1:len(p), y=coredata(p), spar=gl) # und damit dann die Quantile machen 
  fit.y = p; fit.y[]=fit$y
  
  colnames(signal) = colnames(p)  
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  return(list(Signal=signal, Indi=list(ma=merge(t.indi,p,fit.y)),rank=t.indi))
}

if (F)
{
  ls(global_arg$dat)
  global_ParTable=NULL
  x=indi.Generic("signal.spline", global_arg, par=list(gl=-0.4),visual=T, TRAINSYM =data$BENCH)
  #super
  x=indi.Generic("signal.spline", global_arg, par=list(gl=-0.4),visual=T, TRAINSYM =-1)
  #mies
  x=indi.Generic("signal.spline", global_arg, par=list(gl=-0.4),visual=T, TRAINSYM =-1)
  #best
  x=indi.Generic("signal.spline", global_arg, par=list(gl=-0.4),visual=T, TRAINSYM =-1)#
  x=indi.Generic("signal.spline", global_arg, par=list(gl=-0.4),visual=T, TRAINSYM =-1)#,experiment="signal.spline")
  global_ParTable <-NULL
  TrainIndicator( indiName = "signal.spline",  visual=T, TRAINSYM =0)# 
  x=indi.Generic("signal.spline", global_arg, par=list(gl=-0.4),visual=T, TRAINSYM =-1) 
  x=indi.Generic("signal.spline", global_arg, par=list(gl=-0.4),visual=F, TRAINSYM =-1) 
  
}
#########################################################################################

#####################################################################################
signal.wonder<-function(arg, par = mlist( k=c(20,10,40), zlemaN=c(20,10,40)),visual=F,...)
{
  slopes=diff(arg$clos,as.integer(par$k))
  zlem= bt.apply.matrix((sign(slopes)), ZLEMA, n=as.integer(par$zlemaN))#,ratio=0.1) #6
  signal=iif(zlem >=0,1,-1)
  
  return(list(Signal=signal, Indi=list(zlem)))
}

if (F)
{  
  global_ParTable=NULL
  clos=prices[,2:2]
  colnames(clos)
  HotLags(clos,n=20,visual=T)
  
  global_arg = list(clos=clos)
  global_arg<<-list(clos=data.info(data),dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL
  #global_objectId <<-paste("TREND","DAX","signal.SMAzlema")
  globalTrainLevel <<-10
  
  res=signal.wonder(global_arg)   # DOW: 10,150
  res=signal.wonder(global_arg,list( k=10, zlemaN=150),visual=T)   # DOW: 10,150
  res=signal.wonder(global_arg,list( k=76, zlemaN=2),visual=T)   # DOW: 10,150
  
  res=signal.wonder(global_arg,list( k=10, zlemaN=210),visual=T)   # Dax:   10,  210  und  154, 4
  res=signal.wonder(global_arg,list( k=154, zlemaN=4),visual=T) 
  
  #das ganze mit chart und Auswertung
  x=indi.Generic("signal.wonder", global_arg, list( k=154, zlemaN=400), visual=T, TRAINSYM=0)   #jeder mit gleichen BestParams
  x=indi.Generic("signal.wonder", global_arg,list( k=154, zlemaN=4), visual=T, TRAINSYM="DAX")  
  x=indi.Generic("signal.wonder", global_arg,list( k=154, zlemaN=4), visual=T, TRAINSYM=-1)  
  
}

if (F)
{
  ###########################  TRAINING !!! 
  test.system = "signal.wonder"
  global_arg<<-list(clos=prices[,1:2])  #Definition der Preise
  global_ParTable <<-NULL  
  global_objectId <<-paste("TREND","Dax",test.system)   #Welcher Indikator 
  global_StartDate <<- DateS(last(prices))  #der zeitstempel- zusammen mit globla_objectId der joined key in global_ParTable
  #Parametrisierung des Trainings .. ich leg sogar die increment-Werte für den GRID selber fest
  
  mlist( k=c(20,10,30,2), zlemaN=c(20,10,150,10))
  #So sieht jetzt die global_ParTable aus:
  mlist(mode="R")
  global_ParTable$par
  global_ParTable
  
  #Training aller Zeitreihen durchführen  - bei TRAINSYM=0 - bekommen sie einen einheitlichen BestParams - sonjst findet
  #sich für jede zeitreihe ein individueller BestParam
  TrainIndicator (opti="GRID",indiName = test.system,visual=T,TRAINSYM=0)  #ein Bestparam für alle wertpapiere  
  TrainIndicator (opti="GRID",indiName = test.system,visual=T,TRAINSYM=-1)  
  #und das hat er gefunden:
  global_ParTable
  global_ParTable$par
  
  #Abrufen der Indikatoren mit ihren Bestparam - dannach wird das summensystem mit einem Buy-Hold verglichen
  x=indi.Generic(test.system, global_arg,visual=T, TRAINSYM=0)   #jeder mit gleichen BestParams
  x=indi.Generic(test.system, global_arg,visual=T, TRAINSYM=-1)  #jeder mit seinen eigenen BestParams
  
  
  ###########################
  
}
####################################################################################
#Betrachte die ROC-Differenzen auf für Differenzen die den 10 wichtigsten HotLags
#entsprechen und handle in Richtung der gewichteten momentum-Summe dieser Differenzen.
#sehr viele Trades.
#FAZIT:  für Dax und Dow läßt sich jeweils ein anderer Parameter-Satz finden der bei
#hf-trading (ca 4 Tage Haltedauer) gut aussieht - aber eben KEIN gemeinsamer SATZ.

####################################################################################
signal.hotLags<-function(arg, par = mlist(zlemaN=c(20,10,40,10)),visual=F,...)
{
  symi=1   #wähle hier, welches Sympol des xts-pakets du sehen möchtest
  data= arg$dat
  clos= arg$clos[,symi]   #univariat
  zlemaN = as.integer(par$zlemaN)
  
  resHOT__ <<- clos;resHOT__[]=0  #hier wird die Anzahl der HotLags reingeschrieben.
  #auch die HotLags dürfen nur für die bereits verangene Zeitreihe berechnet werden (future-danger)
  
  slopes= rollapplyr(clos,width=100,FUN=
                       function(close) { 
                         today = DateS(last(close))
                         hotLag2 = HotLags2(close,n=100-1)  #schau Dir die letzten 100 Tage an
                         resHOT__[today]<<-sum(hotLag2$lag)
                         mP("%s %s ",today,toString(hotLag2$lag))
                         maxL = min(len(hotLag2$lag),10)  #maximal 10 HotLags-Werte berücksichtigen
                         diffLag = lapply(c(1:maxL),
                                          function(x) 
                                          {LAG = hotLag2$lag[x]; VAL = hotLag2$value[x];
                                           if (sum(hotLag2$lag) < 2 && LAG==1) LAG=zlemaN  #oft gibts nur den HotLag 1 .. dann muss ein anderer Wert ran... 
                                           momentum(close,n=abs(LAG))* -VAL  #gewichtet mit der Wichtigkeit es Lags
                                           #ROC(ZLEMA(close, lag=abs(LAG))) * -VAL
                                           #bad: ROC(ZLEMA(close, lag=abs(LAG)),n=abs(LAG)) * -VAL
                                          })
                         #bilde über alle Lag-Momentum-Werte die (data.frame)-Row-Summe
                         res = na.omit(as.xts(data.frame(diffLag)))
                         res[] = rowSums(res)
                         res = last(res[,1]) },    by.column=T)
  
  
  zlem = slopes
  
  #zlem=ZLEMA(slopes,n=5)
  #zlem= bt.apply.matrix(slopes, ZLEMA, n=15)#,ratio=0.1) #6
  
  #zlem= bt.apply.matrix((sign(slopes)), ZLEMA, n=3)#,ratio=0.1) #6
  
  signal=iif(zlem >=0,1,-1)
  
  
  
  #  ZLEM= bt.apply.matrix(clos, ZLEMA, 5)#n=as.integer(par$zlemaN))
  #  dZLEM =-diff(ZLEM)
  #  signal = iif(resHOT__< 2, sign(dZLEM) ,signal)
  #  signal = sign(dZLEM)
  if (visual)
  {
    symi=1   #wähle hier, welches Sympol des xts-pakets du sehen möchtest
    plotSigPrice(signal=signal[,symi],prices=arg$clos[,symi],indi=list(zlem[,symi],resHOT__),xMarker=list(),main=Title("signal.hotLags",arg,par))  
  } 
  return(list(Signal=signal, Indi=list(zlem)))
}
if (F)
  res=signal.hotLags(global_arg,list( zlemaN=3),visual=T) 


####################################################################################
#ein sehr einfaches System mit dem ich den Optimierer teste
####################################################################################
signal.SMAzlema<-function(arg,par = mlist(smaN=c(20,20,100,10), zlemaN=c(10,10,90,10)),visual=F,...)
{
  clos= arg$clos   #multivariat
  # browser()
  smaN = as.integer(par$smaN)
  zlemaN = as.integer(par$zlemaN)
  
  SMA.val =  bt.apply.matrix(clos, SMA, smaN)
  ZLEMA.val = bt.apply.matrix(clos, ZLEMA, zlemaN)
  
  signal=iif(SMA.val - ZLEMA.val <= 0, 1,-1)
  
  t.indi=ZLEMA.val - SMA.val  
  tsm = thresh.smooth(clos,t.indi)
  thresh = tsm$thresh;  CUT=tsm$CUT
  signal = tsm$Signal
  
  #gib das signal zurück, und auch noch die Hilfsvariablen die Du evtl. in indi.Generic() sehen willst:
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  #gib das signal zurück, und auch noch die Hilfsvariablen die Du evtl. in indi.Generic() sehen willst:
  
  return(list(Signal=signal, Indi=list(sma=merge(SMA.val,ZLEMA.val),indi=merge(t.indi,0,CUT,thresh,-thresh))))
  
  
  return(list(Signal=signal, Indi=list(merge(SMA.val,ZLEMA.val)),rank=t.indi))
}

if (F)
{
  res=signal.SMAzlema(global_arg,list(smaN=20, zlemaN=110),visual=T) 
  indi.Generic("signal.SMAzlema", global_arg,  list(smaN=20, zlemaN=110), visual=T,TRAINSYM="DAX")  
  global_arg<<-list(clos=data.info(data),dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL
  global_objectId <<-paste("TREND","DAX","signal.SMAzlema")
  globalTrainLevel <<-10
  
  indi.Generic("signal.SMAzlema", global_arg)
  TrainIndicator(global_StartDate_ = DateS(last(prices)), opti="GRID",indiName = "signal.SMAzlema",visual=F,TRAINSYM="DAX")  
  
  
  i.system=indi.Generic("signal.SMAzlema", global_arg, par = list(bbM=100,smaN=20, zlemaN=80) ,visual=T, TRAINSYM=-1,do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  
  i.system=indi.Generic("signal.SMAzlema", global_arg, par = list(smaN=150, zlemaN=150) ,visual=T, TRAINSYM=-1,do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
}

###################################################################################
#versuch eines technischen stop-systems
###################################################################################


signal.OLDtechStops<-function(arg,par = mlist(bbM=c(100,100,300,150),smaN=c(20,20,120,60), zlemaN=c(5,4,10,5)),visual=F,...)
{
  bbM= as.integer(par$bbM)
  zlemaN= as.integer(par$zlemaN)
  smaN = as.integer(par$smaN)
  #symbols = colnames(arg$clos)
  symbols = toupper((colnames(arg$clos)))
  
  #mP("signal.techStops-----")
  #browser()
  frame = fromToS(arg$clos)
  
  #price = arg$clos
  #z=Cl(price)
  clos=m.apply(arg$dat, onlysym=symbols, PreFun=Cl,frame=frame)
  
  #  browser()
  
  ##bb=  BBands(HLC(arg$dat),n=as.integer(par$bbM), maType = SMA)
  
  #  bb.dn=m.apply(arg$dat,onlysym=symbols,Fun=BBands, PreFun=HLC, n=bbM ,maType = SMA,frame=frame)[,"dn"]
  
  bb.dn=m.apply(arg$dat,onlysym=symbols,Fun=BBands, PreFun=HLC, n=bbM ,maType = SMA,frame=frame,select="dn")
  
  #z=ZLEMA(Lo(price),n=zlemaN)
  z=m.apply(arg$dat,onlysym=symbols, Fun=ZLEMA, PreFun=Lo, n=zlemaN ,frame=frame)
  
  #sma=SMA(Cl(price),n=smaN)
  sma=m.apply(arg$dat,onlysym=symbols, Fun=SMA, PreFun=Cl, n=smaN ,frame=frame)  
  
  #rL=runLengthEnc2(iif(mROC(Cl(price))>=0,1,-1))
  rL=m.apply(arg$dat,onlysym=symbols, Fun=function(x,...)runLengthEnc2(iif(mROC(Cl(x))>=0,1,-1)) ,PreFun=Cl, n=smaN ,frame=frame)  
  
  sz = z-sma
  cudLos = iif( (sz < 0  & rL >3 ) | sz >=0, 1,0)  
  Tstops= iif(sz <0  ,0 ,1) #z < bb[,"dn"] || cudCl <= -3
  head(Tstops)
  #Tstops=EMA(Tstops ,n=100)
  Tstops = iif(Tstops <0.5 | rL< (-4) | z < bb.dn,0, 1) #  & cudLos==0 | rL < 
  signal = Tstops
  return(list(Signal=signal, Indi=list(z_sma=merge(z,sma),cudLos=rL,bb_dn=merge(clos,bb.dn))))
  
  return(list(Signal=signal, Indi=list(merge(bb.dn,z,sma),cudLos)))
}


signal.techStops<-function(arg,par = mlist(bbM=c(100,100,300,150),smaN=c(20,20,120,60), zlemaN=c(5,4,10,5)),visual=F,...)
{ 
  bbM= as.integer(par$bbM)
  zlemaN= as.integer(par$zlemaN)
  smaN = as.integer(par$smaN)
  
  #  colnames(prices)
  symbols = toupper((colnames(arg$clos)))
  ls(global_arg$dat)
  frame = fromToS(arg$clos)
  
  #cat(symbols)
  #browser()
  #  lapply(arg$dat, function(x)print(colnames(x)))
  
  clos=m.apply(global_arg$dat, onlysym=symbols, PreFun=Cl,frame=frame)
  lo=m.apply(global_arg$dat, onlysym=symbols, PreFun=Lo,frame=frame)
  #browser()
  bb.dn=m.apply(arg$dat,onlysym=symbols,Fun=BBands, PreFun=HLC, n=bbM ,maType = ZLEMA,frame=frame,select="dn,up")
  
  #browser()
  z=m.apply(arg$dat,onlysym=symbols, Fun=ZLEMA, PreFun=Lo, frame=frame,n=zlemaN )
  sma=bt.apply.matrix(clos,SMA,n=smaN)
  
  rL=bt.apply.matrix(clos, function(x) {runLengthEnc2(iif(ROC(x,n=1,type="discrete")>=0,1,-1))} )  
  
  sz = z-sma
  cudLos = iif( (sz < -3  & rL >3 ) | sz >=0, 1,0)  
  Tstops= iif(sz <0  ,0 ,1) #z < bb[,"dn"] || cudCl <= -3
  #head(Tstops)
  #Tstops=EMA(Tstops ,n=100)
  
  rL=bt.apply.matrix(bb.dn[,1], function(x) {runLengthEnc2(iif(ROC(x,n=1,type="discrete")>0,1,-1))} )  
  #Tstops = iif(Tstops <0.5 | rL< (-4) | z < bb.dn,0, 1) #  & cudLos==0 | rL < 
  Tstops = iif( z < bb.dn[,1] | rL <= -3,  0, 1) #  & cudLos==0 | rL < 
  
  #Tstops = iif( lo <= bb.dn[,1] ,  -1, 1) #  sehr schnell - gut gegen steil-sturz
  #Tstops = iif( sz <= 0 ,  -1, 1) #  slow
  
  Tstops = iif( sz <= 0 ,  -1, Tstops) #  slow
  md= abs(diff(sma,3))
  #plot(md)
  # Tstops = iif (md > 15,Tstops,0) 
  #
  #Tstops = iif(rL  > 2 ,1,lag(Tstops))  #schneller einschalten ...führt zu viele minis...
  signal = Tstops
  
  #  cudLos=rL,
  return(list(Signal=signal, Indi=list(z_sma=merge(z,sma),bb_dn=merge(lo,bb.dn))))
  
  
  
  # mchart(merge(z,sma))  
  sz = z-sma
  cudLos = iif( (sz < 0  & rL >3 ) | sz >=0, 1,0)  
  Tstops= iif(sz <0  ,0 ,1) #z < bb[,"dn"] || cudCl <= -3
  Tstops =  bt.apply.matrix(Tstops, EMA, n=smaN)
  
  Tstops = iif(Tstops <0.5 | rL< (-4) | z < bb.dn,0, 1) #  & cudLos==0 | rL < 
  
  Tstops = cudLos
  #  Tstops =  bt.apply.matrix(Tstops, EMA, n=100)
  
  #browser()
  Tstops = iif (z-sma < 0, 0, 1)   #zlema-macd   
  
  rL=bt.apply.matrix(Tstops, function(x) runLengthEnc2(x))
  
  #Tstops = iif(rL > 3, Tstops,0 ) 
  
  signal = Tstops
  mP("signal.techStops  <<<")
  
  return(list(Signal=signal, Indi=list(z_sma=merge(z,sma),cudLos=rL,bb_dn=merge(clos,bb.dn))))
}



if (F)
{
  
  prices=data.info(data)
  
  global_arg<<-list(clos=prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  #global_FastTrain=5
  #ich kann das Training gröber oder feiner gestalten indem ich die Variable
  #global_FastTrain > 0 setzte. dies ist dann die Anzahl der Zwischenschritte pro
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices))
  globalTrainLevel <<-10   
  global_objectId <<-paste("TREND","DAX","signal.techStops") 
  global_xMarker<<-list()
  
  TrainIndicator( opti="GRID", indiName = "signal.techStops",  visual=T, TRAINSYM = "DAX")
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  i.system=indi.Generic("signal.techStops", global_arg, par = list(bbM=100,smaN=80, zlemaN=9) ,visual=T, TRAINSYM="DAX",do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  i.system=indi.Generic("signal.techStops", global_arg, par = list(bbM=100,smaN=200, zlemaN=9) ,visual=T, TRAINSYM="DAX",do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  i.system=indi.Generic("signal.OLDtechStops", global_arg, par = list(bbM=100,smaN=80, zlemaN=9) ,visual=T, TRAINSYM=-1,do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  
  i.system=indi.Generic("signal.techStops", global_arg, par = list(bbM=166,smaN=20, zlemaN=6) ,visual=T, TRAINSYM="DAX",do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  
  
  signal.techStops(global_arg)
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=1,do.assemble.Signals=T)  #jeder mit seinen eigenen 
  i.system=indi.Generic("signal.techStops", global_arg,visual=T, TRAINSYM=-1)  #jeder mit seinen
}

##############################################################################################
##############################################################################################

signal.MA.3_<-function(arg, par = mlist(zlemaN=c(3,3,20,10), slow=c(40,20,120,20), fast=c(40,20,120,20)), visual=F,...)
{
  versuch = 1  #statemachine
  versuch=3  #Faber
  price=arg$clos
  
  if (versuch==1)    #ein vollbrauchbares system
    #geglätteter Kurs  
  {
    # browser()
    fast=as.integer(par$fast)
    zlem= bt.apply.matrix(arg$clos, ZLEMA, n=as.integer(par$zlemaN))
    Ma = runMax(arg$clos,n=as.integer(par$slow))
    Mi = runMin(arg$clos,n=as.integer(par$slow))
    
    #  signal = iif(zlem <= lag(Mi,30),-1,1)
    
    pos <<- 0
    
    #ein Signal-Geber der als statemachine funtioniert (pos ist der state)  
    gib.signal<-function(Y) #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    {
      letztes=shape(Y)
      YY = Y[,1]
      zlem=nval(Y[letztes,1]); Ma=nval(Y[letztes,2]);Mi=nval(Y[letztes,3]); altesMi=nval(Y[1,3]);altesMa=nval(Y[1,2])
      
      alpha=0; beta=0
      
      if (!is.na(first(YY))) #berechne die reg-gerade um altesMi noch anzuheben ...
      {
        x=cbind(Intercept=1,c(1:shape(YY))); y=coredata(YY); lmfit=lm.fit(x=x,y=y)
        beta=coef(lmfit)[2]    
        alpha =last(lmfit$fitted.values)- first(lmfit$fitted.values)  
      }
      if (is.na(altesMi) || is.na(Mi))
        return(pos)
      
      if (pos==1)
      {
        if (beta > 0.00)  #(altesMi-alpha)  nur wenn beta nicht sehr klein ist...
          ref.altesMi=altesMi-alpha
        else
          ref.altesMi = altesMi
        
        signal=ifelse(zlem <= ref.altesMi,0,1)
        
        #if (katastrophe(Y))    signal=0   #kostet viel rechenzeit ! und bringt nichts
      }
      else
      {
        signal =0
        
        if (pos==0)
          if (beta > 0)
            signal= ifelse(altesMi < Mi || altesMa <Ma  ,1,0)  #&& sign(beta) >=0
      }
      
      pos<<- signal
      return(signal)
      
    } #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # browser()
    
    signal=rollapplyr(merge(zlem,Ma,Mi), fast, gib.signal, by.column = F)
    return(list(Signal=signal, Indi=list(ma=merge(zlem,Ma,Mi)),rank=zlem))
  }
  
  #baue eine reihe technischer indikatoren (z.b. reg-Winkel, sma-dif, pos, maxdd, rsi, roc(5))
  #- trainire damit monatlich  ein glm  oder  pnn oder svm und hol dir vom klassifier das signal
  if (versuch==2)
  {
    lomi= runMin(Lo(na.omit(price)),n=20) #lag mäßig ok
    himi= runMax(Hi(na.omit(price)),n=20)
    browser()
    zlem= bt.apply.matrix(arg$clos, ZLEMA, n=as.integer(par$zlemaN))
    
    slow = bt.apply.matrix(arg$clos, SMA, n=as.integer(par$slow))
    fast = bt.apply.matrix(arg$clos, ZLEMA, n=as.integer(par$fast))
    
    #exit.signal = iif(zlem < slow & lag(zlem)>=slow , 0, NA)
    #entry.signal=iif(zlem >=fast , 1, exit.signal)
    
    #signal = m.ifna.prev(entry.signal)  #die letzte meinung
    
    Ma = runMax(arg$clos,n=as.integer(par$slow))
    Mi = runMin(arg$clos,n=as.integer(par$slow))
    MaMi = runMax(Mi,n=as.integer(par$fast))
    signal = iif(zlem < MaMi,-1,1)
    
    #return(list(Signal=signal, Indi=list(ma=merge(zlem,Ma,Mi,MaMi))))
    return(list(Signal=signal, Indi=list(ma=merge(zlem,slow,fast),beta=beta,alpha=alpha)))
  }
  if (versuch == 3)  #signal.Faber .. etwas anders implementiert
  {
    
    perf <- na.omit(merge(monthlyReturn(price)))
    colnames(perf) <- colnames(price)
    # mchart(mRendite(na.omit(perf)))
    
    
    #get cumulative returns for moving average
    cumul <- as.xts(apply(perf+1,MARGIN=2,cumprod),order.by=index(perf)) #mRendite(na.omit(perf)
    #  mchart(cumul)
    #do 10 month Mebane Faber style system
    ma <- lag(merge(runMean(cumul[,1],n=10)))#,runMean(cumul[,2],n=10)),k=1)
    #apply 50% allocation to each fund if they are > 10 month moving average
    ma.perf <- as.xts(apply(as.matrix(cumul>ma) * as.matrix(perf)/2,
                            MARGIN=1,sum),
                      order.by=index(perf))
    ############# krass
    signal_=as.xts( iif(coredata(cumul>ma),1,0),order.by=index(perf))
    #wandel die monats-daten aus signal_ wieder in tagesdaten nach signal
    signal=price;signal[]=NA
    signal[index(signal_)]=signal_
    signal[] =apply(coredata(signal), 2, m.ifna.prev)
    
    return(list(Signal=signal, Indi=list(sig=signal),rank=cumul-ma))
    
    if (F)
    {
      ma.each <- as.xts(as.matrix(cumul>ma) * as.matrix(perf),
                        order.by=index(perf))
      
      mchart(mRendite(na.omit(ma.each)))
      
      #######
      #get 8 month RSI; randomly picked 8; no optimization
      rsi<- lag(merge(RSI(perf[,1],n=8),RSI(perf[,2],n=8)),k=1)
      #allocate between vbmfx and vfinx based on highest RSI
      rsi.perf <- ifelse(rsi[,1]>rsi[,2],perf[,1],perf[,2])
      rsi.each <- as.xts(as.matrix(rsi>50) * as.matrix(perf),
                         order.by=index(perf))
      
      mchart(mRendite(na.omit(rsi.perf)))
      #####
      
      
      #signal<-merge(apply.rolling(ret[,1],FUN="Omega",width=25),apply.rolling(ret[,2],FUN="Omega",width=25))
      omega <- lag(rollapplyr(perf, width=6, FUN="Omega", by.column=T, align = "right"))
      omega[!is.finite(omega)]<-0
      
      #add omega as another allocation method
      #  omega <- lag(merge(apply.rolling(perf[,1],width=6,by=1,FUN=Omega),
      #                     apply.rolling(perf[,2],width=6,by=1,FUN=Omega)),
      #               k=1)
      #if omega >= 1 then apply 50% allocation
      omega.perf <- as.xts(apply(as.matrix(omega>=1) * as.matrix(perf)/2,
                                 MARGIN=1,sum),
                           order.by=index(perf))
      omega.each <- as.xts(as.matrix(omega>=1) * as.matrix(perf),
                           order.by=index(perf))
      
      mchart(mRendite(na.omit(omega.each)))
    }
  }
}
signal.MA.3<-cmpfun(signal.MA.3_) #compilier das Teil


if (F)
{
  prices=data.info(data)
  global_arg<<-list(clos=prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices))
  globalTrainLevel <<-10   
  global_objectId <<-paste("TREND","SG2R","signal.MA.1") 
  
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM ="DAX")
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=190),visual=T, TRAINSYM =-1)
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM ="SG2R")
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM =-1)
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM ="SXEBP")
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM ="REX")
  
  TrainIndicator( opti="GRID", indiName = "signal.MA.1",  visual=T, TRAINSYM = "DAX")
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
  ####################
  
  channelStop()
  
  techStops(data$DAX)
  
}

############################################

#######################################################################################
#Wenn sich der Down-Trailing-Stop ddma  mit dem Up-Trailing-Stop  ddmi kreuzt entsteht
#für viele Titel ein interessanter Schnittpunk für Entry-Exit...
#muss aber individuell trainiert werden
#######################################################################################

signal.drawDown<-function(arg, par = mlist(runMa=c(250,50,500,50),runMi=c(150,50,500,50)) ,visual=F,...)
{
  #browser()
  #par$zlemaN=11 #fix
  levelMi=0.5#,0.1,1,10))
  #levelMi=par$levelMi
  
  #zlem= bt.apply.matrix(mNorm(na.omit(arg$clos)), ZLEMA, n=as.integer(par$zlemaN))
  
  p=mNorm(na.omit(arg$clos))
  ret = na.omit(mROC(p))
  
  zlem=p
  ddma=bt.apply.matrix(zlem, runMax, n=as.integer(par$runMa));ddma=ddma-p
  ddmi=bt.apply.matrix(zlem, runMin, n=as.integer(par$runMi));ddmi=p-ddmi
  
  #plot(ddma)
  #browser()
  #ddma=iif(ddma< levelMi,0,ddma)
  #ddmi=iif(ddmi< levelMi,0,ddmi)
  
  signal=arg$clos; signal[]=NA
  signal[]=merge(signal[,1], iif(ddmi < ddma ,0,1))[,2]
  
  #browser()
  
  #zlemDD=merge(zlem,ddma,ddmi)
  #mchart(zlemDD)
  #  plotSigPrice(signal=na.omit(signal),prices=arg$clos,indi=list(zlemDD=merge(zlem,ddma,ddmi)))#merge(bb[,"dn"]
  
  return(list(Signal=signal, Indi=list(zlemDD=merge(zlem,ddma,ddmi))))
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (F)
{  
  global_ParTable=NULL
  global_arg = list(clos=data$prices, dat=data)
  
  #clos=prices[,2:2]
  
  res=signal.drawDown(global_arg)   # DOW: 10,150
  res=signal.drawDown(global_arg,list( k=10, zlemaN=150),visual=T)   # DOW: 10,150
  res=signal.drawDown(global_arg,list( k=76, zlemaN=2),visual=T)   # DOW: 10,150
  #das ganze mit chart und Auswertung
  
  x=indi.Generic("signal.drawDown", global_arg, visual=T, TRAINSYM="DAX")  
  
  prices=data.info(data)
  
  global_arg<<-list(clos=data.info(data),dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  #global_FastTrain=5
  #ich kann das Training gröber oder feiner gestalten indem ich die Variable
  #global_FastTrain > 0 setzte. dies ist dann die Anzahl der Zwischenschritte pro
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  
  globalTrainLevel <<- 5   
  i.system=indi.Generic("signal.drawDown", global_arg, par = list(runMa=200,runMi=200,levelMi=0.2) ,visual=T, TRAINSYM=-1)
  
  i.system=indi.Generic("signal.drawDown", global_arg, par = list(runMa=100,runMi=50,levelMi=0.2) ,visual=T, TRAINSYM=-1,do.assemble.Signals=F)  #jeder mit seinen eigenen BestParams
  
  i.system=indi.Generic("signal.drawDown", global_arg, par = list(runMa=100,runMi=50,levelMi=0.2) ,visual=T, TRAINSYM=-1,do.assemble.Signals=F)
  
  #und jetzt für alle Zeitreihen als gleichgewichtetes Portfolio
  x=indi.Generic("signal.drawDown", global_arg, visual=T, TRAINSYM="DAX")   #jeder mit gleichen BestParams
  global_FastTrain<<- -1
  TrainIndicator (opti="GRID",indiName = "signal.drawDown",visual=T,TRAINSYM="DAX")  
  
  x=indi.Generic("signal.drawDown", global_arg, visual=T, TRAINSYM="DAX")   #jeder mit gleichen BestParams
  
  
}

##############################################################################################
# hat viel potential:


signal.drawDown1<-function(arg, par = mlist(win=c(200,50,500,50)),visual=F,...)
{
  q.w=0.8
  w2=20
  p=zlem=mNorm(na.omit(arg$clos))
  
  if (par$win>=shape(p)|| w2 >=shape(p))
  {
    signal=p;signal[]=0
    return(list(Signal=signal, Indi=list()))
    
  }
  else
  {
    ddma=bt.apply.matrix(zlem, runMax, n=as.integer(par$win));ddma=ddma-p
    ddmi=bt.apply.matrix(zlem, runMin, n=as.integer(par$win));ddmi=p-ddmi
    
    ddmi=clean.matrix(ddmi); ddma=clean.matrix(ddma)
    
    ddmi=EMA(ddmi,w2);ddma = EMA(ddma,w2)
    t.indi = ddmi-ddma
    #hysterese - cut:
    #t.indi=SMA(sign(diff(indi,2)),n=20) #digital faber
    
    thresh=0
    # thresh=0.2
    #thresh=quantile(abs(t.indi),probs=seq(0,1,0.01),na.rm=T)["60%"]
    #thresh=rollapplyr(abs(t.indi),20, roll.quantile, allPrices=abs(t.indi),maxWin=q.w,Q="60%" )
    
    thresh= runquantile(abs(t.indi), k=300, probs=q.w,align="right")
    #hysterese schwelle 
    d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
    #damit nicht gleich das erste signal falsch ist
    d.signal[1] = sign(first(na.omit(diff(p,20))))
    #der auffüller ...
    sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
    if (has(global_arg$modell.par,"LongShort",what="T"))
      short=-1
    else
      short=0  #hier -1 schreiben wenn er auch short gehen darf
    sig[sig<0]=short
    
    
    signal=arg$clos; signal[]=NA
    signal[]=merge(signal[,1], iif(ddmi < ddma ,short,1))[,2]
    
    #browser()
    
    #zlemDD=merge(zlem,ddma,ddmi)
    #mchart(zlemDD)
    #  plotSigPrice(signal=na.omit(signal),prices=arg$clos,indi=list(zlemDD=merge(zlem,ddma,ddmi)))#merge(bb[,"dn"]
    #auswertung:   schnittstellen hervorheben ...hervorheben  
    cut=sig[(sig==1 & lag(sig)!=1) | (sig==short & lag(sig)!=short) ]
    CUT=p;CUT[]=0; CUT[as.Date(index(cut))]=1
    mP("long trades %d",nrow(cut))
  }
  #return(list(Signal=sig, Indi=list(sma=merge(p,indi),f=merge(t.indi,0,CUT,thresh,-thresh))))#,  
  return(list(Signal=signal, Indi=list(zlemDD=merge(zlem,ddma,ddmi),f=merge(t.indi,0,CUT,thresh,-thresh)),rank=t.indi))
}

if (F) 
{
  x=indi.Generic("signal.drawDown1", global_arg, par = list(win=150) ,visual=T, TRAINSYM=-1) 
  x$Tquality
}

######################## The BEST --------- SEHRGUTESMODELL ----- #################################

signal.Faber.dyn.hysterese <-function(arg, par = mlist(  sma.w=c(200,120,420,10)),visual=F,...)
{
  prices=arg$clos
  p=mNorm(arg$clos);sym=colnames(prices)
  q.w=0.8#as.integer(par$q.w)
  #indi =SMA(na.omit(p),n=as.integer(par$sma.w))
  frame=""
  
  print(sym)
  sma.w = as.integer(par$sma.w)
  if (sma.w >= shape(p))
  {
    indi=p;indi[]=0
  }
  else
    indi=SMA(p,sma.w) 
  indi[is.na(indi)]=SMA(p,20)
  sig = iif(p >= indi,1, 0)
  
  #hysterese - cut:
  t.indi=p-indi  #faber
  #t.indi=SMA(sign(diff(indi,2)),n=20) #digital faber
  
  thresh=0
  # thresh=0.2
  
  thresh= runquantile(abs(t.indi), k=70, probs=q.w,align="right")
  #hysterese schwelle 
  d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
  #damit nicht gleich das erste signal falsch ist
  d.signal[1] = sign(first(na.omit(diff(p,20))))
  #der auffüller ...
  sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
  if (has(global_arg$modell.par,"LongShort",what="T"))
    short=-1
  else
    short=0  #hier -1 schreiben wenn er auch short gehen darf
  sig[sig<0]=short
  
  #auswertung:   schnittstellen hervorheben ...hervorheben  
  cut=sig[(sig==1 & lag(sig)!=1) | (sig==short & lag(sig)!=short) ]
  CUT=p;CUT[]=0; CUT[as.Date(index(cut))]=1
  mP("long trades %d",nrow(cut))
  
  return(list(Signal=sig, Indi=list(sma=merge(p,indi),f=merge(t.indi,0,CUT,thresh,-thresh)),rank=t.indi))#,d=merge(sign(SMA(sign(diff(indi,3)),n=40))))))
}
if (F)
{
  x=indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,experiment="s")
  x$Tquality
}
############

########################################################
##schrott !!!
signal.stochastic <-function(arg, par = mlist(  sma.w=c(200,120,220,20)),visual=F,...)
{
  prices=arg$clos
  p=mNorm(arg$clos);sym=colnames(prices)
  q.w=0.6#as.integer(par$q.w)
  #indi =SMA(na.omit(p),n=as.integer(par$sma.w))
  frame=""
  
  print(sym)
  sma.w = as.integer(par$sma.w)
  if (sma.w >= shape(p))
  {
    indi=p;indi[]=0
  }
  else
  {
    # John Ehlers Stochastic
    indi=stoch = roofing.stochastic.indicator(prices,lookback=as.integer(par$sma.w))
    # 14 Day Stochastic
    #stoch14 = bt.apply(arg$dat, function(x) stoch(HLC(x),14)[,'slowD'])
  }
  #indi[is.na(indi)]=SMA(p,20)
  sig = iif(cross.up(indi, 0.2), 1, iif(cross.dn(indi, 0.8), 0, NA))
  sig = m.ifna.prev(sig)
  
  if (F)
  {
    #hysterese - cut:
    t.indi=p-indi  #faber
    #t.indi=SMA(sign(diff(indi,2)),n=20) #digital faber
    
    thresh=0
    # thresh=0.2
    
    thresh= runquantile(abs(t.indi), k=300, probs=q.w,align="right")
    #hysterese schwelle 
    d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
    #damit nicht gleich das erste signal falsch ist
    d.signal[1] = sign(first(na.omit(diff(p,20))))
    #der auffüller ...
    sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
    if (has(global_arg$modell.par,"LongShort",what="T"))
      short=-1
    else
      short=0  #hier -1 schreiben wenn er auch short gehen darf
    sig[sig<0]=short
    
    #auswertung:   schnittstellen hervorheben ...hervorheben  
    cut=sig[(sig==1 & lag(sig)!=1) | (sig==short & lag(sig)!=short) ]
    CUT=p;CUT[]=0; CUT[as.Date(index(cut))]=1
    mP("long trades %d",nrow(cut))
  } 
  return(list(Signal=sig, Indi=list(sma=merge(p,indi),stoch=indi)))#,d=merge(sign(SMA(sign(diff(indi,3)),n=40))))))
}
if (F)
{
  x=indi.Generic("signal.stochastic", global_arg, par=list(sma.w=60),visual=T, TRAINSYM =-1)
  x$Tquality
}
########################################################
##schrott !!!
signal.mom <-function(arg, par = mlist(  sma.w=c(200,-300,300,20)),visual=F,...)
{
  prices=arg$clos
  p=mNorm(arg$clos);sym=colnames(prices)
  q.w=0.6#as.integer(par$q.w)
  #indi =SMA(na.omit(p),n=as.integer(par$sma.w))
  frame=""
  
  sma.w = as.integer(par$sma.w)
  mP("signal.mom %s  %d  ## %s ## ",sym,sma.w,fromToS(p))
  
  if (sma.w >= shape(p))
  {
    indi=p;indi[]=0
  }
  else
  {
    # indi = momentum.averaged(prices=prices,lookbacks=sma.w)
    
    P=p
    #  q.w=0;P=EMA(p,50)  
    
    if (sma.w > 0)
    {
      mom.lookback.len = sma.w  
      indi=momentum = P / mlag(P, sma.w) - 1
      #sig = ifna(momentum > 0, F)
    }
    else
    {
      # momentum is averaged on 20,60,120,250 days using 3 day lag
      mom.array = c(60,90,120, -sma.w)  
      indi = momentum.averaged(P, mom.array, 3)
    }
    #avgmom.universe = ifna(avg.momentum > 0, F)
  }
  
  #hysterese - cut:
  t.indi = indi  #faber
  #t.indi=SMA(sign(diff(indi,2)),n=20) #digital faber
  
  thresh=0
  # thresh=0.2
  
  if (q.w !=0 )
    thresh= runquantile(abs(t.indi), k=300, probs=q.w,align="right")
  
  #hysterese schwelle 
  d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
  #damit nicht gleich das erste signal falsch ist
  d.signal[1] = sign(first(na.omit(diff(p,20))))
  #der auffüller ...
  sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
  if (has(global_arg$modell.par,"LongShort", what="T"))
    short=-1
  else
    short=0  #hier -1 schreiben wenn er auch short gehen darf
  
  
  sig[sig<0]=short
  
  #auswertung:   schnittstellen hervorheben ...hervorheben  
  cut=sig[(sig==1 & lag(sig)!=1) | (sig==short & lag(sig)!=short) ]
  CUT=p;CUT[]=0; CUT[as.Date(index(cut))]=1
  mP("long trades %d",nrow(cut))
  
  return(list(Signal=sig, Indi=list(sma=merge(p,P,indi),f=merge(t.indi,0,CUT,thresh,-thresh)),rank=t.indi))#,d=merge(sign(SMA(sign(diff(indi,3)),n=40))))))
}

#...................................................


if (F) #TRAIN and TEST
{
  
  x=indi.Generic("signal.mom", global_arg, par=list(sma.w=-200), visual=T, TRAINSYM =-1,experiment="mom.xxx")
  #data$BENCH
  
  global_ParTable <<-NULL
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.0005   #sys.Run() nimmt diese commission !!!!
  
  global_StartDate <<-  DateS(last(data$prices))
  
  TrainIndicator(global_StartDate_=global_StartDate,  opti="GRID", indiName = "signal.mom",  visual=T, TRAINSYM =-1)# data$BENCH)  "SXEBP"
  x$Tquality
  
  #portfolio
  x=indi.Generic("signal.mom", global_arg, par=list(sma.w=400), visual=F, TRAINSYM =-1,do.assemble.Signals=T)
  
  ###########################  nun rollierendes training:  roll.mode="years" 
  
  global_ParTable <<-NULL
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.0005   #sys.Run() nimmt diese commission !!!!
  
  global_StartDate <<-  DateS(last(data$prices))
  
  TrainIndicator( indiName = "signal.mom",  visual=T, TRAINSYM =-1)# data$BENCH)  "SXEBP"
  
  x=indi.Generic("signal.mom", global_arg, par=list(sma.w=-200), visual=F, TRAINSYM =-1,do.assemble.Signals=F)#,experiment=="mom")
  
  TrainIndicator( indiName = "signal.mom",  visual=T, TRAINSYM =-1, roll.mode="quarters")# data$BENCH)  "SXEBP"
  
  #portfolio
  x=indi.Generic("signal.mom", global_arg, par=list(sma.w=400), visual=F, TRAINSYM =-1,do.assemble.Signals=T,experiment=="mom")
  x=indi.Generic("signal.mom", global_arg, par=list(sma.w=-200), visual=F, TRAINSYM =-1,do.assemble.Signals=F,experiment="mom")
  
}
#####################################################################################

signal.sit.feilscher<-function(arg,par = mlist(lookback.len=c(60,40,400,20)),visual=F,...)
{
  #browser()
  clos=prices= arg$clos   #multivariat
  SYM=sym = colnames(prices)
  prices=global_arg$dat$prices
  lookback.len  = as.integer(par$lookback.len)
  safe = SAFE
  bench=sym
  data= arg$dat
  
  mP("signal.sit.feilscher %d %s %s",lookback.len,safe,bench)
  if (!safe %in% colnames(data$prices))
  {
    mP("safe %s not in data$prices",safe)
    print(colnames(data$prices))
    browser()
    return(NULL)
  }
  if (!bench %in% colnames(data$prices))
  {
    mP("bench %s not in data$prices",bench)
    print(colnames(data$prices))
    browser()
    return(NULL)
  }
  # browser()
  
  if (is.null(safe))
    sag("bug at signal.sit.feilscher- define SAFE !",warte=T)
  
  momentum = foreach(sym = c(bench,safe), .combine="merge") %do%
    rank.probMom(data$prices[,sym],xtra=lookback.len)
  
  signal = clean.matrix(momentum[,bench]) > clean.matrix(momentum[,safe]) #& momentum[,bench] >0
  if (bench==safe)
    t.indi=momentum[,bench] 
  else
    t.indi=clean.matrix(momentum[,bench]) - clean.matrix(momentum[,safe])
  
  #gib das signal zurück, und auch noch die Hilfsvariablen die Du evtl. in indi.Generic() sehen willst:
  
  signal[signal]= 1
  signal[!signal]=-1
  
  #t.indi= momentum[,bench] - momentum[,safe]
  #tsm = thresh.smooth(clos,t.indi,kwin=lookback.len/3,q.w=0.80)
  #thresh = tsm$thresh ;signal = tsm$Signal
  
  if (SYM==safe)
    signal[]=1
  
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  # return(list(Signal=signal, Indi=list(mom=momentum,ind=merge(t.indi,0,tsm$thresh,-tsm$thresh ))))
  return(list(Signal=signal, Indi=list(mom=momentum),rank=t.indi))#Indi=list(merge(t.indi,0,th$thresh,-th$thresh))))
}
if (F)
{
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals(); SAFE="BARCLAYS"
  #SAFE
  x=indi.Generic("signal.sit.feilscher", global_arg, par=list(lookback.len=60), visual=T, TRAINSYM =data$BENCH)
  
  x=indi.Generic("signal.sit.feilscher", global_arg, par=list(lookback.len=100), visual=T, TRAINSYM =-1)#,period="weeks")
  
  x=indi.Generic("signal.sit.feilscher", global_arg, par=list(lookback.len=100), visual=T, TRAINSYM =-1,period="weeks")
  
}
##########################################################
#http://www.tradesignalonline.com/de/lexicon/view.aspx?id=Relative+Strength+Levy+%28RSL%29

#signal.Price.itp <-function(arg, par = mlist( sma.w=c(140,-220,220,20)),visual=F,...)
#
signal.levy<-function(arg,par = mlist(win=c(240, -400,400,20)),visual=F,...) #150 europe
{
  #browser()
  clos=prices= arg$clos   #multivariat
  SYM=sym = colnames(prices)
  mode =sign(as.integer(par$win))
  win  = abs(as.integer(par$win))
  if (win==0) win=32
  
  mP("signal.levy %d ",win)
  # browser()
  
  t.indi=clos/SMA(clos,n=win)-1
  
  # t.indi= probAny(t.indi,lookback.len=win)-0.5
  
  signal=sign(t.indi)
  
  if (mode <0)
  {
    tsm = thresh.smooth(clos,t.indi,kwin=win*2,q.w=0.8)
    thresh = tsm$thresh ;signal = tsm$Signal
  }
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  if (mode <0)
    return(list(Signal=signal, Indi=list(ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi))
  else
    return(list(Signal=signal, Indi=list(levy=merge(t.indi,1)),rank=t.indi))#Indi=list(merge(t.indi,0,th$thresh,-th$thresh))))
}
if (F)
{
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals(); SAFE="BARCLAYS"
  global_arg$modell.par$LongShort <-   "F"
  #SAFE
  x=indi.Generic("signal.levy", global_arg, par=list(win=-27), visual=T, TRAINSYM =data$BENCH)
  
  x=indi.Generic("signal.levy", global_arg, par=list(win=-60), visual=T, TRAINSYM =-1)#,period="weeks")
  x=indi.Generic("signal.levy", global_arg, par=list(win=-300), visual=T, TRAINSYM =-1)#,period="weeks")
  
  x=indi.Generic("signal.levy", global_arg, par=list(win=-150), visual=T, TRAINSYM =-1)#,period="weeks")
  
  x=indi.Generic("signal.levy", global_arg, par=list(win=240), visual=T, TRAINSYM =-1)#
  
  global_ParTable <- NULL;  global_FastTrain <<- 5
  
  TrainIndicator( indiName = "signal.levy",  visual=T, TRAINSYM =0)# 
  
}
###########################################################
signal.mfaber<-function(arg,par = mlist(th=c(5,-9,9,1)),visual=F,...) #win=c(150, -400,400,20
{
  clos=prices= arg$clos   
  SYM=sym = colnames(prices)
  th = as.integer(par$th)
  mode =sign(th); th=abs(th)
  win  = 300# abs(as.integer(par$win))
  if (win==0) win=32
  
  mP("signal.mfaber %d ",win)
  
  t.indi=get.rank("rank.mfaber",prices=clos,xtra=win)-th
  
  signal=sign(t.indi)
  
  if (mode <0)
  {
    tsm = thresh.smooth(clos,t.indi,kwin=120,q.w=0.8)
    thresh = tsm$thresh ;signal = tsm$Signal
  }
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  if (mode <0)
    return(list(Signal=signal, Indi=list(ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi))
  else
    return(list(Signal=signal, Indi=list(levy=merge(t.indi,1)),rank=t.indi))#Indi=list(merge(t.indi,0,th$thresh,-th$thresh))))
}
if (F)
{
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals(); SAFE="BARCLAYS"
  global_arg$modell.par$LongShort <-   "F"
  #SAFE
  x=indi.Generic("signal.mfaber", global_arg, par=list(th=-3), visual=T, TRAINSYM =data$BENCH)
  
  
  global_ParTable <- NULL;  global_FastTrain <<- 50
  
  TrainIndicator( indiName = "signal.mfaber",  visual=T, TRAINSYM =0)# 
  
}
#########################################################
###########################################################
signal.HLrelation<-function(arg, par = mlist(maxdd=c(9,-21,21,2)),visual=F,...) #win=c(150, -400,400,20
{
  clos=prices= arg$clos   
  SYM=sym = colnames(prices)
  dd = as.integer(par$maxdd)
  mode =sign(dd); dd=abs(dd)
  win  = 300# abs(as.integer(par$win))
  
  
  mP("rank.HLrelation %d ",win)
  
  t.indi=get.rank("rank.HLrelation",prices=clos,xtra=dd)
  signal=sign(t.indi)
  
  if (mode <0)
  {
    tsm = thresh.smooth(clos,t.indi,kwin=120,q.w=0.8)
    thresh = tsm$thresh ;signal = tsm$Signal
  }
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  if (mode <0)
    return(list(Signal=signal, Indi=list(ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi))
  else
    return(list(Signal=signal, Indi=list(HLrel=merge(t.indi,1)),rank=t.indi))
}#...........................................

if (F)
{
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals(); SAFE="BARCLAYS"
  global_arg$modell.par$LongShort <-   "F"
  reset.cashed.rank("rank.HLrelation")
  
  #SAFE
  x=indi.Generic("signal.HLrelation", global_arg, par=list(maxdd=5), visual=T, TRAINSYM =data$BENCH)
  x=indi.Generic("signal.HLrelation", global_arg, par=list(maxdd=5), visual=T, TRAINSYM ="SXQBP")
  
  x=indi.Generic("signal.HLrelation", global_arg, par=list(maxdd=9), visual=T, TRAINSYM = -1)
  
  global_ParTable <- NULL;  global_FastTrain <<- 50  
  TrainIndicator( indiName = "signal.HLrelation",  visual=T, TRAINSYM =0)# 
  
}


############################################
signal.regimeSwitch<-function(arg,par = mlist( SDn=list(21),w1=list(252),w2=list(250),rsiT=list(50),volRank=list(0.5),wShort=list(90),wLong=list(200),rsiL=list(2)),visual=F,...)
{
  clos=arg$clos
  ret.log = ROC(arg$clos, type='continuous')
  hist.vol = runSD(ret.log, n = par$SDn)
  vol.rank = percent.rank(SMA(percent.rank(hist.vol, par$w1), par$SDn), par$w2)
  
  sma.short = SMA(arg$clos, n= round(par$wShort))
  sma.long= SMA(arg$clos, n= round(par$wLong))
  
  rsi2 = RSI(arg$clos, par$rsiL)  
  
  # Regime Switching  Historical  
  if (F)
    signal = iif(vol.rank > par$volRank, 
                 iif(rsi2 < par$rsiT, 1, -1),
                 iif(sma.short > sma.long, 1, -1)
    )
  signal =  thresh.smooth(clos,sma.short - sma.long)
  
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  #t.indi=r
  #tsm = thresh.smooth(clos,t.indi)
  #thresh = tsm$thresh
  #signal = tsm$Signal
  #gib das signal zurück, und auch noch die Hilfsvariablen die Du evtl. in indi.Generic() sehen willst:
  
  return(list(Signal=signal, Indi=list(ind=merge(sma.short,sma.long),rsi=rsi2)))#list(merge(t.indi,0,CUT,thresh,-thresh))))
}
if (F)
{
  load("stoxx_branchenpure"); data$universe="stoxxBra"
  define.Globals(); SAFE="BARCLAYS"
  #SAFE
  x=indi.Generic("signal.regimeSwitch", global_arg, par=list(SDn=21,w1=252,w2=250,rsiT=50,volRank=0.5,wShort=90,wLong=200,rsiL=2), visual=T, TRAINSYM =-1)
  
}

##########################################################################

lowess.forecast<-function(p,g=0.8)
{
  fit=try(lm.lowess(na.omit(p),visual=F,getIndex=T,main="lowess",glaettung=g))   
  
  w=10
  last.y=last(fit$y, w)
  res=(coredata(last(last.y))-coredata(first(last.y)) )/w  
}
#...................................................

signal.lowess <-function(arg, par = mlist( wlen=c(61,21,120,10),g=c(0.2,0.1,1,0.1)),visual=F,...)
{
  p=arg$clos
  wlen = as.integer(par$wlen)
  glaettung=as.integer(par$g)
  sym=colnames(p)
  
  #der forecast ist der  lm-winkel der letzten h  Tage bei glaettung g
  #- such nach dem optimalen triple:  wlne, h, g
  
  t.indi = rollapplyr( p, FUN="lowess.forecast",width=wlen, na.pad=T,g=glaettung)
  
  signal=sign(t.indi)
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  return(list(Signal=signal, Indi=list(t.indi=merge(t.indi,0)),rank=t.indi))
}  

if (F)
{
  tic() 
  x=indi.Generic("signal.lowess", global_arg, par=list(wlen=400,g=0.2),visual=T, TRAINSYM = data$BENCH)
  x=indi.Generic("signal.lowess", global_arg, par=list(wlen=400,g=0.1),visual=T, TRAINSYM = data$BENCH)
  toc()
  
  tic() 
  x=indi.Generic("signal.arima", global_arg, par=list(wlen=400),visual=T, TRAINSYM = data$BENCH)
  toc()
}

###################################################################################


butter.filter <-function(p, glatt=0.012)
{
  library(signal)
  #sehr nett und schnell: low-pass-filter auf finanz-daten - mit shift und ohne lag-korrektur
  bf <- butter(3, glatt)  #2.par: je kleiner desto glatter # 10 Hz low-pass filter
  x <- coredata(p)
  z <- signal::filter(bf, x) # apply filter  .. einfach den butter - filter-> lag
  res=xts(z,index(p))
}


signal.butter <-function(arg, par = mlist( b1=c(0.009,0.001,1,0.01),b2=c(0.015,0.001,1,0.01)),visual=F,...)
{
  clos=p=arg$clos[,1]
  sym=colnames(p)
  b1 = abs(nval(par$b1))
  b2 = nval(par$b2)
  mode=  nval(par$b1);  win= 50
  library(signal)
  #sehr nett und schnell: low-pass-filter auf finanz-daten - mit shift und ohne lag-korrektur
  bf <- butter(3, b1)  #2.par: je kleiner desto glatter # 10 Hz low-pass filter
  bf2 <- butter(3, b2)  #2.par: je kleiner desto glatter     # 10 Hz 
  # type = c("low", "high", "stop", "pass"),
  
  x <- coredata(p)
  t=1:shape(x)
  #  y <- filtfilt(bf, x)  #versucht das gefilterte signal ohne lag ...(2.pass)
  z1=z2=p
  z1[] <- signal::filter(bf, x) # apply filter  .. einfach den butter - filter-> lag
  z2[] <- signal::filter(bf2, x) # apply fi
  t.indi=z2-z1
  
  # z2 = SMA(z1,300)
  #t.indi=z1-z2
  
  signal=sign(t.indi)
  
  if (mode <0)
  {
    #tsm = thresh.smooth(clos,t.indi,kwin=win*2,q.w=0.8)
    #thresh = tsm$thresh ;signal = tsm$Signal
    
    tw=trendwechsel(p,z2,k=30,trigger.fn = "g.Signal.r",zero=0.5,visual=F,short=0)
    signal = tw$signal
    t.indi = tw$t.indi
    
  }
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  #if (mode <0)
  #  return(list(Signal=signal, Indi=list(ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi))
  #else
  return(list(Signal=signal, Indi=list(t.indi=merge(t.indi,0),but=merge(p,z1,z2)),rank=t.indi))
}  
if (F)
{
  tic() 
  global_arg$modell.par <- list(LongShort="F")
  x=indi.Generic("signal.butter", global_arg, par=list(b1=0.009,b2=0.015),visual=T, TRAINSYM = -1)
  toc()
}
###################################################################################
signal.robMed <-function(arg, par = mlist( b1=c(0.009,0.001,1,0.01),w=c(200,100,300,20)),visual=F,...)
{
  library(robfilter)
  clos=p=arg$clos[,1]
  sym=colnames(p)
  b1 = nval(par$b1)
  win=w = nval(par$w)
  mode=sign(b1); b1=abs(b1)
  
  library(signal)
  #sehr nett und schnell: low-pass-filter auf finanz-daten - mit shift und ohne lag-korrektur
  bf <- butter(3, b1)  #2.par: je kleiner desto glatter # 10 Hz low-pass filter
  # type = c("low", "high", "stop", "pass"),
  
  x <- coredata(p)
  t=1:shape(x)
  #  y <- filtfilt(bf, x)  #versucht das gefilterte signal ohne lag ...(2.pass)
  z1=z2=p
  z1[] <- signal::filter(bf, x) # apply filter  .. einfach den butter - filter-> lag
  z2[]=robreg.filter(ts(x),width=w,extrapolate=1,online=T,method="MED")$level$MED
  
  t.indi=z1-z2
  signal=sign(t.indi)
  
  if (mode <0)
  {
    tsm = thresh.smooth(clos,t.indi,kwin=win*2,q.w=0.8)
    thresh = tsm$thresh ;signal = tsm$Signal
  }
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  if (mode <0)
    return(list(Signal=signal, Indi=list(ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi))
  else
    return(list(Signal=signal, Indi=list(t.indi=merge(t.indi,0),but=merge(p,z1,z2)),rank=t.indi))
}  
if (F)
{
  tic() 
  global_arg$modell.par <- list(LongShort="F")
  set.glob()
  x=indi.Generic("signal.robMed", global_arg, par=list(b1=-0.030,w=300),visual=T, TRAINSYM = -1)
  toc()
}
######################################################################################

signal.zrollReg <-function(arg, par = mlist( w1=c(200,100,300,20),w2=c(200,100,300,20)),visual=F,...)
{
  p=arg$clos[,1]
  sym=colnames(p)
  w1 = win= as.integer(par$w1)
  w2 = as.integer(par$w2)
  mode=sign(w1);  w1=abs(w1)
  
  #z1 = EMA(p,w1)
  #z1= butter.filter(p,0.01)
  #z2 = rollRegressionXTS.smooth(p,w2) ;
  
  #smooth = ZLEMA(p,200); 
  #smooth=rollRegressionXTS.smooth(p,350) ;  
  #smooth=butter.filter(p,0.005); 
  #smooth=EMA(p,200)
  
  #t.indi=-(z1-z2)
  #signal=sign(t.indi)
  z1=z2=smooth=rollRegressionXTS.smooth(p,w1) 
  tw=trendwechsel(p,smooth,k=30,trigger.fn = "g.Signal.r",zero=0.5,visual=T,short=0)
  signal = tw$signal
  t.indi = tw$t.indi
  
  if (mode <0)
  {
    tsm = thresh.smooth(p,t.indi,kwin=win*2,q.w=0.8)
    thresh = tsm$thresh ;signal = tsm$Signal
  }
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  if (mode <0)
    return(list(Signal=signal, Indi=list(ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi))
  else
    return(list(Signal=signal, Indi=list(t.indi=merge(t.indi,0),but=merge(p,z1,z2)),rank=t.indi))
}  
if (F)
{
  tic() 
  global_arg$modell.par <- list(LongShort="F")
  set.glob()
  x=indi.Generic("signal.zrollReg", global_arg, par=list(w1=200,w2=350),visual=T, TRAINSYM = -1)
  
  toc()
}
##############################################
######################################################################################

signal.ZlemaDif <-function(arg, par = mlist( k=c(29,30,40), kd=c(2,2,40)),visual=F,...)
{
  prices = arg$clos
  today = as.Date(index(last(prices)))
  kd = round(par$kd)
  # minSig=par$minSig
  k=round(par$k)
  mode=sign(k);k=abs(k);win=50
  
  #cat ("#", toString(today),"\n  ZlemaDif: minSig", par$minSig," k ",par$k)
  mP(Title("ZlemaDif",arg,par))
  
  scdfast=score.diff(ZLEMA(prices,n=k),k=kd)  #XOFF = 2*k
  t.indi=scdfast[,1]
  scdfast[is.na(scdfast)] <-0
  # if (minSig==0)
  signal = iif( sign(scdfast[,1])>0 ,1,-1)
  #signal=sign(t.indi)
  
  if (mode <0)
  {
    tsm = thresh.smooth(p,t.indi,kwin=win*2,q.w=0.8)
    thresh = tsm$thresh ;signal = tsm$Signal
  }
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  if (mode <0)
    return(list(Signal=signal, Indi=list(ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi))
  else
    return(list(Signal=signal, Indi=list(t.indi=merge(t.indi,0),but=merge(p,t.indi)),rank=t.indi))
}  
if (F)
{
  tic() 
  global_arg$modell.par <- list(LongShort="F")
  set.glob()
  x=indi.Generic("signal.ZlemaDif", global_arg, par=list( k=200, kd=50),visual=T, TRAINSYM = -1)
  
  toc()
}
###############################################

##############################################
#M3


if (F) #MHOT
{
  p=data$prices[,data$BENCH]
  plot(p)
  smooth = ZLEMA(p,200);  lines(smooth,col="red")
  smooth=rollRegressionXTS.smooth(p,350) ;  lines(smooth,col="green")  
  smooth=butter.filter(p,0.005); 
  smooth=EMA(p,200)
  
  purePlot(p,smooth)
  
  colnames(data$prices)
  
  for (sym in c(1,2,3,4,15))
  {
    p=data$prices[,sym]
    #smooth=EMA(p,200)
    smooth=rollRegressionXTS.smooth(p,200) 
    tw=trendwechsel(p,smooth,k=30,trigger.fn = "g.Signal.r",zero=0.5,visual=T,short=0)
    
    sym="T_BOND_FT30"
    sym="EXX50_RI"
    sym="SUP500"
    
    tw$tw
    fromToS(tw$signal)
    fromToS(p)
    nrow(p)
    
    #b=na.omit(merge(p,tw$signal))
    #res=plotSigPrice(signal=b[,2],prices=b[,1],indi=list(smooth=smooth,merge(tw$t.indi,tw$thresh,-tw$thresh,0)))
    
    #res=plotSigPrice(signal=tw$signal,prices=p,indi=list(smooth=smooth))
    amark(index(tw$tw))
  }
  tw=trendwechsel(p,smooth,k=40,trigger.fn = "g.Signal.r",zero=0,visual=T)
}

######################################################################################
rollRegressionXTS.smooth.dyn<-function(p,win=60,probs=0.78,visual=T)
{
  win.long=win
  dolm0 <- function(Y){
    ret=last(fastLm(index(Y),coredata(Y))$fitted.values)
  }
  
  #berechne die cummulierte summe der quadratischen-abweichungen zur linReg 
  dolm0 <- function(p){
    #fit=fastLm(index(p),coredata(p))
    Y=p
    fit=lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y))
    
    resid=rev(fit$residuals)
    dq=last(cumsum(resid*resid))
    
  }
  
  #wenn die summe der abweichungsquadrate den schwellwert übersteigt wird eine neue
  #linreg im so verkürzten Fenster gerechnet
  dolm1 <- function(pq){
    #  if (DateS(last(pq))>=as.Date("2008-08-01"))
    #  {
    #    amark(DateS(last(pq)))
    #    browser()
    #  }
    p=pq[,1] #der kurs
    thresh=nval(last(pq[,2]))  #die schwelle - aus den quantilen
    #fit=fastLm(index(p),coredata(p))
    Y=p
    fit=lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y))
    m.1=coef(fit)[2]; sig=sign(m.1)
    resid=rev(fit$residuals)
    dq=cumsum(resid*resid)
    thresh.i=which(dq>thresh)
    
    if (len(thresh.i)>0 && thresh.i < nrow(pq))
    {
      win=thresh.i=thresh.i[1]
      if (visual)print(thresh.i)
      thresh.i = max(3, thresh.i)
      p.short=tail(p,thresh.i)
      # fit=fastLm(1:shape(p.short),coredata(p.short))
      Y=p.short   
      fit=lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y))
      m.2=coef(fit)[2]
      
      #nur wenn ein Gegenanstieg deutlich wird, flat gehen
      if (sign(m.2)==sign(m.1))
        sig=sign(m.1)
      else
        sig=0
    }
    
    # fitted.values=na.omit(fit$fitted.values)
    #  len1=len(fitted.values); m=(fitted.values[len1]-fitted.values[1])/len1;
    m=coef(fit)[2] 
    #purePlot(tail(p,len1),xts(fit$fitted.values,index(tail(p,len1))))
    
    ret = cbind(last(fit$fitted.values),win,m,sig)
  }
  
  #berechne die cummulierte summe der quadratischen-abweichungen zur linReg
  dret=rollapplyr(p, win, dolm0, by.column = F)
  #Berechnung passender thresholds
  q=runquantile(dret,k = win ,align="right",probs=probs)  #probs macht die entscheidende glaettung in Verbund mit win
  if(visual) mPlots(p,merge(dret,q))
  #berechnung der LinReg - diesmal mit verkürztem win-Fenster sobald die AbweichungsQuadrate zu groß werden.
  ret=rollapplyr( na.omit(merge(p,q)), win, dolm1, by.column = F)
  colnames(ret)=  spl(sprintf("%s.dyn.smoothed%d,win,m,sig",colnames(p),win))
  
  #mPlots(p,ret)
  if (visual)
  {
    mPlots(merge(p,ret[,1]),ret[,2],merge(ret[,3],0),main=colnames(ret)[1])
    purePlot(p,ret[,1]) ;#points(ret[,1])
  }
  return(ret)
}


if (F)
{
  sym=4
  p=data$prices[,sym]
  
  for (sym in c(1,2,3,4,15))
  {
    p=data$prices[,sym]
    mP("%d %s",sym,colnames(p))
  }
  #fast.smoothing
  dim(data$prices)
  for (sym in c(1,2,3,4,15))
  {
    p=data$prices[,sym]
    
    slope200=rollRegressionXTS.smooth.dyn(p,250,probs=0.7) 
    b=na.omit(merge(slope200$sig,p))
    res= plotSigPrice(signal=b[,1],b[,2],indi=list(smooth=merge(p)))
  }
  global_arg$clos=p
  
  signal.Faber.base(global_arg,dat=list(),visual=T)
  #fast.smoothing
  p=data$prices[,1]
  
  slope200=rollRegressionXTS.smooth.dyn(p,200,probs=0.2) 
  b=na.omit(merge(slope200$sig,p))
  plotSigPrice(signal=b[,1],b[,2])
  
  
  
  smooth200=rollRegressionXTS.smooth(p,100) 
  smooth200=rollRegressionXTS.smooth(p,200) 
  mchart(merge(p,smooth200,SMA(p,200),ZLEMA(p,200),EMA(p,50)))
  
  library(RcppArmadillo)
  fit=fastLm(index(p),p)
  
  dolm0 <- function(Y){ret=last(stats::lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y))$fitted.values)}
  
  
  
  win=250
  dolm1 <- function(Y){ret=last(fastLm(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y))$fitted.values)}
  
  ret=rollapplyr(p, win, dolm1, by.column = F)
  colnames(ret)=  sapply(colnames(p),function(x) sprintf("%s.smoothed%d",x,win))
  purePlot(p,ret)#xts(fit$fitted.values,index(p)))
  ls(fit)
}

#..................................................................................
######################################################################################
#arbeitet mit rollRegressionXTS.smooth.dyn
signal.smooth.dyn <-function(arg, par = mlist( win=c(250,100,400,100), probs=c(0.7,0.5,1,0.1)),visual=F,...)
{
  p = arg$clos[,1]
  win=as.integer(par$win)
  probs = nval(par$probs)
  mode=sign(win);win=abs(win)
  mP(Title("signal.smooth.dyn",arg,par))
  
  slope200=rollRegressionXTS.smooth.dyn(p,win=win,probs=probs,visual=F) 
  t.indi=slope200$m
  signal = xts2xts(lineal=p,slope200$sig)
  
  if (mode <0)
  {
    tsm = thresh.smooth(p,t.indi,kwin=win,q.w=0.8)
    thresh = tsm$thresh ;signal = tsm$Signal
    # t.indi= probAny(t.indi,lookback.len=win)-0.5
  }
  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  
  if (mode <0)
    return(list(Signal=signal, Indi=list(ind=merge(t.indi,0,tsm$thresh,-tsm$thresh )),rank=t.indi))
  else
    return(list(Signal=signal, Indi=list(t.indi=merge(t.indi,0),but=merge(p,t.indi)),rank=t.indi))
} 
#..........................................

if (F)
{
  tic() 
  global_arg$modell.par <- list(LongShort="F")
  set.glob()
  x=indi.Generic("signal.smooth.dyn", global_arg, par=list( win=100, probs=0.8),visual=T, TRAINSYM = -1)
  x=indi.Generic("signal.smooth.dyn", global_arg, par=list( win=100, probs=0.8),visual=T, TRAINSYM = "ESTX_CHEMICALS")
  x=indi.Generic("signal.smooth.dyn", global_arg, par=list( win=100, probs=0.9),visual=T, TRAINSYM = "ESTX_OILUGAS")  
  toc()
}
######################################################################################
signal.glaettung<-function(arg, par = mlist( glaettung=c(3,3,10,1)),visual=F,...)
{
  p=mNorm(arg$clos)+100
  itp <<- p; itp[]<<-NA
  m.smooth <<- p; m.smooth[]<<-NA
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(200))
  sma200[is.na(sma200)]<-0
  signal = p; signal[]=0
  glaettung=as.integer(par$glaettung)
  last.sig<<- 0
  new.sig <<-0
  
  #today.list=index(m.to.daily(p))
  today.list = index(m.to.weekly(p))
  
  #today.list = index(m.to.monthly(p))
  
  #foreach(yt = as.Date(index(m.to.monthly(p))) ,.combine="cbind",.errorhandling='stop') %do% {
  sapply(as.Date(today.list), FUN=function(yt){
    print("##############################")
    print(yt)
    RES=0
    #  browser()
    p.t = p[as.Date(index(p))<= as.Date(yt) ] #alles links von yt
    #finde den anfang des last.trend-fensters
    print(shape(p.t))
    if (shape(p.t)>500 )#&& as.Date(yt)>as.Date("2013-09-30"))
    {
      #if (yt== "2009-11-3")
      #  if (yt== "2009-11-27")
      #   browser()
      p.r=last(na.omit(p.t),1000)  #rollierendes Fenster
      last.trend=  find.last.trend.segment(na.omit(p.r),visual=F,glaettung=glaettung)  
      signal[yt]=last.trend$fs$sig[yt]
      new.sig <<-signal[yt]
      
      x.lt=last.trend$x2
      Y.lt=na.omit(p.r[sprintf("%s::%s",x.lt,yt)])
      
      #LongTerm-Trendkanal  
      slope.lt=lm.FITm(Y.lt,visual=F) #lin modell
      m.lt = round(coef(slope.lt)[2]*100,1)
      
      YP.lt=m.predict(slope.lt,firstDate=DateS(first(Y.lt)),n=shape(Y.lt)+1000,visual=F)
      #position im Trendkanal-band
      #  itp[yt]<<-  in.Trend.pos( p.r, YP.lt,yt)      
      
      if (coredata(new.sig) != coredata(last.sig))   
      {  
        #neuer segment- chart 
        no=find.last.trend.segment(na.omit(p.r),visual=F,glaettung=glaettung)  
        lines(YP.lt[,2],col="orange",lwd=2);lines(YP.lt[,3],col="orange",lwd=2)
        
      }
      #die steigung der inneren zeitreihe -  wenn deren abs(weniger wird - wirds schon flach !!)
      slope.smooth=lm.FITm(last.trend$fs$smooth ,visual=F) #lin modell
      m.smooth[yt] <<- round(coef(slope.smooth)[2]*100,5)
      
      last.sig <<- new.sig
      
      if (shape(Y.lt) <100)
        Y.lt= last(na.omit(p.r),100)
      
      #new.sig <<- sign(m.lt)
      # if (new.sig != signal[yt])
      #    browser(mP("NEW SIG"))
      
      signal[yt] <<- new.sig
    }
    RES=1
    return(RES)
  })
  #browser(mP("Chart"))
  #par.compute.Targets
  signal[signal==0]<-NA;signal=m.ifna.prev(signal)
  #plotSigPrice(signal=signal,prices=p)#,indi=list( m.smooth= m.smooth,itp=itp))
  return(list(Signal=signal, Indi=list()))# m.smooth= m.smooth,itp=itp)))
}

if (F)
{
  
  global_arg=list()
  global_arg$clos=na.omit(Cl(data$DAX["2004::2012"]))
  colnames(global_arg$clos)=c("DAX")
  fromTo(global_arg$clos)
  
  x=indi.Generic("signal.glaettung", global_arg, par=list(glaettung=2),visual=T, TRAINSYM =-1)#,safe="REX")
}
######################################################################################
#TRAIN_ALL1
Train_all_signal<-function(signal.Set = "",signalnames=spl("signal.Faber.i2"),SAFE="BARCLAYS",do.assemble.Signals=F,file = dataSet,LongShort="F",pdf=F,  visual=F,TRAINSYM=-1,fastMode=F)
{
  
  if (signal.Set !="")
  {
    wb <- XLConnect::loadWorkbook(sprintf("Models/signals.xls"))#, create = TRUE)
    Signals <<- data.table(wb["Signals"]); setkey(Signals,"Signal")
    
    signalnames =   trim(Signals$Signal[!is.na(Signals[[signal.Set]])])
    print("########################")
    print(signalnames)
    print("########################")
  }
  
  #  SAFE="BARCLAYS";do.assemble.Signals=F;file = "HuAEurope4";LongShort="F";pdf=F;  visual=F
  global_arg$modell.par <<- list(LongShort=LongShort)
  global_arg$clos <<- data$prices
  colnames(global_arg$clos)
  models= list()
  trained="_"
  File=file
  if (do.assemble.Signals)
    trained="_T"
  pdfFile=""
  
  
  
  file=sprintf("Models/%s/Results.xls/AllSig_%s%s%s.data",dataSet,File,trained,LongShort)
  dir.create(dirname(file),recursive=T)
  file2=sprintf("Models/%s/signals/AllSig_%s%s%s.data",dataSet,File,trained,LongShort)
  dir.create(dirname(file2),recursive=T)
  
  #set.glob()
  #Unterschiedliche, gute Trendfolge-modelle  - one Parameter - einheitlich für alle
  #................................................................. 
  
  
  for(signal.name in signalnames)
  {
    View(data.frame(signal=signal.name)) #kleine Anzeige, dass man weiss was er macht
    #"signal.ZlemaDif"
    TrainIndicator ( opti="GRID",indiName = signal.name,visual=T,TRAINSYM=-1)  
    
    #models[[signal.name]] = signal.mod #um zum schluss alle in einer datei zu speichern
    #file1=sprintf("Models/%s/signals/%s%s%s%s.data",dataSet,File,signal.name,trained,LongShort)
    
    # save(signal.mod,file=file1)
  }
  
  #mP("save models at %s",file);save(models,file=file)
  View(global_ParTable)
  
  models
  
}
######################################################################################
#RUN_ALL1
Run_all_signal<-function(data=data,signal.Set = "",signalnames=spl("signal.Faber.i2"),SAFE="BARCLAYS",do.assemble.Signals=F,file = dataSet,LongShort="F",pdf=F,  visual=F,TRAINSYM=-1,fastMode=F)
{
  global_arg$dat <<- data
  if (signal.Set !="")
  {
    wb <- XLConnect::loadWorkbook(sprintf("Models/signals.xls"))#, create = TRUE)
    Signals <<- data.table(wb["Signals"]); setkey(Signals,"Signal")
    
    signalnames =   trim(Signals$Signal[!is.na(Signals[[signal.Set]])])
    print("########################")
    print(signalnames)
    print("########################")
  }
  
  #  SAFE="BARCLAYS";do.assemble.Signals=F;file = "HuAEurope4";LongShort="F";pdf=F;  visual=F
  global_arg$modell.par <<- list(LongShort=LongShort)
  global_arg$clos <<- data$prices
  colnames(global_arg$clos)
  models= list()
  trained="_"
  File=file
  if (do.assemble.Signals)
    trained="_T"
  pdfFile=""
  
  file=sprintf("Models/%s/Results.xls/AllSig_%s%s%s.data",dataSet,File,trained,LongShort)
  dir.create(dirname(file),recursive=T)
  file2=sprintf("Models/%s/signals/AllSig_%s%s%s.data",dataSet,File,trained,LongShort)
  dir.create(dirname(file2),recursive=T)
  
  #set.glob()
  #Unterschiedliche, gute Trendfolge-modelle  - one Parameter - einheitlich für alle
  #................................................................. 
  
  if (pdf )
    pdfFile= sprintf("%s%s",trained,LongShort)
  

  
  if (len(global_arg$dat$macros)>0)
    merk=clone(global_arg$dat$prices)
  
  #.............................................................
  for(signal.name in signalnames)
  {
    View(data.frame(signal=signal.name)) #kleine Anzeige, dass man weiss was er macht
    
    signal.mod=indi.Generic(signal.name, global_arg,visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
    
    models[[signal.name]] = signal.mod #um zum schluss alle in einer datei zu speichern
  
    
    file1=sprintf("Models/%s/signals/%s%s%s%s.data",dataSet,File,signal.name,trained,LongShort)
    
    save(signal.mod,file=file1)
  }
  #.............................................................
  if (len(global_arg$dat$macros)>0)
    global_arg$dat$prices = clone(merk)
  
  
  mP("save models at %s",file);save(models,file=file)
  models
  
}
######################################################################################
#RUN_ALL
run_all_signal<-function(SAFE="BARCLAYS",do.assemble.Signals=F,file = dataSet,LongShort="F",pdf=F,  visual=F,TRAINSYM=-1,fastMode=F)
{
  #  SAFE="BARCLAYS";do.assemble.Signals=F;file = "HuAEurope4";LongShort="F";pdf=F;  visual=F
  global_arg$modell.par <<- list(LongShort=LongShort)
  global_arg$clos <<- data$prices
  colnames(global_arg$clos)
  models= list()
  trained="_"
  File=file
  if (do.assemble.Signals)
    trained="_T"
  pdfFile=""
  #dataSet="xxx"
File = dataSet
  file=sprintf("Models/%s/Results.xls/AllSig_%s%s%s.data",dataSet,File,trained,LongShort)
  dir.create(dirname(file),recursive=T)
  
  #set.glob()
  #Unterschiedliche, gute Trendfolge-modelle  - one Parameter - einheitlich für alle
  #................................................................. 
  
  if (pdf )
    pdfFile= sprintf("%s%s",trained,LongShort)
  
  
  
  models$signal.ZlemaDif=indi.Generic("signal.ZlemaDif", global_arg, par=list( k=200, kd=50),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  
  models$signal.zrollReg=indi.Generic("signal.zrollReg", global_arg, par=list(w1=200,w2=350),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  
  #reset.cashed.rank()  #wichtig fals in TSA.r bei #CACHEON  der cache aktiviert ist 
  models$sit.feilscher=indi.Generic("signal.sit.feilscher", global_arg, par=list(lookback.len=100), visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)#,period="weeks")
  
  
  models$signal.levy=indi.Generic("signal.levy", global_arg, par=list(win=150),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  mP("save models at %s",file); save(models,file=file) ; # return(models)
  
  #reset.cashed.rank()
  models$signal.lm= x2.1=indi.Generic("signal.lm", global_arg, par=list(win=100),visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  #indi.Results(x2.1)  #nett-MEurope0.65 - nicht so gut bei MWorld3
  
  #reset.cashed.rank()
  models$signal.mom = x5.1=indi.Generic("signal.mom", global_arg, par = list(sma.w=-200) ,visual=visual, TRAINSYM=TRAINSYM,safe=SAFE ,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile ,fastMode=fastMode)
  
  #reset.cashed.rank()
  models$signal.Faber.base=x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  #indi.Results(x)
  
  #reset.cashed.rank()
  models$signal.1.smoothing= x1=indi.Generic("signal.1.smoothing", global_arg, par=list(glaettung=200),visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)#,experiment="mWorld3faberS")
  #indi.Results(x1)#benchmark !!!!!
  
  
  #reset.cashed.rank()
  models$signal.any.smoothing = x1.1=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=200,glaettung2=0,dw=3),xarg=list(fn="SMA",fn2="",onlyLong=T,q.w=""),visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=F,pdfFile=pdfFile,fastMode=fastMode) #  do.assemble.Signals)
  #indi.Results(x1.1)  #nett,  auch gut bei MWorld3
  
  
  #reset.cashed.rank()
  #long-short bringt perf. - unten gibts mehr zu itp
  models$signal.Price.itp= x3.1 =indi.Generic("signal.Price.itp", global_arg, par=list(sma.w=140),visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  #indi.Results(x3.1)
  
  #reset.cashed.rank()
  models$signal.Faber.dyn.hysterese = x4.1= indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(sma.w=200),visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)#,experiment = "faberdyn")
  #indi.Results(x4.1)  #!! 
  
  #reset.cashed.rank()
  models$signal.drawDown1 = x5.1=indi.Generic("signal.drawDown1", global_arg, par = list(win=150) ,visual=visual, TRAINSYM=TRAINSYM,safe=SAFE ,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode )
  
  
  #indi.Results(x5.1)#sehr gut:MEurope0.73, sehr gut:MWorld3:0.9
  #Beispiel für ein sehr gutes 3 - Parameter-System
  #  merk= global_warte;  global_warte <<-F
  #reset.cashed.rank()  
  
  #benutzt gib.signal() ... eine statemachine .. scheinbar sehr gut
  models$signal.MA.3 = x6.1= indi.Generic("signal.MA.3", global_arg, par=list(zlemaN=10,slow=60,fast=10),visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)#,experiment="ma3" )
  
  #reset.cashed.rank()
  models$signal.days.since.high=indi.Generic("signal.days.since.high", global_arg, par=list(nHold=100,nHigh=200),visual=F, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)#data$BENCH)
  
  mP("save models at %s",file);save(models,file=file)
  
  #reset.cashed.rank()
  models$signal.Faber.i2=indi.Generic("signal.Faber.i2", global_arg, par=list(sma.w=300),visual=visual, TRAINSYM =TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  #reset.cashed.rank()  
  models$signal.rwf=indi.Generic("signal.rwf", global_arg, par=list(win=150),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,
                                 T.arg=list(cashed="signal.rwf!"),fastMode=fastMode)
  
  #reset.cashed.rank()
  models$signal.SMAzlema=indi.Generic("signal.SMAzlema", global_arg, par = list(smaN=150, zlemaN=150) ,visual=visual, TRAINSYM=TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)  #jeder mit seinen eigenen BestParams
  
  
  
  models$signal.zma=indi.Generic("signal.zma", global_arg, par=list(zwin=100,swin=200),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  
  
  mP("save models at %s",file);save(models,file=file)
  
  #..........................
  #very slow !!!
  
  #models$signal.lowess=indi.Generic("signal.lowess", global_arg, par=list(wlen=400,g=0.2),visual=visual, TRAINSYM = -1,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile)
  
  models$signal.butter=indi.Generic("signal.butter", global_arg, par=list(b1=0.009,b2=0.015),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  #bei b1 < 0 kommt der trenwechsel-alg zum einsatz - nicht der macd
  models$signal.butter2=indi.Generic("signal.butter", global_arg, par=list(b1= - 0.009,b2=0.015),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  models$signal.robMed=indi.Generic("signal.robMed", global_arg, par=list(b1=-0.030,w=300),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  
  #slow - many trades - very good
  models$signal.smooth.dyn=indi.Generic("signal.smooth.dyn", global_arg, par=list( win=100, probs=0.9),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  
  models$signal.attention=indi.Generic("signal.attention", global_arg, par = list( sma.w=200, diff=200,vers=1),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  
  # langsam aber interessant
  models$signal.glaettung=indi.Generic("signal.glaettung", global_arg, par=list(glaettung=2),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  mP("save models at %s",file); save(models,file=file) ;  return(models)
  #.............................................................................
  
  #very,very slow
  models$signal.multi.VAR=indi.Generic("signal.multi.VAR", global_arg, par=list(wlen=400),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  #nicht soo gut
  models$signal.glaettung=indi.Generic("signal.mfaber", global_arg, par=list(th=-7),visual=visual, TRAINSYM = TRAINSYM,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile,fastMode=fastMode)
  
  #nicht soo gut
  # model$signal.HLrelation=indi.Generic("signal.HLrelation", global_arg, par=list(maxdd=9), visual=visual, TRAINSYM = -1,safe=SAFE,do.assemble.Signals=do.assemble.Signals,pdfFile=pdfFile)
  
  
  #................................................................
  file=sprintf("Models/%s/Results.xls/%s%s%s.data",dataSet,File,trained,LongShort)
  mP("save models at %s",file);save(models,file=file)
  
  #  if (pdf )
  #    clr()
  #indi.Results(x6.1)  #!! 
  #ls(x6.1)
  
  #global_warte <<-merk
  
  # m=models.Results(models)  
  #  m.bad= rownames(m[ m["Tquality"]<0.9,,drop=F] )      
  
  
  models
  
} 
#.................................................................................

if (F)
{
  data=data=read.many.xls.all(modelDir="EM4_MAERZ_C",load.data=T)
  #eckhards y-daten einlesen (aus allen xls immer nur das y Target)
  data=data=read.many.xls.y(modelDir="EM4_MAERZ_C",universe = "EM4C_Y",bench="SUP500",safe="REX_RI")
  ls(data)
  no_strategy.Performance.snapshoot <<- T
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="test",LongShort="F",pdf=F,visual=F) 
  
}
if (F)
{
  data=universe_hua_World(reload=F)
  
  data=readRDS(,file=sprintf("%s.rds","Mworld3"))
  data.info(data)
  define.Globals(dataSet="Mworld3.1",bench="SX5R",SAFE="SHY")
  set.glob()
  
  ls(data)
  colnames(data$prices)
  data$SAFE = SAFE = "SHY"
  data$BENCH="SX5R"
  data$universe="Mworld3"
  
  data$Y.names = "SX5R"
  global_arg$clos = data$prices[,data$Y.names]
  dataSet = "sigTest"
  
  data=data=read.many.xls.y(modelDir="EM4_MAERZ_C",universe = "EM4C_Y",bench="STOXX_50",safe="STOXX_50")
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="test",LongShort="F",pdf=T,visual=T) 
  #modell 3 ist schrott - weil: wurde nicht trainiert ... 
  ls(data)
  SAFE="BARCLAYS"
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,pdf=T) #rechne alle timing modelle  
  load(file=sprintf("models/%s/HuA_F.data",dataSet))
  m=models.Results(models)  
  
  
  lapply(models,function(mod) { indi.Results(mod);browser() })
  
  ls(models$x$singleResults)
  
  plot(models$x$equity)
  eq=modelEQ(models)
  purePLot(eq)
  
  mchart(list2xts(models$x$singleResults))
  
  equities = model2equities(models)
  
  purePlot(equities)
  #purePlot(quality)
  #q2=bt.apply.matrix(quality, cutInterval,-2 ,2)
  
  
}
#  data.info(data)
#TEST

#######################################################################################
mP("########### load signal.R")
if (F)
  list_R_functions('MLib/signal.r')
