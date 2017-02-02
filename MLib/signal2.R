################################################################################################################
#Timing-Modelle
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

signal.Faber.base <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  
  signal = iif(p > sma200,1,0)
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}
if (F)
{
  ls(global_arg$dat)
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM ="DAX30")
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,experiment="Faber.base")
  
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
  Indi= res$indi
  
  return(list(Signal=signal, Indi=Indi))
}

if (F)
{
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=0,dw=0),xarg=list(fn="SMA",fn2="",onlyLong=T,q.w=""),visual=T, TRAINSYM ="DAX")
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
  
  x=any.smoothing(dax,glaettung = 300,dw=2,visual=T,fn="EMA",fn2="SMA",glaettung2=0,onlyLong=F)
  
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=10,dw=1),xarg=list(fn="EMA",fn2="",onlyLong=F,q.w="#1%"),visual=T, TRAINSYM =-1)
  
  
  x=any.smoothing(dax,glaettung = 200,dw=5,visual=T)
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM ="DAX")
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
  
  x=any.smoothing(dax,glaettung = 300,dw=0,visual=T,fn="SMA",fn2="",glaettung2=0,onlyLong=T)
  
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=0,dw=0),xarg=list(fn="SMA",fn2="",,onlyLong=T,q.w=""),visual=T, TRAINSYM ="DAX")
 
  dax=data$prices[,"DAX"]
  
  x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="ZLEMA",fn2="SMA",glaettung2=50,onlyLong=T,q.w="25%")
  
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=200,glaettung2=50,dw=1),xarg=list(fn="ZLEMA",fn2="SMA",onlyLong=T,q.w="25%"),  visual=T, TRAINSYM =-1)
  
  x=any.smoothing(dax,glaettung = 300,dw=1,visual=T,fn="EMA",fn2="SMA",glaettung2=0,onlyLong=T)
  
  x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="EMA",fn2="SMA",glaettung2=10,onlyLong=F)
  
  dax=dax["2003::"]
  
  
}
##########################################################################################################

signal.Faber.dyn.hysterese <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)  #, q.w=c(60,30,220,20))
{
  prices=arg$clos
  p=mNorm(arg$clos);sym=colnames(prices)
  #q.w=par$q.w/100
  q.w = 0.6 #fix
  sma.w= as.integer(par$sma.w)
  
  #indi =SMA(na.omit(p),n=as.integer(par$sma.w))
  frame=""
  
  print(sym)
  
  indi=SMA(p,sma.w) #200 
  indi[is.na(indi)]=SMA(p,20)
  sig = iif(p >= indi,1, 0)
  
  #hysterese - cut:
  t.indi=p-indi  #faber
  #t.indi=SMA(sign(diff(indi,2)),n=20) #digital faber
  
  thresh=0
  # thresh=0.2
  #thresh=quantile(abs(t.indi),probs=seq(0,1,0.01),na.rm=T)["60%"]
  #thresh=rollapplyr(abs(t.indi),20, roll.quantile, allPrices=abs(t.indi),maxWin=q.w,Q="60%" )
  
  thresh= runquantile(abs(t.indi), k=100, probs=q.w,align="right")
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
  x=indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(sma.w=150),visual=T, TRAINSYM =data$BENCH)
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
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p2,p))))#,fs=merge(p,fs,ema))))
}
if (F)  #sehr interessant - entscheident ist aber  sma.w=300 statt 200
  x=indi.Generic("signal.Faber.i2", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)


##########################################################################################################################
########################################################################################################################




#super
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
  x=indi.Generic("signal.HoltWinters", global_arg, par=list(mlen=1,sma.w=200),visual=T,TRAINSYM ="DAX")

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
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p),s1=signal.a)))
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
  dd=iif(abs(dd) < 0.1 & (abs(dr)< 0.1 & sign(sdr)==sign(lag(sdr))), NA,dd)   #daempfung
  #gefahr:     wenn beim triggerer-punkt die eine Zusatzbedingung nicht erfüllt ist bleibt wegen der daempfung in falscher pos haengen
  #dafuer brauchts dann das stop-system(unten)
  #dd=iif(abs(dd) < 0.1 & abs(sdr) < 0.1, NA,dd)
  
  plot(abs(dr))
  dd=m.ifna.prev(dd)
  
  #wenn weit genug vom letzten 
  sig=sign(dd)
  
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
x=indi.Generic("signal.Zlema.f1", global_arg, par=list(sma.w=600),visual=T, TRAINSYM =-1)

}