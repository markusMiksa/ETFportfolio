
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
options(error = browser)

##############################################################################################
#technischer signal-generator
#wie fast.smoothing - nur kann hier die Glaettungs-Funktion ?bergeben werden
#entscheidungen k?nnen wahlweise getroffen werden 
#mit und ohne hysterese  (wahlweise statisch mit vorgestelltem # in q.w - oder rollierender schwellenermittlung)
#als cut (dw < 0) oder als richtungs-?nderung (dw >0)  - 
#als macd  eines gegl?etteten mit dem zus?tzlich gegl?tteten dw <0
#oder als macd eines geglaetteten mit einem andere geglaetteten

#vorsicht:  runquantile-ergebnisse m?ssen immer um wlen/2 ge lag d werden
##############################################################################################
any.smoothing<-function(p,glaettung = 200,dw=1,visual=F,p0=NULL,roll=F,fn="EMA",fn2="SMA",glaettung2=40,onlyLong=T,q.w="")#q.w="#60%" oder q.w = "60%"
{
  
  mP("any.smoothing")
  #...... smoothing
  #aus dem r-cookbook
  p=na.omit(p[,1])
  
  t=c(1:nrow(p))
  fn=match.fun(fn)
  smooth <-fn(p,glaettung)
  #smooth <-ZLEMA(smooth,10)
  s=p;s[]=smooth
  
  
  #s2=SMA(s,40)  #falle es zu viele signale gibt, kann man mit fn2 auch noch mal eine zweite Glaettung durchf?hren
  s2=s
  #s[is.na(s)]<-first(na.omit(s))
  
  if (dw >0)  #kr?mmung der zweifach gegl?tteten
  {
    if (fn2 !="" && glaettung2 >= 1)
    { 
      fn2=match.fun(fn2)
      S2 <-fn2(na.omit(s),glaettung2)
      s2=merge(s,S2)[,2]
      #browser(mP("#2"))
      #s2[]=S2
      
      #s2[is.na(s2)]<-first(na.omit(s2))
    }
    else  
      s2=s
    decide = mROC(s2,n=dw,type="continuous")
    
    sig=sign(decide)#einfach die tagesdiff der glatten kurve
  }
  else
    if (dw <0) #schnittpunkt zwischen der einfach und der zweifach gel?tteten
    {
      if (fn2 !="" && glaettung2 >= 1)
      { 
        fn2=match.fun(fn2)
        S2 <-fn2(na.omit(s),glaettung2)
        s2=merge(s,S2)[,2]
        #s2[]=S2
        #s2[is.na(s2)]<-first(na.omit(s2))
      }
      else  s2=s
      #sig=sign(ROC(s2,n=dw,type="continuous"))#einfach die tagesdiff der glatten kurve
      decide = s - s2
      sig = sign(decide)
    }
  
  else #faber/macd: schnittpunkt mit preise oder einfach glatt1 mit glatt2
  {
    if (fn2 !="" && glaettung2 >= 1)
    {
      fn2=match.fun(fn2)
      S2 <-fn2(na.omit(p),glaettung2)
      s2=merge(s,S2)[,2]
      #s2=s
      #s2[]=S2
      #s2[is.na(s2)]<-first(na.omit(s2))
    }
    else 
      s2=p
    
    decide = s2 - s
    sig = sign(decide)
  }
  thresh=0
  
  offset.D=DateS(first(sig[!is.na(sig)]))
  
  if (q.w !="")  #hysterese - entscheid   #wenn vor q.w ein # wird das quantile statisch ermittelt
  {
    t.indi = decide
    # thresh=0.2
    #q.w="#60%"
    if (substr(q.w,1,1)=="#")
    {
      q.w=substr(q.w,2,10)
      thresh=quantile(abs(t.indi),probs=seq(0,1,0.01),na.rm=T)[q.w] #"60%"
    }
    else
    {
      #thresh.old=rollapplyr(abs(t.indi),20, roll.quantile, allPrices=abs(t.indi),maxWin=60,Q=q.w )
      
      # browser(mP("teste speedup von roll.quantile"))
      library(caTools)
      
      thresh=t.indi;
      S=seq(0,1,0.01)
      s.s=sapply(S,function(x)sprintf("%.0f%s",100*x,'%'))#%02.0f%s
      y= runquantile(coredata(abs(t.indi)), 60, probs=S,  align = "right")
      spalte=which(s.s==q.w)
      if (!len(spalte))
        sag("Mist - schreib q.w anders",T)
      #endrule=c("quantile", "NA", "trim", "keep", "constant", "func"),
      thresh[]=y[,1,spalte]
      #browser()      
      #plot(abs(t.indi))
      #plot(thresh)
      #lines(thresh.old,col="red")
      #plot(thresh2)
    }
    
    #hysterese schwelle 
    d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
    #damit nicht gleich das erste signal falsch ist
    d.signal[1] = sign(first(na.omit(diff(p,20))))
    #der auff?ller ...
    #browser(mP("head sig"))
    #head(sig)
    #bug 
    offset.D=DateS(first(sig[!is.na(sig)]))
    
    sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
    sig[sprintf("::%s",offset.D)]<-NA   #stell den offset wieder auf NA 
  }
  
  #  sig = -sig.cut
  #browser()
  #amark (as.Date(index(sig[sig.cut != sig])))
  
  #View(merge(p,s,ROC(s,n=dw,type="continuous"),sign(ROC(s,n=dw,type="continuous")))["2005"])
  #sig[is.na(sig)] <- first(na.omit(sig))
  if (onlyLong)
    sig[sig <0]=0
  ##########################################################################
  
  
  sd = sig#sign(diff(sig))
  rS=runSum(na.omit(sd),n=2)
  #rS=lag(rS,-1)
  peaks <- p[ as.Date(index(rS[rS==0]))]
  
  if (len(peaks) < 2)
  {highx=p[1];lowx=p[1];}
  else
  {
    print(".")
    highx = peaks[peaks >= lag(peaks,1)]
    lowx = peaks[peaks < lag(peaks,1)]
    
    f=""
    if (F && visual) #zus?tzlicher Hilfsplot
    {
      plot(p[f])
      points(peaks[f])
      points(highx,col="blue",lwd=3)
      points(lowx,col="red",lwd=3)
    }
    
    highx=as.Date(index(highx))
    lowx=as.Date(index(lowx))
    peaks=as.Date(index(peaks))
    
  }  
  ##f?r eine visualisierung
  #
  #   browser()
  Indi=list(f=merge(p,s,s2),d=merge(decide,0,thresh,-thresh))#,diff=merge(mROC(s2,7),mROC(s,7)))
  ret=NA
  if (visual)
  {
    if(F)#zus?tzlicher Hilfsplot
    {
      plot(p);lines(s,col="red")
      lines(scaleTo(sig,range(p)),col="green")
      amark(peaks)
    }
    frame=""
    
    if (is.null(p0))
      ret= plotSigPrice(signal = sig[frame],prices=p[frame],indi=Indi)
    else
      ret= plotSigPrnice(signal = sig[frame],prices=p[frame],indi=list(f=merge(p,ZLEMA(p,20),s, scaleTo(p0,range(na.omit(p))))))
  }
  
  if (nrow(s)!=nrow(p))
    browser(mP("bug"))
  
  if (roll)
    return(last(s))
  
  
  return(list(smooth = s,highs =lowx,lows = highx,hl=peaks ,sig=sig,indi=Indi,sys=ret))
  
}

if (F)
{
  
  sym="BUND_FUTURE"
  te= lapply(colnames(prices),function(sym)
  {
    dax=prices[,sym]
    #geil:
    x=any.smoothing(dax,glaettung = 200,dw=5,visual=T,q.w ="1%")
    x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="ZLEMA",fn2="SMA",glaettung2=10,onlyLong=T,q.w="1%")
    
  }
  )
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM ="DAX")
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
  
  x=any.smoothing(dax,glaettung = 300,dw=0,visual=T,fn="SMA",fn2="",glaettung2=0,onlyLong=T)
  
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=0,dw=0),xarg=list(fn="SMA",fn2="",,onlyLong=T,q.w=""),visual=T, TRAINSYM ="DAX")
  
  
  
  x=any.smoothing(dax,glaettung = 200,dw=5,visual=T,q.w ="1%")
  
  x=any.smoothing(dax,glaettung = 100,dw=5,fn="SMA",fn2="ZLEMA",visual=T,q.w ="5%")
  
  x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="ZLEMA",fn2="SMA",glaettung2=10,onlyLong=T,q.w="1%")
  x=any.smoothing(dax,glaettung = 300,dw=1,visual=T,fn="EMA",fn2="SMA",glaettung2=0,onlyLong=T)
  
  x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="EMA",fn2="SMA",glaettung2=10,onlyLong=F)
  
  dax=dax["2003::"]
}


##############################################################################
##############################################################################

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
  global_arg$clos=p
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=0,dw=0),xarg=list(fn="SMA",fn2="",onlyLong=T,q.w=""),visual=T, TRAINSYM ="DAX")
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
  
  x=any.smoothing(dax,glaettung = 300,dw=2,visual=T,fn="EMA",fn2="SMA",glaettung2=0,onlyLong=F)
  
  x=indi.Generic("signal.any.smoothing", global_arg, par=list(glaettung=300,glaettung2=10,dw=1),xarg=list(fn="EMA",fn2="",onlyLong=F,q.w="#1%"),visual=T, TRAINSYM =-1)
}
##############################################################################
##############################################################################
m.write.xls<-function(outData,xlsName="modelData.xls",sheetname="Ergebnisse")
{
  library(XLConnect)
  wb <- XLConnect::loadWorkbook(xlsName, create = TRUE)
  XLConnect::createSheet(wb, name = sheetname)
  ergebnisse = data.frame(outData)
  rownames(ergebnisse)=c(1:nrow(ergebnisse))
  
  XLConnect::writeWorksheet(wb, ergebnisse, sheet = sheetname,rownames="Number")
  XLConnect::saveWorkbook(wb)
}
if (F)
  m.write.xls(normP,"test2.xls","sheet")

#########################################################################


lm.FIT_<-function(Y) {x=as.matrix(c(1:shape(Y))); y=coredata(as.numeric(Y)); res=coef(lm(y~x))}
lm.FIT<-cmpfun(lm.FIT_)

if (F)
{
  slope200=lm.FIT(last(na.omit(p),200))[1]
  slope90=rollapplyr(p, 200, lm.FIT, by.column = F)
}




#Gewichtung gem. Abstand 
lm.FITmw_old<-function(Y) {x=c(1:shape(Y)); w = x/last(x); w=expm1(10*w); x=as.matrix(x); 
                           y=coredata(as.numeric(Y)); res=lm(y~x,weights=w)}

#################################################################################
#filter in dax alle MonatsEnden  -
#wenn ein monatsende auf ein Wochenende f?llt nimm den letzten arbeitstag.
#################################################################################
m.to.monthly<-function(dax)
{
  DateVector=as.Date(index(dax))
  head(DateVector)
  dm=Dates.month.ends(DateVector)
  
  #Problemf?lle wo monatsenden auf einem Wochenende liegen - das Monatsende darf aber nicht einfach fehlen - statt dessen muss dann der letzte businessday des Monats erscheinen 
  we=dm[is.weekend(dm)]
  #alle Wochentage
  bd=DateVector[!is.weekend(DateVector)]
  datePreWE=
    sapply(we,function(x) {preWE=bd[as.Date(bd)<as.Date(x)];dax.preWE=DateS(last(dax[preWE]))})
  #alle monatsenden die nicht auf ein wochenende fallen
  dm=dm[!is.weekend(dm)]
  #f?r die monatsenden die auf ein Wochenende fallen werden die Vorg?nger businessdays hinzugef?gt
  all.dm = sort(as.Date(append(as.character(dm),as.character(datePreWE))))
  dax[all.dm]
}

m.to.daily<-function(dax)
{
  dm=as.Date(index(dax))
  return(dax[!is.weekend(dm)])
}


m.to.weekly<-function(dax)
{
  DateVector=as.Date(index(dax))
  head(DateVector)
  dm=Dates.week.ends(DateVector)
  
  #Problemf?lle wo monatsenden auf einem Wochenende liegen - das Monatsende darf aber nicht einfach fehlen - statt dessen muss dann der letzte businessday des Monats erscheinen 
  we=dm[is.weekend(dm)]
  #alle Wochentage
  bd=DateVector[!is.weekend(DateVector)]
  datePreWE=
    sapply(we,function(x) {preWE=bd[as.Date(bd)<as.Date(x)];dax.preWE=DateS(last(dax[preWE]))})
  #alle monatsenden die nicht auf ein wochenende fallen
  dm=dm[!is.weekend(dm)]
  #f?r die monatsenden die auf ein Wochenende fallen werden die Vorg?nger businessdays hinzugef?gt
  all.dm = sort(as.Date(append(as.character(dm),as.character(datePreWE))))
  dax[all.dm]
}

m.to.quarterly<-function(dax)
{
  DateVector=as.Date(index(dax))
  head(DateVector)
  dm=Dates.quarter.ends(DateVector)
  
  #Problemf?lle wo monatsenden auf einem Wochenende liegen - das Monatsende darf aber nicht einfach fehlen - statt dessen muss dann der letzte businessday des Monats erscheinen 
  we=dm[is.weekend(dm)]
  #alle Wochentage
  bd=DateVector[!is.weekend(DateVector)]
  datePreWE=
    sapply(we,function(x) {preWE=bd[as.Date(bd)<as.Date(x)];dax.preWE=DateS(last(dax[preWE]))})
  #alle monatsenden die nicht auf ein wochenende fallen
  dm=dm[!is.weekend(dm)]
  #f?r die monatsenden die auf ein Wochenende fallen werden die Vorg?nger businessdays hinzugef?gt
  all.dm = sort(as.Date(append(as.character(dm),as.character(datePreWE))))
  dax[all.dm]
}

m.to.yearly<-function(dax)
{
  DateVector=as.Date(index(dax))
  head(DateVector)
  dm=Dates.year.ends(DateVector)
  
  #Problemf?lle wo monatsenden auf einem Wochenende liegen - das Monatsende darf aber nicht einfach fehlen - statt dessen muss dann der letzte businessday des Monats erscheinen 
  we=dm[is.weekend(dm)]
  #alle Wochentage
  bd=DateVector[!is.weekend(DateVector)]
  datePreWE=
    sapply(we,function(x) {preWE=bd[as.Date(bd)<as.Date(x)];dax.preWE=DateS(last(dax[preWE]))})
  #alle monatsenden die nicht auf ein wochenende fallen
  dm=dm[!is.weekend(dm)]
  #f?r die monatsenden die auf ein Wochenende fallen werden die Vorg?nger businessdays hinzugef?gt
  all.dm = sort(as.Date(append(as.character(dm),as.character(datePreWE))))
  dax[all.dm]
}
####################################################################
#gewichtete lineare regression - mit optionalem channel (wenn level ==0 ), und prognose (nplus Tage)
#w.method
#wenn l?cken in den Tagen sind  
#dist.by.date:   jeder Tag erh?ht die distanz - (im Sinne get.Index)
#dist.by.pos:    zwischen zwei items liegt immer die distanz 1 -egal wieviel tage dazwischen liegen
#w.range -- je h?her desto mehr alte Werte werden ber?cksichtigt,  bei Werte kleiner 1 wird die nachbarschaft sehr
#eng..
####################################################################
lm.FITmw_<-function(p,visual=F,getIndex=T,level=0.95,nplus=50, w.method="dist.by.date",w.range=3) #"dist.by.pos"
{
  p.org=p
  firstDate=DateS(first(p))
  
  
  if (!getIndex)
    xdat=x=1:shape(p) 
  else
  {
    
    xdat=as.Date(as.Date(index(first(p.org))):as.Date(index(last(p.org))))
    xdat.Lineal=xts(1:len(xdat), xdat)
    
    xdat= get.Index(xdat.Lineal, as.Date(index(p.org)),lineal=T)
    x=xdat
    
  }
  
  #x=c(1:last(x))
  #y1=expm1(x/last(x)*10);y1=y1/max(y1);plot(y1)
  if (w.method=="dist.by.date")
    w = x/last(x) #dist.by.date 
  else
    w=(x/ (rev(c(1:len(x)))))/last(x)  #dist.by.pos
  
  y1=expm1(w/last(w)/(w.range));y1=y1/max(y1);   #abkling-funktion
  w=y1
  
  if (visual)
  {plot(w,main="weights")
   browser(mP("lm.Fitmw"))
  }
  
  train <- data.frame(x=x, y=coredata(p.org))  
  colnames(train) = c("x","y")
  mod= lm(y ~ x, train,weights=w)
  
  if (!visual && level==0) return(mod)
  
  n=nval(last(xdat))
  nplus=n+nplus
  
  x=predict(mod,newdata=data.frame(x=1:nplus),interval="prediction",level=level)#,level=c(80,90))
  x=  bt.apply.matrix(x,function(x) iif(x<0,0,x))
  
  pre = xts(1:nplus, as.Date(firstDate)+0:(nplus-1));  
  #pre = head(pre[!is.weekend(time(pre))],n)
  
  pre=merge(pre,pre,pre)
  pre[]=x
  mod$channel=pre
  if (!visual) return(mod)
  
  mchart(merge(pre,p.org));points(p.org)
  #browser(mP("lm.Fitm"))  
  
  return(mod)
  
  mod
  
}
lm.FITmw<-cmpfun(lm.FITmw_)

#####################################################################
#lineare regression - mit optionalem channel (wenn level ==0 ), und prognose (nplus Tage)
####################################################################
lm.FITm_<-function(p,visual=F,getIndex=T,level=.95,nplus=0)
{
  p.org=p  #p.org enth?lt keine Wochenenden, somit 604 tage
  firstDate=DateS(first(p))
  p=(as.numeric(p))
  # browser(mP("lm.FITm"))
  
  if (!getIndex || !is.xts(p.org))
  {
    train <- data.frame(x=1:shape(p), y=coredata(p))  
    
    colnames(train) = c("x","y")
    
    mod = lm(y~x, data=train)
    if (!visual && level==0) return(mod)
    
    n=shape(p.org)
    x=predict(mod,newdata=data.frame(x=1:n),interval="prediction",level=level)#,level=c(80,90))
    x=  bt.apply.matrix(x,function(x) iif(x<0,0,x))
    
    nplus=n+nplus
    pre = xts(1:nplus, as.Date(firstDate)+0:(nplus-1));  
    #pre = head(pre[!is.weekend(time(pre))],n)
    
    pre=merge(pre,pre,pre)
    pre[]=x
    mod$channel=pre
    
    if (!visual) return(mod)
    
    mchart(merge(pre,p.org)) ;points(p.org)
    # browser(mP("lm.Fitm"))  
    
    
    return( mod)
  }
  else
  { 
    # browser(mP("lm.Fitm"))
    #if (F)
    #   lm.FITm(upper,getIndex=T)
    #hier liegen die x nicht dicht .. also muss man spreizen
    xdat=as.Date(as.Date(index(first(p.org))):as.Date(index(last(p.org))))
    xdat.Lineal=xts(1:len(xdat), xdat)
    
    xdat= get.Index(xdat.Lineal, as.Date(index(p.org)),lineal=T)
    train <- data.frame(x=xdat, y=coredata(p))
    colnames(train) = c("x","y")
    
    mod = lm(y~x, data=train)
    if (!visual && level==0) return(mod)
    
    n=nval(last(xdat))
    nplus=n+nplus
    
    x=predict(mod,newdata=data.frame(x=1:nplus),interval="prediction",level=level)#,level=c(80,90))
    x=  bt.apply.matrix(x,function(x) iif(x<0,0,x))
    
    pre = xts(1:nplus, as.Date(firstDate)+0:(nplus-1));  
    #pre = head(pre[!is.weekend(time(pre))],n)
    
    pre=merge(pre,pre,pre)
    pre[]=x
    mod$channel=pre
    if (!visual) return(mod)
    
    mchart(merge(pre,p.org));points(p.org)
    #browser(mP("lm.Fitm"))  
    
    return(mod)
    
  }
  
}
lm.FITm<-cmpfun(lm.FITm_) #compilier das Teil

############################################################################
#lowess regression - mit optionalem channel (wenn level ==0 ), und prognose (nplus Tage)
#lowess kann keinen channel
############################################################################
lm.lowess_<-function(p,visual=F,getIndex=T,main="lowess",glaettung=0)
{
  p.org=p  #p.org enth?lt keine Wochenenden, somit 604 tage
  firstDate=DateS(first(p))
  p=(as.numeric(p))
  
  if (!getIndex)
    train <- data.frame(x=1:shape(p), y=coredata(p))  
  else
  { 
    # browser(mP("lm.Fitm"))
    #if (F)
    #   lm.FITm(upper,getIndex=T)
    #hier liegen die x nicht dicht .. also muss man spreizen
    xdat=as.Date(as.Date(index(first(p.org))):as.Date(index(last(p.org))))
    xdat.Lineal=xts(1:len(xdat), xdat)
    xdat= get.Index(xdat.Lineal, as.Date(index(p.org)),lineal=T)
    train <- data.frame(x=xdat, y=coredata(p))  
    
  }
  colnames(train) = c("x","y")
  if (glaettung==0)
    lowe=lowess(y~x,data=train) # und damit dann die Quantile machen ...
  else
    lowe=lowess(y~x,data=train,f=glaettung) # und damit dann die Quantile machen ...
  
  if (visual)
  {
    p.o=p.org; p.o[]=lowe$y
    plot(p.org,main=main); 
    points(p.org,col="blue")    
    lines(p.o,col="red",lwd=2)
  }
  return(lowe)
}
lm.lowess<-cmpfun(lm.lowess_) #compilier das Teil

###################################################################
##lineare regression - mit optionalem channel (wenn level ==0 ), und prognose (nplus Tage)
#optional auch gewichtet,    w = "lm.w"
# oder nach lowess  w="lowess"
#gib model, p.value,r.squared  zur?ck
#w=T heisst weighted .. j?ngere Werte sind dann wichtiger
####################################################################
m.lm.fit<-function(p,visual=F,getIndex=T, w ="",level=0,nplus=0,glaettung=0,w.method="dist.by.date",w.range=3)
{
  
  if (len(p)<2)
  {res = list(fit=NA, m=NA, b= NA,r.squared=NA,p.value=NA)
   return(res)
  }
  if (w == "lowess")
  {
    res = list(fit=NA, m=NA, b= NA,r.squared=NA,p.value=NA)
    A=lm.lowess(p,visual=visual,getIndex=getIndex,glaettung=glaettung)
    x2=shape(A)
    res$m=(A$y[x2]-A$y[x2-1]) / (A$x[x2]-A$x[x2-1])
    res$xy = A
    return(res)
  }
  else
    if (w=="")
      fit=lm.FITm(p ,visual=visual,getIndex=getIndex,level=level,nplus=nplus) 
  else
    if (w=="lm.w")
      fit=lm.FITmw( p ,visual=visual,getIndex=getIndex,level=level,nplus=nplus,w.method=w.method,w.range=w.range)
  else
    sag("Wrong usage of m.lm.fit - unknown w %s",as.character(w),warte=T)
  
  fit.m= coef(fit)[2]
  r.squared=summary(fit)$r.squared
  fstatistic=summary(fit)$fstatistic
  p.value=pf(fstatistic[1], fstatistic[2], fstatistic[3],   lower.tail = FALSE)
  
  res = list(fit=fit, m=fit.m, b= coef(fit)[1],r.squared=r.squared,p.value=p.value)
  
  if (visual)
    print(res)
  res
}

#lm.FITm_<-function(Y) {x=as.matrix(c(1:shape(Y))); y=coredata(as.numeric(Y)); res=lm(y~x)}
#lm.FITm<-cmpfun(lm.FITm_)
#########################################################################################################

m.predict<-function(lm.mod,firstDate="2012-01-03",n=10,y=NULL,interval="prediction",visual =F)#confidence)
{
  if (!is.null(y))
  {
    firstDate=DateS(first(y))
    y=(as.numeric(y))  #entscheident
    n=shape(y)+n
  }
  
  x=predict(lm.mod,newdata=data.frame(x=1:n),interval="prediction")#,level=c(80,90))
  x=  bt.apply.matrix(x,function(x) iif(x<0,0,x))
  
  
  nplus=n*2
  pre = xts(1:nplus, as.Date(firstDate)+0:(nplus-1));  
  pre = head(pre[!is.weekend(time(pre))],n)
  
  pre=merge(pre,pre,pre)
  pre[]=x
  
  if (visual && !is.null(y))
  {
    pre=merge(pre,y)
    mchart(pre,p)
    
  }
  return (pre)
  
  
  pre = xts(1:n, as.Date(firstDate)+0:(n-1))
  pre=merge(pre,pre,pre) ; 
  pre[]=predict(lm.mod,data.frame(x=1:n),interval =interval)# "confidence") #"prediction"
  pre=  bt.apply.matrix(pre,function(x) iif(x<0,0,x))
  pre
  
}
##############################################################################
#bis zu 4 zeitreihen
lm.loess<-function(p,visual=F,getIndex=T,main="loess",glaettung=0,...)
{
  
  p.org=p  #p.org enth???lt keine Wochenenden, somit 604 tage
  firstDate=DateS(first(p))
  p=(as.numeric(p))
  
  if (!getIndex)
    train <- data.frame(x=1:shape(p), y=coredata(p))  
  else
  { 
    # browser(mP("lm.Fitm"))
    #if (F)
    #   lm.FITm(upper,getIndex=T)
    #hier liegen die x nicht dicht .. also muss man spreizen
    xdat=as.Date(as.Date(index(first(p.org))):as.Date(index(last(p.org))))
    xdat.Lineal=xts(1:len(xdat), xdat)
    xdat= get.Index(xdat.Lineal, as.Date(index(p.org)),lineal=T)
    train <- data.frame(x=xdat, y=coredata(p))  
    
  }
  colnames(train) = c("x","y")
  
  if (glaettung==0)
    lowe=loess(y~x,data=train,...) # und damit dann die Quantile machen ...
  else
    lowe=loess(y~x,data=train,span=glaettung,...) # und damit dann die Quantile machen ...
  if (visual)
  {
    p.o=p.org; p.o[]=lowe$y
    plot(p.org,main=main); 
    points(p.org,col="blue")    
    lines(p.o,col="red",lwd=2)
  }
  return(lowe)
}#...........................

###########################################################################################################
#Generalized Boosted Regression Models
#d <- data.frame( y=y , x)

#gmod <- gbm( y ~ ., data=d, distribution="gaussian",
#             n.tree = 2000, shrinkage = .01 , cv.folds=5,
#             verbose = FALSE, n.cores=1)
#tmod4 <- gbm( y ~ ., data=d, distribution="tdist", # defaults to 4 df
#              n.tree=2000, shrinkage = .01, cv.folds=5,
#              verbose = FALSE, n.cores=1)
#tmod6 <- gbm( y ~ ., data=d, distribution=list( name="tdist", df=6 ),
#              n.tree=2000, shrinkage = .01, cv.folds=5,
#              verbose = FALSE, n.cores=1)
#tmod100 <- gbm( y ~ ., data=d, distribution=list( name="tdist", df=100 ),
#                n.tree=2000, shrinkage = .01, cv.folds=5,
#                verbose = FALSE, n.cores=1)

#par(mfrow=c( 2, 2 ) )
#gbest <- gbm.perf( gmod , method="cv" )
#t4best <- gbm.perf( tmod4 , method="cv" )
#t6best <- gbm.perf( tmod6 , method="cv" )
#t100best <- gbm.perf( tmod100 , method="cv" )

#multivar - angeblich noch besser f?r regression wie randomForest
lm.gbm<-function(p,visual=F,main="gbm",glaettung=0,...)
{
  library(gbm)#Generalized Boosted Models
  library(parallel)
  p.org=p  #p.org enth???lt keine Wochenenden, somit 604 tage
  firstDate=DateS(first(p))
  
  train <- na.omit(data.frame(y=coredata(lag(p[,1],-7)),x=coredata(p[,1])))
  colnames(train) = c("y","x")
  
  if (glaettung==0)
    lowe <- gbm(  y ~ . ,  data=train, distribution="gaussian", n.tree = 13000, shrinkage = .01 , cv.folds=ncol(train)-1, verbose = FALSE, n.cores=3)
  
  else
    lowe <- gbm( y ~ .,data=train, distribution="gaussian",      n.tree = 2000, shrinkage = .01 , cv.folds=ncol(train)-1,   verbose = FALSE, n.cores=1,n.trees=glaettung,...)
  
  return(lowe)
}#...........................
if (F)
{
  px=p["2008::2009"]
  p3 =lm.gbm(px)
  plot(coredata(px),type="l",main="gbm");lines(p3$fit,col="red");
  
}
###########################################################################################################
lm.smooth.spline<-function(p,visual=F,getIndex=T,main="loess",glaettung=0,...)
{
  
  p.org=p  #p.org enth???lt keine Wochenenden, somit 604 tage
  firstDate=DateS(first(p))
  p=(as.numeric(p))
  
  if (!getIndex)
    train <- data.frame(x=1:shape(p), y=coredata(p))  
  else
  { 
    # browser(mP("lm.Fitm"))
    #if (F)
    #   lm.FITm(upper,getIndex=T)
    #hier liegen die x nicht dicht .. also muss man spreizen
    xdat=as.Date(as.Date(index(first(p.org))):as.Date(index(last(p.org))))
    xdat.Lineal=xts(1:len(xdat), xdat)
    xdat= get.Index(xdat.Lineal, as.Date(index(p.org)),lineal=T)
    train <- data.frame(x=xdat, y=coredata(p))  
    
  }
  colnames(train) = c("x","y")
  
  if (glaettung==0)
    lowe= smooth.spline(x=xdat, y=coredata(p))  
  else
    lowe=smooth.spline(x=xdat, y=coredata(p),spar=glaettung,...) # und damit dann die Quantile machen ...
  
  if (visual)
  {
    p.o=p.org; p.o[]=lowe$y
    plot(p.org,main=main); 
    points(p.org,col="blue")    
    lines(p.o,col="red",lwd=2)
  }
  return(lowe)
}#...........................

lm.gamm<-function(p,visual=F,getIndex=T,main="gamm",glaettung=0,...)
{
  require(mgcv)
  require(nlme)
  
  p.org=p  #p.org enth???lt keine Wochenenden, somit 604 tage
  firstDate=DateS(first(p))
  p=(as.numeric(p))
  
  if (!getIndex)
    train <- data.frame(x=1:shape(p), y=coredata(p))  
  else
  { 
    # browser(mP("lm.Fitm"))
    #if (F)
    #   lm.FITm(upper,getIndex=T)
    #hier liegen die x nicht dicht .. also muss man spreizen
    xdat=as.Date(as.Date(index(first(p.org))):as.Date(index(last(p.org))))
    xdat.Lineal=xts(1:len(xdat), xdat)
    xdat= get.Index(xdat.Lineal, as.Date(index(p.org)),lineal=T)
    train <- data.frame(x=xdat, y=coredata(p))  
    
  }
  colnames(train) = c("x","y")
  
  if (glaettung==0)
    lowe=gamm(y~x,data=train,...) # und damit dann die Quantile machen ...
  else
    lowe=gamm(y~s(x,k=glaettung),data=train,...) # und damit dann die Quantile machen ...
  
  if (visual)
  {
    p.o=p.org; p.o[]=lowe$y
    plot(p.org,main=main); 
    points(p.org,col="blue")    
    lines(p.o,col="red",lwd=2)
  }
  return(lowe)
}#...........................
######################################################################
#Methoden zum transferieren eines filter-siganls in eine bin?r Richtung
#px ist ein recht glattes (filter) Signal-schau in welche Richtung es zeigt
#via mutlitplen differencen
g.Signal.d=function(px,k=3) {
  m.diff=xts(rowSums(foreach(d=c(1:k),.combine="merge") %do% {diff(px,d) /d}),index(px))
  m.diff
}
#g.Signal.d(px,6)

#px ist ein recht glattes (filter) Signal-schau in welche Richtung es zeigt
#via regressions-winkel
g.Signal.r = function(px,k=3){
  
  m.slope=rollRegressionXTS(na.omit(px),k)
}

#px ist ein recht glattes (filter) Signal, p der Kurs
#liegt der Kurs -oberhalb/unterhalb 
g.Signal.D<-function(p,px) {
  p-px
}


#px ist ein recht glattes (filter) Signal, p der Kurs
#liegt der Kurs -oberhalb/unterhalb  - zlema d-quadrat-gemittelt
g.Signal.D.zlema<-function(p,px,k=5) {
  ZLEMA((p-px)*(p-px),k=k)
}

#px ist ein recht glattes (filter) Signal, p der Kurs
#liegt der Kurs -oberhalb/unterhalb  - zlema d-quadrat-gemittelt
#nur wenn die differenz oberhalb des q-quantils liegt wird umgeschaltet
g.Signal.D.zlema.q<-function(p,px,k=5,q=.10) {
  d=ZLEMA((p-px)*(p-px),k=k)
  
  q=runquantile(d,30,probs=q,align="right")
  d[d<q]=NA
  d=m.ifna.prev(d)
  d
}

############################################

#What are the functions that you wrote, don't quite deserve a package, but you wish to share?
setdiff2 <- function(x,y) {
  #returns a list of the elements of x that are not in y 
  #and the elements of y that are not in x (not the same thing...)
  
  Xdiff = setdiff(x,y)
  Ydiff = setdiff(y,x)
  list(X_not_in_Y=Xdiff, Y_not_in_X=Ydiff)
}


#In the most useful R trick posting I saw a post by Keving from Nov 3 '09 bout dropping unused levels. The first function was provided there. and I took the best step in the second function to drop levels from a subset.

drop.levels <- function (dat) {if (is.factor(dat)) dat <- dat[, drop = TRUE] else dat[] <- lapply(dat, function(x) x[, drop = TRUE]); return(dat) ;};

subset.d    <- function (...) drop.levels(subset(...)); # function to drop levels of su


#I will throw in some of mine:

destring <- function(x) {
  ## convert factor to strings
  if (is.character(x)) {
    as.numeric(x)
  } else if (is.factor(x)) {
    as.numeric(levels(x))[x]
  } else if (is.numeric(x)) {
    x
  } else {
    stop("could not convert to numeric")
  }
}

pad0 <- function(x,mx=NULL,fill=0) {
  ## pad numeric vars to strings of specified size
  lx <- nchar(as.character(x))
  mx.calc <- max(lx,na.rm=TRUE)
  if (!is.null(mx)) {
    if (mx<mx.calc) {
      stop("number of maxchar is too small")
    }
  } else {
    mx <- mx.calc
  }
  px <- mx-lx
  paste(sapply(px,function(x) paste(rep(fill,x),collapse="")),x,sep="")
}
###############################################################

.eval <- function(evaltext,envir=sys.frame()) {
  ## evaluate a string as R code
  eval(parse(text=evaltext), envir=envir)
}


#######################################################################
# Gets the frequencies returned by the FFT function
getFFTFreqs <- function(Nyq.Freq, data)
{
  if ((length(data) %% 2) == 1) # Odd number of samples
  {
    FFTFreqs <- c(seq(0, Nyq.Freq, length.out=(length(data)+1)/2), 
                  seq(-Nyq.Freq, 0, length.out=(length(data)-1)/2))
  }
  else # Even number
  {
    FFTFreqs <- c(seq(0, Nyq.Freq, length.out=length(data)/2), 
                  seq(-Nyq.Freq, 0, length.out=length(data)/2))
  }
  
  return (FFTFreqs)
}

# FFT plot
# Params:
# x,y -> the data for which we want to plot the FFT 
# samplingFreq -> the sampling frequency
# shadeNyq -> if true the region in [0;Nyquist frequency] will be shaded
# showPeriod -> if true the period will be shown on the top
# Returns a list with:
# freq -> the frequencies
# FFT -> the FFT values
# modFFT -> the modulus of the FFT
plotFFT <- function(x, y, samplingFreq, shadeNyq=TRUE, showPeriod = TRUE)
{
  Nyq.Freq <- samplingFreq/2
  FFTFreqs <- getFFTFreqs(Nyq.Freq, y)
  
  FFT <- fft(y)
  modFFT <- Mod(FFT)
  FFTdata <- cbind(FFTFreqs, modFFT)
  plot(FFTdata[1:nrow(FFTdata)/2,], t="l", pch=20, lwd=2, cex=0.8, main="",
       xlab="Frequency (Hz)", ylab="Power")
  if (showPeriod == TRUE)
  {
    # Period axis on top        
    a <- axis(3, lty=0, labels=FALSE)
    axis(3, cex.axis=0.6, labels=format(1/a, digits=2), at=a)
  }
  if (shadeNyq == TRUE)
  {
    # Gray out lower frequencies
    rect(0, 0, 2/max(x), max(FFTdata[,2])*2, col="gray", density=30)
  }
  
  ret <- list("freq"=FFTFreqs, "FFT"=FFT, "modFFT"=modFFT)
  return (ret)
}

#As an example you can try this

# A sum of 3 sine waves + noise
if (F)
{
  x <- seq(0, 8*pi, 0.01)
  sine <- sin(2*pi*5*x) + 0.5 * sin(2*pi*12*x) + 0.1*sin(2*pi*20*x) + 1.5*runif(length(x))
  par(mfrow=c(2,1))
  plot(x, sine, "l")
  res <- plotFFT(x, sine, 100)
  #class(sine)
  #or
  
  plot(dax)
  res <-plotFFT(1:len(dax),diff(log(coredata(dax[frame]))),100)
  frame="::1995"
  m=2
  y=na.omit(coredata(SMA(dax[frame],m)))
  res <-plotFFT(1:y,y,100)
  frame="1999:2000"
  res <-plotFFT(1:len(dax),coredata(dax[frame]),100)
  frame="::1995"
  res <-plotFFT(1:len(dax),coredata(dax[frame]),100)
  
  
  
  
  linearChirp <- function(fr=0.01, k=0.01, len=100, samplingFreq=100)
  {
    x <- seq(0, len, 1/samplingFreq)
    chirp <- sin(2*pi*(fr+k/2*x)*x) 
    
    ret <- list("x"=x, "y"=chirp)
    return(ret)
  }
  
  chirp <- linearChirp(1, .02, 100, 500)
  par(mfrow=c(2,1))
  plot(chirp, t="l")
  res <- plotFFT(chirp$x, chirp$y, 500, xlim=c(0, 4))
}

######################################################################################################
# kleine hilfsmethode:  zeigt alle elemente einer liste an, und wartet nach jedem auf tastendruck
#sehr gut um z.B. sysinvestor-modelle anzusehen
######################################################################################################

inspect<-function(mod)
{
  lapply(names(mod),function(x){mP("###### >> vari %s",x);print(mod[[x]]); print(str(mod[[x]])); mP("###### << vari %s",x);browser()})
}

####################################################################
#baut ein sch?n benamste Liste  mit l.red=l.red,  l.orange=l.orange...
#mkList(l.red,l.orange,l.magenta,m.red,m.orange,m.magenta,sig.red,sig.orange,sig.magenta,sig.sum, d.red,dm.red, dp.red)
#####################################################################
mkList<-function(...)
{
  res =list()
  args=list(...)
  arg.names <- as.list(substitute(list(...)))  
  for(i in 2:len(arg.names))
  {
    list.nam=as.character(arg.names[[i]])
    list.val=args[[i-1]]
    res[[list.nam]]=list.val
  }  
  res
}

MkList<-function(...,res=list())
{
  args=list(...)
  arg.names <- as.list(substitute(list(...)))  
  for(i in 2:len(arg.names))
  {
    list.nam=as.character(arg.names[[i]])
    list.val=args[[i-1]]
    res[[list.nam]]=list.val
  }  
  res
}
if (F)
{
  a=list()
  b=33
  c=44
  a=MkList(b,a)
  a=MkList(c,a)
}
mkTable<-function(...)
{
  # mlist=list(list(a=3,b=5))
  mlist=list(mkList(...))
  res.l=data.frame(rbindlist(mlist))
}
####### man kann einfach beliebige Variablen ?bergeben.. die werden zu einer Tabelle
appendTable<-function(tabName,...)
{
  #browser(mP("appendTable"))
  if (is.character(tabName))
  {tabName <- deparse(substitute(tabName))
   
   if (exists(tabName) && ! is.null(tabName))
     eval(parse(text=sprintf("%s<<-rbind(%s,mkTable(...))",tabName,tabName)))
   else 
     eval(parse(text=sprintf("%s<<-mkTable(...)",tabName)))
   
   return(tabName)
  }
  else
    
  { if ( is.null(tabName))
    return( mkTable(...))
    
    return(rbind(tabName, mkTable(...)))
  }
}


if (F)
{
  a=3
  b=4
  x112=mkTable(a,b)
  rbind(x111,x112)
  appendTable(t1Result, a,b)
}
#########################################################################

find.wlen_<-function(p,target.range=3)  #vorsicht - nimm lieber find.Wlen()
{
  l=len(p)
  res =l
  for(w in 2:l)
  {       
    rag=range(p[(l-w):l])
    #rg=diff(rag)/rag[1]*100
    rg = diff(rag)
    # mP("%d %f  %f",w, diff(rag),rg)
    if (rg >= target.range)
    {res=max(1,w-1); break}
  }
  #  res = as.xts(as.numeric(res),order.by=as.Date(index(last(p))))
  return(res)
}
find.wlen<-cmpfun(find.wlen_) #compilier das Teil
###########################################################################
###########################################################################

get_rob<-function(p,iw=11,ow=110)
{
  library(robfilter)
  #y=Cl(p);newXts=Cl(p)
  y=coredata(p)[,1]
  # Filtering with all methods:
  #y.dw <- dw.filter(y,online=T, outer.width=31, inner.width=11, method="all")
  # Plot:
  #browser(mP("get_rob"))
  #plot(y.dw)
  #coole-online-filter 
  # Filtering with trimmed RM and double window TRM only:
  y2.dw <- dw.filter(y, online=T,outer.width=ow, inner.width=iw, method=spl("MED,RM,MTM,TRM,DWMTM,DWTRM"))
  #Auf dem DWMTM sollte man mom(3) ganz guten Trend-Detektor bauen (geringe Werte)
  #stehe dann f?r horizontale St?cke
  #der RM ist schnell und glatt genug - und schneidet den langsamen MTM
  #ein signal ist aber nur dan, wenn der DWMTM nicht horizontal ist.
  
  #tail(y2.dw$slope)
  #tail(y2.dw$level)
  
  newXts=xts(y2.dw$level, index(p))
  newXts.slope=xts(y2.dw$slope, index(p))
  #browser()  
  ind= merge(p,newXts,SMA(p,200))
  # mchart(ind)
  purePlot(na.omit(ind))
  #mPlots(ind)
  #mchart(ind)
  #lines(SMA(p,200),lwd=2)
  #die DWMTM haben nur 1 bis 2 Tage delay
  #apply(ind,2,function(indi) which(indi==min(indi))) #3Tage Delay!!!
  list(filter=newXts, slope=newXts.slope) 
}

if (F)
  res =get_rob(dax,ow=220)


########################################################################## 
#4 klarer Cut  (auch ein Vorg?ngerTag war schon mindestes bei 3) 
#......................  3  in der Touchier-Zone (cut)
#2   overbought
#1
#...................     0
#-1
#-2 oversold
#.......................-3  in der Touchier Zone (cut)
#-4  klare Cut (auch ein Vorg?nger Tag war schon bei mindestens -3)
########################################################################## 
#Moving Window Quantiles (smoothed %s %d)
# wenn opt.getQ == get.Quantile.M - werden sign der quantil-Steigerungen zurr?ckgegeben
# wenn opt.getQ == 2 - obere und untere Kan?le linear gefitted
if (F)
{
  itp=in.Trend.pos(p,visual=T,main="itp",k=60,K=3,probs=c(0.20,  .5,  0.95))
  mPlots(itp,p) 
  itp=in.Trend.pos(mROC(p,7),visual=T,main="itp",k=160,K=2,probs=c(0.20,  .5,  0.80))
  cloud.check(itp)
  itp[is.infinite(itp)]
  mPlots(itp,p) 
  
}
in.Trend.pos<-function( p, up.low = NULL,date=NULL,visual=F,main="",last=F,opt.getQ="",k=160,K=1,probs=c(0.01,  0.5,  0.99)) #mid,low,up  opt.getQ = get.Quantile.M
{
  p=p[,1]
  P=NULL
  name <- "  "#deparse(substitute(p)) 
  
  if (F)
    if (name=="itp.dp.red" && DateS(last(p))=="2004-03-03") #TEST
      browser(mP("in.Trend.pos - TEST"))
  #browser(mP("in.Trend.pos - TEST"))
  
  if (is.null(date))
  {
    if (is.null(up.low)) #kein Kanal gegeben - nimm einfach die Quantile
    {
      k=min(shape(p)-1,k);  #das moving-window in dem quantile gez?hlt werden 
      #K=1
      Align="right"
      x=coredata(p)
      y=runquantile(x, k, probs=probs,endrule="quantile",align=Align)
      
      #bad y=runquantile(x, k, probs=c(0.05,  0.5,  0.95),endrule="quantile",align=Align)
      #ok y=runquantile(x, k, probs=c(0.05,  0.5,  0.95),endrule="quantile",align="center")
      
      up.low=merge(p,p,p)
      up.low[,2]=y[,1,1]
      up.low[,1]=y[,1,2]
      up.low[,3]=y[,1,3]
      if (K>1)
      {
        up.low[,2]=runmean(y[,1,1],K,align=Align)
        up.low[,1]=runmean(y[,1,2],K,align=Align)
        up.low[,3]=runmean(y[,1,3],K,align=Align)
      }
      #backshift=k/2  # die letzten brauchbaren Quantile-Werte liegen backshift zur?ck
      backshift=0
      mid=clean.matrix(lag(up.low[,1],k= backshift));colnames(mid)="mid"
      low=lag( up.low[,2], k=backshift);colnames(low)="low"
      up=lag(up.low[,3], k=backshift)  ;colnames(up)="up"
      sig = iif(p>=mid,1,-1)
      #browser(mP("###1"))
      
      res = iif (sig >=0, iif(up==mid,0,round((p-mid)/((up-mid)/3))), iif(mid==low,0, round((p-mid)/((mid-low)/3))))
      res=cutInterval(res,-20,20)
      
      if (visual)
      {
        mchart(merge(p,mid,low,up),main = sprintf("in.Trend.Pos(%s,%s smoothed %d)",name,main,K))
        abline(h=last(x),lwd=3,col="orange")
      }
      if(last)
        res = last(res)
      if (visual)mP("in.Trend.Pos %s %s %f",main,name,res)
      if (opt.getQ== "get.Quantiles")
      {
        return(list(itp=res, Q =merge(mid,low,up)))
      }
      if (opt.getQ == "get.Quantile.M") #gib zus?tzlich die groben Steigungen der einh?llenden Quantile zur?ck
      {
        mP("check get.Q")
        #mPlots(mid,m.mid,sign(mROC(EMA(m.mid,7))))
        wi=30
        m.mid = rollRegressionXTS(clean.matrix(mid),win=min(shape(mid),wi,na.rm=T))*100
        m.low = rollRegressionXTS(clean.matrix(low),win=min(shape(low),wi,na.rm=T))*100
        m.up = rollRegressionXTS(clean.matrix(up),win=min(shape(up),wi,na.rm=T))*100
        
        if (visual)
          mPlots(p,merge(m.mid,m.low,m.up),merge(m.up-m.low,0))
        
        Q.m=merge(m.mid=m.mid,m.low=m.low,m.up=m.up,mid=mid,low=low,up=up)
        colnames(Q.m)=spl("m.mid,m.low,m.up,mid,low,up")
        if (visual)
          mPlots(p,Q.m)
        
        
        #mchart(Q.m[,c(4,5,6)])
        
        if (last)
          Q.m=merge(m.mid=last(m.sig(mid,tlen=k)),m.low=last(m.sig(low,tlen=k)),m.up=last(m.sig(up,tlen=k),mid=last(mid),low=last(low),up=last(up)))
        return(list(itp=res, Q.m=Q.m ))
      }
      return(res)  
      
    }
    else
    {
      #if (shape(p) > 1) browser(mP("roll in.Trend.Pos"))
      res= rollapplyr(as.Date(index(p)),FUN=function(date) in.Trend.pos(p,up.low,date=date),width=1)
      res.1=p[,1];  res.1[]=res
    }
    return(res.1)
  }
  else
  {
    if (is.null(up.low))
    {
      P=last(p) #der aktuelle Kurs
      date=as.Date(index(P))
      #browser(mP("itp"))
      k=min(shape(p),k);  #das moving-window in dem quantile gez?hlt werden  
      K=3
      x=coredata(p)
      y=runquantile(x, k, probs=c(0.05,  0.5,  0.95))
      
      up.low=merge(P,P,P)
      up.low[,2]=last(runmean(y[,1,1],K))
      up.low[,1]=last(runmean(y[,1,2],K))
      up.low[,3]=last(runmean(y[,1,3],K))
      sig = iif(p>=mid,1,-1)
      #res = iif (sig >=0, round((P-mid)/((up-mid)/3)), round((P-mid)/((mid-low)/3)))
      res = iif (sig >=0, iif(up==mid,0,round((p-mid)/((up-mid)/3))), iif(mid==low,0, round((p-mid)/((mid-low)/3))))
      res=cutInterval(res,-20,20)
      
      if (visual)
      {
        col = c( "red", "green", "blue")
        
        plot(x, col="black", main =sprintf("2in.Trend.Pos(%s ,%s smoothed %d)",name,main,K))
        lines(runmean(y[,1,1],K), col=col[1])
        lines(runmean(y[,1,2],K), col=col[2])
        lines(runmean(y[,1,3],K), col=col[3])
        abline(h=last(x),lwd=3)
      }
      
      if (visual) mP("2in.Trend.Pos %s %s %f",main,name,res)
      return(res)
    }
    
{
  # browser(mP("itp"))
  P=nval(p[date]) #der aktuelle Kurs
  up=nval(up.low[date,3])
  low=nval(up.low[date,2])
  mid=nval(up.low[date,1])
  if (is.na(P) || is.na(up) || is.na(low) || is.na(mid))
    return(NA)
}

sig = ifelse(P>=mid,1,-1)
if (sig >=0)
{
  d=(up-mid)/3
  if (d==0)
    res=0
  else
    res = round((P-mid)/d)
}
else
{
  d=(mid-low)/3
  if (d==0)
    res=0
  else
    res = round((P-mid)/d)
}

res=cutInterval(res,-20,20)

#browser(mP("in.Trend.pos %d",res))
if (visual)mP("3in.Trend.Pos %s %f",name,res)

res
  }
}
#in.Trend.pos<-cmpfun(in.Trend.pos_) #compilier das Teil

if(F)
  in.Trend.pos(dax[,1],visual=T,main="itp",k=160,K=3,probs=c(0.15,  0.5,  0.80))

#####################################################################
#kleines schiebregister - nimmt tlen werte auf und entfernt ab dann
#den ?ltesten wert.
#entweder als vector .. oder als xts (wenn new.xts ein xts ist)
#####################################################################
pipe.xts_<-function( new.xts,old.xts=NULL, tlen=10)
{
  if (shape(new.xts)==1 && is.na(new.xts)) #keine na aufnehmen
    return(old.xts)
  
  new.xts=tail(new.xts,tlen)  #neuere Werte passen eh nicht rein
  
  
  if (is.null(old.xts))
  {  if (is.xts(new.xts))
    res = new.xts
    else
      res = c(new.xts)
  }
  if (is.xts(new.xts))
    res=rbind(old.xts,new.xts)
  else
    res=c(old.xts,new.xts)
  
  if (shape(res)>tlen)
    res = res[-1]
  #  browser(mP("pipe.xts"))
  res
}
pipe.xts<-cmpfun(pipe.xts_) #compilier das Teil
#............................................
if (F)
{
  pipe=NULL
  k=  last(dax,10)
  for(i in 1:len(k))
  {
    nv=  nval(k[i])
    nv = k[i]
    pipe=  pipe.xts(nv,pipe,3)
  }
}

######################################################################
#er schreibt eine pipe in  die Globale:   itp.PIPES$<nameOf(new.xts)>
#als r?ckgabewert gibt er - sobald die pipe voll ist - die inTrendPos()
#zur?ck .. bezogen auf die Top,Low-Quantile der vergangenen Werte.
# er sagt einem also, ob der neue Wert zu den alten Werten passt... -
# oberhalb oder unterhalb der Mitte ist - oder aus dem Rahmen f?llt (>abs(3))
######################################################################
itp.pipe<-function( new.xts, tlen=10,visual=T,last=T,opt.getQ="")
{
  name=name = deparse(substitute(new.xts))
  
  if (!is.xts(new.xts))
    sag(sprintf("itp.pipe-Warning - no xts at %s",name),warte=T)
  
  name=name = deparse(substitute(new.xts))
  
  if (!exists("itp.PIPES"))
    itp.PIPES<<-list()
  
  #pipe.name=sprintf("itp.PIPES%s%s","$",name)
  if (len(itp.PIPES[[name]]) <1)
    itp.PIPES[[name]]<<-NULL
  
  if (last)
    new.xts=last(new.xts)
  
  
  s=itp.PIPES[[name]] <<- pipe.xts(new.xts,itp.PIPES[[name]],tlen)
  #s = pipe.xts(new.xts,old.xts,tlen)
  if (shape(s)<tlen) return(NA)  
  
  if (!is.xts(s))
  {
    mP("itp.pipe-Warning - no xts at %s",name)
    s=xts(s,Sys.Date()+1:len(s))
  }
  # browser(mP("itp.pipe-TEST1 %s",name))
  itp.s= in.Trend.pos(s,visual=visual,main=sprintf("itp.pipe %s %d",name,tlen),last=last,opt.getQ=opt.getQ)
  
  if (visual)
    browser(mP("itp.pipe-TEST1 %s",name))
  if (last && opt.getQ =="") return(last(itp.s))
  itp.s
}

#............................................
if (F)
{
  itp.PIPES<<-list()
  itp.PIPES$nv=NULL
  names(itp.PIPES)
  
  k=  last(dax,100)
  for(i in 1:len(k))
  {
    nv=  nval(k[i])
    nv = k[i]
    itp=  itp.pipe(nv,30)
    mP("itp %d = %f",i,itp)
    if (i> 30)
      browser(mP("check"))
  }
}
#####################################################################################
# schafft die Voraussetzungen um parallel zu rechnen

#####################################################################################
prepare_Parallel<-function()
{
  mP("###  prepare_Parallel()  ###  >>")
  CORES_<<-detectCores()  
  library(snowfall)
  library(doSNOW)
  
  sfInit(parallel=TRUE, cpus=CORES_-1, type="SOCK")
  snow.cl<<-sfGetCluster()
  #sfStop()
  mP(" %d cores ! ",  getDoParWorkers())
  
  library(doParallel)
  registerDoSNOW(snow.cl)  #f?r foreach
  
  #schick die init_TSA methode zu den cores
  sfExport("init_TSA")
  sfExport("define.Globals")
  
  #rufe sie so auf, dass sie nur sourced und pakete l?dt
  mP("init_TSA  without universe")
  sfClusterCall("init_TSA",universe="") #jetzt l?dt er sich alle packete und sourcen
  
  mP("### export data to cluster ###")
  #schick data rauf zu allen cores
  sfExport("data") 
  #diese Jobs machen alle cores gleich:
  sfClusterEval(ls(data))  #syntax wie in der r-console
  #hier werden methoden und parameter getrennt aufgerufen
  sfClusterCall(ls,data)
  sfClusterCall(runif,4)
  #diese jobs machen sie nur zum teil
  print(unlist(sfLapply( data$symbolnames, print )))
  #sfRemove(define.Globals)    
  
  sfClusterEval(print(ModelRoot))
  sfClusterEval(global_FastTrain)
  #sfCat(data$symbolnames)
  #speicher wieder frei geben
  if (F)
    sfStop()
  
  sfExport("data")  
  sfExport("global_arg")  
  sfExport("global_ParTable")  
  sfExport("global_StartDate")  
  sfExport("global_FastTrain")  
  sfExport("BENCH")  
  
  mP("define.Globals")
  sfExport("define.Globals")
  sfClusterEval(define.Globals())
  
  mP("<< ###  prepare_Parallel()  ###  ")
}

if (F)
{
  sfExport("signal.TEST.1.a")
  sfSource("MLib/labor_signal.r")
  
  system.time(lapply(data$symbolnames,wrapper))
  names(se[[5]])
  c.se[[5]]$best
  
  system.time( sfClusterApplyLB(data$symbolnames,wrapper)) #
  #oder auch 
  c.se = sfLapply(data$symbolnames,function(sym)
    indi.Generic("signal.TEST.1.a", global_arg, par=list(sma.w=200),visual=F,TRAINSYM =sym)) #sfClusterApplyLB
  
  names(c.se)
  names(c.se[[5]])
  c.se[[5]]$best
  
}
#########################################################################
##############################################################
# die steigung wird berechnet als m= y2-y1/x2-x1  
#wobei hier x2 am ende von y gew?hlt wird und tlen = x2-x1 ist
##############################################################
m.sig = function(y,tlen=-1)
{
  
  #browser(mP("m.sig"))
  if (tlen == -1)
  {
    y=na.omit(y)
    tlen= shape(y)
  }
  #y=tail(y,tlen)
  #m=(nval(y[tlen])-nval(y[1]) )/ tlen
  
  # mP("m.sig tlen %d %d",tlen,shape(y))
  #browser()  
  l.y=shape(y)
  tlen=min(l.y-1,tlen)
  
  y=coredata(y) 
  m= (y[l.y]-y[l.y-tlen])/tlen  
  as.xts(m,as.Date(index(last(y))))
}


##gehe so lange nach links bis du ein range von 15% hast
find.Wlen_<-function(p,target.range=3)
{
  l=len(p)
  res =l
  
  for(w in 2:l)
  { 
    rag=range(p[(l-w):l])
    #rg=diff(rag)/rag[1]*100
    rg = diff(rag)/rag[1]*100
    # mP("%d %f  %f",w, diff(rag),rg)
    if (rg >= target.range)
    {res=max(1,w-1); break}
  }
  #  res = as.xts(as.numeric(res),order.by=as.Date(index(last(p))))
  return(res)
}
find.Wlen<-cmpfun(find.Wlen_) #compilier das Teil


########################################################################
#mit Returns ansteuern
#Alternative
#chart.Correlation(ROC(euro.indi["2010::2014"],30),main="2010::2014") 

#correlogramm(ROC(euro.indi["1997::"],30),main=" euro.indi")

#########################################################################
correlogramm<- function(xts.data,main = "")
{
  #delete missing data which is denoted by -0.9999
  xts.data[which(xts.data < -0.99,arr.ind=TRUE)[,1],
           unique(which(xts.data < -0.99,arr.ind=TRUE)[,2])] <- 0
  
  #get a vector of the end of years
  evaldates <- endpoints(xts.data,"years")
  
  for(i in 2:length(evaldates)) {  
    #do correlation table
    ca <- cor(xts.data[evaldates[i-1]:evaldates[i],])
    
    #replace na with 0
    ca[which(is.na(ca),arr.ind=TRUE)[,]] <- 0
    
    #get colors to use for heat map
    brew <- brewer.pal(name="RdBu",n=5)
    #get color ramp
    cc.brew <- colorRampPalette(brew)
    #apply color ramp
    cc <- cc.brew(nrow(ca))
    #do heatmap and sort by degree of correlation to VFINX (Vanguard S&P 500)
    #heatmap(ca,symm=TRUE,Rowv=NA,Colv=NA,col=cc,RowSideColors=cc,main="")
    #title(main=paste("Correlation Table\n",index(french_industry_xts)[evaldates[i]],sep=""),font.main=1,outer=TRUE,line=-2,cex.main=1.3)
    #do with dendrogram ordering
    heatmap(ca,symm=TRUE,col=cc,RowSideColors=cc,main="")
    title(main=paste("Correlation Table (Dendrogram Ordered)\n",index(xts.data)[evaldates[i]],main,sep=""),font.main=1,outer=TRUE,line=-3,cex.main=1.3,adj=0)  
  }
}


################################################################################################################
#damit kann ich Eckhards xls-Sheets  jeweils in ein xts einlesen
################################################################################################################
read.EckhardsXLS <-function( modelDir="EM4_November", xlsName="EuropeMM.xls",sheet.i = 1,startRow=14,belowColnames=1,visual=T,debug=F,dec=",",date.format="%d.%m.%Y",to.month.end=F)
{
  #startRow=startRow-1
  #Beispiel:  Einlesen von xls-File#######################
  library(XLConnect)
  
  dataPath =sprintf("Models/%s",modelDir) #da liegen die xls-files
  mP("read at path %s",dataPath)
  #F?r alle xls-Files
  xlsPat = glob2rx("*.xls") #wildcard-Transfer
  
 # for (xlsfile in 
#       dir(path = dataPath, pattern = xlsPat, all.files = T,
#           full.names = F, recursive = FALSE,
#           ignore.case = FALSE, include.dirs = FALSE)
#  )
#    print(xlsfile)
  
  
  if (debug)browser(mP("read.EckhardXLS"))
  #ueberlies die ersten 5 Zeilen
  xlsfile=sprintf("%s/%s",dataPath,xlsName)  #TEST
  
  mP("read: %s",xlsfile)
  
 if (is.numeric(sheet.i))
 {
  wb <- XLConnect::loadWorkbook(xlsfile)#, create = TRUE)
  sheet.i=XLConnect::getSheets(wb)[sheet.i]
 }
  bm <- readWorksheetFromFile(xlsfile, sheet=sheet.i,  startRow=startRow, header=F) #,

 print(head(bm))
  bm2=bm
  #Gib der ersten Spalte einen anderen Namen:   "Index"
  print("#############################################")  
  newColnames=head(bm2,1) #TEST
  
  print(newColnames)
  mP("<<<- newColnames")

 if (!exists("FUNDIS") || len(FUNDIS)==0)
      FUNDIS=list()
  
  #Colnames=head(bm,1) 
  #print(Colnames)
  #goodCols=which(!is.na(Colnames) & substr(Colnames,1,3) != "Col") 
  #naCols=which(is.na(Colnames)) 
  #brks=naCols[!(naCols-1) %in% naCols]
  #brks=brks[brks>1]
  #fundis = c(brks[1]+1, brks[2]-1)
  
  
  #newColnames[5]
  if (belowColnames>0)
    bm2=bm2[c(-1:- belowColnames),]
  
  b.test=min(10,len(newColnames))
  print("first colnames")
  print(newColnames[1:b.test])
  print("first datarow")
  print(head(bm2[,c(1:b.test)],1))
  
  #finde die erste Spalte in der wirklich das Datum steht - also die vorletzte die keinen Col-Bezeichner hat .. (Eckhards-format ...?)
  firstCol  = 0
  for  (cN in 1:len(colnames(bm2)))  
  {
    firstCol = firstCol+1
    #if (substr(cN, 1, 3) == "Col" && !isEmptyCol(bm2,cN))
    if (!isEmptyCol(bm2,cN))
    {
      break
    }
  }
  mP("%s first Column has Date at Col Nr %d", xlsfile ,firstCol)
  print(head(bm2[,firstCol]))
  if (debug)
    browser()
  gesamt.xts = NULL
  normDate=NULL
emptyCol=0
j=1
  for ( i in (firstCol+1): dim(bm2)[2])   #firstCol zeigt auf die  Datum-Spalte
  {
    if (!isEmptyCol(bm2,colnames(bm2[i])))
    {
      #i=firstCol+1 #TEST
      #erzeuge einen 2 Spalen-Tabelle
      #mP(" use Col  %d",i)
      bm3 = na.omit(bm2[,c(firstCol,i)])
      
      if (!is.null(bm3))  #MM1
        if ( dim(bm3)[1] > 0)
        {
          #hol den wpNamen als Namen der Spalte 2
          
          wpName0 = as.character(newColnames[i])  #colnames(bm3)[2] 
          wpName = prettyWpName(wpName0)
          if (debug)
            browser(mP("debug column %s",wpName))
          #umtauschen von  , in . und umwandeln des datums in standardformta
          if (is.null(normDate) )
          if (date.format == "%b %y")
            normDate=strptime(paste("01",trim(c(bm3[,1]))),"%d %b %y")
          else
            normDate=strptime(trim(c(bm3[,1])),date.format)
          
          if (is.na(normDate[1]))
          {
            sag("unknow-Date-Format ",warte=F)
            browser(mP("#!# press key"))
          }
          
          if (dec == ",")
            bm3.xts=xts(as.numeric(gsub( ",", ".",bm3[,2],fixed=TRUE)), head(as.Date(normDate),nrow(bm3)))  
          else
            bm3.xts=xts(as.numeric(bm3[,2]),as.Date(strptime(normDate)))
          colnames(bm3.xts) = c(wpName)
          
          if (debug)
            print(head(bm3.xts))
          
          #umsetzen der dat?mer aufs montasenden
          if (to.month.end)
          { 
            mP("umsetzen der datuemer auf montasenden")
            bm3.xts=xts( coredata(bm3.xts),Dates.month.ends(as.Date(index(bm3.xts))))
          }
          bm3 = bm3.xts
          if (is.null(gesamt.xts))
            gesamt.xts = bm3
          else
            gesamt.xts = merge(gesamt.xts,bm3)
          j=j+1
        }
    }
    else 
    {
      emptyCol=emptyCol+1
      if (emptyCol==1)
        fundi1=j
      if (emptyCol==2)
        fundi2=j-1
    }
  }
if (len(fundi1)>0 &&  len(fundi2)>0)
  {
  FUNDIS[[xlsName]] <<- c(fundi1:fundi2) 
  print("Fundis: ---------->")
  print(colnames(gesamt.xts[,fundi1:fundi2]))
  #print(colnames(gesamt.xts))
  } 
  
  mP(".... gesamt.xts ist fertig")
  
  if (visual)
  {
    View(gesamt.xts)
    colnames(gesamt.xts)
    print(fromToS(gesamt.xts))
    purePlot(gesamt.xts)
    
  }
  return(gesamt.xts)
}
#...................................................................................................................
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
  
  euro.macros.n= mNorm(lag(euro.macros))["1997::"] #hier schon mal gelagged weil die Teile ja einen Monat Lieferversp?tung haben
  euro.indi.n = mNorm(euro.indi)["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  
  purePlot(ROC(euro.macros["1997::"],1),main="roc 1 euro.macros")  #kaum was zu sehen
  purePlot(ROC(euro.macros["1997::"],30),main="roc 30 euro.macros")  #starke vola-cluster
  purePlot(ROC(euro.indi["1997::"],30),main="roc 30 euro.indi")
  
}

###################################################################################################
if (F)
  read.HUA.XLS( modelDir="HuA_Jan14", xlsName="AAData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=T,dec=",",date.format="%d.%m.%Y")


read.HUA.XLS <-function( modelDir="HuA_Jan14", xlsName="AAData.xls",sheet.i = 1,startRow=10,belowColnames=3,visual=T,dec=",",date.format="%d.%m.%Y",selectedCols=NULL)
{
  mP("read.HUA.XLS %s %s",modelDir,xlsName)
  #startRow=startRow-1
  
  #Beispiel:  Einlesen von xls-File#######################
  library(XLConnect)
  belowColnames=belowColnames+1
  
  dataPath =sprintf("Models/%s",modelDir) #da liegen die xls-files
  mP("read at path %s",dataPath)
  #F?r alle xls-Files
  xlsPat = glob2rx("*.xls") #wildcard-Transfer
  
  if (F)
  {
  for (xlsfile in 
       dir(path = dataPath, pattern = xlsPat, all.files = T,
           full.names = F, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE)
  )
    print(xlsfile)
  }
  
  
  #ueberlies die ersten 5 Zeilen
  xlsfile=sprintf("%s/%s",dataPath,xlsName)  #TEST
  
  mP("read: %s",xlsfile)
  bm <- readWorksheetFromFile(xlsfile, sheet=sheet.i, startRow=startRow, header=F)
  print(head(bm))
  #Gib der ersten Spalte einen anderen Namen:   "Index"
  print("#############################################")  
  print(xlsfile)
  Colnames=head(bm,1) #TEST
  
  
  print(Colnames)
  goodCols=which(!is.na(Colnames) & substr(Colnames,1,3) != "Col") 
  print(Colnames[goodCols])
  #Colnames[goodCols[9]]
  colI = first(goodCols)
  
 # len(goodCols)
  if (!is.null(selectedCols))
    goodCols = goodCols[c(selectedCols)]
#colnames(bm[,23])
#  cloud.check(bm[,goodCols])
#head(bm[,goodCols],5);
#tail(bm[,goodCols],5)

#bm=  na.omit(bm[,  unlist(lapply(goodCols, function(d)c(d-1,d)))]) 



  gesamt.xts=NULL
    for(colI in goodCols )
{
  Bm=na.omit(bm[-(1:belowColnames),c(colI-1,colI)])
  x.times= as.Date(na.omit(Bm[,1]))
  x.data= Bm[,2]
  new.xts=xts(as.numeric(gsub( ",", ".",x.data,fixed=TRUE)),x.times)  
  colnames(new.xts)=Colnames[colI]
  mP("#### %s ##### %s ",colnames(new.xts),fromToS(new.xts))
  
  if (is.null(gesamt.xts)) gesamt.xts= new.xts else gesamt.xts = merge(gesamt.xts,new.xts)
}
global_disable_cloud.check <<- F
cloud.check(gesamt.xts)
mP("gesamt.xts %s  gemeinsam blos:  %s ",fromToS(gesamt.xts),fromToS(na.omit(gesamt.xts)))
View(tail(gesamt.xts,2000))
gesamt.xts=na.omit(gesamt.xts)
cloud.check(gesamt.xts)
print(fromToS(gesamt.xts))
mP(".... gesamt.xts ist fertig %s",fromToS(gesamt.xts))

if (visual)
{
#  View(gesamt.xts)
  colnames(gesamt.xts)
  print(fromToS(gesamt.xts))
#  cloud.check(gesamt.xts)
  mPlots(gesamt.xts)
  purePlot(mNorm(gesamt.xts)) 
}
return(gesamt.xts)
}


#................................................
#lade alle  data-Dateien aus seinem Results.xls  -  verzeichnis und baue aus den vorgefundenen  models - variablen eine lange liste


scan.model.list<-  function (models,datafile="dat")
{   
  print("scan.model.list")
  print(names(models))
  
  if (has(models[[1]],"capital"))  #unterste Ebene erreicht
  {
    mP("####>")
    names(models)=sapply(names(models),function(mod)   sprintf("%s#%s",datafile,mod))
    sapply(names(models),function(mod) colnames(models[[mod]]$equity) <<- mod)
    
    all.models<<- append(all.models, models)
  } 
  else
    {
       lapply(models,scan.model.list,datafile=datafile)
       #all.models= "top"  
    }
  
  all.models
}           



load.models<-function(modelDir,what="")
{
  all.models<<-list()
  dataPath =sprintf("%s/Models/%s/Results.xls",getwd(),modelDir) #da liegen die xls-files
  mP("read at path %s",dataPath)
  #F?r alle xls-Files
  pat="*.data"
  if (F && what != "")
    flist=c(sprintf("%s/%s",dataPath,what))#sprintf("'%s'*.dat",what)
  else
  {
  xlsPat = glob2rx(sprintf("%s%s",what,pat)) #wildcard-Transfer
  flist=     dir(path = dataPath, pattern = xlsPat, all.files = T,
                 full.names = T, recursive = FALSE,
                 ignore.case = T, include.dirs = F)
}  

  for (xlsfile in flist)
    {
    datafile= unlist(str_split( basename(xlsfile),".data"))[1]
    mP("Load models from %s ",datafile)
    load(file=xlsfile) 
    print(".....................................................>")
    print(ls(models))
    scan.model.list(models,datafile=datafile )
   
#   print("#################> all.models")
#   print(names(all.models))
#   browser()   
#all.models
  }
print("#################> all.models")
print(names(all.models))
#
all.models
}
if (F)
{
  m= load.models("HuAFeb_3")
  #,"MWorld3")
  m= load.models("HuaFeb_3","MWorld3")
  
  m= load.models("HuaFeb_3","hua")
  m = list2xts(m,"equity")
  m= na.omit(m)
  mchart(mNorm(m))
  colnames(m)
  
}

#...................................................................................................................
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
  
  euro.macros.n= mNorm(lag(euro.macros))["1997::"] #hier schon mal gelagged weil die Teile ja einen Monat Lieferversp?tung haben
  euro.indi.n = mNorm(euro.indi)["1997::"] #wichtig:  die Anfangslag normiert die (fr?hen) Werte
  
  purePlot(ROC(euro.macros["1997::"],1),main="roc 1 euro.macros")  #kaum was zu sehen
  purePlot(ROC(euro.macros["1997::"],30),main="roc 30 euro.macros")  #starke vola-cluster
  purePlot(ROC(euro.indi["1997::"],30),main="roc 30 euro.indi")
  
}
########################################################################################################
#Du hast ein xts mit Preisen - (z.B. von )
#euro.indi= read.EckhardsXLS(xlsName="Index01.xls",startRow=5,belowColnames=5,debug=F)
#mach draus ein data-objekct
########################################################################################################
make.data<-function(prices,mkTrain=T,bench="DAX30", visual=F)
{
  data = new.env()
  data$symbolnames=colnames(prices)
  
  data$BENCH="DAX30"
  data$prices=prices
  data$weight = data$weights=data$prices;data$weight[]=0
  
  #bastel aus den kurs zeitreihen Close-Zeitreihen in data 
  no=lapply(colnames(data$prices), function(sym)
  {
    #sym="DAX30"
    #sym.val = merge(data$prices[,sym],data$prices[,sym])
    #colnames(sym.val)=c(sprintf("%s.Adjusted",sym),sprintf("%s.Close",sym))
    sym.val=price2dataPrice(data$prices[,sym],name=sym)
    assign(sym,sym.val,envi=data)
  })
  ls(data)
  
  #Target mit TargetMethod -1,0, oder 1 berechnen
  if (mkTrain)
    data$Target   <-compute.Target.by.method(data=data, TargetMethod=-1, w=30 , minReturn=0.15, min.duration=40,glaettung=5,visual=visual)
  data
}
####################################################################################################
beta.again.portfolio<-function(prices)
{
  #*****************************************************************
  # Compute Betas: b = cov(r,m) / var(m)
  # The betas are measured on a two-year rolling window
  # http://en.wikipedia.org/wiki/Beta_(finance)
  #****************************************************************** 
  # find month ends
  month.ends = endpoints(prices, 'months')
  
  prices = prices[month.ends,]
  n = ncol(prices)
  nperiods = nrow(prices)
  
  ret = prices / mlag(prices) - 1
  #next.month.ret = mlag(ret, -1)
  #ret = mlag(next.month.ret)
  beta = ret * NA
  
  # 1/n benchmark portfolio
  benchmark = ntop(ret, n)  
  benchmark.ret = rowSums(benchmark * ret, na.rm=T)
  
  # estimate betas
  for(t in 24:nperiods) {
    t.index = (t-23):t
    benchmark.var = var( benchmark.ret[t.index], na.rm=T )
    
    t.count = count(ret[t.index, ])
    t.cov = cov( ifna(ret[t.index,], 0), benchmark.ret[t.index], use='complete.obs' )
    
    beta[t,] = iif(t.count > 20, t.cov/benchmark.var, NA)
  }
  return(beta)
}
#########################################################################
rowMax<-function(tab)
{
  apply(tab,1,FUN=function(row) max(row,na.rm=T))
}
#########################################################################
retrain.event<-function(toDay, retrain.on,p)
{
  #dont_use_last   noch einbauen
  retrain.event =F
  
  if (retrain.on=="monthly")
  { 
    #abends, am Monatsende . haben wir die Close-Werte f?r die Berechnung der DataCloud und trainieren damit das modell nach.
    #die heutige positionierung von gestern abend, haben wir noch mit dem alten modell gemacht
    ends = Dates.month.ends(index(p)) #Dates.quarter.ends
  }
  else
    if (retrain.on=="quarterly")
    { 
      #abends, am Monatsende . haben wir die Close-Werte f?r die Berechnung der DataCloud und trainieren damit das modell nach.
      #die heutige positionierung von gestern abend, haben wir noch mit dem alten modell gemacht
      ends = Dates.quarter.ends(index(p)) #Dates.quarter.ends
    }
  else
    if (retrain.on=="yearly")
    { 
      #abends, am Monatsende . haben wir die Close-Werte f?r die Berechnung der DataCloud und trainieren damit das modell nach.
      #die heutige positionierung von gestern abend, haben wir noch mit dem alten modell gemacht
      ends = Dates.year.ends(as.Date(index(p))) #Dates.quarter.ends
      
    }
  else
    ends=as.Date(index(p))  #daily is default
  
  
  
  dont_use_last
  if (as.Date(toDay)  %in% ends )
    retrain.event =T  
  
  # if (retrain.event)  
  #    browser(mP("retrain.event"))
  retrain.event 
}
if (F)
{
  dax=data$prices$DAX30["2005"]
  retrain.event("2005-12-30","yearly",p)
}
#########################################################################
# beschr?nkt die Wert auf mi,ma
#########################################################################

cutInterval<-function(p,mi1=0,ma1=1)
{
  if (shape(p)==1)
  {  pi=p
     t1=ifelse(pi>ma1,ma1,pi)
     res=ifelse(t1<mi1,mi1,t1)
     return(res)
  }
  #    browser(mP(colnames(p)))
  no=foreach(col.i = 1:ncol(p)) %do%  { p[is.na(p[,col.i]),col.i]<-0 }
  # no=foreach(col.i = 1:ncol(p)) %do%  { p[is.infinite(p[,col.i]),col.i]<-0 }
  
  no=foreach(col.i = 1:ncol(p)) %do%  {
    pi=p[,col.i]
    t1=iif(pi>ma1,ma1,pi)
    res=iif(t1<mi1,mi1,t1)
  }
  res
}

#########################################################################

faber<-function(p,n=200) p-SMA(p,n)
#########################################################################

roll.Range<-function(p,N,return.type="itp",visual=T)
{
  indi=p[,1]
  range.N = runMax(indi,n=N) - runMin(indi,n=N)
  res = range.N/lag(indi,N)
  if (visual)
    mPlots(merge(indi,runMax(indi,n=N),runMin(indi,N)),res)
  
  if (return.type == "itp")
    res= in.Trend.pos(res,visual=visual,main="itp.faber",k=260,K=10)$itp
  if (visual)
    mPlots(p,res)
  
}
if (F)
  roll.Range(dax,30,visual=T)
###########################################################################
#wie oft ?ndert sich in den letzten 10   7 Tage- Paketen (wochen) die Marktrichtung
#Beispiel:   ist die  swapVola(data$DAX30,5,20,visual=T,return.type="itp") gering (<0) oder fallend
# ist die Chanse eine Trendfolge-Systems dass heute in Pos geht, relativ hoch, dass diese Pos auch
#n?chste Woche noch passt.
#bei einer hohen swapVola sollte er nicht mehr die pos wechseln - h?chstens noch flat gehen
###########################################################################
swapVola<-function(dax, tlen=7, win=10, visual=T, return.type="itp")
{
  p=dax[,1]
  dax.r = sign(mROC(p,tlen))
  #l?sche jeweils tlen-1 viele Tage  (so was wie monats-Tage - nur dass hier statt monat  tlen flexibel gegeben werden kann)
  temp=dax.r
  temp[]=sapply(1:shape(dax.r) ,function(i) (i %% tlen) +1)
  dax.r[]=iif(temp !=tlen,NA,dax.r)
  dax.r = na.omit(dax.r)
  d.dax.r = iif (dax.r != lag(dax.r,1),1,0)
  res = runSum(d.dax.r,win)
  res = EMA(d.dax.r,win)
  
  if (visual) 
    mPlots(p,res)
  
  if (return.type == "itp")
    res= in.Trend.pos(res,visual=visual,main="itp.faber")$itp
  if (visual)
    mPlots(p,res)
  res=m.ifna.prev(cbind(p,res))[,2]
  return (res)
}
if (F)
{
  swapVola(data$DAX30,1,31,visual=T)
  swapVola(data$DAX30,7,20,visual=T)
  swapVola(data$DAX30,30,5,visual=T)
}

#########################################################################
#wandel einen Vector in ein xts - das firstDate ist optional
#falls p2 gegeben ist will er dessen index-informationen nutzen
#allerdings müssen dann p und p2 gleich lang sein -
#ausser bei ingore.len.diff = T - wo ihm p2 dann nur das firstDate liefert
#

vec2Xts<-function(p,firstDate=Sys.Date(),p2=NULL,ignore.len.diff=F)
{
  if (!is.null(p2))
  {
    if (shape(p2) == shape(p) )
    {
      res=xts(coredata(p),index(p2))
      return(res)
    }
    if (shape(p2) != shape(p) && !ignore.len.diff)
    {
      sag("vec2xts  len(p) %d != len(p2) %d",shape(p),shape(p2), warte=F)
      browser()
      return(NULL)
    } 
    
    if ( shape(p2) != shape(p) && ignore.len.diff)
    {
      firstDate = as.Date(index(first(p2)))
    }
  }
  N=length(p)
  x=coredata(p)
  if (len(x)==1)
    xp=xts(x,firstDate)
  else
    xp=xts(x,firstDate+1:N)
}  
#######################################################################
# mache aus einem vector ein xts  - start.date ist optional 
m.xts<-function(vec, start.date=NULL)
{
  #  vec=as.vector(vec)
  if (is.null(start.date))
    start.date= Sys.Date()
  xts(vec, start.date+1:shape(vec) )
}

###########################################################################

#baue monatlich optimierte monats-forecasts mit
#Holtwinters, nnetar, VAR und naive
#und gib eine Tabelle mit den regressions-Winkeln der forecasts zur?ck

#dax=na.omit(data$prices[,"DAX30"])

###########################################################################
forecast.m <-function(dax)
{
  dax.m= m.to.monthly(dax)
  
  first.ti=round(356/31)  #(first.ti+10)
  visual=F
  sym=colnames(dax.m)
  
  all.m=lapply(first.ti:len(dax.m)  ,function(ti)
  {
    today=DateS(dax.m[ti])
    mP("%s %s",sym,today)
    x=tail(dax[sprintf("::%s",today)],500)  #mehr als 20 tage packt er nicht .. dann wird er horizontal
    
    if (visual) 
    {
      print(measures(x))
      #plot(x)
    }
    
    lambda <- BoxCox.lambda(na.contiguous(x))
    x <- BoxCox(x,lambda)
    tsx=ts(coredata(x),frequency=7)
    
    ##### Brauchbar
    fit = HoltWinters(tsx)  #sieht gut aus .. ist aber falsch
    #plot(fit)
    #plot(forecast(fit),include=100)
    fcs=forecast(fit,h=30)$mean
    m.HW=lm.FIT(vec2Xts(fcs))[2]*100
    if (visual )
    {
      plot(forecast(fit,h=30),main= sprintf("HW %f %s",m.HW,today))
      mP("HW %f",m.HW)
    }
    ##### Brauchbar
    fit <- nnetar(tsx)    #ein univar. neuronales netz
    #plot(lag(tsx,-1))
    #lines(fit$fitted,col="red")
    fcs=forecast(fit,h=30)$mean
    m.NN=coef(lm.FITm(vec2Xts(fcs),visual=F))[2]*100
    if(F&& visual)
    { 
      plot(forecast(fit,h=30),main=sprintf("NN %f",m.NN))
      mP("NN %f",m.NN)
    }
    
    #l?uft bei langen daten auch auf schrott  .. ist nicht besser wie ein dummer trend-folger
    #fit sieht aus wie einfach ein lag(dax,1)
    fit <- VAR(data.frame(x=1:len(x),y=coredata(x)),type="const",ic="AIC")
    #plot(fit)
    N=60
    #plot(predict(fit,n.ahead=N,ci=0.95))
    xp=xts(rep(0,N),as.Date(index(last(x)))+1:N)
    xp[]=predict(fit,n.ahead=N,ci=0.95)$fcst[[2]][,1]
    m.VAR = lm.FIT(xp)[2]*100
    if (F && visual)
    {
      plot(rbind(x[,1],xp),main=sprintf("VAR %f",m.VAR));amark(DateS(last(x)))
      mP("NN %f",m.VAR)
    }
    
    res =xts(data.frame(m.HW=m.HW,m.NN=m.NN,m.VAR=m.VAR),as.Date(today))
  })
  #ergebnisse in gesamt xts. packen
  all.m=foreach(xt = all.m,.combine="rbind") %do% {xt}
  naive= dax.m-lag(dax.m,1);colnames(naive)="naive"
  all.m=merge(all.m,naive)
  #soll-ist-vergleich
  soll=lag(dax.m,-1)-dax.m;   sig.soll = sign(soll)
  
  purePlot(merge(all.m,0,soll[sprintf("%s::%s",DateS(first(all.m)),DateS(last(all.m)))]))
  res=foreach(col = 1:ncol(all.m), .combine="cbind") %do%
{ 
  res= iif(sign(all.m[,col]) == sig.soll,1,-1)
}
#
plotSigPrice(signal=lag(res[,4],0),prices=dax.m)
plotSigPrice(signal=sign(soll),prices=dax.m)

#auch die summe der signale ist schrott
all.sig=res[,1];all.sig[]= sign(rowSums(res))
plotSigPrice(signal=sign(all.sig),prices=dax.m)

treffer.rate= colSums(res) / nrow(all.m)*100
treffer.rate
############### schon das trivial modell zlema hier ist besser :


return(res)
}

#########################################################################

doSharpeRatio <-function(r)
{
  res=as.numeric(SharpeRatio(r,FUN="StdDev")) 
}

#########################################################################
#geh durch alle spalten dex xts und nimm jeweils die range
#stell das ergebnis als View und chart da
#ausserdem:  checkt er auf inf-werte und listet die auf der console
#########################################################################
cloud.check<-function(cloud.mac,visual=F        )
{
  if (global_disable_cloud.check)
  {
    print("cloud.check disabled with global_disable_cloud.check")
    return (T)
  }
        
  if (is.null(cloud.mac))
  {
    print("cloud.check recieved NULL")
    return
  }
  arg.names <- as.character(substitute(cloud.mac)  )

  oaw=lapply(colnames(cloud.mac), function(col) { Col=cloud.mac[,col]; mP("%s %s",col,fromToS(na.omit(Col)))})
  
  oaw=lapply(colnames(cloud.mac), function(col) { Col=cloud.mac[,col]; print(tail(Col[is.infinite(Col) | is.na(Col)])); unlist(list(sym=col,infi=len(Col[is.infinite(Col)|is.na(Col)]),range=range(Col,na.rm=T),len=shape(Col)))})
  
  OAW=(t(data.frame(oaw)));rr=rownames(OAW)=OAW[,"sym"]
  OAW=OAW[,-1]
  
  #eine summen-zeile anh?nge
  OAW=rbind(OAW , MAXMIN=c(infi=max(as.numeric(OAW[,c(1)])), range1=min(as.numeric(OAW[,c(2)])) ,range2= max(as.numeric(matrix(OAW[,c(3)]))) ))
  OAW2=apply(OAW,2,as.numeric)
  rownames(OAW2)=c(rr,"MAXMIN")
  
  View(data.frame(OAW2),title="cloud_check")
  
  if (visual)
  {
  norm_Win(2)
  mchart(cloud.mac,main=arg.names)
  
  plot(matrix(OAW[,c(2,3)]),main= arg.names)
  }
  print(dim(cloud.mac))
  gesamt.xts=na.omit(cloud.mac)
  
  try(print(colnames(gesamt.xts)))
  print("--- common length: ")
  try(  print(fromToS(gesamt.xts)))
  
  sag("cloud.check( %s ): press key",as.character(arg.names),warte=T)
  summary(cloud.mac)
}
#########################################################################

col.rename<-function(p, newn="neu",pat=".%s")
{
  Pat=paste("%s",pat,sep="")
  
  colnames(p)<-sapply(colnames(p), function(sym)sprintf(Pat,sym,newn))
  p
}
#########################################################################

listget<-function(item,lis)
{
  if (item %in% names(lis))  #is.element(subv, v)
    return(lis[[item]])
  return(F)
}
if (F)
{
  c1=list(a=T,b=F)
  listget("a",c1)
}

#########################################################################
#split a vector into chunks
#http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r?rq=1
chunk <- function(x, n) split(x, sort(rank(x) %% n))
#########################################################################
if (F)
{x=rep(100:1)
 rank(x)
 chunk(x,20)
}
#########################################################################
#wandelt den Inhalt einer List in einen doku-text
########################################################################
list.info<-function(experi,name="")
{  
  expName=  deparse(substitute(experi))  
  if (name!="")
    expName=name
  info=  paste(unlist( 
    append(append(expName,
                  append(as.character(Sys.time()),          
                         lapply(names(experi),function(nam) 
                         {
                           X=experi[[nam]]
                           ifelse (is.character(X),sprintf("%s=%s",nam,X),sprintf("%s=%s",nam, deparse(substitute(X))))
                         }))  ),  experi$INFO) ),sep="\n")
}

if (F)
{
  info=  list.info(experiment$macro.select.m )
  cat(info,file="outfile.txt",sep="\n")
}

list.Info<-function(fit)
{
  lapply(names(fit),function(x){print(fit[[x]]);sag("warte %s",x,warte=T)})
}

#verhinder den 0-Durchgan eine zeitreihe y  und damit den div0-bug bei ROC und mNorm
preScale<-function(y)
{
  y=y-min(y,na.rm=T)+1   #das w?r die L?sung - blos ist das ein zukunfts-vorgriff - sobald neue minima hinzukommen verformt das die ganze Zeitreihe
} 

#Another way to remove the effect of a possible lurking variable from the correlation of two other variables is to calculate a partial correlation. There is no partial correlation function in R. So to create one, copy and paste this script into the R Console...

### Partial correlation coefficient
### From formulas in Sheskin, 3e
### a,b=variables to be correlated, c=variable to be partialled out of both
pcor = function(a,b,c)
{
  (cor(a,b)-cor(a,c)*cor(b,c))/sqrt((1-cor(a,c)^2)*(1-cor(b,c)^2))
}
##########################################################################
### schreibe ein paar period-ends  nach data

data.periods<-  function(data)  #noLog
{
  prices=na.omit(data$prices)
  #*****************************************************************
  # Compute monthly returns
  #****************************************************************** 
  period.ends = endpoints(prices, 'months')
  period.ends = unique(c(1, period.ends[period.ends > 0]))
  
  data$m.ends <-period.ends
  
  #*****************************************************************
  # Compute weekly returns
  #****************************************************************** 
  period.ends = endpoints(prices, 'weeks')
  period.ends = unique(c(1, period.ends[period.ends > 0]))
  
  data$w.ends <-period.ends
  
  data
}

#######################################################################################
#suche in allen data$prices nach leads zu p
#wenn Du welche findest gib die gelagget Datamatrix zur?ck
#######################################################################################
HotLags2.CC <-function(p=dax,features =data$prices,visual=F)
{
  #  browser(mP("hl"))
  RES = NULL  
  for(fi in 1:ncol(features))  
  {
    feature=features[,fi]
    all.leads=list()
    HL=HotLags2.cc(p,feature,visual=visual)
    sym=colnames(feature)
  
    if (visual)
      {print(sym)
    print(HL$lag)
    }
    if ( len(HL$lag)>0)  
    {
     if (visual)
       {print("########################################")
      mP("%s %s ",colnames(p),colnames(feature) )
      print(HL$lag)
     }
      if (is.null(RES))
        RES=HL$lead.dat[,-1]
      else
        RES=merge(RES,HL$lead.dat[,-1])
      all.leads[[sym]] = HL$lag
    }
    if (visual && !is.null(RES))
      print(head(RES))
  }
  if (visual)
    print(colnames(RES))
  RES 
}
#........................
if (F) 
{
  x=  HotLags2.CC(p=data$prices[,1],features =data$prices)
  ls(x)
  
  x=  HotLags2.CC(p=data$prices[,1],features =data$prices[,"SUP500"])
  ls(x)
  
  x=  HotLags2.cc(data$prices[,1],data$prices[,"SUP500"])
}
#########################################################################
#transformiere eine als factor-level gegeben wert nach numeric
factor2val<-function(fact)
{
  fact.val=as.numeric(levels(fact)[fact])    
}
#########################################################################
## gibts die listen-variable arg ? .. erweitert die contains - funktionalität
#########################################################################
has.old<-function(liste,arg,what=NULL){
  if (len(liste)==0 || len(arg)==0)
    return (F)
  if (contains(names(liste),arg) && len(liste[[arg]])>0)
    if(is.null(what))  return(T) #has.1
  else
    if (len(spl(liste[[arg]]))>1)
      return (contains(spl(liste[[arg]]),what))
  if (len(spl(liste[[arg]])==1) && len(unlist(str_split(what,"*")))==2)  #siehe #has.2
    return( contains(unlist(str_split(liste[[arg]],"*")),what)) #siehe #has.3
  else
    return (contains(spl(liste[[arg]]),what)) #has.4
  return(F)
}

sval<-function(val)
{
  
  if(is.null(val)) return("")
  return(as.character(val))
}


has<-function(liste,arg=NULL,what=NULL,state=NULL){
  
  checkState<-function(state, words,what)
  {
    if (is.null(state))  return(contains(spl(liste[[arg]]),what))  #wie bei has.old# has.4
    for(word in words)
    {
      lword=""
      rword=word
      split =  unlist(str_split(word,"#"))
      if (len(split)>1 )
      {
        lword=trim(split[1])
        rword=trim(split[2])
        if(trim(what)==trim(rword))
        {
          if (contains(unlist(str_split(lword,"*")),trim(state)))
            return(T)
          return(F)
        }
      }
      if (rword==what)
        return (T)
    }
    return(F)
  }
  
  if (len(global_arg$R_arg) >0)
    return(global_arg$R_arg)  #overruling 
  
  if (len(liste)==0 && len(arg)==0)  #2014
    return (T)
  
  mP("has() arg: %s, what: %s at state:%s ",sval(arg),sval(what),sval(state))
  
  if (contains(names(liste),arg) && len(liste[[arg]])>0)
    if(is.null(what))  return(T) #has.1  der Inhalt ist der Abfrage eganl
  else
    if (len(spl(liste[[arg]]))>1)  #dem item wurden mehrere worte zugeordnet
      return ( checkState(state,spl(liste[[arg]]),what)) #has.s4
  #wenn einem listen-Eintrag nur ein einzelnes Wort zugeornet wurde, darf auch auf
  #Einzelbuchstaben-Bestandteile diese Wortes gecheckt werden
  if ((is.list(liste) || is.vector(liste)) && is.null(names(liste)))  #neu 3.4.2014
  {
    if (arg %in% liste)
      return(T)
    else
      return(F)
  }
  if (len(spl(liste[[arg]])==1) && len(unlist(str_split(what,"*")))==2)  #siehe #has.2
    return( contains(unlist(str_split(liste[[arg]],"*")),what)) #siehe #has.3
  else
    return (checkState(state,spl(liste[[arg]]),what)) #has.s4
  return(F)
}


if (F)
{
  R.arg=list(tsa.level="TSA",reporting.level=c("one,transactions"))
  
  has(R.arg,"reporting.level") #has.1   gibts die liste-Variable reporting.level
  has(R.arg,"reporting.level","one")   #has.2    wurde ihr der Werte  one zugewiesen
  has(R.arg,"tsa.level","A")   #has.3   enthält der tsa.level  das Zeichen A
  has(R.arg,"tsa.level","TSA") #has.4   entspricht der tsa.level  dem Wort  TSA
  
  #im state "Timing-Lauf" soll anders reported werden, als im Selektion-Lauf.
  R.arg=list(tsa.level="TSA", reporting.level=c("TS#xlsWriteSignals","SA#xlsWriteTransactions"))
  has(R.arg,"reporting.level","xlsWriteTransactions", state="A")  #dabei ist state z.B.  S
  #führt zu:  
  has(R.arg,"report","turnover",state="T")
}

#if (has(S.arg,"nTop.q"))
#########################################################################
## Prices sei eine monatszeitreihe,  lineal sei ein xts p mit tagestaen,
## raus kommt Prices auf tagesdaten - für alle datümer von p gefüllt

m.to.timescale<-function(Prices, lineal)
{
  b=  merge(lineal,Prices);
  p=b[,-1]
  p=bt.apply.matrix( p, m.ifna.prev)
}



########################################################################################
print("########### load InputConfig_Portfolio_TD4.R")

if (F)
  list_R_functions('MLib/InputConfig_Portfolio_TD4.R')
