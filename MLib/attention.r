########################################################################################
#Attention
#siehe  show.attention bei #MM_ATTENTION
#Beispiele ganz unten

#Vorsicht:   Schwellen die mit Q als Quantil gewählt werden, müssen eigentlich via
#rollapplyr ermittelt werden .... sie sind in Realität KEINE GERADEN  .. also use.rollQ
#daber können wie für Handeslsmodelle so noch nicht genommen werden
#########################################################################################


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
level.price <-function(prices)
{
  levels=prices
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
level.vola<-function(prices)
{
  ret.log = na.omit(bt.apply.matrix(prices, ROC, type='continuous'))
  hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)  
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
level.angelDif<-function(prices)
{
  mSlope90 <<- rollRegressionXTS(na.omit(mNorm2(prices)),win=90)
  mSlope30 <<-SMA(rollRegressionXTS(na.omit(mNorm2(prices)),win=10),n=10)
  res=(mSlope30-mSlope90) #unbedingt, na.omit machen - sonst kommt kein xts sondern ein geshifteter vektor
  colnames(res)<-"diffangle"
  layout(c(1,2,3))
  purePlot(na.omit(mNorm(prices)),plotX=F,main="mSlope30-mSlope(90)")  
  purePlot(na.omit(merge(mSlope30,mSlope90)),plotX=F,col="green")
  purePlot(na.omit(res),plotX=T,col="blue")
  
  return(res)
}

level.faber<-function(prices,vers=0,winLen=200,diff=10)
{
  dynamic=NULL
  mP("faber vers %d winLen %d diff %d",vers,winLen,diff)
  browser()
  p=mNorm(prices);sym=colnames(prices)
  sma200 =SMA(na.omit(p),n=winLen)
  
  layout(c(1,1,2,3))
  
  if (vers==0)
  {
  
    faber=p-sma200
    signal=iif(faber > 0,1,0)
    
    #purePlot(na.omit(merge(p,sma200)),plotX=F,main="sma20",HighLight=faber[faber>0])  
    #plotSigPrice(signal=signal,prices=p)  
  }

  if (vers==2)
  {
    sam200=mFaberIndi(p,10,"months") 
    
    faber=p-sma200
    signal=iif(faber > 0,1,0)
    
  } 

  if (vers==1)#########################################
{
  
  rm= SMA(runMax(p,n=diff),winLen)
  ri= SMA(runMin(p,n=diff),winLen)
  
  rm= runMax(EMA(p,winLen),n=diff)  #oder doch SMA?
  ri= runMin(EMA(p,winLen),n=diff)
  
  dynamic=rm-ri 
  
  dynamic=dynamic+12*(abs(dynamic-lag(dynamic,5)))  #breite des Abstands zwischen high,low und  ob die kurven auch ne steigung haben  ....
  
  #last(dynamic)
  faber = iif(p >= rm, p-rm, ifelse(p <= ri,p-ri , p-sma200))
  
  purePlot(na.omit(merge(p,sma200,rm,ri)),plotX=F,main=sprintf("level.faber %s",sym),HighLight=faber[faber>0])  
  faber[dynamic < 0.02] <-0     #dynamic ist so was wie die Trendstärke !!!!
 #browser(mP("xxx"))
#  faber[na.omit(ROC(na.omit(dynamic)))] <-0     #dynamic ist so was wie die Trendstärke !!!!
  #plot(ROC(dynamic))
  
  #signal=iif(faber > 0,1, 0)
  #plotSigPrice(signal=signal,prices=p)  
  
}
  purePlot(na.omit(faber),plotX=F,col="green",HighLight=faber[faber>0])
  if (!is.null(dynamic))
    purePlot(na.omit(merge(p,dynamic,sma200,rm,ri))[,2],plotX=T,col="blue",HighLight=dynamic[dynamic>0.02])
  
  #browser( mP("press- key"))
  
  return(faber)
  
}  #M1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (F)  #standard-faber  #GUTESMODEL
  problems=  show.attention(data$prices[,"SDF.DE"],fn="level.faber",Thresh1=0,fn.signal="signal.faber.v0",vers=0,winLen=200)
if (F)  #faber vers1 - variante  #GUTESMODEL auf DAX 
  problems=  show.attention(data$prices, fn="level.faber",Thresh1=0,fn.signal="signal.faber.v0",vers=1,winLen=200,diff=50)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

level.rsi <-function(prices,vers=0,winLen=200)
{
  mP("RSI %d",winLen)
  
  #levels = RSI(mNorm(prices),n=winLen) 
  levels=RSI(EMA(mNorm(prices),n=winLen),n=2)
}
#~~~~~~~~~~~~~~~~~~~~~~~BUG~~~~~~~~~~~~~~~~~~~~~~~~~
#  schade:  dynamische aufrufe gehen so nicht
level.TTR <-function(prices,ttr=SMA,winLen=200)
{
  mP("TTR %s %d",ttr,winLen)
  browser()
  #trr.fn=match.fun(ttr)
  #  eval(parse(sprintf("%s(prices,n=%d)",ttr,winLen)))
  
  #EMA(prices,n=winLen)
  levels=ttr.fn(prices,n=winLen)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
level.SMA <-function(prices,winLen)
{
  mP("SMAs %d",winLen)
  #browser()
  
  levels=SMA(prices,n=winLen)
  purePlot(merge(prices,levels))
  levels
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

diff.minmaSMA <-function(prices,winLen,diff=10) #soll Seitwärtsbewegungen finden
{
  mP("minmaSMAs %d",winLen)
  #browser()
  
  sma=SMA(prices,n=winLen)
  fi=first(sma[!is.na(sma)])
  sma[is.na(sma)]<-fi
  
  level=EMA(runMax(sma,n=diff)-runMin(sma,n=diff),9)
  
  layout(c(1,2,2))
  purePlot(merge(prices,sma))
  purePlot(level)
  
  level
}


if (F)  #zeige Phasen mit Seitwärtsbewegung
  problems=  show.attention(data$prices,fn="diff.minmaSMA",Q="37%",fn.signal="signal.standard",fak=1, winLen=32,diff=11)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
level.SMMA <-function(prices,winLen,diff=11)
{
  mP("SMAs %d",winLen)
  
  p=prices
  
  #oberer (auf Max, und unterer SMA)
  rm= SMA(runMax(p,n=diff),winLen)
  ri= SMA(runMin(p,n=diff),winLen)
  
  #wie viele Kurse stehen oberhalb, wie viele unterhalb ?
  rm.c= merge(p,sign(p[p>ri]))[,2];rm.c[is.na(rm.c)]<-0;  rm.c = SMA( rm.c,diff  )
  ri.c= merge(p,sign(p[p<rm]))[,2];ri.c[is.na(ri.c)]<-0;  ri.c = SMA( ri.c,diff  )
  
  
  layout(c(1,2,3))
  purePlot(merge(prices,rm,ri))
  purePlot(merge(rm.c,ri.c),HighLight = p[rm.c>ri.c])
  purePlot((rm.c-ri.c),HighLight = p[rm.c>ri.c])
  
  #levels = rm-ri  #V1
  levels = rm.c-ri.c  #differenz aus beiden zeigt obs rauf oder runter geht
  #browser()
}

if (F)  #zeige Phasen mit Seitwärtsbewegung
  #GUTESTMODEL
  problems=  show.attention(data$prices,fn="level.SMMA",Thresh1=0,fn.signal="signal.standard",fak=1, winLen=200,diff=6)

if (F)  #wie ein wochentrader -  Future-System (lebt wohl davon, dass für das Q-Level zukünftige Daten zur Verfügung stehen ?)  wenn in level.SMMA steht:  #V1
  problems=  show.attention(data$prices,fn="level.SMMA",Q="15%",Lag=1,fn.signal="signal.standard",fak=1, winLen=200,diff=6)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#default signal-generator:  überall da wo HighLight ist, ist das signal1, sonst -1
signal.standard<-function(price,indi,HighLight) #~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  signal=price;signal[]=0; signal[as.Date(index(HighLight))]=1
  signal
}


signal.faber.v0<-function(price,indi,HighLight)
{
  #browser(mP("signal.faber.v0"))
  signal=price;signal[]=0; signal[as.Date(index(HighLight))]=1
  return(signal)
}




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

signal.faber.v1<-function(price,indi,HighLight)  #leider Schrott
{
  #browser(mP("signal.faber.v1"))
  
  signal=price;signal[]=0; 
  T.indi=indi[as.Date(index(HighLight))]
  I.indi=T.indi[,1]
  P=price[as.Date(index(HighLight))]
  #hysterese:
  H=iif(I.indi>T.indi[,2],1, ifelse(I.indi<T.indi[,3],-1,0)) 
  #  signal[as.Date(index(HighLight))]=H
  signal=H
  return(signal)
}
if (F)  #standard-faber  #GUTESMODEL
  problems=  show.attention(data$prices,fn="level.faber",Thresh1=0,fn.signal="signal.faber.v1",vers=0,winLen=200)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

signal.rsi<-function(price,indi,HighLight)
{
  browser(mP("signal.rsi"))
  
  signal=price;signal[]=0; 
  T.indi=indi[as.Date(index(HighLight))]
  mchart(T.indi)
  #P=price[as.Date(index(HighLight))]
  #hysterese:  Wenn oberhalb der Rsi=80 -Linie (T.indi[,1]) dann short, sonst long... 
  #index: 1 der Rsi, 2 obere schwelle, 3 untereschwelle
  H=iif(T.indi[,1]>T.indi[,2],-1, ifelse(T.indi[,1]<T.indi[,3],1,0)) 
  #  signal[as.Date(index(HighLight))]=H
  #plot(H)
  signal=H
  return(signal)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

signal.vola<-function(price,indi,HighLight)
{
  (mP("signal.vola"))
#browser()  
  signal=price;signal[]=0; 
  P=price
  #hysterese:  Wenn oberhalb der Rsi=80 -Linie (T.indi[,1]) dann short, sonst long... 
  #rsi= iif(RSI(P,2)>80,-1,ifelse(RSI(P,2)<20,1)
  #mean reverting
  #rsi= iif(RSI(P) < 50, 1, 0)
  faber = iif(P> SMA(P,200),1,0)
  #index: 1 der Vola, 2 obere schwelle, 3 untereschwelle
  #hohe vola dann flat, sonst: sehr niedrige vola dann rsi, sonst faber 
  #browser()
  signal=iif(indi[,1]>indi[,2],0, ifelse(indi[,1]<indi[,3], faber , faber)) 
  #  signal[as.Date(index(HighLight))]=H
  #plot(H)
  
  return(signal)
}

if (F)
{
  #GUTESMODEL für die dax-titel
  problems=  show.attention(data$prices,fn="level.vola",fn.signal="signal.vola",Q="69%",use.rollQ=T)
  
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### hilft zum beheben von heftigen sprüngen die manchmal in den yahoo-daten sind (vergessene splits???)

signal.sprung.repair<-function(price,indi,HighLight)
{
  signal=iif(indi[,1]>indi[,2],1, ifelse(indi[,1]<indi[,3], 1, 0)) 
  sym=colnames(price)[1]
  newPrice_ <<- data$prices[,sym]
  
  #addiere den price.jump auf alle Folge-Werte 
  repair<-function(bug,price)
  {
    sym=colnames(HighLight)[1]
    mP(". %s",sym)
    bugDate.i = get.Index(price,DateS(bug))
    price.jump=   nval(price[bugDate.i])-nval(price[bugDate.i - 1])
    #price[bugDate.i]-price.jump 
    newPrice_[c(bugDate.i:shape(price))] <<- coredata(newPrice_[c(bugDate.i:shape(price))])-price.jump
    res=1
  }
  #browser(mP("repair 1"))
  #iteriere über allen aufgetretenend sprünge
  rollapplyr(signal[signal>0],width=1,FUN=repair,price=price)
  #die reparierten Kurs stehen nun in newPrice_
  
  #plot(price,main=sym);
  #lines(newPrice_,col="blue");
  
  #plot(data$prices[,sym],main=sym,ylim=range(merge(data$prices[,sym],newPrice_),na.rm=T))
  #lines(newPrice_,col="blue")
  purePlot(merge(data$prices[,sym],newPrice_),main=sym,HighLight=signal[signal>0])
  
  mP("repair this %s ? (y/...)",sym)
  
  inp=scan(n=1,what="")
  if (inp=="Y" || inp== "y")
  {
    mP("i repair for you %s",sym)
    #browser()
    if (F)
    {   
      colnames(data$prices[,sym])
      colnames(newPrice_)
      shape(data$prices[,sym])
      #  shape(price)
      shape(newPrice_)
    }
    coredata(data$prices[,sym])=coredata(newPrice_)
    plot(data$prices[,sym],main = sprintf("%s repaired",sym))
    #browser(mP("done  press key"))
  }
  #shape(data$prices)
  #dim(newPrice_)
  #die eigentliche Reparatur des externen Datenbestandes
  
  #Hier kommt eine Gui-Nachfrage ob ich wirklich die Daten überschreiben soll
  
  if (F)
    res=confirmDialog("repair this ?", handler = function(h,...) {
      mP("i overwrite  data$prices %s  ",sym)
      coredata(data$prices[,sym])<<-coredata(newPrice_)
      ## In this instance dispose finds its parent window and closes it
      dispose(h$obj)
    })
  
  
  return(NULL)
}
if (F)  #repariere augenfällige Preissprünge - nach Rückfrage beim User
  problems=  show.attention(data$prices,fn="level.price",fn.signal="signal.sprung.repair",fak=6,Lag=5,Q="99%")





################################################################################################################################
#quantile brav pro Tag im - rollenden Fenster gerechnet
#
# ein schönes Template für rollapplyr(prices,win=1,allPrices,maxWin=300, ...) .. liefert Werte vom ersten Tag, das Fenster dabei täglich 
#breite bis es die maxbreite maxWin erreicht hat.
#
roll.quantile_<-function(old_prices, allPrices, maxWin=600, Q="95%")
{
  sym=colnames(old_prices)
  if (is.null(sym))
    sym=1
  Prices=allPrices[,sym]
  lastDay = DateS(last(old_prices))
  firstPriceDate.i=max(1,get.Index(allPrices,DateS(last(old_prices)))-maxWin)
  firstPriceDate = DateS(allPrices[firstPriceDate.i])

  # if(lastDay== DateS(last(allPrices)))
  #   browser(mP("roll.quantile"))
  
  abs.diffs.k=Prices[sprintf("%s::%s",firstPriceDate, DateS(last(old_prices))), sym]  #das mit jedem schritt wachsenden Preis-fenster
  quantile(abs.diffs.k,probs=seq(0,1,0.01),na.rm=T)[Q]

  
  
}
roll.quantile<-cmpfun(roll.quantile_)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#gib eine Lise der Auffälligkeiten zurück:  len(problems) == dim(data$prices)[2]
# eigentlich gedacht Preissprünge (wenn Lag != 0) zu finden,  kann aber auch level-übersteigungen (Lag==0)  über dem fak*Q-Qunatil-Nievau (wenn Q) hinaus
#für beliebige level-funktionen anzeigen (fn)
#wenn last = T wird nur dann ein Problem angezeigt, wenn es am letzetn Tag von prices stattfindet.

#damit läßt sich show.attention dann auch im rollapplyr-Fenster anwenden
#wenn fak != 0 werden nur abs(diffs.k) betrachtet und symmetrische Schwellen Thresh1,Thresh2 berechnet
#wenn fak == 0,  werden mit Thresh1 die Quantile der Max, und mit Thresh2 die Quantile der Min - Werte berechnet 
#Wenn Thresh1 oder Thresh2 gegeben sind werden die benutzt, sonst muss das Quantile Q gegeben #sein, und die Schwellen werden automatisch berechnet

#Die problem-Fälle (auf die attentions die Aufmerksamkeit längt) sind gegeben mit
#>Thresh1 | < Thresh2
#also Werte die das T1,T2- Intervall über, unterschreiten      
#problems=diffs.k[diffs.k>Thresh1 | diffs.k<Thresh2]

# statt nur einfache preise einzugeben können diese (z.B.  mit TTR - wie fn()  verändert werden)
# aus price, indi(Highlight, Thresh1,Thresh2), ... kann mit fn.signal() auch ein signal berechnet werden, dass dann mit plotSigPrice() ausgewertet wird
############################################################################
#MM_ATTENTION
m.qnorm<-function(Q="99%")
{
  if (str_sub(Q,str_length(Q),str_length(Q))=="%")
  nval(leftOf(Q,what="%"))/100
  else (nval(Q)/100)
}

show.attention<-function(prices,fn="level.price",fak=0,Lag=NA,Q="99%",last=F,visual=T,Thresh1=NA,Thresh2=NA,fn.signal="signal.standard",use.rollQ=T,...)
{
  #diffs=na.omit(abs(mNorm2(prices)-lag(mNorm2(prices),k=Lag)))
  jump.fn=match.fun(fn)
  library(foreach)
  oThresh1=Thresh1
  oThresh2=Thresh2
  
  res= foreach(k= 1:ncol(prices),.combine="cbind") %do%
{
  Thresh1=oThresh1
  Thresh2=oThresh2
  #browser()
  p=na.omit(prices[,k])
  sym=colnames(p)[1]
  #diffs.k=na.omit(diffs[,k])
  #browser()
  levels=jump.fn(p,...)[,1]  #aufrufe der funktion <<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if (is.na(Lag))
    mode="level"
  else
    mode = "diff"
  
  
  if (mode == "diff")
  {
    #Y=mNorm(levels)
    #diffs.k=abs(Y)-lag(Y,k=Lag) 
    diffs.k=ROC(levels,n=Lag)
  }
  else
    diffs.k=levels
  
  mP("check  %s - mode %s fak %d ",sym,mode,fak) #.......................
  #browser()
  #Q-mode
  if (is.na(Thresh1) && !is.na(Q))  #hol Dir die Aufmerksamkeitsschwelle aus den Quantilen
  {
    mP("Q-mode")
   
    if (fak >0)  #wenn fak != 0 werden nur abs(diffs.k) betrachtet und symmetrische Schwellen Thresh1,Thresh2 berechnet
    {
      mP("symmetrisch")
  # 
      abs.diffs.k= abs(diffs.k)
      
      #qunatile nur einmal gerechnet
      if (!use.rollQ)        
       normDifs=quantile(abs.diffs.k,probs=seq(0,1,0.01),na.rm=T)[Q]
      else
      #quantile brav pro Tag im - rollenden Fenster gerechnet
#      normDifs=rollapplyr(abs.diffs.k,1, roll.quantile, allPrices=abs.diffs.k,maxWin=430,Q=Q ) 
        normDifs=  runquantile(abs.diffs.k, k=430, align="right",probs=m.qnorm(Q))
      
      #shape(normDifs)
      #shape(diffs.k)
      problems=diffs.k[abs(diffs.k)>fak*nval(normDifs)]
      Thresh1 = abs(fak*nval(normDifs))
      Thresh2 = -abs(fak*nval(normDifs))
      #browser()
    }
    else  #wenn fak == 0,  werden mit Thresh1 die Quantile der Max, und mit Thresh2 die Quantile der Min - Werte berechnet 
    {
      mP("asymmetrisch")
      rm=runMax(diffs.k,n=20)
      plot(diffs.k);lines(rm,col="red")
      #browser()
      if (!use.rollQ)        
         normDifs=quantile(rm,probs=seq(0,1,0.01),na.rm=T)[Q]
      else
        #quantile brav pro Tag im - rollenden Fenster gerechnet
      #  normDifs=rollapplyr(rm,1, roll.quantile, allPrices=rm,maxWin=430,Q=Q )
      normDifs=  runquantile(rm, k=430, align="right",probs=m.qnorm(Q))
      
      
      #normDifs=rollapplyr(rm,1, roll.quantile, allPrices=rm,maxWin=400030,Q=Q )
      #browser()
      Thresh1 = (nval(normDifs))  
      
      rm=runMin(diffs.k,n=20)
      lines(rm,col="green")
      
      if (!use.rollQ)        
        normDifs=quantile(-rm,probs=seq(0,1,0.01),na.rm=T)[Q]
      else
        #quantile brav pro Tag im - rollenden Fenster gerechnet
        #normDifs=rollapplyr(-rm,1, roll.quantile, allPrices=-rm,maxWin=430,Q=Q )
      normDifs=  runquantile(-rm, k=430, align="right",probs=m.qnorm(Q))
      
      Thresh2 = -(nval(normDifs))
      #Werte die das T1,T2- Intervall über, unterschreiten      
      problems=diffs.k[diffs.k>Thresh1 | diffs.k<Thresh2]
      
      #Werte die im Intervall liegen
      #problems=diffs.k[diffs.k>Thresh2 | diffs.k<Thresh1]
      #browser(mP("min-max"))
    }
    # mchart(merge(diffs.k,problems,Thresh1,Thresh2));#lines(Thresh1);lines(Thresh2)
    #browser(mP("Q-mode"))
  }
  else  #wenigstens Thresh1 oder Thresh2 gegeben
  {
    mP("Thresh given - ignore Q")
    Q=NA
    if (!is.na(Thresh2))
      problems=diffs.k[diffs.k>nval(Thresh1) | diffs.k<nval(Thresh2) ]
    else
      if (!is.na(Thresh1))
        problems=diffs.k[diffs.k>nval(Thresh1)]
  }
  #  if (colnames(prices[,k])=="SDF.DE")
     #  browser()
  if (shape(problems)>0 ) #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    showTrades=T
    # browser(mP("......."))
    HighLight=p[as.Date(index(problems))]
    #HighLight = data$prices["2011",k]
    layout(c(1,1,2))
    #browser()
    #der erste Plot mit preis und highlight
    purePlot(p,HighLight=HighLight,plotX=F,main=sprintf("%s %s: %s",mode,fn,colnames(p)))
    if (F &&!is.na(Q))
      indi=(merge(diffs.k,fak*nval(normDifs),-fak*nval(normDifs)))
    else
    {
      if (shape(Thresh1)>1 || !is.na(Thresh1)) 
      {
        #showTrades=T
        indi = diffs.k; temp=indi; temp[]=Thresh1; 
        if (shape(Thresh2)>1 || !is.na(Thresh2))
          temp=merge(temp,Thresh2)
        indi = merge(indi,temp)
      } #schwelle und indi
    }
    indi[is.infinite(indi)]<-NA
    temp.indi=merge(p[,1],indi);indi=temp.indi[,-1] #x-ausrichtung an p
    
    #der 2. Plot mit dem Indikator
    purePlot(indi,HighLight=HighLight,plotX=T,col="blue")
    
    
    if (showTrades) #~~~~~ noch ein optionaler plotSigPrice() ~~~~~~~~~~~~~~
    {
      # browser(mP("showTrades"))
      if (fn.signal != "")  #spezifisch übergeben signal-methode
      {
        signal.fn=match.fun(fn.signal)
        signal=signal.fn(p, indi, HighLight) 
        if (!is.null(signal))
          {
          if (visual)plotSigPrice(signal=signal,prices=p,indi=list(signal=indi))
          problems=list(rank=diffs.k,signal=signal)  #signal wird zurückgebeben
        }
      }
      
    }
if (visual)    browser(mP("next sym  -  press key"))
    
    #browser() 
    #plota(na.omit(data$prices[,k]),type="n",main=colnames(problems)[1],plotX=F,x.highlight=HighLight,col="red")
    #plot(na.omit(data$prices[,k]))
    #plota(diffs[,k],x.highlight=HighLight,col="blue")
    
  }
  return(problems) 
}
  res
}

############################# Anwendungsbeispiele #####################################

if (F)
{
  #volatiler markt  
  problems=  show.attention(data$prices,fn="level.vola",fak=1,Q="80%")
  #Zeige Datenstörungen (tages sprünge in den kursen)
  #MM_TODO - damit dann automatisch die Fehler beheben (nach dem Dateneinladen)
  problems=  show.attention(data$prices,fn="level.price",fak=6,Lag=1,Q="99%")
  #leading:  veränderung der markt - volatilty  
  problems=  show.attention(data$prices,fn="level.vola",fn.signal="signal.vola",Q="69%")
  #wann waren starke preisschankungen
  problems=  show.attention(data$prices,fn="level.price",fak=1,Lag=5,Q="99%")
  #wann weicht der kurze Reg-Winkel stark vom lange ab ?, markiert steile Up-Trendwechsel
  problems=  show.attention(data$prices,fn="level.angelDif",fak=1,Q="99%")
  #faber
  #markiert beschleunigung einer marktphase, damit die völlige übertreibung und das nahe Ende
  #sobald die Beschleunigung endet - da sollt man auf der richtigen Seite sein
  problems=  show.attention(data$prices,fn="level.faber",fak=1.2,Q="85%")
  
  #data$prices = data.info(data)
  
  purePlot(mNorm(data$prices[,"ALV.DE"]))
  problems=  show.attention(data$prices[,"SX86BP"],fn="level.faber",fak=1.2,Q="95%")
  
  problems=  show.attention(data$prices[,"SX86BP"],fn="level.faber",fak=1.2,Thresh1=0)
  
  #standard-faber-modell
  problems=  show.attention(data$prices,fn="level.faber",Thresh1=0,fn.signal="signal.faber.v0",vers=0,winLen=200)
  #vers1-faber
  problems=  show.attention(data$prices,fn="level.faber",Thresh1=0.12,Thresh2=-0.12,fn.signal="signal.faber.v1",vers=1,winLen=201)
  
  problems=  show.attention(data$prices,fn="level.faber",Lag=1,fak=1,Q="99%",fn.signal="")
  
  #RSi-Modelle
  problems=  show.attention(data$prices,fn="level.rsi",Thresh1=80,Thresh2=20,fn.signal="signal.rsi",vers=0,winLen=12)
  problems=  show.attention(data$prices,fn="level.rsi",Q="40%",fn.signal="signal.rsi",vers=0,winLen=22)
  
  #repariere augenfällige Preissprünge - nach Rückfrage beim User
  problems=  show.attention(data$prices,fn="level.price",fn.signal="signal.sprung.repair",fak=6,Lag=5,Q="99%")
  
  #markiere Seitwärtsbewegungen - wo nem Faber dann auch nicht zu trauen ist
  problems=  show.attention(data$prices,fn="level.minmaSMA",Lag=5,Q="50%",fn.signal="",winLen=200)
}

##########################################################################################

#geht nicht auf
signal.Faber.base__ <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  
  signal = iif(p > sma200,1,0)
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
  
}


#------ Aufruf der level.faber ..Attentiuon-Methode so, das es auch durch indi.generic() läuft
#und man damit die GesamtPortfolio-Perf ablesen kann
signal.attention <-function(arg, par = mlist( sma.w=c(200,120,220,20), diff=c(200,120,220,20),vers=c(1,1,2,1)),visual=F,...)
{
  p= mNorm(arg$clos)
  winlen=as.integer(par$sma.w)
  diff=as.integer(par$diff)
  vers=as.integer(par$vers)
  
 # signal=  show.attention(p, fn="level.faber",Thresh1=0,fn.signal="signal.faber.v0",vers=vers,winLen=winlen,diff=diff,visual=F)
  att=  show.attention(p,fn="level.vola",fn.signal="signal.vola",Q="89%",visual=F)
 signal=att$signal
 return(list(Signal=signal, Indi=list(signal),rank=att$rank))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (F)
{
  ls(data)
  global_arg<<-list(clos=prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices))
  globalTrainLevel <<-10   
  global_objectId <<-paste("TREND","xx","signal.lm") 
  
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1)
  
  x=indi.Generic("signal.attention", global_arg,visual=T, TRAINSYM =data$BENCH)
  
  TrainIndicator( opti="GRID", indiName = "signal.lm",  visual=T, TRAINSYM = "DAX")
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
  
  ####################
}

#write.csv(data$prices,"MDATA/daxPrices.csv")
if (F) ####################################################################################
{  
  prices=data.info(data)
  colnames(prices)
  ls(data)
  head(data[["DBK.DE"]])
  colnames(prices)
  
  
  data=read.csv("Mdata/DaxPrices.csv")
  assign("symbolnames",s,envir=data)
  
  data$symbolnames=sapply(data$symbolnames, FUN=function(sym) 
  { new.sym = unlist(strsplit(sym,".DE")[1])})
  
  
  sapply(data$symbolnames, FUN=function(sym) {
    new.sym = unlist(strsplit(sym,".DE")[1])
    try({
      assign(new.sym,data[[sym]],envir=data)
      txt=sprintf("rm('%s',envir=data)",sym)
      eval(parse(text=txt))})
  })
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

######################################################################################
print("########### load attention.R")


