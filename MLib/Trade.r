#schau bei #MM_GENERIC
# http://www.rseek.org/
#Marken    
   #MRANK

####################################################################################
#beschreibung technischer indikatoren
#Quelle: http://www.broker-test.de/finanzwissen/technische-analyse/volatilitaet/  
#####################################################################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))

options(warn=1)

options(error = browser)

#library(debug)
#mtrace(f)

######################################################################################

######################################################################################

if (F)
{
  library('doRedis')
  registerDoRedis('jobs')
  startLocalWorkers(n=6, queue='jobs')
  foreach(icount(10),.combine=sum,.multicombine=TRUE,.inorder=FALSE) %dopar%
    4*sum((runif(1000000)^2 + runif(1000000)^2)<1)/10000000
  
  removeQueue('jobs')
}
######################################################################################
###################################################################################### 

GMMA <- function(x) {
  fastMA <- c(3,5,8,10,12,15)
  slowMA <- c(30,35,40,45,50,60)
  x <- sapply(c(fastMA,slowMA),   function(xx) EMA(x,xx))
  return(x)
}

if (F)
{
  require(quantmod)
  require(PerformanceAnalytics)
  
  # Step 1: Get the data
  getSymbols("^GDAXI")
  GSPC=GDAXI
  # Step 2: Create your indicator
  dvi <- DVI(Cl(GSPC))
  
  # Step 3: Construct your trading rule
  sig <- Lag(ifelse(dvi$DVI < 0.5, 1, -1))
  
  # Step 4: The trading rules/equity curve
  ret <- na.omit(mROC(Cl(GSPC))*sig)
  ret <- ret['2005-06-02/2010-09-07']
  
  eq <-mRendite(ret)
  eq <- exp(cumsum(ret))
  plot(eq)
  
  # Step 5: Evaluate strategy performance
  table.Drawdowns(ret, top=10)
  table.DownsideRisk(ret)
  charts.PerformanceSummary(ret)
  
  #########################################################################
  # welcher Return w?rde in R ab Zeitpunkt t gemacht, wenn ich einen maxdrawDown
  # von maxdd in pct hinzunehmen bereit w?r
  #########################################################################
  library(PerformanceAnalytics)
  
  t="2005"
  
  
  
  
  Return.excess(prices, Rf=0)
  
  cumprod(1+na.omit(R))-1
  
  ## Not run: 
  require(tseries)
  prices = as.xts(get.hist.quote("^GDAXI", start = "1999-01-01", quote = "AdjClose", compression = "d"))
  ## End(Not run)
  R = Return.calculate(prices, method="simple")
  
  tail(R)
  cumprod(R[-1]+1)-1
  Return.cumulative(R, geometric = T)
  
  plot(R)
  lines(R,col="green")
  lines(as.zoo(mR),col="red")
  class(R)
  head(mR)
  head(R)
  mR = mROC(as.xts(prices))
  
  str(R.IBM)
  R.IBM = as.xts(R.IBM)
  colnames(R.IBM)="IBM"
  
  x=chart.CumReturns(as.xts(R.IBM),legend.loc="topleft", main="Cumulative Daily Returns for IBM")
  chart.BarVaR(rR)
  
  chart.Drawdown(R)
  chart.VaRSensitivity(R.IBM)
  
  apply.rolling(mR, FUN="mean", width=36)
  
  chart.BarVaR(as.xts(mR), show.clean=TRUE, clean="boudt", lwd=2, methods="ModifiedVaR")
}
#GEMM(Dax)
#####################################################################################
# dispatcher T1-Modell gem. parameter mode
#####################################################################################

#portfolio.returns = weight %*% t(ia$hist.returns)  
#x = compute.drawdowns(portfolio.equity)

#for( i in 1:nrow(weight) ) {  
#  portfolio.equity = cumprod(1 + portfolio.returns[i,])


aTest<-function()
{
  #load.IFO()  #- schreibti die IFO.csv- Reihe
  ta5 <<- T2$new(PortfolioName = "ta5", visual = F ) 
  
  ######################################################################################
  # drawdown-analyse
  #######################################################################################
  
  dax=ta5$mydata$prices["2005::","Dax"]
  prices = dax
  
  undebug(goodReturn)
  goodReturn(prices=dax,  maxdd=5,t= "2009-08-01",visual=T)
  dax
  
  ######################################################################################
  # rolling-window - mit ZigZag-Studie
  #######################################################################################
  
  dat = "2007-01-01"
  for (i in seq(1:700))
  {
    dat = addTime(dat,1,"week")
    f=sprintf("::%s",dat)
    x=ZigZag(prices[f],change=10,percent =T)
    plot(prices[f])
    lines(x,col="blue",lwd=2)
    sag("klick",T)
  }
  
  ######################################################################################
  # die Positionen der ZigZag-Wende-Punkte
  #######################################################################################
  
  oldprices = prices
  prices=oldprices
  #prices=mNorm(prices["2005/2012"])
  thresh= 5  #zigzag-empfindlichkeit
  chartSeries(prices)
  addZigZag(prices[,c("High","Low")],change=thresh,percent =T) 
  
  #####################################################################################
  ################## rolling-window-zigzag-analysisi ##################################
  #####################################################################################
  dat0 = "2006-01-01"
  dat = "2007-01-01"
  dat="2012-01-01"
  step = "week"
  
  for (i in seq(1:700))
  {
    dat0 = addTime(dat0,1,step)
    dat = addTime(dat,1,step)
    f=sprintf("%s/%s",dat0,dat)
    prices = oldprices[f]
    prices=mNorm(prices)
    thresh= 15  #zigzag-empfindlichkeit
    x=na.omit(ZigZag(prices,change=thresh,percent =T,retrace=F))
    
    sig=as.vector(coredata(sign(x-lag(x))))  #segmente positiver und negativer steigung
    enc <-rle(sig)
    len(enc$lengths )# ist die Anzahl der Trades
    
    enc.endidx <- cumsum(enc$lengths)  #ending indices
    enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(prices))  # starting indices
    zzPoints = prices[enc.startidx]
    
    par(mfrow=c(1,1))
    plot(prices,main=sprintf("ZZ-Analysis %s",dat))
    lines(zzPoints,col="blue",lwd=2)
    
    y=prices
    x=time(prices)
    
    plot(x, y,type="l",main=dat,lwd=2);    grid()
    points(x,y)
    abline(lm(y ~ x), lty = 3, col = "blue")
    Span=0.07
    points(loess.smooth(x, y,span=Span, control = loess.control(surface = "direct")), lty = 6, col = "red",lwd=2)
    lines(loess.smooth(x, y,span=Span, control = loess.control(surface = "direct")), lty = 6, col = "blue",lwd=2)
    lines(lowess(x, y,f=Span), lty = 6, col = "green",lwd=2)
    
    
    Span=0.3
    points(loess.smooth(x, y,span=Span, control = loess.control(surface = "direct"),method="model.frame"), lty = 6, col = "yellow",lwd=2)
    lines(loess.smooth(x, y,span=Span, control = loess.control(surface = "direct")), lty = 6, col = "blue",lwd=2)
    lines(lowess(x, y,f=Span), lty = 6, col = "green",lwd=2)
    
    points(zoo(zzPoints),col="magenta",lwd=4)
    lines(zoo(zzPoints),col="magenta")
    sag("wart",T)
    
    #  plot(prices)
    #  holt()
    #  lines(as.xts(HoltWinters(as.ts(prices),gamma=F,beta=F)$fitted),col="red")
    #markiere die higss- und lows- des zigzag in unterschiedlichen farben
    highx = na.omit(enc.endidx[enc$values > 0 ]) 
    highs = prices[highx] 
    
    
    lowx = na.omit(enc.endidx[enc$values < 0 ]) 
    lows = prices[lowx] 
    
    lines(na.omit(Highs),col="red",lwd=2)
    lines(na.omit(Lows), col="green",lwd=2)
    
    points(highs,col="red",lwd=2)  
    points(lows,col="green",lwd=2)
    
    lmHigh=lm(highs~index(highs))
    lmHigh$coefficients
    plot(lmHigh)
    
    #############<<<<<<<<<####################################################
    #
    len(zzPoints)
    #prices=dax
    ##### k?nnen die zzPoints aus den Preisen erkl?rt werden
    len(prices)
    zzPoints2 = prices; zzPoints2[,1]=NA; zzPoints2=merge(zzPoints2,zzPoints)[,2]
    ln1 = lm(zzPoints2 ~ prices)
    summary(ln1)
    predict(ln1)[1:5]
    ###########################################################
    
    #k?nnen die Highs aus den  preisen erkl?rt werden ?    
    len(Highs);len(zzPoints)
    Highs=prices;  Highs[,1]=NA;   Highs = merge(Highs,highs)[,2]
    Lows= prices ; Lows[,1]=NA;    Lows = merge(Lows,lows)[,2]
    ln2 = lm(Highs ~ prices )
    summary(ln2)
    predict(ln2)[1:5]
    ############################################################
    
    Highs=zzPoints;  Highs[,1]=NA;   Highs = merge(Highs,highs)[,2]
    Lows= zzPoints ; Lows[,1]=NA;    Lows = merge(Lows,lows)[,2]
    
    
    lmHighZ=lm(Highs~zzPoints)
    summary(lmHighZ)
    
    lmLowZ=lm(zzPoints~Lows)
    summary(lmLowZ)
    lmHighLowZ=lm(zzPoints~ Highs+Lows)
    summary(lmHighLowZ)
    len(prices)
    len(zzPoints)
    
    zFac=len(prices) /len(zzPoints)#eine funktion von thresh
    
    histogram(coredata(highs),nint=15)
    histogram(coredata(lows),nint=15)
    histogram(coredata(zzPoints),nint=15)
    histogram(coredata(prices),nint=15)
    
    ##################################################################################
    # def seitw?rtsbewegung  - zur generierung von trainignsdaten f?r die Indikatioren
    ##################################################################################
    # wenn der abstand zweier highs ht und ht-1  zueinander geringer ist als der Abstand 
    # ht-lowt  
    dhigh = mROC(highs)*100 
    dlow = mROC(lows)*100
    dzz = mROC(zzPoints)*100 
    
    histogram(coredata(dhigh),nint=10)
    histogram(coredata(dlow),nint=nrow(dlow))
    histogram(coredata(dzz),nint=10)
    #zum Vergleich 
    
    mi=cbind(prices,prices) ; mi[,1:2]=0
    for(i in  1:nrow(dhigh))
    { 
      t =time(dhigh)[i]
      #t="2007-06-01"
      #  mP("%s %f %f ",t , dhigh[i], dzz[t])
      #    v=abs ( dzz[t,1]) - abs(dhigh[t,1])
      v= sign(dhigh[t,1])  ##/ abs ( dzz[t,1])  #kleine Werte heisse:   horizontalbewegung
      #sag(sprintf("%s %f",t,v) ,T)
      if(length(v)<1) v=NA
      mi[t,1] =  as.numeric(v)    
      mi[t,2] =  "H"
    }
    for(i in  1:nrow(dlow))
    { 
      t =time(dlow)[i]
      #t="2007-06-01"
      # mP("%s %f %f ",t , dlow[i], dzz[t])
      #   v=abs ( dzz[t,1]) - abs(dhigh[t,1])
      v= sign(dlow[t,1])  ##/ abs ( dzz[t,1])  #kleine Werte heisse:   horizontalbewegung
      #sag(sprintf("%s %f",t,v) ,T)
      if(length(v)<1) v=NA
      mi[t,1] =  as.numeric(v)
      mi[t,2] = "L"
    }
    
    Mi = mi[mi[,2] != "0"]
    
    
    #verleiche immer ein HL-Paar und schau ob du eine dispari?t findest:
    #homogen wenn:   wenn Hz steigt, steigt auch Lz - 
    # auch interessant:  laufen beide in die gleiche Richtung wie bisher ?
    #alle Mi-Punkte wo sich Mi von Mi-1 unterscheidet - also alle Disparit?spunkte
    dispari = Mi [ Mi[,1] != lag(Mi[,1] )]    #Mi[,2]=="H" &&
    dispari[dispari[,2]=="L"]
    
    len(Mi);len(dispari)
    Mi
    dispari
    #checke von den Betr?gen (dl/dh ) ob es sich um eine ernsthafte dispari?t hanbdelt - oder 
    #filter gleich alle raus wo abs(dhigh) und abs(lag(dlow)) in summe sehr klein sind
    #evtl. sind diese dispari dann Kr?mmungsindikatoren und taugen als trend-ende -/abfang Indikatoren
    
    
    #############  resistance-Stellen - sind solche wo die linreg- durch fast horizontal ist 
    # ein hohes R2 herrscht (also die werte sehr eng am resistor liegen)
    ####  resitance-lines werden-nach identifikation oft noch mal getestet - und wenn sie dann halten
    # fungieren sie als reflektor.
    
    
    if (F)
      mi=  as.xts(sapply(time(dhigh),FUN=function(t){ 
        v=abs( dzz[t]) - abs(dhigh[t])
        if (length(v)<1)v = NA
        return(v)}), order.by= time(dhigh))
    
    #TODO   hohe Werte des mi-Indikators sprechen f?r eine Seitw?rtsbewegung
    
    #TODO Trading-Strategie:
    #Kr?mmungsindikator:    ,,,,,,,,,,,
    #Der Trend ist intakt so lange das high ?ber dem letzen High liegt && das Low ?ber dem lezten Low
    #wenn eins von beiden nicht mehr stimmt:  Ausstieg (damit ist das lezte Low zugleich trailing-Stop)
    #TODo  Widerstandsakkumulatoren
    
    par(new=TRUE)
    
    plot(mi,main=NA,ylim=range(na.omit(mi)))
    #lines(scaleTo(mi,range(prices)),col="purple")
    
    sag("klick",T)
  }
  
  
  
  mi[mi !=0]
  
  #Target-Segmentierer d?rfen auch nach vorne schauen !!!
  
  
  
  plot(mi)
  sum(mi)
  dhigh
  TTR
  
  ################## abklappern der segmente .. z.B. f?r linReg 
  ###### wenn bei der summer der linreg-Segmente R2 zu schlecht wird, muss  thres um einen punkt verringert
  #werden ....
  # --> neuer Test -... so lange bis passendes  thresh (Aufl?sung der ZigZag- gefunden wurde) ###
  ################################################################################################
  
  clos=prices
  for (i in 1:len(enc$lengths))
  {
    print (i)
    i=15
    starti = enc.startidx[i]
    endi = enc.endidx[i]
    tlen = endi-starti
    startTrade = index(clos)[starti]    
    endTrade = index(clos)[endi]
    pos = enc$values[i]
    dret=mROC(clos[starti:endi])
  }
  
  ############################## IFO DATEN ############################################
  
  ta5
  ifo=ta5$mydata$prices["2005::","IFO"]
  
  ifom = to.monthly(ifo)[,1]
  plot(ifom)
  ifom= ifom-min(ifom)+1
  
  plot(mNorm(ifom))
  
  
  plot(mNorm2(ifom))
  difom=mRendite(mROC(ifom))
  plot(difom)
  head(ifo)
  
  ####################################################################################
  
  
  MplotNormedPrices(ta5$ret)
  
  ifo=mNorm(ta5$mydata$prices["2005::","IFO"])  
  rex=mNorm(ta5$mydata$prices["2005::","Rex"])
  dax = mNorm(ta5$mydata$prices["2005::","Dax"])
  sg2r= mNorm(ta5$mydata$prices["2005::","sg2r"])
  sv2r= mNorm(ta5$mydata$prices["2005::","sv2r"])
  usdeur = mNorm(ta5$mydata$prices["2005::","USDEUR"])
  
  mPlot(mNorm(ta5$mydata$prices))
  mPlot(dax,rex,sg2r,ifo,usdeur)
  mPlot(dax,rex,sg2r)
  
  difo=mROC(ifo)
  
  ddax=mROC(dax)
  drex=mROC(rex)
  drex["2005-01"]
  
  signal=lag(SMA(mROC(dax),20)>SMA(mROC(rex),20))
  g=guv(pRet=ddax,pBenchRet=drex,signal=signal,visible=T)
  plot(mRendite(drex))
  
  mPlot(dax,rex, signal=lag(SMA(mROC(dax),20)>SMA(mROC(rex),20))) 
  mPlot(dax,rex, signal=signal)
  
  #ifo plot 
  #ifo=ta5$getOrgData("IFO",visual =F)["2002::","ifo.Low"]
  
  plot(ifo)
  
  #dax =mNorm (Cl(ta5$mydata[["Dax"]]))
  #ddax = mROC(Cl(ta5$mydata[["Dax"]]))
  ddax = mROC(dax)
  ylim=range(ifo,dax)
  plot( dax,ylim=ylim)
  lines(ifo,col="green")
  
  difom = to.monthly( difo)[,1] #original - Reihe
  Difom=data.frame(difom)
  head(difom)
  visual=F
  opt = data.frame(cbind(i=0,sharpe=0))
  
  
  for (i in 1:50)
  {
    i=1
    signal =na.omit(lag(iif(SMA(sign(difom),i)>0,1,0)  ))
    #falls der ifo am Monatsersten schon kommt, kann man ohne lag arbeiten - dann mit i=2 und besserem Ergebnis
    i=2
    signal =na.omit(iif(SMA(sign(difom),i)>0,1,0)  )
    
    #signal =iif(SMA(sign(difom),i)>=2/i,1,0)
    #signal =sign(iif(SMA(difom,i)>0,1,0))
    
    signal=mto.daily(signal)
    #head(signal)
    guv1= na.omit(merge(ddax,signal))
    guv1=cbind(guv1,ddax*signal)
    
    colnames(guv1)=c("ddax","ifoSignal","guv")
    #head(guv1,50)
    ret = cumsum(guv1)
    
    if (visual)
    {
      mPlot(guv1)
      lines(mNorm(dax))   
      plot( zoo(ret))
    }
    sh=sharpe (as.ts(na.omit( ret[,3])))
    cat( i, "  ",sh)
    rr=c(i=i,sharpe=sh)
    opt=rbind(opt, rr)
  }
  #i=2 ist der beste Wert 
  
  max(opt[,2])
  
  GuVr = mRendite(na.omit(guv1))
  
  guv( ddax, difo,signal,NULL,visible=T,indi=NULL)
  
  len(ddax)
  len(signal)
  #iif( SMA(mROC(ifo),60)> 0,1,0)
  x=mROC(ifo)
  X=data.frame(x)
  SMA(na.omit(x))
  head(mROC(ifo))
  ta5$show()
  ta5$bench
  
  #global_ParTable=NULL  #l?sche das Trainingsged?chtnis
  global_StartDate = 1
  
  heute=Sys.Date()
  heute = fromTo(ta5$mydata$prices)[2]
  heute=as.Date(heute)-2
  ta5$update(heute) #schreibt auch schon mal in den global_ParTable und liefert damit dem TrainIndicator via mlist() notwendige Startwerte
  global_ParTable
  global_StartDate
  #trace(TrainIndicator)
  
  TrainIndicator(heute)
  #trace(mlist)
  global_ParTable
  
  ls(ta5$member)
  ls(ta5$mydata)
  
  head(ta5$mydata[["IFO"]])
  
  x=ta5$getData("IFO",visual =T)
  x=ta5$getOrgData("IFO",visual =F)["ifo.Low"]
  plot(x)
  head(x)
  Ti<<-ta5$member[["IFO"]]
  str(Ti)
  
  Titem = Ti$item
  head(Titem$prices)
  data<<-ta5$mydata
  Ti=gTi; ret = gret;data = gdata
  Ti$name
  
}


###########################################################
# transformiert einen mlist()-Parameter in eine Arg-Vector f?r DEoptim
# ml ist eine Liste von listen/vektoren
#par2Vec gibt einen vector zur?ck der jeweils das erste Element 
#dieser listen enth?lt 
#das Gegenteil von mkArgList()
###########################################################
par2Vec<-function(ml,n=1)
{
  r= sapply(ml,FUN=function(x)as.double(x[n]))
  
  return( as.vector(r))
}
#####################################################################################
# Rekursion:     Portfolios als t1-Objekte:
#  Baue aus der guv-Rendite-Kurve eine t1-Zeitreihe
#####################################################################################

dataFrameOfGuv<-function(Ti)
{
  cat("\ndataFrameOfGuv")
  guvDf= data.frame(matrix(nrow=1, ncol=4))
  #TEST
  print("muss evtl.  Ti$Name heissen !!! ") 
  browser()
  tn =Ti$Name
  colnames(guvDf)=   c( sprintf("%s.Open",tn),sprintf("%s.High",tn),sprintf("%s.Low",tn),sprintf("%s.Close",tn))
  return(guvDf)
}

#debug(Trade1)
#####################################################################################
# Rufe die Indicatoren f?r Trend, Swing, Switch und das TrainlingStop-System
######################################################################################
#BUG: Dax in RenditePlus hat gar kein TrendModell !!!!

Trade1<-function(Ti, Ttype="T1", frame="",data,ret)
{
  #browser()
  #cprices sind Preise
  gdata<<-data
  gret<<-ret
  gTi<<-Ti
  
  #Ti=Ti_$item
  name = Ti$name
  clos = getCol(data$prices, name)
  if (length(clos) == 0)
  {
    cat("\nTrade1 of ",name," scip  because no price is given" )
    return("scipped")
  }
  
  #global_StartDate<<-fromTo(clos)[1]
  
  global_objectId <<- paste("TREND",name,Ti$t1Par$TrendIndi) #versuch eines unique names
  global_Ti <<-Ti
  
  arg =  list( name = Ti$name, bench=Ti$bench, Ti= Ti, clos=clos, dat=data, dclos=ret) #mal ein vollst?ndiges environment
  global_arg <<-arg
  
  ########### update des trend-Models #############
  
  trendIndi = sprintf("indi.%s(arg)",Ti$t1Par$TrendIndi)  
  
  mP("Trade1# update des trend-Models: %s ",trendIndi)
  #browser()
  if (toString(Ti$t1Par$TrendIndi)=="NA")
    mP("####### No TrendModel defined #####")  
  else    
    eval(parse(text=trendIndi)) 
  ########### update des seitw?rts-Models #############  
  swingIndi = sprintf("indi.%s(arg)",Ti$t1Par$SwingIndi)
  ########### update des RegimeSwitch-Models #############   
  tsswitch  = sprintf("indi.%s(arg)",Ti$t1Par$TsSwitch)
  
  
  #t1=switch(model,
  #          buyhold=  c(Cgar= last(cprices,1), Ret=mReturn(cprices)),
  #          meanreverting = t1_meanreverting(cprices,r,type, params)           
  #          )
  
  
  # Trade1.Plot(cprices, t1,model)
  #RET<<-data.frame(t1$Ret)
  #SIGNAL<<-data.frame(t1$Sig)
  #cat("\n  Trade:  ",name," Cgar ", t1$Cgar)
  return(arg)
}

#undebug(mlist)
######################################################################################
#  Indikator Parameter trainieren
#
#setzt voraus dass definiert sind:
#global_objectId," ",global_StartDate)
#und dass bereits ein parameter-set im global_ParTable liegt (der dann mit mlist()  abgerufen wird)
# also nur, dass schon mal ein normaler lauf stattgefunden hat 
#kann kann er die namen der Parameter dem globa_ParTable entnehmen und mit
#mkArgList daraus eine  vector->argList - Transformation machen 
#und damit kann train.Indicator als generische wrapper f?r DEoptim eingesetzt werden
#welches nur Funktionen optimieren kann, deren argumente als vector ?bergeben werden
######################################################################################


#undebug(mlist) 
#undebug(indi.Omega)
#indi.Omega(arg=c(3,3,9))
#####################################################################################
# vector->argList
#das Gegenteil von par2Vec
#####################################################################################

mkArgList <-function(parvec)
{
  parvec = unlist(parvec)
  #  print("mkArgList") 
  s="r1=list("
  i=0
  for (ai in unlist(global_Args)) #die Namen der ini-Parameter-Variablen
  { 
    i=i+1
    if (i > 1) s=paste(s,"",sep=",")
    
    s <- tryCatch({
      res = sprintf( "%s %s=%f",s,ai, parvec[i])   #round()  die von DEoptim gelieferten vector-werte f?r die parameter
      # << #####################
    }, error = function(err) {
      
      mP("Bug at mkArgList")
      browser()
      return(NULL)     }
    ,finally =function()
      return(res))
    
    
  }   #TODO  ich mach hier immer  round() obwohl DEOptim  real-zahlen liefert...
  s=paste(s,")")
  
  eval(parse(text=s))   #expression(r1=list( wShort=30.000000, wShort=160.000000 ))
  #  browser()
  #print(s)
  return(r1)
  return(list(width=parvec[1], minLevel=parvec[2]))
}
######################################################################################
#setzt voraus dass definiert sind:
#global_objectId," ",global_StartDate))
# ist dann ein generischer Aufruf eines bliebigen Indikators-Parametersatzes
#das ist die Wrapper Funktion für jeden Optimierer die parameter nimmt,  ein Workhorse aufruft und einen Güteparameter zurückgibt
######################################################################################
train.Indicator_<-function (parvec)
{
  par=mkArgList(parvec)
  
  
  #TODO  um overfitting zu vermeiden (eigentlich ist das ein regions-mittelwert um einen parameter-punkt)
  #TODO wahl ob ?ber viele aktien gleichzeitig gefittet wird ...
  #TODO wahl des Train-Zeitraums -
  #TODO ob  mehrfach - ?ber unterschiedliche Start-Zeiten im Trainings-Zeitraum gefittet werden soll
  
  #sma.short = bt.apply.matrix(prices, SMA, 50)  und dann die mean von ret zur?ckgeben
  
  #  TODO global_arg
  #  was mach ich hier ???
  #  str(global_arg)
  #  head(global_arg,1)
  
  # global_frame="1905-01-01/01-01-2011"
  #  (global_arg$dclos)["2006-01-01/2011-01-01"] 
  
  #ret=indi.MACD(global_arg, par )
  #generisch machen--
  #browser() 
  
  this_arg=global_arg
  this_arg$clos = global_arg$clos[global_frame]
  
  si= strfind(global_indiName,"signal.")
  if (len(si)>0)
    cmd=sprintf("ret=(indi.Generic('%s', this_arg,par=par))",global_indiName) #, TRAINSYM=1
  else
    cmd=sprintf("ret=(%s(this_arg,parg=par))",global_indiName)
  
  # browser()
  eval(parse( text=cmd))
  
  #mP("train.indicator()")
  #browser()
  
  if (len(ret)>1)
    ret = as.double(ret$Tquality)
  else
    ret = as.double(ret)
  
  
  if (ret > maxRet)
  {
    maxRet<<-ret
    bestPars <<-par
  }
  
  #browser()
  # DEoptim sucht MINIMA (nicht Maxima) .. drum  minus !
  mP("   %f <<<<<<< ",-ret)
  return(-ret)
}

train.Indicator<-cmpfun(train.Indicator_) #compilier das Teil


######################################################################################
######################################################################################

indi.BuyHold<-function(  arg,  par=list(),visual = F,main="")
{
  dclos=arg$dclos
  
  print("############## indi.BuyHold #############")
  
  signal = rep(1,len(dclos))
  indicator=BuyHold<-signal
  
  return(indi.finish(signal,arg$clos,indi="indi.BuyHold",visual=visual, Indi=list(),main=main))
  
}
######################################################################################
######################################################################################
if (F)
  ta5$update()

#####################################################################################
#t1-Trading von mean-revertern
#####################################################################################
t1_meanreverting<-function(cprices, r,type, params)
{
  # Mean-Reversion(MR) strategy - mit RSI
  
  rsi2 = bt.apply.matrix(cprices, RSI, 2)
  dcp = mReturn(cprices)
  
  Signal =  iif(rsi2 < 50, 1, -1)
  #  Signal =  iif( rsi2 <50 , -1, -1)
  #Signal[,1]=rep(-1, nrow(Signal))
  
  ret  =    cumsum(na.omit(Lag(Signal,1)) * dcp)
  #ret = cumsum(-1*dcp)
  t1 =list(Cgar= last(ret,1), Ret=ret, Sig = Signal)
  
  return(t1)
}

#####################################################################################
#Trenderkennender Regime-Switcher 
# Modelle die ihn subscriben schaltet er aus - und erh?lt als Return die gleichgewichtete
#Summe der Subscriber-Modelle 
#type1 arbeitet mit der Vola als Detektor
#####################################################################################

t2_regimeSwitch<-function(cprices, r,type, params)
{
  ret.log = bt.apply.matrix(cprices, ROC, type='continuous')
  hist.vol = bt.apply.matrix(ret.log, runSD, n = 21)
  vol.rank = percent.rank(SMA(percent.rank(hist.vol, 252), 21), 250)
  
  # Regime Switching  Historical
  data$weight[] = NA
  data$weight[] = iif(vol.rank > 0.5, 
                      iif(rsi2 < 50, 1, -1),
                      iif(sma.short > sma.long, 1, -1)
  )
  
  
  t1 =list(Cgar= last(cumsum(cprices),1), Ret=cprices)
  print(str(t1))
  return(t1)
}

#####################################################################################
# aus sysinvestor
#####################################################################################

# Avoiding severe draw downs
# http://engineering-returns.com/2010/07/26/rotational-trading-system/
# Only trade the system when the index is either above the 200 MA or 30 MA


if (F)  ##### wonder
{
  slopes=diff(prices,20)
  
  zlem1= bt.apply.matrix((sign(slopes)), ZLEMA, n=2,ratio=0.9); # 4
  zlem2= bt.apply.matrix((sign(slopes)), ZLEMA, n=10,ratio=0.1); #6
  zlem3= bt.apply.matrix((sign(slopes)), ZLEMA, n=200); #6
  
  signal=iif(zlem1 >=0,1,0)
  signal= iif(abs(slopes)< 1,0,signal)
  plotSigPrice(signal=signal,prices=prices,indi=list(zlem,slopes))  
  
  mzlem=sys.Run(prices,signal,compare=T)
  
  
  strategy.performance.snapshoot(mzlem, T)
  plotbt.custom.report.part1(mzlem)       
  compareViewModels(list(mzlem=mzlem,mzlem2=mzlem2),prices,alloc=T)
  pdf_Report(models,experiment)
  
}
####################################################################################
if (F)
{
  data.Info(data)
  prices=data$prices["2001::",13]
  
  data$prices=data.info(data)
  prices = mNorm(data$prices[,c("Dax","Dow")])  #mNormCleaned()
  mchart(prices)
  global_Targets <<-compute.Targets(prices,0.16,0.05)
  
  frame="2008::2010"
  price = prices[frame,1]
  target = global_Targets[frame,1]
  
  plot(price)
  lines(price,col=(target+3),type="h")
  
  global_arg$clos= prices
  res=signal.wonder(global_arg,list( k=20, zlemaN=60),visual=T)   # 
  signals = res$Signal
}

####################################################################
# vergleiche global_Targets und signals miteinandner
# zum Tag today,    ema- gewichtet
# hiermit wird die Güte des signals durch eine Vergleich mit dem Soll-Target
# verglichen.
####################################################################
evalTargetQuality<-function(prices,signals,today = NULL)
{
  firstD = DateS(first(prices))
  if (is.null(today))
    today= as.Date(index(last((prices))))
  
  res=prices
  res[] = NA
  #für jedes sym in prices:
  
  for(sym in colnames(prices))
  {
    #sym= colnames(prices)[1]  #TEST
    m.ps= merge(global_Targets[,sym], signals[,sym])
    check = m.ps[sprintf("%s::%s",firstD,today)]
    #nun vergleiche die beiden spalten in check auf identische Werte
    
    #vergleiche ob der Wert von global_Targets mit dem von signal übereinstimmt
    comp=iif(check[,1]==check[,2],1,0)
    res= prices[,sym];res[]=NA
    #zähle die übereinstimmungen der letzten winLen-Tage
    winLen = 10
    res[winLen:dim(res)[1],sym]=rollsumr(comp,k=winLen)
    #EMA-gewichtet...
    res[,sym]=EMA(res[,sym],n=winLen)
    
  }
  return(res)
}
if (F)
  evalTargetQuality(prices,res$Signal)
##################################################################################################################
# jedes trainierbare indi.<...>()-System endet mti return(indi.finish()).
# Dabei wird eine Liste aus GuV und Signal zurückgegeben.
# Somit kommuniziert indi.finish() mit dem Wrapper train.indicator()  der von TrainIndicator() gerufen im Opitimierungslauf
# gerufen wird.   indi.finish() kann mit plotSigPrices sehr gut ein Trading-System visualisieren.
# Ist die globale  global_xMarker mit einer Liste von Datümer gefüllt werden diese sogar als grüne Linien angezeigt.
###################################################################################################################


indi.finish<-function(signal,prices,indi="???",par=list(),visual=F,Indi=list(),main="")
{ 
  runN<<-runN+1
  sig <- signal
  res  = list()
  res$signal = signal
  if (!exists("global_TrainStyle"))
    global_TrainStyle<<-"GuV" #default-Wert
  if (!exists("global_xMarker"))
    global_xMarker<<- NULL
  
  #if (global_TrainStyle == "Target") 
  mP("indi.finish")
  if (dim(prices)[2]>1)
  {mP("wrong usage:  all indi.<...> methods are uni-variate")
   browser()
  }
  ret <- m.Run(prices,sig)
  nt=  numTrades(sig)$allT
  q= quality(ret)
  
  Transaktionskosten =nt/ncol(prices)/80
  ret[is.na(ret)]=first(ret[!is.na(ret)])
  
  S=compute.sharpe(ret)
  #Transaktionskosten =Transaktionskosten *10.0  #sonst viel zu hoch im optimizer
  mP("%s %s %d:   %d Trades- >q:%f =>%f || %f  ~%f~", indi, toString(par), runN,nt,q,Transaktionskosten,q-Transaktionskosten,S)
  
  #parameter-names im title
  p=sapply(names(mlist()),FUN=function(nam) {sprintf("%s=%s",nam,toString(first(mlist()[[nam]])))})
  title=paste(p,collapse=",")
  title = paste(main,title,sep=": ", toString(as.Date(index(last(prices)))))
  
  if (visual)    
    plotSigPrice(signal=signal,prices=prices,indi=Indi,xMarker=global_xMarker,main=title)  
  
  #compute.sharpe(mROC(prices))/compute.sharpe(ret)
  
  #browser()
  #return (compute.sharpe(ret))
  res$Tquality= q-Transaktionskosten 
  
  return(res )
}
#################################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   MM_Entwicklung 28.5.2013
#Vorbereitung des Trainings
if (F)   #neues Trainings-Schema.  Das     indi.Generic() -> signal.<...>   -> sys.Run()  - Entwurfsmuster 
  #löst das indi.<> -> indi.finish() Muster ab
{
  global_arg<<-list(clos=data$prices[,1:2])  #Definition der Preise
  global_ParTable <<-NULL  
  global_objectId <<-paste("TREND","Dax","signal.SMAzlema")   #Welcher Indikator 
  global_StartDate <<- DateS(last(prices))  #der zeitstempel- zusammen mit globla_objectId der joined key in global_ParTable
  #Parametrisierung des Trainings .. ich leg sogar die increment-Werte für den GRID selber fest
  
  mlist(smaN=c(20,10,100,10), zlemaN=c(10,10,90,5), mode="w")   #wenn ich die 
  #So sieht jetzt die global_ParTable aus:
  mlist(mode="R")
  global_ParTable$par
  global_ParTable
  global_ParTable <<- NULL
  global_arg$dat = data
  View(global_ParTable)
  #Training aller Zeitreihen durchführen  - bei TRAINSYM=0 - bekommen sie einen einheitlichen BestParams - sonjst findet
  #sich für jede zeitreihe ein individueller BestParam
  
  
  TrainIndicator (opti="GRID",indiName = "signal.SMAzlema",visual=T,TRAINSYM=-1)  
  TrainIndicator( opti="GRID",indiName = "signal.sit.feilscher",  visual=T, TRAINSYM =-1)# data$BENCH)  "SXEBP"
  
  x=indi.Generic("signal.sit.feilscher", global_arg, visual=F, TRAINSYM =-1) 
  View(global_ParTable)
  
  
  #und das hat er gefunden:
  global_ParTable
  global_ParTable$par
  
  #Abrufen der Indikatoren mit ihren Bestparam - dannach wird das summensystem mit einem Buy-Hold verglichen
  x=indi.Generic("signal.SMAzlema", global_arg,visual=T, TRAINSYM=0)   #jeder mit gleichen BestParams
  x=indi.Generic("signal.SMAzlema", global_arg,visual=T, TRAINSYM=-1)  #jeder mit seinen eigenen BestParams
}
###########################

library(DEoptim)
#TODO: TimeFrame setzen
##################################################################################################################
#global_StartDate_ setzt einen cursor in die global_ParTable und zeigt damti auf den aktuellen 
#parametersatzt
# der TimeFrame liegt zeitlich vor diesem Datum (ich kann logisch nur daten trainieren die vorher lagen)
#global_objectId_ = "TREND sx5r MACD"
############################################################################################
#Training aller Zeitreihen durchführen  - bei TRAINSYM=0 - bekommen sie einen einheitlichen BestParams - sonjst findet
#sich für jede zeitreihe ein individueller BestParam

#####################
opti="noDEoptim"
TrainIndicator<-function(data=NULL, global_StartDate_=NULL, objectId_ = NULL, opti="GRID", frame ="", indiType ="TREND", indiName="",visual=F, TRAINSYM=0,roll.mode="",train.WinLen=1095)  
{
  
  if (!is.null(data) && (!exists("global_arg") || is.null(global_arg)))
    global_arg<<-list(clos=data.info(data,visual =F)[frame,],dat = data)  #Definition der Preise
  else
    if (!is.null(data))
    {global_arg <<- list(clos =data$prices,  dat=data)}

 
  if (frame !="")
   {
    global_arg$clos = global_arg$dat$weight = global_arg$dat$prices[frame]; global_arg$dat$weight=NA
    global_StartDate = global_StartDate_ = as.Date(index(last(global_arg$dat$prices[frame])))
  }
 
  if (roll.mode != "") #-..........  Rollierendes Training....................
    
  {
   merk_global_arg_clos=global_arg$clos
    
    today.list = select.dates(global_arg$clos[,1],roll.mode) 
    # cashed()
    #reset.cashed.rank
    if (train.WinLen > 0) rolling.train.frame =T
    
    for(today.i in today.list)
    {
      #  today.i="2008-01-01"  #first(today.list)##TEST
      global_StartDate <<- today.i # DateS(last(prices["2008"]))
      
      if (rolling.train.frame)  #mitziehen des start-fensters mit winlen 1095
      {
        start.d = as.character(as.Date(today.i)-train.WinLen) 
        train.frame = sprintf("%s::%s",start.d,today.i)
      }
      else
        train.frame = sprintf("::%s",today.i)
      
      global_arg$clos = global_arg$dat$prices[train.frame]
      mP(" shape of trainingsdata %d  %s",nval(shape(global_arg$clos)),train.frame)
      
      if (nval(shape(global_arg$clos))<200)
        next
      
      print("###########################################################################")
      print(">>>>>>>>>>>>>>>>>>>>>>>>>>>> TRAINING")
      print(global_StartDate)
      print("###########################################################################")
      
      #rekursiver -aufruf 
      #MM
      TrainIndicator(data=data, global_StartDate_=global_StartDate, objectId_ = objectId_, opti=opti, frame ="", indiType =indiType, indiName=indiName,visual=visual, TRAINSYM=TRAINSYM,roll.mode="") 
    }
    global_arg$clos = merk_global_arg_clos
    return("ok")
  }
  
  
  
  if (!exists("global_ParTable"))
    global_ParTable <<- NULL
  
  if (!is.null(global_StartDate_))
    global_StartDate<<-global_StartDate_
  
  
  if (!exists("global_StartDate") || is.null(global_StartDate) && len(global_arg$clos)>0)
    global_StartDate <<- DateS(last(global_arg$clos[train.frame])) 
  
  mP("TrainIndic")
  
  
  if (!is.null ( objectId_ ) )
    global_objectId <<- objectId_
  
  if (indiType != "")
  {
    if (is.character(TRAINSYM))
      sym=TRAINSYM
    else
    {
      tsym=ifelse(TRAINSYM>0,TRAINSYM,1)
      sym=colnames(global_arg$clos[,tsym])  
    }
    global_objectId <<-paste(indiType,"Dax",indiName)
  }
  
  if (!is.null ( frame ) && frame !="" )
    global_frame <<- frame
  else
    global_frame <<-  paste("1995-01-01",global_StartDate,sep="::")
  
  maxRet <<- -1000  #ebenfalls eine globale
  
  print("######### TrainIndicator #########")
  print(global_ParTable)
  print(paste(global_objectId," ",global_StartDate)) #damit steht genau fest welcher Indicator optimiert werden soll
  
  #entnimm aus der global_objectId den Namen des zu optimierenden inidcators
  #und hinterleg ihn f?r  train.indicator() in global_indiName
  # TODO
  #global_indiName <<- "indi.MACD"
  
  merk_global_objectId = global_objectId
  
  merk_global_arg = global_arg
  merk_ml=mlist(mode="R")
  
  
  # browser()  #######
  
  if (is.null(global_ParTable) || len(merk_ml) ==0)
  {
    mP("TrainIndicator:  Sorry1 - You didn't write any par to global_ParTable, first call mlist(w)")
    this_arg = global_arg
    this_arg$clos = global_arg$clos[,1]
    
    indi.fn = match.fun(indiName) 
    indi.fn(arg=this_arg,visual=visual)  #damit der signal.<> via seinem mlist()Aufruf seinen default-parameter nach global_ParTable schreibt.
    #indi.Generic(arg= this_arg, signal.fun=indiName,visual=F,TRAINSYM=1)
    #browser() 
    merk_ml=mlist(mode="R")
    
  }
  
  if (!exists("globalTrainLevel")) 
    globalTrainLevel<<- 10
  
  if (!exists("global_commission"))
    global_commission <<- 0.0005# 0.00001
  
  if (TRAINSYM ==-1)
    TRAINSYM =pureColnames(global_arg$clos)   #Trainiere alle Zeitreihen, aber finde separate BestParams
  
  if (TRAINSYM != 0 && is.numeric(TRAINSYM) )   #trainiere nur eine ausgwählte Zeitreihe
  {
    TRAINSYM=pureColnames(global_arg$clos)[TRAINSYM]
  }
  
  #.....................................................................
  
  while (T)   #alle symbole trainieren  (Ausstieg via break - ganz unten)
  {
    
    if (TRAINSYM !=0)  #trainiere in einer  schleife viele Zeitreihen separat
    {
      if(len(colnames(merk_global_arg$clos)[TRAINSYM[1]])==0)
        sag("wrong usage at Trade.r 1112 %s not at global_arg clos",TRAINSYM[1],warte=T)
      mP("................. NEW sym: %s",TRAINSYM[1])
      #browser()
      
      global_arg$clos <<-merk_global_arg$clos[,TRAINSYM[1]]
      #global_arg$dat$symbolnames = TRAINSYM[1]
      # head(global_arg$clos)
      #erzeuge einen symbol-spezifischen  objectId
      goi =spl(merk_global_objectId,delim=" ")
      New_objectId = sprintf("%s %s %s",goi[1],TRAINSYM[1],goi[3])
      global_objectId <<- New_objectId
      if (len(mlist())==0)
        mlist(merk_ml,mode="w")  #kopiere den init-wert in die global_ParTable (unter neuem objectId)
      
    }
    else
    {
      TRAINSYM = "AllSymbols" #MM3
      #New_objectId = sprintf("TREND %s %s","AllSymbols",indiName)
      New_objectId <<-paste("TREND","AllSymbols",indiName)
      
      global_objectId <<- New_objectId
      
      if (len(mlist())==0)
        mlist(merk_ml,mode="w")  #kopiere den init-wert in die global_ParTable 
    }
    #global_arg$clos <<- global_arg$clos[global_frame] 
    global_indiName <<- indiName
    
    ml=mlist(mode="R")
    #browser()    
    global_Args<<-attributes(ml) #macht die Parameter-Variablennamen f?r mkArgList() lesbar
    
    
    #res<-DEoptim(fn=train.Indicator,lower=c(-5,-5),upper=c(5,5),control=list(storepopfrom=1,itermax=10))
    #TODO:  wie sagt man DEoptim wenn Parameter  ganzzahlig sein m?sse  
    #global_defaultPar
    #browser()
    
    # ml= list(wShort=50 , wLong=200)
    
    #ml= list(wShort=c(50,30,70) , wLong=c(200,160,250))
    #Anzahl an Genen
    
    NumPart = length(ml)*10 #heuristik:  10 mal mehr in der population wie wir parameter haben
    #f?r jede Variable die untere und obere Grenze definieren 
    lower = par2Vec(ml,2)
    upper = par2Vec(ml,3)
    
    if (is.na (lower[1])) #es wurden keine min,max definiert:  ml= list(wShort=50 , wLong=200)
    {
      #definiere  die min,max automatisch als 60% und 140% des ml-wertes
      lower= sapply(ml,FUN=function(x,pct=60) x[1]*pct/100)
      upper= sapply(ml,FUN=function(x,pct=140) x[1]*pct/100)
    }
    
    
    #TODO   setze  intialpop auf den gegenw?rtigen Wert (startpunkt f?r den Optimierer .. damit er nicht zu viel ?ndert)
    #startvector aus bisherigem vector konstruiieren
    initPop = par2Vec(ml)
    #fuer jedes gen einen startvector
    pop=matrix(0,nrow=NumPart,ncol=length(upper))    
    
    #runif(10,5,6)
    #floor(10*runif(10))
    #ToDo  pop auf Zufalls-Wert um initPop setzen, dabei auch auf die Kanten gehen !!!
    
    for(i in 1:NumPart) pop[i,]= initPop;    #Starting guesses:  the last known-par-state
    #browser()
    
    ## optimiser vergleich :  http://cnr.lwlss.net/GlobOptR
    #integer optimizer:  http://stackoverflow.com/questions/3234935/non-linear-integer-programming
    #Rsolnp  http://rgm2.lab.nig.ac.jp/RGM2/func.php?rd_id=Rsolnp:solnp
    if (T)  #PRODUCTION    bevorzuge DEoptim #################################################
{
  #t1=switch(model,
  #          buyhold=  c(Cgar= last(cprices,1), Ret=mReturn(cprices)),
  #          meanreverting = t1_meanreverting(cprices,r,type, params)           
  #          )
  
  if (opti=="DEoptim") #...............................................................................................
  { 
    control=list(itermax=200,F=2, steptol=10, reltol=0.1, NP=NumPart,trace=1)
    r<-DEoptim(fn=train.Indicator, lower=lower, upper=upper, control)
    bestArg =mkArgList(r$optim$bestmem)
    
    plot(r, type = 'b')
    plot(r, plot.type = "bestvalit", type = 'l')
    
  }
  if (opti=="GRID")    #...............................................................................................
  {
    #browser()
    if(exists("runs") )  
      mP("Expect time for %d runs ",runs)
    
    if (is.null(global_ParTable) || len(ml) ==0)
    {
      mP("TrainIndicator:  Sorry - You didn't write any par to global_ParTable, first call mlist(w)")
      #browser()  
      indi.Generic(arg= global_arg, signal.fun=indiName,visual=F,TRAINSYM=1)
      ml= mlist("R")
      
    }
    #browser()
    if (len(ml[[1]]) > 3)  #in der mlist() wurden auch increment -Werte übergeben - also kann man eine level-list bauen:
    { 
      #ml= mlist(smaN=c(20,20,100,3), zlemaN=c(10,10,90,5), mode="w")
      
      #lev=list(seq(10,100,10),  seq(2,30,2),1:2)
      #runs = len(combine2(combine2(seq(10,100,10), seq(2,30,2)),1:12))
      
      #global_FastTrain=5
      #ich kann das Training gröber oder feiner gestalten indem ich die Variable
      #global_FastTrain > 0 setzte. dies ist dann die Anzahl der Zwischenschritte pro Dimension
      
      lev=lapply(ml,function(x) 
      { 
        von=x[2]; bis=x[3]; inc=x[4] 
        if (exists("global_FastTrain") && global_FastTrain > 0 )
          inc=(bis-von)/global_FastTrain
        #browser()
        seq(von,bis,inc)
      }  )  
      ores <<- gridSearch(train.Indicator, levels=lev)
    }
    else
      ores <<- gridSearch(train.Indicator, lower=lower, upper=upper, npar=length(ml),n=globalTrainLevel)
    
    ovalues<<-ores$values[is.finite(ores$values)]
    
    #browser()
    if (visual) plot(ovalues,main=TRAINSYM[1]) #...........
    
    besti=which.min(ovalues)  #Minima suchen
    
    best=ores$values[besti]
    bestArgs = unlist(ores$levels[besti])
    
    mP("#########################BestVal %f",best)
    print(bestArgs)
    bestArg =mkArgList(bestArgs)
    
  }
  else
  {
    r <- genoud(starting.values= initPop, fn=train.Indicator, nvars=length(upper), max=FALSE,pop.size=NumPart,
                max.generations =200,data.type.int=T, boundary.enforcement  = T,
                Domains=cbind(lower,upper))
    
    bestArg =mkArgList(r$par)
  }
}
else  ################## teste einige optimierer
{    
  library(Rsolnp)    
  sag("Y0 solnp:",T)
  #der kann auch noch constrains verkraften 
  Y0 <<-solnp(initPop, fun = train.Indicator, LB=lower,UB=upper )
  print (Y0)
  sag("Y0 optim L-BFGS-B:",T)
  #der klassische gradienten-sucher
  Y1 <<- optim( par = initPop, fn=train.Indicator, method="L-BFGS-B", lower=lower, upper=upper)
  print (Y1)
  print("\nOptim -BFGS-->")
  #  r=optim(runif(2,20,220),fn=train.Indicator, method="L-BFGS-B",control=list(maxit=200))
  #r=optim(runif(2,20,220), fn=train.Indicator, NULL, method = "L-BFGS-B",
  #   lower=rep(20, 60), upper=rep(90, 220),control=list(maxit=2000)) # par[24] is *not* at boundary
  
  Y1b <- optim(par=initPop, fn=train.Indicator, method="BFGS",      control=list(maxit=4000))
  print (Y1b)
  
  #$par    [1] 69.09038 79.97485    $value    [1] -1.327095  
  sag("nlminb:",T)
  Y2 <<- nlminb(start=initPop, obj=train.Indicator, lower=lower, upper=upper)
  print (Y2)
  #$par [1] 69.09038 79.97485     $objective     [1] -1.327095
  #mischung aus newton und genetic, kann auch ganzahlige variable mit data.type.int=TRUE
  # http://sekhon.berkeley.edu/rgenoud/genoud.html  tip: http://stackoverflow.com/questions/3234935/non-linear-integer-programming
  # kann auch multicore !
  sag("genoud:",T)
  
  library(rgenoud)
  Y3 <<- genoud(starting.values= initPop, fn=train.Indicator, nvars=length(upper), max=FALSE,pop.size=NumPart,
                max.generations =200,data.type.int=T, boundary.enforcement  = T,
                Domains=cbind(lower,upper))
  print (Y3)
  #Solution Fitness Value: -1.327095e+000
  #Parameters at the Solution:
  #X[ 1] :  6.900000e+001
  #X[ 2] :  8.000000e+001
  
  
  #DEoptim mit reltol und steptol so parametrisieren dass es auf?rt wenn sich nichts mehr tut
  # steptol ist etwa die mindest-iterations-anzahl - reltol sagt ab welcher dif ergebnisse als unver?ndert gelten f?r fr?her abbruch nach mindestens steptol schritten
  #TODO:  teste  verschiedene strategy [1..6] Werte
  #initialpop = pop,
  #initPop = c(50,180)
  sag("DEoptim:",T)
  control=list(itermax=200,F=2, steptol=10, reltol=0.1, NP=NumPart,trace=1)
  Y4 <<- r<-DEoptim(fn=train.Indicator, lower=lower, upper=upper, control)
  #  best member   :  68.93617 79.72697  best value -1.3271
  summary(Y4)
  wait()
  #par(mfrow = c(2,1))
  plot(r, type = 'b')
  plot(r, plot.type = "bestvalit", type = 'l')
  
  print ("############## ml ")
  print (ml)
  
  
} ###################### TEST-Ende

cat("\nbest vals: ",maxRet, " at \n")
print(bestPars)

# ausw?hlen und abspeichern des besten wertes 
########################## ?berschreiben im global_ParTable mit neu gefundenem Wert
#l=portfolioTicks("RenditePlus")$Name
#l1=l[-which(l=="EUR")]
#Bookkeeping:   gibst den Eintrag schon, 

#browser()  #MM2  PROBLEM    bestArg ist noch nicht im richtigen Format
# .. evtl. sollte der Eintrag auch nicht entfernt sondern nur gepatched werden !!
#den eintrag in dei global_ParTable mit mlist() vornehmen

if (TRAINSYM == "AllSymbols")
  global_objectId <<-paste("TREND","AllSymbols",indiName)

aktuell=mlist(mode="R")
newPar = aktuell
cat("#################################################################################")
mP(" TrainIndicator - found bestArg:  %f", maxRet)
cat("#################################################################################")
# mP(" post  mlist w")

#patch die neuen Werte rein, ohne die optimierungs-parameter zu verändern
for (pars in ls(bestArg))
{
  newPar[[pars]][1]=as.double(bestArg[[pars]] ) 
}
print("----------best par: ")
print(newPar)

mlist(newPar,mode="w")
#View(global_ParTable)

mP(" post  mlist w")
#global_ParTable

newPar=list()
for (pars in ls(aktuell))
{
  newPar[[pars]]=as.double(bestArg[[pars]] ) 
}

if (visual)  
{# train.Indicator(parvec=par2Vec(mlist(),1))
  this_arg= global_arg
  outofsample_frame=sprintf("::%s",as.Date(global_StartDate)+400)
  this_arg$clos = global_arg$clos[outofsample_frame]
  if (TRAINSYM == "AllSymbols")
    x=indi.Generic(signal.fun=indiName, this_arg, par=newPar,visual=T, TRAINSYM = -1)
  else
  x=indi.Generic(signal.fun=indiName, this_arg, par=mlist(),visual=T, TRAINSYM = TRAINSYM)
  
}
#mP("+++~~~~~~++++")
#browser()

if (  TRAINSYM ==0 || TRAINSYM == "AllSymbols")
  break;
if (len(TRAINSYM)==1)  #letzte Zeitreihe wurde trainiert
  break;

TRAINSYM = TRAINSYM[-1]  #erstes Symbol abschneiden

#global_arg$clos <<-merk_global_arg$clos 
  } ##while ... evtl. nachste Zeitreihe, falls die prices separat trainiert werden sollen.
#mlist(mode="R")

if (TRAINSYM !=0)
{
  global_arg<<-merk_global_arg #wieder  zurücksetzen
  global_objectId <<- merk_global_objectId
  
} 
if (visual)    
  View(global_ParTable)

return (newPar)
}  
#TrainIndicator<-cmpfun(TrainIndicator_) #compilier die Funktion

if (F)
{
  TrainIndicator()
  #mtrace(train.Indicator)
  train.Indicator(c(32.7, 113),"indi.ZlemaDif")
}
if (F) #TRAIN and TEST
{
  x=indi.Generic("signal.mom", global_arg, par=list(sma.w=400), visual=T, TRAINSYM =-1)
  #data$BENCH
  
  global_ParTable <<-NULL
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.0005   #sys.Run() nimmt diese commission !!!!
  
  global_StartDate <<-  DateS(last(data$prices)) 
  global_objectId <<-paste("TREND","xx","signal.lm") 
  global_FastTrain <<-20 #hei?t: 
  
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
  
  TrainIndicator( indiName = "signal.mom",  visual=T, TRAINSYM =-1, roll.mode="years")# data$BENCH)  "SXEBP"
  
}
#MM_GENERIC
#####################################################################################
##########################################################
#sys.Run() ein super leichtes PM-System: für Timing-Modelle(signal gegeben) ret=  prices*Signal für viele #MMA
#Aktienin den Prices+Signal-Spalten gleichzeitig
#Sysinvestor sei Dank
#prices und signal dürfen multidimensional sein
#dies  ersetzt die indi.finish()- funktion für die signal.<...>  Systeme die ja alle mit indi.generic()
#laufen und dank sys.Run() multi-asset-tauglich sind.
#wenn experiment != "" wird intensiv reported (xls+pdf)
##########################################################



#########################################################################################
#statt immer die gleichen  indi.SMA,  indi...  Teile zu schreiben (die auch nur single-Asset tauglich sind)
#schreib ich in in Zukunft nur noch  signal.wonder, signal.SMA, signal.... Teile. 
# die sind kürzer und Multi-Asset-tauglich.
#Wichtig:  indi.Generic  wird auch von TainIndicator() aufgerufen - (via train#.Indicator() )
#do.assemble.Signals = T heißt: gehe in global_ParTable durch sämtliche trainierten Parameter durch und bau
#ein Gessamt-Signal aus den sequenzen der der Einzel-Systeme. ..
#nTop ist der Quotinen:  n.top = nsymbols/nTop  # bei nTop = 1 gehen alle symbole
#########################################################################################
#TSA is einfach nur ein anderer Name für indi.Generic()
TSA<-function(signal.fun="SMA", arg,xarg=NULL, par=NULL,visual=F,main="",commission=0.1,TRAINSYM=0,do.assemble.Signals =F,stopSys="",nTop=1,safe=global_arg$dat$SAFE,experiment="",T.arg=list(cashed="NO"),S.arg=list(),A.arg=list(),R.arg=list(),TSA.argSet=TSA.default, indiType="TREND",pdfFile="",period="months",fastMode=F,globCtrl=list(),useXlsPar=F)#trailing.stop
{
  indi.Generic(signal.fun=signal.fun, arg=arg,xarg=xarg, par=par,visual=visual,main=main,commission=commission,TRAINSYM=TRAINSYM,do.assemble.Signals =do.assemble.Signals,stopSys=stopSys,nTop=nTop,safe=safe,experiment=experiment,T.arg=T.arg,S.arg=S.arg,A.arg=A.arg,R.arg=R.arg,TSA.argSet=TSA.argSet, indiType=indiType,pdfFile=pdfFile,period=period,fastMode=fastMode,globCtrl=globCtrl,useXlsPar=useXlsPar)#trailing.stop
    
}

indi.Generic<-function(signal.fun="SMA", arg,xarg=NULL, par=NULL,visual=F,main="",commission=0.1,TRAINSYM=0,do.assemble.Signals =F,stopSys="",nTop=1,safe=global_arg$dat$SAFE,experiment="",T.arg=list(cashed="NO"),S.arg=list(),A.arg=list(),R.arg=list(),TSA.argSet=TSA.default, indiType="TREND",pdfFile="",period="months",fastMode=F,globCtrl=list(),useXlsPar=F)#trailing.stop
{
  if (len(globCtrl)>0)
     global_arg$globCtrl<<- globCtrl
  global_arg$useXlsPar<<-  useXlsPar  #sagt ob in Folge aufgerufene Methoden wie run.roll() oder prepare_for_regression_and_calculate_targets_for_signal_and_ranking() oder prepare_for_classification_and_calculate_targets_for_signal_and_ranking()  aus xls.parameter-Dateien wie cockpit_signal.multi.cubM5P_ESTX_CARSPARTS.xls lesen sollen
  
  universe=ifelse(is.null(data$universe),"",data$universe)
  ass=ifelse(do.assemble.Signals,"A","")
  if (exists("modelDir"))
    dataSet = modelDir
  page(first=T)
  
  #clr()
  if (!is.null(TSA.argSet))   #default-sets von parametergruppen
  {
    if (has(TSA.argSet,"T.arg") && len(T.arg)==0) #timing
      T.arg = TSA.argSet$T.arg
    if (has(TSA.argSet,"S.arg") && len(S.arg)==0) #selection
      S.arg = TSA.argSet$S.arg
    if (has(TSA.argSet,"A.arg") && len(A.arg)==0) #allocation
      A.arg = TSA.argSet$A.arg
    if (has(TSA.argSet,"R.arg") && len(R.arg)==0) #reporting/workflow
      R.arg = TSA.argSet$R.arg
  }
  
  
  if (pdfFile != "" && experiment =="")
    experiment=sprintf("%s_%s_%s%s%s",universe,signal.fun,S.arg$ranking.fn,ass,pdfFile)
  
  model=list()
  org.arg=arg
  #browser(mP("ini.Generic"))
  orgTrainsym = TRAINSYM
  indi.fn = match.fun(signal.fun) #sprintf("signal.%s",signal.fun)) 
  #sym = colnames(arg$clos)
  if (len(colnames(arg$clos)) ==1)
    TRAINSYM=1
  
  
  #if (!is.null ( objectId_ ) )
  #  global_objectId <<- objectId_
  
  if (indiType != "")
  {
    if (is.character(TRAINSYM))
      sym=TRAINSYM
    else
    {
      tsym=ifelse(TRAINSYM>0,TRAINSYM,1)
      sym=colnames(global_arg$clos[,tsym])  
    }
    global_objectId <<-paste(indiType,sym,signal.fun)
  }
  
  #browser(mP("indi.Generic"))
  #if (!exists("global_objectId"))
  #  global_objectId<<-paste("TREND",sym,sprintf("signal.%s",signal.fun))
  
  if (is.null(par)) #wird gebraucht, wenn man unbedingt über Leer-Aufuf von indi.fn() die mlist()-Werte nach gloabl_par-Table transportieren will
  {
    par1 = mlist()
    if (is.null(global_ParTable)  || len(par1)==0)
    {
      this_arg = global_arg
      this_arg$clos = global_arg$clos[,1]
      
      mP("init mlist()")
 #     browser()
      indi.fn(this_arg)  #damit der signal.<> via seinem mlist()Aufruf seinen default-parameter nach global_ParTable schreibt.
      #return(list())
    }
  }
  
  #.....................................................................
 if (TRAINSYM ==0) #MM2
   TRAINSYM =pureColnames(global_arg$clos)   #Teste alle Zeitreihen, aber finde separate 
 
  if (TRAINSYM ==-1)
    TRAINSYM =pureColnames(global_arg$clos)   #Teste alle Zeitreihen, aber finde separate BestParams
  
  if (TRAINSYM != 0 && is.numeric(TRAINSYM) && len(TRAINSYM)==1)   #teste nur eine ausgwählte Zeitreihe
  {
    TRAINSYM=pureColnames(global_arg$clos)[TRAINSYM]
  }
  
  
  if (len(TRAINSYM)==1 &&  TRAINSYM != -1 && TRAINSYM != 0)   #teste nur eine ausgwählte Zeitreihe
  {
    arg$clos = arg$clos[,TRAINSYM]
    
  }
  
  org.arg = arg
  merk_global_objectId = global_objectId
  sym="Allsymbols"
  Signals.all = NULL
  org.par = par
  
  orgTrainsym = TRAINSYM
  
  singleResults = list()
  #.....................................................................
  
  # print("pdf?");browser()
  pdf.f <<- ""
  if (experiment !="")
  {
    pdf.f <<- sprintf("Models/%s/%s_TSA.pdf", dataSet, experiment)
    mP("timing: Write Data Charts to %s",pdf.f)
    dir.create(dirname(pdf.f),recursive=T)
    
    pdf(file = pdf.f, paper="a4r",width=0, height=0)#width=15, height=10)
    if (!is.null(data)) 
    { 
      #data.info(data,visual=F)
      #plot.table(as.matrix(DataInfo))
      #purePlot(mNorm(data$prices)) 
      
      if (exists("SAFE") && !is.null(SAFE) && SAFE != "")
        safe=SAFE
      else
        safe=data$BENCH
      
      textPlot.datainfo(" TSA ")
      purePlot(mNorm(data$prices[,c(data$BENCH,safe)]),main=sprintf("BENCH: %s + SAFE:%s",data$BENCH,safe))
      purePlot(mNorm(data$prices),main=sprintf("Universe",data$BENCH,safe))
      
    }
    
  }
  # browser()
  #................................................................................
  Signals.all =NULL
  rank.all = NULL
  #print("check cashed");browser()
  
  if (!has(T.arg,"cashed","NO") && !visual) #die Signale einfach blos aus dem cashe lesen
  {
    if (len(cashed(T.arg$cashed,  prices=org.arg$clos)) >0)  #ist ge-cashed und die kurse passen was fromToS angeht 
      Signals.all=get.rank(T.arg$cashed,prices=org.arg$clos,syms=colnames(org.arg$clos)) 
  }
  #..............................................................
  
  if (is.null(Signals.all))  #im cashe war noch nichts
    #Timing signale berechnen    
    while (T)   #alle symbole testen  (Ausstieg via break - ganz unten)
    {           #Baue das multi-var-SignaleAll -xts dass, aus evtl. ganz unterschiedlich parametrisierten Teilsystemen generiert wird.
      
      if (TRAINSYM !=0)  #trainiere in einer  schleife viele Zeitreihen separat
      {
        sym = TRAINSYM[1]
        global_objectId <<-paste(indiType,sym,signal.fun)
        
        if (len(colnames(org.arg$clos))>1)   #wahrscheinlich im Trainings-Mode..
          mP("................. Test sym: %s",sym)
        
        if (!sym %in% colnames(org.arg$clos))
          {
          print("################# BUG #############")
          print( colnames(org.arg$clos))
           sag("%s not found1 at data, Grosskleinschreibung ?",sym,warte=T)
        }
        arg$clos <-org.arg$clos[,sym]
        if (dim(arg$clos)[2]==0)
          sag("%s not found at data, Grosskleinschreibung ?",sym,warte=T)
           
          
        
        if(is.null(org.par)) #lies den aktuellen mlist-Wert aus global_ParTable
        {
          #erzeuge einen symbol-spezifischen  objectId zur richtigen Adressierung mit mlist()
          goi =spl(merk_global_objectId,delim=" ")
          New_objectId = sprintf("%s %s %s",goi[1],sym,signal.fun)
          #browser()
          #New_objectId = sprintf("%s %s %s",goi[1],sym,goi[3])
          global_objectId <<- New_objectId
          #nun ist global_objectId für die addressierung in mlist( )fertig: gibts denn in der global_ParTable schon Werte?
        }
      }
      else
      {
        # mP("........ >>")
        # browser()
        goi =spl(merk_global_objectId,delim=" ")
        global_objectId <<- sprintf("%s %s %s",goi[1],"AllSymbols",signal.fun)
        
      }
      
      #...................................................................
      #  MM_TODO  damit ein Indikator, der eine Trainingsgeschichte hat, korrekte    signale liefert müssen diese - auch beim Abruf korrekt zusammengesetzt werden.
      
      #Aufruf  der signal.<-Methode>()
      #wähle ob inkrementell oder komplett mit einem Par-Satz gearbeitet wird
      #browser()
      if (len(org.par) ==0 || do.assemble.Signals)   #sollen Parameter aus global_ParTable benutzt werden?
      {
        #par = mlist()  #falls vorhanden wird hier der zum TRAINSYM[1]#global_StartDate  passende parameter-Satz aus global_ParTable gelesen
        last.trainDay  =""   #signale zusammensetzen aus den jeweiligen - par Sätzen unterschiedlicher Traingstage in global_ParTable
        last.possible.Day = DateS(last(arg$clos))
        
        #global_xMarker  as.list(global_xMarker)
        
        if (!do.assemble.Signals || len(global_ParTable) ==0 || len(global_ParTable[objectId==global_objectId]$time )==0)
        {
          #browser()
          par = mlist() 
          if (len(par)==0)
          {
            mP("There are no parameters  at global_ParTable given for %s",global_objectId) 
            signal.ret = indi.fn(arg=arg,visual=visual,xarg=xarg,main=sprintf("%s %s ",main,"default") )
          }
          else
          {
            title=Title(signal.fun,arg,par,sym=sym)
            mP(sprintf("use org par %s",title))
            signal.ret = indi.fn(arg=arg,visual=visual,xarg=xarg,main=sprintf("%s %s ",main,title) )
          }
        }
        else
          #signal-Assembler  ...  ruft den signal.<> .. mit unterschiedlichen bestParams-  zu den zeiten wie sie in global_ParTable stehen
        {
          #browser()
          
          for (trainDay in global_ParTable[objectId==global_objectId]$time)
          {
            trainDay=as.Date(trainDay)
            
            if (trainDay > as.Date(last.possible.Day)  )
              break;
            
            #if (trainDay > as.Date("2003-01-01"))
            #  browser()
            #im global_ParTable steht das letzte Datum mit dem trainiert wurde.
            
            last.par=last(global_ParTable[time<=trainDay & objectId==global_objectId]) #suche in global_ParTable einen passenden Eintrag
            
            if (len(par)==0)
            {
              mP("Sorry01- there are no parameters  at global_ParTable given") 
              return(list())
            }
            par = last.par$par[[1]] 
            par = lapply(par,function(x)first(x))
            mP(".signal-Assembler.%s.%s     (%s)",global_objectId,trainDay,toString(par))
            
            #Berechne die Signale bis zum kommenden       
            if (len(par)==0)
            {
              mP("Sorry1- there are no parameters  at global_ParTable given") 
              return(list())
            }
            #...................................................................
            title=Title(signal.fun,arg,par,sym=sym)
            mP(title)
            
            # aufruf des Signal-Generators
            if (dim(arg$clos)[2]> 1)
              mP("%s----->>>>>>>>>>>%d symbols >>>>>>>>>>>>",signal.fun,dim(arg$clos)[2])
            
            sig = indi.fn(arg,par,visual,xarg,main=sprintf("%s %s ",main,title) )  #die signal.<..> methode liefert "$Indi" +   "$Signal"
            
            if (last.trainDay =="")
              signal.ret = sig  #es gab vorher noch keinen Trainingstag, dies ist der erste
            else  # nur einen Teil der signal-Ergebnisse verwenden .. nnä..
            {
              if (F)
              {
                signal.ret$Signal = rbind(signal.ret$Signal[sprintf("::%s",as.Date(last.trainDay)-1)] , sig$Signal[sprintf("%s::",last.trainDay)]) #zusammenbasteln
                signal.ret$Indi[[1]] = rbind(signal.ret$Indi[[1]][sprintf("::%s",as.Date(last.trainDay)-1)] , sig$Indi[[1]][sprintf("%s::",last.trainDay)]) #zusammenbasteln
                #MA            
              }           
              #der trainDay ist immer der letzte Tag zu dem trainiert wurde.
              #hierm assemble dürfen neue Daten (sig$Signal) nur nach dem trainDay benutzt werden-
              #sonst wärs eine Kristallkugel (insample-training)
              signal.ret$Signal = rbind(signal.ret$Signal[sprintf("::%s",as.Date(trainDay)-1)] , sig$Signal[sprintf("%s::",trainDay)]) #zusammenbasteln
              signal.ret$Indi[[1]] = rbind(signal.ret$Indi[[1]][sprintf("::%s",as.Date(trainDay)-1)] , sig$Indi[[1]][sprintf("%s::",trainDay)]) #zusammenbasteln
              #MMA            
              
              mP("--- MMassemble> use new data later %s - ", as.character(trainDay)  )
              print(last.par)
              
              
            }
            
            
            
            last.trainDay  = as.character(trainDay) #weiterschalten
            
          }
        }
      }  #parameter werden via kommandozeile ausdrücklich mitgegeben .. sollen also nicht aus global_ParTable gelesen werden
      else
      {
        title=Title(signal.fun,arg,par,sym=sym)
        mP(sprintf("use org par %s",title))
        par=org.par
        # aufruf des Signal-Generators
        if (dim(arg$clos)[2]> 1)
          mP("%s----->>>>>>>>>>>%d symbols >>>>>>>>>>>>",signal.fun,dim(arg$clos)[2])
        if (dim(org.arg$clos)[1]<2)
        {mP("%s----->>>>BUG NODATA>>>>>>> symbols %s >>>>>>>>>>>>",signal.fun,colnames(arg$clos))
         browser()
        }
        if (TRAINSYM==0) #  auch wenn ein parameter gesucht wird der für alle symbole gleich ist wird iteriert
        {
          Signals.all1=NULL
          for (sym.i1 in colnames(org.arg$clos))
          {
          arg$clos = org.arg$clos[,sym.i1]
          signal.ret = indi.fn(arg,par,visual=visual,xarg,main=sprintf("%s %s ",main,title) )
          model=sys.Run(prices=arg$clos,signal=signal.ret$Signal ,compare=visual,viewLevel=spl("timingsys"),main=title, hilfs.Indi=list(signal.ret$Indi),stopSys,signal.fun=signal.fun,data=arg$dat,T.arg=T.arg,S.arg=list(),A.arg=list(),R.arg=R.arg,period=period )
          global_xMarker <<- list()
          #Ergebnissicherung
          if (len(colnames(arg$clos))==1) #MM?
            singleResults[[sym]] = list(equity=model$equity, Tquality=model$Tquality)
          
          if (!has(T.arg,"cashed","NO")) #schreib sie in den cashe  T.arg$cashed 
            get.rank( T.arg$cashed, prices=signal.ret$Signal,syms=sym.i1, xtra="write")#schreibt
          
          
          if (is.null(Signals.all1))
            Signals.all1 = signal.ret$Signal
          else
            Signals.all1 = merge(Signals.all1, signal.ret$Signal )
          }
          
          signal.ret$Signal =Signals.all1
        }
          else
            signal.ret = indi.fn(arg,par,visual=visual,xarg,main=sprintf("%s %s ",main,title) )
      }
      
      #  mP("TestSignals %s  ",global_objectId)
      # browser()
      if (F) #old
      {
        par = mlist() 
        title=Title(signal.fun,arg,par,sym=sym)
        mP(sprintf("use org par %s",title))
        signal.ret = indi.fn(arg,par,visual,xarg,main=sprintf("%s %s ",main,title) )
        
        plot(signal.ret$Indi[[1]])
        
        purePlot(signal.ret$Indi[[1]])
        colnames(signal.ret$Indi[[1]])
      }
      ### nun liegen die signale vor und man kann eine single -modell-system bewertung vornehmen
      # sys.Run() ist das "indi.finish() für  die signal.<...>-systeme: sorgt also für chart und perfauswertung.
      
      mP("..................>>> pre test singelSys:  sys.Run")
      
      if (do.assemble.Signals)
      {
        mP(" do.assemble.Signals  ")
        global_xMarker <<-as.list(global_ParTable[objectId==global_objectId]$time)
      }
      else 
        global_xMarker<<- NULL
      #browser(mP("TEST"))
      if (TRAINSYM !=0 )
      {    
        #browser(mP("check on unknow-sym"))
        #signal.ret$Signal
        model=sys.Run(prices=arg$clos,signal=signal.ret$Signal ,compare=visual,viewLevel=spl("timingsys"),main=title, hilfs.Indi=list(signal.ret$Indi),stopSys,signal.fun=signal.fun,data=arg$dat,T.arg=T.arg,S.arg=list(),A.arg=list(),R.arg=R.arg,period=period )
        global_xMarker <<- list()
        #Ergebnissicherung
        if (len(colnames(arg$clos))==1) #MM?
          singleResults[[sym]] = list(equity=model$equity, Tquality=model$Tquality)
        
        if (!has(T.arg,"cashed","NO")) #schreib sie in den cashe  T.arg$cashed 
          get.rank( T.arg$cashed, prices=signal.ret$Signal,syms= colnames(signal.ret$Signal), xtra="write")#schreibt
        
      }
      
     
      
      if (is.null(Signals.all))
        Signals.all = signal.ret$Signal
      else
        Signals.all = merge(Signals.all, signal.ret$Signal )
      
      if (is.null(rank.all))
         rank.all = signal.ret$rank
      else
        rank.all = merge(rank.all, signal.ret$rank )
      
      if (TRAINSYM ==0)
        break;
      if (len(TRAINSYM)==1)  #letzte Zeitreihe wurde trainiert
        break;
      TRAINSYM = TRAINSYM[-1]  #erstes Symbol abschneiden
      
    } ##while ... evtl. nachste Zeitreihe, falls die prices separat trainiert werden sollen.
  
  #if ( experiment !="" && pdf.f !="")
  #    mdev.off()
  
  #........................... Es jetzt liegen die Signale der evtl. unterschiedlich parametrisierten Indikatoren vor:
  #Signals.al
  
  #mP("pre sys.Run"); browser()
  #print("----> check portfolio signal- performance")  
  
  #sys.Run kann auch ganze Portfolios auswerten, da es auf bt.run des sysinvestors basiert
 
  if ( len(colnames(org.arg$clos))>1  && (len(c(orgTrainsym))>1 || orgTrainsym  < 0 ) )
  {  
    mP("---------------------  PORTFOLIO  sys.Run ----------------------- ")
    #priceSyms(Signals.all)
  
    #browser()#24.01.2017
   
    if ( !fastMode)
    {
    if (orgTrainsym <=0  )
      model=sys.Run(prices=org.arg$clos,signal=Signals.all ,compare=visual,viewLevel=spl("portfolio"), nTop=nTop,safe=safe,experiment=experiment,signal.fun=signal.fun,data=arg$dat,T.arg=T.arg,S.arg=S.arg,A.arg=A.arg,R.arg=R.arg,period=period)
    else  #b1
      model=sys.Run(prices=org.arg$clos[,orgTrainsym],signal=Signals.all ,compare=visual,viewLevel=spl("portfolio"), nTop=nTop,safe=safe,experiment=experiment,signal.fun=signal.fun,data=arg$dat,T.arg=T.arg,S.arg=S.arg,A.arg=A.arg,R.arg=R.arg,period=period)
    
    #mP("portfolio ready ?") #2017
    #browser()
    }
    model$singleResults = singleResults
    
    colnames(rank.all)= colnames(global_arg$dat$prices)
    colnames(Signals.all) = colnames(global_arg$dat$prices)
    model$Signals.all = Signals.all
    model$rank.all = rank.all
    mP("sys.Run ready - need more analysis ? - define parameter: experiment");  
    #browser()
  }
  else return
  #............................................................................................
  if (F)
  {
    #*****************************************************************
    # Create Report
    #******************************************************************    
    
    #  model$trade.summary
    strategy.performance.snapshoot(model, T)
    plotbt.custom.report.part1(model)       
    compareViewModels(list(model=model),prices,alloc=T)
    pdf_Report(list(model=model),experiment)
    
    # Plot Portfolio Turnover for each strategy
    layout(1)
    barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
    
    pdf_Report(experiment)
    #dev.off()
    
    #return(indi.finish(signal=signal.ret$Signal,prices=arg$clos,indi=signal.fun,par=list(par),visual=visual, Indi=signal.ret$Indi,main=main))
  }
  
  global_objectId <<- merk_global_objectId
  return(model)
}
#indi.Generic<-cmpfun(indi.Generic_) #compilier die Funktion


if (F)
{
  global_arg<<-list(clos=prices[,1:1])
  res= indi.Generic("wonder", global_arg,  list( k=20, zlemaN=60), visual=F,commission=0.00001)   
  
  global_ParTable <<-NULL
  global_objectId <<-paste("TREND","Dax","signal.wonder")
  globalTrainLevel <<-10
  mlist()
  global_objectId <<-paste("TREND","Dax","indi.SMA")
  
  indi.SMA(global_arg)
  TrainIndicator(global_StartDate_ = DateS(last(prices)), opti="GRID",indiName = "signal.wonder",visual=F)  
  
  #tune.monthly(prices, symbol="TECDAX", indiName="indi.ZlemaDif",wlen=200,opti="GRID",trainLevel = 100)
  #tune.Generic(prices, endpoints,indiName="wonder",wlen=200,opti="GRID",trainLevel = 100)
  
}

######################################################################################################
indi.ZlemaDif<-function(arg,par = mlist( k=c(29,30,40), kd=c(2,2,40)),visual=F,main="")
{
  
  #  print("indi.ZlemaDif"); browser()
  prices = arg$clos
  today = as.Date(index(last(prices)))
  kd = round(par$kd)
  # minSig=par$minSig
  k=round(par$k)
  
  
  #cat ("#", toString(today),"\n  ZlemaDif: minSig", par$minSig," k ",par$k)
  mP(Title("ZlemaDif",arg,par))
  
  scdfast=score.diff(ZLEMA(prices,n=k),k=kd)  #XOFF = 2*k
  scdfast[is.na(scdfast)] <-0
  # if (minSig==0)
  signal = iif( sign(scdfast[,1])>0 ,1,-1)
  #else
  #   signal = iif( sign(scdfast[,1])>0  & abs(scaleTo(scdfast[,1],c(-1,1)))>minSig,1,-1)
  #main=sprintf("%s %s  winLen %s", main,colnames(clos)[1], toString(par$winLen)) 
  
  # plotSigPrice(signal=signal,prices=arg$clos,indi=list(merge(ZLEMA(prices,n=k),scdfast)),xMarker=global_xMarker,main=main)  
  #browser()
  return(indi.finish(signal,prices,indi="indi.ZlemaDif",par=list(par),visual=visual, Indi=list(ZLEMA(prices,n=k),scdfast),main=main))
  
}
if (F)
  res= indi.ZlemaDif(global_arg,  list( k=30, kd=2), visual=T)   

####################################################################################################


indi.SMA<-function(arg,par=mlist(winLen=c(40,10,200)),visual = F,main="")
{
  clos=arg$clos
  winLen = as.integer(par$winLen)
  sma = tryM(SMA(clos,winLen) )
  
  # if (BUG==1)    browser()
  
  signal = iif(clos >= sma  , 1, 0)
  
  mP(Title("SMA",arg,par))
  #browser()
  return(indi.finish(signal,clos,indi="indi.SMA",par=list(par),visual=visual, Indi=list(merge(sma,clos)),main=main))
  
  #return(indi.finish(signal,arg))
}
if (F)
  indi.SMA(global_arg,  list(winLen=15), visual=T)   

##################################################################################################


#####################################################################################
# aus sysinvestor
#####################################################################################

indi.TSI<-function(arg,par=mlist(tsin=13, w2=2),visual = F,main="")
{
  dat =arg$dat
  tsi = TSI(HLC(dat),par$tsin)
  sma = SMA(tsi, par$w2)
  signal = iif(tsi >= sma && tsi >0 , 1, 0)
  
  mP(Title("TSI",arg,par))
  
  return(indi.finish(signal,arg$clos,indi="indi.TSI",par=list(par),visual=visual, Indi=list(merge(tsi,sma,arg$clos)),main=main))
}
#####################################################################################
# aus sysinvestor
#####################################################################################

# Mean-Reversion(MR) strategy - RSI2
indi.RSI<-function(arg,par = mlist(rsiL=c(2,2,20), rsiThres=c(50,10,90), zlemaN=c(30,0,50)),visual = F,main="")
{
  mP(Title("RSI",arg,par))
  #browser()
  zlemaN=round(par$zlemaN)
  clos=arg$clos
  #  rsi2 = bt.apply.matrix(prices, RSI, 2)  
  rsi2 = RSI(clos, round(par$rsiL))  
  #  signal= iif(rsi2 < par$rsiThres, 1, -1)
  signal= iif(rsi2 < par$rsiThres, -1, iif(rsi2 > 100-par$rsiThres,1,0))
  
  if (zlemaN >0)
    signal = ifelse(ZLEMA(signal,n=zlemaN) >= 0, 1,-1)
  
  #plotSigPrice(signal=signal,prices=clos,indi=list(merge(rsi2,par$rsiThres,100-par$rsiThres)),xMarker=global_xMarker,main=main)  
  #browser()
  
  #main=sprintf("%s %s  rsiL %s rsiThres %s, zlemaN %s", main,colnames(clos)[1], toString(par$rsiL),toString(par$rsiThres),toString(par$zlemaN))
  
  return(indi.finish(signal,arg$clos,indi="indi.RSI",par=list(par),visual=visual, Indi=list(merge(rsi2,par$rsiThres,100-par$rsiThres)),main=main))
}
if (F)
{
  indi.RSI(global_arg)
  x=indi.RSI(global_arg,  list(rsiL=2,rsiThres=20,zlemaN=30), visual=T)     #spannend
  x=indi.RSI(global_arg,list(rsiL=2,rsiThres=45,zlemaN=170),visual=T ) 
}
#####################################################################################
#####################################################################################

indi.Omega<-function(  arg,  par =  mlist(width=c(25,10,100), minLevel =c(1.5,0,3)),visual = F,main="")
{
  w=round(par$width)
  clos=arg$clos
  
  mP(Title("Omega",arg,par))
  ret=mROC(clos)
  Omega<-apply.rolling(ret,FUN="Omega",width=par$width)
  #signal = rep(1,len(dclos))
  #indicator=Omega<-signal
  #lag the data by 1
  signal=iif(Omega > par$minLevel, 1 ,0) #zus?tzliche Gl?ttung
  
  #signal<-lag(signal,k=1)  #ohne das wirds super "vorhersehend" ?????
  #signal[is.na(signal)]<-0
  return(indi.finish(signal,arg$clos,indi="indi.Omega",par=list(par),visual=visual, Indi=list(merge(Omega,par$minLevel)),main=main))
  
}
if (F)
  indi.Omega(global_arg,  list(width=24,minLevel=1.5), visual=T)     #spannend

#####################################################################################
# aus sysinvestor
#####################################################################################

# Trend Following(TF) strategy - MA 50/200 crossover
indi.MACD<-function(arg,par = mlist(wShort=c(50,30,70) , wLong=c(200,160,250)),visual = F,main="")
{
  # browser()
  #sma.short = bt.apply.matrix(prices, SMA, 50)
  #sma.long = bt.apply.matrix(prices, SMA, 200)
  #data$weight[] = NA
  #data$weight[] = iif(sma.short > sma.long, 1, -1)
  
  #print("############## indi.MACD #############")
  #  cat ("\n MACD: wShort",par$wShort[1]," wLong ",par$wLong[1])
  mP(Title("MACD",arg,par))
  
  smaShort = SMA(arg$clos, n= round(par$wShort))  
  smaLong = SMA(arg$clos, n= round(par$wLong))
  
  signal= iif(smaShort > smaLong, 1, -1)
  #  browser()  
  
  return(indi.finish(signal,arg$clos,indi="indi.MACD",par=list(par),visual=visual, Indi=list(merge(smaShort,smaLong,arg$clos)),main=main))
  
}


if (F)
  TrainIndicator()
#####################################################################################
# aus sysinvestor
#####################################################################################

#*****************************************************************
# Regime Switching  Historical
#****************************************************************** 
#classify current volatility by percentile using a 252 day lookback period
#The resulting series oscillate between 0 and 1, and is smoothed using a 21 day 
#percentrankSMA (developed by David Varadi) using a 252 day lookback period.
#percentrank(MA(percentrank(Stdev( diff(log(close)) ,21),252),21),250)
#
#Generell sind starke Aufw?rtstrends mit einer Abnahme der Volatilit?t zu erkl?ren, starke Abw?rtsbewegungen f?hren zu einer Ausweitung/Steigerung der Volatilit?t
#Das Ph?nomen eines "Reversals" tritt gew?hnlicherweise dann auf, wenn die Volatilit?t pl?tzlich zunimmt.
#Quelle: http://www.broker-test.de/finanzwissen/technische-analyse/volatilitaet/

indi.RegimeSwichtCol<-function(arg,par = mlist( SDn=21,w1=252,w2=250,rsiT=50,volRank=0.5),visual = F,main="")
{
  ret.log = ROC(arg$clos, type='continuous')
  hist.vol = runSD(ret.log, n = par$SDn)
  vol.rank = percent.rank(SMA(percent.rank(hist.vol, par$w1), par$SDn), par$w2)
  
  smaShort = SMA(arg$clos, n= round(par$wShort))
  smaLong = SMA(arg$clos, n= round(par$wLong))
  
  rsi2 = RSI(arg$clos, par$rsiL)  
  
  # Regime Switching  Historical  
  signal = iif(vol.rank > par$volRank, 
               iif(rsi2 < par$rsiT, 1, -1),
               iif(sma.short > sma.long, 1, -1)
  )
  
  return(indi.finish(signal,arg$clos,indi="indi.RegimeSwichtCol",par=list(par),visual=visual, Indi=list(),main=main))
}

#####################################################################################
# aus sysinvestor
#####################################################################################
#sma-vergleich:  RSIlevy - zur Benchmark
indi.BenchCompare<-function(arg,par=mlist(winLen=200, compareTo ="Rex"),visual = F,main="")
{
  clos=arg$clos
  
  sma1 = SMA(clos,par$winLen)
  compTo = getCol(arg$data$prices, compareTo)
  sma2 = SMA(compTo,par$winLen)
  signal = iif(sma1 > sma2 , 1, 0)
  
  return(indi.finish(signal,arg$clos,indi="indi.BenchCompare",par=list(par),visual=visual, Indi=list(),main=main))
  
}
####################################################################################################################################################################
indi.HOLTW<-function(arg,par = list())
{
  mP(Title("HOLTW",arg,par))
  prices=par$clos
  slopes <- rollapplyr(prices, width=60, FUN=roll.HOLTW, by.column=FALSE, align = "right")  
  #Align-Signals
  block=mmerge(slopes,prices)
  slopes= block[,1]
  prices= block[,2]
  #new_Win(1)
  signal = sign(slopes)
  
  #signal = ifelse(ZLEMA(sign(slopes),na.rm=T,n=20) >= 0,1,-1)
  #signal=ifelse(abs(slopes)*10000<1, 0,signal)
  
  if (visual)
    plotSigPrice(signal=signal,prices=arg$clos,indi=list(merge(smaShort,smaLong)))  
  
  
  return(indi.finish(signal,prices,indi="indi.HOLTW",par=list(par),visual=visual, Indi=list(scdfast),main=main))
  
}

#####################################################################################
#
#####################################################################################

indi.finishOLD<-function(signal,arg)
{
  signal<-lag(signal,k=1)
  signal[is.na(signal)]<-0
  
  this_dclos= getCol(arg$dclos, arg$Ti$name)
  
  guv = cumsum(signal*this_dclos)
  #  indicator = merge(clos,sma)
  #browser() 
  ret =as.numeric(last(guv))
  print(ret)
  return (ret)
}

#####################################################################################
#  siehe nile  kalman-beispiel aus dlm paket -doku
#####################################################################################

library(dlm)

nileBuild1 <- function(par) {
  #random walk with white noise
  dlmModPoly(1, dV = .3, dW = .01)
  #  dlmModPoly(1, dV = exp(par[1]), dW = exp(par[2]))
  dlmModPoly(1, dV = .9, dW = .11)
  
}

nileBuild2 <- function(par) {
  #random walk with white noise
  #dlmModPoly(1, dV = .99, dW = .20)
  dlmModPoly(1, dV = exp(par[1]), dW = exp(par[2]))
}
#####################################################################################
#  siehe nile  kalman-beispiel aus dlm paket -doku
#####################################################################################

#####################################
#Combining auto.arima() and ets() from the forecast package
#I've been using the ets() and auto.arima() functions from the forecast package to forecast a large
auto.ts <- function(x,ic="aic") {
  XP=ets(x, ic=ic) 
  AR=auto.arima(x, ic=ic)
  
  if (get(ic,AR)<get(ic,XP)) {
    model<-AR
  }
  else {
    model<-XP
  }
  model
}

#str(forecast(fit,1))

aam<-function(c){fit<-auto.arima(ts(arg$clos),ic="aic");  return(forecast(fit,1))}

#aam()

indi.Kalman<-function(arg=list(clos=clos), par = list(dv=.3, dw =.01,w=120))
{
  #smaShort = SMA(arg$clos, par$wShort)
  Clos = ts(arg$clos)
  #  smaLong = ets(Clos)
  #  fit <- ets(Clos)
  signal<-apply.rolling(arg$clos,FUN=aam,          width=par$w)
  
  fit <-auto.arima(Clos, ic="aic")
  plot(Clos)
  plot(forecast(fit,20))
  #signal= iif(smaShort > smaLong, 1, -1)
  
  clos=arg$clos
  
  Nile =ts(na.omit(clos))
  
  nileMLE <- dlmMLE(Nile, rep(9,2), nileBuild1); 
  #nileMLE2 <- dlmMLE(Nile, rep(9,2), nileBuild2);
  #nileMLE$conv
  nileMod <- nileBuild(nileMLE$par)
  #nileMod2 <- nileBuild(nileMLE2$par)
  #V(nileMod)
  #V(nileMod2)
  ##W(nileMod)
  #W(nileMod2)
  
  nileFilt <- dlmFilter(Nile, nileMod)
  
  nileSmooth <- smaLong# dlmFilter(Nile, nileMod2)# dlmSmooth(nileFilt)
  #x=sum(nileFilt$m[-1]- nileSmooth$s[-1])
  #print(x)
  
  #plot(cbind(Nile, nileFilt$m[-1], nileSmooth$s[-1]), plot.type='s',
  #     col=c("black","red","blue"), ylab="Level", main="Nile river", lwd=c(1,2,2))
  plot(cbind(Nile, nileFilt$m[-1], nileSmooth), plot.type='s',
       col=c("black","red","blue"), ylab="Level", main="Nile river", lwd=c(1,2,2))
  
  #  return(indi.finish(signal,arg))
}
if (F)
{
  xts
  par = list(dv=.3, dw =.01,w=120)
  arg=list(clos= data$prices[,data$BENCH])
  plot(arg$clos)
  new_Win()
  indi.Kalman(arg=list(clos= data$prices[,data$BENCH]))#["2007"]) )
}

#TODO
#http://www.broker-test.de/finanzwissen/technische-analyse/williams-percent-r/

#indi.WilliamsPercent
#indi.LinReg




#########################################################################################################################
my.VAR_ <- function(price,n.ahead=15)    #eine Beispielfunktion für rollapplyr
{
  cat(sprintf("my.VAR_: %s ",toString(as.Date(index(first(price))))),"\n")
  
  #price=mNorm(price) #NEU
  pri=ts(merge(price,seq(1,len(price))),frequency=12)
  priceMod <-VAR(pri,p=1,type="none")  #MM_TODO:  noch ander  VAR-type ausprobieren "trend","both", ...
  pred <- predict(priceMod, n.ahead = n.ahead, ci = 0.95)  #anderes Signifikanz-Intervall ausprobieren  
  
  fcst=pred$fcst[1]
  Res=data.frame(fcst)
  res = last(Res[,1])-first(Res[,1])
  return(res)
}
my.VAR<-cmpfun(my.VAR_) #compilier das Teil

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indi.VAR<-function(arg,par=mlist(winLen=c(40,10,200),zlemaN = c(100,10,200)),visual = F,main="")
{
  mP(Title("VAR",arg,par))
  
  clos=arg$clos
  winLen = as.integer(par$winLen)
  zlemaN = as.integer(par$zlemaN)
  #sigNoise = as.double(par$sigNoise) #  sigNoise = c(2,1,10)
  n.ahead = 2# as.integer(par$n.ahead)
  
  
  slopes <- rollapplyr(clos, width=winLen, FUN=my.VAR, n.ahead=n.ahead,by.column=FALSE, align = "right")
  slopes=na.omit(slopes)
  signal = sign(slopes)
  
  #signal= ifelse(abs(slopes)*100<sigNoise, 0,signal)
  
  signal = ifelse(ZLEMA(signal,n=zlemaN) >= 0, 1,-1)
  
  #slopesP=percent.rank(slopes,50)
  
  #sma = tryM(SMA(clos,winLen) )
  # if (BUG==1)    browser()
  
  #plotSigPrice(signal=signal,prices=clos,indi=list(merge(slopes*10)),xMarker=global_xMarker,main=main)  
  
  return(indi.finish(signal,clos,indi="indi.VAR",par=list(par),visual=visual, Indi=list(merge(slopes)),main=main))
  
}
if (F)
  indi.VAR(global_arg,  list(winLen=20,zlemaN=10), visual=T)    #vermutlich super instabil !!

###########################################################################
#parameter-names im title
###########################################################################
Title<-function(main="RSI",arg,par,sym="")
{
  p=sapply(names(par),FUN=function(nam) {sprintf("%s=%s",nam,toString(first(par[[nam]])))})
  title=paste(p,collapse=",")
  title = paste(main,title,sym,sep=": ", toString(as.Date(index(last(arg$clos)))))
  return(title)
}
###################################################################################
#Alternative zum MACD:
#  mit kalmann-Filter

#http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/dlm/html/dlmFilter.html

#http://stats.stackexchange.com/questions/18387/whats-Sthe-difference-between-dlmsmooth-and-dlmfilter-in-rs-dlm-package
#http://stats.stackexchange.com/questions/4296/r-code-for-time-series-forecasting-using-kalman-filter

if (F)
{
  head(Nile,20)
  
  library(dlm)
  new_Win()
  
  
  nileBuild <- function(par) {
    #random walk with white noise
    #dlmModPoly(1, dV = .3, dW = .01)
    dlmModPoly(1, dV = .95, dW = .01)
    #  dlmModPoly(1, dV = exp(par[1]), dW = exp(par[2]))
  }
  
  #head(Nile,30)
  Nile = ts(na.omit(prices$sx5r["2006/2010"]))
  nileMLE <- dlmMLE(Nile, rep(9,2), nileBuild); 
  nileMLE$conv
  nileMod <- nileBuild(nileMLE$par)
  V(nileMod)
  W(nileMod)
  nileFilt <- dlmFilter(Nile, nileMod)
  nileSmooth <- dlmSmooth(nileFilt)
  #str(cbind(Nile, nileFilt$m[-1], nileSmooth$s[-1]))
  plot(cbind(Nile, nileFilt$m[-1], nileSmooth$s[-1]), plot.type='s',
       col=c("black","red","blue"), ylab="Level", main="Nile river", lwd=c(1,2,2))
  
}
#####################################################################################
################## plotte return und stuer-Gr??e cprice in einem plot
#####################################################################################

Trade1.Plot<-function(cprices, t1,model) 
{
  
  ylim=c( min(min(cprices), min(t1$Ret)),max(max(cprices),mamx(t1$Ret)))  
  plot(t1$Ret,ylim=ylim,main=sprintf("Return %s",model))  
  points(cprices, type = "l", col = "blue")
  signal = t1$Sig
  barplot(signal,col="blue",space = 0, border = "blue",xaxt="n",yaxt="n",xlab="",ylab="")
  
}

#undebug(guv)
#guv(mROC(dax),pBenchRet=mROC(Rex),signal= signal)
################################################################################################################
#gib renditen der isin, des benchmark, ein bin?res 0,1 signal und eine allocation
#alles wird im chart ?ber die zeit angezeigt
#################################################################################################################

guv<-function(pRet, pBenchRet=NULL, signal=NULL,alloca=NULL,visible=F,indi=NULL)
{
  name = colnames(pRet[,1])
  if (is.null(signal))
    signal = rep(1,nrow(pRet))
  signal = na.omit(signal)
  alloca = na.omit(alloca)
  pRet = na.omit(pRet)
  
  guv = as.xts(pRet*signal)
  
  GuVr = mRendite(na.omit(guv))
  cgar = last(GuVr);
  
  ret = mRendite(na.omit(pRet))
  if (!is.null(pBenchRet) )
    retB =mRendite(na.omit(mROC(pBenchRet)))
  else
    retB=NULL
  #Beschr?nke die items-liste auf solche elemente die ungleich NULL sind
  items = list( ret,retB,signal,alloca)
  items = items[which(unlist(lapply(items, FUN=function(x) !is.null(x))))]
  mR= na.omit(items)
  
  #mR = na.omit(merge(GuVr,ret,retB,signal,alloca))
  #GuVr = mR[,1]
  #ret =mR[,2]
  #retB = mR[,3]
  #  signal = mR[,4]
  # alloca = mR[,5]
  
  if (visible)
  {
    g_range <- range(mR)# GuVr,ret,retB)
    #browser()
    titel = sprintf("GuV %s %f",name,cgar)
    #windows()
    par(mfrow=c(1,1), oma=c(2,2,2,2),mar=c(2,2,2,2),col="black")
    xlim_ = c(index(last(GuVr)), index(first(GuVr)))
    plot(GuVr,ylim=g_range,pch=15,main= titel)
    if (!is.null(alloca))
    {
      par(new=T)
      lines(alloca,col="lightblue",ylim=range(alloca),type="h")
    }
    par(new=T)
    plot(GuVr,ylim=g_range,pch=15,main=titel)
    lines(ret,col=ifelse(signal <1, "blue", "red"),type="h")
    lines(ret,col="blue",type="l")
    
    par(new=T)
    lines(GuVr,col="green",ylim=g_range,lwd=3)
    if (!is.null(retB))
    {par(new=T)
     lines(retB,col="black",ylim=g_range)
    }
    
    if (!is.null(indi))
    { 
      indi = na.omit(indi)
      par(new=TRUE)
      irange =range(indi)
      plot(indi[,1],yaxt="n",xlab="",ylab="",xlim=xlim_, ylim =irange,main="", auto.grid=T,axes=F)
      for (i in 1:ncol(indi))
        lines(indi[,i],col="black",xlim =xlim_,ylim=irange,lwd=1)
    }
    
    
  } 
  return (guv)
}


############################################################################################
#Hol Dir aus den indi.Generic.Results die equity und mach ein Ranking darüber
############################################################################################
equity.ranking<-function(indi.Generic.Result,win=200)
{
  equities = foreach(sym = indi.Generic.Result$singleResults, .combine= "merge") %do%
{sym$equity}

ranking <- rollRegressionXTS(equities,win=200)*100
mchart(ranking)
ranking
}

if (F)
{
  x5.1=indi.Generic("signal.drawDown1", global_arg, par = list(win=150) ,visual=F, TRAINSYM=-1)
  x5.1$Tquality 
  ranking=equity.ranking(x5.1)
  
}
############################################################################################
indi.Results<-function(indi.Generic.Result)
{
  Tqualities = foreach(sym = indi.Generic.Result$singleResults, .combine= "rbind") %do%
{res=sym$Tquality}
#names(indi.Generic.Result$singleResults)
Tqualities=data.frame(Tqualities)
rownames(Tqualities)=names(indi.Generic.Result$singleResults)
Tqualities = cbind(names(indi.Generic.Result$singleResults),Tqualities)
rownames(Tqualities)=NULL
colnames(Tqualities)=spl("sym,Tquality")
print(Tqualities[order(Tqualities[,1],decreasing=F),])
print("############### Portfolio:")
#ls(x5.1$Signal)
print(indi.Generic.Result$Tquality)
indi.Generic.Result$Tquality
}
if (F)
{
  indi.Results(x5.1)
}



################################################################################################################################
################################################################################################################################

################################################################################################
#für Modelle
################################################################################################
performance.numbers<-function(modelName="",this.model,frame="", data=NULL,visual=T)
{
  if (is.null(data))
    return("sorry -no data given")
  model=clone(this.model)
  fromToS(model$eq)
  
  if (modelName=="HuAWorld_F#signal.Faber.i2")
    browser()
  
  #patche die Zeiten
  #frame="2006::2009"
  model$eq=model$eq[frame];model$equity=model$equity[frame];model$ret=model$ret[frame];model$weight=model$weight[frame];
  model$dates.index=dates2index(data$prices,as.Date(index(data$prices[frame])))
  
  ds=bt.detail.summary(model) #######################
  ds$System$DVR=NULL
  #ds$Trade=append(ds$Trade, list(Turnover= sprintf("%.1f",100 *compute.turnover(model,data))))
  
  nt=try(as.numeric(numTrades(model$Signal[frame])$allT))
  ds$System$nTrades=nt
  temp=list2matrix(ds$System)
  
  #if (visual)    
  colnames(model$equity)=modelName
  #ds$universe=data$universe
  ds$System$modelName = modelName
  # ds$System$sym=ncol(model$Signal)
  
  sym="portfolio"
  ds$System$Exposure=NULL
  if (visual)
  {
    norm_Win(2)
    purePlot(mNorm(merge(data$prices[,data$BENCH],model$equity)[frame]),main=modelName)
    plot.table(temp, keep_all.same.cex = TRUE)
  }
  if (F)
  {
    plotbt.transition.map(model$weight[frame,], x.highlight = T)
    norm_Win()
    temp = plotbt.monthly.table(model$equity[frame,])
    plotbt.holdings.time(model$weight[frame,])
  }   
  ds$System$Tquality = calmar= abs(compute.calmar(model$eq))*sign(ds$System$Cagr);
  
  ds$System
}


################################################################################################  nehm ich auch für modelle weil stabiler
#für Signale  ... und nun auch für modelle   mit !is.null(signal)
################################################################################################
#... falls Du nur ne equity und signals hast...
performance.Numbers<-function(modName="",equity,signal=NULL,frame="", data=NULL,visual=T,model=NULL)
{
  if (is.null(equity))
    return("sorry -no equity data given")
  if (is.null(data))
    return("sorry -no data given")
  equity=na.omit(equity)
  
  sym=colnames(equity)
  equity=equity[frame]
  out = list()
  out$Period = join( format( range(index.xts(equity)), '%b%Y'), ' - ')
  out$Cagr = compute.cagr(equity)
  
  
  ret=mROC(equity)
  out$Sharpe = compute.sharpe(ret)/100 
  out$Volatility = compute.risk(mROC(equity))
  out$MaxDD = compute.max.drawdown(equity)
  temp  = try(compute.avg.drawdown(equity))
  if(inherits(temp, 'try-error'))
    out$AvgDD=0
  else
    out$AvgDD = temp
  
  out$VaR = compute.var(ret)
  out$CVaR = compute.cvar(ret)
  out = lapply(out, function(x) if(is.double(x)) round(100*x,2) else x)
  out = lapply(out, function(x) if(!is.finite(x)) 0 else x)
  
  #ls(model)

  #die können aus unterschiedlichen plattenloads stammen - so dass die datums-ranges
  #garn nicht passen, dann crassed compute.turnover()
#  fromToS(model$weight)
  #fromToS(data$prices)
out$nTrades=1
  if (!is.null(signal)) #ein portfolio-modell - kein symbol- trade-modell
  {
    if (sym %in% colnames(signal))    
     nt=try(as.numeric(numTrades(signal[frame,sym])$allT))
    else
      nt=try(as.numeric(numTrades(signal[frame,])$allT))
    
    out$nTrades=nt
    if (sym %in% colnames(signal))     #??
       out$sym=sym
  }
  else
  {
    if (!is.null(model))
    {
      #model$type="weight"
      cto=compute.turnover(model,data)
       out$nTrades=  nval(sprintf("%.1f",100 *cto))
    }
    out$sym=colnames(equity)    #??
  }
  
  out$modelName=modName
 
  out$Tquality = calmar= nval(mcalmar(equity))-out$nTrades * global_commission# abs(compute.calmar(equity))*sign(out$Cagr);
  
  if (visual)
  {
    norm_Win(2)
    temp=list2matrix(out)
    
    purePlot(mNorm(merge(data$prices[frame,data$BENCH],equity)),main=modName)
    plot.table(temp, keep_all.same.cex = TRUE)
  }
  
  #if (visual)    
  #  colnames(model$equity)="model.equity"
  
  out
}

if (F)
{
  load("MWorld3");  data$universe="MWorld3"
  define.Globals();   SAFE="BARCLAYS"
  
  data=universe_hua_World(reload=F)
  
  model.ig=indi.Generic("signal.Faber.i2", global_arg,visual=F, TRAINSYM =-1)
  fromToS(model.ig$eq)
  
  
  k1= performance.numbers("modName",model.ig,frame="2005",data=data,visual = F)
  names(k1)
  k= performance.Numbers("modName",equity=model.ig$singleResults[[1]]$equity,signal=model.ig$Signal,frame="2005",data=data,visual = F)
  names(k)
  
  ls(model.ig$Signal)
  
  models= load.models("HuaFeb_4","HuaWorld_F")
  
  m=models.Results(models)
  
}


###########################################################################################
#enthalte models eine ganze liste von indi.Generi.Results
#für welche Universe Komponenten klappt das Trendfolge-Modell-Portfolio und welche nicht?
#ToDo .. das Thema: freies quality.fn  ist nur halb fertig ... :-)
#Hier werden vor allem Performance-Zahlen über zeiträume gerechent und in Tabellen zurückgegben
###########################################################################################
models.Results<-function(models, quality.fn="", frame="",longOnly=T,win=600, visual=T, rank.for ="sharpe",ignore.signal.models=F) #cagr
{
  print("----------------------------")  
  mP("models.Results")
  #print(names(models))
  print("----------------------------")
  quality.Fn=NULL
  if (quality.fn !="")
  {
    quality.Fn=match.fun(quality.fn)
  }
  
  indi.results<-function(mod)
  {
    indi.Generic.Result=mod  
    
    if (quality.fn=="" || is.null(quality.Fn))
    {
      #Tqualities =list2xts(indi.Generic.Result$singleResults,"Tquality")
      if (len(mod$singleResults)>0)
      {
        syms= mod$singleResults
        if (frame=="") 
          Tqualities= sapply(syms,function(sym)sym$Tquality)   
        else
        {
          Tqualities= sapply(syms,function(sym)sym$Tquality)
        }
      }
      else Tqualities=0
    }  
    else
    {
      Tqualities = quality.Fn(mod,longOnly=T, win=600,frame="")   
    }
    
    #names(indi.Generic.Result$singleResults)
    Tqualities=data.frame(Tqualities)
    rownames(Tqualities)=names(indi.Generic.Result$singleResults)
    #Tqualities = cbind(names(indi.Generic.Result$singleResults),Tqualities)
    
    #rownames(Tqualities)=NULL
    colnames(Tqualities)=spl("Tquality")
    #Tqualities[order(Tqualities[,1],decreasing=F),]
    Tqualities
  }
  #  mod.n = names(models)[[1]]
  #   mod=models[[mod.n]]
  
  # o= performance.Numbers(names(mod),equity=mod$equity,signal=mod$Signal,frame=frame,data=data,visual=F)
  # tab= indi.results(mod)
  
  #erst mal die portfolio-performances
  #print(frame)
  tab=NULL
  for(mod.n in names(models))#,.combine="rbind",.errorhandling='pass') %do%
  {
    print(mod.n)
    mod=models[[mod.n]]
    if (frame=="" && len(mod$Tquality)>0)
    {
      #    mP("%s    -> %f ",mod.n,mod$Tquality)
      modList=list()
      modList$modelName=mod.n
      modList$Tquality = mod$Tquality
    }
    else
    {
      #temp1=try(performance.numbers(mod.n,this.model=mod,frame=frame,data=data,visual=T)) #von SiT.. crashed ab und zu
      temp= try(performance.Numbers(mod.n,equity=mod$equity,signal=mod$Signal,frame=frame,data=data,visual=visual,mod))
     # View(temp)
    #  browser()
      if(inherits(temp, 'try-error'))
        sag("There was a bug11",warte=T)
      modList =data.frame( temp)
      
      
    }
    rt=data.frame(modList)
    if (is.null(tab))tab=rt else tab=rbind(tab,rt)
  }#............................................................
  
  if (frame =="" && len(tab$Tquality)>0 )
    tabM=tab[order(tab$Tquality,decreasing=T),]
  else   
  {
    #sortieren der portfolio-modelle nach 4 Kriterien ...
    
    # tab$nTrades=as.integer(cut(tab$nTrades,b=10))
    #  tabM=tab[order(-tab$Cagr,tab$nTrades),];tabM
    #View(tab)
    #tabM=tab[order(as.integer(cut(tab$nTrades,b=3)),-as.integer(cut(tab$Sharpe,b=5)),-as.integer(cut(tab$MaxDD,b=20)) ,-tab$Cagr),];tabM
    
  #  tab=tabM
    #multiDimRanking :
    #MRANK
    if (rank.for =="sharpe")  #sort-quality
      # tabM=tab[order(-as.integer(cut(tab$Sharpe,b=10)),-as.integer(cut(tab$MaxDD,b=20)), -as.integer(cut(tab$nTrades,b=100)),-tab$Cagr),]
      tabM=tab[order(as.integer(cut(tab$nTrades,b=3)),-as.integer(cut(tab$Sharpe,b=5)),as.integer(cut(abs(tab$MaxDD),b=20)) ,-tab$Cagr),]
    else 
      if ( rank.for =="cagr")
      #er sollte was die sharpe angeht schon zum oberen fünftel zählen, aber dann kommts vor allem auf cagr an  - alles andere ist nachgelagert
      #tabM=tab[order(-as.integer(cut(tab$Sharpe,b=5)),-as.integer(cut(tab$Cagr,b=50)), -as.integer(cut(tab$MaxDD,b=20)), -as.integer(cut(tab$nTrades,b=100))),]  
      tabM=tab[order(as.integer(cut(tab$nTrades,b=3)),-as.integer(cut(tab$Cagr,b=5)),-as.integer(cut(tab$Sharpe,b=20)) ,-tab$MaxDD),]
    else
    if ( rank.for =="mcalmar")
    {
    #  browser()
      ok.trades= which(tab$nTrades<400) #alles unter 400 turnover ist ok
      bad.trades= which(tab$nTrades>800)  #alles über 800 turnover geht gar nicht
      cut.i=as.integer(cut(tab$nTrades,b=2))  #mich interessiert nur die richtige hälfte
      cut.i[ok.trades] = 0
      cut.i[bad.trades] = 100
      tabM=tab[order(cut.i,-as.integer(cut(tab$Sharpe,b=10)),-as.integer(cut(tab$Tquality,b=20)) ,-as.integer(cut(as.integer(tab$MaxDD),b=20))),]
      View( tabM)
    }
    else
          sag("unknown rank.for",warte=T)
    if (F)
    {
    View(tabM)
    tab = tabM
    tt= tabM;
    tt$nTrades= as.integer(cut(tab$nTrades,b=3))
    tt$Sharpe= as.integer(cut(tab$Sharpe,b=5))
    tt$MaxDD=as.integer(cut(abs(tab$MaxDD),b=20)) 
    tt$Cagr = tab$Cagr
    print(tt)
    View(tt)
    
    y=tab$MaxDD
    library(Hmisc)  #hdquantile ist besser als quantile
    
    quantile.range <- hdquantile(y, probs = seq(0, 1, 1/10),na.rm=F)
    quantile.range = unique(quantile.range) #manchmal macht hdquantile identische Werte
    tt$MaxDD =as.numeric(cut(y,breaks=as.numeric(quantile.range) ,include.lowest=T))# ,labels =F))
    }
    
    #tabM=tab[order(-as.integer(cut(tab$Sharpe,b=2)),tab$MaxDD, tab$nTrades,-tab$Cagr),];tabM
    #tabM=tab[order(-as.integer(cut(tab$Sharpe,b=2)),tab$nTrades),];tabM
    nSyms =len(colnames(models[[1]]$Signal))
    nMods = len(names(models))
    tabM$rank=c(nrow(tabM):1)*100/nMods
    rownames(tabM)=c(1:nrow(tabM))
    # tabM=tabM[,-1]
  }
  rownames(tabM)=NULL
  if (ignore.signal.models || len(models[[1]]$singleResults)==0)
  {
    print("################################################ RESULTS ###  ignore.signal.models #########################")
    print(tabM)
    res=list(portfolio.mod= tabM)
    return(res)
  }
  
  
  Tab.sys= NULL
  #nun für die signal-modelle die performance-zahlen berechnen ... alles weiterer erfolgt dann in 
  #signal.model.analysis ()                             ----------------------------------->
  for(mod.n in names(models))#,.combine="rbind") %do%
  {
    mod=models[[mod.n]]
    if (frame=="" && len(mod$Tquality)>0)
    {
      mP("%s    -> %f ",mod.n,mod$Tquality)
      tab.sys= indi.results(mod) 
      
      if (is.null(Tab.sys)) Tab.sys= tab.sys else Tab.sys = cbind(Tab.sys,tab.sys)
    }
    else
    {#MM2
      colnames(mod$equity)
      if (len(mod$singleResults)>0)
      {for(signal.mod.n in names(mod$singleResults))#,.combine = "rbind") %do%
      {
        sym=signal.mod.n
        temp= try(performance.Numbers(mod.n,equity=mod$singleResults[[sym]]$equity,signal=mod$Signals.all[,sym],frame=frame,data=data,visual=T))
        if(inherits(temp, 'try-error'))
          sag("There was a bug222",warte=T)
        else
          {
            tab.sys =data.frame( temp)
        
        # print(tab.sys)
        
        if (is.null(Tab.sys)) Tab.sys= tab.sys else Tab.sys = rbind(Tab.sys,tab.sys)
          }
      }
      }else Tab.sys = tabM
      
      #print("##############################>>>>")
      #print(Tab.sys)
      #Tab.sys
      #MMX
    }
    
  }
  tab=Tab.sys
  if (frame=="" && len(tab$Tquality)>0)
  {
    #Tab=data.frame(rowSums(tab$Tquality)/ncol(tab))
    Tab=data.frame(rowSums(tab)/ncol(tab))
    
    colnames(Tab)=c("Tquality")
    print(Tab)
    rownames(Tab)
  }
  else
  {
    rownames(Tab.sys)=NULL
    #welches 
    #tab.sort=tab[order(-as.integer(cut(tab$Sharpe,b=10)),-as.integer(cut(tab$MaxDD,b=20)), -as.integer(cut(tab$nTrades,b=100)),-tab$Cagr),];tab.sort
    
    Tab = data.frame(aggregate( Tquality ~ sym, data=tab, FUN=sum))
    Tab[,2]= Tab[,2]/ncol(tab)
  }
  res=Tab[order(-Tab[,"Tquality"]),,drop=F]
  print("################################################ RESULTS ############################")
  print(tabM)
  print("mean Tquality of universe component:")
  print (res)
  #gib die perf-listen für portfolios und signale zurück
  colnames(Tab.sys) = names(models)
  
  list( portfolio.mod= tabM, signal.mod = res,Signal.mod=Tab.sys)
}
#.........................................................................

if (F)
{
  signal.mod.n=names(mod$singleResults)[1]
  m=models.Results(models,quality.fn="model2Quality.calmar")
  m=models.Results(models) #gesamt-zeitraum - rein nach Tquality (calmar) ausgerichtet- sharpe spielt kaum eine rolle
  
  #wahlweise nach sharpe oder cagr ausgerichet
  #zeitraum frei wählbar
  #statt Tquality werden rankings ermitteln (siehe sortier reihenfolge in doc-doku und #sort-quality)
  m=models.Results(models,frame="2008",rank.for="sharpe")  #"cagr
  m=  models.Results(models,frame="::",rank.for="sharpe",ignore.signal.models=T,visual=F)
  
  m=models.Results(models,frame="::",rank.for="sharpe")  #"cagr
  
  m.bad= rownames(m[ m["Tquality"]<0.9,,drop=F] )      
}

#############################################################################################
#wieviel pct aller modell sind für das sym zur zeit t long ? 
#############################################################################################
multi.model.ranking<-function(models,longOnly=T)
{  
  sig<- NULL
  tab=for(mod.n in ls(models))
  {
    mod=models[[mod.n]]
    mod.sig = mod$Signal
    if (longOnly)
      mod.sig = iif(mod.sig <0,0,mod.sig)
    
    if (is.null(sig))
      sig<-mod.sig
    else  #signale aufaddieren
    {
      
      #print(mod.n)
      b=na.omit(merge(sig,mod.sig) )
      #   sig=sig[fromToS(b)];mod.sig=mod.sig[fromToS(b)]  #auf gemeinsame frame bringen
      frame=fromToS(b)
      for(col in (1:ncol(sig))) 
      {
        #print(tail(mod.sig,30))
        b=na.omit(merge(sig[frame,col],mod.sig[frame,col]))
        sig[index(b),col]<-rowSums(b) #sig[frame,col]+mod.sig[frame,col]
        
      }
      #print(tail(b,4))
    }
  }
  
  N=len(ls(models))
  sig = sig/N
  #sig=bt.apply.matrix(sig,function(col) col/N)   #nicht nötig
  print(tail(sig,30))
  
  sig
}

if (F)
{
  ranking=multi.model.ranking(models)
  View(ranking)
  purePlot(ranking)
}

#############################################################################################

######################################################################################
multi.top.model.ranking<-function(models,  n.top=12, k.top=16,longOnly=T,visual=F,period="years",model2Quality.fn="model2Quality")
{
  model2Quality.Fn=match.fun(model2Quality.fn)
  #zur sicherheit ...
  if (is.null(model2Quality.Fn))
    sag("bug at multi.top.model.ranking - unknown model2Quality.fn %s",model2Quality.fn,warte=T)
  
  n.top=min(nval(n.top),len(models))
  k.top=min(nval(k.top),len(models) )
  if (k.top==0)     k.top=n.top
  #ToDO  longOnly = F einfügen
  
  
  quality = model2Quality.Fn(models,longOnly)  #über alle modelle/symbole:  equity/maxdd
  if (visual)mPlots(quality)
  
  #alles signale zu allen symbolen von allen modellen in ein breites xts
  all.Signals = list2xts(models, "Signal")    
  #best.Signals <<- all.Signals;  best.Signals[] <<- 0
  print(tail(all.Signals))
  #View(tail(all.Signals))
  #seletiere pro sym die jeweils n.top besten modelle
  
  last.d = last(as.Date(index(quality)))
  Years = select.dates(quality,mode=period)#"years")
  if (last(Years) != last.d)
    Years=c(Years,as.character(last.d))
  
  symbols=colnames(models[[1]]$Signal)
  
  all.sym.Signals=
    foreach(sym=symbols,.combine="merge") %do%
{ 
  
  mP("############ > find.best.models for   %s ########### ",sym)
  #sym=data$symbolnames[2]  #TEST
  
  #Subselect:  qualität der einzelmodelle zum gegebenen sym,   - zum jahresende
  #browser()
  # if (sym=="BARCLAYS")
  #    browser()
  qualityS = quality[Years,  grep(sym,colnames(quality))]
  mP("nTopSelect from %d series for %s",ncol(qualityS),sym)
  best.syms= nTopK(qualityS,n.top,k.top)
  best.Syms = lag(best.syms)[-1]   #ich schnapp mir ja die best.Sym-Signale
  best.syms=NULL #blos nicht aus versehen auf zukünftige werte zugreifen
  #zwischen pre. und today in best.syms .. folglich muss ich um 1 laggen
  
  print(best.Syms)  
  #seletiere pro sym die jeweils n.top besten modelle
  #MM
  best.Signals=
    foreach(dats = as.Date(index(best.Syms)),.combine = "rbind") %do%
{
  #dats=as.Date("2004-12-31")  #TEST
  best = best.Syms[dats]
  
  pre.dats=last(best.Syms[index(best.Syms)< as.Date(dats)])
  if (len(pre.dats) == 0)
    all.Signals[as.Date(index(all.Signals))<=dats, coredata(best)]
  else
  {
    pre.dats=as.Date(index(pre.dats))
    all.Signals[as.Date(index(all.Signals))<dats & as.Date(index(all.Signals)) >= pre.dats , coredata(best)]
  }
}#die signal-summe der jeweils nTop besten Systeme
best.Signals = clean.matrix(best.Signals)
if (longOnly)
  best.Signals = bt.apply.matrix(best.Signals,function(col) iif(col >=0,col,0))

#verdichte die n.Top-Einzelsignale zu einem einzigen Signal
sym.ranking = xts(rowSums(best.Signals),index(best.Signals)) / n.top
colnames(sym.ranking)=sym
#sym.Signals[] = sign(rowSums(best.Signals))  #mehrheitsbeschluss
#alternativ :   sei nur long wenn alles n.top - modelle long sind
#sym.Signals[] = iif(rowSums(best.Signals)==n.top,1,0)
sym.ranking  #foreach-return
} #nächstes sym
#colnames(all.sym.Signals) = symbols
all.sym.Signals

}
#......................................................................................
#wenn ich historische modelle an Hand ihrer equity beurteilen soll .. welche nehm ich ?
#eine model2Quality.fn - Funktion für multi.top.model.ranking()  uns models.Results()
model2Quality<-function(model,longOnly=T, win=600,frame="")
{
  # eq = model2equities(model,longOnly)[frame]; 
  eq = model$equity[frame]
  
  ret.p=mROC(eq)
  mP("model2Quality")  
  eq = clean.matrix(eq)
  
  ddma=bt.apply.matrix(eq, runMax, n=win)   
  ddmi=bt.apply.matrix(eq, runMin, n=win) 
  
  ddmi=clean.matrix(ddmi); ddma=clean.matrix(ddma)
  maxdd = abs(ddma)-abs(ddmi)
  roc = mROC(eq,win) 
  res = roc / abs(maxdd)  #die eigentliche formel
  res = clean.matrix(roc)
  #  mP("slope200 zu mROD/maxdd")
  #slope200=rollRegressionXTS(res,200)
  #fct=bt.apply.matrix(res, rollRegressionXTS.forecast,win=200,n.ahead=31)
  
  #calmarRatio <- rollapplyr(ret.p, width=90, FUN="CalmarRatio", by.column=T, align = "right")
  #res=clean.matrix(calmarRatio)
  # mP("sharpe")
  #sharpe <- rollapplyr(ret.p, width=60, FUN="doSharpeRatio", by.column=T, align = "right")
  #clean.matrix(sharpe)
  
  res= roc
  res = clean.matrix(res)
  return(res)
}
#.....................................................................................

#eine model2Quality.fn - Funktion für multi.top.model.ranking() und models.Results()
model2Quality.calmar<-function(model,longOnly=T, win=600,frame="",period="years")
{
  #eq = model2equities(model,longOnly)[frame];  
  eq = model$equity[frame]
  ret.p=mROC(eq)
  mP("model2Quality.calmar")  
  eq = clean.matrix(eq)
  ret.p = mROC(eq)
  #die sample-zeitpunkte bilden schon mal den container der ergebnisse
  res=eq[select.dates(eq,period),1];res[]=0
  #stanze Dir die yährlichen zeitframes raus  (geht auch mit irgendeinem apply)
  for(i in 2:nrow(res))
  {
    i=3
    this.ret=ret.p[sprintf("%s::%s",DateS(res[i-1]),DateS(res[i] ))]
    res[,1]=CalmarRatio(this.ret)
    res[,1]=SharpeRatio(this.ret,Rf=0,p=0.95,FUN="StdDev")[1]
    res[,1]= maxDrawdown(this.ret)
    compute.sharpe(this.ret)
    
  }
  
  
  res = clean.matrix(res)
  return(res)
}
#...........................................................................................

model2Quality<-function(model,longOnly=T, win=600,frame="",period="years")
{
}
###########################################################################################
###########################################################################################
### welches portfolio modell ist am besten - kann also in aufeinanderfolgenden Jahen (fenserlänge 3 ..5) die meisten
#ranking-punkte erziehlen ... ?
find.best.portfoliomodel.helper<-function(models, period ="years",rank.for="mcalmar")
{
  Years = select.dates(models[[1]]$equity,mode=period)#"years")
  Years=sapply(Years,function(y) format(as.Date(y),"%Y"))
  
  yearTabs = NULL
  Frame=Years[1]
  for(Frame in Years)
  {
    m=models.Results(models,frame=Frame,rank.for=rank.for,ignore.signal.models=T,visual=F) 
    res.tab = m$portfolio.mod
    if (is.null(yearTabs)) yearTabs=res.tab else yearTabs = rbind(yearTabs, res.tab)
  }
  yearTabs
}

#######################################################################################
#automatisches Auswerten welche portfolio.models aus  der flat list models
#die besten sind -- entweder über den gesamt-Zeitraum einmal gemessen (sharpe,...)
#oder aber rollierend und dann akkumuliert.
#es werden sowohl nach Tquality als auch nach einem multi-dim -ranking
#siehe   #multiDimRanking    bewertet
#######################################################################################
#rank.for = spl("mcalmar,sharpe,cagr") oder ""
find.best.portfolio.model<-function(models, period ="years",visual=T,rolling.mode=T,rank.for="")
{
  #Betrachtung des Gesamt-Zeiraums
  names.old = names(models)
  #evtl.schönere Namen finden
  #names(models)=try(sapply(names(models),function(nam) rightOf(nam,"#")))
  #A) erst mal nicht rollierend:
  
  #hier wird nur auf Tquality geachtet .. Faktoren wie nTrades zählen nicht,
  if (rank.for=="")
     m=  models.Results(models,ignore.signal.models=T,visual=F) 
  else
  #hier zählt:   sharpe, maxDD, cagr, nTrades
    m=  models.Results(models,frame="::",rank.for=rank.for,ignore.signal.models=T,visual=F)  
    
    #m=  models.Results(models,frame="::",rank.for="mcalmar",ignore.signal.models=T,visual=F)  
  #m=  models.Results(models,frame="::",rank.for="sharpe",ignore.signal.models=T,visual=F) 
  #m=  models.Results(models,frame="::",rank.for="cagr",ignore.signal.models=T,visual=F) 
  
  tabM=m$portfolio.mod
  View(tabM[,-1])
  
  if (visual)
  {
    best.mod.name = tabM$modelName[1]
    print(tabM[1:3,-1])
    
    sprices=data$prices[,c(data$BENCH,data$SAFE)]  
    Equity=list2xts(models,"equity")
    colnames(Equity) = names(models)
    
    Cols=getColSet(ncol(Equity)+2)  
    
    #........................................................................
    norm_Win(1) 
    eqs=mNorm(merge( Equity[, (tabM$modelName)],sprices))
    colnames(eqs)=c(tabM$modelName,colnames(sprices))
    purePlot(eqs,main=sprintf("best is %s, brown",best.mod.name))
    best.mod =lines(Equity[,best.mod.name ],lwd=3,col="black")
    #best.mod =lines(Equity[,best.mod.name ],lwd=1,col="yellow")
    
    #...................................................................#scat3d
    norm_Win(1)
    # 3D Scatterplot with Coloring and Vertical Drop Lines
    library(scatterplot3d)
    s3d=scatterplot3d(tabM$Sharpe,tabM$Tquality, tabM$nTrades, pch=16, highlight.3d=TRUE,
                      type="h", main=sprintf("portfolio models \n%s",tabM$modelName[1]),angle=65, scale.y=0.7,box=F) 
    
    
    for(i in c(2:3))
      {
  
  #text(s3d$xyz.convert(tabM$Sharpe[i],tabM$Tquality[i],tabM$nTrades[i]), labels=tabM$modelName[i],cex=1,col="blue")
  points(s3d$xyz.convert(tabM$Sharpe[i],tabM$Tquality[i],tabM$nTrades[i]),col="cyan",lwd=2)
  
    }
    
    best.mod=i=1
    text(s3d$xyz.convert(tabM$Sharpe[i],tabM$Tquality[i],tabM$nTrades[i]), labels="best",cex=1.5,col="darkgreen")
    points(s3d$xyz.convert(tabM$Sharpe[i],tabM$Tquality[i],tabM$nTrades[i]),col="green",lwd=4)
  
  #...........................................................
  if (ncol(eqs)<50)
  {
  norm_Win(1)
  print("RiskReturnScatter")
  reqs=mROC(eqs)
  cn=colnames(eqs)
  #cn=tabM$modelName,c(data$BENCH,data$SAFE
  showNames = c(tabM$modelName[c(1)],c(data$BENCH,data$SAFE))
  cn2 = sapply(cn,function(name)ifelse(name %in% showNames, name, ""))  
  cn2[1]="PRODUCT"
  colnames(reqs) =cn2


  try(chart.RiskReturnScatter(reqs, main = sprintf("Compare models \n %s",tabM$modelName[1]), colorset = Cols, add.name=T))
  }
  #.........................................................
  
  print(rev(tabM$modelName))
  print("--------------------")
    mP("bestModel %s",tabM$modelName[i])
    x=lapply(names(tabM), function(slot) { val = tabM[[slot]][1]; mP("%s ---: ",slot);print(val)})
  }
  
  if (!rolling.mode)
    return(tabM)

  ###############################################################################
  #B) jetzt im Rolling - Mode :
  #jedes Jahr ein Ranking machen und dann die Einzelranking addieren ->
  #nur wer yährilch zu den besten zählt kann gewinnen
  
  #res=  find.best.portfoliomodel.helper(models,period="years",rank.for=rank.for)
  #tabM=res
  #View(tabM)  #jetzt hab ich nicht nur Tquality (absolutes) sondern auch rank als  Zahl für ein (relatives) ranking 
  ##########################################################################
  
  #welches signal-modell hat für welches sym im betrachteten zeitraum die beste Tquality erbracht ?
mP("---------------- rolling -analysis -------------- ")
  tab=tabM
  #aggregiere über alle zeiträume die Tquality auf das tupel  modelName+sym
  tab = data.frame(aggregate( Tquality ~ modelName, data=tab, FUN=sum))
  #sortiere das ergebnis
  tab=tab[order( -tab$Tquality),]
  
  #pick dir jeweils pro tupel nur den besten Wert/bzw. die besten 3 Modelle raus
  print("die besten Tquality-Modelle - über den gesamten Zeitraum betrachtet")
  print(tab)
  #..............................
  #welches signal-modell hat für welches sym im betrachteten zeitraum die beste Tquality erbracht ?
  tab=tabM
  #aggregiere über alle zeiträume die Tquality auf das tupel  modelName+sym
  tab = data.frame(aggregate(Tquality ~ modelName, data=tab, FUN=sum))
  #sortiere das ergebnis
  tab=tab[order(- tab$Tquality),]
  #pick dir jeweils pro tupel nur den besten Wert/bzw. die besten 3 Modelle raus
  print("die besten gerankten -Modelle - über den gesamten Zeitraum betrachtet")
  #View(tabM)
  print(tab)
  ################################################################
  
  ################################################################
  #Wie universell sind die signale einsetzbar ?
  bestN=3
  #Welches signal modell gehört denn am öftesten zu den 3 besten modellen eines sym ?
  #aggregiere über alle zeiträume die Tquality auf das tupel  modelName+sym
  tab=tabM
  best.Tquality.mod=
    foreach(mod = unique(tab$modelName), .combine="rbind") %do%
      head(tab[tab$modelName==mod,],bestN)
  View(best.Tquality.mod)
  best.mod= data.frame(aggregate( Tquality ~ modelName, data=best.Tquality.mod, FUN=sum))
  best.Tquality.mod=best.mod[order(-best.mod$Tquality),]
  print("best.Tquality.mod")
  print(best.Tquality.mod)
  ######################################################################################################
  #................................... das gleiche statt für Tquality für den rank
  ######################################################################################################
  tab=tabM
  #aggregiere über alle zeiträume die Tquality auf das tupel  modelName+sym
  tab = data.frame(aggregate( rank ~ modelName, data=tab, FUN=sum))
  #sortiere das ergebnis
  tab=tab[order( -tab$rank),]
  #pick dir jeweils pro tupel nur den besten Wert/bzw. die besten 3 Modelle raus
  print("die besten rank-Modelle - über den gesamten Zeitraum betrachtet")
  print(tab)
  ################################################################
  #Wie universell sind die signale einsetzbar ?
  bestN=3
  #Welches signal modell gehört denn am öftesten zu den 3 besten modellen eines sym ?
  #aggregiere über alle zeiträume die Tquality auf das tupel  modelName+sym
  tab=tabM
  best.rank.mod=
    foreach(mod = unique(tab$modelName), .combine="rbind") %do%
    head(tab[tab$modelName==mod,],bestN)
  View(best.rank.mod)
  best.mod= data.frame(aggregate(rank ~ modelName, data=best.rank.mod, FUN=sum))
  best.rank.mod=best.mod[order(-best.mod$rank),]
  print( "ROLLING best.rank.mod")
  print(best.rank.mod)
  
  best.rank.mod
}

#........................................

if (F)
{
  models= load.models("HuaFeb_4b","hua_World")
ls(models)

models= load.models("HuaFeb_3","hua_World")
ls(models)

#.........#MMNOW
models= load.models("EM4_MAERZ_C","test")
ls(models)


#load("Models/HuAFeb_4b/Results.xls/hua_World_signal.MA.3rank.slope300f.data")
  #m=dels2flatList(models,datafile="HuAWorld")
  
  best=find.best.portfolio.model(models, period ="years",visual=T,rolling.mode=F,rank.for="mcalmar") #"",cagr, sharpe

best.values = lapply(names(best), function(slot) { val = tabM[[slot]][1]; mP("%s ---: ",slot);print(val)})

#........................

pb=(data$prices[,data$BENCH])
short=-pb+2*nval(pb[1])
mchart(merge(pb,short))

}
#########################################################################################
#### hier werden jetzt die signal-modelle auf Effiziens gescreent
signal.model.analysis<-function(m,Frame="::",period = "years",rank.for ="mcalmar")
{
  #load("Models/HuAFeb_4/Results.xls/HuAWorld_F.data")
  #modell-liste flach machen
  #m=dels2flatList(models,datafile="HuAWorld_F")
  #oder  auch
  #  models= load.models("HuaFeb_4","HuaWorld_F")  #viele data-dateien aus ordner einlesen
  
  
  m=models.Results(models,frame=Frame,visual=F,rank.for=rank.for,ignore.signal.models=F) ####### ..######  ok
  #m2=models.Results(models,frame=Frame,visual=F,stars.for="cagr") ####### ..######  ok
  
  #welches modell sieht am besten aus wenn man nur nach den portfolio-equities geht
  View(m$portfolio.mod)
  #welches symbole werden wie gut (nach Tquality) von den signalen gefahren ?
  View(m$signal.mod)
  #welche signale taugen für welche symbole ?
  View(m$Signal.mod)
  
  
  #die tabelle mit allen per-werten pro signal.modell pro sym
  tab=m$Signal.mod
  if (F)
  {
  #multi-dimensionales -ranking (4 sort-kritierien ...) 
  if (stars.for =="sharpe")  #sort-quality
    tabM=tab[order(tab$modelName, -as.integer(cut(tab$Sharpe,b=10)),-as.integer(cut(tab$MaxDD,b=20)), -as.integer(cut(tab$nTrades,b=100)),-tab$Cagr),]
  else if ( stars.for =="cagr")
    #er sollte was die sharpe angeht schon zum oberen fünftel zählen, aber dann kommts vor allem auf cagr an  - alles andere ist nachgelagert
    tabM=tab[order(tab$modelName,-as.integer(cut(tab$Sharpe,b=5)),-as.integer(cut(tab$Cagr,b=50)), -as.integer(cut(tab$MaxDD,b=20)), -as.integer(cut(tab$nTrades,b=100))),]  
  else if ( stars.for =="mcalmar")
    #er sollte was die sharpe angeht schon zum oberen fünftel zählen, aber dann kommts vor allem auf cagr an  - alles andere ist nachgelagert
    tabM=tab[order(tab$modelName,-as.integer(cut(tab$Sharpe,b=5)),-as.integer(cut(tab$Cagr,b=50)), -as.integer(cut(tab$MaxDD,b=20)), -as.integer(cut(tab$nTrades,b=100))),]  
  }
  
  if (rank.for =="sharpe")  #sort-quality
    # tabM=tab[order(-as.integer(cut(tab$Sharpe,b=10)),-as.integer(cut(tab$MaxDD,b=20)), -as.integer(cut(tab$nTrades,b=100)),-tab$Cagr),]
    tabM=tab[order(as.integer(cut(tab$nTrades,b=3)),-as.integer(cut(tab$Sharpe,b=5)),as.integer(cut(abs(tab$MaxDD),b=20)) ,-tab$Cagr),]
  else 
    if ( rank.for =="cagr")
      #er sollte was die sharpe angeht schon zum oberen fünftel zählen, aber dann kommts vor allem auf cagr an  - alles andere ist nachgelagert
      #tabM=tab[order(-as.integer(cut(tab$Sharpe,b=5)),-as.integer(cut(tab$Cagr,b=50)), -as.integer(cut(tab$MaxDD,b=20)), -as.integer(cut(tab$nTrades,b=100))),]  
      tabM=tab[order(as.integer(cut(tab$nTrades,b=3)),-as.integer(cut(tab$Cagr,b=5)),-as.integer(cut(tab$Sharpe,b=20)) ,-tab$MaxDD),]
  else
    if ( rank.for =="mcalmar")
    {
      #  browser()
      ok.trades= which(tab$nTrades<400) #alles unter 400 turnover ist ok
      bad.trades= which(tab$nTrades>800)  #alles über 800 turnover geht gar nicht
      cut.i=as.integer(cut(tab$nTrades,b=2))  #mich interessiert nur die richtige hälfte
      cut.i[ok.trades] = 0
      cut.i[bad.trades] = 100
      tabM=tab[order(cut.i,-as.integer(cut(tab$Sharpe,b=10)),-as.integer(cut(tab$Tquality,b=20)) ,-as.integer(cut(as.integer(tab$MaxDD),b=20))),]
      View( tabM)
    }
  else
    sag("unknown rank.for",warte=T)
  
  #View(tabM)
  #tabM=tab[order(-as.integer(cut(tab$Sharpe,b=2)),tab$MaxDD, tab$nTrades,-tab$Cagr),];tabM
  #tabM=tab[order(-as.integer(cut(tab$Sharpe,b=2)),tab$nTrades),];tabM
  nSyms =len(colnames(models[[1]]$Signal))
  nMods = len(names(models))
  #stanze das ranking rein (und manifestiere damit die sortierung)
  tabM$rank=rep( c(nSyms:1)*100/nSyms, nMods)
  rownames(tabM)=c(1:nrow(tabM))
  #tabM=tabM[,-1]
  View(tabM)  #jetzt hab ich nicht nur Tquality (absolutes) sondern auch rank als  Zahl für ein (relatives) ranking 
  ##########################################################################
  
  #welches signal-modell hat für welches sym im betrachteten zeitraum die beste Tquality erbracht ?
  tab=tabM
  #aggregiere über alle zeiträume die Tquality auf das tupel  modelName+sym
  tab = data.frame(aggregate( Tquality ~ modelName+sym, data=tab, FUN=sum))
  #sortiere das ergebnis
  tab=tab[order(tab$sym, -tab$Tquality),]
  #pick dir jeweils pro tupel nur den besten Wert/bzw. die besten 3 Modelle raus
  #das beste signal.model für das sym .. wenns nach Tquality geht
  best.Tquality.Sym=  
    foreach(sym = unique(tab$sym), .combine="rbind") %do%
    head(tab[tab$sym==sym,],1)
  #pick dir jeweils pro tupel die drei besten Werte raus
  View(best.Tquality.Sym)
  print("du brauchst für Tquality blos folgende signale")
  print(    unique(best.Tquality.Sym$modelName)  )
  print("streiche")
  print(     names(models)[!(names(models)  %in% unique(best.Tquality.Sym$modelName))]  )
  
  ################################################################
  #Wie universell sind die signale einsetzbar ?
  bestN=3
  
  #Welches signal modell gehört denn am öftesten zu den 3 besten modellen eines sym ?
  #aggregiere über alle zeiträume die Tquality auf das tupel  modelName+sym
  
  best.Tquality.SymS=
    foreach(sym = unique(tab$sym), .combine="rbind") %do%
    head(tab[tab$sym==sym,],bestN)
  
  best.mod= data.frame(aggregate( . ~ modelName, data=best.Tquality.SymS, FUN=count))
  best.Tquality.mod=best.mod[order(-best.mod$sym),c(1,2)]
  print("best.Tquality.mod")
  print(best.Tquality.mod)
  ######################################################################################################
  #................................... das gleiche statt für Tquality für den rank
  ######################################################################################################
  tab=tabM
  #aggregiere über alle zeiträume die rank auf das tupel  modelName+sym
  tab = data.frame(aggregate( rank ~ modelName+sym, data=tab, FUN=sum))
  #sortiere das ergebnis
  tab=tab[order(tab$sym, -tab$rank),]
  #pick dir jeweils pro tupel nur den besten Wert raus
  #das beste signal.model für das sym .. wenns nach rank geht
  best.rank.Sym=
    foreach(sym = unique(tab$sym), .combine="rbind") %do%
    head(tab[tab$sym==sym,],1)
  print ("best.rank.Sym")
  print (best.rank.Sym)
  #du brauchst für dieses universum eigentlich blos folgende Modelle
  print("du brauchst für rank blos folgende signale")
  print(    unique(best.rank.Sym$modelName)  )
  print("streiche")
  print(     names(models)[!(names(models)  %in% unique(best.rank.Sym$modelName))]  )
  ################################################################
  #Wie universell sind die signale einsetzbar ?
  #pick dir jeweils pro tupel die drei besten Werte raus
  best.rank.SymS=
    foreach(sym = unique(tab$sym), .combine="rbind") %do%
    head(tab[tab$sym==sym,],bestN)
  
  #Welches signal modell gehört denn am öftesten zu den 3 besten modellen eines sym ?
  best.mod= data.frame(aggregate( . ~ modelName, data=best.rank.SymS, FUN=count))
  best.rank.mod = best.mod[order(-best.mod$sym),c(1,2)]
  print("best.rank.mod") 
  print(best.rank.mod) 
  
  #  View(Tab)
  #  colnames(Tab)
  #  t=Tab[order(Tab[,"sym"],-Tab[,"Tquality"]),,drop=F]
  #  View(t)
  #  purePlot(mNorm(data$prices[Frame,]))
  #  ls(models[["signal.1.smoothing"]])
  #  lines(mNorm(models[["signal.1.smoothing"]]$equity[Frame]),col="red",lwd=5)
  
  #---------------------------------------
  
}
if (F)
  signal.model.analysis(models,Frame ="::",period="years")
#source("Mlib/mlist.R")

mP("########### load Trade.R")
if (F)
   list_R_functions('MLib/Trade.r')
source("Mlib/sysRun.r")

#source("Mlib/Now.R")
