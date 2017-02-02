
##################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
#########################################################################
#######################################################################



#########################################################################
######################################################################### Baue die Soll-Werte für Optimierer
#
#erstellt eine Kopie des Dax bei der die y-werte -1,1 oder sind, je nachdem ob das Target short
#visual:  geht durch die Trend-Segmente eines dd-ZickZack.
#Markiert mit blau die Linie unterhalb des rechten SegmentEndes die nur noch dd entfernt ist.
#und markiert mit ROT den Schnittpunkt mit Blau - also ab dem der Trend noch dd-Ertrag bringt.
# ORANGE markiert den brk - der Segment-Entstehung - also der erst hinter dem linken-Segmentstart
#kommenden Punkt an dem ein DD zum linken Segment-Wert vorliegt.

# Bei nem gesunden Trend liegt die ORANGE Linie links von der ROTEN 

#
#Ergebnis:  Eine liste 
#oseg trägt die optimierte zerlegung - in dem bad-segmente gemerged wurde
#Bad-Segmente sind solche die zu wenig Tage dauern  - oder solche
#wo nach dem dd-Brk (hier erkennt er dass er im segment ist)
#nicht mals mehr dd bis zum Trendende kommt.  (orange rechts von Rot)
#target:  eine kopie des Dax- nur dass hier jedem Datum ein
#Wert zugeordnet wurde ob er long oder short oder flat sein soll
#flat wird mit 0.1  bzw. -0.1 am Ende des Segmtens dargestellt- wenn
#es nur noch dd vom rechten Rand entfernt ist.

#oder long ist

Target_<-function( Dax, dd,ds=NULL,visual=F)
{
  #Dax = Cl(Dax)
  
  #segmt = segmt  = mZigZag(Dax,dd)
  if (is.null(ds))
    ds=  DdownDupWalkAhead(Dax,dd,dd,visual=F)  
  segmt =ds$peak
  newSeg=segmt
  brk = ds$brk
  if (visual)
  {
    plot(Dax)
    lines(ds$peak,col="red")
    lines(ds$brk,col="blue",type="h")
    print(newSeg)
    browser()
  }
  pos__ = Dax
  pos__[]=0
  
  #  del = c()
  preSegDeleted<-F
  #Segment-Tabellen
  Win=data.frame( post= as.Date(index(segmt[-1])), pre= as.Date(index(segmt[-len(segmt)])))
  #über alle Segmentabschnitte
  ##pos1=apply(Win,1, 
  for(yi in c(1:nrow(Win)))
    #function(x) 
  {
    
    x=as.matrix(Win)[yi,]
    post=x[1]; pre=x[2]                             
    
    lin=Dax[sprintf("%s::%s",pre,post),1]
    m__=sign(as.numeric(Dax[post,])-as.numeric(Dax[pre,]))
    lineq= diff(log((lin)));lineq[1]=0
    
    if (F && pre>="2009-02-09") ############## Hier debug-Dates wählen####################################################
    visual=T
    #if (yi > 2)
    #  visual= T
    
    if (visual )#|| yi ==nrow(Win))
    {
      len(newSeg)
      #plot(newSeg)
      mP("%s %s %s",pre,post,brk)                              
      #norm_Win(1)
      plot(Dax)
      lines(Dax[sprintf("%s::%s",pre,post)],type = "h",col="lightblue")
      lines(Dax,col="black")
      lines(segmt,col="orange",lwd=1)
      lines(newSeg,col="green",lwd=2)
      browser() 
    }   
    
    #plot(cumsum(-(rev(lineq))))
    thresh = as.numeric(last(cumsum(lineq)))-(m__*dd/100)
    #thresh = thresh#*m__
    #dd%-Range unterhalb des rechten Rands
    if (visual)
    {
      #norm_Win(2)
      plot(cumsum(lineq))
      abline(h=thresh,col="blue")
    }
    #oberhalb und                 tags vorher - unterhalb der schwelle
    lt =  cumsum(lineq)- thresh
    above=lt[which(lt>=0)]
    below = lt[which(lag(lt,-1)<=0)]
    offline = na.omit(merge(above,below))
    if (len(offline) ==0)
      offs =as.Date(index(first(above)))
    else
      #offline=lineq[which(cumsum(lineq)- thresh>=0 & (lag(cumsum(lineq),-1)- thresh<=0 | lag(cumsum(lineq),1)- thresh<=0) ]
      #ab diesem Datum liegen alle Werte in der dd%-Range unterhalb des rechten Rands
      offs = as.Date(index(last(offline)))   
    if (len(below) ==0)
      offs =as.Date(index(first(above)))
    
    pos__[sprintf("%s::%s",pre,post),]<- m__; 
    pos__[sprintf("%s::%s",as.Date(offs),post),]<- m__/10.0;
    #head(pos__)
    
    
    # browser()
    brk = as.Date(index(last(ds$brk[as.Date(index(ds$brk)) <= as.Date(post)])))
    if (len(brk) ==0)
      brk = pre
    
    if (visual)  
    {
      abline(v=as.POSIXct(offs),col="red")
      abline(v=as.POSIXct(brk),col="orange")
    }
    
    if (visual)
    {
      print(len(lineq))
      print(fromTo(lineq))
    }
    bad = F
    if (len(lineq)<5 )
    { if (visual) mP("BAD: trend too short ")
      bad =T
    }
    
    if (brk > offs )  #orange rechts von root
    {
      if (visual) 
      {mP("BAD: trend equity too low")
       print (brk)
       print(offs)
      }
      bad = T
    }
    if (visual)
    {
      print("~~~~~~~")
      browser()
    }
    
    #preSegDeletd =  !(len(newSeg[pre]) >0)
    
    
    
    if (bad && !preSegDeleted && len(newSeg[pre]) >0)
    {
      #  userStop("kill",pos="M1")
      # del<-c(del, c(pre,post))
      
      mP("+++++++++kill ++++++++++ %s %s",pre,post)
      if (visual)
      {
        plot(newSeg)
        lines(newSeg[sprintf("%s::%s",pre,post)],col="green")
      }
      preSegDeleted <- T
      hist_=  newSeg[as.Date(index(newSeg)) <= as.Date(post)]
      
      
      killI = xts2Index(pre,newSeg)
      if (killI > 0)
        newSeg <- newSeg[-killI]
      killI = xts2Index(post,newSeg)
      if (killI > 0)
        newSeg <- newSeg[-killI]
      
      #browser()
      #fals vor pre  in newSeg ein gleichsinniges (z.B. short) Trendstück liegt
      #muss dies mit dem jetzigen short verschmolzen werden, falls 
      #post tiefer liegt.
      
      th=tail(hist_,3)
      
      if (len(th)>= 3)
      {
        
        if (visual)
        {
          plot(newSeg)
          lines(hist_,col="darkblue")
          browser()
        }
        
        delSegM = sign(as.numeric(th[3])-as.numeric(th[2]))    
        DelSegM = th[2:3]
        
        if (visual)
          lines(DelSegM,col="orange")
        
        if (killI < len(newSeg))
          PostDelSegM = newSeg[c(killI:(killI+1))]
        else
          PostDelSegM = newSeg[killI]
        if(visual)  
          
          lines(PostDelSegM,col="red")
        prelastSegM = sign(as.numeric(th[2])-as.numeric(th[1]))
        PrelastSegM  = th[1:2]
        
        if (visual)
          lines(PrelastSegM,col="magenta")
        
        NewSeg = newSeg[c(as.Date(index(first(PrelastSegM))),as.Date(index(first(PostDelSegM))))] 
        mNewSeg = sign(as.numeric(last(NewSeg))-as.numeric(first(NewSeg)))
        mPreNewSeg = -mNewSeg  
        
        if (visual)
          lines(NewSeg,col="brown")
        
        if (visual) 
        { print("c#c"); browser()} ###########################################################
        
        #
        if (prelastSegM < 0 && as.numeric(first(DelSegM)) <  as.numeric(first(PostDelSegM)))
          
          #if (T|| prelastSegM == 1) #zwei short flanken 
          #if (as.numeric(segmt[post]) < as.numeric(th[2]))
        {#verschmelzen
          print("---#############>")
          if (visual) browser()
          killI = xts2Index(as.Date(index(first(PostDelSegM))),newSeg)
          if (killI > 0)
            newSeg <- newSeg[-killI]
          #post wieder drankleben
          newSeg=append(newSeg,first(DelSegM))
          if (visual) plot(newSeg)
        }
        
        #low retten
        if (mPreNewSeg < 0 && as.numeric(first(PrelastSegM)) > as.numeric(last(DelSegM)))
          
          #if (T|| prelastSegM == 1) #zwei short flanken 
          #if (as.numeric(segmt[post]) < as.numeric(th[2]))
        {#verschmelzen
          print("+++#############>")
          if (visual) browser()
          killI = xts2Index(as.Date(index(first(PrelastSegM))),newSeg)
          if (killI > 0)
            newSeg <- newSeg[-killI]
          killI = xts2Index(as.Date(index(first(DelSegM))),newSeg)
          if (killI > 0)
            newSeg <- newSeg[-killI]
          #post wieder drankleben
          newSeg=append(newSeg,last(DelSegM))
          if (visual) plot(newSeg)
        }
        
        if (prelastSegM > 0 && as.numeric(first(DelSegM)) >  as.numeric(first(PostDelSegM)))
          
          #if (T|| prelastSegM == 1) #zwei short flanken 
          #if (as.numeric(segmt[post]) < as.numeric(th[2]))
        {#verschmelzen
          print("---###+++##########>")
          if (visual) browser()
          killI = xts2Index(as.Date(index(first(PostDelSegM))),newSeg)
          if (killI > 0)
            newSeg <- newSeg[-killI]
          #post wieder drankleben
          newSeg=append(newSeg,first(DelSegM))
          if (visual) plot(newSeg)
        }
      }
    }
    else 
      preSegDeleted <- F
    
    
    m__<-m__*-1}
  
  if (visual)
  {
    plota(Dax)
    Col = iif(ts$target > 0, ifelse (ts$target ==1, "red","orange" ), ifelse(ts$target ==-1,"darkblue","blue")) 
    plota.lines(Dax,col=col,lwd=2)
  }
  
  return(list(target=pos__, oseg = newSeg))
}

Target<-cmpfun(Target_) #compilier das Teil

if (F)
{
  mGetTickers("Dax",online=F)
  plot(Dax)
  Dax = Cl(Dax)["2003-08-01::"]
  dd=10
  visual=T
  
  #Dax=na.omit(mNorm(Dax))
  
  ts=  Target(Dax,9,visual=F)
  plot(ts$oseg)
  
  
  Col = iif(ts$target > 0, ifelse (ts$target ==1, "red","orange" ), ifelse(ts$target ==-1,"darkblue","blue")) 
  
  plota(Dax)
  plota.lines(Dax,col=Col,lwd=2)
  
  ##########################################################
  source("MLIB/TradeClasses.r")
  gv <<- T2$new(PortfolioName = "gv", bench="Dax",visual = F, online=F) #T! MAIN 
  save(gv,file='T2data/gv.Rdata')   ;#  Load("gv")
  
  gv1  <<- T1$new(name="IFO",visual = T, online=F ) #T! MAIN 
  
  #############################################################
  global_ParTable=NULL  #l?sche das Trainingsged?chtnis
  global_StartDate = 1
  
  
  heute=Sys.Date()
  heute = fromTo(gv$t0data$prices)[2]
  heute=as.Date(heute)-2
  
  gv$update(heute) #schreibt auch schon mal in den global_ParTable und liefert damit dem TrainIndicator via mlist() notwendige Startwerte
  global_ParTable
  global_StartDate
  trace(TrainIndicator)
  
  TrainIndicator(heute,opti="DEoptim")
  #trace(mlist)
  global_ParTable
  
  
  ######################################## super
  
  Load("gv")
  Dax=gv$t0data$prices["1997::","Dax"]
  Mdax =gv$t0data$prices["1997::","MDAX"]
  prices = mNorm(merge(Dax,Mdax))
  
  prices=prices[,"Dax"]
  purePlot(prices)
  scd=0
  k=20
  k=40
  
  k=20
  prices= na.omit(prices)
  scdfast=score.diff(ZLEMA(prices,n=k),k=k)
  signal = ifelse( sign(scdfast[,1])>0,1,-1)
  plotSigPrice(signal=signal,prices=prices,indi=list(scdfast, ZLEMA(prices,n=k)))
  
  
  
  
  ######################################################### schrott
  #diff aus Mdax und Dax
  
  scd=score.diff(prices,k=100)  
  signal2 = sign((scd[,2]-scd[,1]))  
  
  plotSigPrice(signal=signal2,prices=Dax,indi=list(scd1, scd))  
  
  signal3 = signal2*signal
  plotSigPrice(signal=signal3,prices=Dax,indi=list(scd1, scd))  
  
  
  scdM = scd[,"Dax"]  
  MAX <-  rollapplyr(scdM, width=60, FUN=max, by.column=T, align = "right") 
  MIN<- rollapplyr(scdM, width=60, FUN=min, by.column=T, align = "right")  
  extr = ifelse(abs(MAX-scdM) < abs(MIN-scdM), MIN, MAX)
  
  purePlot(MAX,MIN,scdM)
  
  purePlot(scdM,scdM-extr)
  plot(extr)
  mdax1 = extr-scdM
  
  purePlot(mdax1,scdM,extr)  
  
  dMax=score.diff(dscdMdax,k=10)
  purePlot(dMax)
  purePlot(scaleTo(dMax,range(Mdax,na.rm=T)),Mdax)  
  ###############  super:                    #MM_IndIdee1
  Load("gv")
  Dax=gv$t0data$prices["1997::","Dax"]
  ls(data)
  DAX=na.omit(data[["Dax"]]["1997::"])
  Dax = Cl(DAX)
  fromTo(Dax)
  
  hl=  HighLows(mNorm2(Dax),maxdd=2,visual=T)
  DaxLo= Dax[hl$lows]
  DaxHi= Dax[hl$highs]
  
  dLo=  rollapplyr(DaxLo, width=5, FUN=diffCount, by.column=T, align = "right") 
  dHi=  rollapplyr(DaxHi, width=5, FUN=diffCount, by.column=T, align = "right")   
  dHiLo=  rollapplyr(Dax[hl$hl], width=5, FUN=diffCount, by.column=T, align = "right") 
  purePlot(dLo,dHi,scaleTo(Dax,range(dHi,na.rm=T)))
  dLo_=Dax;  dLo_[]=NA;  
  dLo=mmerge(dLo_,dLo)[,2];colnames(dLo)="dLo"
  dHi=mmerge(dLo_,dHi)[,2];colnames(dHi)="dHi"
  dHiLo=mmerge(Dax,dHiLo)[,2];colnames(dHiLo)="dHiLo"
  
  signal=dLo
  #teste unterschiedliche signale
  signal[] = sign(sign(dLo)+sign(dHi));colnames(signal)="signal"
  signal[] = sign(sign(dLo));colnames(signal)="signal"
  
  signal[] = sign(sign(dHi));colnames(signal)="signal"    #sehr gut
  signal[signal ==0]<-NA;signal[] = m.ifna.prev(signal)
  
  plotSigPrice(signal=signal,prices=Dax,indi=list(merge( dHi,dLo),signal))  
  
  #library(PortfolioAnalytics)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~++++++++++++++++++
  #                       das Wunder -Modell           !!!!!!!!!!!!!!!!!!!!!!1
  #[1] "320 Trades"
  #[1] "Haltedauer 13.015625 Tage"
  #[1] "sharpe: 2.436368"
  #[1] "Cgar -0.245654"
  #[1] "MaxDD -11.159295"
  #[1] "calmar 0.220134
  #Wo ist der Bug ??
  ls(data)
  xtsPrice = Cl(Dax)
  
  prices=Cl(Dax)
  
  slopes=diff(prices,20)
  signal = sign(slopes)
  signal = ifelse(ZLEMA(na.omit(sign(slopes)),n=5) >= 0,1,-1) #schlecht
  
  
  zlem=ZLEMA(na.omit(sign(slopes)),n=10,ratio=0.1);colnames(zlem)=spl("zlem")  #
  signal = ifelse(zlem >= 0,1,0)   #super
  signal=ifelse(abs(slopes)*10000<1, 0,signal)  #flat 
  #signal=lag(signal,20)
  plotSigPrice(signal=signal,prices=prices,indi=list(merge( slopes),zlem))  
  
  data$weight=prices
  
  data$weight[] = signal
  capital = 100000
  data$weight[] = (capital / prices) * bt.exrem(data$weight)
  zlem = bt.run(data, type='share', capital=capital, trade.summary=T)
  
  plotbt.custom.report.part1(zlem,mr,buy.hold,trade.summary=T)
  
  
  
  
  
  
  #fromTo(xtsPrice)
  rsi2 = bt.apply.matrix(xtsPrice, RSI, 2)
  signal= iif(rsi2 < 50, 1, -1)	
  plotSigPrice(signal=signal,prices=Dax,indi=list(merge( rsi2,50)))  
  
  #.........................................................
  
  data$prices=data.info(data)
  prices=data$prices
    
  data$weight=data$prices
  data$weight[] = NA
  
  slopes=diff(data$prices,20)
  #signal = sign(slopes)
  #signal = ifelse(ZLEMA(na.omit(sign(slopes)),n=5) >= 0,1,-1)
  
  zlem= bt.apply.matrix(sign(slopes), ZLEMA, n=2,ratio=0.1);
  signal=lag(iif(zlem >=0,1,-1))
  
  
  data$weight = lag(signal)
  
  
  data$weight[] = NA;
  data$weight[] = lag(signal)
  strategy = bt.run.share(data, clean.signal=F)
  
  capital = 100000
  data$weight[] = (capital / prices) * bt.exrem(data$weight)
  wunder = bt.run(data, type='share', capital=capital, trade.summary=T)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Der Boden der      Hi-Close    fällt im Uptrend kontinuierlich nach unten . 
  #Wenn er  wieder ansteigt  ist mit dem LongTrend vorbei.
  
  ClDax=Cl(mNorm(DAX))
  ClDax.weekly = to.weekly(ClDax)
  longIndikator =  Hi(mNorm(DAX))-Cl(mNorm(DAX))  #MM_HICL_BODEN
  long.weekly =to.weekly(longIndikator)
  
  lw1 = Lo(long.weekly)
  pw1 = Cl(ClDax.weekly)
  
  purePlot(pw1,scaleTo(lw1,range(pw1)))
  lw2= lw1[HighLows(lw1,maxdd=5,visual=F)$lows]

  purePlot(pw1,scaleTo(lw2,range(pw1)))
   
  lw3= runMin(lw2,2)  #kostet delay
  purePlot(pw1,scaleTo(lw3,range(pw1)))
  
  #lw4= ZLEMA(na.omit(lw3),n=15)  #kostet delay
  #purePlot(pw1,scaleTo(lw4,range(pw1)))
  
  slopes=diff(lw3,2)
  signal = -sign(slopes)
    
  #signal = ifelse(ZLEMA(na.omit(sign(slopes)),n=5) >= 0,1,-1)
  #signal=ifelse(abs(slopes)*10000<1, 0,signal)  #flat 
  
  signal=merge(ClDax,signal)
  
  na.locf(signal,na.rm=FALSE)
  ifna.prev(signal)
  
  test=mmerge(ClDax,signal,m.ifna.prev(signal)); colnames(test)=spl("dax,sig,sigPrev")
 head(test["2000"],300)
  test["2003-10"]
  options(ds) 
  unique(test)
  signal[] = ifna.prev(signal)
  #signal[signal < 1] <-0
  
  signal = ifelse(ZLEMA(na.omit(signal),n=5) >= 0,1,-1)
  
  plotSigPrice(signal=signal,prices=Dax,indi=list(merge( lw4,slopes),signal))  
  
  
  clr()
  train <- data.frame( merge( dax1, l2))# )  #lag(l2))  )
  colnames(train) = c("Y","xt")
  tail(train)
  
  mod= lm(Y ~ xt, train)
  summary(mod)
  plot(mod, which=1)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  signal[] = sign(dHiLo);colnames(signal)="signal"
  signal[signal ==0]<-NA
  signal[] = ifna.prev(signal)
  plotSigPrice(signal=signal,prices=Dax,indi=list(merge(dHiLo)))  
  #
  
  ##############################
  Load("gv")
  Dax=gv$t0data$prices["1997::","Dax"]
  Dax = mNorm(Dax)
  ts=  Target(Dax,10,visual=F)
  plot(ts$oseg)
  plot(Dax)
  
  Col = iif(ts$target > 0, ifelse (ts$target ==1, "red","orange" ), ifelse(ts$target ==-1,"darkblue","blue")) 
  
  plota(Dax)
  plota.lines(Dax,col=Col,lwd=2)
  #-----------------------------
  
  
  
}
WinR = NULL

do.skid_<- function(x,seg,minSlid=10) {  
  #regression mit fensterlängesteuerung:
  #a) angelehnt an den zigzag-breaks in seg
  #b) und  verkürzt durch die linReg-Güte
  lx =as.Date(index(last(x))) #das jüngste Datum
  lenX=dim(x)[1]
  #browser()
  cat(toString(lx),"\n")
  
  if (is.null(seg))
    daySinceBrk = lenX#-minSlid
  else
    daySinceBrk =daysAtSegmt (lx,seg, x)# Dax .. "2002-12-02"
  
  #mP("daysSinceBrk %d",daySinceBrk)
  daySinceBrk =min(daySinceBrk,lenX) #längenbegrenzung aufs sichtbare
  
  colnames(x) = c("Close")
  
  R=sapply(seq(minSlid,daySinceBrk), FUN=function(daySinceBrk)
  {
    X <- data.frame(x[seq(lenX-daySinceBrk+1, lenX)], xt=seq(1-daySinceBrk, 0))    #TrainingsDaten
    mod= lm(Close ~ xt, X)   #Close ist der SpaltenName der Y-Reihe
    #~~~ jetzt noch automatisch den schlitten verkürzen wenn die
    #Daten keine gute Gerade bilden...
    
    modsum=summary(mod)
    r=modsum$r.squared
    
    f=0
    #f = try(pf(modsum$fstatistic[1], modsum$fstatistic[2], modsum$fstatistic[3],       lower.tail = FALSE) )
    #mP("%s:  %d r %f ,  %f",toString(lx),daySinceBrk,r,f)
    return(r)
  }
  )
  #  browser()
  #  plot(R)
  #Fensterlänge des besten R-Wertes:
  wlen=first(which.max(R))
  bestR = R[wlen]
  BestR[lx] <<- bestR
  WinR[lx] <<-wlen
  X <- data.frame(x[seq(lenX-wlen+1, lenX)], xt=seq(1-wlen, 0))    #TrainingsDaten
  mod= lm(Close ~ xt, X)
  #browser()
  return( coef(mod)[2] )  #Gib die Steigung der Geraden zurück
}    
do.skid<-cmpfun(do.skid_)


######################################################
#wieviel Tage sind seid dem break vor date vergangen ?
######################################################

daysAtSegmt_<-function( date, segmt, Dax)
{
  
  heutei =which( as.Date(index(Dax)) == as.Date(date))
  nexti=0
  nexti= first(which( as.Date(index(segmt)) >= as.Date(date)))
  prei= nexti-1
  if (prei<=0)
    prei=1
  PreiDate = as.Date(index(segmt[prei]))
  #Prei = which( as.Date(index(Dax)) == PreiDate)
  res = as.Date(date)-as.Date(PreiDate)
  #MM_TODO:   das aktuelle segment kann nur gefunden werden, wenn
  #schon brk vorbei ist.
  return (as.numeric(res))
}
daysAtSegmt<-cmpfun(daysAtSegmt_) #compilier das Teil



#einem Indikator kann durchaus die Position des letzten Trendwechsels
#mitgegeben werden.  Dann kann sich z.B. daran ein loakler lm ausrichten  - ausserdem kann gleich die Güte des Indikators am Target gemessen werden

#predict geht auch:    xy=predict(mod, newdata=predictData )
#Wende eine beliebige Funktion - rollierend auf eine Zeitreihe  an (überlappende Fensterbreite 20):  
if (F)
{
  Dax=xtsPrice
  BestR <<- Dax; BestR[]=NA; WinR <<- Dax; WinR[]=NA
  slopes <<- rollapplyr(Dax, width=100, FUN=do.skid, ts$oseg, minSlid=10, by.column=T)  
  
  slopes=  ifna.prev(slopes)
  #prüfen !!!
  slopes[is.na(slopes)]
  
  signal = ifelse(slopes >= 0,1,-1)
  
  plotSigPrice(signal=signal,prices=Dax,indi=list(slopes,WinR))#, BestR,ts$oseg))  
  
  signal = ifelse(ZLEMA(sign(slopes),na.rm=T,n=70) >= 0,1,-1)
  plotSigPrice(signal=signal,prices=Dax,indi=list(slopes,ts$oseg))  
  
}
################################################################################
#Handelssystem für den wlen-automatisierten regressions-schlitten nach Target-dd
#Segmentierung.
################################################################################

slid<-function(Dax,maxSlid=100,minSlid=25,dd=9,visual=F)
{
  if (dd >0)
    ts=  Target(Dax,dd,visual=F)
  else
  {
    ts=list();ts$oseg=NULL
  }
  
  BestR <<- Dax; BestR[]=NA; WinR <<- Dax; WinR[]=NA
  #browser()
  slopes <<- rollapplyr(Dax, width=maxSlid, FUN=do.skid, ts$oseg, minSlid=minSlid, by.column=T)  
  
  slopes=  ifna.prev(slopes)
  #prüfen !!
  #slopes[is.na(slopes)]
  
  if (visual)
  {
    signal = ifelse(slopes >= 0,1,-1)
    
    plotSigPrice(signal=signal,prices=Dax,indi=list(slopes,WinR))#, BestR,ts$oseg))  
    
    signal = ifelse(ZLEMA(sign(slopes),na.rm=T,n=70) >= 0,1,-1)
    plotSigPrice(signal=signal,prices=Dax,indi=list(slopes,ts$oseg))  
  }
  return(slopes)
}
#########################################################
if (F)
  slid(xtsPrice)

if (F) 
  slid(Dax=price,dd=10,visual = T)
################################################################################
# diesmal werden die Segmentierungen nicht wie Target (bzw. ZigZag ) vorgegeben,
#sondern monatlich via T-Test-Analayse (lib bfast) uns dynamischer Segmentierung
#bei der Regression 
#gefunden
################################################################################
#nutze library(bfast) und mach damit eine automatisierte Brk-Analyse im rollierende Fenster (wird z.b. mit Monatsdaten beschickt)
################################################################################
#wichtig:   nicht h  - sondern die Fensterlänge stellt die Anzahl der 
#breaks ein ...  (pro Fensterlänge werden es bei h = 0.2 ) meist 2 bis 3 Segmente -- da ist eher wichtig - ob ich im Fenster 30 Daten, oder 150 liegen hab.
brkAnalysis<-function( xtsPrice, minWlen=150,h=0.2,visual =F)
{
  library(bfast)
  
  lenNa =len(xtsPrice[is.na(xtsPrice)])
  if (lenNa > 0)
  { mP("Warning: There are %d NA at xtsPrice",lenNa)
    xtsPrice = na.omit(xtsPrice)
  }
  lastbreak=""
  lastlbd = as.Date(index(first(xtsPrice))) #dummywert für  neu 
  doSTL = F
  #~~~~~~~~~~~ pre-init der return-felder
  allSlopes=xtsPrice  #hier notier ich die Slopes
  allSlopes[] = NA
  
  allBreaks=xtsPrice  #hier notier ich die breaks wenn ich sie bemerke (-5 heißt: vor 5 tagen eine Break bemerkt)
  allBreaks[] = NA
  
  allWlens=xtsPrice #hier notier ich die Fensterlängen jeden Tag (wieviel Tage vom linken segment -Rand entfernt)
  allWlens[] = NA
  
  #++++++++++
  #minWlen = 12*2+1
  #minWlen = 150
  if (dim(xtsPrice)[1] < minWlen)
  {
    print("Wrong usage:  xtsPrice has less than minWlen data .. ")
    return(NULL)
  }
  
  allI=seq(minWlen, dim(xtsPrice)[1])
  
  for( mi in allI)
  {
    #   mi=549   #TEST
    
    #  browser()
    m=index(xtsPrice)[mi]
    TODAY=as.Date(m)
    mframe=sprintf("%s::%s",lastbreak,m)
    #if (TODAY == as.Date("1997-06-23"))
    #      browser()
    print(mframe)
    price=xtsPrice[mframe,1]
    
    if (dim(price)[1]< minWlen) #lohnt sich ne regression?
      price = xtsPrice[seq(max(mi-minWlen-1,1),mi)]  
    
    ## MM_TODO:  ändert sich was durch diese neue Ergänzung (ohne die waren die Fensterüberlang)
    if (dim(price)[1]> minWlen) #
      price = xtsPrice[seq(max(mi-minWlen+1,1),mi)]  
    
    
    price.ts <- ts(as.vector(price),frequency=12)
    # browser()
    if (T)
    {
      if (doSTL)
      {
        library(forecast)
        #tsp(price.ts)
        Price.stl <- stl(price.ts,s.window="periodic")
        if (F) #visual)
        { 
          try(dev.off(),T)
          plot(Price.stl,main=sprintf("STL Decomposition of %s",colnames(price)))
        }
      }
      
      #Price.bfast <- bfast(price.ts,h=0.2,max.iter=1,season="harmonic")#"none
      
      #Price.bfast <- bfast(price.ts,h=0.9,max.iter=1,season="none")
      
      Price.bfast <- tryCatch({
        res=bfast(price.ts,h=h,max.iter=1,season="none",breaks=1)#,hpc="foreach")# << #####################
      }, error = function(err) {
        return(NULL)     }
                              ,finally =function()
                                return(res))
      #browser()  
      if (!is.null(Price.bfast))
      {
        
        if(visual &&F)
        {
          plot(Price.bfast,type="components",ylim=c(3,max(GSPC.monthly)+1),main="bfast Breakpoints and Components")
          plot(Price.bfast,type="trend",ylim=c(1,max(price.ts)+1),main="S&P 500 with bfast Trend Breakpoints")
          labels=breakdates(Price.bfast$output[[1]]$bp.Vt,format.times=TRUE)
          print(labels)
          #  plot(Price.bfast,type="all")
        }
        #übersetzung nach xts
        brks=breakpoints(Price.bfast$output[[1]]$bp.Vt)$breakpoints
        Brks= as.Date(index(price[brks]))  #weil monthly - sonst: as.Date
        
        print(Brks)
        niter <- length(Price.bfast$output) # nr of iterations
        if (niter > 0 && len(Brks)>0)
        {
          out <- Price.bfast$output[[niter]]  # output of results of the final fitted seasonal and trend models and #nr of breakpoints in both.
          #plot(Price.bfast,type="trend",largest=TRUE)
          #browser()
          res = tail(as.vector(out$Tt),5)   #die Steigung des aktuellen Trends
          #plot(out$Tt)
          #lines(price.ts)
          
          res = (as.numeric(res[5])-as.numeric(res[1])) #ein improvisierte "erfassung" der Steigung am rechten Rand
          lbd=last(Brks)
          
          nowWlen=minWlen
          nowWlen = try( as.numeric(as.Date(TODAY)-as.Date(lbd)))
          
          #if (len(nowWlen)
          try({allWlens[TODAY] = nowWlen})
          
          neu =""
          #wie erkenn ich einen neuen Break:
          #    entweder  hat er mehr als 5 Tage Abstand zum letzten lbd - oder -
          #  besser: es ist auf einmal ein Brek mehr da- als vorher.
          #if (TODAY==as.Date("1997-06-24"))
          #  browser()
          
          if (abs(as.numeric(as.Date(lbd) -as.Date(lastlbd)))  > 7)  #manchmal schwanken die Segmente ein oder 2 Tage-  darum 7 Tage als Trenndifferenz für "neues Segment" gefunden
            try({
              neu ="NEW"
              allBreaks[TODAY] = toString(as.Date(lbd)) #notiere heute das Datum des neuen Breaks
              lastlbd = lbd
            })
          
          mP("Today %s:  found  %s  brk at %s",toString(TODAY),neu,toString(lbd))
          #browser()
          
          #  allBreaks[lbd] = res  #nur den letzten Break eintragen
          if (F)
            allBreaks[!is.na(allBreaks)]
          
          try({allSlopes[TODAY] = res})   #das ist realistischer
          #das Vorzeichen der steigung kann schwanken- auch wenn kein neues
          #Segement hinzukommt (im Zeitverlauf) - 
          #alterativ könnte man auch festlegen, dass ein Vorzeichenwechsel
          #erst mit dem Hinzukommen eines neuen Breaks erlaubt wird
          
          #na.omit(allBreaks)
          #browser()
          if (visual)
          {
            try(dev.off(),T)
            plot(price)
            TREND=price;TREND[]=as.vector(out$Tt)
            lines(TREND,col="red",lwd=2) #der Trend
            abline (v=as.POSIXct(Brks),col="blue")
            print(Brks)
            z=  SMA(price, k=nowWlen)
            z2=  ZLEMA(price, k=nowWlen)
            
            lines(z,col="green")
            lines(z2,col="orange")
            
          }
          lastbreak = lbd
        }
      }
    } 
  }
  #  allBreaks = na.omit(allBreaks)
  return(list(allSlopes=allSlopes, allBreaks=allBreaks, allWlens = allWlens))
  
}

#brkAnalysis<-cmpfun(brkAnalysis_)

if (F)
{
  
  Load("gv")
  Dax=gv$t0data$prices["1997::","Dax"]
  Mdax =gv$t0data$prices["1997::","MDAX"]
  prices = mNorm(merge(Dax,Mdax))
  GDAXI=prices[,1]
  xtsPrice=GDAXI
  fromTo(xtsPrice)
  #####################
  Load("gv")
  GDAXI = gv$t0data$Dax
  xtsPrice = Cl(GDAXI)
  #xtsPrice = Cl(to.monthly(xtsPrice))
  dim(xtsPrice)
  
  frame="2007-11-01::2010-01-01"
  frame="2008-09-24::2010-01-01"
  
  frame="2003-08-01::"
  frame="1997::"
  
  #xtsPrice=log(xtsPrice[frame])
  xtsPrice=xtsPrice[frame]
  xtsPrice = mNorm(xtsPrice)
  
  print(len(xtsPrice[,1]))
  #S1= s1[frame]
  #plot(scaleTo(xtsPrice[frame],c(-1,1)))
  #lines(S1,type="h",col=ifelse(signal > 0,"red","blue"))
  
  #PAR# library("foreach")
  library("strucchange")
  #PAR# library(snow)
  
  #help(makeCluster)  
  #cl <- makeCluster(c("localhost","localhost"), type = "SOCK")
  #clusterApply(cl, 1:2, get("+"), 3)
  #PAR# .Platform$OS.type
  
  
  #PAR# require(doSNOW)  #~~~~~~~~~~ parallel framework - vieleicht buggy
  #PAR# cl <- makeSOCKcluster(3)  
  ## do something like the lines below if necessary:
  #PAR#   clusterEvalQ(cl, library(bfast,xts)) # load any necessary libraries
  #PAR# #clusterExport(cl, list("mu", "sigma", "m3", "m4")) # copy any necessary objects
  
  ## you can monitor your processes (using, e.g., the 'top'
  ## command on a unix shell, and see multiple R
  ## sessions created 
  
  #PAR# registerDoSNOW(cl) # register foreach backend
  
  pRun<-function()
  {
    bA <<- brkAnalysis(xtsPrice, 50, h=0.2);  # 200, 0.2  - wie 150.02
  }
  system.time( pRun())  ##################################
  bA2 = brkAnalysis(xtsPrice, 150, h=0.1) ##################################
  
  #PAR#  stopCluster(cl)
  
  
  ls(bA)
  
  slopes =bA$allSlopes
  wlens = bA$allWlens
  
  #s1 = slopes
  #slopes=na.locf(slopes,na.rm=FALSE)
  #block-ausrichtung und synchrones na entfernen
  block=na.omit(merge(slopes,xtsPrice))
  slopes=block[,1]
  xtsPrice = block[,2]
  signal = sign(slopes)
  #plot(xtsPrice)
  #plot(scaleTo(xtsPrice,c(-1,1)))  
  #lines(signal,type="h",col=ifelse(signal > 0,"red","blue"))
  #lines(scaleTo(xtsPrice,c(-1,1)))
  
  #mto.daily(monthdata=xtsPrice)
  plotSigPrice(signal=signal,prices=xtsPrice,indi=list(slopes,wlens))  
  
  
  bA$allBreaks
  #Warum ist die Phasenlage nicht genauer ????
  #Was passiert mit der Perf. wenn ich den trendwechsel nicht auf 
  #Break lege, sondern auf TODAY - also dem Tag, an dem ich den Break erkenne ?
  #Welche Auswirkungen haben Veränderungen am Parameter h.
  #Kann ich aus den saison-Infos was lernen (intra-trend-Oszillator ?)
  #Wandel ich tatsächlich xts->ts mit frequency=12 ???  - Auswirkungen
  #Passt die Datums-Umwandlung ts->xts ???
  set.seed(123)
  tt <- ts(rnorm(20), start = Sys.Date())  # c(1980,01))
  as.Date(as.numeric(time(tt)), origin = "1970-01-01")
  #  y=ts(x[,2],frequency=252,start=c(2000,1)) <== Create a time-series object in R
  #> plot(y,type='l',xlab='year',ylab='rtn')
  #> title(main='Daily returns of Apple stock: 2000 to 2009')
  
  #noch mal überpürfen:
  #na.locf(y,na.rm=FALSE)  
  #ifna.prev <- function(y)  - entferne die rechten NA- Werte durch ihren Vorgänger
  
}



#############################################

if (F)
{
  library( ifultools)
  #linearSegmentation()
  #andere Philosophie wie fbast:
  #Hier wird eine Kurve durch Geradestücke angenähert -
  #sobald ein Segment hinzukommt ändert sich auch die Steigung
  #im letzten Segment !!
  #Bei fbast ändert sich oft nur der Achsenabschnitt ...
  #Scheint sehr schnell zu sein.
  
  ## obtain some data with approximately 
  ## piecewise-linear trends 
  x <- seq(0,2*pi,length=100)
  y <- sin(x)
  
  ## perform linear segmentation with aspect ratio 
  ## dilation using a 5 degree tolerance and 5 
  ## point windows 
  z <- linearSegmentation(x, y, n.fit=5, angle.tolerance=5, aspect=TRUE)
  ## plot the data and the estimated change-points 
  plot(x, y)
  abline(v=x[z], lty=2)
  
  lmf=linearFit(x, y, n.fit=5, angle.tolerance=5, aspect=TRUE,
                method="last", fit=lm)
  
  plot(lmf)
  summary(lmf)
  
  library( ifultools)
  #Bei fbast ändert sich oft nur der Achsenabschnitt ...
  #Scheint sehr schnell zu sein.
  
  ####################
  #Segmentierung einer Zeitreihe nach Regressions-Winkel-Brüchen..
  ######################################################
  
  
  plot(xtsPrice)
  
  x = Index(xtsPrice)
  y = coredata(xtsPrice)
  n.fit=15
  angle.tolerance=99
  z <- linearSegmentation(x, y, n.fit=n.fit, angle.tolerance=  angle.tolerance, aspect=TRUE)
  plot(x,y,type="l")
  abline(v=x[z], lty=2)
  #: lm, lmsreg, and ltsreg
  lmf=linearFit(x, y, n.fit=n.fit, angle.tolerance=  angle.tolerance, aspect=TRUE,
                method="last", fit=lm)
  
  
  
  set.seed(100)
  ix <- seq(2048)
  stackPlot(x=ix,
            y=data.frame(sunspots[ix], cumsum(rnorm(length(ix)))),
            xlty=2, ylab=list(text=c("sunspots","walk")))
  
  
}  

################################################################################
#linearSegmentation()
#andere Philosophie wie fbast:
#Hier wird eine Kurve durch Geradestücke angenähert -
#sobald ein Segment hinzukommt ändert sich auch die Steigung
#im letzten Segment !!
############################################################
#Segmentierung einer Zeitreihe nach Regressions-Winkel-Brüchen..
######################################################

linSegAnalysis<-function( xtsPrice, minWlen=150, n.fit=15,angle.tolerance=99, do_skid=F,do_spline=F,do_VAR=F, visual =F)
{
  library(ifultools)
  
  lenNa =len(xtsPrice[is.na(xtsPrice)])
  if (lenNa > 0)
  { mP("Warning: There are %d NA at xtsPrice",lenNa)
    xtsPrice = na.omit(xtsPrice)
  }
  lastbreak=""
  lbd=as.Date(index(first(xtsPrice))) #??
  lastlbd = as.Date(index(first(xtsPrice))) #dummywert für  neu 
  doSTL = F
  #~~~~~~~~~~~ pre-init der return-felder
  allSlopes=xtsPrice  #hier notier ich die Slopes
  allSlopes[] = NA
  
  allBreaks=xtsPrice  #hier notier ich die breaks wenn ich sie bemerke (-5 heißt: vor 5 tagen eine Break bemerkt)
  allBreaks[] = NA
  
  allWlens=xtsPrice #hier notier ich die Fensterlängen jeden Tag (wieviel Tage vom linken segment -Rand entfernt)
  allWlens[] = NA
  
  #++++++++++
  #minWlen = 12*2+1
  #minWlen = 150
  if (dim(xtsPrice)[1] < minWlen)
  {
    print("Wrong usage:  xtsPrice has less than minWlen data .. ")
    return(NULL)
  }
  
  allI=seq(minWlen, dim(xtsPrice)[1])
  
  for( mi in allI)
  {
    #mi=249   #TEST    
    # browser()
    
    m=index(xtsPrice)[mi]
    TODAY=as.Date(m)
    mframe=sprintf("%s::%s",lastbreak,m)
    #if (TODAY == as.Date("1997-06-23"))
    #      browser()
    print(mframe)
    price=xtsPrice[mframe,1]
    
    if (dim(price)[1]< minWlen) #lohnt sich ne regression?
      price = xtsPrice[seq(max(mi-minWlen-1,1),mi)]  
    
    if (dim(price)[1]> minWlen) #
      price = xtsPrice[seq(max(mi-minWlen+1,1),mi)]  
    
    Y <- as.vector(price)
    X <-In
    dex(price)
    
    len(X)
    len(Y)
    # browser()
    
    if (visual)
    {
      if (do_spline)
        norm_Win(3)
      else
        norm_Win(2)
      
      #der Übersichtsplot
      plot(xtsPrice)
      Col=ifelse(is.finite(allSlopes) & allSlopes > 0,"red", ifelse( allSlopes==0,"gray","lightblue"))
      Col[as.Date(index(Col))<as.Date(index(first(price)))] ="white"
      Col[index(Col)>index(last(price))] <-"white"
      
      #browser()
      lines(xtsPrice,type="h",col=Col)
      #lines(price,type="h",col="green")
      lines(xtsPrice)
      winS = append(first(price),last(price))
      #browser()
      abline (v=as.POSIXct(as.Date(index(winS))),col="blue")
      
    }
    if (T)
    {
      if (doSTL)
      {
        library(forecast)
        #tsp(price.ts)
        Price.stl <- stl(price.ts,s.window="periodic")
        if (F) #visual)
        { 
          try(dev.off(),T)
          plot(Price.stl,main=sprintf("STL Decomposition of %s",colnames(price)))
        }
      }
      
      lmLast <- tryCatch({
        res=linearFit(X, Y, n.fit=n.fit,angle.tolerance=  angle.tolerance, aspect=TRUE,
                      method="last", fit=lm)
        
        # << #####################
      }, error = function(err) {
        return(NULL)     }
                         ,finally =function()
                           return(res))
      #browser()  
      if (!is.null(lmLast))
      {
        brks <- linearSegmentation(X, Y, n.fit=n.fit, angle.tolerance=  angle.tolerance, aspect=TRUE)
        if (len(brks) < 2)
          brks = c(first(X),brks)
        
        if( visual && F )
        {
          plot(X,Y,type="l")
          abline(v=X[brks], lty=2,col="blue")
        }
        
        #übersetzung nach xts
        Brks= as.Date(index(price[brks]))  #weil monthly - sonst: as.Date
        
        #print(Brks)
        
        if (len(Brks)>0)
        {
          res = coef(lmLast)[2]   #die Steigung des aktuellen Trends
          #if (is.na(res))
          #  res = diff(xtsPrice[Brks])[2]/as.numeric(diff(Brks))
          
          lbd=last(Brks)
          
          nowWlen=minWlen
          nowWlen = try( as.numeric(as.Date(TODAY)-as.Date(lbd)))
          if (nowWlen < 15)  #nicht weniger als 15 (willkürlich)
            nowWlen=15
          
          try({allWlens[TODAY] = nowWlen})
          
          neu =""
          #wie erkenn ich einen neuen Break:
          #    entweder  hat er mehr als 5 Tage Abstand zum letzten lbd - oder -
          #  besser: es ist auf einmal ein Brek mehr da- als vorher.
          #if (TODAY==as.Date("1997-06-24"))
          #  browser()
          
          if (abs(as.numeric(as.Date(lbd) -as.Date(lastlbd)))  > 7)  #manchmal schwanken die Segmente ein oder 2 Tage-  darum 7 Tage als Trenndifferenz für "neues Segment" gefunden
            try({
              neu ="NEW"
              allBreaks[TODAY] = toString(as.Date(lbd)) #notiere heute das Datum des neuen Breaks
              lastlbd = lbd
            })
          
          mP("Today %s:  found  %s  brk at %s",toString(TODAY),neu,toString(lbd))
          #browser()
          
          #  allBreaks[lbd] = res  #nur den letzten Break eintragen
          if (F)
            allBreaks[!is.na(allBreaks)]
          
          if (do_skid) ### mein eigener Regressions-Schlitten-- nimmt die Wlen wo R- am besten ist
          {
            #price=tail(price,nowWlen)  #Fensterlängen-Steuerung übernehmen
            #print(nowWlen)
            
            skidOut= do.skid(price,NULL,n.fit)
            if(F)
              if (sign(skidOut) != sign(res)) 
                visual=T
            else
              visual=F
            
            try({allSlopes[TODAY] = skidOut})   #das ist realistischer
          }
          else
            
            if (do_spline)
            {
              library(splines)  #~~~~~~~~~~~~~~~~~~~~~~~
              Y=coredata(price)
              X=seq(1,len(price))
              
              #ispl <- polySpline(interpSpline(Y  ~ X,  bSpline = F))## <<<<<<<<<<<<<<  #####<
              ispl <- smooth.spline(Y, X,df=50)
              
              pred=predict(ispl)
              Fit = price
              Fit[]=NA
              len(Fit)
              
              Fit[round(pred$x)] = pred$y  
              if (visual)
              {
                #plot(price)
                #lines(na.omit(Fit),col="red",lwd=2)
                
                plot(pred,type="l",col="blue")
              }
              xx=seq(max(X),max(X)+40)
              pred=predict(ispl,xx)
              y1=pred$y[1]
              y2 = pred$y[2]
              res=y2-y1
              if(visual)
              {
                plot(pred,type="l")              
                browser()
              }
              try({allSlopes[TODAY] = res})
            }
          else
            if (do_VAR)
            {
              #plot(xtsMon)
              #price=tail(price,nowWlen)  #Fensterlängen-Steuerung übernehmen ist hier nicht clever
              print(len(price))
              pri=ts(merge(price,seq(1,len(price))),frequency=12)
              priceMod <-VAR(pri,p=1,type="none")  #MM_TODO:  noch ander  VAR-type ausprobieren "trend","both", ...
              pred <- predict(priceMod, n.ahead = n.fit, ci = 0.95)  #anderes Signifikanz-Intervall ausprobieren  
              
              fcst=pred$fcst[1]
              Res=data.frame(fcst)
              res = last(Res[,1])-first(Res[,1])
              if (visual)
              {
                #plot(priceMod)
                plot(price)
                lines(price,col=ifelse(res < 0,"blue","orange"))
                outPred=mysts = xts(Res, TODAY+1:dim(Res)[1])
                lines(outPred[,1],col="red")
                lines(outPred[,2],col="blue")
                lines(outPred[,3],col="green")
                
                #purePlot(outPred[,c(1,2,3)])
                
                browser() 
              }
              try({allSlopes[TODAY] = res})
              
            }
          else
            try({allSlopes[TODAY] = res})   #das ist realistischer
          
          
          #das Vorzeichen der steigung kann schwanken- auch wenn kein neues
          #Segement hinzukommt (im Zeitverlauf) - 
          #alterativ könnte man auch festlegen, dass ein Vorzeichenwechsel
          #erst mit dem Hinzukommen eines neuen Breaks erlaubt wird
          
          #na.omit(allBreaks)
          #browser()
          if (visual && !do_skid && !do_spline && !do_VAR )  
          {
            # try(dev.off(),T)
            plot(price)
            fitted = fitted(lmLast)
            #head(fitted(lmLast))
            
            fittedi = as.numeric(names(fitted))
            TREND=price;TREND[fittedi]=as.vector(fitted)
            lines(TREND,col="red",lwd=2) #der Trend
            abline (v=as.POSIXct(Brks),col="blue")
            
            #str(lmLast)
            
            pred1<-predict(lmLast,newdata=data.frame(x=fittedi),interval="prediction")
            #  pred1<-predict(lm1,newdata=testFaith,interval="prediction")
            
            #head(pred1)
            #len(pred1[,"fit"])
            #len(price)
            
            # lmLast$fitted.values
            
            #off=coef(lmLast)[1]
            #TREND1=price;TREND1[fittedi]=as.vector(pred1[,"fit"])
            
            dd = as.numeric(pred1[1,"upr"]-pred1[1,"fit"] )
            TREND2=price;TREND2[fittedi]=TREND[fittedi]-dd#as.vector(pred1[,"lwr"])
            TREND3=price;TREND3[fittedi]=TREND[fittedi]+dd#as.vector(pred1[,"upr"])
            
            #lines(TREND1,col="green",lwd=2)
            lines(TREND2,col="orange")
            lines(TREND3,col="orange")
            
            
            
            print(Brks)
            #   z=  SMA(price, k=nowWlen)
            #    z2=  ZLEMA(price, k=nowWlen)
            
            # lines(z,col="green")
            #    lines(z2,col="orange")
            mP("skid %f",skidOut)
            browser()     
          }
          lastbreak = lbd
        }
      }
      lastbreak = lbd
    } 
  }
  #  allBreaks = na.omit(allBreaks)
  return(list(allSlopes=allSlopes, allBreaks=allBreaks, allWlens = allWlens))
}

##############################
#   das beste Ergebnis kommt - wenn ich den ganzen Regressions-Kram-Samt Fenstersteuerung
#   vergress und mit das VAR()-Modell  (do_VAR=T) schnapp .. sieht (visual = T) wirlich sinnig aus !!
#   Schnell bei Crashes aber Tolerant bei Rauschen !!! - wenige Signale gute Perf - 
#   mit ZLEMA-Signalfilter noch leicht zu verbessern
##############################


if (F)
{
  Load("gv")
  Dax=gv$t0data$prices["1997::","Dax"]
  Mdax =gv$t0data$prices["1997::","MDAX"]
  prices = mNorm(merge(Dax,Mdax))
  xtsPrice=prices[,2]
  
  fromTo(xtsPrice)
  plot(xtsPrice)
  #####################
  
  Load("gv")
  GDAXI = gv$t0data$Dax
  xtsPrice = Cl(GDAXI)
  #xtsPrice = Cl(to.monthly(xtsPrice))
  dim(xtsPrice)
  
  frame="2006-12-01::2010-01-01"
  
  frame="2007-11-01::2010-01-01"
  frame="2008-09-24::2010-01-01"
  
  frame="2003-08-01::"
  frame="1997::"
  
  xtsPrice=xtsPrice[frame]
  xtsPrice = mNorm(xtsPrice)
  print(len(xtsPrice[,1]))
  fromTo(xtsPrice)
  
  plot(xtsPrice)
  new_Win(1)
  
  BestR=NULL
  

  pRun1<-function()
  {
    bA <<- linSegAnalysis(xtsPrice, 150,  n.fit=15,angle.tolerance=99, do_skid=F,do_spline=F,do_VAR = T, visual=F )
  }
  system.time( pRun1())  ##################################
  
  slopes =bA$allSlopes
  #  head(slopes)
  wlens = bA$allWlens
  #  head(wlens)
  #block-ausrichtung und synchrones na entfernen
  block=mmerge(slopes,xtsPrice,wlens)
  fromTo(block)  
  slopes=block[,1]
  xtsPriceO = block[,2]
  wlens = block[,3]
  #new_Win(1)
  signal = sign(slopes)
  
  signal = ifelse(ZLEMA(sign(slopes),na.rm=T,n=20) >= 0,1,-1)
  signal=ifelse(abs(slopes)*10000<1, 0,signal)
  
  plotSigPrice(signal=signal,prices=xtsPriceO,indi=list(slopes,wlens))  
  
}




myVAR_ <- function(price)    #eine Beispielfunktion für rollapplyr
{
  cat(sprintf("myVAR_: %s ",toString(as.Date(index(first(price))))),"\n")
  
  #price=mNorm(price) #NEU
  
  pri=ts(merge(price,seq(1,len(price))),frequency=12)
  priceMod <-VAR(pri,p=1,type="none")  #MM_TODO:  noch ander  VAR-type ausprobieren "trend","both", ...
  pred <- predict(priceMod, n.ahead = 15, ci = 0.95)  #anderes Signifikanz-Intervall ausprobieren  
  
  fcst=pred$fcst[1]
  Res=data.frame(fcst)
  res = last(Res[,1])-first(Res[,1])
  return(res)
}
myVAR<-cmpfun(myVAR_) #compilier das Teil


sys.VAR_<-function(arg,visual=F)  #braucht globale:  prices   und   AllsSMAcombinations
{
  runN<<-runN+1
  
  width=arg[1]; 
  wZLEMA=arg[2]; 
  sigNoise = 2#arg[3];
  
  slopes <- rollapplyr(prices, width=10, FUN=myVAR, by.column=FALSE, align = "right")
  
  block=mmerge(slopes,prices)
  fromTo(block)  
  slopes=block[,1]
  xtsPriceO = block[,2]
  #new_Win(1)
  signal = -sign(slopes)
  
  signal=ifelse(abs(slopes)*10000<sigNoise, 0,signal)
  signal = ifelse(ZLEMA(signal,n=100) >= 0, 1,-1)
  
  sig <- signal
  ret <- m.Run(prices,sig)
  nt=  numTrades(sig)$allT
  q= quality(ret)
  Transaktionskosten =nt/ncol(prices)/80
  #Transaktionskosten =Transaktionskosten *10.0  #sonst viel zu hoch im optimizer
  mP("%d:  wDiff %d  wZLEMA %d  sigNoise %d,  %d Trades- >q:%f =>%f", runN, wDiff,wZLEMA,sigNoise,nt,q,Transaktionskosten )
  
  if (visual)
    plotSigPrice(signal=signal,prices=prices,indi=list(slopes))  
  
  
  return(q-Transaktionskosten)
}

sys.VAR<-cmpfun(sys.VAR_) #compilier das Teil




#### sehr einfacher Optimierer -- vollständige Suche
optimize.Grid.VAR<-function()
{
  
  Load("gv")
  GDAXI = gv$t0data$Dax
  xtsPrice = Cl(GDAXI)
  #xtsPrice = Cl(to.monthly(xtsPrice))
  dim(xtsPrice)
  
  frame="2006-12-01::2010-01-01"
  
  #frame="2007-11-01::2010-01-01"
  #  frame="2008-09-24::2010-01-01"
  
  #  frame="2003-08-01::"
  frame="1997::"
  
  frame="2009::"
  
  xtsPrice=xtsPrice[frame]
  xtsPrice = mNorm(xtsPrice)
  print(len(xtsPrice[,1]))
  fromTo(xtsPrice)
  plot(xtsPrice)
  
  
  runN <<-0
  prices<<-xtsPrice
  #(wDIFF,wZLEMA,sigNoise)
  lev=list(seq(10,100,10),  seq(2,30,2),1:2)
  runs = len(combine2(combine2(seq(10,100,10), seq(2,30,2)),1:12))
  mP("Expect time for %d runs ",runs)
  
  ores <<- gridSearch(sys.DIFF, levels=lev)
  
  ovalues<<-ores$values[is.finite(ores$values)]
  plot(ovalues)
  
  
  besti=which.max(ovalues)
  best=ores$values[besti]
  mP("BestVal %f",best)
  print(ores$levels[besti])
  
  sys.VAR(unlist(ores$levels[besti]),T)
  
  
  return(ores)  
}
if (F)
  k=optimize.Grid.DIFF() #MM_optimize_DIFF  #hier starten für die Optimierung



######################################


##############################
#   noch viel krasser  geht (NICHT) es mit DIFF als Signalgeber ab!!!
if (F)
{
  prices = xtsPrice
  runN<<-1
  
  sys.DIFF(c(20,15,2),visual=T)
  plot(prices)
}


# wenn man jetzt mit Grid-Search mal den Parameterraum absucht findet man jede Menge Ertragreiches
##############################


sys.DIFF_<-function(arg,visual=F)  #braucht globale:  prices   und   AllsSMAcombinations
{
  runN<<-runN+1
  
  wDiff=arg[1]; 
  wZLEMA=arg[2]; 
  sigNoise = arg[3];
  
  slopes=diff(prices,wDiff)
  
  signal = sign(slopes)
  signal = ifelse(ZLEMA(sign(slopes),n=wZLEMA,na.rm=T) >= 0,1,-1)
  signal=ifelse(abs(slopes)*10000<sigNoise, 0,signal)
  
  #browser()
  #signal = ifelse(ZLEMA(na.omit(sign(slopes)),n=wZLEMA) >= 0,1,-1)
  
  sig <- signal
  
  ret <- m.Run(prices,sig)
  nt=  numTrades(sig)$allT
  q= quality(ret)
  
  Transaktionskosten =nt/ncol(prices)/80
  Transaktionskosten =Transaktionskosten *10.0  #sonst viel zu hoch im optimizer
  mP("%d:  wDiff %d  wZLEMA %d  sigNoise %d,  %d Trades- >q:%f =>%f || %f", runN, wDiff,wZLEMA,sigNoise,nt,q,Transaktionskosten,q-Transaktionskosten)
  
  if (visual)
    plotSigPrice(signal=signal,prices=prices,indi=list(slopes))  
  
  
  return(q-Transaktionskosten)
}
sys.DIFF<-cmpfun(sys.DIFF_) #compilier das Teil



#### sehr einfacher Optimierer -- vollständige Suche
optimize.Grid.DIFF<-function(xtsPrice,today=Sys.Date(),wlen=500)
{
  
  runN <<-0
  prices<<-tail(xtsPrice[as.Date(index(xtsPrice))<today],wlen)
  #(wDIFF,wZLEMA,sigNoise)
  
  lev=list(seq(10,100,10),  seq(2,30,2),1:2)
  hl=HotLags(prices,n=100,visual=T)
  lev=list(unique(c(hl,seq(10,100,10))),  seq(2,30,2),1:2)
  #lev=list(hl,  seq(2,30,2),1:2)
  
  runs = len(combine2(combine2(hl, seq(2,30,2)),1:12))
  mP("Expect time for %d runs ",runs)
  
  ores <<- gridSearch(sys.DIFF, levels=lev)
  
  ovalues<<-ores$values[is.finite(ores$values)]
  plot(ovalues)
  
  
  besti=which.max(ovalues)
  best=ores$values[besti]
  mP("BestVal %f",best)
  print(ores$levels[besti])
  
  sys.DIFF(unlist(ores$levels[besti]),T)
  
  #MM_TODO Bessere Top-selektion mit ausWertg() s.u.
  #return(ores) 
  print("HotLags")
  cat(hl)
  print("level")
  print(lev)
  return(ores$levels[besti])
}
if (F)
  k=optimize.Grid.DIFF(xtsPrice) #MM_optimize_DIFF  #hier starten für die Optimierung


if (F)
{
  #~~~~~~~~~~~~~~~~~~~~~~~~~
  #http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
  hl=HotLags(xtsPrice,n=100,visual=T)
  
  
  beer<-ts(coredata(xtsPrice),freq=5)
  
  db=decompose(beer)
  plot(db)
  plot(xtsPrice)
  plot(db$trend,col="red")
  
  beerfct=HoltWinters(na.omit(db$trend), gamma=FALSE) #ich rechne den forecast nicht auf dem Orginial sondern nur auf dem Trend-Anteil -- läuft auf eine lineare Verlängerung des db$trend raus.
  
  plot(beerfct)
  plot.forecast(forecast.HoltWinters(beerfct, h=10))
  fct=forecast.HoltWinters(beerfct, h=10)
  res=last(fct$mean)-first(fct$mean)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~
}


################################################################################

#Baustelle:  kontinuierliches Lernen:....

#~~~ ein rollapplyR- Workhorse:
roll.HOLTW<-function(xtsPrice)
{
  #http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
  price.ts<-ts(coredata(xtsPrice),freq=5)
  #Trend-Saison-Rauschen.Aufsplittung
  db=decompose(price.ts)
  if (visual)
  {
    plot(db)
    
    #plot(xtsPrice)
    #plot(db$trend,col="red")
  } 
  
  HW=HoltWinters(na.omit(db$trend), gamma=FALSE) #ich rechne den forecast nicht auf dem Orginial sondern nur auf dem Trend-Anteil -- läuft auf eine lineare Verlängerung des db$trend raus.
  
  #plot(HW)
  plot.forecast(forecast.HoltWinters(HW, h=10))
  fct=forecast.HoltWinters(HW, h=10)
  res=last(fct$mean)-first(fct$mean)
  return(res) #die Richtung des forecasts
}

#~~~~~~~ Beispiel für Indi:

#indi.HOLTW<-function(arg,par = NULL) #in Trade.R

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#################################################################################################
# patched die gleichnamige Methode aus TradeClasses.r
#################################################################################################
indi.finishOLD<-function(signal,prices,indi="???",visual=F,Indi=list(),main="")
{ 
  sig <- signal
  
  ret <- m.Run(prices,sig)
  nt=  numTrades(sig)$allT
  q= quality(ret)
  
  Transaktionskosten =nt/ncol(prices)/80
  ret[is.na(ret)]=first(ret[!is.na(ret)])
  
  S=compute.sharpe(ret)
  #Transaktionskosten =Transaktionskosten *10.0  #sonst viel zu hoch im optimizer
  mP("%s %d:   %d Trades- >q:%f =>%f || %f  ~%f~", indi, runN,nt,q,Transaktionskosten,q-Transaktionskosten,S)
  
  #parameter-names im title
  p=sapply(names(mlist()),FUN=function(nam) {sprintf("%s=%s",nam,toString(first(mlist()[[nam]])))})
  title=paste(p,collapse=",")
  title = paste(main,title,sep=": ", toString(as.Date(index(last(prices)))))
  
  if (visual)    
    plotSigPrice(signal=signal,prices=prices,indi=Indi,main=title)  
  
  #compute.sharpe(mROC(prices))/compute.sharpe(ret)
  
  #browser()
  #return (compute.sharpe(ret))
  return(q-Transaktionskosten)
}

#Du kannst Ihn aufrufen wenn Du ihm in arg. u.a. den clos gibst
#mlist() wird am Anfang aufgerufen und schreibt die Standardparameter (par...) in die globale Tabelle:
#  global_ParTable <<- data.table (objectId = global_objectId, time=global_StartDate,  par=I(list(list(...))))
#  Wenn es unter dem Tupel-Key "global_objectId, time=global_StartDate" - schon ne Zeile findet - nimmt es lieber die
#  Parameter die dorst stehen.

if (F) #MM_TRAIN_NOW
{
  #------ Vorbereitende globale Varibalen-Setzung damit das ini.Framework funktioniert:
  global_ParTable <<- NULL
  global_StartDate <<- Sys.Date()
  global_objectId <<-"Dax TREND inid.MACD"
  global_arg <<- list(clos=xtsPrice)
  runN<<-0
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Trage unter global_objectId#global_StartDate  die Parameter in global_ParTable ein:
  mlist(wShort=c(50,30,70) , wLong=c(200,160,250))
  mlist()
  global_ParTable
  #jetzt kann ich das indi-Framework nutzen:
  ##arg =  list( name = Ti$name, bench=Ti$bench, Ti= Ti, clos=clos, dat=data, dclos=ret) #mal ein vollst?ndiges environment
  indi.MACD(arg=list(clos=xtsPrice))
  #ich kann die global_ParTable aber komplett umlaufen indem ich die arg-liste brav direkt übergebe:
  indi.MACD(global_arg,list(wShort=20,wLong=55))
  
  #  TrainIndicator(opti="GRID",indiName = "indi.MACD")  <<####################### MMTRAiNI
  
  
  
  
  #tsp(price.ts)
  ep=endpoints(xtsPrice,"quarters")
  ep = ep[ep>0]
  epDates = as.Date(index(xtsPrice[ep]))
  
  TRAINwlen = 500
  trainData=data.frame(list(Date="dd", Par=list(a=24,b=3)))
  for(epd in epDates)
  {
    epd=epDates[5] #TEST
    if (as.numeric(epd-as.Date(index(first(xtsPrice)))) > TRAINwlen)
    {
      optiParams=  optimize.Grid.DIFF(xtsPrice,today=epd, wlen=TRAINwlen)
      browser()
    }
    
  }
}
###################################################################################
#Baustelle: optimale best Parameter-Selektion
#### jedem Parameter den Erfolg zuordnen
ausWertg<-function()
{
  assoc=list()
  for(oli in seq( 1: len(ores$levels)))
  {
    pk=1
    gewinn= ores$values[oli]
    mP("%d:  gewinn %f",oli,gewinn)
    #oli=1 #TEST
    for (pari in seq(1:len(ores$levels[oli][[1]])))
    {
      # pari=1 #TEST
      #browser()
      osl=ores$levels[oli][[1]]
      Pari = osl[pari]
      sym=sprintf("Par%d=%f",pk,Pari)
      print(sym)
      #Sbrowser()
      oldgewinn = assoc[[sym]]
      #browser()
      if (!is.null(oldgewinn))
        assoc[[sym]]=gewinn+assoc[[sym]]
      else
        assoc[[sym]]=gewinn
      pk=pk+1
    }
  }   
  return(assoc)
}

############## einladen der xtsPrice bzw. price - Zeitreihe die hier 
# überall als Standard- Zeitreihen - Input benötigt werden.
#########################################################

load.Data<-function()
{
  Load("gv")
  GDAXI = gv$t0data$Dax
  xtsPrice = Cl(GDAXI)
  #xtsPrice = Cl(to.monthly(xtsPrice))
  dim(xtsPrice)
  
  frame="2006-12-01::2010-01-01"
  
  #frame="2007-11-01::2010-01-01"
  #  frame="2008-09-24::2010-01-01"
  
  #  frame="2003-08-01::"
  frame="1997::"
  
  xtsPrice<<-xtsPrice[frame]
  xtsPrice = mNorm(xtsPrice)
  print(len(xtsPrice[,1]))
  prices<<-xtsPrice
  price<<-prices
  fromTo(xtsPrice)
  plot(xtsPrice)
}

if (F)
  load.Data()
if (F)
{
  assoc=ausWertg()
  assoc
}



################################################################################
# Du brauchst zukünftige Preise, rechts von Today
# dann kannst Du hiermit die Soll-Positionen berechnen
#Welche Sollpos soll ich Today eingehen, wenn ich bereit eine maxDDtoleranz einzugehen
#um wenigsgtens eine minReturn zu erhalten
# Wichtig: Es wird nicht gesagt bis wann man in der Pos bleiben soll, sondern ob es
# es sich am Tag Today lohnt in diese Pos zu gehen.
#Die Einschätzung .. es lohnt sich .. beruht auf zukünftigen Daten ..
#Es ist nicht so, dass wenn man immer die vorgeschlagene Pos einnimmt den max-Return (minus Transaktionskoste) erreicht.  So kann es sinnvoller sein eine eingenommene 
#Pos bis zum Ende ihres Profitabilitätszuwachses zu behalten, statt eine Gegenposition #einzunehmen, die dann erst nach einem DrawDown < maxDDtoleranz anfängt profitabel zu #werden.   (für letzteres ist ein ZigZag-Basierte Analyse sinnvoller)
#gut ist aber:  man ist meist mitten im Drehzenario
# Leseweise:  Ab dem Datum wo einer nicht mehr Short anzeigt, lohnt es sich nicht
# mehr Short zu gehen, (minReturn wird nicht mehr erwirtschaftet, ...)
################################################################################
targetByDD_<-function( Today="2007-01-01", price, maxDDtoleranz = 0.2, minReturn = 0.3,visual=F)
{
  #price=prices[,wp]
  #stell sicher, dass Today nicht ein Wochenende ist - also dass es einen Kurs Today gibt
  #firstPrice = last(price[as.Date(index(price))<=as.Date(Today)])
  #Today=as.Date(index(firstPrice))
  if (!exists("lastPos__")) lastPos__ <<-0
  if (!exists("PeakPos__")) PeakPos__ <<-0
  if (!exists("eqPos__")) eqPos__ <<-0
  
  
  price = price[sprintf("%s::",Today)]
  if(len(price[,1])<2) return(list())
  ret = ROC(price, type='discrete'); ret[1] = 0; 
  equity = cumprod(1 + ret)
  
  Target = equity
  Target = equity[as.Date(index(Target))>=as.Date(Today)]   #futureequity
  dd= compute.drawdown(Target)
  #plot(Target)
  #plot(dd)
  TargetShort = -Target+2
  #plot(TargetShort)
  ddS= compute.drawdown(TargetShort)
  #plot(TargetShort)
  #plot(-ddS)
  #plot(Target)
  korrektur =maxDDtoleranz 
  ddbrkLong = first(which(-dd >= maxDDtoleranz))
  if (is.na(ddbrkLong))
  {targetxLong = len(Target);korrektur =0}
  else
    targetxLong= ddbrkLong
  
  ddbrkShort = first(which(-ddS >= maxDDtoleranz))
  if (is.na(ddbrkShort))
  {targetxShort = len(Target);korrektur=0}
  else
    targetxShort= ddbrkShort
  
  #plotM(Target,DateS(ddS[targetxShort]))
  # browser()
  Target=Target[c(1:targetxLong)] #von Today bis zum maxDD-Break - oder Ende
  TargetShort=TargetShort[c(1:targetxShort)]
  
  res=list(Date=Today, Long=0, Short=0, BestPos=0, ContinuePos = lastPos__, PeakPos=PeakPos__,eqPos=eqPos__)
  
  if (visual)
  {
    plot(price)
    #plot(Target)
    abline(v=as.POSIXct(as.Date(index(price[Today]))), col=col.add.alpha("red" , 80),lwd=3)
    plot(Target)
    plot(TargetShort)
    browser()
  }  
  #s kommt ertrag aus dem Trade-  auch wenn eine riesen verlust droht
  if (max(Target)-1 >= minReturn)
    res$Long=1; 
  if (max(TargetShort)-1 >= minReturn)
    res$Short=1
  
  if (res$Long==1 || res$Short==1)
  {
    #retL = ROC(Target, type='discrete'); retL[1] = 0; 
    #equityL = cumprod(1 + retL)
    #retS = ROC(TargetShort, type='discrete'); retS[1] = 0; 
    #equityS = cumprod(1 + retS)
    #if (last(as.numeric(equityL)) >= last(as.numeric(TargetShort)))
    if (max(Target)> max(TargetShort))
    {
      res$BestPos=1
      if (res$BestPos != lastPos__)
        if (which(Target==min(Target)) == 1)
          res$PeakPos=1
      
    }
    else
    {
      res$BestPos=-1
      if (res$BestPos != lastPos__)
        if (which(TargetShort==min(TargetShort)) == 1)
          res$PeakPos=1
    }
    if (lastPos__==1 && res$Long !=1  || lastPos__==-1 && res$Short !=1 || lastPos__==0 )
    {
      lastPos__ <<- res$BestPos 
      res$ContinuePos = lastPos__
    }
    eqPos= max(max(Target),max(TargetShort))-1-korrektur
    #kann negativ werden, wenn maxDDtoleranz > minReturn (letzterer wird dann tschaftet ersterer aber nicht mehr)
    
    #    if (korrektur > 0)  #noch nicht an den rechten Rand geknallt
    eqPos__<<-eqPos__+eqPos
    
    res$eqPos=eqPos__
  }
  else
    res$PeakPos=ifelse(res$Long==1,1,ifelse(res$Short==1,-1,0))
  
  #plot(TargetShort)
  #plot(Target)
  
  return(res)
}

targetByDD<-cmpfun(targetByDD_) #compilier das Teil

if (F)
{
  price=xtsPrice["2012::"]
  
  
  Today="2012-01-17"
  
  Today="2012-01-02"
  Today="2012-03-02"  
  Today="2012-04-02"
  Today ="2012-05-01"
  price=xtsPrice["2012::"]
  plotM(price,Today)
  
  plot(price)
  abline(v=as.POSIXct(as.Date(index(price[Today]))),col=col.add.alpha("red" , 80),lwd=3)
  
  #aufruf nur Today
  PeakPos__=0;lastPos__=0;eqPos__=0
  targetByDD( Today=Today, price=price, maxDDtoleranz = 0.51, minReturn = 0.036)
  
  
  #für alle Tage:
  PeakPos__=0;lastPos__=0;eqPos__=0
  oaw <<-lapply(as.Date(index(price)),function(x)  targetByDD( Today=x, price=price, maxDDtoleranz = 0.16, minReturn = 0.05))#maxDDtoleranz = 0.1, minReturn = 0.05))
  
  PeakPos__=0;lastPos__=0;eqPos__=0  
  oaw <<-lapply(as.Date(index(price)),function(x)  targetByDD( Today=x, price=price, maxDDtoleranz = 0.011, minReturn = 0.06))#maxDDtoleranz = 0.1, minReturn = 0.05))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~ Auswertung
  #die Listen von liste -> data.table
  OAW = rbindlist(oaw) #data.table aus liste von listen ...
  #den data.table ->xts
  targetXts=as.xts(OAW, order.by = as.Date(OAW$Date))[,-1]; 
  #head(targetXts)
  #targetXts[] = as.numeric(coredata(targetXts))
  #coredata(targetXts) = as.numeric(targetXts[])
  block=merge(price,targetXts)
  block = block[-dim(block)[1]]  #letzte Zeile entfernen weil nicht gerechnet
  block[is.na(block)]=0
  
  plota(block[,1],main="AnyPos")
  #es gibt positionen da ist sowohle long als auch short denkbar (orange)
  col =iif(block[,"Short"]==1 & block[,"Long"]==1,"orange", iif(block[,"Long"]==1,"red", 
                                                                iif(block[,"Short"]==1,"blue","gray")) )
  plota.lines(block[,1], type='l', col=col,lwd=2)
  pp=block[,"PeakPos"];peaks=pp[pp !=0]
  abline(v=as.POSIXct(as.Date(index(peaks))),col=col.add.alpha("red" , 80),lwd=3)
  
  
  #wo kann man überall long gehen
  plota(block[,1],main="Long")
  col =iif(block[,"Long"]==1,"red","gray")
  plota.lines(block[,1], type='l', col=col,lwd=2)               
  
  plota(block[,1],main="Short")
  col =iif(block[,"Short"]==1,"blue","gray")
  plota.lines(block[,1], type='l', col=col,lwd=2)               
  
  plota(block[,1],main="BestPos")
  #wenn zwei meinungen denkbar wären.. welche wär die Ertragreichste ?
  col =iif(block[,"BestPos"]==1,"red",  iif(block[,"BestPos"]== -1,"blue","gray"))
  plota.lines(block[,1], type='l', col=col,lwd=2)               
  
  #Positionen laufen lassen
  plota(block[,1],main="ContinuePos")
  col =iif(block[,"ContinuePos"]==1,"red",  iif(block[,"ContinuePos"]== -1,"blue","gray"))
  plota.lines(block[,1], type='l', col=col,lwd=2)               
  
  plota(block[,7],main="SumSinglePosEq")
  col =iif(block[,"ContinuePos"]==1,"red",  iif(block[,"ContinuePos"]== -1,"blue","gray"))
  plota.lines(block[,7], type='l', col=col,lwd=2)               
  
}


###suche nun Werte für maxDDtoleranz , minReturn die die
#quality maximieren

sys.Target<-function(arg,visual=F)
{
  
  maxDDtoleranz=arg[1]
  minReturn=arg[2]
  
  runNr__<<-runNr__+1
  print(runNr__)
  Today ="2012-05-01"
  price=xtsPrice["2012::"]
  
  PeakPos__<<-0;lastPos__<<-0;eqPos__<<-0
  oaw <<-lapply(as.Date(index(price)),function(x)  targetByDD( Today=x, price=price, maxDDtoleranz = maxDDtoleranz, minReturn = minReturn))
  lasti = len(oaw)-1
  quality = oaw[[lasti]]$eqPos  #suche nun Werte für maxDDtoleranz , minReturn die die
  
  if (!visual)
    return(quality)
  #quality maximieren
  #coredata(targetXts) = as.numeric(targetXts[])
  #die Listen von liste -> data.table
  OAW = rbindlist(oaw) #data.table aus liste von listen ...
  #den data.table ->xts
  targetXts=as.xts(OAW, order.by = as.Date(OAW$Date))[,-1]; 
  
  block=merge(price,targetXts)
  block = block[-dim(block)[1]]  #letzte Zeile entfernen weil nicht gerechnet
  block[is.na(block)]=0
  
  plota(block[,7],main="SumSinglePosEq")
  col =iif(block[,"ContinuePos"]==1,"red",  iif(block[,"ContinuePos"]== -1,"blue","gray"))
  plota.lines(block[,7], type='l', col=col,lwd=2)               
  
  
}

if (F)
{
  
  lev=list(seq(0.01,0.9,0.05),  seq(0.06,0.59,0.05))
  lev=list( seq(0.01,0.9,0.05), 0.05)#maxDDtoleranz = 0.1, minReturn = 0.05))
  
  
  runs = len(combine2(seq(0.01,0.19,0.05),  seq(0.01,0.19,0.05),sep="|"))
  mP("Expect time for %d runs ",runs)
  runNr__=0  
  ores <<- gridSearch(sys.Target, levels=lev)  ##### der zeitaufwändige optimierungslauf
  #ores1=ores
  ovalues<<-ores$values[is.finite(ores$values)]
  plot(ovalues)
  
  
  besti=which.max(ovalues)
  best=ores$values[besti]
  mP("BestVal %f",best)
  print(ores$levels[besti])
  
  sys.Target(unlist(ores$levels[besti]),T)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~ Auswertung
  #die Listen von liste -> data.table
  OAW = rbindlist(oaw) #data.table aus liste von listen ...
  #den data.table ->xts
  targetXts=as.xts(OAW, order.by = as.Date(OAW$Date))[,-1]; 
  #head(targetXts)
  #targetXts[] = as.numeric(coredata(targetXts))
  #coredata(targetXts) = as.numeric(targetXts[])
  block=merge(price,targetXts)
  block = block[-dim(block)[1]]  #letzte Zeile entfernen weil nicht gerechnet
  block[is.na(block)]=0
  
  plota(block[,7],main="SumSinglePosEq")
  col =iif(block[,"ContinuePos"]==1,"red",  iif(block[,"ContinuePos"]== -1,"blue","gray"))
  plota.lines(block[,7], type='l', col=col,lwd=2)               
  
}

############### so kann man das Target gut aufrufen
#für ein singel-Spalten xts
########################
compute.Target<-function(price,  maxDDtoleranz=0.16, minReturn=0.05)
{
  sym=toString(colnames(price))
  mP("compute.Target of %s ",sym)
  #if (sym=="USDEUR")
  #    browser()
  
  PeakPos__<<-0;lastPos__<<-0;eqPos__<<-0
  oaw <<-lapply(as.Date(index(price)),function(x)  targetByDD( Today=x, price=price, maxDDtoleranz = maxDDtoleranz, minReturn = minReturn))
  lasti = len(oaw)-1
  quality = oaw[[lasti]]$eqPos  #suche nun Werte für maxDDtoleranz , minReturn die die
  
  #quality maximieren
  #coredata(targetXts) = as.numeric(targetXts[])
  #die Listen von liste -> data.table
  OAW = rbindlist(oaw) #data.table aus liste von listen ...
  #den data.table ->xts
  targetXts=as.xts(OAW, order.by = as.Date(OAW$Date))[,-1]; 
  
  block=merge(price,targetXts)
  block[dim(block)[1]] = last(price) 
  #block = block[-dim(block)[1]]  #letzte Zeile entfernen weil nicht gerechnet
  block[is.na(block)]=0
  return (block)  
}
############### so kann man das Target gut aufrufen
#für ein multi-Spalten xts,  z.b  data$prices
########################
compute.Targets<-function(prices, maxDDtoleranz=0.16, minReturn=0.05)
{  
  global_T  <<-NULL
  #compute.Target() gibt immer eine ganze Liste von Vektoren raus             
  T__<<-lapply(colnames(prices),FUN=function(x)
  {PeakPos__<<-0;lastPos__<<-0;eqPos__<<-0;
   priceCol=prices[,x]; 
   cp = try(compute.Target(priceCol,maxDDtoleranz,minReturn))
   if (!is.null(cp) && len(cp$ContinuePos)>0)
     if (is.null(global_T ))
       global_T =cp$ContinuePos
   else
     global_T <<- merge(global_T , cp$ContinuePos)
  })
  mP("... now build global_Targets ...")
  names(T__) = colnames(prices)       
  #nimm von dieser Vektorenliste nur den Slot:  ContinuePos            
  TcontinuePos=lapply(colnames(prices),FUN=function(x){T__[[x]]$ContinuePos})
  names(TcontinuePos) = colnames(prices)             
  #bau aus der Liste von Zeitreihen eine einzige Zeitreihe
  OAW=data.frame(TcontinuePos)
  targetXts=as.xts(OAW, order.by = as.Date(rownames(OAW))); 
  colnames(targetXts) = colnames(prices)             
  return(targetXts)
}

if (F)
{  
  compute.Target(price,0.16,0.05)
  global_Targets <<-compute.Targets(price,0.06,0.005)
  plot(price)
  lines(price,col=(global_Targets+3),type="h")
  prices=na.omit(data$prices)
  
  prices = mNorm(data$prices[,c("USDCHF","USDGBP")])
  purePlot(prices)
  global_Targets <<-compute.Targets(prices,0.16,0.05)
  
  plot(prices[,1])
  lines(prices[,1],col=(global_Targets[,1]+3),type="h")
  
}
###################################################################################
#Training mit der neuen Target Funktion als Soll-Output
#mode =spl("train,test")
#Bezogen auf Today:  Trainiere mit Daten der Vergangenheit, und mach einen Forecast für #die Zukunft.  Falls es ein target gibt, bewerte den Forecast am Target.
###################################################################################
neuralNet<-function(Today=Sys.Date(),  price, priceArgs,  target, data=data)
{
  # hier können noch viele andere Zeitreihen für Multivar-Prognose kommen
  #input ist ein eindim-xts in dem an den verschiedenen Einträgen aber ganz unterschiedliche Infos stehen.
  
  
  #  HotLagDiffs= sapply(HotLags(price), FUN=function(x){as.numeric(last(diff(price,lag=x)))})
  
  #Baue aus einer Bank von InputIndikatoren einen virtuellen xts  Input
  
  # inputC = c( as.numeric(last(RSI(price, 2))), as.numeric(last(RSI(lag(price), 2))),
  #TSIsymbol(data),
  #              c(HotLagDiffs)
  #              )
  
  #ti1 = as.Date(Today)-len(inputC)  
  #inputXts=as.xts(inputC,as.Date(ti1)+1:len(inputC))
  
  library(caret)
  creditlog  <- data.frame(score=credit$score,
                           log.savings=log(credit$savings+1),
                           log.income=log(credit$income+1),
                           log.address=log(credit$time.address+1),
                           log.employed=log(credit$time.employed+1),
                           fte=credit$fte, single=credit$single)
  fit  <- avNNet(score ~ log.savings + log.income + log.address +
                   log.employed, data=creditlog, repeats=25, size=3, decay=0.1,
                 linout=TRUE)
  
}

###################################################################################
# rollierendes, Long/Flat/Short-Forecasting mit dynamisch nachtrainiertem Indikator.
# aus Vergangenheitsdaten wird mit compute.Target- ein Soll-Output vorbereitet der fürs
# Training des Indikators dient

#geht nur uni-variat
###################################################################################
prepareInputSeries<-function(symbol, data)
{
  #die price-Daten sollen univariat und normiert sein
  
  #Baue aus einer Bank von InputIndikatoren einen virtuellen xts  Input
  tail(res)
  
  #hlc=merge(Hi(data[[symbol]]),Lo(data[[symbol]]),Cl(data[[symbol]]))
  hlc=HLC(data[[symbol]])
  price=Cl(hlc)
  price = mNorm2(price)
  res=price
  #5 Momentum-Werte
  for (n in head(HotLags(price),5))
  {
    momentum = price / mlag(prices, n)  
    
    colnames(momentum)=sprintf("diff_%d",n)
    res = merge(res,momentum)
  }
  
  res = res[,-1]
  head(res)
  
  #3 Auto-Winkel
  #  slidPrice=slid(price,maxSlid=100,minSlid=25,dd=10)
  
  #slidPrice
  mSlope90 <<- rollRegressionXTS(price,win=90)
  mSlope20 <<- rollRegressionXTS(price,win=20)
  
  res=merge(res,mSlope90,mSlope20)
  
  #5 RSI
  for (n in c(2,5,10,20,30))
  {
    rsi =RSI(price,n) 
    res = merge(res,rsi)
  }
  
  #TTR-Zeugs ~~~~~~~~~~~~~~~
  
  #1 SMA-Differenz
  sma.short = bt.apply.matrix(price, SMA, 50)
  sma.long = bt.apply.matrix(price, SMA, 200)
  smaDif = sma.long-sma.short
  res = merge(res, smaDif)
  
  #1 Vola s
  ret.log =  ROC(price,type='continuous')
  hist.vol = runSD(ret.log, n = 21)
  vol.rank = SMA(percent.rank(hist.vol, 21),5)
  res = merge(res, vol.rank)
  #3 Technik
  adx = ADX(na.omit(hlc),n=14)$ADX
  tsi = TSI(hlc)
  k=20
  scdfast=score.diff(ZLEMA(prices,n=k),k=k)
  
  res=merge(res,adx,tsi,scdfast) 
  
  
  head(res)
  
  plot(runmax(Hi(data[[symbol]]),k=50))
  
  #position.score = bt.apply(data, function(x)  TSI(HLC(x)) )    
  #http://lifeanalytics.blogspot.de/2011/01/forex-trading-with-r-part-1.html
  #http://lifeanalytics.blogspot.de/2011/02/forex-trading-with-r-part-2.html
  #Forecasts (HolftWinters,  andere Zeitreihen ...)
  myATR <- function(x) ATR(HLC(x))[,'atr']
  mySMI <- function(x) SMI(HLC(x))[,'SMI']
  myADX <- function(x) ADX(HLC(x))[,'ADX']
  myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator
  myBB <- function(x) BBands(HLC(x))[,'pctB']
  myChaikinVol<-function(x)Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
  myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
  myMACD <- function(x) MACD(Cl(x))[,2]
  mySAR <- function(x) SAR(x[,c('High','Close')]) [,1]
  myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]
  myEMA10 <- function(x) EMA(Cl(x),n=10)[,1]
  myEMA20 <- function(x) EMA(Cl(x),n=20)[,1]
  myEMA30 <- function(x) EMA(Cl(x),n=30)[,1]
  myEMA50 <- function(x) EMA(Cl(x),n=50)[,1]
  myEMA60 <- function(x) EMA(Cl(x),n=60)[,1]
  
  
}



rollingTrainRun<-function()
{
  sym="Dax"
  Load("gv")
  data=isSymbol(gv$t0data)
  GDAXI = gv$t0data$Dax
  xtsPrice = Cl(GDAXI)
  prices=xtsPrice["2011::2012"]
  
  
  
  prices = mNorm2(prices)
  month.ends = endpoints(prices, 'months')
  month.ends = month.ends[month.ends > 0]-1  	#schon Donnerstags
  endpoints = DateS(prices[month.ends])
  period.annual.factor=12 
  maxDDtoleranz = 0.16
  minReturn  =0.05
  #Der Soll-Output fürs beaufsichtigte Lernen
  target = compute.Target(prices, maxDDtoleranz=maxDDtoleranz, minReturn  =minReturn)[,"ContinuePos"]#BestPos,AnyPos   #maxDDtoleranz = 0.1, minReturn = 0.05))
  
  #die Parameter für die Multivar-Input-Berechnung:
  priceArgs = list()
  priceArgs$indications = prepareInputSeries(sym,data)
  
  #1 IFO
  
  ifo=Cl(ifo[endpoints(ifo, 'months')])
  dIfo = diff(ifo)
  priceArgs$dIfo=dIfo
  #der rollierende Train/Test-Lauf auf endpoints - die Fensternlänge der Train-Daten
  #berechnet sich forecast selber
  oaw <<-lapply(endpoints,function(x)  neuralNet( Today=x,price=prices, priceArgs=priceArgs,target = target,data=data)) 
  
  #Auswertung und visualisierung der Ergebnisse
  OAW = rbindlist(oaw) #data.table aus liste von listen ...
  targetXts=as.xts(OAW, order.by = as.Date(OAW$Date))[,-1]; 
  block=merge(price,targetXts)
  
  plota(block[,7],main="SumSinglePosEq")
  col =iif(block[,"ContinuePos"]==1,"red",  iif(block[,"ContinuePos"]== -1,"blue","gray"))
  plota.lines(block[,7], type='l', col=col,lwd=2)               
}

###################################################################################
#Training mit der neuen Target Funktion als Soll-Output
#mode =spl("train,test")
#Bezogen auf Today:  Trainiere mit Daten der Vergangenheit, und mach einen Forecast für #die Zukunft.  Falls es ein target gibt, bewerte den Forecast am Target.
###################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## hier werden ganz normale Handelstage simuliert - evtl. - nachtrainieren, evtl. ausgestoppt werden,.. evtl. kaufen, verkaufen...

optimizeAndRun_ZlemaDif<-function(Today=Sys.Date(),  symbol,trainDays, priceArgs,  target, data=data,wlen=60)
{

  firstRun=is.null(global_ParTable )
  #  Today =as.Date("2005-01-01")
  isTrainday=len(trainDays[Today])>0
  mP("## %s ## %s ########### ", Today,"indi.ZlemaDif")
  
  #die Preise der Vergangenheit
  #price =price[as.Date(index(price))<=as.Date(Today)]
  
  browser()
  price=na.omit(Cl(data[[symbol]]))
  
  i=xts2Index(Today,price)  #MMA
  fromDay = index(price[i-wlen])  #mindestens wlen viele Daten werden für den indi benötigt
  
  #auf welchen Zeitfenster vor Today soll trainiert werden (wenigstens wlen lang)
  price = mNorm(price[sprintf("%s::%s",fromDay,Today)])
  
  global_arg<<-list(clos=price)
  global_StartDate <<- Today
  global_objectId <<-paste("TREND",symbol,"indi.ZelamDif")
  
  #mlist()
  
  #MM_TRAIN_NOW
    if (isTrainday || firstRun)
  {
    print("############## Training #################")
    ####################### MMTRAiNI    
    if (firstRun)
      indi.ZlemaDif(global_arg,visual=F, main= "firstRun")
    else
    {      
      #MMNOW
      indi.ZlemaDif(global_arg,visual=T,main= "preTrain")  #vorher  (reinschreiben der Parameter)
      #... merk Dir Guv und Signale seit letztem Trainings-Lauf...
      #... verbessere den best-par-Auswertungs-selektor -- wenn nahe dem aktuellen ParVektor
      #.. ein recht guter Neuer liegt... nimm den.
      #.. woran liegts, dass manchmal die trainingsergebnisse (postTrain) sooo schlecht sind?
      #.. zlema sollte noch nene dritten parameter k2 haben... ??
      
      P("preTrain")
      browser()
    }
    # global_ParTable$par
    #mlist(mode="k")
    TrainIndicator(global_StartDate_ = Today, opti="GRID",indiName = "indi.ZlemaDif")  
    res = indi.ZlemaDif(global_arg,visual=T,main= "postTrain")
    mP("postTrain")
    browser()    #MM2
    
    #res = indi.ZlemaDif(global_arg,list(minSig=1,k=20),visual=T)
    
    
  }
  # else
  #     res=indi.ZlemaDif(global_arg,visual=F)  #vorher  (reinschreiben der Parameter)
  
  mP("---> %f",res)
}
#
#--TrainIndicator() ans Laufen bringen .. 

#--- vector-rechung erhalten .. nicht über jeden Tag einzeln iterieren ?? -- 
#  oder doch .. dann die trainings-Tage als Liste übergeben.
#wenn Trainings-Tag ist:  nachtrainieren und mit diesen neuen Parametern rechen.


######################################## super
rollingTrainRun_ZelamDif<-function()
{
  Load("Mmain")
  prices =data.info(data,ignore=c("VDAX","Nasdaq100")) 
  
  sym="Dax"
  frame="2005::"
  Dax=prices[frame,"Dax"]
  Mdax =prices[frame,"MDAX"]
  prices = mNorm(merge(Dax,Mdax))
  prices=prices[,"Dax"]
tail(prices)
  mchart(prices)
  #purePlot(prices)
  scd=0
  k=20
  WLEN=2*200 #k+k im indi  #offset: so viel platz braucht der inikator bei maximal erlaubter Trainings-Fenster breite ... erst danach darf das erste trainDay kommenn
  
  #fromTo(prices)
  prices= na.omit(prices)
  prices = prices[-c(1:(2*WLEN))]    #dann sollte noch mal so viel Platz für echte daten da sein ... vorsicht: wenn dies zu klein ist, obptimiert er immer nur die sollpos- des letzen monats ... baut aber kein modell ... wird also schlecht generalisieren...
  
  
  month.ends = endpoints(prices, 'months')
  month.ends = month.ends[month.ends > 0]-1    #schon Donnerstags
  endpoints = DateS(prices[month.ends])
  trainDays =prices[month.ends]
  mP("---- start rolling at %s",index(first(trainDays)))
  period.annual.factor=12 
  
  #Der Soll-Output fürs beaufsichtigte Lernen
  if (F)
  { 
    maxDDtoleranz = 0.16
    minReturn  =0.05
    
    target = compute.Target(prices, maxDDtoleranz=maxDDtoleranz, minReturn  =minReturn)[,"ContinuePos"]#BestPos,AnyPos   #maxDDtoleranz = 0.1, minReturn = 0.05))
    plotSigPrice(signal=target,prices=prices,indi=list(target))
  }
  priceArgs = list()
  
  #------ Vorbereitende globale Varibalen-Setzung damit das ini.Optimizing-Framework funktioniert:
  global_ParTable <<- NULL
  global_StartDate <<- as.Date(index(first(prices)))
  global_objectId <<-paste("TREND",sym,"indi.ZelamDif")
  global_arg <<- list(clos=prices)
  globalTrainLevel <<-10
  runN<<-0
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #der rollierende Train/Test-Lauf auf endpoints - die Fensternlänge der Train-Daten
  #berechnet sich forecast selber
  allDates = as.Date(index(na.omit(prices)))
  
  oaw <<-lapply(allDates, function(x)  optimizeAndRun_ZlemaDif( Today=x, symbol=sym,    trainDays=trainDays, priceArgs=priceArgs,target = target, data=data,wlen = 2*WLEN))
  
}

if (F)
  rollingTrainRun_ZelamDif()

################ Lustige Indikatoren aus 
#siehe:  timelyPortfolioSharpeSys.r

CUDIndi <- function(price,n,period="days") {
  
  if (period != "days")
    price <- price[endpoints(price,period)]
  #CUD takes the n-period sum of 1 (up days) and -1 (down days)
  temp <- runSum(ifelse(ROC(price,1,type="discrete") > 0,1,-1),n)
  colnames(temp) <- "CUD"
  temp
}

sharpeIndi <-function(price,nprod=12,nmax=3,period="days") 
{  
  if (period != "days")
    price <- price[endpoints(price,period)]
  
  roc= ROC(price,type="discrete",n=1)  
  sharpe <- (apply.rolling(roc+1,FUN=prod,width=nprod)-1)/(runMax(abs(roc),n=nmax))
  
  #ifelse(sharpeIndi > 0,1,0)
  #ifelse(sharpeIndi > lag(sharpeIndi,k=6),1,0)
}

mFaberIndi <-function(price,n=10,period="days") 
{  
  if (period != "days")
    price <- price[endpoints(price,period)]
  
  cumul=mNorm(price)  
  #do 10 month Mebane Faber style system
  ma <- runMean(cumul,n=10)
  
  #ifelse(cumul > ma,1,0)    
}
omegaIndi <-function(price,n=10,period="days") 
{  
  if (period != "days")
    price <- price[endpoints(price,period)]
  
  cumul=mNorm(price)  
  #do 10 month Mebane Faber style system
  
  omega <-  apply.rolling(cumul,width=n,by=1,FUN=Omega)
}             

# if today's low is higher than yesterday's close 1, else 0
lowCloseIndi <-function(price,n=1,period="days") 
{  
  if (period != "days")
    price <- price[endpoints(price,period)]
  
  price=mNorm(price)
  res =ifelse(Lo(HLC(price)) > Cl(lag(price),n),1,0)
}      

if (F)   #heisse trading-beispiele aus dem netz
{
  #################################################################
  #http://xmphforex.wordpress.com/code-examples/
  require('xts')
  require('quantmod')
  require('blotter')
  require('PerformanceAnalytics')
  require('FinancialInstrument')
  Sys.setenv(TZ="GMT")
  
  #If during the last 2 days VIX return was >6%, then high probability of S&P500 index will be positive on 3rd day
  
  #data part
  getSymbols(c('SPY','^VIX'),from='1995-01-01',index.class=c("POSIXt","POSIXct"))
  dividends<-getDividends('SPY',from='1995-01-01',index.class=c("POSIXt","POSIXct"))
  
  temp<-cbind(dividends,SPY)
  temp[,1][is.na(temp[,1])]<-0
  
  SPY<-cbind(temp[,2],temp[,3],temp[,4],temp[,1]+temp[,5])
  colnames(SPY)<-c('Open','High','Low','Close')
  spy.delta<-Delt(Cl(SPY))
  
  vix.delta<-Delt(Cl(VIX))
  
  signal<-ifelse(vix.delta>0.06 & lag(vix.delta>0.06, 1),1,0)
  chart.CumReturns(lag(signal,1)*spy.delta,main='when VIX >6% during two days')
  
  #####################################################################
  #3 week days in a row
  #if SPY shows lower open, high and close 3 days in a row, then buy on the close of third day and sell it 1 days later
  require('xts')
  require('quantmod')
  getSymbols('SPY',from='1995-01-01',index.class=c("POSIXt","POSIXct"))
  dividends=getDividends('SPY',from='1995-01-01',index.class=c("POSIXt","POSIXct"))
  
  temp=cbind(dividends,SPY)
  temp[,1][is.na(temp[,1])]=0
  SPY=cbind(temp[,2],temp[,3],temp[,4],temp[,1]+temp[,5])
  colnames(SPY)=c("Open","High","Low","Close")
  
  #one day before
  lag1=lag((SPY),1)
  
  #two days defore
  lag2=lag((SPY),2)
  
  signal=ifelse( (Cl(lag2)>Cl(lag1) & Cl(lag1)>Cl(SPY))&
                   (Hi(lag2)>Hi(lag1) & Hi(lag1)>Hi(SPY)) &
                   (Op(lag2)>Op(lag1) & Op(lag1)>Op(SPY)),
                 1,0
  )
  #one day later
  lag3=lag(Cl(SPY),-1)
  
  profit=(lag3/Cl(SPY)-1)*signal
  profit[is.na(profit)]=0
  #png(file='first.png',width=500)
  plot(cumprod(profit+1),main='Profit 1995-2010')
  #dev.off()
  ###################################################
  library(zoo)
  library(tseries)
  
  #Cointegration:
  #http://quanttrader.info/public/testForCoint.html
  gld <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=GLD&ignore=.csv", stringsAsFactors=F)
  gdx <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=GDX&ignore=.csv", stringsAsFactors=F)
  
  
  
  #gld <- read.csv("GLD.csv", stringsAsFactors=F)
  #gdx <- read.csv("GDX.csv", stringsAsFactors=F)
  
  gld <- zoo(gld[,7], as.Date(gld[,1]))
  gdx <- zoo(gdx[,7], as.Date(gdx[,1]))
  
  
  t.zoo <- merge(gld, gdx, all=FALSE)
  t <- as.data.frame(t.zoo)
  
  cat("Date range is", format(start(t.zoo)), "to", format(end(t.zoo)), "\n")
  
  m <- lm(gld ~ gdx + 0, data=t)
  beta <- coef(m)[1]
  
  cat("Assumed hedge ratio is", beta, "\n")
  
  sprd <- t$gld - beta*t$gdx
  ht <- adf.test(sprd, alternative="stationary", k=0)
  
  cat("ADF p-value is", ht$p.value, "\n")
  
  if (ht$p.value < 0.05) {
    cat("The spread is likely mean-reverting\n")
  } else {
    cat("The spread is not mean-reverting.\n")
  }
  
}
#############################################







##################################################

if (F)
  plotOBOS("^GDAXI")

mP("########### load Now.R")
