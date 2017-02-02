####################################################################################
#beschreibung technischer indikatoren
#Quelle: http://www.broker-test.de/finanzwissen/technische-analyse/volatilitaet/  

# siehe auch:  #TESTFILTER
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
########################################################################

if (F)
{
  p=data$prices[,data$BENCH];P=mNorm(p)
  P=na.omit(SMA(na.omit(ZigZag(p[1000:1500])),5))   #zigzag-reihe - gut für causalitäts test
  
}

if (F)
{
  sym="X5YEARNOTE"
  date="2008-09-19"
  w=as.Date(index(dax["2008-09-19"]));visual=T
  
  sfStop()
  prepare_Parallel()
  sfSource("MLib/labor_signal.r")
  
  sfExport("make.tech.cloud")
  ########################  im Batch die tech-Cloud erstellen
  make.tech.cloud(visual=T,symbols=c("REX_RI") )
  t.cloud <<-  make.tech.cloud(visual1=F, visual=F,symbols=spl("SXX50,REX_RI"),step="years")#,REX_RI  ########################  im 
  t.cloud <-  make.tech.cloud(visual1=F, visual=F,symbols="",step="years")#,REX_RI  ########################  im 
  t.cloud <-  make.tech.cloud(visual1=F, visual=F,symbols="",step="days")#,REX_RI  ########################  im 
  t.cloud <-  make.tech.cloud(visual1=F, visual=F,symbols=spl("REX_RI,SXX50,DAX30,USD_EURO"),step="years",do.par=F)#,REX_RI  ##
  
  len(t.cloud)
  t.cloud[[1]]  
  
  x=read.csv(sep=";",dec=".",header=T,sprintf("TechCloud/DAX30_tcloud.csv"),stringsAsFactors=F)
  colnames(data$prices)
  
}


if (F)   #####  die arbeitsbank ###############################################
{
  ls(data)
  dax=Cl(data$DAX)
  
  colnames(data$prices)
  sym="DAX"
  dax = na.omit(dax)
  dax =data$prices[,sym]
  dax=m.ifna.prev(dax)
  plot(dax)
  len(dax[is.na(dax)])
  dax.frame = sprintf("%s::",as.Date(DateS(dax[which(dax==min(dax))]))-200)
  dax.f = dax[dax.frame]
  #dax.f = dax.f["2005::"]
  #HL=HighLows(dax.f,maxdd=38,visual=T)
  #names(HL)
  
  #hol dir die wendepunkte um ein paar interessante abschnitte im chart zu finden
  fs=fast.smoothing(dax,glaettung = 5,visual=T)
  wendepunkte = as.Date(fs$hl)-20   #ein bischen vor dem wendepunkt
  wendepunkte = wendepunkte[-1]
  
  w<<-as.Date(index(dax[500]))
  w<<-as.Date(index(data$prices[500,sym]));visual=T
  visual=T
  
  posTable <-NULL   #lösche hiermit die Ergebnistabelle
  #steuer einen interessanten Wendepunkt an
  for(i in 1:len(wendepunkte)) 
  {
    visual=F
    i=2
    w<<-as.Date(wendepunkte[i])-10
    if (i==1)
      w0 = first(dax)
    elseS
    w0= as.Date(wendepunkte[i-1])+10
    
    dax.w = tail(dax[sprintf("::%s",w)],1500)
    plot(dax.w)
    amark(w0)
    amark(w)
    #laufe ab dem Wendepunkt weiter ####################################>>>>
    
    while (T)
    {
      dax.w = tail(dax[sprintf("%s::%s",w0,w)],1500) #zwischen 2 wendepunkten, aber nicht mehr als 1500 Tage
      plot(dax.w)
      
      p=dax.w
      posTable=estimate.pos(posTable, dax.w,visual=visual)  #der tech-cloud - aufruf
      if (DateS(last(dax))==DateS(last(dax.w)))
        break
      w<<-w+1  #tage vorwärtsgehen   <<<###################################
    }
  } 
  #......... nutze die posTable als technische Umgebung für das signal.system:
  # .. getPos
  
  colnames(posTable)
  View(posTable)
  ignoreCols=-c(which(colnames(posTable) %in% spl("date")))
  Pos=xts(posTable[,ignoreCols], order.by=as.Date(posTable[,"date"]))
  
  Signal = xts(posTable[,"itpSum"], order.by=as.Date(posTable[,"date"]))
  signal = sign(Signal+2)   
  
  signal <- Signal;signal[]=0
  i=100
  ########## das heuristische technische System anwenden #####
  #iteriere über alle tage  ...................................
  last.res<<-1; signal[]=sapply(1:len(Signal),getSig)   #getSig
  #............................................................
  
  B=na.omit(merge(dax,signal,Pos))  #datum-align
  B=B[-c(1:100)] #offset
  plotSigPrice(signal=B[,2],prices=B[,1],indi=list(m=merge(B[,"m.orange"],0)))  
  #############################################################################
  
  View(  data.table(posTable)[date=="2008-04-16"])
  plotSigPrice(signal=B[,2],prices=B[,1],indi=list(m=merge(Pos[,"m.red"],Pos[,"m.magenta"],Pos[,"m.orange"],0)))  
  
  write.table(posTable,"posTable_DAX.csv",sep=";",row.names=F)
  plot(p)
  
  read.xts(sprintf("TechCloud/DAX30_tcloud.csv"))
  
  
  t.cloud <<-  make.tech.cloud(visual1=F, visual=F,symbols=spl("SXX50,REX_RI"),step="years")#,REX_RI  ########################  im Batch die tech-Cloud erstellen
  len(t.cloud)
  t.cloud[[2]]  
}
########################################################################
#baue die tech-cloud
########################################################################
make.tech.cloud_ <-function (visual1=T,visual=F,prof=F,symbols=c(1:6),step="years",do.par=T)
{
  if (do.par)
  {
    mP("run parallel")
    my.apply=match.fun("sfLapply")
    
  }
  else
    my.apply=match.fun("lapply")
  
  # profiling of running time
  count=0
  time.i=1
  #  sfExport("estimate.pos")
  
  if (len(symbols)==0 || symbols=="")
    symbols.i=c(1:len(colnames(data$prices)))
  else
    if (is.numeric(symbols[1]))  #symbols dürfen indexnummern sein oder strings
      symbols.i=symbols
  else
    symbols.i= which(colnames(data$prices) %in%  symbols )
  
  #for (sym in colnames(data$prices))
  allPosTables= my.apply(symbols.i, function(sym.i)  #...sfLapply..parallelisiert das, nach..prepare_Parallel()...................
  {
    sym=colnames(data$prices)[sym.i]
    mP("make.tech.cloud for %s",sym)  
    step.s = endpoints(data$prices[,sym],step)
    step.s =step.s[step.s>=220]
    # Monats kurse: data$prices[step.s,sym]
    
    itp.PIPES<<-list() #pipe-storage-reset
    # sym="INTERNATIONALEQUITY"
    fname=sprintf("TechCloud/%s_tcloud.csv",sym)
    if (!file.exists(fname))
    {
      posTable <-NULL   #lösche hiermit die Ergebnistabelle
      #steuer einen interessanten Wendepunkt an
      dax = data$prices[,sym]
      # visual=T
      #  visual=F
      pre.wi=0
      # browser(mP("##1"))  
      fs=fast.smoothing(dax,glaettung = 5,visual=visual1)
      #layout(1)
      if (len(fs$hl)==0)
        wendepunkte = as.Date(index(last(dax)))-10
      else
        wendepunkte = as.Date(fs$hl)-10   #ein bischen vor dem wendepunkt
      w0 = get.Index(dax, as.Date(index(first(dax))))
      w<-w0 +200
      
      #plot(dax,main=sym)
      if (visual1)
      {
        amark(DateS(dax[w0]),col="green")
        amark(DateS(dax[w]))
        lines(dax,col="red")
      }
      
      #laufe ab dem Wendepunkt weiter
      while (T)
      {
        count = count+1
        
        dax.w = dax[w0:w]
        #if (len(dax.w) <200)
        #  dax.w=dax[(w-200):200]
        dax.w = tail(dax.w,500)
        p=dax.w
        if (visual1) lines(p,col="blue",lwd=2)
        
        mP("%d ##### %s sym  ### %s .. %s # len(p) %d ##",count,sym,DateS(dax[w0]),DateS(dax[w]),len(p))
        #p.200 =dax[(w-201):w]
        
        mP("shape %d",shape(p)) 
        #browser()
        if(prof && count >= 10)
          Rprof("o:\\R\\Nuggets\\Eckhard\\estimate.pos.out")
        
        posTable=estimate.pos(posTable=posTable,p,visual=visual)  #der eigentliche cloud bauer#######################################################################################
        #View(posTable)  
        if(prof && count >= 10)
        {        
          Rprof()
          print(summaryRprof("o:\\R\\Nuggets\\Eckhard\\estimate.pos.out"))
          browser(mP("prof#########################"))
          break
        }
        else    
          if (DateS(last(dax))==DateS(last(dax.w)))
            break
        if (prof && count > 10)
          break
        
        #w<-w+1000   #den tag weiterschalten
        time.i=time.i+1
        
        if (w >= shape(dax) || time.i >= shape(step.s))
          break;
        
        w <-step.s[time.i]
        
        if (F&& DateS(dax[w])>=as.Date("2009-01-01")) #TEST
          sag("2009",warte=T)
        
        if (visual1) amark(DateS(dax[w]))
        
        #..............................................
        
        if (pre.wi <len(wendepunkte))
          if (DateS(dax[w]) > wendepunkte[pre.wi+1] +60)  #30 Tage über den nächsten Wendepunkt 
          {
            
            pre.wi=pre.wi+1
            
            w0=get.Index(dax, wendepunkte[pre.wi])
            #  w> wo
            if (visual1)
            {
              #plot(dax)
              lines(dax,col="red")
              amark(DateS(dax[w0]),col="darkgreen")
              amark(DateS(dax[w]),col="orange")
              mP("neuer wendepunkt####")
            }
            
          }      
      }
      
      write.table(posTable,fname,sep=";",row.names=F)  
      if (visual1)
      {
        mP("write %s ###################################################>",fname)
        View(posTable)
        browser()
      }
      return(posTable)
    } else
      mP("File already exists %s",fname)
    
  })
  allPosTables
}
make.tech.cloud<-cmpfun(make.tech.cloud_) #compilier das Teil

if (F)
{
  batch.Pos()  ########################  im Batch die tech-Cloud erstellen
  batch.Pos(visual=F,prof=T)
}
####################################################################
#passt das vorzeichen der beabsichtigen neuen position res zu den anderen Indikatoren
###################################################################
consistence.check_<-function(i,res,main="")
{
  if (res ==0)
    return(res)
  #return(res)
  cons.syms="itpSum,earlySigSum,uM,loM"
  cons.syms="earlySigSum,uM,loM"
  
  #cons.syms="itpSum,uM,loM"
  
  for (col in spl(cons.syms))
    if (res != sign.val(Pos[i,col]))
    {
      mP("%s to %d : consistence.break on %s",main,res,col)  
      res=0; 
      break
    }
  res
}
consistence.check<-cmpfun(consistence.check_) #compilier das Teil

#..........................................................................
###########################################################################
#ein LongShort-Timing modell - von Hand - welches die technische DataClouds (posTable) auswertet
#mit 
#Signal = xts(posTable[,"itpSum"], order.by=as.Date(posTable[,"date"]))
###########################################################################
getSig<-function(i)
{
  today=DateS(Pos[i,1])
  #browser(mP("%s %d",today,last.res))
  
  if (last.res==0)
    res=0
  else
  {    
    new.res=sign.val(Pos[i,"m.orange"])
    if (new.res != last.res)
    {
      new.res=consistence.check(i,new.res,"pos drehen wollen")
      if (new.res != 0 )
      {
        mP("pos drehen %s to %s",today,res)
        amark(today,col="orange")
        res = new.res
      }
    }
    else
      res = last.res
  }
  sig=nval(Signal[i])
  if (abs(sig) >=  4 && last.res >0 && sign(last.res) != sign(sig))  #crash-stop
  {
    res=0
    mP("stop %s to %s",today,res)
    amark(today,col="red")
  } 
  if (last.res ==0)
    if (abs(sig) < 2 && !Pos[i,"noTrend.i"]    )    #  abs(sig) < 2
    {
      res = sign.val(Pos[i,"SuloM"])
      #res =sign.val(Pos[i,"m.orange"])
      #das signal muss gut passen,
      res =consistence.check(i,res,"reentry wollen")
      
      if (F && today > as.Date("2008-01-01"))  #date debug
      {
        amark(today) 
        browser()
      }
      #sonst lieber wieder auf 0
      if (res !=0)
      {
        mP("reentry %s to %d",today,res)
        amark(today,col="darkgreen")
        #browser()
      }
    }
  last.res<<-res
  res
}
############################################################################
sign.val <-function(x)
{
  if (is.na(x) || is.na(nval(x)))
    return (0)
  return( sign(nval(x)))
}

##############################################################
#beetrachte die Unterschiede der tail der winkel der drei smooth-reihen
#betrachte ihre Vorzeichen
# sum(signals)
#sum(diff(tail(smooths)))   #sum(diff(mROC(tail,smooths)))
#und erkenne STeil-abstürze   sum(diff(tail(price),tail(smooths)))
#betrachte die quantile dieser differenzen - und sag ab wann ein wert
#überschritten wird
#betrachte  auch ob sich ein smooth der Hihgs anders verhält wie ein smooth der lows.
#gib auch sma 90 und sma200 hinzu  (selffullfillings )
#p ist die zeitreihe seit dem letzten Trend-wechsel
##############################################################
estimate.pos_<-function(posTable=NULL, p,visual =F)
{
  mP("\nestimate.pos %s shape  %d ",DateS(last(p)),shape(p))
  #  browser()
  p = m.ifna.prev(p)
  x=1:len(p)
  y=coredata(p)
  #damit loess auch prognostizieren kann:
  loess.m <- loess(y ~ x, data.frame(x=x,y=y),span=0.9,control = loess.control(surface = "direct"))
  
  newx=seq(1, 30, 1)
  loess.forec=predict(loess.m, data.frame(x = newx+last(x)),se=T)
  
  x1=c(x,newx+last(x))
  y1=c(y,loess.forec$fit)
  
  if (visual)
  {
    plot(x1,y1,type="n",col="blue")
    lines(x,y,col="black")
    lines(loess.m$x,loess.m$fitted,col="red",lwd=2)
    lines(tail(x1,len(newx)),tail(y1,len(newx)),col="blue",lwd=3)
  }
  ###################################################
  npmodel1 <- lowess(y~x,f=0.1)
  if (visual)lines(npmodel1, col="magenta", lwd=4)
  ys=ksmooth(p,glaettung=6)
  if (visual)lines(1:shape(ys),coredata(ys),col="orange",lwd=3)
  
  #die Zukunft
  #ys=ksmooth(dax,glaettung=3)
  #lines(1:shape(ys),coredata(ys),col="green")
  tlen=2
  
  p.red=nval(loess.m$fitted)
  p.orange=coredata(ys)
  p.magenta=nval(npmodel1$y)
  
  
  ## hieraus tabelle bauen
  l.red = nval(last(p.red))
  l.orange = nval(last(p.orange))
  l.magenta=nval(last(p.magenta))
  last.p = last(p)
  l.p = nval(last.p)
  l.p.date=DateS(last(p))
  p.90 = last(na.omit(p),90)
  p.200 = last(na.omit(p),206)
  
  #proczentuelle änderungen
  m.red=m.sig(p.red,tlen); 
  m.orange=m.sig(p.orange,tlen)
  m.magenta=m.sig(p.magenta,tlen)
  
  #krümmungen der smoothies
  #if (visual)browser(mP("...."))
  k.red=k.orange=k.magenta=last(p.90)
  k.red[]= last(ROC(ROC(last(p.red,11),5),5))*100
  k.orange[]=last(ROC(ROC(last(p.orange,11),5),5))*100
  k.magenta[]=last(ROC(ROC(last(p.magenta,11),5),5))*100
  
  
  sig.red=sign(m.red)
  sig.orange = sign(m.orange)
  sig.magenta = sign(m.magenta)
  sig.sum=sig.red+sig.orange+sig.magenta
  
  d.red= (l.red- l.orange) + (l.red-l.magenta)
  dm.red= (m.red-m.orange) + (m.red-m.magenta)
  dp.red = (l.red - l.p) 
  
  #mchart(merge(p,p.red))
  
  #soll steile price- crashs bemerken
  itp.dp.red=in.Trend.pos(last(p-p.red,200),visual=visual,main="itp.dp.red",last=T)$itp
  roc5=mROC(p,5); l.roc5 = last(roc5)
  itp.roc5 = in.Trend.pos(last(roc5,200),visual=visual,main="itp.roc5",last=T)$itp
  #  browser(mP("p200"))
  #if (shape(p.200) < 200)
  #  browser(mP("p.200"))
  l.300=last(p,300)
  faber=p-SMA(l.300,min(len(l.300),200)); l.rocfaber5=last(mROC(faber,5))
  itp.faber= in.Trend.pos(last(faber,200),visual=visual,main="itp.faber",last=T)$itp
  
  #..................................................................
  #welch steigung hat der long-term-trend und wie ist die itp im Trend
  #layout(1:2)
  m.lm=m.lm.fit(p,getIndex=T,visual=F,level=.95)
  all.r.squared= m.lm$r.squared
  all.m=m.lm$m
  m.chan=m.lm$fit$channel
  itp.all=in.Trend.pos(last.p, m.chan[l.p.date],visual=visual)   
  #welch steigung hat der 90-er-trend und wie ist die itp im Trend, wie valide ist der trend 
  m.lm.90=m.lm.fit(p.90,getIndex=T,visual=F,level=.95)
  p.90.r.squared= m.lm.90$r.squared
  p.90.m = m.lm.90$m
  m.chan.90=m.lm.90$fit$channel
  #mchart(merge(p.90,m.chan.90))  
  itp.90=in.Trend.pos(last.p, m.chan.90[l.p.date],visual=visual) 
  
  dm90ALL = xts(nval(all.m-p.90.m),as.Date(l.p.date))
  
  itp.dm90All = itp.pipe(dm90ALL,30,visual=F,last=T)
  #plot(p)
  #p.90.m.lowess=m.lm.fit(last(p,200),visual=T,getIndex=T,w="lowess")$m
  #differenz der steigungen zwischen kurs und sma200 muss groß genug sein, sonst taugt
  
  if (F)  #evtl. nicht nötig ... ausser es gibt probleme mit der noTrend-Erkennung
  {  
    #faber nicht als signalgeber
    #die steigung des SMA200 über 30 Tage regressed
    m.lm.SMA30=m.lm.fit(last(SMA(p.200,200),30),visual=F)
    sma.30.r.squared= m.lm.SMA30$r.squared
    sma.30.m = m.lm.SMA30$m
    #die steigung des kurses der letzten 30 Tage
    m.lm.30=m.lm.fit(last(p.90,30),getIndex=T,visual=F)
    p.30.r.squared= m.lm.30$r.squared
    p.30.m = m.lm.30$m
    #differenz der steigungen -> evtl. in noTrend verwenden
    
    sma.dm=p.30.m-sma.30.m
  }
  else
  {
    sma.dm=  p.30.r=p.30.m = m.lm.SMA30=0
  }
  #...........................wie ist die itp im Bollinger Band
  itp.bb=0
  if (T)
  {
    #if (l.p.date=="2000-12-29")  browser(mP("m.break"))
    
    bb=BBands(p.200,n=  min(len(p.200),200),maType = "SMA")
    itp.BB=bb[,4]
    itp.bb=in.Trend.pos(last.p,last( bb[,c("mavg","dn","up")]),last=T,visual=T)
    
    if (F&& visual)  #showBB
    {
      mchart(merge(p,bb[,c("mavg","dn","up")]),main="") #mPlots
      itp.bb=in.Trend.pos(p, bb[,c("mavg","dn","up")])
      mchart2(p,itp.bb)
      mchart(merge(p,bb[,-4]),bb[,"pctB"],itp.bb)
      
    }
  }
  #  if (shape(p.200)<200)
  #    sag("bug p200",warte=T)
  #......... liegt er in der target.range
  tlen=find.Wlen(p.90,target.range=15) #gehe so lange nach links bis du ein range von 15% hast
  Tlen=xts(nval(tlen),as.Date(l.p.date))
  itp.tlen=  itp.pipe(Tlen,60,visual=visual,last=T)
  
  tlen=min(30,tlen)    
  #auch ein itp der tlen... werte ... die sollten in starken Trends länger sein
  
  
  itpP= na.omit(in.Trend.pos(last(p.90,min(tlen,12)),visual=visual,main=sprintf("itpP-%d",tlen))$itp)
  
  
  itp.P= last(itpP)   #wenn itp.P im Intervall 3,-3 liegt - ist das eine
  
  #Indikation für Seitwärtsbewegung:  .. siehe auch range.up, range.low
  if (len(itpP)>10)
  {
    last.itpP=last(itpP,10)
    nl=max(10,len(last.itpP))
    noTrend=last(EMA(last.itpP,nl))  #in den letzten 10 Tagen war nichts los
    noTrend.i= noTrend >=-3 & noTrend <= 3 & itp.P >=3 & itp.P <= 3   #heute auch nicht
  }
  else
    noTrend.i=NA
  #.......... läuft der obere kanal anders wie der untere ?
  showTop=visual
  
  #muss hier alles über tlen statt p.90 laufen ???
  #tlen=20
  k=max(tlen,20); k=min(50,tlen) 
  K=5
  #  x=coredata(p)
  p.tlen=last(p.90,tlen*2)   #evtl. hier lieber bei den vollen p.90  bleiben ...?
  y=runquantile(coredata(p.tlen), k, probs=c(0.25,  0.5,  0.75))#,endrule="quantile",align="right"))
  col = c( "red", "green", "blue")
  up.low=merge(p.tlen,p.tlen,p.tlen)
  up.low[,2]=runmean(y[,,1],K)
  up.low[,1]=runmean(y[,,2],K)
  up.low[,3]=runmean(y[,,3],K)
  #up.low = lag(up.low,k/2)  #sonst wirds unrealistisch
  if (visual) {mchart(up.low,main=sprintf("tlen=%d",tlen));points(p.tlen,col="green",lwd=2)}
  #lines(p.tlen,lwd=2)
  
  #lowess regressionen durch die quantile
  mid=up.low[,1] ; mid.fit = m.sig(mid,3)
  #mid.fit=m.lm.fit(mid,visual=visual,getIndex=T,w="lowess")$m
  low=up.low[,2] ; low.fit=m.sig(low,3)
  #low.fit=m.lm.fit(low,visual=visual,getIndex=T,w="lowess")$m
  up=up.low[,3];   up.fit=m.sig(up,3)
  #up.fit=m.lm.fit(up,visual=visual,getIndex=T,w="lowess")$m
  #die summe der Quantiles-Steigungen
  #die Q steigungen: mehrheitsentscheid
  Q.m = xts(sum(sign(mid.fit),sign(low.fit),sign(up.fit)),as.Date(l.p.date))
  # browser(mP("xxxx"))
  #lineare steigung der preise auf dem stück
  p.tlen.m = xts(m.lm.fit(p.tlen,visual=visual,getIndex=T,w="")$m,as.Date(l.p.date));colnames(p.tlen.m)=colnames(p.90)
  
  
  #  browser(mP("tlen Q.m %d und p.tlen.m %f",Q.m, p.tlen.m))
  
  #  if (visual)mchart(merge(p.90,mid,low,up))
  if (showTop) mchart(merge(p.tlen,mid,low,up),main=sprintf("showTop tlen %d",tlen))
  
  highs <- findpeaks(as.numeric(p.tlen[p.tlen>=up ]) )[,2]
  if (len(highs)> 3)  highs=index(p.tlen[p.tlen>=up ])
  upper=p.tlen[p.tlen>=up ][highs]
  if (showTop) points(upper,col="blue",lwd=1)
  
  lows <- findpeaks(-as.numeric(p.tlen[p.tlen <=low]) )[,2] 
  if (len(lows)> 3)  lows=index(p.tlen[p.tlen<=up ])
  lower = p.tlen[p.tlen <=low][lows]
  if (showTop)points(lower,col="red",lwd=1)
  
  #geringe range.up heißt:  oben liegt ein deckel drauf
  range.up=range(upper);  range.up=diff(range.up)/range.up[1]*100
  range.low= range(lower); range.low=diff(range.low)/range.low[1]*100
  
  
  #die Steigungen durch die oberen und unteren - separat gefitteten Kanäle
  u.lm=m.lm.fit(upper,visual=F,getIndex=T, level=0.95,nplus=3)
  # u.lm=m.lm.fit(upper,visual=T,getIndex=T, level=0.95,nplus=3,w="lm.w",w.range=0.3)
  ls(u.lm)
  uM = u.lm$m   #die steigung des oberen Kanals
  if (showTop && !is.na(u.lm$fit))
  { 
    mchart(merge(upper,u.lm$fit$channel));points(upper)
    
    #plot(upper)
    #X=get.Index(atDate=as.Date(index(upper)))
    #p.lineal=xts(1:last(X),as.Date(index(first(upper)))+1:last(X))
    #upper1=merge(p.lineal,p.lineal,p.lineal);   
    #upper1[]=predict(u.lm$fit,newdata=data.frame(x=1:last(X)),interval="prediction",level=0.95)
    #mchart(upper1)
    #lines(upper,col= "magenta");points(upper)
    
  }
  lo.lm=m.lm.fit(lower,visual=F,getIndex=T,level=0.95,nplus=0)  
  loM= lo.lm$m #die Steigung des unteren Kanals
  if (showTop && !is.na(lo.lm$fit))    
  {
    mchart(merge(lower,lo.lm$fit$channel));points(lower)
  }
  SuloM = uM+loM
  #..........schreib alles in den posTable data.frame
  
  date=l.p.date
  
  ITP=c(coredata(itp.dp.red),itp.roc5,itp.faber, itp.bb, itp.90,itp.P)
  itpSum= mean(coredata(ITP),na.rm=T) #sehr wichtig !!!
  
  early=as.vector(c(coredata(l.roc5),l.rocfaber5  ,k.red,k.orange,k.magenta, Q.m, p.tlen.m,p.30.m))
  earlySigSum = sum(sapply(coredata(early),sign),na.rm=T)
  Q.m = nval(Q.m)
  allSigSum=sum(sapply(na.omit(as.numeric(
    c(uM,loM,SuloM,sig.red,sig.orange,sig.magenta,sig.sum, itpSum, itp.dp.red,itp.roc5,itp.faber,k.red,k.orange,k.magenta, itp.bb, itp.90,itp.P ,earlySigSum,l.roc5,l.rocfaber5,m.red,m.orange,m.magenta,d.red,dm.red, dp.red,all.m, itp.all,  p.90.m,sma.dm,range.up,range.low,Q.m,p.tlen.m))),
    sign),na.rm=T)
  
  target=l.p #nur damit man leichter in xls analysieren kann
  
  #browser(mP("ITP"))
  
  
  posTable = appendTable(posTable,date,target,noTrend.i,uM,loM,SuloM,itpSum,sig.red,sig.orange,sig.magenta,sig.sum, itpSum, itp.dp.red,itp.roc5,itp.faber, itp.bb, itp.90,itp.P ,itp.tlen,itp.dm90All,earlySigSum,allSigSum, l.roc5,l.rocfaber5,k.red,k.orange,k.magenta,m.red,m.orange,m.magenta,d.red,dm.red, dp.red,all.m, all.r.squared,itp.all,  p.90.m,p.90.r.squared,p.30.m,Q.m,p.tlen.m,sma.dm,range.up,range.low  )
  
  #itp.tlen,itp.dm90All
  
  mP("sig.sum %s %d",date,sig.sum)
  
  if (visual)
    View(posTable)
  if (visual)
    browser(mP("......................... %s",l.p.date))
  
  xts(sig.sum,as.Date(index(last(p))))
  return(posTable)
}
estimate.pos<-cmpfun(estimate.pos_) #compilier das Teil



##########################################################
###########################################################

if (F)
{
  plot(dax)
  x=1:shape(dax)
  y=coredata(dax)
  plot(x,y,xlab="log time",ylab="log flux",type="l")
  npmodel1 <- lowess(y~x,f=0.02)
  lines(npmodel1, col=4, lwd=2)
  
  lm=loess.smooth(x,y=y,span=0.02)   #MM_LOESS_SMOOTH
  #grafing in x,y punkten
  plot(x,y,pch=".")
  lines(lm$x,lm$y,col="red",lwd=2)
  
  lm=loess.smooth(x,y=y,span=0.02,family = "gaussian")   #MM_LOESS_SMOOTH
  #grafing in x,y punkten
  #plot(x,y,pch=".")
  lines(lm$x,lm$y,col="blue",lwd=2)
  
  loess.m <- loess(y ~ x, data.frame(x=x,y=y),span=0.02)
  plot(x,y,type="l")
  lines(loess.m$x,loess.m$fitted,col="red",lwd=2)
  len
  
  #damit loess auch prognostizieren kann:
  loess.m <- loess(y ~ x, data.frame(x=x,y=y),span=0.09,control = loess.control(surface = "direct"))
  
  newx=seq(1, 30, 1)
  loess.forec=predict(loess.m, data.frame(x = newx+last(x)),se=T)
  
  x1=c(x,newx+last(x))
  y1=c(y,loess.forec$fit)
  plot(x1,y1,type="n",col="blue")
  lines(x,y,col="black")
  lines(loess.m$x,loess.m$fitted,col="red",lwd=2)
  lines(tail(x1,len(newx)),tail(y1,len(newx)),col="blue",lwd=3)
  ###################################################
  npmodel1 <- lowess(y~x,f=0.1)
  lines(npmodel1, col=4, lwd=2)
  ys=ksmooth(dax,glaettung=3)
  lines(1:shape(ys),coredata(ys),col="green")
  
}
#################################################################################
#################################################################################

signal.TEST.1 <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  sma200[is.na(sma200)]<-0
  signal = iif(p >= sma200,1,0)
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}
#......... dynamisches SMA ...

roll.itp.P<-function(i,P,visual=F)
{
  today=DateS(P[i])
  p=P[i]
  itp.p.ALL=itp.pipe(p,50,visual=visual,last=T,opt.getQ="get.Quantile.M") 
  if (visual)
  {
    print(today)
    print(itp.p.ALL)
  }
  crashStop= itp.p.ALL$itp < -3
  
}

#sieht unfassbar super aus
signal.TEST.1.a <-function(arg, par = mlist( sma.w=c(120,90,220,10)),visual=F,...)
{
  p=mNorm(arg$clos)
  #sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  #sma200[is.na(sma200)]<-0
  #w=3
  #roc.sma=ROC(sma200,w);
  #itp.p=itp.pipe(roc.sma,50,visual=T,last=F)
  #itp.p.ALL=itp.pipe(p,50,visual=visual,last=F,opt.getQ="get.Quantile.M") 
  
  #roll mal über itp  .. dauert ewig lange
  #signal[]=sapply(1:len(p), roll.itp.P,P=p,visual=F)
  #rasend schnell weil voll vector-orientiert
  
  itp.s= in.Trend.pos(p,visual=F,main=sprintf("itp.pipe %s %d",name,50),last=F,opt.getQ="get.Quantile.M",k=as.integer(par$sma.w))
  
  #mchart2(p,-itp.s$Q.m)
  signal = sign((itp.s$Q.m[,2]+itp.s$Q.m[,1]))
  Qm=merge(p,itp.s$Q.m[,4],itp.s$Q.m[,5],itp.s$Q.m[,6]) #mid,low,up
 # browser(mP("Q"))
  #if (Q[,])
  
  #plotSigPrice(signal=signal,prices=dax,indi=list(macd=-itp.s$Q.m))
  #  Q=merge(p,itp.s$mid,itp.s$low,itp.s$up),
  
  return(list(Signal=signal, Indi=list(Q.m=merge(itp.s$Q.m[,2],itp.s$Q.m[,1]),Q=merge(p,itp.s$Q.m[,4],itp.s$Q.m[,5],itp.s$Q.m[,6]))))#Q.m=-itp.s$Q.m)))
}
if (F)
  res=indi.Generic("signal.TEST.1.a", global_arg, par=list(sma.w=120),visual=T,TRAINSYM =-1)#data$BENCH)





#test am zigzag
signal.TEST.1.b <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  p=dax
  DAX1=ZigZag(mNorm(arg$clos));colnames(DAX1) = "DAX1.Close"
  p=DAX1=ZigZag(mNorm(dax));colnames(DAX1) = "DAX1.Close"
  #wendepunke rausholen
  d1=sign(ROC(DAX1))-lag(sign(ROC(DAX1)))
  d1[d1==0]<-NA; 
  d1 = na.omit(d1) /2
  layout(1:2)
  
  plot(DAX1)
  
  amark(d1)
  print(d1)
  amark("2011-02-20",col="blue")   #am Top
  p=DAX1["::2011-02-20"]
  p
  itp.s= in.Trend.pos(p,visual=T,main=sprintf("itp.pipe %s %d",name,50),last=F,opt.getQ="get.Quantile.M",k=50)
  mchart(itp.s$Q.m)
  amark("2011-02-20",col="orange")
  
  
  plot(  p-lag(p,50))
  plot(p)  
  
  k=min(shape(p)-1,50);K=20
  x=coredata(p)
  y=runquantile(x, k, probs=c(0.05,  0.5,  0.95),align="right")
  y=runquantile(x, k, probs=c(0.05,  0.5,  0.99),endrule="quantile",align="right")
  
  up.low=merge(p,p,p)
  
  up.low[,2]=y[,1,1]
  up.low[,1]=y[,1,2]
  up.low[,3]=y[,1,3]
  K=10
  up.low[,2]=runmean(y[,1,1],K,align="right")
  up.low[,1]=runmean(y[,1,2],K,align="right")
  up.low[,3]=runmean(y[,1,3],K,align="right")
  
  mid=up.low[,1]
  low= up.low[,2]
  up=up.low[,3]  
  frame="2007::2010-01-01"
  mchart(merge(p,mid,low,up)[frame]);lines(p,col="magenta",lwd=2)
  sig = iif(p>=mid,1,-1)
  
  Indi=merge(p,mid,low,up)
  colnames(sig)=colnames(p)=c("DAX")
  plotSigPrice(signal =-sig,prices=p,indi=list(a=p),main="mist")
  
  
  res = iif (sig >=0, round((p-mid)/((up-mid)/3)), round((p-mid)/((mid-low)/3)))
  if (visual)
  {
    col = c( "red", "green", "blue")        
    plot(x, col="black", main = sprintf("in.Trend.Pos(%s,%s smoothed %d)",name,main,K))
    lines(runmean(y[,1,1],K), col=col[1])
    lines(runmean(y[,1,2],K), col=col[2])
    lines(runmean(y[,1,3],K), col=col[3])
    abline(h=last(x),lwd=3)
  }
  Q.m=merge(m.mid=m.sig(mid,tlen=k),m.low=m.sig(low,tlen=k),m.up=m.sig(up,tlen=k))
  
  name="DAX1"
  as.Date("2011-02-21")-50
  
  itp.s= in.Trend.pos(p["2011-01-01::2011-02-21"],visual=T,main=sprintf("itp.pipe %s %d",name,50),last=F,opt.getQ="get.Quantile.M",k=50)
  itp.s$Q.m
  
  layout(1:2)
  mchart(p["::2011-01-10"])
  mchart(itp.s$Q.m["::2011-01-10"])
  mchart(p["::2011-02-21"])
  #mchart2(p,-itp.s$Q.m)
  head(  itp.s$Q.m ,52)
  signal = sign((itp.s$Q.m[,2]+itp.s$Q.m[,1]))
  plotSigPrice(signal=signal,prices=p,indi=list(macd=itp.s$Q.m))
  
  return(list(Signal=signal, Indi=list()))#Q.m=-itp.s$Q.m)))
}



#test mit roll
signal.TEST.1.c <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  p.org=mNorm(arg$clos)
  #init arrays
  signal=p.org ;signal[]=NA;  Q.m=merge(p.org,p.org,p.org) ;Q.m[]=NA
  
  no=sapply(51:len(p.org),function(t)
  {
    p=p.org[1:t,]
    itp.s= in.Trend.pos(p,visual=F,main=sprintf("itp.pipe %s %d",name,50),last=F,opt.getQ="get.Quantile.M",k=50)
    Q.m[t,]<<- coredata(last(itp.s$Q.m))
    signal.t =  sign(Q.m[t,2]+Q.m[t,1])
    signal[t,] <<- coredata(signal.t)
  })
  return(list(Signal=signal, Indi=list(Q.m=merge(Q.m[,2],Q.m[,1]))))
  
}
if (F)
  res=indi.Generic("signal.TEST.1.c", global_arg, par=list(sma.w=200),visual=T,TRAINSYM =data$BENCH)




#...........................................................................
wrapper=function(sym) {  
  res=indi.Generic("signal.TEST.1.a", global_arg, par=list(sma.w=200),visual=F,TRAINSYM =sym)
  #browser(mP("wrapper"))
  
  res
}

#.........................................................................
if (F)  #parallel über alle symbole
{
  itp.PIPES$roc.sma=list()
  sfExport("signal.TEST.1.a")
  sfSource("MLib/labor_signal.r")
  sfExport("wrapper")  
  x=wrapper("DAX")
  
  system.time(lapply(data$symbolnames,wrapper))
  names(se[[5]])
  c.se[[5]]$best
  
  system.time(sfLapply(data$symbolnames,function(sym)
    indi.Generic("signal.TEST.1.a", global_arg, par=list(sma.w=200),visual=F,TRAINSYM =sym))) #sfClusterApplyLB
  
  system.time(sfClusterApplyLB(data$symbolnames,wrapper)) #
  
  system.time(sfClusterApplyLB(data$symbolnames,function(sym)
    indi.Generic("signal.TEST.1.a", global_arg, par=list(sma.w=200),visual=F,TRAINSYM =sym))) #sfClusterApplyLB
  
  x=indi.Generic("signal.TEST.1.a", global_arg, par=list(sma.w=300),visual=F, TRAINSYM =-1,experiment="")
  
  
  c.se=sfLapply(data$symbolnames,wrapper) #sfClusterApplyLB
  #c.se=sfClusterApplyLB(data$symbolnames,wrapper) #
  names(c.se)
  names(c.se[[5]])
  c.se[[5]]$best
  
  x=indi.Generic("signal.TEST.1.a", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX")
  
  x=indi.Generic("signal.TEST.1.a", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,experiment="o")
  
  DAX1=ZigZag(dax);colnames(DAX1) = "DAX1.Close"
  
  global_arg$dat$DAX1 =DAX1  
  colnames(global_arg$SG2R)
  global_arg$clos=merge(global_arg$clos,DAX1)
  colnames(global_arg$clos)
  global_arg$symbolnames = c(global_arg$symbolnames ,"DAX1")
  itp.PIPES<<-list()
  names(itp.PIPES)
  dim(itp.PIPES$roc.sma)
  dim(itp.PIPES$p)
  
  itp.PIPES
  plot(itp.p)
}



#--------------------------------------------------------------------------------

signal.TEST.1.schrott <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  sma200[is.na(sma200)]<-0
  signal = iif(p >= sma200,1,0)
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}

#--------------------------------------------------------------------------------


#.... robFilter wurde aussen schon mal berechnet
signal.TEST.2 <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  dax=mNorm(arg$clos)
  rob =get_rob(dax,ow=220)
  # browser()
  macd= rob$filter[,spl("MED,TRM")]
  macd= merge(dax,rob$filter[,spl("MTM")])
  macd[,2] = macd[,2]-0.1
  
  signal= - iif(macd[,1]> macd[,2],-1,1)
  
  return(list(Signal=signal, Indi=list(macd=macd)))
}
#.........................................................................
if (F)
{
  x=indi.Generic("signal.TEST.2", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =data$BENCH)
  x=indi.Generic("signal.TEST.2", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
}   
#...........................................................................
if (F)
{
  wlen=110
  dax = data$prices["SX5R"]
  
  rob =get_rob(mNorm(dax),iw=wlen/2,ow=wlen)
  purePlot(merge(mNorm(dax),rob$filter))
  lines(SMA(mNorm(dax),200),col="orange",lwd=3)
  macd= rob$filter[,spl("MED,TRM")]
  macd= merge(mNorm(dax),rob$filter[,spl("MTM")])
  macd[,2] = macd[,2]-0.1
  
  
  
  purePlot(macd)
  signal= -iif(macd[,1]> macd[,2],-1,1)
  plotSigPrice(signal=signal,prices=dax,indi=list(macd=macd))
  
  
  sma200=SMA(mNorm(dax),200)
  signal=dax[,1]
  signal[]=rowMeans( bt.apply.matrix(rob$filter,function(x) iif(x>sma200,1,-1)))
  signal[]=rowMeans( bt.apply.matrix(rob$filter,function(x) sign(ROC(x,5))))
  
  signal = iif(abs(signal)<1, 0, sign(signal))
  signal = sign(signal)
  plotSigPrice(signal=signal,prices=dax,indi=list(macd=macd))
  
  
}


################################################################################

#forecasting book
#https://www.otexts.org/fpp/

if  (F)#http://robjhyndman.com/hyndsight/batch-forecasting/
{
  library(forecast)
  
  retail <- read.csv("http://robjhyndman.com/data/ausretail.csv",header=FALSE)
  retail <- ts(retail[,-1],f=12,s=1982+3/12)
  
  ns <- ncol(retail)
  h <- 24
  fcast <- matrix(NA,nrow=h,ncol=ns)
  for(i in 1:ns)
    fcast[,i] <- forecast(retail[,i],h=h)$mean
  
  write(t(fcast),file="retailfcasts.csv",sep=",",ncol=ncol(fcast))
}

if (F)#http://robjhyndman.com/hyndsight/forecasting-annual-totals/
{
  #If x is your monthly time series, then you can con???struct annual totals as follows.
  
  x=p
  library(forecast)
  y <- filter(x,rep(1,12), sides=1) # Total of last 12 months
  mPlots(merge(p,y))
  mPlots(y)
  #To get the fore???casts of the annual totals:
  y=p
  fit <- auto.arima(y)
  plot(forecast(fit,h=120))
  #plot(fit)
  forecast(diff(p,5))
  #The last fore???cast is for the total of the next year.
}
if (F)
{
  # It is com???mon to fit a model using train???ing data, and then to eval???u???ate its per???for???mance on a test data set. When the data are time series, it is use???ful to com???pute one-step fore???casts on the test data.  The same process works for ARIMA mod???els when ets is replaced by Arima or auto.arima. Note that it does not work with the arima func???tion from the stats pack???age.
  library(forecast)
  #Exponential smoothing state space model
  fit <- ets(trainingdata)
  fit2 <- ets(testdata, model=fit)
  onestep <- fitted(fit2)
  
  gold.fcast <- rwf(gold[1:60],h=50)
  plot(gold.fcast)
  
  auto.arima()
}
############################################################################################

arima.forecast.1= function(P)
{
  fit.arima= auto.arima(ts(P,356))
  res = forecast(fit.arima)
  #print(last(P))
  print(res$method) 
  res=coredata(res$mean)- coredata(last(P))
  res=xts(res,index(last(P)))
  print(res)
  res
}
arima.forecast.sma= function(p,n=200)
{
  fit.arima= auto.arima(sma(p) )
  res = forecast(fit.arima, h=1)
  print(res$method) 
  res=coredata(res$mean)- coredata(last(P))
  res=xts(res,index(last(P)))
  print(res)
  res
}
ets.forecast= function(p)
{
  fit= ets(p )
  #fit2=auto.arima(p)
  res = forecast(fit, h=1)
  
  print(res$model)
  print(res$method)
  # sig=res$mean[2]-res$mean[1]
  # mPlot(merge(p,xts(res$fitted,index(p)))) 
  res=coredata(res$mean)- coredata(last(p))
  res=xts(res,index(last(p)))
  print(res)
  res
}

####################################################################################

signal.ets <-function(arg, par = mlist( wlen=c(61,21,120,10)),visual=F,...)
{
  p=arg$clos
  wlen=61
  wlen = as.integer(par$wlen)
  sym=colnames(p)
  # p=arg$dat$prices
  
  Ets.forecast= function(p)
  {
    #  browser(mP("xx"))
    #temp= try(HoltWinters(ts(na.omit(p),frequency=7) )) 
    #temp= try(auto.arima(ts(na.omit(p),frequency=7) )) #ein contra-indi
    temp= try(ets(ts(na.omit(p),frequency=7) )) #bessere aic-werte wie auto.arima
    #temp=try(lm.lowess(na.omit(p),visual=T,getIndex=T,main="lowess",glaettung=0.2))   
    #temp <- StructTS(ts(na.omit(p),frequency=7)) #schnell aber nicht doll
    if(inherits(temp, 'try-error')) 
    {
      res=last.res
    }
    else
    {
      fit=temp
      #fit2=auto.arima(p)
      res = forecast(fit, h=31)  #h=10
      #res=predict(fit,n.ahead=2)
      # plot(res)
      #  sag("warte",warte=T)
      print(res$model)
      print(res$method)
      # sig=res$mean[2]-res$mean[1]
      # mPlot(merge(p,xts(res$fitted,index(p)))) 
      #res=coredata(res$mean)- coredata(last(p))
      
      res=res$mean
      
      res=m.xts(coredata(res),start.date=index(last(p)))
      m.lm=m.lm.fit(res,getIndex=T,visual=F,level=.95)  #signal ist die steigung der reg-gerade duch die forecasts
      res=m.lm$m  
      res=m.xts(coredata(res),start.date=index(last(p)))
      print(res)
      
      last.res <<-res
    }
    res
  }
  arima.fcst = rollapplyr( p["2008::2009"], FUN="Ets.forecast",width=wlen, na.pad=T)
  
  #shiny::runGitHub(repo = "shinyApps", username = "lebebr01", subdir = "guessCorr")
  signal=arima.fcst
  #signal[signal==0]<-NA
  #signal = m.ifna.prev(signal)
  signal=sign(signal)
  b= na.omit(merge(signal,mNorm(p)))
  plotSigPrice(signal=b[,1],prices=b[,2])
  
  return(list(Signal=signal, Indi=list(ma=merge(arima.fcst,0))))
}

#
#
if(F)
{
betas=beta.again.portfolio(prices)-1
mchart(betas);abline(h=0)

for(pi in 1:ncol(betas))
{
 P=na.omit(betas[,pi])
 fit.arima= ets(P)
 plot(fit.arima$fitted)
 res = forecast(fit.arima, h=10)
 plot(res)
 mchart(merge(P,0))
}
a

}
####################################################################################



if (F)  #TESTFILTER
{
  P=na.omit(SMA(na.omit(ZigZag(p[1000:1500])),5))   #zigzag-reihe - gut für causalitäts test
  
  
  p_0=lm.lowess(P,glaettung=0.1)#$y
  plot(coredata(P),type="l",main="lowess");lines(p_0$y,col="red");
  
  p0=lm.loess(P,glaettung=0.1)#$y
  plot(p0$y,type="l",main="loess");lines(p0$fitted,col="red");
  
  p1=lm.gamm(P,glaettung=50)#$gam$fitted.values
  plot(p1$gam)
  plot(coredata(P),type="l",main="gamm");lines(p1$gam$fitted.values,col="red");
  
  p2= lm.smooth.spline(P,glaettung=0.6)
  plot(coredata(P),type="l",main="smooth.spline");lines(p2$y,col="red");
  
  
  p3 =lm.gbm(P)
  plot(p3)
  plot(coredata(P),type="l",main="gbm");lines(p3$fit,col="red");
  foreC = xts(predict(p3,data.frame(X = coredata(P)),n.trees=500),index(P))
  plot(foreC,type="l")
  purePlot(foreC,px)
  
  
  pred1<-predict(p1$gam,newdata=data.frame(x=10:20),se=T) 
  
  pred2<-predict(p0,newdata=data.frame(x=10:20),se=T) 
  pred2<-predict(p3$fit,newdata=data.frame(x=10:20),se=T) 
  predict(p2)
  
  ls(p0)
  anova( p1$lm)
  best=m2
  plot(best$gam, residuals = TRUE, pch = 19, cex = 0.75)
  
  plot(p1$gam)
  ls(p1$gam)
  p1$gam$smooth
  
  
  #-----------------------------------------------------
  library(robfilter)
  p=data$prices["2008::2010",data$BENCH]
  p02=robreg.filter(ts(p),width=21,extrapolate=1,online=T,method="MED")#,method="MED") LTS #causal
  plot(p02)
  p03=SMA(P,n=21)
  mchart(merge(P,p03))
  
  plot(coredata(p),type="l")
  lines(runquantile(p,k=91,align="right",probs=0.69),type="l",col="magenta");  #causal
  lines(coredata(p),col="blue",type="l")
  lines(runquantile(p,k=161,align="right",probs=0.01),type="l",col="red");
  lines(runquantile(p,k=161,align="right",probs=0.5),type="l",col="green");
  
  #plot(p,type="l");
  lines(coredata(ksmooth(p,glaettung=5)),col="magenta",lwd=2) #non causal
  
  # Filtering with trimmed RM and double window TRM only:   #causal
  y2.dw <- dw.filter(ts(p["2008:2009"]), online=T,outer.width=61, inner.width=21, method=spl("MED,RM,MTM,TRM,DWMTM,DWTRM"))
  plot(y2.dw)
  
  lines(coredata(P))
  
  library(TTR)
  ZLEMA()
  EMA()
  DEMA()
  filter()
  
  library(forecast)
  afit=auto.arima(ts(p))
  plot(forecast(afit,h=20))
  ls(afit$arma)
  ls(afit$model)
  plot(y2.dw)
  
  px=p
  TrainData = data.frame(X=as.numeric(c(1:dim(px)[1])), Y = as.vector(coredata(px)));
  colnames(TrainData) = c("X","Y")
  
  Price.lo <- ppr(Y ~ X, TrainData, nterms = 2, max.terms = 5, sm.method = c("supsmu", "spline", "gcvspline")[1],bass=99)   #kann auch multivar
  sm=xts(Price.lo$fitted, index(px))
  mchart(merge(px,sm))
  lines(xts(coredata(ksmooth(p,glaettung=5)),index(px)),col="magenta",lwd=2) #non causal
  
  foreC = xts(predict(Price.lo,data.frame(X = as.numeric(c(1:dim(TrainData)[1])))),index(px))
  plot(foreC,type="l")
  purePlot(px, foreC)
  #lines(foreC,col="green",lwd=2)
  
  mchart(na.omit(merge(price,sm)))
  
  ##################################
  library(signal)
  #sehr nett und schnell: low-pass-filter auf finanz-daten - mit shift und ohne lag-korrektur
  bf <- butter(3, 0.009)  #2.par: je kleiner desto glatter # 10 Hz low-pass filter
  bf2 <- butter(3, 0.015)  #2.par: je kleiner desto glatter     # 10 Hz 
  x <- coredata(p["2008::2009"])
  x <- coredata(p)
  
  t=1:shape(x)
  y <- filtfilt(bf, x)  #versucht das gefilterte signal ohne lag ...(2.pass)
  z <- signal::filter(bf, x) # apply filter  .. einfach den butter - filter-> lag
  z2 <- signal::filter(bf2, x) # apply filter  .. einfach den butter - filter-> lag
  
  plot(t, x,type="l")
  lines(t, y, col="green")  #backshifted
  lines(t, z, col="blue")
  lines(t, z2, col="red")
  
  lines(ZLEMA(x,190),col="green",lwd=2)
  lines(SMA(x,200),col="brown",lwd=2)
  lines(SMA(x,90),col="cyan",lwd=2)
  
  legend("bottomleft", legend = c("data", "filtfilt", "filter"), 
         pch = 1, col = c("black", "red", "blue"), bty = "n")
  
  
  f1=ZLEMA(p,100)
  f2=SMA(p,200)
  signal1 = sign(f1-f2)
  signal2= sign(rollRegressionXTS(p,50))
  signal=sign(signal1+signal2)
  
  signal[signal==0]=NA; signal=m.ifna.prev(signal)
  plotSigPrice(signal=signal,prices=p,indi=list(f=merge(p,f1,f2)))
  
  ########################
  p=data$prices[,data$BENCH]  
  P=na.omit(SMA(na.omit(ZigZag(p[1000:1500])),5))   #zigzag-reihe - gut für causalitäts test
  p=P
  library(kza)
  #ein schneller - filter kz - sehr ähnlich zu ksmooth() !!
  k.kz=kz(coredata(p),150)  #nicht kausal
  k.kz.xts <- xts(k.kz,index(p))
  mchart(merge(p,k.kz.xts,ksmooth(p,glaettung=5)))
  
  # kza reconstruction of the signal  (break-point-erkennung ??)
  y=coredata(p)
  m=150
  
  k.kza <- kza(y,m,y=k.kz,min_size=10)
  
  
  ##############
  #feature - extraktion:
  #gegeben sein ein set fl von n filter-lines.
  #zähle für jeden fl unterhalb von wie vielen der anderen fl er sich befindet
  #-> n -dim -vektor von integer-zahlen zwischen n und 0
  
  Z2=xts(z2,index(f1));Z=xts(z,index(f1))
  f3=SMA(p,90)
  
  fl =na.omit(merge(p,f1,f2,f3,Z2,Z)[-c(1:200)])  #einschwinger abschneiden
  purePlot(fl["1999::2000"])  
  purePlot(fl["2000-01-03::2000-01-05::"])
  
  #codiere:  oberhalb von <x> anderen:
  px=Z2
  sapply(c(1,2,3),function(d) diff(coredata(px),d))
  
  px=na.omit(P)
  plot(px)
  tail(px)
  
  
  
  
  #verschieden Methoden zur Auswertung eines Filters
  View(merge(g.Signal.r(px,5),g.Signal.d(px,5),g.Signal.D(p,px),g.Signal.D.zlema(p,px,10),g.Signal.D.zlema.q(p,px,10,.20)))
  
  feature=
    foreach(base.i=1:ncol(fl),.combine="merge") %do% {
      fl.base=fl[,base.i]
      fl.2 =fl[,-base.i]
      res=xts(rowSums(bt.apply.matrix(fl.2 ,function(col) {res1=sign( fl.base-col);res1=ifelse(res1<0,0,1)})),index(fl))
      colnames(res)=colnames(fl.base)
      res
    }
  
  #wie feature aber ohne sign und ohne ifelse:   also die summe der Unterschiede zu den nachbarn
  feature2=
    foreach(base.i=1:ncol(fl),.combine="merge") %do% {
      fl.base=fl[,base.i]
      fl.2 =fl[,-base.i]
      res=xts(rowSums(bt.apply.matrix(fl.2 ,function(col) {res1=( fl.base-col);})),index(fl))
      colnames(res)=colnames(fl.base)
      res
    }
  
  #Target für die Benchmark sei Summe aller multiTarget
  BENCH=1
  t.bench=
    foreach(target=data$multiTarget,.combine="merge") %do% {
      target[,BENCH]
    }
  
  t.Bench= xts(rowSums(t.bench,na.rm=T),index(t.bench)) #summe aller Multitarget für Bench
  #target-kontrolle:
  plotSigPrice(signal=sign(t.Bench),prices=p,indi=list(f=merge(p,f1,f2)))
  ############# 3 Classifier:
  train.data=merge(t.Bench,feature,feature2)
  crs=new.env()
  classifier.randomForest.fit(crs,train.data,dimTarget=1,ntree=1000)   #nach dem Aufruf ist crs bereit
  check.classifier(crs,crs$rf)
  #.................................
  
  library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
  crs$j48 <- J48(as.factor(crs$Target) ~ .,    data=crs$tdata)
  summary(crs$j48)
  check.classifier(crs,crs$j48)
  #................................
  library(rpart)  
  #http://www.statmethods.net/advstats/cart.html
  crs$rpart <- rpart(as.factor(crs$Target) ~ .,
                     data=crs$tdata,
                     method="class",
                     parms=list(split="information"),
                     control=rpart.control(usesurrogate=0, 
                                           maxsurrogate=0))
  check.classifier(crs,crs$rpart)
  # prune the tree
  fit=crs$rpart
  crs$rpart<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  check.classifier(crs,crs$rpart)  #prune hat nur wenig geholfen
  
  # Eine Textansicht des Modells Entscheidungsstruktur erstellen
  summary(crs$rpart)
  
  
  #................................
  mchart(feature2)
  lines(p)
  feature["2000-01-03"]
  View(feature["2000-01-03::2000-01-05::"])
  
  #tune nun den glaettungs-parameter so, das signal aus dem filter optimal zum target passt
  #dazu die signal-train- methode so anpassen, dass sie nicht immer die system-guv optimiert- sonder die differenz zum target minimiert ...
  
}
#########################################################################################
########################################################################################
if (F)  #TESTFILTER
{
  p_0=lm.lowess(P,glaettung=0.1)#$y
  plot(coredata(P),type="l",main="lowess");lines(p_0$y,col="red");
  
  p0=lm.loess(P,glaettung=0.1)#$y
  plot(p0$y,type="l",main="loess");lines(p0$fitted,col="red");
  
  p1=lm.gamm(P,glaettung=50)#$gam$fitted.values
  plot(p1$gam)
  plot(coredata(P),type="l",main="gamm");lines(p1$gam$fitted.values,col="red");
  
  p2= lm.smooth.spline(P,glaettung=0.6)
  plot(coredata(P),type="l",main="smooth.spline");lines(p2$y,col="red");
  
  
  p3 =lm.gbm(p=P)
  plot(p3)
  plot(coredata(P),type="l",main="gbm");lines(p3$fit,col="red");
  foreC = xts(predict(p3,data.frame(X = coredata(P)),n.trees=500),index(P))
  plot(foreC,type="l")
  purePlot(foreC,px)
  
  
  pred1<-predict(p1$gam,newdata=data.frame(x=10:20),se=T) 
  
  pred2<-predict(p0,newdata=data.frame(x=10:20),se=T) 
  pred2<-predict(p3$fit,newdata=data.frame(x=10:20),se=T) 
  predict(p2)
  
  ls(p0)
  anova( p1$lm)
  best=m2
  plot(best$gam, residuals = TRUE, pch = 19, cex = 0.75)
  
  plot(p1$gam)
  ls(p1$gam)
  p1$gam$smooth
  
  
  #-----------------------------------------------------
  library(robfilter)
  p02=robreg.filter(ts(P),width=21,extrapolate=1,online=T)  #causal
  
  plot(coredata(p),type="l")
  lines(runquantile(p,k=91,align="right",probs=0.69),type="l",col="magenta");  #causal
  lines(coredata(p),col="blue",type="l")
  lines(runquantile(p,k=161,align="right",probs=0.01),type="l",col="red");
  lines(runquantile(p,k=161,align="right",probs=0.5),type="l",col="green");
  
  #plot(p,type="l");
  lines(coredata(ksmooth(p,glaettung=5)),col="magenta",lwd=2) #non causal
  
  # Filtering with trimmed RM and double window TRM only:   #causal
  y2.dw <- dw.filter(ts(p["2008:2009"]), online=T,outer.width=61, inner.width=21, method=spl("MED,RM,MTM,TRM,DWMTM,DWTRM"))
  plot(y2.dw)
  
  lines(coredata(P))
  
  library(TTR)
  ZLEMA()
  EMA()
  DEMA()
  filter()
  
  library(forecast)
  afit=auto.arima(ts(p))
  plot(forecast(afit,h=20))
  ls(afit$arma)
  ls(afit$model)
  plot(y2.dw)
  
  px=P
  TrainData = data.frame(X=as.numeric(c(1:dim(px)[1])), Y = as.vector(coredata(px)));
  colnames(TrainData) = c("X","Y")
  Price.lo <- ppr(Y ~ X, TrainData, nterms = 2, max.terms = 5, sm.method = c("supsmu", "spline", "gcvspline")[1],bass=99)   #kann auch multivar
  sm=xts(Price.lo$fitted, index(px))
  mchart(merge(px,sm))
  
  foreC = xts(predict(Price.lo,data.frame(X = as.numeric(c(1:dim(TrainData)[1])))),index(px))
  plot(foreC,type="l")
  purePlot(foreC,px)
  
  
  mchart(na.omit(merge(price,sm)))
  
  ##################################
  
  
  library(signal)
  #sehr nett und schnell: low-pass-filter auf finanz-daten - mit shift und ohne lag-korrektur
  bf <- butter(3, 0.009)  #2.par: je kleiner desto glatter # 10 Hz low-pass filter
  bf2 <- butter(3, 0.05)  #2.par: je kleiner desto glatter     # 10 Hz 
  px=p["2007::2009-04"];plot(px)
  px=p["2006::2008-01"];plot(px)
  
  slope200=rollRegressionXTS(px,200) 
  smooth200=rollRegressionXTS.smooth(px,100) 
  smooth200=rollRegressionXTS.smooth(px,200) 
  purePlot(merge(px,smooth200,SMA(px,200),ZLEMA(px,200),EMA(px,50)))
  
  x <- coredata(px)
  #x <- coredata(p)
  #sehr spannende Filter (scheinbar online)
  t=1:shape(x)
  y <- filtfilt(bf, x)  #versucht das gefilterte signal ohne lag ...(2.pass)
  z <- signal::filter(bf, x) # apply filter  .. einfach den butter - filter-> lag
  z2 <- signal::filter(bf2, x) # apply filter  .. einfach den butter - filter-> lag
  z3 <- fftfilt(rep(1, 20)/20, x) # apply 10-point averaging filter
  z4 <-filter(cheby1(5, 3, 0.1),x)
  
  plot(t, x,type="l")
  lines(t, z, col="blue") #butter
  lines(t, y, col="green")  #butter-backshifted
  lines(t, z2, col="red") #butter-fein
  lines(t, z3, col="brown") #butter-fein
  lines(t, z4, col="magenta") #butter-fein
  
  
  lines(ZLEMA(x,90),col="green",lwd=2)
  lines(SMA(x,200),col="brown",lwd=2)
  lines(SMA(x,90),col="cyan",lwd=2)
  lines(EMA(x,90),col="yellow",lwd=2)
  
  legend("bottomleft", legend = c("data", "filtfilt", "filter"), 
         pch = 1, col = c("black", "red", "blue"), bty = "n")
  
  
  #tech-cloud
  f1=ZLEMA(p,100)
  f2=SMA(p,200)
  signal1 = sign(f1-f2)
  signal2= sign(rollRegressionXTS(p,50))
  signal=sign(signal1+signal2)
  
  signal[signal==0]=NA; signal=m.ifna.prev(signal)
  plotSigPrice(signal=signal,prices=p,indi=list(f=merge(p,f1,f2)))
  
  ########################
  library(kza)
  #ein schneller - filter kz - sehr ähnlich zu ksmooth() !!
  k.kz=kz(coredata(p),150)
  k.kz.xts <- xts(k.kz,index(p))
  colnames(kz.xts)="kz.xts"
  purePlot(merge(p,k.kz.xts,ksmooth(p,glaettung=5)))
  
  # kza reconstruction of the signal  (break-point-erkennung ??)
  y=coredata(p)
  m=150
  
  #crash  k.kza <- kza(y,m,y=k.kz,min_size=10)
  
  ##############
  #feature - extraktion:
  #gegeben sein ein set fl von n filter-lines.
  #zähle für jeden fl unterhalb von wie vielen der anderen fl er sich befindet
  #-> n -dim -vektor von integer-zahlen zwischen n und 0
  
  Z2=xts(z2,index(f1));Z=xts(z,index(f1))
  f3=SMA(p,90)
  
  fl =na.omit(merge(p,f1,f2,f3,Z2,Z)[-c(1:200)])  #einschwinger abschneiden
  purePlot(fl["1999::2000"])  
  purePlot(fl["2000-01-03::2000-01-05::"])
  
  #codiere:  oberhalb von <x> anderen:
  px=Z2
  sapply(c(1,2,3),function(d) diff(coredata(px),d))
  
  px=na.omit(P)
  plot(px)
  tail(px)
  
  
  
  
  #verschieden Methoden zur Auswertung eines Filters
  View(merge(g.Signal.r(px,5),g.Signal.d(px,5),g.Signal.D(p,px),g.Signal.D.zlema(p,px,10),g.Signal.D.zlema.q(p,px,10,.20)))
  
  feature=
    foreach(base.i=1:ncol(fl),.combine="merge") %do% {
      fl.base=fl[,base.i]
      fl.2 =fl[,-base.i]
      res=xts(rowSums(bt.apply.matrix(fl.2 ,function(col) {res1=sign( fl.base-col);res1=ifelse(res1<0,0,1)})),index(fl))
      colnames(res)=colnames(fl.base)
      res
    }
  
  #wie feature aber ohne sign und ohne ifelse:   also die summe der Unterschiede zu den nachbarn
  feature2=
    foreach(base.i=1:ncol(fl),.combine="merge") %do% {
      fl.base=fl[,base.i]
      fl.2 =fl[,-base.i]
      res=xts(rowSums(bt.apply.matrix(fl.2 ,function(col) {res1=( fl.base-col);})),index(fl))
      colnames(res)=colnames(fl.base)
      res
    }
  
  #Target für die Benchmark sei Summe aller multiTarget
  BENCH=1
  t.bench=
    foreach(target=data$multiTarget,.combine="merge") %do% {
      target[,BENCH]
    }
  
  t.Bench= xts(rowSums(t.bench,na.rm=T),index(t.bench)) #summe aller Multitarget für Bench
  #target-kontrolle:
  plotSigPrice(signal=sign(t.Bench),prices=p,indi=list(f=merge(p,f1,f2)))
  #....................................................
  
  purePlot(fl)
  feature3 =merge(rollRegressionXTS(p,200),rollRegressionXTS(p,90),rollRegressionXTS(fl[,"Z2"],20), na.omit(p-SMA(p,200)),rollRegressionXTS(na.omit(p-SMA(p,200)),90))
  
  colnames(feature3)=spl("slope200,slope90,slopeZ2,faber,faberSlop90")
  ############# 3 Classifier:
  feature4=merge(g.Signal.r(p,5),g.Signal.d(p,5))
  ############################################################################>>>
  #rollierendes learn/timing-system mit randomForest .. 
  #technische cloud mit  highcorr-entfernung und coarse-coding
  ############################################################################>>>
  
  ### Bau ein xts aus merkmals-vektoren (cloud)
  train.data= na.omit(merge(feature2,feature3,feature4))
  train.data=na.omit(feature3)
  dim(train.data);fromToS(train.data)
  #ergänze um leads 
  if (F)
  {
    feature.leads= na.omit(HotLags2.CC(p=p, na.omit(train.data),visual=T))
    dim(feature.leads);fromToS(feature.leads)#  warum kostet mich das ein ganzes Jahr vorn ?
    #entferne aber gleich die nun evtl. korrelierten leads
    descrCorr <- cor(na.omit(feature.leads))# die eingentlich - trainings-daten
    highCorr <- findCorrelation(descrCorr, 0.90); print(highCorr)
    if (len(highCorr)>0)feature.leads =feature.leads[,-c(highCorr)];dim(feature.leads)
    dim(feature.leads)
    train.data = merge(train.data,feature.leads);dim(train.data)
  }
  #entferne hoch korrelierte zeitreihen
  descrCorr <- cor(na.omit(train.data))# die eingentlich - trainings-daten
  highCorr <- findCorrelation(descrCorr, 0.95); print(highCorr)
  if (len(highCorr)>0)train.data =train.data[,-c(highCorr)];dim(train.data)
  #coarse-coding
  train.data = bt.apply.matrix(na.omit(train.data), function(col) coarse.code(col,b=100))
  #das Target vorne dazu
  train.data = merge(t.Bench,train.data) 
  #bereinigen
  if (sum(apply(train.data,2,FUN=function(col) len(col[is.infinite(col)]))) >0)
    no=foreach(col.i = 1:ncol(train.data)) %do%  { train.data[is.infinite(train.data[,col.i]),col.i]<-0 }
  dim(train.data);fromToS(train.data);colnames(train.data);
  mPlots(train.data)
  ####
  save(train.data,file="train.data")
  
  #wähle eine trainings-abschnitt in der cloud .. die letzten 150 Tage bleiben für den test
  #.....................................
  
  today="2009-01-01" #alles ok
  today="2008-01-01" #alles falsch
  today="2008-07-02" #guter check - ab wann er den Wendepunkt erkennt
  today="2009-06-02"
  today =DateS(last(train.data))
  
  #liste aller monats-enden
  today.list = index(m.to.monthly(train.data))
  today.list = index(m.to.weekly(train.data))
  
  #alternativ:  nur einige spannende trainings-tage
  hl=HighLows(prices=merge(p,train.data)[,1],maxdd=60,visual=T)$hl
  hl=hl[-1]
  date.list=c(hl,hl+30,index(last(train.data)),as.Date("2000-08-21"))
  today.list=sort(date.list);print(today.list)
  #.............................................
  today.list=today.list[today.list < index(last(train.data))] #nicht übers ende hinaus
  pred=train.data[,c(1,2)];pred[]=0  #hier wird die aktuellste prognose gesammelt
  
  pred.err=foreach(today = today.list, .combine="rbind") %do%
{
  #  today="2013-11-29"
  today=as.character(today)
  #...................................
  x2=get.Index(train.data,atDate=today);  x1=max(1,x2-2000) #2000
  mP("##################  %s x1..x2:  %d..%d ############",as.character(today),x1,x2)
  
  #plot(p);lines( p,col="black",lwd=2)
  #amark(DateS(train.data[x2]),col="blue");amark(DateS(train.data[x1]),col="blue")
  
  #Trainiere den Cloud-Abschnitt
  crs=new.env();dont_use_last<<-1  
  w=classifier.randomForest.fit(crs,train.data[x1:x2], dimTarget=1,ntree=500)   #nach 
  #einen prediction-xts bauen
  
  if ((x2+1) < shape(train.data))
  { #prognose bis ganz nach rechts
    newData=na.omit(train.data[(x2+1):shape(train.data),c(crs$input)])
    
    pred.new=try(classifier.randomForest.predict(crs,newData=newData))
    if(!inherits(pred.new,"try-error"))
      #überschreiben alte Prognosen mit neueren prognosen
      i=index(pred[index(pred.new)]);  pred[i,c(1,2)]=pred.new[i,c(1,2)]
  }
  #w[,"pred.err"]
  w
}
#######################################################################################################################################
#berechen rollierend ksmooth oder auch andere nicht causale indikatoren - baue danach ein system mit ihnen
#MMSWINGBUG
#######################################################################################################################################
Res=NULL
for(today in as.character(today.list))
{
  #  today="2013-11-29"
  # browser()
  #...................................
  x2=get.Index(train.data,atDate=today)
  #last(train.data["::2002-12-28"])
  x1=max(1,x2-2000) #2000
  if (x2 -x1 >= 2000)
  {
    mP("##################  %s x1..x2:  %d..%d ############",today,x1,x2)
    
    #plot(p);lines( p,col="black",lwd=2)
    #amark(DateS(train.data[x2]),col="blue");amark(DateS(train.data[x1]),col="blue")
    
    px=p[x1:x2]
    sm1=ksmooth(px,glaettung=5)
    sm2=SMA(px,n=200)
    #purePlot(merge(px,sm1,sm2))
    sm1.d=g.Signal.d(sm1,3)
    sm2.d=g.Signal.d(sm2,3)
    sm2.d[abs(sm2.d)<10] <- 0
    #  sm2.d = m.ifna.prev(sm2.d)
    
    sm3.d=g.Signal.D(sm1,sm2)
    if (as.Date(today)>=as.Date("2012-05-27"))
      mPlots(merge(px,sm1,sm2), merge(sm1.d,sm2.d,0),merge(sm3.d,0))
    
    # plotSigPrice(sig=sign(sm3.d),prices=p[index(sm3.d)])
    fut =sprintf("%s::%s",last.day,today)#, as.character( min(index(last(p)),as.Date(today)+30)))
    neu= merge(px,sm1,sm2,sm1.d,sm2.d,sm3.d)[fut]
    Res=iif(is.null(Res), neu, rbind(Res,neu))
  }
  last.day=as.Date(today)+1
}
#dynSMA
mPlots(merge(Res[,1],Res[,2],Res[,3]),merge(Res[,4],Res[,5],0),merge(Res[,6],0))
sig=sign(Res[,6])
sig[sig < 0]<-0
View(tail(sig,500))
b=na.omit(merge(sig,px))
plotSigPrice(signal=b[,1],prices=b[,2])

#pred enthält nun die forecasts pred, pred.err die forecast-fehler der traininsläufe
print(pred.err);print("range");range(pred.err)
mP("mean pred.err %f median %f", mean(pred.err),median(pred.err))
ist.soll=na.omit(merge(pred[,1],train.data[, 1],pred[,2]))
comp=apply(ist.soll,1,FUN=function(r)ifelse(r[1]==r[2],0,1));pred.err.2=sum(comp)/length(comp)*100
mP("num %d, model.err 0 %f  ,   pred.err.2 = %f",len(comp),crs$model.err,pred.err.2)

sig=pred[,1];plotSigPrice(signal=sig,prices=p[index(sig)])
#check.classifier.1(crs,crs$rf,1)
#die letzten 150 Werte wurden nicht trainiert !
set.id=1;  check.set=list(crs$test,crs$validate,crs$train)[[set.id]]
chck=crs$dataset[check.set, c(crs$target)]; t1=DateS(chck[1]); t2=DateS(last(chck,1));amark(t1,col="green");amark(t2,col="green")
#wichtigkeit der Variablen
W=w[,-c(1,2)]
t(W)[order(t(W),decreasing=F),]

#teste einen anderen sub-set der train.data:

pred=classifier.randomForest.predict(crs,newData=na.omit(crs$dataset[check.set, c(crs$target,crs$input )]))
#pred auf dem check.set und vergleich mit target
View(merge(pred, crs$dataset[check.set, c(crs$target)]))

sig=classifier.randomForest.predict(crs,newData=na.omit(train.data[, c(crs$target,crs$input )]))[,1]
plotSigPrice(signal=sig,prices=p[index(sig)])
#.................................

highCorr <- findCorrelation(descrCorr, 0.90)


################################################################################################################################
#es gibt sehr einfache systeme (wie faber) die super laufen - aber sobald man die parameter leicht verändert bricht alles zusammen
#wie krieg ich ein autotuning hin - dass sicherstellt, dass die systeme nicht irgendwann völlig abschmieren ?
################################################################################################################################


if (F)
{
  colnames(data$prices)
  sym="EXX50_RI" #"BUND_FUTURE"

#jeden monat
#würfel eine liste von N.all systemen .. diese laufen über alle systeme
#permutiere die parameter (um die arbeitspunkte der aktuellen topN.now-Systeme)
#die N.all-liste enthält die bishereigen topN.now besten Systeme (aller symbole)und die besten neu gewürfelten

#jeden monat, pro symbol:  wähle topN.now (=10) der besten systeme (gemessen an  GuV im Zeitfenster)
#setze einen monat - lang die signal der topN.now - Systeme um  (das glättet den Return -weil hier 10 systeme im Einsatz sind)


  te= lapply(colnames(prices),function(sym)
  {
    dax=prices[,sym]
    #geil:
    #x=any.smoothing(dax,glaettung = 200,dw=5,visual=T,q.w ="1%",onlyLong=F)
    x=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="EMA",fn2="SMA",glaettung2=40,onlyLong=F,q.w="1%")
    x=any.smoothing(dax,glaettung = 100,dw=1,visual=T,fn="ZLEMA",fn2="SMA",glaettung2=200,onlyLong=F,q.w="1%")
  x$ret
  }  )
  

#Kann ich aus einer auswertung As(t) erkennen, dass ich in Zukunft  mit System  1 bzw 2 arbeiten sollte ?  (kann ich aus einer guten Leistung eines Parametersatze für die Vergangenheit folgern, dass dieser auch in Zukunft funktioniert?)


#liste aller monats-enden
today.list = as.character(index(m.to.monthly(train.data)))

pred.err=foreach(today = today.list, .combine="rbind") %do%
{
  #  today="2013-11-29"
  today=as.character(today);x2=get.Index(train.data,atDate=today);  x1=max(1,x2-2000) #2000
  mP("##################  %s x1..x2:  %d..%d ############",as.character(today),x1,x2)
  
  S1=any.smoothing(dax,glaettung = 200,dw=1,visual=T,fn="EMA",fn2="SMA",glaettung2=40,onlyLong=F,q.w="1%")
  S2=any.smoothing(dax,glaettung = 100,dw=1,visual=T,fn="ZLEMA",fn2="SMA",glaettung2=200,onlyLong=F,q.w="1%")
  
}

#.............................

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








#.................................  
library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
crs$j48 <- J48(as.factor(crs$Target) ~ .,    data=crs$tdata)
summary(crs$j48)
check.classifier(crs,crs$j48)
#................................
library(RWeka) #http://www.r-bloggers.com/r-talks-to-weka-about-data-mining/
crs$m5p <- M5P(as.factor(crs$Target) ~ .,    data=crs$tdata)
summary(crs$m5p)
check.classifier(crs,crs$m5p)
#................................
library(rpart)  
#http://www.statmethods.net/advstats/cart.html
crs$rpart <- rpart(as.factor(crs$Target) ~ .,
                   data=crs$tdata,
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))
check.classifier(crs,crs$rpart)
# prune the tree
fit=crs$rpart
crs$rpart<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
check.classifier(crs,crs$rpart)  #prune hat nur wenig geholfen

library(rpart.plot) #http://www.milbo.org/rpart-plot/
prp(crs$rpart)
# Eine Textansicht des Modells Entscheidungsstruktur erstellen
summary(crs$rpart)


#................................
mchart(feature2)
lines(p)
feature["2000-01-03"]
View(feature["2000-01-03::2000-01-05::"])

#tune nun den glaettungs-parameter so, das signal aus dem filter optimal zum target passt
#dazu die signal-train- methode so anpassen, dass sie nicht immer die system-guv optimiert- sonder die differenz zum target minimiert ...

}
################################################################################
if (F)
{
  ls(data$crs$tdata)
  
}
##############################################################################################
mP("########### load labor_signal.r")
if (F)
  list_R_functions('MLib/labor_signal.r')

