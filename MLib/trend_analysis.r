
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
options(error = browser)

find.trend<-function()
{
  ####### Trend-Erkennung
  # wenn die steigung des Trends flach ist, passt ein längerer sma besser, also ein kurzer  
  dax=na.omit(data$prices[,"DAX"])
  wlen=100
  
  #x=find.last.trend.segment(na.omit(dax),visual=T,glaettung=4)
  #normiere den y-Bereich auf ein Intervall von integer 0..100
  
  
  dax.i=na.omit(coarse.code(dax,b=100))
  
  #wie weit muss ich das Fenster vom rechten Rand aus nach links verlängern, damit
  #ich eine target.range im preis erhalte -> steile kurse führen zu schmalen fenstern
  
  
  #...................................... wlen by range ......................
  #wähle als constante wRange - und wähle wlen dynamisch, so, dass innerhalb von wlen
  #der preis wRange einnimmt -> steile kurseflanken sind weniger breit wie fast horizonta
  # plot(ROC((na.omit(to.weekly(dax)[,1]))))
  wLens=rollapply(dax.i,FUN=find.wlen,width=wlen*3,target.range=10) #wird mit target.range
  plot(dax.i,main="wLens über range.wlen und Dax")
  lines(scaleTo(wLens,range(dax.i,na.rm=T)),col="green")
  range(wLens,na.rm=T)
  #..................................  wLens mal über slope steuern
  mSlope <- rollRegressionXTS(na.omit(mNorm(dax)),win=200)*1000
  plot(mSlope)
  wLens=EMA((max(mSlope,na.rm=T)-abs(mSlope))*30 +100, 30)
  plot(dax.i,main="wLens über Slope200 und Dax")
  lines(scaleTo(wLens,range(dax.i,na.rm=T)),col="green")
  #..................................  wLens mal über slope steuern
  hl= HighLows(dax,2,visual=F)
  plot(dax)
  wlen=50
  points(dax[hl$highs],col="red",lwd=1)
  points(dax_[hl$lows],col="blue",lwd=1)
  dim(dax[hl$highs])
  wLens=rollapply(dax.i[hl$highs],FUN=find.wlen,width=wlen,target.range=7) #wird mit target.range
  mPlots(dax,wLens)
  
  #bad exerperiment  >>>
  plot(dax.i,main="wLens über range.wlen und Dax")
  lines(scaleTo(wLens,range(dax.i,na.rm=T)),col="green")
  range(wLens,na.rm=T)
  wLens[is.na(wLens)]<-0
  peaks <- findpeaks(as.numeric(wLens) ) 
  #price, x-lage des peaks,
  #Peaks <-merge( zz[peaks[,2]],zz[peaks[,3]],zz[peaks[,4]]) 
  flat.Ends=wLens[peaks[,2]]
  
  quantile.range <- quantile(flat.Ends, probs = seq(0, 1, 0.01),na.rm=F)
  thresh=quantile.range["85%"]
  flat.ends = flat.Ends[flat.Ends >= thresh] #das range-Fenster ist wenigsten 9 Tage breit
  plot(flat.Ends)
  points(flat.ends,col="red")
  lines(scaleTo(dax.i,range(flat.ends)),col="blue")
  points(scaleTo(flat.ends,range(dax.i,na.rm=T)),col="red")
  
  dim(flat.ends)
  amark(flat.ends)
  
  
  #über die wichtigsten no-trend - verdächtigen (die intervalle die die längste Länge haben müssen um die target.range aufzuweisen)
  p=dax.i
  flat.ends
  Flat.ends=as.matrix(flat.ends)
  seg=  
    #foreach(yt = rownames(Flat.ends), .combine = "rbind") %do% {
    #foreach(yt = as.Date(index(m.to.monthly(p))) , .combine = "rbind") %do% {
    for(yt in as.Date(index(m.to.monthly(p)))) {
      
      #yt ist das Datum des rechten intervall-endes
      # find.best.series(data, visual=T, min.len=100,sym="DAX",date.last=yt,glaettung=5,saveDir="Plots/BEST")   
      #mache dort eine lineare-reg - auch im 90%-Intervall
      #   t=Flat.ends[yt,]
      # t=first(flat.ends)
      
      p.t = p[as.Date(index(p))<= as.Date(yt) ] 
      t= find.wlen(last(p.t,120),target.range=20)
      
      
      #p.t = datum des rechten intervallsendes und intervallbreite
      Y=last(na.omit(p.t), max(32,as.numeric(t)))
      #Beschränkung auf Max-Peaks 
      #peaks <- findpeaks(as.numeric(Y) );  Y=Y[peaks[,2]]
      
      Y.fitted=Y  #init
      
      slope200=lm.FITm(Y) #lin modell
      pred1<-predict(slope200,newdata=data.frame(c(1:shape(Y)),as.numeric(Y)),interval="prediction",level=0.90)
      YP=merge(Y,Y,Y)
      YP[]=pred1
      Yf=coredata(YP[,1])#slope200$fitted.values
      rag=last(YP);rg1=round(rag[,3]-rag[,2]); rg=round(rg1/len(YP)*100) #Kanal-Breite
      #browser()
      #rg=diff(log(rag))
      m=-round((Yf[1]-last(Yf))/len(Yf)*100)#Yf[1]*100
      y2=max(range(YP,na.rm=T))
      y1=y2-20
      main=main=sprintf("%s len %d range %d m %d  G %f",yt,len(Y), rg,m,m/rg)
      plot(YP[,1],ylim=range(YP,na.rm=T),main=main) #compute.max.drawdown(mROC(Y))  fast immer 1  #ylim=c(y1,y2+5),
      #  purePlot(YP)
      lines(YP[,2],col="blue");lines(YP[,3],col="red")
      lines(Y);points(Y,col="magenta")
      
      print(yt)
      print(main)
      plot(dax.i,main=main)
      amark(first(Y));amark(yt,col="blue")
      lines(YP[,2],col="blue");lines(YP[,3],col="red")
      
      #weighted lm - gibt mehr Gewicht auf die jüngsten Werte
      slope200w=lm.FITmw(Y) #lin modell
      YW=Y;  YW[]=slope200w$fitted.values
      lines(YW,col="green")
      
      #browser() #"next",warte=T)
      
      #SEG
      
      if (F)  #fragment
      {
        find.best.series(data, visual=T, min.len=100)
        find.last.trend.segment(mdax,visual=T,glaettung=5)
        
        krisis=mdax[mdax==max(mdax["::2010"])]
        amark(krisis,col="blue")
        find.best.series(data, visual=T, min.len=100,sym="MDAX",date.last=DateS(krisis),glaettung=5)
        
        
        Y.fitted[]=slope200$fitted.values  #die steigungs-gerade
        mchart(merge(Y,Y.fitted))  #original- und gerade
        Y.rot=(Y-Y.fitted)
        plot(Y.rot)
        highs <- findpeaks(as.numeric(Y.rot) )[,2] 
        points(Y.rot[highs],col="blue",lwd=1)
        
        lows <- findpeaks(-as.numeric(Y.rot) )[,2] 
        points(Y.rot[lows],col="red",lwd=1)
      }
      #find.support.resistance(Y.rot)
      
    }
  
  find.best.series(data, visual=T, min.len=100,sym="DAX",glaettung=5)
  #<<--bad exerperiment  >>>
  
  #erstell mal find.best.series (incls. xls und png-file erstellung) für jedes monatsende
  seg=  
    foreach(yt = as.Date(index(m.to.monthly(dax))) , .combine = "rbind") %do% {
      #yt ist das Datum des rechten intervall-endes
      find.best.series(data, visual=T, min.len=100,max.len= 1000,sym="DAX",date.last=yt,glaettung=5,saveDir="Plots/BEST")
      
    }
  
  #.......................................................................
  #wLen enthält für jeden Tag eine andere Fensterlänge
  #Berechne mit diesen Fensterlänge einen moving-average
  SMA.i_<-function(i,p,wLens,fac=1)
  {
    wlen=as.numeric(wLens[i])
    if (is.na(wlen))
      return(NA)
    # browser()
    P=p[max(1,(i-wlen*fac)):i]
    mP("mean %s wlen: %d, len.P %d",DateS(last(P)), wlen, len(P))
    mean(P)
    #last(SMA(P,n=min(len(P)-1,as.integer(wlen*fac))))  
  }
  SMA.i<-cmpfun(SMA.i_)
  
  maxi.i_<-function(i,p,wLens,fac=1)
  {
    wlen=as.numeric(wLens[i])
    if (is.na(wlen))
      return(NA)
    # browser()
    P=p[max(1,(i-wlen*fac)):i]
    mP("max %s wlen: %d, len.P %d",DateS(last(P)), wlen, len(P))
    max(P,na.rm=T)
    #last(SMA(P,n=min(len(P)-1,as.integer(wlen*fac))))  
  }
  max.i<-cmpfun(maxi.i_)
  min.i_<-function(i,p,wLens,fac=1)
  {
    wlen=as.numeric(wLens[i])
    if (is.na(wlen))
      return(NA)
    # browser()
    P=p[max(1,(i-wlen*fac)):i]
    mP("min %s wlen: %d, len.P %d",DateS(last(P)), wlen, len(P))
    min(P, na.rm=T)
    #last(SMA(P,n=min(len(P)-1,as.integer(wlen*fac))))  
  }
  min.i<-cmpfun(min.i_)
  
  
  
  
  #scaliere die wLens auf Faber-Nieveau (200..300)
  range(wLens,na.rm=T)
  wLens[is.na(wLens)]<-1
  wLens.scaled =100+coarse.code(wLens,b=300)
  wLens.scaled = as.integer(wLens)
  range(wLens.scaled)
  plot(wLens)
  #rollapply(dax.i,FUN=SMA.i,width=wlen*3,wLens=wLens)
  sma.dyn=dax
  sma.dyn[] =sapply(1:len(dax.i), FUN=SMA.i,p=dax, wLens=wLens.scaled,fac=2) 
  
  min.dyn=dax
  min.dyn[] =sapply(1:len(dax.i), FUN=min.i,p=dax.i, wLens=wLens+3,fac=2) 
  
  max.dyn=dax
  max.dyn[] =sapply(1:len(dax.i), FUN=max.i,p=dax.i, wLens=wLens+3,fac=2) 
  range(max.dyn,na.rm=T)
  
  #lines(scaleTo(min.dyn,range(wLens,na.rm=T)),col="red")
  #lines(scaleTo(max.dyn,range(wLens,na.rm=T)),col="green")
  
  dax1=dax["::2003"]; top1=dax1[which(dax1==max(dax1))]
  amark(top1)
  
  View(wLens["2000-03-7::2001-03-01"])
  View(max.dyn["2000-03-7::2001-03-01"])
  lines(min.dyn,col="red")
  lines(max.dyn,col="blue")
  
  plot(sma.dyn)
  
  plot(dax);lines(dax,lwd=2)
  lines(sma.dyn,col="red")
  
  max(wLens,na.rm=T)
  rm= runMax(dax.i,n=wlen);  #sollte evtl. besser durch.  max.dyn  ersetzt werden
  rmi=runMin(dax.i,n=wlen);  #min.dyn
  rm=max.dyn;  rmi= min.dyn
  
  plot(dax.i); 
  lines(rm,col="red")  
  lines(rmi,col="blue")
  
  drm=mROC(rm)
  drm=drm[drm>0]
  plot(drm)
  #offset
  amark(DateS(dax[wlen]),col="blue")
  
  quantile.range <- quantile(abs(drm), probs = seq(0, 1, 0.01),na.rm=F)
  thresh=quantile.range["75%"]
  upTrend=as.Date(index(drm[drm>thresh]) ) 
  
  
  plot(dax.i)
  lines(rmi,col="green")
  
  drm=mROC(rmi)
  plot(drm)
  drm=drm[drm < 0]
  #offset
  amark(DateS(dax[200]),col="blue")
  
  quantile.range <- quantile(abs(drm), probs = seq(0, 1, 0.01),na.rm=F)
  thresh=quantile.range["75%"]
  downTrend=as.Date(index(drm[drm<thresh]) ) 
  
  plot(dax.i)  
  amark(downTrend,col="lightblue") 
  amark(upTrend) 
  lines(dax.i)
  lines(rm,col="red")  
  lines(rmi,col="blue")
  lines(scaleTo(wLens,range(dax.i,na.rm=T)),col="green")
  
  
  
}
##############################################################################
##############################################################################

######################################################################################
#schau Dir mit Hilfe von Trendkanälen und SMA200 und innereFunktion die Zeitreihe
#im gleitenden Fenster an. 
#Erkenne a) horizontal-Bereiche (wenn man keinen pos wechsel machen soll)
######################################################################################

trend.analysis<-function(p,visual=T)
{
  if (T)  #analysier blos die stellen mit faber-signalen
  {
    sig = signal.Faber.base(arg=list(clos=p), par = list( sma.w=200))$Signal
    plotSigPrice(signal=sig,p)
    
    sig.d=sign(sig-lag(sig,1))
    sig.d[sig.d==0]<-NA; sig.d = na.omit(sig.d)
    mP("%d Trades",len(sig.d) )
    
  }
  lastSeg="1988-01-01"
  noTrend =p[,1]
  noTrend[]=NA
  train.data.list = list()
  # View(sig.d)
  
  mont= m.to.monthly(p)
  montd = index(mont)
  seg=  
    #foreach(yt = rownames(Flat.ends), .combine = "rbind") %do% {
    #   foreach(yt = as.Date(index(m.to.monthly(p))) , .combine = "rbind",.errorhandling='stop') %do% {
    foreach(yt = as.Date(index(m.to.daily(p))) , .combine = "rbind",.errorhandling='stop') %do% {
      
      #for(yt in montd ){
      
      #  foreach(yt = as.Date(index(sig.d)) , .combine = "rbind",.errorhandling='remove') %do% {
      #yt ist das Datum des rechten intervall-endes
      #yt = "2008-12-31"
      #yt = "2011-06-30"
      
      #alles vor yt
      print("##############################")
      #browser()
      print(yt)
      RES=NA
      
      p.t = p[as.Date(index(p))<= as.Date(yt) ] #alles links von yt
      #finde den anfang des last.trend-fensters
      print(shape(p.t))
      if (shape(p.t)>500 )#&& as.Date(yt)>as.Date("2013-09-30"))
      {
        p.r=last(na.omit(p.t),1000)  #rollierendes Fenster
        
        #   browser()
        #      dim(p.r)
        
        #wo fängt er long-term-trend an ?
        glaettung=10
        if (shape(p.t)>500)
          glaettung=9
        if (shape(p.t)>1000)
          glaettung=7
        
        glaettung=3
        # browser(mP("lllll"))
        last.trend=  find.last.trend.segment(na.omit(p.r),visual=T,glaettung=glaettung)  
        
        x.lt=last.trend$x2
        mP("last %s now %s",lastSeg,x.lt)
        
        if (as.Date(x.lt) >as.Date(lastSeg))
        {
          find.last.trend.segment(na.omit(p.r),visual=T,glaettung=glaettung) 
          mP("last %s now %s",lastSeg,x.lt)
          
          lastSeg=x.lt
          #browser()    
        }
        #if (DateS(last(na.omit(p.r)))=="2013-06-25")
        #{print("~~~~~~~~~~~~~~~~~~~~~");browser()}
        
        #    amark(x.lt)
        Y.lt=na.omit(p.r[sprintf("%s::%s",x.lt,yt)])
        
        if (shape(Y.lt) <100)
          Y.lt= last(na.omit(p.r),100)
        
        #LongTerm-Trendkanal  
        slope.lt=lm.FITm(Y.lt,visual=F) #lin modell
        #  lines(Y.lt,col="red")
        #  plot(Y.lt)
        YP.lt=m.predict(slope.lt,firstDate=DateS(first(Y.lt)),n=shape(Y.lt)+1000,visual=F)
        #mP("#####################################")
        #browser()
        #      mchart(YP.lt)
        #      str(coredata(Y.lt[,1]))
        #      yx=Y.lt
        #      yx[]=as.numeric(Y.lt)
        
        #      lines(Y.lt,col="red")
        
        #    amark(yt)
        #     mP("#############") 
        #    browser()  
        #finde den anfang des target.range- Fensters
        t= try(find.wlen(last(p.r,120),target.range=20))
        
        Y=last(na.omit(p.r), max(32,as.numeric(t)))
        #Beschränkung auf Max-Peaks 
        #peaks <- findpeaks(as.numeric(Y) );  Y=Y[peaks[,2]]
        
        #trend im target.range-fenster
        slope200=lm.FITm(Y) #lin modell
        YP=m.predict(slope200,firstDate=DateS(first(Y)),n=shape(Y)+132)
        #mchart(merge(Y,YP))
        Yf=coredata(YP[,1])#slope200$fitted.values
        rag=last(YP[YP[,3]!=0]);rg1=round(rag[,3]-rag[,2]); rg=rg1/len(YP)*100 #Kanal-Breite
        #browser()
        #weighted lm - gibt mehr Gewicht auf die jüngsten Werte
        slope.w=lm.FITmw(Y) #lin modell
        YW=Y;  YW[]=slope.w$fitted.values
        
        m=round(coef(slope200)[2]*100,1)
        
        #m=-round((Yf[1]-last(Yf))/len(Yf)*100)#Yf[1]*100
        y2=max(range(YP,na.rm=T))
        y1=y2-20
        m.lt = round(coef(slope.lt)[2]*100,1)
        m.w = round(coef(slope.w)[2]*100,1)
        
        if (rg ==0) G=m
        else
          G=round(as.numeric(m/rg),2)
        
        main=sprintf("date='%s', len=%d, range=%.1f,  G=%.2f, m=%.1f, m.lt=%.1f, m.w=%.1f",yt,len(Y), rg,G,m,m.lt,m.w)
        RES=main
        #sichern der bisherigen Feature für einen späteren Trainings-Lauf
        train.data.list = append(train.data.list,main)
        
        #################### plotting 
        
        print(yt);print(main)  
        
        if (visual)
        {
          #micro-Plot
          if (F)
          {
            plot(YP[,1],ylim=range(YP,na.rm=T),main=main) #compute.max.drawdown(mROC(Y))  fast immer 1  #ylim=c(y1,y2+5),
            #  purePlot(YP)
            lines(YP.lt[,2],col="orange",lwd=2);lines(YP.lt[,3],col="orange",lwd=2)
            amark(DateS(first(YP)),col="orange")
            
            amark(x.lt,col="brown")
            lines(YP[,2],col="blue");lines(YP[,3],col="red")
            #plot(Y)
            lines(Y);points(Y,col="magenta")
            lines(YW,col="green")
            amark(yt,col="blue")
          }
          #übersichtsplot- dax - gesamt
          plot(p,main=main)
          lines(YP.lt[,2],col="orange",lwd=2);lines(YP.lt[,3],col="orange",lwd=2)
          amark(x.lt,col="brown")
          
          amark(first(Y));amark(yt,col="blue")
          lines(YP[,2],col="blue",lwd=2);lines(YP[,3],col="red",lwd=2)
          lines(YW,col="green",lwd=2)
          lines(SMA(p.t,200),col="brown")
          ###########Mid-RangePlot########################
          dax.win=na.omit(last(p.r,100))
          dwf= fromToS(dax.win)
          ylim =range(merge(dax.win,YP.lt[dwf,2],YP.lt[dwf,3],YP[dwf,2],YP[dwf,3],YW[dwf]),na.rm=T)
          plot(dax.win,main=main,ylim=ylim)
          lines(YP.lt[,2],col="orange",lwd=2);lines(YP.lt[,3],col="orange",lwd=2)
          amark(x.lt,col="brown")
          
          amark(first(Y));amark(yt,col="blue")
          lines(YP[,2],col="blue",lwd=2);lines(YP[,3],col="red",lwd=2)
          lines(YW,col="green",lwd=2)
          lines(SMA(p.t,200),col="brown")
          
          # clr()
        } 
        #find.support.resistance(Y.rot)
        ##################### sonder-events: 
        use.classifier<- (len(sig.d[yt])>0)
        if (use.classifier)
        {
          #nimm Dir alle Trainingsdaten die bis jetzt da sind und bau daraus eine Trainingsdaten-Tabelle
          mP("########### Retrain ##############")
          
          
          train.data.table=data.table(rbindlist(lapply(train.data.list, FUN=function(x)eval(parse(text=sprintf("list(%s)",x))))))
          print(train.data.table)
        }
        
        #if ((abs(G)<1  && abs(m) <100)|| 
        s=0.1
        if (visual)
          if (abs(m) < s*1 ||abs(m.lt) < s*1 )#|| abs(m.w) < s*1) #noTrend
          {
            noTrend.frame=sprintf("%s::%s",DateS(first(YP[,2])),yt);
            noTrend[noTrend.frame] = 1
            mP("noTrend %s  ~~~~~~~~~~~~~~~~~~",noTrend.frame)
            
            
            browser() 
          }
        
        if (visual)
        {
          print("############### warte")
          #browser() #"next",warte=T)
        }
      }
      return(RES)
    }
  noTrend_<<-noTrend
  seg_<<-seg
  
  plot(dax)
  noTrend_=na.omit(noTrend_)
  len(noTrend_) 
  amark(as.Date(index(noTrend_)))
  lines(dax,col="black")
  
  seg=seg_
  seg=na.omit(seg)
  #cooler trick um aus einer liste von strings mit "x=33,y=44" ... eine entsprechende Tabelle zu machen
  res.l=lapply(seg, FUN=function(x)eval(parse(text=sprintf("list(%s)",x))))
  res.l=data.table(rbindlist(res.l))
}
if (F)
  trend.analysis(p,visual=T)





#mod=lm.FITm(head(p,100))
#pre=m.predict(mod,"2012-02-02",1000,interval="prediction")
#purePlot(pre)
#dim(pre)


#################################################################################

#################################################################################
#suche wie weit sich der runMax - Wert nach links fortsetzt
#dazu gehe vom aktuellen max in max.range 
#################################################################################
find.no.trend<-function(p, max.range=1,d.tol.pct=3,visual =F,dax=NULL)
{
  min.trend.check.len=5
  
  nowT =DateS(last(p))
  mP("++++++++ %s",nowT)
  l=len(p)
  res =l
  last.p= nval(last(p))
  r.max=runMax(p,n=(max.range))
  last.max = tail(r.max,1)
  d.tol = nval(last.max/100*d.tol.pct)
  up.max=nval(last.max+d.tol)
  low.max= nval(last.max-d.tol)
  #last.max=max(p[(l-max.range):l],na.rm=T)
  
  brk=p[r.max < low.max | r.max > up.max ]
  res = last(brk)
  
  #if (as.Date(nowT)>= as.Date("2013-06-07"))
  #   { visual=T;browser(mP("#b1"))}
  span.frame=""
 #browser(mP("#BB1")) 
  if (last.p< low.max || last.p > up.max)  #wenn schon der aktuelle Wert ausserhalb der range liegt (kann nur bei größeren max.range werten passieren)
  {
    r.max=runMax(p,n=(1))
    last.max = tail(r.max,1)
    d.tol = nval(last.max/100*d.tol.pct)
    up.max=nval(last.max+d.tol)
    low.max= nval(last.max-d.tol)
    #last.max=max(p[(l-max.range):l],na.rm=T)
    
    brk=p[r.max < low.max | r.max > up.max ]
    res = last(brk)
    
  }
  
  if (len(res)==0)
    res = first(tail(p,max.range))#p[len(p)-max.range]
  #wie breit ist no-trend-intervall ??
  M=0
  res.span = shape(p)-get.Index(p,DateS(last(res)))+1
  span.frame = sprintf("%s::%s",DateS(last(res)),nowT)
  #as.integer(last(index(p))-last(index(res)))
  if (res.span < 2)
    M=sign(mROC(tail(p,2)))
  
  #jetzt weiss ich, dass innerhalb diese Intervalles alles Werte  +-d.tol.pct um das letzte max liegen - aber es könnte sich bei diesen Werten duchaus um ein flache gerade handeln.
  # if (nowT =="2003-07-03")
  #  browser(mP("#1"))
  no.trend=F
  
  if (res.span >= min.trend.check.len)
  {
    mP("check for trend at span-window")
    if (is.null(dax))
      fit = lm.fit(tail(p,res.span) ,visual=visual)
    else
      fit = lm.fit(dax[span.frame] ,visual=visual)
    if (len(fit) && !is.na(fit$p.value) &&  !is.na(fit$r.squared) && !is.na(fit$m))
    {
      #no.trend ist wenn: a) keine gute Gerade - also p.value > 0.1 oder r.squared < 0.3 
      #      oder b) doch ein gute Gerade aber abs(m) < 0.1 (fast horiozontal)    
      if (fit$p.value > 0.01 || (fit$r.squared < 0.25 && abs(fit$m) < 3))
        no.trend=T   #no.trend
      else  #läßt sich als gerade darstellen - ist die evtl. flach
        if (abs(fit$m) < 1)
          no.trend =T
    }
    if (!no.trend) #wenn no.trend ist es ein no.trend-stück
    {
      res = p[max.range]
      if (visual)
      {mP("there is a trend at observed span %d m:%f",res.span,fit$m)
       # browser(mP("#3"))    
      }
      M = fit$m
      
    }
    else
    {
      mP("NO trend at span %d m:%.3f",res.span,fit$m)
      #browser(mP("#2"))
      M=0
      no.trend=T
    }
  }
  else  
  {
    no.trend=F  #res.span zu kurz für trend   
    M=sign(nval(last(p))-nval(last(res)))
  }
  if (visual)
  {
    #übersicht
    plot(na.omit(dax),main=span.frame)
    amark(DateS(last(p)))
    if (res.span > 7) amark(DateS(res))
    
    #detail  
    mchart(merge(p,r.max),main=sprintf("find.no.trend %s span: %d ",DateS(last(p)),res.span))
    lines(p,lwd=2)
    
    amark(DateS(res))
    amark(DateS(p[l-max.range+1]),col="yellow")
    abline(h=nval(last.max),col="brown")
    abline(h=low.max,col="orange")
    abline(h=up.max,col="blue")
    lines(brk,col="green") 
    points(last(p[p==nval(last.max)]))
    if (T||no.trend &&  res.span>min.trend.check.len)
      browser(mP("press key"))
  }
  res = last(p)
  #if (len(M) ==0)
  #  M=sign(mROC(tail(p,2)))
  
  
  M=ifelse(no.trend,1,0)
  #if (is.na(M))
  #  browser(mP("M is na"))
  res[]=M
  
  return(res)
}
#find.no.trend<-cmpfun(find.no.trend_) #compilier das Teil

if (F)
{
  dax = na.omit(Cl(data$DAX)["2000::"])
  
  norm_Win(2)
  wlen=50
  hl= HighLows(dax.i,1,visual=F)
  
  M=0
  M=rollapply(dax.i[hl$highs],FUN=find.no.trend,width=wlen,max.range=10,d.tol.pct=20,visual =F,dax= na.omit(Cl(data$DAX))) #wird mit 
  plota(dax.i[hl$highs])
  plota.lines(dax.i[hl$highs],col=iif(M==1,"red","blue"),lwd=2)
  #sollen stücke unter 5 tage länge überhaupt zugelassen werden ??
  
  
  dax = na.omit(Cl(data$SWISS_MARKET))["2010::2011"]
  plot(dax.i)
  dax.i=na.omit(coarse.code(dax,b=1000))+100
  #dax["2006::2007"]
  M=rollapply(dax.i["2005"],FUN=find.no.trend,width=wlen,max.range=20,d.tol.pct=1,visual =T) #wird mit 
  M[is.na(M)]<-0
  sig = sign(M)
  plotSigPrice(signal=sig,prices=dax)  
  
  plota(dax.i)
  plota.lines(dax.i,col=iif(M==1,"red","blue"),lwd=2)
  amark("2013-06-07")
  
  
  
} 

#########################################################################################


if (F)
  breakChannel(p,maxdd=10,t="2007-05-01::")

#norm_Win(4)
#longIndikator1= longIndikator[HighLows(longIndikator,maxdd=20,visual=T)$lows]
#longIndikator2= longIndikator1[HighLows(longIndikator1,maxdd=20,visual=T)$lows]
#longIndikator3= longIndikator2[HighLows(longIndikator2,maxdd=20,visual=T)$lows]

#mPlot(scaleTo(longIndikator,c(1,0)))

HighLowsOLD<-function(prices, maxdd=5,visual = F)
{
  prices=scaleTo(prices,c(1,0)) #ohne Skalierung auf positive Werte klappt das alles nicht so gut
  
  zz=na.omit(ZigZag(prices,change=maxdd,percent=T))
  sig=as.vector(coredata(sign(zz-lag(zz))))  #segmente positiver und negativer steigung
  enc <-rle(sig)
  enc.endidx <- cumsum(enc$lengths)  #ending indices
  enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(prices))  # starting indices
  if (is.na(enc$values[1]))
    enc$values[1] = -1*enc$values[2]
  #markiere die higss- und lows- des zigzag in unterschiedlichen farben
  #  highx = na.omit(enc.endidx[enc$values > 0 ]) 
  #  highs = prices[highx] 
  zzPoints = prices[enc.startidx]
  cat( "\n HighLows-Segmente: ",len(zzPoints)," von punkten: ",len(prices))
  
  if (visual)
    plot(prices,type="h")
  #points(zzPoints,col="green",lwd=2)
  if (visual)
    lines(zzPoints,type="l",col="green",lwd=2)
  
  
  #markiere die higss- und lows- des zigzag in unterschiedlichen farben
  highx = na.omit(enc.endidx[enc$values > 0 ]) 
  
  lowx = na.omit(enc.endidx[enc$values < 0 ]) 
  lows = prices[lowx]
  
  if (first(lowx) < first(highx) )#|| first(prices)>prices[first(highx)])
  {
    highx = c(1,highx)
  }
  else
    lowx= c(1,lowx)
  
  if (visual)
  {
    points(prices[highx],col="red",lwd=1)
    points(prices[lowx],col="blue",lwd=1)
  }
  return(list(highs = highx,lows = lowx,hl=enc.endidx ))  
}

################################
#Berechne die Parameter eines Channel-Stop-Systems 
#Berechne die historische Güte des StopSystems
#Berechne den aktuellen Stop-Channel

################################

breakChannel<-function(prices=edhec,t="2012", maxdd=10, visual=T)
{
  prices=prices[sprintf("%s",t)]
  prices= prices[isWeekday(time(prices))]
  
  prices=mRendite(mROC(prices))
  
  zz=na.omit(ZigZag(prices,change=maxdd,percent=T))
  
  sig=as.vector(coredata(sign(zz-lag(zz))))  #segmente positiver und negativer steigung
  enc <-rle(sig)
  enc.endidx <- cumsum(enc$lengths)  #ending indices
  enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(prices))  # starting indices
  if (is.na(enc$values[1]))
    enc$values[1] = -1*enc$values[2]
  #markiere die higss- und lows- des zigzag in unterschiedlichen farben
  #  highx = na.omit(enc.endidx[enc$values > 0 ]) 
  #  highs = prices[highx] 
  zzPoints = prices[enc.startidx]
  
  plot(prices)
  #points(zzPoints,col="green",lwd=2)
  lines(zzPoints,type="l",col="red",lwd=2)
  
  
  #markiere die higss- und lows- des zigzag in unterschiedlichen farben
  highx = na.omit(enc.endidx[enc$values > 0 ]) 
  
  lowx = na.omit(enc.endidx[enc$values < 0 ]) 
  lows = prices[lowx]
  
  if (first(lowx) < first(highx) )#|| first(prices)>prices[first(highx)])
  {
    highx = c(1,highx)
  }
  else
    lowx= c(1,lowx)
  
  
  points(prices[highx],col="blue",lwd=2)
  points(prices[lowx],col="magenta",lwd=2)
  
  
  ########################################
  ## summiere alle long-Teil-St?cke der zz-Zerlegung auf ret
  ret=0
  
  alleSegmenteI =enc.endidx
  alleSegmenteI2 = alleSegmenteI
  #über alle Segmente des zigzack
  #berechne für jedes Segment die Streuung um die Regression-Line (mean-Reverter)
  lasti=1
  lastOKSegmentES=0
  segI=0
  #browser()
  
  for(i in alleSegmenteI) #über alle zz-Segmente
  {
    segI = segI+1
    if (i >first(alleSegmenteI))
    { 
      #browser()
      xxRet  =mROC(prices[lasti:i])
      xxPrices =prices[lasti:i,1]
      
      segx1 = lasti;
      segx2 = i
      
      if (visual)
        lines(xxPrices, col="yellow") #segmentabschnitt in gelb
      
      if (F)
      {
        reg=lm(xxPrices~c(lasti:i))    
        regfitted.values=reg$fitted.values
      }
      else#dynamisierte Version der Regression   #MMA 
      {
        if (F) #auf Tagesdaten
        {
          if(i-lasti > 10 )#&& i +10< len(prices))
          {
            regfitted.values = prices[c(lasti:(lasti+10-1))]
            for(i5 in seq(lasti+10:i)) #über alle Einzeltage des Segments
            {
              x5= c(lasti:i5)
              if (len(x5)> 10)
              {
                y5 = prices[x5,1]
                reg5=lm(y5~x5)
                
                regfitted.values = append(regfitted.values,last(reg5$fitted.values))
              }            
              #
              if (F) #TEST breakpoint
                if (as.Date(time(prices[i5]))> as.Date("2009-03-01"))
                {
                  lines(regfitted.values, col="blue")
                  lines(reg5$fitted.values,col="green")
                  browser()
                }            
            }
          }
          else
            regfitted.values = prices[c(lasti:i)]
        }
        else
        {
          HL = HighLows(prices[seq(from=lasti,to=i)],maxdd= maxdd/5)
          cat("--> 1")
          browser()
          
          if(T)
          {
            regfitted.values=prices[lasti,1]
            
            #  ARBEIT  ##################################################
            
            len(prices)
            
            for(i5 in HL$highs)
            {
              dates =  HL$highs[HL$highs<=i5]
              yy= prices[dats]
              
              trainData = data.frame(yy,dats)
              colnames(trainData) <- c( "Y","X")
              X =  seq(from = i5, to=len(prices))
              predictData= data.frame(prices[X],X)
              
              mod <- lm(Y ~ X, data = trainData)
              m=coef(mod)[2]
              plot(yy)
              lines(as.xts(mod$fitted.values),col="red")
              
              pp = as.xts(predict(mod,newdata=predictData))
              mPlot(rbind(as.xts(mod$fitted.values),pp),rbind(yy,prices[X]))
              
              
              
              myFun3 <- function(x) {
                x <- data.frame(x, xt=endpoints(x,"days")[-1])
                mod= lm(Close ~ xt, x)
                return(coef(mod)[2])
              }
              
              test1 <- rollapplyr(dax["2011::"], width=20, FUN=myFun2, by.column=FALSE)
              if (F) #TEST breakpoint
                if (as.Date(time(prices[i5]))> as.Date("2009-03-01"))
                {
                  lines(regfitted.values, col="blue")
                  lines(reg5$fitted.values,col="green")
                  browser()
                }            
            }
          }
          else
            regfitted.values = prices[c(lasti:i)]
        }
      }
      if (F && visual)
        lines(regfitted.values, col="green")
      
      ddd = (max(xxPrices)-min(xxPrices))/2
      if (F && visual)
        lines(xxPrices-regfitted.values+max(xxPrices)-ddd,col="grey")
    }
    lasti=i
  }
  
  
  return (as.numeric(ret))
  
}  


if(F)
{
  norm_Win()
  prices = mNorm(dax)
  colnames(prices)=c("Close")
  maxdd= 6
  head(prices)  
  
  HL = HighLows(prices,maxdd= maxdd)
  
  work1 <- function(y) {
    xt=endpoints(y,"days")[-1]
    x1 = first(xt)
    x2 = last(xt)
    xt=HL$highs[HL$highs >=x1 & HL$highs <=x2]  
    # browser()
    if (len(xt) >= 2) #gibts überhaupt 2 stellen für die reg
    {
      y1=y[xt]
      cat(x1,x2, "segmente: ",len(xt), "\n")
      #browser()
      train <- data.frame(y1,xt )
      mod= lm(Close ~ xt, train)
    }
    return(y[x1])
    return(coef(mod)[2])
  }
  
  y=prices["2005::2007"]
  test1=3
  head(prices)
  work1(prices["2005::2007"])
  
  test1 <- rollapplyr(prices, width=400, FUN=work1, by.column=FALSE)
  head(test1)
  plot(test1)
  
  regfitted.values=prices[lasti,1]
  
  ARBEIT  ##MMA1 ####LowlongIndikator sauber auswerten############################################
  
  len(prices)
  
  dates =  HL$highs[HL$highs<=i5]
  yy= prices[dats]
  
  trainData = data.frame(yy,dats)
  colnames(trainData) <- c( "Y","X")
  X =  seq(from = i5, to=len(prices))
  predictData= data.frame(prices[X],X)
  
  mod <- lm(Y ~ X, data = trainData)
  m=coef(mod)[2]
  plot(yy)
  lines(as.xts(mod$fitted.values),col="red")
  
  pp = as.xts(predict(mod,newdata=predictData))
  mPlot(rbind(as.xts(mod$fitted.values),pp),rbind(yy,prices[X]))
  
  oDax=Dax
  
  mGetTickers("Dax","Rex")
  dat = mNorm(merge(Lo(Dax), Hi(Dax), Cl(Dax), Cl(Rex)))
  mPlot( dat)
  
  Dax=oDax
  
  Dax=DAX["2002::2006"]
  LoDax = mNorm(Lo(Dax))
  HiDax = mNorm(Hi(Dax))
  ClDax = mNorm(Cl(Dax))
  OpDax = mNorm(Op(Dax))
  longIndikator = HiDax-ClDax
  colnames(longIndikator) = "Close"
  
  norm_Win(2)
  mPlot(ClDax)
  lines(longIndikator*6+max(ClDax)/2,col="black")
  
  LowlongIndikator  = period.min(longIndikator,  endpoints(longIndikator,"months")) #MMA
  lines(LowlongIndikator*6+max(ClDax)/2,col="blue",type="h")
  mPlot(LowlongIndikator)
  
  #HighLows(longIndikator)
  lowLongIndikator= LowlongIndikator[HighLows(LowlongIndikator,maxdd=5,visual=T)$lows]
  
  lines(lowLongIndikator,col="green")
  
  reg=goodReturn(prices = LowlongIndikator, maxdd = 200, t = "", minReturn= 0.3, visual = T,checkSGB=T)
  
  
  myFun2 <- function(x) {
    n<<-n+1
    #cat(toString(as.Date(index(first(x)))), n, "\n")
    colnames(x) = "Close"
    
    train <- data.frame(x, xt=endpoints(x,"days")[-1])
    mod= lm(Close ~ xt, train)
    return(coef(mod)[2])
  }
  
  myFun3 <- function(x) {
    n<<-n+1
    #cat(toString(as.Date(index(first(x)))), n, "\n")
    colnames(x) = "Close"  
    train <- data.frame(x, xt=endpoints(x,"days")[-1])
    
    mod= lm(Close ~ xt, train)
    return(coef(mod)[2])
  }
  n=0;   test1 <- rollapplyr(LowlongIndikator, width=10, FUN=myFun3, by.column=FALSE)
  
  new_Win()
  mPlot( merge(ClDax, LowlongIndikator*100))
  lines(test1*1000,col="black",type="h")
  
  
  
  ########################
  myFun4 <- function(x) {
    n<<-n+1
    #cat(toString(as.Date(index(first(x)))), n, "\n")
    colnames(x) = "Close"
    i=as.integer(index(x))-firstIndex   #baut die korrekten IndexPositionen der spärlichen Extrema über die ich fitte
    train <- data.frame(x, xt=i)
    mod= lm(Close ~ xt, train)
    return(coef(mod)[2])
  }
  
  
  new_Win(2)
  lowLongIndikator= LowlongIndikator[HighLows(LowlongIndikator,maxdd=5,visual=T)$lows]
  n=0; firstIndex=as.integer(first(index(lowLongIndikator)));  test1 <- rollapplyr(lowLongIndikator, width=2, FUN=myFun4, by.column=FALSE)
  
  mPlot( merge(ClDax, LowlongIndikator*100))
  lines(scaleTo(ClDax,c(-2,2)), col="black")
  lines(scaleTo(test1,c(-2,2)),col="green",type="h",lwd=2)
  
  ######################
  
  #longIndikator1 = HiDax-LoDax# schwach - aber die Krümmung find ich interessant
  l1a  = scaleTo(LowlongIndikator,c(0,1))
  dax1 =   scaleTo(period.min(ClDax,  endpoints(ClDax,"months")),c(0,1))
  mchart(merge(l1a,dax1))
  mPlot(dax1,l1a)
  
  train <- data.frame( merge( dax1, l1))  
  colnames(train) = c("Y","xt")
  tail(train)
  
  mod= lm(Y ~ xt, train)
  summary(mod)
  plot(mod,which=1)
  #-------------------------------
  
  library(MASS)
  bcy<-boxcox(mod)
  
  
  ########################
  
  longIndikator2 =  HiDax-ClDax  #super
  l2  = scaleTo(period.min(longIndikator2,  endpoints(longIndikator2,"months")),c(-1,1))
  dax1 =   scaleTo(period.min(ClDax,  endpoints(ClDax,"months")),c(-1,1))
  mchart(na.omit(dax1))
  colnames(dax1)="dax1"
  purePlot(dax1,l2)
  
  train <- data.frame( merge( dax1, l2))# )  #lag(l2))  )
  colnames(train) = c("Y","xt")
  tail(train)
  
  mod= lm(Y ~ xt, train)
  summary(mod)
  plot(mod, which=1)
  ########################
  
  longIndikator3 =  LoDaHiDax
  l3  = scaleTo(period.min(longIndikator3,  endpoints(longIndikator3,"months")),c(-1,1))
  dax1 =   scaleTo(period.min(ClDax,  endpoints(ClDax,"months")),c(-1,1))
  mPlot(dax1,l3)
  
  train <- data.frame( merge( dax1, lag (l3)))# )  #lag(l2))  )
  colnames(train) = c("Y","xt")
  tail(train)
  
  mod= lm(Y ~ xt, train)
  summary(mod)
  plot(mod, which=1)
  
}