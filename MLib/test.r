#save(list=ls(),file="d:/mm.Rdata")
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)

if (F)
{
  channelStop("NIKKEI225",F)
  channelStop("OMX_COPENHAGEN20",F)
  channelStop("TREASURY",F)
  channelStop("REX",T)
  x=channelStop("DAX",F)
  
  x=channelStop("SXTBP",F) ##<<<<<<
  
  prices= Lo(data$TREASURY)
  
  sym ="TREASURY"
  price =na.omit(data[[sym]]["2003::"])
  price = mNorm(price)
  
  data$prices =data.info(data)
  
  lomi= runMin(Lo(na.omit(price)),n=12)
  plot(lomi)
  HighLows(lomi,visual=T)
  
  HighLows(prices,maxdd=15,visual=T) #<<######
  
  for(i in c(1:len(data$symbolnames)))
  {
    sym =data$symbolnames[i]
    channelStop(sym)
  }
  
  
}

######################################################################################

channelStop.daily<-function(sym="DAX",dd=7,visual =F)
{
  ###########################################################################
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #der rollierende Train/Test-Lauf auf endpoints - die Fensternlänge der Train-Daten
  #berechnet sich forecast selber
  #browser()
  price=na.omit(global_arg$dat[[sym]])
  #price = mNorm(price["2002-08-10::"])
  price = mNorm(price["2005-08-10::"])
  
  global_price <<-price
  allDates = as.Date(index(price))
  last_constructionDate_ <<- DateS(first(price))
  last_today_ <<- DateS(first(price))
  #browser()  
  stopLine_<<-price[,c(1,2,3,4,5,6)] # supportLine, Kontakt, beta, 
  stopLine_ <<-cbind(stopLine_, price[,c(1,2)])
  signal_<<- price[,1]; signal_[]<<-0
  is.hot_ <<-T   # damit er überhaupt in eine init-pos gehen kann
  entryPrice_ <<-NA
  
  colnames(stopLine_)<<-spl("supportLine,Kontakt,beta,upLine,upKontakt,Pos,hot,priceMove")
  stopLine_[] <<- NA
  glattW = 3 #12
  lomi= runMin(Lo(na.omit(price)),n=glattW) #lag mäßig ok
  himi= runMax(Hi(na.omit(price)),n=glattW)
  
  if (visual)
    mchart(merge( lomi,himi, Cl(price)))
  
  showZig=visual
  
  zzLo=mZigZag2(lomi,dd=dd,visual=showZig)
  zzHi=mZigZag2(himi,dd=dd,visual=showZig)
  
  #browser(mP(" INIT "))
  offset = 100
  #die initiale Marketpos
  m.pos_<<-sign(first(na.omit(ROC(Cl(price),n=offset))))
  entryPrice_ <<- Cl(price[offset])
  #m.sig<<-sign((na.omit(ROC(Cl(price),n=offset))))
  #plotSigPrice(signal=m.sig,prices=mNorm(Cl(price)),indi=list(roc=merge(Cl(price),ROC(Cl(price)))) )
  
  lows<<- first(Lo(price))
  highs<<- first(Hi(price))
  
  zbrk.Lo =unlist(lapply(zzLo,function(x){ ifelse(x$ptype=="low",toString(x$brk),NA) }))
  zbrk.Lo[zbrk.Lo==""] = NA; 
  
  
  zbrk.Hi =unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",toString(x$brk),NA) }))
  zbrk.Hi[zbrk.Hi==""] = NA;
  
  if(showZig)
  {
    plot(lomi)
    peak.Lo =na.omit(unlist(lapply(zzLo,function(x){ ifelse(x$ptype=="low",DateS(x$peak),NA) })))
    
    amark(peak.Lo,"magenta")
    amark(zbrk.Lo)
    View (zbrk.Lo)
    browser()
  }
  if(showZig)
  {
    plot(himi)
    peak.Hi =na.omit(unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",DateS(x$peak),NA) })))
    amark(peak.Hi,"red")
    amark(zbrk.Hi,"blue")
    View (zbrk.Hi)
    browser(mP("#### showZig"))
  }
  if (F&showZig)
  {
    plot(Lo(global_price))
    plot(lomi)
    
    amark(peak.Lo,"green")
    amark(zbrk.Lo,"yellow")
    
    View(peak.Lo)
  }
  #gib Info über peaks und den Zeitpunkt brk zu dem sie identifiziert wurden
  #--------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  oaw <<-lapply( allDates[c(offset:len(allDates))], function(x)  roll_channelStop( Today=x, price, lomi,himi, symbol=sym, zzLo,zzHi,zbrk.Lo, zbrk.Hi,visual=visual ))
  #<<<<<<<<<<<<<-----------------------------------------
  OAW = rbindlist(oaw) #data.table aus liste von listen ...
  #den data.table ->xts
  #targetXts=as.xts(OAW, order.by = as.Date(OAW$Today))[,-1];   
  res=merge(Lo(global_price),stopLine_)
  
  pric=1;resi=5;supp=2; cutResi=6;cutSupp=7
  
  #detach("package:xtsExtra",unload=T)
  plota(res[,pric],main="RES1")
  plota.lines(res[,pric], col=iif(signal_>0,"blue",ifelse(signal_==0,"black","red")),lwd=3 )
  lines(res[,resi],col="blue")
  lines(res[,supp],col="red")
  
  browser(mP("********************>"))
  
  Posi=res[,"Pos"]
  res[,"upLine"][Posi==1]<-NA
  res[,"supportLine"][Posi==-1]<-NA
  View(res["2010"])

  plotSigPrice(signal=signal_,prices=mNorm(Lo(price)),indi=list(stopLine=merge(Lo(price),res[,"supportLine"],res[,"upLine"])) )
  
  
  browser(mP("############### ok"))
  #  library(xtsExtra)
  #  plot.xts(res[,c(pric,resi,supp)],main="fertig",screens=1,ylim=range(res[,pric]))
  
  res=cbind(res,res[,c(1,2)]); res[,cutResi]=NA; res[,cutSupp]=NA
  
  #  res[,cutResi]=cutLevel(res[,pric],res[,resi],"up"); lines(res[,cutResi],type="p",col="blue")
  # browser(mP("############### fertig #################"))  #MM1
  
  res[,cutSupp]=cutLevel(res[,pric],res[,supp],res[,resi]);
  
  
  #browser(mP("############### fertig #################"))  #MM1
  return(0)
  
  lines(res[,cutSupp],type="p",col="red")  
  
  View(res[,cutSupp])
  print(na.omit(res[,cutSupp]))
  amark(na.omit(res[,cutSupp]))
  icut=get.Index(res[,pric],index(na.omit(res[,cutSupp])))
  
  icutI=2
  
  for(i in seq(-3,0))
  {
    amark(index(res[icut[icutI]]))
    print(res[icut[icutI]-i,pric] < res[icut[icutI]-i,resi])   
  }
  
  #plot.xts(res[,c(pric,resi,supp,cutResi,cutSupp)],main="fertig",screens=1,ylim=range(res[,pric]))
  
  
  ######
  
  plot.xts(res[,c(pric,resi,supp)],main="fertig",
           screens=c(1), #plot 1st series in 1st panel and 2nd and 3rd series in 2nd panel
           layout.screens=c(1,1,2)
           
           ,  #just as an example change the layout so 1st panel is 1/3 of area and 2nd is bottom 2/3
           blocks = list(  #set up blocks for recessions
             start.time=c("2008-01-01","2012-01-01"),
             end.time=c("2009-01-01","2013-01-1"),
             col = "lightblue")
  )         
  
  
  
  d="2009-04-03"
  amark(d)
  #res[d,pric] >=res[d,resi]
  
  coredata(res[get.Index(res[,pric],d),pric]) > coredata(res[get.Index(res[,pric],d)-1,pric])
  
  stopSignal=iif(stopLine_[,"Kontakt"] == 999,0,1)
  plotSigPrice(signal=stopSignal,prices=mNorm(Lo(price)),indi=list(stopLine=merge(Lo(price),stopLine_[,1])) )
  amark(zbrk.Lo)
  
  Res<<-res
}
if (F)
  channelStop.daily(sym="DAX",visual=T)  #<<<<<<<<<<<<<<< hier starten

##########################################################################
if (F)
  res[,cutSupp]=cutLevel(res[,pric],res[,supp],res[,resi]);

##############

###################################################################
#Die resi und spupport-lines liegen vor, werte nun aus -> signal
#Alle resi und supp daten stehen zum Zeitpunkt wo sie gezeichnet werden
#auch zur Verfügung (keine zukunftswissen eingearbeitet)

#wenn ein resi (blau) existiert und ich short bin: schalte
# resi aus und geh long wenn supp existiert und cuteventLowUp
###################################################################

###########################################################################
#schreibe nach   stopLine_<<-price[,c(1,2,3,4)] # supportLine, Kontakt, beta, offset Break
if (F)
  head(na.omit(stopLine_[,1]))
###########################################################################

roll_channelStop<-function(Today=Sys.Date(),price,Lomi,Himi,  symbol,zzLo,zzHi,zbrk.Lo, zbrk.Hi,visual=F )
{
  res = list() #ergebnis-liste
  #Lo(price[Today,])< stopLine_[Today,1]  
  if (visual)
  {
    #sicherheitshalber aussperren zukünftiger Preise
    price =price[as.Date(index(price))<=as.Date(Today)]
    signal = signal_[as.Date(index(price))<=as.Date(Today)]
    mP("## %s %d ## %s ########### p %f   Stop %f", Today,m.pos_,"roll_channelStop",Lo(price[Today,]), stopLine_[Today,1])
  }
  else
    mP("%s %d ",Today,m.pos_)
  
  changedPos=F
  if (Today =="2010-04-22")
    browser(mP("Stop"))
  
  
  if (visual)
  {
    #browser(mP("~~~~~~~+++++++++++++++"))
    #plota(Lo(price),main=sprintf("lower C.UT %s",Today))
    #plota.lines(Lo(price), col=iif(signal>0,"blue",ifelse(signal==0,"black","red")),lwd=3 ) 
    
    amark(last_today_,"gray")
    amark(Today,"yellow")
  }
  
  last_today_ <<- Today
  
  #Test auf CUTs # lower###########################################################
  
  if (!is.na(stopLine_[Today,1] ) && len(na.omit(stopLine_[,1]))>0)  #es wurde bereits mit der Erstellung einer supportline begonnen
  {
    #beeing LONG - check for support-line-break
    try(
      if (is.hot_)
      if (Pos(Today)==1 || Pos(Today)==0)
      if ( Lo(price[Today,])< stopLine_[Today,1]  )
      {
        #stopLine_[Today,2]<<- 1
        m.pos_<<- -1   
        changedPos = T
        mP("*****2:  %s >%d",Today,m.pos_)
        stopLine_[Today,"Kontakt"] <<- 1  
        #View(stopLine_)
        try(      if (visual && stopLine_[Today,2] != 1)
        {
          plota(Lo(price),main=sprintf("lower CUT %s",Today)); 
          plota.lines(Lo(price), col=iif(signal>0,"blue",ifelse(signal==0,"black","red")),lwd=3 )   
          plota.lines(stopLine_[,1],col="red")
          
          col = iif(sig <= 0, wpCol, "black")  #malen den dicken rand bund wenn du flat bist
          # browser()    
          
          amark(last_constructionDate_,"green");   amark(Today,"blue")
          browser()
        })
      })
  }
  #Test auf CUTs #######upper#####################################################
  try(
    #if(!changedPos)
    if (is.hot_)
    if (Pos(Today)== -1 || Pos(Today)==0)
    {
    if ( !is.na(stopLine_[Today,4] ) && len(na.omit(stopLine_[,4]))>0)  #es wurde bereits mit der Erstellung einer resistanceline begonnen
    {    
      #beeing SHORT - check for resistance-line-break
      
      try(  if (   Hi(price[Today,]) > stopLine_[Today,4]  )
      {
       # stopLine_[Today,5]<<-   1
        #View(stopLine_)
        m.pos_ <<- 1
        changedPos=T
        mP("*****1:  %s >%d",Today,m.pos_)
        stopLine_[Today,"upKontakt"] <<- 1  
        
        # browser(mP("upper CUT"))
        try(
          if (visual)# && lag( stopLine_[Today,5]) != 1)
          {
            plota(Lo(price),main=sprintf("upper CUT %s",Today)); plota.lines(stopLine_[,4],col="blue")
            plota.lines(Lo(price), col=iif(signal>0,"blue",ifelse(signal==0,"black","red")),lwd=3 )
            amark(last_constructionDate_,"green");   amark(Today,"blue")
            # browser()
          }
        )
      })
    }
    }  
  )
  
  #######################################################################
  #support zu Absicherung einer long-Position
  if (contains(zbrk.Lo,toString(Today))  )   #gibts heute ein neues Peak identifiziert
  {
    brki = which(zbrk.Lo==Today);   zi = zzLo[[brki]]
    
    if (zi$ptype =="low")
    {
      lomi=Lomi[index(lows)]   #die Ecken
      if (is.na(lomi[1])) lomi[1]=Lo(price[1])
      
      mP("Today is %s , found new %s some days ago: %s %f",zi$brk,zi$ptype,DateS(zi$peak),zi$peak)  
      
      lows <<- append(lows, zi$peak)
      lomi=Lomi[index(lows)] 
      
      rlomi=index(lomi[which(lomi >= lag(lomi))])
      
      if (T)
      {
        dlomi=sign(ROC(lomi,n=1))
        dlomi[is.na(dlomi)]<-   -0.1
        dL = last(dlomi[which(dlomi < 0)])
        #rlomi=lomi[which(lomi>=lag(lomi))]  #die, die höher als ihr vorgänger sind
        dlomi2=dlomi[index(dlomi)>=index(dL)]
        rlomi=index(dlomi2)
      }
      
      #browser(mP("######## at LOW"))
      
      if (len(rlomi) >1 )
      {
        p1=first(rlomi)
        print (rlomi)
        mP("%s lowerChannel fortführen",Today)        
        if (visual)
        {
          plota(Lo(price),main="readjust channel")
          plota.lines(Lo(price), col=iif(signal>0,"blue",ifelse(signal==0,"black","red")),lwd=3 )
          amark(rlomi)
          amark(Today,"yellow")
          browser()
        }
  #      m.pos_ <<-   1
 #       mP("*****3:  %s >%d",Today,m.pos_)
        
        res=supportChannel("low",symbol,Lo(price),lomi, rlomi,p1,Today,visual)
      }      
    }
    if (visual)
      plot(Lo(global_price))
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #support zu Absicherung einer short-Position - bzw. zur Signalisierung der Freigabe für going long
  if ( contains(zbrk.Hi,Today) )   #gibts heute ein neues Peak identifiziert
  {
    brki = which(zbrk.Hi==Today);   zi = zzHi[[brki]]
    
    mP("Today is %s , found new %s some days ago: %s %f",zi$brk,zi$ptype,DateS(zi$peak),zi$peak)  
    
    highs <<- append(highs, zi$peak)
    himi=Himi[index(highs)] 
    rhimi=index(himi[which(himi <= lag(himi))])
    #browser()
    if (T)
    {
      dhimi=sign(ROC(himi,n=1))
      dhimi[is.na(dhimi)]<-   0.1
      dH = last(dhimi[which(dhimi > 0)])
      #rlomi=lomi[which(lomi>=lag(lomi))]  #die, die höher als ihr vorgänger sind
      dhimi2=dhimi[index(dhimi)>=index(dH)]
      rhimi=index(dhimi2)
    }
    mP("######## at HIGH")
    
    if (len(rhimi) >1 )
    {
      p1=first(rhimi)
      print(rhimi)
      mP("%s upperChannel fortführen",Today)        
      if ( visual)
      {
        plota(Lo(price),main="readjust upper channel")
        plota.lines(Lo(price), col=iif(signal>0,"blue",ifelse(signal==0,"black","red")),lwd=3 )
        amark(rhimi)
        amark(Today,"yellow")
        browser()
      }
#      m.pos_ <<-   -1
      mP("*****4:  %s >%d",Today,m.pos_)
      
      res=supportChannel("high",symbol,Hi(price),himi, rhimi,p1,Today,visual)
    }      
  }
  
  #kein neues  Peak gefunden:
  if (len(na.omit(stopLine_[,1]))<1)
  {
    #mP("stop bei MA")
  }
  #ergebnis-liste bauen
  #  res$Date=Today
  
  if (Today > index(first(signal_)))
  {
    #Pos-Änderung 
    
    if (nval(signal_[get.Index(signal_,Today)-1]) !=   m.pos_)
    {
      is.hot_ <<- F
      if (m.pos_ != 0)
      {
       mP("äääääääää NEUE POS ääääääääääääääääääääääääääää %d",m.pos_)
       entryPrice_<<- Cl(price[Today])
       entryDate_ <<- Today
      }
    else
      {
        entryPrice_<<-NA 
       entryDate_ <<-NA
      }
    }
    if (abs(nval(signal_[get.Index(signal_,Today)-1]) -   m.pos_ )==2)
    {
      mP("DREHER ####### ")
      #nun den Offset von Support und Resistance , über, unter den aktuellen Preis ziehen
      frame=sprintf("%s::%s", Today,  as.Date(index(last(global_price))) )
      d=nval((stopLine_[Today,1]-Lo(price[Today])))
      print(d)
      
     # browser(mP("write #######################################"))
      #if (beta > 0)
      stopLine_[frame,1] <<-  stopLine_[frame,1] - nval((stopLine_[Today,1]-Lo(price[Today])))
      stopLine_[frame,4] <<-  stopLine_[frame,4] - nval((stopLine_[Today,1]-Hi(price[Today])))    
    }
  }
# der  stop wird erst scharf, wenn die maxbewegung vom entry weg wenigstens x% von DD ist.   (kein stop im rauschen  bzw. horizontalem...   (posDD wird benötigt .. der DD since Entry ...))
  if (Today =="2010-04-22")
    browser(mP("Stop"))
  
  ddMM=0
  if (!is.hot_)   #wenn der price sich genug vom entryPrice weg bewegt hat wird das reissleinensystem scharf geschaltet
  if (!is.na(entryPrice_) && Pos(Today) != 0)
  {
    hotThresh=7
    #browser()
    ddMM=nval(abs(nval(Cl(price[Today])) - entryPrice_) / entryPrice_ * 100)
    mP("ddMM  mmmmmm %f mmmmm pos: %d",ddMM, m.pos_ )
    if ( ddMM >= hotThresh ||  
           get.Index(signal_,entryDate_)  - get.Index(signal_,Today) >9)
      {is.hot_<<-T   #reissleinensystem scharf schalten
    #browser()
    }
    else
       is.hot_<<-F
  }
  
  stopLine_[Today,"hot"] <<- ifelse(is.hot_,1,0)
  stopLine_[Today,"priceMove"] <<- ddMM
  signal_[Today] <<-   m.pos_
  stopLine_[Today,6] <<- m.pos_  #ebenfalls das Signal
  #mP("---> %f",res)
  return(res)
}

Pos<-function(Today)
{
  signal_[get.Index(signal_,Today)-1]
  #signal_[Today ]
}

nval<-function(x)
{
  res=0
  if (len(x)==0)
    return(0)
  if (is.na(x))
    res=0
  res=try(as.numeric(x))
  return(res)
}

##########################################################################

supportChannel<-function(mode,symbol,price, lomi,rlomi, p1,d2_now, visual=F)
{
  res=list()
  supportPoints =NULL
  schnittpunkt.date=NULL
  segx1 = last_constructionDate_
  
  
  
  if (!is.null(p1)) #soll überhaupt mit supportPoints eine neue support-gerade gesucht werden?
  {
    schnittpunkt.date= as.Date(index(last(lomi)))
    
    if (visual)
    { 
      mP("%d support %s Stützstellen bei %s  (zurück bis %s) ",len(rlomi),mode,d2_now,p1)
      browser()
    }
    supportPoints=lomi[sprintf("%s::%s",p1,d2_now),]
    if (visual)
      if (len(supportPoints) > 0) 
        print(supportPoints )
    else
    {
      print("NO support Points at %s",toString(d2_now))
      #browser()        
    }
  }
  
  if (!is.null(p1) && len(supportPoints) > 0)   #mit gewichteter lm() neue support-line berechnen
  {
    segx1 = get.Index(price, index(first(supportPoints)))
    segx2 = get.Index(price, index(last(supportPoints)))
    
    #übersichtsplot
    if (visual)
    {
      plot(price,main=sprintf("overview %s",toString(colnames((price))))  )
      #auch Lo und Hi einblenden
      lo1=Lo(global_arg$dat[[symbol]][index(price),])
      hi1=Hi(global_arg$dat[[symbol]][index(price),])
      
      lines(mNorm(lo1));
      lines(mNorm(hi1))
      lines(supportPoints,col="blue"); lines(supportPoints,type="p",col="red")
      lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
      
      lines(stopLine_[,1],col="red")
      lines(stopLine_[,4],col="blue")
      amark(d2_now,"yellow")
      #View(stopLine_)
      #browser()
    }
    
    Hist=hist=(price[seq(segx1,segx2),]);hist[]=NA
    Y= merge(hist,supportPoints)[,2]; colnames(Y)=c("y")   
    # browser(mP("##################### ~~~~~~~~~~~~~~~~~~"))   
    X=get.Index(price,index(supportPoints))
    
    train <- data.frame(y=coredata(supportPoints), xt=X)
    colnames(train)=c("y","xt")
    #train=na.omit(train)  -- beta berechnen !!!!
    #mod= lm(y ~ xt, train)
    w=(X-X[1]+1) ; w = w/ last(w); w=expm1(0.3*w);w = w/ last(w); #Gewichtung gem. Abstand - je höher der factor in expm1 dest schärfer klingt der Einfluß der alten supportpoints ab   
    mP("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ w ist %f",w)
    #  if (len(X)>3) 
    #    browser(mP(" press key "))
    mod = lm(y ~ xt, train, weights=w)
    beta <- coef(mod)[2]  #beta-Berechnung !!!
    last_constructionDate_ <<- d2_now
    if (mode=="low") 
      beta=max(beta,0) #vorsicht - bei negativen beta bist Du ungehedged
    if (mode =="high")
      beta=min(beta,0)
    
    #if (beta >= 0) #MM_TODO - vorsicht - bei negativen beta bist Du ungehedged
{
        mod.b<-coef(mod)[1]
        b=coredata(last(supportPoints))  #first
        
        if (T) #MM_check
        {
          xi=0:(segx2-segx1)
          yi = beta*xi+b
          inHist = Hist;inHist[]=yi
          # lines(inHist,col="green")
          dy = as.numeric(last(inHist))-as.numeric(last(supportPoints))
          b=b-dy  #vertikal-verschiebung der reg-geraden auf den letzten supportPoint
        }
        
        if (F&& visual) #plot der hist-reg
        {
          plot(Hist,main="Hist-Test")
          lines(supportPoints,col="blue"); lines(supportPoints,type="p",col="red")
          lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
          
          
          inHist[] = coredata(inHist)-dy
          lines(inHist,col="red")
          browser()
        }
        
        
        xi=0:(len(global_price[,1])-1)
        yi = beta*xi+b
        
        pred.mod = (global_price[]) ;pred.mod[]=yi
        dy = last(as.numeric(pred.mod[as.Date(index(last(supportPoints)))])-as.numeric(last(supportPoints)))
        
        pred.mod[] = coredata(pred.mod)-dy
        
        frame=sprintf("%s::%s", as.Date(index(price[d2_now,])),  as.Date(index(last(global_price))) )
        
        #browser(mP("write #######################################"))
        #if (beta > 0)
        if (mode == "low")
          stopLine_[frame,1] <<- pred.mod[frame,1]
        else
          stopLine_[frame,4] <<- pred.mod[frame,1]
        
        if (visual)
        {
          lines(stopLine_[frame,1],col="red")
          lines(stopLine_[frame,4],col="blue")
          browser() 
        }
        stopLine_[segx2,3]<<-  beta  #beta
        #stopLine_[segx2,4]<<-  b-dy  #offset
        
      } #beta > 0
  }   
  res$Today=d2_now
  res$beta=beta
  
  
  return(res)
}
###############################################################################
cutLevel  <- function(price,supp,resi,visual=T) #supp unten,  resi  oben
{
  #browser()
  resi[lag(resi,k=3)< supp] <-NA   #resi immer oberhalb von supp
  supp[lag(supp,k=3)> resi] <-NA    #supp immer unterhalb von res
  
  # rMin <-  runMin(Lo(price),n=100) 
  #  supp == iif(is.na(supp), lag(rMin,k=10), supp)
  pos=new.pos=-1; expect = 0   
  
  res=price[,1];
  #resi=iif(res > resi & !is.na(supp),supp,resi)
  
  if (visual)
  {
    plot(res,main="RES",ylim=range(res,na.rm=T))  
    #plot(Lo(price))
    lines(supp,col="green")
    lines(resi,col="blue")
    
  }
  res[]=NA
  
  cuteventLow=  iif (!is.na(supp) & price < supp & lag(price) >= supp ,T, F)  
  cuteventLowUp= iif (!is.na(supp) & price > supp & lag(price,k=2) <= supp ,T, F)
  
  
  cuteventHigh= iif (!is.na(resi) & price > resi & lag(price,k=2) <= resi ,T, F)
  cuteventHighDown=  iif (!is.na(resi) & price < resi & lag(price) >= resi ,T, F)
  
  
  global_xMarker <<- list()
  
  
  #sig=iif(Lo(price) <= supp ,-1,1)
  sig = Lo(price);sig[]=0
  sig[!is.na(resi) & price<resi  ] <- -1
  sig[!is.na(resi) & price>resi ] <- 0
  
  sig[!is.na(supp) & price>supp ] <- 1
  sig[!is.na(supp) & price<supp  ] <- -1
  
  
  
  plotSigPrice(signal=sig,prices=price,indi=list(stopLine=merge(price,supp,resi )))
  res=sig
  #browser()
  return(res)
  
  
  
  
  ####################################################
  
  
  if (visual)
  {
    lines(price[cuteventLow],type="p",col="red")  
    lines(price[cuteventLowUp],type="p",col="yellow")  
    lines(price[cuteventHigh],type="p",col="blue")  
    lines(price[cuteventHighDown],type="p",col="magenta")  
    #
    # cuteventLowUp[cuteventLowUp]
    print("lowscut")
    print(price[cuteventLow])
    print("hitcuts")
    print(price[cuteventHigh])
    
    
    browser(mP("cutLevel"))
  }
  
  if (F)
  {
    #einfachste Signal-auswertung
    sig = iif(price > supp, 1, -1 ) #nur supp
    #nur resi  
    sig=iif(price < resi,-1,1)
    #  sig = iif(price > supp & ifelse(is.na(resi) || hier weiter machen), 1, -1 )
    plotSigPrice(signal=sig,prices=price,indi=list(stopLine=merge(price,supp,resi )))
  }
  if(F)
  {
    fLow=first(price[cuteventLow])#2009-01-23
    amark(DateS(fLow))
    cevh=cuteventHigh[cuteventHigh]
    fHigh=first(cevh[index(cevh)> index(fLow)])
    amark(DateS(fHigh))
    
  }
  
  
  for(x in c(1:len(price)))  #get.Index(price,index(price))
  {
    #  if (DateS(price[x])=="2009-04-03")
    #    browser(mP("at Cut %s",DateS(price[x])))
    #Bis zum cut im alten Zustand bleiben
    
    #immediate postion change
    if (pos == 1 )
    {
      if (cuteventLow[x])
      {
        expect = 0    
        new.pos=-1
        mP("%s ##0#  pos=%d~>%d,expect=%d",DateS(price[x]),pos,new.pos,expect)
        amark(index(price[x]),col="red")
        browser()
      }
    }
    
    if (pos == -1)
    {
      if (cuteventLowUp[x] || cuteventHigh[x])
      {
        #expect =0   
        new.pos=1
        
        mP("%s ##a#  pos=%d~>%d,expect=%d",DateS(price[x]),pos,new.pos,expect)
        amark(index(price[x]),col="blue")
        browser()
      }
    }
    
    #delayed postion change A
    
    if (F && pos == -1 )
    {
      if (cuteventHigh[x])
      {
        expect =-1   
        new.pos=pos
        
        mP("%s ##b#  pos=%d~>%d,expect=%d",DateS(price[x]),pos,new.pos,expect)
        amark(index(price[x]),col="blue")
        browser()
      }
    }
    if (F && pos == -1 )
    {
      if (cuteventLow[x])
      {
        expect =1   
        new.pos=0
        
        mP("%s ##c#  pos=%d~>%d,expect=%d",DateS(price[x]),pos,new.pos,expect)
        amark(index(price[x]),col="red")
        browser()
      }
    }
    #browser()  
    
    #delayed postion change B
    if (F && pos==0 && expect != 0)
    {    
      mP("##d# %s pos=0,expect=%d",DateS(price[x]),expect)
      if (expect == 1 && cuteventHigh[x])
      {
        new.pos=1
        amark(index(price[x]),col="blue")
        mP("##e# %s ##### pos=%d~>%d ##### ",DateS(price[x]),pos,new.pos)
        browser()
      }
      if (expect == -1 && cuteventLow[x])
      {
        new.pos=-1
        amark(index(price[x]),col="red") 
        
        mP("##f# %s ##### pos=%d~>%d ##### ",DateS(price[x]),pos,new.pos)
        browser()
        
      }
    }
    
    pos=new.pos
    res[x] = pos    
  }
  
  plotSigPrice(signal=res,prices=price,indi=list(stopLine=merge(price,supp,resi )))
  amark(index(price[cuteventLow]),col="red")
  amark(index(price[cuteventHigh]),col="blue")
  
  browser()
  
  return(res)
}



if (F)
{
  library(snow)
  #hilfe zu snow
  #http://www.sfu.ca/~sblay/R/snow.html#parLapply
  cores = 4
  mycl <- makeCluster(cores, type="SOCK") ## Setup 4 cluster nodes onthe local 
  parLapply(cl, X, fun, ...)
  stopCluster(cl)
}

################################################################################
###########################################################################

signal.MA<-function(arg, par = mlist(zlemaN=c(3,3,20,10), slow=c(40,20,120,20), fast=c(40,20,120,20)),visual=F,...)
{
  #slopes=diff(arg$clos,as.integer(par$k))
  #geglätteter Kurs
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
  
  return(list(Signal=signal, Indi=list(ma=merge(zlem,Ma,Mi,MaMi))))
  return(list(Signal=signal, Indi=list(ma=merge(zlem,slow,fast,rM,rMi))))
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (F)
{  
  global_ParTable=NULL    #MM_TODO bug: er fügt identische keys hinzu ...
  global_commission=0.011
  data$prices = data.info(data)
  global_arg = list(clos=data$prices, dat=data)
  
  res=signal.MA(global_arg)   # DOW: 10,150
  res=signal.wonder(global_arg,list( k=10, zlemaN=150),visual=T)   # DOW: 10,150
  res=signal.wonder(global_arg,list( k=76, zlemaN=2),visual=T)   # DOW: 10,150
  #das ganze mit chart und Auswertung
  
  sym="DAX"
  sym="CAC40"
  x=indi.Generic("signal.MA", global_arg, list(zlemaN=20,slow=120,fast=120), visual=T, TRAINSYM=sym)  
  x=indi.Generic("signal.MA", global_arg, list(zlemaN=12,slow=250,fast=100), visual=T, TRAINSYM=sym)  
  
  #und jetzt für alle Zeitreihen als gleichgewichtetes Portfolio
  x=indi.Generic("signal.MA", global_arg, visual=T, TRAINSYM=-1)   #jeder mit gleichen BestParams
  TrainIndicator (opti="GRID",indiName = "signal.MA",visual=T,TRAINSYM=sym)  
  TrainIndicator (opti="GRID",indiName = "signal.MA",visual=T,TRAINSYM=-1)  
  
  mlist()
}
