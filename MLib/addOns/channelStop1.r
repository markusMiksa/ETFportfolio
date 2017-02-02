
channelStop<-function(sym="DAX",visual=F)
{
  if (visual)
  {
    print("#####################channelStop######################################")
    print(sym)
    #    browser()
  }
  price =na.omit(data[[sym]]["2002::"])
  #price = -mROC(price)  #die short-reihe
  #price = mRendite(price)
  #plot(price)
  price = mNorm(price)
  
  stopLine_<<-price[,c(1,2,3)] # supportLine, Kontakt, Break
  stopLine_[] = NA
  
  lomi= runMin(Lo(na.omit(price)),n=12) #lag mäßig ok
  himi= runMax(Hi(na.omit(price)),n=12)
  #plot(Lo(na.omit(price[c(1:30)])))
  #lines(((himi[c(1:30)])),col="red")
  
  #browser()
  #lomi= Lo(na.omit(price))
  #himi= Hi(na.omit(price))
  
  if (last(lomi) == 0 )
  {
    #Lo(price)=Cl(price) #price-repair: werden keine brauchbaren lows geliefert- geht auf close
    price[,3]=Cl(price)
    lomi= runMin(Lo(na.omit(price)),n=12)
  }
  if (visual)
    mchart(merge( lomi,himi, Cl(price)))
  
  lows=HighLows(lomi, maxdd=5,visual=visual)$lows #5
  Lows=Lo(price[lows])
  price = Lo(price) #damit die Chart-warnings ausbleiben
  price[lows,1]
  #plot(mNorm(Lo(na.omit(price))))
  #die lage der lows
  lomi=lomi[lows]
  dlomi=sign(ROC(lomi,n=1))
  rl=runLengthEnc2(dlomi)
  rl[rl==-1]<-0  #auch das erste Low-Extremum einer Kette mitnehmen, dass noch keine Vorgänger hat
  
  segmente=HighLows(lomi,visual=visual)$hl 
  #an welchen Tagen kommen Lows die rlp mal schon oberhalb ihres Vorgänger-Lows lagen:
  rlp=rl[rl>=0] #das sind jetzt die Punkte durch die sich support-reg-geraden lohnen .. werte von nacheinander rlp mal steigenden Knoten
  if (visual) print(rlp)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  lapply(c(1:len(rlp)), function(x) {
    rlpx=rlp[x,]
    
    if (rlpx >= 1) #2 für reg werden wenigstens 2 stützstellen benötigt
    {      
      d2_now=as.Date(index(rlpx))
      #Lows
      if (F)
        if (rlpx==1)
          p1 = as.Date(index(last(Lows[which(index(Lows)< d2_now)])))
      else
      {p1_vorletzt=as.Date(index(rlp[x-rlpx+1]) )  
       p1 = as.Date(index(last(Lows[which(index(Lows)< p1_vorletzt)])))
      }
      p1=as.Date(index(rlp[x-rlpx]) )
      if (len(p1)==0)
        p1 = as.Date(index(last(Lows[which(index(Lows)< d2_now)])))
      if (visual)
        mP("%d Stützstellen bei %s  (zurück bis %s) ",rlpx,d2_now,p1)
      supportPoints=lomi[sprintf("%s::%s",p1,d2_now),]
      if (visual)
        if (len(supportPoints) > 0) 
          print(supportPoints )
      else
      {print("NO support Points at %s",toString(d2_now))
       browser()        
      }
      if (len(supportPoints) > 0) 
      {
        segx1 = get.Index(price, index(first(supportPoints)))
        segx2 = get.Index(price, index(last(supportPoints)))
        
        #übersichtsplot
        if (visual)
        {
          plot(price,main=sprintf("overview %s",toString(colnames(Lo(price))))  )
          lines(supportPoints,col="blue"); lines(supportPoints,type="p",col="red")
          lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
          browser()
        }
        #if (len(supportPoints)>2)
        # mchart(supportPoints)
        
        Hist=hist=Lo(price[seq(segx1,segx2),]);hist[]=NA
        Y= merge(hist,supportPoints)[,2]; colnames(Y)=c("y")   
        X= coredata(rollapplyr(supportPoints, 1,function(x) get.Index(Lo(price),index(x)))) 
        train <- data.frame(y=coredata(supportPoints), xt=X)
        colnames(train)=c("y","xt")
        #train=na.omit(train)  -- beta berechnen !!!!
        #mod= lm(y ~ xt, train)
        w=(X-X[1,]+1) ; w = w/ last(w); w=expm1(10*w) #Gewichtung gem. Abstand - je höher der factor in expm1 dest schärfer klingt der Einfluß der alten supportpoints ab   
        mod = lm(y ~ xt, train, weights=w)
        beta <- coef(mod)[2]  #beta-Berechnung !!!
        
        if (beta >= 0) #MM_TODO - vorsicht - bei negativen beta bist Du ungehedged
        {
          mod.b<-coef(mod)[1]
          b=coredata(last(supportPoints))  #first
          
          if (visual) #plot der hist-reg
          {
            plot(Hist,main="Hist-Test")
            lines(supportPoints,col="blue"); lines(supportPoints,type="p",col="red")
            lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
            browser()
            xi=0:(segx2-segx1)
            yi = beta*xi+b
            inHist = Hist;inHist[]=yi
            # lines(inHist,col="green")
            dy = as.numeric(last(inHist))-as.numeric(last(supportPoints))
            b=b-dy  #vertikal-verschiebung der reg-geraden auf den letzten supportPoint
            inHist[] = coredata(inHist)-dy
            lines(inHist,col="red")
            browser()
          }
          
          #schnittpunkt suchen...................
          
          xi=0:(len(price[,1])-1)
          yi = beta*xi+b
          
          pred.mod = Lo(price[]) ;pred.mod[]=yi
          # lines(inHist,col="green")
          dy = as.numeric(pred.mod[as.Date(index(last(supportPoints)))])-as.numeric(last(supportPoints))
          pred.mod[] = coredata(pred.mod)-dy
          frame=sprintf("%s::%s", as.Date(index(price[segx1,])),  as.Date(index(last(price))) )
          
          pred=Lo(price[frame,])
          if (visual)
          {
            plot(pred,main="suche stop")
            lines(pred.mod,col="green")
            
            lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
            #wo liegt der Schnittpunkt in der Zukunft ?
          }
          frame.fut=sprintf("%s::%s", as.Date(index(price[segx2,]))+1,  as.Date(index(last(price))) )
          pred.fut= pred[frame.fut,]; pred.mod.fut = pred.mod[frame.fut]
          
          schnittpunkt.date=as.Date(index(first(pred.fut[which(pred.fut<=pred.mod.fut)])))
          schnittpunkt = get.Index(price,schnittpunkt.date)
          
          hasStop=""
          if  (schnittpunkt==0)
          {
            schnittpunkt.date= as.Date(index(last(price)))
            schnittpunkt = get.Index(price,schnittpunkt.date)
            hasStop="NO "
          }
          
          schnittpunkt.price=Lo(price)[schnittpunkt.date]
          
          frame.cut=sprintf("%s::%s", as.Date(index(price[segx1,])),  schnittpunkt.date+10 )
          
          
          if (visual)
          {
            plot(Lo(price[frame.cut,]),main=sprintf("%s CUT %s",hasStop,schnittpunkt.date))
            
            lapply(supportPoints,function(x) abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkgreen" , 95),lwd=3))
            
            if (hasStop!= "NO")
            {
              abline(h=Lo(price)[schnittpunkt.date],col=col.add.alpha("darkblue" , 95),lwd=3)
              abline(v=as.POSIXct(schnittpunkt.date),col=col.add.alpha("darkblue" , 95),lwd=3)
            }
            lines(pred.mod[frame.cut,],col="red")
            browser()
          }
          
          #patche die Stop-Limits in die Kurve ~~~~~~~~~~~~~~~~~~~~~~
          #überschreibe dabei durchaus alte Linien-Werte .. die nun präzisiert werden - wichtig:du darfst das nur machen wo Du nicht schort sein solltest
          #liegen auf NA
          frame.fut=sprintf("%s::%s", as.Date(index(price[segx2,]))+1,  as.Date(index(last(price))) )
          
          if (beta > 0)
            stopLine_[frame.fut,1] <<- pred.mod[frame.fut,1]
          
          #setze die zukünftigen stopLine_Werte- die unter dem aktuellen LossLimit
          
          S=stopLine_[frame.fut,1 ]
          # S[S < as.numeric(schnittpunkt.price)] <-NA
          
          stopLine_[frame.fut,1] <<- S 
          
          if (hasStop != "NO")  #in spalte 2 werden die datümer der Kontakte geschrieben
          {
            
            stopLine_[schnittpunkt.date, 2] <<- Lo(price[schnittpunkt.date])
            
          }
          if (visual)
          {
            mchart(merge(Lo(price),stopLine_[,1]),main="StopLine_ all")
            lapply(na.omit(stopLine_[,2]),function(x)  abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkblue" , 95),lwd=3))
            abline(v=as.POSIXct((index(last(supportPoints)))),col=col.add.alpha("green" , 95),lwd=3)
            
            browser()
            
          }
        } #beta > 0
      }#len(supportPoints)>0
      
      return(1)  #des lapply (unwichtig)
    }#if (rlpx >= 1)
  } #function()
  )#lapply
  if(F)
  {
    mP("mmmm")
    #plot(merge(Lo(price),stopLine_[,1]),main="StopLine_ Result",type="h")
    plot(Lo(price),main="StopLine_ Result")
    lines(stopLine_[,1], col="red",type="h")
    lines(Lo(price),col="black")
    lapply(na.omit(stopLine_[,2]),function(x)  abline(v=as.POSIXct(as.Date(index(x))),col=col.add.alpha("darkblue" , 95),lwd=3))
    
    #jetzt stehen die support-Lines und ich muss mich entscheiden ob ich den Kontakt als Reflexion oder Break werte
    mP("decide:  reflection or break")
    #sobald er mindestens 2 Tage >= Kanal-stop-Wert liegt, ist  ein Break.
  } 
  stopSignal = iif( Lo(price)< stopLine_[,1]  & lag(Lo(price))< stopLine_[,1]| is.na(stopLine_[,1]) ,-1,1)
  
  
  
  ##### typischer Schluss einer signal.<>- methode
  plotSigPrice(signal=stopSignal,prices=mNorm(Lo(price)),indi=list(stopLine=merge(Lo(price),stopLine_[,1])) )
  
  return(stopSignal)
}

if (F)
{
  #price = na.omit(data[[data$symbolnames[i]]]["2002::"])
  channelStop()
  mchart(merge(Lo(price),stopLine_[,1])["2009"])
  
  for(i in c(1:len(data$symbolnames)))
  {
    sym =data$symbolnames[i]
    channelStop(sym)
  }
  
  abline(v=as.POSIXct(as.Date("2008-11-21" )),col="blue",lwd=1)
  abline(v=as.POSIXct(as.Date("2009-01-23" )),col="blue",lwd=)
  abline(v=as.POSIXct(as.Date("2009-03-09" )),col="blue",lwd=3) #das Ende des 2008 -crash
  abline(v=as.POSIXct(as.Date("2009-07-13" )),col="blue",lwd=1)
}
