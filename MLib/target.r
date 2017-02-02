###############################################################################################
#allgemeine ASM-Methoden die zu fachspezifisch sind, als dass sie nach InputConfig gehören
#siehe MM_MAIN
###############################################################################################
# Baue alle Target - und gib sie in eine Liste (soll nach  data$multiTarget )
multiTarget<-function(visual=T)
{ 
  #mit smoothing - methode    
  Target0  <- compute.Target.by.method(data=data, TargetMethod=0, visual=visual,glaettung=5) 
  Target0s <- compute.Target.by.method(data=data, TargetMethod=0, visual=visual,glaettung=2) 
  
  #nimm die Richtung aus dem Forecast von w Tage,  lösch dananch die Trades die nicht wenigstens den
  #minReturn  bringen oder weniger als min.duration viele Tage dauern.
  
  Target_1 <-compute.Target.by.method(data=data, TargetMethod=-1, w=10 , minReturn=0.1, min.duration=20,glaettung=5,visual=visual)
  
  Target_1s <-compute.Target.by.method(data=data, TargetMethod=-1, w=10 , minReturn=0.05, min.duration=20,visual=visual)
  
  #sehr gut - nun auch parallelisiert
  Target1 <- compute.Target.by.method(data=data, TargetMethod=1,maxDDtoleranz=0.16,minReturn=0.05,visual=visual)
  
  return(list(Target0=Target0,Target0s=Target0s,Target_1=Target_1 ,Target_1s=Target_1s,Target1=Target1))
}

if (F)
  data$multiTarget <-  multiTarget()
##########################################################################

#selektiere aus data$multiTarget den zu sym passenden Targetvector

select.multiTarget <-function(data,sym)
{
  res.target= foreach(target = data$multiTarget, .combine = "merge" ) %do%
{ target.sym=target[,sym]  }

colnames(res.target)<-paste(sym,names(data$multiTarget),sep="_")
res.target
}


if (F)
{
  mt=select.multiTarget(data, data$BENCH)
  head(mt)
}
#..........................................................................
############################ verdichte alle Target-Vektoren in einer Summe

select.multiTarget.sum <-function(data,sym)
{
  res.target.1= foreach(target = data$multiTarget, .combine = "merge" ) %do%
{ target.sym=target[,sym]  }

res.target=res.target.1[,1]
res.target[]=rowSums(res.target.1)
colnames(res.target)<-paste(sym,"sumTarget",sep="_")
res.target
}

if (F)
{
  mt=select.multiTarget.sum(data, data$BENCH)
  head(mt)
  mchart(merge(mt,0))
  
  plotSigPrice(signal=sign(mt),prices=data$prices[,data$BENCH])
}


#######################################################################################

#siehe Arbeitsbuch
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



##########################################################################
#methode 1

###########################################################################
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

wrap.compute.Target<-function(priceCol,maxDDtoleranz,minReturn)
{
  PeakPos__<<-0;lastPos__<<-0;eqPos__<<-0;
  #priceCol=prices[,sym]; 
  cp = try(compute.Target(priceCol,maxDDtoleranz,minReturn))
  if(inherits(cp, 'try-error')) 
  {
    mP("bug at computeTarget ")
    res=priceCol; res[]=0
    return(res)
  }
  cp$ContinuePos
}

#alternative zur seriellen implementierung in now.r
par.compute.Targets<-function(prices, maxDDtoleranz=0.16, minReturn=0.05)
{  
  sfExport("wrap.compute.Target")
  
  global_T  <<-NULL
  # browser(mP("xxxxxx"))
  #compute.Target() gibt immer eine ganze Liste von Vektoren raus             
  T__<<-sfLapply(colnames(prices), fun=function(sym)wrap.compute.Target(prices[,sym],maxDDtoleranz=0.16, minReturn=0.05))
  mP("... now build global_Targets ...")
  #  browser()
  targetXts=do.call("merge", T__)
  colnames(targetXts) = colnames(prices)      
  return(targetXts)
  
  
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
  price=dax
  prices=data$prices
  compute.Target(price,0.16,0.05)
  #sfStop()
  prepare_Parallel()  
  global_Targets <<-par.compute.Targets(prices,0.06,0.005)
  showTargets(prices,global_Targets)
  
  plot(price)
  lines(price,col=(global_Targets+3),type="h")
  prices=na.omit(data$prices)
  
  prices = mNorm(data$prices[,c("USDCHF","USDGBP")])
  purePlot(prices)
  global_Targets <<-compute.Targets(prices,0.16,0.05)
  
  plot(prices[,1])
  lines(prices[,1],col=(global_Targets[,1]+3),type="h")
  
}


##########################################################################
#methode 2
##########################################################################


###############################################################################
#Zeige an, ob er an einem Tag long gehen würde, wenn er einen maxDD akzeptiert und 
#bis dahin wenigstens minEQ verdient haben will.

#Jeder Tag wird (unabhängig von einer evtl. schon eingenommenen marketpos) entschieden #Ob in den Folgekursen ein minEQ vor ein stop bei maxDD erziehlt werden kann
#das kann zu super - short-term-positions führen (weil die poslänge keine rolle spielt)
#Wichtig:  der signal=1- Bereich zeigt keinesweg die Dauer einer Sollposition !!!!

################################################################################
roll.Target_<-function(old_prices, Prices,   maxDD=10,minEQ=10)
{
  sym=colnames(old_prices)
  Prices=Prices[,sym]
  lastDay = DateS(last(old_prices))
  #if (lastDay=="2010-05-05")
  #  browser()
  lastDayPrice=nval(Prices[lastDay])
  futPrices = Prices[sprintf("%s::",lastDay)]
  LongThresh = lastDayPrice-lastDayPrice/100*maxDD
  #browser()
  stopOutAt=futPrices[which(futPrices <  LongThresh)]
  toleratedPrices = futPrices[sprintf("::%s",DateS(first(stopOutAt)))]
  bestLongEQ= (max(toleratedPrices,na.rm=T)-lastDayPrice)/ lastDayPrice*100
  #browser()
  res=ifelse(bestLongEQ >= minEQ,1,0)
  if (res==1)
  { LongUntil = DateS(last(toleratedPrices))
    #if (res==1)
    mP("%s:  %s %f %d bis %s",sym,lastDay,lastDayPrice, res, LongUntil)
  }
  return(res)
}
roll.Target<-cmpfun(roll.Target_) #compilier das Teil


if (F)
{
  Dax = na.omit(Cl(data$DAX))["2007::"]
  mchart(Dax)
  roll.Target(Dax["2008-03-04"],Dax,maxDD=10,minEQ=10 )
  roll.Target(Dax["2008-03-05"],Dax,maxDD=10,minEQ=10 )
  
  Entry.signal=rollapplyr(Dax, 1, roll.Target, by.column = T,Prices=Dax, maxDD=10,minEQ=10)
  plotSigPrice(signal=Entry.signal,prices=mNorm(Dax),indi=NULL)
  View(targetPos["2008"])
  #eigentlich bekomm ich eine (pfadabhängige) Pos-Realisierung erst durch eine weitere Transformation des signals:  immer wenn ich long geh, bleib ich long bis zum Stop  (und erst dann schau ich nach einem neuen Entry.signal ... ) - dann wird aber der Trading-Verlauf sehr abhängig von der Wahl des analysierten PriceIntervalls (weil pfadabhänig)
  ls(data)
  Dax=Cl(data$DAX)
  #bau ein data2  -env mit einem subset von symbols aus data:
  data2=new.env()
  data2$Dax=data$DAX
  data2$USDEUR=data$USDEUR
  data2$symbolnames=list("USDEUR","Dax")
  ls(data2)
  #Kristallkugel zur TargetDefinition
  Entry.signals=
    m.apply(data2,function(Dax) 
    {
      #      Dax=data2$Dax
      #    
      #browser()
      zLarge=ZigZag(na.omit(HLC(Dax)),change=10)
      
      Dax = na.omit(Cl(Dax))#["2007::"]
      plot(zLarge)
      lines(Dax,col="blue")
      deTrend=(Dax-zLarge)/Dax*100
      plot(deTrend)
      
      
      Entry.signal<<-rollapplyr(na.omit(Dax), 1, roll.Target, by.column = T,Prices=Dax, maxDD=10,minEQ=20)
      #browser()
      plotSigPrice(signal=Entry.signal,prices=mNorm(Dax),indi=NULL)
      mZ=mZigZag2(Dax=Dax,dd=50,F)
      #ein vektor mit datums-strings
      zpeak =unlist(lapply(mZ,function(x){ amark(x$peak,"green");DateS(x$peak) }))  
      #ein vector mit von-bis- datums-strings - intervalle
      frames<<- unlist(apply(cbind(c("no",zpeak),zpeak)[-c(1,len(zpeak)+1),],1,FUN=function(x){ sprintf("%s::%s",x[1],x[2])}))
      #iteriere über die Zeitintervalle in frames
      maxWinLen =1000
      
      #FeatureExtraktion:  
      allSegments=
        # sapply(frames,FUN=function(x)
        foreach(x = frames, .combine=rbind) %dopar%  #baue die segment-informationen zu einem dicken xts zusammen
{
  daxSegment = na.omit(Dax[x])
  firstD = index(first(daxSegment))
  lastD = DateS(last(daxSegment))
  lastD.brk = as.Date(lastD)+400# .. über die segment-grenze hinweg
  daxSegment = Dax[sprintf("%s::%s",DateS(first(daxSegment)),lastD.brk)]
  plot(daxSegment)
  amark(lastD,"green")
  print(x)
  #geh durch das ganze Segment - mach jedesmal eine lm vom Anfang des Segments
  #bis zum aktuellen Wert und berechen in y2 den letzten fit.lm-Wert
  #for (dx in c(1:len(daxSegment)))
  #browser()
  #gib pro Tag eine ganze Liste von Werten zurück 
  
  seg=lapply(c(1:len(daxSegment)), FUN=function(dx)
  {
    #im gleitenden Fenster von maxWinLen jeden Tag den fit.lm-Wert bei y2 berechen
    if (dx >=10)
    { 
      first.i=max(1,dx-maxWinLen)
      X=c(first.i:dx)
      Y=coredata(daxSegment[X])
      fit.lm=lm(Y~X)
      print(dx)
      y2=nval(last(fitted(fit.lm)))
      #browser()
      pred1<-predict(fit.lm,newdata=data.frame(X=c(dx)),interval="prediction",level=0.95)
      #mchart(pred1)
      #head(pred1)  # 3 spalen: (y,upper,lower)
      rsquare=summary(fit.lm)$r.square  #goodnes of fit; möglichst gut heißt nahe 1
      beta=nval(coef(fit.lm)[2])
      result=list(fit=y2,lwr=pred1[2],upr=pred1[3],rsquare=rsquare,beta=beta )
      # browser()
    }
    else
    {
      dax=nval(daxSegment[dx])
      result=list(fit=dax,lwr=dax,upr=dax,rsquare=0.5 ,beta=0)
    }
    return(result)
  })
  OAW = rbindlist(seg) ;  resSegment=as.xts(OAW, order.by = as.Date(index(daxSegment)))
  browser(mP("................"))
  mchart(resSegment);lines(daxSegment,col="blue")
  amark(lastD,"green")
  ####################################
  browser()
  features= buildFeatures(Dax,resSegment)
  
  
  
  #head(resSegment)
  #plot(resSegment[,"rsquare"])
  #mchart(resSegment[,"beta"])
  
  #lines(SMA(resSegment[,1],90),col="red")
  return(resSegment)
  # browser()        
}  
browser() 
mchart(allSegments)

lines(Dax,col="blue")
lines(ZigZag(na.omit(Dax),change=50),col="magenta")
#berechne aus den Rohdaten Features () 


#berchne die Target-Kategorien (Long,Flat)

Entry.signal<<-rollapplyr(na.omit(Dax), 1, roll.Target, by.column = T,Prices=Dax, maxDD=10,minEQ=20)
#browser()
plotSigPrice(signal=Entry.signal,prices=mNorm(Dax),indi=NULL)
#View(Entry.signal["2010"])
return(Entry.signal)
    },newName="LongEntry")  #  <--- univariate-Feature-Map fertig

}
##########################################################################################
#geh in ein pos gem. signals und bleib so lang drin bist du augestoppt wirst
#signals sind Entry-signale aus roll.Target  .. diese werden diesmal bis zum maxDD durchgeritten, bevor ein neues Entry-Signal bemerkt wird.
##########################################################################################

eval.Target_<-function( Prices, signals,   maxDD=10, visual=F)
{
  pos=0
  temp=na.omit(merge(Cl(Prices),signals)); Prices=temp[,1]; signals=temp[,2]
  N=shape(Prices)
  res = Prices ; res[]=0
  DD=res
  wlen=10
  
  library(foreach)
  
  foreach(i=1:N, .combine="cbind") %do% {
    if (i > wlen)
    {
      if (pos==0)
      {
        today=i
        mPos = sum(signals[c((i-wlen):(i-1))])/wlen#mittlere pos
        print(mPos) 
        if(mPos > 0.2)
          pos=as.numeric(signals[i-1,1])
        
        #browser()
        if (pos != 0 )
        {
          entry.date = today
          entry.price=as.numeric(Prices[i])
        }
      }
      if (pos!=0)
      {
        dd =(entry.price-Prices[i-1])/entry.price*100
        stop.me= (nval(dd) < -abs(maxDD)) 
        #print(dd)
        #browser()
        #if(as.numeric(signals[i,1])==0)
        #  stop.me=T
        if (stop.me )
          pos=0
        DD[i]=dd
      }
      
      res[i]=pos
    }
  }
  if (visual)
    plotSigPrice(signal=res,prices=mNorm(Prices),indi=list(ts=res,dd=DD))
  
  return(res)
}

eval.Target<-cmpfun(eval.Target_) #compilier das Teil

if (F)
{
  plot(Dax)
  realised.signals= eval.Target(Prices=Dax,signals= Train.signal,maxDD=10)
  #realised.signals = Train.signal
}

###########################################################################
#die Sammel-Rufmethode zu Berechnung von Targets nach unterschiedlichen Ansätzen
###########################################################################

if (F)
{
  data$prices = mNorm(data$prices)
  system.time(compute.Target.by.method(data=data, TargetMethod=0, w=10 , minReturn=0.1, min.duration=20,glaettung=5,visual=T))
  
  #nur eine Zeitreihe
  no=long.short.Target(data$prices[,"TREASURY"],w=10,min.value.pct=30,min.duration=10,visual=T)
}


#

compute.Target.by.method<-function(data, TargetMethod,glaettung=5,maxDDtoleranz=0.16,minReturn=0.05,min.duration=10,w=10,visual=T)
{
  if (len(data$Y.names)==0)  #welche preiszeitreihe soll eigentlich ge-targetet werden ?
    data$Y.names = colnames(data$prices) 
  
  if (TargetMethod==  -1)   #MM_MAIN
  {
    mP("compute.Target.by.method")
    sfExport("long.short.Target") #die methode auf die cores pumpen
    #res= long.short.Target(dax,w=10,min.value.pct=minReturn*100,min.duration=10,visual=F)  
    
    if (F) #seriell
      res<-lapply(colnames(data$prices), function(sym) 
        long.short.Target(data$prices[,sym],w=w,min.value.pct=minReturn*100,min.duration=min.duration,visual=F) )
    
    else 
      #parallel
      res<-sfLapply(colnames(data$prices[,data$Y.names]), fun=function(sym) 
        long.short.Target(data$prices[,sym],w=w,min.value.pct=minReturn*100,min.duration=min.duration,visual=F) )
    
    #names(res[[1]]) 
    #pick dir aus der jeweiligen ergebnis-liste das feld $sig heraus   
    res.sig = lapply(res, function(res.i) res.i$sig)
    #bau aus einer liste von xts- reihen breites xts
    targetXts=do.call("merge", res.sig);  colnames(targetXts) = colnames(data$prices[,data$Y.names])      
    
    if (visual)    showTargets(data$prices[,data$Y.names],targetXts,main="method -1")  
    return(targetXts)
    
  }
  else
    if (TargetMethod ==0)
    {
      res=foreach(sym=colnames(data$prices[,data$Y.names]), .combine=cbind) %do%
{ 
  
  res=fast.smoothing(p=na.omit(data$prices[,sym]),visual=visual,glaettung=glaettung)$sig
  
}
#data$Target=res
if (visual) showTargets(data$prices[,data$Y.names],res,main="method 0")
res

    }
else
  if (TargetMethod ==1)   #arbeitet mit dem targetByDD() .. ab wann lohnt es sich short, oder long zu gehen ... es gibt overlap-bereiche !!!
  {
    #res=compute.Targets(na.omit(data$prices),0.16,0.05)
    res=par.compute.Targets(na.omit(data$prices[,data$Y.names]),maxDDtoleranz=0.16,minReturn=0.05)
    if (visual)showTargets(data$prices[,data$Y.names],res,main="method 1")
    res    
  }
else
  if (TargetMethod ==2)
  {
    res=foreach(sym=colnames(data$prices[,data$Y.names]), .combine=cbind) %do%
{ Entry.signal<-rollapplyr(na.omit(data$prices[ ,sym]), 1, roll.Target, by.column = T,Prices=data$prices[,sym], maxDD=10,minEQ=20) }

if (visual)showTargets(data$prices[,data$Y.names],res,,main="method 2")
res
  }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (TargetMethod==3) #klassisch: daily-return als Target
{
  res<-  foreach(sym=colnames(data$prices[,data$Y.names]), .combine=cbind, .packages=c("quantmod")) %do%
{res=iif(lag(dailyReturn(data$prices[,sym], type='log'),-1)>=0,1,-1); colnames(res)=sym ;res}

showTargets(data$prices[,data$Y.names],res,,main="method 3")  
res
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (TargetMethod==4) #klassisch: monthly-return als Target   GUTES MODELL - auch ohne lag(-1).
{
  res  <-  foreach(sym=colnames(data$prices[,data$Y.names]), .combine=cbind, .packages=c("quantmod")) %do%
{res=iif(lag(monthlyReturn(data$prices[,sym], type='log'),-1)>=0,1,0); colnames(res)=sym ;res}

#monats-signale auf tagespositionen projezieren
temp=data$prices[,data$Y.names];  temp[]=NA;     temp[index(res),]=res;    
res=bt.apply.matrix(temp,ifna.prev)
#keine short-postionen
#res=bt.apply.matrix(res,function(x)iif(x < 0,0,x))
showTargets(data$prices[,data$Y.names],res,,main="method 4")   
res
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (TargetMethod==5) #monthly ntop-Ranking
{
  topn=5
  res<-  foreach(sym=colnames(data$prices[,data$Y.names]), .combine=cbind) %do%
{ mr=lag(monthlyReturn(data$prices[,sym], type='log'),-1);
  #  mr[mr <0]<-0
}  #zukünftige Returns
## gib Top n Ranking  matrix
# work with matrix
temp = coredata(mret)
dirMaxMin=T

rank=foreach( i = 1:nrow(temp),.combine= rbind ) %do% {
  x = temp[i,]
  o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
  index = which(!is.na(x))
  x[] = NA
  
  if(len(index)>0) {
    n = min(topn, len(index))
    x[o[1:n]] = 1
  }
  x
}
mrank=mret
mrank[] = rank
colnames(mrank)=colnames(data$prices[,data$Y.names])
mrank[is.na(mrank)]<-0
res = mrank

#fillup: monthly->daily
temp=data$prices[,data$Y.names];  temp[]=NA;     temp[index(res),]=res;    
res=bt.apply.matrix(temp,ifna.prev)

showTargets(data$prices[,data$Y.names],res,,main="method 5")
}

res
}
#compute.Target.by.method<-cmpfun(compute.Target.by.method_) #compilier das Teil

if (F)
  data$Target   <- compute.Target.by.method(data=data, TargetMethod=0)

###############################################################################
#sehr schnell
#schau w Tage nach vorn: welche Pos hätte mehr gebracht- long oder short ?
#je nach dem geh in die Pos
#MM_LST
###############################################################################
long.short.Target_<-function(p,w=20,min.value.pct=20, min.duration=1 , max.drawdown=0, visual=F,debug=F)
{
  p=p[,1]
  print(colnames(p))
  future.p = lag(p,-w)
  future.p = m.ifna.prev(future.p)
  minVal = min.value.pct*p/100;colnames(minVal) = c("minVal")
  rMax=runMax(future.p-p,n=w)
  #auch vorne Werte finden
  rMax[1:w]= sapply(1:w, function(w) max(future.p[1:w,]-p[1:w,]))
  rMin=runMin(future.p-p,n=w)
  #auch vorne Werte finden
  rMin[1:w]= sapply(1:w, function(w) min(future.p[1:w,]-p[1:w,]))
  
  #if (visual) mchart(merge(abs(rMax),abs(rMin)))
  sig=iif(abs(rMax)>=abs(rMin), 1,-1);colnames(sig)=colnames(p)
  sig.value=sig;sig.value[]=0.99
  if (debug)plotSigPrice(signal=sig,prices=log(p),indi=list(),main="pre-delete")
  
  # Calculate equity curves 
  close_ROC <- ROC(p);close_ROC[1] <- 0
  equity <- cumprod(1+close_ROC*sig)
  
  #plot(equity)
  #amark(as.Date(index(dd.tooBig[dd.tooBig])))
  #browser(mP("xxx"))
  if (min.value.pct >0)
  {
    #baue die liste alle trades
    trades=sig[abs(sig-lag(sig))>0]
    trades=rbind(trades,first(sig[sig !=0]))
    #heute realisieren
    trades = rbind(trades,xts(0,as.Date(index(last(p)))))
    #  browser(mP("goo"))
    while (T  && shape(trades)>1)
    {
      mP("%d trades",shape(trades))
      #entry und exit - kurse
      entry.p= p[as.Date(index(trades))]
      exit.p = lag(entry.p,-1)
      #tradereturns
      trade.returns = trades[,1]
      trade.returns[]=rowSums(merge(-entry.p,exit.p))/entry.p*100*sign(trades)
      sig.value= trade.returns 
      #merge( p[c(as.Date(index(entry.p))],p[as.Date(index(exit.p)))])
      
      #trade.maxDD[ p[merge(-entry.p,exit.p)] 
      #filter zu kleine trades raus, den ersten Trade nimm mit
      trades.f=trade.returns[trade.returns < min.value.pct ]
      if (shape(trades.f) < 1)
      {
        print("trade.returns   ##### >>>")
        print(trade.returns)
        
        if (debug)
        {
          mP("in %d are NO too small (%f) ",    shape(trades) ,min.value.pct)
          browser()
        }
        break
      }
      else
      {
        if (debug)
        {print("trade.returns")        
         print(trade.returns)
         mP("in %d are %d too small ",    shape(trades),shape(trades.f) )        
         browser()
        }
      }
      o.trades=order(coredata(trades.f))
      #die Datümer der trades die zu kleine sind, sortiert nach steigenden returns
      to.small.trade.dates=sapply(o.trades,function(o){ DateS(trades.f[o])})
      
      worst.trade=trades[first(to.small.trade.dates)]
      worst.trade.i=get.Index(prices=trades,DateS(worst.trade))
      #entferne nach einander die zu kleinen trades  - nach jedem Entfernung gibts neue Trades und TradeEquities
      trades.f = trades[-worst.trade.i]
      
      #trades.f=rbind(first(trades),trades[trade.returns > min.value.pct ] )
      #doppelte trades entfernen  (anlog bt.exrem())
      trades.ff=trades.f[abs(trades.f-lag(trades.f))>0]
      trades.ff=rbind(first(trades.f[trades.f !=0]),trades.ff)
      
      trades = trades.ff
    } #while
    
    #......................................... das ganze kürzen noch mal für ein anderes kriterium
    
    if (min.duration > 1)  #wenigstens min.duration  viele wochenTage muss der trade halten
    {
      mP("trade-duration  ################################## >>>")
      # debug=T
      while (T  && shape(trades)>1)
      {
        mP("%d trades",shape(trades))
        #entry und exit - kurse
        entry.p= p[as.Date(index(trades))]
        exit.p = lag(entry.p,-1)
        #tradereturns
        trade.returns = trades[,1]
        trade.returns[]=rowSums(merge(-entry.p,exit.p))/entry.p*100*sign(trades)
        sig.value= trade.returns 
        #tradeduratins  
        trade.duration = rollapply(trade.returns,width=2,FUN=function(dat) as.Date(index(dat[2]))-as.Date(index(dat[1])))
        trade.duration = lag(trade.duration,-1)
        trade.info=merge(trades,trade.duration,trade.returns);colnames(trade.info)=spl("pos,duration,return")
        print(trade.info)
        #filter zu kurze trades raus, den ersten Trade nimm mit
        trades.f=trade.duration[trade.duration < min.duration ]
        if (shape(trades.f) < 1)
        {
          print("trade.duration   ##### >>>")
          print(trade.duration)
          
          if (debug)
          {
            mP("in %d are NO too short (%f) ",    shape(trades) ,min.value.pct)
            browser()
          }
          break
        }
        else
        {
          if (debug)
          {
            
            new.sig = sig; new.sig[]=NA
            new.sig[as.Date(index(trades)),] = coredata(trades) 
            new.sig =m.ifna.prev(new.sig)
            
            plotSigPrice(signal=new.sig,prices=log(p),indi=list(),main="post delete log(eq)")
            
            print("trade.duration")        
            print(trade.duration)
            mP("in %d are %d too short ",    shape(trades),shape(trades.f) )  
            
            browser()
          }
        }
        o.trades=order(coredata(trades.f))
        #die Datümer der trades die zu kleine sind, sortiert nach steigenden durations
        to.small.trade.dates=sapply(o.trades,function(o){ DateS(trades.f[o])})
        
        worst.trade=trades[first(to.small.trade.dates)]
        worst.trade.i=get.Index(prices=trades,DateS(worst.trade))
        #entferne nach einander die zu kleinen trades  - nach jedem Entfernung gibts neue Trades und TradeEquities
        trades.f = trades[-worst.trade.i]
        
        #trades.f=rbind(first(trades),trades[trade.returns > min.value.pct ] )
        #doppelte trades entfernen  (anlog bt.exrem())
        trades.ff=trades.f[abs(trades.f-lag(trades.f))>0]
        #bt.exrem(trades.ff)
        trades.ff=rbind(first(trades.f[trades.f !=0]),trades.ff)
        
        trades = trades.ff
      } #while
      
    }
    if (max.drawdown > 0)
    {
      #jetzt hab ich die Anzahl der Trades reduziert - evtl. haben jetzt einzelne Trades aber einen ekelhaft großsen drawdown - dann muss ich jetzt ein tradesegment am Ort seines maxDraw-Downs in 2 Trades splitten
      sag("max.drawdown not yet implemented")
    }
    
    #u.dates=unique(as.Date(index(trades.ff)))
    
    #aus den reduzierten trades wieder signale machen
    new.sig = sig; new.sig[]=NA
    new.sig[as.Date(index(trades)),] = coredata(trades) 
    new.sig =m.ifna.prev(new.sig)
    #equity <- cumprod(1+close_ROC*sig)
    if (debug)plotSigPrice(signal=new.sig,prices=log(p),indi=list(),main="post delete log(eq)")
    
    sig = new.sig
    
  }
  else
    sig[is.na(sig)] <-0
  
  colnames(sig.value) = sprintf("sig.value.%s",colnames(p)[1])
  colnames(rMax)=c("rMax"); colnames(rMin)=c("rMin")
  #if (visual)purePlot(abs(rMax),abs(rMin))
  #browser(mP("long.short.Target()"))
  if (visual)
    plotSigPrice(signal=sig,prices=p,indi=NULL,main="log(eq)",no.eq=T)
  
  
  mkList(sig, sig.value)  
}
long.short.Target <- cmpfun(long.short.Target_) #compilier das Teil


entry.of <- function(sig,trades,xi) 
{
  #browser(mP("xi:%d",xi))
  head(sig)
  head(trades)
  DateS(last(trades[as.Date(index(trades)) <= as.Date(index(sig[xi]))]))
}
if (F)
{
  
  #gehe in die pos die zu Kursbewegung der kommenden w Tage am besten passt
  res= long.short.Target(dax,w=10,min.value.pct=0,visual=T)
  #reduziere die Tradeanzahl durch vergrößereung von w
  res= long.short.Target(dax,w=60,min.value.pct=0,visual=T)
  #reduziere die Tradeanzahl in dem du sagst:  wechsel die Pos nur, 
  #wenn die neue Pos wenigstens min.value.pct  return in w tagen bringt.
  res= long.short.Target(dax,w=10,min.value.pct=5,visual=T)
  res= long.short.Target(dax,w=10,min.value.pct=10,visual=T)
  #jetzt kann die min-duration eines trades noch super kurz sein  
  res= long.short.Target(dax,w=10,min.value.pct=20,visual=T)
  
  res= long.short.Target(dax,w=10,min.value.pct=20,min.duration=30,visual=T)
  res= long.short.Target(dax,w=10,min.value.pct=20,min.duration=10,visual=T)  
}  

###########################################################################

mP("########### load target.r")
#source("Mlib/Now.R")

if (F)
  list_R_functions('MLib/target.r')
