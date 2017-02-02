if (F)
{
Dax=Cl(data$DAX)["2007::2013-10"]
#Baue eine High-segmentierung - peak.Hi gibt die Peaks,  zbrk.Hi sagt, wann diese peaks 
#erkannt werden (etwas später)
zzHi=mZigZag2(Dax,dd=10,visual=T)

zbrk.Hi =unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",toString(x$brk),NA) }))
zbrk.Hi[zbrk.Hi==""] = NA;

plot(Dax)
peak.Hi =na.omit(unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",DateS(x$peak),NA) })))


peak.Hi =na.omit(unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",DateS(x$peak),NA) })))


lapply(zzHi,function(x) {
  brk=x$brk;peak=x$peak
  mP("Today is %s , found new %s some days ago: %s %f",brk,x$ptype,DateS(peak),peak)
})


amark(peak.Hi,"red")
amark(zbrk.Hi,"blue")


peak.Hi.i =na.omit(unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",get.Index(Dax,DateS(x$peak)),NA) })))

zbrk.Hi.i =na.omit(unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",get.Index(Dax,x$brk),NA) })))


diff(peak.Hi.i,1)

sprintf("%s::%s",lag(peak.Hi),peak.Hi)

c(1,peak.Hi.i)

#der Abstand eines jeden Index zum vorherigen Peak
k=unlist(apply(as.matrix(c(1:len(Dax))), 1, FUN=function(x,y,z) {r=last(y[y<x & y<z]); mP("%d %d",r,x);x-r},peak.Hi.i,zbrk.Hi.i))

winLen=unlist(apply(as.matrix(c(1:len(Dax))), 1, FUN=function(x,y,z) {r=last(y[y<x & z < x ]); mP("%d %d",r,x);x-r},peak.Hi.i,zbrk.Hi.i))

len(winLen)
len(Dax)

first(zbrk.Hi.i)+len(winLen)
cbind(k,winLen)

winLen=c( c(1:first(zbrk.Hi.i)),winLen) #vorne noch die ersten Werte bis zum ersten zbrk.Hi.i  vorkleben damit winLen so lang ist wie Dax

head(winLen)
len(winLen)
len(Dax)

#rollapplyr(R,FUN="ES",width=36,p=0.95,na.pad=TRUE)

r=rollRegressionXTS(Y=Dax,win=60)
mchart2(Dax,r,"reg")
signal=sign(r)
plotSigPrice(signal=signal,prices=mNorm(Dax),indi=list(signal=merge(mNorm(Dax),r )))
amark(zbrk.Lo)




r=rollRegressionXTSw(Y=Dax,winvec=winLen)
signal=sign(r)
plotSigPrice(signal=signal,prices=mNorm(Dax),indi=list(signal=merge(mNorm(Dax),r )))

mchart2(Dax,rw,"reg")

r=scaleTo(r,range(Dax))
plota(r,type="l")
lines(Dax,col="red")
}


signal.lm<-function(arg,par = mlist(win=c(200,50,300,50)),visual=F,...)
{
 #browser(mP("signal.lm"))
  clos= arg$clos   #multivariat
  win = as.integer(par$win)
  
  #die beiden Indikatoren deren Schnitt mich interessiert:
mP("win  is:  %d",win)
  r=rollRegressionXTS(Y=clos,win=win)
  signal=sign(r)
  
#  SMA.val =  bt.apply.matrix(clos, SMA, smaN)
  
  signal=sign(r)
  
  #gib das signal zurück, und auch noch die Hilfsvariablen die Du evtl. in indi.Generic() sehen willst:
  
  return(list(Signal=signal, Indi=list(merge(scaleTo(r,range(clos)), clos))))
}

if (F)
{
  prices=data.info(data)
  
  global_arg<<-list(clos=prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices))
  globalTrainLevel <<-10   
  global_objectId <<-paste("TREND","DAX","signal.lm") 

  
  x=indi.Generic("signal.lm", global_arg, par=list(win=68),visual=T, TRAINSYM =-1)
  TrainIndicator( opti="GRID", indiName = "signal.lm",  visual=T, TRAINSYM = "DAX")
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
####################
  
  channelStop()
  
  techStops(data$DAX)
  
}



############################################################################
#wenn in 0.7 aller zeitreihen die Aussichten super mies werden sprech ich von 
# ner katastropeh
#kostet super viel rechenzeit und bringt noch keine guv beim dax im europa-universe
#hier werden pro Zeitpunkt feature von allen andere zeitreihen vergleichen, wenn wenigstens
#x andere zeitreihen auch abschmieren gibts ne katastrophe
############################################################################
katastrophe<-function(Y)
{

  check.SteilerAbstieg <-function(x) #teste diesen Trigger für alle zeitreihen des Universums
  {
    s=      sign(ROC(na.omit(Y[,1]))) #von den letzten 13 Tagen gings zu 0.7% bergab
    w=13
    if( sum(tail(s,w)[,1])<=  -w*0.7 )#&& sum(tail(s,w)[,2])<0)
      {
       #browser(mP("jo"));
       res=1}
    else
      res=0
    #eintragen
    Res=x[,1]
    Res[]=res
    return(Res)
  }
  
  #in wie vielen Zeitreihen aus global_arg$dat ist die Eigenschaft "check.SteilerAbstieg" erfüllt
  res=last(m.apply(global_arg$dat, check.SteilerAbstieg ,frame=  fromToS(Y)))  #iteriere über WP  
  nn= len(global_arg$dat$symbolnames)
  sumfeature= sum(unlist(lapply(res,FUN=function(x) nval(last(x))))) / nn * 100

  if (sumfeature != 0) 
    {
     mP("kata %f %s %f",sumfeature,DateS(last(Y)),sumfeature)
     res = T 
     browser()
    }
  else
     res=F
 
 return(res)
}
##############################################################################################
##############################################################################################

signal.MA.1<-function(arg, par = mlist(zlemaN=c(3,3,20,10), slow=c(40,20,120,20), fast=c(40,20,120,20)),visual=F,...)
{
  versuch = 1  #statemachine
#  versuch=3  #Faber
  price=arg$clos
  
  if (versuch==1)    #ein vollbrauchbares system
  #geglätteter Kurs  
  {
   # browser()
  fast=as.integer(par$fast)
  zlem= bt.apply.matrix(arg$clos, ZLEMA, n=as.integer(par$zlemaN))
  Ma = runMax(arg$clos,n=as.integer(par$slow))
  Mi = runMin(arg$clos,n=as.integer(par$slow))
  
#  signal = iif(zlem <= lag(Mi,30),-1,1)
  
  pos <<- 0
    
  #ein Signal-Geber der als statemachine funtioniert (pos ist der state)  
  gib.signal<-function(Y) #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    letztes=shape(Y)
    YY = Y[,1]
    zlem=nval(Y[letztes,1]); Ma=nval(Y[letztes,2]);Mi=nval(Y[letztes,3]); altesMi=nval(Y[1,3]);altesMa=nval(Y[1,2])
  
    alpha=0; beta=0
    
    if (!is.na(first(YY))) #berechne die reg-gerade um altesMi noch anzuheben ...
    {
      x=cbind(Intercept=1,c(1:shape(YY))); y=coredata(YY); lmfit=lm.fit(x=x,y=y)
      beta=coef(lmfit)[2]    
      alpha =last(lmfit$fitted.values)- first(lmfit$fitted.values)  
    }
    if (is.na(altesMi) || is.na(Mi))
      return(pos)
    
    if (pos==1)
    {
      if (beta > 0.00)  #(altesMi-alpha)  nur wenn beta nicht sehr klein ist...
        ref.altesMi=altesMi-alpha
      else
        ref.altesMi = altesMi
      
      signal=ifelse(zlem <= ref.altesMi,0,1)
    
      #if (katastrophe(Y))    signal=0   #kostet viel rechenzeit ! und bringt nichts
    }
    else
    {
      signal =0
      
      if (pos==0)
        if (beta > 0)
          signal= ifelse(altesMi < Mi || altesMa <Ma  ,1,0)  #&& sign(beta) >=0
    }
    
    pos<<- signal
    return(signal)
    
  } #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # browser()
    
  signal=rollapplyr(merge(zlem,Ma,Mi), fast, gib.signal, by.column = F)
  return(list(Signal=signal, Indi=list(ma=merge(zlem,Ma,Mi))))
}
  
  #baue eine reihe technischer indikatoren (z.b. reg-Winkel, sma-dif, pos, maxdd, rsi, roc(5))
  #- trainire damit monatlich  ein glm  oder  pnn oder svm und hol dir vom klassifier das signal
  if (versuch==2)
{
  lomi= runMin(Lo(na.omit(price)),n=20) #lag mäßig ok
  himi= runMax(Hi(na.omit(price)),n=20)
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
  
  #return(list(Signal=signal, Indi=list(ma=merge(zlem,Ma,Mi,MaMi))))
  return(list(Signal=signal, Indi=list(ma=merge(zlem,slow,fast),beta=beta,alpha=alpha)))
}
  if (versuch == 3)  #signal.Faber .. etwas anders implementiert
  {
    
    perf <- na.omit(merge(monthlyReturn(price)))
    colnames(perf) <- colnames(price)
   # mchart(mRendite(na.omit(perf)))
                          
    
    #get cumulative returns for moving average
    cumul <- as.xts(apply(perf+1,MARGIN=2,cumprod),order.by=index(perf)) #mRendite(na.omit(perf)
  #  mchart(cumul)
    #do 10 month Mebane Faber style system
    ma <- lag(merge(runMean(cumul[,1],n=10)))#,runMean(cumul[,2],n=10)),k=1)
    #apply 50% allocation to each fund if they are > 10 month moving average
    ma.perf <- as.xts(apply(as.matrix(cumul>ma) * as.matrix(perf)/2,
                            MARGIN=1,sum),
                      order.by=index(perf))
    ############# krass
    signal_=as.xts( iif(coredata(cumul>ma),1,0),order.by=index(perf))
    #wandel die monats-daten aus signal_ wieder in tagesdaten nach signal
    signal=price;signal[]=NA
    signal[index(signal_)]=signal_
    signal[] =apply(coredata(signal), 2, m.ifna.prev)
    
    return(list(Signal=signal, Indi=list(sig=signal)))
    
    if (F)
    {
    ma.each <- as.xts(as.matrix(cumul>ma) * as.matrix(perf),
                      order.by=index(perf))
    
    mchart(mRendite(na.omit(ma.each)))
    
    #######
    #get 8 month RSI; randomly picked 8; no optimization
    rsi<- lag(merge(RSI(perf[,1],n=8),RSI(perf[,2],n=8)),k=1)
    #allocate between vbmfx and vfinx based on highest RSI
    rsi.perf <- ifelse(rsi[,1]>rsi[,2],perf[,1],perf[,2])
    rsi.each <- as.xts(as.matrix(rsi>50) * as.matrix(perf),
                       order.by=index(perf))
    
    mchart(mRendite(na.omit(rsi.perf)))
    #####
    
    
    #signal<-merge(apply.rolling(ret[,1],FUN="Omega",width=25),apply.rolling(ret[,2],FUN="Omega",width=25))
    omega <- lag(rollapplyr(perf, width=6, FUN="Omega", by.column=T, align = "right"))
    omega[!is.finite(omega)]<-0
    
    #add omega as another allocation method
  #  omega <- lag(merge(apply.rolling(perf[,1],width=6,by=1,FUN=Omega),
  #                     apply.rolling(perf[,2],width=6,by=1,FUN=Omega)),
  #               k=1)
    #if omega >= 1 then apply 50% allocation
    omega.perf <- as.xts(apply(as.matrix(omega>=1) * as.matrix(perf)/2,
                               MARGIN=1,sum),
                         order.by=index(perf))
    omega.each <- as.xts(as.matrix(omega>=1) * as.matrix(perf),
                         order.by=index(perf))
  
    mchart(mRendite(na.omit(omega.each)))
    }
  }
}
if (F)
{
  prices=data.info(data)
  global_arg<<-list(clos=prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices))
  globalTrainLevel <<-10   
  global_objectId <<-paste("TREND","SG2R","signal.MA.1") 
  
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM ="DAX")
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=190),visual=T, TRAINSYM =-1)
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM ="SG2R")
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM =-1)
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM ="SXEBP")
  x=indi.Generic("signal.MA.1", global_arg, par=list(zlemaN=10,slow=90,fast=10),visual=T, TRAINSYM ="REX")
  
  TrainIndicator( opti="GRID", indiName = "signal.MA.1",  visual=T, TRAINSYM = "DAX")
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
  ####################
  
  channelStop()
  
  techStops(data$DAX)
  
}

############################################

library(parallel)

###see if you have multiple cores

detectCores()

###indicate number of cores used for parallel processing
if (detectCores()>1) {
  cl <- makeCluster(detectCores()-1)
} else cl <- NULL


#################################################################################
#Kombination aus:    Faber Omega lm
#################################################################################

signal.Faber<-function(arg, par = mlist( sma.w=c(200,150,350,10)),visual=F,...)
{
  price=arg$clos
  symbols = toupper((colnames(arg$clos)))
  frame=fromToS(arg$clos)
  sma.w = as.integer(par$sma.w)
 
  
 # browser(mP("Faber %s",symbols))
  cumul = m.apply(arg$dat, onlysym=symbols, function(x) { mNorm(Cl(x)) } ,frame=frame)
  ma =  m.apply(arg$dat,onlysym=symbols, function(x) { SMA(Cl(x),n=sma.w) } ,PreFun=mNorm,frame=frame)  

  #umstieg auf monats daten
  month.ends = endpoints(cumul, 'months')
  month.ends = month.ends[month.ends > 0]    
  
  #iif funktioniert nicht für multi spalten kurse .... signal=iif(cumul > ma,1,0)
  
  signal.month =foreach(x=colnames(cumul),.combine="cbind") %do% 
     { 
       ##FABER
       a=cumul[month.ends,x]  #auf monatsdaten
       b=ma[month.ends,x]
       res.faber=iif( a > b,1,0)
       #plot(b);       lines(a,col="blue")
     
       
       ###LM #####
       dolm <-function(Y){
         x1=cbind(Intercept=1,c(1:shape(Y))); y1=coredata(Y); lmfit=lm.fit(x=x1,y=y1)
         beta=coef(lmfit)[2]    
         last.fitted =nval(last(lmfit$fitted.values) ) 
         return(  c(last.fitted,beta)  )}
    
       #------- browser(mP("lm"))
       
       win=12
       slope90=rollapplyr(a, win, dolm, by.column = F)
       #pick Dir nur die Steigungen raus 
       beta=slope90[,seq(2,ncol(slope90),2)]
       last.fitted=slope90[,seq(1,ncol(slope90),2)]
      
       #plot(last.fitted);lines(a,col="blue")
       #plot(beta);lines(a,col="blue")
       #plot(runSum(a-last.fitted,5));plot(a)
       #plot(a)
       res.lm = iif(beta  > 0 & runSum(a-last.fitted,3) >=0,1,0) #0.5778
       #res.lm = iif(runSum(a-last.fitted,5) >0 ,1,0)
       #####
       
       ## OMEGA
       omega <- rollapplyr(na.omit(mROC(a)), width=6, FUN="Omega", by.column=T, align = "right") #width=6 ist optimal
       omega[!is.finite(omega)]<-0
       res.omega = iif( omega >= 1,1,0)
   
       #Entscheidungs-Regeln
       #die notierten Gütekriterien sind  calmar für das Europa-Universum (2002-mitte 2013)
       
       #res=iif(res.faber==1 & res.omega == 1,1,0)  #0.5442
       
       #res=iif(res.faber==1 | res.omega == 1,1,0)  #0.4225
       #res=iif(res.faber==1 & res.omega == 1 ,1,0)  #0.5442
       #res= res.faber # 0.466717
       #res = res.omega #0.493976
       #res = res.lm # 0.5778
       #res=iif(res.faber==1 & res.omega == 1 & res.lm ==1,1,0)  #0.652  ein cherrypicker .. verschenkt viel hat aber sensationell geringen draw-Down
       res=iif((res.faber==1 & res.omega == 1 )| res.lm ==1,1,0) #0.537
  
       #lustiger Merkmalsvektor .. für den Lernalgorithmus
       cbind(res, res.faber,res.omega, res.lm, beta, runSum(a-last.fitted,3), mROC(a),mROC(b),a-b)
     }
  
  #integriere die monats-signale in die Tagessignale
  signal=price;signal[]=NA  #xts-template
  signal[index(signal.month),]=signal.month[,1]
  signal[] =apply(coredata(signal), 2, m.ifna.prev)  #auffüllen auf Tagesdaten
  
  browser(mP("faber--- Training"))
  library(pnn)#The program is delivered with four functions - learn, smooth, perf and guess 
  set.seed(1)
  
  features=signal.month[,-1]
  colnames(features)= c("res.faber","res.omega", "res.lm", "beta", "runSum_a-last.fitted_3", "mROC_a","mROC_b","a-b")
  #Das Target ist extern schon vorbereitet worden
  if (!exists("Train.signal"))
     browser(mP("usage bug "))
  
  
  train.data = data.frame(na.omit(merge(na.omit(Train.signal), na.omit(features))))
  write.csv(train.data,"MData/TrainData.csv")
  #train.data = read.csv("MData/TrainData.csv")         
  #monatsdaten
  #train.data[,1]= iif(train.data[,1]==1,"L","")
  
  pnn <- pnn::learn(train.data)
  
  pnn$set[1:shape(train.data), ] #set, the training set.
  pnn$category.column#  category.column, the column index of categories.
  
  pnn$categories #categories, the list of found categories.
  pnn$k #the number of variables.
  pnn$n#n, the number of observations.
  pnn <- pnn::smooth(pnn, sigma = 0.72)
  pnn <- pnn::smooth(pnn, sigma = 0.3106217)
  
  #We can now evaluate the performance of our Probabilistic neural network.
  pnn <- perf(pnn)
  pnn$observed
  pnn$guessed
  pnn$success
  pnn$fails
  pnn$success_rate
  #The Bayesian information criterion.
  pnn$bic
  ###################################
  test.data=train.data[,-1]
  test.now = as.numeric(test.data[35,])
  guess(pnn, test.now)
  #following uses a genetic algorithm to find the best sigma value. You can have a look to the message generated by the 'rgenoud' package during the optimization process.
  pnn <- pnn::smooth(pnn)
  
  
  
  
  plotSigPrice(signal=Entry.signal[train.frame,],prices=mNorm(price[train.frame,]),indi=list(stopLine=merge(price[train.frame,]) ))
  
  
  return(list(Signal=signal, Indi=list(sig=merge(ma,price[,1]))))
  ##########################################################################
  
    browser()
    #apply 50% allocation to each fund if they are > 10 month moving average
    #ma.perf <- as.xts(apply(as.matrix(cumul>ma) * as.matrix(perf)/2,
    #                        MARGIN=1,sum),
    #                  order.by=index(perf))
  
  ############# krass
  
  signal_=as.xts( iif(coredata(cumul>ma),1,0),order.by=index(perf))
  
  #die Trainings-Signale
  
  if (F) 
 {
    signal_=as.xts( as.numeric(pnn$guessed)-1,order.by=as.Date(rownames(train.data)))
    #View(signal_)
    #wandel die monats-daten aus signal_ wieder in tagesdaten nach signal
    signal=price;signal[]=NA
    signal[index(signal_)]=signal_
    signal[] =apply(coredata(signal), 2, m.ifna.prev)
    
    #head(signal_["2001::"],100)
    #head(signal_[])
    #amark("2003-05-30",col="blue")
  
    View(train.data)
    plotSigPrice(signal=signal,prices=mNorm(price[train.frame,]),indi=list(stopLine=merge(price[train.frame,]) ))
  ####################################################
    library(rattle)
  rattle()  
  }
    
    return(list(Signal=signal, Indi=list(sig=signal)))
  
}

if (F) #MM_PepareData
{
  prices=data.info(data)
  
  global_arg<<-list(clos=prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices))
  globalTrainLevel <<-10   
  global_objectId <<-paste("TREND","SG2R","signal.Faber") 
  
  system.time(indi.Generic("signal.Faber", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =0))
  system.time(indi.Generic("signal.Faber", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX"))
  
  TrainIndicator( opti="GRID", indiName = "signal.Faber",  visual=T, TRAINSYM = "DAX")
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
  ####################
  #################### Trainings-Daten für beaufsichtigtes Lernen von Entries 
  ####brerechnen:
  
  price=na.omit(Cl(data$DAX))
  
  price=data$prices
  Entry.signal<<-rollapplyr(price, 1, roll.Target, by.column = T,Prices=price, maxDD=10,minEQ=20)
  #man kann das Trainingssignal nur bis zur letzten Long-Position benutzen .. am rechten Rand wirds ungewiss weil minEQ nicht mehr geschafft wird.
  rechter.Rand= DateS(last(Entry.signal[Entry.signal>0]))
  train.frame = sprintf("::%s",rechter.Rand)
  #damit sind die Trainings-Soll-Daten vorbereitet
  Train.signal<<-Entry.signal[train.frame,];  colnames(Train.signal)=c("DAX")
  
  #vorsicht: kein echter pos-plot  - sondern ein entry-plot
  plotSigPrice(signal=Entry.signal[train.frame,],prices=mNorm(price[train.frame,]),indi=list(stopLine=merge(price[train.frame,]) ))
  
  
  ####################################################################################
  channelStop()
  
  techStops(data$DAX)
  
}
