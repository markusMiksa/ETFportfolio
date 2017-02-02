#############################################################################################
#Lade ein Dax-Universum
#Berechne die target-Signale mit roll.Target
#Berechne Indikatoren - auch als xts- Matrix
#Beurteile die Relevanz von Indikatoren als Feature für ein Timing-Modell
#Arbeite dabei intensiv mit rattle()  ... siehe auch Arbeitsbuch.doc

#############################################################################################



if(F)
{
  loadDax()
  show.Train.signal(data$Train.signal)
  ############# willst Du mit pnn die Relevanz von featuren erkunden ?
  test.feature.pnn()
  ########################### oder lieber einen Trainingsdatensatz erstellen den du
  #dann mit rattle erkunden kannst 
  #Schau auch bei #MM_FEATURES
  train.data <<-NULL  #hierhin werden die train-daten geschrieben 
  test.feature.pnn(just_build_data=T)
  dim(train.data)
  colnames(train.data)
  ######################### 
  library(rattle)
  rattle()
  
}

#################################################################
test.feature.pnn<-function(just_build_data=F)
{
  #mit der Benchmark soll angefangen werden
  #symnames= data$symbolnames
  #dax.i = which(symnames=="GDAXI")
  #symnames= c("GDAXI",symnames[-dax.i])
  #data$symbolnames = symnames # das verändert die Verarbeitungsreihenfolge in m.apply
  
  sym.i<<-0
  Res=m.apply(data,function(P){
    sym.i<<- sym.i+1 
    sym= unlist(strsplit(colnames(Cl(P)),"\\."))[1]
    mP("%d %s ",sym.i, sym)
    target=data$Train.signal[,sym.i]  
    
    # Anonymisieren damit ein lange train.data entstehen kann:
    #(Voraussetzung: für Daten mit sysinvestor geladen wurden)
    colnames(P)=c("P.OPEN","P.HIGH","P.LOW","P.CLOSE","P.VOLUME","P.ADJUSTED")
    colnames(target)=c("P")
    
    p=Cl(P[fromToS(target)])
    
    
    mP("go")
    #  plot(10000*rollRegressionXTS(p,win=90)) #überflüssig
    #features=SMA(p,200)-runMax(p,200)  #OK
    #features=merge(SMA(p,200)-runMax(p,200),SMA(p,90)-runMax(p,200),) #Klasse  0.8768
    
    features=merge(SMA(p,200)-runMax(p,200),SMA(p,90)-runMax(p,200),SMA(p,30)-runMax(p,200))#Klasse   0.9024
    
    features=merge(SMA(p,200)-runMax(p,200),SMA(p,90)-runMax(p,200),SMA(p,30)-runMax(p,200), SMA(p,10)-runMax(p,200))#0.9256
    
    features=merge(SMA(p,200)-runMax(p,200),SMA(p,90)-runMax(p,200),SMA(p,60)-runMax(p,200),SMA(p,30)-runMax(p,200), SMA(p,10)-runMax(p,200)) # 0.94
    
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),TDI(p)/100)#0.8616 mit TDI:0.8976
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),ADX(P)/100)#0.8616 mit ADX:0.8784  
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),aroon(LoHi(P))/100) #0.8616 mit aroon:  0.9208  #SUPER
    
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),CCI(HLC(P))/100) #0.8616 mit CCI 0.8928  
    #browser()
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),VHF(p)) #schrott  
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),ATR(HLC(P))) #SUPER 0.9616  
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),cbind(DVI(p)[[1]],DVI(p)[[2]],DVI(p)[[3]]) )  #schrott
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),SMI(HLC(P))) #Schrott 
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),BBands(HLC(P))) #Super 0.9576
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),DEMA(p,n=30,wilder=T))#Super 0.924 
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),ZLEMA(na.omit(p),n=30)) #Super 0.9416
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),CLV(HLC(P))) #Schrott
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),CMO(p)) #Schrott
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),DonchianChannel(HLC(P)[,-3],n=30)) #SUPER 0.94
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),DPO(p,n=30)) #0.89 mäig
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),ROC(p,n=30)) #SCHROTT
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),MACD(p)) # Super 0.9264
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),runMean(p,n=30)) # 0.9304 
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),runMedian(p,n=30)) #mäßig 0.9192
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),runSD(p,n=30)) # #mäßig 0.9144 
    #browser()
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),runCov(p,lag(p,30),n=30)) # S Schrott
    #  features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),runCor(p,lag(p,30),n=30)) # S
    # features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),runMAD(p,n=30)) # S
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),wilderSum(p,n=30)/2000) # S
    #???????? features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),KST(p)) 
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),TRIX(p)) #S
    # features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),chaikinVolatility(HLC(P)[,-3],n=30)) #S
    # features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),mNorm(volatility(P,calc="parkinson"))) #S
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),SAR(HLC(P)[,-3])) #0.9256
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),runPercentRank(p,n=30)) #S
    #features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),VHF(p,n=30)) #S
    features=merge( SMA(p,200)-SMA(p,90),SMA(p,90)-SMA(p,60),williamsAD(HLC(P)))#Super -- manchmal 0.944
    #features=CUDIndi(p,50)  #schrott   
    #features=RSI(p,2)  #schrott
    ##schrott
    #features=rollRegressionXTS(p,win=90) #schrott
    #features=sign(features)  #schrott
    #features =indi.lm(p,lmwin=20)-p  ##Schrott
    #####################################   best of  TTR:
    #MM_FEATURES
    
    # p=mNorm(p) #NEU   nicht gut !!!!
    #dim(p)
    #die beste feature-matrix ----------------------------------------------------------
    features=merge(SMA(p,200)-runMax(p,200), SMA(p,90)-runMax(p,200), SMA(p,60)-runMax(p,200), SMA(p,30)-runMax(p,200), SMA(p,10)-runMax(p,200), # 0.94
                   #aroon(LoHi(P))/100 , #unwichtiger wie nach PNN noch gedacht
                   ATR(HLC(P))[,-c(1)],  #produziert das wichtige trueHigh und das unwichtige tr
                   BBands(HLC(P))[,-4],  #pctB  wird nicht gebraucht
                   ZLEMA(na.omit(p),n=30),
                   DonchianChannel(HLC(P)[,-3],n=30),   
                   #MACD(p),
                   runSD(p,n=30),
                   SAR(HLC(P)[,-3]) ,
                   williamsAD(HLC(P)),
                   rollRegressionXTS(p,win=90),rollRegressionXTS(p,win=200)
    )#
    
    features = na.omit(features)#[fromToS(p)]
    
    if (sym=="GDAXI")  #es bringt nichts .. hier noch Features vom Dax beizumischen
    {
      #bench.features <<-features
      #  colnames(bench.features)<<-  unlist( lapply(colnames(bench.features), FUN= function(x)   return(sprintf("B.%s",x)) ))
      #fromTo(features)
      bench<<- Cl(p)
      bench.features <<-merge(runCor(p,bench) , ROC(bench, n=5))
    }
    
    features=na.omit(features)
    lag.features=  lag(features,20) #1 (taugt nicht) 20 ist besser wie 30
    
    colnames(lag.features)=  unlist( lapply(colnames(lag.features),
                                            FUN= function(x)   return(sprintf("LAG.%s",x)) ))  
    if(just_build_data)
    {
      if (T || is.null( train.data))
        train.data <<- data.frame(na.omit(merge(target, features,lag.features)))#,bench.features)))#,bench.features #lag.features,
      else
        train.data <<- rbind(train.data, data.frame(na.omit(merge(na.omit(target), na.omit(features)))))
    }
    
    
    # colnames(BBands(HLC(P))[,-4])
    print(colnames(train.data))
    print(len(colnames(train.data)))
    plot(p,main=sym);lines(bench,col="red")
    browser(mP("teste rattle"))
    #library(rattle)
    #  rattle()
    
    #alle:0.96
    #ohne:  macd,williams: 0.9664
    #aroon,ATR,...SAR: 0.9647887
    ############################## >>>> forecast -features ...
    #load.packages('forecast,fGarch,fArma,e1071')
    
    #sample = Cl((p["2010::2012"]))  
    #features=mFaberIndi(p,200,"days")
    if (!just_build_data)
    {
      Res = indi.test.pnn(target,features)
      print("---------------------------")
      #Res=0.2
      res=first(p)
      res[]<-Res
      print(res)
      #res=plotSigPrice(signal=signal,prices=p)  
      #res = lag(signal)*na.omit(ROC(p))
      #browser()
      res
    }
    else
      Res=0
  })
  return(list(res=Res, Res=mean(Res) ))
}

########################################################################################
#schau dir die lm-forecasts an-sowohl durch den preis (grün) als auch durch die runMax(blau)
#und runMin (rot)
########################################################################################
indi.lm<-function(p,maxmin=3, lmwin=10,visual=F)
{
  #upper = bt.apply.matrix(p, runMax, n=maxmin)
  #lower = bt.apply.matrix(p, runMin, n=maxmin)
  
  if (visual)
  {
    plot(p);lines(p,lwd=2);lines(upper,col="lightblue");lines(lower,col="magenta")
  }
  
  dolm<-function(Y,n.ahead=1) 
  {
    # browser(mP("dolm"))
    x=as.matrix(c(1:shape(Y))); y=coredata(Y)
    fit=lm(y~x) 
    res=last(predict(fit,n.ahead=n.ahead),n.ahead)
    Res=xts(coredata(res), as.Date(index(last(Y)))+1:shape(res))
    if (visual)
    {
      lines(Res)
      print(Res)
    }
    colnames(Res)=colnames(Y)
    last(Res) #siehe auch last(Res)
  }
  
  ahead=2
  predict=rollapplyr(p, lmwin, dolm, by.column = T,n.ahead=ahead*3)
  
  if (visual)
    lines(predict,col="green")
  if (F)
  {
    browser()
    predict.u=rollapplyr(upper, lmwin, dolm, by.column = T,n.ahead=ahead)
    lines(predict.u,col="blue")
    predict.l=rollapplyr(lower, lmwin, dolm, by.column = T,n.ahead=ahead)
    lines(predict.l,col="red")
  }
  
  
  predict
  
}
if (F)
{
  
  signal =indi.lm(sample)-sample  ##lm.forecast- price today ist das signal
  signal[is.na(signal)]<-0
  signal = iif(signal > 0,1,0)
  ps=p;ps[]=NA
  ps[as.Date(index(signal))-1]=signal[]
  res=plotSigPrice(signal=signal,prices=p,indi=list())  
}


###############################################################
#produzier ein xts wo für jedes model der monats forecast in einer spalte liegt
###############################################################
monthly.forecast<-function()
{
  # mach statt sample = to.monthly die monatswandliung via endpoints ...
  
  sample = Cl(to.monthly(p["2010::2012"]) )  
  
  ts.sample = ts(sample, frequency = 12)
  #library(e1071)
  
  browser(mP("test"))
  
  
  #Bank von Prognosemodellen
  
  models = list(
    arima = armaFit(~ arima(1, 1, 15), data=ts.sample),  
    auto.arima = auto.arima(ts.sample),
    bats = bats(ts.sample),
    HoltWinters = HoltWinters(ts.sample,gamma=F),
    tslm = tslm(ts.sample ~ trend + season),
    ets =ets(ts.sample) ,
    kalm = tsSmooth( StructTS(ts.sample,"level"))   ,
    nnet = nnetar(coredata(ts.sample),P=6),
    nnet12 = nnetar(coredata(ts.sample),P=12),
    var = VAR(ts(merge(sample,seq(1,len(sample))),frequency=12) ,p=2,type="trend")  #MM_TODO:  noch ander  VAR-type ausprobieren "trend"   
  )
  
    foreach(i = 1:len(models), .combine="cbind") %do% {
      
      modname=names(models)[i]
      Res=last(sample)
      t2=nval(Res)
      mP("##### %s #### ",modname)
      #if ( modname=="nnet12"  )
      #     browser()
      
      if ( modname=="var")
      {
        # browser(mP("VAR"))
        pred <- predict(models[[i]], n.ahead = 6, ci = 0.95)  #anderes Signifikanz-Intervall 
        fcst=pred$fcst[1]
        Re=data.frame(fcst)
        out=Re
        out2=as.xts(Re[,4], index(last(sample))+1:6)+t2
        
      }
      else
      {
        out = forecast.helper(models[[i]], 6, level = c(80,95))   
        out2 = forecast2xts(sample, out)
        if (modname !=  "nnet" && modname !=  "nnet12")
          forecast.plot(sample, out, main = names(models)[i])
      }
      
      if (max(abs(out2-t2)) > min(abs(out2-t2))) res = max(out2-t2) else res = min(out2-t2)
      res = res/t2*100
      
      Res[]<-res
      colnames(Res)=modname
      mP("%d %s %f ",i,modname, res)
      
      #browser()
      return(Res)
    }  
  
  browser(mP("++++++++++++++++++++++++++++++++"))
}






# compute future dates for the forecast
forecast2xts <- function(data, forecast) {
  # length of the forecast
  # browser()
  h = nrow(forecast)
  dates = as.Date(index(data))
  
  new.dates = seq(last(dates)+1, last(dates) + 2*365, by='day')
  rm.index = date.dayofweek(new.dates) == 6 | date.dayofweek(new.dates) == 0
  new.dates = new.dates[!rm.index]
  
  new.dates = new.dates[1:h]   
  return(make.xts(forecast, new.dates))
}

#################################################################
forecast.plot <- function(data, forecast, ...) {
  out = forecast2xts(data, forecast)
  
  # create plot
  plota(c(data, out[,1]*NA), type='l', 
        ylim = range(data,out,na.rm=T), ...)     
  
  # highligh sections
  new.dates = index4xts(out)
  temp = coredata(out)
  
  n = (ncol(out) %/% 2)
  for(i in n : 1) {
    polygon(c(new.dates,rev(new.dates)), 
            c(temp[,(2*i)], rev(temp[,(2*i+1)])), 
            border=NA, col=col.add.alpha(i+2,150))
  }
  
  plota.lines(out[,1], col='red')
  
  labels = c('Data,Forecast', paste(gsub('Lo.', '', colnames(out)[2*(1:n)]), '%', sep=''))
  plota.legend(labels, fill = c('black,red',col.add.alpha((1:n)+2, 150)))			
}

#################################################################
indi.test.pnn<-function(target,features)
{
  library(pnn)
  #browser()
  
  print("## indi.test.pnn ####")
  train.data = data.frame(na.omit(merge(na.omit(target), na.omit(features))))
  #write.csv(train.data,"MData/TrainData.csv")
  #train.data = read.csv("MData/TrainData.csv")         
  #monatsdaten
  #train.data[,1]= iif(train.data[,1]==1,"L","")
  
  
  pnn <- pnn::learn(na.omit(train.data))
  
  pnn$set[1:shape(na.omit(train.data)), ] #set, the training set.
  pnn$category.column#  category.column, the column index of categories.
  
  pnn <- pnn::smooth(pnn, sigma = 0.72)
  pnn <- pnn::smooth(pnn, sigma = 0.3106217) #0.8688  
  
  pnn$categories #categories, the list of found categories.
  pnn$k #the number of variables.
  pnn$n#n, the number of observations.
  
  #We can now evaluate the performance of our Probabilistic neural network.
  pnn <- pnn::perf(pnn) ###das dauert
  pnn$observed
  pnn$guessed
  pnn$success
  pnn$fails
  pnn$success_rate
  
  return(pnn$success_rate)
  
  #The Bayesian information criterion.
  pnn$bic
  ###################################
  test.data=train.data[,-1]  #ohne Target
  test.now = as.numeric(test.data[35,])
  guess(pnn, test.now)
  #following uses a genetic algorithm to find the best sigma value. You can have a look to the message generated by the 'rgenoud' package during the optimization process.
  pnn <- pnn::smooth(pnn)
}







#################################################################
#zeige die Train.signal-Daten an
##################################################################
show.Train.signal<-function(Train.signal=data$Train.signal)
{
  sym.i <<-0
  m.apply(data,function(p){
    sym.i<<- sym.i+1 
    mP("%d %s ",sym.i, colnames(Cl(p)))
    signal=Train.signal[,sym.i]
    p=Cl(p[fromToS(signal)])
    res=plotSigPrice(signal=signal,prices=p)  
    res = lag(signal)*na.omit(ROC(p))
    #browser()
  })
}



##################################################################
# lade ein Universum - samt data$Train.signal
loadDax<-function()
{
  
  ##################  einfach nur einladen
  
  data<<-new.env()
  load(file="MData/dax.Rdata",envi=data)
  print(ls(data))
  data$Train.signal
  print(data$symbolnames)
  #Prices=data.info(data) 
  xyplot(data$prices)
  print(fromToS(data$prices))
  return(len(data$symbolnames))
  #show.Train.signal(data$Train.signal)
  
  ############### oder bereitstellen
  if (F)
  {
    data = new.env()
    tickers <-getIndexComposition("^GDAXI")
    tickers[which(tickers=="VOW3.DE")]="VOW.DE"
    
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
    for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)    
    bt.prep(data, align='keep.all', dates='1970::')
    data$symbolnames
    purePlot(data$prices)
    p=data.info(data)
    #data.repair(data$prices)
    #source("MLib/indicators.r")
    Entry.signal<<-rollapplyr(price, 1, roll.Target, by.column = T,Prices=price, maxDD=10,minEQ=20)
    #man kann das Trainingssignal nur bis zur letzten Long-Position benutzen .. am rechten Rand wirds ungewiss weil minEQ nicht mehr geschafft wird.
    
    #MM_TODO... dafür müsst ich jetzt über jedes symbol separat iterieren ...-> Vereinfachung #VX
    if (F)
    {
      
      sym="ADS.DE"
      rechter.Rand= 
        
        DateS(last(Entry.signal[Entry.signal>0]))
      train.frame = sprintf("::%s",rechter.Rand)
      #damit sind die Trainings-Soll-Daten vorbereitet
      Train.signal<<-Entry.signal[train.frame,];  #colnames(Train.signal)=c("DAX")
    }
    Train.signal<<-Entry.signal    #VX
    #vorsicht: kein echter pos-plot  - sondern ein ntry-plot
    #  plotSigPrice(signal=Entry.signal[train.frame,],prices=mNorm(price[train.frame,]),indi=list(stopLine=merge(price[train.frame,]) ))
    
    data$Train.signal = Train.signal  
    
    save(list=ls(data),file="MData/dax.Rdata",envir=data)
    
    
  }
}
##########################################################################################
##########################################################################################

#sulzbach   rosenberg  15... 


buildFeatures<-function(Dax, lmLines ) #unfertig
{
  
  browser()
  
  cbind( 
    ROC((Dax),n=1), 
    ROC((Dax),n=5),
    iif(lmLines[,"rsquare"] >0.6, lmLines[,"beta"],0),
    lmLines[,"upr"]-lmLines[,"lwr"],  #die Kanalbreite
    ROC(lmLines[,"fit"],n=5),  #der trend der fit-werte
    runSum(lmLines[,"upr"]-Dax,n=7),  #fläche über upr
    runSum(Dax-lmLines[,"lwr"],n=7),   #fläche unter lwr
    runSum(Dax-lag(lmLines[,"fit"]),n=3)  #abweichung zur prognose
  )
}
##########################################################################################
##########################################################################################


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
  
  p=mNorm(price)  
  #do 10 month Mebane Faber style system
  ma <- SMA(p,n=n)-p
  
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

# if today's low is higher than yesterday's close 1, else 0
lowCloseIndi <-function(price,n=1,period="days") 
{  
  if (period != "days")
    price <- price[endpoints(price,period)]
  
  price=mNorm(price)
  res =ifelse(Lo(HLC(price)) < Lo(lag(price),n),1,0)
}      

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

myVAR_ <- function(price)    #eine Beispielfunktion für rollapplyr
{
  cat(sprintf("myVAR_: %s ",toString(as.Date(index(first(price))))),"\n")
  
  #price=mNorm(price) #NEU
  
  pri=ts(merge(price,seq(1,len(price))),frequency=12)
  #vl= VARselect(pri, lag.max = 5, type="const")
  #vl=as.numeric(vl[[1]][1])
  #if (is.na(vl) || len(vl)==0 || vl==0)
  #  vl=1
  
  vl=1
  priceMod <-VAR(pri,p=vl,type="trend")  #MM_TODO:  noch ander  VAR-type ausprobieren "trend","both", ...
  pred <- predict(priceMod, n.ahead = 15, ci = 0.95)  #anderes Signifikanz-Intervall ausprobieren  
  
  fcst=pred$fcst[1]
  Res=data.frame(fcst)
  res = last(Res[,1])-first(Res[,1])
  return(res)
}
myVAR<-cmpfun(myVAR_) #compilier das Teil


if (F)  
 {
  #schl
  prices=na.omit(mNorm(Cl(data$DAX)))["2010::",1]
  plot(prices)
  slopes <- rollapplyr(mROC(prices), width=20, FUN=myVAR, by.column=T)

  slopes = bt.apply.matrix(slopes,ifna.prev)
  
  Entry.signal = iif(na.omit(slopes) >0,1,-1)
  plotSigPrice(signal=Entry.signal,prices=prices,indi=list(slopes=slopes))
  
  slopes2 <-ZLEMA(na.omit(slopes),n=3)
#TREND-FOLGER   
Entry.signal = iif(slopes >0.0,1,ifelse(slopes < -0.2,-1,NA))

Entry.signal =prices[,1];Entry.signal[]=NA;
  Entry.signal = iif(slopes2 >0,1,ifelse(slopes2 < -0,-1,NA))  
Entry.signal = bt.apply.matrix(Entry.signal,ifna.prev)
  Entry.signal = ZLEMA(na.omit(Entry.signal))  
plotSigPrice(signal=Entry.signal,prices=prices,indi=list(slopes=slopes))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (F)
{
ret=mROC(Cl(price))
Omega<-rollapplyr(ret,width=20, FUN="Omega",by.column=T)

purePlot(mNorm(Cl(price)))
purePlot(ZigZag(na.omit(Omega),change=40,percent=T))
lines(mNorm(Cl(price))*30,col="blue")
signal=runMin()
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

roll.HOLTW_<-function(xtsPrice,what="trend",visual=F)
{
  #http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
  price.ts<-ts(coredata(xtsPrice),freq=5)
  #Trend-Saison-Rauschen.Aufsplittung
  db=decompose(price.ts)

  if (visual)
  {
    plot(db)
    ls(db)
    db$trend
    plot(xts(db$seasonal,order.by = as.Date(index(xtsPrice))))
    #plot(xtsPrice)
    #plot(db$trend,col="red")
  } 
  
  HW=HoltWinters(na.omit(db$trend), gamma=FALSE) #ich rechne den forecast nicht auf dem Orginial sondern nur auf dem Trend-Anteil -- läuft auf eine lineare Verlängerung des db$trend raus.
  
  #plot(HW)
  if (visual)plot.forecast(forecast.HoltWinters(HW, h=10))
  fct=forecast.HoltWinters(HW, h=10)
  if (what =="trend")
     res=last(fct$mean)-first(fct$mean)
  else  #season
     {
       season=as.numeric(as.factor(db$seasonal))   
       if (is.na(season) || season==0) season=0.0001
       season=season/max(season)
       res = last(season)
     }
  #browser()
  #res=xts(cbind(1,2,3),order.by=as.Date(index(last(xtsPrice))));colnames(res)=spl("a,b,c")
  return(res) #die Richtung des forecasts
}
roll.HOLTW<-cmpfun(roll.HOLTW_) #compilier das Teil


if (F)
{
  head(slopes[sl])
  slopes <- rollapplyr(Cl(price), width=60, FUN=roll.HOLTW, by.column=T,what="trend",visual=F)
  Entry.signal =iif(slopes >0,1,-1)
  #  first(Entry.signal[Entry.signal==1])
  plotSigPrice(signal=Entry.signal,prices=mNorm(price),indi=list(slopes=slopes))
  
}



if (F)
   { 
  slopes <- rollapplyr(Cl(price), width=60, FUN=roll.HOLTW, by.column=T,what="season",visual=F)
 
  #INNER-TREND-OSZILLAOR
  #Entry.signal = iif(slopes >0.7,1,ifelse( slopes < 0.3, -1,0) )
  Entry.signal = iif(slopes >=0.8,-1,ifelse(slopes <=0.2,1,NA) )
  Entry.signal = bt.apply.matrix(Entry.signal,ifna.prev)
  
  plotSigPrice(signal=Entry.signal,prices=mNorm(price),indi=list(slopes=slopes))
  
  #TREND-FOLGER   SUPER GUTESMODELL  
  plot(price)
     slopes <- rollapplyr(Cl(price["2012"]), width=120, FUN=roll.HOLTW, by.column=T,what="trend",visual=F)
     Entry.signal =iif(slopes >0,1,-1)
#  first(Entry.signal[Entry.signal==1])
     plotSigPrice(signal=Entry.signal,prices=mNorm(Cl(price["2012"])),indi=list(slopes=slopes))
  

  
  
  
  #super
  roll.HOLTW_<-function(xtsPrice,visual=F)
  {
    #http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
    price.ts<-ts(coredata(xtsPrice[,1]),freq=5)
    #Trend-Saison-Rauschen.Aufsplittung
    db=decompose(price.ts)
    
    if (visual)
    {
      plot(db)
      ls(db)
      db$trend
      plot(xts(db$seasonal,order.by = as.Date(index(xtsPrice))))
      #plot(xtsPrice)
      #plot(db$trend,col="red")
    } 
    
    HW=HoltWinters(na.omit(db$trend), gamma=FALSE) #ich rechne den forecast nicht auf dem Orginial sondern nur auf dem Trend-Anteil -- läuft auf eine lineare Verlängerung des db$trend raus.
    
    #plot(HW)
    if (visual)plot.forecast(forecast.HoltWinters(HW, h=10))
    fct=forecast.HoltWinters(HW, h=10)
    #if (what =="trend")
    trend.res=last(fct$mean)-first(fct$mean)
    #else  #season
{
      season=as.numeric(as.factor(db$seasonal))
      
      if (is.na(season) || season==0) season=0.0001
      season=season/max(season,na.rm=T)
      rauschen=as.numeric(db$random/max(db$random,na.rm=T))
      rauschen[is.na(rauschen)]<-0
      
    }
    # browser()
    sym=firstColname(colnames(xtsPrice[,1]))
    #browser()
    res=xts(cbind(trend.res,last(season),last(rauschen)),order.by=as.Date(index(last(xtsPrice))));
    colnames(res)=spl(sprintf("%s_TrendForecast,%s_SeasonForecast,%s_Rauschen",sym,sym,sym))
    #res= list(1,2,3)
    return(res) #die Richtung des forecasts
  }
  roll.HOLTW<-cmpfun(roll.HOLTW_) #compilier das Teil
  
  
  
  
  slopes=0
  badslopes=slopes
  slopes <- rollapplyr(Cl(price), width=120, FUN=roll.HOLTW, by.column=F,visual=F)
  Entry.signal =iif(slopes[,1] >0,1,-1)
  #  first(Entry.signal[Entry.signal==1])
  head(price)
  plotSigPrice(signal=Entry.signal,prices=mNorm(Cl(price)),indi=list(slopes=slopes[,1]))
  
  #signal = lag(Entry.signal)
signal=Entry.signal
  signal[1]=0
  plot(mRendite(mROC(Cl(price))*signal))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Now4.r ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Multivariate   Long/Short - Prognose
#
#Target:  
#  Long(12,10)  heißt:  im Prognosezeitraum (1 Monat) wird ein MaxDD von 10% nicht #überschritten   und ein MinReturn von 12% erreicht
#Quelle:
#  Multivar, sowohl Zeitreihen als auch Kategorien-Feature ...
#Letztlich soll ein Expertensystem eine multivariate Einschätzung finden für Long/Short
#Rollierendes Fenster auf Tages/Wochen/Monats - Daten
#Ansätze:  TSLARS, Caret, SuperLearner
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#+++++++++ Main
library(quantmod)
if (F)
{
  #prepare Data
  prices = data.info(data)
  prices=prices["::2007"]
  tail(prices)
  #  indi= m.apply(data,Fun=function(x) Hi(x)-Lo(x))
  #  indi=m.apply(data,Fun=function(x) RSI(na.omit(Cl(x))),newName="RSI")
  #--------------- mische feature-zeitreihen hinzu
  indi=m.apply(data,Fun=function(x) 100*(Cl(x)-SMA(Cl(x),n=90))/lag(Cl(x),90),newName="SMA")
  tail(indi)
  allData=merge(prices,indi)
  tail(allData)
  #--------------- erstellung von monats, und wochen-daten
  toPeriods(allData,T)
  #toPeriods(prices)
  tail(ret)
  #check for forecastability
  #-----------------------------------------------
  library(ForeCA)
  XX <- na.omit(ret)
  omega=Omega(XX)
  colnames(XX)[order(omega,decreasing=T)]
  
  omega=sort(omega,decreasing=T)
  
  mP("Die Vorhersehbarkeit der Tages Zeitreihen im Gesamtzeitraum ist %f",sum(omega))
  #print(omega )  
  #-----
  XX <-na.omit(ret.by.week)
  omega=Omega(XX)
  colnames(XX)[order(omega,decreasing=T)]
  
  omega=sort(omega,decreasing=T)
  mP("Die Vorhersehbarkeit der Wochen Zeitreihen im Gesamtzeitraum ist %f ",sum(omega))
  print(omega ) 
  sum(omega)  
  #-----
  XX <- ts(na.omit(ret.by.month[,c(1:dim(prices)[2])]))
  omega=Omega(XX)
  #omega=base::sort(omega,decreasing=T)
  colnames(XX)[order(omega,decreasing=T)]
  
  mP("Die Vorhersehbarkeit der Monats Zeitreihen im Gesamtzeitraum ist %f",sum(omega))
  print(omega ) 
  #-------
  # ------------------------------------------------------------
  #  TSLARS
  dax_i=which(colnames(prices)=="SG2R") #DAX
  
  #hier kann man einstellen welche Spalte aus ret  das Target sein soll
  
  fromTo(ret)  
  m.tslars(ret,dax_i)
  m.tslars(ret.by.week,dax_i)
  
  tail(ret)
  tail(ret.by.month)
  #  dim(na.omit(ret.by.month["2012::"]))
  m.tslars(na.omit(ret.by.month["::2008"]),dax_i)  
  m.tslars(na.omit(ret.by.month["2005::"]),dax_i)
  
  #apply.yearly(ret.by.week["2010::"], FUN=m.tslars,dax_i)
  
  
  #vorbereitung eines by-ref-feldes 
  signal<<-na.omit(ret.by.month)[,1];signal[]=NA
  #rolle montatlich tslars über win monate- monatsdaten
  win=70  #70 monate
  resu=rollapplyr(na.omit(ret.by.month), win, m.tslars, by.column = F,target_i=dax_i,return_sig=T)
  #rolle montatlich tslars über win monate- monatsdaten
  win=70  #70 monate
  resu=rollapplyr(na.omit(ret.by.week), win, m.tslars, by.column = F,target_i=dax_i,return_sig=T)
  #############################################################################
  #PNN
  Prices=mNorm(prices.by.month)
  mchart(Prices)
  tail(Prices)
  par=list(runMa=30)
  ddma=bt.apply.matrix(Prices, runMax, n=as.integer(par$runMa));ddma=ddma-Prices
  ddmi=bt.apply.matrix(Prices, runMin, n=as.integer(par$runMi));ddmi=p-ddmi
  
  
  
  
  
}



#++++++ Functions

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





###################################################################################
#Erzeuge monats und wochen-preise
###################################################################################

toPeriods<-  function(prices,do_ret=F)  #noLog
{
  prices=na.omit(prices)
  #*****************************************************************
  # Compute monthly returns
  #****************************************************************** 
  period.ends = endpoints(prices, 'months')
  period.ends = unique(c(1, period.ends[period.ends > 0]))
  
  prices.by.month <<- prices[period.ends,]
  
  if(do_ret)
  {
    ret = prices[period.ends,] / mlag(prices[period.ends,]) - 1
    ret = ret[-1]
    ret.by.month <<- ret  
    mchart(ret.by.month)
  }
  else
  {
    ret.by.month <<- (diff(log(prices.by.month)))[-1,]
    
    mchart(prices.by.month)
  }
  #*****************************************************************
  # Compute weekly returns
  #****************************************************************** 
  period.ends = endpoints(prices, 'weeks')
  period.ends = unique(c(1, period.ends[period.ends > 0]))
  prices.by.week <<- prices[period.ends,]  
  
  if (do_ret)
  {
    ret = prices[period.ends,] / mlag(prices[period.ends,]) - 1
    ret = ret[-1]
    ret.by.week <<- ret  
  }
  else
  { 
    ret.by.week <<- (diff(log(prices.by.week)))[-1,]
  }
  
  if (do_ret)
    ret <<- prices / mlag(prices) - 1
  else
    ret  <<- (diff(log(prices)))[-1,]
  
  
}


if (F)
  toPeriods(prices) #jetzt gibts auch prices.by.month und prices.by.week

###############################################################################
###############################################################################

m.tslars<-function(logRets, target_i=4, return_sig=F) 
{
  mP("tslars for %s",fromTo(logRets,visual=F))  
  
  library(tslars)
  #browser()
  xi=target_i #welches Zeitreihe soll prognostiziert werden
  target=colnames(prices[,xi])
  mP("die Parameter für %s",target)
  ret=list(time=DateS(last(logRets)),target=target,xPrices="",predict=NA  )
  
  #y <- ts(diff(log(prices)))[-1,xi]
  #x <- ts(diff(log(prices)))[-1,-xi]
  y <- ts(logRets[,xi])
  x <- ts(logRets[,-xi])
  mytslars <- try( tslars(y~x))
  #browser()
  if (!inherits(mytslars, "try-error"))
    try
{
  # if (is.list(mytslars))
{
  #ls(mytslars)
  mytslars$active
  print(mytslars$nrvar.opt)
  # die Indizes der wichtige Zeitreihen
  paris = c(mytslars$active[c(1:mytslars$nrvar.opt)])
  print(colnames(logRets[,paris]))
  prognose=predict(mytslars)
  signal[DateS(last(logRets))] <<-prognose
  
  ret=list(time=DateS(last(logRets)),target=target,xPrices=paste(colnames(logRets[,paris]),collapse=";"),predict=prognose  )
  
  if (return_sig)
    return(prognose)
  
  print(ret)
}
}
  return(ret)
}
#auto.arima(IBM.1982)        	# Auto fit an ARIMA model
#fit                                             
###############################################################################
###############################################################################

rollReg<-function(Y,win=60)
{
  #browser()
  Y=zoo(Y)
  
  dolm <- function(Y){coef(lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y)))}
  
  slope90=rollapplyr(Y, win, dolm, by.column = F)
  #pick Dir nur die Steigungen raus 
  
  ret=slope90[,seq(2,ncol(slope90),2)]
  colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s_slope%d",x,win))
  return(ret)
}




#############################################################################
#############################################################################
#PNN:  Kategorien lernen
#GNN:  RealWerte lernen
########################################################################
#PNN:
#####################################################################
if (F)
{
  #http://flow.chasset.net/category/machine-learning/
  library(pnn)#The program is delivered with four functions - learn, smooth, perf and guess 
  set.seed(1)
  data(norms, package = "pnn")
  norms[1:10, ]
  pnn <- pnn::learn(norms)
  pnn$set[1:10, ] #set, the training set.
  pnn$category.column#  category.column, the column index of categories.
  pnn$categories #categories, the list of found categories.
  pnn$k #the number of variables.
  pnn$n#n, the number of observations.
  pnn <- pnn::smooth(pnn, sigma = 0.72)
  #following uses a genetic algorithm to find the best sigma value. You can have a look to the message generated by the 'rgenoud' package during the optimization process.
  pnn <- pnn::smooth(pnn)
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
  guess(pnn, c(1, 1))
  
  
  ########################################################################
  
  #http://www.r-bloggers.com/general-regression-neural-network-with-r/
  #http://flow.chasset.net/r-grnn/
  library(grnn)
  pkgs <- c('MASS', 'doParallel', 'foreach', 'grnn')
  lapply(pkgs, require, character.only = T)
  registerDoParallel(cores = 4)
  
  data(Boston)
  # PRE-PROCESSING DATA
  X <- Boston[-14]
  st.X <- scale(X)
  Y <- Boston[14]
  boston <- data.frame(st.X, Y)
  
  # SPLIT DATA SAMPLES
  set.seed(2013)
  rows <- sample(1:nrow(boston), nrow(boston) - 200)
  set1 <- boston[rows, ]
  set2 <- boston[-rows, ]
  
  # DEFINE A FUNCTION TO SCORE GRNN
  pred_grnn <- function(x, nn){
    xlst <- split(x, 1:nrow(x))
    pred <- foreach(i = xlst, .combine = rbind) %dopar%  {
      data.frame(pred = grnn::guess(nn, as.matrix(i)), i, row.names = NULL)
    }
  }
  cv <- foreach(s = seq(0.2, 1, 0.05), .combine = rbind) %dopar% {
    # browser() 
    data.frame(s,33)}
  
  
  # SEARCH FOR THE OPTIMAL VALUE OF SIGMA BY THE VALIDATION SAMPLE
  cv <- foreach(s = seq(0.2, 1, 0.05), .combine = rbind,.export=c("pred_grnn")) %do% {
    #browser()
    grnn <- grnn::smooth(grnn::learn(set1, variable.column = ncol(set1)), sigma = s)
    pred <- pred_grnn(set2[, -ncol(set2)], grnn)
    test.sse <- sum((set2[, ncol(set2)] - pred$pred)^2)
    data.frame(s, sse = test.sse)
  }
  #.export=c("simple_fn")). If you want to export your complete global environment then #just write .export=ls(envir=globalenv()) and you will have it for better or worse.
  
  
  cat("\n### SSE FROM VALIDATIONS ###\n")
  print(cv)
  jpeg('grnn_cv.jpeg', width = 800, height = 400, quality = 100)
  with(cv, plot(s, sse, type = 'b'))
  
  cat("\n### BEST SIGMA WITH THE LOWEST SSE ###\n")
  print(best.s <- cv[cv$sse == min(cv$sse), 1])
  
  # SCORE THE WHOLE DATASET WITH GRNN
  final_grnn <- grnn::smooth(learn(set1, variable.column = ncol(set1)), sigma = best.s)
  pred_all <- pred_grnn(boston[, -ncol(set2)], final_grnn)
  jpeg('grnn_fit.jpeg', width = 800, height = 400, quality = 100)
  plot(pred_all$pred, boston$medv)
  dev.off()
  
  
  
  
  ##########################################
  
  
  pkgs <- c('pnn', 'doParallel', 'foreach', 'grnn')
  lapply(pkgs, require, character.only = T)
  registerDoParallel(cores = 8)
  
  data(norms)
  norm2 <- data.frame(n = ifelse(norms$c == 'A', 1, 0), x = norms$x, y = norms$y)
  detach('package:pnn')
  
  nn2 <- smooth(learn(norm2), sigma = 0.5)
  
  pred_grnn <- function(x, nn){
    xlst <- split(x, 1:nrow(x))
    pred <- foreach(i = xlst, .combine = rbind) %dopar% {
      data.frame(pred = grnn::guess(nn, as.matrix(i)), row.names = NULL)
    }
  }
  
  print(pred_grnn(norm2[1:10, -1], nn2))
  
}


##############################################################################
signal.Faber.base <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  
  signal = iif(p > sma200,1,0)
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}
##############################################################################
#Gutes Modell
##############################################################################
signal.monthlyRoc <-function(arg, par = mlist( mlen=c(1,1,3,1),sma.w=c(200,120,350,10)),visual=F,...)
{
p=mNorm(arg$clos)
sym=colnames(p)
data = arg$dat

#einmal die globalen slopes berechnen
if (!exists("mSlope90"))
  mSlope90 <<- rollRegressionXTS(data$prices,win=90);colnames(mSlope90)= colnames(data$prices)

mlen=as.integer(par$mlen)

mRoc  <-  foreach(sym=colnames(p), .combine=cbind, .packages=c("quantmod")) %do%
{res=iif(runSum(monthlyReturn(p[,sym], type='log'),mlen)>=0,1,0); colnames(res)=sym ;res}

#monats-signale auf tagespositionen projezieren
temp=p;  temp[]=NA;     temp[index(mRoc),]=mRoc;    
signal.a=bt.apply.matrix(temp,ifna.prev)
####### 
#browser()
sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
signal = iif(p > sma200 | signal.a > 0| mSlope90[,sym] > 0,1,0) #sportlicher aber längere haltedauer
#signal = iif(p > sma200 ,1,0)  #reiner Faber, geringster DrawDown

return(list(Signal=signal, Indi=list(ma=merge(sma200,p),s1=signal.a)))
}

if (F)
  x=indi.Generic("signal.monthlyRoc", global_arg, par=list(mlen=1,sma.w=200),visual=T,TRAINSYM =-1,safe="TREASURY")



##########################################################################################

signal.randomForest.temp <-function(arg, par = mlist( sma.w=c(200,120,350,10)),visual=F,...)
{
  browser(mP(("signal.randomForest")))
  #Vorausetzung in arg$dat$Target liegen pro price-Symbol TargetTrainingsdaten
  sym=colnames(arg$clos)
  
  p=mNorm(arg$clos)
  today=last(DateS(p))
  target=na.omit(arg$dat$Target[[,sym]])
  P=arg$dat[[sym]]
  
  #~~~~~~~~~~~~~~~~~~~~ Training~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  train.frame= fromToS[target]
  features=merge(SMA(p,200)-runMax(p,200), SMA(p,90)-runMax(p,200), SMA(p,60)-runMax(p,200),   SMA(p,30)-runMax(p,200), SMA(p,10)-runMax(p,200), # 0.94
                 #aroon(LoHi(P))/100 , #unwichtiger wie nach PNN noch gedacht
                 ATR(HLC(P))[,-c(1)],  #produziert das wichtige trueHigh und das unwichtige tr
                 BBands(HLC(P))[,-4],  #pctB  wird nicht gebraucht
                 ZLEMA(na.omit(p),n=30),
                 DonchianChannel(HLC(P)[,-3],n=30),   
                 runSD(p,n=30),
                 SAR(HLC(P)[,-3]) ,
                 williamsAD(HLC(P)),
                 rollRegressionXTS(p,win=90),rollRegressionXTS(p,win=200)
  )
  
  features = na.omit(features)#[fromToS(p)]
  
  
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}


###############################################
run.randomForest<-function(data)
{
  arg=list(clos=prices,dat=data,target=data$Target)
  x=indi.Generic("signal.randomForest", arg, par=list(Target=Targets),visual=T, TRAINSYM =-1)  
}


cud<-function(price,wlen=10)
{
  cud.in <- function(x,wlen) {
   runSum(iif(na.omit(ROC(x,1,type="continuous")) > 0,1,-1),n=wlen)}

  cud.in(price,wlen=wlen)  
#  bt.apply.matrix(price, cud.in  , wlen=wlen)
}

if (F)
  cud(Cl(price))


##############################################################################
##############################################################################
prepare_System<-function()
{
  loadPortfolios(sprintf("%s/Models",getwd()))   #"o:/r/Nuggets/Eckhard/Models" 
  
  #Rohdaten laden  
  Meurope <<- T2$new(PortfolioName = "Meurope", bench="DAX",visual = F, online=F)
  data<-Meurope$t0data
  
  #MM_TODO: pack hier noch ein paar  short-etf dazu
  data$ifo=  load.IFO(visual=T); tail(data$ifo)
  cud
  MWorld3 <<- T2$new(PortfolioName = "MWorld3", bench="DAX",visual = F, online=F )
  data<-MWorld3$t0data
  purePlot(mNorm(na.omit(data$prices)))
  #lösch ein paar extrem gut gelaufene Daten
  data.rm(data,spl("GOLD,JAKARTA_COMPOSITE"))  #OIL
  #data.rm(data,spl("DOW"))  #OIL
  data$prices <- data.info(data,visual=T)#,ignore=C("ZINSEN"))
  #data.repair(prices=data$prices)
  
  #purePlot(mNorm(data$prices[,"DAX"]))
  #löschen
  prices=mNorm(data$prices)
  prices=purePlot(mNorm(data$prices))
  fromTo(prices)
  #trainings-zeitraum beschränken
  ####################################################### Vorbereitung für das Training
  train.frame = "2006::"
  ls(data)
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices)) 
  global_objectId <<-paste("TREND","xx","signal.lm") 
  global_FastTrain <<-20 #heißt:  für jede dim werden 2 Schritte ausprobiert.
  
  #######################################################
  #Traings-Targets berechnen
  
  data$Target   <- compute.Target.by.method(data=data, TargetMethod=1)
  
  
  
  ##########################################################
  
  
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,safe="TREASURY")
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,safe="REX")
  x=indi.Generic("signal.monthlyRoc", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,safe="TREASURY")
  
  TrainIndicator( opti="GRID", indiName = "signal.Faber.base",  visual=T, TRAINSYM = "DAX")
  
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
}