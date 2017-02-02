


options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
options(error = browser)

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
    #  arima = armaFit(~ arima(1, 1, 15), data=ts.sample),  
    auto.arima = auto.arima(ts.sample),
    bats = bats(ts.sample),
    HoltWinters = HoltWinters(ts.sample),
    tslm = tslm(ts.sample ~ trend + season),
    ets =ets(ts.sample) ,
    kalm = tsSmooth( StructTS(ts.sample,"level"))   ,
    nnet = nnetar(coredata(ts.sample),P=6),
    nnet12 = nnetar(coredata(ts.sample),P=12),
    var = VAR(ts(merge(sample,seq(1,len(sample))),frequency=12) ,p=2,type="trend")  #MM_TODO:  noch ander  VAR-type ausprobieren "trend"   
  )
  
  features=
    
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
  cbind(sign(sum(sign(features))), #die summenprognose
        sign(features))
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
  
  sharpe <- SharpeRatio(roc, Rf=.035/12, FUN="StdDev")
  
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
  
  dax_i=1
  fromTo(ret)  
  m.tslars(ret,dax_i)
  m.tslars(ret.by.month,dax_i)
  
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
  mytslars <- try( tslars(y~x,p.max=30))
  #browser()
  if (!inherits(mytslars, "try-error"))
    try
{
  # if (is.list(mytslars))
{
  ls(mytslars)
  mytslars$active
  summary(mytslars)
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

#S.F
##############################################################################
signal.forecast <-function(arg, par = mlist( foretype=c(1,0,3,1)),visual=F,...)
{
  ftype=as.integer(par$foretype) #0.alle,1 stl(), 2 auto.arima, 3 garch, 4 auto.garch
  p=mNorm(arg$clos)
  sym=colnames(p)
  browser(mP("do forcast:"))
  forecasts <- rollapplyr(p, width=200, FUN="roll.forecast.sample", by.column=F, align = "right", sample.on="days",allPrices=p,f="sample.deltaday.forecast")
  save(forecasts,file="dailydax.data")
  browser(mP("%s forecasts <<<< ready",sym))
  
  arg$dat$crs$forecasts <-forecasts
  #evtl. darf ich hier forecasts = lag(forecasts)  machen, weil ich so geschrieben hab.
  browser(mP("AUSWERTUNG"))
  tail(forecasts)
  #für Tagesvorhersagen:
  
  target.oneday = sign(lag(p,-1)-p)  #die Kristallkugel für Tagesdaten
  plotSigPrice(signal =target.oneday,prices=p,indi=NULL)   
  
  #die Fehlerraten der  einzelnen Methoden
  errRate=foreach(i = 1:ncol(forecasts),.combine = "cbind") %do% {
    forecasts[!is.finite(forecasts[,i]),i]<-0    
    block=na.omit(merge(target.oneday,lag(sign(forecasts[,i]))))
    numBugs = abs(sum(xts(apply(block,1,FUN=function(x) sign(diff(x))),order.by=as.Date(index(block)))))
    errRate= numBugs/nrow(block)
  }
  colnames(errRate)=colnames(forecasts)
  errRate
  okRate = 1-errRate
  #der forecast wenn man die sign(Einzelforecasts) mit ihrer okRate gewichtet summiert
  weighted.mean.forecast=xts(apply(na.omit(forecasts),1,FUN=function(x){sign(sum(sign(x)*okRate))}),order.by=as.Date(index(na.omit(forecasts))))
  colnames(weighted.mean.forecast)="Forecast"
  
  #der Fehler des weighted.mean.forecasts ( aller forecast-modelle )
  block=na.omit(merge(target.oneday, weighted.mean.forecast))
  numBugs = abs(sum(xts(apply(block,1,FUN=function(x) sign(diff(x))),order.by=as.Date(index(block)))))
  errRate.mean= numBugs/nrow(block)
  forecast.signals = merge( sign(forecasts),  sign(weighted.mean.forecast))
  tail(forecast.signals)  #<----- das xts. mit allen forecasts - incls. der gewichteten 
  colnames(forecasts)
  frame=""
  frame="2011::2012"
  frame="2006"
  frame=""
  i=12  #12 ist faber
  #plote die forecasts->signa->performance
  
  res =foreach(i = 1:ncol(forecasts ),.combine = "cbind") %do% {
    
    t.indi=forecasts[frame,i]
    #t.indi = EMA(forecasts[frame,i],n=40) 
    #t.indi = lag(forec,-1)#darf ich lagen ??  nein
    model= colnames(t.indi)
    #nrow(forecasts[frame,model])
    px=p;colnames(px)=model #nur damit ich den modelnamen im chart hab
    
    ##############
    
    thresh=0 #gut für Holtwinters Dax
    #thresh=quantile(abs(t.indi),probs=seq(0,1,0.01),na.rm=T)["60%"]  #constante für immer -- gut für naive
    thresh=rollapplyr(abs(t.indi),20, roll.quantile, allPrices=abs(t.indi),maxWin=60,Q="60%" )
    
    #hysterese schwelle 
    d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
    #damit nicht gleich das erste signal falsch ist
    d.signal[1] = sign(first(na.omit(diff(p,20))))
    #browser(mP("now"))
    #der auffüller ...
    sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-NA
    short=0  #hier -1 schreiben wenn er auch short gehen darf
    sig[sig<0]=short
    
    ret= plotSigPrice(signal = sig[frame],prices=px[frame],indi=list(f=merge(forecasts[frame,i],t.indi,thresh,-thresh,0) ))
    
    
    
    mP(model)
    #browser()
    return(ret)
    
  }
  
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}
if (F)
  x=indi.Generic("signal.forecast", global_arg, par=list(foretype=0),visual=T,TRAINSYM ="DAX")
###########################################################################
#das Teil separiert super scharf (insample) Trendsegmente- kann also als 
#schneller Generator für Trainingssignale dienen
#fast.smoothing() liefert ein so schönes signal, dass man seine steigungs-wechsel 
#zum segmentieren nutzen kann
###########################################################################
fast.smoothing.OLD<-function(p,glaettung = 1,visual=F,p0=NULL,roll=F)
{
  #...... smoothing
  #aus dem r-cookbook
  library(KernSmooth)
  p=na.omit(p[,1])
  gridsize <-nrow(p)
  t=c(1:nrow(p))
  bw <-dpill(t,p,gridsize=gridsize)#findet sehr gute bandweite
  
  bw=bw*glaettung
  lp<-locpoly(x=t,y=p,bandwidth=bw,gridsize=gridsize)
  smooth <-lp$y
  s=p;s[]=smooth
  sig=sign(ROC(s,n=1,type="continuous"))#einfach die tagesdiff der glatten kurve
  
  sd = sig#sign(diff(sig))
  rS=runSum(sd,2)
  rS=lag(rS,-1)
  peaks <- p[ as.Date(index(rS[rS==0]))]
  if (len(peaks) < 2)
  {highx=p[1];lowx=p[1];}
  else
  {
    print(".")
    highx = peaks[peaks > lag(peaks,-1)]
    lowx = peaks[peaks < lag(peaks,1)]
    
    f=""
    if (visual)
    {
      plot(p[f])
      points(peaks[f])
      points(highx,col="blue",lwd=3)
      points(lowx,col="red",lwd=3)
    }
    
    highx=as.Date(index(highx))
    lowx=as.Date(index(lowx))
    peaks=as.Date(index(peaks))
    
    if (visual)
    {
      
      plot(p);lines(s,col="red")
      lines(scaleTo(sig,range(p)),col="green")
      
      frame=""
      if (is.null(p0))
        ret= plotSigPrice(signal = sig[frame],prices=p[frame],indi=list(f=merge(p,s)))
      else
        ret= plotSigPrice(signal = sig[frame],prices=p[frame],indi=list(f=merge(p,ZLEMA(p,20),s, scaleTo(p0,range(na.omit(p))))))
    }
    
    if (nrow(s)!=nrow(p))
      browser(mP("bug"))
  }
  if (roll)
    return(last(s))
  
  return(list(smooth = s,highs = highx,lows = lowx,hl=peaks ,sig=sig))
  
}
if(F)
{
  fast.smoothing(p,glaettung = 10,visual=T)  #höhere Werte 2,3...machen alles glatter
  #wahrscheinlich wird das sig im rollapplyr viel schlechter: 
  #aber mit zunehmender width auch besser (> 500)
  plot(p)
  s = rollapplyr(p,FUN=fast.smoothing,width= 1000,glaettung=50,by.column=F,roll=T)
  lines(s,col="green",lwd=3)
  sig=sign(ROC(s,n=20,type="continuous"))#einfach die tagesdiff der glatten kurve
  
  plot(p);lines(s,col="red")
  lines(sig*3,col="green")
  
  frame=""
  ret= plotSigPrice(signal = sig[frame],prices=p[frame],indi=list(f=merge(p,s)))
  
}
###########################################################################

indi.range<-function(p,m)
{
  p=na.omit(p)
  block=foreach(i = 3:m,.combine = "cbind") %do%  {    
    (runMax(p,n=i) - runMin(p,n=i))/i
  }
  res = block
  # browser()
  return(res)
}

if (F)
{
  #vola-trading
  ir=indi.range(ZLEMA(p,2),m=15); rir=ir[,1]; rir[]=NA
  #purePlot(ir)
  rS=rowSums(ir)
  rir[] =ZLEMA(rS,7)
  #purePlot(merge(p,scaleTo(rir,range(p))))
  rir[]=rS
  fast.smoothing(na.omit(rir),5,visual=T,p0=p)
  
  #mPlots(p,rir)
  
}
#....................................................................
v1.indi.minmax.range<-function(p,m)
{
  p=na.omit(p)
  
  plot(p);lines(runMax(p,m),col="blue");lines(runMin(p,m),col="green")
  mami=runMax(p,m)-runMin(p,m)
  sma.mami=SMA(mami,m)
  plot(mami);lines(sma.mami,col="red")
  #res= indi.range( mami, m)
  sig = iif(mami-sma.mami > 0,1,0)
  frame=""
  ret= plotSigPrice(signal = sig[frame],prices=p[frame],indi=list(f=merge(mami,sma.mami)))
  return(res)
}
#...................
indi.minmax.range<-function(p,m)
{
  p=na.omit(p)
  
  plot(p);lines(runMax(p,m),col="blue");lines(runMin(p,m),col="green")
  mami=ROC(runMax(p,m)-runMin(p,m))
  sma.mami=SMA(mami,m)
  plot(mami);lines(sma.mami,col="red")
  #res= indi.range( mami, m)
  sig = iif(mami-sma.mami > 0,1,0)
  frame=""
  ret= plotSigPrice(signal = sig[frame],prices=p[frame],indi=list(f=merge(mami,sma.mami)))
  return(res)
}

if (F)
{
  #vola-trading
  ir=indi.minmax.range(ZLEMA(p,2),m=75); rir=ir[,1]; rir[]=NA
  #purePlot(ir)
  rS=rowSums(ir)
  rir[] =ZLEMA(rS,7)
  purePlot(merge(p,scaleTo(rir,range(p))))
  rir[]=rS
  fast.smoothing(na.omit(rir),5,visual=T,p0=p)
  
  #mPlots(p,rir)
  
}
########################################################################
#welche trend-richtung und segmente bekommt man wenn man separat die 
#min und max-kurve approximiert ?
#m=150
indi.minmax.trend<-function(p,m)
{
  p=na.omit(p)
  
  plot(p);lines(runMax(p,m),col="blue");lines(runMin(p,m),col="green")
  plot(p);lines(SMA(runMax(p,m),20),col="red") 
  mami=ROC(runMax(p,m)-runMin(p,m))
  sma.mami=SMA(mami,m)
  plot(mami);lines(sma.mami,col="red")
  #res= indi.range( mami, m)
  sig = iif(mami-sma.mami > 0,1,0)
  frame=""
  ret= plotSigPrice(signal = sig[frame],prices=p[frame],indi=list(f=merge(mami,sma.mami)))
  return(res)
}

if (F)
{
  #vola-trading
  ir=indi.minmax.range(ZLEMA(p,2),m=75); rir=ir[,1]; rir[]=NA
  #purePlot(ir)
  rS=rowSums(ir)
  rir[] =ZLEMA(na.omit(rS),7)
  purePlot(merge(p,scaleTo(rir,range(p))))
  rir[]=rS
  fast.smoothing(na.omit(rir),5,visual=T,p0=p)
  
  #mPlots(p,rir)
  
}

###########################################################################
signal.smooth.dyn.hysterese <-function(arg, par = mlist( q.w=c(60,30,220,20)),visual=F,...)
{
  prices=arg$clos
  p=mNorm(arg$clos);sym=colnames(prices)
  q.w=as.integer(par$q.w)
  #indi =SMA(na.omit(p),n=as.integer(par$sma.w))
  frame=""
  
  mP("roll fast.smoothing for %s",sym)
  
  if(exists("indi.cashe"))
    indi=s=indi.cashe
  else
  {
    #sehrrrrr ähnlich einem schlichten SMA(p,20)
    indi = s = rollapplyr(p,FUN=fast.smoothing,width= 50,glaettung=30) #150,100
    indi.cashe <<-indi
  }
  t.indi = ROC(s,n=20,type="continuous")#einfach die tagesdiff der glatten kurve
  t.indi = s-SMA(s,200)  #einfach im faber den price durch die glatte kurve ersetzen
  #hysterese - cut:
  thresh=0
  # thresh=0.2
  
  #  thresh=quantile(abs(t.indi),probs=seq(0,1,0.01),na.rm=T)["30%"]
  
  #  thresh=rollapplyr(abs(t.indi),20, roll.quantile, allPrices=abs(t.indi),maxWin=q.w,Q="40%" )
  
  #hysterese schwelle 
  d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
  #damit nicht gleich das erste signal falsch ist
  d.signal[1] = sign(first(na.omit(diff(p,20))))
  #der auffüller ...
  sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
  
  h.indi.1=ROC(SMA(s,200),n=7,type="continuous") #mean reverting um 0
  #wenn diese range zu gering:  gar nicht traden .. weil dann horizontal
  
  #indikator für horizontalbewegung
  #der median der ranges über fenster 3 bis 14  (median: damit schneller)
  h.indi.range=rollapplyr(SMA(s,30),  14 , FUN=function(x) {d=range(x); abs(diff(d))})
  
  h.indi.1[sig>=0]=NA   #markiere die werte ausserhalb der flat intervalle mit NA
  h.indi = h.indi.1
  
  #finde den jeweils ersten Wert
  h.thresh = 0.001
  #hysterese.. wenn negativ dann mindestens kleiner als...,wenn positiv dann mindestens größer als ...
  h2.indi= iif (h.indi < 0, ifelse(h.indi <= -h.thresh,h.indi, 0),
                ifelse(h.indi >= h.thresh,h.indi, 0)  ) 
  
  s.l.direct =sign(h2.indi )
  
  #s.l.direct  #hier -1 schreiben wenn er auch short gehen darf,sonst 0 #ROC:  evtl. short gehen aber nur wenn ...
  short=0 #flat
  sig[sig<0]=short
  
  sig1 = sig
  #  browser()
  #schau dir die flat-pos an: manchmal magst du dort sogar short gehen... 
  sig[sig==short] = s.l.direct[sig==short]
  browser()
  return(list(Signal=sig, Indi=list(sma=merge(SMA(s,30),indi,p,SMA(s,200),s.l.direct),f=merge(t.indi,0,thresh,-thresh,sig1),h=merge(h.indi,0,h.indi.range))))
}
if (F)
  x=indi.Generic("signal.smooth.dyn.hysterese", global_arg, par=list(q.w=100),visual=T, TRAINSYM ="DAX",safe="TREASURY")

############################################################################
test.hysterese.faber<-function()
{
  sym="DAX"
  
  x=foreach(sym_i = 1:ncol(global_arg$clos),.combine = "cbind") %do% {
    sym=colnames(global_arg$clos[,sym_i]  )
    #sym="DAX"
    frame=""
    p=mNorm(global_arg$clos)[frame,sym]
    colnames(p)=c(sym)
    
    #mchart(p,main=sym)  
    print(sym)
    
    indi=SMA(p,200) 
    indi[is.na(indi)]=SMA(p,20)
    sig = iif(p >= indi,1, 0)
    
    #hysterese - cut:
    t.indi=p-indi  #faber
    #t.indi=SMA(sign(diff(indi,2)),n=20) #digital faber
    
    thresh=0
    # thresh=0.2
    thresh=quantile(abs(t.indi),probs=seq(0,1,0.01),na.rm=T)["60%"]
    
    thresh=rollapplyr(abs(t.indi),20, roll.quantile, allPrices=abs(t.indi),maxWin=60,Q="60%" )
    
    #hysterese schwelle 
    d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
    #damit nicht gleich das erste signal falsch ist
    d.signal[1] = sign(first(na.omit(diff(p,20))))
    #browser(mP("now"))
    #der auffüller ...
    sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
    short=0  #hier -1 schreiben wenn er auch short gehen darf
    sig[sig<0]=short
    
    #auswertung:   schnittstellen hervorheben ...hervorheben  
    cut=sig[(sig==1 & lag(sig)!=1) | (sig==short & lag(sig)!=short) ]
    CUT=p;CUT[]=0; CUT[as.Date(index(cut))]=1
    mP("long trades %d",nrow(cut))
    
    #sig = iif(p - indi> thres,1, 0)
    
    ret= plotSigPrice(signal = sig,prices=p,indi=list(sma=merge(p,indi),f=merge(t.indi,0,CUT,thresh,-thresh),d=merge(sign(SMA(sign(diff(indi,3)),n=40)))))
    
  }
}
if (F)
  test.hysterese.faber()

#####
#lieber diese ganzen Kennzahlen in einen classifier schreiben...
#zusätzlich viele  SMA(xxx)  und SMA+Quantile-Differenzen ...
#diff zum rolling max - bzw. bestValue  seit pos-entry
########################################

##############################################################################
signal.Faber.base <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  sma200[is.na(sma200)]<-0
  signal = iif(p >= sma200,1,0)
  
  
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p))))
}
if (F)
{x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM ="DAX")
 x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
 
}
##############################################################################
signal.Faber.i <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  #sma200[is.na(sma200)]<-0
  p2=EMA(na.omit(p,20))
  signal = iif(p2 >= sma200,1,0)
  #rm=runMax(p,50)
  #  signal = iif(p >= rm,1,0)
  
  #  D=merge(ROC(SMA(p,200),n=2),ROC(EMA(p,20),n=2),0)
  #  print(D[signal ==0])
  # keine ENTRIES machen in horizontal-Bereichen (erkennbar an:
  #                                              unterhalb sein unter runMax-Deckel
  #geringer slope90 - oder geringer D200).. in diesen Fällen erst wieder entries
  #erlauben, wenn das runMax-Dax - hinreichen nach oben durchstoßen wurde
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p2,p))))
}
if (F)
  x=indi.Generic("signal.Faber.i", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX")
##############################################################################
signal.Faber.i2 <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  sma200[is.na(sma200)]<-0
  p2=EMA(na.omit(p,20))
  sma200=runMin(sma200,60)  ####### NEU
  signal = iif(p2 >= sma200,1,0)   #sieht gut aus
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p2,p))))#,fs=merge(p,fs,ema))))
}
if (F)  #sehr interessant - entscheident ist aber  sma.w=300 statt 200
  x=indi.Generic("signal.Faber.i3", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1)
##############################################################################
signal.Faber.i3 <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)[,1]
  sym= colnames(p)
  
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  sma200[is.na(sma200)]<-0
  p2=EMA(na.omit(p,20))
  
  signal=iif(p2 >= runMin(sma200,60),1,0)
  
  
  #plotSigPrice(signal,p,main=sym,indi=list(p=p))
  #browser(mP("stop"))
  # fs = rollapplyr(p,FUN=fast.smoothing,width= 1000,glaettung=5,by.column=F,roll=T)
  #  ema=EMA(p,20)
  #  signal = iif(fs >= ema,1,0)
  
  #  dmin= runMin(p,20)
  #  dmax= runMax(p,20)
  
  #rm=runMax(p,50)
  #  signal = iif(p >= rm,1,0)
  
  #  D=merge(ROC(SMA(p,200),n=2),ROC(EMA(p,20),n=2),0)
  #  print(D[signal ==0])
  # keine ENTRIES machen in horizontal-Bereichen (erkennbar an:
  #                                              unterhalb sein unter runMax-Deckel
  #geringer slope90 - oder geringer D200).. in diesen Fällen erst wieder entries
  #erlauben, wenn das runMax-Dax - hinreichen nach oben durchstoßen wurde
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p2,p))))#,fs=merge(p,fs,ema))))
}
if (F)
{
  nasdaq=global_arg$dat$prices[,"NASDAQ100"]
  p=nasdaq
  s=   fast.smoothing(p=p,visual=T,glaettung=10)
  
  mSlope60 <- rollRegressionXTS(na.omit(p),win=60)
  mSlope60s <- rollRegressionXTS(na.omit(s),win=60)
  
  x=indi.Generic("signal.Faber.i3", global_arg, par=list(sma.w=300),visual=T, TRAINSYM ="NASDAQ100")
  
}
##############################################################################

signal.Faber.dyn.hysterese.OLD <-function(arg, par = mlist( q.w=c(60,30,220,20)),visual=F,...)
{
  prices=arg$clos
  p=mNorm(arg$clos);sym=colnames(prices)
  q.w=as.integer(par$q.w)
  #indi =SMA(na.omit(p),n=as.integer(par$sma.w))
  frame=""
  
  print(sym)
  
  indi=SMA(p,200) 
  indi[is.na(indi)]=SMA(p,20)
  sig = iif(p >= indi,1, 0)
  
  #hysterese - cut:
  t.indi=p-indi  #faber
  #t.indi=SMA(sign(diff(indi,2)),n=20) #digital faber
  
  thresh=0
  # thresh=0.2
  thresh=quantile(abs(t.indi),probs=seq(0,1,0.01),na.rm=T)["60%"]
  
  thresh=rollapplyr(abs(t.indi),20, roll.quantile, allPrices=abs(t.indi),maxWin=q.w,Q="60%" )
  #library(caTools)
  #runquantile(x, k, probs, type=7, 
  #            endrule=c("quantile", "NA", "trim", "keep", "constant", "func"),
  #            align = c("center", "left", "right"))
  
  
  #hysterese schwelle 
  d.signal = iif(abs(t.indi)< thresh,NA, sign(t.indi))
  #damit nicht gleich das erste signal falsch ist
  d.signal[1] = sign(first(na.omit(diff(p,20))))
  #der auffüller ...
  sig = as.xts(m.ifna.prev(d.signal)) ; sig[is.na(sig)]<-0
  short=0  #hier -1 schreiben wenn er auch short gehen darf
  sig[sig<0]=short
  
  #auswertung:   schnittstellen hervorheben ...hervorheben  
  cut=sig[(sig==1 & lag(sig)!=1) | (sig==short & lag(sig)!=short) ]
  CUT=p;CUT[]=0; CUT[as.Date(index(cut))]=1
  mP("long trades %d",nrow(cut))
  
  
  return(list(Signal=sig, Indi=list(sma=merge(p,indi),f=merge(t.indi,0,CUT,thresh,-thresh))))#,d=merge(sign(SMA(sign(diff(indi,3)),n=40))))))
}
if (F)
  x=indi.Generic("signal.Faber.dyn.hysterese", global_arg, par=list(q.w=60),visual=T, TRAINSYM ="DAX")

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
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##########################################################################################

##########################################################################################
#die Datenwolke (features) für den Dax
##########################################################################################

doSharpeRatio <-function(r)
{
  res=as.numeric(SharpeRatio(r,FUN="StdDev")) 
}

#########################################################################################
#Dieser cloudGenerator ist durchaus dax-spezifisch weil hier heftig auch vom Ifo-gebrauch #gemacht wird.  Ferner weden die signale gelagged und coarse.coded()  .. pca können bei bedarf #aktiviert werden.  Ebenso können forecast() eingebunden werden.
#########################################################################################
dataCloud.dax<-function(sym,arg,par,p,P,target)
{
  mP("dataCloud.dax")
  ret.p = mROC(p[,sym])
  normP = mNorm(p[,sym])
  #.......................................................................................
  browser(mP("%s forecasts:",sym))
  forecast_<<-NULL #hier merkt er sich die letzte prognose .. 
  forecasts <- rollapplyr(normP, width=300, FUN="roll.forecast.sample", by.column=F, align = "right", sample.on="months",allPrices=normP,f="sample.forecast")
  arg$dat$crs$forecasts <-forecasts
  browser(mP("%s forecasts <<<< ready",sym))
  
  browser(mP("AUSWERTUNG"))
  tail(forecasts)
  #evtl. darf ich hier forecasts = lag(forecasts)  machen, weil ich so geschrieben hab.
  
  #die Fehlerraten der  einzelnen Methoden
  errRate=foreach(i = 1:ncol(forecasts),.combine = "cbind") %do% {
    forecasts[!is.finite(forecasts[,i]),i]<-0    
    block=na.omit(merge(data$Target[,sym],sign(forecasts[,i])))
    numBugs = abs(sum(xts(apply(block,1,FUN=function(x) sign(diff(x))),order.by=as.Date(index(block)))))
    errRate= numBugs/nrow(block)
  }
  colnames(errRate)=colnames(forecasts)
  errRate
  okRate = 1-errRate
  #der forecast wenn man die sign(Einzelforecasts) mit ihrer okRate gewichtet summiert
  weighted.mean.forecast=xts(apply(na.omit(forecasts),1,FUN=function(x){sign(sum(sign(x)*okRate))}),order.by=as.Date(index(na.omit(forecasts))))
  colnames(weighted.mean.forecast)="Forecast"
  
  #der Fehler des weighted.mean.forecasts ( aller forecast-modelle )
  block=na.omit(merge(data$Target[,sym], weighted.mean.forecast))
  numBugs = abs(sum(xts(apply(block,1,FUN=function(x) sign(diff(x))),order.by=as.Date(index(block)))))
  errRate.mean= numBugs/nrow(block)
  forecast.signals = merge( sign(forecasts),  sign(weighted.mean.forecast))
  tail(forecast.signals)  #<----- das xts. mit allen forecasts - incls. der gewichteten Prognose
  arg$dat$crs$forecasts.signals <- forecast.signals
  #wie würden denn die forecast-signale als modell performen ?
  foreach(i = 1:ncol(arg$dat$crs$forecasts.signals ),.combine = "cbind") %do% {
    plotSigPrice(signal = -arg$dat$crs$forecasts.signals[,i],prices=normP,indi=list(f=forecasts[,i]) )   
  }
  #.......................................................................................
  # colnames(forecasts)=c("forecasts")
  
  if  (is.null(arg$dat$ifo))
    arg$dat$ifo =  load.IFO(visual=T)
  
  IFO.m=lag(to.monthly(arg$dat$ifo)[,6])  #fundamentales Umfeld  -  hängt einen monat hinterher
  
  sd=runSD(ret.p,n=30)
  colnames(sd)=c("sd")
  
  mad=runMAD(p,n=30)
  colnames(mad)=c("mad")
  
  atr=ATR(HLC(to.monthly(P)))
  atr.d=(atr[,2]-atr[,1])
  colnames(atr.d)=c("atr.d")
  
  sar=(Ad(P)-SAR(HLC(P)))/abs(Ad(P))*100
  colnames(sar)=c("sar")
  
  hl=(Hi(to.monthly(Hi(P)))-Cl(to.monthly(p)))/(Hi(to.monthly(Hi(P)))-Lo(to.monthly(Lo(P))))
  colnames(hl)=c("hl")
  
  mP("omega")
  omega <- rollapplyr(ret.p, width=20, FUN="Omega", by.column=T, align = "right")
  omega[!is.finite(omega)]<-0
  colnames(omega)=c("omega")
  
  mP("kelly")  
  kelly <- rollapplyr(ret.p, width=90, FUN="KellyRatio", by.column=T, align = "right")
  kelly[!is.finite(kelly)]<-0
  colnames(kelly)=c("kelly")
  
  mP("calmarRatio")  
  calmarRatio <- rollapplyr(ret.p, width=90, FUN="CalmarRatio", by.column=T, align = "right")
  calmarRatio[!is.finite(calmarRatio)]<-0
  colnames(calmarRatio)=c("calmarRatio")
  
  mP("sharpe")
  sharpe <- rollapplyr(ret.p, width=60, FUN="doSharpeRatio", by.column=T, align = "right")
  sharpe[!is.finite(sharpe)]<-0
  colnames(sharpe)=c("sharpe")
  
  mP("expectedShortfall")
  es <- rollapplyr(ret.p, width=60, FUN="ES", by.column=T, align = "right",p=.95, method="historical")
  es[!is.finite(es)]<-0
  colnames(es)=c("es")
  
  faber=p-SMA(p,200); colnames(faber)=c("faber")
  #~~~~~~~~~~~~~~~~~~~~ Training~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  train.frame= fromToS(na.omit(target))
  
  mP("train.frame is %s",train.frame)
  
  #browser()
  features=merge(
    faber, 
    p-runMax(p,200), # 0.94
    #aroon(LoHi(P))/100 , #unwichtiger wie nach PNN noch gedacht
    atr.d,  #produziert das wichtige trueHigh und das unwichtige tr
    BBands(HLC(P))[,4],  #pctB  wird nicht gebraucht
    p-ZLEMA(na.omit(p),n=30),
    #DonchianChannel(HLC(P)[,-3],n=30),   
    sd,
    #sar,
    #williamsAD(HLC(P)),
    rollRegressionXTS(p,win=90),rollRegressionXTS(p,win=200),
    #runSum(sign(to.monthly(p)),7),
    hl,
    sharpe,
    omega,
    es,
    kelly,
    calmarRatio,
    (IFO.m-SMA(IFO.m,7))/IFO.m, runSum(sign(IFO.m),3),
    p/SMA(p,200)
  )
  
  features= bt.apply.matrix(features,ifna.prev)
  
  rsi.m= merge(RSI(p,2),RSI(p,5),RSI(p,20),RSI(p,60))
  colnames(rsi.m) = spl("rsi2,rsi5,rsi20,rsi60")
  rsi.m[!is.finite(rsi.m)] <-0
  
  
  sma.d=merge(
    SMA(p,200)-runMax(p,200), 
    SMA(p,90)-runMax(p,90), 
    SMA(p,60)-runMax(p,60),   
    SMA(p,30)-runMax(p,30), 
    SMA(p,10)-runMax(p,10) 
  )
  colnames(sma.d) = spl("sma.d200,sma.d90,sma.d60,sma.d30,sma.d10")
  
  #features = na.omit(features)#[fromToS(p)]
  features = merge(features,rsi.m, sma.d)
  features= na.omit( bt.apply.matrix(features,ifna.prev))
  tail(features)
  mP("feature coarseCode %d",dim(features)[2])  
  colnames(features)
  
  features.coarse= bt.apply.matrix(na.omit(features),coarse.code)
  
  #jetzt noch die pca-komponenten
  library("caret")
  
  #pca_features <- preProcess(features, method = "pca",na.remove=F)
  #pca_features$numComp   #so viele Variablen werden gebraucht !!!
  
  mP("pca") #------------------------------------------------------------------
  pca_features=prcomp(features, scale = TRUE,center=T,tol=0.15)$x   #biplot(pc)
  dim(pca_features)
  colnames(pca_features)
  
  # features = merge(features,pca_features)  #auch pca
  #features =pca_features   #nur pca
  #tail(features)
  #----------------------------------------------------------------------------
  #auch ge lagte features hinzunehmen
  features.lag = lag(features,5)
  colnames( features.lag ) =  sapply(colnames(features.lag),function(x){ sprintf("%s%s","LAG5_",x)})
  features.lag5=features.lag
  
  features.lag = lag(features,20)
  colnames( features.lag ) =  sapply(colnames(features.lag),function(x){ sprintf("%s%s","LAG20_",x)})
  features.lag20=features.lag
  
  features.lag = lag(features)
  colnames( features.lag ) =  sapply(colnames(features.lag),function(x){ sprintf("%s%s","LAG_",x)})
  
  features = merge(features,features.lag,features.lag5,features.lag20) #lags benutzen
  #-------------------------------------------------------------------------------
  colnames(target)=c("Target")
  train.data<<-data.frame(na.omit(merge(target,features)))
  browser(mP("ok -----------------------------"))
  
  #fit <- auto.arima(dts)
  #plot(forecast(fit, h=20))
  
  #fit <- HoltWinters(dts,gamma=FALSE)  #nicht schlecht
  #plot(forecast(fit, h=20))
  
  print(colnames(features))
  
  return(train.data)
  ###################################################################################################
}


##########################################################################################

signal.randomForest <-function(arg, par = mlist( sma.w=c(200,120,350,10)),visual=F,...)
{
  # browser(mP(("signal.randomForest")))
  
  #Vorausetzung in arg$dat$Target liegen pro price-Symbol TargetTrainingsdaten
  sym=colnames(arg$clos)
  
  p=mNorm(arg$clos)
  today=as.Date(last(index(p)))
  target=na.omit(arg$dat$Target[,sym])
  crs = arg$dat$crs   #das trainings-environment - siehe rattle  
  P=na.omit(arg$dat[[sym]])
  #berechne alle merkmale für den dax
  
  mP("signal.randomForest >>")
  
  if (T || !exists("train.data") || is.null(train.data))
    features = dataCloud.dax(sym,arg,par,p,P,target)
  else
    features = train.data  #chashe
  
  priceFeatures =merge(p,features)
  
  #laufe über die Daten,  trainiere monatlich, und gibt für jeden Tag die prognose zurück- als tupel aus (signal, signal.sicherheit)
  firstUsefulDate =DateS(first(na.omit(priceFeatures)))
  #browser(mP("....."))
  #  firstUsefulDate = "2002-01-01"  #vorher gings immer nur bergauf
  firstUsefulDateS = sprintf("%s::",firstUsefulDate) #überspringe den anfänglichen NA - Bereich der priceFeatures
  #  first( which(priceFeatures[firstUsefulDateS,"Target"] != 1))
  sig.value <- rollapplyr(p[firstUsefulDateS], width=1000, FUN=roll.classifier, by.column=F,allPrices=priceFeatures,maxWin=2500, retrain.on="quarterly",crs=crs)
  
  browser(mP("signal.fertig ##############################################           "))
  signal = sig.value[,1]  # 2 ist: sig.confidence
  sig.confidence = sig.value[,2]
  model.err = sig.value[,3]
  Signal <<- signal
  #signal= iif(features[,1] >0,1,0)
  #signal = iif(p > SMA(p,200),1,0)
  # View(tail(merge(p,signal1,signal),500))
  # dim(Features)
  #purePlot(mNorm(Features))
  # mP("signal.randomForest %s",fromToS(na.omit(features)))
  
  bug=(target-signal)/nrow(target)*100
  
  plotSigPrice(signal=signal,prices=p)  
  #fromToS(train.data);dim(train.data)
  
  return(list(Signal=signal[train.frame], Indi=list(conf=sig.confidence,model.err=model.err, bug=target-signal))) #ma=merge(features[,1],features[,2],p)[train.frame]
}
#####################################################################################################################

#####################################################################################################################
#laufe täglich über die Daten,  trainiere monatlich, und gibt für jeden Tag die prognose zurück- als tupel aus (signal, signal.sicherheit)
#wir haben ABEND ..  die Close-Kurse liegen vor.
#####################################################################################################################
roll.classifier_<-function(p, allFeatures, maxWin=600, retrain.on="monthly",crs=NULL)
{
  #das bis maxWin wachsende Zeitfenster zum Tag:  lastDay
  sym=colnames(p);  if (is.null(sym))    sym=1
  pricesFeatures=allFeatures
  toDay = DateS(last(p))
  firstPriceDate.i=max(1,get.Index(allFeatures,DateS(last(p)))-maxWin)  #das fenster wächst bis zu maxWin
  firstPriceDate = DateS(allFeatures[firstPriceDate.i])
  #der passende  merge(p,feature) - Datenausschnitt
  priceFeatures=na.omit(allFeatures[sprintf("%s::%s",firstPriceDate,toDay)])  #das mit jedem  schritt wachsenden Preis-fenster
  
  #das Ergebnis wird ein 2 dim  xts aus signal und signalsicherheit
  res=as.xts(data.frame(signal=1,confidence=2),index(last(p)))
  
  print(last(p))
  
  # if (dim(na.omit(priceFeatures))[1] < nrow(p))   #jetzt liegen wenigstens nrow(p) features vor
  #    return()
  #browser(mP("roll.classi"))
  #liegt ein (eher seltenes  retrain- Event vor ?) 
  firstCall=ifelse( len(ls(crs))==0,T,F)#wenn noch nie mit dem trainings-environment gearbeitet wurde, ist es leer, also muss auf jeden fall erst mal trainiert werden
  #++++++++ das aktuelle modell zu einer Prognose heranziehen
  
  prognose=NULL
  if (!firstCall)
    prognose = classifier.randomForest.predict(crs, priceFeatures[toDay,-1])
  
  retrain.event =firstCall
  #+++++++++ muss das modell ge fitted werden ?
  if (retrain.on=="monthly")
  { 
    #abends, am Monatsende . haben wir die Close-Werte für die Berechnung der DataCloud und trainieren damit das modell nach.
    #die heutige positionierung von gestern abend, haben wir noch mit dem alten modell gemacht
    month.ends = Dates.month.ends(index(p)) #Dates.quarter.ends
    
    if (len(which(month.ends==toDay)) >0)   #ist today ein monatsende ?
      retrain.event =T  
  }
  
  if (retrain.on=="quarterly")
  { 
    #abends, am Monatsende . haben wir die Close-Werte für die Berechnung der DataCloud und trainieren damit das modell nach.
    #die heutige positionierung von gestern abend, haben wir noch mit dem alten modell gemacht
    quarter.ends = Dates.quarter.ends(index(p)) #Dates.quarter.ends
    
    if (len(which(quarter.ends==toDay)) >0)   #ist today ein monatsende ?
      retrain.event =T  
  }
  
  if (retrain.event)
  {
    mP("retrain.event !!! at %s ",toDay)
    modell.err= classifier.randomForest.fit(crs, priceFeatures[,-1])   #trainiere - ohne explizit noch mal die Kurse zu übergeben
    mP("modell.err %f",modell.err)
  }
  if (is.null(prognose))
    prognose = classifier.randomForest.predict(crs, priceFeatures[toDay,-1])
  
  return(prognose)
}
roll.classifier<-cmpfun(roll.classifier_)

#fm.fund.factor.test()
###############################################
run.randomForest<-function(data)
{
  data$crs<-new.env()   #stelle ein neues trainings-environment zu Verfügung
  #browser()
  arg=list(clos=na.omit(data$prices["::2014"],dat=data,target=data$Target))
  x=indi.Generic("signal.randomForest", arg, par=list(no=0),visual=T, TRAINSYM ="DAX")  
}

if (F)
{
  run.randomForest(data)   #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # mtrain.data <-data.frame(train.data)
  colnames(train.data)
  head(train.data)
  rattle()
}

cud<-function(price,wlen=1)
{
  cud.in <- function(x,wlen) {
    runSum(iif(ROC(x-min(x),1,type="discrete") > 0,1,-1),n=wlen)}
  
  cud.in(price,wlen=wlen) / wlen 
  #  bt.apply.matrix(price, cud.in  , wlen=wlen)
}

if (F)
  plot(  cud(Cl(to.weekly(price)),wlen=20))

############################################################################################################################################################################################
###############################################################
#produzier ein xts wo für jedes model der monats forecast in einer spalte liegt
###############################################################
sample.forecast<-function(ts.sample, ret.ts.sample,ts, p, lastDate,visual=F)
{
  # mach statt sample = to.monthly die monatswandliung via endpoints ...
  #sample = Cl(to.monthly(p["2010::2012"]) )  
  #  ts.sample = ts(sample, frequency = 12)
  #library(e1071) 
  mP("sample.forecast")
  print(dim(ret.ts.sample))
  
  library(fArma)
  #Bank von Prognosemodellen
  #http://robjhyndman.com/hyndsight/forecast4/
  if (len(ts)<24)
    ts.stl<- ts#ts(data=as.numeric(ts), frequency = 12)
  else
    ts.stl<- ts(data=as.numeric(ts.sample), frequency = 12)
  #ts.sample sind monatsdaten,  ts und p sind tagesdaten
  models = list(
    #arima = armaFit(~ arima(1, 1, 15), data=ts.sample),  
    auto.arima = try(auto.arima(ts.sample,seasonal=F)),
    bats = try(bats(ts.sample)),
    HoltWinters = try(HoltWinters(ts.sample,gamma=F)),#seasonal="mult")),
    tslm = try(tslm(ts.sample ~ trend )),
    ets =try(ets(ts.sample)),
    kalm = try(tsSmooth( StructTS(ts.sample,"level"))) ,
    stl=try(stl(ts.stl,s.window="periodic")),
    nnet = try(nnetar(as.numeric(ts.sample),P=6,size=dim(ts.sample)[1] /5+1)) ,
    nnet12 = try(nnetar(as.numeric(ts.sample),P=12,size=dim(ret.ts.sample)[1] /5+1)) , #P=lags
    var = try(VAR(ts(merge(ts.sample,seq(1,len(ts.sample))),frequency=12) ,p=2,type="trend"))  
  )
  #stl():
  #time.series <- ts(data=sales, frequency = 12)#, start=c(2000, 1), end=c(2002, 12))
  #decomposed  <- stats::stl(time.series, s.window="periodic")
  #plot(decomposed)
  #seasonal   <- decomposed$time.series[,1]
  #trend      <- decomposed$time.series[,2]
  #remainder  <- decomposed$time.series[,3]
  
  #source("garchAuto.R")
  
  #spy = getSymbols("SPY", auto.assign=FALSE)
  #rets = ROC(Cl(spy), na.pad=FALSE)
  #fit = garchAuto(rets, cores=8, trace=TRUE)
  
  #library(neuralnet)
  #net.infert <- neuralnet(case~parity+induced+spontaneous, infert,
  #                        err.fct="ce", linear.output=FALSE, likelihood=TRUE)
  
  #browser(mP("#######################"))
  
  
  allRes=foreach(i = 1:len(models), .combine="cbind") %do% {
    modname=names(models)[i]
    mP("##### %s #### ",modname)
    
    if (!inherits(models[[i]], "try-error"))
    {
      Res=last(ts.sample)
      t2=nval(Res)
      #if ( modname=="nnet12"  )
      #     browser()
      if ( modname=="var" )
      {
        #   browser(mP(modname))
        pred <- predict(models[[i]], n.ahead = 6, ci = 0.95)  #anderes Signifikanz-Intervall 
        fcst=pred$fcst[1]
        Re=data.frame(fcst)
        out=Re
        out2=as.xts(Re[,4], as.Date(lastDate)+1:6)+t2
      }
      else
      {
        out = forecast.helper(models[[i]], 6, level = c(80,95))   
        out2 = forecast2xts(ts.sample, out)
        if (visual)
          forecast.plot(as.xts(ts.sample), out, main = names(models)[i])
      }
      #auswertung:  wir machen einen x periode-forecast .. bei dem kanns rauf und runter gehen
      #gehs mehr rauf (max > min) so interpretier ich das als long, sonst als short
      #browser(mP("wait for %s",modname)
      if (max(abs(out2-t2)) > min(abs(out2-t2))) res = max(out2-t2) else res = min(out2-t2)
      res = res/t2*100   
      #  tail(res)
    }
    else  #naive prognose falls das modell nicht rechnen konnte
    {
      mP(" scip ") 
      res = last(diff(ts.sample))/last(ts.sample)*100
    }
    Res = xts(coredata(res), order.by=as.POSIXct(lastDate))
    colnames(Res)=modname
    mP("%d %s %f ",i,modname, res)
    # browser(mP("++++++++++++++++++++ %s",lastDate ))
    return(Res)
  }  
  mP("additional forecasts")
  
  mP("naive")
  #noch die naive prognose
  res = last(diff(ts.sample))/last(ts.sample)*100
  naiveRes = xts(coredata(res), order.by=as.POSIXct(lastDate))
  colnames(naiveRes)="naive"
  
  mP("stlTrend")
  #noch die naive stl-trend- prognose
  ts1<- ts(data=as.numeric(ts), frequency = 12)
  decomposed  <- stats::stl(ts1, s.window="periodic")
  plot(decomposed)
  trend      <- decomposed$time.series[,2]
  
  res = last(diff(trend))/last(trend)*100
  stlTrend = xts(coredata(res), order.by=as.POSIXct(lastDate))
  colnames(stlTrend)="stlTrend"
  
  mP("faber")
  faber = p-SMA(p,200)
  res = last(faber)/t2*100
  faber = xts(coredata(res), order.by=as.POSIXct(lastDate))
  colnames(faber)="faber"
  
  rets = ROC(p, na.pad=FALSE)
  if (F) #sehr langsame Suche nach dem besten garch modell
  {
    mP("autoGarch")
    #source("MLib/garchAuto.R")
    browser(mP("... now Garch  ..."))
    garchFit = garchAuto(rets,  trace=TRUE)
    garchForecast=predict(garchFit, n.ahead=1, doplot=F)[,1]# one-forecast
    aGarch = garchForecast/ t2 * 100
    colnames(aGarch)=c("aGarch")
  }
  #
  mP("Garch")
  spyGarch = garchFit(~arma(0, 2) + garch(1, 1), data=as.ts(tail(rets, 500)))
  garchForecast2=predict(spyGarch, n.ahead=6, doplot=F)[,1]
  if (max(abs(garchForecast2)) > min(abs(garchForecast2))) res = max(garchForecast2) else res = min(garchForecast2)
  garchForecast2 = res/t2*100   
  garchForecast2 = xts(coredata(garchForecast2), order.by=as.POSIXct(lastDate))
  colnames(garchForecast2)=c("Garch")
  
  
  # the actual forecasts are predict(spyGarch, n.ahead=1, doplot=F)[,1]
  
  allRes = merge(naiveRes,allRes,stlTrend,faber,garchForecast2)
  print (allRes)
  #browser()
  
  return(allRes)
}
######################################################################################
sample.deltaday.forecast<-function(ts.sample, ret.ts.sample,ts, p, lastDate,visual=F)
{
  # mach statt sample = to.monthly die monatswandliung via endpoints ...
  #sample = Cl(to.monthly(p["2010::2012"]) )  
  #  ts.sample = ts(sample, frequency = 12)
  #library(e1071) 
  mP("sample.deltaday.forecast")
  print(dim(ret.ts.sample))
  
  
  library(fArma)
  #Bank von Prognosemodellen
  #http://robjhyndman.com/hyndsight/forecast4/
  #ts.sample sind monatsdaten,  ts und p sind tagesdaten
  models = list(
    #arima = armaFit(~ arima(1, 1, 15), data=ts.sample),  
    auto.arima = try(auto.arima(ts.sample,seasonal=F)),
    bats = try(bats(ts.sample)),
    HoltWinters = try(HoltWinters(ts.sample,gamma=F)),#seasonal="mult")),
    tslm = try(tslm(ts.sample ~ trend )),
    ets =try(ets(ts.sample)),
    kalm = try(tsSmooth( StructTS(ts.sample,"level"))) ,
    stl=try(stl(ts.sample,s.window="periodic")),
    nnet = try(nnetar(as.numeric(ret.ts.sample))),
    var = try(VAR(ts(na.omit(merge(p,lag(p,n=1))),frequency=12) ,p=2,type="trend"))
    
  )
  
  allRes=foreach(i = 1:len(models), .combine="cbind") %do% {
    modname=names(models)[i]
    mP("##### %s #### ",modname)
    #browser()
    if (!inherits(models[[i]], "try-error"))
    {
      Res=last(p)
      t2=nval(Res)
      #if ( modname=="nnet12"  )
      #     browser()
      if ( modname=="var" )
      {
        pred <- predict(models[[i]], n.ahead = 1, ci = 0.95)  #anderes Signifikanz-Intervall 
        fcst=pred$fcst[1]
        Re=data.frame(fcst)
        out=Re
        out2=as.xts(Re[,4], as.Date(lastDate)+1)
      }
      else
      {
        out = forecast.helper(models[[i]], 1)[,1]#, level = c(80,95))   
        out2 = as.xts(out, as.Date(lastDate)+1)
        if (modname != "nnet")
          out2 = out2-t2
        #out2 = forecast2xts(ts.sample, out)-t2
        if (visual)
          forecast.plot(as.xts(ts.sample), out, main = names(models)[i])
      }
      #auswertung:  wir machen einen x periode-forecast .. bei dem kanns rauf und runter gehen
      #gehs mehr rauf (max > min) so interpretier ich das als long, sonst als short
      #browser(mP("wait for %s",modname)
      
      res = out2
      res = res/t2*100   
      #  tail(res)
    }
    else  #naive prognose falls das modell nicht rechnen konnte
    {
      mP(" scip ") 
      res = last(ts.sample)/t2*100
    }
    Res = xts(coredata(res), order.by=as.POSIXct(lastDate))
    colnames(Res)=modname
    mP("%d %s %f ",i,modname, res)
    #   browser(mP("++++++++++++++++++++ %s",lastDate ))
    return(Res)
  }  
  mP("additional forecasts")
  
  mP("naive")
  #noch die naive prognose
  res = last(diff(p))/t2*100
  naiveRes = xts(coredata(res), order.by=as.POSIXct(lastDate))
  colnames(naiveRes)="naive"
  print(naiveRes)
  
  mP("stlTrend")
  #noch die naive stl-trend- prognose
  decomposed  <- stats::stl(ts.sample, s.window="periodic")
  plot(decomposed)
  trend      <- decomposed$time.series[,2]
  res = last(diff(trend))/last(trend)*100
  stlTrend = xts(coredata(res), order.by=as.POSIXct(lastDate))
  colnames(stlTrend)="stlTrend"
  print(stlTrend)
  
  mP("faber")
  faber = p-SMA(p,200)
  res = last(faber)/t2*100
  faber = xts(coredata(res), order.by=as.POSIXct(lastDate))
  colnames(faber)="faber"
  print(faber)
  
  rets = ROC(p, na.pad=FALSE)
  if (F) #sehr langsame Suche nach dem besten garch modell
  {
    mP("autoGarch")
    #source("MLib/garchAuto.R")
    browser(mP("... now Garch  ..."))
    garchFit = garchAuto(rets,  trace=TRUE)
    garchForecast=predict(garchFit, n.ahead=1, doplot=F)[,1]# one-forecast
    aGarch = garchForecast/ t2 * 100
    colnames(aGarch)=c("aGarch")
  }
  
  mP("Garch")
  
  spyGarch = garchFit(~arma(0, 2) + garch(1, 1), data=as.ts(tail(rets, 500)),trace=F)
  garchForecast2=predict(spyGarch, n.ahead=1, doplot=F)[,1]
  garchForecast2 = garchForecast2/t2*100   
  garchForecast2 = xts(coredata(garchForecast2), order.by=as.POSIXct(lastDate))
  colnames(garchForecast2)=c("Garch")
  print(garchForecast2)
  
  # the actual forecasts are predict(spyGarch, n.ahead=1, doplot=F)[,1]
  
  allRes = merge(naiveRes,allRes,stlTrend,faber,garchForecast2)
  print (allRes)
  #browser()
  
  return(allRes)
}
##############################################################################
##############################################################################

roll.xts_<-function(p, allPrices, maxWin=200, sample.on="months",f="sample.forecast",args=NULL)
{
  mP("roll.xts %d ####################################################",count_)
  #das bis maxWin wachsende Zeitfenster zum Tag:  lastDay
  #browser()
  sym=colnames(p);  if (is.null(sym))    sym=1
  pricesFeatures=allPrices
  toDay = DateS(last(p))
  firstPriceDate.i=max(1,get.Index(allPrices,DateS(last(p)))-maxWin)  #das fenster wächst bis zu maxWin
  firstPriceDate = DateS(allPrices[firstPriceDate.i])
  #der passende  merge(p,feature) - Datenausschnitt
  allPrices=na.omit(allPrices[sprintf("%s::%s",firstPriceDate,toDay)])  #das mit jedem  schritt wachsenden Preis-fenster
  
  retrain.event=F
  # browser()
  if (sample.on=="months")
  {
    month.ends = Dates.month.ends(index(p)) #Dates.quarter.ends
    if (len(which(month.ends==toDay)) >0 )   #ist today ein monatsende ?
      retrain.event =T  
    
    if (retrain.event)
    {
      p=na.omit((allPrices[,sym])   )
    }
  }
  else
    if (sample.on=="days")
    {
      p=na.omit((allPrices[,sym])   )
    }
  print(last(p))
  roll.xts.res_ =NA
  if (retrain.event )
  {
    mP("roll.xts %d days %d samples",nrow(allPrices[,sym]), nrow(p))
    
    fun = match.fun(f)
    #forecast_ <<- sample.forecast(ts.sample, ret.ts.sample,ts,p,lastDate=toDay) # <<<------------------
    #roll.xts.res_ <<- fun(p,date.last=toDay,args) # <<<------
    #find.best.series(data, visual=F, date.last=NULL,min.len=100)
    
    count_=count_+1
    
    roll.xts.res_ <<- fun(args$data, visual=F, date.last=toDay,  min.len=as.integer(args$min.len)) # <<<------
    
    mP("<< roll.xts")
  }
  #browser()
  return(roll.xts.res_)
}
roll.xts<-cmpfun(roll.xts_)

if (F)
{#erst wollt ich mit rollapplyr iterieren.   
  #roll.xts ist ein schöner allgemeiner iterator - aber hier 
  #völliger overkill!
  count_=0
  tech.cloud <- rollapplyr(normP, width=500,  FUN="roll.xts",maxWin=1000, by.column=F, allPrices=normP,sample.on="months",f="find.best.series", args=list(data=data,visual=F, min.len=101))
}

######################################################################################

roll.forecast.sample_<-function(p, allPrices, maxWin=200, sample.on="months",f="sample.forecast")
{
  #das bis maxWin wachsende Zeitfenster zum Tag:  lastDay
  sym=colnames(p);  if (is.null(sym))    sym=1
  pricesFeatures=allPrices
  toDay = DateS(last(p))
  firstPriceDate.i=max(1,get.Index(allPrices,DateS(last(p)))-maxWin)  #das fenster wächst bis zu maxWin
  firstPriceDate = DateS(allPrices[firstPriceDate.i])
  #der passende  merge(p,feature) - Datenausschnitt
  allPrices=na.omit(allPrices[sprintf("%s::%s",firstPriceDate,toDay)])  #das mit jedem  schritt wachsenden Preis-fenster
  
  retrain.event=F
  # browser()
  if (sample.on=="months")
  {
    month.ends = Dates.month.ends(index(p)) #Dates.quarter.ends
    if (len(which(month.ends==toDay)) >0 || is.null(forecast_))   #ist today ein monatsende ?
      retrain.event =T  
    
    if (retrain.event)
    {
      sample.ts = na.omit((allPrices[,sym])   )
      ts = ts(as.numeric(sample.ts), f = 12)#,start=as.Date("2004-06-09"
      
      sample = na.omit(Cl(to.monthly(allPrices[,sym]) )  )
      ts.sample = ts(sample, f = 12)#,start=as.Date("2004-06-09"
      #browser(mP("++++++++++++++++++"))
      ret.sample = na.omit(ROC(Cl(to.monthly(allPrices[,sym]) )  ))
      ret.ts.sample = ts(ret.sample, f = 12)
      
      p=na.omit((allPrices[,sym])   )
    }
  }
  else
    if (sample.on=="days")
    {
      retrain.event =T
      sample.ts = na.omit((allPrices[,sym])   )
      
      ts.sample = ts  = ts(as.numeric(sample.ts), f = 7)#,start=as.Date("2004-06-09"
      ret.ts.sample= ROC(na.omit((allPrices[,sym])   ),na.pad=F)
      p=na.omit((allPrices[,sym])   )
    }
  print(last(p))
  
  if (retrain.event )
  {
    mP("roll.forecast.sample_ %d days %d samples",nrow(allPrices[,sym]), nrow(ts.sample))
    
    fun = match.fun(f)
    #forecast_ <<- sample.forecast(ts.sample, ret.ts.sample,ts,p,lastDate=toDay) # <<<------------------
    forecast_ <<- fun(ts.sample, ret.ts.sample,ts,p,lastDate=toDay) # <<<------
    
    mP("<< roll.forecast.sample_")
  }
  return(forecast_)
}
roll.forecast.sample<-cmpfun(roll.forecast.sample_)
##############################################################################
##############################################################################

##############################################################################
##############################################################################
#fsr
find.support.resistance<-function(p,nowP=NULL,maxdd=10,visual=T)
  #suche horizontal-bereiche
  #auf AllTime-High/Low
  #häufig auftretenden Highs/Lows
  #ganzahlinge Kursen (z.B. die Dax-8000er Linie)  
{
  res=list()
  p = na.omit(p[,1])
  ################# 
  if(is.null(nowP))
    nowP=last(p)
  
  nowP.val=as.numeric(nowP)
  
  if (visual)
    plot(p,main=mP("%s %s",colnames(p),DateS(nowP)))
  
  p=p[which(as.Date(index(p)) <= as.Date(index(nowP)))]  #beschränke dich auf die Vergangenheit und Gegenwart
  
  HL=HighLows(p,maxdd=maxdd)
  
  if (visual)
  {
    lines(p,col="black",lwd=2) #die Vergangenheit
    #markiere nowP im Chart
    amark(nowP,"green");abline(h=nowP,col="green")
    
    points(p[HL$lows],col="red")
    points(p[HL$highs],col="blue") 
    
  }
  #mode= "lows"
  
  for(mode in  c("lows","hihgs"))
  {
    #  ex=iif(mode=="lows",p[HL$lows],p[HL$highs])  # #die extrema .. je nach mode .. hier nur die lows
    
    browser(mP(".........mode is  %s",mode))
    ex = p[HL$hl]
    if (mode=="lows")
      ex = ex[ex <= nowP.val]
    else
      ex = ex[ex >= nowP.val]
    points(ex,col="magenta")
    
    
    b=100 #das y-raster: alle lows die in die gleiche zelle fallen, werden zusammengezählt  (und durch ihren mean repräsentiert)
    #die mittelwerte der extrema - gemittelt über solche die zum gleichen von b bins gehören
    mex=ave(ex,cut(ex,b=b,labels=F),FUN=mean)   #ave wendet FUN auf jedes Element der gruppe an ... 
    #und so oft treten diesen Werte auf:
    cmex=sapply(unique(sort(coredata(mex))),FUN=function(x) {res= count(mex[mex==x]);names(res)<-x;return(res)})
    cmex
    #die (mean)Werte der extrema
    cmex.val= as.numeric(names(cmex))
    #es gilt: sum(cmex) == len(ex)
    
    browser(mP("t1"))
    #die m häufigsten extrem-Werte, allerdings beschränkt auf Werte oberhalb/unterhalb von nowP.val 
    cmex<2
    m=3;  #hier kann man gut einstellen wieviele Linie von jeder Sorte gefunden werden sollen
    
    if (mode == "lows")
      hcmex=head(sort(cmex[names(cmex)<=nowP.val ],decreasing=F),m)
    else
      hcmex=head(sort(cmex[names(cmex) >=nowP.val],decreasing=T))
    
    #no=sapply(names(hcmex), FUN=function(x) { abline(h=x)})
    browser(mP("now %s",mode))
    # die extrema die wenigstens mc mal vorkommen:
    mc=last(hcmex)
    tcmex=cmex[cmex >= mc]
    tcmex.val= as.numeric(names(tcmex));names(tcmex.val)=c(1:len(tcmex.val))
    #no=sapply(tcmex.val, FUN=function(x) { abline(h=x)})
    
    # max und min der extream -- AllTimeHighs
    
    if (mode == "lows")
    {
      min.ex=ex[which(ex==min(ex))]
      abline(h=min.ex,col="magenta")
    }
    else
    {
      max.ex=ex[which(ex==max(ex))]
      abline(h=max.ex,col="lightblue"); 
    }
    ###########
    dnow.tcmex= coredata(nowP-tcmex.val) ;
    #gib den index als namen
    names(dnow.tcmex)  <-c(1:len(dnow.tcmex))
    #support:  lediglich cmex werte unterhalb von nowP sind von interesse
    dnow.support = iif(mode=="lows", dnow.tcmex[dnow.tcmex >= 0],dnow.tcmex[dnow.tcmex <= 0])
    #die beiden zu nowP nächst gelegenen cmex
    sort(abs(dnow.support),decreasing=F)
    next.support=tcmex.val[as.integer(names(sort(abs(dnow.support),decreasing=F)[1:5]))]
    no=sapply(next.support, FUN=function(x) { abline(h=x,col=ifelse(mode=="lows","red","blue"))})
    res[[mode]]=next.support
    
  }
  ###### jetzt noch ganze Zahlen (8000er High) hinzufügen
  res
}

#p=data$prices[,"NASDAQ100"]
if (F)
  sr= find.support.resistance(p,nowP=p[1800])
#######################################################################################################################################################
#######################################################################################################################################################

bt.simple.x <- function(p, signal) 
{
  # lag singal
  signal = Lag(signal, 1)   #das Signal von gestern
  
  # back fill
  #Generic function for replacing each NA with the most recent non-NA prior to it.
  signal = na.locf(signal, na.rm = FALSE)
  signal[is.na(signal)] = 0
  
  # calculate Close-to-Close returns
  ret = ROC(p, type='discrete')
  ret[1] = 0
  if (F)
  {
    plot(cumprod(1+ret))
    mPlot(mNorm(p),ylog_=F)
  }
  # compute stats 
  bt = list()
  bt$ret = ret * signal
  bt$equity = cumprod(1 + bt$ret)     #der eignetlich Ertag  - muss noch durch ihren Start wert geteilt wereden um   pct zu erhalten           
  return(bt)
}

#############################################################################################################

faber<-function(P,visual=T)
{
  
  if (shape(P)>200)
  { sma = SMA(P,200); sma[is.na(sma)]<-(min(P)-0.01)}
  else
  {
    sma=P[,1];sma[]=(min(P)-00.1)
  }
  signal = iif(P > sma, 1, 0)
  
  p2=EMA(na.omit(P,20))
  signal = iif(p2 >= sma,1,0)
  # signal[signal !=1]
  
  if (F) #nur so ein spass
  {
    #die perf des Systems sma.cros
    sma.cross = bt.simple.x(P, signal)
    
    #timing chart
    #plotSigPrice(signal=signal,prices=P,indi=list(f=merge(P,sma)))  
    # Create a chart showing the strategies perfromance in 2000:2009
    dates=""
    # Buy & Hold
    signal.bh = rep(1, nrow(P))
    buy.hold = bt.simple.x(P, signal.bh)
    
    buy.hold.equity = buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
    sma.cross.equity = sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])
    EQ=merge(buy.hold.equity,sma.cross.equity)
    colnames(EQ)=c("BuyHold","faber")
    
    mchart(EQ)
    
    if (visual) 
      plotSigPrice(signal=signal,prices=P,indi=list(f=merge(P,sma),eq=EQ)  )
  }
  df = (P-sma)/lag(P)*100
  return(list(signals=signal,df=df))
}
##########################################################################################################
if (F)
  faber(P)

hist.vol<-function(prices)
{
  #  if (colnames(prices)[1]=="EMERGINGMARKETS.OPEN")
  #    return(c(0,0))
  prices=na.omit(prices)
  if (shape(prices) <99)
  {
    res=prices[,1] ; res[]=0; return(res)}
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  hist.vol = bt.apply.matrix(ret.log, runSD, n = 21)
  vol= bt.apply.matrix(hist.vol, EMA, n = min(60,shape(prices)-10))
}
##############################################################################################
#### Qualtitäskritieren für eine Zeitreihe .. wann sieht sie gut aus:  glatt, steil
##############################################################################################

series.quality<-function(P,visual =T,min.len=50, arg=list(dat=new.env()))
{
  #  browser(mP("series.quality"))
  nowT = DateS(last(P))
  P=na.omit(P[,1])+1
  ret =mROC(P) ;  #ist gleich zu  ret1 = Return.calculate(P, method="discrete")
  #ret=na.omit(ROC(P,type="continuous"))  #equal ret =diff(log(P))
  eq=na.omit(mNorm(P)) #Return.cumulative(ret, geometric = T)  
  sym=colnames(P)
  
  #plot(eq)
  #chart.CumReturns(ret,legend.loc="topleft", main="Cumulative Daily Returns")
  #chart.TimeSeries(P)
  #chart.Drawdown(ret) 
  #table.AnnualizedReturns(na.omit(ret))  
  #charts.PerformanceSummary(ret)
  #sehr gut:
  #chart.TimeSeries(P)
  #einfaches faber-trading . gut wär wenn es wenige signale produziert hät
  fb= faber(P,visual=T)#charts.PerformanceSummaryX(ret)
  #wie oft wird ein faber sma geschnitten ?
  nt=as.numeric(numTrades(sign(fb$signals))$allT)-1 
  
  #noch checken was das genau heißt
  #table.CalendarReturns(ret)
  
  #für den Vergleich mit Benchmark ...  
  #chart.RelativePerformance(ret,NULL)
  #InformationRatio(ret,ret)
  
  ############ kennzahlen rauschreiben  
  mP("---series.quality(%s)--- %s..%s ",sym,DateS(first(P)),DateS(last(P)))
  #if (sym=="EMERGINGMARKETS.OPEN")
  #  browser()
  len=shape(P)
  if (visual)mP("len: %d",len)
  if (len < min.len)
  {
    
    return(list(first.date=DateS(first(P)),last.date=nowT,sym=colnames(P),len=len,sharpe=NA, last.eq=NA, calmar=NA,maxdd=NA,cgar=NA,kelly=NA, faber.cut=NA,ES=NA,faber.df=NA,vola=NA, rsi30=NA,cud.week=NA,cud.day=NA,garchForecast2=NA,sma.d200=NA,sma.d90=NA,sma.d60=NA,slope200=NA,slope90=NA))
    
  }
  sharpe=SharpeRatio(ret,FUN="ES")*100 #compute.sharpe(na.omit(ret))
  if (visual)mP("sharpe: %f",sharpe)
  
  if (visual)mP("last(eq) %f",last(eq))
  cgar=compute.cagr(eq)*100
  if (visual)mP("Cgar %f",cgar)
  #ret.pa=Return.annualized(ret)  #identisch cgar
  #if (visual)mP("ret.pa %f",ret.pa)
  maxdd=-maxDrawdown(ret)*100#compute.max.drawdown(eq)*100
  
  guete=sharpe*cgar/abs(maxdd)
  if (visual)mP("guete %f",guete)
  
  if (visual)mP("MaxDD %f",maxdd)
  calmar = abs(compute.calmar(eq))#*sign(cgar);
  if (visual)mP("calmar %f",calmar)
  kelly=as.numeric(KellyRatio(ret))
  if (visual)mP("kelly %f",kelly)
  
  #............. IFO
  if  (is.null(arg$dat$ifo))
    arg$dat$ifo =  load.IFO(visual=T)
  
  IFO.m=lag(to.monthly(arg$dat$ifo)[,6])  #fundamentales Umfeld  -  hängt einen monat hinterher
  ifo.m = as.numeric(last(IFO.m-SMA(IFO.m,7))/IFO.m)
  if (visual)mP("ifo.m %f",ifo.m)
  
  ifo.sum= as.numeric(last(runSum(sign(IFO.m),3)))
  if (visual)mP("ifo.sum %f",ifo.sum)
  
  ################################
  ##### Jetzt einige taktische - short-Term-Maße:   ES, SMA-200-Differenz, vola, RSI(30)
  #if (colnames(P)[1]=="EMERGINGMARKETS")
  #    browser()
  # first do normal ES calc
  #e1=ES(ret, p=.95, method="historical")*100
  e1=ES(ret, p=.99, method="historical")*100
  # now use Gaussian
  e2=ES(ret, p=.95, method="gaussian")*100
  # now use modified Cornish Fisher calc to take non-normal distribution into account
  e3=ES(ret, p=.95, method="modified")*100
  es = as.numeric(min(e1,e2,e3,na.rm=T))
  if (visual)mP("ES %f",es)
  
  faber.df = as.numeric(last(fb$df))/max(len,1) #abweichung des P vom sma200
  if (visual)mP("faber.df %f",faber.df)
  
  hist.vol=ifelse(shape(P)>=99, as.numeric(last(hist.vol(P)))*100,0) #vola 
  if (visual)mP("vola %f",hist.vol)
  rsi30=ifelse(shape(P)>=31, as.numeric(last(RSI(P,30))),0)
  if (visual)mP("RSI30 %f",rsi30)
  P.week=Cl(to.weekly(P))
  
  if (F&&sym=="SXRBP.OPEN")
    browser()
  
  cud.week=as.numeric(last(cud(P.week,wlen=min(10,shape(P.week),na.rm=T))))
  if (visual)mP("cud.week %f",cud.week)
  cud.day=as.numeric(last(cud(P,wlen=20)))
  if (visual)mP("cud.day %f",cud.day)
  
  library(fGarch)
  spyGarch = tryCatch( garchFit(~arma(0, 2) + garch(1, 1), data=as.ts(tail(ret, min(shape(ret),500,na.rm=T))),trace = F),
                       error=function( err ) FALSE, warning=function( warn ) FALSE )
  
  if( ! is.logical(spyGarch)   ) {
    #spyGarch = garchFit(~arma(0, 2) + garch(1, 1), data=as.ts(tail(ret, min(shape(ret),500,na.rm=T))),control = garch.control(trace = F))
    garchForecast2=predict(spyGarch, n.ahead=6, doplot=F)[,1]
    res= ifelse (max(abs(garchForecast2)) > min(abs(garchForecast2)),  max(garchForecast2) , min(garchForecast2))
    garchForecast2 = as.numeric(res/as.numeric(last(ret))*100 )  
  }
  else
    garchForecast2 =NA
  
  #garchForecast2 = xts(coredata(garchForecast2), order.by=as.POSIXct(lastDate))
  #colnames(garchForecast2)=c("Garch")
  if (visual)mP("garchForecast2 %f",garchForecast2)
  
  #MM_TODO
  #die krümmung des approximierten inneren signals  glaettung1 und glaettung 10
  
  #der abstand zum nächsten widerstand*dessen Stärke  ( w-stärke/ w-abstand),  und das selbe zum support
  
  #die holtwinters-prognose (des inneren signals)
  
  #noch einige technische merkmale die sich schon im forest bewährt haben:extra.tech
  #berechne schnell einige Zeitreihen, die auf dem lange stück besser gehen
  p=mNorm(tail(P,200))
  sma.d200=as.numeric(last(SMA(p,min(shape(p),200,na.rm=T))-runMax(p,min(shape(p),200,na.rm=T))))
  if (visual)mP("sma.d200 %f",sma.d200)
  
  sma.d90=as.numeric(last(SMA(p,min(shape(p),90,na.rm=T))-runMax(p,min(shape(p),90,na.rm=T))))
  if (visual)mP("sma.d90 %f",sma.d90)
  
  sma.d60=as.numeric(last(SMA(p,min(shape(p),60,na.rm=T))-runMax(p,min(shape(p),60,na.rm=T))))
  if (visual)mP("sma.d60 %f",sma.d60)
  
  #slope200=lm.FIT(last(na.omit(p),200))[2]
  slope200=coef(lm.FITm(last(na.omit(p),200)))[2]
  
  if (visual)mP("slope200 %f",slope200)
  
  #slope90=as.numeric(last(rollRegressionXTS(p,win=min(shape(p),90,na.rm=T)) ))
  #slope90=lm.FIT(last(na.omit(p),90))[2]
  slope90=coef(lm.FITm(last(na.omit(p),90)))[2]
  
  if (visual)mP("slope90 %f",slope90)
  
  #Reg-Gerade über den letzten Abschnitt
  slopeLen.fit=lm.FITm(P)
  slopeLen=coef(slopeLen.fit)[2]
  if (visual)mP("slopeLen %f",slopeLen)
  
  ND=data.frame(x=c(1:shape(P)),as.numeric(P))
  pred1<-stats::predict(slopeLen.fit,newdata=ND,interval="prediction",level=0.90,)
  YP=merge(P,P,P)
  YP[]=pred1
  Yf=slopeLen.fit$fitted.values
  #die Pos im trend
  itp=in.Trend.pos(P,YP,nowT)
  
  
  PN=rollTrueRange(P,90)
  tr=range(P,60,na.rm=T)
  true.range=tr[2]-tr[1]
  
  if (visual)  #trend-kanal zeichnen  -- die kanal-Beite ist ebenfalls interessant
  {
    #lines(P,col="green")
    lines(YP[,2]);lines(YP[,3])  #YP[,1] die linreg-Gerade, YP[,2] obere und untere kanal kante
  }
  Ranks="rank.slope,rank.momVol,rank.es,rank.momVolslope,rank.beta"
  if (visual)mP("slopeLen %s",Ranks)
  
  ranks=spl(Ranks)
  Rank=list()
  for(rank in spl(Ranks))
  {
    rank.xts= get.cashed.rank(rank,sym);colnames(rank.xts) = c(rank)
    Rank[[rank]]= rank.xts          
  }
  
  #kanal kritieren
  #  Oberhalb, unterhalb der Mitte.
  #  Nahe am Rand 
  
  #  YP[,1] #das
  
  #krümmung des inneren signals 
  
  #beta gegenüber index  ->  rank.beta
  
  if (visual)mP("------------------------------------")
  
  return(append(list(first.date=DateS(first(P)),last.date=nowT,sym=colnames(P),len=len,sharpe=sharpe, last.eq=as.numeric(last(eq)), guete=guete, calmar=calmar,maxdd=maxdd,cgar=cgar,kelly=kelly,ifo.m=ifo.m, ifo.sum=ifo.sum,faber.cut=nt,ES=es,faber.df=faber.df,vola=hist.vol, rsi30=rsi30,cud.week=cud.week,cud.day=cud.day,garchForecast2=garchForecast2,sma.d200=sma.d200,sma.d90=sma.d90,sma.d60=sma.d60,slope200=slope200,slope90=slope90,slopeLen=slopeLen,itp=itp,true.range=true.range), Rank))
  
}

###########################################################################################################################
##############################################################################
#ich hab den chart von data.info(data) vor mir und will schnell die besten 
#charts finden

#finde das letzte segment und berechne performance davon
##############################################################################
#fast.smoothing() liefert ein so schönes signal, dass man seine steigungs-wechsel 
#zum segmentieren nutzen kann
##############################################################################
find.last.trend.segment<-function(p,visual=T,glaettung=10, min.wLen=0)
{
  #p=data$prices[,8]
  p=na.omit(p[,1])
  mP("############# >> %s",DateS(last(p)))
  
  sym=colnames(p)
  print(sym)
  #if (sym=="REX.OPEN")
  #  browser()
  if (F && sym=="SXRBP.OPEN")
  {browser()
   visual=T
  }
  #plot(p)
  #HL=HighLows(p,maxdd=35)
  #lines(p[HL$hl],col="red")
  #schöne glatte segmentierung - die dient dann duch der Segmentierung
  
  fs=   fast.smoothing(p=p,visual=visual,glaettung=glaettung)
  #ls(fs)
  
  if (len(fs$hl)==0)
    fs$hl=DateS(first(p))
  if (len(fs$hl) ==0)
    x2=DateS(last(p))
  else  
  {
    if (is.xts(fs$hl))
      x2=as.Date(index(last(fs$hl)))
    else
      x2 =last(fs$hl)
  }
  if (visual) amark(p[x2])
  x2.D=as.Date(index(last(p)))
  #x2.D=try(as.Date(x2))
  if (F)
     x2.D =tryCatch( as.Date(x2),
                  error=function( err ) 
                  {
                    browser()
                    DateS(last(p))
                  }, 
                  warning=function( warn ) FALSE )

  
  
  last.len=x2.D-as.Date(x2)
  #########   heuristik für den fall dass die  gefundene last-segment-länge kurz erscheint oder gar kürzer als win.wLen ist
  #zurückschalten - aber auch nicht zuuuu weit zurück
  if (len(fs$hl) > 1   && mean( diff (fs$hl))/5 <  3 * min.wLen && last.len < mean( diff (fs$hl))/5) #wenn zu kurz - geh auf das vorletzte segment
    x2 = first(tail(fs$hl,2))
 

  #zurückgehen bis min.wLen 
  zeros = fs$hl
  while (len(zeros) > 1 && (min.wLen> 0 && last.len < min.wLen))
  { 
    zeros  =but.last(zeros)
    x2 = last(zeros) #kürzer sollte er nie werden
    last.len=x2.D-as.Date(x2)  
    print("step back ")
  }
  if (min.wLen > 0 && last.len < min.wLen)
  {
   x2 = index(first(tail(p,min.wLen)))  #jetzt brutal in die pampa setzen - aber die min.wLen darf nicht unterschritten werden
   mP("step back to min.wLen %d",min.wLen)
  } 
  
  if(visual)amark(p[x2],col="green")
  return(list(x2=x2,fs=fs,tw=fs$hl,smooth=fs$smooth))
  
  return(append(list(sdate=toString(x2)) ))
  
}

########################################################################################
#find.best.series<-function(data, visual=F, date.last=NULL,min.len=100)
#series.quality() sicht - sowohl auf das letzte Segment, als auch auf das kürzest  gemeinsam lange segment - und zwar für alle data-series
#.. das über eine ganze historie hinweg geht mit 
#make.cloud<-function(data, cloudName="EU.cloud", frame="2004::2012",min.len=100, freq="months")
#######################################################################################

find.best.series<-function(data, visual=F, date.last=NULL,min.len=100,max.len=5000,sym="",glaettung=5,saveDir="")
{
  #browser(mP("find.best.series"))
  if (visual && F)data.info(data)
  
  if (visual && saveDir !="")
  {
    file=sprintf("%s/%s_%s_best.png",saveDir,date.last,sym)
    png(file=file,width=640,height=600,units="px")
    
    #pdf(file = file, width=8.5, height=11)
  }
  
  if (is.null(date.last)) date.last=DateS(last(data$prices))
  #rollierendes fenster realisieren !!!(linken teil ausblenden)
  first.date = ""
  frame=sprintf("::%s",date.last)
  if (shape(data$prices[frame]) > max.len)
  {
    ifirst=index(data$prices)-max.len
    first.date=DateS(data$prices[ifirst])
  }
  frame=sprintf("%s::%s",first.date,date.last)
  
  if (sym=="")
    onlysym=c()
  else
    onlysym=c(sym)
  
  #nur einer
  #finde das letzte segment und berechne performance davon
  #best.data(data$prices[,4])
  #alle:    
  best=t(m.apply(data, function(p ) 
  {
    # browser(mP("#2"))
    p=na.omit(Ad(p[frame]))
    colnames(p)=pureColnames(p)
    sym=pureColnames(p)[1]
    #P=mNorm(na.omit(data$prices[frame,sym]))
    
    mP("------------> 1 find.last.trend.segment for %s %s",sym,fromToS(p))
    #if (sym=="REX.OPEN")
    p.short.date=find.last.trend.segment(na.omit(p),visual=visual,glaettung=glaettung)$x2 ##<<--------- es lohnt visual  = T---------    #P:  das letzte Segment
    P=na.omit(p[as.Date(index(na.omit(p)))>=as.Date(p.short.date)])
    
    #beurteile die Qualität des letzten trendsegments
    sq = series.quality(P ,visual=T,min.len=min.len,arg=list(dat=data))  # es lohnt visual  = T---
    
  }, frame=frame, onlysym =onlysym )
         
  ) 
  
  #die Qualitäsmatrix
  best = data.frame(best)
  krit="sharpe"
  #sortiere die Qualitätsmatrix nach einem Kritierium
  best=best[with(best, order(-unlist(best[,krit]))), ]
  bestLong=best
  View(bestLong)
  
  #filter die aus, deren segemente zur kurz sind
  best= best[ with(best,best[,"len"]>=min.len),]
  #filter die aus, deren segemente negativen cgar haben
  #  best= best[ with(best,best[,"cgar"] >0),]
  #best.vals=unlist(best[,"sharpe"])
  #sort.best.vals= sort(best.vals,decreasing=T)
  if (nrow(best) > 1)
  {
    # return(best)
    
    #alles anzeigen:
    if (visual)
    {
      xyplot(data$prices[frame],main="all Data")
      chart.RiskReturnScatter((mROC(data$prices[frame])), main=fromToS((data$prices[frame])),colorset = rainbow8equal)#,ylim=range(data$prices[frame],na.rm=T))
      purePlot(mNorm(data$prices[frame]),main="all Data compared")
      
    }
    
    #iteriere über den sortierten data.frame, und hole dir die auf die letzten Segmente gekürzten Zeitreihen
    res=foreach (y=1:nrow(best),.combine="cbind") %do%     {
      row=best[y,]; sym=row.names(row)
      le=unlist(row[,"len"])
      krit.val=unlist(row[,krit])
      
      #der absegmentiere Teil
      
      P=tail(mNorm(na.omit(data$prices[frame,sym])),le)
      #P=tail(data$prices[frame][,sym],le)
      
      #lines(P,lwd=2)#,main=row.name)
      mP("%s %s: %f %d %s",krit,sym,krit.val,le,DateS(first(P)))
      #P=P["2013::"]
      P=P-as.numeric(first(P))+1#normierung
    }
    
    #browser()
    #View(res)
    # View(bt.apply.matrix(res,function(x) {x[is.na(x)]<-0.1}))
    # chart.RiskReturnScatter(na.omit(mROC(res)),main=sprintf("longHistory %d",shape(res)),ylim=range(na.omit(mNorm(res)),na.rm=T))
    
    #das Startdatum der kürzesten Zeitreihe -
    #es kann jetzt eine zeitreihe dabei sein, die kürzer ist als min.len
    #dann wird alles kürzer als min.len
    if (dim(res)[1] < 1)
      return(best )
    
    first.common.date=DateS(first(na.omit(merge(res))[,1]))
    
    #zeige hier die unteschiedlich langen letzten Trens-Segmente
    if (visual) 
    {
      amark(first.common.date)
      #xyplot(res,main="all Data")
      
      purePlot(res,main=sprintf("longHistory %d",shape(res)))
      amark(first.common.date)
      
      mP("first.common.date %s",first.common.date)
    }
    
    frame=sprintf("%s::",first.common.date)
    #print(frame)
    res2=foreach (x=1:ncol(res),.combine="cbind") %do%     {
      P=res[frame,x]
      P=P-as.numeric(P[first.common.date])+1
    }
    
    
    res2=na.omit(res2)
    if (visual) 
    {
      amark(first.common.date)
      purePlot(res2 ,main=sprintf("shortHistory len=%d,   %s",shape(res2),fromToS(res2))) #alle gleich lang
      xyplot(res2)
      chart.RiskReturnScatter(mROC(res2), main=sprintf("len=%d, %s..%s",shape(res2),DateS(first(res2)),DateS(last(res2))),colorset = rainbow8equal,ylim=range(res2,na.rm=T))
      
    }
    #jetzt noch mal eine series.quality() für die gleich  kurzen endstücke  (damit ich nicht Äpfel mit Birnen vergleiche)
    
    res2.table=
      foreach (x=row.names(best),.combine="rbind") %do%     {
        P=res2[,x]
        #beurteile die Qualität des letzten GEMEINSAMEN trendsegments  (das kürzeste trendsegment bestimmt die Länge)
        series.quality(P,min.len=min.len, arg=list(dat=data))  
      }
    row.names(res2.table) =res2.table[,"sym"]
    
    
    best = data.frame(res2.table)
    #sortiere die Qualitätsmatrix nach einem Kritierium
    best=best[with(best, order(-unlist(best[,krit]))), ]
    #filter die aus, deren segemente zur kurz sind
    best= best[ with(best,best[,"len"]>=min.len),]
    #filter die aus, deren segemente negativen cgar haben
    #best= best[ with(best,best[,"cgar"] >0),]
    
    View(best)
    
    if (visual)
    {
      
    }
    
    res=rbind( merge(1,bestLong),merge(2,best))
  }
  
  else  #einzeiliges Ergebnis
    res = merge(1,best)
  
  if (saveDir!="")
  {
    file=sprintf("%s/xls/%s_%s_best.xls",saveDir,date.last,sym)
    m.write.xls(res, file,"bestLongbest")
    if (visual )
      dev.off()
    
  }
  return(res)   #sowohl sicht auf das letzte Segment, als auch auf das kürzest  gemeinsam lange segment
}
if (F)
{
  find.best.series(data,visual =F)
  find.best.series(data, visual=F, date.last="2011-02-28",min.len=100)
}
##################################################################
if (F)
{
  data$prices=data.info(data)
  purePlot(mNorm(na.omit(data$prices)))
  
  frame="2010-01-01::2010-03-01"
  frame="2006::2012"
  normP=na.omit(data$prices[frame])[,1]  #zum langhangeln
  
  
  #viel einfacher:  einfach mit foreach()
  checkDates=Dates.month.ends(index(normP))
  #iteriere einmal über normP,  werde zu sample.on aktiv und rufe dann f auf.
  
  EU.cloud=foreach(dat = checkDates,.combine = "rbind") %do% { find.best.series(data, visual=F, date.last=dat,min.len=100) }
  View(EU.cloud)
  m.write.xls(EU.cloud, "EU.cloud.xlsx","trainData")
  
  
}

##############################################################################
#baue für alle data im frame jeden freq c(months,weeks,quarters,days) die Cloud
#mit find.best.series
##############################################################################
make.cloud<-function(data, cloudName="EU_cloud", frame="2010-01-01::2010-03-01",min.len=100, freq="months",visual=T, TargetMethod=0, preload=T)
{  
  ############################################   CASH
  ## check ob die features schon gerechnet wurden
  trainData=NULL; targetData=NULL
  file.train=file=sprintf("TrainTarget/%s_train.dat",cloudName)
  file.target=sprintf("TrainTarget/%s_target.dat",cloudName)
  if (preload)
  {
    try(load(file=file.train))
    try(load(file=file.target))
  }
  ############################################   TARGETdata
  if (len(targetData) >0)
    if(len(data$Target) < 1)
      data$Target = targetData
  
  #berechne das Target
  if (len(data$Target) < 1 )
    data$Target   <- compute.Target.by.method(data=data, TargetMethod=TargetMethod, visual=visual,glaettung=5) #1  
  
  View(data$Target)
  
  ############################################   TRAINdata
  if (len(trainData) > 0)
  {
    data$Train = trainData
    View(trainData)
    mP("dim of  trainData is %d,%d",dim(trainData)[1],dim(trainData)[2])
    return("preloaded")
  }
  
  #berechne die Feature
  normP=na.omit(data$prices[frame])[,1]  #zum langhangeln
  if (freq=="months")
    checkDates=Dates.month.ends(index(normP))
  if (freq=="weeks")
    checkDates=Dates.week.ends(index(normP))
  if (freq=="quarters")
    checkDates=Dates.quarter.ends(index(normP))
  if (freq=="days")
    checkDates=as.date(index(normP))
  
  mP("Berechne %d Trainingsdatensaetze",len(checkDates))
  EU.cloud=foreach(dat = checkDates,.combine = "rbind") %do% { find.best.series(data, visual=F, date.last=dat,min.len=min.len) }
  
  
  #serialisierung 
  trainData = data.frame(EU.cloud);     rownames(trainData)=c(1:nrow(trainData))
  data$Train = trainData
  save(trainData,file=file.train)
  View(trainData)
  
  targetData = data.frame(data$Target)
  save(targetData,file=file.target)
  
  #serialisierung als xls
  library(XLConnect)
  
  if (F)
  {
    
    m.write.xls(trainData,xlsName=sprintf("%s_train.xlsx",cloudName),sheetname="trainData")
    m.write.xls(trainData,xlsName=sprintf("%s_target.xlsx",cloudName),sheetname="trainData")
  }
  #alles in xls-workbook  
  filename=sprintf("TrainTarget/%s_cloud.xlsx",cloudName)
  wb <- XLConnect::loadWorkbook(filename, create = TRUE)
  XLConnect::createSheet(wb, name = "trainData")
  XLConnect::createSheet(wb, name = "targetData")
  
  XLConnect::writeWorksheet(wb, trainData, sheet = "trainData",rownames="Number")
  XLConnect::writeWorksheet(wb, targetData, sheet = "targetData",rownames="date")
  XLConnect::saveWorkbook(wb)
  
  sag("cloud steht in %s bereit",filename)
  return("calculated")
}
if (F)
{
  make.cloud(data,cloudName="EU_cloud",frame="2010-01-01::2010-02-01",min.len=100,freq="months",visual=T,TargetMethod=0, preload=T)
}
##############################################################################
##############################################################################
prepare_System<-function()
{
  loadPortfolios(sprintf("%s/Models",getwd()))   #"o:/r/Nuggets/Eckhard/Models" 
  wb <- loadWorkbook(sprintf("%s/Securities.xls",customerData))
  #oil from Quandl
  MeuropeX <<- T2$new(PortfolioName = "MeuropeX", bench="DAX",visual = F, online=T)
  data <-MeuropeX$t0data
  
  #Rohdaten laden  
  Meurope <<- T2$new(PortfolioName = "Meurope", bench="DAX",visual = F, online=F)
  data<-Meurope$t0data
  #find.best.series(data,visual =F,date.last="20100101")
  data.info(data)
  find.best.series(data,visual =T,date.last="20120101")  
  
  #MM_TODO: pack hier noch ein paar  short-etf dazu
  data$ifo=  load.IFO(visual=T); tail(data$ifo)
  plot(to.monthly(data$ifo)[,6]);lines(SMA(to.monthly(data$ifo)[,6]),7,col="red")
  tail(data$ifo)
  
  y0=to.monthly(data$ifo)[,6]
  dax=mNorm(Ad(data$DAX))
  #x=merge(dax,y , y-lag(y), cud(y,wlen=7), y-SMA(y,7),dax-SMA(dax,200),cud(to.monthly(dax),wlen=7))
  
  #  fm.fund.factor.test() 
  # colnames(x)= c("dax","ifo","difo","cud","dSMA","faber","cudDax")
  #  purePlot(x)
  #  View(x)
  MWorld3 <<- T2$new(PortfolioName = "MWorld3", bench="DAX",visual = F, online=F)
  data<-MWorld3$t0data
  purePlot(mNorm(na.omit(data$prices)))
  #lösch ein paar extrem gut gelaufene Daten
  data.rm(data,spl("GOLD,JAKARTA_COMPOSITE,OIL"))  #OIL
  data$symbolnames
  
  #data.rm(data,spl("DOW"))  #OIL
  data$prices <- data.info(data,visual=T)#,ignore=C("ZINSEN"))
  #data.repair(prices=data$prices)
  #dax=data$prices[,"DAX"]  
  #fromTo(dax)
  #purePlot(mNorm(data$prices[,"DAX"]))
  #löschen
  prices=mNorm(data$prices)
  purePlot(mNorm(data$prices))
  fromTo(prices)
  #trainings-zeitraum beschränken
  ####################################################### Vorbereitung für das Training
  train.frame = fromtToS(data$prices)
  ls(data)
  global_arg<<-list(clos=data$prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices)) 
  global_objectId <<-paste("TREND","xx","signal.lm") 
  global_FastTrain <<-20 #heißt:  für jede dim werden 2 Schritte ausprobiert.
  
  #######################################################
  #Traings-Targets berechnen- werden fürs Training des classifiers benötigt
  
  data$Target   <- compute.Target.by.method(data=data, TargetMethod=0, visual=T,glaettung=5) #1
  fromToS(data$Target)
  Targets <<- data$Target
  ls(data)
  save(list=ls(data),file="MData/dax.Rdata",envir=data)
  #load("MData/dax.Rdata")
  train.data=NULL #der globale cashe für diese data.cloud
  source("MLib/classifier_randomForest.r")
  #berechne die dataCloud,  im rollarray: trainiere den randomForest, generiere Prognosen damit -- nutzte indi.Generic...()
  run.randomForest(data)   #<<<<<<<<<<<<<<<<<<<<<
  
  ##########################################################
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX",safe="TREASURY")
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX")
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM="DAX")
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,safe="REX")
  x=indi.Generic("signal.monthlyRoc", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1,safe="TREASURY")
  
  TrainIndicator( opti="GRID", indiName = "signal.Faber.base",  visual=T, TRAINSYM = "DAX")
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX")
  
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
}

#########################################################################
#########################################################################
#########################################################################

if (F)
  list_R_functions('MLib/indicators2.R')

##################################################################################
signal.Faber.I <-function(arg, par = mlist( sma.w=c(200,120,220,20)),visual=F,...)
{
  #p=mNorm(prices);sym=colnames(prices)
  #sma200 =SMA(na.omit(p),n=winLen)
  
  p=mNorm(arg$clos)
  sma200 = bt.apply.matrix(p, SMA, n=as.integer(par$sma.w))
  #p2=EMA(na.omit(p,20))
  signal = iif(p >= sma200,1,0)
  return(list(Signal=signal, Indi=list(ma=merge(sma200,p2,p))))
}
if (F)
  x=indi.Generic("signal.Faber.I", global_arg, par=list(sma.w=200),visual=T, TRAINSYM ="DAX")


#library(forecast)
#fit1 <- ets(x, model="ANA", damped=FALSE)
#fit2 <- ets(x, model="AAA", beta=0, damped=FALSE, lower=rep(0,4))


#model  wo die richtung des sma als indicator dient
#model wo die abweichung dient
#sma200 - modell wo vor einem entry,exit noch mal der lt-trend gecheckt wird, ob 
#auch der kanal verletzt wird (oder aber in.Trend.Pos heftig hoch ist)
#sma200 - wlen als funktion der lt-trend-steigung
#wo jede trading entscheidung aus dem forest kommen muss