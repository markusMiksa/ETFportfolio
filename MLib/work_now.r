#############################################################################################
#Marker:  # M1 bis # M5
#######################################################################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)

options(error = browser)

################################################################################################################
#######################################################################################

#lineare-regression - rollierend mit fensterbreite - gib den letzten fit-Wert zurück -> gelättete kurve
rollRegressionXTS.smooth1.dyn<-function(Y,win=60)
{
  #browser()
  vec=F
  if (dim(Y)[2]==1)
  {  Y = cbind(Y,Y)
     vec = T
  }
  dolm1 <- function(Y){ret=last(stats::lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y))$fitted.values)}
  
  ret=rollapplyr(Y, win, dolm1, by.column = F)
  colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s.smoothed%d",x,win))
  if (vec)
    ret = ret[,1]
  return(ret)
}

#g.Signal.D.zlema
#####################################################################################
#M1

if (F)
{
  ls(data)
  global_arg<<-list(clos=prices,dat=data)  #MANDATORY !!!!!
  global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  global_StartDate <<-  DateS(last(prices))
  globalTrainLevel <<-10   
  global_objectId <<-paste("TREND","xx","signal.lm") 
  
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=200),visual=T, TRAINSYM =-1)
  
  
  
  TrainIndicator( opti="GRID", indiName = "signal.lm",  visual=T, TRAINSYM = "DAX")
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
  
  ####################
}

if (F)
{
  prices=data.info(data)
  
  global_ParTable <<-NULL   #leere Parameter-Tabelle vorbereiten
  
  colnames(data$prices)
  prices = prices[,c(1,2,3,4,15)]
  colnames(prices)
  #  sym="BUND_FUTURE"
  for (sym in c(1,2,3,4,15))
  {
    
    p=data$prices[,sym]
    mP("%d %s",sym,colnames(p))
    
    colnames(global_arg$clos)
    sym=colnames(p)
    global_arg<<-list(clos=prices,dat=data)  #MANDATORY !!!!!
    global_commission = 0.00001   #sys.Run() nimmt diese commission !!!!
    
    global_StartDate <<-  DateS(last(prices))
    #globalTrainLevel <<- 3   
    global_FastTrain <<-   -10
    global_objectId <<-paste("TREND",sym,"signal.lm") 
    
    
    #x=indi.Generic("signal.lm", global_arg, par=list(win=68),visual=T, TRAINSYM ="EXX50_RI")
    #x=indi.Generic("signal.lm", global_arg, par=list(win=68),visual=T, TRAINSYM ="EXX50_RI",do.assemble.Signals=T)
    
    #x=indi.Generic("signal.lm", global_arg, par=list(win=68),visual=T, TRAINSYM =c(1,2,3,4,15),do.assemble.Signals=T)
    
    #x=indi.Generic("signal.lm", global_arg, par=list(win=250),visual=T, TRAINSYM ="EXX50_RI")
    
    #TrainIndicator( opti="GRID", indiName = "signal.lm",  visual=T, TRAINSYM = sym)
    
    ## jählich trainieren
    today.list = as.character(as.Date(index(m.to.quarterly(p["2000::"]))))
    today.list = as.character(as.Date(index(m.to.yearly(p["2000::"]))))
    
    for(today.i in today.list)
    {
      global_StartDate <<- today.i # DateS(last(prices["2008"]))
      print("###########################################################################")
      print(global_StartDate)
      print("###########################################################################")
      #browser()
      #TrainIndicator(global_StartDate_=global_StartDate,  opti="GRID", indiName = "signal.lm",  visual=F, TRAINSYM = sym)
      #M4
      TrainIndicator(global_StartDate_=global_StartDate,  opti="GRID", indiName = "signal.Faber.base",  visual=F, TRAINSYM = -1)
      
    }
    #x=indi.Generic("signal.lm", global_arg, par=list(win=68),visual=T, TRAINSYM =sym  ,do.assemble.Signals=T)
    #x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,do.assemble.Signals=T)
    
  }
  writeTable(table=global_ParTable,xlsName="global_ParTable.xls",rownames=T)
  View(global_ParTable)
  
  x=indi.Generic("signal.Faber.base", global_arg, par=list(sma.w=300),visual=T, TRAINSYM =-1,do.assemble.Signals=T)#,experiment="signal.faber.base")
  
  
  x=indi.Generic("signal.lm", global_arg, par=list(win=68),visual=T, TRAINSYM =-1  ,do.assemble.Signals=T)
  x=indi.Generic("signal.lm", global_arg, par=list(win=250),visual=T, TRAINSYM ="MG_METAL_INDEX",do.assemble.Signals=T)
  
  
  #(TRAINSYM=0:  alle gleich trainieren = default,  
  # TRAINSYM = 1..:  trainiere nur die Zeitreihe 1..)
  x=indi.Generic("signal.lm", global_arg, par=list(win=24),visual=T, TRAINSYM ="DAX")
  ####################
  
  channelStop()
  
  techStops(data$DAX)
  
}

###############################################################################
#M5               SPLINE-KINO
###############################################################################
if (F)
{
  #xtsPrice=p
  Y=coredata(xtsPrice)
  X=index(xtsPrice)
  sml=smooth.spline(x=Y , df=20.4)
  mchart(merge(xtsPrice,predict(sml)$y))
  
  sml=smooth.spline(x=Y , df=20.4)
  b=merge(xtsPrice,predict(sml)$y)
  mchart(b)
  
  #damit lässt sich eine Kristallkugel bauen:
  sig=xts(c(NA,sign(diff(predict(sml)$y,1))), index(xtsPrice))
  plotSigPrice(signal=sig,prices=xtsPrice,indi=list(spline=b[,2]))
  
  ############## aber live...
  p=dax
  
  today.list = as.character(as.Date(index(m.to.yearly(p["2000::"]))))
  today.list = as.character(as.Date(index(m.to.quarterly(p["2000::"]))))
  today.list = as.character(as.Date(index(m.to.monthly(p["2003::"]))))
  today.list = as.character(as.Date(index(m.to.weekly(p["2003::"]))))
  today.list = as.character(as.Date(index(p["2003::"])))
  
  res = merge(p,p); res[]=NA; i=0
  #"2008-01-11
  #2011-08-12
  #i=3
  min.wLen = 31
  for(today.i in today.list)
  {
    #today.i = today.list[i] 
    #today.i="2008-01-11"
    #today.i="2006-09-15"
    mP("%d %s ",i,as.character(today.i))
    pi = p[sprintf("::%s",today.i)]
    Y=coredata(pi)
    #sml=smooth.spline(x=Y , df=20.4)
    
    last.trend=find.last.trend.segment(pi,visual=F,glaettung=4, min.wLen=min.wLen)  #wenn wie 2008-01-11 ein crash erfolgt ihm erlaubt sein auch vor der üblichen mindestlaufzeit den trend zu verkürzen ... ??
    print(last.trend$x2)
    ls(last.trend)
    #last.trend$fs
    
    trend.start =last.trend$x2
  
    while(T)
    {
    last.trend.p=pi[sprintf("%s::",trend.start)]
    
    Y=coredata(last.trend.p)
    X=1:shape(Y)
    win=len(X)
    #Y=last(Y,min(len(Y),win)); X=last(x,min(len(x),win))
    
    #sml <- lm(Y~X)
    #sml <- loess(Y~X,span=0.99)
    
    
    m.lm=m.lm.fit(last.trend.p,getIndex=T,visual=F,level=.95)
    all.m=m.lm$m; m.chan=m.lm$fit$channel
    last.p=p[today.i]
    itp=in.Trend.pos(last.p, m.chan[today.i],visual=T)   
    
  
    
    #welch steigung hat der 90-er-trend und wie ist die itp im Trend, wie valide ist der trend 
    #gehe flat wenn     m.lm$r.squared < 0.06  &&  sign(itp) != sign(m.lm$m)
    #m sollte wenigstens 0.11  sein
    #hohe r.squrare  bei gleichzeitigem abs(itp) <=3 markieren intakte trends
    #wenn der dann noch weit weg vom all.time.high ist könnts interessant sein - wenn sein leads auch intakt sind
    #drehe nicht die position wenn  der Unterschied zwischen alter und neuer steigung kleiner ist als .. sondern geh flat
    #erst ein itp von 5 gegen den trend ist ein stop
    #relative lage von High Unter High, oder Low unter Low sind hilfeiche trendbestätiger ..
    
    if (m.lm$r.squared < 0.1  && as.Date(pre.trend.start )< as.Date(trend.start) ) #|| len() &&  sign(itp) != sign(m.lm$m)
      trend.start = pre.trend.start
    else 
       break
    
    }
    #visual:
    if (shape (pi)>min.wLen)
    {
      print(today.i)
      b=merge(p[sprintf("::%s",today.i)],last.trend$smooth,m.lm$fit$channel,xts(sml$fitted,index( last(p[sprintf("::%s",today.i)],win) )));mchart(b,main=sprintf("%s itp %d %s m %.3f r %.3f win: %d",as.character(trend.start),itp,as.character(today.i),m.lm$m, m.lm$r.squared,len(last.trend.p)));
      amark(last.trend$x2)
      browser()
      
    }
    last2=tail(sml$fitted,2)
    
    if (F)
    {
      sml=smooth.spline(x=coredata(p) , df=10.4)
      
      b=merge(p[sprintf("::%s",today.i)],predict(sml)$y);mchart(b);browser() 
    }
if (F)
{
    
    last2=tail(predict(sml)$y,2)
    Res = diff(last2,1)
    res[today.i,1]=Res
    res[today.i,2]=last(last2)
} 
    i=i+1
    pre.trend.start=trend.start
  }
  
  
  
  sml=smooth.spline(x=Y , df=10.4)
  b=merge(res[,2],predict(sml)$y)
  
  
  sig=sign(res[,1])
  plotSigPrice(signal=sig,prices=xtsPrice,indi=list(spline=b))
  
  #wenn er live wacker einem neuen Tal gefolgt ist und dann später merkt - das war nur ein eckchen .. dann läßer er es bei neuen plots halt weg ..
  ###########
  #das selbe gilt für lm und loess
  
  p=dax
  p=data$prices[,2]
  colnames(data$prices)
  today.list = as.character(as.Date(index(m.to.weekly(p["2007::"]))))
  res = merge(p,p); res[]=NA
  today.list = as.character(as.Date(index(p["2002::"])))
  for(today.i in today.list)
  {
    #today.i =first(today.list)  #TEST
    #print(today.i)
    Y=coredata(p[sprintf("::%s",today.i)])
    x=1:shape(Y)
    win=350
    Y=last(Y,min(len(Y),win)); X=last(x,min(len(x),win))
    
    sml <- lm(Y~X)
    #sml <- loess(Y~X,span=0.99)
    
    #visual:
    if (T)
    {
      b=merge(p[sprintf("::%s",today.i)],xts(sml$fitted,index( last(p[sprintf("::%s",today.i)],win) )));mchart(b);browser() 
    }
    last2=tail(sml$fitted,2)
    Res = diff(last2,1)
    res[today.i,1]=Res
    res[today.i,2]=last(last2)
  }
  sml=smooth.spline(x=coredata(p) , df=20.4)
  b=merge(res[,2],predict(sml)$y)
  
  sig=sign(res[,1])
  plotSigPrice(signal=sig,prices=p)
  #######################################################################
  prices=data$prices
  mchart(mNorm(prices[,c("DAX","ATX","CAC40","SWISS_MARKET")]))
  colnames(prices)
  
  ##################################################
  
  ### eckig
  
  plot(sml,type="l")
  library(splines)
  Fit = xtsPrice
  ispl <- polySpline(interpSpline(Y  ~ X,  X, bSpline = TRUE))
  Fit[]=ispl
  
  print( ispl )   # print the piecewise polynomial representation
  plot(xtsPrice)
  lines(Fit,col="blue")
  plot( ispl )    # plots over the range of the knots
  lines(coredata(xtsPrice))
  lines(coredata(xtsPrices))
  
  
}

#############################################################################################################################

if (F) #MMXnow
{
  period="years"
  #universum laden
  data=universe_hua_World(reload=F)
  models = run_all_signal(SAFE=SAFE,do.assemble.Signals=F,file="HuAWorld",LongShort="F",pdf=T,visual=T) 
  #modelle-ergebnisse laden
  load("Models/HuAFeb_4/Results.xls/HuAWorld_F.data")
  #modell-liste flach machen
  m=dels2flatList(models,datafile="HuAWorld_F")
  ls(m)
  
  Frame="2000:2014"
  
  signal.model.analysis(m,Frame="2000::2009",period= "years")
}

  
if (F)
  list_R_functions('MLib/work_now.r')


