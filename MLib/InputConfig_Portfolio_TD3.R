
################################################################################################################
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
##############################################################################################################


rollRange<-function(Y,win=1000)
{
  norm.i =function(clos){ra = range(clos,na.rm=T);ret= ra[2]-ra[1]}
  
  ret=rollapplyr(Y, win, FUN=norm.i, by.column = T)
  return(ret)
}

rollRange.2<-function(Y,win=1000)  #komisch
{
  ret= runMax(coredata(y),n=win,cumulative=F)-runMin(coredata(y),n=win,cumulative=F)
  return(ret)
}
if (F)
{
  
  PN=rollRange(p,90);plot(PN)
  mchart(merge(p,p+PN,p-PN))
  dp=runSD(p,260); mchart(merge(p,p+dp,p-dp))
  PN2=rollRange.2(p,9);plot(PN2)
  mchart(merge(p,p+PN2,p-PN2))
  
  PN=rollNorm(prices)
  mchart(PN)
}

rollRegressionXTS<-function(Y,win=60)
{
  #browser()
  vec=F
  if (dim(Y)[2]==1)
  {  Y = cbind(Y,Y)
     vec = T
  }
  dolm1 <- function(Y){ret=coef(stats::lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y))) }
  
  slope90=rollapplyr(Y, win, dolm1, by.column = F)
  
  #pick Dir nur die Steigungen raus 
  ret=slope90[,seq(2,ncol(slope90),2)]
  colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s.slope%d",x,win))
  if (vec)
    ret = ret[,1]
  return(ret)
}



#lineare-regression - rollierend mit fensterbreite - gib den letzten fit-Wert zur?ck -> gel?ttete kurve
rollRegressionXTS.smooth<-function(Y,win=60)
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
####################################################
#m0
#####################################################################################
# gib die frorecasts-zeitreihen zurück
#####################################################################################

rollRegressionXTS.forecast<-function(p=prices,win=200,n.ahead=4)
{
  dolm1 <- function(p,nplus){
    fit=lm.FITm(p=p+10000,visual=F,getIndex=T,level=.90,nplus=nplus)
    fct=(last(fit$channel[,1]))-10000
  }
  
  slope90=rollapplyr(p, win, dolm1, by.column = T,nplus = n.ahead)
}


if (F)
{
  slope200=rollRegressionXTS(p,200) 
  smooth200=rollRegressionXTS.smooth(p,100) 
  smooth200=rollRegressionXTS.smooth(p,200) 
  mchart(merge(p,smooth200,SMA(p,200),ZLEMA(p,200),EMA(p,50)))
  
  
}

trendwechsel<-function(p, smooth, visual=T)
{
  mchart(merge(p,smooth))
  m=g.Signal.r(smooth,20)
  zero=runquantile(abs(m),k =100,align="right",probs=0.1)
  #  mchart(merge(abs(m),zero))
  res=m[abs(m)<zero]
  amark(dat=index(res))
}

if (F)
{
  smooth = ZLEMA(p,200)
  smooth=rollRegressionXTS.smooth(p,100) 
  trendwechsel(p,smooth)
}


rollRegressionXTS.2<-function(X,Y,win=60,lag.n=0)
{
  #browser()
  x=colnames(X)
  y=colnames(Y)
  lag.y=lag(Y,lag.n)
  #first.y=first(na.omit(lag.y))
  #lag.y[is.na(lag.y)] <-first.y
  b=na.omit(merge(X,lag.y))
  colnames(b)=spl("x,y")
  
  #Y=coredata(b[,2]);X=coredata(b[,1])
  #fit <- lm.fit(cbind(1,X),Y)
  #fit$coefficients
  
  #cbind(1,...) ist wichtig
  dolm1 <- function(Y){ret=coef(stats::lm.fit(cbind(1,coredata(Y[,1])),coredata(Y[,2]))) }
  
  slope90=rollapplyr(b, win, dolm1,by.column = F)
  
  #pick Dir nur die Steigungen raus 
  ret=slope90[,seq(2,ncol(slope90),2)]
  
  colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s.%s_slope%d",x,y,win))
  return(ret)
}
if (F)
{
  sxx50=data$prices[,"SXX50"]
  dax=data$prices[,"DAX30"]
  
  
  HotLags2.cc(sxx50,dax)
  
  if (F)
  {
    #http://www.stat.pitt.edu/stoffer/tsa3/R_toot.htm
    require(dynlm)                          # load the package
    fit = dynlm(cmort~part + lag(part,-4))  # assumes cmort and part are ts objects, which they are
    ded = ts.intersect(cmort,part,part4=lag(part,-4), dframe=TRUE) # align them in a data frame
    fit = lm(cmort~part+part4, data=ded, na.action=NULL)           # now the regression will work
    summary(fit) 
    #########
    # test data
    set.seed(1)
    y <- ts(1:10 + rnorm(10, 0, 0.1))
    
    # fit model
    library(dyn)
    y.lm <- dyn$lm(y ~ lag(y,-1))
    
    # use predict
    tail(predict(y.lm, list(y = y)), 1)
    
    # or multiply by coefficients giving same result coef(y.lm) %*% c(1, tail(y,1))
    
    # Now try it using quantile regression
    library(quantreg)
    y.rq <- dyn$rq(y ~ lag(y,-1))
    tail(predict(y.rq, list(y = y)), 1)
    coef(y.rq) %*% c(1, tail(y,1)) 
  }
  
  
  hl=HotLags(dax);
  hl=ifelse(len(hl)>1,hl[2],1)
  
  ar.1=rollRegressionXTS.2(dax,dax,lag.n=hl)
  plot(dax)
  plot(ar.1)
  #ar.1=rollRegressionXTS.2(dax,dax,lag.n=0)
  
  
  hl=HotLags(dax);
  plot(dax)
  sym=colnames(dax)
  sapply(hl,function(x)
  {
    ar.1=rollRegressionXTS.2(dax,dax,lag.n=x)
    ar.1 = ar.1-runMean(na.omit(ar.1),60)
    plot(ar.1,main=sprintf("%s lag %d mean %f",sym,x,mean(ar.1,na.rm=T)))
    print(mean(ar.1))
    ar.1
  })
  
  require(astsa)
  data(cmort, part)
  ded = ts.intersect(cmort,part,part4=lag(part,-4), dframe=TRUE) # align them in a data frame
  fit = lm(cmort~part+part4, data=ded, na.action=NULL)           # now the regression will work
  summary
  require(dynlm)                          # load the package
  fit = dynlm(cmort~part + lag(part,-4))  # assumes cmort and part are ts objects, which they are
  
  
  
  
  all=foreach(x = hl,.combine="cbind") %do%
{
  ar.1=rollRegressionXTS.2(dax,dax,lag.n=x)
  ar.1 = ar.1-runMean(na.omit(ar.1),60)
  plot(ar.1,main=sprintf("%s lag %d mean %f",sym,x,mean(ar.1,na.rm=T)))
  print(mean(ar.1))
  ar.1
}
all=rowSums(all,na.rm=T)
all.xts=dax[-(1:60)]
all.xts[]=all
mchart(merge(all.xts,0))
######################################

#berechne die summe der drei st?rksten hotlags- autoregressionswinkel
lapply(data$symbolnames,function(sym)
{
  w=60
  topn=5
  dax=data$prices[,sym]
  HL= HotLags2(dax)
  #  HL$value =HL$value[-1]
  #  HL$lag= HL$lag[-1]
  #die 3 st?rksten autoregession-lags
  hl=head(HL$lag,topn)
  mP("%s %d",sym,len(hl))
  print(HL)
  
  #autoregressions-winkel berechnen 
  k=sapply(hl,function(x)rollRegressionXTS.2(dax,dax,lag.n=x,win=w))
  #merge die regressions-winkel-xts e zusammen
  K=  foreach(ki = k,.combine = "merge") %do%
{  
  #     ki =ki-runMean(na.omit(ki),w) 
  plot(ki)
  ki
}
#HotLag-Gewichte dran multiplizieren
if (T)
{
  K=( t(t(K)*(head(HL$value,topn))))
  #wieder zum xts machen
  K=   xts( K , as.Date(rownames(K)))
}
#und summieren
rs=rowSums( K )    
ar=K[,1];ar[]=rs
#hier den mittelwert abziehen damit wir 0-mean-reverting werden
ar=ar-runMean(na.omit(ar),160)
#  ar = SMA(ar,10)
mPlots(dax,merge(ar,0))
b=merge(ar,dax)
plotSigPrice(signal=sign(b[,1]),prices=b[,2])
ar
})


#head(shingle(dax))  


#sym1="DAX30"
#dax=data$prices[,sym1]
#  lapply(data$symbolnames,function(sym)

lapply(colnames(  euro.macros),function(sym)
{
  w=60
  topn=5
  #p=data$prices[,sym]
  p=  euro.macros[,sym]
  HL= HotLags2.cc(dax,p,visual=T)
  if (len(HL)>0)
  {
    #  HL$value =HL$value[-1]
    #  HL$lag= HL$lag[-1]
    #die 3 st?rksten autoregession-lags
    hl=head(HL$lag,topn)
    mP("%s %d",sym,len(hl))
    print(HL)
    browser()
  }
}  )
}

if (F)
{
  sym1= "DAX30"
  sym2="SXX50"
  
  
  #foreach(sym2 =data$symbolnames) %do%
  #{
  foreach(sym2 =colnames(euro.macros)) %do%
{
  
  #block bilden
  
  b=na.omit(merge(mROC(data$prices[,sym1]),mROC(euro.macros[,sym2])))
  
  
  
  
  b=na.omit(merge(mROC(data$prices[,sym1]),mROC(data$prices[,sym2])))
  
  b=last(b,1001)
  Sym1=coredata(b[,1])[1:999]
  Sym2=coredata(b[,2])[1:999]
  #crosscorrelation
  k=ccf(Sym1,Sym2,lag.max=10 )
  #lag0 entfernen - ist meist maximal
  lag.0=which(k$lag==0)
  k$lag=k$lag[-lag.0]
  k$acf=k$acf[-lag.0]
  maxi=which(abs(k$acf)==max(abs(k$acf)))
  lag.max=k$lag[maxi]
  corr.max=k$acf[maxi]
}
ls(k)

str(k)
plot(Sym1)
plot(Sym2)
dim(Sym1)
dim(Sym2)
head(Sym1)
ncol(Sym2)
sessionInfo()
dim(Sym1)
dim(Sym2)
ccf(c(1,2,4),c(3,2,4))

acf(Sym2,ylab = "cross-correlation")
}


w.rollRegressionXTS<-function(Y,win=60)
{
  #browser()
  vec=F
  if (dim(Y)[2]==1)
  {  Y = cbind(Y,Y)
     vec = T
  }
  
  dolm <- function(Y){X=as.matrix(c(1:shape(Y)));W=X;coef(lm.fit(X,coredata(Y),w=W))}
  
  slope90=rollapplyr(Y, win, dolm, by.column = F)
  #pick Dir nur die Steigungen raus 
  ret=slope90[,seq(2,ncol(slope90),2)]
  colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s.slope%d",x,win))
  if (vec)
    ret = ret[,1]
  return(ret)
}


############# langsamer und lm basiert
rollRegressionXTS.lm<-function(Y,win=60)
{
  #browser()
  vec=F
  
  if (dim(Y)[2]==-1)
  {  Y = cbind(Y,Y)
     vec = T
  }
  #  dolm <- function(Y){coef(lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y)))}
  
  dolm<-function(Y) {x=as.matrix(c(1:shape(Y))); y=coredata(Y);res=coef(lm(y~x))[2]}
  
  slope90=rollapplyr(zoo(Y), win, dolm, by.column = T)
  #pick Dir nur die Steigungen raus  ???  nach Umsellung auf r.3.0 anders
  #ret=slope90[,seq(2,ncol(slope90),2)]
  ret=slope90
  #  browser()
  
  #dim(ret)
  #dim(Y)
  #len(colnames(Y))
  colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s.slope%d",x,win))
  if (vec)
    ret = ret[,1]
  ret=as.xts(ret)
  return(ret)
}




#SCHROTT ? winvec ist ein vektor, so lang wie y und enth?lt die jeweils zu nutzende Fensterl?nge
rollRegressionXTSw.lm<-function(Y,winvec=c(60,30,20))
{
  #geth so erst mal nur f?r 
  
  #  dolm <- function(Y){coef(lm.fit(cbind(Intercept=1,index(Y)-index(first(Y))),coredata(Y)))}
  
  #dolm<-function(Y) {wlen=Y[,1];browser();res=coef(lm.fit(x=as.matrix(c(1:shape(Y)), y=coredata(Y[,-1])));  as.vector(res)}
  
  #dolm<-function(Y) {x=as.matrix(c(1:shape(Y))); y=coredata(Y);res=coef(lm(y~x))[2]}
  
  
  dolm<-function(Y) {wlen=nval(last(Y[,1])); x=as.matrix(c(1:wlen)); y=coredata(last(Y[,-1],wlen)); res=coef(lm(y~x))[2]}
  
  win=max(winvec)
  slope90=rollapplyr(zoo(cbind(winvec,Y)), win, dolm, by.column = F)
  
  
  #  browser()
  # win=max(winvec)
  #slope90=rollapplyr(zoo(merge(winvec,Y)), win, dolm, by.column = F)
  #pick Dir nur die Steigungen raus  ???  nach Umsellung auf r.3.0 anders
  #ret=slope90[,seq(2,ncol(slope90),2)]
  ret=slope90
  #dim(ret)
  #dim(Y)
  #len(colnames(Y))
  
  #browser()
  #colnames(ret)=  sapply(colnames(Y),function(x) sprintf("%s_wslope%d",x,win))
  
  
  ret=as.xts(ret)
  return(ret)
}
########################################################
##########################################################
nval<-function(x)
{
  res=0
  if (len(x)==0)
    return(0)
  if (is.xts(x) )
    return(as.numeric(coredata(x)))
  if (is.vector(x) )
    return(as.numeric(x))
  if (is.na(x))
    res=0
  res=try(as.numeric(x))
  return(res)
}
#########################################################################
#schreib die wichtigste Ergebnisse eines bt-Experiments auf pdf und xls
#########################################################################

pdf_Report<-function(models,repName="repName",data=NULL,R.arg=list(pdf.report=spl("compareModels,periods,xls")) ,out=NULL)
{

  print("######### pdf_Report #########")
  if (!has(R.arg,"pdf_report","NO")) 
  {
    pdf.f <- sprintf("Models/%s/%s.pdf", dataSet, repName)
    mP("Write Data Charts to %s",pdf.f)
    #  browser()
    #pdf(file = pdf.f, width=8.5, height=11)  #...............................................
    pdf(file = pdf.f, paper="a4r",width=0, height=0)#
    
    #DataInfo<<-NULL
    textplot(repName)
    
    print(ls(models))
    if( has(R.arg,"pdf.report","compare"))   
      k=compareViewModels(models,data$prices,alloc=T)
    if( has(R.arg,"pdf.report","periods"))  
      custom.period.chart(models) 
    mdev.off()   #............................................................................................
    #mdev.off()
  }
  if( has(R.arg,"pdf.report","xls"))
    writeModelDataXls(models,xlsName=sprintf("Models/%s/Results.xls/%s.xls",dataSet,repName),out)#, universe=DataInfo)
}
#########################################################################
# get Transaktions
#########################################################################

getTrades<-function(model)
{
  return(  model$trade.summary$trades)
}

#########################################################################
#########################################################################


showNa<-function(p)
{
  mP("showNa ------>")
  cat(class(p))
  cat(str(p))
  print ("range")
  print(range(p))
  #browser()  
  if (!is.null(dim(p))  && dim(p)[2] >1)
  {print("matrix")
   apply(p,2,function(col) col[which(is.na(col))])
  }
  else
  {print ("vector")
   k=(p[!is.finite(p)])
   cat(head(p))
  }
}
#########################################################################
#http://stackoverflow.com/questions/5671149/permute-all-unique-enumerations-of-a-vector-in-r
#permutate(c(1,3,5))
#siehe auch gtools - lib
#########################################################################
permutate <- function(d) {
  dat <- factor(d)
  N <- length(dat)
  n <- tabulate(dat)
  ng <- length(n)
  if(ng==1) return(d)
  a <- N-c(0,cumsum(n))[-(ng+1)]
  foo <- lapply(1:ng, function(i) matrix(combn(a[i],n[i]),nrow=n[i]))
  out <- matrix(NA, nrow=N, ncol=prod(sapply(foo, ncol)))
  xxx <- c(0,cumsum(sapply(foo, nrow)))
  xxx <- cbind(xxx[-length(xxx)]+1, xxx[-1])
  miss <- matrix(1:N,ncol=1)
  for(i in seq_len(length(foo)-1)) {
    l1 <- foo[[i]]
    nn <- ncol(miss)
    miss <- matrix(rep(miss, ncol(l1)), nrow=nrow(miss))
    k <- (rep(0:(ncol(miss)-1), each=nrow(l1)))*nrow(miss) + 
      l1[,rep(1:ncol(l1), each=nn)]
    out[xxx[i,1]:xxx[i,2],] <- matrix(miss[k], ncol=ncol(miss))
    miss <- matrix(miss[-k], ncol=ncol(miss))
  }
  k <- length(foo)
  out[xxx[k,1]:xxx[k,2],] <- miss
  out <- out[rank(as.numeric(dat), ties="first"),]
  foo <- cbind(as.vector(out), as.vector(col(out)))
  out[foo] <- d
  t(out)
}

#########################################################################
#combine2(spl("a,b,c"),spl("Vater,Mutter"))
#
#x <- seq(0,1,0.05)
#y <- seq(0,1,0.05)
#xy <- expand.grid(x,y)
#newdat <- data.frame(y1=0, x1=xy[,1], x2=xy[,2])
#
#expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),  sex = c("Male","Female"))
#combn(letters[1:4], 2)

#
#########################################################################

combine2 <-function(v1,v2,sep="")
{require(utils)
 
 sort(apply(expand.grid(v1, v2), 1, paste, collapse = sep, sep = ""))
}

#randperm(seq(2, 10, by=2))  #random permutations  in pracma

#########################################################################
#########################################################################

meanEQ <-function(models)
{
  for(i in c(2:len(ls(models))))
  {  #ignoriere buy hold
    if (i==2)
      eq = models[[i]]$equity
    else
      eq = eq + models[[i]]$equity
  }
  
  norm_Win(2)
  plot(data$prices)
  plot(eq)
  ret = mROC(eq)
  compute.sharpe(ret)
  compute.max.drawdown(eq)
  compute.cagr(eq)
}
#########################################################################
#########################################################################

#l?sche alle Eintr?ge in a deren Dat?mer in b sind
killallIn<-function(Dax,b)
{  
  C = Dax[-c(xts2Index(index(b),Dax)),]  
}
#killallIn(Dax,b)

#########################################################################
#########################################################################

clr<-function()
{
  
  try(mdev.off()>1 )
  
}

mdev.off<-function()
{
  mP("mdev.off")
  dev.off()
}

#########################################################################
# wie na.omit aber f?r mehr spaltige xts
#########################################################################

allFinite<-function(dscd) 
{apply(dscd,2, function(Col) Col[is.finite(Col)])
}
#########################################################################


#### gibt die Zahlen 1.. des Index der Zeitreihe
Index<-function(xtsPrice)
{
  return(endpoints(xtsPrice,"days")[-1])
  
  
  
  x=time(xtsPrice)
  xi=as.numeric(x)-as.numeric(x[1])+1
  return(xi)
}

#########################################################################
#gem. acf sind die folgende Lags die spannenden AutoKorrelationen
#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
#schau Dir nur maximal n  Lags an !!
#Gib die Indizes der Lags.. nach Wichtigkeit sortiert zur?ck.

#########################################################################

HotLags<-function(xtsPrice,n=60,visual=F)
{
  xtsPrice=xtsPrice[,1]
  #beer<-ts(coredata(xtsPrice),freq=12)
  if (visual)
    acf(ts(coredata(mROC(xtsPrice)),freq=7), lag.max=n)
  xacf=acf(ts(coredata(mROC(xtsPrice)),freq=7), lag.max=n,plot=F)
  
  ci=0.95
  sigLvl=qnorm((1 + ci)/2)/sqrt(xacf$n.used)
  
  Acf=xacf$acf
  acfi=order(abs(Acf),decreasing=T)
  #ACf = abs(Acf[acfi])
  ACf = (Acf[acfi])
  mcfT= last(which(abs(ACf) >sigLvl))  #so viele der n Lags sind significant
  #browser()
  hotLags=acfi[1:mcfT] 
  value = ACf[1:mcfT]
  
  hotLags= xacf$lag[xacf$acf  %in%  value] 
  hotLags = xacf$lag[acfi]
  
  return(hotLags)  
}

##########################################################################
# Wie HotLags2 aber gib einen 2. Vector mit den vorzeichenbehafteten
# Werten der Lags zur?ck.
##########################################################################
HotLags2<-function(xtsPrice,n=60,nLag=10,visual=F)
{
  xtsPrice=xtsPrice[,1]
  #beer<-ts(coredata(xtsPrice),freq=12)
  if (visual)
    acf(ts(coredata(mROC(xtsPrice)),freq=7), lag.max=n)
  
  xacf=acf(ts(coredata(mROC(xtsPrice)),freq=7), lag.max=n,plot=F)
  
  ci=0.95
  sigLvl=qnorm((1 + ci)/2)/sqrt(xacf$n.used)
  
  Acf=xacf$acf
  acfi=first(order(abs(Acf),decreasing=T),nLag)
  
  #ACf = abs(Acf[acfi])
  ACf = (Acf[acfi])
  mcfT= last(which(abs(ACf) >sigLvl))  #so viele der n Lags sind significant
  #browser()
  res = list()
  
  res$lag=acfi[1:mcfT] 
  
  res$value = ACf[1:mcfT]
  #  res$lag = xacf$lag[xacf$acf  %in%  res$value] 
  #  res$lag = xacf$lag[acfi]
  
  return(res)  
}
##########################################################################
# Wie HotLags2 aber gib einen 2. Vector mit den vorzeichenbehafteten
# Werten der LEADS von xtsPrice  zur?ck.
#und rechnet auch noch das regressions-modell
#h?ngt xtsPrice2(Y) von einer xtsPrice(X) ab die zeitlich vorgelagert ist.
#lead-lag finden mit ccf - voraussetzung:  whiten
#https://onlinecourses.science.psu.edu/stat510/?q=node/75

#mit whitening
#Ergebnis:  arima.fit,  wichtigkeit, lag, fit (y~x), lead.dat
##########################################################################
HotLags2.cc<-function(xtsPrice2,xtsPrice,n=31,nLag=5,visual=F,main="", ci=0.99555)
{
  if (visual)
    par(mar=c(4,4,1,1))
  xtsPrice=xtsPrice#[1:500,1]
  xtsPrice2=xtsPrice2#[1:500,1]
  sym=colnames(xtsPrice)  
  #beer<-ts(coredata(xtsPrice),freq=12)
  #  if (visual)
  #    acf(ts(coredata(mROC(xtsPrice)),freq=7), lag.max=n)
  
  #b=merge(mROC(xtsPrice),mROC(xtsPrice2))
  res = list(wichtigkeit=0,lag=0)  
  
  b=merge((xtsPrice),(xtsPrice2))
  #whitening .......................................>>>>>>>>> https://onlinecourses.science.psu.edu/stat510/?q=node/75
  library(forecast)
  x=ts(b[,1],356)
  y=ts( b[,2],356)
  
  ar1model=auto.arima(x)
  res[["arima.fit"]]=ar1model
  
  pwx=ar1model$residuals    #x  bereinigt um den arima-trend  ... sol ?berlagert der trend alle ccf-informationen
  newpwy <- residuals(Arima(y,model=ar1model))  #y gefiltert  
  #<<<<....................... whitening
  xacf= ccf (pwx,newpwy,na.action=na.omit,lag.max=n, plot=F, main=sprintf("%s %s",sym,main)) 
  # xacf=ccf(coredata(b)[,1],coredata(b)[,2], lag.max=n,plot=visual,main=main)
  # ci=0.99
  #  ci=0.99555
  sigLvl=qnorm((1 + ci)/2)/sqrt(xacf$n.used)
  sigLvl = sigLvl*1.2
  #plot(x=xacf$lag,y=xacf$acf,type="h")
  #abline(h=sigLvl,col="blue")
  # 
  
  Acf=xacf$acf
  acfi=first(order(abs(Acf),decreasing=T),nLag)
  acfi = acfi[xacf$lag[acfi]<0] #nur die leading
  
  #ACf = abs(Acf[acfi])
  ACf = (Acf[acfi])
  mcfT= which(abs(ACf) >sigLvl)  #so viele der n Lags sind significant
  if (len(mcfT)>0)
  {
    ACf=ACf[mcfT]
    mcfT=len(mcfT)
    #browser()
  }
  else
  {
    #par(ma=c(4,4,2,1))
    #plot(x=xacf$lag,y=xacf$acf,type="h",main=sprintf("%s -> %s NO leads",sym,colnames(xtsPrice2)))
    #abline(h=sigLvl,col="blue"); abline(h=-sigLvl,col="blue")
    
  }
  
  if(len(mcfT) < 1)
    return(res)
  
  res$value = ACf[1:mcfT]
  res$wichtigkeit = sum(abs(res$value)-sigLvl)
  acfi=acfi[1:mcfT]
  
  res$lag = xacf$lag[acfi]
  
  #schnell noch das regressions-modell dazu berechnen:
  
  if (is.null(sym))
    sym="sym"
  dat=data.frame(
    merge(y=xtsPrice2, foreach(lagi = res$lag, .combine="cbind") %do%
{
  spalte=lag(xtsPrice, -lagi)
  colnames(spalte)=sprintf("%s.lead%d",sym,lagi)
  spalte
} ) )

res$fit= lm(y~., dat) 
res$lead.dat = xts(dat,index(xtsPrice2))


#xacf$lag[xacf$acf  %in%  res$value] 
if (visual)
{
  par(ma=c(4,4,2,1))
  
  plot(x=xacf$lag,y=xacf$acf,type="h",main=sprintf("%s -> %s %d leads",sym,colnames(xtsPrice2),len(res$lag)))
  abline(h=sigLvl,col="blue"); abline(h=-sigLvl,col="blue")
  #abline(v=res$lag,col="red")
  lines(res$lag,res$value,type="h",col="red")
  mP("%s %s %f",colnames(xtsPrice),colnames(xtsPrice2),res$wichtigkeit)
  print(res$fit)
  print("wichtigkeit der leads")
  print(res$value)
  mP("%s %d",DateS(last(xtsPrice)), res$lag)
  
}
else
  mP("%s %d",DateS(last(xtsPrice)), res$lag)
return(res)  
}
if (F)
{
  
  #whitening
  if (F)
  {
    #ein konstruierte Zeitreihe y die auf 2 leads (-3,-4) eines arima (1,1,0) - prozesses basiert
    x = arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200) 
    z = cbind(x, lag(x,-3), lag(x,-4))  #3 prognose-spalten
    y = 15+0.8*z[,2]+1.5*z[,3]   #die konstruierte Target Zeitreihe h?ngt linear von z.1..3 ab,  wobei diese zum Teil gelaged sind
    ccf(z[,1],y,na.action = na.omit) #man sieht nichts
    
    a = cbind(y,lag3x = lag(x,-3), lag4x=lag(x, -4))  #das ist z  ..
    lm(y ~lag3x+lag4x, data = a, na.action = na.omit)  #findet genau die formel von y  - aber nur wenn du die lags weisst
    
    Y=m.xts(y)
    X=m.xts(x)
    plot(X)
    plot(Y)
    
    #ccf allein taugt nicht:
    ccf(ts(na.omit(y)),ts(na.omit(x)))
    ccf(ts(na.omit(diff(y))),ts(na.omit(diff(x))))
    
    hl=HotLags2.cc(diff(Y),diff(X),visual=T)
    ls(hl)
    inspect(hl)
    
  }
  
  
  sxx50=data$prices[,"SXX50"]
  dax=data$prices[,"DAX30"]
  dim(sxx50)
  dim(dax)
  
  HotLags2.cc(last(dax,500),last(dax,500),visual=T)
  
  HotLags2(last(dax,500),visual=T)  
}

#######################################################################
# Gibt einen Vector mit den mom-Werten f?r den letzten Kurs in prices
#wobei die mom-Lags den nLag wichtigsten  HotLags entsprechen
#Das Teil ist Multidim einsetzbar - berechnet aber NUR f?r den LEZTEN Wert
#in prices die HotLagMom - ... brauch mindestens 100 Tag in prices !!
#######################################################################
HotLagMom<-function(prices, nLag=10,mode="justRowSum",visual=F) { 
  
  Res=  unlist(lapply(colnames(prices),
                      function(colN)
                      {
                        close=na.omit(prices[,colN])
                        today = DateS(last(close))
                        hotLag2 = HotLags2(close,n=100-1,nLag=nLag,visual=visual)  #schau Dir die letzten 100 Tage an
                        #resHOT__[today]<<-sum(hotLag2$lag)
                        if (visual)
                        {
                          mP("%s %s ",today,toString(hotLag2$lag))
                          mP("%s %s ",today,toString(hotLag2$value))
                        }
                        maxL = min(len(hotLag2$lag),nLag)  #maximal 10 HotLags-Werte ber?cksichtigen
                        diffLag = lapply(c(1:maxL),
                                         function(x) 
                                         {
                                           LAG = hotLag2$lag[x]; VAL = hotLag2$value[x];
                                           #if (sum(hotLag2$lag) < 2 && LAG==1) LAG=zlemaN  #oft gibts nur den HotLag 1 .. dann muss ein anderer Wert ran... 
                                           last(momentum(close,n=LAG))*VAL  #gewichtet mit der Wichtigkeit es Lags
                                           #ROC(ZLEMA(close, lag=abs(LAG))) * -VAL
                                           #bad: ROC(ZLEMA(close, lag=abs(LAG)),n=abs(LAG)) * -VAL
                                         })
                        #bilde ?ber alle Lag-Momentum-Werte die (data.frame)-Row-Summe
                        res = (na.omit(as.xts(data.frame(diffLag))))
                        rs=rowSums(res)
                        if (mode=="justRowSum")
                          return(rs)
                      }) )
  
  names(Res) = sapply(colnames(prices),function(x) paste("HotLag",x,sep="."))
  return(Res)
  #res = cbind(rowSum=rs,res)
  #print(res)
  #return(res)
  #  res[] = rowSums(res)
  #res = last(res[,1]) 
  #
}
if (F)
  x=HotLagMom(prices,nLag=20)

#########################################################################

last = xts::last
globalCAT=""  #wenn "" wird auf alles gestoppt  .. wenn "no"  auf nichts -sondern immer nur auf das zu pos passende..

if (F)
  list_R_functions('Mlib/InputConfig_Portfolio_TD.R')

#########################################################################
#http://cartesianfaith.wordpress.com/2013/03/10/better-logging-in-r-aka-futile-logger-1-3-0-released/
#MM_LOG:
#library(futile.logger)
#flog.info("Got covariance matrix")
#flog.debug("This %s print", "won't")
#flog.warn("This %s print", "will")

#flog.info("This inherits from the ROOT logger", name='logger.a')
#flog.threshold(DEBUG, name='logger.a')
#flog.debug("logger.a has now been set to DEBUG", name='logger.a')
#flog.debug("But the ROOT logger is still at INFO (so this won't print)")

#flog.appender(appender.file("other.log"), name='logger.b')
#flog.info("This writes to a %s", "file", name='logger.b')


DateS<-function(d)
{
  return(toString(as.Date(index(d))))
}


####################################################################
#Filter aus der data-Liste lediglich die items raus die auch unter
#data$symbolnames gelistet sind
#data2=isSymbol(data)
####################################################################

isSymbol<-function(data)
{
  res = list()
  for(sy in data$symbolnames)
  {
    print(sy)
    print(dim(data[[sy]]))
    
    res[[sy]]=data[[sy]]
  }
  
  return(res)
}

###############################################################################
# Wenn 
#position.score = bt.apply(data, function(x)  TSI(HLC(x)) )    
#auf nen Bug l?uft geht:
#position.score = TSIsymbol(data)
#################################################################################
TSIsymbol<-function(data,sym=NULL,n=10)
{
  res = list()
  if (is.null(sym))
  {
    for(sy in data$symbolnames)
    {
      print(sy)
      #print(dim(data[[sy]]))
      
      re=res[[sy]]=TSI(HLC(data[[sy]]),n=n)
      re[!is.finite(re)] <-0
      res[[sy]]=re
    } 
    block=NULL
    for(sy in names(res))
    {
      b=na.omit(merge(data$prices[,sy],res[[sy]]))
      re=b[,2]
      if (is.null(block))
        block=re
      else
        block=merge(block,re)
    }   
  }
  else
  {
    sy = sym
    res[[sy]]=TSI(HLC(data[[sy]]))
    block=as.xts(res[[sy]])
  }
  
  return(block)
}


#########################################################################
#########################################################################

plotM<-function(price,Today=Sys.Date())
{
  plot(price)
  abline(v=as.POSIXct(as.Date(Today)),col=col.add.alpha("red" , 80),lwd=3)
}

## plotOBOS -- displaying overbough/oversold as eg in Bespoke's plots
##
## Copyright (C) 2010 - 2011  Dirk Eddelbuettel
##
## This is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
library(RColorBrewer )

plotOBOS <- function(symbol, n=50, type=c("sma", "ema", "zlema"), years=1, blue=TRUE) {
  
  today <- Sys.Date()
  X <- getSymbols(symbol, src="yahoo", from=format(today-365*years-2*n), auto.assign=FALSE)
  x <- X[,6]                          # use Adjusted
  
  type <- match.arg(type)
  xd <- switch(type,                  # compute xd as the central location via selected MA smoother
               sma = SMA(x,n),
               ema = EMA(x,n),
               zlema = ZLEMA(x,n))
  xv <- runSD(x, n)                   # compute xv as the rolling volatility
  
  strt <- paste(format(today-365*years), "::", sep="")
  x  <- x[strt]                       # subset plotting range using xts' nice functionality
  xd <- xd[strt]
  xv <- xv[strt]
  
  xyd <- xy.coords(.index(xd),xd[,1]) # xy coordinates for direct plot commands
  xyv <- xy.coords(.index(xv),xv[,1])
  
  n <- length(xyd$x)
  xx <- xyd$x[c(1,1:n,n:1)]           # for polygon(): from first point to last and back
  
  if (blue) {
    blues5 <- c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C") # cf brewer.pal(5, "Blues")
    fairlylight <- rgb(189/255, 215/255, 231/255, alpha=0.625) # aka blues5[2]
    verylight <- rgb(239/255, 243/255, 255/255, alpha=0.625) # aka blues5[1]
    dark <- rgb(8/255, 81/255, 156/255, alpha=0.625) # aka blues5[5]
  } else {
    fairlylight <- rgb(204/255, 204/255, 204/255, alpha=0.5)         # grays with alpha-blending at 50%
    verylight <- rgb(242/255, 242/255, 242/255, alpha=0.5)
    dark <- 'black'
  }
  
  plot(x, ylim=range(range(xd+2*xv, xd-2*xv, na.rm=TRUE)), main=symbol, ) 
  # basic xts plot
  lines(x,col=fairlylight)
  polygon(x=xx, y=c(xyd$y[1]+xyv$y[1], xyd$y+2*xyv$y, rev(xyd$y+xyv$y)), border=NA, col=fairlylight)   # upper
  polygon(x=xx, y=c(xyd$y[1]-1*xyv$y[1], xyd$y+1*xyv$y, rev(xyd$y-1*xyv$y)), border=NA, col=verylight)# center
  polygon(x=xx, y=c(xyd$y[1]-xyv$y[1], xyd$y-2*xyv$y, rev(xyd$y-xyv$y)), border=NA, col=fairlylight)   # lower
  lines(xd, lwd=2, col=fairlylight)   # central smooted location
  lines(x, lwd=3, col=dark)           # actual price, thicker
  invisible(NULL)
}

if (F)
  plotOBOS("^GDAXI")


#########################################################################
#rportModels ist ein Liste mit SysInvestor-Modellen
#deren Daten werden in ein xls-workbook geschrieben:
#Ergebnisse, Gewichte, Equity
#########################################################################

data.frame2german.sep<-function(models){
  #return(models)
  dt = data.frame(models)
  for(col in 1:ncol(dt))
    dt[,col ] <- try(nval(dt[,col ]))# ( gsub( ".", ",",dt[,col],fixed=TRUE))
  dt
}

if (F)
  writeModelDataXls(models,"test.xls")


writeModelDataXls<-function(reportModels,xlsName="modelData",universe=NULL,out=NULL)
{
  if (is.null(reportModels))
     return.
  
  mP("writeModelDataXls %s",xlsName)
  

  dir.create(dirname(xlsName),recursive=T)
  if (file.exists(xlsName))   file.remove(xlsName)
  wb <- loadWorkbook(xlsName, create = TRUE)
  # Create a worksheet called 'mtcars'
  if (!is.null(universe))
  {
    createSheet(wb,name="Universe")
    writeWorksheet(wb, data.frame(universe), sheet = "Universe",rownames="Number")
  }
  createSheet(wb, name = "Ergebnisse")
  # Write built-in dataset 'mtcars' to sheet 'mtcars' created above
  if (is.null(out))
    out = plotbt.strategy.sidebyside(reportModels, return.table=T)
  
  e = NULL
  
  for(mod in names(out)) 
  {
    if (is.null(e))
      e <- (data.frame2german.sep(out[[mod]]))
    else
      e <- rbind(e,data.frame2german.sep(out[[mod]]))
  }
  
  writeWorksheet(wb, e, sheet = "Ergebnisse",rownames="Number")
  if (len(which(colnames(data$prices)==SAFE))==0)
    SAFE=data$BENCH
  
  for(modName in ls(reportModels))
  {
    
    if (T || modName=="EW" ) #TEST=
    {      
      mod=reportModels[[modName]]
            
      sheetName = sprintf("%s_weights",modName)
      
      sheetName=prettyWpName(sheetName);
      sheetName = substr(sheetName,1,30)
      
      mP("            writeModelDataXls - sheetName: %s",sheetName)
      
      #browser()
      createSheet(wb, name=sheetName)
      w = data.frame(cbind(rownames(mod$weight),mod$weight))
        w=mod$weight
      writeWorksheet(wb, w, sheet = sheetName,rownames="Date")
      
      sheetName = sprintf("%s_equity",modName)
      sheetName=prettyWpName(sheetName)
      sheetName = substr(sheetName,1,30)
      
      createSheet(wb, name=sheetName)
      eq = data.frame(merge(mod$equity,mNorm(data$prices[,data$BENCH]),mNorm(data$prices[,SAFE])))
      colnames(eq)=spl(sprintf("modelEQ,%s,%s",colnames(data$BENCH),SAFE))
      writeWorksheet(wb, eq, sheet = sheetName,rownames="Date")
      sheetName = sprintf("%s_trades",modName)
      sheetName=prettyWpName(sheetName)
      sheetName = substr(sheetName,1,30)
      
      createSheet(wb, name=sheetName)
      
      #data$weight=NULL
      mod$trade.summary$trades
      tradelist = mod$trade.summary$trades #bt.trade.summary(data,mod)
      
      writeWorksheet(wb, tradelist, sheet = sheetName)
    }
    saveWorkbook(wb)
  }
  
  mP("writeModelDataXls to %s",xlsName)
  # Save workbook - this actually writes the file 'saveMe.xlsx' to disk
  saveWorkbook(wb)
}

############################################################################
writeModelDataXls2<-function(reportModels,xlsName="modelData",universe=NULL,out=NULL,data=NULL)
{
  if (is.null(reportModels))
    return.
  
  mP("writeModelDataXls %s",xlsName)
  
  
  dir.create(dirname(xlsName),recursive=T)
  if (file.exists(xlsName))   file.remove(xlsName)
  wb <- loadWorkbook(xlsName, create = TRUE)
  # Create a worksheet called 'mtcars'
  if (!is.null(universe))
  {
    createSheet(wb,name="Universe")
    writeWorksheet(wb, data.frame(universe), sheet = "Universe",rownames="Number")
  }
  createSheet(wb, name = "Ergebnisse")
  # Write built-in dataset 'mtcars' to sheet 'mtcars' created above
    out = plotbt.strategy.sidebyside(reportModels, return.table=T,data=data)
    e=data.frame2german.sep(t(out)) 

  writeWorksheet(wb, e, sheet = "Ergebnisse",rownames="Number")
  if (len(which(colnames(data$prices)==SAFE))==0)
    SAFE=data$BENCH
  
  for(modName in ls(reportModels))
  {
    
    if (T || modName=="EW" ) #TEST=
    {      
      mod=reportModels[[modName]]
      
      sheetName = sprintf("%s_weights",modName)
      
      sheetName=prettyWpName(sheetName);
      sheetName = substr(sheetName,1,30)
      
      mP("            writeModelDataXls - sheetName: %s",sheetName)
      
      #browser()
      createSheet(wb, name=sheetName)
      w = data.frame(cbind(rownames(mod$weight),mod$weight))
      w=mod$weight
      writeWorksheet(wb, w, sheet = sheetName,rownames="Date")
      
      sheetName = sprintf("%s_equity",modName)
      sheetName=prettyWpName(sheetName)
      sheetName = substr(sheetName,1,30)
      
      createSheet(wb, name=sheetName)
      eq = data.frame(merge(mod$equity,mNorm(data$prices[,data$BENCH]),mNorm(data$prices[,SAFE])))
      colnames(eq)=spl(sprintf("modelEQ,%s,%s",colnames(data$BENCH),SAFE))
      writeWorksheet(wb, eq, sheet = sheetName,rownames="Date")
      sheetName = sprintf("%s_trades",modName)
      sheetName=prettyWpName(sheetName)
      sheetName = substr(sheetName,1,30)
      
      createSheet(wb, name=sheetName)
      
      #data$weight=NULL
      mod$trade.summary$trades
      tradelist = mod$trade.summary$trades #bt.trade.summary(data,mod)
      
      writeWorksheet(wb, tradelist, sheet = sheetName)
    }
    saveWorkbook(wb)
  }
  
  mP("writeModelDataXls to %s",xlsName)
  # Save workbook - this actually writes the file 'saveMe.xlsx' to disk
  saveWorkbook(wb)
}
###############################################################################
library("XLConnect")
writeTable<-function(table,xlsName="modelData.xls",table.name="Table",rownames=NULL,create = TRUE)
{
  wb <- XLConnect::loadWorkbook(xlsName, create = create)
  # Create a worksheet called 'mtcars'
  XLConnect::createSheet(wb, name = table.name)
  # Write built-in dataset 'mtcars' to sheet 'mtcars' created above
  ergebnisse = table
  XLConnect::writeWorksheet(wb, ergebnisse, sheet = table.name,rownames=rownames)
  
  mP("writeTable to %s",xlsName)
  # Save workbook - this actually writes the file 'saveMe.xlsx' to disk
  dir.create(dirname(xlsName),recursive=T)
  
  # We now create a (unnamed) cellstyle to be used for wrapping text in a cell
  #wrapStyle <- createCellStyle(wb)
  # Specify the cellstyle to wrap text in a cell
  #setWrapText(wrapStyle, wrap = F)
  
  # Write the data set to the worksheet created above;
  # offset from the top left corner and with default header = TRUE
  #writeWorksheet(wb, data, sheet = "cellsize", startRow = 4, startCol = 2)
  
  # Set the wrapStyle cellstyle for the long text cells.
  # Note: the row and col arguments are vectorized!
  #setCellStyle(wb, sheet = name, row = 5:6, col = 2, cellstyle = wrapStyle)
  
  mP("writeTable %s ",xlsName)  
  XLConnect::saveWorkbook(wb,file=xlsName)

  #writeNamedRegion(object,data,name,header,rownames)
}
################################################################################

#schreibte eine liste in ein xls-workbook
writeList<-function(list, listname="list",xlsName="list.xls",create=T,tab.name="")
{
  if (tab.name=="")
    tab.name=listname
  
  df=t(data.frame(list))
  df=t(data.frame(t(data.table(sapply(list, function(na) ifelse(is.character(na)|| is.list(na),sprintf("'%s'",na),na)))),stringsAsFactors=F))
  #library (plyr)
  #df <- ldply (list, data.frame)
  
 # df=do.call(rbind.data.frame, sapply(list, function(na) ifelse(is.character(na)|| is.list(na),sprintf("'%s'",na),na)),quote =T)
 list2=lapply(list, function(na)ifelse(is.character(na),sprintf("'%s'",na),na))
 
 df=data.frame(data.table(list2)) 
 rownames(df)=names(list)
  #df = cbind(rownames(df),df)
  colnames(df)=listname 
  writeTable(df,xlsName=xlsName,rownames= "Vars",create=create,table.name=tab.name)
}
if (F)
{
  ctrl=list(a=3,b=5)
  writeList(ctrl,listname="mm","mm.xls")
  writeList(ctrl,listname="mm2","mm.xls",create=F)
}
#...................  liest eine liste aus einem xls-workbook
readList<-function(xlsName="Models/xx/cockpit_signal.randomForest_SUP500.xls",table="cockpit")
{
  df=readTable(xlsName=xlsName,table=table)

  li=list()
  txt=""
  for(i in 1:nrow(df))
  {
    line= sprintf("%s=%s",df[i,1],df[i,2])
    txt = sprintf("%s\n%s",txt,line)
    if (i<nrow(df))
      txt=sprintf("%s,",txt)
  }  
  txt=sprintf("list(%s)",txt)
  res=eval(parse(text=txt))
  return(res)
  
  
  
  for(i in 1:nrow(df))
    {
    val= df[i,2]
    val1 =eval(parse(text=val))
    #if (val=="FALSE") val=FALSE
    #else
    #if (val=="TRUE") val=TRUE
    if (is.list(val))
      li[[df[i,1]]][[names(val)]]  =val1
    else
      if (is.vector(val)) 
         li[[df[i,1]]]  =val1
    else
       if (is.numeric(nval(val)))
         li[[df[i,1]]]  =val1
    else
    li[[df[i,1]]] =val
     }
li
}
if(F)
 ml=  readList()

###########################################################################################

#demo(package = "XLConnect")
readTable<-function(xlsName="modelData.xls",table="Table")
{
  wb <- XLConnect::loadWorkbook(xlsName, create = T)
  
  data <- XLConnect::readWorksheet(wb, sheet = table)
  mP("readTable %s %s",xlsName,table)
  data
}
if (F)
{
  X1= readTable(xlsName="d:\\DataInfo.xls",table="Table")
  X1= readTable(xlsName="Models\\Orders\\customers.xls",table=1)
  getwd()
  wb=data.frame(date=c(todayS2()), model=c("testmodel"), orders =c(2))
  writeTable(wb,xlsName="D:\\OrderLog.xls")
}


#as.numeric(forecast(cur, h = predictDays)$mean)
#############################################################################
############################################################################
#wenn  nicht geht:
#    dv = bt.apply(data, function(x) { DV(HLC(x), 1, TRUE) } )  
#mach
#  dv = m.apply(data, function(x) { DV(HLC(x), 1, TRUE) } )  
#############################################################################



m.apply_<-function(data, Fun=function(x)x, PreFun = function(x)x, ...,ignore = c(), onlysym=c(),frame="",select="",newName="")
{
  fun=match.fun(Fun)
  symbolnames=data$symbolnames
  block=NULL
  if (len(onlysym)>0)
    symbolnames = onlysym
  if (len(ignore)>0 && len(which(symbolnames==ignore))>0)
    symbolnames= symbolnames[-(which(symbolnames==ignore))]
  
  #browser("m.apply1")
  
  if (len(symbolnames) >0)
  {
    res=list()
    isbuggy=F
    
    
    block=foreach(sy =symbolnames, .combine = cbind) %do%      
{    
  
  temp= try(bt.apply.matrix(data[[sy]][frame], m.ifna.prev))
  temp = try(PreFun(temp))
  #a=c(...)
  attr(temp,"name")<-sy
  temp = try(fun(temp,...)) 
  if (!is.numeric(temp))
    if( mm.strFind("Error",temp))
    {
      cat(  tail(data[[sy]]))
      mP("Bug at m.apply %s",sy)
      
      browser()
      
      isbuggy=T
    }
  
  if (select !="")
  {
    #mP("select")
    #browser()
    if (!is.null(strfind(select,"$")))
      temp= (temp)[[select]]
    else
      temp= (temp)[,spl(select)]
    
  }
  
  ##res[[sy]]=temp
  
  temp
}  #foreach


if (isbuggy)
{
  mP("m.apply:     return NULL because:    isbuggy")
  return(NULL)
}


  }
else
{
  block = bt.apply.matrix(data$weight, fun,...)
}
# mP("#################>>")
#browser()  
if (newName =="")      
{
  if (select == "" )
  {
    if (len(block)> 0 && len(ncol(block))>0)
      colnames(block) = symbolnames  
  }
  else
    if (len(spl(select))==1)
      colnames(block)=sapply(symbolnames, function(x)sprintf("%s.%s",x,select))
  else  
  {
    
    k=sapply(symbolnames,  function(x) {x1__<<-x; sapply(spl(select),function(sel) { sprintf("%s.%s",x1__,sel)})})
    #browser()
    colnames(block)=k[,1]
    
  }
}
else
  colnames(block) = sapply(symbolnames, function(x)sprintf("%s.%s",x,newName))

return(block)
}
m.apply<-cmpfun(m.apply_)


library(doSNOW)

m.par.apply_<-function(data, Fun=function(x)x, PreFun = function(x)x, ...,ignore = c(), onlysym=c(),frame="",select="",newName="")
{
  
 
  
  #is.ok(matrix(4,4)[0,1])
  mP.1<-function(...)
  {
    #browser()
    tryM(print(sprintf(...)))
  }
  
  tryM<- function(expr)  #MMA
  {
    W <- NULL
    w.handler <- function(w){ # warning handler
      W <<- w
      invokeRestart("muffleWarning")
    }
    res=  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                           warning = w.handler),
               warning = W)
    
    if (is.null(res) || len(res)==0)
      res = list(value="???")
    if (is.null(res$value ) || len(res$value)==0)
      res.value="??"
    
    if (!is.null(res) && !is.null(res$warning))
    {
      mw = toString(res$warning)
      traceback()
      if (len(mw))
        print(mw)
      res$value = NULL
    }
    if (!is.null(res) )
      if( mm.strFind("Error",res$value))
      { 
        print("tryM-Error-Case")
        browser()
        mw = toString(res$value)
        tb=traceback()
        if (!is.null(tb) && len(tb) >0)
          print(tb)
        if (len(mw))
          print(mw)
        res$value = NULL
        browser()
      }
    #  browser()
    return (res$value)
  }
  
  
  
  mm.strFind<-function(pat,s)
  {
    if (is.null(s) || len(s)==0)
      return(F)
    return(  grepl(pat, s)[1])
  }
  
  
  attr(temp,"name")<-sy
  
  fun=match.fun(Fun)
  symbolnames=data$symbolnames
  block=NULL
  if (len(onlysym)>0)
    symbolnames = onlysym
  if (len(ignore)>0 && len(which(symbolnames==ignore))>0)
    symbolnames= symbolnames[-(which(symbolnames==ignore))]
  
  #browser("m.apply1")
  
  if (len(symbolnames) >0)
  {
    res=list()
    isbuggy=F
    
    
    block=foreach(sy =symbolnames, .combine = cbind,.packages=spl("TTR,quantmod,xts")) %dopar%
      #block=foreach(sy =symbolnames, .combine = cbind) %do%      
{    
  
  temp= try(bt.apply.matrix(data[[sy]][frame], m.ifna.prev))
  temp = try(PreFun(temp))
  #a=c(...)
  temp = try(fun(temp,...))
  
  if (!is.numeric(temp))
    if( mm.strFind("Error",temp))
    {
      cat(  tail(data[[sy]]))
      mP("Bug at m.apply %s",sy)
      
      browser()
      
      isbuggy=T
    }
  
  if (select !="")
  {
    #mP("select")
    #browser()
    if (!is.null(strfind(select,"$")))
      temp= (temp)[[select]]
    else
      temp= (temp)[,spl(select)]
    
  }
  
  ##res[[sy]]=temp
  
  temp
}  #foreach


if (isbuggy)
{
  mP("m.apply:     return NULL because:    isbuggy")
  return(NULL)
}


  }
else
{
  block = bt.apply.matrix(data$weight, fun,...)
}
# browser()
if (newName =="")      
{
  if (select == "")
    colnames(block) = symbolnames
  else
    if (len(spl(select))==1)
      colnames(block)=sapply(symbolnames, function(x)sprintf("%s.%s",x,select))
  else  
  {
    
    k=sapply(symbolnames,  function(x) {x1__<<-x; sapply(spl(select),function(sel) { sprintf("%s.%s",x1__,sel)})})
    #browser()
    colnames(block)=k[,1]
    
  }
}
else
  colnames(block) = sapply(symbolnames, function(x)sprintf("%s.%s",x,newName))

return(block)
}
m.par.apply<-cmpfun(m.par.apply_)


if (F)
{
  
  x=m.apply(data, function(x) {print(colnames(x)); return(Hi(x)-Cl(x))}, ignore=c("Zinsen"))
  x=m.par.apply(data, function(x) {mP("... %s %d",colnames(x[,1]),dim(x)[2]); return(x[,1]-x[,3]);}, ignore=c("Zinsen"))
  tail(x)
  tail(Hi(data$DAX)) - tail(Cl(data$DAX))
  m.apply(arg$dat,onlysym=colnames(arg$clos),Fun=BBands, PreFun=HLC, n=as.integer(par$bbM),maType = SMA)
}
############ wie m.apply
data.apply<-function(data, Fun,...)
{
  
  fun=match.fun(Fun)
  if (len(data$symbolnames) >0)
  {
    res=new.env()
    for(sy in data$symbolnames)
    {
      print(sy)
      temp= try(bt.apply.matrix(data[[sy]], m.ifna.prev))
      
      assign(sy,try(fun(temp,...)),envir=res)
    } 
    assign("symbolnames",data$symbolnames,envir=res)
    assign("prices", m.apply(res, function(x) Cl(x)), envir=res)
    w = res$prices; w[]=NA;
    assign("weight",w,envir=res)  
    
  }
  
  return(res)
}
############ f?ge eine neues Symbol hinzu
data.add<-function(data, symNam,symbol.xts)
{
  print(symNam)
  #browser()
  
  
  if (ncol(symbol.xts)==1)  #die spalten anreichern
  {
    colnames(symbol.xts)=symNam
    new.sym.xts =
      foreach(col = c(1:6), .combine = "merge") %do%
      symbol.xts
    colnames(new.sym.xts)= spl(sprintf("%s.OPEN,%s.HIGH,%s.LOW,%s.CLOSE,%s.VOLUME,%s.ADJUSTED",symNam,symNam,symNam,symNam,symNam,symNam))
  }
  else new.sym.xts=symbol.xts
  
  symbol.xts =new.sym.xts
  assign(symNam,symbol.xts,envir=data,pos=1)
  #  colnames(symbol.xts) =symNam
  
  assign("symbolnames",c(symNam,data$symbolnames),envir=data)
  #assign("prices", m.apply(data, function(x) x)), envir=data)
  symbol.xts=Ad(symbol.xts);  colnames(symbol.xts)=symNam
  data$prices = merge(data$prices,symbol.xts)
  
  w = data$prices; w[]=NA;
  assign("weight",w,envir=data)  
  
 # assign("weights",w,envir=data)  
  
  return(data)
}
if (F)
  data.add(data,"Dax",Dax)  

#............................................................


data.first<-function(data,symbol)
{
  dax = data[[symbol]]
  #data$symbolnames
  #ls(data)
  data.rm(data,symbol)
  data.add(data,symbol,dax)
  print(data$symbolnames)
}



if (F)
{
  
  ls(data     )
  
  data.first(data,"DAX")
  #Dax=data$HealthCare
  ls(data)
  
  data.rm(data,"Dax")
  data$symbolnames
  colnames(data$prices)  #BUG dax fehlt
  data.add(data,"Dax",Dax)  
  colnames(data$price)
  #aus rollierender zeitreihenbetrachtung .. sharep und Cgar berechnen  und MAXdd->daraus
  #attraktivi?tt schlieesen
}


#### symbolnames ist ein vector von strings
data.rm<-function(data, symbolnames)
{
  if (len(symbolnames) >0)
  {
    for(sy in symbolnames)
    {
      data$symbolnames = data$symbolnames[-which(data$symbolnames==sy)]  
      data$prices = data$prices[,-which(colnames(data$prices)==sy)]  
    }
    rm(list=symbolnames,envir=data)
    print("assign new prices+weights")
    #assign("prices", m.apply(data, function(x) Ad(x)), envir=data)
    data$prices = na.omit(data$prices)
    print(data$symbolnames)
    mP("assign %d %s prices ",ncol(data$prices),fromToS(data$prices))
    w = data$prices; w[]=NA;
    assign("weight",w,envir=data)  
    
  }
  
  return(data)
}
if (F)
{
  data.rm(data,c("Utilities"))
  ls(data)
  data$symbolnames
  
  sapply(spl("GOLD,JAKARTA_COMPOSITE"),FUN=function(stock)
  {data$symbolnames=data$symbolnames[-(which(data$symbolnames==stock))]})
  
}
####################### datenspr?nge im Dialog beheben ##############################
data.repair<-function(prices) 
{
  #source("attention.r")
  #repariere augenf?llige Preisspr?nge - nach R?ckfrage beim User  #siehe attention.r
  problems=  show.attention(prices,fn="level.price",fn.signal="signal.sprung.repair",fak=6,Lag=5,Q="99%")
}

###########################################################
mchart<-function(prices,main = "chart",ylimCol=0)
{
  par( oma = c(0, 0, 2, 0))  
  #colS=getColSet(length(colnames(prices)))
  if (ylimCol > 0)
    plot(prices[,1],ylim=c(min(prices[,ylimCol],na.rm=T),max(prices[,ylimCol],na.rm=T)),main = main)
  else
    plot(prices[,1],ylim=c(min(prices,na.rm=T),max(prices,na.rm=T)),main = main)
  for (cn in seq(1,length(colnames(prices)) ))
    lines(na.omit(prices[,cn]),col=cn)#colS[cn])
}
mchart2<-function(Dax,r,main="")
{
  par( oma = c(0, 0, 2, 0))  
  
  plota(Dax,type="l",ylim=range(Dax,na.rm=T),LeftMargin=3,main=main)
  plota2Y(r,type="l",ylim=range(r,na.rm=T),las=1,col="blue",col.axis="blue")
  plota.legend(sprintf("%s,%s",colnames(Dax[,1]),colnames(r[,1])),"black,blue")
}
################################################################################
#Infos zum geladenen Daten .. etwas weniger zeitaufw?ndig wie data.Info()
#und es gibt die sharedPrices zur?ck
##########################################################################
data.info<-function(data,ignore = c(),visual =T)
{
  #browser()
  mP("data$symbolnames: %s",data$symbolnames)
  for(i in data$symbolnames) {colnames(data[[i]])=normColnameS(data[[i]])}
  
  dataf = m.apply(data,PreFun=function(x) na.omit(Ad(x)),ignore=ignore) #ein xts mit allen 
  #browser()
  sharedPrices = (na.omit(merge(dataf)))
  #browser()
  ##.Close abschneiden
  if (F)
    colnames(sharedPrices)  =unlist(
      lapply(colnames(sharedPrices),
             FUN= function(x) trim(unlist(strsplit(x,"\\."))[1] ))  )
  
  res=fromTo(sharedPrices)
  print(toString(res))
  #universum ohnen Gold
  
  if  (visual)
    purePlot(mNorm(sharedPrices))
  
  #try(mchart(mNorm((merge(dataf)))))
  #coint=data.coint(data)#~~~~~~~~~~~~~~~~~~~~~
  
  #  browser()
  #  LongName=SecList[Name==colSym(first(names(dataf)))]$LongName
  dataInfo=
    rbindlist( lapply(dataf, function(x)
    {
      sym=colSym(names(x))
      print(sym)
      if (!is.null(data[[sym]]))
      {
      ft=fromTo(na.omit(data[[sym]])); list(name=sym, LongName=trim(toString(SecList[Name==colSym(names(x))]$LongName)),  from=ft[1],to=ft[2],Min=min(x,na.rm=T),Max=max(x,na.rm=T),len=shape(data[[sym]]))
      }
      } ))
  
  #mP("%s       %s %s    |%s|%s",dataInfo[["name"]],"|", dataInfo[["LongName"]],"|",dataInfo[["from"]],dataInfo[["to"]],dataInfo[["Min"]],dataInfo[["Max"]])
  #MXXXXXX
  
  #browser()
  DataInfo <<-data.frame(dataInfo)  #damit auch im Workspace in RStudio anschaubar
  
  if (visual)
  {
    View(DataInfo)
  }
  print(res)
  return(sharedPrices)
}
###############################################################
###################################################################

colSym<-function(colname)
{
  #colname="hh.cl"  -> hh
  trim(unlist(strsplit(colname,"\\.")[1]))[1]
}

priceSyms<-function(xtsS)
{  sapply(colnames(xtsS), function(x)  colSym(x))
}

normColname<-function(colname)
{
  #colname "hh..Close" ->  hh.Close
  # browser()
  v=unlist(strsplit(colname,"\\."))
  res = trim(toString(v[1]))
  #browser()
  if (len(v)>1)
    res = paste(res,trim(toString(last(v))),sep=".") 
  res = toupper(res)
  return(res)
}

firstColname<-function(colname)
{
  #colname "hh..Close" ->  hh.Close
  # browser()
  v=unlist(strsplit(colname,"\\."))
  res = trim(toString(v[1]))
  res = toupper(res)
  return(res)
}
normColnameS<-function(prices)
{
  newColnames= sapply(colnames(prices), normColname)
  names(newColnames) = newColnames
  unlist(newColnames)
}
if (F)
  lapply(data, function(x) colnames(x) = normColnameS(x))

data.rename<-function(data,olds,news)
{
  #browser(mP("data.rename"))
  merk = data[[olds]]
  #alle spalten in merk umbenennen
  sapply(colnames(merk),function(x,news){rest= unlist(strsplit(x,news))[2]; sprintf("%s%s",news,rest)},news=news)
  
  data.rm(data=data,olds)
  
  data$symbolnames
  colnames(data$prices)
  data.add(data,news,merk)
}

################################################################################
#Infos zum geladenen Daten
#sym="5YEARNOTE"
#as.numeric("dd")
################################################################################
data.Info<-function(data, filename="Data",frame="2001::")
{
  # benenne   5YEARNOTE   in X5YEARNOTE Note um ,   das macht sonst foreach..unkontrolliert
  
  for(sym in data$symbolnames)
    if (!is.na(as.numeric( substring(sym, 1, 1))))# der erste Buchstabe ist eine Ziffer
    {
      new_sym=sprintf("X%s",sym)
      data.rename(data,sym, new_sym)
    }
  
  
  dataf = m.apply(data,function(x) na.omit(Cl(x))) #ein xts mit allen Daten
  
  sharedPrices = mNorm(na.omit(merge(dataf)))
  res=fromTo(sharedPrices)
  print(toString(res))
  #universum ohnen Gold
  
  pdf(file=sprintf("%s.pdf",filename), width=15,height=8.5)
  try(mchart(sharedPrices))#[,-which(colnames(sharedPrices)=="Gold.Close")])
  try(mchart(mNorm((merge(dataf)))))
  dev.off()
  #coint=data.coint(data)#~~~~~~~~~~~~~~~~~~~~~
  
  dataInfo=rbindlist( lapply(dataf, function(x){ft=fromTo(na.omit(x)); list(name=colnames(x),  from=ft[1],to=ft[2],Min=min(na.omit(x)),Max=max(na.omit(x)))} ))
  mP("%s       %s %s    |%s|%s",dataInfo[["name"]],dataInfo[["from"]],dataInfo[["to"]],dataInfo[["Min"]],dataInfo[["Max"]])
  
  DataInfo <<-data.frame(dataInfo)  #damit auch im Workspace in RStudio anschaubar
  
  writeTable(DataInfo,xlsName=sprintf("%s.xls",filename),rownames=NULL)
  
  #nicht gelieferte Daten
  print("bad Data")
  print(toString(dataInfo[is.na(dataInfo$Min) | is.na(dataInfo$Max)]$name))
  
  #ls(data)
  save(data,file=sprintf('T2data/%s.Rdata',filename))   ;#  Load("gv")
  print( toString(fromTo(data$prices)))
  
  #tail(data$prices)  
  #prices = m.apply(data, function(x) tail(x[frame,],1))
  #p=m.apply(data, function(x) {print(tail(x,1)); Cl(na.omit(x[frame,]))})
  prices = m.apply(data, function(x) mNorm(na.omit(Cl(x[frame,]))))
  #purePlot(prices)
  #frame="2001::"
  #print(coint)
  return(res)
}

##########################################################################

contains<-function(what,vec)
{
  return (len(which(vec==what))>0)
}
if (F)
  contains("price",spl("price,coint,target,score"))

##########################################################################
#Berechne eine Reihe von Basis-Aufgaben auf preisdaten und gib diese als Liste zur?ck:
#evtl. sehr zeitaufw?ndig - darum kann man mit dem mode-vektor die Aufgaben w?hlen die einen
#interessierren
#Info,
#cointegration
#targetByDD
##########################################################################
data.analysis<-function(data, xlsFile="DataAnalysis", mode=spl("price,priceCleaned,coint,target,score"))
{
  data=data.Info(data,xlsFile) 
  res=list()
  res$data=data
  
  #mdata= data.analysis(data, xlsFile="DataAnalysis", mode=spl("price"))
  #global_Targets=compute.Targets(mdata$prices)
  
  #ret = Return.clean(ret, method = c("none", "boudt", "geltner")[2])
  #browser()
  frame=sprintf("%s::",DateS(data$prices[1]))
  if (contains("priceCleaned",mode)) #MM_TODO:  hier checken ob ?berhaupt ein Preissprung > 10% vorkommt .. sonst kann man sich das schenken und Zeit sparen
    res$prices= data$prices = tryM(m.apply(data, function(x) mNormCleaned(na.omit(Cl(x[frame,])))))      
  else
    if (contains("price",mode))
      res$prices=data$prices = tryM(m.apply(data, function(x) mNorm(na.omit(Cl(x[frame,])))))
  if (contains("coint",mode))
    res$coint=tryM(data.coint(data))#~~~~~~~~~~~~~~~~~~~~~
  
  if (contains("target",mode))
  {global_Targets <<- tryM(compute.Targets(data$prices,0.16,0.05))
   colnames(global_Targets)<<-pureColnames(data$prices)
   
   res$target = global_Targets
  }
  if (contains("score",mode))
    #erzeuge ein bin-matrix die zeigt wann welche zeitreihe zu den top titeln geh?rt hat
    res$score=tryM(calc.score.nk(data$prices,n=10, K=7, wlen=150))
  
  return(res)
}

if (F)
{
  data.analy = data.analysis(data)
  global_Targets <<- tryM(compute.Targets(data$prices,0.16,0.05))
  
  
}
##########################################################################################################
# die colnames - aber ohne das was rechts vom . steht .. also ohne  .Close...
##########################################################################################################
pureColnames<-function (Xts)
{
  unlist( lapply(colnames(Xts),
                 FUN= function(x) 
                 {
                   norm.ret=toupper(paste(sapply(strsplit(x,"\\."),trim)[1],collapse="."))
                   spli=unlist(strsplit(x,"\\."))
                   abgeschni=""
                   if (shape(spli)>1) abgeschni=spli[2]
                   if (abgeschni == "DE") return(x)  #wenn ich Dax-Titel direkt bei yahoo geladen hab, k?nnen die auf ".DE" enden .. dann darf nichts abgeschnitten werden
                   return(norm.ret)
                 }))
}
##########################################################################################################
#Liste alle cointegrierten Datenpaare
#Siehe auch das globale data.frame - Objekt COINT
#  #http://quanttrader.info/public/testForCoint.html
##########################################################################################################
data.coint<-function(data,frame="")
{
  res = list()
  cointRes = list()
  symbolnames=data$symbolnames
  
  if (len(symbolnames)<1)
    symbolnames= ls(data)
  
  for(sy1 in symbolnames)
  {
    l= list()
    part=symbolnames
    part2=symbolnames[c(which(part==sy1):len(symbolnames))]
    part1=symbolnames[c(1:which(part==sy1))]
    for(sy2 in  part1)  #matrix ist symmetrisch - die h?lfte zu rechnen reicht
    {
      l[[sy2]]="."
    }
    for(sy2 in  part2)
    {
      l[[sy2]] =  is.cointegrated(na.omit(Cl(data[[sy1]])[frame,]),na.omit(Cl(data[[sy2]])[frame,]))   
      if (l[[sy2]] !="")
        res[[sy1]]=sy2
      
    }
    cointRes[[sy1]] = l    
  }
  
  COINT<<- data.frame(rbindlist(cointRes))
  COINT<<-cbind(colnames(COINT),COINT)
  return(res)
}

prices.coint<-function(prices,features=NULL,frame="",symbolnames="")
{
  res = list()
  cointRes = list()
  if (symbolnames=="")
    symbolnames=colnames(prices)
  
  for(sy1 in symbolnames)
  {
    l= list()
    part=symbolnames
    part2=symbolnames[c(which(part==sy1):len(symbolnames))]
    part1=symbolnames[c(1:which(part==sy1))]
    for(sy2 in  part1)  #matrix ist symmetrisch - die h?lfte zu rechnen reicht
    {
      l[[sy2]]="."
    }
    for(sy2 in  part2)
    {
      if (is.null(features))
        l[[sy2]] =  is.cointegrated(na.omit(prices[,sy1])[frame,],na.omit(prices[,sy2])[frame,])
      else
        if (len(features[[sy1]])>0)
          #schau Dir auch indicatore oder leads an die in features übergeben werden
          l[[sy2]] =  is.cointegrated(na.omit(prices[,sy1])[frame,],na.omit(merge(prices[,sy2],features[[sy1]])[frame,]))   
      if (l[[sy2]] !="")
        res[[sy1]]=sy2
      
    }
    cointRes[[sy1]] = l    
  }
  
  COINT<<- data.frame(rbindlist(cointRes))
  COINT<<-cbind(colnames(COINT),COINT)
  return(res)
}

if (F)
{
  SektorSpdrUSA<<- T2$new(PortfolioName = "SektorSpdrUSA", bench="Dax",visual = T, online=T )
  data<-SektorSpdrUSA$t0data
  # data.Info(data,"SektorSpdrUSA")
  
  coint=data.coint(data)
  print(coint)
}

data.coint.matrix<-function(prices,frame="")
{
  res = list()
  cointRes = list()
  symbolnames=colnames(prices)
  
  for(sy1 in symbolnames)
  {
    l= list()
    part=symbolnames
    part2=symbolnames[c(which(part==sy1):len(symbolnames))]
    part1=symbolnames[c(1:which(part==sy1))]
    for(sy2 in  part1)  #matrix ist symmetrisch - die h?lfte zu rechnen reicht
    {
      l[[sy2]]="."
    }
    for(sy2 in  part2)
    {
      l[[sy2]] =  is.cointegrated(na.omit((prices[,sy1])[frame]),na.omit((prices[,sy2])[frame]))   
      if (l[[sy2]] !="")
        res[[sy1]]=sy2
      
    }
    cointRes[[sy1]] = l    
  }
  
  COINT<<- data.frame(rbindlist(cointRes))
  COINT<<-cbind(colnames(COINT),COINT)
  return(res)
}


###############################################################################
if (F)  #entferne Leerzeichen in Colnames
{
  colnames(data$crs$tdata)
  data.coint.matrix(data$crs$tdata)
}
##################################################################################
#http://blog.quanttrader.org/tag/cointegration/
#http://blog.quanttrader.org/tag/pair-trading-3/

#sind zwei zeitreithen cointegriert
#http://blog.quanttrader.org/tag/pair-trading-3/


# Download data

#source("/home/robo/Desktop/PairTrading/downloadV2.R")

# Find co-integrated pairs

#source("/home/robo/Desktop/PairTrading/cointegrationV2.R")

# Analyze data and export output file

#source("/home/robo/Desktop/PairTrading/analysisV2.R")
##################################################################################

is.cointegrated<-function(gld,gdx)
{
  library(tseries)
  library(fUnitRoots)
  
  if (colnames(gld[,1])==colnames(gdx[,1] ))
    return("")
  
  t.zoo <- mNorm( merge(gld, gdx, all=FALSE))
  
  t <- as.data.frame(na.omit(t.zoo))
  print(colnames(t))
  
  #cat("Date range is", format(start(t.zoo)), "to", format(end(t.zoo)), "\n")
  
  
  m <- lm(sprintf("%s ~ %s + 0",colnames(t)[1],colnames(t)[2]), data=t)#gld ~ gdx + 0
  beta <- coef(m)[1]
  
  #cat("Assumed hedge ratio is", beta, "\n")
  
  #sprd <- t[[1]] - beta*t[[2]]
  
  sprd <-resid(m)
  
  #ht <- try(adf.test(sprd))#, alternative="stationary"))
  ht <-adfTest(na.omit(coredata(sprd)), type="nc")@test$p.value
  
  ### ADF - Tests
  ###################################################
  #library(urca)
  #ht <- summary(ur.df(sprd, type = "trend", lags = 10,selectlags="AIC"))
  
  
  if (inherits(ht,"try-error"))
    return("")
  #cat("ADF p-value is", ht$p.value, "\n")
  
  if (ht$p.value < 0.05) {
    print("############################################")
    mP("%s ~%s",colnames(gld[,1]),colnames(gdx[,1] ))
    cat("The spread  is likely mean-reverting\n")
    #browser()
    return (toString(ht$p.value) )
  } else {
    #cat("The spread is not mean-reverting.\n")
    
    
    return("")
  }
}

#########################################################################
#  w?hle einige Spalten c(selection) aus block und bilde daraus ein neues xts.
#########################################################################
sub.block<-function(block,selection=colnames(block))
{ as.xts(data.frame(lapply(selection, function(x) block[,x]))) }
#######################################################################


#####################################################
# Du willst den Kurs vom atDate.
# evtl. ist atDate aber ein Wochenende .. dann willst
# du den letzten Kurs der vorher in p(rices) enthalten ist
#####################################################
fitDate<-function(p,atDate)
{
  atDate = tryCatch(as.Date(atDate),error=function(e){ return("")})
  if (toString(atDate) == "")
  {
    mP("strange Daten at fitDate %s",atDate)
    browser()
  }
  
  atDate2= as.Date(index(last(p[as.Date(index(p))<=as.Date(atDate)])))
  if (abs(atDate2-atDate) > 4) #maximal 4 Feiertage am St?ck tolerieren
    return("")
  return(atDate2)
}

### ?bergib einen xts-Wert (z.B. last(prices[,1]))
# und bau aus dem und den wlen-Tagen vorher liegendem Datum einen frame-string
#######################################################################
get.frame<-function(xtsWert,wlen=100 )
{
  l2 = as.Date(index(xtsWert))
  l1 = l2-wlen
  frame=sprintf("%s::%s",l1,l2)
}
if (F)
  apply.monthly(prices[,1], FUN=function(x) 
  {  frame=get.frame(last(prices)) ; score.nK(prices[frame]) })


#############################################################################
#gib mir den Index (integer 1...) in prices (die wievielte Pos im xts-vektor ist at Date),  mach evtl. vorher  fitDate()  
#nun vektor-tauglich
####################################################################
#xdat.Lineal[as.Date(index(p.org)),]

get.Index_<-function(prices=NULL,atDate =NULL,lineal=F,nextValue=1)
{
  if (len(atDate)==1)
    atDate=as.character(atDate)
  
  if (!is.null(prices))
    if (len(prices[atDate])==0)
      atDate=DateS(first(prices[as.Date(index(prices))>=atDate]))
  
  if (len(atDate)==0)
  {
    sag("get.Index missing %s",atDate,warte=T)
    return(0)
  }
  if (F)
  {
    if (nextValue <2)  #falls Du nach einem Wochenende fragst, gib den freitag vorher an...
    {
      #browser(mP("get.Index - unknown date"))
      return(get.Index(prices,atDate-1,lineal=lineal, nextValue=nextValue+1))
    }
    else return(0)
  }
  
  if (!lineal && is.null(prices))
  {
    xdat=as.Date(as.Date(first(atDate)):as.Date(last(atDate)))
    xdat.Lineal=xts(1:len(xdat), xdat)
    lineal=T
    return(coredata(xdat.Lineal[atDate])  )
  }
  if (lineal && !is.null(prices))
    return(coredata(prices[atDate])  )
  
  if (F)
  {
    p=prices[sprintf("::%s",atDate)]
    ep=endpoints(p,"days")
    ep = ep[ep > 0]   
    res = last(ep)
    print(res)
    return(res)
  }
  
  #browser()
  if (len(atDate)==1)
  {
    p=prices[sprintf("::%s",atDate)]
    #browser()
    ep=endpoints(p,"days")
    ep = ep[ep > 0]   
    res = last(ep)
    return(res)
    
  }
  #  browser(mP("get.Index Slow"))
  res=  vapply(atDate,1,FUN=function(x){
    p=prices[sprintf("::%s",as.character(x))]
    ep=endpoints(p,"days")
    ep = ep[ep > 0]   
    res = last(ep)
    return(res)
  })
  
  
  return(res)
}
get.Index<-cmpfun(get.Index_) #compilier das Teil

###################################################################


get.frame2<-function(prices,atDate,wlen=200 )
{
  l2 = get.Index(prices,fitDate(prices,atDate))
  l1 = l2-wlen
  
  if (l1 <= 0)
  {
    mP("Warning at get.frame2 - first date %s", DateS(prices[wlen+1]))
    return("")
  }
  frame=sprintf("%s::%s",DateS(prices[l1]),atDate)
  #dim(prices[frame])
}

if (F)
  pre = get.frame2(prices,atDate,wlen)



####################################################
####################################################
diffCount<-function(x)
{
  x=na.omit(x)
  dx=sum(sign(na.omit(diff(x))))
}

#### die eigentliche Target-Funktions f?rs Ranking des Monats B
# eigentlich blos  last(equity) / maxDD .. aber es wird zus?tzlich verlangt,
#dass maxdd nicht gr??er ist als maxDD und der Gewinn wenigstens ein 1% ist.
#anderfalls ist xcalmar 0
xcalmar <-function(eq,maxdd,maxDD)
{
  maxDD=maxDD/100
  #tes=cummax(c(1,x))[-1]
  if (abs(maxdd) > abs(maxDD))
    return(0)  #maxloss
  
  if ( last(mNorm(eq)) < 1/100)  #min SollGewinn im Monat:1% 
    return(0)
  
  return(last(mNorm(eq)))
}

getColSet<-function( n)
{
  rgb = colorRampPalette(c("red","black", "blue"),  space = "rgb") #Lab)
  colSet = c("green",rgb(n))
  
}
#######################################################
#geneneriere den lm-formular-string .. besonders f?r multivar-...
# gewicht ~ name + alter
#######################################################
lmFormular<-function(DF,yCol)
{
  coln=colnames(DF)
  ycol=coln[yCol]
  
  xcol=coln[-c(yCol)]
  xcol=paste(xcol,collapse=" + ")
  res = paste(ycol,xcol,sep=" ~ ")
}
if (F)
{
  DF=data.frame(name=c("markus","christine","stefan","tina"),
                alter=c(54,53,18,23) , gewicht=c(1,2,3,5))
  
  x = lmFormular(DF,3)
}

##############################################################################
#Zwei Hilfsmethoden f?r techScore ...
#............................................................................
getTableDataNames<-function(listOfXts)
{
  if (is.null(listOfXts)) return(c())
  return(names(listOfXts))
}
#............................................................................
# list aus einer Liste von xts-Objekte die Werte die zu atDate und coln passen

getTableData<-function(listOfXts,atDate,coln)
{
  if (is.null(listOfXts)) return(c())
  
  res=unlist(lapply(listOfXts,function(x.xts)
  {
    coredata(x.xts[atDate,coln])
  }))
  
  names(res) = names(listOfXts)
  #  print(res)
  #  print(getTableDataNames(listOfXts))
  #  browser()
  return(res)
}
##########################################################################
#wie lag, aber die ersten n Werte werden mit 0 gef?llt (statt NA)
##########################################################################
mlag.0<-function(x,n=1)
{
  res = lag(x,n)
  
  res[c(1:n),]<-0
  res
}


######################################################################################################


########################################### Was bringen eigentlich die Targets ###########
## Die Targets und prices seien von identischer dim und identischen colnames,
#Targets sind 0,1,-1 - signale 
# und werden geplottet
showTargets<-function(prices,Targets,main=main)
{
  sapply(colnames(Targets),FUN=
           function(sym){ mP("showTargets %s",sym); if(len(prices[,sym])==0) sag("sym %s not at prices",sym,warte=T) else 
             plotSigPrice(signal=Targets[,sym],prices=mNorm(na.omit( log(10+(prices[,sym])))),indi=NULL,main=sprintf("%s %s",sym,main),no.eq=T) })
  
}
##############################################################################
#nimm als Signal die Kristallkugel eines globalTarget
##############################################################################

signal.global_Target<-function(arg, par = mlist( Target=c(global_Targets3)),visual=F, Target=NULL, ...)
{
  Target=par$Target
  
  if (is.null(Target))
    stop("BedienungsFehler signal.global_Target - das Target darf nicht NULL sein")
  
  sym=colnames(arg$clos)
  if (len(Target[,sym]) ==0)
    stop(sprintf("BedienungsFehler signal.global_Target - das Target muss eine Spalte mit %s enthalten",sym) )
  
  #sma200 =SMA(na.omit(p),n=winLen)
  
  signal = Target[,sym]
  
  return(list(Signal=signal, Indi=NULL))
}

if(F)
  x=indi.Generic("signal.global_Target", global_arg, par=list(Target=global_Targets3),visual=T, TRAINSYM =-1)

###############################################
showTargets2<-function(prices,Targets)
{
  arg=list(clos=prices)
  x=indi.Generic("signal.global_Target", arg, par=list(Target=Targets), visual=T, TRAINSYM =-1)  
}

########################################################################
coarse.code <-function(dax,method = "n", b=100,visual=F)  #univariat, 
  #method q verformat - weil:  equal binning    rattle::binning(p, bins=4, method="quantile",    labels=NULL, ordered=TRUE, weights=NULL) #ave
  #wie viele sind dann eigentlich im jeweiligen bin
  #  AV=ave(res,cut(res,b=b,labels=F),FUN=count)   #ave wendet FUN auf jedes Element der gruppe an ... 
  # sapply(1:b,FUN=function(x)  first(AV[res==x]))
  #einfacher  bin - z?hler
  #  sapply(1:b,FUN=function(x)  count(res[res==x])) oder  mit netten namen:
  #  sapply(1:b,FUN=function(x) {res= count(cp[cp==x]);names(res)<-x;return(res)})
  
  #  
{
  #dax = na.omit(dax)
  
  if (F)
  {
    #coarse codeing
    plot(as.numeric(cut(dax,b=b)))  #unterteile den Wertebereich einfach in 10 gleichweite Intervalle 
    
    #w?hle hier nicht einfach "gleichweite" - Intervalle , sondern die Intervalle so,
    #dass jeweils gleich viele dax-werte hineinfallen- das wirkt im plot aber gestalt ver?ndernd
    quantile.range <- quantile(dax, probs = seq(0, 1, 0.1),na.rm=F)
    plot(diff(quantile.range))
    plot(as.numeric(cut(dax,breaks=as.numeric(quantile.range)  )))
    
    dax.r = na.omit(mROC(dax))
    library(Hmisc)  #hdquantile ist besser als quantile
    quantile.range <- hdquantile(coredata(dax.r), probs = seq(0, 1, 0.05),na.rm=F)
    plot(diff(quantile.range))
    
    plot(as.numeric(cut(dax.r,breaks=as.numeric(quantile.range)  )))
    plot(dax.q)
  }
  #------------------------------------------------------------------>
  if (method != "q")  # n  #default-methode 
  {
    res=as.numeric(cut(dax,b=b,labels=F), include.lowest=T)
    if (visual)
    {
      plot(scaleTo(dax,c(0,b)));r = dax;r[]=res;    lines(r,col="red")
      #einfacher bin z?hler
      print("freq.loc: ")
      freq.loc =sapply(1:b,FUN=function(x) {res= count(cp[cp==x]);names(res)<-x;return(res)})
      
      print (freq.loc)
    }
  }
  else  
    # q- methode
    #w?hle hier nicht einfach "gleichweite" - Intervalle , sondern die Intervalle so,
    #dass jeweils gleich viele dax-werte hineinfallen
  {
    #browser()
    library(Hmisc)  #hdquantile ist besser als quantile
    quantile.range <- hdquantile(coredata(dax), probs = seq(0, 1, 1/b),na.rm=F)
    quantile.range = unique(quantile.range) #manchmal macht hdquantile identische Werte
    res =as.numeric(cut(dax,breaks=as.numeric(quantile.range) ))# ,labels =F))
    plot(scaleTo(dax,c(0,b)));r = dax;r[]=res;    lines(r,col="red")
    
    #
    browser(mP("no bug at coarse.code"))
  }
  Res=dax[,1];Res[]=NA
  # browser(mP("coarse.code"))
  
  if (shape(res)!=  shape(Res))
    browser(mP("bug at coarse.code"))
  else
    Res[]=res
  
  return(Res)
}
if (F)
{
  #dax=data$prices[,"DAX30"]
  plot(dax["1994::"])
  plot( coarse.code(dax["1994::"],b=10,method="n"))
  plot( coarse.code(dax["1994::"],b=100,method="q"))
  plot( coarse.code(dax["1994::"]))
  
} 
#################################################################
#vorsicht: nutzt zuk?nftige daten
#################################################################
ksmooth<-function(p,glaettung = 1)
{
  library(KernSmooth)
  p=m.ifna.prev(p)
  
  gridsize <-nrow(p)
  t=c(1:nrow(p))
  bw <-dpill(t,p,gridsize=gridsize)#findet sehr gute bandweite
  bw=bw*glaettung
  lp<-locpoly(x=t,y=p,bandwidth=bw,gridsize=gridsize)
  smooth <-lp$y
  s=p;s[]=smooth
  s
}



###########################################################################
#das Teil separiert super scharf (insample) Trendsegmente- kann also als 
#schneller Generator f?r Trainingssignale dienen
#fast.smoothing() liefert ein so sch?nes signal, dass man seine steigungs-wechsel 
#zum segmentieren nutzen kann
#nicht zum traden geeignent wg. lag(-1)
###########################################################################
fast.smoothing<-function(p,glaettung = 1,visual=F,p0=NULL,roll=F)
{
  #...... smoothing
  #aus dem r-cookbook
  library(KernSmooth)
  p=m.ifna.prev(p)
  
  gridsize <-nrow(p)
  t=c(1:nrow(p))
  bw <-dpill(t,p,gridsize=gridsize)#findet sehr gute bandweite
  bw=bw*glaettung
  lp<-locpoly(x=t,y=p,bandwidth=bw,gridsize=gridsize)
  smooth <-lp$y
  s=p;s[]=smooth
  sig=sign(ROC(s,n=1,type="continuous"))#einfach die tagesdiff der glatten kurve
  
  sd = sig#sign(diff(sig))
  rS=runSum(na.omit(sd),n=2)
  rS=lag(rS,-1)
  peaks <- p[ as.Date(index(rS[rS==0]))]
  
  if (!roll)
  {
    if (len(peaks) < 2)
    {highx=p[1];lowx=p[1];}
    else
    {
      print(".")
      highx = peaks[peaks >= lag(peaks,1)]
      lowx = peaks[peaks < lag(peaks,1)]
      
      f=""
      if (F && visual) #zus?tzlicher Hilfsplot
      {
        plot(p[f])
        points(peaks[f])
        points(highx,col="blue",lwd=3)
        points(lowx,col="red",lwd=3)
      }
      
      highx=as.Date(index(highx))
      lowx=as.Date(index(lowx))
      peaks=as.Date(index(peaks))
      
    }
  }
  if (visual)
  {
    if(F)#zus?tzlicher Hilfsplot
    {
      plot(p);lines(s,col="red")
      lines(scaleTo(sig,range(p)),col="green")
      amark(peaks)
    }
    frame=""
    if (is.null(p0))
      ret= plotSigPrice(signal = lag(sig[frame],-1),prices=p[frame],indi=list(f=merge(p,s)))
    else
      ret= plotSigPrice(signal = lag(sig[frame],-1),prices=p[frame],indi=list(f=merge(p,ZLEMA(p,20),s, scaleTo(p0,range(na.omit(p))))))
  }
  
  if (nrow(s)!=nrow(p))
    browser(mP("bug"))
  
  if (roll)
    return(last(s))
  
  
  
  return(list(smooth = xts(smooth,index(p)),highs =highx,lows = lowx,hl=peaks ,sig=sig))
  
}
if(F)
{#MM_STOPSYS
  x=fast.smoothing(p,glaettung = 10,visual=T)  #h?here Werte 2,3...machen alles glatter
  ls(x)
  
  #plot(x$smooth)
  amark(x$lows)
  amark(x$highs,col="blue")
  
  # wie sah denn damals, vor dem high die situation aus ?
  h1=x$highs[1]
  frame=sprintf("::%s",h1)
  x=fast.smoothing(p[frame],glaettung = 10,visual=T)  #h?here Werte 2,3...machen alles glatter
  #schreite in der long-pos voran bis es zum stop kommt
  h1=h1+10
  frame=sprintf("::%s",h1)
  x=fast.smoothing(p[frame],glaettung = 10,visual=T)  #h?here Werte 2,3...machen alles 
  
  last.segment.start=last(x$hl[as.Date(x$hl)<as.Date(h1)])
  last.frame=sprintf("%s::%s",last.segment.start,h1)
  p.seg=p[last.frame]
  x.seg=x$smooth[last.frame]
  #lines(p.seg,col="green")
  #lines(x.seg,col="magenta")
  d.low=p.seg-x.seg; 
  d.low[d.low > 0]<-0
  plot(d.low)
  #x.seg#die smooth  linie im lezten segment
  y2=rollapplyr(abs(d.low),30, roll.quantile, allPrices=abs(d.low),maxWin=shape(d.low),Q="99%" )
  plot(p.seg);lines(x.seg,col="magenta")
  lines(x.seg-y2,col="red")
  plot(d.low)  
  
  
  layout(1:3)
  plot(p.seg)
  r=ROC(p.seg,1);r[1]<-0; r[r>0]<-0
  plot(r)  
  y=-runquantile(-r, 365, probs=c(   0.99))
  Y=r;Y[]=y
  lines(Y,col="red")
  
  r=ROC(p.seg,1);r[1]<-0; r[r<0]<-0
  plot(r)  
  y=runquantile(r, 365, probs=c(   0.99))
  Y=r;Y[]=y
  lines(Y,col="blue")
  
  
  n=5
  layout(1:3)
  plot(p.seg)
  r=ROC(p.seg,5);r[1]<-0; r[r>0]<-0
  plot(r)  
  y=-runquantile(-r, 365, probs=c(   0.99))
  Y=r;Y[]=y
  lines(Y,col="red")
  
  r=ROC(p.seg,5);r[1]<-0; r[r<0]<-0
  plot(r)  
  y=runquantile(r, 365, probs=c(   0.99))
  Y=r;Y[]=y
  lines(Y,col="blue")
  
  
  ls(data$prices)
  
  n=20
  layout(1:3)
  plot(p.seg)
  r=ROC(p.seg,n);r[1]<-0; r[r>0]<-0
  plot(r)  
  y=-runquantile(-r, 250, probs=c(   0.99))
  Y=r;Y[]=y
  lines(Y,col="red")
  Ydown=Y
  
  r=ROC(p.seg,n);r[1]<-0; r[r<0]<-0
  plot(r)  
  y=runquantile(r, 250, probs=c(   0.99))
  Y=r;Y[]=y
  lines(Y,col="blue")
  Yup = Y
  
  YY=Yup+Ydown
  YY[is.na(YY)]<-0
  sig=sign(YY)
  plotSigPrice(sig, p.seg,main="DAX")
  
  
  if (F)
  {
    y=runquantile(abs(d.low), 500, probs=c(    0.95, 0.99))
    
    stop.lines=merge(x.seg,x.seg); stop.lines[]=y[,1,c(1,2)]
    stop.lines[,1]= stop.lines[,1]
    stop.lines[,2]= stop.lines[,2]
    
    col = c(  "green", "blue")
    lines(-stop.lines[,1], col=col[1]) #runmean(y[,1,1],K)
    lines(-stop.lines[,2], col=col[2]) #runmean(y[,1,1],K)
  } 
  
  
  
  #wahrscheinlich wird das sig im rollapplyr viel schlechter: 
  #aber mit zunehmender width auch besser (> 500)
  s = rollapplyr(p,FUN=function(x)fast.smoothing(x,glaettung=15,roll=T), width= 300,by.column=F)
  plot(p)
  lines(s,col="green",lwd=2)
  lines(x,col="red")
  lines(EMA(p,50),col="blue")
  sig=sign(ROC(s,n=20,type="continuous"))#einfach die tagesdiff der glatten kurve
  
  plot(p);lines(s,col="green");lines(x$smooth,col="red")
  lines(sig*3,col="green")
  
  frame=""
  ret= plotSigPrice(signal = sig[frame],prices=p[frame],indi=list(f=merge(p,s)))
  
}


########################################################################################
print("########### load InputConfig_Portfolio_TD3.R")
