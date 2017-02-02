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

######################################################################
#assoc-Liste die dem Key sval mit einer Liste von featuren assoziiert.
#pro Aufruf kommt wird die passenden feature-Liste verl?ngert
#######################################################################

addFeature<-function(hList, sVal,feature)
{
  if (is.null(hList[[sVal]]))      
  {
    jkBinList = list(feature)
    hList [[sVal]]= jkBinList   #asso of SVal with List of jk-Values that generate this sVal    
    
  }
  else
  {
    jkBinList = hList[[sVal]]
    jkBinList = c(jkBinList, list(feature))  #verl?ngere die jkBinList des sVal um eine weiteres jkTupel 
    hList [[sVal]]= jkBinList
  }
  return (hList)  
}

# hList <- hash()
# 
# s=2.1123274
# sVal= toString(round(s,1))
# hList=addFeature(hList,sVal, c(6,7))
# hList=addFeature(hList,"5.2", c(99,3))
# hList=addFeature(hList,"2.1", c(299,223))
# hList
# len(hList[["2.1"]])
# 


#euklidscher Abstand
Euklid_distance <- function (x1, x2) {
  temp <- x1 - x2
  sum(temp * temp)
}
#Euklid_distance(c(1,1),c(4,4))

#undebug(mL)
mL <-function(r)
{
  
  print("mL --->")
  print   (head(r,1))
  print (tail(r,1))
  cat ("cols: ", ncol(r)," lines: ",nrow(r))
}


fromTo <-function(r=data$prices,visual=T)
{
  #  r
  # browser()
  if (!exists("r") || is.null(r) || len(r) ==0 || len( dim(r)) ==0 || dim(r)[1]<1)
  {
    mP("fromTo() called with null")
    browser()
    return(c (0,0))
  }
  Auflegedatum=format(time(head(r,1)), '%Y-%m-%d')
  Bisdatum = format(time(tail(r,1)), '%Y-%m-%d')
  
  #Auflegedatum=format(time((r)[1,]), '%d%b%y')
  #Bisdatum = format(time(tail(r,1)[1,]), '%d%b%y')
  if (visual) cat (colnames(r),"\ncols: ", ncol(r)," lines: ",nrow(r),"\n",Auflegedatum,Bisdatum,"\n")
  return(c (Auflegedatum,Bisdatum))
}


fromToS <-function(r=data$prices)
{
  #browser()
  Auflegedatum="???"
  
  Bisdatum="???"
  Auflegedatum=try(format(time(head(r,1)), '%Y-%m-%d'))
  Bisdatum = try(format(time(tail(r,1)), '%Y-%m-%d'))
  ret=sprintf("%s::%s",Auflegedatum,Bisdatum)
  return(ret)
}


#########################################################################################
writeIndexUniverse <-function(iname)
{
  indexProvider = getProviderTicker(iname)  
  index = indexProvider$tick
  stoxxTicker = getIndexComposition(index)
  m = cbind(stoxxTicker)
  colnames(m)<-c("member")
  file_ = sprintf("Models/universe%s.csv",iname)
  dir.create(dirname(file_),recursive=T)  
  mP("writeIndexUniverse %s",file_)
  write.table(m,file=file_,row.names=F,quote=F)
  cat(index," = ",iname,"\n",file_)
  return(iname)
}

#stoxxTicker = getIndexComposition("^Stoxx50E")
#writeIndexUniverse("^Stoxx50E")

########################################################################################
ALTrmSymbol<-function(symbolstr, data=.GlobalEnv)
{
  # browser()#TT
  
  #i=symbol
  if (exists(symbolstr,envir=data))
  {
    #  sym = paste("",i,"",sep="'")
    #  text_=paste("rm(",sym,",envir=data)")    
    
    
    symbol=get(symbolstr, envir=data) #[symbolstr]
    mP("rmSymbol 1 %s !!!!!!!!!!!!!!!!!  %s",symbolstr,toString(symbol))
    
    #assign("Dax",i,data)
    #ls(data)
    #symbolstr="Dax"
    
    nam=paste("'",symbolstr,"'",sep="")
    
    cat(nam)
    browser()
  try(rm(list(nam),envir=data))
    #rm(nam,envir= data)
    #ls(data)
    
    #   rm(i,envir = data)
    
    #  eval(parse(text=text_))
  }
}


rmSymbol<-function(symbolstr, envir=.GlobalEnv)
{
  data= envir
  if (exists(symbolstr,envir=data))
  {
    #  sym = paste("",i,"",sep="'")
    #  text_=paste("rm(",sym,",envir=data)")    
    
    mP("rmSymbol %s !!!!!!!!!!!!!!!!!  \n",symbolstr)
    # stacktrace()
    #    browser()  
    
    #symbol=data[[symbolstr]]
    #rm(symbol,envir= data)
    
    
    #   rm(i,envir = data)
    text_=sprintf("rm(%s, envir=envir)",symbolstr)
    #  browser()
    try(eval(parse(text=text_)))
  }
}



listTicks<-function(data=.GlobalEnv)
{
  cat("\nlistTicks --->>>\n")
  for (tick_ in ls(data))
  {
    #tick_ = "Dax"
    #cat("\ncheck symbol ------ ",tick_)
    tick= get(tick_,envir=data)
    if (length(tick)!=0 && !is.null(tick) && isPrice(tick,tick_))
      cat("##################",tick_,"\n")
  }
}
#isPrice(dax,"dax")

delNonPrices <-function(data=data)
{
  cat("delNonPrices-->>")
  print(ls(data))
  #debug(isPrice)
  #browser()  
  #if (exists("symbolnames",envir=data))
  #  bt.prep.remove.symbols(b=data)
  
  for (tick_ in ls(data))
  {
    #tick_ = "Dax"
    cat("\ncheck symbol ------ ",tick_)
    #  browser() #T
    tick= get(tick_,envir=data)
    
    if (length(tick)==0 || is.null(tick) || !isPrice(tick,tick_))
    {
      print(class(tick_))
      cat("\n +++++#################### rmSymbol ", tick_)
      browser()
      #Browser()#TTT
      #rm(tick_,envir=data)
      rmSymbol(tick_, envir=data)
    }
  }
  cat("<<<<< delNonPrices")
  print(ls(data))
  
}



if (F)
{
  ls(data)
  #ist das tick=data[[i]]  item ein kurs ?
  sym=data[["dates"]]
  head(sym)
  #symbolExists(sym,data)
  as.symbol(sym)
  tickName(sym)
  
  data
  as.name(sym)
  get(sym,data)
  
  exists(sym,data)
}


symbolExists<-function(sym,data=.GlobalEnv)
{
  return(exists(sym,envir = data))
  ok= tryCatch(length(sym)> 0, error = function(...) return(F))
  return (ok)
}


mtrim <- function
(
  s  # string
)
{
  s = sub(pattern = '^ +', replacement = '', x = s)
  s = sub(pattern = ' +$', replacement = '', x = s)
  return(s)
}




#gib mir die Spalte die im Bezeichner col stehen hat
mCol<-function(wp,col)
{
  coli=grep(col,colnames(wp))
  if (coli > 0)
    return(wp[,coli])
  return(NULL)
}





tickName<-function(tick_, data = .GlobalEnv)
{
  tick_= mtrim(tick_)  
  otick = tick_
  
  if (!symbolExists(tick_,data) || !is.character(tick_))
  {
    mL("#####################>>>>>> wrong usage  at tickName")
    return ("")
  }
  
  if (exists(tick_,data))
  { 
    #print( paste("\ntick!Name:", tick_,":"))
    
    tick=get(tick_,data)   
    tick_=""
    
    if (len(colnames(tick))>0)
    {
      col1 = colnames(tick)[1]
      if (len(col1)>0)
        if (len( grep("\\.Close",col1))>0 || len( grep("\\.Open",col1))>0 || len( grep("\\.High",col1))>0 || len( grep("\\.LOw",col1))>0)
        {
          jead=unlist(strsplit(colnames(tick)[1], "\\."))
          if (len(jead)>0 )
            tick_ = otick #jead[1]
        }
    }
    else
      tick_ = ""
    return(tick_)
  }
  else 
    return("") 
}

#tickName("dax")


isPrice <-function(tick,tick_)
{
  
  #return(tickName(tick_)!= "")
  
  #browser() #MMX
  
  
  if (length(tick) ==0 || is.null(tick)  )
    return (F)
  if (length(colnames(tick)) ==0 || is.null(colnames(tick))  )
    return (F)
  
  #print("\n")
  #print(colnames(tick))
  
  alttick_ = unlist(strsplit(colnames(tick)[1], "\\."))[1]
  if (length(which(colnames(tick)==sprintf("%s.Close", tick_)))>0 || length(which(colnames(tick)==sprintf("%s..Close", tick_))))
    return(T)
  if (length(which(colnames(tick)==sprintf("%s.Close", alttick_)))>0 || length(which(colnames(tick)==sprintf("%s..Close", alttick_))))
    return(T)
  
  return(F)
}


#################### Gib die Liste der Symbole aus envir zurÃ¼ck die im Sinne .Open,...Close als tickNames sind

#dataPrices()
dataPrices<-function(envir=.GlobalEnv)
{
  
  t2 =lapply(ls(envir), FUN=function(x)  tickName(x,data=envir))  
  #browser() #T! dataPrices
  if (len(t2)>0)
    return(unlist(t2)[t2 !=""])
  else
  { cat("Warning: dataPrices:  no data !!")
    return(list())
  }
}


#*****************************************************************
# drawDowns f?r Trailingstop
#mr =  cumprod(1+na.omit(diff(log(clos[starti:endi]))))
#DrawD = drawD(mr,8,geometric=T)
#par(mfrow=c(2,1))
#plot(mr)
#plot(DrawD$dd)

#if (DrawD$kbreak>0)#trailing stop triggered
#{}  
#****************************************************************** 
drawD_<-function(y, thresh=0,geometric=F,firstDate =F) #MM!  #MMA
{
  if (!is.null(firstDate))
    y = y[sprintf("%s::",firstDate)]
  
  nal=len(y[is.na(y[,1])])
  if (nal > 0)
    mP("Warning in drawD:  omit %d NA s",nal)
  y=na.omit(y)
  
  kbreak=-1
  maxDDpct=maxDD=0
  bestval = as.numeric(y[1])
  bestvali=1
  dd=y
  
  
  
  for(i in 1:length(y))
  {
    yval=as.numeric(y[i])
    #browser()
    if (yval > bestval)
    {bestval=yval
     bestvali = index(y[i])
    }
    if (geometric)
      DDpct=  DD = (bestval-yval) / bestval * 100
    else
    {DD=bestval-yval
     DDpct = (bestval-yval) / bestval * 100
    }
    #dd[i] = DD
    if (maxDD< DD)
    {maxDD=DD
     maxDDpct= DDpct  
    }
    if (thresh> 0 && maxDDpct > thresh)
    {kbreak=index(y[i])
     break 
    }
    
  }
  DD = (bestval-yval) / bestval * 100
  
  if (kbreak<0) kbreak = ""
  else  kbreak=as.Date(index(y[kbreak]))
  
  if (bestvali <0) bestvali=""
  else bestvali= as.Date(index(y[bestvali]))
  
  return(list(maxDD=maxDDpct,bestvali=bestvali, bestval=bestval,kbreak=kbreak ))
}

drawD<-cmpfun(drawD_) #compilier das Teil


drawU_<-function(y, thresh=0,geometric=F,firstDate =NULL)
{
  if (!is.null(firstDate))
    y = y[sprintf("%s::",firstDate)]
  
  nal=len(y[is.na(y[,1])])
  if (nal > 0)
    mP("Warning in drawU:  omit %d NA s",nal)
  y=na.omit(y)
  
  kbreak=-1
  maxDUpct=maxDU=0
  bestval = as.numeric(y[1])
  bestvali=1
  du=y
  for(i in 1:length(y))
  {
    yval=as.numeric(y[i])
    if (yval < bestval)  #diesmal ist das das min
    {bestval=yval
     bestvali = index(y[i])  #der geht aufs kommende Minimum
    }
    if (geometric)
      DUpct=  DU = (yval-bestval) / yval * 100
    else
    {DU=yval-bestval
     DUpct =  (yval-bestval) / yval * 100
    }
    #du[i] = DU
    if (maxDU< DU)
    {maxDU=DU
     maxDUpct= DUpct  
    }
    if (kbreak ==-1 && thresh> 0 && maxDUpct > thresh)
    {kbreak=index(y[i])
     break 
    }
  }
  DU = (yval-bestval) / yval * 100
  
  if (kbreak<0) kbreak = ""
  else  kbreak=as.Date(index(y[kbreak]))
  
  if (bestvali <0) bestvali=""
  else bestvali= as.Date(index(y[bestvali]))
  
  return(list(maxDU=maxDUpct,bestvali=bestvali, bestval=bestval, kbreak=kbreak))
}

drawU<-cmpfun(drawU_) #compilier das Teil




################################################################################
#zeigt (grafisch animiert in zwei Plots) wie sich eine DrawDown-DrawUp -Folge
#durch die Zeit bewegt - und dabei eine Trendst?cke-partitionierung vornimmt.

#Indikatoren sollten stehts so trainiert werden, dass sie nur an den 
#Trendwechselstellen ?nderungssignale von sich geben !!!
#Dax=Cl(data$DAX)["2007::2009-06"]
if (F)
{  
  DdownDupWalkAhead(Dax,10,5)
  maxDrawDown=10
  
  minDrawUp=5
  visual=T
}
################################################################################
##Dax = Cl(Dax)
DdownDupWalkAhead_<-  function(Dax,maxDrawDown, minDrawUp,visual =T)
{
  lastDate = NULL
  myFirstDate = as.Date(index( first(Dax)))
  lastDate = NULL
  Brk=c()
  Peak=c(first(Dax))
  
  if (visual)
  {
    if (visual) 
      par(mfrow=c(1,1))
    
    plot(Dax)
    maxDax= max(Dax)
    hor = Dax; hor[]=maxDax
    lines(hor,col="blue")
    ddy = (maxDrawDown*maxDax)/100
    hor[]=max(Dax)-ddy
    lines(hor,col="red")
    userStop(" %s  "," - init - ")
    
    par(mfrow=c(2,1))
    
  }
  while(T)
  {
    DrawD = drawD(Dax,maxDrawDown,geometric=T,firstDate=lastDate)
    
    if (visual)
    {
      plot(Dax)
      peak=Dax[DrawD$bestvali]
      brk =Dax[sprintf("::%s",DrawD$kbreak)]
      lines(brk, type="h",col="red")
      lines(brk, type="l",col="blue")
      lines(peak, type="h",col="blue")
    }
    
    if (DrawD$kbreak <0)  #kein Break mehr gefunden
      break
    
    Brk = append(Brk,Dax[DrawD$kbreak])   #Ergebnisbildung
    
    #browser()
    Peak= append(Peak,Dax[DrawD$bestvali])   
    ##suche einen DrawUp von wenigstens  minDrawUp%  -- andernfalls krieg ich f?r diesen Trend nicht mals die Transaktionskosten rein !
    
    lastDate = DrawD$bestvali
    lastDate = DrawD$kbreak
    
    #Dax = Dax[sprintf("%s::",lastDate)]   #weiter am dem bestVall des letzten DD
    DrawU = drawU(Dax,minDrawUp,geometric=T,firstDate=lastDate)
    if (visual)
    {
      plot(Dax)
      peak=Dax[as.Date(DrawU$bestvali)]
      lines(peak, type="h",col="blue")
      brk =Dax[sprintf("%s::%s",lastDate,DrawU$kbreak)]
      lines(brk, type="h",col="green")
      lines(brk, type="l",col="blue")
      lines(peak, type="h",col="blue")
    }
    if (DrawU$kbreak <0)  #kein Break mehr gefunden
      break
    
    Brk = append(Brk,Dax[DrawU$kbreak])   #Ergebnisbildung
    Peak= append(Peak,Dax[DrawU$bestvali]) 
    
    lastDate = DrawU$bestvali
    lastDate = DrawU$kbreak
    
    if (visual)
    {
      print(lastDate)      
      print(Peak)
      userStop(" %s  ",lastDate)
    }
    #print(lastDate)
    if (lastDate <= myFirstDate)
      break 
  }
  #Gib die Liste mit den Zeitstempeln zur?ck
  Peak= append(Peak,last(Dax))
  res =list(brk=Brk, peak=Peak)
  return(res)
}  
DdownDupWalkAhead<-cmpfun(DdownDupWalkAhead_) #compilier das Teil


amark<-function(dat="2002-03-01",col="magenta")
{
  if (is.xts(dat))
    dat = index(dat)
  
  if (is.vector(dat))
  {
    dat=na.omit(dat)
    lapply(dat,function(x) if (x !="") abline(v=as.POSIXct(x),col=col.add.alpha(col , 85),lwd=2))
  }
  else
    abline(v=as.POSIXct(dat),col=col.add.alpha(col , 95),lwd=3)
  return(0)
}
##########################################################################

##########################################################################
mZigZag<-function(Dax, dd=5,visual=F)
{
  ds=  DdownDupWalkAhead(Dax,dd,dd,visual=F)
  if (visual)
  {
    plot(Dax)
    lines(ds$peak,type="h",col="green")
    lines(ds$brk,type="h",col="blue")
    browser()
  }
  return(ds$peak)
}

prePreis<-function(Dax,datum)
{
  #mark(datum)
  #rowser(mP("prePeis"))
  i=get.Index(Dax,datum)
  if (i > 1)
    return(as.numeric(Dax[(i-1),]))
  else
    return(as.numeric(Dax[1,]))
  
}


mZigZag2<-function(Dax, dd=5,visual=F)
{
  print("mZigZag2")
  ds=  DdownDupWalkAhead(Dax,dd,dd,visual=F)
  
  
  
  if (F)
  {
    x=2
    amark(DateS(ds$peak[x]))
    print( ifelse(ds$peak[x]>prePreis(Dax,index(ds$peak[x])),"high","low"))
    browser(mP("zigzag2"))
  }
  k=lapply(c(1:len(ds$peak)),FUN=function(x) 
  {
    return( list(peak =ds$peak[x], brk= as.Date(index(first(ds$brk[as.Date(index(ds$brk))>as.Date(index(ds$peak[x]))]) )), ptype=toString(ifelse(ds$peak[x]>prePreis(Dax,index(ds$peak[x])),"high","low"))))
  })
  
  if (visual)
  {
    plot(Dax)
    lines(ds$peak,type="h",col="green",lwd="2")
    
    lapply(k,function(x) {if (!is.null(x$ptype)) ifelse (x$ptype=="low", amark(index(x$peak),col="red"),amark(index(x$peak),col="blue"))})
    
    lines(ds$brk,type="h",col="blue")
    #  browser()
  }
  
  return(k)  #patch2
}

if (F)
{
  Dax=data$prices[,1]
  Dax=Cl(data$DAX)["2007::2011-08"]
  k=mZigZag2(Dax,dd=5,T)
  #gib Info ?ber peaks und den Zeitpunkt brk zu dem sie identifiziert wurden
  lapply(k,function(x) {
    brk=x$brk;peak=x$peak
    mP("Today is %s , found new %s some days ago: %s %f",brk,x$ptype,DateS(peak),peak)
  })
  zzLo=mZigZag2(Dax,dd=dd,visual=F)
  zzHi=mZigZag2(Dax,dd=10,visual=F)
  
  zbrk.Lo =unlist(lapply(zzLo,function(x){ ifelse(x$ptype=="low",toString(x$brk),NA) }))
  zbrk.Lo[zbrk.Lo==""] = NA; 
  
  
  zbrk.Hi =unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",toString(x$brk),NA) }))
  zbrk.Hi[zbrk.Hi==""] = NA;
  
  plot(Dax)
  peak.Lo =na.omit(unlist(lapply(zzLo,function(x){ ifelse(x$ptype=="low",DateS(x$peak),NA) })))
  peak.Hi =na.omit(unlist(lapply(zzHi,function(x){ ifelse(x$ptype=="high",DateS(x$peak),NA) })))
  
  amark(peak.Lo,"magenta")
  amark(zbrk.Lo,"blue")
  amark(peak.Hi,"red")
  amark(zbrk.H,"blue")
  
}

#wie zigzag oder HighLow oder DdwonDupWalkAhead:  aber hier wird berücksichtigt, dass major breaks erst mit einem zeitl. delay bemerkt werden 
#gib ein xts mit den peak,break - infos zurück. das datum ist das break-datum, also ein paar tage
#nach dem peak, wenn der drawdown so groß ist, das man merkt, ups - das war ja ein peak...
mZigZag3<-function(Dax, dd=5,visual=F)
{
  print("mZigZag3")
  ds=  DdownDupWalkAhead(Dax,dd,dd,visual=F)
  ds$peak = ds$peak[c(2:(len(ds$peak)-1))]
  if(visual)
     {
      plot(Dax,main="mZigZag3")
      amark(ds$peak)   
     }
  print(ds)
if (len(ds$brk)==0)
  return(NULL)
  res= foreach(x =(1:len(ds$peak)),.combine="rbind") %do% #c(2:(len(ds$peak)-1))
{     
  brk.dat= as.Date(index(first(ds$brk[as.Date(index(ds$brk))>as.Date(index(ds$peak[x]))]) ))
  if ((is.null(brk.dat) || len(brk.dat)==0) && x==len(ds$peak))
    brk.dat = as.Date(index(last(Dax)))
  peak =ds$peak[x]
  peak.Dat = DateS(peak);  peak.dat = as.Date(peak.Dat)
  peak.val= nval(peak)
  peak.type=ptype=toString(ifelse(ds$peak[x]>prePreis(Dax,index(ds$peak[x])),"high","low"))
  ret=xts(cbind(peak.type=peak.type, peak.dat=peak.Dat, peak.val=peak.val),brk.dat)
  if (visual)
    {
      amark(dat=ret,col="orange")
      if (peak.type=="high")
        points( xts(Dax[peak.dat,1], peak.dat),col="blue",lwd=3)
      else
        points( xts(Dax[peak.dat,1], peak.dat),col="red",lwd=3)
     }
  ret
}
 res 
}
if (F)
  k=mZigZag3(p["2007::2009"],dd=10,visual=T)

##########################################################

#nutzt mZigZag3 und untersucht noch die relative Lage der Highs (Lows) zueinander
#gib ein xts mit den peak,break - infos zurück. das datum ist das break-datum, also ein paar tage
#nach dem peak, wenn der drawdown so groß ist, das man merkt, ups - das war ja ein peak...
mZigZag4<-function(price, dd=5,visual=F)
{
  sym=colnames(price[,1])
  mP("mZigZag4 %s",sym)
  
  frame=fromToS(price[,1])
  p=price =na.omit(global_arg$dat[[sym]][frame])
  
res=mZigZag3(Lo(price),dd=dd,visual=visual)
if (len(res)==0)
{
  mP("mZigZag3 %s ist zu glatt !",sym)
  return(NULL)
}
lomi= Lo(price)

lows=as.Date(coredata((res[res[,"peak.type"]=="low","peak.dat"])))
lows.brk=as.Date(index(res[res[,"peak.type"]=="low"]))

Lows=Lo(price[lows]);
last.low.trend=nval(last(sign(diff(tail(Lows,2)))))
Lomi=lomi[lows]

lastSeqLen=shape(price[sprintf("%s::%s",(index(last(Lomi))),(index(last(price))))])-1
lastprices =tail(Cl(price),lastSeqLen)
#virtual.min = lastprices[lastprices == min(lastprices),1]

#if  (sign( nval(virtual.min)-nval(last(Lomi)))
#  Lomi = rbind(Lomi,virtual.min)

if (visual)
{
  mchart(merge(Lo(price),Lomi))
  lines(lastprices,col="blue",lwd=3)
  amark(dat=lows)
  #amark(dat=virtual.min)
}

dlomi=sign(ROC(Lomi,n=1))
rl=runLengthEnc2(dlomi)
if (visual)
{
  rl.dat = rl[abs(rl)==1]; amark(dat=index(rl.dat));points(Lomi,col="red",lwd=3)
  View(rl)
  
}
rl.low=xts(coredata(clean.matrix(rl)), as.Date(lows.brk))
#.................
res=mZigZag3(Hi(price),dd=dd,visual=visual)
himi= Hi(price)
highs=as.Date(coredata((res[res[,"peak.type"]=="high","peak.dat"])))
highs.brk=as.Date(index(res[res[,"peak.type"]=="high"]))

Highs=Hi(price[highs]);

last.high.trend=nval(last(sign(diff(tail(Highs,2)))))
Himi=himi[highs]

lastSeqLen=shape(price[sprintf("%s::%s",(index(last(Himi))),(index(last(price))))])-1
lastprices =tail(Cl(price),lastSeqLen)
#virtual.min = lastprices[lastprices == min(lastprices),1]

#if  (sign( nval(virtual.min)-nval(last(Lomi)))
#  Lomi = rbind(Lomi,virtual.min)

if (visual)
{
  mchart(merge(Hi(price),Himi))
  amark(dat=highs)
  points(Highs,col="blue",lwd=3)
  lines(lastprices,col="blue",lwd=3)
  #amark(dat=virtual.min)
}

dhimi=sign(ROC(Himi,n=1))
rl=runLengthEnc2(dhimi)
if (visual)
{
  rl.dat = rl[abs(rl)==1]; amark(dat=index(rl.dat));points(Himi,col="blue",lwd=3)
  View(rl)
  
}
rl.high=xts(coredata(clean.matrix(rl)), as.Date(highs.brk))
rl.all = merge(rl.low,rl.high)

rl.all.one=xts2xts(lineal=rl.all,rowSums(rl.all,na.rm=T))
#.............................................
#Schwäche:  Wenn in einem plötzlich glatten, guten Trend keine Zacken kommen,  bekommt er auch kein Signal:
#mische also in die NA-Stücke signale hinzu (gem. diff-vorzeichen) sobald kurz oberhalb
k1=rl.all.one
res =  xts2xts(lineal=p,k1,fill=F)
p1=merge(res,p)
p01=p1[!is.na(p1[,1]),2]
p02 =  xts2xts(lineal=p,p01,fill=T)
d1=(p1[,2]-p02)/p02*100   #pct-kursabstand heute zur letzten ecke
d2=iif(abs(d1)>dd+dd, sign(d1),NA)    #schwellwert dd +dd
mm=merge(p1[,1],d2)
mm2=iif( !is.na(mm[,1]),mm[,1],mm[,2] )
rl.added = m.ifna.prev(mm2)

sig=sign(rl.added)

if (visual)plotSigPrice(signal=sig,p)

res = list(lowhigh=rl.all, lowhigh.one =rl.all.one, rl.added =rl.added) 
}


if (F)
{
  p=data$prices["::2007","SXQBP"]
  p=data$prices["","SXQBP"]
  k=mZigZag4(p,dd=5,visual=T)
 
  #k=mZigZag3(mNorm(lomi-1000),dd=3,visual=T)

#ls(dd)
}

#############################################################
#liest mit Runlength-Encoding die eingetroffenene Signale (-1,0,1)
#und z?hlt die trains 
#############################################################
numTrades_<-function(Sig)
{
  allT =0
  res=list()
  Sig[Sig==0] <- -1
  Sig = sign(Sig)
  
  for (i in c(1:ncol(Sig)))
  {
    i=1
    sig = as.vector(Sig[,i])#segmente positiver und negativer steigung
    enc <-rle(sig)
    n=  len(enc$lengths)  #ending indices
    name=colnames(Sig)[i]
    if (is.null(name))
      name = colnames(prices)[i]
    if (is.null(name))
      name = "Sig"
    res[[name]] = n
    allT = allT+n
  }
  res[["allT"]] = as.numeric(allT)
  return(res)
}

numTrades<-cmpfun(numTrades_) #compilier das Teil

#################################################################################
#################################################################################

getCol <-function (dat,col) 
{
  return(dat[, grep(col, colnames(dat), ignore.case = TRUE)])
  stop(sprintf("getCol(): subscript out of bounds: no column name containing %s",col))
}

#################################################################################
#################################################################################

wait <- function(){
  t <- readline("\nPlease 'q' to quit the demo or any other key to continue...\n")
  if (t == "q") TRUE else FALSE
}

global_warte=T
#################################################################################
#################################################################################
sag <- function(str,...,warte=F){
  star <- "**********"
  cat(paste("\n",star,"\n",sprintf(str,...),"\n",star,"\n",sep=""))
  if (warte && global_warte)     wait()
}
##################################################################################
#transformiere monats oder wochendaten zu tagedaten
#quantmod hat auch:  (muss aber mit foreach multi-gespaltet werden)
#allReturns: calculate all available return periods
#dailyReturn: calculate daily returns
#weeklyReturn: calculate weekly returns
#monthlyReturn: calculate monthly returns
#quarterlyReturn: calculate quarterly returns
#annualReturn: 
##################################################################################
mto.daily<-function(monthdata)
{
  ft = fromTo(monthdata)
  startDay =ft[1]
  endDay = ft[2]
  #anzahl tage im zeitraum 
  days = -as.numeric(difftime(startDay,endDay),units="days")
  
  #eine leere xts-reihe mit dageswerten von bis
  emptyXTS = xts(1:days, as.Date(startDay)+1:days)
  #da werden die ifo-monats-werte reingeeschrieben
  ifoDrin= merge(emptyXTS, monthdata)
  #  ifoDrin = ifoDrin[,-1]
  #  head(ifoDrin)
  # backfill  auff?llen der tagedaten
  ifo=ifoDrin
  ifo[] =apply(coredata(ifoDrin), 2, m.ifna.prev)
  #rausfiltern von wochenenden
  ifo= ifo[isWeekday(time(ifo))]
  ifo = ifo[,-1]
  
  colnames(ifo)=c("dayData")
  return (ifo)
}

#################################################################################
#################################################################################
addMonth<-function(dat,n=1)
{
  s=sprintf("%d month",n)
  return (seq(as.Date(dat), by = s, length = 2)[2]  )
}

#################################################################################
#addTime(dat,-3,"day")
#################################################################################
addTime<-function(dat,n=1,d="day")
{
  s=sprintf("%d %s",n,d)
  return (seq(as.Date(dat), by = s, length = 2)[2]  )
}

######################################################################################################
# 
######################################################################################################
expectTrendBreak<-function(prices=edhec,t, maxdd=10, weights=NULL,geometric=TRUE, nStd=FALSE,visual=T)
{
  prices=prices[sprintf("%s::",t)]
  maxdd = 0.5
  colnames(prices)="Close"
  zz=na.omit(ZigZag(prices,change=maxdd,percent=T))
  
  sig=as.vector(coredata(sign(zz-lag(zz))))  #segmente positiver und negativer steigung
  enc <-rle(sig)
  enc.endidx <- cumsum(enc$lengths)  #ending indices
  enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(prices))  # starting indices
  
  #markiere die higss- und lows- des zigzag in unterschiedlichen farben
  highx = na.omit(enc.endidx[enc$values > 0 ]) 
  highs = prices[highx] 
  #ein Trend liegt vor, wenn die Highs in die H?he gehen
  dh = highs-lag(highs)
  colnames(dh) = "deltaHigh"
  dh[,1] = ifelse(is.na(dh),0,dh)
  
  # so lange die Highs h?her gehen geh ich von einem intakten Trend aus ...
  
  for(n in 1:length(highs))
  {
    n=2
    now_ = time(highs[n])
    if (n==1)
    {
      lastT=time(prices[1])
      trend = prices; trend[,1]=NA; TRENDn=trend[1,1]= na.omit(enc$values)[1]
      trendAbschnitt = sprintf("%s/%s",lastT,now_)
      trend[trendAbschnitt,1] = TRENDn
    }
    else
    {
      if (dh[now_] < 0 && TRENDn ==1) # trendbreak
      {
        trendAbschnitt = sprintf("%s/%s",lastT,now_)
        trend[trendAbschnitt,1] = TRENDn
        TRENDn=-1
        lastT =now_  #trendbreak
      }  
      else
      {
        trendAbschnitt = sprintf("%s/%s",lastT,now_)
        trend[trendAbschnitt,1] = TRENDn    
      }
    }   
  }  
}

##NOCH IM TEST

buyTilBreakPerf<-function(prices=edhec, maxdd=10 )
{
  #maxdd=10
  #rausfiltern von Wochenenden
  
  prices= prices[isWeekday(time(prices))]
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
  points(zzPoints,col="green",lwd=2)
  lines(zzPoints,type="l",col="red",lwd=2)
  
  ########################################
  ## summiere alle long-Teil-St?cke der zz-Zerlegung auf ret
  ret=0
  
  for(i in 1 :len(zzPoints))
  {
    #  i=len(zzPoints)
    i=5 #TEST
    now_ = as.Date(time(zzPoints[i]))
    tagSpaeter=as.Date(now_)+1
    
    tagSpaeterPrice = prices[tagSpaeter,1]
    
    isWeekend(tagSpaeter)
    
    zP = prices[as.Date(now_)]
    
    if (i==len(zzPoints))
      nextP=last(prices,1)[,1]
    else
      nextP = zzPoints[i+1,1]
    if (coredata(nextP[,1] )> coredata(zP[,1]) && tagSpaeter != now_) #long
    {
      ret = ret+(as.numeric(nextP[,1]) - as.numeric(prices[tagSpaeter,1]) ) #Transaktionskosten gehen rein indem einfach der Kurs des schlechten Folgetags genommen wird
      cat(i,"  ",toString(as.Date(now_)),toString(as.Date(tagSpaeter)), zP,tagSpaeterPrice,nextP[,1],"   ",ret,  "   \n")
    }
  }
  #ret als pct
  ret = ret/prices[1,1]*100
  
  return (as.numeric(ret))
  
}  

#goodReturn (dax,t="2005", maxdd=10,checkSGB=T)

###################################
#goodReturn #MMA
#wichtig:  die prices m?ssen mNorm sein
#ohne ylim: - warum zeigt der ZickZack 2008 ne viel feinere Zerlegung wenn ich 2007 - statt 1900 anfang ??
#ylim gibt die Range an, auf die sich die DrawDown- in Prozent beziehen - entspricht also quasi dem 100% DrawDown.
#wenn ich keinen ylim geb. wird der automatisch aus der via-frame-Selektion eher zuf?lligen range() des sichtbaren
#zeitreihenausschnitts gew?hlt ... und der kann sich heftig ?ndern !!! 
#f?r ylim muss ich also objektive Werte finden - sonst werden die segmentierunge abh?nig davon welchen ausschnitt der 
#reihe ich mir anschau.
# Je weiter der ylim  Bereich ist, desto mehr Segmente wirds bei gleichem maxdd gegeben.
# im Vergleich zum  sehr langzeitigen Plot - nimmt ein Ausschnitt sich mehr y -Bandbreite
# so kommen gleiche Ergebnisse raus:
if (F)
{
  dax = merge(Hi(Dax),Lo(Dax),Cl(Dax))
  AllDax = mNorm(dax)[,3]
  P=AllDax["2007::2009"] 
  range(P)
  
  ylim= range(AllDax["2007::2009"])
  ylim
  range(AllDax)
  
  
  goodReturn(prices=AllDax,  maxdd=37,visual=T,checkSGB=T)
  #die k?rze Zeitreihe P bekommt die gleiche dynamik (ylim) zugeteilt- wie die Lange AllDax im betreffenden Zeitraum hat.
  goodReturn(prices=P,  maxdd=37,visual=T,checkSGB=T,ylim=ylim)
  
  
  goodReturn(prices=AllDax,  maxdd=37,visual=T,checkSGB=T,ylim=ylim)
  
}


###################################
goodReturn<-function(prices=edhec,t="", maxdd=10, weights=NULL,geometric=TRUE, nStd=FALSE,minReturn=0,checkSGB=F, visual=T,ylim = NULL)
{
  if (t != "")
    prices=na.omit(prices[sprintf("%s::",t)])
  else
    prices = na.omit(prices)
  pricesL1 = as.numeric(last(prices))
  #prices=mRendite(mROC(prices))
  pricesL2 = as.numeric(last(prices))
  #fac = pricesL1/pricesL2 
  #browser() 
  #plot(oprices)
  oprices = prices
  range(oprices)
  if (!is.null(ylim))
    prices = scaleTo(prices,ylim)  #vor dem ZigZag schnell mal in ein Positiv-Skalierung wechseln - sonst klappts nicht
  
  zz=ZigZag(prices,change=maxdd,percent =T,retrace=F)
  zz=na.omit(zz)  #bl?der weise gibts manchmal vorn oder hinten Inf- Werte
  
  if (len(zz)< 3)
    return(0)
  if (visual)
  {
    #browser()
    #  new_Win(1)
    plot(prices)
    lines(zz,col="red")
  }
  
  sig=as.vector(coredata(sign(zz-lag(zz))))  #segmente positiver und negativer steigung
  enc <-rle(sig)
  enc.endidx <- cumsum(enc$lengths)  #ending indices
  enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(prices))  # starting indices
  
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
  
  #browser()
  ZZprice = oprices[enc.endidx,1]
  prices = oprices[,1]
  
  if (visual)
  {
    par(mfrow=c(1,1))
    plot(prices)
    lines(ZZprice,col="red")
    lines(prices,col="blue")
    
    points(prices[highx],col="green")
    points(prices[lowx],col="magenta")
  }
  alleSegmenteI = enc.endidx
  
  if(checkSGB) #Saegeblatt-BreitenUntersuchug
  {
    lasti=1
    alleSGB=c() #pro Segment eine Saegeblatt-Breite
    
    for(i in alleSegmenteI) #?ber alle zz-Segmente
    {
      if (i >first(alleSegmenteI))
      { 
        xxRet  =mROC(prices[lasti:i])
        xxPrices =prices[lasti:i,1]
        
        segx1 = lasti;
        segx2 = i
        #browser()
        if (visual)
        {
          lines(xxPrices, col="grey") #segmentabschnitt in gray
          
        }
        if (T)
        {
          X = c(segx1:segx2)
          reg=lm(xxPrices~X)    
          #reg=lm(xxPrices~c(1:len(xxPrices)))
          regfitted.values=reg$fitted.values
          lines(regfitted.values, col="green")
          #
          
          ddd = (max(xxPrices)-min(xxPrices))/2
          #if (visual)
          #  lines(xxPrices-regfitted.values+max(xxPrices)-ddd,col="brown")
          xNorm=xxPrices-regfitted.values 
          #die S?gezahnbreite
          es = abs(as.numeric(ES(p=0.90,xNorm,method = "historical")))
          alleSGB=c(alleSGB,es)
          
          if (visual)
          {
            lines(regfitted.values-es, col="brown")
            lines(regfitted.values+es, col="brown")
            
            
            
            if (F)
            {
              Maxdd = as.numeric(last(xxPrices)*maxdd/100)
              val1 = last(xxPrices)-Maxdd
              val2 = last(xxPrices)+Maxdd
              #browser()
              lines(val1, col="magenta",type="h")
              lines(val2, col="red",type="h")
            }
          }
          
          
          #predict(reg, type="response") # predicted values
          #resid=residuals(reg, type="deviance") # residuals
          
          nahead=len(prices)-segx2
          
          #prognose berechnen   #MMA
          prog= predict(reg, newdata = data.frame(X=c((segx2+1):(segx2+nahead))))
          #die prognoseXts erh?lt ihre coredaten aus der Prognose - die Dat?mer wie Fortschreibung
          progXts = xts(coredata(prog), index(prices)[segx2]+1:nahead)
          m= coef(reg)[2]
          
          if (m>=0)
            progXts = progXts-es
          else
            progXts =progXts+es
          
          #Der schnittpunkt ist der erste Punkt wo die preise unter die Prognose rutschen
          
          
          
          if (m >= 0)  #der vorhergehende Trend stieg - er prognostiziert damit dann (am Tag seines Endes)
            #die Stop-Level f?r den folgenden Trend - und zeichnet sie schon mal ein.
            cross = first(prices[segx2:len(prices)][prices <= progXts ])
          else            
            cross = first(prices[segx2:len(prices)][prices >= progXts ])
          
          # browser()
          if (len(cross)>0)
          {
            points(cross,col="red",lwd=3)
            #zeichne die Prognose-Gerade  ein - aber nur bis zum cross
            daysuntilCross=c(1: len(time(prices[segx2]):time(cross)))
            crossline = progXts[daysuntilCross]
            
            lines(crossline ,col="red",lwd=2)
          }
          #browser()
          #weitere stoplinien:
          minLineBreite = 460
          #s?geblattbreite unter dem High/Low des lezten Segement -Endes  
          if (m> 0)  
            thresh1 = xts(coredata(rep(max(xxPrices)-2*es ,   minLineBreite)), index(prices)[segx2]+1:  minLineBreite)
          else
            thresh1 = xts(coredata(rep(min(xxPrices)+2*es ,   minLineBreite)), index(prices)[segx2]+1:  minLineBreite)
          lines(thresh1,col="magenta",lwd=2)
          #maxdd unter dem High/Low des lezten Segement -Endes   - erst hier schl?gt der ZickZack zu ... !!!
          if (m> 0)  
            Thresh= max(xxPrices)-maxdd*max(xxPrices)/100
          else
            Thresh= min(xxPrices)+maxdd*max(xxPrices)/100
          thresh2 = xts(coredata(rep(Thresh ,   minLineBreite)), index(prices)[segx2]+1:  minLineBreite)
          lines(thresh2,col="black",lwd=2)
          
          #browser()
          #goodReturn(prices=AllDax,  maxdd=37,visual=T,checkSGB=T,ylim=ylim)
          
        }
        #  browser() 
      }
      lasti=i  #n?chstes Segment
    }
    
  }
  
  lagPrice = mlag(ZZprice)
  lagPrice[1,1]=0
  
  slippageI = enc.endidx+1
  slippageI[len(slippageI)]=last(enc.endidx) #den letzten Index gleich setzen - weil sonst ?berlaf
  slippageP = ZZprice
  slippageP[,1]= coredata(prices[slippageI,1]) 
  slippage = ZZprice-slippageP
  
  transactionCosts = 0.03
  rets = (ZZprice-lagPrice)*(sign(ZZprice-lagPrice))-transactionCosts
  rets = rets-20*abs(slippage)
  
  cs=cumsum(rets)
  erg = as.numeric(last(cs))
  if (visual)
  {
    plot(cs)
  }
  if (checkSGB)
  {
    #browser()
    es=mean(alleSGB) #die mittlere Saegblatt-Breite 
    return(list(Segmente=len(alleSegmenteI)-1,Ertrag=erg, alleSGB=alleSGB, SaegeblattBreite=2*es))
  }
  return(list(Segmente=len(alleSegmenteI)-1, Ertrag=erg))
}




if (F)
{
  dax = merge(Hi(Dax),Lo(Dax),Cl(Dax))
  AllDax = mNorm(dax)[,3]
  P=AllDax["2007::2009"] 
  range(P)
  
  ylim= range(AllDax["2007::2009"])
  ylim
  
  goodReturn(prices=AllDax,  maxdd=37,visual=T,checkSGB=T)#MMA  - warum zeigt der ZickZack 2008 ne viel feinere Zerlegung we
  goodReturn(prices=P,  maxdd=37,visual=T,checkSGB=T,ylim=ylim)#MMA  - warum zeigt der ZickZack 2008 ne viel feinere Z 
  
  
  P0=mNorm(dax)
  mPlot(P0,ylog_=F)
  plot(P0)
  P=scaleTo(P,range(P0["2007::2009"]))
  
  PS=mmerge(P0,P)
  mPlot(PS,ylog_=F)  
  tail(PS)
  Ps =PS[,5]
  ylim=range(PS["2007::2009",1])
  goodReturn(prices=Ps,  maxdd=37,visual=T,checkSGB=T,ylim=ylim)#MMA - warum zeigt der ZickZack 2008 ne viel feinere Zerlegung we
  
  
  
  
  #bb=BBands(P,n=20,sd=2.5)[,c(1:3)]
  #mPlot(merge(P,bb))
  #dbb = P-bb
  #mPlot(merge(P,dbb[,1]))
  new_Win(1)
  bestDD = findBestDrawDown(prices=mNorm(dax[,1]))
  
  goodReturn(prices=P,  maxdd=37,visual=T,checkSGB=T) #MMA  - warum zeigt der ZickZack 2008 ne viel feinere Zerlegung wenn ich 2007 - statt 1900 anfang ??
}



##wie merge aber die Daten werden nicht auf den gemeinsamen Bereich verk?rzt - vielmehr werden die NA die bei reinem cbind auftreten durch die Start/Stop-Werte  ersetzt
mmerge<-function(...)  #MMA
{
  Prices1= cbind(...)
  if (is.null(Prices1))
    browser(mP("bug in mmerge"))
  if (nrow(Prices1)<2)
    return (Prices1)
  
  prices1 = coredata(Prices1)
  prices1=apply(prices1, 2,m.ifna.prev)
  
  #prices1=apply(prices1, 2, ifna.prev.rev)  #wende die Funktion auf alle Spalten der Martrix an ..
  
  coredata(Prices1) <-prices1
  return (Prices1)
}
mmergeRev<-function(...)  #MMA
{
  Prices1= cbind(...)
  if (nrow(Prices1)<2)
    return (Prices1)
  
  prices1 = coredata(Prices1)
  prices1=apply(prices1, 2,m.ifna.prev)
  
  prices1=apply(prices1, 2, ifna.prev.rev)  #wende die Funktion auf alle Spalten der Martrix an ..
  
  coredata(Prices1) <-prices1
  return (Prices1)
}

####################
#zeigt unterschiedlich lange Zeitreihen an - ohne sie auf ein gemeinsames x-intervall zu begrenzen
####################
mPlotPureData<-function(...)  #MMA
{
  
  try(
{
  Prices1=mmerge(...)
  mPlot( Prices1,ylog_=F,main=main)
  
  nrow(Prices1)
  
}
  )

}

############### HighLight ist ein eindim-xts und wird orange gemalt
############### LockLight soll die gleiche Dimension wie Prices1 haben, 
purePlot<-function(...,main="",HighLight=NULL, LockLight = NULL, plotX=T,col="black",Signal=NULL)
{
  
  try(
{
  #browser(mP("purePlot"))
  Prices1=mmerge(...)
  title=sprintf("%s %s",main,fromTo(Prices1))
  Prices1[is.infinite(Prices1)]<-NA
  #x.highlight=get.Index(prices,as.Date(index(HighLight)))
  
  x.highlight= temp= which(!is.na(merge(HighLight,Prices1))[,1])
  
  ylim = range(Prices1,na.rm=T)
  
  
  if (main=="" || main=="no")
    plota(Prices1,type="n",plotX=plotX,ylim=range(Prices1,na.rm=T),x.highlight=x.highlight)
  else
    plota(Prices1,type="n",main=main,plotX=plotX,ylim=range(Prices1,na.rm=T),x.highlight=x.highlight)
  
  #pric#plota.grid()
  # browser()
  colSet = c(col,rainbow(ncol(Prices1)))
  #colSet = c("red", topo.colors(ncol(Prices1)))
  #colSet = c("red",palette(gray(seq(0,.9,len=ncol(ret)-1))))
  
  #plot((mat[,1,drop=F]), ylim =rang, main=title)
  
  if (T)
  {
    #axis(4)  
    for(ci in 1:ncol(Prices1))
      lines(Prices1[,ci,drop=F],col=colSet[ci],lwd=2)
  }
  plota.legend(colnames(Prices1),colSet[1:ncol(Prices1)])#,pch=0.5,cex=0.5,y.intersp=0.7)
  
  
  
  return(1)##########################################################
  
  
  #  for(k in c(1:dim(Prices1)[2]))
  #           plota.lines(Prices1[,k],col="red")
  
  #  return(1)
  # mPlot( Prices1,ylog_=F,main=main)
  #  browser()
  HighLight=HighLight[,1]
  if (!is.null(HighLight))
    rollapplyr(HighLight,width=1,FUN=function(HighLight)
    {
      problem.date = DateS(HighLight)
      amark(problem.date,col="darkgreen")    
      
    }) 
  
  nrow(Prices1)
  
} )

#  )
}

if (F)
{
  HighLight=mNorm(prices[,1])[mNorm((prices[,1]))>1.5 & mNorm((prices[,1]))<2.5 ]
  fromTo(HighLight)
  purePlot( mNorm((prices)),plotX=T,main="test", HighLight=HighLight) 
}


############################### Teste das colle xtsExta
if(F)
{
  library(xtsExtra)
  #plot so nicht mehr funktionieren  .. siehe mchart .. purePlot aber wohl !!
  
  #  plot(dax,main="hallo") werden nicht mehr verbunden
  #  lines(dax,col="red")
  
  plot.xts(prices,
           screens=c(1,2,2), #plot 1st series in 1st panel and 2nd and 3rd series in 2nd panel
           layout.screens=c(1,1,2), #just as an example change the layout so 1st panel is 1/3 of area and 2nd is bottom 2/3
           blocks = list( #set up blocks for recessions
             start.time=as.Date(index(HighLight)),
             #langer ausruck, damit nicht dauernd l?cken in der einf?rbung sind wenn der folgetag ein wochenende ist
             end.time=as.Date(index(prices[get.Index(prices,as.Date(index(HighLight)))+1])),
             col = "lightblue"),
           lwd = 1,
           legend.loc = "topleft", auto.legend=TRUE,
           main=title)
  detach("package:xtsExtra")#  ... beisst sich leider mit xts - also wird mein oft benutzer
  
}


####backfill- umgekehrt
#entfernt die linken (fr?hen) NA- Werte durch den ersten non-NA-Wert
#gegenst?ck zum ifna.prev() des sysinvestors.
ifna.prev.rev<-function(p)     #MMA
{
  return( p[ifna.prevx.rev(p)])
}



findBestDrawDown<-function(prices=dax,t="",minReturn=0.3,ylim=c(0,100))  #MMA
{
  res = list()
  bestRet=list()
  bestVal = 0
  reg=0
  mRES=c()
  
  #Versuche die DrawDownWerte 1..30
  for(dd in seq(1:60))
  {
    reg=goodReturn(prices =prices, maxdd = dd, t = t, minReturn= minReturn,checkSGB =F, visual = F,ylim=ylim)
    reg= reg$Ertrag
    mRES= c(mRES,reg)
    Reg = c(dd,reg)
    if (bestVal==0||reg>bestVal ) 
    {bestRet = Reg
     bestVal= reg
    }
    res = list( Reg,res )
  }
  
  dd= bestRet[1]
  print(bestRet)
  reg=goodReturn(prices = prices, maxdd = dd, t = t, minReturn= minReturn ,checkSGB = T, visual = T)  
  
  cat("bestRet at maxdd ",bestRet,"\n ")
  
  norm_Win(2)
  plot(mRES     )
  plot(prices)
  
  return(bestRet[1])
  #res[[1]][2]
}



#################################################################################
#welcher Return kann von prices  ab zeit t via buy hold erreicht werden ehe ein
#maxdrawdown von  maxdd ausstoppt ?
#visual = T  ergbit 2 plots
#################################################################################
#t="2005"
#prices=dax

goodReturn1<-function(prices=edhec,t, maxdd=10, weights=NULL,geometric=TRUE, nStd=FALSE,minReturn=0, visual=T)
{
  killedPreSegment=F
  prices=prices[sprintf("%s::",t)]
  
  prices=mRendite(mROC(prices))
  lastOKSegmentES=0
  
  if (F)
    #automatische Ermitllung des besten Stop-Bereiches
    if (is.null(maxdd) || maxdd ==0)
    {
      bestdd = 5
      bestRet = -1
      for (dd in 5:30)
      {
        dd=8
        
        thisRet= buyTilBreakPerf(prices, dd)
        if (thisRet>bestRet)
        {
          bestRet=thisRet; bestdd = dd
        }
      }
      maxdd = bestdd
    }
  
  zz=ZigZag(prices,change=maxdd,percent =T)
  
  plot(prices)
  lines(zz,col="red")
  sig=as.vector(coredata(sign(zz-lag(zz))))  #segmente positiver und negativer steigung
  enc <-rle(sig)
  enc.endidx <- cumsum(enc$lengths)  #ending indices
  enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(prices))  # starting indices
  
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
  #if (first(prices))
  len(highx)
  points(prices[highx],col="green")
  points(prices[lowx],col="blue")
  
  #browser()
  if (len(highx)<1)
    maxP = last(prices)
  else
    maxP = prices[first(highx)]
  
  alleSegmenteI =enc.endidx
  alleSegmenteI2 = alleSegmenteI
  #?ber alle Segmente des zigzack
  #berechne f?r jedes Segment die Streuung um die Regression-Line (mean-Reverter)
  lasti=1
  lastOKSegmentES=0
  segI=0
  for(i in alleSegmenteI) #?ber alle zz-Segmente
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
      
      
      if (T)
      {
        reg=lm(xxPrices~c(lasti:i))    
        regfitted.values=reg$fitted.values
      }
      else#dynamisierte Version der Regression 
      {
        if(i-lasti > 10 )#&& i +10< len(prices))
        {
          regfitted.values = prices[c(lasti:(lasti+10-1))]
          for(i5 in seq(lasti+10:i))
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
                #browser()
              }            
          }
        }
        else
          regfitted.values = prices[c(lasti:i)]
        
        
      }
      if (visual)
        lines(regfitted.values, col="green")
      
      ddd = (max(xxPrices)-min(xxPrices))/2
      if (visual)
        lines(xxPrices-regfitted.values+max(xxPrices)-ddd,col="blue")
      
      
      xNorm=xxPrices-regfitted.values 
      
      #browser()
      
      #die S?gezahnbreite
      
      es = ES(p=0.99,xNorm,method = "historical")
      
      
      #min(xNorm)<es
      segmentReturn=mROC(append(prices[segx1],prices[segx2]))
      isTrendSegmentOK = abs(segmentReturn) > lastOKSegmentES  && abs(segmentReturn)> minReturn
      
      if( (lastOKSegmentES == 0 || isTrendSegmentOK))
      {
        lastOKSegmentES=abs(es)
        killedPreSegment = F  
      }
      else #l?sche das unrentable Segment 
      { 
        #die letzten zwei Knoten entfernen - durch setzen von killedPreSegment
        if (visual)
        {
          cat("\n",segI,":kill node ",alleSegmenteI2[segI],"\n segmentReturn is only ",segmentReturn,"\n")
          cat(alleSegmenteI2)
          cat("\nat\n")
          cat(segI,":   ",segx1,segx2,"\n")
        } 
        alleSegmenteI2 = alleSegmenteI2[-c(segI,segI-1)]
        segI = max(1,segI-2)
        lastI = alleSegmenteI[segI]
        segx1= lastI
        newZ=prices[c(segx1,segx2)]
        lines(newZ,col="blue")
        cat("\nnew\n",alleSegmenteI2)
        #browser()
        
      }
      
      if (visual)
      { cat(isTrendSegmentOK)
        cat(" ")    
      }
      
      #browser()
      
      
      
      lasti=i  #n?chstes Segment
    }
  }
  
  cat("--------------------->")
  ret= (as.numeric(maxP)-as.numeric(prices[1,1]))/as.numeric(prices[1,1])*100
  rR=prices[1:highx[2]]
  cat("Ret ",ret)
  #browser()
  
  newZZ=prices[alleSegmenteI2]
  
  lines(newZZ,col="brown",lw=2)
  
  #if (visual) lines(rR,col="blue",type="h")
  return (ret)
  ##################################################
  
  
  
  
  
  
  
  Rorg = mROC(prices)
  R=Rorg
  
  if (visual)
  {
    
    par(mfrow=c(2,1))
    DJ.roc <- R#ROC(prices,n=1,type="discrete")
    DJ.draw = Drawdowns(DJ.roc)
    sort(coredata(- DJ.draw),decreasing = T)
    
    #jpeg(filename="DJ plot with Drawdowns zoo.jpg",
    #  quality=100,width=6.25, height = 6.25,  units="in",res=96)
    plot.zoo(mRendite(R),plot.type="single",
             col=c(2,3,4),ylab="log Price",xlab=NA,main="Dow Jones Indexes")
    
    lines(zz,col="green",lwd=2)
    
    rgb <- hcl(c(0, 0, 260), c = c(100, 0, 100), l = c(50, 90, 50), alpha = 0.3)
    xblocks(index(DJ.roc),as.vector(DJ.draw[,1] < -maxdd/100),col = rgb[1])
    
    xblocks(index(DJ.roc),as.vector(DJ.draw[,1] < -0.20),col = rgb[1])
    #  legend("topleft",inset=0.05,colnames(prices),fill=c(2,3,4),bg="white")
    
    plot(mRendite(R),main=sprintf("goodReturn %s %s %f", colnames(prices)[1],t,maxdd))
    
    
  }
  
  t2 =time(last(R))
  dd =findDrawdowns(ret[,drop=FALSE])
  len(dd$trough)
  len(dd$return)
  
  dds= as.xts( -dd$return*100,order.by=as.Date(time(R[dd$trough,1])))
  
  #table.Drawdowns(R, top = 10, digits = 9)
  #maxdd=80
  ddx = which(dds > maxdd)
  di=dd$trough[ dds > maxdd]
  
  
  if (length(di)>0) #ausgestoppt
  {
    di = first(di)
    t2 = time(R[di])
    R=R[sprintf("::%s",t2)]    
  }
  
  rR=mRendite(R)
  if (visual) lines(rR,col="blue",type="h")
  
  x=rR[rR==max(rR)]
  wo = time(x)
  if (visual) lines(rR[sprintf("%s::",wo)],col="red") 
  gRend= mRendite(Rorg[sprintf("%s/%s",t,time(x))])
  
  
  if (visual)  cat("maxDrawdown ",maxDrawdown(gRend)*100  ,"\n")
  #  findDrawdowns(gRend)
  lines(gRend,col="green")
  guv = as.numeric(last(gRend))-as.numeric(first(gRend))
  return(guv)
}



#################################################################################
#scaliere eine Zeitreihe auf einen gegeben rangen (vector aus zwei werten - min,max)
#################################################################################
scaleTo<-function(close, rang,srange=NULL)
{
  #head(close)
  close=na.omit(close)
  toMin=min(rang,na.rm=T)
  toMax=max(rang,na.rm=T)
  mi=min(close,na.rm =T)
  ma=max(close,na.rm=T)
  
  
  if (is.null(srange))
  {
    fac=(toMax-toMin) / (ma-mi)
    ret=close*fac#-mi*fac
    ret= ret-min(ret)+toMin
  }
  else
  {
    miS=srange[1]
    maS=srange[2]
    corfac = (ma-mi) /  (maS-miS)
    #  browser()
    #corfac=1
    #  fac=(toMax-toMin) / (ma-mi) * corfac
    #  print("scale")
    #  browser()
    #browser()
    #  fac1=(toMax-toMin) / (ma-mi)
    #  ret=close*fac1
    #  ret= ret-min(ret)+toMin
    
    
    fac=(toMax-toMin) / (maS-miS) 
    
    ret=close*fac
    ret= ret-min(ret,na.rm=T)+toMin
    
    head(ret)
    range(close)  
  }
  
  
  return(ret)
}

#################################################################################
#################################################################################

#####################################################
#listet alle in einem r-file definieren funktionen
######################################################
list_SIT_functions<-function()
{
  list_R_functions(file="",path="SysInvestor/SIT-master/R/",recursive=T,include.dirs=T)
}

if (F)
  list_SIT_functions()

list_R_functions<-function(file="",path="MLIB/",search="",include.dirs = FALSE,recursive = FALSE)
{
  ret=c(mP("###list_R_functions: #################### %s ################>",file))
  #data=new.env()
  if (file=="")
  {
    
    ###f?r beide Wildcards  eine liste bauen *.r  und *.R
    allFiles=list()
    xlsPat = glob2rx("*.R") #wildcard-Transfer
    for (rfile in 
         dir(path = path, pattern = xlsPat, all.files = T,
             full.names = T, recursive = recursive,
             ignore.case = FALSE, include.dirs = include.dirs)
    )
      allFiles=append(allFiles,rfile)
    xlsPat = glob2rx("*.r") #wildcard-Transfer
    
    for (rfile in 
         dir(path = path, pattern = xlsPat, all.files = T,
             full.names = T, recursive = recursive,
             ignore.case = FALSE, include.dirs = include.dirs)
    )
      allFiles=append(allFiles,rfile)
    
    allFiles=unlist(allFiles)
    print(allFiles)  
    
    
    for (rfile in  allFiles)  
    {
      data = new.env()
      #browser(mP("rfile"))
      mP("######### SOURCE ######>>  %s ",rfile)
      try(
        source(local=data, file=rfile)#sprintf("%s%s","MLib/",rfile))
      ) 
      if (len(ls(data))==0)
        next
      #print(ls.str(mode = "function",envi=data))
      # browser(mP("sag %s,"rfile))
      
      funcs= c(lsf.str(envi=data))
      
      if (search != "")  #wenn search definiert ist, werden nur methoden genannt die auch search benutzen
      {
        
        #found=codetools::findGlobals(match.fun(data[["level.rsi"]])) #gibt die Abh?ngigkeiten
        xref=as.vector(na.omit(unlist(lapply(funcs, FUN=function(f) 
        { 
          found = codetools::findGlobals(match.fun(data[[f]])) #gibt die Abh?ngigkeiten
          if (search %in% found)
            return(f)
          else
            return(NA)
        }))))
        funcs = xref
      }
      
      variables= as.vector(na.omit(unlist( lapply(ls(data), FUN=function(x,all=funcs)
      {
        if (x %in% all ) 
          return(NA)  
        else 
          return(x)
      }))))
      
      if (len(variables)>0)
      {
        print("##############> variables ###########")
        print(variables)
      }
      
      #    if (rfile =="MLIB/Now2.R")
      #      browser(MP("xx1"))
      
      #if (len(variables)==0)
      #    variables = "no variables"
      
      #browser(MP("xx"))
      #formals(fun=sys.function(sys.parent(2))) #gibt die Parameter zur aktuellen Methode
      #MMxx1
      if (len(funcs)>0)
      {
        funcArgs = sapply (funcs,FUN=function(funx) {sprintf("%s(%s)",funx,toString(attributes(formals(data[[funx]]))$names))},simplify = TRUE)
        
        ret=as.data.frame(rbind(ret,c(sprintf("# %s #",rfile),"######################"),cbind(file,funcArgs)))
      }
      if (len(variables)>0 && search=="")
      {
        #browser(MP("A1"))
        head(ret)
        colnames(ret)
        V=data.frame(file=variables,funcArgs=" +++++++++++++++++ ")
        ret=rbind(ret,V)
      }
      
      #print(ret)  
      mP("##############################################################")
    }
    if (search != "")
    { xref=ret; View(xref)}
    else
      View(ret)  
  }
  else
  {
    data = new.env()
    source(local=data, file=file)
    
    
    print(ls.str(mode = "function",envi=data))
    funcs= c(lsf.str(envi=data))
    
    #browser()
    #function(funx) sprintf("%s(%s)",funx,toString(attributes(formals(funx))$names))
    
    funcArgs = sapply (funcs,FUN=function(funx) {sprintf("%s(%s)",funx,toString(attributes(formals(data[[funx]]))$names))},simplify = TRUE)
    
    ret=as.data.frame(rbind(c("######################","####################################"),cbind(file,funcArgs)))
    #print(ret)  
    mP("##############################################################")
    View(ret)  
  }
  mP(info=T,"write.table list_R_functions.csv")
  write.table(ret,"list_R_functions.csv",sep=";",row.names=F)  
  
  return(ret)
  
}


list_R_functions_At_Dir<-function(
  dataPath="SysInvestor/SIT-master/R",
  dirName = "SysInvestor")
{
  A<-NULL#data.frame(spl("rFile,Args"))
  for (rfile in 
       dir(path = dataPath, pattern = glob2rx("*.r"), all.files = T,
           full.names = F, recursive = F,
           ignore.case = FALSE, include.dirs = FALSE)
  )
  {   
    
    fname=sprintf("%s/%s",dataPath,rfile)
    
    mP(fname)
    a1=list_R_functions(file=fname)
    
    if (is.null(A))
      A<-a1
    else
      A <- rbind(A,a1)
    
  }
  #browser()
  write.table(A,file=sprintf("%s.csv",dirName),sep=";",col.names=c("Funcs","rFile;Args"),quote=FALSE, row.names=FALSE)
  
  print(sprintf("%s.csv",dirName))
}

if(F)
{
  sapply(parse("InputConfig_Portfolio_TD.R"),FUN=function(x){  str_extract(toString(x),glob2rx("*.function"))})
  
  a1=list_R_functions(file="InputConfig_Portfolio_TD.R")
  write.table(a1,file="Funcs.csv",sep=";",col.names=c("rFile","Args"),quote=FALSE)
  
  
  list_R_functions_At_Dir("SysInvestor/SIT-master/R","Info/SysInvestorALL")  #MMA
}


## 1) with traditional 'graphics' package:
showCols1 <- function(bg = "gray", cex = 0.75, srt = 30) {
  m <- ceiling(sqrt(n <- length(cl <- colors())))
  length(cl) <- m*m; cm <- matrix(cl, m)
  ##
  require("graphics")
  op <- par(mar=rep(0,4), ann=FALSE, bg = bg); on.exit(par(op))
  plot(1:m,1:m, type="n", axes=FALSE)
  text(col(cm), rev(row(cm)), cm,  col = cl, cex=cex, srt=srt)
}
#showCols1()

## 2) with 'grid' package:
showCols2 <- function(bg = "grey", cex = 0.75, rot = 30) {
  m <- ceiling(sqrt(n <- length(cl <- colors())))
  length(cl) <- m*m; cm <- matrix(cl, m)
  ##
  require("grid")
  grid.newpage(); vp <- viewport(w = .92, h = .92)
  grid.rect(gp=gpar(fill=bg))
  grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
            vp=vp, gp=gpar(cex = cex, col = cm))
}
#showCols2()

###########################################
#siehe
#MM!  xtsExtraTest
#http://www.r-bloggers.com/plot-xts-is-wonderful/
###########################################

XtsGuvPlot<-function(data.to.plot = NULL,main="Performance Summary")
{
  
  #for a little more advanced application we can start to do charts.PerformanceSummary style plot.xts 
  cumulreturn.panel  <- function(...) {
    mtext("Cumulative Return", side=1, adj=1, line=-3)
    default.panel(...)
    abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
    abline(h=0, col="black")
  }
  
  es.panel <- function(index,x,...) {
    mtext("Expected Shortfall", side=1, adj=1, line=-3) 
    default.panel(index,x,...)
    #silly to do this but if we wanted just certain points like every 4 months we could do something like this
    #default.panel(index[seq(1,NROW(index),by=4)],coredata(x[seq(1,NROW(index),by=4)]),...)
    #abline(h=0, col="black")
    abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
    abline(h=par("yaxp")[1], col="black")
  }
  
  drawdown.panel <-  function(index,x,...) {  
    mtext("Drawdown", side=1, adj=1, line=-2) 
    default.panel(index,x,...)
    #silly to do this but if we wanted just certain points like every 4 months we could do something like this
    #default.panel(index[seq(1,NROW(index),by=4)],coredata(x[seq(1,NROW(index),by=4)]),...)
    #abline(h=0, col="black")
    abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
    abline(h=par("usr")[3], col="black")
  }
  
  #get some risk measurements to add for Performance Summary style plot
  Risk.drawdown <- Drawdowns(R)
  
  Risk.es <- rollapplyr(R,FUN="ES",width=36,p=0.95,na.pad=TRUE)
  #take care of NA with 0 at beginning and interpolation at end
  Risk.es <- apply(Risk.es,MARGIN=2,FUN=na.fill,fill=c(0,"extend"))
  #something wrong with returned value from apply.rolling so indexes don't work properly
  data.to.plot <- as.xts(cbind(coredata(Return.cumulative),Risk.es,coredata(Risk.drawdown)),order.by=index(edhec))
  
  #png("chartsPerformanceSummary.png",width=640,height=600,units="px")
  plot.xts(data.to.plot,
           lwd = c(2,1,1), #do this to show how arguments are recycled
           col = brewer.pal(n=9,"PuBu")[c(8,4,6)],
           auto.grid = FALSE, #usually auto.grid works just fine but turn off for example purposes
           las = 1,yax.loc = "right",  # yax.loc could also be flip or left in this case
           screens = c(1,1,1,2,2,2,3,3,3),  #3 series for each so first 3 in panel 1 second 3 in panel 2 and last 3 in panel 3
           layout.screens = c(1,1,2,3), #panel 1 take up first 2 of 4 so 50% and panels 2 and 3 each 25%
           bty = "n", 
           panel = c(cumulreturn.panel,es.panel,drawdown.panel), #c(first.panel,"auto"), #panel cycles through by panel rather than each series
           ylab = NA, major.format = "%b %Y", minor.ticks = FALSE,
           legend.loc = c("topleft",NA,NA), auto.legend = TRUE,
           legend.pars = list(bty = "n", horiz=TRUE),  #make legend box transparent
           cex.axis = 0.9, 
           main = NA)
  
  title(main = "Performance Summary of EDHEC Indexes", adj = 0, outer = TRUE, line = -1.5)
  
}


assert <- function (condition, ...) { #MMA
  mc <- match.call()
  if (!is.logical(eval(condition)) || ! all(condition)) {
    cat("Assertion failled:", 
        deparse(mc[[2]]), "is not TRUE\n")
    ll <- list(...)
    for (i in seq(along=ll)) {
      cat("  ", deparse(mc[[i+2]]), ": ", ll[[i]], "\n", sep="")
    }
    ca <- sys.calls()
    cat(paste(length(ca):1, ": ", rev(ca), sep="", collapse="\n"), "\n")
    cat("BROWSER (type 'c' to quit):\n")
    browser()
    stop(paste(deparse(mc[[2]]), "is not TRUE"), call.=FALSE)
  }
}
#
#list_R_functions("InputConfig_Portfolio_TD.R")
#listTicks()

###################################


#x="hallo <meta name=\"keywords\" content=\"db x-tr.SMI ETF Inhaber-Anteile 1D (WKN DBX1SM, ISIN LU0274221281), Fonds\">"

#Extrahiert aus einem String x (der l?nge  nchar(x)  den Teil der  zwischen word1 und word2 steht)
#Vorsicht bei Sonderzeichen wie (,/ oder ... )

StrBetween<-function(x,word1,word2) #MMA
{
  rest = ifelse(word2=="","",sprintf("%s",word2))
  pat=sprintf("%s(.*?)%s",word1,rest)
  if (word2=="") #leftOf
    pat=sprintf("%s(.*)",word1)
  if (word1 =="") #rightOf
    pat=sprintf("(.*)%s",word2)
  #  print (pat)
  res = ""
  m=first(regexec(pat,x))
  M=regmatches(x, m)
  #browser()
  res = trim(unlist(M)[2])
  print("###")
  print(res)
  return(res)
}

if (F)  #Tests zu StrBetween  #MMA
{
  x="sadfasfasf<meta name=\"keywords\" content=\"db x-tr.SMI ETF Inhaber-Anteile 1D (WKN DBX1SM, ISIN LU0274221281), Fonds\"> </head> <body >"
  
  res=StrBetween(x,"<meta name=\\\"keywords\\\" content=\\\"","\\\">")  
  res=StrBetween(x,"<meta name=\\\"keywords\\\" content=\\\"","")  
  res=StrBetween(x,"","\\\">") 
  
}


leftOf<-function(x,what)
{
  res=StrBetween(x,"",what)
  return (res)
}


rightOf<-function(x,what)
{
  res=StrBetween(x,what,"")
  return (res)
}

ZahlBetween<-function(x,word1,word2)
{
  return(as.numeric ( StrBetween(x,word1,word2)))
  
  
  library("stringr")
  pat="chart.m\\?secu=(\\d+)&amp"
  rest = ifelse(word2=="","",sprintf("%s",word2))
  pat=sprintf("%s.*%s",word1,rest)
  pat=sprintf("%s(\\d+)%s",word1,rest)
  x2= str_extract(x,pat)
  pat="(\\d+)"
  x3 = str_extract(x,pat)
  print(x3)
  return(x3)
  
}

if (F)
  prettyWpName("^'/R'B^S_Market&amp;_Access_FTSE.\tJSE_Afr  ica_;Top,_40_ETF'")


##################################
#versucht aus nem string einen wp-tauglichen namen zu machen, der auch
#als filename f?r csv-files taugt.
##normalisier ihn indem du . und blank durch "_" ersetzt
##################################
prettyWpName <- function (wpName0) 
{
  
  if (is.na(wpName0) || is.null(wpName0) )
    return (" ")
  
  if (nchar(wpName0)>250) 
    wpName0 =substr(wpName0,1,250)
  wpName0 = trim(wpName0)
  if   (substr(wpName0,1,1)=="^") 
    wpName0 <- sub( "^", "",wpName0,fixed=TRUE)
  if   (substr(wpName0,1,1)==".")  #leading . entfernen
  {
    wpName0 <- substr(wpName0,2,250)
    
  }
  wpName=wpName0
  
  wpName <- gsub("&amp;", "u",wpName0,fixed=TRUE)
  wpName <- gsub( "Kurs", "",wpName,fixed=TRUE)
  wpName <- gsub( "Chart", "",wpName,fixed=TRUE)
  wpName <- gsub( "Nachrichten", "",wpName,fixed=TRUE)
  
  wpName <- gsub( "#", "",wpName,fixed=TRUE)
  wpName <- gsub( ".", "_",wpName,fixed=TRUE)
  wpName <- gsub( ";", "_",wpName,fixed=TRUE)
  wpName <- gsub( ",", "_",wpName,fixed=TRUE)
  wpName <- gsub( "'", "",wpName,fixed=TRUE)
  wpName <- gsub( " ", "_",wpName,fixed=TRUE)
  wpName <- gsub( "___", "_",wpName,fixed=TRUE)
  wpName <- gsub( "__", "_",wpName,fixed=TRUE)
  wpName <- gsub( "\\", "_",wpName,fixed=TRUE)
  wpName <- gsub( "/", "_",wpName,fixed=TRUE)
  wpName <- gsub( "<", "",wpName,fixed=TRUE)
  wpName <- gsub( ">", "",wpName,fixed=TRUE)
  wpName <- gsub( "?", "",wpName,fixed=TRUE)
  wpName <- gsub( "\t", "",wpName,fixed=TRUE)
  wpName <- gsub( "\r", "",wpName,fixed=TRUE)
  wpName <- gsub( "\n", "",wpName,fixed=TRUE)
  wpName <- gsub( "  ", "_",wpName,fixed=TRUE)
  wpName <- gsub( "   ", "_",wpName,fixed=TRUE)
  wpName <- gsub( "&", "u",wpName,fixed=TRUE)
  wpName <- trim(wpName)
  wpName <- gsub( "-", "_",wpName,fixed=TRUE)
  
  #browser(mP("pretty %s",wpName))
  wpName <-toupper(wpName) #MM?
  return(wpName)
}

#...............................

prettyWpName2 <- function (wpName0) 
{
  if (is.na(wpName0) || is.null(wpName0) )
    return (NA)
  wpName = trim(wpName0)
  if (wpName =="")
    return (NA)
  wpName <- gsub( " ", "",wpName,fixed=TRUE)
  wpName <- gsub( "-", "",wpName,fixed=TRUE)
  trim(wpName)
}


######################################################################################

#norm_Win(4)
#longIndikator1= longIndikator[HighLows(longIndikator,maxdd=20,visual=T)$lows]
#longIndikator2= longIndikator1[HighLows(longIndikator1,maxdd=20,visual=T)$lows]
#longIndikator3= longIndikator2[HighLows(longIndikator2,maxdd=20,visual=T)$lows]

#mPlot(scaleTo(longIndikator,c(1,0)))
#####################################################################################



HighLows <-function(prices, maxdd=5,visual = F,percent=T)  #MMA
{
  prices=scaleTo(na.omit(prices[,1]),c(1,0)) #ohne Skalierung auf positive Werte klappt das alles nicht so gut
  if (maxdd==0)
    zz=prices
  else
    zz=na.omit(ZigZag(na.omit(prices),change=maxdd,percent=percent))#,percent=F))
  if (index(last(zz)) < index(last(prices))) #letzten Punkt noch anh?ngen
    zz = append(zz,last(prices))
  
  ZZ1=zz
  if (F)
  {
    #zz=prices
    #es liegt ein offset zwischen  zz und prices !!!!!!
    #wie viele leading NA gibts in zz ??
    #browser()
    block=merge(zz,prices,all=T)[,1]
    nac=as.vector(iif(is.na(block),1,NA))
    enc <-rle(nac)
    offset = enc$lengths[1]
  }
  
  if ( visual)
  {
    plot(prices)
    lines(zz,col="red")
    # browser()  
  }
  
  f=""
  sd = sign(diff(zz))[f]
  rS=runSum(sd,2)
  rS=lag(rS,-1)
  
  peaks <- prices[ as.Date(index(rS[rS==0]))]
  highx = peaks[peaks > lag(peaks,1)]
  lowx = peaks[peaks < lag(peaks,1)]
  
  if (visual)
  {
    plot(prices[f])
    points(peaks[f])
    lines(zz[f],col="red")
    points(highx,col="blue",lwd=3)
    points(lowx,col="red",lwd=3)
  }
  
  highx=as.Date(index(highx))
  lowx=as.Date(index(lowx))
  peaks=as.Date(index(peaks))
  return(list(highs = highx,lows = lowx,hl=peaks,zz=ZZ1 ))
  
  ##############################################################################################
  
  peaks <- findpeaks(as.vector(zz))  
  #price, x-lage des peaks,
  Peaks <-merge(zz, zz[peaks[,2]],zz[peaks[,3]],zz[peaks[,4]]) 
  
  browser(mP("HL"))
  highx = peaks[,2]; Highs=zz[peaks[,2]]
  lowx = na.omit(iif(!is.na(peaks[,3]),peaks[,3], peaks[,4]))
  
  Lows=na.omit(iif(!is.na(Peaks[,3]),Peaks[,3], Peaks[,4]))
  
  #lines(Highs,type="p")
  zzPoints=na.omit(iif(!is.na(Peaks[,2]),Peaks[,2], Peaks[,3]))
  if (index(last(zzPoints)) < index(last(prices))) #letzten Punkt noch anh?ngen
    zzPoints = append(zzPoints,last(prices))
  if (index(first(zzPoints)) > index(first(prices))) #letzten Punkt noch anh?ngen
    zzPoints = append(first(prices),zzPoints)
  
  
  #points(zzPoints,col="green",lwd=2)
  if (visual)
  {
    frame="2009"
    frame=""
    if (F)
    {
      
      plot(prices[frame],type="h")
      
      lines(zzPoints[frame],type="l",col="green",lwd=2)
      
      
      #lines(zz,col="red")
      points(Highs[frame],col="magenta",lwd=1)
      points(Lows[frame],col="blue",lwd=1)
      
      #browser()
    }
    new_Win(2)
    par(mfrow=c(2,1))
    plot(prices[frame],type="l")
    lines(zz[frame],type="l",col="green")    
    points(prices[highx+offset,],col="red")  #bug
    points(prices[lowx+offset,],col="blue") 
    
    head(zz)
    head((diff(zz)))
    #mittlere wegstrecke == kanalbreite
    purePlot(merge(scaleTo(SMA(abs(diff(zz)),40),c(0,1)))[frame],prices[frame])
    browser()
  }
  #markiere die higss- und lows- des zigzag in unterschiedlichen farben
  highx = highx+offset    #es liegt ein offset zwischen  zz und prices !
  lowx = lowx+offset
  hl=sort(append(lowx,highx))  
  
  return(list(highs = highx,lows = lowx,hl=hl+1 ,zz=ZZ1))  
}

if (F)
  hl=HighLows(p,maxdd=15,visual=T)

########################################################################################


####################################################
#jedes ?bergebene xts wird in einem eigenen Fenster - die gestapelt sind-
#mit eigener y-Achse gedruckt

#mPlots(hsm,cl,frame="2008::",titel= "Ernte")

#####################################################

mPlots<-function(...,title="",main="",frame="",show.colnames=T)   #MMA  wie xyplots  nur netter, siehe xyplot(mmerge(hsm,cl),grid=T, main="mist")
{
  #browser()
  #args=list(mmerge(...))
  args =list(...)
  
  #args=mmerge(args)
  if (len(args)==1)
  {
    p = args[[1]]
    
    Args= list()
    for(c in colnames(p))
    {
      Args = append(Args,list(p[,c]))
      
    }
    
  }
  else
    Args=args
  
  args = Args
  norm_Win(min(5,len(args)))
  
  first_ = args[[1]]
  last_ =  args[[len(args)]]
  last_ = last_[frame]
  
  #browser()
  args = args[-len(args)] #alle ausser dem letzten
  
  first=T
  title_=""
  
  for(x in args)
    
  {
    r1 = colnames(x)
    if (is.null(colnames(x)))
    {  r1=sapply(1:ncol(x),function(coli) cn=sprintf("%s_%d",deparse(substitute(x)),coli))        
       colnames(x)=r1
    }
    coln=ifelse(show.colnames, r1,"")
    
    x=na.omit(x[frame])
    
    if (first)
      if (title != "")
        title_ = title
    else
      title_ =  deparse(substitute(...))
    if (first)
      plota(merge(x,first_)[,1],ylim=range(na.omit(x)),main=title_,plotX=F,LeftMargin=1) 
    else
      plota(merge(x,first_)[,1],ylim=range(na.omit(x)),plotX=F,LeftMargin=1) 
    
    if (show.colnames)
      plota.legend(labels=coln)
    
    for(ci in  1:ncol(x))
      plota.lines(x[,ci],col=ci+1)
    
    if (min(x,na.rm = T)<0 && max(x,na.rm=T)>0)
      abline(h=0,col= "blue")
    #lines(x)
    first =F
    
  }
  r1 = colnames(last_)
  if (is.null(r1))
  {  r1=sapply(1:ncol(last_),function(coli) cn=sprintf("%s_%d",deparse(substitute(last_)),coli))        
     colnames(last_)=r1
  }
  coln=ifelse(show.colnames, r1,"")
  
  coln=ifelse(show.colnames, colnames(last_),"")
  
  plota(merge(last_,first_)[,1],ylim=range(na.omit(last_)),LeftMargin=1)
  if (show.colnames)
    plota.legend(labels=coln)
  for(ci in  1:ncol(last_))
    plota.lines(last_[,ci],col=ci+1)
  layout(1)  
}
mPlotS<-function(...,title="",frame="")   #MMA  wie xyplots  nur netter, siehe xyplot(mmerge(hsm,cl),grid=T, main="mist")
{
  args=list(...)
  #args=mmerge(args)
  if (len(args)==1)
  {
    p = args[[1]]
    
    Args= list()
    for(c in colnames(p))
    {
      Args = append(Args,list(p[,c]))
      
    }
  }
  
  args = Args
  #norm_Win(len(args))
  
  
  last_ =  args[[len(args)]]
  last_ = last_[frame]
  
  #browser()
  args = args[-len(args)]
  
  first=T
  title_=""
  
  for(x in args)
  {
    x=na.omit(x[frame])
    if (first)
      if (title != "")
        title_ = title
    else
      title_ =  deparse(substitute(...))
    
    if (first)
      plota(x,ylim=range(x,na.rm=T),main=title_,plotX=T,LeftMargin=1) 
    #else
    #  plota(x,ylim=range(x),plotX=F,LeftMargin=1) 
    
    lines(x)
    first =F
    
  }    
  #plota(last_,ylim=range(last_),LeftMargin=1)
  plota.lines(last_,col="black")
  
}

#seq(5,3)
####################################################
####RunLengthEnc
#es kommt ein vektor rein (z.B. mit 0 und eins - Trains - aus HighLows())
#Heraus kommt ein Vektor der die Train-L?ngen gez?hlt hat und im S1 die 
#Trainl?nge jeweils in den Train schreibt.
#####################################################
runLengthEnc<-function(s1)   #MMA
{
  #s1=ifelse(sign(highs-lag(highs,1))<0,0,1)
  sig=as.vector(s1)  #segmente positiver und negativer steigung
  enc <-rle(sig)
  enc.endidx <- cumsum(enc$lengths)  #ending indices
  S1 = s1
  #trage die  runl?ngen in S1 
  for(i in seq(1, len(enc.endidx)))
    for(j in seq(1, enc$lengths[i]))  ### jedes bit des trains durch die trainl?nge ersetzen 
      S1[enc.endidx[i]+j-1,] = enc$lengths[i] -j+1   #MM_TODO  runLengthEnc - testen und aufpassen dass man keine Info's an die Zukunft weiterreicht ... :-)
  
  return(S1)
}



runLengthEnc2<-function(s1)   #MMA
{
  #s1=ifelse(sign(highs-lag(highs,1))<0,0,1)
  sig=as.vector(s1)  #segmente positiver und negativer steigung
  enc <-rle(sig)
  enc.endidx <- cumsum(enc$lengths)  #ending indices
  enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(s1))  # starting indices
  
  S1 = s1
  S1[]=0
  x=1
  #browser()
  #trage die  runl?ngen in S1 
  for(i in seq(1, length(enc$lengths)))
  {
    lenk=1
    for(j in seq(1, enc$lengths[i]))  ### jedes bit des trains durch die trainl?nge ersetzen 
    {
      #    browser()
      S1[x]=lenk*sign(s1[x])
      lenk=lenk+1
      x=x+1 
    }
  }
  return(S1)
}
runLengthEnc3<-function(s1)   #MMA
{
  #s1=ifelse(sign(highs-lag(highs,1))<0,0,1)
  sig=as.vector(s1)  #segmente positiver und negativer steigung
  enc <-rle(sig)
  enc.endidx <- cumsum(enc$lengths)  #ending indices
  enc.startidx <- c(0, enc.endidx[1:(length(enc.endidx)-1)],length(s1))  # starting indices
  
  S1 = s1
  S1[]=0
  x=1
  #browser()
  #trage die  runl?ngen in S1 
  for(i in seq(1, length(enc$lengths)))
  {
    lenk=1
    for(j in seq(1, enc$lengths[i]))  ### jedes bit des trains durch die trainl?nge ersetzen 
    {
      #    browser()
      S1[x]=enc$lengths[i]*sign(s1[x])
      lenk=lenk+1
      x=x+1 
    }
  }
  return(S1)
}
# 252 - days, 52 - weeks, 26 - biweeks, 12-months, 6,4,3,2,1
#rasterOn(151)

######################################################################
#nimmt sich den zu der liste der erlaubten werte am besten passenden zu x
######################################################################
rasterOn = function(x,possible.values = c(252,52,26,13,12,6,4,3,2,1)) 
{
  index = which.min(abs( x - possible.values ))
  round( possible.values[index] )
}


####################################################
####gegen Ausreisser, outlier  #MMA
## nimm lieber  Return.clean(ret)  aus der PerformanceAnalytics-lib

#PeformanceAnalytics:
#   clean.boudt(R, alpha = 0.01, trim = 0.001)
#http://finzi.psych.upenn.edu/R/library/timeSeries/html/base-subset.html
#geht auch mit timeSeries


#http://braverock.com/brian/R/PerformanceAnalytics/html/Return.clean.html
#Kalman:  tsSmooth(),
#https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/pracma/R/.Rapp.history?revision=5&root=optimist&pathrev=11

if (F)
{
  #ungfiltert
  ret = mROC(prices)
  purePlot(mRendite(ret))
  #ausrei?er bereinigung
  ret = Return.clean(ret, method = c("none", "boudt", "geltner")[2])
  price = mRendite(ret)
  purePlot(price)
  
  
  
  
}

#####################################################

#http://www.r-bloggers.com/moving-window-filters-and-the-pracma-package/
#http://www.r-bloggers.com/finding-outliers-in-numerical-data/
# ExploringData package is FindOutliers, described in this post.  


HampelFilter <- function (x, k,t0=3){ #MMX
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  for (i in (k + 1):(n - k)) {
    x0 <- median(x[(i - k):(i + k)])
    S0 <- L * median(abs(x[(i - k):(i + k)] - x0))
    if (abs(x[i] - x0) > t0 * S0) {
      y[i] <- x0
      ind <- c(ind, i)
    }
  }
  list(y = y, ind = ind)
}

##################################

###################################################################
# ?hnlich wie bt.apply.matrix() wird  die Funktion f auf alle Spalten
#der Matrix angewandt.  Sie gibt aber in der Regel eine ganze Liste
#von Ergebnissen wieder- Das Ergebnis  slot wird ausgew?hlt und in die
#Ergebnis xts-matrix gebaut.
###################################################################

mt.apply.matrix_<-function(prices,f,slot="",...)
{
  Prices = NULL  
  fun=match.fun(f)
  for(col in colnames(prices))
  {
    x=fun(prices[,col,drop=FALSE],...)
    
    if (slot!="")
      x=x[[slot]]
    if (is.null(Prices))
      Prices=x
    else
      Prices =cbind(Prices,x)
  }
  colnames(Prices) = colnames(prices)
  return (Prices)
}

mt.apply.matrix<-cmpfun(mt.apply.matrix_) #compilier die Funktion


##########################################################
#m.Run() ein super leichtes PM-System: ret=  prices*Signal f?r viele #MMA
#Aktienin den Prices+Signal-Spalten gleichzeitig
##########################################################
m.Run_<-function(prices,signal) 
{
  signal=lag(signal)   #!!!!
  Prices = NULL 
  prices = mROC(prices)
  colnames(signal)=colnames(prices)
  #fun=match.fun(f)
  
  for(col in colnames(prices))
  {
    x=prices[,col,drop=FALSE] * signal[,col,drop=FALSE]
    if (is.null(Prices))
      Prices=x
    else
      Prices =cbind(Prices,x)
  }
  colnames(Prices) = colnames(prices)
  return (Prices)
}

m.Run<-cmpfun(m.Run_) #compilier die Funktion


##########################################################
# l?sche xts Spalten die leer sind
##########################################################
m.clean0_BAD<-function(prices)
{
  #fun=match.fun(f)
  #browser()
  Prices = prices
  Prices[] = bt.apply.matrix(coredata(prices), m.ifna.prev)  
  ci=0
  del = c()
  for(col in colnames(prices))
  {
    ci=ci+1
    x=prices[,col,drop=FALSE] 
    
    x[!is.finite(x)]<-0 #ersetze na durch 0
    
    if (nrow(x) == nrow(x[x==0])) #alles 0  
      del=c(del,ci)
  }
  print("m.clean0 del")
  
  if (is.null(del) || len(del)<1)
    return (Prices)
  
  return (Prices[,- (del)])
}



m.clean0<-function(series) #MMMM1
{
  #mP("m.clean0")
  #browser()
  res1=NULL
  for(sym in c(1:len(colnames(series))))
  {
    #browser()
    head(col)
    col=series[,sym];  col = col[!is.na(col)]
    #browser()
    if (len(col) >0)
    {
      goodcol=m.ifna.prev(series[,sym])
      if (is.null(res1))
        res1= goodcol
      else
        res1=cbind(res1,goodcol)
    }
  }
  #browser()
  return(res1)
}
######################################################################
######################################################################

## wie mP .. aber mit einem verpflichtenden Tastendruck
userStop<-function(sag = "", ...,pos="")
{
  cat("\n")
  mP(sag,...)
  cat("\n")
  #browser()
  if ( globalCAT == ""  ||  globalCAT ==pos)
  {
    mP("userStop %s: ####  press Return ####",pos)
    browser()
    #readline()
  }
  else 
    mP("userStop %s: ############# ",pos)
}
Stop<-function(sag = "", ...,pos="")
{
  cat("\n")
  mP(sag,...)
  cat("\n")
  traceback()
  sfStop();stop()
}
#userStop("%s alter","hallo",pos="alloc1")
######################################################################
### sehr einfache Qualit?tsmessung des ret .. hier auf SharpeRatio
## gemeinsam gemessen ?ber alle aktien (rowSums)
quality_<-function(ret,visual =F)
{
  
  if (visual)
  {
    print(KellyRatio(ret))
    print(table.AnnualizedReturns(ret))
    print(maxDrawdown(ret))
  }
  
  Tkosten =0.0008
  nt=as.numeric(numTrades(sign(ret))$allT) 
  Tkosten =Tkosten*nt
  # ich geh von einer slippage von einem Tag aus.
  # punkt genau wird man die Trendwechsel nie erwischen
  if (F) #with slippage ?
  {
    slipF = merge(ret,lag(ret,1 ))
    slipRet=slipF[as.Date(index(ret)),2]
    ret = ROC(slipRet,type="discrete")
  }
  #q=table.AnnualizedReturns(ret)[3,]
  
  #if (ncol(ret) > 1)
  #  q = rowSums(table.AnnualizedReturns(ret)[3,])
  # bt.ret = ROC(ret, type='discrete')
  bt.ret = ret
  bt.ret[1] = 0.000001
  bt.ret[!is.finite(bt.ret)]=0
  
  equity = cumprod(1 + bt.ret)
  
  q = last(equity)#/abs(compute.max.drawdown(ret))
  #  q=compute.sharpe(bt.ret)
  #q=compute.cagr(res) / abs(compute.max.drawdown(res))
  #q =q*100-nt*Tkosten
  
  # q=compute.sharpe(Ret)
  
  #compute.sharpe(mROC(prices))/compute.sharpe(ret)
  
  q=(q-Tkosten) #/ maxDrawdown(bt.ret)
  return (as.numeric(q))
}
quality<-cmpfun(quality_) #compilier das Teil


##########################################################################
# x ist ein DATUM-String,  Dax ist lange xts in der x steckt,
# xts2Index gibt dann die Zahl (1.. len(Dax)) der Position von x zur?ck
##########################################################################

xts2Index<-function(x,Dax)  #MMA
{
  #browser()
  x=as.Date(x)
  i1=which(index(Dax)==index(Dax[x]))
  if(len(i1) ==0)return(0)
  return(i1)
}
##########################################################################
## gib das Datum, dass um xshift zu x1 verschoben ist - im Dax.
## (das ist was anderes als x1+xshift - jedenfalls wenn der Dax L?cken hat)
##########################################################################
xtsAddDate<-function(x,Dax,xshift)
{
  i1=which(index(Dax)==index(Dax[x]))
  i2 = pmax(1,pmin(i1+xshift,len(Dax)))
  if (len(i1)==0|| len(i2)==0)return(0)
  return(i2)
}

##########################################################################
# ist die Spalte colnames des Xts  leer ?
##########################################################################

isEmptyCol<-function (Xts,colname)
{
  Col= Xts[,colname]
  Col = Col[!is.na(Col)]
  if (len(Col) <100) return(TRUE)
  e1 = (0==len(  Col[is.finite(Col)] ))
  e2 =last(cumsum(nchar(Col))==0)
  return (e1&&e2)
}
############################################################################


#########################################################################
# eine try - Variante die sich wie try bedient und auch durchl?uft - aber
#fehler und Warnungen wenigstens anzeigt
#########################################################################

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
      #browser()
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



#x=tryM( log( NULL ) )
#tryM(  1/0  )

#####################################################################################################
#die Steigungen im rollierenden Fenster- hier darf ein ganzes, breites xts-Objekt kommen . dank lm.fit()
#####################################################################################################

#####  ist wie length(x) bzw. dim(x)[1]
shape<-function(Y)
{
  if (is.vector(Y))
    return(len(Y))
  return(dim(Y)[1])
}

########################################################################################
print("########### load InputConfig_Portfolio_TD2.R")
if (F)
  list_R_functions('MLib/InputConfig_Portfolio_TD2.R')
