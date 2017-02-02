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
#hinten oder vorn was abschneiden

but.last <-function( vec)
{
  if (is.null(vec))return(NULL)
  last.i=length(vec)
  
  if (last.i>0)
    res=vec[-last.i]
}

but.first <-function( vec)
{
  if (is.null(vec))return(NULL)
  
  if (length(vec)>0)
    res=vec[-1]
}

if (F)
{
  print(but.last(c(1,2,3)))
  print(but.first(p))
}
########################################################################################

########################################################################################
#gibt einen xts mit den dtw-lead-lag- beziehunge zurück
#die Funktion ist das kernstück von dtw.r
####################################################################(####################
#M1
get.dtw.leads<-function(p1,p2,win=1, getXTS=T, use.HighLows=0,visual=T,normalize=T)
{
  # if (visual)    norm_Win(2)
  
  if (normalize)
  {
    if (visual)mchart((merge(p1,p2)),main="org")
    #damit täler und spitzen eher auf gleicher höhe liegen...
    #aber:  vieleicht kommt der wichtige lead ja aus dem sma- des normalize ??
    b=na.omit(normalize.mean.R(merge(p1,p2),50))
    if (visual)mchart(b,main="normalized")
    
    #b=na.omit(normalize.mean.sd.R(mNorm2(merge(p1,p2)),200))
    p1=b[,1];p2=b[,2]
  }
  
  if (getXTS)
  {
    if (use.HighLows > 0)   #leider kommt dtw nicht NA zurecht 
    {
      p1.hl=HighLows(p1,maxdd=use.HighLows,visual=visual)
      p2.hl=HighLows(p2,maxdd=use.HighLows,visual=visual)
      if (F)
      {
        #nur die Ecken stehen lassen
        p1.eck=p1;p1.eck[]=NA; p1.eck[p1.hl$hl] = p1[p1.hl$hl]
        p2.eck=p2;p2.eck[]=NA; p2.eck[p2.hl$hl] = p2[p2.hl$hl]
        p2=p2.eck; p1 = p1.eck
      }
      
      if (F)
      {
        #.... geh in die zz-Darstellung
        p1=p1.hl$zz; p2 = p2.hl$zz
        # mchart(merge(p1,p1.hl$zz,p2, p2 = p2.hl$zz))
      }
      if (T)
      {
        #ecken hervorheben
        #dtw will bevorzugt werte matchen die auf gleichem level liegen - heb also die wichtigen ecken über das Umgebungsniveau
        #   p1[p1.hl$highs] = p1[p1.hl$highs]+1; p1[p1.hl$lows] = p1[p1.hl$lows]-1
        #    p2[p2.hl$highs] = p2[p2.hl$highs]+1; p2[p2.hl$lows] = p2[p2.hl$lows]-1
        
        m=2;s=1
        p1[p1.hl$highs] = p1[p1.hl$highs]*m+s; p1[p1.hl$lows] = p1[p1.hl$lows]*m+s
        p2[p2.hl$highs] = p2[p2.hl$highs]*m-s; p2[p2.hl$lows] = p2[p2.hl$lows]*m-s
        
      }
      
    }
    
    b=na.omit(merge(p1,p2))+2  # dtw erlaubt keine NA
    #purePlot(b)
    #b=mNorm(b); b=last(b,200)
    mchart(b,main="nach prepro")
    #NA durch min ersetzen
    #b=bt.apply.matrix(b,function(col) iif(is.na(col),min(col,na.rm=T),col))  #dtw erlaubt keine NA
    #purePlot(b)
    #head(b)
    
  }
  else
  {
    query=p1
    template=p2
  }
  #datenformat für dtw - keine na erlaubt !!
  query=coredata(b[,1])
  template=coredata(b[,2])
  
  
  #default
  alignment <- dtw(query, template, keep=T)
  
  #alignment <- dtw(query, template, keep=T,step.pattern=asymmetric) #by asummetric sollte zum schluss gemittelt wedren
  #alignment <- dtw(query, template, keep=T)
  #besser:  (am rechten enden)
  #  alignment <- dtw(query, template, method = "Manhatten",keep=T,step.pattern=asymmetric,open.begin=T,window.type="sakoechiba",window.size=600)
  
  #  alignment <- dtw(query, template, method = "Euclidean",keep=T,step.pattern=asymmetric,open.begin=T,window.type="sakoechiba",window.size=600)
  #alignment <- dtw(query,template,step.pattern=asymmetric,keep=T)#,open.end = TRUE
  dtwPlotTwoWay(alignment) ;alignment$dist
  
  #########
  ##
  ## Subsetting allows warping and unwarping of
  ## timeseries according to the warping curve. 
  ## See first example below.
  ##
  
  if (F)
  {
    ## Most useful: plot the warped query along with reference 
    reference=template
    plot(reference)
    lines(query[alignment$index1]~alignment$index2,col="blue")
    
    ## Plot the (unwarped) query and the inverse-warped reference
    plot(query,type="l",col="blue")
    points(reference[alignment$index2]~alignment$index1)
  }  
  
  #  alignment$index1
  #  alignment$index2
  
  leads=1:max(alignment$index1); leads[]=0
  for(i in alignment$index1) #i ist der realindex
  {
    #diff=0
    #for (k  in max(1, i-win+1): i)
    #  diff = diff+ (k-alignment$index1[k])     
    #leads[i]=as.integer(diff/win)
    
    leads[i] =(i-alignment$index1[i])
  }  
  leads
  
  leads2=1:max(alignment$index2); leads2[]=0
  for(i in alignment$index2)
  {
    #diff=0
    #for (k  in max(1, i-win+1): i)
    #  diff = diff+ (k-alignment$index2[k])     
    #leads2[i]=as.integer(diff/win)
    
    leads2[i] =(i-alignment$index2[i])
  }  
  leads2 
  
  #leads und lags kummulieren
  res=leads-leads2
  
  #zeiche die shifts
  res = xts(res,index(b))
  #res=SMA(res,5)  #wichtig by asymertric
  plot(res,main=">0 means: p1 is lead of p2");abline(h=0,col="red")
  
  #dtwPlotTwoWay(alignment)
  norm_Win(1)
  dtwPlotTwoWay(alignment)
  
  list(leads=res, dist=alignment$dist)  
}

############################################################################
#irgednwas klapapt nicht
get.Leads.bug<-function(p1,p2,wlen=50)
{
  get.leads<-function(b)
  {
    P1=b[,1];P2=b[,2]
    HL=HotLags2.cc(xtsPrice2=P1,xtsPrice=P2, visual=F,ci=0.9)
    if (len(HL$lag)>1)
    {
      print(HL$lag)
      res=first(HL$lag)
    }
    res=nval(HL$lag)[1]
    if (is.na(res))res=0
    res
  }
  b=normalize.mean.R(merge(p1,p2),n=wlen)
  ret <- rollapplyr(b, width=wlen, FUN="get.leads", by.column=F, align = "right")
  plot(ret)
  ret
}
#.............................
#rollierenden anwendung vom withening

get.mLeads<-function(p1,p2,fra="",wlen=500)
{
  res=p1[fra,1];res[]=NA
  
  for (fi in c(1:len(index(p1[fra]))))
  {
    #fra ="2008-11-20";fi=1
    #fra ="2007-11-20";fi=1
    f=index(p1[fra])[fi]
    f1=as.Date(f)-wlen; f= as.character(f)
    P1=p1[sprintf("%s::%s",f1,f)]
    P2=p2[sprintf("%s::%s",f1,f)]
    HL=HotLags2.cc(xtsPrice2=P1,xtsPrice=P2, visual=F,ci=0.9)
    res[f]=HL$lag[1]
  }
  leads=res  
}
#.............................

if (F)
{
  fra="2007::2009"
  p1 = data$prices[,"SG2R"]
  p2 = data$prices[,"SV2R"]
  
  #p1=mROC(p1)  #brint nix
  #p2=lag(p1,6)  #Test
  
  # b=normalize.mean.R(merge(p1,p2),200);  p1=b[,1]; p2=b[,2]  #bring nix
  
  
  #welche der Branchen sind eher growth und welche eher value ?
  mleads = get.mLeads(p1,p2,fra,wlen=500)
  mPlots(merge(p1,p2)[fra],merge(mleads,0)[fra])
  
  mleads2 = get.mLeads(p2,p1,fra,wlen=50)
  mPlots(merge(p1,p2)[fra],merge(mleads,-mleads2,0)[fra])
  
  wlen=50
  mleads[mleads != 0 & mleads2 ==mleads]
  
  x=coredata(p1)
  y=coredata(p2)
  fit1 =lm(y~x,data=data.frame(x,y))
  AIC(fit1) 
  
  
  
  ### TODO ... ...
  #baue eine zeitreihe aus den jeweiligen leads ... 
  #baue eine regression darüber und speicher dir den fitted value...
  for (fi in c(1:len(index(lead[fra]))))
  {
    #fra ="2008-11-20";fi=1
    #fra ="2007-11-20";fi=1
    f=index(p1[fra])[fi]
    wlen=
      f1=as.Date(f)-wlen;
    P1=p1[sprintf("%s",f1)]
    res[f]=HL$lag[1]
  }
  
  
  x=coredata(p1)
  y=coredata(p2)
  fit1 =lm(y~x,data=data.frame(x,y))
  AIC(fit1) 
  
  
  amark("2007-10-10")
  amark("2007-07-09")
  fr="2007-07-09"
  merge(mleads,mleads2)[fr]  
}

#################################################################################################################

HotLags2.dtw <-function(p=dax,features=data$prices,use.HighLows=5,visual=F)
{
  #  browser(mP("hl"))
  RES = NULL
  today=as.Date(index(last(p)))
  for(fi in 1:ncol(features))  
  {
    feature=features[index(p),fi]
    all.leads=list()
    # HL=HotLags2.cc(p,feature,visual=visual)
    m.dtw = get.dtw.leads(p,feature,use.HighLows=use.HighLows)
    if(visual)
      print(m.dtw)
    lag= m.dtw$leads[today]
    sym=colnames(feature)
    print(sym)
    print(lag)
    
    if ( len(lag)>0 && lag[1]<0)  
    {
      lead.dat = lag(features[,fi],-lag[1])[index(p)]
      print("########################################")
      mP("%s %s ",colnames(p),colnames(feature) )
      print(lag)
      if (is.null(RES))
        RES=lead.dat
      else
        RES=merge(RES,lead.dat)
      all.leads[[sym]] = m.dtw$leads
    }
    if (!is.null(RES))
      print(head(RES))
  }
  if (visual)
    print(colnames(RES))
  RES = list(RES=RES, leads = all.leads)
}
#........................  #lead9
if (F) 
{
  x=  HotLags2.CC(p=data$prices[,1],features =data$prices)
  ls(x)
  
  x=  HotLags2.CC(p=data$prices[,1],features =data$prices[,"SUP500"])
  ls(x)
  
  x=  HotLags2.cc(data$prices[,1],data$prices[,"SUP500"])
  #---------------------
  fra="2007::2009"
  fra="2007::2008-11-07"
  fra="2010::2012"
  
  p1 = data$prices[fra,"SG2R"]
  p2 = data$prices[fra,"SV2R"]
  
  #p1=mROC(p1)  #brint nix
  #p2=lag(p1,6)  #Test
  
  # b=normalize.mean.R(merge(p1,p2),200);  p1=b[,1]; p2=b[,2]  #bring nix
  
  
  #welche der Branchen sind eher growth und welche eher value ?
  mleads = get.mLeads(p1,p2,fra,wlen=500)
  mPlots(merge(p1,p2)[fra],merge(mleads,0)[fra])
  
  mleads2 = get.mLeads(p2,p1,fra,wlen=50)
  mPlots(merge(p1,p2)[fra],merge(mleads,-mleads2,0)[fra])
  
  res =HotLags2.dtw(data$prices[fra,"SG2R"], data$prices[,"SV2R"], use.HighLows=5,visual=T)
  #TEST
  #res =HotLags2.dtw(data$prices[fra,"SG2R"], lag(data$prices[,"SG2R"],-6), use.HighLows=5,visual=T)
  res$leads[[1]]
  mchart(merge(data$prices[fra,"SG2R"],res$RES))
  
  
  #fit1=lm.check(data$prices[fra,"SG2R"], data$prices[fra,"SG2R"])
  #fit2= lm.check(data$prices[fra,"SG2R"], lag(data$prices[,"SG2R"],-60)[fra])
  
  #korrigiere den lead ...
  #res =HotLags2.dtw(data$prices[fra,"SG2R"], lag(data$prices[,"SG2R"],-60)[fra], use.HighLows=5,visual=T)
  #print(tail(res$leads[[1]]))
  #fit2.b= lm.check(data$prices[fra,"SG2R"], res$RES)
  
  
  
  fit3= lm.check(data$prices[fra,"SG2R"], data$prices[fra,"SV2R"])
  
  res =HotLags2.dtw(data$prices[fra,"SG2R"], data$prices[fra,"SV2R"], use.HighLows=0,visual=T)
  print(tail(res$leads[[1]]))
  #bauen ein lead-vektor der jeden tag nen anderen shift haben kann
  features=data$prices[fra,"SV2R"]
  lead=res$leads[[1]]; Lead = lead[]; Lead[] =NA;
  
  nrow(lead)==nrow(features)
  for (i in c(1:nrow(Lead)))
  {
    #  i=10
    Lead[i]=features[max(1,i+max(0,lead[i],na.rm=T))]  
  }
  
  res$RES
  fit3.b= lm.check(data$prices[fra,"SG2R"], res$RES) #9778
  fit3.b= lm.check(res$RES, data$prices[fra,"SG2R"] ) #10043
  
  fit3.b= lm.check(data$prices[fra,"SV2R"], res$RES)
  fit3.b= lm.check(res$RES, data$prices[fra,"SV2R"] )
  
  fit3.b= lm.check(data$prices[fra,"SG2R"], Lead)  #9854    #9836  bei use.HighLows=0
  fit3.b= lm.check(Lead, data$prices[fra,"SG2R"] ) #10142
  
  #vergleich mit get.mLeads
  p1=data$prices[fra,"SG2R"]
  features=data$prices[fra,"SV2R"]
  mleads = get.mLeads(p1,features,fra,wlen=500)
  
  Lead2 = mleads[]; Lead2[] =NA;
  for (i in c(1:nrow(Lead)))
  {
    #  i=10
    Lead2[i]=features[max(1,i+max(0,mleads[i],na.rm=T))]  
  }
  
  fit3.c= lm.check(data$prices[fra,"SG2R"], Lead2) #9815
  fit3.c= lm.check(Lead2, data$prices[fra,"SG2R"] )# 10087
  
  fit4= lm.check(data$prices[fra,"SG2R"], data$prices[fra,"SV2R"])  #9815
  
  AIC(fit3.b,fit3.c)
  
  AIC(fit1,fit2,fit2.b,fit3,fit4) #When comparing fitted objects, the smaller the AIC or BIC, the better the fit.
  #........................................................
  #beide lead - methoden bringen keinen echten aic-vorteil ...
  #liegen die wichtigen leads in den sma ?
  fra="2007::2009"
  fra="2007::20012-11-07"
  fra="2010::2012"
  
  fra=""
  p1 = data$prices[fra,"SG2R"]
  p2 = data$prices[fra,"SV2R"]
  
  
  
  rm1=runmean(p1,k=50,align="right")
  rm1=vec2Xts(rm1,p2=p1)
  mchart(merge(SMA(p1,50),p1,rm1))
  
  
  w=10
  mchart(merge(SMA(p1,w),p1))
  mchart(merge(SMA(p2,w),p2))
  mchart(merge(SMA(p1,w),p1,SMA(p2,w),p2))
  
  purePlot(merge(mNorm(SMA(mNorm(p1),w)),mNorm(SMA(mNorm(p2),w))))
  
  purePlot(bt.apply.matrix(mNorm(m.block(mROC(p1,50),mROC(p2,50))),xfun=EMA,n=10))
  #diees Vorverarbeitung resultiert in sehr viel stetigeren get.mLeads()
  b=bt.apply.matrix(mNorm(m.block(mROC(p1,50),mROC(p2,50))),xfun=EMA,n=10)
  P1=b[,1];P2=b[,2]
  mleads = get.mLeads(P1,P2,fra,wlen=500)
  mleads2 = get.mLeads(P2,P1,fra,wlen=500)
  
  purePlot(mleads,mleads2)
  
  
  Lead2 = mleads[]; Lead2[] =NA;
  for (i in c(1:nrow(Lead)))
  {
    #  i=10
    Lead2[i]=features[max(1,i+max(0,mleads[i],na.rm=T))]  
  }
  
  fit3.c= lm.check(data$prices[fra,"SG2R"], Lead2) #9815
  fit3.c= lm.check(Lead2, data$prices[fra,"SG2R"] )# 10087
  
  
  
  res =HotLags2.dtw(data$prices[fra,"SG2R"], data$prices[fra,"SV2R"], use.HighLows=0,visual=T)
  dleads=res$leads[[1]]
  purePlot(dleads,0)
  mPlots(merge(p1,p2), merge(dleads,0))
  
  purePlot(mleads,mleads2,dleads,0)
  Mleads=iif(abs(mleads)> abs(mleads2), mleads,-mleads2)
  purePlot(Mleads, dleads,0)
  
  use.HighLows=5
  p1.hl=HighLows(P1,maxdd=use.HighLows,visual=visual)
  p2.hl=HighLows(P2,maxdd=use.HighLows,visual=visual)
  p1.z=p1.hl$zz; p2.z = p2.hl$zz
  
  mchart(mNorm(merge(p1.z,p2.z)))
  
  b=mNorm(merge(p1.z,p2.z))
  P1=b[,1];P2=b[,2]
  res =HotLags2.dtw(P1,P2, use.HighLows=0,visual=T)
  mleads = get.mLeads(P1,P2,fra,wlen=500)
  
  mleads.org =get.mLeads(p1,p2,fra,wlen=500)
  
  purePlot(mleads,mleads.org)
  Lag=31
  #Aus AIC-Sicht immer noch keinerlei Vorteile !!!
  
  lm.check(tail(p1,200),tail(lag(p2,Lag),200))
  lm.check(tail(p2,200),tail(lag(p1,Lag),200))
  lm.check(tail(p2,200),tail(p1,200))
  lm.check(tail(p1,200),tail(p2,200))
  
}

#....................................................

m.block<-function(p1,p2)
{
  na.omit(merge(p1,p2))
}

#........................................................
#beurteile mit einem AIC ob zwei zeitreihen einander gut erklären (linear)

lm.check<-function(p1,p2)
{
  b=na.omit(merge(p1,p2)); p1=b[,1];p2=b[,2]
  x=coredata(p1)
  y=coredata(p2)
  
  fit =lm(y~x,data=data.frame(x,y))
  print(summary(fit))
  print ("#######>>")
  print(AIC(fit)) #When comparing fitted objects, the smaller the AIC or BIC, the better the fit.
  fit
  
}
#................................................................................


lm.check.aic_<-function(b,p.all=NULL,wlen=0,leads=NULL,search=NULL)
{
  P1=b[,1];P2=b[,2];ready=F;take.best=F;best.res=NULL;search.now=0
  today=as.Date(index(last(b)))
  
  while(!ready)
  {
    if (len(search)>0)
    {
      search.now=first(search)
      leads=vec2Xts(search.now,firstDate=today)
      search=but.first(search)
      take.best=T
    }
    
    if (!is.null(leads) && !is.null(p.all))
    {
      lead=nval(last(leads[sprintf("::%s",today)]))  #der lead von heute bzw. falls für heute keiner da ist der letzte vor heute
      #print(".")
      P2=tail(lag(p.all,abs(lead))[sprintf("::%s",today)],wlen)
      
    }
    
    x=coredata(P1)
    y=coredata(P2)
    
    fit =lm(y~x,data=data.frame(x,y))
    res=AIC(fit) #When comparing fitted objects, the smaller the AIC or BIC, the better the fit.
    
    if (take.best)
    {
      Res=xts(data.frame(aic=res,lead=search.now),today)
      if (is.null(best.res))
        best.res=Res
      else
        if (best.res[,"aic"] > res)
          best.res=Res
      
      res= best.res
    }
    #    print(res)
    if (len(search)==0)
      ready=T
  }
  res
}
lm.check.aic<-cmpfun(lm.check.aic_) #compilier die Funktion

###########################################################################################################################
#### Nach HotLags2.CC und  get.mLeads() (dtw) eine dritte methode um relevante lags zu suchen.
##  diesmal wird hard-core ein lm- modell mit verschieden lags gefitted und der aic gemessen
# der lag der den geringsten aic erzeugt gilt als passend

#gibt xts aus aic und lead zurück
###########################################################################################################################
roll.aic<-function(p1,p2,wlen=50,leads=NULL,search=NULL)
{
  b= na.omit(m.block(p1,p2))
  ret <- rollapplyr(b, width=wlen, FUN="lm.check.aic", by.column=F, align = "right",p.all=p2, wlen=wlen,leads=leads,search=search)
}

#.................................................................................................................

if (F)   #lead10
{
  fra="2010::2012"
  fra="2007::2010"
  
  fra=""
  p1 = data$prices[fra,"SG2R"]
  p2 = data$prices[fra,"SV2R"]
  
  #grundsätzlich ist der code richtig
  tleads=p1;tleads[]=-20
  res = roll.aic(p1,lag(p1,-20),wlen=300,leads=tleads);mean(res,na.rm=T)
  #aber mit den zuvor gefunden dtw und withening-leads kann er nichts anfangen
  
  res = roll.aic(p1,p2,wlen=33);mean(res,na.rm=T)
  
  res = roll.aic(p1,p2,wlen=300,leads=mleads.org);mean(res,na.rm=T)
  res = roll.aic(p1,p2,wlen=300,leads=mleads);mean(res,na.rm=T)
  
  #dann suchen wir doch gleich die leads von -31 bis 31 mit Hifle vn lm.check.aic
  
  #TEST  
  res = roll.aic(p1,lag(p1,-2),wlen=300,search=(-2:2));mean(res,na.rm=T)
  res
  #TEST
  p2 = lag(p1,-2)  #half and half
  p2["2011-01-01::"] <- p1["2011-01-01::"]
  purePlot(p1,p2)
  res = roll.aic(p1,p2,wlen=300,search=(-2:2));mean(res,na.rm=T)   #bei wlen300 dauerts fast 7 monate bis er kapiert, dass der lag weg ist
  res = roll.aic(p1,p2,wlen=50,search=(-2:2));mean(res,na.rm=T)   #bei wlen50 dauerts fast24 tage bis er kapiert, dass der lag weg ist
  res
  res = roll.aic(p1,p2,wlen=20,search=(-2:2));mean(res,na.rm=T)   #bei wlen20 dauerts fast5 tage bis er kapiert, dass der lag weg ist
  res
  
  res = roll.aic(p1,p2,wlen=33,search=(-31:-0));mean(res,na.rm=T)   #bei wlen20 dauerts fast5 tage bis er kapiert, dass der lag we
  res
  plot(res[-c(1:90),2])  #die leads
  plot(res[-c(1:90),1])   #die aic dazus
  
  ### wenn die ganze lead-lag-anlayse hier nichts bringt - lohnt sich denn der slope-vergleich?
  b=m.block(p1,p2)
  slope200=rollRegressionXTS(b,200) 
  purePlot(slope200)
  #ist es ein kiesenzeichen wenn der der slope des growth unter den des value geht ?
  
}


#################################################################################


lm.beta<-function(b,p.all=NULL,wlen=0)
{
  P1=b[,1];P2=b[,2];
  today=as.Date(index(last(b)))
  
  x=coredata(P1)
  y=coredata(P2)
  
  fit =lm(y~x,data=data.frame(x,y))
  
  #res=AIC(fit) #When comparing fitted objects, the smaller the AIC or BIC, the better the fit.
  res=coeff(lm)[2]
  
  res
}
#lm.beta<-cmpfun(lm.beta_) #compilier die Funktion

###########################################################################################################################
#### Nach HotLags2.CC und  get.mLeads() (dtw) eine dritte methode um relevante lags zu suchen.
##  diesmal wird hard-core ein lm- modell mit verschieden lags gefitted und der aic gemessen
# der lag der den geringsten aic erzeugt gilt als passend

#gibt xts aus aic und lead zurück
###########################################################################################################################
roll.beta<-function(p1,p2,wlen=50)
{
  b= na.omit(m.block(p1,p2))
  ret <- rollapplyr(b, width=wlen, FUN="lm.beta", by.column=F, align = "right", wlen=wlen)
}

if (F)
  beta=roll.beta(p1,p2,wlen=50)

#############################################################################

#get.dtw.leads<-cmpfun(get.dtw.leads_) #compilier die Funktion
if (F)
  leads=get.dtw.leads(p1,p2,use.HighLows=5)   #hole dir die lead-lag beziehungen
#leads$leads[today]
##################################################################################

clean.matrix<-function(tdata)
{
  tdata = m.ifna.prev(tdata)
  if (len(ncol(tdata))>0)
  for(col.i in 1:ncol(tdata)) tdata[!is.finite(tdata[,col.i]),col.i]<-0 
else
  tdata[!is.finite(tdata)]<-0
  tdata
}

#######################################################################################
#super methode um datums-listen zu bauen - entweder als sublisten eines übergeben xts p
#oder aber eins dynamisch aus stardate,len - erzeugten  tagesdaten p 
#######################################################################################

select.dates<-function(p=NULL,mode,startdate=NULL,len=0,shift=0,visual=F,glaettung = 3)
{
  if (is.null(p) && len >0)
    if (is.null(startdate))
      p  = xts(1:len, Sys.Date()+0:(len-1))
  else  
    p = xts(1:len, as.Date(startdate)+0:(len-1))
  
  switch(mode, 
         quarters={print("quarters");today.list = as.character(as.Date(index(m.to.quarterly(p))))},
         months={print("months");today.list = as.character(as.Date(index(m.to.monthly(p))))},
         years = {print("years");today.list = as.character(as.Date(index(m.to.yearly(p))))},
         
         weeks = {print("weeks");today.list = as.character(as.Date(index(m.to.weekly(p))))},
        hot={ #spannende zeitpunkte der jüngeren Vergangenheit ansteuern
          
          visual=T
          
          pre.wi=0
          p=mNorm(p)
          fs=fast.smoothing(p,glaettung = glaettung,visual=visual)
         
          if (len(fs$hl)==0)
            wendepunkte = as.Date(index(last(p)))-10
          else
            wendepunkte = as.Date(index(p[get.Index(p,as.Date(fs$hl))-shift]))  #ein bischen vor dem wendepunkt
          
          wendepunkte = wendepunkte [as.Date(wendepunkte) > as.Date("2006-01-01")]  #vorher ist s langweilig
          if (visual)
            amark(p[wendepunkte])
          wp=wendepunkte[1]
          allwps=c()
          for(wp in wendepunkte)
              {wps= as.Date(wp)+ -10:20  #immer 90 Tage ab interessantem Datum machen
               if (len(allwps)==0) allwps = wps else allwps = c(allwps,wps) 
              }
          allwps= unique(allwps)
          today.list = as.character(allwps)
          return(today.list)
          
        },
         
{print("default"); today.list = as.character(as.Date(index(p))+shift) })
today.list
}
#.......................................


if (F)
{
  select.dates(p,"years")
  select.dates(p,"quarters")
  select.dates(p,"days")
  select.dates(p,"hot",glaettung=1,visual=T)
  select.dates(p,"hot",shift=30,visual=T)
  
  select.dates(mode="years",len=5000)  
  select.dates(mode="quarters",startdate= "1991-01-01",len=5000)  
}

#######################################################################################
vec2xts<-function(lineal,vector)
{
  lineal=head(lineal,length(vector))
  res=xts(vector,index(lineal))
}

########################################################################################

list2xts<-function(xtsList,what="")
{
  res= foreach(x = xtsList,.combine="merge") %do%
{
  if (what=="")
    return(x)
  else
    if (has(x,what))
      return(x[[what]])
  else
  {
    sag("list2xts  has no %s ",what,warte=T)
    browser()
    return(NULL)
  }
}
print("list2xts")
#  browser()
#  colnames(res)=names(xtsList)
res
}
################################################################################
# gib die Symbole der ausgewählten Spalten zurück
#################################################################################
nTopK <- function
(
  data,   	# matrix with observations
  n.top = 1, 	# top n
  k.top = 1, 	# keep n
  dirMaxMin = TRUE
) 
{
  nt= ntop.keep(data,n.top, k.top)  #SIT 
  
  sela= (bt.apply.matrix(nt,function(col) iif(col==0, NA ,colnames(col))))
  #jetzt sieh zu, dass du die nicht ausgewählten spaltennamen (NA) los wirst
  w=apply(coredata(sela),1, function(x) na.omit(cbind(x)))
  #und mach ein xts daras
  res =xts(coredata(t(data.frame(w))),index(data))
  #mit ordentlichen spaltenname
  colnames(res)=  sapply(c(1:ncol(res)),function(x)sprintf("place%d",x))
  res
}


################################################################################
#################################################################################

Performance.eq <- function(eq, do.print=T) {
  
  if (ncol(eq)==1)
  {  
    
    x = mROC(eq)
    
    cumRetx = Return.cumulative(x)
    annRetx = Return.annualized(x, scale=252)
    sharpex = SharpeRatio.annualized(x, scale=252)
    winpctx = length(x[x > 0])/length(x[x != 0])
    annSDx = sd.annualized(x, scale=252)
    
    DDs <- findDrawdowns(x)
    maxDDx = min(DDs$return)
    maxLx = max(DDs$length)
    
    Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
    names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                    "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
    if (do.print)
      print(Perf)
    return(Perf)
  }
  
  else
  {
    all.perf= foreach( sym = colnames(eq), .combine= "cbind" ) %do%
{
  Performance.eq( eq[,sym],do.print =F)  
}
colnames(all.perf)=colnames(eq)
all.perf=t(all.perf)

print(all.perf)


#source("MLIB/CustomerReport.r")
if (do.print) 
{
  charts.PerformanceSummaryX(  mROC(eq)["2010::"])
  
  charts.PerformanceSummary( mROC(eq))
  b=na.omit(merge(mROC(prices[,data$BENCH]),mROC(eq)))
  
  norm_Win(1)
  chart.RelativePerformance(b[,-1],b[,1])
  temp=all.perf
  
  temp[] = plota.format(100 * temp, 0, '', '%')
  plot.table(temp, smain='Perform', highlight = TRUE, colorbar = TRUE,text.cex=10)
  
  
}
return(all.perf)
  }
}
###############################################################################

daysSinceHigh <- function(x, n){
  apply(embed(x, n), 1, which.max)-1
}

#############################################################################
#wieviel zeilen r-code hab ich nun schon programmiert ?
############################################################################

countLines<-function(dataPath ="MLIB")
{
  library(R.utils)
  #da liegen die r-files
  mP("read at path %s",dataPath)
  #F?r alle xls-Files
  xlsPat = glob2rx("*.r") #wildcard-Transfer
  
  all.n=0
  for (xlsfile in 
       dir(path = dataPath, pattern = xlsPat, all.files = T,
           full.names = F, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE)
  )
  { 
    path=sprintf("%s/%s",dataPath,xlsfile)
    nn <- R.utils::countLines(path)
    if (!is.null(nn))
      all.n = all.n+nn
    else 
      nn=0
    mP("%s %d  %d", xlsfile,nn,all.n)
    
  }
  
  
}
if (F)
  countLines()

##########################################################################

SIT.load<-function(universe,tickers,offline=F,visual=T)
{
  if (offline)
  {
    load(file=universe)
    if (visual)data.info(data)
    
    return(data)
  }
  data <- new.env()
  getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
  for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
  bt.prep(data, align='remove.na', dates='1970::')
  save(data,file=universe)
  if (visual)data.info(data)
  
  data
}
if (F)
{
  
  universe="USA"
  tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')
  d1=SIT.load(universe,tickers,offline=T)
  
  universe="International"
  tickers=spl("GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT")
  d2=SIT.load(universe,tickers)
  
  
  
}

########################################################################################

#engineering.returns.kpi()
textPlot.datainfo<-function(main="")
{
  if (!exists("SAFE") || is.null(SAFE))
    SAFE=""
  if (is.null(data$BENCH))
    data$BENCH=""
  if (!is.null(data$universe))
    main=sprintf("%s\nuniverse:%s",main,data$universe)
  par(mar=c(2,2,2,2))
  try(textplot(sprintf("%s\n %d ticker, BENCH %s,  SAFE %s",main,ncol(data$prices), data$BENCH,SAFE)))
page()
  dataInfo=try(
    rbindlist( lapply(data$prices, function(x)
    {
      sym=colSym(names(x))
      print(sym)
      Cagr = sprintf("%.2f",compute.cagr(x))
      Sharpe = sprintf("%.2f",compute.sharpe(x)/100		)
      
      ft=fromTo(na.omit(data[[sym]])); list(name=sym, LongName=trim(toString(SecList[Name==colSym(names(x))]$LongName)),cagr=Cagr,Sharpe=Sharpe,  from=ft[1],to=ft[2],Min=min(x,na.rm=T),Max=max(x,na.rm=T),len=shape(data[[sym]]))} )))
  
  #mP("%s       %s %s    |%s|%s",dataInfo[["name"]],"|", dataInfo[["LongName"]],"|",dataInfo[["from"]],dataInfo[["to"]],dataInfo[["Min"]],dataInfo[["Max"]])
  #MXXXXXX
  
  #browser()
  DataInfo <-data.frame(dataInfo)  #damit auch im Workspace in RStudio anschaubar
  rownames(DataInfo)= DataInfo[,1]
  textplot(DataInfo[,c(2,3,4,5,6)])
page()  
DataInfo
}

tview<-function()
{textPlot.datainfo()}


#######################################################################################

alloc.map<-function(models)
{
  layout(1:min(5,len(models)))
  for(m in names(models)) {
    w1=dim(models[[m]]$weight)[2]
    mP("%s %d",m,w1)
    plotbt.transition.map(models[[m]]$weight, name=m)
    legend('topright', legend = m, bty = 'n')
  }
  
}

#alloc.map(models)
turnover.map<-function(models)
{
  barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', T)
}

#.........................................

scatter.map<-function(data, models)
{
norm_Win()
if (len(SAFE)<2)
  SAFE=data$BENCH

sprices=data$prices[,c(data$BENCH,SAFE)]  
Equity = list2xts(models,"equity");
colnames(Equity) = names(models)
purePlot(Equity)
print("RiskReturnScatter")

Cols=c(1:(len(models)+len(colnames(sprices))))
try(chart.RiskReturnScatter(merge(mROC(Equity),mROC(sprices)),  main = sprintf("Compare models"), ,colorset = Cols))
}

#........................................
performance.map<-function(data,models,title="  ")
{
  out=list()
  out$TSA=try(strategy.Performance.snapshoot(models,R.arg= TSA.default$R.arg,state="TSA",title=title, data=data,commission=global_commission))
#  out$TSA = rbind(out$TSA, Turnover= sprintf("%.1f",100*sapply(models, compute.turnover, data)))
  out$TSA = t(out$TSA)      
  View(out$TSA)
  out
}
########################################################################################

price2matrix<-function(p,n)
{
  res=p
  for(i in 2:n)
  {
    res = cbind(res,p)
  }
  res
}
##### ohlc- aus reinen preisen machen
prices2data<-function(data)
{
  
  for(name in colnames(data$prices)) 
  {
    new.p = price2matrix(data$prices[,name],6)
    namePrices = paste(name,"Adjusted",sep=".")
    nameOpen = paste(name,"Open",sep=".")
    nameHigh = paste(name,"High",sep=".")
    nameLow = paste(name,"Low",sep=".")
    nameVol = paste(name,"Volume",sep=".")
    nameClose = paste(name,"Close",sep=".")
    colnames(new.p)=c(nameOpen,nameHigh,nameLow,nameClose,nameVol,namePrices)
    assign(name,new.p,envir=data)
  }
}

#.................................................
price2dataPrice<-function(series,name ="USDEUR")
{ 
  new.p=price2matrix(series[,1],6)
  namePrices = paste(name,"Adjusted",sep=".")
  nameOpen = paste(name,"Open",sep=".")
  nameHigh = paste(name,"High",sep=".")
  nameLow = paste(name,"Low",sep=".")
  nameVol = paste(name,"Volume",sep=".")
  nameClose = paste(name,"Close",sep=".")
  
  colnames(new.p)=c(nameOpen,nameHigh,nameLow,nameClose,nameVol,namePrices)
  new.p
}

set.glob<-function(mdata=data, LongShort="F")
{
  global_arg$dat<<-mdata
  global_arg$modell.par <<- list(LongShort=LongShort)
  global_arg$clos <<- mdata$prices
  print(colnames(global_arg$clos))
}

prices2data.env<-function(prices,bench="",safe="",universe="test",do.load=F)
{  
  fname=sprintf("%s.data",universe)
  if (do.load && file.exists(fname))
    {
     load(fname)
     SAFE = mdata$SAFE
     set.glob(mdata)
    }
  else
  {
  mdata = new.env()
  mdata$prices = prices
  prices2data(mdata)
  #mdata$symbolnames = pureColnames(prices)
  mdata$weight = prices; mdata$weight[]=NA
  mdata$symbolnames = colnames(prices)
  if (safe=="")
    safe=mdata$symbolnames[[1]]
  if (bench=="")
    bench = mdata$symbolnames[[1]]
  mdata$BENCH=bench
  SAFE <<- safe 
    mdata$SAFE=safe
  mdata$universe=universe
  save(mdata,file=fname)
  set.glob(mdata)
  }
  mdata
}
########################################################################################

##########  baue eine "flache" - Liste aus einzelmodelle
dels2flatList<-function(modelList,datafile=datafile)  
{
  all.models <<-list()
  all.models=scan.model.list( models,datafile=datafile) # ,all.models=all.models
  return(all.models)
}

########################################################################################
sort.data.frame<-function(df,coln,...)
{
  df.s = df[order(df[,coln],...),,drop=F]
#  print(df.s)
  df.s
}
################################################
xts2xts<-function(lineal,p,fill=F)
{
 res=  merge(lineal[,1],p)[fromToS(lineal),-1]
 if (fill)
   res = m.ifna.prev(res)
 res
}

#################################################
mdates2index<-function(lineal=p,dats)
{
  #lineal=first(lineal,min(shape(dats),shape(lineal)))
  lineal=merge(lineal[,1],dats)
  lineal[,1]=1:nrow(lineal)
  lineal[dats,1]

}
dates2index <- function( x, dates = 1:nrow(x) ) {
  dates.index = dates
  if(!is.numeric(dates)) {
    temp = x[,1]
    temp[] = 1:nrow(temp)
    dates.index = as.numeric(temp[dates])
  }
  return(dates.index)
}
#####################################################################
# komplettes 
m.bt.run<-function(P,t.indi,trade.summary=F,type="share",visual=F,thresh.smooth.win=0,main="",y=NULL)
{
  #......  t.indi > signal - mit und ohne hysterese
  if (thresh.smooth.win<0)
    t.indi= probAny(t.indi,lookback.len=-thresh.smooth.win)-0.5

  signal = sign(t.indi)
  
  if (thresh.smooth.win>0)
  {
    tsm = thresh.smooth(P,t.indi,kwin=thresh.smooth.win,q.w=0.80)
    thresh = tsm$thresh ;signal = tsm$Signal
  }

  if (!has(global_arg$modell.par,"LongShort",what="T"))
    signal[signal < 0] <-0  #er darf nicht short gehen
  #.........................................................
  mdata =list()
  mdata$prices=P; capital=1000000; n=1; 
  mdata$weight=mdata$prices
  mdata$weight[] = ((capital/n) / mdata$prices) * bt.exrem(signal) #exrem entfernt zeitlich aufeinanderfolgende identische signale
  mdata$execution.price=mdata$prices; mdata$execution.price[]=NA
  
  check.ready.for.bt.run(mdata)
  #SIT-Aufruf
  mod=bt.run(mdata, type=type, capital=1000000, trade.summary=trade.summary,commission=global_commission)
  
  if (visual)
  {
    if (ncol(P)<=1)
      plotSigPrice(signal=sign(t.indi),prices=P,indi=list(ret=t.indi, y=merge(P,y)),main=main)
    else
      strategy.Performance.snapshoot(mod,state="T",title=sprintf("just Timing(%s) + BuyHold + BENCH","m.bt.run()"),data=mdata,commission=global_commission,redirectedOut="")    
    
  }
  mod$Tquality = mcalmar(mod$equity)
  
  return(mod)
}
########################################################################################

check.ready.for.bt.run  <-function(data)
{
  if(sum(dim(data$prices) != dim(data$weight)) !=0)
  {
    print(lapply(colnames(data$prices),function(n1) ifelse (!n1 %in% colnames(data$weight),n1,"")))
    print(lapply(colnames(data$weight),function(n1) ifelse (!n1 %in% colnames(data$prices),n1,"")))
    
    print("BUG222a"); stop()
  }
  if (len(data$symbolnames) != len(colnames(data$prices)))
{
    print("will result in turnover-bug")
    print("BUG333aa"); stop()
  }
     
  print("bt.run.kompatible")
}
###########################################################################################
############################################################################################
#Hol Dir aus den indi.Generic.Results die equity und mach ein Ranking darüber
############################################################################################
equity.ranking<-function(indi.Generic.Result,win=200)
{
  equities = foreach(sym = indi.Generic.Result$singleResults, .combine= "merge") %do%
{sym$equity}

ranking <- rollRegressionXTS(equities,win=200)*100
mchart(ranking)
ranking
}

if (F)
{
  x5.1=indi.Generic("signal.drawDown1", global_arg, par = list(win=150) ,visual=F, TRAINSYM=-1)
  x5.1$Tquality 
  ranking=equity.ranking(x5.1)
  
}
############################################################################################
a.indi.Results<-function(indi.Generic.Result)
{
  Tqualities = foreach(sym = indi.Generic.Result$singleResults, .combine= "rbind") %do%
{res=sym$Tquality}
#names(indi.Generic.Result$singleResults)
Tqualities=data.frame(Tqualities)
rownames(Tqualities)=names(indi.Generic.Result$singleResults)
Tqualities = cbind(names(indi.Generic.Result$singleResults),Tqualities)
rownames(Tqualities)=NULL
colnames(Tqualities)=spl("sym,Tquality")
print(Tqualities[order(Tqualities[,1],decreasing=F),])
print("############### Portfolio:")
#ls(x5.1$Signal)
print(indi.Generic.Result$Tquality)
indi.Generic.Result$Tquality
}
if (F)
{
  a.indi.Results(x5.1)
}
###########################################################################################
#enthalte models eine ganze liste von indi.Generi.Results
#für welche Universe Komponenten klappt das Trendfolge-Modell-Portfolio und welche nicht?
###########################################################################################
a.models.Results<-function(models)
{
  print("----------------------------")  
  mP("models.Results")
  print(names(models))
  print("----------------------------")
  indi.results<-function(indi.Generic.Result)
  {
    Tqualities = foreach(sym = indi.Generic.Result$singleResults, .combine= "rbind") %do%
{res=sym$Tquality}
#names(indi.Generic.Result$singleResults)
Tqualities=data.frame(Tqualities)
rownames(Tqualities)=names(indi.Generic.Result$singleResults)
#Tqualities = cbind(names(indi.Generic.Result$singleResults),Tqualities)

#rownames(Tqualities)=NULL
colnames(Tqualities)=spl("Tquality")
#Tqualities[order(Tqualities[,1],decreasing=F),]
Tqualities
  }
tab=foreach(mod.n = ls(models),.combine="cbind") %do%
{
  mod=models[[mod.n]]
  mP("%s    -> %f ",mod.n,mod$Tquality)
  a.indi.Results(mod)
}
#View(tab)
tab=foreach(mod.n = ls(models),.combine="cbind") %do%
{
  mod=models[[mod.n]]
  mP("%s    -> %f ",mod.n,mod$Tquality)
  indi.results(mod)
}
Tab=data.frame(rowSums(tab)/ncol(tab))
colnames(Tab)=c("Tquality")
res=Tab[order(Tab[,"Tquality"]),,drop=F]
print("mean Tquality of universe component:")
print (res)
res
}

if (F)
{
  m=a.models.Results(models)
  m.bad= rownames(m[ m["Tquality"]<0.9,,drop=F] )      
}
#############################################################################################

if (F)
{
  a.models.Results(models)
  models.Results(models)
  
  ranking=multi.model.ranking(models)
  View(ranking)
  purePlot(ranking)
}


##################################################################################
m.scaleTo<-function(prices,bereich=c(100,200), visual =F)
{
  M=NULL
  for(coln in colnames(prices))
  {
    print(coln)
    p=prices[,coln]
    y=scaleTo(p,bereich) 
    if (visual)    purePlot(y)
    if (is.null(M) ) M=y else M=merge(M,y)
  }
  if (visual)
    purePlot(M)
  M
}
########################################################################################
#hilfsfunction für showTable:  gib einen Vector der Länge n1 zurück (hinte wird " " aufgefüllt)
nvec <-function(vec,n1)
{
  res=vec
  
  for(i in shape(res):n1)
    res=c(res," ")
  res
}
#............................
#gib einen DataFrame mit nvec-gefüllten aufgefüllten Vectoren als spalten zurück 
showTable<-function(...)
{
  df = NULL
  
  arglist=list(...)
  
  if (length(arglist)==1 && is.list(arglist[[1]]))
    arglist=arglist[[1]]

  n1=max(sapply(arglist,FUN=function(col)length(col)))
  
  for(col in arglist)
    if (len(col)>0)
      if (is.null(df))
        df = data.frame(nvec(vec=col,n1=n1))
  else
    df = cbind(df,nvec(vec=col,n1=n1))
  colnames(df)=names(arglist)
  View(df)
  df
}

if (F)
{
  showTable(y=data$Y.names,intermarket=data$intermarket.faktoren,fundamental=data$fundamentale.faktoren,cloud=colnames(data$cloud))
  
  showTable(c(1,3,4),c(3,5))
  nvec(c("1","2","3"),10)
  
  df=data.table(cloud=colnames(data$cloud),sym=nvec(data$Y.names,10))
  df=merge(df,data.frame(sym=data$Y.names))
  View(df)
}
#####################################################################################
#funktion umbiegen http://stackoverflow.com/questions/8384382/access-all-function-arguments-in-r

f.test <- function(a, callback, b, c, d, ...) {
  # Grab the "f" call expression
  fcall <- match.call(expand.dots = FALSE)
  
  # Construct the new call expression
  fcall[[1]] <- callback
  # Filter out / add new args
  fcall$callback <- NULL
  fcall$z <- z
  
  # Do the call
  eval(fcall, parent.frame())
}
########################################################################################
# improved list of objects

.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}
# shorthand
#zeigt schön den speicherbedarf der objekte
lsos <- function(..., n=100) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

########################################################################################

if (F)
   lsos()

print("########### load InputConfig_Portfolio_TD5.R")

if (F)
  list_R_functions('MLib/InputConfig_Portfolio_TD5.R')
