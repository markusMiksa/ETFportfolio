################################################################################################################
# Lade Eckhards  xls daten  nach xts pro sheet 
################################################################################################################

options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
################################################################################################################
if (F)
{
  #einlesen von xls -sheets in xts- variable
  euro.macros= read.EckhardsXLS(xlsName="EuropeMM.xls",startRow=15,date.format = "%b %y",to.month.end=T,debug=F)
  euro.indi= read.EckhardsXLS(xlsName="Index01.xls",startRow=5,belowColnames=5,debug=F)
  
  if (F)  #ist viel Schrott in den Spalten ?
  {
    colSums(diff(euro.macros["1997::"],30),na.rm=T)
    colSums(euro.macros["1997::"],na.rm=T)
    colSums(ROC(euro.indi["1997::"],30),na.rm=T)
    colnames(euro.macros)
  }
  
  dim(euro.macros)  #154 Spalten mit 202 Monaten
  dim(euro.indi)    #8 Kurse mit 4372 Tagen
  
  euro.macros.n= mNorm(lag(euro.macros))["1997::"] #hier schon mal gelagged weil die Teile ja einen Monat Lieferverspätung haben
  euro.indi.n = mNorm(euro.indi)["1997::"] #wichtig:  die Anfangslag normiert die (frühen) Werte
    
  if (F)
  {

    purePlot(ROC(euro.macros["1997::"],1),main="roc 1 euro.macros")  #kaum was zu sehen
    purePlot(ROC(euro.macros["1997::"],30),main="roc 30 euro.macros")  #starke vola-cluster
    purePlot(ROC(euro.indi["1997::"],30),main="roc 30 euro.indi")
    
    
    purePlot(euro.macros.n)
    purePlot(euro.indi.n)
    
    chart.Correlation(ROC(euro.indi["1997::2000"],1),main="1997::2000")
    chart.Correlation(ROC(euro.indi["2008::2011"],1),main="2008::2011")
    chart.Correlation(ROC(euro.indi["2010::2014"],1),main="2010::2014") 
    chart.Correlation(ROC(euro.indi["2010::2014"],30),main="2010::2014") 
    
    correlogramm(ROC(euro.indi["1997::"],30),main=" euro.indi")
    correlogramm(ROC(euro.macros["1997::",c(1:30)],1),main=" euro.macros")
  }
  
  #ein virtuelles data-Teil vorbereiten
  ls(data)
  data = new.env()
  data$symbolnames=colnames(euro.indi.n)
  data$BENCH="DAX30"
  data$prices=euro.indi.n
  data$weight = data$weights=data$prices;data$weight[]=0
  
  #bastel aus den kurzeitreihen Close-Zeitreihen in data - damit das SIT-Framework laufen kann
  no=lapply(colnames(data$prices), function(sym)
  {
  #sym="DAX30"
  sym.val = data$prices[,sym]
  colnames(sym.val)=sprintf("%s.Close",sym)
  assign(sym,sym.val,envi=data)
  })
  ls(data)
  #Target mit TargetMethod -1,0, oder 1 berechnen
  data$Target   <-compute.Target.by.method(data=data, TargetMethod=-1, w=30 , minReturn=0.15, min.duration=40,glaettung=5,visual=T)
  #eine multi-Target-DataCloud vorbereiten  .. einige technische Indikatoren auf indizes und macros 
  
}


