#MM_MAIN    Hier bau ich mein Aktien/Renten - Allocationssystem

#mach voher mmBasicStartup.r
#Einführung in dei Lib des SysInvestors

if (!exists("MM_lesson1"))
{
  rm(list=ls())
  print("### load BasicLib ###")
  source("MLib/portfolioCode.r")
  source("MLib/LoadDataFuncs.r")
  source("MLib/TradeClasses.r")
  source("MLib/InputConfig_Portfolio_TD.r")
}

options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)

#source("MLib/SITpatches.r")  #mein Anpassungen dazu

if (F)  #falls reinit- gewünscht anklicken
  rm(MM_lesson1)

MM_lesson1="ok"
#-----------------------------------------------------------------------------------------------
ModelRoot = "O:/R/Nuggets/Eckhard" 
setwd(ModelRoot)

testXLSread<-F
readMyClasses<-T

#http://ichart.finance.yahoo.com/table.csv?s=%5EHSI&a=11&b=31&c=1986&d=03&e=3&f=2013&g=d&ignore=.csv

#http://chart.yahoo.com/table.csv?s=%5EHSI&a=0&b=01&c=1975&d=3&e=03&f=2013&g=d&q=q&y=0&z=^DJI&x=.csv 

globalPROVIDER = "CSV"
csvDatFormat="%Y-%m-%d"
globalSEP = ";"
globalDEC = "."
globMDataSUB=""

lookback.len = 60
#periodicity = 'weeks'
periodicity = 'months'
prefix = ''
dataSet = "MM_Feb1"#modelDir
myFrame =""
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Load("gv")
  GDAXI = gv$t0data$Dax
  xtsPrice = Cl(GDAXI)
  #xtsPrice = Cl(to.monthly(xtsPrice))
  dim(xtsPrice)
  
  # frame="2006-12-01::2010-01-01"
  
  #frame="2007-11-01::2010-01-01"
  #  frame="2008-09-24::2010-01-01"
  
  #  frame="2003-08-01::"
  frame="1997::"
  
  xtsPrice=xtsPrice[frame]
  xtsPrice = mNorm(xtsPrice)
  print(len(xtsPrice[,1]))
  fromTo(xtsPrice)
plot(xtsPrice)

mon=endpoints(xtsPrice,"months")
mon = mon[mon>0]
xtsMon =xtsPrice[mon]
plot(xtsMon)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~