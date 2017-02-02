####################################################################################
#beschreibung technischer indikatoren
#Quelle: http://www.broker-test.de/finanzwissen/technische-analyse/volatilitaet/  
#####################################################################################
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))

options(warn=1)


####### verschicke email via web.de mit anhang.
#zunächst mit excel.r ein formatiertes excel-sheet mit orders füllen
#dann via batch verschicken:

if (F)
{
commandCall ="d:/R/Nuggets/ETFmodel/MLib/email/testmail.cmd"
setwd("d:/R/Nuggets/ETFmodel/MLib/email")
# Issue the command via system() - sending it to CMD
returnVal <- system(commandCall,intern=T,wait=T)
print(returnVal)
setwd("d:/R/Nuggets/Eckhard")
}

#s
collect.Orders<-function(model,lastDate)
{
  browser(mP("collect.Orders"))
  str(model$trade.summary$trades)
  model$trade.summary$trades 
  #mod$trade.summary$stats 
  
}


mail.Orders<-function(models )
{
  xlsName="Models\\Orders\\Customers.xls"
  wb=readTable(xlsName,table="subscriptions")
  customers=sapply(wb[,"Customer"],trim)
  subs <- data.table(wb); setkey(subs,"Customer")
  #für alle kunden
  allorders= foreach( customer = customers, .combine="rbind") %do% {
    customer="markus"
    lastDate=(first(subs[subs$Customer==customer]$lastOrdersSend) )
    #für die c.models ist der customer  subscribed
    c.models=first(subs[subs$Customer==customer]$Models)  
    if (len(model)<0)
      sag("Sorry-there are no models for Customer %s",customer,warte=T)
    else
      c.models = sapply(spl(c.models),trim)
    
    #wann wurde dem Kunden was zuletzt geschickt ?
    lastDate=(first(subs[subs$Customer==customer]$lastOrdersSend) )
    # die  umgebenden "-zeichen abschneiden
    lastDate=substring(lastDate, 2, nchar(lastDate)-1)
    lastDate = as.Date(lastDate)
    
    #sammel die neuen transaktionen ein
    orders=  foreach(mod = names(models), .combine="rbind") %do%{
      #mod="buyHold"
      if (len(which(c.models==mod))>0)  #gehört das model zu den c.models
        orders=collect.Orders(models[[mod]],lastDate)
      orders = cbind(mod,customer,orders)  #eine Spalte mit dem Modellnamen dazugeben
    }
    
    #schreibe die Ordrs in ein hübsches xls
    library(xlsx)
    wb <- xlsx::createWorkbook()
    sheet <- xlsx::createSheet(wb, sheetName="addDataFrame1")
    data <- data.frame(mon=month.abb[1:10], day=1:10, year=2000:2009,
                       date=seq(as.Date("1999-01-01"), by="1 year", length.out=10),
                       bool=c(TRUE, FALSE), log=log(1:10),
                       rnorm=10000*rnorm(10),
                       datetime=seq(as.POSIXct("2011-11-06 00:00:00", tz="GMT"), by="1 hour",
                                    length.out=10))
    cs1 <- xlsx::CellStyle(wb) + xlsx::Font(wb, isItalic=TRUE) # rowcolumns
    cs2 <- xlsx::CellStyle(wb) + xlsx::Font(wb, color="blue")
    cs3 <- xlsx::CellStyle(wb) + xlsx::Font(wb, isBold=TRUE) + xlsx::Border() # header
    xlsx::addDataFrame(data, sheet, startRow=3, startColumn=2, colnamesStyle=cs3,
                       rownamesStyle=cs1, colStyle=list("2"=cs2, "3"=cs2))
    # Don't forget to save the workbook ...
    xlsx::saveWorkbook(wb, "d:/orderTest.xlsx")
    
    X1= readTable(xlsName="d:\\DataInfo.xls",table="Table")
    
    wb=data.frame(date=c(todayS2()), model=c("testmodel"), orders =c(2))
    
    writeTable(wb,xlsName="sprintf(")
    
    #patche  die customer.xls
    lastDate =todayS2()
    subs[subs$Customer==customer]$lastOrdersSend = lastDate
    
    return(orders)
  }
  writeTable(wb,xlsName=xlsName)
  
}
