
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))

options(warn=1)


#####################################################################################
# serialisieren von parameter-listen- in global_ParTable
# ?ber mlist greifen alle Indikatoren auf ihre Parameter zu
# eine trainings-routine kann zu einem beliebigen Tag angestartet werden und ab diesem tag die Parameter ver?ndern.
#Unter den aktuellen Werten der Globalen:
#  objectId = global_objectId, time=global_StartDate werden die Werte der par-list
# in die globale Tabelle global_ParTable eingetragen - bzw. aus ihr ausgelesen
# wenn sie schon unter diesem Key vorhanden sind.
#für objectId denke man sich was schönes aus - z.B. Dax.indiMacd.Trend... oder so
#####################################################################################
#rm(global_ParTable)
#mlist(wShort = c(50, -5, 5), wLong = c(200, -20, 20)) 
#mlist(winLen=200)

#mlist liest aus global_ParTable die par- Spalte
#wenn der key noch nicht vorhanden ist oder mode =="w"  und in ... ein arg gegeben ist,
#dann schreibt (evtl überschreibt == patched) er auch die Werte
#mode = "r": read .. wie mlist() .. (default)  - lies die aktuelle Zeile aus global_ParTable 
#  - suche .. wenn nichts gefunden wird auch bei den davorliegenden Datümern
#mode = "rx":  wie "r" aber lies nur die Zeile zum aktuellen global_StartDate
#mode = "w" :  schreibe in die global_ParTable () // das macht er auch wenn global_ParTable noch leer ist und arg gegeben ist ... (patcht die Werte mit ...)
#mode = "d" : löscht die aktuelle Zeile

mlist_<-function(...,mode=c("r","rx","w","d","R"),visual=F)
{ 
  mP("call mlist")
#  browser()
  if (is.vector(mode) && len(mode)>1 )    mode =first(mode)
  if (visual)
  {print("\n##### mlist ")
   print(paste(global_objectId," ",global_StartDate))
  }
  # browser()
  #  if(length(global_StartDate)==0)
  #    stop("Bug at mlist ###############################")
  #browser()
  #arg=list(...)
  #arg <- as.list(substitute(list(...)))
  #else
  ArgIslist=F
  arg = c(...)
  if (!is.list(arg))
    {arg = list(...)      
     ArgIslist=T   
  }
  
   
 #mP("mlist")
  #browser()  #MMx1
  #arg = list(...)
  
  #falls in der global_ParTable schon passende Parameter stehen - nimm sie, acndernfall schreib sie hinein
  # syntax-beispiel mit data.table
  
  #df <- data.table (model = "sadf", time="2007",  par=I(list(list(a=3,b=5))))
  #df2<- data.table (model = "sma", time="2007",  par=I(list(list(a=33,c=53))))
  #df3<- data.table (model = "sma", time="2008",  par=I(list(list(a=23,c=883))))
  #df4 <- data.table(rbind(df,df2,df3))                
  
  #len(df4[time=="2009" & model=="sma"]$par)  
  #rm("global_ParTable")
  
  
   #browser()
  
  if (!exists("global_ParTable")  ||length(global_ParTable)==0 || dim(global_ParTable)[1]<1)
  {
    if (length(arg)==0)
      return(list())
    #print(" build new indiPar ")
    global_ParTable <<- data.table (objectId = global_objectId, time=global_StartDate,  par=I(list(list(...)))) #siehe syntaxbeispiel
    # defaultPar = (cbind(...))
    mP("new global_ParTable")
    # browser()
  }
  else    #du musst die parameter reinschreiben weil dieses global_objectId noch nichts reingeschrieben hatte
  {
    
    if (len(arg)>0 && dim(global_ParTable[time==global_StartDate & objectId==global_objectId])[1]<1) 
      {mP("switch to mlist(mode=w)")
       mode="w"
     }
  }
  
  #mP("such in mlist()")
  #browser()
  
  foundParList =global_ParTable[time==global_StartDate & objectId==global_objectId]
  foundPar = NULL
  if (!is.null(foundParList) && dim(foundParList)[1]>0)
    foundPar = foundParList$par
  
  
  if (len(foundPar) > 0)
  { 
   #browser()
    if (visual)cat("\n foundPar ")
    if (mode =="d" ) #löschen
    {
      
      global_ParTable[time==global_StartDate & objectId==global_objectId]<<-NA_character_
      return( list() )  
    }
    if (len(arg)<=0 || mode=="r" || mode=="R")  
      defaultPar = foundPar[[1]] #schon da - nur lesen
    else
    {      
      if (mode == "w")
      {
        if (visual)cat("\n patch value ")
        #browser()
        mP("mlist w")
        
        oldArg=global_ParTable[time==global_StartDate & objectId==global_objectId]$par[[1]] 
        
        newArg = arg
        #mP("WWWWWWW mlist"); browser()
        if (F)
        {
          #patch first-values
          newArg=list()
          for (nam in names(arg))
          {
            oa = oldArg[[nam]]
            oa = c(oa[-1])
            newArg[[nam]]= c(arg[[nam]],oa )
          }
          
        }
        global_ParTable[time==global_StartDate & objectId==global_objectId]$par <<- I(list(list(newArg)))
        #browser()
        #global_ParTable$par
        #mlist()
        defaultPar = newArg  #lesen und neu übergebenen arg-wert einpatchen
        
      }
    }
  }
  else #neu
  {
    # browser()
    #cat("\n new Par") #MM3
    if ( mode != "w" || length(arg)==0)#zum gew?hlten global_StartDate gibts noch keinen Parametersatz
    {
      if (visual)mP("\nunknown par-date %s",toString(arg))
      if (mode == "rx")   # kein Zugriff auf den vor-letzten Eintrag erwünscht
        return(NULL)
      nearestPar = global_ParTable[time<=global_StartDate & objectId==global_objectId ]
      
      foundPar = last(nearestPar)$par
      defaultPar = foundPar[[1]]
      #      foundPar =global_ParTable[time==global_StartDate & objectId==global_objectId]$par
    }
    else
      if (mode=="w")
      {
        
        mP("mlist: write New Line")
        #browser()
        out = list( ... )
        if( is.list(out[[1]]) )
          global_defaultPar= res = out
        else
        global_defaultPar= res=I(list(list(...)))
        #df <- data.table (objectId = global_objectId, time=global_StartDate, par=I(list(list(...))))
        df <- data.table (objectId = global_objectId, time=global_StartDate, par=global_defaultPar)
        
        #par=I((list(...))))
        #h?nge neue Zeile an global_ParTable an
        global_ParTable <<- data.table(rbind(global_ParTable,df))
        
        return (mlist())
        #global_ParTable$par
        #defaultPar = data.frame(cbind(...))
        
        #  mP("1WWWWWWW mlist") ;browser()
        
      }
  }
  
  global_defaultPar <<-defaultPar
  #  print (defaultPar)
  
  if (mode != "R" && mode != "w")   #Beschränkung auf die first values der parameter
    res= lapply(defaultPar,FUN=function(x)as.double(x[1]))
  else
    res = defaultPar
  if (visual) mP("mlist %s: %s",len(res),toString(list(res)))
  
  return(res)
}
mlist<-cmpfun(mlist_) #compilier das Teil

mP("########### load mlist.R")
