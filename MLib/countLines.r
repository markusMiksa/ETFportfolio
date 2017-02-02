
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
