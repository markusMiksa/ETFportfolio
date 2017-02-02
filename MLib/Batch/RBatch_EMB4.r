
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))

options(warn=1)





print("run code at MM_Main.r")

setwd("d:/R/Nuggets/ETFmodel")

#library(RGtk2 )
print("ok")
rm(MM_lesson1)
source("MLib/MM_Main.r")
batchRun()

sag("auf gehts",warte=T)
