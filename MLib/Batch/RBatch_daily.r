
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink();
}))

options(warn=1)





print("run code at email.r")

setwd("d:/R/Nuggets/ETFmodel")
source("MLib/email/email.r")

