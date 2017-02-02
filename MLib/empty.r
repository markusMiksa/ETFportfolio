
options(error = quote({
  #  sink(file="error.txt");
  dump.frames();
  print(attr(last.dump,"error.message"));
  traceback();
  #  sink(); 
})) 
options(warn=1)
options(error = browser)

##############################################################################################
#Aufgaben:

#Marken

##############################################################################################



########################################################################################
print("########### load InputConfig_Portfolio_TD4.R")

if (F)
  list_R_functions('MLib/InputConfig_Portfolio_TD4.R')
