########################### Gui Beispiele

#einfache Consolen Eingaben gehen mit 
#   scan(n=1,what="")

#deducer macht probleme aber gWidgets geht gut,
#hier ein confirmDialog,

################################################################################
if (F)
{
  readChar()
  input()
  scanf()
  
  scan(n=1,what="")
  
  
  readChar()
  readLines(con=stdin())
}

##############################################################################

#http://cran.r-project.org/web/packages/gWidgets/vignettes/gWidgets.pdf
confirmDialog <- function(message, handler=NULL) {
  library(gWidgets)
  library(gWidgetstcltk)
  
  
  window <- gwindow("Confirm")
  group <- ggroup(container = window)
  #gimage("info", dirname="stock", size="dialog", container=group)
  
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group, expand=TRUE)
  
  ## A group to organize the buttons
  button.group <- ggroup(container = inner.group)
  ## Push buttons to right
  addSpring(button.group)
  res=gbutton("ok", handler=handler, container=button.group)
  res=gbutton("cancel", handler = function(h,...) dispose(window),
          container=button.group)
  
  return(res)
}
#The key to making a useful con_rmation dialog is attaching a response to a
#click of the \ok" button. This is carried out by a handler, which are added using the
#argument handler= for the constructor, or with one of the addHandlerXXX functions.
#The handler below prints a message and then closes the dialog. To close the dialog,
#the dispose method is called on the \ok" button widget, which is referenced inside
#the handler by h$obj below. In gWidgets, handlers are passed information via the
#_rst argument, which is a list with named elements. The $obj component refers to
#the widget the handler is assigned to.
#Trying it out produces a widget like that shown in Figure 3
if(F)
confirmDialog("This space for rent", 
              handler = function(h,...) {
  print("what to do... [Change accordingly]")
  ## In this instance dispose finds its parent window and closes it
  dispose(h$obj)
  
}
              )

if(F)
res=confirmDialog("repair this ?", handler = function(h,...) {
  print("what to do... [Change accordingly]")
  ## In this instance dispose finds its parent window and closes it
  dispose(h$obj)
  })



######################################################################################
print(" ############# load Gui.R")