
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

commandCall ="d:/R/Nuggets/ETFmodel/MLib/email/testmail.cmd"
setwd("d:/R/Nuggets/ETFmodel/MLib/email")
# Issue the command via system() - sending it to CMD
returnVal <- system(commandCall,intern=T,wait=T)
print(returnVal)
setwd("d:/R/Nuggets/ETFmodel")





library("mail")
#funktioniert 

#kann nur kurze Nachrichte schicken - keine attachements

print("send mail alter !!")
print(Sys.time())

sendmail("markus.miksa@web.de", "R notice2", paste("Calculation finished.\nFetch your data!",Sys.time()))
#### Problem:  das einloggen auf dem web.de- Server
#sendmailR:   SMTP AUTH is currently unsupported.
#Lsg: --- entweder mit separatem Tool #A)  oder # B)  eTools  - sieht gut aus

if (F)
{
  
  returnVal <- system("d:/R/Nuggets/ETFmodel/MLib/email/testmail.cmd",intern=T,wait=T)
 
  
  
  #mit separatem sendMail.exe .. kann sich beim Server anmelden
  #http://caspian.dotconf.net/menu/Software/SendEmail/
  #http://indiacrunchin.wordpress.com/2011/11/25/sending-email-from-r-sing-sendemail/
  ##A)
  
  # Create the required command line parameters for sendEmail to work
  paramsList <- list()
  paramsList$fromAddress <- c("-f",'markus.miksa@web.de')
  paramsList$toAddress <- c("-t",'markus.miksa@web.de')
  paramsList$emailSubject <- c("-u","Test Email from R")
  paramsList$listemailMessage <- c("-m",paste("Sent at ",format(Sys.time(),"%Y-%d-%m:%H-%M-%S"),sep=" "))
  paramsList$serverAndPort <- c("-s","smtp.web.de:587")
  paramsList$fileAttachPath <- c("-a","d:\test.txt")
  paramsList$accUsername <- c("-xu","markus.miksa")
  paramsList$accPassword <- c("-xp","milkapanter")
  # Add double quotes to the parameter values
  paramsList1 <- lapply(paramsList,function(x){x[2] <- dQuote(x[2]);paste(x,collapse = " ")})
  # Combine to create one single function call
  suffixCall <- paste(do.call("c",paramsList1),collapse = " ")
  mailPrg ="d:/R/Nuggets/ETFmodel/MLib/email/sendEmail.exe"
  commandCall <- paste(mailPrg,suffixCall,sep = " ")
  # Issue the command via system() - sending it to CMD
  returnVal <- system(commandCall,intern=T,wait=T)
  print(returnVal)  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  findFn("email")
  
  
  library("mail")
  #funktioniert 
  
  sendmail("markus.miksa@web.de", "R notice2", "Calculation finished.\nFetch your data!")
  
  
  #############################################################################
  
  sendEmail <- function(subject, text, address, file = FALSE, filename = "MyRFile"){
    if(file == FALSE){
      sys.arg <- paste("echo '", text, "' | mail -s ", subject,  " ", address, sep = "")		
    }else{
      ##		filename <- cat(filename, "." , unlist(strsplit(text, split="\\."))[2], sep = "")
      ##		sys.arg <- paste("uuencode ", text, " ", filename, "| mail -s ", subject, " ", address, sep = "")      
      sys.arg <- paste("mail -s ", subject, " ", address, " < ", text, sep = "") 
    }
    
    system(sys.arg)
  }
  
  
  sendEmail(subject = "subjectText", text = "EmailFiles/log.txt",          address = "markus.miksa@web.de", file = TRUE)
  
  #############################################################################
  
  
  #http://stackoverflow.com/questions/2885660/how-to-send-email-with-attachment-from-r-in-windows
  
  library(sendmailR)
  
  #set working directory
  setwd("d:/R/Nuggets/ETFmodel")
  
  
  #####send plain email
  
  from <- "<markus.miksa@web.de>"
  to <- "<markus.miksa@web.de>"
  subject <- "EmailfromsendmailR"
  body <- "Hallo Markus "                     
  mailControl=list(smtpServer="smtp.web.de")
  
  #contents,subject,from,to,attMIME,attachment,control)
  sendmailR::sendmail(from=from,to=to,subject=subject,  mes=body,control=mailControl)
  
  to <- "<markus.miksa@web.de>"
  subject <- "HellofromR"
  body <- list("It works!")
  sendmailR::sendmail(from, to, subject, body,   control=list(smtpServer="smtp.web.de"))
  
  sendmail(recipient="markus.miksa@web.de",subject="r test",message="hallo",password="rmail")
  
  from <- sprintf("<sendmailR@%s>", Sys.info()[4]) 
  to <- "<markus.miksa@web.de>" 
  
  #####send same email with attachment
  
  #needs full path if not in working directory
  attachmentPath <- "EmailFiles"
  
  #same as attachmentPath if using working directory
  attachmentName <- "log.txt"
  
  #key part for attachments, put the body and the mime_part in a list for msg
  attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
  bodyWithAttachment <- list(body,attachmentObject)
  
  sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)
  
  #In addition, multiple files can be sent by adding another mime_part to the msg list as follows (I also condensed it):
  
  attachmentObject <- mime_part(x="EmailFiles/log.txt",name="log.txt")
  attachmentObject2 <- mime_part(x="EmailFiles/log2.txt",name="log2.txt")
  bodyWithAttachment <- list(body,attachmentObject,attachmentObject2)
  
  sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)
  
  
  ####################################################################################
  #http://stackoverflow.com/questions/4241812/how-can-i-send-receive-smtp-pop3-email-using-r?rq=1
  ####################################################################################
  ##B)
  #HOT
  ##Note: Other programs are wrapped in R functions and system calls.
  #They each have their own licenses which may or may not allow the use suggested here
  #Programs used here:
  #STunnel: http://www.stunnel.org/; Provides an SSL tunnel but requires OpenSSL 
  #OpenSSL: http://www.openssl.org/; OpenSSL to actually provide SSL
  #   Note that these .dlls should be placed with the stunnel exe.
  #   Also note that libssl32.dll may need to be renamed from ssleay32.dll
  #Microsoft Visual C++ 2008 Redistributable (may be required for the SSL .dlls to work correctly)
  #Blat: http://www.blat.net; a public domain SMTP sending program
  #Getmail is free for non-commercial use. If you use it in a business environment, then a fee of $50 USD is payable to Tim Charron. 
  
  #Stunnel is a TSR, so it will need to be killed from the task manager if there is an issue.  If you are willing to install it as a service you may be able to tweak my code to start and stop the service.  
  #My current code does not create .conf file for stunnel the way a full version ought.  Check http://spampal.sanesecurity.com/manual_eng/servers/stunnel/stunnel.htm#sconfig21 to create the appropriate configuration file.
  
  #Set the config values as appropriate
  ##Config##
  BLAT.loc <- "c:/Programming/R/Rmail/blat262/full/blat.exe"
  GetMail.loc <- "C:/Programming/R/RMail/getmail133/getmail.exe"
  stunnel.loc <- "C:/Programming/R/RMail/stunnel/stunnel-4.11.exe"
  
  #The set mail function assigns the username and password to be used as well as the smtp and pop3 servers it starts stunnel (and assumes that the stunnel.conf file is present and set correctly).
  setMail <- function(user,pw,SSL=FALSE,smtp="127.0.0.1:259",pop3="127.0.0.1:1109")
  {
    if (SSL==TRUE)
    {
      print("Starting stunnel; you will need to kill this from the task-manager")
      system(stunnel.loc,wait=FALSE)
      Sys.sleep(2) #Give it time to start 
    }
    return(list(user=user,pw=pw,smtp=smtp,pop3=pop3,SSL=SSL))
  }
  
  #function to send mail, myMail is the resulting list from setMail
  sendmail <- function(myMail, to, subject, msg,VERBOSE=FALSE)
  {
    writeLines(msg, "out.txt", sep = "\n", useBytes = FALSE)
    targ <- paste(getwd(),"/out.txt",sep="")
    call <- paste(BLAT.loc, ' "',targ,'" -subject "',subject,'" -to ',to," -u ",myMail$user," -pw ",myMail$pw, " -f ",myMail$user, " -debug -server ",myMail$smtp,sep="")
    res <- system(call,intern=TRUE)
    if (VERBOSE) {return(res)}
  }
  
  #function to get mail, myMail is the resulting list from setMail; it returns a list with one element that contains everything unparsed, another list provides the number of messages remaining on the server.
  getmail <- function(myMail,VERBOSE=FALSE)
  {      
    unlink("MSG1.txt") #drop previous get
    #download next message
    call <- paste(GetMail.loc," -u ",myMail$user," -pw ",myMail$pw," -s ",strsplit(myMail$pop3,":")[[1]][1],
                  " -port ",strsplit(myMail$pop3,":")[[1]][2]," -n 1",sep="")
    res <- system(call,intern=TRUE)
    if (VERBOSE) {print(res)}
    nmsgtxt <- res[grep("messages on the server.",res)]
    nstart <- regexpr("There are",nmsgtxt)
    nend <- regexpr("messages on the server.",nmsgtxt)
    nmess <- as.numeric(substr(nmsgtxt,10,nend-1))-1
    x <- readLines("MSG1.txt",-1)
    return(list(message=x,remaining=nmess))
  }
  
}