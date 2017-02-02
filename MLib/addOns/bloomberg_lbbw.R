current.kw <- 44
# ************** Functions ***************
regexpr1 <- function(x) {
  if (grepl(reg.ex1, x)) {
    y.pos <- regexpr(reg.ex1, x)
    y <- substr(x, y.pos[1], -1+y.pos[1]+attributes(y.pos)[[1]])
    if (grepl(reg.ex2, x)) {
      z.pos <- regexpr(reg.ex2, x)
      z <- substr(x, z.pos[1], -1+z.pos[1]+attributes(z.pos)[[1]])
    } else {
      z <- NA
    }
  } else {
    y <- NA
    z <- NA
  }
  return(c(y,z))
}

rtg.val <- function(x){
  x.val <- rating[match(x, rating[,1]),2]
  return(x.val)
}

getData <- function(isin) {
  library(Rbbg)
  conn <- blpConnect()
  bbg <- bdp(conn, paste(isin, "corp"), bbg.fields)
  blpDisconnect(conn)
  worst.rtg = pmax(rtg.val(bbg$RTG_MOODY_NO_WATCH), rtg.val(bbg$RTG_SP_NO_WATCH), rtg.val(bbg$RTG_FITCH_NO_WATCH))
  x <- data.frame(ticker = bbg$TICKER, name = bbg$SECURITY_NAME, moody = bbg$RTG_MOODY_NO_WATCH, 
                  sp = bbg$RTG_SP_NO_WATCH, fitch = bbg$RTG_FITCH_NO_WATCH, worst.num = worst.rtg, parent.risk = bbg$ULT_PARENT_CNTRY_OF_RISK,
                  piece=bbg$MIN_PIECE, dur = bbg$DUR_ASK, yld = bbg$YLD_CNV_ASK, price = bbg$PX_DISC_ASK, maturity = bbg$MATURITY,
                  spread = bbg$GOVT_CNV_SPREAD_ASK, currency = bbg$CRNCY, issuer = bbg$ISSUER, coupon = bbg$CPN, subordinated=bbg$IS_SUBORDINATED)
  return(x)
}

getLbbwFile <- function() {
  for (i in 1:100) {
    file <- paste0(path, format(Sys.Date()-i,"%Y%m%d"), stamm, ".csv")
    if (file.exists(file)) {
      print(paste("Datum letzte Datei:", format(Sys.Date()-i,"%Y-%m-%d")))
      break
    }
  }
  return(file)
}

# ************** Functions End ***************
# Parameter
min.rat.num <- 14 # 14 = B+
max.rat.num <- 7 # 7 = A-
max.piece <- 1000 # Max. Stückelung
max.price <- 115 # Max. Preis
min.yld <- 1.0 # Minimum Rendite
ex.countries <- c("PT", "GR", "IE", "CY")
ex.isin <- c("FR0010154385")
path <- "K:/Rentenmanagement/Credits/PUK/"
stamm <- "_PUK_Fokus_Corporates"
# Read LBBW Credit Template - enrich and filter
lbbw.file <- paste0(path, "LBBWCredits_KW", current.kw, "_Portfolio und Empfehlungen.txt")
rating <- read.delim(paste0(path, "rating.txt"), header = T)
bbg.fields <- c("YLD_CNV_ASK", "GOVT_CNV_SPREAD_ASK", "DUR_ASK","PX_DISC_ASK", "CRNCY", "CPN",
                "ULT_PARENT_CNTRY_OF_RISK", "TICKER", "ISSUER", "MATURITY", "SECURITY_NAME",
                "RTG_SP_NO_WATCH", "RTG_MOODY_NO_WATCH", "RTG_FITCH_NO_WATCH", "MIN_PIECE", "IS_SUBORDINATED")
reg.ex1 <- "^(grün|gelb|rot)\\b"
reg.ex2 <- "\\b(DE|XS|IT|FR|BE)[a-zA-Z0-9]{10}\\b"

lbbw <- as.character(t(as.vector(read.delim(lbbw.file))))

x <- lapply(lbbw, regexpr1)
x.df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=T))
res <- x.df[which(complete.cases(x.df)),]
res <- res[!duplicated(res),]
df <- cbind(res, getData(as.character(res[,2])))
#Ausschluss bestimmter Titel
rot <- as.character(unique(df[df[,1]=="rot",]$ticker)) # Ticker welche mind. einen Bond rot haben
df <- df[!(df$ticker %in% rot), ] #Ausschluss aller Ticker mit einem Bond Rot
df <- df[!(df$parent.risk %in% ex.countries),] #Ausschluss der Peripherieländer
df <- df[!(df$X2 %in% ex.isin),] #Ausschluss der ISINs in ex.isin
# Anwendung der Filter
df.in <- df[df$worst.num > max.rat.num & df$worst.num < min.rat.num & 
  df$piece <= max.piece & df$X1 != "rot" & df$price < max.price & df$yld > min.yld, ]
df.in <- df.in[complete.cases(df.in$X2),]

df.ex <- df[!(df$worst.num > max.rat.num & df$worst.num < min.rat.num & 
  df$piece <= max.piece & df$X1 != "rot" & df$price < max.price & df$yld > min.yld), ]
df.ex <- df.ex[complete.cases(df.ex$X2),]

df.final <- data.frame(Emittent=as.character(df.in$issuer), Kupon=df.in$coupon, Fälligkeit=as.Date(df.in$maturity),
                     Kurs=round(df.in$price,2), Rendite=round(df.in$yld,1), Duration=round(df.in$dur,1),
                     Spread=round(df.in$spread, 0), Nachrang=ifelse(df.in$subordinated,"Ja","Nein"), Rating=as.character(rating[match(df.in$worst.num, rating[,2]),1]), 
                     Währung=as.character(df.in$currency), Mindestvolumen=df.in$piece, ISIN=df.in[,2])
df.final.ex <- data.frame(Emittent=as.character(df.ex$issuer), Kupon=df.ex$coupon, Fälligkeit=as.Date(df.ex$maturity),
                       Kurs=round(df.ex$price,2), Rendite=round(df.ex$yld,1), Duration=round(df.ex$dur,1),
                       Spread=round(df.ex$spread, 0), Nachrang=ifelse(df.ex$subordinated,"Ja","Nein"), Rating=as.character(rating[match(df.ex$worst.num, rating[,2]),1]), 
                       Währung=as.character(df.ex$currency), Mindestvolumen=df.ex$piece, ISIN=df.ex[,2])
# Output
neu <- paste0(path, format(Sys.Date(), "%Y%m%d"), stamm, ".csv")
write.csv(df.final, file=neu, row.names=F, quote=F)
write.csv2(df.final, file=paste0(path, format(Sys.Date(), "%Y%m%d"), stamm, "_DE.csv"), row.names=F, quote=F)
# Find previous LBBW file
lbbw.old <- getLbbwFile()
df.final.old <- read.csv(lbbw.old)
# Nur alte und nur neue Titel
old.only <- df.final.ex[(df.final.ex$ISIN %in% df.final.old$ISIN), ]
new.only <- df.final[!(df.final$ISIN %in% df.final.old$ISIN), ]
if (nrow(old.only)==0) {
  print("old.only = 0 --> Keine Titel sind seit dem letzten Erstellungdatum entfallen!")
} else {
  print("old.only > 0 --> Folgende Titel sind seit dem letzten Erstellungdatum entfallen:")
  print(old.only)
  write.csv(old.only, file=paste0(path, format(Sys.Date(), "%Y%m%d"), "_EntfalleneTitelLBBW_EN.csv"), row.names=F, quote=F)
  write.csv2(old.only, file=paste0(path, format(Sys.Date(), "%Y%m%d"), "_EntfalleneTitelLBBW_DE.csv"), row.names=F, quote=F)
}

if (nrow(new.only)==0) {
  print("new.only = 0 --> Keine neuen Titel seit dem letzten Erstellungdatum!")
} else {
  print("new.only > 0 --> Folgende Titel wurden seit dem letzten Erstellungdatum aufgenommen:")
  print(new.only)
  write.csv(new.only, file=paste0(path, format(Sys.Date(), "%Y%m%d"), "_NeueTitelLBBW_EN.csv"), row.names=F, quote=F)
  write.csv2(new.only, file=paste0(path, format(Sys.Date(), "%Y%m%d"), "_NeueTitelLBBW_DE.csv"), row.names=F, quote=F)
}