analyze.ghg <- function(dat) {
  attach(dat)
  mm <- format(as.POSIXct(as.character(dat$date)), format="%m")
  Fetch <- Fetch70_1_1_1
  Time <- as.POSIXct(paste(date,time))
  Dat <- data.frame(date=Time,
                    H=H_1_1_1,
                    LE=LE_1_1_1,
                    Fc=Fc_1_1_1,
                    WS=WS_1_1_1,
                    WD=WD_1_1_1,
                    wdir=cut(WD_1_1_1,
                             breaks=seq(0,360,by=45)),
                    ustar=ustar_1_1_1,
                    Fetch,
                    month=mm)
  detach(dat)
  
  library(openair)
  windRose(mydata=Dat, ws="WS", wd="WD")
  readline()
  
  library(rpart)
  tree <- rpart(ustar ~ Fetch + wdir + month,
                data=Dat)
  plot(tree, compress=T, margin=0.1)
  text(tree, cex=0.7)
  readline()

  timeVariation(mydata=Dat,
                pollutant=c("H","LE"))
  readline()
  out <- timeVariation(mydata=Dat,
                pollutant=c("ustar"), plot=F)
###  print(out, subset=c("hour","month"))
  readline()
}
