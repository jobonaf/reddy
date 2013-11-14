###file <- select.list(dir(pattern=".csv"), title="select file")

read.ghg <- function (file) {
  dat <- read.csv(file,
                  skip=3,head=F)
  nv <- ncol(dat)
  dum <- scan(file,n=3*ncol(dat),sep=",",what="")
  vars <- gsub(" ","",dum[(nv+1):(nv*2)])
  units <- gsub(" ","",dum[(nv*2+1):(nv*3)])
  
  colnames(dat) <- vars
  dat[dat==-9999.] <- NA
  
  return(dat)
}



