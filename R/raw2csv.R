read_cfg <- function(filecfg="dat/SPC.20151222.cfg") {
  cfg <- scan(filecfg, sep = "\n",what="")
  nn <- length(cfg)
  idx <- grep("\\[",cfg)
  blk <- gsub(x=cfg[idx],
              pattern = "[[:punct:]]", 
              replacement="")
  cfg <- cfg[(1:nn)[-idx]]
  cfg <- paste(rep(blk,
                   c(idx[2:4],nn+1)-idx[1:4]-1),
               cfg,sep=".")
  cfg <- gsub("=","<<-",cfg)
  cfg <- gsub("<<- ","<<-",cfg)
  cfg[1] <- paste0(gsub("<<-","<<-'",cfg[1]),"'")
  cfg <- cfg[-grep("\\\\",cfg)]
  for (i in 1:length(cfg)) eval(parse(text=cfg[i]))
}

read_raw <- function(fileraw="raw/test/80359.txt") {
  raw <- scan(fileraw, sep = "\n", what="")
  i1 <- grep(x = raw, pattern = ":x =")
  i2 <- grep(x = raw, pattern = "e8=")
  i2 <- intersect(i1+1, i2)
  i1 <- intersect(i2-1, i1)
  l1 <- gsub(pattern = "[A-Z]:", replacement = "", x = raw[i1])
  l2 <- gsub(pattern = "[A-Z]:", replacement = "", x = raw[i2])
  raw <- paste(l1,l2)
  raw <- raw[nchar(raw)==79]
  ff <- function(x) as.numeric(substring(x,(0:7)*10+4,(0:7)*10+9))
  out <- matrix(unlist(lapply(raw, FUN = ff)), ncol = 8, byrow = T)
  return(out)
}

convert_raw <- function(raw,
                        H2O.Channel,
                        H2O.Multiplier,
                        H2O.Offset,
                        CO2.Channel,
                        CO2.Multiplier,
                        CO2.Offset) {
  posi <- c(2,1,3,4,4+H2O.Channel,4+CO2.Channel)
  mult <- c(0.01,0.01,0.01,0.01,H2O.Multiplier,CO2.Multiplier)
  offs <- c(0,0,0,0,H2O.Offset,CO2.Offset)
  out <- t(t(raw[,posi]) * mult + offs)
  colnames(out) <- c("U","V","W","Tv","H2O","CO2")
  return(out)
}

single_raw2csv <- function(fileraw="78665.txt",
                           filecsv=paste0(format(as.numeric(gsub(".txt","",
                                                          basename(fileraw)))*3600+
                                            as.POSIXct(General.TimeBase),
                                          "%Y%m%d%H%M"),".csv"),
                           dir_raw="raw/test",
                           dir_csv="out") {
  cat(paste("reading file",fileraw,"..."),sep = " ")
  out <- convert_raw(read_raw(paste0(dir_raw,"/",fileraw)),
                     H2O.Channel,
                     H2O.Multiplier,
                     H2O.Offset,
                     CO2.Channel,
                     CO2.Multiplier,
                     CO2.Offset)
  write.table(out,file = paste0(dir_csv,"/",filecsv),
              sep=",",
              quote=F,
              row.names = F,
              na = "-9999.9")
  cat(paste("written file",filecsv),sep = "\n")
}

multi_raw2csv <- function(filecfg=select.list(dir("dat/",
                                                  pattern = "cfg",
                                                  full.names = T),
                                              title = "Configuration file?"),
                          dir_raw=paste0("raw/",dirname <- select.list(dir("raw/",full.names = F),
                                              title = "Raw data directory?")),
                          dir_csv=paste0("out/",dirname)) {
  read_cfg(filecfg)
  readline(prompt = paste0("I'll read raw data from directory ",dir_raw,
                           "\nand write CSV with converted data into directory ",dir_csv,
                           ". Is it ok? (y/n)")) -> ok
  if(ok %in% c("Y","y")) {
    if(!dir.exists(dir_csv)) {
      readline(prompt = paste0("The directory ",dir_csv,
                               " does not exist. Should I create it? (y/n)")) -> ok
      if(ok %in% c("Y","y")) {
        dir.create(dir_csv)
        cat(paste("Created directory",dir_csv),sep = "\n")
      }
    }
    if(length(dir(dir_csv))>0) {
      readline(prompt = paste0("The directory ",dir_csv,
                               " is not empty. Should I proceed anyway? (y/n)")) -> ok
    }
    if(ok %in% c("Y","y")) {
      files <- dir(path = dir_raw, pattern = "txt")
      start.time <- Sys.time()
      i=0
      for (fileraw in files) {
        i=i+1
        single_raw2csv(fileraw = fileraw, 
                       dir_raw = dir_raw, 
                       dir_csv = dir_csv)
        new.time <- Sys.time()
        end.time <- start.time+(new.time-start.time)/i*length(files)
        cat(paste("estimated time of completion:",end.time),sep = "\n")
        dm <- difftime(end.time,new.time,units="min")
        dh <- difftime(end.time,new.time,units="hour")
        if(dm>120) {
          dt <- dh; un<-"hour[s]"
        } else {
          dt <- dm; un<-"minute[s]"
        }
        cat(paste("remaining time: approx.", 
                  round(signif(dt,2),1), un), 
            sep = "\n")        
      }
    }
  }
}
