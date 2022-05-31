###############################################################################
###############################################################################
###############################################################################
#loading package
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(RColorBrewer)
  library(cowplot)
  library(stringr)
###############################################################################
#Frame
  Frame <- read_xlsx("02.Frame/Frame.20200516.xlsx")
  getPalette <- colorRampPalette(brewer.pal(9, "Spectral"))
###############################################################################
#clade list
#Single country source of clade   "4E8","5N7","1D2","5P8"
 List <- list("1A12","1B8","1B9","1B12","1C","1D1","1D3","1E","1F","1G1","1H1",
              "1H3","1H4","1H5","1I","1J1","1J2","1J3","1J4","1J5","1J6",
              "1J7","1K","1L","1M1","1M8","4A14","4B3","4C3","4D3","4E4",
              "4H","5C","5L3","5M7","5O1","5O2","5O3","5O4","5P9","5Q1",
              "5Q2","5R")
#loading trunk file  
  Trunk <- data.frame(statistic=c(),time=c(),lower=c(),mean=c(),upper=c(),clade=c(),Percent=c())
for (i in List) {
  Clade <- read.table(file=paste("06.PACT/PACT.Log/",i,".out.skylines",sep=""),header=T)
  Clade$statistic <- stringr::str_remove(Clade$statistic,"pro_")
  Clade$clade <- paste(i)
  Clade <- plyr::ddply(Clade,"time",transform,Percent=mean/sum(mean)*100)
  Trunk <- rbind(Trunk,Clade)}
 write.csv(Trunk,"06.PACT/PACT.Data/PACT.Trunk.csv")
###############################################################################
#Single country source of clade   "5N7","4E8","1D2","5P8"
  L3Name <- read_excel("02.Frame/Frame.Info/TrunkCladeName.xlsx")
  TrunkM <- filter(Trunk,Trunk$mean>=0.5)
  TrunkM <- filter(TrunkM, TrunkM$statistic!="NA")
write.csv(TrunkM,"06.PACT/PACT.Data/PACT.Filter.csv")
###############################################################################
  Time.Clade <- Frame[,c("Clade","Year")]
  Time.Clade1 <- aggregate(Time.Clade$Year,Time.Clade[,c("Clade")],min)
    names(Time.Clade1) <- c("Clade","Min")
  Time.Clade2 <- aggregate(Time.Clade$Year,Time.Clade[,c("Clade")],max)
    names(Time.Clade2) <- c("Clade","Max")
  Time.Clade <- merge(Time.Clade1,Time.Clade2,by="Clade")
  TimeM <- filter(Time.Clade,Time.Clade$Clade=="5N7"|Time.Clade$Clade=="4E8"|
                    Time.Clade$Clade=="1D2"|Time.Clade$Clade=="5P8")
  TimeM$Country <- c("Vietnam","Indonesia","Brazil","Colombia")
  Min <- TimeM$Min-0.05
  Max <- TimeM$Max+0.05
  Clade <- TimeM$Clade
  Country <- TimeM$Country
  TrunkAddition <- data.frame(statistic=c(),time=c(),lower=c(),mean=c(),upper=c(),clade=c())
for (i in 1:4) {MinM <- Min[i]
  MaxM <- Max[i]
  CladeM <- Clade[i]
  CountryM <- Country[i]
  time <- seq(from=paste(MinM),to=paste(MaxM),by=0.1)
  TrunkAdd<-data.frame(statistic=paste(CountryM),lower=1,mean=1,upper=1,clade=paste(CladeM))
  TrunkTime <- data.frame(time=time)
  TrunkAdd <- cbind(TrunkAdd,TrunkTime)
  TrunkAddition <- rbind(TrunkAddition,TrunkAdd)}
#   
  TrunkAddition$Percent <- 100
  TrunkData <- rbind(TrunkM,TrunkAddition)
write.csv(TrunkData,"06.PACT/PACT.Data/PACT.TrunkAdd.csv")
#
  TrunkData <- left_join(TrunkData,L3Name,by="clade")
  TrunkData$NUM <- 1
  TrunkData$time <- as.numeric(TrunkData$time)
  TrunkData <- unite(TrunkData,statistic,clade,col="Abc",sep="%%%",remove=F)
###############################################################################
#trunk list
  TrunkList <-  group_by(TrunkData,Abc) %>% summarise(NUM=n())
  TrunkList <- subset(TrunkList,NUM<10)
  TrunkData <- subset(TrunkData,Abc!="Australia%%%1K" &
                                Abc!="Nicaragua%%%5O2" &
                                Abc!="Paraguay%%%5R" &
                                Abc!="Sri_Lanka%%%1H5" &
                                Abc!="USA.Hawaii%%%4H" &
                                Abc!="Vietnam%%%1D1")
write.csv(TrunkData,"06.PACT/PACT.Data/PACT.TrunkMerge.csv")












