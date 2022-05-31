###############################################################################
###############################################################################
###############################################################################
#loading package
  library(readxl)
  library(tidyr)
  library(stringr)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(RColorBrewer)
###############################################################################
#loading file
  Frame <- read_xlsx("02.Frame/Frame.20200516.xlsx")
#clade list
#Single country source of clade ( "4E8","5N7","1D2","5P8")
  List <- list("1A12","1B8","1B9","1B12","1C","1D1","1D3","1E","1F","1G1","1H1",
               "1H3","1H4","1H5","1I","1J1","1J2","1J3","1J4","1J5","1J6",
               "1J7","1K","1L","1M1","1M8","4A14","4B3","4C3","4D3","4E4",
               "4H","5C","5L3","5M7","5O1","5O2","5O3","5O4","5P9","5Q1",
               "5Q2","5R")
  List2 <- list("1A12","1B8","1B9","1B12","1C","1D1","1D3","1E","1F","1G1","1H1",
              "1H3","1H4","1H5","1I","1J1","1J2","1J3","1J4","1J5","1J6",
              "1J7","1K","1L","1M1","1M8","4A14","4B3","4C3","4D3","4E4",
              "4H","5C","5L3","5M7","5O1","5O2","5O3","5O4","5P9","5Q1",
              "5Q2","5R","4E8","5N7","1D2","5P8")
###############################################################################
#loading trunk file  
  Trunk <- data.frame(statistic=c(),time=c(),lower=c(),mean=c(),upper=c(),clade=c(),Percent=c())
  for (i in List) {
    Clade <- read.table(file=paste("06.PACT/PACT.Log/",i,".out.skylines",sep=""),header=T)
    Clade$statistic <- stringr::str_remove(Clade$statistic,"pro_")
    Clade$clade <- paste(i)
    Clade <- plyr::ddply(Clade,"time",transform,Percent=mean/sum(mean)*100)
    Trunk <- rbind(Trunk,Clade)}
#trunk filter
  TrunkM <- filter(Trunk,Trunk$mean>=0.5)
  TrunkM <- unite(TrunkM,statistic,clade,col="Abc",sep="%%%",remove=F)
  TrunkM <- subset(TrunkM,Abc!="Australia%%%1K" &
                       Abc!="Nicaragua%%%5O2" &
                       Abc!="Paraguay%%%5R" &
                       Abc!="Sri_Lanka%%%1H5" &
                       Abc!="USA.Hawaii%%%4H" &
                       Abc!="Vietnam%%%1D1" &
                       Abc!="NA%%%1J2" &
                       Abc!="NA%%%1H1")
 write.csv(TrunkM,"06.PACT/PACT.Data/PACT.Ocurr.csv")
########################################################################################
#Time Subgenotype
  Time.Subgenotype <- Frame[,c("Subgenotype","Year")]
  Time.Subgenotype1 <- aggregate(Time.Subgenotype$Year,Time.Subgenotype[,c("Subgenotype")],min)
   names(Time.Subgenotype1) <- c("Clade","Min")
  Time.Subgenotype2 <- aggregate(Time.Subgenotype$Year,Time.Subgenotype[,c("Subgenotype")],max)
   names(Time.Subgenotype2) <- c("Clade","Max")
  Time.Subgenotype <- merge(Time.Subgenotype1,Time.Subgenotype2,by="Clade")
write.csv(Time.Subgenotype,"06.PACT/PACT.Data/Time.Subgenotype.csv")

#Time Clade
  Time.Clade <- Frame[,c("Clade","Year")]
  Time.Clade1 <- aggregate(Time.Clade$Year,Time.Clade[,c("Clade")],min)
   names(Time.Clade1) <- c("Clade","Min")
  Time.Clade2 <- aggregate(Time.Clade$Year,Time.Clade[,c("Clade")],max)
   names(Time.Clade2) <- c("Clade","Max")
  Time.Clade <- merge(Time.Clade1,Time.Clade2,by="Clade")
write.csv(Time.Clade,"06.PACT/PACT.Data/Time.Clade.csv")

#Time cbind 
  Time <- rbind(Time.Subgenotype,Time.Clade)
  TimeList <- data.frame(Clade=unlist(List2))
  Time <- left_join(TimeList,Time,by="Clade")

#########################################################################################
#Plot
ggplot()+theme_classic()+coord_flip()+ylab("time(year)")+xlab("")+
  annotate("rect",xmin="1A12",xmax="5R",ymin=1990,ymax=1995,fill="#EFEFEF")+
  annotate("rect",xmin="1A12",xmax="5R",ymin=2000,ymax=2005,fill="#EFEFEF")+
  annotate("rect",xmin="1A12",xmax="5R",ymin=2010,ymax=2015,fill="#EFEFEF")+
  geom_segment(data=Time,aes(yend=Max,y=Min,x=Clade,xend=Clade),color="gray",size=0.3)+
  geom_point(data=TrunkM,aes(clade,time,color=statistic),size=0.2,shape=15)+
  geom_point(data=Time,aes(Clade,Max),size=0.18,color="gray40")+
  geom_point(data=Time,aes(Clade,Min),size=0.18,color="gray40")+
  scale_y_continuous(breaks=c(seq(1985,2015,by=5)))+
  theme(axis.text=element_text(size=4),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size=0.25),
        axis.title=element_text(size=4),
        axis.ticks.length = unit(0.05,"lines"),
        axis.ticks.y=element_blank(), 
        panel.grid=element_line(size=0.0),
        legend.position=c(0.1,0.3),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.15,"cm"),
        legend.text=element_text(size=5))+
  guides(color=guide_legend(title = NULL,ncol=1))
ggsave("06.PACT/PACT.Plot/OcPlot.pdf",width=12,height=17,units="cm")  
  
