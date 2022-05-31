########################################################################################
################################## PACT Trunk ##########################################
########################################################################################
#loading package
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(RColorBrewer)
  library(cowplot)
###############################################################################
#Frame
  Frame <- read_xlsx("02.Frame/Frame.20200516.xlsx")
  TrunkData <- read.csv("06.PACT/PACT.Data/PACT.TrunkMerge.csv")
  getPalette <- colorRampPalette(brewer.pal(9, "Spectral"))
###############################################################################
#  
  FrameM <- group_by(Frame,Year) %>% summarise(NUM=n())
  FrameM <- mutate(FrameM, Percent = NUM/10)
#
  Trunk.Country.Num <- unite(TrunkData,statistic,time,col="Abc",sep="%%%")
  Trunk.Country.Num <- data.frame(Abc=unique(Trunk.Country.Num$Abc)) %>%
    separate(Abc,into=c("Country","Time"),sep="%%%") %>%
    group_by(Time) %>% summarise(NUM=n())
  Trunk.Country.Num$Time <- as.numeric(Trunk.Country.Num$Time)
#Trunk for clade
  Trunk.Clade <- unite(TrunkData, clade,time,col="Abc",sep="%%%") %>%
    group_by(Abc) %>% summarise(NUM=n()) %>%
    separate(Abc,into=c("clade","time"),sep="%%%")
  Trunk.Clade <- spread(Trunk.Clade,clade,NUM,fill=0)
  Trunk.Clade <- gather(Trunk.Clade,
     "1A12","1B12","1B8","1B9","1C","1D1","1D2","1D3","1E","1F",     
     "1G1","1H1","1H3","1H4","1H5","1I","1J1","1J2","1J3","1J4", 
     "1J5","1J6","1J7","1K","1L","1M1","1M8","4A14","4B3","4C3",
     "4D3","4E4","4E8","4H","5C","5L3","5M7","5N7","5O1","5O2",
     "5O3","5O4","5P8","5P9","5Q1","5Q2","5R",
     key="clade",value="NUM")
  Trunk.Clade$time <- as.numeric(Trunk.Clade$time)
#Trunk for country
  Trunk.Country <- TrunkData
  Trunk.Country <- unite(Trunk.Country, statistic,time,col="Abc",sep="%%%") %>%
    group_by(Abc) %>% summarise(NUM=n())  %>%
    separate(Abc,into=c("statistic","time"),sep="%%%")
#
  Trunk.Country <- spread(Trunk.Country,statistic,NUM,fill=0)
  Trunk.Country <- gather(Trunk.Country,
     "Argentina","Australia","Brazil","Cambodia","China","Colombia","Costa_Rica",
     "East_Timor","Ecuador","Fiji","French_Polynesia","Haiti","India","Indonesia",
     "Japan","Laos","Mexico","Myanmar","New_Caledonia","Nicaragua","Paraguay",
     "Philippines","Puerto_Rico","Saint_Barthelemy","Singapore","Sri_Lanka",
     "Thailand","USA","USA.Hawaii","Venezuela","Vietnam",
                                                key="Country",value="NUM")
  Trunk.Country$time <- as.numeric(Trunk.Country$time)
###############################################################################
#plot
p1 <- ggplot(Trunk.Country)+theme_bw()+xlab("")+ylab("")+
  geom_area(aes(time,NUM,fill=Country),position = position_fill(),
            color="gray20",size=0.05,alpha=.6)+
  scale_x_continuous(expand=c(0,0),limits=c(2005,2015),
                     breaks=c(seq(1995,2015,by=1)))+
  scale_y_continuous(expand=c(0,0),
                     breaks=c(seq(0,1,by=0.1)))+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=5),
        axis.ticks = element_line(size=0.1),
        axis.ticks.length = unit(0.05,"lines"),
        panel.grid = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.15,"cm"),
        legend.text=element_text(size=4))+
  guides(fill=guide_legend(title = NULL,nrow=3))
#plot
p2 <- ggplot(Trunk.Clade)+theme_bw()+xlab("")+ylab("")+
  geom_area(aes(time,NUM,fill=clade),position = position_fill())+
  geom_line(data=FrameM,aes(x=Year,y=Percent/100))+
  scale_x_continuous(expand=c(0,0),limits=c(2005,2015),
                     breaks=c(seq(1995,2015,by=1)))+
  scale_y_continuous(expand=c(0,0),
                     breaks=c(seq(0,1,by=0.1)))+
  guides(fill=guide_legend(ncol=1,byrow=TRUE,keywidth = 0.2,keyheight = 0.01))+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=5),
        axis.ticks = element_line(size=0.1),
        axis.ticks.length = unit(0.05,"lines"),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.15,"cm"),
        legend.text=element_text(size=4))+
  scale_fill_manual(values = getPalette(length(unique(TrunkData$clade))))+
  guides(fill=guide_legend(title = NULL,nrow=3))
 plot_grid(p1,p2,ncol=1,labels=c("a","b"),label_size=8)
ggsave("06.PACT/PACT.Plot/Trunk.Fig.pdf",width=13,height=16,units="cm")

