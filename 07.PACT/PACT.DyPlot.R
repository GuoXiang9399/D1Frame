###############################################################################
###############################################################################
###############################################################################
#loading package
  library(tidyr)
  library(stringr)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(readxl)
###############################################################################
#Frame
  Frame <- read_xlsx("02.Frame/Frame.20200516.xlsx")
###############################################################################
#data
  FrameC <- unite(Frame,Year,Country,Clade,col="Abc",sep="&&&") %>% 
    group_by(Abc) %>%  dplyr::summarise(NUM=n()) %>%  
    separate(Abc,into=c("Year","Country","Clade"),sep="&&&")
  FrameC <- filter(FrameC, FrameC$Country=="Thailand"|
                           FrameC$Country=="Indonesia"|
                           FrameC$Country=="Brazil")
  FrameC$Year <- as.numeric(FrameC$Year)
###############################################################################
#plot
  ggplot(FrameC,aes(x=Year,y=NUM,fill=Clade))+theme_classic()+
    facet_grid(Country~.,scales="free")+
    geom_bar(stat="identity",color="black",width=0.8,size=0.005)+
    guides(fill=guide_legend(ncol=25,byrow=TRUE,keywidth = 0.2,keyheight = 0.01))+
    scale_x_continuous(limits = c(1979,2016),breaks = seq(1980,2016,by=1),expand = c(0,0))+
    scale_y_continuous(breaks = seq(0,60,by=10),limits = c(0,60))+
    theme(axis.text.x = element_text(angle = 45,size = 5,vjust = 0.5,hjust = 0.5), 
          axis.text.y = element_text(size=5),
          axis.line = element_line(size=0.25),
          axis.title = element_text(size=7),
          axis.ticks.length = unit(0.1,"lines"),
          axis.ticks = element_line(size=0.25),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.key.size = unit(0.1,"cm"),
          legend.text = element_text(size=2),
          strip.text=element_text(size=4),
          strip.background=element_blank())
ggsave("06.PACT/PACT.Plot/DYNA.Coun.pdf",width=16,height=14,units="cm")

###############################################################################
#function
CountryPlot <- function(i,List){
  Clade <- Frame
  Clade <- subset(Clade,Country==i)
  Clade <- unite(Clade,Country,Clade,Year,col="Abc",sep="%%%") %>%
    group_by(Abc) %>% summarise(Num=n()) %>%
    separate(Abc,into=c("Country","Clade","Year"),sep="%%%")
  Clade$Year <- as.numeric(Clade$Year)
  Clade <- subset(Clade, Year!="NA")
  Clade <- subset(Clade, Year>1980)
  #plot
  ggplot(Clade)+theme_bw()+xlab("")+ylab("")+
    geom_point(aes(Year,Clade,size=Num,fill=Clade),color="black",stroke=0.1,shape=21)+
    ggtitle(i)+
    scale_size(range = c(1, 5),limits = c(0,700))+
    scale_x_continuous(limits=c(1979,2016),expand=c(0,0), 
                       breaks=c(seq(1980,2016,by=1)))+
    scale_y_discrete(limits=List)+
    theme(plot.title = element_text(size=4),
          plot.background = element_rect(size=0.1),
          panel.background = element_rect(size=0.1,fill="#F8FCF0"),
          panel.grid = element_line(size=0.25,color="white"),
          panel.border = element_rect(size=0.1),
          axis.title = element_text(size=3),
          axis.text = element_text(size=2.5),
          axis.text.x = element_text(angle=45),
          axis.ticks = element_line(size=0.1),
          axis.ticks.length = unit(0.05,"lines"),
          legend.text = element_text(size=3),
          legend.title = element_text(size=3),
          legend.position = "none",
          legend.key.size = unit(0.20,"cm"))
  }
###############################################################################
#files  
ThailandList <- c("1A2","1A3","1A4","1A9","1A11","1A12","1A10","1B1","1A6","1B12","1B3","1B4","1B16",
                  "1B5","1D1","1G1","1H1","1B13","1J6","1B6","1B9","1E1",
                  "1H2","1H3","1H4","1H5","1J1","1J2","1J3","1J5","1G3",
                  "1K1","1G5","1M2","1L1","1F1","1J7","1G2","4A14","5B4","5C1")
IndonesiaList <- c("1D4","1M1","1J1","1M2","1J4","1J6","1M8","1L1","1M3",
                   "1M4","1J7","1L2","1M7","1L3","1M6","1M9","1M5",
                   "4F1","4F2","4H1","4E1","4E5","4B1","4E4","4F3","4B3","4E3",
                   "4D2","4E6","4E7","4E8",
                   "4D3","4F5","4B2","5C1")
BrazilList <- c("5K12","5N1","5N7","5N8","5L1","5N9","5L3","5P9","5R1","5L2")
###############################################################################
  p1 <- CountryPlot("Thailand",ThailandList)
  p2 <- CountryPlot("Indonesia",IndonesiaList)
  p3 <- CountryPlot("Brazil",BrazilList)
 plot_grid(p1,p2,p3,ncol=1)
ggsave("06.PACT/PACT.Plot/DYNA.Point.pdf",width=15.45,height=15,units="cm")
