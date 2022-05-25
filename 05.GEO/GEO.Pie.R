###############################################################################
###############################################################################
###############################################################################
#loading package
  library(readxl)
  library(maps)
  library(scatterpie)
  library(dplyr)
  library(tidyr)
  library(cowplot)
#loading map file
  world <- map_data('world')
  GPS <- read_excel("04.GEO/GEO.Info/Site.GPS.xlsx")
  GPSr <- read_excel("04.GEO/GEO.Info/Reg.GPS.xlsx")
#laoding frame file  
  Frame <- read_excel("02.Frame/Frame.20200516.xlsx")
  FrameS <- filter(Frame, Frame$Country!="NA")
  Site.Info <- read_excel("04.GEO/GEO.info/Site.Info.xlsx")

###############################################################################
#Pie of DENV1-L1
  Clade <- unite(FrameS, Genotype, Country,col="Abc",sep="/")
  Clade <- group_by(Clade , Abc) %>% summarise(n()) %>%
    separate(Abc, into=c("Genotype", "Country"),sep="/")
  L1Name <- unique(Clade$Genotype)
  Clade <- spread(Clade, Genotype, "n()",fill=0)
write.csv(Clade,"04.GEO/GEO.Data/Coun.Pie.L1.csv")
  Clade <- left_join(GPS,Clade,by="Country")
  radius <- Clade[,c("I","IV","V")]
  radius <- apply(radius,1,sum)/2.5
  radius <- (log10((radius)*5))
  Clade <- cbind(Clade, radius)
  names(Clade) <- c("Country","lat","long","I","IV","V","radius")
p1 <- ggplot(world,aes(long,lat))+theme_void()+coord_equal()+
    geom_map(map=world,aes(map_id=region),fill="White", color="black",size=0.1)+
    geom_scatterpie(data=Clade,aes(x=long,y=lat,group=Country,r=radius),
                    cols=L1Name,size=0.1,alpha=0.9)+
    geom_scatterpie_legend(Clade$radius,x=-180, y=-100,n=5)+
    theme(legend.position=c(0.5,0.1),
          legend.key.size=unit(0.4,"cm"),
          legend.text.align=0,
          legend.text=element_text(size=8),
          panel.background=element_rect(fill="#E0F2F7"))+
    guides(fill=guide_legend(title = NULL,nrow=1))
#Pie of DENV1-L2
  Clade <- unite(FrameS, Subgenotype, Country,col="Abc",sep="/")
  Clade <- group_by(Clade , Abc) %>% summarise(n()) %>%
    separate(Abc,into=c("Subgenotype","Country"),sep="/")
  L2Name <- unique(Clade$Subgenotype)
  Clade <- spread(Clade, Subgenotype,"n()",fill=0)
write.csv(Clade,"04.GEO/GEO.Data/Coun.Pie.L2.csv")
  Clade <- left_join(GPS, Clade, by="Country")
  CladeGPS <- Clade[,-(1:3)]
  radius <- apply(CladeGPS,1,sum)
  radius <- (log10((radius)*5))
  Country <- Clade[,1]
  CladeFig <-data.frame(Country,radius)
  CladeFig <-merge(CladeFig,Clade, by="Country")
p2 <- ggplot(world,aes(long,lat))+theme_void()+coord_equal()+
    geom_map(map=world,aes(map_id=region),fill="white", color="black",size=0.1)+
    geom_scatterpie(data=CladeFig,aes(x=long,y=lat,group=Country,r=radius),
                    cols = L2Name,size=0.1,alpha=0.9)+
    geom_scatterpie_legend(CladeFig$radius, x=-180, y=-100,n=4)+
    #geom_text(data=CladeFig,aes(x=long,y=lat,label=Country),size=1)+
    theme(legend.position=c(0.5,0.1),
          legend.key.size=unit(0.2,"cm"),
          legend.text.align = 0,
          legend.text=element_text(size=4),
          panel.background=element_rect(fill="#E0F2F7"))+
    guides(fill=guide_legend(title = NULL,nrow=2))
#Pie of DENV1-L3
  Clade <- unite(FrameS, Clade, Country,col="Abc",sep="/")
  Clade <- group_by(Clade , Abc) %>% summarise(n()) %>%
    separate(Abc, into=c("Subgenotype", "Country"),sep="/")
  L3Name <- unique(Clade$Subgenotype)
  Clade <- spread(Clade,Subgenotype, "n()",fill=0)
write.csv(Clade,"04.GEO/GEO.Data/Coun.Pie.L3.csv")
  Clade <- left_join(GPS, Clade, by="Country")
  CladeGPS <- Clade[,-(1:4)]
  radius <- apply(CladeGPS,1,sum)/2.5
  radius <- (log10((radius)*5))
  Country <- Clade[,1]
  CladeFig <-data.frame(Country,radius)
  CladeFig <-merge(CladeFig,Clade, by="Country")
p3 <- ggplot(world,aes(long,lat))+theme_void()+coord_equal()+
    geom_map(map=world,aes(map_id=region),fill="White", color="black",size=0.1)+
    geom_scatterpie(data=CladeFig,aes(x=long,y=lat,group=Country,r=radius),
                    cols = L3Name,size=0.1,alpha=0.9)+
    geom_scatterpie_legend(CladeFig$radius, x=-180, y=-100,n=5)+
    theme(legend.position=c(0.5,0.08),
          legend.key.size=unit(0.2,"cm"),
          legend.text.align=0,
          legend.text=element_text(size=3),
          panel.background=element_rect(fill="#E0F2F7"))+
    guides(fill=guide_legend(title = NULL,nrow=8))
#merge
  plot_grid(p1,p2,p3,ncol=1,labels=c("a","b","c"))
#saving file
  ggsave("04.GEO/GEO.Plot/Pie.Country.pdf",width=22,height=35,units="cm")

###############################################################################
#Pie of DENV1-L1
  Clade <- left_join(FrameS,Site.Info,by="Country")
  Clade <- unite(Clade, Genotype, Region,col="Abc",sep="/")
  Clade <- group_by(Clade , Abc) %>% summarise(n()) %>%
    separate(Abc,into=c("Genotype", "Region"),sep="/")
  L1Name <- unique(Clade$Genotype)
  Clade <- spread(Clade, Genotype, "n()",fill=0)
write.csv(Clade,"04.GEO/GEO.Data/Reg.Pie.L1.csv") 
  Clade <- left_join(GPSr,Clade,by="Region")
  radius <- Clade[,-(1:3)]
  radius <- apply(radius,1,sum)/2.5
  radius <- log2((radius)+1)
  Clade <- cbind(Clade, radius)
p1 <-  ggplot(world,aes(long,lat))+theme_void()+
  geom_map(map=world,aes(map_id=region),fill="White", color="black",size=0.05)+
  geom_scatterpie(data=Clade,aes(x=long,y=lat,group=Region,r=radius),
                  cols = L1Name,size=0.1)+coord_equal()+
  geom_scatterpie_legend(Clade$radius, x=-180, y=-110,n=4)+
  theme(legend.position = c(0.5,0.1),
        legend.key.size = unit(0.4,"cm"),
        legend.text=element_text(size=8), 
        legend.text.align = 0)+
  theme(panel.background = element_rect(fill = "#E0F2F7"))+
  guides(fill=guide_legend(title = NULL,nrow=1))
#Pie of DENV1-L2
  Clade <- left_join(FrameS,Site.Info,by="Country")
  Clade <- unite(Clade, Subgenotype, Region,col="Abc",sep="/")
  Clade <- group_by(Clade , Abc) %>% summarise(n()) %>%
    separate(Abc,into=c("Subgenotype", "Region"),sep="/")
  L2Name <- unique(Clade$Subgenotype)
  Clade <- spread(Clade,Subgenotype, "n()",fill=0)
write.csv(Clade,"04.GEO/GEO.Data/Reg.Pie.L2.csv") 
  Clade <- left_join(GPSr,Clade,by="Region")
  radius <- Clade[,-(1:3)]
  radius <- apply(radius,1,sum)/2.5
  radius <- log2((radius)+1)
  Clade <- cbind(Clade, radius)
p2 <-  ggplot(world,aes(long,lat))+theme_void()+
  geom_map(map=world,aes(map_id=region),fill="White", color="black",size=0.05)+
  geom_scatterpie(data=Clade,aes(x=long,y=lat,group=Region,r=radius),
                  cols = L2Name,size=0.1)+coord_equal()+
  geom_scatterpie_legend(Clade$radius, x=-180, y=-110,n=4)+
  theme(legend.position = c(0.5,0.1), 
        legend.key.size = unit(0.3,"cm"),
        legend.text=element_text(size=6),
        legend.text.align = 0,
        panel.background = element_rect(fill = "#E0F2F7"))+
  guides(fill=guide_legend(title = NULL,nrow=2))
#Pie of DENV1-L3
  Clade <- left_join(FrameS,Site.Info,by="Country")
  Clade <- unite(Clade, Clade, Region,col="Abc",sep="/")
  Clade <- group_by(Clade , Abc) %>% summarise(n()) %>%
    separate(Abc,into=c("Subgenotype", "Region"),sep="/") 
  L3Name <- unique(Clade$Subgenotype)
  Clade <- spread(Clade, Subgenotype, "n()",fill=0)
write.csv(Clade,"04.GEO/GEO.Data/Reg.Pie.L3.csv") 
  Clade <- left_join(GPSr,Clade,by="Region")
  radius <- Clade[,-(1:3)]
  radius <- apply(radius,1,sum)/2.5
  radius <- log2((radius)+1)
  Clade <- cbind(Clade, radius)
p3 <-  ggplot(world,aes(long,lat))+theme_void()+
  geom_map(map=world,aes(map_id=region),fill="White", color="black",size=0.05)+
  geom_scatterpie(data=Clade,aes(x=long,y=lat,group=Region,r=radius),
                  cols = L3Name,size=0.1)+coord_equal()+
  geom_scatterpie_legend(Clade$radius, x=-180, y=-110,n=4)+
  theme(legend.position=c(0.5,0.08),
        legend.key.size=unit(0.2,"cm"),
        legend.text.align=0,
        legend.text=element_text(size=3),
        panel.background=element_rect(fill="#E0F2F7"))+
  guides(fill=guide_legend(title = NULL,nrow=8))
#merge
  plot_grid(p1,p2,p3,ncol=1,labels=c("a","b","c"))
#saving file
  ggsave("04.GEO/GEO.Plot/Pie.Region.pdf",width=22,height=35,units="cm")
  
