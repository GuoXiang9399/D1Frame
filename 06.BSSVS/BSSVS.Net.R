########################################################################################
############################### Phylogeographical analyses #############################
########################################################################################
#loading library
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(geosphere)
  library(stringr)
  library(ggrepel)
  library(scatterpie)

#Subgenotype list
List <- c("1A","1B","1C","1D","1E","1F","1G","1H","1I","1J","1K","1L","1M",
          "4A","4B","4C","4D","4E","4F","4G","4H",
          "5B","5C","5K","5L","5M","5N","5O","5P","5Q","5R")

###################################### BSSVS Data #######################################
#loading BSSVS data  
  BSSVS.Data <- data.frame(FROM=c(),TO=c(),BF=c(),POSTERIOR=c(),Clade=c())
for (i in List) {
  BSSVS <- read.table(paste("05.BSSVS/BSSVS.Data/",i,".BSSVS.txt",sep=""),header=T)
  names(BSSVS) <- c("FROM","TO","BF","POSTERIOR")
  BSSVS$Subgenotype <- paste(i)
  BSSVS.Data <- rbind(BSSVS.Data,BSSVS)}  

#filter
  BSSVS.Data <- filter(BSSVS.Data,BSSVS.Data$FROM!="NA" & BSSVS.Data$TO!="NA")
  BSSVS.Data <- filter(BSSVS.Data,BSSVS.Data$BF>=6)

#Define level
BSSVS.Data <- mutate(BSSVS.Data,BFlevel = case_when(
  BSSVS.Data$BF >= 10000                        ~ "L5",
  BSSVS.Data$BF  < 10000 & BSSVS.Data$BF >= 100 ~ "L4",
  BSSVS.Data$BF  < 100   & BSSVS.Data$BF >=  30 ~ "L3",
  BSSVS.Data$BF  < 30    & BSSVS.Data$BF >=  10 ~ "L2",
  BSSVS.Data$BF  < 10                           ~ "L1" ))

###################################### Site GPS Data ####################################  
#loadling file  
  Site.GPS <- read_excel("04.GEO/GEO.Info/Site.Nets.xlsx")
  Site.GPS <- Site.GPS[,c("Country","lat","long")]

#GPS data
   names(Site.GPS) <- c("FROM","FROM_lat","FROM_long")
  BSSVS.Data <- left_join(BSSVS.Data,Site.GPS,by="FROM")
   names(Site.GPS) <- c("TO","TO_lat","TO_long")
  BSSVS.Data <- left_join(BSSVS.Data,Site.GPS,by="TO")

################################# Migration Rate Data ###################################  
#loading files
BSSVS.Rate <- data.frame(Rate.mean=c(),Rate.median=c(),Route=c(),Subgenotype=c())
for (i in List) {
  BSSVS <- read.table(paste("05.BSSVS/BSSVS.Data/",i,".BSSVS.rate",sep=""),header=T)
  BSSVS <- t.data.frame(BSSVS)
  BSSVS <- BSSVS[,c(1,5)]
  BSSVS <- as.data.frame(BSSVS)
  BSSVS$route <- row.names(BSSVS)
  names(BSSVS) <- c("Rate.mean","Rate.median","Route") 
  BSSVS <- BSSVS[-1,]
  BSSVS$Subgenotype <- paste(i)
  BSSVS.Rate <- rbind(BSSVS.Rate,BSSVS) }

#route
  BSSVS.Rate$Route <- str_remove(BSSVS.Rate$Route,"states.rates.")
  BSSVS.Rate$Route <- str_replace(BSSVS.Rate$Route,".Hawaii","*Hawaii")
  BSSVS.Rate <- unite(BSSVS.Rate,Route,Subgenotype,col="Label",sep="%%%")

#Data merge 
  BSSVS.Data <- unite(BSSVS.Data,FROM,TO,col="Route",remove=F,sep=".")
  BSSVS.Data$Route <- str_replace(BSSVS.Data$Route,".Hawaii","*Hawaii")
  BSSVS.Data <- unite(BSSVS.Data,Route,Subgenotype,col="Label",sep="%%%",remove=F)
  BSSVS.Data <- left_join(BSSVS.Data,BSSVS.Rate,by="Label") 
  BSSVS.Data$Rate.median <- as.numeric(BSSVS.Data$Rate.median)

###################################### BSSVS Site #######################################
#loading file
  names(Site.GPS) <- c("Country","lat","long")

#Data   
  BSSVS.Site <- BSSVS.Data[,c("FROM","TO")] 
  BSSVS.Site <- data.frame(Country=unlist(BSSVS.Site))
  BSSVS.Site <- group_by(BSSVS.Site,Country) %>% summarise(Connection=n())
  BSSVS.Site <- left_join(BSSVS.Site,Site.GPS,by="Country")

###################################### BSSVS Plot #######################################  
  BSSVS.Circle <- BSSVS.Data[,c("FROM","TO","Subgenotype")]
  BSSVS.Circle <- gather(BSSVS.Circle,"FROM","TO",key="Dire",value="Country")
  BSSVS.Circle <- unite(BSSVS.Circle,Subgenotype,Country,col="Abc",sep="%%%") %>%
    group_by(Abc) %>% summarise(NUM=n()) %>% separate(Abc,into=c("Subgenotype","Country"),sep="%%%")
  BSSVS.CirclE <- spread(BSSVS.Circle,Subgenotype,NUM,fill=0)
  BSSVS.CirclE <- left_join(BSSVS.CirclE,Site.GPS,by="Country")
  BSSVS.CircLE <- BSSVS.CirclE[,c("Country","lat","long")]
  
###################################### BSSVS Plot #######################################  
#plot
ggplot()+coord_equal()+theme_bw()+
  geom_curve(data=BSSVS.Data,aes(x=FROM_long,y=FROM_lat,xend=TO_long,yend=TO_lat,
             color=BFlevel,size=Rate.median),alpha=0.85,curvature = 0.0)+
  geom_scatterpie(data=BSSVS.CirclE,aes(x=long,y=lat,group=Country),
                  cols=unique(BSSVS.Circle$Subgenotype),size=0.1,alpha=0.9)+
  geom_point(data=BSSVS.Site,aes(x=long,y=lat),fill="white",color="black",
             size=4.5,shape=21,stroke=0.05)+
  geom_text(data=BSSVS.Site,aes(x=long,y=lat,label=Country),size=0.8)+
  #scale_color_manual(values = c("#D1E5F0","#92C5DE","#5D9CD9","#0593C3","#0571B0"))+
  scale_color_manual(values = c("gray90","gray80","gray70","gray50","gray20"))+
  scale_size(range = c(0.005,0.6))+
  scale_x_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40,50,60,70))+
  scale_y_continuous(breaks = c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40))+
    theme(legend.key.size = unit(0.2,"cm"),
          legend.text=element_text(size=2),
          legend.text.align = 0,
          legend.title = element_blank(),
          axis.title = element_blank())
ggsave("05.BSSVS/BSSVS.Plot/BSSBS.Nets.pdf",width=16,height=8,units="cm")

###################################### BSSVS Plot #######################################  
#library(networkD3)
#BSSVS.Nets <- BSSVS.Data[,c("FROM","TO")]
#simpleNetwork(BSSVS.Nets,fontSize = 12)

