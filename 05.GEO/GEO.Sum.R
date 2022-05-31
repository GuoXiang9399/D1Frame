########################################################################################
###################################### Summary #########################################
########################################################################################
#loading library
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
#loading file
  Frame <- read_excel("02.Frame/Frame.20200516.xlsx")
########################################################################################
#transform
  Clade <- unite(Frame,Clade,Country,col="Abc",sep="/")
  Clade <- group_by(Clade, Abc) %>% summarise(n())
  Clade <- Clade[,1]
  Clade <- separate(Clade,Abc,into=c("Unite","Country"),sep="/")
  Clade <- group_by(Clade, Country)%>% summarise(NUM=n())
  Clade <- filter(Clade,Clade$NUM>3)
#pplot
ggplot(Clade,aes(reorder(Country,-NUM),NUM))+geom_col(size=0.25,fill="gray",color="black")+
  xlab("")+ylab("number of clades")+theme_bw()+
  theme(axis.text=element_text(size=4,colour="black"),text=element_text(color="black",size=4),
        axis.text.x=element_text(angle = 90,size=4,colour="black"),
        legend.position = "none",axis.ticks.length = unit(0.05,"lines"),
        panel.grid.minor=element_line(size=0.2),panel.grid.major=element_line(size=0.4))
ggsave("04.Geograph/GEO.Data/GEO.Sum.pdf",width=12,height=4,units="cm")
  
