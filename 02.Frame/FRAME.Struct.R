########################################################################################
####################################### Character ######################################
########################################################################################
#loading package
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
#loading file
  Frame <- read_excel("02.Frame/Frame.20200516.xlsx")

############################ Strains number of three levels ############################
  CountL1 <- group_by(Frame,Genotype) %>% dplyr::summarise(Count=n())
write.csv(CountL1,file="02.Frame/Frame.Data/Count.L1.csv")
  CountL2 <- unite(Frame,Genotype,Subgenotype,col="Abc",sep="#",remove=F)
  CountL2 <- group_by(CountL2,Abc) %>% dplyr::summarise(Count=n())
  CountL2 <- separate(CountL2,Abc,into=c("Genotype","Subgenotype"))
write.csv(CountL2,file="02.Frame/Frame.Data/Count.L2.csv")
  CountL2.t <- summary(aov(Count~Genotype,data=CountL2))
  CountL2.t <- data.frame(CountL2.t[[1]])
write.csv(CountL2.t,file="02.Frame/Frame.Data/Count.L2.t.csv") 
  CountL3 <- unite(Frame,Genotype,Clade,col="Abc",sep="#",remove=F)
  CountL3 <- group_by(CountL3,Abc) %>% dplyr::summarise(Count=n())
  CountL3 <- separate(CountL3,Abc,into=c("Genotype","Clade"))
write.csv(CountL3,file="02.Frame/Frame.Data/Count.L3.csv")
  CountL3.t <- summary(aov(Count~Genotype,data=CountL3))
  CountL3.t <- data.frame(CountL3.t[[1]])
write.csv(CountL3.t,file="02.Frame/Frame.Data/Count.L3.t.csv") 
  CountL3.filter <- filter(CountL3,CountL3$Count>1)

############################ Cluster number of three levels ############################
  Count.GC <- Frame[,c("Genotype","Clade")]
  Count.GC <- unique(Count.GC)
  Count.GC <- group_by(Count.GC,Genotype) %>% summarise(Count=n())
write.csv(Count.GC,"02.Frame/Frame.Data/Count.GC.csv")
  Count.SC <- Frame[,c("Subgenotype","Clade")]
  Count.SC <- unique(Count.SC)
  Count.SC <- group_by(Count.SC,Subgenotype) %>% summarise(Count=n())
write.csv(Count.SC,"02.Frame/Frame.Data/Count.SC.csv")
  NameL2 <- CountL2[,c("Genotype","Subgenotype")]
  Count.SC <- left_join(Count.SC,NameL2,by="Subgenotype")

#########################################################################################
p1 <- ggplot()+theme_bw()+xlab("")+ylab("clade number of subgenotype")+
    geom_boxplot(data=Count.SC,aes(x=Genotype,y=Count,fill=Genotype),
                 outlier.shape=NA,color="black")+
    geom_dotplot(data=Count.SC,aes(x=Genotype,y=Count),binaxis="y",dotsize=0.5,
                 binwidth=1,stackdir="center",alpha=0.8,fill="white")+
    theme(axis.text=element_text(size=6,colour="black"),
          axis.title=element_text(size=6,colour="black"),
          axis.ticks.length = unit(0.05,"lines"),
          legend.position = "none",
          text=element_text(color="black",size=4),
          panel.grid.minor=element_line(size=0.2),
          panel.grid.major=element_line(size=0.4))+
    scale_fill_manual(values=c("#F29A91","#9FCF6E","#00B7EF"))
p2 <- ggplot(CountL2)+theme_bw()+xlab("")+ylab("strain number of subgenotype")+
    geom_boxplot(data=CountL2,aes(x=Genotype,y=Count,fill=Genotype),
                 outlier.shape=NA,color="black",weight=0.25)+
    geom_dotplot(data=CountL2,aes(x=Genotype,y=Count),binaxis="y",dotsize=10,
                 binwidth=1,stackdir="center",alpha=0.8,fill="white",color="black")+
    theme(axis.text=element_text(size=6,colour="black"),
          axis.title=element_text(size=6,colour="black"),
          axis.ticks.length = unit(0.05,"lines"),
          legend.position = "none",
          text=element_text(color="black",size=4),
          panel.grid.minor=element_line(size=0.2),
          panel.grid.major=element_line(size=0.4))+
    scale_fill_manual(values=c("#F29A91","#9FCF6E","#00B7EF"))
p3 <- ggplot(CountL3)+theme_bw()+xlab("")+ylab("strain number of clade")+
    geom_boxplot(data=CountL3,aes(x=Genotype,y=log2(Count),fill=Genotype),
                 outlier.shape=NA,color="black")+
    geom_dotplot(data=CountL3.filter,aes(x=Genotype,y=log2(Count)),binaxis="y",dotsize=0.3,
                 binwidth=0.5,stackdir="center",alpha=0.8,fill="white")+
    theme(axis.text=element_text(size=6,colour="black"),
          axis.title=element_text(size=6,colour="black"),
          axis.ticks.length = unit(0.05,"lines"),
          legend.position = "none",
          text=element_text(color="black",size=4),
          panel.grid.minor=element_line(size=0.2),
          panel.grid.major=element_line(size=0.4))+
    scale_fill_manual(values=c("#F29A91","#9FCF6E","#00B7EF"))+
    scale_y_continuous(breaks = c(0,2,4,6,8))
 plot_grid(p1,p2,p3,ncol=3)
ggsave("02.Frame/Frame.Fig/Frame.Struc.pdf",width=16,height=5,units="cm")
