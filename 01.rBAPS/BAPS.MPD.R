#########################################################################################
#########################################################################################
#########################################################################################
#loading library
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(ggExtra)
  library(ggbeeswarm)
  library(RColorBrewer)
#########################################################################################
#Genotype I
  D1GI.L1 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L1_Within_Avg.csv")
  D1GI.L1$Level <- "1"
  D1GI.L2 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L2_Within_Avg.csv")
  D1GI.L2$Level <- "2"
  D1GI.L3 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L3_Within_Avg.csv")
  D1GI.L3$Level <- "3"
  D1GI.L4 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L4_Within_Avg.csv")
  D1GI.L4$Level <- "4"
  D1GI.L5 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L5_Within_Avg.csv")
  D1GI.L5$Level <- "5"
  D1GI.L6 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L6_Within_Avg.csv")
  D1GI.L6$Level <- "6"
  D1GI.L7 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L7_Within_Avg.csv")
  D1GI.L7$Level <- "7"
  D1GI.L8 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L8_Within_Avg.csv")
  D1GI.L8$Level <- "8"
  D1GI.L9 <- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L9_Within_Avg.csv")
  D1GI.L9$Level <- "9"
  D1GI.L10<- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L10_Within_Avg.csv")
  D1GI.L10$Level <- "10"
  D1GI.L11<- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L11_Within_Avg.csv")
  D1GI.L11$Level <- "11"
  D1GI.L12<- read.csv("01.rBAPS/BAPS.MPD/D1GI_E_Group_L12_Within_Avg.csv")
  D1GI.L12$Level <- "12"
#Genotype IV
  D1GIV.L1 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L1_Within_Avg.csv")
  D1GIV.L1$Level <- "1"
  D1GIV.L2 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L2_Within_Avg.csv")
  D1GIV.L2$Level <- "2"
  D1GIV.L3 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L3_Within_Avg.csv")
  D1GIV.L3$Level <- "3"
  D1GIV.L4 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L4_Within_Avg.csv")
  D1GIV.L4$Level <- "4"
  D1GIV.L5 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L5_Within_Avg.csv")
  D1GIV.L5$Level <- "5"
  D1GIV.L6 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L6_Within_Avg.csv")
  D1GIV.L6$Level <- "6"
  D1GIV.L7 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L7_Within_Avg.csv")
  D1GIV.L7$Level <- "7"
  D1GIV.L8 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L8_Within_Avg.csv")
  D1GIV.L8$Level <- "8"
  D1GIV.L9 <- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L9_Within_Avg.csv")
  D1GIV.L9$Level <- "9"
  D1GIV.L10<- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L10_Within_Avg.csv")
  D1GIV.L10$Level <- "10"
  D1GIV.L11<- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L11_Within_Avg.csv")
  D1GIV.L11$Level <- "11"
  D1GIV.L12<- read.csv("01.rBAPS/BAPS.MPD/D1GIV_E_Group_L12_Within_Avg.csv")
  D1GIV.L12$Level <- "12"
#Genotype V  
  D1GV.L1 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L1_Within_Avg.csv")
  D1GV.L1$Level <- "1"
  D1GV.L2 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L2_Within_Avg.csv")
  D1GV.L2$Level <- "2"
  D1GV.L3 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L3_Within_Avg.csv")
  D1GV.L3$Level <- "3"
  D1GV.L4 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L4_Within_Avg.csv")
  D1GV.L4$Level <- "4"
  D1GV.L5 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L5_Within_Avg.csv")
  D1GV.L5$Level <- "5"
  D1GV.L6 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L6_Within_Avg.csv")
  D1GV.L6$Level <- "6"
  D1GV.L7 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L7_Within_Avg.csv")
  D1GV.L7$Level <- "7"
  D1GV.L8 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L8_Within_Avg.csv")
  D1GV.L8$Level <- "8"
  D1GV.L9 <- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L9_Within_Avg.csv")
  D1GV.L9$Level <- "9"
  D1GV.L10<- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L10_Within_Avg.csv")
  D1GV.L10$Level <- "10"
  D1GV.L11<- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L11_Within_Avg.csv")
  D1GV.L11$Level <- "11"  
  D1GV.L12<- read.csv("01.rBAPS/BAPS.MPD/D1GV_E_Group_L12_Within_Avg.csv")
  D1GV.L12$Level <- "12"
###############################################################################
#data merge
  D1GI.MPD <- rbind(D1GI.L1,D1GI.L2,D1GI.L3,D1GI.L4,D1GI.L5,D1GI.L6,
                  D1GI.L7,D1GI.L8,D1GI.L9,D1GI.L10,D1GI.L11,D1GI.L12)
  D1GI.MPD <- subset(D1GI.MPD,d!="n/c")
  D1GI.MPD$d <- as.numeric(D1GI.MPD$d)
  D1GI.MPD$Level <- as.numeric(D1GI.MPD$Level)
  D1GI.MPD$Genotype <- "I"
  
  D1GIV.MPD <- rbind(D1GIV.L1,D1GIV.L2,D1GIV.L3,D1GIV.L4,D1GIV.L5,D1GIV.L6,
                  D1GIV.L7,D1GIV.L8,D1GIV.L9,D1GIV.L10,D1GIV.L11,D1GIV.L12)
  D1GIV.MPD <- subset(D1GIV.MPD,d!="n/c")
  D1GIV.MPD$d <- as.numeric(D1GIV.MPD$d)
  D1GIV.MPD$Level <- as.numeric(D1GIV.MPD$Level)
  D1GIV.MPD$Genotype <- "IV"
  
  D1GV.MPD <- rbind(D1GV.L1,D1GV.L2,D1GV.L3,D1GV.L4,D1GV.L5,D1GV.L6,
                  D1GV.L7,D1GV.L8,D1GV.L9,D1GV.L10,D1GV.L11,D1GV.L12)
  D1GV.MPD <- subset(D1GV.MPD,d!="n/c")
  D1GV.MPD$d <- as.numeric(D1GV.MPD$d)
  D1GV.MPD$Level <- as.numeric(D1GV.MPD$Level)
  D1GV.MPD$Genotype <- "V"
  D1.MPD <- rbind(D1GI.MPD,D1GIV.MPD,D1GV.MPD)
  
###############################################################################
  D1.MPD.Select <- subset(D1.MPD, Level<=4&Level>=1)
  D1GI.MPD.Select <- subset(D1GI.MPD, Level<=4&Level>=1) 
  D1GIV.MPD.Select <- subset(D1GIV.MPD, Level<=4&Level>=1) 
  D1GV.MPD.Select <- subset(D1GV.MPD, Level<=4&Level>=1) 
  
  D1.MPD.Select$Level <- as.character(D1.MPD.Select$Level)
  D1GI.MPD.Select$Level <- as.character(D1GI.MPD.Select$Level)
  D1GIV.MPD.Select$Level <- as.character(D1GIV.MPD.Select$Level)
  D1GV.MPD.Select$Level <- as.character(D1GV.MPD.Select$Level)
#plot  
  p1 <- ggplot(D1.MPD.Select,aes(x=d,fill=Level))+#
    geom_density(alpha=.7,size=0.1)+
    facet_grid(Level~.)+
    geom_vline(xintercept = 0.008,size=0.1)+
    geom_vline(xintercept = 0.020,size=0.1)+
    scale_x_continuous(limits = c(0,0.04),breaks = c(0,0.010,0.020,0.030,0.040))+
    theme(legend.position = "none",
          axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"),
          strip.text = element_text(size=6,angle=90),
          strip.background = element_blank(),
          strip.placement = "inside")
  p2 <- ggplot(D1GI.MPD.Select,aes(x=d,fill=Level))+
    geom_density(alpha=.7,size=0.1)+
    facet_grid(Level~.)+
    geom_vline(xintercept = 0.008,size=0.1)+
    geom_vline(xintercept = 0.020,size=0.1)+
    scale_x_continuous(limits = c(0,0.04),breaks = c(0,0.010,0.020,0.030,0.040))+
    theme(legend.position = "none",
          axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"),
          strip.text = element_text(size=6,angle=90),
          strip.background = element_blank(),
          strip.placement = "inside")
  p3 <-  ggplot(D1GIV.MPD.Select,aes(x=d,fill=Level))+
    geom_density(alpha=.7,size=0.1)+
    facet_grid(Level~.)+
    geom_vline(xintercept = 0.008,size=0.1)+
    geom_vline(xintercept = 0.020,size=0.1)+
    scale_x_continuous(limits = c(0,0.04),breaks = c(0,0.010,0.020,0.030,0.040))+
    theme(legend.position = "none",
          axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"),
          strip.text = element_text(size=6,angle=90),
          strip.background = element_blank(),
          strip.placement = "inside")
  p4 <-  ggplot(D1GV.MPD.Select,aes(x=d,fill=Level))+
    geom_density(alpha=.7,size=0.1)+
    facet_grid(Level~.)+
    geom_vline(xintercept = 0.008,size=0.1)+
    geom_vline(xintercept = 0.020,size=0.1)+
    scale_x_continuous(limits = c(0,0.04),breaks = c(0,0.010,0.020,0.030,0.040))+
    theme(legend.position = "none",
          axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"),
          strip.text = element_text(size=6,angle=90),
          strip.background = element_blank(),
          strip.placement = "inside")
  plot_grid(p1,p2,p3,p4,ncol=4)
  ggsave("01.rBAPS/BAPS.Plot/BAPS.MPDs.pdf",width=16,height=6,units="cm")
###############################################################################
#data 
  D1.MPD$Level <- as.character(D1.MPD$Level)
  D1GI.MPD$Level <- as.character(D1GI.MPD$Level)
  D1GIV.MPD$Level <- as.character(D1GIV.MPD$Level)
  D1GV.MPD$Level <- as.character(D1GV.MPD$Level)
#plot  
  p1 <- ggplot(D1.MPD.Select)+theme_bw()+
    geom_boxplot(aes(Level,d,fill=Level),size=0.25,outlier.alpha = 0)+
    geom_jitter(aes(Level,d),size=0.3,alpha=0.8,color="gray10",stroke=0.01)+ 
    geom_hline(yintercept = 0.0081,size=0.1)+
    geom_hline(yintercept = 0.020,size=0.1)+
    #scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
    theme(axis.text=element_text(size=4,colour="black"),
          axis.title=element_text(size=6,colour="black"),
          axis.ticks.length = unit(0.05,"lines"),
          legend.position = "none",
          text=element_text(color="black",size=4),
          panel.grid.minor=element_line(size=0.2),
          panel.grid.major=element_line(size=0.4))#+
  #scale_y_continuous(limits = c(0,0.03))
  p2 <- ggplot(D1GI.MPD.Select)+theme_bw()+
    geom_boxplot(aes(Level,d,fill=Level),size=0.25,outlier.alpha = 0)+
    geom_jitter(aes(Level,d),size=0.3,alpha=0.8,color="gray10",stroke=0.01)+ 
    geom_hline(yintercept = 0.0081,size=0.1)+
    geom_hline(yintercept = 0.020,size=0.1)+
    #scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
    theme(axis.text=element_text(size=4,colour="black"),
          axis.title=element_text(size=6,colour="black"),
          axis.ticks.length = unit(0.05,"lines"),
          legend.position = "none",
          text=element_text(color="black",size=4),
          panel.grid.minor=element_line(size=0.2),
          panel.grid.major=element_line(size=0.4))#+
  #scale_y_continuous(limits = c(0,0.03))
  p3 <- ggplot(D1GIV.MPD.Select)+theme_bw()+
    geom_boxplot(aes(Level,d,fill=Level),size=0.25,outlier.alpha = 0)+
    geom_jitter(aes(Level,d),size=0.3,alpha=0.8,color="gray10",stroke=0.01)+ 
    geom_hline(yintercept = 0.0081,size=0.1)+
    geom_hline(yintercept = 0.020,size=0.1)+
    #scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
    theme(axis.text=element_text(size=4,colour="black"),
          axis.title=element_text(size=6,colour="black"),
          axis.ticks.length = unit(0.05,"lines"),
          legend.position = "none",
          text=element_text(color="black",size=4),
          panel.grid.minor=element_line(size=0.2),
          panel.grid.major=element_line(size=0.4))#+
  #scale_y_continuous(limits = c(0,0.03))
  p4 <- ggplot(D1GV.MPD.Select)+theme_bw()+
    geom_boxplot(aes(Level,d,fill=Level),size=0.25,outlier.alpha = 0)+
    geom_jitter(aes(Level,d),size=0.3,alpha=0.8,color="gray10",stroke=0.01)+ 
    geom_hline(yintercept = 0.0081,size=0.1)+
    geom_hline(yintercept = 0.020,size=0.1)+
    #scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
    theme(axis.text=element_text(size=4,colour="black"),
          axis.title=element_text(size=6,colour="black"),
          axis.ticks.length = unit(0.05,"lines"),
          legend.position = "none",
          text=element_text(color="black",size=4),
          panel.grid.minor=element_line(size=0.2),
          panel.grid.major=element_line(size=0.4))#+
    #scale_y_continuous(limits = c(0,0.03))
  plot_grid(p1,p2,p3,p4,ncol=4,labels = c("D1","D1GI","D1GIV","D1GV"),label_size = 6)
  ggsave("01.rBAPS/BAPS.Plot/BAPS.BoxPlot.pdf",width=16,height=5,units="cm")
###############################################################################
#data
  D1.MPD.L1 <- subset(D1.MPD, Level==1)
  D1GI.MPD.L1 <- subset(D1GI.MPD, Level==1)
  D1GIV.MPD.L1 <- subset(D1GIV.MPD, Level==1) 
  D1GV.MPD.L1 <- subset(D1GV.MPD, Level==1)
  D1.MPD.L4 <- subset(D1.MPD, Level==4)
  D1GI.MPD.L4 <- subset(D1GI.MPD, Level==4)
  D1GIV.MPD.L4 <- subset(D1GIV.MPD, Level==4) 
  D1GV.MPD.L4 <- subset(D1GV.MPD, Level==4)
#plot
  p1 <- ggplot(D1.MPD.L1,aes(x=d))+
    stat_ecdf(size=0.1)+
    geom_hline(yintercept = 0.9,size=0.1)+
    theme(axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"))
  p2 <- ggplot(D1GI.MPD.L1,aes(x=d))+
    stat_ecdf(size=0.1)+
    scale_x_continuous(breaks = c(0.005,0.010,0.015,0.020))+
    geom_hline(yintercept = 0.9,size=0.1)+
    theme(axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"))
  p3 <- ggplot(D1GIV.MPD.L1,aes(x=d))+
    stat_ecdf(size=0.1)+
    geom_hline(yintercept = 0.9,size=0.1)+
    theme(axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"))
  p4 <- ggplot(D1GV.MPD.L1,aes(x=d))+
    stat_ecdf(size=0.1)+
    geom_hline(yintercept = 0.9,size=0.1)+
    theme(axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"))
  pI <- plot_grid(p1,p2,p3,p4,ncol=2,labels = c("D1","D1GI","D1GIV","D1GV"),label_size = 6)
 #ggsave("01.rBAPS/BAPS.Plot/BAPS.CCplot2.pdf",width=8,height =6,units = "cm" )
  p1 <- ggplot(D1.MPD.L4,aes(x=d))+
    stat_ecdf(size=0.1)+
    scale_x_continuous(breaks = c(0.000,0.010,0.020))+
    geom_hline(yintercept = 0.9,size=0.1)+
    theme(axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"))
  p2 <- ggplot(D1GI.MPD.L4,aes(x=d))+
    stat_ecdf(size=0.1)+
    geom_hline(yintercept = 0.9,size=0.1)+
    theme(axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"))
  p3 <- ggplot(D1GIV.MPD.L4,aes(x=d))+
    stat_ecdf(size=0.1)+
    geom_hline(yintercept = 0.9,size=0.1)+
    theme(axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"))
  p4 <- ggplot(D1GV.MPD.L4,aes(x=d))+
    stat_ecdf(size=0.1)+
    scale_x_continuous(breaks = c(0.000,0.010,0.020))+
    geom_hline(yintercept = 0.9,size=0.1)+
    theme(axis.text = element_text(size=6),
          axis.title = element_text(size=8),
          axis.ticks = element_line(size=0.25),
          axis.ticks.length = unit(0.05,"lines"))
  pII <- plot_grid(p1,p2,p3,p4,ncol=2,labels = c("D1","D1GI","D1GIV","D1GV"),label_size = 6)
 #ggsave("01.rBAPS/BAPS.Plot/BAPS.CCplot3.pdf",width=8,height =6,units = "cm" )
  plot_grid(pI,pII,ncol=2)
 ggsave("01.rBAPS/BAPS.Plot/BAPS.CCplot.pdf",width=16,height =6,units = "cm" )


  
  
  
  
  


  