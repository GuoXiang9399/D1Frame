###############################################################################
###############################################################################
###############################################################################
#loading library
  library(ggplot2)
  library(cowplot)
###############################################################################
#loading file
  BAPS.result <- read.csv("01.rBAPS/BAPS.Data/BAPS.result.csv")
###############################################################################
#ggplot
 p1 <- ggplot(BAPS.result)+guides(fill=F)+xlab("Depth")+ylab("number of cluster")+
    geom_line(aes(Depth,Count,color=Genotype),size=1)+theme_classic()+
    scale_x_continuous(limits = c(1,12),breaks = c(1:12))+
    theme(axis.title=element_text(size=8),
          axis.text=element_text(size=6,color="black"))
 p2 <- ggplot(BAPS.result,aes(Depth,-lml.Mean,color=Genotype))+guides(fill=F)+
    geom_line(size=1)+xlab("Depth")+ylab("likelihood")+theme_classic()+
    scale_y_log10()+scale_x_continuous(limits = c(1,12),breaks = c(1:12))+
    theme(axis.title=element_text(size=8),
          axis.text=element_text(size=6,color="black"))
 p3 <- ggplot(BAPS.result,aes(Depth,Simpson.Mean,color=Genotype))+xlab("Depth")+ylab("Simpson index")+
    geom_line(size=1)+theme_classic()+guides(fill=F)+
    scale_x_continuous(limits = c(1,12),breaks = c(1:12))+
    scale_y_continuous(limits = c(0.1,0.7),breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7))+
    theme(axis.title=element_text(size=8),
          axis.text=element_text(size=6,color="black"))
 p4 <- ggplot(BAPS.result,aes(Depth,Shannon.Mean,color=Genotype))+
    geom_line(size=1)+theme_classic()+xlab("Depth")+ylab("Shannon-Wiener index")+
    scale_x_continuous(limits = c(1,12),breaks = c(1:12))+guides(fill=F)+
    scale_y_continuous(limits = c(0,1.6),breaks = c(0,0.4,0.8,1.2,1.6))+
    theme(axis.title=element_text(size=8),
          axis.text=element_text(size=6,color="black"))
 plot_grid(p1,p2,p3,p4,ncol=2)
 ggsave("01.rBAPS/BAPS.Plot/BAPS.Plot.pdf",width=20,height=16,units="cm") 





