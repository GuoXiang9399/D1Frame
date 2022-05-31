########################################################################################
#################################### Country Cluster ###################################
########################################################################################
#loading package
  library(cluster)
  library(factoextra)
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(cowplot)
  library(ggrepel)
#loading file
  Frame <- read_excel("02.Frame/Frame.20200516.xlsx")
  Site.Info <- read_excel("04.GEO/GEO.Info/Site.Info.xlsx")

###################################### Genotype #########################################
#Genotype data
  Clade <- left_join(Frame,Site.Info,by="Country")
  Clade <- filter(Clade,NUM>10)
  Clade <- unite(Clade, Genotype, Country,col="Abc",sep="/") %>% group_by(Abc) %>%
    summarise(n()) %>% separate(Abc, into=c("Subgenotype", "Country"),sep="/") %>%
    spread(Subgenotype, "n()",fill=0)
  Clade <- data.frame(Clade[,-1], row.names = c(unlist(Clade[,1])))
  Clade <- as.matrix(Clade)
  Clade <- prop.table(Clade, 1)

#pam
p1 <- fviz_nbclust(Clade)
  cluster.res <- pam(Clade, 3)
p2 <- fviz_cluster(cluster.res,data=Clade,ellipse.type = "none",
                   repel=T,main = "",shape = 21,ggtheme = theme_bw(),labelsize=4)
pI <- plot_grid(p1,p2,ncol=2)

#extract cluster data
  Cluster.L1 <-   as.data.frame(cluster.res$cluster)    
  Cluster.L1$Country <- row.names(Cluster.L1)
   names(Cluster.L1) <- c("cluster","Country")
write.csv(Cluster.L1,"04.GEO/GEO.Data/Cluster.L1.csv")

#################################### subgenotype ########################################
#Subgenotype data 
  Clade.L2 <- left_join(Frame,Cluster.L1,by="Country")
  
####################################### Asia ############################################
#Subgenotype cluster=2 data
  Clade.Asia <- filter(Clade.L2,cluster==2)
  Clade.Asia <- filter(Clade.Asia,Country!="Sri_Lanka")
  Clade.Asia <- unite(Clade.Asia,Subgenotype,Country,col="Abc",sep="/") %>% group_by(Abc) %>%
    summarise(n()) %>% separate(Abc, into=c("Subgenotype", "Country"),sep="/") %>%
    spread(Subgenotype, "n()",fill=0)
  Clade.Asia <- data.frame(Clade=Clade.Asia[,-1], row.names = c(unlist(Clade.Asia[,1])))
  Clade.Asia <- as.matrix(Clade.Asia)
  Clade.Asia <- prop.table(Clade.Asia, 1) 
  
#pam
p3 <- fviz_nbclust(Clade.Asia,pam)
  cluster.res <- pam(Clade.Asia,2)
p4 <- fviz_cluster(cluster.res,data=Clade.Asia,ellipse.type = "none",
                   repel=T,main = "",shape = 21,ggtheme = theme_bw(),labelsize=5)
pII <- plot_grid(p3,p4,ncol=2)

####################################### America #########################################
#Subgenotype cluster=1 data
  Clade.America <- filter(Clade.L2,cluster==1)
  Clade.America <- unite(Clade.America,Subgenotype,Country,col="Abc",sep="/") %>% group_by(Abc) %>%
    summarise(n()) %>% separate(Abc, into=c("Subgenotype", "Country"),sep="/") %>%
    spread(Subgenotype, "n()",fill=0)
  Clade.America <- data.frame(Clade=Clade.America[,-1], row.names = c(unlist(Clade.America[,1])))
  Clade.America <- as.matrix(Clade.America)
  Clade.America <- prop.table(Clade.America, 1)
  
#pam
p5 <- fviz_nbclust(Clade.America,pam)
  cluster.res <- pam(Clade.America,4)  
p6 <- fviz_cluster(cluster.res,data=Clade.America,ellipse.type = "none",
                   repel=T,main = "",shape = 21,ggtheme = theme_bw(),labelsize=5)
pIII <- plot_grid(p5,p6,ncol=2)

######################################## Oceania ########################################
#Subgenotype cluster=3 data
  Clade.Oceania <- filter(Clade.L2,cluster==3)
  Clade.Oceania <- filter(Clade.Oceania,Country!="East_Timor")
  Clade.Oceania <- unite(Clade.Oceania,Subgenotype,Country,col="Abc",sep="/") %>% group_by(Abc) %>%
    summarise(n()) %>% separate(Abc, into=c("Subgenotype", "Country"),sep="/") %>%
    spread(Subgenotype, "n()",fill=0)
  Clade.Oceania <- data.frame(Clade=Clade.Oceania[,-1], row.names = c(unlist(Clade.Oceania[,1])))
  Clade.Oceania <- as.matrix(Clade.Oceania)
  Clade.Oceania <- prop.table(Clade.Oceania, 1)
  
#pam  
p7 <- fviz_nbclust(Clade.Oceania,pam)
  cluster.res <- pam(Clade.Oceania, 3)
p8<-  fviz_cluster(cluster.res,data=Clade.Oceania,ellipse.type = "none",
                   repel=T,main = "",shape = 21,ggtheme = theme_bw(),labelsize=5)
pIV <- plot_grid(p7,p8,ncol=2)  
  
######################################## Plot ###########################################
  plot_grid(pI,pII,pIII,pIV,ncol=1)
ggsave("04.GEO/GEO.Plot/Cluster.pdf",width=16,height=24,units="cm")
  plot_grid(p2,p4,p6,p8,ncol=2)
ggsave("04.GEO/GEO.Plot/Cluster2.pdf",width=16,height=12,units="cm")
  