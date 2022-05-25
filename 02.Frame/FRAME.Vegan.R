###############################################################################
###############################################################################
###############################################################################
#loading package
  library(readxl)
  library(vegan)
  library(dplyr)
  library(tidyr)
  library(reshape2)
###############################################################################
#loading file
  Frame <- read_excel("02.Frame/Frame.20200516.xlsx")
###############################################################################
#Subgenotype
Diversity <- unite(Frame, Country, Subgenotype, col="Abc",sep="/") %>% group_by(Abc) %>% 
  summarise(n()) %>% separate(Abc, into=c("Country","Subgenotype"),sep="/") %>%
  acast(Subgenotype~Country, value.var = "n()",fill=0)
  Shannon.Wiener <- diversity(Diversity, index = "shannon")
  Shannon.Wiener <- data.frame(Shannon.Wiener)
  Shannon.Wiener$Subgenotype <- row.names(Shannon.Wiener) 
  Simpson <- diversity(Diversity,index = "simpson")
  Simpson <- data.frame(Simpson)
  Simpson$Subgenotype <- row.names(Simpson)
  Diversity <- merge(Shannon.Wiener,Simpson,by="Subgenotype")
write.csv(Diversity,file="02.Frame/Frame.Data/Vegan.L2.csv")
###############################################################################
#Clade
Diversity <- unite(Frame, Country, Clade, col="Abc",sep="/") %>% group_by(Abc) %>% 
  summarise(n()) %>% separate(Abc, into=c("Country","Clade"),sep="/") %>%
  acast(Clade~Country, value.var = "n()",fill=0)
  Shannon.Wiener <- diversity(Diversity, index = "shannon")
  Shannon.Wiener <- data.frame(Shannon.Wiener)
  Shannon.Wiener$Clade <-  row.names(Shannon.Wiener) 
  Simpson <- diversity(Diversity,index = "simpson")
  Simpson <- data.frame(Simpson)
  Simpson$Clade <- row.names(Simpson)
  Diversity <- merge(Shannon.Wiener,Simpson,by="Clade")
write.csv(Diversity,file="02.Frame/Frame.Data/Vegan.L3.csv")
