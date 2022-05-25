########################################################################################
#################################### Bayesian Skyline ##################################
########################################################################################
#loading library
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(MASS)
  library(scales)

#loading file
  Frame <- read_excel("02.Frame/Frame.20200516.xlsx")           
  Skyline <- read.table("03.BEAST/BEAST.Skyline/Select.Skyline.txt",header=T)
  Skyline <- mutate(Skyline,height=Upper-Lower)
  
#########################################################################################
#strain  
  DYNA.Stain <- group_by(Frame,Year) %>% summarise(NUM=n())
  DYNA.Stain <- mutate(DYNA.Stain,Freq=NUM/4997,Cums=cumsum(Freq))
  
#Country
  DYNA.Country <- group_by(Frame,Country) %>% summarise(MAX=max(Year),MIN=min(Year))
  DYNA.Country <- group_by(DYNA.Country,MIN) %>% summarise(NUM=n())
  DYNA.Country <- mutate(DYNA.Country,Freq=NUM/79,Cums=cumsum(Freq))
  
#Subgenotype
  DYNA.L2 <- group_by(Frame,Subgenotype) %>% summarise(MAX=max(Year),MIN=min(Year))
  DYNA.L2 <- group_by(DYNA.L2,MIN) %>% summarise(NUM=n())
  DYNA.L2 <- mutate(DYNA.L2,Freq=NUM/39,Cums=cumsum(Freq))

#Clade
  DYNA.L3 <- group_by(Frame,Clade) %>% summarise(MAX=max(Year),MIN=min(Year))
  DYNA.L3 <- group_by(DYNA.L3,MIN) %>% summarise(NUM=n())
  DYNA.L3 <- mutate(DYNA.L3,Freq=NUM/208,Cums=cumsum(Freq))
  
  
#########################################################################################
#Plot
  ggplot()+theme_bw()+xlab("")+ylab("")+
    geom_line(data=DYNA.Stain,aes(Year,Cums*100),color="red")+
    geom_line(data=DYNA.Country,aes(MIN,Cums*100),color="blue")+
    geom_line(data=DYNA.L2,aes(MIN,Cums*100),color="green")+
    geom_line(data=DYNA.L3,aes(MIN,Cums*100),color="pink")+
    geom_line(data=Skyline,aes(Time,Lower/20),size=0.1)+
    geom_line(data=Skyline,aes(Time,Upper/20),size=0.05)+
    geom_line(data=Skyline,aes(Time,Median/20),size=0.1)+
    scale_x_continuous(limits=c(1944,2017),breaks=c(1950,1955,1960,
      1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015))+
    scale_color_hue(l=90)+
    theme(axis.text = element_text(size=4))
  ggsave("03.BEAST/BEAST.Plot/BEAST.DYNA.pdf",width=11.7,height=4,units="cm")
    
  #Plot
  ggplot()+theme_bw()+xlab("")+ylab("")+
    #geom_line(data=DYNA.Stain,aes(Year,Freq),color="red")+
    geom_point(data=DYNA.Country,aes(MIN,Freq),color="blue")+
    geom_smooth(data=DYNA.Country,aes(MIN,Freq),color="blue",se=T,span = 1)+
    geom_point(data=DYNA.L2,aes(MIN,Freq),color="green")+
    geom_smooth(data=DYNA.L2,aes(MIN,Freq),color="green",se=T,span = 1)+
    geom_point(data=DYNA.L3,aes(MIN,Freq),color="pink")+
    geom_smooth(data=DYNA.L3,aes(MIN,Freq),color="pink",se=T,span = 1)+
    scale_x_continuous(limits=c(1980,2017),breaks=c(1950,1955,1960,
                                                    1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015))+
    scale_color_hue(l=90)+
    theme(axis.text = element_text(size=4))
  ggsave("03.BEAST/BEAST.Plot/BEAST.DYNA2.point.pdf",width=11.7,height=4,units="cm")
  
  
  