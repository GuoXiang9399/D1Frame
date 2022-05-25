########################################################################################
########################################################################################
########################################################################################
#loading library
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)

#loading file
  Frame <- read_excel("02.Frame/Frame.20200516.xlsx")           

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
  #geom_line(data=DYNA.Stain,aes(Year,Freq),color="red")+
  geom_line(data=DYNA.Country,aes(MIN,Freq),color="blue")+
  geom_smooth(data=DYNA.Country,aes(MIN,Freq),color="blue",se=F,span = 1)+
  geom_line(data=DYNA.L2,aes(MIN,Freq),color="green")+
  geom_smooth(data=DYNA.L2,aes(MIN,Freq),color="green",se=F,span = 1)+
  geom_line(data=DYNA.L3,aes(MIN,Freq),color="pink")+
  geom_smooth(data=DYNA.L3,aes(MIN,Freq),color="pink",se=F,span = 1)+
  scale_x_continuous(limits=c(1980,2017),breaks=c(1950,1955,1960,
    1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015))+
  scale_color_hue(l=90)+
  theme(axis.text = element_text(size=4))
  
#ggsave("03.BEAST/BEAST.Plot/BEAST.DYNA2.point.pdf",width=11.7,height=4,units="cm")

  
  
  
#########################################################################################
#
  FrameM <- mutate(Frame,Year5 = case_when(
    Frame$Year >=1940 & Frame$Year <1945 ~ "1945",
    Frame$Year >=1945 & Frame$Year <1950 ~ "1950",
    Frame$Year >=1950 & Frame$Year <1955 ~ "1955",
    Frame$Year >=1955 & Frame$Year <1960 ~ "1960",
    Frame$Year >=1960 & Frame$Year <1965 ~ "1965",
    Frame$Year >=1965 & Frame$Year <1970 ~ "1970",
    Frame$Year >=1970 & Frame$Year <1975 ~ "1975",
    Frame$Year >=1975 & Frame$Year <1980 ~ "1980",
    Frame$Year >=1980 & Frame$Year <1985 ~ "1985",
    Frame$Year >=1985 & Frame$Year <1990 ~ "1990",
    Frame$Year >=1990 & Frame$Year <1995 ~ "1995",
    Frame$Year >=1995 & Frame$Year <2000 ~ "2000",
    Frame$Year >=2000 & Frame$Year <2005 ~ "2005",
    Frame$Year >=2005 & Frame$Year <2010 ~ "2010",
    Frame$Year >=2010 & Frame$Year <2015 ~ "2015",
    Frame$Year >=2015 & Frame$Year <2020 ~ "2020",
  ))
  
#########################################################################################
#strain  
  DYNA.Stain <- group_by(FrameM,Year5) %>% summarise(NUM=n())
  DYNA.Stain <- mutate(DYNA.Stain,Freq=NUM/4997,Cums=cumsum(Freq))
    DYNA.Stain$Year5 <- as.numeric(DYNA.Stain$Year5) 
  
#Country
  DYNA.Country <- group_by(FrameM,Country) %>% summarise(MAX=max(Year5),MIN=min(Year5))
  DYNA.Country <- group_by(DYNA.Country,MIN) %>% summarise(NUM=n())
  DYNA.Country <- mutate(DYNA.Country,Freq=NUM/79,Cums=cumsum(Freq))
    DYNA.Country$MIN <- as.numeric(DYNA.Country$MIN)
  
#Subgenotype
  DYNA.L2 <- group_by(FrameM,Subgenotype) %>% summarise(MAX=max(Year5),MIN=min(Year5))
  DYNA.L2 <- group_by(DYNA.L2,MIN) %>% summarise(NUM=n())
  DYNA.L2 <- mutate(DYNA.L2,Freq=NUM/39,Cums=cumsum(Freq))
    DYNA.L2$MIN <- as.numeric(DYNA.L2$MIN)
  
#Clade
  DYNA.L3 <- group_by(FrameM,Clade) %>% summarise(MAX=max(Year5),MIN=min(Year5))
  DYNA.L3 <- group_by(DYNA.L3,MIN) %>% summarise(NUM=n())
  DYNA.L3 <- mutate(DYNA.L3,Freq=NUM/208,Cums=cumsum(Freq))
    DYNA.L3$MIN <- as.numeric(DYNA.L3$MIN)
  
#########################################################################################
#plot 
p1 <- ggplot(data=DYNA.Stain,aes(Year5,NUM))+theme_bw()+
  xlab("")+ylab("strain")+
  geom_col(color="black",fill="#CA6951",size=0.25,width = 3.5)+
  geom_smooth(se = F,span = 0.50,color="gray",size=0.75,alpha=0.75)+
  scale_x_continuous(breaks = c(1940,1945,1950,1955,1960,1965,1970,1975,1980,1985,
       1990,1995,2000,2005,2010,2015,2020))+
  theme(axis.text = element_text(size = 4),
        axis.text.x.bottom = element_text(size = 4, angle =45),
        axis.title = element_text(size = 6))
  
p2 <- ggplot(data=DYNA.Country,aes(MIN,NUM))+theme_bw()+
  xlab("")+ylab("country")+
  geom_col(color="black",fill="#2BB572",size=0.25,width = 3.5)+
  geom_smooth(se = F,span = 0.50,color="gray",size=0.75)+
  scale_x_continuous(breaks = c(1940,1945,1950,1955,1960,1965,1970,1975,1980,1985,
                                1990,1995,2000,2005,2010,2015,2020))+
  theme(axis.text = element_text(size = 4),
        axis.text.x.bottom = element_text(size = 4, angle =45),
        axis.title = element_text(size = 6))

p3 <- ggplot(data=DYNA.L2,aes(MIN,NUM))+theme_bw()+
  xlab("")+ylab("subgenotype")+
  geom_col(color="black",fill="#1074BC",size=0.25,width = 3.5)+
  geom_smooth(se = F,span = 0.50,color="gray",size=0.75)+
  scale_x_continuous(breaks = c(1940,1945,1950,1955,1960,1965,1970,1975,1980,1985,
                                1990,1995,2000,2005,2010,2015,2020))+
  theme(axis.text = element_text(size = 4),
        axis.text.x.bottom = element_text(size = 4, angle =45),
        axis.title = element_text(size = 6))

p4 <- ggplot(data=DYNA.L3,aes(MIN,NUM))+theme_bw()+
  xlab("")+ylab("clade")+
  geom_col(color="black",fill="#EE1E7B",size=0.25,width = 3.5)+
  geom_smooth(se = F,span = 0.50,color="gray",size=0.75)+
  scale_x_continuous(breaks = c(1940,1945,1950,1955,1960,1965,1970,1975,1980,1985,
                                1990,1995,2000,2005,2010,2015,2020))+
  theme(axis.text = element_text(size = 4),
        axis.text.x.bottom = element_text(size = 4, angle =45),
        axis.title = element_text(size = 6))

#cowplot  
  plot_grid(p1,p2,p3,p4,ncol = 2)
ggsave("03.TRAITS/TRAITS.PLOT/CUMs.pdf",width=16,height=8,units="cm") 
  
  
  
