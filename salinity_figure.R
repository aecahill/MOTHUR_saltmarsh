#makes figure of salinity measures 2018 data 

salinity<-read.table("C:/Users/aecsk/Desktop/marshsalinity.txt",header=TRUE)

library(ggplot2)

ggplot(data=salinity, aes(x=Site,y=Salinity,group=Month))+
  geom_line(aes(color=Month))+
  geom_point(aes(color=Month),size=4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("green","darkorange2","gold","black","purple","red"))+
  scale_colour_manual(values=c("black","gray32","gray56"))+
  theme(axis.text.x= element_text(size=10))+
  theme(axis.text.y= element_text(size=10))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nSite")+ylab("Salinity (ppt)\n")