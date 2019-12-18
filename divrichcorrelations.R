#code for correlations and scatter plots comparing morpho and molecular data

divcor<-read.table("C:/Users/acahill/Desktop/divcor.txt",header=TRUE)
richcor<-read.table("C:/Users/acahill/Desktop/richnesscor.txt",header=TRUE)

cor.test(divcor$Simpsons_morpho,divcor$Simpsons_molecular)
cor.test(richcor$Richness_morpho,richcor$Richness_molecular)

library(ggplot2)

ggplot(divcor,aes(Simpsons_morpho,Simpsons_molecular))+
  geom_point(size = 4)+
  geom_smooth(method="lm",formula=y~x)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("green","darkorange2","gold","black","purple","red"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nMorphological Diversity")+ylab("Molecular Diversity\n")

ggplot(richcor,aes(Richness_morpho,Richness_molecular))+
  geom_point(size = 4)+
  geom_smooth(method="lm",formula=y~x)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("green","darkorange2","gold","black","purple","red"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nMorphological Richness")+ylab("Molecular Richness\n")