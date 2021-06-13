#code for correlations and scatter plots comparing morpho and molecular data

divcor<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/divcor.txt",header=TRUE)
richcor<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/richnesscor.txt",header=TRUE)
rarecor<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/rarefied_cor.txt",header=TRUE)

cor.test(divcor$Simpsons_morpho,divcor$Simpsons_molecular)
cor.test(richcor$Richness_morpho,richcor$Richness_molecular)
cor.test(rarecor$Rarefied_morpho,rarecor$Rarefied_mol)

library(ggplot2)
library(cowplot)

divplot<-ggplot(divcor,aes(Simpsons_morpho,Simpsons_molecular))+
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

richplot<-ggplot(richcor,aes(Richness_morpho,Richness_molecular))+
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

rareplot<-ggplot(rarecor,aes(Rarefied_morpho,Rarefied_mol))+
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


plot_grid(divplot,rareplot,labels=c("A","B"))