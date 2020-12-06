library(ggplot2)
library(cowplot)
library(RColorBrewer)

#composition plots for the morphological data

aprilmorpho<-read.table("C:/Users/aecsk/Desktop/aprilmorpho.txt",header=T)
julymorpho<-read.table("C:/Users/aecsk/Desktop/julymorpho.txt",header=T)
octmorpho<-read.table("C:/Users/aecsk/Desktop/octmorpho.txt",header=T)

apr<-ggplot(aprilmorpho, aes(fill=Taxon, y=Count, x=Site)) + 
      geom_bar(position="stack", stat="identity")+
      ylim(0,220)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(legend.position="none")+
    theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
      

jul<-ggplot(julymorpho, aes(fill=Taxon, y=Count, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  ylim(0,220)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
  

oct<-ggplot(octmorpho, aes(fill=Taxon, y=Count, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  ylim(0,220)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())



plot_grid(apr,jul,oct,labels=c("A","B","C"),ncol=3)


#composition plots for the molecular data
aprilmol<-read.table("C:/Users/aecsk/OneDrive/Desktop/aprilmol.txt",header=T)
julymol<-read.table("C:/Users/aecsk/OneDrive/Desktop/julymol.txt",header=T)
octmol<-read.table("C:/Users/aecsk/OneDrive/Desktop/octmol.txt",header=T)

aprm<-ggplot(aprilmol, aes(fill=Taxon, y=Percent, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  ylim(0,1)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


julm<-ggplot(julymol, aes(fill=Taxon, y=Percent, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  ylim(0,1)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


octm<-ggplot(octmol, aes(fill=Taxon, y=Percent, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  ylim(0,1)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())



plot_grid(aprm,julm,octm,labels=c("A","B","C"),ncol=3)


## For the insects

aprilins<-read.table("C:/Users/aecsk/OneDrive/Desktop/aprilins.txt",header=T)
julyins<-read.table("C:/Users/aecsk/OneDrive/Desktop/julyins.txt",header=T)
octins<-read.table("C:/Users/aecsk/OneDrive/Desktop/octins.txt",header=T)

apri<-ggplot(aprilins, aes(fill=Taxon, y=Percent, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  ylim(0,1.01)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


juli<-ggplot(julyins, aes(fill=Taxon, y=Percent, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  ylim(0,1.01)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


octi<-ggplot(octins, aes(fill=Taxon, y=Percent, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  ylim(0,1.01)+
  scale_fill_brewer(type="qual",palette="Paired") +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())



plot_grid(apri,juli,octi,labels=c("A","B","C"),ncol=3)