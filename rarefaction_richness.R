##Need code to do rarefaction


library(vegan)
library(wesanderson)
palwes<-c("#F21A00","#EBCC2A","#3B9AB2")


marsh<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morpho_marsh.txt",header=TRUE)
marshsites<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morpho_marsh_sites.txt",header=TRUE)
marshrich<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morphomarshrich.txt",header=T)

marsh2<-rbind(marsh[1:10,],marsh[12:45,])
marshrare<-rarefy(marsh2,min(rowSums(marsh2)))
marshrare2<-cbind(marshrich,marshrare)
colnames(marshrare2)<-c("Month","Site","Richness","Rarefied")


summary(aov(marshrare2$Richness~marshrare2$Month*marshrare2$Site))
summary(aov(marshrare2$Rarefied~marshrare2$Month*marshrare2$Site))

#is Tukey what I need here? I don't like this
TukeyHSD(aov(marshrare2$Rarefied~marshrare2$Site))

# rarefaction curves (color according to site, then season)
palwes<-c("#F21A00","#EBCC2A","#3B9AB2")
colmonth<-c("#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00",
           "#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A",
           "#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2")

rarecurve(marsh2,col=colmonth,label=FALSE,lwd=2)

colsite<-c("#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00",
           "#E67D00","#E67D00","#E67D00","#E67D00","#E67D00","#E67D00","#E67D00",
           "#E4B80E","#E4B80E","#E4B80E","#E4B80E","#E4B80E","#E4B80E","#E4B80E",
           "#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A",
           "#9EBE91","#9EBE91","#9EBE91","#9EBE91","#9EBE91","#9EBE91",
           "#63ADBE","#63ADBE","#63ADBE","#63ADBE","#63ADBE","#63ADBE",
           "#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2")

rarecurve(marsh2,col=colsite,label=FALSE,lwd=3)


#species accumulation curves for rarefaction and exact

plot(specaccum(marsh2,method="rarefaction"))
plot(specaccum(marsh2,method="exact"))

#Guess I need to redo the boxplots
marshraretime<-ggplot(marshrare2,aes(x=Month,y=Rarefied,fill=Month))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=palwes) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

marshraresite<-ggplot(marshrare2,aes(x=Site,y=Rarefied,fill=Site))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 7, type="continuous"))) +
  theme_bw()+
  annotate("text", x = 1, y = 2.65, label = "a", size = 4.5)+
  annotate("text", x = 2, y = 2.65, label = "ab", size = 4.5)+
  annotate("text", x = 3, y = 2.65, label = "ab", size = 4.5)+
  annotate("text", x = 4, y = 3.2, label = "b", size = 4.5)+
  annotate("text", x = 5, y = 2.5, label = "ab", size = 4.5)+
  annotate("text", x = 6, y = 3.2, label = "b", size = 4.5)+
  annotate("text", x = 7, y = 2.8, label = "ab", size = 4.5)+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

#repeat with molecular data

marshotu<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/allmarshallOTU.txt",header=TRUE)
richotu<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/marshrichness.txt",header=TRUE)
colnames(richotu)<-c("Month","Site","Rep","Richness","Rich_reduced")

otu2<-t(marshotu)

marshoturare<-rarefy(otu2,min(rowSums(otu2)))
marshoturare2<-cbind(richotu,marshoturare)
colnames(marshoturare2)<-c("Month","Site","Replicate","Richness","Rich_reduced","Rarefied")


summary(aov(marshoturare2$Richness~marshoturare2$Month*marshoturare2$Site))
summary(aov(marshoturare2$Rarefied~marshoturare2$Month*marshoturare2$Site))

TukeyHSD(aov(marshoturare2$Rarefied~marshoturare2$Site))

# rarefaction curves (color according to site, then season)

palwes<-c("#F21A00","#EBCC2A","#3B9AB2")
colmonthotu<-c("#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00",
            "#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A",
            "#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2")

rarecurve(otu2,col=colmonthotu,label=FALSE,lwd=2)

colsiteotu<-c("#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00","#F21A00",
           "#E67D00","#E67D00","#E67D00","#E67D00","#E67D00","#E67D00","#E67D00","#E67D00","#E67D00",
           "#E4B80E","#E4B80E","#E4B80E","#E4B80E","#E4B80E","#E4B80E","#E4B80E","#E4B80E","#E4B80E",
           "#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A","#EBCC2A",
           "#9EBE91","#9EBE91","#9EBE91","#9EBE91","#9EBE91","#9EBE91","#9EBE91","#9EBE91","#9EBE91",
           "#63ADBE","#63ADBE","#63ADBE","#63ADBE","#63ADBE","#63ADBE","#63ADBE","#63ADBE","#63ADBE",
           "#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2","#3B9AB2")

rarecurve(otu2,col=colsiteotu,label=FALSE,lwd=3)

#species accumulation curves for rarefaction and exact

plot(specaccum(otu2,method="rarefaction"))
plot(specaccum(otu2,method="exact"))

#Guess I need to redo the boxplots

oturaretime<-ggplot(marshoturare2,aes(x=Month,y=Rarefied,fill=Month))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=palwes) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

oturaresite<-ggplot(marshoturare2,aes(x=Site,y=Rarefied,fill=Site))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 7, type="continuous"))) +
  theme_bw()+
  annotate("text", x = 1, y = 460, label = "a", size = 4.5)+
  annotate("text", x = 2, y = 640, label = "b", size = 4.5)+
  annotate("text", x = 3, y = 720, label = "b", size = 4.5)+
  annotate("text", x = 4, y = 770, label = "b", size = 4.5)+
  annotate("text", x = 5, y = 720, label = "b", size = 4.5)+
  annotate("text", x = 6, y = 650, label = "b", size = 4.5)+
  annotate("text", x = 7, y = 670, label = "b", size = 4.5)+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
