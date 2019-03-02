#Code to deal with the salt marsh OTUs, ALL included
#need to read in file, remove infrequent reads, transform matrix
#also will need site file
#then do all OTU analyses

library(vegan)
library(ggplot2)
library(pracma)

biomolmarsh<-read.table("C:/Users/acahill/Desktop/july_OTU.txt",header=F)
sites<-read.table("C:/Users/acahill/Desktop/marsh_biomol_sites.txt",header=TRUE)

#remove read names
biomolmarsh2<-biomolmarsh[,-1]

#seqs with <10 reads start at position 8251, so trim after that
biomolmarsh3<-biomolmarsh2[1:8250,]

#transform matrix
biomolmarsh4<-t(biomolmarsh3)

#fourth-root transform the data
vec<-1:8250
biomolmarsh5 = NULL

for (i in vec) {
  
  b<-nthroot(biomolmarsh4[,i],4)
  biomolmarsh5<-cbind(biomolmarsh5,b)
}

#compute NMDS
biomolmarshMDS<-metaMDS(biomolmarsh5)

data.scores <- as.data.frame(scores(biomolmarshMDS))
datascores<-cbind(data.scores,sites)
head(datascores)

#make hulls, one for each site

grp.a <- data.scores[datascores$Site == "A", ][chull(datascores[datascores$Site == 
                                                                  "A", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Site == "B", ][chull(datascores[datascores$Site == 
                                                                  "B", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Site == "C", ][chull(datascores[datascores$Site == 
                                                                  "C", c("NMDS1", "NMDS2")]), ]
grp.d <- data.scores[datascores$Site == "D", ][chull(datascores[datascores$Site == 
                                                                  "D", c("NMDS1", "NMDS2")]), ]
grp.e <- data.scores[datascores$Site == "E", ][chull(datascores[datascores$Site == 
                                                                  "E", c("NMDS1", "NMDS2")]), ]
grp.f <- data.scores[datascores$Site == "F", ][chull(datascores[datascores$Site == 
                                                                  "F", c("NMDS1", "NMDS2")]), ]
grp.g <- data.scores[datascores$Site == "G", ][chull(datascores[datascores$Site == 
                                                                  "G", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(grp.a, grp.b, grp.c, grp.d,grp.e, grp.f, grp.g) #turn the hulls into a single dataframe
hull.sample<-c("A","A","A","B","B","B","C","C","C","D","D","D","E","E","E","F","F","F","G","G","G") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

#plot in ggplot

ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Site),size=5) + # add the point markers
  scale_colour_manual(values=wes_palette("Zissou1", 7, type = "continuous")) +
  coord_equal() +
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+ 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.sample),alpha=0.20) #add polygon based on the hulls calculated

#add diversity scores

biomolmarshdiv<-cbind(diversity(biomolmarsh5,index="simpson"),sites,c("April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April")) #calculate simpsons index, bind to site information

colnames(biomolmarshdiv)<-c("simpsons","Site","Sample","Month") #rename columns

#Plot of diversity stats

ggplot(biomolmarshdiv, aes(x=Site, y=simpsons, colour=Month))+ 
  geom_point(cex=3)+
  #geom_jitter(position=position_jitter(0.2), cex=2)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #geom_smooth(method='lm',formula=y~x)+
  xlab("\nSample")+ylab("Simpsons\n")+
  scale_colour_manual(values=wes_palette("Zissou1", 1, type = "continuous"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  #theme(legend.position="none")+
  ylim(0.990,1)

biomolmarshrich<-read.table("C:/Users/acahill/Desktop/biomolmarshrich.txt",header=T)

biomolmarshrich2<-cbind(biomolmarshrich,sites,c("April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April","April"))
colnames(biomolmarshrich2)<-c("Rep","Margalef","Site","Sample","Month")

ggplot(biomolmarshrich2, aes(x=Site, y=Margalef, color=Month))+ 
  geom_point(cex=3)+
  #geom_jitter(position=position_jitter(0.2), cex=6)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  geom_smooth(method='lm',formula=y~x)+
  xlab("\nSample")+ylab("Margalef\n")+
  scale_colour_manual(values=wes_palette("Zissou1", 1, type = "continuous"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  theme(legend.position="none")
  #ylim(0,2.5)


