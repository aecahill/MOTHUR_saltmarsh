#Code to deal with the salt marsh OTUs FROM CHRIS, ANNOTATED, UNKNOWNS REMOVED
#need to read in file and transform matrix
#also will need site file


library(vegan)
library(ggplot2)
library(pracma)
library(wesanderson)

#WORKING WITH COUNT TABLE FILE

cbmarsh<-read.table("C:/Users/acahill/Desktop/CT2.txt",header=T)
cbsites<-read.table("C:/Users/acahill/Desktop/TABLE_and_Sites.txt",header=TRUE)


#transform matrix
cbmarsh2<-t(cbmarsh)

#fourth-root transform the data
vec<-1:231
cbmarsh3 = NULL

for (i in vec) {
  
  b<-nthroot(cbmarsh2[,i],4)
  cbmarsh3<-cbind(cbmarsh3,b)
}

#compute NMDS
cbmarshMDS<-metaMDS(cbmarsh3)

data.scores <- as.data.frame(scores(cbmarshMDS))
datascores<-cbind(data.scores,cbsites)
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
  #coord_equal() +
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
  #geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.sample),alpha=0.20) #add polygon based on the hulls calculated


#WORKING WITH ALL OTU FILE

cbmarshOTU<-read.table("C:/Users/acahill/Desktop/cbmarshOTU.txt",header=T)
cbsites<-read.table("C:/Users/acahill/Desktop/TABLE_and_Sites.txt",header=TRUE)


#seqs with <10 reads start at position 5907, so trim after that
cbmarshOTU2<-cbmarshOTU[1:5906,]

#transform matrix
cbmarshOTU3<-t(cbmarshOTU2)

#fourth-root transform the data
vec<-1:5906
cbmarshOTU4 = NULL

for (i in vec) {
  
  b<-nthroot(cbmarshOTU3[,i],4)
  cbmarshOTU4<-cbind(cbmarshOTU4,b)
}

#compute NMDS
cbmarshOTUMDS<-metaMDS(cbmarshOTU4)

data.scores <- as.data.frame(scores(cbmarshOTUMDS))
datascores<-cbind(data.scores,cbsites)
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
  #coord_equal() +
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

