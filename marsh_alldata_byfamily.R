marshotu<-read.table("C:/Users/aecsk/Desktop/allmarshfam.txt",header=TRUE)
#rich<-read.table("C:/Users/acahill/Desktop/otu_rich_oct.txt",header=TRUE)



samples<-c(1:63)
otu = NULL

for (i in samples) {
  collapsed<-tapply(marshotu[,i],marshotu$Family,sum)
  otu<-cbind(otu,collapsed)
  
}

otu[1:15,1:5]
colnames(otu)<-colnames(marshotu[1:63])
otu2<-t(otu)
sites<-read.table("C:/Users/aecsk/Desktop/allmarshsites.txt",header=TRUE)

#sampleA<-colSums(otu2[1:3,])
#sampleB<-colSums(otu2[4:6,])
#sampleC<-colSums(otu2[7:9,])
#sampleD<-colSums(otu2[10:12,])
#sampleE<-colSums(otu2[13:15,])
#sampleF<-colSums(otu2[16:18,])
#sampleG<-colSums(otu2[19:21,])

#allsamples<-rbind(sampleA,sampleB,sampleC,sampleD,sampleE,sampleF,sampleG)
#allsamples2<-cbind(c("A","B","C","D","E","F","G"),allsamples)

#colnames(allsamples2)<-c("Site",colnames(allsamples))



#load vegan
library(vegan)
library(pracma)

#fourth-root transform the data 
vec<-1:115
marsh4 = NULL

for (i in vec) {
  
  b<-nthroot(otu2[,i],4)
  marsh4<-cbind(marsh4,b)
}


#compute NMDS
#asusotunmds<-metaMDS(otu2)
marshotunmds<-metaMDS(marsh4)



#moving plot to ggplot

data.scores <- as.data.frame(scores(marshotunmds))
datascores<-cbind(data.scores,sites)
head(datascores)
species.scores <- as.data.frame(scores(marshotunmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)

library(ggplot2)

#make hulls, one for each SAMPLING POINT
#this needs to be checked with each run because depends on data

grp.a <- data.scores[datascores$Point == "A", ][chull(datascores[datascores$Point == 
                                                                  "A", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Point == "B", ][chull(datascores[datascores$Point == 
                                                                  "B", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Point == "C", ][chull(datascores[datascores$Point == 
                                                                  "C", c("NMDS1", "NMDS2")]), ]
grp.d <- data.scores[datascores$Point == "D", ][chull(datascores[datascores$Point == 
                                                                  "D", c("NMDS1", "NMDS2")]), ]
grp.e <- data.scores[datascores$Point == "E", ][chull(datascores[datascores$Point == 
                                                                  "E", c("NMDS1", "NMDS2")]), ]
grp.f <- data.scores[datascores$Point == "F", ][chull(datascores[datascores$Point == 
                                                                  "F", c("NMDS1", "NMDS2")]), ]
grp.g <- data.scores[datascores$Point == "G", ][chull(datascores[datascores$Point == 
                                                                  "G", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(grp.a, grp.b, grp.c, grp.d,grp.e, grp.f, grp.g) #turn the hulls into a single dataframe
hull.sample<-c("A","A","A","A","B","B","B","B","B","C","C","C","C","C","D","D","D","D","D","E","E","E","E","F","F","F","F","F","F","G","G","G","G","G") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

#plot in ggplot

ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Point),size=5) + # add the point markers
  scale_colour_manual(values=c("green","darkorange2","gold","black","purple","red","blue")) +
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


#make hulls, one for each SAMPLING SEASON
#this needs to be checked with each run because depends on data

grp.a <- data.scores[datascores$Month == "April", ][chull(datascores[datascores$Month == 
                                                                   "April", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Month == "July", ][chull(datascores[datascores$Month == 
                                                                   "July", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Month == "October", ][chull(datascores[datascores$Month == 
                                                                   "October", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(grp.a, grp.b, grp.c) #turn the hulls into a single dataframe
hull.sample<-c("A","A","A","A","A","B","B","B","B","B","B","B","C","C","C","C","C","C","C","C") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

#plot in ggplot

ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Month),size=5) + # add the point markers
  scale_colour_manual(values=c("green","darkorange2","gold","black","purple","red","blue")) +
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


#Diversity indices
allmarshdiv<-cbind(diversity(marsh4,index="simpson"),sites) #calculate simpsons index, bind to site information

colnames(allmarshdiv)<-c("simpsons","Month","Point","Rep") #rename columns

summary(aov(allmarshdiv$simpsons~allmarshdiv$Month))

summary(aov(allmarshdiv$simpsons~allmarshdiv$Point))
TukeyHSD(aov(allmarshdiv$simpsons~allmarshdiv$Point))
