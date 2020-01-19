marshotu<-read.table("C:/Users/aecsk/Desktop/allmarshallOTU.txt",header=TRUE)
rich<-read.table("C:/Users/aecsk/Desktop/marshrichness.txt",header=TRUE)
colnames(rich)<-c("Month","Site","Rep","Richness","Rich_reduced")

otu2<-t(marshotu)
sites<-read.table("C:/Users/aecsk/Desktop/allmarshsites.txt",header=TRUE)


#load vegan
library(vegan)
library(pracma)

#fourth-root transform the data 
vec<-1:16616
marsh4 = NULL

for (i in vec) {
  
  b<-nthroot(otu2[,i],4)
  marsh4<-cbind(marsh4,b)
}


#compute NMDS
#asusotunmds<-metaMDS(otu2)
marshotunmds<-metaMDS(marsh4)

#compute PERMANOVA with a space and time interaction
adonis(formula=marsh4~sites$Point*sites$Month)


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
hull.sample<-c("A","A","A","A","A","A","B","B","B","B","B","B","C","C","C","C","C","C","D","D","D","D","D","D","D","E","E","E","E","E","F","F","F","F","G","G","G","G","G","G") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

#plot in ggplot

moldatasite<-ggplot() +
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
hull.sample<-c("A","A","A","A","A","A","B","B","B","B","B","B","C","C","C","C","C","C","C","C") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

#plot in ggplot

moldatatime<-ggplot() +
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

library(cowplot)

plot_grid(moldatasite,moldatatime,labels=c("A","B"),ncol=1)


#Diversity indices
allmarshdiv<-cbind(diversity(marsh4,index="simpson"),sites) #calculate simpsons index, bind to site information

colnames(allmarshdiv)<-c("Simpsons","Month","Site","Rep") #rename columns

#summary(aov(allmarshdiv$Simpsons~allmarshdiv$Month))
#summary(aov(allmarshdiv$Simpsons~allmarshdiv$Site))
#TukeyHSD(aov(allmarshdiv$simpsons~allmarshdiv$Point))

summary(aov(allmarshdiv$Simpsons~allmarshdiv$Month*allmarshdiv$Site))


#Richness
#summary(aov(rich$Richness~rich$Month))
#summary(aov(rich$Richness~rich$Site))

summary(aov(rich$Richness~rich$Month*rich$Site))

TukeyHSD(aov(rich$Richness~rich$Site))

molrichtime<-ggplot(rich,aes(x=Month,y=Richness,fill=Month))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("green","darkorange2","gold","black","purple","red","blue")) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

molrichsite<-ggplot(rich,aes(x=Site,y=Richness,fill=Site))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("green","darkorange2","gold","black","purple","red","blue")) +
  theme_bw()+
  annotate("text", x = 1, y = 51, label = "a", size = 4.5)+
  annotate("text", x = 2, y = 75, label = "bc", size = 4.5)+
  annotate("text", x = 3, y = 90, label = "bc", size = 4.5)+
  annotate("text", x = 4, y = 95, label = "c", size = 4.5)+
  annotate("text", x = 5, y = 83, label = "bc", size = 4.5)+
  annotate("text", x = 6, y = 75, label = "bc", size = 4.5)+
  annotate("text", x = 7, y = 75, label = "b", size = 4.5)+
    theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


moldivtime<-ggplot(allmarshdiv,aes(x=Month,y=Simpsons,fill=Month))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("green","darkorange2","gold","black","purple","red","blue")) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

moldivsite<-ggplot(allmarshdiv,aes(x=Site,y=Simpsons,fill=Site))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("green","darkorange2","gold","black","purple","red","blue")) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

plot_grid(molrichsite,molrichtime,moldivsite,moldivtime,labels=c("A","B","C","D"),ncol=2)