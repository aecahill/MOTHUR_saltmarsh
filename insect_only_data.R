insectotu<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/allmarsh_insectOTU.txt",header=TRUE)

#load vegan
library(vegan)
library(pracma)
library(wesanderson)

insect2<-t(insectotu)
sites<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/allmarshsites.txt",header=TRUE)

#fourth-root transform the data 
vec<-1:55
insect4 = NULL

for (i in vec) {
  
  b<-nthroot(insect2[,i],4)
  insect4<-cbind(insect4,b)
}


#compute NMDS
#asusotunmds<-metaMDS(otu2)
insectotunmds<-metaMDS(insect4)

#compute PERMANOVA with a space and time interaction
adonis(formula=insect4~sites$Point*sites$Month)

#moving plot to ggplot

data.scores <- as.data.frame(scores(insectotunmds))
datascores<-cbind(data.scores,sites)
head(datascores)
species.scores <- as.data.frame(scores(insectotunmds, "species"))
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
hull.sample<-c("A","A","A","A","A","B","B","B","B","B","B","C","C","C","C","C","D","D","D","D","D","D","E","E","E","E","E","E","F","F","F","F","F","F","G","G","G","G","G","G") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

insectdatasite<-ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Point),size=3) + # add the point markers
  scale_colour_manual(values=rev(wes_palette("Zissou1", n = 7, type="continuous"))) +
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
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.sample),alpha=0.10) #add polygon based on the hulls calculated

#make hulls, one for each SAMPLING SEASON
#this needs to be checked with each run because depends on data

grp.a <- data.scores[datascores$Month == "April", ][chull(datascores[datascores$Month == 
                                                                       "April", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Month == "July", ][chull(datascores[datascores$Month == 
                                                                      "July", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Month == "October", ][chull(datascores[datascores$Month == 
                                                                         "October", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(grp.a, grp.b, grp.c) #turn the hulls into a single dataframe
hull.sample<-c("A","A","A","A","A","A","A","B","B","B","B","B","B","B","C","C","C","C","C","C","C") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

#plot in ggplot
palwes<-c("#F21A00","#EBCC2A","#3B9AB2")

insectdatatime<-ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Month),size=3) + # add the point markers
  scale_colour_manual(values = palwes) +
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
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.sample),alpha=0.10) #add polygon based on the hulls calculated

library(cowplot)

plot_grid(insectdatasite,insectdatatime,labels=c("A","B"),ncol=1)


#Diversity indices
insectdiv<-cbind(diversity(insect4,index="simpson"),sites) #calculate simpsons index, bind to site information

colnames(insectdiv)<-c("Simpsons","Month","Site","Rep") #rename columns

#summary(aov(allmarshdiv$Simpsons~allmarshdiv$Month))
#summary(aov(allmarshdiv$Simpsons~allmarshdiv$Site))
#TukeyHSD(aov(allmarshdiv$simpsons~allmarshdiv$Point))

summary(aov(insectdiv$Simpsons~insectdiv$Month*insectdiv$Site))
TukeyHSD(aov(insectdiv$Simpsons~insectdiv$Site))

insectdivtime<-ggplot(insectdiv,aes(x=Month,y=Simpsons,fill=Month))+
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

insectdivsite<-ggplot(insectdiv,aes(x=Site,y=Simpsons,fill=Site))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 7, type="continuous"))) +
  theme_bw()+
  annotate("text", x = 1, y = 1, label = "a", size = 4.5)+
  annotate("text", x = 2, y = 1, label = "ab", size = 4.5)+
  annotate("text", x = 3, y = 1, label = "b", size = 4.5)+
  annotate("text", x = 4, y = 1, label = "b", size = 4.5)+
  annotate("text", x = 5, y = 1, label = "b", size = 4.5)+
  annotate("text", x = 6, y = 1, label = "ab", size = 4.5)+
  annotate("text", x = 7, y = 1, label = "ab", size = 4.5)+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

#Rarefied richness

insectoturare<-rarefy(insect2,min(rowSums(insect2)))
insectoturare2<-cbind(sites,insectoturare)
colnames(insectoturare2)<-c("Month","Site","Replicate","Rarefied")


summary(aov(insectoturare2$Rarefied~insectoturare2$Month*insectoturare2$Site))

insectraretime<-ggplot(insectoturare2,aes(x=Month,y=Rarefied,fill=Month))+
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

insectraresite<-ggplot(insectoturare2,aes(x=Site,y=Rarefied,fill=Site))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 7, type="continuous"))) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

plot_grid(insectraresite,insectraretime,insectdivsite,insectdivtime,labels=c("A","B","C","D"),ncol=2)
