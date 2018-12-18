marshotu<-read.table("C:/Users/acahill/Desktop/marsh_OTU_collapsed.txt",header=TRUE)
#rich<-read.table("C:/Users/acahill/Desktop/otu_rich_oct.txt",header=TRUE)



samples<-c(1:21)
otu = NULL

for (i in samples) {
  collapsed<-tapply(marshotu[,i],marshotu$Family,sum)
  otu<-cbind(otu,collapsed)
  
}

otu
colnames(otu)<-colnames(marshotu[1:21])
otu2<-t(otu)
sites<-read.table("C:/Users/acahill/Desktop/marsh_biomol_sites.txt",header=TRUE)


#need file asus3 which is the matrix of species
#need file sites with a column of site names and region names


#load vegan
library(vegan)
library(pracma)

#fourth-root transform the data
vec<-1:21
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

#make hulls, one for each sea
#this needs to be checked with each run because depends on data

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

#diversity statistics PICK IT UP HERE!!!!

collrich<-read.table("C:/Users/acahill/Desktop/otucollapsedoct2.txt",header=TRUE)
asuscolldiv<-cbind(diversity(otu2,index="simpson"),collrich) #calculate simpsons index, bind to site information

colnames(asuscolldiv)<-c("simpsons","margalef","asu","Location","site") #rename columns


summary(aov(asusdiv$simpsons~asusdiv$Sea)) #anova among regions
TukeyHSD(aov(asusdiv$simpsons~asusdiv$Sea)) #post-hoc tests among regions

#Plot of diversity stats

ggplot(asuscolldiv, aes(x=site, y=simpsons,color=Location))+ 
  geom_boxplot() +
  geom_point(cex=4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  xlab("\nSite")+ylab("Simpsons\n")+
  scale_colour_manual(values=c("darkorange2","gold","purple","green","black","red"),labels=c("Baltic","Biscay","Gulf of Lions","Adriatic","Black","Red"))+
  scale_x_discrete(labels=c("Karkle","Palanga","Lekeitio","Pasaia","Zumaia","Cassidaigne","Elvine","Rioux","Due Sorelle","Grotta Azzurra","Scalaccia","Aladja","Cherninos","Kamchia","Janib Sa'ara","Qaham"))+
  theme(axis.text.x= element_text(size=12))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  #theme(legend.position="none")+
  ylim(0,1)+
  #annotate("text", x = 1, y = 0.63, label = "ab", size = 6)+
  #annotate("text", x = 2, y = 0.81, label = "ab", size = 6)+
  #annotate("text", x = 3, y = 0.89, label = "ab", size = 6)+
  #annotate("text", x = 4, y = 0.92, label = "b", size = 6)+
  #annotate("text", x = 5, y = 0.72, label = "ab", size = 6)+
  #annotate("text", x = 6, y = 0.43, label = "a", size = 6)+
  #annotate("text", x = 7, y = 0.84, label = "ab", size = 6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#add margalef



summary(aov(collrich$margalef~collrich$region)) #anova among regions
TukeyHSD(aov(collrich$margalef~collrich$region)) #post-hoc tests among regions

#Plot of richness stats

ggplot(asuscolldiv, aes(x=site, y=margalef,color=Location))+ 
  geom_boxplot() +
  geom_point(cex=4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  xlab("\nSite")+ylab("Margalef\n")+
  scale_colour_manual(values=c("darkorange2","gold","purple","green","black","red"),labels=c("Baltic","Biscay","Gulf of Lions","Adriatic","Black","Red"))+
  scale_x_discrete(labels=c("Karkle","Palanga","Lekeitio","Pasaia","Zumaia","Cassidaigne","Elvine","Rioux","Due Sorelle","Grotta Azzurra","Scalaccia","Aladja","Cherninos","Kamchia","Janib Sa'ara","Qaham"))+
  theme(axis.text.x= element_text(size=12))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  ylim(0,2.25)+
  #annotate("text", x = 1, y = 0.63, label = "ab", size = 6)+
  #annotate("text", x = 2, y = 0.81, label = "ab", size = 6)+
  #annotate("text", x = 3, y = 0.89, label = "ab", size = 6)+
  #annotate("text", x = 4, y = 0.92, label = "b", size = 6)+
  #annotate("text", x = 5, y = 0.72, label = "ab", size = 6)+
  #annotate("text", x = 6, y = 0.43, label = "a", size = 6)+
  #annotate("text", x = 7, y = 0.84, label = "ab", size = 6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
