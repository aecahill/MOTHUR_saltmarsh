#need file marsh which is the matrix of species
#need file marshsites 

marsh<-read.table("C:/Users/acahill/Desktop/marsh.txt",header=TRUE)
marshsites<-read.table("C:/Users/acahill/Desktop/marsh_sites.txt",header=TRUE)

#load vegan
library(vegan)

#compute NMDS
marshnmds<-metaMDS(marsh)

#plot NMDS in base R
#ordiplot(asus3nmds,type="n")
#ordiellipse(asus3nmds,groups=sites$Sea,draw="polygon",col="grey90",label=F)
#orditorp(asus3nmds,display="species",col="black",air=0.01)
#orditorp(asus3nmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
anosim(marsh,marshsites$Month)
anosim(marsh,marshsites$Sample)



#moving plot to ggplot

data.scores <- as.data.frame(scores(marshnmds))
datascores<-cbind(data.scores,marshsites)
head(datascores)
species.scores <- as.data.frame(scores(marshnmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)

library(ggplot2)

#make hulls, one for each sea

grp.a <- data.scores[datascores$Month == "April", ][chull(datascores[datascores$Month == 
                                                                        "April", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Month == "July", ][chull(datascores[datascores$Month == 
                                                                      "July", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b) #turn the hulls into a single dataframe
hull.month<-c("April","April","April","April","April","July","July","July","July","July","July") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.month) #attach group names to hull dataframe

#plot in ggplot

ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Month),size=5) + # add the point markers
  scale_colour_manual(values=wes_palette("Zissou1", 2, type = "continuous")) +
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
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.month),alpha=0.20) #add polygon based on the hulls calculated


#diversity statistics

marshdiv<-cbind(diversity(marsh,index="simpson"),marshsites) #calculate simpsons index, bind to site information

colnames(marshdiv)<-c("simpsons","Month","Sample","Replicate") #rename columns


summary(aov(marshdiv$simpsons~marshdiv$Month)) #anova among regions

#Plot of diversity stats

ggplot(marshdiv, aes(x=Sample, y=simpsons, color=Month))+ 
  geom_point(cex=6)+
  #geom_jitter(position=position_jitter(0.2), cex=6)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  geom_smooth(method='lm',formula=y~x)+
  xlab("\nSample")+ylab("Simpsons\n")+
  scale_colour_manual(values=wes_palette("Zissou1", 2, type = "continuous"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  #theme(legend.position="none")+
  ylim(0,1)


marshrich<-read.table("C:/Users/acahill/Desktop/marshrich.txt",header=T)

summary(aov(marshrich$Margalef~marshrich$Month))

ggplot(marshrich, aes(x=Sample, y=Margalef, color=Month))+ 
  geom_point(cex=6)+
  #geom_jitter(position=position_jitter(0.2), cex=6)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  geom_smooth(method='lm',formula=y~x)+
  xlab("\nSample")+ylab("Margalef\n")+
  scale_colour_manual(values=wes_palette("Zissou1", 2, type = "continuous"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  #theme(legend.position="none")+
  ylim(0,2.5)