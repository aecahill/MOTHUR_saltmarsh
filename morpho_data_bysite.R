data.scores <- as.data.frame(scores(marshnmds))
datascores<-cbind(data.scores,marshsites, c("A","B","C","D","E","F","G","A","B","C","D","E","F","G"))
colnames(datascores)<-c("NMDS1","NMDS2","Month","Sample1","Replicate","Sample2")
head(datascores)
species.scores <- as.data.frame(scores(marshnmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)

grp.1 <- data.scores[datascores$Sample == "1", ][chull(datascores[datascores$Sample == 
                                                                       "1", c("NMDS1", "NMDS2")]), ]
grp.2 <- data.scores[datascores$Sample == "2", ][chull(datascores[datascores$Sample == 
                                                                      "2", c("NMDS1", "NMDS2")]), ]
grp.3 <- data.scores[datascores$Sample == "3", ][chull(datascores[datascores$Sample == 
                                                                    "3", c("NMDS1", "NMDS2")]), ]
grp.4 <- data.scores[datascores$Sample == "4", ][chull(datascores[datascores$Sample == 
                                                                    "4", c("NMDS1", "NMDS2")]), ]
grp.5 <- data.scores[datascores$Sample == "5", ][chull(datascores[datascores$Sample == 
                                                                    "5", c("NMDS1", "NMDS2")]), ]
grp.6 <- data.scores[datascores$Sample == "6", ][chull(datascores[datascores$Sample == 
                                                                    "6", c("NMDS1", "NMDS2")]), ]
grp.7 <- data.scores[datascores$Sample == "7", ][chull(datascores[datascores$Sample == 
                                                                    "7", c("NMDS1", "NMDS2")]), ]
hull.datasamples <- rbind(grp.1, grp.2, grp.3, grp.4,grp.5,grp.6,grp.7) #turn the hulls into a single dataframe
hull.sample<-c("1","1","2","2","3","3","4","4","5","5","6","6","7","7") #add column for groups (these are based on this data only)
hull.datasamples<-cbind(hull.datasamples,hull.sample) #attach group names to hull dataframe

#plot in ggplot

ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Sample2),size=5) + # add the point markers
  scale_colour_manual(values=c("green","darkorange2","gold","black","blue","purple","red")) +
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
  geom_polygon(data=hull.datasamples,aes(x=NMDS1,y=NMDS2,group=hull.sample),alpha=0.20) #add polygon based on the hulls calculated

