data.scores <- as.data.frame(scores(marshnmds))
datascores<-cbind(data.scores,marshsites, c("A","B","C","D","E","F","G","A","B","C","D","E","F","G"))
colnames(datascores)<-c("NMDS1","NMDS2","Month","Sample1","Replicate","Sample2")
head(datascores)
species.scores <- as.data.frame(scores(marshnmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)

grp.1 <- data.scores[datascores$Sample2 == "A", ][chull(datascores[datascores$Sample2 == 
                                                                       "A", c("NMDS1", "NMDS2")]), ]
grp.2 <- data.scores[datascores$Sample2 == "B", ][chull(datascores[datascores$Sample2 == 
                                                                      "B", c("NMDS1", "NMDS2")]), ]
grp.3 <- data.scores[datascores$Sample2 == "C", ][chull(datascores[datascores$Sample2 == 
                                                                    "C", c("NMDS1", "NMDS2")]), ]
grp.4 <- data.scores[datascores$Sample2 == "D", ][chull(datascores[datascores$Sample2 == 
                                                                    "D", c("NMDS1", "NMDS2")]), ]
grp.5 <- data.scores[datascores$Sample2 == "E", ][chull(datascores[datascores$Sample2 == 
                                                                    "E", c("NMDS1", "NMDS2")]), ]
grp.6 <- data.scores[datascores$Sample2 == "F", ][chull(datascores[datascores$Sample2 == 
                                                                    "F", c("NMDS1", "NMDS2")]), ]
grp.7 <- data.scores[datascores$Sample2 == "G", ][chull(datascores[datascores$Sample2 == 
                                                                    "G", c("NMDS1", "NMDS2")]), ]
hull.datasamples <- rbind(grp.1, grp.2, grp.3, grp.4,grp.5,grp.6,grp.7) #turn the hulls into a single dataframe
hull.sample<-c("A","A","B","B","C","C","D","D","E","E","F","F","G","G") #add column for groups (these are based on this data only)
hull.datasamples<-cbind(hull.datasamples,hull.sample) #attach group names to hull dataframe

#plot in ggplot

ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Sample2),size=5) + # add the point markers
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
  geom_polygon(data=hull.datasamples,aes(x=NMDS1,y=NMDS2,group=hull.sample),alpha=0.20) #add polygon based on the hulls calculated

