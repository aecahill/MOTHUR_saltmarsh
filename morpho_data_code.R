#need file marsh which is the matrix of species
#need file marshsites 

marsh<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morpho_marsh.txt",header=TRUE)
marshsites<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morpho_marsh_sites.txt",header=TRUE)

#load vegan
library(vegan)
library(wesanderson)

library(cowplot)
library(ggplot2)

#compute NMDS
marshnmds<-metaMDS(marsh)

#plot NMDS in base R
#ordiplot(asus3nmds,type="n")
#ordiellipse(asus3nmds,groups=sites$Sea,draw="polygon",col="grey90",label=F)
#orditorp(asus3nmds,display="species",col="black",air=0.01)
#orditorp(asus3nmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
anosim(marsh,marshsites$Month)
anosim(marsh,marshsites$Site)

#compute PERMANOVA with a space and time interaction
adonis(formula=marsh~marshsites$Month*marshsites$Site)



#moving plot to ggplot

data.scores <- as.data.frame(scores(marshnmds))
datascores<-cbind(data.scores,marshsites)
head(datascores)
species.scores <- as.data.frame(scores(marshnmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)



#make hulls, one for each sea

grp.a <- data.scores[datascores$Month == "April", ][chull(datascores[datascores$Month == 
                                                                        "April", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Month == "July", ][chull(datascores[datascores$Month == 
                                                                      "July", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Month == "October", ][chull(datascores[datascores$Month == 
                                                                      "October", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b,grp.c) #turn the hulls into a single dataframe
hull.month<-c("April","April","April","April","April","April","April","July","July","July","July","July","July","October","October","October","October","October") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.month) #attach group names to hull dataframe

#plot in ggplot

palwes<-c("#F21A00","#EBCC2A","#3B9AB2")

morphotime<-ggplot() +
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
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.month),alpha=0.15) #add polygon based on the hulls calculated

#ACROSS SITES

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
hull.sample<-c("A","A","A","A","A","B","B","B","B","C","C","C","C","C","D","D","D","D","D","E","E","E","E","F","F","F","F","F","G","G","G","G") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

#plot in ggplot

morphosite<-ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Site),size=3) + # add the point markers
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



plot_grid(morphosite,morphotime,labels=c("A","B"),ncol=1)

#diversity statistics

marshdiv<-cbind(diversity(marsh,index="simpson"),marshsites) #calculate simpsons index, bind to site information

colnames(marshdiv)<-c("Simpsons","Month","Site") #rename columns


#summary(aov(marshdiv$Simpsons~marshdiv$Month)) #anova among regions
#summary(aov(marshdiv$Simpsons~marshdiv$Site)) #anova among regions

summary(aov(marshdiv$Simpsons~marshdiv$Month*marshdiv$Site)) #two-way ANOVA

#Margalef

marshrich<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morphomarshrich.txt",header=T)

#summary(aov(marshrich$Richness~marshrich$Month))
#summary(aov(marshrich$Richness~marshrich$Site))

summary(aov(marshrich$Richness~marshrich$Month*marshrich$Site))


marshrichtime<-ggplot(marshrich,aes(x=Month,y=Richness,fill=Month))+
  geom_boxplot()+ 
  scale_fill_manual(values=palwes) +
  theme_bw()+
  annotate("text", x = 1, y = 7.5, label = "a", size = 4.5)+
  annotate("text", x = 2, y = 10.5, label = "a", size = 4.5)+
  annotate("text", x = 3, y = 5, label = "b", size = 4.5)+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
  
  

marshrichsite<-ggplot(marshrich,aes(x=Site,y=Richness,fill=Site))+
  geom_boxplot()+ 
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 7, type="continuous"))) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


marshdivtime<-ggplot(marshdiv,aes(x=Month,y=Simpsons,fill=Month))+
  geom_boxplot()+ 
  scale_fill_manual(values=palwes) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

marshdivsite<-ggplot(marshdiv,aes(x=Site,y=Simpsons,fill=Site))+
  geom_boxplot()+ 
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 7, type="continuous"))) +
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

plot_grid(marshrichsite,marshrichtime,marshdivsite,marshdivtime,labels=c("A","B","C","D"),ncol=2)