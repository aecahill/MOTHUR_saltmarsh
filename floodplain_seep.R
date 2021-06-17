#need file marsh which is the matrix of species
#need file marshsites 
#THIS CODE NOW REQUIRES THE RAREFACTION SCRIPT ALSO!

floodplain<-read.table("C:/Users/acahill/Documents/GitHub/MOTHUR_saltmarsh/floodplainmorpho.txt",header=TRUE)
floodplainsites<-read.table("C:/Users/acahill/Documents/GitHub/MOTHUR_saltmarsh/floodplainsites.txt",header=TRUE)

#load vegan
library(vegan)
library(wesanderson)

library(cowplot)
library(ggplot2)

#compute NMDS
floodplainnmds<-metaMDS(floodplain)

#plot NMDS in base R
ordiplot(floodplainnmds,type="n")
ordiellipse(floodplainnmds,groups=floodplainsites$Location,draw="polygon",col="grey90",label=F)
orditorp(floodplainnmds,display="species",col="black",air=0.01)
orditorp(floodplainnmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
anosim(floodplain,floodplainsites$Location)

#compute PERMANOVA with a space and time interaction
adonis(formula=floodplain~floodplainsites$Location)



#moving plot to ggplot

data.scores <- as.data.frame(scores(floodplainnmds))
datascores<-cbind(data.scores,floodplainsites)
head(datascores)
species.scores <- as.data.frame(scores(floodplainnmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)



#make hulls, one for each sea

grp.a <- data.scores[datascores$Location == "Seep_transect", ][chull(datascores[datascores$Location == 
                                                                       "Seep_transect", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Location == "Floodplain", ][chull(datascores[datascores$Location == 
                                                                      "Floodplain", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(grp.a, grp.b) #turn the hulls into a single dataframe
hull.month<-c("Seep","Seep","Seep","Seep","Seep","Seep","Seep","Seep","Seep","Floodplain","Floodplain","Floodplain","Floodplain","Floodplain") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.month) #attach group names to hull dataframe

#plot in ggplot

palwes<-c("#F21A00","#EBCC2A","#3B9AB2")

ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Location),size=.25, alpha=0.25) + # add the point markers
  scale_colour_manual(values = palwes) +
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
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.month),alpha=0.15) #add polygon based on the hulls calculated

