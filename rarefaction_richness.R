##Need code to do rarefaction


library(vegan)

marsh<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morpho_marsh.txt",header=TRUE)
marshsites<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morpho_marsh_sites.txt",header=TRUE)
marshrich<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/morphomarshrich.txt",header=T)

marsh2<-rbind(marsh[1:10,],marsh[12:45,])
marshrare<-rarefy(marsh2,min(rowSums(marsh2)))
marshrare2<-cbind(marshrich,marshrare)


summary(aov(marshrare2$Richness~marshrare2$Month*marshrare2$Site))
summary(aov(marshrare2$marshrare~marshrare2$Month*marshrare2$Site))

#is Tukey what I need here? I don't like this
TukeyHSD(aov(marshrare2$marshrare~marshrare2$Site))

# rarefaction curves (color according to site, then season)
#species accumulation curves for rarefaction and exact
#Guess I need to redo the boxplots

#repeat with molecular data

marshotu<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/allmarshallOTU.txt",header=TRUE)
richotu<-read.table("C:/Users/aecsk/Documents/GitHub/MOTHUR_saltmarsh/marshrichness.txt",header=TRUE)
colnames(richotu)<-c("Month","Site","Rep","Richness","Rich_reduced")

otu2<-t(marshotu)

marshoturare<-rarefy(otu2,min(rowSums(otu2)))
marshoturare2<-cbind(richotu,marshoturare)

summary(aov(marshoturare2$Richness~marshoturare2$Month*marshoturare2$Site))
summary(aov(marshoturare2$marshoturare~marshoturare2$Month*marshoturare2$Site))

TukeyHSD(aov(marshoturare2$marshoturare~marshoturare2$Site))

# rarefaction curves (color according to site, then season)
#species accumulation curves for rarefaction and exact
#Guess I need to redo the boxplots
