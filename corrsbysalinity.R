#Testing richness and diversity by sampling point versus salinity

#Richness by salinity, OTUs

richsal<-read.table("C:/Users/aecsk/Desktop/sal_rich.txt",header=T)

#Richness, all OTUs, regressed on salinity

summary(aov(richsal$Rich_all~richsal$Salinity))
cor.test(richsal$Rich_all,richsal$Salinity)

#Richness in reduced data, regressed on salinity
summary(aov(richsal$Rich_reduced~richsal$Salinity))
cor.test(richsal$Rich_reduced,richsal$Salinity)

#Richness, all OTUs, regressed on site

summary(aov(richsal$Rich_all~richsal$Point_num))
cor.test(richsal$Rich_all,richsal$Point_num)

#Richness in reduced data, regressed on salinity
summary(aov(richsal$Rich_reduced~richsal$Point_num))
cor.test(richsal$Rich_reduced,richsal$Point_num)

plot(richsal$Point_num,richsal$Rich_all)
plot(richsal$Salinity,richsal$Rich_all)
plot(richsal$Point_num,richsal$Rich_reduced)
plot(richsal$Salinity,richsal$Rich_reduced)


#Richness by salinity, morpho

richsalmorpho<-read.table("C:/Users/aecsk/Desktop/salrichmorpho.txt",header=T)

#Richness, morpho, regressed on salinity

summary(aov(richsalmorpho$Richness_morpho~richsalmorpho$Salinity))
cor.test(richsalmorpho$Richness_morpho,richsalmorpho$Salinity)


plot(richsalmorpho$Salinity,richsalmorpho$Richness_morpho)


#Diversity by salinity, reduced OTU

marshreducedotu<-read.table("C:/Users/aecsk/Desktop/allmarshreducedOTU.txt",header=TRUE)
otu2<-t(marshreducedotu)


#load vegan
library(vegan)
library(pracma)

#fourth-root transform the data 
vec<-1:108
marsh4 = NULL

for (i in vec) {
  
  b<-nthroot(otu2[,i],4)
  marsh4<-cbind(marsh4,b)
}

divmol<-diversity(marsh4,index="simpson")

summary(aov(divmol~richsal$Salinity))
cor.test(divmol,richsal$Salinity)

#Diversity by salinity, all OTU

marshotu<-read.table("C:/Users/aecsk/Desktop/allmarshallOTU.txt",header=TRUE)
otu2<-t(marshotu)

#fourth-root transform the data 
vec<-1:16616
marsh4 = NULL

for (i in vec) {
  
  b<-nthroot(otu2[,i],4)
  marsh4<-cbind(marsh4,b)
}

divmolall<-diversity(marsh4,index="simpson")

summary(aov(divmolall~richsal$Salinity))
cor.test(divmolall,richsal$Salinity)


#Diversity by salinity, morpho

marsh<-read.table("C:/Users/aecsk/Desktop/morpho_marsh.txt",header=TRUE)


divmorpho<-diversity(marsh,index="simpson")

summary(aov(divmorpho~richsalmorpho$Salinity))
cor.test(divmorpho,richsalmorpho$Salinity)

