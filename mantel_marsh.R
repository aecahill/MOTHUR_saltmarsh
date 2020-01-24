morphomarsh<-read.table("C:/Users/aecsk/Desktop/morpho_marsh.txt",header=TRUE)
morphosites<-read.table("C:/Users/aecsk/Desktop/morpho_marsh_sites.txt",header=TRUE)
biomol<-read.table("C:/Users/aecsk/Desktop/allmarshallOTU.txt",header=TRUE)
biomolsites<-read.table("C:/Users/aecsk/Desktop/allmarshsites.txt",header=TRUE)

library(vegan)
library(pracma)

#fourthroot morpho data
vec<-1:25
marsh4 = NULL

for (i in vec) {
  
  b<-nthroot(morphomarsh[,i],4)
  marsh4<-cbind(marsh4,b)
}

#make collapsed list of taxa across sampling points
morpho<-cbind(marsh4,morphosites)

sites<-c(1:25)
region = NULL

for (i in sites) {
  collapsed<-tapply(morpho[,i],morpho$Site,sum)
  region<-cbind(region,collapsed)
  
}

region
colnames(region)<-colnames(morpho[1:25])


#repeat for molecular data

#transform dataframe

biomol2<-t(biomol)

#4th root transform

vec<-1:16616
biomol4 = NULL

for (i in vec) {
  
  b<-nthroot(biomol2[,i],4)
  biomol4<-cbind(biomol4,b)
}

biomol<-cbind(biomol4,biomolsites)

biomolsites2<-c(1:16616)
biomolregion = NULL

for (i in biomolsites2) {
  collapsed<-tapply(biomol[,i],biomol$Point,sum)
  biomolregion<-cbind(biomolregion,collapsed)
  
}

colnames(biomolregion)<-colnames(biomol[1:16616])

#Make distance matrices
braymorpho<-vegdist(region,method="bray")
braybiomol<-vegdist(biomolregion,method="bray")


#mantel tests
mantel(braymorpho,braybiomol,method="spearman")



##redo by season


#make collapsed list of taxa across seasons
morpho<-cbind(marsh4,morphosites)

sites<-c(1:25)
season = NULL

for (i in sites) {
  collapsed<-tapply(morpho[,i],morpho$Month,sum)
  season<-cbind(season,collapsed)
  
}

season
colnames(season)<-colnames(morpho[1:25])

#molecular by season

biomol<-cbind(biomol4,biomolsites)

biomolsites2<-c(1:16616)
biomolseason = NULL

for (i in biomolsites2) {
  collapsed<-tapply(biomol[,i],biomol$Month,sum)
  biomolseason<-cbind(biomolseason,collapsed)
  
}

colnames(biomolseason)<-colnames(biomol[1:16616])

#Make distance matrices
braymorphoseason<-vegdist(season,method="bray")
braybiomolseason<-vegdist(biomolseason,method="bray")


#mantel tests
mantel(braymorphoseason,braybiomolseason,method="spearman")





#collapsing the otus and redoing with collapsed
asuotu<-read.table("C:/Users/acahill/Desktop/asuoct.txt",header=TRUE)

samples<-c(1:33)
otu = NULL

for (i in samples) {
  collapsed<-tapply(asuotu[,i],asuotu$Class,sum)
  otu<-cbind(otu,collapsed)
  
}

otu
colnames(otu)<-colnames(asuotu[1:33])
otu2<-t(otu)

vec<-1:33
coll4 = NULL

for (i in vec) {
  
  b<-nthroot(otu2[,i],4)
  coll4<-cbind(coll4,b)
}

coll<-cbind(coll4,biomolsites)

collseas<-c(1:33)
collregion = NULL

for (i in collseas) {
  collapsed<-tapply(coll[,i],coll$Sea,sum)
  collregion<-cbind(collregion,collapsed)
  
}

colnames(collregion)<-colnames(coll[1:33])

braycoll<-vegdist(collregion,method="bray")


mantel(braymorpho,braycoll,method="spearman")
mantel(braybiomol,braycoll,method="spearman")