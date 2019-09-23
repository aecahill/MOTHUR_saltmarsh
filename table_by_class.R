marshotu<-read.table("C:/Users/acahill/Desktop/allmarshclass.txt",header=TRUE)
#rich<-read.table("C:/Users/acahill/Desktop/otu_rich_oct.txt",header=TRUE)



samples<-c(1:63)
otu = NULL

for (i in samples) {
  collapsed<-tapply(marshotu[,i],marshotu$Class,sum)
  otu<-cbind(otu,collapsed)
  
}

otu[1:15,1:5]
colnames(otu)<-colnames(marshotu[1:63])
otu2<-t(otu)
write.table(otu,file="C:/Users/acahill/Desktop/otuclass.txt")