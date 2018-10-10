tax<-read.table("C:/Users/acahill/Desktop/BOLD_taxonomy.tax",header=T)
head(tax)

discard<-read.table("C:/Users/acahill/Desktop/discard.accnos",header=F)
head(discard)

dim(tax)
dim(discard)

toss<-c()

for (i in discard$V1){
  rownumber<-match(i,tax$ï..V1)
  toss<-append(toss,rownumber)
  
}

length(toss)

filtered<-tax[-toss,]

dim(filtered)

length(filtered$ï..V1)==length(tax$ï..V1)-length(toss)