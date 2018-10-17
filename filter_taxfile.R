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

length(toss)==length(discard$V1)

#need to read in scrap.pcr sequences using SeqinR and remove those numbers as well

library(seqinr)

#read in fasta file as alignment

scrapalign<-read.alignment("C:/Users/acahill/Desktop/scrap.fasta.align",format="fasta")
listnames<-as.list(scrapalign$nam)

clean_names=list() #empty vector of names
nameslength<-c(1:length(listnames)) #vector from 1 to 2014 (number of seqs)

for (i in nameslength){
  d<-gsub("[\t]", "", listnames[i])
  clean_names<-c(clean_names,d)
  
}

clean_names[1:5]
length(clean_names)

#now remove these names from the filtered file

toss2<-c()

for (i in clean_names){
  rownumber2<-match(i,filtered$ï..V1)
  toss2<-append(toss2,rownumber2)
  
}

length(toss2)

toss2_noNA<-na.omit(toss2)

filtered2<-filtered[-toss2_noNA,]

dim(filtered2)



write.table(filtered2, file = "C:/Users/acahill/Desktop/filtered2.tax",row.names=FALSE,sep= )