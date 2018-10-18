library(seqinr)

tax<-read.table("C:/Users/acahill/Desktop/BOLD_taxonomy.tax",header=T) #this file from JKP as reference taxonomy from BOLD
head(tax)

discard<-read.table("C:/Users/acahill/Desktop/discard.accnos",header=F) #seqs discarded from MOTHUR because they didn't align
head(discard)

dim(tax)
dim(discard)

toss<-c() #empty vector

#identify discarded seqs in the taxonomy database and flag their row numbers as a vector
for (i in discard$V1){
  rownumber<-match(i,tax$ï..V1)
  toss<-append(toss,rownumber)
  
}

length(toss)

filtered<-tax[-toss,] #remove rows that correspond to discarded seqs

dim(filtered)

length(filtered$ï..V1)==length(tax$ï..V1)-length(toss)

length(toss)==length(discard$V1)

#need to read in scrap.pcr sequences using SeqinR and remove those numbers as well
#these are sequences that when pcr trimmed had no bases left from MOTHUR

#read in fasta file as alignment
scrapalign<-read.alignment("C:/Users/acahill/Desktop/scrap.fasta.align",format="fasta")
listnames<-as.list(scrapalign$nam) #make a list of names of sequences

clean_names=list() #empty vector of names
nameslength<-c(1:length(listnames)) #vector from 1 to 2014 (number of seqs)

#change the format so that the seq names match the format in the taxonomy file
for (i in nameslength){
  d<-gsub("[\t]", "", listnames[i])
  clean_names<-c(clean_names,d)
  
}

clean_names[1:5]
length(clean_names)

#now remove these names from the filtered taxonomy file

toss2<-c()

#flagging sequences that are in the scrap file
for (i in clean_names){
  rownumber2<-match(i,filtered$ï..V1)
  toss2<-append(toss2,rownumber2)
  
}

length(toss2)

#remove NAs from list (they are here because some of these are already tossed, I think)
toss2_noNA<-na.omit(toss2)

#remove scrap sequences to create a new filtered taxonomy
filtered2<-filtered[-toss2_noNA,]

dim(filtered2)


#now need to remove stuff in the reference that is not in the taxonomy.
#read in large alignment file with seqinR -- may need to jigger names again?
#use matching to check reference against filtered taxonomy file
#if match found, T, else, F to create vector of T and F
#remove lines corresponding to F rows
#write out new alignment


#write out taxonomy file
write.table(filtered2, file = "C:/Users/acahill/Desktop/filtered2.tax",row.names=FALSE,sep= )