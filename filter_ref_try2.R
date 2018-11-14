library(seqinr)


#read in reference alignment

refalign<-read.alignment("C:/Users/acahill/Desktop/reference.align",format="fasta")

#read in table of errors thrown by mothur 
#these are seqs that are in the ref but missing from the tax file; need to be removed from ref

#errortab<-read.table("C:/Users/acahill/Desktop/error_list.txt",header=FALSE)

#turn names in error table into characters

#seqs_2remove<-as.character(errortab$V1)

#need to clean reference names again

listnamesref<-as.list(refalign$nam) #make a list of names of sequences

clean_namesref=list() #empty vector of names
namesreflength<-c(1:length(listnamesref)) #vector from 1 to number of seqs

#change the format so that the seq names match the format in the taxonomy file
for (i in namesreflength){
  e<-gsub("[\t]", "", listnamesref[i])
  clean_namesref<-c(clean_namesref,e)
  
}

clean_namesref[1:5]
length(clean_namesref)

#match names in filtered2 to line numbers in the alignment

tossref<-c()

#make vector to toss FROM REFERENCE
for (i in clean_namesref){
  rownumberref<-match(i,filtered2$ï..V1)
  tossref<-append(tossref,rownumberref)
  
}

length(tossref)

tossref_noNA<-na.omit(tossref)

reffiltered_names<-refalign$nam[-tossref_noNA]
reffiltered_seqs<-refalign$seq[-tossref_noNA]

write.fasta(sequences=reffiltered_seqs,names=reffiltered_names,file.out="C:/Users/acahill/Desktop/reffiltered.fasta")

