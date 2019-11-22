library(seqinr)

goodreads<-read.table("C:/Users/acahill/Desktop/goodreads.txt",header=TRUE)#read in list of representative seqs to keep
all_seqs<-read.fasta("C:/Users/acahill/Desktop/allseqs.fasta",as.string=FALSE,set.attributes=TRUE) #read in list of seqs to extract from


namelist<-NULL #blank list

for (i in goodreads$Read) {
  nameseq<-which(grepl(i,names(all_seqs))) #find the row number where each goodread name occurrs
  namelist<-rbind(namelist,nameseq) #make running list of these rownumbers
}
  
seq_out<-NULL #blank list to append good sequences to

for (i in namelist[,1]) {
  y<-cbind(all_seqs[i],names(all_seqs)[i]) #for each row number, remove the sequence and its name
  seq_out<-rbind(y,seq_out) #turn into a table
} 

write.fasta(seq_out[,1],names=seq_out[,2],"C:/Users/acahill/Desktop/marsh_reduced.fasta")
