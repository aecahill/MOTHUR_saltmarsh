library(seqinr)

goodreads<-read.table("C:/Users/acahill/Desktop/goodreads.txt",header=TRUE)#read in list of representative seqs to keep
all_seqs<-read.fasta("C:/Users/acahill/Desktop/allseqs.fasta",as.string=FALSE,set.attributes=TRUE)


namelist<-NULL

for (i in goodreads$Read) {
  nameseq<-which(grepl(i,names(all_seqs))) 
  namelist<-rbind(namelist,nameseq)
}
  
seq_out<-NULL #blank list to append good sequences to

for (i in namelist[,1]) {
  y<-cbind(all_seqs[i],names(all_seqs)[i])
  seq_out<-rbind(y,seq_out)
} 

write.fasta(seq_out[,1],names=seq_out[,2],"C:/Users/acahill/Desktop/marsh_reduced.fasta")
