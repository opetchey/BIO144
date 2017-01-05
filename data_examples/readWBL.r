path <- "~/Teaching/Bio144/data_examples/WBL/"

d.stream <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/stream.dat",header=T)
write.table(d.stream,paste(path,"stream.dat",sep=""),quote=FALSE)

d.hafer <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/hafer.dat",header=T)
write.table(d.hafer,paste(path,"hafer.dat",sep=""),quote=FALSE)

d.blutzucker <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/blutzucker.dat",header=T)
write.table(d.blutzucker,paste(path,"blutzucker.dat",sep=""),quote=FALSE)

d.cricket <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/cricket.dat",header=T)
write.table(d.cricket,paste(path,"cricket.dat",sep=""),quote=FALSE)

d.catheter <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/catheter.dat",header=T)
write.table(d.catheter,paste(path,"catheter.dat",sep=""),quote=FALSE)


d.auto <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/automob.dat",header=T)
write.table(d.auto,paste(path,"auto.dat",sep=""),sep=",",quote=FALSE,row.names=F)

d.duenger <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/duenger.dat",header=T)
write.table(d.duenger,paste(path,"duenger.dat",sep=""),sep=",",quote=FALSE,row.names=F)
