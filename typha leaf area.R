library(jpeg)

setwd(if(file.exists("E:/")) "E:/Typha project/Typha pictures" else "F:/Typha project/Typha pictures")

files<-list.files(pattern=".jpg")

leaf.area<-matrix(data=NA,ncol=9,nrow=length(files),
	dimnames=list(c(),c("sample.date","pond.section",
		"total.leaf.cm2","green.leaf.cm2","brown.leaf.cm2",
		"green.ratio","ref.pix","green.pix","brown.pix")))

for(i in 1:length(files)){
	test<-readJPEG(files[i])

	#extract RGB channels for each pixel
	df<-data.frame(
		red=matrix(test[,,1],ncol=1),
		green=matrix(test[,,2],ncol=1),
		blue=matrix(test[,,3],ncol=1))
	
	#use ratio of RG:B to determine brown leaf and sticky note reference against background
	df$b.rg.ratio<-df[,3]/((df[,1]+df[,2])/2)
	df$g.rb.ratio<-df[,2]/((df[,1]+df[,3])/2)
	df$r.g.ratio<-df[,1]/df[,2]
	df$material<-NA
	  df$material[which(df$b.rg.ratio>1.3)]<-"reference"
	  df$material[which(df$g.rb.ratio>1.5 & df$b.rg.ratio>!1.3)]<-"greenleaf"
	  df$material[which(df$r.g.ratio>1.1 & df$g.rb.ratio>!1.5)]<-"brownleaf"

	#reference sticky note is 19.38cm2 (3.8 x 5.1)
	ref.pix<-length(which(df$material=="reference"))
	scaler<-ref.pix/19.38
	leaf.area[i,"ref.pix"]<-ref.pix
	leaf.area[i,"green.pix"]<-length(which(df$material=="greenleaf"))
	leaf.area[i,"brown.pix"]<-length(which(df$material=="brownleaf"))
	leaf.area[i,"green.leaf.cm2"]<-(length(which(df$material=="greenleaf"))/scaler)*2
	leaf.area[i,"brown.leaf.cm2"]<-(length(which(df$material=="brownleaf"))/scaler)*2
	leaf.area[i,"total.leaf.cm2"]<-(leaf.area[i,"green.leaf.cm2"]+leaf.area[i,"brown.leaf.cm2"])
	leaf.area[i,"green.ratio"]<-leaf.area[i,"green.leaf.cm2"]/leaf.area[i,"total.leaf.cm2"]
    	
	#concatenates and prints a progress statement that refreshes each iteration
	cat(paste0("picture ",i," of ",length(files)),"\n")
	flush.console()
}

leaf.area<-data.frame(leaf.area)
leaf.area$sample.date<-files
format(leaf.area, scientific=FALSE)