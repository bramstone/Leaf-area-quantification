leaf_quant<-function(path=c(),ref.area=double()) {
  require(jpeg)
  #use either user supplied directory of pictures, or current working directory
  if(is.null(path)) files<-dir(path,pattern=".jpg")
  if(is.null(ref.area)) stop("Must supply area of reference square to scale image pixel counts.")
  else files<-dir(getwd(),pattern=".jpg")
  leaf.area<-matrix(data=NA,ncol=8,nrow=length(files),
    dimnames=list(c(),c("sample.name","total.leaf.area",
    "green.leaf.area","brown.leaf.area","green.ratio",
    "ref.pix","green.pix","brown.pix")))
  for(i in 1:length(files)){
    pic<-jpeg::readJPEG(files[i])
    #extract RGB channels for each pixel
    df<-data.frame(
      red=matrix(pic[,,1],ncol=1),
      green=matrix(pic[,,2],ncol=1),
      blue=matrix(pic[,,3],ncol=1))
    #use ratio of RG:B to determine brown leaf and blue reference square against background
    df$b.rg.ratio<-df[,3]/((df[,1]+df[,2])/2)
    df$g.rb.ratio<-df[,2]/((df[,1]+df[,3])/2)
    df$r.g.ratio<-df[,1]/df[,2]
    df$material<-NA
      df$material[which(df$b.rg.ratio>1.3)]<-"reference"
      df$material[which(df$g.rb.ratio>1.5 & df$b.rg.ratio>!1.3)]<-"greenleaf"
      df$material[which(df$r.g.ratio>1.1 & df$g.rb.ratio>!1.5)]<-"brownleaf"
    #reference square area is supplied by user
    ref.pix<-length(which(df$material=="reference"))
    scaler<-ref.pix/ref.area
    leaf.area[i,"ref.pix"]<-ref.pix
    leaf.area[i,"green.pix"]<-length(which(df$material=="greenleaf"))
    leaf.area[i,"brown.pix"]<-length(which(df$material=="brownleaf"))
    leaf.area[i,"green.leaf.area"]<-(length(which(df$material=="greenleaf"))/scaler)*2
    leaf.area[i,"brown.leaf.area"]<-(length(which(df$material=="brownleaf"))/scaler)*2
    leaf.area[i,"total.leaf.area"]<-(leaf.area[i,"green.leaf.area"]+leaf.area[i,"brown.leaf.area"])
    leaf.area[i,"green.ratio"]<-leaf.area[i,"green.leaf.area"]/leaf.area[i,"total.leaf.area"]
#concatenates and prints a progress statement that refreshes each iteration
    cat(paste0("picture ",i," of ",length(files)),"\r")
    flush.console()
  }
  leaf.area<-data.frame(leaf.area)
  leaf.area$sample.id<-files
  format(leaf.area, scientific=FALSE)
  return(leaf.area)
}
