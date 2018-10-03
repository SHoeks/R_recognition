packagesNeeded <- function(){
  install.packages("pbapply")
  install.packages("png")
  install.packages("raster")
  install.packages("SDMTools")
  source("http://bioconductor.org/biocLite.R")
  biocLite("EBImage")
  require(EBImage)
  require(png)
  require(SDMTools)
  require(raster)
  require(pbapply)
  
}

#### function 1 
listimagesfiles <- function(direcPictures){
  allFiles=list.files(path=paste(direcPictures,sep='')) 
  allFiles=as.matrix(allFiles); 
  return(allFiles) # list images
}

#### function 2
setThreshold <- function(image_to_open,Threshold){
  # opening specific image
  allFiles=list.files(path=paste(direcPictures,sep='')) 
  allFiles=as.matrix(allFiles); 
  a=as.integer(image_to_open)
  imagename=allFiles[a,]
  image=readImage(paste(direcPictures,'/',imagename,sep=''))
  image1=image[,,1]; image2=image[,,2]; image3=image[,,3]
  # threshold certain layer automatic (most clear layer picked automatically as well)
  med1=sd(image1);med2=sd(image2);med3=sd(image3);
  medtot=cbind(med1,med2,med3); medmax=min(medtot); framenr=which(medtot[1,]==medmax)
  normalize(image[,,framenr], separate=TRUE, ft=c(0,1));
  image1=image[,,framenr]; image1org=image[,,framenr]
  dimimage=(dim(image1)); dimimage=as.matrix(dimimage); t=dimimage[2,]
  meet1=t/2; meet11=round(meet1, digits = 0)
  meet2=t/4; meet12=round(meet2, digits = 0)
  meet3=(t/4)*3; meet13=round(meet3, digits = 0)
  meet4=t/5; meet14=round(meet4, digits = 0)
  meet5=t/6; meet15=round(meet5, digits = 0)
  row1=mean(image1[meet11,]);row2=min(image1[meet12,]);row5=min(image1[meet13,]);row10=mean(image1[meet14,]);row11=min(image1[meet15,])
  meanval=(row1+row2+row5+row10+row11)/5
  mini=meanval; 
  d=0
  if (Threshold==0) d=0
  if (Threshold>0) d=Threshold
  if (Threshold<0) d=Threshold
  Threshold=as.numeric(Threshold); mini=as.numeric(mini); d=as.numeric(d)
  totalthres=mini+d
  image1[image1>totalthres+0.1]=1
  image1[image1<1]=0
  y3=writePNG(image1); y4=readPNG(y3); y4[y4>0]=1; y4[y4==0]=2; y4[y4==1]=0; y4[y4==2]=1; 
  # display(image, title="original image"); display(y4, title="image with threhold set"); 
  # Saving to global enviroment
  y3<<-image;
  y4<<-y4; sprintf("$%.2f", y4); ### needed later?
  mini<<-mini; sprintf("$%.2f", mini); ### needed later?
  framenr<<-framenr; sprintf("%.2f", framenr); ### needed later?
  totalthres<<-totalthres; sprintf("$%.2f", totalthres); ### needed later?
}


#### function 3
backgroundObjects <- function(maxsize,minsize){
  # Load image again (normal threshold)
  allFiles=list.files(path=paste(direcPictures,sep='')) 
  allFiles=as.matrix(allFiles); 
  a=as.integer(image_to_open)
  imagename=allFiles[a,]
  image=readImage(paste(direcPictures,'/',imagename,sep=''))
  image1=image[,,1]; image2=image[,,2]; image3=image[,,3]
  # threshold certain layer automatic (most clear layer picked automatically as well)
  med1=sd(image1);med2=sd(image2);med3=sd(image3);
  medtot=cbind(med1,med2,med3); medmax=min(medtot); framenr=which(medtot[1,]==medmax)
  image1=image[,,framenr]
  totalthres=as.numeric(totalthres)
  totalthres2=totalthres+0.17
  image1[image1>totalthres2]=1
  image1[image1<1]=0
  y3=writePNG(image1); yy4=readPNG(y3); yy4[yy4>0]=1; yy4[yy4==0]=2; yy4[yy4==1]=0; yy4[yy4==2]=1; 
  # Default mechanism
  d=0; if (maxsize==0) d=200;if (maxsize>0) d=maxsize
  u=0; if (minsize==0) u=0;if (minsize>0) u=maxsize
  # background creation
  LabelFIN=ConnCompLabel(yy4);stats = PatchStat(LabelFIN);
  stats = stats[stats[,2]<1000,]
  par(mfrow=c(1,1))
  hist(stats[,2],main = paste("distribution of remaining patches"),xlab = "patch size in number of pixels",xlim=c(1,200) )
  stats = stats[stats[,2]<d,] # getting rid of background stats
  stats = stats[stats[,2]>u,] # getting rid of noise
  stats4 = stats[stats[,2]<10000,] # table with patches consisting of background objects
  LabelFIN4 = ConnCompLabel(yy4);
  zNID = stats4[,1]; zEnd=length(zNID); 
  for (zw in 1:(zEnd-1))
  {LabelFIN4[LabelFIN4==(stats4[zw,1])] = 0};
  LabelFIN4[LabelFIN4>1]=1; 
  # display(LabelFIN4, title="unwanted backgroud objects"); 
  # subtraction of background
  ff4=Image(LabelFIN4, colormode=Grayscale); y5=y4-LabelFIN4; # display(y5, title="objects of interest remaining")
  # Saving to global enviroment
  y5<<-y5; sprintf("$%.2f", y5); ### needed later?
  totalthres<<-totalthres; sprintf("$%.2f", totalthres); ### needed later?
}


#### function 4
divideResults <- function(step3,adjustdivision){
  # using right input image, depending if step 3 was skipped or not, 1 means used (y5 is result step 3)
  if (step3==0) qwe=y4; if (step3==1) qwe=y5 
  # finding border between patches of one or two birds
  LabelFIN=ConnCompLabel(qwe);stats = PatchStat(LabelFIN)
  stats = stats[stats[,2]<maxsize,] # getting rid of background stats
  mostcommon2=sort(table(stats$n.cell),decreasing=TRUE)[1:35]; 
  most2=as.matrix(mostcommon2); mat=as.numeric((rownames(most2))); maxmost=max(mat, na.rm=TRUE); 
  hist(stats[,2],main = paste("distribution of remaining patches"),
       xlab = "patch size in number of pixels",xlim=c(1,(round(maxmost*1.4))))
  textend1="value for dividing patches"; endresult1=maxmost
  print(as.vector(cbind(textend1,endresult1)))
  # dividing patches in tables
  qq=0
  if (adjustdivision==0) qq=maxmost
  if (adjustdivision!=0) qq=adjustdivision
  stats2 = stats[stats[,2]<qq,] # table with all one bird patches
  stats3 = stats[stats[,2]>qq,] # table with patches consisting of multiple birds
  # visualizing patches of two or more birds
  LabelFIN2 = ConnCompLabel(qwe);
  zNID = stats2[,1]; zEnd=length(zNID); 
  for (zw in 1:(zEnd-1))
  {LabelFIN2[LabelFIN2==(stats2[zw,1])] = 0};
  LabelFIN2[LabelFIN2>1]=1; 
  ff2=Image(LabelFIN2, colormode=Grayscale)
  # visualizing patches of single birds
  LabelFIN3 = ConnCompLabel(qwe);
  zNID = stats3[,1]; zEnd=length(zNID); 
  for (zw in 1:(zEnd-1))
  {LabelFIN3[LabelFIN3==(stats3[zw,1])] = 0};
  LabelFIN3[LabelFIN3>1]=1; 
  ff3=Image(LabelFIN3, colormode=Grayscale); 
  # Visualizing end result 
  allFiles=list.files(path=paste(direcPictures,sep='')) 
  allFiles=as.matrix(allFiles); 
  a=as.integer(image_to_open)
  imagename=allFiles[a,]
  image=readImage(paste(direcPictures,'/',imagename,sep=''))
  colorMode(ff3) = Grayscale; colorMode(ff2) = Grayscale; 
  x3=paintObjects(ff3, image, opac=c(1,1),col=c('red'),thick=TRUE, closed=FALSE)
  x4=paintObjects(ff2, x3, opac=c(1,1),col=c('blue'),thick=TRUE, closed=FALSE)
  display(x4, title="end result"); 
  y6<<-x3
  y7<<-x4
  amounttwo=length(stats3[,2]); amountone=length(stats2[,2])
  totalamount=amounttwo+amountone; 
  textend="number of objects counted ="; endresult=totalamount
  print(as.vector(cbind(textend,endresult)))
  textend="number of large objects counted ="; endresult=amounttwo
  print(as.vector(cbind(textend,endresult)))
  textend="number of small objects counted ="; endresult=amountone
  print(as.vector(cbind(textend,endresult)))
}
