#-------------------------------------------------------------------------------------#
#                                                                                     #
#   Simple object recognition script by S. Hoeks (Radboud university Nijmegen)        #
#                                                                                     #
#-------------------------------------------------------------------------------------#

# set working dir
setwd("/Users/Home/Desktop/R_recognition-master")

# load/install packages
# install.packages("pbapply"); install.packages("png"); install.packages("raster"); install.packages("SDMTools")
# source("http://bioconductor.org/biocLite.R"); biocLite("EBImage")
require(EBImage); require(png); require(SDMTools); require(raster); require(pbapply)

# source functions
source("Functions.R")

# exec functions
direcPictures=getwd() # set directory images
listimagesfiles(direcPictures) # list all files in folder choosen

image_to_open=3 # choose image to open from list (state number of image in list)
Threshold= -0.1 # threshold is set automatically but can be altered for better results (higher values -> more objects highlighted)
setThreshold(image_to_open,Threshold) # setting threshold
display(y3, title="step 0")
display(y4, title="step 1")

maxsize=600 # max size patches, see histogram for possible adjustment 
minsize=0 # min size patches
backgroundObjects(maxsize,minsize) # creating layer with unwanted objects, e.g to remove shadows etc
display(y5, title="step 2")

adjustdivision=100 # may need adjustment when not choosing size catagories correct (0=default)
divideResults(1,adjustdivision) # dividing remaining patches in two groups and displaying results
display(y6, title="result")
display(y7, title="result")
