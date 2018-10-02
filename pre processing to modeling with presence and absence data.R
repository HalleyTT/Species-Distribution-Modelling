#################################################################
############ Preprocessing-- defining presence & absence

setwd("C:/Users/YZH00_000/Dropbox/udemy/Data/elev/2_SDM Data/bioclim")
library(raster)
library(rgdal)
library(dismo)
library(sp)
datafiles = Sys.glob("*.tif") #Or whatever identifies your files
stck = stack() #empty stack for raster
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}

stck #raster predictors as a stack

plot(stck,1)

### presence data

horn=read.csv("C:/Users/YZH00_000/Dropbox/udemy/Data/elev/2_SDM Data/hornbill_my1.csv")
head(horn)
horn1= horn[,-1]#first column not needed


points(horn1, col='blue') 

prs1= extract(stck, horn1)


set.seed(1)

backgr = randomPoints(stck, 500) #500 random points
absvals = extract(stck, backgr) #choose absence values from the background
pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals)))
sdmdata = data.frame(cbind(pb, rbind(prs1, absvals)))

head(sdmdata)

sdmdata=na.omit(sdmdata)
summary(sdmdata)

tail(sdmdata)
############select an area of absence based on ecological 
#######considerations
library(raster)
e = drawExtent()##dynamically select absence area
abs=crop(stck, e)
plot(abs)
backgr=randomPoints(abs,135)#80-2-ratio
absvals2 = extract(abs, backgr)#400 pseuduo-absence
pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals2)))
smdata=data.frame(cbind(pb,rbind(prs1,absvals2)))
head(sdmdata)