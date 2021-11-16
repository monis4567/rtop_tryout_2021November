#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# on eDNA from Virik
# Authors: Steen Wilhelm Knudsen.

#Get package if needed:
if(!require(raster)){
  install.packages("raster", repos='http://cran.us.r-project.org')
}
library(raster)
#Get package if needed:
if(!require(rgdal)){
  install.packages("rgdal", repos='http://cran.us.r-project.org')
}
library(rgdal)
#Get package if needed:
if(!require(tidyverse)){
  install.packages("tidyverse", repos='http://cran.us.r-project.org')
}
library(tidyverse)
#Get package if needed:
if(!require(rtop)){
  install.packages("rtop", repos='http://cran.us.r-project.org')
}
#
# get biogeo package
if(!require(biogeo)){
  install.packages("biogeo", repos='http://cran.us.r-project.org')
}
library(biogeo)


# get the intamap package
if(!require(intamap)){
  install.packages("intamap", repos='http://cran.us.r-project.org')
}
library(intamap)
# get the leaflet package
if(!require(leaflet)){
  install.packages("leaflet", repos='http://cran.us.r-project.org')
}
library(leaflet)

library(sf)
library(dplyr)
#
library(rtop)
library(raster)
library(rgdal)
library(tidyverse)
#remove everything in the working environment, without a warning!!
rm(list=ls())
# set path
# if you use projects in Rstudio, you don't need to worry about this :-)
rpath = "."
#wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fish_eDNA_210130/test_example_rtop"
#rpath <- wd00
wd00 <- rpath
setwd(wd00)
getwd()
# read sample data points
file <- "test_points.csv"
file.obs <- "dummy_data_S_Norway_v01.csv"
df <- read.table(file.obs,sep=",",header=T)
#head(df,8)
# #get the latitude elements to turn them in to decimal degrees
dd <- as.numeric(gsub("^([0-9]+)DD([0-9]+)MM([0-9]+).*$","\\1",df$LAT))
mm <- as.numeric(gsub("^([0-9]+)DD([0-9]+)MM([0-9]+).*$","\\2",df$LAT))
ss <- as.numeric(gsub("^([0-9]+)DD([0-9]+)MM([0-9]+).*$","\\3",df$LAT))
ns <- "N" #set this to northern hemisphere
df$decLAT <- biogeo::dms2dd(dd,mm,ss,ns)
# #get the longitude elements to turn them in to decimal degrees
dd <- as.numeric(gsub("^([0-9]+)DD([0-9]+)MM([0-9]+).*$","\\1",df$LON))
mm <- as.numeric(gsub("^([0-9]+)DD([0-9]+)MM([0-9]+).*$","\\2",df$LON))
ss <- as.numeric(gsub("^([0-9]+)DD([0-9]+)MM([0-9]+).*$","\\3",df$LON))
ns <- "E" # set this to eastern longitudes
#convert to decimal degrees and append to dataframe
df$decLON <- biogeo::dms2dd(dd,mm,ss,ns)
#append and rename in data frame
df$latitude <- df$decLAT
df$longitude <- df$decLON
#define vector with columns to keep
keeps <- c("latitude",
           "longitude",
           "CONC")
#limit the df to only the columns that are to be kept
df01 <- df[keeps]
df02 <- df01
# say which columns are coordinates
coordinates(df01)=~longitude+latitude
# specify the spatial reference system
# if your coordinates are UTM32, then us "EPSG:25832"
# ours are in lat/lon  WGS84
# see https://epsg.io/
#proj4string(df)<- CRS("EPSG:4326")
proj4string(df01)<- CRS("+proj=longlat +datum=WGS84")
plot(df01)


sdf <- df01
coordinates(sdf) <- ~lon+lat
proj4string(sdf)<- CRS("EPSG:4326") # set CRS to WGS84
plot(sdf)
# save as shape file
raster::shapefile(df01, "dummy_points_S_Norway.shp",overwrite=TRUE )
#_______________________________________________________________________________
# define dummy predicted locations from the dummy input observations
#_______________________________________________________________________________
#make standard deviation for latitude 
sdlap <- (max(df02$latitude)-min(df02$latitude))*0.4
mlat <- mean(df02$latitude)
#make standard deviation for longitude
sdlop <- (max(df02$longitude)-min(df02$longitude))*0.4
mlon <- mean(df02$longitude)
#generate sample of 200 obs. that follows normal dist. with mean=  and sd= as 
# defined by latitude and longitude limits above
predict_lat <- rnorm(200, mean=mlat, sd=sdlap)
predict_lon <- rnorm(200, mean=mlon, sd=sdlop)
#combine in a dataframe
df_pred01 <- as.data.frame(cbind(predict_lat,predict_lon))
df_pred02 <- df_pred01
#head(df_pred02,3)
#plot the dummy predicted locations
# dev.off()
#plot(df_pred01)
# plot(df, col="blue")
# say which columns are coordinates
coordinates(df_pred01)=~predict_lon+predict_lat
# specify the spatial reference system
# if your coordinates are UTM32, then us "EPSG:25832"
# ours are in lat/lon  WGS84
# see https://epsg.io/
#proj4string(df_pred01)<- CRS("EPSG:4326")
proj4string(df_pred01)<- CRS("+proj=longlat +datum=WGS84")
#see the points plotted
plot(df_pred01)
# e.g. if you want to transform to UTM32
new_prj_prd <- CRS("EPSG:25832")
dfUTM32_prd<-spTransform(df_pred01,new_prj_prd)
# save as shape file
raster::shapefile(df_pred01, "predicted_dummy_points_S_Norway.shp",overwrite=TRUE )

# To get a map of rivers in Norway look at these websites
# see: https://gis.stackexchange.com/questions/317744/seeking-norwegian-waterlines-and-water-bodies-shapefile
# and see: https://kartkatalog.geonorge.no/metadata/norges-vassdrags-og-energidirektorat/vannforekomster/b203e422-5270-4efc-93a5-2073725c43ef
# and see: https://nedlasting.nve.no/gis/
# The river map is also placed here
# https://www.dropbox.com/sh/jwow45jfof8bvcj/AADhrNp17Pc5f5nG3p25Grmfa?dl=0
unlink("NVEData", recursive=TRUE)
unlink("Metadata", recursive=TRUE)
#Check if a path for the directory for the rivers have been defined

# if not then get the zip file. Unpack it. Make a path to the river shape 
#directory and then remove the zip file
if (!exists("rpath_Norway_rivers"))
{print("missing shape file with rivers")

  unlink("NVE_rivers.zip")
  NVE_zip_file <- "https://www.dropbox.com/s/m1orkkdh9kq54ly/NVE_60751B14_1635507782111_11488.zip?dl=0"
  download.file(NVE_zip_file, dest="NVE_rivers.zip", method="wget", quiet=T)
  unzip ("NVE_rivers.zip", exdir = "./")
  unlink("NVE_rivers.zip")
  #define the path for the directory where the unzipped downloaded river files are placed
  rpath_Norway_rivers <- "NVEData/Elv"
}


#define the path for the directory where the unzipped downloaded river files are placed
#rpath_Norway_rivers <- "NVE_60751B14_1635507782111_11488/NVEData/Elv"
#paste path together
rpath_NR <- paste(wd00,"/",rpath_Norway_rivers,sep="")
rpath_NR <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fish_eDNA_210130/NVE_60751B14_1635507782111_11488/NVEData/Elv"
# read in the river shape files 
rnet = readOGR(rpath_NR, "Elv_Hovedelv")
#turn off previous plots
dev.off()
#plot the river network
plot(rnet, col="blue")
# add the predicted points to the plot
plot(df_pred01, add=T, col="yellow")
# add the observed points to the plot
plot(sdf, add=T, pch=21, col="red")

#_______________________________________________________________________________
# rtop input requires polygons for both predicted and observed points
# Try and create polygons around points
#_#_
#https://stackoverflow.com/questions/55048518/creating-bordering-polygons-from-spatial-point-data-for-plotting-in-leaflet
#create sf object with points
#convert to sf object
stations_obs02 <- st_as_sf( df02, coords = c( "longitude", "latitude" ) )
#create voronoi/thiessen polygons
dfo.02 <- stations_obs02 %>% 
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()
#make the date frame a spatial data frame
sdf.o02 <- as(dfo.02, 'Spatial')
# chnage projection
proj4string(sdf.o02)<- CRS("+proj=longlat +datum=WGS84")
#write raster files
raster::shapefile(sdf.o02, "obsLoc03.shp",overwrite=TRUE )

#convert to sf object
stations_pred02 <- st_as_sf( df_pred02, coords = c( "predict_lon", "predict_lat" ) )
#create voronoi/thiessen polygons
dfp.02 <- stations_pred02 %>% 
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()
#make the date frame a spatial data frame
sdf.p02 <- as(dfp.02, 'Spatial')
# chnage projection
proj4string(sdf.o02)<- CRS("+proj=longlat +datum=WGS84")
#write raster files
raster::shapefile(sdf.p02, "predLoc03.shp",overwrite=TRUE )

# Read back in shape files
# read in raster files
obLoc3=rgdal::readOGR(rpath, "obsLoc03")
prLoc3=rgdal::readOGR(rpath, "predLoc03")

dev.off()
#plot the river network
plot(rnet, col="blue")
# add the predicted points to the plot
plot(df_pred01, add=T, col="yellow")
# add the observed points to the plot
plot(sdf, add=T, pch=21, col="red")
# add polygons on top
plot(obLoc3, add=T, border = "pink")
plot(prLoc3, add=T, border = "brown")



library(raster)

p <-  obLoc3
pols <- list(p,p)
conc.po <- df02$CONC

polsconc <- lapply(1:length(pols), function(i) {
  p <- pols[[i]]
  p$conc.po <- conc.po[i]
  p
} )

plot(polsconc)
p

polsdatetime 
class(prLoc3)
obs <- df02$CONC

rtopObj2	=	createRtopObject(obLoc3, prLoc3,
                           formulaString = obs~1, 
                           params = list(gDist=TRUE, rresol = 25))
is.null(rtopObj2)

#_#_

