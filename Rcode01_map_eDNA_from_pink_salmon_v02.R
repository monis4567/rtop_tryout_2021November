#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

# Ideally this code is intended to continue the project 
# On environmental DNA from fish found i Norwegian rivers
# As initially presented here:
# https://niva.brage.unit.no/niva-xmlui/handle/11250/2997014

#____________________________________________________________________________#
#remove everything in the working environment, without a warning!!
rm(list=ls())

#Install package if required
if(!require(raster)){
  install.packages("raster", repos='http://cran.us.r-project.org')
}
library(raster)
#Install package if required
if(!require(rgdal)){
  install.packages("rgdal", repos='http://cran.us.r-project.org')
}
library(rgdal)
#Install package if required
if(!require(tidyverse)){
  install.packages("tidyverse", repos='http://cran.us.r-project.org')
}
library(tidyverse)
#Install package if required
if(!require(rtop)){
  install.packages("rtop", repos='http://cran.us.r-project.org')
}
#get libraries
library(rtop)
library(raster)
library(rgdal)
library(tidyverse)

# install package if required
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
library(ggplot2)
library(rgdal)
library(dplyr)
#define working directory
wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2023/MS_pukkellaks_Norge_Elianne_Anette/Rcode_on_eDNA_from_pink_salmon"
wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fish_eDNA_210130/rtop_on_virik_eDNA/rtop_tryout_2021November"
#rpath <- wd00
# set working directory
setwd(wd00)
#getwd()
# #define input files
# lstfil01 <- list.files(".")
# 
# inf1 <- lstfil01[grepl("\\.xls",lstfil01)]
# # read in data frame from local directory
# tibbl_m01<- readxl::read_xlsx(inf1)
# define a variable to decide if the excel file should be fetched from dropbox
get_Norway_eDNA_from_drobbox<- T
# Or read in the excel file from the dropbox folder
if (get_Norway_eDNA_from_drobbox==T)
{print("getting excel file with eDNA levels from drobox")
  unlink("GJE_eDNA_samples.xlsx")
  GJE_eDNA_file <-"https://www.dropbox.com/scl/fi/bb3pw8r7kl7u31agu6lq4/eDNAsamples_GJE_2021_Rform.xlsx?rlkey=8u2atdwc8g4xd2f58v4rl8rw4&dl=0"
  download.file(GJE_eDNA_file, dest="GJE_eDNA_samples.xlsx", method="wget", quiet=T)
  unzip ("GJE_eDNA_samples.xlsx", exdir = "./")
  #unlink("GJE_eDNA_samples.xlsx")
  tibbl_m01 <- readxl::read_xlsx("GJE_eDNA_samples.xlsx")
}

#tibbl_m01
#View(tibbl_m01)
pl01 <- ggplot(tibbl_m01,aes(x=lon,y=lat)) +
  theme_bw() + 
  #geom_point(aes(colour=tot_edna_ngml)) 
  geom_jitter(aes(colour=tot_edna_ngml),
              width=4E-3,height = 4E-3)
#pl01
# subset tibble to only include 3 columns
tibbl_m02 <- tibbl_m01[c("lat","lon","tot_edna_ngml")]
#
# add jitter to positions
# The rtop package requires that several points with data along rivers are
# available. Although all these samples and rpelicates have been
# collected at the same postion
# they need to have slightly different positions to make up a larger
# sample size to allow for creating the rtop object
tibbl_m02$jdlat<- jitter(tibbl_m02$lat,0.6)
tibbl_m02$jdlon<- jitter(tibbl_m02$lon,0.6)
#unique(tibbl_m02$jdlon)
#append and rename in data frame
tibbl_m02$lati <- tibbl_m02$jdlat
tibbl_m02$long <- tibbl_m02$jdlon
#define vector with columns to keep
keeps <- c("lati",
           "long",
           "tot_edna_ngml")
tibbl_m05 <- tibbl_m02
#limit the df to only the columns that are to be kept
df_m04 <- tibbl_m02[keeps]
# say which columns are coordinates, and make it a
# SpatialPointsDataFrame 
coordinates(df_m04)=~long+lati
# specify the spatial reference system
# if your coordinates are UTM32, then us "EPSG:25832"
# ours are in lat/lon  WGS84
# see https://epsg.io/
#proj4string(df)<- CRS("EPSG:4326")
proj4string(df_m04)<- CRS("+proj=longlat +datum=WGS84")
#plot(df_m04)
# copy the data fram
sdf <- df_m04

# make a SpatialPointsDataFrame but in a different projection
# coordinates(sdf) <- ~lon+lat
proj4string(sdf)<- CRS("EPSG:4326") # set CRS to WGS84
#plot(sdf)
# save as shape file
raster::shapefile(df_m04, paste0(wd00,"/obs_points_N_Norway.shp"),overwrite=TRUE )
#_______________________________________________________________________________
# define dummy predicted locations from the dummy input observations
#_______________________________________________________________________________
#head(tibbl_m05,4)
#make standard deviation for latitude 
sdlap <- (max(tibbl_m05$lati)-min(tibbl_m05$lati))*0.32
mlat <- mean(tibbl_m05$lati)
#make standard deviation for longitude
sdlop <- (max(tibbl_m05$long)-min(tibbl_m05$long))*0.42
mlon <- mean(tibbl_m05$long)
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
# plot(df_pred02, col="blue")
# say which columns are coordinates
coordinates(df_pred01)=~predict_lon+predict_lat
# specify the spatial reference system
# if your coordinates are UTM32, then us "EPSG:25832"
# ours are in lat/lon  WGS84
# see https://epsg.io/
#proj4string(df_pred01)<- CRS("EPSG:4326")
proj4string(df_pred01)<- CRS("+proj=longlat +datum=WGS84")
#see the points plotted
#plot(df_pred01)
# e.g. if you want to transform to UTM32
new_prj_prd <- CRS("EPSG:25832")
dfUTM32_prd<-spTransform(df_pred01,new_prj_prd)
# save as shape file
raster::shapefile(df_pred01, "predicted_obs_points_N_Norway.shp",overwrite=TRUE )
#_______________________________________________________________________________
#_______________________________________________________________________________
# To get a map of rivers in Norway look at these websites
# see: https://gis.stackexchange.com/questions/317744/seeking-norwegian-waterlines-and-water-bodies-shapefile
# and see: https://kartkatalog.geonorge.no/metadata/norges-vassdrags-og-energidirektorat/vannforekomster/b203e422-5270-4efc-93a5-2073725c43ef
# and see: https://nedlasting.nve.no/gis/
# If you use 'https://nedlasting.nve.no/gis/' it is a 4 step process to get the 
# map data
# 01 - first select the data under (1) - I selected 'Vektordata' and ticked in 'Elv', 'Insjø' and other water bodies, and then selected 'NESTE'
# 02 - Then under (2) I then selected ESRI .shp file, and WGS84 breddegrader/lengdegrader, and under utvalgsmetode I selected 'Helt innenfor', I select 'Velg på kart' under 'Velg dekningsområde' and zoomed th map to fit the region I wanted to focus on , then selected 'NESTE'
# 03 - this part is simple. Fill out your contact details to get the file
# 04 - this part is also simple, just click to complete and get the file posted to you
# The river maps are also placed here
#https://www.dropbox.com/scl/fi/7xafevro61agrehhlrdaw/NVE_60751B14_1689328325787_768.zip?rlkey=ho28jcm7ij5r2mujjmhdljpc8&dl=0
unlink("NVEData", recursive=TRUE)
unlink("Metadata", recursive=TRUE)
#Check if a path for the directory for the rivers have been defined


# set a variable to define whether the remote drob box file should be downloaded
get_Norway_rivers_from_drobbox <- T
# if not then get the zip file. Unpack it. Make a path to the river shape 
#directory and then remove the zip file
if (get_Norway_rivers_from_drobbox==T)
{print("getting shape file with rivers from drobox")
  unlink("NVE_rivers.zip")
  NVE_zip_file <- "https://www.dropbox.com/scl/fi/7xafevro61agrehhlrdaw/NVE_60751B14_1689328325787_768.zip?rlkey=ho28jcm7ij5r2mujjmhdljpc8&dl=0"
  download.file(NVE_zip_file, dest="NVE_rivers.zip", method="wget", quiet=T)
  unzip ("NVE_rivers.zip", exdir = "./")
  unlink("NVE_rivers.zip")
  #define the path for the directory where the unzipped downloaded river files are placed
  rpath_Norway_rivers <- "NVEData/Elv"
}
#define the path for the directory where the unzipped downloaded river files are placed
#paste path together
rpath_NR <- paste(wd00,"/",rpath_Norway_rivers,sep="")
#rpath_NR <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fish_eDNA_210130/NVE_60751B14_1635507782111_11488/NVEData/Elv"
# read in the river shape files 
rnet = rgdal::readOGR(rpath_NR, "Elv_Hovedelv")
rpath_Norway_rivers <- "NVEData/Elv"
# remove directories recursively
unlink("Metadata", recursive=TRUE)
unlink("NVEData", recursive=TRUE)

# #turn off previous plots
# #dev.off()
outfilpth <- paste0(wd00,"/Fig01_v01_plot_rivers_obs_and_predicts.png")
# # # Exporting PNG files via postscript()
png(c(outfilpth))
    # 1 mm = 0.03937008 inches
    #width=(210*0.03937008),
    #height=(297*0.03937008))
    # #____________________________________________________________________
    op <- par(mfrow=c(1,1), # set number of panes inside the plot - i.e. c(2,2) would make four panes for plots
              oma=c(1,1,0,0), # set outer margin (the margin around the combined plot area) - higher numbers increase the number of lines
              mar=c(1,1,1,1) # set the margin around each individual plot
    )
    #plot the river network
    plot(rnet, col="blue")
    # add the predicted points to the plot
    plot(df_pred01, add=T, col="yellow")
    # add the observed points to the plot
    plot(sdf, add=T, pch=21, col="red")
    # #apply the par settings for the plot as defined above.
    par(op)
#close the png
dev.off()

#__________  __________
#__________  __________
#__________  __________
# try an example with the 'rtop' package
#https://rdrr.io/rforge/rtop/man/rtop-package.html
# I commented this part out, as it worked alright, but did not give the
# intended result. It returns smaller iregular polygons inside a country
# representing municipalities or similar, color coded to a z-value

# But I wanted: Something that resembles Fig 6, Fig 7 and Fig 9 in this paper:
# https://hess.copernicus.org/articles/10/277/2006/

#  example with the 'rtop' package
#https://rdrr.io/rforge/rtop/man/rtop-package.html

# library(rgdal)
# rpath = system.file("extdata",package="rtop")
# observations = readOGR(rpath,"observations")
# class(observations) # it is a '"SpatialPolygonsDataFrame"'
# # head(observations,3)
# # ID EZGID AREASQKM XSTATION YSTATION QSUMMER_OB         obs
# # 0  60    60 43.98384   444255   519555     0.3675 0.008355342
# # 1 113   113 13.64314   410740   471559     0.1676 0.012284566
# # 2 227   227 37.40618   507123   505187     0.4241 0.011337700
# # Create a column with the specific runoff:
# observations$obs = observations$QSUMMER_OB/observations$AREASQKM
# predictionLocations = readOGR(rpath,"predictionLocations")
# class(predictionLocations) # it is a '"SpatialPolygonsDataFrame"'
# # head(predictionLocations,3)
# # ID EZGID AREASQKM XSTATION YSTATION
# # 0 76    76 35.98214   490602   523496
# # 1 77    77 38.48705   490602   523496
# # 2 82    82 15.90664   442364   514959
# params = list(gDist = TRUE, cloud = TRUE)
# rtopObj = createRtopObject(observations, predictionLocations, 
#                            params = params)
# 
# rtopObj = rtopVariogram(rtopObj) # This is fast to run
# rtopObj = rtopFitVariogram(rtopObj) # This takes a while to run
# rtopObj = rtopKrige(rtopObj) # # This is fast to run
# spplot(rtopObj$predictions, col.regions = bpy.colors(), 
#        c("var1.pred","var1.var"))
# rtopObj = rtopKrige(rtopObj, cv = TRUE)
# spplot(rtopObj$predictions, col.regions = bpy.colors(), 
#        c("var1.pred","var1.var"))

# This part above did not return the plot I hoped for
#__________




#__________
#
