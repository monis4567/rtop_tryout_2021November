#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

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
#____________________________________________________________________________#
#remove everything in the working environment, without a warning!!
rm(list=ls())

# install package if required
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
library(ggplot2)
library(rgdal)
library(dplyr)
#define working directory
wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fish_eDNA_210130/rtop_on_virik_eDNA/rtop_tryout_2021November"
rpath <- wd00
# set working directory
setwd(wd00)
getwd()
#define input files
lstfil01 <- list.files(".")
inf1 <- lstfil01[grepl("\\.txt",lstfil01)]
# make a number to count elements
i <- 1
# make an empty list
lsf2 <- list()
# iterate over files
for (f in inf1){
  #read in the file as a df
  dfs01 <- as.data.frame(read.csv(f,
                                  header = TRUE, 
                                  sep = "\t", quote = "\"",
                                  dec = ".", fill = TRUE, 
                                  comment.char = "", 
                                  stringsAsFactors = FALSE))
  # add the filename as a varirable
  dfs01$flnm <- as.character(f)
  colnames(dfs01) <- gsub("stasjon","station",colnames(dfs01))
  #make sure all columns are characters
  # to allow dplyr to combine them all
  #https://stackoverflow.com/questions/43789278/convert-all-columns-to-characters-in-a-data-frame
  dfs01 <- dfs01 %>%
    mutate_all(as.character)
  # add it to the list
  lsf2[[i]] <- dfs01 
  # add to the increasing count
  i <- i+1
  #end iteration over files
}
library(dplyr)
#bind rows together 
# with dplyr 
# https://stackoverflow.com/questions/16138693/rbind-multiple-data-sets
tbl_lsf2 <- bind_rows(lsf2)
df02 <- as.data.frame(tbl_lsf2)
#nrow(df02)
#View(df02)
#sustitute to get species name
df02$gen_specnm <- gsub("^eDNAconc_([A-Za-z]+)_[A-Za-z]+\\.txt","\\1",df02$flnm)
df02$vatten <- gsub("^eDNAconc_([A-Za-z]+)_([A-Za-z]+)\\.txt","\\2",df02$flnm)
#unique(df02$gen_specnm) 
df02$gen_specnm <- gsub("pinksal","Ogorbuscha",df02$gen_specnm )
df02$gen_specnm <- gsub("achar","Salpinus",df02$gen_specnm )
df02$gen_specnm <- gsub("trout","Omykiss",df02$gen_specnm )
# specify columns to make numeric
columnstonum <- c("Sample","Cq","SQ","sample_rep","tech_rep","dyp",
                  "volum","SQ_vol","lat","lon","dist_hav")
#make specified columns numeric
df02[, columnstonum] <- lapply(columnstonum, function(x) as.numeric(df02[[x]]))
####################################################################################

######################################################################################
df02$sample_rep2 <- as.character(df02$sample_rep)
df02$station[grepl("virik",df02$vatten)]
df02$stat_insj <- paste(df02$station,"_",df02$innsjo,sep="")
df02$stat_vatt <- paste(df02$station,"_",df02$vatten ,sep="")
#read in file with positions for sampling sites
#define list of flies
lfx <- list.files(".")[grepl("xls",list.files("."))]
lfxp <- lfx[grepl("lokaliteter",lfx)]
#read in excel sheet as tibble
tb_prlo2021 <- readxl::read_xlsx(lfxp, sheet="samples 2021")
tb_prlo2020 <- readxl::read_xlsx(lfxp, sheet="samples 2020")
# make it a data frame
df_pro2020 <- as.data.frame(tb_prlo2020)
df_pro2021 <- as.data.frame(tb_prlo2021)
#paste station and insjoe together
df_pro2020$st_insj  <- paste(df_pro2020$Stasjon,"_",df_pro2020$Innsjø,sep="")
df_pro2021$st_insj  <- paste(df_pro2021$Stasjon,"_",df_pro2021$Innsjø,sep="")
#make sure all columns are characters
# to allow dplyr to combine them all
#https://stackoverflow.com/questions/43789278/convert-all-columns-to-characters-in-a-data-frame
df_pro2020 <- df_pro2020 %>%
  mutate_all(as.character)
df_pro2021 <- df_pro2021 %>%
  mutate_all(as.character)
#
library(dplyr)
#View(df_pro2021)
#bind rows together 
# with dplyr 
lstdf3 <- list(df_pro2020,df_pro2021)
# https://stackoverflow.com/questions/16138693/rbind-multiple-data-sets
tbl_pro2 <- dplyr::bind_rows(lstdf3)#, .id="`Løpenr (2019)`")
#make it a data frame
dfpro02 <- as.data.frame(tbl_pro2)
# get unique station locations
st1.2020 <- unique(df_pro2020$st_insj)
st1.2021 <- unique(df_pro2021$st_insj)
# bind columns to a data frame
df_st2.2020 <- as.data.frame(cbind(st1.2020))
df_st2.2021 <- as.data.frame(cbind(st1.2021))
# add latitude 
df_st2.2020$dlat <- df_pro2020$Posisjon...31[match(df_st2.2020$st1.2020,df_pro2020$st_insj)] 
df_st2.2021$dlat <- df_pro2021$Posisjon...34[match(df_st2.2021$st1.2021,df_pro2021$st_insj)] 
#match longitude
df_st2.2020$dlon <- df_pro2020$Posisjon...32[match(df_st2.2020$st1.2020,df_pro2020$st_insj)] 
df_st2.2021$dlon <- df_pro2021$Posisjon...35[match(df_st2.2021$st1.2021,df_pro2021$st_insj)] 
#match the municipality area
df_st2.2020$Fylke <- df_pro2020$Fylke[match(df_st2.2020$st1.2020,df_pro2020$st_insj)] 
df_st2.2021$Fylke <- df_pro2021$Fylke[match(df_st2.2021$st1.2021,df_pro2021$st_insj)] 
#make a column for years
df_st2.2020$year <- 2020
df_st2.2021$year <- 2021
#replace column names
colnames(df_st2.2020)[1] <- "st_insj"
colnames(df_st2.2021)[1] <- "st_insj"
#bind rows together
df_st2.2020_2021 <- rbind(df_st2.2020,df_st2.2021)
df_st2 <- df_st2.2020_2021
df02$stat_vatt2 <- df02$stat_vatt
df02$stat_vatt2 <-  gsub("_virik","_Virikbekken",df02$stat_vatt2)
df02$stat_vatt2 <-  gsub("_akersvannet","_Akersvannet",df02$stat_vatt2)
df_st2$st_insj[grepl("Torskevannet",df_st2$st_insj)] <- "Torskevannet"
df02$stat_vatt2[grepl("TV",df02$stat_vatt2)]  <- "Torskevannet"
df_st2$st_insj[grepl("Loven",df_st2$st_insj)] <- "Lovensjoen"
df02$stat_vatt2[grepl("LS",df02$stat_vatt2)]  <- "Lovensjoen"
df_st2$st_insj[grepl("Bretjørna",df_st2$st_insj)] <- "Bretjørna"
df02$stat_vatt2[grepl("BT",df02$stat_vatt2)]  <- "Bretjørna"
#ensure the lat and lon are numeric
df_st2$dlat <- as.numeric(as.character(df_st2$dlat))
df_st2$dlon <- as.numeric(as.character(df_st2$dlon))
# match the lat and long to df with qCR data
df02$dlat <-  df_st2$dlat[match(df02$stat_vatt2,df_st2$st_insj)]
df02$dlon <-  df_st2$dlon[match(df02$stat_vatt2,df_st2$st_insj)]
# split column with 'prove nummer'
dfprNo01 <- data.frame(do.call('rbind', strsplit(as.character(dfpro02$Prøvenr),', ',fixed=TRUE)))
#modify NAs in catch positions
dfpro02$Posisjon...31[is.na(dfpro02$Posisjon...31)] <- ""
dfpro02$Posisjon...32[is.na(dfpro02$Posisjon...32)] <- ""
dfpro02$Posisjon...34[is.na(dfpro02$Posisjon...34)] <- ""
dfpro02$Posisjon...35[is.na(dfpro02$Posisjon...35)] <- ""
#make them characters
dfpro02$Posisjon...31 <- as.character(dfpro02$Posisjon...31)
dfpro02$Posisjon...32 <- as.character(dfpro02$Posisjon...32)
dfpro02$Posisjon...34 <- as.character(dfpro02$Posisjon...34)
dfpro02$Posisjon...35 <- as.character(dfpro02$Posisjon...35)
#paste them together
dfpro02$Posisjon.lat <- paste(dfpro02$Posisjon...31,dfpro02$Posisjon...34,sep="")
dfpro02$Posisjon.lon <- paste(dfpro02$Posisjon...32,dfpro02$Posisjon...35,sep="")

#bind together columns with filtered volume
dfPV1 <- cbind(dfprNo01[1],dfpro02$`Volum 1`)
dfPV2 <- cbind(dfprNo01[2],dfpro02$`Volum 2`)
dfPV3 <- cbind(dfprNo01[3],dfpro02$`Volum 3`)

dfPlat1 <- cbind(dfprNo01[1],dfpro02$Posisjon.lat)
dfPlat2 <- cbind(dfprNo01[2],dfpro02$Posisjon.lat)
dfPlat3 <- cbind(dfprNo01[3],dfpro02$Posisjon.lat)

dfPlon1 <- cbind(dfprNo01[1],dfpro02$Posisjon.lon)
dfPlon2 <- cbind(dfprNo01[2],dfpro02$Posisjon.lon)
dfPlon3 <- cbind(dfprNo01[3],dfpro02$Posisjon.lon)

#change column names
colnames(dfPV1) <- c("ProvNo","VolFilt_mL")
colnames(dfPV2) <- c("ProvNo","VolFilt_mL")
colnames(dfPV3) <- c("ProvNo","VolFilt_mL")

colnames(dfPlat1) <- c("ProvNo","Plat")
colnames(dfPlat2) <- c("ProvNo","Plat")
colnames(dfPlat3) <- c("ProvNo","Plat")

colnames(dfPlon1) <- c("ProvNo","Plon")
colnames(dfPlon2) <- c("ProvNo","Plon")
colnames(dfPlon3) <- c("ProvNo","Plon")

#append as more rows and make it a data frame
dfprNo02 <- as.data.frame(rbind(dfPV1,dfPV2,dfPV3))
dfprNo.lat <- as.data.frame(rbind(dfPlat1,dfPlat2,dfPlat3))
dfprNo.lon <- as.data.frame(rbind(dfPlon1,dfPlon2,dfPlon3))

# specify columns to make numeric
columnstonum <- c("ProvNo","Plon")
#make specified columns numeric
dfprNo.lon[, columnstonum] <- lapply(columnstonum, function(x) as.numeric(dfprNo.lon[[x]]))
# specify columns to make numeric
columnstonum <- c("ProvNo","Plat")
#make specified columns numeric
dfprNo.lat[, columnstonum] <- lapply(columnstonum, function(x) as.numeric(dfprNo.lat[[x]]))
#match lat and long back to data frame with sample numbers
dfprNo02$Plat <- dfprNo.lat$Plat[match(dfprNo02$ProvNo,dfprNo.lat$ProvNo)]
dfprNo02$Plon <- dfprNo.lon$Plon[match(dfprNo02$ProvNo,dfprNo.lon$ProvNo)]
#ensure columns are numeric values
dfprNo02$ProvNo <- as.numeric(dfprNo02$ProvNo)
dfprNo02$VolFilt_mL <- as.numeric(dfprNo02$VolFilt_mL)
#match filtered volume to main data frame
df02$VolFilt_mL <- dfprNo02$VolFilt_mL[match(df02$Sample,dfprNo02$ProvNo)]
df02$VolFilt_mL[is.na(df02$VolFilt_mL)] <- 0
#match lat and long to main data frame
df02$dlat <- dfprNo02$Plat[match(df02$Sample,dfprNo02$ProvNo)]
df02$dlon <- dfprNo02$Plon[match(df02$Sample,dfprNo02$ProvNo)]
#make the columns with ølatitude and longitude numeric
df02$dlat <- as.numeric(df02$dlat)
df02$dlon <- as.numeric(df02$dlon)
#remove rows that do not have a lat and long postion
df02 <- df02[!is.na(df02$dlat),] 
df02 <- df02[!is.na(df02$dlon),] 
#get max and min lat and lon
mxlat <- max(df02$dlat)
mnlat <- min(df02$dlat)
mxlon <- max(df02$dlon)
mnlon <- min(df02$dlon)
#add and subtract on from min and max
mxlatp1 <- mxlat+1
mnlatp1 <- mnlat-1
mxlonp1 <- mxlon+1
mnlonp1 <- mnlon-1
#round down
mnlonp1 <- floor(mnlonp1)
mnlatp1 <- floor(mnlatp1)
#round up
mxlonp1 <- ceiling(mxlonp1)
mxlatp1 <- ceiling(mxlatp1)

df02$gen_specnm <- as.character(df02$gen_specnm)
df02$gen_specnm <- gsub("eDNAconc_(.*)_(.*).txt","\\1",df02$gen_specnm)
flet1 <- gsub("(^[A-Za-z]{1}).*","\\1",df02$gen_specnm)
flet2 <- gsub("(^[A-Za-z]{1})(.*)","\\2",df02$gen_specnm)
#Change to uppercase and paste strings together
df02$gen_specnm2 <- paste(toupper(flet1),". ",flet2,sep="")
#replace nas w zero
df02$SQ_vol[is.na(df02$SQ_vol)] <- 0
# make numeric
df02$SQ_vol <- as.numeric(df02$SQ_vol)
df02$SQ_v.l10p1 <- log10(df02$SQ_vol+1)
# define plot symbols
df02$pch.symb <- NA
df02$pch.symb[df02$SQ_vol==0] <- 3
df02$pch.symb[df02$SQ_vol>0] <- 21
df02$pch.symb <- as.character(df02$pch.symb)
df02$eval1 <- NA
df02$eval1[df02$SQ_vol==0] <- "zero"
df02$eval1[df02$SQ_vol>0] <- "above zero"
#head(df02,4)

#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", 
                "#009E73", "#F0E442", "#0072B2", 
                #"#D55E00", 
                "#CC79A7")

cl <- cbbPalette
# get volume filtered in Liter
df02$VolFilt_L <- df02$VolFilt_mL/1000
#Elianne eluerte i 150 microL TE elueringsbuffer. 
# Brukte 5 microL som templat i hver qPCR reaksjon.
# Så er der 150/5 * kopier i en ekstraktion
# En ekstraktion repræsenterer 'df02$VolFilt_L' af 1 L
#calculate copies  per Liter
df02$SQpL <- df02$SQ*(150/5)*(1/df02$VolFilt_L)
#calculate copies  per mili Liter
df02$SQpmL <- df02$SQpL*1000
#paste sample number together
df02$Sstrp <- paste(df02$Sample,"_",df02$sample_rep,"_",df02$tech_rep,sep="")
#check latitude
#"akersvannet"  er vest for Oslo
# unique(df02$dlat[grepl("akers",df02$vatten)])
# unique(df02$dlon[grepl("akers",df02$vatten)])
# "finnsval"   er en rodebutik der består af
# både prøver fra svalbard plus fra  grænsen mellem Norge og Finland
# unique(df02$dlat[grepl("fin",df02$vatten)])
# unique(df02$dlon[grepl("fin",df02$vatten)])
#"virik"  er vest for Oslo
# unique(df02$dlat[grepl("viri",df02$vatten)])
# unique(df02$dlon[grepl("viri",df02$vatten)])
#unique(df02$vatten)
#df02
# add jitter to positions
# The rtop package requires that several points with data along rivers are
# available. Although all these samples and rpelicates have been
# collected at the same postion
# they need to have slightly different positions to make up a larger
# sample size to allow for creating the rtop object
df02$jdlat<- jitter(df02$dlat,0.6)
df02$jdlon<- jitter(df02$dlon,0.6)
#limit to only compirse Virik samples
df03 <- df02[grepl("viri",df02$vatten),]
 

#_______________________________________________________________________________
#append and rename in data frame
df03$latitude <- df03$jdlat
df03$longitude <- df03$jdlon

#define vector with columns to keep
keeps <- c("latitude",
           "longitude",
           "SQpmL")
#limit the df to only the columns that are to be kept
df04 <- df03[keeps]
df05 <- df04
#View(df05)
# say which columns are coordinates
coordinates(df04)=~longitude+latitude
# specify the spatial reference system
# if your coordinates are UTM32, then us "EPSG:25832"
# ours are in lat/lon  WGS84
# see https://epsg.io/
#proj4string(df)<- CRS("EPSG:4326")
proj4string(df04)<- CRS("+proj=longlat +datum=WGS84")
plot(df04)

sdf <- df04
# coordinates(sdf) <- ~lon+lat
proj4string(sdf)<- CRS("EPSG:4326") # set CRS to WGS84
plot(sdf)
# save as shape file
raster::shapefile(df04, "obs_points_S_Norway.shp",overwrite=TRUE )

#_______________________________________________________________________________
# https://stackoverflow.com/questions/47203587/r-delimit-a-voronoi-diagram-according-to-a-map
# Try cutting voronoi tiles in Norway
library(dismo)
library(rgeos)
library(deldir)
library(maptools)

df06 <- df05
coordinates(df06) <- c("longitude", "latitude")
proj4string(df06) <- CRS("+proj=longlat +datum=WGS84")

data(wrld_simpl)
nor <- wrld_simpl[wrld_simpl$ISO3 == 'NOR', ]

# transform to a planar coordinate reference system (as suggested by @Ege Rubak)
prj <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80  +units=m")
prj <- CRS("+proj=longlat +datum=WGS84")
df06 <- spTransform(df06, prj)
nor <- spTransform(nor, prj)
# voronoi function from 'dismo'
# note the 'ext' argument to spatially extend the diagram
vor <- dismo::voronoi(df06, ext=extent(nor) + 10)
# use intersect to maintain the attributes of the voronoi diagram
r <- raster::intersect(vor, nor)
plot(r, col=rainbow(length(r)), lwd=3)
points(df03, pch = 20, col = "white", cex = 3)
points(df03, pch = 20, col = "red", cex = 2)
#df03$CONC
# or, to see the names of the areas
spplot(r, 'SQpmL')
# save as shape file
raster::shapefile(r, "obs_points_S_Norway_02.shp",overwrite=TRUE )

#_______________________________________________________________________________


#_______________________________________________________________________________
# define dummy predicted locations from the dummy input observations
#_______________________________________________________________________________
#make standard deviation for latitude 
sdlap <- (max(df05$latitude)-min(df05$latitude))*0.4
mlat <- mean(df05$latitude)
#make standard deviation for longitude
sdlop <- (max(df05$longitude)-min(df05$longitude))*0.4
mlon <- mean(df05$longitude)
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
raster::shapefile(df_pred01, "predicted_obs_points_S_Norway.shp",overwrite=TRUE )
#_______________________________________________________________________________
# Try cutting voronoi tiles in Norway
library(dismo)
library(rgeos)
library(deldir)
library(maptools)

df_pred03 <- df_pred02
df_pred03$latitude <- df_pred02$predict_lat
df_pred03$longitude <- df_pred02$predict_lon
df_pred03$predict_lat <- NULL
df_pred03$predict_lon <- NULL
coordinates(df_pred03) <- c("longitude", "latitude")
proj4string(df_pred03) <- CRS("+proj=longlat +datum=WGS84")

data(wrld_simpl)
nor <- wrld_simpl[wrld_simpl$ISO3 == 'NOR', ]

# transform to a planar coordinate reference system (as suggested by @Ege Rubak)
prj <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80  +units=m")
prj <- CRS("+proj=longlat +datum=WGS84")
df_pred03 <- spTransform(df_pred03, prj)
nor <- spTransform(nor, prj)
# voronoi function from 'dismo'
# note the 'ext' argument to spatially extend the diagram
vor <- dismo::voronoi(df_pred03, ext=extent(nor) + 10)
# use intersect to maintain the attributes of the voronoi diagram
r <- raster::intersect(vor, nor)
plot(r, col=rainbow(length(r)), lwd=3)
points(df_pred03, pch = 20, col = "white", cex = 3)
points(df_pred03, pch = 20, col = "red", cex = 2)
# or, to see the names of the areas
#spplot(r, 'CONC')
# save as shape file
raster::shapefile(r, "pred_obs_points_S_Norway_02.shp",overwrite=TRUE )

#_______________________________________________________________________________
# To get a map of rivers in Norway look at these websites
# see: https://gis.stackexchange.com/questions/317744/seeking-norwegian-waterlines-and-water-bodies-shapefile
# and see: https://kartkatalog.geonorge.no/metadata/norges-vassdrags-og-energidirektorat/vannforekomster/b203e422-5270-4efc-93a5-2073725c43ef
# and see: https://nedlasting.nve.no/gis/
# The river map is also placed here
# https://www.dropbox.com/sh/jwow45jfof8bvcj/AADhrNp17Pc5f5nG3p25Grmfa?dl=0
unlink("NVEData", recursive=TRUE)
unlink("Metadata", recursive=TRUE)
#Check if a path for the directory for the rivers have been defined

#define the path for the directory where the unzipped downloaded river files are placed
#rpath_Norway_rivers <- "NVE_60751B14_1635507782111_11488/NVEData/Elv"
#paste path together
rpath_NR <- paste(wd00,"/",rpath_Norway_rivers,sep="")
rpath_NR <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fish_eDNA_210130/NVE_60751B14_1635507782111_11488/NVEData/Elv"
# read in the river shape files 
rnet = rgdal::readOGR(rpath_NR, "Elv_Hovedelv")
rpath_Norway_rivers <- "NVEData/Elv"
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
class(rnet)
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
stations_obs02 <- st_as_sf( df05, coords = c( "longitude", "latitude" ) )
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

obLoc4=rgdal::readOGR(rpath, "obs_points_S_Norway_02")
prLoc4=rgdal::readOGR(rpath, "pred_obs_points_S_Norway_02")

dev.off()
#plot the river network
plot(rnet, col="blue")
# add the predicted points to the plot
plot(df_pred01, add=T, col="yellow")
# add the observed points to the plot
plot(sdf, add=T, pch=21, col="red")
# add polygons on top
plot(obLoc4, add=T, border = "pink")
plot(prLoc4, add=T, border = "brown")
# replace NAs with zero
df05$SQpmL[is.na(df05$SQpmL)] <- 0
#make numeric
df05$SQpmL <- as.numeric(df05$SQpmL)

# copy to new vector
obs <- df05$SQpmL
observations <- obs

#make rtopObj
rtopObj2	=	rtop::createRtopObject(obLoc4, prLoc4,
                                  formulaString = obs~1, 
                                  params = list(gDist=TRUE, rresol = 25))
#check if the rtopObj2 actually ended up with something 
# it should return FALSE if it is not NULL
is.null(rtopObj2)

#_______________________________________________________________________________
#https://rdrr.io/rforge/rtop/man/rtop-package.html
# There are help-methods available in cases when data are not available as
# shape-files, or when the observations are not part of the shape-files. 
# See readAreaInfo and readAreas.
rtop::readAreaInfo(obLoc4)
rtop::readAreas(prLoc4)

# Compare the pred and obs polygons from the 'rtop' example
# with the polygons for obs and pred generated by your own data
# Looks like the variables 'EZGID' and 'XSTATION' and 'YSTATION'
# are missing
obLoc4@proj4string
ncol(obLoc4@data)
colnames(obLoc4@data)
ncol(prLoc4@data)
colnames(prLoc4@data)
#_______________________________________________________________________________
rtopObj3 = rtop::rtopFitVariogram(rtopObj2)

rtopObj4	=	rtop::checkVario(rtopObj3, cloud = TRUE, identify = TRUE,
                     acor = 0.000001)

rtopObj4 = rtop::checkVario(rtopObj3, acor = 0.000001,
                     acomp = data.frame (acl1 = c(2,2,2,2,3,3,3,4,4),
                                         acl2 =c(2,3,4,5,3,4,5,4,5)))
rtopObj <- rtopObj3 
rtopObj = rtop::rtopKrige(rtopObj, cv=TRUE)
predictions = rtopObj$predictionLocations
#predictions = rtopObj$predictions
sstot = sum((predictions$obs - mean(predictions$obs))^ 2)
rtopsserr = sum((predictions$obs - predictions$var1.pred)^ 2)
rtoprsq = 1 - rtopsserr/sstot
summary(predictions)
#_______________________________________________________________________________
pred = rtopObj$predictions
pred <- rtopObj$predictionLocations
is.null(pred)
# this is NULL????? 
# what went wrong?

rtopObj = checkVario(rtopObj, cloud = TRUE,
                     identify = TRUE,
                     acor = 0.000001)


rtopObj = rtopKrige(rtopObj, cv=TRUE)
predictions = rtopObj$predictions
sstot = sum((predictions$obs - mean(predictions$obs))^ 2)
rtopsserr = sum((predictions$obs - predictions$var1.pred)^ 2)
rtoprsq = 1 - rtopsserr/sstot
summary(predictions)
library(automap)
automap::automapPlot(predictions)
plot(predictions)

rtopObj = rtop::rtopKrige(rtopObj)


#rnet
pred = rtopObj$predictions
#pred$
rnet$pred = pred$var1.pred[match(rnet$EZGA, pred$EZGID)]
spplot(rnet, "pred", col.regions = bpy.colors())


at = seq(0, max(rnet$pred, na.rm =TRUE), 0.01)
cols = bpy.colors(length(at))
cobs = observations@data[, c("XSTATION", "YSTATION", "obs")]
names(cobs) = c("x", "y", "obs")
coordinates(cobs) = ~ x + y
cobs$class = findInterval(cobs$obs, at)


spplot(rnet, "pred", col.regions = bpy.colors(), at = at,
       panel = function(x, y, ...){
         panel.polygonsplot(x, y, ...)
         sp.points(cobs[, "obs"], cex = 1, pch = 16, col = cols[cobs$class])})

writeOGR(rnet, dsn, layer, "ESRI Shapefile")

#_______________________________________________________________________________

# haven't worked out yet how we match the results to the stream network...

# rnet = readOGR(rpath, "riverNetwork")
# rnet$pred = pred$var1.pred[match(rnet$EZGA, pred$EZGID)]



