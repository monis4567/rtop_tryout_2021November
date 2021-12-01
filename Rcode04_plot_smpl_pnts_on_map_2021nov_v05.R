#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# on eDNA from Virik
# Authors: Steen Wilhelm Knudsen.

# Change the working directory to a path on your own computer , and run
# the individual parts below to reproduce the diagrams presented in the paper
#
# All input data required needs to be available as csv-files in the same directory 
# as this R-code use for working directory.
#
# Occassionally the code will have difficulties producing the correct diagrams,
# if the packages and libraries are not installed.
# Make sure the packages are installed, and libraries are loaded, if the R-code
# fails in producing the diagrams.
#
#________________IMPORTANT!!_________________________________________________#
# (1)
#You have to change the path to the working directory before running this code
#
# (2)
# must be located in the same working directory - as specified in the code below
#
#This code is able to run in:
#
#____________________________________________________________________________#
#remove everything in the working environment, without a warning!!
rm(list=ls())
#see this
#website
#on how to only install required packages
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  scales, 
  fields, 
  gplots,
  plyr)
## install the package 'scales', which will allow you to make points on your plot more transparent
#install.packages("scales")
if(!require(scales)){
  install.packages("scales")
  library(scales)
}
library(scales)
#install.packages("fields")
if(!require(fields)){
  install.packages("fields")
  library(fields)
}
library(fields)
## install the package 'gplots', to be able to translate colors to hex - function: col2hex
#install.packages("gplots")
if(!require(gplots)){
  install.packages("gplots")
  library(gplots)
}
library(gplots)
## install the package 'glad', to be able to color using the function 'myPalette'
#install.packages("glad")
#library(glad)
if(!require(graphics)){
  install.packages("graphics")
  library(graphics)
}
library(graphics)

## install the package 'marmap', which will allow you to plot bathymetric maps
#install.packages("marmap")
#library(marmap)
#get the package that enables the function 'subplot'
#install.packages("TeachingDemos")
#library(TeachingDemos)
#get package to make maps
#install.packages("rworldmap")
#require (rworldmap)
#install.packages("rworldxtra")
#require(rworldxtra)
#get package to read excel files
#install.packages("readxl")
#library(readxl)
#get package to do count number of observations that have the same value at earlier records:
# see this website: https://stackoverflow.com/questions/11957205/how-can-i-derive-a-variable-in-r-showing-the-number-of-observations-that-have-th
#install.packages("plyr")
# install package if required
if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}
library(plyr)
#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("mapdata")
#library(mapdata)
# install package if required
if(!require(officer)){
  install.packages("officer")
  library(officer)
}
#library(ReporteRs)
library(officer)
#install.packages("tableHTML")
#https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
# install package if required
if(!require(tableHTML)){
  install.packages("tableHTML")
  library(tableHTML)
}
# install package if required
if(!require(envDocument)){
  install.packages("envDocument")
  library(envDocument)
}
# install package if required
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
library(ggplot2)

library(dplyr)
#define working directory
wd00="/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fish_eDNA_210130/rtop_on_virik_eDNA"
wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fish_eDNA_210130/rtop_on_virik_eDNA/rtop_tryout_2021November"
#wd00 <- "/Users/steenknudsen/Documents/Documents/NIVA_Ansaettelse_2020/NOVANA_proever_2018_2019"
#wd00 <- "/home/hal9000/test_plot_jpg_instead_of_pdf/"
# define output and input directories
# wd01 <- "out01_std_curve_plots_from_R"
# #make complete path to output dir
# wd00_wd01 <- paste(wd00,"/",wd05,sep="")
# #check the wd
# #Delete any previous versions of the output directory
# unlink(wd00_wd01, recursive=TRUE)
# #Create a directory to put resulting output files in
# dir.create(wd00_wd01)
# set working directory
setwd(wd00)
getwd()
#define input files
lstfil01 <- list.files(".")
inf1 <- lstfil01[grepl("\\.txt",lstfil01)]
#inf1 <- inf1[grepl("virik",inf1)]
#inf1 <- inf1[grepl("akersvan",inf1)]

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

#sustitute to get species name
df02$gen_specnm <- gsub("^eDNAconc_([A-Za-z]+)_[A-Za-z]+\\.txt","\\1",df02$flnm)
df02$vatten <- gsub("^eDNAconc_([A-Za-z]+)_([A-Za-z]+)\\.txt","\\2",df02$flnm)
#unique(df02$gen_specnm) 
df02$gen_specnm <- gsub("pinksal","Ogorbuscha",df02$gen_specnm )
df02$gen_specnm <- gsub("achar","Salpinus",df02$gen_specnm )
df02$gen_specnm <- gsub("trout","Omykiss",df02$gen_specnm )
#getwd()
# specify columns to make numeric
columnstonum <- c("Sample","Cq","SQ","sample_rep","tech_rep","dyp",
"volum","SQ_vol","lat","lon","dist_hav")
#make specified columns numeric
df02[, columnstonum] <- lapply(columnstonum, function(x) as.numeric(df02[[x]]))

####################################################################################

######################################################################################
df02$sample_rep2 <- as.character(df02$sample_rep)

#df02$station <- df02$stasjon

df02$station[grepl("virik",df02$vatten)]

df02$stat_insj <- paste(df02$station,"_",df02$innsjo,sep="")
df02$stat_vatt <- paste(df02$station,"_",df02$vatten ,sep="")

unique(df02$stat_vatt)
unique(df02$stat_insj)
plt01 <- ggplot2::ggplot(df02, aes(x = stat_vatt,
                           y = SQ_vol,
                            group= sample_rep,
                            #color = sample_rep)) +
                            shape=sample_rep2,
                            color = gen_specnm)) +

  geom_point()

plt01

#read in file with positions for sampling sites
#plot(df02$station,df02$SQ_vol, pch=3)
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

#
#make sure all columns are characters
# to allow dplyr to combine them all
#https://stackoverflow.com/questions/43789278/convert-all-columns-to-characters-in-a-data-frame
df_pro2020 <- df_pro2020 %>%
  mutate_all(as.character)
df_pro2021 <- df_pro2021 %>%
  mutate_all(as.character)
#
library(dplyr)
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

#unique(df_st2.2020_2021$st_insj)

df_st2 <- df_st2.2020_2021
#unique(df02$stat_vatt)
df02$stat_vatt2 <- df02$stat_vatt
df02$stat_vatt2 <-  gsub("_virik","_Virikbekken",df02$stat_vatt2)
df02$stat_vatt2 <-  gsub("_akersvannet","_Akersvannet",df02$stat_vatt2)
#unique(df02$stat_vatt2)
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
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot on map -start
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# https://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/
#install packages needed
if(!require(maps)){
  install.packages("maps")
  library(maps)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}


library(ggplot2)
library(maps)


# # #https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# To get rgdal and googleway to work,
#first run these in a terminal:

# $ sudo apt install netcdf-*
# $   sudo apt install libnetcdf-dev
# $ sudo apt install libjq-dev
# $ sudo apt install gdal-bin libgdal-dev libproj-dev
# $ sudo apt install libudunits2-dev
if(!require(cowplot)){
  install.packages("cowplot")
  library(cowplot)
}

if(!require(googleway)){
  install.packages("googleway")
  library(googleway)
}
if(!require(ggrepel)){
  install.packages("ggrepel")
  library(ggrepel)
}
if(!require(ggspatial)){
  install.packages("ggspatial")
  library(ggspatial)
}
# if(!require(libwgeom)){
#   install.packages("libwgeom")
#   library(libwgeom)
# }
if(!require(sf)){
  install.packages("sf")
  library(sf)
}

if(!require(rnaturalearth)){
  install.packages("rnaturalearth")
  library(rnaturalearth)
}
if(!require(rnaturalearthdata)){
  install.packages("rnaturalearthdata")
  library(rnaturalearthdata)
}
#install rgeos
if(!require(rgeos)){
  install.packages("rgeos")
  library(rgeos)
}
#get 'rnaturalearthhires' installed
if(!require(rnaturalearthhires)){
  #install.packages("rnaturalearthhires")
  install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
  library(rnaturalearthhires)
}
# # 
library("ggplot2")
theme_set(theme_bw())
library("sf")

#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
# # 
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
# # Get a map, use a high number for 'scale' for a coarse resolution
# use a low number for scale for a high resolution
# if the map 'world' does not exist, then download it
if (!exists("norw_map"))
{  
  norw_map <- ne_countries(country="norway",scale = 10, returnclass = "sf")  %>% st_as_sf()
}

#change color scheme:
# https://stackoverflow.com/questions/53750310/how-to-change-default-color-scheme-in-ggplot2
#https://data-se.netlify.app/2018/12/12/changing-the-default-color-scheme-in-ggplot2/
library(viridis)
opts <- options()  # save old options

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
#st_crs(norw_map)
# define level of jitter on each point
jitlvl <- 0.004

#count number of species , as you need to know this count
# set the columns and  rows for the facet part in the ggplot
#length(unique(df02$gen_specnm2))
# plot  on map
# also see : https://github.com/tidyverse/ggplot2/issues/2037
p01 <- ggplot(data = norw_map) +
  #p01 <- ggplot(st_transform(norw_map, 4326)) +
  
  geom_sf(color = "black", fill = "azure3") +
  #no transformation
  geom_jitter(data = df02, 
              aes(x = dlon, y = dlat, #, 
                  color=gen_specnm2,
                  fill=SQpmL,
                  shape=eval1),
              width = jitlvl, #0.07, jitter width 
              height = jitlvl, #0.07, # jitter height
              
              #size = df02$SQ_v.l10p1*1000000) +
              size = 3) +
  #define shape of points
  scale_shape_manual(values=c(21,3)) +
  # split in mulitple plots per group
  facet_wrap(~gen_specnm2, nrow = 1, ncol=6) + #'facet_wrap' subsets by column value in dataframe
  #define colour gradient for fill
  scale_fill_gradient2(low = "white", mid = "white", high = "blue",
                       guide = guide_colorbar(order = 1)) +
  
  # scale_fill_manual(values=alpha(
  #   c(cl),
  #   c(0.7)
  # ))+
  # #transformed
  #define limits of map
  # ggplot2::coord_sf(xlim = c(9.5, 10.5),
  #                   ylim = c(59, 59.3), 
  #                   expand = FALSE)
  
ggplot2::coord_sf(xlim = c(4, 33),
                  ylim = c(58, 83), 
                  expand = FALSE)

p01 <- p01 + xlab("longitude") + ylab("latitude")
# see the plot
p01

p01b <- p01
# you will have to change the legend for all legends
p01b <- p01b + labs(color='species')
p01b <- p01b + labs(fill='mean copies/ mL filt water')
p01b <- p01b + labs(shape='eDNA presence')


head(df02,3)
library(dplyr)
# get mean per group
tbl03 <- df02 %>%                   # Specify data frame
  dplyr::group_by(Sample,gen_specnm2) %>%  # Specify group indicator
  dplyr::summarise_at(vars(SQpmL),            # Specify column
                      list(SQm = mean))
#make it a data frame
df03 <- as.data.frame(tbl03)
# set NAs to zero
df03$SQm[is.na(df03$SQm)] <- 0
# make numeric
df03$SQm <- as.numeric(df03$SQm)
#match other values
df03$dlat <- df02$dlat[match(df03$Sample,df02$Sample)]
df03$dlon <- df02$dlon[match(df03$Sample,df02$Sample)]
#define evaluation category
df03$eval1 <- NA
df03$eval1[df03$SQm==0] <- "zero"
df03$eval1[df03$SQm>0] <- "above zero"
df03$SQmean <- df03$SQm
#
#getwd()
write.table(df03,"mean_eDNA_conc_allfish_allcollection_points.csv", sep=",",
            dec=".")
#define vector with S Norway species
NoSsp <- c("E. lucius","S. trutta")
# subset data frame by matching the elements in the vector defined above
df04 <- subset(df03, subset = gen_specnm2 %in% NoSsp)
#_______________________________________________________________________________
# also see : https://github.com/tidyverse/ggplot2/issues/2037
# also see : https://github.com/tidyverse/ggplot2/issues/2037
p02 <- 
  ggplot(data = norw_map) +
  #ggplot(st_transform(norw_map, 9122)) +
  geom_sf(color = "black", fill = "azure3") +
  geom_jitter(data = df04, 
              aes(x = dlon, y = dlat, #, 
                  color=gen_specnm2,
                  fill=SQmean,
                  shape=eval1),
              width = jitlvl, #0.07, jitter width 
              height = jitlvl, #0.07, # jitter height
              size = 3) +
  #define shape of points
  scale_shape_manual(values=c(21,3)) +
  # define outline of each pch point
  scale_color_manual(values=c("purple","red")) +
  # split in mulitple plots per group
  facet_wrap(~gen_specnm2, nrow = 1, ncol=2) + #'facet_wrap' subsets by column value in dataframe
  #define colour gradient for fill
  # scale_fill_gradient2(low = "white", mid = "white", high = "blue",
  #                      guide = guide_colorbar(order = 1)) +
  #scale_fill_gradientn(colors = heat.colors(10),
  scale_fill_gradientn(colors = alpha(c("white","red","black"),0.6),
                       guide = guide_colorbar(order = 1)) +
  
  #define limits of map
  ggplot2::coord_sf( xlim = c(10.15, 10.35) ,
                     #xlim = c(10.3, 10.4),
                     ylim = c(59.065, 59.265), 
                     #ylim = c(59.165, 59.265), 
                     default_crs = sf::st_crs(4326),
                     
                     expand = FALSE)

p02 <- p02 + xlab("longitude") + ylab("latitude")
# see the plot
p02
p02b <- p02
# you will have to change the legend for all legends
p02b <- p02b + labs(color='species')
p02b <- p02b + labs(fill='mean copies/ mL filt water')
p02b <- p02b + labs(shape='eDNA presence')

#https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
# colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
#                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# pie(rep(1, 8), col = colorBlindBlack8)

#define vector with S Norway species
NoNsp <- c("O. gorbuscha","O. mykiss", 
           "S. alpinus", "S. salar")
# subset data frame by matching the elements in the vector defined above
df04 <- subset(df03, subset = gen_specnm2 %in% NoNsp)
#_______________________________________________________________________________
# also see : https://github.com/tidyverse/ggplot2/issues/2037
# also see : https://github.com/tidyverse/ggplot2/issues/2037
p04 <- 
  ggplot(data = norw_map) +
  #ggplot(st_transform(norw_map, 9122)) +
  geom_sf(color = "black", fill = "azure3") +
  geom_jitter(data = df04, 
              aes(x = dlon, y = dlat, #, 
                  color=gen_specnm2,
                  fill=SQmean,
                  shape=eval1),
              width = jitlvl, #0.07, jitter width 
              height = jitlvl, #0.07, # jitter height
              size = 3) +
  #define shape of points
  scale_shape_manual(values=c(21,3)) +
  # define outline of each pch point
  scale_color_manual(values=c("#E69F00", "#56B4E9", 
                              "#009E73", 
                              "#F0E442")) +
  # split in mulitple plots per group
  facet_wrap(~gen_specnm2, nrow = 1, ncol=4) + #'facet_wrap' subsets by column value in dataframe
  #define colour gradient for fill
  # scale_fill_gradient2(low = "white", mid = "white", high = "blue",
  #                      guide = guide_colorbar(order = 1)) +
  #scale_fill_gradientn(colors = heat.colors(10),
  scale_fill_gradientn(colors = alpha(c("white","red","black"),0.6),
                       guide = guide_colorbar(order = 1)) +
  
  #define limits of map
  ggplot2::coord_sf( #xlim = c(26, 32) ,
    #xlim = c(30.5, 32),
    xlim = c(30.5, 31.5),
    #ylim = c(66, 74), 
    #ylim = c(69.4, 69.9), 
    ylim = c(69.4, 69.9), 
    default_crs = sf::st_crs(4326),
    
    expand = FALSE)

p04 <- p04 + xlab("longitude") + ylab("latitude")
# see the plot
p04b <-  p04

# you will have to change the legend for all legends
p04b <- p04b + labs(color='species')
p04b <- p04b + labs(fill='mean copies/ mL filt water')
p04b <- p04b + labs(shape='eDNA presence')

p04b



#define vector with S Norway species
NoNsp <- c("O. gorbuscha","O. mykiss", 
           "S. alpinus", "S. salar")
# subset data frame by matching the elements in the vector defined above
df04 <- subset(df03, subset = gen_specnm2 %in% NoNsp)
#_______________________________________________________________________________
# also see : https://github.com/tidyverse/ggplot2/issues/2037
# also see : https://github.com/tidyverse/ggplot2/issues/2037
p05 <- 
  ggplot(data = norw_map) +
  #ggplot(st_transform(norw_map, 9122)) +
  geom_sf(color = "black", fill = "azure3") +
  geom_jitter(data = df04, 
              aes(x = dlon, y = dlat, #, 
                  color=gen_specnm2,
                  fill=SQmean,
                  shape=eval1),
              width = jitlvl, #0.07, jitter width 
              height = jitlvl, #0.07, # jitter height
              size = 4) +
  #define shape of points
  scale_shape_manual(values=c(21,3)) +
  # define outline of each pch point
  scale_color_manual(values=c("#E69F00", "#56B4E9", 
                              "#009E73", 
                              "#F0E442")) +
  # split in mulitple plots per group
  facet_wrap(~gen_specnm2, nrow = 1, ncol=4) + #'facet_wrap' subsets by column value in dataframe
  #define colour gradient for fill
  # scale_fill_gradient2(low = "white", mid = "white", high = "blue",
  #                      guide = guide_colorbar(order = 1)) +
  #scale_fill_gradientn(colors = heat.colors(10),
  scale_fill_gradientn(colors = alpha(c("white","red","black"),0.6),
                       guide = guide_colorbar(order = 1)) +
  
  #define limits of map
  ggplot2::coord_sf( #xlim = c(26, 32) ,
    #xlim = c(30.5, 32),
    xlim = c(10, 36),
    #ylim = c(66, 74), 
    ylim = c(76.6, 80.8), 
    #ylim = c(69, 83), 
    default_crs = sf::st_crs(4326),
    
    expand = FALSE)

p05 <- p05 + xlab("longitude") + ylab("latitude")
# see the plot
p05b <-  p05

# you will have to change the legend for all legends
p05b <- p05b + labs(color='species')
p05b <- p05b + labs(fill='mean copies/ mL filt water')
p05b <- p05b + labs(shape='eDNA presence')

p05b
#define the path for the directory where the unzipped downloaded river files are placed
#rpath_Norway_rivers <- "NVE_60751B14_1635507782111_11488/NVEData/Elv"
#paste path together
#rpath_NR <- paste(wd00,"/",rpath_Norway_rivers,sep="")
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


# p02b <- p02
# # you will have to change the legend for all legends
# p02b <- p02b + labs(color='species')
# p02b <- p02b + labs(fill='mean copies/ mL filt water')
# p02b <- p02b + labs(shape='eDNA presence')
# Label appearance ##http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
# filltxc = rep("black", noofspcsnms)
# filltxc[10] <- "red"
#p02b <- p02b + theme(legend.text = element_text(colour="blue", size = 10, face = "italic"))

p03 <- p02b + geom_path(data = rnet, aes(x = long, y = lat, group = group),
                       lwd=0.4,
                       color="blue")
p03

getwd()
# ------------- plot Combined figure -------------
library(patchwork)
# set a variable to TRUE to determine whether to save figures
bSaveFigures <- T
p <- p03
p03b <- p03
# # on how to arrange plots in patchwork
# p <-  p01b +
#   #
#   
#   plot_layout(nrow=3,byrow=T) + #xlab(xlabel) +
#   plot_layout(guides = "collect") +
#   plot_annotation(caption=fnm04) #& theme(legend.position = "bottom")
#p
fignm01 <- "mapplot_v01_eDNA_conc_from_6spcs"
fignm02 <- "mapplot_v02_eDNA_conc_from_Elus_Stru"
fignm03 <- "mapplot_v03_eDNA_conc_from_Elus_Stru"
fignm04 <- "mapplot_v04_eDNA_conc_from_4spcs_NNorway"
fignm05 <- "mapplot_v05_eDNA_conc_from_4spcs_Svalb"
#make filename to save plot to
flnm01.png <- paste0(fignm01,".png")
flnm01.pdf <- paste0(fignm01,".pdf")

flnm02.png <- paste0(fignm02,".png")
flnm02.pdf <- paste0(fignm02,".pdf")

flnm03.png <- paste0(fignm03,".png")
flnm03.pdf <- paste0(fignm03,".pdf")

flnm04.png <- paste0(fignm04,".png")
flnm04.pdf <- paste0(fignm04,".pdf")

flnm05.png <- paste0(fignm05,".png")
flnm05.pdf <- paste0(fignm05,".pdf")

lstplts <- c(p01b,p02b,
             p03b,
             p04b,p05b)
lst_pngnms <- c(flnm01.png,
                flnm02.png,
                flnm03.png,
                flnm04.png,
                flnm05.png)

lst_pdfnms <- c(flnm01.pdf,
                flnm02.pdf,
                flnm03.pdf,
                flnm04.pdf,
                flnm05.pdf)
nplt<- length(lstplts)

if(bSaveFigures==T){
  ggsave(p01b,file=flnm01.png,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
  ggsave(p01b,file=flnm01.pdf,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}


if(bSaveFigures==T){
  ggsave(p02b,file=flnm02.png,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
  ggsave(p02b,file=flnm02.pdf,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}


if(bSaveFigures==T){
  ggsave(p03b,file=flnm03.png,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
  ggsave(p03b,file=flnm03.pdf,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}

if(bSaveFigures==T){
  ggsave(p04b,file=flnm04.png,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
  ggsave(p04b,file=flnm04.pdf,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}


if(bSaveFigures==T){
  ggsave(p05b,file=flnm05.png,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
  ggsave(p05b,file=flnm05.pdf,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}

  #