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
inf1 <- inf1[grepl("virik",inf1)]
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
  # add it to the list
  lsf2[[i]] <- dfs01 
  # add to the increasing count
  i <- i+1
  #end iteration over files
}
#split up the list to make a data frame
df02 <- do.call(rbind, lsf2)
#sustitute to get species name
df02$gen_specnm <- gsub("^eDNAconc_([A-Za-z]+)_virik\\.txt","\\1",df02$flnm)
#getwd()
####################################################################################
#
# prepare std dilution curve plots for each for species
#
####################################################################################
#get the unique species names
#latspecnm <- unique(df02$gen_specnm)
######################################################################################
#head(df02,3)
df02$sample_rep2 <- as.character(df02$sample_rep)
plt01 <- ggplot2::ggplot(df02, aes(x = station,
                           y = SQ_vol,
                            group= sample_rep,
                            #color = sample_rep)) +
                            shape=sample_rep2,
                            color = gen_specnm)) +

  geom_point()

plt01

#plot(df02$station,df02$SQ_vol, pch=3)
#define list of flies
lfx <- list.files(".")[grepl("xls",list.files("."))]
lfxp <- lfx[grepl("lokaliteter",lfx)]
#read in excel sheet as tibble
tb_prlo <- readxl::read_xlsx(lfxp, sheet="samples 2021")
# make it a data frame
df_pro <- as.data.frame(tb_prlo)
# get unique station locations
st1 <- unique(df_pro$Stasjon)
# bind columns to a data frame
df_st2 <- as.data.frame(cbind(st1))
# add latitude and longitude
df_st2$dlat <- df_pro$Posisjon...34[match(df_st2$st1,df_pro$Stasjon)] 
df_st2$dlon <- df_pro$Posisjon...35[match(df_st2$st1,df_pro$Stasjon)] 
df_st2$Fylke <- df_pro$Fylke[match(df_st2$st1,df_pro$Stasjon)] 
#ensure the lat and lon are numeric
df_st2$dlat <- as.numeric(as.character(df_st2$dlat))
df_st2$dlon <- as.numeric(as.character(df_st2$dlon))
# match the lat and long to df with qCR data
df02$dlat <-  df_st2$dlat[match(df02$station,df_st2$st1)]
df02$dlon <-  df_st2$dlon[match(df02$station,df_st2$st1)]



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
# split column with 'prove nummer'
dfprNo01 <- data.frame(do.call('rbind', strsplit(as.character(df_pro$Prøvenr),', ',fixed=TRUE)))
#bind together columns with filtered volume
dfPV1 <- cbind(dfprNo01[1],df_pro$`Volum 1`)
dfPV2 <- cbind(dfprNo01[2],df_pro$`Volum 2`)
dfPV3 <- cbind(dfprNo01[3],df_pro$`Volum 3`)
#change column names
colnames(dfPV1) <- c("ProvNo","VolFilt_mL")
colnames(dfPV2) <- c("ProvNo","VolFilt_mL")
colnames(dfPV3) <- c("ProvNo","VolFilt_mL")
#append as more rows and make it a data frame
dfprNo02 <- as.data.frame(rbind(dfPV1,dfPV2,dfPV3))
#ensure columns are numeric values
dfprNo02$ProvNo <- as.numeric(dfprNo02$ProvNo)
dfprNo02$VolFilt_mL <- as.numeric(dfprNo02$VolFilt_mL)
#match filtered volume to main data frame
df02$VolFilt_mL <- dfprNo02$VolFilt_mL[match(df02$Sample,dfprNo02$ProvNo)]
df02$VolFilt_mL[is.na(df02$VolFilt_mL)] <- 0
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
  facet_wrap(~gen_specnm2, nrow = 2, ncol=1) + #'facet_wrap' subsets by column value in dataframe
  #define colour gradient for fill
  scale_fill_gradient2(low = "white", mid = "white", high = "blue",
                       guide = guide_colorbar(order = 1)) +
  
  # scale_fill_manual(values=alpha(
  #   c(cl),
  #   c(0.7)
  # ))+
  # #transformed
  #define limits of map
  ggplot2::coord_sf(xlim = c(9.5, 10.5),
                    ylim = c(59, 59.3), 
                    expand = FALSE)

p01 <- p01 + xlab("longitude") + ylab("latitude")
# see the plot
p01


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
write.table(df03,"mean_eDNA_conc_virik.csv", sep=",",
            dec=".")
#_______________________________________________________________________________
# also see : https://github.com/tidyverse/ggplot2/issues/2037
# also see : https://github.com/tidyverse/ggplot2/issues/2037

norw_map_sp <- ne_countries(country="norway",scale = 10, returnclass = "sp")

p02 <- 
  ggplot() +
  geom_polygon(data=norw_map_sp,aes(x = long, y = lat, group = group),color = "black", fill = "azure3") +
  geom_jitter(data = df03,
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
  scale_fill_gradientn(colors = alpha(c("white","red","black"),0.6),
                       guide = guide_colorbar(order = 1)) +
  #define limits of map
  ggplot2::coord_sf(xlim = c(10.15, 10.35) ,
                    ylim = c(59.065, 59.265),
                    #default_crs = sf::st_crs(4326),
                    expand = FALSE) +
  xlab("longitude") + ylab("latitude")

# see the plot
p02

<<<<<<< HEAD
=======


>>>>>>> 132b8d30042ec67da38dc3467d8f0a00ac615d96
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


p02b <- p02
# you will have to change the legend for all legends
p02b <- p02b + labs(color='species')
p02b <- p02b + labs(fill='mean copies/ mL filt water')
p02b <- p02b + labs(shape='eDNA presence')
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
# # on how to arrange plots in patchwork
# p <-  p01b +
#   #
#   
#   plot_layout(nrow=3,byrow=T) + #xlab(xlabel) +
#   plot_layout(guides = "collect") +
#   plot_annotation(caption=fnm04) #& theme(legend.position = "bottom")
#p
fnm04 <- "mapplot_eDNA_conc_from_Rcode02"

#make filename to save plot to
figname03 <- paste0(fnm04,".png")
figname04 <- paste0(fnm04,".pdf")


if(bSaveFigures==T){
  ggsave(p,file=figname03,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}

if(bSaveFigures==T){
  ggsave(p,file=figname04,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}

#