#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
#remove everything in the working environment, without a warning!!
rm(list=ls())

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

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}
library(leaflet)


#head(df02,3)
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

#define vector with S Norway species
NoSsp <- c("E. lucius","S. trutta")
# subset data frame by matching the elements in the vector defined above
df04 <- subset(df03, subset = gen_specnm2 %in% NoSsp)


# create leaflet map
SNorway_map <- leaflet(df04) %>%
  addTiles() %>%
  addCircles(lng = ~dlon,
             lat = ~dlat,
             weight = 1,
             radius = ~sqrt(SQmean) * 30,
                   color = "red")


SNorway_map
#https://stackoverflow.com/questions/31336898/how-to-save-leaflet-in-r-map-as-png-or-jpg-file
if(!require(mapview)){
  install.packages("mapview")
  webshot::install_phantomjs()
  # install_phantomjs(version = "2.1.1",
  #                   baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/",
  #                   force = FALSE)
    library(mapview)
}
library(mapview)

getwd()
pthf06 <- paste(wd00,"/mapplot_v06_leaflet.png",sep="")
mapshot(SNorway_map, file = pthf06)

getwd()
pthf07 <- paste(wd00,"/input_eDNA_Elucius_from_R05.csv",sep="")
write_csv(df04,pthf07)
