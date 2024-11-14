# Program for reading the past data
# This program culates the climate data, and makes RData file in Save folder.

rm(list=ls())
#--- Reading libraries ---
library(sf)
library(dplyr)

#--- Loading setting and functions ---
source("00setting.R")
source("functions.R")

#------------------------------------------
#--- Reading crop data and climate data ---
#------------------------------------------
#- Reading crop data -
DfCalendar = read.csv(file=path_calendar, fileEncoding="SJIS")
DfYield    = read.csv(file=path_yield, fileEncoding="SJIS")
DfYield    = subset(DfYield, Year%in%yr_gam)

#- Reading average temperature -
DfTmp = NULL
for(iyr in 1:length(yr_gam)){
  tpath = paste0(dir_past_clim,"/TMP_mea/",yr_gam[iyr],"_TMP_mea.csv")
  dat   = read.csv(tpath, fileEncoding="SJIS")
  if(length(dat[1,])==370){
    dat$cur366 = rep(NA, length(dat[,1]))
  }
  DfTmp = rbind(DfTmp, dat)
  cat("Year",yr_gam[iyr],"for DfTmp was read","\n")
}

#- Reading precipitation -
DfPre = NULL
for(iyr in 1:length(yr_gam)){
  tpath = paste0(dir_past_clim,"/APCP/",yr_gam[iyr],"_APCP.csv")
  dat   = read.csv(tpath, fileEncoding="SJIS")
  if(length(dat[1,])==370){
    dat$cur366 = rep(NA, length(dat[,1]))
  }
  DfPre = rbind(DfPre, dat)
  cat("Year",yr_gam[iyr],"for DfPre was read","\n")
}

#- Reading solar radiation -
DfGSR = NULL
for(iyr in 1:length(yr_gam)){
  tpath = paste0(dir_past_clim,"/GSR/",yr_gam[iyr],"_GSR.csv")
  dat   = read.csv(tpath, fileEncoding="SJIS")
  if(length(dat[1,])==370){
    dat$cur366 = rep(NA, length(dat[,1]))
  }
  DfGSR = rbind(DfGSR, dat)
  cat("Year",yr_gam[iyr],"for DfGSR was read","\n")
}

#--------------------------
#--- Reading shape file ---
#--------------------------
#- Reading the shape file -
# Shape file of 1993
JShape = sf::st_read("./shp_files/93_japan.shp")
n    = length(JShape$geometry)
lati = rep(NA, n)
for(i in 1:n){
  geo = JShape$geometry[[i]]
  lati[i] = sf::st_centroid(geo)[2]
}
DfJShape = dplyr::select(as.data.frame(JShape), - geometry)
DfJShape = DfJShape[,c(1,4,7)]
names(DfJShape) = c("Pref", "Muni", "MuniCode")
DfJShape$Lati = lati

# Shape files after 2000
for(i in shape_years){
  JShape = sf::st_read(paste0("./shp_files/",i,"_japan.shp"))
  n = length(JShape$geometry)
  lati = rep(NA, n)
  for(i in 1:n){
    geo = JShape$geometry[[i]]
    lati[i] = sf::st_centroid(geo)[2]
  }
  dat = dplyr::select(as.data.frame(JShape), - geometry)
  dat = dat[,c(1,4,5)]
  names(dat) = c("Pref", "Muni", "MuniCode")
  dat$Lati = lati
  DfJShape = rbind(DfJShape, dat)
}

#--------------------------------------
#--- Making data frame for analysis ---
#--------------------------------------
#- The information of municipalities -
DfBase = DfTmp[, 1:5]
NBase = length(DfBase[, 1])
names(DfBase) = c("PrefCode", "Pref", "Muni", "MuniCode", "Year")

#- Mean temperature -
MtTmp = as.matrix(DfTmp[, -c(1:5)])

#- Calculating day length for each municipality -
MtDayLen = array(NA, dim(MtTmp))
Lati     = rep(NA, NBase)
for(i in 1:NBase){
  t_muni   = sprintf("%05d",DfBase$MuniCode[i])
  r_jshape = which(DfJShape$MuniCode == t_muni)
  if(length(r_jshape) > 0){
    t_lati  = DfJShape$Lati[r_jshape[1]]
    t_rlati = t_lati * pi/180
    MtDayLen[i, 1:366] = f_tau_day(t_rlati, f_phi_soldec(1:366))*24
    Lati[i] = t_lati
    }
}

#- Make DfBase -
Sowing    = rep(NA, NBase)
Yield     = rep(NA, NBase)
Area      = rep(NA, NBase)
for(i in 1:NBase){
  t_pref = DfBase$PrefCode[i]
  t_muni = DfBase$MuniCode[i]
  t_year = DfBase$Year[i]
  
  r_cal1 = which(DfCalendar$Pcode == t_pref)
  r_cal2 = which(DfYield$Municipalities == t_muni & DfYield$Year==t_year)
  # When only the municipality has crop yield data and crop calendar data
  if(length(r_cal1) > 0 & length(r_cal2) > 0){
    Sowing[i]    = DfCalendar$Sowing_DOY[r_cal1]
    Yield[i]     = DfYield$kg[r_cal2[1]]
    Area[i]      = DfYield$ha[r_cal2[1]]
  }
}
DfBase$Sowing    = Sowing
DfBase$Yield     = Yield
DfBase$Area      = log(Area+1) # Note that area is log transformed
DfBase$Lati      = Lati

#- Sorting the climate data -
MtPre = array(NA, dim(MtTmp))
MtGSR = array(NA, dim(MtTmp))
PrePre = as.matrix(DfPre [,-c(1:5)])
PreGSR = as.matrix(DfGSR [,-c(1:5)])
for(i in 1:NBase){
  t_muni = as.numeric(DfBase$MuniCode[i])
  r_col1 = which(DfPre$Municipalities == t_muni)[1]
  r_col2 = which(DfGSR$Municipalities == t_muni)[1]
  MtPre[i,] = as.numeric(PrePre [r_col1, ])
  MtGSR[i,] = as.numeric(PreGSR [r_col2, ])
}

#- Deleting the rows that do not have data -
r_na = which(is.na(DfBase$Sowing) | is.na(MtDayLen[,1]))
MtDayLen = MtDayLen[-r_na, ]
MtTmp    = MtTmp   [-r_na, ]
DfBase   = DfBase  [-r_na, ]
MtPre    = MtPre   [-r_na, ]
MtGSR    = MtGSR   [-r_na, ]

#-----------------------
#--- Saving the data ---
#-----------------------
dir.create("Save", show=FALSE)
save(MtDayLen, file="./Save/MtDayLen.RData")
save(MtTmp,    file="./Save/MtTmp.RData")
save(MtPre,    file="./Save/MtPre.RData")
save(MtGSR,    file="./Save/MtGSR.RData")
save(DfBase,   file="./Save/DfBase.RData")
