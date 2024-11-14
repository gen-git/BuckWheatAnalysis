# Program for analyzing the past data

rm(list=ls())
#--- Reading libraries ---
library(sf)
library(mgcv)

#--- Loading the setting and the functions ---
source("00setting.R")
source("functions.R")

#--- Read the shaped data ---
load(file="./Save/MtDayLen.RData")
load(file="./Save/MtTmp.RData")
load(file="./Save/MtPre.RData")
load(file="./Save/MtGSR.RData")
load(file="./Save/DfBase.RData")
load(file="./Save/Params1.RData")
load(file="./Save/Params2.RData")

#--- Preparation ---
NBase = length(DfBase[,1])

#--- Reading crop calendar data ---
DfCalendar = read.csv(file=path_calendar, fileEncoding="SJIS")

#----------------------------------
#--- Summarize the climate data ---
#----------------------------------
# SF: from sowing to flowering
# FH: from flowering to harvesting
# Preparation of the data frame
DfAna = data.frame(DfBase,
                   TmpMeanSF=rep(NA,NBase),
                   PreMeanSF=rep(NA,NBase),
                   TmpMeanFH=rep(NA,NBase),
                   PreMeanFH=rep(NA,NBase),
                   GSRInteSF=rep(NA,NBase),
                   GSRInteFH=rep(NA,NBase),
                   GSRInteAll=rep(NA,NBase))

#- Calculating phenology -
params1      = apply(Params1, 2, mean)
params2      = apply(Params2, 2, mean)
Sowing       = rep(NA, NBase)
Flowering    = rep(NA, NBase)
Harvest      = rep(NA, NBase)
SowingFix    = rep(NA, NBase)
FloweringFix = rep(NA, NBase)
HarvestFix   = rep(NA, NBase)
LenFlowering = rep(NA, NBase)
LenHarvest   = rep(NA, NBase)
LenFH        = rep(NA, NBase)
for(i in 1:NBase){
  t_pref = DfBase$PrefCode[i]
  t_muni = DfBase$MuniCode[i]
  t_year = DfBase$Year[i]
  r_cal1 = which(DfCalendar$Pcode==t_pref)
  if(length(r_cal1)>0 & !is.na(MtDayLen[i,1])){
    if(!is.na(DfCalendar$Sowing_DOY[r_cal1])){
      Sowing[i] = DfCalendar$Sowing_DOY[r_cal1]
      
      Flowering[i] = Sowing[i]    + f3(MtTmp[i,Sowing[i]:365],MtDayLen[i,Sowing[i]:365],params1)
      Harvest[i]   = Flowering[i] + f3(MtTmp[i,Flowering[i]:365],MtDayLen[i,Flowering[i]:365],params2)
      
      LenFlowering[i] = Flowering[i] - Sowing[i]
      LenHarvest[i]   = Harvest[i]   - Sowing[i]
      LenFH[i]        = Harvest[i]   - Flowering[i]
      
    }
  }
}
DfBase$Sowing       = Sowing
DfBase$Flowering    = Flowering
DfBase$Harvest      = Harvest
DfBase$LenFlowering = LenFlowering
DfBase$LenHarvest   = LenHarvest
DfBase$LenFH        = LenFH

# Summarizing the climate data and inserting the variables to the data frame
for(i in 1:NBase){
  jS = DfBase$Sowing[i]
  jF = DfBase$Flowering[i]
  jH = DfBase$Harvest[i]
  
  DfAna$TmpMeanSF[i]  = mean(MtTmp[i,jS:jF])
  DfAna$TmpMeanFH[i]  = mean(MtTmp[i,jF:jH])
 
  DfAna$PreMeanSF[i]  = mean(MtPre[i,jS:jF])
  DfAna$PreMeanFH[i]  = mean(MtPre[i,jF:jH])

  DfAna$GSRInteSF[i]  = sum (MtGSR[i,jS:jF])
  DfAna$GSRInteFH[i]  = sum (MtGSR[i,jF:jH])
  DfAna$GSRInteAll[i] = sum (MtGSR[i,jS:jH])
}

#------------------------------------------------
#--- Preparation of the explanatory variables ---
#------------------------------------------------
ListTmpSF = c("s(TmpMeanSF)", "No")
ListTmpFH = c("s(TmpMeanFH)", "No")
ListPreSF = c("s(PreMeanSF)", "No")
ListPreFH = c("s(PreMeanFH)", "No")
ListGSRSF = c("s(GSRInteSF)", "No")
ListGSRFH = c("s(GSRInteFH)", "No")
ListArea  = c("s(Area)",      "No")

DfListClim = expand.grid(
                         ListTmpSF,
                         ListTmpFH,
                         ListPreSF,
                         ListPreFH,
                         ListArea,
                         ListGSRSF,
                         ListGSRFH,
                         stringsAsFactors = FALSE)

NModel = length(DfListClim[,1])

#----------------------------------------
#--- Calculating each model using GAM ---
#----------------------------------------
Rsts = list(NULL)
AICs = rep(NModel)
for(i in 1:NModel){
  mdl = paste0("Yield ~ s(Year) + ",
               paste0(DfListClim[i,], collapse=" + "))
  mdl = gsub("\\+ No", "", mdl)
  nfac = length(DfListClim[i,]) - length(which(DfListClim[i,]=="No"))
  fml = formula(mdl)
  rst = gam(fml, data=DfAna, sp = rep(3, nfac+1))
  Rsts[[i]] = rst
  AICs[i] = AIC(rst)
  cat("Model",i,"/",NModel,"\n")
}

#-----------------------
#--- Saving the data ---
#-----------------------
save(Rsts,       file="./Save/Rsts.RData")
save(AICs,       file="./Save/AICs.RData")
save(DfListClim, file="./Save/DfListClim.RData")
save(DfAna,      file="./Save/DfAna.RData")
