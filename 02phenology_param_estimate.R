# Program for estimating the values of parameters of the growth rate model

rm(list=ls())
#--- Reading libraries ---
library(sf)
library(mgcv)

#--- Loading the setting and the functions ---
source("00setting.R")
source("functions.R")

#--- Reading data ---
load(file="./Save/MtDayLen.RData")
load(file="./Save/MtTmp.RData")
load(file="./Save/MtPre.RData")
load(file="./Save/MtGSR.RData")
load(file="./Save/DfBase.RData")

#--- Extracting data of Year 1995 ---
tgt_r = which(DfBase$Year == yr_pheno)
DfBase   = DfBase  [tgt_r, ]
MtDayLen = MtDayLen[tgt_r, ]
MtTmp    = MtTmp   [tgt_r, ]

#--- Reading the crop calendar data ---
DfCalendar = read.csv(file=path_calendar, fileEncoding="SJIS")

#--- Extracting phenology data from the crop calendar ---
NBase = dim(DfBase)[1]
Sowing       = rep(NA, NBase)
Flowering    = rep(NA, NBase)
Harvest      = rep(NA, NBase)
LenSF        = rep(NA, NBase)
LenFH        = rep(NA, NBase)
for(i in 1:NBase){
  t_pref = DfBase$PrefCode[i]
  
  r_cal1 = which(DfCalendar$Pcode == t_pref)
  Sowing[i]       = DfCalendar$Sowing_DOY   [r_cal1]
  Flowering[i]    = DfCalendar$Flowering_DOY[r_cal1]
  Harvest[i]      = DfCalendar$Harvest_DOY  [r_cal1]
  LenSF[i]        = Flowering[i] - Sowing[i]
  LenFH[i]        = Harvest[i] - Flowering[i]
}
DfBase$Sowing    = Sowing
DfBase$Flowering = Flowering
DfBase$Harvest   = Harvest
DfBase$LenSF = LenSF
DfBase$LenFH = LenFH

#--- Minimization 1 ---
if(boot_n == 0){
  Params1 = matrix(NA, 1, 5)
  tgtMtDayLen = MtDayLen
  tgtMtTmp    = MtTmp
  tgtStart    = DfBase$Sowing
  tgtLenDays  = DfBase$LenSF
  rst = optim(par=c(0.01,0.01,15,0,10),
              fn=f2,
              MtDayLen = tgtMtDayLen,
              MtTmp    = tgtMtTmp,
              Start    = tgtStart,
              LenDays  = tgtLenDays)
  Params1[1,] = rst$par
  cat("Param1", "finished","\n")
}
if(boot_n > 0){
  N = length(MtDayLen[, 1])
  Params1 = matrix(NA, boot_n, 5)
  for(i in 1:boot_n){
    a = sample(1:N, N, rep=TRUE)
    tgtMtDayLen = MtDayLen[a,]
    tgtMtTmp    = MtTmp[a,]
    tgtStart    = DfBase$Sowing[a]
    tgtLenDays  = DfBase$LenSF[a]
    rst = optim(par=c(0.01,0.01,15,0,10),
                fn=f2,
                MtDayLen = tgtMtDayLen,
                MtTmp    = tgtMtTmp,
                Start    = tgtStart,
                LenDays  = tgtLenDays)
    Params1[i,] = rst$par
    cat(i, "finished","\n")
  }
}
save(Params1, file="./Save/Params1.RData")

#--- Minimization 2 ---
if(boot_n == 0){
  Params2 = matrix(NA, 1, 5)
  tgtMtDayLen = MtDayLen
  tgtMtTmp    = MtTmp
  tgtStart    = DfBase$Flowering
  tgtLenDays  = DfBase$LenFH
  rst = optim(par=c(0.01,0.01,15,0,10),
              fn=f2,
              MtDayLen = tgtMtDayLen,
              MtTmp    = tgtMtTmp,
              Start    = tgtStart,
              LenDays  = tgtLenDays)
  Params2[1,] = rst$par
  cat("Param2", "finished","\n")
}
if(boot_n > 0){
  N = length(MtDayLen[, 1])
  Params2 = matrix(NA, boot_n, 5)
  for(i in 1:boot_n){
    a = sample(1:N, N, rep=TRUE)
    tgtMtDayLen = MtDayLen[a,]
    tgtMtTmp    = MtTmp[a,]
    tgtStart    = DfBase$Flowering[a]
    tgtLenDays  = DfBase$LenFH[a]
    rst = optim(par=c(0.01,0.01,15,0,10),
                fn=f2,
                MtDayLen = tgtMtDayLen,
                MtTmp    = tgtMtTmp,
                Start    = tgtStart,
                LenDays  = tgtLenDays)
    Params2[i,] = rst$par
    cat(i, "finished","\n")
  }
}
save(Params2, file="./Save/Params2.RData")
