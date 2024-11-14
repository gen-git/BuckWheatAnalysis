rm(list=ls())
#--- Read packages ---
library(sf)
library(dplyr)
library(latex2exp)

#--- Load ---
source("00setting.R")
source("functions.R")
load("Save/Params1.RData")
load("Save/Params2.RData")
load("Save/BestModel.RData")
load("Save/DfAna.RData")

#--- Preparation of directory ---
dir.create("Prediction", show=FALSE)

#--- Read data ---
for(issp in 1:length(ssp_list)){
  for(igcm in 1:length(gcm_list)){
      
    #- Read data of base year and calculate average -
    years = base_years 
    for(iyr in 1:length(years)){
      #--- Read ---
      ipath = paste0("Prediction/prediction_",years[iyr],"_",gcm_list[igcm],"_",ssp_list[issp],".csv")
      dat = read.csv(ipath,fileEncoding="SJIS")
      if(iyr != 1){
        DfAnaB = rbind(DfAnaB, dat)
      }
      if(iyr == 1){
        DfAnaB = dat
      }
    } # iyr
    
    MeanDfAnaB = DfAnaB %>% group_by(MuniCode) %>% summarise(BaseYield     = mean(Yield, na.rm=TRUE),
                                                             BaseTmpMeanSF = mean(TmpMeanSF, na.rm=TRUE),
                                                             BaseTmpMeanFH = mean(TmpMeanFH, na.rm=TRUE),
                                                             BasePreMeanSF = mean(PreMeanSF, na.rm=TRUE),
                                                             BasePreMeanFH = mean(PreMeanFH, na.rm=TRUE),
                                                             BaseGSRInteSF = mean(GSRInteSF, na.rm=TRUE),
                                                             BaseGSRInteFH = mean(GSRInteFH, na.rm=TRUE),
                                                             BaseLenSF     = mean(LenFlowering, na.rm=TRUE),
                                                             BaseLenFH     = mean(LenFH, na.rm=TRUE))
                                                             
    
    
    #- Read data of future years and calculate averages -
    years = ana_years
    for(iyr in 1:length(years)){
      #--- Read ---
      ipath = paste0("Prediction/prediction_",years[iyr],"_",gcm_list[igcm],"_",ssp_list[issp],".csv")
      dat = read.csv(ipath,fileEncoding="SJIS")
      if(iyr != 1){
        DfAnaF = rbind(DfAnaF, dat)
      }
      if(iyr == 1){
        DfAnaF = dat
      }
    } # iyr
    
    MeanDfAnaF = DfAnaF %>% group_by(MuniCode) %>%  summarise(Yield         = mean(Yield, na.rm=TRUE),
                                                              YieldFixGSRSF = mean(YieldFixGSRSF, na.rm=TRUE),
                                                              YieldFixTmpSF = mean(YieldFixTmpSF, na.rm=TRUE),
                                                              YieldFixPreSF = mean(YieldFixPreSF, na.rm=TRUE),
                                                              YieldFixGSRFH = mean(YieldFixGSRFH, na.rm=TRUE),
                                                              YieldFixTmpFH = mean(YieldFixTmpFH, na.rm=TRUE),
                                                              YieldFixPreFH = mean(YieldFixPreFH, na.rm=TRUE),
                                                              TmpMeanSF     = mean(TmpMeanSF, na.rm=TRUE),
                                                              TmpMeanFH     = mean(TmpMeanFH, na.rm=TRUE),
                                                              PreMeanSF     = mean(PreMeanSF, na.rm=TRUE),
                                                              PreMeanFH     = mean(PreMeanFH, na.rm=TRUE),
                                                              GSRInteSF     = mean(GSRInteSF, na.rm=TRUE),
                                                              GSRInteFH     = mean(GSRInteFH, na.rm=TRUE),
                                                              LenSF         = mean(LenFlowering, na.rm=TRUE),
                                                              LenFH         = mean(LenFH, na.rm=TRUE),
                                                              TmpMeanFixSF  = mean(TmpMeanFixSF, na.rm=TRUE),
                                                              TmpMeanFixFH  = mean(TmpMeanFixFH, na.rm=TRUE),
                                                              PreMeanFixSF  = mean(PreMeanFixSF, na.rm=TRUE),
                                                              PreMeanFixFH  = mean(PreMeanFixFH, na.rm=TRUE),
                                                              GSRInteFixSF  = mean(GSRInteFixSF, na.rm=TRUE),
                                                              GSRInteFixFH  = mean(GSRInteFixFH, na.rm=TRUE))
    
    MeanDfAna = inner_join(MeanDfAnaB, MeanDfAnaF, by="MuniCode")
    MeanDfAna = na.omit(MeanDfAna)
    MeanDfAna$ChangeYield         = (MeanDfAna$Yield / MeanDfAna$BaseYield              - 1) * 100
    MeanDfAna$ChangeYieldFixGSRSF = (MeanDfAna$YieldFixGSRSF / MeanDfAna$BaseYield - 1) * 100
    MeanDfAna$ChangeYieldFixTmpSF = (MeanDfAna$YieldFixTmpSF / MeanDfAna$BaseYield - 1) * 100
    MeanDfAna$ChangeYieldFixPreSF = (MeanDfAna$YieldFixPreSF / MeanDfAna$BaseYield - 1) * 100
    MeanDfAna$ChangeYieldFixGSRFH = (MeanDfAna$YieldFixGSRFH / MeanDfAna$BaseYield - 1) * 100
    MeanDfAna$ChangeYieldFixTmpFH = (MeanDfAna$YieldFixTmpFH / MeanDfAna$BaseYield - 1) * 100
    MeanDfAna$ChangeYieldFixPreFH = (MeanDfAna$YieldFixPreFH / MeanDfAna$BaseYield - 1) * 100
    MeanDfAna$BaseTmpMeanSF       = MeanDfAna$BaseTmpMeanSF
    MeanDfAna$BaseTmpMeanFH       = MeanDfAna$BaseTmpMeanFH
    MeanDfAna$BasePreMeanSF       = MeanDfAna$BasePreMeanSF
    MeanDfAna$BasePreMeanFH       = MeanDfAna$BasePreMeanFH
    MeanDfAna$BaseGSRInteSF       = MeanDfAna$BaseGSRInteSF
    MeanDfAna$BaseGSRInteFH       = MeanDfAna$BaseGSRInteFH
    MeanDfAna$ChangeTmpMeanSF     = (MeanDfAna$TmpMeanSF / MeanDfAna$BaseTmpMeanSF - 1) * 100
    MeanDfAna$ChangeTmpMeanFH     = (MeanDfAna$TmpMeanFH / MeanDfAna$BaseTmpMeanFH - 1) * 100
    MeanDfAna$ChangePreMeanSF     = (MeanDfAna$PreMeanSF / MeanDfAna$BasePreMeanSF - 1) * 100
    MeanDfAna$ChangePreMeanFH     = (MeanDfAna$PreMeanFH / MeanDfAna$BasePreMeanFH - 1) * 100
    MeanDfAna$ChangeGSRInteSF     = (MeanDfAna$GSRInteSF / MeanDfAna$BaseGSRInteSF - 1) * 100
    MeanDfAna$ChangeGSRInteFH     = (MeanDfAna$GSRInteFH / MeanDfAna$BaseGSRInteFH - 1) * 100
    MeanDfAna$ChangeTmpMeanFixSF  = (MeanDfAna$TmpMeanFixSF / MeanDfAna$BaseTmpMeanSF - 1) * 100
    MeanDfAna$ChangeTmpMeanFixFH  = (MeanDfAna$TmpMeanFixFH / MeanDfAna$BaseTmpMeanFH - 1) * 100
    MeanDfAna$ChangePreMeanFixSF  = (MeanDfAna$PreMeanFixSF / MeanDfAna$BasePreMeanSF - 1) * 100
    MeanDfAna$ChangePreMeanFixFH  = (MeanDfAna$PreMeanFixFH / MeanDfAna$BasePreMeanFH - 1) * 100
    MeanDfAna$ChangeGSRInteFixSF  = (MeanDfAna$GSRInteFixSF / MeanDfAna$BaseGSRInteSF - 1) * 100
    MeanDfAna$ChangeGSRInteFixFH  = (MeanDfAna$GSRInteFixFH / MeanDfAna$BaseGSRInteFH - 1) * 100
    MeanDfAna$DiffLenSF           =  MeanDfAna$LenSF - MeanDfAna$BaseLenSF
    MeanDfAna$DiffLenFH           =  MeanDfAna$LenFH - MeanDfAna$BaseLenFH
    
    # Output for each GCM
    opath = paste0("Prediction/period_mean_",gcm_list[igcm],"_",ssp_list[issp],".csv")
    write.csv(MeanDfAna, file=opath, row.names=FALSE)
    
    # Connect the results of all GCMs
    if(igcm == 1){
      MeanDfAnaAll = MeanDfAna
    }
    if(igcm != 1){
      MeanDfAnaAll = rbind(MeanDfAnaAll, MeanDfAna)
    }
    cat("GCM:",gcm_list[igcm],", SSP:",ssp_list[issp],"was finished", "\n")

  } # igcm
  MeanMeanDfAnaAll = MeanDfAnaAll %>% group_by(MuniCode) %>% summarise(
                     ChangeYield             = mean(ChangeYield, na.rm=TRUE),
                     ChangeYieldFixGSRFH     = mean(ChangeYieldFixGSRFH, na.rm=TRUE),
                     ChangeYieldFixTmpFH     = mean(ChangeYieldFixTmpFH, na.rm=TRUE),
                     ChangeYieldFixPreFH     = mean(ChangeYieldFixPreFH, na.rm=TRUE),
                     BaseTmpMeanSF           = mean(BaseTmpMeanSF, na.rm=TRUE),
                     BaseTmpMeanFH           = mean(BaseTmpMeanFH, na.rm=TRUE),
                     BasePreMeanSF           = mean(BasePreMeanSF, na.rm=TRUE),
                     BasePreMeanFH           = mean(BasePreMeanFH, na.rm=TRUE),
                     BaseGSRInteSF           = mean(BaseGSRInteSF, na.rm=TRUE),
                     BaseGSRInteFH           = mean(BaseGSRInteFH, na.rm=TRUE),
                     ChangeTmpMeanSF         = mean(ChangeTmpMeanSF, na.rm=TRUE),
                     ChangeTmpMeanFH         = mean(ChangeTmpMeanFH, na.rm=TRUE),
                     ChangePreMeanSF         = mean(ChangePreMeanSF, na.rm=TRUE),
                     ChangePreMeanFH         = mean(ChangePreMeanFH, na.rm=TRUE),
                     ChangeGSRInteSF         = mean(ChangeGSRInteSF, na.rm=TRUE),
                     ChangeGSRInteFH         = mean(ChangeGSRInteFH, na.rm=TRUE),
                     DiffLenSF               = mean(DiffLenSF, na.rm=TRUE),
                     DiffLenFH               = mean(DiffLenFH, na.rm=TRUE),
                     ChangeTmpMeanFixSF      = mean(ChangeTmpMeanFixSF, na.rm=TRUE),
                     ChangeTmpMeanFixFH      = mean(ChangeTmpMeanFixFH, na.rm=TRUE),
                     ChangePreMeanFixSF      = mean(ChangePreMeanFixSF, na.rm=TRUE),
                     ChangePreMeanFixFH      = mean(ChangePreMeanFixFH, na.rm=TRUE),
                     ChangeGSRInteFixSF      = mean(ChangeGSRInteFixSF, na.rm=TRUE),
                     ChangeGSRInteFixFH      = mean(ChangeGSRInteFixFH, na.rm=TRUE),
                     DiffChangeGSRInteFixSF  = mean(ChangeGSRInteSF, na.rm=TRUE) - mean(ChangeGSRInteFixSF, na.rm=TRUE),
                     DiffChangeGSRInteFixFH  = mean(ChangeGSRInteFH, na.rm=TRUE) - mean(ChangeGSRInteFixFH, na.rm=TRUE),
                     DiffChangeTmpMeanFixSF  = mean(ChangeTmpMeanSF, na.rm=TRUE) - mean(ChangeTmpMeanFixSF, na.rm=TRUE),
                     DiffChangeTmpMeanFixFH  = mean(ChangeTmpMeanFH, na.rm=TRUE) - mean(ChangeTmpMeanFixFH, na.rm=TRUE),
                     DiffChangePreMeanFixSF  = mean(ChangePreMeanSF, na.rm=TRUE) - mean(ChangePreMeanFixSF, na.rm=TRUE),
                     DiffChangePreMeanFixFH  = mean(ChangePreMeanFH, na.rm=TRUE) - mean(ChangePreMeanFixFH, na.rm=TRUE),
                     DiffChangeYieldFixGSRSF = mean(ChangeYield, na.rm=TRUE) - mean(ChangeYieldFixGSRSF, na.rm=TRUE),
                     DiffChangeYieldFixTmpSF = mean(ChangeYield, na.rm=TRUE) - mean(ChangeYieldFixTmpSF, na.rm=TRUE),
                     DiffChangeYieldFixPreSF = mean(ChangeYield, na.rm=TRUE) - mean(ChangeYieldFixPreSF, na.rm=TRUE),
                     DiffChangeYieldFixGSRFH = mean(ChangeYield, na.rm=TRUE) - mean(ChangeYieldFixGSRFH, na.rm=TRUE),
                     DiffChangeYieldFixTmpFH = mean(ChangeYield, na.rm=TRUE) - mean(ChangeYieldFixTmpFH, na.rm=TRUE),
                     DiffChangeYieldFixPreFH = mean(ChangeYield, na.rm=TRUE) - mean(ChangeYieldFixPreFH, na.rm=TRUE))

  # Output of the average of GCM
  opath = paste0("Prediction/all_gcm_mean_",ssp_list[issp],".csv")
  write.csv(MeanMeanDfAnaAll, file=opath, row.names=FALSE)
  
  # Output of the statistics of the result of all GCM
  sink("Summary/all_gcm_mean_stat.txt")
  cat("Factor         : mean, S.D.", "\n")
  cat("ChangeYield    : ", mean(MeanMeanDfAnaAll$ChangeYield    ), ",", sd(MeanMeanDfAnaAll$ChangeYield    ), "\n")
  cat("ChangeTmpMeanSF: ", mean(MeanMeanDfAnaAll$ChangeTmpMeanSF), ",", sd(MeanMeanDfAnaAll$ChangeTmpMeanSF), "\n")
  cat("ChangeTmpMeanFH: ", mean(MeanMeanDfAnaAll$ChangeTmpMeanFH), ",", sd(MeanMeanDfAnaAll$ChangeTmpMeanFH), "\n")
  cat("ChangePreMeanSF: ", mean(MeanMeanDfAnaAll$ChangePreMeanSF), ",", sd(MeanMeanDfAnaAll$ChangePreMeanSF), "\n")
  cat("ChangePreMeanFH: ", mean(MeanMeanDfAnaAll$ChangePreMeanFH), ",", sd(MeanMeanDfAnaAll$ChangePreMeanFH), "\n")
  cat("ChangeGSRInteSF: ", mean(MeanMeanDfAnaAll$ChangeGSRInteSF), ",", sd(MeanMeanDfAnaAll$ChangeGSRInteSF), "\n")
  cat("ChangeGSRInteFH: ", mean(MeanMeanDfAnaAll$ChangeGSRInteFH), ",", sd(MeanMeanDfAnaAll$ChangeGSRInteFH), "\n")
  cat("DiffLenSF      : ", mean(MeanMeanDfAnaAll$DiffLenSF      ), ",", sd(MeanMeanDfAnaAll$DiffLenSF      ), "\n")
  cat("DiffLenFH      : ", mean(MeanMeanDfAnaAll$DiffLenFH      ), ",", sd(MeanMeanDfAnaAll$DiffLenFH      ), "\n")
  cat("\n")
  cat("ChangeTmpMeanFixSF: ", mean(MeanMeanDfAnaAll$ChangeTmpMeanFixSF), ",", sd(MeanMeanDfAnaAll$ChangeTmpMeanFixSF), "\n")
  cat("ChangeTmpMeanFixFH: ", mean(MeanMeanDfAnaAll$ChangeTmpMeanFixFH), ",", sd(MeanMeanDfAnaAll$ChangeTmpMeanFixFH), "\n")
  cat("ChangePreMeanFixSF: ", mean(MeanMeanDfAnaAll$ChangePreMeanFixSF), ",", sd(MeanMeanDfAnaAll$ChangePreMeanFixSF), "\n")
  cat("ChangePreMeanFixFH: ", mean(MeanMeanDfAnaAll$ChangePreMeanFixFH), ",", sd(MeanMeanDfAnaAll$ChangePreMeanFixFH), "\n")
  cat("ChangeGSRInteFixSF: ", mean(MeanMeanDfAnaAll$ChangeGSRInteFixSF), ",", sd(MeanMeanDfAnaAll$ChangeGSRInteFixSF), "\n")
  cat("ChangeGSRInteFixFH: ", mean(MeanMeanDfAnaAll$ChangeGSRInteFixFH), ",", sd(MeanMeanDfAnaAll$ChangeGSRInteFixFH), "\n")
  cat("\n")
  cat("ChangeYieldFixGSRFH: ", mean(MeanMeanDfAnaAll$ChangeYieldFixGSRFH), ",", sd(MeanMeanDfAnaAll$ChangeYieldFixGSRFH), "\n")
  cat("ChangeYieldFixTmpFH: ", mean(MeanMeanDfAnaAll$ChangeYieldFixTmpFH), ",", sd(MeanMeanDfAnaAll$ChangeYieldFixTmpFH), "\n")
  cat("ChangeYieldFixPreFH: ", mean(MeanMeanDfAnaAll$ChangeYieldFixPreFH), ",", sd(MeanMeanDfAnaAll$ChangeYieldFixPreFH), "\n")

  sink()
} # issp

