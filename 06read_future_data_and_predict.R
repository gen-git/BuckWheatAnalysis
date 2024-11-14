# Program for prediction

#--- Reading libraries ---
library(sf)
library(dplyr)
library(latex2exp)
library(mgcv)

#--- Loading the setting, the functions, and the data ---
source("00setting.R")
source("functions.R")
load("Save/Params1.RData")
load("Save/Params2.RData")
load("Save/BestModel.RData")
load("Save/DfAna.RData")

#--- Preparing the folder ---
dir.create("Prediction", show=FALSE)

#--- Reading the shape file ---
# Reading the file
JShape = st_read(path_j_shape_future)

#- Calculating the length -
NJShape = length(JShape$geometry)

#- Extracting the data frame from the shape file -
DfJShape = dplyr::select(as.data.frame(JShape), -geometry)
DfJShape = DfJShape[,c(1,4,5)]
names(DfJShape) = c("Pref","Muni","MuniCode")

#--- Calculating the center for each municipality ---
lati = rep(NA, NJShape)
for(i in 1:NJShape){
  geo = JShape$geometry[[i]]
  lati[i] = st_centroid(geo)[2]
}
DfJShape$Lati = lati

#--- Prediction ---
for(igcm in 1:length(gcm_list)){
  for(issp in 1:length(ssp_list)){
    DfBaseFix = NULL
    for(iyr in 1:length(yr_future)){
        
      #- Reading the crop calendar data -
      DfCalendar = read.csv(file=path_calendar, fileEncoding="SJIS")
      
      #- Reading the future temperature -
      tpath = paste0(dir_future_clim,"/tas/",yr_future[iyr],"_tas_",
                     gcm_list[igcm],"_",ssp_list[issp],".csv")
      DfTmp  = read.csv(tpath, fileEncoding="SJIS")
      
      #- Reading the future precipitation -
      tpath = paste0(dir_future_clim,"/pr/",yr_future[iyr],"_pr_",
                     gcm_list[igcm],"_",ssp_list[issp],".csv")
      DfPre  = read.csv(tpath, fileEncoding="SJIS")
      
      #- Reading the future solar radiation -
      tpath = paste0(dir_future_clim,"/rsds/",yr_future[iyr],"_rsds_",
                     gcm_list[igcm],"_",ssp_list[issp],".csv")
      DfGSR  = read.csv(tpath, fileEncoding="SJIS")
      
      #--- Making the data frame for the analysis ---
      #- Extract the basic information -
      DfBase = DfTmp[,1:3]
      NBase  = length(DfBase[,1])
      names(DfBase) = c("PrefCode","MuniCode","Year")
      
      #- Average temperature -
      MtTmp = as.matrix(DfTmp[,-c(1:3)])
      
      #- Ordering the matrix of the climate data -
      MtPre = array(NA, dim(MtTmp))
      MtGSR = array(NA, dim(MtTmp))
      PrePre = as.matrix(DfPre [,-c(1:3)])
      PreGSR = as.matrix(DfGSR [,-c(1:3)])
      for(i in 1:NBase){
        t_muni = as.numeric(DfBase$MuniCode[i])
        r_col1 = which(DfPre$Municipalities==t_muni)[1]
        r_col2 = which(DfGSR$Municipalities==t_muni)[1]
        MtPre[i,] = as.numeric(PrePre [r_col1, ])
        MtGSR[i,] = as.numeric(PreGSR [r_col2, ])
      }
      
      #- Calculating the day length -
      MtDayLen = array(NA, dim(MtTmp))
      Lati = rep(NA, NBase)
      for(i in 1:NBase){
        t_muni = sprintf("%05d",DfBase$MuniCode[i])
        r_jshape = which(DfJShape$MuniCode==t_muni)
        if(length(r_jshape)>0){
          t_lati = DfJShape$Lati[r_jshape]
          t_rlati = t_lati * pi/180
          MtDayLen[i,1:365] = f_tau_day(t_rlati,f_phi_soldec(1:365))*24
          Lati[i] = t_lati
        }
      }
      
      # Calculating the phenology
      params1      = apply(Params1, 2, mean)
      params2      = apply(Params2, 2, mean)
      Sowing       = rep(NA, NBase)
      Flowering    = rep(NA, NBase)
      Harvest      = rep(NA, NBase)
      FloweringFix = rep(NA, NBase)
      HarvestFix   = rep(NA, NBase)
      LenFlowering = rep(NA, NBase)
      LenHarvest   = rep(NA, NBase)
      LenFH        = rep(NA, NBase)
      if(!(yr_future[iyr] %in% base_years)){
        MeanDfBaseFix = DfBaseFix %>% group_by(MuniCode) %>%  
          summarise(Flowering = mean(Flowering, na.rm=TRUE), Harvest = mean(Harvest, na.rm=TRUE))
      }
      for(i in 1:NBase){
        t_pref = DfBase$PrefCode[i]
        t_muni = DfBase$MuniCode[i]
        r_cal1  = which(DfCalendar$Pcode==t_pref)
        if(length(r_cal1)>0 & !is.na(MtDayLen[i,1])){
          if(!is.na(DfCalendar$Sowing_DOY[r_cal1])){
            
            Sowing[i] = DfCalendar$Sowing_DOY[r_cal1]
            
            Flowering[i] = Sowing[i]    + f3(MtTmp[i,Sowing[i]:365],   
                                             MtDayLen[i,Sowing[i]:365],   params1)
            Harvest[i]   = Flowering[i] + f3(MtTmp[i,Flowering[i]:365],
                                             MtDayLen[i,Flowering[i]:365],params2)
            
            LenFlowering[i] = Flowering[i] - Sowing[i]
            LenHarvest[i]   = Harvest[i]   - Sowing[i]
            LenFH[i]        = Harvest[i]   - Flowering[i]
            
            if(!(yr_future[iyr] %in% base_years)){
              r_cal2 = which(MeanDfBaseFix$MuniCode == t_muni)[1]
              if(length(r_cal2) > 0){
                FloweringFix[i] = MeanDfBaseFix$Flowering[r_cal2]
                HarvestFix[i]   = MeanDfBaseFix$Harvest[r_cal2]
              }
            }
          }
        }
      }
      DfBase$Sowing       = Sowing
      DfBase$Flowering    = Flowering
      DfBase$Harvest      = Harvest
      DfBase$LenFlowering = LenFlowering
      DfBase$LenHarvest   = LenHarvest
      DfBase$LenFH        = LenFH
      DfBase$Lati         = Lati
      DfBase$FloweringFix = FloweringFix
      DfBase$HarvestFix   = HarvestFix
      if(yr_future[iyr] %in% base_years){
        DfBaseFix = rbind(DfBaseFix, DfBase)
      }
      
      #- Omit the rows including NA -
      r_na = which(is.na(DfBase$Sowing) | is.na(MtDayLen[,1]))
      MtDayLen = MtDayLen[-r_na,]
      MtTmp    = MtTmp   [-r_na,]
      DfBase   = DfBase  [-r_na,]
      MtPre    = MtPre   [-r_na,]
      MtGSR    = MtGSR   [-r_na,]

      #--- Preparation ---
      NBase = length(DfBase[,1])
      
      #--- Summarizing the climate data ---
      DfAnaF = data.frame(DfBase,
                          Area         = rep(mean(DfAna$Area, na.rm=TRUE), NBase),
                          TmpMeanSF    = rep(NA,NBase),
                          PreMeanSF    = rep(NA,NBase),
                          GSRInteSF    = rep(NA,NBase),
                          TmpMeanFH    = rep(NA,NBase),
                          PreMeanFH    = rep(NA,NBase),
                          GSRInteFH    = rep(NA,NBase),
                          GSRInteAll   = rep(NA,NBase),
                          
                          GSRInteFixSF = rep(NA,NBase),
                          TmpMeanFixSF = rep(NA,NBase),
                          PreMeanFixSF = rep(NA,NBase),
                          
                          GSRInteFixFH = rep(NA,NBase),
                          TmpMeanFixFH = rep(NA,NBase),
                          PreMeanFixFH = rep(NA,NBase)
       )

      for(i in 1:NBase){
        jS = DfBase$Sowing[i]
        jF = DfBase$Flowering[i]
        jH = DfBase$Harvest[i]
        DfAnaF$TmpMeanSF[i]    = mean(MtTmp[i,jS:jF])
        DfAnaF$TmpMeanFH[i]    = mean(MtTmp[i,jF:jH])
   
        DfAnaF$PreMeanSF[i]    = mean(MtPre[i,jS:jF])
        DfAnaF$PreMeanFH[i]    = mean(MtPre[i,jF:jH])
   
        DfAnaF$GSRInteSF[i]    = sum(MtGSR[i,jS:jF])
        DfAnaF$GSRInteFH[i]    = sum(MtGSR[i,jF:jH])

        DfAnaF$GSRInteAll[i]   = sum(MtGSR[i, jS:jH])
        
        if(!(yr_future[iyr] %in% base_years)){
          jFFix = DfBase$FloweringFix[i]
          jHFix = DfBase$HarvestFix[i]
          if(!is.nan(jFFix)){
            DfAnaF$PreMeanFixSF[i] = mean(MtPre[i, jS:jFFix])
            DfAnaF$TmpMeanFixSF[i] = mean(MtTmp[i, jS:jFFix])
            DfAnaF$GSRInteFixSF[i] = sum (MtGSR[i, jS:jFFix])
            
            DfAnaF$PreMeanFixFH[i] = mean(MtPre[i, jFFix:jHFix])
            DfAnaF$TmpMeanFixFH[i] = mean(MtTmp[i, jFFix:jHFix])
            DfAnaF$GSRInteFixFH[i] = sum (MtGSR[i, jFFix:jHFix])
          }
        }
      }
      
      #--- Prediction ---
      DfAnaF$Year = 2020
      Yield = predict(BestModel, newdata=DfAnaF)
      DfAnaF$Yield = Yield
      
      if(!(yr_future[iyr] %in% base_years)){
        
        #--- Prediction (Solar radiation BF using the phenology of 1995)  ---
        DfAnaFixGSRSF_F = DfAnaF[,c("TmpMeanSF",
                                    "TmpMeanFH",
                                    "PreMeanSF",
                                    "PreMeanFH",
                                    "GSRInteFixSF",
                                    "GSRInteFH",
                                    "Area",
                                    "Year")]
        names(DfAnaFixGSRSF_F) = gsub("Fix","",names(DfAnaFixGSRSF_F))
        YieldFixGSRSF = predict(BestModel, newdata=DfAnaFixGSRSF_F)
        DfAnaF$YieldFixGSRSF = YieldFixGSRSF
        
        #--- Prediction (Temperature BF using the phenology of 1995) ---
        DfAnaFixTmpSF_F = DfAnaF[,c("TmpMeanFixSF",
                                    "TmpMeanFH",
                                    "PreMeanSF",
                                    "PreMeanFH",
                                    "GSRInteSF",
                                    "GSRInteFH",
                                    "Area",
                                    "Year")]
        names(DfAnaFixTmpSF_F) = gsub("Fix","",names(DfAnaFixTmpSF_F))
        YieldFixTmpSF = predict(BestModel, newdata=DfAnaFixTmpSF_F)
        DfAnaF$YieldFixTmpSF = YieldFixTmpSF
        
        #--- Prediction (Precipitation BF using the phenology of 1995) ---
        DfAnaFixPreSF_F = DfAnaF[,c("TmpMeanSF",
                                    "TmpMeanFH",
                                    "PreMeanFixSF",
                                    "PreMeanFH",
                                    "GSRInteSF",
                                    "GSRInteFH",
                                    "Area",
                                    "Year")]
        names(DfAnaFixPreSF_F) = gsub("Fix","",names(DfAnaFixPreSF_F))
        YieldFixPreSF = predict(BestModel, newdata=DfAnaFixPreSF_F)
        DfAnaF$YieldFixPreSF = YieldFixPreSF

        #--- Prediction (Solar radiation AF using the phenology of 1995)  ---
        DfAnaFixGSRFH_F = DfAnaF[,c("TmpMeanSF",
                                    "TmpMeanFH",
                                    "PreMeanSF",
                                    "PreMeanFH",
                                    "GSRInteSF",
                                    "GSRInteFixFH",
                                    "Area",
                                    "Year")]
        names(DfAnaFixGSRFH_F) = gsub("Fix","",names(DfAnaFixGSRFH_F))
        YieldFixGSRFH = predict(BestModel, newdata=DfAnaFixGSRFH_F)
        DfAnaF$YieldFixGSRFH = YieldFixGSRFH
        
        #--- Prediction (Temperature AF using the phenology of 1995) ---
        DfAnaFixTmpFH_F = DfAnaF[,c("TmpMeanSF",
                                    "TmpMeanFixFH",
                                    "PreMeanSF",
                                    "PreMeanFH",
                                    "GSRInteSF",
                                    "GSRInteFH",
                                    "Area",
                                    "Year")]
        names(DfAnaFixTmpFH_F) = gsub("Fix","",names(DfAnaFixTmpFH_F))
        YieldFixTmpFH = predict(BestModel, newdata=DfAnaFixTmpFH_F)
        DfAnaF$YieldFixTmpFH = YieldFixTmpFH
        
        #--- Prediction (Precipitation AF using the phenology of 1995) ---
        DfAnaFixPreFH_F = DfAnaF[,c("TmpMeanSF",
                                    "TmpMeanFH",
                                    "PreMeanSF",
                                    "PreMeanFixFH",
                                    "GSRInteSF",
                                    "GSRInteFH",
                                    "Area",
                                    "Year")]
        names(DfAnaFixPreFH_F) = gsub("Fix","",names(DfAnaFixPreFH_F))
        YieldFixPreFH = predict(BestModel, newdata=DfAnaFixPreFH_F)
        DfAnaF$YieldFixPreFH = YieldFixPreFH
        
        }
      if(yr_future[iyr] %in% base_years){
        DfAnaF$YieldFixGSRSF = NA
        DfAnaF$YieldFixTmpSF = NA
        DfAnaF$YieldFixPreSF = NA
        DfAnaF$YieldFixGSRFH = NA
        DfAnaF$YieldFixTmpFH = NA
        DfAnaF$YieldFixPreFH = NA
        
      }
      #--- Output ---
      opath = paste0("Prediction/prediction_",yr_future[iyr],"_",
                     gcm_list[igcm],"_",ssp_list[issp],".csv")
      write.csv(DfAnaF, file=opath,fileEncoding="SJIS", row.names=FALSE)
      
      cat("Year:",yr_future[iyr],
          ", GCM:",gcm_list[igcm],
          ", SSP:",ssp_list[issp],"was finished", "\n")

    }
  }
}
