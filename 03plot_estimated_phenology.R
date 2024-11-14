# Program for plotting the estimated parameters and the character of the parameters

rm(list=ls())
#--- Reading libraries ---
library(sf)
library(dplyr)
library(latex2exp)
library(ggplot2)
library(fields)

#--- Loading the settings, the functions, and the data ---
source("00setting.R")
source("functions.R")
load(file="./Save/Params1.RData")
load(file="./Save/Params2.RData")
load(file="./Save/BestModel.RData")
load(file="./Save/DfAna.RData")
load(file="./Save/MtDayLen.RData")
load(file="./Save/MtTmp.RData")
load(file="./Save/MtPre.RData")
load(file="./Save/MtGSR.RData")
load(file="./Save/DfBase.RData")

#--- Reading the crop calendar data ---
DfCalendar = read.csv(file=path_calendar, fileEncoding="SJIS")

#--- Extracting data of Year 1995 ---
tgt_r = which(DfBase$Year == yr_pheno)
DfBase   = DfBase  [tgt_r, ]
MtDayLen = MtDayLen[tgt_r, ]
MtTmp    = MtTmp   [tgt_r, ]

#--- Extracting phenology data from the crop calendar
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

#- Saving data -
DfBase1995 = DfBase
save(DfBase1995, file="./Save/DfBase1995.RData")


#--- Calculating the average of the paremeters ---
av_params1 = apply(Params1, 2, mean)
av_params2 = apply(Params2, 2, mean)

#--- Calculating the standard deviation of the parameters ---
sd_params1 = apply(Params1, 2, sd)
sd_params2 = apply(Params2, 2, sd)

#--------------------------------------
#--- Plotting histogram and summary ---
#--------------------------------------
# Histogram (Param1)
opath = "Plot/hist_param1.pdf"
pdf(opath, width=7, height=5)
par(mfrow=c(2,3))
par(mar=c(4.5,4.5,1,1))
for(i in 1:5){
  x = bquote(paste("p"[.(i)]))
  hist(Params1[,i], xlab=x, main="")
}
dev.off()

# Histogram (Param2)
opath = "Plot/hist_param2.pdf"
pdf(opath, width=7, height=5)
par(mfrow=c(2,3))
par(mar=c(4.5,4.5,1,1))
for(i in 1:5){
  x = bquote(paste("p"[.(i)]))
  hist(Params2[,i], xlab=x, main="")
}
dev.off()

# Statistics (Param1)
opath = "Summary/stat_param1.txt"
sink(opath)
cat("parameter: mean, S.D.", "\n")
for(i in 1:5) cat("p",i,":",av_params1[i],", ",sd_params1[i],"\n")
sink()

# Statistics (Param2)
opath = "Summary/stat_param2.txt"
sink(opath)
cat("parameter: mean, S.D.", "\n")
for(i in 1:5) cat("p",i,":",av_params2[i],", ",sd_params2[i],"\n")
sink()

#-------------------------------------------------------------
#--- Comparison between the estimation and the observation ---
#-------------------------------------------------------------
# Param1
Sowing = DfBase$Sowing
estLen = rep(NA, length(Sowing))
estLen_plus = rep(NA, length(Sowing))
for(i in 1:length(Sowing)){
  estLen[i] = f3(MtTmp   [i, Sowing[i]:365],
                 MtDayLen[i, Sowing[i]:365],
                 av_params1)
}
rst1 = cor.test(estLen, DfBase$LenSF, test="ken")

# Param2
Flowering = DfBase$Flowering
estLen = rep(NA, length(Flowering))
estLen_plus = rep(NA, length(Sowing))
for(i in 1:length(Sowing)){
  estLen[i] = f3(MtTmp   [i, Flowering[i]:365],
                 MtDayLen[i, Flowering[i]:365],
                 av_params2)
}
rst2 = cor.test(estLen, DfBase$LenFH, test="ken")

# Output the correlation (Param1)
opath = "./Summary/cor.test_obs_vs_est_LenSF.txt"
sink(opath)
print(rst1)
sink()

# Output the correlation (Param2)
opath = "./Summary/cor.test_obs_vs_est_LenFH.txt"
sink(opath)
print(rst2)
sink()

#---------------------
#--- Plot heat map ---
#---------------------
# Calculation
MeanMtTmp    = apply(MtTmp, 2, mean)
MeanMtDayLen = apply(MtDayLen, 2, mean)
TmpChange    = seq(-4, 4, len=50)
DayLenChange = seq(-1, 1, len=50)
MtEstLen1 = matrix(NA, length(TmpChange), length(DayLenChange))
MtEstLen2 = matrix(NA, length(TmpChange), length(DayLenChange))
avSowing    = mean(Sowing)
avFlowering = mean(Flowering)
for(i in 1:length(TmpChange)){
  for(j in 1:length(DayLenChange)){
    MtEstLen1[i,j] = f3(MeanMtTmp[avSowing:365] + TmpChange[i],
                     MeanMtDayLen[avSowing:365] + DayLenChange[j],
                     av_params1)
    MtEstLen2[i,j] = f3(MeanMtTmp[avFlowering:365] + TmpChange[i],
                     MeanMtDayLen[avFlowering:365] + DayLenChange[j],
                     av_params2)
  }
}
# Output
cols = colorRampPalette(c("steelblue4", "palegoldenrod", "orangered2", "palegoldenrod"))(20)
opath = "Plot/param_character.pdf"
pdf(opath, width=7, height=3.2)
par(mfrow=c(1,2))
par(mar=c(4.5,4.5,3,5))
# Param1
minz = min(MtEstLen1)
maxz = max(MtEstLen1)
fields::image.plot(MtEstLen1, xaxt="n", yaxt="n",
                   xlab=bquote(paste("Temperature change (deg C)")),
                   ylab=bquote(paste("Day length change (day)")),
                   main = "Before flowering", col=cols, breaks=seq(minz,maxz,len=21))
axis(1, at = seq(0,1,len=length(seq(-4, 4, 1))), label=seq(-4, 4, 1))
axis(2, at = seq(0,1,len=length(seq(-1, 1, 0.5))), label=seq(-1, 1, 0.5))

# Param2
minz = min(MtEstLen2)
maxz = max(MtEstLen2)
fields::image.plot(MtEstLen2, xaxt="n", yaxt="n",
                   xlab=bquote(paste("Temperature change (deg C)")),
                   ylab=bquote(paste("Day length change (day)")),
                   main = "After flowering", col=cols, breaks=seq(minz,maxz,len=21))
axis(1, at = seq(0,1,len=length(seq(-4, 4, 1))), label=seq(-4, 4, 1))
axis(2, at = seq(0,1,len=length(seq(-1, 1, 0.5))), label=seq(-1, 1, 0.5))
dev.off()

