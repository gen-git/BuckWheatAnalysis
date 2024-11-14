# Program for plotting the result of the analysis using the past data

rm(list=ls())
#--- Reading library ---
library(sf)
library(mgcv)
library(tidyverse)

#--- Loading the setting and the functions ---
source("00setting.R")
source("functions.R")

#--- Preparation of the folders ---
dir.create("Plot", show=FALSE)
dir.create("Summary", show=FALSE)

#--- Reading the data and the results ---
load(file="./Save/MtDayLen.RData")
load(file="./Save/MtTmp.RData")
load(file="./Save/MtPre.RData")
load(file="./Save/MtGSR.RData")
load(file="./Save/DfBase.RData")
load(file="./Save/Rsts.RData")
load(file="./Save/AICs.RData")
load(file="./Save/DfListClim.RData")
load(file="./Save/DfAna.RData")

#--- Selecting the model that has minimum AIC ---
BestModel = Rsts[[which(AICs==min(AICs))]]

#--- Output the summary of the best model ---
opath = paste0("Summary/summary_best_model.txt")
write(capture.output(summary(BestModel)), file=opath)

#--- Saving the result of the best model ---
save(BestModel, file="Save/BestModel.RData")

#--- Extract the climate factors selected in the best model ---
# SFac  = BestModel$sp %>% names()
SFac  = BestModel$full.sp %>% names()
SFac  = gsub("s\\(","",SFac)
SFac  = gsub("\\)","", SFac)
NSFac = length(SFac)

#--- Calculating the data for plotting ---
PredData = list(NULL)
for(i in 1:NSFac){
  newdat = f_newdata(DF=DfAna, target_name=SFac[i], n=1000)
  pred = predict(BestModel, newdata=newdat, se.fit=TRUE)
  PredData[[i]] = data.frame(newdat,
                             fit=pred$fit,
                             up=pred$fit+1.97*pred$se.fit,
                             lw=pred$fit-1.97*pred$se.fit
                             )
}

#--- Plotting the yield response to each climatic factor ---
# Deleting the file
lf = list.files("Plot")
rn = grep("response_vs_", lf)
if(length(rn) > 0) file.remove(paste0("Plot/",lf[rn]))
#- Plotting -
for(i in 1:NSFac){
  # Preparation
  dat = PredData[[i]]
  tfac = SFac[i]
  xxx = dat[,tfac]; yyy = dat[,"fit"]; yup = dat[,"up"]; ylw = dat[,"lw"]
  xname = fac_name[which(fac_raw==tfac)]
  # Plot
  opath = paste0("Plot/response_vs_",tfac,".pdf")
  pdf(opath,width=4,height=4)
  par(mar=c(3,3.2,1,1), mgp=c(2.0, 0.7, 0))
  plot(yyy~xxx, xlab=xname, ylab=yield_name, type="l",　ylim=plot_ylim)
  polygon(x=c(xxx,rev(xxx)),y=c(yup,rev(ylw)), col="grey77",　border=FALSE)
  colline = "steelblue4"
  if(tfac == "GSRInteAll") colline = "red"
  lines(yyy~xxx, col=colline)
  dev.off()
}
