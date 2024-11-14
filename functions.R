f_phi_soldec = function(i_jul){
  # Thornley & France 2003 Eq. 8.7
  # Approximation of solar declination
  # Input
  #   i_jul: Julian day number (int) (1-365)
  # Output
  #   phi_soldec: solar decline (rad)
  phi_soldec = 2*pi/360 * 23.45 * sin(2*pi*(i_jul-81)/365)
  return(phi_soldec)
}

f_tau_day = function(phi_lat, phi_soldec){
  # Thornley & France 2003 Eq. 8.12-8.13
  # Approximation of day length
  # Input
  #   Latitude (rad)
  #   Solar decline (rad)
  # Output
  #   Day length (day) (1 = 24 hours)
  z = - tan(phi_lat) * tan(phi_soldec)
  tau_day = 2*acos(z)/(2*pi)
  return(tau_day)
}

f_newdata = function(DF, target_name, n){
  # Function for making new data frame for plotting
  # in which the explanatory variables assigned as "target_name"
  # are rearranged to have sequential values. The explanatory
  # variables that are not assigned are changed to the average
  # values or the value that is positioned in the first line of the data frame.
  m = length(DF[1,])
  newmat = matrix(NA, n, m)
  newDF = data.frame(newmat)
  names(newDF) = names(DF)
  for(i in 1:m){
    if(mode(DF[,i])=="character"){
      newDF[,i] = DF[1,i]
    }
    if(mode(DF[,i])=="numeric"){
      newDF[,i] = mean(DF[,i], na.rm=TRUE)
    }
  }
  x = DF[,target_name]
  newDF[,target_name] = seq(min(x,na.rm=T),max(x,na.rm=T),len=n)
  return(newDF)
}

f_DVI = function(DL,　Tmp,　param){
  # The function for calculating DVI
  # DL: Day length
  # Tmp: Temperature
  # param: Parameters[1:5]
  if(DL < abs(param[3])){
    DVR = ( 1 - exp( abs(param[1])*( DL - abs(param[3]) ) ) ) /
      ( ( 1 + exp( - abs(param[2])*(Tmp - param[4] ) ) ) / abs(param[5]) )
  }
  else{
    DVR = 0
  }
  return(DVR)
}

f_DVI_NoDL = function(Tmp,　param){
  # The function for calculating DVI
  # Tmp: Temperature
  # param: Parameters[1:2]
    DVR = max(abs(param[2])*(Tmp - param[1]), 0)
  return(DVR)
}

f1 = function(ObsFLen,Tmps,DLs,param){
  # The function that is used in f2
  # In this function, first, DVI is calculated.
  # Then, if the DVI exceed, the estimated length of the days
  # is compared with the observed length of the days
  n = length(Tmps)
  dvi = 0
  DD = 99
  EstFLen = 0
  for(i in 1:n){
    dvi = dvi + f_DVI(DLs[i], Tmps[i], param)
    if(dvi >= 1){
      EstFLen = i
      DD = ObsFLen - EstFLen
      if(EstFLen == 1) DD = 99
      break
    }
  }
  if(DD==99) DD = DD*abs((1 - dvi))
  return(DD)
}

f1_NoDL = function(ObsFLen, Tmps, param){
  # The function that is used in f2
  # In this function, first, DVI is calculated.
  # Then, if the DVI exceed, the estimated length of the days
  # is compared with the observed length of the days
  n = length(Tmps)
  dvi = 0
  DD = 99
  EstFLen = 0
  for(i in 1:n){
    dvi = dvi + f_DVI_NoDL(Tmps[i], param)
    if(dvi >= 1){
      EstFLen = i
      DD = ObsFLen - EstFLen
      if(EstFLen == 1) DD = 99
      break
    }
  }
  if(DD==99) DD = DD*abs((1 - dvi))
  return(DD)
}

f2 = function(MtDayLen, MtTmp, Start, LenDays, param){
  # Calculating the RMSE using function f1
  n = length(MtTmp[,1])
  DDs = rep(NA, n)
  for(i in 1:n){
    DDs[i] = f1(LenDays[i],
                MtTmp[i,Start[i]:365],
                MtDayLen[i,Start[i]:365],
                param)
  }
  DDs2 = sum(sqrt(DDs^2))
  return(DDs2)
}

f2_NoDL = function(MtTmp, Start, LenDays, param){
  # Calculating the RMSE using function f1
  n = length(MtTmp[,1])
  DDs = rep(NA, n)
  for(i in 1:n){
    DDs[i] = f1_NoDL(LenDays[i],
                MtTmp[i,Start[i]:365],
                param)
  }
  DDs2 = sum(sqrt(DDs^2))
  return(DDs2)
}

f3 = function(Tmps,DLs,param){
  # Calculating the length of days
  # using function f_DVI
  n = length(Tmps)
  dvi = 0
  EstLen = 0
  for(i in 1:n){
    dvi = dvi + f_DVI(DLs[i], Tmps[i], param)
    if(dvi >= 1){
      EstLen = i
      if(EstLen > 50){
        EstLen = 50
      }
      break
    }
  }
  return(EstLen)
}

f3_NoDL = function(Tmps,param){
  # Calculating the length of days
  # using function f_DVI
  n = length(Tmps)
  dvi = 0
  EstLen = 0
  for(i in 1:n){
    dvi = dvi + f_DVI_NoDL(Tmps[i], param)
    if(dvi >= 1){
      EstLen = i
      if(EstLen > 50){
        EstLen = 50
      }
      break
    }
  }
  return(EstLen)
}
