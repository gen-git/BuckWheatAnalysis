rm(list=ls())
#--- Read packages ---
library(sf)
library(dplyr)
library(latex2exp)
library(ggplot2)

#--- Load ---
source("00setting.R")
source("functions.R")
source("make_municipal_heat_map.R")

#--- Read shape file ---
JShape = st_read(path_j_shape_future)

#- Calculate the length -
NJShape = length(JShape$geometry)

#- Extract of the data frame -
DfJShape = dplyr::select(as.data.frame(JShape), -geometry)
DfJShape = DfJShape[,c(1,4,5)]
names(DfJShape) = c("Pref","Muni","MuniCode")

#--- Plot ---
for(issp in 1:length(ssp_list)){
  # Read data
  ipath = paste0("Prediction/all_gcm_mean_",ssp_list[issp],".csv")
  dat = read.csv(ipath)

  # Output
  for(icol in 1:length(plot_fac)){
    x = dat[,plot_fac[icol]]
    if(abs_max[icol] != 0){
      y1 = which(x < - abs_max[icol])
      y2 = which(x >   abs_max[icol])
      dat[y1, plot_fac[icol]] = - abs_max[icol]
      dat[y2, plot_fac[icol]] =   abs_max[icol]
      ymin_value = - abs_max[icol]
      ymax_value = + abs_max[icol]
    }
    if(abs_max[icol] == 0){
      ymin_value = min(x, na.rm=TRUE)
      ymax_value = max(x, na.rm=TRUE)
    }
    opath = paste0("Plot/map_",ssp_list[issp],"_",plot_fac[icol],".tiff")
    tiff(opath, height=7, width=7, res=500, units="in")
    if(col_list[icol] == 1) col <- colorRampPalette(c("steelblue4","lightyellow1","darksalmon"))(12)
    if(col_list[icol] == 2) col <- colorRampPalette(c("firebrick3","lightyellow1","forestgreen"))(12)
    if(col_list[icol] == 3) col <- colorRampPalette(c("gold", "white", "blueviolet"))(12)
    if(col_list[icol] == 4) col <- colorRampPalette(c("grey44","darksalmon","goldenrod1"))(12)
    if(col_list[icol] == 5) col <- colorRampPalette(c("tan1","white","steelblue"))(12)
    if(col_list[icol] == 6) col <- colorRampPalette(c("blue", "deepskyblue", "white", "gold", "firebrick4"))(12)
    if(col_list[icol] == 7) col <- colorRampPalette(c("blue", "skyblue", "white", "yellow", "firebrick4"))(12)
    if(col_list[icol] == 8) col <- colorRampPalette(c("steelblue","goldenrod3","firebrick3"))(12)
    
    make_municipal_heat_map(shpfile       = JShape, 
                            df            = na.omit(dat),
                            graph_title   = title_name[icol],
                            legend_title  = legend_unit[icol],
                            x             = "MuniCode",
                            y             = plot_fac[icol],
                            z             = 6668, # EPSG code of shp file
                                                 # (2016~: 6668(default), 1993~2015: 4612)
                            col           = col,
                            topleft_label = "",
                            ylims         = c(23.5,  46),
                            xlims         = c(123.4, 150),
                            ymin          = ymin_value,
                            ymax          = ymax_value,
                            ssize         = font_size_list[icol]
    )
    dev.off()
    cat(icol,"/",length(plot_fac),":",plot_fac[icol],"mapped", "\n")
  }
  
}
