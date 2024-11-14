#--- Read library ---
library(latex2exp)

#---------------
#--- Setting ---
#---------------
#- Path of "BuckWheatData" folder -
path_BWD = "../../Data/BuckWheatData"

#- Path of the crop data -
path_calendar = paste0(path_BWD,"/okabe20220217/Buckwheat_calender/Buckwheat_calender_1995.csv")
path_yield    = paste0(path_BWD,"/okabe20220217/Buckwheat_yields/Buckwheat_yields.csv")

#- Path of the climate data -
dir_past_clim   = paste0(path_BWD,"/okabe20220217/PastClimaticFactorData")
dir_future_clim = paste0(path_BWD,"/okabe20220217/FutureClimaticFactorData")

#- Training year and Prediction years -
yr_pheno  = 1995
yr_gam    = 1993:2020
yr_future = c(2020:2022, 2051:2060)

#- Path of the shape files -
path_j_shape_future = "./shp_files/2020_japan.shp"
shape_years = c(2000, 2005, 2006, 2007, 2010, 2011, 2012, 2013:2020)

#- GCM list -
gcm_list = c("MRI-ESM2-0", "IPSL-CM6A-LR", "MIROC6", "ACCESS-CM2", "MPI-ESM1-2-HR")
ssp_list = c("ssp585")

#- The number of boot strapping -
boot_n = 300

#- Setting for the names of the predictions and the plots -
base_years  = 2020:2022
ana_years   = 2051:2060
fix_year    = 2022
plot_fac    = c("ChangeYield",
                "ChangeYieldFixGSRFH",     
                "ChangeYieldFixTmpFH",     
                "ChangeYieldFixPreFH",     
                "BaseTmpMeanSF",           
                "BaseTmpMeanFH",           
                "BasePreMeanSF",           
                "BasePreMeanFH",           
                "BaseGSRInteSF",           
                "BaseGSRInteFH",           
                "ChangeTmpMeanSF",         
                "ChangeTmpMeanFH",         
                "ChangePreMeanSF",         
                "ChangePreMeanFH",         
                "ChangeGSRInteSF",         
                "ChangeGSRInteFH",         
                "DiffLenSF",               
                "DiffLenFH",               
                "ChangeTmpMeanFixSF",      
                "ChangeTmpMeanFixFH",      
                "ChangePreMeanFixSF",      
                "ChangePreMeanFixFH",      
                "ChangeGSRInteFixSF",      
                "ChangeGSRInteFixFH",      
                "DiffChangeGSRInteFixSF",           
                "DiffChangeGSRInteFixFH",           
                "DiffChangeTmpMeanFixSF",           
                "DiffChangeTmpMeanFixFH",           
                "DiffChangePreMeanFixSF",           
                "DiffChangePreMeanFixFH",           
                "DiffChangeYieldFixGSRSF",
                "DiffChangeYieldFixTmpSF",
                "DiffChangeYieldFixPreSF",
                "DiffChangeYieldFixGSRFH",
                "DiffChangeYieldFixTmpFH",
                "DiffChangeYieldFixPreFH")
                
title_name  = c("Yield change",
                "Yield change",
                "Yield change",
                "Yield change",
                "Temperature (BF)",
                "Temperature (AF)",
                "Precipitation (BF)",
                "Precipitation (AF)",
                "Total radiation (BF)", 
                "Total radiation (AF)",
                "Temperature change BF",
                "Temperature change AF",
                "Precipitation change BF",
                "Precipitation change AF",
                "Total radiation change BF", 
                "Total radiation change AF",
                "Difference of period of BF",
                "Difference of period of AF",
                "Temperature change BF (Fixed)", 
                "Temperature change AF (Fixed)",
                "Precipitation change BF (Fixed)", 
                "Precipitation change AF (Fixed)",
                "Total radiation change BF (Fixed)", 
                "Total radiation change AF (Fixed)",
                "∂ Total radiation change BF", 
                "∂ Total radiation change AF",
                "∂ Temperature change BF", 
                "∂ Temperature change AF",
                "∂ Precipitation change BF", 
                "∂ Precipitation change AF",
                "∂ Yield change",
                "∂ Yield change",
                "∂ Yield change",
                "∂ Yield change",
                "∂ Yield change",
                "∂ Yield change")
font_size_list = c( rep(12, 4), rep(18, 26), rep(12, 6))
abs_max     = c(20,
                20,
                20,
                20,
                0,
                0,
                0,
                0,
                0, 
                0,
                15, 
                15,
                50,
                50,
                15, 
                15,
                1, 
                1,
                15, 
                15,
                50, 
                50,
                15, 
                15,
                3,
                3,
                3, 
                3,
                3, 
                3,
                3,
                3, 
                3,
                3,
                3,
                3)
col_list    = c(2,
                2,
                2,
                2,
                8,
                8,
                8, 
                8,
                8,
                8,
                1,
                1,
                5,
                5,
                6,
                6,
                3, 
                3,
                1,
                1,
                5, 
                5,
                6, 
                6,
                7, 
                7,
                7,
                7,
                7, 
                7,
                2,
                2,
                2,
                2,
                2,
                2)
legend_unit = c(rep(TeX("Change rate (%)"), 4),
                rep(TeX("deg C"), 2),
                rep(TeX("mm day$^{-1}$"), 2),
                rep(TeX("MJ m$^{-2}$"), 2),
                rep(TeX("Change rate (%)"), 6),
                rep(TeX("Days"), 2), 
                rep(TeX("Change rate (%)"), 18))

#- Range of the y-axis for model plot -
plot_ylim = c(0,150)

#- Labels of the explanatory variables -
fac_raw = c("TmpMeanSF",
            "PreMeanSF",
            "GSRInteSF",
            "GSRInteAll",
            "LenFlowering",
            
            "TmpMeanFH",
            "PreMeanFH",
            "GSRInteFH",
            "LenHarvest",
            
            "Area")
fac_name = c(TeX("Mean temerature BF (deg C)"),
             TeX("Mean precipitation BF (mm day$^{-1}$)"),
             TeX("Integrated solar radiation BF (MJ m$^{-2}$)"),
             TeX("Integrated solar radiation (MJ m$^{-2}$)"),
             TeX("The number of days BF (day)"),
             
             TeX("Mean temerature AF (deg C)"),
             TeX("Mean precipitation AF (mm day$^{-1}$)"),
             TeX("Integrated solar radiation AF (MJ m$^{-2}$)"),
             TeX("The number of days AF (day)"),
             
             TeX("log(Area (ha))"))
yield_name = TeX("Yield response (kg 10a$^{-1}$)")
