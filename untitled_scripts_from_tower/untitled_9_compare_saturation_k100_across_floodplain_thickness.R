## 
#
##

boxdir <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/"

k100_0.5 <- readRDS(paste0(boxdir, "K_100m_day/sunny/soil_0.5m/soil_0.5m_saturation_T1.RData"))
k100_1.0 <- readRDS(paste0(boxdir, "K_100m_day/sunny/soil_1.0m/soil_1.0m_saturation_T1.RData"))
k100_2.0 <- readRDS(paste0(boxdir, "K_100m_day/sunny/soil_2.0m/soil_2.0m_saturation_T1.RData"))
k100_3.0 <- readRDS(paste0(boxdir, "K_100m_day/sunny/soil_3.0m/soil_3.0m_saturation_T1.RData"))


filled.contour(k100_0.5[,2,1,1,"X"],
               k100_0.5[1,2,17:52,1,"Z"],
               k100_0.5[,2,17:52,1,"Sat"])
filled.contour(k100_1.0[,2,1,1,"X"],
               k100_1.0[1,2,17:57,1,"Z"],
               k100_1.0[,2,17:57,1,"Sat"])

filled.contour(k100_2.0[,2,1,1,"X"],
               k100_2.0[1,2,17:67,1,"Z"],
               k100_2.0[,2,17:67,1,"Sat"])

filled.contour(k100_3.0[,2,1,1,"X"],
               k100_3.0[1,2,17:77,1,"Z"],
               k100_3.0[,2,17:77,1,"Sat"])
