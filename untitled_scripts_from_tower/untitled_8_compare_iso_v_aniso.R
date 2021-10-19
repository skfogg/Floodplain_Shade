
library(HGSReader)

boxdir <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/"
k400sunny3.0_iso <- HGSFile(paste0(boxdir,
                                    "K_400m_day/sunny/soil_3.0m/new/temperatureTestero.pm.dat"))
k400iso <- HGSGetData(k400sunny3.0_iso, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)
k400iso_v <- HGSGetData(k400sunny3.0_iso, variables = c("Vx","Vy", "Vz"), blockNumbers = 1)

saveRDS(k400iso, paste0(boxdir, "K_400m_day/sunny/soil_3.0m/new/time1_all.RData"))
saveRDS(k400iso_v, paste0(boxdir, "K_400m_day/sunny/soil_3.0m/new/velocity.RData"))


k400sunny3.0_aniso <- HGSFile(paste0(boxdir,
                                     "K_400m_day/sunny/soil_3.0m/newnew/temperatureTestero.pm.dat"))
k400aniso <- HGSGetData(k400sunny3.0_aniso, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)
k400aniso_v <- HGSGetData(k400sunny3.0_aniso, variables = c("Vx","Vy", "Vz"), blockNumbers = 1)

saveRDS(k400aniso, paste0(boxdir, "K_400m_day/sunny/soil_3.0m/newnew/time1_all.RData"))
saveRDS(k400aniso_v, paste0(boxdir, "K_400m_day/sunny/soil_3.0m/newnew/velocity.RData"))


k400sunny3.0_aniso2 <- HGSFile(paste0(boxdir,
                                     "K_400m_day/sunny/soil_3.0m/newnewnew/temperatureTestero.pm.dat"))
k400aniso2 <- HGSGetData(k400sunny3.0_aniso2, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)
k400aniso2_v <- HGSGetData(k400sunny3.0_aniso2, variables = c("Vx","Vy", "Vz"), blockNumbers = 1)

saveRDS(k400aniso2, paste0(boxdir, "K_400m_day/sunny/soil_3.0m/newnewnew/time1_all.RData"))
saveRDS(k400aniso2_v, paste0(boxdir, "K_400m_day/sunny/soil_3.0m/newnewnew/velocity.RData"))


k400sunny0.5_iso <- HGSFile(paste0(boxdir,
                                   "K_400m_day/sunny/soil_0.5m/iso/temperatureTestero.pm.dat"))
k400sun0.5_iso <- HGSGetData(k400sunny0.5_iso, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)
k400sun0.5_iso_v <- HGSGetData(k400sunny0.5_iso, variables = c("Vx","Vy", "Vz"), blockNumbers = 1)

k400sunny0.5_aniso <- HGSFile(paste0(boxdir,
                                   "K_400m_day/sunny/soil_0.5m/temperatureTestero.pm.dat"))
k400sun0.5_aniso <- HGSGetData(k400sunny0.5_aniso, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)
k400sun0.5_aniso_v <- HGSGetData(k400sunny0.5_aniso, variables = c("Vx","Vy", "Vz"), blockNumbers = 1)



dim(k400iso_v)
filled.contour(1:945,
               1:76,
               k400iso_v[,2,,1,"Vx"],
               main = "isotropic velocity x")
filled.contour(1:945,
               1:76,
               k400iso_v[,2,,1,"Vy"],
               main = "isotropic velocity y")
filled.contour(1:945,
               1:76,
               k400iso_v[,2,,1,"Vz"],
               main = "isotropic velocity z")

filled.contour(1:945,
               1:76,
               k400aniso_v[,2,,1,"Vx"],
               main = "anisotropic velocity x")
filled.contour(1:945,
               1:76,
               k400aniso_v[,2,,1,"Vy"],
               main = "anisotropic velocity y")
filled.contour(1:945,
               1:76,
               k400aniso_v[,2,,1,"Vz"],
               main = "anisotropic velocity z")

summary(c(k400iso_v[,2,,1,"Vz"]))
summary(c(k400iso_v[,2,,1,"Vx"]))
summary(c(k400iso_v[,2,,1,"Vy"]))


summary(c(k400aniso_v[,2,,1,"Vz"]))
summary(c(k400aniso_v[,2,,1,"Vx"]))
summary(c(k400aniso_v[,2,,1,"Vy"]))


filled.contour(k400iso[,2,1,1,"X"],
               k400iso[1,2,17:77,1,"Z"],
               k400iso[,2,17:77,1,"Sat"],
               main = "isotropic saturation")

filled.contour(k400aniso[,2,1,1,"X"],
               k400aniso[1,2,17:77,1,"Z"],
               k400aniso[,2,17:77,1,"Sat"],
               main = "anisotropic saturation")

filled.contour(k400aniso2[,2,1,1,"X"],
               k400aniso2[1,2,17:77,1,"Z"],
               k400aniso2[,2,17:77,1,"Sat"],
               main = "anisotropic 2 saturation")


filled.contour(k400iso[,2,1,1,"X"],
               k400iso[1,2,17:77,1,"Z"],
               k400iso[,2,17:77,1,"Head"],
               main = "isotropic head")

filled.contour(k400aniso[,2,1,1,"X"],
               k400aniso[1,2,17:77,1,"Z"],
               k400aniso[,2,17:77,1,"Head"],
               main = "anisotropic head")

plot(k400iso[,2,37,1,"Head"]~ k400iso[,2,1,1,"X"])
points(k400aniso[,2,37,1,"Head"]~ k400iso[,2,1,1,"X"], col = "blue")


### 0.5 meter run ###
dim(k400sun0.5_iso)
filled.contour(k400sun0.5_iso[,2,1,1,"X"],
               k400sun0.5_iso[1,2,17:52,1,"Z"],
               k400sun0.5_iso[,2,17:52,1,"Sat"],
               main = "0.5m: isotropic saturation")

filled.contour(k400sun0.5_aniso[,2,1,1,"X"],
               k400sun0.5_aniso[1,2,17:52,1,"Z"],
               k400sun0.5_aniso[,2,17:52,1,"Sat"],
               main = "0.5m: anisotropic saturation")

plot(k400sun0.5_aniso[,2,1,1,"X"], k400sun0.5_aniso[,2,1,1,"Depth2GWT"],
     type = "l", ylab = "Depth 2 Groundwater Table", xlab = "Flow path meter",
     col = "blue")
lines(k400sun0.5_iso[,2,1,1,"X"], k400sun0.5_iso[,2,1,1,"Depth2GWT"], col = "orange")

## max Depth2GWT Difference
max(abs(k400sun0.5_aniso[,2,1,1,"Depth2GWT"]-k400sun0.5_iso[,2,1,1,"Depth2GWT"]))

max(0.5 - k400sun0.5_aniso[,2,1,1,"Depth2GWT"])
max(k400sun0.5_aniso[,2,1,1,"Depth2GWT"]-0.5)

max(0.5 - k400sun0.5_iso[,2,1,1,"Depth2GWT"])
max(k400sun0.5_iso[,2,1,1,"Depth2GWT"]-0.5)



plot(k400sun0.5_aniso[,2,17,1,"temp"] ~ k400sun0.5_aniso[,2,1,1,"X"], type= "l",
     main = "HZ temperature across depth and flowpath length",
     ylab = "Temperature",
     xlab = "Flow path length (m)")
mapply(function(z,c) lines(k400sun0.5_aniso[,2,z,1,"temp"] ~ k400sun0.5_aniso[,2,1,1,"X"],
                           col = c),
       17:47,
       hcl.colors(31,"TealGrn"))

mapply(function(z,c) lines(k400sun0.5_iso[,2,z,1,"temp"] ~ k400sun0.5_iso[,2,1,1,"X"],
                           col = c),
       17:47,
       hcl.colors(31,"PinkYl"))
## maximum difference in temperature 
max(abs(k400sun0.5_iso[,2,17:47,1,"temp"] - k400sun0.5_aniso[,2,17:47,1,"temp"]))

#######################################
### 3.0 runs, final analysis ###
filled.contour(k400iso[,2,1,1,"X"],
               k400iso[1,2,17:77,1,"Z"],
               k400iso[,2,17:77,1,"Sat"],
               main = "3.0m: isotropic saturation")

filled.contour(k400aniso[,2,1,1,"X"],
               k400aniso[1,2,17:77,1,"Z"],
               k400aniso[,2,17:77,1,"Sat"],
               main = "3.0m: anisotropic saturation")

filled.contour(k100_3.0[,2,1,1,"X"],
               k100_3.0[1,2,17:77,1,"Z"],
               k100_3.0[,2,17:77,1,"Sat"],
               main = "k100 3.0m: anisotropic saturation")


plot(k400aniso[,2,1,1,"X"], k400aniso[,2,1,1,"Depth2GWT"],
     type = "l", ylab = "3.0m: Depth 2 Groundwater Table", xlab = "Flow path meter",
     col = "blue")
lines(k400iso[,2,1,1,"X"], k400iso[,2,1,1,"Depth2GWT"], col = "orange")

## max Depth2GWT Difference
max(abs(k400aniso[,2,1,1,"Depth2GWT"]-k400iso[,2,1,1,"Depth2GWT"]))

max(abs(k400aniso[,2,1,1,"Depth2GWT"]-3.0))
max(abs(k400iso[,2,1,1,"Depth2GWT"]-3.0))

max(3- k400aniso[,2,1,1,"Depth2GWT"])

plot(k400aniso[,2,17,1,"temp"] ~ k400aniso[,2,1,1,"X"], type= "l",
     main = "HZ temperature across depth and flowpath length",
     ylab = "Temperature",
     xlab = "Flow path length (m)")
mapply(function(z,c) lines(k400aniso[,2,z,1,"temp"] ~ k400aniso[,2,1,1,"X"],
                           col = c),
       17:41,
       hcl.colors(31,"TealGrn"))

mapply(function(z,c) lines(k400iso[,2,z,1,"temp"] ~ k400iso[,2,1,1,"X"],
                           col = c),
       17:47,
       hcl.colors(31,"PinkYl"))
## maximum difference in saturated temperature 
max(abs(k400iso[,2,17:41,1,"temp"] - k400aniso[,2,17:41,1,"temp"]))

