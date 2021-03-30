## 
## Tilted model versus flat
##

boxdir <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/"
f <- HGSFile(paste0(boxdir,
                    "BC_investigation/flat/temperatureTestero.pm.dat"))
flat <- HGSGetData(f, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)

c <- HGSFile(paste0(boxdir,
                    "BC_investigation/current/temperatureTestero.pm.dat"))
tilt <- HGSGetData(c, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)

fullc <- HGSFile(paste0(boxdir,
                        "K_400m_day/sunny/soil_3.0m/temperatureTestero.pm.dat"))
fc1 <- HGSGetData(fullc, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)
fc2 <- HGSGetData(fullc, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 2)
fc <- HGSGetData(fullc, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"))
dim(fc2)

mo2file <- HGSFile(paste0(boxdir,
                        "BC_investigation/mo_2/temperatureTestero.pm.dat"))
mo2 <- HGSGetData(mo2file, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)

mo6file <- HGSFile(paste0(boxdir,
                          "BC_investigation/mo_6/temperatureTestero.pm.dat"))
mo6 <- HGSGetData(mo6file, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)

yr2file <- HGSFile(paste0(boxdir,
                          "BC_investigation/yr_2/temperatureTestero.pm.dat"))
yr2 <- HGSGetData(yr2file, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)

yr7file <- HGSFile(paste0(boxdir,
                          "BC_investigation/yr_7/temperatureTestero.pm.dat"))
yr7 <- HGSGetData(yr7file, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)


k400sunny3.0_redo <- HGSFile(paste0(boxdir,
                                    "K_400m_day/sunny/soil_3.0m/new/temperatureTestero.pm.dat"))
redo <- HGSGetData(k400sunny3.0_redo, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)

k400sunny3.0_redo2 <- HGSFile(paste0(boxdir,
                                    "K_400m_day/sunny/soil_3.0m/newnew/temperatureTestero.pm.dat"))
redo2 <- HGSGetData(k400sunny3.0_redo2, variables = c("X", "Y", "Z", "Sat", "Depth2GWT", "Head", "temp"), blockNumbers = 1)



## SATURATION #
dim(tilt)
filled.contour(tilt[,2,1,1,"X"],
               tilt[1,2,17:77,1,"Z"],
               tilt[,2,17:77,1,"Sat"],
               main = "tilt")

filled.contour(flat[,2,1,1,"X"],
               flat[1,2,17:77,1,"Z"],
               flat[,2,17:77,1,"Sat"],
               main = "flat")

filled.contour(fc1[,2,1,1,"X"],
               fc1[1,2,17:77,1,"Z"],
               fc1[,2,17:77,1,"Sat"],
               main = "7 yr output")


mod <- mo2
filled.contour(mod[,2,1,1,"X"],
               mod[1,2,17:77,1,"Z"],
               mod[,2,17:77,1,"Sat"],
               main = "2 mo output")

mod <- mo6
filled.contour(mod[,2,1,1,"X"],
               mod[1,2,17:77,1,"Z"],
               mod[,2,17:77,1,"Sat"],
               main = "6 mo output")

mod <- redo
filled.contour(mod[,2,1,1,"X"],
               mod[1,2,17:77,1,"Z"],
               mod[,2,17:77,1,"Sat"],
               main = "redo output")
filled.contour(mod[,2,1,1,"X"],
               mod[1,2,17:77,1,"Z"],
               mod[,2,17:77,1,"temp"],
               main = "redo output: iso")
mod <- redo2
filled.contour(mod[,2,1,1,"X"],
               mod[1,2,17:77,1,"Z"],
               mod[,2,17:77,1,"temp"],
               main = "redo output: aniso")

plot(redo2[,2,17,1,"temp"]~redo2[,2,1,1,"X"], type = "l")
mapply(function(z,c) lines(redo2[,2,z,1,"temp"]~redo2[,2,1,1,"X"], col = c),
       17:47,
       hcl.colors(31))

mapply(function(z,c) lines(redo[,2,z,1,"temp"]~redo[,2,1,1,"X"], col = c),
       17:47,
       hcl.colors(31, "Lajolla"))


plot(yr7[,2,47,1,"Depth2GWT"]~yr7[,2,47,1,"X"], type = "l")
lines(yr2[,2,47,1,"Depth2GWT"]~yr2[,2,47,1,"X"], col = "forestgreen")
lines(mo6[,2,47,1,"Depth2GWT"]~mo6[,2,47,1,"X"], col = "red")
lines(mo2[,2,47,1,"Depth2GWT"]~mo2[,2,47,1,"X"], col = "violet")
lines(redo[,2,47,1,"Depth2GWT"]~redo[,2,47,1,"X"], col = "blue")
plot(redo2[,2,47,1,"Depth2GWT"]~redo2[,2,47,1,"X"], col = "brown")


## Temp ##
filled.contour(tilt[,2,1,1,"X"],
               tilt[1,2,17:77,1,"Z"],
               tilt[,2,17:77,1,"temp"],
               main = "tilt")

filled.contour(flat[,2,1,1,"X"],
               flat[1,2,17:77,1,"Z"],
               flat[,2,17:77,1,"temp"],
               main = "flat")

filled.contour(fc1[,2,1,1,"X"],
               fc1[1,2,17:77,1,"Z"],
               fc1[,2,17:77,1,"temp"],
               main = "7 yr output")

filled.contour(mod[,2,1,1,"X"],
               mod[1,2,17:77,1,"Z"],
               mod[,2,17:77,1,"temp"],
               main = "6 mo output")



