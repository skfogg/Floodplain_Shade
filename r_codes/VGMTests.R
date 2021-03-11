library(HGSReader)
library(lubridate)

boxdir <- "C:\\Users\\t24x137\\Box\\Floodplain_Shade_Box"
mysat <- readRDS(paste0(boxdir, "\\K_200m_day\\sunny\\soil_1.0m\\soil_1.0m_saturation_T1.RData"))
thomasat <- readRDS(paste0(boxdir, "\\VGM_sensitivity\\full_models\\ThomaVGM\\thomavgm_saturation_T1.RData"))
defaultsat <- readRDS(paste0(boxdir, "\\VGM_sensitivity\\full_models\\DefaultVGM\\defaultvgm_saturation_T1.RData"))

mytemp <- readRDS(paste0(boxdir, "\\K_200m_day\\sunny\\soil_1.0m\\soil_1.0m_dailymeans.RData"))
thomatemp <- readRDS(paste0(boxdir, "\\VGM_sensitivity\\full_models\\ThomaVGM\\thomavgm_dailymeans.RData"))
defaulttemp <- readRDS(paste0(boxdir, "\\VGM_sensitivity\\full_models\\DefaultVGM\\defaultvgm_dailymeans.RData"))

plot(mysat[80,2,45:57,1,"Sat"], mysat[80,2,45:57,1,"Z"], type = "o")
lines(thomasat[80,2,45:57,1,"Sat"], thomasat[80,2,45:57,1,"Z"], type = "o",
       col = "red")
lines(defaultsat[80,2,45:57,1,"Sat"], defaultsat[80,2,45:57,1,"Z"], type = "o",
      col = "blue")


par(mfrow = c(1,3))
plot(mysat[1,2,45:57,1,"Sat"], mysat[1,2,45:57,1,"Z"], 
     type = "p",col = "red",
     xlim = c(0,1))
mapply(function(x, c) points(mysat[x,2,45:57,1,"Sat"], mysat[1,2,45:57,1,"Z"], col = c),
       1:496,
       hcl.colors(496))
plot(thomasat[1,2,45:57,1,"Sat"], thomasat[1,2,45:57,1,"Z"], 
     type = "p",col = "red",
     xlim = c(0,1))
mapply(function(x, c) points(thomasat[x,2,45:57,1,"Sat"], thomasat[1,2,45:57,1,"Z"], col = c),
       1:496,
       hcl.colors(496))
plot(defaultsat[1,2,45:57,1,"Sat"], defaultsat[1,2,45:57,1,"Z"], 
     type = "p",col = "red",
     xlim = c(0,1))
mapply(function(x, c) points(defaultsat[x,2,45:57,1,"Sat"], defaultsat[1,2,45:57,1,"Z"], col = c),
       1:496,
       hcl.colors(496))



plot(mysat[1:496,2,48,1,"Sat"]~mysat[1:496,2,48,1,"X"], 
     type = "l", ylim = c(0,1))
mapply(function(z,c) lines(mysat[1:496,2,z,1,"Sat"]~mysat[1:496,2,48,1,"X"], col = c),
       44:57,
       hcl.colors(14))
plot(thomasat[1:496,2,48,1,"Sat"]~thomasat[1:496,2,48,1,"X"], 
     type = "l", ylim = c(0,1))
mapply(function(z,c) lines(thomasat[1:496,2,z,1,"Sat"]~thomasat[1:496,2,48,1,"X"], col = c),
       44:57,
       hcl.colors(14))
# abline(v=c(100,200,300,400,500,600,700,800, 900, 1000),col = "gray")

plot(defaultsat[1:496,2,48,1,"Sat"]~defaultsat[1:496,2,48,1,"X"], 
     type = "l", ylim = c(0,1))
mapply(function(z,c) lines(defaultsat[1:496,2,z,1,"Sat"]~defaultsat[1:496,2,48,1,"X"], col = c),
       44:57,
       hcl.colors(14))

par(mfrow=c(1,3))
plot(mytemp[27,,2]~mysat[1:496,2,27,1,"X"], type = "l", ylim = c(2,20))
mapply(function(t,c) lines(mytemp[27,,t]~mysat[1:496,2,27,1,"X"], col = c, lwd = 2),
       t = seq(1,24,by = 1),
       c = hcl.colors(24))
plot(thomatemp[27,,1]~thomasat[1:496,2,27,1,"X"], type = "l", ylim = c(2,20))
mapply(function(t,c) lines(thomatemp[27,,t]~mysat[1:496,2,27,1,"X"], col = c, lwd = 2),
       t = seq(1,24,by = 1),
       c = hcl.colors(24))

plot(defaulttemp[27,,1]~defaultsat[1:496,2,27,1,"X"], type = "l", ylim = c(2,20))
mapply(function(t,c) lines(defaulttemp[27,,t]~mysat[1:496,2,27,1,"X"], col = c, lwd = 2),
       t = seq(1,24,by = 1),
       c = hcl.colors(24))


### MAX difference between default and Thoma
mapply(function(t) max(abs(thomatemp[27,,t] - defaulttemp[27,,t])),
       1:24)
max(mapply(function(t) max(abs(thomatemp[27,,t] - defaulttemp[27,,t])),
       1:24))

par(mfrow = c(1,1))
plot(thomatemp[27,,17]~defaultsat[1:496,2,27,1,"X"], type = "l", ylim = c(12,20), lwd = 2, col = "red")
lines(defaulttemp[27,,17]~defaultsat[1:496,2,27,1,"X"], lwd = 2, col = "blue")


## MAX difference between Thoma and my VGM model:
mapply(function(t) max(abs(thomatemp[27,,t] - mytemp[27,,t])),
       1:24)
max(mapply(function(t) max(abs(thomatemp[27,,t] - mytemp[27,,t])),
           1:24))
plot(thomatemp[27,,17]~defaultsat[1:496,2,27,1,"X"], type = "l", ylim = c(12,20), lwd = 2, col = "red")
lines(mytemp[27,,17]~defaultsat[1:496,2,27,1,"X"], lwd = 2, col = "black")

