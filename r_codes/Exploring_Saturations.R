library(HGSReader)
library(lubridate)

boxdir <- "C:\\Users\\t24x137\\Box\\Floodplain_Shade_Box"
sat100 <- readRDS(paste0(boxdir, "\\K_100m_day\\sunny\\soil_1.0m\\soil_1.0m_saturation_T1.RData"))
sat200 <- readRDS(paste0(boxdir, "\\K_200m_day\\sunny\\soil_1.0m\\soil_1.0m_saturation_T1.RData"))
sat300 <- readRDS(paste0(boxdir, "\\K_300m_day\\shady\\soil_1.0m\\soil_1.0m_saturation_T1.RData"))
sat400 <- readRDS(paste0(boxdir, "\\K_400m_day\\short_runs\\sunny\\soil_1.0m\\soil_1.0m_saturation_T1.RData"))

sat100_1 <- sat100
sat100_0.5 <- readRDS(paste0(boxdir, "\\K_100m_day\\sunny\\soil_0.5m\\soil_0.5m_saturation_T1.RData"))
sat100_2 <- readRDS(paste0(boxdir, "\\K_100m_day\\sunny\\soil_2.0m\\soil_2.0m_saturation_T1.RData"))
sat100_3 <- readRDS(paste0(boxdir, "\\K_100m_day\\sunny\\soil_3.0m\\soil_3.0m_saturation_T1.RData"))


## Q: See if there's a difference in Saturations across the different values of K
plot(sat100[5,2,45:57,1,"Sat"], sat100[5,2,45:57,1,"Z"], type = "o")
lines(sat200[5,2,45:57,1,"Sat"], sat200[5,2,45:57,1,"Z"], type = "o", col = "red")
lines(sat300[5,2,45:57,1,"Sat"], sat300[5,2,45:57,1,"Z"], type = "o", col = "blue")
lines(sat400[5,2,45:57,1,"Sat"], sat400[5,2,45:57,1,"Z"], type = "o", col = "forestgreen")
## Result: No difference between unsaturated zone values across the different
### saturated K values

## Q: Is saturation in the vadose zone the same for one model longitudinally?
###  X-dimensionw
plot(sat200[1,2,45:57,1,"Sat"], sat200[1,2,45:57,1,"Z"], type = "p",col = "red")
mapply(function(x, c) points(sat200[x,2,45:57,1,"Sat"], sat200[1,2,45:57,1,"Z"], col = c),
       1:496,
       hcl.colors(496))
## Result: Differences in the first node of the unsaturated zone, but less and 
### less different as we decrease in depth
### Max difference is 12% points for 1st node:
max(sat200[1:496,2,48,1,"Sat"])-min(sat200[1:496,2,48,1,"Sat"])
### 5 percent difference for 2nd node:
max(sat200[1:496,2,49,1,"Sat"])-min(sat200[1:496,2,49,1,"Sat"])

## Q: Where does saturation "well-up" in the model? 
plot(sat200[1:496,2,48,1,"Sat"]~sat200[1:496,2,48,1,"X"], type = "l", ylim = c(0.1,0.8))
mapply(function(z,c) lines(sat200[1:496,2,z,1,"Sat"]~sat200[1:496,2,48,1,"X"], col = c),
       48:57,
       hcl.colors(10))
## Result: Down-gradient "welling-up" of water in unsat zone


## Q: How does the vadose zone saturation vary across unsat layer thickness?
plot(sat100_3[5,2,45:77,1,"Sat"], sat100_3[5,2,45:77,1,"Z"], type = "o", col = hcl.colors(4)[1], pch = 19)
lines(sat100_2[5,2,45:67,1,"Sat"], sat100_2[5,2,45:67,1,"Z"], type = "o", col = hcl.colors(4)[2], pch = 19)
lines(sat100_1[5,2,45:57,1,"Sat"], sat100_1[5,2,45:57,1,"Z"], type = "o", col = hcl.colors(4)[3], pch = 19)
lines(sat100_0.5[5,2,45:52,1,"Sat"], sat100_0.5[5,2,45:52,1,"Z"], type = "o", col = hcl.colors(4)[4], pch = 19)
## Result: Minimal variance in unsat layer thickness on saturation. Max difference is in the first node of the unsaturated zone, 
### where saturation values vary by approximately 10%. Overall the 0.5m scenario is "wetter" than the 3.0 m scenario.
### The 0.5m scenario encompasses alot of the capillary fringe area.
