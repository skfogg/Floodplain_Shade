##
## Process temperatureTester.o.pm.dat
##

#source('C:/Users/t24x137/Desktop/Floodplain_Shade/r_codes/functions/processAndSave.R')
source('/r_codes/functions/proces')

processAndSave("soil_3.0m", "riveronly", "400", 
               folder = "C:/Users/t24x137/Box/Floodplain_Shade_Box")


s <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/riveronly/soil_0.5m/soil_0.5m_dailymeans.RData")
plot(s[30,,1], type = "l")
lines(s[30,,6])
lines(s[30,,12])
lines(s[30,,18])

source('C:/Users/t24x137/Desktop/Floodplain_Shade/r_codes/functions/processAndSave2.R')
processAndSave2(folderpath = "C:/Users/t24x137/Box/Floodplain_Shade_Box/VGM_sensitivity/full_models/DefaultVGM/",
                modelname = "defaultvgm")



library(HGSReader)
source("C:/Users/t24x137/Desktop/Floodplain_Shade/r_codes/functions/calcDailyMean.R")
library(sp)
library(raster)
library(rgeos)
library(tidyr)
library(stringr)
library(gstat)
boxdirectory <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/"
bulkprocessindex <- c("sunny/soil_0.5m/", "sunny/soil_1.0m/", "sunny/soil_2.0m/", "sunny/soil_3.0m/",
                      "shady/soil_0.5m/", "shady/soil_1.0m/", "shady/soil_2.0m/", "shady/soil_3.0m/")
over <- mapply(function(x) paste0(boxdirectory, x),
               bulkprocessindex,
               USE.NAMES = F)
modnames <- rep(c("soil_0.5m", "soil_1.0m", "soil_2.0m", "soil_3.0m"), times = 2)

mapply(processAndSave2,
       folderpath = over[2:8],
       modelname = modnames[2:8])
