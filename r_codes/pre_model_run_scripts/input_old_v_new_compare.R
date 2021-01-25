library(zoo)
library(lubridate)
library(xts)

wd <- "C:/Users/Katie Fogg/Desktop/HGSwork"

air_sun_old <- read.table(paste0(wd, "/katie_byron_input_compare/soilTemp_katie.txt"), skip = 1)
air_shade_old <- read.table(paste0(wd,"/katie_byron_input_compare/soilTempShaded_katie.txt"), skip = 1)
stream_old <- read.table(paste0(wd, "/katie_byron_input_compare/streamTemp_katie.txt"), skip=1)

soil_sun <- read.table(paste0(wd, "/outputTimes_inputTemps/IskulpaaSoil30Temp.txt"), skip = 1)
stream <- read.table(paste0(wd, "/outputTimes_inputTemps/IskulpaaSoil30Temp.txt"), skip = 1)

plot(air_sun_old[,3], type = "l")
lines(soil_sun[,3], col = "brown")

plot(soil_sun[,3], type ="l")
