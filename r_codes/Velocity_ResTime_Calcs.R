##
## Velocity
##

library(devtools)
#install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
source("C:/Users/Katie Fogg/Desktop/HGSwork/r_codes/calcDailyMean.R")

memory.limit(size = 56000)
folderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\sunny_iskulpaa_soil_input\\"

foldername <- "soil_1.0m"

f <- HGSFile(paste0(folderpath,
                    foldername, 
                    "\\temperatureTestero.pm.dat"))
velocity <- HGSGetData(f, variables = c("Vx", "Vy", "Vz"), blockNumbers = c(1))
saveRDS(velocity, file = paste0(folderpath, foldername, "\\", foldername, "_velocity.RData"))

velocity <-readRDS("C:/Users/Katie Fogg/Desktop/HGSwork/sensitivity/K_100m_day/sunny_iskulpaa_soil_input/soil_1.0m/soil_1.0m_velocity.RData")

plot(velocity[,1,32,,"Vx"])
summary(velocity[,1,32,,"Vx"])

## Residence time of 1 meter flow path
1/mean(velocity[,1,32,,"Vx"]) #m/s

## Residence time of 1 meter flow path (in HOURS)
1/mean(velocity[,1,32,,"Vx"])/60/60

## Flow path length with a 6 month residence time
mean(velocity[,1,32,,"Vx"])*(182*86400)

## Flow path length with a 1 day residence time
mean(velocity[,1,32,,"Vx"])*86400

## Residence time of 1000 meter flow path (DAYS)
1/mean(velocity[,1,32,,"Vx"])*1000/86400
  ## approx MONTHS
1/mean(velocity[,1,32,,"Vx"])*1000/86400/30


## 
RT_labels <- c("1 day", "1 week", "1 month", "6 months")
RT_times <- c(86400, 86400*7, 86400*14, 86400*30, 86400*60, 86400*120, 86400*180)

(meters_at_RT_times <- mean(velocity[,1,32,,"Vx"])*RT_times)

