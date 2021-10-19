library(lubridate)
library(zoo)
library(xts)
library(stringr)

## WHEN WORKING ON TOWER:
#box_directory <- "C:/Users/t24x137/Box/UmatillaShare/"

## WHEN WORKING ON LENOVO:
box_directory <- "C:/Users/skati/Box/UmatillaShare/" 

atm_col <- c("IDX", "DateTime", "Temp", "Lux")
soil_col <- c("IDX", "DateTime", "Temp")

# SUNNY ATM: 10355260
sunnyatm1 <- read.csv(paste0(box_directory, "sun/10355260_0.csv"), 
                      skip = 1,
                      col.names = atm_col)
sunnyatm2 <- read.csv(paste0(box_directory, "sun/10355260_1_20_21.csv"), 
                      skip = 1,
                      col.names = atm_col)
sunnyatm3 <- read.csv(paste0(box_directory, "sun/10355260_4_22_21.csv"),
                       skip = 1,
                      col.names = atm_col)
sunnyatm <- rbind(sunnyatm1, sunnyatm2, sunnyatm3)

# SUNNY SOIL: 10546881
sunnysoil1 <- read.csv(paste0(box_directory, "sun/10546881.csv"), 
                       skip = 1,
                       col.names = soil_col)
sunnysoil2 <- read.csv(paste0(box_directory, "sun/10546881_1_20_21.csv"), 
                       skip = 1,
                       col.names = soil_col)
sunnysoil3 <- read.csv(paste0(box_directory, "sun/10546881_4_22_21.csv"),
                       skip = 1,
                       col.names = soil_col)
sunnysoil <- rbind(sunnysoil1, sunnysoil2, sunnysoil3)

# SHADY ATM: 10546877, 10761229
shadyatm1 <- read.csv(paste0(box_directory, "shade/10546877.csv"), 
                     skip = 1,
                     col.names = atm_col)
shadyatm2 <- read.csv(paste0(box_directory, "shade/10546877_1_20_21.csv"), 
                      skip = 1,
                      col.names = atm_col)
shadyatm3 <- read.csv(paste0(box_directory, "shade/10761229_4_22_21.csv"),
                      skip = 1,
                      col.names = atm_col)
shadyatm <- rbind(shadyatm1, shadyatm2, shadyatm3)

# SHADY SOIL: 10546878, 1037964
shadysoil1 <- read.csv(paste0(box_directory, "shade/10546878.csv"), 
                      skip = 1,
                      col.names = soil_col)
shadysoil2 <- read.csv(paste0(box_directory, "shade/10371964_4_22_21.csv"),
                       skip = 1,
                       col.names = soil_col)
shadysoil <- rbind(shadysoil1, shadysoil2)

sunnyatmx <- xts(zoo(sunnyatm[,3:4], order.by = mdy_hms(sunnyatm$DateTime)))
shadyatmx <- xts(zoo(shadyatm[,3:4], order.by = mdy_hms(shadyatm$DateTime)))

sunnysoilx <- xts(zoo(sunnysoil[,3], order.by = mdy_hms(sunnysoil$DateTime)))
shadysoilx <- xts(zoo(shadysoil[,3], order.by = mdy_hms(shadysoil$DateTime)))

## Sunny v. Shady Lux
plot.zoo(sunnyatmx$Lux,
         col = "orange",
         main = "Lux Compare",
         ylab = "Lux")
lines(as.zoo(shadyatmx$Lux),
      col = "brown")
legend("topright",
       c("Sunny", "Shady"),
       lty = 1,
       col = c("orange", "brown"))

## Sunny v. Shady Temp
plot.zoo(sunnyatmx$Temp,
         col= "orange",
         main = "Atm Temp Compare",
         ylab = "Temperature")
lines(as.zoo(shadyatmx$Temp),
      col = "brown")
legend("topright",
       c("Sunny", "Shady"),
       lty = 1,
       col = c("orange", "brown"))

## Sunny v. Shady Soil Temp
plot.zoo(sunnysoilx,
         type = "l",
         col = "orange",
         main = "Soil Temp Compare",
         ylim = c(min(shadysoil$Temp, sunnysoil$Temp), 
                  max(shadysoil$Temp, sunnysoil$Temp)),
         ylab = "Temperature")
lines(as.zoo(shadysoilx),
     col = "brown")
legend("topright",
       c("Sunny", "Shady"),
       lty = 1,
       col = c("orange", "brown"))

## Sunny atmospheric temp and soil temp
plot.zoo(sunnyatmx$Temp,
     col = "dodgerblue")
lines(as.zoo(sunnysoilx),
             col = "saddlebrown")

## Shady atmospheric temp and soil temp
plot.zoo(shadyatmx$Temp,
         col = "dodgerblue")
lines(as.zoo(shadysoilx),
      col = "saddlebrown")

##### DATA CLEANING FOR USE IN COMPUND SINE MODELING #####

## Clean up and save sunny soil data:
plot(sunnysoilx["2020-07-29"])
plot(sunnysoilx["2020-09-21/2020-09-23"])
plot(sunnysoilx["2021-01-20/2021-01-24"])
plot(sunnysoilx["2021-04-22/"])

# * Remove [07-29-2020], [09-22-2020], [01-20-2021:01-23-2021], and [04-22-2021] *
remove_dates_sunny_soil <- c("07/29/20", "09/22/20", "01/20/21", "01/22/21", "01/23/21", "04/22/21")
for(i in remove_dates_sunny_soil){
   sunnysoil <- sunnysoil[!complete.cases(str_locate(sunnysoil$DateTime, i)),]
}

plot(sunnysoil$Temp, type = "l")
write.csv(sunnysoil, paste0(box_directory, "sun/sunnysoil.csv"), row.names = F)

## Clean up and save shady soil data:
plot(shadysoilx["2020-07-29"])
plot(shadysoilx["2020-09-09"])
plot(shadysoilx["2021-01-21"])
plot(shadysoilx["2021-04-22/"])

# * Remove [07/29/20], [09/09/20], [01/21/21], [04/22/21] * #
remove_dates_shady_soil <- c("07/29/20", "09/09/20", "01/21/21", "04/22/21")
for(i in remove_dates_shady_soil){
   shadysoil <- shadysoil[!complete.cases(str_locate(shadysoil$DateTime, i)),]
}
plot(shadysoil$Temp, type = "l")
write.csv(shadysoil, paste0(box_directory, "shade/shadysoil.csv"), row.names = F)






## Inner join sunny atmospheric data with sunny soil data
sunnys <- merge(sunnyatm[,c(2:4)], sunnysoil[,c(2:3)], 
                by = "DateTime",
                sort = F)
names(sunnys) <- c("DateTime", "atmTemp", "Lux", "soilTemp")
sunnys <- sunnys[1:4200,]
sunnysx <- xts(zoo(sunnys[,2:4], order.by = mdy_hms(sunnys$DateTime)))
sunnysx

timeid <- "2020-08-01/2020-08-08"
plot.zoo(sunnysx$atmTemp[timeid], 
     type = "l", 
     col = "dodgerblue",
     ylim = c(-10,50))
lines(as.zoo(sunnysx$soilTemp[timeid]),
      col = "saddlebrown")

sunnysxmeans <- apply.daily(sunnysx, mean)

plot.zoo(sunnysxmeans$atmTemp,
         col = "blue")
lines(as.zoo(sunnysxmeans$soilTemp),
      col = "brown")

plot(smooth(sunnysx$atmTemp), type = "l")

plot(soilTemp~atmTemp, sunnys)

## *** LOOK INTO AUTOCORRELATION OF THE 2 TIME SERIES...






library(lubridate)
library(zoo)
library(xts)


umatilla <- read.csv("C:/Users/t24x137/Box/HGSwork/iskulpaa_db_file/RGM_RGS_WSS_Data.csv")
airdf <- subset(umatilla, LocationName == "WSS" & ParameterName == "Temperature"& Status != "Error")

air1 <- airdf[1:14135,]
soil30df <-airdf[14136:112871,]   #[14136:28270,]
soil30df <- soil30df[complete.cases(soil30df),]

air <- xts(zoo(air1[,3], order.by = ymd_hms(air1$DataDateTime)))
airdf2003 <- (as.data.frame(air["2003"]))
colnames(airdf2003) <- "temp"
airdf2003$DataDateTime <- ymd_hms(index(air["2003"]))


soil30 <- xts(zoo(soil30df$Value, order.by = ymd_hms(soil30df$DataDateTime)))
soil30df2003 <- (as.data.frame(soil30["2003"]))
soil30df2003 <- as.data.frame(soil30df2003[1:7904,])
colnames(soil30df2003) <- "temp"
soil30df2003$DataDateTime <- ymd_hms(index(soil30["2003"]))

airdf2003$hdofyr <- yday(airdf2003$DataDateTime) + hour(airdf2003$DataDateTime)/24
soil30df2003$hdofyr <- yday(soil30df2003$DataDateTime) + hour(soil30df2003$DataDateTime)/24
sunnysoil$hdofyr <- yday(mdy_hms(sunnysoil$DateTime)) + hour(mdy_hms(sunnysoil$DateTime))/24
shadysoil$hdofyr <- yday(mdy_hms(shadysoil$DateTime)) + hour(mdy_hms(shadysoil$DateTime))/24
sunnyatm$hdofyr <- yday(mdy_hms(sunnyatm$DateTime)) + hour(mdy_hms(sunnyatm$DateTime))/24
shadyatm$hdofyr <- yday(mdy_hms(shadyatm$DateTime)) + hour(mdy_hms(shadyatm$DateTime))/24



plot(temp ~ hdofyr, soil30df2003,
     type = "l",
     ylab = "Temperature",
     xlab = "Julian Day",
     xlim = c(200,300),
     main = "Iskulpaa & Meacham Soil Temperatures",
     ylim = c(min(soil30df2003$temp), max(sunnysoil$Temp)))
lines(Temp ~ hdofyr, sunnysoil,
      col = "orange")
lines(Temp ~ hdofyr, shadysoil,
      col = "brown")
legend("topright",
       c("Iskulpaa", "Meacham Sunny", "Meacham Shady"),
       lty = 1,
       col = c("black", "orange", "brown"))


plot(temp ~ hdofyr, airdf2003,
     type = "l",
     ylab = "Temperature",
     xlab = "Julian Day",
     xlim = c(200,280),
     main = "Iskulpaa & Meacham Atmosphere Temperatures",
     ylim = c(min(airdf2003$temp), max(sunnyatm$Temp)),
     lwd = 2)
lines(Temp ~ hdofyr, sunnyatm,
      col = "orange",
      lwd = 2)
lines(temp ~ hdofyr, airdf2003,
      col = "black",
      lwd = 2)

lines(Temp ~ hdofyr, shadyatm,
      col = "brown",
      lwd = 2)
legend("topright",
       c("Iskulpaa", "Meacham Sunny", "Meacham Shady"),
       lty = 1,
       col = c("black", "orange", "brown"))


