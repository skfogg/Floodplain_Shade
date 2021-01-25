#


# umaAir <- read.csv("C:/Users/Katie Fogg/Desktop/HGSwork/1710667.csv")
# plot(as.numeric(umaAir$HourlyDryBulbTemperature))
# head(umaAir$DATE)

setwd("C:/Users/Katie Fogg/Desktop/HGSwork")

# Times, 11 yrs
secondsInYr <- 86400*365
secondsInSim <- secondsInYr*11
times <- seq(0, secondsInSim-3600, 3600)
timeon <- times
timeoff <- times + 3600

#####################
# WATER TEMPERATURE #
#####################

# annual signal of daily mean temperatures
annualmax <- 22
annualmin <- 2
annualamp <- (annualmax - annualmin)/2
annualfreq <- 1/secondsInYr
annualmean <- annualmax - annualamp
annualphase <- -1.5
annualtimes <- seq(0, secondsInSim-86400, 86400)

annualsin <- annualamp*sin(2*pi*annualtimes*annualfreq + annualphase) + annualmean
plot(annualsin, type = "l")

annualdailymean <- rep(annualsin, each = 24)
plot(annualdailymean)

length(annualdailymean)

#annual amplitude sine
summerrange <- 5
winterrange <- 1
amplitudeamp <- (summerrange - winterrange)/2
amplitudemean <- summerrange - amplitudeamp
amplitudephase <- annualphase


amplitudesin <- amplitudeamp*sin(2*pi*annualtimes*annualfreq + amplitudephase) + amplitudemean
plot(amplitudesin, type = "l")
annualdailyamp <- rep(amplitudesin, each = 24)


#daily + annual streamwater input temperatures
freq <- 1/86400
phase <- -3
finalsin <- annualdailyamp*sin(2*pi*times*freq + phase) + annualdailymean
plot(finalsin, type = "l")
plot(finalsin[41000:41048], type = "l")

inputStreamTemp <- data.frame(timeon, timeoff, finalsin)
write.table(inputStreamTemp, "inputWaterTemp.txt", row.names = FALSE, col.names = FALSE)

#############################
# SOIL SURFACE TEMPERATURES #
#############################
# Times

# soil annual signal of daily mean temperatures
soilannualmax <- 35
soilannualmin <- 0
soilannualamp <- (soilannualmax - soilannualmin)/2
soilannualfreq <- 1/secondsInYr
soilannualmean <- soilannualmax - soilannualamp
soilannualphase <- -1.5
soilannualtimes <- seq(0, secondsInSim-86400, 86400)

soilannualsin <- soilannualamp*sin(2*pi*soilannualtimes*soilannualfreq + soilannualphase) + soilannualmean
plot(soilannualsin, type = "l")

soilannualdailymean <- rep(soilannualsin, each = 24)
plot(soilannualdailymean[1:48])

length(soilannualdailymean)

#annual amplitude sine
summerrange <- 27-16
winterrange <- 4- -2
soilamplitudeamp <- (summerrange - winterrange)/2
soilamplitudemean <- summerrange - soilamplitudeamp
soilamplitudephase <- soilannualphase


soilamplitudesin <- soilamplitudeamp*sin(2*pi*soilannualtimes*soilannualfreq + soilamplitudephase) + soilamplitudemean
plot(soilamplitudesin, type = "l")

soilannualdailyamp <- rep(soilamplitudesin, each = 24)
length(soilannualdailyamp)

########################
## Compound Soil Temp ##
########################
soilfreq <- 1/86400
soilphase <- -3

soilmax <- 38 #(100F)
soilmin <- 15 #(60F)
soilmean <- (soilmax + soilmin)/2
soilamp <- (soilmax - soilmin)/2

soiltemperature <- soilannualdailyamp*sin(2*pi*times*soilfreq + soilphase) + soilannualdailymean
plot(soiltemperature, type = "l")



plot(soiltemperature[1:300], type = "l")
max(soiltemperature) # 114 F
min(soiltemperature) # 21 F

inputSoilTemp <- data.frame(timeon, timeoff, soiltemperature)
write.table(inputSoilTemp, "inputSoilCompound.txt", row.names = FALSE, col.names = FALSE)

###############
# Shaded Soil #
###############
# soil annual signal of daily mean temperatures
soilannualmax <- 35-13.5
soilannualmin <- 0
soilannualamp <- (soilannualmax - soilannualmin)/2
soilannualfreq <- 1/secondsInYr
soilannualmean <- soilannualmax - soilannualamp
soilannualphase <- -1.5
soilannualtimes <- seq(0, secondsInSim-86400, 86400)

soilannualsin <- soilannualamp*sin(2*pi*soilannualtimes*soilannualfreq + soilannualphase) + soilannualmean
soilannualdailymean <- rep(soilannualsin, each = 24)

#annual amplitude sine
summerrange <- 27-16 -5
winterrange <- 4- -2 -1
soilamplitudeamp <- (summerrange - winterrange)/2
soilamplitudemean <- summerrange - soilamplitudeamp
soilamplitudephase <- soilannualphase

soilamplitudesin <- soilamplitudeamp*sin(2*pi*soilannualtimes*soilannualfreq + soilamplitudephase) + soilamplitudemean
soilannualdailyamp <- rep(soilamplitudesin, each = 24)
soilfreq <- 1/86400
soilphase <- -3

soilmax <- 38-13.5 #(100F)
soilmin <- 15 #(60F)
soilmean <- (soilmax + soilmin)/2
soilamp <- (soilmax - soilmin)/2

soiltemperatureshaded <- soilannualdailyamp*sin(2*pi*times*soilfreq + soilphase) + soilannualdailymean
plot(soiltemperatureshaded, type = "l")

max(soiltemperatureshaded) # 81 F
min(soiltemperatureshaded) # 24.8 F

inputSoilTempShaded <- data.frame(timeon, timeoff, soiltemperatureshaded)
write.table(inputSoilTempShaded, "inputSoilShaded.txt", row.names = FALSE, col.names = FALSE)

length(inputSoilTempShaded[,3])


############################################
setwd("C:/Users/Katie Fogg/Desktop/HGSwork")
soilsurface <- read.table("C:/Users/Katie Fogg/Desktop/HGSwork/inputSoilCompound.txt", skip = 1)
waterinput <- read.table("C:/Users/Katie Fogg/Desktop/HGSwork/inputWaterTemp.txt", skip = 1)
soilshaded <- read.table("C:/Users/Katie Fogg/Desktop/HGSwork/inputSoilShaded.txt", skip = 1)


plot(soilsurface[1:24,3], col = "brown", type = "l", ylim =c(min(waterinput[,3], soilsurface[,3]), max(soilsurface[,3], waterinput[,3])))
lines(waterinput[1:24,3], col = "dodgerblue")

length(waterinput[,3])
max(soilsurface[,3])

library(xts)
library(zoo)
library(lubridate)
timeseq <- seq(mdy_hms("01-01-2000 00:00:00"), by = 3600, length.out = 96360)
soil <- xts(zoo(soilsurface$V3), order.by = timeseq)
water <- xts(zoo(waterinput$V3), order.by = timeseq)
soilshade <- xts(zoo(soilshaded$V3), order.by = timeseq)

OGpar <- par()

#### SOIL ####
png(file = "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\for_presentation\\soilBC.png",
    width = 1000,
    height = 600)
par(mar = c(5,5,4,1),
    bg = "black",
    col = "gray96",
    col.axis = "gray96",
    col.lab = "gray96",
    col.main = "gray96",
    cex = 2,
    cex.axis = 1.2,
    cex.main = 2,
    cex.lab = 1.7)
plot.zoo(soil["2001"],
         ylab = expression(paste("Temperature (",degree,"C)")),
         xlab = "Month",
         col = "tan3",
         col.ticks = "gray96",
         main = "Soil Boundary Temperature",
         ylim = c(-6, 46))
dev.off()

#### WATER ####
png(file = "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\for_presentation\\waterBC.png",
    width = 1000,
    height = 600)
par(mar = c(5,5,4,1),
    bg = "black",
    col = "gray96",
    col.axis = "gray96",
    col.lab = "gray96",
    col.main = "gray96",
    cex = 2,
    cex.axis = 1.2,
    cex.main = 2,
    cex.lab = 1.7)
plot.zoo(water["2001"],
         ylab = expression(paste("Temperature (",degree,"C)")),
         xlab = "Month",
         col = "royalblue",
         col.ticks = "gray96",
         main = "Water Boundary Temperature",
         ylim = c(-6, 46), lwd =2)
dev.off()

#### SOIL SHADE ####
png(file = "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\for_presentation\\soilshadeBC.png",
    width = 1000,
    height = 600)
par(mar = c(5,5,4,1),
    bg = "black",
    col = "gray96",
    col.axis = "gray96",
    col.lab = "gray96",
    col.main = "gray96",
    cex = 2,
    cex.axis = 1.2,
    cex.main = 2,
    cex.lab = 1.7)
plot.zoo(soilshade["2001"],
         ylab = expression(paste("Temperature (",degree,"C)")),
         xlab = "Month",
         col = "tan3",
         col.ticks = "gray96",
         main = "Shaded Soil Boundary Temperature",
         ylim = c(-6, 46))
dev.off()



##### SOIL SUNNY 2 #######
png(file = "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\for_presentation\\soilBC2.png",
    width = 1000,
    height = 600)
par(mar = c(5,5,4,1),
    bg = "black",
    col = "gray96",
    col.axis = "gray96",
    col.lab = "gray96",
    col.main = "gray96",
    cex = 2,
    cex.axis = 1.2,
    cex.main = 2,
    cex.lab = 1.7)
plot.zoo(soilshade["2001"],
         ylab = expression(paste("Temperature (",degree,"C)")),
         xlab = "Month",
         col = rgb(col2rgb("chocolate")[1]/255, col2rgb("chocolate")[2]/255, col2rgb("chocolate")[3]/255, alpha = 0.5),
         col.ticks = "gray96",
         main = "Shaded Soil Boundary Temperature",
         ylim = c(-6, 46), lwd = 2)
lines(soil["2001"],
      ylab = expression(paste("Temperature (",degree,"C)")),
      xlab = "Month",
      col = rgb(col2rgb("lightgoldenrod")[1]/255, col2rgb("lightgoldenrod")[2]/255, col2rgb("lightgoldenrod")[3]/255, alpha = 0.65),
      col.ticks = "gray96",
      main = "Soil Boundary Temperature",
      ylim = c(-6, 46), lwd = 2)
dev.off()


png(file = "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\for_presentation\\soilshadeBC2.png",
    width = 1000,
    height = 600)
par(mar = c(5,5,4,1),
    bg = "black",
    col = "gray96",
    col.axis = "gray96",
    col.lab = "gray96",
    col.main = "gray96",
    cex = 2,
    cex.axis = 1.2,
    cex.main = 2,
    cex.lab = 1.7)
plot.zoo(soil["2001"],
         ylab = expression(paste("Temperature (",degree,"C)")),
         xlab = "Month",
         col = rgb(col2rgb("chocolate")[1]/255, col2rgb("chocolate")[2]/255, col2rgb("chocolate")[3]/255, alpha = 0.5),
         col.ticks = "gray96",
         main = "Soil Boundary Temperature",
         ylim = c(-6, 46), lwd = 2)
lines(soilshade["2001"],
      ylab = expression(paste("Temperature (",degree,"C)")),
      xlab = "Month",
      col = rgb(col2rgb("lightgoldenrod")[1]/255, col2rgb("lightgoldenrod")[2]/255, col2rgb("lightgoldenrod")[3]/255, alpha = 0.65),
      col.ticks = "gray96",
      main = "Shaded Soil Boundary Temperature",
      ylim = c(-6, 46), lwd = 2)
dev.off()


##### TWO PLOTS #####
png(file = "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\for_presentation\\soilBCBOTH.png",
    width = 1000,
    height = 300)
par(mar = c(5,5,4,1),
    bg = "black",
    col = "gray96",
    col.axis = "gray96",
    col.lab = "gray96",
    col.main = "gray96",
    cex = 2,
    cex.axis = 1.2,
    cex.main = 2,
    cex.lab = 1.7,
    mfrow = c(1,2))
plot.zoo(soilshade["2001"],
         ylab = expression(paste("Temperature (",degree,"C)")),
         xlab = "Month",
         col = "tan3",
         col.ticks = "gray96",
         main = "Shaded Soil Temperature",
         ylim = c(-6, 46),
         lwd = 2)
plot.zoo(soil["2001"],
         ylab = expression(paste("Temperature (",degree,"C)")),
         xlab = "Month",
         col = "tan3",
         col.ticks = "gray96",
         main = "UnShaded Soil Temperature",
         ylim = c(-6, 46),
         lwd = 2)
dev.off()

