source("C:/Users/Katie Fogg/Desktop/HGSwork/r_codes/functions/ModelingFunctions.R")
library(lubridate)
library(zoo)
library(xts)
umatilla <- read.csv("C:/Users/Katie Fogg/Desktop/HGSwork/iskulpaa_db_file/RGM_RGS_WSS_Data.csv")
riverdf <- subset(umatilla, LocationName == "RGS" & ParameterName == "Temperature" & Status != "Error")
airdf <- subset(umatilla, LocationName == "WSS" & ParameterName == "Temperature"& Status != "Error")

soil30df <-airdf[14136:28270,]

river <- xts(zoo(riverdf[1:20106,3], order.by = ymd_hms(riverdf$DataDateTime[1:20106])))
soil30 <- xts(zoo(soil30df$Value, order.by = ymd_hms(soil30df$DataDateTime)))


river2 <- includeNATimes(river)
#plot.zoo(river2)

river2003 <- river2['2003']
#plot.zoo(river2003)

riverw <- rmIncompleteDays(river)
#plot.zoo(includeNATimes(riverw))



sunnysoil <- read.table("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\outputTimes_inputTemps\\IskulpaaSoil30Temp.txt",
                        col.names = c("s", "e", "temp"))
timeseq <- seq(ymd_hms("2002-01-01 00:00:00"), ymd_hms("2004-12-31 00:00:00"), by = 3600)
cmps <- xts(zoo(sunnysoil$temp, order.by = timeseq))

rivermodeled <- read.table("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\outputTimes_inputTemps\\IskulpaaRiverTemp.txt",
                           col.names = c("s", "e", "temp"))
rivermod<- xts(zoo(rivermodeled$temp, order.by = timeseq))

shadysoil <- read.table("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\outputTimes_inputTemps\\IskulpaaSoil30Temp_FakeUnshaded.txt",
                        col.names = c("s", "e", "temp"))
shady <- xts(zoo(shadysoil$temp, order.by = timeseq))

##################
### NICE PLOTS ###
##################
par(mfrow = c(3,1),
    mar = c(4,5,2,1))
plotidx <- '2002-12-17/2004-09-20'


png("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\BoundaryInputs.png",
    width = 500*8, height = 800*8, res = 72*8)
par(mfrow = c(3,1),
    mar = c(4,5,2,1),
    cex.main = 2,
    cex.axis = 1.5,
    cex.lab = 1.5)
plot.zoo(cmps[plotidx], type = "l", 
         ylim = c(0,28), 
         col = "white", 
         lwd = 2,
         ylab = expression(paste("Temperature, ", degree,"C")),
         xlab = "",
         main = "Umatilla River Temperature")
lines(as.zoo(includeNATimes(riverw)), col = "dodgerblue", lwd = 2)
lines(as.zoo(rivermod[plotidx]), col = alpha("blue", 0.5))
legend("topleft", c("Measured", "Modeled"), 
       lwd = 2, 
       col = c("dodgerblue", alpha("blue", 0.5)),
       bty = "n")

plot.zoo(cmps[plotidx], type = "l", 
         ylim = c(0,28), 
         col = "white", 
         lwd = 2,
         ylab = expression(paste("Temperature, ", degree,"C")),
         xlab = "",
         main = "Sunny Site Soil Temperature")
lines(as.zoo(soil30[plotidx]), col = "coral", lwd = 2)
lines(as.zoo(cmps[plotidx]), col = alpha("firebrick", 0.5), lwd = 2)
legend("topleft", c("Measured", "Modeled"), 
       lwd = 2, 
       col = c("coral", alpha("firebrick", 0.5)),
       bty = "n")

plot.zoo(cmps[plotidx], type = "l", 
         ylim = c(0,28), 
         col = "white", 
         lwd = 2,
         ylab = expression(paste("Temperature, ", degree,"C")),
         xlab = "",
         main = "Shady Site Soil Temperature")
### INSERT REAL SHADE HERE ###
lines(as.zoo(shady[plotidx]), col = alpha("saddlebrown", 0.5), lwd = 2)
mtext("Time", side = 1, line =2)
legend("topleft", c("Measured", "Modeled"), 
       lwd = 2, 
       col = c("peru", alpha("saddlebrown", 0.5)),
       bty = "n")
dev.off()
