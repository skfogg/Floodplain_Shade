library(lubridate)
library(zoo)
library(xts)
library(stringr)
source('~/Floodplain_Shade/r_codes/functions/includeNATimes.R')

## WHEN WORKING ON TOWER:
#box_directory <- "C:/Users/t24x137/Box/UmatillaShare/"

## WHEN WORKING ON LENOVO:
box_directory <- "C:/Users/skati/Box/UmatillaShare/" 

sunnysoil <- read.csv(paste0(box_directory, "sun/sunnysoil.csv"))
shadysoil <- read.csv(paste0(box_directory, "shade/shadysoil.csv"))
sunnysoilmodel <- read.csv(paste0(box_directory, "sun/meachamsun10yr.csv"))
shadysoilmodel <- read.csv(paste0(box_directory, "shade/meachamshade10yr.csv"))

sunnysoilx <- xts(zoo(sunnysoil$Temp, order.by = mdy_hms(sunnysoil$DateTime)))
shadysoilx <- xts(zoo(shadysoil$Temp, order.by = mdy_hms(shadysoil$DateTime)))
sunnymodelx <- xts(zoo(sunnysoilmodel$temp, order.by = seq(mdy_hms("01-01-2020 00:00:00"),
                                                           by = 3600,
                                                           length.out = length(sunnysoilmodel$temp))))
shadymodelx <- xts(zoo(shadysoilmodel$temp, order.by = seq(mdy_hms("01-01-2020 00:00:00"),
                                                           by = 3600,
                                                           length.out = length(shadysoilmodel$temp))))
sunandshade <- merge(shadysoil, sunnysoil, by = "DateTime")
sunandshade <- sunandshade[, c(1,3,5)]    
colnames(sunandshade) <- c("DateTime", "shadetemp", "suntemp")
sunshadex <- xts(zoo(sunandshade, order.by = mdy_hms(sunandshade$DateTime)))
                   
plot.zoo(sunnymodelx["2020/2021"])
lines(as.zoo(sunnysoilx), col = "orange")

plot.zoo(shadymodelx["2020/2021"])
lines(as.zoo(shadysoilx), col = "pink")

png(paste0(box_directory, "meacham_soil_bc_plot.png"),
    width = 800*5,
    height = 350*5,
    res = 72*5)
par(bg = "black",
    fg = "ghostwhite",
    col.axis = "ghostwhite",
    col.lab = "ghostwhite",
    col.main = "ghostwhite",
    col.sub = "ghostwhite")
plot.zoo(sunnymodelx["2020/2021"], col = "orange",
         xlab = "Date",
         ylab = expression(paste("Temperature, ", degree, "C")),
         ylim = c(0,31),
         main = "Meacham Creek Floodplain Soil Temperatures",
         lwd =2)
lines(as.zoo(shadymodelx["2020/2021"]), col = "brown3",
      lwd = 2)
lines(as.zoo(includeNATimes(sunshadex$suntemp)), col = "gold",
      lwd = 1)
lines(as.zoo(includeNATimes(sunshadex$shadetemp)), col = "lightcoral",
      lwd = 1)
legend("topleft", c("sunny", "shady"), lwd = c(2,2), 
       col = c("gold", "brown3"), bty = "n",
       cex = 1.5)
dev.off()



