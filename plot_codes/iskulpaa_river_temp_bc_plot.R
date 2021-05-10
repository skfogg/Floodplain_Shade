

river <- read.table("C:\\Users\\t24x137\\Downloads\\river.txt", skip = 1,
                    col.names = c("s", "e", "temp"))
head(river)
library(zoo)
library(lubridate)
library(xts)

r2 <- xts(zoo(river$temp, order.by = seq(ymd_hms("2002-01-01 00:00:00"), by = 3600, length.out = length(river$temp))
))




umatilla <- read.csv("C:/Users/t24x137/Downloads/RGM_RGS_WSS_Data.csv")
riverdf <- subset(umatilla, LocationName == "RGS" & ParameterName == "Temperature" & Status != "Error")
uma <- xts(zoo(riverdf[1:20106,3], order.by = ymd_hms(riverdf$DataDateTime[1:20106])))
rreal <- includeNATimes(uma)

plot.zoo(rreal, col = "white")


png("C:/Users/t24x137/Desktop/river_bc_plot.png",
    width = 800*5,
    height = 350*5,
    res = 72*5)
par(cex.lab = 1.3,
    cex.main = 1.5,
    cex.axis = 1.3,
    mar = c(5,5,5,1),
    bg = "black",
    fg = "ghostwhite",
    col.axis = "ghostwhite",
    col.main = "ghostwhite",
    col.lab = "ghostwhite")
plot.zoo(r2["2003/2004"],
         ylab = expression(paste("Temperature, ", degree, "C")),
         xlab = "Date",
         col = "dodgerblue",
         ylim = c(0,31),
         lwd = 2,
         main = "Measured and Modeled River Temperatures")
lines(as.zoo(rreal),
      col = "cadetblue1")

dev.off()




