###
### Results first look
###

file <- "C:/Users/Katie Fogg/Desktop/HGSwork/Heat1/modelTests/test1/simpleTemp2Testero.pm.dat"


# Short Floodplain
simple <- unscramble(file)
# simple <- unscrambleLong(file)
# nodes <- scan(file, skip = 6817, nlines = 1497)

index <- seq(2, 6000, by=3)
#seq(2, 12000, by =3)
#
#seq(1, 4000, by = 2)

yvelocity <- scan(file, skip = 7211, nlines = 599)

mo1 <- data.frame(t = simple$temp, layer = simple$layer, col = simple$tempcolors, y = simple$y)[index,]
mo3 <- data.frame(t = simple$temp3mo, layer = simple$layer, col = simple$temp3mocolors, y = simple$y)[index,]
mo6 <- data.frame(t = simple$temp6mo, layer = simple$layer, col = simple$temp6mocolors, y = simple$y)[index,]
mo9 <- data.frame(t = simple$temp9mo, layer = simple$layer, col = simple$temp9mocolors, y = simple$y)[index,]

setwd("C:/Users/Katie Fogg/Desktop/HGSwork/7_3_19")
png("SoilShaded_MiddleLayer.png", width = 1000, height = 400)
par(bg = "black",
    col.axis = "gray95",
    col.lab = "gray95",
    col = "gray95",
    col.main = "gray95",
    fg = "gray95",
    cex = 1.5,
    cex.lab = 1.5,
    cex.main = 1.5,
    cex.axis = 1.5,
    mar = c(5,5,3,1))
plot(subset(mo1, layer == "middle")$y, 
     subset(mo1, layer == "middle")$t, 
     type = "l",
     ylab = "Temperature", 
     xlab = "Flow Path Length",
     col = "cyan",
     xlim = c(1000,0), 
     lwd = 2,
     ylim = c(0,30),
     main = "Saturated Layer Temperature",
     xaxt = "n")
lines(subset(mo3, layer == "middle")$y, 
      subset(mo3, layer == "middle")$t, 
      col = "dodgerblue",
      lwd =2)
lines(subset(mo6, layer == "middle")$y, 
      subset(mo6, layer == "middle")$t, 
      col = "brown1",
      lwd =2)
lines(subset(mo9, layer == "middle")$y, 
      subset(mo9, layer == "middle")$t, 
      col = "gold",
      lwd =2)
axis(1, at = seq(1000, 0, -200), labels = seq(0, 1000, 200))
dev.off()

scatterplot3d(x = simple$x,
              y = simple$y,
              z = simple$z,
              color = simple$tempcolors,
              xlim = c(-20,20),
              angle = 10,
              pch = 16)

plot(simple$y, simple$z, pch = ".")
library(rgl)
plot3d(x = simple$x,
       y = simple$y,
       z = simple$z,
       col = simple$temp9mocolors,
       xlim = c(-20,20))


# JANUARY #
plot(subset(mo1, layer == "middle")$y, 
     subset(mo1, layer == "top")$t, 
     type = "l",
     ylab = "Temperature", 
     xlab = "Distance",
     col = "gray75",
     xlim = c(1000,0), 
     lwd = 2,
     ylim = c(min(mo1$t), max(mo1$t)),
     main = "January")
lines(subset(mo1, layer == "middle")$y, 
      subset(mo1, layer == "topsat")$t, 
      col = "gray50",
      lwd =2)
lines(subset(mo1, layer == "middle")$y, 
      subset(mo1, layer == "middle")$t, 
      col = "gray25",
      lwd =2)
lines(subset(mo1, layer == "middle")$y, 
      subset(mo1, layer == "base")$t, 
      col = "black",
      lwd =2)

# March
plot(subset(mo3, layer == "top")$y, 
     subset(mo3, layer == "top")$t, 
     type = "l",
     ylab = "Temperature", 
     xlab = "Distance",
     col = "gray75",
     xlim = c(1000, 0), 
     lwd = 2,
     ylim = c(min(mo3$t), max(mo3$t)),
     main = "March")
lines(subset(mo3, layer == "top")$y, 
      subset(mo3, layer == "topsat")$t, 
      col = "gray50",
      lwd =2)
lines(subset(mo3, layer == "top")$y, 
      subset(mo3, layer == "middle")$t, 
      col = "gray25",
      lwd =2)
lines(subset(mo3, layer == "top")$y, 
      subset(mo3, layer == "base")$t, 
      col = "black",
      lwd =2)


# June
plot(seq(2,1000,2), 
     subset(mo6, layer == "top")$t, 
     type = "l",
     ylab = "Temperature", 
     xlab = "Distance",
     col = "gray75",
     xlim = c(1000, 0), 
     lwd = 2,
     ylim = c(min(mo6$t), max(mo6$t)),
     main = "June")
lines(seq(2,1000,2), 
      subset(mo6, layer == "topsat")$t, 
      col = "gray50",
      lwd =2)
lines(seq(2,1000,2), 
      subset(mo6, layer == "middle")$t, 
      col = "gray25",
      lwd =2)
lines(seq(2,1000,2), 
      subset(mo6, layer == "base")$t, 
      col = "black",
      lwd =2)

# September
plot(seq(2,1000,2), 
     subset(mo9, layer == "top")$t, 
     type = "l",
     ylab = "Temperature", 
     xlab = "Distance",
     col = "gray75",
     xlim = c(1000, 0), 
     lwd = 2,
     ylim = c(min(mo9$t), max(mo9$t)),
     main = "September")
lines(seq(2,1000,2), 
      subset(mo9, layer == "topsat")$t, 
      col = "gray50",
      lwd =2)
lines(seq(2,1000,2), 
      subset(mo9, layer == "middle")$t, 
      col = "gray25",
      lwd =2)
lines(seq(2,1000,2), 
      subset(mo9, layer == "base")$t, 
      col = "black",
      lwd =2)

# base_l <- raster("C:/Users/Katie Fogg/Desktop/HGSwork/simpleUnSatDebug/2_working_sat_3000m/fpLayers/longaquifer.asc")
# mid_l <- raster("C:/Users/Katie Fogg/Desktop/HGSwork/simpleUnSatDebug/2_working_sat_3000m/fpLayers/longLayer1_25.asc")
# sattop_l <- raster("C:/Users/Katie Fogg/Desktop/HGSwork/simpleUnSatDebug/2_working_sat_3000m/fpLayers/longLayer2_5.asc")
# top_l <- raster("C:/Users/Katie Fogg/Desktop/HGSwork/simpleUnSatDebug/2_working_sat_3000m/fpLayers/longLayer3_0.asc")
# 
# 
# plot(base_l)
# plot(mid_l)
# c(min(getValues(base_l)), max(getValues(base_l)))
# c(min(getValues(mid_l)), max(getValues(mid_l)))
# c(min(getValues(sattop_l)), max(getValues(sattop_l)))
# c(min(getValues(top_l)), max(getValues(top_l)))
