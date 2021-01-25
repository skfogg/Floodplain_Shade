####################################
## Plot of soil input temperature ##
####################################
shadysoil <- read.table("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\outputTimes_inputTemps\\IskulpaaSoil30Temp_FakeUnshaded.txt",
                        col.names = c("s", "e", "temp"))
sunnysoil <- read.table("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\outputTimes_inputTemps\\IskulpaaSoil30Temp.txt",
                        col.names = c("s", "e", "temp"))


png("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\sunnyShadyInput.png",
    width = 800*8, height = 500*8, res = 72*8)
plot(sunnysoil$temp[1:13128], type = "l",
     col = "orange",
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Day", 
     xaxt = "n",
     main = "Sunny and Shady Soil Temperature")
lines(shadysoil$temp[1:13128], col = "brown")

axis(1, at = c(as.numeric(row.names(subset(shadysoil, e %in% c(3600*24*seasons$jday)))),
               8784, 10944, 13104),
     labels = c(seasons$dayofyear, "Jan 1", "Apr 1", "Jun 30"))
#as.numeric(row.names(subset(shadysoil, e %in% c(3600*24*seasons$jday))))

legend("topleft",
       c("Sunny", "Shady"),
       lty = 1,
       col =c("orange", "brown"))
dev.off()