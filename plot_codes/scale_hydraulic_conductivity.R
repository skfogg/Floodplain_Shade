
library(lubridate)

####------ READ IN ALL DATA ------####
box_directory <- "C:/Users/skati/OneDrive - Montana State University/BoxMigratedData/Floodplain_Shade_Box/meacham_updated_results/"

kvals <- rep(c("100", "400"), each = 12)
bcvals <- rep(rep(c("sunny", "shady", "riveronly"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)

eachfolder <- numeric(24)
for (i in 1:24){
  eachfolder[i] <- paste0(box_directory, "K_", kvals[i], "m_day/", bcvals[i], "/", "soil_", soilvals[i], "m")
}

eachmodel <- numeric(24)
for (i in 1:24){
  eachmodel[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}

for (i in 1:24){
  assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
}

# localbox <- "C:/Users/skati/OneDrive - Montana State University/BoxMigratedData"
outtimes <- read.table("C:/Users/skati/OneDrive - Montana State University/BoxMigratedData/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

listofHZidx <- list(6:36, 11:40, 21:50, 31:60,
                    6:36, 11:40, 21:50, 31:60)


dim(k100_1.0_shady)

plot(colMeans(k100_0.5_shady[listofHZidx[[1]],,2]))
points(colMeans(k400_0.5_sunny[listofHZidx[[1]],,2]), col = "green")

plot(colMeans(k100_1.0_shady[listofHZidx[[2]],,2]))
points(colMeans(k400_1.0_sunny[listofHZidx[[2]],,2]), col = "green")

plot(colMeans(k100_2.0_shady[listofHZidx[[3]],,2]))
points(colMeans(k400_2.0_sunny[listofHZidx[[3]],,2]), col = "green")

plot(colMeans(k100_3.0_shady[listofHZidx[[4]],,2]) ~ k100_3.0_shady_s[,1,1,"X"],
     ylim = c(2,15))
points(colMeans(k400_3.0_sunny[listofHZidx[[4]],,2]) ~ k100_3.0_sunny_s[,1,1,"X"], 
       col = "green")


png("plots/scaling_hydraulic_cond.png",
    width = 800*5,
    height = 1200*5,
    res = 72*5)
par(mfcol = c(4,2),
    mar = c(4,4,2,1),
    cex.axis = 1.3,
    cex.lab = 1.3,
    cex.main = 1.3)

## Scaling Shady 100 -> 400 meters per day
plot(colMeans(k100_3.0_shady[listofHZidx[[4]],,2]) ~ I(k100_3.0_shady_s[,1,1,"X"]*4),
     ylim = c(2,15),
     xlim = c(0,2000),
     main = "3.0m scaled: shady",
     xlab = "Flow path length",
     ylab = "Temperature",
     lwd = 2)
points(colMeans(k400_3.0_shady[listofHZidx[[4]],,2]) ~ k100_3.0_shady_s[,1,1,"X"], 
       col = "green", lwd = 2)
legend("topright", c("k = 100", "k = 400"), col = c("black", "green"), pch = c(1,1), pt.lwd = 2)

plot(colMeans(k100_2.0_shady[listofHZidx[[3]],,2]) ~ I(k100_2.0_shady_s[,1,1,"X"]*4),
     ylim = c(2,15),
     xlim = c(0,2000),
     main = "2.0m scaled: shady",
     xlab = "Flow path length",
     ylab = "Temperature")
points(colMeans(k400_2.0_shady[listofHZidx[[3]],,2]) ~ k100_2.0_shady_s[,1,1,"X"], 
       col = "green")

plot(colMeans(k100_1.0_shady[listofHZidx[[2]],,2]) ~ I(k100_1.0_shady_s[,1,1,"X"]*4),
     ylim = c(2,15),
     xlim = c(0,2000),
     main = "1.0m scaled: shady",
     xlab = "Flow path length",
     ylab = "Temperature")
points(colMeans(k400_1.0_shady[listofHZidx[[2]],,2]) ~ k100_1.0_shady_s[,1,1,"X"], 
       col = "green")

plot(colMeans(k100_0.5_shady[listofHZidx[[1]],,2]) ~ I(k100_0.5_shady_s[,1,1,"X"]*4),
     ylim = c(2,15),
     xlim = c(0,2000),
     main = "1.0m scaled: shady",
     xlab = "Flow path length",
     ylab = "Temperature")
points(colMeans(k400_0.5_shady[listofHZidx[[1]],,2]) ~ k100_0.5_shady_s[,1,1,"X"], 
       col = "green")



## Scaling sunny 100 -> 400 meters per day
plot(colMeans(k100_3.0_sunny[listofHZidx[[4]],,2]) ~ I(k100_3.0_sunny_s[,1,1,"X"]*4),
     ylim = c(2,15),
     xlim = c(0,2000),
     main = "3.0m scaled: sunny",
     xlab = "Flow path length",
     ylab = "Temperature")
points(colMeans(k400_3.0_sunny[listofHZidx[[4]],,2]) ~ k100_3.0_sunny_s[,1,1,"X"], 
       col = "green")
legend("topright", c("k = 100", "k = 400"), col = c("black", "green"), pch = c(1,1), pt.lwd = 2)


plot(colMeans(k100_2.0_sunny[listofHZidx[[3]],,2]) ~ I(k100_2.0_sunny_s[,1,1,"X"]*4),
     ylim = c(2,15),
     xlim = c(0,2000),
     main = "2.0m scaled: sunny",
     xlab = "Flow path length",
     ylab = "Temperature")
points(colMeans(k400_2.0_sunny[listofHZidx[[3]],,2]) ~ k100_2.0_sunny_s[,1,1,"X"], 
       col = "green")

plot(colMeans(k100_1.0_sunny[listofHZidx[[2]],,2]) ~ I(k100_1.0_sunny_s[,1,1,"X"]*4),
     ylim = c(2,15),
     xlim = c(0,2000),
     main = "1.0m scaled: sunny",
     xlab = "Flow path length",
     ylab = "Temperature")
points(colMeans(k400_1.0_sunny[listofHZidx[[2]],,2]) ~ k100_1.0_sunny_s[,1,1,"X"], 
       col = "green")

plot(colMeans(k100_0.5_sunny[listofHZidx[[1]],,2]) ~ I(k100_0.5_sunny_s[,1,1,"X"]*4),
     ylim = c(2,15),
     xlim = c(0,2000),
     main = "0.5m scaled: sunny",
     xlab = "Flow path length",
     ylab = "Temperature")
points(colMeans(k400_0.5_sunny[listofHZidx[[1]],,2]) ~ k100_0.5_sunny_s[,1,1,"X"], 
       col = "green")

dev.off()