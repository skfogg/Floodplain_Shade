##
## SFS 2021 Presentation Plots
##

library(HGSReader)
library(lubridate)
library(xts)
library(zoo)

box_directory <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/meacham_results/"
riveronly_directory <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/iskulpaa_results/"

kvals <- rep(c("100", "400"), each = 8)
bcvals <- rep(rep(c("sunny", "shady"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 4)

eachfolder1 <- numeric(16)
for (i in 1:16){
  eachfolder1[i] <- paste0(box_directory, "K_", kvals[i], "m_day/", bcvals[i], "/", "soil_", soilvals[i], "m")
}

eachmodel1 <- numeric(16)
for (i in 1:16){
  eachmodel1[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}


ro <- rep("riveronly", times = 8)
eachfolder_ro <- numeric(8)
for (i in 1:8){
  eachfolder_ro[i] <- paste0(riveronly_directory, "K_", kvals[i], "m_day/", ro[i], "/", "soil_", soilvals[i], "m")
}
eachmodel_ro <- numeric(8)
for (i in 1:8){
  eachmodel_ro[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", ro[i])
}

eachmodel <- c(eachmodel1, eachmodel_ro)
eachfolder <- c(eachfolder1, eachfolder_ro)

for (i in 1:24){
  assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
  assign(paste0(eachmodel[i], "_f100"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_first100m.RData")))
  
  }

aquifer_z_idx <- 17:47
outtimes <- read.table("C:/Users/t24x137/Box/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

plotlwd <- 3
time_idx <- c(2,8,14,20)
x_idx <- 1:490

par(bg = "black",
    fg = "ghostwhite",
    col.axis = "ghostwhite",
    col.main = "ghostwhite",
    col.lab = "ghostwhite")
plot(k100_1.0_riveronly_s[,1,1,"X"],
     colMeans(k100_1.0_riveronly[aquifer_z_idx,,time_idx[1]]),
     type = "l",
     lwd = plotlwd,
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Flow Path Length (m)",
     main = days[time_idx[ti]],
     ylim = c(4,20))
lines(k100_1.0_shady_s[,1,1,"X"],
      colMeans(k100_1.0_shady[aquifer_z_idx,,time_idx[1]]),
      col = "brown2",
      lwd = plotlwd)
lines(k100_1.0_sunny_s[,1,1,"X"],
      colMeans(k100_1.0_sunny[aquifer_z_idx,,time_idx[1]]),
      col = "gold",
      lwd = plotlwd)

ti = 2
plot(k100_1.0_riveronly_s[,1,1,"X"],
     colMeans(k100_1.0_riveronly[aquifer_z_idx,,time_idx[ti]]),
     type = "l",
     lwd = plotlwd,
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Flow Path Length (m)",
     main = days[time_idx[ti]],
     ylim = c(4,20))
lines(k100_1.0_shady_s[,1,1,"X"],
      colMeans(k100_1.0_shady[aquifer_z_idx,,time_idx[ti]]),
      col = "brown2",
      lwd = plotlwd)
lines(k100_1.0_sunny_s[,1,1,"X"],
      colMeans(k100_1.0_sunny[aquifer_z_idx,,time_idx[ti]]),
      col = "gold",
      lwd = plotlwd)

ti = 3
plot(k100_1.0_riveronly_s[,1,1,"X"],
     colMeans(k100_1.0_riveronly[aquifer_z_idx,,time_idx[ti]]),
     type = "l",
     lwd = plotlwd,
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Flow Path Length (m)",
     main = days[time_idx[ti]],
     ylim = c(4,20))
lines(k100_1.0_shady_s[,1,1,"X"],
      colMeans(k100_1.0_shady[aquifer_z_idx,,time_idx[ti]]),
      col = "brown2",
      lwd = plotlwd)
lines(k100_1.0_sunny_s[,1,1,"X"],
      colMeans(k100_1.0_sunny[aquifer_z_idx,,time_idx[ti]]),
      col = "gold",
      lwd = plotlwd)

ti = 4
plot(k100_1.0_riveronly_s[,1,1,"X"],
     colMeans(k100_1.0_riveronly[aquifer_z_idx,,time_idx[ti]]),
     type = "l",
     lwd = plotlwd,
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Flow Path Length (m)",
     main = days[time_idx[ti]],
     ylim = c(4,20))
lines(k100_1.0_shady_s[,1,1,"X"],
      colMeans(k100_1.0_shady[aquifer_z_idx,,time_idx[ti]]),
      col = "brown2",
      lwd = plotlwd)
lines(k100_1.0_sunny_s[,1,1,"X"],
      colMeans(k100_1.0_sunny[aquifer_z_idx,,time_idx[ti]]),
      col = "gold",
      lwd = plotlwd)


ti=3
plot(k100_1.0_shady_s[,1,1,"X"],
     k100_1.0_shady[aquifer_z_idx[1],,time_idx[ti]],
     type = "l",
     lwd = plotlwd,
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Flow Path Length (m)",
     main = days[time_idx[ti]],
     ylim = c(4,20))
mapply(function(z, c) lines(k100_1.0_shady_s[,1,1,"X"], k100_1.0_shady[z,,time_idx[ti]], col = c),
       1:57,
       hcl.colors(31))

plot(k100_1.0_riveronly_s[,1,1,"X"],
     k100_1.0_riveronly[aquifer_z_idx[1],,time_idx[ti]],
     type = "l",
     lwd = 1,
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Flow Path Length (m)",
     main = days[time_idx[ti]],
     ylim = c(4,20))
mapply(function(z, c) lines(k100_1.0_riveronly_s[,1,1,"X"], k100_1.0_riveronly[z,,time_idx[ti]], col = c),
       1:57,
       hcl.colors(31))



filled.contour(k100_1.0_shady_f100[,1,1,1,"X"],
               k100_1.0_shady_f100[1,1,17:57,1,"Z"],
               k100_1.0_shady_f100[,1,17:57,1,"temp"],
               zlim = c(2,18))
abline(h = 48, lty = 2, col = "black")  
# abline(h = 45, lty = 2, col = "black")  

filled.contour(k100_1.0_sunny_f100[,1,1,1,"X"],
               k100_1.0_sunny_f100[1,1,17:57,1,"Z"],
               k100_1.0_sunny_f100[,1,17:57,1,"temp"],
               zlim = c(2,18))
abline(h = 48, lty = 2, col = "black")  

filled.contour(k100_1.0_riveronly_f100[,1,1,1,"X"],
               k100_1.0_riveronly_f100[1,1,17:57,1,"Z"],
               k100_1.0_riveronly_f100[,1,17:57,1,"temp"],
               zlim = c(2,18))
abline(h = 48, lty = 2, col = "black")  

