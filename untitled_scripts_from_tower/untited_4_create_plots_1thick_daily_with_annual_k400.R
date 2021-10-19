library(HGSReader)
library(lubridate)
source('C:/Users/t24x137/Desktop/Floodplain_Shade/r_codes/functions/retrieveDailySignal.R')


shady_400_1.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/shady/soil_1.0m/soil_1.0m_dailymeans.RData")
sunny_400_1.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/sunny/soil_1.0m/soil_1.0m_dailymeans.RData")
structure_1.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_400m_day/shady/soil_1.0m/soil_1.0m_modelstructure.RData")


shady_400_1.0_daily <- retrieveDailySignal("soil_1.0m",
                                           "shady",
                                           "400")
sunny_400_1.0_daily <- retrieveDailySignal("soil_1.0m",
                                           "sunny",
                                           "400")

days <- c(2,8,14,22)
hours <- seq(1,24,by=4)
aquifer_z_idx <- 17:47
outtimes <- read.table("C:/Users/t24x137/Box/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
x_to_plot <- 2:50

starttimes <- days*24 - 23
daytimes <- mapply(function(s) seq(s, s+23, by = 4),
                   starttimes)
daytimesvector <- c(daytimes[,1], daytimes[,2], daytimes[,3], daytimes[,4])
lubridays <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))


png("plots/shaded_1.0_daily_with_annual_k400.png",
    height = 300*3,
    width = 600*3,
    res = 71*3)
par(mfrow = c(1,1),
    mar = c(5,1.5,1,1),
    oma = c(0,4,4,0))
layout(matrix(c(1,2,2,2), 1, 4))
#layout.show(2)

plot(shady_400_1.0_daily[x_to_plot,2,30,1,"X"],
     rowMeans(shady_400_1.0_daily[x_to_plot,2,aquifer_z_idx,1,"temp"]),
     type = "n",
     ylim = c(3,21),
     ylab = expression(paste("Temperature, ", degree, "C")),
     xlab = "Flow path length, m")
mapply(function(t,c) lines(shady_400_1.0_daily[x_to_plot,2,30,1,"X"],
                           rowMeans(shady_400_1.0_daily[x_to_plot,2,aquifer_z_idx,t,"temp"]),
                           col = c,
                           lwd = 2),
       daytimesvector,
       hcl.colors(24, "Fall"))
text(x = 8,
     y = colMeans(shady_400_1.0[aquifer_z_idx,1,days]) + c(-0.9,0,1.3,0),
     labels = paste0(month(lubridays[days], label = T, abbr = T), "-",
                     day(lubridays[days])),
     pos = 3
)

plot(structure_1.0[,2,30,"X"],
     shady_400_1.0[30,,days[1]],
     type = "n",
     ylim = c(3,21),
     ylab = "",
     xlab = "Flow path length, m")
mapply(function(t, c) lines(structure_1.0[,2,30,"X"], 
                            colMeans(shady_400_1.0[aquifer_z_idx,,t]),
                            col = c,
                            lwd = 2),
       days,
       hcl.colors(4, "Fall"))
text(x = 850,
     y = colMeans(shady_400_1.0[aquifer_z_idx,450,days]) + c(0.1,0.25,0,0),
     labels = paste0(month(lubridays[days], label = T, abbr = T), "-",
                     day(lubridays[days])),
     pos = c(1,1,3,3)
)

mtext(expression(paste("Temperature, ", degree, "C")), side = 2, line = 1, outer = T)       
mtext("Shaded Floodplain w/ 1.0m Soil Layer, K = 400 m/day", side = 3, line = 1, outer = T)
dev.off()

#####################
### THE SUNNY ONE ###
#####################
png("plots/sunny_1.0_daily_with_annual_k400.png",
    height = 300*3,
    width = 600*3,
    res = 71*3)
par(mfrow = c(1,1),
    mar = c(5,1.5,1,1),
    oma = c(0,4,4,0))
layout(matrix(c(1,2,2,2), 1, 4))
#layout.show(2)

plot(sunny_400_1.0_daily[x_to_plot,2,30,1,"X"],
     rowMeans(sunny_400_1.0_daily[x_to_plot,2,aquifer_z_idx,1,"temp"]),
     type = "n",
     ylim = c(3,21),
     ylab = expression(paste("Temperature, ", degree, "C")),
     xlab = "Flow path length, m")
mapply(function(t,c) lines(sunny_400_1.0_daily[x_to_plot,2,30,1,"X"],
                           rowMeans(sunny_400_1.0_daily[x_to_plot,2,aquifer_z_idx,t,"temp"]),
                           col = c,
                           lwd = 2),
       daytimesvector,
       hcl.colors(24, "Fall"))
text(x = 8,
     y = colMeans(shady_400_1.0[aquifer_z_idx,1,days]) + c(-0.9,0,1.3,0),
     labels = paste0(month(lubridays[days], label = T, abbr = T), "-",
                     day(lubridays[days])),
     pos = 3
)

plot(structure_1.0[,2,30,"X"],
     sunny_400_1.0[30,,days[1]],
     type = "n",
     ylim = c(3,21),
     ylab = "",
     xlab = "Flow path length, m")
mapply(function(t, c) lines(structure_1.0[,2,30,"X"], 
                            colMeans(sunny_400_1.0[aquifer_z_idx,,t]),
                            col = c,
                            lwd = 2),
       days,
       hcl.colors(4, "Fall"))
text(x = 850,
     y = colMeans(sunny_400_1.0[aquifer_z_idx,450,days]) + c(0.1,0.25,0,0),
     labels = paste0(month(lubridays[days], label = T, abbr = T), "-",
                     day(lubridays[days])),
     pos = c(1,1,3,3)
)

mtext(expression(paste("Temperature, ", degree, "C")), side = 2, line = 1, outer = T)       
mtext("Sunny Floodplain w/ 1.0m Soil Layer, K = 400 m/day", side = 3, line = 1, outer = T)
dev.off()


