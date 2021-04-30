##
## Plots to compare effects of floodplain soil thickness
## K 100 m day-1
##

library(HGSReader)
library(lubridate)

shady_100_0.5 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/shady/soil_0.5m/soil_0.5m_dailymeans.RData")
shady_100_1.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/shady/soil_1.0m/soil_1.0m_dailymeans.RData")
shady_100_2.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/shady/soil_2.0m/soil_2.0m_dailymeans.RData")
shady_100_3.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/shady/soil_3.0m/soil_3.0m_dailymeans.RData")

sunny_100_0.5 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/sunny/soil_0.5m/soil_0.5m_dailymeans.RData")
sunny_100_1.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/sunny/soil_1.0m/soil_1.0m_dailymeans.RData")
sunny_100_2.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/sunny/soil_2.0m/soil_2.0m_dailymeans.RData")
sunny_100_3.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/sunny/soil_3.0m/soil_3.0m_dailymeans.RData")

riveronly_100_0.5 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/riveronly/soil_0.5m/soil_0.5m_dailymeans.RData")
riveronly_100_1.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/riveronly/soil_1.0m/soil_1.0m_riveronly_dailymeans.RData")
riveronly_100_2.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/riveronly/soil_2.0m/soil_2.0m_riveronly_dailymeans.RData")
riveronly_100_3.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/riveronly/soil_3.0m/soil_3.0m_dailymeans.RData")

structure_0.5 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/shady/soil_0.5m/soil_0.5m_modelstructure.RData")
structure_1.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/shady/soil_1.0m/soil_1.0m_modelstructure.RData")
structure_2.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/shady/soil_2.0m/soil_2.0m_modelstructure.RData")
structure_3.0 <- readRDS("C:/Users/t24x137/Box/Floodplain_Shade_Box/K_100m_day/shady/soil_3.0m/soil_3.0m_modelstructure.RData")

structureList <- list(structure_0.5, structure_1.0, structure_2.0, structure_3.0)
tempList_shady <- list(shady_100_0.5, shady_100_1.0, shady_100_2.0, shady_100_3.0)
tempList_sunny <- list(sunny_100_0.5, sunny_100_1.0, sunny_100_2.0, sunny_100_3.0)
tempList_riverOnly <- list(riveronly_100_0.5, riveronly_100_1.0, riveronly_100_2.0, riveronly_100_3.0)

x_to_800_idx <- 1:445
aquifer_z_idx <- 17:47
outtimes <- read.table("C:/Users/t24x137/Box/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

plotcols <- hcl.colors(5, "Oranges", rev = F)
#plotcols <- plotcols[2:5] #when rev = T
plotcols <- plotcols[1:4] # when rev = F
plotlwd <- 3
time_idx <- c(2,8,14,20)
x_idx <- 1:490

originalpar <- par()
  #par(mfcol = c(1,1), oma = c(0,0,0,0), mar = c(5,4,4,1)+0.1)
par(mfcol = c(4,2),
    oma = c(5,4,0,0),
    mar = c(2,2,4,1))


thickness <- c("Soil 0.5m", "Soil 1.0m", "Soil 2.0m", "Soil 3.0m")
time_idx <- c(1,3,5,7,9,11,13,15,17,19,21,23)
plotacrosstime <- function(x, temp, modelrun) {
  plot(x[x_idx,2,30,"X"], 
       colMeans(temp[aquifer_z_idx,x_idx,time_idx[1]]),
       type = "l",
       col = plotcols[1],
       ylim = c(3,19),
       lwd = plotlwd,
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       main = modelrun,
       xlim = c(0,2000))
  mapply(function(t,c) lines(x[x_idx,2,30,"X"], colMeans(temp[aquifer_z_idx,x_idx,t]), col = c, lwd = plotlwd),
         time_idx,
         hcl.colors(12,"Fall"))
}


## 2 column, no river control ##
# png("plots/compare_thickness_and_shade_k_100.png",
#     height = 1000*5,
#     width = 600*5,
#     res = 72*5)
# par(mfcol = c(4,2),
#     oma = c(4,4,4,0),
#     mar = c(2,2,3,1),
#     cex.main = 2,
#     cex.axis = 1.5)


png("plots/compare_thickness_and_shade_k_100_w_riveronly_x2000.png",
    height = 1000*5,
    width = 900*5,
    res = 72*5)
par(mfcol = c(4,3),
    oma = c(4,4,4,0),
    mar = c(2,2,3,1),
    cex.main = 2,
    cex.axis = 1.5)

## RIVER ONLY ##
mapply(plotacrosstime,
       structureList,
       tempList_riverOnly,
       thickness)
## SHADY ##
mapply(plotacrosstime,
       structureList,
       tempList_shady,
       thickness)
## SUNNY ##
mapply(plotacrosstime,
       structureList,
       tempList_sunny,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")), side = 2,
      line = 1, outer = T,
      cex = 2)
mtext("Flow Path Length (m)", side = 1, line = 2, outer = T,
      cex = 2)
dev.off()

########################
## RIVER ONLY CONTROL ##
########################
par(mfcol = c(3,1))
mapply(plotacrosstime,
       structureList,
       tempList_riverOnly,
       thickness)


############
## Legend ##
############
par(originalpar)
png("plots/compare_thickness_and_shade_legend.png",
    height = 230*5,
    width = 400*5,
    res = 72*5)
plot(rep(5, times = 12), 
     0:11, 
     bty = "n",
     col = hcl.colors(12,"Fall", rev = T),
     pch = 175,
     cex = 5,
     xaxt = "n",
     yaxt = "n",
     ylab = "",
     xlab = "")
text(rep(4.72, times = 6), seq(11.1, 0.1, by = -2), 
     c("Jan", "Mar", "May", "Jul", "Sep", "Nov"), 
     cex = 0.65,
     adj = 1)
mtext("Month", side = 3, line = 0)
dev.off()


###########################
## Other plots, not used ##
## SHADY ##
for(i in 1:4){
  plot(structure_0.5[x_idx,2,30,"X"], 
       colMeans(shady_100_0.5[aquifer_z_idx,x_idx,time_idx[i]]),
       type = "l",
       col = plotcols[1],
       ylim = c(3,19),
       lwd = plotlwd,
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       main = paste0("Shaded Floodplain: ", months(days[time_idx[i]]), " ", day(days[time_idx[i]])))
  mapply(function(x, temp, c) lines(x[x_idx,2,30,"X"], colMeans(temp[aquifer_z_idx,x_idx,time_idx[i]]), col = c, lwd = plotlwd),
         structureList,
         tempList_shady,
         plotcols)
}

### SUNNY ###
for(i in 1:4){
  plot(structure_0.5[,2,30,"X"], 
       colMeans(sunny_100_0.5[aquifer_z_idx,,time_idx[i]]),
       type = "l",
       col = plotcols[1],
       ylim = c(3,19),
       lwd = plotlwd,
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       main = paste0("Sunny Floodplain: ", months(days[time_idx[i]]), " ", day(days[time_idx[i]])))
  mapply(function(x, temp, c) lines(x[,2,30,"X"], colMeans(temp[aquifer_z_idx,,time_idx[i]]), col = c, lwd = plotlwd),
         structureList,
         tempList_sunny,
         plotcols)
}
