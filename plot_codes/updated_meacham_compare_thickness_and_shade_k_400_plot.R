##
## Plots to compare effects of floodplain soil thickness
## K 400 m day-1
##

library(HGSReader)
library(lubridate)

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

for (i in 13:24){
  assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
}


structure_0.5 <- k400_0.5_riveronly_s
structure_1.0 <- k400_1.0_riveronly_s
structure_2.0 <- k400_2.0_riveronly_s
structure_3.0 <- k400_3.0_riveronly_s

structureList <- list(structure_0.5, structure_1.0, structure_2.0, structure_3.0)

tempList_shady <- list(k400_0.5_shady, k400_1.0_shady, k400_2.0_shady, k400_3.0_shady)
tempList_sunny <- list(k400_0.5_sunny, k400_1.0_sunny, k400_2.0_sunny, k400_3.0_sunny)
tempList_riveronly <- list(k400_0.5_riveronly, k400_1.0_riveronly, k400_2.0_riveronly, k400_3.0_riveronly)


x_to_800_idx <- 1:445
aquifer_z_idx <- 17:47

localbox <- "C:/Users/skati/Box/"
outtimes <- read.table(paste0(localbox, "HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt"))
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))

plotcols <- hcl.colors(5, "Oranges", rev = F)
#plotcols <- plotcols[2:5] #when rev = T
plotcols <- plotcols[1:4] # when rev = F
plotlwd <- 3
time_idx <- c(2,8,14,20)
x_idx <- 1:413

originalpar <- par()
#par(mfcol = c(1,1), oma = c(0,0,0,0), mar = c(5,4,4,1)+0.1)
par(mfcol = c(4,2),
    oma = c(5,4,0,0),
    mar = c(2,2,4,1))


thickness <- c("Soil 0.5m", "Soil 1.0m", "Soil 2.0m", "Soil 3.0m")
time_idx <- c(2,7,12,17,22)
#c(1,3,5,7,9,11,13,15,17,19,21,23)
plotacrosstime <- function(x, temp, modelrun) {
  plot(x[x_idx,2,30,"X"], 
       colMeans(temp[aquifer_z_idx,x_idx,time_idx[1]]),
       type = "l",
       col = plotcols[1],
       ylim = c(3,20),
       lwd = plotlwd,
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       main = modelrun)
  mapply(function(t,c) lines(x[x_idx,2,30,"X"], colMeans(temp[aquifer_z_idx,x_idx,t]), col = c, lwd = plotlwd),
         time_idx,
         hcl.colors(length(time_idx),"Zissou 1"))
}


png("plots/compare_thickness_and_shade_k_400_2.png",
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
       tempList_riveronly,
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
