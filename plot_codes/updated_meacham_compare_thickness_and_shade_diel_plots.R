##
## Compare Thickness and Shade Daily Signal
##

library(lubridate)

####------ READ IN ALL DATA ------####
box_directory <- "C:/Users/skati/OneDrive - Montana State University/BoxMigratedData/Floodplain_Shade_Box/meacham_updated_results/"
#"C:/Users/skati/Box/Floodplain_Shade_Box/meacham_updated_results/"

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
  #assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  #assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
  assign(paste0(eachmodel[i], "_100m"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_first100m.RData")))
}

dielList_shady_100 <- list(k100_0.5_shady_100m, k100_1.0_shady_100m, k100_2.0_shady_100m, k100_3.0_shady_100m)
dielList_sunny_100 <- list(k100_0.5_sunny_100m, k100_1.0_sunny_100m, k100_2.0_sunny_100m, k100_3.0_sunny_100m)
dielList_riveronly_100 <- list(k100_0.5_riveronly_100m, k100_1.0_riveronly_100m, k100_2.0_riveronly_100m, k100_3.0_riveronly_100m)

dielList_shady_400 <- list(k400_0.5_shady_100m, k400_1.0_shady_100m, k400_2.0_shady_100m, k400_3.0_shady_100m)
dielList_sunny_400 <- list(k400_0.5_sunny_100m, k400_1.0_sunny_100m, k400_2.0_sunny_100m, k400_3.0_sunny_100m)
dielList_riveronly_400 <- list(k400_0.5_riveronly_100m, k400_1.0_riveronly_100m, k400_2.0_riveronly_100m, k400_3.0_riveronly_100m)

localbox <- "C:/Users/skati/OneDrive - Montana State University/BoxMigratedData/"
outtimes <- read.table(paste0(localbox, "HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt"))
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))
alltimes <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1-(365*86400*7))

thickness <- c("Soil 0.5m", "Soil 1.0m", "Soil 2.0m", "Soil 3.0m")

#### 5 DAYS ####
t_idx <- seq(2,24,5)
days_id <- t_idx
starttimes <- days_id*24 - 23
daytimes <- mapply(function(s) seq(s, s+23, by = 5),
                   starttimes)
daytimesvector <- c(daytimes[,1], daytimes[,2], daytimes[,3], daytimes[,4], daytimes[,5])
lubridays <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))
#################

time_idx <- seq(1,24, by = 4)+1

day_starts <- seq(1,by = 24, length.out = 24)
day_ends <- seq(24, by = 24, length.out = 24)

x_idx = 1:95
time_idx <- c(1,5,9,13,17,21)
day_starts_2 <- seq(1,by = 24, length.out = 24)[time_idx]
day_ends_2 <- seq(24, by = 24, length.out = 24)[time_idx]
hours_idx <- seq(0, 23, by = 4)+1
aquifer_z_idx <- 18:47 
 
daily_time_idx <- rep(day_starts_2, each = 6) + rep(hours_idx, times = 6) 

daily_time_idx <- daytimesvector
plotlwd <- 3

# x = k400_0.5_riveronly_100m
# modelrun = thickness[1]

plotacrosstime_daily <- function(x, modelrun) {
  plot(x[x_idx,2,30,1,"X"], 
       rowMeans(x[x_idx,2,aquifer_z_idx, daily_time_idx[1], "temp"]),
       # colMeans(temp[aquifer_z_idx,x_idx,daily_time_idx[1]]),
       type = "l",
       col = "white",
       ylim = c(0,22),
       lwd = plotlwd,
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       # main = modelrun,
       xlim = c(0,10))
  mapply(function(t,c) lines(x[x_idx,2,30,1,"X"], rowMeans(x[x_idx,2,aquifer_z_idx, t, "temp"]), col = c, lwd = plotlwd),
         daily_time_idx,
         rep(hcl.colors(24,"Fall")[t_idx], each = 5)
  )
}

plotacrosstime_daily(k400_0.5_riveronly_100m, thickness[1])


#### --- K100 --- ####
png("plots/compare_diel_thickness_and_shade_k_100.png",
    height = 1000*5,
    width = 900*5,
    res = 72*5)
par(mfcol = c(4,3),
    oma = c(4,4,4,0),
    mar = c(2,2,2,1),
    cex.main = 2,
    cex.axis = 1.8)

## RIVER ONLY ##
mapply(plotacrosstime_daily,
       dielList_riveronly_100,
       thickness)

## SHADY ##
mapply(plotacrosstime_daily,
       dielList_shady_100,
       thickness)

## SUNNY ##
mapply(plotacrosstime_daily,
       dielList_sunny_100,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")), side = 2,
      line = 1, outer = T,
      cex = 2)
mtext("Flow Path Length (m)", side = 1, line = 2, outer = T,
      cex = 2)
dev.off()

#### --- K400 --- ####
png("plots/compare_diel_thickness_and_shade_k_400.png",
    height = 1000*5,
    width = 900*5,
    res = 72*5)
par(mfcol = c(4,3),
    oma = c(4,4,4,0),
    mar = c(2,2,2,1),
    cex.main = 2,
    cex.axis = 1.8)

## RIVER ONLY ##
mapply(plotacrosstime_daily,
       dielList_riveronly_400,
       thickness)

## SHADY ##
mapply(plotacrosstime_daily,
       dielList_shady_400,
       thickness)

## SUNNY ##
mapply(plotacrosstime_daily,
       dielList_sunny_400,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")), side = 2,
      line = 1, outer = T,
      cex = 2)
mtext("Flow Path Length (m)", side = 1, line = 2, outer = T,
      cex = 2)
dev.off()

#################
###################

#### DIEL LEGEND ####
par(originalpar)
png("plots/compare_diel_thickness_and_shade_legend.png",
    height = 230*5,
    width = 400*5,
    res = 72*5)
plot(rep(5, times = length(time_idx)), 
     0:(length(time_idx)-1), 
     bty = "n",
     col = hcl.colors(24,"Fall", rev = T)[t_idx],
     pch = 175,
     cex = 5,
     xaxt = "n",
     yaxt = "n",
     ylab = "",
     xlab = "")
text(rep(4.72, times = 6), seq(11.0, 0, by = -2), 
     c("Jan", "Mar", "May", "Jul", "Sep", "Nov"), 
     cex = 0.65,
     adj = 1)
mtext("Month", side = 3, line = 0)
dev.off()



#### FIRST 100 METERS ####
plotacrosstime_daily_100 <- function(x, modelrun) {
  plot(x[x_idx,2,30,1,"X"], 
       rowMeans(x[x_idx,2,aquifer_z_idx, daily_time_idx[1], "temp"]),
       # colMeans(temp[aquifer_z_idx,x_idx,daily_time_idx[1]]),
       type = "l",
       col = plotcols[1],
       ylim = c(0,22),
       lwd = plotlwd,
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       main = modelrun,
       xlim = c(0,100))
  mapply(function(t,c) lines(x[x_idx,2,30,1,"X"], rowMeans(x[x_idx,2,aquifer_z_idx, t, "temp"]), col = c, lwd = plotlwd),
         daily_time_idx,
         rep(hcl.colors(5,"Zissou 1"), each = 5)
  )
}

#### --- K100 --- ####
png("plots/compare_diel_thickness_and_shade_k_100_extended.png",
    height = 1000*5,
    width = 900*5,
    res = 72*5)
par(mfcol = c(4,3),
    oma = c(4,4,4,0),
    mar = c(2,2,3,1),
    cex.main = 2,
    cex.axis = 1.5)

## RIVER ONLY ##
mapply(plotacrosstime_daily_100,
       dielList_riveronly_100,
       thickness)

## SHADY ##
mapply(plotacrosstime_daily_100,
       dielList_shady_100,
       thickness)

## SUNNY ##
mapply(plotacrosstime_daily_100,
       dielList_sunny_100,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")), side = 2,
      line = 1, outer = T,
      cex = 2)
mtext("Flow Path Length (m)", side = 1, line = 2, outer = T,
      cex = 2)
dev.off()


#### --- K400 --- ####
png("plots/compare_diel_thickness_and_shade_k_400_extended.png",
    height = 1000*5,
    width = 900*5,
    res = 72*5)
par(mfcol = c(4,3),
    oma = c(4,4,4,0),
    mar = c(2,2,3,1),
    cex.main = 2,
    cex.axis = 1.5)

## RIVER ONLY ##
mapply(plotacrosstime_daily_100,
       dielList_riveronly_400,
       thickness)

## SHADY ##
mapply(plotacrosstime_daily_100,
       dielList_shady_400,
       thickness)

## SUNNY ##
mapply(plotacrosstime_daily_100,
       dielList_sunny_400,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")), side = 2,
      line = 1, outer = T,
      cex = 2)
mtext("Flow Path Length (m)", side = 1, line = 2, outer = T,
      cex = 2)
dev.off()




