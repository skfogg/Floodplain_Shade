##
## TIME-DOWN HEAT MAPS ACROSS FP LENGTH
##

library(lubridate)
library("raster")
library("akima")


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

listofSunny <- list(k100_0.5_sunny, k100_1.0_sunny, k100_2.0_sunny, k100_3.0_sunny,
                       k400_0.5_sunny, k400_1.0_sunny, k400_2.0_sunny, k400_3.0_sunny)
listofShady <- list(k100_0.5_shady, k100_1.0_shady, k100_2.0_shady, k100_3.0_shady,
                    k400_0.5_shady, k400_1.0_shady, k400_2.0_shady, k400_3.0_shady)
listofRiverOnly <- list(k100_0.5_riveronly, k100_1.0_riveronly, k100_2.0_riveronly, k100_3.0_riveronly,
                        k400_0.5_riveronly, k400_1.0_riveronly, k400_2.0_riveronly, k400_3.0_riveronly) 
listofHZidx <- list(6:36, 11:40, 21:50, 31:60,
                    6:36, 11:40, 21:50, 31:60)

#### Times ####
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
#########

getDiffs <- function(ro, x, hz_idx){
  diff.ls <- list()
  
  for(t in 1:24){
    diffs.df <- data.frame(x = k100_0.5_riveronly_s[,1,1,"X"],
                        y = rep(t, times = length(k100_0.5_riveronly_s[,1,1,"X"])),
                        diffs = colMeans(ro[hz_idx,,t]) - colMeans(x[hz_idx,,t]))
    diff.ls[[t]] <- diffs.df
  }
  big_data <- do.call(rbind, diff.ls)
  return(big_data)
}

shadeDiffs <- mapply(getDiffs,
                     listofRiverOnly,
                     listofShady,
                     listofHZidx,
                     SIMPLIFY = F)
sunnyDiffs <- mapply(getDiffs,
                     listofRiverOnly,
                     listofSunny,
                     listofHZidx,
                     SIMPLIFY = F)

for(i in 1:8){
  coordinates(shadeDiffs[[i]]) <- ~x+y
  coordinates(sunnyDiffs[[i]]) <- ~x+y
}

shadeDiffsInterp <- mapply(interp,
                           shadeDiffs,
                           MoreArgs = list(z = "diffs",
                                           xo = seq(0, 2000, by = 1),
                                           yo = seq(1,24, by = 1),
                                           extrap = T,
                                           nx = 20001,
                                           ny = 24))
sunnyDiffsInterp <- mapply(interp,
                           sunnyDiffs,
                           MoreArgs = list(z = "diffs",
                                           xo = seq(0, 2000, by = 1),
                                           yo = seq(1,24, by = 1),
                                           extrap = T,
                                           nx = 20001,
                                           ny = 24))

shadeDiffRasters <- mapply(raster,
                           shadeDiffsInterp)
sunnyDiffRasters <- mapply(raster,
                           sunnyDiffsInterp)


shady_vd_100 <- readRDS("analysis_data/shady_vd_100.RData")
shady_vd_400 <- readRDS("analysis_data/shady_vd_400.RData")
sunny_vd_100 <- readRDS("analysis_data/sunny_vd_100.RData")
sunny_vd_400 <- readRDS("analysis_data/sunny_vd_400.RData")

allShadyVD <- cbind(shady_vd_100, shady_vd_400)
allSunnyVD <- cbind(sunny_vd_100, sunny_vd_400)


png("plots/diff_from_RO_heatmaps_k100_shady.png", 
    width = 800*5, 
    height = 1200*5, res = 72*5)
par(mfrow = c(4,1),
    mar = c(3,3,1,3),
    cex.axis = 1.5)
for(i in 1:4){
  plot(shadeDiffRasters[[i]],
       asp = 0, 
       col = hcl.colors(35, "Purple-Brown", rev= T)[2:35],
       breaks = seq(-16.5, 16.5, by = 1),
       # xlim = c(0,400),
       yaxt = "n")
  abline(h = seq(2.5,24.5, by = 2))
  axis(2, at = seq(1.5,24,by = 2),
       labels = c("J", "F", "M", "A", "M", "J", 
                  "J", "A", "S", "O", "N", "D"),
       cex = 1.5)
  points(x = allShadyVD[,i], y = 1:24, type = "l", lwd = 2, col = "gray")
}
dev.off()

png("plots/diff_from_RO_heatmaps_k400_shady.png", 
    width = 800*5, 
    height = 1200*5, res = 72*5)
par(mfrow = c(4,1),
    mar = c(3,3,1,3),
    cex.axis = 1.5)
for(i in 5:8){
  plot(shadeDiffRasters[[i]],
       asp = 0, 
       col = hcl.colors(35, "Purple-Brown", rev= T)[2:35],
       breaks = seq(-16.5, 16.5, by = 1),
       # xlim = c(0,400),
       yaxt = "n")
  abline(h = seq(2.5,24.5, by = 2))
  axis(2, at = seq(1.5,24,by = 2),
       labels = c("J", "F", "M", "A", "M", "J", 
                  "J", "A", "S", "O", "N", "D"),
       cex = 1.5)
  points(x = allShadyVD[,i], y = 1:24, type = "l", lwd = 2, col = "gray")
}
dev.off()

png("plots/diff_from_RO_heatmaps_k100_sunny.png", 
    width = 800*5, 
    height = 1200*5, res = 72*5)
par(mfrow = c(4,1),
    mar = c(3,3,1,3),
    cex.axis = 1.5)
for(i in 1:4){
  plot(sunnyDiffRasters[[i]],
       asp = 0, 
       col = hcl.colors(35, "Purple-Brown", rev= T)[2:35],
       breaks = seq(-16.5, 16.5, by = 1),
       # xlim = c(0,400),
       yaxt = "n")
  abline(h = seq(2.5,24.5, by = 2))
  axis(2, at = seq(1.5,24,by = 2),
       labels = c("J", "F", "M", "A", "M", "J", 
                  "J", "A", "S", "O", "N", "D"))
  points(x = allSunnyVD[,i], y = 1:24, type = "l", lwd = 2, col = "gray")
}
dev.off()

png("plots/diff_from_RO_heatmaps_k400_sunny.png", 
    width = 800*5, 
    height = 1200*5, res = 72*5)
par(mfrow = c(4,1),
    mar = c(3,3,1,3),
    cex.axis = 1.5)
for(i in 5:8){
  plot(sunnyDiffRasters[[i]],
       asp = 0, 
       # col = c(hcl.colors(10, "Oranges", rev = F)[1:9], "white", hcl.colors(8, "Purples", rev = T)[2:8]),
       col = hcl.colors(35, "Purple-Brown", rev= T)[2:35],
       breaks = seq(-16.5, 16.5, by = 1),
       # xlim = c(0,400),
       yaxt = "n")
  abline(h = seq(2.5,24.5, by = 2))
  axis(2, at = seq(1.5,24,by = 2),
       labels = c("J", "F", "M", "A", "M", "J", 
                  "J", "A", "S", "O", "N", "D"))
  points(x = allSunnyVD[,i], y = 1:24, type = "l", lwd = 2, col = "gray")
}
dev.off()


#### MAKE AN ACCURATE LEGEND ####
fullrange <- hcl.colors(35, "Purple-Brown", rev= T)[2:35]

png("plots/diff_RO_legend.png",
    height = 500*5,
    width = 600*5,
    res = 72*5)
par(mar= c(1,1,1,1),
    ljoin = 1,
    lend = 2)
plot(rep(5, times = 27), 
     seq(-15.5, 10.5, by = 1),
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     type = "n")
rect(xleft = rep(5, times = 27),
     ybottom = seq(10.5, -15.5, by = -1)[1:27],
     xright = rep(5.25, times = 27),
     ytop = seq(10.5, -15.5, by = -1)[2:28],
     col = fullrange[2:28],
     border = NA)
segments(x0 = rep(5.25, times = 27),
         y0 = seq(-15.5, 10.5, by = 1),
         x1 = rep(5.35, times = 27),
         lwd = 2)
rect(xleft = c(5),
     ybottom = -15.5,
     xright = 5.25,
     ytop = 10.5,
     lwd = 2)
text(x = rep(5.35, times = 28),
     y = seq(-15.5, 10.5, by = 2),
     labels = seq(-10.5, 15.5, by = 2),
     pos = 4,
     cex = 1.8)
dev.off()



