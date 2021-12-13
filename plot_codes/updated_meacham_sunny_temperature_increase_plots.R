##
## Compare sunny and shady HZ temperatures 
##

  
# library(devtools)
# install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(lubridate)

####------ READ IN ALL DATA ------####
box_directory <- "C:/Users/skati/Box/Floodplain_Shade_Box/meacham_updated_results/"

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

localbox <- "C:/Users/t24x137/Box/"
outtimes <- read.table(paste0(localbox, "HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt"))
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))


#-------------------------------------------------------------

model_100_1.0_xvals <- k100_1.0_sunny_s[,2,1,"X"]
model_400_1.0_xvals <- k400_1.0_sunny_s[,2,1,"X"]
model_1.0_zvals <- k100_1.0_shady_s[1,2,,"Z"]
aquifer_z_idx <- 17:47


# flow path lengths: 1, 2, 3, 4, 10.9, 50.9, 100.9, 200.9, 300.9, 600.9, 901
fpInterest <- c(11,21,31,41,50, 70, 95, 145, 195, 345, 413)

par(mfrow = c(1,1))
for(i in 1:length(fpInterest)){
  plot(colMeans(k100_1.0_shady[aquifer_z_idx,fpInterest[i],]), 
       type = "l", 
       col = "black", 
       ylim = c(2,22),
       main = paste0(round(model_100_1.0_xvals[fpInterest[i]], 1), " meters"),
       ylab = "Temperature",
       xlab = "Day",
       xaxt = "n")
  lines(colMeans(k100_1.0_sunny[aquifer_z_idx,fpInterest[i],]), col = "orange")
  legend("topleft", c("shady", "sunny"), col = c("black", "orange"), lty = 1)
  axis(1, at = seq(1,24,2), labels = paste0(months(days[seq(1,24,2)], abbreviate = T), " ",mday(days[seq(1,24,2)])))
  text(10, 21, 
       paste0("max temperature difference: ", round(max(colMeans(k100_1.0_sunny[aquifer_z_idx,fpInterest[i],]) - colMeans(k100_1.0_shady[aquifer_z_idx,fpInterest[i],])), 2)
       ),
       pos = 4)
}


##########***************************************###################
##########***************************************###################

kNames2 <- rep(c("100", "400"), each = 4)
modelNames <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)
modelNames2 <- modelNames[1:8]
listofSunny <- list(k100_0.5_sunny, k100_1.0_sunny, k100_2.0_sunny, k100_3.0_sunny,
                    k400_0.5_sunny, k400_1.0_sunny, k400_2.0_sunny, k400_3.0_sunny)
listofShady <- list(k100_0.5_shady, k100_1.0_shady, k100_2.0_shady, k100_3.0_shady,
                    k400_0.5_shady, k400_1.0_shady, k400_2.0_shady, k400_3.0_shady)



## Temperature differences across all flow paths ##
tempdiffsplot <- function(sunnyModel, shadyModel, k, depth2HZ, yl = c(0,2.65), colorpal = hcl.colors(5), ...){
  if(k == "100"){
    structure2use <- model_100_1.0_xvals
    textAtX <- 380
  }else{
    structure2use <- model_400_1.0_xvals
    textAtX <- 380
  }
  reverseindex <- seq(length(sunnyModel[,1,1]), 1, by = -1)
  
  plot(colMeans(sunnyModel[aquifer_z_idx,,1]) - colMeans(shadyModel[aquifer_z_idx,,1]) ~ structure2use, 
       type = "n",
       main = paste0(k, ", ", depth2HZ),
       ylim = yl,
       xlim = c(0, 2000),
       xlab = "Flow Path Length (m)",
       ylab = "Temperature Difference")
  mapply(function(t, c) lines(colMeans(sunnyModel[reverseindex[aquifer_z_idx],,t]) - colMeans(shadyModel[reverseindex[aquifer_z_idx],,t]) ~ structure2use, 
                              col = c,
                              ...),
         c(1,3,5,7,9,11,13,15,17,19,21,23),
         colorpal)
  abline(h =0, col = "black", lty = 2, lwd = 2)
  # mapply(function(t, c, xadj) text(structure2use[textAtX] + xadj, 
  #                                  (mean(sunnyModel[aquifer_z_idx,textAtX,t]) - mean(shadyModel[aquifer_z_idx,textAtX,t])), 
  #                                  paste0(months(days[t], abbreviate = T), "-", day(days[t])),
  #                                  col = c,
  #                                  pos = 3,
  #                                  cex = 2),
  #        seq(2,24,5),
  #        colorpal,
  #        c(-690, -460, -230, 0, 230))
  
  #text(-30,2.6, paste0("K = ", k,"m/day, depth2HZ = ", depth2HZ, "m"), pos = 4, cex = 2)
}

savetempdiffplots <- function(k = "100", d2hz = "0.5", sunnymodel, shadymodel, yl = c(0,2.65), colorpal = hcl.colors(5),...){
  
  png(paste0(getwd(),"/plots/sunnyTempIncrease_k", k, "_", d2hz, ".png"),
      height = 600,
      width = 800)
  par(cex.lab = 1.8,
      cex.main = 2,
      cex.axis = 1.5,
      mar = c(5,5,5,1),
      bg = "white",
      fg = "black",
      col.axis = "black",
      col.main = "black",
      col.lab = "black")
  tempdiffsplot(sunnymodel, shadymodel, k, d2hz, yl = yl, colorpal = colorpal, ...)
  dev.off()
}


mapply(savetempdiffplots,
       kNames2,
       modelNames2,
       listofSunny,
       listofShady,
       MoreArgs = list(lwd = 4, yl = c(-3,8), colorpal = hcl.colors(12, "Fall")))


### All on one ###

png(paste0(getwd(),"/plots/all_sunny_temp_increases.png"),
    height = 600*5,
    width = 1500*5,
    res = 72*5)
par(cex.lab = 1.8,
    cex.main = 2,
    cex.axis = 1.5,
    mar = c(2,2,3,1),
    mfrow = c(2,4))
mapply(tempdiffsplot,
       listofSunny,
       listofShady,
       kNames2,
       modelNames2,
       MoreArgs = list(lwd = 4, yl = c(-3,8), colorpal = hcl.colors(12, "Fall")))
dev.off()


