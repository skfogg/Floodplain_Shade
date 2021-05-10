##
## Compare sunny and shady HZ temperatures 
##


# library(devtools)
# install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(lubridate)

####------ READ IN ALL DATA ------####
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

kvals_ro <- rep(c("100", "400"), each = 4)
ro <- rep("riveronly", times = 8)
eachfolder_ro <- numeric(8)
for (i in 1:8){
  eachfolder_ro[i] <- paste0(riveronly_directory, "K_", kvals_ro[i], "m_day/", ro[i], "/", "soil_", soilvals[i], "m")
}
eachmodel_ro <- numeric(8)
for (i in 1:8){
  eachmodel_ro[i] <- paste0("k", kvals_ro[i], "_", soilvals[i], "_", ro[i])
}

eachmodel <- c(eachmodel1, eachmodel_ro)
eachfolder <- c(eachfolder1, eachfolder_ro)

for (i in 1:24){
  assign(eachmodel[i], readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_modelstructure.RData")))
  # assign(paste0(eachmodel[i], "_f100"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_first100m.RData")))
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
fpInterest <- c(11,21,31,41,50, 70, 95, 145, 195, 345, 471)

for(i in 1:length(fpInterest)){
  plot(colMeans(shady_100_1.0[aquifer_z_idx,fpInterest[i],]), 
       type = "l", 
       col = "black", 
       ylim = c(2,22),
       main = paste0(round(model_1.0_xvals[fpInterest[i]], 1), " meters"),
       ylab = "Temperature",
       xlab = "Day",
       xaxt = "n")
  lines(colMeans(sunny_100_1.0[aquifer_z_idx,fpInterest[i],]), col = "orange")
  legend("topleft", c("shady", "sunny"), col = c("black", "orange"), lty = 1)
  axis(1, at = seq(1,24,2), labels = paste0(months(days[seq(1,24,2)], abbreviate = T), " ",mday(days[seq(1,24,2)])))
  text(10, 21, 
       paste0("max temperature difference: ", round(max(colMeans(sunny_100_1.0[aquifer_z_idx,fpInterest[i],]) - colMeans(shady_100_1.0[aquifer_z_idx,fpInterest[i],])), 2)
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
       main = "Increase in HZ Temps under Sunny Floodplain",
       ylim = yl,
       xlim = c(0, 2000),
       xlab = "Flow Path Length (m)",
       ylab = "Temperature Difference")
  mapply(function(t, c) lines(colMeans(sunnyModel[reverseindex[aquifer_z_idx],,t]) - colMeans(shadyModel[reverseindex[aquifer_z_idx],,t]) ~ structure2use, 
                              col = c,
                              ...),
         seq(2,24,5),
         colorpal)
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
      bg = "black",
      fg = "ghostwhite",
      col.axis = "ghostwhite",
      col.main = "ghostwhite",
      col.lab = "ghostwhite")
  tempdiffsplot(sunnymodel, shadymodel, k, d2hz, yl = yl, colorpal = colorpal, ...)
  dev.off()
}


mapply(savetempdiffplots,
       kNames2,
       modelNames2,
       listofSunny,
       listofShady,
       MoreArgs = list(lwd = 3, yl = c(-1,8), colorpal = hcl.colors(5, "RdYlGn")))
