##
## Make Plots for commitee meeting 
##


library(devtools)
install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(lubridate)

####------ READ IN ALL DATA ------####
localbox <- "C:/Users/skati/Box/"

kFolders <- rep(c("K_100m_day", "K_400m_day"), each = 12)
bcFolders <- rep(rep(c("sunny", "shady", "riveronly"), each = 4), times = 2)
modelFolders <- rep(c("soil_0.5m", "soil_1.0m", "soil_2.0m", "soil_3.0m"), times = 6)

kNames <- rep(c("100", "400"), each = 12)
modelNames <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)

for(i in 1:24){
assign(paste0(bcFolders[i], "_", kNames[i], "_", modelNames[i]),
       readRDS(paste0(localbox, "Floodplain_Shade_Box/", kFolders[i], "/", bcFolders[i], "/", modelFolders[i], "/", modelFolders[i], "_dailymeans.RData")))
}

for(i in 1:24){
  assign(paste0(bcFolders[i], "_", kNames[i], "_", modelNames[i], "_str"),
         readRDS(paste0(localbox, "Floodplain_Shade_Box/", kFolders[i], "/", bcFolders[i], "/", modelFolders[i], "/", modelFolders[i], "_modelstructure.RData")))
}

outtimes <- read.table(paste0(localbox, "HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt"))
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))


#-------------------------------------------------------------

dim(shady_100_1.0)
dim(shady_100_1.0_str)

model_100_1.0_xvals <- shady_100_1.0_str[,2,1,"X"]
model_400_1.0_xvals <- shady_400_1.0_str[,2,1,"X"]
model_1.0_zvals <- shady_100_1.0_str[1,2,,"Z"]
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


## Temperature differences across all flow paths ##
tempdiffsplot <- function(sunnyModel, shadyModel, k, depth2HZ, ...){
  if(k == "100"){
    structure2use <- model_100_1.0_xvals
    textAtX <- 450
    }else{
      structure2use <- model_400_1.0_xvals
      textAtX <- 900
    }
    
    plot(colMeans(sunnyModel[aquifer_z_idx,,1]) - colMeans(shadyModel[aquifer_z_idx,,1]) ~ structure2use, 
         type = "n",
         main = "Increase in HZ Temps under Sunny Floodplain",
         ylim = c(0,2.65),
         xlim = c(0, 2000),
         xlab = "Flow Path Length (m)",
         ylab = "Temperature Difference")
    mapply(function(t, c) lines(colMeans(sunnyModel[aquifer_z_idx,,t]) - colMeans(shadyModel[aquifer_z_idx,,t]) ~ structure2use, 
                                col = c,
                                ...),
           seq(1,24,5),
           hcl.colors(5))
    mapply(function(t, c) text(structure2use[textAtX], 
                               (mean(sunnyModel[aquifer_z_idx,textAtX,t]) - mean(shadyModel[aquifer_z_idx,textAtX,t])), 
                               months(days[t], abbreviate = T),
                               col = c,
                               pos = 3,
                               cex = 1.3),
           seq(1,24,5),
           hcl.colors(5))
  
text(-30,2.6, paste0("K = ", k,"m/day, depth2HZ = ", depth2HZ, "m"), pos = 4, cex = 1.5)
}

savetempdiffplots <- function(k = "100", d2hz = "0.5", sunnymodel, shadymodel, ...){
  
  png(paste0(getwd(),"/plots/sunnyTempIncrease_k", k, "_", d2hz, ".png"),
      height = 600,
      width = 800)
  par(cex.lab = 1.5,
      cex.main = 1.5,
      cex.axis = 1.2)
  tempdiffsplot(sunnymodel, shadymodel, k, d2hz, ...)
  dev.off()
}

kNames2 <- rep(c("100", "400"), each = 4)
modelNames2 <- modelNames[1:8]
listofSunny <- list(sunny_100_0.5, sunny_100_1.0, sunny_100_2.0, sunny_100_3.0,
                    sunny_400_0.5, sunny_400_1.0, sunny_400_2.0, sunny_400_3.0)
listofShady <- list(shady_100_0.5, shady_100_1.0, shady_100_2.0, shady_100_3.0,
                    shady_400_0.5, shady_400_1.0, shady_400_2.0, shady_400_3.0)

mapply(savetempdiffplots,
       kNames2,
       modelNames2,
       listofSunny,
       listofShady,
       MoreArgs = list(lwd = 2))

## Sando boxo ##
savetempdiffplots("100", "0.5", sunny_100_0.5, shady_100_0.5, lwd = 2)

tempdiffsplot(sunny_100_1.0, shady_100_1.0, "100", "1.0", lwd = 3)
tempdiffsplot(sunny_100_2.0, shady_100_2.0, "100", "2.0")
tempdiffsplot(sunny_100_3.0, shady_100_3.0, "100", "3.0")

tempdiffsplot(sunny_400_0.5, shady_400_0.5, "400", "0.5")
tempdiffsplot(sunny_400_1.0, shady_400_1.0, "400", "1.0")
tempdiffsplot(sunny_400_2.0, shady_400_2.0, "400", "2.0")
tempdiffsplot(sunny_400_3.0, shady_400_3.0, "400", "3.0")



## cat crap box ##
par(mfrow = c(2,1))
plot(colMeans(shady_100_1.0[,fpInterest[1],]), type = "l", col = "brown",
     ylim = c(2,22),
     ylab = "Temperature")
mapply(function(f, c) lines(colMeans(shady_100_1.0[,f,]), col = c),
       fpInterest,
       hcl.colors(length(fpInterest)))
plot(sunny_100_1.0[32,fpInterest[1],], type = "l", col = "brown",
     ylim = c(2,22),
     ylab = "Temperature")
mapply(function(f, c) lines(sunny_100_1.0[32,f,], col = c),
       fpInterest,
       hcl.colors(length(fpInterest)))

par(mfrow = c(1,1))
plot(colMeans(shady_100_1.0[,41,]), type = "l")

