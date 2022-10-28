


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
tempList_shady_100 <- list(k100_0.5_shady, k100_1.0_shady, k100_2.0_shady, k100_3.0_shady)
tempList_sunny_100 <- list(k100_0.5_sunny, k100_1.0_sunny, k100_2.0_sunny, k100_3.0_sunny)
tempList_riverOnly_100 <- list(k100_0.5_riveronly, k100_1.0_riveronly, k100_2.0_riveronly, k100_3.0_riveronly)

tempList_shady_400 <- list(k400_0.5_shady, k400_1.0_shady, k400_2.0_shady, k400_3.0_shady)
tempList_sunny_400 <- list(k400_0.5_sunny, k400_1.0_sunny, k400_2.0_sunny, k400_3.0_sunny)
tempList_riverOnly_400 <- list(k400_0.5_riveronly, k400_1.0_riveronly, k400_2.0_riveronly, k400_3.0_riveronly)


localbox <- "C:/Users/skati/OneDrive - Montana State University/BoxMigratedData/"
outtimes <- read.table(paste0(localbox, "HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt"))
days <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))


## Model inputs
inputs_directory <- "C:/Users/skati/OneDrive - Montana State University/BoxMigratedData/Floodplain_Shade_inputDocs/Boundary_Conditions/new_bcs_dec_3_2021"
riverin <- read.table(paste0(inputs_directory, "/river.txt"),
                      skip = 1)
shadyin <- read.table(paste0(inputs_directory, "/shady.txt"),
                      skip = 1)
sunnyin <- read.table(paste0(inputs_directory, "/sunny.txt"),
                      skip = 1)

riverinsum <- c(min(riverin[,3]),
                mean(riverin[,3]),
                max(riverin[,3]))

shadyinsum <- c(min(shadyin[,3]),
                mean(shadyin[,3]),
                max(shadyin[,3]))

sunnyinsum <- c(min(sunnyin[,3]),
                mean(sunnyin[,3]),
                max(sunnyin[,3]))

par(originalpar)
par(mfcol = c(4,2),
    mar = c(2,2,1,1))

plotOneFP <- function(shadymodel, sunnymodel, x_idx = 362){
  plot(days, 
       colMeans(shadymodel[aquifer_z_idx,x_idx,]),
       type = "l",
       ylim = c(5,20))
  abline(h = mean(riverin[,3]), col = "blue")
  abline(h = mean(shadyin[,3]))
  abline(h = mean(sunnyin[,3]), lty = 2)
  for(i in 1:24){
    points(days[i],
           mean(shadymodel[aquifer_z_idx,x_idx,i]),
           col = hcl.colors(24, "Fall")[i],
           pch = 19)
    points(days[i],
           mean(shadymodel[aquifer_z_idx,x_idx,i]),
           col = "black",
           pch = 1)
  }
  lines(days, 
        colMeans(sunnymodel[aquifer_z_idx,x_idx,]),
        type = "l",
        col = "black",
        lwd = 1,
        lty = 2)
  for(i in 1:24){
    points(days[i],
           mean(sunnymodel[aquifer_z_idx,x_idx,i]),
           bg = hcl.colors(24, "Fall")[i],
           pch = 23,
           col = "black")
  }
  for(i in 1:24){
   if(i == 1){
     legend("topleft", c("shady", "sunny"), lty = c(1,2), pch = c(21,23), 
            col = "black", pt.bg = "gray")
   } 
  }
  
  
  
}

png("plots/flowpath_1950_sunny_and_shady.png",
    height = 1000*5,
    width = 800*5,
    res = 72*5)
par(mfcol = c(4,2),
    mar = c(2,2,1,1),
    cex.axis = 1,
    cex = 1.3)
mapply(plotOneFP,
       tempList_shady_100,
       tempList_sunny_100,
       MoreArgs = list(x_idx = 407))
mapply(plotOneFP,
       tempList_shady_400,
       tempList_sunny_400,
       MoreArgs = list(x_idx = 407))
dev.off()


plotOneFP_meanrange <- function(shadymodel, sunnymodel, x_idx = 362){
  shadysum <- c(min(colMeans(shadymodel[aquifer_z_idx,x_idx,])), mean(colMeans(shadymodel[aquifer_z_idx,x_idx,])), max(colMeans(shadymodel[aquifer_z_idx,x_idx,])))
  sunnysum <- c(min(colMeans(sunnymodel[aquifer_z_idx,x_idx,])), mean(colMeans(sunnymodel[aquifer_z_idx,x_idx,])), max(colMeans(sunnymodel[aquifer_z_idx,x_idx,])))
  
  plot(c(1,2,3,4,5),
       c(riverinsum, shadyinsum, sunnyinsum, shadysum, sunnysum)[seq(1, 15, by = 3)],
       xaxt = "n",
       ylab = "Temperature",
       xlab = "",
       pch = 25,
       bg = "black",
       ylim = c(-1,30))
  points(c(1,2,3,4,5),
         c(riverinsum, shadyinsum, sunnyinsum, shadysum, sunnysum)[seq(1, 15, by = 3)+1],
         pch = 23)
  points(c(1,2,3,4,5),
         c(riverinsum, shadyinsum, sunnyinsum, shadysum, sunnysum)[seq(1, 15, by = 3)+2],
         pch = 24,
         bg = "black")
  # axis(1, at = 1:5, labels = c("river input", "shady input", "sunny input", "shady hz temp", "sunny hz temp"),
  #      las = 2)
}

plotOneFP_meanrange(k100_0.5_shady,
                    k100_0.5_sunny,
                    x_idx = 407)
par(mfcol = c(4,1),
    mar = c(2,2,1,1))
mapply(plotOneFP_meanrange,
       tempList_shady_100,
       tempList_sunny_100,
       MoreArgs = list(x_idx = 407))
mapply(plotOneFP_meanrange,
       tempList_shady_400,
       tempList_sunny_400,
       MoreArgs = list(x_idx = 407))
 
mean(colMeans(tempList_shady_100[[1]][aquifer_z_idx,407,]))
mean(colMeans(tempList_shady_100[[2]][aquifer_z_idx,407,]))
mean(colMeans(tempList_shady_100[[3]][aquifer_z_idx,407,]))
mean(colMeans(tempList_shady_100[[4]][aquifer_z_idx,407,]))

mean(colMeans(tempList_sunny_100[[1]][aquifer_z_idx,407,]))
mean(colMeans(tempList_sunny_100[[2]][aquifer_z_idx,407,]))
mean(colMeans(tempList_sunny_100[[3]][aquifer_z_idx,407,]))
mean(colMeans(tempList_sunny_100[[4]][aquifer_z_idx,407,]))


mean(colMeans(tempList_shady_400[[1]][aquifer_z_idx,407,]))
mean(colMeans(tempList_shady_400[[2]][aquifer_z_idx,407,]))
mean(colMeans(tempList_shady_400[[3]][aquifer_z_idx,407,]))
mean(colMeans(tempList_shady_400[[4]][aquifer_z_idx,407,]))

mean(colMeans(tempList_sunny_400[[1]][aquifer_z_idx,407,]))
mean(colMeans(tempList_sunny_400[[2]][aquifer_z_idx,407,]))
mean(colMeans(tempList_sunny_400[[3]][aquifer_z_idx,407,]))
mean(colMeans(tempList_sunny_400[[4]][aquifer_z_idx,407,]))
