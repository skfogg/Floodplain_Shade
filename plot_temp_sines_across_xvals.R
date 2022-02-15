##
## Plot Annual temperature sine waves across x-vals (flow path length) for each model
##

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

### ---- Time Data --- ###
outtimes <- read.table("C:/Users/skati/Box/HGSwork/outputTimes_inputTemps/OutputTimesYr7_fulldays.txt")
lubridays <- ymd_hms("2021-01-01 00:00:00") + (outtimes$V1[seq(1,by = 23, length.out = 24)]-(365*86400*7))
jd <- c(1,16,31,46,61,76,91,106,121,136,151,166,181,196,211,226,
        241,256,271,286,301,316,331,346)

# x-values (should be the same for all models)
x_vals <- k400_1.0_riveronly_s[,1,1,"X"]


mean_matrix <- function(x){
  hz_mean <- matrix(data = 0, nrow = 24, ncol = 413)
  for(i in 1:413){
    hz_mean[,i] <- round(colMeans(x[1:40, i, ]), 4)
  }
  return(hz_mean)
}


hz_mean_df <- data.frame(x = rep(x_vals[1], times = 24), y = jd, z = colMeans(x[1:40, 1, ]))
mean_df <- function(x){
  hz_mean_df <- data.frame(x = rep(x_vals[1], times = 24), y = jd, z = colMeans(x[1:40, 1, ]))
  for (i in 2:413){
    rbind(hz_mean_df, data.frame(x = rep(x_vals[i], times = 24), y = jd, z = colMeans(x[1:40, i, ])))
  }
  return(hz_mean_df)
}

k100_0.5_shady_mean <- mean_matrix(k100_0.5_shady)
k100_1.0_shady_mean <- mean_matrix(k100_1.0_shady)
k100_2.0_shady_mean <- mean_matrix(k100_2.0_shady)
k100_3.0_shady_mean <- mean_matrix(k100_3.0_shady)

k100_0.5_sunny_mean <- mean_matrix(k100_0.5_sunny)
k100_1.0_sunny_mean <- mean_matrix(k100_1.0_sunny)
k100_2.0_sunny_mean <- mean_matrix(k100_2.0_sunny)
k100_3.0_sunny_mean <- mean_matrix(k100_3.0_sunny)

k400_0.5_shady_mean <- mean_matrix(k400_0.5_shady)
k400_1.0_shady_mean <- mean_matrix(k400_1.0_shady)
k400_2.0_shady_mean <- mean_matrix(k400_2.0_shady)
k400_3.0_shady_mean <- mean_matrix(k400_3.0_shady)

k400_0.5_sunny_mean <- mean_matrix(k400_0.5_sunny)
k400_1.0_sunny_mean <- mean_matrix(k400_1.0_sunny)
k400_2.0_sunny_mean <- mean_matrix(k400_2.0_sunny)
k400_3.0_sunny_mean <- mean_matrix(k400_3.0_sunny)



par(mfcol = c(4,2), mar = c(2,2,1,1))
lots <- hcl.colors(413)
yl <- c(2,22)

## K100
plot(k100_0.5_shady_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k100_0.5_shady_mean[,i], col = lots[i])
}
plot(k100_1.0_shady_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k100_1.0_shady_mean[,i], col = lots[i])
}
plot(k100_2.0_shady_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k100_2.0_shady_mean[,i], col = lots[i])
}
plot(k100_3.0_shady_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k100_3.0_shady_mean[,i], col = lots[i])
}

plot(k100_0.5_sunny_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k100_0.5_sunny_mean[,i], col = lots[i])
}
plot(k100_1.0_sunny_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k100_1.0_sunny_mean[,i], col = lots[i])
}
plot(k100_2.0_sunny_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k100_2.0_sunny_mean[,i], col = lots[i])
}
plot(k100_3.0_sunny_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k100_3.0_sunny_mean[,i], col = lots[i])
}


## K400
plot(k400_0.5_shady_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k400_0.5_shady_mean[,i], col = lots[i])
}
plot(k400_1.0_shady_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k400_1.0_shady_mean[,i], col = lots[i])
}
plot(k400_2.0_shady_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k400_2.0_shady_mean[,i], col = lots[i])
}
plot(k400_3.0_shady_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k400_3.0_shady_mean[,i], col = lots[i])
}

plot(k400_0.5_sunny_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k400_0.5_sunny_mean[,i], col = lots[i])
}
plot(k400_1.0_sunny_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k400_1.0_sunny_mean[,i], col = lots[i])
}
plot(k400_2.0_sunny_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k400_2.0_sunny_mean[,i], col = lots[i])
}
plot(k400_3.0_sunny_mean[,1], type = "l", ylim = yl)
for(i in 2:413){
  lines(k400_3.0_sunny_mean[,i], col = lots[i])
}




longtest <- data.frame(x = rep(x_vals, each = 24),
                       y = rep(jd, times = 413),
                       z = k100_3.0_shady_mean[,1])

library(scatterplot3d)
library(rgl)
par(mfrow = c(1,1))
plot3d(x = rep(x_vals[1], times = 24), y = jd, z = k100_3.0_shady_mean[,1], 
       zlim = c(2,18), xlim = c(0,2000), type = "l", col = lots[1],
       phi = 40, theta = 20)
for(i in seq(3,413, by = 2)){
  lines3d(x = rep(x_vals[i], times = 24), y = jd, z = k100_3.0_shady_mean[,i], 
          zlim = c(2,18), xlim = c(0,2000), type = "l", col = lots[i],
          add= T)
}



