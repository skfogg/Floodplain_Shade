##
## POINT OF Vertical heat exchange effects
##

library(lubridate)

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
  assign(paste0(eachmodel[i], "_100m"), readRDS(paste0(eachfolder[i], "/", eachmodel[i], "_first100m.RData")))
}

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


dielList_shady_100 <- list(k100_0.5_shady_100m, k100_1.0_shady_100m, k100_2.0_shady_100m, k100_3.0_shady_100m)
dielList_sunny_100 <- list(k100_0.5_sunny_100m, k100_1.0_sunny_100m, k100_2.0_sunny_100m, k100_3.0_sunny_100m)
dielList_riveronly_100 <- list(k100_0.5_riveronly_100m, k100_1.0_riveronly_100m, k100_2.0_riveronly_100m, k100_3.0_riveronly_100m)

dielList_shady_400 <- list(k400_0.5_shady_100m, k400_1.0_shady_100m, k400_2.0_shady_100m, k400_3.0_shady_100m)
dielList_sunny_400 <- list(k400_0.5_sunny_100m, k400_1.0_sunny_100m, k400_2.0_sunny_100m, k400_3.0_sunny_100m)
dielList_riveronly_400 <- list(k400_0.5_riveronly_100m, k400_1.0_riveronly_100m, k400_2.0_riveronly_100m, k400_3.0_riveronly_100m)

# 
# ro <- k400_1.0_riveronly_100m
# ro_dm <- k400_1.0_riveronly
# ro_s <- k400_1.0_riveronly_s
# shad <- k400_1.0_shady_100m
# shad_dm <- k400_1.0_shady
# shad_s <- k400_1.0_shady_s
# sun <- k400_1.0_sunny_100m
# sun_dm <- k400_1.0_sunny
# sun_s <- k400_1.0_sunny_s
# ti <- 530
# 
# plot(ro[,1,1,ti,"X"], rowMeans(ro[,1,18:47,ti,"temp"]),
#      type = "l", ylim = c(0,22))
# lines(shad[,1,1,ti,"X"], rowMeans(shad[,1,18:47,ti,"temp"]),
#       col = "blue")
# lines(sun[,1,1,ti,"X"], rowMeans(sun[,1,18:47,ti,"temp"]),
#       col = "orange")
# 
# plot(ro[,1,1,ti,"X"], rowMeans(ro[,1,18:47,ti,"temp"]),
#      type = "l", ylim = c(0,22))
# plot(ro_s[,1,1,"X"], colMeans(ro_dm[11:41, ,24]),
#      type = "l",
#      xlim = c(0,150), ylim = c(0,22))
# lines(shad_s[,1,1,"X"], colMeans(shad_dm[11:41, ,24]),
#       col = "blue")
# lines(sun_s[,1,1,"X"], colMeans(sun_dm[11:41, ,24]),
#       col = "orange")
# 
# 
# shaddiff <- rowMeans(ro[,1,18:47,ti,"temp"]) - rowMeans(shad[,1,18:47,ti,"temp"])
# sundiff <- rowMeans(ro[,1,18:47,ti,"temp"]) - rowMeans(sun[,1,18:47,ti,"temp"])
# 
# 
# shdiff_0.5 <- names(shaddiff[shaddiff < 0.53 & shaddiff > 0.47])
# shdiff_1.0 <- names(shaddiff[shaddiff < 1.1 & shaddiff > 0.999])
# 
# sudiff_0.5 <- names(sundiff[sundiff < 0.56 & sundiff > 0.47])
# sudiff_1.0 <- names(sundiff[sundiff < 1.1 & sundiff > 0.999])
# 
# 
# abline(v = shad[as.numeric(shdiff_0.5),1,1,1,"X"],
#        col = "blue")
# abline(v = shad[as.numeric(shdiff_1.0),1,1,1,"X"],
#        col = "blue")
# 
# abline(v = sun[as.numeric(sudiff_0.5),1,1,1,"X"],
#        col = "orange")
# abline(v = sun[as.numeric(sudiff_1.0),1,1,1,"X"],
#        col = "orange")
# 
# t = 530
# x = dielList_shady_100[[4]]
# ro = ro
# threshold = 0.5
vert_damp_loc <- function(x, ro, t, threshold = 0.5){
  diffs <- rowMeans(ro[,1,18:47,t,"temp"]) - rowMeans(x[,1,18:47,t,"temp"])
  diff_loc <- names(diffs[abs(diffs) < (threshold + 0.06) & abs(diffs) > (threshold - 0.06)])
  if(length(diff_loc) > 1){
    ## if the threshold returns more than 1 diff location, find the min and return that one:
    #diff_loc <- names(diffs[diff_loc][abs(diffs[diff_loc] - threshold) == min(abs(diffs[diff_loc] - threshold))])
    ## RETURN THE MIN INSTEAD:
    diff_loc <- min(as.numeric(diff_loc))
    }
  if(length(diff_loc) == 0){
    diff_loc <- NA
  }
  return(as.numeric(diff_loc))
}

vert_damp_loc_dm <- function(x_dm, hz_idx, ro_dm, t, threshold = 0.5){
  diffs <- colMeans(ro_dm[hz_idx,,t]) - colMeans(x_dm[hz_idx,,t])
  names(diffs) <- 1:length(diffs)
  diff_loc <- names(diffs[abs(diffs) < (threshold + 0.06) & abs(diffs) > (threshold - 0.06)])
  if(length(diff_loc) > 1){
    diff_loc <- min(as.numeric(diff_loc))
  }
  if(length(diff_loc) == 0){
    diff_loc <- NA
  }
  return(as.numeric(diff_loc))
}

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

vert_damp <- function(x, ro, threshold = 0.5){
  locs <- mapply(vert_damp_loc,
                 t = 1:576,
                 MoreArgs = list(x, ro, threshold))
  loc_matrix <- matrix(unlist(locs), nrow = 24, ncol = 24)
  locations <- apply(loc_matrix, 2, getmode)
  return_matrix <- x[locations,1,1,1,"X"]
  names(return_matrix) <- c(1:24)
  return(return_matrix)
}


shady_vd_100 <- mapply(vert_damp,
                   x = dielList_shady_100,
                   ro = dielList_riveronly_100)
colnames(shady_vd_100) <- c(0.5, 1.0, 2.0, 3.0)
shady_vd_400 <- mapply(vert_damp,
                   x = dielList_shady_400,
                   ro = dielList_riveronly_400)
colnames(shady_vd_400) <- c(0.5, 1.0, 2.0, 3.0)
sunny_vd_100 <- mapply(vert_damp,
                       x = dielList_sunny_100,
                       ro = dielList_riveronly_100)
colnames(sunny_vd_100) <- c(0.5, 1.0, 2.0, 3.0)
sunny_vd_400 <- mapply(vert_damp,
                       x = dielList_sunny_400,
                       ro = dielList_riveronly_400)
colnames(sunny_vd_400) <- c(0.5, 1.0, 2.0, 3.0)

shady_vd_100
shady_vd_400
sunny_vd_100
sunny_vd_400

#### FILL IN SHADY 400 ####
which_thickness <- floor(which(is.na(shady_vd_400))/24)
which_days <- (which(is.na(shady_vd_400))/24 - which_thickness)*24
  
longer <- data.frame(which_thickness, which_days, which_model = NA, which_ro_model = NA, vert_damp_loc = NA)

for(i in 1:nrow(longer)){
  if(longer[i,"which_thickness"] == 4){
    longer$which_model[i] <- "k400_3.0_shady"
    longer$which_days[i] <- 24
    longer$which_ro_model[i] <- "k400_3.0_riveronly"
    longer$which_thickness[i] <- 3
  }else{
    longer$which_model[i] <- paste0("k400_", longer[i,"which_thickness"], ".0_shady")
    longer$which_ro_model[i] <- paste0("k400_", longer[i,"which_thickness"], ".0_riveronly")
  }
  if(longer[i,"which_thickness"] == 0){
    longer$which_model[i] <- paste0("k400_", longer[i,"which_thickness"], ".5_shady")
    longer$which_ro_model[i] <- paste0("k400_", longer[i,"which_thickness"], ".5_riveronly")
  }
}

for (i in c(1:37, 39:nrow(longer))){
  if(longer[i,"which_thickness"] == 0){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 6:36, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 1){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 11:40, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 2){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 21:50, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 3){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 31:60, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
}
longer[38,]$vert_damp_loc <- vert_damp_loc_dm(x_dm = k400_3.0_shady, hz_idx = 31:60, ro_dm = k400_3.0_riveronly, t = 1)

col_loc <- longer$which_thickness + 1
row_loc <- longer$which_days

for(i in c(1:37, 39:nrow(longer))){
  shady_vd_400[as.character(row_loc[i]), col_loc[i]] <- k400_0.5_shady_s[longer$vert_damp_loc[i],1,1,"X"]
}

shady_vd_400[1,4] <- k400_0.5_shady_s[longer$vert_damp_loc[38],1,1,"X"]
shady_vd_400

#### FILL IN SHADY 100 ####
which_thickness <- floor(which(is.na(shady_vd_100))/24)
which_days <- (which(is.na(shady_vd_100))/24 - which_thickness)*24

longer <- data.frame(which_thickness, which_days, which_model = NA, which_ro_model = NA, vert_damp_loc = NA)

for(i in 1:nrow(longer)){
  if(longer[i,"which_thickness"] == 4){
    longer$which_model[i] <- "k100_3.0_shady"
    longer$which_days[i] <- 24
    longer$which_ro_model[i] <- "k100_3.0_riveronly"
    longer$which_thickness[i] <- 3
  }else{
    longer$which_model[i] <- paste0("k100_", longer[i,"which_thickness"], ".0_shady")
    longer$which_ro_model[i] <- paste0("k100_", longer[i,"which_thickness"], ".0_riveronly")
  }
  if(longer[i,"which_thickness"] == 0){
    longer$which_model[i] <- paste0("k100_", longer[i,"which_thickness"], ".5_shady")
    longer$which_ro_model[i] <- paste0("k100_", longer[i,"which_thickness"], ".5_riveronly")
  }
}

for (i in 1:nrow(longer)){
  if(longer[i,"which_thickness"] == 0){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 6:36, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 1){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 11:40, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 2){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 21:50, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 3){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 31:60, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
}

col_loc <- longer$which_thickness + 1
row_loc <- longer$which_days

for(i in 1:nrow(longer)){
  shady_vd_100[as.character(row_loc[i]), col_loc[i]] <- k100_0.5_shady_s[longer$vert_damp_loc[i],1,1,"X"]
}

shady_vd_100



#### FILL IN SUNNY 400 ####
which_thickness <- floor(which(is.na(sunny_vd_400))/24)
which_days <- (which(is.na(sunny_vd_400))/24 - which_thickness)*24

longer <- data.frame(which_thickness, which_days, which_model = NA, which_ro_model = NA, vert_damp_loc = NA)

for(i in 1:nrow(longer)){
  if(longer[i,"which_thickness"] == 4){
    longer$which_model[i] <- "k400_3.0_sunny"
    longer$which_days[i] <- 24
    longer$which_ro_model[i] <- "k400_3.0_riveronly"
    longer$which_thickness[i] <- 3
  }else{
    longer$which_model[i] <- paste0("k400_", longer[i,"which_thickness"], ".0_sunny")
    longer$which_ro_model[i] <- paste0("k400_", longer[i,"which_thickness"], ".0_riveronly")
  }
  if(longer[i,"which_thickness"] == 0){
    longer$which_model[i] <- paste0("k400_", longer[i,"which_thickness"], ".5_sunny")
    longer$which_ro_model[i] <- paste0("k400_", longer[i,"which_thickness"], ".5_riveronly")
  }
}

for (i in c(1:12, 14:nrow(longer))){
  if(longer[i,"which_thickness"] == 0){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 6:36, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 1){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 11:40, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 2){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 21:50, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
  if(longer[i,"which_thickness"] == 3){
    longer[i,]$vert_damp_loc <- vert_damp_loc_dm(get(longer[i,"which_model"]), 31:60, get(longer[i,"which_ro_model"]), longer[i,"which_days"])
  }
}
longer[13,]$vert_damp_loc <- vert_damp_loc_dm(x_dm = k400_3.0_sunny, hz_idx = 31:60, ro_dm = k400_3.0_riveronly, t = 1)

col_loc <- longer$which_thickness + 1
row_loc <- longer$which_days

for(i in c(1:12, 14:nrow(longer))){
  sunny_vd_400[as.character(row_loc[i]), col_loc[i]] <- k400_0.5_sunny_s[longer$vert_damp_loc[i],1,1,"X"]
}

sunny_vd_400[1,4] <- k400_0.5_sunny_s[longer$vert_damp_loc[13],1,1,"X"]
sunny_vd_400




saveRDS(shady_vd_100, "analysis_data/shady_vd_100.RData")
saveRDS(shady_vd_400, "analysis_data/shady_vd_400.RData")
saveRDS(sunny_vd_100, "analysis_data/sunny_vd_100.RData")
saveRDS(sunny_vd_400, "analysis_data/sunny_vd_400.RData")



