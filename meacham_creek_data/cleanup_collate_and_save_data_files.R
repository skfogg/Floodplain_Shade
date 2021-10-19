library(lubridate)
library(zoo)
library(xts)
library(stringr)

atm_col <- c("IDX", "DateTime", "Temp", "Lux")
soil_col <- c("IDX", "DateTime", "Temp")

directory <-  "C:/Users/skati/Documents/Floodplain_Shade/meacham_creek_data/csv_files"
extensions_atm <- c("/shade/atm", "/sun/atm")
extensions_soil <- c("/shade/soil", "/sun/soil")

for(i in 1:length(extensions_atm)){
poop <- list.files(paste0(directory, extensions_atm[i]), full.names = T)
names <- list.files(paste0(directory, extensions_atm[i]), pattern="*.csv")
list2env(
  lapply(setNames(poop, make.names(gsub("*.csv$", "", names))), 
         read.csv,
         skip = 1,
         col.names = atm_col), 
  envir = .GlobalEnv)
}

for(i in 1:length(extensions_soil)){
  poop <- list.files(paste0(directory, extensions_soil[i]), full.names = T)
  names <- list.files(paste0(directory, extensions_soil[i]), pattern="*.csv")
  list2env(
    lapply(setNames(poop, make.names(gsub("*.csv$", "", names))), 
           read.csv,
           skip = 1,
           col.names = soil_col), 
    envir = .GlobalEnv)
}

shade_atm <- rbind(shade_atm_1, shade_atm_2, shade_atm_3, shade_atm_4)
shade_soil <- rbind(shady_soil_1, shady_soil_2, shady_soil_3)
sunny_atm <- rbind(sunny_atm_1, sunny_atm_2, sunny_atm_3, sunny_atm_4)
sunny_soil <- rbind(sunny_soil_1, sunny_soil_2, sunny_soil_3, sunny_soil_4)


sunnyatmx <- xts(zoo(sunny_atm[,3:4], order.by = mdy_hms(sunny_atm$DateTime)))
shadyatmx <- xts(zoo(shade_atm[,3:4], order.by = mdy_hms(shade_atm$DateTime)))
sunnysoilx <- xts(zoo(sunny_soil[,3], order.by = mdy_hms(sunny_soil$DateTime)))
shadysoilx <- xts(zoo(shade_soil[,3], order.by = mdy_hms(shade_soil$DateTime)))

##### DATA CLEANING  #####

## Clean up and save sunny soil data:
plot(sunnysoilx["2020-07-29"])
plot(sunnysoilx["2020-09-21/2020-09-23"])
plot(sunnysoilx["2021-01-20/2021-01-24"])
plot(sunnysoilx["2021-04-22/2021-04-24"])
plot(sunnysoilx["2021-05-27"])
plot(sunnysoilx["2021-08-16/"])

# * Remove [07-29-2020], [09-22-2020], [01-20-2021:01-23-2021], and [04-22-2021] *
remove_dates_sunny_soil <- c("07/29/20", "09/22/20", "01/20/21", "01/22/21", 
                             "01/23/21", "04/22/21", "05/27/21", "08/17/21")
for(i in remove_dates_sunny_soil){
  sunny_soil <- sunny_soil[!complete.cases(str_locate(sunny_soil$DateTime, i)),]
}

plot(sunny_soil$Temp, type = "l")
write.csv(sunny_soil, paste0(directory, "/sunnysoil.csv"), row.names = F)

## Clean up and save shady soil data:
plot(shadysoilx["2020-07-29"])
plot(shadysoilx["2020-09-09"])
plot(shadysoilx["2021-01-21"])
plot(shadysoilx["2021-04-22"])
plot(shadysoilx["2021-05-27/2021-05-31"])
plot(shadysoilx["2021-08-16/"])

# * Remove [07/29/20], [09/09/20], [01/21/21], [04/22/21] * #
remove_dates_shady_soil <- c("07/29/20", "09/09/20", "01/21/21", "04/22/21",
                             "05/27/21", "08/17/21")
for(i in remove_dates_shady_soil){
  shade_soil <- shade_soil[!complete.cases(str_locate(shade_soil$DateTime, i)),]
}
plot(shade_soil$Temp, type = "l")
write.csv(shade_soil, paste0(directory, "/shadysoil.csv"), row.names = F)

## Clean up and save sunny atm data:
plot(sunnyatmx$Temp["2020-07-27"])# KEEP
plot(sunnyatmx$Temp["2020-09-22"])# KEEP
plot(sunnyatmx$Temp["2020-09-21/2020-09-24"]) # KEEP
plot(sunnyatmx$Lux["2020-09-21/2020-09-24"]) # KEEP
plot(sunnyatmx$Temp["2021-01-20"])
plot(sunnyatmx$Temp["2021-01-22"])
plot(sunnyatmx$Temp["2021-04-22"])
plot(sunnyatmx$Temp["2021-05-28"])
plot(sunnyatmx$Temp["2021-08-16"])

remove_dates_sunny_atm <- c( "01/20/21", "01/22/21", "04/22/21", 
                             "05/26/21", "05/27/21", "08/16/21")
for(i in remove_dates_sunny_atm){
  sunny_atm <- sunny_atm[!complete.cases(str_locate(sunny_atm$DateTime, i)),]
}

plot(sunny_atm$Temp, type = "l")
plot(sunny_atm$Lux, type = "l")
write.csv(sunny_atm, paste0(directory, "/sunnyatm.csv"), row.names = F)

## Clean up and save shady atm data:
plot(shadyatmx$Temp["2020-07-24"])
plot(shadyatmx$Temp["2020-09-30"])
plot(shadyatmx$Temp["2021-01-21"])
plot(shadyatmx$Temp["2021-04-22"])
plot(shadyatmx$Temp["2021-05-27"])
plot(shadyatmx$Temp["2021-08-17"])

remove_dates_shady_atm <- c("07/24/20", "09/30/20", "01/21/21",
                            "04/22/21", "05/27/21", "08/17/20")
for(i in remove_dates_shady_atm){
  shade_atm <- shade_atm[!complete.cases(str_locate(shade_atm$DateTime, i)),]
}

plot(shade_atm$Temp, type = "l")
plot(shade_atm$Lux, type = "l")
write.csv(shade_atm, paste0(directory, "/shadeatm.csv"), row.names = F)

##### MEACHAM WATER TEMPERATURE ####
water_temp <- read.csv(paste0(directory, "/water/meacham_creek_temperature.csv"),
                       skip = 2,
                       col.names = c("DateTime", "Temp", "trash1", "trash2", "trash3"))
water_temp <- water_temp[,1:2]
head(water_temp)

#### CHANGE TO UTC-8 (previously UTC-7)
water_temp$DateTime <- mdy_hm(water_temp$DateTime)
water_temp$DateTime <- water_temp$DateTime-3600
head(water_temp)

watertempx <- xts(zoo(water_temp$Temp, order.by=water_temp$DateTime))
plot.zoo(watertempx["2021-07/"])
plot.zoo(watertempx["2021-07-22/2021-07-25"])
plot(watertempx["2021-07-18/2021-07-22"])
## Any date after July 18, 2021 needs to be cut.

plot.zoo(watertempx)
plot.zoo(watertempx["/2021-07-18"])

par(mfrow = c(2,1),
    mar = c(1,2,1,1))
plot.zoo(watertempx["2020-07"], ylim = c(12,30))
plot.zoo(watertempx["2021-07"], ylim = c(12,30))

# Check subset
plot(subset(water_temp, DateTime < mdy("07/19/2021"))$Temp, type = "l")
plot.zoo(watertempx["/2021-07-18"])
water_temp <- subset(water_temp, DateTime < mdy("07/19/2021"))

head(water_temp)
plot(watertempx["2020-06-17"])
plot(watertempx["2020-06-17/2020-06-20"])

water_temp <- water_temp[!complete.cases(str_locate(water_temp$DateTime, "2020-06-17")),]
head(water_temp)
tail(water_temp)

write.csv(water_temp, paste0(directory, "/water_temp.csv"), row.names = F)



