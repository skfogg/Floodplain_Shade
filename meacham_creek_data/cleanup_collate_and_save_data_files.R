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
tail(water_temp)

## (1) Get rid of first date of data, 06/17/2020:
# change DateTime to Posixct
water_temp$DateTime <- mdy_hm(water_temp$DateTime)
# test subset
head(subset(water_temp, DateTime >= mdy("06-18-20")))
# remove first date
water_temp <- subset(water_temp, DateTime >= mdy("06-18-20"))
head(water_temp)


## (2) Fix Daylight savings: 11-01-20 and 03-14-21
# make into xts format
watertx <- xts(zoo(water_temp$Temp, order.by = water_temp$DateTime))

# PDT GROUP 1:
watertx_pdt1 <- watertx["/2020-11-01 01:00:00"]
# check beginning, should be 2020-06-18 00:00:00
head(watertx_pdt1)
# check ending of this group, should be 2020-11-01 01:00:00 (twice)
tail(watertx_pdt1)
# this is the one that belongs to pst
watertx_pdt1["2020-11-01 01:00:00"][2]
# test subset, now should only have one 2020-11-01 01:00:00
tail(watertx_pdt1[-length(watertx_pdt1)])
# re-save without the repeat 1am value:
watertx_pdt1 <- watertx_pdt1[-length(watertx_pdt1)]
# check:
tail(watertx_pdt1)
# subtract an hour:
index(watertx_pdt1) <- index(watertx_pdt1) - 3600
# check: (should be 2020-11-01 00:00:00)
tail(watertx_pdt1)
# now need to remove the first entry because it is before 2020-06-18 00:00:00:
head(watertx_pdt1)
# remove first entry:
watertx_pdt1 <- watertx_pdt1[-1]
# re-check head
head(watertx_pdt1)


# PST GROUP:
watertx_pst <- watertx["2020-11-01 01:00:00/2021-03-14 01:00:00"]
# check beginning, should be 2020-11-01 01:00:00 (twice)
head(watertx_pst)
# remove the first 
watertx_pst <- watertx_pst[-1]
# check:
head(watertx_pst)
# check ending, should be 2021-03-14 01:00:00
tail(watertx_pst)

# PDT GROUP 2:
watertx_pdt2 <- watertx["2021-03-14 03:00:00/"]
# check beginning:
head(watertx_pdt2)
# subtract an hour:
index(watertx_pdt2) <- index(watertx_pdt2) - 3600
# check beginning, should now be 2021-03-14 02:00:00
head(watertx_pdt2)
# now remove all dates after 07-18-2021
watertx_pdt2 <- watertx_pdt2["/2021-07-18"]
# check ending, should now be 2021-07-18 23:00:00
tail(watertx_pdt2)

## NOW COMBINE:
watertx_corrected <- c(watertx_pdt1, watertx_pst, watertx_pdt2)
# check beginning:
head(watertx_corrected)
# check ending:
tail(watertx_corrected)
# check 2020-11-01:
watertx_corrected["2020-11-01"]
# check 2021-04-14:
watertx_corrected["2021-04-14"]


## Should be divisible by 24:
length(watertx_corrected)/24
# IT IS NOT !!

## Find the days with readings not equal to 24:
unique(apply.daily(watertx_corrected, length))
subset(apply.daily(watertx_corrected, length), x == 25)

## 2020-08-28:
watertx_corrected["2020-08-28"]
# "2020-08-28 11:41:00"

## 2020-11-09:
watertx_corrected["2020-11-09"]
# "2020-11-09 11:57:00"

# Create a data.frame from the xts/zoo values
wt <- data.frame(DateTime = index(watertx_corrected),
           Temp = coredata(watertx_corrected))

# create a new data.frame with those weird dates removed
wt2 <- wt[!wt$DateTime %in% c(ymd_hms("2020-08-28 11:41:00", "2020-11-09 11:57:00")),]

# check to see if it is divisible by 24:
length(wt2$DateTime)/24
## IT IS

## Re-assign water_temp to be the corrected water temperature data
water_temp <- wt2
# make sure names are right
names(water_temp)
# correct names:
names(water_temp) <- c("DateTime", "Temp")

## SAVE corrected water_temp data:
write.csv(water_temp, paste0(directory, "/watertemp.csv"), row.names = F)



