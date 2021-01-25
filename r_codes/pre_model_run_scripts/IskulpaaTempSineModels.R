## 
## Compare mine and byron's inputs
##
library(zoo)
library(lubridate)
library(xts)

wd <- "C:/Users/Katie Fogg/Desktop/HGSwork"

airB <- read.table(paste0(wd, "/katie_byron_input_compare/airConc_byron.txt"), skip = 1)
airK_sun <- read.table(paste0(wd, "/katie_byron_input_compare/soilTemp_katie.txt"), skip = 1)
airK_shade <- read.table(paste0(wd,"/katie_byron_input_compare/soilTempShaded_katie.txt"), skip = 1)

streamB <- read.table(paste0(wd, "/katie_byron_input_compare/specConc_byron.txt"), skip=1)
streamK <- read.table(paste0(wd, "/katie_byron_input_compare/streamTemp_katie.txt"), skip=1)

par(bg = "white")

plot(airK_sun[,1], airK_sun[,3], col = "red", type = "l")
lines(airK_shade[,1], airK_shade[,3], col = "orange")
lines(airB[,1], airB[,2], type = "l")

plot(streamK[,1], streamK[,3], col = "forestgreen", type = "l")
lines(streamB[,1], streamB[,3], type = "l", col = "blue")

plot(airK_sun[,1], airK_sun[,3], col = "red", type = "l")
lines(airK_shade[,1], airK_shade[,3], col = "orange")
lines(streamK[,1], streamK[,3], col = "dodgerblue")

plot(airB[,1], airB[,2], type = "l", col = "brown")
lines(streamB[,1], streamB[,3], col = "blue")

### ISKULPAA ###
# Exploratory #
umatilla <- read.csv("C:/Users/Katie Fogg/Desktop/HGSwork/iskulpaa_db_file/RGM_RGS_WSS_Data.csv")
riverdf <- subset(umatilla, LocationName == "RGS" & ParameterName == "Temperature" & Status != "Error")
airdf <- subset(umatilla, LocationName == "WSS" & ParameterName == "Temperature"& Status != "Error")

air1 <- airdf[1:14135,]
plot(air1$Value, type = "l")

soil30df <-airdf[14136:28270,]
lines(soil30df$Value, col = "red")

soil75df <- airdf[28271:nrow(airdf),]
lines(soil75df$Value, col = "dodgerblue")

river <- xts(zoo(riverdf[1:20106,3], order.by = ymd_hms(riverdf$DataDateTime[1:20106])))
air <- xts(zoo(air1[,3], order.by = ymd_hms(air1$DataDateTime)))
soil30 <- xts(zoo(soil30df$Value, order.by = ymd_hms(soil30df$DataDateTime)))
soil75 <- xts(zoo(soil75df$Value, order.by = ymd_hms(soil75df$DataDateTime)))

plot.zoo(air)
lines(as.zoo(river), col = "dodgerblue")
lines(as.zoo(soil30), col = "brown")
lines(as.zoo(soil75), col = "forestgreen")

plot(air['2003-08-01'])
lines(river['2003-08-01'], col = "dodgerblue")
lines(soil30['2003-08-01'], col = "brown")
lines(soil75['2003-08-01'], col = "forestgreen")

plot(air['2003-08-08/2003-08-15'])
lines(river['2003-08-08/2003-08-15'], col = "dodgerblue")
lines(soil30['2003-08-08/2003-08-15'], col = "brown")
lines(soil75['2003-08-08/2003-08-15'], col = "forestgreen")

airyr <- air['2003-02-15/2004-02-14']
riveryr <- river['2003-02-15/2004-02-14']

plot.zoo(airyr)

ind <- ''
plot(airyr[ind], auto.grid = F)
lines(riveryr[ind], col = "dodgerblue")
c(min(airyr), max(airyr))

c(min(riveryr), max(riveryr))
c(min(streamK[,3]), max(streamK[,3]))

aug8 <- 220
plot(streamK[220*86400, ])


plot(subset(streamK, V1 >= 220*86400 & V2 <= 221*86400)[,3], type = "l", xlim = c(0,24))
lines(seq(0.5,24, by = 0.5), coredata(river['2003-08-08']), col = "orange")



#############
### RIVER ###
#############
### This code makes a continuous dataset with all time values, 
### NA where there are no values

source("C:/Users/Katie Fogg/Desktop/HGSwork/r_codes/ModelingFunctions.R")

river2 <- includeNATimes(river)
plot.zoo(river2)

river2003 <- river2['2003']
plot.zoo(river2003)

## Make a data set that only includes the days where we 
## have a complete set of data

riverw <- rmIncompleteDays(river)

# Daily mean temp
riverdailymeantemp <- apply.daily(riverw, mean)

# Diel range
riverdailyrangetemp <- apply.daily(riverw, rangecalc)
plot(riverdailyrangetemp)

## Plot the whole-day only data and the daily means
plot.zoo(riverw)
lines(riverdailymeantemp, col = "dodgerblue")


# frequency: oscillations per second
freqAnnual <- 1/(365*86400)
freqDaily <- 1/86400

#omega
omegaAnnual <- freqAnnual*2*pi
omegaDaily <- freqDaily*2*pi

## Sine: y(time) = amplitude*sin(2*pi*frequency*time + phase)

## Annual Temperature Signal Model
temptimedf <- toTempTimeDf(riverdailymeantemp)
names(temptimedf) <- c("temps", "t")


plot(temps~t, data = temptimedf)

model <- nls(temps ~ a*sin(omegaAnnual*t + p) + m, 
             data = temptimedf,
             start = list(a = 15, p = 0.1, m = 12))
summary(model)
annualAmpFitted <- coef(model)[[1]]
annualPhaseFitted <- coef(model)[[2]]
annualMeanFitted <- coef(model)[[3]]

pred <- predict(model, newdata=data.frame(t=t))    
plot(temps~t, data = temptimedf)
points(pred~t, col = "dodgerblue")

plot(annualAmpFitted*sin(omegaAnnual*seq(0, 365*86400*2, by = 86400) + annualPhaseFitted) + annualMeanFitted)

## Daily Amp Model
temptimedf <- toTempTimeDf(riverdailyrangetemp/2)
names(temptimedf) <- c("temps", "t")

plot(temps~t, data = temptimedf)

model2 <- nls(temps ~ a*sin(omegaAnnual*t + p) + m, 
             data = temptimedf,
             start = list(a = 4, p = 0.1, m = 2))
summary(model2)
annualRangeAmpFitted <- coef(model2)[[1]]
annualRangePhaseFitted <- coef(model2)[[2]]
annualRangeMeanFitted <- coef(model2)[[3]]

pred2 <- predict(model2, newdata=data.frame(t=t))    
plot(temps~t, data = temptimedf)
points(pred2~t, col = "dodgerblue")

## Daily model
dailymeanvec <- rep(riverdailymeantemp, each = 48)
rivernormalized <- riverw-dailymeanvec

plot.zoo(rivernormalized)

ttdf <- toTempTimeDf(rivernormalized)
names(ttdf) <- c("temps", "t")

x <- rivernormalized
temps <- coredata(x)
t <- as.numeric(index(x))
temptimedf <- data.frame(temps = temps, t = t)

plot(temps~t, data = ttdf)

model3 <- nls(temps ~ a*sin(2*pi*(1/86400)*t + p), 
              data = ttdf,
              start = list(a = 2, p = 0.1))
summary(model3)

t <- as.numeric(index(rivernormalized))
pred3 <- predict(model3, newdata=data.frame(t=t))    
plot(temps~t, data = ttdf, xlim = c(1.05e9, 1.05e9+(86400*7)))
points(pred3~t, col = "dodgerblue")

dailyPhaseFitted <- coef(model3)[[2]]

## Compound River
easytime <- seq(0, 365*86400*2, by = 1800)
means <- annualAmpFitted*sin(omegaAnnual*easytime + annualPhaseFitted) + annualMeanFitted
plot(means)

amps <- annualRangeAmpFitted*sin(omegaAnnual*easytime + annualRangePhaseFitted) + annualRangeMeanFitted
plot(amps)

compound <- amps*-sin(2*pi*(1/86400)*easytime + dailyPhaseFitted) + means

plot(compound, type = "l")
lines(coredata(riverw['2003-01-01/']), col = "dodgerblue")

plot(compound, xlim = c(0, 200))
points(coredata(riverw['2003-01-01/']), col = "dodgerblue")


## Normalize Time to line up with 'compound' model
normalt <- as.numeric(index(riverw)) - as.numeric(index(riverw))[[1]]

plot(easytime, compound, col = "dodgerblue", type = "l", ylim = c(0,25))
points(normalt, coredata(riverw), col = rgb(0,0,0, alpha = 0.6), pch = ".", cex = 2)

## Line up compound to the correct days of yr
#timeseqold <- seq(ymd_hms("2002-12-17 00:00:00"), ymd_hms("2004-12-16 00:00:00"), by = 1800)
timeseq <- seq(ymd_hms("2002-01-01 00:00:00"), ymd_hms("2004-12-31 00:00:00"), by = 1800)


cmp <- xts(zoo(compound, order.by = timeseq))
plot.zoo(cmp['2003'])

## Check alignment of Phase
plot.zoo(cmp['2003-05-08/2003-05-15'], ylim = c(5,16))
lines(riverw['2003-05-08/2003-05-15'], col = "blue")

## Remake model to values only at the hour
easytime <- seq(0, 365*86400*2, by = 3600)
means <- annualAmpFitted*sin(omegaAnnual*easytime + annualPhaseFitted) + annualMeanFitted
amps <- annualRangeAmpFitted*sin(omegaAnnual*easytime + annualRangePhaseFitted) + annualRangeMeanFitted
compound <- amps*-sin(2*pi*(1/86400)*easytime + dailyPhaseFitted) + means
timeseq <- seq(ymd_hms("2002-01-01 00:00:00"), ymd_hms("2004-12-31 00:00:00"), by = 3600)
cmp <- xts(zoo(compound, order.by = timeseq))

plot(cmp['2003'])
lines(riverw['2003'], col = "dodgerblue")

## Create a vector of repeating years
rivermodel <- data.frame(timeon = seq(0, by = 3600, length.out = length(rep(coredata(cmp['2003']), times = 10))), 
                         timeoff = seq(3600, by = 3600, length.out = length(rep(coredata(cmp['2003']), times = 10))),
                         temperature = round(rep(coredata(cmp['2003']), times = 10), 3))
head(rivermodel)
plot(temperature~timeon, data = rivermodel, col= "red", type = "l")
write.table(rivermodel, 
            "C:/Users/Katie Fogg/Desktop/HGSwork/IskulpaaRiverTemp.txt",
            row.names = FALSE,
            col.names = FALSE)

###############
### SOIL 30 ###
###############
source('ModelingFunctions.R')

# Include missing values as NA
soil302 <- includeNATimes(soil30)
length(soil302)-length(soil30)
## There are no missing values in the soil30 data

# Remove incomplete days:
soil30 <- rmIncompleteDays(soil30)

# Daily mean temp
soil30dailymeantemp <- apply.daily(soil30, mean)

# Diel range
soil30dailyrangetemp <- apply.daily(soil30, rangecalc)
plot(soil30dailyrangetemp)

## Plot the whole-day only data and the daily means
plot.zoo(soil30)
lines(soil30dailymeantemp, col = "coral")
abline(v=index(soil30['2004-01-01']))

## Sine: y(time) = amplitude*sin(2*pi*frequency*time + phase)

## Daily Mean Model
temptimedf <- toTempTimeDf(soil30dailymeantemp)
names(temptimedf) <- c("temps", "t")
plot(temps~t, data = temptimedf)

model <- nls(temps ~ a*sin(omegaAnnual*t + p) + m, 
             data = temptimedf,
             start = list(a = 12, p = 0.1, m = 14))
summary(model)
annualAmpFittedSoil30 <- coef(model)[[1]]
annualPhaseFittedSoil30 <- coef(model)[[2]]
annualMeanFittedSoil30 <- coef(model)[[3]]

t <- temptimedf$t
pred <- predict(model, newdata=data.frame(t=t))    
plot(temps~t, data = temptimedf)
points(pred~t, col = "coral")

## Daily Amp Model
temptimedf <- toTempTimeDf(soil30dailyrangetemp/2)
names(temptimedf) <- c("temps", "t")
plot(temps~t, data = temptimedf)

model2 <- nls(temps ~ a*sin(omegaAnnual*t + p) + m, 
              data = temptimedf,
              start = list(a = 1, p = 0.1, m = 1))
summary(model2)
annualRangeAmpFittedSoil30 <- coef(model2)[[1]]
annualRangePhaseFittedSoil30 <- coef(model2)[[2]]
annualRangeMeanFittedSoil30 <- coef(model2)[[3]]

t <- temptimedf$t
pred2 <- predict(model2, newdata=data.frame(t=t))    
plot(temps~t, data = temptimedf)
points(pred2~t, col = "coral")

## Daily model
dailymeanvec <- rep(soil30dailymeantemp, each = 24)
soil30normalized <- soil30-dailymeanvec
plot.zoo(soil30normalized)

temptimedf <- toTempTimeDf(soil30normalized)
names(temptimedf) <- c("temps", "t")
plot(temps~t, data = temptimedf)

model3 <- nls(temps ~ a*sin(2*pi*(1/86400)*t + p), 
              data = temptimedf,
              start = list(a = 1.5, p = 0.1))
summary(model3)

t <- temptimedf$t
pred3 <- predict(model3, newdata=data.frame(t=t))    
plot(temps~t, data = temptimedf)
points(pred3~t, col = "coral")

plot(temps~t, data = temptimedf, xlim = c(1.05e9, 1.05e9+(86400*7)))
points(pred3~t, col = "coral")

dailyPhaseFittedSoil30 <- coef(model3)[[2]]

## Compound Soil30
easytime <- seq(0, 365*86400*3, by = 3600)

means <- annualAmpFittedSoil30*sin(omegaAnnual*easytime + annualPhaseFittedSoil30) + annualMeanFittedSoil30
plot(means)


amps <- annualRangeAmpFittedSoil30*sin(omegaAnnual*easytime + annualRangePhaseFittedSoil30) + annualRangeMeanFittedSoil30
plot(amps)

compoundsoil30 <- amps*-sin(2*pi*(1/86400)*easytime + dailyPhaseFittedSoil30) + means

as.numeric(index(soil30[1])) - as.numeric(ymd_hms("2003-01-01 00:00:00"))
3110400/3600
plot(compoundsoil30[864:length(compoundsoil30)], type = "l", ylim = c(0,28))
lines(coredata(soil30), col = "coral")

## Line up compound to the correct days of yr
timeseq <- seq(ymd_hms("2002-01-01 00:00:00"), ymd_hms("2004-12-31 00:00:00"), by = 3600)
cmps <- xts(zoo(compoundsoil30, order.by = timeseq))
plot.zoo(cmps[])
lines(soil30, col = "coral")

## Check alignment of Phase
plot.zoo(cmps['2003-05-08/2003-05-15'], ylim = c(5,16))
lines(soil30['2003-05-08/2003-05-15'], col = "coral")

## Plot 1 yr
plot(cmps['2003'])

## Create a vector of repeating years
soil30model <- data.frame(timeon = seq(0, by = 3600, length.out = length(rep(coredata(cmps['2003']), times = 10))), 
                         timeoff = seq(3600, by = 3600, length.out = length(rep(coredata(cmps['2003']), times = 10))),
                         temperature = round(rep(coredata(cmps['2003']), times = 10), 3))
head(soil30model)
plot(temperature~timeon, data = soil30model, col= "red", type = "l")
#write.table(soil30model, 
#            "C:/Users/Katie Fogg/Desktop/HGSwork/IskulpaaSoil30Temp.txt",
#            row.names = FALSE,
#            col.names = FALSE)


plot(rivermodel$temperature, type = "l", ylim = c(0,28))
lines(soil30model$temperature, col = "coral")

plot(riverw, ylim = c(0,28))
lines(soil30, col = "coral")


## FAKE UNSHADED
decreaseinamp <- data.frame(rangediff = c(8.5, 7,4), depth = c(1,5,10))
plot(rangediff ~ depth, data = decreaseinamp)
sagebrushampmodel <- lm(rangediff ~ depth, data = decreaseinamp)
summary(sagebrushampmodel)
#model equation
coef(sagebrushampmodel)[1] + coef(sagebrushampmodel)[2]*30


means_unshade <- (annualAmpFittedSoil30+2)*sin(omegaAnnual*easytime + annualPhaseFittedSoil30) + (annualMeanFittedSoil30-2)
plot(means_unshade)


compoundsoil30unshaded <- (amps-0.3)*-sin(2*pi*(1/86400)*easytime + dailyPhaseFittedSoil30) + means_unshade
plot(compoundsoil30, type= "l", col = "orange",
     ylab = "Temperature (C)",
     main = "Sunny and Shady Soil Temperatures")
lines(compoundsoil30unshaded, col = "brown")
legend("topleft",c("Sunny", "Shady"),
       col = c("orange", "brown"),lty =1)



cmps_unshaded <- xts(zoo(compoundsoil30unshaded, order.by = timeseq))
soil30model_unshaded <- data.frame(timeon = seq(0, by = 3600, length.out = length(rep(coredata(cmps['2003']), times = 10))), 
                          timeoff = seq(3600, by = 3600, length.out = length(rep(coredata(cmps['2003']), times = 10))),
                          temperature = round(rep(coredata(cmps_unshaded['2003']), times = 10), 3))


write.table(soil30model_unshaded,
            "C:/Users/Katie Fogg/Desktop/HGSwork/IskulpaaSoil30Temp_FakeUnshaded.txt",
            row.names = FALSE,
            col.names = FALSE)



###############
### SOIL 75 ###
###############
source('ModelingFunctions.R')

plot(soil75)
## No need for daily signal modeling, just annual. The daily signal has
#  been damped out.

# Include missing values as NA
soil752 <- includeNATimes(soil75)
length(soil752)-length(soil75)
## There are no missing values in the soil30 data

# Remove incomplete days:
soil75 <- rmIncompleteDays(soil75)

## Sine model
## Sine: y(time) = amplitude*sin(2*pi*frequency*time + phase)
## Annual Model
temptimedf <- toTempTimeDf(soil75)
names(temptimedf) <- c("temps", "t")
plot(temps~t, data = temptimedf)

model <- nls(temps ~ a*sin(2*pi*1/(86400*365)*t + p) + m, 
             data = temptimedf,
             start = list(a = 12, p = 0.1, m = 13))
summary(model)
annualAmpFittedSoil75 <- coef(model)[[1]]
annualPhaseFittedSoil75 <- coef(model)[[2]]
annualMeanFittedSoil75 <- coef(model)[[3]]

t <- temptimedf$t
pred <- predict(model, newdata=data.frame(t=t))    
plot(temps~t, data = temptimedf)
points(pred~t, col = "deeppink")

## Soil75 Model
easytime <- seq(0, 365*86400*3, by = 3600)
sinesoil75 <- annualAmpFittedSoil75*sin(2*pi*(1/(86400*365))*easytime + annualPhaseFittedSoil75) + annualMeanFittedSoil75
plot(sinesoil75)

## Line up compound to the correct days of yr
timeseq <- seq(ymd_hms("2002-01-01 00:00:00"), ymd_hms("2004-12-31 00:00:00"), by = 3600)
smps <- xts(zoo(sinesoil75, order.by = timeseq))
plot.zoo(smps[], ylim = c(0,28))
lines(soil75, col = "deeppink")

## Check alignment of Phase
plot.zoo(smps['2003-05-08/2003-06-08'], ylim = c(5,20))
lines(soil75['2003-05-08/2003-06-08'], col = "deeppink")

## Plot 1 yr
plot(smps['2003'])

## Create a vector of repeating years
soil75model <- data.frame(timeon = seq(0, by = 3600, length.out = length(rep(coredata(smps['2003']), times = 10))), 
                          timeoff = seq(3600, by = 3600, length.out = length(rep(coredata(smps['2003']), times = 10))),
                          temperature = round(rep(coredata(smps['2003']), times = 10), 3))
head(soil75model)
plot(temperature~timeon, data = soil75model, col= "red", type = "l")
write.table(soil75model, 
            "C:/Users/Katie Fogg/Desktop/HGSwork/IskulpaaSoil75Temp.txt",
            row.names = FALSE,
            col.names = FALSE)



##################
### NICE PLOTS ###
##################
par(mfrow = c(2,1),
    mar = c(4,5,2,1))
plotidx <- '2002-12-17/2004-09-20'

plot.zoo(cmp[plotidx], type = "l", ylim = c(0,28), col = "white", lwd = 2,
         ylab = expression(paste("Temperature, ", degree,"C")),
         xlab = "",
         main = "Iskulpaa Measured Temperatures")
lines(includeNATimes(riverw), col = "dodgerblue", lwd = 2)
lines(soil30, col = "coral", lwd = 2)
lines(soil75, col = "deeppink4", lwd = 2)
mtext("Time", side = 1, line =2)
legend("topleft", c("River", "Soil, 30cm", "Soil, 75cm"), lwd = 2, 
       col = c("dodgerblue", "coral", "deeppink4"),
       bty = "n")


plot.zoo(cmp[plotidx], type = "l", ylim = c(0,28), col = "dodgerblue", lwd = 2,
         ylab = expression(paste("Temperature, ", degree,"C")),
         xlab = "",
         main = "Modeled Temperatures")
lines(cmps[plotidx], col = "coral", lwd = 2)
lines(smps[plotidx], col = "deeppink4", lwd = 2)
mtext("Time", side = 1, line =2)
legend("topleft", c("River", "Soil, 30cm", "Soil, 75cm"), lwd = 2, 
       col = c("dodgerblue", "coral", "deeppink4"),
       bty = "n")



#################
#### Appendix ###
#################
## Looking at Phase shifting ##
plot(-sin(2*pi*1/86400*seq(0,86400, by = 3600)), type = "o", xlim = c(-1,28))
lines(-sin(2*pi*1/86400*seq(0,86400, by = 3600)-(8)/24), 
      type = "o", 
      col = "green")

plot(riverw["2003-01-06"], type = "o")


subset(riverw["2003-01-06"], riverw["2003-01-06"] == min(riverw["2003-01-06"]))
# phase shift for 2003-01-06 = -8/24

plot(riverw["2003-01-01"], type = "o")
subset(riverw["2003-01-01"], riverw["2003-01-01"] == min(riverw["2003-01-01"]))

m <- "2003-12"
plot(riverw[m], type = "l")
d <- "2003-12-07"
plot(riverw[d], type = "o")
subset(riverw[d], riverw[d] == min(riverw[d]))

splitriver <- split(riverw, "days")
morningmin <- function(x){
  hour(index(subset(x, x[1:24] == min(x[1:24]))))[1]
}
morningmin(splitriver[[4]])
lapply(splitriver, morningmin)

timesplit <- as.POSIXct(unlist(lapply(splitriver, index)), origin = '1970-01-01', tz = "UTC")
dailymintimes <- xts(zoo(unlist(lapply(splitriver, morningmin)), order.by = index(riverw[endpoints(riverw,"days")])))
plot(dailymintimes)
plot(dailymintimes['2003-02'])
plot(riverw['2003-02-04'], type = "o")
mean(dailymintimes)



