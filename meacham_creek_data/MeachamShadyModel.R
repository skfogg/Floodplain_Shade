### SHadY SOIL COMPOUND SINE WAVE CALC ###
library(zoo)
library(xts)
library(lubridate)

## WHEN WORKING ON TOWER:
box_directory <- "C:/Users/t24x137/Box/UmatillaShare/"

source(paste0(box_directory, "includeNATimes.R"))
shadysoil <- read.csv(paste0(box_directory, "shade/shadysoil.csv"))

shadysoilx <- xts(zoo(shadysoil$Temp, order.by = mdy_hms(shadysoil$DateTime)))
shadysoilx <- includeNATimes(shadysoilx)
shadysoil <- includeNATimes(shadysoilx, dropTS = T)
colnames(shadysoil) <- c("DateTime", "Temp")

shadysoil$DateTime <- as.character(shadysoil$DateTime)
shadysoil$PosixTime <- ymd_hms(shadysoil$DateTime)
shadysoil$NumTime <- as.numeric(shadysoil$PosixTime)
shadysoil$NumTimeOfYr <- shadysoil$NumTime - as.numeric(mdy_hms("01/01/20 00:00:00"))

shadysoildailymeanx <- apply.daily(shadysoilx, mean)
plot(shadysoildailymeanx)
shadysoil$DailyMean <- rep(coredata(shadysoildailymeanx), each = 24) 
plot(shadysoil$DailyMean, type = "l")

shadysoildaily <- data.frame(DateTime = as.character(index(shadysoildailymeanx)),
                             PosixTime = index(shadysoildailymeanx),
                             NumTime = as.numeric(index(shadysoildailymeanx)),
                             NumTimeOfYr = as.numeric(index(shadysoildailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                             DailyMean = coredata(shadysoildailymeanx))
names(shadysoildaily)[5] <- "DailyMean"

# mean + amplitude*cos(2*pi*frequency*t + period)


### --------------------------- DAILY MEAN MODEL --------------------------- ###
### ------------------------------------------------------------------------ ###
# start val for m:
mean(shadysoildaily$DailyMean, na.rm = T)
# start val for a:
(max(shadysoildaily$DailyMean, na.rm = T)-min(shadysoildaily$DailyMean, na.rm = T))/2
# fucking guess the radian shift for p

dailymeanmodel <- nls(DailyMean ~ m + a*sin(2*pi*(1/31536000)*NumTimeOfYr + p),
                      shadysoildaily,
                      start = list(m = 8.47,
                                   a = 8.45,
                                   p = 0.1))
summary(dailymeanmodel)
plot(DailyMean ~ I(NumTimeOfYr/86400), shadysoildaily, type = "l")
points(shadysoildaily[complete.cases(shadysoildaily$DailyMean),4]/86400, predict(dailymeanmodel), col = "blue")


t <- seq(0, 31536000*2, by = 86400)
plot(coef(dailymeanmodel)[1] + coef(dailymeanmodel)[2]*sin(2*pi*(1/31536000)*t + coef(dailymeanmodel)[3]),
     type = "l", 
     lwd = 2,
     ylab = "Daily Mean Temperture")
lines(DailyMean ~ I(NumTimeOfYr/86400), shadysoildaily, col = "blue")

### -------------------- DAILY AMPLITUDE MODEL -------------------------- ###
### --------------------------------------------------------------------- ###
amps <- apply.daily(shadysoilx, function(x) (max(x)-min(x))/2)
plot.zoo(amps)

shadysoildaily$DailyAmp <- coredata(amps)
plot(shadysoildaily$DailyAmp, type = "l")

# start val for m:
mean(shadysoildaily$DailyAmp, na.rm = T)
# start val for a:
(max(shadysoildaily$DailyAmp, na.rm = T)-min(shadysoildaily$DailyAmp, na.rm = T))/2
# fucking guess the radian shift for p

dailyampmodel <- nls(DailyAmp ~ m + a*sin(2*pi*(1/31536000)*NumTimeOfYr + p),
                     shadysoildaily,
                     start = list(m = 0.3,
                                  a = 0.4,
                                  p = 0.1))
summary(dailyampmodel)
plot(coef(dailyampmodel)[1] + coef(dailyampmodel)[2]*sin(2*pi*(1/31536000)*t + coef(dailyampmodel)[3]),
     type = "l", 
     lwd = 2,
     ylab = "Daily Amplitude",
     ylim = c(0,3))
lines(DailyAmp ~ I(NumTimeOfYr/86400), shadysoildaily, col = "violet")

## --------------------- DAILY FREQUENCY MODEL ------------------- ##
## --------------------------------------------------------------- ##

dailyfreqmodel <- nls(I(Temp-DailyMean) ~ a*sin(2*pi*(1/86400)*NumTimeOfYr + p),
                      shadysoil,
                      start = list(a = 2,
                                   p = 0.1))
summary(dailyfreqmodel)
t2 <- seq(0, 31536000*2, by = 3600)
plot(coef(dailyfreqmodel)[1]*sin(2*pi*(1/86400)*t2 + coef(dailyfreqmodel)[2]),
     type = "l")
lines(shadysoil$Temp - shadysoil$DailyMean, col = "yellow")

plot(shadysoil$Temp - shadysoil$DailyMean, type = "l")
lines(predict(dailyfreqmodel), col = "yellow")


## --------------------- COMPOUND MODEL -------------------------- ##
## --------------------------------------------------------------- ##

means <- coef(dailymeanmodel)[1] + coef(dailymeanmodel)[2]*sin(2*pi*(1/31536000)*t2 + coef(dailymeanmodel)[3])
amps <- coef(dailyampmodel)[1] + coef(dailyampmodel)[2]*sin(2*pi*(1/31536000)*t2 + coef(dailyampmodel)[3])

meachamshade <- means + amps*sin(2*pi*(1/86400)*t2 + coef(dailyfreqmodel)[2])
plot(meachamshade ~ t2, type = "l")
lines(shadysoil$Temp ~ shadysoil$NumTimeOfYr,
      col = "blue")


t10 <- seq(0, 31536000*10, by = 3600)
means10yr <- coef(dailymeanmodel)[1] + coef(dailymeanmodel)[2]*sin(2*pi*(1/31536000)*t10 + coef(dailymeanmodel)[3])
amps10yr <- coef(dailyampmodel)[1] + coef(dailyampmodel)[2]*sin(2*pi*(1/31536000)*t10 + coef(dailyampmodel)[3])

meachamshade10yr <- means10yr + amps10yr*sin(2*pi*(1/86400)*t10 + coef(dailyfreqmodel)[2])
plot(meachamshade10yr)

meachamshade10yrdf <- data.frame(s = t10, e = t10+3600, temp = meachamshade10yr)
write.csv(meachamshade10yrdf, paste0(box_directory, "shade/meachamshade10yr.csv"))

 







