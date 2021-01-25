
unscramble <- function(file){
  
  # EXTRACT PERTINENT DATA FROM FILE
  # Time Step: 10 yr (315360000)
  xvals <- scan(file, skip = 6, nlines = 1200)
  yvals <- scan(file, skip = 1207, nlines = 1200)
  zvals <- scan(file, skip = 2408, nlines = 1200)
  head <- scan(file, skip = 4209, nlines = 1200)
  # saturation <- scan(file, skip = 5410, nlines = 1200)
  # depth2GWT <- scan(file, skip = 6611, nlines = 1200)
  # temp <- scan(file, skip = 9612, nlines = 1200)
  # temp3mo <- scan(file, skip = 13809, nlines = 1200)
  # temp6mo <- scan(file, skip = 15011, nlines = 1200)
  # temp9mo <- scan(file, skip = 16213, nlines = 1200)
  # 
  # 
  
  #saturation <- scan(file, skip = 5410, nlines = 1200)
  depth2GWT <- scan(file, skip = 5410, nlines = 1200)
  temp <- scan(file, skip = 8411, nlines = 1200)
  temp3mo <- scan(file, skip = 12608, nlines = 1200)
  temp6mo <- scan(file, skip = 13810, nlines = 1200)
  temp9mo <- scan(file, skip = 15012, nlines = 1200)
  
  
  # A COLUMN THAT LABELS MY LAYERS SO I CAN USE subset() TO GET DATA FROM ONE LAYER
  layer <- c(rep("base", times = 6000/4), rep("middle", times = 6000/4), rep("topsat", times = 6000/4), rep("top", times = 6000/4))
  
  bigdf <- data.frame(x = xvals, 
                      y = yvals, 
                      z = zvals, 
                      temp = temp,
                      temp3mo = temp3mo,
                      temp6mo = temp6mo,
                      temp9mo = temp9mo,
                      head = head, 
                      # saturation = saturation, 
                      depth2GWT = depth2GWT,
                      layer = layer)
  
  longdf <- data.frame(allt = c(temp, temp3mo, temp6mo, temp9mo),
                       alltround = round(c(temp, temp3mo, temp6mo, temp9mo), 1))
  
  # ADDING MORE COLUMN TO THE DATAFRAME
  # tround IS TEMPERATURE ROUNDED TO TENTHS OF DEGREES
  # I did this so i could use the unique() function to see 
  # the range of values of temperature in my model output
  bigdf$tround <- round(bigdf$temp, 1)
  bigdf$tround3mo <- round(bigdf$temp3mo, 1)
  bigdf$tround6mo <- round(bigdf$temp6mo, 1)
  bigdf$tround9mo <- round(bigdf$temp9mo, 1)
  
  # SETTING UP A COLOR RAMP FUNCTION TO USE FOR TEMPERATURES AND SATURATIONS
  # colorRamp returns a function (rasterpal) that grades color 
  # from color 1 ("dodgerblue") to color 2 ("red")
  rasterpal <- colorRamp(c("skyblue1", "red")) 
  
  # NORMALIZING ROUNDED TEMPERATURES TO BE A FRACTION BETWEEN 0 AND 1
  # My minimum temp is 0, max temp is 1; this is just the format that 
  # rasterpal() needs
  normalizeAll <- (longdf$alltround - min(longdf$alltround))/(max(longdf$alltround) - min(longdf$alltround))
  # ADDING COLUMN FOR TEMPERATURE COLORS TO USE IN MY PLOTS
  longdf$tempcolors <- rgb(rasterpal(normalizeAll), max = 255)
  bigdf$tempcolors <- longdf$tempcolors[1:6000]
  bigdf$temp3mocolors <- longdf$tempcolors[6001:12000]
  bigdf$temp6mocolors <- longdf$tempcolors[12001:18000]
  bigdf$temp9mocolors <- longdf$tempcolors[18001:24000]
  
  # ADDING COLUMN FOR SATURATION COLORS TO USE IN PLOTS
  # bigdf$colorsat <- rgb(rasterpal(round(bigdf$saturation, 2)), max = 255)
  
  
  
  # min 1.1 round to 1, max 28.8 round to 29
  # scale temperature colors to range 1, 29
  # minTempColorRange <- -6
  # maxTempColorRange <- 46
  # colorForce2 <- (bigdf$tround - minTempColorRange)/(maxTempColorRange - minTempColorRange)
  # bigdf$newtempcolors <- rgb(rasterpal(colorForce2), max = 255)
  
  return(bigdf)
}


noatm <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simpleSolidTop/1/simple_solido.pm.dat")
unshaded <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simpleSolidTop/2/simple_solido.pm.dat")
shaded <- unscramble("C:/Users/Katie Fogg/Desktop/HGSwork/simpleSolidTop/3/simple_solido.pm.dat")

index <- seq(2, 6000, by=3)

noatm_mo1 <- data.frame(t = noatm$temp, layer = noatm$layer, col = noatm$tempcolors, y = noatm$y)[index,]
noatm_mo3 <- data.frame(t = noatm$temp3mo, layer = noatm$layer, col = noatm$temp3mocolors, y = noatm$y)[index,]
noatm_mo6 <- data.frame(t = noatm$temp6mo, layer = noatm$layer, col = noatm$temp6mocolors, y = noatm$y)[index,]
noatm_mo9 <- data.frame(t = noatm$temp9mo, layer = noatm$layer, col = noatm$temp9mocolors, y = noatm$y)[index,]

unshaded_mo1 <- data.frame(t = unshaded$temp, layer = unshaded$layer, col = unshaded$tempcolors, y = unshaded$y)[index,]
unshaded_mo3 <- data.frame(t = unshaded$temp3mo, layer = unshaded$layer, col = unshaded$temp3mocolors, y = unshaded$y)[index,]
unshaded_mo6 <- data.frame(t = unshaded$temp6mo, layer = unshaded$layer, col = unshaded$temp6mocolors, y = unshaded$y)[index,]
unshaded_mo9 <- data.frame(t = unshaded$temp9mo, layer = unshaded$layer, col = unshaded$temp9mocolors, y = unshaded$y)[index,]

shaded_mo1 <- data.frame(t = shaded$temp, layer = shaded$layer, col = shaded$tempcolors, y = shaded$y)[index,]
shaded_mo3 <- data.frame(t = shaded$temp3mo, layer = shaded$layer, col = shaded$temp3mocolors, y = shaded$y)[index,]
shaded_mo6 <- data.frame(t = shaded$temp6mo, layer = shaded$layer, col = shaded$temp6mocolors, y = shaded$y)[index,]
shaded_mo9 <- data.frame(t = shaded$temp9mo, layer = shaded$layer, col = shaded$temp9mocolors, y = shaded$y)[index,]

setwd("C:/Users/Katie Fogg/Desktop/HGSwork")

noatm_alpha <- 1
unshaded_alpha <- 1
shaded_alpha <- 1

noatm_alpha <- 1
unshaded_alpha <- 1
shaded_alpha <- 1

rgb(col2rgb("black")[1]/255, col2rgb("cyan")[2]/255, col2rgb("cyan")[3]/255, alpha = noatm_alpha)

png("ForWaterCenterProposal.png", width = 1000*4, height = 550*4, res = 72*4)
par(cex = 1.5,
    cex.lab = 1.5,
    cex.main = 1.5,
    cex.axis = 1.5,
    mar = c(5,5,3,1))
plot(subset(shaded_mo6, layer == "middle")$y, 
     subset(shaded_mo6, layer == "middle")$t, 
     type = "l",
     ylab = expression("Temperature (" ~ degree ~ "C)"), 
     xlab = "Flow Path Length (m)",
     col = "black",
     xlim = c(1000,0), 
     lwd = 2,
     ylim = c(0,25),
     main = "June Hyporheic Temperature",
     xaxt = "n",
     yaxt = "n",
     lty = 1)
lines(subset(unshaded_mo6, layer == "middle")$y, 
      subset(unshaded_mo6, layer == "middle")$t, 
      col = "black",
      lwd = 2,
      lty = 2)
lines(subset(noatm_mo6, layer == "middle")$y, 
      subset(noatm_mo6, layer == "middle")$t, 
      col = "blue",
      lwd = 2,
      lty = 2)
axis(1, at = seq(1000, 0, -200), labels = seq(0, 1000, 200))
axis(2, at = seq(5, 30, by = 5), labels = c("5", "10", "15", "20", "25", "30"))

dev.off()


#########
## ALL ##
#########
linewd <- 5
jancol <- "skyblue"
marchcol <- "dodgerblue"
junecol <- "orange"
septcol <- "chocolate"

png("AllScenarios_white_color.png", width = 1200*4, height = 1650*4, res = 72*4)
par(cex = 1.5,
    cex.lab = 1.5,
    cex.main = 1.5,
    cex.axis = 1.5,
    mar = c(5,5,3,1),
    mfrow = c(3,1))
#### NO ATM ####
par(cex = 1.5,
    cex.lab = 1.5,
    cex.main = 1.5,
    cex.axis = 1.5,
    mar = c(3,5,3,1))
plot(subset(noatm_mo1, layer == "middle")$y, 
     subset(noatm_mo1, layer == "middle")$t, 
     type = "l",
     ylab = expression("Temperature (" ~ degree ~ "C)"), 
     xlab = "",
     col = jancol,
     xlim = c(1000,0), 
     lwd = linewd,
     ylim = c(-5,30),
     main = "Hyporheic Temperature: No Atmospheric Heat Exchange Scenario",
     xaxt = "n",
     yaxt = "n")
lines(subset(noatm_mo3, layer == "middle")$y, 
      subset(noatm_mo3, layer == "middle")$t, 
      col = marchcol,
      lwd = linewd)
lines(subset(noatm_mo6, layer == "middle")$y, 
      subset(noatm_mo6, layer == "middle")$t, 
      col = junecol,
      lwd = linewd)
lines(subset(noatm_mo9, layer == "middle")$y, 
      subset(noatm_mo9, layer == "middle")$t, 
      col = septcol,
      lwd = linewd)
axis(1, at = seq(1000, 0, -200), labels = seq(0, 1000, 200))
axis(2, at = seq(-5, 30, by = 5), labels = c("", "0", "", "10", "", "20", "", "30"))

### unshaded ###
par(cex = 1.5,
    cex.lab = 1.5,
    cex.main = 1.5,
    cex.axis = 1.5,
    mar = c(3,5,3,1))
plot(subset(unshaded_mo1, layer == "middle")$y, 
     subset(unshaded_mo1, layer == "middle")$t, 
     type = "l",
     ylab = expression("Temperature (" ~ degree ~ "C)"), 
     xlab = "",
     col = jancol,
     xlim = c(1000,0), 
     lwd = linewd,
     ylim = c(-5,30),
     main = "Hyporheic Temperature: Unshaded Scenario",
     xaxt = "n",
     yaxt = "n")
lines(subset(unshaded_mo3, layer == "middle")$y, 
      subset(unshaded_mo3, layer == "middle")$t, 
      col = marchcol,
      lwd = linewd)
lines(subset(unshaded_mo6, layer == "middle")$y, 
      subset(unshaded_mo6, layer == "middle")$t, 
      col = junecol,
      lwd = linewd)
lines(subset(unshaded_mo9, layer == "middle")$y, 
      subset(unshaded_mo9, layer == "middle")$t, 
      col = septcol,
      lwd = linewd)
axis(1, at = seq(1000, 0, -200), labels = seq(0, 1000, 200))
axis(2, at = seq(-5, 30, by = 5), labels = c("", "0", "", "10", "", "20", "", "30"))

#### shaded ####
par(cex = 1.5,
    cex.lab = 1.5,
    cex.main = 1.5,
    cex.axis = 1.5,
    mar = c(5,5,3,1))
plot(subset(shaded_mo1, layer == "middle")$y, 
     subset(shaded_mo1, layer == "middle")$t, 
     type = "l",
     ylab = expression("Temperature (" ~ degree ~ "C)"), 
     xlab = "Flow Path Length (m)",
     col = jancol,
     xlim = c(1000,0), 
     lwd = linewd,
     ylim = c(-5,30),
     main = "Hyporheic Temperature: Shaded Scenario",
     xaxt = "n",
     yaxt = "n")
lines(subset(shaded_mo3, layer == "middle")$y, 
      subset(shaded_mo3, layer == "middle")$t, 
      col = marchcol,
      lwd = linewd)
lines(subset(shaded_mo6, layer == "middle")$y, 
      subset(shaded_mo6, layer == "middle")$t, 
      col = junecol,
      lwd = linewd)
lines(subset(shaded_mo9, layer == "middle")$y, 
      subset(shaded_mo9, layer == "middle")$t, 
      col = septcol,
      lwd = linewd)
axis(1, at = seq(1000, 0, -200), labels = seq(0, 1000, 200))
axis(2, at = seq(-5, 30, by = 5), labels = c("", "0", "", "10", "", "20", "", "30"))
dev.off()


