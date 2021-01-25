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
