
## Packages ##
library(HGSReader)
library(raster)
library(scatterplot3d)
library(RColorBrewer)
source('C:/Users/Katie Fogg/Desktop/HGSwork/incrementDim.R')
library(animation)
library(sp)
library(gstat)
library(raster)
library(dismo)
library(deldir)
library(rgeos)
library(stringr)
library(scales)
library(paletteer)

####################
### Name Vectors ###
####################
rivercontrols <- c( 
  "soil_0.3m_riveronly",
  "soil_0.5m_riveronly",
  "soil_1.0m_riveronly",
  "soil_2.0m_riveronly")

soilcontrols <- c(
  "soil_0.3m_soilonly",
  "soil_0.5m_soilonly",
  "soil_1.0m_soilonly",
  "soil_2.0m_soilonly")

models <- c(
  "soil_0.3m",
  "soil_0.5m",
  "soil_1.0m",
  "soil_2.0m")

allruns <- c(rivercontrols, soilcontrols, models)

allruns[str_detect(allruns, models[1])]
#################
### Load Data ###
#################
folderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\sunny_iskulpaa_soil_input\\"

for(i in 1:length(allruns)){
  assign(paste0(allruns[i], "_means"), readRDS(paste0(folderpath, allruns[i], "\\", allruns[i], "_dailymeans.RData")))
  assign(paste0(allruns[i], "_structure"), readRDS(paste0(folderpath, allruns[i], "\\", allruns[i], "_modelstructure.RData")))
}

#####################
#### Output Times ###
#####################
yr7Jdays <- seq(365*86400*7, by = 15*86400, length.out = 24)

yr7Jdays_fulldays <- unlist(lapply(yr7Jdays, 
                                   function(x)
                                     seq(x, by = 3600, length.out = 24)))
normalJday <- (yr7Jdays- (365*86400*7))/86400

everyother <- seq(2,24,by=2)
everyothermonth <- everyother[seq(1,12,by=2)]

monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthnames_abrv <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

daysinmonth <- c(31, 28, 31, 
                 30, 31, 30,
                 31, 31, 30, 
                 31, 30, 31)

dayofyr <- paste(monthnames_abrv[1], seq(1:daysinmonth[1]))
for(i in 2:12){
  dayofyr <- c(dayofyr, paste(monthnames_abrv[i], seq(1:daysinmonth[i])))
}
jframe <- data.frame(outputnumber = 1:24, jday = normalJday+1, dayofyear = dayofyr[normalJday+1])
seasons <- subset(jframe, jday %in% c(1,91,181,271))

## bedrock = 34.4m, hz = 36.5 ##
zIDX <- c(bedrock = 16, hz = 32)
depths <- zIDX

##################
### Color Shiz ###
##################
timePalette <- colorRampPalette(c("purple", "thistle", "dodgerblue", "skyblue", "seagreen"))
xPalette <- colorRampPalette(c("dodgerblue","purple", "red"))
timecolpal <- "jcolors::pal12"


# hzsampledf <- data.frame(times = rep(1:24, times = 496),
#                          temps = hzsample,
#                          x_meters = rep(as.numeric(xs), each = 24),
#                          x_index = rep(1:496, each = 24))

###
### HZ Daily Mean List @ Particular X-Values
###

xvalues <- c(m1.0 = 11, m4.0 = 41, m10.9 = 50, m60.9 = 75, m110.9 = 100, m510.9 = 300)

# hzdatalist <- lapply(xvalues, function(xIDX) subset(hzsampledf, x_index == xIDX))


#############
### Plotz ###
#############

plotfolderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\"
times  <- everyothermonth


# xs <- g[,2,1,1,"X"]
# ys <- g[1,2,1,1,"Y"]
# zs <- g[1,2,,1,"Z"]


##### X plot control triplet #####
linewidth <- 3
for (i in 1:length(models)){
png(paste0(plotfolderpath, models[i], "_xplot_controltriplet.png"),
    width = 400*5,
    height = 900*5,
    res = 72*5)
par(cex.main = 3,
    cex.lab = 1.8,
    cex.axis = 1.8,
    mfrow = c(3,1),
    mar = c(5,5,4,1)+0.1)
## plot 1
plot(get(paste0(models[i], "_structure"))[,2,32,"X"], 
     get(paste0(models[i], "_means"))[32,,1],
     type = "n",
     ylim = c(0,20),
     ylab = "Temperature",
     xlab = "Flow Path Length (m)",
     main = models[i])
mapply(function(t, c) lines(get(paste0(models[i], "_structure"))[,2,32,"X"], 
                            get(paste0(models[i], "_means"))[32,,t],
                            col = c,
                            lwd = linewidth),
       times,
       paletteer_d(timecolpal, n=12, direction = 1)[seq(1,12,by=2)])
legend("bottomright",
       monthnames[seq(1,12,by=2)],
       col = paletteer_d(timecolpal, n=12, direction = 1)[seq(1,12,by=2)],
       lwd = linewidth,
       ncol = 6,
       cex = 1.3)

## plot 2
plot(get(paste0(rivercontrols[i], "_structure"))[,2,32,"X"], 
     get(paste0(rivercontrols[i], "_means"))[32,,1],
     type = "n",
     ylim = c(0,20),
     ylab = "Temperature",
     xlab = "Flow Path Length (m)",
     main = rivercontrols[1])
mapply(function(t, c) lines(get(paste0(rivercontrols[i], "_structure"))[,2,32,"X"], 
                            get(paste0(rivercontrols[i], "_means"))[32,,t],
                            col = c,
                            lwd = linewidth),
       times,
       paletteer_d(timecolpal, n=12, direction = 1)[seq(1,12,by=2)])
legend("bottomright",
       monthnames[seq(1,12,by=2)],
       col = paletteer_d(timecolpal, n=12, direction = 1)[seq(1,12,by=2)],
       lwd = linewidth,
       ncol = 6,
       cex = 1.3)

## plot 3
plot(get(paste0(soilcontrols[i], "_structure"))[,2,32,"X"], 
     get(paste0(soilcontrols[i], "_means"))[32,,1],
     type = "n",
     ylim = c(0,20),
     ylab = "Temperature",
     xlab = "Flow Path Length (m)",
     main = soilcontrols[i])
mapply(function(t, c) lines(get(paste0(soilcontrols[i], "_structure"))[,2,32,"X"], 
                            get(paste0(soilcontrols[i], "_means"))[32,,t],
                            col = c,
                            lwd = linewidth),
       times,
       paletteer_d(timecolpal, n=12, direction = 1)[seq(1,12,by=2)])
legend("bottomright",
       monthnames[seq(1,12,by=2)],
       col = paletteer_d(timecolpal, n=12, direction = 1)[seq(1,12,by=2)],
       lwd = linewidth,
       ncol = 6,
       cex = 1.3)
dev.off()
}


### Summer compare ###
zdepth <- 30
hz_zdepths <- list(soil_0.3m = c(4:34),
                   soil_0.5m = c(6:36),
                   soil_1.0m = c(11:41),
                   soil_2.0m = c(21:51))

### HZ all seasons ribbon plots ###
for(s in 1:nrow(seasons)){
png(paste0(plotfolderpath, gsub(" ", "_", seasons[s,3]),"_allhz.png"),
    width = 500*5,
    height = 900*5,
    res = 72*5)
  layout(matrix(c(1,5, 1,5,
                  1,5, 2,5, 2,6,
                  2,6, 3,6, 3,6,
                  3,7, 4,7, 4,7,
                  4,7), nrow = 12, ncol = 2, byrow = T), widths = c(5,1))
  
  for(i in 1:length(models)){
    
    plot(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"], 
         get(paste0(models[i], "_means"))[hz_zdepths[[i]][1],,seasons[s,1]],
         main = paste(models[i], seasons[s,3]),
         col = "blue",
         ylab = "Temperature",
         xlab = "Flow Path Length (m)",
         type = "n",
         lwd = linewidth,
         ylim = c(2,22))
    
    mapply(function(z,c) lines(get(paste0(soilcontrols[i], "_structure"))[,2,z,"X"], 
                               get(paste0(soilcontrols[i], "_means"))[z,,seasons[s,1]],
                               col = c,
                               lwd = 1),
           hz_zdepths[[i]],
           hcl.colors(31, "Peach"))
    mapply(function(z,c) lines(get(paste0(models[i], "_structure"))[,2,z,"X"], 
                               get(paste0(models[i], "_means"))[z,,seasons[s,1]],
                               col = c,
                               lwd = 1),
           hz_zdepths[[i]],
           hcl.colors(31, "Purp"))
    mapply(function(z,c) lines(get(paste0(rivercontrols[i], "_structure"))[,2,z,"X"], 
                               get(paste0(rivercontrols[i], "_means"))[z,,seasons[s,1]],
                               col = c,
                               lwd = 1),
           hz_zdepths[[i]],
           hcl.colors(31, "Emrld"))
    
  }
  
  par(mar = c(0,0,2,0)+0.1)
  plot(seq(0,1,length.out=31),
       1:31,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       frame = F,
       main = "soil only")
  mapply(function(y, c) segments(x0 = 0, y0=y, x1 = 1, y1=y,col = c, lwd = 4),
         1:31,
         hcl.colors(31, "Peach")[seq(31,1,-1)])
  text(0, 32, labels = c("hz depth (cm)"))
  mapply(function(y, ylabel) text(0, y, labels = ylabel, pos = 2),
         1:31,
         seq(300,0,-10))
  
  
  plot(seq(0,1,length.out=31),
       1:31,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       frame = F,
       main = "combined",
       xlim = c(-1,1),
       ylim = c(0,33))
  mapply(function(y, c) segments(x0 = 0, y0=y, x1 = 1, y1=y,col = c, lwd = 4),
         1:31,
         hcl.colors(31, "Purp")[seq(31,1,-1)])
  text(0, 32, labels = c("hz depth (cm)"))
  mapply(function(y, ylabel) text(0, y, labels = ylabel, pos = 2),
         1:31,
         seq(300,0,-10))
  
  plot(seq(0,1,length.out=31),
       1:31,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       frame = F,
       main = "river only",
       xlim = c(-1,1),
       ylim = c(0,33))
  mapply(function(y, c) segments(x0 = 0, y0=y, x1 = 1, y1=y,col = c, lwd = 4),
         1:31,
         hcl.colors(31, "Emrld")[seq(31,1,-1)])
  text(0, 32, labels = c("hz depth (cm)"))
  mapply(function(y, ylabel) text(0, y, labels = ylabel, pos = 2),
         1:31,
         seq(300,0,-10))
  
  
  dev.off()
}
                                                             



# mean line with range line
i=1
s=3
plot(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"], 
     mapply(function(x) mean(get(paste0(models[i], "_means"))[hz_zdepths[[i]],x,seasons[s,1]]),
            1:496),
     main = paste(models[i]),
     col = "tomato",
     ylab = "Temperature",
     xlab = "Flow Path Length (m)",
     type = "l",
     lwd = 2,
     ylim = c(2,22),
     xaxt = "n",
     xlim = c(0,750))
lines(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"], 
      mapply(function(x) max(get(paste0(models[i], "_means"))[hz_zdepths[[i]],x,seasons[s,1]]),
             1:496),
      lty= 2,
      col = "tomato1")
lines(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"], 
      mapply(function(x) min(get(paste0(models[i], "_means"))[hz_zdepths[[i]],x,seasons[s,1]]),
             1:496),
      lty= 2,
      col = "tomato1")
axis(side =1, at = meters_at_RT_times, labels = RT_labels, las = 2)

s=1
lines(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"], 
      mapply(function(x) mean(get(paste0(models[i], "_means"))[hz_zdepths[[i]],x,seasons[s,1]]),
             1:496),
      lty= 1,
      col = "lightblue",
      lwd = 2)

lines(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"], 
      mapply(function(x) max(get(paste0(models[i], "_means"))[hz_zdepths[[i]],x,seasons[s,1]]),
             1:496),
      lty= 2,
      col = "lightblue2")
lines(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"], 
      mapply(function(x) min(get(paste0(models[i], "_means"))[hz_zdepths[[i]],x,seasons[s,1]]),
             1:496),
      lty= 2,
      col = "lightblue2")
####
###
#####
#####
###


soil_0.3m_hzmean <- mapply(function(x) mean(soil_0.3m_means[hz_zdepths$soil_0.3m,x,19]),
                           1:496)
lines(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"],
      soil_0.3m_hzmean,
      col = "red")



########## RT
i = 3
plot(get(paste0(models[i], "_structure"))[,2,hz_zdepths[[1]][1],"X"], 
     get(paste0(models[i], "_means"))[hz_zdepths[[i]][1],,seasons[s,1]],
     main = paste(models[i], seasons[s,3]),
     col = "blue",
     ylab = "Temperature",
     xlab = "Flow Path Length (m)",
     type = "n",
     lwd = linewidth,
     ylim = c(2,22),
     xaxt = "n",
     xlim = c(0,750))

mapply(function(z,c) lines(get(paste0(soilcontrols[i], "_structure"))[,2,z,"X"], 
                           get(paste0(soilcontrols[i], "_means"))[z,,seasons[s,1]],
                           col = c,
                           lwd = 1),
       hz_zdepths[[i]],
       hcl.colors(31, "Peach"))
mapply(function(z,c) lines(get(paste0(models[i], "_structure"))[,2,z,"X"], 
                           get(paste0(models[i], "_means"))[z,,seasons[s,1]],
                           col = c,
                           lwd = 1),
       hz_zdepths[[i]],
       hcl.colors(31, "Purp"))
mapply(function(z,c) lines(get(paste0(rivercontrols[i], "_structure"))[,2,z,"X"], 
                           get(paste0(rivercontrols[i], "_means"))[z,,seasons[s,1]],
                           col = c,
                           lwd = 1),
       hz_zdepths[[i]],
       hcl.colors(31, "Emrld"))
axis(side =1, at = meters_at_RT_times, labels = RT_labels, las = 2)









