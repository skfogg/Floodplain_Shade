########################
### SATURATION PLOTS ###
########################
library(HGSReader)
library(raster)
library(scatterplot3d)

modelrun <- "soil_2.0m"
foldername <- paste0("C:/Users/Katie Fogg/Desktop/HGSwork/April2020/Sensitivity_1/", modelrun)
datafile <- paste0(foldername, 
                   "/temperatureTestero.pm.dat")

f <- HGSFile(datafile)
sat <- HGSGetData(f, variables = c("X", "Y", "Z", "Sat"), blockNumbers = 1)

## z = 17 is 35m (start of alluvium)
plot(sat[10,2,17:length(sat[1,2,,,"Z"]),,"Sat"],
     sat[10,2,17:length(sat[1,2,,,"Z"]),,"Z"]-tail(sat[1,2,,,"Z"],1),
     type = "o",
     ylab = "Depth below surface (m)",
     xlab = "Water Content",
     main = paste0("Saturation: ", modelrun),
     xlim = c(0,1)
     )


source('C:/Users/Katie Fogg/Desktop/HGSwork/incrementDim.R')

#dimnames(g) 
#newg <- incrementDim(g, dimIDX = 5, newVals = 1, incrementName = "color")

satPalette <- colorRamp(c("linen", "blue"))

sround <- round(sat[,,,,"Sat"], 1)
sNorm <- (sround - min(sround))/(max(sround) - min(sround))
satcols <- rgb(satPalette(sNorm), max = 255)

sat <- incrementDim(sat, dimIDX = 5, newVals = satcols, incrementName = "color")

par(bg = "black",
    mar = c(1,1,1,1),
    col.main = "white")

plotBlack <- function(x, t){
  plot(x[,2,17:67,t,"X"],
       x[,2,17:67,t,"Z"],
       col = x[,2,,t,"color"],
       pch = ".", 
       cex = 2.5,
       main = t
       #ylim = c(34.9,40.0),
       #xlim = c(0, 150)
  )
 # abline(a = 38,
 #        b = -(1/100),
 #        col = "olivedrab1")
}

plotBlack(sat, 1)

par(bg = "white")


