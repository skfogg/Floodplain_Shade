library("sp")
library("rgeos")
source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\loadMeanData.R")
source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\loadStructureData.R")

soil_1.0m_structure_shady_100 <- loadStructureData("soil_1.0m", "shady", "100")

xs <- soil_1.0m_structure_shady_100[,2,,"X"]
zs <- soil_1.0m_structure_shady_100[,2,,"Z"]

as.vector(xs)

par(mar = c(4,4,1,1)+0.1)
plot(as.vector(xs),
     as.vector(zs),
     pch = ".",
     xlim = c(0,4),
     ylim = c(25,39),
     bty = "L",
     ylab = "Z-dimension (m)",
     xlab = "X-dimension (m)")
plot(as.vector(xs),
     as.vector(zs),
     pch = ".",
     xlim = c(0,11),
     ylim = c(9,39),
     bty = "L",
     ylab = "Z-dimension (m)",
     xlab = "X-dimension (m)")
plot(as.vector(xs),
     as.vector(zs),
     pch = ".",
     xlim = c(0,1000),
     ylim = c(0,39),
     bty = "L",
     ylab = "Z-dimension (m)",
     xlab = "X-dimension (m)")

poop <- readWKT("POLYGON((0 35, 1000 25, 1000 28, 0 38, 0 35))")
plotfolderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\"
png(paste0(plotfolderpath, "ModelGrid.png"),
    width = 900*5,
    height = 500*5,
    res=72*5)
par(bty = "l",
    mar = c(5,5,4,1),
    yaxt = "s",
    cex.axis = 1.5,
    cex.lab = 1.5,
    cex.main= 2)

plot(as.vector(xs),
     as.vector(zs),
     pch = ".",
     #xlim = c(0,1000),
     #ylim = c(0,39),
     bty = "L",
     ylab = "Z-dimension (m)",
     xlab = "X-dimension (m)",
     main = "Model Nodes",
     col = "black")
# plot(poop, 
#      add = T, 
#      col = "grey",
#      border = "grey")
# points(as.vector(xs),
#      as.vector(zs),
#      pch = ".")

# abline(35, -0.01, col = "orange", lwd = 1.5)
# abline(38, -0.01, col = "orange", lwd = 1.5)
dev.off()


