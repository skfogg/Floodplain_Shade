plotfolderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\"
source("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\r_codes\\functions\\loadStructureData.R")

soil_2.0m_structure_shady_100 <- loadStructureData("soil_2.0m", "shady", "100")

xs <- soil_2.0m_structure_shady_100[,2,,"X"]
zs <- soil_2.0m_structure_shady_100[,2,,"Z"]


png(paste0(plotfolderpath, "ModelGeomAndGrid.png"),
    width = 1000*5,
    height = 900*5,
    res=72*5)
par(bty = "l",
    mar = c(2,5,4,1),
    oma = c(5,0,0,0),
    yaxt = "s",
    cex.axis = 1.5,
    cex.lab = 1.5,
    cex.main= 2,
    mfrow = c(2,1))
plot(c(-110,1110), c(-1,41), 
     type ="n",
     xlab = "",
     ylab = "Z dimension (m)",
     main = "Model Geometry")
segments(0,10,1000,0, lwd = 2)
segments(0,35,1000,25, lwd = 2)
segments(0,38,1000,28, lwd = 2)
segments(0,40,1000,30, lty= 2, lwd = 2)
segments(0,40,0,10, lwd = 2)
segments(1000,30,1000,0, lwd = 2)
points(200,36.8, pch = 6, cex = 1.3, lwd = 2)
text(500,20,"Bedrock", srt = 353.5, cex = 1.5)
text(500,31.5,"Hyporheic Zone", srt = 353.5, cex = 1.5)
text(500,34,"Unsaturated Sediment", srt = 353.5, cex = 0.9)
#text(6,36.6,"{", pos= 2, srt = 359, cex = 2.3)
segments(-20, 36.8, 30, 36.3, lwd =3, lend = 2, col = "gray45")
segments(30, 36.3, 20,37, lwd = 3, lend = 2, col = "gray45")
segments(30, 36.3, 20,35.6, lwd = 3, lend = 2, col = "gray45")

segments(980, 26.8, 1030, 26.3, lwd = 3, lend = 2, col = "gray45")
segments(1030, 26.3, 1020, 27, lwd =3, lend = 2, col = "gray45")
segments(1030, 26.3, 1020, 25.6, lwd = 3, lend = 2, col = "gray45")

text(-70,36.8, "River\nDown-\nwelling", cex = 1.3, col = "gray45",
     font = 3)
text(1080,25.4, "Free\nDrainage", cex = 1.3, col = "gray45",
     font = 3)

plot(as.vector(xs),
     as.vector(zs),
     pch = ".",
     xlim = c(-110,1110),
     ylim = c(-1,41),
     bty = "L",
     ylab = "Z-Dimension (m)",
     xlab = "",
     main = "Model Nodes",
     col = "black")

mtext("X-Dimension (m)",
      side = 1,
      outer = T,
      line = 2, cex = 1.5)

dev.off()