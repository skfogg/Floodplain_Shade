##
## Vertical Influence Plots
##

shady_vd_100 <- readRDS("analysis_data/shady_vd_100.RData")
shady_vd_400 <- readRDS("analysis_data/shady_vd_400.RData")
sunny_vd_100 <- readRDS("analysis_data/sunny_vd_100.RData")
sunny_vd_400 <- readRDS("analysis_data/sunny_vd_400.RData")

co <- hcl.colors(5, "BrwnYl", rev = T)[2:5]
lw <- 1
ltype <- "o"
ylm <- c(0,425)
p <- 15
cx <- 2


###### FLIPPED ######
png("plots/point_of_vert_influence_v2.png", width = 1000*5, height = 900*5,
    res = 72*5)
par(mfrow = c(2,2),
    cex.axis = 2,
    cex.lab = 2,
    cex.main = 2,
    mar = c(6,6,4,1))
plot(shady_vd_100[,1], seq(24, 1, -1), xlim = ylm, type = ltype,
     col = co[1], main = "k = 100, Shady", lwd = lw,
     yaxt = "n",
     ylab = "Month",
     xlab = "Flow Path Length (m)",
     pch = p,
     cex = cx)
points(rev(shady_vd_100[,2]), 1:24, col = co[2], type = ltype, lwd = lw, pch = p, cex = cx)
points(rev(shady_vd_100[,3]), 1:24, col = co[3], type = ltype, lwd = lw, pch = p, cex = cx)
points(rev(shady_vd_100[,4]), 1:24, col = co[4], type = ltype, lwd = lw, pch = p, cex = cx)
axis(2, at = seq(24, 1, -4), 
     labels = c("Jan", "Mar", "May", "Jun", "Aug", "Oct"))
legend("topright", c("0.5", "1.0", "2.0", "3.0"), col = co, lty = 1, 
       pch = 15, lwd = lw, title = "Vadose Zone Thickness", cex =2)

plot(rev(sunny_vd_100[,1]), 1:24, xlim = ylm, type = ltype,
     col = co[1], main = "k = 100, Sunny", lwd = lw,
     yaxt = "n",
     ylab = "Month",
     xlab = "Flow Path Length (m)", pch = p, cex = cx)
points(rev(sunny_vd_100[,2]), 1:24, col = co[2], type = ltype, lwd = lw, pch = p, cex = cx)
points(rev(sunny_vd_100[,3]), 1:24, col = co[3], type = ltype, lwd = lw, pch = p, cex = cx)
points(rev(sunny_vd_100[,4]), 1:24, col = co[4], type = ltype, lwd = lw, pch = p, cex = cx)
axis(2, at = seq(24, 1, -4), 
     labels = c("Jan", "Mar", "May", "Jun", "Aug", "Oct"))
legend("topright", c("0.5", "1.0", "2.0", "3.0"), col = co, lty = 1, 
       pch = 15, lwd = lw, title = "Vadose Zone Thickness", cex =2)

plot(rev(shady_vd_400[,1]), 1:24, xlim = ylm, type = ltype,
     col = co[1], main = "k = 400, Shady", lwd = lw,
     yaxt = "n",
     ylab = "Month",
     xlab = "Flow Path Length (m)", pch = p, cex = cx)
points(rev(shady_vd_400[,2]), 1:24, col = co[2], type = ltype, lwd = lw, pch = p, cex = cx)
points(rev(shady_vd_400[,3]), 1:24, col = co[3], type = ltype, lwd = lw, pch = p, cex = cx)
points(rev(shady_vd_400[,4]), 1:24, col = co[4], type = ltype, lwd = lw, pch = p, cex = cx)
axis(2, at = seq(24, 1, -4), 
     labels = c("Jan", "Mar", "May", "Jun", "Aug", "Oct"))

plot(rev(sunny_vd_400[,1]), 1:24, xlim = ylm, type = ltype,
     col = co[1], main = "k = 400, Sunny", lwd = lw,
     yaxt = "n",
     ylab = "Month",
     xlab = "Flow Path Length (m)", pch = p, cex = cx)
points(rev(sunny_vd_400[,2]), 1:24, col = co[2], type = ltype, lwd = lw, pch = p, cex = cx)
points(rev(sunny_vd_400[,3]), 1:24, col = co[3], type = ltype, lwd = lw, pch = p, cex = cx)
points(rev(sunny_vd_400[,4]), 1:24, col = co[4], type = ltype, lwd = lw, pch = p, cex = cx)
axis(2, at = seq(24, 1, -4), 
     labels = c("Jan", "Mar", "May", "Jun", "Aug", "Oct"))
dev.off()


##### ANNUAL MEAN #####
(shady_vd_100_annual <- round(colMeans(shady_vd_100, na.rm = T)))
(sunny_vd_100_annual <- round(colMeans(sunny_vd_100, na.rm = T)))
(shady_vd_400_annual <- round(colMeans(shady_vd_400, na.rm = T)))
(sunny_vd_400_annual <- round(colMeans(sunny_vd_400, na.rm = T)))

shadycol <- "tan4"
sunnycol <- "orange"
par(mfrow = c(2,1))
plot(shady_vd_100_annual ~ c(0.5, 1, 2, 3), 
     type = "p", ylim = c(10, 200), main = "k = 100",
     col = shadycol,
     pch = 15)
lines(sunny_vd_100_annual ~ c(0.5, 1, 2, 3), type = "p", col = sunnycol,
      pch = 15)
legend("topleft", c("Shady", "Sunny"), pch = 15, col = c(shadycol, sunnycol))

plot(shady_vd_400_annual ~ c(0.5, 1, 2, 3), 
     type = "p", ylim = c(10, 200), main = "k = 100",
     col = shadycol,
     pch = 15)
lines(sunny_vd_400_annual ~ c(0.5, 1, 2, 3), type = "p", col = sunnycol,
      pch = 15)

##### FLIPPED ANNUAL MEAN #####
png("plots/point_of_vert_influence_annual_mean.png", width = 800*5, height = 1100*5,
    res = 5*72)
par(mfrow = c(2,1),
    cex.axis = 2,
    cex.lab = 2,
    cex.main = 2,
    mar = c(6,6,4,1))
plot(shady_vd_100_annual, c(0.5, 1, 2, 3), 
     type = "p", 
     xlim = c(0, 200), main = "k = 100",
     col = shadycol,
     pch = 15,
     ylim = c(0,3.5),
     yaxt = "n",
     ylab = "Vadose Zone Thickness",
     xlab = "Flow Path Length (m)",
     cex = 3)
lines(sunny_vd_100_annual, c(0.5, 1, 2, 3), type = "p", col = sunnycol,
      pch = 15,
      cex = 3)
legend("topright", c("Shady", "Sunny"), pch = 15, col = c(shadycol, sunnycol), cex = 3)
axis(2, at = c(0.5, 1.0, 2.0, 3.0), labels = c("0.5", "1.0", "2.0", "3.0"))

plot(shady_vd_400_annual, c(0.5, 1, 2, 3), 
     type = "p", xlim = c(0, 200), main = "k = 400",
     col = shadycol,
     pch = 15,
     ylim = c(0,3.5),
     yaxt = "n",
     ylab = "Vadose Zone Thickness",
     xlab = "Flow Path Length (m)",
     cex = 3)
lines(sunny_vd_400_annual, c(0.5, 1, 2, 3), type = "p", col = sunnycol,
      pch = 15,
      cex = 3)
axis(2, at = c(0.5, 1.0, 2.0, 3.0), labels = c("0.5", "1.0", "2.0", "3.0"))

dev.off()


