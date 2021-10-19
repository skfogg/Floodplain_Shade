aquifer_z_idx <- 17:47
t_idx <- seq(2,24,5)

z2plot <- c(aquifer_z_idx, aquifer_z_idx[31]+1:10)
x2plot <- 95
t2plot <- 2


png(paste0(getwd(),"/plots/k100_shady_1.0_ztempAt100m.png"),
    width = 400*5,
    height = 500*5,
    res = 72*5)
par(cex.lab = 1.8,
    cex.main = 2,
    cex.axis = 1.5,
    mar = c(5,5,5,1),
    bg = "black",
    fg = "ghostwhite",
    col.axis = "ghostwhite",
    col.main = "ghostwhite",
    col.lab = "ghostwhite")
plot(rev(k100_1.0_shady[,x2plot,t2plot])[z2plot],
     k400_1.0_sunny_s[1,2,z2plot,"Z"],
     xlim = c(1,23),
     ylab = "Z Depth (m)",
     xlab= expression(paste("Temperature, ", degree, "C")),
     type = "n",
     yaxt = "n")
mapply(function(t,c) lines(rev(k100_1.0_shady[,x2plot,t])[z2plot],
                           k100_1.0_shady_s[1,2,z2plot,"Z"], col = c, lwd = 3),
       t_idx,
       hcl.colors(5, "RdYlGn"))
abline(h = 48, lty = 2)
axis(2, at = 45:49, labels = seq(4,0,by=-1))
dev.off()







plot(rev(k100_1.0_shady[,x2plot,t2plot])[z2plot],
     k400_1.0_sunny_s[1,2,z2plot,"Z"],
     xlim = c(0,31))
