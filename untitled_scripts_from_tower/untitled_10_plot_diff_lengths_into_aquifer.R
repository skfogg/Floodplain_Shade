
library(zoo)
library(xts)
model_1.0_xvals



x_idx <- c(1, 56, 70, 132, 307)
x_names <- c("chan", "22", "51", "175", "955")


ro = k100_1.0_riveronly
sh = k100_1.0_shady
su = k100_1.0_sunny

for(i in 1:5){
assign(paste0("ro_", x_names[i]),  
       xts(zoo(rep(colMeans(ro[aquifer_z_idx,x_idx[i],]), times = 2), 
               order.by = c(days, days+(365.25*86400)))))

assign(paste0("sh_", x_names[i]),  
       xts(zoo(rep(colMeans(sh[aquifer_z_idx,x_idx[i],]), times = 2), 
               order.by = c(days, days+(365.25*86400)))))

assign(paste0("su_", x_names[i]),  
       xts(zoo(rep(colMeans(su[aquifer_z_idx,x_idx[i],]), times = 2), 
               order.by = c(days, days+(365.25*86400)))))
}

rolist <- list(ro_22, ro_51, ro_175, ro_955)
shlist <- list(sh_22, sh_51, sh_175, sh_955)
sulist <- list(su_22, su_51, su_175, su_955)

#labellines <- paste0("Flow Path Length:", round(k400_0.5_riveronly_s[x_idx[4],1,aquifer_z_idx[15],"X"],0))

plot.zoo(ro_chan["2021-07-14/2022-07-14"],
         col = "white",
         ylab = "Temperature",
         ylim = c(2, 22),
         lwd = 3,
         main = "River-Only")
mapply(function(x, c) lines(as.zoo(x["2021-07-14/2022-07-14"]), col = c, lwd = 2),
       rolist,
       hcl.colors(5, "Mint")[1:4])

plot.zoo(ro_chan["2021-07-14/2022-07-14"],
         col = "white",
         ylab = "Temperature",
         ylim = c(2, 22),
         lwd = 3,
         main = "Shady")
mapply(function(x, c) lines(as.zoo(x["2021-07-14/2022-07-14"]), col = c, lwd = 2),
       shlist,
       hcl.colors(4, "Heat"))

plot.zoo(ro_chan["2021-07-14/2022-07-14"],
         col = "white",
         ylab = "Temperature",
         ylim = c(2, 22),
         lwd = 3,
         main = "Sunny")
mapply(function(x, c) lines(as.zoo(x["2021-07-14/2022-07-14"]), col = c, lwd = 2),
       sulist,
       hcl.colors(4, "YlOrRd"))


lines(as.zoo(k400_0.5_sh_x["2021-07-14/2022-07-14"]), col = "brown")
lines(as.zoo(k400_0.5_su_x["2021-07-14/2022-07-14"]), col = "gold")


