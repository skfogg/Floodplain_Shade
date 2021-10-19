boxdir <- "C:/Users/t24x137/Box/Floodplain_Shade_Box/"
k100_v <- readRDS(paste0(boxdir, "meacham_results/K_100m_day/shady/soil_0.5m/k100_0.5_shady_velocity.RData"))
k400_v <- readRDS(paste0(boxdir, "meacham_results/K_400m_day/shady/soil_0.5m/k400_0.5_shady_velocity.RData"))

dim(k100_v)
mean(k100_v[,1,aquifer_z_idx,1,"Vx"])*86400
mean(k400_v[,1,aquifer_z_idx,1,"Vx"])*86400

