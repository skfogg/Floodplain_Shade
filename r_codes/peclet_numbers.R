## 
## PECLET NUMBER CALCS
##

# porosity
p = 0.25

# characteristic length (D50 from scott)
L = 0.059

# thermal conductivity
k_s = 1.6
k_w = 0.5
k = (k_s*(1-p)) + (k_w*p)

# density
rho_s = 3000
rho_w = 1000
rho = (rho_s*(1-p)) + (rho_w*p)

# speific heat capacity
c_s = 870
c_w = 4185
c = (c_s*(1-p)) + (c_w*p)

# thermal diffusivity
alpha = k/(rho*c)

# local flow velocity
v_400 <- readRDS("C:\\Users\\skati\\Box\\Floodplain_Shade_Box\\meacham_updated_results\\K_400m_day\\riveronly\\soil_1.0m\\k400_1.0_riveronly_velocity.RData")
v_100 <- readRDS("C:\\Users\\skati\\Box\\Floodplain_Shade_Box\\meacham_updated_results\\K_100m_day\\riveronly\\soil_1.0m\\k100_1.0_riveronly_velocity.RData")


mu_400 = mean(v_400[,,45,,"Vx"])
mu_100 = mean(v_100[,,45,,"Vx"])

## Thermal Peclet
(Pe_400 = (L*mu_400)/alpha)
(Pe_100 = (L*mu_100)/alpha)

## Anderson, 2005's Thermal Pe
(rho_w*c_w*(mu_400*p)*L)/k
(rho_w*c_w*(mu_100*p)*L)/k


