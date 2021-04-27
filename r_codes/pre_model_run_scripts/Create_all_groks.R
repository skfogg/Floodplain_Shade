library(stringr)


fileCon <- "C:/Users/skati/Desktop/Floodplain_Shade_inputDocs/k100_0.5_riveronly/k100_0.5_riveronly.grok" 
test <- readLines(fileCon, n = -1)

whichk <- c("k100", "k400")
x0_zelevation <- rep(c("0.0 48.5", "0.0 49.0", "0.0 50.0", "0.0 51.0"), times = 2)
x2000_zelevation <- rep(c("2000.0 28.5", "2000.0 29.0", "2000.0 30.0", "2000.0 31.0"), times = 2)
min_thickness <- rep(c("3.5", "4.0", "5.0", "6.0"), times = 2)
uniform_sublayering <- rep(c("35.0", "40.0", "50.0", "60.0"), times = 2)
soil_thickness <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 2)

test_df <- data.frame(line = 1:length(test), string = test)

for(k in 1:2){

## ---- HYDROLOGY MATERIALS ---- ##
## Note this shouldn't change for k = 1
  hydro_materials_s <- test_df[complete.cases(str_extract(test, "Porous Media Properties")),]$line
  hydro_materials_e <- test_df[complete.cases(str_extract(test, "Initial Conditions$")),]$line
  
test[hydro_materials_s:hydro_materials_e] <- str_replace_all(
  test[hydro_materials_s:hydro_materials_e], 
  "properties/hydrology_materials_k100.mprops", paste0("properties/hydrology_materials_", 
                                                       whichk[k],
                                                       ".mprops"))

## ---- SOIL THICKNESS ---- ##
## ORDER ALWAYS 0.5 -> 1.0 -> 2.0 -> 3.0

alluvium_layer_s <- test_df[complete.cases(str_extract(test, "alluvium")),]$line
alluvium_layer_e <- test_df[complete.cases(str_extract(test, "end grid generation")),]$line

alluvium_grid_block <- test[alluvium_layer_s:alluvium_layer_e] #str_remove_all(test[58:70], "\t")

for (i in 2:8){
alluvium_grid_block <- str_replace(alluvium_grid_block, x0_zelevation[i-1], x0_zelevation[i])
alluvium_grid_block <- str_replace(alluvium_grid_block, x2000_zelevation[i-1], x2000_zelevation[i])
alluvium_grid_block <- str_replace(alluvium_grid_block, min_thickness[i-1], min_thickness[i])
alluvium_grid_block <- str_replace(alluvium_grid_block, uniform_sublayering[i-1], uniform_sublayering[i])

test[alluvium_layer_s:alluvium_layer_e] <- alluvium_grid_block
soilThickness <- as.double(min_thickness[i], length = 2)-3.0

## ---- INITAL HEADS ---- ##
  initial_head_s <- test_df[complete.cases(str_extract(test, "Initial Conditions$")),]$line
  initial_head_e <- test_df[complete.cases(str_extract(test, "Overland Flow Boundary Conditions")),]$line

  initial_head_block <- test[initial_head_s:initial_head_e]
  initial_head_block <- str_replace(initial_head_block, paste0("restartheads_", soil_thickness[i-1]), paste0("restartheads_", soil_thickness[i]))
  test[initial_head_s:initial_head_e] <- initial_head_block
  
    
##############################
#### SAVE RIVERONLY MODEL ####
##############################
writeLines(test, paste0("C:/Users/skati/Desktop/Floodplain_Shade_inputDocs/", whichk[k], "_", sprintf("%.1f", soilThickness), "_riveronly/" , whichk[k], "_", sprintf("%.1f", soilThickness), "_riveronly.grok"))

## ---- SOIL BOUNDARY CONDITIONS ---- ##
## ORDER ALWAYS riveronly -> shady -> sunny then go back to riveronly

  soil_boundary_s <- test_df[complete.cases(str_extract(test, "Soil Concentration")),]$line
  soil_boundary_e <- test_df[complete.cases(str_extract(test, "Time Step Control")),]$line
  
  soil_bc_block <- test[soil_boundary_s:soil_boundary_e]

## (1) uncomment soil concentration bc:
t = c("!clear chosen nodes", "!choose nodes top", "!specified concentration")
r = c("clear chosen nodes", "choose nodes top", "specified concentration")
for(i in 1:3){
  soil_bc_block <- str_replace(soil_bc_block, t[i], r[i])
}

## (2) uncomment shady bc
soil_bc_block <- str_replace(soil_bc_block, "!include bcs/shady.txt", "include bcs/shady.txt")

test[soil_boundary_s:soil_boundary_e] <- soil_bc_block

############################
##### SAVE SHADY MODEL #####
############################
writeLines(test, paste0("C:/Users/skati/Desktop/Floodplain_Shade_inputDocs/", whichk[k],"_", sprintf("%.1f", soilThickness),"_shady/", whichk[k],"_", sprintf("%.1f", soilThickness),"_shady.grok"))

## (3) Re-comment shady bc
test <- str_replace(test, "include bcs/shady.txt", "!include bcs/shady.txt")

## (4) uncomment sunny bc
test <- str_replace(test, "!include bcs/sunny.txt", "include bcs/sunny.txt")

##########################
#### SAVE SUNNY MODEL ####
##########################
writeLines(test, paste0("C:/Users/skati/Desktop/Floodplain_Shade_inputDocs/", whichk[k], "_", sprintf("%.1f", soilThickness),"_sunny/", whichk[k], "_", sprintf("%.1f", soilThickness),"_sunny.grok"))

## (5) Re-comment sunny bc
test <- str_replace(test, "include bcs/sunny.txt", "!include bcs/sunny.txt")

## (5) Re-comment soil bc block
for(i in 1:3){
  soil_bc_block <- str_replace(soil_bc_block, r[i], t[i])
}
test[soil_boundary_s:soil_boundary_e] <- soil_bc_block

#### GO BACK TO RIVERONLY #####
}
}
