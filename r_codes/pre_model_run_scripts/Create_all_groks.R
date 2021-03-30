library(stringr)


fileCon <- "C:/Users/skati/Desktop/Floodplain_Shade_inputDocs/k100_0.5_riveronly/k100_0.5_riveronly.grok" 
test <- readLines(fileCon, n = -1)

whichk <- c("k100", "k400")
x0_zelevation <- rep(c("0.0 48.5", "0.0 49.0", "0.0 50.0", "0.0 51.0"), times = 2)
x2000_zelevation <- rep(c("2000.0 28.5", "2000.0 29.0", "2000.0 30.0", "2000.0 31.0"), times = 2)
min_thickness <- rep(c("3.5", "4.0", "5.0", "6.0"), times = 2)
uniform_sublayering <- rep(c("35.0", "40.0", "50.0", "60.0"), times = 2)


for(k in 1:2){

## Change hydrology materials
## Note this shouldnt change for k = 1
test[110:130] <- str_replace_all(test[110:130], "properties/hydrology_materials_k100.mprops", paste0("properties/hydrology_materials_", whichk[k],".mprops"))
  
  
### CHANGE soil thickness
## ORDER ALWAYS 0.5 -> 1.0 -> 2.0 ->3.0

alluvium_grid_block <- test[58:70] #str_remove_all(test[58:70], "\t")

for (i in 2:8){
alluvium_grid_block <- str_replace(alluvium_grid_block, x0_zelevation[i-1], x0_zelevation[i])
alluvium_grid_block <- str_replace(alluvium_grid_block, x2000_zelevation[i-1], x2000_zelevation[i])
alluvium_grid_block <- str_replace(alluvium_grid_block, min_thickness[i-1], min_thickness[i])
alluvium_grid_block <- str_replace(alluvium_grid_block, uniform_sublayering[i-1], uniform_sublayering[i])
test[58:70] <- alluvium_grid_block
soilThickness <- as.double(min_thickness[i], length = 2)-3.0

##############################
#### SAVE RIVERONLY MODEL ####
##############################
writeLines(test, paste0("C:/Users/skati/Desktop/Floodplain_Shade_inputDocs/", whichk[k], "_", sprintf("%.1f", soilThickness), "_riveronly/" , whichk[k], "_", sprintf("%.1f", soilThickness), "_riveronly.grok"))

### CHANGE Soil boundary conditions
## ORDER ALWAYS riveronly -> shady -> sunny then go back to riveronly

## Test this block to see if it contains the correct lines
soil_bc_block <- test[924:930]
if(!any(mapply(function(s) any(str_detect(soil_bc_block, s)),
       c("Soil Concentration", "!clear chosen nodes", "!choose nodes top",
         "", "!specified concentration", "!include bcs/shady.txt", 
         "!include bcs/sunny.txt")))){
  print("Soil Boundary Condition block missing from lines 924-930")
}

## (1) uncomment soil concentration bc:
t = c("!clear chosen nodes", "!choose nodes top", "!specified concentration")
r = c("clear chosen nodes", "choose nodes top", "specified concentration")
for(i in 1:3){
  soil_bc_block <- str_replace(soil_bc_block, t[i], r[i])
}
test[924:930] <- soil_bc_block

## (2) uncomment shady bc
test <- str_replace(test, "!include bcs/shady.txt", "include bcs/shady.txt")

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
test[924:930] <- soil_bc_block

#### GO BACK TO RIVERONLY #####
}
}
