##
## LEARN SPATIAL INTERPOLATION & KRIGING
##

#### TUTORIAL 1: https://mgimond.github.io/Spatial/interpolation-in-r.html #####
library(rgdal)
library(tmap)

# Load precipitation data
z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/precip.rds"))
P <- readRDS(z)

# Load Texas boudary map
z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
W <- readRDS(z)

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="Precip_in", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)


crs(newspat) <- "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
crs(spat) <- "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"



tm_shape(newspat) + tm_polygons()



####### Try spatial interp #########
library(sp)
library(gstat)
spat <- as.data.frame(g[[model2plot]][,2,,1,c("X", "Z", "temp")])
class(spat) <- "data.frame"
spat0 <- spat[,-1]

latdd <- 45.000
longdd <- -112.000

spat$X <- spat$X + longdd
spat$Z <- spat$Z + latdd

coordinates(spat) <- ~ X + Z
class(spat)

crs(spat) <- "+proj=utm +zone=12 +datum=NAD83 +units=m"
str(spat)
#plot(spat)


# Now I need to make my own coordinate values
z0s <- round(subset(spat0, X == 0)$Z, 3)
z1000s <- round(subset(spat0, X == 1000)$Z, 3)
layers <- data.frame(X = rep(seq(0, 1000, by = 0.1), 67),
                     Z = seq(10, 0, by = -0.001))
zlist <- tapply(z1000s, 
                list(1:67), 
                function(x)
                  seq(x+10, x, by = -0.001))
layers$Z <- unlist(zlist)
layers$Z <- round(layers$Z,3)
## z values at X match to when rounded to 1 decimal place
subset(layers, X == 0)$Z == round(subset(spat0, X == 0)$Z,3)

# Fill with values
spatlrg <- cbind(layers, data.frame(temp = NA))
spat0$Z <- round(spat0$Z, 3)

newspat <- merge(spatlrg, spat0, all.x = T, by = c("X", "Z"))
head(newspat) #look
subset(newspat, is.na(temp.y)) # check to see where the NAs at

newspat <- newspat[,-3]
colnames(newspat) <- c("X", "Z", "temp")
head(newspat)

newspat$X <- newspat$X + longdd
newspat$Z <- newspat$Z + latdd

coordinates(newspat) <- ~ X + Z
class(newspat)

crs(newspat) <- "+proj=utm +zone=12 +datum=NAD83 +units=m"
str(newspat)


## NOW DO KRIGING
# Variogram
spat.vgm <- variogram(temp ~ X + Z, data = spat)
spat.fit <- fit.variogram(spat.vgm, model=vgm(7,"Wav", nugget = 5))
spat.fit
plot(spat.vgm, spat.fit)
# kinda working

# Stopped mid-calc....taking forever
spat.kriged <- krige(temp ~ X + Z, spat, newspat, model = spat.fit)

##### Try something else: rasters #####
library(gstat)
library(raster)
library(dismo)
library(deldir)
library(rgeos)
### SEE: https://rspatial.org/raster/analysis/4-interpolation.html

spardf <- as.data.frame(g[[model2plot]][,2,,1,c("X", "Z", "temp")])
class(spardf) <- "data.frame"
spardf <- spardf[,-1]

# 'spar' is the SpatialPointsDataFrame
spar <- spardf
coordinates(spar) <- ~X+Z

# Make the points into a raster, there are blanks where there is no data in the intersect
blankrast <- raster(ncols = 500, nrows = 40, xmn = 0, xmx = 1000, ymn = 0, ymx = 40)
rasttest <- rasterize(spar, blankrast, field = "temp", fun = mean)
plot(rasttest,
     xlim = c(1,200))          
image(rasttest)


# Just the Bedrock
bedrock <- as.data.frame(g[[model2plot]][,2,1:16,1,c("X", "Z", "temp")])
class(bedrock) <- "data.frame"
bedrock <- bedrock[,-1]
coordinates(bedrock) <- ~X+Z
bedrockblank <- raster(ncols = 1000, nrows = 50, xmn = 0, xmx = 1000, ymn = 0, ymx = 35)
bed <- rasterize(bedrock, bedrockblank, field = "temp", fun = mean)
plot(bed,
     xlim = c(1,200))
bv <- voronoi(bedrock)
plot(bv, xlim = c(1,100))

#spplot(bv, 'temp', col.regions=rev(get_col_regions()))

bedrework <- rasterize(bv, bedrockblank, 'temp')
plot(bedrework, xlim = c(0,200))
image(bedrework)

# alluvium <- as.data.frame(g[[model2plot]][42:496,2,17:67,1,c("X", "Z", "temp")])
# class(alluvium) <- "data.frame"
# alluvium <- alluvium[-1]
# coordinates(alluvium) <- ~X+Z
# alluviumblank <- raster(ncols = 1000, nrows = 50, xmn = 0, xmx = 1000, ymn = 25, ymx = 40)

# Do voronoi intersection for all points
voro <- voronoi(spar)
polygon1 <- readWKT("POLYGON((0 10, 1000 0, 1000 30, 0 40, 0 10))") 
vorocut <- intersect(voro, polygon1)
allblank <- raster(ncols = 2000, nrows = 500, xmn = 0, xmx = 1000, ymn = 0, ymx = 40)
allrework <- rasterize(vorocut, blankrast, "temp")
image(allrework, col = hcl.colors(30, palette = "LaJolla"))
plot(allrework, xlim = c(0,100), ylim = c(9,41), asp = NA)

# Nearest neighbor interpolation. This looks the best
gs <- gstat(formula=temp~1, locations=spar, nmax=5, set=list(idp = 0))
nn <- interpolate(allblank, gs)
nnmsk <- mask(nn, polygon1) # cut to be the model polygon
#plot(nnmsk)
image(nnmsk)
abline()

image(nnmsk, ylim = c(34,40))

### Alluvium only
alluviumpolygon <- readWKT("POLYGON((0 35, 1000 25, 1000 30, 0 40, 0 35))") 
alluvium <- mask(nnmsk,alluviumpolygon)
image(alluvium, ylim = c(24,40))
abline(a = 38,
       b = -(1/100),
       col = "black",
       lty = 3)

### Alluvium and 5m of Bedrock
alluvium_5mbedrock_polygon <- readWKT("POLYGON((0 30, 1000 20, 1000 30, 0 40, 0 30))") 
all_5mbed <- mask(nnmsk, alluvium_5mbedrock_polygon)
image(all_5mbed, ylim = c(20,40))
abline(a = 38,
       b = -(1/100),
       col = "black",
       lty = 3)
abline(a = 35,
       b = -(1/100),
       col = "black",
       lty = 3)


# image(x = 1:496, 
#       y = 1:67,
#       z = g[[model2plot]][,2,,1,"temp"])



# # inverse distance weight -- ## NO GOOD
# gs2 <- gstat(formula=temp~1, locations=spar)
# idw <- interpolate(allblank, gs2)
# ## [inverse distance weighted interpolation]
# idwr <- mask(idw, polygon1)
# image(idwr)


