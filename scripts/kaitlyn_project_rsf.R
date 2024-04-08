
#source the R folder to load any packages and functions
#lapply(dir('R', '*.R', full.names = TRUE), source)
source("scripts/packages.R")

#read in prepped GPS data
gps <- readRDS("input/all_gps.rds")
middens <- raster("output/midden_raster.tif")

#prep gps data before converting to spatial data
gps <- gps[yr == 2017 & grid == "Kloo"]
gps[, id := as.factor(id)]
gps <- gps[, .(id, x_proj, y_proj)]

#convert gps data to spatial data
coordinates(gps) <- c("x_proj", "y_proj")
proj4string(gps) <- CRS("+proj=utm +zone=7 +datum=WGS84 +units=m +no_defs")
#proj4string(gps) <- crs = 3154


#create mcps, it automatically runs by the id column in gps
#mcps output is a list of mcps essentially
mcps <- mcp(gps)
plot(mcps)
mcps

#try out the spsample function. SEE HOW IT WORKS
#this line runs the spsample function on just one individual "22130"
test <- spsample(mcps[mcps$id == 22130,], n = 1000, type = "regular")
#crop for only points within KL grid 
#cropped <- over(test, grid)
#cropped <- cropped[!is.na(cropped$id), ]
#try out extract
test2 <- st_as_sf(test, crs = 3154)

raster2 <- 

test2$midden <- ifelse(!is.na(raster::extract(midden_raster, test2)), "yes", "no")
#now convert to a data frame
out <- as.data.frame(test)
#create an id column and a status column
out$id <- 22130
#create available column
out$status <- "available"


#turn that into a function that will run on just an argument for ID
#note for thisI just used 500 individuals
getavail <- function(id){
  locs <- spsample(mcps[mcps$id == id,], n = 500, type = "regular")
  out <- as.data.frame(locs)
  out$id <- id
  out$status <- "available"
  return(out)
}

#test the function on one bunny
onebun <- getavail(24553)

#now create the list of individuals
ids <- as.list(mcps$id)

#lapply the custom function to the list of individuals
final <- lapply(ids, getavail)
#the output will be a list of dataframes
#rbindlist all those dataframes together if you like
final <- rbindlist(final)

#plot in ggplot just to confirm it worked
ggplot(final)+
  geom_point(aes(x = x1, y = x2, color = id))

#test used points
#pull mcp of bun 22130 from mcps
onemcp <- mcps[mcps$id == 22130,]
#pull gps data of bun 22130 from gpsKL
onegps <- gpsKL[gpsKL$id == 22130,]
#this line crops any gps points for only those that fall within the HR
usedpoints <- crop(onegps, onemcp)
#now convert to a data frame
out2 <- as.data.frame(usedpoints)
#create id column and status column
out2$id <- 22130
out2$status <- "used"

#used points
getused <- function(id){
  mcps <- mcps[mcps$id == id,]
  gps <- gpsKL[gpsKL$id == id,]
  locs <- crop(gps, mcps)
  out <- as.data.frame(locs)
  out$id <- id
  out$status <- "used"
  return(out)
}

#lapply the custom function to the list of individuals
used <- lapply(ids, getused)
#the output will be a list of dataframes
#rbindlist all those dataframes together
used <- rbindlist(used)

# Convert KLmiddens_buffered to a raster
# Create an empty raster layer with the desired extent and resolution
empty_raster <- raster(extent(KLmiddens_buffered), crs = CRS("+proj=utm +zone=7 +datum=WGS84 +units=m +no_defs"))

# Rasterize KLmiddens_buffered
midden_raster <- rasterize(KLmiddens_buffered, empty_raster, field = 1)

