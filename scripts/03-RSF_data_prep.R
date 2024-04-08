
#load packages
source("scripts/packages.R")



# read in data -----------------------------------------------------------

middens <- raster("output/midden_raster.tif")
kloo <- read_sf("output/kloo_polygon.shp")
gps_allyear <- readRDS("input/gps_allyear.rds")


# bunny gps data ----------------------------------------------------------

#cut gps data to kloo, year, etc
gpsKL <- gps_allyear[grid == "Kloo" & yr == 2017, c("grid", "yr", "mnth", "x_proj", "y_proj", "datetime", "id")]
gpsKL[, c("date", "time") := list(as.Date(datetime), format(as.POSIXct(datetime), "%H:%M:%S"))]
gpsKL[, datetime := NULL]

# Convert 'time' column to time object
gpsKL$time <- as_hms(gpsKL$time)

# Filter the data table
gpsKL <- gpsKL[between(time, as_hms("11:00:00"), as_hms("15:00:00"))]

#make id a factor
gpsKL[, id := as.factor(id)]
gpsKL <- gpsKL[, .(id, x_proj, y_proj)]


# SF conversion -----------------------------------------------------------

# Read your data into a sf object (assuming it's already in EPSG 32608)
gpsKL_sf <- st_as_sf(gpsKL, coords = c("x_proj", "y_proj"), crs = 32608)
# Transform the coordinates to EPSG 3154 (UTM zone 7N)
gpsKL_sf <- st_transform(gpsKL_sf, crs = 3154)

# SP conversion -----------------------------------------------------------

#convert gps data to spatial data
coordinates(gpsKL) <- c("x_proj", "y_proj")
proj4string(gpsKL) <- CRS("+proj=utm +zone=7 +datum=WGS84 +units=m +no_defs")



# MCPs --------------------------------------------------------------------

# Perform MCP analysis. This is on the SP file
mcps <- mcp(gpsKL)
plot(mcps)



# create available points -------------------------------------------------

#turn that into a function that will run on just an argument for ID
#note for thisI just used 500 individuals
getavail <- function(id){
  locs <- spsample(mcps[mcps$id == id,], n = 500, type = "regular")
  out <- as.data.frame(locs)
  out$id <- id
  out$status <- "available"
  return(out)
}

#now create the list of individuals
ids <- as.list(mcps$id)

#lapply the custom function to the list of individuals
avail <- lapply(ids, getavail)
#the output will be a list of dataframes
#rbindlist all those dataframes together if you like
avail <- rbindlist(avail)

setnames(avail, c("x1", "x2"), c("x_proj", "y_proj"))

#plot in ggplot just to confirm it worked
ggplot(avail)+
  geom_point(aes(x = x_proj, y = y_proj, color = id))

# Read your data into a sf object (assuming it's already in EPSG 32608)
avail_sf <- st_as_sf(avail, coords = c("x_proj", "y_proj"), crs = 32608)

# Transform the coordinates to EPSG 3154 (UTM zone 7N)
avail_sf <- st_transform(avail_sf, crs = 3154)



# get used points only within home ranges -------------------------------------------------------

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

setnames(used, c("x", "y"), c("x_proj", "y_proj"))

#plot in ggplot just to confirm it worked
ggplot(used)+
  geom_point(aes(x = x_proj, y = y_proj, color = id))

# Read your data into a sf object (assuming it's already in EPSG 32608)
used_sf <- st_as_sf(used, coords = c("x_proj", "y_proj"), crs = 32608)

# Transform the coordinates to EPSG 3154 (UTM zone 7N)
used_sf <- st_transform(used_sf, crs = 3154)



# extract from raster -----------------------------------------------------

# Extract values for available points
avail_sf$midden <- ifelse(!is.na(raster::extract(middens, avail_sf)), "yes", "no")

# Extract values from HR_midden_raster using SF data
used_sf$midden <- ifelse(!is.na(raster::extract(middens, used_sf)), "yes", "no")



# clip final data to the grid polygon -------------------------------------

#clip used and available to the kloo grid polygon
avail_clipped <- st_intersection(kloo, avail_sf)
used_clipped <- st_intersection(kloo, used_sf)

#plot available points
plot(avail_sf)
plot(avail_clipped)

#plot used points
plot(used_sf)
plot(used_clipped)

#rbind used and available, make data.table
done <- as.data.table(rbind(avail_clipped, used_clipped))

#summary of sample size
done[, .N, by = .(id, status)]

#summary of midden
done[, .N, by = .(midden, id)]



# save --------------------------------------------------------------------

saveRDS(done, "output/ready_for_rsf.rds")


