source("scripts/packages.R")

# bunny gps data ----------------------------------------------------------
gps_allyear <- readRDS("input/gps_allyear.rds")
gpsKL <- gps_allyear[grid == "Kloo" & yr == 2017, c("grid", "yr", "mnth", "x_proj", "y_proj", "datetime", "id")]
gpsKL[, c("date", "time") := list(as.Date(datetime), format(as.POSIXct(datetime), "%H:%M:%S"))]
gpsKL[, datetime := NULL]

# Convert 'time' column to time object
gpsKL$time <- as_hms(gpsKL$time)

# Filter the data table
gpsKL <- gpsKL[between(time, as_hms("11:00:00"), as_hms("15:00:00"))]


gpsKL[, id := as.factor(id)]
gpsKL <- gpsKL[, .(id, x_proj, y_proj)]


# Read your data into a sf object (assuming it's already in EPSG 32608)
gpsKL_sf <- st_as_sf(gpsKL, coords = c("x_proj", "y_proj"), crs = 32608)

# Transform the coordinates to EPSG 3154 (UTM zone 7N)
gpsKL_sf <- st_transform(gpsKL_sf, crs = 3154)

# MCP to calculate home range ------------------------------------------------------------

# Test on bunny 22109
testbun <- gpsKL_sf 
  #filter(animal == 22109)

# allbuns <- subset(gpsKL_sf, select = -c(grid, yr, mnth, date, time))
#st_crs(testbun) <- 3154


# Not necessary, needs to be a spatial object. 
  # testbun <- as.data.table(testbun)
  # testbun[, c("x_coordinate", "y_coordinate") := tstrsplit(geometry, " ")]
  # testbun[, x_coordinate := as.numeric(gsub("[^0-9.]", "", x_coordinate))]
  # testbun[, y_coordinate := as.numeric(gsub("[^0-9.]", "", y_coordinate))]
  # testbun[, geometry := NULL]

# Extract coordinates from sf object
coords <- st_coordinates(testbun)

# Create SpatialPoints object
points <- SpatialPoints(coords[, c("X", "Y")])

# Perform MCP analysis
testbun_mcp <- mcp(points)

# Clip the MCP to KL grid
  # clipped_mcp <- st_intersection(concave_polygon, testbun_mcp_sf)
  # clipped_mcp <- clipped_mcp %>%
    # select(-id, -area)


# Used vs available -------------------------------------------------------

# Convert testbun_mcp to sf object
testbun_mcp_sf <- st_as_sf(testbun_mcp)
st_crs(testbun_mcp_sf) <- 3154
  # clipped_mcp_sf <- st_as_sf(clipped_mcp)

# Generate stratified random points within the home range polygon
set.seed(123)  # for reproducibility
# using regular since middens are small, this way I know I will capture middens in this.
available_points <- spsample(testbun_mcp, 1000, type = "regular")
# available_points <- st_sample(clipped_mcp, 1000, type = "regular")

# Convert available_points to sf object
available_points_sf <- st_as_sf(available_points)
st_crs(available_points_sf) <- 3154

# Filter available points falling within the home range polygon
available_within <- st_intersection(available_points_sf, testbun_mcp_sf)
  # available_within <- st_intersection(available_points_sf, clipped_mcp_sf)

# Filter GPS points within the home range polygon
used_within <- st_intersection(testbun, testbun_mcp_sf)
# used_within <- st_intersection(testbun, clipped_mcp_sf)

source("scripts/02-make_midden_raster.R")
# Filter for middens within this bun's HR and within KL
HR_middenbuffers <- st_intersection(KLmiddens_buffered, testbun_mcp_sf)
  # HR_middenbuffers <- st_intersection(KLmiddens_buffered, clipped_mcp_sf)

# Make sure all layers are under the same projection
st_crs(available_within) <- 3154
st_crs(used_within) <- 3154

# Clip the MCP and 'available' points to KL grid
clipped_mcp <- st_intersection(concave_polygon, testbun_mcp_sf)
available_clipped <- st_intersection(concave_polygon, available_within)
used_clipped <- st_intersection(concave_polygon, used_within)

# # Remove 'id' and 'area' columns
# clipped_mcp <- clipped_mcp %>%
#   select(-id, -area)
# 
# used_clipped <- used_clipped %>%
#   select(-id, -area)
# 
# HR_middenbuffers <- HR_middenbuffers %>%
#   select(-id, -area)

# Visualize

# Convert spatial objects to data frames
available_df <- as.data.frame(st_coordinates(available_clipped))
mcp_df <- as.data.frame(st_coordinates(clipped_mcp))
used_df <- as.data.frame(st_coordinates(used_clipped))
midden_df <- as.data.frame(st_coordinates(HR_middenbuffers))

ggplot() +
  geom_point(data = available_df, aes(x = X, y = Y), color = "blue") +
  geom_polygon(data = mcp_df, aes(x = X, y = Y), fill = "red") +
  geom_point(data = used_df, aes(x = X, y = Y), color = "green") +
  geom_sf(data = HR_middenbuffers, fill = NA, color = "purple") +
  theme_void()

 # Midden status -----------------------------------------------------------

# Convert HR_middenbuffers to a raster
# Create an empty raster layer with the desired extent and resolution
empty_raster <- raster(extent(HR_middenbuffers), crs = 3154, ncols = 3000, nrows = 3000)

# Rasterize HR_middenbuffers and project to CRS 3154
HR_midden_raster <- rasterize(HR_middenbuffers, empty_raster, field = 1)

# Extract values from HR_midden_raster for points in available_clipped & used_clipped
available_clipped$midden <- ifelse(!is.na(raster::extract(HR_midden_raster, available_clipped)), "yes", "no")
used_clipped$midden <- ifelse(!is.na(raster::extract(HR_midden_raster, used_clipped)), "yes", "no")

# Double-check numbers
print(table(available_clipped$midden))
print(table(used_clipped$midden))

# Add 'status' column to used_clipped
used_clipped$status <- "used"

# Add 'status' column to available_clipped
available_clipped$status <- "available"

# Remove extra columns
available_clipped <- available_clipped %>%
  select(-id, -area)

# Combine used_clipped and available_clipped
testbunrsf <- rbind(used_clipped, available_clipped)

# Reorder the columns
testbunrsf <- testbunrsf %>%
  select("status", "midden", "polygons")




