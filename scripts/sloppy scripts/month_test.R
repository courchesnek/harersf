#load packages
source("scripts/00-packages.R")

# read in data -----------------------------------------------------------

middens <- raster("output/midden_raster.tif")
kloo <- read_sf("output/kloo_polygon.shp")
gps_allyear <- readRDS("input/gps_allyear.rds")


# bunny gps data ----------------------------------------------------------

#cut gps data to kloo, year, etc
gpsKL <- gps_allyear[grid == "Kloo" & yr == 2017, c("grid", "yr", "mnth", "x_proj", "y_proj", "datetime", "id")]
gpsKL[, c("date", "time") := list(as.Date(datetime), format(as.POSIXct(datetime), "%H:%M:%S"))]
gpsKL[, datetime := NULL]

#convert 'time' column to time object
gpsKL$time <- as_hms(gpsKL$time)

#filter the data table
gpsKL <- gpsKL[between(time, as_hms("11:00:00"), as_hms("15:00:00"))]

#make month numeric
gpsKL$mnth <- as.numeric(gpsKL$mnth)

#make id a factor
gpsKL[, id := as.factor(id)]

#create a copy of gpsKL with only 'id', 'x_proj', 'y_proj', and 'mnth'
gpsKL_for_mcp <- gpsKL[, .(id, x_proj, y_proj, mnth)]

# Assuming gpsKL_for_mcp is a data.table
setDT(gpsKL_for_mcp)

# Transform the coordinates to the desired CRS
coords <- st_as_sf(gpsKL_for_mcp, coords = c("x_proj", "y_proj"), crs = 32608)
coords_transformed <- st_transform(coords, crs = 3154)

# Define a function to compute MCP for each group
compute_mcp <- function(data) {
  sp_points <- SpatialPoints(coords = data[, c("x_proj", "y_proj")])
  mcp_result <- mcp(sp_points)
  return(list(mcp = mcp_result))
}

# Compute MCP for each ID and month
mcps <- gpsKL_for_mcp[, .(mcp = compute_mcp(.SD)), by = .(id, mnth)]

# Function to plot MCPs
plot_mcps <- function(mcps) {
  # Create a SpatialPolygons object containing all MCPs
  all_mcps <- do.call("rbind", mcps$mcp)
  
  # Create a data frame with ID and month information for each MCP
  mcps_info <- data.frame(id = rep(mcps$id, sapply(mcps$mcp, function(x) length(slot(x, "polygons")))),
                          mnth = rep(mcps$mnth, sapply(mcps$mcp, function(x) length(slot(x, "polygons")))))
  
  # Plot all MCPs together, colored by ID and month
  plot(all_mcps, col = heat.colors(length(unique(mcps_info$id))), border = "black", main = "MCPs by ID and Month")
  legend("topright", legend = unique(mcps_info$id), col = heat.colors(length(unique(mcps_info$id))), pch = 20, bty = "n")
}

plot(mcps)
