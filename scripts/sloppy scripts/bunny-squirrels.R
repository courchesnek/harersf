# Connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

# Connection to krsp_suppl database
con3 <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                      dbname ="krsp_suppl",
                      username = Sys.getenv("krsp_user"),
                      password = Sys.getenv("krsp_password")
)


# grid stake data ---------------------------------------------------------

grid_stakes <- collect(tbl(con3, "grid_stakes"))
class(grid_stakes)
grid_stakes <- as.data.table(grid_stakes)
class(grid_stakes$grid)
KLstakes <- grid_stakes[grid == "KL"]
KLstakes <- KLstakes[, .(stake, north, west)]
class(KLstakes$west)

# midden reflo data -------------------------------------------------------

dbamidden <- collect(tbl(con, "dbamidden"))
class(dbamidden)
dbamidden <- as.data.table(dbamidden)
KLmiddens <- dbamidden[grid == "KL"]
class(KLmiddens$year)

KLmiddens[, date := ymd(date)]

KLmiddens[, year := year(date)]

KLmiddens <- KLmiddens[, .(date, reflo, locX, locY)]

class(KLmiddens$locY)

# UNECESSARILY LONG : KLmiddens[, c("locX", "locY") := lapply(.SD, as.numeric), .SDcols = c("locX", "locY")]

KLmiddens[, c("locX", "locY") := .(as.numeric(locX), as.numeric(locY))]

# Round to 1 sig dig
meanloc <- KLmiddens[, .(mean_locX = round(mean(locX), 1), mean_locY = round(mean(locY), 1)), by = reflo]

min_max_years <- KLmiddens[, .(minYear = min(year), maxYear = max(year)), by = reflo]

KLmiddenlocs <- merge(meanloc, min_max_years, by = "reflo")
KLmiddenlocs <- na.omit(KLmiddenlocs)

# bunny gps data ----------------------------------------------------------

gps_allyear <- readRDS("input/gps_allyear.rds")
gpsKL <- gps_allyear[grid == "Kloo" & yr == 2017, c("grid", "yr", "mnth", "x_proj", "y_proj", "datetime", "animal")]

gpsKL[, c("date", "time") := list(as.Date(datetime), format(as.POSIXct(datetime), "%H:%M:%S"))]
gpsKL[, datetime := NULL]
class(gpsKL$time)

# Convert 'time' column to time object
gpsKL$time <- as_hms(gpsKL$time)

# Filter the data table
gpsKL <- gpsKL[between(time, as_hms("11:00:00"), as_hms("15:00:00"))]

# Read your data into a sf object (assuming it's already in EPSG 32608)
gpsKL_sf <- st_as_sf(gpsKL, coords = c("x_proj", "y_proj"), crs = 32608)

# Transform the coordinates to EPSG 3154 (UTM zone 7N)
gpsKL_sf <- st_transform(gpsKL_sf, crs = 3154)


# Visualize bunny data ----------------------------------------------------

# Convert mnth to factor
gpsKL_sf$mnth <- factor(gpsKL_sf$mnth)

# Define colors for each month
month_colors <- c("red", "blue", "green", "yellow", "orange", "purple",
                  "cyan", "magenta", "brown", "pink", "gray", "turquoise")

# Plot with manual color scale
ggplot() +
  theme_void() +
  geom_sf(data = gpsKL_sf, aes(color = mnth)) +
  scale_color_manual(name = "Month", values = month_colors)


# Average location per bunny per day --------------------------------------

# Create buffer around all points
buffered_gpsKL <- gpsKL_sf %>%
  st_buffer(dist = 10)

# Filter for points only within KL grid
points_within_polygon <- buffered_gpsKL %>%
  st_intersection(concave_polygon_sf)

# Group by animal, month and date
grouped_buffers <- points_within_polygon %>%
  st_set_agr("geometry") %>%
  st_cast("POLYGON") %>%
  st_collection_extract() %>%
  st_sf() %>%
  st_set_agr("geometry") %>%
  group_by(animal, mnth, date) %>%
  summarize()

class(grouped_buffers)

# Convert mnth to factor
grouped_buffers$mnth <- factor(grouped_buffers$mnth)

# Define colors for each month
month_colors <- c("red", "blue", "green", "yellow", "orange", "purple",
                  "cyan", "magenta", "brown", "pink", "gray", "turquoise")

# Plot with manual color scale
ggplot() +
  geom_polygon(data = grid_df, aes(x = X, y = Y), color = "black", fill = NA) +
  theme_void() +
  geom_sf(data = grouped_buffers, aes(color = mnth)) +
  scale_color_manual(name = "Month", values = month_colors)

