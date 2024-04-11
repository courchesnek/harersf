library(sf)
library(tidyr)
library(concaveman)
library(dplyr)
library(ggplot2)

KL_utm <- read.csv("C:/Users/Joseph/Downloads/KL_utm.csv")
cord_UTM <- read.csv("C:/Users/Joseph/Downloads/cord.UTM.csv")

# Ensure the data frame is ordered by x and y coordinates
KL_utm <- KL_utm %>% arrange(geometry)

#Remove outliers
KL_utm <- KL_utm[-c(1:3), ]

# Set geometry as a spatial feature for grids
grids <- KL_utm %>%
  separate(geometry, into = c("x", "y"), sep = "\\|") %>%
  st_as_sf(coords=c("x", "y"), crs=3154) %>%
  select(geometry)

# Create the concave hull polygon
concave_polygon <- concaveman(grids)

# Set geometry as a spatial feature for midden
midden <- cord_UTM %>%
  separate(geometry, into = c("x", "y"), sep = "\\|") %>%
  st_as_sf(coords=c("x", "y"), crs=3154) %>%
  select(geometry)

# Extract the geometry column from midden
midden_points <- st_geometry(midden)

# Convert concave hull polygon and points to data frames with correct column names
concave_df <- as.data.frame(st_coordinates(concave_polygon))
midden_df <- as.data.frame(st_coordinates(midden))

# Plot the concave hull polygon
ggplot() +
  geom_polygon(data = concave_df, aes(x = X, y = Y), color = "black", fill = NA) +
  geom_point(data = midden_df, aes(x = X, y = Y), color = "red", size = 1) +
  theme_void()
