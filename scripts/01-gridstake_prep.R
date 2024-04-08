source("scripts/packages.R")

# Connection to KRSP database
con_suppl <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                           dbname ="krsp_suppl",
                           username = Sys.getenv("krsp_user"),
                           password = Sys.getenv("krsp_password")
)


# import grid stake lat long values ---------------------------------------

grid_stakes<-tbl(con_suppl, "grid_stakes") %>% 
  filter(!is.na(north),
         !is.na(west)) %>% 
  collect()

# Convert to spatial coordinates

grid_stakes$north = as.numeric(grid_stakes$north)
class(grid_stakes$north)
grid_stakes$west = as.numeric(grid_stakes$west)
grid_stakes$east = -grid_stakes$west

cord.dec = st_as_sf(grid_stakes, coords=c("east","north"), crs=4326)
KL_utm <- st_transform(cord.dec, crs=3154)

# Filter for only KL stakes and remove 'west' column
KL_utm <- KL_utm %>%
  filter(grid == 'KL') %>%
  select(-comments)

st_write(KL_utm, "Output/KL_utm.shp")
saveRDS(KL_utm, file = "output/KL_utm.rds")
fwrite(KL_utm, "output/KL_utm.csv")