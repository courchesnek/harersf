
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped GPS data
gps <- readRDS("Data/all_gps.rds")

gps <- gps[yr == 2017 & grid == "Kloo"]
gps[, id := as.factor(id)]
gps <- gps[, .(id, x_proj, y_proj)]

coordinates(gps) <- c("x_proj", "y_proj")
proj4string(gps) <- utm7N
mcptest <- mcp(gps)





