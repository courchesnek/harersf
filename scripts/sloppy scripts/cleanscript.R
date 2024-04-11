#read in prepped GPS data
gps <- readRDS("input/all_gps.rds")

#clean data
gpsKL <- gps[grid == "Kloo" & yr == 2017, c("grid", "yr", "mnth", "x_proj", "y_proj", "datetime", "id")]

gpsKL[, c("date", "time") := list(as.Date(datetime), format(as.POSIXct(datetime), "%H:%M:%S"))]
gpsKL[, datetime := NULL]

gpsKL$time <- as_hms(gpsKL$time)

gpsKL <- gpsKL[between(time, as_hms("11:00:00"), as_hms("16:00:00"))]

gpsKL <- gpsKL[, c("id", "x_proj", "y_proj")]

gpsKL[, id := as.factor(id)]

gpsKL <- gpsKL[, .(id, x_proj, y_proj)]

coordinates(gpsKL) <- c("x_proj", "y_proj")
proj4string(gpsKL) <- CRS("+proj=utm +zone=7 +datum=WGS84 +units=m +no_defs")

#run mcp
mcps <- mcp(gpsKL)
plot(mcps)

#function for running by id
getavail <- function(id){
  locs <- spsample(mcps[mcps$id == id,], n = 1000, type = "regular")
  out <- as.data.frame(locs)
  names(out) <- c("x","y")
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

#plot in ggplot just to confirm it worked
ggplot(avail)+
  geom_point(aes(x = x1, y = x2, color = id))

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

#combine avail and used
rsf <- rbind(avail, used)




