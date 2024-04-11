### Converting KRSP locations on the grid to UTMs ###
## Owner: Gladiana Spitz
## Last Updated: 03/25/2024


#load packages
source("scripts/00-packages.R")

#read in midden data
KLmiddens2017 <- read.csv("output/KLmiddens2017.csv")
KLmiddens2017$locx <- loc_to_numeric(KLmiddens2017$locx)
KLmiddens2017$locy <- loc_to_numeric(KLmiddens2017$locy)

#grid_stakes must be a table or data frame with the desired grid specified in the grid_stakes call -- 
# ie if you are interested in converting locations in both KL and SU you need two different grid_stakes tables for each grid


#x is your desired set of census or other location data with locx and locy to be converted to UTMs
#The syntax is specific for the census data, but can be adapted for other types within the function
# location columns must be locx and locx and must be numeric. I will ideally make adjustments to be less case sensitive

locs_to_utms <- function(grid_stakes, x){
  #split stake names from XY to X and Y
  split = separate(grid_stakes[1:105,],stake,c("x","y"),sep=2)
  split[105,"y"] = "0"
  split2 = separate(grid_stakes[106:121,],stake,c("x","y"),sep=0,fill="left") 
  split2$x = "0"
  split3 = separate(grid_stakes[122:425,],stake,c("x","y"),sep=1)
  #recombine and make x and y numeric
  stakes = rbind(split,split2,split3)
  stakes$x = loc_to_numeric(stakes$x)
  stakes$y = as.numeric(stakes$y)
  stakes = na.omit(stakes)
  #run regression to calculate the northings and westings from locx and locy
  xfit = lm(stakes$x~stakes$north+stakes$west)
  yfit = lm(stakes$y~stakes$north+stakes$west)
  # pull coefficients to calculate the line
  a=coef(xfit)[1]
  b=coef(xfit)[2]
  c=coef(xfit)[3]
  
  d=coef(yfit)[1]
  e=coef(yfit)[2]
  f=coef(yfit)[3]
  #pull target locx and locy from x data table
  locx = x$locx
  locy = x$locy
  #apply line to locx and locy
  x$north = as.numeric((f*locx-a*f+c*d-c*locy)/(b*f - c*e))
  x$west = as.numeric((b*locy-d*b+e*a-e*locx)/(b*f - c*e))
  x$east = -x$west
  #convert northing and easting coordinates to a spatial points object
  cord.dec = st_as_sf(x, coords=c("east","north"), crs=4326)
  cord.UTM <- st_transform(cord.dec, crs=3154)
  return(cord.UTM)
}

con <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)
con_suppl <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                          dbname ="krsp_suppl",
                          username = Sys.getenv("krsp_user"),
                          password = Sys.getenv("krsp_password")
)

# Grid Stakes Table
grid_stakes <- tbl(con_suppl, "grid_stakes") %>%
  filter(!is.na(north),!is.na(west),
         grid =="KL",
         north > 55) %>%
  collect() %>% 
  mutate(east = -west)

# run the function
cord.UTM <- locs_to_utms(grid_stakes = grid_stakes, x = KLmiddens2017)
head(cord.UTM)
## you should see your data and a new geometry column that is the x,y coordinates of your point in sf
plot(cord.UTM$geometry) # nice for confirmation the function worked as intended

class(cord.UTM$geometry)

cord.UTM <- cord.UTM %>%
  select(-west)

fwrite(cord.UTM, "output/cord.UTM.csv")
