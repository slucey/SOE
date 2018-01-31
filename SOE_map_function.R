## SOE Mapping Function

# Instructions - The colors in the thermal habitat projections are 
# scaled to the max values from the 1969-1993 hindcasts. I put the hindcast data
# into a separate folder and looped through those files to find max values, then changed the wd
# to where I stored the projections. Shapefiles for clipping the projections needs to be stored in 
# a folder (e.g. /soe_maps/data) within the folder where the projection files
# are located (e.g. /soe_maps/).

## Libraries

PKG <-c("dplyr","ncdf4","rgdal","tmap","raster","tmaptools",
        "maptools","mapdata","maps","colorRamps","fields")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    library(p)  }
}

## Find max values of historic projection for calibrating color scale of future projections
setwd("F:/soe/soe_data/Max_TH")

list.files()

for (i in list.files()){
  data <- nc_open(i)
  z <- ncvar_get(data, "zi")
  ndat <- dim(z)
  
  proj <- data.frame(z = z)
  proj <- proj %>% filter(z != "NA",z>0)
  assign(i,max(proj$z))
  rm(data)
}


setwd("F:/soe/soe_data/soe_maps")

## Set constants (crs)
map.crs <- CRS("+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0
                +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
can0<-getData('GADM', country="CAN", level=0)
usa <- map("state", fill = TRUE,plot = FALSE)
usa2 <- map("state", fill = TRUE, plot = FALSE)

# Convert map to sp objects
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa2 <- map2SpatialPolygons(usa2, IDs=IDs, proj4string=map.crs)
usa <- map2SpatialPolygons(usa, IDs=IDs, 
                           proj4string=map.crs)
# Define color scale
colo <- matlab.like2(100)

## Function

# Leave file type OFF of clipping.data variable
soe.map <- function(data, clipping.data = NULL, color = color,max.z = max.z,
                    clipping.loc = NULL, map.crs = map.crs, lat = lat, lon = lon, z = z){
  
  ## Get and filter data
  data <- nc_open(data)
  lon <- ncvar_get(data, "xi", verbose = F)
  nlon <- dim(lon)
  
  lat <- ncvar_get(data, "yi", verbose = F)
  nlat <- dim(lat)
  
  z <- ncvar_get(data, "zi")
  ndat <- dim(z)
  
  proj <- data.frame(lon = lon,
                     lat = lat,
                     z = z)
  proj <- proj %>% filter(z != "NA",z>0)
  
  #Convert data.frame to sp object
  coordinates(proj) <- ~lon+lat
  proj@proj4string <- map.crs
  proj1 <- spTransform(proj, map.crs)
  
  #Get clipping data
  strata <- readOGR(dsn = clipping.loc, layer = clipping.data)
  strata@proj4string <- map.crs
  
  #Clip thermal habitat data
  proj1 <- proj1[strata,]
  usa <- usa[proj1, ]
  
  #set bbox to match kde figures
  proj1@bbox <- matrix(data = c(-77,35,-65,45),ncol = 2)
  

  #Mapping
  print("Mapping data...")
  tm_shape(usa, bbox = proj1@bbox, projection = map.crs) +
    tm_borders(col = "grey", lwd = 1) +
    tm_fill(palette = "grey") +
    tm_grid(x = c(-76,-74,-72,-70,-68,-66),
            y = c(44,42,40,38,36),
            labels.inside.frame = F,
            labels.size = 1.1,
            col = "white") +
    tm_layout(outer.margins = c(.05,.03,.1,.1),
              outer.bg.color = "grey80")+
    tm_shape(proj1,axes = T) +
    tm_dots("z", palette = colo,
            breaks = seq(0,max.z,length.out = 100),legend.show = F) +
    tm_shape(usa2, bbox = proj1@bbox, projection = map.crs) +
    tm_borders(col = "grey", lwd = 1) +
    tm_fill(palette = "grey") +
    tm_shape(can0, bbox = proj1@bbox, projection = map.crs)+
    tm_borders(col = "grey", lwd = 1) +
    tm_fill(palette = "grey")

}

soe.map.legend <- function(data, color = color, max.z = max.z){
  data <- nc_open(data)
  lon <- ncvar_get(data, "xi", verbose = F)
  nlon <- dim(lon)
  
  lat <- ncvar_get(data, "yi", verbose = F)
  nlat <- dim(lat)
  
  z <- ncvar_get(data, "zi")
  ndat <- dim(z)
  
  proj <- data.frame(lon = lon,
                     lat = lat,
                     z = z)
  proj <- proj %>% filter(z != "NA",z>0)
 # par(new=F)
  image.plot(-70,40, z = as.matrix(seq(0,max.z,length.out = 100)),
             add = T, col = colo, legend.only = T)
}

## MAB
soe.map(data = "Black Sea Bassfall_2.nc", color = colo,max.z = `Black Sea Bassfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
               map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Black Sea Bassfall_2.nc", color = colo, max.z = `Black Sea Bassfall_1.nc`)

soe.map(data = "Black Sea Bassfall_4.nc", color = colo,max.z = `Black Sea Bassfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Black Sea Bassfall_4.nc", color = colo, max.z = `Black Sea Bassfall_1.nc`)

soe.map(data = "Sea Scallopfall_2.nc", color = colo,max.z = `Sea Scallopfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Sea Scallopfall_2.nc", color = colo, max.z = `Sea Scallopfall_1.nc`)

soe.map(data = "Sea Scallopfall_4.nc", color = colo,max.z = `Sea Scallopfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Sea Scallopfall_4.nc", color = colo, max.z = `Sea Scallopfall_1.nc`)

soe.map(data = "Summer Flounderfall_2.nc", color = colo,max.z = `Summer Flounderfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Summer Flounderfall_2.nc", color = colo, max.z =`Summer Flounderfall_1.nc`)

soe.map(data = "Summer Flounderfall_4.nc", color = colo,max.z = `Summer Flounderfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Summer Flounderfall_4.nc", color = colo, max.z =`Summer Flounderfall_1.nc`)

### New England

soe.map(data = "Atlantic Codfall_2.nc", color = colo,max.z = `Atlantic Codfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Atlantic Codfall_2.nc", color = colo, max.z =`Atlantic Codfall_1.nc`)

soe.map(data = "Atlantic Codfall_4.nc", color = colo,max.z = `Atlantic Codfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Atlantic Codfall_4.nc", color = colo, max.z =`Atlantic Codfall_1.nc`)

soe.map(data = "Haddockfall_2.nc", color = colo,max.z = `Haddockfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Haddockfall_2.nc", color = colo, max.z =`Haddockfall_1.nc`)

soe.map(data = "Haddockfall_4.nc", color = colo,max.z = `Haddockfall_1.nc`,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Haddockfall_4.nc", color = colo, max.z =`Haddockfall_1.nc`)
