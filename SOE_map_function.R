## SOE Mapping Function

setwd("F:/soe/soe_data/soe_maps")
## Libraries

PKG <-c("dplyr","ncdf4","rgdal","tmap","raster","tmaptools",
        "maptools","mapdata","maps","colorRamps","fields")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    library(p)  }
}

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
soe.map <- function(data, clipping.data = NULL, color = color,
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
  z.max <- max(proj1@data$z)
  #Mapping
  #par(new=F)
  print("Mapping data...")
  map <- tm_shape(usa, bbox = proj1@bbox, projection = map.crs) +
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
            breaks = seq(0,2,length.out = 1000),legend.show = F) +
    tm_shape(usa2, bbox = proj1@bbox, projection = map.crs) +
    tm_borders(col = "grey", lwd = 1) +
    tm_fill(palette = "grey") +
    tm_shape(can0, bbox = proj1@bbox, projection = map.crs)+
    tm_borders(col = "grey", lwd = 1) +
    tm_fill(palette = "grey")
  print(max(proj1@data$z))
  return(map)
}

soe.map.legend <- function(data, color = color){ 
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
 
  par(new = F)
  image.plot(-70.5,40, z = as.matrix(seq(0,2,length.out = 1000)),
             add = T, col = colo, legend.only = T)
}

soe.map(data = "Black Sea Bassfall_2.nc", color = colo,
        clipping.loc = "data",clipping.data = "strata",
               map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Black Sea Bassfall_2.nc", color = colo)

soe.map(data = "Black Sea Bassfall_4.nc", color = colo,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Black Sea Bassfall_4.nc", color = colo)
soe.map(data = "Sea Scallopfall_2.nc", color = colo,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Sea Scallopfall_2.nc", color = colo)

soe.map(data = "Black Sea Bassfall_4.nc", color = colo,
        clipping.loc = "data",clipping.data = "strata",
        map.crs = map.crs, lon = "yi", lat = "xi", z = "zi")
soe.map.legend("Black Sea Bassfall_4.nc", color = colo)



