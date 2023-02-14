.libPaths("C:/Packages") ## Set up for working from home.
setwd("G:/My Drive/TUoS/PARAMO/Landslides")

## Packages
library(sf)
library(tidyverse)
library(rnaturalearth)
library(terra)
library(tidyterra)
library(raster)
sf::sf_use_s2(F)

library(viridis)

## Load shapefiles
Nat_roads <- st_read('Data/National roads/RedVial_OD_5.shp')
Third_lvl_roads <- st_read('Data/third level roads/EJES.shp')
NG_roads <- st_read('Data/Nelson_Grima/hotosm_col_roads_lines_shp_new/hotosm_col_roads_lines.shp')
Colombia <- ne_countries(country = "Colombia", scale = "medium", returnclass = "sf")
Departments <- st_read('Data/COL_adm/COL_adm1.shp')

# The road data nelson used has more components and detail.
ggplot() + geom_sf(data = Nat_roads) + geom_sf(data = Third_lvl_roads)
ggplot() + geom_sf(data = Colombia) + geom_sf(data = NG_roads)

# Susceptibility
#Amazonas <- rast("Data/Susceptibility/Amazonas/amazonas/w001001.adf")
Antioquia <- rast("Data/Susceptibility/Antioquia/antioquia/w001001.adf")
#Arauca <- rast("Data/Susceptibility/arauca/arauca/w001001.adf")
#Atlantico <- rast("Data/Susceptibility/atlantico/atlantico/w001001.adf")
#Bolivar <- raster("Data/Susceptibility/bolivar/bolivar/w001001.adf")
#Boyaca <- rast("Data/Susceptibility/boyaca/boyaca/w001001.adf")
#Caldas <- rast("Data/Susceptibility/caldas/caldas/w001001.adf")
#Caqueta <- rast("Data/Susceptibility/caqueta/caqueta/w001001.adf")
#Casanare <- rast("Data/Susceptibility/casanare/casanare/w001001.adf")
#Casanare <- rast("Data/Susceptibility/casanare/casanare/w001001.adf")
#Cauca <- rast("Data/Susceptibility/cauca/cauca/w001001.adf")
#Cesar <- rast("Data/Susceptibility/cesar/cesar/w001001.adf")

## Check the data
Antioquia
res(Antioquia)
dim(Antioquia)
plot(Antioquia)
ggplot() + 
  geom_sf(data = Colombia, fill = "white") + 
  geom_spatraster(data = Antioquia, maxcell = 10000) +
  #geom_spatraster(data = Bolivar, maxcell = 10000) +
  scale_fill_viridis(na.value = NA) + 
  geom_sf(data = Nat_roads, fill = NA) +
  theme_void()

#### Data Preparation ####

crs(Antioquia)
projection(Nat_roads)
projection(Colombia)

## Buffer the roads
Nat_roads_150 <- st_buffer(Nat_roads, dist = 150)
## Checked with large buffer
plot(Nat_roads_150)

## transform this to the raster projection (reprojecting the raster takes time)
RAW_Nat_road_Ras <- st_transform(Nat_roads, "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")
Nat_road_Ras <- st_transform(Nat_roads_150, "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")
Colombia_Ras <- st_transform(Colombia, "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")
Dep_Ras <- st_transform(Departments, "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")

## crop the roads to the raster extent
ext(Antioquia)
Nat_roads_Ant <- st_crop(Nat_road_Ras, c(xmin= 506175.805479698, xmax=1148375.25392544, ymin=1088718.54930205, ymax=1487211.22382904))
plot(Nat_roads_Ant)
st_length(RAW_Nat_road_Ras)
mask1 <-mask(vect(RAW_Nat_road_Ras), vect(filter(Dep_Ras, NAME_1 == "Antioquia")))
plot(mask1)

## Check
ggplot() + 
  geom_sf(data = Colombia, fill = "white") + 
  geom_spatraster(data = Antioquia, maxcell = 10000) +
  scale_fill_viridis(na.value = NA) + 
  geom_sf(data = Nat_roads_Ant, aes(geometry = geometry, colour = as.factor(objectid)), fill = NA) +
  theme_void() +
  theme(legend.position = "none")

## Turn to spatvector
vect_roads <- vect(Nat_roads_Ant)
## get suscep for each geometry
Suscep <- extract(Antioquia, vect_roads)



## Rasterizing fails
d <- stars::st_rasterize(Nat_roads_Ant %>% mutate(road = 1) %>% dplyr::select(road, geometry))
e <- rast(d)
plot(e, col = "black")
d$ID
