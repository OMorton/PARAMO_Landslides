.libPaths("C:/Packages") ## Set up for working from home.
setwd("G:/My Drive/TUoS/PARAMO/Landslides")
options(scipen=99)

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



## transform this to the raster projection (reprojecting the raster takes time)
RAW_Nat_road_Ras <- st_transform(Nat_roads, "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")
Colombia_Ras <- st_transform(Colombia, "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")
Dep_Ras <- st_transform(Departments, "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")

## crop the roads to the raster extent
ext(Antioquia)
Nat_roads_Ant <- st_crop(RAW_Nat_road_Ras, c(xmin= 506175.805479698, xmax=1148375.25392544, 
                                             ymin=1088718.54930205, ymax=1487211.22382904))
Roads_Ant_mask <-st_intersection(Nat_roads_Ant, filter(Dep_Ras, NAME_1 == "Antioquia"))

## Length of all the roads in the department
st_length(Roads_Ant_mask)
st_length(Roads_Ant_mask) %>% sum()/1000 ## km

## Check
ggplot() + 
  geom_sf(data = Colombia, fill = "white") + 
  geom_spatraster(data = Antioquia, maxcell = 10000) +
  scale_fill_viridis(na.value = NA,trans = 'reverse', option = "inferno") + 
  geom_spatvector(data = Roads_Ant_mask, fill = NA) +
  #geom_sf(data = Nat_roads_Ant, aes(geometry = geometry, colour = as.factor(objectid)), fill = NA) +
  theme_void() +
  theme(legend.position = "none")


## Buffer the roads
Nat_roads_150 <- st_buffer(Roads_Ant_mask, dist = 150) %>% 
  # make the road object 
  st_union(by_feature = TRUE)

## Checked with large buffer
ggplot() + 
  geom_sf(data = Colombia, fill = "white") + 
  geom_spatraster(data = Antioquia, maxcell = 10000) +
  scale_fill_viridis(na.value = NA,trans = 'reverse', option = "inferno") + 
  geom_sf(data = Roads_Ant_mask, aes(geometry = geometry, colour = as.factor(objectid)), fill = NA) +
  theme_void() +
  theme(legend.position = "none")

## Turn spatraster to sf object
poly_Ant <- as.polygons(Antioquia)
Spdf_Ant <- st_as_sf(poly_Ant)
## already transformed so can just reset the CRS
Spdf_Ant2 <- st_set_crs(Spdf_Ant, "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")
## Gte the suscep values within 150m of a road
Roads_Ant_mask2 <-st_intersection(Spdf_Ant2, Nat_roads_150)
st_write(Roads_Ant_mask2, "OM/Outputs/Ant_Suscep_Roads/Ant_Suscep_Road.shp")

ggplot() + 
  geom_sf(data = Roads_Ant_mask2, aes(fill = as.factor(SUSCEP), colour = as.factor(SUSCEP)))

#### Summarising ####
Suscep_Area <- data.frame(Suscep = Roads_Ant_mask2$SUSCEP, 
                          codigo.via = Roads_Ant_mask2$codigo_via,
                          area_m2 = as.vector(st_area(Roads_Ant_mask2))) %>%
  mutate(Total = sum(area_m2))

Suscep_Area %>% group_by(Suscep) %>% summarise(Prop = sum(area_m2)/sum(Suscep_Area$area_m2))

## Suscep area by road
ggplot(Suscep_Area, aes(area_m2, codigo.via, fill = as.character(Suscep))) +
  geom_col() +
  scale_fill_viridis_d(option = "inferno", direction = -1) +
  guides(fill=guide_legend(title="Susceptibility")) +
  theme_minimal() +
  theme(legend.position = "bottom")

## Roads
ggplot() + 
  geom_sf(data = filter(Departments, NAME_1 == "Antioquia")) +
  geom_sf(data = Roads_Ant_mask2, aes(fill = as.factor(SUSCEP), colour = as.factor(SUSCEP)))  +
  scale_colour_viridis_d("Susceptibility", option = "inferno", direction = -1) +
  scale_fill_viridis_d("Susceptibility", option = "inferno", direction = -1) +
  theme_minimal() +
  theme(legend.position = "bottom")

## Susceptibility
ggplot() + 
  geom_sf(data = Spdf_Ant2, aes(fill = SUSCEP)) +
  geom_sf(data = Roads_Ant_mask2, fill = "black", colour = "black")  +
  scale_fill_viridis_d("Susceptibility", option = "inferno", direction = -1) +
  theme_minimal() +
  theme(legend.position = "bottom")

