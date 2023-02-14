.libPaths("C:/Packages") ## Set up for working from home.
setwd("G:/My Drive/TUoS/PARAMO/Landslides")

## Packages
library(sf)
library(tidyverse)
library(rnaturalearth)
library(terra)
library(tidyterra)
sf::sf_use_s2(F)

library(viridis)

# Load shapefiles
Nat_roads <- read_sf('Data/National roads/RedVial_OD_5.shp')
Third_lvl_roads <- read_sf('Data/third level roads/EJES.shp')
NG_roads <- read_sf('Data/Nelson_Grima/hotosm_col_roads_lines_shp_new/hotosm_col_roads_lines.shp')
Colombia <- ne_countries(country = "Colombia", scale = "medium", returnclass = "sf")

# The road data nelson used has more components and detail.
ggplot() + geom_sf(data = Nat_roads) + geom_sf(data = Third_lvl_roads)
ggplot() + geom_sf(data = Colombia) + geom_sf(data = NG_roads)

# Susceptibility
#Amazonas <- rast("Data/Susceptibility/Amazonas/amazonas/w001001.adf")
Antioquia <- rast("Data/Susceptibility/Antioquia/antioquia/w001001.adf")
#Arauca <- rast("Data/Susceptibility/arauca/arauca/w001001.adf")
#Atlantico <- rast("Data/Susceptibility/atlantico/atlantico/w001001.adf")
Bolivar <- rast("Data/Susceptibility/bolivar/bolivar/w001001.adf")
#Boyaca <- rast("Data/Susceptibility/boyaca/boyaca/w001001.adf")
#Caldas <- rast("Data/Susceptibility/caldas/caldas/w001001.adf")
#Caqueta <- rast("Data/Susceptibility/caqueta/caqueta/w001001.adf")
#Casanare <- rast("Data/Susceptibility/casanare/casanare/w001001.adf")
#Casanare <- rast("Data/Susceptibility/casanare/casanare/w001001.adf")
#Cauca <- rast("Data/Susceptibility/cauca/cauca/w001001.adf")
#Cesar <- rast("Data/Susceptibility/cesar/cesar/w001001.adf")

Antioquia

res(Antioquia)
dim(Antioquia)

plot(Antioquia)
ggplot() + 
  geom_sf(data = Colombia, fill = "white") + 
  geom_spatraster(data = Antioquia, maxcell = 10000) +
  geom_spatraster(data = Bolivar, maxcell = 10000) +
  scale_fill_viridis(na.value = NA) + 
  geom_sf(data = Nat_roads, fill = NA) +
  theme_void()



