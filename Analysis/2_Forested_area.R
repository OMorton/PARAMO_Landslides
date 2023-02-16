.libPaths("C:/Packages") ## Set up for working from home.
setwd("G:/My Drive/TUoS/PARAMO/Landslides")
options(scipen=99)
source("OM/Analysis/Functions/Functions.R")

## Packages
library(sf)
library(tidyverse)
library(rnaturalearth)
library(terra)
library(tidyterra)
library(raster)
sf::sf_use_s2(F)

library(viridis)

Landuse <- st_read('Data/Nelson_Grima/Mapa_LandUse_2017/Shape_E_ECCMC_Ver21_100K/E_ECCMC_Ver21_100K.shp')
Departments <- st_read('Data/COL_adm/COL_adm1.shp')
Roads_Ant_mask2 <- st_read("OM/Outputs/Ant_Suscep_Roads/Ant_Suscep_Road.shp", quiet = TRUE)
Road_edge_Ant_mask2 <- st_read("OM/Outputs/Ant_Suscep_Roads/Ant_Suscep_Roadedge.shp", quiet = TRUE)

CRS <-  "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"

Landuse_Ras <- st_transform(Landuse, CRS)
Dep_Ras <- st_transform(Departments, CRS)
Roads_Ant_mask2 <- st_set_crs(Roads_Ant_mask2, CRS) ## already transformed
Road_edge_Ant_mask2 <- st_set_crs(Road_edge_Ant_mask2, CRS) ## already transformed

## Select the "forest" landuses
Forest_landuse <- Landuse_Ras %>% filter(ECOS_SINTE %in% c("Bosque fragmentado", "Subxerofitia",
                                                           "Vegetacion secundaria", "Bosque"))

## Focus on just Antioquia
## for speed do this once then read it in in the future (saves reading in the whole colombia landuse file)
#Ant_Forest <- st_intersection(Forest_landuse, filter(Dep_Ras, NAME_1 == "Antioquia"))
#st_write(Ant_Forest, "OM/Outputs/Ant_Forest/Ant_Forest.shp", append=FALSE)

Ant_Forest <- st_read("OM/Outputs/Ant_Forest/Ant_Forest.shp", quiet = TRUE)
Ant_Forest <- st_set_crs(Ant_Forest, CRS)

ggplot() +  
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  geom_sf(data = Ant_Forest, aes(fill = ECOS_SINTE, colour = ECOS_SINTE)) +
  geom_sf(data = Roads_Ant_mask2, fill = "black", colour = "black") +
  scale_fill_manual(values = c("darkgreen", "chartreuse4", "chartreuse3", "olivedrab1")) +
  scale_colour_manual(values = c("darkgreen", "chartreuse4", "chartreuse3", "olivedrab1")) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")


st_area(Road_edge_Ant_mask2) %>% sum() # 453,045,169 m2 edge buffer area
Forest_Road_edge <- st_intersection(Ant_Forest, Road_edge_Ant_mask2) %>% st_sf()
st_area(Forest_Road_edge) %>% sum() # 32,498,653 m2 of edge buffer area is already forested


## Forested area
ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  geom_sf(data = Road_edge_Ant_mask2, aes(geometry = geometry), fill = "grey", colour = "grey") +
  geom_sf(data = Forest_Road_edge, aes(geometry = geometry), fill = "darkgreen", colour = "darkgreen") +
  coord_sf(xlim = c(912000, 916000), ylim = c(1144000, 1146000), crs = CRS, datum = CRS) +
theme_void()

## Area not forested
Not_Forest_Road_Edge <- st_difference(st_union(Road_edge_Ant_mask2), 
                                      st_union(Forest_Road_edge)) %>% st_sf()

st_area(Not_Forest_Road_Edge) %>% sum() # 420,546,516 m2 of edge buffer area would be considered "needing" forest

ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  geom_sf(data = Road_edge_Ant_mask2, aes(geometry = geometry), fill = "grey", colour = "grey") +
  geom_sf(data = Not_Forest_Road_Edge, aes(geometry = geometry), fill = "black", colour = "black") +
  coord_sf(xlim = c(912000, 916000), ylim = c(1144000, 1146000), crs = CRS, datum = CRS) +
  theme_void()
