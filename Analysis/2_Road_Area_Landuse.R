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

## Read in 
Landuse <- st_read('Data/Nelson_Grima/Mapa_LandUse_2017/Shape_E_ECCMC_Ver21_100K/E_ECCMC_Ver21_100K.shp')
Departments <- st_read('Data/COL_adm/COL_adm1.shp')
Roads_Ant_mask2 <- st_read("OM/Outputs/Ant_Suscep_Roads/Ant_Suscep_Road.shp", quiet = TRUE)
Road_edge_Ant_mask2 <- st_read("OM/Outputs/Ant_Suscep_Roads/Ant_Suscep_Roadedge.shp", quiet = TRUE)

CRS <-  "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"

## Transform or confirm setting
Landuse_Ras <- st_transform(Landuse, CRS) %>% st_make_valid()
Dep_Ras <- st_transform(Departments, CRS)
Roads_Ant_mask2 <- st_set_crs(Roads_Ant_mask2, CRS) ## already transformed
Road_edge_Ant_mask2 <- st_set_crs(Road_edge_Ant_mask2, CRS) ## already transformed
st_area(Road_edge_Ant_mask2) %>% sum()

## Simplify the land raster
Landuse_Ras2 <- Landuse_Ras %>% mutate(Cover = case_when( 
  ## Water based so can't be costed up to be reforested
  COBERTURA %in% c("Cuerpo de agua artificial", "Rio",
                   "Zonas pantanosas", "Turberas",
                   "Manglar de aguas mixohalinas", 
                   "Vegetacion acuatica sobre cuerpos de agua",
                   "Laguna", "Lago",
                   "Estanques para acuicultura marina",
                   "Canales", "Manglar", 
                   "Laguna costera", "Pantano costero",
                   "Manglar de aguas marinas") ~ "Water-based",
  ## Misc types
  COBERTURA %in% c("Zonas arenosas naturales", "Nubes",
                   "Salitral", ## Saltpeter check meaning??
                   "Sedimentos expuestos en bajamar", 
                   "Playas", "N.A.", 
                   "Campos de dunas") ~ "Misc",
  ## exposed rock not viable for reforesting
  COBERTURA %in% c("Afloramientos rocosos") ~ "Rock",
  ## glaciers and snow not viable for reforesting
  COBERTURA %in% c("Zonas de glaciares y nieves") ~ "Glaciers_snow",
  ## Forest type so cant't be reforested
  COBERTURA %in% c("Bosque fragmentado", "Subxerofitia",
                   "Vegetacion secundaria", "Bosque",
                   "Bosque abierto bajo", "Bosque denso alto",
                   "Bosque abierto alto", "Plantacion forestal",
                   "Bosque fragmentado con vegetacion secundaria",
                   "Bosque de galeria y ripario", 
                   "Bosque denso bajo",
                   "Bosque mixto de guandal") ~ "Forest",
  ## Grass shrubland - own ecosystem
  COBERTURA %in% c("Herbazal abierto", "Arbustal abierto",
                   "Herbazal denso", "Arbustal denso",
                   "Arbustal abierto alto") ~ "Grass_Shrub",
  ## Productive systems for opportunity costing
  COBERTURA %in% c("Mosaico de cultivos y pastos", "Pastos",
                   "Mosaico de pastos con espacios naturales",
                   "Mosaico de cultivos, pastos y espacios naturales",
                   "Plátano y Banano",
                   "Bosque fragmentado con pastos y cultivos",
                   "Mosaico de cultivos y espacios naturales",
                   "Mosaico de pastos y espacios naturales",
                   "Territorio artificializado", ## Check meaining
                   "Cultivos permanentes", "Cultivos transitorios",
                   "Mosaico de cultivos con espacios naturales" ,
                   "Cafe", "Arroz", "Palma de aceite",
                   "Papa", ## Check meaning
                   "Caña") ~ "Productive",
  COBERTURA == "Areas abiertas sin vegetacion" ~ "No_vegetation",
  TRUE ~ "FAIL"))

## Check no landuse left un converted
filter(Landuse_Ras2, Cover == "FAIL") %>% distinct(COBERTURA)

## Crop to department level and write out for speed
#Ant_Landuse <- st_intersection(Landuse_Ras2, filter(Dep_Ras, NAME_1 == "Antioquia"))
#st_write(Ant_Landuse, "OM/Outputs/Ant_Landuse/Ant_Landuse.shp", append=FALSE)
Ant_Landuse <- st_read("OM/Outputs/Ant_Landuse/Ant_Landuse.shp", quiet = TRUE)
Ant_Landuse <- st_set_crs(Ant_Landuse, CRS)

ggplot() +  
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  geom_sf(data = Ant_Landuse, aes(fill = Cover), colour = NA) +
  geom_sf(data = Roads_Ant_mask2, fill = "black", colour = "black") +
  scale_fill_manual(values = c("darkgreen", "chartreuse4", "grey75", "grey25", "tomato", "dodgerblue")) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

## Get the road edge landuse
Road_Edge_LandUse <- st_intersection(Landuse_Ras2, Road_edge_Ant_mask2) %>% st_sf()
Road_Edge_LandUse <- st_make_valid(Road_Edge_LandUse) %>% st_union(by_feature = TRUE)
unique(Road_Edge_LandUse$Cover)

st_area(Road_Edge_LandUse) %>% sum() # 453,045,165 [m^2]

Landuse_sum <- data.frame(Cover = Road_Edge_LandUse$Cover, Area = as.vector(Road_Edge_LandUse %>% st_area())) %>%
  group_by(Cover) %>% tally(Area)


ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  #geom_sf(data = Road_edge_Ant_mask2, aes(geometry = geometry), fill = "grey", colour = "grey") +
  geom_sf(data = Road_Edge_LandUse, aes(geometry = geometry, fill = Cover), colour = NA) +
  scale_fill_manual(values = c("darkgreen", "chartreuse4", "grey25", "tomato", "dodgerblue")) +
  coord_sf(xlim = c(912000, 916000), ylim = c(1144000, 1146000), crs = CRS, datum = CRS) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  #geom_sf(data = Road_edge_Ant_mask2, aes(geometry = geometry), fill = "grey", colour = "grey") +
  geom_sf(data = Road_Edge_LandUse, aes(geometry = geometry, fill = as.factor(SUSCEP),), colour = NA) +
  scale_fill_viridis_d("Susceptibility", option = "inferno", direction = -1) +
  coord_sf(xlim = c(912000, 916000), ylim = c(1144000, 1146000), crs = CRS, datum = CRS) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggplot(Landuse_sum, aes(n, Cover, fill = Cover)) + 
  geom_col() +
  scale_fill_manual(values = c("darkgreen", "chartreuse4", "grey25", "tomato", "dodgerblue")) +
  theme_minimal() +
  theme(legend.position = "none", axis.title.y = element_blank())



