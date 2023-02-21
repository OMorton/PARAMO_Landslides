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
Opp_costs <- data.table::fread('Data/Nelson_Grima/Opp_costs/Opp_costs.csv')
Nat_roads <- st_read('Data/National roads/RedVial_OD_5.shp', quiet = TRUE)


CRS <-  "+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 
                                              +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"

## Transform or confirm setting
Landuse_Ras <- st_transform(Landuse, CRS) %>% st_make_valid()
Nat_roads <- st_transform(Nat_roads, CRS) %>% st_make_valid()
Dep_Ras <- st_transform(Departments, CRS)
Roads_Ant_mask2 <- st_set_crs(Roads_Ant_mask2, CRS) ## already transformed
Road_edge_Ant_mask2 <- st_set_crs(Road_edge_Ant_mask2, CRS) ## already transformed
Roads_Ant_mask <-st_intersection(Nat_roads, filter(Dep_Ras, NAME_1 == "Antioquia"))
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

Landuse_sum <- data.frame(Cover = Road_Edge_LandUse$Cover, SUSCEP = Road_Edge_LandUse$SUSCEP,
                          Area = as.vector(Road_Edge_LandUse %>% st_area())) %>%
  group_by(Cover, SUSCEP) %>% tally(Area)

# 34,113,224 m2 forest
# 382,224,910 m2 productive
# 31,285,840 m2 water based
# 1,051,063 m2 no veg
# 4,370,128 m2 grass shrub
Landuse_sum %>% group_by(Cover) %>% tally(n) # 34,113,224 m2 forest

## Road edge landuse
ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  #geom_sf(data = Road_edge_Ant_mask2, aes(geometry = geometry), fill = "grey", colour = "grey") +
  geom_sf(data = Road_Edge_LandUse, aes(geometry = geometry, fill = Cover), colour = NA) +
  scale_fill_manual(values = c("darkgreen", "chartreuse4", "grey25", "tomato", "dodgerblue")) +
  coord_sf(xlim = c(912000, 916000), ylim = c(1144000, 1146000), crs = CRS, datum = CRS) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

## Road edge suscep
ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  #geom_sf(data = Road_edge_Ant_mask2, aes(geometry = geometry), fill = "grey", colour = "grey") +
  geom_sf(data = Road_Edge_LandUse, aes(geometry = geometry, fill = as.factor(SUSCEP),), colour = NA) +
  scale_fill_viridis_d("Susceptibility", option = "inferno", direction = -1) +
  coord_sf(xlim = c(912000, 916000), ylim = c(1144000, 1146000), crs = CRS, datum = CRS) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

## Landuse suscep types
ggplot(Landuse_sum, aes(n, Cover, fill = Cover)) + 
  geom_col() +
  scale_fill_manual(values = c("darkgreen", "chartreuse4", "grey25", "tomato", "dodgerblue")) +
  facet_wrap(~SUSCEP, scales = "free_x") +
  xlab("Area (m2)") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.y = element_blank())

#### Economic costs per area ####

# Convert net USD per hectare to per m2
Opp_costs <- Opp_costs %>% mutate(Net_USD_m2 = Net_USD_ha / 10000)

## Now the land use data has a myriad of crops and mosaic landscapes.
## We make the following assumptions going forward 
## 1. If the land is a mosaic of pasture and any other natural forest fragments or 
## cultivation we assume the opportunity cost to be equal to if it was all pasture 
## (as we cannot know whether the non pasture fragments cover 1% or 99% of the area - 
## this approach therefore will purposely maximize costs).
## 2. If the land is a mosaic of pasture and cultivation  (where the crop is 
## not known) we assume the whole are to be Coffee as the crop with the greatest 
## net value per ha (and greater than pasture).
## 3. Similarly in cultivation and forest/natural fragment landscapes we assume
## the whole area to be coffee.
## 4. If the exact crop is not known, it assumed to be coffee again - to maximize
## the possible opportunity cost. e.g. worst case scenario.
unique(Road_Edge_LandUse$COBERTURA)
Road_Edge_LandUseCost <- Road_Edge_LandUse %>% 
  mutate(Opp_cost_product_match = case_when(
    ## Cultivation (coffee)
    COBERTURA %in% c("Mosaico de cultivos y pastos",
                     "Mosaico de cultivos, pastos y espacios naturales",
                     "Bosque fragmentado con pastos y cultivos",
                     "Mosaico de cultivos y espacios naturales",
                     "Mosaico de cultivos con espacios naturales",
                     # unknown crop so assume coffee
                     "Territorio artificializado",
                     "Cultivos permanentes", "Cultivos transitorios") ~ "Coffee",
    ## Pasture
    COBERTURA %in% c("Mosaico de pastos con espacios naturales",
                     "Pastos",
                     "Mosaico de pastos y espacios naturales") ~ "Pasture",
    ## Banana
    COBERTURA %in% c("Plátano y Banano") ~ "Banana",
    ## Coffee
    COBERTURA %in% c("Cafe") ~ "Coffee",
    ## Rice
    COBERTURA %in% c("Arroz") ~ "Rice",
    ## Oil palm
    COBERTURA %in% c("Palma de aceite") ~ "Oil palm",
    ## Native potato,
    COBERTURA %in% c("Papa") ~ "Potato",
    ## Sugar cane
    COBERTURA %in% c("Caña") ~ "Sugar cane",
    TRUE ~ "FAIL")) %>%
  ## Append the cost data
  left_join(Opp_costs, by = c("Opp_cost_product_match" = "Product")) %>%
  ## add the m2 polygon area
  mutate(Patch_area_m2 = as.vector(st_area(Road_Edge_LandUse)),
         ## assume no vegatation areas have a cost of zero?
         #Net_USD_m2 = ifelse(COBERTURA == "Areas abiertas sin vegetacion", 0, Net_USD_m2)
         Annual_opp_cost = Patch_area_m2 * Net_USD_m2)
    

SUSCEP_by_Cost <- Road_Edge_LandUseCost %>% as.data.frame() %>%
  group_by(SUSCEP, Net_USD_m2) %>% tally(Patch_area_m2) %>%
  filter(!is.na(Net_USD_m2)) %>%
  mutate(Cost_band = case_when(Net_USD_m2 == 0.0075 ~ "Low",
                               Net_USD_m2 == 0.0122 ~ "Medium",
                               Net_USD_m2 == 0.047 ~ "High",
                               TRUE ~ "CHECK MISSING COSTS"))

sum(SUSCEP_by_Cost$n) # 382,224,910 m2 productive area 

ggplot(SUSCEP_by_Cost, aes(SUSCEP, n, fill = Cost_band)) +
  geom_col() +
  scale_fill_manual(values = c("tomato", "chartreuse3", "yellow3")) +
  xlab("Landslide susceptibility") +
  ylab("Total area (m2)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.title = element_blank())


ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  geom_sf(data = Roads_Ant_mask , aes(geometry = geometry), colour = "black") +
  geom_sf(data = filter(Road_Edge_LandUseCost, !is.na(Net_USD_m2)), 
         aes(geometry = geometry, fill = as.factor(Net_USD_m2)), colour = NA) +
  geom_sf(data = filter(Road_Edge_LandUseCost, Cover == "Forest"), 
         aes(geometry = geometry), fill = "darkgreen", colour = NA) +
  coord_sf(xlim = c(912000, 927000), ylim = c(1143000, 1150000), crs = CRS, datum = CRS) +
  scale_fill_manual(values = c("chartreuse3", "yellow3", "tomato"),
                    labels = c("Low", "Medium", "High")) +
  facet_wrap(~SUSCEP, ncol = 1) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia")) + 
  geom_sf(data = filter(Road_Edge_LandUseCost, !is.na(Net_USD_m2)), 
          aes(geometry = geometry, fill = as.factor(Net_USD_m2)), colour = NA) +
  geom_sf(data = filter(Road_Edge_LandUseCost, Cover == "Forest"), 
          aes(geometry = geometry), fill = "darkgreen", colour = NA) +
  scale_fill_manual(values = c("chartreuse3", "yellow3", "tomato"),
                    labels = c("Low", "Medium", "High")) +
  #facet_wrap(~SUSCEP, ncol = 1) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggplot() + 
  geom_sf(data = filter(Departments, NAME_1 == "Antioquia")) +
  geom_sf(data = Road_Edge_LandUseCost, aes(fill = as.factor(SUSCEP)), colour = NA)  +
  scale_fill_viridis_d("Susceptibility", option = "inferno", direction = -1) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")


#### Prioritizing specific projects ####

## So the 6203 has the highest average susceptibility score of 3.68 so for this example we will
## assume that is a necessary project
Road_ave_SUSCEP <- Roads_Ant_mask2 %>% as.data.frame() %>% 
  mutate(Road_and_buffer_area = as.vector(st_area(Roads_Ant_mask2)),
         Road_and_buffer_area_SUSCEP = Road_and_buffer_area*SUSCEP) %>%
  group_by(codigo_via) %>%
  summarise(Average_SUSCEP_m2 = sum(Road_and_buffer_area_SUSCEP) / sum(Road_and_buffer_area)) %>%
  arrange(Average_SUSCEP_m2)

## Visualise the road
ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia")) + 
  geom_sf(data = Roads_Ant_mask2, 
          aes(geometry = geometry)) +
  geom_sf(data = filter(Roads_Ant_mask2, codigo_via == 6203), 
          aes(geometry = geometry), fill = "red", colour = "red") +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

Proj_6203 <- st_intersection(filter(Roads_Ant_mask2, codigo_via == 6203), Road_Edge_LandUseCost) %>% 
  st_make_valid()

## 114km of road
Length_6203 <- st_length(filter(Roads_Ant_mask, codigo_via == 6203)) %>% sum() /1000

## Using Nelsons costs prices for a small road based off 2017 prices
as.vector(Length_6203) * 650262 # $74,128,819 to replace the road.

Proj_6203 %>% filter(is.na(Annual_opp_cost)) %>% distinct(Cover)

ggplot() + 
  #geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  #geom_sf(data = Roads_Ant_mask , aes(geometry = geometry), colour = "black") +
  geom_sf(data = filter(Proj_6203), 
          aes(geometry = geometry, fill = as.factor(Net_USD_m2)), colour = NA) +
  scale_fill_manual(values = c("chartreuse3", "tomato", "grey"),
                    labels = c("Low", "High", "Water/grassland/forest")) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

## Annual opportunity cost $3,688,812
Proj_6203 %>% as.data.frame() %>% filter(!is.na(Annual_opp_cost)) %>% summarise(sum(Annual_opp_cost))


#### low cost - high susceptibility areas ####

## Issue is balancing maximizing the area protected with forests in the highest susceptibility areas but also 
## accounting for the fact that lots of really small patches might not be any help at all.

Productive_roadside_area <- sum(SUSCEP_by_Cost$n) # 382,224,910 m2 productive area 

## 1 - The minimum
## Protect all low cost high and very high susceptibility areas of road
Min1_poly <- Road_Edge_LandUseCost %>%
  mutate(Cost_band = case_when(Net_USD_m2 == 0.0075 ~ "Low",
                               Net_USD_m2 == 0.0122 ~ "Medium",
                               Net_USD_m2 == 0.047 ~ "High",
                               TRUE ~ "CHECK MISSING COSTS")) %>% 
  filter(SUSCEP %in% c("4", "5"), Cost_band == "Low")

## 13% of the total surrounding road area
st_area(Min1_poly) %>% sum() / Productive_roadside_area
## 56.6% of the surrounding road area with 4 or 5 suscep
st_area(Min1_poly) %>% sum() / filter(SUSCEP_by_Cost, SUSCEP %in% c(4, 5))$n %>% sum() 
sum(Min1_poly$Annual_opp_cost)

Buffer_edge <- st_union(Road_Edge_LandUseCost)

ggplot() + 
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = "white") + 
  geom_sf(data = Buffer_edge, aes(geometry = geometry), fill = NA, colour = "black") +
  geom_sf(data = Roads_Ant_mask , aes(geometry = geometry), colour = "black") +
  geom_sf(data = filter(Min1_poly), 
          aes(geometry = geometry, fill = as.factor(Net_USD_m2)), colour = NA, fill = "chartreuse3") +
  geom_sf(data = filter(Road_Edge_LandUseCost, Cover == "Forest"), 
          aes(geometry = geometry), fill = "darkgreen", colour = NA) +
  geom_sf(data = filter(Road_Edge_LandUseCost, Cover == "Water-based"), 
          aes(geometry = geometry), fill = "dodgerblue", colour = NA) +
  coord_sf(xlim = c(912000, 916000), ylim = c(1144000, 1146000), crs = CRS, datum = CRS)
  

#### Land allocation test ####

## make a small test region with multiple polygons (various susceps and costs + some forest patches)
Test_sf <- st_crop(Road_Edge_LandUseCost, c(xmin = 912000, xmax = 916000, 
                                             ymin = 1144000, ymax = 1146000)) %>%
  mutate(Cost_band = case_when(Net_USD_m2 == 0.0075 ~ "Low",
                               Net_USD_m2 == 0.0122 ~ "Medium",
                               Net_USD_m2 == 0.047 ~ "High",
                               TRUE ~ "CHECK MISSING COSTS"))

## Aims
# 1. small reforested polygons in islands of expensive productivity is likely undesirable
# 2. small small fragment gap between reforested area and forests should be bridged.
unique(Test_sf$Cover)

small <- Test_sf %>% filter(Patch_area_m2 < 6600 & Patch_area_m2 > 6500) %>% st_make_valid()
The_rest <- Test_sf %>% filter(Patch_area_m2 > 6600 | Patch_area_m2 < 6500) %>% st_make_valid()
g <- st_touches( small, The_rest)

tes1 <- st_intersection(The_rest, small) %>% st_make_valid()

ggplot() +
  geom_sf(data = Test_sf, colour = NA, fill = "grey") +
  geom_sf(data = slice(The_rest, 10), fill = "green") +
  geom_sf(data = small, fill = "red")

shp <- st_union(Test_sf)
ggplot() +
  geom_sf(data = shp, fill = "green")

st_area(Test_sf) %>% sum()
st_area(shp)

## Get elevation data
elev <- elevatr::get_elev_raster(Ant_Landuse, src = "aws", crs = CRS, z = 12)
elev_sr <- rast(elev)

Ant_elev <- extract(elev_sr, vect(filter(Dep_Ras, NAME_1 == "Antioquia")))

Ant_elev <- raster::extract(elev, filter(Dep_Ras, NAME_1 == "Antioquia"))
elev_ant <- rast(Ant_elev)

ggplot() +
  geom_spatraster(data = elev_sr, maxcell = 10000)+
  scale_fill_viridis_c() +
  geom_sf(data = filter(Dep_Ras, NAME_1 == "Antioquia"), fill = NA, colour = "red")

