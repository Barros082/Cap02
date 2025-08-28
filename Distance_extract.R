# Distance to municipal seats

library(tidyverse)
library(sf)        
library(geobr)

sf::sf_use_s2(F)

PA_shape<-read_sf("Outputs/PA_IT_shape.gpkg")

pointss<-PA_shape %>% 
  st_as_sf() %>%
  st_transform("EPSG:5880") %>% 
  mutate(centroid = st_centroid(geom)) %>% 
  dplyr::select(new_code, centroid) %>% 
  glimpse

sede_muni<-geobr::read_municipal_seat() %>% 
  st_transform("EPSG:5880") %>% 
  dplyr::select(1) %>% 
  glimpse


dist_matrix <- st_distance(pointss$centroid, sede_muni)
min_dist <- apply(dist_matrix, 1, min)

dist_PA <- tibble(
  new_code   = as.factor(pointss$new_code),
  min_dist_m = as.numeric(min_dist)) %>%  glimpse


saveRDS(dist_PA, "Outputs/PA_distance.rds")
