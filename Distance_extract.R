# Distance to urban centers

library(here)
library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
library(geobr)

sf::sf_use_s2(F)
Sys.setenv(TMPDIR = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")
terraOptions(tempdir = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")

PA_shape<-read_sf("Outputs/PA_IT_shape.gpkg")

pointss<-PA_shape %>% 
  st_as_sf() %>%
  st_transform("EPSG:5880") %>% 
  mutate(
    centroid = st_centroid(geom),
    lat = st_coordinates(centroid)[,2],
    long = st_coordinates(centroid)[,1]) %>% 
  dplyr::select(new_code, centroid, lat, long) %>% 
  glimpse

land_use_list<-list.files("DATA/Mapbiomas/land_use", 
           pattern = "\\.tif$", full.names = TRUE)

PA_centroid<-pointss %>% 
  st_drop_geometry() %>% 
  dplyr::select(centroid) %>% glimpse

#LU_list2<-lapply(land_use_list, function(x){
#  LU_data<-rast(x)
  #LU_data<-project(LU_data, "EPSG:5880", method = "near", res=60)
#  })

#values_R1<-LU_list2[[1]] %>% unique()

#LU_list3<-lapply(land_use_list, function(x){
#  LU_data<-rast(x)
  #LU_data<-project(LU_data, "EPSG:5880", method = "near", res=60)
#  LU_data <- ifel(LU_data == 24, 24, NA) # just urban areas
#  return(LU_data)
#})


dist_list <- lapply(seq_along(land_use_list), function(i){
  r <- rast(land_use_list[i])
  r_urb <- ifel(r == 24, 1, NA)
  r_dist <- distance(r_urb)
  
  dist_vals <- terra::extract(r_dist, vect(PA_centroid))
  dist_vals$filename <- basename(land_use_list[i])
  return(dist_vals)
})

dist_all <- bind_rows(dist_list)














######
LU_list3<-lapply(land_use_list, function(x){
  LU_data<-rast(x)
  #LU_data<-project(LU_data, "EPSG:5880", method = "near", res=60)
  LU_data <- ifel(LU_data == 24, 24, NA) # just urban areas
  LU_data <- as.points(LU_data, na.rm = TRUE)
  points_raster <- st_as_sf(LU_data)
})

urban_points <- do.call(rbind, LU_list3)
urban_points <- st_transform(urban_points, 5880)

# join

PA_centroid<-pointss %>% 
  st_drop_geometry() %>% 
  dplyr::select(centroid) %>% glimpse

dist_LA <- st_distance(PA_centroid, urb_p) 
min_dist <- apply(dist_LA, 1, min) 