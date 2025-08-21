# Climatic var extraction

library(here)
library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
library(geobr)
sf::sf_use_s2(F)

# Data
PA_shape<-read_sf("Outputs/PA_IT_shape.gpkg")

elev_fabdem<-read_sf("DATA/FABDEM_data_elevation/FABDEM_v1-2_tiles.geojson")

br_shape<-read_country() %>% 
  st_transform(., "EPSG:4326")

files_br <- st_intersection(elev_fabdem, br_shape) %>% 
  glimpse

# elevation
Sys.setenv(TMPDIR = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")
terraOptions(tempdir = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")

a<-list.files(here("DATA/FABDEM_data_elevation/S10W040-N00W030_FABDEM_V1-2"), 
              pattern = ".*\\.tif$",
              full.names = TRUE)
b<-list.files(here("DATA/FABDEM_data_elevation/S10W050-N00W040_FABDEM_V1-2"), 
              pattern = ".*\\.tif$",
              full.names = TRUE)
c<-list.files(here("DATA/FABDEM_data_elevation/S20W040-S10W030_FABDEM_V1-2"), 
              pattern = ".*\\.tif$",
              full.names = TRUE)
d<-list.files(here("DATA/FABDEM_data_elevation/S20W050-S10W040_FABDEM_V1-2"), 
              pattern = ".*\\.tif$",
              full.names = TRUE)

files_4<-list(a, b, c, d) %>% 
  unlist()

raster_elevation<-lapply(files_4, function(r){
  r<-terra::rast(r)
  r<-terra::project(r, "EPSG:4674", method="bilinear")
  r<-terra::aggregate(r, fact=2, fun="mean") # 60 m resolution
})

sapply(raster_elevation, terra::ext) # different extensions
sapply(raster_elevation, terra::res) 

unido_elevation <- do.call(terra::merge, raster_elevation)

plot(unido_elevation)

# cropping and masking to our shape

crop_and_mask<-function(r){
  r_crop <- crop(r, PA_shape)
  r_masked <- mask(r_crop, PA_shape)
  
  return(r_masked)
}

raster_elevation <- crop_and_mask(unido_elevation)

plot(raster_elevation)

PA_elevation<-PA_shape %>% 
  mutate(elevation_area=exact_extract(raster_elevation, ., fun = "stdev"))

write_sf(PA_elevation, "Outputs/PA_elevation.gpkg")
