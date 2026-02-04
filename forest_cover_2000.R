# forest cover 2000

library(here)
library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
sf::sf_use_s2(F)

Sys.setenv(TMPDIR = "D:/temp")
terraOptions(tempdir = "D:/temp")

LU2000<-rast("DATA/Mapbiomas/novos/brazil_coverage_2000.tif")
LU2000

PA_shape_4326<-read_sf("Outputs/PA_IT_QUI_shape.gpkg") %>% 
  st_transform(., "EPSG:4326")

PA_shape<-read_sf("Outputs/PA_IT_QUI_shape.gpkg") %>% 
  st_transform(., "EPSG:5880")

#we consider forest all primary and secondary vegetation category in mapbiomas

LU2000_crop<-crop(LU2000, vect(PA_shape_4326))
LU2000_mask<-mask(LU2000_crop, vect(PA_shape_4326))
LU2000_5880 <- project(LU2000_mask, "EPSG:5880") 
pixel_area <- prod(res(LU2000_5880)) 

forest_area_2000 <- exactextractr::exact_extract(
  LU2000_5880, PA_shape,
  fun = function(df) {  
    result <- data.frame(
      forestprim_area = sum(df$coverage_fraction[df$value == 1], na.rm = TRUE) * pixel_area,
      forestsec_area = sum(df$coverage_fraction[df$value == 10], na.rm = TRUE) * pixel_area
    )
    return(result)
  },
  summarize_df = TRUE, force_df = TRUE
)

PA_shape <- PA_shape %>%
  mutate(forest_cover_2000 = forest_area_2000$forestprim_area+
           forest_area_2000$forestsec_area)

terra::writeRaster(LU2000_5880, "Outputs/temporarios/forestcover_br_2000.gpkg")
write_sf(PA_shape, "Outputs/PA_cover2000.gpkg")
