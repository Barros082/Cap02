# Deforestation extraction

library(here)
library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
library(beepr)

sf::sf_use_s2(F)
terraOptions(tempdir = here("temp_terra"))

# data 
PA_shape<-read_sf("Outputs/PA_IT_QUI_shape.gpkg") %>% 
  st_transform(., "EPSG:5880")

defor22<-rast("DATA/Mapbiomas/deforestation/defor_c09_2022.tif")

defor22 %>% crs()  #4326
PA_crs<-PA_shape %>% st_transform(., "EPSG:4326")

crop_defor <- terra::crop(defor22, vect(PA_crs))

mask_defor <- terra::mask(crop_defor, vect(PA_crs))

defor22_5880 <- terra::project(mask_defor, "EPSG:5880", 
                         method = "near")
#terra::writeRaster(defor22_5880,
#                   "Outputs/temporarios/defor_22_recortadoPA.tif")
#defor22_5880<-rast("Outputs/temporarios/defor_22_recortadoPA.tif")
plot(defor22_5880)

pixel_area <- prod(res(defor22_5880))
PA_shape %>%  st_crs()

class_areas <- exact_extract(
  defor22_5880, 
  PA_shape,  
  fun = function(df) {  
    result <- data.frame(
      class_1_area = sum(df$coverage_fraction[df$value == 1], na.rm = TRUE) * pixel_area,
      class_2_area = sum(df$coverage_fraction[df$value == 2], na.rm = TRUE) * pixel_area,
      class_3_area = sum(df$coverage_fraction[df$value == 3], na.rm = TRUE) * pixel_area,
      class_4_area = sum(df$coverage_fraction[df$value == 4], na.rm = TRUE) * pixel_area,
      class_5_area = sum(df$coverage_fraction[df$value == 5], na.rm = TRUE) * pixel_area,
      class_6_area = sum(df$coverage_fraction[df$value == 6], na.rm = TRUE) * pixel_area
    )
    return(result)
  },
  summarize_df = TRUE, force_df = TRUE)

class_areas$new_code<-PA_shape$new_code
class_areas %>% summary()

defor_PA<-class_areas %>% 
  rowwise() %>%
  mutate(defor_amount= sum(c_across(c(class_4_area, 
                                      class_6_area)),
                           na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(new_code, defor_amount) %>% 
  left_join(PA_shape, by="new_code") %>% 
  st_as_sf() %>%
  glimpse 

saveRDS(class_areas, "Outputs/mapbiomas_raw_extract.rds")
write_sf(defor_PA, "Outputs/PA_defor.gpkg")

