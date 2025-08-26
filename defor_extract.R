# Deforestation extraction

library(here)
library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
sf::sf_use_s2(F)

Sys.setenv(TMPDIR = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")
terraOptions(tempdir = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")

# data 
PA_shape<-read_sf("Outputs/PA_IT_shape.gpkg")

defor22<-rast("DATA/Mapbiomas/deforestation/defor_c09_2022.tif")

# cropping and masking 
defor22 %>% crs()  #4326
PA_crs<-PA_shape %>% st_transform(., "EPSG:4326")


PA_crs_small <- st_filter(PA_crs, 
                          st_as_sfc(st_bbox(defor22))) %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") 


raster_list <- lapply(1:nrow(PA_crs_small), function(i) {
  poly <- PA_crs_small[i, ]
  code <- poly$new_code
  message("Processando polígono ", i, " - código: ", code)
  
  crop_defor <- terra::crop(defor22, vect(poly))
  mask_defor <- terra::mask(crop_defor, vect(poly))
  r_proj <- terra::project(mask_defor, "EPSG:4674", method = "near")

  return(r_proj)
    
})

raster_list <- raster_list[!sapply(raster_list, is.null)]


areas_list <- list()
for (i in seq_along(raster_list)) {
  poly <- PA_crs_small[i, ]
  code <- poly$new_code
  message("Extraindo áreas para polígono ", i, " - código: ", code)
  
  raster_proj <- terra::project(raster_list[[i]], "EPSG:5880", method = "near")
  pixel_area <- prod(res(raster_proj))
  poly_proj <- st_transform(poly, 5880)
  
  class_areas <- exact_extract(
    raster_proj, 
    poly_proj,  
    fun = function(df) {  # A função recebe um dataframe 'df'
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
    summarize_df = TRUE,
    force_df = TRUE
  )
  
  class_areas$new_code <- code
  class_areas$polygon_id <- i
  
  areas_list[[i]] <- class_areas
}

areas_by_class <- bind_rows(areas_list) %>% 
  mutate(new_code=as.factor(new_code))

areas_by_class %>% summary()

defor_PA<-areas_by_class %>% 
  rowwise() %>%
  mutate(defor_amount= sum(c_across(c(class_4_area, 
                                      class_6_area)),
                           na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(new_code, defor_amount) %>% 
  left_join(PA_shape %>% 
              mutate(new_code=as.factor(new_code )), 
            by="new_code") %>% 
  st_as_sf() %>% 
  glimpse # we just lose one row

saveRDS(areas_by_class, "Outputs/mapbiomas_raw_extract.rds")
write_sf(defor_PA, "Outputs/PA_defor.gpkg")


