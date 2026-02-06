#distance agricultute pixels

library(here)
library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
library(beepr)

sf::sf_use_s2(F)
terraOptions(tempdir = here("temp_terra"))

PA_shape<-read_sf("Outputs/PA_IT_QUI_shape.gpkg") %>% 
  st_transform(., "EPSG:5880")

pointss_5880 <- PA_shape %>%
  st_transform("EPSG:5880") %>%
  st_centroid() %>% dplyr::select(new_code) %>% glimpse

# Land use rasters
list_LU22_files <- list.files(
  here("DATA/Mapbiomas/land_use"),
  pattern = "\\.tif$", full.names = TRUE)

### step 01
agr_class<-c(14, 15, 18, 19, 39, 20, 40, 62, 41, 
             36, 46, 47, 35, 48, 9, 21)

r_proj_all <- list()
for (nm in seq_along(list_LU22_files)) {
  cat("Processing raster:", basename(list_LU22_files[[nm]]), "\n")
  r <- rast(list_LU22_files[[nm]])
  r_urb <- ifel(r %in% agr_class, 100, NA)
  beep()
  r_proj <- project(r_urb, "EPSG:5880") 
  beep()
  r_proj_all[[paste0(nm)]] <- r_proj
}
beep()

#t1<-r_proj_all[[1]]
#terra::writeRaster(t1, "Outputs/temporarios/agr_br_5880_t1.tif")
#t2<-r_proj_all[[2]]
#terra::writeRaster(t2, "Outputs/temporarios/agr_br_5880_t2.tif")
#t3<-r_proj_all[[3]]
#terra::writeRaster(t3, "Outputs/temporarios/agr_br_5880_t3.tif")
#t4<-r_proj_all[[4]] # null
#r_proj_all_list<-list(t1, t2, t3)
#agr_br<-do.call(merge, r_proj_all_list)
#beep()
#plot(agr_br)
#terra::writeRaster(agr_br, "Outputs/temporarios/agr_br_5880_full.tif")
#beep()
agr_br<-rast("Outputs/temporarios/agr_br_5880_full.tif")

agr_br_agg <- aggregate(
  agr_br,
  fact = 2,   #~60 m pixels
  fun = max,
  na.rm = TRUE)

dist_terra<-distance(agr_br_agg, 
                     #filename = "D:/temp/dist_agr_temp.tif",
                     #overwrite = TRUE
                     )

dist_vals <- terra::extract(
  dist_terra,
  vect(pointss_5880),
  bind = TRUE)

PA_shape_dist_agr<-as.data.frame(dist_vals) %>%  
  rename(dist_agr=classification_2022 ) %>%  glimpse

saveRDS(PA_shape_dist_agr, "Outputs/PA_dis_agr.rds")

