# idea: LU urban pixels ----
library(here)
library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
library(geobr)

sf::sf_use_s2(F)
terraOptions(tempdir = here("temp_terra"))
# https://www.geoaplicada.com/dados/territorios-quilombolas/
## PA shapes
PA_shape<-read_sf("Outputs/PA_IT_QUI_shape.gpkg")

pointss <- PA_shape %>%
  st_transform("EPSG:4326") %>%
  st_centroid() %>%
  dplyr::select(new_code)

pointss_5880<-pointss %>%
  st_transform("EPSG:5880")


# Land use rasters
list_LU22_files <- list.files(
  here("DATA/Mapbiomas/land_use"),
  pattern = "\\.tif$", full.names = TRUE)

### step 01

r_proj_all <- list()
for (nm in seq_along(list_LU22_files_2)) {
  cat("Processing raster:", basename(list_LU22_files_2[[nm]]), "\n")
  r <- rast(list_LU22_files_2[[nm]])
  r_urb <- ifel(r == 24, 1, NA)
  r_proj <- project(r_urb, "EPSG:5880") 
  r_proj_all[[paste0(nm)]] <- r_proj
}

#testing and saving (i did it because my pc feature memories issues with the distance extraction from rasters)
#t1<-r_proj_all[[1]]
#plot(t1)
#t2<-r_proj_all[[2]]
#t3<-r_proj_all[[3]]
#t4<-r_proj_all[[4]] # null
#r_proj_all<-list(t1, t2, t3)

#urb_br<-do.call(merge, r_proj_all)
#plot(urb_br)
#terra::writeRaster(urb_br, "Outputs/temporarios/urb_br_temp03.tif")
#urb_br<-rast("Outputs/temporarios/urb_br_temp03.tif")

urb_poly1 <- as.polygons(urb_br, values = TRUE, na.rm = TRUE)
urb_poly3 <- aggregate(urb_poly1)                       
urb_poly4 <- st_as_sf(urb_poly3)                        
urb_poly5 <- st_make_valid(urb_poly4)
nearest_idx <- st_nearest_feature(pointss_5880,
                                  urb_poly5)
dist_vals <- st_distance(pointss_5880, 
                         urb_poly5[nearest_idx, ],
                         by_element = TRUE) #meters

dist_df <- tibble(
  new_code = pointss_5880$new_code,
  dist_to_urban = as.numeric(dist_vals) 
)

saveRDS(dist_df, "Outputs/PA_dist_urban.rds")


