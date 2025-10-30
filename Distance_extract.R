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
PA_shape<-read_sf("Outputs/PA_IT_shape.gpkg")

pointss <- PA_shape %>%
  st_transform("EPSG:4326") %>%
  st_centroid() %>%
  dplyr::select(new_code)

pointss_5880<-pointss %>%
  st_transform("EPSG:5880")
  

pointss_split <- split(pointss, 
                       cut(seq_len(nrow(pointss)), 
                           6,
                           labels = FALSE))
pointss_5880_split <- split(pointss_5880, 
                            cut(seq_len(nrow(pointss_5880)), 
                                6,
                                labels = FALSE))

# Land use rasters
list_LU22_files <- list.files(
  here("DATA/Mapbiomas/land_use"),
  pattern = "\\.tif$", full.names = TRUE)

### 1

r_proj_all <- list()
for (nm in seq_along(list_LU22_files)) {
  cat("Processing raster:", basename(list_LU22_files[[nm]]), "\n")
  r <- rast(list_LU22_files[[nm]])
  r_urb <- ifel(r == 24, 1, NA)
  r_proj <- project(r_urb, "EPSG:5880") 
  r_proj_all[[paste0(nm)]] <- r_proj
}
terra::writeRaster(r_proj_all[[1]], "Outputs/lu5880_prt01.tif")
terra::writeRaster(r_proj_all[[2]], "Outputs/lu5880_prt02.tif")
terra::writeRaster(r_proj_all[[3]], "Outputs/lu5880_prt03")

dist_all <- list()
for (i in seq_along(pointss_5880_split)) {
    cat("Chunk", i, "of", length(pointss_5880_split), "\n")
    pts_chunk <- pointss_5880_split[[i]]
    r_crop <- crop(r_proj, ext(vect(pts_chunk)))
    r_dist <- distance(r_crop)
    dvals <- terra::extract(r_dist, vect(pts_chunk))
    dvals <- dvals %>%
      mutate(Cod_setor = pts_chunk$new_code,
             raster_name = basename(list_LU22_files[[nm]]),
             chunk = i)
    
    dist_all[[paste0(nm, "_", i)]] <- dvals
    gc()
}






### 2
dist_all<-list()
for (nm in seq_along(list_LU22_files)) {
  r <- rast(list_LU22_files[[nm]])
  #r_crs<-crs(r)
  #print(r_crs)}
  r_crp  <- crop(r, vect(pointss))
  r_mask <- mask(r_crp, vect(pointss))
  r_urb <- ifel(r_mask == 24, 1, NA)
  r_proj <- project(r_urb, "EPSG:5880")
  r_dist <- distance(r_proj)
  dvals <- terra::extract(r_dist, vect(pointss_5880)) %>% 
    mutate(Cod_setor=pointss$new_code)
  
  dist_all[[nm]] <- dvals
}

dist22_allBR<-do.call(rbind, dist_all)




# idea: municipal seats ----
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
