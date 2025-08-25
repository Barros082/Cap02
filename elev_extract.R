# Elevation vars extraction

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

elev_fabdem<-read_sf("DATA/FABDEM_data_elevation/FABDEM_v1-2_tiles.geojson") %>% 
  st_transform("EPSG:4674")

files_br <- st_intersection(elev_fabdem, PA_shape %>% 
                              dplyr::select(new_code)) %>% 
  glimpse

tile_files2 <- files_br %>%
  dplyr::select(new_code, file_name) %>%
  distinct()

all_files<-list.files("DATA/FABDEM_data_elevation/All", 
           pattern = "\\.tif$", full.names = TRUE)


# filtering and fixing names ----

path_code<-tile_files2 %>%  
  mutate(dum01="DATA", dum02="FABDEM_data_elevation",
         dum03="All",
         path=paste(dum01, dum02, dum03, file_name, sep="/"), 
         new_code=as.factor(new_code)) %>% 
  dplyr::select(new_code, path) %>% 
  glimpse

path_code %>%
  mutate(match_direct = path %in% all_files) %>% 
  View()

path_code2<-path_code %>% 
  mutate(
    path_fixed = gsub("([NS])0+([0-9]{2})", "\\1\\2", path), 
    match_direct = path_fixed %in% all_files) %>% 
  glimpse

# extracting elevation

extract_results <- path_code2 %>%
  split(.$new_code) %>%
  lapply(function(df) {
    code <- unique(df$new_code)
    message("Processando ", code)
    
    raster_list <- lapply(df$path_fixed, function(p) {
      r <- terra::rast(p)
      r <- terra::project(r, "EPSG:4674", method = "bilinear")
      r <- terra::aggregate(r, fact = 2, fun = "mean") # 60m
      return(r)
    })
    
    if (length(raster_list) == 1) {
      r_merged <- raster_list[[1]]
    } else {
      r_merged <- do.call(terra::mosaic, raster_list)
    }
    
    pa <- PA_shape %>% filter(new_code == code)
    
    values <- exactextractr::exact_extract(r_merged, pa, fun = "mean")
    
    tibble(new_code = code, stdev = values)
  })

results_df <- bind_rows(extract_results)

options(scipen = 99999)
results_df %>%  summary()

PA_with_elevation <- PA_shape %>%
  left_join(results_df, by = "new_code")

write_sf(PA_with_elevation, "Outputs/PA_elevation")