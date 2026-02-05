# climatic extractions

library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
library(here)
sf::sf_use_s2(F)

Sys.setenv(TMPDIR = "D:/temp")
terraOptions(tempdir = "D:/temp")

# data 
PA_shape<-read_sf("Outputs/PA_IT_QUI_shape.gpkg")

list_clim_files <- list.files(
  here("DATA/WorldClim"),
  pattern = "\\.tif$", full.names = TRUE)

prec_files <- list_clim_files[str_detect(list_clim_files, "prec")]
tmin_files <- list_clim_files[str_detect(list_clim_files, "tmin")]
tmax_files <- list_clim_files[str_detect(list_clim_files, "tmax")]

clim_vars <- list(prec = prec_files, 
                  tmin = tmin_files, 
                  tmax = tmax_files)

res_all <- list()
for (var_name in names(clim_vars)) {
  
  files <- clim_vars[[var_name]]
  
  rast_stack <- rast(files)
  rast_proj <- project(rast_stack, "EPSG:4674")
  rast_crop <- crop(rast_proj, vect(PA_shape))
  rast_mask <- mask(rast_crop, vect(PA_shape))
  
  rast_stat <- if (var_name == "prec") {
    sum(rast_mask)   
  } else {
    mean(rast_mask) 
  }
  
  values <- exact_extract(rast_stat, PA_shape, fun = "mean")
  
  df_temp <- tibble(
    id = PA_shape$new_code, 
    var = var_name,
    value = values)
  
  res_all[[var_name]] <- df_temp
}

res_final <- bind_rows(res_all) %>% 
  pivot_wider(names_from = var, 
              values_from = value) %>% 
  mutate(id=as.factor(id)) %>% 
  glimpse

res_final %>%  summary()

PA_with_clim <- PA_shape %>%
  left_join(res_final, by = c("new_code"="id")) %>% 
  glimpse

#PA_with_clim %>% 
#  filter(is.na(tmax)) %>%  glimpse #just 3NA (2 PI and 1 APA)

write_sf(PA_with_clim, "Outputs/PA_climatic.gpkg")
