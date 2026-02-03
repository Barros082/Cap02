# Distance to energy lines and roads

library(here)
library(tidyverse)
library(sf)        

sf::sf_use_s2(F)

list_dist_files <- list.files(
  here("DATA/outros"),
  pattern = "\\.shp$", full.names = TRUE)

shp_all <- list()
for (nm in seq_along(list_dist_files)) {
  shp <- read_sf(list_dist_files[[nm]]) %>% 
    st_transform(., "EPSG:5880")
  shp_all[[paste0(nm)]] <- shp
}

energy_lines_dum<-shp_all$`2` %>% glimpse
roads_dum<-bind_rows(shp_all$`3`, shp_all$`4`) %>%
  mutate(across(.cols = c(categoria:ID), 
                .fns = as.character)) %>% 
  bind_rows(shp_all$`1`) %>% 
  glimpse

PA_shape<-read_sf("Outputs/PA_IT_QUI_shape.gpkg") %>% 
  st_transform(., "EPSG:5880")

centroidss<-PA_shape %>% st_centroid() %>% 
  select(1) %>% glimpse

to_est_dist<-list(energy_lines_dum, roads_dum)

for (i in seq_along(to_est_dist)) {
  i_df<-to_est_dist[[i]]
  i_feat<-st_nearest_feature(centroidss, i_df)
  
  dist_i <- as.numeric(
    st_distance(
      centroidss,
      i_df[i_feat, ],
      by_element = TRUE))
  
  if(i==1){
    centroidss$dist_energy<-dist_i
  } else {centroidss$dist_roads<-dist_i}
}

centroidss %>%  summary()

PA_dist_energy_roads<-PA_shape %>% 
  left_join(centroidss %>% st_drop_geometry(), 
            by="new_code") %>% 
  glimpse

write_sf(PA_dist_energy_roads, 
         "Outputs/PA_dist_energy_roads.gpkg")
