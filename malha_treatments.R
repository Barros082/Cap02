# Join PA - IT plus UC

library(tidyverse)
library(sf)
sf::sf_use_s2(F)

UC_shape<-readRDS("Outputs/UC_socio_data.rds") %>% 
  select(1, 2, 9, 10, 13, Pop:dead_less1year) %>% 
  rename(year_ref=Ano.de.Criação, 
         PA_name=NOME_UC) %>%
  st_as_sf() %>% 
  mutate(year_ref=as.character(year_ref)) %>% 
  glimpse

IT_shape<-sf::read_sf("Outputs/IT_socio_data.gpkg") %>% 
  mutate(new_cat="IT", 
         new_cat=as.factor(new_cat)) %>%
  rename(geometry=geom, 
         year_ref=data_atual, 
         PA_name=terrai_nom) %>% 
  select(COD_IT, 1:2, year_ref, Pop:new_cat) %>% 
  st_as_sf() %>% 
  glimpse

PA_shape<-bind_rows(UC_shape, IT_shape) %>% 
  mutate(
    code_uc_dummy = ifelse(!is.na(COD_UC), 
                           paste0("UC_", COD_UC), NA),
    code_it_dummy = ifelse(!is.na(COD_IT),
                           paste0("IT_", COD_IT), NA),
    new_code = coalesce(code_uc_dummy, code_it_dummy)) %>% 
  select(new_code, PA_name, new_cat, 
         year_ref:dead_less1year, geometry, 
         everything()) %>%  glimpse
  
# We need to collect manually the years from each IT. 
write_sf(PA_shape, "Outputs/PA_IT_shape.gpkg")


