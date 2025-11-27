# Join PA - IT plus UC

library(tidyverse)
library(sf)
sf::sf_use_s2(F)

# debugging
# 2 - How many each minor-PA (original categories) type are?
# 3 - How many each PA type (federal, municipal, estadual) type are?
#readRDS("Outputs/UC_socio_data.rds") %>% 
#  st_drop_geometry() %>% 
#  mutate(
#    Bioma=as.factor(Bioma), 
#    bioma_dum=str_detect(Bioma, ",")) %>%
#  filter(!bioma_dum==TRUE) %>% #51
#  filter(!new_cat%in%c("APA", "ARIE", "RPPN")) %>% #381
#  filter(Bioma!="Pampa") %>% #377
#  filter(!CATEGORIA%in%c(
#    "Monumento Natural",
#    "Refúgio da Vida Silvestre", 
#    "Reserva de Fauna"
#  )) %>% #365
#  group_by(Bioma, ESFERA, new_cat, CATEGORIA) %>% 
#  summarise(ucs=n_distinct(COD_UC)) %>% 
#  print(n=62)

# join
UC_shape<-readRDS("Outputs/UC_socio_data.rds") %>% #787
  filter(!CATEGORIA%in%c(
        "Monumento Natural", #SPA
        "Refúgio da Vida Silvestre", #SPA 
        "Reserva de Fauna" #SUPA
      )) %>% #dim() #775
  select(1, 2, 9, 10, 13, Pop:waste, dead_less4year) %>% 
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
  select(COD_IT, 1:2, year_ref, Pop:new_cat, 
         -dead_less1year, -dead_less10years) %>% 
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
         year_ref:dead_less4year, geometry, 
         everything()) %>%  glimpse #1240
  
# We need to collect manually the years from each IT. 
write_sf(PA_shape, "Outputs/PA_IT_shape.gpkg") #1240 


