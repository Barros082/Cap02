# IT shape

library(tidyverse)
library(readxl)

# download from: 
#https://www.gov.br/funai/pt-br/atuacao/terras-indigenas/geoprocessamento-e-mapas

# important annotations:
# TI = Em estudo < Delimitada < Declarada < Homologada < Regularizada
# Reserva Indigena = Encaminhada RI < Regularizada

IT_shp<-read_sf("DATA/garbage/tis_poligonais/tis_poligonaisPolygon.shp",
                options = "ENCODING=LATIN1") %>% 
  glimpse

valid_IT<-st_make_valid(IT_shp) %>% 
  mutate(
    across(.cols=c(1, 2, 5, 
                   6, 8, 9,
                   12, 17), .fns=as.factor)
  ) %>% 
  select(-cr, -undadm_cod:-dominio_un, -epsg) %>% 
  glimpse

dim(valid_IT)
summary(valid_IT)
#any(!st_is_valid(valid_IT))

right_TI<-valid_IT %>% 
  filter(fase_ti%in%c("Homologada", 
                      "Regularizada")) %>%
  select(2:4, 6, 12, 13) %>% 
  #DataExplorer::plot_missing() # just data have NA values
  glimpse

dup_shp <- right_TI %>% 
  st_drop_geometry() %>% 
  group_by(uf_sigla) %>% 
  count(terrai_nom) %>% 
  filter(n > 1) 
# Kariri-Xocó, Uneiuxi
# Understand where are this geometries?
#right_TI %>%
#  filter(terrai_nom=="Uneiuxi") %>%
#  ggplot() + geom_sf(aes(color=terrai_cod),alpha=0.5)
# we have two options: join and exlcude thsi area. Join is simple, see:
#right_TI %>%
#  filter(terrai_nom%in%dup_shp$terrai_nom) %>% 
#  group_by(terrai_nom) %>% 
#  summarise(.groups = "drop") %>% 
#  glimpse
# but we choose exclude it to be more parcimoniosly. 

end_funai<-right_TI %>% 
  filter(!terrai_nom%in%dup_shp$terrai_nom) %>% 
  glimpse #497

# testing join with  socioecnomic IBGE data
pop_ind<-read_xlsx("DATA/garbage/pop_ind.xlsx")[-1:-5, c(-1, -4:-5)] %>%
  rename(code_TI_IBGE=`...2`, 
         name_TI=`...3`, 
         pop=`...6`) #578

dup_pop <- pop_ind %>%
  count(name_TI) %>%
  filter(n > 1)

pop_IT<-pop_ind %>%
  filter(!name_TI%in%dup_pop$name_TI) %>%
  glimpse #556

TI_cleaned<-end_funai %>% left_join(pop_IT, by=c(
  "terrai_nom"="name_TI"
)) %>%
  #count(name_TI) %>% filter(n > 1) # there is no duplicated name
  #DataExplorer::plot_missing() # these NA values in pop were because the duplicated IT that I remove in original funai data
  filter(!is.na(pop)) %>% # 475
  mutate(code_TI_IBGE=as.factor(code_TI_IBGE),
         pop=if_else(pop=="-", "0", pop),
         pop=as.numeric(pop)) %>%  
  glimpse

sf::write_sf(end_funai, "Outputs/IT_shape.gpkg")
