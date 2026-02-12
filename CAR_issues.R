# TEST CAR -- Solving issues before continues

library(tidyverse)
library(sf)

sf_use_s2(F)

PA_shape <-read_sf("Outputs/PA_IT_QUI_shape.gpkg") %>% 
  glimpse

t_AC_peri<-read_sf("DATA/CAR/AREA_IMOVEL/AREA_IMOVEL_1.shp") %>%
  select(cod_imovel, contains("ind_"), 
         des_condic, contains("dat_")) %>% 
  mutate(across(.cols = c(2:4), 
                .fns=as.factor), 
         dat_criaca = year(dmy(dat_criaca)),
         dat_atuali = year(dmy(dat_atuali))) %>% 
  filter(dat_criaca<=2022)
  glimpse

t_AC_peri %>%  summary()
#PA_shape %>% st_crs() #4674
#t_AC_peri %>% st_crs() #4674

t_AC_intersec<-st_intersection(PA_shape, t_AC_peri) %>% 
  glimpse

t_AC_intersec %>%  
  mutate(new_cat=as.factor(new_cat)) %>% 
  summary()
# ind_status: SU=0
# des_condic : Cancelado por decisao administrativa=762
# there are CAR above 2022

t_AC_intersec %>% 
  st_drop_geometry() %>% 
  filter(!new_cat %in%c("APA", "ARIE")) %>% 
  group_by(new_cat, ind_status, ind_tipo, des_condic) %>% 
  summarise(sum_imov=n_distinct(cod_imovel)) %>% 
  print(n = 100)
