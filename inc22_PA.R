# gathering income 

# income varibale doesn't exist to PA. However, it exist on 2022 census to "census tract"
# so, we'll gather it using the mean household census tract income based in the intersected CT area that each PA has. 

library(tidyverse)
library(censobr)
library(geobr)
library(sf)

sf_use_s2(F)

inc22_br<-censobr::read_tracts(dataset = "ResponsavelRenda",
                              year = 2022, 
                              as_data_frame = T) %>% 
  dplyr::select(1, code_muni, code_state, V06004) %>% 
  mutate(across(.cols = c(1:3), .fns=as.factor)) %>% 
  rename(inc_pcp2022=V06004) %>% 
  left_join(geobr::read_census_tract(code_tract = "all", 
                                     year = 2022,
                                     simplified = F) %>% 
              dplyr::select(code_tract, code_muni, code_state, geom) %>% 
              mutate(across(.cols = c(1:3), .fns=as.factor)), 
            by=c("code_tract", "code_muni", "code_state")) %>% 
  st_as_sf() %>% 
  st_transform("EPSG:5880") %>% 
  mutate(CT_area=sf::st_area(geom)) %>% 
  glimpse

PA_br<-readRDS("Outputs/PA_balanced.rds") %>% 
  glimpse
  
inc22CT_PA_intersc<-sf::st_intersection(PA_br, inc22_br) %>% 
  mutate(area_intersec = as.numeric(st_area(.))) %>% 
  glimpse

full_data<-inc22CT_PA_intersc %>% 
  st_drop_geometry() %>% 
  group_by(new_code) %>% 
  summarise(
    across(.cols = c(1:21), .fns=first), 
    inc_pcp_by_area=sum(inc_pcp2022 * area_intersec, na.rm = TRUE) / 
      sum(area_intersec, na.rm = TRUE),
    ) %>% 
  mutate(geom=st_geometry(PA_br)[match(new_code, PA_br$new_code )]) %>% 
  glimpse

# testing
full_data %>% 
  filter(!name_biome%in%c("Pampa", 
                          "Pantanal")) %>% 
  filter(!new_cat%in%c("RPPN", 
                       "APA",
                       "ARIE")) %>%
  group_by(name_biome, new_cat) %>% 
  summarise(
    qa=quantile(inc_pcp_by_area)
  ) %>%  print(n = 140)
# perfect
  
saveRDS(full_data, "Outputs/PA_balanced_with_incpcp.rds")

#teste<-readRDS("Outputs/PA_balanced_with_incpcp.rds") %>% 
#  glimpse
#teste %>% st_as_sf(sf_column_name = "geom") %>% 
#  ggplot()+ geom_sf()