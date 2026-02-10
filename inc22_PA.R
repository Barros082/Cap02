# gathering income 

# income varibale doesn't exist to PA. However, it exist on 2022 census to "census tract"
# so, we'll gather it using the mean household census tract income based in the intersected CT area that each PA has. 

library(tidyverse)
library(censobr)
library(geobr)
library(sf)

sf_use_s2(F)
# to 2000 and 2022, we collect income var that represent the meanf of householder income per capita.
#censobr::data_dictionary(year = 2000, dataset = "tracts")

# saving to avoid the geobr cache issue 
#CT2000<-geobr::read_census_tract(code_tract = "all", 
#                                 zone = "urban",
#                         year = 2000,
#                         simplified = F)
#CT2000_rur<-geobr::read_census_tract(code_tract = "all", 
#                                 zone = "rural",
#                         year = 2000,
#                        simplified = F)
#CT_00_end<-bind_rows(CT2000, CT2000_rur) %>% glimpse
#write_sf(CT_00_end, 
#         "Outputs/temporarios/CT2000.gpkg")
#CT2022<-geobr::read_census_tract(code_tract = "all", 
#                          year = 2022,
#                          simplified = F, 
#                          cache = F
#                          )
#write_sf(CT2022, 
#         "Outputs/temporarios/CT2022.gpkg")

CT2000<-read_sf("Outputs/temporarios/CT2000.gpkg")
CT2022<-read_sf("Outputs/temporarios/CT2022.gpkg")

inc00_br<-CT2000 %>%
       mutate(across(c(code_tract, code_muni, code_state), as.factor)) %>%
       left_join(
             censobr::read_tracts(
                   dataset = "Basico",
                   year = 2000,
                   as_data_frame = TRUE) %>%
                   select(code_tract, code_muni,
                          code_state, VAR03) %>%
                   mutate(across(c(code_tract,
                                   code_muni, 
                                   code_state), 
                                 as.factor)) %>%
                   rename(inc_pcp2000 = VAR03),
             by = c("code_tract", "code_muni",
                    "code_state")) %>%
       st_transform(5880) %>% glimpse
  
inc22_br<-censobr::read_tracts(dataset = "ResponsavelRenda",
                              year = 2022, 
                              as_data_frame = T) %>% 
  dplyr::select(1, code_muni, code_state, V06004) %>% 
  mutate(across(.cols = c(1:3), .fns=as.factor)) %>% 
  rename(inc_pcp2022=V06004) %>% 
  left_join(CT2022 %>% 
              dplyr::select(code_tract, code_muni, 
                            code_state, geom) %>% 
              mutate(across(.cols = c(1:3), .fns=as.factor)), 
            by=c("code_tract", "code_muni", "code_state")) %>% 
  st_as_sf() %>% 
  st_transform("EPSG:5880") %>% 
  mutate(CT_area=sf::st_area(geom)) %>% 
  glimpse

PA_br<-readRDS("Outputs/PA_balanced.rds") %>% 
  select(-centroid) %>% 
  st_as_sf(geom) %>% 
  glimpse

inc_list<-list(inc00_br, inc22_br)
inc_00_22<-list()

for (i in seq_along(inc_list)) {
  df_i<-inc_list[[i]]
  inc_var <- if (i == 1) "inc_pcp2000" else "inc_pcp2022"
  inc_name <- if (i == 1) "inc_pcp2000_by_area" else "inc_pcp2022_by_area"
  inc_00_22[[i]]<-sf::st_intersection(PA_br, df_i) %>% 
    mutate(area_intersec = as.numeric(st_area(.))) %>% 
    st_drop_geometry() %>% 
    group_by(new_code) %>% 
    summarise(
      !!inc_name := sum(.data[[inc_var]] * area_intersec, 
                          na.rm = TRUE) /
                          sum(area_intersec, na.rm = TRUE)) %>%
    ungroup()
}

inc_full<-do.call(full_join, inc_00_22) %>% glimpse


fulldata<-readRDS("Outputs/PA_balanced.rds") %>% 
  left_join(inc_full, 
            by="new_code") %>% glimpse

# testing
fulldata %>% 
  st_drop_geometry() %>% 
  filter(!name_biome%in%c("Pampa", 
                          "Pantanal")) %>% 
  filter(!new_cat%in%c("RPPN", 
                       "APA",
                       "ARIE")) %>%
  group_by(name_biome, new_cat) %>% 
  summarise(
    qa_00=quantile(inc_pcp2000_by_area),
    qa_22=quantile(inc_pcp2022_by_area)
  ) %>%  print(n = 140)
# there are some 0 or low values
  
saveRDS(fulldata, "Outputs/PA_balanced_with_incpcp.rds")

#teste<-readRDS("Outputs/PA_balanced_with_incpcp.rds") %>% 
#  glimpse
#teste %>% st_as_sf(sf_column_name = "geom") %>% 
#  ggplot()+ geom_sf()