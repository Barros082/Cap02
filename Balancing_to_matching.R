# Balancing data to macthing

library(tidyverse)
library(sf)

sf_use_s2(F)

# Join
PA_full <-read_sf("Outputs/PA_IT_QUI_shape.gpkg") %>% 
  left_join(readRDS("Outputs/UC_socio_data.rds") %>%
              st_drop_geometry() %>% 
              select(COD_UC, ESFERA) %>% 
              mutate(COD_UC=as.character(COD_UC)),
            by="COD_UC") %>% 
  mutate(PA_scale=case_when(
    new_cat %in% c("IT", "QUI") ~ "Federal", 
    TRUE ~ ESFERA
  )) %>% 
  select(-ESFERA) %>% 
  left_join(read_sf("Outputs/PA_climatic.gpkg") %>%
              st_drop_geometry() %>% 
              select(new_code, prec:tmax),
            by="new_code") %>% 
  left_join(read_sf("Outputs/PA_elevation.gpkg") %>%
              st_drop_geometry() %>% 
              select(new_code, elevation_mean),
            by="new_code") %>% 
  left_join(read_sf("Outputs/PA_dist_energy_roads.gpkg") %>%
              st_drop_geometry() %>% 
              select(new_code, contains("dist_")),
            by="new_code") %>%
  left_join(read_sf("Outputs/PA_cover2000.gpkg") %>%
              st_drop_geometry() %>% 
              select(new_code, forest_cover_2000),
            by="new_code") %>%
  left_join(readRDS("Outputs/PA_dis_agr.rds"),
            by="new_code") %>%
  left_join(readRDS("Outputs/PA_dist_urban.rds"),
            by="new_code") %>%
  left_join(read_sf("Outputs/PA_defor.gpkg") %>%
              st_drop_geometry() %>% 
              select(new_code, defor_amount),
            by="new_code") %>%
  glimpse

# cleaning and add IT year ----
PA_full %>%  summary() #1339
# climatic had 3 NA

to_fill_TIyear<-PA_full %>% 
  select(-COD_UC:-code_it_dummy, 
         -lit, -waste) %>%
  filter(!is.na(prec)) %>% #dim() #1336 - right!
  mutate(
    across(.cols = c(new_cat, new_code), .fns=as.factor),
    year_ref=as.numeric(year_ref)
  ) %>%  
  filter(is.na(year_ref)) %>% 
  st_drop_geometry() %>% 
  select(1:2, year_ref) %>% 
  glimpse()

write.csv(to_fill_TIyear, 
          "Outputs/temporarios/tofill_ITyears.csv")

# debugging some IT
#PA_full %>% 
#  filter(PA_name=="Toldo Chimbangue" #& new_code=="IT_7595"
#         ) %>% 
  #glimpse
#  ggplot()+
#  geom_sf(aes(color=new_code), 
#          alpha=0.3)

it_years<-read.csv("DATA/ITyears_finished.csv") %>% 
  select(new_code, year_ref, dum) %>%  
  rename(yr_rf=year_ref) %>% 
  glimpse
  
# full data ----

PA_almost_done<-PA_full %>% 
  full_join(it_years, by="new_code") %>%
  mutate(
    dum=case_when(
     is.na(dum) ~ "ok", 
     TRUE ~ dum), 
    year_ref=case_when(
      new_cat=="IT" ~ NA,
      TRUE ~ year_ref),
    yr=coalesce(year_ref, yr_rf), 
    yr=as.numeric(yr)) %>% 
  filter(dum!="remove") %>% #dim() #1316
  select(-year_ref, -COD_UC:-code_it_dummy, 
         -yr_rf, -dum, -lit, -waste) %>% 
  filter(yr<=2022) %>%
  glimpse #1305

# Estimating PA area
PA_finished<-PA_almost_done %>% 
  st_transform(., "EPSG:5880") %>% #1305
  st_make_valid() %>% 
  mutate(
    PA_area=st_area(geom), # m²
    yr_creation=yr,
    expo_time=(yr-2022)*-1, 
    testgeom=st_is_valid(geom)
  ) %>% 
  filter(!is.na(prec)) %>% #dim() #1302
  #filter(testgeom==FALSE) %>% glimpse # 0 
  select(-yr, -testgeom) %>% 
  glimpse()  #1302

#how many PA we have for each biome? ----
biomas<-geobr::read_biomes() %>% 
  filter(!is.na(code_biome)) %>%  
  select(name_biome, geom) %>% 
  st_transform(., "EPSG:5880") %>% 
  glimpse

# with repetition
st_intersection(PA_finished, biomas) %>% 
  st_drop_geometry() %>% 
  filter(!new_cat%in%c(
    "APA", "ARIE", "RPPN"
  )) %>% 
  group_by(name_biome, new_cat) %>% 
  summarise(
    N_PA=n_distinct(new_code)
  ) %>%  
  print(n=100)
# Amazon, Caatinga, Cerrado and Atlantic forest is ok

duplicated_by_biome<-st_intersection(PA_finished, biomas) %>% 
  st_drop_geometry() %>% 
  mutate(test=duplicated(new_code)) %>% 
  filter(test==TRUE) %>% 
  glimpse#85


st_intersection(PA_finished, biomas) %>% 
  st_drop_geometry() %>%
  filter(!new_code%in%duplicated_by_biome$new_code) %>% #-166 (2*83)
  filter(!new_cat%in%c(
    "APA", "ARIE", "RPPN"
  )) %>% 
  group_by(name_biome, new_cat) %>% 
  summarise(
    N_PA=n_distinct(new_code)
  ) %>%  
  print(n=100)
# Amazon, Caatinga, Cerrado and Atlantic forest is ok

# preparing to match ----

PA_done<-st_intersection(PA_finished, biomas) %>% 
  #st_drop_geometry() %>%
  filter(!new_code%in%duplicated_by_biome$new_code) %>% #dim() # 1217
  #ggplot()+geom_sf()
  mutate(
    centroid=st_centroid(geom),
    lat=st_coordinates(centroid)[,2],
    long=st_coordinates(centroid)[,1], 
  ) %>% #1217
  # PA from states and municipals. 
  #st_drop_geometry() %>% 
  #filter(!new_cat%in%c("APA", "ARIE", "RPPN")) %>% 
  #group_by(name_biome, new_cat, PA_scale) %>% 
  #summarise(sum_PA=n_distinct(new_code)) %>%  print(n=30)
  filter(PA_scale=="Federal") %>% #dim() #686 
  # Estimating the final PA amount. 
  #st_drop_geometry() %>% 
  #filter(!new_cat%in%c("APA", "ARIE", "RPPN")) %>% 
  #filter(!name_biome%in%c("Pampa", "Pantanal")) %>%
  #group_by(name_biome, new_cat) %>% 
  #summarise(sum_PA=n_distinct(new_code)) %>% print(n=60)
  glimpse 

saveRDS(PA_done, "Outputs/PA_balanced.rds")


# understand how many PA we have----

# are there PA the was in more than 1 state?
#PA_state<-readRDS("Outputs/PA_balanced.rds") %>% #686
#  filter(!new_cat%in%c("APA", "RPPN", "ARIE")) %>% #dim()#651
#  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% #dim()#637
#  st_intersection(., 
#                  geobr::read_state() %>%  
#                    select(1:2, geom) %>% 
#                    st_transform(., "EPSG:5880")
#                  ) %>% 
#  st_drop_geometry() %>% 
#  group_by(abbrev_state, new_cat) %>% 
#  summarise(
#    N_PA=n_distinct(new_code)
#  ) %>%  
#  print(n=100)
