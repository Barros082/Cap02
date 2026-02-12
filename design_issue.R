# correlation test

library(tidyverse)
library(sf)
library(corrplot)
library(censobr)
library(geobr)

PA_data<-readRDS("Outputs/PA_balanced_with_incpcp.rds") %>% #686 
  filter(!new_cat%in%c("RPPN", "ARIE", "APA")) %>% #651
  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% #637
  select(1:3, Pop:sanitation, contains("inc_")) %>% 
  glimpse

#censobr::data_dictionary(year = 2000, dataset = "tracts")
CT2000<-read_sf("Outputs/temporarios/CT2000.gpkg")

baseline2000_CT<-CT2000 %>%
  mutate(across(c(code_tract, code_muni, 
                  code_state), as.factor)) %>%
  left_join(
    censobr::read_tracts(
      dataset = "Basico",
      year = 2000,
      as_data_frame = TRUE) %>%
      select(code_tract, code_muni,
             code_state, VAR12) %>%
      #Moradores em domicílios particulares permanentes (V0239) ou população residente em domicílios particulares permanentes (V1332) 
      mutate(across(c(code_tract,
                      code_muni, 
                      code_state), 
                    as.factor)) %>%
      rename(pop_00 = VAR12),
    by = c("code_tract", "code_muni",
           "code_state")) %>%
  left_join(
    censobr::read_tracts(
      dataset = "Domicilio",
      year = 2000,
      as_data_frame = TRUE) %>%
      select(code_tract, code_muni,
             code_state, V0018, V0021, V0025,
             V0030, V0031
             #V0018, V0021, V0025 - Domicílios particulares permanentes com abastecimento de água da rede geral, de água de poço ou nascente na propriedade, e outra forma de abastecimento de água (respectivamente) 
             #30/31- Domicílios particulares permanentes com banheiro ou sanitário e esgotamento sanitário via rede geral de esgoto ou pluvial, e via fossa séptica 
             ) %>%
      mutate(across(c(code_tract,
                      code_muni, 
                      code_state), 
                    as.factor), 
             water_00=V0018+V0021+V0025,
             sanitation_00=V0030+V0031,
             ),
    by = c("code_tract", "code_muni",
           "code_state")) %>%
  st_transform(5880) %>% 
  select(-contains("V00")) %>% 
  glimpse


PA<-PA_data %>% 
  rename(pop_22=Pop, 
         water_22=water, 
         sanitation_22=sanitation, 
         inc_00=inc_pcp2000_by_area, 
         inc_22=inc_pcp2022_by_area) %>%  glimpse


end_values<-sf::st_intersection(PA, baseline2000_CT) %>% 
  mutate(area_intersec = as.numeric(st_area(.))) %>% 
  st_drop_geometry() %>% 
  group_by(new_code) %>% 
  summarise(
    pop_00_adj=sum(pop_00 * area_intersec, 
                      na.rm = TRUE) /
      sum(area_intersec, na.rm = TRUE), 
    water_00_adj=sum(water_00 * area_intersec, 
                   na.rm = TRUE) /
      sum(area_intersec, na.rm = TRUE),
    sanitation_00_adj=sum(sanitation_00 * area_intersec, 
                     na.rm = TRUE) /
      sum(area_intersec, na.rm = TRUE),
    ) %>%
  ungroup() %>% 
  right_join(PA, by="new_code") %>% 
  glimpse

corr_data<-end_values %>% 
  sf::st_drop_geometry() %>% 
  select(2:4, 7:11) %>%  glimpse

corr_matrix <- cor(corr_data)
corrplot(corr_matrix, method = "number", 
         tl.col = "black", tl.srt = 20, addCoef.col = "black")

corr_data %>%  summary()
