# Balancing data to macthing

library(tidyverse)
library(sf)

sf_use_s2(F)

# Join
PA_full <-read_sf("Outputs/PA_IT_shape.gpkg") %>% 
  left_join(read_sf("Outputs/PA_climatic.gpkg") %>%
              st_drop_geometry() %>% 
              select(new_code, prec:tmax),
            by="new_code") %>% 
  left_join(read_sf("Outputs/PA_elevation.gpkg") %>%
              st_drop_geometry() %>% 
              select(new_code, elevation_mean),
            by="new_code") %>% 
  left_join(readRDS("Outputs/PA_distance.rds"),
            by="new_code") %>%
  left_join(read_sf("Outputs/PA_defor.gpkg") %>%
              st_drop_geometry() %>% 
              select(new_code, defor_amount),
            by="new_code") %>%
  glimpse

# cleaning
PA_full %>%  summary()
# climatic had 3 NA
# defor featured 1 NA

PA_full %>% 
  filter(!is.na(prec) & !is.na(defor_amount)) %>% #dim() #1248 - right!
  select(-COD_UC:-code_it_dummy) %>% 
  mutate(
    across(.cols = c(new_cat, new_code), .fns=as.factor),
    year_ref=as.numeric(year_ref)
  ) %>%  summary()


